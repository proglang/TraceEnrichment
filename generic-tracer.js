/* Copyright 2015, 2016 Johannes Kloos, MPI-SWS.
 *
 * Based on a template under the following license:
 *
 * Copyright 2014 Samsung Information Systems America, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Author: Koushik Sen
// Author: Johannes Kloos
// do not remove the following comment
// JALANGI DO NOT INSTRUMENT
// In the following callbacks one can choose to not return anything.
// If all of the callbacks return nothing, we get a passive analysis where the
// concrete execution happens unmodified and callbacks are used to observe the execution.
// Once can choose to return suitable objects with specified fields in some callbacks
// to modify the behavior of the concrete execution.  For example, one could set the skip
// field of an object returned from putFieldPre to true to skip the actual putField operation.
// Similarly, one could set the result field of the object returned from a write callback
// to modify the value that is actually written to a variable. The result field of the object
// returned from a conditional callback can be suitably set to change the control-flow of the
// program execution.  In functionExit and scriptExit,
// one can set the isBacktrack field of the returned object to true to reexecute the body of
// the function from the beginning.  This in conjunction with the ability to change the
// control-flow of a program enables us to explore the different paths of a function in
// symbolic execution.
(function(sandbox) {
    function GenericAnalysis(global, strategyBuilder) {
        var objects = new WeakMap();
        var functions = new WeakMap();
        var objids = 0;
        var funids = 0;
        var special_names = [
            "caller", "callee", "arguments", "this",
            "*J$IID*", "*J$SIG", 
        ];
        var top_special_names = [
            "J$", "acorn", "esotope"
        ];

        // HACK There seem to be two schools of thought on how to handle
        // global variables: They may be represented as entries in the global
        // object,
        // or as bindings in the global environment without any connection to
        // the
        // global object. We assume that each interpreter uses only one way to
        // handle globals, but we need to know which.
        // CAVE: The global object has to come first. We use the fact that the
        // global object has index 0 in the oracle.
        // The second argument is used to control the fields that get added.
        // In particular, we use it to exclude debris from the instrumentation.
        // Fill in descriptions for standard library objects if missing. XXX do we need to do something here?
        var strategy = strategyBuilder(global.J$ === J$, { global: { type: typeof global, id: 0 } });
        valid(global);

        // recurse along prototype chain
        function filter_special(at_top, name) {
            if (special_names.indexOf(name) !== -1) return true;
            if (at_top && top_special_names.indexOf(name) !== -1) return true;
            return false;
        }
        function writeobj(obj, queue, name) {
            if (objects.has(obj)) {
                return { type: typeof obj, id: objects.get(obj) };
            } else {
                var id = objids++;
                objects.set(obj, id);
                queue.push([obj, id, name]);
                return { type: typeof obj, id: id };
            }
        }
        function writeval(val, queue, name) {
            switch (typeof val) {
                case "undefined":
                    return {
                        type : "undefined"
                    };
                case "boolean":
                case "number":
                case "string":
                case "symbol":
                    return {
                        type : typeof obj,
                        val : val.toString()
                    }
                case "function":
                    var desc = writeobj(val, queue, name);
                    desc.funid = funcid(val);
                    return desc;
                default:
                    if (val === null) {
                        return {
                            type : "null"
                        }
                    } else {
                        return writeobj(val, queue, name);
                    }
            }
        }
        function valid(val) {
            var queue = [];
            var valdesc = writeval(val, queue, "./");
            var objid;
            var handled = new Set();
            while (objid = queue.shift()) {
                var obj = objid[0];
                var id = objid[1];
                var name = objid[2];
                if (handled.has(id)) continue;
                handled.add(id);
                var desc = {};
                var props = Object.getOwnPropertyNames(obj);
                props.sort();
                for (var i = 0; i < props.length; i++) {
                    var prop = props[i];
                    if (filter_special(obj === this, prop))
                        continue;
                    var propdesc = Object.getOwnPropertyDescriptor(obj, prop) || {};
                    var skip_value = false;
                    if (propdesc.get) {
                        propdesc.get = writeval(propdesc.get, queue, name + "/get:" + prop);
                        skip_value = true;
                    }
                    if (propdesc.set) propdesc.set = writeval(propdesc.set, queue, name + "/set:" + prop);
                    if (!skip_value)
                        propdesc.value = writeval(obj[prop], queue, name + "/" + prop);
                    else
                        propdesc.value = undefined;
                    desc[prop] = propdesc;
                }
                strategy.addObject(id, desc);
            }
            return valdesc;
        }

        /*
        function describe_level(obj, desc) {
            var props = Object.getOwnPropertyNames(obj);
            for (var i = 0; i < props.length; i++) {
                var prop = props[i];
                if (filter_special(obj === this, prop))
                    continue;
                var propdesc = Object.getOwnPropertyDescriptor(obj, prop);
                if (propdesc == undefined)
                    propdesc = {};
                var skip_value = false;
                if (propdesc["get"]) {
                    propdesc.get = valid(propdesc.get)
                        skip_value = true;
                }
                if (propdesc["set"]) {
                    propdesc.set = valid(propdesc.set)
                }
                if (!skip_value)
                    propdesc.value = valid(obj[prop]);
                else
                    propdesc.value = undefined;
                desc[prop] = propdesc;
            }
            var proto = Object.getPrototypeOf(obj);
            if (proto !== null && proto !== Object.getPrototypeOf(obj))
                return describe_level(obj, desc);
            else
                return desc;
        }

        function describeobj(obj) {
            return describe_level(obj, {});
        }
        */
        function funcid(obj) {
            // We know that obj is of type function
            if (functions.has(obj)) {
                return functions.get(obj);
            } else {
                var id = funids++;
                functions.set(obj, id);
                strategy.addFunction(id, {
                    instrumented : obj.toString(),
                    obj : valid(obj)
                });
                return id;
            }
        }

        /*
        function valid(obj) {
            switch (typeof obj) {
                case "undefined":
                    return {
                        type : "undefined"
                    };
                case "boolean":
                case "number":
                case "string":
                case "symbol":
                    return {
                        type : typeof obj,
                        val : obj.toString()
                    }
                case "function":
                    if (objects.has(obj)) {
                        return {
                            type : "function",
                            id : objects.get(obj),
                            funid : funcid(obj)
                        }
                    } else {
                        var id = valids++;
                        objects.set(obj, id);
                        strategy.addObject(id, describeobj(obj));
                        return {
                            type : "function",
                            id : id,
                            funid : funcid(obj)
                        }
                    }
                default:
                    if (obj === null) {
                        return {
                            type : "null"
                        }
                    } else if (objects.has(obj)) {
                        return {
                            type : typeof obj,
                            id : objects.get(obj)
                        }
                    } else if (typeof obj == "object") {
                        var id = valids++;
                        objects.set(obj, id);
                        strategy.addObject(id, describeobj(obj));
                        return {
                            type : typeof obj,
                            id : id
                        }
                    } else {
                        var id = valids++;
                        objects.set(obj, id);
                        return {
                            type : typeof obj,
                            id : id
                        }
                    }
            }
        }
        */

        this.invokeFunPre = function(iid, f, base, args, isConstructor,
                isMethod) {
            strategy.addStep({
                step : "funpre",
                iid : iid,
                f : valid(f),
                base : valid(base),
                args : valid(args),
                isConstructor : isConstructor,
                isMethod : isMethod
            });
        };

        this.invokeFun = function(iid, f, base, args, result, isConstructor,
                isMethod) {
            strategy.addStep({
                step : "funpost",
                iid : iid,
                f : valid(f),
                base : valid(base),
                args : valid(args),
                isConstructor : isConstructor,
                isMethod : isMethod,
                result : valid(result)
            });
        };

        this.literal = function(iid, val, hasGetterSetter) {
            // Special handling for function literals.
            var id = valid(val);
            strategy.addStep({
                step : "literal",
                iid : iid,
                val : id,
                hasGetterSetter : hasGetterSetter
            });
            if (typeof val == "function") {
                var data = J$.smap[J$.sid];
                if (data[iid]) {
                    var pos = data[iid].map(function(x) {
                        return x - 1
                    });
                    var lines = data.code.split("\n");
                    var text;
                    if (pos[0] == pos[2]) {
                        text = lines[pos[0]].substr(pos[1], pos[3] - pos[1]);
                    } else {
                        text = lines[pos[0]].substr(pos[1]);
                        for (var i = pos[0] + 1; i < pos[2]; i++) {
                            text += "\n" + lines[i];
                        }
                        text += "\n" + lines[pos[2]].substr(0, pos[3]);
                    }
                    strategy.functionCode(id, text);
                }
            }
        };

        this.forinObject = function(iid, val) {
            strategy.addStep({
                step : "forin",
                iid : iid,
                val : valid(val)
            });
        };

        this.declare = function(iid, name, val, isArgument, argumentIndex,
                isCatchParam) {
            strategy.addStep({
                step : "declare",
                iid : iid,
                name : name,
                val : valid(val),
                isArgument : isArgument,
                argumentIndex : argumentIndex,
                isCatchParam : isCatchParam
            });
        };

        this.getFieldPre = function(iid, base, offset, isComputed, isOpAssign,
                isMethodCall) {
            strategy.addStep({
                step : "getpre",
                iid : iid,
                base : valid(base),
                offset : offset.toString(),
                isComputed : isComputed,
                isOpAssign : isOpAssign,
                isMethodCall : isMethodCall
            });
        };

        this.getField = function(iid, base, offset, val, isComputed,
                isOpAssign, isMethodCall) {
            strategy.addStep({
                step : "getpost",
                iid : iid,
                base : valid(base),
                offset : offset.toString(),
                val : valid(val),
                isComputed : isComputed,
                isOpAssign : isOpAssign,
                isMethodCall : isMethodCall
            });
        };

        this.putFieldPre = function(iid, base, offset, val, isComputed,
                isOpAssign) {
            strategy.addStep({
                step : "putpre",
                iid : iid,
                base : valid(base),
                offset : offset.toString(),
                val : valid(val),
                isComputed : isComputed,
                isOpAssign : isOpAssign
            });
        };

        this.putField = function(iid, base, offset, val, isComputed, isOpAssign) {
            strategy.addStep({
                step : "putpost",
                iid : iid,
                base : valid(base),
                offset : offset.toString(),
                val : valid(val),
                isComputed : isComputed,
                isOpAssign : isOpAssign
            });
        };

        this.read = function(iid, name, val, isGlobal, isScriptLocal) {
            strategy.addStep({
                step : "read",
                iid : iid,
                name : name,
                val : valid(val),
                isGlobal : isGlobal,
                isScriptLocal : isScriptLocal
            });
        };

        this.write = function(iid, name, val, lhs, isGlobal, isScriptLocal) {
            strategy.addStep({
                step : "write",
                iid : iid,
                name : name,
                val : valid(val),
                lhs : valid(lhs),
                isGlobal : isGlobal,
                isScriptLocal : isScriptLocal
            });
        };

        this._return = function(iid, val) {
            strategy.addStep({
                step : "return",
                iid : iid,
                val : valid(val)
            });
        };

        this._throw = function(iid, val) {
            strategy.addStep({
                step : "throw",
                iid : iid,
                val : valid(val)
            });
        };

        this.functionEnter = function(iid, f, dis, args) {
            strategy.addStep({
                step : "funcenter",
                iid : iid,
                f : valid(f),
                "this" : valid(dis),
                args : valid(args)
            });
        };

        this.functionExit = function(iid, returnVal, wrappedExceptionVal) {
            strategy.addStep({
                step : "funcexit",
                iid : iid,
                ret : valid(returnVal),
                exc : valid(wrappedExceptionVal)
            });
        };

        this.scriptEnter = function(iid, instrumentedFileName, originalFileName) {
            strategy.addStep({
                step : "scriptenter"
            });
        };

        this.scriptExit = function(iid, wrappedExceptionVal) {
            if (wrappedExceptionVal === undefined) {
                strategy.addStep({
                    step : "scriptexit"
                });
            } else {
                strategy.addStep({
                    step : "scriptexc",
                    exc : valid(wrappedExceptionVal)
                });
            }
        };

        this.binaryPre = function(iid, op, left, right, isOpAssign,
                isSwitchCaseComparison, isComputed) {
            strategy.addStep({
                step : "binarypre",
                iid : iid,
                op : op,
                left : valid(left),
                right : valid(right),
                isOpAssign : isOpAssign,
                isSwitchComparison : isSwitchCaseComparison,
                isComputed : isComputed
            });
        };

        this.binary = function(iid, op, left, right, result, isOpAssign,
                isSwitchCaseComparison, isComputed) {
            strategy.addStep({
                step : "binarypost",
                iid : iid,
                op : op,
                left : valid(left),
                right : valid(right),
                isOpAssign : isOpAssign,
                isSwitchComparison : isSwitchCaseComparison,
                isComputed : isComputed,
                result : valid(result)
            });
        };

        this.unaryPre = function(iid, op, left) {
            strategy.addStep({
                step : "unarypre",
                iid : iid,
                op : op,
                left : valid(left)
            });
        };

        this.unary = function(iid, op, left, result) {
            strategy.addStep({
                step : "unarypost",
                iid : iid,
                op : op,
                left : valid(left),
                result : valid(result)
            });
        };

        this.conditional = function(iid, result) {
            strategy.addStep({
                step : "conditional",
                iid : iid,
                result : valid(result)
            });
        };

        this.endExpression = function(iid) {
            strategy.addStep({
                step : "exprend",
                iid : iid
            });
        };

        this.endExecution = function() {
            strategy.end();
        };

        this._with = function(iid, val) {
            strategy.addStep({
                step : "with",
                iid : iid,
                val : val
            });
        };
    }

    function consoleJSONStrategy(gap, globals) {
        var object_array = [];
        var function_array = [];
        var trace = [];
        function pad(array, n) {
            while (array.length <= n) {
                array.push(undefined);
            }
        }
        var strategy = {};
        strategy.addFunction = function (id, desc) {
            pad(function_array, id);
            function_array[id] = desc;
        };
        strategy.addObject = function  (id, desc) {
            pad(object_array, id);
            object_array[id] = desc;
        };
        strategy.functionCode = function (id, code) {
            function_array[id.funid].uninstrumented = code;
        }
        strategy.addStep = function (step) { trace.push(step); };
        strategy.end = function () {
            console.log(JSON.stringify({
                globals_are_properties: gap,
                globals: globals,
                obj: object_array,
                func: function_array,
                trace: trace
            }));
        };
        return strategy;
    }

    function debugStrategy(gap, globals) {
        var Console = require('console').Console;
        var fs = require('fs');
        var output = fs.createWriteStream("/dev/stdout");
        var console = new Console(output, output);
        console.log("globals are properties: " + gap);
        console.log("globals: " + JSON.stringify(globals));
        var strategy = {};
        strategy.addFunction = function (id, desc) {
            console.log("Adding function " + id + ": " + JSON.stringify(desc));
        }
        strategy.addObject = function (id, desc) {
            console.log("Adding object " + id + ": " + JSON.stringify(desc));
        }
        strategy.functionCode = function (id, code) {
            console.log("Adding uninstrumented code to " + id.funid + ": " + code);
        }
        strategy.addStep = function(step) {
            console.log("Adding trace step: " + JSON.stringify(step));
        }
        strategy.end = function () {
            console.log("Tracing finished");
        }
        return strategy;
    }

    function dumpEnvStrategy() {
        console.log(JSON.stringify(Object.getOwnPropertyNames(this)));

    }

    function xhrStrategy(gap, globals) {
        if (!global.XMLHttpRequest) {
            XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;
        }
        var xhr = new XMLHttpRequest();
        var urlbase =
            "http://" +
            (J$.initParams.host || "localhost") + ":" +
            (J$.initParams.port || "8080") + "/";
        xhr.open("POST", urlbase + "new", false);
        xhr.send(JSON.stringify([gap, globals]));
        if (xhr.readyState != 4) { throw new Exception("XHR session initialisation failed"); }
        var session = xhr.response;
        var url = urlbase + session + "/facts";
        var facts = [];

        var canSend = true;
        var timeoutId;
        var maxLength = 20;
        var timeout = 100;

        function sendXHR() {
            if (facts != []) {
                canSend = false;
                xhr = new XMLHttpRequest();
                xhr.open("POST", url, true);
                xhr.onreadystatechange = sendXHRCallback;
                xhr.send(JSON.stringify(facts));
                facts = [];
            }
        }

        function sendXHRCallback() {
            if (xhr.readyState >= 2) {
                canSend = true;
                sendStateMachine(false);
            }
        }

        function sendStateMachine(pushy) {
            if (canSend) {
                if (facts.length == 0) {
                    if (timeoutId) {
                        clearTimeout(timeoutId);
                        timeoutId = undefined;
                    }
                } else if (facts.length < maxLength && !pushy) {
                    if (!timeoutId)
                        timeoutId = setTimeout(sendXHR(), timeout);
                } else if (facts.length >= maxLength || pushy) {
                    if (timeoutId) {
                        clearTimeout(timeoutId);
                        timeoutId = undefined;
                    }
                    sendXHR();
                }
            }
        }

        function sendFact(fact, pushy) {
            facts.push(fact);
            sendStateMachine(pushy);
        }

        var strategy = {};
        strategy.addFunction = function (id, desc) {
            sendFact([ "function", id, desc ]);
        }
        strategy.addObject = function (id, desc) {
            sendFact([ "object", id, desc ]);
        }
        strategy.functionCode = function (id, code) {
            sendFact([ "function-uninstrumented", id, code ]);
        }
        strategy.addStep = function(step) {
            sendFact([ "step", step ]);
        }
        strategy.end = function () {
            sendFact([ "end" ], true);
        }
        return strategy;
    }

    var strategies = {
        debug: debugStrategy,
        console: consoleJSONStrategy,
        dumpEnv: dumpEnvStrategy,
        xhr: xhrStrategy
    }
    var whichStrategy = J$.initParams.strategy || "debug";
    sandbox.analysis = new GenericAnalysis(this, strategies[whichStrategy]);
})(J$);
