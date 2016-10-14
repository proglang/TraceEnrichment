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
        console.log("Instantiating generic analysis");
        var objects = new AlmostWeakMap();
        var functions = new AlmostWeakMap();
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
        // global object has index 0 all over the place.
        // The second argument is used to control the fields that get added.
        // In particular, we use it to exclude debris from the instrumentation.
        // Fill in descriptions for standard library objects if missing. XXX do we need to do something here?
        console.log("About to build strategy");
        var strategy = strategyBuilder(global.J$ === J$, { global: { type: typeof global, id: 0 } });
        var initialized = false;
        var currentSID;
        var sendIIDs;

        function initialize() {
            if (!initialized) {
                valid(global);
                strategy.start();
                currentSID = undefined;
                sendIIDs = [];
                initialized = true;
            }
        }

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
                        type : typeof val,
                        val : val.toString()
                    }
                case "function":
                    var desc = writeobj(val, queue, name);
                    if (functions.has(val)) {
                        desc.funid = functions.get(val);
                    } else {
                        var id = funids++;
                        functions.set(val, id);
                        var fdesc = {
                            instrumented: val.toString(),
                            obj: desc.id
                        };
                        strategy.addFunction(id, fdesc);
                        desc.funid = id;
                    }
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
        function getDescription(queue, name, obj, prop) {
            var propdesc = Object.getOwnPropertyDescriptor(obj, prop) || {};
            if (propdesc.get) propdesc.get = writeval(propdesc.get, queue, name + "/get:" + prop);
            if (propdesc.set) propdesc.set = writeval(propdesc.set, queue, name + "/set:" + prop);
            /* Note that the semantics depend on whether a getter exists or not.
               If a getter exists, this will only be used for the initial object graph. */
            try {
                propdesc.value = writeval(obj[prop], queue, name + "/" + prop);
            } catch (e) {
                // FIXME: We actually need an additional type here: "getter failed".
                propdesc.value = { type: "undefined" }
            }
            return propdesc;
        }

        function valid(val) {
            var handled = new Set();
            var queue = [];
            var valdesc = writeval(val, queue, "./");
            var objid;
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
                    desc[prop] = getDescription(queue, name, obj, prop);
                }
                if (!desc.prototype) {
                    desc.prototype = { value: writeval(Object.getPrototypeOf(obj), queue, name + "/prototype") };
                }
                strategy.addObject(id, desc);
            }
            return valdesc;
        }

        function funcid(obj) {
            // We know that obj is of type function
        }

        function addStep(message) {
            initialize();
            if (J$.sid != currentSID) {
                currentSID = J$.sid;
                strategy.sendSID(currentSID);
                if (sendIIDs.indexOf(currentSID) == -1) {
                    sendIIDs.push(currentSID);
                    strategy.addIIDMap(currentSID, J$.smap[currentSID] || {});
                }
            }
            strategy.addStep(message);
        }
        console.log("Preparing dynamic analysis");
        this.invokeFunPre = function(iid, f, base, args, isConstructor,
                isMethod) {
            addStep({
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
            addStep({
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
            initialize();
            var id = valid(val);
            addStep({
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
            addStep({
                step : "forin",
                iid : iid,
                val : valid(val)
            });
        };

        this.declare = function(iid, name, val, isArgument, argumentIndex,
                isCatchParam) {
            addStep({
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
            addStep({
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
            addStep({
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
            addStep({
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
            addStep({
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
            addStep({
                step : "read",
                iid : iid,
                name : name,
                val : valid(val),
                isGlobal : isGlobal,
                isScriptLocal : isScriptLocal
            });
        };

        this.write = function(iid, name, val, lhs, isGlobal, isScriptLocal) {
            addStep({
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
            addStep({
                step : "return",
                iid : iid,
                val : valid(val)
            });
        };

        this._throw = function(iid, val) {
            addStep({
                step : "throw",
                iid : iid,
                val : valid(val)
            });
        };

        this.functionEnter = function(iid, f, dis, args) {
            addStep({
                step : "funcenter",
                iid : iid,
                f : valid(f),
                "this" : valid(dis),
                args : valid(args)
            });
        };

        this.functionExit = function(iid, returnVal, wrappedExceptionVal) {
            addStep({
                step : "funcexit",
                iid : iid,
                ret : valid(returnVal),
                exc : valid(wrappedExceptionVal)
            });
        };

        this.scriptEnter = function(iid, instrumentedFileName, originalFileName) {
            addStep({
                step : "scriptenter"
            });
        };

        this.scriptExit = function(iid, wrappedExceptionVal) {
            if (wrappedExceptionVal === undefined) {
                addStep({
                    step : "scriptexit"
                });
            } else {
                addStep({
                    step : "scriptexc",
                    exc : valid(wrappedExceptionVal)
                });
            }
            return { wrappedExceptionVal: wrappedExceptionVal, isBacktrack: false }
        };

        this.binaryPre = function(iid, op, left, right, isOpAssign,
                isSwitchCaseComparison, isComputed) {
            addStep({
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
            addStep({
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
            addStep({
                step : "unarypre",
                iid : iid,
                op : op,
                left : valid(left)
            });
        };

        this.unary = function(iid, op, left, result) {
            addStep({
                step : "unarypost",
                iid : iid,
                op : op,
                left : valid(left),
                result : valid(result)
            });
        };

        this.conditional = function(iid, result) {
            addStep({
                step : "conditional",
                iid : iid,
                result : valid(result)
            });
        };

        this.endExpression = function(iid) {
            addStep({
                step : "exprend",
                iid : iid
            });
        };

        this.endExecution = function() {
            strategy.end();
        };

        this._with = function(iid, val) {
            addStep({
                step : "with",
                iid : iid,
                val : val
            });
        };
        console.log("Starting dynamic analysis");
    }

    function consoleJSONStrategy(gap, globals) {
        var object_array = [];
        var function_array = [];
        var trace = [];
        var iids = {};
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
                trace: trace,
                iid: iids
            }));
        };
        strategy.sendSID = function (sid) {
            trace.push({ step: "switchscript", sid: sid })
        }
        strategy.addIIDMap = function (sid, iidmap) {
            iids[sid] = iidmap
        }
        strategy.start = function () { };
        return strategy;
    }

    function debugStrategy(gap, globals) {
        var console;
        if (typeof window === 'undefined') {
            var Console = require('console').Console;
            var fs = require('fs');
            var output = fs.createWriteStream("/dev/stdout");
            console = new Console(output, output);
        } else {
            console = window.console;
        }
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
        strategy.start = function () {
            console.log("Tracing started");
        };
        strategy.sendSID = function (sid) {
            console.log("Switching to SID " + sid);
        };
        strategy.addIIDMap = function(sid, iidmap) {
            console.log("IID map for " + sid + ": " + JSON.stringify(iidmap));
        }
        return strategy;
    }

    function htmlStrategy(gap, globals) {
        console.log("Setting up");
        var trace = null;
        var strategy = {};
        function setTrace() {
            //document.getElementById("GAP").appendChild(document.createTextNode(gap));
            trace = document.getElementById("trace");
        }
        function format_value(val) {
            var desc = val.value;
            if (desc == undefined) {
                return document.createTextNode("[value with getter]");
            } else {
                switch (desc.type) {
                    case "null":
                        return document.createTextNode("null");
                    case "undefined":
                        return document.createTextNode("undefined");
                    case "boolean":
                    case "number":
                        return document.createTextNode(desc.val);
                    case "string":
                        var inner = document.createElement("pre");
                        inner.appendChild(document.createTextNode(desc.val));
                        return inner;
                    case "symbol":
                        return document.createTextNode("symbol " + desc.val);
                    case "function":
                        return document.createTextNode("function " + desc.id + "/" + desc.funid);
                    case "object":
                        return document.createTextNode("object " + desc.id);
                    default:
                        return document.createTextNode(desc.type + " " + desc.id);
                }
            }
        }
        strategy.addFunction = function (id, desc) {
            setTrace();
            var data = document.createElement("li");
            data.appendChild(document.createTextNode("Adding function " + id + ": object " + desc.obj + ", instrumented code: "));
            var pre = document.createElement("pre");
            pre.appendChild(document.createTextNode(desc.instrumented));
            data.appendChild(pre);
            trace.appendChild(data);
        }
        strategy.addObject = function (id, desc) {
            setTrace();
            var data = document.createElement("dl");
            var keys = Object.keys(desc);
            var i;
            for (i = 0; i < keys.length; i++) {
                var key = keys[i];
                var inner = document.createTextNode(key);
                var outer = document.createElement("dt");
                outer.appendChild(inner);
                data.appendChild(outer);
                inner = format_value(desc[key]);
                outer = document.createElement("dd");
                outer.appendChild(inner);
                data.appendChild(outer);
            }
            var entry = document.createElement("li");
            entry.appendChild(document.createTextNode("Adding object " + id));
            entry.appendChild(data);
            trace.appendChild(entry);
        }
        strategy.functionCode = function (id, code) {
            setTrace();
            var data = document.createElement("li");
            data.appendChild("Adding uninstrumented code to " + id.funid + ": ");
            var pre = document.createElement("pre");
            pre.appendChild(document.createTextNode(code));
            data.appendChild(pre);
            trace.appendChild(data);
        }
        strategy.addStep = function(step) {
            setTrace();
            var data = document.createElement("dl");
            var keys = Object.keys(step);
            var i;
            for (i = 0; i < keys.length; i++) {
                var key = keys[i];
                var value = step[key];
                var inner = document.createTextNode(key);
                var outer = document.createElement("dt");
                outer.appendChild(inner);
                data.appendChild(outer);
                inner = document.createTextNode(value);
                outer = document.createElement("dd");
                outer.appendChild(inner);
                data.appendChild(outer);
            }
            var entry = document.createElement("li");
            entry.appendChild(document.createTextNode("Adding step"));
            entry.appendChild(data);
            trace.appendChild(entry);
        }
        strategy.end = function () {
            setTrace();
            var entry = document.createElement("li");
            entry.appendChild(document.createTextNode("Tracing finished"));
            trace.appendChild(entry);
        }
        strategy.start = function () {
            setTrace();
            var entry = document.createElement("li");
            entry.appendChild(document.createTextNode("Tracing started"));
            trace.appendChild(entry);
        };
        strategy.sendSID = function (sid) {
            setTrace();
            var entry = document.createElement("li");
            entry.appendChild(document.createTextNode("Switching to SID " + sid));
            trace.appendChild(entry);
        };
        strategy.addIIDMap = function(sid, iidmap) {
            setTrace();
            var entry = document.createElement("li");
            entry.appendChild(document.createTextNode("IID map"));
            trace.appendChild(entry);
        }
        return strategy;
    }

    function xhrStrategy(gap, globals) {
        console.log("Instantiating XHR strategy");
        if (!window.XMLHttpRequest) {
            XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;
        }
        var xhr = new XMLHttpRequest();
        console.log("Found XHR object");
        var urlbase =
            "http://" +
            (J$.initParams.host || "localhost") + ":" +
            (J$.initParams.port || "8080") + "/";
        console.log("URL base: " + urlbase);
        xhr.open("POST", urlbase + "new", false);
        xhr.send(JSON.stringify([gap, globals]));
        if (xhr.readyState != 4) { throw new Exception("XHR session initialisation failed"); }
        var session = xhr.response;
        var url = urlbase + session + "/facts";
        J$.initParams.session_url = urlbase + session;
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
            } else {
                console.log("sendXHR called with empty fact set");
            }
        }

        function sendXHRCallback() {
            console.log("In XHR callback, state: " + xhr.readyState);
            if (xhr.readyState >= 2) {
                console.log("State allows sending, call state machine");
                canSend = true;
                sendStateMachine(false);
            }
        }

        function sendStateMachine(pushy) {
            if (canSend) {
                console.log("Can send, handling cases");
                if (facts.length == 0) {
                    console.log("No facts, clearing timeout");
                    if (timeoutId) {
                        clearTimeout(timeoutId);
                        timeoutId = undefined;
                    }
                } else if (facts.length < maxLength && !pushy) {
                    console.log("Only few facts and not pushy, set timeout for flush");
                    if (!timeoutId)
                        timeoutId = setTimeout(sendXHR(), timeout);
                } else if (facts.length >= maxLength || pushy) {
                    console.log("Enough facts for immediate sending, clear timeout and send");
                    if (timeoutId) {
                        clearTimeout(timeoutId);
                        timeoutId = undefined;
                    }
                    sendXHR();
                }
            }
        }

        function sendFact(fact, pushy) {
            console.log("Adding fact: " + JSON.stringify(fact));
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
            J$.next_page = urlbase + session
        }
        strategy.start = function () {
            sendFact([ "start" ], true);
        }
        strategy.sendSID = function (sid) {
            strategy.addStep({ step: "switchscript", sid: sid })
        }
        strategy.addIIDMap = function (sid, iidmap) {
            sendFact([ "iidmap", sid, iidmap ])
        }
        return strategy;
    }

    var strategies = {
        debug: debugStrategy,
        console: consoleJSONStrategy,
        xhr: xhrStrategy,
        html: htmlStrategy
    }
    var whichStrategy = J$.initParams.strategy || "debug";
    sandbox.analysis = new GenericAnalysis(this, strategies[whichStrategy]);
})(J$);
