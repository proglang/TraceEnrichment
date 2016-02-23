var valid = [];
var invalid = [];
var nonobject = [];
var global = (function () { return this; })();
var props = Object.getOwnPropertyNames(global);
var wmap = new WeakMap();
for (var i = 0; i < props.length; i++) {
    var prop = props[i];
    var value = global[prop];
    var type = typeof value;
    if ((type === 'object' || type === 'function') && value != null) {
        try {
            wmap.set(global[prop], {});
            valid.push(prop);
        } catch (e) {
            invalid.push(prop + " (" + e + ")");
        }
    } else {
        nonobject.push(prop + " (" + type + ")");
    }
}
console.log("valid: " + valid.join(", "));
console.log("invalid: " + invalid.join(", "));
console.log("non-object: " + nonobject.join(", "));

