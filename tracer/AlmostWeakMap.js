// JALANGI DO NOT INSTRUMENT
/* A best-effort weak map from objects to values.
 * Where possible, keys and values are mapped
 * weakly, but for those cases where the weak
 * map fails, a strong mapping is stored instead.
 */

var exports = (function() {
    /* Check if WeakMap is available */
    try { new WeakMap(); } catch (e) {
        var WeakMap = function () {}
        WeakMap.prototype = {
            get: function (key) { return undefined },
            has: function (key) { return false },
            set: function (key, data) { throw new TypeError("WeakMap not supported") }
        }
    }
    var get = function (key) {
        var result = this.map.get(key);
        if (result) return result;
        result = this.fallback.find(function (kvpair) { return (kvpair[0] === key) });
        if (result != undefined) return result[1];
        return undefined;
    }
    var has = function (key) {
        if (this.map.has(key)) return true;
        return this.fallback.some(function (kvpair) { return (kvpair[0] === key) });
    }
    var set = function (key, value) {
        try {
            this.map.set(key, value);
            return this;
        } catch (e) {
            if (!(e instanceof TypeError)) throw e;
            if (typeof key != "object" && typeof key != "function") throw new TypeError("Not an object");
            for (var i = 0; i < this.fallback.length; i++) {
                if (this.fallback[i][0] === key) {
                    this.fallback[i][1] = value;
                    return this;
                }
            }
            this.fallback.push([key, value]);
            return this;
        }
    };
    function AlmostWeakMap () {
        return {
            map: new WeakMap(),
            fallback: new Array(),
            get: get,
            set: set,
            has: has
        }
    }
    return { AlmostWeakMap: AlmostWeakMap };
})();

AlmostWeakMap = exports.AlmostWeakMap;
