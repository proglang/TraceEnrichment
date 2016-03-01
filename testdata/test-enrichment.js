var a = 5;
var x = 0;
function outer() {
    var x = 1;
    function inner() {
        var z = 4;
        return x + y + z;
    }
    var y = 2;
    var z = 3;
    return inner;
}
console.log(outer()());
if (-1 + 2 > 0) {
    function throwing() {
        throw new Error("Generic error");
    }
    try {
        eval("0;");
        eval("throwing()");
    } catch (e) {
        var c = {};
        c.exn = e;
    }
}
function cons () {
    return { meth: function () { this.x = 1 } }
}
(new cons()).meth();

