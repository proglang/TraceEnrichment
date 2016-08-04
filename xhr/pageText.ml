(* vim: set ft=htmljinja: *)
let trace_collector_index = {html|
<html>
    <head>
        <title>JSCollector API server - overview</title>
    </head>
    <body>
        <h1>Global operations</h1>
        <form action="shutdown" method=post>
            <button type=submit>Shut down server</button>
        </form>
        {% for operation in global_operations %}
        {% if operation.post %}
        <form action="{{operation.path}}" method=post>
            <button type=submit>{{operation.name}}</button>
        </form>
        {% else %}
        <a href="{{operation.path}}">{{operation.name}}</a>
        {% endif %}
        {% endfor %}
        <h1>Instrumented files</h1>
        {% for file in instrumented_files %}
        <a href="{{file}}">{{file}}</a>
        {% endfor %}
        <h1>Traces</h1>
        <table>
            {% for session in sessions %}
            <tr>
                <td>{{session}}</td>
                {% for operation in local_operations %}
                <td>
                    {% if operation.post %}
                    <form action="{{session}}/{{operation.path}}" method=post>
                        <button type=submit>{{operation.name}}</button>
                    </form>
                    {% else %}
                    <a href="{{session}}/{{operation.path}}">{{operation.name}}</a>
                    {% endif %}
                </td>
                {% endfor %}
            </tr>
            {% endfor %}
        </table>
    </body>
</html>
|html}

let operation_menu = {html|
<html>
    <head>
        <title>JSCollector API server - trace operations</title>
    </head>
    <body>
        <h1>Operations on trace ${session}</h1>
        <ul>
            {% for operation in local_operations %}
            <p>
                {% if operation.post %}
                <form action="{{session}}/{{operation.path}}" method=post>
                    <button type=submit>{{operation.name}}</button>
                </form>
                {% else %}
                <a href="{{session}}/{{operation.path}}">{{operation.name}}</a>
                {% endif %}
            </p>
            {% endfor %}
        </ul>
    </body>
</html>
|html}

let analysis_driver = {html|
<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
        <title>Dynamic analysis for {{basename}}</title>
        <script>
// JALANGI DO NOT INSTRUMENT
window.onload = function () {
    J$.endExecution();
    if (J$.next_page) {
        window.location.assign(J$.next_page);
    }
}
        </script>
    </head>
    <body>
        <script src="{{basename}}.js"></script>
        <span id="status">Script running...</span>
    </body>
</html>
|html}
