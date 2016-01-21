(function($) {
    $.QueryString = (function(a) {
        if (a == "") return {};
        var b = {};
        for (var i = 0; i < a.length; ++i) {
            var p=a[i].split('=');
            if (p.length != 2) continue;
            b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
        }
        return b;
    })(window.location.search.substr(1).split('&'))
})(jQuery);

$(function() {

    var editor, session;

    var setupEditor = function() {

        editor = ace.edit('code');

        editor.renderer.setShowGutter(true);
        editor.setFontSize(14);
        editor.setShowPrintMargin(false);

        session = editor.getSession();

        session.setMode('ace/mode/haskell');
        session.setValue($('#textarea').val());
        session.setUseWrapMode(true);

        session.on('change', _.debounce(function() {

            compile();
        }, 500));

        compile();
    };

    var execute = function(js, bundle) {

        var $iframe = $('<iframe>');

        $('.results').empty().append($iframe);

        var iframe = $iframe.get(0).contentWindow.document;

        // TODO: make the HTML editable
        iframe.open();
        iframe.write(
            [ '<html>'
            , '  <head>'
            , '    <title>Try PureScript!</title>'
            , '  </head>'
            , '  <body>'
            , '    <div id="console"></div>'
            , '    <canvas id="canvas" width="800" height="800"></canvas>'
            , '  </body>'
            , '</html>'
            ].join('\n'));

        var consoleScript =
            [ 'var console = {'
            , '  log: function(s) {'
            , '    var text = document.createTextNode(s);'
            , '    var code = document.createElement("code");'
            , '    code.style.fontFamily = "Monaco, Menlo, Ubuntu Mono, Consolas, monospace"'
            , '    code.style.fontSize = "14px";'
            , '    var div = document.createElement("div");'
            , '    div.appendChild(code);'
            , '    code.appendChild(text);'
            , '    document.getElementById("console").appendChild(div);'
            , '  }'
            , '};'
            , 'window.onerror = function(e) {'
            , '  console.log(e);'
            , '  return true;'
            , '};'
            ].join('\n');

        // Replace any require() statements with the PS['...'] form using a regex substitution.
        var replaced = js.replace(/require\("[^"]*"\)/g, function(s) {

            return "PS['" + s.substring(9, s.length - 2) + "']";
        });

        // Wrap the compiled code so that main() runs.
        var wrapped =
            [ 'var module = {};'
            , '(function(module) {'
            , replaced
            , '})(module);'
            , 'module.exports.main && module.exports.main();'
            ].join('\n');

        var scripts = [consoleScript, bundle, wrapped].join("\n");

        var script = iframe.createElement('script');
        script.appendChild(iframe.createTextNode(scripts));
        var head = iframe.getElementsByTagName('head')[0];
        head.appendChild(script);
    };

    var compile = function() {

        var code = session.getValue();

        $.ajax({
            url: 'http://compile.purescript.org/compile',
            dataType: 'json',
            data: code,
            method: 'POST',
            contentType: 'text/plain',
            success: function(res) {
                if (res.error) {
                    $('.results')
                        .empty()
                        .append($('<pre>').append($('<code>').append(res.error)));
                } else if (res.js) {
                    $.get('js/bundle.js').done(function(bundle) {

                        execute(res.js, bundle);
                    }).fail(function(err) {

                        console.log("Unable to load JS bundle");
                    });
                }
            },
            error: function(res) {
                $('.results').empty()
                    .append($('<h2>').append('Error'))
                    .append($('<pre>').append($('<code>').append(res.responseText)));
            }
        });
    };

    var loadFromGist = function(id) {

        $.ajax({
            url: '//api.github.com/gists/' + id,
            dataType: 'json'
        }).done(function(gistInfo) {

            if (gistInfo.files && gistInfo.files.hasOwnProperty("Main.purs")) {

                var url = gistInfo.files["Main.purs"].raw_url;

                $.ajax({
                    url: url,
                    dataType: 'text'
                }).done(function(gistText) {

                    $('#textarea').val(gistText);
                    setupEditor();
                }).fail(function() {

                    console.log("Unable to load gist text");
                    setupEditor();
                });
            } else {
                console.log("File named Main.purs does not exist in gist");
            }
        }).fail(function() {

            console.log("Unable to load gist metadata");
            setupEditor();
        });
    };

    var gist = $.QueryString["gist"];

    if (gist) {
        loadFromGist(gist);
    } else {
        setupEditor();
    }
});
