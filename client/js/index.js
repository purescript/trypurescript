$(function() {

    var editor = ace.edit('code');

    editor.renderer.setShowGutter(true);
    editor.setFontSize(14);
    editor.setShowPrintMargin(false);

    var session = editor.getSession();

    session.setMode('ace/mode/haskell');
    session.setValue($('#textarea').val());
    session.setUseWrapMode(true);
    session.on('change', function(){

        $('#textarea').val(editor.getSession().getValue());
    });

    $('#code').css({
        'min-height': '250px'
    });

    $('#compile').click(function() {

        var code = $('#textarea').val();

        $.ajax({
            url: 'http://localhost:8081/compile',
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
                        var $iframe = $('<iframe>');

                        $('.results').empty()
                            .append($iframe);

                        var iframe = $iframe.get(0).contentWindow.document;

                        iframe.open();
                        iframe.write(
                            [ '<html>'
                            , '  <head>'
                            , '    <title>PureScript Playground</title>'
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
                            ].join('\n');

                        var replaced = res.js.replace(/require\("[^"]*"\)/g, function(s) {

                            return "PS['" + s.substring(9, s.length - 2) + "']";
                        });

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
                    });
                }
            },
            error: function(res) {
                $('.results').empty()
                    .append($('<h2>').append('Error'))
                    .append($('<pre>').append($('<code>').append(res.responseText)));
            }
        });
    });
});
