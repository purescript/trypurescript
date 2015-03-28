$(function() {
    
    var editor = ace.edit('code');

    editor.setTheme('ace/theme/dawn');
    editor.renderer.setShowGutter(false);

    var session = editor.getSession();

    session.setMode('ace/mode/haskell');
    session.setValue($('#textarea').val());
    session.setUseWrapMode(true);
    session.on('change', function(){
      $('#textarea').val(editor.getSession().getValue());
    });

    $('#code').height('250px');
    
    $('#compile').click(function() {
        
        var code = $('#textarea').val();
        
        $.ajax({
            url: '/compile/text',
            dataType: 'text',
            data: code,
            method: 'POST',
            success: function(res) {
                var $iframe = $('<iframe>');
            
                $('#results').empty()
                    .append($('<h2>').append('Compiled Result'))
                    .append($iframe);
                    
                $('html, body').animate({
                    scrollTop: $iframe.offset().top
                }, 500);
                
                var iframe = $iframe.get(0).contentWindow.document;
                
                iframe.open();
                iframe.write(
                    [ '<html>'
                    , '  <head>'
                    , '    <title>Try PureScript</title>'
                    , '  </head>'
                    , '  <body>'
                    , '    <div id="console"></div>'
                    , '  </body>'
                    , '</html>'
                    ].join('\n'));
                    
                var initScript = 
                    [ 'var console = {'
                    , '  log: function(s) {'
                    , '    var text = document.createTextNode(s);'
                    , '    var code = document.createElement("code");'
                    , '    var div = document.createElement("div");'
                    , '    div.appendChild(code);'
                    , '    code.appendChild(text);'
                    , '    document.getElementById("console").appendChild(div);'
                    , '  }'
                    , '};'
                    ].join('\n');
                
                var scripts = [initScript, res];
                
                for (var i = 0; i < scripts.length; i++) {
                    var script = iframe.createElement('script');
                    script.appendChild(iframe.createTextNode(scripts[i]));
                    var head = iframe.getElementsByTagName('head')[0];
                    head.appendChild(script);
                }
            },
            error: function(res) {
                $('#results').empty()
                    .append($('<h2>').append('Error'))
                    .append($('<pre>').append($('<code>').append(res.responseText)));
            }
        });
    });
});