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

    var setupEditorWith = function(name, ta_name, lang) {

        var editor = ace.edit(name);

        editor.renderer.setShowGutter(true);
        editor.setFontSize(13);
        editor.setShowPrintMargin(false);

        var session = editor.getSession();

        session.setMode(lang);
        session.setValue($('#' + ta_name).val());
        session.setUseWrapMode(true);

        session.on('change', _.debounce(function() {

            $('#' + ta_name).val(session.getValue());
            compile();
        }, 500));

        compile();
    };

    var setupEditor = function() {

        setupEditorWith('code', 'code_textarea', 'ace/mode/haskell');
        setupEditorWith('html', 'html_textarea', 'ace/mode/html');
    };

    var execute = function(js, bundle) {

        var $iframe = $('<iframe>');

        $('#results')
            .css('flex', '1')
            .empty()
            .append($iframe);

        var iframe = $iframe.get(0).contentWindow.document;

        // TODO: make the HTML editable
        iframe.open();
        iframe.write($('#html_textarea').val());

        var consoleScript =
            [ 'var console = {'
            , '  log: function(s) {'
            , '    var text = document.createTextNode(s);'
            , '    var div = document.createElement("div");'
            , '    div.appendChild(text);'
            , '    var cons = document.getElementById("console");'
            , '    cons && cons.appendChild(div);'
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

        if (head) {
            head.appendChild(script);
        } else {
            console.log("<head> element is missing!");
        }
    };

    var compile = function() {

        $('#results')
            .css('flex', '0')
            .empty()
            .append($("<div>").addClass("loading").append("Loading..."));

        var code = $('#code_textarea').val();

        $.ajax({
            url: 'http://compile.purescript.org/compile',
            dataType: 'json',
            data: code,
            method: 'POST',
            contentType: 'text/plain',
            success: function(res) {

                if (res.error) {
                    $('#results')
                        .css('flex', '0')
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
                $('#results')
                    .css('flex', '0')
                    .empty()
                    .append($('<pre>').append($('<code>').append(res.responseText)));
            }
        });
    };

    var tryLoadFileFromGist = function(gistInfo, filename) {

        if (gistInfo.files && gistInfo.files.hasOwnProperty(filename)) {

            var url = gistInfo.files[filename].raw_url;

            return $.ajax({
                url: url,
                dataType: 'text'
            });
        } else {

            console.log("File named " + filename + " does not exist in gist");

            var promise = $.Deferred();
            promise.resolve(null);
            return promise;
        }
    };

    var loadFromGist = function(id) {

        $.ajax({
            url: '//api.github.com/gists/' + id,
            dataType: 'json'
        }).done(function(gistInfo) {

            $.when(tryLoadFileFromGist(gistInfo, "Main.purs"), tryLoadFileFromGist(gistInfo, "index.html"))
                .done(function(code, html) {

                    code && $('#code_textarea').val(code[0]);
                    html && $('#html_textarea').val(html[0]);
                    setupEditor();
                }).fail(function() {

                    console.log("Unable to load gist contents");
                    setupEditor();
                });
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
