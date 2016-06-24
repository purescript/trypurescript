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
    var loadOptions = function() {
      var view_mode = $.QueryString["view"];
      if (view_mode && (view_mode === "sidebyside" || view_mode === "code" || view_mode === "output")) {
        $('#view_' + view_mode).click();
      }

      var showjs = $.QueryString["js"];
      if (showjs) {
        $('input:checkbox[name=showjs]').prop('checked', showjs === "true");
      }
      var auto_compile = $.QueryString["compile"];
      if (auto_compile) {
        $('input:checkbox[name=auto_compile]').prop('checked', auto_compile === "true");
      }
    };


    var setupEditorWith = function(name, ta_name, lang) {

        var editor = ace.edit(name);

        editor.renderer.setShowGutter(true);
        editor.setFontSize(13);
        editor.setShowPrintMargin(false);

        var session = editor.getSession();

        session.setMode(lang);
        session.setValue($('#' + ta_name).val());
        session.setOptions({ tabSize: 2, useSoftTabs: true });

        session.on('change', _.debounce(function() {

            $('#' + ta_name).val(session.getValue());
            if ($("#auto_compile").is(":checked")) {
              compile();
            }
        }, 750));

        compile();
    };

    var setupEditor = function() {

        loadOptions();
        setupEditorWith('code', 'code_textarea', 'ace/mode/haskell');
    };

    var execute = function(js, bundle) {

        var $iframe = $('<iframe>');

        $('#column2')
            .empty()
            .append($iframe);

        var iframe = $iframe.get(0).contentWindow.document;

        iframe.open();
        iframe.write(
            [ '<!DOCTYPE html>'
            , '<html>'
            , '  <head>'
            , '    <title>Try PureScript!</title>'
            , '    <link rel="stylesheet" href="css/style.css">'
            , '  </head>'
            , '  <body>'
            , '    <div id="console"></div>'
            , '  </body>'
            , '</html>'
            ].join('\n')
        );

        var consoleScript =
            [ 'console.log = function(s) {'
            , '  var div = document.createElement("div");'
            , '  div.appendChild(document.createTextNode(s));'
            , '  div.innerHTML = div.innerHTML.replace(/\\?gist=([A-Fa-f0-9]+)/g, "<a href=\'?gist=$1\' target=\'_top\'>$1</a>");'
            , '  var cons = document.getElementById("console");'
            , '  cons && cons.appendChild(div);'
            , '};'
            , 'window.onerror = function(e) {'
            , '  console.log(e);'
            , '  return true;'
            , '};'
            ].join('\n');

        // Replace any require() statements with the PS['...'] form using a regex substitution.
        var replaced = js.replace(/require\("[^"]*"\)/g, function(s) {

            return "PS['" + s.substring(12, s.length - 2) + "']";
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

        $('#column2')
            .empty()
            .append($("<div>").addClass("loading").append("Loading..."));

        var code = $('#code_textarea').val();

        $.ajax({
            url: 'https://compile.purescript.org/compile',
            dataType: 'json',
            data: code,
            method: 'POST',
            contentType: 'text/plain',
            success: function(res) {

                if (res.error) {
                    $('#column2')
                        .empty()
                        .append($('<pre>').append($('<code>').append(res.error)));
                } else if (res.js) {
                    if ($("#showjs").is(":checked")) {
                      $('#column2')
                          .empty()
                          .append($('<pre>').append($('<code>').append(res.js)));
                    } else {
                      $.get('js/bundle.js').done(function(bundle) {

                          execute(res.js, bundle);
                      }).fail(function(err) {

                          console.log("Unable to load JS bundle");
                      });
                    }
                }
            },
            error: function(res) {

                $('#column2')
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
            url: 'https://api.github.com/gists/' + id,
            dataType: 'json'
        }).done(function(gistInfo) {

            tryLoadFileFromGist(gistInfo, "Main.purs")
                .done(function(code) {

                    code && $('#code_textarea').val(code);
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


    $('#showjs').change(compile);
    $('#compile_label').click(compile);

    $('input[name=view_mode]').change(function () {
      var view_mode = $(this).filter(':checked').val();

      if (view_mode === "code") {
        $('#column1').show();
        $('#column2').hide();
        $('#showjs_label').hide();
        $('#showjs').hide();
      }
      else if (view_mode === "output") {
        $('#column1').hide();
        $('#column2').show();
        $('#showjs_label').show();
        $('#showjs').show();
      }
      else { // (view_mode === "sidebyside")
        $('#column1').show();
        $('#column2').show();
        $('#showjs_label').show();
        $('#showjs').show();
      }
    });

    var gist = $.QueryString["gist"];

    if (gist) {
        loadFromGist(gist);
    } else {
        setupEditor();
    }


});

