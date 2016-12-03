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
    })(window.location.search.substr(1).split('&'));

    $.setQueryParameters = function(params) {
        var url = location.href.split('?')[0];
        var encodedParams = Object.keys(params).map(function(key) {
            return key + '=' + encodeURIComponent(params[key]);
        }).join('&');

        document.location = url + '?' + encodedParams;
    };

})(jQuery);

$(function() {
    var endpoint = "https://compile.purescript.org/try";

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

    var setupSession = function() {
        function guid() {
          function s4() {
            return Math.floor((1 + Math.random()) * 0x10000)
              .toString(16)
              .substring(1);
          }
          return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
            s4() + '-' + s4() + s4() + s4();
        }

        var sessionId = $.QueryString['session'];
        if (!sessionId) {
            $.QueryString['session'] = sessionId = guid();
            $.setQueryParameters($.QueryString);
        }

        return sessionId;
    };

    var cacheCurrentCode = function() {
        if (window.localStorage) {
            var sessionId = $.QueryString['session'];
            var code = $('#code_textarea').val();

            localStorage.setItem(sessionId, code);
        }
    };

    var tryRestoreCachedCode = function(sessionId) {
        var code;

        if (window.localStorage) {
            code = localStorage.getItem(sessionId);

            if (code) {
                $('#code_textarea').val(code);
                setupEditor();
            }
        }

        return !!code;
    }

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

            cacheCurrentCode();
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

        var scripts = [bundle, wrapped].join("\n");

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
            url: endpoint + '/compile',
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
                      $.get(endpoint + '/bundle').done(function(bundle) {

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
    var publishNewGist = function() {
        if (!confirm('Do you really want to publish this code as an annonymous Gist?\n\nNote: this code will be available to anyone with a link to the Gist.')) { return; }

        var data = {
            "description": "Published with try.purescript.org",
            "public": false,
            "files": {
                "Main.purs": {
                    "content": $('#code_textarea').val()
                }
            }
        };

        $.ajax({
            url: 'https://api.github.com/gists',
            type: 'POST',
            dataType: 'json',
            data: JSON.stringify(data)
        })
            .success( function(e) {
                console.log(e);
                delete $.QueryString.session;
                $.QueryString.gist = e.id;
                $.setQueryParameters($.QueryString);
            })
            .error( function(e) {
                alert("Failed to create gist.");
                console.warn("Gist creation failed: ", e);
            });
    };
    $('#gist_save').click(publishNewGist);


    $('#hamburger').click(function() {
        $('#menu').toggleClass("show");
    });
    $('#view_mode_label').click(function() {
        $('#view_mode').toggleClass("showBlock");
    });

    var gist = $.QueryString["gist"];

    if (gist) {
        loadFromGist(gist);
    }
    else {
        var sessionId = setupSession();
        var hasSessionCode = tryRestoreCachedCode(sessionId);
        setupEditor();
    }
});
