var myconsole = console;

$.ajaxSetup({ dataType: 'text' });

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


var coreStart =
    ['module Main where'
    ,''
    ,'import Prelude'
    ,'import Data.Foldable (fold)'
    ,'import TryPureScript'
    ,''
    ,'main ='
    ,'    render $ fold'
    ,'      [ h1 (text "Try PureScript!")'
    ,'      , p (text "Try out the examples below, or create your own!")'
    ,'      , h2 (text "Examples")'
    ,'      , list (map fromExample examples)'
    ,'      , h2 (text "Try PureScript Libraries")'
    ,'      , list [ link "?backend=thermite" (text "Try Thermite") '
    ,'               <> text ", a front-end library for PureScript which uses React"'
    ,'             ]'
    ,'      , h2 (text "Share Your Code")'
    ,'      , p (text "Code can be loaded from a GitHub Gist. To share code, simply include the Gist ID in the URL as follows:")'
    ,'      , indent (p (code (text "  try.purescript.org?gist=gist-id")))'
    ,'      , p (fold'
    ,'          [ text "The Gist should contain a file named "'
    ,'          , code (text "Main.purs")'
    ,'          , text " containing your PureScript code."'
    ,'          ])'
    ,'      ]'
    ,'  where'
    ,'    fromExample { title, gist } ='
    ,'      link ("?gist=" <> gist) (text title)'
    ,''
    ,'    examples ='
    ,'      [ { title: "Algebraic Data Types"'
    ,'        , gist: "37c3c97f47a43f20c548"'
    ,'        }'
    ,'      , { title: "Loops"'
    ,'        , gist: "cfdabdcd085d4ac3dc46"'
    ,'        }'
    ,'      , { title: "Operators"'
    ,'        , gist: "3044550f29a7c5d3d0d0"'
    ,'        }'
    ,'      , { title: "Records"'
    ,'        , gist: "b80be527ada3eab47dc5"'
    ,'        }'
    ,'      , { title: "Recursion"'
    ,'        , gist: "ff49cc7dc85923a75613"'
    ,'        }'
    ,'      , { title: "Do Notation"'
    ,'        , gist: "47c2d9913c5dbda1e963"'
    ,'        }'
    ,'      , { title: "Type Classes"'
    ,'        , gist: "1a3b845e8c6defde659a"'
    ,'        }'
    ,'      , { title: "Generic Programming"'
    ,'        , gist: "3f735aa2a652af592101"'
    ,'        }'
    ,'      , { title: "QuickCheck"'
    ,'        , gist: "69f7f94fe4ff3bd47f4b"'
    ,'        }'
    ,'      ]'
    ].join('\n');


thermiteStart =
    ['module Main where'
    ,''
    ,'import Prelude'
    ,''
    ,'import React as R'
    ,'import React.DOM as R'
    ,'import React.DOM.Props as RP'
    ,'import Thermite as T'
    ,'import Thermite.Try as T'
    ,''
    ,'type Link ='
    ,'  { title :: String'
    ,'  , gist  :: String'
    ,'  }'
    ,''
    ,'lessons :: Array Link'
    ,'lessons ='
    ,'  [ { title: "State"'
    ,'    , gist: "82cf5744940bcc4553f7bde5c68e0342&backend=thermite"'
    ,'    }'
    ,'  , { title: "Actions"'
    ,'    , gist: "f2e21dc17f614df71dd445b523e53af0&backend=thermite"'
    ,'    }'
    ,'  , { title: "Async"'
    ,'    , gist: "e6a2142e872dbc7e6557a78a55f03589&backend=thermite"'
    ,'    }'
    ,'  , { title: "Components"'
    ,'    , gist: "144765549b8524116b1ada5b6fbcb487&backend=thermite"'
    ,'    }'
    ,'  , { title: "Lists"'
    ,'    , gist: "ff03c99df15bd16f2289&backend=thermite"'
    ,'    }'
    ,'  ]'
    ,''
    ,'examples :: Array Link'
    ,'examples ='
    ,'  [ { title: "Task List"'
    ,'    , gist: "f5f273e4c5e4161fceff&backend=thermite"'
    ,'    }'
    ,'  ]'
    ,''
    ,'renderLink :: Link -> Array R.ReactElement'
    ,'renderLink link ='
    ,'  [ R.a [ RP.href ("?gist=" <> link.gist)'
    ,'        , RP.target "_top"'
    ,'        ]'
    ,'        [ R.text link.title ]'
    ,'  ]'
    ,''
    ,'render :: T.Render _ _ _'
    ,'render _ _ _ _ ='
    ,'  [ R.h1\' [ R.text "Try Thermite!" ]'
    ,'  , R.p\'  [ R.text "Browse the lessons and examples below, or check out the "'
    ,'          , R.a [ RP.href "http://pursuit.purescript.org/packages/purescript-thermite/"'
    ,'                , RP.target "_new"'
    ,'                ]'
    ,'                [ R.text "Thermite documentation" ]'
    ,'          , R.text "."'
    ,'          ]'
    ,'  , R.h2\' [ R.text "Lessons" ]'
    ,'  , R.ol\' (map (R.li\' <<< renderLink) lessons)'
    ,'  , R.h2\' [ R.text "Examples" ]'
    ,'  , R.ul\' (map (R.li\' <<< renderLink) examples)'
    ,'  , R.h2\' [ R.text "Try PureScript" ]'
    ,'  , R.p\'  [ R.p\' [ R.text "New to PureScript? You might want to try PureScript using the core libraries." ]'
    ,'           , R.p\' [ R.text "You can do that "'
    ,'                   , R.a [ RP.href "?backend=core"'
    ,'                         , RP.target "_top"'
    ,'                         ]'
    ,'                         [ R.text "here" ]'
    ,'                   , R.text "."'
    ,'                   ]'
    ,'          ]'
    ,'  , R.hr\' []'
    ,'  , R.p\'  [ R.small\' [ R.text "Powered by "'
    ,'                     , R.a [ RP.href "http://purescript.org/"'
    ,'                           , RP.target "_new"'
    ,'                           ]'
    ,'                           [ R.text "PureScript" ]'
    ,'                     , R.text "."'
    ,'                     ]'
    ,'          ]'
    ,'  ]'
    ,''
    ,'spec :: T.Spec _ _ _ _'
    ,'spec = T.simpleSpec T.defaultPerformAction render'
    ,''
    ,'main = T.defaultMain spec unit'
    ].join('\n');


$(function() {

    var getBackend = function(backend) {
        if (backend === "thermite") {
            return { backend: backend,
                     endpoint: "https://compile.purescript.org/thermite",
                     mainSnippet: thermiteStart,
                     extra_styling: '    <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">',
                     extra_body: '    <div id="app"></div>'
                   };
        } else { // core
            return { backend: "core",
                     endpoint: "https://compile.purescript.org/try",
                     mainSnippet: coreStart,
                     extra_styling: '',
                     extra_body: ''
                   };
        }
    };

    var loadOptions = function() {

      var view_mode = $.QueryString["view"];
      if (view_mode && (view_mode === "sidebyside" || view_mode === "code" || view_mode === "output")) {
        $('#view_' + view_mode).click();
      }

      var backendQP = $.QueryString["backend"];
      var backend;
      if (backendQP) {
          backend = getBackend(backendQP);
          $('#backend_' + backend.backend).click();
      }
      else {
          backend = getBackend($('input[name=backend_inputs]').filter(':checked').val());
      }
      if ($('#code_textarea').val() === "") {
          $('#code_textarea').val(backend.mainSnippet);
      }

      var showjs = $.QueryString["js"];
      if (showjs) {
        $('input:checkbox[name=showjs]').prop('checked', showjs === "true");
      }

      var auto_compile = $.QueryString["compile"];
      if (auto_compile) {
        $('input:checkbox[name=auto_compile]').prop('checked', auto_compile === "true");
      }


      $('input[name=backend_inputs]').change(function (e) {
          var backend = getBackend($(this).filter(':checked').val());
          if (confirm("Replace your current code with the " + backend.backend + " backend sample code?")) {
              ace.edit("code").setValue(backend.mainSnippet, -1);
              if (!$("#auto_compile").is(":checked")) {
                  setTimeout(compile, 1000);
              }
          } else {
              setTimeout(compile, 1000);
              setTimeout(cacheCurrentCode, 1000);
          }
          hideMenus();
      });
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
            var backend = $('input[name=backend_inputs]').filter(':checked').val();
            localStorage.setItem(sessionId+'backend', backend);
        }
    };

    var tryRestoreCachedCode = function(sessionId) {
        if (window.localStorage) {
            var code = localStorage.getItem(sessionId);
            var backend = localStorage.getItem(sessionId+'backend');
            if (backend) {
                $('#backend_' + backend).click();
            }
            if (code) {
                $('#code_textarea').val(code);
            }
        }
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
    var hideMenus = function() {
        $('#menu').removeClass("show");
        $('#view_mode').removeClass("show-sub-menu");
        $('#backend').removeClass("show-sub-menu");
    };

    var setupEditor = function() {

        loadOptions();
        setupEditorWith('code', 'code_textarea', 'ace/mode/haskell');
        cacheCurrentCode();
    };

    var execute = function(js, bundle) {

        var $iframe = $('<iframe id="output-iframe">');

        $('#column2')
            .empty()
            .append($iframe);

        var backend = getBackend($('input[name=backend_inputs]').filter(':checked').val());
        var iframe = $iframe.get(0).contentWindow.document;
        iframe.open();
        iframe.write(
            [ '<!DOCTYPE html>'
            , '<html>'
            , '  <head>'
            , '    <title>Try PureScript!</title>'
            , backend.extra_styling
            , '    <link rel="stylesheet" href="css/style.css">'
            , '  </head>'
            , '  <body>'
            , backend.extra_body
            , '  </body>'
            , '</html>'
            ].join('\n')
        );
        document.getElementById("output-iframe").contentWindow.document.body.onclick = function() {
            hideMenus();
        };

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
        var backend = getBackend($('input[name=backend_inputs]').filter(':checked').val());

        $.ajax({
            url: backend.endpoint + '/compile',
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
                      if ($('input[name=backend_inputs]').filter(':checked').val() === "thermite") {
                           $.when($.get("js/console.js"),
                                  $.get("js/react.min.js"),
                                  $.get("js/react-dom.min.js"),
                                  $.get(backend.endpoint + "/bundle")
                                 ).done(function(consoleScript, react, react_dom, bundle) {

                           var replaced = bundle[0].replace(/require\("react"\)/g, 'window.React')
                                                   .replace(/require\("react-dom"\)/g, 'window.ReactDOM')
                                                   .replace(/require\("react-dom\/server"\)/g, 'window.ReactDOM');

                          execute(res.js, [ consoleScript[0], react[0], react_dom[0], replaced ].join("\n"));
                        }).fail(function(err) {

                          myconsole.warn("Unable to load JS bundle", err);
                        });
                      } else {
                          $.get(backend.endpoint + '/bundle').done(function(bundle) {

                              execute(res.js, bundle);
                          }).fail(function(err) {

                              myconsole.warn("Unable to load JS bundle", err);
                          });
                      }
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
                var sess = $.QueryString.session;
                delete $.QueryString.session;
                $.QueryString.gist = e.id;
                var backend = $('input[name=backend_inputs]').filter(':checked').val();
                $.QueryString.backend = backend;
                $.QueryString.session = sess;
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
        $('#view_mode').toggleClass("show-sub-menu");
    });
    $('#backend_label').click(function() {
        $('#backend').toggleClass("show-sub-menu");
    });

    $('#editor_view').click(function() {
        hideMenus();
    });

    var sessionId = setupSession();
    tryRestoreCachedCode(sessionId);

    var gist = $.QueryString["gist"];
    if (gist) {
        loadFromGist(gist);
    } else {
        setupEditor();
    }

});
