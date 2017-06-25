var myconsole = console;

$.ajaxSetup({
  dataType: 'text'
});

(function($) {
  $.QueryString = (function(a) {
    if (a == "") return {};
    var b = {};
    for (var i = 0; i < a.length; ++i) {
      var p = a[i].split('=');
      if (p.length != 2) continue;
      b[p[0]] = decodeURIComponent(p[1].replace(/\+/g, " "));
    }
    return b;
  })(window.location.search.substr(1).split('&'));

  $.setQueryParameters = function(params) {
    var url = location.href.split('?')[0];
    var encodedParams = Object.keys(params).map(function(key) {
      return key + '=' + encodeURIComponent(params[key].replace('\/', ''));
    }).join('&');

    document.location = url + '?' + encodedParams;
  };
})(jQuery);

$(function() {

  var defaultBundleAndExecute = function(js, backend) {
    $.get(backend.endpoint + '/bundle').done(function(bundle) {
      execute(js, bundle, backend);
    }).fail(function(err) {
      myconsole.warn("Unable to load JS bundle", err);
    });
  };

  var getBackend = function(backend) {
    if (backend === "thermite") {
      return {
        backend: backend,
        endpoint: "https://compile.purescript.org/thermite",
        mainGist: "85383bb058471109cfef379bbb6bc11c",
        extra_styling: '    <link rel="stylesheet" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css">',
        extra_body: '    <div id="app"></div>',
        bundleAndExecute: function(js, backend) {
          $.when(
            $.get("js/console.js"),
            $.get("js/react.min.js"),
            $.get("js/react-dom.min.js"),
            $.get(backend.endpoint + "/bundle")
          ).done(function(consoleScript, react, react_dom, bundle) {

            var replaced = bundle[0].replace(/require\("react"\)/g, 'window.React')
              .replace(/require\("react-dom"\)/g, 'window.ReactDOM')
              .replace(/require\("react-dom\/server"\)/g, 'window.ReactDOM');

            execute(js, [consoleScript[0], react[0], react_dom[0], replaced].join("\n"), backend);
          }).fail(function(err) {

            myconsole.warn("Unable to load JS bundle", err);
          });
        }
      };
    } else if (backend === "slides") {
      return {
        backend: "slides",
        endpoint: "https://compile.purescript.org/slides",
        mainGist: "c62b5778a6a5f2bcd32dd97b294c068a",
        extra_styling: '<link rel="stylesheet" href="css/slides.css">',
        extra_body: '<div id="main"></div>',
        bundleAndExecute: defaultBundleAndExecute
      };
    } else if (backend === "flare") {
      return {
        backend: "flare",
        endpoint: "https://compile.purescript.org/flare",
        mainGist: "4f54d6dd213caa54d736ead597e17fee",
        extra_styling: '<link rel="stylesheet" href="css/flare.css">',
        extra_body: '<div id="controls"></div><div id="output"></div><div id="tests"></div><canvas id="canvas" width="800" height="600"></canvas>',
        bundleAndExecute: defaultBundleAndExecute
      };
    } else if (backend === "mathbox") {
      return {
        backend: "mathbox",
        endpoint: "https://compile.purescript.org/purescript-mathbox",
        mainGist: "aeecffd458fa8a365b4af3b3cd9d7759",
        extra_styling: ['<script src="js/mathbox-bundle.js"></script>', '<link rel="stylesheet" href="css/mathbox.css">'].join("\n"),
        extra_body: '',
        bundleAndExecute: defaultBundleAndExecute
      };
    } else { // core
      return {
        backend: "core",
        endpoint: "https://compile.purescript.org/try",
        mainGist: "b57a766d417e109785540d584266fc33",
        extra_styling: '',
        extra_body: '',
        bundleAndExecute: defaultBundleAndExecute
      };
    }
  };

  var loadOptions = function(backend) {

    $('#backend_' + backend.backend).attr('checked', 'checked');

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

    var gist = $.QueryString["gist"];
    if (gist) {
      $('#view_gist').attr('href', 'https://gist.github.com/' + gist);
    } else {
      $('#view_gist_li').hide();
    }

    $('input[name=backend_inputs]').change(function(e) {
      var backend = getBackend($(this).filter(':checked').val());
      if (confirm("Replace your current code with the " + backend.backend + " backend sample code?")) {
        location.href = "?backend=" + backend.backend;
      } else {
        setTimeout(function() {
          compile();
          cacheCurrentCode(backend);
        }, 1000);
      }
      hideMenus();
    });
  };

  var setupSession = function(onSessionExists) {
    var guid = function() {
      var s4 = function() {
        return Math.floor((1 + Math.random()) * 0x10000)
          .toString(16)
          .substring(1);
      }
      return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
        s4() + '-' + s4() + s4() + s4();
    }

    var sessionId = $.QueryString['session'];
    if (sessionId) {
      onSessionExists(sessionId);
    } else {
      $.QueryString['session'] = sessionId = guid();
      $.setQueryParameters($.QueryString);
    }
  };

  var cacheCurrentCode = function(backend) {
    if (window.localStorage) {
      var sessionId = $.QueryString['session'];
      var code = $('#code_textarea').val();

      localStorage.setItem(sessionId, code);
      localStorage.setItem(sessionId + 'backend', backend.backend);
    }
  };

  var tryRestoreCachedCode = function(sessionId) {
    if (window.localStorage) {
      var code = localStorage.getItem(sessionId);
      var backend = localStorage.getItem(sessionId + 'backend');
      if (backend) {
        $('#backend_' + backend).click();
      }
      if (code) {
        $('#code_textarea').val(code);
        return backend;
      }
    }
  }

  var editor, cleanupActions = [];

  var setupEditorWith = function(name, ta_name, lang) {

    editor = ace.edit(name);

    editor.renderer.setShowGutter(true);
    editor.setFontSize(13);
    editor.setShowPrintMargin(false);

    var session = editor.getSession();

    session.setMode(lang);
    session.setValue($('#' + ta_name).val());
    session.setOptions({
      tabSize: 2,
      useSoftTabs: true
    });

    session.on('change', _.debounce(function() {

      $('#' + ta_name).val(session.getValue());

      var backend = getBackend($('input[name=backend_inputs]').filter(':checked').val());
      cacheCurrentCode(backend);
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

  var setupEditor = function(backend) {

    loadOptions(backend);
    setupEditorWith('code', 'code_textarea', 'ace/mode/haskell');
    cacheCurrentCode(backend);
  };

  var execute = function(js, bundle, backend) {

    var $iframe = $('<iframe id="output-iframe">');

    $('#column2')
      .empty()
      .append($iframe);

    var iframe = $iframe.get(0).contentWindow.document;
    iframe.open();
    iframe.write(
      ['<!DOCTYPE html>'
      , '<html>'
      , '  <head>'
      , '    <meta content="text/html;charset=utf-8" http-equiv="Content-Type">'
      , '    <meta content="utf-8" http-equiv="encoding">'
      , '    <meta name="viewport" content="width=device-width, initial-scale=1.0">'
      , '    <title>Try PureScript!</title>'
      , '    <link rel="stylesheet" href="css/style.css">'
      , backend.extra_styling
      , '  </head>'
      , '  <body>'
      , backend.extra_body
      , '  </body>'
      , '</html>'
      ].join('\n')
    );
    iframe.close();

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

    $('iframe').ready(function() {
      var checkExists = setInterval(function() {
        var body = iframe.getElementsByTagName('body')[0];
        if (body) {
          body.onclick = function() {
            hideMenus();
          };
          body.appendChild(script);
          clearInterval(checkExists);
        }
      }, 100);

    });

  };

  var compile = function() {

    backend = getBackend($('input[name=backend_inputs]').filter(':checked').val());

    $('#column2')
      .empty()
      .append($("<div>").addClass("loading").append("Loading..."));

    var code = $('#code_textarea').val();

    $.ajax({
      url: backend.endpoint + '/compile',
      dataType: 'json',
      data: code,
      method: 'POST',
      contentType: 'text/plain',
      success: function(res) {

        for (var i = 0; i < cleanupActions.length; i++) {
          cleanupActions[i]();
        }

        cleanupActions = [];

        if (res.error) {
          switch (res.error.tag) {
            case "CompilerErrors":
              var errors = res.error.contents;

              $('#column2').empty();

              for (var i = 0; i < errors.length; i++) {
                var error = errors[i];
                $('#column2')
                  .append($('<h1>').addClass('error-banner').append("Error " + (i + 1) + " of " + errors.length))
                  .append($('<pre>').append($('<code>').append(error.message)));

                var startColumn = error.position.startColumn;
                var endColumn = error.position.endColumn;

                if (error.position.startLine === error.position.endLine && endColumn <= error.position.startColumn) {
                  // Make sure the range is at least one character wide.
                  if (startColumn > 0) {
                    startColumn = endColumn - 1;
                  } else {
                    endColumn = startColumn + 1;
                  }
                }

                // Add an error marker
                var range = new(ace.require("ace/range").Range)
                  (error.position.startLine - 1, startColumn - 1, error.position.endLine - 1, endColumn - 1);

                var marker = editor.session.addMarker(range, "error", "text", true);

                editor.session.addGutterDecoration(error.position.startLine - 1, "gutter-error");

                cleanupActions.push((function(marker, line) {
                  return function() {
                    editor.session.removeMarker(marker);
                    editor.session.removeGutterDecoration(line, "gutter-error");
                  };
                })(marker, error.position.startLine - 1));
              }

              break;
            case "OtherError":
              $('#column2')
                .empty()
                .append($('<pre>').append($('<code>').append(res.error.contents)));
              break;
          }
        } else if (res.js) {
          if ($("#showjs").is(":checked")) {
            $('#column2')
              .empty()
              .append($('<pre>').append($('<code>').text(res.js)));
          } else {
            (backend.bundleAndExecute || defaultBundleAndExecute)(res.js, backend);
          }
        }
      },
      error: function(res) {
        $('#column2')
          .empty()
          .append($('<pre>').append($('<code>').append(res.responseText)));
        console.warn("failed to communicate with compilation server", res);
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

  var loadFromGist = function(id, backend) {
    $.ajax({
      url: 'https://api.github.com/gists/' + id,
      dataType: 'json'
    }).done(function(gistInfo) {
      tryLoadFileFromGist(gistInfo, "Main.purs")
        .done(function(code) {
          code && $('#code_textarea').val(code);
          setupEditor(backend);
        }).fail(function() {
          console.log("Unable to load gist contents");
          setupEditor(backend);
        });
    }).fail(function() {

      console.log("Unable to load gist metadata");
      setupEditor(backend);
    });
  };

  $('#showjs').change(compile);
  $('#compile_label').click(compile);

  $('input[name=view_mode]').change(function() {
    var view_mode = $(this).filter(':checked').val();

    if (view_mode === "code") {
      $('#column1').show();
      $('#column2').hide();
      $('#showjs_label').hide();
      $('#showjs').hide();
    } else if (view_mode === "output") {
      $('#column1').hide();
      $('#column2').show();
      $('#showjs_label').show();
      $('#showjs').show();
    } else { // (view_mode === "sidebyside")
      $('#column1').show();
      $('#column2').show();
      $('#showjs_label').show();
      $('#showjs').show();
    }
  });

  var publishNewGist = function() {
    if (!confirm('Do you really want to publish this code as an anonymous Gist?\n\nNote: this code will be available to anyone with a link to the Gist.')) {
      return;
    }

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
      .success(function(e) {
        console.log(e);
        var sess = $.QueryString.session;
        delete $.QueryString.session;
        $.QueryString.gist = e.id;
        var backend = $('input[name=backend_inputs]').filter(':checked').val();
        $.QueryString.backend = backend;
        $.QueryString.session = sess;
        $.setQueryParameters($.QueryString);
      })
      .error(function(e) {
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

  function withSession(sessionId) {
    var cachedBackend = tryRestoreCachedCode(sessionId);
    if (cachedBackend) {
      setupEditor(getBackend(cachedBackend));
    } else {
      var backend = getBackend($.QueryString["backend"]);
      var gist = $.QueryString["gist"] || backend.mainGist;
      loadFromGist(gist, backend);
    }
  };
  setupSession(withSession);
});
