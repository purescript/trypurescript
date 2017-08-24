"use strict";

exports.get = function(uri, done, fail) {
  $.get(uri).done(done).fail(function(err) {
    fail(err.statusText);
  });
};

exports.getGistById = function(id, done, fail) {
  $.ajax({
    url: 'https://api.github.com/gists/' + id,
    dataType: 'json'
  }).done(done).fail(function(err) {
    fail(err.statusText);
  });
}

exports.getQueryString = function(key) {
  return $.QueryString[key];
};

exports.click = function(jq) {
  jq.click();
};

exports.filter = function(jq, sel) {
  return jq.filter(sel);
};

exports.getValue = function(jq) {
  return jq.val();
};

exports.confirm = function(msg) {
  return function() {
    return window.confirm(msg);
  };
};

exports.navigateTo = function(url) {
  return function() {
    location.href = url;
  };
};

exports.setupSession = function(onSessionExists) {
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

exports.cacheCurrentCode = function(backend) {
  if (window.localStorage) {
    var sessionId = $.QueryString['session'];
    var code = $('#code_textarea').val();

    localStorage.setItem(sessionId, code);
    localStorage.setItem(sessionId + 'backend', backend.backend);
  }
};

exports.tryRestoreCachedCode = function(sessionId) {
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
};

var editor, cleanupActions = [];

exports.setupEditorWith = function(pursImports, name, ta_name, lang) {

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

    var backend = pursImports.getBackend($('input[name=backend_inputs]').filter(':checked').val());
    exports.cacheCurrentCode(backend);
    if ($("#auto_compile").is(":checked")) {
      exports.compile(pursImports);
    }
  }, 750));

  exports.compile(pursImports);
};

exports.hideMenus = function() {
  $('#menu').removeClass("show");
  $('#view_mode').removeClass("show-sub-menu");
  $('#backend').removeClass("show-sub-menu");
};

exports.changeViewMode = function(jq) {
  var view_mode = $(jq).filter(':checked').val();

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
};

exports.execute = function(js, bundle, backend) {

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
          exports.hideMenus();
        };
        body.appendChild(script);
        clearInterval(checkExists);
      }
    }, 100);
  });
};

exports.compile = function(pursImports) {

  var backend = pursImports.getBackend($('input[name=backend_inputs]').filter(':checked').val());

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

exports.tryLoadFileFromGist = function(gistInfo, filename, done, fail) {

  if (gistInfo.files && gistInfo.files.hasOwnProperty(filename)) {

    var url = gistInfo.files[filename].raw_url;

    return $.ajax({
      url: url,
      dataType: 'text'
    }).done(done).fail(function(err) {
      fail(err.statusText);
    });
  } else {

    console.log("File named " + filename + " does not exist in gist");

    var promise = $.Deferred();
    promise.resolve(null);
    return promise.done(done).fail(function(err) {
      fail(err.statusText);
    });
  }
};

exports.publishNewGist = function() {
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
  }).success(function(e) {
    console.log(e);
    var sess = $.QueryString.session;
    delete $.QueryString.session;
    $.QueryString.gist = e.id;
    var backend = $('input[name=backend_inputs]').filter(':checked').val();
    $.QueryString.backend = backend;
    $.QueryString.session = sess;
    $.setQueryParameters($.QueryString);
  }).error(function(e) {
    alert("Failed to create gist.");
    console.warn("Gist creation failed: ", e);
  });
};
