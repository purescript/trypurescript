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

exports.getQueryString = function() {
  return window.location.search;
};

exports.setQueryParameters = function(params) {
  var url = location.href.split('?')[0];
  var encodedParams = Object.keys(params).map(function(key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  document.location = url + '?' + encodedParams;
};

exports.click = function(jq) {
  return function () {
    jq.click();
  };
};

exports.filter = function(jq, sel) {
  return jq.filter(sel);
};

exports.getValue = function(jq) {
  return jq.val();
};

exports.navigateTo = function(url) {
  return function() {
    location.href = url;
  };
};

exports.storeSession = function(sessionId, state) {
  if (window.localStorage) {
    localStorage.setItem(sessionId, state.code);
    localStorage.setItem(sessionId + 'backend', state.backend);
  }
};

exports.tryRetrieveSession = function(sessionId) {
  if (window.localStorage) {
    var code = localStorage.getItem(sessionId);
    var backend = localStorage.getItem(sessionId + 'backend');
    if (code && backend) {
      return { code: code, backend: backend };
    }
  }
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
    fail("Gist does not contain a file named " + filename);
  }
};

exports.uploadGist = function(content, done, fail) {
  var data = {
    "description": "Published with try.purescript.org",
    "public": false,
    "files": {
      "Main.purs": {
        "content": content
      }
    }
  };

  $.ajax({
    url: 'https://api.github.com/gists',
    type: 'POST',
    dataType: 'json',
    data: JSON.stringify(data)
  }).success(function(e) {
    done(e.id);
  }).error(function(e) {
    fail(e);
  });
};

/*****************************************************************************/

exports.setupEditorWith = function(backend) {
  setEditorContent($('#code_textarea').val());

  onEditorChanged(function() {
    $('#' + ta_name).val(session.getValue());

    exports.cacheCurrentCode(backend);
    if ($("#auto_compile").is(":checked")) {
      exports.compile(backend);
    }
  }, 750);

  exports.compile(backend);
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

exports.compile = function(backend) {

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

      cleanUpMarkers();

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

              addErrorMarker(error.position.startLine, error.position.startColumn,
                error.position.endLine, error.position.endColumn);
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
