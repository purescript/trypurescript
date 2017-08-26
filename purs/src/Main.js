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
  return function() {
    jq.click();
  };
};

exports.empty = function(jq) {
  return function() {
    jq.empty();
  };
};

exports.filter = function(jq, sel) {
  return jq.filter(sel);
};

exports.is = function(jq, sel) {
  return jq.is(sel);
};

exports.getValue = function(jq) {
  return jq.val();
};

exports.setEditorContent = setEditorContent;
exports.onEditorChanged = onEditorChanged;
exports.cleanUpMarkers = cleanUpMarkers;
exports.addErrorMarker = addErrorMarker;
exports.setupIFrame = setupIFrame;

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

exports.compileApi = function(backend, code, done, fail) {
  $.ajax({
    url: backend.endpoint + '/compile',
    dataType: 'json',
    data: code,
    method: 'POST',
    contentType: 'text/plain',
    success: function(res) {
      done(res);
    },
    error: function(res) {
      fail(res.responseText)
    }
  });
}
