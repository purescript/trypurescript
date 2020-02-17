"use strict";

exports.getGistById_ = function(id, done, fail) {
  $.ajax({
    url: 'https://api.github.com/gists/' + id,
    dataType: 'json'
  }).done(done).fail(function(err) {
    fail("Unable to load Gist metadata");
  });
}

exports.tryLoadFileFromGist_ = function(gistInfo, filename, done, fail) {
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

exports.uploadGist_ = function(content, done, fail) {
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
