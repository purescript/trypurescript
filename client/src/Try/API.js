"use strict";

exports.get_ = function(uri, done, fail) {
  $.get(uri).done(done).fail(function(err) {
    fail(err.statusText);
  });
};

exports.compile_ = function(endpoint, code, done, fail) {
  $.ajax({
    url: endpoint + '/compile',
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
