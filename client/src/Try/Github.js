"use strict";

exports.getRawGithubFile_ = function(id, done, fail) {
  $.ajax({
    url: 'https://raw.githubusercontent.com/' + id,
    dataType: 'text'
  }).done(done).fail(function(err) {
    fail("Unable to load file from GitHub");
  });
}
