"use strict";

exports.getPlasteById_ = function(id, done, fail) {
  $.ajax({
    url: 'http://gathering.purescript.org:7777/raw/' + id,
    dataType: 'text'
  }).done(done).fail(function(err) {
    fail("Unable to load Plaste metadata");
  });
}

exports.uploadPlaste_ = function(content, done, fail) {
  $.ajax({
    url: 'http://compile.purescript.org/plaste/',
    type: 'POST',
    dataType: 'json',
    data: content
  }).success(function(e) {
    done(e.plaste_id);
  }).error(function(e) {
    fail(e);
  });
};
