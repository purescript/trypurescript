"use strict";

exports.getQueryString = function() {
  return window.location.search;
};

exports.setQueryParameters = function(params) {
  var encodedParams = Object.keys(params).map(function(key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  window.location.search = '?' + encodedParams;
};
