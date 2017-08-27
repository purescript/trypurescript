"use strict";

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
