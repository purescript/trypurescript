"use strict";

exports.setInnerHTML = function(html) {
  return function() {
    document.body.innerHTML = html;
  };
};

exports.encode = function(text) {
  return text
    .replace('<', '&lt;')
    .replace('>', '&gt;')
    .replace('&', '&amp;')
    .replace('"', '&quot;');
};
