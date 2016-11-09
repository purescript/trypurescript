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

exports.withConsoleImpl = function(f) {
  return function() {
    var oldLog = console.log;
    var oldError = window.onerror;
    var lines = [];
    console.log = function(s) {
      lines.push(s);
    };
    window.onerror = function(e) {
      console.log(e);
      return true;
    };
    try {
      f();
      return lines;
    } finally {
      console.log = oldLog;
      window.onerror = oldError;
    }
  };
};
