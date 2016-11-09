"use strict";

exports.setInnerHTML = function(html) {
  return function() {
    document.body.innerHTML += html;
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
    var oldError = console.error;
    var oldWindowError = window.onerror;
    var lines = [];

    console.log = console.error = function(s) {
      lines.push(s);
    };

    window.onerror = function(e) {
      lines.push(e.message);
      return true;
    };

    try {
      f();
    } catch (e) {
      lines.push(e.message);
    } finally {
      console.log = oldLog;
      console.error = oldError;
      window.onerror = oldWindowError;
    }

    return lines;
  };
};
