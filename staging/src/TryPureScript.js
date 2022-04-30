"use strict";

export function setInnerHTML(html) {
  return function() {
    document.body.innerHTML += html;
  };
}

export function withConsoleImpl(f) {
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
}
