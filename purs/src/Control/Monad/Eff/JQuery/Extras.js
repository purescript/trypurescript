"use strict";

exports.click = function(jq) {
  return function() {
    jq.click();
  };
};

exports.empty = function(jq) {
  return function() {
    jq.empty();
  };
};

exports.filter = function(jq) {
  return function(sel) {
    return function() {
      return jq.filter(sel);
    };
  };
};

exports.is = function(jq) {
  return function(sel) {
    return function() {
      return jq.is(sel);
    };
  };
};

exports.getValue = function(jq) {
  return function() {
    return jq.val();
  };
};
