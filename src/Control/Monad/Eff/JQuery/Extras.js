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

exports.fadeIn = function(jq) {
  return function() {
    jq.fadeIn();
  };
};

exports.fadeOut = function(jq) {
  return function() {
    jq.fadeOut();
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
