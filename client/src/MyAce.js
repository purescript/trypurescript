"use strict";

// Upstream version missing inFront, which is
// interpreted as `false` when omitted.
exports.getMarkersImpl = function (inFront, session) {
  return function () {
    var markerObj = session.getMarkers(inFront);
    var ks = Object.getOwnPropertyNames(markerObj);
    var result = [];
    for (var i = 0; i < ks.length; i++) {
      result[i] = markerObj[ks[i]];
    }
    return result;
  };
};