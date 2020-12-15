"use strict";

exports.rawUrl_ = function (gistInfo_, filename) {
  try {
    var gistInfo = JSON.parse(gistInfo_);
    return gistInfo.files[filename].raw_url;
  } catch (e) {
    return null;
  }
};
