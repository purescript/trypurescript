"use strict";

exports.rawUrl_ = function (gistInfo, filename) {
  if (gistInfo.files && gistInfo.files.hasOwnProperty(filename)) {
    return gistInfo.files[filename].raw_url;
  } else {
    return null;
  }
};
