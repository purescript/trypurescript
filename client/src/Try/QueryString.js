export {
  compressToEncodedURIComponent,
  decompressFromEncodedURIComponent as decompressFromEncodedURIComponent_,
} from 'lz-string';

export function getQueryString() {
  return window.location.search;
}

export function setQueryParameters(params) {
  var encodedParams = Object.keys(params).map(function(key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  window.history.replaceState(null, '', '?' + encodedParams);
}
