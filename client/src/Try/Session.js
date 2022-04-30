export function storeSession_(sessionId, state) {
  if (window.localStorage) {
    localStorage.setItem(sessionId, state.code);
    localStorage.setItem(sessionId + 'backend', state.backend);
  }
}

export function tryRetrieveSession_(sessionId) {
  if (window.localStorage) {
    var code = localStorage.getItem(sessionId);
    var backend = localStorage.getItem(sessionId + 'backend');
    if (code && backend) {
      return { code: code, backend: backend };
    }
  }
}
