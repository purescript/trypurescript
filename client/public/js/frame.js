(function() {
  var parent;

  document.addEventListener("DOMContentLoaded", function() {
    window.addEventListener("message", function(event) {
      parent = event.source;
      parent.postMessage("trypurescript", "*");
      const code = event.data.code;
      const scriptEl = document.createElement("script");
      scriptEl.type = "module";
      // See https://stackoverflow.com/a/6433770
      try {
        scriptEl.appendChild(document.createTextNode(code));
      } catch (e) {
        scriptEl.text = code;
      } finally {
        document.body.appendChild(scriptEl);
      }
    }, { once: true });
  }, { once: true });

  document.addEventListener("click", function(event) {
    if (parent && event.target.nodeName === "A" && event.target.hostname === "gist.github.com") {
      event.preventDefault();
      parent.postMessage({
        gistId: event.target.pathname.split("/").slice(-1)[0]
      }, "*");
    }
    if (parent && event.target.nodeName === "A" && event.target.hostname === "github.com") {
      event.preventDefault();
      parent.postMessage({
        githubId: event.target.pathname
      }, "*");
    }
  }, false);
})();
