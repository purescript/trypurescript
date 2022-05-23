(function() {
  var parent;

  // Ensures `main` is only called once.
  this.window.runCount = 0;

  document.addEventListener("DOMContentLoaded", function() {
    window.addEventListener("message", function(event) {
      parent = event.source;
      parent.postMessage("trypurescript", "*");
      const code = `
      ${event.data.code}
      if (window.runCount === 0) {
        main();
        // bump runCount to ensure we don't re-run this.
        window.runCount = 1;
      }`;
      const scriptEl = document.createElement("script");
      scriptEl.type = "module";
      scriptEl.appendChild(document.createTextNode(code));
      document.body.appendChild(scriptEl);
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
