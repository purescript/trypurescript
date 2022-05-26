(function() {
  var parent;

  document.addEventListener("DOMContentLoaded", function() {
    window.addEventListener("message", function(event) {
      parent = event.source;
      parent.postMessage("trypurescript", "*");
      // `es-module-shims` gets notified every time the DOM
      // is modified, and it will attempt to run all script
      // elements with `type="module"`.
      //
      // If the code below modifies the DOM in any way (e.g. using
      // `TryPureScript.render`), `es-module-shims` will reactivate,
      // which causes it to rerun the script below again.
      //
      // To prevent that from happening, we remove the `type="module"`
      // part from the script element right before the `main` function
      // is called. The below edits to the compiled JS code
      // works as follows:
      // 1. Script element gets added to DOM, activating `es-module-shims`
      // 2. `es-module-shims` resolves the NPM dependencies and runs the script.
      // 3. Running the script only schedules the below action.
      // 4. With the script fully processed and NPM dependencies resolved,
      //    we remove the `type="module"` part from the script element.
      // 5. `main` is called. When `es-module-shims` reactivates,
      //    it sees that the `try-purescript-script-id` element is not
      //    an ES module and skips it.
      // 6. Thus, the `main` function is not called an infinite number of times.
      const scriptId = "try-purescript-script-id";
      const code = `
      ${event.data.code}
      setTimeout(() => {
        document.getElementById("${scriptId}").type = "classic";
        main();
      }, 0);
      `;
      const scriptEl = document.createElement("script");
      scriptEl.type = "module";
      scriptEl.id = scriptId;
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
