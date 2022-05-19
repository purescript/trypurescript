(function() {
  var parent;

  document.addEventListener("DOMContentLoaded", function() {
    window.addEventListener("message", function(event) {
      parent = event.source;
      parent.postMessage("trypurescript", "*");
      const scriptEl = document.createElement("script");
      scriptEl.type = "module";
      const code = event.data.code;
      const url = event.data.url;
      const codeFixedImports = code
        .split("\n")
        .map((line) => line.replace(/^import (.+) from "..\/([^"]+)";$/, `import $1 from "${url}/$2";`))
        .join("\n");
      const finalCode = codeFixedImports + "\n\nmain();"
      // See https://stackoverflow.com/a/6433770
      try {
        scriptEl.appendChild(document.createTextNode(finalCode));
      } catch (e) {
        scriptEl.text = finalCode;
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
