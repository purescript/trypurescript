(function() {
  function evalSources(sources) {
    var modules = {};
    function dirname(str) {
      var ix = str.lastIndexOf("/");
      return ix < 0 ? "" : str.slice(0, ix);
    }
    function resolvePath(a, b) {
      if (b[0] === "." && b[1] === "/") {
        return dirname(a) + b.slice(1);
      }
      if (b[0] === "." && b[1] === "." && b[2] === "/") {
        return dirname(dirname(a)) + b.slice(2);
      }
      return b;
    }
    return function load(name) {
      if (modules[name]) {
        return modules[name].exports;
      }
      function require(path) {
        return load(resolvePath(name, path));
      }
      var module = modules[name] = { exports: {} };
      new Function("module", "exports", "require", sources[name])(module, module.exports, require);
      return module.exports;
    };
  }

  document.addEventListener("DOMContentLoaded", function() {
    window.addEventListener("message", function(event) {
      event.source.postMessage("trypurescript", "*");
      var file = evalSources(event.data)("<file>");
      if (file.main && typeof file.main === "function") {
        file.main();
      }
    }, { once: true });
  }, { once: true });
})();
