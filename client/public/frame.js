(function () {
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
      console.log("\n//Loading", name);
      if (modules[name]) {
        console.log("//cached returning:\n", modules[name].exports);
        return modules[name].exports;
      }
      function require(path) {
        return load(resolvePath(name, path));
      }
      var module = modules[name] = { exports: {} };
      console.log("//making function source:\n", sources[name]);
      //console.log("module and exports:\n", module);
      //console.log("require:\n", require);
      new Function("module", "exports", "require", sources[name])(module, module.exports, require);
      return module.exports;
    };
  }

  var parent;

  document.addEventListener("DOMContentLoaded", function () {
    console.log("content loaded in iframe");
    window.addEventListener("message", function (event) {
      //console.log("iframe got message", event.data);
      parent = event.source;
      parent.postMessage("trypurescript", "*");
      //console.log("Contents:\n", event.data["<file>"]);

      var file = evalSources(event.data)("<file>");
      //console.log("file is:\n", file);
      if (file.main && typeof file.main === "function") {
        //console.log("launching main");
        file.main();
      }
    }/*, { once: true }*/);
  }/*, { once: true }*/);

  document.addEventListener("click", function (event) {
    console.log("iframe got click");
    if (parent && event.target.nodeName === "A" && event.target.hostname === "gist.github.com") {
      event.preventDefault();
      parent.postMessage({
        gistId: event.target.pathname.split("/").slice(-1)[0]
      }, "*");
    }
  }, false);
})();
