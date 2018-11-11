function PSRequireShim(sources) {
  var nameRegex = /^\.\.\/(.*)+\/index\.js$/;
  var modules = {};
  return function load(name) {
    if (modules[name]) {
      return modules[name].exports;
    }
    var src = sources[name];
    if (src == null) {
      throw new Error("Module not found: " + name);
    }
    function req(path) {
      if (path === "./foreign.js") {
        return load(name + "$Foreign");
      } else {
        return load(path.match(nameRegex)[1]);
      }
    }
    var mod = { exports: {} };
    modules[name] = mod;
    new Function("module", "exports", "require", src)(mod, mod.exports, req);
    return mod.exports;
  }
}
