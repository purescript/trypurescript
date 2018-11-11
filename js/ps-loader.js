var PSMODULE = /^\.\.\/(.*)+\/index\.js$/;

function PSResolver(output) {
  var REQUIRE = /(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g;
  var CACHE = {};

  function getDeps(current, src) {
    var modules = [];
    src.replace(REQUIRE, function(all, ignore, id) {
      if (!ignore) {
        if (id === "./foreign.js") {
          modules.push({
            module: current + "$Foreign",
            path: [output, current, "foreign.js"].join("/")
          })
        } else {
          var mod = id.match(PSMODULE)[1];
          modules.push({
            module: mod,
            path: [output, mod, "index.js"].join("/")
          });
        }
      }
    });
    return modules;
  }

  function load(mod, path) {
    var cached = CACHE[mod];
    if (cached) {
      return Promise.resolve({
        module: mod,
        src: cached
      });
    }
    return fetch(path)
      .then(function(resp) {
        return resp.text();
      })
      .then(function(src) {
        return {
          module: mod,
          src: src
        };
      });
  }

  function go(modules, accum) {
    return Promise.all(
      modules.map(function(dep) {
        return load(dep.module, dep.path);
      })
    ).then(function(sources) {
      var pass = {};
      var more = [];
      sources.forEach(function(source) {
        CACHE[source.module] = source.src;
        accum[source.module] = source.src;
        var deps = getDeps(source.module, source.src);
        deps.forEach(function(dep) {
          if (!pass[dep.module]) {
            pass[dep.module] = true;
            more.push(dep);
          }
        });
      });
      if (more.length) {
        return go(more, accum);
      } else {
        return accum;
      }
    });
  }

  return function(src) {
    return function(cb) {
      return function() {
        go(getDeps("<file>", src), {}).then(function(result) {
          cb(result)();
        }).catch(function(err) {
          console.error(err);
          cb({});
        });
      };
    };
  };
}

function PSLoader(sources) {
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
        return load(path.match(PSMODULE)[1]);
      }
    }
    var mod = { exports: {} };
    modules[name] = mod;
    new Function("module", "exports", "require", src)(mod, mod.exports, req);
    return mod.exports;
  }
}
