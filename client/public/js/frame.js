/*
This script executes the JS files returned by PS compilation.
*/

// Get directory name of path
function dirname(str) {
  let ix = str.lastIndexOf("/");
  return ix < 0 ? "" : str.slice(0, ix);
};

// Concatenates paths together
function resolvePath(a, b) {
  // `b` relative to current directory with `./`
  if (b[0] === "." && b[1] === "/") {
    return dirname(a) + b.slice(1);
  }
  // `b` relative to `a` parent directory with `../`
  if (b[0] === "." && b[1] === "." && b[2] === "/") {
    return dirname(dirname(a)) + b.slice(2);
  }
  // `b` is either shim or path from root
  return b;
};

// Executes JS source and all dependencies.
// Maintains cache of previously-executed sources.
function evalSources(sources) {
  // Cache all modules
  var modules = {};
  // Executes module source, or returns cached exports.
  return function load(name) {
    // Check if module is already cached
    if (modules[name]) {
      return modules[name].exports;
    }
    // Not cached, so execute contents.
    // Provide custom `require`, `module`, and `exports`.
    // Custom `require` which executes file contents, as well as any dependencies.
    function require(path) {
      return load(resolvePath(name, path));
    }
    // Provide empty exports, which will be set, and then returned.
    var module = modules[name] = { exports: {} };
    // Create a function from the module's file contents,
    // and execute this function with our substitutions.
    new Function("module", "exports", "require", sources[name])(module, module.exports, require);
    return module.exports;
  };
};

function loadFrame(str) {
  // Convert JSON string back to object.
  // keys: file paths
  // values: compressed JS source
  obj = JSON.parse(str);

  // Decompress values back to JS source
  Object.keys(obj).forEach(function (key) {
    obj[key] = LZString.decompressFromEncodedURIComponent(obj[key]);
  });

  // Execute all sources, and save returned `exports` from `<file>`.
  // Expecting a `exports.main` entry point.
  let file = evalSources(obj)("<file>");

  // Check if `main` can be launched
  if (!file.main) {
    console.log('Missing "main"');
  } else if (typeof file.main !== "function") {
    console.log('"main" is not a function');
  } else {
    // Launch entry point
    file.main();
  }
};

// Call script tag contents when frame loads.
// Expects a call to loadFrame, passing JS sources.
window.onload = function() {
  // https://stackoverflow.com/a/8677590
  //grab the last script tag in the DOM
  //this will always be the one that is currently evaluating during load
  let tags = document.getElementsByTagName('script');
  let tag = tags[tags.length -1];
  //force evaluation of the contents
  eval( tag.innerHTML );
};
