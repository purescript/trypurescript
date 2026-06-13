const tag = "master";

// NOTE: __DEVELOPMENT__ will be replaced with either `true` or `false` by
// esbuild commands in package.json
const config = __DEVELOPMENT__
  ? {
      loaderUrl: "/js/output",
      compileUrl: "http://localhost:8081",
      mainGitHubExample: `/purescript/trypurescript/${tag}/client/examples/Main.purs`,
    }
  : {
      loaderUrl: "https://compile.purescript.org/output",
      compileUrl: "https://compile.purescript.org",
      mainGitHubExample: `/purescript/trypurescript/${tag}/client/examples/Main.purs`,
    };

export const loaderUrl = config.loaderUrl;
export const compileUrl = config.compileUrl;
export const mainGitHubExample = config.mainGitHubExample;
