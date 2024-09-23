// adapted from https://github.com/varkor/quiver/blob/master/service-worker/build.js
const { generateSW } = require("workbox-build");

generateSW({
  globDirectory: ".",
  globPatterns: ["index.html", "js/*.js", "katex/**"],
  // so that it does not generate some additional workbox-*.js
  inlineWorkboxRuntime: true,
  swDest: "service-worker.js",
  sourcemap: false,
}).then(({ count, size, warnings }) => {
  if (warnings.length > 0) {
    console.warn(
      "Warnings encountered while generating a service worker:",
      warnings.join("\n")
    );
  }

  console.log(
    `Generated a service worker, which will precache ${count} files, totalling ${size} bytes.`
  );
});