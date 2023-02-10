A diagram editor in your browser. Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk for a short presentation. 

To test offline: open grapheditor.html in your browser

To compile: elm make src/Main.elm --output=elm.js
(or download the latest compiled version from https://amblafont.github.io/graph-editor/elm.js)

To compile the electron app (with easy integration of latex documents: see directory tools/):

- `yarn install` to install the nodejs dependencies
- `yarn start -- -- arg1 arg2` to run the app
- `yarn run make` to build the .deb/.zip



