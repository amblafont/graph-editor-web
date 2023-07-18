A diagram editor in your browser. Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk for a short presentation. 

To test offline: open grapheditor.html in your browser

To compile: elm make src/Main.elm --output=elm.js
(or download the latest compiled version from https://amblafont.github.io/graph-editor/elm.js)

To compile the desktop app (with easy integration of latex documents: see directory tools/), you
first need `yarn` to be installed. For this, you need to install `npm`, and then `npm install --global yarn`.
Then,

- `yarn install` to install the nodejs dependencies
- `yarn start -- -- arg1 arg2` to run the app
- `yarn make` to build the .deb/.zip





