A diagram editor in your browser. Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk for a short presentation. 

To test offline: open grapheditor.html in your browser

To compile: elm make src/Main.elm --output=elm.js
(or download the latest compiled version from https://amblafont.github.io/graph-editor/elm.js)

To compile the desktop app (with easy integration of latex documents: see directory tools/), you
first need `yarn` to be installed. For this, you need to install `npm`, and then `npm install --global yarn`.
Then,

- `yarn install` to install the nodejs dependencies
- `yarn compile` to compile the app
- `yarn start -- -- arg1 arg2` to run the app
- `yarn make` to build the .deb/.zip


Ipc capabilities when the electron app is launched by another node.js app using `child_process`
---------------
(To be used in a vscode extension building upon coq-lsp)

Examples:
- sends `{ key : "incomplete-equation", content: "{ f ; {_} = {_}}" }`
 when pressing 'I' on a selected triangle diagram with two unnamed arrows.
- sends `{ key : "generate-proof", content: coq-script }`
 when generating a coq script from the diagram
- receives `{ key : 'load', content: content}` where content is
  a json graph or an equation of the user-friendly format, and loads it 
  in the editor
- receives `{ key : "complete-equation", content : {statement : string, script: string}  }` 
   and unifies the statement with the unnamed arrows
   in the selected subdiagram, adding the script as a proof node for the subdiagram.





