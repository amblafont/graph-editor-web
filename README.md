A diagram editor in your browser. Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk and https://amblafont.github.io/articles/yade.pdf for a short presentation. 

To test offline: open grapheditor.html in your browser.

To compile: elm make src/Main.elm --output=elm.js
(or download the latest elm.js from https://amblafont.github.io/graph-editor/elm.js)

The desktop application (check the release pages) embeds the web app using electron and offers additional features:
- easy editing of diagrams in latex documents (see the directory tools/),
- Coq mechanisation features (see https://github.com/amblafont/vscode-yade-example).

To compile the desktop application, you first need `yarn` to be installed. For this, you need to install `npm`, and then `npm install --global yarn`.
Then,

- `yarn install` to install the nodejs dependencies
- `yarn compile` to compile the app
- `yarn start -- -- arg1 arg2` to run the app
- `yarn make` to build the .deb/.zip

Alternative, some instructions to build a docker image or a virtualbox image with everything installed are available at https://github.com/amblafont/vscode-yade-example/tree/master/images.

Ipc capabilities when the desktop app is launched by another node.js app using `child_process`
---------------
(To be used in a vscode extension building upon coq-lsp)

Examples:
- sends `{ key : "incomplete-equation", content: "{ f ; {_} = {_}}" }`
 when pressing 'I' on a selected diagram with two unnamed arrows.
- sends `{ key : "generate-proof", content: coq-script }`
 when generating a coq script from the diagram
- receives `{ key : 'load', content: content}` where content is
  a json graph or an equation of the user-friendly format, and loads it 
  in the editor
- receives `{ key : "complete-equation", content : {statement : string, script: string}  }` 
   and unifies the statement with the unnamed arrows
   in the selected subdiagram, adding the script as a proof node for the subdiagram.
- sends `{ key : "apply-proof", content: {statement:"{ f ; {_} = {_}}", script:string } }`
when pressing 'I' on a selected proof node in a diagram
- receives `{ key: "applied-proof", content: string }`





