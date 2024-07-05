A diagram editor in your browser. Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk and https://amblafont.github.io/articles/yade.pdf for a short presentation. 

To test offline: open index.html in your browser. The button "open directory" might not work because of the file system API security restrictions, unless you open the page on a local web server. Alternatively you can install the web application (an install button will show up at the top of the html file if your browser allows it).

There is also a vscode extension which embeds the web app to provide Coq mechanisation features (see https://github.com/amblafont/vscode-yade-example).

# Easy editing of latex documents

The button "open directory" offers easy editing of diagrams in latex documents (see the directory tools/ for an example). It relies on the files system API, so it does not work (yet?) with Firefox or Safari.
You must pick a directory which includes a file "yade-config.json". This file must contain a field "watchedFile" which indicates the file to be watched. Other fields may be specified: check the top of the file [watcher.ts](watcher.ts).

# Compiling the web app

- `elm make src/Main.elm --output=elm.js`
- `tsc watcher.ts`

(or download the latest elm.js and watcher.js from https://amblafont.github.io/graph-editor/elm.js and https://amblafont.github.io/graph-editor/watcher.js)


# Compiling the desktop app

The desktop app (embedding the web app in an electron runtime) is now deprecated as its features are now available  using the feature "open directory".

To compile the desktop application, you first need `yarn` to be installed. For this, you need to install `npm`, and then `npm install --global yarn`.
Then,

- `yarn install` to install the nodejs dependencies
- `yarn compile` to compile the app
- `yarn start -- -- arg1 arg2` to run the app
- `yarn make` to build the .deb/.zip

Alternatively, some instructions to build a docker image or a virtualbox image with everything installed are available at https://github.com/amblafont/vscode-yade-example/tree/master/images.

# Ipc capabilities

(This section may be slightly outdated.)

When the desktop (electron) app is launched by another node.js app using `child_process`.
This is also the interface used for communicating when it is embeded in a vscode webview.

Examples:
- sends `{ key : "incomplete-equation", content: {statement :"{ f ; {_} = {_}}", script:string }`
 when pressing 'I' on a selected diagram with two unnamed arrows.
- sends `{ key : "generate-proof", content: coq-script }`
 when generating a coq script from the diagram
- receives `{ key : 'load', content: content}` where content is
  a json graph or an equation of the user-friendly format, and loads it 
  in the editor
- sends `{ key : "apply-proof", content: {statement:"{ f ; {_} = {_}}", script:string } }`
when pressing 'I' on a selected proof node in a diagram
- receives `{ key: "applied-proof", content : {statement : string, script: string}  }`
   Three cases:
   If a subdiagram or a proof node is selected, unifies the statement with the unnamed arrows and add (or mark it as validated) the proof node
   If a chain is selected, creates the other branch and the proof node





