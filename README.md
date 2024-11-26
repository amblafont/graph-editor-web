A diagram editor in your browser (with collaborating features). Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk and https://amblafont.github.io/articles/yade.pdf for a short presentation. 

To test offline: open index.html in your browser. The button "open directory" might not work because of the file system API security restrictions, unless you open the page on a local web server. Alternatively you can install the web application (an install button will show up at the top of the html file if your browser allows it).

There is also a vscode extension which embeds the web app to provide Coq mechanisation features (see https://github.com/amblafont/vscode-yade-example).

# Collaborating on the same diagram

By running a server, it is possible to collaborate on the same diagram (check the "Connect" button at the top of the diagram editor). But for that you need to run a server (see below).

## VSCode

The easiest way to use this feature is to install vscode with the [appropriate extension](https://marketplace.visualstudio.com/items?itemName=amblafont.coreact-yade).

You can then share the server running locally using the LiveShare vscode extension.

If you prefer to run the server manually (on port 8080, cf head of server.ts):
- `npm install` (the first time, or if needed)
- `make server`

## Hosting platform

There are some web hosting platforms (such as glitch.com which has a free tier) that can host a server powered by node.js, by importing a github repository.
For this purpose, you can use https://github.com/amblafont/yade-server.


# Easy editing of latex documents

The button "open directory" offers easy editing of diagrams in latex documents (see the directory tools/ for an example). It relies on the files system API, so it does not work (yet?) with Firefox or Safari.
You must pick a directory which includes a file "yade-config.json". This file must contain a field "watchedFile" which indicates the file to be watched. Other fields may be specified: check the top of the file [watcher.ts](ts/watcher.ts).

# Compiling the web app

- `npm install` (the first time, or if needed)
- `make`

(or download the latest elm.js and bundle.js from https://amblafont.github.io/graph-editor/js/elm.js and https://amblafont.github.io/graph-editor/js/bundle.js)




# Ipc capabilities

(This section may be slightly outdated.)

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





