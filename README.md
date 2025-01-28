A diagram editor in your browser (with collaborating features). Try it out: https://amblafont.github.io/graph-editor/index.html.
See https://www.youtube.com/watch?v=iWSw4RK8wEk and https://amblafont.github.io/articles/yade.pdf for a short presentation. 

To test offline: open index.html in your browser. The button "open directory" might not work because of the file system API security restrictions, unless you open the page on a local web server. Alternatively you can install the web application (an install button will show up at the top of the html file if your browser allows it).

There is also a vscode extension which embeds the web app to provide Coq mechanisation features (see https://github.com/amblafont/vscode-yade-example).

# Collaborating on the same diagram

By running a server, it is possible to collaborate on the same diagram (check the "Connect" button at the top of the diagram editor, or use the url index.html?server=address-of-the-server). But for that you need to run a server (see below).

## Hosting platform

Check https://github.com/amblafont/yade-server to give it a try with a public available test server, and more information about how to host your own server.

## VSCode

The [vscode extension](https://marketplace.visualstudio.com/items?itemName=amblafont.coreact-yade) includes an implementation of the server, that you can start with the appropriate command.
You can then share the server running locally using the LiveShare vscode extension.

If you prefer to run the server manually (on port 8080, cf head of server.ts):
- `npm install` (the first time, or if needed)
- `make server`



# Easy editing of latex documents

The button "open directory" offers easy editing of diagrams in latex documents (see the directory tools/ for an example). It relies on the files system API, so it does not work (yet?) with Firefox or Safari.
You must pick a directory which includes a file "yade-config.json". This file must contain a field "watchedFile" which indicates the file to be watched. Other fields may be specified: check the top of the file [watcher.ts](ts/watcher.ts).

## Step-by-step process

(might not work with firefox)

1. Create a file named yade-config.json in the same directory as the your latex file `main.tex` with the following content
```
{
    // watchedFile could also be an array of filenames
    "watchedFile": "main.tex",
    "baseDir": "diagrams",
    "externalOutput": true
}
```

2) Create a subdirectory, say, diagrams/ in the main directory; open an instance of the YADE web app, and point to the directory via the Open directory button
3) create new diagrams in the latex file, via `% YADE DIAGRAM name-of-diagram.json` (followed by saving the main.tex file)
4) edit the diagram in the web editor, then press Save in the web editor 

→ now one has a clean generated diagram included in the main.tex via some `\input{...}` command, but also a pair of a .tex file and a .json file in the diagrams sub-directory

5) Bonus feature: whenever a diagram must be edited after creation, all that needs to be done is to delete everything but the `% YADE DIAGRAM name-of-diagram.json string`, and YADE web app will import the .json file created previously; save will regenerate the latex

One could also inline the diagram data in the main latex file by writing  `% YADE DIAGRAM` (without a json filename) to create a new diagram. The generated latex code can also be inlined by removing the `externalOutput` field.

# Compiling the web app

- `npm install` (the first time, or if needed)
- `make`

(or download the latest [elm.js](https://amblafont.github.io/graph-editor/js/elm.js) and [bundle.js](https://amblafont.github.io/graph-editor/js/bundle.js).


# Known issues

- The editor doesn't work with Safari
- The latex integration doesn't work with Firefox (because it doesn't support the file system API)





