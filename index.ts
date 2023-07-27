// main.js (electron)

// Modules to control application life and create native browser window
import { app, BrowserWindow, ipcMain } from 'electron'
// const { app, BrowserWindow, ipcMain } = require('electron')
// const argparse = require('argparse')
import argparse from 'argparse'
import path from 'path'
// const path = require('path')
const { dialog } = require('electron')
import fs from 'fs';
const tmp = require('tmp');
const electronPrompt = require('electron-prompt');
const escapeStringRegexp = require( '@stdlib/utils-escape-regexp-string' );
// const escapeReg = require('escape-string-regexp');
// const lineByLine = require('n-readlines');
import lineByLine from 'n-readlines';
const { exit } = require('process');

type Scenario = "watch" | "standard";
const watchScenario = "watch" as Scenario;
const standardScenario = "standard" as Scenario;
type Exports = Record<string,string>;
// const watchScenario = "watch";
// const normalScenario = "standard"

function readLine(w:lineByLine):string|false {
  let l = w.next();
  if (l === false)
    return l;
  else
    return l.toString();
}
function writeLine(fd:fs.PathOrFileDescriptor, line:string|false) {
  if (line !== false)
     fs.appendFileSync(fd, line + "\n");
}

const defaults = {magic: "% YADE DIAGRAM", 
                  external_output : false,
                  watch : true,
                  export: "tex",
                  prefix: "(TO BE DEFINED)",
                  suffix: "",
                  include_cmd: "\\input{@}"
                };

const defaultsExt = 
{"tex":
   { prefix: "% GENERATED LATEX",
     suffix: "% END OF GENERATED LATEX",
     include_cmd: "\\input{@}"
   },
 "lyx":
   { prefix: "\\end_layout\\n\\end_inset\\n\\begin_inset Preview \\n\\begin_layout Standard\\n\\begin_inset CommandInset include\\nLatexCommand input\\npreview true",
     suffix: "\\end_inset\\n\\end_layout\\n\\end_inset\\n\\begin_inset Note Note\\nstatus open\\n\\begin_layout Plain Layout",
     include_cmd: "filename \"@\"",
     external_output : true
   },
   "md":
   { prefix: "-->\\n<!-- GENERATED SVG -->",
     suffix: "<!-- END OF GENERATED SVG -->\\n<!-- ",
     export: "svg"
   },
   "v":
   { prefix: "*)\\n(* GENERATED COQ SCRIPT *)",
     suffix: "(* END OF GENERATED COQ SCRIPT *)\\n(* ",
     export: "coq"
   },
   "json":
   { watch: false }
};
const description = `
This script launches the diagram editor, inspects the given *main file* and waits 
until there is an *incomplete diagram*, that is, a *magic line* of the shape 
MAGIC_STRING [diagram] that is not followed by the list of lines (separated by \\n) specified by 
--prefix.

- MAGIC_STRING is the string specified by --magic
- [diagram] (optional) is a filename, a json encoded diagram (pasted from the
  editor by using C-c  on some selected diagram, or from the content of a saved 
  file), or an equation in the format { a -- f -> b -- g -> c = a -- h -> d -- k -> c }.
- empty lines are ignored in the process and lines are stripped for comparison

Then, the script loads the first incomplete diagram of the main file in the editor (or creates a new
one if the given filename does not exist, or the diagram was not provided) and waits until
the user saves the diagram. Then, it updates the magic line in the first line (i.e., 
update the json encoded diagram in case no filename was provided), adds the prefixes after the magic line, 
and then the latex generated by the diagram (or it writes it to an external file,
see options --external-tex and --include-cmd). Finally, it adds a list of lines specified
by --suffix.

Default prefix/suffix/include argument by extension of the watched file (default is tex):
` + JSON.stringify(defaultsExt, null, 2) + JSON.stringify(defaults, null, 2);

let parser = new argparse.ArgumentParser({formatter_class: argparse.RawDescriptionHelpFormatter, description: description});
parser.add_argument("filename", {nargs: "?"})
parser.add_argument("--watch", {help: 'is it a file to monitor?', action:"store_const", const: true});
parser.add_argument("--make-cmd", {help:"make command to be executed"});

parser.add_argument("--magic", {help: "prompt command"});
parser.add_argument("--export", {help: "tex/svg/coq"});
parser.add_argument("--prefix");
parser.add_argument("--suffix");
//  parser.add_argument("--ext", {default:".json"});
parser.add_argument("--dir", {default:"", help:"relative directory of diagram files (tex/svg/coq/json)"});
parser.add_argument("--include-cmd", {help:"use in combination with --external-output"});
parser.add_argument("--external-output", {action:"store_const",
    const: true,
    help:"For diagrams stored in an external file, this flag "
  + "redirects the generated output (tex/svg/..) to an external file which is included with --include-cmd"});



// from https://github.com/electron/electron/issues/4690#issuecomment-422617581
if (app.isPackaged) {
  process.argv.unshift('')
}

let ext:string

function hasProperty<T extends Object>(obj:T, key: PropertyKey): key is keyof T {
  return key in obj;
}

function getOrDefault(s:keyof typeof defaults) {
  if (args[s] !== undefined)
     return args[s];
  // if (hasProperty(defaultsExt, ext) && hasProperty(defaultsExt[ext], s))
  if (hasProperty(defaultsExt, ext) && hasProperty(defaultsExt[ext], s))
      return defaultsExt[ext][s];
  return defaults[s];
}

let args = parser.parse_args();
// console.log(args);
let main_file = args.filename;
let main_directory: string;
let watched_file: string = "";




let is_watch = false;
if (typeof main_file === "string") {
  main_directory = path.dirname(main_file);
  watched_file = main_file
  if (getOrDefault("watch")) {
     watched_file = main_file;
     is_watch = true
  }
  let extname = path.extname(main_file);
  if (extname.length > 0 && extname[0] == '.')
     ext = extname.slice(1);
}
else
  ext = "tex"






let prefixes:string[] = getOrDefault("prefix").split("\\n");
let suffixes:string[] = getOrDefault("suffix").split("\\n");
let includecmd:string = getOrDefault("include_cmd");
let externalOutput:string = getOrDefault("external_output");
let magic:string = getOrDefault("magic");
let exportFormat:string = getOrDefault("export");
let basedir:string = args.dir;



let magic_re:RegExp = new RegExp(escapeStringRegexp(magic.trim()) + "(.*)$");

let handleSave: (filename:string, data:Object, exports:Exports) => void;
let onfocus :()=> void;
// those reset functions could be 
// avoided using 'once' rather than 'on'
function resetHandleSave() {
  handleSave = function(msg) {};
}

function resetOnFocus() {
  onfocus = function () {
  }
}

resetHandleSave();
resetOnFocus();

let mainWindow:BrowserWindow;
function waitIncompleteMsg() {
  mainWindow.webContents.send('simple-msg', 
  "Waiting for incomplete diagram in " + watched_file);
}


function contentToFileName(content:string):string {
  // return path.join(basedir, content + extension);
  return path.join(main_directory, basedir, content);
}

function outputFileName(content:string, ext:string):string {
  return path.join(basedir, path.dirname(content), path.basename(content, path.extname(content)) + "." + ext);
}

function parseMagic(line:string) {
  let search = magic_re.exec(line.trim());

  if (search !== null) {
    return search[1].trim();
  } else {
    return null;
  }
}

function contentIsFile(content:string) {
  return content.trim() != "" && content.trim().charAt(0) != "{"
}

function parsePrefix(line:string, remainder_arg:string[]) {
  
  // copy the array
  let remainder = [...remainder_arg];

  if (remainder.length == 0) {
    return [];
  }

  let linestrip = line.trim();
  if (linestrip === "") {
    return remainder;
  }
  // we checked that remainder is not empty above
  let head = remainder.shift()!.trim();
  // reaminder is now the tail
  if (head === "") {
    parsePrefix(linestrip, remainder);
  }

  if (linestrip === head) {
    return remainder;
  } else {
    return null;
  }
}

function loadData(data:string, filename:string, scenario:Scenario) {
  let stripped_diag = data.trim();
  try {

    let json = JSON.parse (stripped_diag);
    mainWindow.webContents.send('load-graph', 
       json, filename, scenario); 
    }
  catch (e) {
      if (e instanceof SyntaxError) {
      // if it is not a valid json, then it must be an equation 
        let equation = stripped_diag.slice(1, -1);
        mainWindow.webContents.send('load-equation', equation , "watch");
      }
      else 
        throw e;
  }
}

function loadEditor(diag:string) {
  let stripped_diag = diag.trim();

  if (stripped_diag == "") {
    console.log("Creating new diagram");
    // clear();
    mainWindow.webContents.send('clear-graph', "watch"); 
  } else {
    console.log("Loading diagram ");
    loadData(stripped_diag, "", watchScenario);
  }    
}


function handleFileOneIteration() {
  resetOnFocus();
  const file_lines = new lineByLine(watched_file);

  let remainder:string[]|null = [];
  let index = 0;
  let line = "" as string|false;
  let content:string|null = null;
  while (line !== false && remainder !== null && remainder.length == 0) {
    index++;
    content = null;
    while (content === null) {
      line = readLine(file_lines);
      if (line === false)
          break;
      content = parseMagic(line);
    }
    if (line === false)
      break;
    
    console.log("Graph found");
    remainder = prefixes;
    while (remainder !== null && remainder.length > 0) {
      line = readLine(file_lines);
      if (line === false)
         // EOF
         break;
      remainder = parsePrefix(line, remainder)
    }
  }

  
  if ((remainder === null || remainder.length > 0) && content !== null) {
    console.log("do something with " + content);
    let diagFile:null|string = null;
  
    if (contentIsFile(content)) {
      diagFile = content;
      let rfile = contentToFileName(diagFile);
  
      if (fs.existsSync(rfile)) {
        content = fs.readFileSync(rfile).toString();        
      } else {
        console.log(rfile + " doesn't exist.")
        content = "";
      }
    }
  
    
    handleSave = function(filename:string, newcontent_json:Object, exports:Exports) {
      resetHandleSave();
      let newcontent = JSON.stringify(newcontent_json);
      /*
      newtimestamp = os.path.getmtime(filename);
    
      if (newtimestamp !== oldtimestamp || newcontent === null) {
        ret = true;
    
        if (newcontent === null) {
          console.log("reloading file");
        } else {
          // ici
          ret = dialog.showMessageBoxSync({message: "Main has file changed meanwhile. Do you wish to continue anyway?",
            type: "question",
            'buttons': [
              'Yes',
              'No'
          ]}) == 0;
        }
    
        focusSvg();
    
        if (ret) {
          return true;
        }
      }
      */
      // latex = "nouveau latex"; // genLatex();
      let generatedOutput = exports[exportFormat];
      if (diagFile !== null) {
        let wfile = contentToFileName(diagFile);
        console.log("writing to the file " + wfile);
        fs.writeFileSync(wfile, newcontent);
        
    
        if (externalOutput) {
          let outputFile = path.join(main_directory, outputFileName(diagFile, exportFormat));
          console.log("writing " + exportFormat + " file " + outputFile);
          fs.writeFileSync(outputFile, generatedOutput);
        }
      }
    
      
      writeContent(newcontent, generatedOutput, index);
      handleFileOneIteration();
    }
    // console.log(content);
    loadEditor(content);
  }
  else {
    waitIncompleteMsg();
    onfocus = handleFileOneIteration;
  }
  
 //  handleSave({"graph":{"edges":[],"latexPreamble":"","nodes":[{"id":0,"label":{"isMath":true,"label":"","pos":[277,89.13333129882812]}}],"sizeGrid":200},"version":8});
  
  return false;
  
}

function writeContent(newcontent:string, output:string, index:number) {
  
  const tmpobj = tmp.fileSync();
  const fd = tmpobj.fd;
  const file_lines = new lineByLine(watched_file);
  
  let line:false|string = false;
  let content = null;
  for (let i=0; i < index; i++) {
    writeLine(fd, line);
    content = null;
    line = false;
    while (content === null) {
       writeLine(fd, line);
       line = readLine(file_lines);
       if (line === false)
         break;
       content = parseMagic(line)
    }
  }
  if (content === null) {
     console.log("error");
     app.exit(-1);
     process.exit(-1);
     return;
  }
  let isFile = contentIsFile(content);
  if (isFile)
     writeLine(fd, line)
  else
     writeLine(fd, magic + " " + newcontent)
  writeLine(fd, prefixes.join("\n"));
  if (! externalOutput || ! isFile)
     writeLine(fd, output);
  else
     writeLine(fd, includecmd.replace("@", outputFileName(content, exportFormat)));
  writeLine(fd, suffixes.join("\n"));
  while (line !== false) {
    line = readLine(file_lines);
    if (line === false) {
       break;

    }
    writeLine(fd, line);
  }
  fs.copyFileSync(tmpobj.name, watched_file);

  // tmpobj.removeCallback();
}

function quicksave(filename:string, data:Object, exports:Exports, feedback?:boolean) {
  fs.writeFileSync(filename, JSON.stringify(data));
  if (feedback){
    let msg = "Written to file " + filename;
    dialog.showMessageBoxSync(mainWindow, { message : msg })
  }
}

function saveGraphFile(filename:string, data:Object):void {
  let path = dialog.showSaveDialogSync(mainWindow, { defaultPath : filename});
  if (path === undefined)
     return;
  fs.writeFileSync(path, JSON.stringify(data));
  mainWindow.webContents.send('rename', path);   
}

function openGraphFile() {
  let paths = dialog.showOpenDialogSync(mainWindow);
  if (paths === undefined || paths.length != 1)
     return;
  let path = paths[0];
  loadGraph(path);
}

function loadGraph(path:string) {
  let content = fs.readFileSync(path)
  if (! content)
     return;
  let json = JSON.parse(content.toString());
  let scenario:Scenario = is_watch ? watchScenario : standardScenario;
  mainWindow.webContents.send('load-graph', 
     json, path, scenario);   
}

function configureIpc() {
  process.on("message", msg => 
     {
       const validMsg = msg && typeof msg === "object" 
                         && "key" in msg
                         && "content" in msg;
       if (!validMsg) {
          console.log ("received unknwon message: ");
          console.log(msg);
          return;
       }
      
       switch (msg.key) {
          case "load":
            loadData(msg.content as string, "graph.json", standardScenario)
            break;
          case "complete-equation":
            mainWindow.webContents.send(msg.key, msg.content);
            break;
             
       }
      }
  );
}

function sendExternalMsg(key:string, content:Object, error?:string) {
  console.log({key:key, content:content});
  if (!process.send) {
    if (error)
       dialog.showMessageBoxSync(mainWindow, { message : 
          error });
    return;
  }
  process.send({key:key, content:content})
}


const createWindow = () => {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  let query:Record<string,string> = {}
  mainWindow.loadFile('grapheditor.html' , { query : query });

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()
  mainWindow.webContents.once('did-finish-load', () => {
  ipcMain.on('save-graph', (e, filename, data, exports) => handleSave(filename, data,exports));
  ipcMain.on('open-graph', (e) => openGraphFile());
  ipcMain.on('prompt', (e, question, defaut) =>
  electronPrompt({label:question, value:defaut, width:800}, mainWindow)
       .then((r:any) => mainWindow.webContents.send('answer-prompt', r))
       .catch((_:any) => mainWindow.webContents.send('answer-prompt', null))
  );
  ipcMain.on('quick-save-graph', 
     (e, filename, data, exports, feedback) => is_watch ? handleSave(filename, data,exports)
      : quicksave(filename, data, exports, feedback));
  ipcMain.on('incomplete-equation', (e, s) => sendExternalMsg("incomplete-equation", s, 
     "This feature is only enabled when running the appropriate vscode extension"));
  ipcMain.on('generate-proof', (e, s) => sendExternalMsg("generate-proof", s));
  // ipcMain.on('save-graph', function(a) {console.log("saved")});
  if (is_watch)
  {
    let scenario:Scenario = "watch"
    mainWindow.webContents.send('clear-graph', scenario); 
    mainWindow.on('focus', (e:any) => onfocus());
    handleFileOneIteration();
  }
  else {
    handleSave = (filename, data, exports) =>
         saveGraphFile(filename, data);
    
    if (main_file) {
      loadGraph(main_file)
    }
  }

    // if we are using ipc
  if (process.send) {
    configureIpc();
  }
 }
 );
 
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
  createWindow();
  // handleFileOneIteration();

  app.on('activate', () => {
    // On macOS it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})

