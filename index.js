// main.js (electron)

// Modules to control application life and create native browser window
const { app, BrowserWindow, ipcMain } = require('electron')
const argparse = require('argparse')
const path = require('path')
const { dialog } = require('electron')
const fs = require('fs');
const tmp = require('tmp');
const escapeStringRegexp = require( '@stdlib/utils-escape-regexp-string' );
// const escapeReg = require('escape-string-regexp');
const lineByLine = require('n-readlines');
const { exit } = require('process');

function readLine(w) {
  var l = w.next();
  if (l === false)
    return l;
  else
    return l.toString();
}
function writeLine(fd, line) {
  if (line !== false)
     fs.appendFileSync(fd, line + "\n");
}

const defaults = 
{"tex":
   { prefix: "% GENERATED LATEX",
     suffix: "% END OF GENERATED LATEX",
     include_cmd: "\\input{@}",
     external_tex : false
   },
 "lyx":
   { prefix: "\\end_layout\\n\\end_inset\\n\\begin_inset Preview \\n\\begin_layout Standard \\n\\begin_inset CommandInset include\\nLatexCommand input\\npreview true",
     suffix: "\\end_inset\\n\\end_layout\\n\\end_inset\\n\\begin_inset Note Note\\nstatus open\\n\\begin_layout Plain Layout",
     include_cmd: "filename \"@\"",
     external_tex : true
   }
};
const description = `
This script launches the diagram editor, inspects the given *main file* and waits 
until there is an *incomplete diagram*, that is, a *magic line* of the shape 
MAGIC_STRING [diagram] that is not followed by the list of lines (separated by \\n) specified by 
--prefix.

- MAGIC_STRING is the string specified by --magic
- [diagram] (optional) is a filename, or a json encoded diagram (pasted from the
  editor by using C-c  on some selected diagram, or from the content of a saved 
  file).
- empty lines are ignored in the process and lines are stripped for comparison

Then, the script loads the first incomplete diagram of the main file in the editor (or creates a new
one if the given filename does not exist, or the diagram was not provided) and waits until
the user saves the diagram. Then, it updates the magic line in the first line (i.e., 
update the json encoded diagram in case no filename was provided), adds the prefixes after the magic line, 
and then the latex generated by the diagram (or it writes it to an external file,
see options --external-tex and --include-cmd). Finally, it adds a list of lines specified
by --suffix.

Default prefix/suffix/include argument by extension of the watched file (default is tex):
` + JSON.stringify(defaults, null, 2);
const emptyGraph = {"graph":{"edges":[],"latexPreamble":"","nodes":[],"sizeGrid":200},"version":8};

var parser = new argparse.ArgumentParser({formatter_class: argparse.RawDescriptionHelpFormatter, description: description});
parser.add_argument("--watch", {help: 'file to monitor'});
parser.add_argument("--make-cmd", {help:"make command to be executed"});
var magic = "% YADE DIAGRAM"
parser.add_argument("--magic", {default:magic, help: "default: " + magic});
parser.add_argument("--prefix");
parser.add_argument("--suffix");
//  parser.add_argument("--ext", {default:".json"});
parser.add_argument("--dir", {default:"", help:"directory of diagram files (tex and json)"});
parser.add_argument("--include-cmd", {help:"use in combination with --external-tex"});
parser.add_argument("--external-tex", {action:"store_const",
    const: true,
    help:"For diagrams stored in an external file, this flag "
  + "redirects the generated tex to an external file which is included with --include-cmd"});



// from https://github.com/electron/electron/issues/4690#issuecomment-422617581
if (app.isPackaged) {
  process.argv.unshift(null)
}

var args = parser.parse_args();
// console.log(args);
var watched_file = args.watch;
var is_watch = watched_file != undefined;
var ext = "tex";

if (is_watch) {
  var extname = path.extname(watched_file);
  if (extname.length > 0 && extname[0] == '.')
     ext = extname.slice(1);
}

function getOrDefault(s) {
  return (args[s] === undefined ? defaults[ext][s] : args[s]);;
}

var prefixes = getOrDefault("prefix").split("\\n");
var suffixes = getOrDefault("suffix").split("\\n");
var includecmd = getOrDefault("include_cmd");
var externaltex = getOrDefault("external_tex");
// var watched_file = undefined;


if (is_watch && ! fs.existsSync(watched_file)) {
   console.log(watched_file + " does not exist");
   is_watch = false;
}

// var makeCmd = args.make_cmd;
magic = args.magic;
var basedir = args.dir;
//  var extension = args.ext;



var magic_re = new RegExp(escapeStringRegexp(magic.trim()) + "(.*)$");

var handleSave, onfocus;
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

var mainWindow;
function waitIncompleteMsg() {
  mainWindow.webContents.send('simple-msg', 
  "Waiting for incomplete diagram in " + watched_file);
}


function contentToFileName(content) {
  // return path.join(basedir, content + extension);
  return path.join(basedir, content);
}

function texFileName(content) {
  return path.join(basedir, path.basename(content, path.extname(content)) + ".tex");
}

function parseMagic(line) {
  var search = magic_re.exec(line.trim());

  if (search !== null) {
    return search[1].trim();
  } else {
    return null;
  }
}

function contentIsFile(content) {
  return content.trim() != "" && content.trim().charAt(0) != "{"
}

function parsePrefix(line, remainder_arg) {
  
  // copy the array
  var remainder = [...remainder_arg];

  if (remainder.length == 0) {
    return [];
  }

  var linestrip = line.trim();
  if (linestrip === "") {
    return remainder;
  }
  var head = remainder.shift().trim();
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

function loadEditor(diag) {
  var stripped_diag = diag.trim();

  var json;
  if (stripped_diag == "") {
    console.log("Creating new diagram");
    // clear();
    json = emptyGraph;
  } else {
    console.log("Loading diagram ");
    json = JSON.parse (stripped_diag);
    
    
  }
  mainWindow.webContents.send('load-graph', 
       json, "filename", "noload");    
}


function handleFileOneIteration() {
  resetOnFocus();
  const file_lines = new lineByLine(watched_file);

  var remainder = [];
  var index = 0;
  while (remainder.length == 0) {
    var line = "";
    var content = null;
    index++;
    while (content === null) {
      var line = readLine(file_lines);
      if (line === false)
          break;
      content = parseMagic(line);
    }
    if (content != null)
       console.log("Graph found");
    else
       break;
    remainder = prefixes;
    while (remainder !== null && remainder.length > 0) {
      line = readLine(file_lines);
      if (line === false)
         // EOF
         break;
      remainder = parsePrefix(line, remainder)
    }
    if (line === false || remainder === null)
         // EOF
      break;
    

  }
  //  if (file_lines.next() === false)
      // file_lines.close();
  if ((remainder === null || remainder.length > 0) && content !== null) {
    console.log("do something with " + content);
    diagFile = null;
  
    if (contentIsFile(content)) {
      diagFile = content;
      rfile = contentToFileName(diagFile);
  
      if (fs.existsSync(rfile)) {
        content = fs.readFileSync(rfile).toString();        
      } else {
        content = "";
      }
    }
  
    
    handleSave = function(newcontent_json, latex) {
      resetHandleSave();
      var newcontent = JSON.stringify(newcontent_json);
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
    
      if (diagFile !== null) {
        wfile = contentToFileName(diagFile);
        console.log("writing to the file " + wfile);
        fs.writeFileSync(wfile, newcontent);
        
    
        if (externaltex) {
          texFile = texFileName(diagFile);
          console.log("writing tex file " + texFile);
          fs.writeFileSync(texFile, latex);
        }
      }
    
      writeContent(newcontent, latex, index);
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

function writeContent(newcontent, latex, index) {
  
  const tmpobj = tmp.fileSync();
  const fd = tmpobj.fd;
  const file_lines = new lineByLine(watched_file);
  
  var line = false;
  var content = null;
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
  var isFile = contentIsFile(content);
  if (isFile)
     writeLine(fd, line)
  else
     writeLine(fd, magic + " " + newcontent)
  writeLine(fd, prefixes.join("\n"));
  if (! externaltex || ! isFile)
     writeLine(fd, latex);
  else
     writeLine(fd, includecmd.replace("@", texFileName(content)));
  writeLine(fd, "\n"+suffixes.join("\n"));
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


const createWindow = () => {
  // Create the browser window.
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  // and load the index.html of the app.
  mainWindow.loadFile('grapheditor.html');

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()
  mainWindow.webContents.once('did-finish-load', () => {
   
  ipcMain.on('save-graph', (e, data, tex) => handleSave(data,tex));
  // ipcMain.on('save-graph', function(a) {console.log("saved")});
  if (is_watch)
  {
    mainWindow.on('focus', function(e) {
      onfocus();
    })
    handleFileOneIteration();
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

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
