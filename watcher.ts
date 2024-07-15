// The config interface lists what fields can be found in yade-config.json
// Some default values are available below
// (check defaults, and defaultsExt for each extension)
interface Config {
    // name of the file to watch
    watchedFile:string,
    magic:string,
    // Latex preamble file used for new diagrams
    preambleFile:string,
    /*
    the external output option means that the latex code generated from the diagram is not inlined in the watched file, but is put in an external file, in the directory baseDir, included with the includeCmd command (see defaultsExt.tex below)
    */
    externalOutput:boolean,   
    // base directory to look for and save diagrams
    baseDir:string,
    // see the defaults below
    exportFormat:string,     
    includeCmd:string,
    prefixes:string[],
    suffixes:string[],
    // pwa ne gere pas les commandes shell
    // makeCmd:string|undefined
}

// some defaults
const defaultConfig = {magic: "% YADE DIAGRAM", 
    externalOutput : false,
    exportFormat: "tex",
    baseDir: ".",
    prefixes: [],
    suffixes: [],
    includeCmd: "\\input{@}",
    preambleFile:""
  };

  //
let defaultsExt:any = 
{"tex":
   { prefix: "% GENERATED LATEX",
     suffix: "% END OF GENERATED LATEX",
     includeCmd: "\\input{@}"
   },
 "lyx":
   { prefix: "\\end_layout\\n\\end_inset\\n\\begin_inset Preview \\n\\begin_layout Standard\\n\\begin_inset CommandInset include\\nLatexCommand input\\npreview true",
     suffix: "\\end_inset\\n\\end_layout\\n\\end_inset\\n\\begin_inset Note Note\\nstatus open\\n\\begin_layout Plain Layout",
     includeCmd: "filename \"@\"",
     externalOutput : true
   },
   "md":
   { prefix: "-->\\n<!-- GENERATED SVG -->",
     suffix: "<!-- END OF GENERATED SVG -->\\n<!-- ",
     exportFormat: "svg"
   },
   "v":
   { prefix: "*)\\n(* GENERATED COQ SCRIPT *)",
     suffix: "(* END OF GENERATED COQ SCRIPT *)\\n(* ",
     exportFormat: "coq"
   }};

function hasProperty<T extends Object>(obj:T, key: PropertyKey): key is keyof T {
    return key in obj;
}
  

for (let key in defaultsExt) {
    if (!hasProperty(defaultsExt,key))
        continue;
    const entry = defaultsExt[key];
    entry["prefixes"] = entry.prefix.split('\\n');
    entry["suffixes"] = entry.suffix.split('\\n');
    // remove the key prefix from entry
    delete entry.prefix;
    delete entry.suffix;
}



 

type Exports = Record<string,string>;
function joinPath(...parts:string[]):string {
    return parts.join('/');
}


function pathBasename(p:string):string {
    let lastSlash = p.lastIndexOf('/');
    let lastDot = p.lastIndexOf('.');
    let startIdx = lastSlash + 1;
    let endIdx = lastDot === -1 ? p.length : lastDot;
    return p.substring(startIdx, endIdx);
}

function outputFileName(config:Config, content:string):string {
    return joinPath(config.baseDir, pathBasename(content) + "." + config.exportFormat);
  }

async function getFilehandleFromPath(d:FileSystemDirectoryHandle, filePath:string, options?:FileSystemGetFileOptions):Promise<FileSystemFileHandle>{
    let parts = filePath.split('/'); // Split the path into parts
    let currentHandle = d;
    while (parts.length > 1) {
        const part = parts.shift() as string;
        if (part == ".")
            continue;
        currentHandle = await currentHandle.getDirectoryHandle(part);        
    }
    return currentHandle.getFileHandle(parts[0],options);
}

async function checkFileExistsFromPath(d:FileSystemDirectoryHandle, filePath:string):Promise<boolean>{
    try {
        await getFilehandleFromPath(d,filePath);
        return true;
    }
    catch (e) {
        return false;
    }
}

async function getTextFromFilepath(d:FileSystemDirectoryHandle, filePath : string):Promise<string>{
    let filehandle = await getFilehandleFromPath(d,filePath);
    let file = await filehandle.getFile();
    return file.text();
}

function getLinesFromFilepath(d:FileSystemDirectoryHandle, filePath : string):Promise<string[]>{
    return  getTextFromFilepath(d,filePath).then((text) => text.split('\n'));
}

function readLine(s:string[]):string|false {
    let line = s.shift();
    if (line === undefined) {
        return false;
    }
    return line;
}
function escapeStringRegexp(s:string):string {
    return s.replace(/[/\-\\^$*+?.()|[\]{}]/g, '\\$&');
 }

function parseMagic(magic:string, line:string):{content:string|null,indent:string} {
    let magicRe = new RegExp(escapeStringRegexp(magic.trim()) + "(.*)$");
    let search = magicRe.exec(line.trim());
  
    if (search !== null) {
      let indent = line.search(/\S|$/);
      return {content:search[1].trim(), 
              indent:line.substring(0,indent)};
    } else {
      return {content:null,indent:""};
    }
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

function contentIsFile(content:string) {
    return content.trim() != "" && content.trim().charAt(0) != "{"
}

function contentToFileName(config:Config, content:string) {
    return joinPath(config.baseDir,content);
}

async function fsWriteFile(d:FileSystemDirectoryHandle, filename:string, content:string) {
    let fileHandle = await getFilehandleFromPath(d, filename,{create:true});
    let writer = await fileHandle.createWritable();
    await writer.write(content);
    await writer.close();
}

function writeLine(fd:string[], line:string|false) {
    if (line !== false)
       fd.push( line + "\n");
  }

function writeLines(fd:string[], lines:string[], indent:string) {
    for (let line of lines) {
      writeLine(fd, indent + line);
    }
  }



async function writeContent( config:Config, d:FileSystemDirectoryHandle, newcontent:string, output:string, index:number) {
    let fd:string[] = [];
    const file_lines = await getLinesFromFilepath(d, config.watchedFile);
    
    let line:false|string = false;
    let content = null;
    let indent = "";
    for (let i=0; i < index; i++) {
      writeLine(fd, line);
      content = null;
      line = false;
      while (content === null) {
         writeLine(fd, line);
         line = readLine(file_lines);
         if (line === false)
           break;
         let magic = parseMagic(config.magic, line);
         content = magic.content;
         indent = magic.indent;
      }
    }
    if (content === null) {
       console.log("error");
       throw new Error("error");
       return;
    }
    let isFile = contentIsFile(content);
    if (isFile)
       writeLine(fd, line)
    else
       writeLine(fd, indent + config.magic + " " + newcontent)
       writeLines(fd, config.prefixes, indent);
    if (! config.externalOutput || ! isFile)
       writeLines(fd, output.split("\n"), indent);
    else
       writeLine(fd, indent + config.includeCmd.replace("@", outputFileName(config, content)));
    writeLines(fd, config.suffixes, indent);
    while (line !== false) {
      line = readLine(file_lines);
      if (line === false) {
         break;
  
      }
      writeLine(fd, line);
    }
    // fs.copyFileSync(tmpobj.name, watchedFile);
    // console.log("on va ecrire ceci:" + fd);
    return fsWriteFile(d, config.watchedFile, fd.join(""));
  
    // tmpobj.removeCallback();
  }

interface HandleFileConfig {
    diagFile : null|string,
    index: number,
    // not used by handleSave
    content:string,
    // true if we need to regenerate the external file
    // because it does not exists
    onlyExternalFile:boolean
}
// save
async function watchSaveDiagram(config:Config,
           handleConfig:HandleFileConfig, d:FileSystemDirectoryHandle,
           newcontent_json:Object, exports:Exports) {
    // resetHandleSave();
    let newcontent = JSON.stringify(newcontent_json);
    let generatedOutput = exports[config.exportFormat];
    if (handleConfig.diagFile !== null) {
      let wfile = contentToFileName(config,handleConfig.diagFile);
      console.log("writing to the file " + wfile);
      await fsWriteFile(d, wfile, newcontent);
      
  
      if (config.externalOutput) {
        let outputFile = outputFileName(config,handleConfig.diagFile);
        await fsWriteFile(d,outputFile, generatedOutput);
      }
    }
  
    if (!handleConfig.onlyExternalFile)
        await writeContent(config, d, newcontent, generatedOutput, handleConfig.index);

  }

async function getContent(d:FileSystemDirectoryHandle, config:Config, diagFile:string) {
  let content = "";
  let rfile = contentToFileName(config, diagFile);
  try {
    let fileHandle = await getFilehandleFromPath(d, rfile);
    let file = await fileHandle.getFile();
    content = await file.text();
  }
  // catch NotFoundError
  catch (e) {
      console.log("Error when accessing " + rfile);
      console.log(e);
      // if (e.name === "NotFoundError")
      //     console.log(rfile + " doesn't exist.");
      // else 
      //     console.log(e.message);
  }
  return content;
}
  // undefined means error
  // false means no update
async function checkWatchedFile(config:Config, d:FileSystemDirectoryHandle):Promise<undefined|false|HandleFileConfig> {
//    resetOnFocus();
   let file_lines:string[];
   try {
   file_lines = await getLinesFromFilepath(d,config.watchedFile);
   }
   catch (e) {
       alert("Unable to read " + config.watchedFile);
       console.log(e);
       return undefined;
   }
    
  
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
        content = parseMagic(config.magic, line).content;
      }
      if (line === false)
        break;
      
      console.log("Graph found");
      // check if the tex file exists
      if (content !== null && config.exportFormat && contentIsFile(content)) {
        let diagFile = content;
        let outputFile = outputFileName(config,diagFile);
        let checkExist = await checkFileExistsFromPath(d,outputFile);
        if (!checkExist) {
          let data = await getContent(d, config, diagFile);
          return {diagFile:diagFile, index:index, content:data,
                     onlyExternalFile:true};
        }
      }
      remainder = config.prefixes;
      while (remainder !== null && remainder.length > 0) {
        line = readLine(file_lines);
        if (line === false)
           // EOF
           break;
        remainder = parsePrefix(line, remainder)
      }
    }
  
    if (!((remainder === null || remainder.length > 0) && content !== null)){
      return false;
    }
    
      console.log("do something with " + content);
      let diagFile:null|string = null;
    
      if (contentIsFile(content)) {
        content = await getContent(d, config, content);
      }
    
      
      
    let handleConfig:HandleFileConfig = 
       {content:content, diagFile:diagFile, index:index, onlyExternalFile: false};
    return handleConfig;
      // console.log(content);
    //   loadEditor(content);
  
    
   //  handleSave({"graph":{"edges":[],"latexPreamble":"","nodes":[{"id":0,"label":{"isMath":true,"label":"","pos":[277,89.13333129882812]}}],"sizeGrid":200},"version":8});
    
    // return true;
  }