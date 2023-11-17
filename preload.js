const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  loadEquation: (callback) => ipcRenderer.on('load-equation', callback),
  loadGraph: (callback) => ipcRenderer.on('load-graph', callback),
  setFirstTab: (callback) => ipcRenderer.on('set-first-tab', callback),
  clearGraph: (callback) => ipcRenderer.on('clear-graph', callback),
  rename: (callback) => ipcRenderer.on('rename', callback),
  saveGraph: (filename, json, exports) => ipcRenderer.send('save-graph', filename, json, exports),
  quicksaveGraph: (filename, json, exports, feedback) => ipcRenderer.send('quick-save-graph', 
     filename, json, exports, feedback),
  openFile: () => ipcRenderer.send('open-graph'),
  simpleMsg: (callback) => ipcRenderer.on('simple-msg', callback),
  prompt: (question, defaut) => ipcRenderer.invoke('prompt', question, defaut),
  incompleteEquation: (statement) => ipcRenderer.send('incomplete-equation', statement),
  generateProof: (script) => ipcRenderer.send('generate-proof', script),
  completeEquation: (callback) => ipcRenderer.on("complete-equation", callback)

})