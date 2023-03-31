const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  loadGraph: (callback) => ipcRenderer.on('load-graph', callback),
  rename: (callback) => ipcRenderer.on('rename', callback),
  saveGraph: (filename, json, tex, svg) => ipcRenderer.send('save-graph', filename, json, tex, svg),
  quicksaveGraph: (filename, json, tex, svg) => ipcRenderer.send('quick-save-graph', 
     filename, json, tex, svg),
  openFile: () => ipcRenderer.send('open-graph'),
  simpleMsg: (callback) => ipcRenderer.on('simple-msg', callback)
})