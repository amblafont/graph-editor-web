const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  loadGraph: (callback) => ipcRenderer.on('load-graph', callback),
  saveGraph: (json, tex) => ipcRenderer.send('save-graph', json, tex),
  simpleMsg: (callback) => ipcRenderer.on('simple-msg', callback)
})