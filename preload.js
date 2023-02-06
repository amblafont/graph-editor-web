const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  loadGraph: (callback) => ipcRenderer.on('load-graph', callback),
  saveGraph: (json) => ipcRenderer.send('save-graph', json),
  simpleMsg: (callback) => ipcRenderer.on('simple-msg', callback)
})