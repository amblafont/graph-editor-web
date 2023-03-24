const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  loadGraph: (callback) => ipcRenderer.on('load-graph', callback),
  rename: (callback) => ipcRenderer.on('rename', callback),
  saveGraph: (json, tex) => ipcRenderer.send('save-graph', json, tex),
  quicksaveGraph: (json, tex, filename) => ipcRenderer.send('quick-save-graph', json, tex, filename),
  simpleMsg: (callback) => ipcRenderer.on('simple-msg', callback)
})