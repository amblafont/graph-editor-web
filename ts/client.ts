
  /* ****************



client to collaborate on the same diagram




  ***************** */
export let expectedIdFromServer = 0;

function logExpectedId() {
  console.log("expectedId: ");
  console.log(expectedIdFromServer);
}


export function requestSnapshot(ws:WebSocket):void {
  let msg = null as unknown as Data;
  sendDataOnSocket(ws, {
  snapshot : false,
  break : false,
  history:false,
  broadcast:false,
  msg:msg});
}

export function handleServerToClientMsg(
    ws:WebSocket
  , snapshotRequest:((_:null) => void)
  , normalRequest:((_:ServerToClientDiff[]) => void)
  , data:string) {
  let msg = JSON.parse(data) as ServerToClientMsg;
  switch (msg.type) {
    case "diffs":
      handleServerToClientDiffs(ws, normalRequest, msg.data);
      break;
    case "snapshotRequest":
      snapshotRequest(null);
      break;
  }
}

function handleServerToClientDiffs(
    ws:WebSocket
  , normalRequest:((_:ServerToClientDiff[]) => void)
  , data:ServerToClientDiff[]) {
  let diffs = [];

  for(let i = 0; i < data.length; i++) {
    let diff = data[i];
    // logExpectedId();
    if (diff.id > expectedIdFromServer && !diff.snapshot) {
      requestSnapshot(ws);
      return [];
    }
    // logExpectedId();
    // console.log("avant");
    expectedIdFromServer = diff.id + 1;
    // console.log("apres");
    // logExpectedId();
    diffs.push(diff);
 
  }
  normalRequest(diffs);
}

function sendDiffOnSocket(ws:WebSocket, d:ClientToServerDiff) {
  console.log("sending data on websocket");
  console.log(d);
  ws.send(JSON.stringify(d));
}

function sendDataOnSocket(ws:WebSocket,
    data:{msg:Data, break:boolean, snapshot : boolean,
      broadcast:boolean,
    history:boolean }):void {
      // console.log("avant2");
      // logExpectedId();
      let moreData:ClientToServerDiff =  Object.assign(data, {"expectedId" :expectedIdFromServer}); 
      // {...data, "expectedId" :expectedId};
      // logExpectedId();
      // console.log("sending moredata: ");
      // console.log(moreData);
      sendDiffOnSocket(ws, moreData);
   
}
export function broadcastDataOnSocket(ws:WebSocket,
data:{msg:Data, break:boolean, snapshot : boolean,
history:boolean }):void {
  sendDataOnSocket(ws, {...data, broadcast:true});
}
