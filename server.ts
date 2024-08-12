
import * as WebSocket from 'ws';

// import {Data, ServerToClientDiffs, ClientToServerDiff, ServerToClientDiff, ServerToClientMsg} from "./interface.js";

const wss = new WebSocket.Server({ port: 8080 });

/*
Ca peut demenader un 
*/

const depthHistory = 20;

let queue:{msg:Data, snapshot:boolean, id:number, sender:WebSocket.WebSocket}[] = [];

let nextId = 0;
let lastBreakId:null|number = null; 
let lastSnapshot:null|{id : number, data:any} = null;



function sendToClient(ws:WebSocket.WebSocket, data:ServerToClientMsg) {
  ws.send(JSON.stringify(data));
}

function sendRequestSnapshot() {
  wss.clients.forEach(function each(client) {
    sendToClient(client, {type:"snapshotRequest"});
  });
}

function sendQueueSince(ws:WebSocket.WebSocket, expectedId:number) {
  
  // bigger is impossible
  if (expectedId >= nextId) {
    if (expectedId > nextId)
      console.log("impossible: expectedId(%d) > nextId(%d) ", expectedId, nextId);
    return;
  }

  let idFirst = expectedId;
  let diffs:ServerToClientDiff[] = [];
  let snapshot = false;
  if (lastSnapshot != null && lastSnapshot.id > expectedId) {
    idFirst = lastSnapshot.id;
    snapshot = true;
    if (lastSnapshot != null)
      diffs.push(lastSnapshot.data); 
  } 
  

  let depth = nextId - idFirst;
  if (depth > queue.length) {
    sendRequestSnapshot();
    return;
  }
  // let data:ServerToClientDiff[] = []; // queue.slice(-depth);
  for ( let i = 0; i < depth; i++) {
    let msg = queue[queue.length - depth + i];
    diffs.push({isSender : ws === msg.sender, msg: msg.msg
        , id:msg.id, snapshot:msg.snapshot
    });
  }
  let stuff:ServerToClientDiffs = 
  { type:"diffs" , data:diffs,
  };

  let stuff2 : ServerToClientMsg = stuff;

  sendToClient(ws, stuff2);
}



wss.on('connection', function connection(ws) {
  ws.on('error', console.error);

  ws.on('message', function message(data, isBinary) {
    console.log('received: %s', data);
    console.log('the queue:');
    console.log(queue);
    const msg:ClientToServerDiff = JSON.parse(data.toString());
    
    sendQueueSince(ws, msg.expectedId)

    if (lastBreakId !== null && msg.expectedId <= lastBreakId) {
      return;
    }
    let currentId = nextId;
    
    if (msg.break) 
      lastBreakId = currentId;
    
    if (msg.snapshot && (lastSnapshot === null || lastSnapshot.id > msg.expectedId)) {
      lastSnapshot = { id : msg.expectedId - 1, data : msg};
    }

    // msg.id = currentId;
    if (msg.history) {
      nextId++;
      queue.push({msg : msg.msg, sender : ws, id : currentId, snapshot: msg.snapshot});
      queue = queue.slice(-depthHistory);
    }
    if (!msg.broadcast) {
      return;
    }

    wss.clients.forEach(function each(client) {
      let data = {msg:msg.msg, isSender:client === ws, id:currentId,
           snapshot:msg.snapshot
      };
      if (client.readyState === WebSocket.OPEN) {
        sendToClient(client, { data:[data],
          type:"diffs"
        });
        // client.send(JSON.stringify(msg));
      }
    });
  });
});