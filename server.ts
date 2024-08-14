
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

function sendRequestSnapshot(clients:WebSocket.WebSocket[]) {
  clients.forEach(client => sendToClient(client, {type:"snapshotRequest"}));
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
  // looking if snapshot is available
  if (lastSnapshot != null && lastSnapshot.id > expectedId) {
    idFirst = lastSnapshot.id;
    snapshot = true;
    if (lastSnapshot != null)
      diffs.push(lastSnapshot.data); 
  } 
  

  let depth = nextId - idFirst;
  if (depth > queue.length) {
    let clients = chooseSnapshotClient();
    sendRequestSnapshot(clients);
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

const expectedIdKey = "expectedId";

function hasProperty<T extends Object>(obj:T, key: PropertyKey): key is keyof T {
  return key in obj;
}


// function hasExpectedId(ws:WebSocket.WebSocket):boolean {
//   return hasProperty(ws,expectedIdKey);
// }

function getExpectedId(ws:WebSocket.WebSocket):number {
  if (hasProperty(ws,expectedIdKey)) {
    return ws[expectedIdKey] as number;
  }
  return -1;
}

function setExpectedId(ws:WebSocket.WebSocket, id:number) {
  let ws2 = ws as any;
  ws2[expectedIdKey] = id;
}

function minimalId() {
  return nextId - queue.length - 1;
}

function chooseSnapshotClient():WebSocket.WebSocket[] {
  let clients:WebSocket.WebSocket[] = [];
  wss.clients.forEach(ws => {
    if (getExpectedId(ws) >= minimalId())
      clients.push(ws);
  });
  return clients;
  
}


wss.on('connection', function connection(ws:WebSocket.WebSocket) {
  
  let minId = minimalId();
  console.log("new connection with minId: %d", minId);
  if(minId > 0 && (lastSnapshot == null || lastSnapshot.id < minId)) {
    let snapshotClients = chooseSnapshotClient();
    if (snapshotClients.length == 0) {
      let reason = "no updated client to get data from";
      console.log("new connection closed because " + reason);
      ws.close(1011, reason);
      return null;
    }
    sendRequestSnapshot(snapshotClients);
  }
  
  ws.on('error', console.error);
  ws.on('message', function message(data, isBinary) {
    console.log('received: %s', data);
    // console.log('the queue:');
    // console.log(queue);
    const msg:ClientToServerDiff = JSON.parse(data.toString());
    if (msg.expectedId > getExpectedId(ws))
      setExpectedId(ws, msg.expectedId);
    
    sendQueueSince(ws, msg.expectedId)

    if (lastBreakId !== null && msg.expectedId <= lastBreakId) {
      return;
    }
    
    if (msg.break) 
      lastBreakId = nextId;
    
    if (msg.snapshot && (lastSnapshot === null || lastSnapshot.id > msg.expectedId)) {
      lastSnapshot = { id : msg.expectedId - 1, data : msg};
    }

    // msg.id = currentId;
    let currentId:number;
    if (msg.history) {
      currentId = nextId;
      queue.push({msg : msg.msg, sender : ws, id : currentId, snapshot: msg.snapshot});
      nextId++;
      
      queue = queue.slice(-depthHistory);
    }
    else {
      currentId = nextId - 1;
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

console.log("Server started.");