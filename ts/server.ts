
import * as WebSocket from 'ws';

// import {Data, ServerToClientDiffs, ClientToServerDiff, ServerToClientDiff, ServerToClientMsg} from "./interface.js";
let port:number = 8080;
if (process.env.PORT !== undefined) {
  let n = parseInt(process.env.PORT);
  if (!isNaN(n))
    port = n;
}

const wss = new WebSocket.Server({ port: port });

/*
Ca peut demenader un 
*/

const depthHistory = 20;

interface item 
{msg:Data, snapshot:boolean, id:number, sender:WebSocket.WebSocket}

let queue:item[] = [];

let nextId = 0;
let lastBreakId:null|number = null; 
let lastSnapshot:null|{msg:Data,id : number, snapshot:true, sender:WebSocket.WebSocket} = null;

function closeAll(reason:string) {
    wss.clients.forEach(ws => {
        closeConnection(ws, reason);
      });
}

function restart() {
  queue = [];
  nextId = 0;
  lastBreakId = null;
  lastSnapshot = null;
  console.log("Server restarted.");
}

function sendToClient(ws:WebSocket.WebSocket, data:ServerToClientMsg) {
  ws.send(JSON.stringify(data));
}

function sendRequestSnapshot(clients:WebSocket.WebSocket[]) {
  console.log("sending snapshot request to %d clients", clients.length);
  clients.forEach(client => sendToClient(client, {type:"snapshotRequest"}));
}

function closeConnection(ws:WebSocket.WebSocket, reason:string) {
  console.log("closing connection: %s", reason);
  ws.close(1011, reason);
}

function sendQueueSince(ws:WebSocket.WebSocket, expectedId:number):boolean {
  
  // strictly bigger is impossible (already checked before)
  if (expectedId >= nextId) {
    return true;
  }

  let idFirst = expectedId;
  let diffs:item[] = [];
  // looking if snapshot is available
  if (lastSnapshot != null && lastSnapshot.id >= expectedId) {    
    idFirst = lastSnapshot.id + 1;
    diffs.push(lastSnapshot);
  } 
  

  let depth = nextId - idFirst;
  if (depth > queue.length) {
    let clients = chooseSnapshotClient();
    if (clients.length == 0) {
      const reason = "no updated client to get data from (server restarted)";
      closeAll(reason);
      return false;
    }
    sendRequestSnapshot(clients);
    return true;
  }
  // let data:ServerToClientDiff[] = []; // queue.slice(-depth);
  for ( let i = 0; i < depth; i++) {
    let msg = queue[queue.length - depth + i];
    diffs.push(msg);
    // diffs.push({isSender : ws === msg.sender, msg: msg.msg
    //     , id:msg.id, snapshot:msg.snapshot
    // });
  }
  const finalDiffs:ServerToClientDiff[] = diffs.map((msg) =>
    {return {"isSender" : ws === msg.sender
     , "msg": msg.msg
     , "id":msg.id
     ,"snapshot":msg.snapshot}}
  );
  let stuff:ServerToClientDiffs = 
  { type:"diffs" , 
    data:finalDiffs
  };

  let stuff2 : ServerToClientMsg = stuff;
  console.log("sending %d diffs to one client", diffs.length);
  sendToClient(ws, stuff2);
  return true;
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
  let id = nextId - queue.length - 1;
  if (lastSnapshot != null && lastSnapshot.id > id)
      return lastSnapshot.id;
  return id;
}

function chooseSnapshotClient():WebSocket.WebSocket[] {
  let clients:WebSocket.WebSocket[] = [];
  const minId = minimalId();
  wss.clients.forEach(ws => {
    if (getExpectedId(ws) >= minId)
      clients.push(ws);
  });
  return clients;
  
}

function saveMsg(ws:WebSocket.WebSocket, msg:ClientToServerDiff) {
  if (!msg.history)
    return;
  queue.push({msg : msg.msg, sender : ws, id : nextId, snapshot: msg.snapshot});
  nextId++;
  queue = queue.slice(-depthHistory);
  console.log("saving msg; queue length: %d", queue.length);
}

function handleReceiveStart(ws:WebSocket.WebSocket, msg:ClientToServerDiff) {
  if (!msg.snapshot) {
      sendRequestSnapshot([ws]);
      return;
  }
  let id = nextId;
  lastBreakId = id;
  nextId++;
  // saveMsg(ws, msg);
  updateLastSnapshot(ws,msg.msg, id);  
  broadcastMsg(ws, msg, id);
}

function updateLastSnapshot(ws:WebSocket.WebSocket, msg:Data, id :number) {
  lastSnapshot = { id : id, snapshot:true, msg:msg, sender:ws };
}

function broadcastMsg(ws:WebSocket.WebSocket, msg:ClientToServerDiff, id:number) {
  if (!msg.broadcast) {
    return;
  }
  console.log("sending diff to %d clients", wss.clients.size);
  wss.clients.forEach(function each(client) {
    let data = {msg:msg.msg, isSender:client === ws, id:id,
         snapshot:msg.snapshot
    };
    if (client.readyState === WebSocket.OPEN) {
      sendToClient(client, { data:[data],
        type:"diffs"
      });
      // client.send(JSON.stringify(msg));
    }
  });
}

wss.on('connection', function connection(ws:WebSocket.WebSocket) {
  
  console.log("new connection");
  
    // */
  
  ws.on('error', console.error);
  ws.on('close', function close() {
    if (wss.clients.size == 0)
        restart();
  });
  ws.on('message', function message(data, isBinary) {
    let str = data.toString();;
    console.log('received: %s', str.substring(0,200));
    // console.log('the queue:');
    // console.log(queue);
    const msg:ClientToServerDiff = JSON.parse(str);

    // if (msg.expectedId > getExpectedId(ws))
    setExpectedId(ws, msg.expectedId);

    if (msg.expectedId > nextId) {
      closeConnection(ws, 
        "impossible: expectedId(" + msg.expectedId + ") > nextId(" + nextId + ")");
      return;
    }

    if (lastSnapshot === null) {
      console.log("no snapshot available (prelude)");
      handleReceiveStart(ws, msg);
      return;
    }
    
    if (!sendQueueSince(ws, msg.expectedId))
        return;



    if (lastBreakId !== null && msg.expectedId <= lastBreakId) {
      return;
    }
    
    if (msg.break) 
      lastBreakId = nextId;

    
    if (msg.snapshot && lastSnapshot.id > msg.expectedId) {
      updateLastSnapshot(ws, msg.msg, msg.expectedId - 1);
    }

    // msg.id = currentId;
    let currentId:number;
    
    if (msg.history)
      currentId = nextId;      
    else
      currentId = nextId - 1;

    saveMsg(ws, msg);



   broadcastMsg(ws, msg, currentId);
  });
});

console.log("Server started on port " + port);