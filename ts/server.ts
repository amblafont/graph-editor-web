
import * as WebSocket from 'ws';
import * as http from 'http';

// import {Data, ServerToClientDiffs, ClientToServerDiff, ServerToClientDiff, ServerToClientMsg} from "./interface.js";
let port:number = 8080;
if (process.env.PORT !== undefined) {
  let n = parseInt(process.env.PORT);
  if (!isNaN(n))
    port = n;
}


// Define the port and hostname
let hostname = '0.0.0.0'; // localhost
if (process.env.YADEHOSTNAME !== undefined) {
  hostname = process.env.YADEHOSTNAME;
}

// Create the server

const server = http.createServer((req, res) => {
  // Set the response HTTP headers
  res.statusCode = 200; // OK
  res.setHeader('Content-Type', 'text/html');

  res.end('Server ready! <a href="https://amblafont.github.io/graph-editor/index.html?server=' 
            + 'wss://' + req.headers.host
          + '">Connect from the diagram editor</a>.'
          );
      
});

// Start the server
server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});

const wss = new WebSocket.Server({ noServer: true });

server.on('upgrade', function upgrade(request, socket, head) {
    wss.handleUpgrade(request, socket, head, function done(ws) {
      wss.emit('connection', ws, request);
    });
});


/*
Ca peut demenader un 
*/

const depthHistory = 20;

interface item 
{msg:Data, snapshot:boolean, id:number, sender:WebSocket.WebSocket}

interface session {name:string, queue: item[], nextId:number; lastBreakId:null|number,
    lastSnapshot:null|{msg:Data,id : number, snapshot:true, sender:WebSocket.WebSocket};
}

function freshSession(name:string):session {
  return  {name:name, queue : [], nextId : 0, lastBreakId : null, lastSnapshot : null};
}
let sessions :Record<string, session> = {};

function getSessionFromClient(ws:WebSocket.WebSocket):session|null {
  let sessionName = getSessionName(ws);
  if (!(sessionName  in sessions)) 
    return null;
  return sessions[sessionName];
}

// let queue:item[] = [];

// let nextId = 0;
// let lastBreakId:null|number = null; 
// let lastSnapshot:null|{msg:Data,id : number, snapshot:true, sender:WebSocket.WebSocket} = null;

function closeSession(session : session, reason:string) {
    getSessionClients(session).forEach(ws => {
          closeConnection(ws, reason);
      });
    
    console.log("Closing session " + session.name + ": " + reason);
    delete sessions[session.name];
    let ks = Object.keys(sessions);
    console.log("Remaining opened sessions: " + ks.join(", "));
}

function closeIfNoClient(session:session) {
  let nclient = getSessionClients(session).length;
  // for (let ws of wss.clients) {
  //   if (getSessionName(ws) == session)
  //     nclient++;
  // }
  if (nclient > 0) {
    console.log("Session " + session.name + ": still " + nclient + " connected");
  }
  else {
    closeSession(session, "no client left");
  }
  // console.log("Deleting session " + session + " (no client left).");
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

function sendQueueSince(session:session, ws:WebSocket.WebSocket, expectedId:number):boolean {
  
  // strictly bigger is impossible (already checked before)
  if (expectedId >= session.nextId) {
    return true;
  }

  let idFirst = expectedId;
  let diffs:item[] = [];
  // looking if snapshot is available
  if (session.lastSnapshot != null && session.lastSnapshot.id >= expectedId) {    
    idFirst = session.lastSnapshot.id + 1;
    diffs.push(session.lastSnapshot);
  } 
  

  let depth = session.nextId - idFirst;
  if (depth > session.queue.length) {
    let clients = chooseSnapshotClient(session);
    if (clients.length == 0) {
      const reason = "no updated client to get data from (server restarted)";
      closeSession(session, reason);
      return false;
    }
    sendRequestSnapshot(clients);
    return true;
  }
  // let data:ServerToClientDiff[] = []; // queue.slice(-depth);
  for ( let i = 0; i < depth; i++) {
    let msg = session.queue[session.queue.length - depth + i];
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
  console.log("sending %d diffs to one client (%s)", diffs.length, session.name);
  sendToClient(ws, stuff2);
  return true;
}

// we add those properties to each ws client.
const expectedIdKey = "expectedId";
const sessionNameKey = "sessionName";

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

function getSessionName(ws:WebSocket.WebSocket):string {
  if (hasProperty(ws,sessionNameKey)) {
    return ws[sessionNameKey] as string;
  }
  console.log("client has not " + sessionNameKey);
  return "";
}

function setSessionName(ws:WebSocket.WebSocket, sessionName:string) {
  let ws2 = ws as any;
  ws2[sessionNameKey] = sessionName;
}

function minimalId(session:session) {
  let id = session.nextId - session.queue.length - 1;
  if (session.lastSnapshot != null && session.lastSnapshot.id > id)
      return session.lastSnapshot.id;
  return id;
}

// function getSessionClients(session:session):WebSocket.WebSocket[] {
//   let clients = [];
//   for (client of wss.clients)
    
//   return clients;
// }

function chooseSnapshotClient(session:session):WebSocket.WebSocket[] {
  let clients:WebSocket.WebSocket[] = [];
  const minId = minimalId(session);
  getSessionClients(session).forEach(ws => {
    if (getExpectedId(ws) >= minId)
      clients.push(ws);
  });
  return clients;
  
}

function saveMsg(session:session, ws:WebSocket.WebSocket, msg:ClientToServerDiff) {
  if (!msg.history)
    return;
  session.queue.push({msg : msg.msg, sender : ws, id : session.nextId, snapshot: msg.snapshot});
  session.nextId++;
  session.queue = session.queue.slice(-depthHistory);
  console.log("saving msg; queue length: %d", session.queue.length);
}

function handleReceiveStart(session:session, ws:WebSocket.WebSocket, msg:ClientToServerDiff) {
  if (!msg.snapshot) {
      sendRequestSnapshot([ws]);
      return;
  }
  let id = session.nextId;
  session.lastBreakId = id;
  session.nextId++;
  // saveMsg(ws, msg);
  updateLastSnapshot(session,ws,msg.msg, id);  
  broadcastMsg(session, ws, msg, id);
}

function updateLastSnapshot(session:session, ws:WebSocket.WebSocket, msg:Data, id :number) {
  session.lastSnapshot = { id : id, snapshot:true, msg:msg, sender:ws };
}

function getSessionClients(session:session):WebSocket.WebSocket[]{
  let clients:WebSocket.WebSocket[] = [];
  wss.clients.forEach(function each(client) {
      if (getSessionName(client) == session.name)
        clients.push(client);
    });
  return clients;
}

function broadcastMsg(session:session, ws:WebSocket.WebSocket, msg:ClientToServerDiff, id:number) {
  if (!msg.broadcast) {
    return;
  }
  let clients = getSessionClients(session);

  console.log("sending diff to %d clients", clients.length);
  clients.forEach(function each(client) {
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

wss.on('connection', function connection(ws:WebSocket.WebSocket, request:http.IncomingMessage) {
  // extract session name from url
  let url = request.url;
  console.log("new connection with url: " + url);
  if (url === undefined) {
    closeConnection(ws, "no session specified");
    return;
  }
  let sessionName = url.substring(1); // removing leading /
  
  if (sessionName == "")
      sessionName = "default";
  console.log("session name: " + sessionName);
  setSessionName(ws, sessionName);
  let session = getSessionFromClient(ws);
  if (session === null) {
    console.log("Creating new session: " + sessionName);
    session = freshSession(sessionName);
    sessions[sessionName] = session;    
  }
  
 
  

    // */
  
  ws.on('error', console.error);
  ws.on('close', function close() {
    // if (wss.clients.size == 0)
        closeIfNoClient(session);
  });
  ws.on('message', function message(data) {
    let str = data.toString();;
    console.log('received (' + session.name + '): %s', str.substring(0,200));
    // console.log('the queue:');
    // console.log(queue);
    const msg:ClientToServerDiff = JSON.parse(str);

    // if (msg.expectedId > getExpectedId(ws))
    setExpectedId(ws, msg.expectedId);

    if (msg.expectedId > session.nextId) {
      closeConnection(ws, 
        "impossible: expectedId(" + msg.expectedId + ") > nextId(" + session.nextId + ")");
      return;
    }

    if (session.lastSnapshot === null) {
      console.log("no snapshot available (prelude)");
      handleReceiveStart(session, ws, msg);
      return;
    }
    
    if (!sendQueueSince(session, ws, msg.expectedId))
        return;



    if (session.lastBreakId !== null && msg.expectedId <= session.lastBreakId) {
      return;
    }
    
    if (msg.break) 
      session.lastBreakId = session.nextId;

    
    if (msg.snapshot && session.lastSnapshot.id > msg.expectedId) {
      updateLastSnapshot(session, ws, msg.msg, msg.expectedId - 1);
    }

    // msg.id = currentId;
    let currentId:number;
    
    if (msg.history)
      currentId = session.nextId;      
    else
      currentId = session.nextId - 1;

    saveMsg(session, ws, msg);



   broadcastMsg(session, ws, msg, currentId);
  });
});
