

// dummy interface for type-checking
interface Data {
    dummy:any
}


// interface ClientToServerSnapshot {
//     type : "snapshot"
// }

interface ClientToServerDiff {
    // type: "diff"
    expectedId : number
    snapshot : boolean,
    break : boolean,
    history:boolean,
    broadcast:boolean,
    msg:Data
}

interface ServerToClientDiff {
    // type: "diff"
    id : number,
    snapshot:boolean,
    isSender:boolean,
    // id : number,
    msg:Data
}

type ServerToClientMsg =
  | ServerToClientDiffs
  | ServerToClientSnapshot;

type ServerToClientDiffs = {
  type : "diffs";
  // if the first diff is a snapshot
//   snapshot:boolean;
  // id of the first diff
//   idFirst:number;
  // id of the last diff
//   idLast:number;
  data : ServerToClientDiff[]
};

type ServerToClientSnapshot = {
    type : "snapshotRequest";
};