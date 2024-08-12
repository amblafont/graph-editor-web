

// dummy interface for type-checking
export interface Data {
    dummy:any
}


// export interface ClientToServerSnapshot {
//     type : "snapshot"
// }

export interface ClientToServerDiff {
    // type: "diff"
    expectedId : number
    snapshot : boolean,
    break : boolean,
    history:boolean,
    broadcast:boolean,
    msg:Data
}

export interface ServerToClientDiff {
    // type: "diff"
    id : number,
    snapshot:boolean,
    isSender:boolean,
    // id : number,
    msg:Data
}

export type ServerToClientMsg =
  | ServerToClientDiffs
  | ServerToClientSnapshot;

export type ServerToClientDiffs = {
  type : "diffs";
  // if the first diff is a snapshot
//   snapshot:boolean;
  // id of the first diff
//   idFirst:number;
  // id of the last diff
//   idLast:number;
  data : ServerToClientDiff[]
};

export type ServerToClientSnapshot = {
    type : "snapshotRequest";
};