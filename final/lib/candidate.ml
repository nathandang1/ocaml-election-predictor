type t = {
  name : string;
  party : string;
}

let create (nm, pty) = { name = nm; party = pty }
let name c = c.name
let party c = c.party
