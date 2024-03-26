type t = {name: string; votes: int; pop: int}

type d = float list

let outcome data = 
  if data = [] then [0.]
  else [0.5; 0.5]


