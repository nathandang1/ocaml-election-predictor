type t = string * string

let shadow local global = (local, global)
let path (local, global) = if Sys.file_exists local then local else global
