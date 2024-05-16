type t
(** [t] represents a file path that has both a local and global version. *)

val shadow : string -> string -> t
(** [shadow local global] creates a file path [t] with both a local and global
    version. *)

val path : t -> string
(** [path file_path] returns the most local version of [file_path]. *)
