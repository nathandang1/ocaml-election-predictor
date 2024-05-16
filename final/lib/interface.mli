(** [screen] represents a possible screen in the terminal interface. *)
type screen =
  | Quit
  | Title
  | Menu
  | StatePoll
  | Simulator
  | Results of Candidate.t list * State.t list * Model.model * bool

val title : unit -> screen
(** [title ()] runs the title screen. *)

val menu : unit -> screen
(** [menu ()] runs the menu screen, where users can choose which features they
    would like to use. *)

val state_poll : unit -> screen
(** [state_poll ()] runs the state poll screen, where users can choose to add
    more data to states. *)

val simulator : unit -> screen
(** [simulator ()] runs the simulator screen, where users can choose to
    configure the simulation in advance. *)

val results : Candidate.t list * State.t list -> Model.model -> bool -> screen
(** [results (candidates, state, polling) model randomized] runs the results
    screen, where users view the results of the simulation. *)

val transition : screen -> unit
(** [transition scr] runs an instance of the interface starting at [scr]. *)
