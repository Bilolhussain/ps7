(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref ;;

(*......................................................................
Problem 1: Write a function `has_cycle` that returns whether a mutable
list has a cycle. You may want a recursive auxiliary function. Don't
worry about space usage.
......................................................................*)
                                      
let has_cycle (lst : 'a mlist) : bool =
  failwith "has_cycle not implemented"

(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

let flatten (lst : 'a mlist) : unit =
  failwith "flatten not implemented"

(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  failwith "mlength not implemented"

(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int =
  failwith "time estimate not provided" ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "...your reflections here..." ;;
