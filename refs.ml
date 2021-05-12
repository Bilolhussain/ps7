(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref ;;

 (*......................................................................
Problem 1: Write a function `has_cycle` that returns `true` if a
mutable list has a cycle, `false` otherwise. You may want a recursive
auxiliary function. Don't worry about space usage.

For instance, we can establish a cyclic and an acyclic mutable list
like this:

    # let sample_end = ref Nil ;;
    # let cyclic = Cons (1, ref (Cons (2, sample_end))) ;;
    # sample_end := cyclic ;;
    # let acyclic = Cons (3, ref (Cons(4, ref Nil))) ;;

and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
......................................................................*)
                                        
let rec check (mls: 'a mlist) (ls:'a mlist ref list) : 'a mlist ref option =
  match mls with
  | Nil -> None
  | Cons (_, tl) -> 
      match !tl with 
      | Nil -> None
      | Cons (_, y_ls) -> 
          if (!y_ls != Nil && List.exists (fun x -> !x == y_ls) ls) then Some tl 
          else check !tl (ref tl::ls)
;;

let has_cycle (ls: 'a mlist) : bool =  
  match ls with 
  | Nil -> false
  | Cons (_, _) -> if ( check ls [] == None) then false else true;;

(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

let flatten (lst: 'a mlist)  : unit =
  match lst with 
  | Nil -> ()
  | Cons (_, tail) -> if (has_cycle (lst)) then 
        match (rt_cycle (ref lst)) with
        | None -> ()
        | Some t -> t:= Nil

(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
......................................................................*)
let rec helper_length (mls: 'a mlist) (ls: 'a mlist ref list) (n: int) : int  = 
  match mls with 
  | Nil -> n
  | Cons (_, tail) -> 
      if (has_cycle mls) then n 
      else 
      if (!tail == Nil) then n+1 else 
        (helper_length !tail (tail::ls) n+1);;
                    
let mlength (lst: 'a mlist) : int =
  let find_cycle = (if (has_cycle lst) then flatten lst) in 
  if (lst == Nil) then 0 
  else helper_length lst [] 0;;

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
  1400 ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "I learned a great deal about manipulating references and wish my eyes allowed me to spend more time than I did" ;;
