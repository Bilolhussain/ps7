(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                       Part 2: Lazy Evaluation
 *)

(*======================================================================
Section 2.1: Series acceleration with infinite streams

In the file `nativeLazyStreams.ml`, we provide the definitions of
lazy streams using OCaml's native `Lazy` module as presented in
Section 17.3 in the textbook, and in the file `sampleStreams.ml`, we
provide some sample streams from that chapter, up to and including
code for approximating pi through partial sums of the terms in a
Taylor series (Section 17.4). In lab 15, you used streams to find
faster approximations for pi by averaging adjacent elements in the
stream. In this section, you'll use Aitken's method to generate even
faster convergence to pi. *)
   
open NativeLazyStreams ;;
open SampleStreams ;;

(* Recall from Section 17.4 the use of streams to generate
approximations of pi of whatever accuracy. Try it. You should be able
to reproduce the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within 0.001 of the value of pi. This method converges quite
slowly. 

But we can increase the speed dramatically using a technique called
"series acceleration". In lab 15, you began experimenting with series
acceleration, speeding up the convergence of the `pi_sums` stream by
averaging its elements. The `average` function takes a `float stream`
and returns a stream of `float`s each of which is the average of
adjacent values in the input stream. For example:

    # first 5 (average (to_float nats)) ;;
    - : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
 *)

let rec helper head headtail = 
  ((head +. headtail) /. 2.)
  
let rec average (s : float stream) : float stream = 
  lazy (Cons ( helper (head s) (head (tail s)) , average (tail s)) ) ;;
 
(* 
let av_stream = average (pi_stream) in 
first 10 av_stream;;
let av_stream2 = average (to_float(odds)) in 
first 10 av_stream2;;
let av_stream3 = average (to_float(nats)) in 
first 10 av_stream3;;
*)

(* Now instead of using the stream of approximations in `pi_sums`, we
can instead use the stream of averaged `pi_sums`, which converges much
more quickly:

    # within 0.01 (average pi_sums) ;;
    - : int * float = (9, 3.13707771416749859)
    # within 0.001 (average pi_sums) ;;     
    - : int * float = (30, 3.14209630544471885)
    # within 0.0001 (average pi_sums) ;;
    - : int * float = (99, 3.14154315231477277)
    # within 0.00001 (average pi_sums) ;;
    - : int * float = (315, 3.14158766221239905)
    # within 0.000001 (average pi_sums) ;;
    - : int * float = (999, 3.14159215408966919)
    # within 0.0000001 (average pi_sums) ;;
    - : int * float = (3161, 3.1415926035968269)

We've placed a table below of the number of steps required for the
`pi_sums` and `average pi_sums` methods.

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set writeup in
Section 17.8.3. *)
  
(*......................................................................
Problem 4: Implementing Aitken's method

Write a function `aitken` to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
   
let rec aitken (s: float stream) : float stream =
  let s1 = head (s) in
  let s2 = head (tail(s)) in 
  let s3 = head (tail (tail(s))) in
  let rtn_stream x x1 x2 x3 : float =
    (x1 -. ((x2 -. x3) ** 2.)) /. (x2 -. 2.0 *. x3 +. x3)
  in 
  let y = rtn_stream s s1 s2 s3 in
  lazy (Cons (y, (aitken (tail(s))) ));;

(*
let ait_str = aitken (pi_stream) in 
first 5 ait_str
;;
let ait_stream2 = aitken (to_float(nats)) in 
first 10 ait_stream2;;*)
(*......................................................................
Problem 5: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi using Aitken's method.
 *)
(*
    -------------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  Aitken's method
    -------------------------------------------------------------
    0.1      |        19 |                 2 |
    -------------------------------------------------------------
    0.01     |       199 |                 9 |
    -------------------------------------------------------------
    0.001    |      1999 |                30 |
    -------------------------------------------------------------
    0.0001   |     19999 |                99 |
    -------------------------------------------------------------
    0.00001  |    199999 |               315 |
    -------------------------------------------------------------
    0.000001 |  too many |               999 |
    -------------------------------------------------------------
 *)
(*....................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type `'a
tree` holds a value of some type `'a`, and a (conventional, finite)
list of one or more (lazy) child trees. Complete the implementation by
writing `print_depth`, `tmap`, `tmap2`, and `bfenumerate`. We
recommend implementing them in that order.
......................................................................*)
   
type 'a tree_internal = Node of 'a * 'a tree list
 and 'a tree = 'a tree_internal Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 6: Implement the following functions for manipulating
infinite trees. *)

(*......................................................................
node t -- Returns the element of type `'a` stored at the root node of
tree `t` of type `'a tree`.
......................................................................*)
  
let node (t : 'a tree) : 'a =
  match Lazy.force t with
  |Node (a, _::_::_::_) 
  |Node (a, _::[]) 
  |Node (a, [])
  | Node (a, [_;_])  -> a;;

(*......................................................................
children t -- Returns the list of children of the root node of tree `t`.
......................................................................*)
   
let children (t : 'a tree) : 'a tree list =
  match Lazy.force t with
  | Node (_, [l_c; r_c]) -> 
      let Node (rootl, [_]) = Lazy.force l_c in 
      let Node (rootr, [_]) = Lazy.force r_c in 
      if (rootr != None || rootl != None) 
      then [l_c; r_c] else [];;

(*......................................................................
print_depth n indent t -- Prints a representation of the first `n`
levels of the tree `t` indented `indent` spaces. You can see some
examples of the intended output of `print_depth` below.
......................................................................*)
   
(*
          let rec print_depth (n : int) (indent : int) (t : int tree) : 'a * 'b =
            let rec helper (n: int) (indent: int) (t: int tree) : unit  = 
              let output = node t in 
              let spaces = indent in 
              let sp = " " in
              let printf = printf ("%s" sp) in 
              let printl = printf ("%a\n%..." output) in 
              match (Lazy.force children t) with
              | []  -> print ("%a\n%..." output)
              | [lc, rc] -> 
                  let s = "" in 
                  let rec loop spaces str = 
                    if (spaces != 0) then 
                      loop (spaces-1) (s^print(lazy printf)) 
                    else if (space == 0) then
                      printl 
                  in
                  loop indent s 
            in 
            (helper n indent lc, helper n indent rc);;
*)
let rec print_depth (n : int) (indent : int) (t : int tree) : unit =
  let rec helper (n: int) (indent: int) (t: int tree) : unit  = 
    let output = node t in 
    let spaces = !indent in 
    let no_spaces := ref 0 in 
    let sp = " " in
    let printf = print ("%s" sp) in 
    let printl = print ("%a\n%..." output) in 
    match children t with
    | ls = [lc == Nil, rc == Nil]  -> print ("%a\n%..." output)
    | ls = [lc, rc] -> 
        let s = "" in 
        let rec loop spaces str = 
          if (!spaces !== 0) then 
            loop (spaces-1) (s^print(lazy printf)) 
          else if (!space == 0) then
            lazy printl 
        in
        loop indent s 
  in 
  print (helper n indent lc, helper n indent rc);;

(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function `f` over
each node in `t`.
......................................................................*)
   
let tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  failwith "tmap not implemented" ;;
  
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Node (r, ls = []) -> f r
  | Node (r', ls = [l_c = {Node (_, _)}, r_c = {Node (_, _)}]) -> Node (f r', lazy (tmap f l_c::tmap f r_c))
  ;;
  (*
  let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match Lazy.force t with 
  | Node (r', [l_c; r_c]) ->
      let Node (rootl, [_]) = Lazy.force l_c in 
      let Node (rootr, [_]) = Lazy.force r_c in 
      if (rootr != None && rootl != None) then 
        Node ((f r'), [tmap f l_c;tmap f r_c])
      else
        Node (f r', []);;
  *)

(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function
`f` to corresponding nodes in `t1` and `t2`, which must have the same
"shape". If they don't, an `Invalid_argument` exception is raised.
......................................................................*)
   
let tmap2 (f : 'a -> 'b -> 'c)
          (t1 : 'a tree) (t2 : 'b tree)
        : 'c tree =
  failwith "tmap2 not implemented" ;;
  
let rec tmap2 (f : 'a -> 'b -> 'c)
              (t1 : 'a tree) (t2 : 'b tree)
            : 'c tree =
   match t1, t2 with
  | Node (r, ls = [t_lc! = [] || Null ;_), Node (r', ls = [t_lc' = []; _]) -> Invalid_argument
  | Node (r, ls = [_;  t_rc = []]), Node (r', ls = [_; t_rc' != []]) -> Invalid_argument
  | Node (r, ls = [t_lc != []; t_rc != []]), Node (r', ls' = [t_lc'!= [], t_rc' != []]) 
    ->
      Node (Node ((f r) (f r'), lazy (f tl_c)::lazy (f t_rc):: lazy (tmap2 f tl_c)::lazy (tmap2 f t_rc)))

(*......................................................................
bfenumerate tree_list -- Returns a `stream` of the nodes in the list
of trees `tree_list` enumerated in breadth-first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of `bfenumerate` being applied below. If
there isn't an infinite set of nodes in the list of trees (think about
how that could come about), raise a `Finite_tree` exception.
......................................................................*)
   
let bfenumerate (tree_list : 'a tree list) : 'a stream =
  failwith "bfenumerate not implemented" ;;
  
let rec bfenumerate (tslist : 'a tree list) : 'a stream =
  match tslist with 
  | Node (r, ls = (fst == Nil, tl == Nil) ) -> lazy Cons (r, [])
  | Node (r, ls = (fst !== Nil, tl == Nil) ) -> lazy Cons (r, lazy (Cons((bfenumerate fst), Nil)))
  | Node (r, ls = (fst == Nil, tl != Nil) ) -> lazy Cons (r, lazy (Cons (Nil, (bfenumerate tl)))
  | Node (r, ls = [fst;tl]) -> lazy Cons (r, Cons (lazy (bfenumerate fst), lazy (bfenumerate tl)) ;;


(* Now you'll use your implementation to generate some interesting
infinite trees.  Hint: Drawing a tree considering how the values
change along each branch will yield helpful intuition for the next
problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
   
let onest : int tree =
  lazy (failwith "onest not implemented") ;;
  
let rec onest : int tree =
  Node (1, [onest, onest]);; ;;


(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument `n`. For example:

    # print_depth 2 0 (levels 0) ;;
    0
     1
      2...
      2...
     1
      2...
      2...
    - : unit = ()
......................................................................*)
   
let levels (n : int) : int tree =
  failwith "levels not implemented" ;;
  
let rec levels (n : int) : int tree =
  let rec loop ln_times n () =
      if (ln_times == 0) then 
        let ll = l := !l + 1 in 
        Node (n+1, lazy (loop l*l ll)))
      else 
        let ln_tm = ln_times := !ln_times - 1 in 
        Node (n, lazy [loop ln_tm l])
      in
  let l := !n in
  if (n==0) then Node (0, lazy [loop 1 1])
  else 
  lazy loop l*l l;


(*......................................................................
tree_nats -- An infinite binary tree where the value of each
node in the tree is consecutively numbered in breadth-first order
starting with 0. For example:

    # print_depth 2 0 tree_nats ;;
    0
     1
      3...
      4...
     2
      5...
      6...
    - : unit = ()

    # first 10 (bfenumerate [tree_nats]) ;;
    - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
   
let tree_nats : int tree =
  lazy (failwith "tree_nats not implemented") ;;
  
let rec tree_nats : int tree =
  let rec loop (t: int stream) () =
    let r = node t in
    match t with 
    | Node (r, children t)
    | Node (r, [lc;rc]) -> Node (0, (lazy (loop (mapt (fun x -> x * 2) lc )))::(lazy loop (mapt (fun x -> ((x * 2) + 1)) rc ))) in
  loop nats

(*======================================================================
Reflection on the problem set

     Please fill out the information about time spent and your
     reflection thereon in the file refs.ml.
 *)
