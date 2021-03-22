(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                         Native Lazy Streams
 *)

(*......................................................................
A native implementation of lazy streams with some useful functions and
applications. See the corresponding .mli file for documentation.

                    YOU SHOULD NOT EDIT THIS FILE.
 *)

type 'a stream_internal = Cons of 'a * 'a stream
 and 'a stream = 'a stream_internal Lazy.t ;;

let head (s : 'a stream) : 'a =
  let Cons (hd, _tl) = Lazy.force s in hd ;;

let tail (s : 'a stream) : 'a stream =
  let Cons (_hd, tl) = Lazy.force s in tl ;;
  
let rec first (n : int) (s : 'a stream) : 'a list =
  if n = 0 then []
  else head s :: first (n - 1) (tail s) ;;
  
let rec smap (f : 'a -> 'b)
             (s : 'a stream)
           : ('b stream) = 
  lazy (Cons (f (head s), smap f (tail s))) ;;
  
let rec smap2 (f : 'a -> 'b -> 'c)
              (s1 : 'a stream)
              (s2 : 'b stream)
            : 'c stream = 
  lazy (Cons (f (head s1) (head s2), 
              smap2 f (tail s1) (tail s2))) ;;
