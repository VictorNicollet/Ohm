(** Collapsing immutable values.
    
    Immutable data structures allow the reuse of common sub-expression between
    complex values: two lists that have the same tail may indeed share the same
    tail value, two binary trees may share certain sub-trees, and so on. In
    practice, such sharing only happens when the second value was constructed
    from the first. If two independent values happen to share most of their
    content, there will be no actual memory sharing. This creates an 
    unnecessary memory load, because more memory is used than otherwise necessary.

    Should this increased memory usage become an issue (for instance, for
    long-lived values in a web server), collapsing common sub-expressions 
    eliminates repetition by recursively traversing the objects and ensuring for
    every two sub-expression [e] and [e'] that if [e = e'] then [e == e'].

    This module provides basic primitives for constructing a collapser for a 
    specific type.

    @author Victor Nicollet
    @version 1.0
*)


(** A collapser function. Such a function guarantees that if [e] and [e'] are
    equal, then [f e == f e']. The actual meaning of {i are equal} is to be
    determined by the user: it can be standard equality ([e = e']), referential
    equality ([e == e']), comparison equality ([compare e e' = 0]) or any
    other possible definition.
*)
type 'a t = 'a -> 'a

(** A referential equality collapser. Returns its argument untouched,
    which ensures that [keep e == keep e'] if [e == e']. Use this compressor
    when working with otherwise non-comparable values (functions, objects).

    {[
let a = "hello", (fun a -> a) in
let b = "hello", (fun a -> a + 1) in

let collapse = Compressor.pair (Compressor.basic ()) (Compressor.keep) in

let a = collapse a in
let b = collapse b in

fst a == fst b (* true *)
    ]}
*)
val keep   : 'a t

(** A list collapser. Uses an item collapser to collapse the list items,
    then collapses the list tail itself. 

    {[
let x = ["a";"b";"c"]
let y = ["b";"c"]

let collapse = Compressor.list (Compressor.basic ()) in

let x = collapse x in
let y = collapse y in
 
List.tl x == y (* true *)
    ]}
   
*)
val list   : 'a t -> 'a list t 

(** An option collapser. Applies the provided collapser to the
    value, if present. 
*)
val option : 'a t -> 'a option t

(** A basic type collapser. [basic ()] is a collapser that works based on 
    equality, so that [f e == f e'] if [e = e']. It is intended for [float] and
    [string] types, as [int] and [bool] values gain nothing from being collapsed.

    Do not use this collapser for values that have sub-values you wish to collapse,
    because it will not recurse.

    {[
let a = "hello" in
let b = "hello" in 

let string = Compressor.basic () in

let a = string a in
let b = string b in

a == b (* true *)
    ]}
*)
val basic  : unit -> 'a t

(** A pair compressor. Recursively compresses the two pair elements.

    {[
let a = "hello", `EQUAL in
let b = "hello", `EQUAL in

let collapse = Compressor.pair (Compressor.basic (), Compressor.keep)

let a = collapse a in
let b = collapse b in

a == b (* true *)
    ]}
*)
val pair   : 'a t -> 'b t -> ('a * 'b) t

(** A simple value collapser. [let f = simple recurse] uses equality, so 
    that [f e == f e'] if [e = e']. The argument [recurse] is a function
    which recursively compresses the contents of the argument, and will
    be called if no equal value has been encountered yet.

    Use this function whenever a recursive value must be handled.

    {[
type t = { a : string ; b : string }

let x = { a = "hello" ; b = "hello" }
let y = { a = "hello" ; b = "hello" }

let string = Compressor.basic () 
let collapse = Compressor.simple
  (fun x -> { a = string x.a ; b = string x.b })

let x = collapse x
let y = collapse y 

x == y (* true *)
x.a == x.b (* true *)
    ]}
*)
val simple : ('a -> 'a) -> 'a t

(** An arbitrary value collapser. [let f = simple project recurse] uses
    the [project] function to determine equality, so that [f e == f e']
    if [project e = project e']. This is used to turn non-comparable
    objects into comparable objects. [recurse] is expected to collapse
    sub-values of the argument, and will be called if no equal value
    has been encountered yet.

    {[
let x = object
  method a = "hello"
  method b = "hello"
end

let y = object
  method a = "hello"
  method b = "hello"
end

let string = Compressor.basic () 
let collapse = Compressor.any 
  (fun x -> x # a, x # b)
  (fun x -> (object
    val a = string (x # a)
    method a = a
    val b = string (x # b)
    method b = b
   end))

let x = collapse x in
let y = collapse y in 

x == y (* true *)
x # a == x # b (* true *)
    ]}
*)
val any    : ('a -> 'b) -> ('a -> 'a) -> 'a t
