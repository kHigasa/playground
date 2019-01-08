`Can;;
`May;;
`Can 2;;
`April ("fool", false);;
[`Can 2; `Bottle; `Can 3];;
let c = `Can and m = May and b = `Bottle;;
[c; m];;
(`Can 2, `Can true);;
let kani = function
    `Right -> "walking to the right"
  | `Left -> "walking to the left";;
type seasons = [ `Spring | `Summer | `Autumn | `Winter ];;
type seasons_of_japan = [ seasons | `Tsuyu ];;
let hito = function
    `Forward -> "walking forward"
  | `Backward -> "walking backward"
  | (`Right | `Left) as x -> kani x;;
type kani_dir = [`Left | `Right];;
let hito = function
    `Forward -> "walking forward"
  | `Backward -> "walking backward"
  |  #kani_dir as x -> kani x;;
let rec length = function
    `Nil -> 0
  | `Cons (a, l) -> 1 + length l;;
let rec max_list = function
    `Cons (x, `Nil) -> x
  | `Cons (x, `Cons (y, l)) ->
    if x < y then max_list (`Cons (y, l)) else (`Cons (x, l));;
let rec max_list = function
    `Cons (x, `Nil) -> x
  | `Cons (x, (`Cons (_, _) as l)) ->
    let m = max_list l in if x > m then x else m;;
let l1 = `Nil and l2 = `Cons (1, `Nil) and l3 = `Cons (2, `Cons (1, `Nil));;
let l4 = `Cons (1, `App (l2, l3));;
let rec alength = function
    `Nil -> 0
  | `Cons (a, l) -> 1 + alength l
  | `App (l1, l2) -> alength l1 + alength l2;;
alength l4;;
let make_length f = function
    `Nil -> 0
  | `Cons (a, l) -> 1 + f l;;
let rec length l = make_length length l;;
length l3;;
let make_alength f = function
    (`Nil | `Cons (_, _)) as l -> make_length f l
  | `App (l1, l2) -> f l1 + f l2;;
let rec alength l = make_alength alength l;;
alength l3;;
alength l4;;
type ('a, 'b) mylist = [`Nil | `Cons of 'a * 'b];;
let make_alength f = function
    #mylist as l -> make_length f l
  | `App (l1, l2) -> f l1 + f l2;;
let rec alength l = make_alength alength l;;
