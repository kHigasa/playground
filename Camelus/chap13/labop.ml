ListLabels.fold_left ~f:(fun x y -> x + y) ~init:0 [1; 2; 3; 4];;
ListLabels.fold_left ~init:0 ~f:(fun x y -> x + y) [1; 2; 3; 4];;
type ('a, 'b) foldarg = {f: 'a -> 'b -> 'a; init: 'a};;
let rec fold_left' { f=f; init=init } = function
    [] -> init
  | a::rest -> fold_left' {f = f; init = f init a} rest;;
fold_left' {f = (fun x y -> x + y); init = 0} [1; 2; 3; 4];;
fold_left' {init = 0; f = (fun x y -> x + y)};;
(* label付き引数宣言 *)
let rec fold_left ~f:func ~init:e = function
    [] -> e
  | a::rest -> fold_left ~f:func ~init:(func e a) rest;;
(* 略記 *)
let rec fold_left ~f ~init = function
    [] -> init
  | a::rest -> fold_left ~f ~init:(f init a) rest;;
(* オプション引数宣言 *)
let rec seq from ?step:(s=1) n =
  if n <= 0 then [] else from :: seq (from + s) ~step:s (n - 1);;
(* 略記 *)
let rec seq from ?(step=1) n =
  if n <= 0 then [] else from :: seq (from + step) ~step (n - 1);;
seq 5 ~step:2 3;;
seq 3 10;;
seq 1 10 ~step:4;;
(* デフォルト値を与えない *)
let rec seq from ?step n =
  match step with
    None -> if n <= 0 then []
            else from :: seq (from + 1) (n - 1)
  | Some s -> if n <= 0 then []
              else from :: seq (from + s) ~step:s (n - 1);;
seq 1 10 ~step:4;;
seq 1 10;;
let rec seq from ?step n =
  let s = match step with None -> 1 
  | Some s -> s in
  if n <= 0 then [] else from :: seq (from + s) ?step (n - 1);;
let test (f : int -> ?step:int -> int -> 'a list) = f 10 ~step:2 4;;
test seq;;
