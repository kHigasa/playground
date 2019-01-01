let rec nth n l =
  match (n, l) with
    (n, l) when n <= 0 -> None
  | (1, a::_) -> Some a
  | (_, _::rest) -> nth (n-1) rest
  | (_, []) -> None;;
let rec find x = function
    [] -> None
  | a :: l when a = x -> Some a
  | _ :: l -> match find x l with
      None -> None
    | Some i -> Some (i+1);;
let map_sqrt l =
  map (fun x -> if x < 0.0 then None else Some (sqrt x)) l;;
map_sqrt [1.0; -5.0; 6.25];;
let rec map_sqrt = function
    [] -> Some []
  | x :: rest ->
      if x < 0.0 then None
      else match map_sqrt rest with
          None -> None
        | Some l -> Some (sqrt x::l);;
map_sqrt [1.0; -5.0; 6.25];;
raise Division_by_zero;;
raise Not_found;;
raise (Sys_error "File not found");;
let rec fact n =
  if n < 0 then raise (Invalid_argument "fact: negative argument")
  else if n = 0 then 1
  else n * fact (n-1);;
fact 5;;
10 + fact (-10);;
let rec find x = function
    [] -> raise Not_found
  | a :: l when a = x -> 1
  | _ :: l -> 1 + find x l;;
find 9 [1; 2; 3];;
try find 7 [1; 2; 3; 4] with Not_found -> 0;;
try find 2 [1; 2; 3; 4] with Not_found -> 0;;
let rec find' x = function
    [] -> raise Not_found
  | a :: l when a = x -> 1
  | _ :: l -> 1 + find' x l
  let find x l = try find' x l with Not_found -> 0;;
find 7 [0; 8; 7; 3];;
find 9 [];;
find 9 [0; 8; 7; 3];;
let map_sqrt l =
  let sqrt' x =
    if x < 0.0 then raise (Invalid_argument "sqrt'")
    else sqrt x in try Some (map sqrt' l) with Invalid_argument "sqrt'" -> None;;
map_sqrt [1.0; 2.0; 0.25];;
map_sqrt [1.0; -2.0; 0.25];;

