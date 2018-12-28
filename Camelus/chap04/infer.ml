let fst (x, y) = x;;
(* 恒等関数 identity function ;最も単純な多相的関数*)
let id x = x;;
(* 二つの引数を受け取って一方を他方に適用する *)
let apply f x = f x;;
let apply f = fun x -> f x;;
apply abs (-5);;
let twice f x = f (f x);;
twice (fun x -> x + 1) 3;;
twice (fun s -> "<" ^ s ^ ">") "abc";;
twice twice (fun x -> x + 1) 3;;
twice twice (fun s -> "<" ^ s ^ ">") "abc";;
let fourtimes x = twice twice x in (fourtimes (fun s -> "<" ^ s ^ ">") "abc");;
