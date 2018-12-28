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
(* λ-calculus, combinatorylogic *)
(* Combinator 明示的なパラメータの導入なく単純な関数を組み合わせて複雑な関数を構成できる *)
let ($) f g x = f (g x);;
let f = ( ~-. ) $ sqrt in f 2.0;;
(* I-combinator:恒等関数id *)
(sqrt $ id) 3.0;;
(id $ sqrt) 3.0;;
(* K-combinator:定数関数を構成し、K xは必ずxを返す *)
let k x y = x;;
let const17 = k 17 in const17 4.0;;
(* S-combinator:関数合成の・を一般化 *)
let s x y z = x z (y z);;
(* I-combinator == S K K *)
s k k 5;;
