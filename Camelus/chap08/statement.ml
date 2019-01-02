(* 出力 *)
print_string "Hello, world.\n";;
(* 入力 *)
read_line ();;
(* unit型：()という値をただ一つの要素として持つ *)
();;
type 'a seq = Cons of 'a * (unit -> 'a seq);;
let rec from n = Cons (n, (fun () -> from (n+1)));;
let Cons (x1, f1) = from 1;;
let Cons (x2, f2) = f1 ();;
let Cons (x3, f3) = f2 ();;
let rec mapseq f (Cons (x, tail)) = Cons (f x, fun () -> mapseq f (tail ()));;
(* 逆数 *)
let reciprocals = mapseq (fun x -> 1.0 /. float_of_int x) (from 2);;
