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
(* 文字列の書き換え *)
let s = "life";;
s.[2]<-'k';
s;;
let pair = ("life", 2);;
(fst pair).[2]<-'k';;
pair;;
let pair = ("life", "life");;
(fst pair).[2]<-'k';;
pair;;
let pair = let p = "life" in (p, p);;
(fst pair).[2]<-'k';;
pair;;
let s = "life";;
let pair1 = ("life", s);;
let pair2 = (s, s);;
(pair1 = pair2, fst pair1 = fst pair2, snd pair1 = snd pair2);;
(* alias: s, snd pair1, snd pair2: 物理的等価なデータに対し違う名前や参照方法がある *)
(pair1 == pair2, fst pair1 == fst pair2, snd pair1 == snd pair2);;
let update_string s1 s2 =
  let () = s1.[0] <- 'a' in
  let () = s2.[0] <- 'b' in
    s1.[0] = s2.[0];;
update_string "xyz" "xyz";;
let s = "xyz" in update_string s s;;
(* フィールドを書き換え可能にする *)
(* reference:書き換え可能なフィールドを一つだけ持つもの *)
type teacher = {name : string; mutable office : string};;
let t = {name = "Koji"; office = "505"};;
(* レコードの書き換え *)
t.office<-"140";;
t;;
t.office.[2]<-'4';;
t;;
let p = ref 5 and q = ref 2;;
(* 参照先の中身の取り出し *)
(!p, !q);;
(* 参照の書き換え *)
p := !p + !q;;
(!p, !q);;
let reflist = [p; q; p];;
reflist;;
p := 100;;
reflist;;
(* 参照の参照 *)
let p = ref 5 and q = ref 2;;
let refp = ref p and refq = ref q;;
!refq := !(!refp);;
(!p, !q);;
(* 配列の生成 *)
let ar = [| 1; 2|];;
(* 配列パターンを使った要素の取り出し、配列パターンの長さはマッチング対象の配列の長さと一致させる *)
let [| b; c|] = ar;;
(* 空リストへの参照を定義 *)
let x = ref [];;
(2 :: !x, true :: !x);;
x := [1];;
(* []への参照が共有されていて両方で多相性を使って、それを異なる型のものとして扱おうとしている *)
let (get, set) =
  let r = ref [] in
  ((fun () -> !r), (fun x -> r:=x));;
1 :: get ();;
"abc" :: get ();;
set ["abc"];;
1 :: get ();;
(* 逐次実行 *)
ignore;;
let print_hello () = print_string "Hello, "; 0;;
print_hello (); print_string "World";;
ignore (print_hello ()); print_string "World\n";;
(* ;とifはifの方が結合が強い *)
let f1 b = if b then print_string "a"; print_string "b\n";;
let f2 b = (if b then print_string "a"); print_string "b\n";;
f1 false;;
f1 true;;
f2 false;;
let f3 b = if b then (print_string "a"; print_string "b");;
f3 false;;
(* 文の並びを囲むときは括弧ではなくbegin/endを使うほうが良い *)
let f3 b = if b then begin print_string "a"; print_string "b" end;;
(* 繰り返し *)
let fact n =
  let i = ref 1 and res = ref 1 in
  while (!i <= n) do
    res := !res * !i; i := !i+1
  done;
  !res;;
fact 5;;
let parrot () =
  let s = ref "" in
  while (s := read_line (); !s <> ".") do
    print_string !s;
    print_endline !s
  done;;
parrot ();;
(* 繰り返し構文を再帰関数で書く *)
let rec whle condition body =
  if condition () then begin body (); whle condition body end;;
let fact n =
  let i = ref 1 and res = ref 1 in
  whle (fun () -> (!i <= n)) (fun () -> res := !res * !i; i := !i+1);
  !res;;
fact 5;;
let rec iter f = function
    [] -> ()
  | a :: rest -> begin f a; iter f rest end;;
iter (fun s -> print_string "Station:"; print_endline s) ["Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"];;
(* Queue *)
type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;
type 'a queue = {mutable head : 'a mlist; mutable tail : 'a mlist};;
let create () = {head = MNil; tail = MNil};;
let q : int queue = create ();;
let add a = function
    {head = MNil; tail = MNil} as q -> let c = MCons (a, ref MNil)
    in q.head <- c; q.tail <- c
  | {tail = MCons (_, next)} as q -> let c = MCons (a, ref MNil)
    in next := c; q.tail <- c
  | _ -> failwith "enqueue: input queue broken";;
add 1 q; add 2 q; add 3 q;;
q;;
let peek = function
    {head = MNil; tail = MNil} -> failwith "hd: queue is empty"
  | {head = MCons (a, _)} -> a
  | _ -> failwith "hd: queue is broken";;
peek q;;
let take = function
    {head = MNil; tail = MNil} -> failwith "dequeue: queue is empty"
  | {head = MCons (a, next)} as q -> q.head <- !next; a
  | _ -> failwith "dequeue: queue is broken";;
take q;;
take q;;
add 4 q; take q;;
ignore (take q); add 5 q; peek q;;
