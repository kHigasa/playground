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
