(* レコードの型を定義 *)
type student = { name : string; id : int };;
(* レコードの作成 *)
let st1 = {name = "Taro Yamada"; id = 123456} and st2 = {id = 51; name = "Ichiro Suzuki"};;
(* すでに作成されているレコードを利用して新しいレコードを作成する *)
let st3 = {st1 with id = 234567};;
(* レコードのパターンマッチング *)
let string_of_student {name = n; id = i} = n ^ "'s ID is " ^ string_of_int i;;
string_of_student st1;;
let name_of_student {name = n} = n;;
name_of_student st1;;
(* Dot notation *)
let string_of_student st = st.name ^ "'s ID is " ^ string_of_int st.id;;
string_of_student st1;;
(* 入れ子のレコードの型を一度に定義することはできない *)
type teacher = { tname : string; office : string; ext : int };;
type student_teacher = { s : student; t : teacher };;
let stst = {s = {name = "Taro Yamada"; id = 123456}; t = {tname = "Akira Matsuda"; office = "142"; ext = 1234}};;
(* ヴァリアントを定義 *)
type figure = 
  Point (* 英大文字から始まるのがconstructor *)
| Circle of int
| Rectangle of int * int
| Square of int;;
(* constructorを対応する型の値に適用してヴァリアントの値を構成 *)
let c = Circle 3;;
let figs = [Point; Square 5; Rectangle (4, 5); c];;
(* Rectangleはintのペア1つではなく、intを2つ受け取るコンストラクタ(::も同様) *)
let p = (1, 2) in Rectangle p;;
(* ヴァリアントパターン *)
let area_of_figure = function
  Point -> 0
| Circle r -> r * r * 3 (* Constructor pattern *)
| Rectangle (x, y) -> x * y
| Square x -> x * x;;
area_of_figure c;;
map area_of_figure figs;;
let similar x y =
  match (x, y) with
    (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
    | (Rectangle (11, 12), Rectangle (13, 14)) -> (13 * 12 - 14 * 11) = 0
  | _ -> false;;
similar (Rectangle (2, 4)) (Rectangle (1, 2));;
(* 列挙型 *)
type color = Black | Blue | Red | Magenta | Green | Cyan | Yellow | White;;
let reverse_color = function
  Black -> White | Blue -> Yellow | Red -> Cyan | Magenta -> Green | Green -> Magenta | Cyan -> Red | Yellow -> Blue | White -> Black;;
(* 再帰ヴァリアント型 *)
(* 自然数を表す型nat *)
type nat = Zero | OneMoreThan of nat;;
let zero = Zero and two = OneMoreThan (OneMoreThan Zero);;
(* 足し算 *)
let rec add m n =
  match m with Zero -> Zero | OneMoreThan m' -> OneMoreThan (add m' n);;
(* 整数リスト *)
type intlist = INil | ICons of int * intlist;;
(* 奇数・偶数の相互再帰的定義 *)
type even = Zero | OMT_E of odd and odd = OMT_O of even;;
let rec o_plus_e (OMT_O e1) e2 = OMT_O (e_plus_e e1 e2) and e_plus_e e1 e2 =
  match e1 with Zero -> e2 | OMT_E o -> OMT_E (o_plus_e o e2);;
(* 多相的ヴァリアント型 *)
type 'a mylist = Nil | Cons of 'a * 'a mylist;;
(* 多相的レコード型 *)
type 'a with_location = {loc_x: float; loc_y: float; body: a'};;
{loc_x = 0.0; loc_y = 0.0; body = Point};;
type ('a, 'b) list_with_tail = Nil of b' | Cons of 'a * ('a, 'b) list_with_tail;;
Cons (2, Cons(3, Nil("end")));;
(* オプション型 *)
type 'a option = None | Some of 'a
let fact n =
  let rec fact' n =
    if n = 0 then 1
    else n * fact' (n-1) in
    if n < 0 then None else Some (fact' n);;
fact 3;;
fact (-10);;
type ('a, 'b) sum = Left of 'a | Right of 'b;;
[Left 3; Right "foo"];;
(* 二分木：binary tree *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
(* 木のノード数 *)
let rec size = function
    Lf -> 0
  | Br (_, left, right) -> 1 + size left + size right;;
(* 木の深さ *)
let rec depth = function
    Lf -> 0
  | Br (_, left, right) -> 1 + max (depth left) (depth right);;
(* complete binary tree *)
let comptree = Br (1, Br (2, Br (4, Lf, Lf), Br (5, Lf, Lf)), Br (3, Br (6, Lf, Lf), Br (7, Lf, Lf)));;
size comptree;;
depth comptree;;
(* 行きがけ順 *)
let rec preorder = function
    Lf -> []
  | Br (x, left, right) -> x :: (preorder left) @ (preorder right);;
preorder comptree;;
(* 通りがけ順 *)
let rec inorder = function
    Lf -> []
  | Br (x, left, right) -> (inorder left) @ (x :: inorder right);;
inorder comptree;;
(* 帰りがけ順 *)
let rec postorder = function
    Lf -> []
  | Br (x, left, right) -> (postorder left) @ (postorder right) @ [x];;
postorder comptree;;
let rec preord t l =
  match t with
    Lf -> l
  | Br (x, left, right) -> x :: (preord left (preord right l));;
preord comptree [];;
(* binary search tree *)
let rec mem t x =
  match t with
    Lf -> false
  | Br (y, left, right) ->
      if x = y then true
      else if x < y then mem left x
      else mem right x;;
let rec add t x =
  match t with
    Lf -> Br (x, Lf, Lf)
  | (Br (y, left, right) as whole) when x = y -> whole
  | Br (y, left, right) when x < y -> Br (y, add left x, right)
  | Br (y, left, right) -> Br (y, left, add right x);;
(* rose tree：子ノードの数が不定 *)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;
(* XML:eXtensible Markup Language *)
type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;
let addressbook =
  XBr ("addressbook", [
    XBr ("person", [
      XBr ("name", [
        XLf (Some "Atsushi Igarashi")]);
        XBr ("tel", [XLf (Some "075-123-4567")])
    ]);
    XBr ("person", [XLf None]);
    XBr ("person", [XLf None])
  ]);;
let rec string_of_xml = function
    XBr (tag, xml_list) -> "<" ^ tag ^ ">" ^ string_of_xmllist xml_list ^ "</" ^ tag ^ ">"
  | XLf None -> ""
  | XLf (Some s) -> s
  and string_of_xmllist = function
      [] -> ""
    | xml :: rest -> string_of_xml xml ^ string_of_xmllist rest;;
string_of_xml addressbook;;
let rec rosetree_of_tree = function
    Lf -> RLf
  | Br (a, left, right) -> RBr(a, map rosetree_of_tree [left; right]);;
let rec tree_of_rtree = function
    RLf -> Br (None, Lf, Lf)
  | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
  and tree_of_rtreelist = function
      [] -> Lf
    | rtree :: rest -> let Br (a, left, Lf) = tree_of_rtree rtree in
                       Br (a, left, tree_of_rtreelist rest);;
let rtree =
  RBr("a", [
    RBr ("b", [
      RBr ("c", [RLf]);
      RLf;
      RBr ("d", [RLf])
    ]);
    RBr ("e", [RLf]);
    RB ("f", [RLf])
  ]);;
tree_of_rtree rtree;;
(* 無限列 *)
type intseq = Cons of int * (int -> intseq);;
let rec f x = Cons(x+1, f);;
f 0;;
f 1;;
let rec step2 x = Cons(x+2, step2);;
step2 1;;
let Cons(x1, f1) = step2 0
let Cons(x2, f2) = f1 x1
let Cons(x3, f3) = f2 x2;;
let rec step n x = Cons(x+n, step (n+1));;
let Cons(x1,f1) = step 1 0
let Cons(x2,f2) = f1 x1
let Cons(x3,f3) = f2 x2
let Cons(x4,f4) = f3 x3
let Cons(x5,f5) = f4 x4;;
let rec nthseq n (Cons(x, f)) =
  if n = 1 then x
  else nthseq (n-1) (f x);;
nthseq 7 (step2 0);;
nthseq 6 (step 1 0);;
let is_prime x =
  let rec is_divisible_from_2_to n =
    (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1)) in
    not (is_divisible_from_2_to (x-1));;
is_prime 3;;
is_prime 9;;
is_prime 97;;
let rec next_prime x =
  if is_prime (x+1) then x+1
  else next_prime (x+1);;
next_prime 7;;
next_prime 20;;
next_prime 97;;
let rec prime_seq x =
  if is_prime (x+1) then Cons(x+1, prime_seq)
  else prime_seq (x+1);;
nthseq 20 (prime_seq 1);;
