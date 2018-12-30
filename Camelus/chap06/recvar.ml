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
