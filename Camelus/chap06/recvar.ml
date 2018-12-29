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
