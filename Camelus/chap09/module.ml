List.length [5; 6; 8];;
List.concat [[4; 35; 2]; [1]; [9; -4]];;
let q = Queue.create();;
Queue.add 1 q; Queue.add 2 q;;
Queue.take q;;
Queue.peek q;;
Queue.take q;;
Queue.take q;;
Array.make;;
Array.make 4 'a';;
Array.init;;
Array.init 9 (fun i -> char_of_int (i + 48));;
let x = 110;;
Printf.printf "decimal: %d hexadecimal: %x string: %s\n" x x "foo";;
Printf.printf "pad with zero: %04d\n" 5;;
Printf.fprintf stdout "decimal: %d hexadecimal: %x string: %s\n" x x "foo";;
Printf.sprintf "decimal: %d hexadecimal: %x string: %s\n" x x "foo";;
let f name age = name ^ (if age < 18 then ", you cannot vote." else ", you can vote.");;
let g s = Scanf.sscanf s "%s is %d years old." f;;
g "Higasa is 20 years old.";;
g "I am 15 years old.";;
g "He is 5 years old.";;
format_of_string "abc %s def %d";;
(* 非標準ライブラリモジュール *)
#load "nums.cma";;
Num.add_num;;
open List;;
length [3; 9; 10];;
let x = Num.Int 1 and y = Num.Int 3;;
Num.(+/) x y;;
(* モジュール内で定義された中置演算子を中置演算子として使うために読み込む *)
open Num;;
string_of_num (x // y +/ y // x);;
(* module名は大文字で始める *)
module Tree =
  struct
    type 'a t = Lf | Br of 'a * 'a t * 'a t
    let rec size = function
        Lf -> 0
      | Br (_, left, right) -> 1 + size left + size right
    let rec depth = function
        Lf -> 0
      | Br (_, left, right) -> 1 + max (depth left) (depth right)
  end;;
let tr = Tree.Br (1, Tree.Lf, Tree.Br (2, Tree.Lf, Tree.Lf));;
Tree.depth tr;;
(* モジュール内で定義されたレコード型の要素アクセス *)
module M =
  struct
    type r = {a : int; b : int}
  end;;
let x = {M.a = 1; M.b = 2};;
x.M.a + x.M.b;;
(* table *)
module Table =
  struct
    type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t
    let empty = Empty
    let add key datum table = Entry (key, datum, table)
    let rec retrieve key = function
        Empty -> None
      | Entry (key', datum, rest) ->
        if key = key' then Some datum else retrieve key rest
    let rec delete key = function
        Empty -> Empty
      | Entry (key', datum, rest) ->
        if key = key' then delete key rest else Entry (key', datum, delete key rest)
    let rec dump = function  
        Empty -> []            
      | Entry (key, contents, rest) -> (key, contents) :: (dump (delete key rest))
  end;;
let ( <<< ) table (key, content) = Table.add key content table;;
let table = Table.empty <<< ("a", "the first letter of the English alphabet")
                        <<< ("b", "the second letter of the English alphabet")
                        <<< ("xxx", "sleeping noise");;
Table.retrieve "a" table;;
(* "a"のエントリを上書き *)
let table' = table <<< ("a", "an indefinite article");;
Table.retrieve "a" table';;
Table.dump table';;
(* signatureによる情報隠蔽 *)
(* signatureの定義、補助関数の隠蔽 *)
module type TABLE1 =
  sig
    type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t
    val empty : ('a, 'b) t
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val retrieve : 'a -> ('a, 'b) t -> 'b option
    val dump : ('a, 'b) t -> ('a * 'b) list
  end;;
(* モジュールにシグネチャを与える *)
module Table1 : TABLE1 = Table;;
let ( <<< ) table (key, content) = Table1.add key content table;;
let table = Table1.empty <<< ("a", "the first letter of the English alphabet")
                         <<< ("b", "the second letter of the English alphabet")
                         <<< ("xxx", "sleeping noise") in
let table' = table <<< ("a", "an indefinite article") in
(Table1.retrieve "a" table', Table1.dump table');;
(* =の右辺でシグネチャを指定 *)
module Table1' = (Table : TABLE1);;
(* 抽象データ型：型の定義内容を隠す *)
module type TABLE2 =
  sig
    type ('a, 'b) t
    val empty : ('a, 'b) t
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val retrieve : 'a -> ('a, 'b) t -> 'b option
    val dump : ('a, 'b) t -> ('a * 'b) list
  end;;
module AbsTable : TABLE2 = Table;;
Table.empty;;
AbsTable.empty;;
let atable = AbsTable.add "a" "the first letter of the English alphabet" AbsTable.empty;;
module TableAL = (* AL = associationlist *)
  struct
    let empty = []
    let add key datum table = (key, datum) :: table
    let retrieve key table = try Some (List.assoc key table) with Not_found -> None
    let delete key table = List.filter (fun (key', datum) -> key <> key') table
      let rec dump = function
        [] -> []
      | (key, datum) :: rest -> (key, datum) :: (dump (delete key rest))
  end;;
(* signature matchingではじかれる *)
module AbsTableAL : TABLE2 = TableAL;;
module TableAL : TABLE2 =
  struct
    type ('a, 'b) t = ('a * 'b) list (* 既存の型に別名を付けた *)
    let empty = []
    let add key datum table = (key, datum) :: table
    let retrieve key table = try Some (List.assoc key table) with Not_found -> None
    let delete key table = List.filter (fun (key', datum) -> key <> key') table
      let rec dump = function
        [] -> []
      | (key, datum) :: rest -> (key, datum) :: (dump (delete key rest))
  end;;
