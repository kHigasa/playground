(* クラス定義 *)
class calc =
  object
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method plus = let x = num in func <- (fun y -> x + y)
    method eq = func num
  end;;
(* インスタンス作成 *)
let c = new calc;;
(* メソッド呼び出し *)
c#input 4; c#plus; c#input 2; c#eq;;
type base =
    Oct
  | Dec
  | Hex;;
let string_of = function
    Oct -> Printf.sprintf "%o"
  | Dec -> Printf.sprintf "%d"
  | Hex -> Printf.sprintf "%x";;
class calc_base b =
  object
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method plus = let x = num in func <- (fun y -> x + y)
    method eq = string_of b (func num)
  end;;
let c = new calc_base Hex;;
c#input 17; c#plus; c#input 30; c#eq;;
class calc_double =
  object (self) (* 自分自身を表す名前の宣言 *)
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method plus = let x = num in func <- (fun y -> x + y)
    method eq = func num
    method double = self#plus; self#eq
  end;;
let d = new calc_double in d#input 13; d#double;;
class calc_many_buttons =
  object (self)
    val mutable num = 0
    val mutable func = fun x -> x

    method private shift n = num <- num * 10 + n

    method zero  = self#shift 0
    method one   = self#shift 1
    method two   = self#shift 2
    method three = self#shift 3
    method four  = self#shift 4
    method five  = self#shift 5
    method six   = self#shift 6
    method seven = self#shift 7
    method eight = self#shift 8
    method nine  = self#shift 9

    method plus = let x = num in func <- (fun y -> x + y); num <- 0
    method eq = let r = func num in num <- 0; func <- (fun n -> n); r
  end;;
let c = new calc_many_buttons;;
c#one; c#zero; c#plus; c#two; c#nine; c#eq;;
class demo_calc n m =
  object (self)
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method plus = let x = num in func <- (fun y -> x + y)
    method eq = func num
    initializer
      self#input n; self#plus; self#input m; Printf.printf "%d + %d = %d\n" n m (self#eq)
  end;;
new demo_calc 32 28;;
(* inheritance *)
class calc_minus =
  object
    inherit calc
    method minus = let x = num in func <- (fun y -> x - y)
  end;;
let cm = new calc_minus in cm#input 13; cm#minus; cm#input 4; cm#eq;;
class calc_for_kids =
  object
    inherit calc_minus
    method eq = max (func num) 0
  end;;
let kc = new calc_for_kids in kc#input 4; kc#minus; kc#input 13; kc#eq;;
class calc_for_kids2 =
  object
    inherit calc_minus as super
    method eq = max (super#eq) 0
  end;;
let calc_obj =
  object
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method plus = let x = num in func <- (fun y -> x + y)
    method eq = func num
  end;;
(* クラス名はオブジェクト型(メソッドの型の並び)の単なる別名 *)
let foo x = if x then calc_obj else new calc;;
[calc_obj; new calc];;
(* coercion 型変換 コアーション *)
[new calc; (new calc_double :> calc); (new calc_minus :> calc)];;
(* 多相的オブジェクト型 *)
let test_calc c = c#input 10; c#plus; c#input 20; c#eq = 30
in test_calc (new calc) && test_calc (new calc_for_kids);;
(* #class名でclass名の部分型を指定 *)
let input_ten (c : #calc) = c#input 10;;
input_ten (new calc_for_kids);;
let input_ten calc = calc#input 10 and push_plus calc = calc#plus;;
let f b = if b then input_ten else push_plus;;
(* 抽象クラスを定義 *)
class virtual abstract_calc_demo n m op_name =
  object (s)
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method virtual op : unit (* 抽象メソッド *)
    method eq = func num

    initializer
      s#input n; s#op; s#input m; Printf.printf "%d %s %d = %d\n" n op_name m s#eq
  end;;
class calc_demo_plus n m =
  object
    inherit abstract_calc_demo n m "+"
    
    method op = let x = num in func <- (fun y -> x + y)
  end;;
new calc_demo_plus 6 9;;
class calc_demo_mult n m =
  object
    inherit abstract_calc_demo n m "*"

    method op = let x = num in func <- (fun y -> x * y)
  end;; 
new calc_demo_mult 6 9;;
class calc_many_buttons_00 =
  object
    inherit calc_many_buttons as super

    method zerozero = super#shift 0; super#shift 0
  end;;
(* 明示的なクラス型宣言により、クラス外から隠蔽 *)
class calc_counter :
  object
    inherit calc
    method get : int
  end
  =
    object (s)
      inherit calc as super
      val mutable c = 0

      method get = c
      method private incr = c <- c+1
      method eq = s#incr; super#eq
    end;;
class virtual foo :
  object
    val a : int
    method virtual m : int -> int
  end
  =
  object
    val mutable a = 0
    method m x = 100 + x
  end;;
class type calc_counter_t =
  object
    inherit calc
    method get : int
  end;;
class calc_counter : calc_counter_t =
  object (s)
    inherit calc as super
    val mutable c = 0

    method get = c
    method private incr = c <- c+1
    method eq = s#incr; super#eq
  end;;
let generic_calc init f =
  object
    val mutable num = init
    val mutable func = fun x -> x

    method input n = num <- n
    method op = func <- f num
    method eq = func num
  end;;
let f_calc = generic_calc 0.0 (fun x y -> x *. y) in
f_calc#input 2.0; f_calc#op; f_calc#input 3.14; f_calc#eq;;
let s_calc = generic_calc "" (fun x y -> x ^ y) in s_calc#input "Hello, World!"; s_calc#eq;;
(* 型に関してパラメータ化されている、多相クラス *)
class virtual ['a] generic_calc init =
  object
    val mutable num = (init : 'a)
    val mutable func = fun x -> x

    method input n = num <- n
    method virtual op : unit
    method eq = func num
  end;;
class fcalc =
  object
    inherit [float] generic_calc 0.0

    method op = let x = num in func <- (fun y -> x *. y)
  end;;
let f_calc = new fcalc;;
f_calc#input 2.0; f_calc#op; f_calc#input 3.14; f_calc#eq;;
(* 型として使う *)
(f_calc :> float generic_calc);;
class ['a] olist init =
  object
    val mutable l = (init : 'a list)
    method cons a = l <- a :: l
    method length = List.length l
    method append l' = l <- List.append l l'
  end;;
let ol = new olist [1];;
ol#cons 3; ol#length;;
(* 多相メソッド *)
class ['a] olist_fold init =
  object
    inherit ['a] olist init
    method fold_right : 'b.('a -> 'b -> 'b) -> 'b -> 'b = fun f e -> List.fold_right f l e
  end;;
let l = new olist_fold [1; 2; 3];;
l#fold_right (fun x y -> float_of_int x +. y) 0.0;;
l#fold_right (fun x y -> string_of_int x ^ "; " ^ y) "";;
class ['a] olist_fold' init =
  object (self : <fold_right : 'b.('a -> 'b -> 'b) -> 'b -> 'b; ..>)
    inherit ['a] olist init
    method fold_right f e = List.fold_right f l e
  end;;
