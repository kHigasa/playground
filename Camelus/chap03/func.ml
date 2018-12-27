let pi = 3.1415926535;;
let area_of_circle r = (* rは仮引数（パラメーター） *)
  r *. r *. pi;;
(* val area_of_circle : float -> float = <fun> ->は型構築子*)
area_of_circle 2.0;; (* 実引数を与え返り値を得る *)
let x = 1 and y = 2;;
let x = y and y = x;; (* 同時定義の場合、右辺はそこで宣言されるどの変数のscopeにも入らないため、上の定義を参照することになる *)
let big_tuple = ((3, 'a'), (9.3, "hello", false));;
let ((i, c), (f, _, b)) = big_tuple;; (* wildcard pattern *)