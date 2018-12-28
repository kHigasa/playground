let pi = 3.1415926535;;
let area_of_circle r = (* rは仮引数（パラメーター） *)
  r *. r *. pi;;
(* val area_of_circle : float -> float = <fun> ->は型構築子*)
area_of_circle 2.0;; (* 実引数を与え返り値を得る *)
let x = 1 and y = 2;;
let x = y and y = x;; (* 同時定義の場合、右辺はそこで宣言されるどの変数のscopeにも入らないため、上の定義を参照することになる *)
let big_tuple = ((3, 'a'), (9.3, "hello", false));;
let ((i, c), (f, _, b)) = big_tuple;; (* wildcard pattern *)
let rec fact n = (* n's factorial linear recursion *)
  if n = 1 then 1 else fact (n-1) * n;;
fact 4;;
fact (-1);;
#trace fact;;
#untrace fact;;
let rec fib n = (* Fibonacci *)
  if n = 1 || n = 2 then 1 else fib(n-1) + fib(n-2);;
fib 100;;
(* tupleを用い修正する *)
let fib n = 
  let rec fib_tuple n =
    if n = 1 then (1, 0)
    else let (i, j) = fib_tuple(n-1) in (i + j, i)
    in
    let (i, _) = fib_tuple n in i;;
(* 相互再帰 mutual recursion *)
let rec even n = (* n⇤Z *)
  if n = 0 then true else odd(n-1)
  and odd n =
    if n = 0 then false else even(n-1);;

let rec pos n =
  neg(n-1) +. 1.0 /. (float_of_int(4 * n + 1))
  and neg n =
    if n < 0 then 0.0
    else pos n -. 1.0 /. (float_of_int(4 * n + 3));;

let fact n =
  let rec iterative_fact (i, res) = (* 反復的階乗関数 *)
    if i = n then res
    else iterative_fact (i+1, res * (i+1))
    in iterative_fact (1, 1);;
fact 4;;

let rec tailfact (n, res) =
  if n = 1 then res (* equal to res 1 *)
  else tailfact (n-1, n * res);;
tailfact (4, 1);;

let rec sum_of (f, n) = (* higher-order function *)
  if n = 0 then 0 else sum_of (f, n-1) + f n;;
let rec sum_curry_of f n =
  if n = 0 then 0 else f n + sum_curry_of f (n-1);;

let sum_of_square n =
  let square x = x * x in sum_of (square, n);;
let sum_of_cube n =
  let cube x = x * x * x in sum_of (cube, n);;

let sum_of_cube n = sum_of ((fun x -> x * x * x), n);;

(fun x -> x * x) 7;;

(* concatenate *)
let concat (s1, s2) = s1 ^ s2 ^ s1;;
let concat_curry s1 = fun s2 -> s1 ^ s2 ^ s1;;
let concat_curry = fun s1 s2 -> s1 ^ s2 ^ s1;;
let concat_curry s1 s2 = s1 ^ s2 ^ s1;; (* 二引数関数とも言えるがcurryingしてあるだけ *)
(concat_curry "abc") "def";;
let emphasize = concat_curry "_";;
emphasize "OCaml";; (* 部分適用している: curryingされた関数の一部の引数を与え、特化した関数を作成 *)

abs ~-3;; (* prefix operator: ~ *)
~-.3.3;;

(+);;
( * );;

let (^-^) x y = x * 2 + y * 3;; (* def infix operator *)
9 ^-^ 6;;

let ( !! ) x = x + 1;; (* prefix operator *)
!!4;;