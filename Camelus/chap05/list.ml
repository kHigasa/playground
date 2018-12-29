let oddnums = [3; 9; 253];;
let more_oddnums = 99 :: oddnums;; (* cons operator *)
let evennums = (* ::は右結合 e1 :: e2 :: l = e1 :: (e2 :: l) *)
  4 :: 10 :: [256; 12];;
[];;
let boollist = true :: false :: [];;
(::)(1, [2]);;
let cons = fun (x, y) -> (::)(x, y);;
let p = (1, [2]) in cons p;;
[(fun x -> x + 1); (fun x -> x * 2);];;
[1; 2; 3] :: [[4; 5]; []; [6; 7; 8]];;
let sum_list3 [x; y; z] = x + y + z;;
let rec sum_list l =
    match l with
      [] -> 0
    | n :: rest -> n + (sum_list rest);;
let rec sum_list = function
  [] -> 0 (* sum_listの再帰呼び出しはない *)
  | n :: rest -> n + (sum_list rest);; (* sum_list l'という再帰呼び出ししか現れない *) 
let rec length = function (* listの構造に関して帰納的である *)
  [] -> 0
  | _ :: rest -> 1 + (length rest);;
length [1; 2; 3];;
length [[true; false]; [false; true; false]];;
let rec append l1 l2 =
  match l1 with
    [] -> l2
  | v :: l1' -> v :: (append l1' l2);;
[1; 2; 3] @ [4; 5; 6];;
let rec reverse = function (* n(n-1)/2 time:cons *)
  [] -> []
  | v :: l' -> reverse l' @ [v];;
let rec revAppend l1 l2 =
  match l1 with
    [] -> l2
  | v :: l1' -> revAppend l1' (v :: l2);;
  let rev l = revAppend l [];; (* n time:cons *)
revAppend [1; 2; 3] [4; 5; 6];;
rev ['a'; 'b'; 'c'; 'd'];;
let rec map f = function
  [] -> []
  | x :: rest -> f x :: map f rest;;
map int_of_char ['a'; 'A'; 'l'];;
map length [[true; false]; []; [false; true; false]];;
let rec forall p = function
  [] -> true
  | x :: rest -> if p x then forall p rest else false;; (* == (p x) && (forall p rest) *)
forall (fun x -> x >= 5) [9; 3; 5];;
let rec exists p = function
  [] -> false
  | x :: rest -> (p x) || (exists p rest);;
exists (fun x -> (x mod 7) = 0) [23; -98; 19; 53];;
(* 畳み込み演算fold：リストの全要素を用いて順に計算する、e：タネ *)
let rec fold_right f l e =
  match l with
    [] -> e
  | x :: rest -> f x (fold_right f rest e)
  let rec fold_left f e l =
    match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest;;
let length l = fold_right (fun x y -> 1 + y) l 0;;
length [1; 2; 3; 4; 5; 2];;
let rec nth n l =
  if n = 1 then let a :: _ = l in a
  else let _ :: rest = l in nth (n-1) rest;;
let rec nth n l =
  match n with
    1 -> let a :: _ = l in a (* constant pattern *)
  | _ -> let _ :: rest = l in nth (n-1) rest;;
let rec nth n l =
  match (n, l) with
    (1, a :: _) -> 1 (* tuple pattern *)
  | (_, _ :: rest) -> nth (n-1) rest;;
let rec nth n l =
  match (n, l) with
    (1, a :: _) -> 1
  | (n', _ :: rest) when n' > 0 -> nth (n-1) rest;; (* ガード付き節 guarded clause *)
(* 連想リスト：association list(ただのtuple(特にペア)のlist) *)
let city_phone = [("Kyoto", "075"); ("Kagawa", "0877"); ("Tokyo", "03")];;
let rec assoc a = function
  (a', b) :: rest -> if a = a' then b else assoc a rest;;
assoc "Kagawa" city_phone;;
assoc "Osaka" city_phone;;
(* sorting *)
let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed in t -. m *. floor (t /. m)
  let rec randlist n seed tail =
    if n = 0 then (seed, tail)
    else randlist (n-1) (nextrand seed) (seed::tail);;
randlist 10 1.0 [];;
(* 挿入ソート：insersion sort *)
let rec insert x = function
  [] -> [x]
  | y :: rest when x < y -> x :: (y :: rest)
  | y :: rest -> y :: (insert x rest);;
let rec insertion_sort = function
  [] -> []
  | x :: rest -> insert x (insersion_sort rest);;
insertion_sort (snd (randlist 10 1.0 []));;
(* Quick sort C.A.R Hoare<-分割統治法：divide-and-conquer method *)
let rec partition pivot = function
  [] -> ([], [])
  | y :: rest -> let (left, right) = partition pivot rest in
    if pivot < y then (left, y::right) else (y::left, right);;
partition 7.0 [9.0; 1.0; 5.0; 4.0; 18.0];;
let rec quick_sort = function
  [] -> []
  | pivot :: rest -> let (left, right) = partition pivot rest in
    quick_sort left @ (pivot :: quick_sort right);;
quick_sort (snd (randlist 10 1.0 []));;
let rec partition left right pivot = function (* 末尾再帰的 *)
  [] -> (left, right)
  | y :: rest ->
    if pivot < y then partition left (y::right) pivot rest
    else partition (y::left) right pivot rest;;
partition [] [] 7.0 [9.0; 1.0; 5.0; 4.0; 18.0];;
let rec quick_start = function
  [] -> []
  | pivot :: rest -> let (left, right) = partition [] [] pivot rest in
    quick_sort left @ (pivot :: quick_sort right);;
quick_sort (snd (randlist 10 1.0 []));;
let rec quick_start = function
  [] -> []
  | pivot :: rest ->
    let rec partition left right = function
      [] -> (left, right)
      | y :: ys ->
        if pivot < y then partition left (y::right) ys
        else partition (y::left) right ys in
        let (left, right) = partition [] [] rest in (quick_sort left) @ (pivot :: quick_sort right);;
quick_sort (snd (randlist 10 1.0 []));;
let rec quick_sort = function
  ([] | [_]) as l -> l (* or pattern, as pattern *)
  | pivot :: rest ->
    let rec partition left right = function
      [] -> (quick_sort left) @ (pivot :: quick_sort right)
      | y :: ys ->
        if pivot < y then partition left (y::right) ys
        else partition (y::left) right ys in partition [] [] rest;;
quick_sort (snd (randlist 10 1.0 []));;
insertion_sort (snd (randlist 10000 1.0 []));;
quick_sort (snd (randlist 10000 1.0 []));;
