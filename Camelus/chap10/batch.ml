let _ = exit 0;;
let _ = exit 1;;
let _ =
  for i = 0 to Array.length Sys.argv - 1 do
    print_endline Sys.argv.(i)
  done;;
type spec =
    Unit of (unit -> unit) (*引数無しキーワード*)
  | Set of bool ref (*引数無しキーワード:与えられた参照にtrueを代入する*)
  | Clear of bool ref (*引数無しキーワード:与えられた参照にfalseを代入する*)
  | String of (string -> unit) (*文字列引数のキーワード*)
  | Int of (int -> unit) (*整数引数のキーワード*)
  | Float of (float -> unit) (*実数引数のキーワード*)
  | Rest of (string -> unit);; (*以後の引数の解釈*)
