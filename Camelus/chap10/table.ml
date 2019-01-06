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
