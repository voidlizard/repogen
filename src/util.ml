let (|>) f x = x f

let (<|) f x = f x

let with_head f = function
| x::xs -> (f x)::xs
| []    -> []


let some = function None -> assert false | Some(x) -> x

let some_def def = function None -> def | Some(x) -> x
