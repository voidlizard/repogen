open Postgresql
open ExtArray

module P = Printf

let () = Random.self_init ()

let select_all ?bind:(b=[]) (conn:connection) sql f   = 
    let res = (conn#exec ~expect:[Tuples_ok] ~params:(Array.of_list b) sql)
    in  f res#get_all_lst

let with_connection f conn_info = 
    let c = new connection ~conninfo:conn_info ()
    in let x = try f c
                with e -> c#finish; raise e 
    in  c#finish; x

let with_block (conn:connection) f = 
    let _    = conn#exec  ~expect:[Command_ok] "BEGIN;"
    in let r = f ()
    in let _ = conn#exec  ~expect:[Command_ok] "END;"
    in r

let temp_table ?prefix:(p="tmptbl") (conn:connection) sel b = 
    let s = (P.sprintf "%s_%08d") p (Random.int 0x0FFFFFFF)
    in let _ = conn#exec ~expect:[Command_ok] ~params:(Array.of_list b) (P.sprintf "CREATE TEMP TABLE %s ON COMMIT DROP AS (%s);"  s sel)
    in s

let placeholder i n = (P.sprintf "$%d" i)

