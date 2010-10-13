open Postgresql
open ExtArray

module P = Printf

let select_all ?bind:(b=[]) (conn:connection) sql f   = 
    let res = (conn#exec ~expect:[Tuples_ok] ~params:(Array.of_list b) sql)
    in  f res#get_all_lst

let with_connection f conn_info = 
    let c = new connection ~conninfo:conn_info ()
    in let x = try f c
                with e -> c#finish; raise e 
    in  c#finish; x


let placeholder i n = (P.sprintf "$%d" i)

