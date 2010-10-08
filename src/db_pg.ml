open Postgresql

let select_all (conn:connection) sql f = 
    let res = (conn#exec ~expect:[Tuples_ok] sql)
    in  f res#get_all

let with_connection f conn_info = 
    let c = new connection ~conninfo:conn_info ()
    in let x = try f c
                with e -> c#finish; raise e 
    in  c#finish; x


