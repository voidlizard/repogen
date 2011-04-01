module Model =
struct

    open ExtList
    open ExtHashtbl
    open ExtString
    open Util
    open CamlTemplate.Model

    let make row_hdr row_data vars var_desc =

        let make_hash items = 
            let hash = Hashtbl.create (List.length items)
            in let () = List.iter (fun (n,v) -> Hashtbl.add hash n (Tstr v)) items
            in (Thash hash)

        in let make_tlist row = Tlist(List.map (fun s -> Tstr(snd s)) row)

        in let mk_obj items = 
            let hash = Hashtbl.create (List.length items)
            in let () = List.iter (fun (n,v) -> Hashtbl.add hash n v) items
            in (Thash hash)

        in let mk_item w = (mk_obj [("list",(make_tlist w));
                                            ("obj", (make_hash w))])

        in let mk_var v = 
            let alias = fst v
            in let value =  snd v
            in let name  = try List.assoc alias var_desc with Not_found -> ""
            in mk_obj [("alias", Tstr alias);("name", Tstr name);("value", Tstr value)]

        in let join ~args =
          match args with
            [Tstr(d); Tlist(x)] -> let v = List.map (function Tstr(s) -> s | _ -> "") x
                                   in Tstr(String.join d v)
            | _ -> raise (Tfun_error "Invalid arguments")

        in let root = Hashtbl.create 10
        in let _ = Hashtbl.add root "header"    (mk_item row_hdr) 
        in let _ = Hashtbl.add root "rows"      (Tlist(List.map mk_item row_data))
        in let _ = Hashtbl.add root "variables" (Tlist(List.map mk_var vars))
        in let _ = Hashtbl.add root "join" (Tfun(join))
        in let _ = List.iter (fun (n,v) -> Hashtbl.add root n (Tstr v)) vars
        in root

end


