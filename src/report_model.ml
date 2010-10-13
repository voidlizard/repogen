module Model =
struct

    open ExtList
    open ExtHashtbl
    open Util
    open CamlTemplate.Model

    let make row_hdr row_data vars =

        let make_hash items = 
            let hash = Hashtbl.create (List.length items)
            in let () = List.iter (fun (n,v) -> Hashtbl.add hash n (Tstr v)) items
            in (Thash hash)

        (* FIXME: fix column order *)
        in let column_list ~args =
         match args with
             | [ Thash h ] -> (Tlist (List.of_enum (Hashtbl.values h)))
             | _ -> raise (Tfun_error "Invalid argument")

        in let root = Hashtbl.create 10
        in let _ = Hashtbl.add root "header" (make_hash row_hdr)
        in let _ = Hashtbl.add root "columns" (Tlist (List.map  (fun (a,b) -> make_hash [("name", a);("display_name",b)]) row_hdr))
        in let _ = Hashtbl.add root "rows"   (Tlist (List.map make_hash row_data))
        in let _ = Hashtbl.add root "row_columns" (Tfun column_list)
        in let _ = List.iter (fun (n,v) -> Hashtbl.add root n (Tstr v)) vars
        in root

end


