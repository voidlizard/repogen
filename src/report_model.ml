module Model =
struct

    open ExtList
    open CamlTemplate.Model

    let make row_hdr row_data vars =

        let make_hash items = 
            let hash = Hashtbl.create (List.length items)
            in let () = List.iter (fun (n,v) -> Hashtbl.add hash n (Tstr v)) items
            in (Thash hash)

        in let root = Hashtbl.create 10
        in let _ = Hashtbl.add root "header" (make_hash row_hdr)
        in let _ = Hashtbl.add root "rows"   (Tlist (List.map make_hash row_data))
        in let _ = List.iter (fun (n,v) -> Hashtbl.add root n (Tstr v)) vars
        in root

end


