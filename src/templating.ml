module Templating = 
struct
    open CamlTemplate.Model
    module CC = CamlTemplate.Cache

    let cache () = CC.create ~loader:(CC.make_file_loader ~template_dir:"") ()
    
    let from_file cache fname =
        let tmpl = CC.get_template cache fname 
        in tmpl

    let from_string cache s = assert false

    let render_string ?bufsize:(bs=1024) tmpl model =
        let buf = Buffer.create bs
        in let () = CamlTemplate.merge tmpl model buf
        in (Buffer.contents buf)

end



