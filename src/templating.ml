module Templating = 
struct
    open CamlTemplate.Model

    let cache () = CamlTemplate.Cache.create ()
    
    let from_file cache fname = 
        let tmpl = CamlTemplate.Cache.get_template cache fname
        in tmpl

    let from_string cache s = assert false

    let render_string ?bufsize:(bs=1024) tmpl model =
        let buf = Buffer.create bs
        in let () = CamlTemplate.merge tmpl model buf
        in (Buffer.contents buf)

end



