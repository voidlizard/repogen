
module P = Printf
module V = Version_gen

let version_string = P.sprintf "Version: %s\nBuilt: %s\nGit commit: %s\n" 
                               (V.repogen_TAG) 
                               (V.repogen_BUILD_TIME)
                               (V.repogen_HASH)

