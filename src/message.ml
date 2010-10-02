(** Error messages. *)

open Lexing
open Util

(** [lexer_from_channel fn ch] returns a lexer stream which takes
    input from channel [ch]. The filename (for reporting errors) is
    set to [fn].
*)
let lexer_from_channel fn ch =
  let lex = Lexing.from_channel ch in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = fn; pos_lnum = 1; } ;
    lex

(** [lexer_from_string str] returns a lexer stream which takes input
    from a string [str]. The filename (for reporting errors) is set to
    [""]. *)
let lexer_from_string str ?fn:(fname=None) =
  let lex = Lexing.from_string str in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = some_def "" fname; pos_lnum = 1; } ;
    lex

