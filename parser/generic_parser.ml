open Merlin_kernel
open Ocaml_preprocess
open Ocaml_utils

let parse ty str =
  let str = Msource.make str in
  let state = Warnings.backup () in
  let keywords = Lexer_raw.keywords [] in 
  let lexer = Mreader_lexer.make state keywords Mconfig.initial str in
  let parser = Mreader_parser.make state lexer ty in
  Mreader_parser.result parser

let implementation buf =
  match parse Mreader_parser.ML buf with
  | `Implementation impl -> impl
  | `Interface _ -> []

let interface buf =
  match parse Mreader_parser.MLI buf with
  | `Implementation _ -> []
  | `Interface intf -> intf

let expression buf =
  match parse Mreader_parser.ML buf with
  | `Implementation [{pstr_desc=Pstr_eval (exp, _);_}] -> exp
  | _ -> failwith "invalid expr"
