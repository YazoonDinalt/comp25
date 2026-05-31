(** Copyright 2025-2026, Vitaliy Dyachkov, Ruslan Nafikov, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open DopsaML_lib

type opts =
  { mutable input_file : string option
  ; mutable output_file : string option
  }

let opts = { input_file = None; output_file = None }

let speclist =
  [ ( "-o"
    , Arg.String (fun f -> opts.output_file <- Some f)
    , "Output LLVM IR file (default: out.ll)" )
  ]
;;

let anon_fun anon =
  match opts.input_file with
  | None -> opts.input_file <- Some anon
  | Some _ ->
    Printf.eprintf "Unexpected argument: %s\n" anon;
    exit 1
;;

let usage_msg = "Usage: DopsaML [options] <file.ml>"

let () =
  Arg.parse speclist anon_fun usage_msg;
  let input_file =
    match opts.input_file with
    | Some f -> f
    | None ->
      Arg.usage speclist usage_msg;
      exit 1
  in
  let output_file = Option.value opts.output_file ~default:"out.ll" in
  let source = In_channel.with_open_text input_file In_channel.input_all in
  match Parser.parser source with
  | Error msg ->
    Printf.eprintf "Parse error: %s\n" msg;
    exit 1
  | Ok stmts ->
    (match Codegen.codegen_program (Anf.anf_program stmts) output_file with
     | Error msg ->
       Printf.eprintf "Codegen error: %s\n" msg;
       exit 1
     | Ok () -> ())
;;
