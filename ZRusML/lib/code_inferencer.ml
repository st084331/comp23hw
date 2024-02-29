(** Copyright 2023-2024, Rustam Shangareev and Danil Yevdokimov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Inferencer
open Typing

let rec find x = function
  | [] -> 0
  | h :: t -> if x = h then 1 else 1 + find x t
;;

let env_inference prog env =
  let typs, _ =
    List.fold_left
      (fun (typs, environment) dec ->
        let tcheck dec =
          match dec with
          | DLet (_, PtWild, _) -> "- ", run_inference dec environment
          | DLet (_, PtVar name, _) -> name, run_inference dec environment
          | _ -> "", Error `Matching_failed
        in
        let environment' =
          match snd (tcheck dec) with
          | Ok (env, _) -> env
          | _ -> environment
        in
        tcheck dec :: typs, environment')
      ([], env)
      prog
  in
  List.rev typs
;;

let inference_prog prog = env_inference prog TypeEnv.empty

let env_show_inference prog env =
  let id_x_typs = env_inference prog env in
  let only_typs = List.map snd id_x_typs in
  let error_check =
    List.find_opt
      (function
        | Error _ -> true
        | _ -> false)
      only_typs
  in
  match error_check with
  | None ->
    Ok
      (List.fold_left
         (fun acc (a, b) ->
           match b with
           | Ok (_, typ) -> acc ^ Format.sprintf "val %s : %s\n" a (show_typ typ)
           | Error e -> acc ^ show_error e)
         ""
         id_x_typs)
  | Some (Error e) ->
    let index = find (Error e) only_typs in
    Error (Format.sprintf "Error in №%d declaration: \n%s\n" index (show_error e))
  | _ -> Error (show_error `Unreachable)
;;

let show_inference code =
  match Parser.parse Parser.prog code with
  | Ok prog ->
    (match env_show_inference prog TypeEnv.empty with
     | Ok res | Error res -> res)
  | _ -> "Parse error"
;;

let inference fmt code = Format.fprintf fmt "%s" (show_inference code)
