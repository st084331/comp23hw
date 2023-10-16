#use "topfind";;
#require "curly";;
#require "yojson";;
#require "str";;

type config = {mutable file: string; mutable token: string; mutable issue: int }
let config =
  let c = { file=""; token=""; issue = 0 } in
  Arg.parse
    [ ("-file", Arg.String(fun s -> c.file <- s ), " input file ")
    ; ("-token", Arg.String(fun s -> c.token <- s ), " access token")
    ; ("-issue", Arg.Int (fun s -> c.issue <- s ), " issue/PR number")
    ]
    (fun _ -> assert false)
    "";
  c

let () =
  let url =
    let owner = "Kakadu" in
    let repo = "comp23hw" in
      Printf.sprintf
        "https://api.github.com/repos/%s/%s/issues/%d/comments"
        owner
        repo
        config.issue
    in
    let headers =
      [ "Authorization", Printf.sprintf "Bearer %s" config.token
      ; "Accept", "application/vnd.github+json"
      ; "X-GitHub-Api-Version", "2022-11-28"
      ]
    in
    let data = In_channel.with_open_text config.file (fun ch ->
      let s = In_channel.input_all ch in
      s |>
      (* Str.global_replace (Str.regexp "\n") "\\n" |> *)
      Str.global_replace (Str.regexp "'") "'\\''"
      )
    in
    let body =
      `Assoc [ "body", `String data ]
      |> Yojson.Safe.pretty_to_string
    in
    (match Curly.(run (Request.make ~body ~headers ~url ~meth:`POST ())) with
     | Ok x ->
       Format.printf "status: %d\n" x.Curly.Response.code;
       Format.printf "body: %s\n" x.Curly.Response.body
     | Error e -> Format.eprintf "Failed: %a" Curly.Error.pp e)
