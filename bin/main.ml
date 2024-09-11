open Prover.Resolution

(* open Prover.Prop *)
open Prover.Parse
open Yojson.Basic.Util

let parse x = x |> member "proposition" |> prop_of_json
let kb_json = Yojson.Basic.from_file "knowledge.json"
let query_json = Yojson.Basic.from_file "query.json"
let kb = parse kb_json
let query = parse query_json
let _ = resolution kb query true
