open Yojson.Basic.Util
open Prop

let rec prop_of_json (json : Yojson.Basic.t) : prop =
  let t = json |> member "type" |> to_string in
  let attribute x = json |> member "attributes" |> member x in
  match t with
  | "atom" -> Atom (attribute "value" |> to_string)
  | "not" -> Not (attribute "prop" |> prop_of_json)
  | dub -> (
      let left = attribute "left_prop" |> prop_of_json in
      let right = attribute "right_prop" |> prop_of_json in
      match dub with
      | "or" -> Or (left, right)
      | "and" -> And (left, right)
      | "imp" -> Imp (left, right)
      | "iff" -> Iff (left, right)
      | x -> failwith ("unknown joiner: " ^ x))

(* let kb_json = Yojson.Basic.from_file "kb.json" let propositions = kb_json |> member
   "proposition" |> to_list |> List.map prop_of_json let _ = List.iter (fun x ->
   print_endline (string_of_prop x)) propositions *)
