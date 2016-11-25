open Ocamlbuild_plugin

module Atdgen = struct
  let cmd = "atdgen" (* TODO detect from ./configure phase *)
  let run_atdgen dst tagger env _ =
    let tags = tagger (tags_of_pathname (env dst) ++"atdgen") in
    let dir = Filename.dirname (env dst) in
    let fname = Filename.basename (env "%.atd") in
     Cmd (S [A "cd"; Px dir; Sh "&&"; A cmd; T tags; Px fname])

  let rules () =
    rule "%.atd -> %_j.ml{i}" ~prods:["%_j.ml";"%_j.mli"] ~dep:"%.atd"
     (run_atdgen "%_j.ml" (fun tags -> tags++"generate"++"json"));
    rule "%.atd -> %_t.ml{i}" ~prods:["%_t.ml";"%_t.mli"] ~dep:"%.atd"
     (run_atdgen "%_t.ml" (fun tags -> tags++"generate"++"typedef"));
    rule "%.atd -> %_b.ml{i}" ~prods:["%_b.ml";"%_b.mli"] ~dep:"%.atd"
     (run_atdgen "%_b.ml" (fun tags -> tags++"generate"++"biniou"));
    rule "%.atd -> %_v.ml{i}" ~prods:["%_v.ml";"%_v.mli"] ~dep:"%.atd"
     (run_atdgen "%_v.ml" (fun tags -> tags++"generate"++"validator"));
    flag ["atdgen"; "generate"; "json"] & S[A"-j"];
    flag ["atdgen"; "generate"; "std_json"] & S[A"-j-std"];
    flag ["atdgen"; "generate"; "typedef"] & S[A"-t"];
    flag ["atdgen"; "generate"; "biniou"] & S[A"-b"];
    flag ["atdgen"; "generate"; "validator"] & S[A"-v"];
end

let () = dispatch @@ function After_rules -> Atdgen.rules () | _ -> ()
