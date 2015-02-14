open Printf
open Easy_format
  
let json_string_of_int = sprintf "\x1b[31m%i\x1b[m" 
let json_string_of_string = sprintf "\x1b[32m%s\x1b[m" 
let json_string_of_ident = sprintf "\x1b[33m%s\x1b[m:" 

let std_json_string_of_float = sprintf "\x1b[34m%f\x1b[m:" 
let json_string_of_float = sprintf "\x1b[34m%f\x1b[m:" 

let is_object_or_array x =
  match x with
      `List _
    | `Assoc _ -> true
    | _ -> false

let array = list
let record = list
let tuple = { list with
		space_after_opening = false;
		space_before_closing = false;
		align_closing = false }
let variant = { list with
		  space_before_closing = false; }

let rec format std (x : Yojson.json) =
  match x with
      `Null -> Atom ("null", atom)
    | `Bool x -> Atom ((if x then "true" else "false"), atom)
    | `Int x -> Atom (json_string_of_int x, atom)
    | `Float x ->
	let s =
	  if std then std_json_string_of_float x
	  else json_string_of_float x
	in
	Atom (s, atom)
    | `String s -> Atom (json_string_of_string s, atom)
    | `Intlit s
    | `Floatlit s
    | `Stringlit s -> Atom (s, atom)
    | `List [] -> Atom ("[]", atom)
    | `List l -> List (("[", ",", "]", array), List.map (format std) l)
    | `Assoc [] -> Atom ("{}", atom)
    | `Assoc l -> List (("{", ",", "}", record), List.map (format_field std) l)
    | `Tuple l ->
	if std then
	  format std (`List l)
	else
	  if l = [] then
	    Atom ("()", atom)
	  else
	    List (("(", ",", ")", tuple), List.map (format std) l)

    | `Variant (s, None) ->
	if std then
	  format std (`String s)
	else
	  Atom ("<" ^ json_string_of_string s ^ ">", atom)

    | `Variant (s, Some x) ->
	if std then
	  format std (`List [ `String s; x ])
	else
	  let op = "<" ^ json_string_of_string s ^ ":" in
	  List ((op, "", ">", variant), [format std x])
	    
and format_field std (name, x) =
  (*let s = sprintf "%s:" (json_string_of_string name) in*)
  let s = json_string_of_ident name in
  Label ((Atom (s, atom), label), format std x)


let format ?(std = false) x =
  if std && not (is_object_or_array x) then
    Yojson.json_error
      "Root is not an object or array as requested by the JSON standard"
  else
    format std (x :> Yojson.json)

let to_string ?std x =
  Easy_format.Pretty.to_string (format ?std x)

let to_channel ?std oc x =
  Easy_format.Pretty.to_channel oc (format ?std x)
