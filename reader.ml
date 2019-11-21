#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type number =
  | Int of int
  | Float of float;;


type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;
(* MAOR *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let nt_whitespace = const (fun ch -> ch <= ' ');;

let nt_whitespaces= star(char ' ' );;
let nt_comments = star(char ';');;

let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;
let boolean_parser = 
  let nt = disj (word_ci "#f") (word_ci "#t")  in
  let last_nt  = pack nt (fun (prefix) -> match((list_to_string prefix)) with
      |  "#f" -> Bool(false)
      |  "#F" -> Bool(false)
      | "#t" -> Bool(true)
      | "#T" -> Bool(true)
      | _ -> raise X_no_match) in 
  let last_nt = make_spaced last_nt in
  last_nt;;


(* MAOR *)
let symbol_parser = 
let symbol_list_nt = disj_list [(word_ci "!");(word_ci "$");(word_ci "^");
  (word_ci "*");(word_ci "-");(word_ci "_");(word_ci "=");(word_ci "+");(word_ci "<")
  ;(word_ci "<");(word_ci "?");(word_ci "/");(word_ci ":")] in
let a_z_nt_capital = plus(range_ci 'A' 'Z') in
let a_z_nt = plus(range_ci 'a' 'z') in
let a_z_nt = pack (disj a_z_nt_capital a_z_nt) (List.map lowercase_ascii)in
let numbers_range_nt = plus(range_ci '0' '9') in
let symbol_disj_nt = plus (disj_list [symbol_list_nt;a_z_nt;numbers_range_nt]) in
let symbol_disj_nt = pack symbol_disj_nt (fun (list_of_lists) ->
                                          List.flatten list_of_lists) in
let sym_parse = pack symbol_disj_nt (fun (prefix) ->  (Symbol(list_to_string prefix))) in sym_parse;;


(* MAOR *)
let char_parser = 
(* parser for chars which are greater than space in ASCII *)
let nt_visible_simple_char = const (fun ch -> ch > ' ')in
(* parser for all of the named chars. *)
let list_nt = disj_list [(word_ci "nul");(word_ci "newline") ;(word_ci "return");(word_ci "tab");
                         (word_ci "page");(word_ci "space")] in
(* beacuse the nt_visible_simple_char is just a char and we need char list this func' solves it *)
let temp_nt = pack (caten nt_visible_simple_char nt_epsilon) (fun (e,s) -> e::s)in
(* thats out char parser *)
let char_nt = caten (word_ci "#\\") (disj list_nt temp_nt) in
let char_nt2 = pack char_nt (fun (prefix,rest) -> match ((list_to_string prefix), (list_to_string rest)) with
    | ("#\\", "tab") -> Char '\t'
    | ("#\\", "newline") -> Char '\n'
    | ("#\\", "space") -> Char ' '
    | ("#\\", "return") -> Char '\r'
    | ("#\\", "page") -> Char '\012'
    | ("#\\", "nul") -> Char '\000'
    | ("#\\", c) -> Char c.[0]
    | (_, _) -> raise X_no_match) in char_nt2;;




(* string MAOR *)
(* special nots *)

let meta_nt = disj_list [(word_ci "\\r");(word_ci "\\n");(word_ci "\\t");(word_ci "\\f");
(word_ci "\\\\");(word_ci "\"")]
let string_Literal_Char_nt = const (fun ch -> ch != '\"' && ch != '\\');;
let sting_temp_nt = pack (caten string_Literal_Char_nt nt_epsilon) (fun (e,s) -> e::s)
let stringChar_nt = star(disj meta_nt sting_temp_nt);;
let star_stringChar_nt = pack stringChar_nt (fun (list_of_lists) ->
                                          List.flatten list_of_lists);;
let string_parser = pack star_stringChar_nt (fun nadav -> String(list_to_string nadav)) 







module Reader: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end
= struct
  let normalize_scheme_symbol str =
    let s = string_to_list str in
    if (andmap
          (fun ch -> (ch = (lowercase_ascii ch)))
          s) then str
    else Printf.sprintf "|%s|" str;;


  let read_sexpr string = raise X_not_yet_implemented ;;

  let read_sexprs string = raise X_not_yet_implemented;;




end;; (* struct Reader *)