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

(* MAOR *)



let nt_whitespaces= star(const (fun ch -> ch <= ' '));;

let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;

let nt_line_comments = 
  let nt_semicolon = char ';' in
  let nt_rest_of_comment = star (const (fun ch -> (ch != (char_of_int 4)) && (ch != (char_of_int 10)))) in
  let nt_comments = pack (caten nt_semicolon nt_rest_of_comment) (fun (e1, e2) -> e1 :: e2) in
  make_spaced (star nt_comments);;

let make_seperated_of_comments nt = make_paired nt_line_comments nt_line_comments nt;;

let make_spaced_and_commented nt = make_seperated_of_comments (make_spaced nt);;





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
      | (_, _) -> raise X_no_match) in 
  make_spaced_and_commented char_nt2;;


let boolean_parser = 
  let nt = disj (word_ci "#f") (word_ci "#t")  in
  let last_nt  = pack nt (fun (prefix) -> match((list_to_string prefix)) with
      |  "#f" -> Bool(false)
      |  "#F" -> Bool(false)
      | "#t" -> Bool(true)
      | "#T" -> Bool(true)
      | _ -> raise X_no_match) in 
  let last_nt = make_spaced last_nt in
  make_spaced_and_commented last_nt;;

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
  let sym_parse = pack symbol_disj_nt (fun (prefix) ->  (Symbol(list_to_string prefix))) in 
  make_spaced_and_commented sym_parse;;



(* STRING *)
let nt_string = 
  let string_Literal_Char_nt = const (fun ch -> ch != '\"' && ch != '\\') in
  let backslash = char '\\' in
  let meta_list = disj_list [(char 'r');(char 'n');(char 't');(char 'f');
                             (char '\\');(char '\"')] in
  let meta_chars = caten backslash meta_list in
  let convert_to_char_nt = pack meta_chars (fun s-> match (s) with
      | (('\\','r')) -> '\r'
      | (('\\','n')) -> '\n'
      | (('\\','t')) -> '\t'
      | (('\\','f')) -> char_of_int 12
      | (('\\','\"')) -> '\"'
      | (('\\','\\')) -> '\\'
      |_ ->raise X_no_match ) in
  let string_char = disj string_Literal_Char_nt convert_to_char_nt in
  let star_string_char = star string_char in
  let string_tok = pack (make_paired (char '\"') (char '\"') star_string_char) (fun s -> 
      String(list_to_string s)) in
  make_spaced_and_commented string_tok;;


let nt_number = 
  let make_nt_digits ch_from ch_to displacement =
    let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) - displacement in
                      fun ch -> (Char.code ch) - delta) in
    nt in
  let nt_sign = disj (char '+') (char '-') in
  let nt_sign = pack (caten nt_sign nt_epsilon) (fun (e, s) -> e::s) in
  let nt_digits = plus (make_nt_digits '0' '9' 0) in
  let nt_unsigned_integer = pack nt_digits (fun digits ->
      List.fold_left (fun a b -> 10 * a + b) 0 digits) in
  let nt_integer = 
    let nt_unsigned_sexp_integer = pack nt_unsigned_integer (fun integer -> Number(Int integer)) in
    let nt_signed_sexp_integer = pack (caten nt_sign nt_unsigned_integer) (fun (sign, integer) ->
        match sign with
        | ['+'] -> Number(Int integer)
        | ['-'] -> Number(Int((-1) * integer))
        | _ -> raise X_no_match) in
    let nt_integer = disj nt_unsigned_sexp_integer nt_signed_sexp_integer in
    nt_integer in
  let nt_float = 
    let nt_dot = char '.' in
    let nt_fraction = caten nt_dot nt_digits in
    let nt_fraction = pack nt_fraction (fun (dot, digits) -> (digits, (List.length digits))) in
    let nt_fraction = pack nt_fraction (fun (digits, length) ->
        let frac = List.fold_left (fun a b -> 10 * a + b) 0 digits in
        let mult = 10. ** ((float_of_int length) *. (-1.)) in
        let frac = (float_of_int frac) *. mult in
        frac) in
    let nt_unsigned_float = pack (caten nt_unsigned_integer nt_fraction) (fun (integer, fraction) ->
        (float_of_int integer) +. fraction) in
    let nt_unsigned_float_sexp = pack nt_unsigned_float (fun number -> Number(Float number)) in
    let nt_signed_float_sexp = caten nt_sign nt_unsigned_float in
    let nt_signed_float_sexp = pack nt_signed_float_sexp (fun (sign, num) ->
        match sign with
        | ['+'] -> Number(Float num)
        | ['-'] -> Number(Float((float_of_int (-1)) *. num))
        | _ -> raise X_no_match) in
    let nt_float = disj nt_unsigned_float_sexp nt_signed_float_sexp in
    nt_float in
  let nt_number = disj nt_float nt_integer in
  make_spaced_and_commented nt_number;;

let nt_nil = 
  let nt_lparen = make_spaced (char (char_of_int 40)) in
  let nt_rparen = make_spaced (char (char_of_int 41)) in
  let nt_nil = pack (caten nt_lparen nt_rparen) (fun (lparen, rparen) -> Nil) in
  let nt_nil = make_seperated_of_comments nt_nil in
  nt_nil;;


(* OKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK *)
let x = 10.**3.;;
(* TODO add ability to get floats *)
let make_nt_digits ch_from ch_to displacement =
  let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
  let nt = pack nt (let delta = (Char.code ch_from) - displacement in
                    fun ch -> (Char.code ch) - delta) in
  nt ;;
let nt_sign = disj (char '+') (char '-') ;;
let nt_sign = pack (caten nt_sign nt_epsilon) (fun (e, s) -> e::s) ;;
let nt_digits = plus (make_nt_digits '0' '9' 0);;
let nt_unsigned_integer = pack nt_digits (fun digits ->
    List.fold_left (fun a b -> 10 * a + b) 0 digits);;
let nt_unsigned_float_from_int = pack nt_unsigned_integer (fun x-> (float_of_int x));; 
let make_float_with_sign = pack (caten nt_sign nt_unsigned_float_from_int) (fun (sign, float) ->
    match sign with
    | ['+'] -> (float)
    | ['-'] -> ((float_of_int (-1)) *. (float ))
    | _ -> raise X_no_match) ;;

let make_float = disj nt_unsigned_float_from_int make_float_with_sign;;
let nt_e = (word_ci "e");;
let scinetific_form = caten make_float (caten nt_e make_float);;
let num_nt = pack scinetific_form (fun (f,(e,n)) -> ((f*.( 10.** n))));;
let make_it_Number = pack num_nt (fun num -> Number (Float num));;
(* OKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK *)

let nt_dot = char '.' ;;
let nt_fraction = caten nt_dot nt_digits ;;
let nt_fraction = pack nt_fraction (fun (dot, digits) -> (digits, (List.length digits))) ;;
let nt_fraction = pack nt_fraction (fun (digits, length) ->
    let frac = List.fold_left (fun a b -> 10 * a + b) 0 digits in
    let mult = 10. ** ((float_of_int length) *. (-1.)) in
    let frac = (float_of_int frac) *. mult in
    frac) ;;
let nt_unsigned_float = pack (caten nt_unsigned_integer nt_fraction) (fun (integer, fraction) ->
    (float_of_int integer) +. fraction) ;;
let nt_signed_float_sexp = caten nt_sign nt_unsigned_float ;;
let nt_signed_float_sexp = pack nt_signed_float_sexp (fun (sign, num) ->
    match sign with
    | ['+'] -> num
    | ['-'] -> (float_of_int (-1)) *. num
    | _ -> raise X_no_match) ;;
let nt_float = disj_list [nt_unsigned_float ;nt_signed_float_sexp;make_float] ;;
let scinetific_form_float = caten nt_float (caten nt_e nt_float);;
let num_nt_float = pack scinetific_form (fun (f,(e,n)) -> ((f*.( 10.** n))));;



let make_normal_paranthesized nt = 
  make_paired (make_spaced (char (char_of_int 40))) (make_spaced (char (char_of_int 41))) nt;;

let do_nothing () = ();;

let reverse_list lst =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] lst;;

let nt_lparen = make_spaced_and_commented (char (char_of_int 40));;
let nt_rparen = make_spaced_and_commented (char (char_of_int 41));; 
let nt_dot = make_spaced_and_commented (char '.');;


let rec all_exps exp = disj_list [boolean_parser; char_parser; nt_number; symbol_parser; nt_empty_list; nt_not_dotted_list;
                                  nt_dotted_list; nt_quoted; nt_quasi_quote; nt_unquoted; nt_unquoted_spliced] exp


and nt_empty_list exp = 
  let no_parens = pack (nt_whitespaces) (fun exp -> Nil) in
  let with_parens = make_normal_paranthesized no_parens in
  (make_normal_paranthesized (disj no_parens with_parens)) exp;

and nt_not_dotted_list exp = 
  let no_parens = pack (plus all_exps) (fun exps ->
      List.fold_left (fun a b -> Pair(b, a)) Nil (reverse_list exps)) in
  let with_parens = make_normal_paranthesized no_parens in
  (make_normal_paranthesized (disj no_parens with_parens)) exp;

and nt_dotted_list exp = 
  let nt_spaced_exp = make_spaced_and_commented all_exps in
  let nt_car_and_dot = pack (caten nt_spaced_exp nt_dot) (fun (car, dot) -> car) in
  let no_parens = pack (caten nt_car_and_dot nt_spaced_exp) (fun (car, cdr) -> Pair(car, cdr)) in
  let with_parens = make_normal_paranthesized no_parens in
  (make_normal_paranthesized (disj no_parens with_parens)) exp;

and nt_quoted exp = 
  let nt_quote = char (char_of_int 39) in
  let nt_quoted = pack (caten nt_quote all_exps) (fun (quote, exp) -> Pair(Symbol("quote"), Pair(exp, Nil))) in
  (make_spaced_and_commented nt_quoted) exp;

and nt_quasi_quote exp = 
  let nt_quote = char (char_of_int 96) in
  let nt_quoted = pack (caten nt_quote all_exps) (fun (quote, exp) -> Pair(Symbol("quasiquote"), Pair(exp, Nil))) in
  (make_spaced_and_commented nt_quoted) exp;

and nt_unquoted_spliced exp =
  let nt_quote = word_ci ",@" in
  let nt_quoted = pack (caten nt_quote all_exps) (fun (quote, exp) -> Pair(Symbol("unquote-splicing"), Pair(exp, Nil))) in
  (make_spaced_and_commented nt_quoted) exp;

and nt_unquoted exp = 
  let nt_quote = char ',' in
  let nt_quoted = pack (caten nt_quote all_exps) (fun (quote, exp) -> Pair(Symbol("unquote"), Pair(exp, Nil))) in
  (make_spaced_and_commented nt_quoted) exp;;



(* let rec nt_sexp_comment s = 
   let ntx = caten (word_ci "#;") all_exps in
   let nty = pack ntx (fun _ -> ()) in

   nty s;; *)





(* string MAOR *)
(* special nots *)

let tok_lparen =
  let lp = char '(' in
  let spaced= caten(caten nt_whitespaces lp) nt_whitespaces in
  pack spaced (fun ((l, p), r) -> p);;

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