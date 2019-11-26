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
  make_spaced nt_comments;;

(*let rec nt_sexpr_comment s = 
  let nt_start = star (word_ci "#;") in
  let nt = caten nt_start all_exps in
  let nt = (try nt s
            with X_no_match -> nt_sexpr_comment s) in
  nt;;*)


  
  

let make_seperated_of_comments nt = make_paired (star nt_line_comments) (star nt_line_comments) nt;;

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
  let nt_sign = disj (char_ci '+') (char_ci '-') in
  let nt_digits = plus (range_ci '0' '9') in
  let unsigned_nt_integer = pack nt_digits (fun digits -> int_of_string(list_to_string digits)) in
  let nt_signed_integer = pack (caten (maybe nt_sign) unsigned_nt_integer) (fun (maybe_sign, integer) ->
                                                                  match maybe_sign with
                                                                  | Some('+') -> Number (Int integer)
                                                                  | Some('-') -> Number (Int ((-1) * integer))
                                                                  | None -> Number (Int integer)
                                                                  | _ -> raise X_no_match) in
  let nt_unsigned_float = pack (caten nt_digits (char '.')) (fun (digits, dot) -> List.append digits [dot]) in
  let nt_unsigned_float = pack (caten nt_unsigned_float nt_digits) (fun (digits_and_dot, digits) ->
                                                                      List.append digits_and_dot digits) in
  let nt_unsigned_float = pack nt_unsigned_float (fun digits -> float_of_string(list_to_string digits)) in
  let nt_signed_float = pack (caten (maybe nt_sign) nt_unsigned_float) (fun (maybe_sign, flt) ->
                                                                  match maybe_sign with
                                                                  | Some('+') -> Number (Float flt)
                                                                  | Some('-') -> Number (Float ((float_of_int (-1)) *. flt))
                                                                  | None -> Number (Float flt)
                                                                  | _ -> raise X_no_match) in
  let nt_illegal_postfix = disj_list [(word_ci "!");(word_ci "$");(word_ci "^");
      (word_ci "*");(word_ci "-");(word_ci "_");(word_ci "=");(word_ci "+");(word_ci "<")
      ;(word_ci "<");(word_ci "?");(word_ci "/");(word_ci ":"); (plus (range_ci 'a' 'z')); (plus (range_ci 'A' 'Z'))] in
  make_spaced_and_commented (not_followed_by (disj nt_signed_float nt_signed_integer) nt_illegal_postfix);;

  
let nt_radix = 
  let make_NT_digit ch_from ch_to displacement =
      let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
      let nt = pack nt (let delta = (Char.code ch_from) - displacement in
            fun ch -> (Char.code ch) - delta) in
      nt in
  let nt_digits = (make_NT_digit '0' '9' 0) in
  let nt_chars_digits = plus (range_ci '0' '9') in 
  let nt_integer_digits_and_letters = plus (disj_list [(make_NT_digit 'a' 'z' 10); (make_NT_digit 'A' 'Z' 10); nt_digits]) in
  let nt_float_digits_and_letters = 
    let nt = pack (caten nt_integer_digits_and_letters (char_ci '.')) (fun (digits, dot) -> digits) in
    let nt = caten nt nt_integer_digits_and_letters in
    nt in
  let nt_hash = char_ci '#' in
  let nt_base = 
    let nt = pack nt_chars_digits (fun digits -> int_of_string(list_to_string digits)) in
    let nt = pack (caten nt_hash nt) (fun (hash, base) -> base) in
    let nt = pack (caten nt (char_ci 'r')) (fun (base, r) -> base) in
    nt in
  let nt_sign = maybe (disj (char '+') (char '-')) in
  let nt_radix = 
    let nt_integer_radix = pack (caten nt_base (caten nt_sign nt_integer_digits_and_letters)) (fun (base, (sign, digits_list)) ->
                                let value = (List.fold_left (fun a b -> a * base + b) 0 digits_list) in
                                match sign with
                                | Some('+') -> Number (Int value)
                                | Some('-') -> Number (Int ((-1) * value))
                                | None -> Number (Int value)
                                | _ -> raise X_no_match) in
    let nt_float_radix = pack (caten nt_base (caten nt_sign nt_float_digits_and_letters)) 
                          (fun (base, (sign, (integer_digits, fraction_digits))) ->
                            let integer_part = float_of_int(List.fold_left (fun a b -> a * base + b) 0 integer_digits) in
                            let fraction_part = 
                              (let length = float_of_int(List.length fraction_digits * (-1)) in
                              let fraction = float_of_int(List.fold_left (fun a b -> a * base + b) 0 fraction_digits) in
                              let fraction = fraction *. (float_of_int(base) ** length) in
                              fraction) in
                              let value = integer_part +. fraction_part in
                              match sign with
                              | Some ('+') -> Number (Float value)
                              | Some ('-') -> Number (Float ((float_of_int (-1)) *. value))
                              | None -> Number (Float value)
                              | _ -> raise X_no_match) in
    (disj nt_float_radix nt_integer_radix) in
  make_spaced_and_commented nt_radix;; 



  let rec make_pairs = fun lst ->
    match lst with 
    | e :: s when (List.length lst) > 1 -> Pair(e, (make_pairs s))
    | e :: [] when (List.length lst) = 1 -> Pair(e, Nil)
    | [] -> Nil
    | _ -> raise X_no_match;;
  
  let rec make_pairs_no_nil = fun lst ->
    match lst with 
    | e :: s when (List.length lst) > 1 -> Pair(e, (make_pairs_no_nil s))
    | e :: [] when (List.length lst) = 1 -> e
    | _ -> raise X_no_match;;
  
  
  
  let rec check_double_tagged = fun name lst ->
    let rec check_nested_exp = fun exp ->
                              match exp with
                              | TaggedSexpr(a1, a2) when a1 = name -> raise X_this_should_not_happen
                              | TaggedSexpr(a1, a2) -> TaggedSexpr(a1, (check_nested_exp a2))
                              | Pair(a1, a2) -> Pair((check_nested_exp a1), (check_nested_exp a2))
                              | _ -> exp in 
    match lst with 
    | [] -> []
    | TaggedSexpr(a1, a2) :: s when a1 = name -> raise X_this_should_not_happen 
    | TaggedSexpr(a1, a2) :: s -> TaggedSexpr(a1, (check_nested_exp a2)) :: (check_double_tagged name s)
    | Pair(a1, a2) :: s -> Pair((check_nested_exp a1), (check_nested_exp a2)) :: (check_double_tagged name s)
    | e :: s -> e :: (check_double_tagged name s);;


let nt_nil = 
  let nt_lparen = make_spaced (char (char_of_int 40)) in
  let nt_rparen = make_spaced (char (char_of_int 41)) in
  let nt_nil = pack (caten nt_lparen nt_rparen) (fun (lparen, rparen) -> Nil) in
  let nt_nil = make_seperated_of_comments nt_nil in
  nt_nil;;
  
let make_normal_paranthesized nt = 
  make_paired (make_spaced (char (char_of_int 40))) (make_spaced (char (char_of_int 41))) nt;;



let nt_not_white_space = 
  let a = star(const (fun ch -> ' ' < ch)) in
  make_paired (word_ci "#{") (char_ci '}') a;;



let nt_lparen = make_spaced_and_commented (char (char_of_int 40));;
let nt_rparen = make_spaced_and_commented (char (char_of_int 41));; 
let nt_dot = make_spaced_and_commented (char '.');;

  
let rec all_exps exp = disj_list [boolean_parser; char_parser; nt_number; symbol_parser; nt_empty_list; nt_not_dotted_list;
nt_dotted_list; nt_quoted; nt_quasi_quote; nt_unquoted; nt_unquoted_spliced; nt_tagged_exp; nt_sexp_comments] exp

    
    and nt_empty_list exp = 
      let no_parens = pack (nt_whitespaces) (fun exp -> Nil) in
      let with_parens = make_normal_paranthesized no_parens in
      (make_normal_paranthesized (disj no_parens with_parens)) exp;
    
    and nt_not_dotted_list exp = 
      let no_parens = pack (plus all_exps) (fun exps ->
                                            List.fold_left (fun a b -> Pair(b, a)) Nil (List.rev exps)) in
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
      (make_spaced_and_commented nt_quoted) exp;
    

    and nt_tagged_exp exp =
      let nt_not_white_space = star(const (fun ch -> (' ' < ch) && (ch != '}'))) in
      let nt_tag_ref_exp = 
        let nt_tag_ref_exp = make_paired (word_ci "#{") (char '}') nt_not_white_space in
        let nt_tag_ref_exp = pack nt_tag_ref_exp (fun ref_name_as_list -> TagRef(list_to_string ref_name_as_list)) in
        nt_tag_ref_exp in
      let nt_proper_list = make_normal_paranthesized (star (disj all_exps nt_tag_ref_exp)) in 
      let nt_improper_list = 
        let nt_dot = char '.' in
        let nt = pack (caten (disj all_exps nt_tag_ref_exp) nt_dot) (fun (exp, dot) -> exp) in
        let nt = pack (caten nt (disj all_exps nt_tag_ref_exp)) (fun (exp1, exp2) -> [exp1; exp2]) in
        make_normal_paranthesized nt in
      let nt_all_not_paranthesized = pack (disj all_exps nt_tag_ref_exp) (fun (exp) -> [exp]) in
      let nt_tagged_exp = make_paired (word_ci "#{") (word_ci "}") nt_not_white_space in
      let nt_tagged_exp = pack (caten nt_tagged_exp (char (char_of_int 61))) (fun (e, eq) -> e) in
      let nt_tagged_exp = 
        let nt_1 = pack (caten nt_tagged_exp nt_proper_list) 
                                    (fun (ref_name_as_list, rest_of_exp) ->
                                      let name = list_to_string ref_name_as_list in
                                      let legal_list = check_double_tagged name rest_of_exp in
                                      let list_of_pairs = make_pairs legal_list in
                                      TaggedSexpr(name, list_of_pairs)) in
        let nt_2 = pack (caten nt_tagged_exp (disj_list [nt_all_not_paranthesized; nt_improper_list])) 
                                    (fun (ref_name_as_list, rest_of_exp) ->
                                      let name = list_to_string ref_name_as_list in
                                      let legal_list = check_double_tagged name rest_of_exp in
                                      let list_of_pairs = make_pairs_no_nil legal_list in
                                      TaggedSexpr(name, list_of_pairs)) in
        disj nt_1 nt_2 in
      (make_spaced_and_commented (disj nt_tagged_exp nt_tag_ref_exp)) exp;
    
    and nt_sexp_comments exp = 
      let nt_start = word_ci "#;" in
      let nt = pack (caten nt_start all_exps) (fun (hash, exp) -> exp) in
      let nt = pack (caten nt all_exps) (fun (exp1, exp2) -> exp2) in
      (make_spaced_and_commented nt) exp;;
  
   




      
    
    

    
    

(* string MAOR *)
(* special nots *)


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