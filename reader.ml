
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

let nt_boolean = 
  let nt = disj (word_ci "#f") (word_ci "#t") in
  let last_nt s = (match (nt s) with
      | (['#'; 'f'], _) -> Bool(false)
      | (['#'; 't'], _) -> Bool(true)
      | (['#'; 'T'], _) -> Bool(true)
      | (['#'; 'F'], _) -> Bool(false)
      | _ -> raise X_no_match) in 
  last_nt;;

let visible_simple_char_nt = 
  fun ch ->
  if ((int_of_char ch)>32)
  then Char(ch) 
  else raise X_no_match;;

let named_char_nt =
  let nt = disj_list [(word_ci "nul");(word_ci "newline") ;(word_ci "return");(word_ci "tab");
  (word_ci "page");(word_ci "space")] in
  let last_nt s = (match (nt s) with
      | (['n'; 'u';'l'], _) -> Char(char_of_int 0)
      | (['n'; 'e';'w';'l';'i';'n';'e'], _) -> Char(char_of_int 10)
      | (['r'; 'e';'t';'u';'r';'n'], _) -> Char(char_of_int 13)
      | (['t'; 'a';'b'], _) -> Char(char_of_int 9)
      | (['p';'a';'g';'e'], _) -> Char(char_of_int 12)
      | (['s';'p';'a';'c';'e'], _) -> Char(char_of_int 32)
      | _ -> raise X_no_match) in 
  last_nt;;


let charPrefixnt = 
  let nt_cp = (word_ci "#\\") in
  let comb_nt = disj named_char_nt nt_whitespace
  let last_nt s = (match (nt_cp s) with
      |(['#';'\\'],[c]) -> (comb_nt c)
      | _ -> raise X_no_match) in 
  last_nt;;



let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let nt_whitespace = const (fun ch -> ch <= ' ');;

let nt_char = 
  let nt_c = disj (char 'A') (char 'B') in
  let nt s = (match (nt_c s) with
      |('A',_)-> char('A')
      | _ -> raise X_no_match)in nt;; 

let nt_whitespaces= star(char ' ' );;

let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;

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
