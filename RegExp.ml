(* RegExp module body *)

(* Aluno 1: José Duarte ALmeida nº50838
Aluno 2: Tomás Rocha Pereira nº50285

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
01234567890123456789012345678901234567890123456789012345678901234567890123456789
   80 columns
*)


(* REGULAR EXPRESSIONS *)

type regExp =
      Str of string
    | Class of string
    | NClass of string
    | Any
    | Seq of regExp * regExp
    | Or of regExp * regExp
    | Not of regExp
    | Zero of regExp
    | ZeroOrOne of regExp
    | ZeroOrMore of regExp
    | OneOrMore of regExp
    | Repeat of int * int * regExp
;;


(* STRINGS *)

let cut s =
    (String.get s 0, String.sub s 1 ((String.length s)-1))
;;

let join x xs =
    (Char.escaped x)^xs
;;

let rec list_of_string s =
    if s = "" then []
    else
        let (x,xs) = cut s in
            x::list_of_string xs
;;

let rec string_of_list l =
    match l with
       [] -> ""
     | x::xs -> join x (string_of_list xs)
;;


(* matchAtStart *)

let rec matchAtStartStr line str_in_list =
	match line, str_in_list with
		[],_ -> (false, [], [])
		| _,[] -> (true, [], line)
		| x::xs,y::ys -> let(b, m, r) = matchAtStartStr xs ys in
					if x=y && b then (b, y::m, r) 
					else (false, [], [])
;;

let rec str_contain_elem str_in_list elem =
		match str_in_list with
		| [] -> false
		| x::xs -> if x = elem then true 
				else str_contain_elem xs elem
;;


let rec matchAtStartRE line re =
    match re with
      	Zero p -> (true, [], line)
      
      | Any -> (true, [List.hd line], List.tl line)
      | Str str -> matchAtStartStr line (list_of_string str)
			| Class str -> (match line with
													[] -> (false, [], [])
													|x::xs -> if str_contain_elem (list_of_string str) x 
																		then (true, [x], xs)
																		else (false, [], []))
			| NClass str -> (match line with
													[] -> (false, [], [])
													|x::xs -> if str_contain_elem (list_of_string str) x 
																		then (false, [], [])
																		else (true, [x], xs))
			| Seq (p,q) -> let (b1, m1, r1) = matchAtStartRE line p in
													if b1 then let (b2, m2, r2) = matchAtStartRE r1 q in
														if b2 then (b2, m1@m2, r2)
														else (false,[],[])
													else (false,[],[])
			| Or (p,q) -> let (b1, m1, r1) = matchAtStartRE line p in
													let (b2, m2, r2) = matchAtStartRE line q in
														if b1 && b2 then
															if List.length m1 >= List.length m2 
															then (b1, m1, r1)
															else (b2, m2, r2)
														else if b1 then (b1, m1, r1)
														else if b2 then (b2, m2, r1)
														else (false,[],[])
			| ZeroOrOne p -> let (b, m, r) = matchAtStartRE line p in 
													if b then (b, m, r) 
													else (true,[],line)
			| ZeroOrMore p -> let (b, m, r) = matchAtStartRE line p in
													if b then matchAtStartOM line p
													else (true,[],line)
			| Not p -> let (b, m, r) = matchAtStartRE line p in
													if b then (false, [], [])
													else (true, [], line)
			| OneOrMore p -> matchAtStartOM line p
			| Repeat (k, n, p) ->  matchAtStartRep k n line p


and matchAtStartRep k n line p = 
	if k > n then (false, [], [])
	else
  	let (b1, m1, r1) = matchAtStartRE line p in 
  		if k <> 0 then 
				if k >= 1 then
					if b1 then
  					let (b2, m2, r2) = matchAtStartRep (k-1) (n-1) r1 p in
  						if b2 then (b2, m1@m2, r2)
  						else (false, [], [])
					else (false, [], [])
				else
					if b1 then
						if n > 1 then
							let (b2, m2, r2) = matchAtStartRep (k-1) (n-1) r1 p in
								if b2 then (b2, m1@m2, r2)
								else (b1, m1, r1)
						else (b1, m1, r1)
					else (true, m1, line)
			else 
				if b1 && n > 1 then
					let (b2, m2, r2) = matchAtStartRep (k-1) (n-1) r1 p in
						if b2 then (b2, m1@m2, r2) 
						else (b1, m1, r1)
				else (true, [], line)


and matchAtStartOM line p =
	let (b1, m1, r1) = matchAtStartRE line p in
		if b1 then let (b2, m2, r2) = matchAtStartOM r1 p in
			if b2 then (b2, m1@m2, r2)
			else (b1, m1, r1)
		else (false, [], [])
		
	
;;


let matchAtStart line re =
    let (b,m,r) = matchAtStartRE (list_of_string line) re in
        (b, string_of_list m, string_of_list r)
;;


(* firstMatch *)

let rec firstMatchRE line re =
    match line with
		| [] -> (false, [], [], [])
		| x::xs -> let (b, m, r) = matchAtStartRE line re in
								if b then (b, [], m, r) 
								else let (b2, a, m2, r2) = firstMatchRE xs re in
											if b2 then (b2, [x]@a, m@m2, r2)
											else firstMatchRE r2 re
;;

let firstMatch line re =
    let (b,p,m,r) = firstMatchRE (list_of_string line) re in
        (b, string_of_list p, string_of_list m, string_of_list r)
;;


(* allMatches *)

let rec allMatchesRE line re =
    match line with
		| [] -> []
		| x::xs -> let (b, a, m, r) = firstMatchRE line re in
								if b then [[x]@a, m, r]@allMatchesRE xs re
								else allMatchesRE xs re
								
;;

let allMatches line re =
    List.map
        (fun (p,m,r) -> (string_of_list p, string_of_list m, string_of_list r))
        (allMatchesRE (list_of_string line) re)
;;


(* replaceAllMatches *)

let rec replaceAllMatchesRE line rpl re =
    match line with
		| [] -> []
		| x::xs -> let [a, m, r] = allMatchesRE line re in
								if m <> [] then a@m@replaceAllMatchesRE xs rpl re
								else replaceAllMatchesRE xs rpl re
;;

let replaceAllMatches line rpl re =
    let lineStr = list_of_string line in
      let rplStr = list_of_string rpl in
        let res = replaceAllMatchesRE lineStr rplStr re in
          string_of_list res
;;


(* allMatchesFile *)

let allMatchesFile ni re =
    []
;;


(* allMatchesOverlap *)

let rec allMatchesOverlapRE line re =
    []
;;

let allMatchesOverlap line re =
    List.map
        (fun (p,m,r) -> (string_of_list p, string_of_list m, string_of_list r))
        (allMatchesOverlapRE (list_of_string line) re)
;;


(* matchAtStartGreedyRE *)

let matchAtStartGreedyRE line re =
    (false, [], [])
;;

let matchAtStartGreedy line re =
    let (b,m,r) = matchAtStartGreedyRE (list_of_string line) re in
        (b, string_of_list m, string_of_list r)
;;



