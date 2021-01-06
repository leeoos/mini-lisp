(* mini_lisp.smli*)

datatype lisp = i of int | s of string | c of char | b of bool | r of real | none | cons of lisp*lisp | plus of lisp*lisp | car of lisp | cdr of lisp | quote of lisp; 

(*indipendenti da cosa contengano le liste *)
fun car(lst: lisp): lisp =
    case lst of
        none => none
    | cons(h,t) => h

fun cdr(lst: lisp): lisp =
    case lst of
        none => none
    | cons(h,t) => t


fun convert (none) = "()"
|convert (i x) = Int.toString x
|convert (s x) = "\""^x^"\""
|convert (c x) = Char.toString x
|convert (b x) = Bool.toString x
|convert (r x) = Real.toString x
|convert (plus(x,y)) = (convert x) ^"+"^ (convert y)
|convert (quote x) = "\""^convert x^"\""
|convert (cons(h,t)) = (convert h) ^","^ (convert t) 

fun isList (x:string) = 
    if String.isSubstring "," x then
        "["^ x ^"]"
    else x

fun eval (x:lisp) = isList (convert x)



(*translate f s returns the string generated from s by mapping each character in s by f. It is equivalent to concat(List.map f (explode s))*)
(*
fun editString(stringa,chars) = 
    let fun rm this = 
        if String.isSubstring(str this) chars then 
            "" 
        else 
            str c 
    in String.translate rm stringa 
    end;
*)
