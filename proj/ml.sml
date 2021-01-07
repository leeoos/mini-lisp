(* mini lisp *)
type symbol = string
datatype lisp = Unit of unit 
| Int of int 
| Str of string 
| Char of char 
| Bool of bool 
| Real of real 
| Var of symbol
| Sym of symbol
| none 
| plus of lisp*lisp
| car of lisp 
| cdr of lisp  
| lel of lisp
| lambda of lisp
| apply of lisp*lisp
| quote of lisp 
| cons of lisp*lisp
| lol of lisp list


(* given a Int-lisp it return an int*)  
fun getInt (Int i) = i

(* returns the part of the list that follows the first item*)
fun sum(lst:lisp list): int = 
    case lst of 
        [] => 0
    | h :: t => (getInt h) + (sum t)


fun eval (Unit u) = Unit u
| eval (Int i) = Int i
| eval (Str s) = Str s
| eval (Char c) = Char c
| eval (Bool b) = Bool b
| eval (Real r) = Real r
| eval (plus (a,b)) = Int ((getInt (eval a)) + (getInt (eval b)))
| eval (cons(h,t)) = cons(h,t)

(*car of a list displays the first elemnt of that list *)
| eval (car lst) = let fun getCar(lst)= 
    case lst of
        none => Unit ()
    | cons(h,t) => (eval h)
    in getCar (eval lst) end

(*cdr of a list displays all the elemnts of that list but the first *)
| eval (cdr lst) = let fun getCdr(lst) =  
    case lst of
        none => Unit ()
    | cons(h,t) => (eval t)
    in getCdr lst end

(* return*)
| eval (apply((Str h),t)) = let fun getFun(h,t): lisp =
    case h of
       "plus" => eval (plus (car t, (car (cdr t))))
    |"car" => eval (car t)
    |"cdr" => eval (cdr t)
    in getFun(h,t) end



fun pretty (Unit u) = "()"
| pretty (Int i) = Int.toString i 
| pretty (Str s) = "\""^s^"\""
| pretty (Char c) = Char.toString c
| pretty (Bool b) = Bool.toString b
| pretty (Real r) = Real.toString r
| pretty (plus (Int a, Int b)) = pretty(Int (a + b))
| pretty (cons(h,t)) = "("^ let fun printCons(lst:lisp):string = 
    case lst of 
            none => ""
    | cons(h,t) => (pretty h) ^" "^ (printCons t)
    in printCons (cons(h,t)) end  ^")"
    

fun printer term = (print ("\n- "^ (pretty (eval term)) ^"\n"^"\n"))


fun getType (Unit u) = "Unit"
| getType (Int i) = "int"
| getType (Str s) = "string"
| getType (Char c) = "char"
| getType (Bool b) = "bool"
| getType (Real r) = "real"
| getType (Var x) = "variable"
| getType (Sym y) = "symbol" 
| getType (cons(h,t)) = "cons"

fun typeOf term = (print ("\n- "^ (getType term) ^"\n"^"\n"))



