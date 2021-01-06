(* mini lisp *)
type sym = string

datatype lisp = Unit of unit 
| Int of int 
| Str of string 
| Char of char 
| Bool of bool 
| Real of real 
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

(* return the first element of a s-expression*)
fun getCar(lst: lisp): lisp =
    case lst of
        none => Unit ()
    | cons(h,t) => h

(* return the firs element of a s-expression of type cons*)
fun getCdr(lst: lisp): lisp =
    case lst of
        none => Unit ()
    | cons(h,t) => t

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
(*| eval (plus (lol lst)) = Int (sum lst)*)
| eval (car lst) = (getCar lst)
| eval (cdr lst) = (getCdr lst)
| eval (cons(h,t)) = cons(h,t)
| eval (lol lst) = (lol lst)
| eval (quote M) = lol [M]
| eval (apply((Str h),t)) = let fun getFun(h,t): lisp =
    case h of
        "plus" => eval (plus ( eval (car t), eval (car (eval (cdr t)))))
       (*"plus" => eval (plus ((eval(car t), eval ( apply((Str "plus"), cdr
       t)))))*)
    in getFun(h,t) end

fun pretty (Unit u) = "()"
| pretty (Int i) = Int.toString i 
| pretty (Str s) = "\""^s^"\""
| pretty (Char c) = Char.toString c
| pretty (Bool b) = Bool.toString b
| pretty (Real r) = Real.toString r
| pretty (plus (Int a, Int b)) = pretty(Int (a + b))
(*| pretty (plus (lol lst)) = pretty(eval(plus(lol lst)))*)
| pretty (lol lst) =  "("^ 
    let fun printList(lst:lisp list):string = 
        case lst of 
            [] => ""
        | h :: t => (pretty h) ^" "^ (printList t)
    in printList lst end  ^")"
(*| pretty (cons(h,t)) =  "$"^ (pretty h) ^" "^ (pretty t)*)
| pretty (cons(h,t)) = "("^  
    let fun printCons(lst:lisp):string = 
        case lst of 
            none => ""
        | cons(h,t) => (pretty h) ^" "^ (printCons t)
    in printCons (cons(h,t)) end  ^")"

        
fun printer value = (print (""^"\n- "^ (pretty (eval value)) ^"\n"^"\n"))

                   




