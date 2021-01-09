(* Mini-LISP *)

type symbol = string

datatype lisp = Unit of unit 
| Int of int 
| Str of string 
| Char of char 
| Bool of bool 
| Real of real 
| Var of symbol
| Sym of symbol
| Exp of symbol
| none 
| plus of lisp*lisp 
| Plus of lisp
| car of lisp       
| cdr of lisp       
| letLisp of lisp*lisp
| lambda of lisp*lisp   
| apply of lisp*lisp
| quote of lisp 
| cons of lisp*lisp


(* given a Int-lisp it return an int*)  
fun getInt (Int i) = i
| getInt _ = raise Fail "Wrong argument type, integer needed"
  


fun len(lst) = case lst of none => 0
                |cons(h,t) => 1 + (len t)
                |_ => raise Fail "Error argument not a list"

fun id x = x


fun eval (Unit u) = Unit u
| eval (Int i) = Int i
| eval (Str s) = Str s
| eval (Char c) = Char c
| eval (Bool b) = Bool b
| eval (Real r) = Real r
| eval (Var x) = Var x
| eval (Sym sym) = Sym sym
| eval (none) = none
| eval (plus (a,b)) = Int ((getInt (eval a)) + (getInt (eval b)))

(*This function adds its arguments together *)

| eval (Plus lst) = Int (let fun sum(lst) = 
    case lst of 
        none => 0
    |cons(h,t) => getInt(eval h) + sum(t)
    in sum (eval lst) end)

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

| eval (lambda(var,body)) = eval (car body)

(*apply calls lisp functions with given arguments*)
| eval (apply((Sym h),t)) = let fun getFun(h,t): lisp =
    case h of
     "plus" => eval (plus (car t, (car (cdr t))))
    |"Plus" => eval(Plus t)
    |"car" => eval (car t)
    |"cdr" => eval (cdr t)
    in getFun(h,t) end

| eval (apply(lambda(var,body),args)) = 
    if (len var) = (len args) then 
        let val Env =
            let fun startEnv( var, args, env)= case var of
                none => env
                | cons (h,t) => startEnv(eval(cdr(var)), eval(cdr(args)),cons(cons(eval(car(var)),cons(eval(car(args)),none)),env))
            in startEnv(var,args,none) end 

        in let fun getEnv ((Var v), env) = 
            case env of
            none => raise Fail "Empty environment"
            | cons (h,t) => if  v = let fun getVar(Var x) = x in getVar(eval(car(h))) end
                            then eval (car (cdr h) ) else getEnv( (Var v), t ) in

            let fun evalExp(body,res) = case body of
                Plus(none) => Int res
                |Plus(cons(Var x, t)) => evalExp(Plus (t), res + getInt(getEnv((Var x), Env))) 
                |Plus(cons(Int i, t)) => evalExp(Plus(t) , res + i) 
                |car(Var x) => eval(car(getEnv((Var x), Env)))   
                |car(cons(Var x, t)) => eval(getEnv((Var x), Env))
                |car(cons(h, t)) => eval(h)
            in evalExp(body,0) end 
        end end
 
    else raise Fail "wrong number of arguments"

| eval (cons(h,t)) = cons(h,t)
| eval _ = raise Fail "non exaustive match"


fun pretty (Unit u) = "()"
| pretty (Int i) = Int.toString i 
| pretty (Str s) = "\""^s^"\""
| pretty (Char c) = Char.toString c
| pretty (Bool b) = Bool.toString b
| pretty (Real r) = Real.toString r
| pretty (Var x) = x
| pretty (Sym sym) = sym
| pretty (Exp e) = e
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
| getType (Sym sym) = "symbol" 
| getType (cons(h,t)) = "cons"

fun typeOf term = (print ("\n- "^ (getType term) ^"\n"^"\n"));

print ("\n\nExamples:\n");
val t = cons((Int 1), cons((Int 2), cons((Int 3), cons((Int 4),none))));

val l = (lambda(cons((Var "a"),cons((Var "b"),none)), cons((Sym "Plus"),cons((Var "a"),cons((Var "b"),none)))));

val body = Plus(cons(eval(car(cons((Var "x"), cons((Var "y"),none)))), cons((Var "y"),none)));

