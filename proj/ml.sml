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
| letLisp of lisp*lisp*lisp
| lambda of lisp*lisp*lisp   
| apply of lisp*lisp
| quote of lisp 
| quoteCons of lisp 
| cons of lisp*lisp


(* given a Int-lisp it return an int*)  
fun getInt (Int i) = i
| getInt (Var x) = raise Fail ("Unbound variable " ^ x)
| getInt _ = raise Fail "Wrong argument type, integer needed"

fun isInt (elem) = case elem of (Int i) => true | _=> false
fun isVar (elem) = case elem of (Var v) => true | _=> false
fun isQuote(elem) = case elem of quote(l) => true | _ => false   
fun isFun(elem) = case elem of lambda(a,b,c) => true 
                    | letLisp(a,b,c) => true
                    |_ => false   
  
fun len(lst) = case lst of none => 0
                |cons(h,t) => 1 + (len t)
                |_ => raise Fail "Error argument not a list"
val Env = none

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

(*the add function adds its arguments together *)
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
    in getCdr (eval lst) end

(*let expression *)
| eval (letLisp(var,body,ENV)) = let val args = 
    let fun getArgs(var,arg) = case var of
        none => arg
    |cons(h,t) => getArgs(t,cons(eval(car(cdr(h))),arg))
    in getArgs(var,none) end
    in let val vars =    
        let fun getVars(var,vars) = case var of
            none => vars
        |cons(h,t) => getVars(t,cons(eval(car(h)),vars))
        in getVars(var,none) end
    in eval(apply(lambda(vars,body,ENV),args)) end end
 
(*lambda expression is a function object written in lisp *)
| eval (lambda(var,body,ENV)) = lambda(var,body,ENV)

(*apply calls lisp functions with given arguments *)
| eval (apply((Sym h),t)) = let fun getFun(h,t): lisp =
    case h of
     "plus" => eval (plus (car t, (car (cdr t))))
    |"Plus" => eval(Plus t)
    |"car" => eval (car t)
    |"cdr" => eval (cdr t)
    in getFun(h,t) end

(*secial case, apply of lambda *) 
| eval (apply(lambda(var,body,ENV),args)) = (*let fun applyLam(lambda(var,body),args,ENV)*)
    if (len var) = (len args) then 
        let val Env =
            let fun startEnv( var, args, env)= case var of
                none => env
                | cons (h,t) => startEnv(eval(cdr(var)), eval(cdr(args)),cons(cons(eval(car(var)),cons(eval(car(args)),none)),env))
            in startEnv(var,args,ENV) end
            
        in let fun getEnv ((Var v), env) = 
                case env of
                    none => raise Fail ("Empty environment or variable not found " ^ v)
                | cons (h,t) => if  v = let fun getVar(Var x) = x in getVar(eval(car(h))) end
                            then eval (car (cdr h) ) else getEnv( (Var v), t )
            
            in let fun evalExp(body,res) = case body of
                (Var x) => getEnv((Var x), Env)
                |Plus(none) => Int res
                |Plus(cons(Var x, t)) => evalExp(Plus (t), res + getInt(getEnv((Var x), Env))) 
                |Plus(cons(Int i, t)) => evalExp(Plus(t) , res + i) 
                |plus(x,y) => if isVar(x) then if isVar(y) then eval(plus(getEnv(x, Env),getEnv(y, Env)))
                                               else eval(plus(getEnv(x, Env),y))
                              else if isVar(y) then eval(plus(x,getEnv(y,Env))) else eval(plus(x,y))
                |car(Var x) => eval(car(getEnv((Var x), Env)))   
                |car(cons(Var x, t)) => eval(getEnv((Var x), Env))
                |car(cons(h, t)) => eval(h)
                |quote(x) => if isVar(x) then eval(quote(getEnv(x,Env))) else eval(quote(x))
                |apply(x,y) => if isVar(x) then let val X = getEnv(x,Env) in
                                if isVar(y) then eval(apply(X,getEnv(y,Env))) else eval(apply(X,y)) end 
                                else eval(apply(x,y)) 
                |letLisp(x,y,z) => if isVar(x) then let val X = getEnv(x,Env) in
                                if isVar(y) then eval(letLisp(X,getEnv(y,Env),Env)) else eval(letLisp(X,y,Env)) end 
                                else eval(letLisp(x,y,Env)) 
                in evalExp(body,0) end 
        end end (*in applyLam(lambda(var,body),args,none) end*)
        else raise Fail "wrong number of arguments"

| eval (quote l) = let fun mkCons (Unit u) = cons(Unit u, none) 
    |mkCons (Int i) = cons(Int i, none)
    |mkCons (Str s) = cons(Str s, none)
    |mkCons (Char c) = cons(Char c, none)
    |mkCons (Bool b) = cons(Bool b, none)
    |mkCons (Real r) = cons(Real r, none)
    |mkCons (Var x) = cons(Var x, none)
    |mkCons (Sym sy) = cons(Sym sy, none)
    |mkCons (none) = cons(Sym "none", none)
    |mkCons (Plus lst) = cons(Sym "Plus", mkCons(lst))
    |mkCons (car lst) = cons(Sym "car", mkCons(lst))
    |mkCons (cdr lst) = cons(Sym "cdr", mkCons(lst))
    |mkCons (letLisp(m,n,ENV)) = cons(Sym "let", cons(mkCons(m),cons(mkCons(n),none)))
    |mkCons (lambda(var,body,ENV)) = cons(Sym "lambda", cons(mkCons(var),cons(mkCons(body),none)))
    |mkCons (quote lst) = cons(Sym "quote", mkCons(lst))
    |mkCons (cons(h,t)) = cons(Sym "cons", cons(h,mkCons(t)))
    in mkCons(l) end

| eval (cons(h,t)) = cons(h,t)
| eval _ = raise Fail "impossible to evaluate the expression, non exaustive match"


fun pretty (Unit u) = "()"
| pretty (Int i) = Int.toString i 
| pretty (Str s) = "\""^s^"\""
| pretty (Char c) = Char.toString c
| pretty (Bool b) = Bool.toString b
| pretty (Real r) = Real.toString r
| pretty (Var x) = x
| pretty (Sym sym) = sym

| pretty (none) = ""
| pretty (plus(a,b)) = "(+ " ^ pretty(a) ^" "^ pretty(b) ^")"
| pretty (Plus lst) = "(+ " ^ pretty(lst) ^" )"
| pretty (car lst) = "(car " ^ pretty(lst) ^")"
| pretty (cdr lst) = "(cdr " ^ pretty(lst) ^")"
| pretty (lambda (var,body,ENV)) = "(lambda "^ pretty(var) ^" "^ pretty(body) ^")"
| pretty (apply (m,n)) = "(apply '" ^ pretty(m) ^"' "^ pretty(n) ^ ")"

| pretty (quote l) = let fun printQuote (Unit u) = "()"
    | printQuote (Int i) = Int.toString i 
    | printQuote (Str s) = s
    | printQuote (Char c) = Char.toString c
    | printQuote (Bool b) = Bool.toString b
    | printQuote (Real r) = Real.toString r
    | printQuote (Var x) = x
    | printQuote (Sym sy) = sy
    | printQuote (none) = ""     
    | printQuote (Plus lst) = pretty(Plus lst)
    | printQuote (car lst) = pretty(car lst)
    | printQuote (cdr lst) = pretty(cdr lst)
    | printQuote (lambda (var,body,ENV)) = pretty(lambda(var,body,ENV))
    | printQuote (apply (m,n)) = pretty(apply(m,n))    
    | printQuote (quote x) = "(quote " ^ printQuote(x) ^")"
    | printQuote (cons(h,t)) =   
        let fun printList(lst:lisp):string = 
            case lst of 
                none => "none)"
            |cons(h,t) => "(cons "^  (printQuote h) ^" "^ (printList t)
        in printList (cons(h,t)) end 
    in printQuote(l) end

| pretty (cons(h,t)) = "("^ let fun printCons(lst:lisp):string = 
    case lst of 
            none => ""
    | cons(h,t) => (pretty h) ^" "^ (printCons t)
    in printCons (cons(h,t)) end  ^")"


fun printer term = (print ("\n- "^ (if isQuote(term) then (pretty term) else pretty(eval term)) ^"\n"^"\n"))
(*fun printer term = (print ("\n- "^ (pretty(eval term)) ^"\n"^"\n"))*)


fun getType (Unit u) = "Unit"
| getType (Int i) = "int"
| getType (Str s) = "string"
| getType (Char c) = "char"
| getType (Bool b) = "bool"
| getType (Real r) = "real"
| getType (Var x) = "variable"
| getType (Sym sym) = "symbol" 
| getType (cons(h,t)) = "cons"

fun typeOf term = (print ("\n- "^ (getType (eval term)) ^"\n"^"\n"));

(* t = (1 2 3 4) *)
val t = cons((Int 1), cons((Int 2), cons((Int 3), cons((Int 4),none))));

print ("\n\nExamples:\n");
(* var =  (x y) *)
val var = cons(Var "x", cons(Var "y", none));

(* args =  (2 4) *)
val args = cons(Int 2, cons(Int 4, none));

(* body = (+ x y) *)
val body = Plus(cons(eval(car(cons((Var "x"), cons((Var "y"),none)))), cons((Var "y"),none)));

val j = Plus(cons(Int 1, cons(Int 2, none)));

val a = cons(Sym "Plus", cons(cons(Int 1, cons(Int 2, none)),none))

(*eval(apply(lambda(var,apply(Var "x", Var "y")),a));*)


val v = cons(cons((Var "x"), cons(j,none)),cons(cons(Var "y",cons(j,none)),none));

val a = cons(cons((Var "x"), cons(plus((Int 1),(Int 2)),none)),cons(cons((Var "y"), cons((Int 1),none)),none));

(*eval(apply(lambda(cons((Var "z"),none),b),cons(a,none)));*)


(*let val x = 7 in ((fn y => (let val x = 3 in y 9 end)) (fn z => x)) *)
(*
let val x = 7 in (let val  y =  (let val  z = 9 in x end) in (let val x = 3 in y  end) end ) end;*)

val x = cons(cons((Var "x"), cons((Int 7),none)),none);
val lam = letLisp(cons(cons((Var "z"),cons((Int 9),none)),none),(Var "x"),none);
val y = cons((Var "y"),cons(lam,none));
val body_lam = letLisp(cons(cons((Var "x"), cons((Int 3),none)),none), (Var "y"),none);
val body_let = letLisp(cons(y,none),body_lam,none);

eval(letLisp(x,body_let,none));

(*eval(letLisp(x,apply(body_let,cons(lam,none))));*)
(*
fun startEnv( var, args, Env)= case var of
    none => Env
    | cons (h,t) => startEnv(eval(cdr(var)), eval(cdr(args)),cons(cons(eval(car(var)),cons(eval(car(args)),none)),Env));
    
startEnv(var,args,Env);
printer Env;*)

apply(lambda(cons((Var "x"),none),letLisp(y,(Var "y"),none),none),cons((Int 3),none));
(fn x => let val y = (let val z=9 in z end) in y end) 3;
(fn x => let val y = (let val z=9 in x end) in y end) 3;
