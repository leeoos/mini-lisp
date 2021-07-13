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
| none
| plus of lisp*lisp
| Plus of lisp
| car of lisp
| cdr of lisp
| letLisp of lisp*lisp
| lambda of lisp*lisp
| apply of lisp*lisp
| quote of lisp
| quoteCons of lisp
| cons of lisp*lisp
| applyFun of lisp*lisp*lisp
| letFun of lisp*lisp*lisp

(* given a Int-lisp it return an int*)
fun getInt (Int i) = i
| getInt (Var x) = raise Fail ("Unbound variable " ^ x)
| getInt _ = raise Fail "Wrong argument type, integer needed"

fun isInt (elem) = case elem of (Int i) => true | _=> false
fun isVar (elem) = case elem of (Var v) => true | _=> false
fun isQuote(elem) = case elem of quote(l) => true | _ => false

fun len(lst) = case lst of none => 0
                |cons(h,t) => 1 + (len t)
                |_ => raise Fail "Error argument not a list"


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
        | cons(h,t) => h
    in getCar (eval lst) end

(*cdr of a list displays all the elemnts of that list but the first *)
| eval (cdr lst) = let fun getCdr(lst) =
    case lst of
        none => Unit ()
        | cons(h,t) => t
    in getCdr (eval lst) end

(*let expression *)
| eval (letLisp(var,body)) = eval(letFun(var,body,none)) (*mask function *)
| eval(letFun(var,body,ENV)) =
    (*extracting function arguments from var *)
    let val args =
        let fun getArgs(var,arg) = case var of
            none => arg
            |cons(h,t) => getArgs(t,cons(eval(car(cdr(h))),arg))
        in getArgs(var,none) end
    (*extracting variable parameters from var *)
    in let val vars =
        let fun getVars(var,vars) = case var of
            none => vars
            |cons(h,t) => getVars(t,cons(eval(car(h)),vars))
        in getVars(var,none) end
    (*calling the apply function on the lambda term equivalent to let *)
        in eval(applyFun(lambda(vars,body),args,ENV)) end
	  end

(*lambda expression is a function  written in lisp *)
| eval (lambda(var,body)) = lambda(var,body)

(*apply calls lisp functions with given arguments *)
| eval (apply((Sym h),t)) = let fun getFun(h,t): lisp =
    case h of
       "plus" => eval (plus (car t, (car (cdr t))))
      |"Plus" => eval(Plus t)
      |"car" => eval (car t)
      |"cdr" => eval (cdr t)
    in getFun(h,t) end

(*secial case, apply of lambda *)
| eval (apply(lambda(var,body),args)) = eval (applyFun(lambda(var,body),args,none)) (*mask function*)
| eval (applyFun(x,args,ENV)) =
    (*check if the firs argument is a lambda expression or another lisp term*)
    let fun f(x,args , ENV) = case x of
		    Sym s => eval(apply(x,args))
		    | lambda(var,body) =>       (*if is a lambda ...*)
			  if (len var) = (len args) then
        (*fill the enviroment *)
				let val Env =
				    let fun startEnv( var, args, env)= case var of
						      none => env
						      | cons (h,t) => startEnv(eval(cdr(var)), eval(cdr(args)), cons(cons(eval(car(var)),cons(eval(eval(car(args))),none)),env))
					  in startEnv(var,args,ENV) end

        (*local fun to get values of a var in the enviroment *)
				in let fun getEnv ((Var v), env) =
						case env of
						      none => raise Fail "Empty environment or variable not found"
						      | cons (h,t) => if  v = let fun getVar(Var x) = x in getVar(eval(car(h))) end then
                                      eval (car (cdr h) )
                                  else getEnv( (Var v), t )

			      (*evaluate the body of the function case by case*)
            in let fun evalExp(body,res) = case body of
                (Var x) => eval(getEnv((Var x),Env))
  							|Plus(none) => Int res
  							|Plus(cons(Var x, t)) => evalExp(Plus (t), res + getInt(getEnv((Var x), Env)))
  							|Plus(cons(Int i, t)) => evalExp(Plus(t) , res + i)
  							|plus(x,y) => if isVar(x) then
                                  if isVar(y) then evalExp(plus(getEnv(x, Env),getEnv(y, Env)),0)
										              else evalExp(plus(getEnv(x,Env),y),0)
										          else
                                  if isVar(y) then evalExp(plus(x,getEnv(y,Env)),0)
                                  else eval(plus(x,y))

  							|car(Var x) => eval(car(getEnv((Var x), Env)))
  							|car(cons(Var x, t)) => eval(getEnv((Var x), Env))
  							|car(cons(h, t)) => eval(h)
                |cdr(Var x) => eval(cdr(getEnv((Var x), Env)))
                |cdr(cons(h,t)) => eval(t)
  							|quote(x) => if isVar(x) then eval(quote(getEnv(x,Env))) else eval(quote(x))

                (*if apply or let are inside the function's body then we need to make a recursive call passing the old env *)
  							|apply(x,y) =>  if isVar(x) then
                                    let val X = getEnv(x,Env) in
                                    if isVar(y) then
                                        let val Y = getEnv(y,Env)
        													      in eval(applyFun(X,cons(Y,none),Env)) end
      												      else eval(applyFun(X,y,Env)) end
											          else
                                  if isVar(y) then
                                      let val Y = getEnv(y,Env)
													            in eval(applyFun(x,cons(Y,none),Env)) end
												          else eval(applyFun(x,y,Env))

							  |letLisp(var,body) => if isVar(var) then
    													            let val VAR = getEnv(var,Env) in
    														          if isVar(body) then
        																      let val Body=getEnv(body,Env)
        																      in eval(letFun(VAR,Body,Env)) end
    															        else eval(letFun(VAR,body,Env)) end
												              else
                                          if isVar(body) then
        														          let val Body=getEnv(body,Env)
        																      in eval(letFun(var,Body,Env)) end
  												                else eval(letFun(var,body,Env))
						    in evalExp(body,0) end
					end
				end
			else raise Fail "wrong number of arguments"
		in f(x,args,ENV)
	end

(* quote returns its single argument, as written, without evaluating it *)
| eval (quote l) = let fun mkCons (Unit u) = cons(Unit u, none)
    |mkCons (Int i) = Int i
    |mkCons (Str s) = Str s
    |mkCons (Char c) = Char c
    |mkCons (Bool b) = Bool b
    |mkCons (Real r) = Real r
    |mkCons (Var x) = Var x
    |mkCons (Sym sy) = Sym sy
    |mkCons (none) = cons(Sym "none", none)
    |mkCons (plus( (Int x), (Int y) )) = cons(Sym "plus", cons(Int x, cons( Int y, none )))
    |mkCons (Plus lst) = cons(Sym "Plus", mkCons(lst))
    |mkCons (car lst) = cons(Sym "car", mkCons(lst))
    |mkCons (cdr lst) = cons(Sym "cdr", mkCons(lst))
    |mkCons (letLisp(m,n)) = cons(Sym "let", cons(mkCons(m),cons(mkCons(n),none)))
    |mkCons (lambda(var,body)) = cons(Sym "lambda", cons(mkCons(var),cons(mkCons(body),none)))
    |mkCons (quote lst) = cons(Sym "quote", mkCons(lst))
    |mkCons (cons(h,t)) = cons(Sym "cons", cons(h,mkCons(t)))
    in mkCons(l) end

(*cons create s-expression *)
| eval (cons(h,t)) = cons(h,t)
| eval _ = raise Fail "impossible to evaluate the expression, non exaustive match"

(*pretty converts lisp in string*)
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
| pretty (letLisp (var,body)) = "(let "^ pretty(var) ^" "^ pretty(body) ^")"
| pretty (lambda (var,body)) = "(lambda "^ pretty(var) ^" "^ pretty(body) ^")"
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
    | printQuote (letLisp(var,body)) = pretty(letLisp(var,body))
    | printQuote (lambda (var,body)) = pretty(lambda(var,body))
    | printQuote (apply (m,n)) = pretty(apply(m,n))
    | printQuote (quote x) = "(quote " ^ printQuote(x) ^")"
    | printQuote (cons(h,t)) =
        let fun printList(lst:lisp):string =
            case lst of
                none => "none)"
                | cons(h,t) => "(cons "^  (printQuote h) ^" "^ (printList t)
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
| getType (lambda(var,body)) = "lambda"

fun typeOf term = (print ("\n- "^ (getType (eval term)) ^"\n"^"\n"));
