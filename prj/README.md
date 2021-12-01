# SOME EXAMPLES

## 1

### LISP version:
- ((lambda ((x) (y))  (x y)) (1 (lambda (z) z)) 
### MiniLisp version:
- val identity=lambda (cons((Var "z"),none),(Var "z"));
- val funzione=lambda(cons((Var "x"),cons((Var "y"),none)),apply((Var "y"), (Var "x")));

- printer (quote(apply(funzione,cons((Int 1),cons(identity,none)))));
- printer (apply(funzione,cons((Int 1),cons(identity,none))));

## 2

### LISP version:
- (apply (lambda (x) (let ((y 5)) (apply x 1)) zlam)
### MiniLisp version:
- val zlam = lambda( (cons(Var "z", none), Var "y" ));

- val appl_zlam = apply( lambda( cons((Var "x"), none), letLisp( cons(cons((Var "y"), cons((Int 5), none)), none),apply((Var "x"), cons((Int 1),none)))), cons(zlam,none));
- printer (quote appl_zlam);
- printer(appl_zlam);

## 3

### LISP version:
- let val x = 3 in (let val y = x in (let val x = 3 in y+1 end)end)end
### MiniLisp version:
- val let_3 = letLisp( cons(cons((Var "x"), cons((Int 3),none)),none), plus((Var "y"),(Int 1)));
- val let_2 = letLisp( cons(cons((Var "y"), cons((Var "x"),none)),none), let_3);
- val let_1 = letLisp( cons(cons((Var "x"), cons((Int 2),none)),none), let_2);
- printer(quote(let_1));
- printer(let_1);