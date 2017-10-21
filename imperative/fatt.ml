Printf.printf ("ESEMPIO: Fattoriale\n");;

let number = 4;;
let fact = Let("fact",
				Rec("fact", Fun(["x"],Ifthenelse(Eq(Den "x",Eint 0),Eint 1, Prod(Den "x",Appl(Den "fact",[Diff(Den "x", Eint 1)]))))),
				Appl(Den "fact",[Eint number]));;

let semfact = sem fact (emptyenv Unbound) (emptystore Undefined);;
