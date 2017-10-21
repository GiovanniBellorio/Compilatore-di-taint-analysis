Printf.printf("Reflect\n");;

let s = "While(Not(Eq(Val(Den z),Eint 0)),[Assign(Den w,Prod(Val(Den w),Val(Den z)));Assign(Den z,Diff(Val(Den z),Eint 1))])";;

let d = [("z",Newloc(Eint 4));("w",Newloc(Eint 1))];;
let (rho,sigma) = semdv d (emptyenv Unbound) (emptystore Undefined);;

let com = Reflect(Estring s);;

let sigma1 = semc com rho sigma;;

sem (Val(Den "z")) rho sigma1;;
sem (Val(Den "w")) rho sigma1;;
