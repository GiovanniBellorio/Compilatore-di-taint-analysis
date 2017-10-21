Printf.printf("Reflect\n");;

let com = Reflect(Estring s);;

let (l,sigma) = allocate(emptystore(Undefined), Undefined);;
let rho = bind(emptyenv(Unbound), "result", Dloc l);;
let sigma1 = semc com rho sigma;;

#use "output.ml"
