(*
##################################################
##################################################
#################### ex.ml #######################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : esempi
# Nome File     : ex.ml
# ------------------------------------------------ 
#
*)

Printf.printf("ESEMPI: Reflect\n");;

(* ESEMPIO 1 *)
let s = "Sum(Eint 1,Sum(Eint 5,Eint 4))";;
let com = Reflect(Estring s);;

let d = [("result",Newloc(Estring ""))];;
let (rho,sigma) = semdv d (emptyenv Unbound) (emptystore Undefined);;
let sigma1 = semc com rho sigma;;

#use "output.ml"

(* ESEMPIO 2 *)
let s = "Sum(Sum(Eint 10,Eint 1),Diff(Eint 2,Eint 1))";;
let com = Reflect(Estring s);;

let d = [("result",Newloc(Estring ""))];;
let (rho,sigma) = semdv d (emptyenv Unbound) (emptystore Undefined);;
let sigma1 = semc com rho sigma;;

#use "output.ml"

(* ESEMPIO 3 *)
let s = "Substr(Estring CIAO,Sum(Eint 0,Eint 1),Sum(Eint 1,Eint 1))";;
let com = Reflect(Estring s);;

let d = [("result",Newloc(Estring ""))];;
let (rho,sigma) = semdv d (emptyenv Unbound) (emptystore Undefined);;
let sigma1 = semc com rho sigma;;

#use "output.ml"

(* ESEMPIO 4 *)
let s = "Sum(Diff(Eint 5,Minus(Minus(Eint 1))),Len(Estring ciao))";;
let com = Reflect(Estring s);;

let d = [("result",Newloc(Estring ""))];;
let (rho,sigma) = semdv d (emptyenv Unbound) (emptystore Undefined);;
let sigma1 = semc com rho sigma;;

#use "output.ml"

(* ESEMPIO 5 *)
let s = "Reflect(Sum(Diff(Eint 5,Minus(Minus(Eint 1))),Len(Estring ciao)))";;
let com = Reflect(Estring s);;

let d = [("result",Newloc(Estring ""))];;
let (rho,sigma) = semdv d (emptyenv Unbound) (emptystore Undefined);;
let sigma1 = semc com rho sigma;;

#use "output.ml"
