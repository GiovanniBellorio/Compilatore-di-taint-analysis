(*
##################################################
##################################################
################# ta_com3.ml #####################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : taint analisi su comandi (3)
# Nome File     : ta_com3.ml
# ------------------------------------------------ 
#
*)

(* tainted   --> dati untrusted *)
(* untainted --> dati trusted   *)

(* DATI INPUT *)
let ah = Eint 1;;
let al = Eint 5;;
let bh = Eint 4;;
let bl = Eint 0;;
let ch = Eint 1;;
let cl = Eint 5;;
let dh = Eint 4;;
let dl = Eint 0;;

(* PARTIZIONAMENTO MEMORIA & AMBIENTE *)

(* RX - TAINTED *)
let (l,sigma)  = allocate(emptystore(Undefined),Undefined);;
let rho        = bind(emptyenv(Unbound),"ah",Dloc l);;
let sigma      = semc (Assign(Den "ah",ah)) rho sigma;;

let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"al",Dloc l);;
let sigma      = semc (Assign(Den "al",al)) rho sigma;;

let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"bh",Dloc l);;
let sigma      = semc (Assign(Den "bh",bh)) rho sigma;;

let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"bl",Dloc l);;
let sigma      = semc (Assign(Den "bl",bl)) rho sigma;;

(* RX - UNTAINTED *)
let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"ch",Dloc l);;
let sigma      = semc (Assign(Den "ch",ch)) rho sigma;;

let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"cl",Dloc l);;
let sigma      = semc (Assign(Den "cl",cl)) rho sigma;;

let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"dh",Dloc l);;
let sigma      = semc (Assign(Den "dh",dh)) rho sigma;;

let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"dl",Dloc l);;
let sigma      = semc (Assign(Den "dl",dl)) rho sigma;;

(* TAINT ANALYSIS *)
let (l,sigma)  = allocate(sigma,Undefined);;
let rho        = bind(rho,"result_taint",Dloc l);;

(* INPUT *)
let d = [("str",Newloc(Estring "Sum(Val(Den ch),Sum(Val(Den cl),Val(Den dh)))"))];; (* Sum(Eint 1,Sum(Eint 5,Eint 4)) *)
let (rho,sigma) = semdv d rho sigma;;
let s = Reflect(Val(Den "str"));;

(* START *)
let (rho,sigma) = taintc(s,rho,sigma);;

(* RESULT - OUTPUT *)
sem (Estring "OUTPUT") rho sigma;;

sem (Estring "Ah --> tainted") rho sigma;;
sem (Val(Den "ah")) rho sigma;;

sem (Estring "Al --> tainted") rho sigma;;
sem (Val(Den "al")) rho sigma;;

sem (Estring "Bh --> tainted") rho sigma;;
sem (Val(Den "bh")) rho sigma;;

sem (Estring "Bl --> tainted") rho sigma;;
sem (Val(Den "bl")) rho sigma;;

sem (Estring "Ch --> untainted") rho sigma;;
sem (Val(Den "ch")) rho sigma;;

sem (Estring "Cl --> untainted") rho sigma;;
sem (Val(Den "cl")) rho sigma;;

sem (Estring "Dh --> untainted") rho sigma;;
sem (Val(Den "dh")) rho sigma;;

sem (Estring "Dl --> untainted") rho sigma;;
sem (Val(Den "dl")) rho sigma;;

sem (Val(Den "str")) rho sigma;;
s;;
sem (Val(Den "result_taint")) rho sigma;;
if (equals(sem (Val(Den "result_taint")) rho sigma,String("untainted"))) then
	sem (Estring "Parsing load...") rho sigma
else
	failwith ("Code Injection");;

#use "reflect_taint.ml";;
