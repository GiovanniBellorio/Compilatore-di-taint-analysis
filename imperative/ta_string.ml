(*
##################################################
##################################################
################# ta_string.ml ###################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : taint analisi su stringhe
# Nome File     : ta_string.ml
# ------------------------------------------------ 
#
*)

(* tainted   --> dati untrusted *)
(* untainted --> dati trusted   *)

(* DATI INPUT *)
let ax = Estring "ciao" ;;
let bx = Estring "mondo";;
let cx = Estring "hello";;
let dx = Estring "world";;

(* PARTIZIONAMENTO MEMORIA & AMBIENTE *)

(* RX - TAINTED *)
let (l1,sigma1) = allocate(emptystore(Undefined),Undefined);;
let rho1        = bind(emptyenv(Unbound),"ax",Dloc l1);;
let sigma1      = semc (Assign(Den "ax",ax)) rho1 sigma1;;

let (l2,sigma2) = allocate(sigma1,Undefined);;
let rho2        = bind(rho1,"bx",Dloc l2);;
let sigma2      = semc (Assign(Den "bx",bx)) rho2 sigma2;;

(* RX - UNTAINTED *)
let (l3,sigma3) = allocate(sigma2,Undefined);;
let rho3        = bind(rho2,"cx",Dloc l3);;
let sigma3      = semc (Assign(Den "cx",cx)) rho3 sigma3;;

let (l4,sigma4) = allocate(sigma3,Undefined);;
let rho4        = bind(rho3,"dx",Dloc l4);;
let sigma4      = semc (Assign(Den "dx",dx)) rho4 sigma4;;

(* TAINT ANALYSIS *)
let (l5,sigma5) = allocate(sigma4,Undefined);;
let rho5        = bind(rho4,"result_taint",Dloc l5);;

(* INPUT *)
let s = Concat(Val(Den "ax"),Val(Den "dx"));;

(* START *)
let (rho,sigma) = taint(s,rho5,sigma5);;

(* RESULT - OUTPUT *)
sem (Estring "OUTPUT") rho sigma;;

sem (Estring "AX --> tainted") rho sigma;;
sem (Val(Den "ax")) rho sigma;;

sem (Estring "BX --> tainted") rho sigma;;
sem (Val(Den "bx")) rho sigma;;

sem (Estring "CX --> untainted") rho sigma;;
sem (Val(Den "cx")) rho sigma;;

sem (Estring "DX --> untainted") rho sigma;;
sem (Val(Den "dx")) rho sigma;;

s;;
sem (Val(Den "result_taint")) rho sigma;;
if (equals(sem (Val(Den "result_taint")) rho sigma,String("untainted"))) then sem s rho sigma else failwith ("Code Injection");;
