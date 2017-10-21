(* TESI - TAINT FAIL *)

[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-8"]

(* SUPPORT FUNCTION - TYPECHECK EVAL *)
let typecheck_fail (x, y) = match x with
    | "Eint" -> (match y with
      | Eint(u) -> true
      | _ -> false)
    | "Ebool" -> (match y with
      | Ebool(u) -> true
      | _ -> false)
    | "Estring" -> (match y with
      | Estring(u) -> true
      | _ -> false)
    | _ -> failwith ("not a valid type")
;;

(* TYPECHECK PER TIPO VARIABILE DI SUPPORTO - AAX *)
let init_aax = 
	if typecheck_fail("Eint",ax) then 
		let aax = Eint 0 (* untaint *)
		in aax
	else if typecheck_fail("Ebool",ax) then 
		let aax = Ebool true (* untaint *)
		in aax
	else if typecheck_fail("Estring",ax) then 
		let aax = Estring "" (* untaint *)
		in aax
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER TIPO VARIABILE DI SUPPORTO - BBX *)
let init_bbx = 
	if typecheck_fail("Eint",bx) then 
		let bbx = Eint 0 (* untaint *)
		in bbx
	else if typecheck_fail("Ebool",bx) then 
		let bbx = Ebool true (* untaint *)
		in bbx
	else if typecheck_fail("Estring",bx) then 
		let bbx = Estring "" (* untaint *)
		in bbx
	else 
		failwith ("not a valid type")
;;

(* INIT RX - UNTAINTED - AAX - BBX *)
let aax = init_aax;;
let bbx = init_bbx;;

let (l,sigma)   = allocate(sigma,Undefined);;
let rho         = bind(rho,"aax",Dloc l);;
let sigma       = semc (Assign(Den "aax",aax)) rho sigma;;

let (l,sigma)   = allocate(sigma,Undefined);;
let rho         = bind(rho,"bbx",Dloc l);;
let sigma       = semc (Assign(Den "bbx",bbx)) rho sigma;;

(* TYPECHECK PER LA COPIA DELLA VARIABILE - AAX *)
let init_while_aax = 
	if typecheck_fail("Eint",ax) then 
		let while_ax = While(Not(Eq(Val(Den "ax"),Eint 0)),[Assign(Den "aax",Sum(Val(Den "aax"),Eint 1));Assign(Den "ax",Diff(Val(Den "ax"),Eint 1))])
		in while_ax
	else if typecheck_fail("Ebool",ax) then 
		let while_ax = While(Not(Val(Den "ax")),[Assign(Den "aax",Ebool false);Assign(Den "ax",Ebool true)])
		in while_ax
	else if typecheck_fail("Estring",ax) then 
		let while_ax = While(Not(Eq(Len(Val(Den "ax")),Eint 0)),[Cifthenelse(Not(Eq(Len(Val(Den "ax")),Eint 1)),[Assign(Den "aax",Concat(Val(Den "aax"),Substr(Val(Den "ax"),Eint 0,Eint 0)));Assign(Den "ax",Substr(Val(Den "ax"),Eint 1,Diff(Len(Val(Den "ax")),Eint 1)))],[Assign(Den "aax",Concat(Val(Den "aax"),Substr(Val(Den "ax"),Eint 0,Eint 0)));Assign(Den "ax",Estring "")])])
		in while_ax
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER LA COPIA DELLA VARIABILE - BBX *)
let init_while_bbx = 
	if typecheck_fail("Eint",bx) then 
		let while_bx = While(Not(Eq(Val(Den "bx"),Eint 0)),[Assign(Den "bbx",Sum(Val(Den "bbx"),Eint 1));Assign(Den "bx",Diff(Val(Den "bx"),Eint 1))])
		in while_bx
	else if typecheck_fail("Ebool",bx) then 
		let while_bx = While(Not(Val(Den "bx")),[Assign(Den "bbx",Ebool false);Assign(Den "bx",Ebool true)])
		in while_bx
	else if typecheck_fail("Estring",bx) then 
		let while_bx = While(Not(Eq(Len(Val(Den "bx")),Eint 0)),[Cifthenelse(Not(Eq(Len(Val(Den "bx")),Eint 1)),[Assign(Den "bbx",Concat(Val(Den "bbx"),Substr(Val(Den "bx"),Eint 0,Eint 0)));Assign(Den "bx",Substr(Val(Den "bx"),Eint 1,Diff(Len(Val(Den "bx")),Eint 1)))],[Assign(Den "bbx",Concat(Val(Den "bbx"),Substr(Val(Den "bx"),Eint 0,Eint 0)));Assign(Den "bx",Estring "")])])
		in while_bx
	else 
		failwith ("not a valid type")
;;

(* AVVIO COPIA DELLA VARIABILE - AAX *)
let while_ax = init_while_aax;;
let sigma    = semc while_ax rho sigma;;

(* AVVIO COPIA DELLA VARIABILE - BBX *)
let while_bx = init_while_bbx;;
let sigma    = semc while_bx rho sigma;;

(* COMPILATORE FAIL *)
#use "fail/semantica_fail.ml";;

(* RESULT - OUTPUT *)
sem (Estring "OUTPUT") rho sigma;;

sem (Estring "AX  --> tainted") rho sigma;;
sem (Val(Den "ax")) rho sigma;;

sem (Estring "BX  --> tainted") rho sigma;;
sem (Val(Den "bx")) rho sigma;;

sem (Estring "CX  --> untainted") rho sigma;;
sem (Val(Den "cx")) rho sigma;;

sem (Estring "DX  --> untainted") rho sigma;;
sem (Val(Den "dx")) rho sigma;;

sem (Estring "AAX --> untainted") rho sigma;;
sem (Val(Den "aax")) rho sigma;;

sem (Estring "BBX --> untainted") rho sigma;;
sem (Val(Den "bbx")) rho sigma;;

s;;
sem (Val(Den "result_taint")) rho sigma;;
sem_fail s rho sigma;;

[@@@ocaml.warning "+26"]
[@@@ocaml.warning "+8"]
