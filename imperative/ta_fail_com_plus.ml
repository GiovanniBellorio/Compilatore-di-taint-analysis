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

(* TYPECHECK PER TIPO VARIABILE DI SUPPORTO - AAh *)
let init_aah = 
	if typecheck_fail("Eint",ah) then 
		let aah = Eint 0 (* untaint *)
		in aah
	else if typecheck_fail("Ebool",ah) then 
		let aah = Ebool true (* untaint *)
		in aah
	else if typecheck_fail("Estring",ah) then 
		let aah = Estring "" (* untaint *)
		in aah
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER TIPO VARIABILE DI SUPPORTO - AAl *)
let init_aal = 
	if typecheck_fail("Eint",al) then 
		let aal = Eint 0 (* untaint *)
		in aal
	else if typecheck_fail("Ebool",al) then 
		let aal = Ebool true (* untaint *)
		in aal
	else if typecheck_fail("Estring",al) then 
		let aal = Estring "" (* untaint *)
		in aal
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER TIPO VARIABILE DI SUPPORTO - BBh *)
let init_bbh = 
	if typecheck_fail("Eint",bh) then 
		let bbh = Eint 0 (* untaint *)
		in bbh
	else if typecheck_fail("Ebool",bh) then 
		let bbh = Ebool true (* untaint *)
		in bbh
	else if typecheck_fail("Estring",bh) then 
		let bbh = Estring "" (* untaint *)
		in bbh
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER TIPO VARIABILE DI SUPPORTO - BBl *)
let init_bbl = 
	if typecheck_fail("Eint",bl) then 
		let bbl = Eint 0 (* untaint *)
		in bbl
	else if typecheck_fail("Ebool",bl) then 
		let bbl = Ebool true (* untaint *)
		in bbl
	else if typecheck_fail("Estring",bl) then 
		let bbl = Estring "" (* untaint *)
		in bbl
	else 
		failwith ("not a valid type")
;;

(* INIT RX - UNTAINTED - AAh - AAl - BBh - BBl *)
let aah = init_aah;;
let aal = init_aal;;
let bbh = init_bbh;;
let bbl = init_bbl;;

let (l,sigma)   = allocate(sigma,Undefined);;
let rho         = bind(rho,"aah",Dloc l);;
let sigma       = semc (Assign(Den "aah",aah)) rho sigma;;

let (l,sigma)   = allocate(sigma,Undefined);;
let rho         = bind(rho,"aal",Dloc l);;
let sigma       = semc (Assign(Den "aal",aal)) rho sigma;;

let (l,sigma)   = allocate(sigma,Undefined);;
let rho         = bind(rho,"bbh",Dloc l);;
let sigma       = semc (Assign(Den "bbh",bbh)) rho sigma;;

let (l,sigma)   = allocate(sigma,Undefined);;
let rho         = bind(rho,"bbl",Dloc l);;
let sigma       = semc (Assign(Den "bbl",bbl)) rho sigma;;

(* TYPECHECK PER LA COPIA DELLA VARIABILE - AAh *)
let init_while_aah =
	if typecheck_fail("Eint",ah) then 
		let while_ah = While(Not(Eq(Val(Den "ah"),Eint 0)),[Assign(Den "aah",Sum(Val(Den "aah"),Eint 1));Assign(Den "ah",Diff(Val(Den "ah"),Eint 1))])
		in while_ah
	else if typecheck_fail("Ebool",ah) then 
		let while_ah = While(Not(Val(Den "ah")),[Assign(Den "aah",Ebool false);Assign(Den "ah",Ebool true)])
		in while_ah
	else if typecheck_fail("Estring",ah) then 
		let while_ah = While(Not(Eq(Len(Val(Den "ah")),Eint 0)),[Cifthenelse(Not(Eq(Len(Val(Den "ah")),Eint 1)),[Assign(Den "aah",Concat(Val(Den "aah"),Substr(Val(Den "ah"),Eint 0,Eint 0)));Assign(Den "ah",Substr(Val(Den "ah"),Eint 1,Diff(Len(Val(Den "ah")),Eint 1)))],[Assign(Den "aah",Concat(Val(Den "aah"),Substr(Val(Den "ah"),Eint 0,Eint 0)));Assign(Den "ah",Estring "")])])
		in while_ah
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER LA COPIA DELLA VARIABILE - AAl *)
let init_while_aal =
	if typecheck_fail("Eint",al) then 
		let while_al = While(Not(Eq(Val(Den "al"),Eint 0)),[Assign(Den "aal",Sum(Val(Den "aal"),Eint 1));Assign(Den "al",Diff(Val(Den "al"),Eint 1))])
		in while_al
	else if typecheck_fail("Ebool",al) then 
		let while_al = While(Not(Val(Den "al")),[Assign(Den "aal",Ebool false);Assign(Den "al",Ebool true)])
		in while_al
	else if typecheck_fail("Estring",al) then 
		let while_al = While(Not(Eq(Len(Val(Den "al")),Eint 0)),[Cifthenelse(Not(Eq(Len(Val(Den "al")),Eint 1)),[Assign(Den "aal",Concat(Val(Den "aal"),Substr(Val(Den "al"),Eint 0,Eint 0)));Assign(Den "al",Substr(Val(Den "al"),Eint 1,Diff(Len(Val(Den "al")),Eint 1)))],[Assign(Den "aal",Concat(Val(Den "aal"),Substr(Val(Den "al"),Eint 0,Eint 0)));Assign(Den "al",Estring "")])])
		in while_al
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER LA COPIA DELLA VARIABILE - BBh *)
let init_while_bbh = 
	if typecheck_fail("Eint",bh) then 
		let while_bh = While(Not(Eq(Val(Den "bh"),Eint 0)),[Assign(Den "bbh",Sum(Val(Den "bbh"),Eint 1));Assign(Den "bh",Diff(Val(Den "bh"),Eint 1))])
		in while_bh
	else if typecheck_fail("Ebool",bh) then 
		let while_bh = While(Not(Val(Den "bh")),[Assign(Den "bbh",Ebool false);Assign(Den "bh",Ebool true)])
		in while_bh
	else if typecheck_fail("Estring",bh) then 
		let while_bh = While(Not(Eq(Len(Val(Den "bh")),Eint 0)),[Cifthenelse(Not(Eq(Len(Val(Den "bh")),Eint 1)),[Assign(Den "bbh",Concat(Val(Den "bbh"),Substr(Val(Den "bh"),Eint 0,Eint 0)));Assign(Den "bh",Substr(Val(Den "bh"),Eint 1,Diff(Len(Val(Den "bh")),Eint 1)))],[Assign(Den "bbh",Concat(Val(Den "bbh"),Substr(Val(Den "bh"),Eint 0,Eint 0)));Assign(Den "bh",Estring "")])])
		in while_bh
	else 
		failwith ("not a valid type")
;;

(* TYPECHECK PER LA COPIA DELLA VARIABILE - BBl *)
let init_while_bbl = 
	if typecheck_fail("Eint",bl) then 
		let while_bl = While(Not(Eq(Val(Den "bl"),Eint 0)),[Assign(Den "bbl",Sum(Val(Den "bbl"),Eint 1));Assign(Den "bl",Diff(Val(Den "bl"),Eint 1))])
		in while_bl
	else if typecheck_fail("Ebool",bl) then 
		let while_bl = While(Not(Val(Den "bl")),[Assign(Den "bbl",Ebool false);Assign(Den "bl",Ebool true)])
		in while_bl
	else if typecheck_fail("Estring",bl) then 
		let while_bl = While(Not(Eq(Len(Val(Den "bl")),Eint 0)),[Cifthenelse(Not(Eq(Len(Val(Den "bl")),Eint 1)),[Assign(Den "bbl",Concat(Val(Den "bbl"),Substr(Val(Den "bl"),Eint 0,Eint 0)));Assign(Den "bl",Substr(Val(Den "bl"),Eint 1,Diff(Len(Val(Den "bl")),Eint 1)))],[Assign(Den "bbl",Concat(Val(Den "bbl"),Substr(Val(Den "bl"),Eint 0,Eint 0)));Assign(Den "bl",Estring "")])])
		in while_bl
	else 
		failwith ("not a valid type")
;;

(* AVVIO COPIA DELLA VARIABILE - AAh *)
let while_ah = init_while_aah;;
let sigma    = semc while_ah rho sigma;;

(* AVVIO COPIA DELLA VARIABILE - AAl *)
let while_al = init_while_aal;;
let sigma    = semc while_al rho sigma;;

(* AVVIO COPIA DELLA VARIABILE - BBh *)
let while_bh = init_while_bbh;;
let sigma    = semc while_bh rho sigma;;

(* AVVIO COPIA DELLA VARIABILE - BBl *)
let while_bl = init_while_bbl;;
let sigma    = semc while_bl rho sigma;;

(* COMPILATORE FAIL *)
#use "fail/semantica_fail.ml";;

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

sem (Estring "AAh --> untainted") rho sigma;;
sem (Val(Den "aah")) rho sigma;;

sem (Estring "AAl --> untainted") rho sigma;;
sem (Val(Den "aal")) rho sigma;;

sem (Estring "BBh --> untainted") rho sigma;;
sem (Val(Den "bbh")) rho sigma;;

sem (Estring "BBl --> untainted") rho sigma;;
sem (Val(Den "bbl")) rho sigma;;

s;;
sem (Val(Den "result_taint")) rho sigma;;
let sigma = semc_fail s rho sigma;;

(* sem (Val(Den "w")) rho sigma;; *)
(* sem (Val(Den "z")) rho sigma;; *)
(* #use "reflect_taint.ml";;      *)

[@@@ocaml.warning "+26"]
[@@@ocaml.warning "+8"]
