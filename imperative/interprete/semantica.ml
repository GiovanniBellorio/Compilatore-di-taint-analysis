(*
##################################################
##################################################
############### semantica.ml #####################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Semantica delle espressioni
# Nome File     : semantica.ml
# ------------------------------------------------ 
#
*)

(* SEMANTICS: LINK EVERY FUNCTION TAG WITH THE CORRECT OPERATION *)
let rec sem (e:exp) (r:dval env) (s:mval store) = match e with
  | Eint(n)   -> Int(n)
  | Ebool(b)  -> Bool(b)
  | Den(i)    -> dvaltoeval(applyenv(r,i))
  | Iszero(a) -> iszero((sem a r s))
  | Eq(a,b)   -> equ((sem a r s) , (sem b r s))
  | Prod(a,b) -> mult((sem a r s), (sem b r s))
  | Sum(a,b)  -> plus((sem a r s), (sem b r s))
  | Diff(a,b) -> diff((sem a r s), (sem b r s))
  | Minus(a)  -> minus((sem a r s))
  | And(a,b)  -> et((sem a r s), (sem b r s))
  | Or(a,b)   -> vel((sem a r s), (sem b r s))
  | Not(a)    -> non((sem a r s))

  | Estring(c)    -> String(c)
  | Cstring(c)    -> string_cc(c)
  | Len(c)        -> len(sem c r s)
  | Concat(c1,c2) -> concat(sem c1 r s, sem c2 r s)
  | Substr(c,a,b) -> substr(sem c r s, sem a r s, sem b r s)

  | Ifthenelse(a,b,c) -> let g = sem a r s in
      if typecheck("bool",g) then
        (if g = Bool(true) then sem b r s else sem c r s)
      else failwith ("nonboolean guard")
      
  | Val(e) -> let (v, s1) = semden e r s in (match v with
    | Dloc n -> mvaltoeval(applystore(s1, n))
    | _ -> failwith("not a variable"))
      
  | Let(i,e1,e2) -> let (v, s1) = semden e1 r s in sem e2 (bind (r ,i, v)) s1
  | Fun(i,e1)    -> dvaltoeval(makefun(e,r))
  | Appl(a,b)    -> let (v1, s1) = semlist b r s in applyfun(evaltodval(sem a r s), v1, s1)
  | Rec(i,e1)    -> makefunrec(i, e1, r)
  | _            -> failwith ("nonlegal expression for sem")

(* definizione della semantica delle espressioni che restituiscono un valore denotabile *)
and semden (e:exp) (r:dval env) (s:mval store) = match e with
  | Den(i)     -> (applyenv(r,i), s)
  | Fun(i,e1)  -> (makefun(e, r), s)
  | Proc(il,b) -> (makeproc(e, r), s)
  | Newloc(e)  -> let m = evaltomval(sem e r s) in let (l, s1) = allocate(s, m) in (Dloc l, s1)
  | _          -> (evaltodval(sem e r s), s)

(* definizione della semantica dei comandi *)
and semc (c:com) (r:dval env) (s:mval store) = match c with
  | Assign(e1,e2) -> let (v1,s1) = semden e1 r s in (match v1 with
    | Dloc(n) -> update(s, n, evaltomval(sem e2 r s))
    | _ -> failwith ("wrong location in assignment"))

  (* comando if then else *)
  | Cifthenelse(e,cl1,cl2) -> let g = sem e r s in
      if typecheck("bool",g) then
        (if g = Bool(true) then semcl cl1 r s else semcl cl2 r s)
      else failwith ("nonboolean guard")

  (* comando while *)
  | While(e,cl) ->
        let functional ((fi: mval store -> mval store)) =
          function sigma ->
            let g = sem e r sigma in
              if typecheck("bool",g) then
                (if g = Bool(true) then fi(semcl cl r sigma) else sigma)
              else failwith ("nonboolean guard")
        in
        let rec ssfix = function x -> functional ssfix x in ssfix(s)

  (* blocco *)
  | Block(dl,cl) -> semb (dl,cl) r s
    
  (* chiamata di una funzione *)
  | Call(e1,e2) -> let (z, s1) = semden e1 r s in let (v, s2) = semlist e2 r s1 in applyproc(z, v, s2)

  (* FUNCTION FOR VALUATE AN INPUT STRING e *)
  | Reflect(e)   -> let g = sem e r s in
        (* CHECK THE FIRST TOKEN:
           IF ITS A VALID COMMAND             -> EXECUTE THE FIRST BLOCK  (parserCom)
           IF ITS NOT A COMMAND BUT IS VALID  -> EXECUTE THE SECOND BLOCK (parser)
           OTHERWISE                          -> FAIL BECAUSE STRING IS NOT VALID
        *)
        (* FIRST BLOCK - parserCom *)
        if ( typecheck("string",g) && maggioreuguale(len(g),Int(5)) && iscom(g) ) then
          let stack = emptystack(100,Undefinedstack) in     (* String Stack*)
          let stackstr = emptystack(100,Novalue) in         (* Operation Stack*)
          let com = parsericcom(g,stack,stackstr) in
          if (equalsInt(scanCC(g,String("(")),scanCC(g,String(")")))) then
            semc com r s
          else
            failwith ("parse ric error or command not valid")
        
        (* SECOND BLOCK - parser *)
        else if ( typecheck("string",g) && maggioreuguale(len(g),Int(5)) ) then
          let stack = emptystack(100,Undefinedstack) in     (* String Stack*)
          let stackstr = emptystack(100,Novalue) in         (* Operation Stack*)
          let exp = parseric(g,stack,stackstr) in
          (* If String stack is empty the result is OK *)
          if ( (empty(stackstr) || equals(substr(top(stackstr),Int(0),Int(0)),String(")"))) && (equalsInt(scanCC(g,String("(")),scanCC(g,String(")")))) ) then
            semc (Assign(Den "result",exp)) r s
          else
            failwith ("parse ric error or command not valid")
        
        (* OTHERWISE *)
        else failwith ("string not valid")

(* definizione della semantica di una lista di comandi *)
and semcl cl r s = match cl with
  | [] -> s
  | c::cl1 -> semcl cl1 r (semc c r s)

(* definizione della semantica di un blocco *)
and semb (dl, cl) r s =
  let (r1, s1) = semdv dl r s in semcl cl r1 s1

(* definizione della semantica delle dichiazioni di lista denotabile *) 
and semdl (dl, rl) r s =
  let (r1, s1) = semdv dl r s in semdr rl r1 s1

(* definizione della semantica per rompere un ambiente *)
and semdr rl r s =
  let functional ((r1:dval env)) = (match rl with
    | [] -> r
    | (i,e)::rl1 -> let (v, s2) = semden e r1 s in let (r2, s3) = semdr rl1 (bind(r, i, v)) s in r2) in 
      let rec rfix = function x -> functional x
      in (r, s)

(* definizione della semantica delle dichiazioni di variabili *)
and semdv dl r s = match dl with
  | [] -> (r,s)
  | (i,e)::dl1 -> let (v, s1) = semden e r s in semdv dl1 (bind(r, i, v)) s1

(* definizione della semantica delle funzioni *)
and makefun ((a:exp),(x:dval env))  = (match a with
  | Fun(ii,aa) -> Dfunval(function (d, s) -> sem aa (bindlist (x, ii, d)) s)
  | _ -> failwith ("Non-functional object"))

(* definizione della semantica di lista *)
and semlist el r s = match el with
  | [] -> ([], s)
  | e::el1 -> let (v1, s1) = semden e r s in let (v2, s2) = semlist el1 r s1 in (v1 :: v2, s2)

(* definizione della semantica per applicare una funzione *)
and applyfun ((ev1:dval),(ev2:dval list), s) = match ev1 with
  | Dfunval(x) -> x (ev2, s)
  | _ -> failwith ("attempt to apply a non-functional object")

(* definizione della semantica per creare una funzione ricorsiva *)
and makefunrec (i, Fun(ii, aa), r) =
  let functional ff (d, s1) =
    let r1 = bind(bindlist(r, ii, d), i, Dfunval(ff)) in sem aa r1 s1 in
    let rec fix = function x -> functional fix x in Funval(fix)

(* definizione della semantica delle procedure *)
and makeproc ((a:exp),(x:dval env)) = match a with
  | Proc(ii,b) -> Dprocval(function (d, s) -> semb b (bindlist (x, ii, d)) s)
  | _ -> failwith ("Non-functional object")

(* definizione della semantica per applicare una procedura *)
and applyproc ((ev1:dval),(ev2:dval list), s) = match ev1 with
  | Dprocval(x) -> x (ev2, s)
  | _ -> failwith ("attempt to apply a non-functional object")

(* taint analysis semantica espressioni *)
and taint ((e:exp),(r:dval env),(s:mval store)) = match e with
  (* case base - RX *)
  | Den(i)    ->  if (i = "ah" || i = "al" || i = "bh" || i = "bl" || i = "ax" || i = "bx") then
                    let s1 = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s1)
                  else
                    let s2 = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s2)
  | Val(e)    ->  taint(e,r,s)
    
  (* arithmetical instructions *)
  | Prod(a,b) ->  prod_arithmetical_taint(taint(a,r,s),taint(b,r,s),r,s,a,b)
  | Sum(a,b)  ->  sum_arithmetical_taint(taint(a,r,s),taint(b,r,s),r,s,a,b)
  | Diff(a,b) ->  sum_arithmetical_taint(taint(a,r,s),taint(b,r,s),r,s,a,b)
  | Minus(a)  ->  taint(a,r,s)
  | Eq(a,b)   ->  operands_taint(taint(a,r,s),taint(b,r,s),r,s)
  | Iszero(a) ->  taint(a,r,s)

  (* string instructions *)
  | Len(c)        ->  let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  | Concat(c1,c2) ->  string_taint(taint(c1,r,s),taint(c2,r,s),r,s)
  | Substr(c,a,b) ->  let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)

  (* boolean instructions *)
  | Or(a,b)   ->  boolean_or_taint(taint(a,r,s),taint(b,r,s),r,s,a,b)
  | And(a,b)  ->  boolean_and_taint(taint(a,r,s),taint(b,r,s),r,s,a,b)
  | Not(a)    ->  taint(a,r,s)

  (* if instructions *)
  | Ifthenelse(a,b,c) ->  conditional_taint(taint(a,r,s),taint(b,r,s),taint(c,r,s),r,s,a)
  | Let(i,e1,e2)      ->  operands_taint(taint(e1,r,s),taint(e2,r,s),r,s)

  (* loc instructions *)
  | Newloc(e) ->  taint(e,r,s)          
     
  (* function instructions *)
  | Fun(i,e1) ->  taint(e1,r,s)
  | Appl(a,b) ->  taintlist(b,r,s)
  | Rec(i,e1) ->  taint(e1,r,s)          

  (* failwith *)
  | _           -> failwith ("nonlegal expression for taint analysis")

(* taint analysis semantica lista espressioni *)
and taintlist ((e:exp list),(r:dval env),(s:mval store)) = match e with
  | []   -> (r,s)
  | h::t -> let (r,s) = taint(h,r,s) in 
            if equals((sem (Val(Den "result_taint")) r s),String("tainted")) then
              (r,s)
            else
              let (r,s) = taintlist(t,r,s) in (r,s)

(* taint analysis semantica comandi *)
and taintc ((c:com),(r:dval env),(s:mval store)) = match c with
  | Assign(e1,e2)           ->  operands_taint(taint(e1,r,s),taint(e2,r,s),r,s)  
  | Cifthenelse(e,cl1,cl2)  ->  conditional_taint(taint(e,r,s),taintcomlist(cl1,r,s),taintcomlist(cl2,r,s),r,s,e)
  | While(e,cl)             ->  operands_taint(taint(e,r,s),taintcomlist(cl,r,s),r,s)
  | Call(e1,e2)             ->  operands_taint(taint(e1,r,s),taintlist(e2,r,s),r,s)
  | Reflect(e)              ->  let g = sem e r s in
                                (* com *)
                                if ( typecheck("string",g) && maggioreuguale(len(g),Int(5)) && iscom(g) ) then
                                  let stack = emptystack(100,Undefinedstack) in
                                  let stackstr = emptystack(100,Novalue) in
                                  let com = parsericcom(g,stack,stackstr) in
                                  if (equalsInt(scanCC(g,String("(")),scanCC(g,String(")")))) then
                                    taintc(com,r,s)
                                  else
                                    failwith ("taint reflect parse ric error or command not valid")
        
                                (* exp *)
                                else if ( typecheck("string",g) && maggioreuguale(len(g),Int(5)) ) then
                                  let stack = emptystack(100,Undefinedstack) in
                                  let stackstr = emptystack(100,Novalue) in
                                  let exp = parseric(g,stack,stackstr) in
                                  if ( (empty(stackstr) || equals(substr(top(stackstr),Int(0),Int(0)),String(")"))) && (equalsInt(scanCC(g,String("(")),scanCC(g,String(")")))) ) then
                                    taint(exp,r,s)
                                  else
                                    failwith ("taintc reflect parse ric error or command not valid")
        
                                (* ... *)
                                else failwith ("taint reflect string not valid")

(* taint analysis semantica lista comandi *)
and taintcomlist ((c:com list),(r:dval env),(s:mval store)) = match c with
  | []   -> (r,s)
  | h::t -> let (r,s) = taintc(h,r,s) in 
            if equals((sem (Val(Den "result_taint")) r s),String("tainted")) then
              (r,s)
            else
              let (r,s) = taintcomlist(t,r,s) in (r,s)

(* taint analysis semantica lista dichiarazioni *)
and taintdv (dl,(r:dval env),(s:mval store)) = match dl with
  | []   -> (r,s)
  | ((i:ide),(e:exp))::t -> let (r,s) = taint(e,r,s) in 
                            if equals((sem (Val(Den "result_taint")) r s),String("tainted")) then
                              (r,s)
                            else
                              let (r,s) = taintdv(t,r,s) in (r,s)

(* --- FUNCTIONS FOR TAINT ANALYSIS - START --- *)

and sum_arithmetical_taint (x,y,r,s,a,b) =
  let (r,s1) = x in
  let (r,s2) = y in
  (* 0 *)
  if equals((sem (Val(Den "result_taint")) r s1),String("untainted")) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) && (sem b r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && equals((sem (Val(Den "result_taint")) r s2),String("untainted")) && (sem a r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("untainted")) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) && (sem a r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && equals((sem (Val(Den "result_taint")) r s2),String("untainted")) && (sem b r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    operands_taint (x,y,r,s)

and prod_arithmetical_taint (x,y,r,s,a,b) =
  let (r,s1) = x in
  let (r,s2) = y in
  (* 0 *)
  if equals((sem (Val(Den "result_taint")) r s1),String("untainted")) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) && (sem b r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && equals((sem (Val(Den "result_taint")) r s2),String("untainted")) && (sem a r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("untainted")) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) && (sem a r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && equals((sem (Val(Den "result_taint")) r s2),String("untainted")) && (sem b r s2) = Int(0) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  (* 1 *)
  else if equals((sem (Val(Den "result_taint")) r s1),String("untainted")) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) && (sem b r s2) = Int(1) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && equals((sem (Val(Den "result_taint")) r s2),String("untainted")) && (sem a r s2) = Int(1) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("untainted")) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) && (sem a r s2) = Int(1) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && equals((sem (Val(Den "result_taint")) r s2),String("untainted")) && (sem b r s2) = Int(1) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    operands_taint (x,y,r,s)

and operands_taint (x,y,r,s) =
  let (r,s1) = x in
  let (r,s2) = y in
  if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) || equals((sem (Val(Den "result_taint")) r s2),String("tainted")) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)

and boolean_or_taint (x,y,r,s,a,b) = 
  let (r,s1) = x in
  let (r,s2) = y in
  if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && (sem b r s2) = Bool(true) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && (sem b r s2) = Bool(false) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)

and boolean_and_taint (x,y,r,s,a,b) = 
  let (r,s1) = x in
  let (r,s2) = y in
  if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && (sem b r s2) = Bool(false) then
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)
  else if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) && (sem b r s2) = Bool(true) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)

and string_taint (x,y,r,s) = 
  let (r,s1) = x in
  let (r,s2) = y in
  if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) || equals((sem (Val(Den "result_taint")) r s2),String("tainted")) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)

and conditional_taint (x,y,z,r,s,a) = 
  let (r,s1) = x in
  let (r,s2) = y in
  let (r,s3) = z in
  if equals((sem (Val(Den "result_taint")) r s1),String("tainted")) then
    let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
  else
    if sem a r s = Bool(true) && equals((sem (Val(Den "result_taint")) r s2),String("tainted")) then
      let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
    else if sem a r s = Bool(false) && equals((sem (Val(Den "result_taint")) r s3),String("tainted")) then
      let s = semc (Assign(Den "result_taint",Estring "tainted")) r s in (r,s)
    else
      let s = semc (Assign(Den "result_taint",Estring "untainted")) r s in (r,s)

(* --- FUNCTIONS FOR TAINT ANALYSIS - END --- *)
