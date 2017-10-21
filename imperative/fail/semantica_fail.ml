(*
##################################################
##################################################
############# semantica_fail.ml ##################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Semantica delle espressioni fail
# Nome File     : semantica_fail.ml
# ------------------------------------------------ 
#
*)

(* definizione ricorsiva della semantica delle espressioni che restituiscono un valore esprimibile *)
let rec sem_fail (e:exp) (r:dval env) (s:mval store) = match e with
  | Eint(n)   -> Int(n)
  | Ebool(b)  -> Bool(b)
  | Den(i)    -> if (i = "ah") then
                    dvaltoeval(applyenv(r,"aah"))
                 else if (i = "al") then
                    dvaltoeval(applyenv(r,"aal"))
                 else if (i = "ax") then
                    dvaltoeval(applyenv(r,"aax"))
                 else if (i = "bh") then
                    dvaltoeval(applyenv(r,"bbh"))
                 else if (i = "bl") then
                    dvaltoeval(applyenv(r,"bbl"))
                 else if (i = "bx") then
                    dvaltoeval(applyenv(r,"bbx"))
                 else 
                    dvaltoeval(applyenv(r,i))
  | Iszero(a) -> iszero((sem_fail a r s))
  | Eq(a,b)   -> equ((sem_fail a r s) , (sem_fail b r s))
  | Prod(a,b) -> mult((sem_fail a r s), (sem_fail b r s))
  | Sum(a,b)  -> plus((sem_fail a r s), (sem_fail b r s))
  | Diff(a,b) -> diff((sem_fail a r s), (sem_fail b r s))
  | Minus(a)  -> minus((sem_fail a r s))
  | And(a,b)  -> et((sem_fail a r s), (sem_fail b r s))
  | Or(a,b)   -> vel((sem_fail a r s), (sem_fail b r s))
  | Not(a)    -> non((sem_fail a r s))

  | Estring(c)    -> String(c)
  | Cstring(c)    -> string_cc(c)
  | Len(c)        -> len(sem_fail c r s)
  | Concat(c1,c2) -> concat(sem_fail c1 r s, sem_fail c2 r s)
  | Substr(c,a,b) -> substr(sem_fail c r s, sem_fail a r s, sem_fail b r s)

  | Ifthenelse(a,b,c) -> let g = sem_fail a r s in
      if typecheck("bool",g) then
        (if g = Bool(true) then sem_fail b r s else sem_fail c r s)
      else failwith ("nonboolean guard")
      
  | Val(e) -> let (v, s1) = semden_fail e r s in (match v with
    | Dloc n -> mvaltoeval(applystore(s1, n))
    | _ -> failwith("not a variable"))
      
  | Let(i,e1,e2) -> let (v, s1) = semden_fail e1 r s in sem_fail e2 (bind (r ,i, v)) s1
  | Fun(i,e1)    -> dvaltoeval(makefun(e,r))
  | Appl(a,b)    -> let (v1, s1) = semlist_fail b r s in applyfun(evaltodval(sem_fail a r s), v1, s1)
  | Rec(i,e1)    -> makefunrec(i, e1, r)
  | _            -> failwith ("nonlegal expression for sem")

(* definizione della semantica delle espressioni che restituiscono un valore denotabile *)
and semden_fail (e:exp) (r:dval env) (s:mval store) = match e with
  | Den(i)     -> if (i = "ah") then
                    (applyenv(r,"aah"), s)
                  else if (i = "al") then
                    (applyenv(r,"aal"), s)
                  else if (i = "ax") then
                    (applyenv(r,"aax"), s)
                  else if (i = "bh") then
                    (applyenv(r,"bbh"), s)
                  else if (i = "bl") then
                    (applyenv(r,"bbl"), s)
                  else if (i = "bx") then
                    (applyenv(r,"bbx"), s)
                  else 
                    (applyenv(r,i), s)
  | Fun(i,e1)  -> (makefun(e, r), s)
  | Proc(il,b) -> (makeproc(e, r), s)
  | Newloc(e)  -> let m = evaltomval(sem_fail e r s) in let (l, s1) = allocate(s, m) in (Dloc l, s1)
  | _          -> (evaltodval(sem_fail e r s), s)

(* definizione della semantica dei comandi *)
and semc_fail (c:com) (r:dval env) (s:mval store) = match c with
  | Assign(e1,e2) -> let (v1,s1) = semden_fail e1 r s in (match v1 with
    | Dloc(n) -> update(s, n, evaltomval(sem_fail e2 r s))
    | _ -> failwith ("wrong location in assignment"))

  (* comando if then else *)
  | Cifthenelse(e,cl1,cl2) -> let g = sem_fail e r s in
      if typecheck("bool",g) then
        (if g = Bool(true) then semcl_fail cl1 r s else semcl_fail cl2 r s)
      else failwith ("nonboolean guard")

  (* comando while *)
  | While(e,cl) ->
        let functional ((fi: mval store -> mval store)) =
          function sigma ->
            let g = sem_fail e r sigma in
              if typecheck("bool",g) then
                (if g = Bool(true) then fi(semcl_fail cl r sigma) else sigma)
              else failwith ("nonboolean guard")
        in
        let rec ssfix = function x -> functional ssfix x in ssfix(s)

  (* blocco *)
  | Block(dl,cl) -> semb_fail (dl,cl) r s
    
  (* chiamata di una funzione *)
  | Call(e1,e2) -> let (z, s1) = semden_fail e1 r s in let (v, s2) = semlist_fail e2 r s1 in applyproc(z, v, s2)

  (* comando reflect *)
  | Reflect(e)   -> let g = sem_fail e r s in
        (* com *)
        if ( typecheck("string",g) && maggioreuguale(len(g),Int(5)) && iscom(g) ) then
          let stack = emptystack(100,Undefinedstack) in
          let stackstr = emptystack(100,Novalue) in
          let com = parsericcom(g,stack,stackstr) in
          if (equalsInt(scanCC(g,String("(")),scanCC(g,String(")")))) then
            semc_fail com r s
          else
            failwith ("parse ric error or command not valid")
        
        (* exp *)
        else if ( typecheck("string",g) && maggioreuguale(len(g),Int(5)) ) then
          let stack = emptystack(100,Undefinedstack) in
          let stackstr = emptystack(100,Novalue) in
          let exp = parseric(g,stack,stackstr) in
          if ( (empty(stackstr) || equals(substr(top(stackstr),Int(0),Int(0)),String(")"))) && (equalsInt(scanCC(g,String("(")),scanCC(g,String(")")))) ) then
            semc_fail (Assign(Den "result",exp)) r s
          else
            failwith ("parse ric error or command not valid")
        
        (* ... *)
        else failwith ("string not valid")

(* definizione della semantica di una lista di comandi *)
and semcl_fail cl r s = match cl with
  | [] -> s
  | c::cl1 -> semcl_fail cl1 r (semc_fail c r s)

(* definizione della semantica di un blocco *)
and semb_fail (dl,  cl) r s =
  let (r1, s1) = semdv_fail dl r s in semcl_fail cl r1 s1

(* definizione della semantica delle dichiazioni di lista denotabile *) 
and semdl_fail (dl, rl) r s =
  let (r1, s1) = semdv_fail dl r s in semdr_fail rl r1 s1

(* definizione della semantica per rompere un ambiente *)
and semdr_fail rl r s =
  let functional ((r1:dval env)) = (match rl with
    | [] -> r
    | (i,e)::rl1 -> let (v, s2) = semden_fail e r1 s in let (r2, s3) = semdr_fail rl1 (bind(r, i, v)) s in r2) in 
      let rec rfix = function x -> functional x
      in (r, s)

(* definizione della semantica delle dichiazioni di variabili *)
and semdv_fail dl r s = match dl with
  | [] -> (r,s)
  | (i,e)::dl1 -> let (v, s1) = semden_fail e r s in semdv_fail dl1 (bind(r, i, v)) s1

(* definizione della semantica delle funzioni *)
and makefun_fail ((a:exp),(x:dval env))  = (match a with
  | Fun(ii,aa) -> Dfunval(function (d, s) -> sem_fail aa (bindlist (x, ii, d)) s)
  | _ -> failwith ("Non-functional object"))

(* definizione della semantica di lista *)
and semlist_fail el r s = match el with
  | [] -> ([], s)
  | e::el1 -> let (v1, s1) = semden_fail e r s in let (v2, s2) = semlist_fail el1 r s1 in (v1 :: v2, s2)

(* definizione della semantica per applicare una funzione *)
and applyfun_fail ((ev1:dval),(ev2:dval list), s) = match ev1 with
  | Dfunval(x) -> x (ev2, s)
  | _ -> failwith ("attempt to apply a non-functional object")

(* definizione della semantica per creare una funzione ricorsiva *)
and makefunrec_fail (i, Fun(ii, aa), r) =
  let functional ff (d, s1) =
    let r1 = bind(bindlist(r, ii, d), i, Dfunval(ff)) in sem_fail aa r1 s1 in
    let rec fix = function x -> functional fix x in Funval(fix)

(* definizione della semantica delle procedure *)
and makeproc_fail ((a:exp),(x:dval env)) = match a with
  | Proc(ii,b) -> Dprocval(function (d, s) -> semb_fail b (bindlist (x, ii, d)) s)
  | _ -> failwith ("Non-functional object")

(* definizione della semantica per applicare una procedura *)
and applyproc_fail ((ev1:dval),(ev2:dval list), s) = match ev1 with
  | Dprocval(x) -> x (ev2, s)
  | _ -> failwith ("attempt to apply a non-functional object")
