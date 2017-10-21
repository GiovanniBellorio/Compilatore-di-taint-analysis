(*
##################################################
##################################################
############### operazioni.ml ####################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Operazioni
# Nome File     : operazioni.ml
# ------------------------------------------------ 
#
*)

(* SUPPORT FUNCTION - TYPECHECK *)
let typecheck (x, y) = match x with
    | "int" -> (match y with
      | Int(u) -> true
      | _ -> false)
    | "bool" -> (match y with
      | Bool(u) -> true
      | _ -> false)
    | "string" -> (match y with
      | String(u) -> true
      | _ -> false)
    | _ -> failwith ("not a valid type")

(* --- BASIC FUNCTIONS - START --- *)
(* COMPUTE THE OPPOSITE OF A NUMBER *)
let minus x = 
  if typecheck("int",x) then 
    (match x with | Int(y) -> Int(-y)
                  | _ -> failwith ("minus match error"))
  else 
    failwith ("minus type error")

(* COMPUTE IF A NUMBER IS EQUAL TO ZERO *)
let iszero x = 
  if typecheck("int",x) then 
    (match x with | Int(y) -> Bool(y=0)
                  | _ -> failwith ("iszero match error"))
  else 
    failwith ("iszero type error")

(* COMPUTE IF AN INT X IS EQUAL TO AN INT Y *)
let equ (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> Bool(u = w)
                      | _ -> failwith ("equ match error"))
  else 
    failwith ("equ type error")

(* COMPUTE A SUM OF TWO INTS *)
let plus (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> Int(u+w)
                      | _ -> failwith ("plus match error"))
  else 
    failwith ("plus type error")

(* COMPUTE A DIFFERNCE OF TWO INTS *)
let diff (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> Int(u-w)
                      | _ -> failwith ("diff match error"))
  else 
    failwith ("diff type error")

(* COMPUTE A MULTIPLICATION OF TWO INTS *)
let mult (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> Int(u*w)
                      | _ -> failwith ("mult match error"))
  else 
    failwith ("mult type error")

(* COMPUTE THE LOGIC OPERATION "AND" *)
let et (x,y) = 
  if typecheck("bool",x) && typecheck("bool",y) then 
    (match (x,y) with | (Bool(u), Bool(w)) -> Bool(u && w)
                      | _ -> failwith ("et match error"))
  else 
    failwith ("et type error")

(* COMPUTE THE LOGIC OPERATION "OR" *)
let vel (x,y) = 
  if typecheck("bool",x) && typecheck("bool",y) then 
    (match (x,y) with | (Bool(u), Bool(w)) -> Bool(u || w)
                      | _ -> failwith ("vel match error"))
  else 
    failwith ("vel type error")

(* COMPUTE THE LOGIC OPERATION "NOT" *)
let non x = 
  if typecheck("bool",x) then 
    (match x with | Bool(y) -> Bool(not y)
                  | _ -> failwith ("non match error"))
  else 
    failwith ("non type error")
(* --- BASIC FUNCTIONS - END --- *)

(* --- AUX FUNCTIONS - START --- *)
let maggiore (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> (u > w)
                      | _ -> failwith ("maggiore match error"))
  else 
    failwith ("maggiore type error")

let maggioreuguale (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> (u >= w)
                      | _ -> failwith ("maggioreuguale match error"))
  else 
    failwith ("maggioreuguale type error")

let minore (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> (u < w)
                      | _ -> failwith ("minore match error"))
  else 
    failwith ("minore type error")

let minoreuguale (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> (u <= w)
                      | _ -> failwith ("minoreuguale match error"))
  else 
    failwith ("minoreuguale type error")
(* --- AUX FUNCTIONS - END --- *)

(* --- OPERATIONS FUNCTIONS - START --- *)
(* CHECK IF A NUMBER IS EQUAL TO ZERO - like function iszero() but a bool-type is returned *)
let isnull x = 
  if typecheck("int",x) then 
    (match x with | Int(y) -> (y=0)
                  | _ -> failwith ("isnull match error"))
  else 
    failwith ("isnull type error")

let isnotnull x = 
  if typecheck("int",x) then 
    (match x with | Int(y) -> (y!=0)
                  | _ -> failwith ("isnotnull match error"))
  else 
    failwith ("isnotnull type error")

(*  CHECK IF TWO INTEGERS ARE EQUAL - a bool-type is returned  *)
let equalsInt (x,y) = 
  if typecheck("int",x) && typecheck("int",y) then 
    (match (x,y) with | (Int(u), Int(w)) -> (u = w)
                      | _ -> failwith ("equalsInt match error"))
  else 
    failwith ("equalsInt type error")

(* CHECK IF TWO STRINGS ARE EQUAL - like function streq() but a bool-type is returned *)
let equals (s1, s2) = 
  if typecheck("string",s1) && typecheck("string",s2) then
    (match (s1,s2) with | (String(u), String(w)) -> isnull(Int(String.compare (u) w))
                        | _ -> failwith ("equals match error"))
  else 
    failwith ("equals type error")
(* --- OPERATIONS FUNCTIONS - END --- *)

(* --- STRING FUNCTIONS - START --- *)
(* CHECK IF A STRING X HAS ONLY THESE CHARACTERS: [a - z], [A - Z], [0 - 9] *)
let isCharAlfanumeric x = 
  (* (Char > 'A') && (Char < 'Z')    OR    (Char > 'a') && (Char < 'z')    OR    (Char > '0') && (Char < '9') *)
  if ( ((int_of_char(x)>64) && (int_of_char(x)<91)) || ((int_of_char(x)>96) && (int_of_char(x)<123)) || ((int_of_char(x)>47) && (int_of_char(x)<58)) ) then
    true
  else
    false

let rec isStringAlfanumeric x = 
  let index = 0 in 
  let chaar = String.get (x) index in 
  let bool_chaar = isCharAlfanumeric( chaar ) in
  (*  IF current Char is alfanumeric AND "i" is not pointing at the end of the String
      then call function isAlfanumeric recursively
      else bool_c has the correct output *)
  if ( bool_chaar && index < (String.length x -1) ) then 
    isStringAlfanumeric(String.sub (x) (index+1) (String.length x -1) )
  else
    bool_chaar

let string_cc x = 
  if ( isStringAlfanumeric(x) ) then 
    (match x with x -> String(x))
  else
    failwith ("string not alfanumeric")

(* COMPUTE THE LENGTH OF THE STRING X *)
let len x =
  if typecheck("string",x) then 
    (match x with | String(x) -> Int(String.length x)
                  | _ -> failwith ("len match error"))
  else 
    failwith ("len type error")

(* CONCATENATE STRING X TO STRING Y *)
let concat (x,y) =
  if typecheck("string",x) && typecheck("string",y) then
    (match (x,y) with | (String(u), String(w)) -> String(String.concat (u) ["";w])
                      | _ -> failwith ("concat match error"))
  else 
    failwith ("concat type error")
 
(* CUT A STRING X AND FROM INDEX "i1" (included) TO INDEX "i2" (not included) *)
let substr (s,x,y) =
  if typecheck("string",s) && typecheck("int",x) && typecheck("int",y) && maggiore(len(s),Int(0)) && (x>=Int(0)) && (y>=Int(0)) && (x<=y) && (x<=len(s)) && (y<=len(s)) then
    (match (s,x,y) with | (String(z), Int(u), Int(w)) -> String(String.sub (z) u (w-u+1))
                        | _ -> failwith ("substr match error"))
  else 
    failwith ("substr type error")

(* RETURN THE CHAR IN Y POSITION OF THE STRING X *)
let charAt (x,y) = 
  if typecheck("string",x) && typecheck("int",y) then 
    (match (x,y) with | (String(x), Int(y)) -> String(String.sub (x) y 1)
                      | _ -> failwith ("charAt match error"))
  else 
    failwith ("charAt type error")
(* --- STRING FUNCTIONS - END --- *)

(* --- FUNCTIONS FOR REFLECT - START --- *)
(* COUNT THE OCCURRENCE OF A CHARACTER Y IN A STRING X *)
let scanCC (a,b) = 
  if typecheck("string",a) && typecheck("string",b) then 
    (match (a,b) with (String(x), String(y)) ->
      let tmp = 0 in
      let tmp2 = 0 in
      let i = 0 in
      let x_len = len(String(x)) in
      let rec loop ( Int(i),Int(tmp) ) = 
        if ( maggioreuguale(Int(i),x_len) ) then
          Int(tmp)
        else if ( equals( substr(String(x),Int(i),Int(i)), String(y)) ) then
          let tmp2 = plus(Int(tmp),Int(1)) in
          loop ( plus(Int(i),Int(1)), plus(Int(tmp),Int(1)) )
        else
          loop ( plus(Int(i),Int(1)), Int(tmp) ) 
      in loop ( Int(i), Int(tmp) )
    )
  else
    failwith ("scanCC type error")

(* CONVERTER FUNCTION - STRING-TO-INTEGER *)
let str_to_int (s) = 
  if typecheck("string",s) then
    (match (s) with String(u) -> int_of_string(u))
  else
    failwith ("str_to_int type error")

(* CONVERTER FUNCTION - STRING-TO-BOOLEAN *)
let str_to_bool (s) = 
  if typecheck("string",s) then
    (match (s) with String(u) -> bool_of_string(u))
  else
    failwith ("str_to_bool type error")

(* DO BOTH ISTRUCTIONS TOP AND POP *)
let topandpop s = 
  let top = top(s) in let pop = pop(s) in top

(* CHECK IF THE TAG S IS A VALID COMMAND *)
let iscom s = match s with String(g) ->
  if equals(substr(String(g),Int(0),Int(5)),String("Assign")) || 
     equals(substr(String(g),Int(0),Int(10)),String("Cifthenelse")) ||
     equals(substr(String(g),Int(0),Int(4)),String("While")) ||
     equals(substr(String(g),Int(0),Int(3)),String("Call")) ||
     equals(substr(String(g),Int(0),Int(6)),String("Reflect")) then
    true
  else
    false
(* --- FUNCTIONS FOR REFLECT - END --- *)

(* Function "parser" for Reflect - FUNCTIONS *)
let rec parseric (e,stack,stackstr) = match e with String(n) ->
  (* String is empty - then return it *)
  if ( isnull(len(String(n))) ) then 
    Estring n
  else if equals(String(n),String(")")) then 
    Estring n
  (* "," character is ignored *)
  else if equals(substr(String(n),Int(0),Int(0)),String(",")) then 
    parseric(String(String.sub (n) 1 (((String.length) n)-1)),stack,stackstr)
  (* ")" character is ignored *)
  else if equals(substr(String(n),Int(0),Int(0)),String(")")) && (String.length(n)!=1) then 
    parseric(String(String.sub (n) 1 (((String.length) n)-1)),stack,stackstr)
  (* check "()" *)
  else if (String.contains(n) '(') && (String.contains(n) ')') && (String.index(n) '(') = ((String.index(n) ')')-1) && (String.length(n)!=1) then
    failwith ("parse ric error or command not valid")

  (* Operations
      * If an operation type is recognized:
      * i1 -> push in op_stack the first terminal type. That is obtained from a recursive parser call.
      * i2 -> push in op_stack the second terminal type. (Needed only if the operation requires 2 or more parameters)
      * i3 -> push in op_stack the third terminal type. (Needed only if the operation requires 3 or more parameters)
      * Call the correct operation and take the parameters from the top of op_stack
  *)

  (* Operation Prod *)
  else if (((String.length) n)>=4) && equals(String(String.sub (n) 0 4),String("Prod")) then
    let i1 = push(parseric(String(String.sub (n) 5 (((String.length) n)-5)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Prod(topandpop(stack),topandpop(stack))
  (* Operation Sum *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Sum")) then
    let i1 = push(parseric(String(String.sub (n) 4 (((String.length) n)-4)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Sum(topandpop(stack),topandpop(stack))
  (* Operation Diff *)
  else if (((String.length) n)>=4) && equals(String(String.sub (n) 0 4),String("Diff")) then
    let i1 = push(parseric(String(String.sub (n) 5 (((String.length) n)-5)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Diff(topandpop(stack),topandpop(stack))
  (* Operation Minus *)
  else if (((String.length) n)>=5) && equals(String(String.sub (n) 0 5),String("Minus")) then
    let i1 = push(parseric(String(String.sub (n) 6 (((String.length) n)-6)),stack,stackstr), stack) in
    Minus(topandpop(stack))
  (* Operation Len *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Len")) then
    let i1 = push(parseric(String(String.sub (n) 4 (((String.length) n)-4)),stack,stackstr), stack) in
    Len(topandpop(stack))
  (* Operation Concat *)
  else if (((String.length) n)>=6) && equals(String(String.sub (n) 0 6),String("Concat")) then
    let i1 = push(parseric(String(String.sub (n) 7 (((String.length) n)-7)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Concat(topandpop(stack),topandpop(stack))
  (* Operation Substr *)
  else if (((String.length) n)>=6) && equals(String(String.sub (n) 0 6),String("Substr")) then
    let i1 = push(parseric(String(String.sub (n) 7 (((String.length) n)-7)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    let i3 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Substr(topandpop(stack),topandpop(stack),topandpop(stack))
  (* Operation Eq *)
  else if (((String.length) n)>=2) && equals(String(String.sub (n) 0 2),String("Eq")) then
    let i1 = push(parseric(String(String.sub (n) 3 (((String.length) n)-3)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Eq(topandpop(stack),topandpop(stack))
  (* Operation Iszero *)
  else if (((String.length) n)>=6) && equals(String(String.sub (n) 0 6),String("Iszero")) then
    let i1 = push(parseric(String(String.sub (n) 7 (((String.length) n)-7)),stack,stackstr), stack) in
    Iszero(topandpop(stack))
  (* Operation Or *)
  else if (((String.length) n)>=2) && equals(String(String.sub (n) 0 2),String("Or")) then
    let i1 = push(parseric(String(String.sub (n) 3 (((String.length) n)-3)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Or(topandpop(stack),topandpop(stack))
  (* Operation And *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("And")) then
    let i1 = push(parseric(String(String.sub (n) 4 (((String.length) n)-4)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    And(topandpop(stack),topandpop(stack))
  (* Operation Not *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Not")) then
    let i1 = push(parseric(String(String.sub (n) 4 (((String.length) n)-4)),stack,stackstr), stack) in
    Not(topandpop(stack))
  (* Operation Ifthenelse *)
  else if (((String.length) n)>=10) && equals(String(String.sub (n) 0 10),String("Ifthenelse")) then
    let i1 = push(parseric(String(String.sub (n) 11 (((String.length) n)-11)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    let i3 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Ifthenelse(topandpop(stack),topandpop(stack),topandpop(stack))
  (* Operation Let *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Let")) then
    let i1 = (String.sub (n) 4 (((String.index(n) ',')-4)-1)) in
    let l  = push(String(String.sub (n) (String.index(n) ',') (((String.length) n)-(String.index(n) ','))), stackstr) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    let i3 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Let(i1,topandpop(stack),topandpop(stack))
  (* Operation Val *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Val")) then
    let i1 = push(parseric(String(String.sub (n) 4 (((String.length) n)-4)),stack,stackstr), stack) in
    Val(topandpop(stack))
  (* Operation Newloc *)
  else if (((String.length) n)>=6) && equals(String(String.sub (n) 0 6),String("Newloc")) then
    let i1 = push(parseric(String(String.sub (n) 7 (((String.length) n)-7)),stack,stackstr), stack) in
    Newloc(topandpop(stack))
  (* Operation Fun *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Fun")) then
    let i1 = (String.sub (n) ((String.index(n) '[')+1) ((String.index(n) ']')-1)) in
    let l  = push(String(String.sub (n) ((String.index(n) ']')+1) (((String.length) n)-((String.index(n) ']')+1))), stackstr) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Fun([i1],topandpop(stack))
  (* Operation Appl *)
  else if (((String.length) n)>=4) && equals(String(String.sub (n) 0 4),String("Appl")) then
    let i1 = push(parseric(String(String.sub (n) 5 (((String.length) n)-5)),stack,stackstr), stack) in
    let i2 = parsericlist(topandpop(stackstr),stack,stackstr) in
    Appl(topandpop(stack),i2)
  (* Operation Rec *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Rec")) then
    let i1 = (String.sub (n) 4 (((String.index(n) ',')-4)-1)) in
    let l  = push(String(String.sub (n) (String.index(n) ',') (((String.length) n)-(String.index(n) ','))), stackstr) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Rec(i1,topandpop(stack))

  (* Terminal types [Den, Eint, Ebool, Estring]
      * If a terminal type is recognized:
      * First block: check if is an operation first operator, before a char "," and a char ")" -> otherwise run second block
      * Second block: check if is an operation second operator, before only char ")" -> otherwise run third block
      * Third block: is alone
  *)

  (* Den type *)
  else if (((String.length) n)>=3) && equals(String(String.sub (n) 0 3),String("Den")) then
    if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
      (* Den: first block *)
      let i = push(Den(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)), stack) in
      let l = push(substr(String(n),Int(((String.index(n) ',')+1)),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else if (String.contains(n) ')') then
      (* Den: second block *)
      let i = push(Den(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)), stack) in
      let l = push(substr(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else
      (* Den: third block *)
      let i = push(Den(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)), stack) in
      topandpop(stack)
  (* Eint type *)
  else if (((String.length) n)>=4) && equals(String(String.sub (n) 0 4),String("Eint")) then
    if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
      (* Eint: first block *)
      let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)))), stack) in
      let l = push(substr(String(n),Int(((String.index(n) ',')+1)),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else if ((String.contains(n) ')')) then
      (* Eint: second block *)
      let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)))), stack) in
      let l = push(substr(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else
      (* Eint: third block *)
      let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)))), stack) in
      topandpop(stack)
  (* Ebool type *)
  else if (((String.length) n)>=5) && equals(String(String.sub (n) 0 5),String("Ebool")) then
    if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
      (* Ebool: first block *)
      let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)))), stack) in
      let l = push(substr(String(n),Int(((String.index(n) ',')+1)),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else if ((String.contains(n) ')')) then
      (* Ebool: second block *)
      let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)))), stack) in
      let l = push(substr(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else
      (* Ebool: third block *)
      let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)))), stack) in
      topandpop(stack)
  (* Estring type *)
  else if (((String.length) n)>=7) && equals(String(String.sub (n) 0 7),String("Estring")) then
    if (String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')')) then
      (* Estring: first block *)
      let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)), stack) in
      let l = push(substr(String(n),Int(((String.index(n) ',')+1)),diff(len(String(n)),Int(1))), stackstr) in 
      topandpop(stack)
    else if ((String.contains(n) ')')) then
      (* Estring: second block *)
      let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)), stack) in
      let l = push(substr(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), stackstr) in
      topandpop(stack)
    else
      (* Estring: third block *)
      let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)), stack) in
      topandpop(stack)
  
  (* No command recognized *)
  else
    failwith ("parse ric error or command not valid")

(* Interpretate list for the function "parser" *)
and parsericlist (e,stack,stackstr) = match e with String(n) ->
  let listcom = [] in
      (* CHECK: IF STRING CONTAINS A '[' AND A ';' -> ITS THE FIRST ELEMENT
       *        IF STRING CONTAINS A ';'           -> ITS AN INTERMEDIATE ELEMENT
       *        IF STRING CONTAINS A ']'           -> ITS THE LAST ELEMENT
       * IF A "LAST ELEMENT" IS ENCOUNTERED, DO NOT USE A RECURSIVE CALL
       *)
  if (String.contains(n) '[') && ((String.index(n) '[')<(String.index(n) ';')) then
    let i1 = ( String.sub (n) ((String.index(n) '[')+1) (((String.length) n)-(String.index(n) '[')-1) ) in
    let i2 = push(String(i1), stackstr) in
    let l1 = parseric(String(i1),stack,stackstr) in
    let l2 = parsericlist(topandpop(stackstr),stack,stackstr) in
    let l = l1 :: l2 in l
  else if (String.contains(n) ';') && ((String.index(n) ';')<(String.index(n) ']')) then
    let i1 = ( String.sub (n) ((String.index(n) ';')+1) (((String.length) n)-(String.index(n) ';')-1) ) in
    let i2 = push(String(i1), stackstr) in
    let l1 = parseric(String(i1),stack,stackstr) in
    let l2 = parsericlist(topandpop(stackstr),stack,stackstr) in
    let l = l1 :: l2 in l
  else
    let i1 = ( String.sub (n) ((String.index(n) ']')+1) (((String.length) n)-(String.index(n) ']')-1) ) in
    let i2 = push(String(i1), stackstr) in
    listcom

(* Function "parser" for Reflcet - COMMANDS *)
let rec parsericcom (e,stack,stackstr) = match e with String(n) ->
  (* "," character is ignored *)
  if equals(substr(String(n),Int(0),Int(0)),String(",")) then 
    parsericcom(String(String.sub (n) 1 (((String.length) n)-1)),stack,stackstr)
  (* ")" character is ignored *)
  else if equals(substr(String(n),Int(0),Int(0)),String(")")) && (String.length(n)!=1) then 
    parsericcom(String(String.sub (n) 1 (((String.length) n)-1)),stack,stackstr)
  (* check "()" *)
  else if (String.contains(n) '(') && (String.contains(n) ')') && (String.index(n) '(') = ((String.index(n) ')')-1) && (String.length(n)!=1) then
    failwith ("parse ric error or command not valid")

  (* Command Assign *)
  else if (((String.length) n)>=6) && equals(String(String.sub (n) 0 6),String("Assign")) then
    let i1 = push(parseric(String(String.sub (n) 7 (((String.length) n)-7)),stack,stackstr), stack) in
    let i2 = push(parseric(topandpop(stackstr),stack,stackstr), stack) in
    Assign(topandpop(stack),topandpop(stack))

  (* Command Cifthenelse *)
  else if (((String.length) n)>=11) && equals(String(String.sub (n) 0 11),String("Cifthenelse")) then
    let i1 = push(parseric(String(String.sub (n) 12 (((String.length) n)-12)),stack,stackstr), stack) in
    let i2 = parsericcomlist(topandpop(stackstr),stack,stackstr) in
    let i3 = parsericcomlist(topandpop(stackstr),stack,stackstr) in
    Cifthenelse(topandpop(stack),i2,i3)

  (* Command while *)
  else if (((String.length) n)>=5) && equals(String(String.sub (n) 0 5),String("While")) then
    let i1 = push(parseric(String(String.sub (n) 6 (((String.length) n)-6)),stack,stackstr), stack) in
    let i2 = parsericcomlist(topandpop(stackstr),stack,stackstr) in
    While(topandpop(stack),i2)
    
  (* Command Call *)
  else if (((String.length) n)>=4) && equals(String(String.sub (n) 0 4),String("Call")) then
    let i1 = push(parseric(String(String.sub (n) 5 (((String.length) n)-5)),stack,stackstr), stack) in
    let i2 = parsericlist(topandpop(stackstr),stack,stackstr) in
    Call(topandpop(stack),i2)
    
  (* Command Reflect *)
  else if (((String.length) n)>=7) && equals(String(String.sub (n) 0 7),String("Reflect")) then
    let i1 = Estring(String.sub (n) 8 (((String.length) n)-9)) in
    Reflect(i1)

  (* No command recognized *)
  else
    failwith ("parse ric error or command not valid")

(* Interpretate list for the function "parserCom" *)
and parsericcomlist (e,stack,stackstr) = match e with String(n) ->
  let listcom = [] in
         (* CHECK: IF STRING CONTAINS A '[' AND A ';' -> ITS THE FIRST ELEMENT
          *        IF STRING CONTAINS A ';'           -> ITS AN INTERMEDIATE ELEMENT
          *        IF STRING CONTAINS A ']'           -> ITS THE LAST ELEMENT
          * IF A "LAST ELEMENT" IS ENCOUNTERED, DO NOT USE A RECURSIVE CALL
          *)
  if (String.contains(n) '[') && ((String.index(n) '[')<(String.index(n) ';')) then
    let i1 = ( String.sub (n) ((String.index(n) '[')+1) (((String.length) n)-(String.index(n) '[')-1) ) in
    let i2 = push(String(i1), stackstr) in
    let l1 = parsericcom(String(i1),stack,stackstr) in
    let l2 = parsericcomlist(topandpop(stackstr),stack,stackstr) in
    let l = l1 :: l2 in l
  else if (String.contains(n) ';') && ((String.index(n) ';')<(String.index(n) ']')) then
    let i1 = ( String.sub (n) ((String.index(n) ';')+1) (((String.length) n)-(String.index(n) ';')-1) ) in
    let i2 = push(String(i1), stackstr) in
    let l1 = parsericcom(String(i1),stack,stackstr) in
    let l2 = parsericcomlist(topandpop(stackstr),stack,stackstr) in
    let l = l1 :: l2 in l
  else
    let i1 = ( String.sub (n) ((String.index(n) ']')+1) (((String.length) n)-(String.index(n) ']')-1) ) in
    let i2 = push(String(i1), stackstr) in
    listcom
(* --- FUNCTIONS FOR REFLECT - END --- *)
