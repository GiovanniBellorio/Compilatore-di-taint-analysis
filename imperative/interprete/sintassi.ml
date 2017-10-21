(*
##################################################
##################################################
############### sintassi.ml ######################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Domini Sintattici
# Nome File     : sintassi.ml
# ------------------------------------------------ 
#
*)

(* IDENTIFIER *)
type ide = string

(* TYPE EXPRESSABLE *)
type exp = 

  (* CONSTANTS *)
  | Eint of int
  | Ebool of bool
  | Estring of string
  | Cstring of string

  (* INT OPERATIONS *)
  | Prod of exp * exp         
  | Sum of exp * exp
  | Diff of exp * exp
  | Minus of exp

  (* STRING OPERATIONS *)
  | Len of exp
  | Concat of exp * exp
  | Substr of exp * exp * exp

  (* CONTROLS *)
  | Eq of exp * exp
  | Iszero of exp
  | Or of exp * exp
  | And of exp * exp
  | Not of exp
      
  (* SELECTION CONSTRUCT *)
  | Ifthenelse of exp * exp * exp

  (* APPLY A SPECIFIC ENVIRONMENT IN A BLOCK *)
  | Let of ide * exp * exp
      
  (* identificatori *)
  | Den of ide                        (* riferimento *)
  | Val of exp                        (* valore *) 

  (* OPERATIONS ON MEMORY LOCATIONS *)
  | Newloc of exp                     (* locazione senza nome *)
     
  (* OPERATIONS ON FUNCTIONS *)
  | Fun of ide list * exp             (* dichiarazione *)
  | Appl of exp * exp list            (* applicazione *)
  | Rec of ide * exp                  (* ricorsione *)

  (* OPERATIONS ON PROCEDURES *)
  | Proc of ide list * block

  (* ERROR - NOT FOUND EXPRESSION *)
  | Undefinedstack

and decl = 
    (ide * exp) list

(* CONSTRUCT BLOCK need for Proc and Block *)
and block = 
    (ide * exp) list * com list
	
(* TYPE COMMAND *)
and com = 
  | Assign of exp * exp                       (* assegnamento *)
  | Cifthenelse of exp * com list * com list  (* if-then-else *)
  | While of exp * com list                   (* while *)
  | Block of decl * com list                  (* blocco *)
  | Call of exp * exp list                    (* procedura *)
  | Reflect of exp                            (* reflect *)
