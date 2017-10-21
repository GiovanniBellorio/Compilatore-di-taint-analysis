(*
##################################################
##################################################
############# 2_Env_Semantica.ml #################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Semantica Ambiente
# Nome File     : 2_Env_Semantica.ml
# ------------------------------------------------ 
#
*)

module Funenv:ENV =

struct
  type 't env = string -> 't

  exception WrongBindlist

  (* genera un ambiente vuoto *)
  let emptyenv(x) = function y -> x

  (* trova un valore ide in amb *)
  let applyenv(x,y) = x y

  (* aggiunge un legame parametro-valore in amb *)
  let bind((r: 'a env) , (l:string),  (e:'a)) = 
    function lu -> if lu = l then e else applyenv(r,lu)

  (* aggiunge una lista di legami in amb *)
  let rec bindlist(r, il, el) = match (il,el) with
    | ([],[]) -> r
    | i::il1, e::el1 -> bindlist (bind(r, i, e), il1, el1)
    | _ -> raise WrongBindlist
	
end
