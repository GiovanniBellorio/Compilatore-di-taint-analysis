(*
##################################################
##################################################
############# 2_Env_interfaccia.ml ###############
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Interfaccia ambiente
# Nome File     : 2_Env_interfaccia.ml
# ------------------------------------------------ 
#
*)

module type ENV =
sig
  type 't env

  val emptyenv : 't -> 't env             	(* genera un ambiente vuoto *)
  val applyenv : 't env * string -> 't    	(* trova valore ide in amb *)
  val bind :                              	(* aggiunge un legame tra un parametro e un valore *)
    't env  * string * 't -> 't env
  val bindlist :                         	(* aggiunge una lista di legami parametro-valore *)
    't env * (string list) * ('t list) -> 't env

  exception WrongBindlist 
end
