(*
##################################################
##################################################
############## stackM_Interfaccia.ml #############
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Interfaccia stack modificabile
# Nome File     : stackM_Interfaccia.ml
# ------------------------------------------------ 
#
*)

module type MPILA =
sig
  type 't stack
  
  val emptystack : int * 't -> 't stack		    (* genera lo stack vuoto *)
  val push       : 't * 't stack -> unit      (* inserimento di un valore *)
  val pop        : 't stack -> unit           (* pulizia della cima *)    
  val top        : 't stack -> 't             (* estrazione del valore sulla cima *)
  val empty      : 't stack -> bool           (* vuoto *)
  val lungh      : 't stack -> int            (* numero di elementi contenuti *)
  val svuota     : 't stack -> unit           (* svuota lo stack *)
  val access     : 't stack * int -> 't       (* accesso ad una certa posizione *)
  
  exception Emptystack
  exception Fullstack
  exception Wrongaccess
end