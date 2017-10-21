(*
##################################################
##################################################
############# 4_Stack_Interfaccia.ml #############
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Interfaccia stack
# Nome File     : 4_Stack_Interfaccia.ml
# ------------------------------------------------ 
#
*)

module type PILA =
sig
  type 't stack

  val emptystack : int * 't -> 't stack
  val push : 't * 't stack -> 't stack
  val pop : 't stack -> 't stack
  val top : 't stack -> 't
  val empty : 't stack -> bool
  val lungh : 't stack -> int

  exception Emptystack
  exception Fullstack

end
