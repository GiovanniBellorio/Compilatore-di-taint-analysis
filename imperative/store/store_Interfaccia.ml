(*
##################################################
##################################################
############# 3_Store_Interfaccia.ml #############
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Interfaccia store
# Nome File     : 3_Store_Interfaccia.ml
# ------------------------------------------------ 
#
*)

module type STORE =
sig
  type 't store
  type loc

  val emptystore : 't -> 't store          (* genera uno store vuoto *)
  val applystore : 't store * loc -> 't    (* trova valore loc in store *)
  val allocate :                           (* alloca loc in store *)
    't store  * 't -> loc * 't store
  val update :                             (* aggiorna loc in store *)
    't store  * loc * 't -> 't store
end
