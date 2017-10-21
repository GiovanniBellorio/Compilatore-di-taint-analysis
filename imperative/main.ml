(*
##################################################
##################################################
#################### main.ml #####################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : main interprete
# Nome File     : main.ml
# ------------------------------------------------ 
#
*)

[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-8"]

(* Eliminazione Warning:
 * "-8": pattern-matching is not exhaustive
 * "-26": unused variable
 *)

#use "interprete/sintassi.ml";;

#use "env/env_Interfaccia.ml";;
#use "env/env_Semantica.ml";;
open Funenv;;

#use "store/store_Interfaccia.ml";;
#use "store/store_Semantica.ml";;
open Funstore;;

#use "stack/stack_Interfaccia.ml";;
#use "stack/stack_Semantica.ml";;
open SemPila;;

#use "stack/stackM_Interfaccia.ml";;
#use "stack/stackM_Semantica.ml";;
open SemMPila;;

#use "interprete/eval.ml";;
#use "interprete/operazioni.ml";;
#use "interprete/semantica.ml";;

[@@@ocaml.warning "+26"]
[@@@ocaml.warning "+8"]