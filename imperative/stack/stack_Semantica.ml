(*
##################################################
##################################################
############## stack_Semantica.ml ################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Semantica dello stack
# Nome File     : stack_Semantica.ml
# ------------------------------------------------ 
#
*)

module SemPila: PILA =

struct
  type 'a stack = Empty of int | Push of 'a stack * 'a

  exception Emptystack
  exception Fullstack

  let emptystack (n, x) = Empty(n)
  let rec max = function
    | Empty n -> n
    | Push(p,a) -> max p
  let rec lungh = function
    | Empty n -> 0
    | Push(p,a) -> 1 + lungh(p)
  let push (a, p) = if lungh(p) = max(p) then raise Fullstack else Push(p,a)
  let pop = function
    | Push(p,a) -> p
    | Empty n -> raise Emptystack
  let top = function
    | Push(p,a) -> a
    | Empty n -> raise Emptystack
  let empty = function
    | Push(p,a) -> false
    | Empty n -> true

end
