(*
##################################################
##################################################
############## store_Semantica.ml ################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Semantica di store
# Nome File     : store_Semantica.ml
# ------------------------------------------------ 
#
*)

module Funstore:STORE =

struct
  type loc = int
  type 't store = loc -> 't 

  (* genera una nuova locazione *)
  let (newloc,initloc) =
    let count = ref(-1) in
      ((fun () -> count := !count +1; !count),
       (fun () -> count := -1))
      
  (* genera uno store vuoto *)
  let emptystore(x) = initloc(); function y -> x

  (* trova valore loc in store *)
  let applystore(x,y) = x y

  (* alloca loc in store *)
  let allocate((r: 'a store), (e:'a)) =
    let l = newloc() in
      (l, function lu -> if lu = l then e else applystore(r,lu))
 
  (* aggiorna loc in store *)
  let update((r: 'a store), (l:loc), (e:'a)) = 
    function lu -> if lu = l then e else applystore(r,lu)    
end
