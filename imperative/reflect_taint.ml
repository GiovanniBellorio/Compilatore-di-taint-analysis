(*
##################################################
##################################################
################ reflect_taint.ml ################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : 
# Nome File     : reflect_taint.ml
# ------------------------------------------------ 
#
*)

Printf.printf("Reflect\n");;

let com = s;;

let (l,sigma) = allocate(sigma, Undefined);;
let rho       = bind(rho, "result", Dloc l);;
let sigma1    = semc com rho sigma;;

#use "output.ml"
