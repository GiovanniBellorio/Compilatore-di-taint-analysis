(*
##################################################
##################################################
################ reflect_exe.ml ##################
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : esecuzione reflect su espressioni su s
# Nome File     : reflect_exe.ml
# ------------------------------------------------ 
#
*)

Printf.printf("Reflect\n");;

let com = Reflect(Estring s);;

let (l,sigma) = allocate(emptystore(Undefined), Undefined);;
let rho = bind(emptyenv(Unbound), "result", Dloc l);;
let sigma1 = semc com rho sigma;;

#use "output.ml"
