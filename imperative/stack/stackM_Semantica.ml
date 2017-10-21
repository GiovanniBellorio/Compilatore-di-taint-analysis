(*
##################################################
##################################################
############## stackM_Semantica.ml ###############
##################################################
##################################################
#
# ------------------------------------------------
# Nome Studente : Bellorio
# Anno          : 2016-17
# Corso         : Informatica
# Descrizione   : Semantica dello stack modificabile
# Nome File     : stackM_Semantica.ml
# ------------------------------------------------ 
#
*)

module SemMPila: MPILA = 

struct
  type 'a stack = ('a SemPila.stack) ref
   
  exception Emptystack
  exception Fullstack
  exception Wrongaccess
  
  let emptystack (n, a) = ref(SemPila.emptystack(n, a))
  let lungh x = SemPila.lungh(!x)
  let push (a, p) = p := SemPila.push(a, !p)
  let pop x =  x := SemPila.pop(!x)
  let top x = SemPila.top(!x)
  let empty x = SemPila.empty !x
  let rec svuota x =  if empty(x) then () else (pop x; svuota x)
  let rec faccess (x, n) =
    if n = 0 then SemPila.top(x) else faccess(SemPila.pop(x), n-1)
  let access (x, n) = let nofpops = lungh(x) - 1 - n in
    if nofpops < 0 then raise Wrongaccess else faccess(!x, nofpops)

end
