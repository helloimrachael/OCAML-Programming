type ide = string;;
type exp = (*data type*)
	| Eint of int
	| Estring of string 
	| Ebool of bool
	| Prod of exp * exp
	| Sum of exp * exp
	| Sub of exp * exp
	| Eq of exp * exp
	| Minus of exp
	| IsZero of exp
	| Or of exp * exp
	| And of exp * exp
	| Not of exp
	| Append of exp * exp
	| Den of ide
	| Ifthenelse of exp * exp * exp
	| Let of ide * exp * exp (*idefun, body, bodylet*)
	| Fun of ide * exp
	| FunBin of ide * ide * exp
	| FunCall of exp * exp
	| Letrec of ide * ide * exp * exp (*idefun, arg, body, bodylet*)
	| Dictionary of (ide * exp) list (*il dizionario è una lista di coppie chiave-valore*)
	| Insert of ide * exp * exp (*chiave, valore, dizionario*)
	| Delete of ide * exp (*chiave*)
	| Has_Key of ide * exp (*chiave, dizionario*)
	| Iterate of exp * exp (*funzione, dizionario*)
	| Fold of exp * exp * exp (*funzione, dizionario, valore iniziale*)
	| Filter of ide list * exp;; (*lista di chiavi, dizionario*)

(*ambiente polimorfo*)
type 't env = (string * 't) list;;
let emptyenv (v : 't) = [("",v)];; (*funzione che ti crea l'ambiente vuoto*)
let rec applyenv ((r : 't env), (i : ide)) = match r with
	| [(_,e)] -> e
	| (id, ed)::xs -> if i=id then ed else applyenv(xs, i)
	| [] -> failwith("Not Found in This Env");;

let bind (r : 't env) (i : ide) (v : 't) = (i,v)::r;; (*funzione per collegare la coppia (i,v) all'ambiente*)

type evT = (*tipi esprimibili nell'ambiente*)
	| Int of int
	| Bool of bool
	| String of string
	| Unbound
	| FunVal of ide * exp * evT env
	| FunBinVal of ide * ide * exp * evT env
	| RecFunVal of ide * evFun
	| DictionaryVal of (ide * evT) list
and evFun = ide * exp * evT env;;

let typecheck (s : string) (v : evT) : bool =
	match s with
	| "int" -> (match v with 
				| Int(_) -> true
				| _ -> false)
	| "bool" -> (match v with
				| Bool(_) -> true
				| _ -> false)
	| "string" -> (match v with
				| String(str) -> true
				| _ -> false)
	| _ -> failwith("invalid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n), Int(u)) -> Int(n*u)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y) 
	then (match (x,y) with
		| (Int(n), Int(u)) -> Int(n+u)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

let sub x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n), Int(u)) -> Int(n-u)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		| (Int(n), Int(u)) -> Bool(n=u)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

let minus x = if (typecheck "int" x) 
	then (match x with
		| Int(n) -> Int(-n)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

let iszero x = if (typecheck "int" x) 
	then (match x with
		| Int(n) -> Bool(n=0)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

(*or*)
let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		| (Bool(b), Bool(e)) -> Bool(b||e)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

(*and*)
let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		| (Bool(b), Bool(e)) -> Bool(b&&e)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

(*not*)
let non x = if (typecheck "bool" x)
	then (match x with
		| Bool(true) -> Bool(false)
		| Bool(false) -> Bool(true)
		| _ -> failwith("Type Error"))
	else failwith("Type Error");;

(*append*)
let append x y = if (typecheck "string" x) && (typecheck "string" y)
	then (match (x,y) with
		| (String(n), String(u)) -> String(n^u)
		| _ -> failwith("Type Error"))
else failwith("Type Error");;

(*Aux Fun*)
let rec inDizionario d x = match d with (*funzione che mi controlla se una chiave è presente o meno nel dizionario*)
	| [] -> false
	| (z,y)::xs -> if (x=z) then true else inDizionario xs x;;

let rec noDuplicati d = match d with (*funzione che mi restituisce un dizionario senza duplicati*)
	| [] -> []
	| (x,y)::xs -> if (inDizionario xs x) then noDuplicati xs else (x,y)::noDuplicati xs;;

let rec inLista l x = match l with (*funzione che mi controlla se un elemento è presente o meno nella lista*)
	| [] -> false
	| y::xs -> if (x=y) then true else inLista xs x;;

let rec typec d s = match d with (*questa funzione mi assicura che nel dizionario i valori delle coppie del dizionario abbiano tutte lo stesso tipo*)
	| [] -> []
	| (x,y)::xs -> if (typecheck s y) then (x,y)::(typec xs s) else failwith("Type Error");;

(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	| Eint n -> Int n
	| Estring s -> String s
	| Ebool b -> Bool b
	| IsZero a -> iszero (eval a r) 
	| Den i -> applyenv (r,i)
	| Eq(a,b) -> eq (eval a r) (eval b r)
	| Prod(a,b) -> prod (eval a r) (eval b r)
	| Sum(a,b) -> sum (eval a r) (eval b r)
	| Sub(a,b) -> sub (eval a r) (eval b r)
	| Minus a -> minus (eval a r)
	| And(a,b) -> et (eval a r) (eval b r)
	| Or(a,b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
	| Append(a,b) -> append (eval a r) (eval b r)
	| Ifthenelse(a,b,c) -> let g = (eval a r) in
		if (typecheck "bool" g)
			then (if g = Bool(true) then (eval b r) else (eval c r))
		else failwith("Non Boolean Guard")
	| Let(i,e1,e2) -> eval e2 (bind r i (eval e1 r))
	| Fun(i,a) -> FunVal(i,a,r)
	| FunBin(i1, i2, a) -> FunBinVal(i1, i2, a, r)
	| FunCall(f, eArg) -> let fClosure = (eval f r) in
		(match fClosure with
			| FunVal(arg, fBody, fDecEnv) ->
				eval fBody (bind fDecEnv arg (eval eArg r))
			| RecFunVal(g, (arg, fBody, fDecEnv)) ->
				let aVal = (eval eArg r) in (*eval parametro attuale nell'env chiamante*)
					let env1 = (bind fDecEnv g fClosure) in (*new env esteso con il binding tra g e la chiusura*)
						let env2 = (bind env1 arg aVal) in (*new env con la chiusura ricorsiva esteso con il binding
							tra parametro formale e env dove è eseguita l'eval del parametro attuale*)
							eval fBody env2 (*eval del body nell'ultimo env aggiornato*)
			| _ -> failwith("Non Functional Value"))
	| Letrec(f,i,funDef,letBody) -> (match funDef with
		| Fun(i, fBody) -> let r1 = 
			(bind r f (RecFunVal(f,(i,fBody,r)))) in 
				eval letBody r1
		| _ -> failwith("Non Functional Def"))
	(*rimuovo i duplicati dalla lista, valuto la lista*)
	| Dictionary(lis) -> let thisd = noDuplicati lis
		in DictionaryVal(evalDizionario thisd r)
	| Insert(i, v, d) -> (match (eval d r) with
		| DictionaryVal(myd) -> if (inDizionario myd i) then failwith("Key already inserted")
								else (match myd with
									| [] -> DictionaryVal((i, eval v r)::[])
									| (x,y)::xs -> 
										if typecheck "int" y then 
											if typecheck "int" (eval v r) then DictionaryVal((i, eval v r)::myd)
											else failwith("Wrong Type")
										else if typecheck "bool" y then
											if typecheck "bool" (eval v r) then DictionaryVal((i, eval v r)::myd)
											else failwith("Wrong Type")	
										else if typecheck "string" y then
											if typecheck "string" (eval v r) then DictionaryVal((i, eval v r)::myd)
											else failwith("Wrong Type")
										else failwith("Unkown Type"))
		| _ -> failwith("Wrong Evaluation"))
	| Delete(i, d) -> (match (eval d r) with
		| DictionaryVal(myd) -> if (inDizionario myd i) then DictionaryVal(removeCouple myd i)
								else failwith("Key Not Found")
		| _ -> failwith("Wrong Value"))
	| Has_Key(i, d) -> (match (eval d r) with
		| DictionaryVal(myd) -> Bool(inDizionario myd i)
		| _ -> failwith("Wrong Valure"))
	| Iterate(f, d) -> (match (eval d r) with
		| DictionaryVal(myd) -> DictionaryVal(iterateCall myd f r)
		| _ -> failwith("Wrong Value"))
	| Fold(f, d, v) -> (match (eval d r) with
		| DictionaryVal(myd) -> foldCall myd f r v
		| _ -> failwith("Wrong Value"))
	| Filter(l, d) -> (match (eval d r) with
		| DictionaryVal(myd) -> DictionaryVal(filterCall myd l r)
		| _ -> failwith("Wrong Value"))
	and removeCouple (d : (ide * evT) list) (i : ide) = (match d with
		| [] -> []
		| (x,y)::xs -> if (i=x) then (removeCouple xs i) else (x,y)::(removeCouple xs i))
	and evalDizionario (d : (ide * exp) list) (r : evT env) = let ed = (match d with
		| [] -> []
		| (x,y)::xs -> (x, eval y r)::(evalDizionario xs r))
		in checkType ed
	and checkType (d : (ide * evT) list) = (match d with
		| [] -> []
		| (x,y)::xs -> if typecheck "int" y then (typec d "int")
					   else if typecheck "bool" y then (typec d "bool")
					   else if typecheck "string" y then (typec d "string")
					   else failwith("Wrong Type"))
	and iterateCall (d : (ide * evT) list) (f : exp) (r : evT env) =
		(match d with
			| [] -> []
			| (x,y)::xs -> (x, (dFCall f y r))::iterateCall xs f r)
	and foldCall (d : (ide * evT) list) (f : exp) (r : evT env) (n : exp) = match f with
		| FunBin(i1, i2, a) -> (match d with
			| [] -> eval n r
			| (x,y)::xs -> evalBinFun a r i1 i2 y (foldCall xs f r n))
		| Fun(_,_) -> failwith("Not Binary")
		| _ -> failwith("Wrong Value")
	and evalBinFun (a : exp) (amb : evT env) (i1 : ide) (i2 : ide) (y : evT) (n : evT) =
		eval a (bind (bind amb i2 n) i1 y)
	and filterCall (d : (ide * evT) list) (l : ide list) (r : evT env) =
		(match d with
			| [] -> []
			| (x,y)::xs -> if inLista l x then (x,y)::filterCall xs l r else filterCall xs l r)
	and dFCall (f : exp) (y : evT) (r : evT env) =
		let fClosure = (eval f r) in (match fClosure with
			| FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg y)
			| RecFunVal(g, (arg, fBody, fDecEnv)) -> (*il parametro attuale nell'env è gia un evT*)
				let newEnv = (bind fDecEnv g fClosure) in (*new env esteso con il binding tra g e la chiusura*)
					let aEnv = (bind newEnv arg y) in (*new env con la chiusura esteso con il binding tra il parametro formale
					e l'env dove è valutato il parametro attuale*)
						eval fBody aEnv (*eval del body nel'env aggiornato*)
			| _ -> failwith("Non Functional Value"));;

(* test per interprete base*)

(*new empty env*)
let env = emptyenv Unbound;;

let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;

eval e1 env;;

let e2 = FunCall(Let("x", Eint 2, Fun("y", Sum(Den "y", Den "x"))), Eint 3);;

eval e2 env;;

(*test per interprete esteso*)

let emptyD = Dictionary([]);;

let myD = Dictionary([("mele", Eint 430);("banane", Eint 312);("arance", Eint 525);("pere", Eint 217)]);;

eval myD env;;

let insertD = Insert("kiwi", Eint 300, myD);;

eval insertD env;;

let deleteD = Delete("pere", insertD);;

eval deleteD env;;

let searchD = Has_Key("pere", deleteD);;

eval searchD env;;

let iterateD = Iterate(Fun("val", Sum(Den "val", Eint 1)), myD);;

eval iterateD env;;

let foldD = Fold(FunBin("acc", "x", Sum((Sum(Den "x", Eint 1)), Den "acc")), myD, Eint 0);;

eval foldD env;;

let boolD = Dictionary([("mele", Ebool true);("banane", Ebool true);("pere", Ebool false)]);;

eval boolD env;;

let foldB = Fold(FunBin("acc", "x", And(Not(Den "x"), Den "acc")), boolD, Ebool true);;

eval foldB env;;

let stringD = Dictionary([("mele", Estring "mele");("banane", Estring "simone");("pere", Estring "nico")]);;

eval stringD env;;

let foldS = Fold(FunBin("acc", "x", Append(Den "x", Den "acc")), stringD, Estring "1");;

eval foldS env;;

let filterD = Filter(["kiwi";"banane"], deleteD);;

eval filterD env;;

(*test per eccezioni*)

let excepD = Dictionary([("pere", Estring "mele");("kiwi", Eint 200)]);;

eval excepD env;;

eval (Iterate(Fun("val", Sum(Den "val", Eint 1)), excepD)) env;;