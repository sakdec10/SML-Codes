datatype 'a inflist = NIL
                    | CONS of 'a * (unit -> 'a inflist);

exception Empty;
exception Subscript;

fun HD (CONS(a,b)) = a
  | HD NIL = raise Empty; 

fun TL (CONS(a,b)) = b()
  | TL NIL = raise Empty;

fun NUL NIL = true
  | NUL _ = false;

fun NTH 0 L = HD L
  | NTH n L = NTH (n-1) (TL L);

fun TAKE (xs, 0) = []
  | TAKE (NIL, n) = raise Subscript
  | TAKE (CONS(x, xf), n) = x::TAKE(xf(), n-1);

fun FROMN n = CONS(n, fn () => FROMN (n+1));

fun FIB n m = CONS(n, fn () => FIB m (n+m));

fun STUB _ = CONS(0, fn () => STUB 0)

fun FILTER f l =
  if NUL l
  then NIL
  else if f (HD l)
       then CONS(HD l, fn() => (FILTER f (TL l)))
       else FILTER f (TL l);

fun SIFT NIL = NIL
  | SIFT l =
     let val a = HD l
     in CONS(a, fn () => SIFT(FILTER (fn x => x mod a <> 0) (TL l)))
     end;

	 
(**********************
 *
 * FUNCTION AND INFLIST STUBS -- YOU MUST IMPLEMENT THESE
 * 
 * printList and printPairList must write to the file named by f.
 * Anything printed to the terminal will not be graded.
 *
 **********************)
 
fun even (x : int) : bool = if x mod 2 = 0 then true else false
fun odd  (x : int) : bool = if x mod 2 <> 0 then true else false

val fibs     = FIB 0 1 
val evenFibs = FILTER even (FIB 0 1)
val oddFibs  = FILTER odd (FIB 0 1)  

fun printGenList (f : ('a -> 'b)) (l : ('a list)) : unit = case l of h::t => ( f h; printGenList f t)
																	|	_ => ()
fun printList (f : string, l : int list) : unit = let 
	val outstream = TextIO.openOut f;
	fun helper l = 
	case l of h::t => (TextIO.output(outstream, Int.toString(h) ^ " "); helper t)
		|	_ => TextIO.closeOut outstream
	in 
	helper l
	end
fun printPairList (f : string, l : (int * int) list) : unit = let
	val outstream = TextIO.openOut f;
	fun helper l =
	case l of (h1,h2)::t => (TextIO.output(outstream,"(" ^ Int.toString(h1) ^ ", " ^ Int.toString(h2) ^ ") ");helper t)
		| _ => TextIO.closeOut outstream
	in
	helper l
	end
fun ZIP (infL1 : 'a inflist, infL2 : 'b inflist) : ('a * 'b) inflist = if NUL infL1 = true orelse NUL infL2 = true then NIL
																	else CONS(((HD infL1),(HD infL2)),fn() => ZIP((TL infL1),(TL infL2)))
