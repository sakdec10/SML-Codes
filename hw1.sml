val y = explode("abcdefghijklmnopqrstuvwxyz");

fun check(chr,xl) = case xl of 
							(h::t) => if chr = h then true else check(chr,t)
							| _ => false

fun pangram (x,c) = case x of (h::t) => if check(h,c) = true then pangram(t,c) else false
							| _ => true



fun hw1(infile : string, outfile : string) =
let
	val instream = TextIO.openIn infile
	val outstream = TextIO.openOut outfile
	val reading = TextIO.inputLine instream
	fun helper(reading : string option) =
	   case reading of 
		NONE => (TextIO.closeIn instream; TextIO.closeOut outstream)
	      | SOME(c) => (
			if pangram(y,explode(c)) = true then TextIO.output(outstream,"true\r\n") else TextIO.output(outstream,"false\r\n");

	helper(TextIO.inputLine instream))
in
	helper(reading)
end

