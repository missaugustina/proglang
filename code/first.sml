(* comment! first program! *)

(* ML is strongly typed *)

val x = 34;
(* static environment: x : int *)
(* dynamic environment (env when running program): x-->34 *)

val y = 17;
(* x --> 34, y -->17 *)

(* a program is a sequence of bindings *)

val z = (x + y) + (y + 2);
(* addition has type int if vars have type int *)
(* evaluated in the current dynamic environment *)

(* can you use later bindings? no *)
(* static env doesn't run the program, just deals with types *)

val abs_of_z = if z < 0 then 0 - z else z;

val abs_of_z_simpler = abs(z);

