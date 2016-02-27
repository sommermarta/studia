param n := read "skojarzenie3.txt" as "1n" skip 1 use 1;

set V := {1..n};
set E := {read "skojarzenie3.txt" as "<1n,2n>" skip 3} + {read "skojarzenie3.txt" as "<2n,1n>" skip 3};

var x[E] binary;
subto r1: forall <v> in V do sum <v,i> in E do x[v,i] + sum <i,v> in E do x[i,v] <= 1;

maximize f: sum<i,j> in E do x[i,j];