# ii

param n := 10;
set I := {0..n};
set R := {read "radio_nadajniki.txt" as "<1n,2n>" comment "#"};     
set P := {<i,j> in I*I};                                            

param zas1[R] := read "radio_nadajniki.txt" as "<1n,2n>3n" comment "#";
param zas2[R] := read "radio_nadajniki.txt" as "<1n,2n>4n" comment "#";
param odleg[<i,j,k,l> in P*R] := sqrt((i-k)^2 + (j-l)^2);          

var x[R] binary;
var zas[R];
var y[P*R] binary;    

subto ogr1: forall <k,l> in R do zas[k,l] <= zas2[k,l]*x[k,l];
subto ogr2: forall <k,l> in R do zas[k,l] >= zas1[k,l]*x[k,l];
subto ogr3: forall <i,j,k,l> in P*R do zas[k,l] >= odleg[i,j,k,l]*y[i,j,k,l];
subto ogr4: forall <i,j> in P do sum <k,l> in R do y[i,j,k,l]==1;

minimize fcelu: sum <k,l> in R do zas[k,l]; 