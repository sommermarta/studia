# i

param n := 10;
set I := {0..n};
set R := {read "radio_nadajniki.txt" as "<1n,2n>" comment "#"};    
set P := {<i,j> in I*I};                                           

param zas[R] := read "radio_nadajniki.txt" as "<1n,2n>4n" comment "#";
param odleg[<i,j,k,l> in P*R] := sqrt((i-k)^2 + (j-l)^2);          

var x[R] binary;                                                    

minimize fcelu: sum <i,j> in R do zas[i,j]*x[i,j];                 

param czywzas[<i,j,k,l> in P*R] := if odleg[i,j,k,l] <= zas[k,l] then 1 else 0 end;   

subto ogr: forall <i,j> in P do sum <k,l> in R do czywzas[i,j,k,l]*x[k,l] >= 1;



 