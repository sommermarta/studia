set F := {1..3};
set D := {1..4};
set K := {1..5};

var x[F*D];
var y[D*K];

param maxprod[F] := <1>100, <2>150, <3>200;
param minobr[D] := <1>25, <2>20, <3>15, <4>50;
param maxobr[D] := <1>70, <2>50, <3>30, <4>120;
param zapot[K] := <1>100, <2>50, <3>40, <4>30, <5>20;

param kosztf[F] := <1>2, <2>1.8, <3>1.7;
param kosztd[D] := <1>2.1, <2>1.9, <3>2.2, <4>2.3;

subto ogr1: sum <d> in D do x[1,d] >= 50;
subto ogr2: forall <f> in F do sum <d> in D do x[f,d] <= maxprod[f];
subto ogr3: forall <d> in D do sum <k> in K do y[d,k] <=maxobr[d];
subto ogr4: forall <d> in D do sum <k> in K do y[d,k] >=minobr[d];
subto ogr5: forall <k> in K do sum <d> in D do y[d,k] == zapot[k];
subto ogr6: forall <d> in D do (sum <f> in F do x[f,d]) == (sum <k> in K do y[d,k]);

minimize koszt: sum <f,d> in F*D do x[f,d]*kosztf[f] + sum <d,k> in D*K do y[d,k]*kosztd[d];