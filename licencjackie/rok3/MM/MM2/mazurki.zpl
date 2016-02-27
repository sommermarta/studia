set I := {1,2,3};

param baza := 10;
param polewy [I] := <1>3, <2>2.5, <3>1.5;
param orzechy [I] := <1>3.2, <2>1.7, <3>4.7;
param koszty [<i,j> in I*I] := baza + polewy[i] + orzechy[j];
param all := 140;
param renoma [<i,j> in I*I] := <1,1>3.5, <2,1>3.4, <3,1>0.4, <1,2>0.8, <2,2>0.4, <3,2>0.4, <1,3>1.5, <2,3>2.7, <3,3>2.4;

var x[I*I] integer >=0;

subto v1: sum <i> in I do x[1,i] >= 30;
subto v2: sum <i> in I do x[3,i] >= 30;
subto v3: sum <i> in I do x[2,i] >= 20;
subto v4: sum <i> in I do x[i,2] >= 0.4*all;
subto v5: sum <i> in I do x[i,2] >= 1.5*(sum <i> in I do x[i,1]);
subto v6: sum <i> in I do x[2,i] <= 45;
subto v7: sum <i,j> in I*I do x[i,j] == all;

maximize zysk: sum <i,j> in I*I do x[i,j]*(0.25*koszty[i,j] + renoma[i,j]);