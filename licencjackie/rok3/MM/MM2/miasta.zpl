set V := { read "miasta.dat" as "<1n>" };

param X[V] := read "miasta.dat" as "<1n>2n";
param Y[V] := read "miasta.dat" as "<1n>3n";

set E := {<i,j> in V*V with (X[i]-X[j])^2 + (Y[i]-Y[j])^2 <= 10000 and i!=j};
var x[E];

subto v1: sum<3,i> in E do x[3,i]==1;
subto v2: sum<i,3> in E do x[i,3]==0;
subto v3: sum<i,5> in E do x[i,5]==1;
subto v4: sum<5,i> in E do x[5,i]==0;
subto v5: forall <v> in V\{1,5} do sum <v,i> in E do x[v,i]== sum<i,v> in E do x[i,v];

minimize f: sum<i,j> in E do x[i,j];