set V:={1 .. 6};
set E:={<1,2>,<2,3>,<3,4>,<3,5>,<2,4>,<4,5>,<5,6>, <2,1>, <6,5>};
var x[E];
minimize fcelu: sum<v,i> in E do x[v,i]; 

subto r1: sum<1,i> in E do x[1,i]==1;
subto r11: sum<1,i> in E do x[i,1]==0;
subto r2: sum<i,6> in E do x[i,6]==1;
subto r22: sum<i,6> in E do x[6,i]==0;
subto r3: forall <v> in V\{1,6} do sum <v,i> in E do x[v,i]==sum<i,v> in E do x[i,v];