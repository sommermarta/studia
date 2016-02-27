set V:={1,2,3,4,5,6};
set E:={<1,2>,<2,3>,<2,4>,<4,5>,<5,6>,<3,5>,<3,6>};

var x[E];
subto v1: sum <1,i> in E do x[1,i]==1;
subto v2: sum<i,6> in E do x[i,6]==1;
subto v3: forall <v> in V\{1,6} do sum <v,i> in E do x[v,i]== sum<i,v> in E do x[i,v];

minimize f: sum<i,j> in E do x[i,j];