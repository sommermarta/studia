set Z:={read "shifts.dat" as "<4n>" comment "#"};    # zbiór nazw zmiany
param N:=35;      #liczba pracowników
set P:={1..35};    #zbiór pracowników
var x[P*Z] binary;    #czy pracownik ma wtedy pracować, czy nie

subto v1: forall <j> in  Z do sum <i> in P do x[i,j]==1;     # żeby wszystkie zmiany były wykonywane przez jednego pracownika

param day[Z]:=read "shifts.dat" as "<4n>1n" comment "#";         
param begin[Z]:=read "shifts.dat" as "<4n>5n" comment "#";
param End[Z]:=read "shifts.dat" as "<4n>6n" comment "#";

set E:= {<i,j> in Z*Z-{<k,k> in Z*Z} with (day[i]==day[j] and begin[i]<=begin[j] and begin[i]<=End[j]+11*60) or (day[i]+1==day[j] and 24*60-End[i]+begin[j]<=11*60)};

subto v2: forall <p> in P do forall <i,j> in E do x[p,i]+x[p,j]<=1;

param duration[Z]:=read "shifts.dat" as "<4n>7n" comment "#";

subto v3: forall <p> in P do sum <z> in Z do x[p,z]*duration[z]<=55*60;


# podobna liczba zmian:
var S1;

subto s1: forall <p> in P do sum <z> in Z do x[p,z]>=S1;
subto s2: forall <p> in P do sum <z> in Z do x[p,z]<=S1+1;

# do print E;
