set I:={read "szeregowanie.txt" as "<1n>" comment "#"};

param deadline [I]:= read "szeregowanie.txt" as "<1n>2n" comment "#";
param kara [I]:= read "szeregowanie.txt" as "<1n>3n" comment "#";

# do print deadline [5];
# do forall <i> in I do print i, "-", deadline[i], "-", kara[i];
# do forall <i> in I do check i >= 0;

param n:=card(I);       #liczba elementów
set J:={1..n};

var kiedy[I*J] binary;
var zkara[I] binary;

subto v1: forall <i> in I do sum <j> in J do kiedy[i,j] == 1;
subto v2: forall <j> in J do sum <i> in I do kiedy[i,j] == 1;
subto v3: forall <i> in I do sum <j> in {1..deadline[i]} do kiedy[i,j] == (1 - zkara[i]);

minimize fcelu: sum <i> in I do zkara[i]*kara[i];