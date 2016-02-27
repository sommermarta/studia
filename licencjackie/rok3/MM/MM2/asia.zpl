# Joanna Czapla
#ii.

set I:={0 to 10};
set J := {<i,j> in I*I};
set V:={read "radio_nadajniki.txt" as "<1n,2n>" comment "#"};						# zbiór punktów, w których mogą być nadajniki 

param n:=card(V);																	# sprawdzam liczność nadajników;
set E:={1 to n};																	# zbiór indeksów nadajników

param x[E]:=read "radio_nadajniki2.txt" as "1n" comment "#"; 						# współrzędna x-owa
param y[E]:=read "radio_nadajniki2.txt" as "2n" comment "#"; 						# współrzędna y-owa
param min_z[E]:=read "radio_nadajniki2.txt" as "3n" comment "#"; 					# zasięg minimalny
param max_z[E]:=read "radio_nadajniki2.txt" as "4n" comment "#"; 					# zasięg maksymalny

defnumb odleglosc(i,j,e):= sqrt((i-x[e])^2+(j-y[e])^2); 						# odleglosc punktu [i,j] od najdajnika e

var zasieg[E];
set JE := J * E;
var w_zasiegu[JE] binary;

subto ogr1: forall <i,j> in J do sum<e> in E do w_zasiegu[i, j, e] >=1;	#kazdy punkt w naszym kwadracie [0,10]x[0,10] musi byc w zasiegu choc jednego nadajnika
subto ogr2: forall <e> in E do zasieg[e]<=max_z[e];
subto ogr3: forall <e> in E do zasieg[e]>=0;		
subto ogr4: forall <i, j, e> in JE do
  if odleglosc(i, j, e) <= max_z[e]
    then vif (w_zasiegu[i, j, e] == 1)
      then zasieg[e] >= odleglosc(i, j, e)
    end, checkonly
  else
    w_zasiegu[i, j, e] == 0
  end;

minimize fcelu: sum<e> in E : zasieg[e]; 