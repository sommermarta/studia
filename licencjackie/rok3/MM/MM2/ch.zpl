#Nadajniki

set I:={1 to 10};
set D:={read "ch.txt" as "<1n>" comment "#"}; # plik jest zmodyfikowany- dodana kolumna z liczbą porządkową próbowałem indeksować inaczej, ale program nie polubił mojego pomysłu.


param nadajnik_x[D]:=read "ch.txt" as "<1n>2n" comment "#";
param nadajnik_y[D]:=read "ch.txt" as "<1n>3n" comment "#";
param zasieg_min[D]:=read "ch.txt" as "<1n>4n" comment "#";
param zasieg_max[D]:=read "ch.txt" as "<1n>5n" comment "#";
param odl[<i,j,d> in I*I*D]:= sqrt((i-nadajnik_x[d])^2+(j-nadajnik_y[d])^2); # odleglosc punktu [i,j] od najdajnika d
param czywzasiegu[<i,j,d> in I*I*D]:=if(odl[i,j,d]<=zasieg_max[d]) then 1 else 0 end; # parametr mówiący, czy punkt i,j jest w zasięgu nadajnika d

var wybor[D] binary; # ktore nadajniki wybieramy
#var zasieg[D];
#subto ogr1: forall <d> in D do zasieg[d]<=zasieg_max[d];
#subto ogr2: forall <d> in D do zasieg[d]>=zasieg_min[d];

subto ogr4: forall <i,j> in I*I do sum<d> in D do czywzasiegu[i,j,d]*wybor[d]>=1; # musi istnieć co najmniej jeden nadajnik, w którego zasięgu jest punkt
minimize fcelu: sum<d> in D do wybor[d];
#minimize fcelu: sum<d> in D do zasieg[d]*wybor[d]; zakomentowałem bo nie działa cały drugi podpunkt, zostawiłem pierwszy
 # Mikołaj Chrabąszcz