* Laczenie zbiorow w 4GL;
*Konkatenacja;

data a;
input x $ y;
cards;
A 1
B 2
C 3
;
run;
data b;
input x $ y;
cards;
x 10
y 20
z 30
;
run;

data razem;
set a b;
run;

data razem;
set b a;
run;

proc append base=a data=b;
run;

data razem;
put 'Pierwszy ' _all_;
set a b;
put 'Drugi ' _all_;
run;
data a;
input x $ y;
cards;
A 1
B 2
C 3
;
run;
data b;
input x $ z;
cards;
x 10
y 20
z 30
;
run;
data razem;
set a b;
run;
proc append base=a data=b;
run;
*interleaving;
*przeplot;
data a;
input x $ y;
cards;
A 1
B 2
C 3
;
run;
data b;
input x $ z;
cards;
A 10
C 20
D 30
;
run;
data razem;
set a b;
by x;
run;
data razem;
set b a;
by x;
run;


*9(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((;

* one-to-one reading;
data a;
input x$ y;
cards;
a 1
b 2
c 3
d 4
;
run;
data b;
input u v;
cards;
1 2 
3 4 
5 6
;
run;
data razem;
set a;
set b;
run;
data razem;
set b;
set a;
run;
data a;
input x$ y;
cards;
a 1
b 2
c 3
d 4
;
run;
data b;
input x v;
cards;
1 2 
3 4
5 6
;
run;
data razem;
set a;
set b;
run;
data a;
input x$ y;
cards;
a 1
b 2
c 3
d 4
;
run;
data b;
input u y @@;
cards;
1 2 3 4 5 6
;
run;
data razem;
set a;
set b;
run;
data razem;
set a;
set b (rename=(y=ynowy));
run;
* one-to-one merging;
data a;
input x$ y;
cards;
a 1
b 2
c 3
d 4
;
run;
data b;
input u v @@;
cards;
1 2 3 4 5 6
;
run;
data razem;
merge a b;
run;
data a;
input x$ y;
cards;
a 1
b 2
c 3
d 4
;
run;
data b;
input x$ v @@;
cards;
1 2 3 4 5 6
;
run;
data razem;
merge a b;
run;
data a1;
input x1$ y1;
cards;
a 1
b 2
c 3
d 4
;
run;
data a2;
input x2$ y2;
cards;
a 1
b 2
c 3
d 4
;
run;
data a3;
input x3$ y3;
cards;
a 1
b 2
c 3
d 4
;
run;
data a4;
input x4$ y4;
cards;
a 1
b 2
c 3
d 4
;
run;
data razem;
merge a1 a2 a3 a4;
run;
data razem1;
merge a1 a2 ;
merge a3 a4 ;
run;
proc compare base=razem compare=razem1;
run;
*match-merging;
data a;
input x$ y;
cards;
a 10
b 20
c 80
;
run;
data b;
input x$ z;
cards;
a 5
b 9
z 8
;
run;
data razem;
merge a b;
by x;
run;
data razem;
merge b a;
by x;
run;
data razem;
merge a a;
by x;
run;
data razem;
merge a b a;
by x;
run;
data a;
input x$ y;
cards;
a 10
b 20
b 30
c 80
;
run;
data b;
input x$ z;
cards;
a 5
b 7
z 8
z 2
;
run;
data razem;
merge a b;
by x;
run;
data a;
input x$ y;
cards;
a 10
b 20
b 30
b 40
c 80
;
run;
data b;
input x$ z;
cards;
a 5
b 7
b 9
z 8
z 2
;
run;
data razem;
merge a b;
by x;
run;
* zmienne IN;
data a;
input x$ y;
cards;
a 10
b 20
b 30
b 40
c 80
;
run;
data b;
input x$ y;
cards;
a 5
b 7
b 9
z 8
z 2
;
run;
data razem;
set a (in=w_a) b (in=w_b);
run;
data razem;
set a (in=w_a) b (in=w_b);
w_aa=w_a;
w_bb=w_b;
run;
data razem;
set a (in=w_a) b (in=w_b);
by x;
w_aa=w_a;
w_bb=w_b;
run;
data a;
input x;
cards;
1
2
3
4
5
;
run;
data suma;
set a end=k;
suma+x;
if k then output;
keep suma;
run;
data suma1;
set a end=k;
suma+x;
if k;
keep suma;
run;
data razem;
merge a suma;
run;
data razem;
merge a suma;
by x;
run;
data razem;
set a suma;
run;
data razem;
set a;
set suma;
run;
data suma;
set a end=k;
suma+x;
*if k then output;
run;
data razem;
merge a suma;
by x;
run;
data suma;
set a end=k nobs=ile;
suma+x;
if k then do;
do i=1 to ile;
output;
end;
end;
keep suma;
run;
data suma;
set a end=k nobs=ile;
suma+x;
if k then do;
do i=1 to ile;
output;
end;
end;
* keep suma;
run;
data razem;
merge a suma;
run;
data suma;
set a end=k nobs=ile;
suma+x;
if k then do;
do i=1 to ile;
set a;
output;
end;
end;
keep x suma;
run;
data e;
do i=1 to 5;
set a;
output;
put _all_;
end;
drop i;
run;
data e;
do i=1 to 10;
set a;
output;
put _all_;
end;
drop i;
run;
data e;
do i=1 to 5;
set a;
output;
end;
do i=1 to 5;
set a;
output;
end;
drop i;
run;
data a;
input x1 x2 x3 x4;
cards;
1 2 3 4
2 3 4 5
3 5 6 6
5 6 6 6
;
run;
data b;
input k;
cards;
2
4
1
2
;
run;
data razem;
merge a b;
run;
data nowy;
set b;
set a point=k;
run;


*----------------------------------------------------------------------------------------------------------------------------;
data a;
 input x$ y;
 cards;
A 1
B 2
C 3
;
run;

data b;
 input u$ v;
 cards;
A 1
B 2
C 3
D 4
E 5
;
run;

data razem;
 set a;
 set b;
run;

data razem;
 put 'Pierwszy ' _all_;
 set a;
  put 'Drugi ' _all_;
 set b;
  put 'Trzeci ' _all_;
run;

data a;
 input x$ y;
 cards;
A 1
B 2
C 3
;
run;

data b;
 input x$ v;
 cards;
AA 1
BB 2
CC 3
DD 4
EE 5
;
run;

data razem;
 set a;
 set b;
run;

data razem;
 set b;
 set a;
run;

data razem;
 set a;
 set b (rename=(x=xx));
run;

data razem;
 set a;
 set b;
 rename x=xx;
run;

data a;
 input x$ y;
 cards;
A 1
B 2
C 3
;
run;

data b;
 input u$ v;
 cards;
AA 1
BB 2
CC 3
DD 4
EE 5
;
run;

data razem;
 merge a b;
run;

data razem;
 merge b a;
run;

data a;
 input x$ y;
 cards;
A 1
B 2
C 3
;
run;

data b;
 input x$ v;
 cards;
AA 1
BB 2
CC 3
DD 4
EE 5
;
run;

data razem;
 merge a b;
run;

data c;
 input x1$ y1;
cards;
a 1
b 2
c 3
d 4
;
run;
data d;
 input u1 v1 @@;
 cards;
1 2 3 4 5 6
;
run;

data a;
 input x$ y;
 cards;
A 1
B 2
C 3
;
run;

data b;
 input u$ v;
 cards;
AA 1
BB 2
CC 3
DD 4
EE 5
;
run;

data razem;
 merge a b;
 merge c d;
 format x: u: v: y: 3.;
run;

data razem;
 merge c d;
 merge a b;
 format x: u: v: y: 3.;
run;

data razem;
 merge c d a b;
 format x: u: v: y: 3.;
run;

data razem;
 merge c d; 
 set a b;
 format x: u: v: y: 3.;
run;

data razem;
 merge a b;
 merge c d;
run;

*match merging; 

data a;
 input x$ y;
 cards;
A 10
C 20
E 30
;
run;

data b;
 input x$ v;
 cards;
A 1
B 2
C 3
D 4
F 5
;
run;

data razem;
 merge a b;
 by x;
run;

data razem;
 merge b a;
 by x;
run;

data a;
 input x$ y;
 cards;
A 10
A 20
E 30
;
run;

data b;
 input x$ v;
 cards;
A 1
A 2
A 3
D 4
F 5
;
run;

data razem;
 merge a b;
 by x;
run;

data a;
 input x$ y;
 cards;
A 10
A 20
E 30
;
run;

data b;
 input x$ y;
 cards;
A 1
A 2
A 3
D 4
F 5
;
run;

data razem;
 set a (in=w_a) b (in=w_b);
 by x;
run;

data razem;
 set a (in=w_a) b (in=w_b);
 by x;
 wwa=w_a;
 wwb=w_b;
run;

data a;
 input x $ y;
 cards;
A 1
B 2
C 3
;
run;
data b;
 input x $ z;
 cards;
A 10
C 20
D 30
;
run;

