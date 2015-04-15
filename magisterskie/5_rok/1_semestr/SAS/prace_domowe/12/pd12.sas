******************************* Zad.12.1 *******************************;

data samochody;
	input nrsamochodu nazwa $;
cards;
1 mercedes
2 audi
3 fiat
4 opel
;
run;

data klienci;
	input nrklienta nazwisko $;
cards;
1 kowalski
2 sasinski
3 walec
4 korecki
;
run;

data wypozyczenia;
	input nrwyp nrsamochodu nrklienta;
cards;
1 1 4
2 3 3
3 1 4
4 4 2
;
run;

data _null_;
	set wypozyczenia;
	call symput('ws'||catt(nrwyp), put(nrsamochodu,1.));
	call symput('wk'||catt(nrwyp), put(nrklienta,1.));
run;

%macro info(nrwyp);

proc print data=samochody;
	where nrsamochodu=&&ws&nrwyp;
run;

proc print data=klienci;
	where nrklienta=&&wk&nrwyp;
run;

%mend;

%info(3);

* inaczej:;

data _null_;
	set wypozyczenia;
	call symput('ws'||catt(nrwyp), put(nrsamochodu,1.));
	call symput('wk'||catt(nrwyp), put(nrklienta,1.));
run;

data _null_;
	set klienci;
	call symput('k'||catt(nrklienta), nazwisko);
run;

data _null_;
	set samochody;
	call symput('s'||catt(nrsamochodu), nazwa);
run;

%macro info2(nrwyp);

%put &&&&s&&ws&nrwyp;
%put &&&&k&&wk&nrwyp;

%mend;

%info2(3);

******************************* Zad.12.2 *******************************;

proc sql;

select distinct id 
into :m seperated by  ' , '
from lab8.maly
;

select mean(sales)
from lab8.duzy
where id in (&m)
;

quit;

******************************* Zad.12.3 *******************************;

data a;
	do i=1 to 20;
		x=floor(10*ranuni(0));
		output;
	end;
	keep x;
run;

data b;
	do i=1 to 20;
		x=floor(10*ranuni(0));
		y=floor(10*ranuni(0));
		output;
	end;
	keep x y;
run;

%macro dziel(zbior,n);
	
data _null_;
	set &zbior nobs=aaaa;
	if _n_=1 then 
		do; 
			ile=ceil(aaaa/&n);
			call symput('ilezbiorow',ile); 
			stop; 
		end;
run;

%let pierwsza=1;
%let ostatnia=&n;

%do i=1 %to &ilezbiorow;
	data &zbior&i;
		set &zbior (firstobs=&pierwsza obs=&ostatnia) end=k;
		if k then 
			do;
				%let pierwsza=%eval(&ostatnia+1);
				%let ostatnia=%eval(&ostatnia+&n);
			end;
	run;
%end;

%mend;

%dziel(a,6)
%dziel(b,6)

******************************* Zad.12.6 *******************************;

data b;
	do i=1 to 20;
		x=floor(10*ranuni(0));
		y=floor(10*ranuni(0));
		z=floor(10*ranuni(0));
		output;
	end;
	drop i;
run;

data bb;
	set b;
	if x=6 then x=.;
	if y=2 then y=.;
	if z=4 then z=.;
run;

%macro usunbraki(zbior);

data _null_;
	set &zbior;
	array tab(*) _all_;
	do i=1 to dim(tab);
		if _n_=1 then call symput("ktora"||catt(i),"");
		if tab(i)=. then 
			do;
				call symput("ktora"||catt(i),vname(tab(i))); 
			end;
		end;
run;

data nowy_&zbior;
	set &zbior;
	drop %do i=1 %to 3; &&ktora&i %end;;
run;

%mend;

%usunbraki(bb)

******************************* Zad.12.8 *******************************;

data a;
	input x$ y yy z$;
	cards;
a 3 4 e
b 4 2 e
a 3 3 c
a 6 78 d
z 3 33 c
b 3 2 c
run;

option mprint;

%macro podzial(zbior, zmienna, n);

proc sql noprint;

select min(&zmienna), max(&zmienna)
into :mala, :duza
from &zbior
;

quit;

%let przedzial=%sysevalf((&duza-&mala)/(&n-1));

%local x1;
%let x1=&mala;

%do i=2 %to %eval(&n-1);
	%local x&i;
	%local k;
	%let k=%eval(&i-1);
	%let x&i=%sysevalf(&&x&k+&przedzial);
%end;

%local x&n;
%let x&n=&duza;

data dystr;
	set &zbior nobs=ile end=k;
	array dyst(&n) _temporary_ (&n*0);
	array d(&n);
	%local k;
	%do i=1 %to %eval(&n-1);
		%let k=%eval(&i+1);
		if &zmienna < &&x&k and &zmienna >= &&x&i then dyst[&i]=sum(dyst[&i],1);
	%end;
	if &zmienna >= &&x&n then dyst[&n]=sum(dyst[&n],1);
	if k then
		do;
			d[1] = dyst[1];
			do i=2 to &n;
				d[i] = dyst[i]+d[i-1];
			end;
			do i=1 to &n;
				d[i] = d[i]/ile;
			end;
			output;
		end;
	keep d:;
run;

%mend;

%podzial(a, yy, 6)
%podzial(a, y, 2)

******************************* Zad.12.4 *******************************;

option nomprint;

%macro cos(id, zbiory);

%let ilezbiorow=%sysfunc(countw(&zbiory," "));

data wszystkienaraz;
set %do i=1 %to &ilezbiorow;
	%sysfunc(scan(&zbiory,&i," "))
%end;;
run;

proc sql noprint;

create table tabelaid as
select distinct id
from wszystkienaraz
;

quit;

%let czyistnieje=0;
data _null_;
	set tabelaid;
	if id=&id then do; call symput('czyistnieje','1'); stop; end; 
run;

proc sql noprint;

select x 
into :wartosciksa
from wszystkienaraz
where id=&id and date=(
	select max(date)
	from wszystkienaraz
	where id=&id
	)
;
		
quit;

data _null_;
	a=symget('czyistnieje');
	if a='0' then put "Numer id nieprawidlowy!"; 
	else do;
		a=symget('wartosciksa');
		put a;
	end;
run;

%mend;

%cos(0009,lab12.a0236 lab12.a0346 lab12.a0447)
%cos(01009,lab12.a0236 lab12.a0346 lab12.a0447)
%cos(0003,lab12.a0236)

******************************* Zad.12.7 *******************************;

data a;
	input x$ y yy z$;
	cards;
a 3 4 e
b 4 2 e
a 3 3 c
a 6 78 d
z 3 33 c
b 3 2 c
run;

option mprint;

%macro podzial(zbior, zmienna);

proc sql noprint;

select type
into :jakityp
from dictionary.columns
where libname='WORK' and name="&zmienna" and memname=%upcase("&zbior");

select count(distinct &zmienna)
into :ilezbiorow
from &zbior 
;
hehe
%let zmcos=%sysfunc(compress(&ilezbiorow));

select distinct &zmienna
into :zm1-:zm&zmcos
from &zbior 
;

%do i=1 %to &ilezbiorow;
	create table z&i as
	select * 
	from &zbior
	where &zmienna = %if &jakityp=char %then "&&zm&i"; %else &&zm&i;;
%end;

%mend;

%podzial(a,x)
%podzial(a,yy)

******************************* Zad.12.9 *******************************;

data a;
	input x$ y yy z$;
	cards;
a 3 4 e
b 4 2 e
a 3 3 c
a 6 78 d
z 3 33 c
b 3 2 c
run;

data samochody;
	input nrsamochodu nazwa $;
cards;
1 mercedes
2 audi
3 fiat
4 opel
;
run;

data klienci;
	input nrklienta nazwisko $;
cards;
1 kowalski
2 sasinski
3 walec
4 korecki
;
run;

data wypozyczenia;
	input nrwyp nrsamochodu nrklienta;
cards;
1 1 4
2 3 3
3 1 4
4 4 2
;
run;

data z;
	length zbior$ 15 zmienna$ 10;
	input zbior$ zmienna$;
	cards;
wypozyczenia nrklienta
klienci nazwisko
samochody nazwa
a y
;
run;

%macro zz();

data z2;
	set z;
	razem=zbior || '(keep=' || zmienna || ')';
	keep razem;
run;

proc sql noprint;

select razem
into :zbiorki separated by " "
from z2
;

quit;

data zz;
	merge &zbiorki;
run;

%mend;

%zz()

******************************* Zad.12.10 *******************************;

data a;
	input x$;
	cards;
a
d
g
6
;
run;

option mprint;

%macro kombinacje(zbior,k);

%let czybib=%sysfunc(countw(&zbior,'.'));
%let zb=%upcase(%sysfunc(scan(&zbior,-1,'.')));
%let bibli=%upcase(%sysfunc(scan(&zbior,-2,'.')));

%if &czybib=1 %then %do; 
	proc sql noprint;
	select name
	into :nazwa
	from dictionary.columns
	where memname="&zb" and libname='WORK'
	; 
	%end; %else %do;
		proc sql noprint;
		select name
		into :nazwa
		from dictionary.columns
		where memname="&zb" and libname="&bibli"
		; 
		%end;

%let nazwa=%sysfunc(compress(&nazwa));

proc sort out=aaa data=&zbior;
by &nazwa;
run;

proc sql noprint;

create table komb as
select *  
from %do i=1 %to %eval(&k-1); 
	aaa(rename=(&nazwa=&nazwa&i)) as a&i, 
	%end; 
aaa(rename=(&nazwa=&nazwa&k)) as a&k
where %do i=1 %to %eval(&k-2); 
	a&i..&nazwa&i>a%eval(&i+1).&nazwa%eval(&i+1) and 
	%end; 
a&k..&nazwa&k<a%eval(&k-1).&nazwa%eval(&k-1)
;

%mend;

%kombinacje(a, 2);

******************************* Zad.12.5 *******************************;

data a;
	input o;
	cards;
3
6
8
2
;
run;

option mprint;

%macro nowyformat(zbior);

%let czybib=%sysfunc(countw(&zbior,'.'));
%let zb=%upcase(%sysfunc(scan(&zbior,-1,'.')));
%let bibli=%upcase(%sysfunc(scan(&zbior,-2,'.')));

%if &czybib=1 %then %do; 
	proc sql noprint;
	select name
	into :nazwa
	from dictionary.columns
	where memname="&zb" and libname='WORK'
	; 
	%end; %else %do;
		proc sql noprint;
		select name
		into :nazwa
		from dictionary.columns
		where memname="&zb" and libname="&bibli"
		; 
		%end;

%let nazwa=%sysfunc(compress(&nazwa));

proc sort out=aa data=&zbior;
	by &nazwa;
run;

proc sql noprint;

select nobs
into :ilezmiennych
from dictionary.tables
where libname='WORK' and memname='AA'
;

%let ilezmiennych=%sysfunc(compress(&ilezmiennych));
%let ilezmiennychplusjeden=%eval(&ilezmiennych+1);

data _null_;
	set aa end=k;
	call symput('ppp'||compress(_n_),&nazwa);
	call symput('f'||compress(_n_),put(_n_,roman10.));
	if k then call symput('f'||compress(_n_+1),put(_n_+1,roman10.));
run;

proc format;
	value zadpiec
low-&ppp1=&f1
%do i=1 %to %eval(&ilezmiennych-1);
%let nast=%eval(&i+1);
&&ppp&i-&&ppp&nast=&&f&nast
%end;
&&ppp&ilezmiennych-high=&&f&ilezmiennychplusjeden
;
run;

%mend;

%nowyformat(a)

data _null_;
	do i=1 to 10;
		z=put(i,zadpiec.);
		put z;
	end;
run;
