************************************************************;
********************* kolokwium 2014 ***********************;
************************************************************;

*********************** zad.1 *****************************;

data d;
	do i=1 to 5;
		x=floor(10*ranuni(0)+1);
		y=floor(10*ranuni(0)+1);
		z=floor(10*ranuni(0)+1);
	end;
	drop i;
run;

data b;
	input ala $ z;
	cards;
d 1
f 5
. 3
g 66
ooo 1
;
run;

data c;
	input z;
	cards;
3
4
.
6
.
.
;
run;


data a;
	input alaa $ z;
	cards;
d 1
f .
f 3
g 66
ooo 1
;
run;

option mprint;

%macro stworz(bibl, zmienna);

proc sql noprint;
	select memname, count(memname)
	into :zbiory separated by ' ', :ilezb
	from dictionary.columns
	where libname="%upcase(&bibl)" and name="&zmienna"
	;
quit;

%do i=1 %to &ilezb;
	%let zb&i=;
%end;

%let i=1;
%let z=%scan(&zbiory, &i);

%do %while (&z ne );

	data _null_;
		set &z (keep=&zmienna) end=k;
		retain suma 0;
		if &zmienna=. then do; suma=suma+1; stop; end;
		if k then call symput("zb&i","&z"); 
	run;

	%let i=%eval(&i+1);
	%let z=%scan(&zbiory, &i);

%end;

data zbiorki;
	%do i=1 %to &ilezb;
		%if &&zb&i ne %then %do; zbior="&&zb&i"; output; %end;
	%end;
run;

%mend;

%stworz(work,z)

*********************** zad.2 *****************************;

proc sql;

select nazwisko, count(*) as ile
from(
	select distinct a.marka, k.nazwisko
	from kolo.auta as a
	join kolo.wypozyczenia as w on a.id_auto=w.id_auto
	join kolo.klienci as k on k.id_klient=w.id_klient 
	)
group by nazwisko
having ile=1
;

quit;

*********************** zad.3 *****************************;

data dane;
	input x $ y;
	cards;
a 5
a 2
a 0
b 1
b 8
c 1
c 3
;
run;

data ktore;
	input x$ k;
	cards;
a 3
b 1
c 2
;
run;

data _null_;
	merge dane ktore end=koniec;
	by x;
	retain ile 1 suma 0 dziel 0;

	if ile=k then 
		do;
			suma=suma+y;
			dziel=dziel+1;
		end;

	ile = ile+1;
	if last.x then ile=1;
	
	if koniec then 
		do;
			sr=suma/dziel;
			put 'Srednia:   ' sr;
		end;
run;

*********************** zad.4 *****************************;

data a;
	array a(5);
	do j=1 to 5;
		do i=1 to 5;
			a[i]=floor(10*ranuni(0)-2);
		end;
		output;
	end;
	drop i j;
run;

proc iml;
reset log print;

use a;
read all into macierz;

macierzplus=macierz;
macierzplus[loc(macierzplus[,]<0)]=-macierzplus[loc(macierzplus[,]<0)];

norm1=max(macierzplus[+,]);
norm8=max(macierzplus[,+]);
norm2=max(sqrt(eigval(macierz*macierz`)));

norm= norm1||norm2||norm8;
norm=norm`;
kol="norma";

create normy from norm[colname=kol];
append from norm;

quit;

************************************************************;
********************* kolokwium 2011 ***********************;
************************************************************;

*********************** zad.1 *****************************;

data a;
	input tekst $ y cos $;
	cards;
a 5 gtdcfgh
gghjk 7 kjhh
hgf 6666 ty
;
run;

%macro optymalizuj(zbior);

%let bib=%scan(&zbior,1,'.');
%let zb=%scan(&zbior,2,'.');

proc sql noprint;
	select name
	into :zmienne separated by ' '
	from dictionary.columns
	where libname="%upcase(&bib)" and memname="%upcase(&zb)" and type='char'
	;
quit;

%let i=1;
%let zm=%scan(&zmienne,&i);

%do %while (&zm ne );

	data _null_;
		set &zbior end=k;
		retain maks 0;	

		l=length(&zm);
		if l>maks then maks=l;
	
		if k then call symput("dlugosc&i", maks);
	run;

	%let i=%eval(&i+1);
	%let zm=%scan(&zmienne,&i);

%end;

data kopia;
	length 
		%let i=1;
		%let zm=%scan(&zmienne,&i);

		%do %while (&zm ne );
				
			&zm $ &&dlugosc&i

			%let i=%eval(&i+1);
			%let zm=%scan(&zmienne,&i);			
		%end;;
	set &zbior;
run;

%mend;

%optymalizuj(work.a)

*********************** zad.3 *****************************;

data a;
	array a(5);
	do i=1 to 5;
		do j=1 to 5;
			a[j]=floor(10*ranuni(0));
		end;
		output;
	end;
	drop i j;
run;

%macro minory(zbior);

proc iml;

use &zbior;
read all into macierz;

n = ncol(macierz);
ile=0;

do i=1 to n;
	komb=allcomb(n,i);
	ile = ile+nrow(komb);
end;

minory = j(ile,1,0);
z=1;

do i=1 to n;
	komb=allcomb(n,i);
	do j=1 to nrow(komb);
		minory[z,1]=det(macierz[komb[j,],komb[j,]]);
		z=z+1;
	end;
end;

create minory from minory;
append from minory;

quit;

%mend;

%minory(a)

************************************************************;
********************* kolokwium 2008 ***********************;
************************************************************;

*********************** zad.2 *****************************;

proc sql;

select nazwisko, count(nazwisko) as ilemarek
from(
	select distinct k.nazwisko, a.marka
	from kolo.wypozyczenia as w
	join kolo.klienci as k on k.id_klient=w.id_klient
	join kolo.auta as a on a.id_auto=w.id_auto
	)
group by nazwisko
;

*********************** zad.3 *****************************;

data a;
	do i=1 to 7;
		x=floor(10*ranuni(0)+1);
		y=floor(10*ranuni(0)+1);
		output;
	end;
	drop i;
run;

data b;
	do i=1 to 7;
		x=floor(10*ranuni(0)+1);
		output;
	end;
	drop i;
run;

data c;
	do i=1 to 7;
		z=floor(10*ranuni(0)+1);
		output;
	end;
	drop i;
run;

data d;
	input x $;
	cards;
d
h
arg
reat
d
fag
;
run;

data e;
	do i=1 to 17;
		z=floor(10*ranuni(0)+1);
		output;
	end;
	drop i;
run;

%macro mediana(bibl);

proc sql noprint;
	select memname, name
	into :zbiory separated by ' ', :kolumny separated by ' '
	from dictionary.columns
	where libname="%upcase(&bibl)" and memname in ( 
		select memname
		from dictionary.tables
		where libname="%upcase(&bibl)" and nvar=1
		) and type='num'
	;

quit;

data wszystko;

	set

	%let i=1;
	%let zbi=%scan(&zbiory,&i);
	%let kol=%scan(&kolumny,&i);

	%do %while (&zbi ne );
		&zbi (rename=(&kol=wspolnie))

		%let i=%eval(&i+1);
		%let zbi=%scan(&zbiory,&i);
		%let kol=%scan(&kolumny,&i);
	%end;;

run;

proc means data=wszystko median noprint;
	var wspolnie;
	output out=mediana (drop=_type_ _freq_) median=mediana;
run;

proc delete data=wszystko (gennum=all);
run;

%mend;

%mediana(work);

*********************** zad.4 *****************************;

data cos;
	set kolo.c41 (in=a) kolo.c42 (in=b);
	czya=a;
	czyb=b;
run;

data _null_;
	merge c41 c42 (rename=(pyt1-pyt10=p1-p10)) end=k;
	by id_osoby;
	array p(*) _character_;
	
	retain ile 0;

	suma=0;
	do i=1 to 10;
		if p[i] ne p[10+i] then suma=suma+1;
	end;
	if suma=10 then ile=ile+1;
	
	if k then put 'Liczba osob, ktore zmienily odp na wszystkie pytania:   ' ile;
run;

************************************************************;
********************* kolokwium 2009 ***********************;
************************************************************;

*********************** zad.1 *****************************;

data kolo.ufo;
	input a $ b $ c $ d $ e $ aa $ bb $ cc $ dd $ ee $ aaa $ bbb $ ccc $ ddd $ eee $ aaaa $ bbbb $ cccc $ dddd $ eeee;
	cards;
e e e e e e e e e e e e e e e e e e e 1
;
run;

%macro gwiazdki(bibl);

proc sql noprint;
	create table zbiorokolumny as
	select memname, name
	from (
		select memname, name, type 
		from dictionary.columns
		where libname="%upcase(&bibl)" and memname in (
			select memname
			from dictionary.tables
			where libname="%upcase(&bibl)" and nvar=20 and nobs=1
			)
		)
	where type='char' 
	group by memname
	having count(*)=20
	; 

	select distinct memname
	into :zbiory separated by ' '
	from zbiorokolumny
	;

	select distinct name
	into :zmienne separated by ' '
	from zbiorokolumny
	;

quit;

data razem;
	set
	%let i=1;
	%let zb=%scan(&zbiory, &i);

	%do %while (&zb ne );
		&bibl..&zb
		%let i=%eval(&i+1);
		%let zb=%scan(&zbiory, &i);
	%end;;

run;

%let zmmmm=%eval(&i-1);

%do i=1 %to 20;
	%let zm&i=;
%end;

data razemrazem;
	set razem end=k;
	array dane(*) _all_;
	array tab(20) _temporary_;
	
	do i=1 to 20;
		if dane[i]='**' then tab[i]=sum(1,tab[i]);
	end;

	if k then 
		do;
			do i=1 to 20;
				if tab[i]=&zmmmm then 
					do;
						call symput('zm'||catt(i),vname(dane[i]));
					end;
			end;
		end;
run;

proc delete data=razem razemrazem zbiorokolumny (gennum=all);
run;

%put Zmienne spelniajace zalozenia zadania:;
%do i=1 %to 20;
	%if &&zm&i ne %then %put &&zm&i;
%end;

%mend;

%gwiazdki(kolo);

*********************** zad.2 *****************************;

proc sql;
	select nazwisko
	from (
		select distinct k.nazwisko, a.marka
		from kolo.wypozyczenia as w
		join kolo.auta as a on a.id_auto=w.id_auto
		join kolo.klienci as k on k.id_klient=w.id_klient
		)
	group by nazwisko
	having count(marka)= (
		select count(*) as ile
		from (
			select distinct marka
			from kolo.auta
			)
		)
	;

*********************** zad.4 *****************************;

data a;
	array x(5);
	do j=1 to 5;
		do i=1 to 5;
			x[i]=floor(10*ranuni(0)+1);
		end;
		output;
	end;
	drop i j;
run;

proc iml;
reset log print;

use a;
read all into macierz;

n=ncol(macierz);
ile=0;

do i=1 to n;
	komb=allcomb(n,i);
	ile=ile+nrow(komb);
end;

minory=j(ile,1,0);
z=1;

do i=1 to n;
	komb=allcomb(n,i);
	do j=1 to nrow(komb);
		minory[z,1]=det(macierz[komb[j,],komb[j,]]);
		z=z+1;
	end;
end;

maks=max(minory);
g=0;

do i=1 to n;
	komb=allcomb(n,i);
	do j=1 to nrow(komb);
		f=det(macierz[komb[j,],komb[j,]]);
		if f=maks then 
			do;	
				ff=macierz[komb[j,],komb[j,]];
				create najwiekszyminor from ff;
				append from ff;
				g=1;
				stop;
			end;
	end;
	if g=1 then stop;
end;

quit;

*********************** zad.3 *****************************;

data a;
	array x(5);
	do j=1 to 5;
		do i=1 to 5;
			x[i]=floor(10*ranuni(0)+1);
		end;
		output;
	end;
	drop i j;
run;

data a;
	input x y z k;
	cards;
1 2 3 4
1 . 4 5
-1 2 3 -5
. . . 5
. . 6 0
-1 0 . -2 6
. -1 5 6
. . . .
;
run;

data cos;
	set a;
	array wszy(*) _all_;

	ile=0;
	do i=1 to dim(wszy);
		if wszy[i]=. then ile=ile+1;
	end;
	if ile=dim(wszy) then do; sr=.; end;
	else do;
	
	if wszy[1] ne . then do; mi=wszy[1]; ma=mi; end;
	else
		do;
			i=1;
			do while (wszy[i] = .);
				mi=wszy[i+1];
				ma=wszy[i+1];
				i=i+1;
			end;
		end;

	do i=1 to dim(wszy);
		if wszy[i] ne . then
			do; 
				if wszy[i]<mi then mi=wszy[i];
				if wszy[i]>ma then ma=wszy[i];
			end;
	end;
	
	wsk=0;
	krop=0;
	do i=1 to dim(wszy);
		if wszy[i]=mi and wszy[i] ne . then wsk=wsk+1;
		if wszy[i]=. then krop=krop+1;
	end;

	if wsk>1 and krop<dim(wszy)-2 then mii=mi;
	else if krop=dim(wszy)-1 then mii=.;
	else 
		do;
			mii=ma;
			wskk=0;
			do i=1 to dim(wszy);
				if wszy[i]=mi and wszy[i] ne . then wskk=1;
				if wszy[i]<mii and wskk=0 and wszy[i] ne . then mii=wszy[i];
				if wszy[i]=mi and wszy[i] ne . then wskk=0;
			end;
		end;

	sr=(mi+mii)/2;
	end;

	drop i wsk wskk ma ile mi krop mii;
run;

************************************************************;
********************* kolokwium 2010 ***********************;
************************************************************;

*********************** zad.1 *****************************;

proc sql;

select distinct a.marka 
from kolo.wypozyczenia as w
join kolo.auta as a on a.id_auto=w.id_auto
join kolo.klienci as k on k.id_klient=w.id_klient
where k.nazwisko like "N%"
;

quit;










