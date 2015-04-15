* Marta Sommer BSMAD;

******************** Zad.11.1 *******************************;

option mprint;

%macro stworz(prefiks, N, k, l);

%do i=1 %to &N;
	data &prefiks&i;
		do j=1 to &l;
			%do z=1 %to &k;
				x&z=2*ranuni(0)-1;
			%end;
			output;
		end;
		drop j;
	run;
%end;

%mend;

%stworz(ja, 15, 6, 2)

******************** Zad.11.2 *******************************;

data a;
	do i=1 to 10;
		x=floor(10*ranuni(0));
		y=floor(10*ranuni(0));
		maslo=floor(10*ranuni(0));
		output;
	end;
	drop i;
run;

%macro zlicz(zbior, zmienne, n);

%let ile=%sysfunc(countw("&zmienne"));
%put &ile;

data _null_;
		%do i=1 %to &ile;
			%let zm&i=%sysfunc(scan(&zmienne, &i));
		%end;
run;

data aa;
	set &zbior end=k;
	array pom(&ile) _temporary_;
	%do i=1 %to &ile;
		if &&zm&i>&n then pom(&i)=sum(pom(&i),1);
	%end;
	if k then
		do;
			%do i=1 %to &ile;
				ile_&&zm&i=pom(&i);
			%end;
			output;
		end;
	keep 
		%do i=1 %to &ile;
			ile_&&zm&i
		%end;;
run;

%mend;

%zlicz(a, x y maslo, 0)

******************** Zad.11.3 *******************************;

%macro zrobbezkropek(zbior);

proc transpose data=&zbior out=a;
run;

data _null_;
	set &zbior end=k;
	retain i 0;
	i=i+1;
	if k then call symput('ileob',i);
run;

data aaa;
	set a end=k;
	retain mak 0;
	array tab(&ileob) _temporary_;
	z=1;
	%do i=1 %to &ileob;
		if col&i ne . then 
			do;
				tab(z)=col&i;
				z=z+1;
			end;
	%end;
	%do i=1 %to &ileob;
		col&i=tab(&i);
		if tab(&i) ne . then kiedy=&i;
		tab(&i)=.;
	%end; 
	mak=max(kiedy,mak);
	if k then call symput('makk',trim(left(mak)));
	drop z;
run;

proc transpose data=aaa out=aaaa (drop=_name_);
run;

data bezkropek2;
	set aaaa;
	if _n_<=&makk;
run;

%mend;

%zrobbezkropek(lab11.kropki)

******************** Zad.11.4 *******************************;

data a;
	do i=1 to 50;
		x=floor(11*ranuni(0));
		output;
	end;
	drop i;
run;

* a);

%macro srednia(zbior);

proc transpose data=&zbior out=aaa (keep=_name_);
run;

data _null_;
	set aaa;
	call symput('x',_name_);
run;

data aa;
	merge %do i=1 %to 50;
		a (firstobs=&i obs=50 rename=(&x=x&i))
	%end;;
run;

data srednie;
	set aa;
	sr=mean(of &x:);
	keep sr;	
run;

%mend;

%srednia(a)

* b);

%macro sredniab(zbior);

proc transpose data=&zbior out=aaa;
run;

data srednieb;
	set aaa;
	array tab(*) _numeric_;
	%do j=1 %to 50;
		sum=0;
		ile=0;
		%let jj=%eval(50-&j+1);
		%do i=1 %to &jj;
			sum=tab(50-&i+1)+sum;
			ile=ile+1;
		%end;
		sr=sum/ile;
		output;
	%end;
	keep sr;
run;

%mend;

%sredniab(a)

******************** Zad.11.5 *******************************;

* a);

%macro liczbaslow(slowko);

data _null_;
	x=countw("&&&slowko");
	put x;
run;

%mend;

%let ah=ale oo ile fajnych slowek!;
%let m=    ele mele dudki,     gospodarz malutki!!!! Ot co ...;
%let a=mam tak samo jak ty miasto moje a w nim;
%liczbaslow(ah)
%liczbaslow(m)
%liczbaslow(a)

* b);

%macro slowa(slowko, nazwa);

data _null_;
	x=countw("&&&slowko");
	call symput('ile', x);
run;

data _null_;
	%do i=1 %to &ile;
		a=scan("&&&slowko",&i);
		%global &nazwa&i;
		call symput("&nazwa&i",a);
	%end;
run;

%mend;

%let ah=ale oo ile fajnych slowek!;
%let m=    ele mele dudki,     gospodarz malutki!!!! Ot co ...;
%let a=mam tak samo jak ty miasto moje a w nim;

%slowa(ah, ja)
%slowa(m, todrugie)
%slowa(a, jestemzmiasta)

%put _user_;

******************** Zad.11.6 *******************************;

%macro silnia(n);

%global s;
%if &n=1 %then %let s=1;
%else
	%do;
		%silnia(%eval(&n-1));
		%let s=%eval(&s*&n);
	%end;

%mend;

%silnia(1)
%put &s;

%silnia(3)
%put &s;

%silnia(20)
%put &s;

******************** Zad.11.7 *******************************;

%macro slowa(slowko, nazwa, gdzieile);

data _null_;
	%global &gdzieile;
	x=countw("&slowko");
	call symput("&gdzieile", x);
run;

data _null_;
	%do i=1 %to &&&gdzieile;
		a=scan("&&slowko",&i);
		%global &nazwa&i;
		call symput("&nazwa&i",a);
	%end;
run;

%mend;

%macro usun(nazwy, znaki);

%slowa(&nazwy, q, tutaj);

%do j=1 %to %sysfunc(countw(&znaki));
	%let qq&j=%sysfunc(scan(&znaki,&j));
%end;

%do i=1 %to &tutaj;
	%let czy=tak;
	%do j=1 %to %sysfunc(countw(&znaki));
		%let oooooo=%sysfunc(findc("&&q&i",&&qq&j));
		%if &oooooo ne 0 %then %let czy=nie;
	%end;
	%if &czy=tak %then %put &&q&i;
%end;

%mend;

%usun(wow wow wiliiiii uuuu ale to bylo dobre! hej, o u h)

******************** Zad.11.8 *******************************; 

%macro tworz(napis, litera);

%let ileslow=%sysfunc(countw(&napis));

%do i=1 %to &ileslow;
	%let x=%sysfunc(scan(&napis, &i));
	%let o=%sysfunc(countc(&x,&litera));
	data _null_;
		call symputx("&x", "&o",'g');
	run;
%end;

%mend;

%tworz(ale znowu te napisy mam juz dosc!!! ..., m)
%put _user_;

%tworz(zjez jestemzzzzz drzewem a, z)
%put _user_;

******************** Zad.11.9 *******************************; 

* a);

%macro wygeneruj(n);

data _null_;
	%do i=1 %to &n;
		o=substr('QWERTYUIOPASDFGHJKLZXCVBNM',ceil(26*ranuni(0)),1);
		call symput("z&i",o);
	%end;
run;

%let wczes1=&z1;
%put z1 &z1;

%if &n ne 1 %then 
	%do i=2 %to &n;
		%let czy=nie;
		%do j=1 %to %eval(&i-1);
			%if &&z&j=&&z&i %then %let czy=tak;
		%end;
		%if &czy=nie %then %put z&i &&z&i;
	%end;
run;

%mend;

%wygeneruj(26)

* b);

%macro wygeneruj2(n);

%if &n>26 %then %put N wieksze od 26 - BLAD;
%else
	%do;
		data a;
			array tab(&n) $;
			k=0;
			do while (k ne &n);
				o=substr('QWERTYUIOPASDFGHJKLZXCVBNM',ceil(26*ranuni(0)),1);

				if k=0 then do; tab(1)=o; k=k+1; end;
		
				czy=0;
				do j=1 to k;
					if tab(j)=o then czy=1;
				end;

				if czy ne 1 then
						do;
							k=k+1;
							tab(k)=o;
						end;
			end;
			keep tab:;
		run;

		data _null_;
			set a;
			array tab(*) _all_;
			do i=1 to &n;
				call symputx('z'||left(i),tab(i),'g');
			end;
		run;
	%end;
%mend;

%wygeneruj2(26)
%put _user_;

%wygeneruj2(36)


******************** Zad.11.10 *******************************; 

%macro komb(n,k);

data kombinacje;
	array z(&k);
	do a1=1 to %eval(&n-&k+1);
		%do i=2 %to &k;	
			do a&i=%eval(&i) to %eval(&n-&k+&i);
		%end;
		%do i=1 %to &k;
			z(&i)=a&i;
		%end;	
		output;
		%do i=2 %to &k;
			end;
		%end;
	end;
	keep z:;
run;

%mend;

%komb(10,3);
