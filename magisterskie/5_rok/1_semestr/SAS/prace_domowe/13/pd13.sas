************************ Zad.13.1 *****************************;

option mprint;

data a;
	do i=1 to 10;
		x=floor(10*ranuni(0)+1);
		y=floor(10*ranuni(0)+1);
		output;
	end;
	drop i;
run;

%macro brakidanych(zbior,n);

data a_braki;
	set &zbior nobs=ile;
	array &zbior&zbior _all_;
	array tab(&n) _temporary_;
	
	if _n_=1 then
		do i=1 to &n;
			pierwszy=floor((ile*dim(&zbior&zbior))*ranuni(0)+1);
			if i=1 then tab[1]=pierwszy;
			else
				do;
					licznik=0;
					do until (licznik = i-1);
						licznik=0;
						do j=1 to i-1;
							if tab[j] ne pierwszy then licznik=licznik+1;
						end;
						tab[i]=pierwszy;
						pierwszy=floor((ile*dim(&zbior&zbior))*ranuni(0)+1);
					end;
				end;
		end;

	do i=1 to dim(&zbior&zbior);
		wsp=(_n_-1)*dim(&zbior&zbior)+i;
		czyusunac=0;
		do j=1 to dim(tab);
			if wsp=tab[j] then czyusunac=1; 	
		end;
		if czyusunac=1 then &zbior&zbior[i]=.;
	end;

	drop i j czyusunac wsp pierwszy licznik;
run;

%mend;

%brakidanych(a,4)
%brakidanych(a,10)

************************ Zad.13.2 *****************************;	

option mprint;
option nosymbolgen;

%macro zad2(lzm, lgrp);

data %do ii=1 %to &lzm; gen&ii (keep=zm&ii) %end;;
	array zmienne $ zm1-zm&lzm;
	%do i=1 %to &lzm; array tab&i(&lgrp) _temporary_; %end; 

	do until (%do ii=1 %to %eval(&lzm-1); suma&ii + %end; suma&lzm = %eval(&lgrp*&lzm));
		%do ii=1 %to &lzm; c&ii = floor(&lgrp*ranuni(0)+1); %end;
		%do ii=1 %to &lzm; zmienne[&ii]='A_'||catt(c&ii); output gen&ii; %end;
		%do ii=1 %to &lzm; suma&ii=0; %end;
		do j=1 to &lgrp;
			%do ii=1 %to &lzm; if c&ii=j then tab&&ii[j]=1; %end;
			%do ii=1 %to &lzm; suma&ii=suma&ii+tab&ii[j]; %end;
		end;
	end;
run;

%do ii=1 %to &lzm; 
proc sort data=gen&ii out=gengen&ii;
by zm&ii;
run;
%end;

data gen;
	merge %do ii=1 %to &lzm; gengen&ii %end;;
run;

%mend;

%zad2(2,3)
%zad2(4,5)

************************ Zad.13.4 *****************************;

%let a=jestem!jestem!;
%let y=%symexist(a);
%put *****&y*****;

%let c='istnieje';
%let y=%symexist(c);
%put *****&y*****;

%symdel c;
%let y=%symexist(c);
%put *****&y*****;

************************ Zad.13.5 *****************************;

data a;
	z='agrae';
	do i=1 to 10;
		x=floor(100*ranuni(0));
		output;
	end;
	keep x z;
run;

data b;
	do i=1 to 5;
		x=floor(10*ranuni(0));
		y=floor(10*ranuni(0));
		z=floor(10*ranuni(0));
		output;
	end;
	keep x y z;
run;

data c;
	do i=1 to 3;
		m='geirug';
		output;
	end;
	drop i;
run;

%macro m(bibl);

proc sql noprint;
	create table dddddd as
	select memname, name
	from dictionary.columns
	where libname="%upcase(&bibl)" and type='num'
	;

select distinct memname
into :zbiory separated by ' '
from dddddd
;

%let i=1;
%let zb=%scan(&zbiory,&i);
%do %while (&zb ne );

	select name
	into :nazkol separated by ','
	from dddddd
	where memname="%upcase(&zb)"
	;

	select max(maxwkol)
	into :max&zb
	from(
		select max(&nazkol) as maxwkol
		from &bibl..&zb
		)
	;

	%let i=%eval(&i+1);
	%let zb=%scan(&zbiory,&i);

%end;


data maksymalne;
	length zbior $ 30;
	%let i=1;
	%let zb=%scan(&zbiory,&i);
	%do %while (&zb ne );
	
		zbior="&zb";
		maksymalnawartosc=&&max&zb;
		output;

		%let i=%eval(&i+1);
		%let zb=%scan(&zbiory,&i);
	
	%end;

run;

proc delete data=dddddd (gennum=all);
run;

%mend;

%m(work)

************************ Zad.13.6 *****************************;

data b;
	do i=1 to 5;
		xgaergt=floor(10*ranuni(0));
		ytwrt=floor(10*ranuni(0));
		qeryz=floor(10*ranuni(0));
		qeraehtyz=floor(10*ranuni(0));
		qeryza=floor(10*ranuni(0));
		output;
	end;
	drop i;
run;

%macro usun(zbior, litera);

%if %sysfunc(countw(&zbior))=2 %then 
	%do;
		%let bibl=%scan(&zbior,1,'.');
		%let zbi=%scan(&zbior,2,'.');

		proc sql noprint;
			select name
			into :kolumny separated by ' '
			from dictionary.columns
			where libname="%upcase(&bibl)" and memname="%upcase(&zbi)"
			;
		
		%let i=1;
		%let j=1;
		%let kol=%scan(&kolumny, &i);

		%do %while (&kol ne );
			
			%let l=%length(&kol);
			%if %substr(&kol,&l,1) ne &litera %then 
				%do;
					%let to&j=&kol;
					%let j=%eval(&j+1);
				%end;

			%let i=%eval(&i+1);
			%let kol=%scan(&kolumny, &i);
		%end;

		data &bibl..&zbi.2;
			set &bibl..&zbi(keep=%do i=1 %to %eval(&j-1); &&to&i  %end;);
		run;

	%end;
%else 
	%do;
		%let bibl=work;
		%let zbi=&zbior;

		proc sql noprint;
			select name
			into :kolumny separated by ' '
			from dictionary.columns
			where libname="%upcase(&bibl)" and memname="%upcase(&zbi)"
			;
		
		%let i=1;
		%let j=1;
		%let kol=%scan(&kolumny, &i);

		%do %while (&kol ne );
			
			%let l=%length(&kol);
			%if %substr(&kol,&l,1) ne &litera %then 
				%do;
					%let to&j=&kol;
					%let j=%eval(&j+1);
				%end;

			%let i=%eval(&i+1);
			%let kol=%scan(&kolumny, &i);
		%end;

		data &bibl..&zbi.2;
			set &bibl..&zbi(keep=%do i=1 %to %eval(&j-1); &&to&i  %end;);
		run;

	%end;

%mend;

%usun(work.b,t)
%usun(b,t)
%usun(work.b,z)
%usun(work.b,a)

************************ Zad.13.7 *****************************;

data a;
	z='agrae';
	do i=1 to 10;
		x=floor(100*ranuni(0));
		output;
	end;
	keep x z;
run;

data b;
	do i=1 to 5;
		x=floor(10*ranuni(0));
		y=floor(10*ranuni(0));
		z=floor(10*ranuni(0));
		output;
	end;
	keep x y z;
run;

data c;
	do i=1 to 3;
		m='geirug';
		output;
	end;
	drop i;
run;

%macro eksportuj(bibl,dir);

proc sql noprint;
	select memname
	into :zbiory separated by ' '
	from dictionary.tables
	where libname="%upcase(&bibl)"
	;

%let i=1;
%let zb=%scan(&zbiory,&i);

%do %while (&zb ne );

	proc export data=&bibl..&zb
    	outfile="&dir.\&zb..txt";
   		delimiter=' ';
	run;
	
	%let i=%eval(&i+1);
	%let zb=%scan(&zbiory,&i);

%end; 

%mend;

%eksportuj(work,C:\Users\Marta\Dropbox\sas\mojeplikiprobne)

************************ Zad.13.8 *****************************;

data a;
 input grupa wart;
 cards;
1 3
2 5
1 4
2 4
1 5
3 1
;
run;

data b;
 input grupa wart ile;
 cards;
1 30 2
2 50 3
1 4 4
2 40 5
1 5 6
3 10 7
;
run;

data c;
 input grupa $ wart ile $;
 cards;
1 3 a
2 5 b
1 4 d
2 4 ew
1 5 e
3 1 b
;
run;

data d;
 input g w ile $;
 cards;
1 3 a
2 5 b
1 4 d
2 4 ew
1 5 e
3 1 b
;
run;


%macro ile(bib, grupa, wart);

proc sql noprint;
	select memname
	into :zbiory separated by ' '
	from dictionary.columns
	where libname="%upcase(&bib)" and type="num" and name in ("&wart","&grupa")
	group by memname
	having count(*)=2
	;

data roboczy;
	set &zbiory;
	keep &grupa &wart;
run;

proc sql noprint;

	select grupa 
	into :gr separated by ' '
	from(
		select &grupa, count(distinct &wart) as ile
		from roboczy
		group by &grupa
		)
	where ile= (
		select max(ile)
		from(
			select &grupa, count(distinct &wart) as ile
			from roboczy
			group by &grupa
			)
		)
	;

proc delete data=roboczy (gennum=all);
run;

%put Grupy dla ktorych bylo najwiecej roznych wartosci: &gr;

%mend;

%ile(work, grupa, wart)


************************ Zad.13.10 *****************************;

data a;
	z='agrae';
	do i=1 to 10;
		x=floor(100*ranuni(0));
		output;
	end;
	keep x z;
run;

data b;
	do i=1 to 5;
		x=floor(10*ranuni(0));
		y=floor(10*ranuni(0));
		z=floor(10*ranuni(0));
		output;
	end;
	keep x y z;
run;

data c;
	do i=1 to 3;
		m='geirug';
		output;
	end;
	drop i;
run;

%macro tworz(bibl);

proc sql noprint;
	create table robocza1 as
	select name, memname
	from dictionary.columns
	where libname="%upcase(&bibl)" 
	;

	select max(ile)
	into :maks
	from (
		select count(*) as ile
		from dictionary.columns
		where libname="%upcase(&bibl)" 
		group by name
		)
	;

proc sort data=robocza1 out=robocza2;
	by name;
run;

proc transpose data=robocza2 out=robocza3;
	var memname;
	by name;
run;

proc transpose data=robocza3 out=przynaleznosc (drop=_name_);
	var %do i=1 %to &maks; col&i %end;; 
	id name;
run;	

proc delete data=robocza1 robocza2 robocza3 (gennum=all);
run;

%mend;

%tworz(work)






