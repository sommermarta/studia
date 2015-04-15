/* **************************************************************** */
/* Imie i nazwisko: Marta Sommer*/

/* ZADANIE 1 ******************************* */

data a;
	array x(2);
	do i=1 to dim(x);
		do j=1 to dim(x);
			x[j]=floor(10*ranuni(0));
		end;
		output;
	end;
	drop i j;
run;

data b;
	array x(2);
	do i=1 to dim(x);
		do j=1 to dim(x);
			x[j]=floor(10*ranuni(0));
		end;
		output;
	end;
	drop i j;
run;

data c;
	array x(3);
	do i=1 to dim(x);
		do j=1 to dim(x);
			x[j]=floor(10*ranuni(0));
		end;
		output;
	end;
	drop i j;
run;

data d;
	do i=1 to 2;
		x='a';
		y='a';
		z=1;
		output;
	end;
	drop i;
run;

%macro z1(bib,n);

proc sql;

select memname, name
into :zbiory separated by ' ', :kolumny separated by ' ' 
from dictionary.columns
where libname="%upcase(&bib)" and memname in (
	select memname
	from dictionary.tables
	where libname="%upcase(&bib)" and nobs=2
	) and type='num'
;

quit;

%put &zbiory &kolumny;

%let i=1;
%let zb=%scan(&zbiory,&i);
%let kol=%scan(&kolumny,&i);

%do %while (zb ne );

	%let j=%eval(&i+1);
	%let zbb=%scan(&zbiory,&j);
	%let koll=%scan(&kolumny,&j);

	%do %while (zb2 ne );
		data _null_;
			set &zb (rename=(&kol=pierwszy));
			set &zbb (rename=(&koll=drugi)) end=k;
			retain suma 0;
			rozn=abs(pierwszy-drugi);
			suma=suma+rozn;
			if k then call symput("&zb&kol&zbb&koll",suma);
		run;

		%let j=%eval(&j+1);
		%let zbb=%scan(&zbiory,&j);
		%let koll=%scan(&kolumny,&j);
	%end;

	%let i=%eval(&i+1);
	%let zb=%scan(&zbiory,&i);
	%let kol=%scan(&kolumny,&i);
%end;

%put &Ax1Ax2;

%mend;

%z1(work,2)

/* ZADANIE 2 ******************************* */

data cos;
	set kolo.b1;
	array b(*) _all_;
	array tab(10) _temporary_;

	do i=1 to 10;
		set kolo.grupy point=i;
		array gr(*) _numeric_;

		suma=0;
		do j=1 to 20;
			plus=(b[j+1]-gr[j])*(b[j+1]-gr[j]);
			suma=suma+plus;
		end;
		odl=sqrt(suma);
		tab[i]=odl;
	end;

	m=tab[1];
	do i=2 to 10;
		if tab[i]<m then m=tab[i];
	end;
run;

/* ZADANIE 3 ******************************* */

proc sql;

select marka
from(
	select distinct w.id_klient, a.marka 
	from kolo.wypozyczenia as w
	join kolo.auta as a on a.id_auto=w.id_auto
	)
group by a.marka
having count(id_klient)>=0.5*(
	select count(*)
	from kolo.klienci 
	)
;

quit;

/* ZADANIE 4 ******************************* */


%macro im(a,b);

proc iml;

use &a;
read all into a;

use &b;
read all into b;

if det(a) = 0 then
	do;
		wart=max(abs(eigval(a)[,1]));
		create zad4 from wart[colname='wart_wl'];
		append from wart;
	end;
else
	do;
		x=solve(a,b);
		create zad4 from x;
		append from x;
	end;

quit;

%mend;

%im(kolo.macierz, kolo.wektor)

data a;
	array x(2);
	do i=1 to dim(x);
		do j=1 to dim(x);
			x[j]=floor(10*ranuni(0));
		end;
		output;
	end;
	drop i j;
run;

data b;
	input a;
	cards;
1
3
;
run;

%im(a, b)

