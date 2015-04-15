**************************** Zad.14.1 ********************************;

proc iml;

start min_max(a);

	aaa = a;
	aaa[loc(a=.)] = 0;
	maxmacierzy = max(aaa);
	minmacierzy = min(aaa);
	z = {'max_macierzy','min_macierzy'};
	to = maxmacierzy || minmacierzy;

	create max_min 
	from to[colname=z]; 
	append from to; 

	return("Szukany zbior zostal utworzony w bibliotece WORK");

finish;

a = {-1 -2 ., -4 -5 .};
b = min_max(a);
print b;

quit;

**************************** Zad.14.2 ********************************;

data a;
	do i=1 to 7;
		x=floor(10*ranuni(0)+1);
		y=floor(10*ranuni(0)+1);
		z=floor(10*ranuni(0)+1);
		output;
	end;
	drop i;
run;

%macro usun(zbior, n);

proc sql noprint;
	
	select nobs, nvar
	into :liczbaobs, :liczbazmien
	from dictionary.tables
	where libname='WORK' and memname="%upcase(&zbior)"
	;
	
	select name
	into :nazwyzmien separated by '" "'
	from dictionary.columns
	where libname='WORK' and memname='A'
	;

quit;

%let nazwyzmien={"&nazwyzmien"};

data _null_;
	array tab(&liczbaobs, &liczbazmien) _temporary_;

	licz=0;

	do until (licz = &n);

		wsk=0;

		do until (wsk = 1); 

			i = floor(&liczbaobs*ranuni(0)+1);
			j = floor(&liczbazmien*ranuni(0)+1);	

			if tab[i,j] ne 1 then 
				do; 
					tab[i,j]=1; 
					wsk=1; 
					licz=licz+1;
					call symput('i'||catt(licz),catt(i));
					call symput('j'||catt(licz),catt(j));		 
				end;

		end;
	end;



run;

proc iml;

	use &zbior;
	read all into macierz;

	%do i=1 %to &n;
		macierz[&&i&i,&&j&i]=.;
	%end;

	create braki from macierz[colname=&nazwyzmien];
	append from macierz;

quit;



%mend;

%usun(a,3)
%usun(a,13)
%usun(a,21)
%usun(a,20)

**************************** Zad.14.3 ********************************;

data a;
	input x y;
	cards;
1 2
. 4
. 5
4 5
4 3
. .
8 .
;
run;

proc iml;
reset log print;

use a;
read all into macierz;

do i=1 to ncol(macierz);
	macierz=macierz[loc(macierz[,i]^=.),];
end;

quit;

**************************** Zad.14.4 ********************************;

data srednie;
	do i=1 to 3;
		m=floor(4*ranuni(0));
		output;
	end;
	keep m;
run;

data sigma;
	input c1 c2 c3;
	cards;
4 3 2
3 6 5
2 5 10
;
run;

proc iml;
reset log print;

use sigma;
read all into w;

use srednie;
read all into sr;

n=ncol(w);
c=i(n);

c[1,1]=sqrt(w[1,1]);

do j=2 to n;
	do i=1 to j;
		if i=1 then c[j,i]=w[j,i]/c[i,i];
		else
			do;
				cos=0;
				do k=1 to i-1;
					cos=cos+c[j,k]*c[i,k];
				end;
				c[j,i]=(w[j,i]-cos)/c[i,i];
		end;	
	end;
end;

m=10;
y=j(3,m,0);

do i=1 to 3;
	do j=1 to m;
		y[i,j]=rannor(0);
	end;
end;

wynik=c*y+sr;

kol='z1':'z10';
create probka from wynik[colname=kol];
append from wynik;

quit;

proc corr data=probka;
	var z1-z10;
run;

**************************** Zad.14.5 ********************************;

proc iml;
reset log print;

a={4 0 6,0 1 4,6 4 -1};

wart=eigval(a);
wekt=eigvec(a);

d=diag(wart);
b=t(wekt)*a*wekt;

quit;

**************************** Zad.14.6 ********************************;

data sigma;
	input c1 c2 c3;
	cards;
4 3 2
3 6 5
2 5 10
;
run;

proc iml;
reset log print;

use sigma;
read all into macierz;

n=nrow(macierz);
temp=0;

do i=1 to n;
	wyzn=det(macierz[1:i,1:i]);
	if (wyzn<=0) then temp=temp+0; else temp=temp+1;
end;

if temp=n then CzyDodatnioOkreslona="TAK";
else CzyDodatnioOkreslona="NIE";

quit;


**************************** Zad.14.6 ********************************;













