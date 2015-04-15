* Marta Sommer BSMAD;

***************************** zad.8.1 ***************************************;

data _null_;
	retain ile k 1;
	set lab8.z2 nobs=zz2; 
	set lab8.z1 (rename=(x=xx)) nobs=zz;
	if _n_=1 then ile=max(zz2,zz)-min(zz2,zz);
	if x^=xx then ile=ile+1;
	if k=min(zz,zz2) then put 'Ile roznych:  ' ile;
	k=k+1;
run;

***************************** zad.8.2 ***************************************;

proc sort data=lab8.maly out=maly2;
	by id;
run;

proc sort data=lab8.duzy out=duzy2;
	by id;
run;

data _null_;
	retain ile 0 suma 0;
	merge maly2 (in=czywmaly) duzy2 end=k;
	by id;
	if czywmaly then 
		do;
			suma=suma+sales;
			ile=ile+1;
		end;
	if k then 
		do;
			sr=suma/ile;
			put 'Srednia:  ' sr;
		end;
run;

***************************** zad.8.3 ***************************************;

data _null_;
	retain suma 0 ile 0;
	set lab8.numery end=k;
	set lab8.duzy point=nr;
	suma=suma+sales;
	ile=ile+1;
	if k then 
		do;
			sr=suma/ile;
			put 'Srednia:  ' sr;
		end;
run;

***************************** zad.8.4 ***************************************;

data zz1 (keep=z1) zz2 (keep=z2) zz3 (keep=z3);
	set lab8.kropki;
	if z1 ne . then output zz1;
	if z2 ne . then output zz2;
	if z3 ne . then output zz3;
run;	

data bezkropek2;
	set zz1;
	set zz2;
	set zz3;
run;

***************************** zad.8.5 ***************************************;

data liczby;
	do i=1 to 50;
		x=floor(10*ranuni(0));
		output;
	end;
	keep x;
run;

data sumy;
	set liczby (rename=(x=x1));
	set liczby (rename=(x=x2)  firstobs=2); 
	set liczby (rename=(x=x3)  firstobs=3); 
	set liczby (rename=(x=x4)  firstobs=4); 
	set liczby (rename=(x=x5)  firstobs=5); 
	suma=sum(of x:);
	keep suma;
run;

***************************** zad.8.6 ***************************************;

proc sql noprint;

create table aa as
select *, count(y) as licznik
from lab8.a
group by x
having count(y)>5
;

quit;

data aaa;
	retain ile 0 ktory 0 licznik 0 suma 0;
	set lab8.a end=k nobs=ileile;
	by x;
	licznik=licznik+1;
	if ile=0 then ktory=ktory+1;
	ile=ile+1;
	if last.x and ile>5 then czy=1; 
	if czy=1 then
		do;
			do i=suma+1 to licznik;
				set lab8.a point=i;
				output;
			end;
		end;	
	if last.x then do; suma=suma+ile; ile=0; end;
	keep x y ile;
	rename ile=licznik;
run;

***************************** zad.8.7 ***************************************;

data w;
	merge lab8.zb1 (rename=(x=x1)) lab8.zb2 (rename=(x=x2)) lab8.zb3 (rename=(x=x3)) lab8.zb4 (rename=(x=x4)) lab8.zb5 (rename=(x=x5));
	by data;
	l=lag(data);
	ll=data-l;
	if ll=1 or _n_=1 then output;
	if ll ne 1 and _n_ ne 1 then
		do;
			dzien=data;
			xx1=x1;
			xx2=x2;
			xx3=x3;
			xx4=x4;
			xx5=x5;
			do i=1 to ll-1;
				data=l+1;
				x1=.;
				x2=.;
				x3=.;
				x4=.;
				x5=.;
				output;
				ll=l+1;
			end;	
			data=dzien;
			x1=xx1;
			x2=xx2;
			x3=xx3;
			x4=xx4;
			x5=xx5;
			output;
		end;
	keep data x:;
	drop xx:;
run;

***************************** zad.8.8 ***************************************;

data aa;
	merge lab8.jan lab8.feb lab8.mar;
	by osoba;
run;

***************************** zad.8.9 ***************************************;

data _null_;
	retain suma 0;
	set lab8.zx (rename=(x=xx)) nobs=ilex;
	set lab8.zy (rename=(y=yy)) nobs=iley;
	set lab8.zxy nobs=ilexy;
	if xx=x and yy=y then suma=suma+1;
	if _n_=min(ilex, iley, ilexy) then put 'Liczba obserwacji, na ktorych sa rowne:  ' suma;
run;

***************************** zad.8.10 ***************************************;

data aaaa;
	retain licznik 1;
	merge lab8.pierwszy lab8.drugi;
	by year;
	array tab(*) _all_;
	p=mod(licznik,12);
	if month=p or p=0 then 
		do; 
			licznik=licznik+1; 
			sales=sum(sales,tab(month+4)); 
			if sales ne . then output; 
		end;
	else
		do;
			m=month;
			s=sales;
			do i=mod(licznik,12) to (m-1);
				month=i;
				sales=.;
				sales=sum(sales,tab(month+4)); 
				if sales ne . then output;
				licznik=licznik+1;
			end;
			month=m;
			sales=s;
			sales=sum(sales,tab(month+4)); 
			licznik=licznik+1;
			if sales ne . then output;
		end; 
	if last.year and month ne 12 then
		do;
			do i=month+1 to 12;
				month=i;
				sales=.;
				sales=sum(sales,tab(month+4)); 
				licznik=licznik+1;
				if sales ne . then output;
			end;
		end;
	if last.year then licznik=1;
	keep year month sales;
run;
