* Marta Sommer BSMAD

****************************** zad.10.1 ***************************************;

data a;
	set lab10.produkty;
	where kod_produktu not in (477,3052);
	start=kod_produktu;
	label=nazwa_produktu;
	fmtname="prod";
	keep start label fmtname;
run;

proc format cntlin=a;
run;

data _null_;
	set lab10.magazyn;
	where ilosc=0 and kod_produktu not in (477,3052);
	file "C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\10\zad1.txt";
	put data kod_produktu kod_produktu prod.;
run;

****************************** zad.10.2 ***************************************;

data a;
	set lab8.maly;
	start=id;
	label='maly';
	fmtname='m';
	drop id;
run;

proc format cntlin=a;
run;

proc means data=lab8.duzy;
	var sales;
	where put(id, m.)='maly';
run;

****************************** zad.10.3 ***************************************;

data a;
	 array z(5);
	 do i=1 to 50;
		do j=1 to 5;
			z(j)=rannor(0)*sqrt(10)+100;
		end;
		output;
	 end;
	 drop i j;
run;

proc means data=a noprint;
	output out=b;
run;

data stat;
	set a b (rename=(_stat_=statystyka));
	if _n_<=50 then statystyka='w'||catt(_n_);
	drop _type_ _freq_;
run;

****************************** zad.10.4 ***************************************;

data a;
	 array z(5);
	 do i=1 to 50;
		do j=1 to 5;
			z(j)=rannor(0)*sqrt(10)+100;
		end;
		output;
	 end;
	 drop i j;
run;

proc means data=a q1 median q3 range noprint;
	output out=b q1= median= q3= range= / autoname;
run;

proc transpose data=b out=c;
run;

data d;
	retain i 0;
	set c (firstobs=3);
	array tab(5) _temporary_;
	array z(5);
	i=i+1;
	tab(i)=col1;
	if i=5 then 
		do; 
			i=0; 
			do j=1 to dim(tab);
				z(j)=tab(j);
			end;
			output; 
		end;
	keep z:;
run;

data e;
	retain statystyka;
	statystyka='q1       '; output;
	statystyka='median'; output;
	statystyka='q3'; output;
	statystyka='range'; output;
run;

data f;
	set d;
	set e;
run;

data stat;
	set a f;
	if _n_<=50 then statystyka='w'||catt(_n_);
run;

****************************** zad.10.5 ***************************************;

proc format;
	invalue miesiac
'January'=1
'February'=2
'March'=3
'April'=4
'May'=5
'June'=6
'July'=7
'August'=8
'September'=9
'October'=10
'November'=11
'December'=12
;
run;

data a;
	x='January 22, 2011'; output;
	x='October 3, 1991'; output;
	x='May 31, 1956'; output;
run;

data aa;
	set a;
	m=input(scan(x,1),miesiac.);
	d=scan(x,2);
	r=scan(x,3);
	data=mdy(m,d,r);
	spr=put(data,yymmdd10.);
	keep x data spr;
run;

****************************** zad.10.6 ***************************************;

proc means data=lab10.oceny noprint;
	class uczen kod;
	types uczen*kod;
	output out=ocenyy;
run;

data ocenny;
	set ocenyy;
	if _stat_='MEAN';
	drop _type_ _freq_ kiedy _stat_;
run;

proc transpose data=ocenny out=oc (drop=_name_);
	by uczen;
	id kod;
run;


****************************** zad.10.7 ***************************************;

proc means data=lab10.dane noprint;
	class grupa;
	var x y;
	output out=a;
run;

data _null_;
	retain srglobx srgloby mini i 0 ktory;
	set a end=k;
	if _type_=0 and _stat_='MEAN' then
		do;
			srglobx=x;
			srgloby=y;
		end;
	if _stat_='MEAN' and _type_ ne 0 then 
		do;
			if i=0 then do; mini = sumabs(x-srglobx, y-srgloby); ktory=grupa; i=i+1; end;
			aa=sumabs(x-srglobx, y-srgloby);
			if aa < mini then 
				do;
					mini=aa;
					ktory=grupa;
				end;
		end;
	if k then put 'Szukana grupa to:  ' ktory;
run;

****************************** zad.10.8 ***************************************;

proc format;
value $ liczby
'0'='zero'
'1'='jeden'
'2'='dwa'
'3'='trzy'
'4'='cztery'
'5'='piec'
'6'='szesc'
'7'='siedem'
'8'='osiem'
'9'='dziewiec'
;
run;

data a;
	x=1.4; output;
	x=1.7; output;
	x=4.4; output;
	x=1.2; output;
	x=8.2; output;
	x=5.3; output;
	x=0.9; output;
run;

data aa;
	set a;
	a=put(scan(x,1),liczby.);
	b=put(scan(x,2),liczby.);
	slownie=catt(a)||' kropka '||b;
	drop a b;
run;

****************************** zad.10.9 ***************************************;

data a;
	do i=1 to 100;
		x=rannorm(0);
		output;
	end;
	drop i;
run;

proc means data=a noprint median q3 q1;
	output out=aa range=roz median=med q1=drugi q3=trzeci;
run;

data aaa;
	retain odstdol odstgor;
	cos=1;
	set a;
	set aa (keep = med drugi trzeci) point=cos;
	if _n_=1 then 
		do;
			odstdol=med-(trzeci-drugi);
			odstgor=med+(trzeci-drugi);
		end;
	if x>=odstgor or x<=odstdol then 
		do;
			ktora=_n_;
			output;
		end;
	keep ktora x;
run;

****************************** zad.10.10 ***************************************;

proc univariate data=lab10.probka noprint;
	output out=a probn=p;
	qqplot;
	histogram / normal (noprint);
run;

* nie ma podstaw do odrzucenia hipotezy o normalnosci rozkladu;




