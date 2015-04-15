* Marta Sommer BSMAD pd4

********************* Zad.5.1 ***************************;

data aa;
	set lab5.a (firstobs=1 obs=1);
	array k(*) _all_;
	array kk(10) _temporary_;
	if _n_=1 then 
	do;
		do i=1 to dim(kk);
			kk(i)=k(i);
		end;
	end;
	i=1;
	do until (kk(i)=.);
		b=kk(i);
		set lab5.a point=b; 
		output;
		i=i+1;
	end;
	drop i;
run;

********************* Zad.5.2 ***************************;
	
data _null_;
	set lab5.drzewo (firstobs=1 obs=1);
	put '1';
	nast=1;
	do until (nast=.);
		set lab5.drzewo point=nast;
		ktory=floor(2*ranuni(0));
		if prawy=. and lewy=. then leave;
		if ktory=0 then 
			do;
				if lewy=. then nast=prawy; else nast=lewy;
				put nast; 
			end;
		else
			do;
				if prawy=. then nast=lewy; else nast=prawy;
				put nast;
			end;
	end;
run;

********************* Zad.5.3 ***************************;

data p1;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\p1.txt' dlm='x' missover;
	input numer wzrost waga plec $ wiek imie $;
run;

data p2;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\p2.txt' missover;
	input numer 1 indeks 2-6 plec $ 7 wiek 8-9 imie $;
run;

data p3;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\p3.txt' missover;
	input numer imie $ nazwisko $ liczba: comma10.2 data yymmdd8.;
run;

data p4;
	length nazwisko $ 15;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\p4.txt' missover;
	input imie $ / nazwisko $ / numer;
run;

********************* Zad.5.4 ***************************;

data eksp;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\eksperyment.txt' missover;
	input data yymmdd8. numer czas $;
run;

data eksp2;
	retain pocz;
	set eksp;
	if czas='START' then pocz=data;
	if czas='STOP' then 
		do;
			kon=data;
			ileczasu=kon-pocz;
			output;
		end;
	keep ileczasu;
run;

data eksp3;
	retain ilestart max 0 kmax 0 k 0;
	set eksp end=e;
	if czas='START' then do; ilestart=numer; k=k+1; end;
	if czas='STOP' then 
		do;
			ilestop=numer;
			rozn=ilestart-ilestop;
			if rozn > max then do; max=rozn; kmax=k; end;
		end;
	if e then put 'W eksperymencie numer: ' kmax;
run;

********************* Zad.5.5 ***************************;

data zad5;
	retain a;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\plikB.txt' missover;
	input data ddmmyy10. @'r1' r1 3. @1 @'r2' r2 3. @1 @'r3' r3 3. @1 @'r4' r4 3. @1 @;
	format data ddmmyy10.;	
	if data=. then data=a;
	a=data;
	drop a;
run;

********************* Zad.5.6 ***************************;

data zad6;
	retain a;
	length co $ 10;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\plikC.txt' missover;
	input nr co $ l1: comma10. l2: comma10. data ddmmyy10. @;
	format data ddmmyy10.;
	if data=. then data=a;
	a=data;
	drop a;
	x = scan(co, 1, '/');
	ile=l1;
	output;
	x = scan(co, 2, '/');
	ile=l2;
	output;
	keep nr x ile data;
run;

********************* Zad.5.7 ***************************;

data zad7;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\plikD.txt';
	input id $ ile litera $ @;
	do while (litera='x');
		input ile litera $ @;
	end;
	drop litera;
run;

********************* Zad.5.8 ***************************;

data zad8;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\p.txt' n=10;
	array x(10);
	array k(10);
	input x(*);
	do i=1 to dim(x);
		if x(i)^=. then do; input #(x(i)) k(*); output; end;
	end;
	keep k:;
run; 

********************* Zad.5.9 ***************************;

data zad9;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\braki.txt';
	input a @;
	do while (a=.);
		input a @;
	end;
	input b @;
	do while (b=.);
		input b @;
	end;
	input c @;
	do while (c=.);
		input c @;
	end;
run;

********************* Zad.5.10 ***************************;

data zad10;
	retain z 0;
	infile 'C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\5\lab05\bloki.txt' missover;
	array liczby(4,12) _temporary_;
	input rok @;
	array lata(*) rok2004-rok2007;
	z=z+1;
	do i=1 to 4;
		if rok=2003+i then 
			do;
				do j=1 to 12;
					input liczby(i,j) comma1. +1 @;
				end;
			end;
	end;
	if z=4 then 
	do;
		do i=1 to 12;
			do j=1 to 4;
				lata(j)=liczby(j,i);
			end;
			output;
		end;
		z=0;
	end;
	keep rok2:;
run;
