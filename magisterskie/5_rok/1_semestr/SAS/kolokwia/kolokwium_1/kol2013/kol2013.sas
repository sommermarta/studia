* zad.1;

data a2;
	set kol2013.a;
	array tab(50) _temporary_;
	array x(*) _all_;
	array y(50);
	k=0;
	do i=1 to dim(x);
		if x(i) ne . then 
			do;
				k=k+1;
				tab(k)=x(i);
			end;
		if i=dim(x) then
			do;
				do i=1 to dim(x);
					y(i)=tab(i);
					tab(i)=.;
				end;
				output;
			end;
	end; 
	keep y:;
	format y: 1.;
run;

* zad.2;

proc sql;

	select count(*) label="Liczba kwartalow"
	from(
	select distinct k
	from kol2013.b
	group by o 
	having avg(w)<w
	)
	;

quit;

* zad.3;

data cc;
	length t $ 100;
	array licz(4);
	array gdzie(2);
	infile "C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\kolokwium_1\kol2013\c.txt";
	input data yymmdd10. licz(*) / gdzie(*) / t $;
	kwartal=qtr(data);
	liczba=licz(kwartal);
	pocz=gdzie(1);
	kon=gdzie(2);
	tekst=substr(t,pocz+1,kon-pocz-1);
	format data yymmdd10.;
	keep data liczba tekst;
run;

* zad.4;

data dddd;
	retain ile 0 dni 1 suma 0 data;
	set kol2013.d end=k;
	by d;

	if _n_=1 then data=d;
	if first.d then
		do;
			if (d-data) ne 1 then do; suma=suma+(d-data-1)*ile; dni=dni+(d-data-1); end;
		end;

	if in="*" then ile=ile+1;
	if out="*" then ile=ile-1;

	if last.d then 
		do;
			dni=dni+1;
			suma=suma+ile;
			data=d;
		end;
	if k then do; sr=suma/dni; put "Srednie dzienne oblozenie szpitala: " sr; end;
run;

