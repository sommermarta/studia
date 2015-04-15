* zad.1;

data a2;
	set kol2012.a;
	array li(30) _temporary_;
	array licz(30);
	by g;
	if _n_=1 then do; do i=1 to dim(li); li(i)=0; end; end;
	dlug=length(s);
	li(dlug)=li(dlug)+1;
	if last.g then 
		do;
			do i=1 to dim(li);
				licz(i)=li(i);
				li(i)=0;
			end;
			output;
		end;
	keep g licz:; 
	format g licz: 1.;
run;

* zad.2;

data xx;
	retain k 0 pocz;
	set kol2012.b;
	l=lag(x);
	if x=. and k=0 then 
		do;
			pocz=l;
			k=1;
		end;
	if (pocz ne .) and (x ne .) then 
		do;
			cos=pocz+x;
			output;
			pocz=.;
			k=0;
		end;
	cos=x; 
	if cos ne . then output;
	keep cos;
run;

* zad.3;

proc sql;
	
	select o, count(o)
	from(
	select * 
	from kol2012.c
	having w>(
		select w
		from kol2012.c
		where o="A"
		having w=min(w)
		)
	)
	group by o
	;

quit;

* zad.4;

data dd;
	infile "C:\Users\Marta\Desktop\Marta\studia\rok5\SAS\kolokwium_1\kol2012\plikD.txt" N=9;
	do i=1 to 3;
		input #i id $; 
		input #(i+3) data ddmmyy10.;
		input #(i+6) wynik;
		format data: ddmmyy10.;
		output;
	end;
	drop i;
run;
