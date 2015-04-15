/* **************************************************************** */
/* Imie i nazwisko: Marta Sommer BSMAD */

/* ZADANIE 1 ******************************* */

data a;
	array gdzie(2);
	infile "h:\Windows7\Desktop\SAS\kolokwium\k1\a.txt";
	input co $ 1. co2 $ 1. gdzie(*) @;
	if co='x' then do; xx=gdzie(1); yy=gdzie(2); end;
	else do; xx=gdzie(2); yy=gdzie(1); end;
	input / @(xx) x 1. @(yy) y 1.;
	keep x y;
run;

/* ZADANIE 2 ******************************* */

data b;
	array x(10);
	do j=1 to 13;
		do i=1 to dim(x);
			x(i)=floor(10*ranuni(0));
		end;
		output;
	end;
	format x: 2.;
	drop i j;
run;

data bb;
	set b end=k;
	array x(*) _all_;
	array minn(10) _temporary_;
	array maxx(10) _temporary_;
	max=max(of x:);
	min=min(of x:);
	do i=1 to dim(x);
		if x(i)=max then maxx(i)=sum(1,maxx(i));
		if x(i)=min then minn(i)=sum(1,minn(i));
	end;
	if k then
	do;
		do i=1 to dim(x);
			x(i)=maxx(i);
		end;
		output;
		do i=1 to dim(x);
			x(i)=minn(i);
		end;
		output;
	end;
	keep x:;
run;

/* ZADANIE 3 ******************************* */

proc sql;

select distinct r
from(
	select r, count(r) as ilerazy
	from(
		select r
		from kolo.c
		group by o
		having w=max(w)
		)
	group by r
	)
where ilerazy=(
	select max(ilerazy)
	from(
		select r, count(r) as ilerazy
		from(
			select r
			from kolo.c
			group by o
			having w=max(w)
			)
		group by r
		)
	)
;

quit;

/* ZADANIE 4 ******************************* */

data dd;
	retain i 1;
	set kolo.d (rename=(y=yy));
	by x;
	array y(5);
	array tab(5) _temporary_;
	if yy>0 then
		do;
			tab(i)=yy;
			i=i+1;
		end;
	if last.x then
		do;
			i=1;
			do j=1 to dim(tab);
				y(j)=tab(j);
				tab(j)=.;
			end;
			output;
		end;
	drop i yy j;
run;


