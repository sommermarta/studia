* 3.1;

data lab03.grupy2;
	do i=1 to 26;
	x=byte(64+i);
	v=floor(5*ranuni(0));
		do j=1 to v;
			y=ceil(40*ranuni(0));
			output;
		end;
	end;
	keep x y;
run;

data lab03.srby;    * dokladnie to samo - niczym sie nie rozni! ale!!! ukazuja sie ukryte zmienne;
	set lab03.grupy;
	by x;
run;


data lab03.srby;    
	set lab03.grupy;
	by x;
	first=first.x;
	last=last.x;
run;


data lab03.srby;    
	set lab03.grupy;
	by x;
	i+1;
	suma+y;
	if last.x then do;
		sr=suma/i;
		i=0;
		suma=0;
		output;
	end;
	keep x sr;
run;

* 3.5;

proc sort data=lab03.a out=lab03.asort nodupkey;
	by u x;   *najpierw po zmiennej u, a potem po zmiennej x;
run;

data lab03.zad5;
	set lab03.asort;
	by u;
	i+1;
	if last.u then do;
		output;
		i=0;
		end;
run;

proc sort data=lab03.zad5;
	by descending i;   * malejaco;
run;   

* jeszcze jeden data step, ktory wybierze odpowiednie u;
