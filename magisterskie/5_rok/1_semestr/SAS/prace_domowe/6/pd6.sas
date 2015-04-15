* Marta Sommer BSMAD - praca domowa; 

******************** zad.6.1 ************************;

data a;
	array z(10);
	do i=1 to 20;
		do j=1 to 10;
			z(j)=ranuni(0);
		end;
		output;
	end;
	keep z:;
run;

data a_transp;
	set a end=k;
	array z(*) _all_;
	array tab(10,20) _temporary_;
	array zz(20);
	do i=1 to dim(z);
		tab(i,_n_)=z(i);
	end;
	if k then
		do;
			do i=1 to dim1(tab);
				do j=1 to dim2(tab);
					zz(j)=tab(i,j);	
				end;
				output;
			end;
		end;
	keep zz:;
run;

******************** zad.6.2 ************************;

data zad2a;
	set lab6.z1;
	if dat^=.;
	t=put(dat,date9.);
	dat2=catt('d:',t);
	drop t;
run;

proc transpose data=zad2a out =zad2 (drop=_name_);
	by art;
	id dat;
	idlabel dat2;
run;

******************** zad.6.3 ************************;

proc transpose data=lab6.z out=zz (rename=(_name_=literki));
run;

proc sort data=zz out=zzz;
	by literki;
run;

proc transpose data=zzz out=zzzz (drop=_name_);
	id literki;
run;

******************** zad.6.4 ************************;

data aa;
	retain nr 0 ost;
	set lab6.a;
	if _n_=1 then ost=wsk;
	if wsk>ost then
		do;
			ost=wsk;
			output;
		end;
	else
		do;
			ost=wsk;
			nr=nr+1;
			output;	
		end;
	drop ost;
run;

proc transpose data=aa out=aaa (drop=_name_ nr);
	by nr;
	id wsk;
run;

******************** zad.6.5 ************************;

proc sql;

select avg(x) as sr_x, avg(y) as sr_y, avg(z) as sr_z 
from lab6.b
;

select max(x) as max_x, max(y) as max_y, max(z) as max_z 
from lab6.b
;

create table zad5 as
select x, y, z, mean(x,y,z) as sr, max(x,max(y,z)) as max
from lab6.b
;

select count(*) label='Ile brakow danych w x'
from lab6.b
where x is missing
;

select count(*) label='Ile x wiekszych niz 10'
from lab6.b
where x>10
;

select count(*) label='Ile sum wiekszych niz 30'
from lab6.b
where sum(x,y,z)>30
;

quit;

******************** zad.6.6 ************************;

proc sql;

select mean(w) label='Srednia wynikow osoby A'
from lab6.wyniki
where o='A'
;

select max(w) label='Najlepszy wynik osoby C w drugiej polowie roku'
from lab6.wyniki
where o='C' and k in (3,4)
;

quit;

* 1;

proc sort data=lab6.wyniki out=lab66;
	by o;
run;

data zad6a;
	retain suma 0;
	set lab66;
	by o; 
	suma=suma+w;
	if last.o=1 then do; srA=suma/_n_; output; stop; end;
	keep srA;
run;

* 2;

data zad6b;
	retain max 0;
	set lab6.wyniki;
	where o='C' and (k=3 or k=4);
	max=max(max,w);
	keep max;
run;

data zad6bb;
	set zad6b end=k;
	if k then output;
run;


******************** zad.6.7 ************************;

proc sql;

select o, avg(w) as srednia
from lab6.wyniki
group by o
order by srednia descending
;

select distinct k
from lab6.wyniki
group by o
having w=max(w)
;

select o
from(
	select *
	from lab6.wyniki
	group by o
	having w=max(w)
	)
where k in (1,2)
;

select r, k, o
from lab6.wyniki
group by r, k
having w=max(w)
;

quit;

* 1;

proc sort data=lab6.wyniki out=a;
	by o;
run;

data aa;
	retain suma 0 nr 0;
	set a;
	by o;
	nr=nr+1;
	suma=suma+w;
	if last.o then
		do;
			sr = suma/nr;
			output;
			nr=0;
			suma=0;
		end;
	keep o sr;
run;

proc sort data=aa out=aaa;
	by descending sr;
run;

* 2;

proc sort data=lab6.wyniki out=a;
	by o descending w;
run;

data _null_;
	retain m;
	array kwartaly(4) _temporary_;
	set a end=koniec;
	by o descending w;
	if first.o then 
		if last.w then do; m=w; output; kwartaly(k)=1; end;
		else do; m=w; output; kwartaly(k)=1; end;
	else
		if w=m then do; output; kwartaly(k)=1; end;
	if koniec then
		do;	
			do i=1 to 4;
				if kwartaly(i) then put format i roman.;
			end;
		end;
run;

* 3;

proc sort data=lab6.wyniki out=a;
	by o descending w;
run;

data aa;
	retain m;
	set a end=koniec;
	by o descending w;
	if first.o then 
		if last.w then do; m=w; output; end;
		else do; m=w; output; end;
	else
		if w=m then do; output; end;
run;

data aaa;
	set aa;
	if k<=2 then output;
run;

data _null_;
	set aaa;
	by o;
	if first.o then put o;
run;

* 4;

proc sort data=lab6.wyniki out=a;
	by r k descending w;
run;

data aa;
	retain m;
	set a;
	by r k descending w;

	if first.r then do; m=w; output; end;
	else
		if first.k then do; m=w; output; end;
		else 
			if w=m then output;
	drop m;
run;

******************** zad.6.8 ************************;

proc sql;

select count(*)
from lab6.wyniki
where o='A' and w > (select min(w)
					 from lab6.wyniki
					 where o='B'
					 )
;

select o, rozrzut
from(
	select o, max(w)-min(w) as rozrzut
	from lab6.wyniki
	group by o
	having rozrzut=(select max(w)-min(w)
					from lab6.wyniki
					group by o;
					)

;

select o,max(w)-min(w) as rozrzut
from lab6. wyniki
group by o
having max(w)-min(w)=(
				select max(rozrzut)
				from(
					select max(w)-min(w) as rozrzut
					from lab6.wyniki
					group by o
					)
				)
;

quit;

* 1;

proc sort data=lab6.wyniki out=a (drop=r k);
	by w o;
run;

data aa;
	retain ile 0 suma 0;
	set a end=koniec;
	if o='B' and ile=0 then
		do;
			ile=1;
			wynikb=w;
		end; 
	if ile=1 and o='A' then
		do;
			suma=suma+1;
		end;
	if koniec then put suma;
run;

* 2;

proc sort data=lab6.wyniki out=a;
	by o descending w;
run;

data aa;
	retain max;
	set a;
	by o descending w;
	if first.o then max=w;
	if last.o then 
		do;
			min=w;
			roznica=max-min;
			output;
		end; 
	keep roznica o;
run;

proc sort data=aa out=aaa;
	by descending roznica;
run;

data _null_;
	retain max;
	set aaa;
	by descending roznica;
	if _n_=1 then max=roznica;
	if roznica=max then put o;
run;

******************** zad.6.9 ************************;

proc sql;

select k
from(
select *
from lab6.wyniki
group by o
having w=max(w)
)
group by k
having count(distinct o)>1
;

quit;


proc sort data=lab6.wyniki out=a;
	by o descending w;
run;

data _null_;
	retain m;
	array kwartaly(4) _temporary_;
	set a end=koniec;
	by o descending w;
	if first.o then 
		if last.w then do; m=w; output; kwartaly(k)= sum(kwartaly(k),1); end;
		else do; m=w; output; kwartaly(k)= sum(kwartaly(k),1); end;
	else
		if w=m then do; output; kwartaly(k)= sum(kwartaly(k),1); end;
	if koniec then
		do;	
			do i=1 to 4;
				if kwartaly(i)>1 then put format i roman.;
			end;
		end;
run;

******************** zad.6.10 ************************;

proc sql;

create table d as
select distinct k, w, o
from(
select k, w, o
from lab6.wyniki
group by k 
having o='D' 
)
group by k
having w=min(w)
;

select k, w, o
from lab6.wyniki as pierwszy
group by k 
having o='C' and w=max(w,(select w from d as drugi where pierwszy.k=drugi.k or drugi.k=.))
;

select k, w, o
from lab6.wyniki as pierwszy
group by k 
having o='C' and w>(select w from d as drugi where pierwszy.k=drugi.k)
;

quit;


proc sort data=lab6.wyniki out=a;
	by k o;
run;

data aa;
	retain minD;
	array d(4) _temporary_;
	set a end=koniec;
	by k o;
	if o='D' then if min(minD,w)=w then minD=w;
	if last.k then do; d(k)=minD; minD=.; end;
	if koniec then
	do;
		do i=1 to ile;
			set a nobs=ile point=i;
			minD=d(k);
			output;
		end;
	end;
run;

data zad10a;
	set aa;
	if o='C' and w>minD then output;
	keep k w o;
run;

data zad10b;
	set aa;
	if o='C' and w>minD and minD ne . then output;
	keep k w o;
run;
