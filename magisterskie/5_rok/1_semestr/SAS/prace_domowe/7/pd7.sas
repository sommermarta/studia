*** Marta Sommer BSMAD

******************* zad.7.1 **************************;

/* tworze pomocniczy zbiorek, zeby miec wiecej szczegolnych 
   przypadkow, bo ten zbior lab7.a byl jakis za bardzo porzadny :D */

data aa; 
	input x $ 1. y;
	cards;
a 0
a 0
a 2
a 1
a 2
b 0
c 2
c 2
c 3
d 1
d 1
e 1
e 1
e 6
e 6
e 6
e 7
e 7
e 7
e 1
e 1
f 10
g 1
g 2
g 3
g 4
g 5
run;

* a);

proc sql;

select distinct x, max(y) as y
from(
	select *, max(ile_y) as maks_y
	from(
		select *, count(y) as ile_y
		from aa
		group by x, y
		)
	group by x
	)
where ile_y=maks_y
group by x
;

quit;

* b);

proc sql;

select distinct x, y
from(
	select *, max(ile_y) as maks_y
	from(
		select *, count(y) as ile_y
		from aa
		group by x, y
		)
	group by x
	)
where ile_y=maks_y
group by x
;

quit;

* c);

proc sql;

select x 
from(
	select *, count(x) as ilenajmniejszych
	from(
		select *
		from aa
		group by x
		having y=min(y)
		)
	group by x
	)
where ilenajmniejszych=1
;

quit;

* d);

proc sql;

select x
from(
	select *, count(y) as ileigrekow 
	from aa
	group by x, y
	)
group by x
having count(y)=sum(ileigrekow)
;

quit;

* e);

proc sql;

select x
from(
	select x, count(y) as ileroznych
	from(
		select distinct *
		from aa
		group by x, y
		)
	group by x
	)
where ileroznych=(
	select max(ileroznych)
	from(
		select x, count(y) as ileroznych
		from(
			select distinct *
			from aa
			group by x, y
			)
		group by x
		)
	)
;

quit;

* f);

proc sql;

select distinct x
from(
	select *, min(y) as miny, max(y) as maxy, count(y) as ileigrekow
	from(
		select *
		from aa
		)
	group by x
	having maxy=ileigrekow and miny=1
	)
;

quit;

* g);

proc sql;

select distinct x
from(
	select *, min(y) as miny, max(y) as maxy, count(y) as ileigrekow
	from(
		select distinct *
		from aa
		)
	group by x
	having maxy=ileigrekow and miny=1
	)
;

quit;

* h);

proc sql;

select distinct y
from(
	select *, count(x) as wiluxjestteny
	from(
		select distinct *
		from aa
		)
	group by y
	)
where wiluxjestteny >= 0.5*(
	select count(distinct x)
	from aa
	)
;

quit;

******************* zad.7.2 **************************;

* a);

proc sql;

select distinct id
from lab7.z3
where id not in(
	select distinct id
	from lab7.z3
	where year < 1993
	)
order by id
;

quit;

* b);

proc sql;

select distinct id
from lab7.z3
where id in(
	select id
	from(
		select id, year, min(year) as minrok
		from lab7.z3
		)
	where year=minrok
	) and id in(
		select id
		from(
			select id, year, max(year) as maxrok
			from lab7.z3
			)
		where year=maxrok
		)
;

quit;

* c);

proc sql;

select id
from(
	select id, year, count(year) as wilulatach
	from lab7.z3
	group by id
	)
where wilulatach=(
	select count(distinct year)
	from lab7.z3
	)
;

quit;

******************* zad.7.3 **************************;

* tylko w z1;

proc sql;

select count(distinct x)
from lab7.z1
where x not in(
	select x
	from lab7.z2
	)
;

quit;

* tylko w z2;

proc sql;

select count(distinct x)
from lab7.z2
where x not in(
	select x
	from lab7.z1
	)
;

quit;

* w z1 i z2;

proc sql;

select count(distinct x)
from lab7.z2
where x in(
	select x
	from lab7.z1
	)
;

quit;

******************* zad.7.4 **************************;

* a);

proc sql;

select distinct a1, x1
from lab7.b as pierwszy
where x1 <= (
	select max(x2)
	from lab7.b as drugi
	where pierwszy.a1=drugi.a2
	group by a2
	) and x1>= (
	select min(x2)
	from lab7.b as drugi
	where pierwszy.a1=drugi.a2
	group by a2
	)
;

quit;

* b);

proc sql;

select a1
from(
	select a1, ilewszystkich, max(ilewszystkich) as max
	from(
		select *, sum(ilea1, ilea2) as ilewszystkich
		from(
			select a1, count(a1) as ilea1
			from lab7.b
			group by a1
			) as pierwszy 
		join (
			select a2, count(a2) as ilea2
			from lab7.b
			group by a2
			) as drugi on pierwszy.a1=drugi.a2
		)
	)
where ilewszystkich=max
;

quit;

******************* zad.7.5 **************************;

* a);

proc sql;

select miesiac
from(
	select miesiac, ilebrakow, max(ilebrakow) as maxbrakow
	from(
		select miesiac, count(miesiac) as ilebrakow
		from(
			select month(dzien) as miesiac
			from lab7.c
			where r2=.
			)
		group by miesiac
		)
	)
where ilebrakow=maxbrakow
;

quit;

* b);

proc sql;

select miesiac
from(
	select *, max(rozrzut) as maxrozrzut
	from(
		select miesiac, max(r1)-min(r1) as rozrzut
		from(
			select r1, month(dzien) as miesiac
			from lab7.c
			where r1 ne .
			)
		group by miesiac
		)
	)
where rozrzut=maxrozrzut
;

quit;
