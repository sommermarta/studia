* Marta Sommer BSMAD

*********************** 9.1 *****************************;

* a);

proc sql;

select nr_miejsca_wyp, ilerazy
from(
	select *, max(ilerazy) as maks
	from(
		select nr_miejsca_wyp, count(*) as ilerazy
		from lab9.wypozyczenia
		where input(data_wyp, yymmdd10.) >= input('1999-01-01',yymmdd10.) 
			and input(data_wyp, yymmdd10.) <= input('1999-06-06',yymmdd10.)
		group by nr_miejsca_wyp
		)
	)
where ilerazy=maks
;

* b);

select t.nr_klienta, t.nazwisko
from lab9.klienci as t
where t.nr_klienta in(
	select nr_klienta 
	from lab9.wypozyczenia 
	group by nr_klienta
	having count(nr_klienta)>=2
	) and t.nr_klienta in (
		select nr_klienta
		from lab9.wypozyczenia as w, lab9.samochody as s 
		where s.marka='OPEL' and s.nr_samochodu=w.nr_samochodu 
		)
;

* c);

select distinct nr_miejsca_wyp, nr_samochodu, 
	(input(data_odd,yymmdd10.)-input(data_wyp,yymmdd10.)) as dlugosc_wypozyczenia  
from lab9.wypozyczenia
where input(data_wyp,yymmdd10.) >= input('1998-10-01',yymmdd10.) 
	and input(data_wyp,yymmdd10.) <= input('1998-12-31',yymmdd10.)
group by nr_miejsca_wyp
order by nr_miejsca_wyp, dlugosc_wypozyczenia
;

* d);

select distinct w.nr_klienta, k.nazwisko
from lab9.wypozyczenia as w, lab9.samochody as s, lab9.klienci as k 
where w.nr_samochodu=s.nr_samochodu and w.nr_klienta=k.nr_klienta
group by w.nr_klienta
having count(*)>1 and count(distinct marka)=count(*)
;


* e);

select nr_pracownika, imie, nazwisko
from lab9.pracownicy
where nr_pracownika not in (
	select distinct nr_pracow_wyp
	from lab9.wypozyczenia
	where input(data_wyp,yymmdd10.) >= input('1999-10-01',yymmdd10.) 
		and input(data_wyp,yymmdd10.) < input('2000-03-01',yymmdd10.)
	)
;

* f);

select nr_wypozyczenia, pw.nazwisko as nazwisko_prac_wyp, po.nazwisko as nazwisko_prac_prz
from lab9.wypozyczenia, lab9.pracownicy as pw, lab9.pracownicy as po
where (nr_miejsca_wyp ne nr_miejsca_odd) and (nr_miejsca_odd ne "") and 
	nr_pracow_wyp=pw.nr_pracownika and nr_pracow_odd=po.nr_pracownika
;

* g);

select *
from(
	select nr_pracow_wyp, sum((input(data_odd,yymmdd10.)-input(data_wyp,yymmdd10.)+1)*cena_jedn) as dochod
	from lab9.wypozyczenia
	where nr_pracow_wyp in (
		select nr_pracownika
		from lab9.pracownicy
		where year(input(data_zatr,yymmdd10.))<1998
		) and year(input(data_wyp,yymmdd10.))=1999 and data_odd ne ""
	group by nr_pracow_wyp
	)
having dochod=max(dochod)
;

* h);

select data_wyp, data_odd, nazwisko as nazw_pracow_wyp, 
	(input(data_odd,yymmdd10.)-input(data_wyp,yymmdd10.)+1)*cena_jedn as cena_wypoz
from lab9.wypozyczenia, lab9.pracownicy
where nr_samochodu="000006" and nr_pracownika=nr_pracow_wyp
;

quit;

*********************** 9.2 *****************************;

proc sql;

select *
from(
	select d.instrument, d.data, pomiar
	from lab9.daty as d, lab9.pomiary as p
	where p.instrument=d.instrument
	group by d.instrument, d.data
	having max(d.data-p.data,p.data-d.data)=min(max(d.data-p.data,p.data-d.data))
	)
group by instrument, data
having max(pomiar)=pomiar
;

quit;

*********************** 9.3 *****************************;

proc sql;

select avg(sales) as srednia
from lab8.duzy as d, lab8.maly as m
where d.id=m.id
;

quit;

*********************** 9.4 *****************************;

data razem;
	merge lab9.a (in=ina) lab9.b (in=inb);
	by a b c;
	if inb;
	if inb and not ina then indyk=1;
run;

proc sql;

select coalesce(i.a,b.a) as a, coalesce(i.b,b.b) as b, coalesce(i.c,b.c) as c, indyk
from lab9.b as b
left join (
	select *, 1 as indyk
	from(
		select *
		from lab9.b 
		except 
		select *
		from lab9.a
		)
	) as i on i.a=b.a and i.b=b.b and i.c=b.c 
;

quit;

*********************** 9.5 *****************************;

proc sql;

* pierwszy chodzi na te same przedmioty, co drugi (ale drugi moze chodzic na jakies inne jeszcze) ;

select pierwszy, drugi
from (
	select id_studenta as pierwszy, id_przedmiotu as prz1, count(*) as ile
	from lab9.studenci
	group by id_studenta
	), (
		select id_studenta as drugi, id_przedmiotu as prz2
		from lab9.studenci
		) 
where pierwszy ne drugi and prz1=prz2
group by pierwszy, drugi
having count(*)>=ile
;

* pierwszy chodzi na dokladnie te same przedmioty, co drugi;

select pierwszy, drugi
from (
	select id_studenta as pierwszy, id_przedmiotu as prz1, count(*) as ile
	from lab9.studenci
	group by id_studenta
	), (
		select id_studenta as drugi, id_przedmiotu as prz2, count(*) as ile2
		from lab9.studenci
		group by id_studenta
		) 
where pierwszy ne drugi and prz1=prz2
group by pierwszy, drugi
having count(*)=ile and ile=ile2
;

quit;
