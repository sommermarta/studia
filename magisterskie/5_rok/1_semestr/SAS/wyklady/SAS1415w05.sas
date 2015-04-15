* pisanie do plikow tekstowych;

filename plik 'C:\Users\mini\Documents\My SAS Files\plik.txt';

data _null_;
 file plik;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0);
  put x y;
 end;
run;

data _null_;
 file plik dlm='09'x;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0);
  put x y;
 end;
run;

data a;
 infile plik;
 input z b;
run;

data a;
 infile plik dlm='09'x;
 input z b;
run;

data a b c;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0); z=ranuni(0);
  output ;
 end;
run;

data a b c;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0); z=ranuni(0);
  if x+y+z>0.5 then output a b ;
   else output c;
 end;
run;

data a b c;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0); z=ranuni(0);
  if x+y+z>0.5 then output a b ;
   else output c;
 end;
 keep x;
run;

data a (keep=x y) b(drop=y) c;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0); z=ranuni(0);
  if x+y+z>0.5 then output a b ;
   else output c;
 end;

run;

data a (keep=x y) b(drop=y) c;
 do i=1 to 10;
  x=ranuni(0); y=ranuni(0); z=ranuni(0);
  if x+y+z>0.5 then output a b ;
   else output c;
 end;
 rename x=xxx;
run;

data cc;
 set b (rename=(xxx=x));
run;

*data set options;

data a;
 input x y;
 cards;
10 3
2 9
2 10
4 1
4 2
2 5
;
run;

proc sql;

 select avg(y)
 from a
 ;

 select *,avg(y)
 from a
 ;

 select *
 from a
 group by x
 ;

 select avg(y)
 from a
 group by x
 ;

 select x,avg(y)
 from a
 group by x
 ;

 select x,avg(y) as srednia_y
 from a
 group by x
 ;

 select *,avg(y) as srednia_y
 from a
 group by x
 ;

  select x,avg(y)
 from a
 group by x
 having avg(y)>2
 ;

  select avg(y)
 from a
 group by x
 having avg(y)>2
 ;

  select *,avg(y)
 from a
 group by x
 having avg(y)>2
 ;

 select *
 from a
 group by x
 having avg(y)>2
 ;

 *wybrac y, ktore sa wieksze niz globalna srednia y;

  select *
  from a
  where y>avg(y)
  ;

  select y
  from (select *,avg(y)
        from a
	    )
  where y>avg(y)
  ;

  select y
  from (select *,avg(y) as srednia_y
        from a
	    )
  where y>srednia_y
  ;

  select *,avg(y) as srednia_y
        from a;


  select y
  from a
  where y>(select avg(y)
           from a
		   )
		   ;
*podzapytanie nieskorelowane, uncorrelated subquery;

		   select y
  from a
  where y>(select x
           from a
		   )
		   ;

 *wybrac wiersze, w ktorych x sa wieksze od wszystkich y;
select x,y
from a
where x>(select y from a)
;

select x,y
from a
where x in (select y from a)
;

select x
from a
where x>any (select y from a)
;

select x
from a
where x>all (select y from a)
;

select x
from a
where x>=all (select y from a)
;

select x
from a
where x>(select max(y)
         from a
		 )
		 ;

select x
from a
where x>(select min(y)
         from a
		 )
		 ;

select x
from a
where x>min(select y
         from a
		 )
		 ;
select x
from a
where x>min((select y
         from a
		 ))
		 ;

select x
from a
where x>min(((select y
         from a
		 )))
		 ;
quit;
data a;
 input id $ x y;
 cards;
A 1 1000
B 1 1
C 1 1
;
run;

proc sql;
 select *
 from a
 where id ne 'A'
 group by x
 having avg(y)>1
 ;

 select *
 from a
 where id ne 'A'
 group by x
 having avg(y)=1
 ;

 data a;
  input x y;
  cards;
  2 4
  3 1
  4 6
  2 5
  3 2
  4 2
  ;
 run;

 *wybrac te obserwacje, w ktorych x jest wiekszy niz srednia swoich y;

 proc sql;

  select *
  from a
  where x> (select avg(y)
            from a
			where x=x
			)
		   ;

  select avg(y)
            from a
			where x=x;

  select *
  from a as pierwszy
  where pierwszy.x> (select avg(y)
            from a as drugi
			where drugi.x=pierwszy.x
			)
		   ;
		   *correlated subquery;

 select *
  from a as pierwszy
  where x> (select avg(y)
            from a as drugi
			where drugi.x=pierwszy.x
			)
		   ;

 select *
 from a
 group by x
 having x>avg(y)
 ;

 /* kolejnosc:

 create table
 select
 from
 where
 group by
 having
 order by

 */

 proc sql;
  describe table a
  ;

  data z;
   input x $ y;
   cards;
A 3
A 2
A 1
B 1
B 2
C 2
C 3
C 4
D 0
;
run;

data zz;
 set z nobs=ileile end=k;
 by x;
 ile+1;
 if k then 
  do;
   do i=1 to ileile;
    set z point=i;
	output;
   end;
  end;

run;

data zzz;
 set z ;
 by x;
 retain poczatek koniec;
 ile+1;
 if first.x then poczatek=_n_;
 if last.x then 
  do;
   do i=1 to ileile;
    set z point=i;
	output;
   end;
  end;

run;


proc sql;

*ile y wiekszych niz globalna srednia y;

select count(*)
from z
where y>(select avg(y) from z
        )
		;

		select avg(y) from z;
*ile roznych y wiekszych niz globalna srednia y;

		select count(distinct y)
from z
where y>(select avg(y) from z
        )
		;

select *,max(y)-min(y) as rozrzut
from z
group by x

;
select max(rozrzut)
from 
(
select *,max(y)-min(y) as rozrzut
from z
group by x
)
;


select *,max(y)-min(y) as rozrzut
from z
group by x
having rozrzut=max(rozrzut)
;

select *,max(y)-min(y) as rozrzut
from z
group by x
having rozrzut=(select max(rozrzut)
from 
(
select *,max(y)-min(y) as rozrzut
from z
group by x
)
)
;

*wybrac te y, ktore znajduja sie w co najmniej dwoch grupach wyznaczonych przez x;

select *
from z
group by y
having count(distinct x)>=2
;

