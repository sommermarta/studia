
-- zad.1

-- a)

create table student (
	id_studenta int not null,
	imie varchar(30) not null,
	nazwisko varchar(30) not null,
	plec bit null
	)

create table przedmiot (
	id_przedmiotu int not null,
	data_rozpoczecia date not null,
	nr_przedmiotu int not null,
	imie_odpowiedzialnego varchar(30) not null,
	nazwisko_odpowiedzialnego varchar(30) not null,
	czy_anglojezyczny bit not null,
	liczba_studentow int null
	)

create table przedmiot_student (
	id_studenta int not null,
	id_przedmiotu int not null,
	data_zapisu date null
	)

alter table student
add constraint pk_student 
primary key (id_studenta)

alter table przedmiot
add constraint pk_przedmiot 
primary key (id_przedmiotu)

alter table przedmiot_student
add constraint pk_przedmiot_student 
primary key (id_przedmiotu, id_studenta)

alter table przedmiot_student
add constraint fk_przedmiot 
foreign key (id_przedmiotu) references przedmiot (id_przedmiotu)

alter table przedmiot_student
add constraint fk_student 
foreign key (id_studenta) references student (id_studenta)

-- b)

insert into przedmiot
values (1,'2012-06-10',230,'Marcin','Hulajnoga',1,90), (2,'2013-05-18',670,'Maja','Rej',0,60)

-- spr:

select * from przedmiot  -- ok :D

-- c)

insert into przedmiot
values (4,'2012-06-10',230,'Marcin','Hulajnoga',1,0), (5,'2013-05-18',670,'Maja','Rej',0,0)

select * from przedmiot

-- tu sie zaczyna:

delete from przedmiot
where liczba_studentow=0

-- spr:

select * from przedmiot -- ok :D

-- d)

insert into przedmiot
values (4,'2012-06-10',230,'Marta','Nowak',1,0), (5,'2013-05-18',670,'Marta','Nowak',0,0)

select * from przedmiot

-- tu sie zaczyna:

update przedmiot
set imie_odpowiedzialnego='Marek', nazwisko_odpowiedzialnego='Kowalski'
where imie_odpowiedzialnego='Marta' and nazwisko_odpowiedzialnego='Nowak'

-- spr:

select * from przedmiot --ok :D

-- e)

select * from student

-- tu sie zaczyna:

alter table student
add liczba_przedmiotow int, liczba_anglojezycznych int

-- spr:

select * from student

-- zad.2

-- student:
----- id_studenta -> klastrowy, unikalny
----- imie, nazwisko -> nieklastrowy, nieunikalny

-- przedmiot:
----- id_przedmiotu -> klastrowy, unikalny
----- imie_odpowiedzialnego, nazwisko_odpowiedzialnego -> nieklastrowy, nieunikalny
----- data_rozpoczecia -> nieklastrowy, nieunikalny

-- przedmiot_student:
----- id_studenta, id_przedmiotu -> klastrowy, unikalny

create unique clustered index idstud on student (id_studenta)
create index data_rozp on przedmiot (data_rozpoczecia)

-- zad.3

-- a)

create procedure nowynrprzedmiotu @nazwisko varchar(30) as
begin

	update przedmiot
	set nr_przedmiotu=10*nr_przedmiotu+7
	where nazwisko_odpowiedzialnego=@nazwisko and len(nr_przedmiotu)=3

end

-- spr:

insert into przedmiot
values (3,'2012-06-10',2300,'Marta','Nowak',1,0), (6,'2013-05-18',670,'Marta','Nowak',0,0)

select * from przedmiot

execute nowynrprzedmiotu 'Nowak'

select * from przedmiot

execute nowynrprzedmiotu 'Kowalski'

select * from przedmiot

-- b)

alter procedure nowynrprzedmiotu @nazwisko varchar(30) as
begin
	
	begin transaction
		update przedmiot
		set nr_przedmiotu=10*nr_przedmiotu+7
		where nazwisko_odpowiedzialnego=@nazwisko and len(nr_przedmiotu)=3
	commit

end

select * from przedmiot

execute nowynrprzedmiotu 'Hulajnoga'

select * from przedmiot

-- zad.4

alter trigger nowaliczbaprzedmiotow on przedmiot_student after insert as
begin

	declare @idstud int
	declare @liczang bit

	set @idstud = (select id_studenta from inserted)
	set @liczang = (
		select p.czy_anglojezyczny 
		from inserted as i 
		join przedmiot as p on i.id_przedmiotu =p.id_przedmiotu
		)

	update student
	set liczba_przedmiotow = liczba_przedmiotow + 1, liczba_anglojezycznych = liczba_anglojezycznych + @liczang
	where id_studenta=@idstud 

end

--  dodatkowe

alter trigger nowaliczbaprzedmiotow on przedmiot_student after insert as
begin

	declare @idstud int
	declare @liczang bit
	declare @idprzedm int

	declare i cursor local for (select id_studenta, id_przedmiotu from inserted)
	open i

	fetch next i into @idstud, @idprzedm

	while @@fetch_status=0
	begin

	set @liczang = (
		select czy_anglojezyczny 
		from przedmiot
		where id_przedmiotu=@idprzedm
		)

	update student
	set liczba_przedmiotow = liczba_przedmiotow + 1, liczba_anglojezycznych = liczba_anglojezycznych + @liczang
	where id_studenta=@idstud 

	fetch next i into @idstud, @idprzedm
	end


	close i
	deallocate i

end