data wyniki;    * zad1
input kod $ kol1 kol2 ocena;
cards;
AD11423 19 23 3.5
AG19020 16 21 3
AW93048 35 12 4
RG04729 4 15 2
DR03827 8 11 2
;
run;

libname lab1 'h:\Windows7\Desktop\SAS';   * tworzenie biblioteki kodem, a nie klikaniem;

* tworzenie kolumny kodem, a nie klikaniem:   /*    */;

data wyniki2;
	set wyniki;    * wczytujemy zbior wyniki, set - petla idaca po wierszach zbioru;
	suma=kol1+kol2;
run;

data a;
	x=today();
	y='01 Apr 2000d';   * to jest tekst, zeby przeczytal jako date jest to d
run;

* datastep, ktory nie tworzy zadnego zbioru:;

data _null_;
	x=5;
run;

data _null_;
	x='10 Jun 1991'd;
	y=today();
	z=y-x;
	put 'zyjesz ' z 'dni';   * wyswietli sie :D;
run;

data lab01.a;   * zapisuje wtedy w tej bibliotece, a nie w worku
	do i=1 to 10;
		output; * wtedy run juz nie dziala, jak pojawia sie output;
	end;
run;

data b;
	suma=0;
	set a;
	suma=suma+i;    * nie dziala ;( ;
run;

data b;
	if _n_=1 then suma=0; * jesli jestes w pierwszym obrocie petli glownej;
	set a;
	suma=suma+i; 
run;

data b;
	retain suma 0;
	set a;
	suma=suma+i; 
run;

data _null_;
	retain suma 0;
	set a end=k;   * koniec zbioru to k - zero we wszystkich obserwacjach, a w ostatniej jeden (nie bedzie wyswietlana w zbiorze wynikowym);
	suma=suma+i; 
	if k=1 then put suma;
run;

* inaczej to samo:;

data _null_;
	set a end=k;
	suma+i; *(pamieta, ze ma retaina na sumie, i ze zaczyna od zera);
	if k=1 then put suma;
run;

data _null_;
	retain suma 1;
	set a end=k;
	suma=suma*i;
	if k=1 then put suma;
run;


proc means data=wyniki;
	var kol1 kol2;
run;
