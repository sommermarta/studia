/* ****************************************************************************** */
/* SAS0910 - LAB   06  ********************************************************** */
/* ****************************************************************************** */
/* TRANSPOZYCJA                                           *********************** */
/* ****************************************************************************** */

* Prosty zbior wejsciowy;
data a;
 input x y;
 cards;
 1 4
 2 5
 3 .
run;
* Podstawowe uzycie procedury TRANSPOSE (w OnlineDocu - 
BaseSAS\SASProcedures\Procedures);
proc transpose data=a out=ta;
run;
* Nalezy zwrocic uwage na nazwy i etykiety utworzonych automatycznie zmiennych!;
proc transpose data=a out=ta;
 var x; *mozna specyfikowac zmienne;
run;
proc transpose data=a out=ta;
 var x y; *jak zwykle separatorem w 4GL jest spacja;
run;

* Prosty zbior ze zmiennymi tekstowymi;
data a;
 input x $ y $;
 cards;
 a b
 c d
 e f
run;
proc transpose data=a out=ta;
run;
* NOTE: No variables to transpose.;
* Zbior ze zmiennymi tekstowymi i numerycznymi;
data a;
 input x $ y;
 cards;
a 1
b 2
c 3
run; 
proc transpose data=a out=ta;
run;
proc transpose data=a out=ta;
 var x;
run;
* Moral - jesli chcemy transponowac zmienne tekstowe, trzeba je umieszczac w VAR;

* Nieco bardziej skomplikowany przyklad;
data a;
 input marka $ silnik moc;
 format silnik 3.1;
 cards;
 Mercedes 2.0 110
 BMW 3.0 200
 Audi 4.2 200
 ;
run;
proc transpose data=a out=ta;
run;
* Chcemy spowodowac, aby nazwy marek staly sie nazwami zmiennych;
proc transpose data=a out=ta;
 id marka;
run;
* W nastepnym stepie chcemy zmienic nazwe zmiennej _NAME_ - nie ma potrzeby robienia
tego osobnym DATA STEPem;
proc transpose data=a out=ta (rename=(_name_=zmienna));
 id marka;
run;
* Nazwa zmiennej sie zmienila, etykieta - nie;
* Jak radza sobie z ta sytuacja inne (niz ViewTable) przegladarki?;
proc print data=ta;
run;
proc fsview data=ta;
run;
* Etykietke mozna zmienic DATA STEPem (statement LABEL). Na jednym z pozniejszych 
wykladow zostanie pokazany bardziej elegancki sposob;

*To wszystko i tak przestaje dzialac w nastepujacym przypadku;
data a;
 input marka $ silnik moc;
 cards;
 Mercedes 2.0 110
 BMW 3.0 200
 Audi 4.2 200
 BMW 2.0 125
 ;
run;
proc transpose data=a out=ta;
run;
proc transpose data=a out=ta;
 id marka;
run;
*ERROR: The ID value "BMW" occurs twice in the input data set.;
* Co mozna zrobic w takiej sytuacji? Wprowadzamy zmienna dodatkowa zmienna, ktora 
jednoznacznie identyfikuje obiekt;
data a;
 input nr marka $ silnik moc;
 cards;
 1 Mercedes 2.0 110
 2 BMW 3.0 200
 3 Audi 4.2 200
 4 BMW 2.0 125
 ;
run;
proc transpose data=a out=ta;
 id nr;
run;
* Nazwy zmiennych w SASie nie moga zaczynac sie od cyfry, wiec kompilator wstawia
podkreslniki. Chcemy wyswietlac marki jako etykiety;
proc transpose data=a out=ta;
 id nr;
 idlabel marka;
run;


* Bardzo uzyteczna cecha procedury TRANSPOSE jest mozliwosc transponowania 
w grupach;
data a;
 input rok kwartal cena;
 cards;
 2001 1 10
 2001 2 11
 2001 3 12
 2001 4 13
 2002 1 90
 2002 2 89
 2002 3 87
 ;
run;
proc transpose data=a out=ta;
run;
proc transpose data=a out=ta;
 by rok;
run;
proc transpose data=a out=ta;
 by rok kwartal;
run;
proc transpose data=a out=ta;
 by rok; 
 id kwartal;
run;
proc transpose data=a out=ta;
 by kwartal; 
 id rok;
run;
* Zle, bo nieposortowane;
proc sort data=a out=as;
 by kwartal  rok;
run;
proc transpose data=as out=tas;
 by kwartal; 
 id rok;
run;
data asmod;
 set as;
 tekst='Cena w roku '||rok;
run;
/* Kulturalniej */
data asmod;
 set as;
 pom=put(rok,4.0 -l);
 tekst='Cena w roku '||pom;
run;
proc transpose data=asmod out=tasmod;
 by kwartal; 
 id rok;
 idlabel tekst;
run;
proc transpose data=asmod out=tasmod (drop=_name_);
 by kwartal; 
 id rok;
 idlabel tekst;
run;

* Do przeczytania: opcja PREFIX procedury TRANSPOSE;
