******************** 3.1 *************************************;

data lab3.gr2;
	do i=1 to 26;
		lit = byte(64+i);
		ile = floor(5*ranuni(0));
		if (ile=0) then continue;
			else do j=1 to ile;
					y = floor(20*ranuni(0));
					output;
				end; 
	end;
	keep lit y;
run;

data lab3.sortby;
	retain suma 0 ile 0;
	set lab3.gr2;
	by lit;
	suma = suma+y;
	ile = ile +1;
	if (last.lit = 1) then do;
							sr = suma/ile;
							suma = 0;
							ile = 0;
							output;
						 end;
	keep lit sr;
run;

data lab3.sort;
	retain suma 0 ile 1;
	set lab3.gr2 end=koniec;
	llit = lag(lit);
	ly = lag(y);
	if (lit ^= llit) then do;
							sr = suma/ile;
							suma=0;
							ile=0;	
							if not (_n_=1) then output;
						end;
	suma = suma+y;
	ile = ile+1;
	if (koniec) then do;
						sr=suma/ile;
						llit=lit;
						output;
					end;
	keep llit sr;
run;

******************** 3.2 *************************************;

data lab3.jed;
	do i=1 to 100;
		x = floor(101*ranuni(0));
		output;
	end;
	keep x;
run;

data lab3.jedwar;
	set lab3.jed;
	if (x>=0 and x<=20) then do war=1; end;
	if (x>=21 and x<=40) then do war=2; end;
	if (x>=41 and x<=60) then do war=3; end;
	if (x>=61 and x<=80) then do war=4; end;
	if (x>=81 and x<=100) then do war=5; end;
	output;
run;

proc sort data=lab3.jedwar out=lab3.jedwarsort;
	by war;
run;

data lab3.ile;
	retain maks;
	set lab3.jedwarsort;
	by war;
	if (_n_=1) then maks=x;
	if (maks<x) then maks=x;
	if (last.war=1) then output;
	keep maks war;
run;

******************** 3.3 *************************************;

data lab3.bb;
	retain ile 0 war 1;
	set lab3.b;
	by x;
	ile = ile + last.x;
	output;
	if (ile=10) then do ile = 0; war = war + 1; end;
	keep x war;
run;

data lab3.bbb;
	retain ile 0 maks 1;
	set lab3.bb;
	by war x;
	ile=ile+1;
	if (maks<ile) then maks=ile;
	if (last.war=1 or last.x=1) then do; output; ile=0; end;
	if (last.war=1) then maks=1;
run;

data lab3.bbbb;
	retain g;
	set lab3.bbb;
	by war;
	if (first.war=1) then g=x;
	if (ile=maks) then g=max(g,x);
	if(last.war=1) then output;
	keep war g;
run;

******************** 3.4 *************************************;

proc sort data=lab3.a out=lab3.aa nodup;
	by u;
run;

data _null_;
	retain ile 0;
	set lab3.aa;
	by u;
	ile=ile+1;
	if (last.u=1 and ile^=1) then put u;
	if (last.u=1) then do; output; ile=0; end;
run;

******************** 3.5 *************************************;

proc sort data=lab3.a out=lab3.aa nodup;
	by u;
run;

data lab3.aaa;
	retain ile 0;
	set lab3.aa;
	by u;
	ile=ile+1;
	if (last.u=1) then do; output; ile=0; end;
run;

proc sort data=lab3.aaa out=lab3.aaaa;
	by descending ile;
run;

data _null_;
	retain i 0;
	set lab3.aaaa;
	by descending ile;
	if (i=0) then put u;
	if (last.ile=1) then i=i+1; 
run;

******************** 3.6 *************************************;

data lab3.aa;
	retain suma 0 ile 0;
	set lab3.a;
	by x;
	suma=suma+u;
	ile=ile+1;
	if(ile=5 or last.x=1) then do;
								sr=suma/ile;
								suma=0;
								ile=0;
								output;	
							end;
run;

data lab3.aaa;
	set lab3.aa;
	by x;
	if(first.x=1) then output;
	keep x sr;
run;

******************** 3.7 *************************************;

data lab3.oo;
	retain ile 0;
	set lab3.a;
	by x;
	ile=ile+1;
	output;
	if(last.x=1) then ile=0;
run;

proc sort data=lab3.oo out=lab3.aa;
	by descending x descending ile;
run;

data lab3.aaa;
	retain suma 0 i 0;
	set lab3.aa;
	by descending x;
	suma=suma+u;
	i=i+1;
	if(i=5 or last.x=1) then do;
								sr=suma/i;
								suma=0;
								i=0;
								output;	
							end;
run;

data lab3.aaaa;
	set lab3.aaa;
	by descending x;
	if(first.x=1) then output;
	keep x sr;
run;

******************** 3.8 *************************************;

data _null_;
	retain zero 0 dziewiec 0;
	set lab3.a;
	by x;
	if(u=0) then zero=zero+1;
	if(u=9) then dziewiec=dziewiec+1;
	if((last.x=1 and zero<1 and dziewiec>=1) or (last.x=1 and zero>=1 and dziewiec<1) or (last.x=1 and zero<1 and dziewiec<1)) then do;
													zero=0;
													dziewiec=0;
													put x;
												end;
	
	if(last.x=1) then do; zero=0; dziewiec=0; end;
run;

******************** 3.9 *************************************;

proc sort data=lab3.funkcja out=lab3.f2 nodup;
	by x;
run;

data lab3.f3;
	set lab3.f2 end=k;
	l=lag(x);
	if(x=l) then suma+1;
	if k then if suma>0 then put "Nie, to nie jest funkcja."; else put "Tak, to jest funkcja."; 
run;

******************** 3.10 *************************************;

proc sort data=lab3.wybory out=lab3.w;
	by partia;
run;

data lab3.ww;
	retain suma 0 ile 0;
	set lab3.w;
	by partia;
	suma=suma+p;
	ile=ile+1;
	if last.partia then do; sr=suma/ile; suma=0; ile=0; output; end;
	keep partia sr;
run;

*******************************************;

data lab3.wyb2;
	retain suma 0;
	set lab3.wybory;
	by id;
	suma=suma+p;
	if last.id then do; output; suma=0; end;
run;

data _null_;
	retain ile 0;
	set lab3.wyb2 end=k;
	if(suma^=100) then ile=ile+1;
	if k then put ile;
run;

*******************************************;

data lab3.w;
	retain ile 0;
	set lab3.wybory;
	lpar=lag(partia);
	lp=lag(p);
	llpar=lag2(partia);
	llp=lag2(p);
	ile=ile+1;
	if ile=3 then do; ab=lp+llp; ac=p+llp; bc=lp+p; ile=0; output; end;
	keep id ac ab bc;
run;

data lab3.ww;
	set lab3.w;
	maks=max(ab, max(ac,bc));
	if maks=ab and (ab^=ac and ab^=bc) then kolal="ab"; else do; 
	if maks=ac and (ac^=ab and ac^=bc)then kolal="ac"; else do;
	if maks=bc and (bc^=ac and bc^=ab)then kolal="bc"; else do; kolal="0"; end; end; end; 
run;

proc sort data=lab3.ww out=lab3.www;
	by kolal;
run;

data lab3.wwww;
	retain suma 0;
	set lab3.www;
	by kolal;
	suma=suma+1;
	if last.kolal then do; output; suma=0; end;
	drop id;
run;

data lab3.wwwww;
	set lab3.wwww;
	if(_n_^=1) then output;
	keep suma kolal;
run;
