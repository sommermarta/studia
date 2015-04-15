********************** 2.1 *******************************;

* generuje zbior;
data a;
	do i=1 to 20; 
		x = 2*ranuni(0)-1; 
		y = 2*ranuni(0)-1;
		output;
	end;
	drop i;
run;

* srednie i maksymalne wartosci zmiennych x i y w kolejnych obserwacjach;
data b;
	set a;
	maks=max(x,y);
	sr=mean(x,y);
run;

* srednie i maksymalne wartosci zmiennych x i y (w kolumnach);
data c;
	retain sumax 0 sumay 0 maks_x maks_y i 0;
	set a end=koniec;
	if _n_=1 then do;
		maks_x=x;
		maks_y=y;
		end;
	sumax=sumax+x;
	sumay=sumay+y;
	maks_x=max(x,maks_x);
	maks_y=max(y,maks_y);
	i=i+1;
	if koniec then do;
		sr_x=sumax/i; 
		sr_y=sumay/i; 
		output;
		end;
	keep sr_x sr_y maks_x maks_y;
run;

********************** 2.2 *******************************;

data b;
	set lab2.a;
	do i=1 to x;
		output;
	end;
	drop i;
run;

********************** 2.3 *******************************;

data rank2;
	retain nr;
	set lab2.rank;
	l=lag(x);
	if _n_=1 then nr=1;
	if x~=l & _n_~=1 then nr=nr+1;	
	drop l;
run;

********************** 2.4 *******************************;

data cb2;
	retain wz 0 spad 0 wz2 spad2;
	set lab2.cb end=koniec;
	if _n_=1 then do;
		wz2=cb;
		spad2=cb;
		end;
	if cb>wz2 then do;
		wz=wz+1;
		end;
	if cb<spad2 then do;
		spad=spad+1;
		end;
	wz2=cb;
	spad2=cb;
	if koniec then output;
	keep wz spad;
	put _all_;
run;

********************** 2.5 *******************************;

* ad. k1;
data _null_;
	retain v3;
	put 'v1= ' v1 'v2= ' v2 'v3= ' v3;
	set lab2.zbior;
	put 'v1= ' v1 'v2= ' v2 'v3= ' v3;
	v3=v1+v2;
run;

* ad. k2;
data _null_;
	retain v3;
	put 'v1= ' v1 'v2= ' v2 'v3= ' v3 'v4= ' v4;
	set lab2.zbior;
	v3=v1+v2;
	v4=v1+v2;
	put 'v1= ' v1 'v2= ' v2 'v3= ' v3 'v4= ' v4;
run;

* ad. k3;
data _null_;
	retain v4;
	put 'v1= ' v1 'v2= ' v2 'v3= ' v3 'v4= ' v4;
	v3=v1+v2;
	put 'v1= ' v1 'v2= ' v2 'v3= ' v3 'v4= ' v4;
	set lab2.zbior;
	v4=v1+v2;
run;

********************** 2.6 *******************************;

* przyjelam w tym zadaniu konwencje, ze brzegi nie sa maksimami lokalnymi;
data cb2;
	retain suma;
	set lab2.cb end=koniec;
	l=lag(cb);
	ll=lag2(cb);
	if _n_<=2 then do;
		suma=0;
		end;
	else do;
		if l>=ll & l>=cb then do;
			suma=suma+1;
			end;
		end;
	if koniec then output;
	keep suma;
run;

********************** 2.7 *******************************;

data dif;
	set lab2.brak end=koniec;
	d=dif2(x);
	if _n_>=2 then output;
	if koniec then do; d=.; output; end;
	keep d;
run;

data lag;
	set lab2.brak;
	l=lag(x); 
	keep l;
run;

data brak2;
	set lab2.brak; 
	set dif;
	set lag; 
	if x=. then x=(l+d+l)/2;
	keep x;
run;


********************** 2.8 *******************************;

data bezkropek2;
	set lab2.bezkropek;
	sumy=0; 
	sumx=0;
	xx=x;
	yy=y;
	do until (sumy=1 & sumx=1);
	if sumx=0 & sumy=0 then do;
		a=floor(4*ranuni(0));
		if a=0 then do;
				x=xx; 
				y=yy; 
				sumy=1;
				sumx=1;
				output;
				end;
		if a=1 then do;
				x=.;
				y=yy;
				sumy=1;
				output;
				end;
		if a=2 then do;
				y=.;
				x=xx;
				sumx=1;
				output;
				end;
		if a=3 then do;
				x=.;
				y=.;
				output;
				end;
		end;
	if sumx=1 & sumy=0 then do;
	a=floor(2*ranuni(0));
		if a=0 then do;
				x=.;
				y=yy;
				sumy=1;
				output;
				end;
		if a=1 then do;
				x=.;
				y=.;
				output;
				end;
		end;
	if sumx=0 & sumy=1 then do;
	a=floor(2*ranuni(0));
		if a=0 then do;
				y=.;
				x=xx;
				sumx=1;
				output;
				end;
		if a=1 then do;
				x=.;
				y=.;
				output;
				end;
		end;
	end;
	keep x y;
run;


********************** 2.9 *******************************;

data dyst;
	i=0;
	suma=0;
	do while (suma<1);
		los=0.5*ranuni(0);
		suma=suma+los;
		i=i+1;
		if suma>1 then suma=1;
		output;
	end;
	drop los;
run;

data rozklad;
	set dyst;
	pstwo=dif(suma);
	if _n_=1 then pstwo=suma;
run;

********************** 2.10 *******************************;

data traj;
	do i=1 to 20;
		a=ranuni(0);
		b=ranuni(0);
		if a>=0.5 then k1=1; else k1=-1;
		if b>=0.5 then k2=1; else k2=-1;
		output;
	end;
run;

data traj2;
	retain suma1 0 suma2 0 maks1 0 maks2 0 wierzch 1 przec 0 pam 0;
	pam = k1+k2;
	z=k1;
	zz=k2;
	set traj end=k;
	pam2=k1+k2;
	if suma1=suma2 & pam=0 & pam2=0 & z=k1 & zz=k2 then przec=przec+1;
	suma1=suma1+k1;
	suma2=suma2+k2;
	maks1=max(maks1, abs(suma1));
	maks2=max(maks2, abs(suma2));
	if(suma1=suma2) then wierzch=wierzch+1;
	if k then output;
	keep maks1 maks2 wierzch przec;
run;

