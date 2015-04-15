* niedziela do 22:00 - prace domowe;
* zad.1;
* zad.3 - na cwiczenie funkcji lag i diff, lag2 i diff2;

******* zad1 *********;

data zad1;
	do i=1 to 10;
	x=2*ranuni(0)-1;
	y=2*ranuni(0)-1;
	output;
	end;
	keep x y z;   * zachowuje tylko zmienna x i y, a nie zliczajaca;
run;

data avg1;
	set zad1;
	max=max(x,y);
	sr=(x+y)/2;
run;

data avg2;
	retain sumax 0 sumay 0 i 0;
	set zad1 end=koniec;
	sumax=sumax+x;
	sumay=sumay+y;
	i=i+1;
	if koniec then do;  * jesli chce wiecej niz jedna komende po then;
		srx=sumax/i;
		sry=sumay/i;
		put 'srednia x:' srx 'srednia y:' sry; 
		end;
	drop srx sry i sumax sumay;   * wywala zmienne; 
run;

data _null_;
	retain maxx  maxy ;
	set zad1 end=koniec;
	if _n_=1 then do;
		maxx=x;
		maxy=y;
		end;
	maxx=max(x,maxx);
	maxy=max(y,maxy);
	if koniec then put 'max x:' maxx 'max y:' maxy;
	drop maxx maxy;
run;


*************** zad5 *******************************;

data a;
	do x=1 to 20;
		output;
	end;
run;

data b;
	set a;
	y=lag2(x);
	z=dif2(x);
run;

data _null_;
	put v1= v2=;
	set lab02.zbior;
	put v1= v2=;
run;

data _null_;
	put v1= v2= v3=;
	set lab02.zbior;
	v3=v1+v2;
	put v1= v2= v3=;
run;

data _null_;   * zad5 - czesc pierwsza;
	retain v3;
	v3=v1+v2;
	put v1= v2= v3=;	
	set lab02.zbior;	
	put v1= v2= v3=;
run;

data _null_;   * zad5 - czesc druga;
	retain v3;
	put v1= v2= v3= v4=;
	set lab02.zbior;	
	v3=v1+v2;
	v4=v1+v2;
	put v1= v2= v3= v4=;
run;

data _null_;   * zad5 - czesc trzecia nie dziala na razie;
	retain v3;
	retain v4;
	v4=v1+v2;
	put v1= v2= v3= v4=;
	set lab02.zbior;	
	v3=v1+v2;
	put v1= v2= v3= v4=;
run;

**************  zad10 ***************************;

data zad10;
	retain sumax 0 sumay 0;
	do n=1 to 200;
		x=ranuni(0);
		y=ranuni(0);
		if x < 0.5 then k1=-1;
		else k1=1;
		if y < 0.5 then k2=-1;
		else k2=1;
		sumax=sumax+k1;
		sumay=sumay+k2;
		output;
	end;	
run;
