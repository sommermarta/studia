* 4.1 - sami wygenerujemy ankiete;

data lab04.ankieta2;
	array pyt(10) $;
		format pyt1-pyt10 $1.; * ze znaki moga byc dlugosci trzy;
			do i=1 to 10;
				do j=1 to 10;
					x=3*ranuni(0);
					pyt(j)=byte(65+floor(x));
				end;
				output;
			end;
	drop i j x;
run;

data lab04.zad1a;
	set lab04.ankieta;
	array pyt(*) _character_;
	a=0; b=0; c=0;
	do i=1 to dim(pyt);
		if(pyt(i)="A") then a=a+1;
		if(pyt(i)="B") then b=b+1;
		if(pyt(i)="C") then c=c+1;
	end;
	output;
	keep a b c;
run;

data lab04.zad1b;
	retain a 0 b 0 c 0;
	set lab04.ankieta end=k;
	array pyt(*) _character_;
	do i=1 to dim(pyt);
		if(pyt(i)="A") then a=a+1;
		if(pyt(i)="B") then b=b+1;
		if(pyt(i)="C") then c=c+1;
	end;
	if k then output;
	keep a b c;
run;


data lab04.zad1c;
	set lab04.ankieta end=k;
	array pyt(*) _character_;
	array pytt(10,10) _character_;
	do i=1 to dim(pyt);
		if(pyt(i)="A") then pytt(i)=pytt(i)+1;
		if(pyt(i)="B") then b=b+1;
		if(pyt(i)="C") then c=c+1;
	end;
run;
