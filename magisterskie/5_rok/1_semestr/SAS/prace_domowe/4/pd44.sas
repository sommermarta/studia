* Imie i nazwisko:    Marta Sommer BSMAD

***************************** 4.1 ***********************************;

data zad1a;
	set lab4.ankieta;
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

data zad1b;
	retain a 0 b 0 c 0;
	set lab4.ankieta end=k;
	array pyt(*) _character_;
	do i=1 to dim(pyt);
		if(pyt(i)="A") then a=a+1;
		if(pyt(i)="B") then b=b+1;
		if(pyt(i)="C") then c=c+1;
	end;
	if k then output;
	keep a b c;
run;

data zad1c;
	set lab4.ankieta end=k;
	array pyt(*) _all_;
	array pytt(10,100) $ _temporary_;
	array pom(100) $;
	do i=1 to dim(pyt);
		pytt(i,_n_)=pyt(i);
	end;
	if k then 
	do;
		do i=1 to dim1(pytt);
			do j=1 to dim2(pytt);
				pom(j)=pytt(i,j);
			end;
			output;
		end;
	end;
	keep pom:;
run;

data zad1cc;
	set zad1c;
	array os(*) _character_;
	a=0; b=0; c=0;
	do i=1 to dim(os);
		if(os(i)="A") then a=a+1;
		if(os(i)="B") then b=b+1;
		if(os(i)="C") then c=c+1;
	end;
	keep a b c;
run;

***************************** 4.2 ***********************************;

data powt;
	set lab4.a;
	array xx(*) _all_;
	array temp(10) _temporary_;
	array ost(10);
	do i=1 to l;
		temp(i)=ceil(10*ranuni(0));
		ost(i)=xx(temp(i));
	end;
	keep ost:;
run;

data niepowt;
	set lab4.a;
	array xx(*) _all_;
	array temp(10) _temporary_;
	array czy(10) _temporary_;
	array ost(10);
	do i=1 to l;
		do until (sum=0);
			sum=0;
			y=ceil(10*ranuni(0));
			do j=1 to 10;
				if czy(j)=y then sum=sum+1;
			end;
			temp(i)=y;
			czy(i)=y;
		end; 
		ost(i)=xx(temp(i));
		sum=1;
	end;
	keep ost:;
run;

***************************** 4.3 ***********************************;

data zad3_latwiej;
	array p(10);
	array poprz(10) _temporary_;
	p(1)=1; output;
	poprz(1)=1;
	do i=2 to 10;
		do j=2 to i;
			p(j)=sum(poprz(j-1),poprz(j));
		end;
		do k=1 to 10;
			poprz(k)=p(k);
		end;
		output;
	end;
	keep p:;
run;

data zad3_trudniej;
	n=6;							   * tu trzeba zmienic;
	array p(13);                       * tu trzeba zmienic - dla danego n, p powinno miec wymiar (2n+1);
	array poprz(13) _temporary_;	   * tu trzeba zmienic;
	sr=(2*n+1+1)/2;
	p(sr)=1; output;
	poprz(sr)=p(sr); 
	do i=1 to n;
		do j=2 to (dim(p)-1);
			p(j)=sum(poprz(j-1),poprz(j+1));
		end;
		do k=1 to dim(p);
			poprz(k)=p(k);
		end;
		if i=n then do; p(1)=1; p(2*n+1)=1; end;
		output;
	end;
	keep p:;
run;

***************************** 4.4 ***********************************;

data z;
	array z(100);
	do i=1 to dim(z);
		z(i)=2*ranuni(0)-1;
	end;
	drop i;
run;

data pz;
	set z end=k;
	array zz(*) _all_;
	array tab(10,10) _temporary_;
	array pz(10);
	do i=0 to 9;
		do j=0 to 9;
			tab(i+1,j+1)=zz(10*i+j+1);
		end;
	end;
	if k then
	do;
		do i=1 to dim1(tab);
			do j=1 to dim2(tab);
				pz(j)=tab(i,j);
			end;
			output;
		end;
	end;
	keep pz:;
run;

data braki;
	set pz end=k;
	array ppz(*) _all_;
	array tab(10,10) _temporary_;
	k=0;
	if _n_=1 then do;
		do until (sum=0 and k=25);
			sum=0;
			i=ceil(10*ranuni(0));
			j=ceil(10*ranuni(0));
			if(tab(i,j)=1) then sum=sum+1;
			else do; tab(i,j)=1; k=k+1; end;
		end;
	end;
	do i=1 to 10;
		if tab(_n_,i) then ppz(i)=.;
	end;
	keep pz:;
run;

***************************** 4.5 ***********************************;

data zad5;
	set lab4.a end=kk;
	array x(*) _all_;
	array tab(11);
	do i=1 to dim(x);
		tab(i)=x(i);
	end;
	do until (k=0);
		k=0;
		do i=1 to 10;
			if tab(i)>tab(i+1) then
			do;
				z=tab(i+1);
				tab(i+1)=tab(i);
				tab(i)=z;
				k=1;
			end;
			else continue;
		end;
	end;
	keep tab:;
run;

***************************** 4.6 ***********************************;

data konkon;
	set lab4.konwersja;
	m=scan(data,1,'.');
	r=scan(data,2,'.');
	d=scan(data,3,'.');
	dataSAS=mdy(m,d,r);
	liczba0=liczba;
	do j=1 to length(kod);
		jakie=anyalpha(kod,j);
		if jakie=0 then continue; else 
			do;
				co=substr(kod,jakie,1);
				ile=substr(kod,jakie+2,1);
				do i=1 to length(liczba);
					gdzie=anyalpha(liczba,i);
					if gdzie=0 then continue; else
					do;
						coto=substr(liczba,gdzie,1);
						if coto=co then substr(liczba,gdzie,1)=ile; 
					end;
				end;
			end;
	end;
	liczba_num=liczba+0;
	keep liczba_num kod data dataSAS liczba0;
run;

***************************** 4.7 ***********************************;

data bin;
	set lab4.sysdwa end=k;
	array b(*) _all_;
	array bb(7,9) _temporary_;
	ilekr=0;
	do i=dim(b) to 1 by (-1);
		if b(i)=. then do; ilekr=ilekr+1; continue; end; else
			do;
				bb(_n_,i+ilekr)=b(i);
			end;
	end;
	if k then
		do;
			do i=1 to dim1(bb);
				do j=1 to dim2(bb);
					suma=sum(suma,bb(i,j)*(2**(9-j)));
				end;
			output;
			suma=0;
			end;
		end;
	keep suma;
run;

***************************** 4.8 ***********************************;

data avg2;
	set lab4.xa end=k;
	array a(*) _character_;
	array x(*) _numeric_;
	array aa(5,5) _temporary_;
	array kk(5,5) _temporary_;
	array sr(5);
	do i=1 to 5;
		do j=1 to 5;
			if (a(i)=('A'||catt(j))) then do; aa(j,i)=sum(aa(j,i),x(i)); kk(j,i)=sum(kk(j,i),1); end;
		end;
	end;
	if k then
	do;
		do i=1 to 5;
			do j=1 to 5;
				sr(j)=aa(i,j)/kk(i,j);
			end;
			output;
		end;
	end;
	keep sr:;
run;

***************************** 4.9 ***********************************;

data aa;
	set lab4.a;
	drop l;
run;

data rosn;
	set aa end=kk;
	array x(*) _all_;
	array tab(10);
	do i=1 to dim(x);
		tab(i)=x(i);
	end;
	do until (k=0);
		k=0;
		do i=1 to 9;
			if tab(i)<tab(i+1) then
			do;
				z=tab(i+1);
				tab(i+1)=tab(i);
				tab(i)=z;
				k=1;
			end;
			else continue;
		end;
	end;
	keep tab:;
run;

data sr;
	set rosn;
	array tab(*) _all_;
	sr=mean(tab1,tab2,tab3);
	keep sr;
run;

***************************** 4.10 ***********************************;

data aa;
	set lab4.a1;
	array rr(*) _all_;
	g=_n_;
	do i=1 to 4;
		if rr(i)=. then 
			do;
				gdzie="w"||catt(g)||"k"||catt(i);
				output;
			end;
	end;
	keep gdzie;
run;

