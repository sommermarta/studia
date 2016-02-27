var x_1;
var x_2;
var x_3;
var x_4;
var x_5;
var y;

maximize fcelu: -110*x_1 - 120*x_2 - 130*x_3 - 110*x_4 - 115*x_5 + 150*y; 

subto ogr1: x_1 + x_2 <= 200;
subto ogr2: x_3 + x_4 + x_5 <= 250;
subto ogr3: 8.8*x_1 + 6.1*x_2 + 2*x_3 + 4.2*x_4 + 5*x_5 - 6*y <= 0;
subto ogr4: 8.8*x_1 + 6.1*x_2 + 2*x_3 + 4.2*x_4 + 5*x_5 - 3*y >= 0;
subto ogr5: x_1 + x_2 +x_3 + x_4 + x_5 -y == 0;


