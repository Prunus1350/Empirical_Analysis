* 実証分析入門第13章 最尤法 (MLE)：OLSとは違うのだよ、OLSとは！;

%let indir = /folders/myfolders/Empirical_Analysis/csv/;


data temp;
    do x = -6 to 6 by 0.1;
        y = pdf("normal", x, -2, 1);
        output;
    end;
run;

data temp2;
    x =  1.364; z = pdf("normal", x, -2, 1); output;
    x =  0.235; z = pdf("normal", x, -2, 1); output;
    x = -0.846; z = pdf("normal", x, -2, 1); output;
    x = -0.285; z = pdf("normal", x, -2, 1); output;
    x = -1.646; z = pdf("normal", x, -2, 1); output;
run;

proc sort data = temp2;
    by x;
run;

data temp3;
    merge temp (in=in1)
          temp2(in=in2);
    by x;
run;

* P141 図13-1 ;
proc sgplot data = temp3;
    series x = x y = y;
    needle x = x y = z;
run;

