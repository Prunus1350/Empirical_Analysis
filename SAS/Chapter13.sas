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


data bankruptcy_sim;
    infile "&indir.bankruptcy_sim.csv" dsd missover firstobs = 2;
    attrib bankruptcy length = 8 label = ""
           pop        length = 8 label = ""
           raw        length = 8 label = ""
    ;
    input bankruptcy pop raw;
run;

proc sgplot data = bankruptcy_sim;
    vbar bankruptcy / stat = freq;
run;

proc sgplot data = bankruptcy_sim;
    histogram bankruptcy / binwidth = 1;
run;

proc sgplot data = bankruptcy_sim;
    scatter x = bankruptcy y = pop / group = raw;
run;

proc sgscatter data = bankruptcy_sim;
    matrix bankruptcy pop raw;
run;

proc reg data = bankruptcy_sim outest = reg_result;
    model bankruptcy = pop raw / aic;
run;
/*
proc reg data = bankruptcy_sim plots = diagnostics(stats=(aic));
    model bankruptcy = pop raw;
run;
*/

proc genmod data = bankruptcy_sim;
    model bankruptcy = pop raw / dist = poisson
                                 link = log
    ;
run;

proc genmod data = bankruptcy_sim;
    model bankruptcy = pop raw / dist = negbin
    ;
run;
