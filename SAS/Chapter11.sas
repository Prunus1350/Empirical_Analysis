* 実証分析入門第11章 不均一分散への対処：こんなこともあろうかと;

%let indir = /folders/myfolders/Empirical_Analysis/csv/;

data crimes;
    infile "&indir.criminal.csv" dsd missover firstobs = 2;
    attrib id    length = 8 label = "ID"
           crime length = 8 label = "犯罪件数"
           unemp length = 8 label = "失業者数"
           pop   length = 8 label = "人口"
    ;
    input id crime unemp pop;
run;

proc print data = crimes(obs=10);
run;

* Breusch-Pagan test : SAS/ETSが必要？ ;
proc model data = crimes;
    parms b0 b1;
    crime = b0 + b1 * unemp;
    fit crime / breusch = (1 unemp);
run;

* OLS ;
proc reg data = crimes;
    model crime = unemp;
run;

* OLS + robust SE ;
proc surveyreg data = crimes;
    model crime = unemp;
run;

* WLS ;
proc reg data = crimes;
    weight pop;
    model crime = unemp;
run;

* FGLS : SAS/ETSが必要？ ;
proc tscsreg data = crimes;
    model crime = unemp;
run;

