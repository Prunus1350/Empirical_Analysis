* 実証分析入門第12章 目的変数が質的変数の場合の分析手法：飛ばねぇ豚はただの豚だ;

%let indir = /folders/myfolders/Empirical_Analysis/csv/;

data porco;
    infile "&indir.PorcoRosso.csv" dsd missover firstobs = 2;
    attrib pig_id      length = 8 label = "豚ID"
           flying      length = 8 label = ""
           flyhour     length = 8 label = ""
           kids6       length = 8 label = "6歳未満の子供"
           kids18      length = 8 label = "6-18歳の子供"
           age         length = 8 label = "年齢"
           educ        length = 8 label = "教育"
           flywage     length = 8 label = ""
           spouseage   length = 8 label = "配偶者の年齢"
           spouseeduc  length = 8 label = "配偶者の教育"
           spousewage  length = 8 label = ""
           faminc      length = 8 label = "配偶者の収入"
           taxrate     length = 8 label = ""
           momeduc     length = 8 label = ""
           dadeduc     length = 8 label = ""
           exp         length = 8 label = "飛行経験"
           otherfaminc length = 8 label = ""
           logwage     length = 8 label = ""
           expsq       length = 8 label = ""
    ;
    input pig_id
          flying
          flyhour
          kids6
          kids18
          age
          educ
          flywage
          spouseage
          spouseeduc
          spousewage
          faminc
          taxrate
          momeduc
          dadeduc
          exp
          otherfaminc
          logwage
          expsq
    ;
run;

proc print data = porco(obs=10);
run;


* P129 図12-1 : 無理やりOLE推定 ;
proc reg data = porco;
    model flying = exp;
run;


* P130 図12-2 : 飛行経験のヒストグラム ;
proc sgpanel data = porco;
    panelby flying;
    histogram exp;
run;


* P138 表12-3 豚はなぜ飛ぶのか？ : LPM ;
proc reg data = porco;
    model flying = age educ spouseeduc otherfaminc exp kids6 kids18;
run;

* Probit ;
proc probit data = porco;
    model flying(event="1") = age educ spouseeduc otherfaminc exp kids6 kids18;
run;

* Logit ;
proc logistic data = porco;
    model flying(event="1") = age educ spouseeduc otherfaminc exp kids6 kids18;
run;




