libname dataset "C:\Users\dkm33\Documents";
proc contents data = dataset.labdata; run;

/*part 1*/
proc freq data = dataset.labdata;
table HAD1 HAD3 HAD4;
table HAD1*HAD3*HAD4
/list missing; run;

/*The largest sample size we can have is 1486 patients, that is, patiens previously told they have diabetes, but not when they were pregnant*/
/*part 2*/
/*create a temporary dataset*/
data one;
set dataset.labdata;
if HSSEX = 1 then do;
if had1 = 1 then dmstatus = 1;
else if had1 = 2 then dmstatus = 2; end;

if HSSEX = 2 then do;
if had1 = 2 then dmstatus = 2;
else if had1 = 1 and had3 = 1 and had4 = 2 then dmstatus = 3;
else if (had1 = 1 and had3 = 1 and had4 = 1) or (had1 = 1 and had3 = 2) then dmstatus = 1; end; run;

proc freq data = one;
tables dmstatus*hssex*had1*had3*had4
/list missing; run;

proc freq data = one;
table dmstatus; run;

/*69 participants have gestational diabetes, and these will be excluded. New Sample size is 1481 participants (male and female with diabetes)*/
/*part 3*/
data dataset.diabetes; /*make a permanent dataset*/
set one;
where dmstatus = 1;
/*create smoking status variable*/
if har1 = 2 then smokehx3 = 1;
else if har1 = 1 and har3 = 1 then smokehx3 = 3;
else if har1 = 1 and har3 = 2 then smokehx3 = 2;

/*create education variable*/
if hfa8r >= 0 and hfa8r <= 11 then educ3 = 1;
else if hfa8r = 12 then educ3 = 2;
else if hfa8r >= 13 and hfa8r <= 17 then educ3 = 3;

/*create marital status variable*/
if hfa12 = 1 or hfa12 = 2 or hfa12 = 3 then marital4 = 1;
else if hfa12 = 4 then marital4 = 2;
else if hfa12 = 5 or hfa12 = 6 then marital4 = 3;
else if hfa12 = 7 then marital4 = 4;run;

/*check for corresponding variables and any missing values*/
proc freq data = dataset.diabetes;
tables smokehx3*har1*har3
educ3*hfa8r
marital4*hfa12
/list missing; run;

/*Create a new dataset*/
data lab4;
set dataset.diabetes;

/*create retinopathy variable*/
if had15 = 1 then retinop = 1;
else if had15 = 2 then retinop = 0;

/*create insulin variable*/
if had6 = 1 then insulin = 1;
else if had6 = 2 then insulin = 0;

/*Oral medication variable*/
if had10 = 1 then oralmed = 1;
else if had10 = 2 then oralmed = 0;

/*Create female variable*/
if hssex = 2 then female = 1;
else if hssex = 1 then female = 0;

/*create disease variables for athritis, stroke etc.8*/
array disease_code[5] HAC1A HAC1D HAC1E HAC1O HAF10;
array disease[5] ARTHRIT STROKE ASTHMA CANCER MI;

do i = 1 to 5;
if disease_code[i] = 1 then disease[i] = 1;
else if disease_code[i] = 2 then disease[i] = 0; end;
drop i;

/*create cc_sum variable*/
cc_sum = sum(ARTHRIT, STROKE, ASTHMA, CANCER, MI);
cc_plus = ARTHRIT+STROKE+ASTHMA+CANCER+MI;

/*create cc_sum3*/
if cc_sum = 0 then cc_sum3 = 0;
else if cc_sum = 1 then cc_sum3 = 1;
else if 2 <= cc_sum <= 5 then cc_sum3 = 2; run;

proc freq data = lab4;
table retinop*had15
insulin*had6
oralmed*had10
female*hssex
HAC1A*ARTHRIT
HAC1D*STROKE
HAC1E*ASTHMA
HAC1O*CANCER
HAF10*MI
cc_sum*ARTHRIT*STROKE*ASTHMA*CANCER*MI
/list missing; run;

/*Create subset table*/
data dataset.diab2;
set lab4;
where dmarethn ne . and retinop ne .;
drop hssex hac1a hac1d hac1e hac1o haf10 har1 har3 had1 had3 had4 had6 had10 had15; run;

/*Means Calculations plus ANOVA*/
/*calulate means*/
proc means data = dataset.diab2;
class dmarethn;
var hsageir;run;
/*perform analysis of variance*/
proc anova data = dataset.diab2;
class dmarethn;
model hsageir = dmarethn;run;

/*Chi Square Test*/
proc freq data = dataset.diab2;
table (female educ3 marital4 retinop insulin oralmed smokehx3 cc_sum3)*dmarethn
/chisq; run;












