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









