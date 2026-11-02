PROC IMPORT OUT= WORK.maxdata 
            DATAFILE= "C:\Users\mavolio2\OneDrive - Johns Hopkins\JHU\St
udents\Max\Data\allgrowthdatamarch25.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="allgrowthdatamarch25$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc sort data=Maxdata;
by Species day Treatment Location Line_ID ID;
proc mixed data=Maxdata;
where Species="VA";
class day Treatment Location Line_ID ID;
model cond2=day|Treatment|Location;
random Line_ID;
repeated day/subject=ID type=cs;
*lsmeans day*Treatment*Location/pdiff;
run;

proc sort data=Maxdata;
by Species day Treatment Location Line_ID ID;
proc mixed data=Maxdata;
where Species="LP";
class day Treatment Location Line_ID ID;
model cond2=day|Treatment|Location;
random Line_ID;
repeated day/subject=ID type=cs;
lsmeans day*Location/pdiff;
lsmeans day*Treatment/pdiff;
run;


PROC IMPORT OUT= WORK.survival 
            DATAFILE= "C:\Users\mavolio2\OneDrive - Johns Hopkins\JHU\St
udents\Max\Data\MaxSaltToleranceGreenhouseExperiment.xlsx" 
            DBMS=EXCEL REPLACE;
     RANGE="MaxSaltToleranceGreenhouseExper$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc sort data=Maxdata;
by Species Treatment Location Line_ID;
proc glimmix data=Maxdata;
where Species="LP";
class Treatment Location Line_ID;
model Survival=Treatment|Location/ dist=binomial link=logit solution;
random Line_ID;
*lsmeans day*Location/pdiff;
*lsmeans day*Treatment/pdiff;
run;

proc sort data=Maxdata;
by Species Treatment Location Line_ID;
proc glimmix data=Maxdata;
where Species="VA";
class Treatment Location Line_ID;
model Survival=Treatment|Location/ dist=binomial link=logit solution;
random Line_ID;
*lsmeans day*Location/pdiff;
*lsmeans day*Treatment/pdiff;
run;
