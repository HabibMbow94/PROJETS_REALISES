LIBNAME prob "C:\Users\mbowe\Desktop\M2_MIASHS\seminaire";

proc import datafile="C:\Users\mbowe\Desktop\M2_MIASHS\seminaire\african_crises.csv"
	out=prob.cluster
	dbms=csv
	replace;
	getnames=yes;
run;


PROC CONTENTS DATA=prob.cluster;
RUN;

/**************************************************************/
/*           STATISTIQUES DESCRIPTIVES 					*/

/**************************************************************/

      /*           ANALYSES UNIVARIEES             */
     
ODS RTF FILE= "C:\Users\mbowe\Desktop\M2_MIASHS\seminaire\sortie.doc";
PROC MEANS DATA=prob.cluster N MIN Q1 Mean STD Q3 MAX;
VAR exch_usd inflation_annual_cpi gdp_weighted_default ;
RUN;

PROC FREQ DATA=prob.cluster;
TABLES cc3 Country year systemic_crisis domestic_debt_in_default sovereign_external_debt_default 
 independence currency_crises inflation_crises banking_crisis;
RUN;
ODS RTF CLOSE ;

 /*---------------------------------------------- */
      /*                ANALYSES  BIVARIES            */
      /*--------------------------------------------*/

PROC MEANS DATA=work.seminaire;
VAR exch_usd inflation_annual_cpi;
CLASS Banking_crisis;
RUN;

PROC CORR DATA=prob.seminaire pearson plots(maxpoints = 1000000) =matrix(histogram);
VAR  exch_usd inflation_annual_cpi gdp_weighted_default;
RUN;

PROC FREQ DATA=prob.seminaire;
TABLES (Country year systemic_crisis domestic_debt_in_default sovereign_external_debt_default gdp_weighted_default
independence currency_crises inflation_crises)*banking_crisis /CHISQ NOCOL NOPERCENT;
RUN;

ODS RTF FILE= "C:\Users\mbowe\Desktop\M2_MIASHS\seminaire\test.doc";
proc corr DATA=prob.cluster pearson plots(maxpoints = 1000000) =matrix(histogram) ;
var exch_usd inflation_annual_cpi gdp_weighted_default; 
RUN;

proc ttest DATA=prob.cluster  ;
class banking_crisis;
var exch_usd inflation_annual_cpi gdp_weighted_default; 
RUN;

/* Test du Rang (Wilcoxon), Kolmogorov-Smirnov*/
proc npar1way data =prob.cluster  ;
class banking_crisis;
var exch_usd inflation_annual_cpi gdp_weighted_default; 
run;
ODS RTF CLOSE ;

PROC BOXPLOT data =prob.cluster;
PLOT (exch_usd inflation_annual_cpi gdp_weighted_default)*banking_crisis;
RUN;

PROC FREQ DATA=prob.cluster;
TABLES Country*year*systemic_crisis*domestic_debt_in_default*sovereign_external_debt_default*gdp_weighted_default*independence*currency_crises*inflation_crises
;
RUN;

PROC LOGISTIC DATA=prob.cluster plots=roc;;
CLASS Systemic_crisis country domestic_debt_in_default sovereign_external_debt_default gdp_weighted_default
independence currency_crises inflation_crises/ param=ref ;
MODEL Banking_crisis(event='crisis')= Systemic_crisis country domestic_debt_in_default sovereign_external_debt_default gdp_weighted_default
independence currency_crises inflation_crises exch_usd inflation_annual_cpi gdp_weighted_default
 year / SELECTION=Stepwise
  LACKFIT;
  ODS OUTPUT ParameterEstimates=WORK.EstiLogitBack;
  OUTPUT OUT =WORK.LogitBack predprobs=I ;
RUN;
 



ODS RTF FILE= "C:\Users\mbowe\Desktop\M2_MIASHS\seminaire\logiquee.doc";
ods graphics on;
ODS GRAPHICS ON;
PROC LOGISTIC DATA=prob.seminaire plots=roc;;
CLASS sovereign_external_debt_default  
independence currency_crises inflation_crises / param=ref ;
MODEL Banking_crisis(event='crisis')=  sovereign_external_debt_default 
independence currency_crises inflation_crises 
  /  selection=stepwise
  LACKFIT;
  ODS OUTPUT ParameterEstimates=WORK.EstiLogitBack;
  OUTPUT OUT =WORK.LogitBack predprobs=I ;
RUN;

ods graphics off;
ODS RTF CLOSE ;

