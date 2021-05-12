
LIBNAME projet "C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance";
proc import datafile="C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance\base.xlsx" 
out=Projet.base dbms=xlsx;
run;
PROC EXPORT DATA=Projet.Base1;
OUTFILE = "C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance\base.xlsx"
       DBMS = xlsx REPLACE    ;
RUN;



PROC CONTENTS DATA=Projet.Base;
RUN;


/**************************************************************/
/*           STATISTIQUES DESCRIPTIVES 					*/

/**************************************************************/

      /*           ANALYSES UNIVARIEES             */
     
     
PROC MEANS DATA=PROJET.BASE N MIN Q1 Mean STD Q3 MAX;
VAR Age Value Density Poldur  Bonus Exppdays nb1 Nb2;
RUN;

PROC FREQ DATA=PROJET.BASE ;
Tables CalYear gender type occupation Group1 category group2 adind surv1 surv2 subgroup2  ;
RUN;
 


 /*       CATEGORISATION DES VARIABLES QUANTITATIVES      */

DATA PROJET.BASE1;
 SET PROJET.BASE;

/*Recodages des variables :
Pour minimiser lÅfimpact des grandes valeurs, les variables numeriques : Density et value ont ete mise en echelle logarithmique
et note ln_density et ln_value. Par la suite, les valeurs suivantes ont ete regroupees suivant leurs modalite pour eviter une 
sous-representation de certaines classes de modalites.  
*/
 
ln_Value = log(Value) ;
ln_Density = log(Density) ;
/* On va integrer une variable la duree d'observation offset comme variable explicatives supposee exogene*/
offset=exppdays/365;
/*  Discretisations,  */

/* Variable  a expliquer */
/*Surv1= 1 si la frequence des sinistres materiels RC est superieure ou egale a 1
	    0 sinon 
*/
/* Conducteur */
if Age <30 then Age_Conduct = 'A' /*'18-30'*/;
 if (Age>=30 and Age <45) then Age_Conduct ='B' /*'31-50'*/;
 if (Age >=45 and Age <60) then Age_Conduct ='C' /*'50-65'*/;
if  Age >=60  then Age_Conduct ='D' /*'65-75'*/;

if bonus<0 then Bonus_discr= '1' ;
if (bonus>=0 and bonus< 50) then Bonus_discr= '2' ;
if (bonus>=50 and bonus< 100) then Bonus_discr= '3' ;
if (bonus>=100  then Bonus_discr= '4' ;

if Density< 51 THEN Density_hab='1'; 
else if (Density>=51 AND Density<94.4) THEN Density_hab='2';
else if (Density>=94.4 AND Density<174.5) THEN Density_hab='3';
if Density>=174.5 THEN Density_hab='4';
 
 /* Contrat */
/* Anciennete du contrat en 4 categories */
IF poldur<=1 THEN poldur_4cat=1;
IF (poldur>1 AND poldur<=5) THEN poldur_4cat=2;
IF (poldur>5 AND poldur<=10) THEN poldur_4cat=3;
IF poldur>10 THEN poldur_4cat=4;

/* Vehicule */
IF Value<=8380 THEN Value_vh='1'; 
ELSE IF ( Value>8380 AND Value <=14610) THEN Value_vh='2';
IF (Value>14610 AND Value<=22575)THEN Value_vh='3';
IF Value>22575 THEN Value_vh='4';
RUN;

/* Analyse Univariee des variables recodees */
PROC FREQ DATA=PROJET.BASE1 ;
TABLES  Age_Conduct Bonus_discr Density_hab Value_vh poldur_4cat;
RUN;



   
   
      /*---------------------------------------------- */
      /*                ANALYSES  BIVARIES            */
      /*--------------------------------------------*/

PROC MEANS DATA=PROJET.BASE1;
  VAR Value Density ln_Value ln_Density Bonus offset  Poldur Nb1 Nb2;
  Class Surv1;
  RUN;

PROC FREQ DATA=PROJET.BASE1;
TABLES (CalYear GENDER Type Category Occupation Age_Conduct 
Bonus_discr Poldur_4cat Group1 Value_vh Adind Density_hab Surv2 Group2)*SURV1
/CHISQ NOCOL  NOPERCENT;
RUN;


ods graphics on;

PROC CORR DATA=PROJET.BASE1 PEARSON;
VAR Age Value Density Poldur Bonus Nb1 Nb2 offset  ;
RUN;

PROC CORR DATA=PROJET.BASE1  pearson  ;
VAR Value ln_Value Surv1 ;
RUN;

PROC CORR DATA=PROJET.BASE1 pearson plots(maxpoints = 1000000) =matrix(histogram);
VAR Density ln_Density ;
RUN;


/**************************************************************/
/*           MODELE BINAIRE		 					*/
/**************************************************************/


        /*-------------------------------------------------*/
        /*       MODELE LOGIT AVEC LES VARIABLES CategorIelles */  
         /*-----------------------------------------------*/  ;


ODS GRAPHICS ON ;
proc logistic DATA=PROJET.BASE1  plots=roc;
    class gender(ref='Male')  Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
 Value_vh(REF='1') Adind (REF='1')
    Bonus_discr (ref='1')  Poldur_4cat(REF='1')  Density_hab (ref='1');
model Surv1  (event='1') = gender Bonus_discr Density_hab Age_Conduct  
Occupation Type  Adind Value_vh Poldur_4cat/ link=logit 
 SELECTION=backward
  OFFSET=offset
  LACKFIT;
 output out=projet_modele predprobs = I;
run ; 

ods graphics off; /* MODELE 1 */

          /*-------------------------------------------------*/
        /*  MODELE LOGIT avec LES VARIABLES CategorIelles  sans Aec Bonus et sans Bonus_discr */  
ODS GRAPHICS ON ;
proc logistic DATA=PROJET.BASE1  plots=roc;
    class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model Surv1  (event='1') =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat/ link=logit 
 SELECTION=backward
  OFFSET=offset;
 output out=projet_modele predprobs = I;
run ; 
ods graphics off; /* MODELE 2 */

/*  MODELE LOGIT avec LES VARIABLES CategorIelles sans Bonus */ 
ods graphics on;
proc logistic DATA=PROJET.BASE1  plots=roc;
     class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model Surv1  (event='1') =gender  Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat/ link=logit 
 SELECTION=stepwise
  OFFSET=offset;
 output out=projet_modele predprobs = I;
run ; 
ods graphics off; /* MODELE 3 */

/* Avec les variables continues +log */
ods graphics on;
proc logistic DATA=PROJET.BASE1  plots=roc;
    class   Gender (Ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A')
      Group1(ref='10') Density_hab (ref='1');
model Surv1  (event='1') =  Gender Age_Conduct  
Occupation Type Group1 Bonus Poldur_4cat ln_Density ln_value  / link=logit 
SELECTION=Stepwise ;
run ;
ods graphics off; /* MODELE 4 */



/**************************************************************/
/*           COMPTAGE			 					*/
/**************************************************************/

   /*-------------------------------------------------*/
        /*               MODELE DE POISSON                */
         /*-----------------------------------------------*/

/**** POISSON ORDINAIRE*/

proc genmod data = Projet.Base1  PLOTS=all ;
    class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model NB1 =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat / dist = poisson
OFFSET= offset  link=log ;
output out=projet.poisson pred=predpoisson ;
store poisson;
run ;
proc sort data=projet.poisson;
   by PolNum;
run;

/* examen des valeurs predites */
proc means data = projet.poisson
n min q1 mean median q3 max std var maxdec = 4 ;
var predpoisson  Nb1;
class Surv1 ;
run;

 proc means data = projet.poisson
n min q1 mean median q3 max std var maxdec = 4 ;
var predpoisson Nb1 ;
run;

proc sgplot data = projet.poisson ;
histogram predpoisson  ;
run ;


/* REGRESSION POISSON : meme specification + correction des SE par le parametre d'echelle  */

proc genmod data = Projet.Base1  PLOTS=all ;
    class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model NB1 =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat / dist = poisson
OFFSET= offset  link=log PSCALE;
output out=projet.poisson pred=predpoisson ;
store poisson;
run ;

 /*-------------------------------------------------*/
   /*             MODELE BINOMIAL NEGATIF           */
    /*-----------------------------------------------*/   
   
proc genmod data = Projet.Base1  PLOTS=all ;
    class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model NB1 =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat  / 
OFFSET= offset dist = NB link=log ;
output out=PROJET.negbin pred=predNB;
store negbin;
RUN;

proc sort data=projet.negbin;
   by PolNum;
run;

 proc means data = projet.negbin
n min q1 mean median q3 max std var maxdec = 4 ;
var      predNB ;
class Surv1 ;
run;

 proc means data = projet.negbin
n min q1 mean median q3 max std var maxdec = 4 ;
var      predNB Nb1 ;
run;

proc sgplot data = Projet.negbin ;
histogram predNB  ;
run ;


 /* --------------------------------------------- */
      /* -------   MODELE A INFLACTION DE ZERO ---------*/
      /* -----------------------------------------------*/
   
             /*  ZIP (Poisson)    */

proc genmod data = Projet.Base1  PLOTS=all ;
     class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model NB1 =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat / 
OFFSET= offset dist = ZIP link=log ;
zeromodel bonus 
/ link = logit ;
output out=projet.zip pred=predzip  pzero=p0;
  store zip;
run ;
proc sort data=projet.zip;
   by PolNum;
run;

data projet.poisson_vs_zip;
   merge projet.poisson projet.zip;
   by PolNum;
run;

%include "C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance\vuong.sas" ;

%vuong(data=PROJET.poisson_vs_zip, response=Nb1,
       model1=zip,   p1=predzip,   dist1=zip ,   scale1=1, pzero1=p0,
       model2=poisson, p2=predpoisson, dist2=poi, scale2=1, 
       nparm1=43,nparm2=42 );

/* ZINB (binomiale)*/


proc genmod data = Projet.Base1  PLOTS=all ;
     class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model NB1 =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat / 
OFFSET= offset dist = ZINB link=log ;
zeromodel bonus 
/ link = logit ;
output out=projet.zinb pred=predzinb  pzero=p0_zinb;
  store zinb;
run ;

proc sort data=projet.zinb;
   by PolNum;
run;

data projet.all_count;
   merge projet.poisson_vs_zip projet.negbin projet.zinb;
   by PolNum;
run;

%vuong(data=projet.all_count, response=Nb1,
       model1=zinb,   p1=predzinb ,  dist1=zinb ,   scale1=1, pzero1=p0_zinb,
       model2=negbin, p2=predNB, dist2=nb, scale2=1, 
       nparm1=43   nparm2=42  ) 

proc means data = PROJET.all_count
n min q1 mean median q3 max std var maxdec = 4 ;
var      predpoisson predNB predzip predzinb Nb1 ;
class SURV1 ;
run;

 proc means data = PROJET.all_count
n min q1 mean median q3 max std var maxdec = 4 ;
var predpoisson predNB predzip predzinb Nb1;
run;


proc sgplot data = PROJET.all_count;
histogram predpoisson predNB predzip predzinb ;
run ;



              /*---------------------------*/
              /*       LA PREVISION       */
              /*--------------------------*/

LIBNAME projet "C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance";
proc import datafile="C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance\pricing.csv" 
out=PROJET.NIANE dbms=csv;
delimiter=';';
run;

/* CATEGORISATION DES VARIABLES CONTINUES */
  
DATA PROJET.HABIB;
SET PROJET.niane;

/*Recodages des variables :
Pour minimiser lÅfimpact des grandes valeurs, les variables numeriques : Density et value ont ete mise en echelle logarithmique
et note ln_density et ln_value. Par la suite, les valeurs suivantes ont ete regroupees suivant leurs modalite pour eviter une 
sous-representation de certaines classes de modalites.  
*/
 
ln_Value = log(Value) ;
ln_Density = log(Density) ;
/* On va integrer une variable la duree d'observation offset comme variable explicatives supposee exogene*/
offset=exppdays/365;
/*  Discretisations,  */

/* Variable  a expliquer */
/*Surv1= 1 si la frequence des sinistres materiels RC est superieure ou egale a 1
	    0 sinon 
*/
/* Conducteur */
if Age <30 then Age_Conduct = 'A' /*'18-30'*/;
 if (Age>=30 and Age <45) then Age_Conduct ='B' /*'31-50'*/;
 if (Age >=45 and Age <60) then Age_Conduct ='C' /*'50-65'*/;
if  Age >=60  then Age_Conduct ='D' /*'65-75'*/;

if bonus<0 then Bonus_discr= '1' ;
if (bonus>=0 and bonus< 50) then Bonus_discr= '2' ;
if (bonus>=50 and bonus< 100) then Bonus_discr= '3' ;
if (bonus>=100 then Bonus_discr= '4' ;

if Density< 51 THEN Density_hab='1'; 
else if (Density>=51 AND Density<94.4) THEN Density_hab='2';
else if (Density>=94.4 AND Density<174.5) THEN Density_hab='3';
if Density>=174.5 THEN Density_hab='4';
 
 /* Contrat */
/* Anciennete du contrat en 4 categories */
IF poldur<=1 THEN poldur_4cat=1;
IF (poldur>1 AND poldur<=5) THEN poldur_4cat=2;
IF (poldur>5 AND poldur<=10) THEN poldur_4cat=3;
IF poldur>10 THEN poldur_4cat=4;

/* Vehicule */
IF Value<=8380 THEN Value_vh='1'; 
ELSE IF ( Value>8380 AND Value <=14610) THEN Value_vh='2';
IF (Value>14610 AND Value<=22575)THEN Value_vh='3';
IF Value>22575 THEN Value_vh='4';
RUN;

ODS GRAPHICS ON ;
proc logistic DATA=PROJET.BASE1  plots=roc;
    class gender(ref='Male') Age_Conduct(ref = 'B') Occupation (ref ='Employed') Type (ref ='A') 
  Adind (REF='1') Group1(ref='10')  Density_hab (ref='1') Poldur_4cat(REF='1');
model Surv1  (event='1') =Gender Bonus Density_hab Age_Conduct  
Occupation Type Group1 Adind  Poldur_4cat/ link=logit 
 SELECTION=backward;
 score data=PROJET.HABIB out=sortie;
run ; 

DATA WORK.Prevision1;
merge Projet_modele projet.Poisson projet.negbin projet.zip Projet.Zinb;
KEEP PolNum IP_1 predpoisson predNB prodzip predZinb;
run;

DATA WORK.prevision (rename=IP_1=Proba);
SET WORK.Sortie;
KEEP PolNum IP_1;
RUN;

PROC EXPORT DATA = WORK.SORTIE
         OUTFILE = "C:\Users\mbowe\Desktop\M2_MIASHS\EconometrieAssurance\Projets_Assurance\prevision.csv"
DBMS=csv
REPLACE;
DELIMITER=";";
RUN;
    
