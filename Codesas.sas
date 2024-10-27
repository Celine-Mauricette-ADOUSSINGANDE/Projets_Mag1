

/*Affichage de la base de données*/
proc contents data= WORK.PROJET;run;

/* Créez une nouvelle variable binaire nommée "Conso_alcool_encoded" */
data work.PROJET_encoded;
set work.PROJET;
if Conso_alcool_O_N = "Occasionnellement (en soirée, le week-end...)" then Conso_alcool_encoded =1;
else if Conso_alcool_O_N = "Souvent (3 à 5 fois par semaine)" then Conso_alcool_encoded =1 ;
else if Conso_alcool_O_N = "Tous les jours" then Conso_alcool_encoded =1;
else if Conso_alcool_O_N = "Jamais" then Conso_alcool_encoded =0;run;


proc contents data=work.PROJET_encoded; run;

proc freq data=work.PROJET_encoded;table Conso_alcool_encoded; run;

/*Création de l'indicatrice de réponse*/

/*impossible d'utiliser ceci
data work.PROJET_encoded1;
set work.PROJET_encoded;
if Conso_alcool_encoded =1 then nonrep = 0;
else if Conso_alcool_encoded= 0 then nonrep= 0; 
else  nonrep=1;
run;*/

/*Donc on utilise la variable répondants qu'il y a dans la base*/

data work.PROJET_encoded1;
set work.PROJET_encoded;
if Repondants ="Oui" then nonrep = 0;/* Car il a répondu*/
else nonrep= 1; /* Car il n'a pas répondu */
run;

proc freq data=work.PROJET_encoded1;table nonrep; run;

/*Vérification de la corrélation entre l'indicatrice de réponse et la variable d'intéret*/
/*Test de chi-carré*/

PROC FREQ DATA=work.PROJET_encoded1;
TABLES nonrep*Conso_alcool_encoded / CHISQ;
RUN;

/*Corrélation non négligeable donc biais non négligeable
recherche d'une variable de strate liée à la non réponse en exploitant l'information auxiliaire
Modélisationde de la probabilité  de non réponse à l'aide d'un proc probit*/

/*Encodage des informations auxiliares*/


data work.PROJET_encoded2;
set work.PROJET_encoded1; 

if Age_A < 23 then Age_Aencoded=1;
else if Age_A < 28 then Age_Aencoded=2;
else  Age_Aencoded=3; 

if Nationalite="FRANCAISE" then Natcoded=1;
else Natcoded=0;run;



/*Modèle PROBIT*/
proc probit data=work.PROJET_encoded2;
model nonrep = Age_Aencoded Natcoded Type_bac_A ;run;

/*Les deux variables sont liées à la bon réponse.  On va stratifier par nationalité*/


/*Récupère l'echantillon de la base initiale.  Si on considère la population de répondants comme aléatoire*/

DATA work.echantillonOVE;
    SET work.PROJET_encoded2;
    IF Conso_alcool_encoded= 1 OR Conso_alcool_encoded = 0;
RUN;

proc freq data=work.echantillonOVE;
table Conso_alcool_encoded; run;

/*on répertorie l'echantillon par strate.  On stratifie par nationalité*/

data work.CL2;	/* nouvelle base de donnees */
set work.echantillonOVE;		/* base d'origine */  
if Natcoded=1 then strate=1;
else strate=2;run;
; /* strate est "h" du cours h=1,...5 */

proc freq data=work.CL2;	             /* tableau de fréquences */
table strate;
run;


/*Il faut d’abord commencer par trier les données par strate comme ca il va avoir la variable de strate bien trié.  
 on trie les donnees */
proc sort data=work.CL2;by strate;run; 

/*Moyenne par catégorie*/
proc means data=work.CL2;
by strate;
var Conso_alcool_encoded;run;
/*moyenne dans l'échantillon on peut calculer manuellement aussi*/

proc means data=work.CL2;
var Conso_alcool_encoded;run;

/* Création de la strate dans la po^pulation pour caler sur les effectifs de la population*/

data work.PROJETSTRATE;	/* nouvelle base de donnees */
set work.PROJET_encoded2;		/* base d'origine */  
if Natcoded=1 then strate=1;
else strate=2;run;


/*Récupération des effectifs*/
proc freq data=PROJETSTRATE;
table strate;run;



/*Calage de l'echantillon*/
proc surveymeans data=work.CL2;
poststrata strate/PSTOTAL=(37877,5282) outpswgt=echantillon_red1;
var Conso_alcool_encoded ;
run;

proc freq data=work.echantillon_red1;
tables _PSWt_;
run;


