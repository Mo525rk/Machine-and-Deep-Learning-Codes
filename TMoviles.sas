libname trabajo "C:\Users\Usuario\Desktop\A entregar\EAMD\12-10-PracticaFinal";


/***************************************/
/*1. ANALISIS EXPLORATORIO DE LOS DATOS*/
/***************************************/
/*--------------------------*/
/*VARIABLES CUANTITATIVAS  -*/
/*--------------------------*/

/*Estadísticos básicos de las variables cuantitativas*/
proc means data = trabajo.moviles
	MEAN
	STD
	MIN
	MAX
	N
	NMISS;
	VAR bateria vel_micro camara almacenamiento 
		grosor peso nucleos res_alt res_anch
		ramGB pixeles benchmarking precio;
run;


/*Análisis de la matriz de correlaciones para dependencia en variables numéricas*/
proc princomp DATA= trabajo.moviles plots=ALL 
       		OUTSTAT=estad
			OUT=proj;
	  VAR bateria vel_micro camara almacenamiento 
		  grosor peso nucleos res_alt res_anch
		  ramGB pixeles benchmarking precio;
run;


/*Estandarización de las variables cuantitativas para posterior detección de outliers*/
PROC STANDARD data=trabajo.moviles mean=0 std=1 out=trabajo.standarpre;
    VAR bateria vel_micro camara almacenamiento 
		grosor peso nucleos res_alt res_anch
		ramGB pixeles benchmarking precio;
RUN;

/*Se compilan las macros*/
%include "C:\Users\Usuario\Desktop\A entregar\EAMD\12-10-PracticaFinal\Macro_detencion_outliers.sas";
%include "C:\Users\Usuario\Desktop\A entregar\EAMD\12-10-PracticaFinal\Macro_tramos.sas";
%include "C:\Users\Usuario\Desktop\A entregar\EAMD\12-10-PracticaFinal\Macro_normalidad.sas";

/*Análisis de outliers*/
%DeteccionOutliers(lib=trabajo,dataset=standarpre,
varNames=bateria vel_micro camara almacenamiento 
	           grosor peso nucleos res_alt res_anch
		       ramGB pixeles);



/*---------------------------------------------------------------------------------------------*/
/*--------------------------*/
/*VARIABLES CUALITATIVAS   -*/
/*--------------------------*/


/*Tablas de frecuencias de las variables culitativas*/
PROC FREQ data=trabajo.moviles;
    TABLES bluetooth dual_sim cuatroG tresG tactil wifi;
RUN;

/*Histogramas benchmarking*/
PROC UNIVARIATE DATA=trabajo.moviles;
CLASS bluetooth;
VAR benchmarking;
HISTOGRAM;
RUN; 

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS dual_sim;
VAR benchmarking;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS cuatroG;
VAR benchmarking;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS tresG;
VAR benchmarking;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS tactil;
VAR benchmarking;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS wifi;
VAR benchmarking;
HISTOGRAM;
RUN;

/*Histogramas precio*/
PROC UNIVARIATE DATA=trabajo.moviles;
CLASS bluetooth;
VAR precio;
HISTOGRAM;
RUN; 

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS dual_sim;
VAR precio;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS cuatroG;
VAR precio;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS tresG;
VAR precio;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS tactil;
VAR precio;
HISTOGRAM;
RUN;

PROC UNIVARIATE DATA=trabajo.moviles;
CLASS wifi;
VAR precio;
HISTOGRAM;
RUN;


/*Test de chi cuadrado*/
PROC FREQ DATA=trabajo.moviles;
TABLE bluetooth / chisq norow nocol nopercent;
TABLE dual_sim / chisq norow nocol nopercent;
TABLE cuatroG / chisq norow nocol nopercent;
TABLE tresG / chisq norow nocol nopercent;
TABLE tactil / chisq norow nocol nopercent;
TABLE wifi / chisq norow nocol nopercent;
RUN;


/*Analisis de correspondencias simples*/
PROC FREQ DATA=trabajo.moviles;
TABLE bluetooth*dual_sim / chisq norow nocol nopercent;
TABLE bluetooth*cuatroG / chisq norow nocol nopercent;
TABLE bluetooth*tresG / chisq norow nocol nopercent;
TABLE bluetooth*tactil / chisq norow nocol nopercent;
TABLE bluetooth*wifi / chisq norow nocol nopercent;
TABLE dual_sim*cuatroG / chisq norow nocol nopercent;
TABLE dual_sim*tresG / chisq norow nocol nopercent;
TABLE dual_sim*tactil / chisq norow nocol nopercent;
TABLE dual_sim*wifi / chisq norow nocol nopercent;
TABLE cuatroG*tresG / chisq norow nocol nopercent;
TABLE cuatroG*tactil / chisq norow nocol nopercent;
TABLE cuatroG*wifi / chisq norow nocol nopercent;
TABLE tresG*tactil / chisq norow nocol nopercent;
TABLE tresG*wifi / chisq norow nocol nopercent;
TABLE tactil*wifi / chisq norow nocol nopercent;
RUN;




/*---------------------------------------------------------------------------------------------*/
/*----------------------------------*/
/*ANALISIS COMPONENTES PRINCIPALES -*/
/*----------------------------------*/

/*Estandarización de las variables cuantitativas*/
PROC STANDARD data=trabajo.moviles mean=0 std=1 out=trabajo.standar;
    VAR camara almacenamiento 
		grosor nucleos res_alt res_anch
		ramGB;
RUN;


/*Sintaxis del procedimiento princomp sobre la matriz de covarianzas de las variables estandarizadas*/
proc princomp DATA= trabajo.standar cov plots=ALL 
       		OUTSTAT=estad
			OUT=proj;
	  VAR camara almacenamiento 
	      grosor nucleos res_alt res_anch
	      ramGB;
run;


/*--------------------------*/
/*ANALISIS FACTORIAL       -*/
/*--------------------------*/
/*Sintaxis del procedimiento factor sobre la matriz de correlaciones con la opción de todas las gráficas*/
proc factor data= trabajo.standar cov rotate=VARIMAX plots=all n=3
			outstat=fact_est
			out=fact;
	  VAR camara almacenamiento 
	      grosor nucleos res_alt res_anch
	      ramGB;
run;


data fact;
set fact; 
bluetooth2 = bluetooth*1;
dual_sim2 = dual_sim*1;
cuatroG2 = cuatroG*1;
tresG2 = tresG*1;
tactil2 = tactil*1;
benchmarking2 = benchmarking*1;
drop bluetooth dual_sim cuatroG tresG tactil benchmarking;
rename bluetooth2 = bluetooth dual_sim2 = dual_sim cuatroG2 = cuatroG 
tresG2 = tresG tactil2 = tactil benchmarking2 = benchmarking;
run;

/************************************************************************/
/*2. AGRUPACIÓN DE LOS TERMINALES EN GRUPOS DE CARACTERÍSTICAS SIMILARES*/
/************************************************************************/
%DeteccionOutliers(lib=work,dataset=fact,
varNames=factor1 factor2 factor3 bateria vel_micro peso pixeles);


/*Generacion de los cluster y sus gráficos*/
PROC CLUSTER DATA= fact method=ave standard plots=all
				outtree=Tree CCC pseudo;
VAR factor1 factor2 factor3 bateria vel_micro peso pixeles 
	bluetooth dual_sim cuatroG tactil;
COPY factor1 factor2 factor3 bateria vel_micro peso pixeles 
	bluetooth dual_sim cuatroG tactil benchmarking;
RUN;

PROC TREE DATA =Tree ncl=3 out=salida_cluster horizontal;
height _rsq_;
COPY factor1 factor2 factor3 bateria vel_micro peso pixeles bluetooth dual_sim cuatroG benchmarking;
RUN;


/*Análisis de Correspondencias Simples (relación entre cada cluster y sus modalidades)*/
proc corresp data=salida_cluster print=both all out= salida_coresp dimens=2;
Tables cluster, benchmarking;
run;


/************************************/
/*3. DESARROLLO DE DISTINTOS MODELOS*/
/************************************/

/**********************************************************************************/
/*a. Determinar benchmarking a partir de las características de un nuevo terminal */
/**********************************************************************************/
/*-------------------------*/
/*ANALISIS DISCRIMINANTE  -*/
/*-------------------------*/

/*Con los factores obtenidos*/
PROC STEPDISC DATA =fact
METHOD =STEPWISE SLE=0.05 SLS=0.10;
VAR factor1 factor2 factor3 bateria vel_micro peso pixeles ;
CLASS benchmarking;
RUN;

/*Con las variables originales*/
PROC STEPDISC DATA = trabajo.standar  
METHOD =STEPWISE SLE=0.05 SLS=0.10;
VAR bateria vel_micro camara almacenamiento grosor peso nucleos res_alt res_anch ramGB pixeles;
CLASS benchmarking;
RUN;


/*Outliers*/
/*Variables originales*/
data bench1;
set trabajo.standar;
where benchmarking= 1 ;
run;
%DeteccionOutliers (work,bench1,benchmarking,varNames= almacenamiento res_alt ramGB vel_micro grosor);

data bench2;
set trabajo.standar;
where benchmarking= 2 ;
run;
%DeteccionOutliers (work,bench2,benchmarking,varNames=almacenamiento res_alt ramGB vel_micro grosor);


data bench3;
set trabajo.standar;
where benchmarking= 3 ;
run;
%DeteccionOutliers (work,bench3,benchmarking,varNames=almacenamiento res_alt ramGB vel_micro grosor);


data bench4;
set trabajo.standar;
where benchmarking= 4 ;
run;
%DeteccionOutliers (work,bench4,benchmarking,varNames= almacenamiento res_alt ramGB vel_micro grosor);

data bench5;
set trabajo.standar;
where benchmarking= 5;
run;
%DeteccionOutliers (work,bench5,benchmarking,varNames=almacenamiento res_alt ramGB vel_micro grosor);


data bench6;
set trabajo.standar;
where benchmarking= 6 ;
run;
%DeteccionOutliers (work,bench6,benchmarking,varNames=almacenamiento res_alt ramGB vel_micro grosor);


data bench7;
set trabajo.standar;
where benchmarking= 7 ;
run;
%DeteccionOutliers (work,bench7,benchmarking,varNames=almacenamiento res_alt ramGB vel_micro grosor);


/*NORMALIDAD MULTIVARIANTE*/

%multnorm (version 9.4 , data =trabajo.standar, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench1, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench2, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench3, var = almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench4, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench5, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench6, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);
%multnorm (version 9.4 , data =bench7, var =almacenamiento res_alt ramGB vel_micro grosor, plot= both);



/*Separar las muestras de training y test*/
proc sort data=trabajo.standar; by benchmarking;run;
proc surveyselect data=trabajo.standar out=trabajo.standarsep outall method=srs samprate=0.8 seed=3333;
strata benchmarking;
run;

data train test;
set trabajo.standarsep;
if selected=1 then output train;
else output test;
run;

/*Contraste de la igualdad de medias*/
PROC DISCRIM DATA =train anova manova method =normal;
CLASS benchmarking;
VAR almacenamiento res_alt ramGB vel_micro grosor;
PRIORS prop;
RUN;

/*Contraste de la hipotesis de homogeneidad de la matriz de covarianzas*/

PROC DISCRIM DATA=train pool =test slpool=0.1 method =normal;
CLASS benchmarking;
VAR almacenamiento res_alt ramGB vel_micro grosor;
PRIORS prop;
RUN;


/*Otras salidas*/
PROC DISCRIM DATA=train TESTDATA =test pool =test slpool=0.1 method =normal
out=fundisc outstat=sta_fundisc listerr;
CLASS benchmarking;
VAR almacenamiento res_alt ramGB vel_micro grosor;
PRIORS prop;
RUN;



/*Validacion Cruzada*/
PROC DISCRIM DATA=trabajo.standar pool =test slpool=0.1 method =normal
out=crossval outstat=sta_crossval crossvalidate;
CLASS benchmarking;
VAR almacenamiento res_alt ramGB vel_micro grosor;
PRIORS prop;
RUN;



/**********************************************************************************/
/*b. Determinar precio a partir de las características de un nuevo terminal       */
/**********************************************************************************/

/*-------------------------*/
/*REGRESION LINEAL        -*/
/*-------------------------*/

/*Separar los conjuntos de entrenamiento y de prueba*/
proc sort data=fact; by benchmarking;run;
proc surveyselect data=fact out=factsep outall method=srs samprate=0.8 seed=3333;
strata benchmarking;
run;

data train1 test1;
set factsep;
if selected=1 then output train1;
else output test1;
run;

data train2;
set train1; 
bluetooth2 = bluetooth*1;
dual_sim2 = dual_sim*1;
cuatroG2 = cuatroG*1;
tresG2 = tresG*1;
tactil2 = tactil*1;
wifi2 = wifi*1;
drop bluetooth dual_sim cuatroG tresG tactil wifi;
rename bluetooth2 = bluetooth dual_sim2 = dual_sim cuatroG2 = cuatroG 
tresG2 = tresG tactil2 = tactil wifi2 = wifi;
run;

/*Seleccion de variables para el modelo por diferentes metodos*/

/*Cp de Mallows*/
PROC REG DATA=train2;
model precio=factor1 factor2 factor3 bateria vel_micro peso pixeles 
	         dual_sim cuatroG tactil / 
selection=cp aic rsquare adjrsq bic;
QUIT;

/*Seleccion hacia delante*/
PROC REG DATA =train2;
model precio=factor1 factor2 factor3 bateria vel_micro peso pixeles  dual_sim cuatroG tactil/ 
selection =forward slentry=0.05;
QUIT;

/*Seleccion hacia atras*/
PROC REG DATA =train2;
model precio=factor1 factor2 factor3 bateria vel_micro peso pixeles  dual_sim cuatroG tactil/ 
selection =backward slstay =0.05;
QUIT;

/*Seleccion por pasos*/
PROC REG DATA =train2;
model precio=factor1 factor2 factor3 bateria vel_micro peso pixeles  dual_sim cuatroG tactil/ 
selection =stepwise slentry=0.05 slstay =0.05;
QUIT;


PROC CORR data=train2 cov; 
var factor1 factor2 factor3 bateria vel_micro peso;
run;

/*Deteccion de observaciones influyentes*/
PROC REG data =train2
plots =(RStudentByLeverage CooksD DFFITS DFBETAS);
model precio=factor1 factor2 factor3 bateria vel_micro peso dual_sim cuatroG tactil/ influence;
output out= salida r =resid cookd =cookd dffits= dffits ;
QUIT;


/*Test de normalidad de los residuos*/
PROC UNIVARIATE DATA =salida plot normal ;
VAR resid;
RUN;


/*Modelo de regresion*/
PROC REG DATA =train2;
model precio=factor1 factor2 factor3 bateria vel_micro peso dual_sim cuatroG tactil/ ;
QUIT;







