#include "std.ch"
#define PC852
#command INIT SCREEN => __run("font852")
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad)) //"@P")
#define A_NARZUT FIELD->narzut/100
#define A_WADO eval(memvar->podpis)
#define UpP(x) UPPER(x)
#define A_XPRN
#define A_DRUKCOMP
#define DatE() memvar->dzisiaj
#define isPrinter() .t.
#define A_LPNUM
#define A_WAGI
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -51599523140
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Os¢b Starszych"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. ½ywiecka 15"
#define A_AUTOR   "AD 1992-2004, Marek Dˆugosz, http://www.polbox.com/m/mdlugosz, tel.0-601842030"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
#define ZAP_BIEZ MEMVAR->zap_biez
