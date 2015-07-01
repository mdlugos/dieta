#include "mdstd.ch"
#define PLWIN
#define A_WAGI
#define A_DILTH 4
#define A_WO_JAD '   12'
//#define A_GOCZ
#define PC852
#command INIT SCREEN => //__run("uniznak 8 r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define UpP(x) UPPER(x)
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_XPRN
#define A_STYLUS
#define A_PCL
#define A_LPNUM 2
//#define A_DDBF
//#command INIT PRINTER => qqout("@P")
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
//#define A_SWW
#define A_SUMK    -37349365841
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Dzieci"
#define A_KOMU_A  "Skocz¢w, ul. Mickiewicza 36"
#define A_AUTOR   "A.D. 1992-2012, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
//#define MAG_BIEZ MEMVAR->mag_biez
