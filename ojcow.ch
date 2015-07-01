#include "mdstd.ch"
//#define A_GOCZ
#define PC852
#define PLWIN
#define PROC_EN memvar->proc_en
//#define A_WAGI
#define A_NARZUT (100+field->narzut)/100
#command INIT SCREEN => //__run("uniznak 8 r")
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_XPRN
#define A_STYLUS
#define A_PCL
#define A_LPNUM 2
#define UpP(x) UPPER(x)
//#define A_DDBF
//#command INIT PRINTER => qqout("@P")
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SWW
#define A_SUMK    -45580404756
#define A_KOMU_N  "Dom Pomocy Spoˆecznej Im ˜w. Brata Alberta"
#define A_KOMU_A  "Zgromadzenie Braci Albertyn¢w Ojc¢w 64"
#define A_AUTOR   "A.D. 1992-1999, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
//#define MAG_BIEZ MEMVAR->mag_biez
