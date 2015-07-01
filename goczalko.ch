#define A_DILTH 4
#define PLWIN
#ifdef __HARBOUR__
//  #define A_ADS 1
#endif
#include "lan.ch"
#define A_GOCZ
#define A_WO_JAD "  3"
#define A_KODY "Nr kodowy"
#define PC852
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define PROC_EN MEMVAR->proc_en
#command INIT SCREEN => __run("uniznak 8 r")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_STYLUS
#define A_PCL
#define A_15CALI
#define A_LPNUM 2
#define A_DDBF
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SWW
#define A_SUMK    -41325921079
#define A_KOMU_N  "Uzdrowisko Goczaˆkowice Zdr¢j"
#define A_KOMU_A  "Goczaˆkowice, ul. Uzdrowiskowa 54"
#define A_AUTOR   "A.D. 1992-2004, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0-601842030"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
//#define A_MYSZ
#define A_NOZAP
#define ZAP_BIEZ MEMVAR->mag_biez
