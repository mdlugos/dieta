#include "lan.ch"
#define A_WAGI
#define PROC_EN
#define A_POLOWA
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_PCL
#define A_XPRN
#define A_STYLUS
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define A_DDBF
#define STANY   INDX_MAT
#define A_SUMK    -37293221861
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Dzieci"
#define A_KOMU_A  "Strumieä, ul. 1-go Maja 12"
#define A_AUTOR   "A.D. 1992-1996, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_DRUKCOMP
#define A_LPNUM 2
#define A_WADO eval(MEMVAR->podpis)
