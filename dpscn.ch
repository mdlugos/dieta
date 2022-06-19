#include "lan.ch"
#define A_NORMY
#define PC852
#define PLWIN
#define A_DDBF
#define A_CDX DBFCDX
#define A_DILTH 9
#define A_GREX
#define A_ZAP_DAN
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_MALWA
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
//"&l3A(17U(s10h12V")
#define A_PCL
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
//#define PROC_EN
#define A_XPRN
#define isPrinter() .t.
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_SWW
#define A_SUMK    -32558939352
#define A_KOMU_N  "DOM POMOCY SPOECZNEJ"
#define A_KOMU_A  "Cieszyn, ul. Korfantego 1"
#define A_AUTOR   "A.D. 1991-2010, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define DatE()    MEMVAR->dzisiaj
#define A_LPNUM 2
#define A_BACKUP memvar->backup
