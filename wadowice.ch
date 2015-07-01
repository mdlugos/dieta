#include "lan.ch"
#define PC852
//#define A_EXT chgwin
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_DDBF
#define A_DRUKCOMP
#define isPrinter (.t.)
#define A_XPRN
#command INIT SCREEN => __run("uniznak r")
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_15CPI
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT GERMAN
#define STANY INDX_MAT
#define A_SUMK    -32566727102
#define A_KOMU_N  "Dom Pomocy Spoˆecznej"
#define A_KOMU_A  "Wadowice, ul. Parkowa 1"
#define A_AUTOR   "A.D. 1992-2011, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
#define A_NOZAP
#define DatE() MEMVAR->dzisiaj
#define A_LPNUM 3
#define A_SWW
#define A_WAGI
#define A_BACKUP MEMVAR->backup
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_ZAP_DAN
#define A_WADO eval(MEMVAR->podpis)
#define A_JMALT
#define ZAP_BIEZ memvar->zap_biez
