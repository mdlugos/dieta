#include "lan.ch"
#define A_DDBF
#define A_LPNUM 2
#define A_DILTH 9
#define A_GREX
#define PROC_EN  memvar->proc_en
#define A_WO_JAD memvar->energia
#define UpP(x) UPPER(x)
#command INITIALIZE SCREEN => //__run("font852.com")
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_PCL
#define A_XPRN
#define A_BACKUP eval(memvar->do_backup)
#ifdef __PLATFORM__WINDOWS
#define A_EXT HB_CODEPAGE_PLMAZ
#define PLWIN
#define A_WIN_PRN eval(memvar->do_getoprn)
#define A_GETLPT eval(memvar->do_getlpt,getenv('mdslpt'))
#define A_PRINT(x) eval(memvar->do_print,x)
#endif
#define PC852
#define A_STYLUS
#define A_17CPI
//#define A_15CPI
#define A_DRUKCOMP
#define DTOV(dat) left(dtoc(dat),5)
#define DatE() M->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_KODY    'Rodzaj'
#define A_SUMK    -45625664166
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla przewlekle chorych"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Olimpijska 11"
#define A_AUTOR   "A.D. 1992-1997, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
#define ZAP_BIEZ memvar->zap_biez
#define A_WADO eval(memvar->podpis)
