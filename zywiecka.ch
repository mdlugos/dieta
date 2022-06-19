#include "lan.ch"
#define A_DILTH 9
#define PC852
#define A_BACKUP memvar->backup
#ifdef __HARBOUR__
    #define A_PCL
    #define A_15CPI
    #define A_PRINT(x) eval(memvar->do_print,x)
    #define A_GETLPT eval(memvar->do_getlpt)
    #define PLWIN
    #define A_WIN_PRN memvar->p_win
    #define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
#command INIT SCREEN => //__run("font852")
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad)) //"@P")
#define A_DDBF
#define A_GREX
#define A_NARZUT (100*FIELD->narzut)
#define A_WADO eval(memvar->podpis)
#define UpP(x) UPPER(x)
#define A_XPRN
#define A_DRUKCOMP
#define DatE() memvar->dzisiaj
#define isPrinter() .t.
#define A_LPNUM
#define A_WAGI
#define A_NOZAP
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -42315376231
#define A_KOMU_N  "Dom Pomocy Spoˆecznej dla Os¢b Starszych"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. ½ywiecka 15"
#define A_AUTOR   "AD 1991-2016, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
//#define ZAP_BIEZ MEMVAR->zap_biez
#define PROC_EN  MEMVAR->proc_en
