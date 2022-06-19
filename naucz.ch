#include "lan.ch"
#define A_MYSZ
#define PC852
//#define PLWIN
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(memvar->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_EXT WIN_PRINTFILERAW
#define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define PROC_EN memvar->proc_en
#define A_15CPI
#define A_15CALI
#define isPrinter() .t.
#define UpP(x) UPPER(x)
#define A_XPRN
#define A_DRUKCOMP
#define DTOV(dat) left(dtoc(dat),5)
#define A_DDBF
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_KODY    "Alergen"
#define A_JMALT
#define A_NOZAP
#define A_SUMK    -42464526415
#define A_KOMU_N  'Dom Pomocy Spoˆecznej - "Dom Nauczyciela"'
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Pocztowa 14a"
#define A_AUTOR   "A.D. 1991-2015, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_LPNUM 2
#define A_DILTH 4
#define A_DISUM
//#define A_ZAP_DAN
#define A_BACKUP  defa+'bejkap.bat'
