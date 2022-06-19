#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
 #define PLWIN
 #define A_WIN_PRN .t.
 #define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
#define A_DDBF
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->P_INIT,wasbad))
#define A_NARZUT (100+field->narzut)
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_BACKUP    memvar->bejkap
#define UpP(x) UPPER(x)
//#define A_EXT __RUN
#define A_XPRN
#define A_PCL
#define PC852
#define A_DILTH 4
#define A_WO_JAD memvar->energia
#define A_ZAP_DAN
#define PROC_EN memvar->proc_en
#define isPrinter() .t.
#define DatE()    MEMVAR->dzisiaj
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SUMK    -41237034438
#define A_KOMU_N  "Dom Pomocy Spoˆecznej Braci Albertyn¢w"
#define A_KOMU_A  "Bulowice, ul. Bˆ.Faustyny 119"
#define A_AUTOR   "A.D. 1992-1996, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)24048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_DRUKCOMP
#define A_LPNUM 2
