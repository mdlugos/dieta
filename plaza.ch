#ifdef __PLATFORM__WINDOWS
    #define PLWIN
    #define A_WIN_PRN memvar->p_win
    #define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
#define A_ZAP_DAN
//#define A_GOCZ
#include "lan.ch"
#define PC852
#define UpP(x) UPPER(x)
#define A_STYLUS
#define A_XPRN
#define A_PCL
#define isPrinter (.t.)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -26017207231
#define A_KOMU_N  'Dom Pomocy Spoˆecznej w Pˆazie'
#define A_KOMU_A  'Pˆaza, ul. Wiosny Lud¢w 4'
#define A_AUTOR   'A.D. 2002-2017, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553'
#define A_DIETA   .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM 3
#define A_DILTH 9
#define A_GREX
#define A_DDBF
#define A_SWW
#define A_WAGI
#define A_BACKUP MEMVAR->backup
#define A_WADO eval(memvar->podpisy)
