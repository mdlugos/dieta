#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
 #define A_WIN_PRN .t.
 #define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Us�ug Informatycznych Marek D�ugosz, 43-400 Cieszyn, ul. R�wna 16'
 #define PLWIN
#endif
#define A_NORMY
#define A_SUMOS 3
#define A_GREX
#define PC852
#define A_WAGI
//#define PROC_EN memvar->proc_en
//#define A_WO_JAD '  3'
#define A_POLOWA
#define A_DILTH 9
#define A_ZAP_DAN
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
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
#define A_SUMK    -34772187218
#define A_KOMU_N  "Dom Pomocy Spo�ecznej dla Dzieci"
#define A_KOMU_A  "Strumie�, ul. 1-go Maja 12"
#define A_AUTOR   "A.D. 1992-2017, Marek D�ugosz, Cieszyn, ul. R�wna 16, tel. 338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_DRUKCOMP
#define A_LPNUM 2
#define A_WADO eval(MEMVAR->podpis)
