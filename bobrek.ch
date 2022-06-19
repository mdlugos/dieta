#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_STYLUS
  #define A_15CPI
  #define A_GETLPT   eval(memvar->do_getlpt)
  #define A_PRINT(x) eval(memvar->do_print,x)
  #define A_PCL
//  #define A_WIN_PRN .t.
#endif
#define PROC_EN  memvar->proc_en
#define A_WO_JAD "  3"
#define A_DDBF
#define A_BACKUP defa+'bejkap.bat'
#define A_GREX
#define A_ZAP_DAN
#define A_DILTH 9
#define A_LPNUM 3
#command INIT SCREEN => //__run("font")
#command INIT PRINTER => wwout(eval(MEMVAR->P_INIT,wasbad))
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define A_NOZAP
#define STANY     INDX_MAT
#define A_SUMK    -34434014895
#define A_KOMU_N  "Dom Pomocy Spoˆecznej"
#define A_KOMU_A  "Bobrek, ul. Ksi©¾nej Ogiäskiej 2"
#define A_AUTOR   "A.D. 1995-2006, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
#define A_DRUKCOMP
#define A_XPRN
#define UpP(x) UPPER(x)
//#define A_WAGI
#define PC852
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_WADO eval(MEMVAR->podpis)
#define A_JMALT
