#include "mdstd.ch"
#ifdef __PLATFORM__WINDOWS
  #define PLWIN
  #define A_WIN_PRN .t.
  #define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#endif
#define A_NORMY
#define A_CDX DBFCDX
#define A_MYSZ
//#define A_WAGI
#define PROC_EN memvar->proc_en
//#define A_WO_JAD '  3'
//#define A_ZAP_DAN
#define A_POLOWA
//#define A_GREX
#define A_DILTH 4
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define PC852
#define A_PCL
#define A_XPRN
#define A_15CPI
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -49333212608
#define A_KOMU_N  'Powiatowa Stacja Sanitarno-Epidemiologiczna'
#define A_KOMU_A  "Cieszyn, ul. Liburnia 2a"
#define A_AUTOR   "A.D. 2016, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_DIETA
#define A_FILELIMIT '45'
#define A_NOZAP
#define A_LPNUM 2
//#define A_DDBF
//#define A_WADO eval(MEMVAR->podpis)
