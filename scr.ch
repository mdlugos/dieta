#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
#define PLWIN
#define A_WIN_PRN .t.
#define A_STOPKA 'Program: System Dieta, '+wersja()+', producent: Firma Us�ug Informatycznych Marek D�ugosz, 43-400 Cieszyn, ul. R�wna 16'
#endif
#define A_DILTH 9
#define A_GREX
#define A_MYSZ
#define A_SCR
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_ZAP_DAN
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_BACKUP MEMVAR->backup
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
#define A_SUMK    -37773342968
#define A_KOMU_N  '�l�skie Centrum Rehabilitacji i Prewencji'
#define A_KOMU_A  "Ustro�, ul. Zdrojowa 6"
#define A_AUTOR   "A.D. 2011-2016, Marek D�ugosz, Cieszyn, ul. R�wna 16, tel. 338522553"
#define A_DIETA
#define A_FILELIMIT '45'
#define A_NOZAP
#define A_JMALT
#define A_LPNUM 2
#define A_DDBF
#define A_WADO eval(MEMVAR->podpis)
#define MAG_BIEZ "11"
