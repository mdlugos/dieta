#include "lan.ch"
#ifdef __PLATFORM__WINDOWS
  #define A_WIN_PRN .t.
  #define PLWIN
#endif
#define A_DILTH 4
#define A_NORMY
#define PC852
#define A_15CPI
#define A_BACKUP memvar->bejkap
//#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_STYLUS
#define A_XPRN
#define A_PCL
#define A_WADO "      Wystawiˆ:                               Zatwierdziˆ:"
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
//#define A_WAGI
#define isPrinter (.t.)
#define MAG_BIEZ memvar->mag_biez
#define ZAP_BIEZ memvar->zap_biez
#define A_ZAP_DAN
#define A_GOCZ
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_DRUKCOMP
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
//#define STANY     INDX_MAT
#define A_SUMK    -29424524016
#define A_KOMU_N  '½ˆobki Miejskie'
#define firmA_N   A_KOMU_N+subs(firma_n,rat(' -',firma_n))
#define A_KOMU_A  'Cieszyn, ul. Moniuszki 13'
#define A_AUTOR   'A.D. 2002-2014, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553'
#define A_DIETA   .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM 2
#define A_DDBF
#define A_KODY "alergen"
