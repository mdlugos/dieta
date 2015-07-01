#include "lan.ch"
#define UpP(x) UPPER(x)
#define A_CDX DBFCDX
#define A_DDBF
#define PC852
#command INIT SCREEN =>
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#ifdef __PLATFORM__WINDOWS
  #define PLWIN
#endif
#define A_NOZAP
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_15CPI
#define A_XPRN
#define A_DRUKCOMP
#define A_PCL
#define A_STYLUS
#define isPrinter() .t.
#define DTOV(dat) left(dtoc(dat),5)
#define DatE() memvar->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -34686481916
#define A_KOMU_N  "DOM POMOCY SPOùECZNEJ"
#define A_KOMU_A  "Izdebnik 3"
#define A_AUTOR   "A.D.1991-2000, Marek Dàugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_LPNUM 2
#define A_MYSZ
//#define PROC_EN
