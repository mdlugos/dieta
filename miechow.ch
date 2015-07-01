#define PC852
#ifdef __HARBOUR__
  #define PLWIN
#else
  #define UpP(x) UPPER(x)
#endif
#define A_CDX DBFCDX
#include "lan.ch"
//#define A_MYSZ
#define A_WAGI
#define A_DDBF
#command INITIALIZE SCREEN =>
#command INITIALIZE  PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define A_PCL
#define A_XPRN
#define isPrinter .t.
#define DTOV(dat) left(dtoc(dat),5)
#define DatE() MEMVAR->dzisiaj
#define A_LPNUM 2
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -43868170018
#define A_KOMU_N  'Ewangelicki Dom Opieki "Ostoja Pokoju"'
#define A_KOMU_A  "Miechowice, ul. Matki Ewy 1"
#define A_AUTOR   "A.D. 1991-2003, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_NOZAP
#define A_DRUKCOMP
