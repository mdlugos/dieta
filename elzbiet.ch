#include "lan.ch"
#define UpP(x) UPPER(x)
#define A_CDX DBFMDX
#define PC852
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
//#command INIT PRINTER => qqout("(s1q0p0s0b12v10H")
#define DatE()    MEMVAR->dzisiaj
#define isPrinter .t.
#define A_XPRN
#define A_DRUKCOMP
#define A_LPNUM 3
#define A_PCL
//#define A_OKI4W
#define A_ELZ
#define DTOV(dat) right(dtoc(dat),5)
#define A_SET_DAT GERMAN
#define STANY INDX_MAT
#define A_SUMK    -46728976243
#define A_KOMU_N  'Dom Pomocy Spoˆecznej "Betania"'
#define A_KOMU_A  "Cieszyn, ul. Katowicka 1"
#define A_AUTOR   "A.D. 1999, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
//#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '50'
#define A_MYSZ
