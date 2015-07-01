#include "std.ch"
#define PC852
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define isPrinter (.t.)
#define A_DRUKCOMP
#define A_XPRN
#define DTOV(dat) right(dtoc(dat),5)
#define A_SET_DAT ANSI
#define STANY     INDX_MAT
#define A_SUMK    -43936806811
#define A_KOMU_N  "DOM OPIEKI ®SAMARYTANIN¯"
#define A_KOMU_A  "Bielsko-Biala, ul. Bednarska 10"
#define A_AUTOR   "A.D. 1994, Marek Dlugosz, Cieszyn, ul. Korfantego 24, tel. (0-386)24048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_LPNUM 3
#define A_WAGI
#define DatE() MEMVAR->dzisiaj
//#define posilki konta
//#define posilek konto
