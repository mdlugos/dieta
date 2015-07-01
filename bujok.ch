#include "std.ch"
#define PC852
#define A_XPRN
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
//"&l3A(17U(s10h12V")
#define A_PCL
#define isPrinter() .t.
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
//#define A_NOZAP
#define A_SUMK    -36795225246
#define A_KOMU_N  "DOM POMOCY SPOECZNEJ"
#define A_KOMU_A  "Cieszyn, ul. Korfantego 1"
#define A_AUTOR   "A.D. 1994, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 524048"
#define DatE()    MEMVAR->dzisiaj
