#include "lan.ch"
//#define A_GOCZ
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define PC852
#define A_PCL
#define A_XPRN
#define A_STYLUS
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -48712568019
#define A_KOMU_N  "Zgr. C�rek Bo�ej Mi�o�ci Zak�ad Leczniczo-Opieku�czy"
#define A_KOMU_A  "Jastrz�bie Zdr�j, ul. Pszczy�ska 11"
#define A_AUTOR   "A.D. 1992-2008, Marek D�ugosz, Cieszyn, ul. R�wna 16, tel. (0-33)8522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM 2
#define A_DDBF
#define A_WADO eval(MEMVAR->podpisy)
