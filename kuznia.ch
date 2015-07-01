#include "lan.ch"
#define PC852
#define A_CDX DBFCDX
#ifndef __HARBOUR__
#define UpP(x) UPPER(x)
#endif
//#define A_STYLUS
#define A_XPRN
#define A_PCL
#define isPrinter (.t.)
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
//#define MAG_BIEZ memvar->mag_biez
//#define ZAP_BIEZ memvar->zap_biez
//#define A_GOCZ
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -47693770046
#define A_KOMU_N  'Mˆodzie¾owy O˜rodek Wychowawczy'
#define A_KOMU_A  'Ku«nia Raciborska, ul. Klasztorna 1'
#define A_AUTOR   'A.D. 2002, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048'
#define A_DIETA   .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM 3
#define A_DDBF
#define A_SWW
#define A_WAGI
#define PROC_EN memvar->proc_en
//#define A_BACKUP MEMVAR->backup
