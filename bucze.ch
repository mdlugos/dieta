#include "lan.ch"
#define A_WAGI
#define A_DDBF
#define PC852
#define A_CDX DBFCDX
#define A_GREX
#define A_POLOWA
#define A_ELZ
#define UpP(x) UPPER(x)
#command INIT SCREEN => //run('uniznak.exe r')
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_DRUKCOMP
#define A_JMALT
#define A_WO_JAD memvar->energia
#define A_ZAP_DAN
#define A_XPRN
#define A_STYLUS
#define A_DILTH 9
//#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_DIETA  .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -37001590688
#define A_KOMU_N  'O˜rodek Leczniczo-Rehabilitacyjny "Bucze" Sp. z o.o.'
#define A_KOMU_A  'G¢rki Wielkie, ul. Harcerska 31'
#define A_AUTOR   "A.D. 2014, Marek Dˆugosz, Cieszyn ul. R¢wna 16, tel. 601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_BACKUP  MEMVAR->bejkap
#define A_LPNUM   2
