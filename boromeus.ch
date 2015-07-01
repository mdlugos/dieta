#include "lan.ch"
#define A_CDX DBFCDX
#define PROC_EN memvar->proc_en
#define A_BACKUP MEMVAR->backup
#define A_DILTH 4
#define A_WO_JAD 3
#define A_WAGI
#define PLWIN
#define A_WIN_PRN memvar->p_win
#define PC852
#define UpP(x) UPPER(x)
#define A_DDBF
#command INIT SCREEN => //__run("font852")
#define A_PCL
#define A_XPRN
#define A_15CPI
//#define A_OKI4W
#define A_STULUS
#define A_DRUKCOMP
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
//#command INIT PRINTER => qqout("(174U(s1q4099t0p0s0b12v10H")
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_SET_DAT GERMAN
#define DatE() MEMVAR->dzisiaj
#define isPrinter .t.
#define STANY INDX_MAT
#define A_SUMK    -43293847990
#define A_KOMU_N  "Zak. Opiek. - Leczn. S.S.Boromeuszek"
#define A_KOMU_A  "Cieszyn, ul. G¢rny Rynek 6"
#define A_AUTOR   "A.D. 1994, Marek D’ugosz, Cieszyn, ul. Korfantego 24, tel. 524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_MYSZ
#define A_WADO memvar->podpis
#define A_LPNUM 2
