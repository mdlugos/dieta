#include "lan.ch"
#define A_EXT chgmaz
#define PC852
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
//#define A_STYLUS
#define A_XPRN
#define A_PCL
#define PROC_EN
#define isPrinter (.t.)
#define A_15CPI
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
#define A_SUMK    -43758561941
#define A_KOMU_N  'Korona Sanatorium Uzdrowiskowe'
#define A_KOMU_A  'Muszyna, ul. M�ciwujewskiego 2'
#define A_AUTOR   'A.D. 2006, Marek D�ugosz, Cieszyn, ul. R�wna 16, tel. 0-338522553'
#define A_DIETA   .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM
#define A_DDBF
#define A_SWW
#define A_WAGI
#define A_BACKUP MEMVAR->backup
#define A_WADO eval(MEMVAR->podpisy)
