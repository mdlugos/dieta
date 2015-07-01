#include "lan.ch"
#define PC852
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
//#define A_STYLUS
#define A_XPRN
#define A_PCL
#define isPrinter (.t.)
//#define MAG_BIEZ memvar->mag_biez
//#define ZAP_BIEZ memvar->zap_biez
#define A_GOCZ
#define PROC_EN
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT (ferase('druk.prn'),'druk.prn')
#define A_PRINT(x) __run('dosprint druk.prn SCREEN')
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -47485796176
#define A_KOMU_N  'Paästwowy Zakˆad Opiekuäczo-Leczniczy'
#define A_KOMU_A  'Mi©dzybrodzie Bialskie, ul. Graniczna 7'
#define A_AUTOR   'A.D. 2005, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0338522553'
#define A_DIETA   .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_LPNUM 3
#define A_DDBF
#define A_SWW
#define A_WAGI
#define A_BACKUP MEMVAR->backup
