#include "lan.ch"
//#define A_GOCZ
//-------------------------
//#define sur_naZ 1
//#define jmaG jm
//#define jedN jm_opcja
//#define indx_maT index
#define A_CDX DBFCDX
//#define A_MYSZ
//-------------------------
//#define A_CENPOS
#define A_GREX
#define A_DILTH 9
#define A_WAGI
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_POLOWA
#define UpP(x) UPPER(x)
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define A_15CPI
//#define A_DEKDUZE
#define PC852
//#define PLWIN
#define A_PCL
#define A_XPRN
#define A_WIN_PRN .f.
//#define A_OKI4W
#define A_STYLUS
#define A_DRUKCOMP
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define DatE() MEMVAR->dzisiaj
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -43181607442
#define A_KOMU_N  'Sanatorium Uzdrowiskowe "MALWA"'
#define A_KOMU_A  "Ustro�, ul. Szpitalna"
#define A_AUTOR   "A.D. 2006, Marek D�ugosz, Cieszyn, ul. R�wna 16, tel. 0338522553"
#define A_DIETA
#define A_FILELIMIT '45'
#define A_NOZAP
#define A_LPNUM 2
#define A_DDBF
#define A_WADO eval(MEMVAR->podpis)
#define A_BACKUP MEMVAR->backup
#define A_MALWA
