#include "lan.ch"
#define PC852
#define PLWIN
#command INIT SCREEN => //__run("uniznak m r")
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT   eval(memvar->do_getlpt)
#define A_PRINT(x) eval(memvar->do_print,x)
#define PROC_EN memvar->proc_en
#define UpP(x) UPPER(x)
#define A_DILTH 4
#define A_KONTROLA
#define A_DDBF
#define A_STYLUS
//#define A_15CALI
#define A_LPNUM 3
#define A_15CPI
//#define A_MSCPI
//#define A_GOCZ
#define A_ZAP_DAN
#define A_WO_JAD '  3'
#define A_XPRN
#define A_PCL
#define A_FIFO
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_SUMK    -50604452428
#define A_KOMU_N  "Dom Pomocy Spoˆecznej w Jaworznie"
#define A_KOMU_A  "Jaworzno, ul. Obroäc¢w Poczty Gdaäskiej 63"
#define A_AUTOR   "A.D. 2000, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
//#define A_SHORTIND
#define A_SWW
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "######"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
//#define A_MULTIDI
#define A_SB
//#define A_JMALTTOT(a,sel,x) (sel)->przel
#define A_BACKUP memvar->backup
