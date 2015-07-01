#include "std.ch"
#define PC852
#command INIT SCREEN =>
#command INIT PRINTER => qqout(MEMVAR->p_init)
#define A_STYLUS
#define A_PCL
#define A_XPRN
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
//#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_FIFO
#define A_SUMK    -41691427436
#define A_KOMU_N  "CENTRUM dla Os¢b z Upo˜ledzeniem Umysˆowym WOMS"
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Sobieskiego 158"
#define A_AUTOR   "A.D. 1992-99, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define A_SHORTIND
//#define A_SWW
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "#X##"
#define A_LFIRST
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
#define A_LPNUM 3
//#define A_MULTIDI
#define A_KOMOR
