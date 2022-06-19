#include "lan.ch"
#define MAG_BIEZ " 6"
#define A_WAGI
#define PROC_EN {{'  4',4,' 81',},{'  5',9,' 82',},{'  9',4,' 83',}}
#define A_WO_JAD '  3'
#define A_DDBF
#define PC852
//#ifdef __PLATFORM__Windows
//#define A_CDX ADS
//#define IndexkeY(x) strtran(indexkey(x),'UPPER(','UPP(')
//#else
#define A_CDX DBFCDX
//#endif
#define UpP(x) UPPER(x)
//#define A_POLOWA
//#define A_DODATKI //iložci posi’k¢w w zapotrzebowaniu i menu
//#define A_GOCZ
#command INIT SCREEN => //run('uniznak.exe r')
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT   eval(MEMVAR->dogetlpt)
//{|_f_|_f_:=getenv('TEMP')+'\',fclose(fcreateu(@_f_)),if(!"."$right(_f_,4),_f_+'.',_f_)})
#define A_PRINT(x) eval(MEMVAR->doprint,x)
#define A_DRUKCOMP
#define A_XPRN
#define A_STYLUS
#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_DIETA  .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_DILTH 4
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -50654140159
#define A_KOMU_N  'SP ZOZ Zakˆad Piel©gnacyjno-Opiekuäczy'
#define A_KOMU_A  'Jaworzno, ul. Zawiszy Czarnego 4'
#define A_AUTOR   "A.D. 2004, Marek Dˆugosz, http://dlugosz.n9.pl tel. 0601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_LPNUM      // 2
#define A_WADO    eval(MEMVAR->podpisy)
