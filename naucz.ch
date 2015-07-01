#include "lan.ch"
#define A_MYSZ
#define PC852
#define PLWIN
#command INIT SCREEN =>
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define PROC_EN memvar->proc_en
#define A_15CPI
#define A_15CALI
#define isPrinter() .t.
#define UpP(x) UPPER(x)
#define A_XPRN
#define A_DRUKCOMP
#define DTOV(dat) left(dtoc(dat),5)
#define A_DDBF
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_JMALT
#define A_NOZAP
#define A_SUMK    -43138968948
#define A_KOMU_N  'Dom Pomocy Spoˆecznej - "Dom Nauczyciela"'
#define A_KOMU_A  "Bielsko-Biaˆa, ul. Pocztowa 14a"
#define A_AUTOR   "A.D. 1991-2005, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. (0-33)8522553"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_LPNUM 2
#define A_DILTH 4
#define A_DISUM
#define A_BACKUP (memowrit(defa+'bejkap.bat','lha u -rx dieta '+defa+' *.db? *.ini *.bat *.pp?'+chr(13)+chr(10)+'xcopy /D /I /Y *.lzh %1\'+dtos(date())),errorlevel(41))
//#define PROC_EN
