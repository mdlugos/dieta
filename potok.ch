#include "std.ch"
#command INIT SCREEN => __run("font.com")
#command INIT PRINTER => eval({|x|SET(17,.f.),qqout("Px0%1"),set(17,x)},set(17))
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT GERMAN
#define STANY	INDX_MAT
#define A_SUMK    -42436135899
#define A_KOMU_N  "Dom Pomocy Spo’ecznej Nr 5"
#define A_KOMU_A  "Bielsko-Bia’a, ul. Potok 6"
#define A_AUTOR   "A.D. 1995, Marek D’ugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
//#define posilki konta
//#define posilek konto
