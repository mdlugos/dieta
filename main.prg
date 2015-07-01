field   grupa,posilek,dieta,opis,data,path,nazwa,cena

MEMVAR  grupy,narzuty,diety,posilki,grstr,dietystr,posstr,firma_n,_snorm,level1,stary_rok,defa,cennik,dzisiaj,dietylong,_sbkgr,_sbnorm
#ifdef A_XPRN
memvar apcomp,p_pcl,landscape
#endif
#ifdef A_WIN_PRN
memvar oprn
#endif
#ifdef PLWIN
    #include   'hbgtinfo.ch'
#endif
#ifdef A_ELZ
request descend
#endif
#ifdef A_EXT
request A_EXT
#endif
PROCEDURE MAIN(parametr)

local scr_menu,menu,i,txt

#ifdef PLWIN
   hb_gtInfo( HB_GTI_WINTITLE , "Dieta" )
#endif
#ifdef A_WIN_PRN
   public oprn
#endif
public defa

if parametr='DIETADEF='
   defa:=subs(parametr,10)
   parametr:=NIL
else
   defa:=getenv("DIETADEF")
endif

IF ""#defa .and. right(defa,1)<>HB_OsPathSeparator()
   defa+=HB_OsPathSeparator()
endif

if ""=defa
  defa:="."+HB_OsPathSeparator()
  SET PATH TO (defa)
else
  SET PATH TO ("."+HB_OsPathSeparator() +HB_OsPathListSeparator()+defa)
endif

SETPOS(3,0)

? padc(" €€€€€€ €     €  €€€€€€ €€€€€€€ €€€€€€€ €       €",maxcol()-1)
? padc("€        €   €  €          €    €       €€     €€",maxcol()-1)
? padc(" €€€€€    € €    €€€€€     €    €€€€€€  € €   € €",maxcol()-1)
? padc("      €    €          €    €    €       €  € €  €",maxcol()-1)
? padc("€€€€€€     €    €€€€€€     €    €€€€€€€ €   €   €",maxcol()-1)
? 
? padc("€€€€€‹   €  €€€€€€€ €€€€€€€   €    ",maxcol())
? padc("€    ﬂ€  €  €          €     € €   ",maxcol())
? padc("€     €  €  €€€€€€     €    €‹‹‹€  ",maxcol())
? padc("€    ‹€  €  €          €   €ﬂﬂﬂﬂﬂ€ ",maxcol())
? padc("€€€€€ﬂ   €  €€€€€€€    €  €       €",maxcol())

IF DISKSPACE(if(":"$defa,asc(upper(defa))-64,))<500000
  alarm("TYLKO"+STR(DISKSPACE(),7)+" BAJT‡W WOLNEGO MIEJSCA NA DYSKU !",,3,3)
ENDIF

set default to (defa+"roboczy"+HB_OsPathSeparator())

   STARY_ROK=NIL

#ifdef A_XPRN
   public landscape:=.f.
#ifdef A_PCL
   public p_pcl:=.t.
//#ifdef A_DRUKCOMP
//#endif
#else
   public p_pcl:=.f.
#endif
#endif

#ifdef A_GOCZ
   memvar->p_jadinit:={||firma_n}
#endif

i:={maxrow(),maxcol()}
   txt:="dieta.ini"
   do while inirest(@txt)
   (&txt,txt:=NIL)
   enddo
#ifdef A_XPRN
   p_reset()
   txt:="xprn.ini";do while inirest(@txt);(&txt,txt:=NIL);enddo
#endif

   txt:=set(_SET_DEFAULT)+"daty.ini"
   do while inirest(@txt)
     (&txt,txt:=NIL)
   enddo

txt:=savescreen(0,0,i[1],i[2])
clear screen
restscreen(0,0,i[1],i[2],txt)
   readinit()
   reuse()


*******************                    inicjacja zmiennych

#ifdef DatE
private dzisiaj:=date()
#endif

if !empty(parametr)
    set cursor on
    set color to w
    (&parametr,parametr:=NIL)
    quit
endif

#ifdef DatE
@ 18,0
SAYL "Data dzisiaj: "
readmodal( {_GET_( dzisiaj, "dzisiaj",,,)} )
#endif
//#ifdef A_ELZ
//setkey(-1,{|x,y|y:=setkey(-1,),x:=push_stat(),surowce(),pop_stat(x),setkey(-1,y)})
//#else
setkey(-1,{||magazyn()})
//#endif
setkey(-2,{||relewy->(jad_in(.t.))})
setkey(-3,{|x|x:=setkey(-3,),dania->(rec_in(.t.,.f.)),setkey(-3,x)})
setkey(-4,{|x|x:=setkey(-4,),surowce->(sur_in(.t.,.f.)),setkey(-4,x)})
setkey(-5,{|x|x:=setkey(-5,),aczojs(zaw_ar({},,dania->danie),"",1,,"ZawartoòÜ w: "+dania->nazwa),setkey(-5,x)})
setkey(-6,{|x|x:=setkey(-6,),aczojs(zaw_ar({},,,relewy->(dtos(data)+posilek+dieta)),"",1,,"ZawartoòÜ w posiàku "+relewy->(dtoc(data)+"/"+posilek+trim(dieta))),setkey(-6,x)})

DO WHILE .T.
*************************************


if isCOLOR()
    SET COLOR TO BG+  // TWORZENIE EKRANU
else
    set color to w
endif
          CLEAR screen
      ?? padc(firma_n,maxcol())
      ?
      ? padc("SYSTEM DIETA",maxcol())

if iscolor()
      SET COLOR TO (_sbkgr)
endif

          @ 4,maxcol()/2-28,7,maxcol()/2+28 BOX "…Õª∫ºÕ»∫ "

          @ 4,maxcol()/2-4 SAY " M E N U " COLOR if(isCOLOR(),_sbkgr,"W+")

          @ maxrow()-2,0,maxrow(),maxcol() BOX "…Õª∫ºÕ»∫ "

  IF STARY_ROK#NIL
    @ maxrow()-1,9 say stary_rok+' !' color if(isCOLOR(),"*"+_sbkgr,"W*+")
  else
    @ maxrow()-1,9 say "WYB‡R OPCJI" color if(isCOLOR(),_sbnorm,"W+")
  ENDIF   

          @ maxrow()-1,25 SAY "POMOC  f2 - f7 BEZPóREDNI DOST®P DO DANYCH esc WYJóCIE" color if(isCOLOR(),_sbnorm,"W+")

      SET COLOR TO I

          @ maxrow()-1,2 SAY chr(26)
          @ maxrow()-1,4 SAY chr(27)
          @ maxrow()-1,6 SAY chr(17)+chr(217)
          @ maxrow()-1,22 SAY "F1"
          @ maxrow()-1,32 SAY "F2"
          @ maxrow()-1,37 SAY "F7"
          @ maxrow()-1,68 say "Esc"

      SET MESSAGE TO 5 CENTER

if isCOLOR()
    SET COLOR TO (_sbnorm)
ELSE
    SET COLOR TO W
endif

      @ 6,maxcol()/2-26 PROMPT "Osoby" MESSAGE "Wyswietlanie kartotek osobowych."
      @ 6,maxcol()/2-20 PROMPT "Posiàki" MESSAGE "Posiàki wedàug dat."
      @ 6,maxcol()/2-12 PROMPT "Dania" MESSAGE '"Kartoteka" da‰.'
      @ 6,maxcol()/2-6  PROMPT "Surowce" MESSAGE "Skàadniki da‰ - surowce."
      @ 6,maxcol()/2+2  PROMPT "Elementy" MESSAGE "Makro i mikroelementy - skàadniki surowc¢w."
      @ 6,maxcol()/2+11 PROMPT "Zestawienia" message "Wydruk zestawie‰."
      @ 6,maxcol()/2+23 PROMPT "Inne" message "Programy specjalne, import koszt¢w."

      menu:=level1

      MENU TO menu

      set color to (_snorm)

      IF menu=0

         inisave(set(_SET_DEFAULT)+"daty.ini")
         inisave("dieta.ini")

         IF tak("Czy rzeczywiòcie ko‰czysz prac©",18,,.t.,.F.)
            EXIT
         ENDIF

         loop
      ENDIF

      level1:=menu
      setpos(maxrow()/2,maxcol()/2)
begin sequence
      DO CASE

    CASE level1 = 1
      DO katalog

    CASE level1 = 2
      DO rel_jad

    case level1 =3
      do dan_kat

    case level1 =4
      do surowce

    case level1 =5
      do elementy

    case level1=6
      do zestawienia

    CASE level1 =7
      DO pomocnicze

      ENDCASE
end
#ifdef A_PRINT
    txt:=set(_SET_PRINTFILE,'')
    if ! txt == set(_SET_PRINTFILE) .and. File(txt)
        A_PRINT(txt)
     endif
#endif
    SET PRINTER TO
#ifdef A_WIN_PRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
set console on
set device to screen
      ******************** RESET BAZ
      FOR i = 1 TO 10
          SELECT (i)
          SET FILTER TO
          SET RELATION TO
      NEXT

      UNLOCK ALL

   ENDDO

********************************
IF tak("CZY ARCHIWOWAè",18,,.t.,.F.)
#ifdef A_BACKUP
    __RUN( A_BACKUP )
#else
    ERRORLEVEL(41)
#endif
ENDIF

@ 4,0 clear

SET CURSOR ON

return
*****************
proc readinit
public grupy:={},posilki:={},DIETY:={},PosStr:="",DietyStr:="",GrStr:=""
#ifdef A_XPRN
public apcomp:={}
#endif
#ifdef A_ELZ
#undef A_CENNIK
#endif
#ifdef A_CENNIK
public cennik:={}
#define D_CENNIK ,aadd(cennik,cena)
#else
#define D_CENNIK
#endif
sel('grupy',,,.t.)
#ifdef A_NARZUT
PUBLIC narzuty:={}
dbeval({||aadd(grupy,grupa+" "+opis), GrStr+=grupa, aadd(narzuty,A_NARZUT)},,{||grupa#' '},,,.f.)
#else
dbeval({||aadd(grupy,grupa+" "+opis), GrStr+=grupa },,{||grupa#' '},,,.f.)
#endif
use
sel('posilki',,,.t.)
dbeval({||aadd(posilki,posilek+" "+opis), PosStr+=posilek D_CENNIK},,{||posilek#' '},,,.f.)
use
sel('diety',,,.t.)
#ifdef A_GOCZ
public dietylong:={}
#define D_GOCZ , aadd(dietylong,trim(nazwa))
#else
#define D_GOCZ
#endif
dbeval({||aadd(diety,dieta+" "+opis), dietyStr+=dieta D_GOCZ},,{||dieta#' '},,,.f.)
use
return
*********************************
proc reuse()
close databases
set default to (defa+if(stary_rok#NIL,stary_rok,"roboczy")+HB_OsPathSeparator())
      select 0
      SEL("relewy",1)
      SEL("osoby",2)
      SEL("main",2)
      SEL("dania",2)
      SEL("menu",2)
#ifdef A_DIETA
#define D_DIETA ,.t.
#else
#define D_DIETA
#endif

      SEL("surowce",2 D_DIETA)
#ifndef A_LAN
#ifdef A_DIETA
      flock()
#endif
#endif
      SEL("ZAPOT",2 D_DIETA)
#ifndef A_LAN
#ifdef A_DIETA
      flock()
#endif
#endif
      SEL("sklad",2)
      SEL("elementy",2)
      sel("zawar",2)
#ifdef A_ELZ
      sel("cennik",1)
#endif
#ifdef A_CENPOS
      sel("cenpos",1)
EXTERNAL DESCEND
#endif
return
*****************
func wersja()
#ifdef __DATE__
return 'Wersja: 2.'+ltrim(str(stod(__DATE__)-stod('20100101')))
#else
return 'Wersja: 2.2'
#endif
*************
