#undef A_ELZ
#include "set.ch"

#ifndef ZAP_BIEZ
 #define ZAP_BIEZ "W"
#endif
#ifndef MAG_BIEZ
 #define MAG_BIEZ " 1"
#endif

#command REPLACE wartosc WITH <x> => field2bin('d_wartosc',<x>)
#define WARTOSC bin2d(field->d_wartosc)

#define A_AUTOKOR

memvar getlist,defa,stary_rok,cennik,mies_rob,posilki,oprn
********************************************
PROCEDURE pomocnicze
memvar menupom,_snorm
local m,txt

m=menupom
SET COLOR TO GR+/GR
#ifdef A_XPRN
@ 7,maxcol()/2+15 TO 15,maxcol()/2+33 DOUBLE //BOX '…Õª∫ºÕ»∫ '
#else
@ 7,maxcol()/2+15 TO 14,maxcol()/2+33 DOUBLE //BOX '…Õª∫ºÕ»∫ '
#endif


if !isCOLOR()
   set color to W
endif
@ 17,maxcol()/2-39 TO 19,maxcol()/2+39 DOUBLE //BOX '…Õª∫ºÕ»∫ '

if isCOLOR()
   SET COLOR TO W+/GR
endif
SET MESSAGE TO 18 center
#ifdef A_ELZ
@  8,maxcol()/2+16 PROMPT '1. Wycena.       ' MESSAGE 'Ponowna wycena posiàk¢w po aktualizacji cennika.'
#else
#ifdef A_CENNIK
@  8,maxcol()/2+16 PROMPT '1. Wycena.       ' MESSAGE 'Wycena posiàk¢w wedàug aktualnego cennika.'
#else
@  8,maxcol()/2+16 PROMPT '1. Import koszt¢w' MESSAGE 'Importowanie danych z programu MAGAZYN - koszty dzienne wyæywienia.'
#endif
#endif
@  9,maxcol()/2+16 PROMPT '2. Symbole i kody' MESSAGE 'Przegladanie i porawianie baz danych oraz opis¢w symboli i kod¢w.'
@ 10,maxcol()/2+16 PROMPT '3. Przen.do arch.' MESSAGE 'Przeniesienie zeszàorocznych zapis¢w do archiwum.'
if stary_rok#NIL
@ 11,maxcol()/2+16 PROMPT '4. Wyjòcie z arch' MESSAGE 'Wyjòcie z archiwum.'
else
@ 11,maxcol()/2+16 PROMPT '4. Archiwum.     ' MESSAGE 'Wejòcie do archiwum.'
endif
@ 12,maxcol()/2+16 PROMPT '5. IloòÜ linii.  ' message 'Zmiana iloòci linii widocznych na ekranie.'
@ 13,maxcol()/2+16 PROMPT '6. Ratuj.        ' message 'Reindeksacja baz - Odtwarzanie skorowidzy.'
#ifdef A_XPRN
@ 14,maxcol()/2+16 PROMPT '7. Drukarka.     ' message 'Wyb¢r typu drukarki: Zgodna z EPSON lub HP.'
#endif
SETKEY(4,{||NIL})
SETKEY(19,{||KIBORD(CHR(27)+CHR(5)+CHR(13))})
    MENU TO MENUPOM
SET KEY 19 TO
SET KEY 4 TO
  SETPOS(MENUPOM+7,maxcol()/2+16)

IF menupom=0
   menupom=m
   RETURN
ENDIF

m=menupom

set color to w
@ 20,0 clear

DO CASE
#ifndef A_ELZ
   CASE m=1
        import()
#endif
   CASE m=2
        browse()

   case m=3 .and. stary_rok=NIL
        zamkn()

   CASE m=4


    iniSAVE(set(_SET_DEFAULT)+"daty.ini")

   if stary_rok#NIL
      txt:={"POWR‡T"} //nie wiem, jak g©boko zakopany
   else
      txt:={}
   endif
   if 'STARY'<>stary_rok .and. file(defa+'stary\relewy.dbf')
      aadd(txt,'STARY')
   endif
   aeval(directory(defa+'????','D'),{|x|if(val(x[1])<>0.and.x[1]<>stary_rok.and.file(defa+x[1]+'\relewy.dbf'),aadd(txt,x[1]),)})
   setpos(11,maxcol()/2+25)
   m:=1
   aczojs(txt,"",@m)
   if stary_rok#NIL .and. m=1
      stary_rok:=NIL
   elseif m#0
      STARY_ROK:=txt[m]
   endif
   set default to (defa+if(stary_rok#NIL,stary_rok,"roboczy")+HB_OsPathSeparator())
   readinit()
   reuse()


/*

    iniSAVE(set(_SET_DEFAULT)+"daty.ini")

    close databases

   if ! stary_rok
      if file(defa+"STARY\daty.ini")
         SET DEFAULT TO (defa+"STARY\")              // roboczy katalog
         STARY_ROK=.T.
      endif
   else
      SET DEFAULT TO (defa+"ROBOCZY\")                 // ROBOCZY KATALOG
      STARY_ROK=.F.
   endif
   readinit()
   reuse()

*/

   case m=5
      if isega()
      setpos(11,maxcol()/2+25)
      m:=1
#ifdef PLWIN
    #include   'hbgtinfo.ch'
      aczojs({"98x32","80x25 (Norm)","Zmieniaj iloòÜ linii","Zmieniaj wielkoòÜ czcionki","Peàny ekran (Alt+Enter)"},"",@m)
      do case
      case m<2
        SetMode(32,98)
      case m=2
        SetMode(25,80)
      case m=3
        hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
      case m=4
        hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
      case m=5
        hb_gtInfo( HB_GTI_ISFULLSCREEN, ! hb_gtInfo( HB_GTI_ISFULLSCREEN ) )
      endcase
#else
      aczojs({"80x25 (Norm)","80x30","80x43","80x50","80x60","132x25","132x30","132x43","132x50","132x60"},"",@m)
      sysint(16,3)
      setmode(25,80)
      if m<2
        setmode(25,80)
      elseif m=2
        sysint(16,36)
      elseif m=3
        setmode(43,80)
      elseif m=4
        setmode(50,80)
      elseif m=5
        //sysint(16,38) //80x60
        sysint(16,20226,264)
      elseif m=6
        sysint(16,20226,265) //132x25
        //sysint(16,32)
      elseif m=7
        sysint(16,33) //132x30
      elseif m=8
        //sysint(16,34) //132x43
        sysint(16,20226,266)
      elseif m=9
        sysint(16,20226,267) //132x50
      elseif m=10
        //sysint(16,35) //132x60
        sysint(16,20226,268)
      endif
      setmode(,)
      if maxcol()<79
         sysint(16,3)
         setmode(25,80)
         setmode(25,80)
      endif
#endif
      init screen
#ifdef A_MYSZ
      sysint(51,0)
#endif
       else
          alarm("NIE DOST®PNE NA MONITORZE TYPU HERCULES")
       endif

   case m= 6;ratuj()

#ifdef A_XPRN
   case m = 7
      m:=if(MEMVAR->p_pcl,2,1)
      MEMVAR->p_pcl:=2=ALARM("Podaj rodzaj drukarki:;(zgodna z)",{"EPSON","HEWLETT PACKARD"},m,m)
      p_reset()
#endif
ENDCASE

RETURN
***********
static proc zamkn

field data,kod_osoby,danie,skladnik,element
local i,ye:=year(date()),a,txt

select RELEWY
go top
a:={}
DO while !eof() .and. year(data)<ye
   txt:=STR(year(data),4)
   aadd(a,txt)
   dbseek(txt+'9')
enddo

if empty(a)
      alarm("Nic do roboty.")
      return
endif

setpos(11,maxcol()/2+25)
? 'Kt¢ry rok przenosimy? '
i:=1
aczojs(a,"",@i)
if i=0
   Return
endif
ye:=a[i]

if file(defa+ye+"\relewy.dbf")
   Alarm('Ten rok juæ jest w archiwum.')
   return
endif

begin sequence

   altd()
   ?
   ? "Tworzenie kartoteki STARY."
   mkdir(defa+ye)
   ?
   ? "Tworzenie zbioru DATY.INI ."
   copy file (set(_SET_DEFAULT)+"daty.ini") to (defa+ye+"\daty.ini")
   /*
   ?
   ? "Tworzenie pustych baz w kartotece STARY: "
   for i:=1 to 10
       select (i)
       txt:=alias()
       ?? txt+", "
       a:=dbstruct()
       use
       dbcreate(defa+ye+"\"+txt,a)
   next
      *
      STARY_ROK:=ye
      reuse()
   */
        ?
        ? "Dopisywanie do archiwum z bazy RELEWY."
        select relewy
        copy to (defa+ye+'\relewy') for dtos(data)<=ye
        //append from (defa+'roboczy\relewy') for dtos(data)<=ye

        ?
        ? "Dopisywanie do archiwum z bazy MAIN."
        select main
        copy to (defa+ye+'\main') for dtos(data)<=ye
        //append from (defa+'roboczy\main') for dtos(data)<=ye

        ?
        ? "Dopisywanie do archiwum z bazy MENU."
        select menu
        copy to (defa+ye+'\menu') for dtos(data)<=ye
        //append from (defa+'roboczy\menu') for dtos(data)<=ye

        ?
        ? "Dopisywanie do archiwum z bazy ZAPOT."
        select zapot
        copy to (defa+ye+'\zapot') for dtos(data)<=ye
        //append from (defa+'roboczy\zapot') for dtos(data)<=ye

        ?
        ? "Kopiowanie do archiwum bazy OSOBY."

        select main
        set order to tag main_kod
        select osoby
        copy to (defa+ye+'\osoby') for {||main->(dbseek(osoby->kod_osoby).and.dtos(data)<=ye)}
        /*
        select main
        set order to tag main_kod
        select osoby
        append from (defa+'roboczy\osoby') for {|x|x:=kod_osoby,main->(dbseek(x))}
        */

        ?
        ? "Kopiowanie do archiwum bazy DANIA."

        select dania
        copy to (defa+ye+'\dania')
        /*
        select menu
        set order to tag menu_dan
        select dania
        append from (defa+'roboczy\dania') for {|x|x:=danie,menu->(dbseek(x))}
        */

        ?
        ? "Kopiowanie do archiwum bazy SKLAD."

        set order to tag dan_kod
        select sklad
        copy to (defa+ye+'\sklad') for {||dania->(dbseek(sklad->danie))}
        /*
        set order to tag dan_kod
        select sklad
        append from (defa+'roboczy\sklad') for {|x|x:=danie,dania->(dbseek(x))}
        */

        ?
        ? "Kopiowanie do archiwum bazy SUROWCE."

        set order to tag skl_skl
        select zapot
        set order to tag zap_skl
        select zawar
        set order to tag zaw_skl
        select surowce
        copy to (defa+ye+'\surowce') for {|x|x:=skladnik,zapot->(dbseek(x).and.dtos(data)<=ye).or.sklad->(dbseek(x)).or.zawar->(dbseek(x))}
        //append from (defa+'roboczy\surowce') for {|x|x:=skladnik,zapot->(dbseek(x)).or.sklad->(dbseek(x))}

        ?
        ? "Kopiowanie do archiwum bazy ZAWAR."
        set order to tag sur_kod
        select zawar
        set order to tag zaw_ele
        copy to (defa+ye+'\zawar') for {|x|x:=skladnik,surowce->(dbseek(x))}
        //append from (defa+'roboczy\zawar') for {|x|x:=skladnik,surowce->(dbseek(x))}

        ?
        ? "Kopiowanie do archiwum bazy ELEMENTY."
        //set order to tag zaw_ele
        select elementy
        copy to (defa+ye+'\elementy') for {|x|x:=element,zawar->(dbseek(x))}
        //append from (defa+'roboczy\elementy') for {|x|x:=element,zawar->(dbseek(x))}

        //stary_rok:=NIL
        //set default to (defa+'roboczy\')
        close databases

#ifndef A_CDX
        ?
        ? "Kasowanie zbior¢w indeksowych w kartotece ROBOCZY."

        AEVAL(DIRECTORY(set(_SET_DEFAULT)+"*.??X"),{|X|qout(x[1]),FERASE(set(_SET_DEFAULT)+X[1])})
#endif
        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy RELEWY."
        USE relewy EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for dtos(data)<=ye
        pack

        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy MAIN."
        USE MAIN EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for dtos(data)<=ye
        pack

        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy MENU."
        use menu EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for dtos(data)<=ye
        pack

        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy ZAPOT."
        use ZAPOT EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for dtos(data)<=ye
        pack

        close databases

        ratuj()


/*
field kod_osoby,danie,skladnik,element,data

local i,ye:=year(date()),a,txt

if file(defa+"STARY\relewy.dbf")
   use (defa+"stary\relewy") new alias stary readonly
   go bottom
   if year(data)=ye
      alarm("Rocznik "+str(ye,4)+" juæ jest w archiwum.")
      use
      return
   endif
endif
begin sequence

if !file(defa+"stary\daty.ini")
   ?
   ? "Tworzenie kartoteki STARY."
   mkdir(defa+"stary")
   ?
   ? "Tworzenie zbioru DATY.INI ."
   copy file (defa+"roboczy\daty.ini") to (defa+"stary\daty.ini")
   ?
   ? "Tworzenie pustych baz w kartotece STARY: "
   for i:=1 to 10
       select (i)
       txt:=alias()
       ?? txt+", "
       a:=dbstruct()
       use
       dbcreate(defa+"stary\"+txt,a)
   next
endif


    close databases
        ?
        ? "Dopisywanie do archiwum z bazy RELEWY."
        USE (defa+"stary\relewy") EXCLUSIVE
        append from relewy for year(data)<ye

        ?
        ? "Dopisywanie do archiwum z bazy MAIN."
        USE (defa+"stary\main") EXCLUSIVE
        append from main for year(data)<ye

        ?
        ? "Dopisywanie do archiwum z bazy MENU."
        use (defa+"stary\menu") EXCLUSIVE
        append from menu for year(data)<ye

        ?
        ? "Dopisywanie do archiwum z bazy ZAPOT."
        use (defa+"stary\zapot") EXCLUSIVE
        append from zapot for year(data)<ye

        ?
        ? "Kopiowanie do archiwum bazy OSOBY."
        USE osoby alias roboczy readonly //EXCLUSIVE
        set order to tag osob_kod

        use (defa+"stary\osoby") new EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        set relation to kod_osoby into roboczy
        delete for !roboczy->(eof())
        set relation to
        roboczy->(dbclosearea())
        pack
        append from osoby

        ?
        ? "Kopiowanie do archiwum bazy DANIA."
        USE dania alias roboczy readonly //EXCLUSIVE
        set order to tag dan_kod

        use (defa+"stary\dania") new EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        set relation to danie into roboczy
        delete for !roboczy->(eof())
        set relation to
        pack

        ?
        ? "Kopiowanie do archiwum bazy SKLAD."

        index on danie to (defa+"stary\dan_kod")

        select roboczy
        use (defa+"stary\sklad") EXCLUSIVE
        set relation to danie into dania
        delete for dania->(eof())
        set relation to
#ifdef A_CDX
        ordlistclear()
#endif
        pack
        append from sklad
        use
        select dania

        ordlistclear()

        append from dania

        ?
        ? "Kopiowanie do archiwum bazy SUROWCE."
        USE surowce alias roboczy readonly // EXCLUSIVE
        set order to tag sur_kod

        use (defa+"stary\surowce") new EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        set relation to skladnik into roboczy
        delete for !roboczy->(eof())
        set relation to
        pack

        ?
        ? "Kopiowanie do archiwum bazy ZAWAR."

        index on skladnik to (defa+"stary\sur_kod")

        select roboczy
        use (defa+"stary\zawar") EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        set relation to skladnik into surowce
        delete for surowce->(eof())
        set relation to
        pack
        append from zawar
        use
        select surowce
        ordlistclear()

        append from surowce

        ?
        ? "Kopiowanie do archiwum bazy ELEMENTY."
        USE elementy alias roboczy readonly //EXCLUSIVE
        set order to tag ele_kod

        use (defa+"stary\elementy") new EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        set relation to element into roboczy
        delete for !roboczy->(eof())
        set relation to
        roboczy->(dbclosearea())
        pack
        append from elementy

        close databases

        ?
        ? "Kasowanie zbior¢w indeksowych w kartotece STARY."
        AEVAL(DIRECTORY(defa+"STARY\*.??X"),{|X|qout(x[1]),FERASE(defa+"STARY\"+X[1])})

        //set defa to (defa+"roboczy\")

#ifndef A_CDX
        ?
        ? "Kasowanie zbior¢w indeksowych w kartotece ROBOCZY."

        AEVAL(DIRECTORY(defa+"ROBOCZY\*.??X"),{|X|qout(x[1]),FERASE(defa+"ROBOCZY\"+X[1])})
#endif
        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy RELEWY."
        USE relewy EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for year(data)<ye
        pack

        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy MAIN."
        USE MAIN EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for year(data)<ye
        pack

        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy MENU."
        use menu EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for year(data)<ye
        pack

        ?
        ? "Kasowanie zeszàorocznych zapis¢w z bazy ZAPOT."
        use ZAPOT EXCLUSIVE
#ifdef A_CDX
        ordlistclear()
#endif
        delete for year(data)<ye
        pack

        close databases

        ratuj()
        //reuse()
*/

?
wait "Gotowe, naciònij klawisz ..."
recover
//set defa to (defa+"roboczy\")
ratuj()
?
set color to *w
wait "PRZERWANO DZIAùANIE PROGRAMU ..."
set color to w
?
reuse()
end sequence

return
****************
procedure import(od,do)
#ifdef A_ELZ
field d_wartosc,ile_pos,data,posilek,dieta,cena,skladnik,ilosc
local d,e,f,g,chg,x
@ 21,10 say "Wycena od dnia:" get mies_rob picture "@D"
read
if readkey()=27
   break
endif
select cennik
set order to 1
select zapot
set order to tag zap_rel
select relewy
//lock all
seek dtos(mies_rob)
do while !eof()
     setpos(23,0)
     ?? DATA,POSILEK,ILE_POS
     d:={}
     e:={}
     chg:=dtos(data)+posilek
     select zapot
     seek chg
     exec {|a|cennik->(dbseek(zapot->skladnik+left(chg,8)),if(skladnik=zapot->skladnik,,dbgoto(lastrec()+1))),a:=ascan(d,dieta),if(a=0,(aadd(d,dieta),aadd(e,ilosc*cennik->cena)),e[a]+=ilosc*cennik->cena)} rest while dtos(data)+posilek=chg
     select relewy

#ifdef A_LAN
     #define D_LAN .and. reclock()
#else
     #define D_LAN
#endif

     //seek chg  //po co?
     exec {|a|a:=ascan(d,dieta),f:=if(a=0,0,e[a]),g:=left(dieta,1),if(g<'0',if(a=0,,e[a]:=e[a]/ile_pos),aeval(d,{|x,i|if(x<'0'.and.dind(g,x),f+=ile_pos*e[i],)})),relewy->(field2bin('d_wartosc',f))} rest while dtos(data)+posilek=chg D_LAN
enddo
#else
#ifdef A_CENNIK
field d_wartosc,ile_pos,data,posilek,dieta
setpos(10,0)
aeval(cennik,{|x,i|qout(posilki[i],x)})
@ 21,10 say "Wycena od dnia:" get mies_rob picture "@D"
read
if readkey()=27
   break
endif
select relewy
//lock all
*
#ifdef A_LAN
     #define D_LAN reclock()
#else
     #define D_LAN
#endif
*
seek dtos(mies_rob)
EXECUTE {|c|c:=cennik[ascan(posilki,posilek)],D_LAN,relewy->(field2bin('d_wartosc',ile_pos*c)) ,setpos(23,0),qqout(DATA,posilek,dieta,STRPIC(ile_pos*c,12,A_ZAOKR,"@E "),ILE_POS,STRPIC(c,7,A_ZAOKR,"@E ",.t.))} REST
#else

LOCAL txt,carry,carry_key,carry_di,carry_1,carry_di_1,i,di,ko,da,w,bir,bit,x,tot_rec,tot_w
memvar mies_rob,posilki,diety
field data,nr_zlec,posilek,ile_pos,dieta,nr_mag,pozycja,wartosc,wart_tot,d_wartosc,smb_dow,nr_dowodu,grupa

SELECT 0
di:=getenv("MAGDEF")
if empty(di)
  x:=rat(HB_OsPathSeparator(),left(defa,len(defa)-1))
  di:=if(x=0,defa+'..'+HB_OsPathSeparator(),left(defa,x))+'magazyn'+HB_OsPathSeparator()
endif
di+="roboczy"+HB_OsPathSeparator()

begin sequence
#ifdef A_DDBF
#define zmienna1 daty->d_z_mies1
#define zmienna2 daty->d_z_mies2
#define zmienna3 daty->d_z_rok
use (di+"DATY") readonly shared
#else
   txt:=asize(getlines(memoread(di+"daty.ini")),3)
   for i:=1 to 3
     txt[i]:=&(subs(txt[i],at(":=",txt[i])+2))
   next i
#define zmienna1 txt[1]
#define zmienna2 txt[2]
#define zmienna3 txt[3]
#endif
if pcount()<2
  od:=zmienna2+1

  @ 21,10 say "Import od dnia:" get od picture "@D" valid (od<zmienna1 .or. alarm("Dane nie pewne !;nie zamkni©to okresu od "+dtoc(zmienna1+1)+" w magazynie",,,3)#NIL)
  read
  if readkey()=27
     break
  endif
endif
if od<=zmienna3
   di:=left(di,len(di)-8)+str(year(od),4)+HB_OsPathSeparator()
#ifdef A_DDBF
use (di+"DATY") readonly shared
#else
   txt:=asize(getlines(memoread(di+"daty.ini")),3)
   for i:=1 to 3
     txt[i]:=&(subs(txt[i],at(":=",txt[i])+2))
   next i
#endif
endif
if pcount()<2
  do:=if(zmienna1>od,zmienna1,od)
  @ 21,45 say "do dnia:" get do picture "@D" valid (do>=od .or. alarm('Za wczeònie !; - "od" p¢´niejsze  -',,,3)=NIL) .and. (year(do)=year(od).or. alarm("Pojedynczy import musi si© zawieraÜ w jednym roku !",,,3)=NIL) .and. (do<=zmienna1 .or. alarm("Dane nie pewne !;nie zamkni©to okresu od "+dtoc(zmienna1+1)+" w magazynie",,,3)#NIL)
  read
  if readkey()=27
     break
  endif
endif
mies_rob:=min(zmienna1+1,do)
USE (di+"MAIN") READONLY ALIAS MAGAZYN SHARED
if pcount()<2
@ 23,0 say "Prosz© chwil© poczekaÜ "
copy fields data,nr_zlec,wartosc TO TMP FOR (message(1000),.t.) .and. left(nr_zlec,1)$ZAP_BIEZ .and. nr_mag=MAG_BIEZ .and. DATA>=od .and. data<=do .and. wartosc#0
else
copy fields data,nr_zlec,wartosc TO TMP FOR left(nr_zlec,1)$ZAP_BIEZ .and. nr_mag=MAG_BIEZ .and. DATA>=od .and. data<=do .and. wartosc#0
endif
use
recover
#ifdef A_DDBF
select (SELECT("DATY"))
use
#endif
select (SELECT("MAGAZYN"))
use
return
end sequence
#ifdef A_DDBF
select (SELECT("DATY"))
use
#endif
if pcount()<2
alarm("Przygotuj drukark©;(na wszelki wypadek)",,,3)
if lastkey()=27
   use
   return
endif
devout(".","*W")
setpos(23,0)
endif
#ifdef A_WIN_PRN
  if A_WIN_PRN
    oprn:=.f.
    set printer to import.prn
  endif
#endif
select relewy
//lock all
#ifdef A_AUTOKOR
  select main
  set order to tag main_rel
  seek dtos(od)
#ifdef A_GREX
  exec {|x|relewy->(if(!dbseek((x:=dseek(,'data,posilek,dieta',main->data,main->posilek,''))+' '),(dbappend(),data:=main->data,posilek:=main->posilek),),if(!dbseek(x+main->dieta+'/'+main->grupa+' '),(dbappend(),data:=main->data,posilek:=main->posilek,dieta:=main->dieta+'/'+main->grupa),))} WHILE data<=do
#else
  exec {|x|relewy->(if(!dbseek((x:=dseek(,'data,posilek,dieta',main->data,main->posilek,''))+' '),(dbappend(),data:=main->data,posilek:=main->posilek),),if(!dbseek(x+main->dieta+' '),(dbappend(),data:=main->data,posilek:=main->posilek,dieta:=main->dieta),))} WHILE data<=do
#endif
  select relewy
#endif

seek dtos(od)
w:=IndexkeY()
bir:={||&w}
w:=strtran(strtran(UpP(w),'POSILEK','SUBS(NR_ZLEC,2,1)'),'DIETA','SUBS(NR_ZLEC,3)')
bit:={||&w}

USE TMP NEW EXCLUSIVE
carry_key:="";tot_rec:=tot_w:=0
BEGIN SEQUENCE
ordcreate('TMP','TMP',w,bit)
DO WHILE relewy->( data<=do .and. !eof() ) .or. data<=do .and. !eof()
  TXT:=trim(DTOS(DATA)+subs(nr_zlec,2))
  da:=data
  ko:=subs(nr_zlec,2,1)
  di:=trim(subs(nr_zlec,3))
  IF !eof() .and. ( RELEWY->(eval(bir))>eval(bit) .or. relewy->(eof()) )
    if ascan(posilki,ko)=0 .or. !dival(,di)
//if pcount()<2
      print(1)
      ?? DATA,NR_ZLEC
      ?? " NIEPRAWIDùOWY KOD ROZCHODU, PROSZ® POPRAWIè W MAGAZYNIE !!!"
      ?
      set print off
/*else
      alarm("NIEPRAWIDùOWY KOD, PROSZ® POPRAWIè: "+dtoc(data)+' '+nr_zlec)
endif*/
      w:=eval(bit)
      seek left(w,len(w)-1)+chr(asc(right(w,1))+1)
      loop
    endif
    if carry_key#dtos(da)+ko
       relewy->(dbappend())
       if di>" "
          relewy->data:=da
          relewy->posilek:=ko
          relewy->(dbappend(.f.))
       endif
    else
       relewy->(dbappend(.f.))
    endif
    relewy->data:=da
    relewy->posilek:=ko
    relewy->dieta:=di

    if di>" "
       select relewy
       i:=0
       aeval(mkfxar(da,ko),{|x|if(dind(x[1],di),i+=x[3],)})
       ile_pos:=i
       select tmp
    endif

  ELSE
     da:=relewy->data
     ko:=relewy->posilek
     di:=trim(relewy->dieta)
#ifdef A_LAN
    IF carry_key#dtos(da)+ko
       LOCK IN RELEWY
    ELSE
       LOCK RELEWY->(RECNO()) IN RELEWY
    ENDIF
#endif
#ifndef A_GREX
    IF '/'$di
      relewy->(dbdelete())
      relewy->(dbskip())
      loop
    ENDIF
#endif
  ENDIF
  w:=0
  IF trim(RELEWY->(dtos(data)+posilek+dieta))==txt  //dtos(data)+subs(nr_zlec,2)
     exec {||w-=wartosc} WHILE TRIM(DTOS(DATA)+SUBS(NR_ZLEC,2))==TXT REST
     w:=ROUND(w,2)
  ENDIF
#ifdef A_AUTOKOR
  select main
  dbseek(dtos(da)+ko)
#ifdef A_GREX
  sum ile_pos to i rest for dind(dieta+'/'+grupa,di) while data=da .and. posilek=ko
#else
  sum ile_pos to i rest for dind(dieta,di) while data=da .and. posilek=ko
#endif
  select relewy
  IF ILE_POS#I
     setpos(row(),0)
     //PRINT(1)
     ?? DA,ko,di,STRPIC(WARTOSC,12,A_ZAOKR,"@E "),ile_pos
     ?? " POPRAWA ILOóCI POSIùK‡W NA",i,"!!!"
     //? ""
     ?
     //set print off
     ile_pos:=i
  endif
#else
  SELECT RELEWY
  i:=ile_pos
#endif

  WART_TOT:=w

  IF ILE_POS=0 .AND. WART_TOT#0
     PRINT(1)
     ?? DAta,posilek,dieta,STRPIC(WART_TOT,12,A_ZAOKR,"@E "),Ile_pos
     ?? " NIKT NIE JADù !!!"
     ?
  ELSEif pcount()<2
     ?? DA,ko,di,STRPIC(W,12,A_ZAOKR,"@E "),I,STRPIC(W/I,7,A_ZAOKR,"@E ",.t.)
     setpos(row(),0)
  ENDIF

  if carry_key#dtos(da)+ko
     carry_di:={}
     carry:={}
     carry_key:=dtos(da)+ko
     tot_rec:=recno()
  endif

  evfxar(carry,carry_di)

  skip
  do while dtos(data)+posilek+dieta+' '=dtos(da)+ko+di+' '
     lock recno()
     delete
     PRINT(1)
     ?? DATA,posilek,dieta,STRPIC(WARTOSC,12,A_ZAOKR,"@E "),ILE_POS
     ?? " ZDUBLOWANY - WYKASOWAùEM !!!"
     ?
     skip
  enddo
  if carry_key#dtos(data)+posilek .and. carry_key#dtos(TMP->data)+subs(TMP->nr_zlec,2,1)
     aeval(carry_di,{|b|i:=b,aeval(carry,{|a|if(dind(i[1],a[1]),i[2]+=i[3]*a[2]/a[3],)})})
     i:=recno()
     goto tot_rec
     WHILE carry_key=dtos(data)+posilek
       w:=0
       aeval(carry_di,{|x|if(dind(x[1],dieta),w+=x[2],)})

       REPLACE wartosc WITH w
#ifdef A_GREX
       IF ile_pos#0 .AND. W=0 .and. dieta>='0' .and. subs(dieta,2)='/' .and. subs(dieta,3)>='0' .and. empty(subs(dieta,4))
#else
       IF ile_pos#0 .AND. W=0 .and. dieta>='0' .and. empty(subs(dieta,2))
#endif
         PRINT(1)
         ?? DAta,posilek,dieta,STRPIC(W,12,A_ZAOKR,"@E "),Ile_pos
         ?? " DARMOWY !!!"
         ?
       endif
       SKIP
     END
     UNLOCK
     goto i
  endif
  SET PRINT OFF
  SELECT TMP
ENDDO
END
  SET PRINT OFF
#ifdef A_WIN_PRN
  if oprn:=A_WIN_PRN
     x:=getlines(memoread(set(_SET_PRINTFILE,'')))
     if !empty(x)
       set printer to
       Print(1)
       aeval(x,{|y|wqq(y),wq()})
       oprn:destroy()
       oprn:=NIL
     endif
  endif
#endif
#ifdef A_PRINT
  x:=set(_SET_PRINTFILE,'')
  if ! x==set(_SET_PRINTFILE) .and. File(x)
      A_PRINT(x)
  endif
#endif
  set print to
SELECT TMP
USE
ferase(set(_SET_DEFAULT)+"tmp.dbf")
ferase(set(_SET_DEFAULT)+"tmp"+ordbagext())
#endif
#endif
unlock in RELEWY
if pcount()<2
wait "Koniec, naciònij klawisz ..."
endif

return
**************
