Field kod_osoby,nazwisko,stanowisko,ile_pos,grupa,d_wartosc,wart_tot,posilek,data,dieta,kto_pisal,pozycja

#include "dm_form.ch"

#command REPLACE wartosc WITH <x> => field2bin('d_wartosc',<x>)
#define WARTOSC bin2d(field->d_wartosc)

MEMVAR CHANGED,diety,posilki,posstr,grupy,mies_rob,choicee,choicef,choiced,choiceg,choicew
static poprec,oldrec,keyp,startrec,kos,il,di,gr
**********************
proc rel_jad

select main
set order to tag main_rel
select menu
set order to tag menu_rel
select zapot
set order to tag zap_rel
select surowce
set order to tag sur_kod
select dania
set order to tag dan_kod
select osoby
set order to tag osob_kod
#ifdef A_CENPOS
select cenpos
set order to 1
select relewy
//set relation to posilek+dtos(data) into cenpos
   #define D_CENA (CENPOS->(dbseek(RELEWY->posilek+dtos(RELEWY->data),.t.)),cenpos->cena)
#else
   #define D_CENA RELEWY->(WARTOSC/ILE_POS)
#endif

SELECT RELEWY
seek dseek(,'data,posilek,dieta',data,posilek,'')

#ifdef A_POLOWA
  #define D_ILPIC "@KE ###.#"
#else
  #define D_ILPIC "@K #####"
#endif

szukam({0,10,maxrow(),,1,4,"Data���"+padc("Dieta�F8",A_DILTH,"�")+"����Ilo���Cena",;
{||tran(Dtos(data)+posilek+dieta,"@R ####.##.##|X|"+repl("X",A_DILTH))+"�"+;
if(menu->(dbseek(relewy->(dtos(data)+posilek))),"J"," ")+;
if(zapot->(dbseek(relewy->(dtos(data)+posilek))),"Z"," ")+"�"+;
tran(ILE_POS,D_ILPIC)+"�"+strpic(D_CENA,7,A_ZAOKR,"@E ",.t.)},;
{|k,s|RELe(k,s,.t.)},LEFT(DTOS(DATA),4)})

return
*********************
FUNCTION RELe(_skey,_s,upden)
field DATA,posilek
local getlist,a,b,c,d,scr

   DO CASE 
      CASE _SKEY=0
    _snagkol:=6
    _spform:={|p,l|tranR(RIGHT(p,l),"####.##.##|X|"+repl("X",A_DILTH))}
    if alias()="RELEWY"
       _sfor:={||dieta=" "}
       if [at(] $ lower(INDEXKEY(0))
          _sp2s:={|x|if(len(x)<=8,x,dseek(,'data,posilek,dieta',stod(left(x,8)),subs(x,9,1),subs(x,10)))}
          _ss2p:={|x|left(x,8)+if(len(x)>8,subs(posstr,asc(subs(x,9))%16,1)+subs(x,10),'')}
          _spform:={|p,l|tranr(eval(_ss2p,p,l),"####.##.##|X|"+repl("X",A_DILTH))}
       endif
    elseif ordsetfocus()="MAIN_KOD"
       if [at(] $ lower(INDEXKEY(0))
          _sp2s:={|x|if(len(x)<=11,x,dseek(,'kod_osoby,data,posilek,dieta',left(x,3),stod(subs(x,4,8)),subs(x,12,1),subs(x,13)))}
          _ss2p:={|x|left(x,11)+if(len(x)>11,subs(posstr,asc(subs(x,12))%16,1)+subs(x,13),'')}
          _spform:={|p,l|tranr(eval(_ss2p,p,l),"XXX|####.##.##|X")}
       else
          _spform:={|p|tranR(p,"XXX|####.##.##|X")}
       endif
    else
       if [at(] $ lower(INDEXKEY(0))
          _sp2s:={|y,l,x,n|x:=right(y,l),n:=len(y)-l,y:=left(y,n),y+if(len(x)<=8,x,subs(dseek(,'data,posilek,dieta',stod(left(x,8)),subs(x,9,1),subs(x,10)),n+1))}
          _ss2p:={|y,l,x,n|x:=right(y,l),n:=len(y)-l,y:=left(y,n),y+left(x,8)+if(len(x)>8,subs(posstr,asc(subs(x,9))%16,1)+subs(x,10),'')}
          _spform:={|p,l|tranr(right(eval(_ss2p,p,l),l),"####.##.##|X|"+repl("X",A_DILTH))}
       endif
    endif
    _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
    if ! ( (eval(_swar,_spocz).or.dbseek(_spocz)).and._skip(0,,_s) )
      _spocz=LEFT(_spocz,len(_spocz)-_slth)
      _slth=0
      _sef:=.f.
      if !eval(_swar,_spocz)
         _skip(-1,,_s)
         if !eval(_swar,_spocz)
            seek _spocz
         endif
      endif
    endif
    set cursor on

  case _skey=27
       return .t.

  CASE (_SKEY=13 .or. _skey=22) .and. 0<(_skey:=alarm("WYBIERZ CO�:",{"KTO JAD�","JAD�OSPIS","ZAPOTRZEBOWANIE"},choicee))

    private changed:=.f.
    choicee:=_skey
    IF choicee=1
      _skey:=DOK_IN(!upden)
    ELSEIF choicee=2
      _skey:=JAD_IN(!upden)
    ELSE
      _skey:=zap_IN(!upden)
    ENDIF
    if changed
       if upden
          if alias()="RELEWY"
             goto _skey
          endif
          if !eval(_swar,_spocz)
             _spocz:=left(_spocz,len(_spocz)-_slth)
             _slth:=0
          endif
          refresh(,_s)
       else
          go _srec[_sm]
          REFRESH LINE _srow1-1+_sm DIRECTION 0
       ENDIF
    endif
    SET CURSOR ON

   case _skey=-8
    _sfil(_s)

   case _si=0

   CASE _SKEY=9
        _skey:=message("CO DRUKOWA�:        Kopii:;(J - Jad�ospis, O - Osoby, Z - Zapotrzebowanie);dla diety:;dla grupy:")
        getlist:={}
        if type('choicef')='U'
           choicef:='JOZ'
        endif
        if type('choiced')='U'
           choiced:=RELEWY->dieta
           choiceg:=' '
        endif
        a:=if(choicef='J  ',choiced,space(len(relewy->dieta)))
        b:=if(choicef='J  ',choiceg,' ')
        c:=1
        d:=choicew
        Getlist:={}
        @ _skey[1]+1,_skey[2]+15 get choicef picture "@! NNN" valid {||a:=if(choicef='J  ',choiced,space(len(relewy->dieta))),b:=if(choicef='J  ',choiceg,' '),getlist[2]:display(),getlist[3]:display(),.t.}
        @ _skey[1]+1,_skey[2]+30 get c picture "#"  //when choicef='J  '
        @ _skey[1]+3,_skey[2]+14 get a picture "@!KS"+LTRIM(STR(A_DILTH)) valid {|r|r:=dival(,@a),if(r,choiced:=a,),r} when choicef='J  '
#ifdef A_GOCZ
#ifndef A_WO_JAD
 #define A_WO_JAD '  3'
#endif
#endif
#ifdef A_WO_JAD
        if valtype(d)='L'
          sayl 'Warto�� od�ywcza:' get d PICTURE "L" valid {|r,x|choicew:=x,.t.} when choicef='J  '
        else
          sayl 'Warto�� od�ywcza:' get d PICTURE "##" valid {|r,x|choicew:=x,.t.} when choicef='J  '
        endif
#endif
        @ _skey[1]+4,_skey[2]+14 get b picture "@!" valid {|r|r:=b=' '.or.aczojs(grupy),if(r,choiceg:=b,),r} when choicef='J  '
        read
        message(_skey)
        if readkey()=27
           return .f.
        endif
        _skey:=push_stat()
        *
        if c>0
           SET CONSOLE OFF
/*
#ifdef A_WIN_PRN
           MEMVAR->oprn:=A_WIN_PRN
#endif
           print()
*/
        else
           scr:=savescreen()
           set alternate to (left(procname(),8)+'.TXT' )
           set alternate on
           set color to w
           cls
        endif
        *
        IF choicef="J  "
           WYDRUK_JAD(a,b,max(c,1),d)
        ELSEIF "J"$choicef .or. "O"$choicef .or. "Z"$choicef
           WYDRUK_dok()
        ENDIF
        *
        if c=0
           set alternate off
           fview(a:=SET(_SET_ALTFILE,''))
           ferase(a)
           restscreen(,,,,scr)
        endif
        SET CONSOLE ON
        *
        pop_stat(_skey)

   case _skey=-9
    if "REL"$ordsetfocus() //indexord()=2
    _slist(".\R"+left(alias(),2)+"*.frm",_s)
    else
    _slist(".\"+left(alias(),2)+"*.frm",_s)
    endif

   case !upden

   CASE _SKEY=-7 .and. ALIAS()="RELEWY"
    _sfor:=if(_sfor=NIL,{||dieta=" "},)
    REFRESH(,_s)

   ENDCASE

RETURN .F.   
*********************
func dok_in(deep)
//static poprec,oldrec,keyp,startrec
local stat,r,vars:={poprec,oldrec,keyp,startrec,kos,il,di,gr},_f

if keyp#NIL
   deep=.t.
endif

poprec:=startrec:=oldrec:=0
keyp:={data,posilek}

stat:=push_stat()
begin sequence
   select zapot
      set order to tag zap_rel
   select menu
      SET ORDER TO tag menu_rel
   SELECT OSOBY
      SET ORDER TO tag osob_kod
   SELECT MAIN
      SET ORDER TO tag main_rel
   SELECT RELEWY

  dbseek(dseek(,'data,posilek,dieta',keyp[1],keyp[2],''),.f.)
  keyp:=dtos(keyp[1])+keyp[2]

  select main
  if dbseek(keyp)
     kibord(chr(3))
  endif
  SELECT RELEWY
  if deep
  inkey()
  if !eof()
    FORM_EDIT(_f:={0,59,3,1,999,;
{|f|DOK1(f)},;
{||dok2({})},;
{||NIL},;
{||DBSELECTAREA("MAIN"),ordsetfocus("main_rel")},;
{|f|dok4(f,{},deep)},;
{|f|dok6(f,-2)}})
  endif
else
    lock
    FORM_EDIT(_f:={0,63,3,1,999,;
{|f|DOK1(f)},;
{|f,g|dok2(g)},;
{|f,g|dok3(f,g,@poprec,@keyp,@startrec)},;
{||DBSELECTAREA("MAIN"),ordsetfocus("main_rel")},;
{|f,g|dok4(f,g,deep)},;
{|f|dok5(f)}})
  if _flp=0.and.!main->(dbseek(keyp)).and.!zapot->(dbseek(keyp)).and.!menu->(dbseek(keyp))
#undef D_LAN
#ifdef A_LAN
  #define D_LAN reclock() .and. wart_tot=0
#else
  #define D_LAN wart_tot=0
#endif
     SELECT RELEWY
     seek dseek(,'data,posilek,dieta',stod(left(keyp,8)),subs(keyp,9,1),'')
     delete while keyp=dtos(data)+posilek for D_LAN
     changed:=.t.
  endif
endif

end sequence

  SELECT RELEWY
  r:=recno()
  unlock
pop_stat(stat)

poprec:=vars[1]
oldrec:=vars[2]
keyp:=vars[3]
startrec:=vars[4]  //space recover
kos:=vars[5]
il:=vars[6]
di:=vars[7]
gr:=vars[8]
RETURN r
**********************************************
static procedure dok1(_f)

 set cursor off
 SET COLOR TO (_sbkgr)
  scroll(0,_fco1,0,_fco2)
  @ 0,_fco1,3,_fco2 BOX '�ͻ�� �� '
  @ 0,_fco1+1 say "SPIS OS�B KORZYSTAJ�CYCH Z POSI�KU"
  @ 1,2 say "Ilo��"
 @ 1,10 say "Cena"
  @ 1,20 say "Data"
  @ 1,26 say "Pos."
  @ 1,42 say "Jad�ospis"
  @ 1,52 say "Zapotrzebow"
  @ 3,0,5,_fco2 BOX '�͹���Ⱥ '
  @ 3,2 say;
 '����Kto����������������������������Ilo�����di�gr�warto��'

return
***********
static proc dok2(getlist)
  local da,kon
  select relewy
  if eof()
     da:=DatE()
     kon:=left(posilki[1],1)
  else
     da:=data
     kon:=posilek
  endif
  @ 2,30 say posilki[max(1,ascan(posilki,kon))] COLOR _sbkgr
  @ 2,43 say if(menu->(dbseek(dtos(da)+KON,.f.)),"jest","brak") color _sbkgr
  @ 2,55 say if(zapot->(dbseek(dtos(da)+KON,.f.)),"jest","brak") color _sbkgr
  @ 2,18 get da picture "@D" valid {||setpos(2,43),devout(if(menu->(dbseek(dtos(da)+KON,.f.)),"jest","brak"),_sbkgr),setpos(2,55),devout(if(zapot->(dbseek(dtos(da)+KON,.f.)),"jest","brak"),_sbkgr),.t.}
  @ 2,30 get kon valid {|x,y|x:=NIL,y:=aczojs(posilki,,@x),devout(posilki[x],_sbkgr),setpos(2,43),devout(if(menu->(dbseek(dtos(da)+KON,.f.)),"jest","brak"),_sbkgr),setpos(2,55),devout(if(zapot->(dbseek(dtos(da)+KON,.f.)),"jest","brak"),_sbkgr),y}
  __setproc(procname(0))
return
**********

proc dok3(_f,getlist,poprec,keyp,startrec)
  local da,kon
  da:=getlist[1]:varget()
  kon:=getlist[2]:varget()
  if  dtos(da)+kon#keyp
      keyp:=dtos(data)+posilek
      eval(_fmainpre,_f)
      if dbseek(keyp)
         startrec:=recno()
         @ 3,_fco1+3 say 'F9' color _sbkgr
      else
         startrec:=0
         @ 3,_fco1+3 say '��' color _sbkgr
      endif
      select relewy
      changed:=.t.
      _fj=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
      if dbseek(dseek(,'data,posilek,dieta',da,kon,''))
         lock
         eval(_fmainpre,_f)
         if dbseek(dtos(da)+kon,.f.)
            @ 4,_fco1,5,_fco2 BOX '� ����Ⱥ ' color _sbkgr
            RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*2*(1+2*_fskip+_frow)+1))
            _fpopkey:=.f.
            poprec:=0
            select relewy
            keyp:=dtos(data)+posilek
            return
         endif
         select relewy
      else
        append blank
        data:=da
        posilek:=kon
        replace wartosc with 0
        //unlock
      endif
      _fpopkey:=.t.
#ifdef A_DEMO
      poprec:=0
#else
      poprec:=startrec
#endif
      @ 4,_fco1,5,_fco2 BOX '� ����Ⱥ ' color _sbkgr
      if startrec#0
         @ 5,_fco1+2 say "podpowied� z posi�ku "+tranR(keyp,"####.##.##/X")+" pod F9" color _sbkgr
      endif
  elseif _flp=0 .and. ile_pos=0 .and. !main->(dbseek(keyp)).and.!zapot->(dbseek(keyp)).and.!menu->(dbseek(keyp))
      delete while keyp=dtos(data)+posilek for D_LAN
      changed:=.t.
  else
      lock
  endif
return
*************
static proc DOK4(_f,getlist,deep)
static fpstart:=0
  _fnowy:=DTOS(data)+posilek#relewy->(DTOS(data)+posilek)
#ifdef A_LAN
  if !_fnowy .and. _fpopkey
    lock
  endif
#endif
  if _fnowy .and. poprec#0
     oldrec:=recno()
     go poprec
     _fpopkey:=.t.
     _fpos:=fpstart
  else
     oldrec:=0
  endif
  IF !_fnowy .or. oldrec#0
    OSOBY->(dbseek(main->KOD_OSOBY,.f.))
    KOS:=KOD_OSOBY+osoby->nazwisko
    il:=ile_pos
    gr:=grupa
    di:=dieta
    if oldrec#0
       startrec:=poprec
       skip
       poprec:=if(DTOS(data)+posilek=keyp,recno(),0)
       go oldrec
    endif
    showwar(_f,il,di,deep)
  elseif !_fpopkey .and. _fi>1
     if !deep 
        kibord(chr(27))
     endif
     return
  else
    il:=0
    kos:=space(30)
    di:=gr:=" "
    _fpos:=1
  endif
  @ _fk,6 GET kos PICTURE "@RK XXX,XXXXXXXXXXXXXXXXXXXXXXXXXXX" VALID {|k,r|if(k:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=kosval(_f,getlist).and.showwar(_f,il,di),setkey(-8,k),r}
  @ _fk,38 GET il PICTURE D_ILPIC valid {|k|if(k:changed.and.fpstart=0,fpstart:=2,),showwar(_f,il,di)}
  @ _fk,45 GET di valid aczojs(diety).and.showwar(_f,il,di)
  @ _fk,47 get gr valid aczojs(grupy)
  __setproc(procname(0))
  getlist[1]:reader:=getlist[2]:reader:=getlist[3]:reader:=getlist[4]:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
  fpstart:=0
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||dtos(data)+posilek=relewy->(dtos(data)+posilek)}))})
#endif
RETURN
********
stat proc f9(g,_f,getlist)
   local r:=osoby->(recno())
   oldrec:=recno()
   set relation to kod_osoby into osoby
   if startrec#0
      go if(poprec=0,startrec,poprec)
   endif
   if szukam({2,min(col(),maxcol()-60),maxrow(),,1,9,"Osobo-dni",;
     {||tran(dtos(data)+posilek,"@R XXXX.XX.XX|X")+"�"+kod_osoby+" "+osoby->nazwisko+"�"+tran(ile_pos,D_ILPIC)+"�"+dieta+"�"+grupa},;
     {|k,_s|(_sret:=k=13).or.rele(k,_s,.f.)},keyp})
    set relation to
    poprec:=recno()
    g:killfocus() // aby uniemo�liwi� undo()
    kos:=KOD_OSOBY+osoby->nazwisko
    il:=ile_pos
    di:=dieta
    gr:=grupa
    g:setfocus()
    showwar(_f,il,di)
    aeval(getlist,{|g|g:display()})
    startrec:=poprec
    keyp:=dtos(data)+posilek
    skip
    poprec:=if(DTOS(data)+posilek=keyp,recno(),0)
    updated(.t.)
   else
    set relation to
    osoby->(dbgoto(r))
    poprec:=0
   endif
   go oldrec
return
********
static PROC DOK5(_F)
local i,totrec,da,kon,carry,w,d,coldtot,cnewtot,a,b

#ifdef A_LPNUM
  setkey(402,NIL)
#endif
      if (updated() .or. oldrec#0) .and. !(_fnowy .and.(_fkey=27 .or. il=0))
        changed:=.t.
        if _fnowy
          _fnowy:=.f.
          append blank
          DATA:=RELEWY->DATA
          posilek:=RELEWY->posilek
#ifdef A_LPNUM
          pozycja:=str(_fi,len(pozycja))
#endif
        endif
        KOD_OSOBY:=osoby->kod_osoby
        if ile_pos#il .or. dieta#di .or. grupa#gr

           select relewy
           da:=data
           kon:=posilek
           totrec:=recno()
           if !Empty(main->dieta).and. !Empty(main->grupa)
#undef D_LAN
#ifdef A_LAN
  #define D_LAN .and. reclock(,,,,recno())
#else
  #define D_LAN
#endif
#ifdef A_GREX
              EXEC {||ile_pos-=main->ile_pos} WHILE data=da .and. posilek=kon FOR dind(main->dieta+'/'+main->grupa,dieta) D_LAN
#else
              EXEC {||ile_pos-=main->ile_pos} WHILE data=da .and. posilek=kon FOR dind(main->dieta,dieta) D_LAN
#endif
           endif
           if !Empty(di).and. !Empty(gr)
#ifdef A_GREX
             if !dbseek(dseek(,'data,posilek,dieta',da,kon,di+'/'+gr+' '))
#else
             if !dbseek(dseek(,'data,posilek,dieta',da,kon,di+' '))
#endif
               dbappend(.f.)
               data:=da
               posilek:=kon
#ifdef A_GREX
               dieta:=di+'/'+gr
#else
               dieta:=di
#endif
               REPLACE wartosc WITH 0
             endif
             go totrec
#ifdef A_GREX
             EXEC {||ile_pos+=il} WHILE data=da .and. posilek=kon FOR dind(di+'/'+gr,dieta) D_LAN
#else
             EXEC {||ile_pos+=il} WHILE data=da .and. posilek=kon FOR dind(di,dieta) D_LAN
#endif
           endif
           go totrec
           select main
           grupa:=gr
           dieta:=di
           ilE_POS:=il
#ifdef A_DODATKI
           field->opis:=osoby->stanowisko
#endif
        endif
        showwar(_f,il,di)
        if il=0
          delete
          _fnowy:=.t.
#ifdef A_LPNUM
          if _fi<_flp
          totrec:=recno()
          skip -1  // po zmianie pozycji by�by niepewny uk�ad, w dm_form i tak skip
          IF !BOF()
             totrec:=recno()
          ELSE
             GO totrec
          ENDIF
          skip
#ifdef A_LAN
          replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while DTOS(data)+posilek=relewy->(DTOS(data)+posilek) for reclock()
          go totrec
          lock
#else
          replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while DTOS(data)+posilek=relewy->(DTOS(data)+posilek)
          go totrec
#endif
          endif
#endif
        endif
        RELEWY->(fixrel())
      elseif oldrec#0 .and. _fkey#27
        _fkey:=13
        _fnowy:=.f.
      ENDIF
      unlock
RETURN
*****************
proc dok6(_f,mkey)
local bx,cx,dx
do while _fpopkey .and. (!_fnowy .or. _fi=1)

  @ _fk,_fco1+1 say chr(16) color _sbkgr
  @ _fk,_fco2-1 say chr(17) color _sbkgr

#ifdef A_MYSZ
     sysint(51,1)
     bx:=cx:=dx:=0
     do while (_fkey:=inkey())=0
        sysint(51,3,@bx,@cx,@dx)
        if bx#0
            _fkey:=14
            cx:=round(cx*.125,0)
            dx:=round(dx*.125,0)
            exit
        endif
     enddo
     sysint(51,2)
#else
   _fkey:=inkey(0)
#endif

  @ _fk,_fco1+1 say " " color _sbkgr
  @ _fk,_fco2-1 say " " color _sbkgr
  
  do case
    case _fkey=5  .and. _fi>1
         _fkey=18
    case _fkey=18 .and. _fj>0
         skip _fj+1-_fi
         _fi:=_fj+1
    case _fkey=24 .and. _fi<_flp
         _fkey:=3
    case _fkey=3 .and. _fl<_flp
         skip _fl-_fi
         _fi:=_fl
    case setkey(_fkey)#NIL
         eval(setkey(_fkey),procname(1),procline(1),"")
         loop
#ifdef A_MYSZ
    case _fkey=14
         if dx=_fk .and. cx>_fco1 .and. cx<_fco2 .and. setkey(mkey)#NIL
         eval(setkey(mkey),procname(1),procline(1),"")
         loop
         else
         readkey(,{bx,cx,dx})
         endif
#endif
    case _fkey = 13 .and. setkey(mkey)#NIL
         eval(setkey(mkey),procname(1),procline(1),"")
         loop
    case _fkey#23 .and. _fkey#27
         loop
  endcase
  exit
enddo

return
**********
proc evfxar(a,b)
  LOCAL d:=dieta,w:=WART_TOT,il:=ILE_POS,c

#ifdef A_GREX
  if d>='0' .and. subs(d,2)='/' .and. subs(d,3)>='0' .and. empty(subs(d,4))
#else
  if d>='0' .and. empty(subs(d,2))
#endif
    if w<>0 .or. il<>0
      aadd(b,{trim(d),w,il})
    endif
  else
    if w<>0 .and. il<>0
      aadd(a,{dieta,w,il}) // w/il
    endif
  endif
return
***************
func mkfxar(da,po)
local key:=dtos(da)+po, rec:=recno(), a:={}, b:={}, c

seek dseek(,'data,posilek,dieta',da,po,"")

EXEC evfxar(@a,@b) WHILE key = dtos(data)+posilek

aeval(b,{|x|c:=x,aeval(a,{|x|if(dind(c[1],x[1]),c[2]+=c[3]*x[2]/x[3],)})})

goto rec
return b
*******************
proc fixrel()
local key:=dtos(data)+posilek, rec:=recno(), b, d, w

b:= mkfxar(data,posilek)
WHILE key=dtos(data)+posilek
   w:=0
   d:=dieta
   aeval(b,{|x|if(dind(x[1],d),w+=x[2],)})
   LOCK recno()
   REPLACE wartosc WITH w
   SKIP
ENDDO
goto rec

return
*************************
static function showwar(_f,il,di,deep)

field  ile_pos,d_wartosc
local rec,w,i,da,kon
//if deep=.t.
   select RELEWY
   rec:=recno()
   da:=data
   kon:=posilek
   if !dbseek(dseek(,'data,posilek,dieta',da,kon,di))
      goto rec
   endif
   //i:=ile_pos
   select MAIN
   //i+=il-if(_fnowy .or. MAIN->dieta<>di,0,MAIN->ile_pos)
   @ _fk,50  say strpic(round(il*D_CENA,A_ZAOKR),10,A_ZAOKR,"@E ",.t.) color _sbkgr
#ifdef A_DODATKI
   @ _fk,49 say trim(main->opis) COLOR _sbkgr
#endif
   RELEWY->(dbgoto( rec ))
//endif
@ 2,9   say strpic(D_CENA,5,A_ZAOKR,"@E ",.t.) color _sbkgr
@ 2,2   say relewy->ile_pos+if(_fnowy,il,il-ile_pos) picture D_ILPIC color _sbkgr
return .t.
***************************
static FUNCTION kosval(_f,getlist)
LOCAL ZNALAZ,recm,recr,reco,r
field ile_pos,kod_osoby,nazwisko,stanowisko,grupa

  IF KOS=osoby->KOD_OSOBY .and. !kos="   "
     if kos#osoby->KOD_OSOBY+osoby->nazwisko
        kos:=osoby->KOD_OSOBY+osoby->nazwisko
     endif
     if il=0
        il:=1
        di:=osoby->dieta
        gr:=osoby->grupa
        aeval(getlist,{|g|g:display()},2)
        updated(.t.)
     endif
     RETURN .T.
  endif
  recm:=recno()
  recr:=relewy->(recno())
  reco:=osoby->(recno())
  select OSOBY
    set order to tag osob_kod
    set relation to

  ZNALAZ:=szukam({0,min(col(),maxcol()-60),maxrow(),,1,len(trim(kos)),,;
  {||kod_osoby+"�"+nazwisko+"�"+stanowisko+"�"+dieta+"�"+grupa},;
    {|k,s|sosob(k,s,.f.)},UpP(trim(kos))})
  select relewy
    go recr
  SELECT osoby
    SET ORDER TO tag osob_kod
    SET RELATION TO
  select zapot
    SET ORDER TO tag zap_rel
    SET RELATION TO
  select menu
    SET ORDER TO tag menu_rel
    SET RELATION TO
  SELECT MAIN
    SET ORDER TO tag main_rel
    SET RELATION TO
    go recm

  IF ZNALAZ
    IF _fnowy .and. oldrec#0 .and. osoby->(recno())#reco
       poprec:=startrec
    ENDIF
    kos:=osoby->KOD_OSOBY+osoby->nazwisko
    il:=1
    di:=osoby->dieta
    gr:=osoby->grupa
    aeval(getlist,{|g|g:display()},2)
    updated(.t.)
  RETURN .T.
   ELSE
   osoby->(dbgoto(reco))
   ENDIF
RETURN .F.
*******************
FUNCTION KATALOG(_s)
      SETPOS(10,0)
       select OSOBY
       set order to tag osob_kod
       set relation to
DEFAULT _s        TO array(_sLEN)
DEFAULT _sbeg     TO 1
DEFAULT _slth     TO 0
DEFAULT _sprompt  TO {||kod_osoby+"�"+nazwisko+"�"+stanowisko+"�"+dieta+"�"+grupa}
DEFAULT _sinfo    TO {|_skey,_s|sosob(if(_skey=13,9,_skey),_s,.t.)}
DEFAULT _spocz    TO ''
RETURN szukam(_s)
******************
FUNCTION sosob(_skey,_s,upden)

local n,g,s,i,k,d,r,getlist

DO CASE
  CASE _skey=0
DEFAULT _snagl    TO 'Kod�'+pad('Nazwisko',len(nazwisko),'�')+'�Stanowisko'
      if _slth=0
      elseif _spocz>'9'
        SET ORDER TO tag osob_naz
        _sbeg:=5
        SEEK _spocz
      else
        _spocz:=left(_spocz,3)
        _slth:=len(_spocz)
        if DBSEEK(_spocz) .and. _spocz=kod_osoby
           RETURN (_sret:=.T.)
        endif
      ENDIF
    _swar:=&('{|p|'+IndexkeY(0)+'=p'+'}')
    if ! ( (eval(_swar,_spocz).or.dbseek(_spocz)).and._skip(0,,_s) )
      _spocz:=LEFT(_spocz,len(_spocz)-_slth)
      _slth:=0
      _sef:=.f.
      if !eval(_swar,_spocz)
         _skip(-1,,_s)
         if !eval(_swar,_spocz)
            seek _spocz
         endif
      endif
    endif
    set cursor on
    _snagkol:=0

   case _skey=27
        return .t.

   CASE _skey=2 .AND. _sbeg=1 // ^>
    SET ORDER TO tag osob_naz
    _sbeg=5
    _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
    _spocz=LEFT(_spocz,len(_spocz)-_slth)
    _slth=0
    refresh(1,_s)

   CASE _skey=26 .AND. _sbeg#1 // ^<
    SET ORDER TO tag osob_kod
    _spocz=LEFT(_spocz,len(_spocz)-_slth)
    _slth=0
    _sbeg=1
    _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
    refresh(1,_s)

   CASE _skey=22 .or. _skey=43 //+

    go _srec[_sm]
    lock
    k:=kod_osoby
    n:=nazwisko
    d:=dieta
    g:=grupa
    s:=stanowisko
    getlist:={}
    r:=.f.
    if ascan(grupy,grupa)=0
       g:=left(grupy[1],1)
    endif
    if ascan(diety,dieta)=0
       d:=left(diety[1],1)
    endif
    if _skey=43
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
       _skey:=indexord()
       set order to tag osob_kod
       go bottom
       k:=str(val(kod_osoby)+1,3)
       set order to (_skey)
       r:=.t.
    endif
    @ _srow1+_sm-1,_scol1 get k
    getl n
    getl s
    getl d valid aczojs(diety)
    getl g valid aczojs(grupy)
    read
    if readkey()#27 .and. (updated() .or. k#kod_osoby)
      if empty(n)
        SELECT MAIN
        SET ORDER TO tag main_kod
        SET RELATION TO
        if dbseek(osoby->kod_osoby)
           select osoby
           alarm("Tej osoby nie mo�na wykasowa�,;jad�a posi�ek dnia "+dtoc(main->data),,,3)
        else
           select osoby
           delete
           refresh(,_s)
        endif
      else
        set order to tag osob_kod
        if empty(k)
           go bottom
           k:=str(val(kod_osoby)+1,3)
           append blank
           kod_osoby := k
         elseIF !dbseek(k)
           append blank
           kod_osoby := k
         endif
         nazwisko:=n
         grupa:=g
         dieta:=d
         stanowisko:=s
         set order to if(_sbeg=1,"osob_kod","osob_naz")
         if ! eval(_swar,_spocz)
            _spocz=LEFT(_spocz,LEN(_spocz)-_slth)
           _slth=0
         endif
         refresh(,_s)
      endif
    endif
    unlock
    return r

   case _skey>31 .and. _skey<256
    do case
      case _skey>64 .AND. _sbeg=1
        SET ORDER TO tag osob_naz
        _spocz=LEFT(_spocz,len(_spocz)-_slth)+UpP(CHR(_SKEY))
        _sbeg=5
        _slth=1

      case _sbeg#1  .and. _slth=1 .and. _skey<58
        SET ORDER TO tag osob_kod
        _sbeg=1
      OTHERWISE
        RETURN .F.
    ENDCASE       
    _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')
    if ! dbseek(_spocz)
      _spocz=LEFT(_spocz,LEN(_spocz)-_slth)
      _slth=0
    endif
    refresh(,_s)
    return .t.
   
   case _skey=-8
      _sfil(_s)

   CASE _si=0

   CASE _SKEY=13
    RETURN(_sret:=.T.)

   CASE _skey=9  // tab
       _skey:=push_stat()
        n:=KOD_OSOBY+left(dtos(mies_rob),6)
        set order to tag osob_kod
        SELECT MAIN
        SET ORDER TO tag main_kod
        SET RELATION TO RELEWY->(dseek(,'data,posilek,dieta',MAIN->data,MAIN->posilek,MAIN->dieta)) INTO RELEWY,;
         to UpP(kod_osoby) into osoby
        SEEK n
szukam({1,min(col()+20,maxcol()-30),maxrow(),,1,9,"Data������Ilo����Warto����",;
{||kod_osoby+"|"+tran(Dtos(data)+posilek+dieta,"@R ####.##.##|X|X")+"�"+tran(ILE_POS,D_ILPIC)+"�"+strpic(ile_pos*D_CENA,9,A_ZAOKR,"@E ",.t.)+"�"+osoby->nazwisko},;
{|k,s|RELe(k,s,upden)},n})
        pop_stat(_skey)
      
   case _skey=-9
      _slist(".\O*.frm",_s)

ENDCASE

RETURN(.F.)
************
