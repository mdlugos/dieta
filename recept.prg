#include "dm_form.ch"

static poprec,oldrec,keyp,startrec,curprompt,nowy,kon,na,gra,jed,die,op,il,di

MEMVAR CHANGED,diety,posilki,grupy,mies_rob

field posilek,danie,ilosc,gramatura,jedn,skladnik,nazwa,indx_mat,przel,dieta,gram,data,opis,jmaG,cena,pozycja

*****************
func SURowce(upden)
DEFAULT upden TO .t.
SELECT SUROWCE
SET ORDER TO tag sur_naZ
#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif
select zawar
set order to tag "zaw_skl"
#ifdef A_ELZ
select cennik
set order to 1
select surowce
set relation to skladnik into cennik
return SZUKAM({0,maxcol()/2-35,,,1,0,"PRZEGL¤D SUROWCàW CENA DATA",{||NAZWA+if(zawar->(found()),"³","!")+jmaG+'³'+str(cennik->cena)+'³'+dtoc(cennik->data)},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#else
select surowce
set relation to skladnik into zawar
return SZUKAM({0,maxcol()/2-20,,,1,0,"PRZEGL¤D SUROWCàW",{||NAZWA+if(zawar->(found()),"³","!")+jmaG},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#endif
*****************
func rec_in(deep,n)

local stat,r,vars:={poprec,oldrec,keyp,startrec,curprompt,nowy,kon,na,gra,jed,die,op,il,di}
if keyp#NIL
   deep=.t.
endif
poprec:=startrec:=oldrec:=0
keyp:=danie
nowy:=n .or. eof()
stat:=push_stat()

begin sequence

  select menu
      SET ORDER TO tag menu_dan

	select surowce
      SET ORDER TO tag sur_kod

	select dania
      SET ORDER TO tag dan_kod

  dbseek(keyp,.f.)

  curprompt:=trim(nazwa)+" "+trim(dieta)+" "+trim(gramatura)+" "+trim(jedn)

  kon:=posilek

	select sklad
    SET ORDER TO tag skl_dan

 if dbseek(keyp)
    kibord(chr(3))
 endif

select dania

if deep
   inkey()
   if !eof()
    FORM_EDIT({20,61+A_DILTH,4,1,999,;
{|f|RDOK1(f)},;
{|f|rdok2(f,{})},;
{||setcursor(0)},;
{||DBSELECTAREA("SKLAD"),ordsetfocus("skl_dan")},;
{|f|Rdok4(f,{},deep)},;
{|f|dok6(f,-4)}})
    endif
else
    lock
    FORM_EDIT({20,61+A_DILTH,4,1,999,;
{|f|RDOK1(f)},;
{|f,g|rdok2(f,g)},;
{|f,g|rdok3(f,g)},;
{||DBSELECTAREA("SKLAD"),ordsetfocus("skl_dan")},;
{|f,g|Rdok4(f,g,deep)},;
{|f|Rdok5(f)}})
endif

end sequence

select dania
r:=recno()
unlock

pop_stat(stat)
poprec:=vars[1]
oldrec:=vars[2]
keyp:=vars[3]
startrec:=vars[4]
curprompt:=vars[5]
nowy:=vars[6]
kon:=vars[7]
na:=vars[8]
gra:=vars[9]
jed:=vars[10]
die:=vars[11]
op:=vars[12]
il:=vars[13]
di:=vars[14]

RETURN r
**************
stat PROC RDOK1(_f)
	SET CURSOR OFF
	SET COLOR TO (_SBKGR)
  @ 0,_fco1,3,_fco2 BOX 'ÉÍ»ºº ºº '
  @ 4,_fco1,6,_fco2 BOX 'ÌÍ¹º¼ÍÈº '
  @ 4,_fco1+3 say 'F9'
  @ 0,_fco1+1 SAY "DANIE - NAZWA - SKADNIKI"
  @ 2,_fco1+10 say "Gramatura:"
  @ 2,_fco1+31 say "Dieta:"
  @ 4,_fco1+6 say 'Skˆadnik'
  @ 4,_fco1+26 say 'Ilo˜†'
  @ 4,_fco2-6 say 'Dieta'
RETURN
***********
stat proc rdok2(_f,getlist)
  local now
  select dania
  na:=nazwa
  gra:=gramatura
  jed:=jedn
  die:=dieta
  op:=if(""=opis,"bez uwag",opis)
  now:=if(nowy,"NOWY   ","POPRAWA")
  @  1,_fco1+1 get kon valid aczojs(posilki)
  @  1,_fco1+3 get NA picture "@KS"+ltrim(str(_fco2-_fco1-4))
  @  2,_fco1+2 get now picture "@K" valid {||nowy:=if(now=" ",!nowy,nowy),now:=if(nowy,"NOWY   ","POPRAWA"),.t.}
  @  2,_fco1+21 get gra picture "@K" valid {||gra:=padl(trim(gra),4),.t.}
  @  2,_fco1+26 get jed picture "@K"
  @  2,_fco1+38 get die picture "@KS"+ltrim(str(A_DILTH)) VALID {|g|dival(g)}
  @  3,_fco1+2  get op picture "@KS"+ltrim(str(_fco2-_fco1-3)) send cargo:=.t.
  __setproc(procname(0))
return
************
stat proc rdok3(_f)
  local dan
  if updated()
      keyp:=danie
      if sklad->(dbseek(keyp))
         startrec:=sklad->(recno())
         curprompt:=trim(nazwa)+" "+trim(dieta)+" "+trim(gramatura)+" "+trim(jedn)
//         @ 4,_fco1+3 say 'F9' color _sbkgr
      else
//         @ 4,_fco1+3 say 'ÍÍ' color _sbkgr
         startrec:=0
      endif
      changed:=.t.
      _fj=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
    if nowy
      nowy:=.f.
      set order to tag dan_naz
      if dbseek(dseek(,'posilek,nazwa',kon,na)).and.gra+jed+die=gramatura+jedn+dieta .OR. empty(NA)
        @ 5,_fco1,6,_fco2 BOX 'º ºº¼ÄÈº ' color _sbkgr
        RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*2*(1+2*_fskip+_frow)+1))
        _fpopkey:=.f.
        return
      endif
      @ 5,_fco1,6,_fco2 bOX 'º ºº¼ÍÈº ' color _sbkgr
      _fpopkey:=.t.
      SET ORDER TO tag dan_kod
#ifdef A_LPNUM
      go bottom
      dan:=str(val(danie)+1,len(danie))
#else
      GO lastrec()
      dan:=if(eof(),chr(0)+chr(0),i2bin(bin2w(DANIE)+1))
#endif
      APPEND BLANK
      DANIE:=DAN
#ifdef A_DEMO
      poprec:=0
#else
      poprec:=startrec
#endif
    elseif empty(na)
      select menu
      set order to tag menu_dan
      if !dbseek(dania->danie)
        if 1=alarm("Czy skasowa† danie ?",{"Tak","Nie"},2)
           lock
           select sklad
           set order to tag skl_dan
           dbseek(dania->danie)
#ifdef A_LAN
           delete while danie==dania->danie rest for reclock()
#else
           delete while danie==dania->danie rest
#endif
           unlock
           select dania
           delete
           commit
        endif
      else
        alarm("Wyst©puje w jadˆospisie dnia "+dtoc(menu->data)+";nie mo¾na wykasowa†.",,,3)
      endif
      select dania
      break
    else
      lock
    endif
    NAZWA:=NA
    posilek:=KON
    dieta:=die
    jedn:=jed
    gramatura:=gra
    op:=trim(op)
    opis:=if("bez uwag"=op,"",op)
  endif

return
***********
stat proc RDOK4(_f,getlist,deep)
  static fpstart:=0
  _fnowy:=!danie==dania->danie
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

    if _fnowy .and. oldrec=0
     if !_fpopkey .and. _fi>1
       if !deep
          kibord("")
       endif
       return
     endif
     il:=0
     na:=space(len(surowce->nazwa))
     di:=space(len(dieta))
     _fpos:=1
   else
      il:=ilosc
      di:=dieta
      surowce->(dbseek(sklad->skladnik,.f.))
      na:=surowce->nazwa
      showzaw(_f)
    if oldrec#0
       startrec:=poprec
       skip
       poprec:=if(danie==keyp,recno(),0)
       go oldrec
    endif
    endif

    @ _fk,_fco1+6 GET na PICTURE "@KS20" VALID {|k,r|if(k:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=surval(_f,getlist,@na,@poprec,oldrec,startrec,.f.) .and. showzaw(_f),setkey(-8,k),r}
    GETL il picture "####.##" valid {|k|if(k:changed.and.fpstart=0,fpstart:=2,),showzaw(_f)}
    @ _fk,_fco2-A_DILTH-1 GET di PICTURE "@KS"+ltrim(str(A_DILTH)) valid {|g|if(g:changed.and.fpstart=0,fpstart:=3,),dival(g)}
     __setproc(procname(0))
  //if startrec#0
     aeval(getlist,{|g|g:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}})
  //endif
  fpstart:=0
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||danie==dania->danie}))})
#endif
RETURN
********
stat proc f9(g,_f,getlist)
local r:=surowce->(recno()),s:=dania->(recno())
   oldrec:=recno()

   if startrec=0
      poprec:=0
      if dan_kat(.f.)
         SELECT DANIA
         set order to tag dan_kod
         keyp:=danie
         curprompt:=trim(nazwa)+" "+trim(dieta)+" "+trim(gramatura)+" "+trim(jedn)
         SELECT SKLAD
         seek keyp
         startrec:=recno()
         dania->(dbgoto(s))
      else
         select dania
         set order to tag dan_kod
         go s
         select surowce
         go r
         select sklad
         go oldrec
         return
      endif
   endif

   set relation to skladnik into surowce
   go if(poprec=0,startrec,poprec)
    if szukam({2,min(col(),maxcol()-60),maxrow(),,0,0,curprompt,;
     {||surowce->nazwa+"³"+str(ilosc)+" "+surowce->jedN},;
     {|k,_s D_MYSZ|(_sret:=k=13).or.sur(k,_s,.f. D_MYSZ)},keyp})
    set relation to
    poprec:=recno()
    g:killfocus()
    il:=ilosc
    di:=dieta
    na:=surowce->nazwa
    g:setfocus()
    showzaw(_f)
    getlist[2]:display()
    startrec:=poprec
    skip
    poprec:=if(danie==keyp,recno(),0)
    updated(g:changed:=.t.)
   else
    set relation to
    surowce->(dbgoto(r))
    poprec:=0
   endif
   go oldrec
RETURN
*********
stat PROC RDOK5(_f)
local totrec
    if (oldrec#0 .or. updated()) .and. !((il=0 .or. _fkey=27).and. _fnowy)
      changed:=.t.

        if _fnowy
          _fnowy:=.f.
          append blank
          DANIE:=DANIA->DANIE
#ifdef A_LPNUM
          pozycja:=str(_fi,len(pozycja))
#endif
        endif

     SKLADNIK:=if(empty(na),'',SUROWCE->SKLADNIK)
     ILOSC:=IL
     DIETA:=di
     if il=0
          delete
          _fnowy:=.t.
#ifdef A_LPNUM
          if _fi<_flp
          totrec:=recno()
          skip -1  // po zmianie pozycji byˆby niepewny ukˆad, w dm_form i tak skip
          IF !BOF()
             totrec:=recno()
          ELSE
             GO totrec
          ENDIF
          skip
#ifdef A_LAN
          replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while DANIE=DANIA->DANIE for reclock()
          go totrec
          lock
#else
          replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while DANIE=DANIA->DANIE
          go totrec
#endif
          endif
#endif
      endif
    elseif oldrec#0 .and. _fkey#27
        _fkey:=13
        _fnowy:=.f.
    endif
    unlock
RETURN
***************
stat func showzaw(_f)
  @ _fk,_fco1+35 say surowce->jedN color _sbkgr
return .t.
*****************
FUNCTION SURVAL(_f,getlist,na,poprec,oldrec,startrec,aflag,apos)

local da,sk,su,re,za,znalaz,sel

/*
#ifdef A_GOCZ
if empty(na)
   return .t.
endif
#endif
*/
if na=surowce->nazwa .and. !surowce->(eof())
   return .t.
endif
sel:=select()
da:=dania->(recno())
sk:=sklad->(recno())
re:=relewy->(recno())
za:=zapot->(recno())
#ifdef A_ELZ
select cennik
set order to 1
select surowce
su:=recno()
SET ORDER TO tag sur_naZ
set relation to skladnik into cennik
znalaz:=SZUKAM({0,,maxrow(),maxcol(),1,len(trim(na)),"PRZEGL¤D SUROWCàW",if(alias(sel)="ZAPOT",{||NAZWA+"³"+jmaG+"³"+str(cennik->cena)+'³'+dtoc(cennik->data)},{||NAZWA+"("+jmaG+")³"+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#else
select surowce
su:=recno()
SET ORDER TO tag sur_naZ
znalaz:=SZUKAM({0,min(col(),maxcol()-30),maxrow(),,1,len(trim(na)),"PRZEGL¤D SUROWCàW",if(alias(sel)="ZAPOT",{||NAZWA+"³"+jmaG},{||NAZWA+"("+jmaG+")³"+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#endif
  set order to tag sur_kod
  select zapot
  SET ORDER TO tag zap_rel
    SET RELATION TO
    goto za
  select relewy
    goto re
  select dania
    SET ORDER TO tag dan_kod
    goto da
  select sklad
    SET ORDER TO tag skl_dan
    SET RELATION TO
  goto sk
  select (sel)
if znalaz
  IF _fnowy .and. oldrec#0 .and. surowce->(recno())#su
    if aflag
       --apos
    else
       poprec:=startrec
    endif
  ENDIF
  na:=surowce->nazwa
  //getlist[1]:varput(surowce->nazwa)
  updated(.t.)
  return .t.
endif
aflag:=.f.
surowce->(dbgoto(su))
RETURN .F.
**************************
function sur(_skey,_s,upden D_MYSZ)
field index,nazwa,stan,jm,waznosc,data_przy
static choice:=0
local stat
do case
  CASE _SKEY=0 .and. alias()="SUROWCE"
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
#ifdef A_MYSZ
    _skproc[14]:=NIL

   case _skey=14
    if bx=1.and.dx=_srow1+_sm-1.and.cx>_scol2-5.and.cx<_scol2
       evakey(22,_s)
	else
       return mysz(_s,bx,cx,dx,myszflag)
	endif
#endif

  case _skey=27
		return .t.

   case _skey=43
      go _srec[_sm]
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
    private changed:=.f.
    _skey:=sur_in(!upden,.t.)
    if changed==.t.
      if alias()='SUROWCE'
         go _skey
      endif
      if ! eval(_swar,_spocz)
        if _slth=0
           dbseek(_spocz)
        else
        _spocz=LEFT(_spocz,len(_spocz)-_slth)
        _slth=0
       endif
      endif
      refresh(,_s)
    elseif upden
      go _srec[_sm]
      REFRESH LINE _srow1-1+_sm DIRECTION 0
    endif
    return .t.

      
  case _skey=22

     private changed:=.f.

    _skey:=sur_in(!upden,.f.)

    if changed==.t.
      if alias()='SUROWCE'
         go _skey
      endif
      if ! eval(_swar,_spocz)
        if _slth=0
           dbseek(_spocz)
        else
        _spocz=LEFT(_spocz,len(_spocz)-_slth)
        _slth=0
       endif
      endif
      refresh(,_s)
    elseif upden
      go _srec[_sm]
      REFRESH LINE _srow1-1+_sm DIRECTION 0
    endif

  CASE _si=0
  case _skey=9 .or. _skey=92  //asc('\')
    stat:=push_stat()
#ifdef A_ELZ
    if _skey=92
       go _srec[_sm]
       _slth=_slth-1
       _spocz=left(_spocz,LEN(_spocz)-1)
       choice:=4
    else
       alarm("WYBIERZ CO—:",{"DANIA","ZAPOTRZEBOWANIE","STANY MAGAZYNOWE","CENNIK"},@choice)
    endif
    if choice=1
#else
    if alarm("WYBIERZ CO—:",{"DANIA","ZAPOTRZEBOWANIE","STANY MAGAZYNOWE"},@choice)=1
#endif
       select dania
       set order to tag dan_kod
       select sklad
       set order to tag skl_skl
       set relation to danie into dania
       dbseek(surowce->skladnik,.f.)
       szukam({1,min(maxcol()-60,col()+5),maxrow(),,0,0,'Danie',{||dania->(nazwa+if(""=opis,"³","&")+left(dieta,A_DILTH)+"³"+gramatura+" "+jedn)},{|_skey,_s D_MYSZ|_skey:=if(_skey=13,9,_skey),danszuk(_skey,_s,upden D_MYSZ)},surowce->skladnik})
    elseif choice=2
       select zapot
       set order to tag zap_skl
        set relation to RELEWY->(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,'')) into relewy
       dbseek(surowce->skladnik,.f.)
       szukam({1,min(maxcol()-27,col()+5),maxrow(),,1,6,"dataÄÄÄÄÂile "+trim(surowce->jmaG),{||tran(Dtos(data)+posilek+dieta,"@R ####.##.##|X|"+REPLICATE("X",A_DILTH))+"³"+str(ILOSC)},{|k,s|RELe(k,s,upden)},surowce->skladnik+left(dtos(mies_rob),6)})
    elseif choice=3
       SZUKAM({1,1,maxrow(),,1,0,"PRZEGL¤D MAGAZYNU SPO½YWCZEGO",{||INDEX+"³"+NAZWA;
       +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"ð","³");
       +STR(STANY->STAN)+" "+JM},{|_skey,_s D_MYSZ|if(_skey=13,.f.,STANMAG(_skey,_s D_MYSZ))},trim(surowce->indx_mat),.F.})
#ifdef A_ELZ
    elseif choice=4
       select cennik
       setpos(row()+2,col())
       dbseek(surowce->skladnik)
       szukam({,,,,0,,'Cena za '+surowce->jmaG,{||dtoc(data)+"|"+str(cena)},{|k,s|cen(k,s)},surowce->skladnik+chr(0),surowce->skladnik+chr(255)})
       dbseek(surowce->skladnik,.f.)
       select surowce
       REFRESH LINE _srow1-1+_sm DIRECTION 0
#endif
    endif
    pop_stat(stat)
    if _skey>=32
       return .t.
    endif

   case _skey=13
		return _sret:=.t.
   case _skey=-8
      _sfil(_s)
      
   case _skey=-9
      _slist(".\"+left(alias(),3)+"*.frm",_s)

endcase
return .f.
#ifdef A_ELZ
**************************
function cen(_skey,_s)
LOCAL c,d,getlist
do case
   case _skey=27
    return .t.
   case _skey=22 .or. _skey=asc('+')
    d:=DatE()
    if _si=0
         _srec[1]=recno()
         _sm=1
         go lastrec()+1
    elseif _skey=asc('+')
       go _srec[_sm]
       _slth=_slth-1
       _spocz=left(_spocz,LEN(_spocz)-1)
    else
         lock
         d:=data
    endif
    c:=cena
    getlist:={}
    @ _srow1+_sm-1,_scol1 get d
    getl c picture "@K"
    read
    if readkey()#27 .and. updated()
      if empty(c) .or. empty(d)
         delete
      elseif d=data .and. c#cena .or. d<=data .and. c=cena
         cena:=c
         data:=d
      else
        append blank
        data:=d
        cena:=c
        skladnik:=surowce->skladnik
      endif
    endif
    refresh(,_s)
    if _skey>=32
       return .t.
    endif

  case _skey=19
       kibord(chr(27)+chr(5)+'\')

  case _skey=4
       kibord(chr(27)+chr(24)+'\')

  CASE _si=0

  case _skey=-8
      _sfil(_s)
      
  case _skey=-9
      _slist(".\"+left(alias(),3)+"*.frm",_s)

endcase
return .f.
***********
#endif
