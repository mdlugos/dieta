#include "dm_form.ch"

field ilosc,jedn,jmaG,skladnik,nazwa,indx_maT,przel,gram,element,danie,data,;
      posilek,cena,pozycja,kod,opis,dieta,gramatura

static poprec,oldrec,keyp,startrec,curprompt,nowy,na,in,jem,prz,jed,gra,il,ce,da,keyf9,kw,op

MEMVAR CHANGED,diety,posilki,grupy,mies_rob

#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif

#ifdef __HARBOUR__
#define D_REST 4
#else
#define D_REST 2
#endif
************************
func sur_in(deep,n)

local stat,r,vars:={poprec,oldrec,keyp,startrec,curprompt,nowy,na,in,jem,prz,jed,gra,il,ce,da,keyf9,kw,op}
poprec:=startrec:=oldrec:=0
if keyp#NIL
   deep=.t.
endif
keyp:=skladnik
nowy:=n .or. EOF()

stat:=push_stat()

begin sequence
#ifdef A_ELZ
  select cennik
     set order to 1
#endif
  select sklad
      set order to tag skl_skl
  select zapot
      set order to tag zap_skl
  select elementy
      SET ORDER TO tag ele_kod
  select SUROWCE
      SET ORDER TO tag sur_kod

  dbseek(keyp,.f.)
  curprompt:=trim(nazwa)+" "+jmaG
  select zawar
  SET ORDER TO tag zaw_skl

 if dbseek(keyp)
    kibord(chr(3))
 endif

  select surowce

if deep
   inkey()
   if !eof()
    FORM_EDIT({20,61,5,1,999,;
{|f|sDOK1(f)},;
{|f|sdok2(f,{},deep)},;
{||setcursor(0)},;
{||DBSELECTAREA("zawar"),ordsetfocus("zaw_skl")},;
{|f|sdok4(f,{},deep)},;
{|f|dok6(f)}})
    endif
else
    lock
    FORM_EDIT({20,61,5,1,999,;
{|f|sDOK1(f)},;
{|f,g|sdok2(f,g,deep)},;
{|f|sdok3(f)},;
{||DBSELECTAREA("zawar"),ordsetfocus("zaw_skl")},;
{|f,g|sdok4(f,g,deep)},;
{|f|sdok5(f)}})
endif

end sequence

select surowce
r:=recno()
unlock

pop_stat(stat)

poprec:=vars[1]
oldrec:=vars[2]
keyp:=vars[3]
startrec:=vars[4]
curprompt:=vars[5]
nowy:=vars[6]
na:=vars[7]
in:=vars[8]
jem:=vars[9]
prz:=vars[10]
jed:=vars[11]
gra:=vars[12]
il:=vars[13]
ce:=vars[14]
da:=vars[15]
keyf9:=vars[16]
kw:=vars[17]
op:=vars[18]
RETURN r
**************
stat pROC sDOK1(_f)
SET CURSOR OFF
SET COLOR TO (_sbkgr)
  @ 0,_fco1,4,_fco2 BOX '�ͻ�� �� '
  @ 5,_fco1,7,_fco2 BOX '�͹���Ⱥ '
  @ 5,_fco1+3 say 'F9'
  @ 0,_fco1+1 say "ZAWARTO�� ELEMENT�W"
  @ 1,_fco1+9 say "sk�adnik"
  @ 1,_fco1+25 SAY 'kod magazynu'
#ifdef A_KODY
  @  3,_fco1+2  say A_KODY+':'
#else
#ifdef A_ELZ
  @ 3,_fco1+5 say 'cena            wa�na od'
#endif
#endif
  @ 4,_fco1+2 say '1      =          , zawarto�� w     :'

RETURN
***********
stat proc sdok2(_f,getlist,deep)
  local now,greader
  field index,stan,jm,waznosc,data_przy,jedN
  select surowce
  if !nowy
     lock
  endif
  na:=nazwa
  in:=indx_maT
  prz:=przel
  jed:=jedN
  jem:=jmaG
  gra:=gram
  now:=if(nowy,"NOWY   ","POPRAWA")
#ifdef A_KODY
  kw:=kod
  if fieldpos('OPIS')<>0
     op:=opis
  endif
#endif
  @  5,_fco1+3 SAY if(empty(deep),'F9','��') COLOR (_sbkgr)
  @  2,_fco1+2  get now picture "@K" valid {||nowy:=if(now=" ",!nowy,nowy),now:=if(nowy,"NOWY   ","POPRAWA"),.t.}
  @  2,_fco1+10 get NA picture "@KS23"
  @  2,_fco1+34 get in picture "@!KS"+str(min(7,len(in)),1)
  @  4,_fco1+4  get jem picture "@K"
  @  4,_fco1+11 get prz picture "@K 9999" valid prz#0 .or.alarm("MUSI BY� R�NY OD ZERA",,3,3)=NIL
  @  4,_fco1+16 get jed picture "@K"
  @  4,_fco1+34 get gra PICTURE "@K 9999.9"
#ifdef A_KODY
  @  3,_fco1+len(A_KODY)+4  get kw WHEN .f. COLOR _sbnorm
  if !nowy .and. fieldpos('OPIS')<>0
    getl op PICTURE "@S"+ltrim(str(_fco2-col()-1)) WHEN .f. COLOR _sbnorm
  endif
#else
#ifdef A_ELZ
  cennik->(dbseek(keyp,.f.))
  ce:=cennik->cena
  da:=cennik->data
  @  3,_fco1+10 get ce picture "@KE #####.##" valid {||if(!Empty(ce).and.empty(da),da:=DatE(),),getlist[5]:display(),.t.}
  @  3,_fco1+30 get da
#endif
#endif
if empty(deep)
  greader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
#ifndef A_ELZ
#ifdef A_KODYyy
#define D_KODY +'�'+KOD
#else
#define D_KODY
#endif
  getlist[1]:reader:=getlist[4]:reader:=getlist[5]:reader:=getlist[6]:reader:=getlist[7]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,maxrow(),,1,0,"PRZEGL�D MAGAZYNU SPO�YWCZEGO",{||INDEX+"�"+NAZWA D_KODY;
  +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"�","�");
  +STR(STANY->STAN)+" "+JM},{|k,s D_MYSZ|stanmag(k,s D_MYSZ)},"",.T.}).and.showhead(getlist);
  }),eval(greader,g),setkey(-1,k)}
  getlist[2]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,maxrow(),,1,0,"PRZEGL�D MAGAZYNU SPO�YWCZEGO",{||INDEX+"�"+NAZWA D_KODY;
  +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"�","�");
  +STR(STANY->STAN)+" "+JM},{|k,s|stanmag(k,s)},UpP(trim(na)),.T.}).and.showhead(getlist);
  }),eval(greader,g),setkey(-1,k)}
  getlist[3]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,maxrow(),,1,0,"PRZEGL�D MAGAZYNU SPO�YWCZEGO",{||INDEX+"�"+NAZWA D_KODY;
  +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"�","�");
  +STR(STANY->STAN)+" "+JM},{|k,s|stanmag(k,s)},UpP(trim(in)),.T.}).and.showhead(getlist);
  }),eval(greader,g),setkey(-1,k)}
#endif
endif
  __setproc(procname(0))
return
************
#ifndef A_ELZ
stat func showhead(getlist)

  Na:=LEFT(INDX_MAT->NAZWA,LEN(Na))
  In:=INDX_MAT->INDEX
#ifdef A_KODY
//  kw:=INDX_MAT->kod
#endif

  if INDX_MAT->JM="kg"
     jed:="gram"
     prz:=1000
  elseif INDX_MAT->JM="litr"
     jed:="ml  "
     prz:=1000
#ifdef A_JMALT
  elseif INDX_MAT->jm_opcja="KG"
     jed:="gram"
     prz:=INDX_MAT->przel*1000
  elseif INDX_MAT->jm_opcja="G"
     jed:="gram"
     prz:=INDX_MAT->przel
  elseif INDX_MAT->JM_OPCJA="L"
     jed:="ml  "
     prz:=INDX_MAT->przel*1000
#endif
  elseif INDX_MAT->JM#jem
     jed:=INDX_MAT->JM
     prz:=1
  endif
  Jem:=INDX_MAT->JM
  aeval(getlist,{|g|g:display()},2,5)
return .t.
#endif
***********
stat proc sdok3(_f)
  local skl
  field jedN
  altd()
  if updated()
      keyp:=skladnik
      if startrec=0
         if zawar->(dbseek(keyp))
           keyf9:=keyp
           startrec:=zawar->(recno())
           curprompt:=trim(nazwa)+" "+jmaG
         //@ 5,_fco1+3 say 'F9' color _sbkgr
         endif
      endif
      changed:=.t.
      _fj:=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
    if nowy
      nowy:=.f.
      set order to tag sur_naZ
      if dbseek(UpP(na)) .and. alarm("TAKA NAZWA JU� ISTNIEJE;CZY DOPISA� MIMO WSZYSTKO",{"TAK","NIE"})=2 .OR. empty(NA)
        @ 6,_fco1,7,_fco2 BOX '� ����Ⱥ ' color _sbkgr
        RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*(1+2*_fskip+_frow)*D_REST+1))
        _fpopkey:=.f.
        return
      endif
      @ 6,_fco1,7,_fco2 BOX '� ����Ⱥ ' color _sbkgr
      _fpopkey:=.t.
      SET ORDER TO tag sur_kod
#ifdef A_LPNUM
      go bottom
      skl:=str(val(skladnik)+1,len(skladnik))
#else
      GO lastrec()
      skl:=if(eof(),chr(0)+chr(0),i2bin(bin2w(skladnik)+1))
#endif
      APPEND BLANK
      skladnik:=skl
#ifdef A_DEMO
      poprec:=0
#else
      poprec:=startrec
#endif
    elseif empty(na)
      lock
      select sklad
      set order to tag skl_skl
      select zapot
      set order to tag zap_skl
      if !dbseek(surowce->skladnik) .and. !sklad->(dbseek(surowce->skladnik))
        select zawar
        set order to tag zaw_skl
        dbseek(surowce->skladnik)
#ifdef A_LAN
        delete while skladnik==surowce->skladnik rest FOR reclock()
#else
        delete while skladnik==surowce->skladnik rest
#endif
        unlock
#ifdef A_ELZ
        select cennik
        dbseek(surowce->skladnik,.f.)
#ifdef A_LAN
        delete while skladnik==surowce->skladnik rest FOR reclock()
#else
        delete while skladnik==surowce->skladnik rest
#endif
        unlock
#endif
        select surowce
        delete
      endif
      select surowce
      unlock
      break
    else
      lock
    endif
    nazwa:=na
    indx_maT:=in
    przel:=prz
    jedN:=jed
    jmaG:=jem
    gram:=gra
#ifdef A_KODY
    kod:=kw
    if fieldpos('opis')>0
       opis:=op
    endif
#endif
#ifdef A_ELZ
    select cennik
    if if(dbseek(surowce->skladnik,.f.),cena<>ce .or. data<>da,ce<>0)
       if !eof() .and. da=data .and. ce#cena .or. da<=data .and. ce=cena
          lock
       else
          dbappend()
          skladnik:=surowce->skladnik
       endif
       cena:=ce
       data:=da
       unlock
    endif
    select surowce
#endif
endif

return
***********
stat proc sDOK4(_f,getlist,deep)
static fpstart:=0
  _fnowy:=!skladnik==surowce->skladnik
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
     _fpos:=1
      na:=space(len(elementy->nazwa))
    else
      il:=ilosc
      elementy->(dbseek(zawar->element,.f.))
      na:=elementy->nazwa
      showel(_f)
      if oldrec#0
         startrec:=poprec
         skip
         poprec:=if(skladnik==keyf9,recno(),0)
         go oldrec
      endif
    endif

    @ _fk,_fco1+6 GET na PICTURE "@KS20" VALID {|k,r|if(k:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=elval(_f,@na,@poprec,oldrec,startrec) .and. showel(_f,getlist),setkey(-8,k),r}
    GETL il picture "#####.###" valid {|k|if(k:changed.and.fpstart=0,fpstart:=2,),.t.}
     __setproc(procname(0))
    fpstart:=0
if empty(deep)
     getlist[1]:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||skladnik==surowce->skladnik}))})
#endif
endif
RETURN
********
stat proc f9(g,_f,getlist)
   local r:=elementy->(recno()),curprompt,s:=surowce->(recno()),are:=select()
   oldrec:=recno()
   altd()
   if startrec=0
      poprec:=0
      if surowce(.f.)
         select surowce
         set order to tag sur_kod
         curprompt:=trim(nazwa)+" "+jmaG
         select zawar
         seek surowce->skladnik
         keyf9:=surowce->skladnik
         startrec:=recno()
         if surowce->(select())=are
            poprec:=startrec
#ifdef A_KODY
            varput(getlist,'kw',surowce->kod)
            if surowce->(fieldpos('opis')>0)
               varput(getlist,'op',surowce->opis)
            endif
#endif
            varput(getlist,'gra',surowce->gram)
            updated(.t.)
            @ 6,_fco1,7,_fco2 BOX '� ����Ⱥ ' color _sbkgr
            RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*(1+2*_fskip+_frow)*D_REST+1))
            surowce->(dbgoto(s))
            dbseek(surowce->skladnik)
#ifdef A_LAN
            dbeval({||dbdelete()},{||reclock()},{||skladnik=surowce->skladnik})
#else
            dbeval({||dbdelete()},,{||skladnik=surowce->skladnik})
#endif
            select surowce
            return
         endif
         surowce->(dbgoto(s))
      else
         select surowce
         set order to tag sur_kod
         go s
         select elementy
         go r
         select zawar
         go oldrec
         select(are)
         return
      endif
   else
      go startrec
      keyf9:=skladnik
      select surowce
      set order to tag sur_kod
      dbseek(keyf9)
      curprompt:=trim(nazwa)+" "+jmaG
      go s
      select zawar
   endif
   set relation to element into elementy
   go if(poprec=0,startrec,poprec)
   if szukam({2,min(col(),maxcol()-60),maxrow(),,0,0,curprompt,;
     {||elementy->nazwa+"�"+str(ilosc)+" "+elementy->jedn},;
     {|k,_s|(_sret:=k=13).or.ele(k,_s,.f.)},keyf9})
    set relation to
    poprec:=recno()
    g:killfocus()
    il:=ilosc
    na:=elementy->nazwa
    g:setfocus()
    showel(_f)
    getlist[2]:display()
    startrec:=poprec
    skip
    poprec:=if(skladnik==keyf9,recno(),0)
    updated(g:changed:=.t.)
   else
    set relation to
    elementy->(dbgoto(r))
    poprec:=0
   endif
   go oldrec
RETURN
*********
stat PROC sDOK5(_f)
local totrec
    if (updated() .or. oldrec#0) .and. !((il=0 .or. _fkey=27).and. _fnowy)
      changed:=.t.


        if _fnowy
          _fnowy:=.f.
          append blank
          SKLADNIK:=SUROWCE->SKLADNIK
#ifdef A_LPNUM
          pozycja:=str(_fi,len(pozycja))
#endif
        endif

      element:=elementy->element
      ILOSC:=IL
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
          replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while SKLADNIK=SUROWCE->SKLADNIK for reclock()
          go totrec
          lock
#else
          replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while SKLADNIK=SUROWCE->SKLADNIK
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
********
stat func showel(_f)
  @ _fk,_fco2-5 SAY elementy->jedn color _sbkgr
return .t.
*****************
func SURowce(upden)
DEFAULT upden TO .t.
SELECT SUROWCE
SET ORDER TO tag sur_naZ
select zawar
set order to tag "zaw_skl"
#ifdef A_ELZ
select cennik
set order to 1
select surowce
set relation to skladnik into cennik
return SZUKAM({0,maxcol()/2-35,,,1,0,"PRZEGL�D SUROWC�W CENA DATA",{||NAZWA+if(zawar->(found()),"�","!")+jmaG+'�'+str(cennik->cena)+'�'+dtoc(cennik->data)},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#else
select surowce
set relation to skladnik into zawar
#ifdef A_KODY
return SZUKAM({0,maxcol()/2-30,,,12,0,"PRZEGL�D SUROWC�W",{||KOD+"�"+NAZWA+if(zawar->(found()),"�","!")+jmaG},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#else
return SZUKAM({0,maxcol()/2-20,,,1,0,"PRZEGL�D SUROWC�W",{||NAZWA+if(zawar->(found()),"�","!")+jmaG},{|_skey,_s D_MYSZ|if(upden .and. _skey=13,_skey:=9,),sur(_skey,_s,upden D_MYSZ)},""})
#endif
#endif
*******************
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
znalaz:=SZUKAM({0,,maxrow(),maxcol(),1,len(trim(na)),"PRZEGL�D SUROWC�W",if(alias(sel)="ZAPOT",{||NAZWA+"�"+jmaG+"�"+str(cennik->cena)+'�'+dtoc(cennik->data)},{||NAZWA+"("+jmaG+")�"+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#else
select surowce
su:=recno()
SET ORDER TO tag sur_naZ
#ifdef A_KODY
znalaz:=SZUKAM({0,min(col(),maxcol()-40),maxrow(),,12,len(trim(na)),"PRZEGL�D SUROWC�W",if(alias(sel)="ZAPOT",{||KOD+"�"+NAZWA+"�"+jmaG},{||KOD+"�"+NAZWA+"("+jmaG+")�"+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#else
znalaz:=SZUKAM({0,min(col(),maxcol()-30),maxrow(),,1,len(trim(na)),"PRZEGL�D SUROWC�W",if(alias(sel)="ZAPOT",{||NAZWA+"�"+jmaG},{||NAZWA+"("+jmaG+")�"+surowce->jedN}),{|k,s D_MYSZ|sur(k,s,.t. D_MYSZ)},UpP(trim(na))})
#endif
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
    set order to tag sur_naz
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
       alarm("WYBIERZ CO�:",{"DANIA","ZAPOTRZEBOWANIE","STANY MAGAZYNOWE","CENNIK"},@choice)
    endif
    if choice=1
#else
    if alarm("WYBIERZ CO�:",{"DANIA","ZAPOTRZEBOWANIE","STANY MAGAZYNOWE"},@choice)=1
#endif
       select dania
       set order to tag dan_kod
       select sklad
       set order to tag skl_skl
       set relation to danie into dania
       dbseek(surowce->skladnik,.f.)
       szukam({1,min(maxcol()-60,col()+5),maxrow(),,0,0,'Danie',{||dania->(nazwa+if(""=opis,"�","&")+left(dieta,A_DILTH)+"�"+gramatura+" "+jedn)},{|_skey,_s D_MYSZ|_skey:=if(_skey=13,9,_skey),danszuk(_skey,_s,upden D_MYSZ)},surowce->skladnik})
    elseif choice=2
       select zapot
       set order to tag zap_skl
        set relation to RELEWY->(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,'')) into relewy
       dbseek(surowce->skladnik,.f.)
       szukam({1,min(maxcol()-27,col()+5),maxrow(),,1,6,"data�����ile "+trim(surowce->jmaG),{||tran(Dtos(data)+posilek+dieta,"@R ####.##.##|X|"+REPLICATE("X",A_DILTH))+"�"+str(ILOSC)},{|k,s|RELe(k,s,upden)},surowce->skladnik+left(dtos(mies_rob),6)})
    elseif choice=3
       SZUKAM({1,1,maxrow(),,1,0,"PRZEGL�D MAGAZYNU SPO�YWCZEGO",{||INDEX+"�"+NAZWA;
       +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"�","�");
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
#ifdef A_KODY
   case alias()<>'SUROWCE'
      return .f.

   CASE _skey=2 .AND. _sbeg=1 // ^>
    SET ORDER TO tag sur_naz
    _sbeg:=len(kod)+2
    _swar:=&('{|p|'+IndexkeY(0)+'=p'+'}')
    _spform:={|p|tranr(p,"X/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}
    _spform:={|p,l|RIGHT(p,l)}
    _spocz:=''
    _slth:=0
    refresh(1,_s)

   CASE _skey=26 .AND. _sbeg#1 // ^<
    SET ORDER TO tag sur_smb
    _spocz:=''
    _spform:={|p|tranr(p,repl("X",len(kod))+"|"+repl("X",len(nazwa)))}
    _slth:=0
    _sbeg:=1
    _swar:=&('{|p|'+IndexkeY(0)+'=p'+'}')
    refresh(1,_s)
#endif
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
********************
