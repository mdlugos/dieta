#include "dm_form.ch"

field ilosc,jedn,skladnik,nazwa,przel,gram,element,danie,data,;
      posilek,pozycja,ignoruj,dieta,zaw_min,zaw_max

static keyp,ig,nowy,na,jed,za_MI,za_ma

MEMVAR CHANGED,diety,posilki,grupy


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
*************
proc elementy

select elementy
SET ORDER TO tag ele_naz
#ifdef PROC_EN
  #define D_PROC_EN +'�'+tran(ELEMENTY->ignoruj,'Y')
#else
  #define D_PROC_EN
#endif

SZUKAM({0,maxcol()/2-10,,,1,0,"PRZEGL�D ELEMENT�W",{||NAZWA+"�"+jedn D_PROC_EN },{|_skey,_s|if(_skey=13,_skey:=9,),ele(_skey,_s,.t.)},""})
RETURN 
*****************
FUNCTION elVAL(_f,na,poprec,oldrec,startrec)

local su,el,za,znalaz

//memvar na

if na=elementy->nazwa .and. !elementy->(eof())
   return .t.
endif
za:=recno()
su:=surowce->(recno())
select elementy
SET ORDER TO tag ele_naz
el:=recno()
znalaz:=SZUKAM({0,min(col(),maxcol()-30),maxrow(),,1,len(trim(na)),"PRZEGL�D ELEMENT�W",{||NAZWA+"�"+jedn D_PROC_EN},{|k,s|ele(k,s,.t.)},UpP(trim(na))})
set order to tag ele_kod
  select surowce
    SET ORDER TO tag sur_kod
    goto su
  select zawar
    SET ORDER TO tag zaw_skl
    SET RELATION TO
    goto za
if znalaz
  IF _fnowy .and. oldrec#0 .and. elementy->(recno())#el
    poprec:=startrec
  ENDIF
  updated(.t.)
  na:=elementy->nazwa
  return .t.
endif
elementy->(dbgoto(el))
RETURN .F.
**************************
function ele(_skey,_s,upden)
LOCAL N,J,I,L,getlist,prpt
do case
   CASE _SKEY=0
    IF _slth>0 .and. !DBSEEK(_spocz)
      _spocz:=""
      _slth:=0
    ENDIF

#ifdef PROC_EN
    prpt := _sprompt
    _sprompt:={|a,b,c|a:=eval(prpt),if(!empty(c).or.!ELEMENTY->ignoruj,a,(dispout(a,"B" ),""))}
#endif

  case _skey=27
    return .t.

  case _skey=43
      go _srec[_sm]
      _slth=_slth-1
      _spocz=left(_spocz,LEN(_spocz)-1)
    private changed:=.f.
    _skey:=ele_in(!upden,.t.)
    if changed==.t.
      if alias()='ELEMENTY'
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

    _skey:=ele_in(!upden,.f.)

    if changed==.t.
      if alias()='ELEMENTY'
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

/************

      if _si=0
         _srec[1]=recno()
         _sm=1
      endif
    lock
    n:=nazwa
    j:=jedn
    getlist:={}
    @ _srow1+_sm-1,_scol1 get n
    getl j
#ifdef PROC_EN
    l:=ELEMENTY->ignoruj
    getl l picture 'Y'
#endif
    read
    if readkey()#27 .and. updated()
      if empty(n)
         select zawar
         set order to tag zaw_ele
         select elementy
         if !zawar->(dbseek(elementy->element))
            delete
            skip
            if eof()
              skip -1
            endif
         else
            alarm("Nie mo�na wykasowa�, znajduje si� w opisie surowca.",,,3)
         endif
      else
      IF 2=(i:=alarm("CO ROBI�:",{"POPRAWI�","DOPISA�"}))
#ifdef A_LPNUM
        set order to tag ele_kod
        go bottom
        set order to tag ele_naz
        i:=str(val(element)+1,len(element))
#else
        go lastrec()
        i:=if(eof(),chr(0)+chr(0),i2bin(bin2w(element)+1))
#endif
        append blank
        element:=i
      elseif I=0
        return .f.
      endif
      nazwA:=n
      jedn:=j
#ifdef PROC_EN
      ELEMENTY->ignoruj:=l
#endif
      unlock
      endif
      if ! eval(_swar,_spocz)
        _spocz=LEFT(_spocz,len(_spocz)-_slth)
        _slth=0
      endif
      refresh(,_s)
    endif
**************************/
  CASE _si=0
  case _skey=9
     _skey:=push_stat()
     select surowce
     set order to tag sur_kod
     select zawar
     set order to tag zaw_ele
     set relation to skladnik into surowce
     dbseek(elementy->element,.f.)
       szukam({1,min(col()+5,maxcol()-70),maxrow(),,0,0,'ZAWARTO�� '+elementy->jedn+ ' W SK�ADNIKACH',{||surowce->(nazwa+"�"+str(zawar->ilosc)+"/"+str(gram)+" "+surowce->jedN)},{|_skey,_s D_MYSZ|_skey:=if(_skey=13,9,_skey),sur(_skey,_s,.f. D_MYSZ)},elementy->element})
     pop_stat(_skey)

   case _skey=13
        return _sret:=.t.
   case _skey=-8
      _sfil(_s)
      
   case _skey=-9
      _slist(".\"+left(alias(),3)+"*.frm",_s)

endcase
return .f.
***********
func zaw_ar(atot,ilo,dan,dat,ign)

//memvar maxcol(),maxrow()

field data,posilek,danie,dieta

local aret:={},d,g,stat,b,i,j,k  //mes:=message("Chwileczk� ")

stat:=push_stat()

select elementy
set order to tag ele_kod
if pcount()>1

select zawar              //******
set order to tag zaw_skl
set relation to element into elementy

if ilo=NIL

   select surowce
   set order to tag sur_kod

if dan=NIL

if len(dat)<9
   d:=pad(dat,1)
   aczojs(diety,@d,@i,,"Wybierz:")
   if i=0
      pop_stat(stat)
      //message(mes)
      return {}
   endif
#ifdef A_GREX
   g:=pad(subs(dat,2,2),2)
   if g='/' .and. g>='/0'
      g:=subs(g,2,1)
   else
      g:=' '
   endif
   aczojs(grupy,@g,@j,,"Wybierz:")
   if j=0
      pop_stat(stat)
      //message(mes)
      return {}
   endif
#endif
else

   select zapot
   set order to tag zap_rel
   d:=pad(subs(dat,10,1),1)
   aczojs(diety,@d,@i,,"Wybierz:")
   if i=0
      pop_stat(stat)
      //message(mes)
      return {}
   endif
#ifdef A_GREX
   g:=pad(subs(dat,11,2),2)
   if g='/' .and. g>='/0'
      g:=subs(g,2,1)
   else
      g:=' '
   endif
   aczojs(grupy,@g,@j,,"Wybierz:")
   if j=0
      pop_stat(stat)
      //message(mes)
      return {}
   endif
   d:=d+'/'+g
   aret:={TRim(subs(diety[i],2))+'/'+Trim(subs(grupy[j],2))}
#else
   aret:={TRim(subs(diety[i],2))}
#endif

   dbseek(dat:=left(dat,9),.f.)

   do while dtos(data)+posilek=dat
      if dind(d,dieta) .and. relewy->(dbseek(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,d),.f.)) .and. (relewy->ile_pos<>0)
         i:=ipcalc(dieta)
         if i<>0
           surowce->(dbseek(zapot->skladnik,.f.))
           zawar->(mal(atot,zapot->ilosc*surowce->przel/i,ign))
         endif
      endif
      skip
   enddo
endif
else

   select sklad              //******
   set order to tag skl_dan
   set relation to skladnik into surowce
   seek dan

   exec zawar->(mal(atot,sklad->ilosc,ign)) rest while danie==dan

endif

else

    mal(atot,ilo,ign)

endif
endif

select elementy

#ifdef PROC_EN
 k:=0
 for i:=1 to len(PROC_EN)
   b:=PROC_EN[i,1]
   j:=ascan(atot,{|x|x[1]=b})
   b:=if(j=0,0,atot[j,2])*PROC_EN[i,2]
   PROC_EN[i,4]:=b
   k+=b
 next i
 if k<>0
 for i:=1 to len(PROC_EN)
   b:=PROC_EN[i,3]
   if (.f.==ign) .or. (dbseek(b,.f.) .and. !ELEMENTY->ignoruj)
     j:=ascan(atot,{|x|x[1]=b})
     if j=0
       aadd(atot,{b,0})
       j:=len(atot)
     endif
     atot[j,2]:=PROC_EN[i,4]*100/k
   endif
 next i
 endif
 asort(atot,,,{|x,y|x[1]<y[1]})
 aeval(atot,{|x|dbseek(x[1],.f.),aadd(aret,if(empty(jedn),'*'+str(val(x[1])%100,2)+' '+subs(nazwa,2),nazwa+if(jedn='%',str(x[2],4)+" "+'%',str(x[2],10,3)+" "+jedn)))})
#else
 asort(atot,,,{|x,y|x[1]<y[1]})
 aeval(atot,{|x|dbseek(x[1],.f.),aadd(aret,nazwa+str(x[2],10,3)+" "+jedn)})
#endif
#ifdef A_NORMY
 if !empty(d) .and. len(dat)<9
   select normy
   aeval(aret,{|x,y|i:=atot[y,1],dbseek(i),__dblocate({||dind(d,dieta)},{||element=i}),if(found(),if(atot[y,2]<zaw_min,aret[y]+=' <<',if(atot[y,2]>zaw_max,aret[y]+=' >>',)),)})
 endif
#endif

 pop_stat(stat)
 //message(mes)
return aret
***********
proc mal(atot,ilo,ign)
local i,il,a,b,c

   seek surowce->skladnik
   do while skladnik==surowce->skladnik .and. !eof()
      message(100)
      il:=ilo*ilosc/surowce->gram
#ifdef PROC_EN
      if (.f.==ign) .or. elementy->(dbseek(zawar->element,.f.) .and. !elementy->ignoruj)
#endif
        i:=ascan(atot,{|x|x[1]==element})
        if i=0
          aadd(atot,{element,il})
        else
          atot[i,2]+=il
        endif
#ifdef PROC_EN
      endif
#endif
      skip
   enddo
return
**************************
func ele_in(deep,n)
local stat,r,vars:={keyp,ig,nowy,na,jed,za_mi,za_ma}
if keyp#NIL
   deep=.t.
endif
keyp:=element
nowy:=n .or. EOF()

stat:=push_stat()

begin sequence
  select zawar
      set order to tag zaw_ele
  select elementy
      SET ORDER TO tag ele_kod

  dbseek(keyp,.f.)
//  curprompt:=trim(nazwa)+" "+jedn
#ifdef A_NORMY
  select normy
      SET ORDER TO tag nor_ele

 if dbseek(keyp)
    kibord(chr(3))
 endif

  select elementy
#endif
if deep
   inkey()
   if !eof()
    FORM_EDIT({30,min(maxcol(),max(len(nazwa),A_DILTH+17)+40),3,1,999,;
{|f|eDOK1(f)},;
{|f|edok2(f,{})},;
{||setcursor(0)},;
{||DBSELECTAREA("normy"),ordsetfocus("nor_ele")},;
{|f|edok4(f,{},deep)},;
{|f|dok6(f,0)}})
    endif
else
    lock
    FORM_EDIT({30,min(maxcol(),max(len(nazwa),A_DILTH+17)+40),3,1,999,;
{|f|eDOK1(f)},;
{|f,g|edok2(f,g)},;
{|f|edok3(f)},;
{||DBSELECTAREA("normy"),ordsetfocus("nor_ele")},;
{|f,g|edok4(f,g,deep)},;
{|f|edok5(f)}})
endif

end sequence

select elementy
r:=recno()
unlock

pop_stat(stat)

keyp :=vars[1]
ig   :=vars[2]
nowy :=vars[3]
na   :=vars[4]
jed  :=vars[5]
za_mi:=vars[6]
za_ma:=vars[7]
RETURN r
**************
stat pROC eDOK1(_f)
  SET CURSOR OFF
  SET COLOR TO (_SBKGR)
  @ 0,_fco1,3,_fco2 BOX '�ͻ���Ⱥ '
  @ 1,_fco1+12 say "element"
#ifdef PROC_EN
  @ 1,_fco2-8 say "ignoruj"
#endif
#ifdef A_NORMY
  @ 3,_fco1,5,_fco2 BOX '�͹���Ⱥ '
  @ 0,_fco1+1 say "NORMY ELEMENT�W POKARMOWYCH"
#endif

RETURN
***********
stat proc edok2(_f,getlist)
  local now
  select elementy
  if !nowy
     lock
  endif
  na:=nazwa
  jed:=jedN
  ig:=ELEMENTY->ignoruj
  now:=if(nowy,"NOWY   ","POPRAWA")
  @ 1,_fco1+2 get now picture "@K" valid {||nowy:=if(now=" ",!nowy,nowy),now:=if(nowy,"NOWY   ","POPRAWA"),.t.}
  @ 2,_fco1+2 get NA picture "@KS"+ltrim(str(_fco2-_fco1-9))
  getl jed picture "@K"
#ifdef PROC_EN
  getl ig picture "@Y"
#endif
#ifdef A_NORMY
  @ 3,_fco1+5 say "dieta" color _sbkgr
  @ 3,_fco1+10+A_DILTH say "min" color _sbkgr
  @ 3,_fco1+20+A_DILTH say "max" color _sbkgr
#endif
  __setproc(procname(0))
return
************
stat proc edok3(_f)
  local ele
  field jedN
  if updated() .or. nowy
      keyp:=element
      normy->(dbseek(keyp))
      changed:=.t.
      _fj=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
    if nowy
      nowy:=.f.
      set order to tag ele_naZ
      if dbseek(UpP(na)) .and. alarm("TAKA NAZWA JU� ISTNIEJE;CZY DOPISA� MIMO WSZYSTKO",{"TAK","NIE"})=2 .OR. empty(NA)
	@ 4,_fco1,5,_fco2 BOX '� ����Ⱥ ' color _sbkgr
	RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*(1+2*_fskip+_frow)*D_REST+1))
        _fpopkey:=.f.
        return
      endif
      @ 4,_fco1,5,_fco2 BOX '� ����Ⱥ ' color _sbkgr
      _fpopkey:=.t.
      SET ORDER TO tag ele_kod
#ifdef A_LPNUM
      go bottom
      ele:=str(val(element)+1,len(element))
#else
      GO lastrec()
      ele:=if(eof(),chr(0)+chr(0),i2bin(bin2w(element)+1))
#endif
      APPEND BLANK
      element:=ele
    elseif empty(na)
      lock
      select zawar
      set order to tag zaw_ele
      if !dbseek(elementy->element)
#ifdef A_NORMY
	select normy
	set order to tag nor_ele
	dbseek(elementy->element)
#ifdef A_LAN
	delete while element==elementy->element rest FOR reclock()
#else
	delete while element==elementy->element rest
#endif
	unlock
#endif
	select elementy
        delete
      endif
      select elementy
      unlock
      break
    else
      lock
    endif
    nazwa:=na
    ELEMENTY->ignoruj:=ig
    jedN:=jed
endif
#ifndef A_NORMY
    break
#endif
return
***********
stat proc eDOK4(_f,getlist,deep)
  _fnowy:=!element==elementy->element
#ifdef A_LAN
  if !_fnowy .and. _fpopkey
    lock
  endif
#endif

    if _fnowy
     if !_fpopkey .and. _fi>1
       if !deep
	  kibord(chr(27))
       endif
       return
     endif
     za_mi:=za_ma:=0
     _fpos:=1
      na:=space(A_DILTH)
    else
      za_mi:=zaw_min
      za_ma:=zaw_max
      na:=dieta
    endif

    @ _fk,_fco1+6 GET na PICTURE "@KS"+ltrim(str(A_DILTH)) valid {|g|dival(g)}
    GETL za_mi picture "#####.###"
    GETL za_ma picture "#####.###"
     __setproc(procname(0))
#ifdef A_LPNUM
    setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||element==elementy->element}))})
#endif
RETURN
********
stat PROC eDOK5(_f)
local totrec
    if updated() .and. !((za_ma=0 .and. za_mi=0 .or. _fkey=27).and. _fnowy)
      changed:=.t.


        if _fnowy
          _fnowy:=.f.
          append blank
	  element:=elementy->element
#ifdef A_LPNUM
          pozycja:=str(_fi,len(pozycja))
#endif
        endif

      dieta:=na
      zaw_min:=za_mi
      zaw_max:=za_ma
      if za_mi=0 .and. za_ma=0
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
	  replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while element=elementy->element for reclock()
          go totrec
          lock
#else
	  replace pozycja with str(val(pozycja)-1,len(pozycja)) rest while element=elementy->element
          go totrec
#endif
          endif
#endif
        endif
    endif
    unlock
RETURN
**************************
