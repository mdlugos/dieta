#include "dm_form.ch"

field ilosc,jedn,jmaG,skladnik,nazwa,indx_maT,przel,gram,element,danie,data,;
      posilek,cena,pozycja,kod,ignoruj
MEMVAR CHANGED,diety,posilki,grupy

static poprec,oldrec,keyp,startrec,curprompt,nowy,na,in,jem,prz,jed,gra,il,ce,da,keyf9,kw
*************
proc elementy

select elementy
SET ORDER TO tag ele_naz
#ifdef PROC_EN
  #define D_PROC_EN +'³'+tran(ignoruj,'Y')
#else
  #define D_PROC_EN
#endif

SZUKAM({0,maxcol()/2-10,,,1,0,"PRZEGL¤D ELEMENTàW",{||NAZWA+"³"+jedn D_PROC_EN },{|_skey,_s|if(_skey=13,_skey:=9,),ele(_skey,_s,.t.)},""})
RETURN 
*****************

func sur_in(deep,n)

local stat,r,vars:={poprec,oldrec,keyp,startrec,curprompt,nowy,na,in,jem,prz,jed,gra,il,ce,da,keyf9,kw}
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
{|f|sdok2(f,{})},;
{||setcursor(0)},;
{||DBSELECTAREA("zawar"),ordsetfocus("zaw_skl")},;
{|f|sdok4(f,{},deep)},;
{|f|dok6(f)}})
    endif
else
    lock
    FORM_EDIT({20,61,5,1,999,;
{|f|sDOK1(f)},;
{|f,g|sdok2(f,g)},;
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
//#ifdef A_ELZ
ce:=vars[14]
da:=vars[15]
keyf9:=vars[16]
//#endif
kw:=vars[17]
RETURN r
**************
stat pROC sDOK1(_f)
	SET CURSOR OFF
	SET COLOR TO (_SBKGR)
  @ 0,_fco1,4,_fco2 BOX 'ÉÍ»ºº ºº '
  @ 5,_fco1,7,_fco2 BOX 'ÌÍ¹º¼ÍÈº '
  @ 5,_fco1+3 say 'F9'
  @ 0,_fco1+1 say "ZAWARTO— ELEMENTàW"
  @ 1,_fco1+9 say "skˆadnik"
  @ 1,_fco1+25 SAY 'kod magazynu'
#ifdef A_KODY
  @  3,_fco1+2  say A_KODY+':'
#else
#ifdef A_ELZ
  @ 3,_fco1+5 say 'cena            wa¾na od'
#endif
#endif
  @ 4,_fco1+2 say '1      =          , zawarto˜† w     :'

RETURN
***********
#ifdef A_MYSZ
#define D_MYSZ ,bx,cx,dx,myszflag
#else
#define D_MYSZ
#endif
stat proc sdok2(_f,getlist)
  local now
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
#endif
  @  2,_fco1+2  get now picture "@K" valid {||nowy:=if(now=" ",!nowy,nowy),now:=if(nowy,"NOWY   ","POPRAWA"),.t.}
  @  2,_fco1+10 get NA picture "@KS23"
  @  2,_fco1+34 get in picture "@!KS"+str(min(7,len(in)),1)
#ifdef A_KODY
  @  3,_fco1+len(A_KODY)+4  get kw picture "@K"
#else
#ifdef A_ELZ
  cennik->(dbseek(keyp,.f.))
  ce:=cennik->cena
  da:=cennik->data
  @  3,_fco1+10 get ce picture "@KE #####.##" valid {||if(!Empty(ce).and.empty(da),da:=DatE(),),getlist[5]:display(),.t.}
  @  3,_fco1+30 get da
#endif
#endif
  @  4,_fco1+4  get jem picture "@K"
  @  4,_fco1+11 get prz picture "@K 9999" valid prz#0 .or.alarm("MUSI BY Rà½NY OD ZERA",,3,3)=NIL
  @  4,_fco1+16 get jed picture "@K"
  @  4,_fco1+34 get gra PICTURE "@K 9999"
#ifndef A_ELZ
#ifdef A_KODY
#define D_KODY +'³'+KOD
#else
#define D_KODY
#endif
  getlist[1]:reader:=getlist[4]:reader:=getlist[5]:reader:=getlist[6]:reader:=getlist[7]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,maxrow(),,1,0,"PRZEGL¤D MAGAZYNU SPO½YWCZEGO",{||INDEX+"³"+NAZWA D_KODY;
  +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"ð","³");
  +STR(STANY->STAN)+" "+JM},{|k,s D_MYSZ|stanmag(k,s D_MYSZ)},"",.T.}).and.showhead(getlist);
  }),getreader(g),setkey(-1,k)}
  getlist[2]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,maxrow(),,1,0,"PRZEGL¤D MAGAZYNU SPO½YWCZEGO",{||INDEX+"³"+NAZWA D_KODY;
  +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"ð","³");
  +STR(STANY->STAN)+" "+JM},{|k,s|stanmag(k,s)},UpP(trim(na)),.T.}).and.showhead(getlist);
  }),getreader(g),setkey(-1,k)}
  getlist[3]:reader:=;
  {|g,k|k:=setkey(-1,{|p,g|g:=getactive(),g:changed:=;
  SZUKAM({2,1,maxrow(),,1,0,"PRZEGL¤D MAGAZYNU SPO½YWCZEGO",{||INDEX+"³"+NAZWA D_KODY;
  +IF(WAZNOSC>0 .and. STANY->STAN>0 .and. STANY->DATA_PRZY+WAZNOSC<date(),"ð","³");
  +STR(STANY->STAN)+" "+JM},{|k,s|stanmag(k,s)},UpP(trim(in)),.T.}).and.showhead(getlist);
  }),getreader(g),setkey(-1,k)}
#endif
  __setproc(procname(0))
return
************
#ifndef A_ELZ
stat func showhead(getlist)

  Na:=LEFT(INDX_MAT->NAZWA,LEN(Na))
  In:=INDX_MAT->INDEX
#ifdef A_KODY
  kw:=INDX_MAT->kod
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
  if updated()
      keyf9:=keyp:=skladnik
      if zawar->(dbseek(keyp))
         startrec:=zawar->(recno())
         curprompt:=trim(nazwa)+" "+jmaG
         //@ 5,_fco1+3 say 'F9' color _sbkgr
      else
         //@ 5,_fco1+3 say 'ÍÍ' color _sbkgr
         startrec:=0
      endif
      changed:=.t.
      _fj=0
      _fi:=1
      _flp:=_flpmax
      _fl:=1
    if nowy
      nowy:=.f.
      set order to tag sur_naZ
      if dbseek(UpP(na)) .and. alarm("TAKA NAZWA JU½ ISTNIEJE;CZY DOPISA MIMO WSZYSTKO",{"TAK","NIE"})=2 .OR. empty(NA)
        @ 6,_fco1,7,_fco2 BOX 'º ºº¼ÄÈº ' color _sbkgr
        RESTSCREEN(1+2*_fskip+_frow,_fco1,maxrow(),_fco2,SUBSTR(_fscr,(_fco2-_fco1+1)*2*(1+2*_fskip+_frow)+1))
        _fpopkey:=.f.
        return
      endif
      @ 6,_fco1,7,_fco2 BOX 'º ºº¼ÍÈº ' color _sbkgr
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

    @ _fk,_fco1+6 GET na PICTURE "@KS20" VALID {|k,r|if(k:changed.and.fpstart=0,fpstart:=1,),k:=setkey(-8,NIL),r:=elval(_f) .and. showel(_f,getlist),setkey(-8,k),r}
    GETL il picture "#####.###" valid {|k|if(k:changed.and.fpstart=0,fpstart:=2,),.t.}
     __setproc(procname(0))
  //if startrec#0
     getlist[1]:reader:={|g|setkey(-8,{|p,g|g:=getactive(),p:=setkey(-8,NIL),f9(g,_f,getlist),setkey(-8,p)}),getreader(g),setkey(-8,NIL)}
  //endif
    fpstart:=0
#ifdef A_LPNUM
  setkey(402,{|p,g|g:=getactive(),if(_fnowy.or.updated(NIL),tone(130,3),doinsline(_f,getlist,g,{||skladnik==surowce->skladnik}))})
#endif
RETURN
********
stat proc f9(g,_f,getlist)
   local r:=elementy->(recno()),curprompt,s:=surowce->(recno())
   oldrec:=recno()
   if startrec=0
      poprec:=0
      if surowce(.f.)
         select surowce
         set order to tag sur_kod
         keyf9:=skladnik
         curprompt:=trim(nazwa)+" "+jmaG
         select zawar
         seek keyf9
         startrec:=recno()
         surowce->(dbgoto(s))
      else
         select surowce
         set order to tag sur_kod
         go s
         select elementy
         go r
         select zawar
         go oldrec
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
     {||elementy->nazwa+"³"+str(ilosc)+" "+elementy->jedn},;
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
          skip -1  // po zmianie pozycji byˆby niepewny ukˆad, w dm_form i tak skip
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
FUNCTION elVAL(_f)

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
znalaz:=SZUKAM({0,min(col(),maxcol()-30),maxrow(),,1,len(trim(na)),"PRZEGL¤D ELEMENTàW",{||NAZWA+"³"+jedn D_PROC_EN },{|k,s|ele(k,s,.t.)},UpP(trim(na))})
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
LOCAL N,J,I,L,getlist
do case
   CASE _SKEY=0
    IF _slth>0 .and. !DBSEEK(_spocz)
      _spocz:=""
      _slth:=0
    ENDIF

  case _skey=27
    return .t.
  case _skey=22 .and. alias()="ELEMENTY" .and. upden
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
    l:=ignoruj
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
            alarm("Nie mo¾na wykasowa†, znajduje si© w opisie surowca.",,,3)
         endif
      else
      IF 2=(i:=alarm("CO ROBI:",{"POPRAWI","DOPISA"}))
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
      ignoruj:=l
#endif
      unlock
      endif
      if ! eval(_swar,_spocz)
        _spocz=LEFT(_spocz,len(_spocz)-_slth)
        _slth=0
      endif
      refresh(,_s)
    endif

  CASE _si=0
  case _skey=9
     _skey:=push_stat()
     select surowce
     set order to tag sur_kod
     select zawar
     set order to tag zaw_ele
     set relation to skladnik into surowce
     dbseek(elementy->element,.f.)
       szukam({1,min(col()+5,maxcol()-70),maxrow(),,0,0,'ZAWARTO— '+elementy->jedn+ ' W SKADNIKACH',{||surowce->(nazwa+"³"+str(zawar->ilosc)+"/"+str(gram)+" "+surowce->jedN)},{|_skey,_s D_MYSZ|_skey:=if(_skey=13,9,_skey),sur(_skey,_s,.f. D_MYSZ)},elementy->element})
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

local aret:={},d,g,stat,b,i,j,k  //mes:=message("Chwileczk© ")

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
      aczojs(grupy,@g,@j,,"Wybierz:")
      if j=0
        pop_stat(stat)
        //message(mes)
        return {}
      endif
   endif
   d:=d+'/'+g
   aret:={TRim(subs(diety[i],2))+'/'+Trim(subs(grupy[j],2))}
#else
   aret:={TRim(subs(diety[i],2))}
#endif

   dbseek(dat:=left(dat,9),.f.)

   do while dtos(data)+posilek=dat
      if dind(d,dieta) .and. relewy->(dbseek(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,d),.f.)) .and. (relewy->ile_pos<>0)
         relewy->(dbseek(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,zapot->dieta),.f.))
         surowce->(dbseek(zapot->skladnik,.f.))
         zawar->(mal(atot,zapot->ilosc*surowce->przel/relewy->ile_pos,ign))
      endif
      skip
   enddo
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
   if (.f.==ign) .or. (dbseek(b,.f.) .and. !ignoruj)
     j:=ascan(atot,{|x|x[1]=b})
     if j=0
       aadd(atot,{b,0})
       j:=len(atot)
     endif
     atot[j,2]:=PROC_EN[i,4]*100/k
   endif
 next i
 endif
#endif

 asort(atot,,,{|x,y|x[1]<y[1]})
 aeval(atot,{|x|dbseek(x[1],.f.),aadd(aret,nazwa+str(x[2],10,3)+" "+jedn)})

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
