#define WARTOSC bin2d(field->d_wartosc)
#ifdef A_DRUKCOMP
request __run
#endif
memvar posilki,diety,getlist,firma_n,strona,grupy,dietylong,oprn
#undef P_ROWN
#ifdef A_XPRN
//memvar P_12LPI,P_6LPI,P_SUPON,P_SUPOFF,P_UON,P_UOFF,p_rown,p_coln
memvar  p_rown,p_cpi,p_pcl,P_4XON,P_4XOFF,P_COLN,P_BON,P_BOFF,P_UON,P_UOFF,P_36LPI,P_12LPI,P_8LPI,P_7LPI,P_6LPI,P_SUPON,P_SUPOFF,P_MARGIN,P_PON,P_POFF,P_HALFPAGE,p_port,p_land,p_rownl,p_colnl
#ifdef A_PCL
#define P_PORT eval(p_land,.f.)
#define P_LAND eval(p_land,.t.)
#define P_ROWN if(landscape,p_rownl,p_rown)
#define P_COLN if(landscape,p_colnl,p_coln)
#endif
#else
#ifdef A_PCL
#define P_PCL  .t.
#define P_ROWN if(landscape,40,58)
#define P_COLN if(landscape,113, 78)
#define P_12LPI '&'+'l12D'
#define P_6LPI  '&'+'l6D'
#define P_7LPI  '&'+'l7D'
#define P_8LPI  '&'+'l8D'
#define P_SUPON '(s8V'
#define P_SUPOFF '(s12V'
#define P_UON   '&'+'d0D'
#define P_UOFF   '&'+'d@'
#define P_PORT   if(landscape .and. !(landscape:=.f.),'&'+'l0O','')
#define P_LAND   if(!landscape .and. (landscape:=.t.),'&'+'l1O','')
#define P_BON   "(s3B"
#define P_BOFF  "(s0B"
#define P_MARGIN {|x|'&'+'a'+ltrim(str(x,3))+'L'}
#else
#define P_PCL  .f.
#define P_ROWN  60
#define P_COLN  80
#define P_12LPI '3'
#define P_6LPI  '2'
#define P_7LPI  '1'
#define P_8LPI  '0'
#define P_SUPON 'S'
#define P_SUPOFF 'T'
#define P_UON   '-'
#define P_UOFF   '-0'
#define P_BON   'G'
#define P_BOFF  'H'
#define P_MARGIN {|x|'l'+chr(x)}
#endif
#endif

#define P_EJECT  chr(13)+chr(12)
#ifdef A_PCL
#undef A_15CALI
#define A_15CALI
#ifdef A_XPRN
memvar landscape
#else
static landscape:=.f.
#endif
#endif
#ifdef A_POLOWA
  #define D_ILPIC "@EZ ####.#"
  #define D_ILPOZ1 ,1
#else
  #define D_ILPIC "@E ######"
  #define D_ILPOZ1
#endif

field kod_osoby,nazwisko,ile_pos,grupa,posilek,DATA,d_wartosc,dieta,nazwa,;
gramatura,jedn,danie,skladnik,ilosc,element,przel,gram,opis,pozycja,wart_tot

#define eject   specout(P_EJECT);setprc(0,0)

******************
proc zestawienia
memvar _sbnorm,defa
memvar j,while,for,a,b,c,d,e,f,g,h,skip,getlist,self,buf
field baza,order,nr_zes,nazwa,pola,relacje,druk_proc
local i,l,win,ap,txt,jh,jf,jl,jt,el,x,y,z
memvar menuzest,_snorm
local m
private strona:=0
#ifdef A_WIN_PRN
oprn:=A_WIN_PRN
#command ?  [<explist,...>]         => WOUT( <explist> )
#command ?? [<explist,...>]         => WWOUT( <explist> )
#define qout(x) wout(x)
#define qqout(x) wwout(x)
#endif
#ifndef A_DRUKCOMP
m=menuzest
SET COLOR TO GR+/GR
@ 7,maxcol()/2+8,12,maxcol()/2+26 BOX '�ͻ���Ⱥ'
if !isCOLOR()
   set color to W
endif
@ 17,maxcol()/2-39,19,maxcol()/2+39 BOX '�ͻ���Ⱥ '

if isCOLOR()
   SET COLOR TO W+/GR
endif
SET MESSAGE TO 18 center
@  8,maxcol()/2+9 PROMPT '1. Jad�. dekadowy' MESSAGE 'Wydruk jad�ospisu dekadowego ( lub za dowolny inny okres ).'
@  9,maxcol()/2+9 PROMPT '2. Zapotrzeb.dek.' MESSAGE 'Wydruk zapotrzebowa� dekadowych.'
@ 10,maxcol()/2+9 PROMPT '3. Koszty.       ' MESSAGE 'Koszty �ywienia za dany okres (po imporcie koszt�w !).'
@ 11,maxcol()/2+9 PROMPT '4. Analiza menu. ' MESSAGE 'Zawart�� el. pok. w posi�kach na podst. zapotrzebowa� w podanym okresie .'

SETKEY(4,{||KIBORD(CHR(27)+CHR(24)+CHR(13))})
SETKEY(19,{||NIL})
    MENU TO menuzest
SET KEY 4 TO
SET KEY 19 TO
  SETPOS(menuzest+7,maxcol()/2)

IF menuzest=0
   menuzest=m
   RETURN
ENDIF

m=menuzest

set color to w
@ 20,0 clear
setprc(0,0)
DO CASE

   CASE m=1
    dekada()

  case m=2
    zapdek()

  case m=3
    koszty()

  case m=4
    zawar()

ENDCASE
IF STRONA>0
#else
l:=col()
set color to w
@ 20,0 clear
setpos(7,l)
IF !w_zes(@menuzest)
elseif strona>0
#endif
   eject
   if set(_SET_ALTERNATE,.f.).and. file(a:=set(_SET_ALTFILE,""))
      fview(a)
      ferase(a)
   else
#ifdef A_WIN_PRN
      if valtype(oprn)='O'
         oprn:Destroy()
         oprn:=NIL
         landscape:=.f.
      else
         specout(P_PORT)
      endif
#else
#ifdef A_PCL
      specout(P_PORT)
#endif
#endif
#ifdef A_PRINT
      x:=set(_SET_PRINTFILE,'')
      if ! set(_SET_PRINTFILE)==x .and. File(x)
         A_PRINT(x)
      endif
#endif
   endif
ELSE
   alarm("BRAK TAKICH DANYCH !",,,3)
ENDIF
SET PRINTER off
set printer to
set alternate to
set alternate off
RETURN
*******************
#ifdef A_DRUKCOMP
#undef EVLINE
#define EVLINE(buf,lbl,lc) while j>0 .and. j<=lc .and. (j=1 .or. valtype(buf[j-1])#"C" .or. buf[j-1]<>lbl);
   ;self:=evline(buf,j++,@x);
   ;IF self==NIL;
   ;ELSE;
     ;IF self[3]<>NIL;
        ;x:=Self[3];
        ;PRIVATE &x;
     ;END;
     ;x:=&(self[1]);
   ;END;
;ENDDO
*****************************
func w_zes(it_zesmnu)
memvar _sbnorm,defa,strona,apcomp
memvar j,while,for,a,b,c,d,e,f,g,h,skip,getlist,self,buf
field baza,order,nr_zes,nazwa,pola,relacje,druk_proc
local i,l,win,ap,txt,jh,jf,jl,jt,el,x,y
/*
#ifndef A_XPRN
#define P_ROWN 58
#else
memvar p_rown
#endif
*/
IF !FILE("ZES_DEF.DBF")
   RETURN .f.
ENDIF
sel("zes_def")
if it_zesmnu#NIL
   goto it_Zesmnu
endif
setpos(row()+recno(),col())
if szukam({1,col(),,,0,0,"Zestawienia",{||nr_zes+" "+nazwa}})
   it_zesmnu:=recno()
   ap:=getlines(POLA)
   l:=len(ap)
   do while 1<=l .and. ap[1]="&:"
      (&(subs(ap[1],3)),adel(ap,1),--l)
   enddo
   for i:=1 to l
      ap[i]:=asize(getlines(ap[i],";"),6)
   next
   win:=window(l,60,_sbnorm)
   @ win[1],win[2]+2 say trim(nazwa)
   private getlist:={}
   select 0
   for i:=1 to l
        if ZES_DEF->baza#"MEMVAR"
        &(ap[i,2]):=ZES_DEF->&(ap[i,2])
        endif
        @ win[1]+i,win[2]+1 say padl(ap[i,1],15)
        txt:=GETnew(row(),col()+1,memvarblock(ap[i,2]),ap[i,2])
        if !empty(ap[i,5])
           txt:preblock:=&(ap[i,5])
        endif
        if !empty(ap[i,4])
           txt:postblock:=&(ap[i,4])
        endif
        if !empty(ap[i,3])
           txt:picture:=ap[i,3]
        endif
        if type(ap[i,2])=if(ZES_DEF->baza="MEMVAR","C" ,"M")
           txt:cargo:=.t.
           if empty(txt:picture)
              txt:picture:='@S45'
           endif
        endif
        aadd(getlist,txt):display()
   next
   readmodal(getlist)
   select zes_def
   window(win)
   if readkey()#27
      begin sequence
      if ZES_DEF->baza#"MEMVAR"
         lock
         for i:=1 to l
           FIELD->&(ap[i,2]):=MEMVAR->&(ap[i,2])
         next
         sel(baza,trim(zes_def->order))
         rel(make_subar(getlines(zes_def->relacje),4))
         unlock in zes_def
         go top
      else
         select 0
      endif
      private while:={||!eof()}
      private for
      private skip:={||dbskip(1)}
      private a,b,c,d,e,f,g,h,j,self,buf

      txt:=zes_def->druk_proc
      buf:=zes_def->(recno())
      i:=ascan(apcomp,{|x|x[1]==buf})
      if i#0
        buf:=apcomp[i,2]
      else
        txt:=trim(txt)
        if len(txt)<=12
          if !"."$txt
            txt+=".ppz"
          endif
          i:=findfile(txt)
          if !empty(i)
             buf:=getlines(memoread(i))
          endif
        else
          buf:=getlines(txt)
          txt:=NIL
        endif
        aadd(apcomp,{zes_def->(recno()),buf})
      endif
      l:=len(buf)
      cls
      j:=1
      EVLINE(buf,":HEADER",l)
      if j<2 .or. j>l
         break
      endif
      jh:=j
      __dbLocate( for,while,,,.t.)
      IF eval(while) .and. strona=0
         print()
      ELSE
         strona:=0
      ENDIF
      do while eval(while)
         ++strona
         j:=jh
         EVLINE(buf,":LINES",l)
         if j<2
            exit
         endif
         jl:=j
         do while eval(while) .and. prow()<P_ROWN //.or. !set(_SET_PRINTER)
            j:=jl
            EVLINE(buf,":FOOTER",l)
            if j<2
               exit
            endif
            jf:=j
            eval(skip)
            __dbLocate( for,while,,,.t.)
         enddo
         if !eval(while)
            j:=if(jt=NIL,ascan(buf,{|x|valtype(x)="C".and. x=":TOTAL"})+1,jt)
            exit
         endif
         if jf=NIL
            jf:=ascan(buf,{|x|valtype(x)="C".and. x=":FOOTER"})+1
            if jf<2
               exit
            endif
         endif

         j:=jf

         EVLINE(buf,":TOTAL",l)
         if j<2
            exit
         endif
         jt:=j
         ?? spec(chr(13)+chr(12))
         setprc(0,0)

      enddo
      if strona=0
         break
      endif

      EVLINE(buf,"RETURN",l)
      end sequence
      x:=.t.
   else
      x:=.f.
   endif
else
   x:=.f.
endif
select zes_def
use
return x
********************************
#endif
*******************
procedure wydruk_dok
memvar choicef
local a,l,il,i,j,k,w,rec,flag,key:=dtos(relewy->data),darr,s,txt,dicomplex,mes,cpi,x,posil:={},pos_c
l:=relewy->data
select RELEWY
aeval(posilki,{|x|if(dbseek(dseek(,'data,posilek,dieta',l,left(x,1),'')),aadd(posil,x),)})
dbseek(key)
l:=len(posil)
rec:=array(l)

if l>7
   cpi:=7
elseif l>5
#ifdef A_XPRN
#ifdef A_15CPI
   cpi:=6
#else
   cpi:=if(p_pcl,6,7)
#endif
#else
#ifdef A_15CPI
   cpi:=6
#else
   cpi:=7
#endif
#endif
elseif l>=4
   cpi:=5
else
   cpi:=4
endif

begin sequence
if !set(_SET_ALTERNATE)
  set console off
#ifdef A_WIN_PRN
 oprn:=A_WIN_PRN
#endif
  print()
endif
if ! SET(_SET_CONSOLE)
mes:=message("Prosz� czeka�;TRWA WYDRUK")
endif
?? '     '+padr(firma_n,P_COLN-20) //+"dnia "+dtoc(DatE()) // skip 1/7
?
if "J"$choicef
   select dania
   set order to tag dan_kod
   set relation to
   select MENU
   set order to tag menu_rel
   set relation to danie into dania
   if dbseek(key)
      ? "     Jad�ospis",CDOW(relewy->data),"dnia",relewy->data
      ?
#ifdef A_SCR
      i:=ascan(posil,'D')
      if i<>0
         k:=posil[i]
         adel(posil,i)
         posil[len(posil)]:=k
      endif
      i:=ascan(posil,'P')
      if i<>0
         k:=posil[i]
         adel(posil,i)
         posil[len(posil)]:=k
      endif
#endif
      i:=k:=0
      do while k<l
         ++k
         if i=0
            ?
         endif
         j:=left(posil[k],1)
         IF dbseek(key+j)
            ++i
            ?? spec(P_BON)+padc(subs(posil[k],3),int(P_COLN/2))
            ?? spec(P_BOFF)
            rec[i]:={j,recno(),''}
         elseif k<l
             loop
         ENDIF
         if i=2 .or. k=l
            ?
            flag:=.t.
            do while flag
               if mes <> NIL
                  message(1)
               endif
               ?
               flag:=.f.
               for j:=1 to i
                   go rec[j,2]
                   if rec[j,1]=posilek .and. dtos(data)=key
                      flag:=.t.
                      if empty(rec[j,3])
                      a:=len(trim(dieta))
                      if a>0
                        rec[j,3]:=Trim(dieta)+":"
                        ++a
                      endif
                      select dania
#ifdef A_DODATKI
                      IF menu->ile_pos<>0 .and. Round(menu->ile_pos-ipcalc(menu->dieta),1)<>0
                      a+=len(ltrim(str(menu->ile_pos)))+1
                      ?? rec[j,3]+=if(empty(gramatura+jedn),memoline(nazwa,a:=int(P_COLN/2)-a-1),memoline(nazwa,a:=int(P_COLN/2)-a-10)+gramatura+' '+jedn)+" "+ltrim(str(menu->ile_pos))+"|"
                      else
#endif
                      ?? rec[j,3]+=if(empty(gramatura+jedn),memoline(nazwa,a:=int(P_COLN/2)-a-1),memoline(nazwa,a:=int(P_COLN/2)-a-10)+gramatura+' '+jedn)+"|"
#ifdef A_DODATKI
                      endif
#endif
                      if empty(memoline(nazwa,a,2))
                        select menu
                        skip
                        rec[j,2]:=recno()
                        rec[j,3]:=''
                      else
                        rec[j,3]:=memoline(nazwa,a,2)
                        select menu
                        rec[j,2]:=recno()
                      endif
                      else
                        ?? pad(rec[j,3],int(P_COLN/2)-1)+'|'
                        skip
                        rec[j,2]:=recno()
                        rec[j,3]:=''
                      endif
                   else
                      ?? space(int(P_COLN/2)-1)+'|'
                   endif
               next
            enddo
            ?
            i:=0
         endif
      enddo
   endif
endif
if "O"$choicef
select relewy
set relation to
select osoby
set order to tag osob_kod
set relation to
select main
set order to tag main_rel
set relation to
if dbseek(key)
   if mes <> NIL
      message(1)
   endif
   darr:={}
   s:=0
   il:=array(l,1+len(diety))
//#ifdef A_GREX
   exec aadd(darr,{kod_osoby+chr(ascan(diety,dieta)+64)+chr(ascan(grupy,grupa)+64),recno()}) rest while dtos(data)=key
   asort(darr,,,{|x,y|x[1]<y[1]})
   txt:=""
   i:=""
   j:=""
   aeval(darr,{|x,y|if(txt#(txt:=x[1]),++s,),y:=right(x[1],1),if(y$i,(++s,i+=y),),y:=subs(x[1],-2,1),if(y$j,(++s,j+=y),)})
/*
#else
   exec aadd(darr,{kod_osoby+chr(ascan(diety,dieta)+64),recno()}) rest while dtos(data)=key
   asort(darr,,,{|x,y|x[1]<y[1]})
   txt:="",i:=""
   aeval(darr,{|x,y|if(txt#(txt:=x[1]),++s,),y:=right(x[1],1),if(y$i,(++s,i+=y),)})
#endif
*/
   ?? ccpi(4)
   if s+prow()+5>P_ROWN .and. s+10<P_ROWN
#ifdef A_WADO
?
? A_WADO
//"     Sporz�dzi�:                                    Zatwierdzi�:"
#endif
      eject
      ?? '     '+padr(firma_n,P_COLN-20)//+"dnia "+dtoc(DatE()) // skip 1/7
      ?
   endif
    ?
    if prow()<5
      ?? "     Spis os�b korzystaj�cych z wy�ywienia",CDOW(relewy->data),"dnia",relewy->data
    endif
    ? space(if(l<5,5,0))
    ?? ccpi(cpi)+spec(P_UON)+"Spis os�b                 dieta "
    for j=1 to l
        afill(il[j],0)
        afill(rec,0)
        ?? cpad(subs(posil[j],2),10,,3)
    next
    ?? spec(P_UOFF)
    ?? ccpi(4)
//#ifdef A_GREX
    txt:=array(l,len(grupy)+1)
    aeval(txt,{|x|afill(x,0)})
//#endif
    for i=1 to len(darr)
       if mes <> NIL
          message(1)
       endif
       go darr[i,2]
       j:=ascan(posil,posilek)
       k:=ascan(diety,dieta)
       il[j,k+1]+=ile_pos
       il[j,1]+=ile_pos
       rec[j]+=ile_pos
//#ifdef A_GREX
       s:=ascan(grupy,grupa)
       txt[j,s+1]+=ile_pos
       if i=len(darr) .or. darr[i+1,1]#kod_osoby+chr(k+64)+chr(s+64)
/*#else
       if i=len(darr) .or. darr[i+1,1]#kod_osoby+chr(k+64)
#endif*/
          osoby->(dbseek(main->kod_osoby,.f.))
          ? space(if(l<5,5,0))
//#ifdef A_GREX
          ?? ccpi(cpi)+cpad(osoby->nazwisko,27,,3),dieta+'/'+grupa+':'
/*#else
          ?? ccpi(cpi)+cpad(osoby->nazwisko,29,,3),dieta+':'
#endif*/
          aeval(rec,{|x|qqout("|"+tran(x,D_ILPIC)+"   ")})
          ?? ccpi(4)
          afill(rec,0)
       endif
    next
   ?? spec(chr(13))
   ?? space(if(l<5,5,0))
   ?? ccpi(cpi)+spec(P_UON)+space(32+10*l)
   ? ccpi(4)+space(if(l<5,5,0))
   ?? ccpi(cpi)+padl("RAZEM:",32)
   aeval(il,{|x|qqout("|"+tran(x[1],D_ILPIC)+"   ")})
   ?? spec(P_UOFF)
   ?? ccpi(4)
   for j=1 to len(diety)
      if ascan(il,{|X|x[j+1]#0})=0
         loop
      endif
      ? space(if(l<5,5,0))
      ?? ccpi(cpi)+padl(subs(diety[j],1),32)
      aeval(il,{|x|qqout("|"+tran(x[j+1],D_ILPIC)+"   ")})
      ?? ccpi(4)
   next
//#ifdef A_GREX
   ?? spec(chr(13))
   ?? space(if(l<5,5,0))
   ?? ccpi(cpi)+spec(P_UON)+space(32+10*l)
   ?? spec(P_UOFF)+ccpi(4)
   for j=1 to len(grupy)
      if ascan(txt,{|X|x[j+1]#0})=0
         loop
      endif
      ? space(if(l<5,5,0))
      ?? ccpi(cpi)+padl(subs(grupy[j],1),32)
      aeval(txt,{|x|qqout("|"+tran(x[j+1],D_ILPIC)+"   ")})
      ?? ccpi(4)
   next
//#endif
   ?
endif
endif
if "Z"$choicef
select surowce
set order to tag sur_kod
set relation to
select zapot
set order to tag zap_rel
set relation to
if dbseek(key)
#ifdef A_ELZ
               w:=0
               pos_c:=array(len(posil))
               afill(pos_c,0)
               ++cpi
#endif
               if mes <> NIL
                  message(1)
               endif
          darr:={};txt:={}
          j:=0
#ifdef A_MAGSORT
          exec {if(ascan(txt,skladnik+dieta)=0,aadd(txt,skladnik+dieta),), surowce->(dbseek(zapot->skladnik,.f.)),aadd(darr,{surowce->indx_mat+chr(ascan(posil,posilek)+64)+pozycja,recno(),skladnik+dieta})} rest while dtos(data)=key
#else
#ifdef A_LPNUM
          exec {if(ascan(txt,skladnik+dieta)=0,aadd(txt,skladnik+dieta),),aadd(darr,{chr(ascan(posil,posilek)+64)+pozycja,recno(),skladnik+dieta})} rest while dtos(data)=key
#else
          exec {if(ascan(txt,skladnik+dieta)=0,aadd(txt,skladnik+dieta),),aadd(darr,{chr(ascan(posil,posilek)+64),recno(),skladnik+dieta})} rest while dtos(data)=key
#endif
#endif
          s:=len(txt)
   if s+prow()+5>P_ROWN .AND. prow()>5 // .and. s+10<P_ROWN
#ifdef A_WADO
?
? A_WADO
//"     Wystawi�:                                      Zatwierdzi�:"
#endif
      eject
      ?? '     '+padr(firma_n,P_COLN-20)//+"dnia "+dtoc(DatE()) // skip 1/7
      ?
   endif
   if prow()<5
      ? "     Zapotrzebowanie �ywno�ciowe",CDOW(relewy->data),"dnia",relewy->data
      ?
    else
      ?
      ? "     Zapotrzebowanie �ywno�ciowe"
      ?
    endif
    ?? ccpi(cpi)
     ?
#ifdef A_ELZ
      a:=P_COLN*{5,6,25/3,10,12,15,50/3,20}[cpi]*.1-pcol()-10*l-23
#else
      ?? if(l<5,space(5),'')
      a:=P_COLN*{5,6,25/3,10,12,15,50/3,20}[cpi]*.1-pcol()-10*l-15
#endif
      a:=min(a,len(surowce->nazwa)+5)
      specout(P_UON)
      ?? pad(" sk�adnik       ",a-5)+"Dieta|  ilo�� |j.m."
#ifdef A_ELZ
      ?? "|Warto��"
#endif
          for j=1 to l
              ?? '|'
              ?? cpad(subs(posil[j],3),9,,1)
          next
          ?? '|'
          specout(P_UOFF)
          asort(darr,,,{|a,b|a[1]<b[1]})
          j:=0
          for i:=1 to len(darr)-2
              if (j:=ascan(darr,{|x|darr[i,3]=left(x[3],len(skladnik))},max(i+1,++j)))#0
              //{chr(ascan(posil,posilek)+64)+pozycja,recno(),skladnik+dieta}
                 txt:=darr[j]
                 adel(darr,j)
                 if txt[3]<darr[i,3]
                    --i
                 endif
                 ains(darr,i+1)
                 darr[i+1]:=txt
              endif
          next
          s:=0
          dicomplex:={}
          il:={}

          for i:=1 to len(darr)



          if prow()+5>P_ROWN
             ?? spec(chr(13)+replicate('_',pcol()))
             ?? ccpi(4)
             ? 'dalszy ci�g na nast�pnej stronie'
             eject
             ?? '     '+padr(firma_n,P_COLN-20)
             ?
             ? "     Zapotrzebowanie �ywno�ciowe",CDOW(relewy->data),"dnia",relewy->data
             ? "     dalszy ci�g..."
             ?? ccpi(cpi)
             ?
#ifdef A_ELZ
#else
             ?? if(l<5,space(5),'')
#endif
             specout(P_UON)
             ?? pad(" sk�adnik       ",a)+"|  ilo�� |j.m."
#ifdef A_ELZ
             ?? "|Warto��"
#endif
             for j=1 to l
              ?? '|'
              ?? cpad(subs(posil[j],3),9,,1)
             next
             ?? '|'
             specout(P_UOFF)
          endif




               if mes <> NIL
                  message(1)
               endif
              goto darr[i,2]
              j:=ascan(posil,posilek)
              k:=ascan(dicomplex,dieta)
              if k=0
                 aadd(dicomplex,dieta)
                 aadd(il,array(l))
                 k:=len(dicomplex)
                 afill(il[k],0)
              endif
              il[k,j]+=ilosc
#ifdef A_ELZ
              CENNIK->(dbseek(zapot->skladnik+dtos(relewy->data)))
              IF CENNIK->skladnik=zapot->skladnik
                 pos_c[j]+=ilosc*CENNIK->cena
              ENDIF
#endif
              s+=ilosc
              if i=len(darr) .or. darr[i+1,3]#skladnik
#ifdef A_ELZ
                 ?
#else
                 ? if(l<5,space(5),'')
#endif
                 surowce->(dbseek(zapot->skladnik,.f.))
                 if len(dicomplex)=1
                    ?? cpad(surowce->nazwa,a-len(trim(dicomplex[1]))-1,,1)+" "+trim(dicomplex[1])+str(s,9,3)+" "+surowce->jmaG  //30
                 else
                    ?? cpad(surowce->nazwa,a,,1)+str(s,9,3)+" "+surowce->jmaG  //30
                 endif
#ifdef A_ELZ
                 ?? cennik->(if(skladnik=zapot->skladnik,strpic(cennik->cena*s,8,2,"@E "),space(8)))
                 w+=s*cennik->cena
#endif
                   for k:=1 to len(dicomplex)
                    if ascan(il[k],{|x|x#0})=0
                       loop
                    endif
#ifdef A_ELZ
                    if k#1
                       ? padl(trim(dicomplex[k]),a)+space(22)
                    endif
#else
                    if k#1
                       //? space(if(l<5,a+1,a-4))+pad(dicomplex[k],18)
                       ? padl(trim(dicomplex[k]),if(l<5,a+5,a))+space(14)
                    endif
#endif
                    aeval(il[k],{|X|qqout("|"+tran(x,"@Z #####.###"))})
                    ?? '|'
                    afill(il[k],0)
                 next
                 s:=0
                 dicomplex:={}
                 il:={}
              endif
          next
#ifdef A_ELZ
?? spec(chr(13)+replicate('_',a+22+10*l))
? padl('RAZEM:      '+strpic(w,10,2,"@E "),a+22)
aeval(pos_c,{|x|qqout(strpic(x,10,2,"@E "))})
#endif
          ?
endif
endif
#ifdef A_WADO
? ccpi(4)
? A_WADO
//"     Wystawi�:                                      Zatwierdzi�:"
#endif
#ifdef A_PCL
      specout(P_PORT)
#endif
      eject
end sequence
#ifdef A_WIN_PRN
      if valtype(oprn)='O'
         oprn:Destroy()
         oprn:=NIL
      endif
#endif
#ifdef A_PRINT
      x:=set(_SET_PRINTFILE,'')
      if ! set(_SET_PRINTFILE)==x .and. File(x)
         A_PRINT(x)
      endif
#endif
set print off
set print to
set console on
message(mes)
return
***************
#ifdef A_GOCZ
#ifndef A_WO_JAD
#define A_WO_JAD '  3'
#endif
procedure wydruk_JAD(di,gr,kop,wo)
memvar firma_n,firma_n1
local a,b,c,d,i,x,p:=.f.
DEFAULT kop TO 1
gr:=trim(gr)
di:=trim(di)
a:={''}
b:={''}


if !empty(gr)
 private firma_n:=A_KOMU_N
endif
x:=getlines(memoread('podpis.txt'))

#ifdef A_WIN_PRN
 oprn:=NIL
#endif
if !set(_SET_ALTERNATE)
set console off
#ifdef A_WIN_PRN
 oprn:=.f.
#endif
 print()
endif
if !empty(gr) .and. di>='0' .and. len(di)=2
 wydruk_ja(subs(di,1,1),gr,a,wo,1,.t.)
 wydruk_ja(subs(di,2,1),gr,b,wo,1,.t.)
 d:=if(len(a)<len(b),a,b)
 if max(len(a),len(b))>67-len(x)
    aadd(d,'')
    aeval(getlines(memoread('podpis2.txt')),{|x|aadd(d,x)})
    p:=.t.
 endif
else
 wydruk_ja(di,gr,a,wo,1,.f.)
 d:=a
endif
 c:=min(if(p,68,77-len(x)),max(len(a),len(b)))
 asize(a,c)
 asize(b,c)
 for kop=kop to 1 step -1
 for i:=1 to c
   if i>1
      ?
   endif
   if valtype(a[i])='C'
      ?? a[i]
   endif
   if valtype(b[i])='C' .and. i>7
      ?? ccpi(4,8)
      ?? spec(chr(13))+space(40)+IF(I>12,+'| ','  ')+b[i]
   endif
 next i
   if !p
      ?
      ? memoread('podpis.txt')
   endif
 eject
 next kop
#ifdef A_PRINT
      x:=set(_SET_PRINTFILE,'')
      if ! set(_SET_PRINTFILE)==x .and. File(x)
         A_PRINT(x)
      endif
#endif
 set print off
 set print to
 set console on
return
stat proc wydruk_JA(di,gr,oa,wo,kop,go)
local a:={},b,f,h,i,j,mes,da:=relewy->data,po,txt,x,y,z

#command ? => aadd(oa,'')
#command ? <x> => aadd(oa,<x>)
#command ?? <x> => oa\[len(oa)\]+=<x>
#else
procedure wydruk_JAD(di,gr,kop,wo)
local a:={},b,f,h,i,j,mes,da:=relewy->data,po,txt,x,y,z

if !set(_SET_ALTERNATE)
#ifdef A_WIN_PRN
  oprn:=A_WIN_PRN
#endif
  set console off
  print()
#endif
endif
DEFAULT kop TO 1
IF empty(di) .or. !empty(subs(di,2))
   wo:=NIL
else
di:=trim(di)
ENDIF
#ifdef A_GREX
IF empty(gr)
   wo:=NIL
else
gr:=trim(gr)
ENDIF
#endif
begin sequence
if !SET(_SET_CONSOLE)
mes:=message("Prosz� czeka�;TRWA WYDRUK")
endif
#ifdef A_GOCZ
?? eval(MEMVAR->p_jadinit,di,gr)
? memvar->firma_n1
#else
for kop:=kop to 1 step -1
?? padr(firma_n,P_COLN-15) //+"dnia "+dtoc(DatE())
#endif
if !empty(gr)
? spec(if(P_PCL,"(s1p14V",'')+P_BON)
? subs(grupy[ascan(grupy,gr)],1)
?? spec(P_BOFF+if(P_PCL,"(s12v0P",''))
endif
?
? spec(P_BON)+padc("JAD�OSPIS "+CDOW(DA)+" DNIA "+dtoc(DA),P_COLN)
?? spec(P_BOFF)
?
if !empty(di)
#ifdef A_GOCZ
if di>='0' .and. empty(subs(di,2))
  txt:=dietylong[ascan(diety,left(di,1))]
  ? memoline(txt,35,1,,.t.)
  for i:=2 to mlcount(txt,35,,.t.)
    ? padl(trim(memoline(txt,35,i,,.t.)),35)
  next i
else
  ? 'Dieta: '+di
endif
#else
? 'Dieta:',if(di>='0'.and.empty(subs(di,2)),subs(diety[ascan(diety,left(di,1))],1),di)
#endif
?
endif
select relewy
set relation to
select zapot
set order to tag zap_rel
set relation to
select sklad
set order to tag skl_dan
set relation to
select surowce
set order to tag sur_kod
select elementy
set order to tag ele_kod
select zawar
set order to tag zaw_skl
set relation to
select dania
set order to tag dan_kod
select main
set order to tag main_rel
set relation to
select menu
set order to tag menu_rel
set relation to
for i:=1 to len(posilki)
    po:=left(posilki[i],1)
    if !dbseek(DTOS(da)+po,.f.)
      loop
    endif
    h:=.t.
do while data=da .and. posilek=po
   txt:=''
#ifdef A_GREX
   j:=relewy->(recno())
   relewy->(dbseek(dseek(,'data,posilek,dieta',da,po,'0')))
   relewy->(dbeval({|y|y:=left(dieta,3),if(dind(y,di).and.dind(y,menu->dieta).and.(empty(gr).or.dind(y,'/'+gr)),txt+=','+y,)},{||left(dieta,1)>='0'.and.subs(dieta,2,1)='/'.and.subs(dieta,3,1)>='0'.and.empty(subs(dieta,4))},{||data=da .and. posilek=po}))
   relewy->(dbgoto(j))
   if empty(txt)
      skip
      loop
   endif
#else
   aeval(diety,{|x,y|y:=left(x,1),if(dind(y,di).and.dind(y,dieta),txt+=y,)})
   if empty(txt)
      skip
      loop
   endif
   if !empty(gr)
      select main
      seek dtos(da)+po
      if empty(txt)
         locate for grupa=gr while data=da .and. posilek=po
      else
         locate for grupa=gr .and. dieta$txt while data=da .and. posilek=po
      endif
      select menu
      if !MAIN->(found())
         skip
         loop
      endif
   endif
#endif
   if h
     ?
     ?
     ? spec(P_UON+P_BON)+subs(posilki[i],2)
     ?? spec(P_BOFF+P_UOFF)
#ifdef A_WO_JAD
     if if(valtype(wo)='N',wo>0,.t.=wo)
       x:=if(empty(a),0,a[ascan(a,{|x|x[1]=A_WO_JAD}),2])
       if zapot->(dbseek(dtos(da)+po))
         zaw_ar(a,,,dtos(da)+po+di+'/'+gr)
       else
         y:=recno()
         while data=da .and. posilek=po
#ifdef A_GREX
           if dind(di+'/'+gr,dieta)
              select sklad
              seek menu->danie
              exec zawar->(mal(a,sklad->ilosc,.t.)) rest for dind(di+'/'+gr,dieta) .and. surowce->(dbseek(sklad->skladnik)) while danie==menu->danie
              select menu
           endif
#else
           if dind(di,dieta)
              select sklad
              seek menu->danie
              exec zawar->(mal(a,sklad->ilosc,.t.)) rest for dind(di,dieta) .and. surowce->(dbseek(sklad->skladnik)) while danie==menu->danie
              select menu
           endif
#endif
           skip
         enddo
         go y
       endif
       b:=ascan(a,{|x|x[1]=A_WO_JAD})
       if b<>0 .and. if(valtype(wo)='N',wo%2>0,.t.=wo)
          ?? str(a[b,2]-x,5)+' kcal'
       endif
     endif
#endif
     ?
     h:=.f.
   endif
               if mes <> NIL
                  message(1)
               endif
#ifdef A_GOCZ
  if go=.t.
     ?
  else
#endif
  if di>='0' .and. empty(subs(di,2))
     ? space(12)
  elseif len(trim(dieta))#1
     ? padr(dieta,12)
  else
     ? subs(diety[ascan(diety,trim(dieta))],1,11)+" "
  endif
#ifdef A_GOCZ
  endif
#endif
  select dania
  dbseek(menu->danie,.f.)
#ifdef A_GOCZ
   ?? if(go=.t.,ccpi(5,4)+cpad(nazwa,30,12,1),nazwa)+' '+gramatura+' '+jedn
   ?? ccpi(4)
#else
   ?? nazwa,gramatura,jedn
#endif
  select menu
#ifdef A_DODATKI
  if ile_pos<>0
   ?? str(ile_pos)
  endif
#endif
  skip
enddo
next
#ifdef A_WO_JAD
//if .t.=wo
if if(valtype(wo)='N',wo%4>1,.t.=wo)
?
? 'Warto�� od�ywcza:'
#ifdef A_GOCZ
IF go=.t.
aeval(zaw_ar(a),{|x|aadd(oa,ccpi(5,4)+pad(x,P_COLN*.6))})
//aeval(zaw_ar(a),{|x|aadd(oa,cpad(x,P_COLN/2,12,1))})
ELSE
aeval(zaw_ar(a),{|x|aadd(oa,x)})
ENDIF
#command ? => qout()
#command ? [<List,...>] => qout([<List>])
#command ?? [<xn,...>] => [qqout(<xn>)]
#ifdef A_WIN_PRN
#command ? => wout()
#command ?  [<explist,...>]         => WOUT( <explist> )
#command ?? [<explist,...>]         => [WWOUT( <explist> )]
#endif
#else
aeval(zaw_ar(a),{|x|qout(x)})
#endif
endif
#endif
#ifndef A_GOCZ
#ifdef A_WADO
?
?
? A_WADO
//"Wystawi�:                                      Zatwierdzi�:"
#endif
eject

next kop

#ifdef A_WIN_PRN
      if valtype(oprn)='O'
         oprn:Destroy()
         oprn:=NIL
      endif
#endif
#ifdef A_PRINT
      x:=set(_SET_PRINTFILE,'')
      if ! set(_SET_PRINTFILE)==x .and. File(x)
         A_PRINT(x)
      endif
#endif
set print off
set print to
set console on
#endif
end sequence
message(mes)
return
***************
procedure wydruk_rec

local mes,x
/*
local flag
flag:=1=alarm("Czy podawa� zawarto�� element�w w sk�adnikach ?",{"TAK","NIE"},1)
*/
begin sequence
set console off
print()
mes:=message("Prosz� czeka�;TRWA WYDRUK")
select dania
?? spec(eval(p_margin,5))
?? padr(firmA_n,P_COLN-20)+"dnia "+dtoc(DatE())
?
? "Receptura:",nazwa,gramatura,jedn,dieta
?
select surowce
set order to tag sur_kod
select sklad
set order to tag skl_dan
set relation to skladnik into surowce
seek dania->danie
do while danie==dania->danie
   message(1)
   ? surowce->nazwa,ilosc,surowce->jedN
   skip
enddo
?
if ""#dania->opis
for x:=1 TO mlcount(dania->opis,P_COLN-5)
   ? memoline(dania->opis,P_COLN-5,x)
next x
?
endif
? "Warto�� od�ywcza:"
?
aeval(zaw_ar({},,dania->danie),{|x|qout(space(9)+x)})
eject
end sequence
message(mes)
#ifdef A_WIN_PRN
      if valtype(oprn)='O'
         oprn:Destroy()
         oprn:=NIL
      endif
#endif
#ifdef A_PRINT
      x:=set(_SET_PRINTFILE,'')
      if ! set(_SET_PRINTFILE)==x .and. File(x)
         A_PRINT(x)
      endif
#endif
set print off
set print to
set console on
return
***************
procedure koszty(dokl)
local e,ip,ipw,wp,ipg,wpg,cp,aip,awp,an,aep,wtot,itot,wGR,iGR,egr,i,j,k,l,m,od,do,ak,ag,ad,w,txt,im,ild
memvar posilki,grupy,diety,narzuty
field kod_osoby,posilek,data,ile_pos,grupa,waga
do:=DatE()-day(DatE())
od:=do-day(do)+1
@ 21, 5 say "od" get od picture "@D" valid {||do:=max(od,do),.t.}
@ 21,20 say "do" get do picture "@D" valid {||if(do<od,(do:=od)=NIL,.t.)}
read
if readkey()=27
    return
endif
print()
setprc(P_ROWN,0)
ag:=len(grupy)
ak:=len(posilki)
ad:=len(diety)
wgr:=array(ag,ak,ad)
#ifdef A_NARZUT
an:=array(ag)
afill(an,0)
#endif
aeval(wgr,{|y|aeval(y,{|x|afill(x,0)})})
igr:=aclone(wgr)
aip:=aclone(igr)
awp:=aclone(aip)
SELECT MAIN
#ifdef A_GREX
SET RELATION to RELEWY->(dseek(,'data,posilek,dieta',MAIN->data,MAIN->posilek,MAIN->dieta+'/'+MAIN->grupa)) into relewy
#else
SET RELATION to RELEWY->(dseek(,'data,posilek,dieta',MAIN->data,MAIN->posilek,MAIN->dieta+' ')) into relewy
#endif
*****************************
im:=array(ag)
afill(im,0)
aep:=array(do-od+1,ak,ag)
aeval(aep,{|x|aeval(x,{|x|afill(x,0)})})
SET ORDER TO tag main_rel
seek(dtos(od))
do while !eof() .and. DATA<=do
    if ile_pos<>0
       if relewy->(EOF()).and. 1=alarm("Prosz� zaimportowa� dane z magazynu �ywno�ciowego;brak danych z dnia "+dtoc(data)+" posilek: "+posilek+dieta,{"Przerwij","Kontynuuj"})
         break
       endif
       i:=ascan(grupy,grupa)
       j:=ascan(posilki,posilek)
       if i*j=0 .and.  NIL#alarm("Niedopuszczalne wart�ci p�l GRUPA, posilek",,0,3)
         break
       endif
       l:=data-od+1
       w:=ile_pos*relewy->(WARTOSC)/relewy->ile_pos
       im[i]+=w
       aep[l,j,i]+=w
    endif
    SKIP
enddo
e:=0
aeval(im,{|x,i|if(x=0,,(x:=Round(e+=x,A_ZAOKR),e-=x,im[i]:=x-im[i]))})
aeval(aep,{|x|aeval(x,{|x|;
   w:=x,;
   e:=round(e,2),;
   aeval(x,{|m,i|;
      IF(m=0,,(;
        m:=Round(m+(e+im[i])*.5,A_ZAOKR),;
        e+=w[i]-m,;
        im[i]+=w[i]-m,;
        w[i]:=m-w[i];
      ));
   });
})})
*********************
select osoby
set order to tag osob_kod
SELECT MAIN
SET ORDER TO tag main_kod
go top
do while !eof()
  osoby->(dbseek(txt:=main->kod_osoby,.f.))
  seek txt+DTOS(od)
  if eof()
     exit
  endif
  if txt+DTOS(do)<kod_osoby+DTOS(Data)
     seek txt+"@"
     loop
  endif
  w:=0
  DO WHILE txt+DTOS(do)>=kod_osoby+DTOS(Data) .and. !eof()
    if relewy->(EOF())
       alarm("Prosz� zaimportowa� dane z magazynu �ywno�ciowego;brak danych z dnia "+dtoc(data)+" posilek: "+posilek+dieta)
       break
    endif
    i:=ascan(grupy,grupa)
    j:=ascan(posilki,posilek)
    k:=ascan(diety,dieta)
    if i*j*k=0
       alarm("Niedopuszczalne wart�ci p�l POSILEK, GRUPA, DIETA",,0,3)
       break
    endif
    if ile_pos<>0
      e:=ile_pos*relewy->(WARTOSC)/RELEWY->ile_pos
      l:=data-od+1
      m:=Round(e+aep[l,j,i],A_ZAOKR)
      e-=m
      aep[l,j,i]+=e
    ***********
      aWP[i,j,k]+=m
      if aip[i,j,k]=0
         ++w
      endif
      aIP[i,j,k]+=ILE_POS
    endif
    SKIP
  ENDDO
  if prow()+w+2>P_ROWN
   if strona>0
      specout(ccpi(4)+chr(13)+"")
   endif
   setprc(0,0)
?? padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Koszty �ywienia za okres od",od,"do",do,"w��cznie strona"+str(++strona,3)
?
#ifdef A_NARZUT
   specout(ccpi(5)+p_uon)
? "Nazwisko                    Grupa     Posi�ek     Dieta      Ilo��        Wsad      Narzut"
#else
   specout(P_UON)
? "Nazwisko                    Grupa     Posi�ek     Dieta      Ilo��       Koszt"
#endif
   specout(P_UOFF)
  endif
  ? pad(osoby->nazwisko, P_COLN-53)
  l:=ip:=wp:=cp:=0
  for i:=1 to ag;for j:=1 to ak;for k:=1 to ad
      if aip[i,j,k]#0
/******************
        if awp[i,j,k]<>0
           egr[i,j,k]+=awp[i,j,k]-(awp[i,j,k]:=round(awp[i,j,k]+egr[i,j,k]+e,A_ZAOKR))
           e:=0 //z powodu mo�liwo�ci b��du w pierszym przebiegu, doda� to tutaj i wyzerowa�
        endif
*******************/
        if l<>0
        ? space(P_COLN-53)
        endif
        ?? cpad(subs(grupy[i],1,10)+" "+subs(posilki[j],3,10)+" "+subs(diety[k],1,10),32,10,1),str(aip[i,j,k],6 D_ILPOZ1),strpic(awp[i,j,k],12,A_ZAOKR,"@E ")
#ifdef A_NARZUT
        m:=Round(awp[i,j,k]*100,0)*narzuty[i]/100
        an[i]+=m
        ?? strpic(m,12,A_ZAOKR,"@EZ ")
        cp+=m
#endif
        ip+=aip[i,j,k]
        wp+=awp[i,j,k]
        igr[i,j,k]+=aip[i,j,k]
        wgr[i,j,k]+=awp[i,j,k]
        aip[i,j,k]:=awp[i,j,k]:=0
        ++l
      endif
  next;next;next
  if l=1
     ?? spec(chr(13)+space(P_COLN-20)+P_UON+space(20))
     ?? spec(P_UOFF)
  else
     ? space(P_COLN-20)
     ?? spec(P_BON+P_UON)+str(ip,6 D_ILPOZ1),strpic(wp,12,A_ZAOKR,"@E ")
#ifdef A_NARZUT
     ?? strpic(cp,12,A_ZAOKR,"@EZ ")
#endif
     ?? spec(P_BOFF+P_UOFF)
  endif
  seek txt+"@"
enddo
/********
?
aeval(im,{|x|qqout(str(x*100,3))})
aeval(aep,{|x|qout(),aeval(x,{|x|aeval(x,{|x|qqout(str(x*100,3))})})})
*********/
if strona>0
if prow()+(ag+3)*ak*ad+5>P_ROWN
   specout(ccpi(4)+chr(13)+"")
   setprc(0,0)
?? padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Koszty �ywienia za okres od",od,"do",do,"w��cznie"
?
? "strona"+str(++strona,3)
#ifdef A_NARZUT
   specout(ccpi(5))
#endif
endif
select relewy
seek dtos(od)
i:=NIL
//k:=left(posilki[1],1)
//sum WART_TOT,if(posilek=k .and. i<>(i:=data),1,0) to w,ild for ile_pos<>0 .or. WART_TOT<>0 while data<=do rest
sum WART_TOT,if(i<>(i:=data),1,0) to w,ild for ile_pos<>0 .or. WART_TOT<>0 while data<=do rest
#ifdef A_WAGI
sel('posilki',,,.t.)
#endif
?
? "ca�y okres razem (",str(ild,3),"dni ):",strpic(w,12,A_ZAOKR,"@E "),"z�. - Koszt w/g wyda� z magazynu."
?
? "                                    ilo��     warto��    cena"
#ifdef A_NARZUT
?? "    cena z n.  narzut"
#endif
aip:=awp:=0
wtot:=array(ag,ak)
aeval(wtot,{|x|afill(x,0)})
itot:=aclone(wtot)
for i=1 to ag
   ?
   l:=wp:=ip:=ipw:=wpg:=ipg:=im:=cp:=0
   txt:=''
   for j=1 to ak

#ifdef A_WAGI
      k:=left(posilki[j],1)
      m:=waga/100
      locate for posilek=k
      if grupa<>txt
         ?? strpic(wpg/ipg,7,A_ZAOKR,"@ZE ",.t.)
         cp+=wpg/ipg
         ipw+=ipg*m
         wpg:=ipg:=0
      endif
      txt:=grupa
#endif
      m:=0
      for k=1 to ad
         if igr[i,j,k]#0
            ++m
            ? subs(grupy[i],3,10),subs(posilki[j],3,10),subs(diety[k],1,10),str(igr[i,j,k],6 D_ILPOZ1),strpic(wgr[i,j,k],12,A_ZAOKR,"@E "),strpic(wgr[i,j,k]/igr[i,j,k],7,A_ZAOKR,"@E ",.t.)
            itot[i,j]+=igr[i,j,k]
            wtot[i,j]+=wgr[i,j,k]
         endif
      next k
      if itot[i,j]#0
         ++l
         ip+=itot[i,j]
         wp+=wtot[i,j]
#ifdef A_WAGI
         ipg:=max(ipg,itot[i,j])
         wpg+=wtot[i,j]
#else
         cp+=wtot[i,j]/itot[i,j]
#endif
         im:=max(itot[i,j],im)
         if m>1
            ? subs(grupy[i],3,10),subs(posilki[j],3,10),space(8),str(itot[i,j],8 D_ILPOZ1),strpic(wtot[i,j],12,A_ZAOKR,"@E "),strpic(wtot[i,j]/itot[i,j],7,A_ZAOKR,"@E ",.t.)
         endif
         ?? " *"
      endif
   next j
#ifdef A_WAGI
   ?? strpic(wpg/ipg,7,A_ZAOKR,"@ZE ",.t.)
   cp+=wpg/ipg
   ipw+=ipg*waga/100
#endif
   if l>0
      if l>1
         ?
#ifdef A_WAGI
         ? padl(Trim(subs(grupy[i],1))+" OSOBODNI W/G WAG POSI�KOW:",30),str(ipw,8 D_ILPOZ1),strpic(wp,12,A_ZAOKR,"@E "),strpic(wp/ipw,7,A_ZAOKR,"@E ",.t.)
#ifdef A_NARZUT
         cp:=Round(100*wp/ip,0)*narzuty[i]/100
#endif
#else
         ? padl(grupy[i]+' SUMA CEN POSI�K�W:',30),str(ip,8 D_ILPOZ1),strpic(wp,12,A_ZAOKR,"@E "), strpic(cp,7,A_ZAOKR,"@E ",.t.)
         ? padl("MAKSYMALNA ILO�� DZIENNIE:",30),str(im,8 D_ILPOZ1),strpic(wp,12,A_ZAOKR,"@E "),strpic(wp/im,7,A_ZAOKR,"@E ",.t.)
#ifdef A_NARZUT
         cp:=Round(100*cp*narzuty[i],0)/100
#endif
#endif
      endif
      ?? " *"
      aip+=ip
      awp+=wp
#ifdef A_NARZUT
      ?? strpic(cp,7,A_ZAOKR,"@ZE ",.t.)
      ?? strpic(an[i],12,A_ZAOKR,"@ZE ")
#endif
   endif
next i
#ifdef A_WAGI
use
select relewy
#endif
? spec(P_BON)+padl("SUMA WSZYSTKICH POSI�K�W:",30),str(aip,8 D_ILPOZ1),strpic(awp,12,A_ZAOKR,"@E "),"***"
?? spec(P_BOFF)
? "R��nica zaokr�gle�:",strpic(w,12,A_ZAOKR,"@E "),"-",strpic(awp,12,A_ZAOKR,"@E "),"=",strpic(w-awp,12,A_ZAOKR,"@E ")
endif
return
******************
procedure zapdek
local od,do,da,dni,l:=len(diety),t,i,d,flag,key,key1,p,txt
od:=DatE()-8-day(DatE())%10
do:=od+9
do-=day(do)%10
od-=day(od)%10-1
@ 21, 5 say "od" get od picture "@D" valid {||do:=max(od,do),.t.}
@ 21,20 say "do" get do picture "@D" valid {||if(do<od,(do:=od)=NIL,.t.)}
#ifdef A_GREX
d:=left(diety[1],1)+'/'+left(grupy[1],1)
@ 21,35 SAY "dieta" get d picture "X/X" valid {|g|dival(g)}
#else
d:=left(diety[1],1)
@ 21,35 SAY "dieta" get d picture "X" valid {|g|dival(g)}
#endif
read
if readkey()=27
    return
endif
da:=od
dni:=(do-od)+1
select dania
set order to tag dan_kod

select menu
set order to tag menu_rel
set relation to danie into dania
set filter to dind(d,dieta)

select zapot
set order to tag zap_skl
SET RELATION to RELEWY->(dseek(,'data,posilek,dieta',ZAPOT->data,ZAPOT->posilek,ZAPOT->dieta)) into relewy
set filter to dind(d,dieta)

select surowce
set order to tag sur_naZ

go top
print()
?? padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Zapotrzebowanie dekadowe za okres od",od,"do",do,"w��cznie."
? "na podstawie zapotrzebowa�, na jedn� osob�."
?
? "dieta",d,"Strona",STR(++STRONA,2)
?
?
do while da<=do
   key:=dtos(da)
   if prow()+10>P_ROWN
   setprc(0,0)
?? spec(chr(13)+"")+padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Zapotrzebowanie dekadowe za okres od",od,"do",do,"w��cznie."
? "na podstawie zapotrzebowa�, na jedn� osob�."
?
? "dieta",d,"Strona",STR(++STRONA,2)
?
   endif
   ? spec(P_UON),da,CDOW(da)
   ?? spec(P_UOFF)
   for i:=1 to len(posilki)
      select zapot
      set order to tag zap_rel
      select menu
      key1:=key+left(posilki[i],1)
      if dbseek(key1,.f.) .or. zapot->(dbseek(key1,.f.))
#ifdef A_OKI4W
         specout(ccpi(5))
#else
         specout(ccpi(7))
#endif
         ? subs(posilki[i],3)+": "
         flag:=.f.
         do while key1=dtos(data)+posilek
            if flag
               ?? ", "
            endif
            txt:=trim(dania->nazwa)
#ifdef A_OKI4W
            if len(txt)+pcol()>95
#else
            if len(txt)+pcol()>135
#endif
               ? space(len(posilki[1]))
            endif
            ?? txt
            flag:=.t.
            skip
         enddo
         ?? spec(chr(13)+ccpi(4)+P_UON+space(P_COLN))
         ?? spec(P_UOFF)
#ifdef A_OKI4W
         specout(ccpi(5))
#else
         specout(ccpi(7))
#endif
         ?
      endif
   select zapot
   set order to tag zap_skl
   select surowce
   go top
   flag:=.f.
   do while !eof()
      select zapot
      dbseek(dseek(,'skladnik,data,posilek,dieta',surowce->skladnik,stod(left(key1,8)),subs(key1,9),''))
      sum ilosc/relewy->ile_pos*surowce->przel to t rest while skladnik==surowce->skladnik .and. dtos(data)+posilek=key1
      if t#0
         if flag
            ?? ", "
         endif
         txt:=  trim(surowce->nazwa)+" "+ltrim(strtran(str(t,7,2),".00"))+" "+trim(surowce->jedN)
#ifdef A_OKI4W
         if len(txt)+pcol()>95
#else
         if len(txt)+pcol()>135
#endif
            if prow()>P_ROWN
              setprc(0,0)
?? spec(chr(13)+""+ccpi(4))+padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Zapotrzebowanie dekadowe za okres od",od,"do",do,"w��cznie."
? "na podstawie zapotrzebowa�, na jedn� osob�."
?
? "dieta",d,"Strona",STR(++STRONA,2)
?
? da,CDOW(da),posilki[i],"ci�g dalszy: "
#ifdef A_OKI4W
specout(ccpi(5))
#else
specout(ccpi(7))
#endif
            else
              ?
            endif
         endif
         flag:=.t.
         ?? txt
      endif
      select surowce
      skip
   enddo
   next
   specout(ccpi(4))
   ++da
enddo

return
******************
procedure zawar(ign)
local od,do,da,dni,b,p:=" ",d,atot:={},i,j,k
do:=DatE()-day(DatE())
od:=do-day(do)+1
@ 21, 5 say "od" get od picture "@D" valid {||do:=max(od,do),.t.}
@ 21,20 say "do" get do picture "@D" valid {||if(do<od,(do:=od)=NIL,.t.)}
@ 21,35 say "posi�ek" get p valid p=" " .or. aczojs(posilki)
#ifdef A_GREX
d:=left(diety[1],1)+'/'+left(grupy[1],1)
@ 21,45 say "dieta" get d picture "X/X" valid {|g|dival(g)}
#else
d:=left(diety[1],1)
@ 21,45 say "dieta" get d picture "X" valid {|g|dival(g)}
#endif
read
if readkey()=27
    return
endif
dni:=0
d:=trim(d)
p:=trim(p)

select elementy
set order to tag ele_kod

select zawar
set order to tag zaw_skl

select surowce
set order to tag sur_kod

select relewy
goto 0

select zapot
set order to tag zap_rel

seek dtos(od)

do while !eof() .and. data<=do
   if posilek#p
      if posilek>p
         seek dtos(data)+"~"
      else
         seek dtos(data)+p
      endif
      loop
   endif
   relewy->(dbseek(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,d),.f.))
   if relewy->ile_pos<>0 .and. dind(d,dieta) .and. surowce->(dbseek(zapot->skladnik,.f.))
      if relewy->data<>data .or. relewy->posilek<>posilek .or. relewy->dieta<>dieta
        relewy->(dbseek(dseek(,'data,posilek,dieta',zapot->data,zapot->posilek,zapot->dieta),.f.))
        if data<>da
           ++dni
        endif
        @ 21,55 Say da:=data
      endif
      zawar->(mal(atot,zapot->ilosc*surowce->przel/relewy->ile_pos,ign))
   endif
   skip
enddo

if empty(atot)
   return
endif



print()
strona:=1
?? padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Sk�adniki pokarmowe w posi�kach za okres od",od,"do",do,"w��cznie."
? "na podstawie zapotrzebowa�, na jedn� osob�, �rednio dziennie."
? "dieta:",d
if ""#p
   ?? ', posi�ek:',subs(posilki[ascan(posilki,p)],3)
endif
?

SELECT ELEMENTY
#ifdef PROC_EN
 k:=0
 for i:=1 to len(PROC_EN)
   b:=PROC_EN[i,1]
   j:=ascan(atot,{|x|x[1]=b})
   b:=if(j=0,0,atot[j,2])*PROC_EN[i,2]
   PROC_EN[i,4]:=b
   k+=b
 next i
 for i:=1 to len(PROC_EN)
   b:=PROC_EN[i,3]
   if (.f.==ign) .or. (dbseek(b,.f.) .and. !field->ignoruj)
     j:=ascan(atot,{|x|x[1]=b})
     if j=0
       aadd(atot,{b,0})
       j:=len(atot)
     endif
     atot[j,2]:=PROC_EN[i,4]*100*dni/k
   endif
 next i
#endif
 asort(atot,,,{|x,y|x[1]<y[1]})
 aeval(atot,{|x|dbseek(x[1],.f.),qout(nazwa+' '+str(x[2]/dni,10,3)+' '+jedn),message(100)})

return
******************
proc dekada
local od,do,key:="",key1:="",flag,I,rec,l:=len(posilki),half,a,b
select dania
set order to tag dan_kod
select menu
set order to tag menu_rel
set relation to danie into dania
od:=DatE()+12-day(DatE())%10
do:=od+9
do-=day(do)%10
od-=day(od)%10-1
@ 21, 5 say "od" get od picture "@D" valid {||do:=max(od,do),.t.}
@ 21,20 say "do" get do picture "@D" valid {||if(do<od,(do:=od)=NIL,.t.)}
read
if readkey()=27
    return
endif
#ifdef A_WIN_PRN
  oprn:=A_WIN_PRN
#endif
print()
strona:=1
?? padr(firma_n,P_COLN-15)+"dnia "+dtoc(DatE())
?
? "Jad�ospis za okres od",od,"do",do,"w��cznie"
?
   if l<4
      rec:=array(l)
      half:=array(l)
      ? spec(P_BON)
      for i:=1 to l
         ?? padc(subs(posilki[i],3),26)
      next
      ? spec(P_BOFF)
   endif
seek dtos(od)
do while data<=do .and. !eof()
   key:=dtos(data)
   ? spec(chr(13)+P_BON+P_UON),data,CDOW(data),''
   ?? spec(P_UOFF+P_BOFF)
   if l<4
      for i:=1 to l
        dbseek(key+left(posilki[i],1))
        rec[i]:=recno()
        half[i]:=1
      next
      flag:=.t.
      do while flag
         ?
         flag:=.f.
         for i:=1 to l
            go rec[i]
            if posilki[i]=posilek .and. dtos(data)=key
               a:=memoline(dania->nazwa,20,half[i])
               b:=memoline(dania->nazwa,20,++half[i])
               flag:=.t.
               ?? cpad(trim(dieta),5,,3)+pad(a,20)+"|"
               if empty(b)
                  skip
                  half[i]:=1
               endif
               rec[i]:=recno()
            else
               ?? space(26)
               half[i]:=.f.
            endif
         next
      enddo
 else
   setprc(prow(),pcol()-8)
   for i:=1 to len(posilki)
      key1:=key+left(posilki[i],1)
      if dbseek(key1)
         specout(P_BON)
#ifdef A_DEKDUZE
#define A_OKI4W
#endif
#ifdef A_OKI4W
         ? ccpi(5)
#else
         ? ccpi(7)
#endif
         ?? subs(posilki[i],3)+": "
         specout(P_BOFF)
         //setprc(prow(),pcol()-5)
         flag:=.f.
         do while key1=dtos(data)+posilek
            if flag
               ?? ", "
            endif
#ifdef A_OKI4W
            if len(trim(dania->nazwa))+if(dieta=" ",0,len(Trim(dieta)))+pcol()>P_COLN*1.2
#else
            if len(trim(dania->nazwa))+if(dieta=" ",0,len(Trim(dieta)))+pcol()>P_COLN*1.66
#endif
               ? space(len(posilki[1]))
            endif
            if dieta#" "
               ?? Trim(dieta)+":"
            endif
            ?? trim(dania->nazwa)
            flag:=.t.
            skip
         enddo
         ?? ccpi(4)
      endif
   next
   ?? spec(chr(13)+P_UON+space(P_COLN))
   ?? spec(P_UOFF)
   endif
   seek key+"~"
enddo
#ifndef A_DRUKCOMP
?
? "Sporz�dzi�:       Prze�.Piel�gn.:            Lekarz:          Dyrektor:"
#endif
?? spec(P_6LPI)
return
*****************
