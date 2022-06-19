#ifdef A_EXT
request A_EXT
#endif

//#define SIMPLE
#define NONTXERR
//#define TCVT(x) tran(x,)
#include "error.ch"
#include "inkey.ch"
#ifdef __HARBOUR__
#include "hbgtinfo.ch"
init proc esys()
#else
ANNOUNCE _ntxerr
proc ErrorSys()
#endif


memvar _sbnorm,_sbkgr,_sramka,_sel,_snorm,_slinia,_sunsel,firma_n,firma_a,defa,pcl
local sc:=A_SUMK,a:=A_AUTOR,f:=A_KOMU_N,fa:=A_KOMU_A

local i,n,t

#ifdef SIMPLE
#include "simpleio.ch"
#endif

SET(_SET_DEBUG,.t.)
ErrorBlock( {|e| DefError(e)} )

t:=a+f+fa
n:=int(len(t)/4)*4
for i:=1 to n step 4
  sc+=bin2l(substr(t,i,4))
next

if sc#0
  altd()
  quit
endif

public _sbnorm,_sbkgr,_sramka,_sel,_snorm,_slinia,_sunsel,defa,firma_n:=f,firma_a:=fa

#ifdef __HARBOUR__
  hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
  REQUEST HB_LANG_PL
  HB_LANGSELECT('PL')

  REQUEST HB_CODEPAGE_PL852
  #ifdef PLWIN
   REQUEST HB_CODEPAGE_PLWIN
   request hb_translate
   hb_gtInfo( HB_GTI_FONTNAME , "Lucida Console" )
   hb_gtInfo( HB_GTI_FONTWIDTH, 10  )
   hb_gtInfo( HB_GTI_FONTSIZE , 20 )
   //hb_gtInfo( HB_GTI_WINTITLE , "Rozruch" )
   hb_gtInfo( HB_GTI_ALTENTER, .T. )  // allow alt-enter for full screen
   SetCursor( 0 )
   hb_gtInfo( HB_GTI_CLOSABLE, .t. )
   hb_gtInfo( HB_GTI_CLOSEMODE, 1) //Generates HB_K_CLOSE keyboard event (does not close application)

    #ifdef PC852
     HB_CDPSELECT('PL852')
    #else
     HB_CDPSELECT('PLWIN')
    #endif

  #endif
   //SET(_SET_DEBUG, .t.)
#ifdef A_ADS
#ifdef __PLATFORM__WINDOWS
      #include "ads.ch"
#endif
      REQUEST ADS
      RddRegister( "ADS", 1 )
      rddsetdefault("ADS")

#ifdef A_CDX
      SET FILETYPE TO CDX
#else
      SET FILETYPE TO NTX
#endif
      AdsSetServerType( A_ADS )
      //  /* ADS_LOCAL_SERVER + */ ADS_REMOTE_SERVER )
      SET AXS LOCKING ON
#ifdef PC852
#ifndef __PLATFORM__UNIX
      AdsSetCharType(ADS_OEM,.t.)
#endif
#endif
#else
      REQUEST _DBF
#ifdef A_CDX
      REQUEST A_CDX
      SET RDD DEFAULT TO A_CDX
#else
      REQUEST DBFNTX
      SET RDD DEFAULT TO DBFNTX
#endif
      REQUEST DBFFPT
#endif
   SET HARDCOMMIT ON
#else
#ifdef A_CDX
   SET RDD DEFAULT TO A_CDX
#else
   SET RDD DEFAULT TO DBFNTX           // Set up DBFNTX as default driver
#endif
#endif

#ifdef A_LAN
set exclusive off
#endif

SET SOFTSEEK ON                        // W calym programie !!!
SET DELETED ON
SET DATE    A_SET_DAT
SET EPOCH TO year(date())-98
SET CENTURY ON

#ifndef A_XPRN
#ifdef A_PCL
public pcl:=.t.
#else
public pcl:=.f.
#endif
#endif
#ifdef SIMPLE
?? firma_n
? firma_a
?
? a
?
#else
#ifdef A_HBGET
    SetKey(K_CTRL_RET,{||Key_Ctrl_Ret(GetActive())},{||GetActive()<>NIL .and. PROCNAME(2)='HBGETLIST:GETAPPLYKEY'})
    SetKey(K_F10,{||hb_keyput(K_CTRL_W),.t.},{||GetActive()<>NIL .and. PROCNAME(2)='HBGETLIST:GETAPPLYKEY'})
    SetKey(K_CTRL_L,{||hb_keyput(K_CTRL_W),.t.},{||GetActive()<>NIL .and. PROCNAME(2)='HBGETLIST:GETAPPLYKEY'})
#endif
//setkey(28,{|x|help(x)})
isega() //musi, bo korzysta z jej wyniku d.asm
SET CURSOR OFF
SET SCOREBOARD OFF
SET CONFIRM ON

SETBLINK(.t.)
if iscolor()   //VGA COLOR
  _sramka:="BG+/B"              // Kolor ramki dla szukam()
  _sel:="I"
  _snorm:="W,I,,W,W+/B"
  _sbkgr:="BG+/br,I,,w+/BR,W+/B"
  _sbnorm:="w+/BR,i,,bg+/br,w+/b"
  _slinia:=_sunsel:="W+/B"
  SET COLOR TO GR+/BG,I,,,W+/B  // _snorm,_sel,_sramka,_sborder,_slinia
else // MDA, VGA MONO
  _sramka:=_sbnorm:=_sbkgr:=_snorm:="W,I,,W,W+"
  _sel:="I"
  _slinia:="U+"
  _sunsel:="W+"
  SET COLOR TO W,I,,W,W+
endif
#ifdef __HARBOUR__
  setmode(min(maxrow()+1,Round((maxcol()+1)*5/16,0)),maxcol()+1)
#endif
INIT SCREEN
CLEAR screen
@ 1,0 say padc("program dla:     "+firma_n+space(17),maxcol())
@ 2,0 say padc(firma_a,maxcol())
@ 16,0 say padc(a,maxcol())
//@ 20,0 say padc("Program napisano w j�zyku CLIPPER 5.01, numer licencji: CDX 218558",maxcol())
@ 20,0 say padc(wersja()+', kompilator: '+version(1),maxcol())
#ifdef A_LAN
@ 22,0 say padc(trim("WERSJA SIECIOWA "+os()+" "+netname()),maxcol())
#else
@ 22,0 say padc(os(),maxcol())
#endif
#ifdef A_DEMO
@ 23,0 say padc(IF(DTOS(date())<A_DEMO,"NA OKRES WDRO�ENIOWY","DLA CEL�W DEMONSTRACYJNYCH"),maxcol())
#endif
@ 18,0 say padc("PROSZ� CHWIL� POCZEKA� !",maxcol()) color "*"+setcolor()
#ifdef A_MYSZ
sysint(51,0)
#else
#ifdef __HARBOUR__
MHIDE()
#endif
#endif
#endif
return


#ifdef PLWIN
PROCEDURE HB_GTSYS()

   REQUEST HB_GT_WVT_DEFAULT
   REQUEST HB_GT_WIN

   RETURN
#endif

/***
*  DefError()
*/

func DefError(e)
local i, cMessage:="", aOptions:={}, nChoice,r,t,n,bk,h,f,a,b,c,d
field nazwa,baza,klucz,path,plik,for,unique,descend
static s:=0,ee:=NIL

// put messages to STDERR
  if e:severity=NIL
     e:severity:=ES_ERROR
  endif

  if ee#NIL .and. e:severity > ES_WARNING
  // display message and traceback
#ifndef SIMPLE
  set color to w
  set cursor on
#endif



// used below
#define NTRIM(n)    ( LTrim(Str(n)) )
  ErrorMessage(e,@cMessage)
  ? cMessage
  ?
  cMessage:=''
  ErrorMessage(ee,@cMessage)
  ? cMessage
  ?
  ? "Stos: "
  i := 2
  while ( !Empty(ProcName(i)) )
    ?? Trim(ProcName(i))+"(" + NTRIM(ProcLine(i)) + "), "
    i++
  end
  ?
  // give up
  ErrorLevel(1)
#ifdef SIMPLE
  quit
#else
  errorinhandler()
#endif
  endif

  ee := e
  if ( e:genCode == EG_ZERODIV )
    ee:=NIL
    return 0 // (e:args[1])
#ifdef A_LAN
  elseif e:genCode == EG_OPEN .AND. e:osCode == 5 .AND. e:subcode==1001 .and. procname(2)=='DBUSEAREA'
    nuse (e:filename) SHARED READONLY
    ee:=NIL
    return (.f.)
  elseIf ( e:genCode == EG_OPEN .OR. e:genCode == EG_CREATE).AND. e:osCode == 55
   __Run( "Echo > Nul")
   //Return (.t.)
    e:description := 'B��d sieci MS-Windows, wybierz "spr�buj"...'
#endif

  elseif ( e:genCode == EG_DATAWIDTH )
    e:description := "WYNIK NIE MIE�CI SI� W POLU BAZY DANYCH !!!;Istnieje du�e prawdopodobie�stwo, �e nast�pi�o rozkojarzenie danych."

  elseif ( e:genCode == EG_LOCK )
#ifndef NONTXERR
    i:=seconds()
    if i-s<3
       ee:=NIL
       return .t.
    endif
    if i-s<5 //drugi raz
  #ifdef SIMPLE
  ?? chr(7)
  #else
  tone(130,5)
  clear typeahead
  #endif
       ALARM("UWAGA SIE�: Nie potrafi� uzyska� prawa zapisu do skorowidza. Prawdopodobnie zablokowany przez innego u�ytkownika. Sprawd�, kto to jest i dlaczego wisi. Pr�buj a� do skutku! Przerwanie programu w takiej sytuacji nie jest bezpieczne dla danych.;Naci�nij Enter aby ponowi� pr�b�.")
    endif
    s:=seconds()
#endif
    ee:=NIL
    return .t.

  elseif ( e:genCode == EG_APPENDLOCK )
    e:description := "Nie potraf� dopisa� nowego rekordu do bazy "+alias()+";Jest zablokowana przez innego u�ytkownika."
    //e:canDefault:=.f.
    //neterr(.t.)


  elseif ( e:osCode = 32 )
    e:description := "Nie mam dost�pu do zbioru.;Jest zablokowany przez inny program."
    //e:canDefault:=.f.
    //neterr(.t.)


  elseif ( e:osCode > 32 )
    e:description := "Nie mam dost�pu do zbioru.;Prawdopodobnie awaria sieci."
    //e:canDefault:=.f.
    //neterr(.t.)

  elseif ( e:osCode == 8 )
    e:description := 'Za ma�o miejsca na dysku.;Nie potrafi� zapisa� zbioru'
    //e:canDefault:=.f.

  elseif ( e:osCode == 4 )
    e:description := 'Za ma�y limit otwartych zbior�w.;Program nie uruchomiony za pomoc� w�a�ciwego pliku "BAT", lub;za ma�a deklaracja ilo�ci zbior�w w sieci, lub;za ma�o zadeklarowanych "FILES" w CONFIG.SYS, lub;za ma�o zadeklarowanych zbior�w w "SET CLIPPER=Fxx" w AUTOEXEC.BAT;wymagania programu: '+A_FILELIMIT+'.;Zbi�r:'
    //e:canDefault:=.f.

  elseif ( e:genCode == EG_PRINT )
  i:=3
  do while "PRINT"$procname(i)
     ++i
  enddo
  #ifdef SIMPLE
  ?? chr(7)
  #else
  tone(130,5)
  clear typeahead
  #endif
#ifdef A_LAN
  nChoice:=alarm('Drukarka nie jest gotowa. Sprawd�, czy jest "On line".;'+;
      'Je�eli skierujesz wydruk do zbioru, to mo�esz go potem;'+;
      'wydrukowa� za pomoca komendy PRINT.',;
      {"Spr�buj","Przerwij","Druk do zbioru"})
      if nChoice > 2
         f:=set(_SET_DEFAULT)
         h:=fcreateu(@f)
         if h#-1
            fclose(h)
            if !"."$right(f,4)
               f+='.'
            endif
         else
            f:=left(procname(i),8)+".PRN"
         endif
      endif
#else
  f:=left(procname(i),8)+".PRN"
  nChoice:=alarm('Drukarka nie jest gotowa. Sprawd�, czy jest "On line".;'+;
      'Je�eli skierujesz wydruk do zbioru, to mo�esz go potem;'+;
      'wydrukowa� za pomoca komendy :;PRINT '+set(_SET_DEFAULT)+f,;
      {"Spr�buj","Przerwij","Druk do zbioru"})
#endif
  do case
    case nChoice = 2 ; SET PRINTER TO ;ee:=NIL; BREAK(e)
    case nChoice = 3 ; SET PRINTER TO (f)
  endcase
    ee:=NIL
    return .t.

#ifdef A_ADS
  elseif  e:subsystem = 'ADS' .and. e:subcode=7041 .and. file("indeks.dbf")
 #ifndef __PLATFORM__LINUX
          AdsRegCallBack( {|nPercent|dispout(replicate( "�", int(npercent/100*(f[4]-f[2]-5))-(i-f[2]))),message(1),i:=col(),dispout(str(nPercent,3)+'%'),setpos(row(),i),.f.}  )
 #endif

    cMessage:=lower(alias())
    sel("indeks")
    n:=recno()
    go top
    locate for expand(b:=Lower(trim(BAZA))) == CMESSAGE
    Do while Found()
 #ifdef A_CDX
      d:=Expand(Lower(trim(nazwa)))
 #else
      locate while Lower(trim(BAZA))==b FOR {||d:=Expand(Lower(trim(nazwa))),(HB_ps()+d+'.')$(HB_ps()+Lower(e:filename)+'.')  }
      IF !found()
         Exit
      endif
 #endif
          if empty(nazwa) .or. empty(klucz)
             skip
             loop
          endif
          a:=trim(klucz)

          f:=MESSAGE("Odtwarzanie skorowidza "+d+", baza: "+expand(b)+".DBF, klucz: "+expand(a)+";.")
          setpos(f[3]-1,f[2]+1)
          select (cMessage)
 #ifndef A_CDX
          c:=e:filename
          c:=left(c,rat(HB_ps(),c))
          nchoice:=0 ; while !empty(ordbagname(++nchoice)) ; aadd(aoptions,c+ordbagname(nchoice)) ; enddo
 #endif
          c:=trim(indeks->for)
          if empty(c)
             ordCondSet(,,,,{||dispout("�"),message(1),.t.},int(1+lastrec()/(f[4]-f[2]-2)),RECNO(),,,,indeks->descend)
          else
             ordCondSet( expand(c),,,,{||dispout("�"),message(1),.t.},int(1+lastrec()/(f[4]-f[2]-2)),RECNO(),,,,indeks->descend)
          endif
          i:=f[2]
          //a:=strtran(expand(a),'UPP(','UPPER(')

 #ifdef A_CDX
          ordCreate(,d,a,,indeks->unique)
          Message(f)
          dbselectar('INDEKS')
          CONTINUE
 #else
          ordCreate(d,,a,,indeks->unique)
          Message(f)
          ordlistclear() ; aeval(aoptions,{|x|ordlistadd(x)})
          ordlistadd(d)
          exit
 #endif
    enddo
    indeks->(dbgoto(n))
 #ifndef __PLATFORM__LINUX
          AdsClrCallBack()
 #endif
 #ifdef A_CDX
    select (cMessage)
 #endif
    ee:=NIL
    return .f.

#else

  elseif e:subsystem = 'DB' .and. (e:subcode=1050 .or. lower(right(e:filename,1))="x" .and. ( e:subcode = 1003 .or. e:subcode = 1012 )) .and. file("indeks.dbf")
    if !e:canRetry.and.!e:candefault.and.lower(right(e:filename,1))="x"
        CLOSE DATABASES
        if ferase(e:filename)=0
#ifndef SIMPLE
        clear typeahead
#endif
        alarm("Uszkodzony skorowidz "+e:filename+" zosta� usuni�ty;Prosz� uruchomi� program jeszcze raz.")
        endif
        errorinhandler()
    endif
    if procname(2)='DBUSEAREA'
       //e:candefault:=.f.
       ee:=NIL
       return .f.
    endif

    cMessage:=alias()
    r:=select("indeks")
    if r=0
       sel("indeks")
    else
       select (r)
       n:=recno()
    endif
    locate for expand(b:=trim(BAZA)) == CMESSAGE .and. !empty(INDEKS->klucz)
#ifdef A_ADS
#define D_CDX
#else
#ifdef A_CDX
#define D_CDX
#endif
#endif

#ifdef D_CDX
    if found()
    ******
          a:=''
          if !empty(indeks->path)
             a:=trim(indeks->path)
             IF a="&:"
                a:=trim(&(subs(a,3)))
             ENDIF
             a:=expand(a)
             if right(a,1)<>HB_ps()
                a+=HB_ps()
             endif
          endif
          if fieldpos("PLIK")=0 .or. empty(indeks->plik)
             a+=Lower(b)
          else
             a+=trim(indeks->plik)
          endif
          t:=findfile(expand(a)+".dbf")
          t:=left(t,rat(".",t)-1)
 #ifdef A_CDX
          if lower(e:filename)#lower(t+ordbagext())
             e:filename:=Lower(t+ordbagext())
          endif
 #endif
       ******
       select (cMessage)
       h:=shared()
       if h
          bk:=get_relat()
          ee:=NIL
          nuse (t) EXCLUSIVE
          ee:=e
          aeval(bk,{|x|dbsetrelation(x[1],&('{||'+x[2]+'}'),x[2])})
       endif
       ordlistclear()
 #ifdef A_SX
       if !DBINFO(132)
          DBINFO(140,A_SX)
       endif
 #endif
       select INDEKS
       while trim(INDEKS->BAZA) == b
          if empty(nazwa)
             skip
             loop
          endif
          a:=trim(klucz)
          c:=trim(for)
          d:=Expand(Lower(trim(nazwa)))
 #ifdef SIMPLE
          ?
          ?? "Odtwarzanie skorowidza "+d+", baza: "+expand(b)+".DBF, klucz: "+expand(a)
          ?
          select (cMessage)
          if empty(c)
             ordCondSet(,,,,{||outerr("."),.t.},int(1+lastrec()/78),RECNO(),,,,indeks->descend)
          else
             ordCondSet( expand(c),{||&c},,,{||outerr("."),.t.},int(1+lastrec()/79),RECNO(),,,,indeks->descend)
          endif
 #else
          f:=MESSAGE("Odtwarzanie skorowidza "+d+", baza: "+expand(b)+".DBF, klucz: "+expand(a)+";.")
          setpos(f[3]-1,f[2]+1)
          select (cMessage)
          if empty(c)
             ordCondSet(,,,,{||dispout("�"),message(1),.t.},int(1+lastrec()/(f[4]-f[2]-2)),RECNO(),,,,indeks->descend)
          else
             ordCondSet( expand(c),{||&c},,,{||dispout("�"),message(1),.t.},int(1+lastrec()/(f[4]-f[2]-2)),RECNO(),,,,indeks->descend)
          endif
 #endif
 #ifdef A_ADS

  #ifdef SIMPLE
          i:=0
          AdsRegCallBack( {|nPercent|outerr(replicate(".",nPercent/2-i),i:=nPercent/2,.f.}  )
  #else
          i:=f[2]
          AdsRegCallBack( {|nPercent|dispout(replicate( "�", int(npercent/100*(f[4]-f[2]-5))-(i-f[2]))),message(1),i:=col(),dispout(str(nPercent,3)+'%'),setpos(row(),i),.f.}  )
  #endif

  #ifdef A_CDX
          //ordCreate(,d,strtran(expand(a),'UPP(','UPPER('),,indeks->unique)
          ordCreate(,d,expand(a),,indeks->unique)
  #else
          if !empty(indeks->path)
             c:=trim(indeks->path)
             if c="&:"
                c:=&(subs(c,3))
             endif
             c:=expand(c)
             if right(c,1)<>HB_ps()
                c+=HB_ps()
             endif
          else
             if indeks->(fieldpos("PLIK"))=0 .or. empty(indeks->plik)
                c:=b
             else
                c:=trim(indeks->plik)
             endif
             c:=findfile(expand(c)+".dbf")
             c:=left(c,rat(HB_ps(),c))
          endif
          //ordCreate(c+d,,strtran(expand(a),'UPP(','UPPER('),,indeks->unique)
          ordCreate(c+d,,expand(a),,indeks->unique)
  #endif
          AdsClrCallBack()
 #else
          ordCreate(,d,expand(a),{||&a},indeks->unique)
 #endif
 #ifndef SIMPLE
          MESSAGE(f)
 #endif
          select INDEKS
          skip
       enddo
       if r=0
          use
       else
          goto n
       endif
       select (cMessage)
       if h
          ee:=NIL
          nuse (t) SHARED
          aeval(bk,{|x|dbsetrelation(x[1],&('{||'+x[2]+'}'),x[2])})
          ee:=e
       endif
       if e:candefault
          //e:candefault:=.f.
          ee:=NIL
          return .f.
       endif
#else
    locate while trim(BAZA)==b FOR {||d:=trim(nazwa), (i:=Lower(expand(d)))==Lower(subs(e:filename,-len(i)-4,len(i)))}
    if found()
          if !empty(path)
             a:=trim(path)
             if a="&:"
                a:=&(subs(a,3))
             endif
             t:=expand(a)
             if right(t,1)<>HB_ps()
                t+=HB_ps()
             endif
             e:filename:=t+i+Lower(ordbagext())
          else
             if fieldpos("PLIK")=0 .or. empty(indeks->plik)
                c:=b
             else
                c:=trim(indeks->plik)
             endif
             a:=findfile(expand(c)+".dbf")
             t:=left(a,rat(HB_ps(),a))
             if Lower(e:filename)#Lower(t)
                e:filename:=t+i+Lower(ordbagext())
             endif
          endif
          a:=trim(klucz)
          c:=trim(for)
          select (cMessage)
#ifdef __HARBOUR__
          nchoice:=0 ; while !empty(ordbagname(++nchoice)) ; aadd(aoptions,t+ordbagname(nchoice)) ; enddo
#else
          nchoice:=0 ; while !empty(t:=ordbagname(++nchoice)) ; aadd(aoptions,t) ; enddo
#endif

#ifdef SIMPLE
          ?
          ?? "Odtwarzanie skorowidza "+expand(d)+", baza: "+expand(b)+".dbf, klucz: "+expand(a)
          ?
          ordCondSet( expand(c),if(empty(c),,{||&c}),,,{||outerr("."),.t.},int(1+lastrec()/80),recno(),,,,indeks->descend)
#else
          t:=MESSAGE("Odtwarzanie skorowidza "+expand(d)+", baza: "+expand(b)+".dbf, klucz: "+expand(a)+";.")
          setpos(t[3]-1,t[2]+1)
          ordCondSet( expand(c),if(empty(c),,{||&c}),,,{||dispout("�"),message(1),.t.},int(1+lastrec()/(t[4]-t[2]-1)),recno(),,,,indeks->descend)
#endif
#ifdef IndexkeY
          ordCreate(e:filename,,strtran(expand(a),'UPP(','UPPER('),{||&a},indeks->unique)
#else
          ordCreate(e:filename,,expand(a),{||&a},indeks->unique)
#endif
          if r=0
             indeks->(dbclosearea())
          else
             indeks->(dbgoto(n))
          endif
          ordlistclear() ; aeval(aoptions,{|x|ordlistadd(x)})
#ifndef SIMPLE
          MESSAGE(t)
#endif
          if e:canRetry
             ee:=NIL
             return .t.
          elseif e:candefault
             ordlistadd(e:filename)
             ee:=NIL
             return .f.
          endif
#endif
    elseif r=0
       use
    else
       goto n
    endif

#endif A_ADS


    select (cMessage)
    cMessage:=""
  endif

  ErrorMessage(e,@cMessage,@aOptions)

  // put up alert box
  nChoice := 0
  #ifdef SIMPLE
  ?? chr(7)
  #else
  tone(130,5)
  clear typeahead
  #endif
  while ( nChoice == 0 )

    nChoice := alarm( cMessage, aOptions )

//    if ( nChoice == NIL )
//      exit
//    end

  end


  if ( !Empty(nChoice) )

    ee:=NIL
    // do as instructed

    if ( aOptions[nChoice] = "P" )
      Break(e)

    elseif ( aOptions[nChoice] = "S" )
      return (.t.)

    elseif ( aOptions[nChoice] = "O" )
      return (.f.)

    end

  end


  // display message and traceback
#ifndef SIMPLE
  set color to w
  set cursor on
#endif



// used below
//#define NTRIM(n)    ( LTrim(Str(n)) )

  ? cMessage
  ?
  ? 'Stos: '
  i := 2
  while ( !Empty(ProcName(i)) )
    ?? Trim(ProcName(i)) + "(" + NTRIM(ProcLine(i)) + "), "
    i++
  end
  ?
  // give up
  ErrorLevel(1)
  QUIT

return (.f.)




/***
*  ErrorMessage()
*/
static proc ErrorMessage(e,cMessage,aOptions)

if ""=cMessage
  // start error message
  cMessage := if( e:severity > ES_WARNING, "B��d ", "Uwaga " )

  // add subsystem name if available
  if ( ValType(e:subsystem) $ "MC" )
    cMessage += e:subsystem
  end


  // add subsystem's error code if available
  if ( ValType(e:subCode) == "N" )
    cMessage += ("/" + NTRIM(e:subCode))
  end


  // add error description if available
  if ( ValType(e:description) $ "MC" )
    cMessage += ("  " + e:description)
  end


  // add either filename or operation
  if ( !Empty(e:filename) )
    cMessage += (": " + e:filename)
  endif
  if ( !Empty(e:operation) )
    cMessage += (": " + e:operation)
    if !Empty(e:args)
       cMessage+="("
       aeval(e:args,{|x,y|y:=valtype(x),cMessage+=y+IF(y$"MCA",ltrim(str(len(x))),'')+if(y$'ABOU','',IF(y$"MC",':"'+left(x,10)+'"',":"+alltrim(TCVT(x))))+', '})
       cMessage:=left(cMessage,len(cMessage)-2)+")"
    endif
  end

  if ( !Empty(e:osCode) )
    cMessage +=  ";(DOS Error " + NTRIM(e:osCode) + ")"
  end

endif
  // build options array

  aoptions:={}

  if (e:canRetry)
    AAdd(aOptions, "Spr�buj")
  end


  if (e:severity > ES_WARNING )
     if (e:canDefault)
       AAdd(aOptions, "Omi�")
     end
     aadd(aoptions,"Przerwij")
     aadd(aoptions,"Koniec !")
  else
     AAdd(aOptions, "OK")
  endif

return


#ifndef SIMPLE
******************
proc ratuj(f)
field baza, path, nazwa, plik, klucz
local txt:='',y,x,a
        close DATABASES
        xselect indeks
        begin sequence
        do while !eof()
           if txt==lower(trim(baza))
#ifndef A_CDX
           if !empty(klucz)
              a:=expand(lower(trim(nazwa)))
              ferase(x+a+ordbagext())
           endif
#endif
              skip
           endif
           a:=''
           txt:=lower(trim(baza))
           x:=trim(path)
           if !empty(x)
             if x="&:"
                x:=trim(&(subs(x,3)))
             endif
             x:=expand(x)
             if right(x,1)<>HB_ps()
                x+=HB_ps()
             endif
           endif
           x+=if(fieldpos("PLIK")=0 .or. empty(plik),txt,trim(plik))
           x:=findfile(expand(x)+".dbf")
           if empty(x)
              loop
           endif
           x:=left(x,len(x)-4)
           select 1
           begin sequence
#ifdef A_CDX
           IF !empty(INDEKS->KLUCZ)
              nUSE (x) EXCLUSIVE ALIAS (expand(txt))
              do while !empty(y:=ordname(1))
                orddestroy(y)
              enddo
              ordlistclear()
              use
              ferase(x+ordbagext())
           ENDIF
#else
           x:=left(x,rat(if(HB_ps()$x,HB_ps(),":"),x))
#endif
           end sequence
           select indeks
        enddo
        end sequence
        IF ! (f=.f.)
          reuse()
        ENDIF
return
**********************
#endif

#ifdef A_LAN
// l -  .t. - omin
//       M  - message
//       c  - NIL - break
//       al - alias
//       rec - record
func reclock(l,M,c,al,rec)
static e:=1
local s,p
      if empty(al)
         al:=select()
      elseif valtype(al)$'MC'
         al=select(al)
      endif
/*
s:=(al)->(dbrlocklist())
if empty(rec)
   p:=(al)->(recno())
   if 0<>ascan( s, p )
      (al)->(aeval( s , {|x|if(x=p,,dbrunlock( x ))} ))
      return .t.
   endif
elseif 0<>ascan( s,rec )
   return .t.
endif
s:=NIL
*/
if eof()
  if empty(rec)
    dbrunlock()
  endif
  return .t.
endif
do while !(al)->(if(empty(rec),dbrlock(),dbrlock(rec)))
   if s=NIL
  #ifdef SIMPLE
  ?? chr(7)
  #else
  tone(130,5)
  clear typeahead
  #endif
      s:=.t.
      alarm("UWAGA SIE�: "+IF(M=NIL,"Nie potrafi� uzyska� wy��cznego prawa zapisu.",M)+";BAZA: "+alias(al),if(l=.t.,{"Pr�buj","Rezygnuj","Omi�"},{"Pr�buj","Rezygnuj"}),@e,2,@s)
      if e>1
#ifndef SIMPLE
         window(s)
#endif
         if c=NIL
            if e=2
               break(e)
            endif
            return .f.
         endif
         return c
      endif
#ifdef SIMPLE
      ? "[Esc] - rezygnacja, Pr�ba  "
   else
      message(1)
#ifdef __HARBOUR__
      HB_IDLESLEEP(.5)
#endif      
#else
      @ s[3]-1,s[2]+1 say padl("[Esc] - rezygnacja",s[4]-s[2]-2)
      @ s[3]-1,s[2]+2 say "Pr�ba..."
   elseif inkey(.6-seconds()%.5)#0
      window(s)
      s:=NIL
   else
      message(1)
#endif
   endif
enddo
#ifndef SIMPLE
   if s#NIL
      message(1)
      window(s)
   endif
#endif
return .t.
*****************
func filock(l,M,c,al)
static e:=1
local s

      if empty(al)
         al:=select()
      elseif valtype(al)$'MC'
         al=select(al)
      endif

do while !(al)->(flock())
   if s=NIL
  #ifdef SIMPLE
  ?? chr(7)
  #else
  tone(130,5)
  clear typeahead
  #endif
      s:=.t.
      alarm("UWAGA SIE�: "+IF(M=NIL,"Nie potrafi� uzyska� wy��cznego prawa zapisu.",M)+";BAZA: "+alias(al),if(l=.t.,{"Pr�buj","Rezygnuj"},{"Pr�buj","Rezygnuj","Omi�"}),@e,2,@s)
      if e>1
#ifndef SIMPLE
         window(s)
#endif
         if c=NIL
            if e=2
               break(e)
            endif
            return .f.
         endif
         return c
      endif
#ifdef SIMPLE
      ? "[Esc] - rezygnacja, Pr�ba"
   else
      ?? chr(8)+message(1)
#else
      @ s[3]-1,s[2]+1 say padl("[Esc] - rezygnacja",s[4]-s[2]-2)
      @ s[3]-1,s[2]+2 say "Pr�ba..."
   elseif inkey(.6-seconds()%.5)#0// .or. seconds()>w
      window(s)
      s:=NIL
   else
      message(1)
#endif
   endif
enddo
#ifndef SIMPLE
   if s#NIL
      message(1)
      window(s)
   endif
#endif
return .t.
#endif A_LAN
*******************
