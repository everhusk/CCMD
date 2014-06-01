c  begin file core_ANC.f
c
c  This file contains core routines for the ancillary equations for vapor
c  pressure, saturated liquid density and saturated vapor density.
c
c  contained here are:
c     subroutine SETPS (nread,icomp,ierr,herr)
c     subroutine PSATT (t,x,p,ierr,herr)
c     subroutine PSATK (icomp,t,p,ierr,herr)
c     subroutine DPSATK (icomp,t,dpdt,ierr,herr)
c     subroutine D2PSTK (icomp,t,d2pdt2,ierr,herr)
c     subroutine TSATP (p,x,t,ierr,herr)
c     subroutine SETPL (nread,icomp,ierr,herr)
c     subroutine PLSATT (t,x,p,ierr,herr)
c     subroutine PLSATK (icomp,t,p,ierr,herr)
c     subroutine TSATPL (p,x,t,ierr,herr)
c     subroutine SETDL (nread,icomp,ierr,herr)
c     subroutine DLSATT (t,x,d,ierr,herr)
c     subroutine DLSATK (icomp,t,d,ierr,herr)
c     subroutine SETDV (nread,icomp,ierr,herr)
c     subroutine DVSATT (t,x,d,ierr,herr)
c     subroutine DVSATK (icomp,t,d,ierr,herr)
c     subroutine TSATD (d,x,t,ierr,herr)
c     subroutine SOLVEA (iflag,pd,x,t,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      subroutine SETPS (nread,icomp,ierr,herr)
c
c  set up working arrays for use with the vapor pressure ancillary equation
c
c  inputs:
c    nread--file to read data from (file should have already been
c           opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c
c  outputs:
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
c  read data from file
c     write (*,*) ' SETPS--read component',icomp,' from unit',nread
      read (nread,*) pstmin(icomp)        !lower temperature limit
      read (nread,*) pstmax(icomp)        !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) pstrd(icomp),psprd(icomp)!reducing parameters
      read (nread,*) nps1(icomp),nps2(icomp),nps3(icomp),
     &               nps4(icomp),nps5(icomp),nps6(icomp)
      if (nps1(icomp).gt.0) then
        do k=1,nps1(icomp)
          read (nread,*) pska(icomp,k),psexp(icomp,k)
        enddo
      endif
      ierr=0
      herr=' '
c
      RETURN
      end                                              !subroutine SETPS
c
c ======================================================================
c
      subroutine PSATT (t,x,p,ierr,herr)
c
c  compute mixture or pure fluid vapor pressure with appropriate
c  ancillary equation
c
c  inputs:
c        t--temperature (K)
c        x--composition [array of mol frac]
c   output:
c        p--vapor pressure [kPa]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        121 = error:  T>Tc
c                        100 = error:  unknown vapor pressure equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      p=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call PSATK (icomp,t,p,ierr,herr)
      else
        pp=0.0d0
        do i=1,nc
          call PSATK (i,t,p,ierr,herr)   !Add Raoult's Law here
          pp=pp+x(i)*p
        enddo
        p=pp
      end if
c     write (*,1200) t,p
c1200 format (' PSATT--t,p: ',2f11.6)
c
      RETURN
      end                                              !subroutine PSATT
c
c ======================================================================
c
      subroutine PSATK (icomp,t,p,ierr,herr)
c
c  compute pure fluid vapor pressure with the appropriate ancillary equation
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        p--vapor pressure [kPa]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        2 = error:  no equation available
c                        121 = error:  T>Tc
c                        100 = error:  unknown vapor pressure equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      p=0.0d0
      if (t.lt.pstmin(icomp)) then
        p=ptpn(icomp)
        ierr=1
        write (herr,1097) t,pstmin(icomp),hnull
        call ERRMSG (ierr,herr)
 1097   format ('[PSATK error 1] ',
     &          'temperature less than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        RETURN
      endif
c
      if (t.gt.tcrit(icomp)) then
        p=pcrit(icomp)
        ierr=121
        write (herr,1098) t,tcrit(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[PSATK error 121] ',
     &          'temperature greater than critical temperature; T =',
     &          g12.5,' K, Tc =',g12.5,' K.',a1)
        RETURN
      endif
c
c  i is the value following 'PS' in the .fld file
      i=0
      i=ICHAR(hpsk(icomp)(3:3))-48
      if (i.lt.0) i=0
      if (i.gt.9) i=0
      if (hpsk(icomp)(1:2).eq.'PS' .and. i.gt.0) then
        tr=ABS(1.d0-t/pstrd(icomp))
        if (MOD(i,2).eq.0) tr=SQRT(tr)  !Even values of i
        pr=0.0d0
        if (nps1(icomp).ne.0) then
          do k=1,nps1(icomp)
            pr=pr+pska(icomp,k)*tr**psexp(icomp,k)
          enddo
        endif
        if (i.eq.1 .or. i.eq.2) pr=1.d0+pr
        if (i.eq.3 .or. i.eq.4) pr=EXP(pr)
        if (i.eq.5 .or. i.eq.6) pr=EXP(pstrd(icomp)/t*pr)
        p=psprd(icomp)*pr
c  do not return error message if fluid contains no vapor pressure line
      elseif (hpsk(icomp).eq.'NBS') then
        p=pcrit(icomp)
        ierr=2
        herr='[PSATK error 2] vapor pressure equation not available'
        call ERRMSG (ierr,herr)
      else
        p=0.0d0
        ierr=100
        write (herr,1099) hpsk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[PSATK error 100] ',
     &          'unknown vapor pressure equation:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,p
c1200 format (' PSATK--icomp,t,p: ',i4,2f11.6)
c
      RETURN
      end                                              !subroutine PSATK
c
c ======================================================================
c
      subroutine DPSATK (icomp,t,dpdt,ierr,herr)
c
c  compute the first derivative of the vapor pressure with the
c  appropriate equation
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c     dpdt--dp/dt [kPa/K]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        2 = error:  no equation available
c                        121 = error:  T>Tc
c                        100 = error:  unknown vapor pressure equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  05-30-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      dpdt=0.0d0
      if (t.lt.pstmin(icomp)) then
        dpdt=ptpn(icomp)
        ierr=1
        write (herr,1097) t,pstmin(icomp),hnull
        call ERRMSG (ierr,herr)
 1097   format ('[DPSATK error 1] ',
     &          'temperature less than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        RETURN
      endif
c
      if (t.gt.tcrit(icomp)) then
        dpdt=pcrit(icomp)
        ierr=121
        write (herr,1098) t,tcrit(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[DPSATK error 121] ',
     &          'temperature greater than critical temperature; T =',
     &          g12.5,' K, Tc =',g12.5,' K.',a1)
        RETURN
      endif
c
c  i is the value following 'PS' in the .fld file
      i=0
      i=ICHAR(hpsk(icomp)(3:3))-48
      if (i.lt.0) i=0
      if (i.gt.9) i=0
      if (hpsk(icomp)(1:2).eq.'PS' .and. i.gt.0) then
        tr=ABS(1.d0-t/pstrd(icomp))
        if (MOD(i,2).eq.0) tr=SQRT(tr)  !Even values of i
        pr=0.0d0
        dpr=0.0d0
        if (nps1(icomp).ne.0) then
          do k=1,nps1(icomp)
            pe=psexp(icomp,k)
            pr=pr+pska(icomp,k)*tr**pe
            if (MOD(i,2).eq.1) then           !odd values of i
              dpr=dpr-pe*pska(icomp,k)*tr**(pe-1.d0)/pstrd(icomp)
            else                              !even values of i
              dpr=dpr-pe/2.d0*pska(icomp,k)*tr**(pe-2.d0)/pstrd(icomp)
            endif
          enddo
        endif
        tr=pstrd(icomp)/t
        if (i.eq.1 .or. i.eq.2) pr=dpr
        if (i.eq.3 .or. i.eq.4) pr=EXP(pr)*dpr
        if (i.eq.5 .or. i.eq.6) pr=EXP(tr*pr)*tr*(dpr-pr/t)
        dpdt=psprd(icomp)*pr
c  do not return error message if fluid contains no vapor pressure line
      elseif (hpsk(icomp).eq.'NBS') then
        dpdt=pcrit(icomp)
        ierr=2
        herr='[PSATK error 2] vapor pressure equation not available'
        call ERRMSG (ierr,herr)
      else
        dpdt=0.0d0
        ierr=100
        write (herr,1099) hpsk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[DPSATK error 100] ',
     &          'unknown vapor pressure equation:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,dpdt
c1200 format (' DPSATK--icomp,t,dp/dt: ',i4,2f11.6)
c
      RETURN
      end                                             !subroutine DPSATK
c
c ======================================================================
c
      subroutine D2PSTK (icomp,t,d2pdt2,ierr,herr)
c
c  compute the second derivative of the vapor pressure with the
c  appropriate equation
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c   d2pdt2--d2p/dt2 [kPa/K^2]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        2 = error:  no equation available
c                        121 = error:  T>Tc
c                        100 = error:  unknown vapor pressure equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  05-30-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      d2pdt2=0.0d0
      if (t.lt.pstmin(icomp)) then
        d2pdt2=ptpn(icomp)
        ierr=1
        write (herr,1097) t,pstmin(icomp),hnull
        call ERRMSG (ierr,herr)
 1097   format ('[D2PSTK error 1] ',
     &          'temperature less than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        RETURN
      endif
c
      if (t.gt.tcrit(icomp)) then
        d2pdt2=pcrit(icomp)
        ierr=121
        write (herr,1098) t,tcrit(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[D2PSTK error 121] ',
     &          'temperature greater than critical temperature; T =',
     &          g12.5,' K, Tc =',g12.5,' K.',a1)
        RETURN
      endif
c
c  i is the value following 'PS' in the .fld file
      i=0
      i=ICHAR(hpsk(icomp)(3:3))-48
      if (i.lt.0) i=0
      if (i.gt.9) i=0
      if (hpsk(icomp)(1:2).eq.'PS' .and. i.gt.0) then
        tr=ABS(1.d0-t/pstrd(icomp))
        if (MOD(i,2).eq.0) tr=SQRT(tr)  !Even values of i
        pr=0.0d0
        dpr=0.0d0
        d2pr=0.0d0
        if (nps1(icomp).ne.0) then
          do k=1,nps1(icomp)
            pe=psexp(icomp,k)
            pr=pr+pska(icomp,k)*tr**pe
            if (MOD(i,2).eq.1) then           !odd values of i
              dpr=dpr-pe*pska(icomp,k)*tr**(pe-1.d0)/pstrd(icomp)
              d2pr=d2pr+pe*(pe-1.d0)*pska(icomp,k)*tr**(pe-2.d0)
     &            /pstrd(icomp)**2
            else                              !even values of i
              dpr=dpr-pe/2.d0*pska(icomp,k)*tr**(pe-2.d0)/pstrd(icomp)
              d2pr=d2pr+pe*(pe-2.d0)/4.d0*pska(icomp,k)*tr**(pe-4.d0)
     &            /pstrd(icomp)**2
            endif
          enddo
        endif
        tr=pstrd(icomp)/t
        if (i.eq.1 .or. i.eq.2) pr=d2pr
        if (i.eq.3 .or. i.eq.4) pr=EXP(pr)*(dpr**2+d2pr)
        if (i.eq.5 .or. i.eq.6) pr=EXP(tr*pr)*
     &     ((tr*(dpr-pr/t))**2+2.d0*tr/t**2*pr-2.d0*tr/t*dpr+tr*d2pr)
        d2pdt2=psprd(icomp)*pr
c  do not return error message if fluid contains no vapor pressure line
      elseif (hpsk(icomp).eq.'NBS') then
        d2pdt2=pcrit(icomp)
        ierr=2
        herr='[D2PSTK error 2] vapor pressure equation not available'
        call ERRMSG (ierr,herr)
      else
        d2pdt2=0.0d0
        ierr=100
        write (herr,1099) hpsk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[D2PSTK error 100] ',
     &          'unknown vapor pressure equation:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,d2pdt2
c1200 format (' D2PSTK--icomp,t,dp/dt: ',i4,2f11.6)
c
      RETURN
      end                                             !subroutine D2PSTK
c
c ======================================================================
c
      subroutine TSATP (p,x,t,ierr,herr)
c
c  compute the vapor temperature as a function of pressure.
c
c  inputs:
c        p--vapor pressure [kPa]
c        x--composition [array of mol frac]
c   output:
c        t--temperature [K]
c        ierr--error flag:  0 = successful
c                           1 = error:  p<ptrp
c                           2 = error:  no equation available
c                           141 = error:  p>pc
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hpsk(icomp).eq.'NBS') then  !Return if no vapor pressure eq.
          t=tcrit(icomp)
          ierr=2
          herr='[TSATP error 2] vapor pressure equation not available'
          call ERRMSG (ierr,herr)
          RETURN
        endif
        if (p.lt.ptpn(icomp)) then
          ierr=1
          t=ttpn(icomp)
          write (herr,1148) p/1000.0d0,ptpn(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1148     format ('[TSATP error 1] ',
     &            'pressure less than triple point pressure; P =',
     &          g12.5,' MPa, Ptp =',g12.5,' MPa.',a1)
          RETURN
        endif
        if (p.gt.pcrit(icomp)) then
          ierr=141
          t=tcrit(icomp)
          write (herr,1149) p/1000.0d0,pcrit(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1149     format ('[TSATP error 141] ',
     &            'pressure greater than critical pressure; P =',
     &          g12.5,' MPa, Pc =',g12.5,' MPa.',a1)
          RETURN
        endif
      endif
c
      call SOLVEA (0,p,x,t,ierr,herr)
c
      RETURN
      end                                              !subroutine TSATP
c
c ======================================================================
c
      subroutine SETPL (nread,icomp,ierr,herr)
c
c  set up working arrays for use with the liquid pressure ancillary equation
c
c  inputs:
c    nread--file to read data from (file should have already been
c           opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c
c  outputs:
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  09-18-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
c  read data from file
c     write (*,*) ' SETPL--read component',icomp,' from unit',nread
      read (nread,*) pltmin(icomp)        !lower temperature limit
      read (nread,*) pltmax(icomp)        !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) pltrd(icomp),plprd(icomp)!reducing parameters
      read (nread,*) npl1(icomp),npl2(icomp),npl3(icomp),
     &               npl4(icomp),npl5(icomp),npl6(icomp)
      if (npl1(icomp).gt.0) then
        do k=1,npl1(icomp)
          read (nread,*) plka(icomp,k),plexp(icomp,k)
        enddo
      endif
      ierr=0
      herr=' '
c
      RETURN
      end                                              !subroutine SETPL
c
c ======================================================================
c
      subroutine PLSATT (t,x,p,ierr,herr)
c
c  compute mixture or pure fluid liquid pressure with appropriate
c  ancillary equation
c  (used only for pseudo-pure fluids)
c
c  inputs:
c        t--temperature (K)
c        x--composition [array of mol frac]
c   output:
c        p--liquid pressure [kPa]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        121 = error:  T>Tc
c                        100 = error:  unknown liquid pressure equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  09-18-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      p=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call PLSATK (icomp,t,p,ierr,herr)
      else
        pp=0.0d0
        do i=1,nc
          call PLSATK (i,t,p,ierr,herr)   !Add Raoult's Law here
          pp=pp+x(i)*p
        enddo
        p=pp
      end if
c     write (*,1200) t,p
c1200 format (' PLSATT--t,p: ',2f11.6)
c
      RETURN
      end                                             !subroutine PLSATT
c
c ======================================================================
c
      subroutine PLSATK (icomp,t,p,ierr,herr)
c
c  compute pure fluid liquid pressure with the appropriate ancillary equation
c  (used only for pseudo-pure fluids)
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        p--liquid pressure [kPa]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        2 = error:  no equation available
c                        121 = error:  T>Tc
c                        100 = error:  unknown liquid pressure equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      p=0.0d0
      if (t.lt.pltmin(icomp)) then
        p=ptpn(icomp)
        ierr=1
        write (herr,1097) t,pltmin(icomp),hnull
        call ERRMSG (ierr,herr)
 1097   format ('[PLSATK error 1] ',
     &          'temperature less than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        RETURN
      endif
c
      if (t.gt.tcrit(icomp)) then
        p=pcrit(icomp)
        ierr=121
        write (herr,1098) t,tcrit(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[PLSATK error 121] ',
     &          'temperature greater than critical temperature; T =',
     &          g12.5,' K, Tc =',g12.5,' K.',a1)
        RETURN
      endif
c
c  i is the value following 'PL' in the .fld file
      i=0
      i=ICHAR(hplk(icomp)(3:3))-48
      if (i.lt.0) i=0
      if (i.gt.9) i=0
      if (hplk(icomp)(1:2).eq.'PL' .and. i.gt.0) then
        tr=ABS(1.d0-t/pltrd(icomp))
        if (MOD(i,2).eq.0) tr=SQRT(tr)  !Even values of i
        pr=0.0d0
        if (npl1(icomp).ne.0) then
          do k=1,npl1(icomp)
            pr=pr+plka(icomp,k)*tr**plexp(icomp,k)
          enddo
        endif
        if (i.eq.1 .or. i.eq.2) pr=1.d0+pr
        if (i.eq.3 .or. i.eq.4) pr=EXP(pr)
        if (i.eq.5 .or. i.eq.6) pr=EXP(pltrd(icomp)/t*pr)
        p=plprd(icomp)*pr
c  do not return error message if fluid contains no liquid pressure line
      elseif (hplk(icomp).eq.'NBS') then
        p=pcrit(icomp)
        ierr=2
        herr='[PLSATK error 2] vapor pressure equation not available'
        call ERRMSG (ierr,herr)
      else
        p=0.0d0
        ierr=100
        write (herr,1099) hplk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[PLSATK error 100] ',
     &          'unknown liquid pressure equation:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,p
c1200 format (' PLSATK--icomp,t,p: ',i4,2f11.6)
c
      RETURN
      end                                             !subroutine PLSATK
c
c ======================================================================
c
      subroutine TSATPL (p,x,t,ierr,herr)
c
c  compute the liquid temperature as a function of liquid pressure.
c  (used only for pseudo-pure fluids)
c
c  inputs:
c        p--liquid pressure [kPa]
c        x--composition [array of mol frac]
c   output:
c        t--temperature [K]
c        ierr--error flag:  0 = successful
c                           1 = error:  p<ptrp
c                           2 = error:  no equation available
c                           141 = error:  p>pc
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  09-19-02 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hplk(icomp).eq.'NBS') then    !Return if no liquid pressure eq.
          t=tcrit(icomp)
          ierr=2
          herr='[TSATPL error 2] vapor pressure equation not available'
          call ERRMSG (ierr,herr)
          RETURN
        endif
        if (p.lt.ptpn(icomp)) then
          ierr=1
          t=ttpn(icomp)
          write (herr,1148) p/1000.0d0,ptpn(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1148     format ('[TSATPL error 1] ',
     &            'pressure less than triple point pressure; P =',
     &          g12.5,' MPa, Ptp =',g12.5,' MPa.',a1)
          RETURN
        endif
        if (p.gt.pcrit(icomp)) then
          ierr=141
          t=tcrit(icomp)
          write (herr,1149) p/1000.0d0,pcrit(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1149     format ('[TSATPL error 141] ',
     &            'pressure greater than critical pressure; P =',
     &          g12.5,' MPa, Pc =',g12.5,' MPa.',a1)
          RETURN
        endif
      endif
c
      call SOLVEA (4,p,x,t,ierr,herr)
c
      RETURN
      end                                             !subroutine TSATPL
c
c ======================================================================
c
      subroutine SETDL (nread,icomp,ierr,herr)
c
c  set up working arrays for use with the saturated liquid density
c  ancillary equation
c
c  inputs:
c    nread--file to read data from (file should have already been
c           opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c
c  outputs:
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  09-18-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
c  read data from file
c     write (*,*) ' SETDL--read component',icomp,' from unit',nread
      read (nread,*) dltmin(icomp)        !lower temperature limit
      read (nread,*) dltmax(icomp)        !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) dltrd(icomp),dldrd(icomp)!reducing parameters
      read (nread,*) ndl1(icomp),ndl2(icomp),ndl3(icomp),
     &               ndl4(icomp),ndl5(icomp),ndl6(icomp)
      if (ndl1(icomp).gt.0) then
        do k=1,ndl1(icomp)
          read (nread,*) dlka(icomp,k),dlexp(icomp,k)
        enddo
      endif
      ierr=0
      herr=' '
c
      RETURN
      end                                              !subroutine SETDL
c
c ======================================================================
c
      subroutine DLSATT (t,x,d,ierr,herr)
c
c  compute mixture or pure fluid saturated liquid density with appropriate
c  ancillary equation
c
c  inputs:
c        t--temperature (K)
c        x--composition [array of mol frac]
c   output:
c        d--saturated liquid density [mol/L]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        121 = error:  T>Tc
c                        100 = error:  unknown saturated liquid density equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      d=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call DLSATK (icomp,t,d,ierr,herr)
      else
        dd=0.0d0
        do i=1,nc
          call DLSATK (i,t,d,ierr,herr)   !Add Raoult's Law here
          dd=dd+x(i)*d
        enddo
        d=dd
      end if
c     write (*,1200) t,d
c1200 format (' DLSATT--t,d: ',2f11.6)
c
      RETURN
      end                                             !subroutine DLSATT
c
c ======================================================================
c
      subroutine DLSATK (icomp,t,d,ierr,herr)
c
c  compute pure fluid saturated liquid density with appropriate equation
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        d--saturated liquid density [mol/L]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        2 = error:  no equation available
c                        121 = error:  T>Tc
c                        100 = error:  unknown saturated liquid density equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      d=0.0d0
      if (t.lt.dltmin(icomp)) then
        d=dtpn(icomp)
        ierr=1
        write (herr,1097) t,dltmin(icomp),hnull
        call ERRMSG (ierr,herr)
 1097   format ('[DLSATK error 1] ',
     &          'temperature less than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        RETURN
      endif
c
      if (t.gt.tcrit(icomp)) then
        d=dcrit(icomp)
        ierr=121
        write (herr,1098) t,tcrit(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[DLSATK error 121] ',
     &          'temperature greater than critical temperature; T =',
     &          g12.5,' K, Tc =',g12.5,' K.',a1)
        RETURN
      endif
c
c  i is the value following 'DL' in the .fld file
      i=0
      i=ICHAR(hdlk(icomp)(3:3))-48
      if (i.lt.0) i=0
      if (i.gt.9) i=0
      if (hdlk(icomp)(1:2).eq.'DL' .and. i.gt.0) then
        tr=ABS(1.d0-t/dltrd(icomp))
        if (MOD(i,2).eq.0) tr=tr**(1.d0/3.d0)  !Even values of i
        dr=0.0d0
        if (ndl1(icomp).ne.0) then
          do k=1,ndl1(icomp)
            dr=dr+dlka(icomp,k)*tr**dlexp(icomp,k)
          enddo
        endif
        if (i.eq.1 .or. i.eq.2) dr=1.d0+dr
        if (i.eq.3 .or. i.eq.4) dr=EXP(dr)
        if (i.eq.5 .or. i.eq.6) dr=EXP(dltrd(icomp)/t*dr)
        d=dldrd(icomp)*dr
c  do not return error message if fluid contains no saturated liquid density equation
      elseif (hdlk(icomp).eq.'NBS') then
        d=dcrit(icomp)
        ierr=2
        herr='[DLSATK error 2] liquid density equation not available'
        call ERRMSG (ierr,herr)
      else
        d=0.0d0
        ierr=100
        write (herr,1099) hdlk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[DLSATK error 100] ',
     &        'unknown saturated liquid density equation:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,d
c1200 format (' DLSATK--icomp,t,d: ',i4,2f11.6)
c
      RETURN
      end                                             !subroutine DLSATK
c
c ======================================================================
c
      subroutine SETDV (nread,icomp,ierr,herr)
c
c  set up working arrays for use with the saturated vapor density
c  ancillary equation
c
c  inputs:
c    nread--file to read data from (file should have already been
c           opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c
c  outputs:
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
c  read data from file
c     write (*,*) ' SETDV--read component',icomp,' from unit',nread
      read (nread,*) dvtmin(icomp)        !lower temperature limit
      read (nread,*) dvtmax(icomp)        !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) dvtrd(icomp),dvdrd(icomp)!reducing parameters
      read (nread,*) ndv1(icomp),ndv2(icomp),ndv3(icomp),
     &               ndv4(icomp),ndv5(icomp),ndv6(icomp)
      if (ndv1(icomp).gt.0) then
        do k=1,ndv1(icomp)
          read (nread,*) dvka(icomp,k),dvexp(icomp,k)
        enddo
      endif
      ierr=0
      herr=' '
c
      RETURN
      end                                              !subroutine SETDV
c
c ======================================================================
c
      subroutine DVSATT (t,x,d,ierr,herr)
c
c  compute mixture or pure fluid saturated vapor density with appropriate
c  ancillary equation
c
c  inputs:
c        t--temperature (K)
c        x--composition [array of mol frac]
c   output:
c        d--saturated vapor density [mol/L]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        121 = error:  T>Tc
c                        100 = error:  unknown saturated vapor density equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      d=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call DVSATK (icomp,t,d,ierr,herr)
      else
        dd=0.0d0
        do i=1,nc
          call DVSATK (i,t,d,ierr,herr)   !Add Raoult's Law here
          dd=dd+x(i)*d
        enddo
        d=dd
      end if
c     write (*,1200) t,d
c1200 format (' DVSATT--t,d: ',2f11.6)
c
      RETURN
      end                                             !subroutine DVSATT
c
c ======================================================================
c
      subroutine DVSATK (icomp,t,d,ierr,herr)
c
c  compute pure fluid saturated vapor density with appropriate equation
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        d--saturated vapor density [mol/L]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        2 = error:  no equation available
c                        121 = error:  T>Tc
c                        100 = error:  unknown saturated vapor density equation
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      d=0.0d0
      if (t.lt.dvtmin(icomp)) then
        d=dtpn(icomp)
        ierr=1
        write (herr,1097) t,dvtmin(icomp),hnull
        call ERRMSG (ierr,herr)
 1097   format ('[DVSATK error 1] ',
     &          'temperature less than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        RETURN
      endif
c
      if (t.gt.tcrit(icomp)) then
        d=dcrit(icomp)
        ierr=121
        write (herr,1098) t,tcrit(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[DVSATK error 121] ',
     &          'temperature greater than critical temperature; T =',
     &          g12.5,' K, Tc =',g12.5,' K.',a1)
        RETURN
      endif
c
c  i is the value following 'DV' in the .fld file
      i=0
      i=ICHAR(hdvk(icomp)(3:3))-48
      if (i.lt.0) i=0
      if (i.gt.9) i=0
      if (hdvk(icomp)(1:2).eq.'DV' .and. i.gt.0) then
        tr=ABS(1.d0-t/dvtrd(icomp))
        if (MOD(i,2).eq.0) tr=tr**(1.d0/3.d0)  !Even values of i
        dr=0.0d0
        if (ndv1(icomp).ne.0) then
          do k=1,ndv1(icomp)
            dr=dr+dvka(icomp,k)*tr**dvexp(icomp,k)
          enddo
        endif
        if (i.eq.1 .or. i.eq.2) dr=1.d0+dr
        if (i.eq.3 .or. i.eq.4) dr=EXP(dr)
        if (i.eq.5 .or. i.eq.6) dr=EXP(dvtrd(icomp)/t*dr)
        d=dvdrd(icomp)*dr
c  do not return error message if fluid contains no saturated vapor density equation
      elseif (hdvk(icomp).eq.'NBS') then
        d=dcrit(icomp)
        ierr=2
        herr='[DLSATK error 2] liquid density equation not available'
        call ERRMSG (ierr,herr)
      else
        d=0.0d0
        ierr=100
        write (herr,1099) hdvk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[DVSATK error 100] ',
     &         'unknown saturated vapor density equation:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,d
c1200 format (' DVSATK--icomp,t,d: ',i4,2f11.6)
c
      RETURN
      end                                             !subroutine DVSATK
c
c ======================================================================
c
      subroutine TSATD (d,x,t,ierr,herr)
c
c  compute the saturated temperature as a function of saturated density.
c
c  inputs:
c        d--saturated density [mol/L]
c        x--composition [array of mol frac]
c   output:
c        t--temperature [K]
c        ierr--error flag:  0 = successful
c                           1 = no ancillary
c                           2 = error:  p<ptrp
c                           141 = error:  p>pc
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  03-13-01 EWL, original version
c  12-14-11 EWL, add error for equation not available
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      i=1
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  Return if no saturated ancillary equation
        t=ttpn(icomp)
        if ((hdlk(icomp).eq.'NBS' .and. d.ge.dcrit(icomp)) .or.
     &      (hdvk(icomp).eq.'NBS' .and. d.le.dcrit(icomp))) then
          ierr=1
          herr='[TSATD error 1] Ancillary equation not available'
          call ERRMSG (ierr,herr)
          RETURN
        endif
c       if (d.lt.dtpv(1)) then    !dtpv has not been set yet!
c         ierr=2
c         write (herr,1148) d,dtpv(1),hnull
c         call ERRMSG (ierr,herr)
c1148     format ('[TSATD error 2] ',
c    &            'density less than triple point density; d =',
c    &          g12.5,' mol/L, dtp =',g12.5,' mol/L.',a1)
c         RETURN
c       endif
        if (d.gt.dtpn(icomp)) then
          ierr=141
          write (herr,1149) d,dtpn(icomp),hnull
          call ERRMSG (ierr,herr)
 1149     format ('[TSATD error 141] ',
     &            'density greater than triple point density; d=',
     &          g12.5,' mol/L, dtp =',g12.5,' mol/L.',a1)
c         RETURN
        endif
        if (d.ge.dcrit(icomp)) i=1
        if (d.lt.dcrit(icomp)) i=2
      endif
c
      call SOLVEA (i,d,x,t,ierr,herr)
c
      RETURN
      end                                              !subroutine TSATD
c
c ======================================================================
c
      subroutine SOLVEA (iflag,pd,x,t,ierr,herr)
c
c  solve for temperature given vapor pressure or saturated liquid
c  or vapor density
c
c  inputs:
c     iflag--use:  vapor pressure equation when iflag=0
c                  saturated liquid density equation when iflag=1
c                  saturated vapor density equation when iflag=2
c                  liquid pressure equation when iflag=4
c       pd--pressure [kPa] or density [mol/L]
c        x--composition [array of mol frac]
c   output:
c        t--temperature (K)
c     ierr--error flag:  0 = successful
c                       124 = no convergence
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  03-13-01 EWL, original version
c  07-20-10 EWL, change how t is modified if p1<1.d-15
c  04-04-13 EWL, change initial t to 0.75*tc so that t>ttrp of CO2
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      tol=0.5d-8
      dx=0.001d0
      dt=-10.d0
      call CRITP (x,tc,pc,rhoc,ierr,herr)
c     ttrp=ttpn(icomp)
      t=0.75*tc
c  calculate vapor pressure (or saturated density) and its derivative,
c  then update temperature iteratively
      do i=1,50
        dy=0
        if (t+dx.ge.tc) dy=dx !Prevent numerical deriv. going above Tc
        if (iflag.eq.0) then
          call PSATT (t-dy   ,x,pp,ierr,herr)
          call PSATT (t-dy+dx,x,p1,ierr,herr)
        elseif (iflag.eq.1) then
          call DLSATT (t-dy   ,x,pp,ierr,herr)
          call DLSATT (t-dy+dx,x,p1,ierr,herr)
        elseif (iflag.eq.2) then
          call DVSATT (t-dy   ,x,pp,ierr,herr)
          call DVSATT (t-dy+dx,x,p1,ierr,herr)
        elseif (iflag.eq.4) then
          call PLSATT (t-dy   ,x,pp,ierr,herr)
          call PLSATT (t-dy+dx,x,p1,ierr,herr)
        endif
        if (ierr.ne.0) return
        told=t
        if (ABS(p1).lt.1.d-15) then
          t=t-0.5d0*dt
        else
          if (iflag.eq.1) then
            dpt=(p1-pp)/dx
            dt=-(pp-pd)/dpt        !1st order Newton's method
          else
            dpt=ABS(log(p1)-log(pp))/dx
            dt=-(log(pp)-log(pd))/dpt        !1st order Newton's method
          endif
          t=t+dt
        endif
        if (ABS(dt).lt.tol) RETURN
c       if (t.lt.ttrp) t=(told+ttrp)/2.d0
        if (t.ge.tc) t=(told+tc)/2.d0
      enddo
      ierr=124
      t=-9.99999d6
      write (herr,1100)
      call ERRMSG (ierr,herr)
 1100 format('[SOLVEA error 124] maximum number of iterations exceeded')
      RETURN
      end                                             !subroutine SOLVEA
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file core_ANC.f
c ======================================================================
