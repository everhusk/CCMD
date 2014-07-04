c  begin file core_MLT.f
c
c  This file contains core routines for melting lines and sublimation lines.
c
c  contained here are:
c     subroutine SETMLT (nread,icomp,ierr,herr)
c     subroutine MELTT (t,x,p,ierr,herr)
c     subroutine MELTP (p,x,t,ierr,herr)
c     subroutine MELTK (icomp,t,p,ierr,herr)
c     subroutine MLTH2O (t,p1,p2)
c     subroutine MLTPH2 (icomp,t,p)
c     subroutine SETSBL (nread,icomp,ierr,herr)
c     subroutine SUBLT (t,x,p,ierr,herr)
c     subroutine SUBLP (p,x,t,ierr,herr)
c     subroutine SUBLK (icomp,t,p,ierr,herr)
c     subroutine SOLVEM (iflag,p,x,t,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      subroutine SETMLT (nread,icomp,ierr,herr)
c
c  set up working arrays for use with melting line equation
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
c  11-16-98 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      herr=' '
      ierr=0
c
c  read data from file
c     write (*,*) ' SETMLT--read component',icomp,' from unit',nread
      read (nread,*) tminmt(icomp)          !lower temperature limit
      read (nread,*) tmaxmt(icomp)          !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) trdmlt(icomp),prdmlt(icomp)!reducing parameters
      read (nread,*) ntrmm1(icomp),ntrmm2(icomp),ntrmm3(icomp),
     &               ntrmm4(icomp),ntrmm5(icomp),ntrmm6(icomp)
      do k=1,ntrmm1(icomp)
        read (nread,*) pmeltk(icomp,k),pmltex(icomp,k)
      enddo
      do k=1,ntrmm2(icomp)
        j=k+ntrmm1(icomp)
        read (nread,*) pmeltk(icomp,j),pmltex(icomp,j)
      enddo
      do k=1,ntrmm3(icomp)
        j=k+ntrmm1(icomp)+ntrmm2(icomp)
        read (nread,*) pmeltk(icomp,j),pmltex(icomp,j)
      enddo
c
      RETURN
      end                                             !subroutine SETMLT
c
c ======================================================================
c
      subroutine MELTT (t,x,p,ierr,herr)
c
c  compute the melting line pressure as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c   output:
c        p--melting line pressure [kPa]
c        ierr--error flag:  0 = successful
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  11-16-98 EWL, original version
c  06-22-99 EWL, return if ptpn(i)=0 for a mixture
c  09-02-99 EWL, add check for p>pmax
c  03-08-07 EWL, remove p=ptpn(i) when t<ttrp and call MELTT anyways, thus making sure the fluid has a melting line.
c  03-08-07 EWL, change p to p2 so that an intermittent value does not get returned when there is no melting line
c
cx DEC$ ATTRIBUTES DLLEXPORT :: MELTT
c
      include 'commons.for'
      dimension x(ncmax)
      character*255 herr
c
      p=0.d0
      p2=0.d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call MELTK (icomp,t,p,ierr,herr)
        pmax=peos(icomp)
c     else if (hmelt.eq.'MLX' .or. hmelt.eq.'MLM') then
      else
c  mixture melting line--use simple mixing rules
        ttrp=0
        ptrp=0
        do i=1,nc
c  do not return a value if any of the fluids does not have a triple point pressure
          if (ptpn(i).lt.1.d-15) return
          ttrp=ttrp+x(i)*ttpn(i)
          ptrp=ptrp+x(i)*log(ptpn(i))
        enddo
        ptrp=exp(ptrp)
        sum=0.d0
        pmax=0.d0
        do i=1,nc
          if (ttrp.gt.0.d0) then
            t2=t/ttrp*ttpn(i)
            if (t2.lt.ttpn(i)) t2=ttpn(i)
            call MELTK (i,t2,p2,ierr,herr)
c  do not return a value if any of the fluids does not have a melting line
            if (p2.lt.1.d-15) return
            if (ptpn(i).gt.0.d0) sum=sum+x(i)*LOG(p2/ptpn(i))
            pmax=pmax+x(i)*peos(i)
          endif
        enddo
        p=EXP(sum)*ptrp
      end if
c
      RETURN
      end                                              !subroutine MELTT
c
c ======================================================================
c
      subroutine MELTP (p,x,t,ierr,herr)
c
c  compute the melting line temperature as a function of pressure
c  and composition.
c
c  inputs:
c        p--melting line pressure [kPa]
c        x--composition [array of mol frac]
c   output:
c        t--temperature [K]
c        ierr--error flag:  0 = successful
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  02-12-99 EWL, original version
c  01-21-00 EWL, return if fluids does not have a melting line
c
cx DEC$ ATTRIBUTES DLLEXPORT :: MELTP
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hmeltk(icomp).eq.'NBS') then  !Return if no melting line
          t=ttpn(icomp)
          RETURN
        endif
        if (p.lt.ptpn(icomp)) then
          ierr=2
          t=ttpn(icomp)
          write (herr,1149) p/1000.0d0,ptpn(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1149     format ('[MELTP error 2] ',
     &            'pressure less than triple point pressure; P =',
     &          g12.5,' MPa, Ptp =',g12.5,' MPa.',a1)
          return
        endif
      endif
c
      call solvem (1,p,x,t,ierr,herr)
c
      RETURN
      end                                              !subroutine MELTP
c
c ======================================================================
c
      subroutine MELTK (icomp,t,p,ierr,herr)
c
c  compute melting line with appropriate core model
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        p--melting line pressure [kPa]
c     ierr--error flag:  0 = successful
c                        1 = error:  T<Ttrp
c                        105 = error:  unknown melting line
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  11-16-98 EWL, original version
c  10-16-01 EWL, change order, call MLTPH2 instead of MELT2
c  10-16-01 EWL, change ML3 to MLP and ML4 to MLW
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
      p=0
      tt=ttpn(icomp)
      if (hmeltk(icomp).eq.'MLW') tt=251.165d0
      if (t.lt.tt) then
        p=0.0d0
        ierr=1
        write (herr,1098) t,tt,hnull
        call ERRMSG (ierr,herr)
 1098   format ('[MELTK error 1] ',
     &          'temperature less than lower temperature limit; T =',
     &          g12.5,' K, Tlow =',g12.5,' K.',a1)
        return
      endif
c
      if (hmeltk(icomp).eq.'MLW') then
        call MLTH2O (t,p,p2)     !Water
      elseif (hmeltk(icomp).eq.'MLP') then
        call MLTPH2 (icomp,t,p)  !Parahydrogen
      elseif (hmeltk(icomp)(1:2).eq.'ML') then
        tr=t/trdmlt(icomp)
        pr=0.0d0
        do k=1,ntrmm1(icomp)
          pr=pr+pmeltk(icomp,k)*tr**pmltex(icomp,k)
        enddo
        do k=1,ntrmm2(icomp)
          j=k+ntrmm1(icomp)
          pr=pr+pmeltk(icomp,j)*(tr-1.0d0)**pmltex(icomp,j)
        enddo
        do k=1,ntrmm3(icomp)
          j=k+ntrmm1(icomp)+ntrmm2(icomp)
          pr=pr+pmeltk(icomp,j)*log(tr)**pmltex(icomp,j)
        enddo
        p=pr
        if (hmeltk(icomp).eq.'ML1') p=prdmlt(icomp)*pr
        if (hmeltk(icomp).eq.'ML2') p=prdmlt(icomp)*exp(pr)
c  do not return error message if fluid contains no melting line
      elseif (hmeltk(icomp).eq.'NBS') then
      else
        p=0.0d0
        ierr=105
        write (herr,1099) hmeltk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[MELTK error 105] ',
     &          'unknown melting line model:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,p
c1200 format (' MELTK--icomp,t,p: ',i4,2f11.6)
c
      RETURN
      end                                              !subroutine MELTK
c
c ======================================================================
c
      subroutine MLTH2O (t,p1,p2)
c
c  compute melting line of water, see fluid file for reference
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c       p1--higher melting line pressure [kPa]
c       p2--lower melting line pressure [kPa]
c       above 273.16 K, only p1 returns a physical answer.
c       between 251.165 and 273.16 K, two pressures are returned.
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  11-17-98 EWL, original version
c  07-03-01 EWL, add patch to fix water equation above 700 K
c  10-22-10 EWL, update with latest equation of Wagner
c
cx DEC$ ATTRIBUTES DLLEXPORT :: MLTH2O
c
      include 'commons.for'
c
      p1=0.0d0
      p2=0.0d0
c     Ice I
      if (t.gt.251.165d0 .and. t.le.273.1600000001d0) then
        th=t/273.16d0
        p2=0.611657d0*(1.d0
     &    +1195393.37d0*(1.0d0-th**3.0d0)
     &    +80818.3159d0*(1.0d0-th**25.75d0)
     &    +3338.26860d0*(1.0d0-th**103.75d0))
      end if
c
c     Ice III
      if (t.ge.251.165d0 .and. t.lt.256.164d0) then
        th=t/251.165d0
        p1=208566d0*(1.0d0-0.299948d0*(1.d0-th**60.d0))
      end if
c
c     Ice V
      if (t.ge.256.164d0 .and. t.lt.273.31d0) then
        th=t/256.164d0
        p1=350100d0*(1.0d0-1.18721d0*(1.d0-th**8.d0))
      end if
c
c     Ice VI
      if (t.ge.273.31d0 .and. t.lt.355.0d0) then
        th=t/273.31d0
        p1=632400d0*(1.0d0-1.07476d0*(1.d0-th**4.6d0))
      end if
c
c     Ice VII
      if (t.ge.355.d0 .and. t.lt.637.8d0) then
        th=t/355.d0
        p1=2216000.d0*EXP(0.173683d+1*(1.d0-th**(-1.d0))
     &                   -0.544606d-1*(1.d0-th**5.d0)
     &                   +0.806106d-7*(1.d0-th**22.d0))
      end if
c
c  The Wagner melting equation for water above its upper limit of 715 K
c  quickly drops and becomes negative above 840 K.  The following patch
c  corrects this problem.  Although the Wagner equation is valid up to 715 K,
c  the equation is used starting at 637.8 K, where the patch and Wagner's
c  equation cross.  Differences between the two equations within these two
c  temperatures is less than 1% up to 693 K, at which point the Wagner
c  equation starts to turn downward.  The maximum 1% error is well within
c  the uncertainty of the original equation in this region, which is about 5%.
c
      if (t.ge.637.8d0) then
        p1=1000.d0*EXP(0.00805246d0*t + 4.27074d0)
      end if

      RETURN
      end                                             !subroutine MLTH2O
Cc
Cc ======================================================================
Cc
C      subroutine MLTH2O (t,p1,p2)
Cc
Cc  compute melting line of water (these are the old coefficients from the
Cc  1994 publication of Wagner, Saul, and Pruss).
Cc
Cc  inputs:
Cc    icomp--component i
Cc        t--temperature (K)
Cc   output:
Cc       p1--higher melting line pressure [kPa]
Cc       p2--lower melting line pressure [kPa]
Cc       above 273.16 K, only p1 returns a physical answer.
Cc       between 251.165 and 273.16 K, two pressures are returned.
Cc
Cc  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
Cc  11-17-98 EWL, original version
Cc  07-03-01 EWL, add patch to fix water equation above 700 K.
Cc
C      include 'commons.for'
Cc
Ccx DEC$ ATTRIBUTES DLLEXPORT :: MLTH2O
Cc
C      p1=0.0d0
C      p2=0.0d0
Cc     Ice I
C      if (t.gt.251.165d0 .and. t.le.273.1600000001d0) then
C        th=t/273.16d0
C        p2=0.000611657d0*(1.0d0-626000.d0*(1.0d0-th**(-3.0d0))
C     &   +197135d0*(1.0d0-th**21.2d0))
C      end if
Cc
Cc     Ice III
C      if (t.ge.251.165d0 .and. t.lt.256.164d0) then
C        th=t/251.165d0
C        p1=209.9d0*(1.0d0-.295252d0*(1.0d0-th**60.0d0))
C      end if
Cc
Cc     Ice V
C      if (t.ge.256.164d0 .and. t.lt.273.31d0) then
C        th=t/256.164d0
C        p1=350.1d0*(1.0d0-1.18721d0*(1.0d0-th**8.0d0))
C      end if
Cc
Cc     Ice VI
C      if (t.ge.273.31d0 .and. t.lt.355.0d0) then
C        th=t/273.31d0
C        p1=632.4d0*(1.0d0-1.07476d0*(1.0d0-th**4.6d0))
C      end if
Cc
Cc     Ice VII
C      if (t.ge.355.d0 .and. t.lt.637.8d0) then
C        th=t/355.d0
C        p1=2216.d0*EXP(0.173683d+1*(1.d0-th**(-1.d0))
C     &                -0.544606d-1*(1.d0-th**5.d0)
C     &                +0.806106d-7*(1.d0-th**22.d0))
C      end if
Cc
Cc  The Wagner melting equation for water above its upper limit of 715 K
Cc  quickly drops and becomes negative above 840 K.  The following patch
Cc  corrects this problem.  Although the Wagner equation is valid up to 715 K,
Cc  the equation is used starting at 637.8 K, where the patch and Wagner's
Cc  equation cross.  Differences between the two equations within these two
Cc  temperatures is less than 1% up to 693 K, at which point the Wagner
Cc  equation starts to turn downward.  The maximum 1% error is well within
Cc  the uncertainty of the original equation in this region, which is about 5%.
Cc
C      if (t.ge.637.8d0) then
C        p1=EXP(0.00805246d0*t + 4.27074d0)
C      end if
Cc
C      p1=p1*1000.0d0
C      p2=p2*1000.0d0
Cc
C      RETURN
C      end                                             !subroutine MLTH2O
c
c ======================================================================
c
      subroutine MLTPH2 (icomp,t,p)
c
c  compute melting line of parahydrogen
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        p--melting line pressure [kPa]
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  11-17-98 EWL, original version
c
      include 'commons.for'
c
      tr=t/trdmlt(icomp)
      pr=0.0d0
c
c     melting function for parahydrogen
      if (t.gt.22) then
        pr=pmeltk(icomp,1)+pmeltk(icomp,2)*tr**pmltex(icomp,2)
      else
        pr=pmeltk(icomp,3)+pmeltk(icomp,4)*tr**pmltex(icomp,4)
      endif
c
      p=prdmlt(icomp)*pr
c
      RETURN
      end                                             !subroutine MLTPH2
c
c
c ======================================================================
c
      subroutine SETSBL (nread,icomp,ierr,herr)
c
c  set up working arrays for use with sublimation line equation
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
c  02-12-99 EWL, original version
c
      include 'commons.for'
      character*255 herr
c
      herr=' '
      ierr=0
c
c  read data from file
c     write (*,*) ' SETSBL--read component',icomp,' from unit',nread
      read (nread,*) tminsb(icomp)          !lower temperature limit
      read (nread,*) tmaxsb(icomp)          !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) trdsbl(icomp),prdsbl(icomp)!reducing parameters
      read (nread,*) ntrmb1(icomp),ntrmb2(icomp),ntrmb3(icomp),
     &               ntrmb4(icomp),ntrmb5(icomp),ntrmb6(icomp)
      do k=1,ntrmb1(icomp)
        read (nread,*) psublk(icomp,k),psblex(icomp,k)
      enddo
      do k=1,ntrmb2(icomp)
        j=k+ntrmb1(icomp)
        read (nread,*) psublk(icomp,j),psblex(icomp,j)
      enddo
      do k=1,ntrmb3(icomp)
        j=k+ntrmb1(icomp)+ntrmb2(icomp)
        read (nread,*) psublk(icomp,j),psblex(icomp,j)
      enddo
c
      RETURN
      end                                             !subroutine SETSBL
c
c ======================================================================
c
      subroutine SUBLT (t,x,p,ierr,herr)
c
c  compute the sublimation line pressure as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c   output:
c        p--sublimation line pressure [kPa]
c        ierr--error flag:  0 = successful
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  02-12-99 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SUBLT
c
      include 'commons.for'
      dimension x(ncmax)
      character*255 herr
c
      p=0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call SUBLK (icomp,t,p,ierr,herr)
c     else if (hsubl.eq.'SBX' .or. hsubl.eq.'SBM') then
      else
c  mixture sublimation line--use simple mixing rules
        ttrp=0
        ptrp=0
        do i=1,nc
          ttrp=ttrp+x(i)*ttpn(i)
          ptrp=ptrp+x(i)*log(ptpn(i))
        enddo
        ptrp=exp(ptrp)
        sum=0
        do i=1,nc
          call SUBLK (i,t/ttrp*ttpn(i),p,ierr,herr)
          if (p.gt.0 .and. ptpn(i).gt.0) sum=sum+x(i)*LOG(p/ptpn(i))
        enddo
        p=EXP(sum)*ptrp
      end if
c
      RETURN
      end                                              !subroutine SUBLT
c
c ======================================================================
c
      subroutine SUBLP (p,x,t,ierr,herr)
c
c  compute the sublimation line temperature as a function of pressure
c  and composition.
c
c  inputs:
c        p--sublimation line pressure [kPa]
c        x--composition [array of mol frac]
c   output:
c        t--temperature [K]
c        ierr--error flag:  0 = successful
c        herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  02-12-99 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SUBLP
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      t=300
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (p.gt.ptpn(icomp)) then
          ierr=2
          t=ttpn(icomp)
          write (herr,1149) p/1000.0d0,ptpn(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1149     format ('[MELTP error 2] ',
     &            'pressure greater than triple point pressure; P =',
     &          g12.5,' MPa, Ptp =',g12.5,' MPa.',a1)
          return
        endif
      endif
c
      if (p.gt.0.d0) call solvem (2,p,x,t,ierr,herr)
c
      RETURN
      end                                              !subroutine SUBLP
c
c ======================================================================
c
      subroutine SUBLK (icomp,t,p,ierr,herr)
c
c  compute sublimation line with appropriate core model
c
c  inputs:
c    icomp--component i
c        t--temperature (K)
c   output:
c        p--sublimation line pressure [kPa]
c     ierr--error flag:  0 = successful
c                        15 = T>Ttrp
c                        105 = unknown model
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  02-12-99 EWL, original version
c  10-17-01 EWL, change SB2 to SB3 and add SB2 similar to ML2
c
      include 'commons.for'
      character*255 herr
c
      p=0
      ierr=0
      herr=' '
      if (t.le.0.0d0) then
        p=0.0d0
        ierr=15
        write (herr,1097) t
        call ERRMSG (ierr,herr)
 1097   format ('[SUBLK error 15] ',
     &          'temperature less than zero; T =',g12.5,' K',a1)
        return
      endif
      if (t.gt.ttpn(icomp)) then
        p=0.0d0
        ierr=15
        write (herr,1098) t,ttpn(icomp),hnull
        call ERRMSG (ierr,herr)
 1098   format ('[SUBLK error 15] ',
     &         'temperature greater than triple point temperature; T =',
     &          g12.5,' K, Ttp =',g12.5,' K.',a1)
        return
      endif
c
      if (hsublk(icomp)(1:2).eq.'SB') then
        tr=t/trdsbl(icomp)
        pr=0.0d0
        do k=1,ntrmb1(icomp)
          pr=pr+psublk(icomp,k)*tr**psblex(icomp,k)
        enddo
        do k=1,ntrmb2(icomp)
          j=k+ntrmb1(icomp)
          pr=pr+psublk(icomp,j)*(1.0d0-tr)**psblex(icomp,j)
        enddo
        do k=1,ntrmb3(icomp)
          j=k+ntrmb1(icomp)+ntrmb2(icomp)
          pr=pr+psublk(icomp,j)*log(tr)**psblex(icomp,j)
        enddo
        p=pr
        if (hsublk(icomp).eq.'SB1') p=prdsbl(icomp)*pr
        if (pr.lt.200) then
          if (hsublk(icomp).eq.'SB2') p=prdsbl(icomp)*exp(pr)
          if (hsublk(icomp).eq.'SB3' .and. t.gt.0.d0)     !used for CO2
     &        p=prdsbl(icomp)*exp(trdsbl(icomp)/t*pr)
        else
          pr=0.d0
        endif
c  do not return error message if fluid contains no melting line
      elseif (hsublk(icomp).eq.'NBS') then
      else
        p=0.0d0
        ierr=105
        write (herr,1099) hsublk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[SUBLK error 105] ',
     &          'unknown sublimation line model:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,t,p
c1200 format (' SUBLK--icomp,t,p: ',i4,2f11.6)
c
      RETURN
      end                                              !subroutine SUBLK
c
c ======================================================================
c
      subroutine SOLVEM (iflag,p,x,t,ierr,herr)
c
c  solve for the melting or sublimation temperature given p
c
c  inputs:
c     iflag--solve for melting pressure when iflag=1, for sublimation pressure
c           when iflag=2
c        p--pressure [kPa]
c        x--composition [array of mol frac]
c   output:
c        t--temperature (K)
c     ierr--error iflag:  0 = successful
c                       124 = no convergence
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  02-18-99 EWL, original version
c  03-08-00 EWL, add check for t<ttrp
c  10-24-00 EWL, add logic for ICE I (for water)
c  07-20-10 EWL, change how t is modified if pp<0
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      tol=0.5d-8
      dx=0.001d0
      dt=-10.d0
      if (iflag.eq.2) dx=-dx
c  get triple point, use as starting place
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        ttrp=ttpn(icomp)
      else
        ttrp=0
        do i=1,nc
          ttrp=ttrp+x(i)*ttpn(i)
        enddo
      endif
      iw=0
      if (icomp.ne.0) then
        if (hmeltk(icomp).eq.'MLW' .and. iflag.eq.1) then
          ttrp=251.165d0
          call MELTT (ttrp,x,pw,ierr,herr)
          if (p.lt.pw) iw=1      !Water phase below 209.9 MPa (Ice I)
        endif
      endif
      t=ttrp+1.0d0
      if (iflag.eq.2) t=ttrp-1.0d0
c  calculate melting (sublimation) pressure and its derivative,
c  then update temperature iteratively
      do i=1,100
        if (iflag.eq.1) then
          if (iw.eq.0) then
            call MELTT (t   ,x,pp,ierr,herr)
            call MELTT (t+dx,x,p1,ierr,herr)
          else
            call MLTH2O (t   ,px,pp)
            call MLTH2O (t+dx,px,p1)
          endif
        elseif (iflag.eq.2) then
          call SUBLT (t   ,x,pp,ierr,herr)
          call SUBLT (t+dx,x,p1,ierr,herr)
        endif
        if (ierr.ne.0) return
        told=t
        if (pp.le.0.d0 .or. p1.le.0.d0) then
          t=t-0.5d0*dt
          if (iw.eq.1) t=told-(273.16d0-told)/2.d0
          dx=dx/2.d0
        else
          dpt=(log(p1)-log(pp))/dx
          dt = (log(p)-log(pp))/dpt       !1st order Newton's method
          t=t+dt
        endif
        if (iflag.eq.1 .and. t.lt.ttrp) t=(told+ttrp)/2.d0
        if (iflag.eq.2 .and. t.gt.ttrp) t=(told+ttrp)/2.d0
        if (iw.eq.1 .and. t.gt.273.16d0) t=(273.16d0+told)/2.d0
        if (t.lt.0) t=0.5d0*told          !do not let t become negative
        if (ABS(p-pp).lt.tol) RETURN
      enddo
      ierr=124
      t=-9.99999d6
      write (herr,1100)
      call ERRMSG (ierr,herr)
 1100 format('[SOLVEM error 124] maximum number of iterations exceeded')
      RETURN
      end                                             !subroutine SOLVEM
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file core_MLT.f
c ======================================================================
