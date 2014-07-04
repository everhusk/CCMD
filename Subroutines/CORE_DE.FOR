c  begin file core_DE.f
c
c  This file contains core routines for the dielectric constant.
c
c  contained here are:
c     subroutine DIELEC (t,rho,x,de)
c     subroutine SETDE (nread,icomp,hcasno,ierr,herr)
c     subroutine DEK (icomp,t,rho,de,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      subroutine DIELEC (t,rho,x,de)
c
c  compute the dielectric constant as a function of temperature, density,
c  and composition.
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c   output:
c       de--dielectric constant
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  07-01-98 EWL, original version
c  08-05-04 EWL, add mixture equation
c  03-02-05 EWL, add error checking
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DIELEC
c
      include 'commons.for'
      dimension x(ncmax)
      character*255 herr
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        call DEK (icomp,t,rho,de,ierr,herr)
      else if (hdiel.eq.'DEX' .or. hdiel.eq.'DEM') then
c  mixture calculation (from Harvey and Lemmon, 2005)
        vz=0.d0
        pm=0.d0
        do i=1,nc
          vz=vz+x(i)/rhoz(i)
        enddo
        dr=rho*vz
        do i=1,nc
          call DEK (i,t,dr*rhoz(i),dei,ierr,herr)
          if (ierr.ne.0) then
            de=0.d0
            RETURN
          endif
          p=(dei-1.d0)*(2.d0*dei+1.d0)/9.d0/dei
          pm=pm+x(i)/rhoz(i)/vz*p
        enddo
        de=0.25d0*(1.d0+9.d0*pm+3.d0*SQRT(9.d0*pm**2+2.d0*pm+1.d0))
      else
        ierr=99
        de=-9.999d6
        write (herr,1199) hdiel,hnull
        call ERRMSG (ierr,herr)
 1199   format ('[DE error 99] ',
     &          'unknown dielectric constant model:  (',a3,')',a1)
c       write (*,*) ' DE--output de (ierr = 99):  ',de
      end if
c
      RETURN
      end                                             !subroutine DIELEC
c
c ======================================================================
c
      subroutine SETDE (nread,icomp,hcasno,ierr,herr)
c
c  set up working arrays for use with "DE" dielectric constant model
c
c  inputs:
c    nread--file to read data from (file should have already been
c           opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c   hcasno--CAS number of component icomp (not required, it is here
c           to maintain parallel structure with SETBWR and SETFEQ)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                        101 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  07-01-98 EWL, original version
c
      include 'commons.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETDE error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
        RETURN
      else
        herr=' '
        ierr=0
      end if
c
c  read data from file
c     write (*,*) ' SETDE--read component',icomp,' from unit',nread
      read (nread,*) tminde(icomp)          !lower temperature limit
      read (nread,*) tmaxde(icomp)          !upper temperature limit
c  the pressure and density limit are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
c  read reducing parameters and coefficients
      read (nread,*) tredde(icomp),dredde(icomp),predde(icomp)
      read (nread,*) ntrmd1(icomp),ntrmd2(icomp),ntrmd3(icomp),
     &               ntrmd4(icomp),ntrmd5(icomp),ntrmd6(icomp)
      do k=1,ntrmd1(icomp)
        read (nread,*) decf(icomp,k),deexpt(icomp,k),deexpd(icomp,k),
     &                 deexpp(icomp,k)
      enddo
      do k=1,ntrmd2(icomp)
        j=k+ntrmd1(icomp)
        read (nread,*) decf(icomp,j),deexpt(icomp,j),deexpd(icomp,j),
     &                 deexpp(icomp,j)
      enddo
      do k=1,ntrmd3(icomp)
        j=k+ntrmd1(icomp)+ntrmd2(icomp)
        read (nread,*) decf(icomp,j),deexpt(icomp,j),deexpd(icomp,j),
     &                 deexpp(icomp,j)
      enddo
c
      RETURN
      end                                              !subroutine SETDE
c
c ======================================================================
c
      subroutine DEK (icomp,t,rho,de,ierr,herr)
c
c  compute dielectric constant with appropriate core model
c
c  inputs:
c    icomp--component i
c      tau--dimensionless temperature (1 - T/Tc)
c   output:
c       de--dielectric constant [N/m]
c     ierr--error flag:  0 = successful
c                        1 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  04-17-97 EWL, original version (based on DE)
c  09-02-99 EWL, return if pi<0
c  08-05-04 EWL, add DE3 and DE4
c  02-22-10 EWL, add check for tau=1
c  03-11-13 EWL, add check for negative values raised to the 0th or 1st power
c
      include 'commons.for'
      character*255 herr
      double precision na,mu,kk
      dimension x(ncmax)
c
      de=0
      ierr=0
      herr=' '
c
      if (rho.lt.1.d-12) then
        de=1.d0
      elseif (hdielk(icomp).eq.'DE1') then
        call press(t,rho,x,p)
        pi=p/1000.0d0/predde(icomp)
        if (pi.lt.0) return
        del=rho/dredde(icomp)
        tau=t/tredde(icomp)
        cm=0.0d0
        do k=1,ntrmd1(icomp)
          cm=cm+decf(icomp,k)*tau**deexpt(icomp,k)*del**deexpd(icomp,k)
     &         *pi**deexpp(icomp,k)
        enddo
        do k=1,ntrmd2(icomp)
          j=k+ntrmd1(icomp)
          cm=cm+decf(icomp,j)*tau**deexpt(icomp,j)*del**deexpd(icomp,j)
     &         *pi**deexpp(icomp,j)*log(1.0d0+1.0d0/tau)
        enddo
        de=(1.0d0+2.0d0*cm)/(1.0d0-cm)
      elseif (hdielk(icomp).eq.'DE2') then
        na=6.0221367d23          !1/mol
        alpha=1.636d-40          !C^2.m^2/J
        mu=6.138d-30             !C.m
        eps=8.854187817d-12      !C^2/J.m
        kk=1.380658d-23           !J/K
        del=rho/dredde(icomp)
        tau=tredde(icomp)/t
        g=1
        do k=1,ntrmd1(icomp)
          g=g+decf(icomp,k)*tau**deexpt(icomp,k)*del**deexpd(icomp,k)
        enddo
        do k=1,ntrmd2(icomp)
         j=k+ntrmd1(icomp)
         t1=t/deexpt(icomp,j)-1.d0
         if (t1.le.0) RETURN
         g=g+decf(icomp,j)*del**deexpd(icomp,j)/t1**deexpp(icomp,j)
        enddo
c
c  Harris-Alder model:
        a = 1000.d0*na*rho*g*mu**2/(eps*kk*t)
        b = 1000.d0*na*rho*alpha/(3.d0*eps)
        c = dsqrt(9.d0 + 2.d0*a + 18.d0*b + a**2 + 10.d0*a*b+9.d0*b**2)
        de = (1.d0 + a + 5.d0*b + c)/(4.d0 - 4.d0*b)
      elseif (hdielk(icomp).eq.'DE3' .or. hdielk(icomp).eq.'DE4') then
        del=rho/dredde(icomp)
        tau=t/tredde(icomp)
        cm=0.0d0
        do k=1,ntrmd1(icomp)
          cm=cm+decf(icomp,k)*tau**deexpt(icomp,k)*del**deexpd(icomp,k)
        enddo
        do k=1,ntrmd2(icomp)
          j=k+ntrmd1(icomp)
          if (abs(deexpt(icomp,j)).lt.1.d-20) then
            cm=cm+decf(icomp,j)*del**deexpd(icomp,j)
          elseif (abs(deexpt(icomp,j)-1.d0).lt.1.d-20) then
            cm=cm+decf(icomp,j)*(tau-1.d0)*del**deexpd(icomp,j)
          else
            cm=cm+decf(icomp,j)*(tau-1.d0)**deexpt(icomp,j)
     &        *del**deexpd(icomp,j)
          endif
        enddo
        do k=1,ntrmd3(icomp)
          j=k+ntrmd1(icomp)+ntrmd2(icomp)
          if (abs(deexpt(icomp,j)).lt.1.d-20) then
            cm=cm+decf(icomp,j)*del**deexpd(icomp,j)
          elseif (abs(deexpt(icomp,j)-1.d0).lt.1.d-20) then
            cm=cm+decf(icomp,j)*(1.d0/tau-1.d0)*del**deexpd(icomp,j)
          else
            cm=cm+decf(icomp,j)*(1.d0/tau-1.d0)**deexpt(icomp,j)
     &        *del**deexpd(icomp,j)
          endif
        enddo
        if (hdielk(icomp).eq.'DE3') then
          de=(1.0d0+2.0d0*cm)/(1.0d0-cm)
        else
          de=0.25d0*(1.d0+9.d0*cm+3.d0*SQRT(9.d0*cm**2+2.d0*cm+1.d0))
        endif
      else
        ierr=1
        de=-9.99999d6
        write (herr,1099) hdielk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[DE error 99] ',
     &          'unknown dielectric constant model:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,tau,de
c1200 format (' DEK--icomp,tau,de: ',i4,2f11.6)
c
      RETURN
      end                                                !subroutine DEK
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                     end file core_DE.f
c ======================================================================
