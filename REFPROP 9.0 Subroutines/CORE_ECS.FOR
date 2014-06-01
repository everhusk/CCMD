c  begin file core_ECS.f
c
c  This file contains routines implementing an extended corresponding
c  states model with temperature- and density-dependent shape factors.
c
c  contained here are:
c     function PHIECS (icomp,itau,idel,tau,del)
c     subroutine CRTECS (icomp,tc,pc,rhoc)
c     subroutine SETECS (nread,icomp,hcasno,hreff,heqn,ierr,herr)
c     subroutine FJ (icomp,t,d,f,dfdt,d2fdt2,dfdd,d2fdd2,d2fdtd)
c     subroutine HJ (icomp,t,d,h,dhdt,d2hdt2,dhdd,d2hdd2,d2hdtd)
c
c ======================================================================
c ======================================================================
c
      function PHIECS (icomp,itau,idel,tau,del)
c
c  compute reduced Helmholtz energy or a derivative as functions
c  of dimensionless temperature and density for the ECS model
c
c  inputs:
c    icomp--pointer specifying component (1..nc)
c     itau--flag specifying order of temperature derivative to calc
c     idel--flag specifying order of density derivative to calculate
c           when itau = 0 and idel = 0, compute A/RT
c           when itau = 0 and idel = 1, compute 1st density derivative
c           when itau = 1 and idel = 1, compute cross derivative
c           etc.
c      tau--dimensionless temperature (To/T)
c      del--dimensionless density (D/Do)
c  output (as function value):
c      phi--residual (real-gas) part of the Helmholtz energy, or one
c           of its derivatives (as specified by itau and idel),
c           in reduced form (A/RT)
c           itau  idel    output (dimensionless for all cases)
c             0    0      A/RT
c             1    0      tau*[d(A/RT)/d(tau)]
c             2    0      tau**2*[d**2(A/RT)/d(tau)**2]
c             0    1      del*[d(A/RT)/d(del)]
c             0    2      del**2*[d**2(A/RT)/d(del)**2]
c             1    1      tau*del*[d**2(A/RT)/d(tau)d(del)]
c                         etc.
c
c        The Helmholtz energy consists of ideal and residual (real-gas)
c        terms; this routine calculates only the residual part.
c
c        This function computes pure component properties only.
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-02-96  MM, original version
c  03-19-96  MM, add dipole moment to /CCON/
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  09-23-96 EWL, change calls to FJ, HJ to include density dependence
c  11-13-97 EWL, check for itau.ge.1 before idel.ge.1
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  01-26-00 EWL, add check for del>=0 and tau>=0
c  09-05-00 EWL, return 0 for invalid idel or itau
c  01-17-01 EWL, change tau0.ge.0 to tau0.gt.0 and avoid 0**0
c
      include 'commons.for'
c
      phiecs=0.0d0    !initialize outputs and intermediate results
      phi01=0.0d0
      phi02=0.0d0
      phi10=0.0d0
      phi20=0.0d0
      phi11=0.0d0
c
      if (del.le.1.0d-10) then    !trivial solution at zero density
        RETURN                    !for any and all derivatives
      end if
c
c  recover the temperature and density from tau and del (need to compute
c  shape factors at actual temperature rather than reduced temperature)
c
      i=icomp
      t=tcrit(i)/tau
      rho=del*Dcrit(i)
      call FJ (icomp,t,rho,f,dfdt,d2fdt2,dfdd,d2fdd2,d2fdtd)
      call HJ (icomp,t,rho,h,dhdt,d2hdt2,dhdd,d2hdd2,d2hdtd)
c
c  find the reducing temperature to compute tau for the reference fluid
c  and then calculate reduced Helmholtz for the reference fluid at the
c  conformal temperature and density
c
c  note:  cannot use the PHIK function here because it calls this one,
c         and recursive calls are not generally allowed in fortran
c
      iref=-icomp
      tred0=tz(iref)
      Dred0=rhoz(iref)
      if (hmxeos(iref).eq.'BWR') then
        call CRTBWR (iref,tc0,pc0,Dc0)
        tred0=tc0
        Dred0=Dc0
        tau0=tred0/t*f
        del0=rho*h/Dred0
c  compute PHI with same order as input arguments
        phixx=PHIBWR (iref,itau,idel,tau0,del0)
        phixx=phixx/del0**idel/tau0**itau
c  calculate additional PHIs as required
        if (itau.ge.1) then
          phi01=PHIBWR (iref,0,1,tau0,del0)     /del0
          if (idel.eq.1) then
            phi02=PHIBWR (iref,0,2,tau0,del0)   /del0**2
            phi10=PHIBWR (iref,1,0,tau0,del0)   /tau0
            phi20=PHIBWR (iref,2,0,tau0,del0)   /tau0**2
          else if (itau.eq.2) then
            phi10=PHIBWR (iref,1,0,tau0,del0)   /tau0
            phi11=PHIBWR (iref,1,1,tau0,del0)   /del0/tau0
            phi02=PHIBWR (iref,0,2,tau0,del0)   /del0**2
          end if
        end if
        if (idel.ge.1) then
          phi10=PHIBWR (iref,1,0,tau0,del0)     /tau0
          if (idel.eq.2) then
            phi01=PHIBWR (iref,0,1,tau0,del0)   /del0
            phi11=PHIBWR (iref,1,1,tau0,del0)   /del0/tau0
            phi20=PHIBWR (iref,2,0,tau0,del0)   /tau0**2
          end if
        end if
      else if (hmxeos(iref)(1:2).eq.'FE') then
        tau0=tred0/t*f
        del0=rho*h/Dred0
c  compute PHI with same order as input arguments
        phixx=0
        if (del0.ge.0 .and. tau0.gt.0) then
          phixx=PHIFEQ (iref,itau,idel,tau0,del0)
          phixx=phixx/del0**idel/tau0**itau
c  calculate additional PHIs as required
          if (itau.ge.1) then
            phi01=PHIFEQ (iref,0,1,tau0,del0)   /del0
            if (idel.eq.1) then
              phi02=PHIFEQ (iref,0,2,tau0,del0) /del0**2
              phi10=PHIFEQ (iref,1,0,tau0,del0) /tau0
              phi20=PHIFEQ (iref,2,0,tau0,del0) /tau0**2
            else if (itau.eq.2) then
              phi10=PHIFEQ (iref,1,0,tau0,del0) /tau0
              phi11=PHIFEQ (iref,1,1,tau0,del0) /del0/tau0
              phi02=PHIFEQ (iref,0,2,tau0,del0) /del0**2
            end if
          end if
          if (idel.ge.1) then
            phi10=PHIFEQ (iref,1,0,tau0,del0)   /tau0
            if (idel.eq.2) then
              phi01=PHIFEQ (iref,0,1,tau0,del0) /del0
              phi11=PHIFEQ (iref,1,1,tau0,del0) /del0/tau0
              phi20=PHIFEQ (iref,2,0,tau0,del0) /tau0**2
            end if
          end if
        end if
      else
c  reference fluid not found, but no way to return an error from here
        phixx=-9.99d99
      end if
c
c  check if derivatives are requested
c
      if (itau.eq.0) then
        if (idel.eq.0) then
c  compute dimensionless residual Helmholtz of reference fluid
          phiecs=phixx
        else if (idel.eq.1 .and. ABS(f).gt.1.d-20) then
c  compute 1st derivative w.r.t. del (dimensionless density)
          phiecs=Dcrit(i)/Dred0*phixx*(h + rho*dhdd)
     &          +Dcrit(i)*tau0/f*phi10*dfdd
        else if (idel.eq.2) then
c  compute 2nd derivative w.r.t. del
          phiecs=Dcrit(i)**2*(((phixx*(h+rho*dhdd)/dred0
     &    +tred0/t*phi11*dfdd)*(h+rho*dhdd)
     &    +phi01*(2.d0*dhdd+rho*d2hdd2))/dred0
     &    +tred0/t*((phi11/dred0*(h+rho*dhdd)+tred0/t*phi20*dfdd)*dfdd
     &    +d2fdd2*phi10))
        end if
c
      else if (itau.eq.1) then
        dtlogh=dhdt/h  !equal to temperature derivative of log(h)
        if (idel.eq.0) then
c  compute 1st derivative w.r.t. tau (dimensionless temperature)
          phiecs=tred0/tcrit(i)*(f-t*dfdt)*phixx
     &          -t*t*del0/tcrit(i)*dtlogh*phi01
        else if (idel.eq.1) then
c  compute cross derivative
          phiecs=Dcrit(i)/Dred0/tcrit(i)*
     &          (tred0*(f-t*dfdt)*(phixx*(h + rho*dhdd)
     &          +tred0/t*dred0*phi20*dfdd)
     &          +tred0*phi10*Dred0*(dfdd - t*d2fdtd)
     &          -t*t*(phi01*dhdt+rho*dhdt*(phi02/dred0*(h + rho*dhdd)
     &          +tred0/t*phixx*dfdd)
     &          +rho*phi01*d2hdtd))
        end if
c
      else if (itau.eq.2) then
c  compute 2nd derivative w.r.t. tau
        dtlogh=dhdt/h  !equal to temperature derivative of log(h)
        phiecs=(1.0d0/tcrit(i)**2)*
     &        (tred0**2*(f-t*dfdt)**2*phixx
     &        -2.0d0*t*t*tred0*del0*(f-t*dfdt)*dtlogh*phi11
     &        +t**4*del0**2*dtlogh**2*phi02
     &        +tred0*t**3*d2fdt2*phi10
     &        +t**3*del0/h*(t*d2hdt2+2.0d0*dhdt)*phi01)
      else
c  invalid itau and/or idel, but no way to return an error from here
c       phiecs=-9.99d99
      end if
      phiecs=phiecs*del**idel*tau**itau
c
      RETURN
      end                                               !function PHIECS
c
c ======================================================================
c
      subroutine CRTECS (icomp,tc,pc,rhoc)
c
c  returns critical parameters associated with ECS model
c
c  N.B.  these critical parameters may not necessarily be most
c        accurate values, but they are consistent with the ECS fit
c
c  input:
c    icomp--pointer specifying component (1..nc)
c  outputs:
c       tc--critical temperature [K]
c       pc--critical pressure [kPa]
c     rhoc--molar density [mol/L] at critical point
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-06-96  MM, original version
c  03-19-96  MM, add dipole moment to /CCON/
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c
      include 'commons.for'
c
      tc=tcrit(icomp)
      pc=pcrit(icomp)
      rhoc=Dcrit(icomp)
c
      RETURN
      end                                             !subroutine CRTECS
c
c ======================================================================
c
      subroutine SETECS (nread,icomp,hcasno,hreff,heqn,ierr,herr)
c
c  set up working arrays for use with ECS model
c
c  inputs:
c    nread--file to read data from (file should have already been
c           opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c   hcasno--CAS number of component icomp (not required, it is here
c           to maintain parallel structure with SETBWR and SETFEQ)
c
c  outputs:
c     hreff--file containing reference fluid EOS (character*255)
c     heqn--model ('BWR', etc) for reference fluid EOS (character*3)
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-06-95  MM, original version
c  03-19-96  MM, add dipole moment to /CCON/
c  03-21-96  MM, delete reference to /MODEL/, not needed
c  03-22-96  MM, replace /CPMOD/ with /EOSMOD/
c  06-03-96  MM, add limits to /EOSLIM/
c  09-23-96 EWL, add density dependent shape factors
c  09-30-96  MM, change order of density factors in .fld files
c  08-19-97  MM, get rid of herr=herr, etc (avoid warning); flag nread<=0
c  07-08-98 EWL, change character strings from *80 to *255
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-22-98 EWL, set Reos to 8.31451
c  06-22-99 EWL, reset ptp and dtp to zero
c  06-22-99 EWL, change Reos to 8.314472, set R to Reos
c  05-03-04 EWL, change dtp to rhomax
c
      include 'commons.for'
      character*3 heqn
      character*12 hcasno
      character*255 hreff
      character*255 herr
c
      if (nread.le.0) then
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETECS error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
        RETURN
      else
        herr=' '
        ierr=0
      end if
c
c  read data from file
c     write (*,*) ' SETECS--read component',icomp,' from unit',nread
c     iref=-icomp
      Reos(icomp)=8.314472d0
      if (nc.eq.1 .and. icomp.eq.1) R=Reos(icomp)
      ptpn(icomp)=0.0d0
      dtpv(icomp)=0.0d0
      dnbpl(icomp)=0.0d0
      dnbpv(icomp)=0.0d0
      read (nread,*) tminecs(icomp)          !lower temperature limit
      read (nread,*) tmaxecs(icomp)          !upper temperature limit
      read (nread,*) pmaxecs(icomp)          !upper pressure limit
      read (nread,*) dmaxecs(icomp)        !upper density limit
      read (nread,2003) hmodcp(icomp)     !pointer to Cp0 model
      read (nread,2080) hreff              !reference fluid .fld file
      read (nread,2003) heqn              !ref fluid equation of state
      read (nread,*) acfref(icomp)        !acc fac for reference fluid
      read (nread,*) Zcref(icomp)         !Zc for reference fluid
      read (nread,*) acfecs(icomp)        !acentric factor
      read (nread,*) tcrit(icomp)         !critical temperature [K]
      read (nread,*) pcrit(icomp)         !critical pressure [kPa]
      read (nread,*) Dcrit(icomp)         !critical density [mol/L]
      tz(icomp)=tcrit(icomp)
      rhoz(icomp)=Dcrit(icomp)
c     write (*,1020) tcrit(icomp),pcrit(icomp),Dcrit(icomp)
c1020 format (1x,' SETECS--critical T, p, rho: ',f8.3,f8.2,f8.5)
      Zcrit(icomp)=pcrit(icomp)/(Reos(icomp)*tcrit(icomp)*Dcrit(icomp))
c  copy ECS parameters to general fluid constants common block
c     accen(icomp)=acfecs(icomp)
      read (nread,*) nfecs(icomp)    !no. temperature terms for 'f' ESRR
      do j=1,nfecs(icomp)
        read (nread,*) fecs(icomp,j,1),fecs(icomp,j,2)
      enddo
      read (nread,*) nfdecs(icomp)   !no. density terms for 'f' ESRR
      if (nfdecs(icomp).ge.1) then
        do j=1,nfdecs(icomp)
          read (nread,*) fdecs(icomp,j,1),fdecs(icomp,j,2)
        enddo
      end if
      read (nread,*) nhecs(icomp)    !no. temperature terms for 'h' ESRR
      do j=1,nhecs(icomp)
        read (nread,*) hecs(icomp,j,1),hecs(icomp,j,2)
      enddo
      read (nread,*) nhdecs(icomp)   !no. density terms for 'h' ESRR
      if (nhdecs(icomp).ge.1) then
        do j=1,nhdecs(icomp)
          read (nread,*) hdecs(icomp,j,1),hdecs(icomp,j,2)
        enddo
      end if
c
c  copy limits into /EOSLIM/ arrays
      tmeos(icomp)=tminecs(icomp)
      txeos(icomp)=tmaxecs(icomp)
      peos(icomp)=pmaxecs(icomp)
      Deos(icomp)=dmaxecs(icomp)
      dtpn(icomp)=dmaxecs(icomp)
c
      RETURN
 2003 format (a3)
 2080 format (a255)
      end                                             !subroutine SETECS
c
c ======================================================================
c
      subroutine FJ (icomp,t,d,f,dfdt,d2fdt2,dfdd,d2fdd2,d2fdtd)
c
c  compute the equivalent substance reducing ratio (for temperature)
c  and its temperature derivative for use in the ECS model
c
c  This routine implements the function of Huber & Ely, Int. J. Refrig.
c  17:18-31 (1994) modified by additional general terms in (T/Tc) and by
c  density-dependent terms.  The beta1, beta2 of Huber & Ely are
c  hecs(i,1,1) and hecs(i,2,1) here.
c
c  inputs:
c    icomp--pointer specifying component (1..nc)
c        t--temperature [K]
c        d--density [mol/L]
c  outputs:
c        f--equivalent substance reducing ratio (for temperature)
c           this is equal to (Tc,icomp/Tc,ref)*theta(T)
c           where theta is the energy (or temperature) shape factor
c     dfdt--temperature derivative of f
c   d2fdt2--second temperature derivative of f
c     dfdd--density derivative of f
c   d2fdd2--second density derivative of f
c   d2fdtd--temperature-density cross derivative of f
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-06-96  MM, original version
c  03-19-96  MM, add dipole moment to /CCON/
c  09-23-96 EWL, add density dependent shape factors
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  01-17-01 EWL, initialize values and return if t<=0
c  11-15-07 MLH, remove dead code
c
      include 'commons.for'
c
      f=0.d0
      dfdt=0.d0
      d2fdt2=0.d0
      dfdd=0.d0
      d2fdd2=0.d0
      d2fdtd=0.d0
      if (t.le.0.d0) RETURN
      i=icomp
      iref=-icomp
      Tr=t/tcrit(i)
      Dr=d/Dcrit(i)
      domega=acfecs(i)-acfref(i)
c  theta is the density shape factor
      theta=1.0d0+domega*(fecs(i,1,1)+fecs(i,2,1)*log(Tr))
c  first and second (reduced) temperature derivatives of theta
      d1th=domega*fecs(i,2,1)/Tr        !first Tr derivative of theta
      d2th=-domega*fecs(i,2,1)/(Tr*Tr)  !second Tr derivative of theta
c  sum terms beyond those in Huber & Ely function
      if (nfecs(i).ge.3) then
        do j=3,nfecs(i)
          theta=theta+fecs(i,j,1)*Tr**fecs(i,j,2)
          d1th=d1th+fecs(i,j,1)*fecs(i,j,2)*Tr**(fecs(i,j,2)-1.0d0)
          d2th=d2th+fecs(i,j,1)*fecs(i,j,2)*(fecs(i,j,2)-1.0d0)
     &        *Tr**(fecs(i,j,2)-2.0d0)
        enddo
      end if
      d1thd=0.d0
      d2thd=0.d0
      if (nfdecs(i).gt.0) then
        do j=1,nfdecs(i)
          theta=theta+fdecs(i,j,1)*Dr**fdecs(i,j,2)
          d1thd=d1thd+fdecs(i,j,1)*fdecs(i,j,2)*Dr**(fdecs(i,j,2)-1.d0)
          d2thd=d2thd+fdecs(i,j,1)*fdecs(i,j,2)*(fdecs(i,j,2)-1.d0)
     &         *Dr**(fdecs(i,j,2)-2.d0)
        enddo
      end if
c
c  convert theta and its derivatives to 'f' and its derivatives
      Tcrat=tcrit(icomp)/tcrit(iref)
      f=Tcrat*theta
      dfdt=d1th/tcrit(iref)
      d2fdt2=d2th/(tcrit(icomp)*tcrit(iref))
      dfdd=d1thd*Tcrat/Dcrit(icomp)
      d2fdd2=d2thd*Tcrat/Dcrit(icomp)**2
      !d2fdtd=d1thdt*Tcrat/Dcrit(icomp)/tcrit(icomp)
      d2fdtd=0.0d0 !approximation. if needed, calculation of d1thdt is required.
c
      RETURN
      end                                                 !subroutine FJ
c
c ======================================================================
c
      subroutine HJ (icomp,t,d,h,dhdt,d2hdt2,dhdd,d2hdd2,d2hdtd)
c
c  compute the equivalent substance reducing ratio (for density)
c  and its temperature derivative for use in the ECS model
c
c  This routine implements the function of Huber & Ely, Int. J. Refrig.
c  17:18-31 (1994) modified by additional general terms in (T/Tc) and by
c  density-dependent terms.  The beta1, beta2 of Huber & Ely are
c  hecs(i,1,1) and hecs(i,2,1) here.
c
c  inputs:
c    icomp--pointer specifying component (1..nc)
c        t--temperature [K]
c        d--density [mol/L]
c  outputs:
c        h--equivalent substance reducing ratio (for density)
c           this is equal to (rhoc,ref/rhoc,icomp)*phi(T)
c           where phi is the volume (or density) shape factor
c     dhdt--temperature derivative of h
c   d2hdt2--second temperature derivative of h
c     dhdd--density derivative of h
c   d2hdd2--second density derivative of h
c   d2hdtd--temperature-density cross derivative of h
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-06-96  MM, original version
c  03-19-96  MM, add dipole moment to /CCON/
c  09-23-96 EWL, add density dependent shape factors
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  01-17-01 EWL, initialize values and return if t<=0
c  11-15-07 MLH, remove dead code
c
      include 'commons.for'
c
      h=0.d0
      dhdt=0.d0
      d2hdt2=0.d0
      dhdd=0.d0
      d2hdd2=0.d0
      d2hdtd=0.d0
      if (t.le.0.d0) RETURN
      i=icomp
      iref=-icomp
      Tr=t/tcrit(i)
      Dr=d/Dcrit(i)
      domega=acfecs(i)-acfref(i)
      Zrat=Zcref(i)/Zcrit(i)
c  phi is the density shape factor, not to be confused with PHI=A/RT
      phi=Zrat*(1.0d0+domega*(hecs(i,1,1)+hecs(i,2,1)*log(Tr)))
c
c  first and second (reduced) temperature derivatives of phi
      d1phi=Zrat*domega*hecs(i,2,1)/Tr
      d2phi=-Zrat*domega*hecs(i,2,1)/(Tr*Tr)
c
c  sum terms beyond those in Huber & Ely function
      if (nhecs(i).ge.3) then
        do j=3,nhecs(i)
          phi=phi+hecs(i,j,1)*Tr**hecs(i,j,2)
          d1phi=d1phi+hecs(i,j,1)*hecs(i,j,2)*Tr**(hecs(i,j,2)-1.0d0)
          d2phi=d2phi+hecs(i,j,1)*hecs(i,j,2)*(hecs(i,j,2)-1.0d0)
     &         *Tr**(hecs(i,j,2)-2.0d0)
        enddo
      end if
c
      d1phid=0.0d0
      d2phid=0.0d0
      if (nhdecs(i).gt.0) then
        do j=1,nhdecs(i)
         phi=phi+hdecs(i,j,1)*Dr**hdecs(i,j,2)
         d1phid=d1phid+hdecs(i,j,1)*hdecs(i,j,2)*Dr**(hdecs(i,j,2)-1.d0)
         d2phid=d2phid+hdecs(i,j,1)*hdecs(i,j,2)*(hdecs(i,j,2)-1.d0)
     &        *Dr**(hdecs(i,j,2)-2.d0)
        enddo
      end if
c
c  convert phi and its derivatives to 'h' and its derivatives
      Dcrat=Dcrit(iref)/Dcrit(icomp)
      h=Dcrat*phi
      dhdt=d1phi*Dcrat/tcrit(icomp)
      d2hdt2=d2phi*Dcrat/tcrit(icomp)**2
      dhdd=d1phid*Dcrat/Dcrit(icomp)
      d2hdd2=d2phid*Dcrat/Dcrit(icomp)**2
c
      RETURN
      end                                                 !subroutine HJ
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file core_ECS.f
c ======================================================================
