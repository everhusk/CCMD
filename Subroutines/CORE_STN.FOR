c  begin file core_STN.f
c
c  This file contains core routines for the surface tension.
c
c  contained here are:
c     subroutine SURFT (t,rhol,xl,sigma,ierr,herr)
c     subroutine SURTEN (t,rhol,rhov,xl,xv,sigma,ierr,herr)
c     subroutine SETST1 (nread,icomp,hcasno,ierr,herr)
c     subroutine STN (t,rhol,rhov,xl,xv,sigma,ierr,herr)
c     subroutine STNK (icomp,tau,sigma,ierr,herr)
c     subroutine CRITF (zeta,x,tc,pc,Dc,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      subroutine SURFT (t,rhol,xl,sigma,ierr,herr)
c
c  compute surface tension
c
c  inputs:
c        t--temperature [K]
c       xl--composition of liquid phase [array of mol frac]
c  outputs:
c     rhol--molar density of liquid phase [mol/L]
c           if rho > 0 use as input value
c                  < 0 call SATT to find density
c    sigma--surface tension [N/m]
c     ierr--error flag:   0 = successful
c                         1 = T < Tmin
c                         8 = x out of range
c                         9 = T and x out of range
c                       120 = CRITP did not converge
c                       121 = T > Tcrit
c                       122 = TPRHO-liquid did not converge in SATT
c                       123 = TPRHO-vapor did not converge in SATT
c                       124 = SATT pure fluid iteration did not converge
c                       128 = SATT mixture iteration did not converge
c     herr--error string if ierr<>0 (character*255)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  03-24-96  MM, original version
c  03-27-96  MM, add error checks; move calculations to STN (in core_STN)
c  04-05-96  MM, test for supercritical '.ge. tc' rather than '.gt. tc'
c  05-31-96  MM, if error on call to SATT modify herr and return
c  06-03-96  MM, check input temperature against limits
c  06-07-96  MM, fix loss of LIMITX warnings on calls to SATT, STN
c  04-21-97  MM, delete tcrit from call to STN (t > tcrit check moved to STN)
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-15-97  MM, pass any ierr,herr from STN as outputs
c  06-02-99  MM, always call SATT (need rhov, xv for STH model)
c                [should now call SURTEN; this retained for compatibility]
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SURFT
c
      include 'commons.for'
      character*255 herr,herr2
      dimension xl(ncmax),xliq(ncmax),xvap(ncmax)
c
      ierr=0
      herr=' '
c
c  check that input conditions (in this case t and x) are within limits
c  (check that t < tcrit done in STN)
c
      Ddum=0.d0
      pdum=0.d0
      sigma=0.d0
      call LIMITX ('STN',t,Ddum,pdum,xl,tmin,tmax,Dmx,pmx,ierr,herr2)
      if (t.lt.tmin .and. ierr.le.0) then
        ierr=1
        herr='[SURFT error 1] t<tmin'
        call ERRMSG (ierr,herr)
        RETURN
      elseif (ierr.lt.0) then
c  temperature is outside limits, but in region where extrapolation is
c  usually reliable, set warning flag
        ierr=ierr-20
        write (herr,1002) ierr,herr2(1:234),hnull
 1002   format ('[SURFT warning',i4,'] ',a234,a1)
        call ERRMSG (ierr,herr)
      elseif (ierr.gt.0) then
c  temperature and/or x are outside limits, set error flag
        write (herr,1003) ierr,herr2(1:237),hnull
 1003   format ('[SURFT error',i3,'] ',a237,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
c  calculate density of saturated liquid, if required
c     if (rhol.le.0.0d0) then
c       kph=1
c       call SATT (t,xl,kph,p,rhol,rhov,xliq,xvap,ierr2,herr2)
c       if (ierr2.ne.0) then
c         write (herr,1005) ierr2,herr2(1:237),hnull
c1005     format ('[SURFT error',i3,'] ',a237,a1)
c         call ERRMSG (ierr,herr)
c         if (ierr2.gt.0) RETURN
c       end if
c     end if
c     call STN (t,rhol,xl,sigma,ierr,herr)
c
c  because of model 'STH' in STN, it is necessary to provide rhov, xv
      kph=1
      call SATT (t,xl,kph,p,rhol,rhov,xliq,xvap,ierr2,herr2)
      if (ierr2.ne.0) then
        ierr=ierr2
        write (herr,1005) ierr2,herr2(1:237),hnull
 1005   format ('[SURFT error',i3,'] ',a237,a1)
        call ERRMSG (ierr,herr)
        if (ierr2.gt.0) RETURN
      end if
      call STN (t,rhol,rhov,xl,xvap,sigma,ierr,herr)
c
      RETURN
      end                                              !subroutine SURFT
c
c ======================================================================
c
      subroutine SURTEN (t,rhol,rhov,xl,xv,sigma,ierr,herr)
c
c  compute surface tension
c
c  inputs:
c        t--temperature [K]
c     rhol--molar density of liquid phase [mol/L]
c     rhov--molar density of vapor phase [mol/L]
c           if either rhol or rhov < 0 call SATT to find densities
c       xl--composition of liquid phase [array of mol frac]
c       xv--composition of vapor phase [array of mol frac]
c           (xv is optional input if rhol < 0 or rhov < 0)
c  outputs:
c    sigma--surface tension [N/m]
c     ierr--error flag:   0 = successful
c                         1 = T < Tmin
c                         8 = x out of range
c                         9 = T and x out of range
c                       120 = CRITP did not converge
c                       121 = T > Tcrit
c                       122 = TPRHO-liquid did not converge in SATT
c                       123 = TPRHO-vapor did not converge in SATT
c                       124 = SATT pure fluid iteration did not converge
c                       128 = SATT mixture iteration did not converge
c     herr--error string if ierr<>0 (character*255)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-02-99  MM, original version; based on and replaces SURFT
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SURTEN
c
      include 'commons.for'
      character*255 herr,herr1,herr2
      dimension xl(ncmax),xv(ncmax),xliq(ncmax)
c
      ierr=0
      herr=' '
c
c  check that input conditions (in this case t and x) are within limits
c  (check that t < tcrit done in STN)
c
      Ddum=0.0d0
      pdum=0.0d0
      call LIMITX ('STN',t,Ddum,pdum,xl,tmin,tmax,Dmx,pmx,ierr1,herr1)
      if (rhov.gt.0.0d0) then
c  check input vapor composition only if vapor density is specified
c  (otherwise, vapor comp is computed by a call to SATT, and input value
c  is irrelevant)
        call LIMITX ('STN',t,Ddum,pdum,xv,tmin,tmax,Dmx,pmx,ierr2,herr2)
      else
        ierr2=0
        herr2=hnull
      end if
      if (ierr1.gt.0 .or. ierr2.gt.0) then
c  temperature and/or x are outside limits, set error flag
        if (ierr1.gt.ierr2) then
          ierr=ierr1
          herr2=herr1
        else
          ierr=ierr2
        end if
        write (herr,1002) ierr,herr2(1:236),hnull
 1002   format ('[SURTEN error',i3,'] ',a236,a1)
        call ERRMSG (ierr,herr)
        sigma=xnotc
        RETURN
      else if (ierr1.lt.0) then
c  temperature is outside limits, but in region where extrapolation is
c  usually reliable, set warning flag
        ierr=ierr1-20
        write (herr,1004) ierr,herr1(1:233),hnull
 1004   format ('[SURTEN warning',i4,'] ',a233,a1)
        call ERRMSG (ierr,herr)
      end if
c
c  calculate density of saturated liquid, if required
      if (rhol.le.0.0d0 .or. rhov.le.0.0d0) then
        kph=1
        call SATT (t,xl,kph,p,rhol,rhov,xliq,xv,ierr2,herr2)
        if (ierr2.ne.0) then
          write (herr,1005) ierr2,herr2(1:236),hnull
 1005     format ('[SURTEN error',i3,'] ',a236,a1)
          call ERRMSG (ierr,herr)
          if (ierr2.gt.0) then
            sigma=0.0d0
            RETURN
          end if
        end if
      end if
      call STN (t,rhol,rhov,xl,xv,sigma,ierr,herr)
c
      RETURN
      end                                             !subroutine SURTEN
c
c ======================================================================
c
      subroutine SETST1 (nread,icomp,hcasno,ierr,herr)
c
c  set up working arrays for use with "ST1" surface tension model:
c
c    sigma = sum[sigma_k*tau**sigexp_k]
c      tau = 1 - t/tc
c
c    Note:  The critical temperature used is that of the current
c           equation of state. This may differ slightly from that used
c           in the original correlation of surface tension; this change
c           is necessary to give proper behavior of surface tension near
c           the critical point and to avoid possible numerical crashes.
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
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  03-24-96  MM, original version (skeleton only)
c  08-16-96  MM, add actual ST1 model
c  08-19-97  MM, get rid of herr=herr (avoid warning); flag nread<=0
c  12-02-97  MM, skip over pressure and density limit and Tc on file read
c
      include 'commons.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETST1 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
        RETURN
      else
        herr=' '
        ierr=0
      end if
c
c  read data from file
c     write (*,*) ' SETSTN--read component',icomp,' from unit',nread
      read (nread,*) tminst(icomp)          !lower temperature limit
      read (nread,*) tmaxst(icomp)          !upper temperature limit
c  the pressure and density limit and the Tc are not presently used,
c  but are contained in the file for consistency and possible future use;
c  skip over them in reading the file
      read (nread,*) !pjunk               !upper pressure limit (n/a)
      read (nread,*) !rhojnk              !upper density limit (n/a)
      read (nread,*) ntermu(icomp)
      read (nread,*) !Tcjunk              !Tc in original fit (not used)
      do k=1,ntermu(icomp)
        read (nread,*) sigmak(icomp,k),sigexp(icomp,k)
      enddo
c
      RETURN
      end                                             !subroutine SETST1
c
c ======================================================================
c
      subroutine STN (t,rhol,rhov,xl,xv,sigma,ierr,herr)
c
c  compute surface tension with appropriate core model
c
c  inputs:
c        t--temperature [K]
c     rhol--molar density of liquid phase [mol/L]
c     rhov--molar density of vapor phase [mol/L]
c       xl--composition of liquid phase [array of mol frac]
c       xv--composition of vapor phase [array of mol frac]
c   output:
c    sigma--surface tension [N/m]
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  03-27-96  MM, original version (skeleton only)
c  08-16-96  MM, add actual ST1 model
c  04-17-97  MM, add pcrit to argument, break pures to separate STNK
c                add mixture model (Holcomb's mod of Moldover & Rainwater)
c  04-21-97  MM, delete critical par from arguments, add call to CRITF
c  12-15-97  MM, return if error from CRITF, set sigma = "not calculated"
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  06-02-99  MM, add vapor density and composition to argument list to
c                accommodate new mixture model of Holcomb & Higashi
c
      include 'commons.for'
      character*255 herr,herr2
      dimension xl(ncmax),zeta(ncmax),f(ncmax),cx(ncmax),xcritf(ncmax),
     &          xv(ncmax),zmole(ncmax)
c
      ierr=0
      herr=' '
c
      i=ierrcrt
      ierrcrt=1
      call CRITP (xl,tc,pc,dc,ierr,herr2)
      ierrcrt=i
      if (ierr.gt.0) then
c  error condition--set outputs, issue warning, and return
        ierr=160
        sigma=0.0d0
        write (herr,1016) herr2(1:236),hnull
 1016   format ('[STN error 160] ',a236,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
      if (t.gt.tc) then
        ierr=121
        write (herr,1121) t,tc,hnull
        call ERRMSG (ierr,herr)
 1121   format ('[STN error 121] ',
     &          'temperature input to surface tension routine is ',
     &          'greater than critical temperature; T =',g12.5,
     &          ' K, Tcrit =',g12.5,' K.',a1)
        sigma=0.0d0
c       write (*,*) ' STN--output sigma (ierr = 121):  ',sigma
        RETURN
      end if
      call ISPURE (xl,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        tau=1.0d0-t/tc
        call STNK (icomp,tau,sigma,ierr,herr)
        RETURN
      end if
c  base surface tensions on critical parameters at same composition,
      tau=1.0d0-t/tc
      tau126=tau**1.26d0
      alpha=0.10d0
      const=3.74d0**1.5d0*SQRT(R)*alpha*(1.0d0-alpha)*(2.0d0-alpha)
      do i=1,nc
c  define effective parameter in function sigma = sigma0*tau**1.26
        call STNK (i,tau,sigk,ierr,herr)
        if (sigk.lt.0) RETURN
        sig0=0.d0
        if (tau126.gt.0.d0) sig0=sigk/tau126
c     write (*,*) ' STN--icomp, effective sigma_0: ',icomp,sig0
        cx(i)=sig0**1.50d0/(const*SQRT(Tcrit(i))*Pcrit(i))
      enddo
      if (hsten.eq.'STX' .or. hsten.eq.'STM') then
c  mixture case--apply mixing rules to the cx(i)
c  compute fugacities and fugacity fraction
        call FGCTY2 (t,rhol,xl,f,ierr,herr)
        fsum=0.0d0
        do i=1,nc
          fsum=fsum+f(i)
        enddo
        do i=1,nc
          zeta(i)=f(i)/fsum
          xcritf(i)=xl(i)   !initial guess for crit comp at same zeta
        enddo
c       write (*,1244) (zeta(i),i=1,nc)
c1244   format (1x,' STN--zeta(i):  ',5f14.7)
c       write (*,1245) (cx(i),i=1,nc)
c1245   format (1x,' STN--cx(i):    ',5e14.4)
c  find critical parameters at same fugacity fraction
        call CRITF (zeta,xcritf,tcritf,pcritf,Dcritf,ierr,herr)
        if (ierr.gt.0) then
c  error in CRITF--solution not possible
          sigma=xnotc
          RETURN
        end if
      else if (hsten.eq.'STH') then
c  mixture case--apply Holcomb & Higashi modification of Moldover & Rainwater
c  (i.e. apply mixing rules at overall mass composition corresponding to a
c  liquid volume fraction of 0.5)
c  (M.R. Moldover and J.C. Rainwater, J. Chem. Phys., 88:7772-7780, 1988.)
        do i=1,nc
          zmole(i)=(xl(i)*rhol+xv(i)*rhov)/(rhol+rhov)
        enddo
        call XMASS (zmole,zeta,xmw)
        call CRITP (zmole,tcritf,pcritf,Dcritf,ierr,herr)
      else
        ierr=99
        sigma=-9.999d6
        write (herr,1199) hsten,hnull
 1199   format ('[STN error 99] ',
     &          'unknown surface tension model:  (',a3,')',a1)
c       write (*,*) ' STN--output sigma (ierr = 99):  ',sigma
      end if
c
      cmix=0.0d0
      if (hsten.eq.'STM' .or. hsten.eq.'STH') then
c  use Moldover & Rainwater or Holcomb & Higashi mod of M & R method
c  difference is in the zeta defined above
        do i=1,nc
          cmix=cmix+zeta(i)*cx(i)
        enddo
c       write (*,*) ' STN--cmix by M-R:  ',cmix
      else if (hsten.eq.'STX') then
c  use Holcomb's modification of Moldover & Rainwater method
        pcsum=0.0d0
        do i=1,nc
          pcsum=pcsum+zeta(i)**2*Pcrit(i)
        enddo
        do i=1,nc
c  sum i = j terms
          cmix=cmix+zeta(i)**2*cx(i)
          if (i.lt.nc) then
            do j=i+1,nc
c  sum cross terms
c         cij=0.5d0*(pcritf-pcsum)*SQRT(cx(i)*cx(j)/(pcrit(i)*pcrit(j)))
c    &       /(zeta(i)*zeta(j))
c         cmix=cmix+2.0d0*zeta(i)*zeta(j)*cij !factor 2 from ij = ji
c  above lines reduce to
              cmix=cmix+
     &             (pcritf-pcsum)*SQRT(cx(i)*cx(j)/(Pcrit(i)*Pcrit(j)))
            enddo
          end if
        enddo
c       write (*,*) ' STN--cmix by Holcomb''s mod to M-R:  ',cmix
      end if
c
c  recover mixture sigma_0 parameter from cmix
c  this expression is based on critical parameters at same zeta
      sig0=(const*SQRT(tcritf)*pcritf*cmix)**(2.0d0/3.0d0)
      sigma=sig0*tau**1.26d0
c     write (*,*) ' STN--hsten,sigma: ',hsten,' ',sigma
c
      RETURN
      end                                                !subroutine STN
c
c ======================================================================
c
      subroutine STNK (icomp,tau,sigma,ierr,herr)
c
c  compute surface tension with appropriate core model
c
c  inputs:
c    icomp--component i
c      tau--dimensionless temperature (1 - T/Tc)
c   output:
c    sigma--surface tension [N/m]
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Physical & Chemical Properties Division, Boulder, Colorado
c  04-17-97  MM, original version (based on STN)
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
c
      if (hstenk(icomp).eq.'ST1') then
        sigma=0.0d0
        do k=1,ntermu(icomp)
          sigma=sigma+sigmak(icomp,k)*tau**sigexp(icomp,k)
        enddo
      else
        sigma=-999.0d0
        write (herr,1099) hstenk(icomp),hnull
        call ERRMSG (ierr,herr)
 1099   format ('[STN error 99] ',
     &          'unknown surface tension model:  (',a3,')',a1)
      end if
c     write (*,1200) icomp,tau,sigma
c1200 format (' STNK--icomp,tau,sigma: ',i4,2f11.6)
c
      RETURN
      end                                               !subroutine STNK
c
c ======================================================================
c
      subroutine CRITF (zeta,x,tc,pc,Dc,ierr,herr)
c
c  critical parameters as a function of fugacity fraction
c
c  inputs:
c     zeta--fugacity fraction [array of f/f]
c        x--initial guess for composition [array of mol frac]
c  outputs:
c        x--composition [array of mol frac]
c    tc--critical temperature [K]
c    pc--critical pressure [kPa]
c    Dc--critical density [mol/L]
c     ierr--error flag:  0 = successful
c                      160 = did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Physical and Chemical Properties Division, Boulder, Colorado
c  04-21-97  MM, original version
c  12-15-97  MM, change value of ierr for non-convergence
c
      include 'commons.for'
      character*255 herr,herr1
      dimension zeta(ncmax),zetaj(ncmax),x(ncmax),f(ncmax),xnew(ncmax)
c
      parameter (itmax=20)
      tolx=1.0d-5
      ierr=0
      herr=' '
c
c     do i=1,nc
c       x(i)=zeta(i)    !initial guess for composition
c     enddo
      do it=1,itmax
        call CRITP (x,tc,pc,Dc,ierr1,herr1)
        call FGCTY2 (tc,Dc,x,f,ierr,herr)
        fsum=0.0d0
        do i=1,nc
          fsum=fsum+f(i)
        enddo
        do i=1,nc
          zetaj(i)=f(i)/fsum
        enddo
        delx=0.0d0
        xsum=0.0d0
        do i=1,nc
c  simple successive substitution
          xnew(i)=x(i)-(zetaj(i)-zeta(i))
          if (xnew(i).lt.0.0d0) xnew(i)=0.0d0
          xsum=xsum+xnew(i)
          delx=delx+abs(xnew(i)-x(i))
        enddo
c       write (*,1160) it,tc,pc,delx,x(1),zetaj(1),zeta(1)
c1160   format (1x,' CRITF--it,tc,pc,delx,x,zetaj,zeta:  ',i4,6f14.6)
        if (delx.lt.tolx) then
          RETURN    !iteration converged
        end if
        do i=1,nc
          xnew(i)=xnew(i)/xsum
          x(i)=xnew(i)
        enddo
      enddo
      ierr=160
      herr='[SURFT error 160] CRITF (find critical parameters at '//
     &     'a specified fugacity fraction) did not converge in the '//
     &     'surface tension calculation'//hnull
      call ERRMSG (ierr,herr)
c
      RETURN
      end                                              !subroutine CRITF
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file core_STN.f
c ======================================================================
