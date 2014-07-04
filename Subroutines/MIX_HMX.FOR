c  begin file mix_HMX.f
c
c  This file contains the routines implementing the mixture Helmholtz
c  equation of state of Lemmon and Jacobsen and of Kunz and Wagner.
c
c  contained here are:
c     function PHIHMX (itau,idel,tau,del,x)
c     function PHIMIX (i,j,itau,idel,tau,del,x)
c     subroutine CRTHMX (x,tc,pc,Dc,ierr,herr)
c     subroutine REDHMX (x,tred,Dred)
c     subroutine RDXHMX (ix,icmp,icmp2,x,tred,Dred,ierr,herr)
c     subroutine SETHMX (hfmix,ierr,herr)
c     subroutine RDBNC (nread,icomp,jcomp,ibin,ierr,herr)
c     subroutine RDMIX (nread,icomp,jcomp,hmodij,lij,ierr,herr)
c     subroutine ESTBNC (i,j,ibin,ierr,herr)
c     subroutine ESTTC (i,j,ierr,herr)
c     subroutine ESTVC (i,j,ierr,herr)
c     subroutine AMH2OR
c
c ======================================================================
c ======================================================================
c
      function PHIHMX (itau,idel,tau,del,x)
c
c  compute reduced Helmholtz energy or a derivative as functions
c  of dimensionless temperature and density for the mixture Helmholtz
c  equation of state
c
c  based on notes of E.W. Lemmon and R.T Jacobsen, U. Idaho, 1995
c
c  inputs:
c     itau--flag specifying order of temperature derivative to calc
c     idel--flag specifying order of density derivative to calculate
c           when itau = 0 and idel = 0, compute A/RT
c           when itau = 0 and idel = 1, compute 1st density derivative
c           when itau = 1 and idel = 1, compute cross derivative
c           etc.
c      tau--dimensionless temperature (To/T)
c      del--dimensionless density (D/Do)
c        x--composition array (mol frac)
c  output (as function value):
c      phi--residual (real-gas) part of the Helmholtz energy, or one
c           of its derivatives (as specified by itau and idel),
c           in reduced form (A/RT)
c
c  N.B.  The reducing parameters To and Do are often, but not
c        necessarily, equal to the critical temperature and density.
c
c        The Helmholtz energy consists of ideal gas and residual (real-
c        gas) terms.  The residual term consists of ideal-solution and
c        mixing terms.  This routine calculates only the residual term.
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-02-95  MM, original version
c  11-26-95  MM, rearrange argument list (x at end)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  02-29-96  MM, replace calls to PHIBWR, PHIFEQ with general PHIK
c  03-21-96  MM, delete reference to /MODEL/, not used
c  07-01-96  MM, move x(i)*x(j) multiplier here (remove from PHIMIX)
c  06-03-97 EWL, add third derivative of Helmholtz energy with respect to density.
c  01-07-98  MM, move x(i)*x(j) multiplier back to PHIMIX
c  09-00-00 EWL, switch order of execution, check x=0 before call to PHIMIX
c
      include 'commons.for'
      dimension x(ncmax)
c
      phihmx=0.0d0
      if (del.le.1.d-10) RETURN  !trivial solution at zero density
c
      phisum=0.d0
      do i=1,nc
        if (x(i).gt.1.d-12) then
c  calculate ideal solution term
          phiii=PHIK(i,itau,idel,tau,del)
          phisum=phisum+x(i)*phiii
c         write (*,1200) i,i,x(1),x(2),x(3),phiii,phisum
c1200     format (1x,' PHIHMX--i,j,x,phi_ij:  ',2i4,3f8.5,1pe14.6)
        end if
      enddo
c!$omp parallel private(phisum)
      do i=1,nc-1
c  calculate mixing term
        if (x(i).gt.1.d-12) then
          do j=i+1,nc
            if (x(j).gt.1.d-12) then
              phiij=PHIMIX(i,j,itau,idel,tau,del,x)
              phisum=phisum+phiij
            end if
          enddo
        endif
      enddo
c!$omp end parallel
      phihmx=phisum
c
      RETURN
      end                                               !function PHIHMX
c
c ======================================================================
c
      function PHIMIX (icomp,jcomp,itau,idel,tau,del,x)
c
c  compute reduced Helmholtz energy of mixing (or a derivative)
c  for the binary interaction of components i and j as a function of
c  composition and dimensionless temperature and density for the
c  mixture Helmholtz equation of state
c
c  inputs:
c    icomp--component i
c    jcomp--component j
c     itau--flag specifying order of temperature derivative to calc
c     idel--flag specifying order of density derivative to calculate
c           when itau = 0 and idel = 0, compute Amix/RT
c           when itau = 0 and idel = 1, compute 1st density derivative
c           when itau = 1 and idel = 1, compute cross derivative
c           etc.
c      tau--dimensionless temperature (To/T)
c      del--dimensionless density (D/Do)
c        x--composition array (mol frac)
c  output (as function value):
c      phi--mixture interaction (excess) part of the Helmholtz energy,
c           or one of its derivatives (as specified by itau and idel),
c           in reduced form (Amix/RT)
c
c        The Helmholtz energy consists of ideal gas and residual (real-
c        gas) terms.  The residual term consists of ideal-solution and
c        mixing terms.  This routine calculates only the mixing term.
c
c  N.B.  This routine incorporates the mixture-specific multiplier
c        (e.g. the Fpq of the Lemmon & Jacobsen model).
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-02-95  MM, original version
c  11-26-95  MM, rearrange argument list (x at end)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  01-10-96  MM, add commons /CFXHMX/ and /CFIHMX/
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-21-96  MM, delete reference to /MODEL/, not used
c  05-06-96  MM, add /MIXMOD/, cases for different mix models
c  06-12-96  MM, add /HMXSAV/, LJi and LMi models
c  07-01-96  MM, move x(i)*x(j) multiplier to PHIHMX; remove x from inputs
c  11-04-96  MM, replace van der Waals rule with linear (LIN),
c                change nmxpar from 4 to 6
c  11-26-97 APP, add mixture model for Ammonia-Water and common block /AMMWAT/
c  12-01-97  MM, add composition to inputs (used by ammonia-water)
c  01-07-98  MM, move x(i)*x(j) back to here from PHIHMX
c  12-22-98 EWL, reorganize ammonia-water code
c  12-22-98 EWL, fix errors in derivatives of ammonia-water equation
c  12-24-98 EWL, add third derivative of tau to LJ model
c  12-24-98 EWL, change AMMWAT common block to FLAGS2 common block
c  09-00-00 EWL, add logic to return old value if inputs are same
c  09-00-00 EWL, change del^d, etc. to LOG format to increase calculation speed
c  09-00-00 EWL, add exponential pieces to mixture terms
c  02-14-01 EWL, increase dimension on amix to 4
c                change use of iexpmx(i,j,k) to amix(i,j,k,4)
c  07-30-01 EWL, move iamwat=0 to beginning of code
c  08-08-01 EWL, change -delli to delli to mimic core_feq routines
c  08-17-06 EWL, update program for GERG-2004 equation of Kunz and Wagner
c  10-19-06 EWL, change calculation of ncode2 to work with "KW"
c  04-16-10 EWL, increase nmsav from 25 to 55 to allow KWA, KWB, KWC, etc.
c
      include 'commons.for'
      dimension x(ncmax)
c
      phimix=0.0d0
      if (del.le.1.0d-10) then    !trivial solution at zero density
        RETURN                    !for any and all derivatives
      end if
      if (tau.le.0.0d0) RETURN
c
      i=icomp
      j=jcomp
      ncode2=0
      if (hmodmx(i,j)(1:2).eq.'LJ') ncode2=ichar(hmodmx(i,j)(3:3))-47
      if (hmodmx(i,j)(1:2).eq.'KW') ncode2=ichar(hmodmx(i,j)(3:3))-37
      if (hmodmx(i,j)(1:2).eq.'LM') ncode2=ichar(hmodmx(i,j)(3:3))-27
      if (ncode2.lt.0 .or. ncode2.gt.nmsav) then
        phimix=1.d20
        RETURN
      endif
      if (hmodmx(i,j).eq.'LIN' .or. i.eq.j) then
c
c  linear mixing rules:  all departures from ideal solution are
c  contained in the reducing parameters (i.e. Kt and/or Kv)
c
        phimix=0.0d0
c
      elseif (ncode2.ne.0) then
        ncode=idel*4+itau+1
c
c  Lemmon-Jacobsen or modified Lemmon-Jacobsen mixing function
c
        phisum=0.0d0
        ifij=3
        if (hmodmx(i,j)(1:2).eq.'KW') ifij=5
        if (abs(tau-tausvh(ncode2)).lt.1.0d-12 .and.
     &      abs(del-delsvh(ncode2)).lt.1.0d-12) then
          if (drvflh(ncode2)(ncode:ncode).eq.'1') then
            PHIMIX=x(i)*x(j)*fmix(i,j,ifij)*drvsvh(ncode2,ncode)
            RETURN
          endif
c  retrieve value from previous call
        else
c  otherwise, compute new values and save for possible future use
          drvflh(ncode2)='0000000000000000'
          do k=1,namix(i,j)
            dellih(ncode2,k)=0
            if (abs(amix(i,j,k,4)).gt.1.d-20)
     &          dellih(ncode2,k)=del**amix(i,j,k,4)
            savijk=amix(i,j,k,1)*EXP(amix(i,j,k,2)*log(tau)
     &            +amix(i,j,k,3)*log(del)-dellih(ncode2,k))
            phisvh(ncode2,k)=savijk
            phisum=phisum+savijk
c           write (*,1010) k,phisvh(ncode2,k),phisum
c1010       format (1x,i3,2d30.20)   !write out each term for debugging
          enddo
          if (namix2(i,j).ne.0) then
            do k=namix(i,j)+1,namix(i,j)+namix2(i,j)
              dellih(ncode2,k)=amix(i,j,k,4)*(del-amix(i,j,k,5))**2
              delli2         =amix(i,j,k,6)*(del-amix(i,j,k,7))
              savijk=amix(i,j,k,1)*EXP(amix(i,j,k,2)*log(tau)
     &              +amix(i,j,k,3)*log(del)-dellih(ncode2,k)
     &                                     -delli2)
              phisvh(ncode2,k)=savijk
              phisum=phisum+savijk
            enddo
          endif
          tausvh(ncode2)=tau
          delsvh(ncode2)=del
          drvsvh(ncode2,1)=phisum
          drvflh(ncode2)(1:1)='1'
        end if
c
c  check if derivatives are requested, calculations make use of fact
c  that terms in derivative summations are very similar to A/RT terms
c
        if (idel.eq.1) then
c  compute derivative w.r.t. del (dimensionless density)
c  save individual terms for possible use in cross derivative
          phisum=0.0d0
          do k=1,namix(i,j)
            phix01(k)=phisvh(ncode2,k)
     &              *(amix(i,j,k,3)-amix(i,j,k,4)*dellih(ncode2,k))
            phisum=phisum+phix01(k)
          enddo
          if (namix2(i,j).ne.0) then
            do k=namix(i,j)+1,namix(i,j)+namix2(i,j)
              phix01(k)=phisvh(ncode2,k)
     &                *(amix(i,j,k,3)
     &   -2.d0*del*amix(i,j,k,4)*(del-amix(i,j,k,5))-del*amix(i,j,k,6))
              phisum=phisum+phix01(k)
            enddo
          endif
        elseif (idel.eq.2) then
c  compute 2nd derivative w.r.t. del (dimensionless density)
          phisum=0.0d0
          do k=1,namix(i,j)
            ax3=amix(i,j,k,3)
            dell=amix(i,j,k,4)*dellih(ncode2,k)
            phix02(k)=phisvh(ncode2,k)
     &           *(ax3**2-ax3-dell*(2.0d0*ax3+amix(i,j,k,4)-1.0d0-dell))
            phisum=phisum+phix02(k)
c         end if
          enddo
          if (namix2(i,j).ne.0) then
            do k=namix(i,j)+1,namix(i,j)+namix2(i,j)
              aa3=amix(i,j,k,3)
              aa4=amix(i,j,k,4)
              phix02(k)=phisvh(ncode2,k)
     &  *(del**2*(aa3/del-2.d0*aa4*(del-amix(i,j,k,5))-amix(i,j,k,6))**2
     &  -aa3-2.d0*aa4*del**2)
              phisum=phisum+phix02(k)
            enddo
          endif
        elseif (idel.eq.3) then
c  compute 3nd derivative w.r.t. del (dimensionless density)
          phisum=0.0d0
          do k=1,namix(i,j)
            ax3=amix(i,j,k,3)
            dell=amix(i,j,k,4)*dellih(ncode2,k)
            dl=amix(i,j,k,4)
            phix03(k)=phisvh(ncode2,k)
     &      *(2.0d0*ax3-3.0d0*ax3*ax3+ax3**3+dell*(-2.0d0+3.0d0*dl-dl**2
     &       +6.0d0*ax3-3.0d0*ax3*dl-3.0d0*ax3**2
     &       +dell*(-3.0d0+3.0d0*dl+3.0d0*ax3-dell)))
            phisum=phisum+phix03(k)
          enddo
          if (namix2(i,j).ne.0) then
            do k=namix(i,j)+1,namix(i,j)+namix2(i,j)
c  3rd derivative has not been implemented yet
c             phix03(k)=...
c             phisum=phisum+phix03(k)
            enddo
          endif
        end if
c
        if (itau.eq.1) then
c  compute derivative w.r.t. tau (dimensionless temperature)
c  save individual terms for possible use in cross derivative
          phisum=0.0d0
          do k=1,namix(i,j)+namix2(i,j)
            phix10(k)=phisvh(ncode2,k)*amix(i,j,k,2)
            phisum=phisum+phix10(k)
          enddo
        elseif (itau.eq.2) then
c  compute 2nd derivative w.r.t. tau (dimensionless temperature)
          phisum=0.0d0
          do k=1,namix(i,j)+namix2(i,j)
            ax2=amix(i,j,k,2)
            phix20(k)=phisvh(ncode2,k)*ax2*(ax2-1.0d0)
            phisum=phisum+phix20(k)
          enddo
        elseif (itau.eq.3) then
c  compute 3rd derivative w.r.t. tau (dimensionless temperature)
          phisum=0.0d0
          do k=1,namix(i,j)+namix2(i,j)
            ax2=amix(i,j,k,2)
            phisum=phisum+phisvh(ncode2,k)*ax2*(ax2-1.0d0)*(ax2-2.0d0)
          enddo
        end if
c
        if (itau.eq.1 .and. idel.eq.1) then
c  compute cross derivative using terms from 1st derivatives
          phisum=0.0d0
          do k=1,namix(i,j)+namix2(i,j)
            if (ABS(phisvh(ncode2,k)).gt.1.d-20)
     &          phisum=phisum+phix10(k)*phix01(k)/phisvh(ncode2,k)
          enddo
        end if
        drvsvh(ncode2,ncode)=phisum
        drvflh(ncode2)(ncode:ncode)='1'
        PHIMIX=x(i)*x(j)*fmix(i,j,ifij)*phisum    !fmix(i,j,3) is Fpq
c
      elseif (hmodmx(i,j).eq.'TR1') then
c
c  ammonia-water model of Tillner-Roth
c
c  flip compositions; fmix(i,j,5) is gamma
         ij=iamwat
         phisum=0.0d0
c
         do k=1,14
           ak=amix(i,j,k,1)
           tk=amix(i,j,k,2)
           dk=amix(i,j,k,3)
           ik=amix(i,j,k,4)
           xk=1.0d0
           if (k.ge.7) xk=x(ij)
           if (k.eq.14) xk=xk**2
           delik=-del**ik
           bk=1.0d0
           if (ik.gt.0) bk=exp(delik)
           aka=xk*ak*tau**tk*del**dk*bk
c
           if (itau.eq.0 .and. idel.eq.0) then           !no deriv
             phisum=phisum+aka
           elseif (itau.eq.0 .and. idel.eq.1) then    !1st deriv of del
             phisum=phisum+aka*(dk+ik*delik)
           elseif (itau.eq.0 .and. idel.eq.2) then    !2nd deriv of del
             phisum=phisum+aka*(dk*(dk-1.0d0)
     &             +ik*delik*(ik-1.0d0+2.0d0*dk+ik*delik))
           elseif (itau.eq.0 .and. idel.eq.3) then    !3rd deriv of del
             dki = ik*delik
             phisum=phisum+(dk*(dk-1.0d0)*(dk-2.0d0)
     &         +dki*(3.0d0*dk*((dk-2.0d0)+ik)+(ik-1.0d0)*(ik-2.0d0)
     &         +3.0d0*dki*(dk+(ik-1.0d0))+dki*dki))*aka
           elseif (itau.eq.1 .and. idel.eq.0) then    !1st deriv of tau
             phisum=phisum+tk*aka
           elseif (itau.eq.2 .and. idel.eq.0) then    !2nd deriv of tau
             phisum=phisum+tk*(tk-1.0d0)*aka
           elseif (itau.eq.3 .and. idel.eq.0) then    !3rd deriv of tau
             phisum=phisum+tk*(tk-1.0d0)*(tk-2.0d0)*aka
           elseif (itau.eq.1 .and. idel.eq.1) then!deriv of tau and del
             phisum=phisum+aka*tk*(dk+ik*delik)
           end if
         enddo
         PHIMIX=phisum*x(ij)*(1.0d0-x(ij)**fmix(i,j,5))
      else
c
c  space for additional mixing rule
c
        PHIMIX=0.0d0                !not currently implemented
c       write (*,*) ' PHIMIX ERROR--unknown mixing rule'
      end if
c
      RETURN
      end                                               !function PHIMIX
c
c ======================================================================
c
      subroutine CRTHMX (x,tc,pc,Dc,ierr,herr)
c
c  returns critical parameters associated with mixture Helmholtz EOS
c
c  N.B.  these critical parameters are estimates based on polynomial
c        fits to the binary critical lines and (for 3 or more components)
c        combining rules applied to the constituent binaries
c
c  input:
c        x--composition array (mol frac)
c  outputs:
c       tc--critical temperature (K)
c       pc--critical pressure (kPa)
c       Dc--molar density (mol/L) at critical point
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                     -131 = coefficients not available for calculation,
c                            this occurs because either the binary has
c                            not been fit (i.e., beta versions of the program)
c                            or the mixture is not type 1
c                     -132 = spline curves are available, but no solution available
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-02-95  MM, original version
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  01-29-96  MM, critical parameters based on average of pure fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-19-19  MM, add dipole moment to /CCON/
c  05-06-96  MM, replace temporary func for crit pars with reducing pars
c  11-19-96  MM, add empirical function for critical line(s)
c  11-14-97 EWL, fix bug in critical line correlation
c  11-24-97  MM, Tsum = x(i)*x(j)*Tij, not xij*tcij, ditto for Vsum
c  01-21-98 APP, add Rainwater's ammonia-water critical line equations
c                for hmodmx(i,j) = 'TR1'
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-22-98 EWL, if x(i)=1 for any component, return with pure fluid parameters
c  12-23-98 EWL, rearrange code to eliminate extra calculations with amm-water
c  03-04-02 EWL, check sum(x)=0
c  08-17-06 EWL, update program for GERG-2004 equation of Kunz and Wagner
c  08-21-06 EWL, if TC1 or VC1 are not available, return reducing parameters instead of an error message
c  02-13-13 EWL, return values from spline curves if available
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        tc=Tcrit(icomp)
        Dc=Dcrit(icomp)
        pc=Pcrit(icomp)
        RETURN
      end if
      if (iSpline.ne.0) then
        if (ierrcrt.eq.0) then
          ix=1
          do i=1,nc
            if (abs(x(i)-xspln(i)).gt.1.d-6) ix=0
          enddo
          if (ix.eq.1) then
            tc=crtt
            pc=crtp
            Dc=crtd
            RETURN
          endif
        endif
      end if

c
      tsum=0.0d0
      vsum=0.0d0
      xsum=0.0d0
c     write (*,1010) (x(k),k=1,nc),(tcrit(k),k=1,nc)
c1010 format (' CRTHMX input x,Tc:  ',10f10.4)
      do i=1,nc-1
        if (x(i).gt.0.d0) then
        do j=i+1,nc
          if (x(j).gt.0.d0) then
            xsum=xsum+x(i)*x(j)
c  the tcij and Vcij are the binary critical line correlations, and
c  these terms (ONLY) are evaluated at the pseudo-binary compositions
c  defined by xi and xj:
            xi=0.5d0*(x(i)+(1.0d0-x(j)))
            xj=0.5d0*(x(j)+(1.0d0-x(i)))
            xij=xi*xj
            if (hmodtc(i,j).eq.'TC1') then
              if (abs(ftcij(i,j,1)).lt.1.d-12) then
                ierr=-131
                herr='[CRTHMX error -131] coefficients not available '//
     &               'for critical line calculation, mixture may not '//
     &               'be type 1'//hnull
              endif
              tcij=xi*Tcrit(i)+xj*Tcrit(j)+xij*ftcij(i,j,1)
c  terms assuming mixture specified in same order as in hmx.bnc file
     &            +xj*xi*(ftcij(i,j,2)+(ftcij(i,j,3)+(ftcij(i,j,4)
     &            +(ftcij(i,j,5)+ftcij(i,j,6)*xi)*xi)*xi)*xi)*xi
c  extra terms to allow specifying mix in opposite order (j,i)
c  note:  either terms 2-6 or 7-11 should be set to zero in RDBNC
     &            +xi*xj*((ftcij(i,j,7)+(ftcij(i,j,8)+(ftcij(i,j,9)
     &            +(ftcij(i,j,10)+ftcij(i,j,11)*xj)*xj)*xj)*xj)*xj)
c           write (*,*) 'Tcij using mixture model TC1:  ',i,j,tcij
            else
c             ierr=1
c             herr='[CRTHMX error] Tcrit model not found'//hnull
c             call ERRMSG (ierr,herr)
c             tcij=xi*tcrit(i)+xj*tcrit(j)
              call REDHMX (x,tc,Dc)
              goto 100
            end if
            tsum=tsum+x(i)*x(j)*tcij
            if (hmodvc(i,j).eq.'VC1') then
              Vcij=xi/Dcrit(i)+xj/Dcrit(j)+xij*fvcij(i,j,1)
c  terms assuming mixture specified in same order as in hmx.bnc file
     &            +xj*xi*((fvcij(i,j,2)+(fvcij(i,j,3)+(fvcij(i,j,4)
     &            +(fvcij(i,j,5)+fvcij(i,j,6)*xi)*xi)*xi)*xi)*xi)
c  extra terms to allow specifying mix in opposite order (j,i)
c  note:  either terms 2-6 or 7-11 should be set to zero in RDBNC
     &            +xi*xj*((fvcij(i,j,7)+(fvcij(i,j,8)+(fvcij(i,j,9)
     &            +(fvcij(i,j,10)+fvcij(i,j,11)*xj)*xj)*xj)*xj)*xj)
            else
c             ierr=1
c             herr='[CRTHMX error] Vcrit model not found'//hnull
c             call ERRMSG (ierr,herr)
c             Vcij=xi/Dcrit(i)+xj/Dcrit(j)
              call REDHMX (x,tc,Dc)
              goto 100
            end if
            vsum=vsum+x(i)*x(j)*Vcij
          endif
c         write (*,1016) i,j,tcij,Vcij,xsum
c1016     format (' CRTHMX:  i,j,Tc_ij,Vc_ij,xsum:  ',2i3,3f12.6)
c         write (*,1017) i,j,(ftcij(i,j,k),k=1,nmxtc)
c1017     format (' CRTHMX:  i,j,ftcij:  ',2i3,8f10.6)
        enddo
        endif
      enddo
      if (xsum.gt.1.0d-12) then
        tc=tsum/xsum
        Dc=xsum/vsum
      else
        tc=300
        Dc=10
        pc=10
        ierr=1
        herr='[CRTHMX error] sum(x)=0'//hnull
        call ERRMSG (ierr,herr)
      end if
c
c  base Pc on Tc, Dc
 100  continue
      call PRESS (tc,Dc,x,pc)
      if (pc.lt.0) pc=Pcrit(1)
      if (ierrcrt.gt.0 .and. iSpline.ne.0 .and. ierr.eq.0) then
        ierr=-132
        herr='[CRTHMX error -132] Failed to find critical point from'//
     &       ' splines, values given here used TC1 and VC1'//
     &       ' coefficients'//hnull
      endif
c     write (*,1024) tc,1.0/Dc,pc
c1024 format (' CRTHMX:  Tc,Vc,Pc:  ',15x,3f12.6)
c
      RETURN
      end                                             !subroutine CRTHMX
c
c ======================================================================
c
      subroutine REDHMX (x,tred,Dred)
c
c  01-26-10 EWL, rename original routine with new inputs, keep this routine
c                for old applications.
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call RDXHMX (0,0,0,x,tred,Dred,ierr,herr)
      end                                             !subroutine REDHMX
c
c ======================================================================
c
      subroutine RDXHMX (ix,icmp,icmp2,x,tred,Dred,ierr,herr)
c
c  returns reducing parameters and their derivatives associated with
c  the mixture Helmholtz EOS; these are used to calculate the 'tau' and 'del'
c  that are the independent variables in the EOS.
c
c  input:
c        x--composition array [mol frac]
c     icmp--component number for which derivative will be calculated
c    icmp2--second component number for which derivative will be calculated
c       ix--flag specifying order of composition derivative to calculate
c           when ix = 0, compute T(red) and rho(red)
c         for icmp2=0:
c           when ix = 1, compute 1st derivative with respect to x(icmp)
c           when ix = 2, compute 2nd derivative with respect to x(icmp)
c         for icmp<>0 and icmp2<>0:
c           when ix = 11, compute cross derivative with respect
c                         to x(icmp) and x(icmp2)
c  outputs:
c     tred--reducing temperature [K] or derivative
c     Dred--reducing molar density [mol/L] or derivative of reducing volume
c           (ix=0: rhoc; ix=1: dvc/dxi; ix=2: d^2vc/dxi^2; ix=11: d^2vc/dxidxj)
c     ierr--error flag:  0 = successful
c                      190 = Mixing rule not found for i,j
c                      191 = Derivative not available
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-02-95  MM, original version
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-19-19  MM, add dipole moment to /CCON/
c  03-21-96  MM, replace call to REDFEQ with general REDX, delete /MODEL/
c  05-06-96  MM, add /MIXMOD/,/CFXHMX/,/CFIHMX/; add Kt, Kv dependence;
c                branch for arithmetic or geometric mean of temperature
c  05-14-96  MM, modify volume mixing rule for VDW model; use arithmetic
c                average of critical volumes, rather than Reiner's rule
c  06-10-96  MM, modify volume mixing rule for VDW model (again); use
c                average of cube root of critical volumes
c                add Lemmon-Jacobsen mixing rules
c  06-12-96  MM, add LMx mixing rules (Lemmon-Jacobsen modified to be of
c                form compatible with van der Waal's (VDW) model
c  07-01-96  MM, fix bug--missing **3 in VDW volume term
c  11-04-96  MM, replace van der Waals rule with linear (LIN),
c                change nmxpar from 4 to 6
c  11-08-96  MM, fix wrong array reference for Kv
c  11-14-96  MM, fix wrong array reference for zeta, xi (LJi model)
c  05-22-97  MM, bug fix:  beta, gamma were applied to V, should not be
c  11-26-97 APP, add Ammonia-Water model and common block /AMMWAT/
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-24-98 EWL, change AMMWAT common block to FLAGS2 common block
c  12-24-98 EWL, fix ammonia-water code to fit within do-loop
c  12-24-98 EWL, avoid reducing values of 0 when a fluid has not been loaded
c  02-14-01 EWL, increase dimension on amix to 4
c                change use of iexpmx(i,j,k) to amix(i,j,k,4)
c  08-08-01 EWL, remove the 0.5* in tc12= and vc12= used in the linear and LM models
c  08-17-06 EWL, update program for GERG-2004 equation of Kunz and Wagner
c  08-21-06 EWL, add Kunz and Wagner mixing rules
c  03-05-07 EWL, add fmix2 array in case fluids are entered in opposite order as required by Kunz and Wagner
c  01-26-10 EWL, rename subroutine
c  01-26-10 EWL, add derivatives with respect to composition
c  02-13-13 EWL, change error numbers
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      tsum=0.0d0
      vsum=0.0d0
      onethrd=-0.33333333333333333333d0
c
      if (ix.le.0) then
        do i=1,nc
          if (x(i).gt.0.d0) then
          do j=i,nc
            if (x(j).gt.0.d0) then



            if (ix.eq.-1) then
c-----------------------------------------------------------------------
c  These are the full Kunz-Wagner relations, but should only be used
c  for numerical testing of analytical derivatives.  If used in combination
c  with other mixing rules (e.g., LJ6 or TR1), the computations will not
c  be correct.  These equations work when sum(x(k))<>0, as is the case
c  when taking a numerical derivative of a property with respect to x(i)
c  holding all other compositions constant.
              if (hmodmx(ione1,itwo2)(1:2).eq.'KW') then
                tc12=SQRT(tz(i)*tz(j))
                vc12=(rhoz(i)**onethrd+rhoz(j)**onethrd)**3/8.d0
                xij=x(i)*x(j)*(x(i)+x(j))
                if (i.eq.j) then
                  ftij=xij/(x(i)+x(j))
                  fvij=xij/(x(i)+x(j))
                  ctij=tc12
                  cvij=vc12
                else
                  ftij=xij/(fmix(i,j,1)**2*x(i)+fmix2(i,j,1)**2*x(j))
                  fvij=xij/(fmix(i,j,3)**2*x(i)+fmix2(i,j,3)**2*x(j))
                  ctij=2.d0*fmix(i,j,1)*fmix2(i,j,1)*fmix(i,j,2)*tc12
                  cvij=2.d0*fmix(i,j,3)*fmix2(i,j,3)*fmix(i,j,4)*vc12
                endif
                tsum=tsum+ctij*ftij
                vsum=vsum+cvij*fvij
c  original Lemmon-Jacobsen rule using zeta, xi
              elseif (hmodmx(ione1,itwo2)(1:2).eq.'LJ') then
                if (i.eq.j) then
                  tsum=tsum+x(i)*tz(i)
                  vsum=vsum+x(i)/rhoz(i)
                else
                  xij=x(i)*x(j)
                  xijt=x(i)**fmix(i,j,4)*x(j)**fmix(i,j,5)
                  tsum=tsum+xijt*fmix(i,j,1)
                  vsum=vsum+xij*fmix(i,j,2)
                endif
              endif
c-----------------------------------------------------------------------



c  special case when i = j
            elseif (i.eq.j) then
              tsum=tsum+x(i)*tz(i)
              vsum=vsum+x(i)/rhoz(i)

c  Kunz-Wagner rule
            elseif (hmodmx(i,j)(1:2).eq.'KW' .and. heos.ne.'PR') then
              tc12=SQRT(tz(i)*tz(j))
              vc12=(rhoz(i)**onethrd+rhoz(j)**onethrd)**3/8.d0
              xij=x(i)*x(j)*(x(i)+x(j))
              ftij=xij/(fmix(i,j,1)**2*x(i)+fmix2(i,j,1)**2*x(j))
              fvij=xij/(fmix(i,j,3)**2*x(i)+fmix2(i,j,3)**2*x(j))
              ctij=2.d0*fmix(i,j,1)*fmix2(i,j,1)*fmix(i,j,2)*tc12
              cvij=2.d0*fmix(i,j,3)*fmix2(i,j,3)*fmix(i,j,4)*vc12
              tsum=tsum+ctij*ftij
              vsum=vsum+cvij*fvij
c  the following addition allows the use of linear mixture rules x(i)*Tc(i)
c  when the specified rule (as in this case for the GERG-2004 equation) is
c  not linear:  x(i)^2*Tc(i)
              tsum=tsum-x(i)*x(j)*(tz(i)+tz(j))
              vsum=vsum-x(i)*x(j)*(1.d0/rhoz(i)+1.d0/rhoz(j))

c  original Lemmon-Jacobsen rule using zeta, xi
            elseif (hmodmx(i,j)(1:2).eq.'LJ' .and. heos.ne.'PR') then
              xij=x(i)*x(j)
              xijt=x(i)**fmix(i,j,4)*x(j)**fmix(i,j,5) !fmix(i,j,5) = gamma
              tsum=tsum+xijt*fmix(i,j,1)               !fmix(i,j,1) = zeta(T)
              vsum=vsum+xij*fmix(i,j,2)                !fmix(i,j,2) = xi  (x)

c  mixing parameters for the Ammonia-Water model
c  fmix(i,j,1)=Kt, fmix(i,j,2)=Kv, fmix(i,j,3)=alpha, fmix(i,j,4)=beta
            elseif (hmodmx(i,j).eq.'TR1' .and. heos.ne.'PR') then
              ij=i
              if (iamwat.eq.2) ij=j
              tc12=tz(i)+tz(j)
              vc12=1.0d0/rhoz(i)+1.0d0/rhoz(j)
             tsum=tsum+x(ij)*(1.0d0-x(ij)**fmix(i,j,3))*fmix(i,j,1)*tc12
     &                -x(i)*x(j)*tc12
             vsum=vsum+x(ij)*(1.0d0-x(ij)**fmix(i,j,4))*fmix(i,j,2)*vc12
     &                -x(i)*x(j)*vc12

c  simple linear mixing rules
            elseif (hmodmx(i,j).eq.'LIN') then        !Not used in hmx.bnc
              xij=x(i)*x(j)
              tc12=(tz(i)+tz(j))
              vc12=(1.0d0/rhoz(i)+1.0d0/rhoz(j))
              tsum=tsum+xij*(fmix(i,j,1)-1.0d0)*tc12   !fmix(i,j,1) = Kt(i,j)
              vsum=vsum+xij*(fmix(i,j,2)-1.0d0)*vc12   !fmix(i,j,2) = Kv(i,j)

c  Lemmon-Jacobsen rule modified to use Kt, Kv, rather than zeta, xi
            elseif (hmodmx(i,j)(1:2).eq.'LM') then    !obsolete (from version 6)
              xij=x(i)*x(j)
              xijt=x(i)**fmix(i,j,4)*x(j)**fmix(i,j,5) !fmix(i,j,4) = beta
              tc12=(tz(i)+tz(j))                       !fmix(i,j,5) = gamma
              vc12=(1.0d0/rhoz(i)+1.0d0/rhoz(j))
              tsum=tsum+xijt*(fmix(i,j,1)-1.0d0)*tc12  !fmix(i,j,1) = Kt(i,j)
              vsum=vsum+xij*(fmix(i,j,2)-1.0d0)*vc12   !fmix(i,j,2) = Kv(i,j)

c  mixing rule not specified or not found--should not get here
c  call ERRMSG anyways in case error messages are being written to the screen
            else
              ierr=190
              herr='[RDXHMX error 190] Mixing rule not found for i,j'
              call ERRMSG (ierr,herr)
            end if
            end if
          enddo
          end if
        enddo
        if (ABS(tsum).gt.0.5d-12 .and. ABS(vsum).gt.0.5d-12) then
          tred=tsum
          Dred=1.0d0/vsum
        else
          tred=100
          Dred=10
        endif

c  calculate derivatives of Tc and vc
      else
        if (heos.eq.'PR') goto 100
        do k=1,nc
         if (x(k).gt.0.d0 .and. x(icmp).gt.0.d0) then
c         if (ix.eq.1 .and. k.eq.icmp) then                 !LJ linear mixing rules
c           tsum=tsum+1.d0*tz(icmp)                !Needs to be 2.d0* to work with KW, but 1.0 for LJ
c           vsum=vsum+1.d0/rhoz(icmp)

          if (k.eq.icmp .and. ix.eq.1) then
            tsum=tsum+2.d0*x(icmp)*tz(icmp)
            vsum=vsum+2.d0*x(icmp)/rhoz(icmp)
          elseif (k.eq.icmp .and. ix.eq.2) then
            tsum=tsum+2.d0*tz(icmp)
            vsum=vsum+2.d0/rhoz(icmp)
          elseif (ix.eq.11 .and. k.eq.icmp) then
          elseif (hmodmx(k,icmp)(1:2).eq.'KW') then
c  Kunz-Wagner mixing rules
            tc12=SQRT(tz(k)*tz(icmp))
            vc12=(rhoz(k)**onethrd+rhoz(icmp)**onethrd)**3/8.d0
            ctij=2.d0*fmix(k,icmp,1)*fmix2(k,icmp,1)*fmix(k,icmp,2)*tc12
            cvij=2.d0*fmix(k,icmp,3)*fmix2(k,icmp,3)*fmix(k,icmp,4)*vc12
            xij=x(k)+x(icmp)
            f11=fmix (k,icmp,1)**2
            f13=fmix (k,icmp,3)**2
            f21=fmix2(k,icmp,1)**2
            f23=fmix2(k,icmp,3)**2
            if (k.lt.icmp) then
              ftij=f11*x(k)+f21*x(icmp)
              fvij=f13*x(k)+f23*x(icmp)
              st=f21/ftij
              sv=f23/fvij
              ut=f11/ftij
              uv=f13/fvij
            else
              ftij=f21*x(k)+f11*x(icmp)
              fvij=f23*x(k)+f13*x(icmp)
              st=f11/ftij
              sv=f13/fvij
              ut=f21/ftij
              uv=f23/fvij
            endif
            if (ix.eq.1) then
              ftij=x(k)/ftij*(xij+x(icmp)*(1.d0-st*xij))
              fvij=x(k)/fvij*(xij+x(icmp)*(1.d0-sv*xij))
            elseif (ix.eq.2) then
              ftij=x(k)/ftij*(1.d0-st*xij)*(2.d0-x(icmp)*2.d0*st)
              fvij=x(k)/fvij*(1.d0-sv*xij)*(2.d0-x(icmp)*2.d0*sv)
            elseif (ix.eq.11) then
              if (k.eq.icmp2) then
                ftij=(xij+x(k)*(1.d0-ut*xij)+x(icmp)*(1.d0-st*xij)
     &              -x(icmp)*x(k)*(st+ut-2.d0*st*ut*xij))/ftij
                fvij=(xij+x(k)*(1.d0-uv*xij)+x(icmp)*(1.d0-sv*xij)
     &              -x(icmp)*x(k)*(sv+uv-2.d0*sv*uv*xij))/fvij
              endif
            endif
            if (ix.ne.11 .or. (ix.eq.11 .and. k.eq.icmp2)) then
              tsum=tsum+ctij*ftij
              vsum=vsum+cvij*fvij
            endif

c  Lemmon-Jacobsen mixing rules (not working due to linear vs. quadratic terms for i=j)
c         elseif (hmodmx(k,icmp)(1:2).eq.'LJ') then
c           goto 100
c           if (ix.eq.1) then
c             if (fmix(k,icmp,4).ne.1.d0.or.fmix(k,icmp,5).ne.1.d0) then
c               goto 100
c             else
c               tsum=tsum+fmix(k,icmp,1)*x(k)
c               vsum=vsum+fmix(k,icmp,2)*x(k)
c             endif
c           elseif (ix.eq.11 .and. k.eq.icmp2) then
c             tsum=tsum+fmix(icmp,icmp2,1)
c             vsum=vsum+fmix(icmp,icmp2,2)
c           endif

c  All others
          elseif (hmodmx(k,icmp).ne.'i=j') then
            goto 100
          endif
          endif
        enddo
        tred=tsum
        Dred=vsum
      endif
c
c     write (*,*) "tred and dred for X:",x(1),x(2),tred,Dred
c1120 format ('  RDXHMX--x,t0,D0: ',f10.7,f10.3,f10.5)
c
      RETURN
c
 100  continue
      ierr=191
      herr='[RDXHMX error 191] Derivative not available'
      call ERRMSG (ierr,herr)
      RETURN
      end                                             !subroutine RDXHMX
c
c ======================================================================
c
      subroutine SETHMX (hfmix,ierr,herr)
c
c  read parameters for the HMX mixture model from a file
c
c  input:
c    hfmix--file name containing mixture parameters [character*255]
c  outputs:
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     [fluid parameters, etc. returned via various common blocks]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  01-09-96  MM, original version
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-19-96  MM, add dipole moment to /CCON/
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  05-06-96  MM, add /MIXMOD/ to specify model for binary pairs;
c                initialize j,i same as i,j; initialize i = j to 1.0
c  05-07-96  MM, move read of binary model(s) to RDMIX
c  05-08-96  MM, add /MXINFO/ and read corresponding info from file
c  11-19-96  MM, move initialization of common blocks to RDBNC, ESTBNC
c  03-27-97  MM, if file not found, search in likely directories
c  07-03-97  MM, pass any error from ESTBNC up the chain
c  08-22-97  MM, use double backslash on file open to avoid problem with Unix machines
c  11-26-97 APP, additions for Ammonia-Water calculations, new common block /AMMWAT/
c  04-15-98 EWL, add check for nist14 directory
c  07-08-98 EWL, change character strings from *80 to *255
c  12-24-98 EWL, change AMMWAT common block to FLAGS2 common block
c  12-24-98 EWL, restructure check for ammonia-water mixtures
c  01-10-01 EWL, only call checks for estimation scheme if ierr1<>0, not ierr
c  02-02-01 EWL, remove double backslash, which caused problems in Windows
c  02-14-01 EWL, increase dimension on amix to 4
c                change use of iexpmx(i,j,k) to amix(i,j,k,4)
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c  07-13-04 EWL, read in a version number on the second line of hmx.bnc
c  09-28-05  DT, change occurrences of a backslash to 'char(92)' for compatibility
c                with some compilers using F77
c  08-17-06 EWL, update program for GERG-2004 equation of Kunz and Wagner
c  09-06-06 EWL, change nbrule to 20
c  01-24-13 MLH, check for alchohols in mixtures
c
      include 'commons.for'
      character*3 htype
      character*255 hfmix
      character*255 herr,herr1
      logical lmix,lij(nx,nx),lfbnc,lftc,lfvc
c
c  commons associated with the binary mixing rule(s)
c  the amix(i,j,k,1..3) are the coefs for the (general) mixing term
c  the fmix(i,j,1..nmxpar) are the parameters for the i-j binary
c  the namix(i,j) are the number of afeq(i,j,k,1..3) terms
c  /MXINFO/ contains information on the mixing rules and parameters
c  hmfile specifies the files from which mixing rules originate
c  (stored in order 1,2  1,3  2,3; element zero is file called in SETUP)
c  hbin provides documentation for the current binary parameters
c  hrule contains descriptions of the currently available mixing rules
c  hbpar contains descriptions of the binary parameters (e.g. Kt, Kv)
c  associated with the currently available mixing rules
c
      hmfile(0)=hfmix
      ierr=0
      ierr1=0
      herr=' '
      do i=1,nx
        do j=1,nx
          do k=1,nmxprb
            fprkij(i,j,k)=0.d0
          enddo
        enddo
      enddo
c  (re)initialize contents of /HMXSAV/ when a new mixture is read in
      do i=1,nmsav
        delsvh(i)=0.0d0
        tausvh(i)=0.0d0
        do k=0,nmxtrm
          phisvh(i,k)=0.0d0
        enddo
      enddo
      iamwat=0
c
c  initialize the diagonal terms (i = j)
c
      do i=1,nc
        hmodmx(i,i)='i=j'
        hmodtc(i,i)='i=j'
        hmodvc(i,i)='i=j'
        hprkij(i,i)='   '
c  the i = j terms in the fmix summation should not be called (neither
c  the i > j), but set the number of terms to zero as precaution
        namix(i,i)=0
        namix2(i,i)=0
        do k=1,nmxpar
          fmix(i,i,k)=0.0d0
          fmix2(i,i,k)=0.0d0
        enddo
        do k=1,nmxtc
          ftcij(i,i,k)=0.0d0
        enddo
        do k=1,nmxvc
          fvcij(i,i,k)=0.0d0
        enddo
      enddo
c
      if (hfmix(1:5).eq.'BDATA' .or. hfmix(1:5).eq.'bdata') then
c  get coefficients from common blocks--not currently implemented
        ierr=1
        herr='[SETHMX error] Block data option not available'//hnull
        call ERRMSG (ierr,herr)
        RETURN
      else
c
c  read parameters for the binary pairs from file
c
c        write (*,1105) hfmix
c1105   format (/1x,'reading mixture coefficients from file: (',a40,')')
        nread=12             !logical unit for file reads
        call OPENFL (nread,hfmix,1,ierr,herr)
        if (ierr.ne.0) goto 999
        rewind (nread)
        read (nread,2003) htype
        if (htype.ne.heos) then
          ierr=1
          herr='[SETHMX error] Mixture file does not match model'//hnull
          call ERRMSG (ierr,herr)
          RETURN
        end if
        read (nread,'(i2)') ivrsnm
        ibin=0     !initialize counter for binary pair number
        do i=1,nc-1
          do j=i+1,nc
            ibin=ibin+1
            hmfile(ibin)=hfmix
            call RDBNC (nread,i,j,ibin,ierr1,herr1)
            if (ierr1.ne.0 .and. ierr.le.0) then
              ierr=ierr1
              herr=herr1
            endif
c  check if any binary parameters need to be estimated
            lfbnc=.true.
            lftc=.true.
            lfvc=.true.
            if (ierr1.ne.0) then
              if (ierr.lt.0) then
                ierr2=-ierr-80
                if (mod(ierr2,4).eq.0) then
                  lfvc=.true.
                  ierr2=ierr2-4
                end if
                if (mod(ierr2,2).eq.0) then
                  lftc=.true.
                  ierr2=ierr2-2
                end if
                if (ierr2.eq.1) then
                  lfbnc=.true.
                end if
              end if
              if (lfbnc) then
                call ESTBNC (i,j,ibin,ierr1,herr1)
                hmfile(ibin)='estimated by ESTBNC'
                if (ierr1.ne.0 .and. ierr.le.0) then
                  ierr=ierr1
                  herr=herr1
                endif
              end if
              if (lftc) call ESTTC (i,j,ierr1,herr1)
              if (lfvc) call ESTVC (i,j,ierr1,herr1)
c  any error from ESTBNC takes precedence over those from ESTTC or ESTVC
              if (ierr1.ne.0 .and. ierr.le.0) then
                ierr=ierr1
                herr=herr1
              end if
            end if
c  check to see if Peng-Robinson parameters need to be estimated
            if (abs(fprkij(i,j,1)).lt.1.d-20)
     &          call ESTPR (i,j,ibin,ierr1,herr1)
          enddo
        enddo
c
c  read coefficients for general mixing rule(s) from file
c
        call RDMIX (nread,icomp,jcomp,'ALL',lij,ierr1,herr1)
        if (ierr1.ne.0 .and. ierr.le.0) then
          ierr=ierr1
          herr=herr1
        end if
c  check that mixing term for each binary has been found
        lmix=.true.
        do i=1,nc-1
          do j=i+1,nc
            lmix=lmix.and.lij(i,j)
c           write (*,1024) i,j,lij(i,j),lmix
c1024       format (1x,' SETHMX  i,j,lij,lmix:        ',2i3,1x,2i2)
          enddo
        enddo
c
c  check for the presence of water in the mixture
        do i=1,nc
          if (hcas(i).eq.'7732-18-5') iwat=i
        enddo
c
c  check for the presence of alcohols in the mixture
        do i=1,nc
          if (hcas(i).eq.'67-56-1')   ialc=i   !methanol
          if (hcas(i).eq.'64-17-5')   ialc=i   !ethanol
          if (hcas(i).eq.'71-23-8')   ialc=i   !propanol
          if (hcas(i).eq.'67-63-0')   ialc=i   !ipropanol
          if (hcas(i).eq.'71-36-3')   ialc=i   !1-butanol
          if (hcas(i).eq.'78-83-1')   ialc=i   !isobutanol
          if (hcas(i).eq.'78-92-2')   ialc=i   !2-butanol
        end do
c
c  Convert LJ model to KW
        do i=1,nc-1
          do j=i+1,nc
            if (hmodmx(i,j).eq.'LJ6') then
              if (fmix(i,j,4).eq.1.d0 .and. fmix(i,j,5).eq.1.d0 .and.
     &            fmix(i,j,3).eq.0.d0) then  !Only do this last one if converting to KW0 to enforce no excess function
                onethrd=-0.33333333333333333333d0
                tc12=SQRT(tz(i)*tz(j))
                tc12=(fmix(i,j,1)+tz(i)+tz(j))/2.d0/tc12
                vc12=(rhoz(i)**onethrd+rhoz(j)**onethrd)**3/8.d0
                vc12=(fmix(i,j,2)+1.d0/rhoz(i)+1.d0/rhoz(j))/2.d0/vc12
                fij=fmix(i,j,3)
                do k=1,4
                  fmix(i,j,k)=1.d0
                  fmix(j,i,k)=1.d0
                  fmix2(i,j,k)=1.d0
                  fmix2(j,i,k)=1.d0
                enddo
                fmix(i,j,5)=fij
                fmix(j,i,5)=fij
                fmix(i,j,2)=tc12
                fmix(j,i,2)=tc12
                fmix(i,j,4)=vc12
                fmix(j,i,4)=vc12
                hmodmx(i,j)='KW0'
                hmodmx(j,i)='KW0'
              endif
            endif
          enddo
        enddo

c
c  check for Ammonia-Water model
        if (hmodmx(ione1,itwo2).eq.'TR1') then
c  set whether mixture is water-ammonia or ammonia-water
          if (hcas(ione1).eq.'7664-41-7'.and.hcas(itwo2).eq.'7732-18-5')
     &        iamwat=1
          if (hcas(ione1).eq.'7732-18-5'.and.hcas(itwo2).eq.'7664-41-7')
     &        iamwat=2
          if (iamwat.eq.0) then
            ierr=106
            herr='[SETMHX error 106] Wrong components; '//
     &           'model TR1 valid for ammonia + water only'//hnull
            call ERRMSG (ierr,herr)
          endif
          if (nc.ne.2) then
            ierr=107
            herr='[SETMHX error 107] Wrong number of components;'//
     &           ' model TR1 valid for binary mixtures only'//hnull
            call ERRMSG (ierr,herr)
          end if
        end if
c
        if (.not.lmix .and. abs(ierr).ne.117) then
          ierr=-117
          herr='[SETHMX error] Mixing term not found for one or '//
     &         'more binary pair(s)'//hnull
          call ERRMSG (ierr,herr)
        end if
c  rewind and close the file
        rewind (nread)
        close (nread)
c
c  write all parameters for debugging
c       write (*,*)
c       write (*,*) ' SETHMX:  mixing parameters'
c       write (*,1281)
c1281   format ('   i  j  mod    par_1     par_2     par_3',
c    &           '     par_4     par_5     par_6'/)
c       do i=1,nc
c       do j=1,nc
c       write (*,1280) i,j,hmodmx(i,j),(fmix(i,j,k),k=1,nmxpar)
c       enddo
c       enddo
c1280   format (1x,2i3,2x,a3,11f10.5)
c       write (*,*)
c       write (*,*) ' SETHMX:  critical line (Tc) parameters'
c       write (*,1283)
c1283   format ('   i  j  mod    par_1     par_2     par_3',
c    &           '     par_4     par_5     par_6     par_7',
c    &           '     par_8     par_9    par_10    par_11'/)
c       do i=1,nc
c       do j=1,nc
c       write (*,1280) i,j,hmodtc(i,j),(ftcij(i,j,k),k=1,nmxtc)
c       enddo
c       enddo
c       write (*,*)
c       write (*,*) ' SETHMX:  critical line (Vc) parameters'
c       write (*,1283)
c       do i=1,nc
c       do j=1,nc
c       write (*,1280) i,j,hmodvc(i,j),(fvcij(i,j,k),k=1,nmxvc)
c       enddo
c       enddo
        RETURN
      end if
c
c  error terminations
c
 999  continue
      ierr=111
      herr='[SETHMX error] Error in opening mixture file'//hnull
      call ERRMSG (ierr,herr)
      RETURN
c
 2003 format (a3)
c
      end                                             !subroutine SETHMX
c
c ======================================================================
c
      subroutine RDBNC (nread,icomp,jcomp,ibin,ierr,herr)
c
c  read binary parameters for the HMX mixture model and also the
c  critical line model from a file
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data (not currently implemented)
c           >0 read from logical unit n (file should have already
c              been opened and pointer set by subroutine SETHMX)
c    icomp--component i
c    jcomp--component j
c     ibin--counter for binary pair (1,2 = 1; 1,3 = 2; 2,3 = 3; etc.)
c  outputs:
c     ierr--error flag:  0 = successful
c                     -114 = critical temperature line will be estimated
c                     -115 = critical volume line will be estimated
c                     -116 = critical temperature and volume lines will be estimated
c                     -117 = binary pair not found, all parameters will be estimated
c                     -118 = binary pair found, but no thermo interaction parameters
c                      118 = error in reading mixture file
c     herr--error string (character*255 variable if ierr<>0)
c     other outputs written to common
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  01-10-96  MM, original version
c  01-12-96  MM, call ESTBNC if binary parameter not found in file
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  05-07-96  MM, change inputs from CAS numbers to icomp, jcomp
c  05-08-96  MM, add /MXINFO/ and read corresponding info from file
c  11-04-96  MM, add gamma to LJi and LMi models (i,j not eq to j,i),
c                change nmxpar from 4 to 6
c  11-19-96  MM, move initialization of common blocks to RDBNC, ESTBNC
c                read critical line coefficients
c  02-19-97  MM, fix bug: parameter name is ncvc, not ncvv
c  05-22-97  MM, change format to read coeff from f10.x to f12.x
c  06-04-97 EWL, change value of nctc and ncvc to read in six coefficients
c  11-28-97 APP, add code for Ammonia-Water model and common /AMMWAT/
c  12-22-98 EWL, remove common /AMMWAT/
c  05-26-00 EWL, make the i,j and j,i entries in the fvcij and ftcij identical
c  12-05-00  MM, set either 7-11 or 2-6 of ftcij and fvcij to zero's (allow for
c                specified mixture order to be opposite that in coeff file)
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c  07-15-04 EWL, read in kij for Peng-Robinson equation of state
c  10-08-04 MLH, initialize PR coefficients to zero
c  08-17-06 EWL, redo logic to make more efficient and allow nonformatted inputs
c  08-17-06 EWL, update program for GERG-2004 equation of Kunz and Wagner
c  12-24-06 MLH, allow for 4 binary interaction parameters for viscosity
c  01-09-07 EWL, modify the code so that other entries than just 'KW#' or 'LJ#'
c                can be on the same line as the CAS numbers (thus, the KW or LJ
c                entries do not have to exist)
c  01-28-07 MLH, allow for 3 binary interaction parameters for thermal conductivity
c  03-05-07 EWL, add fmix2 array in case fluids are entered in opposite order as required by Kunz and Wagner
c  05-27-08 MLH, add one more binary int par for dilute gas viscosity xdij in trnbin
c  06-11-08 MLH, add second bin int for dilute gas vis xdij2
c  07-08-10 EWL, add check for official GERG2004 coefficients
c
      include 'commons.for'
      include 'comtrn.for'
      character*1 hstar,h1
      character*3 hmodij,hmodt,hmodv
      character*3 hmodc,hmodz
      character*3 hflag,htype
      character*12 hcas1,hcas2,hcasi,hcasj
      character*255 herr,hbinp,h255,hmdijx,hmodtx,hmodvx
      character*255 hmodcx,hmodzx
c
      ierr=0
      herr=' '
      i=icomp
      j=jcomp
      hcasi=hcas(icomp)
      hcasj=hcas(jcomp)
      ifmt=27   !Starting location of all LJ1, KW1, etc., identifiers
c
c  initialize arrays storing coeff for critical lines
      xljs(i,j)=0.0d0
      xljs(j,i)=0.0d0
      xlje(i,j)=0.0d0
      xlje(j,i)=0.0d0
      xkij(i,j)=0.0d0
      xkij(j,i)=0.0d0
      xlij(i,j)=0.0d0
      xlij(j,i)=0.0d0
      xaji(i,j)=0.0d0
      xaji(j,i)=0.0d0
      xkijk(i,j)=0.0d0
      xkijk(j,i)=0.0d0
      xlijk(i,j)=0.0d0
      xlijk(j,i)=0.0d0
      xdij(i,j)=0.0d0
      xdij(j,i)=0.0d0
      xdij2(i,j)=0.0d0
      xdij2(j,i)=0.0d0
      do k=1,nmxpar
        fmix(i,j,k)=0.0d0
        fmix(j,i,k)=0.0d0
        fmix2(i,j,k)=0.0d0
        fmix2(j,i,k)=0.0d0
      enddo
      do k=1,nmxtc
        ftcij(i,j,k)=0.0d0
        ftcij(j,i,k)=0.0d0
      enddo
      do k=1,nmxvc
        fvcij(i,j,k)=0.0d0
        fvcij(j,i,k)=0.0d0
      enddo
      do k=1,nmxprb
        fprkij(i,j,k)=0.0d0
        fprkij(j,i,k)=0.0d0
      enddo
c
      if (nread.le.0) then
c  get coefficients from common blocks--not currently implemented
        ierr=1
        herr='[RDBNC error] Block data option not available'//hnull
        call ERRMSG (ierr,herr)
        RETURN
      else
c
c  rewind file and read coefficients
c
        rewind (nread)
c
c  search for key characters in cols 1 and 2-4 and branch accordingly
c  hstar = '#' or '@' indicates start of model specification
c  hflag indicates type of model:
c       'BNC' = binary mixture coefficients
c       'MXM' = mixture model specification
c
        do iline=1,20000
        read (nread,2013,end=199,err=199) hstar,hflag
        if (hstar.eq.'@' .and. (hflag.eq.'END'.or.hflag.eq.'end')) then
          ierr=-117
          herr='[RDBNC error] Binary pair not found (end of file):  '//
     &         hcasi//hcasj//hnull
          call ERRMSG (ierr,herr)
          RETURN
        end if
        if (hstar.eq.'#' .or. hstar.eq.'@') then
          read (nread,2003) htype
 100      continue
          read (nread,2001) h1
          if (h1.eq.'?') goto 100   !comment block terminated by '!'
        end if
        if (hflag.eq.'BNC' .and. htype.eq.'BNC') then
 110      continue
            hmodij=' '
            hmdijx=' '
            hmodt =' '
            hmodtx=' '
            hmodv =' '
            hmodvx=' '
            hmodc =' '
            hmodcx=' '
            hmodz =' '
            hmodzx=' '
            hbinp =' '
            hcas1 =' '
            hcas2 =' '
c  read in one block of interaction parameters at a time
 115        continue
            read (nread,2255,err=199,end=199) h255
            if (h255(1:1).eq.'?') then
              if (hbinp.eq.' ') then
                hbinp=h255(2:255)
              else
                ii=index(hbinp,'                     ')
                hbinp=hbinp(1:ii)//char(92)//h255(2:255)
              endif
              goto 115
            elseif (h255(1:1).eq.'!') then
            elseif (h255(ifmt:ifmt+1).ne.' ') then
              if (h255(1:ifmt-1).ne.' ') then
                ii=index(h255(1:ifmt-1),'/')
                if (ii.gt.0) then
                  hcas1=h255(1:ii-1)
                  hcas2=h255(ii+1:ifmt-1)
                else
                  goto 199
                endif
              endif
              if (h255(ifmt:ifmt+1).eq.'KW' .or.
     &            h255(ifmt:ifmt+1).eq.'GE' .or.
     &            h255(ifmt:ifmt+1).eq.'LJ' .or.
     &            h255(ifmt:ifmt+1).eq.'LM' .or.
     &            h255(ifmt:ifmt+2).eq.'TR1') then
                hmodij=h255(ifmt:ifmt+2)
                hmdijx=h255(ifmt+3:255)
              elseif (h255(ifmt:ifmt+1).eq.'TC') then
                hmodt =h255(ifmt:ifmt+2)
                hmodtx=h255(ifmt+3:255)
              elseif  (h255(ifmt:ifmt+1).eq.'VC') then
                hmodv =h255(ifmt:ifmt+2)
                hmodvx=h255(ifmt+3:255)
              elseif  (h255(ifmt:ifmt+1).eq.'PR') then
                hmodc =h255(ifmt:ifmt+2)
                hmodcx=h255(ifmt+3:255)
              elseif  (h255(ifmt:ifmt+2).eq.'TRN') then
                hmodz =h255(ifmt:ifmt+2)
                hmodzx=h255(ifmt+3:255)
              else
                goto 199
              endif
              goto 115
            else
              goto 120
            endif

c  check for match with CAS numbers
            if ((hcasi.eq.hcas1 .and. hcasj.eq.hcas2) .or.
     &          (hcasi.eq.hcas2 .and. hcasj.eq.hcas1)) then

              hbin(ibin)=hbinp
              hmodmx(i,j)=hmodij
              hmodmx(j,i)=hmodij      !pair j,i is same as i,j
              hmodtc(i,j)=hmodt
              hmodtc(j,i)=hmodt       !pair j,i is same as i,j
              hmodvc(i,j)=hmodv
              hmodvc(j,i)=hmodv       !pair j,i is same as i,j
              hprkij(i,j)=hmodc
              hprkij(j,i)=hmodc
              if (hmodij.ne.' ') then
                read (hmdijx,*) (fmix(i,j,k),k=1,nmxpar)
                read (hmdijx,*) (fmix(j,i,k),k=1,nmxpar)
                do k=1,nmxpar
                  fmix2(i,j,k)=1.d0  !These are not in the hmx.bnc file, manually set here
                  fmix2(j,i,k)=1.d0
                enddo
              else
               ierr=-118
               herr='[RDBNC error] Binary pair found, but equilibrium'//
     &              ' interaction parameters are not available:  '//
     &              hcasi//hcasj//hnull
               call ERRMSG (ierr,herr)
              endif
c             write (*,1016) icomp,jcomp,hmodij,(afeq(k),k=1,nmxpar)
c1016         format (1x,' RDBNC--i,j,mod,fij: ',9x,2i3,2x,a3,8f8.4)
c
c  check for specification that critical line(s) are to be estimated,
c  if so, set warning (estimation routine will be called from SETHMX)
              ierr2=0
              if (hmodt.eq.'EST') then
                ierr2=ierr2-1
              end if
              if (hmodv.eq.'EST') then
                ierr2=ierr2-2
              end if
              if (ierr2.lt.0) then
                ierr=-113+ierr2
              end if
c
c  for model TC1, the order i,j vs j,i is significant; first coeff is
c  symmetric, elements 2-6 store coeff for order i,j; 7-11 for j,i
              if (hmodt.eq.'TC1') then
                read (hmodtx,*) (ftcij(i,j,k),k=1,6)
                ftcij(j,i,1)=ftcij(i,j,1)
                do k=2,6
                  ftcij(j,i,k)=ftcij(i,j,k)
                  ftcij(i,j,k+5)=0.0d0
                  ftcij(j,i,k+5)=0.0d0
                enddo
              end if
c
c  for model VC1, the order i,j vs j,i is significant; first coeff is
c  symmetric, elements 2-6 store coeff for order i,j; 7-11 for j,i
              if (hmodv.eq.'VC1') then
                read (hmodvx,*) (fvcij(i,j,k),k=1,6)
                fvcij(j,i,1)=fvcij(i,j,1)
                do k=2,6
                  fvcij(j,i,k)=fvcij(i,j,k)
                  fvcij(i,j,k+5)=0.0d0
                  fvcij(j,i,k+5)=0.0d0
                enddo
              end if
c
c  read in Peng-Robinson interaction parameters
              if (hmodc.eq.'PR1') then
                read (hmodcx,*) (fprkij(i,j,k),k=1,nmxprb)
                do k=1,nmxprb
                  fprkij(j,i,k)=fprkij(i,j,k)
                enddo
              endif
c
c  read in transport interaction parameters
              if (hmodz.eq.'TRN') then
                read (hmodzx,*) xljs(i,j),xlje(i,j),xkij(i,j),xlij(i,j),
     &              xaji(i,j),xkijk(i,j),xlijk(i,j),xdij(i,j),xdij2(i,j)
                xljs(j,i)=xljs(i,j)
                xlje(j,i)=xlje(i,j)
                xkij(j,i)=xkij(i,j)
                xlij(j,i)=xlij(i,j)
                xaji(j,i)=xaji(i,j)
                xkijk(j,i)=xkijk(i,j)
                xlijk(j,i)=xlijk(i,j)
                xdij(j,i)=xdij(i,j)
                xdij2(j,i)=xdij2(i,j)
              end if

              if (hcasi.eq.hcas2 .and. hcasj.eq.hcas1) then
c  specified order is opposite that in file hfmix
c  pair j,i is same as i,j except for beta and gamma parameters in
c  the LJi and LMi models are associated with a particular fluid
                if (hmodij(1:2).eq.'LM' .or. hmodij(1:2).eq.'LJ') then
                  a4=fmix(i,j,4)
                  a5=fmix(i,j,5)
                  fmix(j,i,4)=a5
                  fmix(j,i,5)=a4
                  fmix(i,j,4)=a5
                  fmix(i,j,5)=a4
                end if
                if (hmodij(1:2).eq.'KW' .or. hmodij(1:2).eq.'GE') then
                  fmix2(j,i,1)=fmix(j,i,1)
                  fmix2(j,i,3)=fmix(j,i,3)
                  fmix2(i,j,1)=fmix(i,j,1)
                  fmix2(i,j,3)=fmix(i,j,3)
                  fmix(j,i,1)=1.d0
                  fmix(j,i,3)=1.d0
                  fmix(i,j,1)=1.d0
                  fmix(i,j,3)=1.d0
                end if
                if (hmodt.eq.'TC1') then       !Similar for TC1
                  do k=2,6
                    ftcij(i,j,k+5)=ftcij(i,j,k)
                    ftcij(j,i,k+5)=ftcij(j,i,k)
                    ftcij(i,j,k)=0.d0
                    ftcij(j,i,k)=0.d0
                  enddo
                end if
                if (hmodv.eq.'VC1') then       !Similar for VC1
                  do k=2,6
                    fvcij(i,j,k+5)=fvcij(i,j,k)
                    fvcij(j,i,k+5)=fvcij(j,i,k)
                    fvcij(i,j,k)=0.0d0
                    fvcij(j,i,k)=0.0d0
                  enddo
                end if
              end if
c  Check for official GERG coefficients.
c  If found, change from GE to KW, but keep reading fluid file
c  for additional KW values unless the GERG option has been turned on.
c  The GERG coefficients must be placed in the hmx.bnc file BEFORE the KW values
c  for the same binary mixture.
              if (hmodmx(i,j)(1:2).eq.'GE') then
                hmodmx(i,j)='KW'//hmodmx(i,j)(3:)
                hmodmx(j,i)=hmodmx(i,j)
                if (iGERG04.eq.0) goto 110
              endif
              RETURN
            endif
          goto 110
        end if
 120    continue
        enddo
      end if
      RETURN
c
 199  continue       !abnormal termination
c     write (*,*) ' RDBNC line 199, hstar,hflag: ',hstar,hflag
      ierr=118
      herr='[RDBNC error 118] Error in reading mixture file in RDBNC'
     &    //hnull
      call ERRMSG (ierr,herr)
      RETURN
c
 2001 format (a1)
 2003 format (a3)
 2013 format (a1,a3)
 2255 format (a255)
c
      end                                              !subroutine RDBNC
c
c ======================================================================
c
      subroutine RDMIX (nread,icomp,jcomp,hmodij,lij,ierr,herr)
c
c  read coefficients for mixing rule(s) from file
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data (not currently implemented)
c           >0 read from logical unit nread (file should have already
c              been opened by subroutine SETHMX)
c    icomp--component i; ignored if hmodij='ALL'
c    jcomp--component j; ignored if hmodij='ALL'
c   hmodij--model for binary interaction of i and j [character*3]
c           'ALL' indicates read model(s) for all binary pair(s)
c                 (used when this routine is called from SETHMX)
c           when a specific model is specified (e.g. 'LJ1' or 'MKT')
c                 read only that model (used when called from SETKTV)
c  outputs:
c      lij--flags indicating if mixing term found for binary pair (i,j)
c           [logical array of dimension (nc,nc)]; used only in
c           conjunction with hmodij='ALL'
c     ierr--error flag:  0 = successful
c                      118 = error in reading mixture file
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  05-07-96  MM, original version (extracted from SETHMX)
c  05-08-96  MM, add /MXINFO/ and read corresponding info from file
c  05-10-96  MM, read default binary parameters (used only by interface)
c  11-04-96  MM, change nmxpar from 4 to 6
c  12-00-00 EWL, add bideal variable, storing fideal
c  02-14-01 EWL, increase dimension on aterm to 4, change use of iexp to aterm
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c  07-08-02 EWL, initialize 4th term in amix to 0
c  08-17-06 EWL, update program for GERG-2004 equation of Kunz and Wagner
c  01-03-07 EWL, change the 2000 to 20000 in 'do iline=1,2000' to read in more lines
c
      include 'commons.for'
      character*1 hstar,h1
      character*3 hmodij
      character*3 hflag,htype
      character*8 hpar(nmxpar)
      character*251 hinfo
      character*255 herr
      dimension aterm(nmxtrm,nmxcof),fideal(nmxpar)
      logical lij(nx,nx)
c
c  commons associated with the binary mixing rule(s)
c  the amix(i,j,k,1..3) are the coefs for the (general) mixing term
c  the fmix(i,j,1..nmxpar) are the parameters for the i-j binary
c  the namix(i,j) are the number of afeq(i,j,k,1..3) terms
c  /MXINFO/ contains information on the mixing rules and parameters
c  hmfile specifies the files from which mixing rules originate
c  (stored in order 1,2  1,3  2,3; element zero is file called in SETUP)
c  hbin provides documentation for the current binary parameters
c  hrule contains descriptions of the currently available mixing rules
c  hbpar contains descriptions of the binary parameters (e.g. Kt, Kv)
c  associated with the currently available mixing rules
c
c  search for key characters in cols 1 and 2-4 and branch accordingly
c  hstar = '#' or '@' indicates start of model specification
c  hflag indicates type of model:
c       'BNC' = binary mixture coefficients
c       'MXM' = mixture model specification
c
      ierr=0
      do i=1,nmxtrm
        do j=1,nmxcof
          aterm(i,j)=0
        enddo
      enddo
      if (hmodij.eq.'ALL') then
c  initialize flags indicating if mixing terms have been found
        irule=0
        do i=1,nc-1
          do j=i+1,nc
            lij(i,j)=.false.
          enddo
        enddo
      else
        do k=1,nrule
          if (hmodij.eq.hrule(k)(1:3)) then
            irule=k
            goto 100
          end if
        enddo
        irule=nrule+1
c        write (*,*) ' RDMIX--new mixing rule found',hmodij
 100  continue
      end if
c  rewind file (RDBNC may have left it at the end)
      rewind (nread)
      do iline=1,20000
      read (nread,2013,end=998,err=998) hstar,hflag
      if ((hstar.eq.'#' .or. hstar.eq.'@').and. hflag.eq.'MXM') then
        if (hmodij.eq.'ALL') then
          irule=irule+1
        end if
        read (nread,2053) htype,hinfo
        do k=1,1000             !skip over source comments
          read (nread,2001) h1
          if (h1.ne.'?') goto 110     !comment block terminated by '!'
        enddo
 110    continue
c  read descriptors for binary-specific parameters
        read (nread,2048) (hpar(k),k=1,nmxpar)
c  read default (ideal-solution) parameters
        read (nread,*) (fideal(k),k=1,nmxpar)
c  read coefficients for mixing rule
        nterm=0
        ncoef=0
        nspar=0
        nterm2=0
        ncoef2=0
        read (nread,*,err=120) nterm,ncoef,nspar,nterm2,ncoef2
 120    continue
        if (nspar.eq.1) ncoef=ncoef+1  !Compatibility for version 7.0
c  nterm is number of terms
c  ncoef is number of coefficients per term
        do n=1,nmxtrm
          do k=1,nmxcof
            aterm(n,k)=0.d0
          enddo
        enddo
        do k=1,nterm
          read (nread,*) (aterm(k,n),n=1,ncoef)
        enddo
        if (nterm2.ne.0) then
          do k=1,nterm2
            read (nread,*) (aterm(k+nterm,n),n=1,ncoef2)
          enddo
        endif
        if (hmodij.eq.'ALL') then
          ipair=0               !counter for binary pair
c         write (*,1342) irule,htype,hinfo
c1342     format (1x,' SETHMX--mixing function',i3,' (',a3,'): ',a251)
          hrule(irule)=htype//hinfo
          do k=1,nmxpar
            hbpar(irule,k)=hpar(k)
            bideal(irule,k)=fideal(k)
          enddo
          do i=1,nc-1
            do j=i+1,nc
              if (htype.eq.hmodmx(i,j)) then
c  mixing term found; load coefficients into working arrays
                lij(i,j)=.true.
                ipair=ipair+1
c               write (*,1344) irule,htype,i,j,hrule(irule)
c1344           format (1x,' SETHMX:  use mixing rule #',i3,' (',a3,
c    &                 ') for i,j =',2i3,':  ',a255)
                namix(i,j)=nterm
                namix2(i,j)=nterm2
                do k=1,nterm+nterm2
                  do n=1,nmxcof
                    amix(i,j,k,n)=aterm(k,n)
                  enddo
                enddo
              end if
            enddo
          enddo
          nrule=irule
        elseif (htype.eq.hmodij) then
c  mixing term found; load coefficients into working arrays
          lij(icomp,jcomp)=.true.
c          write (*,1370) htype,icomp,jcomp
c1370     format (1x,' SETKTV:  use ',a3,' mixing term for i,j =',2i3)
c          write (*,*) ' SETHMX--mixing term irule = ',irule,htype,hinfo
          hrule(irule)=htype//hinfo
          do k=1,nmxpar
            hbpar(irule,k)=hpar(k)
            bideal(irule,k)=fideal(k)
          enddo
          namix(icomp,jcomp)=nterm
          namix(jcomp,icomp)=nterm          !pair (j,i) is same as (i,j)
          namix2(icomp,jcomp)=nterm2
          namix2(jcomp,icomp)=nterm2        !pair (j,i) is same as (i,j)
          do k=1,nterm+nterm2
            do n=1,nmxcof
              amix(icomp,jcomp,k,n)=aterm(k,n)
              amix(jcomp,icomp,k,n)=aterm(k,n)!pair (j,i) is same as (i,j)
            enddo
          enddo
        end if
      elseif (hflag.eq.'END') then
c       write (*,*) ' RDMIX end of file encountered'
        RETURN
      end if
      enddo
      RETURN
c
c  error terminations
c
 998  continue
c     write (*,*) ' SETHMX line 998, hstar,hflag: ',hstar,hflag
      ierr=118
      herr='[RDMIX error 118] Error in reading mixture file in RDMIX'
     &    //hnull
      call ERRMSG (ierr,herr)
      RETURN
c
 2001 format (a1)
 2013 format (a1,a3)
c  multiplier in format should match "parameter (nmxpar=6)"
 2048 format (6(a8,1x))     !descriptors for binary-specific parameters
 2053 format (a3,a251)
c
      end                                              !subroutine RDMIX
c
c ======================================================================
c
      subroutine ESTBNC (i,j,ibin,ierr,herr)
c
c  estimate binary parameters for the HMX mixture model
c
c  inputs:
c        i--component i
c        j--component j
c     ibin--counter for binary pair (1,2 = 1; 1,3 = 2; 2,3 = 3; etc.)
c  outputs:
c     ierr--error flag:  0 = successful
c                     -117 = no binary parameters found (warning)
c                      117 = no binary parameters found (critical error)
c     herr--error string (character*255 variable if ierr<>0)
c     other outputs written to common
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  01-12-96  MM, original version
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-19-19  MM, add dipole moment to /CCON/
c  05-06-96  MM, change default model to VDW
c  05-08-96  MM, add /MXINFO/ and load corresponding info
c  11-04-96  MM, replace van der Waals rule with linear (LIN),
c                change nmxpar from 4 to 6
c  11-19-96  MM, move initialization of common blocks to RDBNC, ESTBNC
c                change argument list
c  07-03-97  MM, return ierr=117 if this routine is called (causes error
c                alert to appear in GUI); !temporary until prediction in place
c  09-08-97  MM, change error 117 to warning--GUI displayed alert, then quit
c  11-06-97  MM, implement preliminary predictive model of Lemmon
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  04-19-99 EWL, do not use estimation scheme with hydrogen sulfide
c  06-14-99 EWL, do not use estimation scheme if estimate value > 200
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c  02-05-02 EWL, allow estimation scheme to return values, even if out of range
c  05-09-05 EWL, add check for very small acentric factor ratio
c  09-07-06 EWL, change LJ1 to LJ6
c  09-12-06 EWL, keep fmix(1) bounds within 500 to avoid crashes
c  08-23-10 EWL, remove acetone from list of fluids not allowed in mixtures
c
      include 'commons.for'
      character*255 herr
      logical lest
c
c  commons associated with the binary mixing rule(s)
c  the amix(i,j,k,1..3) are the coefs for the (general) mixing term
c  the fmix(i,j,1..nmxpar) are the parameters for the i-j binary
c
c     write (*,1002) i,j,hcas(i),hcas(j)
c1002 format (1x,' ESTBNC will estimate i,j =   ',2i3,2x,a12,1x,a12)
      lest=.true.   !enable estimation scheme
      if (.not.lest) then
c  return default values for now
        hmodmx(i,j)='LIN'
        hmodmx(j,i)='LIN'
        hbin(ibin)='No mixture data are available for this binary pair.'
     &         //' Ideal solution behavior will be assumed.'
     &         //hnull
        do k=1,2
          fmix(i,j,k)=1.0d0           !fmix(i,j,1) = Kt
          fmix(j,i,k)=1.0d0           !fmix(i,j,2) = Kv
        enddo
        fmix(i,j,3)=0.0d0           !Fpq(i,j)
        fmix(j,i,3)=0.0d0           !Fpq(k,i)
        do k=4,5
          fmix(i,j,k)=1.0d0           !fmix(i,j,4) = beta
          fmix(j,i,k)=1.0d0           !fmix(i,j,5) = gamma
        enddo
        do k=6,nmxpar
          fmix(i,j,k)=0.0d0           !not used
          fmix(j,i,k)=0.0d0
        enddo
c  return warning message to GUI
        ierr=117
        write (herr,1000) hnull
        call ERRMSG (ierr,herr)
        RETURN
      end if
      iflg=0
      do k=1,2
        n=0
        if (k.eq.1) n=i
        if (k.eq.2) n=j
        if (hcas(n).eq.'7732-18-5' ) iflg=1    !water
        if (hcas(n).eq.'7789-20-0' ) iflg=1    !heavy water
        if (hcas(n).eq.'7664-41-7' ) iflg=1    !ammonia
        if (hcas(n).eq.'7440-59-7' ) iflg=1    !helium
        if (hcas(n).eq.'1333-74-0' ) iflg=1    !hydrogen
        if (hcas(n).eq.'7782-39-0' ) iflg=1    !deuterium
        if (hcas(n).eq.'1333-74-0p') iflg=1    !parahydrogen
        if (hcas(n).eq.'67-56-1'   ) iflg=1    !methanol
        if (hcas(n).eq.'64-17-5'   ) iflg=1    !ethanol
c       if (hcas(n).eq.'67-64-1'   ) iflg=1    !acetone
        if (hcas(n).eq.'7782-41-4' ) iflg=1    !fluorine
        if (hcas(n).eq.'7440-01-9' ) iflg=1    !neon
        if (hcas(n).eq.'7783-54-2' ) iflg=1    !NF3
      enddo
      if (iflg.eq.1) then
        ierr=117
        herr='[SETUP error 117] Estimation of mixing parameters is not '
     &    //'possible for most mixtures containing water, heavy water, '
     &    //'helium, hydrogen, parahydrogen, deuterium, neon, ammonia, '
     &    //'fluorine, methanol, or ethanol.'//hnull
        hbin(ibin)='Estimation of mixing parameters is not possible '
     &    //'for this mixture.'//hnull
        hmodmx(i,j)='LJ6'
        hmodmx(j,i)='LJ6'
        call ERRMSG (ierr,herr)
        RETURN
      endif
c
c  return warning message to GUI
      ierr=-117
      herr='[SETUP warning -117] No mixture data are available for one '
     &   //'or more binary pairs in the specified mixture. The mixing '
     &   //'parameters have been estimated.'//hnull
      call ERRMSG (ierr,herr)
c
c  ordering of the components depends on ratio of the dipole moments
c  interim predictive scheme of Lemmon
      hmodmx(i,j)='LJ6'
      hmodmx(j,i)='LJ6'
      hbin(ibin)='No mixture data are available for this binary pair. '
     &         //'The mixing parameters have been estimated.'
     &         //hnull
      k=0
      if (ABS(dipole(i)-dipole(j)).lt.1.0d-5) then
c  special case for equal dipole moments
        if (tcrit(i)/(pcrit(i)*accen(i)).gt.
     &      tcrit(j)/(pcrit(j)*accen(j))) k=1
      else
        if (dipole(i).lt.dipole(j)) k=1
      end if
      if (k.eq.1) then
        tratio=tcrit(i)/tcrit(j)
        pratio=pcrit(i)/pcrit(j)
        accenr=accen(i)/accen(j)
      else
        tratio=tcrit(j)/tcrit(i)
        pratio=pcrit(j)/pcrit(i)
        accenr=accen(j)/accen(i)
      endif
      if (tratio/pratio/accenr.gt.5 .and. abs(accenr).lt.0.1d0) then
        if (tratio .gt. 0.75d0 .and. tratio.lt.1.25d0) then
          tratio=1.d0/tratio
          pratio=1.d0/pratio
          accenr=1.d0/accenr
        endif
      endif
c
c  define the zeta parameter (the one applied to the reducing temperature)
      fmix(i,j,1)=(40.4d0-25.03d0*2.0d0**(tratio/pratio/accenr))/tratio
c
c
c     c1=-4.47546077875894
c     c2=6.95801916004273
c     c3=0.301565763059889
c     c4=0.429854120106733
c     c5=0.869600175257425
c     xx=(tcrit(j)-tcrit(i))*(tcrit(i)/100.d0)**c4/100.d0
c     if (tcrit(i).gt.tcrit(j))
c    &  xx=(tcrit(i)-tcrit(j))*(tcrit(j)/100.d0)**c4/100.d0
c     fmix(i,j,1)=c1+c2*xx+c3*xx**2+c5*xx**3
c
c
c
c
      fmix(j,i,1)=fmix(i,j,1)
c  set remaining parameters to defaults; 2 = xi; 3 = Fpq; 4 = beta; 5 = gamma
      do k=2,3
        fmix(i,j,k)=0.0d0  !k = 2:  xi (term in reducing volume)
        fmix(j,i,k)=0.0d0  !k = 3:  Fpq (multiplier for excess function)
      enddo
      do k=4,5
        fmix(i,j,k)=1.0d0  !k = 4: beta (exponent for first component)
        fmix(j,i,k)=1.0d0  !k = 5: gamma (exponent for second component)
      enddo
      do k=6,nmxpar
        fmix(i,j,k)=0.0d0  !not used
        fmix(j,i,k)=0.0d0
      enddo
      if (ABS(fmix(i,j,1)).gt.200.0d0) then
        if (fmix(i,j,1).gt.500.d0) fmix(i,j,1)=500.d0
        if (fmix(i,j,1).lt.-500.d0) fmix(i,j,1)=-500.d0
        ierr=117
        write (herr,1000) hnull
        call ERRMSG (ierr,herr)
 1000   format('[SETUP error 117] No mixture data are available for',
     &   ' one or more binary pairs in the specified mixture. The',
     &   ' mixture is outside the range of the model and',
     &   ' calculations will not be made.',A1)
      endif
      RETURN
c
      end                                             !subroutine ESTBNC
c
c ======================================================================
c
      subroutine ESTTC (i,j,ierr,herr)
c
c  estimate parameters for binary critical temperature line
c
c  inputs:
c        i--component i
c        j--component j
c  outputs:
c     ierr--error flag:  0 = successful
c                        1 = error
c     herr--error string (character*255 variable if ierr<>0)
c     other outputs written to common
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-19-96  MM, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c
      include 'commons.for'
      character*255 herr
c
c     write (*,1002) i,j
c1002 format (1x,' ESTTC will estimate i,j =    ',2i3)
c  return default values for now
      hmodtc(i,j)='TC1'
      hmodtc(j,i)='TC1'
      hprkij(j,i)='   '
      do k=1,nmxtc
        ftcij(i,j,k)=0.0d0
        ftcij(j,i,k)=0.0d0
      enddo
      ierr=0
      herr=' '
      RETURN
c
      end                                              !subroutine ESTTC
c
c ======================================================================
c
      subroutine ESTVC (i,j,ierr,herr)
c
c  estimate parameters for binary critical volume line
c
c  inputs:
c        i--component i
c        j--component j
c  outputs:
c     ierr--error flag:  0 = successful
c                        1 = error
c     herr--error string (character*255 variable if ierr<>0)
c     other outputs written to common
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-19-96  MM, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c
      include 'commons.for'
      character*255 herr
c
c     write (*,1002) i,j
c1002 format (1x,' ESTVC will estimate i,j =    ',2i3)
c  return default values for now
      hmodvc(i,j)='VC1'
      hmodvc(j,i)='VC1'
      do k=1,nmxvc
        fvcij(i,j,k)=0.0d0
        fvcij(j,i,k)=0.0d0
      enddo
      ierr=0
      herr=' '
      RETURN
c
      end                                              !subroutine ESTVC
c ======================================================================
c
      subroutine AMH2OR
c
c  check for ammonia-water mixture, if set, use international standard settings
c  check that the user has not modified the reference state for their own use
c
      include 'commons.for'
      character*3 hrf
      character*255 herr
      dimension x0(ncmax)
c
      if (nc.eq.2) then
        i=0
        if (hcas(1).eq.'7732-18-5'.and. hcas(itwo2).eq.'7664-41-7') i=1
        if (hcas(itwo2).eq.'7732-18-5'.and. hcas(1).eq.'7664-41-7') i=2
        if (i.ne.0) then
          if (abs(tref(i)-300.) .lt. 0.001 .and.
     &    abs(hdef(i)-45957.).lt.1. .and. abs(sdef(i)-164.).lt.1. .and.
     &    abs(hdef(3-i)-28945.).lt.1. .and. abs(sdef(3-i)-159.).lt.1.)
     &    then
            reos(i)=8.314471d0
            Credcp(i)=8.314471d0
            hdef(i)=45957.744019720d0
            sdef(i)=164.007259009d0
            hdef(3-i)=28945.5688500127d0
            sdef(3-i)=159.336684302313d0
c           hsvrfs=' '
            ixflag=1
            h0=0.d0
            s0=0.d0
            t0=0.d0
            p0=0.d0
            hrf='DEF'
            call SETREF (hrf,ixflag,x0,h0,s0,t0,p0,ierr,herr)
          endif
        endif
      endif
      end                                             !subroutine AMH2OR
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                     end file mix_HMX.f
c ======================================================================
