c  begin file sat_sub.f
c
c  This file contains routines for saturation properties
c
c  contained here are:
c     subroutine SATT (t,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
c     subroutine SATP (p,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
c     subroutine SATD (rho,x,kph,kr,t,p,rhol,rhov,xliq,xvap,ierr,herr)
c     subroutine SATH (h,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,ierr,herr)
c     subroutine SATE (e,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,ierr,herr)
c     subroutine SATS (s,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,
c    &                 k3,t3,p3,d3,ierr,herr)
c     subroutine MAXWELL (icomp,ts,ps,dl,dv,ierr,herr)
c     subroutine EHMAXX (x,ieflag,ehmax,tehmax,ierr,herr)
c     subroutine SMAXX (x,smx,tsmx,smn,tsmn,smmin,tsmmin,ierr,herr)
c     subroutine CSATK (icomp,t,kph,p,rho,csat,ierr,herr)
c     subroutine CV2PK (icomp,t,rho,cv2p,csat,ierr,herr)
c     subroutine DPTSATK (icomp,t,kph,p,rho,csat,dpt,ierr,herr)
c     subroutine TPRHOB (t,p,rho1,rho2,x,rho,ierr,herr)
c     subroutine DLDV (t,p,rhol,rhov,xl,xv,ierr,herr)
c     subroutine LIQSPNDL (t,x,rhol,ierr,herr)
c     subroutine VAPSPNDL (t,x,rhov,ierr,herr)
c     subroutine SETVSPNDL (icomp,ierr,herr)
c     subroutine SETLSPNDL (icomp,ierr,herr)
c     subroutine FNCRPT (x,d,p,t,ierr,herr)
c     function CRTDEN (x,t,d)
c     subroutine SATTEST (t,x,kph,p,x2,ierr,herr)
c     subroutine SATPEST (p,x,kph,t,x2,ierr,herr)
c     subroutine PTEST (inp,t,p,alpha,x,tc2,pc2,acf2,calc,xout,ierr,herr)
c     subroutine SATTP (t,p,x,iFlsh,iGuess,d,Dl,Dv,xliq,xvap,q,ierr,herr)
c     subroutine SATEST (iFlash,t,p,x,xliq,xvap,ierr,herr)
c     subroutine SAT0EST (t,p,x,xliq,xvap,ierr,herr)
c     subroutine GETXY (z,vf,philn,phi,x,y)
c     subroutine LGPHI (iderv,iphase,t,rho,p,x,philn,dpt,dpdrho,ddt,cp,
c    &                  ierr,herr)
c     subroutine CRTPNT (z,tc,pc,rhoc,ierr,herr)
c     subroutine SATGV (t,p,z,vf,b,ipv,ityp,isp,rhox,rhoy,x,y,ierr,herr)
c     subroutine SATSPLN (z,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      subroutine SATT (t,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
c
c  iterate for saturated liquid and vapor states given temperature
c  and the composition of one phase
c
c  inputs:
c        t--temperature [K]
c                       if t is negative, then use other variables as
c                       initial guesses at abs(t)
c        x--composition [array of mol frac] (phase specified by kph)
c      kph--phase flag: 1 = input x is liquid composition (bubble point)
c                       2 = input x is vapor composition (dew point)
c                       3 = input x is liquid composition (freezing point)
c                       4 = input x is vapor composition (sublimation point)
c  outputs:
c        p--pressure [kPa]
c     rhol--molar density [mol/L] of saturated liquid
c     rhov--molar density [mol/L] of saturated vapor
c           For a pseudo pure fluid, the density of the equilibrium phase
c           is not returned.  Call SATT twice, once with kph=1 to get
c           pliq and rhol, and once with kph=2 to get pvap and rhov.
c     xliq--liquid phase composition [array of mol frac]
c     xvap--vapor phase composition [array of mol frac]
c     ierr--error flag:   0 = successful
c                         1 = T < Tmin
c                         8 = x out of range
c                         9 = T and x out of range
c                       120 = CRITP did not converge
c                       121 = T > Tcrit
c                       122 = TPRHO-liquid did not converge (pure fluid)
c                       123 = TPRHO-vapor did not converge (pure fluid)
c                       124 = pure fluid iteration did not converge
c           following 3 error codes are advisory--iteration will either
c           converge on later guess or error out (ierr = 128)
c                      -125 = TPRHO did not converge for parent ph (mix)
c                      -126 = TPRHO did not converge for incipient (mix)
c                      -127 = composition iteration did not converge
c                       128 = mixture iteration did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  07-11-95  MM, original version
c  09-11-95  MM, add error string to argument list
c  09-25-95  MM, rearrange argument list (outputs in order p, rho, x)
c  10-06-95  MM, use stored acentric factor for pure fluids
c  10-11-95  MM, RETURN if any error detected
c  11-26-95  MM, Raoult's law as first guess for mixture
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-18-95  MM, fill xliq, xvap with zeros for undefined components
c  12-19-20  MM, add full mixture iteration using fugacity
c  12-27-95  MM, pratio for new pressure if no converge for TPRHO for mix
c  01-09-96  MM, move check for supercritical outside nc = 1 block
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-06-96  MM, reset warning from TPRHO if loop eventually converges
c  03-19-96  MM, add dipole moment to /CCON/
c  04-05-96  MM, test for supercritical '.ge. tc' rather than '.gt. tc'
c  05-30-96  MM, check input temperature against limits
c  06-03-96  MM, add 'EOS' to calling list for LIMITX
c  06-05-96  MM, refine error numbers and messages;
c                also ensure that all outputs are set on error condition
c  11-14-96  MM, adjust pratio, etc to get closer to critical
c   2-07-96 EWL, add pressure increment/decrement when TPRHO does not converge
c   2-13-96 EWL, add initial guess for densities near the critical point
c                return critical point values if within delta of Tc
c   6-06-96 EWL, return critical point values for failure to converge if
c                Tc - T < 10 mK
c  10-01-97  MM, add compiler switch to allow access by DLL
c  11-13-97 EWL, initialize fpit(j); potential bomb if no value when writing error message
c  11-14-97 EWL, add line following 500 to improve critical region convergence
c  02-09-98  MM, limit delp step, change delp if TPRHO does not converge
c  02-11-98  MM, check that new guess for mix pressure is < p_crit
c  03-24-98 EWL, use critical region initial guesses when converge fails
c                due to liquid and vapor roots being equal
c  04-06-98 EWL, call subroutine AG to get Gibbs energy
c  07-30-98  MM, check for f1=0 in phase 2 iteration; separate kguess for each phase
c  08-03-98 EWL, check for fpit(2)-fpit(1)<>0 before calculation of pit(3)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-22-98 EWL, zero out all elements of xliq and xvap
c  12-22-98 EWL, use pure fluid algorithm when called with a mixture but x(i)=1
c  02-04-99 EWL, increase tolerance if number of iterations hits 8 or 12
c  02-04-99 EWL, if fugacity is zero, don't restart, just change x2
c  02-04-99 EWL, add check for equal densities
c  02-04-99 EWL, allow pressure to be greater than critical pressure
c  02-04-99 EWL, do not allow outer loop to converge if inner loop has not
c  02-04-99 EWL, if TPRHO fails, set x2 to original values before restarting
c  08-23-99 EWL, change khpsav to kphsav
c  11-16-99 EWL, do not modify x2new(i) after the check for fugacity=0
c                when x2(i)=x1(i)=0 (composition was set to zero by user)
c  02-23-00 EWL, add kph=3,4 as inputs and call melting or sublimation lines
c  03-14-00 MLH, change 'do 100 i=1,ncmax' from ncmax to nc
c  12-18-00 EWL, use different pratio for dew and bubble sides
c  02-27-01 EWL, don't allow too large of jumps on first mixture iteration for p
c  07-16-01 EWL, add calls to ancillary equations to get better estimates
c  07-16-01 EWL, call THERM one last time on the liquid side once converged
c  11-08-01 EWL, add alternative method that converges near the critical point
c  11-20-01 EWL, check for ierr=-16 (t<ttrp)
c  02-25-02 EWL, allow initial guesses to be passed in by negating the pressure
c  05-28-02 EWL, check for bad root in two phase
c  09-19-02 EWL, add ancillary check for liquid pressure
c  09-19-02 EWL, exit after calculating ancillaries for pseudo-pure fluids
c  07-28-03 EWL, add check to keep x2 from bouncing around
c  11-30-04 EWL, add checks to remove crashes (p>1.d5, rhol-rhov<.1)
c  11-16-05 EWL, add check for negative temperature for use in getting initial values
c  07-21-08 EWL, add missing parenthesis in check for delp-delp2
c  02-18-08 EWL, add check for icomp<>old icomp (icsav)
c  04-12-10 EWL, add check for dp/dT negative
c  04-22-10 EWL, rename variables in TSTSAV
c  09-15-11  MK, add call to Maxwell subroutine
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATT
c
      include 'commons.for'
      character*255 herr,herr2,herr3
      dimension x(ncmax),xliq(ncmax),xvap(ncmax),x2org(ncmax),xs(ncmax)
      dimension x2(ncmax),f1(ncmax),f2(ncmax),x2new(ncmax)
      dimension pit(3),fpit(2)
c
      iaga=0
      call ISPURE (x,icomp)
      herr2=hnull
      if (lsatt) then
        if (ABS(t-tsavt).lt.1.0d-9 .and. kph.eq.kphsvt) then
          lsame=.true.
          if (icomp.ne.icsavt) lsame=.false.
          if (icomp.eq.0) then
            do i=1,nc
              if (ABS(x(i)-xsavt(i)).gt.1.0d-9) lsame=.false.
            enddo
          endif
          if (lsame) then
            p=psavt
            rhol=dlsavt
            rhov=dvsavt
            do i=1,nc
              xliq(i)=xlsavt(i)
              xvap(i)=xvsavt(i)
            enddo
            ierr=0
            herr=' '
            RETURN
          endif
        endif
      endif
c  set tolerance and maximum number of iterations
      tolr=1.0d-6
      itmax=25
c     write (*,*) ' SATT--entering with t,kph = ',t,kph
      ierr=0
      iflag=0        !flag for equal densities close to critical
      herr=' '
      delp2=1.d6
      fpit(1)=0.0d0
      fpit(2)=0.0d0
      initflg=0
c  make the temperature negative to use the other parameters in the call
c  statement as initial guesses
      if (t.lt.0.d0) then
        initflg=1
        t=abs(t)
      else
        p=0.0d0
        rhol=0.0d0
        rhov=0.0d0
        if (icomp.eq.0) then
          do i=1,nc
            xliq(i)=x(i)
            xvap(i)=x(i)
          enddo
        else
          do i=1,nc
            xliq(i)=0.d0
            xvap(i)=0.d0
          enddo
          xliq(icomp)=1.d0
          xvap(icomp)=1.d0
        endif
      endif
c
c  check if melting or sublimation line requested and call appropriate routines.
      if (kph.eq.3) then      !liquid/solid
        call MELTT (t,x,p,ierr2,herr2)
        rhol=dtpn(1)
        if (p.gt.1.d-15) call TPRHO (t,p,x,1,1,rhol,ierr,herr)
        if (ierr.eq.0) then
          ierr=ierr2
          herr=herr2
        endif
        goto 900
      else if (kph.eq.4) then      !vapor/solid
        call SUBLT (t,x,p,ierr2,herr2)
        if (t.gt.0.d0) rhov=p/(R*t)
        if (rhov.lt.1.d10) call TPRHO (t,p,x,2,1,rhov,ierr,herr)
        if (ierr.eq.0) then
          ierr=ierr2
          herr=herr2
        endif
        goto 900
      end if
c
c  check that input conditions (in this case t and x) are within limits
c
      Ddum=0.0d0
      pdum=0.0d0
      call LIMITX ('EOS',t,Ddum,pdum,x,tmin,tmax,rhomax,pmax,ierr,herr2)
      if (ierr.gt.0 .or. ierr.eq.-16) then
        if (ierr.eq.1 .and. t.lt.tmin .and. icomp.ne.0) then      !vapor/solid
          ierr2=ierr
          call SUBLT (t,x,p,ierr,herr)
          if (t.gt.0.d0 .and. p.gt.0.d0) then
            rhov=p/(R*t)
            if (rhov.lt.1.d10) call TPRHO (t,p,x,2,1,rhov,ierr3,herr3)
            goto 900
          endif
          ierr=ierr2
        endif
        ierr=abs(ierr)
c  T and/or x are out of bounds, set error flag and return
        write (herr,1000) ierr,herr2(1:238),hnull
 1000   format ('[SATT error',i3,'] ',a238,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      call CRITP (x,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
c  error condition--set outputs, issue warning, and return
        ierr=120
        p=0.0d0
        rhol=0.0d0
        rhov=0.0d0
        do i=1,nc
          xliq(i)=x(i)
          xvap(i)=x(i)
        enddo
        write (herr,1120) herr2(1:237),hnull
 1120   format ('[SATT error 120] ',a237,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      acf=0.d0
      if (icomp.eq.0) then
        do i=1,nc
          acf=acf+x(i)*accen(i)
        enddo
      endif
c
      if (icomp.ne.0) acf=accen(icomp)
      if (t.gt.tc-1.0d-8 .and. icomp.ne.0) then
c  input temperature is equal to or greater than critical,
c  return critical parameters for output pressure and densities
        p=pc
        rhol=rhoc
        rhov=rhoc
        do i=1,nc
          xliq(i)=x(i)
          xvap(i)=x(i)
        enddo
        if (t.gt.tc+1.0d-8) then
c  supercritical temperature as input, set error flag and return
c  critical parameters for output pressure and densities
          ierr=121
          write (herr,1121) t,tc,hnull
          call ERRMSG (ierr,herr)
 1121     format ('[SATT error 121] ',
     &            'temperature input to saturation routine is ',
     &            'greater than critical temperature; T =',g12.5,
     &            ' K, Tcrit =',g12.5,' K.',a1)
        end if
        RETURN
      end if
c
c  generate initial guess for pressure using acentric factor
c
      if (initflg.eq.0) then
        p=pc*10.d0**(-2.333333d0*(1.d0+acf)*(tc/t-1.d0))
      endif
c  for near-critical states, generate initial guesses for density;
c  using correlation developed by E.W. Lemmon, NIST
      theta=(ABS(1.0d0-t/tc))**(1.0d0/3.0d0)*(1.5d0+acf)
      rholi=rhoc*(1.0d0+1.113614d0*theta+0.080400d0*theta**2)
      rhovi=rhoc*(1.0d0-1.078683d0*theta+5.014057d-2*theta**2)
      if (rhovi.lt.0.d0) rhovi=1.d-6
c
      if (heos.eq.'AGA') then
        heos='HMX'
        iaga=1
      endif
c
      if (icomp.eq.0 .and. iSatFlg.eq.0) then
        call SATTP (t,p,x,kph,0,d,rhol,rhov,xliq,xvap,q,ierr,herr)
        if (ierr.eq.0 .and. kph.eq.2 .and. rhol.lt.rhov*0.8d0) then
          call SATGV (t,p,x,1.d0,d,1,1,1,rhol,rhov,xliq,xvap,ierr,herr)
        endif
        if (ierr.eq.0) goto 900
      endif
c
c  pure fluid iteration
      if (icomp.ne.0) then
        do i=1,nc
          xliq(i)=0.d0
          xvap(i)=0.d0
        enddo
        xliq(icomp)=1.d0
        xvap(icomp)=1.d0
c
c  Iterate for saturated liquid and vapor states given temperature using
c  a simple successive substitution method.  The independent variable
c  in the iteration is the vapor pressure.  The convergence criteria
c  is equality of Gibbs free energy in both phases.
c
        if ((t.gt.0.99d0*tc .or. iflag.eq.1) .and. ianc(icomp).eq.0)then
          goto 300
        else
c  assume nothing about densities on initial calls to TPRHO
          kguess=0
        endif

        rhol=rholi
        rhov=rhovi
        if (hpsk(icomp).ne.' ' .and. hpsk(icomp).ne.'NBS') then
          call PSATK (icomp,t,p,ierr,herr)
        endif
        if (hplk(icomp).ne.' ' .and. hplk(icomp).ne.'NBS'
     &                         .and. kph.eq.1) then
          call PLSATK (icomp,t,p,ierr,herr)!Get liquid pressure for mixtures
        endif
        if (hdlk(icomp).ne.' ' .and. hdlk(icomp).ne.'NBS') then
          call DLSATK (icomp,t,rhol,ierr,herr)
          if (ierr.gt.0) rhol=rholi
          if (ierr.le.0) kguess=1
        endif
        if (hdvk(icomp).ne.' ' .and. hdvk(icomp).ne.'NBS') then
          call DVSATK (icomp,t,rhov,ierr,herr)
          if (ierr.gt.0) rhov=rhovi
          if (ierr.le.0) kguess=1
        endif
        if (ianc(icomp).eq.1 .and. kph.eq.2) then
          call TPRHO (t,p,x,2,kguess,rhov,ierr,herr) !find vapor density
          rhol=rhov   !Don't return liquid density for equilibrium phase
          goto 900    !For pseudo-pure fluid, stop here
        elseif (ianc(icomp).eq.1) then
          call TPRHO (t,p,x,1,kguess,rhol,ierr,herr)!find liquid density
          rhov=rhol   !Don't return vapor density for equilibrium phase
          goto 900    !For pseudo-pure fluid, stop here
        endif

c  call new Maxwell routine of Matthias Kunick
c       call MAXWELL (icomp,t,p,rhol,rhov,ierr,herr)
c       if (ierr.eq.0) return

        do 200 it=1,itmax
c       write (*,*) 'SATT--t,p input to TPRHO: ',t,p
        call TPRHO (t,p,x,1,kguess,rhol,ierr,herr2) !find liquid density
        if (ierr.ne.0 .or. rhol.le.0.d0) then
          kguess=0
          p=p*1.005d0
          goto 200
        end if
        call TPRHO (t,p,x,2,kguess,rhov,ierr,herr2) !find vapor density
        if (ierr.ne.0 .or. rhov.le.0.d0) then
          p=p*0.95d0
          goto 200
        end if
c  use previous densities as initial guesses for calls to TPRHO after
c  first iteration
        kguess=1
c
        call AG (t,rhol,x,Aliq,Gliq)
        call AG (t,rhov,x,Avap,Gvap)
        ZG=Gliq-Gvap
c       write (*,1014) it,t,rhol,rhov,p,Gliq,Gvap,ZG
c1014   format (1x,'SATT:  it,t,rhol,rhov,p,Gliq,Gvap,ZG: ',
c    &              i4,f8.3,2f12.8,e14.6,2f12.4,e14.6)
c
c  check convergence
c
c  check that liquid and vapor densities are different
        if (ABS(rhol-rhov).lt.1.0d-8 .or.
     &     (ABS(1.d0/rhol-1.d0/rhov).lt.0.1d0 .and. t.lt.tc-1.d0)) then
          ierr=124
          herr='[SATT error 124] density roots equal'
          call ERRMSG (ierr,herr)
          goto 300
        end if
        delpp=ZG/(1.d0/rhol-1.d0/rhov)
c  the delp-delp2 check is only important for very low pressures on the
c  liquid surface (propane or R124).  See comments in TPRHO.
        if (abs(delpp/p).lt.tolr .or.
     &       abs((delpp-delp2)/delp2).lt.1.d-11) then
          p=p-delpp
          call TPRHO (t,p,x,2,kguess,rhov,ierr,herr2) !find vap density
          if (ierr.ne.0 .or. p.gt.pc .or. rhov.gt.rhoc) then
            ierr=123
            write (herr,1123) it,herr2(1:147),hnull
 1123       format ('[SATT error 123] vapor density iteration in ',
     &            'saturation routine did not converge for pressure ',
     &            'iteration',i3,'; ',a147,a1)
            call ERRMSG (ierr,herr)
c  return critical parameters if not converged and very close to Tc
            goto 300
          end if
          call TPRHO (t,p,x,1,kguess,rhol,ierr,herr2) !find liq density
c  call THERM again to get exact p at T and rhol.  This is only important
c  for very low pressures.
c...(12-13-06 EWL)  Call THERM with RHOV if p < 1 kPa.  This was put in place
c                   because some of the pressures were coming back erratic
c                   from the PR model (for example, butane at T<160 K)
          if (p.gt.1.d0) then
            call THERM (t,rhol,x,p,e,h,s,cv,cp,w,hjt)
          else
            call THERM (t,rhov,x,p,e,h,s,cv,cp,w,hjt)
          endif
          if (ierr.ne.0 .or. p.gt.pc .or. rhol.lt.rhoc) then
            ierr=122
            write (herr,1122) it,herr2(1:146),hnull
 1122       format ('[SATT error 122] liquid density iteration in ',
     &            'saturation routine did not converge for pressure ',
     &            'iteration',i3,'; ',a146,a1)
c  return critical parameters if not converged and very close to Tc
            goto 300
          end if
c  !debug--next six lines for debug only
c         call GIBBS (t,rhol,x,Aliq,Gliq)
c         call GIBBS (t,rhov,x,Avap,Gvap)
c         ZG=Gliq-Gvap
c         write (*,1015) it,t,rhol,rhov,p,Gliq,Gvap,ZG
c1015   format (1x,'SATT:  it,t,rhol,rhov,p,Gliq,Gvap,ZG: ',
c    &              i4,f8.3,2f12.8,e14.6,2f12.4,e14.6)
          ierr=0
          herr=' '
          goto 900         !normal termination for pure fluid
        end if
        delp2=delpp
c
c  continue iteration, define next guess (check that delp
c  will not result in negative [or very small] pressure)
c  02-09-98 MM:  limit step size
        if (ABS(delpp).gt.0.4d0*p) then
          do jj=1,10
c           write (*,1198) p,delp
c1198       format(1x,'% SATT advisory; delp gives p<0; p,delp:',2e12.4)
            delpp=0.5*delpp
            if (ABS(delpp).lt.0.4d0*p) goto 110
          enddo
        end if
 110    continue
        p=p-delpp
        if (p.gt.1.d5) goto 210   !prevent overflow
c
 200    continue
c  iteration has not converged
 210    continue
        ierr=124
        write (herr,1124) t,hnull
 1124   format ('[SATT error 124] ',
     &          'iteration for saturation state did not converge; ',
     &          'T =',g12.5,' K.',a1)
        call ERRMSG (ierr,herr)
c  return critical parameters if not converged and very close to Tc
        if (t.gt.0.999975*tc) then
          p=pc
          rhol=rhoc
          rhov=rhoc
        end if
c
c  Alternative method for finding saturation boundaries.  The routines works
c  best near critical by finding the spinodal points on the vapor and liquid
c  sides, and uses these points to bound the iteration.
 300    continue
        if (rholi.gt.0.d0 .and. rhovi.gt.0.d0) then
          call LIQSPNDL (t,x,rholi,ierr,herr)
          call VAPSPNDL (t,x,rhovi,ierr,herr)
          if (ierr.gt.0) goto 390
c
c  Get the pressures at the spinodals and find the densities in the opposite
c  phase at the spinodal pressures.  The liquid pressure could be negative.
c  These new densities will then bound the iteration, i.e., the liquid density
c  will be between rholi and rholj, and likewise for the vapor density.
          call PRESS (t,rholi,x,pl)
          call PRESS (t,rhovi,x,pv)
          CALL TPRHOB (t,pv,rholi,rholi*2.d0,x,rholj,ierr,herr)
          if (pl.le.0.d0) then
            rhovj=0.d0
          else
            CALL TPRHOB (t,pl,0.d0,rhovi,x,rhovj,ierr,herr)
          endif

c  Find the saturation condition.  Use the midpoint of the spinodal pressures
c  for the initial guess.  Call TPRHOB to get both densities, and call AG to
c  get Gibbs energy.  The difference in G is used to get the next pressure.
          it=0
          p=(pl+pv)/2.d0
          if (p.lt.0.d0) p=pv/2.d0
 370      continue
          CALL TPRHOB (t,p,rhovi,rhovj,x,rhov,ierr,herr)
          if (ierr.ne.0) then
            p=p*1.001d0
            goto 380
          endif
          CALL TPRHOB (t,p,rholi,rholj,x,rhol,ierr,herr)
          if (ierr.ne.0) then
            p=p*0.999d0
            goto 380
          endif
          call AG (t,rhol,x,Aliq,Gliq)
          call AG (t,rhov,x,Avap,Gvap)
          ZG=Gliq-Gvap
          delpp=ZG/(1.d0/rhol-1.d0/rhov)
          if (p-delpp.lt.0.d0) then
            p=p/2.d0
          else
            p=p-delpp
          endif
          if (abs(delpp/p)*100.lt.tolr) goto 900   !Exit when solved
 380      continue
          it=it+1
          if (it.lt.20) goto 370
 390      continue
          ierr=124
          write (herr,1124) t,hnull
          call ERRMSG (ierr,herr)
          p=pc
          rhol=rhoc
          rhov=rhoc
        endif
c
c  end of pure fluid iteration
c
      else
c
c  begin mixture iteration
c
c  Iterate for the pressure and the composition of the incipient phase
c  (vapor phase for a bubble point calculation, liquid for dew point)
c  given temperature and the composition of the parent phase.  Iteration
c  is generally based on the algorithm given by Smith & Van Ness (Intro
c  to Chem Engr Thermo, McGraw-Hill, 1975); convergence criteria is the
c  equality of fugacity for each component in both phases.
c
        x2sum=0.d0
        tolr=1.d-8
        call SATTEST (t,x,kph,psum,x2,ierr,herr)
        if (psum.gt.pc) psum=0.99d0*pc  !helps critical region conv.
c
c  variable kph2 specifies the state of the incipient phase (x2):
c     1 = liq,  2 = vap
c  it is used in calls to TPRHO
c  pratio is multiplier for pressure to use when TPRHO does not converge
c  different values for liquid and vapor phases, such that new guess for
c  pressure is further into corresponding single-phase region
        if (kph.eq.1) then
          kph2=2
          prtio1=1.05d0          !Choose different pratio for the dew
          prtio2=1.02d0          !and bubble sides to avoid loops
          if (initflg.ne.0) then
            do i=1,nc
              x2(i)=xvap(i)
            enddo
            psum=p
          endif
        else
          kph2=1
          prtio1=0.95d0
          prtio2=0.98d0
          if (initflg.ne.0) then
            do i=1,nc
              x2(i)=xliq(i)
            enddo
            psum=p
          endif
        end if
c
c  begin main mixture iteration--outer loop for pressure,
c  using Raoult's Law result from above as first guess
c
        p=psum
        do i=1,nc
          if (kph.eq.1) then
            xvap(i)=x2(i)
          else
            xliq(i)=x2(i)
          endif
        enddo
c
        kgues1=0              !for first calls to TPRHO
        kgues2=0              !kguess flags for parent & incipient phase
        lppos=.false.         !flags for reguli-falsi iteration
        lpneg=.false.
        pneg=0.0d0
        ppos=0.0d0
        fpneg=0.0d0
        fppos=0.0d0
        if (initflg.eq.0) then
          rho1=0.0d0
          rho2=0.0d0
        else
          kgues1=1
          kgues2=1
          if (kph.eq.1) then
            rho1=rhol
            rho2=rhov
          else
            rho1=rhov
            rho2=rhol
          endif
        endif
        tbad2=0.0d0
        ibad1=0
        ibad2=0
        j=1
        do ii=1,nc
          x2org(ii)=x2(ii)
        enddo
        pit(1)=psum           !first guess for pressure = sum (x1*Pi)
c
c
c
c       write (*,*) ' SATT--begin outer iteration loop for pressure'
        do 400 itpp=1,itmax
c  increase tolerance to account for errors in numerical derivatives in FGCTY2
        if (itpp.eq.8) tolr=tolr*10
        if (itpp.eq.12) tolr=tolr*10
        if (itpp.eq.15) tolr=tolr*10
        p=pit(j)
c       write (*,*) ' SATT--pressure iteration',itpp,' w/ p =',pit(j)
        lx2con=.false.        !flag for convergence of inner loop
c  compute density and fugacities for parent phase
        i=iprnterr
        iprnterr=0
        call TPRHO (t,p,x,-kph,kgues1,rho1,ierr,herr2)   !parent phase
        iprnterr=i
        kgues1=1
        if (kph.eq.1) then
          if (rho1.lt.rhoc/2.d0) ierr=1
          if (rho1.lt.rhoc*1.2d0 .and.itpp.eq.1 .and.initflg.eq.0)ierr=1
        endif
        if (ierr.gt.0) then
          ibad1=ibad1+1
          if (ibad1.lt.6) then
            if (kph.eq.1) then
              call DLDV (t,p,rho1,rho2,x,x2,ierr,herr2)
            else
              call DLDV (t,p,rho2,rho1,x2,x,ierr,herr2)
            endif
            pit(j)=p
          endif
          if (ierr.ne.0) then
            ierr=-125
            write (herr,1125) itpp,herr2(1:149),hnull
 1125       format ('[SATT advisory -125] density iteration in ',
     &              'saturation routine did not converge for pressure ',
     &              'iteration',i3,'; ',a149,a1)
            kgues1=0              !do not reuse faulty density as guess
            pit(j)=pit(j)*prtio1 !try another pressure and use up one
            goto 400              !iteration (to prevent infinite loop)
          else
            kgues2=1
          endif
        end if
        call FGCTY2 (t,rho1,x,f1,ierr,herr)
c       write (*,1082) itpp,t,p,rho1,(x(i),i=1,2),(f1(i),i=1,2)
c1082   format (1x,' SATT--phase 1:  ',i3,f8.2,2e14.6,2e16.8,2e18.10)
c
c  begin inner iteration loop for composition of phase 2
c
c       write (*,*) 'SATT--begin inner loop for composition of phase 2'
c
        sumdl2=2.0d0
        do itx=1,itmax*2
        do i=1,nc
        xs(i)=x2(i)
        enddo
c  compute density and fugacities for phase 2
        i=iprnterr
        iprnterr=0
        call TPRHO (t,p,x2,-kph2,kgues2,rho2,ierr,herr2)!incipient phase
        if (itx.ne.1) then
          if (ABS(rho2-rhoc).lt.1 .and. ABS(rho1-rhoc).gt.10) then
            rho2o=rho2
c  jump away from critical point when a bad root was found
            rho2=rho2*.5d0
            ierr2=ierr
            call TPRHO (t,p,x2,-kph2,kgues2,rho2,ierr,herr3)
            if (ierr.ne.0 .or. ABS(rho1-rho2).lt.0.1d0) then
              herr2=herr3
              ierr=ierr2
              rho2=rho2o
            endif
          endif
        endif
        iprnterr=i
        kgues2=1
        itx1=0
 420    continue
        if (ierr.gt.0 .or. ABS(rho1-rho2).lt.0.1d0) then
          ibad2=ibad2+1
          if (ibad2.lt.15) then
            if (ABS(rho1-rho2).lt.0.1d0 .and.
     &      ABS(tcrit(1)-tcrit(izero0+2)).gt.100.d0) then  !Use izero0+2 to trick the compiler to ignore an error for element 2 when ncmax=1
              do i=1,nc
                x2(i)=(tbad2*x(i)+x2org(i))/(1.d0+tbad2)
              enddo
              tbad2=tbad2+0.2d0
            endif
            if (kph.eq.1) then
              call DLDV (t,p,rho1,rho2,x,x2,ierr,herr2)
            else
              call DLDV (t,p,rho2,rho1,x2,x,ierr,herr2)
            endif
            call FGCTY2 (t,rho1,x,f1,ierr,herr)
            pit(j)=p
          endif
          if (ABS(f1(1)).gt.1.d10) goto 810
          if (ierr.ne.0 .or. ABS(rho1-rho2).lt.0.1d0) then
            ierr=-126
            write (herr,1126) itx,herr2(1:146),hnull
 1126       format ('[SATT advisory -126] density iteration in ',
     &            'saturation routine did not converge for composition',
     &            ' iteration',i3,'; ',a146,a1)
            do ii=1,nc
              x2(ii)=x2org(ii)
            enddo
            kgues2=0              !do not reuse faulty density as guess
            pit(j)=pit(j)/prtio2 !try another pressure and use up one
            goto 400              !iteration (to prevent infinite loop)
          endif
        endif
        call FGCTY2 (t,rho2,x2,f2,ierr,herr)
c       write (*,1086) itx,rho2,(x2(i),i=1,2),(f2(i),i=1,2)
c1086   format (1x,'       phase 2:  ',i3,22x,e14.6,2e16.8,2e18.10)
c  calculate new x2's by ratio of fugacities; inner loop has converged
c  when x2's change by less than a convergence tolerance
        x2sum=0.0d0
        do i=1,nc
          if (f2(i).gt.1.0d-20 .and. f1(i).gt.0.0d0) then
            x2new(i)=x2(i)*f1(i)/f2(i)
          else
c  in case fugacity is zero; e.g., if x(i)=0, then slightly modify x2(i)
            if (x(i).gt.0.d0) then
              rho2=rho1
              itx1=itx1+1
              if (itx1.lt.10) goto 420
            endif
            x2new(i)=0
c           if (x2(i).gt.0) x2new(i)=x2(i)+0.01d0
c           if (x2new(i).gt.1.0d0) x2new(i)=x2(i)-0.01d0
          end if
          x2sum=x2sum+x2new(i)
        enddo
        if (x2sum.le.0.d0 .or. x2sum.gt.1.d6) goto 810
c  normalize the x2 compositions; this yields next guess for x2 and
c  ensures that the x2 always sum to one
        sumdel=0.0d0
        do i=1,nc
          x2new(i)=x2new(i)/x2sum
          sumdel=sumdel+abs(x2(i)-x2new(i))  !change in compositions
          x2(i)=x2new(i)
        enddo
c       write (*,1560) (x2(i),i=1,nc)
c1560   format (1x,'SATT:  new compositions in inner loop:  ',5f10.5)
        if (sumdel.lt.tolr .or. abs(sumdel-sumdl2).lt.tolr*100.d0) then
c  inner iteration loop has converged
          lx2con=.true.
          ierr=0
          goto 700
        end if
        sumdl2=sumdel
c  if not, continue inner iteration loop
        if (int(itx/10)*10.eq.itx) then
c  occasionally average out the compositions on two successive iterations.
c  sometimes x2 bounces back and forth between two values.
          do i=1,nc
            x2(i)=(x2(i)+xs(i))/2.d0
          enddo
        endif
        enddo
c
c  inner iteration loop has not converged
        ierr=-127
        write (herr,1127) t,sumdel,hnull
 1127   format ('[SATT advisory -127] ',
     &          'iteration for composition in saturation routine ',
     &          'did not converge; T =',g12.5,
     &          ' K; deltaX =',g12.5,' mol frac.',a1)
        call ERRMSG (ierr,herr)
c
c  end of inner (x2) iteration loop
c
 700    continue
        fpit(j)=1.0d0-x2sum
c  outer (pressure) loop has converged when the x2's sum to one, i.e.,
c  when the fugacities of each component in each phase are equal
c       write (*,*) ' SATT--check conv, p, fp: ',pit(j),fpit(j)
        call DPDT (t,rho2,x2,dpt2)
        if (dpt2.le.0.d0) goto 810  !dp/dT should never be negative
        if (ABS(fpit(j)).lt.tolr .and. lx2con) then
          ierr=0
          herr=' '
          goto 850
        else
c  provided that the inner loop has converged, update positive and
c  negative bounds on pressure for possible use in reguli-falsi iteration
          if (lx2con) then
            if (fpit(j).lt.0.0d0) then
              lpneg=.true.
              pneg=pit(j)
              fpneg=fpit(j)
            else
              lppos=.true.
              ppos=pit(j)
              fppos=fpit(j)
            end if
          end if
        end if
c
c  compute new guess for saturation pressure
c
        if (j.eq.1) then
c  for first iteration, new pressure is ratio of old
          if (x2sum.gt.5) x2sum=5
          j=2
          if (kph.eq.1) then
c  bubble point
            pit(2)=pit(1)*x2sum
          else
c  dew point
            pit(2)=pit(1)/x2sum
          end if
        else
c  subsequent iterations--use secant method
          if (ABS(fpit(2)-fpit(1)).gt.1.0d-12)
     &    pit(3)=pit(2)-fpit(2)*(pit(2)-pit(1))/(fpit(2)-fpit(1))
c  check that new pressure is not outside bounds, if so use reguli-falsi
          if (lpneg .and. lppos .and. (pit(3).gt.1.001d0*MAX(ppos,pneg)
     &        .or. pit(3).lt.0.990d0*MIN(ppos,pneg))) then
            pit(3)=ppos-fppos*(ppos-pneg)/(fppos-fpneg)
          end if
c  discard oldest iteration
          pit(1)=pit(2)
          pit(2)=pit(3)
          fpit(1)=fpit(2)
        end if
c       write (*,1799) itpp,j,pit(1),pit(2),fpit(2)
c1799   format (1x,' SATT--itpp,j,p1,p2,fp2:  ',2i4,3e14.6)
 400    continue
c  outer iteration loop has not converged
 810    continue
        ierr=128
        write (herr,1128) t,hnull
 1128   format ('[SATT error 128] ',
     &          'iteration for saturation state did not converge; T =',
     &          g12.5,' K.',a1)
        call ERRMSG (ierr,herr)
        p=pc
        rho1=rhoc
        rho2=rhoc
c
c  end of outer (pressure) iteration loop
c
 850    continue
c
c  assign final compositions and densities for parent and incipient
c  phases (x and x2, rho1 and rho2, respectively) to outputs
c
        if (kph.eq.1) then
c  bubble point
          rhol=rho1
          rhov=rho2
          do i=1,nc
            xliq(i)=x(i)
            xvap(i)=x2(i)
          enddo
        else
c  dew point
          rhol=rho2
          rhov=rho1
          do i=1,nc
            xliq(i)=x2(i)
            xvap(i)=x(i)
          enddo
        end if
c  call new routine SATTP if ierr>0 and attempt to get convergence
c       if (ierr.gt.0 .and. icomp.eq.0 .and. iSatFlg.eq.0) then
c         call SATTP(t,p,x,kph,0,d,rhol,rhov,xliq,xvap,q,ierr,herr)
c       endif
        if (rhov.gt.rhol*0.99d0) then
          if (ABS(xliq(1)-xvap(1)).lt.0.1d0) then
            p=pc
            rhov=rhoc
            rhol=rhoc
            if (t.lt.tc) then
              ierr=128
              write (herr,1128) t,hnull
              call ERRMSG (ierr,herr)
            else
              ierr=121
              write (herr,1121) t,tc,hnull
              call ERRMSG (ierr,herr)
            endif
          endif
        endif
c  end of mixture iteration
      end if
c
c  save results
 900  continue
      if (ierr.eq.0) then
        if (p.lt.0.00001d0 .and. icomp.ne.0) p=rhov*r*t
        tsavt=t
        psavt=p
        icsavt=icomp
        kphsvt=kph
        dlsavt=rhol
        dvsavt=rhov
        if (icomp.eq.0) then
          do i=1,nc
            xsavt(i)=x(i)
            xlsavt(i)=xliq(i)
            xvsavt(i)=xvap(i)
          enddo
        else
          do i=1,nc
            xsavt(icomp)=0.d0
            xlsavt(icomp)=0.d0
            xvsavt(icomp)=0.d0
          enddo
          xsavt(icomp)=1.d0
          xlsavt(icomp)=1.d0
          xvsavt(icomp)=1.d0
        endif
        lsatt=.true.
      endif
      if (iaga.eq.1) then
        heos='AGA'
        if (ierr.eq.0) call TPRHO (t,p,xvap,2,1,rhov,ierr,herr)
      endif
      RETURN
c
      end                                               !subroutine SATT
c
c ======================================================================
c
      subroutine SATTEST (t,x,kph,p,x2,ierr,herr)
c
c  estimate initial values for saturation states given temperature
c  and the composition of one phase
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac] (phase specified by kph)
c      kph--phase flag: 1 = input x is liquid composition (bubble point)
c                       2 = input x is vapor composition (dew point)
c  outputs:
c        p--estimated pressure [kPa]
c       x2--estimated composition of unknown phase [array of mol frac]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  08-06-09 EWL, original version, taken from code in SATT
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATTEST
c
      include 'commons.for'
      dimension x(ncmax),x2(ncmax),pcomp(ncmax)
      character*255 herr
c
c  generate initial guesses for mixture pressure and compositions
c  using Raoult's law
c
c  estimates for vapor pressures of pure components
      ierr=0
      herr=' '
      do i=1,nc
        pcomp(i)=pcrit(i)
     &        *10.d0**(-2.333333d0*(1.d0+accen(i))*(tcrit(i)/t-1.d0))
      enddo
c
      p=0.d0
      if (kph.eq.1) then
c  bubble point
        do i=1,nc
          p=p+x(i)*pcomp(i)
        enddo
        do i=1,nc
          x2(i)=x(i)*pcomp(i)/p
        enddo
      else
c  dew point
        ysum=0.d0
        do i=1,nc
          ysum=ysum+x(i)/pcomp(i)
        enddo
        do i=1,nc
          x2(i)=x(i)/pcomp(i)/ysum
          p=p+x2(i)*pcomp(i)
        enddo
      end if
      RETURN
c
      end                                            !subroutine SATTEST
c
c ======================================================================
c
      subroutine SATP (p,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
c
c  iterate for saturated liquid and vapor states given pressure
c  and the composition of one phase
c
c  inputs:
c        p--pressure [kPa]
c        x--composition [array of mol frac] (phase specified by kph)
c      kph--phase flag:  1 = input x is liquid composition
c                        2 = input x is vapor composition
c                        3 = input x is liquid composition (freezing point)
c                        4 = input x is vapor composition (sublimation point)
c
c  outputs:
c        t--temperature [K]
c     rhol--molar density [mol/L] of saturated liquid
c     rhov--molar density [mol/L] of saturated vapor
c           For a pseudo pure fluid, the density of the equilibrium phase
c           is not returned.  Call SATP twice, once with kph=1 to get
c           tliq and rhol, and once with kph=2 to get tvap and rhov.
c     xliq--liquid phase composition [array of mol frac]
c     xvap--vapor phase composition [array of mol frac]
c     ierr--error flag:  0 = successful
c                        2 = P < Ptp
c                        4 = P < 0
c                        8 = x out of range
c                       12 = P and x out of range
c                      140 = CRITP did not converge
c                      141 = P > Pcrit
c                      142 = TPRHO-liquid did not converge (pure fluid)
c                      143 = TPRHO-vapor did not converge (pure fluid)
c                      144 = pure fluid iteration did not converge
c           following 3 error codes are advisory--iteration will either
c           converge on later guess or error out (ierr = 148)
c                     -144 = Raoult's law (mixture initial guess) did
c                            not converge
c                     -145 = TPRHO did not converge for parent ph (mix)
c                     -146 = TPRHO did not converge for incipient (mix)
c                     -147 = composition iteration did not converge
c                      148 = mixture iteration did not converge
c     herr--error string if ierr<>0 (character*255)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  07-13-95  MM, original version
c  09-11-95  MM, add error string to argument list
c  09-25-95  MM, rearrange argument list (outputs in order t, rho, x)
c  10-11-95  MM, RETURN if any error detected
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-11-95  MM, Raoult's law as first guess for mixture
c  12-18-95  MM, fill xliq, xvap with zeros for undefined components
c  12-27-95  MM, add full mixture iteration using fugacity, based on SATT
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-19-96  MM, add dipole moment to /CCON/
c  04-05-96  MM, test for supercritical '.ge. pc' rather than '.gt. pc'
c  11-14-96  MM, adjust initial guesses, tratio, etc to get closer to critical
c  02-12-96 EWL, special initial guess for temperature near the critical point
c  07-15-97  MM, add errors/warnings to parallel SATT
c  10-01-97  MM, add compiler switch to allow access by DLL
c  11-13-97 EWL, initialize ft(j); potential bomb if no value when writing error message
c  12-05-97  MM, check that TPRHO gives density within bounds
c                if Raoult's law iteration D.N.C., revert to initial guess
c  02-10-98  MM, add reguli-falsi, quadratic interpolation and bisection
c                to Raoult's law iteration
c  04-06-98 EWL, call subroutine AG to get Gibbs energy
c  07-30-98  MM, check for f1=0 in phase 2 iteration; separate kguess for each phase
c  11-30-98 EWL, change kguess to kgues2 in call to TPRHO in vapor search.
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-22-98 EWL, zero out all elements of xliq and xvap
c  12-22-98 EWL, use pure fluid algorithm when called with a mixture but x(i)=1
c  12-23-98 EWL, modify step size for T when TPRHO fails
c  12-23-98 EWL, replace rhov with rhoc/2 when TPRHO finds liquid root
c  08-03-99 EWL, if TPRHO fails, set x2 to original values before restarting
c  08-03-99 EWL, if dpdrho>1d6, then return an error message
c  01-11-00 EWL, remove ierr from lines where 'i3' was not in format statement
c  02-23-00 EWL, add kph=3,4 as inputs and call melting or sublimation lines
c  12-17-01 EWL, call SATT if iteration fails
c  05-28-02 EWL, check for bad root in two phase
c  08-14-02 EWL, add logic if ammonia/water in use
c  09-19-02 EWL, add checks for ancillary routines and exit for pseudo-pures
c  03-24-05 EWL, add check for small rho in liquid phase. (for mixtures, inner loop)
c  09-21-06 EWL, increase itmax, check for ierr=-147, add small value to keep x2sum<>1,
c                add check for nh3+h2o, split tratio into trtio1 and trtio2
c  03-07-07 EWL, add check for very low pcomp(i)
c  02-18-08 EWL, add check for icomp<>old icomp (icsav)
c  04-22-10 EWL, rename variables in PSTSAV
c  11-29-12 EWL, in inner loop, if density from TPRHO is out range, exit after 10 iterations
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATP
c
      include 'commons.for'
      character*255 herr,herr2,herr3
      dimension x(ncmax),xliq(ncmax),xvap(ncmax),x2org(ncmax)
      dimension x2(ncmax),f1(ncmax),f2(ncmax),x2new(ncmax)
      dimension tk(4),ft(3)                  !used for T iteration
c
      iaga=0
      call ISPURE (x,icomp)
      ierr=0
      rho1=0
      rho2=0
      herr=' '
      herr2=hnull
      if (lsatp) then
        if (ABS(p-psavp).lt.1.d-9 .and. kph.eq.kphsvp
     &          .and. p.gt.1.d-7) then
          lsame=.true.
          if (icomp.ne.icsavp) lsame=.false.
          if (icomp.eq.0) then
            do i=1,nc
              if (ABS(x(i)-xsavp(i)).gt.1.0d-9) lsame=.false.
            enddo
          endif
          if (lsame) then
            t=tsavp
            rhol=dlsavp
            rhov=dvsavp
            do i=1,nc
              xliq(i)=xlsavp(i)
              xvap(i)=xvsavp(i)
            enddo
            RETURN
          endif
        endif
      endif
c  set tolerance and maximum number of iterations
      tolr=1.0d-6
      itmax=100
      ft(1)=0.0d0  !initialize to avoid potential problem
      ft(2)=0.0d0  !when writing error message to GUI
c
c  initialize outputs in event of failure of routines
      t=300.0d0
      rhol=0.0d0
      rhov=0.0d0
      if (icomp.eq.0) then
        do i=1,nc
          xliq(i)=x(i)
          xvap(i)=x(i)
        enddo
      else
        do i=1,nc
          xliq(i)=0.d0
          xvap(i)=0.d0
        enddo
        xliq(icomp)=1.d0
        xvap(icomp)=1.d0
      endif
c
c  check if melting or sublimation line requested and call appropriate routines.
      if (kph.eq.3) then      !liquid/solid
        call MELTP (p,x,t,ierr2,herr2)
        rhol=dtpn(1)
        if (t.gt.0.d0) call TPRHO (t,p,x,1,1,rhol,ierr,herr)
        if (ierr.eq.0) then
          ierr=ierr2
          herr=herr2
        endif
        goto 900
      else if (kph.eq.4) then      !vapor/solid
        call SUBLP (p,x,t,ierr2,herr2)
        if (t.gt.0.d0) then
          rhov=p/(R*t)
          if (rhov.lt.1.d10) call TPRHO (t,p,x,2,1,rhov,ierr,herr)
        endif
        if (ierr.eq.0) then
          ierr=ierr2
          herr=herr2
        endif
        goto 900
      end if
c
      call CRITP (x,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
c  error condition--set outputs, issue warning, and return
        ierr=140
        write (herr,1140) herr2(1:237),hnull
 1140   format ('[SATP error 140] ',a237,a1)
        call ERRMSG (ierr,herr2)
        RETURN
      end if
c
c  check that input conditions (in this case p and x) are within limits
c
      Ddum=0.0d0
      tdum=0.8d0*tc
      call LIMITX ('EOS',tdum,Ddum,p,x,tmin,tmax,rhomax,pmax,ierr,herr2)
c     write (*,*) ' SATP--density limit:  ',rhomax
      if (ierr.gt.1) then     !ignore ierr = 1 (t out of range)
c  p and/or x are out of bounds, set error flag and return
        t=0.8d0*tc
        write (herr,1000) ierr,herr2(1:238),hnull
 1000   format ('[SATP error',i3,'] ',a238,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      if (icomp.ne.0) then
        if (p.lt.ptpn(icomp)) then
          call SUBLP (p,x,t,ierr,herr)        !vapor/solid
          if (t.gt.0.d0) then
            rhov=p/(R*t)
            if (rhov.lt.1.d10) call TPRHO (t,p,x,2,1,rhov,ierr3,herr3)
            goto 900
          endif
          ierr=2
          t=ttpn(icomp)
          write (herr,1149) p/1000.0d0,ptpn(icomp)/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1149     format ('[SATP error 2] ',
     &            'pressure less than triple point pressure; P =',
     &          g12.5,' MPa, Ptp =',g12.5,' MPa.',a1)
          RETURN
        endif
      endif
c
      if (p/pc.ge.0.99999999d0 .and. icomp.ne.0) then
c  input pressure is equal to or greater than critical point value,
c  return critical parameters for output temperature and densities
c  output compositions initialized above
        t=tc
        rhol=rhoc
        rhov=rhoc
        if (p/pc.gt.1.000001d0) then
c  supercritical pressure as input, set error flag and return
c  critical parameters for output pressure and densities
          ierr=141
          write (herr,1141) p/1000.0d0,pc/1000.0d0,hnull
          call ERRMSG (ierr,herr)
 1141     format ('[SATP error 141] ',
     &            'pressure input to saturation routine is ',
     &            'greater than critical pressure; P =',g12.5,
     &            ' MPa, Pcrit =',g12.5,' MPa.',a1)
        end if
        RETURN
      end if
c
      if (heos.eq.'AGA') then
        heos='HMX'
        iaga=1
      endif
C
      if (icomp.eq.0 .and. iSatFlg.eq.0) then
        iGue=0
        call SATTP (t,p,x,kph+2,iGue,d,rhol,rhov,xliq,xvap,q,ierr,herr)
        if (ierr.eq.0) goto 900
      endif
c
      if (icomp.ne.0) then
c
c  pure fluid iteration
        do i=1,nc
          xliq(i)=0.d0
          xvap(i)=0.d0
        enddo
        xliq(icomp)=1.d0
        xvap(icomp)=1.d0
c
c  Iterate for saturated liquid and vapor states given pressure using
c  a simple successive substitution method.  The independent variable
c  in the iteration is the temperature.  The convergence criteria is
c  equality of Gibbs free energy in both phases.
c
c  generate initial guess using acentric factor
c
        t=tc/(1.0-0.428571*log10(p/pc)/(1.0+accen(icomp)))
c       write (*,*) 'SATP:  T(0): ',t
c
c  assume nothing about densities on initial calls to TPRHO
        kguess=0
        if (p.gt.0.98*pc) then
c  for near-critical states, generate initial guesses for density;
c  using correlation developed by E.W. Lemmon, NIST
          theta=(1-p/pc)*(1.6d0-accen(icomp))
          t=tc*(1.0d0-0.103947d0*theta-4.108265d-2*theta**2)
          theta=(1-p/pc)**(1.0d0/3.0d0)*(3.0d0+accen(icomp))
          rhov=rhoc*(1.0d0-0.290039d0*theta-7.120197d-3*theta**2)
          rhol=rhoc*(1.0d0+0.298544d0*theta+1.870808d-2*theta**2)
          kguess = 1
        endif
        if (hpsk(icomp).ne.' ' .and. hpsk(icomp).ne.'NBS') then
          call TSATP (p,x,tt,ierr,herr)
          if (tt.gt.0.) t=tt
        endif
        if (hplk(icomp).ne.' ' .and. hplk(icomp).ne.'NBS'
     &                         .and. kph.eq.1) then
          call TSATPL (p,x,tt,ierr,herr)!Get liquid pressure for mixtures
          if (tt.gt.0.) t=tt
        endif
        if (hdlk(icomp).ne.' ' .and. hdlk(icomp).ne.'NBS') then
          kguess=1
          call DLSATK (icomp,t,rhol,ierr,herr)
        endif
        if (hdvk(icomp).ne.' ' .and. hdvk(icomp).ne.'NBS') then
          kguess=1
          call DVSATK (icomp,t,rhov,ierr,herr)
        endif
        stp=1.0002d0
        if (ianc(icomp).eq.1 .and. kph.eq.2) then
          call TPRHO (t,p,x,2,kguess,rhov,ierr,herr) !find vapor density
          rhol=rhov   !Don't return liquid density for equilibrium phase
          goto 900    !For pseudo-pure fluid, stop here
        elseif (ianc(icomp).eq.1) then
          call TPRHO (t,p,x,1,kguess,rhol,ierr,herr)!find liquid density
          rhov=rhol   !Don't return vapor density for equilibrium phase
          goto 900    !For pseudo-pure fluid, stop here
        endif
        do 200 it=1,itmax
        call TPRHO (t,p,x,1,kguess,rhol,ierr,herr2) !find liquid density
        if (ierr.ne.0 .or. rhol.lt.rhoc) then
          t=t/stp
          stp=1.0d0+(stp-1.0d0)/1.2d0
          goto 200
c         herr=' ERROR from SATP:  '//herr2
c         call ERRMSG (ierr,herr)
c         RETURN
        end if
        call TPRHO (t,p,x,2,kguess,rhov,ierr,herr2)  !find vapor density
        if (ierr.ne.0 .or. rhov.gt.rhoc) then
          t=t*1.001d0
          if (t.gt.tc) t=t/1.001d0*1.00005d0
          if (rhov.gt.rhoc) rhov=rhoc/2.0d0
          goto 200
c         herr=' ERROR from SATP:  '//herr2
c         call ERRMSG (ierr,herr)
c         RETURN
        end if
c  use previous densities as initial guesses for calls to TPRHO after
c  first iteration
        kguess=1
c       call GIBBS (t,rhol,x,Aliq,Gliq)
c       call GIBBS (t,rhov,x,Avap,Gvap)
        call AG (t,rhol,x,Aliq,Gliq)
        call AG (t,rhov,x,Avap,Gvap)
        call ENTRO (t,rhol,x,sliq)
        call ENTRO (t,rhov,x,svap)
        ZG=Gliq-Gvap
c       write (*,1014) it,p,rhol,rhov,t,Gliq,Gvap,ZG
c1014   format (1x,'SATP:',i4,e14.6,2f12.8,f10.5,2f12.4,e14.6)
c
c  check convergence
c
        delt=ZG/(sliq-svap)
        if (abs(delt).lt.tolr) then
c  pure component iteration is done (make use of current delt)
          t=t+delt
          call TPRHO (t,p,x,1,kguess,rhol,ierr,herr2) !find liq density
          if (ierr.ne.0 .or. t.gt.tc .or. rhol.lt.rhoc) then
            ierr=142
            write (herr,1142) it,herr2(1:142),hnull
 1142       format ('[SATP error 142] liquid density iteration in ',
     &            'saturation routine did not converge for temperature',
     &            ' iteration',i3,'; ',a142,a1)
            call ERRMSG (ierr,herr)
c  return critical parameters if not converged and very close to Pc
            if (p.gt.0.9999*pc) then
              t=tc
              rhol=rhoc
              rhov=rhoc
            end if
            goto 300
c           RETURN
          end if
          call TPRHO (t,p,x,2,kguess,rhov,ierr,herr2) !find vap density
          if (ierr.ne.0 .or. p.gt.pc .or. rhov.gt.rhoc) then
            ierr=143
            write (herr,1143) it,herr2(1:143),hnull
 1143       format ('[SATP error 143] vapor density iteration in ',
     &            'saturation routine did not converge for temperature',
     &            ' iteration',i3,'; ',a143,a1)
            call ERRMSG (ierr,herr)
c  return critical parameters if not converged and very close to Pc
            if (p.gt.0.9999*pc) then
              t=tc
              rhol=rhoc
              rhov=rhoc
            end if
            goto 300
c           RETURN
          end if
c  !debug--next four lines for debug only
c         call GIBBS (t,rhol,x,Aliq,Gliq)
c         call GIBBS (t,rhov,x,Avap,Gvap)
c         ZG=Gliq-Gvap
c         write (*,1015) it,p,rhol,rhov,t,Gliq,Gvap,ZG
c1015   format (1x,'SATP:',i4,e14.6,2f12.8,f10.5,2f12.4,e14.6)
          ierr=0
          herr=' '
          goto 900         !normal termination for pure fluid
        end if
c
c  continue iteration, define next guess (check that delt
c  will not result in too large a change in temperature)
        if (delt.gt.0.5*t) then
          do j=1,100
c           write (*,1198) t,delt
c1198       format (1x,'% SATP advisory; delt > 0.5*t; t,delt:',2f12.6)
            delt=0.25*delt
            if (delt.lt.0.5*t) goto 110
          enddo
        end if
 110    continue
        t=t+delt
c
 200    continue
c
c  In case of failure, try calling SATT iteratively to find the saturated
c  temperature.  This takes advantage of the alternate method in SATT used
c  at temperatures very close to the critical point.
 300    continue
        i=0
        t1=tc*.999d0
        call SATT (t1,x,kph,p1,rhol,rhov,xliq,xvap,ierr,herr)
        t2=tc*.9995d0
 310    continue
        call SATT (t2,x,kph,p2,rhol,rhov,xliq,xvap,ierr,herr)
        t=t2
        if (ABS(p2-p).lt.tolr .and. ierr.eq.0) goto 900  !Convergence
        i=i+1
        if (ABS(p2-p1).lt.1.d-12) goto 320
        if (i.gt.20) goto 320
        t=t1-(p1-p)/(p2-p1)*(t2-t1)
        if (t.gt.tc .and. t2.gt.t1) t=(t2+tc)/2.d0
        if (t.gt.tc .and. t1.gt.t2) t=(t1+tc)/2.d0
        t1=t2
        p1=p2
        t2=t
        goto 310
c
c  iteration has not converged
 320    continue
        ierr=144
        write (herr,1144) p/1000.0d0,hnull
 1144   format ('[SATP error 144] ',
     &          'iteration for saturation state did not converge; ',
     &          'P =',g12.5,' MPa.',a1)
        call ERRMSG (ierr,herr)
c  return critical parameters if not converged and very close to Tc
        if (p.gt.0.9999*pc) then
          t=tc
          rhol=rhoc
          rhov=rhoc
        end if
        goto 900
c
c  end of pure fluid iteration
c
      else
c
c  begin mixture iteration
c
c  Iterate for the temperature and the composition of the incipient
c  phase (vapor phase for a bubble point calculation, liquid for dew
c  point) given pressure and the composition of the parent phase.
c  Iteration is generally based on the algorithm given by Van Ness
c  & Abbott (Classical Thermodynamics of Nonelectrolyte Solutions with
c  Applications to Phase Equilibria, McGraw-Hill, 1982); convergence
c  criteria is the equality of fugacity for each component in both
c  phases.
c
        call SATPEST (p,x,kph,t,x2,ierr,herr)
        j=2
        tk(j)=t
        if (kph.eq.1) then
c  bubble point
c  variable kph2 specifies the state of the incipient phase (x2): 1=liq, 2=vap
c  trtio is temperature multiplier to use when TPRHO does not converge;
c  different values for liquid and vapor phases, such that new guess
c  is further into corresponding single-phase region
          kph2=2
          trtio1=0.995d0
          trtio2=0.998d0
        else
c  dew point
          kph2=1
          trtio1=1.005d0
          trtio2=1.002d0
        end if
c
c  initial temperature (satisfying Raoult's law) has been found,
c  check that this temperature is not above critical
        tmax=0.998d0*tc
        if (tk(j).gt.tmax .or. tk(j).le.0) tk(j)=tmax
c
c  the following line adjusts the initial guess for ammonia/water mixtures
c  allowing the saturation routines to work substantially better, especially in
c  the critical region.  It may work for other mixtures as well.
        if (iamwat.ne.0) tk(j)=tk(j)*1.05d0
c
c  generate initial guesses for densities & incipient phase composition;
c  the do loop allows for the possibility that TPRHO does not converge,
c  it should normally exit with just one pass
c
        do it=1,itmax
          t=tk(j)
c  first guess for densities (separate flags for each phase)
          kgues1=0
          kgues2=0
          call TPRHO (t,p,x,kph,kgues1,rho1,ierr,herr2)     !parent phase
          kgues1=1
          if (ierr.gt.0) then
            kgues1=0             !do not reuse faulty density as guess
            ierr=-145
            write (herr,1145) herr2(1:146),hnull
 1145       format ('[SATP advisory -145] density iteration in ',
     &            'saturation routine did not converge for the parent',
     &            ' phase; ',a146,a1)
            call ERRMSG (ierr,herr)
            if (t.gt.0.8*tc .and. kph.eq.1) then
c   non-convergence probably because too close to critical
              tnew=0.995*t
            else
              tnew=1.005*t
            end if
            tk(j)=tnew
          else
            call TPRHO (t,p,x2,kph2,kgues2,rho2,ierr,herr2) !incipient ph
            kgues2=1
            if (ierr.gt.0) then
              kgues2=0             !do not reuse faulty density as guess
              ierr=-144
              write (herr,1146) herr2(1:146),hnull
 1146         format ('[SATP advisory -146] density iteration in ',
     &              'saturation routine did not converge for the ',
     &              'incipient phase; ',a146,a1)
              call ERRMSG (ierr,herr)
              if (t.gt.0.8*tc .and. kph2.eq.1) then
c   non-convergence probably because too close to critical
                tnew=0.995*t
              else
                tnew=1.005*t
              end if
              tk(j)=tnew
            else
              goto 550   !both parent and incipient phases have converged
            end if
          end if
        enddo

c
c  main outer iteration loop for mixtures
c  loop for temperature, using Raoult's Law result (above) as first guess
c
 550    continue
c
        t=tk(j)
        do i=1,nc
          if (kph.eq.1) then
            xvap(i)=x2(i)
          else
            xliq(i)=x2(i)
          endif
        enddo
c
        x2sum=0.0d0
        ltpos=.false.         !flags for reguli-falsi iteration
        ltneg=.false.
        tneg=0.0d0
        tpos=0.0d0
        ftneg=0.0d0
        ftpos=0.0d0
        do ii=1,nc
          x2org(ii)=x2(ii)
        enddo
c       write (*,*) ' SATP--start main iteration; j,tk(j):  ',j,tk(j)
        tk(1)=tk(j)           !first guess for temperature from above
        j=1                   !reset iteration flag
        kguess=0              !for first calls to TPRHO
        do 400 itt=1,itmax
        if (int(itt/10)*10.eq.itt .and. itt.gt.30) then
          trtio1=trtio1**2
          trtio2=trtio2**2
        endif
        t=tk(j)
        lx2con=.false.        !flag for convergence of inner loop
c  compute density and fugacities for parent phase
c       write (*,*) ' SATP call TPRHO (parent) for it,T = ',itt,t
        call TPRHO (t,p,x,kph,kguess,rho1,ierr,herr2)   !parent phase
        if (itt.ne.1) then
          if (ABS(rho1-rhoc).lt.1 .and. ABS(rho2-rhoc).gt.4) then
c  jump away from critical point when a bad root was found
            rho1=rho1*1.5d0
            call TPRHO (t,p,x,kph,kguess,rho1,ierr,herr2)
          endif
        endif
        kgues1=1
        if (ierr.gt.0) then
          kgues1=0             !do not reuse faulty density as guess
          ierr=-145
          write (herr,1245) itt,herr2(1:146),hnull
 1245     format ('[SATP advisory -145] density iteration in ',
     &            'saturation routine did not converge for temperature',
     &            ' iteration',i3,'; ',a146,a1)
          tk(j)=tk(j)*trtio1  !try another temperature and use up one
          goto 400             !iteration (to prevent infinite loop)
        else if (rho1.gt.1.1d0*rhomax) then
c  density from TPRHO is out of range, reset
          rho1=rhomax
          ierr=-145
          write (herr,1245) itt,herr2(1:146),hnull
        end if
        call FGCTY2 (t,rho1,x,f1,ierr,herr)
c       write (*,1082) itt,t,p,rho1,(x(i),i=1,2),(f1(i),i=1,2)
c1082   format (1x,' SATP--phase 1:  ',i3,f11.5,2e14.6,2e16.8,2e18.10)
c
c  begin inner iteration loop for composition of phase 2
c
        do itx=1,itmax*2
c  compute density and fugacities for phase 2
          call TPRHO (t,p,x2,kph2,kguess,rho2,ierr,herr2) !incipient phase
          kgues2=1
          if (ierr.gt.0 .or. ABS(rho2-rho1).lt.0.01d0 .or.
     &       (rho2.lt..01d0 .and. kph2.eq.1)) then
            kgues2=0             !do not reuse faulty density as guess
            ierr=-146
            write (herr,1246) itx,herr2(1:146),hnull
 1246       format ('[SATP advisory -146] density iteration in ',
     &            'saturation routine did not converge for composition',
     &            ' iteration',i3,'; ',a146,a1)
            do ii=1,nc
              x2(ii)=x2org(ii)
            enddo
            kguess=0
            tk(j)=tk(j)/trtio2   !try another pressure and use up one
            goto 400             !iteration (to prevent infinite loop)
          else if (rho2.gt.1.2*rhomax) then
c  density from TPRHO is out of range, reset
            rho2=rhomax
            ierr=-146
            write (herr,1246) itt,herr2(1:146),hnull
            if (itx.gt.10) goto 700
          end if
          call FGCTY2 (t,rho2,x2,f2,ierr,herr)
c       write (*,1086) itx,rho2,(x2(i),i=1,2),(f2(i),i=1,2)
c1086   format (1x,'       phase 2:  ',i3,25x,e14.6,2e16.8,2e18.10)
c  calculate new x2's by ratio of fugacities; inner loop has converged
c  when x2's change by less than a convergence tolerance
          x2sum=0.0d0
          do i=1,nc
            if (x2(i).gt.0.d0) then
            if (f2(i).gt.1.0d-20 .and. f1(i).gt.0.0d0) then
              x2new(i)=x2(i)*f1(i)/f2(i)
            else
c  in case fugacity is zero; e.g., if x(i)=0 x2(i) does not change
              x2new(i)=x2(i)+0.0001d0 !add a little to keep x2sum<>1
            end if
            x2sum=x2sum+x2new(i)
            endif
          enddo
c  normalize the x2 compositions; this yields next guess for x2 and
c  ensures that the x2 always sum to one
          sumdel=0.0d0
          do i=1,nc
            if (x2(i).gt.0.d0) then
            x2new(i)=x2new(i)/x2sum
            sumdel=sumdel+abs(x2(i)-x2new(i))  !change in compositions
            x2(i)=x2new(i)
            endif
          enddo
          if (sumdel.lt.tolr) then
c  inner iteration loop has converged
            lx2con=.true.
            goto 700
          end if
c  if not, continue inner iteration loop
        enddo
c  inner iteration loop has not converged
        ierr=-147
        write (herr,1147) p/1000.0d0,sumdel,hnull
 1147   format ('[SATP advisory -147] ',
     &          'iteration for composition in saturation routine ',
     &          'did not converge; P =',g12.5,
     &          ' MPa; deltaX =',g12.5,' mol frac.',a1)
        call ERRMSG (ierr,herr)
c
c  end of inner (x2) iteration loop
c
 700    continue
        ft(j)=1.0d0-x2sum
c  outer (temperature) loop has converged when the x2's sum to one, i.e.,
c  when the fugacities of each component in each phase are equal
        if (abs(ft(j)).lt.tolr .and. ierr.ne.-147) then
          goto 850
        else
c  provided that the inner loop has converged, update positive and
c  negative bounds on pressure for possible use in reguli-falsi iteration
          if (lx2con) then
            if (ft(j).lt.0.0d0) then
              ltneg=.true.
              tneg=tk(j)
              ftneg=ft(j)
            else
              ltpos=.true.
              tpos=tk(j)
              ftpos=ft(j)
            end if
          end if
        end if
c
c  compute new guess for saturation temperature
c
        if (j.eq.1) then
c  for first iteration, new temperature is ratio of old
          j=2
          kguess=1    !use previous density as initial guess to TPRHO
c  ratio for next guess of temperature; the 0.10d0 is adjustable
          tnew=1.0d0+0.10d0*(x2sum-1.0d0)
c         write (*,*) ' PSAT--x2sum,tnew:  ',x2sum,tnew
          if (kph.eq.1) then
c  bubble point
            tk(2)=tk(1)/tnew
          else
c  dew point
            tk(2)=tk(1)*tnew
          end if
        else
c  subsequent iterations--use secant method, check for divide by zero
          if (ABS(ft(2)-ft(1)).lt.1.0d-10) then
            tk(3)=0.5*(tk(1)+tk(2))
          else
            tk(3)=tk(2)-ft(2)*(tk(2)-tk(1))/(ft(2)-ft(1))
          end if
c  check that new temperature is not outside bounds, if so use reguli-falsi
          if (ltneg .and. ltpos .and. (tk(3).gt.MAX(tpos,tneg)
     &        .or. tk(3).lt.MIN(tpos,tneg))) then
            tk(3)=tpos-ftpos*(tpos-tneg)/(ftpos-ftneg)
          end if
          if (tk(3).lt.0) tk(3)=tk(2)*.95d0
          if (tk(3).gt.tc*1.5d0) tk(3)=tk(2)*1.05d0
c         write (*,1137) tk(1),tk(2),tk(3),yyyyy,ft(2)
c1137     format (1x,' SATP--tguess_1,2,3; ft_1,2:  ',5e14.6)
c  discard oldest iteration
          tk(1)=tk(2)
          tk(2)=tk(3)
          ft(1)=ft(2)
        end if
 400    continue
c  outer iteration loop has not converged
        ierr=148
        write (herr,1148) p/1000.0d0,hnull
 1148   format ('[SATP error 148] ',
     &          'iteration for saturation state did not converge; P =',
     &          g12.5,' MPa.',a1)
        call ERRMSG (ierr,herr)
c
c  end of outer (temperature) iteration loop
c
 850    continue
c
c  assign final compositions and densities for parent and incipient
c  phases (x and x2, rho1 and rho2, respectively) to outputs
c
        t=tk(j)
        if (kph.eq.1) then
c  bubble point
          rhol=rho1
          rhov=rho2
          do i=1,nc
            xliq(i)=x(i)
            xvap(i)=x2(i)
          enddo
        else
c  dew point
          rhol=rho2
          rhov=rho1
          do i=1,nc
            xliq(i)=x2(i)
            xvap(i)=x(i)
          enddo
        end if
        call DPDD (t,rhol,xliq,dpdrh1)
        call DPDD (t,rhov,xvap,dpdrh2)
        if (dpdrh1.gt.1.0d6 .or. dpdrh2.gt.1.0d6) then
          ierr=148
          write (herr,1148) p/1000.d0,hnull
          call ERRMSG (ierr,herr)
        endif
        if ((rhol.gt.rhoc .and. rhov.gt.rhoc) .or.
     &      (rhol.lt.rhoc .and. rhov.lt.rhoc)) then
c  for some mixtures, both rhol and rhov can be greater than rhoc, so
c  check for p close to pc first before returning error
          if (p.lt.0.95d0*pc .or. icomp.ne.0 .or. kph.eq.2) then
            ierr=148
            write (herr,1148) p/1000.d0,hnull
            call ERRMSG (ierr,herr)
          endif
        endif
c  call new routine SATTP if ierr>0 and attempt to get convergence
c       if (ierr.gt.0 .and. icomp.eq.0 .and. iSatFlg.eq.0) then
c         call SATTP(t,p,x,kph+2,0,d,rhol,rhov,xliq,xvap,q,ierr,herr)
c       endif
c
c  end of mixture iteration
      end if
c
c  save results
 900  continue
      if (ierr.eq.0) then
        tsavp=t
        psavp=p
        icsavp=icomp
        kphsvp=kph
        dlsavp=rhol
        dvsavp=rhov
        if (icomp.eq.0) then
          do i=1,nc
            xsavp(i)=x(i)
            xlsavp(i)=xliq(i)
            xvsavp(i)=xvap(i)
          enddo
        else
          do i=1,nc
            xsavp(i)=0.d0
            xlsavp(i)=0.d0
            xvsavp(i)=0.d0
          enddo
          xsavp(icomp)=1.d0
          xlsavp(icomp)=1.d0
          xvsavp(icomp)=1.d0
        endif
        lsatp=.true.
      endif
      if (iaga.eq.1) then
        heos='AGA'
        if (ierr.eq.0) call TPRHO (t,p,xvap,2,1,rhov,ierr,herr)
      endif
      RETURN
c
      end                                               !subroutine SATP
c
c ======================================================================
c
      subroutine SATPEST (p,x,kph,t,x2,ierr,herr)
c
c  estimate initial values for saturation states given pressure
c  and the composition of one phase
c
c  inputs:
c        p--pressure [kPa]
c        x--composition [array of mol frac] (phase specified by kph)
c      kph--phase flag: 1 = input x is liquid composition (bubble point)
c                       2 = input x is vapor composition (dew point)
c  outputs:
c        t--estimated temperature [K]
c       x2--estimated composition of unknown phase [array of mol frac]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  08-06-09 EWL, original version, taken from code in SATP
c  08-06-09 EWL, remove the check on pcomp(i)<10.  The impact of this may not be good for all situations.
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATPEST
c
      include 'commons.for'
      dimension x(ncmax),x2(ncmax),pcomp(ncmax)
      dimension tk(4),ft(3)                  !used for T iteration
      character*255 herr
c
      ltpos=.false. !flags for reguli-falsi iteration
      ltneg=.false.
      tneg=0.d0
      tpos=0.d0
      ftneg=0.d0
      ftpos=0.d0
      tolr=1.0d-6
      itmax=100
      j=1                     !flag for first iteration
c
c  generate initial guess using Raoult's law,
c  first generate initial guess for temperature by simple ratio of
c  pure component saturation temperatures
      tk(1)=0.d0
      acf=0.d0              !average acentric factor
      do i=1,nc
        ti=tcrit(i)/(1.0-0.428571*log10(p/pcrit(i))/(1.0+accen(i)))
c         write (*,*) ' SATP:  tsat-guess for component ',i,ti
        tk(1)=tk(1)+x(i)*ti
        acf=acf+x(i)*accen(i)
      enddo
      tzero=tk(1)  !save in case Raoult iteration does not converge
c
c  secant method iteration to find t which satisfies Raoult's law
c
      do it=1,itmax
c  approximate pure component vapor pressures with acentric factor
      do i=1,nc
        pxp=-2.333333*(1.0+accen(i))*(tcrit(i)/tk(j)-1.0)
        if (ABS(pxp).lt.50) pcomp(i)=pcrit(i)*10.0**pxp
c       if (pcomp(i).lt.10.) pcomp(i)=10.d0
      enddo
c
      if (kph.eq.1) then
c  bubble point
        psum=0.d0
        do i=1,nc
          psum=psum+x(i)*pcomp(i)
        enddo
        ft(j)=1.d0-p/psum
        if (abs(ft(j)).lt.tolr) goto 500  !iteration has converged
c  update + & - bounds on temperature for possible use in reguli-falsi
        if (ft(j).lt.0.d0) then
          ltneg=.true.
          tneg=tk(j)
          ftneg=ft(j)
        else
          ltpos=.true.
          tpos=tk(j)
          ftpos=ft(j)
        end if
c  generate next guess
        if (it.eq.1) then
          tratio=1.d0-0.42857d0*log10(psum/p)/(1.d0+acf)
          tk(2)=tk(1)*tratio
          j=2
        else if (it.ge.itmax/3 .and. ltpos .and. ltneg) then
c  if iteration has not converged after many iterations, use bisection
c  (provided that guesses bounding the root are available)
          tk(2)=0.5d0*(tpos+tneg)
          tk(1)=tk(2)
          ft(1)=ft(j)
          j=2
        else
c  use secant method
          if (ABS(ft(2)-ft(1)).gt.1.0d-10) then
            tk(3)=tk(2)-ft(2)*(tk(2)-tk(1))/(ft(2)-ft(1))
          else
            tk(3)=0.5d0*(tk(1)+tk(2))
          end if
c  check that new temperature is not outside bounds, if so use reguli-falsi
          if (ltneg .and. ltpos .and. (tk(j+1).gt.MAX(tpos,tneg)
     &        .or. tk(j+1).lt.MIN(tpos,tneg))) then
            tk(j+1)=tpos-ftpos*(tpos-tneg)/(ftpos-ftneg)
          end if
          tk(1)=tk(2)
          tk(2)=tk(3)
          ft(1)=ft(2)
        end if
      else
c  dew point
        xdamp=1.d0     !damping ratio for secant method
        ypsum=0.d0
        do i=1,nc
          ypsum=ypsum+x(i)*p/pcomp(i)
          ft(j)=1.0-ypsum
        enddo
        if (ABS(ft(j)).lt.1.0d3*tolr) goto 500  !iteration has converged
c  update + & - bounds on pressure for possible use in reguli-falsi
        if (ft(j).lt.0.d0) then
          ltneg=.true.
          tneg=tk(j)
          ftneg=ft(j)
        else
          ltpos=.true.
          tpos=tk(j)
          ftpos=ft(j)
        end if
        if (it.eq.1) then
          tratio=1.d0/(1.0-0.42857*log10(ypsum)/(1.d0+acf))
          tk(2)=tk(1)*tratio
          j=2
        else if (it.eq.2) then
c  secant method for 2nd guess
            if (ABS(ft(2)-ft(1)).gt.1.0d-10) then
              tk(3)=tk(2)-xdamp*ft(2)*(tk(2)-tk(1))/(ft(2)-ft(1))
            else
              tk(3)=0.5d0*(tk(1)+tk(2))
            end if
c  check that new temperature is not outside bounds, if so use reguli-falsi
            if (ltneg .and. ltpos .and. (tk(j+1).gt.MAX(tpos,tneg)
     &          .or. tk(j+1).lt.MIN(tpos,tneg))) then
              tk(j+1)=tpos-ftpos*(tpos-tneg)/(ftpos-ftneg)
            end if
            j=3
        else if (it.le.itmax/3 .or. .not.(ltpos.and.ltneg)) then
c  2nd order secant (inverse quadratic interpolation) for subsequent guesses
c  see Numerical Recipes, p 252
          rr=ft(3)/ft(2)  !these are the R,S,T used in Num. Rec.
          rs=ft(3)/ft(1)
          rt=ft(1)/ft(2)
          rrst=(rt-1.d0)*(rr-1.d0)*(rs-1.d0)
          if (abs(rrst).gt.1.d-20) then
            tk(4)=tk(3)
     &       +rs*(rt*(rr-rt)*(tk(2)-tk(3))-(1.d0-rr)*(tk(3)-tk(1)))/rrst
          else
            tk(j+1)=-1.d0    !Fix so that reguli-falsi will kick in
          endif
c  check that new temperature is not outside bounds, if so use reguli-falsi
          if (ltneg .and. ltpos .and. (tk(j+1).gt.MAX(tpos,tneg)
     &        .or. tk(j+1).lt.MIN(tpos,tneg))) then
            tk(j+1)=tpos-ftpos*(tpos-tneg)/(ftpos-ftneg)
          end if
c  discard oldest iteration
          tk(1)=tk(2)
          tk(2)=tk(3)
          tk(3)=tk(4)
          ft(1)=ft(2)
          ft(2)=ft(3)
        else
c  if iteration has not converged by now, use bisection
          tk(2)=0.5d0*(tpos+tneg)
          tk(1)=tk(2)
          ft(1)=ft(j)
          j=2
        end if
      end if
c       write (*,1006) it,j,tk(j-1),tk(j),ft(j-1)
c1006   format (1x,' SATP Raoult''s: it,j,t1,t2,ft:',2i4,2f12.4,e16.7)
      enddo                                     !next trial for t
c
c  iteration has not converged, issue warning and proceed
      ierr=-144
      herr='[SATP advisory -144] Raoult''s law iteration (to '//
     &     'generate mixture initial guess) has not converged.'//
     &     hnull
      call ERRMSG (ierr,herr)
      if (ABS(ft(j)).lt.1.0d4*tolr .or. ABS(tk(j)-tk(j-1)).lt.1.d0) then
      else
c  if current guess is not even close go back to initial guess
        tk(j)=tzero
      end if
c
 500  continue

      psum=0.d0
      if (kph.eq.1) then
c  bubble point
        do i=1,nc
          psum=psum+x(i)*pcomp(i)
        enddo
        do i=1,nc
          x2(i)=x(i)*pcomp(i)/psum
        enddo
      else
c  dew point
        ysum=0.d0
        do i=1,nc
          ysum=ysum+x(i)/pcomp(i)
        enddo
        do i=1,nc
          x2(i)=x(i)/pcomp(i)/ysum
          psum=psum+x2(i)*pcomp(i)
        enddo
      end if
      t=tk(j)
      RETURN
c
      end                                            !subroutine SATPEST
c
c ======================================================================
c
      subroutine SATD (rho,x,kph,kr,t,p,rhol,rhov,xliq,xvap,ierr,herr)
c
c  iterate for temperature and pressure given a density along the
c  saturation boundary and the composition
c
c  inputs:
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c      kph--flag specifying desired root for multi-valued inputs
c           has meaning only for water at temperatures close to its triple point
c          -1 = return middle root (between 0 and 4 C)
c           1 = return highest temperature root (above 4 C)
c           3 = return lowest temperature root (along freezing line)
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c     rhol--molar density [mol/L] of saturated liquid
c     rhov--molar density [mol/L] of saturated vapor
c     xliq--liquid phase composition [array of mol frac]
c     xvap--vapor phase composition [array of mol frac]
c       kr--phase flag: 1 = input state is liquid
c                       2 = input state is vapor in equilibrium with liq
c                       3 = input state is liquid in equilibrium with solid
c                       4 = input state is vapor in equilibrium with solid
c     ierr--error flag:   0 = successful
c                         2 = D > Dmax
c                         8 = x out of range
c                        10 = D and x out of range
c                       160 = CRITP did not converge
c                       161 = SATD did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  N.B. kr = 3,4 presently working only for pure components
c
c  either (rhol,xliq) or (rhov,xvap) will correspond to the input state
c  with the other pair corresponding to the other phase in equilibrium
c  with the input state
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  11-22-99  MM, original version
c  02-08-00 EWL, add pure fluid version using Maxwell criteria
c  02-28-00 EWL, add checks for water
c  08-13-02 EWL, add check for ierr<>0 in mixture routine
c  07-28-11 EWL, calculate rhov at Tc in vapor phase for mixtures to get ft(1)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATD
c
      include 'commons.for'
      character*255 herr,herr1
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      dimension tt(3),ft(2)
c
      iaga=0
      call ISPURE (x,icomp)
      ierr=0
      herr=' '
      t0=300.0d0
      p0=100.0d0
      call LIMITX ('EOS',t0,rho,p0,x,tmin,tmax,Dmax,pmax,ierr,herr1)
c  [don't care if t0 or p0 are out of bounds, only rho and x]
      if (ierr.ge.8 .or. rho.gt.Dmax .or. rho.le.0.0d0) then
        write (herr,1001) ierr,herr1(1:235),hnull
 1001   format ('[SATD error',i3,'] ',a235,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      t=tc
      p=pc
      rhol=rhoc
      rhov=rhoc
      if (ierr.gt.0) then
c  error condition--set outputs, issue warning, and return
        ierr=160
        write (herr,1160) herr1(1:237),hnull
 1160   format ('[SATD error 160] ',a237,a1)
        call ERRMSG (ierr,herr1)
        RETURN
      end if
      if (ABS(rhoc-rho).lt.0.001d0 .and. icomp.ne.0) then
        kr=1
        RETURN
      endif
c  calculate density at triple point if not set
      if (icomp.ne.0 .and. dtpv(icomp).lt.1.d-15) THEN
        call SATT (ttpn(icomp),x,2,p1,rhol,rhov,xliq,xvap,ierr,herr)
        if (ierr.gt.0 .and. ttpn(icomp).lt.10.d0) then
          ierr=0   !Ignore for eos testing (ttp<10 K)
          rhov=1.d-10
        endif
        dtpv(icomp)=rhov
      endif
c  check if pure water
      iw=0
      rhow=55.504316185d0  !maximum liquid density from Pruss Eq.
      if (hcas(icomp).eq.'7732-18-5' .and. icomp.ne.0) then
        iw=1
        if (rho.lt.dtpn(icomp) .and. (kph.eq.-1 .or. kph.eq.3)) goto 210
        if (rho.gt.rhow .and. (kph.eq.-1 .or. kph.eq.1)) goto 210
      endif
c  determine region
      if (rho.lt.dtpv(icomp) .and. icomp.ne.0) then
        kr=4                         !vapor/solid
      elseif (rho.lt.rhoc) then
        kr=2                         !vapor
      elseif (iw.eq.1 .and. rho.lt.rhow .and. kph.ne.3) then   !water
        kr=1
      elseif (rho.gt.dtpn(icomp) .and. abs(dtpn(icomp)).gt.1.d-20
     &       .and. icomp.ne.0) then
        kr=3                         !liquid/solid
      else
        kr=1                         !liquid
      end if
c     write (*,*) 'SATD--initial phase:  ',kr
c
      if (heos.eq.'AGA') then
        heos='HMX'
        iaga=1
      endif
c
c  pure fluid VLE iteration
      if (icomp.ne.0 .and. kr.le.2 .and. kph.ne.-1
     &               .and. ianc(icomp).eq.0) then
        tol=1.0d-6
        itmax=20
c  estimate liquid density for any fluid
        if (d72l.lt.1.d-8) then
          t=0.72d0*tc
          call SATT (t,x,1,p,rhol,rhov,xliq,xvap,ierr,herr)
          d72l=rhol
          if (ianc(icomp).eq.1)
     &      call SATT (t,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
          d72v=rhov
        endif
        if (kr.eq.1) then
          d=rho/rhoc+(1.2d0-d72l/rhoc)*(rho-rhoc)/(d72l-rhoc)-1.d0
          t=9.272d0*LOG(d+1.0d0)-5.195d0*d
          t=tc*(1.0d0-t**(1.d0/0.337d0))
        elseif (kr.eq.2) then
          t=tc
          if (d72v.gt.0) then
            dx=LOG(d72v/rhoc)
            del=LOG(rho/rhoc)
            d=(3.5d0+dx)*(del/dx)**1.3d0-del
            t=0.5d0*LOG(d+1.0d0)-0.026d0*d**0.5D0
          endif
          if (t.lt.0) t=0
          t=tc/(1.d0+t**(1.d0/0.37d0))
        endif
c
        d1=0.001d0
        t2=t+0.00001d0
        stp1=1.00001d0
        stp2=1.00001d0
        do it=1,itmax
c  calculate pressure, if less than zero, increase t and try again
          call PRESS (t,rho,x,p)
          if (p.lt.0) then
            t=t*stp1
            stp1=1.0d0+(stp1-1.0d0)*1.5d0
            goto 140
          endif
          iii=iprnterr
          iprnterr=0
          call TPRHO (t,p,x,3-kr,0,rho2,ierr,herr)
          iprnterr=iii
c  calculate pressure, if error, decrease t and try again
          if (ierr.ne.0) then
            t=t/stp2
            stp2=1.0d0+(stp2-1.0d0)*2d0
            goto 140
          endif
          stp1=1.00001d0
          stp2=1.00001d0
c  use Maxwell criterion to generate next guess for t
          if (ABS(rho-rho2).gt.1d-11) then
            if (kr.eq.1) then
              rhol=rho
              rhov=rho2
            else
              rhol=rho2
              rhov=rho
            endif
            call AG (t,rho,x,a,g1)
            call AG (t,rho2,x,a,g2)
            call ENTRO (t,rho,x,s1)
            call ENTRO (t,rho2,x,s2)
            d2=d1
            if (ABS(s1-s2).gt.1d-11) d1=(g1-g2)/(s1-s2)
            if (ABS(d1-d2).gt.1d-11) f=-(t2-t)/(d2-d1)
            t2=t
            if (ABS(d1).le.tol) goto 900
            if (t+f*d1.le.0) then
              t=t*0.95d0
            else
              t=t+f*d1
            endif
          else
            t=t/stp1
          endif
        enddo
 140    continue
c  method failed (generally at very low temperatures in the liquid), try
c  alternate method:
c       write (*,*) 'Maxwell solution failed'
      endif
c
c  iterate for temperature using a combination of Newton's method
c  and reguli-falsi
c
      if (icomp.ne.0) then
        if (hmeltk(icomp).eq.'NBS' .and. rho.gt.dtpn(icomp)) then !no melt line
          ierr=2
          write (herr,1002) ierr,rho,dtpn(icomp),hnull
 1002     format('[SATD error',i3,'] density above triple-point density'
     &          ,'; D =',g12.5,' mol/L; Dtp =',g12.5,' mol/L',a1)
          call ERRMSG (ierr,herr)
          goto 900
        endif
        if (hsublk(icomp).eq.'NBS' .and. rho.lt.dtpv(icomp)) then !no subl line
          ierr=2
          write (herr,1003) ierr,rho,dtpv(icomp),hnull
 1003     format('[SATD error',i3,'] density below triple-point density'
     &          ,'; D =',g12.5,' mol/L; Dtp =',g12.5,' mol/L',a1)
          call ERRMSG (ierr,herr)
          goto 900
        endif
      end if
c
      tol=1.0d-7
      itmax=20
      rhowm=rhol
      if (kr.eq.3) then
        tt(1)=ttpn(icomp)
        rhol=dtpn(icomp)
        if (iw.eq.1) then !check for water
c  calculate p and d at slightly higher than the lowest possible temperature
          tt(1)=251.1650000001d0
          call MLTH2O (tt(1),p,p2)
        else
c  Call melting routine in case dtpn(1) is not exactly eq. to rho(ttrp)
          call MELTT (tt(1),x,p,ierr,herr)
        endif
        call TPRHO (tt(1),p,x,1,1,rhol,ierr,herr)
        ft(1)=log(rho/rhol)
      elseif (kr.eq.4) then
        tt(1)=ttpn(icomp)
        ft(1)=log(rho/dtpv(icomp))
      else
        tt(1)=tc
        ft(1)=log(rho/rhoc)
        if (kr.eq.2 .and. nc.gt.1) then  !For mixtures at Tc in the vapor phase, the value of rhoc is not correct
          call SATT (tc,x,kr,p,rhol,rhov,xliq,xvap,ierr,herr)
          if (ierr.eq.0 .and. rhov.gt.0) ft(1)=log(rho/rhov)
        endif
        if (iw.eq.1) then
          if (rho.ge.dtpn(icomp) .and. kph.eq.-1) then
            tt(1)=ttpn(icomp)
            ft(1)=log(dtpn(icomp)/rhoc)
          endif
        endif
      endif
c
c  generate second guess for temperature
c
      if (kr.eq.1) then
        tt(2)=0.85*tc
        if (iw.eq.1 .and. kph.eq.-1 .and. rho.ge.dtpn(icomp)) tt(2)=275
      else if (kr.eq.2) then
        tt(2)=0.75*tc
      else if (kr.eq.3) then
        tt(2)=0.85*tc
        if (iw.eq.1) tt(2)=273.16d0
      else if (kr.eq.4) then
        tt(2)=0.95*ttpn(icomp)
      end if
c
c  initialize iteration flags
c
      lneg=.false.
      lpos=.false.
      ltp=.false.      !flag indicating if SATT called at triple point
      tneg=0.0d0
      tpos=0.0d0
c  store variables for reguli-falsi
      if (ft(1).lt.0.0) then
        lneg=.true.
        tneg=tt(1)
      else
        lpos=.true.
        tpos=tt(1)
      end if
      jt=2
c
      do 200 it=1,itmax
      ierr=0
      if (kr.eq.1) then           !liquid
        call SATT (tt(jt),x,1,p,rhol,rhov,xliq,xvap,ierr,herr)
        if (rhol.gt.0.0d0) ft(jt)=log(rho/rhol)
      else if (kr.eq.2 .or. kr.eq.4) then      !vapor
        call SATT (tt(jt),x,kr,p,rhol,rhov,xliq,xvap,ierr,herr)
        if (rhov.gt.0) ft(jt)=log(rho/rhov)
      else if (kr.eq.3) then      !liquid/solid
        if (iw.eq.1) then !check for water
          call MLTH2O (tt(jt),p,p2)
          if (rho.lt.rhowm) p=p2
        else
          call MELTT (tt(jt),x,p,ierr,herr)
        endif
        kguess=1
        rhol=dtpn(icomp)
        call TPRHO (tt(jt),p,x,1,kguess,rhol,ierr,herr)
        if (rhol.gt.0) ft(jt)=log(rho/rhol)
      end if
      if (ierr.ne.0) then
        tt(jt)=0.999d0*tt(jt)
        goto 200
      endif
c     write (*,1999) it,jt,tt(jt),ft(jt)
c1999 format (1x,'% it,jt,t,ft: ',2i4,f9.4,f16.10)
c  check for convergence
      t=tt(jt)
      if (abs(ft(jt)).lt.tol) goto 900
c  store variables for reguli-falsi
      if (ft(jt).lt.0.0) then
        lneg=.true.
        tneg=tt(jt)
      else
        lpos=.true.
        tpos=tt(jt)
      end if
c  compute next guess for temperature
      if (jt.le.1) then
        jt=2
      else
        if (abs(ft(2)-ft(1)).gt.1.d-20)
     &  tt(3)=tt(2)-ft(2)*(tt(2)-tt(1))/(ft(2)-ft(1))
c  check if new guess is outside of bounds of previous guesses,
c  if so (and if upper and lower bounds are available) use
c  reguli-falsi
        if (lpos .and. lneg .and.
     &     (tt(3).gt.max(tpos,tneg) .or. tt(3).lt.min(tpos,tneg)))then
           tt(3)=0.5*(tpos+tneg)
c  check against triple point
        elseif (tt(3).lt.ttpn(icomp).and.icomp.ne.0 .and. iw.eq.0) then
          if (.not.ltp) then
            ltp=.true.
            tt(3)=ttpn(icomp)
          else if (tt(3).le.0.0d0) then
            tt(3)=0.5d0*tt(2)
          end if
        end if
        tt(1)=tt(2)
        tt(2)=tt(3)
        ft(1)=ft(2)
      end if
 200  continue
 210  continue
      p=pc
      t=tc
      rhol=rhoc
      rhov=rhoc
      ierr=161
      write (herr,1161) rho,hnull
 1161 format ('[SATD error 161] ',
     &        'iteration for saturation state given density did not ',
     &        'converge for D =',g12.5,' mol/L',a1)
      call ERRMSG (ierr,herr)
 900  continue
      if (iaga.eq.1) then
        heos='AGA'
        if (ierr.eq.0) call TPRHO (t,p,xvap,2,1,rhov,ierr,herr)
      endif
      RETURN
c
      end                                               !subroutine SATD
c
c ======================================================================
c
      subroutine SATH (h,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,ierr,herr)
c
c  iterate for temperature, pressure, and density given enthalpy along
c  the saturation boundary and the composition
c
c  inputs:
c        h--molar enthalpy [J/mol]
c        x--composition [array of mol frac]
c      kph--flag specifying desired root
c           0 = return all roots along the liquid-vapor line
c           1 = return only liquid VLE root
c           2 = return only vapor VLE roots
c           3 = return liquid SLE root (melting line)
c           4 = return vapor SVE root (sublimation line)
c  outputs:
c    nroot--number of roots.  Value is set to one for kph=1,3,4 if ierr=0
c       k1--phase of first root (1-liquid, 2-vapor, 3-melt, 4-subl)
c       t1--temperature of first root [K]
c       p1--pressure of first root [kPa]
c       d1--molar density of first root [mol/L]
c       k2--phase of second root (1-liquid, 2-vapor, 3-melt, 4-subl)
c       t2--temperature of second root [K]
c       p2--pressure of second root [kPa]
c       d2--molar density of second root [mol/L]
c     ierr--error flag:   0 = successful
c                         2 = h < hmin
c                         4 = h > hmax
c                         8 = h > htrp (for subl input)
c                       160 = CRITP did not converge
c                       161 = SATH did not converge for one root
c                       162 = SATH did not converge for both roots
c     herr--error string (character*255 variable if ierr<>0)
c
c  The second root is always set as the root in the vapor at temperatures
c  below the maximum enthalpy on the vapor saturation line.  If kph is
c  set to 2, and only one root is found in the vapor (this occurs when h<hcrit)
c  the state point will be placed in k2,t2,p2,d2.  If kph=0 and this situation
c  occurred, the first root (k1,t1,p1,d1) would be in the liquid (k1=1, k2=2).
c
c  N.B. kph = 3,4 presently working only for pure components
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-22-00 EWL, original version
c  05-08-08 EWL, add iteration count to avoid endless loop
c  09-04-08 EWL, add check for water and adjust tmin to 273.16 K
c  01-04-11 EWL, add txmax=thmax to set txmax if hmax has already been found
c  03-07-11 EWL, increase tol when not converging (such as CO2 very near critical)
c  03-15-11 EWL, add check for h1>h2>h3 when determining hmax
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATH
c
      include 'commons.for'
      character*255 herr,herr1
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      dimension tt(3),ft(2)
c
      call ISPURE (x,icomp)
      ierr=0
      ierr1=0
      iflag=0
      herr=' '
      k1=0
      t1=0
      p1=0
      d1=0
      k2=0
      t2=0
      p2=0
      d2=0
      nroot=0
      call MAXT (x,tmaxt,tmaxp,tmaxd,ierr,herr1)
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      if (ierr.gt.0) then
c  error condition--set outputs, issue warning, and return
        ierr=160
        write (herr,1160) herr1(1:237),hnull
 1160   format ('[SATH error 160] ',a237,a1)
        call ERRMSG (ierr,herr1)
        RETURN
      end if
c
c  calculate enthalpy at triple point if not set
      call LIMITS ('EOS',x,tmin,tmax,Dmax,pmax)
      if (ABS(htpl).lt.1.d-15) THEN
c  check for water
        if (hcas(icomp).eq.'7732-18-5' .and. icomp.ne.0) tmin=273.16d0
        t=tmin
        call SATT (t,x,1,p,rhol,rhov,xliq,xvap,ierr,herr)
        call ENTHAL (t,rhol,x,htpl)
        call ENERGY (t,rhol,x,etpl)
        if (icomp.eq.0 .or. ianc(icomp).eq.1) then
 100      continue
          call SATT (t,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
          if (ierr.gt.0 .and. t.lt.tc) then
            t=t+5
            goto 100
          endif
        endif
        call ENTHAL (t,rhov,x,htpv)
        call ENERGY (t,rhov,x,etpv)
      endif
      xtpl=htpl
      xtpv=htpv
      call ENTHAL (tc,rhoc,x,xc)
      if (ieflg.eq.1) then
        xtpl=etpl
        xtpv=etpv
        call ENERGY (tc,rhoc,x,xc)
      endif
c
c  find maximum enthalpy (or energy) along the saturated vapor dome
      xmax=hmax
      if (ieflg.eq.1) xmax=emax
      if (ABS(xmax).lt.1.d-15) THEN
        call EHMAXX (x,ieflg,xmax,tmax,ierr,herr)
      endif
      txmax=thmax
      if (ieflg.eq.1) txmax=temax
c
c  determine region
      if (kph.eq.3 .and. icomp.ne.0) then
        kphs=3                         !liquid/solid
      else if (kph.eq.4 .and. icomp.ne.0) then
        kphs=4                         !vapor/solid
      else
        if (h.lt.xc) then
          kphs=1                         !liquid
        else
          kphs=2                         !vapor
        end if
      endif
c     write (*,*) 'SATH--initial phase:  ',kphs
c
      if (h.gt.xtpv .and. kphs.eq.4) then
        ierr=8
        write (herr,1008) ierr,h,xtpv,hnull
 1008   format('[SATH error',i3,'] enthalpy greater than triple point'
     &     ,' value; h =',g12.5,' J/mol, max =',g12.5,' J/mol',a1)
        call ERRMSG (ierr,herr)
        RETURN
      endif
      if (h.lt.xtpl .and. kphs.ne.4) then
        ierr=2
        write (herr,1002) ierr,h,xtpl,hnull
 1002   format('[SATH error',i3,'] enthalpy below triple-point '
     &      ,'value; h =',g12.5,' J/mol, min =',g12.5,' J/mol',a1)
        call ERRMSG (ierr,herr)
        RETURN
      endif
      if (h.gt.xmax .and. kphs.ne.3) then
        ierr=4
        write (herr,1004) ierr,h,xmax,hnull
 1004   format('[SATH error',i3,'] enthalpy greater than maximum '
     &      ,'value; h =',g12.5,' J/mol, max =',g12.5,' J/mol',a1)
        call ERRMSG (ierr,herr)
        RETURN
      endif
c
c  iterate for temperature using a combination of Newton's method
c  and reguli-falsi
c
      tol=1.0d-6
      itmax=20
      if (kphs.eq.3) then
        tt(1)=tmin
        ft(1)=xtpl-h
      elseif (kphs.eq.4) then
        tt(1)=tmin
        ft(1)=xtpv-h
      else
        tt(1)=tc
        ft(1)=xc-h
c  Use values from Splines if set up by call to SATSPLN, should return the root with the higher temperature
        if (iSpline.ne.0 .and. ieflg.eq.0) then
          d=rhoc
          call SPLNROOT (nc+4,0,h,d,ierr,herr)
          call SPLNVAL (nc+1,0,d,t,ierr,herr)
          iflsh=1
          if (d.lt.rhoc) iflsh=2
          if (d.lt.rhoc .and. d.gt.tmaxd .and. tmaxd.ne.0) iflsh=-2
          call SATTP (t,p,x,iflsh,0,d,rhol,rhov,xliq,xvap,q,ierr,herr)
          call ENTHAL (t,d,x,hc)
          tt(1)=t
          ft(1)=hc-h
          if (iflsh.lt.0) kphs=iflsh
        endif
      endif
c
c  initialize iteration flags
c
 130  continue
      lneg=.false.
      lpos=.false.
      tpos=0
      tneg=0
c  store variables for reguli-falsi
      if (ft(1).lt.0.0) then
        lneg=.true.
        tneg=tt(1)
        if (abs(kphs).eq.2) then
          lpos=.true.
          tpos=txmax
        endif
      else
        lpos=.true.
        tpos=tt(1)
        if (abs(kphs).eq.2) then
          lneg=.true.
          tneg=txmax
        endif
      end if
c
c  generate second guess for temperature
c
      if (iSpline.ne.0) then
        tt(2)=tt(1)-0.1d0
      elseif (kphs.eq.1) then
        tt(2)=0.95*tc
      else if (abs(kphs).eq.2) then
        tt(2)=(tneg+tpos)/2.0d0
      else if (kphs.eq.3) then
        tt(2)=0.85*tc
      else if (kphs.eq.4) then
        tt(2)=0.95*tmin
      end if
      jt=2
c
      do it=1,itmax
      call SATT (tt(jt),x,kphs,p,rho,rhov,xliq,xvap,ierr,herr)
      if (abs(kphs).eq.2 .or. kphs.eq.4) rho=rhov
      call ENTHAL (tt(jt),rho,x,h1)
      if (ieflg.eq.1) call ENERGY (tt(jt),rho,x,h1)
      ft(jt)=h1-h
c     write (*,1999) it,jt,tt(jt),ft(jt)
c1999 format (1x,'% it,jt,t,ft: ',2i4,f9.4,f16.10)
c  check for convergence
      if (it.eq.10) tol=tol*10
      if (it.eq.15) tol=tol*10
      if (it.eq.18) tol=tol*10
      if (abs(ft(jt)).lt.tol) goto 160
c  store variables for reguli-falsi
      if (ft(jt).lt.0.0) then
        lneg=.true.
        tneg=tt(jt)
      else
        lpos=.true.
        tpos=tt(jt)
      end if
c  compute next guess for temperature
      if (abs(ft(2)-ft(1)).gt.1.d-20)
     &  tt(3)=tt(2)-ft(2)*(tt(2)-tt(1))/(ft(2)-ft(1))
c  check if new guess is outside of bounds of previous guesses,
c  if so (and if upper and lower bounds are available) use
c  reguli-falsi
      if (lpos .and. lneg .and.
     &  (tt(3).gt.max(tpos,tneg) .or. tt(3).lt.min(tpos,tneg))) then
        tt(3)=0.5*(tpos+tneg)
      elseif (tt(3).lt.tmin .and. kphs.ne.4) then
        tt(3)=tmin
      elseif (tt(3).le.tmin/10.0d0 .and. icomp.ne.0) then !subl check
        if (it.eq.1 .and. kphs.eq.4) then
          tt(3)=0.d0
        else
          goto 150
        endif
      endif
      tt(1)=tt(2)
      tt(2)=tt(3)
      ft(1)=ft(2)
      enddo
 150  continue
      if (ierr1.ne.161) then
        ierr=161
        ierr1=161
        write (herr,1161) h,nroot+1,hnull
 1161   format ('[SATH error 161] ',
     &         'iteration for saturation state did not ',
     &         'converge for h =',g12.5,' J/mol; root = ',i1,a1)
        herr1=herr
      else
        ierr=162
        write (herr,1162) h,hnull
 1162   format ('[SATH error 162] ',
     &         'iteration for saturation state did not ',
     &         'converge for both roots; h =',g12.5,' J/mol',a1)
      endif
      call ERRMSG (ierr,herr)
      tt(jt)=0
      p=0
      rho=0
 160  continue
      t=tt(jt)
      if (abs(kphs).eq.kph .or. kph.eq.0) then
        nroot=nroot+1
        if (iflag.eq.0) then
c  first root found, save values
          t1=t
          p1=p
          d1=rho
          k1=abs(kphs)
        else
c  second root found, save values
          t2=t
          p2=p
          d2=rho
          k2=abs(kphs)
          if (ierr.eq.0 .and. ierr1.ne.0) then
            ierr=ierr1
            herr=herr1
          endif
        endif
      endif
c  check if second root might exist
      if (h.gt.xtpv .and. h.lt.xmax .and. iflag.eq.0) then
c  only calculate second root if requested
        if (kph.eq.0 .or. kph.eq.2) then
          tt(1)=tmin
          ft(1)=xtpv-h
          kphs=2
          iflag=1
          if (iSpline.ne.0 .and. ieflg.eq.0) then
            d=0.01d0
            call SPLNROOT (nc+4,0,h,d,ierr,herr)
            call SPLNVAL (nc+1,0,d,t,ierr,herr)
            t=t+0.05d0  !Decrease slightly to bound root
            call SATTP (t,p,x,kphs,0,d,rhol,rhov,xliq,xvap,q,ierr,herr)
            call ENTHAL (t,d,x,hc)
            if (ieflg.eq.1) call ENERGY (t,d,x,hc)
            tt(1)=t
            ft(1)=hc-h
          endif
          goto 130
        endif
      endif
      RETURN
c
      end                                               !subroutine SATH
c
c ======================================================================
c
      subroutine MAXWELL (icomp,ts,ps,dl,dv,ierr,herr)
c
c  iterate for saturated liquid and vapor states of a pure fluid given temperature
c
c  inputs:
c    icomp--number of component
c       ts--temperature [K]
c       dl--initial guess for molar density [mol/L] of saturated liquid
c       dv--initial guess molar density [mol/L] of saturated vapor
c
c  outputs:
c       ps--saturation pressure [kPa]
c       dl--molar density [mol/L] of saturated liquid
c       dv--molar density [mol/L] of saturated vapor
c
c     ierr--error flag:   0 = successful
c                         1 = no convergence after kmax iterations
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Matthias Kunick, NIST Thermophysics Division, Boulder, Colorado
c  09-15-11  MK, original version
c
      include 'commons.for'
      dimension x(ncmax)
      character herr*255
      parameter (eps=1.d-4)
      parameter (kmax=50)
c
      ierr=0
      herr=' '
      dlsp=-1.d0
      dvsp=-1.d0
c
      if (tminfeq(icomp).lt.ttpn(icomp)) then
        tt=ttpn(icomp)
      else
        tt=tminfeq(icomp)
      endif
      tc  =tcfeq(icomp)
      rhoc=rhocfeq(icomp)
      qt=(ts-tt)/(tc-tt)
      vc=1.d0/rhoc
      dtl=dtpn(icomp)
      dtg=ptpn(icomp)/r/ttpn(icomp)
      vtl=1.d0/dtl*0.95d0   !<= vtr'
      vtg=1.d0/dtg*1.2d0    !>= vtr"
c
c --- start values for v1 and v2 -------------------------------------------
      v1=1.d0/dl
      v2=1.d0/dv
      v1ini=v1
      v2ini=v2
      if (v1ini.lt.vtl) vtl=v1ini*0.95D0
      if (v2ini.gt.vtg) vtg=v2ini*1.05D0
      k=0
c
c *** iteration algorithm ******************************************************
      do
        k=k+1
        dl=1.d0/v1
        dv=1.d0/v2
        call PRESS (ts,dl,x,p1)
        call AG    (ts,dl,x,a1,g)
        call DPDD  (ts,dl,x,alpha1)
        call PRESS (ts,dv,x,p2)
        call AG    (ts,dv,x,a2,g)
        call DPDD  (ts,dv,x,alpha2)
        alpha1=-alpha1*dl*dl
        alpha2=-alpha2*dv*dv
c
        is=0
        ierr1=0
        if (alpha1.gt.0.d0 .and. dlsp.lt.0.d0)
     &     call LIQSPNDL (t,x,dlsp,ierr1,herr)
        do while (alpha1.gt.0.d0) !Maxwell loop (liquid)
          is=is+1
          if (is.gt.10) goto 999
          if (ierr1.eq.0) then
            v1=1.d0/dlsp*0.995d0
          else
            v1=(vtl+v1)*0.5d0
          endif
          dl=1.d0/v1
          call DPDD (ts,dl,x,alpha1)
          alpha1=-alpha1*dl*dl
          if (alpha1.lt.0.d0) then
            call PRESS (ts,dl,x,p1)
            call AG (ts,dl,x,a1,g)
          end if
        end do
        is=0
        if (alpha2.gt.0.d0 .and. dvsp.lt.0.d0)
     &     call VAPSPNDL (t,x,dvsp,ierr1,herr)
        do while (alpha2.gt.0.d0) !Maxwell loop (vapor)
          is=is+1
          if (ierr1.eq.0) then
            v2=1.d0/dvsp*1.005d0
          else
            if (qt.gt.0.9d0) then
              v2=v2*1.005d0
              if (v2.gt.v2ini) v2=v2ini
            else
              v2=v2*1.2d0
            endif
            if (is.gt.10) v2=v2ini
          endif
          if (is.gt.20) goto 999
          dv=1.d0/v2
          call DPDD (ts,dv,x,alpha2)
          alpha2=-alpha2*dv*dv
          if (alpha2.lt.0.d0) then
            call PRESS (ts,dv,x,p2)
            call AG (ts,dv,x,a2,g)
          end if
        end do
c
        vdif=v2-v1
        pm=(a1-a2)/vdif
        if (dabs(vdif).lt.1.d-12 .or. k.gt.kmax) goto 999
        if (dabs(p1-p2)/p2.lt.eps) goto 998
c
        a=0.5d0*alpha1*(alpha1-alpha2)
        b=alpha1*(p1-p2-alpha2*(v1-v2))
        c=alpha2*(v1-v2)*(pm-p1)+0.5d0*(p1-p2)*(p1-p2)
        e=0.25d0*b*b/(a*a)-c/a
        if (e.le.1.d-10) then
          es=e*1.d12
          delv1=-0.5d0*b/a+
     &           dsign(1.d-6*dsqrt(dabs(es)),(alpha1-alpha2)/alpha2)
        else
          delv1=-0.5d0*b/a+dsign(dsqrt(dabs(e)),(alpha1-alpha2)/alpha2)
        end if
        delv2=(p1-p2+alpha1*delv1)/alpha2
c
c --- calculate new values for v1 and v2 -----------------------------------
        v1=v1+delv1
        v2=v2+delv2
        if (v1.gt.vc)  v1=0.9999d0*vc
        if (v1.lt.vtl) v1=vtl
        if (v2.lt.vc)  v2=1.0001d0*vc
        if (v2.ge.vtg) v2=vtg
        if (qt.lt.0.9d0 .and. v1.gt.v1ini*1.2d0) v1=(vtl+v1ini)*0.5d0
      end do

 998  ps=pm
      RETURN

 999  continue
      ps=-1.d0
      dl=-1.d0
      dv=-1.d0
      ierr=1
      write (herr,1)
    1 format('[MAXWELL error] no convergence')
c
      end                                            !subroutine MAXWELL
c
c ======================================================================
c
      subroutine EHMAXX (x,ieflag,ehmax,tehmax,ierr,herr)
c
c  find maximum enthalpy or energy on saturated vapor line.
c
c  inputs:
c        x--composition [array of mol frac]
c    ieflag--set to 0 for enthalpy, 1 for energy
c  outputs:
c    ehmax--maximum enthalpy or energy [J/mol]
c   tehmax--temperature at maximum enthalpy or energy [K]
c     ierr--error flag:   0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  06-15-11 EWL, original version, remove piece from SATH and place here
c  11-27-12 EWL, use values from Splines if available
c
      include 'commons.for'
      character*255 herr,herr1
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      call MAXT (x,tmaxt,tmaxp,tmaxd,ierr,herr1)
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      call LIMITS ('EOS',x,tmin,tmax,Dmax,pmax)

c  Use values from Splines if set up by call to SATSPLN
      tinc=tc/10.d0
      if (iSpline.ne.0) then
        d=rhoc
        call SPLNROOT (nc+4,1,0.d0,d,ierr,herr)
        call SPLNVAL (nc+1,0,d,tt1,ierr,herr)
        tinc=1.d0
        tt1=tt1+tinc
      else
        tt1=tc-tinc
      endif
      ifirst=1
  10  continue
c  set up initial three points
      call SATT (tt1,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
      call ENTHAL (tt1,rhov,x,h1)
      if (ieflag.eq.1) call ENERGY (tt1,rhov,x,h1)
      tt2=tt1-tinc
      call SATT (tt2,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
      call ENTHAL (tt2,rhov,x,h2)
      if (ieflag.eq.1) call ENERGY (tt2,rhov,x,h2)
      tt3=tt2-tinc
      call SATT (tt3,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
      call ENTHAL (tt3,rhov,x,h3)
      if (ieflag.eq.1) call ENERGY (tt3,rhov,x,h3)
      it=0
      if (h1.gt.h2 .and. h2.gt.h3 .and. ifirst.eq.1) then
        tt1=tc-tc/100.d0
        ifirst=2
        goto 10
      endif
c  use quadratic solution to find next guess for Tmax
 140  continue
      it=it+1
      b1=(h2-h1)/(tt2-tt1)
      b2=((h3-h2)/(tt3-tt2)-(h2-h1)/(tt2-tt1))/(tt3-tt1)
      b3=b1-b2*tt1-b2*tt2
      tehmax=-b3/2.0d0/b2
      if (nc.eq.1 .or. tmaxd.eq.0.d0) then
        if (tehmax.gt.tc) tehmax=(tc+tt1)/2.0d0
      else
        if (tehmax.gt.tmaxt) tehmax=(tmaxt+tt1)/2.0d0
      endif
      if (tehmax.lt.tmin) tehmax=(tmin+tt1)/2.0d0
      call SATT (tehmax,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
      if (ieflag.ne.1) then
        call ENTHAL (tehmax,rhov,x,ehmax)
        thmax=tehmax
        hmax=ehmax
      else
        call ENERGY (tehmax,rhov,x,ehmax)
        temax=tehmax
        emax=ehmax
      endif
c  discard a point and load in the new one
      if (tehmax.lt.tt3) then
        tt1=tt2
        tt2=tt3
        tt3=tehmax
        h1=h2
        h2=h3
        h3=ehmax
      elseif (tehmax.gt.tt1) then
        tt3=tt2
        tt2=tt1
        tt1=tehmax
        h3=h2
        h2=h1
        h1=ehmax
      elseif (tehmax.gt.tt2) then
        tt1=tehmax
        h1=ehmax
      else
        tt3=tehmax
        h3=ehmax
      endif
      hdiff=hold-ehmax
      hold=ehmax
      if (ABS(hdiff).gt.1.d-9 .and. it.lt.50) goto 140  !check for convergence
      RETURN
      end                                               !subroutine HMAX
c
c ======================================================================
c
      subroutine SATE (e,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,ierr,herr)
c
c  iterate for temperature, pressure, and density given energy along
c  the saturation boundary and the composition
c
c  inputs:
c        e--molar energy [J/mol]
c        x--composition [array of mol frac]
c      kph--flag specifying desired root
c           0 = return all roots along the liquid-vapor line
c           1 = return only liquid VLE root
c           2 = return only vapor VLE roots
c           3 = return liquid SLE root (melting line)
c           4 = return vapor SVE root (sublimation line)
c  outputs:
c    see SATH for description of outputs
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  05-16-05 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATE
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      ieflg=1
      call SATH (e,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,ierr,herr)
      ieflg=0
      if (ierr.ne.0) then
        i=index(herr,'SATH')
        if (i.gt.0) herr=herr(1:i+2)//'E'//herr(i+4:255)
        i=index(herr,'h =')
        if (i.gt.0) herr=herr(1:i-1)//'e'//herr(i+1:255)
        i=index(herr,'enthalpy')
        if (i.gt.0) herr=herr(1:i-1)//'energy'//herr(i+8:255)
      endif
      RETURN
c
      end                                               !subroutine SATE
c
c ======================================================================
c
      subroutine SATS (s,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,
     &                 k3,t3,p3,d3,ierr,herr)
c
c  iterate for temperature, pressure, and density given an entropy along
c  the saturation boundary and the composition
c
c  inputs:
c        s--molar entropy [J/mol-K]
c        x--composition [array of mol frac]
c      kph--flag specifying desired root
c           0 = return all roots along the liquid-vapor line
c           1 = return only liquid VLE root
c           2 = return only vapor VLE roots
c           3 = return liquid SLE root (melting line)
c           4 = return vapor SVE root (sublimation line)
c  outputs:
c    nroot--number of roots.  Set to one for kph=1,3,4 if ierr=0
c       k1--phase of first root (1-liquid, 2-vapor, 3-melt, 4-subl)
c       t1--temperature of first root [K]
c       p1--pressure of first root [kPa]
c       dl--molar density of first root [mol/L]
c       k2--phase of second root (1-liquid, 2-vapor, 3-melt, 4-subl)
c       t2--temperature of second root [K]
c       p2--pressure of second root [kPa]
c       d2--molar density of second root [mol/L]
c       k3--phase of third root (1-liquid, 2-vapor, 3-melt, 4-subl)
c       t3--temperature of third root [K]
c       p3--pressure of third root [kPa]
c       d3--molar density of third root [mol/L]
c     ierr--error flag:   0 = successful
c                         1 = no roots found for specified input phase
c                         2 = s < smin
c                         4 = s > smax
c                         8 = s > strp (for subl input)
c                       160 = CRITP did not converge
c                       161 = SATS did not converge for one root
c                       162 = SATS did not converge for two roots
c                       163 = SATS did not converge for all roots
c     herr--error string (character*255 variable if ierr<>0)
c
c  The second root is always set as the root in the vapor at temperatures
c  below the maximum entropy on the vapor saturation line.  If kph is
c  set to 2, and only one root is found in the vapor (this occurs when s<scrit)
c  the state point will be placed in k2,t2,p2,d2.  If kph=0 and this situation
c  occurred, the first root (k1,t1,p1,d1) would be in the liquid (k1=1, k2=2).
c
c  The third root is the root with the lowest temperature.  For fluids
c  with multiple roots:  When only one root is found in the vapor phase
c  (this happens only at very low temperatures past the region where three
c  roots are located), the value of the root is still placed in
c  k3,t3,p3,d3.  For fluids that never have more than one root (when there
c  is no maximum entropy along the saturated vapor line), the value of the
c  root is always placed in k1,t1,p1,d1.
c
c  N.B. kph = 3,4 presently working only for pure components
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-24-00 EWL, original version
c  04-18-07 EWL, add check for SATT failure on the vapor side when finding stpv
c  03-04-10 EWL, add error message 1001 for root not found in specified phase
c  03-04-10 EWL, check for cases where melting line is not available
c  03-31-11 EWL, add check for p>pmax along melting line
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SATS
c
      include 'commons.for'
      character*255 herr,herr1
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      dimension tt(3),ft(2)
c
      call ISPURE (x,icomp)
      herr=' '
      ierr=0
      ierr1=0
      iflag=0
      nroot=0
      k1=0
      t1=0.d0
      p1=0.d0
      d1=0.d0
      k2=0
      t2=0.d0
      p2=0.d0
      d2=0.d0
      k3=0
      t3=0.d0
      p3=0.d0
      d3=0.d0
      call MAXT (x,tmaxt,tmaxp,tmaxd,ierr,herr1)
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      call ENTRO (tc,rhoc,x,sc)      !entropy at critical point
      if (ierr.gt.0) then
c  error condition--set outputs, issue warning, and return
        ierr=160
        write (herr,1160) herr1(1:237),hnull
 1160   format ('[SATS error 160] ',a237,a1)
        call ERRMSG (ierr,herr1)
        RETURN
      end if
c
c  calculate entropy at triple point if not set
      call LIMITS ('EOS',x,tmin,tmax,Dmax,pmax)
      if (ABS(stpl).lt.1.d-15) THEN
        if (icomp.ne.0) then
          call INFO (icomp,wmm,ttrp,tnbpt,tq,pq,Dq,Zc,ac1,dip,Rg1)
          if (tmin.lt.ttrp) tmin=ttrp  !Check for water
        endif
        t=tmin
        call SATT (t,x,1,p,rhol,rhov,xliq,xvap,ierr,herr)
        call ENTRO (t,rhol,x,stpl)
        if (icomp.eq.0 .or. ianc(icomp).eq.1) then
 100      continue
          call SATT (t,x,2,p,rhol,rhov,xliq,xvap,ierr,herr)
          if (ierr.gt.0 .and. t.lt.tc) then
            t=t+5
            goto 100
          endif
        endif
        call ENTRO (t,rhov,x,stpv)
      endif
c
c  find maximum and minimum entropy along the saturated vapor dome, and
c  the minimum entropy along the melting line
      if (ABS(tsmax).lt.1.d-15) THEN
        call SMAXX (x,smax,tsmax,smin,tsmin,sminm,tsminm,ierr,herr)
      endif
c
c  determine region
      if (kph.eq.3 .and. icomp.ne.0) then
        kphs=3                         !liquid/solid
      else if (kph.eq.4 .and. icomp.ne.0) then
        kphs=4                         !vapor/solid
      else
        if (s.lt.sc) then
          kphs=1                         !liquid
        else
          kphs=2                         !vapor
        end if
      endif
c     write (*,*) 'SATS--initial phase:  ',kphs
c
      if (s.lt.stpv .and. kphs.eq.4) then
        ierr=8
        write (herr,1008) ierr,s,stpv,hnull
 1008   format('[SATS error',i3,'] entropy less than triple point'
     &    ,' entropy; s =',g12.5,' J/mol-K, smin =',g12.5,' J/mol-K',a1)
        call ERRMSG (ierr,herr)
        RETURN
      endif
      if (s.lt.stpl .and. (s.lt.sminm .or. kphs.ne.3)) then
        ierr=2
        smx=stpl
        if (sminm.lt.stpl .and. kphs.eq.3) smx=sminm
        write (herr,1002) ierr,s,smx,hnull
 1002   format('[SATS error',i3,'] entropy below minimum '
     &     ,'entropy; s =',g12.5,' J/mol-K, smin =',g12.5,' J/mol-K',a1)
        call ERRMSG (ierr,herr)
        RETURN
      endif
      if (s.gt.smax .and. s.gt.stpv .and. kphs.le.2) then
        ierr=4
        smx=smax
        if (stpv.gt.smax) smx=stpv
        write (herr,1004) ierr,s,smx,hnull
 1004   format('[SATS error',i3,'] entropy greater than maximum '
     &     ,'entropy; s =',g12.5,' J/mol-K, smax =',g12.5,' J/mol-K',a1)
        call ERRMSG (ierr,herr)
        RETURN
      endif
c
c  iterate for temperature using a combination of Newton's method
c  and reguli-falsi
c
      tol=1.0d-6
      itmax=20
      if (kphs.eq.3) then
        tt(1)=tmin
        ft(1)=stpl-s
        if (tsminm.gt.0) then
          tt(1)=tsminm
          ft(1)=sminm-s
        endif
      elseif (kphs.eq.4) then
        tt(1)=tmin
        ft(1)=stpv-s
      else
        tt(1)=tc
        ft(1)=sc-s
c  Check for situations where three roots exists in the vapor, but the current
c  input is beyond the three root region, and only one root exists.
        if (s.gt.smax .and. tsmax.gt.0) then
          tt(1)=tmin
          ft(1)=stpv-s
          iflag=2
          kphs=2
        endif
c  Use values from Splines if set up by call to SATSPLN, should return the root with the lowest entropy
        if (iSpline.ne.0) then
          d=rhoc
          if (tsmax.gt.0 .and. s.gt.smax) d=0.01d0
          call SPLNROOT (nc+5,0,s,d,ierr,herr)
          call SPLNVAL (nc+1,0,d,t,ierr,herr)
          iflsh=1
          if (d.lt.rhoc) iflsh=2
          if (d.lt.rhoc .and. d.gt.tmaxd .and. tmaxd.ne.0) iflsh=-2
          call SATTP (t,p,x,iflsh,0,d,rhol,rhov,xliq,xvap,q,ierr,herr)
          call ENTRO (t,d,x,sc)
          tt(1)=t
          ft(1)=sc-s
          if (iflsh.lt.0) kphs=iflsh
        endif
      endif
c
c  initialize iteration flags
c
 130  continue
      lneg=.false.
      lpos=.false.
      tpos=0
      tneg=0
c  store variables for reguli-falsi
      if (ft(1).lt.0.0) then
        lneg=.true.
        tneg=tt(1)
        if (abs(kphs).eq.2 .and. tsmax.gt.0) then
          lpos=.true.
          tpos=tsmax
          if (iflag.eq.2) tpos=tsmin
        endif
        if (kphs.eq.3 .and. iflag.eq.1) then
          lpos=.true.
          tpos=tsminm
        endif
      else
        lpos=.true.
        tpos=tt(1)
        if (abs(kphs).eq.2 .and. tsmax.gt.0) then
          lneg=.true.
          tneg=tsmax
          if (iflag.eq.2) tneg=tsmin
        endif
        if (kphs.eq.3 .and. iflag.eq.1) then
          lneg=.true.
          tneg=tsminm
        endif
      end if
c
c  generate second guess for temperature
c
      if (iSpline.ne.0) then
        tt(2)=tt(1)-0.1d0
      elseif (kphs.eq.1) then
        tt(2)=0.95*tc
      else if (abs(kphs).eq.2 .and. tneg.gt.0 .and. tpos.gt.0) then
        tt(2)=(tneg+tpos)/2.0d0
      else if (abs(kphs).eq.2) then
        tt(2)=0.85*tc
      else if (kphs.eq.3 .and. tneg.gt.0 .and. tpos.gt.0) then
        tt(2)=(tneg+tpos)/2.0d0
      else if (kphs.eq.3 .and. tsminm.gt.0) then
        tt(2)=1.05*tsminm
      else if (kphs.eq.3) then
        tt(2)=1.05*tmin
      else if (kphs.eq.4) then
        tt(2)=0.95*tmin
      end if
      jt=2
c
      do it=1,itmax
      if (it.eq.10) tol=tol*10.d0
      if (kphs.gt.0) then
        call SATT (tt(jt),x,kphs,p,rho,rhov,xliq,xvap,ierr,herr)
      else
        call SATTP (tt(jt),p,x,kphs,0,d,rho,rhov,xliq,xvap,q,ierr,herr)
      endif
      if (p.le.0.d0 .and. ierr.eq.0) goto 200
      if (abs(kphs).eq.2 .or. kphs.eq.4) rho=rhov
      call ENTRO (tt(jt),rho,x,s1)
      ft(jt)=s1-s
c     write (*,1999) it,jt,tt(jt),ft(jt)
c1999 format (1x,'% it,jt,t,ft: ',2i4,f9.4,f16.10)
c  check for convergence
      if (abs(ft(jt)).lt.tol) goto 160
c  store variables for reguli-falsi
      if (ft(jt).lt.0.0) then
        lneg=.true.
        tneg=tt(jt)
      else
        lpos=.true.
        tpos=tt(jt)
      end if
c  compute next guess for temperature
      if (abs(ft(2)-ft(1)).gt.1.d-20)
     &  tt(3)=tt(2)-ft(2)*(tt(2)-tt(1))/(ft(2)-ft(1))
c  check if new guess is outside of bounds of previous guesses,
c  if so (and if upper and lower bounds are available) use
c  reguli-falsi

      if (lpos .and. lneg .and. abs(ft(2)).gt.tol*1.d4 .and.
     &  (tt(3).gt.max(tpos,tneg) .or. tt(3).lt.min(tpos,tneg))) then
        tt(3)=0.5*(tpos+tneg)
      elseif (tt(3).lt.tmin .and. kphs.ne.4) then
        tt(3)=tmin
      else if (tt(3).le.tmin/10.0d0 .and. icomp.ne.0) then !subl check
        goto 150
      endif
      tt(1)=tt(2)
      tt(2)=tt(3)
      ft(1)=ft(2)
      enddo
 150  continue
      if (ierr1.ne.161 .and. ierr1.ne.162) then
        ierr=161
        ierr1=161
        write (herr,1161) s,nroot+1,hnull
 1161   format ('[SATS error 161] ',
     &         'iteration for saturation state given entropy did not ',
     &         'converge for s =',g12.5,' J/mol-K; root = ',i1,a1)
        herr1=herr
      elseif (ierr1.ne.162) then
        ierr=162
        write (herr,1162) s,hnull
 1162   format ('[SATS error 162] ',
     &         'iteration for saturation state given entropy did not ',
     &         'converge for two of the roots; s =',g12.5,' J/mol-K',a1)
      else
        ierr=163
        write (herr,1163) s,hnull
 1163   format ('[SATS error 163] ',
     &         'iteration for saturation state given entropy did not ',
     &         'converge for all roots; s =',g12.5,' J/mol-K',a1)
      endif
      call ERRMSG (ierr,herr)
      tt(jt)=0
      p=0
      rho=0
 160  continue
      t=tt(jt)
      if (abs(kphs).eq.kph .or. kph.eq.0) then
        nroot=nroot+1
        if (iflag.eq.0) then
c  first root found, save values
          t1=t
          p1=p
          d1=rho
          k1=abs(kphs)
        elseif (iflag.eq.1) then
c  second root found, save values
          t2=t
          p2=p
          d2=rho
          k2=abs(kphs)
        elseif (iflag.eq.2) then
c  third root found, save values
          t3=t
          p3=p
          d3=rho
          k3=abs(kphs)
        endif
        if (ierr.eq.0 .and. ierr1.ne.0) then
          ierr=ierr1
          herr=herr1
        endif
      endif
c  check if second root might exist
c  only calculate second root if requested
      if ((kph.eq.0 .or. kph.eq.2) .and. tsmax.gt.0) then
        if (s.gt.smin .and. s.lt.smax .and. iflag.eq.0) then
          kphs=2
          iflag=iflag+1
          tt(1)=tsmin
          ft(1)=smin-s
          goto 130
        elseif (s.gt.smin .and. s.lt.smax .and. s.lt.stpv
     &         .and. iflag.eq.1) then
          kphs=2
          iflag=iflag+1
          tt(1)=tmin
          ft(1)=stpv-s
          goto 130
        endif
      endif
      if (kph.eq.3 .and. tsminm.gt.0) then
        if (s.gt.sminm .and. s.lt.stpl .and. iflag.eq.0) then
          iflag=iflag+1
          tt(1)=tmin
          ft(1)=smin-s
          goto 130
        endif
      endif
 200  if (k1.eq.0 .and. k2.eq.0 .and. k3.eq.0 .and. ierr.eq.0) then
        ierr=1
        write (herr,1001) ierr,s,hnull
 1001   format('[SATS error',i3,'] no roots found for specified phase;'
     &    ,' s =',g12.5,' J/mol-K', a1)
        call ERRMSG (ierr,herr)
      endif
c  check for p>pmax along the melting line
      if (kph.eq.3 .and. nroot.eq.1 .and. ierr.eq.0) then
        call LIMITX ('EOS',t1,d1,p1,x,tmin,tmax,rhomax,pmax,ierr,herr)
      endif
      RETURN
c
      end                                               !subroutine SATS
c
c ======================================================================
c
      subroutine SMAXX (x,smx,tsmx,smn,tsmn,smmin,tsmmin,ierr,herr)
c
c  find maximum and minimum entropy along the saturated vapor dome, and
c  the minimum entropy along the melting line
c
c  inputs:
c        x--composition [array of mol frac]
c  outputs:
c      smx--maximum entropy [J/mol-K]
c     tsmx--temperature at maximum entropy [K]
c      smn--minimim entropy [J/mol-K]
c     tsmn--temperature at minimum entropy [K]
c    smmin--minimim entropy along melting line [J/mol-K]
c   tsmmin--temperature at minimum entropy along melting line [K]
c     ierr--error flag:   0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  06-15-11 EWL, original version, remove piece from SATS and place here
c
c
      include 'commons.for'
      character*255 herr,herr1
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      call LIMITS ('EOS',x,tmin,tmax,Dmax,pmax)
      call ENTRO (tc,rhoc,x,sc)      !entropy at critical point
      sold=0.d0
      istart=1
      tsmmin=-1
      smmin=stpl
      tsmn=-1
      smn=sc
      if (stpv.lt.sc) smn=stpv
      tsmx=-1
      smx=stpv
c  check first to see if melting line is double valued
      p=0.d0
      call SATT (tmin,x,3,p,rhol,rhov,xliq,xvap,ierr,herr)
      if (ABS(p).gt.1.d-15) then
        call ENTRO (tmin,rhol,x,s1)
        call SATT (tmin+1.0d0,x,3,p,rhol,rhov,xliq,xvap,ierr,herr)
        call ENTRO (tmin+1.0d0,rhol,x,s2)
        if (s2.lt.s1) istart=0      !melting line is doubled valued if s2<s1
      endif
c  set up initial three points
      j=0
      do i=istart,2
        if (i.eq.1) then
          tt1=tc-tc/20.d0
          tt2=tt1-tc/20.d0
          tt3=tt2-tc/20.d0
        else
          tt3=tmin+tmin/20.d0
          tt2=tt3+tmin/20.d0
          tt1=tt2+tmin/20.d0
        endif
        k=2
        if (i.eq.0) k=3
        call SATT (tt1,x,k,p,rhol,rho,xliq,xvap,ierr,herr)
        if (i.eq.0) rho=rhol
        call ENTRO (tt1,rho,x,s1)
        call SATT (tt2,x,k,p,rhol,rho,xliq,xvap,ierr,herr)
        if (i.eq.0) rho=rhol
        call ENTRO (tt2,rho,x,s2)
        call SATT (tt3,x,k,p,rhol,rho,xliq,xvap,ierr,herr)
        if (i.eq.0) rho=rhol
        call ENTRO (tt3,rho,x,s3)
c  use quadratic solution to find next guess for Tsm
 140    continue
        b1=(s2-s1)/(tt2-tt1)
        b2=((s3-s2)/(tt3-tt2)-(s2-s1)/(tt2-tt1))/(tt3-tt1)
        b3=b1-b2*tt1-b2*tt2
        tsm=-b3/2.0d0/b2
        j=j+1
        if (tsm.lt.tmin .or. j.gt.100) then
          if (i.eq.0) goto 110
          RETURN           !no solution (VLE line is not double valued)
        endif
        if (tsm.gt.tc) tsm=(tc+tt1)/2.0d0
        call SATT (tsm,x,k,p,rhol,rho,xliq,xvap,ierr,herr)
        if (i.eq.0) rho=rhol
        call ENTRO (tsm,rho,x,sm)
c  discard a point and load in the new one
        if (tsm.lt.tt3) then
          tt1=tt2
          tt2=tt3
          tt3=tsm
          s1=s2
          s2=s3
          s3=sm
        elseif (tsm.gt.tt1) then
          tt3=tt2
          tt2=tt1
          tt1=tsm
          s3=s2
          s2=s1
          s1=sm
        elseif (tsm.gt.tt2) then
          tt1=tsm
          s1=sm
        else
          tt3=tsm
          s3=sm
        endif
        sdiff=sold-sm
        sold=sm
        if (ABS(sdiff).gt.1.d-9) goto 140  !check for convergence
c  load appropriate fixed points with the max/min value.
        if (i.eq.1) then
          smx=sm
          tsmx=tsm
        elseif (i.eq.2) then
          smn=sm
          tsmn=tsm
        elseif (i.eq.0) then
          smmin=sm
          tsmmin=tsm
        endif
 110    continue
      enddo
      RETURN
      end                                               !subroutine SMAX
c
c ======================================================================
c
      subroutine CSATK (icomp,t,kph,p,rho,csat,ierr,herr)
c
c  compute the heat capacity along the saturation line as a function of
c  temperature for a given component
c
c  csat can be calculated two different ways:
c     Csat = Cp - T(DvDT)(DPDTsat)
c     Csat = Cp - beta/rho*hvap/(vliq - vvap)
c     where beta is the volume expansivity
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      kph--phase flag: 1 = liquid calculation
c                       2 = vapor calculation
c  outputs:
c        p--saturation pressure [kPa]
c      rho--saturation molar density [mol/L]
c     csat--saturation heat capacity [J/mol-K]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  09-30-98 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CSATK
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
c
      ic2=icnc
      icnc=icomp
      csat=0.d0
      call SATT (t,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
      if (ierr.gt.0 .or. rhol.le.0.d0) then
        icnc=ic2
        if (ierr.eq.0) ierr=1
        p=0.d0
        rho=0.d0
        RETURN
      end if
      rho=rhol
      if (kph.eq.2) rho=rhov
      call DPDT (t,rho,x,dpt)
      call DPDD (t,rho,x,dpdrho)
      call CVCPK (icomp,t,rho,cv,cp)
      call ENTHAL (t,rhol,x,hl)
      call ENTHAL (t,rhov,x,hv)
      beta=dpt/dpdrho/rho
      csat=cp-beta/rho*(hl-hv)/(1.d0/rhol-1.d0/rhov)
      icnc=ic2
c
      RETURN
      end                                              !subroutine CSATK
c
c ======================================================================
c
      subroutine DPTSATK (icomp,t,kph,p,rho,csat,dpt,ierr,herr)
c
c  compute the heat capacity and dP/dT along the saturation line as a
c  function of temperature for a given component.  See also subroutine CSATK.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      kph--phase flag: 1 = liquid calculation
c                       2 = vapor calculation
c  outputs:
c        p--saturation pressure [kPa]
c      rho--saturation molar density [mol/L]
c     csat--saturation heat capacity [J/mol-K] (same as that called from CSATK)
c      dpt--dP/dT along the saturation line [kPa/K]
c           (this is not dP/dT "at" the saturation line for the single phase
c            state, but the change in saturated vapor pressure as the
c            saturation temperature changes.)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  09-25-06 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DPTSATK
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
c
      ic2=icnc
      icnc=icomp
      csat=0.d0
      dpt=0.d0
      call SATT (t,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
      if (ierr.gt.0 .or. rhol.le.0.d0) then
        icnc=ic2
        if (ierr.eq.0) ierr=1
        p=0.d0
        rho=0.d0
        RETURN
      end if
      rho=rhol
      if (kph.eq.2) rho=rhov
      call DPDT (t,rho,x,dpt)
      call DPDD (t,rho,x,dpdrho)
      call CVCPK (icomp,t,rho,cv,cp)
      call ENTHAL (t,rhol,x,hl)
      call ENTHAL (t,rhov,x,hv)
      beta=dpt/dpdrho/rho
      csat=cp-beta/rho*(hl-hv)/(1.d0/rhol-1.d0/rhov)
      dpt=             (hl-hv)/(1.d0/rhol-1.d0/rhov)/T
      icnc=ic2
c
      RETURN
      end                                            !subroutine DPTSATK
c
c ======================================================================
c
      subroutine CV2PK (icomp,t,rho,cv2p,csat,ierr,herr)
c
c  compute the isochoric heat capacity in the two phase (liquid+vapor)
c  region
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--density [mol/l] if known
c           If rho=0, then a saturated liquid state is assumed.
c
c  outputs:
c     cv2p--isochoric two-phase heat capacity [J/mol-K]
c     csat--saturation heat capacity [J/mol-K]
c           (Although there is already a csat routine in REFPROP,
c            it is also returned here.  However, the calculation
c            speed is slower than csat.)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  03-30-05 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CV2PK
c
      include 'commons.for'
      character*255 herr,herr1,herr2
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
c
      ierr=0
      herr=' '
      ic2=icnc
      icnc=icomp
      cv2p=0.d0
      dt=0.01d0
      i=1
      t1=t+dt
      t2=t-dt
      call SATT (t ,x,i,p ,dl ,dv ,xliq,xvap,ierr ,herr)
      call SATT (t1,x,i,p1,dl1,dv1,xliq,xvap,ierr1,herr1)
      call SATT (t2,x,i,p2,dl2,dv2,xliq,xvap,ierr2,herr2)
      if (rho.le.0.d0) rho=dl
      if (ierr.gt.0 .or. ierr1.gt.0 .or. ierr2.gt.0 .or. dl.le.0.d0)then
        if (ierr.eq.0 .and. ierr1.ne.0) then
          ierr=ierr1
          herr=herr1
        endif
        if (ierr.eq.0 .and. ierr2.ne.0) then
          ierr=ierr2
          herr=herr2
        endif
        if (ierr.eq.0 .and. dl.le.0.d0) ierr=1
        icnc=ic2
        RETURN
      endif
c
      call DPDT (t,dl,x,dpt)
      call DPDD (t,dl,x,dpdrho)
      call CVCPK (icomp,t,dl,cv,cp)
      call ENTHAL (t,dl,x,hl)
      call ENTHAL (t,dv,x,hv)
      beta=dpt/dpdrho/dl
      dpdtsat=(hv-hl)/t/(1.d0/dv-1.d0/dl)     ! d(p)/d(T) at sat. liq.
      dddtsat=(dl1-dl2)/2.d0/dt               ! d(rho)/d(T) at sat. liq.
      d2pdtsat=(p2+p1-2.d0*p)/dt**2           ! d^2(p)/d(T)^2 at sat. liq.
      csat=cp-beta/dl*dpdtsat*t
      cv2p=csat+t/dl**2*dddtsat*dpdtsat+t*(1.d0/rho-1.d0/dl)*d2pdtsat
      icnc=ic2
c
      RETURN
      end                                              !subroutine CV2PK
c
c ======================================================================
c
      subroutine TPRHOB (t,p,rho1,rho2,x,rho,ierr,herr)
c
c  iterate for density given temperature, pressure and an upper and lower
c  bound for the density.  This routine is only meant to replace TPRHO
c  in special cases near the critical point.  (And is only used by SATT.)
c
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c     rho1--first bound on density [mol/L]
c     rho2--second bound on density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c      rho--molar density [mol/L]
c     ierr--error flag:   0 = successful
c                         1 = failed to converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  11-08-01 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)

      ierr=0
      herr=' '
      tolr=1.d-9
      d1=rho1
      d2=rho2
      if (d1.gt.d2) then
        d1=rho2
        d2=rho1
      endif
      it=1
      rho=(d1+d2)/2.d0
 140  continue
      call PRESS (t,rho,x,p1)
      call DPDD (t,rho,x,dpd)
      rho0=rho
c  use false position to get next root
      if (abs(dpd).gt.1.d-20) rho=rho+(p-p1)/dpd
c  keep within bounds
      if (rho.lt.d1) rho=(rho0+d1)/2.d0
      if (rho.gt.d2) rho=(rho0+d2)/2.d0
      if (ABS(p1-p).lt.tolr) RETURN
      it=it+1
      if (it.eq.10) tolr=tolr*100
      if (it.eq.15) tolr=tolr*100
      if (it.lt.25) goto 140

      ierr=1
      RETURN
      end                                             !subroutine TPRHOB
c
c ======================================================================
c
      subroutine DLDV (t,p,rhol,rhov,xl,xv,ierr,herr)
      include 'commons.for'
      character*255 herr
      dimension xl(ncmax),xv(ncmax)

      tolr=1.0d-6
      deld=1.d0
      call CRITP (xl,tc,pc,rhoc,ierr,herr)
      rhol=rhoc*1.1d0
      rhov=rhoc
      it=0
 140  continue
      it=it+1

      it2=0
 110  continue
      call DPDD2 (t,rhol,xl,dpdl2)
      call DPDD (t,rhol,xl,dpdl)
      call PRESS (t,rhol,xl,pl)
      if (abs(rhol).gt.1.d6) then
        ierr=1
        herr=' '
        RETURN
      endif
      if (dpdl2.lt.0.d0 .or. dpdl.lt.0.d0 .or. pl.lt.0.d0) then
        rhol=rhol*1.05d0
        it2=it2+1
        if (it2.lt.100) goto 110
      endif
      if (dpdl.gt.1.d4) then
        if (ABS(pl).gt.pc*1.5d0 .and. rhol-rhoc.lt.5) then
          rhol=rhol*1.5d0
          it2=it2+1
          if (it2.lt.100) goto 110
        endif
      endif


      it2=0
 120  continue
      call DPDD2 (t,rhov,xv,dpdv2)
      call DPDD (t,rhov,xv,dpdv)
      call PRESS (t,rhov,xv,pv)
      if (ABS(dpdv).gt.1.d5) then
        if (ABS(pv).gt.pc*1.5d0) then
          rhov=rhov*0.5d0
          it2=it2+1
          if (it2.lt.100) goto 120
        endif
      endif
      if (dpdv2.gt.0.d0 .or. dpdv.lt.0.d0) then
        rhov=rhov*.95d0
        it2=it2+1
        if (it2.lt.100) goto 120
      endif
      if (it.eq.1) then
        rhov=rhov*.95d0
        call DPDD2 (t,rhov,xv,dpdv2)
        call DPDD (t,rhov,xv,dpdv)
        call PRESS (t,rhov,xv,pv)
      endif



      p=(pl+pv)/2.d0

      if (abs(pv-p).gt.1.d-10 .and. abs(dpdv).gt.1.d-10) then
        deld=1.d0/(-dpdv/(pv-p)+dpdv2/2.d0/dpdv)
      else
        deld=deld/2.d0
      endif
 860  continue
      if (ABS(deld/rhov).gt.0.5d0) then
        deld=deld/10.d0
        goto 860
      endif
      rhov=rhov+deld

      if (abs(pl-p).gt.1.d-10 .and. abs(dpdl).gt.1.d-10) then
        deld=1.d0/(-dpdl/(pl-p)+dpdl2/2.d0/dpdl)
      else
        deld=deld/2.d0
      endif
 870  continue
      if (ABS(deld/rhol).gt.1.d0) then
        deld=deld/10.d0
        goto 870
      endif
      rhol=rhol+deld
      if (it.lt.50 .and. ABS(pl-pv).gt.tolr) goto 140

      ierr=0
      herr=' '
      RETURN
      end                                               !subroutine DLDV
c
c ======================================================================
c
      subroutine LIQSPNDL (t,x,rhol,ierr,herr)
c
c  Find the liquid spinodal density for a given temperature.
c  An estimate (rhol) can be provided otherwise it will be calulated from an
c  auxiliary spline function (which was prepared during SETUP).
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c     rhol--density at liquid spinodal [mol/L] (initial guess)
c           for negative inputs the corresponding spline function will be evaluated
c  outputs:
c     rhol--liquid spinodal [mol/L] (initial guess can be provided)
c     ierr--error flag:   0 = successful
c                       124 = failed to converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  11-28-06 EWL, original version taken from code in SATT
c  09-15-11  MK, split subroutine SPNDL into liquid and vapor routines
c
      include 'commons.for'
      character*255 herr
      character*255 herr2
      dimension x(ncmax)
c
      ierr=0
      herr=' '
c
      call CRITP (x,tc,pc,rhoc,ierr,herr)
      call ISPURE (x,icomp)
c
      if (t.gt.tc+1.0d-8 .and. icomp.ne.0) then
        ierr=121
        write (herr,1121) t,tc,hnull
        call ERRMSG (ierr,herr)
 1121   format ('[LIQSPNDL error 121] ',
     &          'temperature input to spinodal routine is ',
     &          'greater than critical temperature; T =',g12.5,
     &          ' K, Tcrit =',g12.5,' K.',a1)
        RETURN
      end if
c
      if (rhol.le.0.d0) then
        call CSPLNVAL (ispndl_l,tspndl_l,dspndl_l,cspndl_l,
     &                        t,rhol,ierr2,herr2)
        if (ierr2.ne.0) rhol=dtpn(icomp)
      endif
c
      it=0
      tolr=1.d-8
c  find the liquid spinodal (point where dpdrho=0)
 310  continue
      call DPDD (t,rhol,x,dpd)
      call DPDD2 (t,rhol,x,dpd2)
      d1=rhol
      dp1=dpd
      if (abs(dpd2).gt.1.d-20) rhol=rhol-dpd/dpd2
      it=it+1
      if (it.eq.10) tolr=tolr*100.d0
      if (it.eq.15) tolr=tolr*100.d0
      if (it.gt.30 .or. rhol.lt.rhoc) then
c  in case of failure, use the false position method:
c  (when the nonanalytical terms are used, this may be caused due to the
c   uncalculated dpdd2 part)
        d2=rhol
        call DPDD (t,d2,x,dp2)
c  check for good bounds; if bad, find new ones:
        if (rhol.lt.rhoc .or. dp1*dp2.ge.0.d0) then
          it=0
          d1=rhoc
          d2=rhoc
          call DPDD (t,d1,x,dp1)
 320      continue
          d2=d2*1.02d0
          call DPDD (t,d2,x,dp2)
          it=it+1
          if (it.gt.100) goto 390
          if (dp1*dp2.gt.0) goto 320
        endif
        it=0
        tolr=1.d-8
 330    continue
        if (abs(dp2-dp1).gt.1.d-20) rhol=d1-dp1*(d2-d1)/(dp2-dp1)
        call DPDD (t,rhol,x,dpd)
        if (dpd*dp1.lt.0.d0) then
          d2=rhol
          dp2=dpd
        else
          d1=rhol
          dp1=dpd
        endif
        it=it+1
        if (it.gt.100) goto 390
        if (abs(dpd).gt.tolr) goto 330
      endif
      if (abs(dpd).gt.tolr) goto 310
      RETURN
C
 390  continue
      ierr=124
      write (herr,1124) t,hnull
      call ERRMSG (ierr,herr)
      rhol=rhoc
 1124 format ('[LIQSPNDL error 124] ',
     &        'iteration for spinodals did not converge; ',
     &        'T =',g12.5,' K.',a1)
      RETURN
      end                                           !subroutine LIQSPNDL
c
c ======================================================================
c
      subroutine VAPSPNDL (t,x,rhov,ierr,herr)
c
c  Find the vapor spinodal density for a given temperature.
c  An estimate (rhov) can be provided otherwise it will be calulated from an
c  auxiliary spline function (which was prepared during SETUP).
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c     rhov--density at vapor spinodal [mol/L] (initial guess)
c           for negative inputs the corresponding spline function will be evaluated
c  outputs:
c     rhov--density at vapor spinodal [mol/L]
c     ierr--error flag:   0 = successful
c                       124 = failed to converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  11-28-06 EWL, original version taken from code in SATT
c  09-15-11  MK, split subroutine SPNDL into liquid and vapor routines
c
      include 'commons.for'
      character*255 herr
      character*255 herr2
      dimension x(ncmax)
c
      ierr=0
      herr=' '
c
      call CRITP (x,tc,pc,rhoc,ierr,herr)
      call ISPURE (x,icomp)
c
      if (t.gt.tc+1.0d-8 .and. icomp.ne.0) then
        ierr=121
        write (herr,1121) t,tc,hnull
        call ERRMSG (ierr,herr)
 1121   format ('[VAPSPNDL error 121] ',
     &          'temperature input to spinodal routine is ',
     &          'greater than critical temperature; T =',g12.5,
     &          ' K, Tcrit =',g12.5,' K.',a1)
        RETURN
      end if
c
      if (rhov.le.0.d0) then
        call CSPLNVAL (ispndl_v,tspndl_v,dspndl_v,cspndl_v,
     &                        t,rhov,ierr2,herr2)
        if (ierr2.ne.0) rhov=0.1d0
      endif
c
      it=0
      tolr=1.d-8
 340  continue
      call DPDD (t,rhov,x,dpd)
      call DPDD2 (t,rhov,x,dpd2)
      d1=rhov
      dp1=dpd
      if (abs(dpd2).gt.1.d-20) rhov=rhov-dpd/dpd2
      it=it+1
      if (it.eq.10) tolr=tolr*100.d0
      if (it.eq.15) tolr=tolr*100.d0
      if (it.gt.30 .or. rhov.gt.rhoc) then
c  false position method:
        d2=rhov
        call DPDD (t,d2,x,dp2)
c  check for good bounds; if bad, find new ones:
        if (d1.gt.rhoc .or. d2.gt.rhoc .or. dp1*dp2.ge.0.d0) then
          it=0
          d1=rhoc
          d2=rhoc
          call DPDD (t,d1,x,dp1)
 350      continue
          d2=d2/1.02d0
          call DPDD (t,d2,x,dp2)
          it=it+1
          if (it.gt.100) goto 390
          if (dp1*dp2.gt.0) goto 350
        endif
        it=0
        tolr=1.d-8
 360    continue
        if (abs(dp2-dp1).gt.1.d-20) rhov=d1-dp1*(d2-d1)/(dp2-dp1)
        call DPDD (t,rhov,x,dpd)
        if (dpd*dp1.lt.0.d0) then
          d2=rhov
          dp2=dpd
        else
          d1=rhov
          dp1=dpd
        endif
        it=it+1
        if (it.gt.100) goto 390
        if (abs(dpd).gt.tolr) goto 360
      endif
      if (abs(dpd).gt.tolr) goto 340
      RETURN
C
 390  continue
      ierr=124
      write (herr,1124) t,hnull
      call ERRMSG (ierr,herr)
      rhov=rhoc
 1124 format ('[VAPSPNDL error 124] ',
     &        'iteration for spinodals did not converge; ',
     &        'T =',g12.5,' K.',a1)
      RETURN
c
      end                                           !subroutine VAPSPNDL
c
c ======================================================================
c
      subroutine SETLSPNDL (icomp,ierr,herr)
c
c  Prepares a cubic spline interpolating the liquid spinodal.
c
c  inputs:
c    icomp--pure fluid number for which spinodals will be calculated
c  outputs:
c     ierr--error flag:   0 = successful
c                         1 = called with a mixture composition (pure fluids only)
c                         2 = convergence porblems in LIQSPNDL
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Matthias Kunick, NIST Thermophysics Division, Boulder, Colorado
c  09-15-11  MK, original version
c
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      parameter (qcritreg=0.85d0)
      parameter (icritreg=10)
c
      ierr=0
      herr=' '
c
      if (icomp.eq.0) then
        ierr=1
        write (herr,1001)
        call ERRMSG (ierr,herr)
 1001   format ('[SETLSPNDL error 1] ',
     &          'SETLSPNDL can handle pure fluids only')
        RETURN
      endif
c
      call INFO (icomp,wmm,tt,tb,tc,pc,dc,zc,acf,dip,rgas)
      call CRITP (x,tc,pc,rhoc,ierr,herr)
c
      t=tt
      pt=pc*10.d0**(-2.333333d0*(1.d0+accen(icomp))*(tc/tt-1.d0))
      call TPRHO (tt,pt,x,1,0,rhol,ierr,herr)
      dt=(tc-tt)*qcritreg/(iptmax-icritreg-1)
      j=0
      rhol=dtpn(icomp)
      do i=1,iptmax-icritreg
        call LIQSPNDL (t,x,rhol,ierr,herr)
        if (ierr.eq.0) then
          j=j+1
          dspndl_l(j)=rhol
          tspndl_l(j)=t
        endif
        t=t+dt
      enddo
c
      dt=(tc-t)/(icritreg-1)
      do i=1,icritreg
        call LIQSPNDL (t,x,rhol,ierr,herr)
        if (ierr.eq.0) then
          j=j+1
          dspndl_l(j)=rhol
          tspndl_l(j)=t
        endif
        t=t+dt
      enddo
c
      if (j.lt.5) then
        ierr=2
        write (herr,1002)
        call ERRMSG (ierr,herr)
 1002   format ('[SETLSPNDL error 2] ',
     &          'Less then 5 points converged in LIQSPNDL')
        RETURN
      endif
c
      ispndl_l=j
      call CSPLINE (ispndl_l-1, tspndl_l, dspndl_l, cspndl_l)
c
      end                                          !subroutine SETLSPNDL
c
c ======================================================================
c
      subroutine SETVSPNDL (icomp,ierr,herr)
c
c  Prepares a cubic spline interpolating the vapor spinodal.
c
c  inputs:
c    icomp--pure fluid number for which spinodals will be calculated
c  outputs:
c     ierr--error flag:   0 = successful
c                         1 = called with a mixture composition (pure fluids only)
c                         2 = convergence problems in LIQSPNDL
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Matthias Kunick, NIST Thermophysics Division, Boulder, Colorado
c  09-15-11  MK, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
      dimension dspndl_v_save(iptmax)
      dimension tspndl_v_save(iptmax)
c
      parameter (qcritreg=0.85d0)
      parameter (qlowtreg=0.75d0)
      parameter (icritreg=10)
      parameter (ilowtreg=10)
c
      ierr=0
      herr=' '
c
      if (icomp.eq.0) then
        ierr=1
        write (herr,1001)
        call ERRMSG (ierr,herr)
 1001   format ('[SETVSPNDL error 1] ',
     &          'SETVSPNDL can handle pure fluids only')
        RETURN
      endif
c
      call INFO (icomp,wmm,tt,tb,tc,pc,dc,zc,acf,dip,rgas)
      call CRITP (x,tc,pc,rhoc,ierr,herr)
c
      t=tt
      pt=pc*10.d0**(-2.333333d0*(1.d0+accen(icomp))*(tc/tt-1.d0))
      call TPRHO (tt,pt,x,2,0,rhov,ierr,herr)
      tlow=tt*qlowtreg
      dt=(tlow-tt)/(ilowtreg-1)
      j=0
      rhov=ptpn(icomp)/r/ttpn(icomp)
      if (rhov.gt.200) rhov=1.d-10  !Check for fitted fld file with t<1
      do i=1,ilowtreg
        call VAPSPNDL (t,x,rhov,ierr,herr)
        if (ierr.eq.0) then
          j=j+1
          dspndl_v(j)=rhov
          tspndl_v(j)=t
        endif
        t=t+dt
      enddo
c
      !re-order points
      do k=1,j
        dspndl_v_save(k)=dspndl_v(k)
        tspndl_v_save(k)=tspndl_v(k)
      enddo
      do k=1,j
        dspndl_v(k)=dspndl_v_save(j-k+1)
        tspndl_v(k)=tspndl_v_save(j-k+1)
      enddo
c
      t=tspndl_v(j)-dt
      dt=(tc-t)*qcritreg/(iptmax-icritreg-ilowtreg-1)
      rhov=dspndl_v(j)
      do i=1,iptmax-icritreg-ilowtreg
        call VAPSPNDL (t,x,rhov,ierr,herr)
        if (ierr.eq.0) then
          j=j+1
          dspndl_v(j)=rhov
          tspndl_v(j)=t
        endif
        t=t+dt
      enddo
c
      dt=(tc-t)/(icritreg-1)
      do i=1,icritreg
        call VAPSPNDL (t,x,rhov,ierr,herr)
        if (ierr.eq.0) then
          j=j+1
          dspndl_v(j)=rhov
          tspndl_v(j)=t
        endif
        t=t+dt
      enddo
c
      if (j.lt.5) then
        ierr=2
        write (herr,1002)
        call ERRMSG (ierr,herr)
 1002   format ('[SETVSPNDL error 2] ',
     &          'Less then 5 points converged in VAPSPNDL')
        RETURN
      endif
c
      ispndl_v=j
      call CSPLINE (ispndl_v-1, tspndl_v, dspndl_v, cspndl_v)
c
      end                                          !subroutine SETVSPNDL
c
c ======================================================================
c
      subroutine FNCRPT (x,d,p,t,ierr,herr)
c
c  Find the critical point of a pure fluid based on the EOS.
c  This is currently only for pure fluids!
c
c  inputs:
c        x--composition [array of mol frac]
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        d--density [mol/L]
c     ierr--error flag:   0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-01-96 EWL, original version taken from code in SATT
c
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call CRITP (x,t,pc2,d,ierr,herr)
      iter=0
      ac=1.d-10
c     ac=1.d-8
c  get first point
      told=t+.1d0
      d=crtden(x,told,d)
      if (d.le.0) goto 30
      call DPDD (told,d,x,fold)
      fold=fold/1000.d0
 20   d=crtden(x,t,d)
      if (d.le.0) goto 30
      call DPDD (t,d,x,f)
      f=f/1000.d0
      iter=iter+1
      if (iter.gt.20) goto 30
      if(ABS(f).lt.ac)then
        call PRESS (t,d,x,p)
        RETURN
      endif
c  get next point by using the slope of the previous two points.
      if (fold.eq.f) goto 30
      t1=t-f*(told-t)/(fold-f)
      told=t
      fold=f
      t=t1
      if (t.gt.0) goto 20
 30   continue
c     write (*,*) ' *** Fncrpt: No convergence ***'
      end                                             !subroutine FNCRPT
      function CRTDEN (x,t,d)
      include 'commons.for'
      dimension x(ncmax)
      iter=0
 10   iter=iter+1
      call DPDD2 (t,d,x,df0)
      crtden=d
      if(ABS(df0).lt.1.d-12) RETURN
c     if(ABS(df0).lt.1.d-10) return
      if (iter.gt.30) then
c       write (*,*) 'Crtden: No convergence'
        RETURN
      endif
      call DPDD2 (t,d+0.0001d0,x,df1)
      call DPDD2 (t,d-0.0001d0,x,df2)
      fp=(df1-df2)/0.0002d0
      fpp=(df1+df2-2.d0*df0)/0.00000001d0
      d=d+1.d0/(-fp/df0+fpp/2.d0/fp)
      if (d.le.0) RETURN
      goto 10
      end                                               !function CRTDEN
c
c ======================================================================
c
      subroutine SATEST (iFlash,t,p,x,xliq,xvap,ierr,herr)
c
c  Estimate temperature, pressure, and compositions to be used
c  as initial guesses to SATTP.
c
c  inputs:
c   iFlash--see inputs to SATTP
c        t--temperature [K] (input or output)
c        p--pressure [MPa] (input or output)
c        x--composition [array of mol frac]
c  outputs:
c     xliq--liquid phase composition [array of mol frac]
c     xvap--vapor phase composition [array of mol frac]
c     ierr--error flag:   0 = successful
c                         1 = unsuccessful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Diego Ortiz, NIST Thermophysics Division, Boulder, Colorado
c  07-01-10 DOO, original version
c
      include 'commons.for'
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      character herr*255
c     dimension tini,ti,Tol,told,fnold
c     dimension i,j,k,dummy
c     dimension amag,fn,fnprime,tmp2
c     dimension tmp3,tmp4,taux1,taux2,taux3
c     dimension fnaux1,fnaux2,fnaux3,fnini
c     dimension tx,alpha
      dimension pc2(ncmax),tc2(ncmax),acf2(ncmax),wmm2(ncmax)
c     dimension wmm,ttrp,tnbpt,tc,pc,dc,zc,acf,dip,Rgas
c
      ierr=0
      do i=1,nc
        Call INFO (i,wmm2(i),ttrp,tnbpt,tc2(i),pc2(i),dc,zc,acf2(i),dip,
     & Rgas)
        xliq(i)=x(i)
        xvap(i)=x(i)
      enddo
      alpha=1

      If (t.eq.0 .or. iFlash.eq.3 .or. iFlash.eq.4) Then
        If (p.le.0) Then
          ierr=1
          RETURN
        endif
        tini=0
        do i=1,nc
          tini=tini+x(i)*tc2(i)/(1-
     & 0.428571*(Log(p/pc2(i))/Log(10D0))/(1+acf2(i)))
        enddo
      Else
        tini=t
      End If
      Tol=0.00000001

C     Do not remove parenthesis around p on the calls to PTEST.  It needs to send the value, not the memory location.
      If (iFlash.eq.1) Then
        Call PTEST (1,t,(p),alpha,x,tc2,pc2,acf2,p,xvap,ierr,herr)
        Call PTEST (5,t,(p),alpha,x,tc2,pc2,acf2,p,xvap,ierr,herr)

      ElseIf (iFlash.eq.2) Then
        Call PTEST (2,t,(p),alpha,x,tc2,pc2,acf2,p,xliq,ierr,herr)
        Call PTEST (6,t,(p),alpha,x,tc2,pc2,acf2,p,xliq,ierr,herr)

      ElseIf (iFlash.eq.3 .or. iFlash.eq.4) Then
        do j=1,7
          dummy=0.7d0+(1.d0-j)/10.d0
          Call PTEST(iFlash,tini,p,alpha,x,tc2,pc2,acf2,fnini,xvap,ierr,
     & herr)
          taux1=dummy*tini
          Call PTEST (iFlash,taux1,p,alpha,x,tc2,pc2,acf2,fnaux1,xvap,
     & ierr,herr)
          taux2=tini/dummy
          Call PTEST (iFlash,taux2,p,alpha,x,tc2,pc2,acf2,fnaux2,xvap,
     & ierr,herr)
          If (fnini*fnaux1.lt.0 .or. fnini*fnaux2.lt.0) GoTo 10
        enddo
 10   continue
        If (fnini*fnaux1.lt.0) Then
          taux2=(tini+taux1)/2
          Call PTEST (iFlash,taux2,p,alpha,x,tc2,pc2,acf2,fnaux2,xvap,
     & ierr,herr)
          If (fnaux2*fnini.lt.0) Then
            taux1=(taux2+tini)/2
            Call PTEST (iFlash,taux1,p,alpha,x,tc2,pc2,acf2,fnaux1,xvap,
     & ierr,herr)
            t=(taux1+taux2)/2
            If (fnaux1*fnini.lt.0) t=(taux1+tini)/2
          Else
            taux3=(taux2+taux1)/2
            Call PTEST (iFlash,taux3,p,alpha,x,tc2,pc2,acf2,fnaux3,xvap,
     & ierr,herr)
            t=(taux3+taux1)/2
            If (fnaux3*fnaux2.lt.0) t=(taux3+taux2)/2
          End If
        ElseIf (fnini*fnaux2.lt.0) Then
          taux1=(tini+taux2)/2
          Call PTEST (iFlash,taux1,p,alpha,x,tc2,pc2,acf2,fnaux1,xvap,
     & ierr,herr)
          If (iFlash.eq.3) Then
            t=(taux1+taux2)/2
            If (fnaux1*fnini.lt.0) t=(taux1+tini)/2
          ElseIf (iFlash.eq.4) Then
            If (fnaux1*fnini.lt.0) Then
              taux2=(taux1+tini)/2
              Call PTEST (iFlash,taux2,p,alpha,x,tc2,pc2,acf2,fnaux2,
     & xvap,ierr,herr)
              t=(taux2+taux1)/2
              If (fnaux2*fnini.lt.0) t=(taux2+tini)/2
            Else
              taux3=(taux1+taux2)/2
              Call PTEST (iFlash,taux3,p,alpha,x,tc2,pc2,acf2,fnaux3,
     & xvap,ierr,herr)
              t=(taux3+taux2)/2
              If (fnaux3*fnaux1.lt.0) t=(taux3+taux1)/2
            End If
          End If
        End If
        amag=2*Tol
        tmp3=100
        tmp4=100
        j=0
 30   continue
        If (amag.gt.Tol .or. tmp3.gt.1 .or. tmp4.gt.1) Then
          j=j+1
          If (j.gt.100) Then
            ierr=1
            RETURN
          endif
          Call PTEST (iFlash,t,p,alpha,x,tc2,pc2,acf2,fn,xvap,ierr,herr)
          tmp2=0
          do i=1,nc
            tx=5.373*tc2(i)
            If (iFlash.eq.3) tmp2=tmp2+(Exp((-tx*acf2(i)-tx)/t+
     &          5.373*acf2(i)))*(tx*pc2(i)*acf2(i)*x(i)+tx*pc2(i)*x(i))
            If (iFlash.eq.4) tmp2=tmp2+(Exp((tx*acf2(i)+tx)/t-
     &          5.373*acf2(i)))*((-tx*acf2(i)*x(i))/pc2(i)-
     &          (tx*x(i))/pc2(i))
          enddo
          fnprime=215.508424158*tmp2*((fn-1)/t)**2/p
          If (iFlash.eq.4) fnprime=-0.004640189839*p*tmp2/(t**2)
          told=t
          If (fnprime.ne.0) t=told-(fn/fnprime)
          fnold=fn
          Call PTEST (iFlash,t,p,alpha,x,tc2,pc2,acf2,fn,xvap,ierr,herr)
          amag=Abs(fn)
          tmp3=(Abs(told-t)/told)*100
          tmp4=(Abs(fnold-fn)/fnold)*100
          GoTo 30
        End If
        If (iFlash.eq.3) Call PTEST(5,t,p,alpha,x,tc2,pc2,acf2,fn,xvap,
     &      ierr,herr)
        If (iFlash.eq.4) Call PTEST(6,t,p,alpha,x,tc2,pc2,acf2,fn,xliq,
     &      ierr,herr)
      End If
      end                                             !subroutine SATEST
c
c ======================================================================
c
      subroutine SAT0EST (t,p,x,xliq,xvap,ierr,herr)
c
c  Estimate compositions for 2-phase states to be used
c  as initial guesses to SATTP.
c
c  inputs:
c   iFlash--see inputs to SATTP
c        t--temperature [K] (input or output)
c        p--pressure [MPa] (input or output)
c        x--composition [array of mol frac]
c  outputs:
c     xliq--liquid phase composition [array of mol frac]
c     xvap--vapor phase composition [array of mol frac]
c     ierr--error flag:   0 = successful
c                         1 = unsuccessful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Diego Ortiz, NIST Thermophysics Division, Boulder, Colorado
c  07-01-10 DOO, original version
c
      include 'commons.for'
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      character herr*255
c     dimension alphaini,ti,Tol,alphaold,fnold
c     dimension i,j,k,dummy,Pb,Tb,Pd,Td
c     dimension temp1,amag,fn,fnprime,temp2
c     dimension temp3,temp4,alpha1,alpha2,alpha3,alpha
c     dimension fnaux1,fnaux2,fnaux3,fnini
      dimension pc2(ncmax),tc2(ncmax),acf2(ncmax),wmm2(ncmax)
c     dimension wmm,ttrp,tnbpt,tc,pc,dc,zc,acf,dip,Rgas
c
      ierr=0
      do i=1,nc
        Call INFO (i,wmm2(i),ttrp,tnbpt,tc2(i),pc2(i),dc,zc,acf2(i),dip,
     & Rgas)
      enddo

C     Check if (T,P) is in the two-phase region
      Tb=0.d0
      Td=0.d0
      Call SATEST (1,t,Pb,x,xliq,xvap,ierr,herr)
      Call SATEST (2,t,Pd,x,xliq,xvap,ierr,herr)
      Call SATEST (3,Tb,p,x,xliq,xvap,ierr,herr)
      Call SATEST (4,Td,p,x,xliq,xvap,ierr,herr)
      If ((t-Td)*(t-Tb).gt.0 .or. (p-Pb)*(p-Pd).gt.0) Then
        do i=1,nc
        xvap(i)=x(i)/2
        xliq(i)=x(i)
        enddo
        RETURN
      End If

      alphaini=0.5
      alpha1=0.000000001
      alpha2=0.999999999
      Call PTEST (4,t,p,alphaini,x,tc2,pc2,acf2,fnini,xvap,ierr,herr)
      Call PTEST (4,t,p,alpha1,x,tc2,pc2,acf2,fnaux1,xvap,ierr,herr)
      Call PTEST (4,t,p,alpha2,x,tc2,pc2,acf2,fnaux2,xvap,ierr,herr)

      If (fnini*fnaux1.lt.0) Then
        alpha2=(alphaini+alpha1)/2
        Call PTEST (4,t,p,alpha2,x,tc2,pc2,acf2,fnaux2,xvap,ierr,herr)
        If (fnaux2*fnini.lt.0) Then
          alpha1=(alpha2+alphaini)/2
          Call PTEST (4,t,p,alpha1,x,tc2,pc2,acf2,fnaux1,xvap,ierr,herr)
          alpha=(alpha1+alpha2)/2
          If (fnaux1*fnini.lt.0) alpha=(alpha1+alphaini)/2
        Else
          alpha3=(alpha2+alpha1)/2
          Call PTEST (4,t,p,alpha3,x,tc2,pc2,acf2,fnaux3,xvap,ierr,herr)
          alpha=(alpha3+alpha1)/2
          If (fnaux3*fnaux2.lt.0) alpha=(alpha3+alpha2)/2
        End If
      ElseIf (fnini*fnaux2.lt.0) Then
        alpha1=(alphaini+alpha2)/2
        Call PTEST (4,t,p,alpha1,x,tc2,pc2,acf2,fnaux1,xvap,ierr,herr)
        If (fnaux1*fnini.lt.0) Then
          alpha2=(alpha1+alphaini)/2
          Call PTEST (4,t,p,alpha2,x,tc2,pc2,acf2,fnaux2,xvap,ierr,herr)
          alpha=(alpha2+alpha1)/2
          If (fnaux2*fnini.lt.0) alpha=(alpha2+alphaini)/2
        Else
          alpha3=(alpha1+alpha2)/2
          Call PTEST (4,t,p,alpha3,x,tc2,pc2,acf2,fnaux3,xvap,ierr,herr)
          alpha=(alpha3+alpha2)/2
          If (fnaux3*fnaux1.lt.0) alpha=(alpha3+alpha1)/2
        End If
      End If

      Tol=0.00001d0
      amag=2*Tol
      temp3=100
      temp4=100
      j=0
 30   continue
      If (amag.gt.Tol .or. temp3.gt.1 .or. temp4.gt.1) Then
        j=j+1
        Call PTEST (4,t,p,alpha,x,tc2,pc2,acf2,fn,xvap,ierr,herr)
        temp1=0
        do i=1,nc
          temp2=Exp((5.373d0*tc2(i)*acf2(i)+5.373*tc2(i))/t)
          temp3=pc2(i)*215.508424158d0**(1+acf2(i))
          temp1=temp1-(x(i)*0.004640189839d0*temp2*(p*temp2-
     & temp3))/((alpha*(p*temp2-temp3)-p*temp2)**2)
        enddo
        fnprime=p*temp1*215.508424158d0
        alphaold=alpha
        alpha=alphaold-(fn/fnprime)
        if (alpha.gt.1.d0) alpha=(1.d0+alphaold)/2.d0
        If (alpha.gt.1E+20 .or. j.gt.100) Then
          ierr=1
          RETURN
        endif
        fnold=fn
        Call PTEST (4,t,p,alpha,x,tc2,pc2,acf2,fn,xvap,ierr,herr)
        amag=Abs(fn)
        temp3=(Abs(alphaold-alpha)/alphaold)*100
        temp4=(Abs(fnold-fn)/fnold)*100
        GoTo 30
      End If

      Call PTEST (6,t,p,alpha,x,tc2,pc2,acf2,fn,xliq,ierr,herr)
      do i=1,nc
        xvap(i)=(x(i)-xliq(i)*(1-alpha))/alpha
      enddo
      end                                            !subroutine SAT0EST
c
c ======================================================================
c
      subroutine PTEST(inp,t,p,alpha,x,tc2,pc2,acf2,calc,xout,ierr,herr)
c
c  Part of the SATEST routines
c
c  inputs:
c  outputs:
c     ierr--error flag:   0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Diego Ortiz, NIST Thermophysics Division, Boulder, Colorado
c  07-01-10 DOO, original version
c
      include 'commons.for'
      dimension x(ncmax),xout(ncmax),pc2(ncmax),tc2(ncmax),acf2(ncmax)
      character herr*255
c     dimension i
c     dimension sum
      dimension ptemp(ncmax)
c
      ierr=0
      herr=' '

      If (t.le.0) RETURN
      do i=1,nc
        ptemp(i)=pc2(i)*Exp(5.373*(1+acf2(i))*(1-tc2(i)/t))
        If (ptemp(i).gt.pc2(i)) ptemp(i)=pc2(i)+1*(ptemp(i)-pc2(i))
      enddo
      sum=0
      If (inp.eq.1) Then
        do i=1,nc
          sum=sum+x(i)*ptemp(i)
        enddo
        calc=sum
      ElseIf (inp.eq.2) Then
        do i=1,nc
          sum=sum+x(i)/ptemp(i)
        enddo
        calc=1/sum
      ElseIf (inp.eq.3) Then
        do i=1,nc
          sum=sum+x(i)*ptemp(i)
        enddo
        calc=1-p/sum
      ElseIf (inp.eq.4) Then
        do i=1,nc
          divs=(1-alpha+alpha*ptemp(i)/p)
          if (divs.gt.0.d0) sum=sum+x(i)/divs
        enddo
        calc=1-sum
      ElseIf (inp.eq.5) Then
        do i=1,nc
          xout(i)=x(i)/p*ptemp(i)
          If (xout(i).lt.0.000001 .and. x(i).gt.0.d0) xout(i)=0.000001
          If (xout(i).gt.0.999999) xout(i)=0.999999
          sum=sum+xout(i)
        enddo
        do i=1,nc
          xout(i)=xout(i)/sum
        enddo
      ElseIf (inp.eq.6) Then
        do i=1,nc
          xout(i)=x(i)/(1-alpha+alpha*ptemp(i)/p)
          If (xout(i).lt.0.000001 .and. x(i).gt.0.d0) xout(i)=0.000001
          If (xout(i).gt.0.999999) xout(i)=0.999999
          sum=sum+xout(i)
        enddo
        do i=1,nc
          xout(i)=xout(i)/sum
        enddo
      Else
      End If
      end                                              !subroutine PTEST
c
c ======================================================================
c
      subroutine SATTP (t,p,x,iFlsh,iGuess,d,Dl,Dv,xliq,xvap,q,ierr,
     & herr)
c
c  Estimate temperature, pressure, and compositions to be used
c  as initial guesses to SATTP
c
c  inputs:
c   iFlsh--Phase flag:    0 - Flash calculation (T and P known)
c                         1 - T and xliq known, P and xvap returned
c                         2 - T and xvap known, P and xliq returned
c                         3 - P and xliq known, T and xvap returned
c                         4 - P and xvap known, T and xliq returned
c                         if this value is negative, the retrograde point will be returned
c        t--temperature [K] (input or output)
c        p--pressure [MPa] (input or output)
c        x--composition [array of mol frac]
c   iGuess--if set to 1, all inputs are used as initial guesses for the calculation
c  outputs:
c        d--overall molar density [mol/L]
c       Dl--molar density [mol/L] of saturated liquid
c       Dv--molar density [mol/L] of saturated vapor
c     xliq--liquid phase composition [array of mol frac]
c     xvap--vapor phase composition [array of mol frac]
c        q--quality
c     ierr--error flag:   0 = successful
c                         1 = unsuccessful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Andreas Jaeger, Johannes Gernert, Diego Ortiz, and Eric Lemmon
c  06-20-11 EWL, original version
c
      include 'commons.for'
      dimension x(ncmax),xliq(ncmax),xvap(ncmax),ix(ncmax),ij(ncmax)
      character herr*255
      dimension dadn(ncmax),dnadn(ncmax)
      dimension xvap_new(ncmax),xliq_new(ncmax)
      dimension xJacMatrix(50,50),Delta_X(50),GibbsEQN(50)
      dimension xlnfiliq(ncmax),xlnfivap(ncmax),
     &          dphidxjliq(ncmax,ncmax),dphidxjvap(ncmax,ncmax),
     &          dphidTliq(ncmax),dphidPliq(ncmax),
     &          dphidTvap(ncmax),dphidPvap(ncmax)

c  Set up pointer array ix to sort the components by largest first, removing
c  any that are zero.
      iGues=iGuess
      nc2=nc
      do i=1,nc
        ij(i)=1
      enddo
      do i=1,nc
        if (x(i).le.0.d0) nc2=nc2-1
        k=1
        do j=1,nc
          if (ij(k).eq.0) k=j
          if (x(j).gt.x(k) .and. ij(j).eq.1) k=j
        enddo
        ij(k)=0
        if (x(k).gt.0.d0) ix(i)=k
      enddo

      call ISPURE (x,icomp)
      call CRITP (x,tc,pc,rhoc,ierr,herr)
      iSatFlg=0
      irdxerr=0
      eMax=0
      iretrog=0
      if (iFlsh.lt.0) iretrog=1
      iFlash=abs(iFlsh)

      if (iSpline.ne.0 .and. iFlash.gt.0) then
        call MAXT (x,tmaxt,tmaxp,tmaxd,ierr,herr)
        call MAXP (x,pmaxt,pmaxp,pmaxd,ierr,herr)
        d=rhoc*2.d0
        if (iFlash.eq.2 .or. iFlash.eq.4) d=rhoc/10.d0
        if (iretrog.eq.1 .and. tmaxd.gt.0.d0) d=(tmaxd+rhoc)/2.d0
        if (iFlash.eq.1 .or. iFlash.eq.2) then
          call SPLNROOT (nc+1,0,t,d,ierr,herr)
          if (ierr.gt.0) then
            d=d*0.75d0
            if (d.lt.1.d-5) d=rhoc*0.75d0
            call SPLNROOT (nc+1,0,t,d,ierr,herr)
          endif
          if (ierr.gt.0) d=rhoc*2.d0
          call SPLNVAL (nc+1,1,d,dydd,ierr,herr)
          if (iFlash.eq.1 .and. dydd.gt.0) goto 10  !Exit if d(T)/d(rho) is positive (vapor surface)
        else
          call SPLNROOT (nc+2,0,p,d,ierr,herr)
          call SPLNVAL (nc+2,1,d,dydd,ierr,herr)
          if (iFlash.eq.3 .and. dydd.gt.0) goto 10  !Exit if d(P)/d(rho) is positive (vapor surface)
        endif
        Dl=d
        Dv=d

        do j=1,nc+3
          call SPLNVAL (j,0,d,y,ierr,herr)
          y=abs(y)
          if (j.le.nc) then
            if (iFlash.eq.1 .or. iFlash.eq.3) then
              xvap(j)=abs(y)
              xliq(j)=x(j)
            else
              xliq(j)=abs(y)
              xvap(j)=x(j)
            endif
          elseif (j.eq.nc+1) then
            if (iFlash.eq.3 .or. iFlash.eq.4) t=y
          elseif (j.eq.nc+2) then
            if (iFlash.eq.1 .or. iFlash.eq.2) p=y
          elseif (j.eq.nc+3) then
            if (iFlash.eq.1 .or. iFlash.eq.3) Dv=y
            if (iFlash.eq.2 .or. iFlash.eq.4) Dl=y
          endif
        enddo
        sumx=sum(xliq(1:nc))
        sumy=sum(xvap(1:nc))
        xliq(1:nc)=xliq(1:nc)/sumx
        xvap(1:nc)=xvap(1:nc)/sumy
        iGues=1
      endif

 10   continue
c  PR does not have derivatives needed for SATTP, must use SATT
      if (icomp.ne.0 .or. irdxerr.ne.0 .or .heos.eq.'PR') then
        iSatFlg=1
        if (iFlash.eq.1 .or. iFlash.eq.2) then
          call SATT (t,x,iFlash,p,Dl,Dv,xliq,xvap,ierr,herr)
        elseif (iFlash.eq.3 .or. iFlash.eq.4) then
          call SATP (p,x,iFlash-2,t,Dl,Dv,xliq,xvap,ierr,herr)
        else
          call TPFLSH(t,p,x,d,Dl,Dv,xliq,xvap,q,e,h,s,cv,cp,w,ierr,herr)
        endif
        if (iFlash.eq.1 .or. iFlash.eq.3) then
          q=0
          d=Dl
        elseif (iFlash.eq.2 .or. iFlash.eq.4) then
          q=1
          d=Dv
        endif
        iSatFlg=0
        if (ierr.eq.0) then
          if (abs(dl-dv).lt.0.1d0 .and. abs(dl-rhoc).gt.0.1d0) ierr=1
          if (dl+dv.lt.rhoc) ierr=1
        endif
        RETURN
      endif

      If (iGues.eq.0) Then
        dv=rhoc
        dl=rhoc
        If (iFlash.eq.3 .or. iFlash.eq.4) t=300
        If (iFlash.ne.0) Call SATEST (iFlash,t,p,x,xliq,xvap,ierr,herr)
        If (iFlash.eq.0) Call SAT0EST (t,p,x,xliq,xvap,ierr,herr)
      End If

      tol1=1.d-12
      tol2=1.d-10
      ierr=0
      herr=' '
      dvap=dv
      dliq=dl
      ieqn=nc2
      if (iFlash.eq.0) ieqn=2*(nc2-1)

      do il=1,30
        GibbsEQN=0.d0
c...Vapor
        call TPRHO (t,p,xvap,2,iGues,dvap,ierr,herr)
        if (ierr.ne.0) then
          call TPRHO (t,p,xvap,1,0,dvap,ierr,herr)
          if (ierr.ne.0) goto 1000
        endif
        call PHIDERV (2,t,dvap,xvap,dadn,dnadn,ierr,herr)
        if (ierr.eq.191) then !Derivatives of the Helmholtz equation are not available, use SATT or SATP
          irdxerr=ierr
          goto 10
        endif
        if (ierr.ne.0) goto 1000
        do j=1,nc2
          xlnfivap(j)=xlnfi(ix(j))
          dphidtvap(j)=dphidt(ix(j))
          dphidpvap(j)=dphidp(ix(j))
          do k=1,nc2
          dphidxjvap(j,k)=dphidxj(ix(j),ix(k))-dlnphiidxj(ix(nc2),ix(k))
          enddo
          dphidxjvap(j,nc2)=dphidxjvap(j,nc2)-1.d0/xvap(ix(nc2))
        end do

c...Liquid
        call TPRHO (t,p,xliq,1,iGues,dliq,ierr,herr)
        if (ierr.ne.0 .and. iFlash.eq.3) then
          t=t-5.d0
          call TPRHO (t,p,xliq,1,iGues,dliq,ierr,herr)
          if (ierr.ne.0) then
            t=t-5.d0
            call TPRHO (t,p,xliq,1,iGues,dliq,ierr,herr)
          endif
          if (ierr.ne.0) t=t+10.d0
        endif
        if (ierr.ne.0 .and. iFlash.eq.1) then
          p0=p
          p=p*1.2d0
          call TPRHO (t,p,xliq,1,iGues,dliq,ierr,herr)
          if (ierr.ne.0) then
            p=p*1.5d0
            call TPRHO (t,p,xliq,1,iGues,dliq,ierr,herr)
          endif
          if (ierr.ne.0) then
            p=p*1.5d0
            call TPRHO (t,p,xliq,1,iGues,dliq,ierr,herr)
          endif
          if (ierr.ne.0) p=p0
        endif
        if (ierr.ne.0) then
          call TPRHO (t,p,xliq,2,0,dliq,ierr,herr)
          if (ierr.ne.0) goto 1000
        endif
        call PHIDERV (2,t,dliq,xliq,dadn,dnadn,ierr,herr)
        if (ierr.ne.0) goto 1000
        do j=1,nc2
          xlnfiliq(j)=xlnfi(ix(j))
          dphidtliq(j)=dphidt(ix(j))
          dphidpliq(j)=dphidp(ix(j))
          do k=1,nc2
          dphidxjliq(j,k)=dphidxj(ix(j),ix(k))-dlnphiidxj(ix(nc2),ix(k))
          enddo
          dphidxjliq(j,nc2)=dphidxjliq(j,nc2)-1.d0/xliq(ix(nc2))
        end do

        do j=1,nc2
          GibbsEQN(j)=xlnfivap(j)-xlnfiliq(j)
        end do
        if (iFlash.eq.0) then
          do j=1,nc2-2
            dxkdxj_vap=(xvap(ix(nc2-1))-x(ix(nc2-1)))/
     &                 (xvap(ix(j))-x(ix(j)))
            dxkdxj_liq=(xliq(ix(nc2-1))-x(ix(nc2-1)))/
     &                 (xliq(ix(j))-x(ix(j)))
            GibbsEQN(nc2+j)=dxkdxj_vap-dxkdxj_liq
          end do
        end if

        if (il.eq. 8) tol1=tol1*10
        if (il.eq.12) tol1=tol1*10
        if (il.eq.16) tol1=tol1*10
        if (il.eq.20) tol1=tol1*10
        if (il.eq. 8) tol2=tol2*10
        if (il.eq.12) tol2=tol2*10
        if (il.eq.16) tol2=tol2*10
        if (il.eq.20) tol2=tol2*10
        eMax=maxval(dabs(GibbsEQN))
        if (eMax.lt.tol1) goto 1000

c..Set up Jacobian matrix
        xJacMatrix(1:ieqn,1:ieqn)=0.d0
        if (iFlash.eq.0) then
          do j=1,nc2-1
            do k=1,nc2
              xJacMatrix(k,j)=dphidxjvap(j,k)
              xJacMatrix(k,nc2-1+j)=-dphidxjliq(j,k)
            end do
          end do
          do j=1,nc2-2
            xJacMatrix(nc2+j,j)=
     &               -(xvap(ix(nc2-1))-x(ix(nc2-1)))/
     &               ((xvap(ix(j))-x(ix(j)))**2)
            xJacMatrix(nc2+j,nc2-1)=1.d0/(xvap(ix(j))-x(ix(j)))
            xJacMatrix(nc2+j,nc2-1+j)=
     &                (xliq(ix(nc2-1))-x(ix(nc2-1)))/
     &               ((xliq(ix(j))-x(ix(j)))**2)
            xJacMatrix(nc2+j,2*(nc2-1))=-1.d0/(xliq(ix(j))-x(ix(j)))
          end do
        else
          do k=1,nc2
            do j=1,nc2-1
              if (iFlash.eq.1 .or. iFlash.eq.3) then
                xJacMatrix(k,j)=dphidxjvap(j,k)
              elseif (iFlash.eq.2 .or. iFlash.eq.4) then
                xJacMatrix(k,j)=-dphidxjliq(j,k)
              endif
            end do
            if (iFlash.eq.1 .or. iFlash.eq.2) then
              xJacMatrix(k,nc2)=dphidpvap(k)-dphidpliq(k)
            elseif (iFlash.eq.3 .or. iFlash.eq.4) then
              xJacMatrix(k,nc2)=dphidtvap(k)-dphidtliq(k)
            endif
          end do
        endif
c..Solve Jacobian matrix
        Delta_X=-GibbsEQN
        call LUdecomp (ieqn,50,xJacMatrix,Delta_X,ierr,herr)
        eMax=maxval(dabs(Delta_X))
        if (eMax.lt.tol2) goto 1000

 100    continue
        jj=0
        if (iFlash.eq.0) jj=nc2-1
        t_new=0.d0
        p_new=0.d0
        sum_liq=0.d0
        sum_vap=0.d0
        sum_vsmall=0.d0

        do j=1,nc2
          if (x(ix(j)).gt.0.d0) then
            if (j.lt.nc2) then
              xvap_new(ix(j))=xvap(ix(j))+Delta_X(j)
              xliq_new(ix(j))=xliq(ix(j))+Delta_X(j+jj)
            else
              xvap_new(ix(nc2))=1.d0-sum_vap-sum_vsmall
              xliq_new(ix(nc2))=1.d0-sum_liq
              t_new=t+Delta_X(j)
              p_new=p+Delta_X(j)
            end if
            sum_vap=sum_vap+xvap_new(ix(j))
            sum_liq=sum_liq+xliq_new(ix(j))
            if (xvap_new(ix(j)).lt.1.d-10)
     &        sum_vsmall=sum_vsmall+xvap_new(ix(j))
            k=0
            if (iFlash.eq.1 .or. iFlash.eq.3) then
              if (xvap_new(ix(j)).le.0.d0.or.xvap_new(ix(j)).ge.1.d0)k=1
              if (p_new.lt.0) k=1
            elseif (iFlash.eq.2 .or. iFlash.eq.4) then
              if (xliq_new(ix(j)).le.0.d0.or.xliq_new(ix(j)).ge.1.d0)k=1
            else
              if (((xvap_new(ix(j))-x(ix(j)))
     &            *(xliq_new(ix(j))-x(ix(j))).gt.0.d0)
     &         .or.(xliq_new(ix(j))*xvap_new(ix(j)).lt.0.d0)) k=1
            endif
            if (k.eq.1) then
              Delta_X=Delta_X/2.d0
              if (maxval(dabs(Delta_X)).lt.1.d-10) goto 999
              goto 100
            end if
          endif
        end do

        Dl=dliq
        Dv=dvap
        if (iFlash.eq.1) then
          q=0
          xvap(ix(1:nc2))=xvap_new(ix(1:nc2))
          p=p_new
          d=dliq
        elseif (iFlash.eq.2) then
          q=1
          xliq(ix(1:nc2))=xliq_new(ix(1:nc2))
          p=p_new
          d=dvap
        elseif (iFlash.eq.3) then
          q=0
          xvap(ix(1:nc2))=xvap_new(ix(1:nc2))
          t=t_new
          d=dliq
        elseif (iFlash.eq.4) then
          q=1
          xliq(ix(1:nc2))=xliq_new(ix(1:nc2))
          t=t_new
          d=dvap
        elseif (iFlash.eq.0) then
          q=(x(ix(1))-xliq(ix(1)))/(xvap(ix(1))-xliq(ix(1)))
          xvap(ix(1:nc2))=xvap_new(ix(1:nc2))
          xliq(ix(1:nc2))=xliq_new(ix(1:nc2))
          d=1.d0/((1.0d0-q)/dliq+q/dvap)
        else
          stop
        endif
        if (p.le.0.d0) goto 1000
      enddo

 999  ierr=3
      herr='SATTP:  Error in iteration'
      if (il.gt.30) then
        ierr=2
        herr='SATTP:  Maximum number of iterations exceeded'
      endif
1000  continue
      if (dv.lt.0) ierr=1
      if (dl.lt.0) ierr=1
      if (abs(dl-dv).lt.0.1d0 .and. abs(dl-rhoc).gt.0.1d0) ierr=1
c     if (dv.gt.dl*1.1d0) ierr=1
      if (p.gt.pc*10) ierr=1
      if (p.le.0) ierr=1
      if (ierr.ne.0 .and. herr(1:1).eq.' ') then
        herr='SATTP:  Iteration failed'
      endif
      if (ierr.gt.0 .and. iSatFlg2.eq.0) then
        irdxerr=ierr
        goto 10
      endif
      RETURN
      end                                              !subroutine SATTP
c
c ======================================================================
c
      subroutine SATSPLN (z,ierr,herr)
c
c  calculates the phase boundary of a mixture at a given composition,
c  and the critical point, cricondentherm, and cricondenbar.
c
c  inputs:
c        z--composition [array of mol frac]
c  outputs:
c     ierr--error flag:   0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  09-20-11 EWL, original version
c  12-01-12 EWL, change to density marching method
c
      include 'commons.for'
      dimension xPnts(1000),yPnts(1000),coefs(1000)
      dimension x(ncmax),y(ncmax),z(ncmax),slp(20)
      character herr*255
      common /GVtemp/itersgv

      ierr=0
      herr=' '
c     if (iSpline.ne.0) then
c       lsame2=.true.
c       do i=1,nc
c         if (abs(z(i)-xspln(i)).gt.1.d-6) lsame2=.false.
c       enddo
c       if (lsame2) goto 999
c     end if

      iSpline=0
      slp=0.d0
      call ISPURE (z,icomp)
      if (icomp.ne.0) goto 999
      call CRITP (z,tc,pc,rhoc,ierr,herr)
      call LIMITS ('EOS',z,tmin,tmax,Dmax,pmax)
      p=0.001d0
      t=300
      rho=0.d0

c  call RDXHMX to make sure that analytical derivatives are available
c  for the mixture
      call RDXHMX (1,1,0,Z,tr01,dr01,ierr,herr)
      if (ierr.gt.0 .and. heos.ne.'PR') goto 999


 5    continue
      call SATTP (t,p,z,4,0,d,rhox,rhoy,x,y,q,ierr,herr)
      if (ierr.gt.0 .or. t.lt.tmin*0.95d0) then
        p=p*10.d0
        if (p.lt.pmax) goto 5
        ierr=1
        herr='[SATSPLN error 1] saturation routine failed in '//
     &       'calculating initial state'
        goto 999
      endif

      if (tmin.gt.t) tmin=t
      iSatFlg2=1
      vf=1
      d=rhoy
      n=0
      isame=0
      dinc=0.1d0
      d0=1.d-12
 10   continue
        call THERM (t,rhoy,y,pp,e,h,s,cv,cp,w,hjt)
        n=n+1
        do i=1,nc
          xarr(i,n)=-500.d0
          if (x(i).gt.0.d0) xarr(i,n)=log(x(i))
        enddo
        xarr(   0,n)=log(rhoy)
        xarr(nc+1,n)=log(t)
        xarr(nc+2,n)=log(p)
        xarr(nc+3,n)=log(rhox)
        xarr(nc+4,n)=h
        xarr(nc+5,n)=s
        if (n.le.5) then
c  add several very low density states to spline arrays to catch low densities
          d0=d0*100.d0
          if (rhoy.gt.d0) then
            xarr(   0,n)=log(d0)
            xarr(nc+2,n)=log(d0*r*t)
            goto 10
          endif
        endif
        rhoyo=rhoy
        rhoxo=rhox
        iex=0
        slp(2:20)=slp(1:19)

 20     continue
        if (d.lt.dinc/10.d0) then
          d=d*10.d0
        else
          d=d+dinc
        endif
        if (d.lt.0.d0) goto 30

        rhoy=d
        if (n.gt.3) then
            xval=log(d)
            xPnts(1:3)=xarr(0,n-2-iex:n-iex)
            yPnts(1:3)=xarr(nc+3,n-2-iex:n-iex)
            call POLYN (3,xPnts,yPnts,xval,yval,ierr,herr)
          rhox=exp(yval)
            yPnts(1:3)=xarr(nc+1,n-2-iex:n-iex)
            call POLYN (3,xPnts,yPnts,xval,yval,ierr,herr)
          t=exp(yval)
            yPnts(1:3)=xarr(nc+2,n-2-iex:n-iex)
            call POLYN (3,xPnts,yPnts,xval,yval,ierr,herr)
          p=exp(yval)
c           yPnts(1:3)=xarr(nc+5,n-2-iex:n-iex)
c           call POLYN (3,xPnts,yPnts,xval,yval,ierr,herr)
c         s=yval
          do i=1,nc
            yPnts(1:3)=xarr(i,n-2-iex:n-iex)
            call POLYN (3,xPnts,yPnts,xval,yval,ierr,herr)
            xi=exp(yval)
            if (xi.gt.0) x(i)=xi
          enddo
          sumx=sum(x(1:nc))
          if (abs(sumx).gt.1.d-10) then
            x(1:nc)=x(1:nc)/sumx
          else
            x(1:nc)=y(1:nc)
          endif
        endif

c  density marching algorithm
        call SATGV (t,p,z,vf,d,1,6,0,rhox,rhoy,x,y,ierr,herr)
        if (ierr.ne.0) then
          call SATGV (t,p,z,vf,d,2,6,0,rhox,rhoy,x,y,ierr,herr)
          if (ierr.ne.0 .and. d.lt.1) then
            p=p*10.d0
            call SATTP (t,p,z,4,0,dd,rhox,rhoy,x,y,q,ierr,herr)
            if (ierr.eq.0) d=dd
          endif
        endif

        if (ierr.le.0) then
          if (rhoyo.ne.rhoy) s1=(rhoxo-rhox)/(rhoyo-rhoy)
          slp(1)=s1
          if (n.gt.10 .and. abs(s1).gt.0.1d0) then
            if (rhoy.lt.rhoc*1.3d0 .and. rhoy.gt.rhoc*0.7d0) then
              sa=SUM(slp(2:10))/9.d0  !Average of last several points
              if (s1*sa.gt.0 .and. abs(s1).lt.abs(sa)) then
                !Both points same sign, and new slope less than average
              elseif (abs(slp(2)).le.0.1d0) then
              elseif (abs((s1-slp(2))/slp(2)).gt.5d0) then
                ierr=1
              endif
            endif
          endif
        endif

c       if (ierr.eq.0) then
c         if (abs(rhox-rhoxo)/rhox.gt.0.2d0) then
c           if (rhox.lt.rhoc*2.d0 .and. rhox.gt.rhoc/2.d0) ierr=1
c         endif
c       endif

        if (abs(rhox-rhoy)/rhox.lt.0.01d0) then
          isame=isame+1
          if (isame.gt.3 .and. ierr.eq.0) ierr=1
        else
          isame=0
        endif

        if (ierr.gt.0) then
          if (iex.lt.15 .and. n.gt.20) then
            iex=iex+1
          else
            iex=0
          endif
          if (d.gt.rhoc*4.d0) goto 30
          if (t.gt.tmin*0.999d0 .and. n.lt.narm) goto 20
        endif
      if (t.gt.tmin*0.999d0 .and. n.lt.narm) goto 10



 30   continue
      narr=n
      if (narr.lt.20) then
        ierr=2
        herr='[SATSPLN error 2] saturation routine failed'
        narr=0
        goto 999
      endif


c  Create splines
      xPnts(1:narr)=xarr(0,1:narr)
      do k=1,nc+5
        yPnts(1:narr)=xarr(k,1:narr)
        call CSPLINE (narr-1,xPnts,yPnts,coefs)
        carr(k,1:narr)=coefs(1:narr)
      enddo
      iSpline=1
      xspln(1:nc)=z(1:nc)

c  cricondentherm (search for dtdx=0)
      dtdx=0.d0
      d=rhoc/4.d0
      call SPLNROOT (nc+1,1,dtdx,d,ierr,herr)
      call SPLNVAL  (nc+1,0,d,tmxt,ierr1,herr)
      call SPLNVAL  (nc+2,0,d,tmxp,ierr1,herr)
      tmxd=d
      ierrtmx=0
      if (ierr.gt.0) then
        ierrtmx=1
        tmxt=300
        tmxp=0
        tmxd=0
      endif

c  cricondenbar (search for dpdx=0)
      dpdx=0.d0
      d=rhoc
      call SPLNROOT (nc+2,1,dpdx,d,ierr,herr)
      if (ierr.gt.0) then
        d=rhoc/2.d0
        call SPLNROOT (nc+2,1,dpdx,d,ierr,herr)
      endif
      call SPLNVAL  (nc+1,0,d,pmxt,ierr1,herr)
      call SPLNVAL  (nc+2,0,d,pmxp,ierr1,herr)
      pmxd=d
      ierrpmx=0
      if (ierr.gt.0) then
        ierrpmx=1
        pmxt=300
        pmxp=0
        pmxd=0
      endif

c  critical point [the point at which x(i)=y(i) where i is the fluid with max x]
      i=1
      do j=1,nc
        if (z(j).gt.z(i)) i=j
      enddo
      y1=z(i)
      d=rhoc
      call SPLNROOT (   i,0,y1,d,ierr,herr)
      if (ierr.gt.0) then
        d=rhoc/2.d0
        call SPLNROOT (   i,0,y1,d,ierr,herr)
      endif
      call SPLNVAL  (nc+1,0,d,crtt,ierr1,herr)
      call SPLNVAL  (nc+2,0,d,crtp,ierr1,herr)
      crtd=d
      ierrcrt=0
      if (ierr.gt.0 .or. crtp.gt.50000) then
        ierrcrt=1
        crtt=tc
        crtp=pc
        crtd=rhoc
      endif

      ierr=0
      herr=' '
 999  continue
      iSatFlg2=0
      RETURN
      end                                            !subroutine SATSPLN
c
c ======================================================================
c
      subroutine GETXY (z,vf,philn,phi,x,y)
c
c  calculate the liquid and vapor compositions with the known values
c  of the overall composition, the vapor fraction, and the log of the
c  fugacity coefficients.
c
c  inputs:
c         z--overall composition [array of mol frac]
c        vf--vapor fraction (0>=vf>=1)
c            set vf=0 for liquid and vf=1 for vapor
c     philn--natural log of the fugacity coefficients [-]
c
c  outputs:
c       phi--fugacity coefficients [-]
c         x--liquid composition [array of mol frac]
c         y--vapor composition [array of mol frac]
c
c  written by G. Venkatarathnam, Indian Institute of Technology Madras
c  00-00-12  GV, original version
c  10-29-12 EWL, adapt code for use in REFPROP
c
      include 'commons.for'
      dimension z(ncmax),x(ncmax),y(ncmax),philn(ncmax+3),phi(ncmax)
      do i=1,nc
        phi(i)=EXP(philn(i))
        x(i)=z(i)
        xdiv=1.d0-vf+vf*phi(i)
        if (ABS(xdiv).gt.1.d-10 .and. xdiv.lt.1.d+10) x(i)=z(i)/xdiv
        y(i)=x(i)*phi(i)
      enddo
      sumx=SUM(x(1:nc))
      sumy=SUM(y(1:nc))
      if (ABS(sumx).gt.1.d-10) x(1:nc)=x(1:nc)/sumx
      if (ABS(sumy).gt.1.d-10) y(1:nc)=y(1:nc)/sumy
      RETURN
      end                                              !subroutine GETXY
c
c ======================================================================
c
      subroutine LGPHI (iderv,iphase,t,rho,p,x,philn,dpt,dpdrho,ddt,cp,
     &                  ierr,herr)
c
c  inputs:
c     iderv--set to 1 for calculation of fugacity coefficient (philn) only
c            set to 2 for full calculations (when calling PHIDERV) for use in other places
c            set to 3 to calculate p as function of t and rho,
c                     and for full calculations of everything else
c         t--temperature [K]
c       rho--density [mol/l] (if iderv=3)
c         p--pressure [kPa] (if iderv<>3)
c         x--composition [array of mol frac]
c    iphase--input phase, 1: liquid, 2: vapor,
c                         0: phase with minimum gibbs energy
c
c  outputs:
c    iphase--correct phase, computed using GV's pi criteria [2:vap,1:liq]
c       rho--density [mol/l] (input value should be an initial guess)
c         p--pressure [kPa] (if iderv=3)
c     philn--ln of fugacity coefficient [-]
c      ierr--error flag
c      herr--error string (character*255 variable if ierr<>0)
c
c  written by G. Venkatarathnam, Indian Institute of Technology Madras
c  00-00-12  GV, original version
c  10-29-12 EWL, adapt code for use in REFPROP
c
      include 'commons.for'
      dimension x(ncmax),philn(ncmax),phi(ncmax)
      dimension dadn(ncmax),dnadn(ncmax)
      character herr*255

      call CRITP (x,tc,pc,rhoc,ierr,herr)
      if (iderv.eq.3) then
        call PRESS (t,rho,x,p)
      else
        if (iphase.ne.0) then
          call TPRHO (t,p,x,iphase,1,rho,ierr,herr)
          if (ierr.ne.0) call TPRHO (t,p,x,3-iphase,0,rho,ierr,herr)
          if (rho.eq.0.d0) rho=1.d-20
        else  ! phase with minimum gibbs energy
          grv=1.d36  !no convergence
          grl=1.d36
          call TPRHO (t,p,x,1,0,rhol,ierr,herr)
          if (ierr.le.0) call GIBBS (t,rhol,x,arl,grl)
          call TPRHO (t,p,x,2,0,rhov,ierr,herr)
          if (ierr.le.0) call GIBBS (t,rhov,x,arv,grv)
          rho=rhol
          if (grv.le.grl) rho=rhov
          if (rho.eq.0.d0) rho=1.d-20
        endif
      endif
      if (rho.lt.0.d0) rho=rhoc+1.d-20

      call THERM2 (t,rho,x,pp,e,h,s,cv,cp,w,z,hjt,a,g,
     &                   xkappa,beta,dpdrho,d2pdd2,dpt,ddt,drhodp,
     &                   d2pt2,d2pdtd,spare3,spare4)
      if (iderv.eq.1) then
        call FUGCOF (t,rho,x,phi,ierr,herr)
        philn(1:nc)=log(phi(1:nc))
      else
        call PHIDERV (iderv,t,rho,x,dadn,dnadn,ierr,herr)
        if (ierr.gt.0) RETURN
        RTrho=R*t*rho
        do i=1,nc
          phi(i)=1.d0
          if (ABS(dnadn(i)).lt.100.0d0) then
c  Correct calculation of log(phi) is:
c           if (p.gt.0) philn(i)=DLOG(RTrho/p)+dnadn(i)
c  but use this instead since p can be negative, and in the solution
c  of VLE the p's must be equal, so they cancel out.  The 1.d3 keeps the
c  value of RTrho on the same scale as dnadn (and also cancels out),
            philn(i)=DLOG(RTrho/1.d3)+dnadn(i)
          endif
        enddo
      endif
      if (dpt.ne.0.d0 .and. dpdrho.ne.0.d0)
     &    pifactor=2.d0-rho*(d2pdtd/dpt-d2pdd2/dpdrho)
      iphase=2
      if (pifactor.gt.1.d0) iphase=1
      if (rho.gt.rhoc*2.d0) iphase=1  !Special case for mixtures with water where pifactor<0 in the liquid
      RETURN
      end                                              !subroutine LGPHI
c
c ======================================================================
c
      subroutine CRTPNT (z,tc,pc,rhoc,ierr,herr)
c
c     Subroutine for the determination of true critical point of a
c     mixture using the Method of Michelsen (1984)
c
c     The routine requires good initial guess values of pc and tc.
c     On convergence, the values of bb and cc should be close to zero
c     and dd > 0 for a two-phase critical point.
c     bb=0, cc=0 and dd <= 0 for an unstable critical point.
c
c  inputs:
c         z--composition [array of mol frac]
c
c  outputs:
c        tc--critical temperature [K]
c        pc--critical pressure [kPa]
c      rhoc--critical density [mol/l]
c      ierr--error flag
c      herr--error string (character*255 variable if ierr<>0)
c
c  written by G. Venkatarathnam, Indian Institute of Technology Madras
c  10-15-11  GV, original version
c  01-07-13 EWL, adapt code for use in REFPROP
c
      include 'commons.for'
      dimension z(ncmax),y(ncmax),yn(ncmax),v(ncmax),g(ncmax),u(ncmax)
      dimension a(ncmax,ncmax),dfds(2),dpht(ncmax),dphp(ncmax)
      dimension phy(ncmax),phz(ncmax),dft2(2),dfp2(2)
      character*255 herr

      ierr=0
      herr=' '
      eps=1.d-4        ! Numerical perturbation of z
      call ISPURE (z,icomp)
      if (icomp.ne.0) then
        call CRITP (z,tc,pc,rhoc,ierr,herr)
        RETURN
      endif

 10   continue
      v(1:nc)=DSQRT(z(1:nc))
      pc=abs(pc)
      tol=1.d-8
      iz=1
      do k=1,20
        call LGPHI (2,iz,tc,rhoc,pc,z,phz,a1,a2,a3,a4,ierr,herr)
        if (ierr.gt.0) RETURN
        if (k.gt.1 .and. ABS(bb)+abs(cc).lt.tol) then
c  convergence obtained, return
c           write (*,*) k
          RETURN
        endif

        do i=1,nc
          do j=i,nc
            a(i,j)=v(i)*v(j)*dphidnj(i,j)
            a(j,i)=a(i,j)
          enddo
          a(i,i)=a(i,i)+1.d0
        enddo
        xlamda=0.d0
        call MINEIGEN (nc,a,u,xlamda,ierr,herr)

        if (k.eq.10) tol=tol*100
        dpht(1:nc)=dphidt(1:nc)
        dphp(1:nc)=dphidp(1:nc)
        do j=1,2
          rhoy=rhoc
          s=eps
          if (j.eq.1) s=-s
          y(1:nc)=ABS(s*u(1:nc)*v(1:nc)+z(1:nc))
          sumy=SUM(y(1:nc))
          if (ABS(sumy).gt.1.d-10) yn(1:nc)=y(1:nc)/sumy
          call LGPHI (2,iz,tc,rhoy,pc,yn,phy,a1,a2,a3,a4,ierr,herr)
          g(1:nc)=DLOG(y(1:nc)/z(1:nc))+phy(1:nc)-phz(1:nc)
          dfds(j)=SUM(v(1:nc)*u(1:nc)*g(1:nc))
          dft2(j)=SUM(v(1:nc)*u(1:nc)*(dpht(1:nc)-dphidt(1:nc)))
          dfp2(j)=SUM(v(1:nc)*u(1:nc)*(dphp(1:nc)-dphidp(1:nc)))
        enddo

        bb=xlamda/2.d0
        bp=(dfp2(2)-dfp2(1))/eps/4.d0
        bt=(dft2(2)-dft2(1))/eps/4.d0
        cc=(dfds(2)+dfds(1))/eps**2/6.d0
        cp=(dfp2(2)+dfp2(1))/eps**2/6.d0
        ct=(dft2(2)+dft2(1))/eps**2/6.d0

        denom=(bp*ct-bt*cp)
        if (abs(denom).lt.1.d-20) then
          ierr=1
          herr='dp, dt -> infinity. Provide better p, t guess values'
          RETURN
        endif
        dp=(cc*bt-bb*ct)/denom
        dt=(cp*bb-cc*bp)/denom
c       write (*,*) dt,dp
c       write (*,*) bb,cc
        do i=1,25
          if (ABS(dt).gt.2 .or. ABS(dp).gt.1000) then
            dt=dt/10.d0
            dp=dp/2.d0
          else
            EXIT
          endif
        enddo
        tc=tc-dt
        pc=pc-dp
        if (k.ge.20 .or. pc.lt.0.d0 .or. tc.lt.0.d0 .or.
     &      pc.gt.100.d3 .or. tc.gt.1.d3) then
          if (eps.lt.0.099d0) then
            call CRITP (z,tc,pc,rhoc,ierr,herr)
            eps=eps*10.d0
c           write (*,*) eps
            goto 10
          endif
          ierr=1
          herr='No convergence. Provide better p, t guess values'
          RETURN
        endif
      enddo
      end                                             !subroutine CRTPNT
c
c ======================================================================
c
      subroutine SATGV (t,p,z,vf,b,ipv,ityp,isp,rhox,rhoy,x,y,ierr,herr)
c
c  Calculates the bubble or dew point state using the entropy or density method
c  of GV.  The caculation method is similar to the volume based algorithm of GERG.
c  The cricondenbar and cricondentherm are estimated using the method in:  M.L.
c  Michelsen, Saturation point calculations, Fluid Phase Equilibria, 23:181, 1985.
c
c  inputs:
c         t--temperature [K]
c         p--pressure [kPa]
c         z--overall composition [array of mol frac]
c        vf--vapor fraction (0>=vf>=1)
c            set vf=0 for liquid and vf=1 for vapor
c            for ityp=6, vf=1 assumes x is liquid and y is vapor,
c                    and vf=0 assumes y is liquid and x is vapor
c         b--input value, either entropy [J/mol-K] or density [mol/l]
c       ipv--pressure or volume based algorithm
c            1 -> pressure based
c            2 -> volume based
c      ityp--input values
c            0 -> given p, calculate t
c            1 -> given t, calculate p
c            2 -> cricondentherm condition, calculate t,p (ipv=1 only)
c            3 -> cricondenbar condition, calculate t,p (ipv=1 only)
c            5 -> given entropy, calculate t,p
c            6 -> given density, calculate t,p
c       isp--use values from Splines as initial guesses if set to 1
c
c  outputs: (initial guesses must be sent in all variables (unless isp=1))
c         t--temperature [K]
c         p--pressure [kPa]
c      rhox--density of x phase [mol/l]
c      rhoy--density of y phase [mol/l]
c         x--composition of x array [array of mol frac]
c         y--composition of y array [array of mol frac]
c      ierr--error flag:  0 = successful
c                         1 = LUdecomp failed
c                         2 = derivatives are not available in RDXHMX
c                        71 = no convergence
c                        72 = log values too large
c                        73 = p or T out of range
c                        74 = trival solution
c                        75 = unacceptable F
c                        76 = False roots
c                        77 = density out of range
c                        80 = vf < 0 or vf > 1
c                        81 = sum(z)<>1
c                        82 = input rho<=0
c      herr--error string (character*255 variable if ierr<>0)
c
c
c  equations to be solved simultaneously are:
c  --pressure based:
c      f(1:n)=log(y/x)-log((fxi/nxi)/(fyi/nyi))=0
c      f(n+1)=sum(y(i)-x(i))=0
c      f(n+2)=b/binput-1=0, where b = p, t, d, or s
c
c  --volume based:
c      f(1:n) - log(y/x)-log((fxi/nxi)/(fyi/nyi))=0
c      f(n+1) - sum(y(i)-x(i))=0
c      f(n+2) - py=px
c      f(n+3) - b/binput-1=0, where b = p, t, d, or s
c
c  variables:
c     1 to nc - log(k(i))
c     nc+1    - log(t)
c     nc+2    - log(p) or log(rhox)
c     nc+3    -           log(rhoy)
c
c  written by G. Venkatarathnam, Indian Institute of Technology Madras
c  00-00-12  GV, original version
c  10-29-12 EWL, adapt code for use in REFPROP
c
      include 'commons.for'
      character*255 herr,hr
      dimension xjacob(50,50),f(50)
      dimension phx(ncmax),phy(ncmax),phz(ncmax),vrb(ncmax+3)
      dimension x(ncmax),y(ncmax),z(ncmax),phi(ncmax),vrb0(ncmax+3)
      dimension xo(ncmax),yo(ncmax)
      dimension dfxdp(ncmax),dfxdt(ncmax),dfxdnj(ncmax,ncmax)
      dimension dfydp(ncmax),dfydt(ncmax),dfydnj(ncmax,ncmax)
      dimension dfxdv(ncmax),dfydv(ncmax)

c     dimension x2(ncmax),y2(ncmax),tjcb(10,10)
c     dimension xincr(ncmax+3),phz1(ncmax),phx1(ncmax),phy1(ncmax)

      common /GVcom/ ixp,iyp,izp
      common /GVtemp/iters


      ierr=0
      herr=' '
      xo(1:nc)=x(1:nc)
      yo(1:nc)=y(1:nc)
      nvars=nc+2                ! Number of variables
      il=2
      if (ipv.eq.2) then
        il=3
        nvars=nc+3
      endif
      cf=1.d0-vf
      error2=1.d-12
      p1=1.d0
!c  use less strict convergence tolerance for cricondenbar and cricondentherm calculations
!      if (ityp.eq.2) error2=1.d-12
!      if (ityp.eq.2) error3=1.d-5
!c  cricondenbar can occur very close to critical point. Lower convergence criteria is therefore used.
!      if (ityp.eq.3) error2=1.d-12
!      if (ityp.eq.3) error3=1.d-4

c  get values from splines if already set up
      if (iSpline.ne.0 .and. isp.ne.0) then
        rhoin=0.1d0
        if (vf.eq.0) then
          call SPLNVAL(0,-2,t,rhoin,ierr,herr)
          rhoin=rhoin*0.9d0
        endif
        if (ityp.eq.0) call SPLNROOT (nc+2,0,p,rhoin,ierr,herr)
        if (ityp.eq.1) call SPLNROOT (nc+1,0,t,rhoin,ierr,herr)
        if (ityp.eq.5) call SPLNROOT (nc+5,0,b,rhoin,ierr,herr)
        if (ityp.eq.6) rhoin=b
        if (vf.eq.1) y(1:nc)=z(1:nc)
        if (vf.eq.0) x(1:nc)=z(1:nc)
        if (vf.eq.1) rhoy=rhoin
        if (vf.eq.0) rhox=rhoin
        do j=1,nc+3
          call SPLNVAL (j,0,rhoin,vout,ierr,herr)
          if (ierr.eq.0) then
            vout=abs(vout)
            if (j.le.nc) then
              if (vout.lt.1.d-100) vout=0.d0
              if (vf.eq.1) x(j)=vout
              if (vf.eq.0) y(j)=vout
            elseif (j.eq.nc+1) then
              if (ityp.ne.1) t=vout
            elseif (j.eq.nc+2) then
              if (ityp.ne.0) p=vout
            elseif (j.eq.nc+3) then
              if (vf.eq.1) rhox=vout
              if (vf.eq.0) rhoy=vout
            endif
          endif
        enddo
        sumx=sum(x(1:nc))
        sumy=sum(y(1:nc))
        if (abs(sumx).gt.1.d-10) then
          x(1:nc)=x(1:nc)/sumx
        else
          x=y
        endif
        if (abs(sumy).gt.1.d-10) then
          y(1:nc)=y(1:nc)/sumy
        else
          y=x
        endif
      endif

c  check input for errors
      sumz=sum(z(1:nc))
      if (vf.gt.1.d0 .or. vf.lt.0.d0) then
        ierr=80
        herr='[SATGV error 80] vf greater than 1 or less than 0'
      elseif (dabs(sumz-1.d0).ge.1.d-8) then
        ierr=81
        herr='[SATGV error 81] Sum of feed composition not equal to 1.0'
      elseif (rhox.le.0.d0 .or. rhoy.le.0.d0) then
        ierr=82
        herr='[SATGV error 82] Input density less than or equal to 0.0'
      endif
      if (ierr.gt.0) goto 999

c  use values for ixyzphase from previous call, unless rho is >2*rhoc or <rhoc/10
      call CRITP (y,tc,pc,rhoc,ierr,herr)
      if (vf.eq.0) call CRITP (x,tc,pc,rhoc,ierr,herr)
      rho=vf*rhoy+cf*rhox
      if (rhox.gt.rhoc*2.d0 .or. rhoy.lt.0.1d0)  ixp=1
      if (rhoy.gt.rhoc*2.d0 .or. rhox.lt.0.1d0)  iyp=1
      if (rho .gt.rhoc*2.d0) izp=1
      if (rho .lt.rhoc/5.d0) izp=2
      if (rhox.lt.rhoc/5.d0) ixp=2
      if (rhoy.lt.rhoc/5.d0) iyp=2

c  set up variables in equations to be solved
c  calculate initial log of K values from input x and y
      do i=1,nc
        vrb(i)=1.d0
        if (x(i).gt.0.d0 .and. y(i).gt.0.d0) vrb(i)=log(y(i)/x(i))
      enddo
      vrb(nc+1)=log(t)
      vrb(nc+2)=log(p)
      rhox1=rhox
      rhoy1=rhoy
      if (ipv.eq.2) then
        vrb(nc+2)=log(rhox1)
        vrb(nc+3)=log(rhoy1)
      endif
      if (ityp.eq.0) s=log(p)   !log(P)
      if (ityp.eq.0 .and. ipv.eq.2) s=p    !P
      if (ityp.eq.1) s=log(t)   !log(T)
      if (ityp.eq.5) s=b        !entropy
      if (ityp.eq.6) s=log(b)   !log(rho)

      iters=0
      do while (iters.lt.20)
        iters=iters+1
        t1=exp(vrb(nc+1))
        if (ipv.ne.2) then
          p1=exp(vrb(nc+2))
        elseif (ipv.eq.2) then
          rhox1=exp(vrb(nc+2))
          rhoy1=exp(vrb(nc+3))
        endif

c  check if the guess values for the next iteration are reasonable
        do i=1,nc
          if (dabs(vrb(i)).gt.100) then
            ierr=72
            write (herr,1072) ierr,i,vrb(i)
          endif
        enddo
        if (t1.gt.2000 .or. t1.lt.2) then !Temperature
          ierr=73
          write (herr,1073) ierr,p1,t1
        elseif (p1.gt.500000 .and. ipv.eq.1) then  !Pressure
          ierr=73
          write (herr,1073) ierr,p1,t1
        elseif (rhox1.gt.100 .and. ipv.eq.2) then  !Density
          ierr=77
          write (herr,1077) ierr,rhox1,t1
        elseif (rhoy1.gt.100 .and. ipv.eq.2) then  !Density
          ierr=77
          write (herr,1077) ierr,rhoy1,t1
        endif
        if (ierr.gt.0) goto 999

c  call routine to analytically get all derivatives (including A with respect to n and x)
        call GETXY (z,vf,vrb,phi,x,y)
        call LGPHI(il,ixp,t1,rhox1,p1,x,phx,dpxt,dpxd,dtx,cpx,ierr,herr)
        if (ierr.gt.0) RETURN
        if (ipv.ne.2) then
          dfxdnj(1:nc,1:nc)=dphidnj(1:nc,1:nc)
          dfxdt(1:nc)=t1*dphidt(1:nc)
          dfxdp(1:nc)=p1*dphidp(1:nc)
        else
          if (heos.eq.'PR') then
            ierr=99
            herr='Derivatives not yet available for PR'
            RETURN
          endif
          dfxdnj(1:nc,1:nc)=d2adnn(1:nc,1:nc)
          dfxdt(1:nc)=t1*dlnfinidt(1:nc)
          dfxdv(1:nc)=   dlnfinidv(1:nc)
          px1=p1
        endif
        call LGPHI (il,iyp,t1,rhoy1,p1,y,phy,dpyt,dpyd,dty,cpy,i,herr)
        if (ipv.ne.2) then
          dfydnj(1:nc,1:nc)=dphidnj(1:nc,1:nc)
          dfydt(1:nc)=t1*dphidt(1:nc)
          dfydp(1:nc)=p1*dphidp(1:nc)
        else
          dfydnj(1:nc,1:nc)=d2adnn(1:nc,1:nc)
          dfydt(1:nc)=t1*dlnfinidt(1:nc)
          dfydv(1:nc)=   dlnfinidv(1:nc)
          py1=p1
        endif

c  set up f matrix with variables in equations to be solved
        f(1:nc)=phy(1:nc)-phx(1:nc)+vrb(1:nc)                          !ln(fugacity coefficients)=log(y/x) [which equals: ln(x*phix/(y*phiy))=0]
        f(nc+1)=sum(z(1:nc)*(phi(1:nc)-1.d0)/(cf+vf*phi(1:nc)))            !sum(y-x)=0
        if (ipv.eq.2) f(nc+2)=(px1-py1)/1000.d0
        if (ityp.eq.0) then      !pressure input
          if (p1.gt.0.) f(nvars)=log(p1)/s-1.d0       !****works with vf=0,1 for volume based?
          if (ipv.eq.2) f(nvars)=s-(vf*px1+cf*py1)
        elseif (ityp.eq.1) then  !temperature input
          f(nvars)=log(t1)/s-1.d0
        elseif (ityp.eq.5) then  !entropy input
          call ENTRO (t1,rhox1,x,sx)
          call ENTRO (t1,rhoy1,y,sy)
          f(nvars)=(vf*sy+cf*sx)/s-1.d0
        elseif (ityp.eq.6) then  !density input
          s1=vf*rhoy1+cf*rhox1
          f(nvars)=log(s1)/s-1.d0
!        elseif (ityp.eq.2 .or. ityp.eq.3) then  !cricondentherm/bar input
!          izp=0
!          call LGPHI (2,izp,t1,rhoz1,p1,z,phz,d1,d2,d3,d4,ierr,herr)
!          term1=0.d0
!          term2=0.d0
!          do i=1,nc
!            if (ityp.eq.2) then
!              term1=term1+x(i)*(dfxdp(i)/p1-dphidP(i))
!              term2=term2+y(i)*(dfydp(i)/p1-dphidP(i))
!            else
!              term1=term1+x(i)*(dfxdt(i)/t1-dphidT(i))
!              term2=term2+y(i)*(dfydt(i)/t1-dphidT(i))
!            endif
!          enddo
!          f(nvars)=cf*term2+vf*term1
        endif

c  Determine Jacobian of funtions f(1) to f(n+3) using analytical derivatives
        xjacob(1:nvars,1:nvars)=0.d0
        if (ipv.eq.2) then
          xjacob(nc+2,nc+1)=t1*(dpxt-dpyt)/1000.d0
          xjacob(nc+2,nc+2)= rhox1*dpxd/1000.d0
          xjacob(nc+2,nc+3)=-rhoy1*dpyd/1000.d0
        endif
        do i=1,nc
          zeta=z(i)*phi(i)/(cf+vf*phi(i))**2                             !=y(i) for vf=0 and x(i) for vf=1
          xjacob(nc+1,i)=zeta                                          !d(sum(y-x))/d(ln(Ki))
          xjacob(1:nc,i)=zeta*(cf*dfydnj(1:nc,i)+vf*dfxdnj(1:nc,i))    !d(ln(fy/fx))/d(ln(Ki))
          xjacob(i,i)   =1.d0+xjacob(i,i)
          xjacob(i,nc+1)=dfydt(i)-dfxdt(i)                     !d(ln(fy/fx))/d(ln(T))
          if (ipv.ne.2) xjacob(i,nc+2)=dfydp(i)-dfxdp(i)                     !d(ln(fy/fx))/d(ln(P))
          if (ipv.eq.2) then
            xjacob(nc+2,i)=R*t1*zeta*(cf*dfydv(i)+vf*dfxdv(i))/1000.d0
            if (ityp.eq.0)
     &      xjacob(nc+3,i)=R*t1*zeta*(cf*dfydv(i)-vf*dfxdv(i))
            xjacob(i,nc+2)= dfxdv(i)/rhox1                             !d(ln(fy/fx))/d(ln(rhox))
            xjacob(i,nc+3)=-dfydv(i)/rhoy1                             !d(ln(fy/fx))/d(ln(rhoy))
          endif
        enddo

c  set up the last section in the Jacobian for the input variables
        if (ityp.eq.1) then          !temperature
          xjacob(nvars,nc+1)=1.d0/s
        elseif (ipv.ne.2) then       !pressure based method
          if (ityp.eq.0) then        !pressure
            xjacob(nvars,nvars)=1.d0/s
          elseif (ityp.eq.5) then    !entropy
            xjacob(nvars,nc+1)=(vf*cpy+cf*cpx)/s   !d(ln(s))/d(ln(T))
            xjacob(nvars,nc+2)=p1*(vf*dty/rhoy1**2+cf*dtx/rhox1**2)/s    !d(ln(s))/d(ln(P))
            if (vf.gt.0.d0 .and. vf.lt.1.d0) then
              do j=1,nc
                zeta=z(j)*phi(j)*cf*vf/(cf+vf*phi(j))**2
                dsdyi=-R*(log(y(j))+phy(j)+dfydt(j))
                dsdxi= R*(log(x(j))+phx(j)+dfxdt(j))
                xjacob(nvars,j)=zeta*(dsdyi+dsdxi)/s
              enddo
            endif
          elseif (ityp.eq.6) then     !density
            xjacob(nvars,nc+1)=t1*(cf* dtx/rhox1+vf* dty/rhoy1)/s      !d(ln(rho))/d(ln(T))
            xjacob(nvars,nc+2)=p1*(cf/dpxd/rhox1+vf/dpyd/rhoy1)/s      !d(ln(rho))/d(ln(P))
            if (vf.gt.0.d0 .and. vf.lt.1.d0) then
              ierr=81
              herr='SATGV not set up for vf>0 and vf<1 with ityp=6'
              goto 999
            endif
          endif
        else
          if (ityp.eq.0) then        !pressure
            xjacob(nvars,nc+1)=-t1*(vf*dpxt+cf*dpyt)
            xjacob(nvars,nc+2)=-vf*rhox1*dpxd
            xjacob(nvars,nc+3)=-cf*rhoy1*dpyd
          elseif (ityp.eq.5) then    !Not implemented for entropy
          elseif (ityp.eq.6) then    !density
            xjacob(nvars,nc+3)=vf*1.d0/s
            xjacob(nvars,nc+2)=cf*1.d0/s
          endif
        endif

c  uncomment the following lines to test the calculation of Jacobian elements
c  by numerical perturbation of variables vrb on the functions:  d(f(1..nc+3))/d(vrb)
!        do i = 1,nc
!          xincr(i)=1.d-4          ! delta ln(Ki)
!        enddo
!        xincr(nc+1)=1.d-4         ! delta t
!        xincr(nc+2)=1.d-4         ! delta p or delta rhox
!        xincr(nc+3)=1.d-4         !            delta rhoy
!        do i=1,nvars
!          phio=exp(vrb(i))  !old value of fugacity coef.
!          vrb(i)=vrb(i)+xincr(i)
!          t2=exp(vrb(nc+1))
!          p2=exp(vrb(nc+2))
!          if (ipv.ne.2) then
!            call GETXY (z,vf,vrb,phi,x2,y2)
!            rhox2=rhox1
!            rhoy2=rhoy1
!            call LGPHI(2,iyp,t2,rhoy2,p2,y2,phy,d1,d2,d3,d4,ierr,herr)
!            call LGPHI(2,ixp,t2,rhox2,p2,x2,phx,d1,d2,d3,d4,ierr,herr)
!          else
!            call GETXY (z,vf,vrb,phi,x2,y2)
!            rhox2=exp(vrb(nc+2))
!            rhoy2=exp(vrb(nc+3))
!            if (i.le.nc) then
!c             phi(1:nc)=exp(vrb(1:nc))
!c             x2(1:nc)=z(1:nc)/(1.d0-vf+vf*phi(1:nc))
!c             y2(1:nc)=x2(1:nc)*phi(1:nc)
!              dxki=phi(i)-phio
!              if (vf.eq.1) then
!c               dnxdn=-dxki*x(i)/(1.d0-x(i))/phio
!                dnxdn=-x(i)**2/y(i)*dxki  !This is not working quite right
!                rhox2=(1.d0+dnxdn)*rhox2
!              else
!                dnydn=dxki*x(i)/(1.d0-y(i))
!                rhoy2=(1.d0+dnydn)*rhoy2
!              endif
!            endif
!           call LGPHI(3,iyp,t2,rhoy2,py2,y2,phy,d1,d2,d3,d4,ierr,herr)
!           call LGPHI(3,ixp,t2,rhox2,px2,x2,phx,d1,d2,d3,d4,ierr,herr)
!          endif
!          do j=1,nvars
!            if (j.le.nc) then
!              tjcb(j,i)=(phy(j)-phx(j)+vrb(j)-f(j))/xincr(i)
!            elseif (j.eq.nc+1) then
!              sumx=sum(z(1:nc)*(phi(1:nc)-1.d0)/(cf+vf*phi(1:nc)))
!              tjcb(j,i)=(sumx-f(j))/xincr(i)
!            elseif (j.eq.nvars) then
!              if (ipv.eq.2) then
!                p2=px2
!                if (i.eq.nvars .and. j.eq.nvars) p2=py2
!              endif
!              if (ityp.eq.0) tjcb(j,i)=(p2/s-1.d0-f(j))/xincr(i)
!              if (ityp.eq.0 .and. ipv.eq.2)
!     &                       tjcb(j,i)=(s-p2-f(j))/xincr(i)
!              if (ityp.eq.1) tjcb(j,i)=(log(t2)/s-1.d0-f(j))/xincr(i)
!              if (ityp.eq.5) then
!                call ENTRO (t2,rhox2,x2,sx)
!                call ENTRO (t2,rhoy2,y2,sy)
!                tjcb(j,i)=((vf*sy+cf*sx)/s-1.d0-f(j))/xincr(i)
!              elseif (ityp.eq.6) then
!                tjcb(j,i)=((vf*log(rhoy2)+cf*log(rhox2))/s
!     &                   -1.d0-f(j))/xincr(i)
!              endif
!            elseif (j.eq.nc+2) then
!              tjcb(j,i)=((px2-py2)/1000.d0-f(j))/xincr(i)
!            endif
!            if (ipv.eq.2 .and. rhoy.gt.5.6 .and. rhoy.lt.5.65) then
!              abc=1    !Use for break points
!            endif
!          enddo
!          vrb(i)=vrb(i)-xincr(i)
!        enddo





!c  calculation of jacobian by numerical perturbation of
!c  variables (x,t) of functions f(x,t)
!        if (ityp.eq.2 .or. ityp.eq.3) then
!          dt=0
!          dp=0
!          if (ityp.eq.3) dt=1.d-4
!          if (ityp.eq.2) dp=1.d-4
!          dd=dt+dp
!          xincr(1:nc)=1.d-5
!          xincr(nc+1)=1.d-4          ! K
!          xincr(nc+2)=1.d-4          ! kPa
!          do i=1,nvars
!            vrb(i)=vrb(i)+xincr(i)
!            call GETXY (z,vf,vrb,phi,x,y)
!            p1=exp(vrb(nc+2))
!            t1=exp(vrb(nc+1))
!            rhox2=rhox1
!            rhoy2=rhoy1
!            iz=0
!            call LGPHI (1,ixp,t1,rhox2,p1,x,phx,d1,d2,d3,d4,j,herr)
!            call LGPHI (1,iyp,t1,rhoy2,p1,y,phy,d1,d2,d3,d4,j,herr)
!            call LGPHI (1,iz,   t1,rhoz2,p1,z,phz,d1,d2,d3,d4,j,herr)
!            iz=0
!           call LGPHI(1,ixp,t1+dt,rhox2,p1+dp,x,phx1,d1,d2,d3,d4,j,hr)
!           call LGPHI(1,iyp,t1+dt,rhoy2,p1+dp,y,phy1,d1,d2,d3,d4,j,hr)
!           call LGPHI(1,iz,   t1+dt,rhoz2,p1+dp,z,phz1,d1,d2,d3,d4,j,hr)
!            sumx=0.d0
!            do k=1,nc
!              fuggy=(phy1(k)-phy(k))/dd
!              fuggx=(phx1(k)-phx(k))/dd
!              fuggz=(phz1(k)-phz(k))/dd
!              sumx=sumx+cf*y(k)*(fuggy-fuggz)+vf*x(k)*(fuggx-fuggz)
!            enddo
!            xjacob(nc+2,i)=(sumx-f(nc+2))/xincr(i)
!            vrb(i)=vrb(i)-xincr(i)
!          enddo
!        endif

c  check for trivial solution
        itest=0
        do i=1,nc
          if (dabs(vrb(i)).le.1.d-6) itest=itest+1
        enddo
        if (itest.eq.nc) then
          ierr=74
          write (herr,1074) ierr,(vrb(i),i=1,nc)
          goto 999
        endif

c  solve linearized equations by LU decomposition
c  f is the solution vector [xnew-xold]
        f(1:nvars)=-f(1:nvars)
        call LUdecomp (nvars,50,xjacob,f,ierr,herr)
        if (ierr.ne.0) goto 999
        vrb0(1:nvars)=vrb(1:nvars)
        vrb(1:nvars)=vrb(1:nvars)+f(1:nvars)
        sum0=DOT_PRODUCT(f(1:nvars),f(1:nvars))/nvars





        if (ipv.eq.2) then
          iter1=0
11        continue
          tt=t1
          t1=exp(vrb(nc+1))
          rhox1=exp(vrb(nc+2))
          rhoy1=exp(vrb(nc+3))
          call GETXY (z,vf,vrb,phi,x,y)
          call PRESS (t1,rhox1,x,px2)
          call PRESS (t1,rhoy1,y,py2)
          pp=(cf*py2+vf*px2)
          pp1=(cf*py1+vf*px1)
          if (pp.le.0 .or. pp/pp1.gt.100 .or. pp/pp1.lt.0.01 .or.
     &        t1/tt.lt.0.9 .or. t1/tt.gt.1.1) then
            f(1:nvars)=f(1:nvars)/2.d0
            vrb(1:nvars)=vrb0(1:nvars)+f(1:nvars)
            iter1=iter1+1
            if (iter1.lt.10) goto 11
          endif
        endif


        lconv=sum0.le.error2
!        if (ityp.eq.2 .or. ityp.eq.3) then
!          sum1=DOT_PRODUCT(f(1:nc+1),f(1:nc+1))/(nc+1)
!          lconv=sum1.le.error2 .and. dabs(f(nvars)).le.error3
!        endif
        if ((lconv).and.iters.ne.1) then
          if (ityp.ne.1) t=exp(vrb(nc+1))
          if (ityp.ne.0) p=exp(vrb(nc+2))
          call GETXY (z,vf,vrb,phi,x,y)
          if (ipv.ne.2) then
            rhox=rhox1
            rhoy=rhoy1
            call TPRHO (t,p,x,ixp,1,rhox,ierr,herr)
            if (ierr.ne.0) call TPRHO (t,p,x,3-ixp,0,rhox,ierr,herr)
            call TPRHO (t,p,y,iyp,1,rhoy,ierr,herr)
            if (ierr.ne.0) call TPRHO (t,p,y,3-iyp,0,rhoy,ierr,herr)
          else
            rhox=exp(vrb(nc+2))
            rhoy=exp(vrb(nc+3))
            if (ityp.ne.0) then
              call PRESS (t,rhox,x,px1)
              call PRESS (t,rhoy,y,py1)
              p=(px1+py1)/2.d0
            endif
          endif
          itest=0
          do i=1,nc+1
            if (dabs(f(i)).le.1.d-5) itest=itest+1
          enddo
          if (itest.ne.nc+1) then
            ierr=75
            write (herr,1075) ierr,iters,(f(i),i=1,nvars)
          endif
          if (ixp.eq.2 .and. iyp.eq.2) then
            ierr=76
            herr='[SATGV error 76] False roots. ixphase=iyphase=2'
          endif
          if (ierr.gt.0) goto 999
          RETURN
        endif
      enddo

      ierr=71
      write (herr,1071) ierr,iters,sum0
 999  continue
      call ERRMSG (ierr,herr)
      x(1:nc)=xo(1:nc)
      y(1:nc)=yo(1:nc)
      RETURN

 1071 format('[SATGV error ',i2,'] Routine did not converge in ',i2,
     &       ' iterations.  Sum = ',e11.4)
 1072 format('[SATGV error ',i2,'] ln(',i1,') value too high: ',e11.4)
 1073 format('[SATGV error ',i2,'] Pressure or Temperature outside ',
     &       'bounds.  Pressure = ',e11.4,' Temperature = ',e11.4)
 1074 format('[SATGV error ',i2,'] Trivial solution.  ',
     &       'ln(ki) = ',20(e9.2,1x))
 1075 format('[SATGV error ',i2,'] Converged in ',i2,' iterations.  ',
     &       'But F < acceptable. ', 20(e9.2,1x))
 1077 format('[SATGV error ',i2,'] Density outside ',
     &       'bounds.  Density = ',e11.4,' Temperature = ',e11.4)
      end                                              !subroutine SATGV
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                     end file sat_sub.f
c ======================================================================
