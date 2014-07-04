c  begin file trns_ECS.f
c
c  This file contains the routines implementing the extended corresponding
c  states (ECS) method for the transport properties.
c
c  contained here are:
c     subroutine TRNECS (t,rho,x,eta,tcx,ierr,herr)
c     subroutine TCBKMX (t,rho,x,fj,fx,hj,hx,tcx,Flam,lerrt,lerrD,
c    &                   ierr,herr,irefn)
c     subroutine SETTRN (nread,icomp,hcasno,hreff,heoss,hvs,htc,
c    &                   ierr,herr)
c     function FINT (icomp,t)
c     function PSI (icomp,tr,rhor)
c     function CHI (icomp,tr,rhor)
c     subroutine TCKVIR (icomp,t,tcx0,tcxcol,tcxvir,ierr,herr)
c     subroutine ECSLIM (t,D,tmin,tmax,Dmax,lerrt,lerrD,terr,Derr)
c     function ETA0DG (icomp,t)
c     function OMEGAS (il,is,tau)
c     FUNCTION DELHSV (TX,DX,X,hj,irefn)
c     SUBROUTINE ENSKOG (N,RHO,SIGMA,CMW,X,ETA)
c     function ETAMIX (t,x)
c     subroutine CONFTD (j,amx,Zmix,tj,rhoj,ierr,herr)
c     subroutine CONFD (j,amx,Zmix,tj,rhoj,ierr,herr)
c     subroutine CONFT (k,amx,rhok,tk,ierr,herr)
c     subroutine TRNEC (t,rho,x,eta,tcx,ierr,herr,irefn)
c     subroutine pTRNEC (icomp,t,rho,etares,tcxres,ierr,herr,irefn)
c     subroutine ETAbkp (jj,t,rho,fj,fx,hj,hx,etabk,ierr,herr)
c
c
c ======================================================================
c ======================================================================
c
      subroutine TRNECS (t,rho,x,eta,tcx,ierr,herr)
c
c  compute the transport properties of thermal conductivity and
c  viscosity as functions of temperature, density, and composition
c
c  based on the modification of the Huber-Ely ECS method given by:
c  Klein, S.A., McLinden, M.O. and Laesecke, A. (1997). An improved
c  extended corresponding states method for estimation of viscosity of
c  pure refrigerants and mixtures. Int. J. Refrigeration 20:208-217
c
c  N.B. --equation numbers below refer to this paper;
c       --factor of (ref fluid mol wt)**-0.5 is missing from Eq 31 in paper
c       --in Eq 34 reference fluid should be evaluated at (t0,rho0), not
c         at (t/fj,rho*hj)
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition array [mol frac]
c  outputs:
c      eta--viscosity [uPa.s]
c      tcx--thermal conductivity [W/m.K]
c     ierr--error flag:  0 = successful
c                       -1 = inputs are out of bounds
c                      -35 = temperature out of range for conductivity of ref. fluid
c                      -36 = density out of range for conductivity of ref. fluid
c                      -37 = T and D out of range for conductivity of ref. fluid
c                      -41 = temperature out of range for viscosity of fluid j
c                      -42 = density out of range for viscosity of fluid j
c                      -43 = T and D out of range for viscosity of fluid j
c                      -45 = temperature out of range for viscosity of ref. fluid
c                      -46 = density out of range for viscosity of ref. fluid
c                      -47 = T and D out of range for viscosity of ref. fluid
c                      -48 = ref. fluid viscosity correlation in invalid region
c                      -55 = T out of range for both visc and t.c.
c                      -56 = D out of range for both visc and t.c.
c                      -57 = T and/or D out of range for both visc and t.c.
c                  -58,-59 = ECS model did not converge
c                      -60 = pure fluid is exactly at critical point; t.c. is infinite
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden
c  NIST Physical & Chemical Properties Division, Boulder, Colorado
c  based on the routine by S.A. Klein (in turn, based on Refprop5 routine)
c  03-05-97  MM, original version
c  08-22-97  MM, evaluate critical part of t.c. at simple reduced t,rho rather
c                than the conformal t,rho used for the background part
c  08-25-97  MM, replace calls to ETA1 with ETAK0 (ref fluid) and ETA0DG
c  08-26-97  MM, break-out mix t.c. into separate subroutine TCBKMX
c  09-09-97  MM, calls to old GETFXHX routine, commented out
c                if GETFH does not converge, set to xnotc and RETURN
c  09-25-97  MM, restructure around new CONFTD for finding f's, h's
c  10-01-97  MM, make correction to Eq 34 as noted above
c  10-08-97  MM, evaluate crit part of t.c. at average of reduced and conformal
c  10-09-97  MM, fix bug in do loop associated with calc of xmij
c  10-24-97  MM, Eucken term f_int now a function of t
c  11-13-97  MM, fix bomb when rho = 0
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-02-99 EWL, call limitk with 300 K and 0 mol/L to avoid error messages
c  11-05-01 MLH, catch t.c. at exactly pure fluid critical point
c  11-06-01 MLH, implement new t.c. crit enhancement model, catch conftd errors
c  11-23-01 MLH, add flags to catch errors in calls to etakb,tcxkb,tcxkc,critp
c  11-26-01 MLH, flag individual fluid viscosity out of range errors in mixture calculations
c  06-28-02 MLH, added constraint on low pressure calculations as temp fix for vapor nonconvergence
c  07-01-02 MLH, allow tk6 model
c  11-26-02 EWL, check for pure fluid outside of bounds
c  09-07-04 MLH, add error flags, extrap on vapor side for ref fluid, multiple ref fluids, limit min t and max rho calcs
c  10-26-05 MLH, adapted for multiple ref fluids w/o calling setfld
c  10-30-06 MLH, revised ECS for multiple ref. fluids use fam type, w and dipole mom.
c  11-12-06 MLH, revise so that all pures self-reference and mix ref fluid is irrelevant; use nitrogen for all
c  11-13-06 MLH, limit gj near zero
c  11-30-06 MLH, remove discontinuity for size diff effects
c  12-24-06 MLH, add binary int parameters kij, lij for residual viscosity; deactivate hsdel
c  12-26-06 EWL, remove initial check on pressure
c  01-24-07 MLH, cap upper limit enhancement on mixtures; fix ref fluid bug for purefld usage
c  12-17-07 MLH, deactivate some bounds checks on ref. fluid and on indiv. fluids
c  02-13-08 MLH, adjustment of region to calc shape factors to fix problems in supercritical region of jp900
c  05-27-08 MLH, add one more binary int par for dilute gas viscosity xdij in trnbin
c  06-11-08 MLH, add second bin int for dilute gas vis xdij2
c  07-09-08 MLH, turned off out of bounds warning messages
c  04-27-10 EWL, add checks for i=j and undefined variables
c  09-20-10 MLH, corrected Null vs nul error, look only for ierr=-60 at crit point
c  10-28-10 EWL, remove composition dependence if nc=1
c  06-01-11 EWL, remove common block SHAPES; none of the variables are used in other routines
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr,herrs,herrk,herret,hercrt
      character*255 herrvj, herrsv
c     character*255 hfile(n0:nx), refer1, refer2
      logical lertt,lertD,lervt2,lervD2
      dimension x(ncmax)
      dimension xmj(ncmax)           !equivalent mol mass (Eq 33)
c     COMMON /SHAPES/ fj(nx), hj(nx), fx, hx
      dimension fj(nx), hj(nx)
c
c     for use in k critical enhancement TCXM1C
c     ifamc1c2=0
c     ifamcyc=0
      call ISPURE (x,icomp)
c     initialize error flags
c     routines conftd, etakb, tcxkb,tcxkc, critp all reset error flag so it is necessary
c     to keep separate error counters to keep track of the errors in each routine
      irefn=0    !set reference fluid to slot 0 initially
      ierr=0
      herr=' '
      ierret=0
      herret(1:1)=hnull
      ierrk=0
      herrk(1:1)=hnull
      iercrt=0
      hercrt(1:1)=hnull
      ierrs=0
      herrs(1:1)=hnull
      ierrvj=0
      herrvj(1:1)=hnull
      ierrsv=0
      herrsv(1:1)=hnull
c  set visc and t.c. to flags indicating 'not calculated' so that some
c  value is returned to GUI in event of failure of routines
      eta=xnotc
      tcx=xnotc

      if (icomp.ne.0) then
        if (t.lt.tmecst(icomp) .or. t.gt.txecst(icomp)*1.5d0
     &                       .or. rho.gt.Dxecst(icomp)) then
          ierr=-1
          herr='[TRNECS error -1] one or more inputs are out of bounds'
     &          //hnull
          call ERRMSG (ierr,herr)
          RETURN
        endif
      end if

c  select 1st reference fluid for mixture calculations
c     IF(icomp.eq.0) then
c        acnmax=0.0d0
c        acnmin=1000.d0
c        dipmax=0.0d0
c        dipmin=1000.d0
c        acenx=0.0d0
c        do i=1,nc
c          IF((family(i).eq.'n-alkane').AND.(accen(i).lt.0.1))then
c            ifamc1c2=1
c          endif
c          IF(family(i).eq.'naphthene')then
c            ifamcyc=1
c          endif
c          acenx=x(i)*accen(i)+acenx
c          IF(accen(i).gt.acnmax)acnmax=accen(i)
c          IF(accen(i).lt.acnmin)acnmin=accen(i)
c          IF(dipole(i).gt.dipmax)dipmax=dipole(i)
c          IF(dipole(i).lt.dipmin)dipmin=dipole(i)
c        enddo
c        IF((acnmax.lt.0.1).AND.(ifamc1c2.eq.0))then
c          refer1='nitrogen.fld'
c          irefn=0
c        ELSEIF(acnmax.le.0.37)then   !up to c7
c          refer1='propane.fld'
c          irefn=-ncmax-1
c        ELSEIF((acnmax.gt.0.37))then
c          refer1='c12.fld'
c          irefn=-ncmax-2
c        ENDIF
c  If all fluids are fairly polar, use r134a
c        IF(dipmin.gt.0.5)then
c          refer1='r134a.fld'
c          irefn=-ncmax-3
c        ENDIF
c     ENDIF
c         refer1='nitrogen.fld'
c

c     irefn=0
c     if (nc.eq.1 .or. icnc.ne.0) then
c       icomp=1
c       if (icnc.ne.0) icomp=icnc
c       if (icnc.ne.0) irefn=-ic
c     endif

      irefn=0
      if (icomp.ne.0 .and. nc.ne.1) irefn=-icomp

c  find amx, Zmix and conformal t,rho for reference fluid
      call CRITP (x,tcmxec,pcmxec,dcmxec,iercrt,hercrt)
c     write (*,*) ' TRNECS--ierr from CRITP, tcmxec:  ',iercrt,tcmxec
      call REDX (x,tred,Dred)
      tau=tred/t
      del=rho/Dred
      amx=PHIX(0,0,tau,del,x)
      Zmix=1.0d0+PHIX(0,1,tau,del,x)
      t0=t*tcrit(irefn)/tcmxec          !initial guess for conformal temperature
      rho0=rho*Dcrit(irefn)/dcmxec  !initial guess for conformal density
c     default values in case of convergence failure
      fxx0=tcmxec/tcrit(irefn)
      hxx0=Dcrit(irefn)/dcmxec
      plimi=zmix*r*t0*rho0
c  find "exact" conformal t,rho only if density is significant;
c  at zero density, use the initial guesses above (CONFTD can fail at
c  very low density, and the dilute-gas contribution is dominant anyway)
      IF((zmix.gt.0.3).and. (plimi.lt.1.1*pcrit(irefn)).AND.
     &  (rho0.lt.Dcrit(irefn)))then    !vapor side
        fx=fxx0
        hx=hxx0
      ELSE
        call CONFTD (irefn,amx,Zmix,t0,rho0,ierrs,herrs)
        fx=t/t0
        hx=rho0/rho
        pctf=ABS(100.*(fx-fxx0)/fxx0)
        pcth=ABS(100.*(hx-hxx0)/hxx0)
        IF(ierrs.ne.0)then
          IF((pctf.gt.10).OR.(pcth.gt.10).OR.(t0.lt.1.d0)) THEN
            ierrsv=-58
            j00=0
            write (herrsv,1058) j00, hnull
            call ERRMSG (ierrsv,herrsv)
          ENDIF
        ENDIF
      ENDIF
      if (icomp.ne.0) then
c  for pure fluid, component f,h are same as mixture values
        fj(icomp)=fx
        hj(icomp)=hx
      else
c  for mixture, find conformal t,rho for each of the components
        do j=1,nc
          if (x(j).gt.0.d0) then
          fjj0=tcrit(j)*fx/tcmxec
          hjj0=hx*dcmxec/Dcrit(j)
c         tj=t*tcrit(j)/(fx*tcrit(0))       !initial guess for conformal temp
c         rhoj=rho*hx*Dcrit(j)/Dcrit(0) !initial guess for conformal density
          tj=t*tcrit(j)/tcmxec             !initial guess for conformal temp
c         write (*,1097) j,t,tcrit(j),tcmxec,tj
c1097     format (1x,' TRNECS--j,t,tcrit(j),tcmxec,tj: ',i3,4f12.6)
          rhoj=rho*Dcrit(j)/dcmxec     !initial guess for conformal density
          plimi=zmix*r*tj*rhoj
c  find "exact" conformal t,rho only if density is significant;
c  at zero density, use the initial guesses above (CONFTD can fail at
c  very low density, and the dilute-gas contribution is dominant anyway)
c         IF((zmix.gt.0.3).and.(plimi.lt.1.1*pcrit(j)).AND.
          IF((zmix.gt.0.3).AND.       !gas-like
     &      (rhoj.lt.Dcrit(j)))then    !vapor side
                fj(j)=fjj0
                hj(j)=hjj0
          ELSE
            call CONFTD (j,amx,Zmix,tj,rhoj,ierrs,herrs)
            fj(j)=tj*fx/t
            hj(j)=rho*hx/rhoj
c           check to make sure values are reasonable for this region
            pctf=ABS(100.*(fj(j)-fjj0)/fjj0)
            pcth=ABS(100.*(hj(j)-hjj0)/hjj0)
            IF(ierrs.ne.0)THEN
c           allow nonconvergence to small deviations
              if((pctf.gt.10).OR.(pcth.gt.10).OR.(tj.lt.1.0d0)) then
                ierrsv=-58
                write (herrsv,1058) j,hnull
                call ERRMSG (ierrsv,herrsv)
              else
                ierrs=0
                ierrsv=0
              endif
c           write (*,*) ' TRNECS--ierr from CONFTD, tj: ',ierrs,tj
c           write (*,1098) j,tj,fx,t,tcmxec
c1098       format (1x,' TRNECS--j,tj,fx,t,tcmxec: ',i3,4f12.6)
            ENDIF
          ENDIF
          ! override for sc region; linearly interpolate to value at 1000K
          ! to avoid discontinuity at tcmxec
            IF(zmix.gt.0.3) then  !gas-like
              IF(t.gt.tcmxec) then  !supercritical
              ratiof=(t-1000.0d0)*(fjj0-fj(j))/(1000.0d0-tcmxec)
              ratioh=(t-1000.0d0)*(hjj0-hj(j))/(1000.0d0-tcmxec)
              fj(j)=fjj0+ratiof
              hj(j)=hjj0+ratioh
              IF(t.gt.1000.0d0)fj(j)=fjj0
              IF(t.gt.1000.0d0)hj(j)=hjj0
              ierrs=0
              ierrsv=0
            ENDIF
          ENDIF
          endif
c       write (*,1099) j,fj(j),hj(j)
c1099   format (1x,' TRNECS--j,fj,hj:',i2,2f12.6)
        enddo
      end if
c
c  estimate the Lennard-Jones parameters, if necessary
      if (icomp.ne.0) then
        if (iLJflag(icomp).eq.0) then
          epsktr(icomp)=tcrit(icomp)/tcrit(irefn)*epsktr(irefn)
          sigtrn(icomp)=sigtrn(irefn)
     &                *(Dcrit(irefn)/Dcrit(icomp))**(1.0d0/3.0d0)
        else if (iLJflag(icomp).eq.2) then
c  estimation method of Huber & Ely (1992) FPE 80:239-248
          epsktr(icomp)=epsktr(irefn)*fj(icomp)
          sigtrn(icomp)=sigtrn(irefn)*hj(icomp)**(1.0d0/3.0d0)
        end if
      else
        do j=1,nc
          if (x(j).gt.0.d0) then
            if (iLJflag(j).eq.0) then
              epsktr(j)=tcrit(j)/tcrit(irefn)*epsktr(irefn)
              sigtrn(j)=sigtrn(irefn)
     &                 *(Dcrit(irefn)/Dcrit(j))**(1.0d0/3.0d0)
            else if (iLJflag(j).eq.2) then
c  estimation method of Huber & Ely (1992) FPE 80:239-248
              epsktr(j)=epsktr(irefn)*fj(j)
              sigtrn(j)=sigtrn(irefn)*hj(j)**(1.0d0/3.0d0)
            end if
          endif
        enddo
      endif
c
c  find correlation limits for reference fluid
      p=0.0d0
c     tt=300.0d0
c     rr=0.0d0
c     call LIMITK ('ETA',irefn,tt,rr,p,tminv,tmaxv,Deta,peta,ierr,herr)
c     call LIMITK ('TCX',irefn,tt,rr,p,tmint,tmaxt,Dtcxt,ptcxt,ierr,herr)
c  initialize error flags
      lertt=.false.
c     lervt=.false.
c     lertD=.false.
c     lervD=.false.
c     lervtj=.false.
c     lervDj=.false.
c     write (*,1013) tminv,tmaxv,Deta,tmint,tmaxt,Dtcxt
c1013 format (1x,' TRNECS--ref fluid visc limits--t,rho: ',2f8.2,f12.6/
c    &        1x,'                   t.c. limits--t,rho: ',2f8.2,f12.6)
c
      if (icomp.ne.0) then
c  special case for pure fluid
        gx=wmas(icomp)/wmas(irefn)
        tpsi=t/tcrit(icomp)
        rhopsi=rho/Dcrit(icomp)
        rho0v=rho*hj(icomp)*PSI(icomp,tpsi,rhopsi)               !Eq 21
c  reference fluid background viscosity
        call ETAKB (irefn,t0,rho0v,eta0bk,ierrs,herrs)
        IF(eta0bk.LE.-1000.0d0)THEN  !in two-phase, frozen, or other invalid region
          ierrs=-48
          write (herrs,2068)
          call ERRMSG (ierrs,herrs)
          eta0bk=0.0
        endif
        if(ierrs.ne.0)then
          ierret=ierrs
          herret=herrs
        endif
        eta1dg=ETA0DG(icomp,t)                  !dilute gas visc
        Feta=SQRT(fj(icomp)*gx)*hj(icomp)**(-2.0d0/3.0d0)          !Eq 11
        eta=eta1dg+eta0bk*Feta                             !Eqs 5,10

c  check conformal t,rho against limits of reference fluid correlation
c       call ECSLIM (t0,rho0v,tminv,tmaxv,Deta,lervt,lervD,tcf,Dcf)
c  similar terms for thermal conductivity
        tchi=tpsi
        rhochi=rhopsi
        rho0t=rho*hj(icomp)*CHI(icomp,tchi,rhochi)
c  note that F-factor for t.c. has inverse power of gx compared to visc
        Flam=SQRT(fj(icomp)/gx)*hj(icomp)**(-2.0d0/3.0d0)
        call TCXKB (irefn,t0,rho0t,tcx0b,ierrs,herrs)
        if(ierrs.ne.0)then
          ierrk=ierrs
          herrk=herrs
        endif
c  find dilute-gas parts from collisions and internal degrees of freedom
        tcx1dg=1.0d-3*15.0d0*R*eta1dg/(4.0d0*wmas(icomp))
c       tcx1in=1.32d-3*eta1dg/wmas(1)*(CP0K(1,t)-2.5d0*R)
        tcx1in=FINT(icomp,t)*eta1dg/wmas(icomp)*(CP0K(icomp,t)-2.5d0*R)
c  apply ECS method to background part of ref fluid
        tcx=tcx0b*Flam+tcx1dg+tcx1in
c  check conformal t,rho against limits of reference fluid correlation
c       call ECSLIM (t0,rho0t,tmint,tmaxt,Dtcxt,lertt,lertD,tcf,Dcf)
c
c       write (*,1015) t,1,tcx1dg+tcx1in,tcx0b*Flam,Flam,tcx
c1015   format ('  TRNECS--t,j = ',f8.2,i3,
c    &          '; tcx_dg,tcx_bk,Flam,tcx: ',4f10.6)
c       write (*,1017) PSI(1,tpsi,rhopsi),Feta,CHI(1,tchi,rhochi),Flam
c1017   format (1x,' TRNECS--psi,Feta,chi,Flam: ',4f14.8)
c
      else
c
c  general (mixture) case; begin mixture viscosity calculation
c
c  calculate "equivalent mass" gx, using pure fluid residual viscosities,
c  either from a pure fluid correlation or the pure fluid ECS method;
c  check conformal t,rho against limits of reference fluid correlation
c  and find reference fluid residual viscosity
c  allow t extrapolations on vapor side
c       call ECSLIM (t0,rho0,tminv,tmaxv,Deta,lervt,lervD,tcf,Dcf)
c        if (lervt) then
c          IF(rho0.lt.Dcrit(irefn)) lervt=.false.
c          IF(rho0.lt.1.2*Deta) lervt=.FALSE.  !allow extrap to 30% over max
c        else
c        end if
        call ETAKB (irefn,t0,rho0,eta0bk,ierrs,herrs)
        IF(eta0bk.LE.-1000.0d0)THEN  !in two-phase, frozen, or other invalid region
          ierrs=-48
          write (herrs,2068)
          call ERRMSG (ierrs,herrs)
          eta0bk=0.0
        endif
        if(ierrs.ne.0)then
          ierret=ierrs
          herret=herrs
        endif
c       write (*,1022) t0,rho0,eta0bk
c1022   format (1x,' TRNECS-- ref fluid eval at t,rho =',
c    &          f9.3,f12.6,' eta0bk = ',f12.6)
        gxsum=0.0d0
        do j=1,nc
          if (x(j).gt.0.d0) then
          ierrvj=0
          ierrs=0
          if (heta(j)(1:2).ne.'EC') then
c          a pure fluid correlation is available
c          write (*,1024) j,heta(j)
c1024      format (1x,' TRNECS--pure fluid corr for j = ',i3,':  ',a3)
           tj=t*fj(j)/fx                                    !Eq 36
           rhoj=rho*hx/hj(j)                                !Eq 37
c  check that pure fluid correlation is within its limits
c  if it isn't, continue calculation but set error flags
           call LIMITK ('ETA',j,tj,rhoj,p,tmn,tmx,Dmx,pmx,ierrvj,herrvj)
           if(ierrvj.ne.0)then
              ierrvj=0 ! do not track errors here
              herrvj=' '
c             individual viscosity correlation out of range
c              ierrvj=-40-ierrvj !error numbering consistent with TRNPRP
c              if(ierrvj.EQ.-41)then
c                WRITE(herrvj,2065)j
c                lervtj=.true.
c              elseif(ierrvj.EQ.-42)then
c                WRITE(herrvj,2066)j
c                lervDj=.true.
c              elseif(ierrvj.EQ.-43)then
c                WRITE(herrvj,2067)j
c                lervDj=.true.
c                lervtj=.true.
c                ierrvj=43
c              endif
           endif
c   don't evaluate at t less than limits or rho gt limits for j fluid
           IF(tj.lt.tmn)tj=tmn
           IF(rhoj.gt.Dmx)rhoj=Dmx
           call ETAKB (j,tj,rhoj,etaj,ierrs,herrs)
           if(ierrs.ne.0)then
             ierret=ierrs
             herret=herrs
           endif
          end if
c
          IF(heta(j)(1:2).eq.'EC')then
c        the following line revised
c        - do not switch methods upon error in ierr2
c        or a discontinuity will occur. Just keep the error flag
c        if (heta(j)(1:2).eq.'EC' .or. ierr2.ne.0) then
c  must use ECS method to estimate
c         write (*,1026) j
c1026     format (1x,' TRNECS--will use ECS method for visc, j = ',i3)
            tpsi=t*fj(j)/(fx*tcrit(j))
            rhopsi=rho*hx/(hj(j)*Dcrit(j))
            rho0j=rho*hx*PSI(j,tpsi,rhopsi)                  !Eq 39
            Deta=1000.d0
c  check conformal t,rho against limits of reference fluid correlation
            call ECSLIM(t0,rho0j,tminv,tmaxv,Deta,lervt2,lervD2,tcf,Dcf)
c  don't evaluate at t less than limits or rho gt limits
            lervt2=.false.
            lervd2=.false.
            IF(t0.lt.tmecst(irefn))t0=tmecst(irefn) !ref fluid for mix
            IF(rho0j.gt.Dxecst(irefn))rho0j=Dxecst(irefn)
            IF(t0.lt.tmecst(j))t0=tmecst(j)
            IF(rho0j.gt.Dxecst(j))rho0j=Dxecst(j)
            call ETAKB (irefn,t0,rho0j,etarfj,ierrs,herrs)
            if(ierrs.ne.0)then
              ierret=ierrs
              herret=herrs
            endif
            call etabkp (j,t,rho,fj,fx,hj,hx,etaj,ierr,herr)
          end if

          if (abs(eta0bk).lt.1.d-20) then
            ierr=-59
            write (herr,1058) 0, hnull
            call ERRMSG (ierr,herr)
            return
          endif
          if (etaj.gt.1.0d-12 .and. eta0bk.gt.1.0d-6) then
            gj=etaj/eta0bk/SQRT(fj(j))*hj(j)**(2.0d0/3.0d0)  !Eq 35
          else
c  it is possible for residual viscosity to go through zero
c           gj=wmas(j)/wmas(irefn)/SQRT(fj(j))*hj(j)**(2.0d0/3.0d0)
            IF(etaj.lt.1.0d-12)etaj=1.0d-12
            IF(eta0bk.lt.1.0d-6)eta0bk=1.0d-6
            gj=etaj/eta0bk/SQRT(fj(j))*hj(j)**(2.0d0/3.0d0)  !Eq 35
          end if
          xmj(j)=gj*gj*wmas(irefn)                                 !Eq 33
          endif
c       write (*,*) ' TRNECS--j,xmj(j) for visc:  ',j,xmj(j)
c
        enddo
        do j=1,nc
        if (x(j).gt.0.d0) then
          do i=1,nc
            if (x(i).gt.0.d0) then
              if (i.ne.j) then
                xmij=2.0d0*xmj(i)*xmj(j)/(xmj(i)+xmj(j))
                fij=SQRT(fj(j)*fj(i))*(1.0d0-xkij(i,j))         !Eq 26
                hij=(hj(j)**(1.0d0/3.0d0)+hj(i)**(1.0d0/3.0d0))**3
     &            *0.125d0*(1.0d0-xlij(i,j))                    !Eq 27
              else
                xmij=xmj(i)
                fij=fj(i)
                hij=hj(j)
              endif
              gxsum=gxsum+x(j)*x(i)*SQRT(fij*xmij)*hij**(4.0d0/3.0d0) !Eq 31
            endif
          enddo
c       write (*,1156) j,fj(j),hj(j),etaj
c1156   format (1x,' TRNECS--j,fj,hj,etaj:  ',i2,3f12.6)
        endif
        enddo
c       gx=gxsum/(SQRT(fx*wmas(0))*hx**(4.0d0/3.0d0))        !Eq 31
c       Feta=SQRT(fx)*hx**(-2.0d0/3.0d0)*gx                !Eq 30
c  Eq 30 + 31 reduce to following expression
c  factor of 1/SQRT(ref fluid mol wt) is missing from Klein paper
        Feta=gxsum/hx**2/SQRT(wmas(irefn))
c       write (*,1158) fx,hx,gxsum,Feta
c1158   format (1x,' TRNECS--fx,hx,gxsum,Feta:  ',4f12.6)
        etaxdg=ETAMIX(t,x)      !dilute gas viscosity of mixture
        if (abs(etaxdg).lt.1.d-20) then
          ierr=-59
          write (herr,1058) 0, hnull
          call ERRMSG (ierr,herr)
          return
        endif
c        if (rho.gt.1.0d-6) then
c  apply size correction only if density is significant
c          del=DELHSV(t,rho,x,hj,irefn)  !Enskog size correction
c        else
          del=0.0d0
c        end if
c don't use size correction if polar fluid is reference
c        IF(irefn.EQ.-ncmax-3)del=0.0d0  !ref fluid is R134a
        eta=etaxdg+eta0bk*Feta+del                         !Eq 23
c       write (*,1160) etaxdg,eta0bk,Feta,del,eta
c1160   format (1x,' TRNECS--etaxdg,eta0bk,Feta,del,eta:  ',5f12.6)
c
c  mixture thermal conductivity calculation
c  evaluate background part (including dilute-gas part) of mixture
c
        call TCBKMX (t,rho,x,fj,fx,hj,hx,tcx,Flam,lertt,lertD,
     &               ierrk,herrk,irefn)
c       write (*,1164) tcx,Flam,lertt,lertD
c1164   format (1x,' TRNECS--tcx,Flam,lertt,lertD out of TCBKMX:  ',
c    &          2f12.6,2(3x,i1))
c
      end if           !end mixture case
c
c  for transmitting to critical enhancement routine TCXM1C in block critenh
      etacal=eta
c
c  now compute critical enhancement part of thermal conductivity
c
c  critical enhancement part of t.c. is evaluated at simple reduced t,rho;
c  (if it were calculated at same conformal t,rho as the background part,
c  the enhancement would peak at something other than the critical point)
c     tr=t/tcmxec*tcrit(0)
c     rhor=rho/dcmxec*Dcrit(0)
c
c  The critical enhancement part of t.c. is evaluated at a weighted
c  average of the conformal t,rho and the simple reduced t,rho, such that
c  it approaches the latter at the critical point and the former away
c  from the critical point. If it were calculated at same conformal t,rho
c  as the background part (even close to the critical point), the
c  enhancement would peak at something other than the critical point.
c  But, simply evaluating it always at the reduced t,rho can result in
c  states inside the two-phase region.  Thus, the need for a compromise.
c  the following model is deprecated due to discontinuities near the critical point
c      tr=t/tcmxec
c      rhor=rho/dcmxec
c     write (*,*) ' TRNECS--tcmxec,dcmxec:  ',tcmxec,dcmxec
c      tslope=5.0d0       !revert to conformal at t > 1.2tr
c      Dslope=2.5d0
c      qt=MIN(1.0d0,tslope*ABS(1.0d0-tr))
c      qD=MIN(1.0d0,Dslope*ABS(1.0d0-rhor))
c      tr=qt*t0+(1.0d0-qt)*tr*tcrit(0)
c      rhor=qD*rho0+(1.0d0-qD)*rhor*Dcrit(0)
c
c      call TCXKC (0,tr,rhor,tcx0c,ierr,herr)
c     write (*,1168) tr,rhor,tcx0c
c1168 format (1x,' TRNECS--t,rho for ref fluid,tcx0c:  ',3f12.6)
c  scale the critical enhancement by same Flam as rest of ECS method
c      tcxcr=tcx0c*Flam
c
c    xcsum=0.0d0
c      tcrsum=0.0d0
c      do 260 j=1,nc
c      if (htcx(j)(1:2).ne.'EC') then
c  a pure fluid correlation is available for component j; use for the
c  corresponding portion of the critical enhancement; otherwise mixture
c  calculation as x-->1 would not be continuous with pure fluid
c        tr=t/tcmxec*tcrit(j)
c        rhor=rho/dcmxec*Dcrit(j)
c        call TCXKC (j,tr,rhor,tcxjc,ierr,herr)
c        xcsum=xcsum+x(j)
c        tcrsum=tcrsum+x(j)*tcxjc
c      end if
c  260 continue
c
c  now, finally, add the critical enhancement to the total t.c.
c     tcrsum=tcrsum+(1.0d0-xcsum)*tcxcr
c     call new model for mixture thermal conductivity enhancement
c     previous model had discontinuities near critical due to the scaling factors applied.

      tcrsum=TCXM1C(x,t,rho,ierr,herr)
      tcrmix=tcrsum

      y=0.0d0
c     process errors from critical enhancement
      if (ierr.eq.-60) then       ! a components is exactly at crit point
c  don't give a value if exactly at pure fluid crit point, k is infinite
        tcx=xnotc
      endif
c
c     if model used for t.c. enhancement was other than 'null','tk3', or 'tk6'
c     force mixture continuity as x-->1
c     this is a linear interpolation used close to pure fluid limit
      if (icomp.eq.0) then                      !mixtures only
        do j=1,nc
        if (x(j).gt.0.99d0) then            !only use near end point
          y=tcrmix
          if (htcxcr(j).eq.'TK3') then      !use existing mix model
          elseif (htcxcr(j).eq.'NUL') then  !use existing mix model
          elseif (htcxcr(j).eq.'TK6') then  !use existing mix model
          else              !need to adjust for pure fluid correlation
c           a pure fluid correlation is available for component j but it is
c           not in the form of a 'TK3'or 'TK6' model; use its contribution to force mixture
c           calculation as x-->1 to be continuous with pure fluid
c           get correlation value at pure fluid limit and blend linearly
            call TCXKC (j,t,rho,tcxjc,ierrs,herrs)
            if(ierrs.ne.0)then
              ierrk=ierrs
              herrk=herrs
            endif
            y=100.*(tcxjc-tcrmix)*x(j)+100.*tcrmix-99.0d0*tcxjc
          end if
        endif
        enddo
        do j=1,nc
          if (x(j).gt.0.99d0) then
            if (ierrs.eq.0) then
              tcrsum=y     !blended value
            else           !calculation failure, revert to mix value
              tcrsum=tcrmix
            endif
          endif
        enddo
      endif
c
c     add the enhancement term to the background term to get the total
c     limit enhancement for mixtures to 100% of background
      IF(tcrsum.gt.tcx)tcrsum=tcx
      tcx=tcx+tcrsum
c     write (*,1216) tcxbk0,tcbkcr,tcrsum,tcx
c1216 format (1x,' TRNECS--bk_ref,bk_mix,crit,tcx:',4f12.8)
c     write (*,1217) tcxcr
c1217 format ('  TRNECS--tcx_crit:',2f10.6)
c
c  process warning/errors for problems with calls to etakb
      if (ierret.ne.0) then !etakb failed;
c       do not return numbers for eta
        ierr=ierret
        herr=herret
        eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
      endif
c
c  process warning/errors for problems with calls to tcxkb, tcxkc
c      if (ierrk.ne.0) then !tcxkb or tcxkc failed;
c         return numbers with a warning
c        ierr=ierrk
c        herr=herrk
c       eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
c      endif
c
c  process warnings/errors for problems with CRITP routine
      if (iercrt.gt.0) then !critp failed;
c         return numbers with a warning
        ierr=iercrt
        herr=hercrt
c       eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
      endif
c
c  process warnings/errors for conformal states outside range of ref fluid
c      lertc=lertt.or.lertD
c      lervs=lervt.or.lervD
c      if (lertc .and. lervs) then
c  both thermal conductivity and viscosity generated errors
c        eta=xnotc  !values used by GUI as non-convergence flag
c        tcx=xnotc
c        return a value but with message
c        if ((lertt.or.lervt) .and. (lertD.or.lervD)) then
c          ierr=-57
c          write (herr,2057) t,rho
c        else if (lertD.or.lervD) then
c          ierr=-56
c          write (herr,2056) t,rho
c        else
c          ierr=-55
c          write (herr,2055) t,rho
c        end if
c      else if (lertc) then
c  only thermal conductivity generated errors
c       tcx=xnotc  !value used by GUI as non-convergence flag
c       return a value but with message
c         if (lertt .and. lertD) then
c          ierr=-37
c          write (herr,2037) t,rho
c        else if (lertD) then
c          ierr=-36
c          write (herr,2036) Dcf,Dtcxt
c        else
c          ierr=-35
c          write (herr,2035) tcf,tmint,tmaxt
c        end if
c      else if (lervs) then
c  only viscosity generated errors
c       eta=xnotc  !value used by GUI as non-convergence flag
c       return a value but with message
c        if (lervt .and. lervD) then
c          ierr=-47
c          write (herr,2047) t,rho
c        else if (lertD) then
c          ierr=-46
c          write (herr,2046) Dcf,Deta
c        else
c          ierr=-45
c          write (herr,2045) tcf,tminv,tmaxv
c        end if
c      end if
c
c  process warnings/errors for conformal states outside range of individual fluid
c  viscosity correlations
c      lervsj=lervtj.or.lervDj
c      if (lervsj) then
c     viscosity generated errors
c     return a number with a warning
         !eta=xnotc  !value used by GUI as non-convergence flag
c        ierr=ierrvj
c        herr=herrvj
c      end if
c
      if (ierrsv.ne.0) then !conftd failed
c       do not return numbers
        ierr=ierrsv
        herr=herrsv
        eta=xnotc  !values used by GUI as non-convergence flag
        tcx=xnotc
      endif
      if (ierr.ne.0) call ERRMSG (ierr,herr)
c
c     switch the reference fluid for mixtures and recompute
c      IF(eta.eq.xnotc)return !if major error in eta, exit
c      IF(tcx.eq.xnotc)return !if major error in k, exit
C
c     following multiple reference fluid code unnecessary with self-referencing pures
c      refer2=refer1 !initialize
c      IF(icomp.eq.0)then
c        acen00=accen(irefn)
c        eta00=eta
c        tcx00=tcx
c     first ref fluid chosen on maxim accen and dipole moment
c     select second reference fluid based on min acentric factor and dipole moment
c
c        IF((acnmin.lt.0.1).AND.(ifamc1c2.eq.0))then
c          refer2='nitrogen.fld'
c          irefn=0
c        ! naphthenes tend to be underpredicted; go for next heavier ref fluid.
c        ELSEIF((acnmin.le.0.37).AND.(ifamcyc.eq.0))then
c          refer2='propane.fld'
c          irefn=-ncmax-1
c        ELSEIF((acnmin.gt.0.37))then
c          refer2='c12.fld'
c          irefn=-ncmax-2
c        ENDIF
c       if all fluids are polar, use r134a
c        IF(dipmax.gt.0.5)then
c          refer2='r134a.fld'
c          irefn=-ncmax-3
c        ENDIF
c
c        IF(refer2.ne.refer1) then
c          acen1=accen(irefn)
c         since recursive calls are not standard, use duplicate algorithm
c          call TRNEC (t,rho,x,eta1,tcx1,ierr,herr,irefn)
c
c          IF((acnmin.ge.0.1).AND.(acnmax.le.0.4))then
c            eta=eta1
c            tcx=tcx1
c          ELSEIF((acnmin.gt.0.4).AND.(acnmax.gt.0.4))then
c            eta=eta1
c            tcx=tcx1
c          else
c         interpolate
c            acenr=(acenx-acen00)/(acen1-acen00)
c            eta= (eta1-eta00)*acenr+eta00
c            tcx= (tcx1-tcx00)*acenr+tcx00
c          endif
c          IF(eta1.eq.xnotc)eta=xnotc
c          IF(tcx1.eq.xnotc)tcx=xnotc
c        ENDIF
c      ENDIF

      RETURN
c
 1058     format ('[TRNECS warning 58] 2-D Newton-Raphson method for ',
     &        'conformal temperature and density did not converge ',
     &        'for component',i3,a1)
c2035     format ('[TRNECS warning -35] conformal temperature in ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'thermal conductivity correlation; T_conf =',g11.5,
c    &      ' K; T_min,max =',g11.5,',',g11.5,' K')
c2036     format ('[TRNECS warning -36] conformal density in ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'thermal conductivity correlation; rho_conf =',g11.5,
c    &      ' mol/L; rho_max =',g11.5,' mol/L')
c2037     format ('[TRNECS warning -37] T and rho input to ECS-',
c    &      'transport method are outside range of reference fluid ',
c    &      'thermal conductivity correlation; T_in =',g11.5,
c    &      ' K; rho_in =',g11.5,' mol/L')
c2045     format ('[TRNECS warning -45] conformal temperature in ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'thermal conductivity correlation; T_conf =',g11.5,
c    &      ' K; T_min,max =',g11.5,',',g11.5,' K')
c2046     format ('[TRNECS warning -46] conformal density in ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'viscosity correlation; rho_conf =',g11.5,
c    &      ' mol/L; rho_max =',g11.5,' mol/L')
c2047     format ('[TRNECS warning -47] T and rho input to ECS-',
c    &      'transport method are outside range of reference fluid ',
c    &      'viscosity correlation; T_in =',g11.5,
c    &      ' K; rho_in =',g11.5,' mol/L')
c2055     format ('[TRNECS warning -55] temperature input to ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'viscosity and thermal conductivity correlations; T_in =',
c    &      g11.5,' K; rho_in =',g11.5,' mol/L')
c2056     format ('[TRNECS warning -56] density input to ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'viscosity and thermal conductivity correlations; T_in =',
c    &      g11.5,' K; rho_in =',g11.5,' mol/L')
c2057     format ('[TRNECS warning -57] T and/or rho input to ECS-',
c    &      'transport method are outside range of reference fluid ',
c    &      'viscosity and thermal conductivity correlations; T_in =',
c    &      g11.5,' K; rho_in =',g11.5,' mol/L')
c2065     format ('[TRNECS warning -41] conformal temperature in ECS-',
c    &      'transport method is outside range of fluid ',I3,
c    &      ' viscosity correlation')
c2066     format ('[TRNECS warning -42] conformal density in ECS-',
c    &      'transport method is outside range of fluid ',I3,
c    &      ' viscosity correlation')
c2067     format ('[TRNECS warning -43] conformal temperature and ',
c    &      'density in ECS-',
c    &      'transport method is outside range of fluid ',I3,
c    &      ' viscosity correlation')
 2068     format ('[TRNECS warning -48] invalid region for ',
     &        'viscosity of reference fluid 1 ')

c
      end                                             !subroutine TRNECS
c
c ======================================================================
c
      subroutine TCBKMX (t,rho,x,fj,fx,hj,hx,tcx,Flam,lerrt,lerrD,
     &                   ierr,herr,irefn)
c
c  compute the background thermal conductivity of a mixture where the
c  background t.c. is composed of the internal, dilute-gas, and residual
c  contributions (but not the critical enhancement)
c
c  based on the modification of the Huber-Ely ECS method given by:
c  Klein, S.A., McLinden, M.O. and Laesecke, A. (1997). An improved
c  extended corresponding states method for estimation of viscosity of
c  pure refrigerants and mixtures. Int. J. Refrigeration 20:208-217
c
c  N.B. --equation numbers below refer to this paper;
c       --factor of (ref fluid mol wt)**-0.5 is missing from Eq 31 in paper
c       --in Eq 34 reference fluid should be evaluated at (t0,rho0), not
c         at (t/fj,rho*hj)
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition array [mol frac]
c       fj--array of temperature shape factors for the components
c       fx--temperature shape factor for the mixture
c       hj--array of temperature shape factors for the components
c       hx--density shape factor for the mixture
c  outputs:
c      tcx--thermal conductivity [W/m.K]
c     Flam--multiplier for t.c. (t.c._j = Flam * t.c._ref)
c    lerrt--error flag:  .true. if temperature out of range
c    lerrD--error flag:  .true. if density out of range
c     ierr--error flag:  0 = successful
c                      <>0 indicates problem from underlying routine
c                          passed to calling routine
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  08-26-97  MM, original version, extracted from TRNECS
c  10-01-97  MM, make correction to Eq 34 as noted above
c                add Flam to argument list
c  10-09-97  MM, fix bug in do loop associated with calc of xmij
c  10-24-97  MM, fint is now f(T) rather than const = 1.32d-3
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  03-31-99  MM, implement "correct" ECS mixing rule and Mason & Saxena
c                modification of Wassiljewa eqn for dilute gas mixtures
c  11-23-01 MLH, revise how out of limits error on correlations are handled
c                to prevent discontinuities.
c  11-25-01 MLH, limit how small tcx0b can get to prevent asymptote in mixing
c                rule for GXSUM (prevent division by zero or very small number)
c  07-09-02 MLH, limit how small tcxj, tcx0jb can get to prevent divide by zero
c  12-07-03 MLH, add modified Hanley-based bigx factor for mixture k
c  07-22-05 MLH, allow alternative reference fluids
c  11-24-06 MLH, smooth transition to pure ends; deactivate Hanley factor no longer necessary
c  01-25-07 MLH, allow constant in Mason& Saxena mixing rule to vary depending on fluid system
c  01-28-07 MLH, allow binary interaction parameter for residual thermal conductivity
c  12-17-07 MLH, deactivate reference fluid bounds checks.
c  05-27-08 MLH, add one more binary int par for dilute gas viscosity xdij in trnbin
c  06-11-08 MLH, add second bin int for dilute gas vis xdij2
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      logical lerrt,lerrD
      dimension x(ncmax)
      dimension fj(ncmax),hj(ncmax)  !reducing ratios for components
      dimension tcxin(nx),tcxdg(nx)  !internal and dilute-gas parts
      dimension xmj(ncmax)           !equivalent mol mass (Eq 33)
      dimension etadg(ncmax)
c
      ierr=0
c     ierrs=0
      herr=' '
c  find correlation limits for reference fluid
      p=0.0d0
      tcx=0.0d0
c      call LIMITK ('TCX',irefn,t,rho,p,tmint,tmaxt,Dtcxt,ptcxt,ierr2,herr)
c  conformal temperature, density for mixture
      t0=t/fx
      rho0=rho*hx
c  reference fluid thermal conductivity at conformal conditions
c  check conformal t,rho against limits of reference fluid correlation
c     call ECSLIM (t0,rho0,tmint,tmaxt,Dtcxt,lerrt,lerrD,tcf,Dcf)
      call TCXKB (irefn,t0,rho0,tcx0b,ierr,herr)

c     if (tcx0b.eq.0) RETURN
c     check for very small value of tcx0b. It can go through zero and
c     cause an asymptote in the mixing rule for gxsum (division by zero)
c     thus don't let it get too small or be negative
      if(tcx0b.lt.(1.0d-16))tcx0b=1.0d-16
c
c  calculate "equivalent mass" gx, using pure fluid residual values,
c  either from a pure fluid correlation or the pure fluid ECS method
      gxsum=0.0d0
      tdgMSW=0.0d0
c     gxsum6=0.0d0  !alternative interpretation of gx factor in Refprop6
c     tinsum=0.0d0  !summation for internal contribution--superceded
c     tdgsum=0.0d0  !summation for translational part--superceded
      do j=1,nc
        if (x(j).gt.0.d0) then
        tj=t*fj(j)/fx
        rhoj=rho*hx/hj(j)
        if (htcx(j)(1:2).ne.'EC') then
c  a pure fluid correlation is available
c       write (*,1224) j,htcx(j)
c1224   format (1x,' TRNECS--pure fluid corr for j = ',i3,':  ',a3)

c  check that pure fluid correlation is within its limits
          call LIMITK ('TCX',j,tj,rhoj,p,tmn,tmx,Dmx,pmx,ierr,herr)
c          if(ierr.ne.0)then
c             !ierrs=ierr !save the error
c             !herrs=herr
c          endif
c       removed if/then condition on ierr so that you do not switch method
c       since this leads to an obvious discontinuity. retain error parameter
c       and print error message instead.
c       if (ierr.eq.0) then
c  dilute gas visc of component j using generalized dilute-gas function
            etadg(j)=ETA0DG(j,t)
c  dilute-gas correlation also includes internal contribution; calculate
c  internal contribution separately as different mixing rule is (was) used
c           tcxin(j)=1.32d-3*etadg(j)/wmas(j)*(CP0K(j,t)-2.5d0*R)
            tcxin(j)=FINT(j,t)*etadg(j)/wmas(j)*(CP0K(j,t)-2.5d0*R)
            call TCXK0 (j,t,tcx0,ierr2,herr)
            tcxdg(j)=tcx0-tcxin(j)
c  apply ECS method to background (residual) part of t.c.
            call TCXKB (j,tj,rhoj,tcxj,ierr2,herr)

c  check for very small value of tcxj. It can go through zero and
c  cause division by zero thus don't let it get too small or be negative
            if(tcxj.lt.(1.0d-16))tcxj=1.0d-16
c         write (*,1225) j,tcx0,tcxj,htcx(j)
c1225     format ('  TRNECS--tcx_dg,bk for comp:',i3,2f10.6,' by ',a3)
c        end if
        end if
        if (htcx(j)(1:2).eq.'EC' ) then
c  dilute-gas and internal contributions for component j
          etadg(j)=ETA0DG(j,t)
          tcxin(j)=FINT(j,t)*etadg(j)/wmas(j)*(CP0K(j,t)-2.5d0*R)
          tcxdg(j)=1.0d-3*15.0d0*R*etadg(j)/(4.0d0*wmas(j))
          tcx0=tcxin(j)+tcxdg(j)
c  get ECS value for pure fluid j with respect to its own reference fluid
c  not necessarily the same as the mixture; only residual part needed.
          CALL pTRNEC (j,tJ,rhoJ,etaj,tcxJ,ierr,herr,-j)
c       write (*,1228) j,tcx0,tcxj
c1228   format ('  TRNECS--tcx_dg,bk: comp:',i3,2f10.6,' by ECS')
        end if

        if (tcxj.gt.1.0d-16) then
          gj=tcx0b/tcxj*SQRT(fj(j))/hj(j)**(2.0d0/3.0d0)
        else
c  avoid possibility of division by zero (e.g. zero density as input)
cc        gj=SQRT(wmas(irefn)/wmas(j))*SQRT(fj(j))/hj(j)**(2.0d0/3.0d0)
          tcxj=1.0d-16
          gj=tcx0b/tcxj*SQRT(fj(j))/hj(j)**(2.0d0/3.0d0)
        end if
        xmj(j)=gj*gj*wmas(irefn)
        endif
c     write (*,1254) j,tcxj,tcx0b,xmj(j)
c1254 format (1x,' TRNECS--j,tcxj,tcx0,xmj(j) for t.c.:  ',i4,3f12.6)
      enddo
c
      do j=1,nc
        if (x(j).gt.0.d0) then
        Ajisum=0.0d0
        do i=1,nc
          if (x(i).gt.0.d0) then
            if (i.ne.j) then
              xmij=2.0d0*xmj(i)*xmj(j)/(xmj(i)+xmj(j))
              fij=SQRT(fj(j)*fj(i))*(1.0d0-xkijk(i,j))
              hij=0.125d0*(hj(j)**(1.0d0/3.0d0)+hj(i)**(1.0d0/3.0d0))**3
     &           *(1.0d0-xlijk(i,j))
            else
              xmij=xmj(i)
              fij=fj(i)
              hij=hj(j)
            endif
c         gxsum6=gxsum6+x(j)*x(i)*SQRT(fij*xmij)*hij**(4.0d0/3.0d0)
          gxsum=gxsum+x(i)*x(j)*SQRT(fij/xmij)*hij**(4.0d0/3.0d0)
c  mixing rule for the internal contribution (version 6.0--superceded)
c         tinsum=tinsum
c    &      +x(i)*x(j)*2.0d0*tcxin(i)*tcxin(j)/(tcxin(i)+tcxin(j))
c  Mason & Saxena modification of Wassiljewa mixing rule
c  allow constant out front to change depending on fluid system
          if (i.ne.j) then
            Aji=(1.0d0-xaji(j,i))*(1.0d0+SQRT(etadg(j)/etadg(i))
     &         *(wmas(j)/wmas(i))**0.25d0)**2
     &         /SQRT(8.0d0*(1.0d0+wmas(j)/wmas(i)))
          else
            Aji=1.d0
          endif
          Ajisum=Ajisum+x(i)*Aji
          endif
        enddo
        tdgMSW=tdgMSW+x(j)*(tcxin(j)+tcxdg(j))/Ajisum
c  assume dilute-gas part is simple mole-fraction average--superceded
c       tdgsum=tdgsum+x(j)*tcxdg(j)
        endif
      enddo
c  mixing rule in version 6.0--superceded
c     gxroot=SQRT(wmas(0)*fx)*hx**(4.0d0/3.0d0)/gxsum6
c     Flam6=SQRT(fx)*hx**(-2.0d0/3.0d0)*gxroot
c  above two lines reduce to:
c     Flam6=fx*hx**(2.0d0/3.0d0)*SQRT(wmas(0))/gxsum6
c
c  alternative interpretation from Ely & Hanley Tech Note (1981)
c     gxroot=SQRT(wmas(0)/fx)*hx**(-4.0d0/3.0d0)*gxsum
c     Flam=SQRT(fx)*hx**(-2.0d0/3.0d0)*gxroot
c  above two lines reduce to:
      Flam=SQRT(wmas(irefn))/hx**2*gxsum
c
c     tcx6=tcx0b*Flam+tinsum+tdgsum  !version 6.0 superceded
c     write (*,*) ' TRNECS--tcx0b,Flam,tdgMSW:  ',tcx0b,Flam,tdgMSW
C
C     compute correction factor loosely based on Ind. Eng. Chem. Fundam. 22(1):90-97 (1983)
C     mixture ref fluid not necessarily the same as pures
c     with individual correction factors for all pure this is not necessary
c      zcmix=0.
c      do ii=1,nc
c        zcmix=x(ii)*zcrit(ii)+zcmix
c      enddo
c      zrat=zcrit(irefn)/zcmix
c      bigx=zrat**1.5
c      tcx=tcx0b*Flam*bigX+tdgMSW
c
       tcx=tcx0b*Flam+tdgMSW
c
c     process out of correlation limits errorc
c     if(ierrs.ne.0)then
c       ierr=ierrs
c       herr=herrs
c     endif
      RETURN
      end                                             !subroutine TCBKMX
c
c ======================================================================
c
      subroutine SETTRN (nread,icomp,hcasno,hreff,heoss,hvs,htc,
     &                   ierr,herr)
c
c  set up working arrays for the ECS transport property model
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     hreff--file containing reference fluid EOS (character*255)
c     heos--model ('BWR', etc) for reference fluid EOS (character*3)
c     hvs--model ('VS1', etc) for ref fluid viscosity (character*3)
c     htc--model ('TC1', etc) for ref fluid conductivity (character*3)
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in common /WCFBWR/
c
c  written by S. Klein, NIST Thermophysics Division, Boulder, Colorado
c  12-14-95 SAK, original version
c  03-13-96  MM, add Zcrit to common /CCON/, change parameter n0=-ncmax,
c                other modifications
c  03-05-97  MM, new commons to match with new version of TRNECS
c  08-19-97  MM, get rid of herr=herr (avoid warning); flag nread<=0
c  09-04-97  MM, add second polynomial fit (w/ crossover t or rho)
c  10-24-97  MM, read in f_int term in Eucken correlation in ECS method for t.c.
c  10-28-97  MM, read in f_int only for fluid file version no >= 6.001
c  07-08-98 EWL, change character strings from *80 to *255
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-15-01 MLH, allow ecs to read critical enhancement model
c
      include 'commons.for'
      include 'comtrn.for'
      character*3 heoss,hvs,htc
      character*12 hcasno
      character*255 hreff
      character*255 herr
c
      if (nread.le.0) then
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETTRN error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
        RETURN
      end if
c
c  read data from file
c     write (*,*) ' SETTRN--read component',icomp,' from unit',nread
      read (nread,*) tmecst(icomp)              !lower temperature limit
      read (nread,*) txecst(icomp)              !upper temperature limit
      read (nread,*) pxecst(icomp)              !upper pressure limit
      read (nread,*) Dxecst(icomp)            !upper density limit
      read (nread,2083) heoss,hreff    !reference fluid EOS and .fld file
      read (nread,2003) hvs          !reference fluid viscosity model
      read (nread,2003) htc          !reference fluid conductivity model
      read (nread,*) iLJflag(icomp)   !Lennard-Jones flag
      read (nread,*) sigtrn(icomp)    !Lennard-Jones coef Sig
      read (nread,*) epsktr(icomp)     !Lennard-Jones coef EPS
c     write (*,*) ' SETTRN--L-J parameters:  ',sigtrn(icomp),epsktr(icomp)
c
c  read number of terms for f_int in Eucken correlation
c     write (*,*) ' SETTRN--icomp, version #: ',icomp,verfl(icomp)
      if (verfl(icomp).ge.6.0009d0) then
        read (nread,*) nEuck(icomp)
        if (nEuck(icomp).ge.1) then
c  read polynomial term(s) for f_int
          do j=1,nEuck(icomp)
c  read coeff, power of T, spare1, spare2
            read (nread,*) (cEuck(icomp,j,k),k=1,4)
c         write (*,*) ' SETTRN--Eucken par:',(cEuck(icomp,j,k),k=1,2)
          enddo
        end if
      else
        nEuck(icomp)=0
      end if
c
c  read number of terms for viscosity shape factor (incl. spare)
      read (nread,*) npsi1(icomp),npsi2(icomp),npsi3(icomp)
      if (npsi1(icomp).ge.1) then
c  read polynomial term
        do j=1,npsi1(icomp)
c  read coeff, power of Tr, power of Dr, spare
          read (nread,*) (cpsi(icomp,j,k),k=1,4)
c       write (*,*) ' SETTRN--psi par:  ',(cpsi(icomp,j,k),k=1,3,2)
        enddo
      end if
      if (npsi2(icomp).ge.1) then
c  read coeff, power of Tr, power of Dr, spare for 2nd polynomial term
c  first set of coeff is crossover (t or rho)
        do j=npsi1(icomp)+1,npsi1(icomp)+npsi2(icomp)
          read (nread,*) (cpsi(icomp,j,k),k=1,4)
        enddo
      end if
c
c  ditto for thermal conductivity shape factor
      read (nread,*) nchi1(icomp),nchi2(icomp),nchi3(icomp)
      if (nchi1(icomp).ge.1) then
        do j=1,nchi1(icomp)
c  read coeff, power of Tr, power of Dr, spare
          read (nread,*) (cchi(icomp,j,k),k=1,4)
c       write (*,*) ' SETTRN--chi par:  ',(cchi(icomp,j,k),k=1,3,2)
        enddo
      end if
      if (nchi2(icomp).ge.1) then
c  read coeff, power of Tr, power of Dr, spare for 2nd polynomial term
c  first set of coeff is crossover (t or rho)
        do j=nchi1(icomp)+1,nchi1(icomp)+nchi2(icomp)
          read (nread,*) (cchi(icomp,j,k),k=1,4)
        enddo
      end if
c
c     read pointer to enhancement model for ecs
      read (nread,2003) htcxcrecs(icomp)

      RETURN
 2003 format (a3)
 2083 format (a3,1x,a255)
      end                                             !subroutine SETTRN
c
c ======================================================================
c
      function FINT (icomp,t)
c
c  factor f_int appearing in Eucken correlation for thermal conductivity
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c     FINT--the factor f_int
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-24-97  MM, original version
c
      include 'commons.for'
      include 'comtrn.for'
c
      if (nEuck(icomp).le.0) then
c  no correlation for f_int is present, use value corresponding to
c  modified Eucken correlation
        FINT=1.32d-3
      else
        fsum=0.0d0
        do k=1,nEuck(icomp)
          fsum=fsum+cEuck(icomp,k,1)*t**cEuck(icomp,k,2)
        enddo
        FINT=fsum
      end if
c     write (*,*) ' FINT--t,f_int:  ',t,FINT
c
      RETURN
      end                                                 !function FINT
c
c ======================================================================
c
      function PSI (icomp,tr,rhor)
c
c  viscosity shape factor for a pure fluid or mixture component, as
c  defined by:  Klein et al., Int J Refrigeration 20:208-217 (1997)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c       tr--reduced temperature; = t/tc for a pure fluid
c                                = (t/fx)/(tc_j/fj) for a mixture
c     rhor--reduced density;     = rho/rhoc for a pure fluid
c                                = rho*hx/(hj*rhoc_j) for a mixture
c  output (as function value):
c      psi--the viscosity shape factor, i.e. an additional factor entering
c           into the definition of the conformal density in the ECS method
c           such that:  rho_0 = rho*hj*psi
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  03-05-97  MM, original version
c  09-04-97  MM, add second polynomial fit (w/ crossover t or rho)
c  10-24-97  MM, changes in /WNTTRN/ to accommodate Eucken correlation
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      if (npsi1(i).le.0) then
c  no transport shape factor is present
        PSI=1.0d0
      else
        psisum=0.0d0
        if (npsi2(i).ge.2) then
c  a second polynomial term is present; check whether input (t or rho)
c  is below the crossover value (k = 1,2 in cpsi(i,npsi1(i)+1,k));
c  this term allows for a piece-wise fit of PSI
          nz=npsi1(i)+1
          if (tr.lt.cpsi(i,nz,1) .or. rhor.lt.cpsi(i,nz,2)) then
            do k=nz+1,npsi1(i)+npsi2(i)
             psisum=psisum+cpsi(i,k,1)*tr**cpsi(i,k,2)*rhor**cpsi(i,k,3)
            enddo
            PSI=psisum
            RETURN
          end if
        end if
c  apply the first polynomial term (either it's the only one present
c  or the 2nd polynomial term does not apply)
        do k=1,npsi1(i)
          if (rhor.gt.0) then
            psisum=psisum+cpsi(i,k,1)*tr**cpsi(i,k,2)*rhor**cpsi(i,k,3)
          else
            psisum=psisum+cpsi(i,k,1)*tr**cpsi(i,k,2)
          endif
        enddo
        PSI=psisum
      end if
c     write (*,*) ' PSI:  ',PSI
c
      RETURN
      end                                                  !function PSI
c
c ======================================================================
c
      function CHI (icomp,tr,rhor)
c
c  thermal conductivity shape factor for a pure fluid or mixture
c  component, analogous to the viscosity shape factor defined by
c  Klein et al., Int J Refrigeration 20:208-217 (1997)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c       tr--reduced temperature; = t/tc for a pure fluid
c                                = (t/fx)/(tc_j/fj) for a mixture
c     rhor--reduced density;     = rho/rhoc for a pure fluid
c                                = rho*hx/(hj*rhoc_j) for a mixture
c  output (as function value):
c      chi--thermal conductivity shape factor, i.e. an additional factor
c           entering into the definition of the conformal density in
c           the ECS method such that:  rho_0 = rho*hj*psi
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  03-05-97  MM, original version
c  09-04-97  MM, add second polynomial fit (w/ crossover t or rho)
c  10-24-97  MM, changes in /WNTTRN/ to accommodate Eucken correlation
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      if (nchi1(i).le.0) then
c  no transport shape factor is present
        CHI=1.0d0
      else
        chisum=0.0d0
        if (nchi2(i).ge.2) then
c  a second polynomial term is present; check whether input (t or rho)
c  is below the crossover value (k = 1,2 in cchi(i,nchi1(i)+1,k));
c  this term allows for a piece-wise fit of CHI
          nz=nchi1(i)+1
          if (tr.lt.cchi(i,nz,1) .or. rhor.lt.cchi(i,nz,2)) then
            do k=nz+1,nchi1(i)+nchi2(i)
             chisum=chisum+cchi(i,k,1)*tr**cchi(i,k,2)*rhor**cchi(i,k,3)
            enddo
            CHI=chisum
            RETURN
          end if
        end if
c  apply the first polynomial term (either it's the only one present
c  or the 2nd polynomial term does not apply)
        do k=1,nchi1(i)
          if (rhor.gt.0) then
            chisum=chisum+cchi(i,k,1)*tr**cchi(i,k,2)*rhor**cchi(i,k,3)
          else
            chisum=chisum+cchi(i,k,1)*tr**cchi(i,k,2)
          endif
        enddo
        CHI=chisum
      end if
c     write (*,*) ' CHI:  ',CHI
c
      RETURN
      end                                                  !function CHI
c
c ======================================================================
c
      subroutine TCKVIR (icomp,t,tcx0,tcxcol,tcxvir,ierr,herr)
c
c  thermal conductivity virial coefficient for a pure fluid or mixture
c  component, based on:
c    Nieto de Castro, C.A., Friend, D.G., Perkins, R.A. and Rainwater,
c    J.C. (1990). Thermal conductivity of a moderately dense gas.
c    Chemical Physics  145: 19-26.
c  equation numbers in the comments refer to this paper
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c     tcx0--dilute gas thermal conductivity [W/m-K]
c   tcxcol--contribution of collisions to the dilute-gas t.c. [W/m-K]
c   tcxvir--thermal conductivity second virial coefficient [L/mol];
c           i.e. the multiplier times the collisional part of the dilute-
c           gas conductivity which gives the initial density dependence
c           of that part of the thermal conductivity
c     ierr--error flag:  0 = successful
c                      <>0 = error code originating in TCXK0
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  09-24-97  MM, original version
c  10-24-97  MM, fint is now f(T) rather than const = 1.32d-3
c  10-26-06  MLH, changed htcx(1) to htcx(icomp)
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      herr=' '
c
      etadg=ETA0DG(icomp,t)
      tcxcol=1.0d-3*15.0d0*R*etadg/(4.0d0*wmas(icomp))            !Eq 7
      if (htcx(icomp)(1:2).eq.'EC') then
c  component is modeled with ECS--use generalized function for internal
c  contributions to t.c.
c       tcxint=1.32d-3*etadg/wmas(icomp)*(CP0K(icomp,t)-2.5d0*R)
        tcxint=FINT(icomp,t)*etadg/wmas(icomp)*(CP0K(icomp,t)-2.5d0*R)
        tcx0=tcxcol+tcxint
      else
c  use fluid-specific correlation
        call TCXK0 (icomp,t,tcx0,ierr,herr)
      end if
      if (abs(epsktr(icomp)).lt.1.d-20) return
      tstar=t/epsktr(icomp)
      bprime=(2.9749d0+0.1140d0*tstar)/(tstar-0.04953d0)        !Eq 12
      blam=(bprime-0.625d0*(tcx0/tcxcol-1.0d0))/(tcx0/tcxcol)
c  Eq 3, where the const = 2*pi*N0/3 *1d-27 (sigtrn in nm) *1d3 (vol in L)
      tcxvir=blam*1.26127336d0*sigtrn(icomp)**3
c
      RETURN
      end                                             !subroutine TCKVIR
c
c ======================================================================
c
      subroutine ECSLIM (t,D,tmin,tmax,Dmax,lerrt,lerrD,terr,Derr)
c
c  check input t,rho against limits and return error flags
c
c  inputs:
c        t--temperature [K]
c        D--molar density [mol/L]
c     tmin--minimum temperature for model [K]
c     tmax--maximum temperature [K]
c     Dmax--maximum density [mol/L]
c  outputs:
c    lerrt--logical flag; .true. if t outside limits, set only if .true.
c    lerrD--logical flag; .true. if D outside limits, set only if .true.
c     terr--same as input t, but set only if lerrt = .true.
c     Derr--same as input D, but set only if lerrD = .true.
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  03-24-97  MM, original version
c  01-27-00 EWL, add check for nitrogen: no bounds checking on temperature
c  07-09-08 MLH, no upper temp bounds checking
c
      include 'commons.for'
      include 'comtrn.for'
      logical lerrt,lerrD
c
      if (D.gt.Dmax) then
        lerrD=.true.
        Derr=D
      end if
c
c  Nitrogen has no limits:
      if (hcas(izero0).eq.'7727-37-9' .and. t.gt.0.0d0) RETURN
c
c     if (t.gt.1.5d0*tmax .or. t.lt.tmin) then
      if (t.lt.tmin) then
       lerrt=.true.
        terr=t
      end if
c
      RETURN
      end                                             !subroutine ECSLIM
c
c ======================================================================
c
      function ETA0DG (icomp,t)
c
c  dilute-gas contribution to viscosity for use with ECS model
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta0dg--the dilute-gas part of the viscosity [uPa-s]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  08-25-97  MM, original version, based on ETA1 function by S.A. Klein
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c
      include 'commons.for'
      include 'comtrn.for'
c
      ETA0DG=0.d0
      i=icomp
      tau=1.0d0
      if (abs(epsktr(i)).lt.1.d-20) return
      tau=t/epsktr(i)
c  in this case, the dilute gas is simply the Chapman-Enskog term
      if (abs(sigtrn(i)).gt.1.d-20)
     &ETA0DG=26.692d-3*SQRT(wmas(i)*t)/(sigtrn(i)**2*OMEGAS(2,2,tau))
c    write (*,1001) i,t,tau,ETA0DG
c1001  format (1x,' ETA0DG--dilute-gas visc: i,t,tau,eta',i4,2f9.4,f12.6)
c    write (*,*) ' ETA0DG--sigtrn,omega_2,2:  ',sigtrn(i),OMEGAS(2,2,tau)
c
      RETURN
      end                                               !function ETA0DG
c
c ======================================================================
c
      function OMEGAS (il,is,tau)
c
c  collision integral for Lennard-Jones fluid; returns value for
c  Omega_1,1 or Omega_2,2; based on:
c  Neufeld, Janzen, and Aziz. (1972). J Chem Phys 57:1100-1102
c
c  inputs:
c       il--order of integral
c       is--order of integral
c      tau--dimensionless temperature = t/epsktr, where epsktr is the Lennard-
c           Jones energy parameter (epsilon/k)
c  output (as function value):
c   OMEGAS--value of collision integral
c
c     N.B.--only inputs of (il = is = 1) and (il = is = 2) are valid
c
c  originally implemented in MIPROPS/SUPERTRAPP by J.F. Ely
c  08-25-97 MM, revised and documented by M. McLinden,
c               NIST Physical & Chemical Properties Div, Boulder, CO
c
      include 'commons.for'
      include 'comtrn.for'
c
      if (il.eq.1 .and. is.eq.1) then
        OMEGAS=1.06036d0/tau**0.15610d0
     &        +0.19300d0*EXP(-0.47635d0*tau)
     &        +1.03587d0*EXP(-1.52996d0*tau)
     &        +1.76474d0*EXP(-3.89411d0*tau)
      else
        OMEGAS=1.16145d0/tau**0.14874d0
     &        +0.52487d0*EXP(-0.77320d0*tau)
     &        +2.16178d0*EXP(-2.43787d0*tau)
      end if
c     write (*,1001) il,is,tau,omegas
c1001 format (1x,' OMEGAS--il,is,tau,omega:  ',2i3,2f12.7)
c
      RETURN
      end                                               !function OMEGAS
c
c ======================================================================
c
      FUNCTION DELHSV (TX,DX,X,hj,irefn)
C
C     ENSKOG CORRECTION FOR SIZE AND MASS DIFFERENCE EFFECTS
C     IN MIXTURE VISCOSITY PREDICTION
C
C     BASED ON : J.F. ELY, J. RES. NBS 86(6) 1981, P597-604
C     11-06-01 MLH changed calls to CBRT to CBRTX to avoid compiler complaints
C     03-18-02 MLH changed 200 (propane value) to VC0REF (ref fluid crit vol)
C     06-15-04 MLH corrected units
C     07-22-05 MLH allow selection of alternative ref fluid
c     11-30-06 mlh revised to allow negative values, and scaled so that n2 matches Reid et al.
C
      include 'commons.for'
      include 'comtrn.for'
      dimension hj(ncmax)
      DIMENSION X(nx), Z(nx), S(nx), SIGM(nx,nx)
      DIMENSION SIGDUM(nx,nx), CMWDUM(nx), ZDUM(nx)
      DIMENSION CMW(ncmax)
      parameter (DCON=6.023D-4, IONE=1)
C
      SIGDUM(1,1) = 1.0D0
      CMWDUM(1)   = 1.0D0
      ZDUM(1)     = 1.0D0
      CMWN = wmas(NC)
      DO N = 1, NC
        CMW(N)=wmas(N)
        Z(N) = X(N)
        IF (X(N).LE.0.0D0) Z(N) = 1.0D-8
        VC0REF=1000.0D0/Dcrit(irefn)
C     VC0REF is reference fluid critical volume, cm3/mol
c       S(N) = CBRTX(VC0REF * hj(N)/(3.058D0 * 0.6023D0) )
C       ORIGINAL ELY MATCHED METHANE WITH METHANE REF FLUID
C       REVISE TO MATCH NITROGEN WITH NITROGEN REF FLUID 3.798 FROM REID ET AL.
        S(N) = CBRTX(VC0REF * HJ(N)/(2.71D0 * 0.6023D0) )
        CMW(N) = CMW(N) / CMWN
      ENDDO
C
      SN = S(NC)
      DO N = 1, NC
        S(N) = S(N) / SN
      ENDDO
      RHOX = DCON * DX * SN**3
      SIG1 = 0.0D0
      CMW1 = 0.0D0
      DO I=1,NC
        SI = 0.0D0
        CI = 0.0D0
        DO J = 1,NC
          SIJ = 0.5D0 * (S(I) + S(J))
          TERM = Z(J) * SIJ**3
          SI = SI + TERM
          CI = CI + SIJ * TERM * SQRT(CMW(I) * CMW(J) / (CMW(I)+CMW(J)))
          SIGM(I,J) = SIJ
          ENDDO
        SIG1 = SIG1 + Z(I) * SI
        CMW1 = CMW1 + Z(I) * CI
      ENDDO
      TERM = CBRTX(SIG1)
      CMW1 = 2.0D0 * CMWN * (CMW1 / (SIG1 * TERM ))**2
      SIG1 = SN * TERM
      RHO1 = DCON * DX * SIG1**3
C
c  change arg from nc -> ncc (should not pass element in common)
      ncc=NC
      CALL ENSKOG(ncc,RHOX,SIGM,CMW,Z,VISX)
      VIS0 = 26.692D-3 * SQRT(CMWN*TX) / (SN * SN)
      VISX = VIS0 * VISX
C
      CALL ENSKOG(IONE,RHO1,SIGDUM,CMWDUM,ZDUM,VIS1)
      VIS0 = 26.692D-3 * SQRT(CMW1*TX) / (SIG1*SIG1)
      VIS1 = VIS0 * VIS1
      DO N = 1, NC
        CMW(N) = CMW(N) * CMWN
      ENDDO
C
      DELHSV = VISX - VIS1
      DELHSV=DELHSV*100.0D0  !convert to uPa.s
C     IF(delhsv.lt.0)delhsv=0.d0
C     OK TO LET CORRECTION BE NEGATIVE
      RETURN
      END                                               !function DELHSV
c
c ======================================================================
c
      SUBROUTINE ENSKOG (N,RHO,SIGM,CMW,X,ETA)
      include 'commons.for'
      include 'comtrn.for'
      double precision MJI
      DIMENSION SIGM(nx,nx), CMW(nx), X(nx), Y(nx)
      DIMENSION H(nx,21), S(nx,nx)
      SAVE
      parameter (PI6=0.523598776D0)
C
C                        PRELIMINARY CALCULATIONS
      ETA = 0.0D0
      TERM = RHO * PI6
      S2 = 0.0D0
      S3 = 0.0D0
      DO I=1,N
        TEMP = X(I) * TERM * SIGM(I,I)**2
        S2 = S2 + TEMP
        S3 = S3 + TEMP * SIGM(I,I)
      ENDDO
      S3 = 1.0 - S3
      A1 = S3 * S3
      A2 = S3 * S2
      A3 = S2 * S2
      A4 = S3 * A1
C
      DO I = 1, N
        Y(I) = 0.0
        ETA2 = 0.0
        DO J = 1, N
          S2 = SIGM(I,I) * SIGM(J,J) / (SIGM(I,I) + SIGM(J,J))
          S3 = 2.0 * S2 * S2
          S2 = 3.0 * S2
          BIJ = 4.0 * PI6 * RHO * SIGM(I,J)**3
          YIJ = BIJ * (A1 + A2*S2 + A3*S3) / A4
          MJI = CMW(J) / (CMW(I) + CMW(J))
          EIJ = BIJ * SQRT(2.0D0 * CMW(I) * MJI) / SIGM(I,J)**2
          Y(I) = Y(I) + X(J) * MJI * YIJ
          ETA2 = ETA2 + X(J) * EIJ * YIJ
          S(I,J) = X(J) * YIJ * MJI * MJI / EIJ
        ENDDO
        Y(I) = X(I) * (1.0D0 + 0.8D0 * Y(I))
        ETA = ETA + X(I) * ETA2
      ENDDO
C                        GENERATE THE H MATRIX
      DO I = 1, N
      DO J = 1, N
        S2 = 0.0D0
        A1 = 0.0D0
        IF (J.EQ.I) A1 = 1.0D0
        DO K = 1, N
          RATIO = CMW(I) / (3.0D0 * CMW(K))
          A2 = 0.0D0
          IF (K.EQ.J) A2 = 2.0D0
          TERM = A1 * (1.0D0 + 5.0D0 * RATIO) - A2 * RATIO
          S2 = S2 + S(I,K) * TERM
        ENDDO
        H(I,J) = 2.0D0 * X(I) * S2
      ENDDO
      ENDDO
C                        SOLVE FOR THE EXPANSION COEFFICIENTS
      N1 = N + 1
      IF (N.LE.1) THEN
        H(1,2) = Y(1) / H(1,1)
      ELSE
        DO I = 1, N
          H(I,N1) = Y(I)
        ENDDO
        DO I = 1, N
          I1 = I + 1
          DO J = I1, N1
            H(I,J) = H(I,J) / H(I,I)
          ENDDO
          H(I,I) = 1.0
          DO J = 1, N
            IF (J.NE.I) THEN
              DO K = I1, N1
                H(J,K) = H(J,K) - H(J,I) * H(I,K)
              ENDDO
              H(J,I) = 0.0D0
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      ETA1 = 0.0D0
      DO I = 1, N
        ETA1 = ETA1 + H(I,N1) * Y(I)
      ENDDO
      ETA = ETA1 + 8.0D0 * ETA / (25.0D0 * PI6)
      RETURN
      END                                             !subroutine ENSKOG
c
c ======================================================================
c
      function ETAMIX (t,x)
c
c  compute the viscosity of a dilute-gas mixture assuming the components
c  are described by a Lennard-Jones potential
c
c  inputs:
c        t--temperature [K]
c        x--composition array [mol frac]
c  outputs:
c      eta--viscosity [uPa.s]
c
c  source of original version lost to history (possibly J.F. Ely)
c  adopted for use in Refprop by S.A. Klein, January, 1996
c  08-25-97  MM, revised and documented by M. McLinden,
c                NIST Physical & Chemical Properties Div, Boulder, CO
c                name changed from ETA0 to ETAMIX
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  08-29-01 EWL, fix the mixture values at the pure fluid limits to the
c                pure fluid correlations.
c  12-11-06 MLH, implement alternative method to match pure fluid ends,
c                add interaction parameters
c  05-27-08 MLH, add one more binary int par for dilute gas viscosity xdij in trnbin
c  06-12-08 MLH, revised empirical correction for binaries
c  04-27-10 EWL, add code to check for x(i)=0
c
      include 'commons.for'
      include 'comtrn.for'
      dimension x(ncmax),etai(ncmax)
      character*255 herr
      DIMENSION EOK(nx,nx), SIGM(nx,nx), RM(nx,nx), H(nx,nx+1)
      parameter (DCON=0.320300D0, VCON2=26.692D-3)
C
      ETAMIX=0.0D0
      NC1 = NC + 1
      DO I = 1, NC
      DO J = 1, NC1
        H(I,J) = 1.0D0
      ENDDO
      ENDDO
C
      DO I = 1, NC
      if (x(i).gt.0.d0) then
        DO J = 1, NC
          RM(I,J) = 2.0D0 * WMAS(I) * WMAS(J) / (WMAS(I) + WMAS(J))
          IF(I.EQ.J)then
           DIJS = 1.0D0
           DIJE = 1.0D0
          else
           DIJS = 1.0D0 - xljs(I,J)
           DIJE = 1.0D0 - xlje(I,J)
          endif
          EOK(I,J) = SQRT(epsktr(I) * epsktr(J)) * DIJE
          SIGM(I,J) = 0.5D0 * (sigtrn(I) + sigtrn(J)) * DIJS
        ENDDO
      endif
      ENDDO
C
      DO I = 1, NC
      if (x(i).gt.0.d0) then
      DO J = 1, NC
        if (x(j).gt.0.d0) then
        DIJ = 0.0D0
        IF (I.EQ.J) DIJ = 1.0D0
        SUML = 0.0D0
        DO K = 1, NC
          if (x(K).gt.0.d0) then
            IF (abs(EOK(I,K)).lt.1.d-20) return
            TS = T / EOK(I,K)
            TERM = SQRT(RM(I,K)*T) / SIGM(I,K)**2
            RHODIL = DCON * TERM / OMEGAS(1,1,TS)
            ETAIL  = VCON2 * TERM / OMEGAS(2,2,TS)
            IF (I.EQ.K) ETAI(I)=ETAIL
            DJL = 0.0D0
            IF (J.EQ.K) DJL = 1.0D0
            TERM=(RM(J,J)/(RHODIL*RM(K,K)))*(DIJ-DJL)+
     &            0.5D0*(DIJ+DJL)/ETAIL
            SUML = SUML + X(K) * RM(I,K) * RM(I,K) * TERM
          endif
        ENDDO
        H(I,J) = SUML / (RM(I,I) * RM(J,J))
        endif
      ENDDO
      endif
      ENDDO
C
      DO I = 1, NC
        if (x(i).gt.0.d0) then
        DO J = I+1, NC1
          H(I,J) = H(I,J) / H(I,I)
        ENDDO
        H(I,I) = 1.0D0
        DO J = 1, NC
          IF (J.NE.I .and. x(j).gt.0.d0) THEN
            DO K = I+1, NC1
              H(J,K) = H(J,K) - H(J,I) * H(I,K)
            ENDDO
            H(J,I) = 0.0D0
          ENDIF
        ENDDO
        endif
      ENDDO
c
c     additional empirical correction for binaries
      do i=1,nc
      if (x(i).gt.0.d0) then
      do j=1,nc
         if (i.ne.j) then
           if (x(j).gt.0.d0) then
c            xijpr=x(i)*x(j)
             xis=x(i)/(x(i)+x(j)) !scaled binary composition
             ff=1+xdij(i,j)*xis-(xdij(i,j)+xdij2(i,j))*xis**2
     &        +xdij2(i,j)*xis**3
             xijsum=x(i)+x(j)
             xijdif=1.0d0-xijsum
             h(i,nc1)=h(i,nc1)*ff*xijsum+h(i,nc1)*xijdif
           endif
         endif
      end do
      endif
      end do
C
c     Force the pure ends to match (individual correlation sometimes
c     does not use LJ but mixture always does)
      ETAM = 0.0D0
      DO I = 1, NC
        if (x(i).gt.0.d0) then
        call ETAK0 (I,T,eta,ierr,herr)
        IF(ierr.eq.0) THEN
          ETAM = ETAM + X(I) * H(I,NC1)* eta/etai(i)
        ELSE
          ETAM = ETAM + X(I) * H(I,NC1)
        ENDIF
        endif
      ENDDO
      ETAMIX = ETAM
c
c     etamix = 0.0d0
c      do i=1,nc
c        call ETAK0 (i,t,eta,ierr,herr)
c        if (ABS(eta).lt.1.d-12) call ETAK (i,t,0.d0,eta,ierr,herr)
c        if (etai(i).ne.0.d0) ETAMIX=ETAMIX+x(i)*ETAM/etai(i)*eta
c      enddo
      RETURN
      END                                               !function ETAMIX
c ======================================================================
c
      subroutine CONFTD (j,amx,Zmix,tj,rhoj,ierr,herr)
c
c  Find the conformal temperature and density for component j (which
c  might be the reference fluid) which give a reduced residual Helmholtz
c  energy and compressibility which match the input value.  The system
c  of equations to be solved is thus:
c    aj(tj,rhoj) - amx = 0
c    Zj(tj,rhoj) - Zmix = 0
c  these are put into dimensionless temperature and density tau and del
c  and linearized to the form:
c    (da/dtau)*deltau + (da/ddel)*deldel = amx - aj(tj,rhoj)
c    (dZ/dtau)*deltau + (dZ/ddel)*deldel = Zmix - Zj(tj,rhoj)
c  to allow a solution by the classic Newton-Raphson method in two
c  dimensions.  In the following code, the above nomenclature becomes:
c    a11*x1 + a12*x2 = fx1
c    a21*x1 + a22*x2 = fx2
c
c  inputs:
c        j--component number
c           j = 0:  find conformal conditions for reference fluid
c           j > 0:  find conformal conditions for component j
c     amx--reduced residual Helmholtz energy (A - A*)/RT [-]
c     Zmix--compressibility (pV/RT) [-]
c       tj--initial guess for conformal temperature for component j [K]
c     rhoj--initial guess for conformal density for component j [mol/L]
c  outputs:
c       tj--converged conformal temperature for component j [K]
c     rhoj--converged conformal density for component j [mol/L]
c     ierr--error flag:  0 = successful
c                      -58 = did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  09-29-97  MM, original version, replaces MINH by S.A. Klein
c  01-13-98  MM, check for error from CONFD in case CONFTD does not converge
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-22-99 EWL, increase itmax from 25 to 50 for better vapor convergence
c   1-24-00 EWL, increase itmax from 50 to 200 for better vapor convergence
c   1-31-00 EWL, increase maximum change from 5% to 20%
c  11-06-01 MLH, added check to see if alternative solution from CONFD actually is a root
c                further study on this needed, as there are multiple solutions to the eqn.
c  11-21-01 MLH, check that solution is reasonable (t not too high)
c  12-04-01 MLH, deactivate alternative solution mode
c  06-28-02 LV,  correct dz/ddel
c  09-06-04 MLH, on nonconvergence, do not return initial guess- return actual value
c  12-25-06 MLH, allow up to twice max t
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      parameter (itmax=200)
      parameter (tolf=1.0d-7)  !convergence tol for function evaluations
      parameter (tolx=1.0d-12) !tolerance for step in independent variables
c
c  save initial guesses in case of non-convergence
c      tjsav=tj
c      rhojsv=rhoj
c  set maximum acceptable temperature
      tjmax=1.10*txecst(j)  !allow small extrapolation above max T
c  find the reducing ratios for component j
      tredj=tz(j)
      Dredj=rhoz(j)
c     write (*,*) ' CONFTD--reducing t,rho: ',tredj,Dredj
      x1=tredj/tj       !tauj (dimensionless temperature)
      x2=rhoj/Dredj     !delj (dimensionless density)
c     write (*,1788) j,tj,rhoj,x1,x2
c1788 format (1x,' CONFTD--input j,tj,rhoj: ',i2,2e14.6,
c    &           '; resulting tau,del: ',2e14.6)
c  begin iteration
      do 800 it=0,itmax
c  check for reasonableness of guess by means of derivative dP/d(rho)
c  [actually, the quantity calculated is (1/RT)*(dP/drho)]
      phi00=PHIK(j,0,0,x1,x2)
      phi01=PHIK(j,0,1,x1,x2)
      phi02=PHIK(j,0,2,x1,x2)
      dpdrho=1.0d0+2.0d0*phi01+phi02
      if (dpdrho.lt.0.0d0) then
c     write (*,1799) j,it,x1,x2,dpdrho
c1799 format (' CONFTD--j,it,tau,del: (dpdrho < 0),dP/dD: ',2i3,2e15.6,
c    &        30x,e15.6)
        if (x2*Dredj.gt.Dcrit(j)) then
c  liquid-phase, go to higher density
          x2=1.04d0*x2
        else
c  vapor-phase, go to higher temperature (lower tau)
          x1=0.96d0*x1
        end if
        goto 800        !use up one iteration (avoid infinite loop)
      end if
c  evaluate objective function
      fx1=amx-phi00
      fx2=Zmix-1.0d0-phi01
c     write (*,1800) j,it,x1,x2,fx1,fx2,dpdrho
c1800 format (' CONFTD--j,it,tau,del,amx-a,Zmix-Z,dP/dD: ',2i3,5e15.6)
c  check for convergence
      error1=ABS(fx1)+ABS(fx2)
      if (error1.lt.tolf) then
c  iteration has converged
        goto 840
      end if
c
c  calc remaining derivatives and new guess for independent variables
      phi10=PHIK(j,1,0,x1,x2)
      phi11=PHIK(j,1,1,x1,x2)
      a11=phi10/x1          !partial a w.r.t. tau
      a12=phi01/x2          !partial a w.r.t. del
      a21=phi11/x1          !partial Z w.r.t. tau
      a22=(phi02+phi01)/x2  !partial Z w.r.t. del
      denom=a11*a22-a21*a12
      if (ABS(denom).lt.1.0d-16) then
c  system has singularity, no solution for this guess, try another
        delx1=0.05d0*x1
        delx2=-0.05d0*x2
      else
        delx1=(a22*fx1-a12*fx2)/denom
        delx2=(a11*fx2-a21*fx1)/denom
      end if
      sumdel=ABS(delx1)+ABS(delx2)
c  do not allow too great a step (x1,x2 should always be positive)
c     write (*,'(8f12.6)') tredj/x1,dredj*x2,sumdel,error
      x1=x1+SIGN(1.0d0,delx1)*MIN(ABS(delx1),0.20d0*x1)
      x2=x2+SIGN(1.0d0,delx2)*MIN(ABS(delx2),0.20d0*x2)
c  if step is within tolerance, also consider iteration finished
      if (sumdel.lt.tolx) goto 840
  800 continue
c
c  iteration loop has not converged, try another scheme
      ierr=-58
      write (herr,1058) j,hnull
 1058 format ('[TRNECS warning 58] 2-D Newton-Raphson method for ',
     &        'conformal temperature and density did not converge ',
     &        'for component',i3,a1)
      call ERRMSG (ierr,herr)
c     write (*,1801) x1,x2,fx1,fx2
c1801 format (' CONFTD--2-D Newton''s method iteration did not',
c    &        ' converge; x1,x2,fx1,fx2: ',4e15.6)
c  go back to starting guesses for tj, rhoj
      ! not in use
      !tj=tjsav
      !rhoj=rhojsv
c
c     although the following code sometimes finds a root, it apparently is
c     not always and appropriate solution. Apparently there are multiple
c     roots. deactivate until method to determine correct root is found.
c     try alternative CONFD method
c     call CONFD (j,amx,Zmix,tj,rhoj,ierr1,herr1)
c     since confd just minimizes the function, doesn't actually find a root,
c     it may not satisfy original equation. Thus check to
c     see if it is a true solution before accepting it.
c     NOTE: there are known bugs with this procedure- there can be multiple roots
c           and it doesn't always get the correct one!
c      x1=tredj/tj       !tauj (dimensionless temperature)
c      x2=rhoj/Dredj     !delj (dimensionless density)
c      phi00=PHIK(j,0,0,x1,x2)
c      phi01=PHIK(j,0,1,x1,x2)
c      fx1=amx-phi00
c      fx2=Zmix-1.0d0-phi01
c      error2=ABS(fx1)+ABS(fx2)
c     accept only if it meets original criteria and t is not too high
c      if ((error2.lt.tolf).AND.(tj.le.tjmax)) then
c        ierr=0
c        herr=' '
c      else
c        tj=tjsav      !return original guess of tj, rhoj
c        rhoj=rhojsv
c        ierr=-58
c        write (herr,1058) j,hnull
c        call ERRMSG (ierr,herr)
c      endif
c
c
c     the following code has been deactivated and superceded by the code above.
c     call CONFD (j,amx,Zmix,tj,rhoj,ierr1,herr1)
c     write (*,*) ' CONFTD--ierr,tj from CONFD: ',ierr1,tj
c     if (ierr1.ne.0) then
c       write (*,*) ' CONFD--did not converge; using initial tj,rhoj'
c        ierr=ierr1
c        herr=herr1
c        tj=tjsav
c        rhoj=rhojsv
c      end if
       GOTO 840
C      RETURN
c
c  iteration has converged, assign outputs
  840 continue
      tj=tredj/x1
      rhoj=x2*Dredj
c     check to make sure converged solution is reasonable
      if(tj.gt.2.0d0*tjmax)then
       ! tj=tjsav
       ! rhoj=rhojsv
        ierr=-58
        write (herr,1058) j,hnull
        call ERRMSG (ierr,herr)
      endif
c
      RETURN
      end                                             !subroutine CONFTD
c
c ======================================================================
c
      subroutine CONFD (j,amx,Zmix,tj,rhoj,ierr,herr)
c
c  Find the conformal temperature and density for component j (which
c  might be the reference fluid) which give a reduced residual Helmholtz
c  energy and compressibility which match the input value.  The quantity
c    FX = [a(tj,rhoj) - amx]**2 + [Z(tj,rhoj) - Zmix]**2
c  is minimized using Brent's method adapted from:
c    Press, W.H., Flannery, B.P., Teukolsky, S.A. and Vettering, W.T.
c    (1986).  Numerical Recipes:  The Art of Scientific Computing,
c    Cambridge University Press.
c  This minimization is in terms of density.  The single-dimension
c  Brent's method is extended to the two-dimensional problem here by an
c  auxiliary routine CONFT which finds the optimum conformal temperature
c  for a given guess of the conformal density.
c
c  inputs:
c        j--component number
c           j = 0:  find conformal conditions for reference fluid
c           j > 0:  find conformal conditions for component j
c     amx--reduced residual Helmholtz energy (A - A*)/RT [-]
c     Zmix--compressibility (pV/RT) [-]
c       tj--initial guess for conformal temperature for component j [K]
c     rhoj--initial guess for conformal density for component j [mol/L]
c  outputs:
c       tj--converged conformal temperature for component j [K]
c     rhoj--converged conformal density for component j [mol/L]
c     ierr--error flag:  0 = successful
c                      -58 = did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  09-26-97  MM, original version, based on MINH by S.A. Klein
c  01-24-00 EWL, decrease TOL to 1E-11
c  09-04-00 EWL, change variable R to RR to avoid conflict with R in Gcnst
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      parameter (CGOLD=0.61803399, ZEPS=1E-20)
      parameter (TOL=1E-11, ITMAX=50)
c
      ierr=0
      herr=' '
      V=rhoj                           !first guess for rhoj
      A=0.80d0*rhoj                    !lower bound on rhoj
      B=1.20d0*rhoj                    !upper bound on rhoj
      W=V
      X=V
      E=0.0d0
      D=0.0d0
c  find the reducing ratios for component j
      tredj=tz(j)
      Dredj=rhoz(j)
c  find the optimum tj for the initial guess of rhoj and evaluate the
c  objective function FX
c     write (*,*) ' CONFD--j,tj input to CONFT, first iteration: ',j,tj
      tj0=tj    !save initial guess
      call CONFT (j,amx,rhoj,tj,ierr,herr)
      if (ABS(tj/tj0-1.0d0).ge.0.2) then
c  do not allow too great a change in tj
c       write (*,*) ' CONFD--unconstrained tj from CONFT: ',tj
        tj=tj0*(1.0d0+SIGN(0.2d0,tj/tj0-1.0d0))
      end if
c     write (*,*) ' CONFD--ierr,tj from CONFT:  ',ierr,tj
      tauj=tredj/tj
      delj=rhoj/Dredj
      aj=PHIK(j,0,0,tauj,delj)
      dela=amx-aj
      Zj=1.0d0+PHIK(j,0,1,tauj,delj)
      delZ=Zmix-Zj
      FX=dela**2+delZ**2
c     call DPDDK (j,tj,rhoj,dpdrho)             !debug only
c     write (*,1089) j,0,tj,rhoj,amx,aj,Zmix,Zj,dpdrho
c
      FV=FX
      FW=FX
      DO ITER=1,ITMAX
      XM=0.5d0*(A+B)
      TOL1=TOL*ABS(X)+ZEPS
      TOL2=2.0d0*TOL1
      IF (ABS(X-XM).LE.TOL2-0.5d0*(B-A)) THEN
        rhoj=X                !value which minimizes objective function
c       write (*,1099) j,amx,Zmix,tj,rhoj,FU
        RETURN
      ENDIF
      IF (ABS(E).GT.TOL1) THEN
        RR=(X-W)*(FX-FV)
        Q=(X-V)*(FX-FW)
        P=(X-V)*Q-(X-W)*RR
        Q=2.0*(Q-RR)
        IF (Q.GT.0.0d0) P=-P
        Q=ABS(Q)
        ETEMP=E
        E=D
        IF ((ABS(P).GE.ABS(0.5d0*Q*ETEMP)).or.(P.LE.Q*(A-X)).or.
     &        (P.GE.Q*(B-X))) THEN
          IF (X.GE.XM) THEN
             E=A-X
          else
             E=B-X
          endif
          D=CGOLD*E
        else
          D=P/Q     !Parabolic step
          U=X+D
          IF ((U-A.LT.TOL2).or.(B-U.LT.TOL2)) D=SIGN(TOL1,XM-X)
        endif
      else
        IF (X.GE.XM) THEN
          E=A-X
        else
          E=B-X
        endif
        D=CGOLD*E
      endif
      IF (ABS(D).GE.TOL1) THEN
        U=X+D
      else
        U=X+SIGN(TOL1,D)
      endif
c  find the optimum tj for the current guess of rhoj and evaluate the
c  objective function
      rhoj=U
c     write (*,*) ' CONFD--it,tj input to CONFT: ',iter,tj
      call CONFT (j,amx,rhoj,tj,ierr,herr)
c     write (*,*) ' CONFD--ierr,tj from CONFT:   ',ierr,tj
      tauj=tredj/tj
      delj=rhoj/Dredj
      aj=PHIK(j,0,0,tauj,delj)
      dela=amx-aj
      Zj=1.0d0+PHIK(j,0,1,tauj,delj)
      delZ=Zmix-Zj
c     call DPDDK (j,tj,rhoj,dpdrho)             !debug only
c     write (*,1089) j,ITER,tj,rhoj,amx,aj,Zmix,Zj,dpdrho
c1089 format (' CONFD--j,it,tj,rhoj,amx,aj,Zmix,Zj,dP/dD:  ',2i3,f9.3,
c    &        f10.5,2e14.6,2f9.5,e14.6)
      FU=dela**2+delZ**2
c
      IF (FU.LE.FX) THEN
        IF (U.GE.X) THEN
          A=X
        else
          B=X
        endif
        V=W
        FV=FW
        W=X
        FW=FX
        X=U
        FX=FU
      else
        IF (U.LT.X) THEN
          A=U
        else
          B=U
        endif
        IF ((FU.LE.FW).or.(ABS(W-X).LT.1.0D-12)) THEN
          V=W
          FV=FW
          W=U
          FW=FU
        else
          IF ((FU.LE.FV) .or. (ABS(V-W).LT.1.0D-12) .or.
     &        (ABS(V-X).LT.1.0D-12)) THEN
            V=U
            FV=FU
          endif
        endif
      endif
      ENDDO
      IERR=-58
      herr='[CONFD error 58] ECS-transport routines did not converge'//
     &     hnull
      call ERRMSG (ierr,herr)
c     write (*,1099) j,amx,Zmix,tj,rhoj,FU
c1099 format (' CONFD--j,A,Z,tj,rhoj,resid: ',i3,e14.6,f9.5,f9.3,f10.5,
c    &        e14.6)
      RETURN
      end                                              !subroutine CONFD
c
c ======================================================================
c
      subroutine CONFT (k,amx,rhok,tk,ierr,herr)
c
c  Find the conformal temperature for component k (including, possibly,
c  the reference fluid) which gives a reduced residual Helmholtz energy
c  which matches the input value.  The zero of the function
c    FX = [a(tk,rhok) - amx]
c  is found using a damped secant method combined with a reguli-falsi.
c  This routine is called from within CONFTD, and the value of rhok is
c  the current guess value from that routine.
c
c  inputs:
c        k--component number
c           k = 0:  find conformal conditions for reference fluid
c           k > 0:  find conformal conditions for component k
c     amx--reduced residual Helmholtz energy (A - A*)/RT [-]
c     rhok--conformal density for component k [mol/L]
c       tk--initial guess for conformal temperature for component k [K]
c  outputs:
c       tk--converged conformal temperature for component k [K]
c     ierr--error flag:  0 = successful
c                      -58 = did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  09-26-97  MM, original version, replaces MINF by S.A. Klein
c  01-13-98  MM, set damping ratio to 0.8 and constrain new guesses
c  01-24-00 EWL, decrease tolr to 1.0d-11
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      logical lxneg,lxpos
      dimension x(3),fx(2)
      parameter (itmax=20)
      parameter (tolr=1.0d-11)    !convergence tolerance
      parameter (xdamp=0.8d0)    !damping (or acceleration) ratio, normal=1
c
      ierr=0
      herr=' '
c  find the reducing ratios for component k
      tredk=tz(k)
      Dredk=rhoz(k)
      delk=rhok/Dredk       !density is fixed for this iteration
c
c  begin iteration--the basic iteration is a secant method, but
c  positive and negative roots are stored, allowing use of slower, but
c  more robust, reguli-falsi method in event that guess by secant method
c  is further from solution
c
      lxpos=.false.         !initialize flags for reguli-falsi iteration
      lxneg=.false.
      xneg=0.0d0
      xpos=0.0d0
      fxneg=0.0d0
      fxpos=0.0d0
      j=1                   !j=1 for first iteration, j=2 for all others
      x(1)=tk               !first guess for independent variable
      do it=1,itmax
c  evaluate objective function
        tauk=tredk/x(j)       !x(j) is current guess for tk
        fx(j)=amx-PHIK(k,0,0,tauk,delk)
c  check for convergence
        if (ABS(fx(j)).lt.tolr) then
c  iteration has converged
          goto 840
        else
          if (fx(j).lt.0.0d0) then
            lxneg=.true.
            xneg=x(j)
            fxneg=fx(j)
          else
            lxpos=.true.
            xpos=x(j)
            fxpos=fx(j)
          end if
        end if
c
c  compute new guess for independent variable
        if (j.eq.1) then
c  for first iteration, new guess is ratio of old
          x(2)=x(1)*1.005
          j=2
        else
c  subsequent iterations--use (damped) secant method
          if (ABS(fx(2)-fx(1)).lt.1.0d-12) then
c  avoid division by zero, use average of previous guesses; if fx(1) was
c  equal to fx(2) by coincidence, this should allow solution; if there
c  is a more severe problem iteration will probably not converge, but
c  neither will it blow up
            x(3)=0.5d0*(x(1)+x(2))
          else
            x(3)=x(2)-xdamp*fx(2)*(x(2)-x(1))/(fx(2)-fx(1))
          end if
          if (ABS(x(3)/x(2)-1.0d0).ge.0.6) then
c  do not allow too great a change in tk, a.k.a. x
c         write (*,*) '   CONFD--unconstrained tk from CONFT: ',x(3)
            x(3)=x(2)*(1.0d0+SIGN(0.6d0,x(3)/x(2)-1.0d0))
          end if
c       write (*,*) '   CONFT--old, new tk: ',x(2),x(3)
c  check that new guess is not outside bounds, if so use reguli-falsi
c  (provided that bounds on root have been found)
          if ((x(3).gt.MAX(xpos,xneg).or. x(3).lt.MIN(xpos,xneg))
     &       .and. lxneg .and. lxpos) then
            x(3)=xpos-fxpos*(xpos-xneg)/(fxpos-fxneg)
          end if
c  discard oldest iteration
          x(1)=x(2)
          x(2)=x(3)
          fx(1)=fx(2)
        end if
      enddo
c
c  iteration loop has not converged
c     write (*,1801) k,x(1),x(2),fx(1)
c1801 format (' CONFT--secant method for t_conf did not converge; ',
c    &        'k,tk1,tk2,ft1: ',i3,2f10.3,e14.6)
c
  840 continue
      tk=x(j)
c
      RETURN
      end                                              !subroutine CONFT

c ======================================================================
c
      subroutine pTRNEC (icomp,t,rho,etares,tcxres,ierr,herr,irefn)
c
c  compute the excess contribution of transport properties of thermal conductivity and
c  viscosity as functions of temperature, density for pure fluids in
c  a mixture using their individual reference fluids
c
c  based on the modification of the Huber-Ely ECS method given by:
c  Klein, S.A., McLinden, M.O. and Laesecke, A. (1997). An improved
c  extended corresponding states method for estimation of viscosity of
c  pure refrigerants and mixtures. Int. J. Refrigeration 20:208-217
c
c  N.B. --equation numbers below refer to this paper;
c       --factor of (ref fluid mol wt)**-0.5 is missing from Eq 31 in paper
c       --in Eq 34 reference fluid should be evaluated at (t0,rho0), not
c         at (t/fj,rho*hj)
c
c  inputs:
c    irefn--reference fluid number
c        t--temperature [K]
c      rho--molar density [mol/L]
c
c  outputs:
c      eta--viscosity [uPa.s]
c      tcx--thermal conductivity [W/m.K]
c     ierr--error flag:  0 = successful
c                       -1 = inputs are out of bounds
c                      -35 = temperature out of range for conductivity of ref. fluid
c                      -36 = density out of range for conductivity of ref. fluid
c                      -37 = T and D out of range for conductivity of ref. fluid
c                      -41 = temperature out of range for viscosity of fluid j
c                      -42 = density out of range for viscosity of fluid j
c                      -43 = T and D out of range for viscosity of fluid j
c                      -45 = temperature out of range for viscosity of ref. fluid
c                      -46 = density out of range for viscosity of ref. fluid
c                      -47 = T and D out of range for viscosity of ref. fluid
c                      -48 = ref. fluid viscosity correlation in invalid region
c                      -55 = T out of range for both visc and t.c.
c                      -56 = D out of range for both visc and t.c.
c                      -57 = T and/or D out of range for both visc and t.c.
c                  -58,-59 = ECS model did not converge
c                      -60 = pure fluid is exactly at critical point; t.c. is infinite
c     herr--error string (character*255 variable if ierr<>0)
c
c  NIST Physical & Chemical Properties Division, Boulder, Colorado
c  based on the routine by S.A. Klein (in turn, based on Refprop5 routine)
c  11-11-06 MLH, initial version.
c  12-17-07 MLH, deactivate some bounds checks on ref. fluids
c  02-13-08 MLH, deactivate more bounds checks on fluids
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr,herrs,herrk,herret,hercrt
      character*255 herrvj, herrsv
c     character*255 hfile(n0:nx)
      logical lervtj,lervDj,lervsj
      dimension fj(nx), hj(nx)
      dimension zx(ncmax)
c
c     initialize error flags
c     routines conftd, etakb, tcxkb,tcxkc, critp all reset error flag so it is necessary
c     to keep separate error counters to keep track of the errors in each routine

      ierr=0
      herr=' '
      ierret=0
      herret(1:1)=hnull
      ierrk=0
      herrk(1:1)=hnull
      iercrt=0
      hercrt(1:1)=hnull
      ierrs=0
      herrs(1:1)=hnull
      ierrvj=0
      herrvj(1:1)=hnull
      ierrsv=0
      herrsv(1:1)=hnull
c
      do i=1,nc
        zx(i)=0.d0
      end do
      zx(icomp)=1.d0


c  find amx, Zmix and conformal t,rho for reference fluid
      call CRITP (zx,tcmxec,pcmxec,dcmxec,iercrt,hercrt)
c     write (*,*) ' pTRNECS--ierr from CRITP, tcmxec:  ',iercrt,tcmxec
      call REDX (zx,tred,Dred)
      tau=tred/t
      del=rho/Dred
      amx=PHIX(0,0,tau,del,zx)
      Zmix=1.0d0+PHIX(0,1,tau,del,zx)
      t0=t*tcrit(irefn)/tcmxec      !initial guess for conformal temperature
      rho0=rho*Dcrit(irefn)/dcmxec  !initial guess for conformal density

c     default values in case of convergence failure
      fxx0=tcmxec/tcrit(irefn)
      hxx0=Dcrit(irefn)/dcmxec
      plimi=zmix*r*t0*rho0
c  find "exact" conformal t,rho only if density is significant;
c  at zero density, use the initial guesses above (CONFTD can fail at
c  very low density, and the dilute-gas contribution is dominant anyway)
      IF((zmix.gt.0.3).and. (plimi.lt.1.1*pcrit(irefn)).AND.
     &  (rho0.lt.Dcrit(irefn)))then    !vapor side
        fx=fxx0
        hx=hxx0
      ELSE
        call CONFTD (irefn,amx,Zmix,t0,rho0,ierrs,herrs)
        fx=t/t0
        hx=rho0/rho
        pctf=ABS(100.*(fx-fxx0)/fxx0)
        pcth=ABS(100.*(hx-hxx0)/hxx0)
        IF(ierrs.ne.0)then
          IF((pctf.gt.10).OR.(pcth.gt.10).OR.(t0.lt.1.d0)) THEN
            ierrsv=-58
            j00=0
            write (herrsv,1058) j00, hnull
            call ERRMSG (ierrsv,herrsv)
          ENDIF
        ENDIF
      ENDIF


c  for component j in a mixture, find conformal t,rho for the component
          j=icomp
          fjj0=tcrit(j)*fx/tcmxec
          hjj0=hx*dcmxec/Dcrit(j)
c         tj=t*tcrit(j)/(fx*tcrit(0))       !initial guess for conformal temp
c         rhoj=rho*hx*Dcrit(j)/Dcrit(0) !initial guess for conformal density
          tj=t*tcrit(j)/tcmxec         !initial guess for conformal temp
c         write (*,1097) j,t,tcrit(j),tcmxec,tj
c1097     format (1x,' pTRNECS--j,t,tcrit(j),tcmxec,tj: ',i3,4f12.6)
          rhoj=rho*Dcrit(j)/dcmxec     !initial guess for conformal density
          plimi=zmix*r*tj*rhoj
c  find "exact" conformal t,rho only if density is significant;
c  at zero density, use the initial guesses above (CONFTD can fail at
c  very low density, and the dilute-gas contribution is dominant anyway)
          IF((zmix.gt.0.3).and.(plimi.lt.1.1*pcrit(j)).AND.
     &      (rhoj.lt.Dcrit(j)))then    !vapor side
                fj(j)=fjj0
                hj(j)=hjj0
          ELSE
            call CONFTD (j,amx,Zmix,tj,rhoj,ierrs,herrs)
            fj(j)=tj*fx/t
            hj(j)=rho*hx/rhoj
c           check to make sure values are reasonable for this region
            pctf=ABS(100.*(fj(j)-fjj0)/fjj0)
            pcth=ABS(100.*(hj(j)-hjj0)/hjj0)
            IF(ierrs.ne.0)THEN
c           allow nonconvergence to small deviations
              if((pctf.gt.10).OR.(pcth.gt.10).OR.(tj.lt.1.d0)) then
                ierrsv=-58
                write (herrsv,1058) j,hnull
                call ERRMSG (ierrsv,herrsv)
              endif
c           write (*,*) ' pTRNECS--ierr from CONFTD, tj: ',ierrs,tj
c           write (*,1098) j,tj,fx,t,tcmxec
c1098       format (1x,' pTRNECS--j,tj,fx,t,tcmxec: ',i3,4f12.6)
            ENDIF
          ENDIF
c       write (*,1099) j,fj(j),hj(j)
c1099   format (1x,' pTRNECS--j,fj,hj:',i2,2f12.6)


c  find correlation limits for reference fluid
      p=0.0d0
      tt=300.0d0
      rr=0.0d0
      call LIMITK ('ETA',irefn,tt,rr,p,tminv,tmaxv,Deta,peta,ierr,herr)
c      call LIMITK ('TCX',irefn,tt,rr,p,tmint,tmaxt,Dtcxt,ptcxt,ierr,herr)
c  initialize error flags
c     lertt=.false.
c     lervt=.false.
c     lertD=.false.
c     lervD=.false.
      lervtj=.false.
      lervDj=.false.

      tchi=t/fj(icomp)
      rhochi=rho*hx/(hj(icomp)*Dcrit(icomp))
      rho0j=rho*hx*CHI(icomp,tchi,rhochi)
      gxj=wmas(icomp)/wmas(-icomp)
      Flam=SQRT(fj(icomp)/gxj)*hj(icomp)**(-2.0d0/3.0d0)
      call TCXKB (-icomp,tchi,rho0j,tcx0b,ierrs,herrs)
      tcxres=flam*tcx0b

      tpsi=t/fj(icomp)
      rhopsi=rho*hx/(hj(icomp)*Dcrit(icomp))
      rho0j=rho*hx*psi(icomp,tpsi,rhopsi)
      gxj=wmas(icomp)/wmas(-icomp)
      Feta=SQRT(fj(icomp)*gxj)*hj(icomp)**(-2.0d0/3.0d0)
c  do not let ref fluid for viscosity be evaluated beyond its limits
c  since the correlations generally do not extrapolate well
      IF(tpsi.lt.tminv)tpsi=tminv !ref fluid for fluid of interest
      IF(rho0j.gt.Deta)rho0j=Deta
      call ETAKB (-icomp,tpsi,rho0j,eta0b,ierrs,herrs)
      etares=feta*eta0b

c  process warning/errors for problems with calls to etakb
      if (ierret.ne.0) then !etakb failed;
c       do not return numbers for eta
        ierr=ierret
        herr=herret
c       eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
      endif
c
c  process warning/errors for problems with calls to tcxkb, tcxkc
      if (ierrk.ne.0) then !tcxkb or tcxkc failed;
c         return numbers with a warning
        ierr=ierrk
        herr=herrk
c       eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
      endif
c
c  process warnings/errors for problems with CRITP routine
      if (iercrt.gt.0) then !critp failed;
c         return numbers with a warning
        ierr=iercrt
        herr=hercrt
c       eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
      endif
c
c  process warnings/errors for conformal states outside range of ref fluid
c     lertc=lertt.or.lertD
c     lervs=lervt.or.lervD
c      if (lertc .and. lervs) then
c  both thermal conductivity and viscosity generated errors
c       eta=xnotc  !values used by GUI as non-convergence flag
c       tcx=xnotc
c       if ((lertt.or.lervt) .and. (lertD.or.lervD)) then
c         ierr=-57
c          write (herr,2057) t,rho
c        else if (lertD.or.lervD) then
c          ierr=-56
c          write (herr,2056) t,rho
c        else
c          ierr=-55
c          write (herr,2055) t,rho
c        end if
c      else if (lertc) then
c  only thermal conductivity generated errors
c       tcx=xnotc  !value used by GUI as non-convergence flag
c       if (lertt .and. lertD) then
c          ierr=-37
c          write (herr,2037) t,rho
c        end if
c      else if (lervs) then
c  only viscosity generated errors
c       eta=xnotc  !value used by GUI as non-convergence flag
c       if (lervt .and. lervD) then
c          ierr=-47
c          write (herr,2047) t,rho
c        end if
c      end if
c
c  process warnings/errors for conformal states outside range of individual fluid
c  viscosity correlations
      lervsj=lervtj.or.lervDj
      if (lervsj) then
c     viscosity generated errors
c     return a number with a warning
c       eta=xnotc  !value used by GUI as non-convergence flag
        ierr=ierrvj
        herr=herrvj
      end if
c
      if (ierrsv.ne.0) then !conftd failed
c       do not return numbers
        ierr=ierrsv
        herr=herrsv
c        eta=xnotc  !values used by GUI as non-convergence flag
c        tcx=xnotc
      endif
      if (ierr.ne.0) call ERRMSG (ierr,herr)
c
c700  continue
      RETURN

c
 1058     format ('[pTRNEC warning 58] 2-D Newton-Raphson method for '
     &        ,'conformal temperature and density did not converge ',
     &        'for component',i3,a1)
c2035     format ('[pTRNEC warning -35] conformal temperature in ECS-'
c    &      ,'transport method is outside range of reference fluid ',
c    &      'thermal conductivity correlation; T_conf =',g11.5,
c    &      ' K; T_min,max =',g11.5,',',g11.5,' K')
c2036     format ('[pTRNEC warning -36] conformal density in ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'thermal conductivity correlation; rho_conf =',g11.5,
c    &      ' mol/L; rho_max =',g11.5,' mol/L')
c2037     format ('[pTRNEC warning -37] T and rho input to ECS-',
c    &      'transport method are outside range of reference fluid ',
c    &      'thermal conductivity correlation; T_in =',g11.5,
c    &      ' K; rho_in =',g11.5,' mol/L')
c2045     format ('[pTRNEC warning -45] conformal temperature in ECS-'
c    &      ,'transport method is outside range of reference fluid ',
c    &      'thermal conductivity correlation; T_conf =',g11.5,
c    &      ' K; T_min,max =',g11.5,',',g11.5,' K')
c2046     format ('[pTRNEC warning -46] conformal density in ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'viscosity correlation; rho_conf =',g11.5,
c    &      ' mol/L; rho_max =',g11.5,' mol/L')
c2047     format ('[pTRNEC warning -47] T and rho input to ECS-',
c    &      'transport method are outside range of reference fluid ',
c    &      'viscosity correlation; T_in =',g11.5,
c    &      ' K; rho_in =',g11.5,' mol/L')
c2055     format ('[pTRNEC warning -55] temperature input to ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'viscosity and thermal conductivity correlations; T_in =',
c    &      g11.5,' K; rho_in =',g11.5,' mol/L')
c2056     format ('[pTRNEC warning -56] density input to ECS-',
c    &      'transport method is outside range of reference fluid ',
c    &      'viscosity and thermal conductivity correlations; T_in =',
c    &      g11.5,' K; rho_in =',g11.5,' mol/L')
c2057     format ('[pTRNEC warning -57] T and/or rho input to ECS-',
c    &      'transport method are outside range of reference fluid ',
c    &      'viscosity and thermal conductivity correlations; T_in =',
c    &      g11.5,' K; rho_in =',g11.5,' mol/L')
c2065     format ('[pTRNEC warning -41] conformal temperature in ECS-'
c    &      ,'transport method is outside range of fluid ',I3,
c    &      ' viscosity correlation')
c2066     format ('[pTRNEC warning -42] conformal density in ECS-',
c    &      'transport method is outside range of fluid ',I3,
c    &      ' viscosity correlation')
c2067     format ('[pTRNEC warning -43] conformal temperature and ',
c    &      'density in ECS-',
c    &      'transport method is outside range of fluid ',I3,
c    &      ' viscosity correlation')
c2068     format ('[pTRNEC warning -48] invalid region for ',
c    &        'viscosity of reference fluid 1 ')

c
      end                                             !subroutine pTRNEC
c ======================================================================
c
      subroutine ETAbkp (jj,t,rho,fj,fx,hj,hx,etabk,ierr,herr)
c
c  compute the background viscosity of pure fluid component in a mixture
c  using ecs, and evaluating with the individual fluids reference
c  fluid, which may differ from that of the mixture
c
c  based on the modification of the Huber-Ely ECS method given by:
c  Klein, S.A., McLinden, M.O. and Laesecke, A. (1997). An improved
c  extended corresponding states method for estimation of viscosity of
c  pure refrigerants and mixtures. Int. J. Refrigeration 20:208-217
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c       fj--array of temperature shape factors for the components
c       fx--temperature shape factor for the mixture
c       hj--array of temperature shape factors for the components
c       hx--density shape factor for the mixture
c  outputs:
c      etabk--background viscosity [uPa.s]
c     ierr--error flag:  0 = successful
c                      <>0 indicates problem from underlying routine
c                          passed to calling routine
c     herr--error string (character*255 variable if ierr<>0)
c
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension fj(ncmax),hj(ncmax)  !reducing ratios for components
c
      ierr=0
      herr=' '
      tj=t*fj(jj)/fx
      rhoj=rho*hx/hj(jj)
c  get ECS value for pure fluid j with respect to its own reference fluid
c  not necessarily the same as the mixture; only residual part needed.
      CALL pTRNEC (jj,tJ,rhoJ,etaj,tcxJ,ierr,herr,-jj)
      etabk=etaj

      RETURN
      end                                             !subroutine ETAbkp
c
c ======================================================================
c
c ======================================================================
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file trns_ECS.f
c ======================================================================
