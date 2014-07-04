c  begin file prop_sub.f
c
c  This file contains the basic (non-iterative) routines to calculate
c  various properties of fluids and mixtures.  These routines must first
c  be initialized by a call to the subroutine SETUP.
c
c  contained here are:
c     subroutine CRITP (x,tc,pc,Dc,ierr,herr)
c     subroutine MAXT (x,tm,pm,Dm,ierr,herr)
c     subroutine MAXP (x,tm,pm,Dm,ierr,herr)
c     subroutine THERM (t,rho,x,p,e,h,s,cv,cp,w,hjt)
c     subroutine THERM2 (t,rho,x,p,e,h,s,cv,cp,w,Z,hjt,A,G,
c    &                   xkappa,beta,dPdrho,d2PdD2,dPT,drhodT,drhodP,
c    &                   d2PT2,d2PdTD,spare3,spare4)
c     subroutine THERM3 (t,rho,x,
c    &           xkappa,beta,xisenk,xkt,betas,bs,xkkt,thrott,pi,spht)
c     subroutine THERM0 (t,rho,x,p0,e0,h0,s0,cv0,cp00,w0,A0,G0)
c     subroutine RESIDUAL (t,rho,x,pr,er,hr,sr,cvr,cpr,Ar,Gr)
c     subroutine ENTRO (t,rho,x,s)
c     subroutine ENTHAL (t,rho,x,h)
c     subroutine ENERGY (t,rho,x,e)
c     subroutine CVCP (t,rho,x,cv,cp)
c     subroutine CVCPK (icomp,t,rho,cv,cp)
c     subroutine GIBBS (t,rho,x,Ar,Gr)
c     subroutine AG (t,rho,x,a,g)
c     subroutine PRESS (t,rho,x,p)
c     subroutine DPDD (t,rho,x,dpdrho)
c     subroutine DPDDK (icomp,t,rho,dpdrho)
c     subroutine DPDD2 (t,rho,x,d2PdD2)
c     subroutine DPDT (t,rho,x,dpt)
c     subroutine DPDTK (icomp,t,rho,dpt)
c     subroutine DDDP (t,rho,x,drhodp)
c     subroutine DDDT (t,rho,x,drhodt)
c     subroutine DERVPVT (t,rho,x,
c    &                    dPdD,dPdT,d2PdD2,d2PdT2,d2PdTD,
c    &                    dDdP,dDdT,d2DdP2,d2DdT2,d2DdPT,
c    &                    dTdP,dTdD,d2TdP2,d2TdD2,d2TdPD)
c     subroutine DHD1(t,rho,x,dhdt_d,dhdt_p,dhdd_t,dhdd_p,dhdp_t,dhdp_d)
c     subroutine FGCTY (t,rho,x,f)
c     subroutine FGCTY2 (t,rho,x,f,ierr,herr)
c     subroutine FUGCOF (t,rho,x,phi,ierr,herr)
c     subroutine CHEMPOT (t,rho,x,u,ierr,herr)
c     subroutine ACTVY (t,rho,x,actv,gamma,ierr,herr)
c     subroutine PHIDERV (iderv,t,rho,x,dadn,dnadn,ierr,herr)
c     subroutine VIRB (t,x,b)
c     subroutine DBDT (t,x,dbt)
c     subroutine DBDT2 (t,x,dbt2)
c     subroutine VIRC (t,x,c)
c     subroutine DCDT (t,x,dct)
c     subroutine DCDT2 (t,x,dct2)
c     subroutine VIRD (t,x,d)
c     subroutine VIRBA (t,x,ba)
c     subroutine VIRCA (t,x,ca)
c     subroutine VIRBCD (t,x,b,c,d)
c     subroutine B12 (t,x,b)
c     subroutine EXCESS (t,p,x,kph,rho,vE,eE,hE,sE,aE,gE,ierr,herr)
c     subroutine FPV (t,rho,p,x,f)
c     subroutine RMIX (x)
c     subroutine RMIX2 (x,Rgas)
c     subroutine SPECGR (t,rho,p,gr)
c     subroutine HEAT (t,rho,x,hg,hn,ierr,herr)
c     subroutine ENTHHC (icmb,t1,t2,h)
c     subroutine ISPURE (x,icomp)
c     subroutine RIEM (t,rho,x,riemc)
c
c  these routines use the following common blocks from other files
c     common /MODEL/ hrefst,heos,hpheq,h2eos(n0:nx),hmixp,htran,hsten
c     common /CREF/ tref(n0:nx),rhoref(n0:nx),href(n0:nx),sref(n0:nx)
c     common /Gcnst/ R,tz(n0:nx),rhoz(n0:nx)
c
c  various arrays are dimensioned with parameter statements
c
c ======================================================================
c ======================================================================
c
      subroutine CRITP (x,tc,pc,Dc,ierr,herr)
c
c  critical parameters as a function of composition
c
c  input:
c        x--composition [array of mol frac]
c  outputs:
c       tc--critical temperature [K]
c       pc--critical pressure [kPa]
c       Dc--critical density [mol/L]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-20-94  MM, original version
c  07-21-95  MM, call CRTBWR instead of accessing arrays directly
c  08-07-95  MM, add call to Fundamental (Helmholtz) EOS
c  09-13-95  MM, add ierr, herr to argument list
c  10-03-95  MM, change /MODEL/--models specified by strings
c  11-02-95  MM, add call mixture Helmholtz model (HMX)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c                add call to ECS model
c  03-19-19  MM, add dipole moment to /CCON/
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  03-07-07 EWL, add check for tc, pc, dc less than 0
c  09-15-11 EWL, remove checks for FEQ, BWR, etc., and just return values from common block
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CRITP
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
        pc=Pcrit(icomp)
        Dc=Dcrit(icomp)
      else
        call CRTHMX (x,tc,pc,Dc,ierr,herr)
      end if
      if (tc.le.0) tc=100.d0
      if (pc.le.0) pc=0.d0
      if (dc.le.0) dc=0.d0
c
      RETURN
      end                                              !subroutine CRITP
c
c ======================================================================
c
      subroutine MAXT (x,tm,pm,Dm,ierr,herr)
c
c  values at the maximum temperature along the saturation line, these are
c  returned from the call to SATSPLN and apply only to the composition x
c  sent to SATSPLN.
c
c  input:
c        x--composition [array of mol frac]
c  outputs:
c       tm--temperature [K]
c       pm--pressure [kPa]
c       Dm--density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  12-01-12 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      tm=300
      pm=0
      dm=0
      ierr=1
      herr='[MAXT error] Maximum temperature not known'
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        call CRITP (x,tm,pm,Dm,ierr,herr)
      else
        if (iSpline.ne.0) then
          if (ierrtmx.eq.0) then
            ix=1
            do i=1,nc
              if (abs(x(i)-xspln(i)).gt.1.d-6) ix=0
            enddo
            if (ix.eq.1) then
              tm=tmxt
              pm=tmxp
              Dm=tmxd
            endif
          endif
        end if
        if (Dm.gt.0.01) then
          ierr=0
          herr=' '
        endif
      end if
c
      RETURN
      end                                               !subroutine MAXT
c
c ======================================================================
c
      subroutine MAXP (x,tm,pm,Dm,ierr,herr)
c
c  values at the maximum pressure along the saturation line, these are
c  returned from the call to SATSPLN and apply only to the composition x
c  sent to SATSPLN.
c
c  input:
c        x--composition [array of mol frac]
c  outputs:
c       tm--temperature [K]
c       pm--pressure [kPa]
c       Dm--density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  12-01-12 EWL, original version
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      tm=300
      pm=0
      dm=0
      ierr=1
      herr='[MAXP error] Maximum pressure not known'
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        call CRITP (x,tm,pm,Dm,ierr,herr)
      else
        if (iSpline.ne.0) then
          if (ierrpmx.eq.0) then
            ix=1
            do i=1,nc
              if (abs(x(i)-xspln(i)).gt.1.d-6) ix=0
            enddo
            if (ix.eq.1) then
              tm=pmxt
              pm=pmxp
              Dm=pmxd
            endif
          endif
        end if
        if (Dm.gt.0.01) then
          ierr=0
          herr=' '
        endif
      end if
c
      RETURN
      end                                               !subroutine MAXP
c
c ======================================================================
c
      subroutine THERM (t,rho,x,p,e,h,s,cv,cp,w,hjt)
c
c  compute thermal quantities as a function of temperature, density,
c  and compositions using core functions (Helmholtz free energy, ideal
c  gas heat capacity and various derivatives and integrals)
c
c  Based on derivations in Younglove & McLinden, JPCRD 23 #5, 1994,
c  Appendix A for pressure-explicit equations (e.g. MBWR) and
c  Baehr & Tillner-Roth, Thermodynamic Properties of Environmentally
c  Acceptable Refrigerants, Berlin:  Springer-Verlag (1995) for
c  Helmholtz-explicit equations (e.g. FEQ).
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        p--pressure [kPa]
c        e--internal energy [J/mol]
c        h--enthalpy [J/mol]
c        s--entropy [J/mol-K]
c       Cv--isochoric heat capacity [J/mol-K]
c       Cp--isobaric heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c      hjt--isenthalpic Joule-Thomson coefficient [K/kPa]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  10-11-94  MM, original version
c  08-04-95  MM, add calls to Fundamental (Helmholtz) EOS
c  10-03-95  MM, change /MODEL/--models specified by strings
c  10-10-95  MM, compute ideal gas pressure and pass to PHI0
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-06-95  MM, add calls to mixture ideal gas function
c  11-08-95  MM, convert calls to PHI0, CP0, CPI, CPT to mixture form
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-13-95  MM, compute entropy using Cp0, etc rather than PHI0
c  01-18-96  MM, fix s and h ref state for HMX: s = s - sum[x(i)*sref(i)]
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-19-96  MM, add dipole moment to /CCON/
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  04-19-96  MM, change call to PHI0:  pass rho instead of pideal
c                calculate e,h,s using PHI0 rather than Cp0
c  07-05-96  MM, change e, Cv:  PHI0 returns tau*d(phi0)/d(tau), etc.
c  04-22-97  MM, lower bound on rho for s calc set to 1.0d-20
c  10-01-97  MM, add compiler switch to allow access by DLL
c  03-30-98 EWL, add Joule-Thomson coeff to MBWR case
c  03-31-98  MM, special case for Joule-Thomson coeff for rho = 0
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-02-98 EWL, remove compositional dependence for pure fluids
c  12-02-98 EWL, restructure to closely mimic THERM2
c  12-22-98 EWL, recalculate R for mixtures based on values for pure fluids
c  05-25-00 EWL, moved calculation of Z to bottom AFTER p is calculated!
c  09-00-00 EWL, removed the del from del*phi01, etc.  The del's and tau's
c                are now included in the core routines.  Put the reducing
c                variables tz and rhoz directly in the common blocks.
c  09-05-02 EWL, change check on rho from 1.d-10 to 1.0d-8 to avoid
c                zero's coming back from core_feq at low rhos.
c  10-04-06 EWL, change remaining checks on rho from 1.d-10 to 1.0d-8
c  06-18-07 EWL, change all checks for rho in prop_sub.for from 1.d-20 to 1.d-40
c                for ideal gas situations
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THERM
c
      include 'commons.for'
      dimension x(ncmax)
c
      call RMIX (x)
      RT=R*t
      w2=0.d0
      p=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      hjt=0.d0
      if (t.le.0.d0) return
      if (rho.lt.1.0d-40) then
c  entropy calc will crash if rho = 0
        rhos=1.0d-40
      else
        rhos=rho
      end if
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
        a=ABWR(icomp,t,rho)
        dadt=DABWR(icomp,t,rho)
        cpiint=CPI(t,x)
        cptint=CPT(t,x)
        dPdrho=DPDBWR(icomp,t,rho)
        dPT=DPTBWR(icomp,t,rho)
        e=a-t*dadt
c    &    +cpiint-R*(t-tref(icomp))  ! R*tref is const, merge w/ href
     &    +cpiint-RT
     &    -href(icomp)
c
        s=-dadt+R*log(rhoref(icomp)/rhos)+cptint-R*log(t/tref(icomp))
     &    -sref(icomp)
        cv=-t*D2ABWR(icomp,t,rho)+CP0(t,x)-R
        if (rho.gt.1.0d-8) then
          cp=cv+t/rho**2*dPT**2/dPdrho
        else
          cp=CP0(t,x)
        end if
        if (rho.gt.1.0d-8) then
          h=e+p/rho
          hjt=(t/rho*dPT/dPdrho-1.0d0)/rho/cp
        else
          h=e+RT
          call VIRB (t,x,b)
          call DBDT (t,x,dbt)
          hjt=(dbt*t-b)/cp
        end if
c  if any of the factors in speed of sound are negative (e.g. in two-
c  phase region) return 0.0
c       w=SQRT(1.0d3/wm*cp/cv*dPdrho)
        w2=cp/cv*dPdrho
        if (w2.gt.0.0d0) then
          w=SQRT(1.0d3/wmas(icomp)*w2)
        else
          w=0.0d0
        end if
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi00=PHIK(icomp,0,0,tau,del)  !real-gas terms
          phi01=PHIK(icomp,0,1,tau,del)
          phi10=PHIK(icomp,1,0,tau,del)
          phi11=PHIK(icomp,1,1,tau,del)
          phi02=PHIK(icomp,0,2,tau,del)
          phi20=PHIK(icomp,2,0,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi00=PHIX(0,0,tau,del,x)   !real-gas terms
          phi01=PHIX(0,1,tau,del,x)
          phi10=PHIX(1,0,tau,del,x)
          phi11=PHIX(1,1,tau,del,x)
          phi02=PHIX(0,2,tau,del,x)
          phi20=PHIX(2,0,tau,del,x)
        end if
c       write (*,1003) t,rho,phi00,phi01,phi10,phi11,phi02,phi20
c1003   format (1x,' THERM--t,rho,PHIs:   ',f8.2,f12.6,6d16.6)
c
        phig00=PHI0(0,0,t,rhos,x)      !ideal-gas terms
        phig10=PHI0(1,0,t,rho,x)
        phig20=PHI0(2,0,t,rho,x)
c       write (*,1005) (x(j),j=1,ncmax)
c1005   format (1x,' THERM--output x(i): ',5f14.8)
c       write (*,1024) phig00,phig10,phig20
c1024   format (1x,' THERM--phig-00/01/02:',20x,3d16.6)
        p=RT*rho*(1.0d0+phi01)
        e=RT*(phig10+phi10)
        if (icomp.ne.0) then
          e=e-href(icomp)
        else
          do i=1,nc
            e=e-x(i)*href(i)
          enddo
        endif
        h=e+RT*(1.0d0+phi01)
        s=R*(phig10+phi10-phig00-phi00)
c       write (*,*) ' THERM--t,rho,x,s,sref:  ',t,rho,x(1),s,sref(1)
        if (icomp.ne.0) then
          s=s-sref(icomp)
        else
          do i=1,nc
            s=s-x(i)*sref(i)
          enddo
        endif
        cv=-R*(phi20+phig20)
c       write (*,*) ' THERM--tau,del,Cv_resid:  ',tau,del,phi20
        cp=cv+R*(1.0d0+phi01-phi11)**2/(1.0d0+2.0d0*phi01+phi02)
        if (cv.gt.0.d0) w2=RT*cp/cv*(1.0d0+2.0d0*phi01+phi02)
c  if any of the factors in speed of sound are negative (e.g. in two-
c  phase region) return 0.0
        if (w2.gt.0.0d0) then
          w=SQRT(w2*1.0d3/WMOL(x))  !convert from molar to mass units
        else
          w=0.0d0
        end if
        if (rho.gt.1.0d-8) then
          hjt=-1.0d0/(cp*rho)*(phi01+phi02+phi11)/
     &        (1.0d0+2.0d0*phi01+phi02)
        else
          call VIRB (t,x,b)
          call DBDT (t,x,dbt)
          hjt=(dbt*t-b)/cp
        end if
      end if
c
      RETURN
      end                                              !subroutine THERM
c
c ======================================================================
c
      subroutine THERM2 (t,rho,x,p,e,h,s,cv,cp,w,Z,hjt,A,G,
     &                   xkappa,beta,dPdrho,d2PdD2,dPT,drhodT,drhodP,
     &                   d2PT2,d2PdTD,spare3,spare4)
c
c  compute thermal quantities as a function of temperature, density,
c  and compositions using core functions (Helmholtz free energy, ideal
c  gas heat capacity and various derivatives and integrals)
c
c  this routine is the same as THERM, except that additional properties
c  are calculated
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        p--pressure [kPa]
c        e--internal energy [J/mol]
c        h--enthalpy [J/mol]
c        s--entropy [J/mol-K]
c       Cv--isochoric heat capacity [J/mol-K]
c       Cp--isobaric heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c        Z--compressibility factor (= PV/RT) [dimensionless]
c      hjt--isenthalpic Joule-Thomson coefficient [K/kPa]
c        A--Helmholtz energy [J/mol]
c        G--Gibbs free energy [J/mol]
c   xkappa--isothermal compressibility (= -1/V dV/dP = 1/rho dD/dP) [1/kPa]
c     beta--volume expansivity (= 1/V dV/dT = -1/rho dD/dT) [1/K]
c   dPdrho--derivative dP/drho [kPa-L/mol]
c   d2PdD2--derivative d^2P/drho^2 [kPa-L^2/mol^2]
c      dPT--derivative dP/dT [kPa/K]
c   drhodT--derivative drho/dT [mol/(L-K)]
c   drhodP--derivative drho/dP [mol/(L-kPa)]
c    d2PT2--derivative d2P/dT2 [kPa/K^2]
c   d2PdTD--derivative d2P/dTd(rho) [J/mol-K]
c   sparei--2 space holders for possible future properties
c
c  written by M. McLinden, NIST Physical & Chem Properties Div, Boulder, CO
c  03-16-98  MM, original version; based on THERM
c  03-30-98 EWL, add Joule-Thomson coeff to MBWR case
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-02-98 EWL, remove compositional dependence for pure fluids
c  12-02-98 EWL, restructure to closely mimic THERM
c  12-02-98 EWL, add the reference states to the calculation of A and G
c  12-22-98 EWL, recalculate R for mixtures based on values for pure fluids
c  02-11-99 EWL, skip calculation of d2PdD2 if rho=0
c  04-27-01 EWL, change order of calculation of a and g in BWR section
c  04-27-01 DGF, change sign before sref in the calculation of a and g
c  09-05-02 EWL, add ideal gas isothermal compressibility and d2pdD2
c  09-05-02 EWL, change check on rho from 1.d-10 to 1.0d-8 to avoid
c                zero's coming back from core_feq at low rhos.
c  06-14-06 EWL, change dPdD to dPdrho, etc, to avoid compiler problems with subroutine dPdD, etc.
c  03-04-08 EWL, add checks for T=0
c  12-04-09 EWL, add d2P/dT2
c  03-26-10 EWL, add d2P/dTdrho
c  01-03-11 EWL, fixed d2P/dT2 calculation (divided by T)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THERM2
c
      include 'commons.for'
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      spare3=xnotc  !flag indicating not calculated
      spare4=xnotc
      p=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      hjt=0.d0
      Z=0.d0
      A=0.d0
      G=0.d0
      xkappa=0.d0
      beta=0.d0
      dPdrho=0.d0
      d2PdD2=0.d0
      dPT=0.d0
      drhodT=0.d0
      drhodP=0.d0
      d2PT2=0.d0
      d2PdTD=0.d0
      if (t.le.0.d0) return
c
      call RMIX (x)
      RT=R*t
      call DBDT (t,x,dbt)
      call VIRB (t,x,b)
      rhos=rho
c  entropy calc will crash if rho = 0
      if (rho.lt.1.0d-40) rhos=1.0d-20
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
        ar=ABWR(icomp,t,rho)
        dadt=DABWR(icomp,t,rho)
        cpiint=CPI(t,x)
        cptint=CPT(t,x)
        dPdrho=DPDBWR(icomp,t,rho)
        dPT=DPTBWR(icomp,t,rho)
        e=ar-t*dadt
c    &    +cpiint-R*(t-tref(icomp))  ! R*tref is const, merge w/ href
     &    +cpiint-RT
     &    -href(icomp)
c  additional properties added to THERM2 not in THERM
        d2PdD2=D2PBWR(icomp,t,rho)
        drhodT=-dPT/dPdrho
        drhodP=1.0d0/dPdrho
c
        s=-dadt+R*log(rhoref(icomp)/rhos)+cptint-R*log(t/tref(icomp))
     &    -sref(icomp)
        cv=-t*D2ABWR(icomp,t,rho)+CP0(t,x)-R
        A=e-t*s
        if (rho.gt.1.0d-8) then
          G=A+p/rho
          beta=-drhodT/rho
          xkappa=drhodP/rho
        else
          G=A+R*t
          beta=1.0d0/t  !if rho = 0, then ideal-gas behavior
          xkappa=xnotc
          if (p.gt.0.d0) xkappa=1.d0/p
        end if
        if (rho.gt.1.0d-8) then
          cp=cv+t/rho**2*dPT**2/dPdrho
        else
          cp=CP0(t,x)
        end if
        if (rho.gt.1.0d-8) then
          h=e+p/rho
          hjt=(t/rho*DPTBWR(icomp,t,rho)/dPdrho-1.0d0)/rho/cp
        else
          h=e+RT
          hjt=(dbt*t-b)/cp
        end if
c  if any of the factors in speed of sound are negative (e.g. in two-
c  phase region) return 0.0
c       w=SQRT(1.0d3/wm*cp/cv*dPdrho)
        w2=cp/cv*dPdrho
        if (w2.gt.0.0d0) then
          w=SQRT(1.0d3/wmas(icomp)*w2)
        else
          w=0.0d0
        end if
c
      else
c  call general PHIK or PHIX routines for all other models
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi00=PHIK(icomp,0,0,tau,del)  !real-gas terms
          phi01=PHIK(icomp,0,1,tau,del)
          phi10=PHIK(icomp,1,0,tau,del)
          phi11=PHIK(icomp,1,1,tau,del)
          phi12=PHIK(icomp,1,2,tau,del)
          phi21=PHIK(icomp,2,1,tau,del)
          phi02=PHIK(icomp,0,2,tau,del)
          phi20=PHIK(icomp,2,0,tau,del)
          phi03=PHIK(icomp,0,3,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi00=PHIX(0,0,tau,del,x)   !real-gas terms
          phi01=PHIX(0,1,tau,del,x)
          phi10=PHIX(1,0,tau,del,x)
          phi11=PHIX(1,1,tau,del,x)
          phi12=PHIX(1,2,tau,del,x)
          phi21=PHIX(2,1,tau,del,x)
          phi02=PHIX(0,2,tau,del,x)
          phi20=PHIX(2,0,tau,del,x)
          phi03=PHIX(0,3,tau,del,x)
        end if
c       write (*,1003) t,rho,phi00,phi01,phi10,phi11,phi02,phi20
c1003   format (1x,' THERM--t,rho,PHIs:   ',f8.2,f12.6,6d16.6)
c
        phig00=PHI0(0,0,t,rhos,x)      !ideal-gas terms
        phig10=PHI0(1,0,t,rho,x)
        phig20=PHI0(2,0,t,rho,x)
c       write (*,1005) (x(j),j=1,ncmax)
c1005   format (1x,' THERM--output x(i): ',5f14.8)
c       write (*,1024) phig00,phig10,phig20
c1024   format (1x,' THERM--phig-00/01/02:',20x,3d16.6)
        p=RT*rho*(1.0d0+phi01)
        e=RT*(phig10+phi10)
        if (icomp.ne.0) then
          e=e-href(icomp)
        else
          do i=1,nc
            e=e-x(i)*href(i)
          enddo
        endif
        h=e+RT*(1.0d0+phi01)
        s=R*(phig10+phi10-phig00-phi00)
c       write (*,*) ' THERM--t,rho,x,s,sref:  ',t,rho,x(1),s,sref(1)
        if (icomp.ne.0) then
          s=s-sref(icomp)
        else
          do i=1,nc
            s=s-x(i)*sref(i)
          enddo
        endif
        cv=-R*(phi20+phig20)
c       write (*,*) ' THERM--tau,del,Cv_resid:  ',tau,del,phi20
        cp=cv+R*(1.0d0+phi01-phi11)**2/
     &     (1.0d0+2.0d0*phi01+phi02)
        w2=RT*cp/cv*(1.0d0+2.0d0*phi01+phi02)
c  if any of the factors in speed of sound are negative (e.g. in two-
c  phase region) return 0.0
        if (w2.gt.0.0d0) then
          w=SQRT(w2*1.0d3/WMOL(x))  !convert from molar to mass units
        else
          w=0.0d0
        end if
        if (rho.gt.1.0d-8) then
          hjt=-1.0d0/(cp*rho)*(phi01+phi02+phi11)/
     &        (1.0d0+2.0d0*phi01+phi02)
        else
          hjt=(dbt*t-b)/cp
        end if
c  additional properties added to THERM2 not in THERM
        A=RT*(phi00+phig00)
        G=A+RT*(1.0d0+phi01)
        if (icomp.ne.0) then
          a=a-href(icomp)+sref(icomp)*t
          g=g-href(icomp)+sref(icomp)*t
        else
          do i=1,nc
            a=a-x(i)*(href(i)-sref(i)*t)
            g=g-x(i)*(href(i)-sref(i)*t)
          enddo
        endif
        dPdrho=RT*(1.0d0+2.0d0*phi01+phi02)
        dPT=R*rho*(1.0d0+phi01-phi11)
        d2PT2=R*rho*phi21/T        !d2P/dT2=d3A/dT2dV=1/T*dCv/dV
        d2PdTD=R*(1.d0+2.d0*phi01+phi02-2.d0*phi11-phi12)
        drhodP=1.0d0/(RT*(1.0d0+2.0d0*phi01+phi02))
        drhodT=-rho*(1.0d0+phi01-phi11)/(t*(1.0d0+2.0d0*phi01+phi02))
        if (rho.gt.1.0d-8) then
          d2PdD2=RT/rho*(2.0d0*phi01+4.0d0*phi02+phi03)
          beta=-drhodT/rho
          xkappa=drhodP/rho
        else
          d2PdD2=2.d0*b*R*t
          beta=xnotc
          beta=1.0d0/t  !if rho = 0, then ideal-gas behavior
          xkappa=xnotc
          if (p.gt.0.d0) xkappa=1.d0/p
        end if
      end if
      if (rho.lt.1.0d-40) then
        Z=1.0d0       !if rho = 0, then ideal-gas behavior
      else
        Z=p/(RT*rho)
      end if
c
      RETURN
      end                                             !subroutine THERM2
c
c ======================================================================
c
      subroutine THERM0 (t,rho,x,p0,e0,h0,s0,cv0,cp00,w0,A0,G0)
c
c  compute ideal gas thermal quantities as a function of temperature, density,
c  and compositions using core functions
c
c  this routine is the same as THERM, except it only calculates ideal gas
c  properties (Z=1) at any temperature and density
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c       p0--pressure [kPa]
c       e0--internal energy [J/mol]
c       h0--enthalpy [J/mol]
c       s0--entropy [J/mol-K]
c      Cv0--isochoric heat capacity [J/mol-K]
c     Cp00--isobaric heat capacity [J/mol-K]
c       w0--speed of sound [m/s]
c       A0--Helmholtz energy [J/mol]
c       G0--Gibbs free energy [J/mol]
c
c  11-26-02 EWL, original version; based on THERM
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THERM0
c
      include 'commons.for'
      dimension x(ncmax)
c
      p0=0.d0
      e0=0.d0
      h0=0.d0
      s0=0.d0
      cv0=0.d0
      cp00=0.d0
      w0=0.d0
      A0=0.d0
      G0=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      RT=R*t
      rhos=rho
c  entropy calc will crash if rho = 0
      if (rho.lt.1.0d-40) rhos=1.0d-20
      phig00=PHI0(0,0,t,rhos,x)      !ideal-gas terms
      phig10=PHI0(1,0,t,rho,x)
      phig20=PHI0(2,0,t,rho,x)
      p0=RT*rho
      e0=RT*phig10
      s0=R*(phig10-phig00)
      A0=RT*phig00
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        e0=e0-href(icomp)
        s0=s0-sref(icomp)
        a0=a0-href(icomp)+sref(icomp)*t
      else
        do i=1,nc
          e0=e0-x(i)*href(i)
          s0=s0-x(i)*sref(i)
          a0=a0-x(i)*(href(i)-sref(i)*t)
        enddo
      endif
      cv0=-R*phig20
      cp00=cv0+R
      h0=e0+RT
      G0=A0+RT
      w2=RT*cp00/cv0
      if (w2.gt.0.0d0) then
        w0=SQRT(w2*1.0d3/WMOL(x))
      else
        w0=0.0d0
      end if
c
      RETURN
      end                                             !subroutine THERM0
c
c ======================================================================
c
      subroutine RESIDUAL (t,rho,x,pr,er,hr,sr,cvr,cpr,Ar,Gr)
c
c  compute the residual quantities as a function of temperature, density,
c  and compositions (where the residual is the property minus the ideal
c  gas portion).
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c       pr--residual pressure [kPa]  (p-rho*R*T)
c       er--residual internal energy [J/mol]
c       hr--residual enthalpy [J/mol]
c       sr--residual entropy [J/mol-K]
c      Cvr--residual isochoric heat capacity [J/mol-K]
c      Cpr--residual isobaric heat capacity [J/mol-K]
c       Ar--residual Helmholtz energy [J/mol]
c       Gr--residual Gibbs free energy [J/mol]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  07-07-10 EWL, original version; based on THERM2
c
cx DEC$ ATTRIBUTES DLLEXPORT :: RESIDUAL
c
      include 'commons.for'
      dimension x(ncmax)
c
      pr=0.d0
      er=0.d0
      hr=0.d0
      sr=0.d0
      cvr=0.d0
      cpr=0.d0
      Ar=0.d0
      Gr=0.d0
      if (t.le.0.d0) return
      call ISPURE (x,icomp)
      call RMIX (x)
      if (icomp.ne.0) then
c  pure fluid
        tau=tz(icomp)/t
        del=rho/rhoz(icomp)
        phi00=PHIK(icomp,0,0,tau,del)  !real-gas terms
        phi01=PHIK(icomp,0,1,tau,del)
        phi10=PHIK(icomp,1,0,tau,del)
        phi11=PHIK(icomp,1,1,tau,del)
        phi02=PHIK(icomp,0,2,tau,del)
        phi20=PHIK(icomp,2,0,tau,del)
      else
c  mixture
        call REDX (x,t0,rho0)
        tau=t0/t
        del=rho/rho0
        phi00=PHIX(0,0,tau,del,x)   !real-gas terms
        phi01=PHIX(0,1,tau,del,x)
        phi10=PHIX(1,0,tau,del,x)
        phi11=PHIX(1,1,tau,del,x)
        phi02=PHIX(0,2,tau,del,x)
        phi20=PHIX(2,0,tau,del,x)
      end if
c
      RT=R*T
      pr=RT*rho*phi01
      er=RT*phi10
      hr=er+RT*phi01
      sr=R*(phi10-phi00)
      cvr=-R*phi20
      cpr=cvr+R*(1.0d0+phi01-phi11)**2/(1.0d0+2.0d0*phi01+phi02)-R
      Ar=RT*phi00
      Gr=Ar+RT*phi01
c
      RETURN
      end                                           !subroutine RESIDUAL
c
c ======================================================================
c
      subroutine ENTRO (t,rho,x,s)
c
c  compute entropy as a function of temperature, density and composition
c  using core functions (temperature derivative of Helmholtz free energy
c  and ideal gas integrals)
c
c  based on derivations in Younglove & McLinden, JPCRD 23 #5, 1994,
c  equations A5, A19 - A26
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c        s--entropy [J/mol-K]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  10-05-94  MM, original version
c  10-03-95  MM, change /MODEL/--models specified by strings
c  10-10-95  MM, compute ideal gas pressure and pass to PHI0
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-08-95  MM, convert calls to PHI0, CP0, CPI, CPT to mixture form
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  01-19-96  MM, fix ref state for HMX: s = s - sum[x(i)*sref(i)]
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  04-19-96  MM, change call to PHI0:  pass rho instead of pideal
c                calculate s using PHI0 rather than Cp0
c  07-19-96  MM, change general calls to PHI0 (same as THERM)
c  04-22-97  MM, lower bound on rho for s calc set to 1.0d-20
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-02-98 EWL, remove compositional dependence for pure fluids
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ENTRO
c
      include 'commons.for'
      dimension x(ncmax)
c
      s=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (rho.lt.1.0d-40) then
c  entropy calc will crash if rho = 0
        rhos=1.0d-40
      else
        rhos=rho
      end if
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        s=-DABWR(icomp,t,rho)+R*log(rhoref(icomp)/rhos)+CPT(t,x)
     &    -R*log(t/tref(icomp))-sref(icomp)
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi00=PHIK(icomp,0,0,tau,del)  !real-gas terms
          phi10=PHIK(icomp,1,0,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi00=PHIX(0,0,tau,del,x)  !real-gas terms
          phi10=PHIX(1,0,tau,del,x)
        end if
c
        phig00=PHI0(0,0,t,rhos,x)      !ideal-gas terms
        phig10=PHI0(1,0,t,rho,x)
        s=R*(phig10+phi10-phig00-phi00)
        if (icomp.ne.0) then
          s=s-sref(icomp)
        else
          do i=1,nc
            s=s-x(i)*sref(i)
          enddo
        endif
      end if
c
      RETURN
      end                                              !subroutine ENTRO
c
c ======================================================================
c
      subroutine ENTHAL (t,rho,x,h)
c
c  compute enthalpy as a function of temperature, density, and
c  composition using core functions (Helmholtz free energy and ideal
c  gas integrals)
c
c  based on derivations in Younglove & McLinden, JPCRD 23 #5, 1994,
c  equations A7, A18, A19
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c        h--enthalpy [J/mol]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  10-06-94  MM, original version
c  10-03-95  MM, change /MODEL/--models specified by strings
c  10-10-95  MM, compute ideal gas pressure and pass to PHI0
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-08-95  MM, convert calls to PHI0, CP0, CPI, CPT to mixture form
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  01-19-96  MM, fix ref state for HMX: h = h - sum[x(i)*href(i)]
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  07-19-96  MM, change general calls to PHI0 (same as THERM)
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-02-98 EWL, remove compositional dependence for pure fluids
c  04-05-00 EWL, check for rho=0 and avoid division by zero
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ENTHAL
c
      include 'commons.for'
      dimension x(ncmax)
c
      h=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        if (rho.lt.1.d-8) then
          h=ABWR(icomp,t,rho)-t*DABWR(icomp,t,rho)+CPI(t,x)-href(icomp)
        else
          h=ABWR(icomp,t,rho)-t*DABWR(icomp,t,rho)+
     &     PBWR(icomp,t,rho)/rho-R*t+CPI(t,x)-href(icomp)
        endif
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)
          phi10=PHIK(icomp,1,0,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi10=PHIX(1,0,tau,del,x)
        end if
c
        RT=R*t
        phig10=PHI0(1,0,t,rho,x)
        e=RT*(phig10+phi10)
        if (icomp.ne.0) then
          e=e-href(icomp)
        else
          do i=1,nc
            e=e-x(i)*href(i)
          enddo
        endif
        h=e+RT*(1.0d0+phi01)
      end if
c
      RETURN
      end                                             !subroutine ENTHAL
c
c ======================================================================
c
      subroutine ENERGY (t,rho,x,e)
c
c  compute energy as a function of temperature, density, and
c  composition using core functions (Helmholtz free energy and ideal
c  gas integrals)
c
c  based on derivations in Younglove & McLinden, JPCRD 23 #5, 1994
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c        e--energy [J/mol]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  12-13-00 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ENERGY
c
      include 'commons.for'
      dimension x(ncmax)
c
      e=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        e=ABWR(icomp,t,rho)-t*DABWR(icomp,t,rho)-R*t+CPI(t,x)
     &   -href(icomp)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi10=PHIK(icomp,1,0,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi10=PHIX(1,0,tau,del,x)
        end if
c
        phig10=PHI0(1,0,t,rho,x)
        e=R*t*(phig10+phi10)
        if (icomp.ne.0) then
          e=e-href(icomp)
        else
          do i=1,nc
            e=e-x(i)*href(i)
          enddo
        endif
      end if
c
      RETURN
      end                                             !subroutine ENERGY
c
c ======================================================================
c
      subroutine CVCP (t,rho,x,cv,cp)
c
c  compute isochoric (constant volume) and isobaric (constant pressure)
c  heat capacity as functions of temperature, density, and composition
c  using core functions
c
c  based on derivations in Younglove & McLinden, JPCRD 23 #5, 1994,
c  equation A15, A16
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c       cv--isochoric heat capacity [J/mol-K]
c       cp--isobaric heat capacity [J/mol-K]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  10-06-94  MM, original version
c  10-03-95  MM, change /MODEL/--models specified by strings
c  10-10-95  MM, compute ideal gas pressure and pass to PHI0
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-08-95  MM, convert calls to PHI0, CP0, CPI, CPT to mixture form
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  07-19-96  MM, change general calls to PHI0 (same as THERM)
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CVCP
c
      include 'commons.for'
      dimension x(ncmax)
c
      cv=0.d0
      cp=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        cv=-t*D2ABWR(icomp,t,rho)+CP0(t,x)-R
        if (rho.gt.1.0d-8) then
          cp=cv+t/rho**2*DPTBWR(icomp,t,rho)**2/DPDBWR(icomp,t,rho)
        else
          cp=cv+R
        end if
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi02=PHIK(icomp,0,2,tau,del)
          phi11=PHIK(icomp,1,1,tau,del)
          phi20=PHIK(icomp,2,0,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi02=PHIX(0,2,tau,del,x)
          phi11=PHIX(1,1,tau,del,x)
          phi20=PHIX(2,0,tau,del,x)
        end if
c
        phig20=PHI0(2,0,t,rho,x)         !ideal-gas term
        cv=-R*(phi20+phig20)
        cp=cv+R*(1.0d0+phi01-phi11)**2/
     &     (1.0d0+2.0d0*phi01+phi02)
      end if
c
      RETURN
      end                                               !subroutine CVCP
c
c ======================================================================
c
      subroutine CVCPK (icomp,t,rho,cv,cp)
c
c  compute isochoric (constant volume) and isobaric (constant pressure)
c  heat capacity as functions of temperature for a given component
c
c  analogous to CVCP, except for component icomp, this is used by transport
c  routines to calculate Cv & Cp for the reference fluid (component zero)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c       cv--isochoric heat capacity [J/mol-K]
c       cp--isobaric heat capacity [J/mol-K]
c
c  written by M. McLinden, NIST Physical & Chem Properties Div, Boulder, CO
c  06-16-97  MM, original version; based on CVCP
c  10-01-97  MM, add compiler switch to allow access by DLL
c  03-06-98  MM, check hmxeos, not heos, for 'BWR' (crash for icomp = 0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CVCPK
c
      include 'commons.for'
c
      cv=0.d0
      cp=0.d0
      if (t.le.0.d0) return
      if (hmxeos(icomp).eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        if (rho.gt.1.0d-8) then
          cv=-t*D2ABWR(icomp,t,rho)+CP0K(icomp,t)-R
          cp=cv+t/rho**2*DPTBWR(icomp,t,rho)**2/DPDBWR(icomp,t,rho)
        else
          cp=CP0K(icomp,t)
          cv=cp-R
        end if
c
      else
c  call general PHIK or PHIX routines for all other models
        tau=tz(icomp)/t
        del=rho/rhoz(icomp)
        phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
        phi02=PHIK(icomp,0,2,tau,del)
        phi11=PHIK(icomp,1,1,tau,del)
        phi20=PHIK(icomp,2,0,tau,del)
c
        phig20=PHI0K(icomp,2,0,t,rho)  !ideal-gas term
        cv=-R*(phi20+phig20)
        cp=cv+R*(1.0d0+phi01-phi11)**2/(1.0d0+2.0d0*phi01+phi02)
      end if
c
      RETURN
      end                                              !subroutine CVCPK
c
c ======================================================================
c
      subroutine GIBBS (t,rho,x,Ar,Gr)
c
c  compute residual Helmholtz and Gibbs free energy as a function of
c  temperature, density, and composition using core functions
c
c  N.B.  The quantity calculated is
c
c        G(T,rho) - G0(T,P*) = G(T,rho) - G0(T,rho) + RTln(RTrho/P*)
c
c        where G0 is the ideal gas state and P* is a reference pressure
c        which is equal to the current pressure of interest.  Since Gr
c        is used only as a difference in phase equilibria calculations
c        where the temperature and pressure of the phases are equal, the
c        (RT/P*) part of the log term will cancel and is omitted.
c
c        "normal" (not residual) A and G are computed by subroutine AG
c
c  based on derivations in Younglove & McLinden, JPCRD 23 #5, 1994,
c  equations A8 - A12
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c       Ar--residual Helmholtz free energy [J/mol]
c       Gr--residual Gibbs free energy [J/mol]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  10-07-94  MM, original version
c  08-07-95  MM, add calls to Fundamental (Helmholtz) EOS
c  10-03-95  MM, change /MODEL/--models specified by strings
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: GIBBS
c
      include 'commons.for'
      dimension x(ncmax)
c
      Ar=0.d0
      Gr=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        Ar=ABWR(icomp,t,rho)
        Gr=Ar+PBWR(icomp,t,rho)/rho+R*t*(-1.0d0+log(rho))
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi00=PHIK(icomp,0,0,tau,del)  !real-gas terms
          phi01=PHIK(icomp,0,1,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi00=PHIX(0,0,tau,del,x)  !real-gas terms
          phi01=PHIX(0,1,tau,del,x)
        end if
c
        RT=R*t
        Ar=RT*phi00
        Gr=Ar+RT*(1.0d0+phi01)+RT*(-1.0d0+log(rho))
      end if
c
      RETURN
      end                                              !subroutine GIBBS
c
c ======================================================================
c
      subroutine AG (t,rho,x,a,g)
c
c  compute Helmholtz and Gibbs energies as a function of temperature,
c  density, and composition.
c
c  N.B.  These are not residual values (those are calculated by GIBBS).
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        a--Helmholtz energy [J/mol]
c        g--Gibbs free energy [J/mol]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c   3-27-98 EWL, original version
c  12-02-98 EWL, reorganize code so to eliminate x(i) in pure fluid calculation
c
cx DEC$ ATTRIBUTES DLLEXPORT :: AG
c
      include 'commons.for'
      dimension x(ncmax)
c
      a=0.d0
      g=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (rho.lt.1.0d-40) then
c  calc will crash if rho = 0
        rhos=1.0d-40
      else
        rhos=rho
      end if
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
        a=ABWR(icomp,t,rho)
        cpiint=CPI(t,x)
        cptint=CPT(t,x)
        a=a+cpiint-R*t-href(icomp)
     &   -t*(R*log(rhoref(icomp)/rhos)+cptint-R*log(t/tref(icomp))
     &    -sref(icomp))
        if (rho.gt.0.d0) then
          g=a+p/rho
        else
          g=a+R*t
        endif
c
      else
        RT=R*t
        phig00=PHI0(0,0,t,rhos,x)      !ideal-gas terms
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi00=PHIK(icomp,0,0,tau,del)  !real-gas terms
          phi01=PHIK(icomp,0,1,tau,del)
          a=RT*(phig00+phi00)-(href(icomp)-t*sref(icomp))
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi00=PHIX(0,0,tau,del,x)   !real-gas terms
          phi01=PHIX(0,1,tau,del,x)
          a=RT*(phig00+phi00)
          do i=1,nc
            a=a-x(i)*(href(i)-t*sref(i))
          enddo
        end if
        g=a+RT*(1.0d0+phi01)
      end if
c
      RETURN
      end                                                 !subroutine AG
c
c ======================================================================
c
      subroutine PRESS (t,rho,x,p)
c
c  compute pressure as a function of temperature,
c  density, and composition using core functions
c
c  direct implementation of core function of corresponding model
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c        p--pressure [kPa]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-19-94  MM, original version
c  08-07-95  MM, add calls to Fundamental (Helmholtz) EOS
c  10-03-95  MM, change /MODEL/--models specified by strings
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PRESS
c
      include 'commons.for'
      dimension x(ncmax)
c
      p=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          p=R*t*rho*(1.0d0+PHIK(icomp,0,1,tau,del))
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          p=R*t*rho*(1.0d0+PHIX(0,1,tau,del,x))
        end if
      end if
c
      RETURN
      end                                              !subroutine PRESS
c
c ======================================================================
c
      subroutine DPDD (t,rho,x,dpdrho)
c
c  compute partial derivative of pressure w.r.t. density at constant
c  temperature as a function of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c   dpdrho--dP/drho [kPa-L/mol]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  04-23-95  MM, original version
c  08-07-95  MM, add calls to Fundamental (Helmholtz) EOS
c  10-03-95  MM, change /MODEL/--models specified by strings
c  11-03-95  MM, add calls to mixture Helmholtz (HMX) model
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-16-96  MM, change name from DPRHO to DPDD
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DPDD
c
      include 'commons.for'
      dimension x(ncmax)
c
      dpdrho=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        dpdrho=DPDBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi02=PHIK(icomp,0,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi02=PHIX(0,2,tau,del,x)
        end if
        dpdrho=R*t*(1.0d0+2.0d0*phi01+phi02)
      end if
c
      RETURN
      end                                               !subroutine DPDD
c
c ======================================================================
c
      subroutine DPDDK (icomp,t,rho,dPdrho)
c
c  compute partial derivative of pressure w.r.t. density at constant
c  temperature as a function of temperature and density for a specified
c  component
c
c  analogous to DPDD, except for component icomp, this is used by transport
c  routines to calculate dP/dD for the reference fluid (component zero)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output:
c   dPdrho--dP/drho [kPa-L/mol]
c
c  written by M. McLinden, NIST Physical & Chem Properties Div, Boulder, CO
c  06-16-97  MM, original version; based on DPDD
c  09-29-97  MM, if component uses MBWR, call DPDBWR
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DPDDK
c
      include 'commons.for'
c
      dpdrho=0.d0
      if (t.le.0.d0) return
      if (hmxeos(icomp).eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        dpdrho=DPDBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        tau=tz(icomp)/t
        del=rho/rhoz(icomp)
        phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
        phi02=PHIK(icomp,0,2,tau,del)
        dpdrho=R*t*(1.0d0+2.0d0*phi01+phi02)
      end if
c
      RETURN
      end                                              !subroutine DPDDK
c
c ======================================================================
c
      subroutine DPDD2 (t,rho,x,d2PdD2)
c
c  compute second partial derivative of pressure w.r.t. density at const
c  temperature as a function of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c   d2pdD2--d^2P/drho^2 [kPa-L^2/mol^2]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  06-03-97 EWL, original version
c  10-01-97  MM, add compiler switch to allow access by DLL
c  02-11-99 EWL, skip calculation of d2PdD2 if rho=0
c  09-05-02 EWL, add ideal gas d2PdD2
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DPDD2
c
      include 'commons.for'
      dimension x(ncmax)
c
      d2PdD2=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        d2PdD2=D2PBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi02=PHIK(icomp,0,2,tau,del)
          phi03=PHIK(icomp,0,3,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi02=PHIX(0,2,tau,del,x)
          phi03=PHIX(0,3,tau,del,x)
        end if
        if (rho.gt.1.0d-8) then
          d2PdD2=R*t/rho*(2.0d0*phi01+4.0d0*phi02+phi03)
        else
          call VIRB (t,x,b)
          d2PdD2=2.d0*b*R*t
        end if
      end if
c
      RETURN
      end                                              !subroutine DPDD2
c
c ======================================================================
c
      subroutine DPDT (t,rho,x,dpt)
c
c  compute partial derivative of pressure w.r.t. temperature at constant
c  density as a function of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c      dpt--dP/dT [kPa/K]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  10-16-96  MM, original version, based on DPDD
c  10-28-96  MM, insert missing rho into form using PHI's
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DPDT
c
      include 'commons.for'
      dimension x(ncmax)
c
      dpt=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        dpt=DPTBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi11=PHIK(icomp,1,1,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi11=PHIX(1,1,tau,del,x)
        end if
        dpt=R*rho*(1.0d0+phi01-phi11)
      end if
c
      RETURN
      end                                               !subroutine DPDT
c
c ======================================================================
c
      subroutine DPDTK (icomp,t,rho,dpt)
c
c  compute partial derivative of pressure w.r.t. temperature at constant
c  density as a function of temperature and density for a specified component
c
c  analogous to DPDT, except for component icomp, this is used by transport
c  routines to calculate dP/dT
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output:
c      dpt--dP/dT [kPa/K]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  07-07-98 EWL, original version, based on DPDT
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DPDTK
c
      include 'commons.for'
c
      dpt=0.d0
      if (t.le.0.d0) return
      if (hmxeos(icomp).eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        dpt=DPTBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        tau=tz(icomp)/t
        del=rho/rhoz(icomp)
        phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
        phi11=PHIK(icomp,1,1,tau,del)
        dpt=R*rho*(1.0d0+phi01-phi11)
      end if
c
      RETURN
      end                                              !subroutine DPDTK
c
c ======================================================================
c
      subroutine DDDP (t,rho,x,drhodp)
c
c  compute partial derivative of density w.r.t. pressure at constant
c  temperature as a function of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c   drhodp--drho/dP [mol/(L-kPa)]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  08-29-97  MM, original version, based on DPDD (just the inverse)
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DDDP
c
      include 'commons.for'
      dimension x(ncmax)
c
      drhodp=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        drhodp=1.0d0/DPDBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi02=PHIK(icomp,0,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi02=PHIX(0,2,tau,del,x)
        end if
        drhodp=1.0d0/(R*t*(1.0d0+2.0d0*phi01+phi02))
      end if
c
      RETURN
      end                                               !subroutine DDDP
c
c ======================================================================
c
      subroutine DDDT (t,rho,x,drhodt)
c
c  compute partial derivative of density w.r.t. temperature at constant
c  pressure as a function of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  output:
c   drhodt--drho/dT [mol/(L-K)]
c
c   d(rho)/d(T) = -d(rho)/dP x dP/dT = -dP/dT / (dP/d(rho))
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  08-29-97  MM, original version, based on DPDD and DPDT
c  10-01-97  MM, add compiler switch to allow access by DLL
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DDDT
c
      include 'commons.for'
      dimension x(ncmax)
c
      drhodt=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        drhodt=-DPTBWR(icomp,t,rho)/DPDBWR(icomp,t,rho)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi11=PHIK(icomp,1,1,tau,del)
          phi02=PHIK(icomp,0,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi11=PHIX(1,1,tau,del,x)
          phi02=PHIX(0,2,tau,del,x)
        end if
        drhodt=-rho*(1.0d0+phi01-phi11)/(t*(1.0d0+2.0d0*phi01+phi02))
      end if
c
      RETURN
      end                                               !subroutine DDDT
c
c ======================================================================
c
      subroutine DERVPVT (t,rho,x,
     &                    dPdD,dPdT,d2PdD2,d2PdT2,d2PdTD,
     &                    dDdP,dDdT,d2DdP2,d2DdT2,d2DdPT,
     &                    dTdP,dTdD,d2TdP2,d2TdD2,d2TdPD)
c
c  compute derivatives of temperature, pressure, and density
c  using core functions for Helmholtz free energy equations only
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c     dPdD--derivative dP/drho [kPa-L/mol]
c     dPdT--derivative dP/dT [kPa/K]
c     dDdP--derivative drho/dP [mol/(L-kPa)]
c     dDdT--derivative drho/dT [mol/(L-K)]
c     dTdP--derivative dT/dP [K/kPa]
c     dTdD--derivative dT/drho [(L-K)/mol]
c   d2PdD2--derivative d^2P/drho^2 [kPa-L^2/mol^2]
c   d2PdT2--derivative d2P/dT2 [kPa/K^2]
c   d2PdTD--derivative d2P/dTd(rho) [J/mol-K]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  08-21-12 EWL, original version; based on THERM2
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DERVPVT
c
      include 'commons.for'
      dimension x(ncmax)
c
      if (t.le.0.d0) return
      call RMIX (x)
      RT=R*t
c
c  call general PHIX routines
      call REDX (x,t0,rho0)
      tau=t0/t
      del=rho/rho0
c     phi00=PHIX(0,0,tau,del,x)   !real-gas terms
      phi01=PHIX(0,1,tau,del,x)
c     phi10=PHIX(1,0,tau,del,x)
      phi11=PHIX(1,1,tau,del,x)
      phi12=PHIX(1,2,tau,del,x)
      phi21=PHIX(2,1,tau,del,x)
c     phi20=PHIX(2,0,tau,del,x)
      phi02=PHIX(0,2,tau,del,x)
      phi03=PHIX(0,3,tau,del,x)
c
      u    = 1.d0+phi01-phi11
      dut  = phi21
      dud  = phi01+phi02-phi11-phi12
c
      v    = 1.d0+2.d0*phi01+phi02
      dvt  =-2.d0*phi11-phi12
      dvd  = 2.d0*phi01+4.d0*phi02+phi03
c
      dPdD    = RT*v
      dPdT    = R*rho*u
      dDdT    =-rho*u/(t*v)
      dDdP    = 1.d0/dPdD
      d2PdT2  = R*rho*dut/t
      d2PdTD  = R*(v+dvt)
      d2DdPT  =-(v+dvt-u/v*dvd)/t/RT/v**2
      d2DdT2  = rho*u/v**2/t**2*(u+v-u/v*dvd+dvt+dud-v/u*dut)
c
      if (rho.gt.1.0d-8) then
        dTdP    = 1.d0/dPdT
        dTdD    = 1.d0/dDdT
        d2PdD2  = RT/rho*dvd
        d2DdP2  =-dvd/v**3/rho/RT**2
        d2TdP2  =-dut/(R*rho)**2/t/u**3
        d2TdD2  = t*v/rho**2/u**2*(u+v-u/v*dvd+dvt+dud-v/u*dut)
        d2TdPD  =-(u+dud-v/u*dut)/r/rho**2/u**2
      else
        call VIRB (t,x,b)
        d2PdD2 = 2.d0*b*R*t
        d2DdP2 =-2.d0*b/RT**2
        dTdP   = xinf
        dTdD   = xinf
        d2TdP2 = xinf
        d2TdD2 = xinf
        d2TdPD = xinf
      end if
c
      RETURN
      end                                            !subroutine DERVPVT
c
c ======================================================================
c
      subroutine DHD1(t,rho,x,dhdt_d,dhdt_p,dhdd_t,dhdd_p,dhdp_t,dhdp_d)
c
c  compute partial derivatives of enthalpy w.r.t. t, p, or rho at constant
c  t, p, or rho as a function of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c   dhdt_d--dH/dT at constant density [J/(mol-K)]
c   dhdt_p--dH/dT at constant pressure [J/(mol-K)]
c   dhdd_t--dH/drho at constant temperature [(J/mol)/(mol/L)]
c   dhdd_p--dH/drho at constant pressure [(J/mol)/(mol/L)]
c   dhdp_t--dH/dP at constant temperature [J/(mol-kPa)]
c   dhdp_d--dH/dP at constant density [J/(mol-kPa)]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  10-02-00 EWL, original version
c  05-30-06 EWL, change subroutine name from DHDT to DHD1, and add other
c                derivates of h with respect to t, p, or rho
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DHD1
c
      include 'commons.for'
      dimension x(ncmax)
c
      dhdt_d=0.d0
      dhdt_p=0.d0
      dhdd_t=0.d0
      dhdd_p=0.d0
      dhdp_t=0.d0
      dhdp_d=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rhos=rho
      if (rho.lt.1.0d-10) rhos=1.0d-10
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        tau=tz(icomp)/t
        del=rho/rhoz(icomp)
        phi01=PHIBWR(icomp,0,1,tau,del)  !real-gas terms
        phi11=PHIBWR(icomp,1,1,tau,del)
        phi20=PHIBWR(icomp,2,0,tau,del)
        phi02=PHIBWR(icomp,0,2,tau,del)
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
          phi11=PHIK(icomp,1,1,tau,del)
          phi20=PHIK(icomp,2,0,tau,del)
          phi02=PHIK(icomp,0,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)  !real-gas terms
          phi11=PHIX(1,1,tau,del,x)
          phi20=PHIX(2,0,tau,del,x)
          phi02=PHIX(0,2,tau,del,x)
        end if
      end if
      phig20=PHI0(2,0,t,rho,x)
      phig11=PHI0(1,1,t,rho,x)
      call THERM2 (t,rhos,x,p,e,h,s,cv,cp,w,Z,hjt,A,G,xkappa,beta,
     &      dPdrho,d2PdD2,dPT,drhodT,drhodP,d2PT2,d2PdTD,spare3,spare4)
      dhdt_p=cp
      dhdt_d=R*(-phig20-phi20+phi01-phi11+1.d0)
      if (rho.gt.1.0d-8) then
        dhdp_t=1.d0/rho+t*drhodT/rho**2
        dhdd_t=R*T/rho*(phig11+phi11+phi01+phi02)
        dhdp_d=dhdp_t+dhdt_p/dPT
        dhdd_p=dhdd_t+dhdt_d/drhodT
      else
        call VIRB (t,x,b)
        call DBDT (t,x,dbt)
        dhdp_t=1.d0/rhos+t*drhodT/rhos**2
        dhdd_t=-r*t**2*dbt+r*t*b
        dhdp_d=xinf
        dhdd_p=xinf
      endif
c
      RETURN
      end                                               !subroutine DHD1
c
c ======================================================================
c
      subroutine FGCTY2 (t,rho,x,f,ierr,herr)
c
c  compute fugacity for each of the nc components of a mixture by
c  analytical differentiation of the dimensionless residual Helmholtz energy
c
c  based on derivations in the GERG-2004 document for natural gas
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        f--array (1..nc) of fugacities [kPa]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  02-22-10 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: FGCTY2
c
      include 'commons.for'
      dimension x(ncmax),f(ncmax)
      dimension dadn(ncmax),dnadn(ncmax)
      character*255 herr
c
      ierr=0
      herr=' '
      call ISPURE (x,icomp)
      call RMIX (x)
      do i=1,nc
        f(i)=0.0d0
      enddo
      if (t.le.0.d0 .or. rho.lt.1.0d-40) RETURN
      if (icomp.ne.0) goto 10  !Call old FGCTY routine for pure fluids
c
      RTrho=R*t*rho
      call PHIDERV (1,t,rho,x,dadn,dnadn,ierr,herr)
      if (ierr.ne.0) goto 10
      do i=1,nc
        f(i)=xerr
        if (ABS(dnadn(i)).lt.100.0d0) f(i)=x(i)*RTrho*exp(dnadn(i))
      enddo
      RETURN
c
 10   continue
      call FGCTY (t,rho,x,f)
      ierr=0
      herr=' '
      RETURN
      end                                             !subroutine FGCTY2
c
c ======================================================================
c
      subroutine PHIDERV (iderv,t,rho,x,dadn,dnadn,ierr,herr)
c
c  calculate various derivatives needed for VLE determination
c  based on derivations in the GERG-2004 document for natural gas
c
c  inputs:
c        iderv--set to 1 for first order derivatives only (dadn and dnadn)
c               set to 2 for full calculations
c        t--temperature (K)
c      rho--density (mol/L)
c        x--composition [array of mol frac]
c
c  outputs: (where n is mole number)
c           (the listed equation numbers are those in the GERG manuscript)
c    dnadn--partial(n*alphar)/partial(ni)                   Eq. 7.15
c     dadn--n*partial(alphar)/partial(ni)                   Eq. 7.16
c    daddn--del*n*par.(par.(alphar)/par.(del))/par.(ni)     Eq. 7.17
c     dvdn--n*[partial(Vred)/partial(ni)]/Vred              Eq. 7.18
c           (=-n*[partial(Dred)/partial(ni)]/Dred)
c     dtdn--n*[partial(Tred)/partial(ni)]/Tred              Eq. 7.19
c    dadxi--partial(alphar)/partial(xi)                     Eq. 7.21g
c   sdadxi--sum[xi*partial(alphar)/partial(xi)]             Eq. 7.21g
c   dadxij--partial^2(alphar)/partial(xi)/partial(xj)       Eq. 7.21i
c    daddx--del*partial^2(alphar)/partial(xi)/partial(del)  Eq. 7.21j
c    dadtx--tau*partial^2(alphar)/partial(xi)/partial(tau)  Eq. 7.21k
c   dphidT--par.(ln(phi))/par.(T) (constant p,n,x)          Eq. 7.29
c   dphidp--par.(ln(phi))/par.(p) (constant T,n,x)          Eq. 7.30
c  dphidnj--n*par.[ln(phi(i))]/par(nj) (constant T,p)       Eq. 7.31
c  dlnfinidT--par.[ln(fi/ni)]/par(T)                        Eq. 7.36
c  dlnfinidV--n*par.[ln(fi/ni)]/par(V)                      Eq. 7.37
c   d2adbn--    par.[par.(n*alphar)/par.(ni)]/par.(T)       Eq. 7.44
c   d2adnn--n*partial^2(n*alphar)/partial(ni)/partial(nj)   Eq. 7.46 and 7.47 (similar to 7.38)
c   d2addn--del*par.[n*par.(alphar)/par.(ni)]/par.(del)     Eq. 7.50
c   d2adtn--tau*par.[n*par.(alphar)/par.(ni)]/par.(tau)     Eq. 7.51
c   d2adxn--    par.[n*par.(alphar)/par.(ni)]/par.(xj)      Eq. 7.52
c   ddrdxn--par.[n*par.(Dred)/par.(ni)]/par.(xj)            Eq. 7.55
c   dtrdxn--par.[n*par.(Tred)/par.(ni)]/par.(xj)            Eq. 7.56
c     dpdn--n*partial(p)/partial(ni)                        Eq. 7.63 constant T,V,nj
c    dpdxi--partial(p)/partial(xi)                          constant T,V
c d2adxnTV--par.[n*par.(alphar)/par.(ni)]/par.(xj)          constant T,V
c  dadxiTV--partial(alphar)/partial(xi)                     constant T,V
c daddxiTV--del*partial^2(alphar)/partial(xi)/partial(del)  constant T,V
c  dphidxj--par.(ln(phi))/par.(xj)                          constant T,p,x
c    xlnfi--Log of modified fugacity
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  02-22-10 EWL, original version
c  12-09-12 EWL, fix equations for dlnfinidT and dlnfinidV
c
      include 'commons.for'
      dimension x(ncmax)
      character*255 herr
      dimension dadn(ncmax),dnadn(ncmax),
     &          aok(ncmax),aok01(ncmax),aok10(ncmax),
     &          sdadxi(ncmax),sdaddxi(ncmax),sdadtxi(ncmax),
     &          tr01(ncmax),dr01(ncmax),sdadxx(ncmax),
     &          phimxk(ncmax,ncmax),
     &          phimxk01(ncmax,ncmax),phimxk10(ncmax,ncmax),
     &          dr11(ncmax,ncmax),tr11(ncmax,ncmax)
c
      ierr=0
      herr=' '
      call RMIX (x)
      call REDX (x,t0,rho0)
      tau=t0/t
      del=rho/rho0
      Rt=R*t

      do i=1,nc
        dadn(i)=0.d0
        dnadn(i)=0.d0
        daddn(i)=0.d0
        d2addn(i)=0.d0
        d2adtn(i)=0.d0
        daddx(i)=0.d0
        dadtx(i)=0.d0
        dtdn(i)=0.d0
        dvdn(i)=0.d0
        dadxi(i)=0.d0
        aok(i)=0.d0
        aok01(i)=0.d0
        aok10(i)=0.d0
        sdadxi(i)=0.d0
        sdaddxi(i)=0.d0
        sdadtxi(i)=0.d0
        tr01(i)=0.d0
        dr01(i)=0.d0
        dpdn(i)=0.d0
        sdadxx(i)=0.d0
        do j=1,nc
          d2adnn(i,j)=0.d0
          ddrdxn(i,j)=0.d0
          dtrdxn(i,j)=0.d0
          d2adxn(i,j)=0.d0
          dadxij(i,j)=0.d0     !if i=j, derivative=0
        enddo
      enddo

      if (heos.eq.'PR') then
c       call FGCTY (t,rho,x,dnadn)
        dummy=PHIPRX (-1,0,tau,del,x)
        dnadn(1:nc)=daddn(1:nc)   !dnadn is temporarily stored in daddn
        daddn(1:nc)=0.d0
        ierr=0
        if (dummy.ne.-1.d0) ierr=1
        RETURN
      endif
c
c  get Helmholtz energy of pure fluid
      ar  =PHIX(0,0,tau,del,x)
      ar01=PHIX(0,1,tau,del,x)
      ar10=PHIX(1,0,tau,del,x)
      if (ar01.le.-1.d0) then
        ierr=1
        return
      endif

      do i=1,nc
        phimxk(i,i)=0.d0
        if (x(i).gt.0.d0) then
          call RDXHMX (1,i,0,x,tr01(i),dr01(i),ierr,herr)
          if (ierr.ne.0) RETURN
          aok(i)=PHIK(i,0,0,tau,del)
          if (i.ne.nc) then
            do j=i+1,nc
              if (x(j).gt.0.d0) then
                phimxk(i,j)=PHIMIX(i,j,0,0,tau,del,x)
                phimxk(j,i)=phimxk(i,j)
              endif
            enddo
          endif
        endif
      enddo
c
c  calculate first order derivatives only
      if (iderv.eq.1) then
        do i=1,nc
          if (x(i).gt.0.d0) then
          do j=1,nc
            if (x(j).gt.0.d0) then
c  get derivatives of reducing parameters
              dtdn(i)=dtdn(i)-x(j)*tr01(j)
              dvdn(i)=dvdn(i)-x(j)*dr01(j)
              sdadxi(i)=sdadxi(i)+x(j)*aok(j)
              if (i.eq.j) then
                dtdn(i)=dtdn(i)+tr01(j)
                dvdn(i)=dvdn(i)+dr01(j)
                dadxi(i)=dadxi(i)+aok(j)
              else
c  add excess Helmholtz energy of i-j interaction
                dadxi(i)=dadxi(i)+phimxk(i,j)/x(i)
              endif
              if (j.ne.nc) then
                do k=j+1,nc
c  subtract excess Helmholtz energy of i-j interaction twice (to include j-i)
                  if (x(k).ne.0.d0) sdadxi(i)=sdadxi(i)+phimxk(k,j)*2.d0
                enddo
              endif
            endif
          enddo
          endif
        enddo
c
c  *** do this only while testing numerical derivatives!!! ***
c       call RDXHMX (-1,0,0,x,t0,rho0,ierr,herr)

        do i=1,nc
          if (x(i).gt.0) then
            dtdn(i)=dtdn(i)/t0
            dvdn(i)=dvdn(i)*rho0
            dadn(i)=ar01*(1.d0+dvdn(i))+ar10*dtdn(i)+dadxi(i)-sdadxi(i)   !Eq. 7.16 in GERG
            dnadn(i)=ar+dadn(i)                                           !Eq. 7.15
          endif
        enddo
c
c  calculate first and second order derivatives
      else
        ar02=PHIX(0,2,tau,del,x)
        ar20=PHIX(2,0,tau,del,x)
        ar11=PHIX(1,1,tau,del,x)
        call PRESS (t,rho,x,p)
        call DPDT (t,rho,x,dPT)
        call DPDD (t,rho,x,dpdrho)
        call DDDT (t,rho,x,drhodt)  !Only needed for 7.66
        dPdV=-dpdrho*rho**2
        if (p.eq.0.d0) p=1.d-20
        if (dPdV.eq.0.d0) dPdV=1.d-20
        dVdT=0.d0
        if (rho.ne.0.d0) dVdT=-drhodt/rho**2
c       if (dpdrho.lt.0 .or. dPT.lt.0) then
c         ierr=1
c         return
c       endif

        do i=1,nc
          phimxk01(i,i)=0.d0
          phimxk10(i,i)=0.d0
          if (x(i).gt.0.d0) then
            aok01(i)=PHIK(i,0,1,tau,del)
            aok10(i)=PHIK(i,1,0,tau,del)
            if (i.ne.nc) then
              do j=i+1,nc
                if (x(j).gt.0.d0) then
                  phimxk01(i,j)=PHIMIX(i,j,0,1,tau,del,x)
                  phimxk10(i,j)=PHIMIX(i,j,1,0,tau,del,x)
                  phimxk01(j,i)=phimxk01(i,j)
                  phimxk10(j,i)=phimxk10(i,j)
                endif
              enddo
            endif
            do j=1,nc
              if (x(j).gt.0.d0) then
                ij=11
                if (i.eq.j) ij=2
                call RDXHMX (ij,i,j,x,tr11(i,j),dr11(i,j),ierr,herr)
                dr11(i,j)=2*rho0**3*dr01(j)*dr01(i)-rho0**2*dr11(i,j)
                if (ierr.ne.0) RETURN
              endif
            enddo
          endif
        enddo

        do j=1,nc
          if (x(j).gt.0.d0) then
            do i=1,nc
              if (x(i).gt.0.d0) then
                ddrdxn(i,j)= dr01(j)*rho0**2                              !Eq. 7.55
                dtrdxn(i,j)=-tr01(j)                                      !Eq. 7.56
                do k=1,nc
                  if (x(k).gt.0.d0) then
                    ddrdxn(i,j)=ddrdxn(i,j)-x(k)*dr11(k,j)
                    dtrdxn(i,j)=dtrdxn(i,j)-x(k)*tr11(k,j)
                    if (i.eq.k) then
                      ddrdxn(i,j)=ddrdxn(i,j)+dr11(k,j)
                      dtrdxn(i,j)=dtrdxn(i,j)+tr11(k,j)
                    endif
                  endif
                enddo
              endif
            enddo
          endif
        enddo

        do i=1,nc
        if (x(i).gt.0.d0) then
        do j=1,nc
          if (x(j).gt.0.d0) then
            dtdn(i)=dtdn(i)-x(j)*tr01(j)
            dvdn(i)=dvdn(i)-x(j)*dr01(j)
            sdadxi(i)=sdadxi(i)+x(j)*aok(j)
            sdaddxi(i)=sdaddxi(i)+x(j)*aok01(j)
            sdadtxi(i)=sdadtxi(i)+x(j)*aok10(j)
            if (i.eq.j) then
              daddx(i)=daddx(i)+aok01(j)
              dadtx(i)=dadtx(i)+aok10(j)
              dtdn(i)=dtdn(i)+tr01(j)
              dvdn(i)=dvdn(i)+dr01(j)
              dadxi(i)=dadxi(i)+aok(j)
            endif
c  add excess Helmholtz energy of i-j interaction
            dadxi(i)=dadxi(i)+phimxk(i,j)/x(i)
            daddx(i)=daddx(i)+phimxk01(i,j)/x(i)
            dadtx(i)=dadtx(i)+phimxk10(i,j)/x(i)
            if (j.ne.nc) then
              do k=j+1,nc
c  subtract excess Helmholtz energy of i-j interaction twice (to include j-i)
              if (x(k).gt.0.d0) then
                sdadxi(i) =sdadxi(i) +phimxk(k,j)*2.d0
                sdaddxi(i)=sdaddxi(i)+phimxk01(k,j)*2.d0
                sdadtxi(i)=sdadtxi(i)+phimxk10(k,j)*2.d0
              endif
              enddo
            endif
          endif
        enddo
        dtdn(i)=dtdn(i)/t0
        dvdn(i)=dvdn(i)*rho0
        endif
        enddo
c
        do i=1,nc
          if (x(i).gt.0.d0) then
            dvdn1=1.d0+dvdn(i)
            dadn(i)=ar01*dvdn1+ar10*dtdn(i)+dadxi(i)-sdadxi(i)            !Eq. 7.16 in GERG
            daddn(i)=ar02*dvdn1+ar11*dtdn(i)+daddx(i)-sdaddxi(i)          !Eq. 7.17
            dnadn(i)=ar+dadn(i)                                           !Eq. 7.15
            d2addn(i)=(ar01+ar02)*dvdn1+ar11*dtdn(i)+daddx(i)-sdaddxi(i)  !Eq. 7.50
            d2adtn(i)=ar11*dvdn1+(ar10+ar20)*dtdn(i)+dadtx(i)-sdadtxi(i)  !Eq. 7.51
            d2adbn(i)=-1.d0/T*(ar10+d2adtn(i))                            !Eq. 7.44
            xlnfi(i)=dlog(x(i)/(1.d0+ar01))+dnadn(i)                      !Log of modified fugacity
            dlnfinidT(i)=(1.d0-ar10-d2adtn(i))/T                          !Eq. 7.36 with use of 7.44
            dlnfinidV(i)=-rho*(1.d0+ar01+d2addn(i))                       !Eq. 7.37 with use of 7.45 (this is really n*Eq7.37)
          endif
        enddo

        do i=1,nc
          if (x(i).gt.0.d0) then
            daddnk=ar02*(1.d0+dvdn(i))+ar11*dtdn(i)+daddx(i)-sdaddxi(i)   !Eq. 7.17
            dpdn(i)=rho*Rt*(1.d0+ar01*(2.d0+dvdn(i))+daddnk)              !Eq. 7.63 in GERG
            dadxiTV(i)=ar01*rho0*dr01(i)+ar10/t0*tr01(i)+dadxi(i)         !par(ar)/par(xi) at constant T and V
            daddxiTV(i)=ar02*rho0*dr01(i)+ar11/t0*tr01(i)+daddx(i)        !par^2(ar)/par(xi)/par(del) at constant T and V
            dpdxi(i)=rho*Rt*(dr01(i)*rho0*ar01+daddxiTV(i))               !d(P)/d(xi) at constant T and V
            vrt=-dpdn(i)/dpdv/rt                                          !Eq. 7.32 divided by RT
            dphidT(i)=d2adbn(i)+1.d0/t-vrt*dpt                            !Eq. 7.29  d(ln(phi))/dT at constant p and x (or n)
            dphidp(i)=vrt-1.d0/p                                          !Eq. 7.30  d(ln(phi))/dP at constant T and x (or n)
            dmurdt(i)=r*dnadn(i)+rt*d2adbn(i)-dpdn(i)*dvdt                 !Eq. 7.66 without the ideal gas part
          endif
        enddo
        do j=1,nc
          if (x(j).gt.0.d0) then
            do i=1,nc
              if (x(i).gt.0.d0) then
                dadxij(i,j)=phimxk(i,j)/x(j)/x(i)
                sdadxx(j)=sdadxx(j)+x(i)*dadxij(i,j)
              endif
            enddo
          endif
        enddo
        do i=1,nc
          if (x(i).gt.0.d0) then
            do j=1,nc
              if (x(j).gt.0.d0) then
              d2adxn(i,j)=daddx(j)*(1.d0+dvdn(i))                         !Eq. 7.52
     &                 -ar01/rho0*(ddrdxn(i,j)-rho0**2*dr01(j)*dvdn(i))
     &                 +dadtx(j)*dtdn(i)
     &                 +ar10/t0*(dtrdxn(i,j)-tr01(j)*dtdn(i))
     &                 +dadxij(i,j)-dadxi(j)-sdadxx(j)
              d2adxnTV(j,i)=d2addn(i)*rho0*dr01(j)                        !d^2(n*ar)/dxj/dni at constant T and V
     &                 +d2adtn(i)/t0*tr01(j)+d2adxn(i,j)+dadxiTV(j)
              endif
            enddo
          endif
        enddo
        do i=1,nc
          if (x(i).gt.0.d0) then
          do j=1,nc
            if (x(j).gt.0.d0) then
              sd2adxn=0.d0
              do k=1,nc
                sd2adxn=sd2adxn+x(k)*d2adxn(i,k)
              enddo
              !Eq. 7.38 times n is the same as Eq. 7.46 (d2adnn)
              d2adnn(i,j)=dadn(j)+d2addn(i)*(1.d0+dvdn(j))                !Eq. 7.46
     &                   +d2adtn(i)*dtdn(j)+d2adxn(i,j)-sd2adxn
              dphidnj(i,j)=d2adnn(i,j)+1.d0+dpdn(j)*dpdn(i)/dpdv/Rt       !Eq. 7.31
              dlnphiidxj(i,j)=d2adxnTV(i,j)+dPdn(j)/Rt*dpdxi(i)/dpdV
              dphidxj(i,j)=dlnphiidxj(i,j)
              if (i.eq.j) dphidxj(i,j)=dphidxj(i,j)+1.d0/x(i)
            endif
          enddo
          endif
        enddo
      endif

      RETURN
      end                                            !subroutine PHIDERV
c
c ======================================================================
c
      subroutine FGCTY (t,rho,x,f)
c
c  old routine to compute fugacity for each of the nc components of a mixture
c  by numerical differentiation (using central differences) of the
c  dimensionless residual Helmholtz energy
c
c  based on derivations in E.W. Lemmon, MS Thesis, University of Idaho
c  (1991); section 3.2
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        f--array (1..nc) of fugacities [kPa]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  12-15-95  MM, original version
c  12-18-95  MM, add pure component fugacity as a special case
c  01-08-96  MM, bug on call to PHIFEQ (wrong arguments)
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c                replace calls to PHIHMX, PHIFEQ with general PHIX, PHIK
c  03-19-19  MM, add dipole moment to /CCON/
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-16-97  MM, add check for rho = 0; overflow on exponent (set to xerr)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-22-98 EWL, recalculate R for mixtures based on values for pure fluids
c  05-08-06 EWL, modify how delp and deln are calculated for x>0.9999
c  01-25-07 EWL, change default f(i) from 0 to 1.  Skip calculation if x(i)=0
c  02-26-09 EWL, set f(icomp) equal to a very large number instead of xerr when the variable arg is huge
c  11-20-09 BFT, change deln to delmol in check for x(i).gt.1.0d0-delmol
c
cx DEC$ ATTRIBUTES DLLEXPORT :: FGCTY
c
      include 'commons.for'
      dimension x(ncmax),f(ncmax)
      dimension xplus(ncmax),xminus(ncmax)
c
      delmol=1.0d-4
      call ISPURE (x,icomp)
c
c  fill output fugacity array with zeros (final value for undefined
c  components and insurance against problems for others)
      do i=1,nc
        f(i)=0.0d0
      enddo
      if (t.le.0.d0) return
c
      call RMIX (x)
c  check for zero input density
      if (rho.lt.1.0d-40) RETURN
c
      RTrho=R*t*rho
      if (icomp.ne.0) then
c  pure component
        if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--use BWR-specific routines
          Ar=ABWR(icomp,t,rho)
          p=PBWR(icomp,t,rho)
          f(icomp)=RTrho*exp(Ar/(R*t)+p/RTrho-1.0d0)
        else
c  for other models, use general PHIK routines
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi00=PHIK(icomp,0,0,tau,del)
          phi01=PHIK(icomp,0,1,tau,del)
c  check for potential under- or over-flow (can happen in 2-phase, but
c  the fugacity is meaningless there anyway)
          arg=phi00+phi01
          if (ABS(arg).lt.500.0d0) then
            f(icomp)=RTrho*exp(arg)
          else
            f(icomp)=1.d100
          end if
        end if
      else
c
c  mixture
        do i=1,nc
c  compute positive and negative increments to number of moles
c  general case:  deln < x(i) < 1 - deln
          delpp=delmol
          deln=-delmol
          if (x(i).gt.0.d0) then
          if (x(i).lt.delmol) then
c  special case--composition of component i is nearly zero
            deln=-x(i)/2.d0
            delpp=-deln
          else if (x(i).gt.1.0d0-delmol) then
c  special case--composition of component i is nearly one (pure fluid)
            delpp=(1.0d0-x(i))/2.d0
            deln=-delpp
          end if
          delp1=1.0d0/(1.0d0+delpp)
          deln1=1.0d0/(1.0d0+deln)
c  since total number of moles is now 1 + (delp or deln), all of the
c  compositions have changed
          do j=1,nc
            xplus(j)=x(j)*delp1
            xminus(j)=x(j)*deln1
          enddo
          xplus(i)=(x(i)+delpp)*delp1
          xminus(i)=(x(i)+deln)*deln1
c  derivative is at constant volume, so must adjust density
          Dplus=rho*(1.0d0+delpp)
          Dminus=rho*(1.0d0+deln)
c  compute residual Helmholtz at 'plus' and 'minus' density and composition
c  could call subroutine GIBBS here, but more efficient to directly call
c  the core routines (via PHIX)
          call REDX (xplus,t0,rho0)
          tau=t0/t
          del=Dplus/rho0
          Aplus=PHIX(0,0,tau,del,xplus)          !real-gas terms
          call REDX (xminus,t0,rho0)
          tau=t0/t
          del=Dminus/rho0
          Aminus=PHIX(0,0,tau,del,xminus)        !real-gas terms
          dnadn0=((1.0d0+delpp)*Aplus-(1.0d0+deln)*Aminus)/(delpp-deln)
c         write (*,*) ' FGCTY--delp,deln:  ',delp,deln
c         write (*,*) ' FGCTY--i,A+, A-, dAdN: ',i,Aplus,Aminus,dnadn0
c  check for potential under- or over-flow (can happen in 2-phase, but
c  the fugacity is meaningless there anyway)
          f(i)=xerr
          if (ABS(dnadn0).lt.100.0d0) f(i)=x(i)*RTrho*exp(dnadn0) !A is dimensionless
          end if
        enddo
      end if
c
      RETURN
      end                                              !subroutine FGCTY
c
c ======================================================================
c
      subroutine CHEMPOT (t,rho,x,u,ierr,herr)
c
c  compute the chemical potentials for each of the nc components of a
c  mixture
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        u--array (1..nc) of the chemical potentials [J/mol]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-18-10 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CHEMPOT
c
      include 'commons.for'
      dimension x(ncmax),u(ncmax)
      dimension dadn(ncmax),dnadn(ncmax)
      character*255 herr
c
      ierr=0
      herr=' '
      do i=1,nc
        u(i)=0.d0
      enddo
      if (t.le.0.d0) return
      if (rho.lt.1.0d-40) RETURN
c
      call RMIX (x)
      RT=R*t
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  pure component
        call AG (t,rho,x,a,g)
        u(icomp)=g
      else
        call PHIDERV (1,t,rho,x,dadn,dnadn,ierr,herr)
        do i=1,nc
          da0dn=PHI0K(i,0,0,t,rho)     !ideal-gas terms  (Eq. 7.14 in GERG)
          da0dn=da0dn-href(i)/RT+sref(i)/R+1.D0+log(x(i))
          u(i)=(dnadn(i)+da0dn)*RT
        enddo
      end if
c
      RETURN
      end                                            !subroutine CHEMPOT
c
c ======================================================================
c
      subroutine FUGCOF (t,rho,x,phi,ierr,herr)
c
c  compute the fugacity coefficient for each of the nc components of a
c  mixture
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c      phi--array (1..nc) of the fugacity coefficients [-]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-21-10 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: FUGCOF
c
      include 'commons.for'
      dimension x(ncmax),phi(ncmax),f(ncmax)
      character*255 herr
c
      ierr=0
      herr=' '
      do i=1,nc
        phi(i)=0.d0
      enddo
      if (t.le.0.d0) return
      if (rho.lt.1.0d-40) RETURN
c
      call ISPURE (x,icomp)
      call FGCTY2 (t,rho,x,f,ierr,herr)
      call PRESS (t,rho,x,p)
      if (p.gt.0.d0) then
        if (icomp.ne.0) then
c  pure component
          phi(icomp)=f(icomp)/p
        else
          do i=1,nc
            if (x(i).gt.0.d0) phi(i)=f(i)/p/x(i)
          enddo
        endif
      endif
c
      RETURN
      end                                             !subroutine FUGCOF
c
c ======================================================================
c
      subroutine ACTVY (t,rho,x,actv,gamma,ierr,herr)
c
c  compute the activity and activity coefficient for each of the nc
c  components of a mixture
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c     actv--array (1..nc) of the activities
c    gamma--array (1..nc) of the activity coefficients
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-21-10 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ACTVY
c
      include 'commons.for'
      dimension x(ncmax),actv(ncmax),gamma(ncmax),f(ncmax),fp(ncmax)
      dimension xl(ncmax),xv(ncmax)
      character*255 herr
c
      ierr=0
      herr=' '
      do i=1,nc
        actv(i)=0.d0      !Pure fluid values
        gamma(i)=0.d0
      enddo
      if (t.le.0.d0) return
      if (rho.lt.1.0d-40) RETURN
c
      call ISPURE (x,icomp)
      if (icomp.eq.0) then
        call FGCTY2 (t,rho,x,f,ierr,herr)
        call PRESS (t,rho,x,p)
        ic2=icnc
        do i=1,nc
          icnc=i
          if (x(i).gt.0.d0) then
            call RMIX (x)
c           call TPRHO(t,p,x,2,0,dp,ierr,herr)
            call TPFLSH (t,p,x,dp,dl,dv,xl,xv,q,e,h,s,cv,cp,w,ierr,herr)
            call FGCTY2 (t,dp,x,fp,ierr,herr)
            if (fp(i).gt.0.d0) actv(i)=f(i)/fp(i)
            gamma(i)=actv(i)/x(i)
          endif
        enddo
        icnc=ic2
        call RMIX (x)
      endif
c
      RETURN
      end                                              !subroutine ACTVY
c
c ======================================================================
c
      subroutine VIRB (t,x,b)
c
c  compute second virial coefficient as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        b--second virial coefficient [L/mol]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  03-27-98 EWL, original version
c  08-30-04 EWL, change rho to 0.00000001
c
cx DEC$ ATTRIBUTES DLLEXPORT :: VIRB
c
      include 'commons.for'
      dimension x(ncmax)
c
      b=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rho=0.00000001d0
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
        b=(p/rho/R/t-1.0d0)/rho
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi01=PHIK(icomp,0,1,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi01=PHIX(0,1,tau,del,x)
        end if
        b=phi01/rho
      end if
c
      RETURN
      end                                               !subroutine VIRB
c
c ======================================================================
c
      subroutine DBDT (t,x,dbt)
c
c  compute the 1st derivative of B (B is the second virial coefficient) with
c  respect to T as a function of temperature and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c      dbt--1st derivative of B with respect to T [L/mol-K]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  07-30-01 EWL, original version
c  08-30-04 EWL, change rho to 0.00000001
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DBDT
c
      include 'commons.for'
      dimension x(ncmax)
c
      dbt=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rho=0.00000001d0
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
        dpt=DPTBWR(icomp,t,rho)
        dbt=(dpt - p/t)/rho**2/t/R
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi11=PHIK(icomp,1,1,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi11=PHIX(1,1,tau,del,x)
        end if
        dbt=-phi11/rho/t
      end if
c
      RETURN
      end                                               !subroutine DBDT
c
c ======================================================================
c
      subroutine DBDT2 (t,x,dbt2)
c
c  compute the 2nd derivative of B (B is the second virial coefficient) with
c  respect to T as a function of temperature and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c      dbt2--2nd derivative of B with respect to T [L/mol-K^2]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  10-29-07 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DBDT2
c
      include 'commons.for'
      dimension x(ncmax)
c
      dbt2=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rho=0.00000001d0
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        dbt2=0     !not yet implemented
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi21=PHIK(icomp,2,1,tau,del)
          phi11=PHIK(icomp,1,1,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi21=PHIX(2,1,tau,del,x)
          phi11=PHIX(1,1,tau,del,x)
        end if
        dbt2=(phi21+2.d0*phi11)/rho/t**2
      end if
c
      RETURN
      end                                              !subroutine DBDT2
c
c ======================================================================
c
      subroutine VIRC (t,x,c)
c
c  compute the third virial coefficient as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        c--third virial coefficient [(L/mol)^2]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c   3-27-98 EWL, original version
c  12-02-98 EWL, change rho to 0.0001 to avoid numerical problems in BWR calc.
c  08-30-04 EWL, change rho to 0.00000001
c  05-24-06 EWL, change rho to 0.000001 for the BWR
c  05-07-09 EWL, change rho to 0.0001 for the BWR
c
cx DEC$ ATTRIBUTES DLLEXPORT :: VIRC
c
      include 'commons.for'
      dimension x(ncmax)
c
      c=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rho=0.000001d0
      if (heos.eq.'BWR') then
        rho=0.0001d0
c  pure fluid MBWR equation of state--call BWR-specific routines
        icomp=1
        p=PBWR(icomp,t,rho)
        dpd=DPDBWR(icomp,t,rho)
        c=((dpd-2.0d0*p/rho)/R/t+1.0d0)/rho**2
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi02=PHIK(icomp,0,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi02=PHIX(0,2,tau,del,x)
        end if
        c=phi02/rho**2
      end if
c
      RETURN
      end                                               !subroutine VIRC
c
c ======================================================================
c
      subroutine DCDT (t,x,dct)
c
c  compute the 1st derivative of C (C is the third virial coefficient) with
c  respect to T as a function of temperature and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c      dct--1st derivative of C with respect to T [(L/mol)^2-K]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  10-29-07 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DCDT
c
      include 'commons.for'
      dimension x(ncmax)
c
      dct=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rho=0.00000001d0
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        dct=0.d0      !not yet implemented
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi12=PHIK(icomp,1,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi12=PHIX(1,2,tau,del,x)
        end if
        dct=-phi12/rho**2/t
      end if
c
      RETURN
      end                                               !subroutine DCDT
c
c ======================================================================
c
      subroutine DCDT2 (t,x,dct2)
c
c  compute the 2nd derivative of C (C is the third virial coefficient) with
c  respect to T as a function of temperature and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c      dct2--2nd derivative of C with respect to T [(L/mol-K)^2]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  10-29-07 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DCDT2
c
      include 'commons.for'
      dimension x(ncmax)
c
      dct2=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
      rho=0.00000001d0
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
        dct2=0     !not yet implemented
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi22=PHIK(icomp,2,2,tau,del)
          phi12=PHIK(icomp,1,2,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi22=PHIX(2,2,tau,del,x)
          phi12=PHIX(1,2,tau,del,x)
        end if
        dct2=(phi22+2.d0*phi12)/rho**2/t**2
      end if
c
      RETURN
      end                                              !subroutine DCDT2
c
c ======================================================================
c
      subroutine VIRD (t,x,d)
c
c  compute the fourth virial coefficient as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        d--fourth virial coefficient [(L/mol)^3]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  11-26-01 EWL, original version
c  12-08-11 EWL, change rho to 1.d-4, was too small for d=1 Gaussian terms
c
      include 'commons.for'
      dimension x(ncmax)
c
      d=0.d0
      if (t.le.0.d0) return
      call RMIX (x)
c     rho=0.00000001d0
      rho=1.d-4
      if (heos.eq.'BWR') then
c  pure fluid MBWR equation of state--call BWR-specific routines
c       icomp=1
c       p=PBWR(icomp,t,rho)
c       dpd=DPDBWR(icomp,t,rho)
c  need to update with correct formula:
c       c=((dpd-2.0d0*p/rho)/R/t+1.0d0)/rho**2
        d=0
c
      else
c  call general PHIK or PHIX routines for all other models
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
c  pure fluid
          tau=tz(icomp)/t
          del=rho/rhoz(icomp)
          phi03=PHIK(icomp,0,3,tau,del)
        else
c  mixture
          call REDX (x,t0,rho0)
          tau=t0/t
          del=rho/rho0
          phi03=PHIX(0,3,tau,del,x)
        end if
        d=phi03/rho**3
      end if
c
      RETURN
      end                                               !subroutine VIRD
c
c ======================================================================
c
      subroutine VIRBA (t,x,ba)
c
c  compute second acoustic virial coefficient as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        ba--second acoustic virial coefficient [L/mol]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  10-29-07 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: VIRBA
c
      include 'commons.for'
      dimension x(ncmax)
c
      ba=0.d0
      if (t.le.0.d0) return
      call VIRB (t,x,b)
      call DBDT (t,x,dbt)
      call DBDT2 (t,x,dbt2)
      cp00=CP0(t,x)
      gpg=cp00/(cp00-R)
c  Trusler and Zarari, J. Chem. Thermodyn., 28:329-335, 1996.
c  Gillis and Moldover, Int. J. Theromphys., 17(6):1305-1324, 1996.
      ba=2.d0*b+2.d0*(gpg-1.d0)*t*dbt+(gpg-1.d0)**2/gpg*t**2*dbt2
c
      RETURN
      end                                              !subroutine VIRBA
c
c ======================================================================
c
      subroutine VIRCA (t,x,ca)
c
c  compute third acoustic virial coefficient as a function of temperature
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        ca--third acoustic virial coefficient [(L/mol)^2]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  10-29-07 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: VIRCA
c
      include 'commons.for'
      dimension x(ncmax)
c
      ca=0.d0
      if (t.le.0.d0) return
      call VIRB (t,x,b)
      call VIRBA (t,x,ba)
      call DBDT (t,x,dbt)
      call DBDT2 (t,x,dbt2)
      call VIRC (t,x,c)
      call DCDT (t,x,dct)
      call DCDT2 (t,x,dct2)
      cp00=CP0(t,x)
      gpg=cp00/(cp00-R)
c  Gillis and Moldover, Int. J. Theromphys., 17(6):1305, 1996.
c  Estela-Uribe and Trusler, Int.  J. Theromphys., 21(5):1033, 2000.
      q=b+(2.d0*gpg-1.d0)*t*dbt+(gpg-1.d0)*t**2*dbt2
      ca=(gpg-1.d0)*q**2+(2.d0*gpg+1.d0)*c
      ca=ca+(gpg**2-1.d0)*t*dct+(gpg-1.d0)**2/2.d0*t**2*dct2
      ca=ca/gpg
c  to convert to the pressure expansion form, use this:
c     ca=(el-ba*b)/R/t
c
      RETURN
      end                                              !subroutine VIRCA
c
c ======================================================================
c
      subroutine B12 (t,x,b)
c
c  compute b12 as a function of temperature and composition.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        b--b12 [(L/mol)^2]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  04-19-01 EWL, original version
c
      include 'commons.for'
      dimension x(ncmax),xb(ncmax)
c
      if (nc.ne.2 .or. x(1).le.0.d0 .or. x(itwo2).le.0.d0) then   !Only calculate b12 for a binary
        b=0
        RETURN
      endif
      call VIRB (t,x,bx)
c     ic2=icnc         !Do not change icnc because it affects parallel code while fitting
c     icnc=1
      xb(ione1)=1.d0
      xb(itwo2)=0.d0
      call VIRB (t,xb,b1)
c     icnc=2
      xb(ione1)=0.d0
      xb(itwo2)=1.d0
      call VIRB (t,xb,b2)
      b=(bx-x(1)**2*b1-x(itwo2)**2*b2)/2.d0/x(1)/x(itwo2)
c     icnc=ic2
      RETURN
      end                                                !subroutine B12
c
c ======================================================================
c
      subroutine EXCESS (t,p,x,kph,rho,vE,eE,hE,sE,aE,gE,ierr,herr)
c
c  compute excess properties as a function of temperature, pressure,
c  and composition.
c
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        x--composition [array of mol frac]
c      kph--phase flag:  1 = liquid
c                        2 = vapor
c                        0 = stable phase
c  outputs:
c       rho--molar density [mol/L] (if input less than 0, used as initial guess)
c        vE--excess volume [L/mol]
c        eE--excess energy [J/mol]
c        hE--excess enthalpy [J/mol]
c        sE--excess entropy [J/mol-K]
c        aE--excess Helmholtz energy [J/mol]
c        gE--excess Gibbs energy [J/mol]
c      ierr--error flag:  0 = successful
c                        55 = T,p inputs in different phase for the pure fluids
c      herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  04-25-02 EWL, original version
c  11-04-08 EWL, add ierr and herr to argument list
c  11-26-08 EWL, add aE and gE to argument list
c  03-21-10 EWL, add log(x(i)) to sE, gE, and aE
c  08-24-11 EWL, replace use of 'ic2=ic' with z(i)=1 so that the fitter can parallelize the code
c  11-23-11 EWL, remove the checks for inputs in different phases
c
      include 'commons.for'
      dimension x(ncmax),xliq(ncmax),xvap(ncmax),z(ncmax)
      character*255 herr
c
      ierr=0
      herr=' '
      d=rho
      vE=0.d0
      eE=0.d0
      hE=0.d0
      sE=0.d0
      aE=0.d0
      gE=0.d0
      rho=0.d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) RETURN
c
      kguess=0
      if (d.lt.0) kguess=1
      d=abs(d)
      if (kph.ne.0) then
       call TPRHO (t,p,x,kph,kguess,d,ierr,herr)
       if (ierr.ne.0) then
         d=d*2
         call TPRHO (t,p,x,kph,1,d,ierr,herr)
       endif
       if (ierr.ne.0)
     & call TPFLSH(t,p,x,d,dl,dv,xliq,xvap,q,eE,hE,sE,cv,cp,w,ierr,herr)
       call THERM (t,d,x,pp,eE,hE,sE,cv,cp,w,hjt)
       call AG (t,d,x,aE,gE)
      else
       call TPFLSH(t,p,x,d,dl,dv,xliq,xvap,q,eE,hE,sE,cv,cp,w,ierr,herr)
       call AG (t,d,x,aE,gE)
      endif
      if (ierr.ne.0) RETURN
      rho=d
      if (d.gt.0.d0) vE=1.d0/d
c
      call CRITP (x,tc2,pc2,Dc2,ierr,herr)
c     ic2=icnc
      do i=1,nc
        do j=1,nc
          z(j)=0.d0
        enddo
        z(i)=1.d0
c       icnc=i
c...Do not use kph or call TPRHO since the pure fluids could be in either phase
        call RMIX (z)
        RT=R*t
        call INFO (i,wmm,ttrp,tnbpt,tc,pc,dc,zc,acf,dip,rgas)
        call TPFLSH (t,p,z,d,dl,dv,xliq,xvap,q,e,h,s,cv,cp,w,ierr,herr)
c       if (d.gt.dc .and. rho.lt.Dc2 .and. t.lt.tc) goto 100
c       if (d.lt.dc .and. rho.gt.Dc2 .and. t.lt.tc) goto 100
        call AG (t,d,z,a,g)
        if (d.gt.0.d0) vE=vE-x(i)/d
        eE=eE-x(i)*e
        hE=hE-x(i)*h
        sE=sE-x(i)*(s- R*log(x(i)))
        aE=aE-x(i)*(a+RT*log(x(i)))
        gE=gE-x(i)*(g+RT*log(x(i)))
      enddo
c     icnc=ic2
      call RMIX (x)
      RETURN
c
c100  continue
c     vE=0.d0
c     eE=0.d0
c     hE=0.d0
c     sE=0.d0
c     aE=0.d0
c     gE=0.d0
c     ierr=55
c     herr='[EXCESS error] temperature and pressure inputs are in '//
c    &     'different phases for the pure fluids'//hnull
c     call ERRMSG (ierr,herr)
c     icnc=ic2
c     call RMIX (x)
c     RETURN
      end                                             !subroutine EXCESS
c
c ======================================================================
c
      subroutine FPV (t,rho,p,x,f)
c
c  Compute the supercompressibility factor, Fpv.
c
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c  outputs:
c        f--Fpv = sqrt[Z(60 F, 14.73 psia)/Z(T,P)]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  11-07-02 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: FPV
c
      include 'commons.for'
      dimension x(ncmax)
      character*255 herr
c
      tfpv = 288.705555555556d0    !60 F
      pfpv = 101.55977492837d0     !14.73 psia
      call TPRHO (tfpv,pfpv,x,2,0,dfpv,ierr,herr)
      if (p.gt.0.d0) then
        f=SQRT(pfpv/tfpv/dfpv*rho*t/p)
      else
        f=SQRT(pfpv/tfpv/dfpv/R)
      endif

      RETURN
      end                                                !subroutine FPV
Cc
Cc ======================================================================
Cc
C      subroutine SPECGR (t,rho,p,gr)
Cc
Cc  Compute the specific gravity (relative density).
Cc
Cc  inputs:
Cc        t--temperature [K]
Cc        p--pressure [kPa]
Cc      rho--molar density [mol/L]
Cc  outputs:
Cc       gr--specific gravity [dimensionless]
Cc
Cc  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
Cc  11-07-02 EWL, original version
Cc
C      include 'commons.for'
Cc
C      rhoair=1.d0      !Need to add formulation for air here
C      if (rhoair.gt.0.d0) then
C        gr=rho/rhoair
C      else
C        gr=1.d0
C      endif
C
C      RETURN
C      end                                             !subroutine SPECGR
c
c ======================================================================
c
      subroutine RMIX (x)
c
c  inputs:
c        x--composition [array of mol frac]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-19-01 EWL, original version
c
      include 'commons.for'
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        R=Reos(icomp)
      else
        R=0.0d0
        do i=1,nc
          R=R+x(i)*Reos(i)
        enddo
      endif
      if (R.lt.1.d-10) R=8.314472d0  !Check for bad x(i)
      RETURN
      end                                               !subroutine RMIX
c
c ======================================================================
c
      subroutine RMIX2 (x,Rgas)
c
c  Return the gas "constant" as a combination of the gas constants for
c  the pure fluids
c
c  inputs:
c        x--composition [array of mol frac]
c  outputs:
c     Rgas--gas constant [J/mol-K]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-21-10 EWL, original version
c
      include 'commons.for'
      dimension x(ncmax)
c
      call RMIX (x)
      Rgas=R
      RETURN
      end                                              !subroutine RMIX2
c
c ======================================================================
c
      subroutine THERM3 (t,rho,x,
     &           xkappa,beta,xisenk,xkt,betas,bs,xkkt,thrott,pi,spht)
c
c  Compute miscellaneous thermodynamic properties
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c
c  outputs:
c   xkappa--Isothermal compressibility [1/kPa]
c     beta--Volume expansivity [1/K]
c   xisenk--Isentropic expansion coefficient [-]
c      xkt--Isothermal expansion coefficient [-]
c    betas--Adiabatic compressibility [1/kPa]
c       bs--Adiabatic bulk modulus [kPa]
c     xkkt--Isothermal bulk modulus [kPa]
c   thrott--Isothermal throttling coefficient [L/mol]
c       pi--Internal pressure [kPa]
c     spht--Specific heat input [J/mol]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  06-16-06 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THERM3
c
      include 'commons.for'
      dimension x(ncmax)
c
      call THERM2 (t,rho,x,p,e,h,s,cv,cp,w,Z,hjt,A,G,xkappa,beta,
     &             dPdrho,d2PdD2,dPT,drhodT,drhodP,
     &             d2PT2,d2PdTD,spare3,spare4)
      wmm=WMOL(x)
      xisenk=0.d0
      if (p.le.0.d0) then
        xkt=1
        if (t.gt.0.d0) xisenk=w**2/R/T*wmm*0.001d0
      else
        xkt=rho/p*dPdrho               !Isothermal expansion coefficient
        xisenk=w**2*rho/p*wmm*0.001d0   !Isentropic expansion coefficient
      endif
      betas=xnotc
      if (rho.gt.0.d0 .and. w.gt.0.d0)
     &  betas=1.d0/rho/w**2/wmm*1000.d0 !Adiabatic compressibility
      bs=xisenk*p                      !Adiabatic bulk modulus
      xkkt=xkt*p                       !Isothermal bulk modulus
      thrott=-hjt*cp                   !Isothermal throttling coef.
      pi=t*dpt-p                       !Internal pressure
      if (abs(dpt).gt.1.d-20) then
        spht=rho*cp*dpdrho/dpt         !Specific heat input
      else
        spht=cp*t
      end If
      RETURN
      end                                             !subroutine THERM3
c
c ======================================================================
c
      subroutine VIRBCD (t,x,b,c,d)
c
c  Compute virial coefficients as a function of temperature
c  and composition.  The routine currently works only for pure fluids and
c  for the Helmholtz equation.
c  All values are computed exactly based on the terms in the eos, not
c  as done in VIRB by calculating properties at rho=1.d-8.
c
c  inputs:
c        t--temperature [K]
c        x--composition [array of mol frac]
c  outputs:
c        b--second virial coefficient [l/mol]
c        c-- third virial coefficient [(l/mol)^2]
c        d--fourth virial coefficient [(l/mol)^3]
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  01-01-10 EWL, original version
c  10-08-11 EWL, include Gaussian terms and change input parameters
c
      include 'commons.for'
      dimension x(ncmax),phisvv(n0:nx,mxtrm)
c
      b=0.d0
      c=0.d0
      d=0.d0
      call ISPURE (x,icomp)
      if (icomp.eq.0) RETURN
      if (t.le.0.0d0) RETURN
      if (hmxeos(icomp)(1:2).ne.'FE') RETURN
C
      tau=tz(icomp)/t
      elntau=log(tau)
      do k=1,ntermf(icomp)
        phisvv(icomp,k)=afeq(icomp,k)*EXP(tifeq(icomp,k)*elntau)
      enddo
      do i=1,ncrt(icomp)
        k=i+ntermf(icomp)
        da=-deltaf(icomp,i)
        ta=tau-gammaf(icomp,i)
        if (dlif(icomp,k).eq.2 .and. tlif(icomp,i).eq.2) then
          df=alphaf(icomp,i)*da**INT(dlif(icomp,k))
          tf= betaf(icomp,i)*ta**INT(tlif(icomp,i))
        else
          df=alphaf(icomp,i)*da**dlif(icomp,k)
          tf= betaf(icomp,i)*ta**tlif(icomp,i)
        endif
        if (eitf(icomp,i).ne.0.d0) then
          phisvv(icomp,k)=0.d0
        else
          phisvv(icomp,k)=afeq(icomp,k)*EXP(tifeq(icomp,k)*elntau+tf+df)
        endif
      enddo
c
c  second virial coefficient
      do k=1,ntermf(icomp)+ncrt(icomp)
        if (difeq(icomp,k).eq.1) then
          b=b+phisvv(icomp,k)
        endif
      enddo
      b=b/rhoz(icomp)
c
c  third virial coefficient
      do k=1,ntermf(icomp)
        if (difeq(icomp,k).eq.2) then
          c=c+2.d0*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.1 .and. dlif(icomp,k).eq.1) then
          c=c-2.d0*phisvv(icomp,k)
        endif
      enddo
      do i=1,ncrt(icomp)
        k=i+ntermf(icomp)
        if (difeq(icomp,k).eq.2) then
          c=c+2.d0*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.1) then
          c=c-4.d0*alphaf(icomp,i)*deltaf(icomp,i)*phisvv(icomp,k)
        endif
      enddo
      c=c/rhoz(icomp)**2
c
c  fourth virial coefficient
      do k=1,ntermf(icomp)
        if (difeq(icomp,k).eq.3) then
          d=d+6.d0*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.1 .and. dlif(icomp,k).eq.2) then
          d=d-6.d0*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.2 .and. dlif(icomp,k).eq.1) then
          d=d-6.d0*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.1 .and. dlif(icomp,k).eq.1) then
          d=d+3.d0*phisvv(icomp,k)
        endif
      enddo
      do i=1,ncrt(icomp)
        k=i+ntermf(icomp)
        if (difeq(icomp,k).eq.3) then
          d=d+6.d0*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.2) then
          d=d-12.d0*alphaf(icomp,i)*deltaf(icomp,i)*phisvv(icomp,k)
        elseif (difeq(icomp,k).eq.1) then
          d=d+phisvv(icomp,k)*(+6.d0*alphaf(icomp,i)
     &         +12.d0*(alphaf(icomp,i)*deltaf(icomp,i))**2)
        endif
      enddo
      d=d/rhoz(icomp)**3
c
      RETURN
      end                                             !subroutine VIRBCD
c
c ======================================================================
c
      subroutine HEAT (t,rho,x,hg,hn,ierr,herr)
c
c  Compute the ideal gas gross and net heating values.
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c
c  outputs:
c       hg--gross (or superior) heating value [J/mol]
c       hn--net (or inferior) heating value [J/mol]
c     ierr--error flag:  0 = successful
c                        1 = error in chemical formula
c                        2 = not all heating values available
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  01-09-08 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: HEAT
c
      include 'commons.for'
      character*255 herr,hcf
      dimension x(ncmax),v(6)
c
      call ISPURE (x,icomp)
      hg=0.d0
      hn=0.d0
      do k=1,nc
        acrb=0.d0   !Number of carbon atoms
        ahyd=0.d0
        aoxy=0.d0
        anit=0.d0
        asul=0.d0
        hgk=0.d0
        hnk=0.d0
c
c  extract chemical formula from hsyn1, use the part in {} if available
        hcf=hsyn1(k)
        i=index(hcf,'{')
        if (i.eq.0) then
          i=index(hcf,'!')
          if (i.ne.0) hcf=hcf(1:i-1)
        else
          hcf=hcf(i+1:255)
          i=index(hcf,'}')
          if (i.ne.0) hcf=hcf(1:i-1)
        endif
c
c  extract the number of carbon, hydrogen, oxygen, ..., atoms from
c  the chemical formula
 10     continue
        if (hcf(2:2).lt.'a' .or. hcf(2:2).gt.'z')
     &      hcf=hcf(1:1)//' '//hcf(2:255)       !Add space in second slot
        do i=3,5
          if (hcf(i:i).lt.'0' .or. hcf(i:i).gt.'9')
     &        hcf=hcf(1:i-1)//' '//hcf(i:255)       !Add space in ith slot
        enddo
c
        read (hcf(3:5),'(i3)') j
        if (j.eq.0) j=1
        if (hcf(1:2).eq.'C ') then           !Carbon
          if (acrb.gt.0) goto 999
          acrb=real(j)
        elseif (hcf(1:2).eq.'H ') then       !Hydrogen
          if (ahyd.gt.0) goto 999
          ahyd=real(j)
        elseif (hcf(1:2).eq.'O ') then       !Oxygen
          if (aoxy.gt.0) goto 999
          aoxy=real(j)
        elseif (hcf(1:2).eq.'N ') then       !Nitrogen
          if (anit.gt.0) goto 999
          anit=real(j)
        elseif (hcf(1:2).eq.'S ') then       !Sulfur
          if (asul.gt.0) goto 999
          asul=real(j)
        endif
        hcf=hcf(6:255)
        if (hcf.ne.' ') goto 10
c
c       if (acrb+ahyd+aoxy+anit+asul.eq.0) goto 999   !Unknown substance
c
        if (abs(hcmbst(k)+1.d0).lt.1.d-12) goto 998
        hgk=hcmbst(k)*1000.d0
        v(1)=-ahyd/2.d0                        !Water produced
        v(2)=acrb+ahyd/4.d0-aoxy/2.d0+asul     !Oxygen needed
        v(3)=-acrb                             !CO2 produced
        v(4)=-anit/2.d0                        !Nitrogen
        v(5)=-asul                             !SO2
c
        t25=298.15d0
        rho0=0.d0
        call ENTHAL (t25,rho0,x,h25)
        call ENTHAL (t,rho0,x,h)
        hgk=hgk-(h25-h)
c
        call ENTHHC (0,t25,t,h)                !Liquid water
        hgk=hgk-v(1)*h
        do i=2,5
          call ENTHHC (i,t25,t,h)
          hgk=hgk-v(i)*h
        enddo
c
        call ENTHHC (0,t,-1.d0,h1)                !Liquid water
        call ENTHHC (1,t,-1.d0,h2)                !Ideal gas water
        hnk=hgk+v(1)*(h2-h1)
c
        if (k.eq.icomp) then
          hg=hgk
          hn=hnk
        else
          hg=hg+x(k)*hgk
          hn=hn+x(k)*hnk
        endif
      enddo
c
      ierr=0
      herr=' '
      RETURN
c
 998  ierr=2
      herr='[HEAT error 2] Heating values are not available for all '//
     &     'species in the mixture'//hnull
      hg=0.d0
      hn=0.d0
      RETURN
c
 999  ierr=1
      herr='[HEAT error 1] Error in chemical formula'//hnull
      hg=0.d0
      hn=0.d0
      RETURN
      end                                               !subroutine HEAT
c
c ======================================================================
c
      subroutine ENTHHC (icmb,t1,t2,h)
c
c  Compute the ideal gas enthalpy difference between temperatures t1 and
c  t2 for several combustion gases.  This is used in conjunction with the
c  heat of combustion subroutine so that the extra fluids do not have to be
c  loaded in memory.  If the EOS ever changes, then the coefficients
c  here must be updated.
c
c  inputs:
c       t1--temperature [K]
c       t2--temperature [K]
c           if t2 is less than zero, then absolute enthalpy calculated at t1
c     icmb--fluid identifier:
c           0-saturated liquid water
c           1-water
c           2-oxygen
c           3-CO2
c           4-nitrogen
c           5-SO2
c
c  outputs:
c        h--ideal gas enthalpy difference [J/mol]
c           (for i=0, then saturated liquid enthalpy difference of water)
c
c  written by E.W. Lemmon, NIST Physical & Chem Properties Div, Boulder, CO
c  01-09-08 EWL, original version
c  04-30-12 EWL, check for T>Tc(water)
c
      include 'commons.for'
      dimension xkhc(10),cpchc(10)
c
      h=0
      if (icmb.eq.0) then
        ntc=6
        cpchc(1)= 0.8943272d+5     !Enthalpy of saturated liquid water.
        cpchc(2)=-0.6138239d+5     !Fit by EWL on 1/9/2007
        cpchc(3)=-0.4415242d+5     !Matches Wagner & Pruss equation to within
        cpchc(4)=-0.1092092d+5     !0.02% over full saturation range.
        cpchc(5)= 0.6703065d+5     !Extrapolation to 200 K is smooth.
        cpchc(6)=-0.1763063d+6
        xkhc(1) = 0.032d0
        xkhc(2) = 0.078d0
        xkhc(3) = 0.825d0
        xkhc(4) = 4.d0
        xkhc(5) = 9.d0
        xkhc(6) =12.d0
        tcwater=647.096d0
        if (t1.gt.tcwater .or. t2.gt.tcwater) then
          h=0.d0
        else
          do i=1,ntc
                            h=h+cpchc(i)*(1.d0-t1/tcwater)**xkhc(i)
            if (t2.ge.0.d0) h=h-cpchc(i)*(1.d0-t2/tcwater)**xkhc(i)
          enddo
        endif
        RETURN
      elseif (icmb.eq.1) then
        ntc=1
        nte=5
        cpchc(1)=0.400632d+1             !Water
        cpchc(2)=0.124360d-1
        cpchc(3)=0.973150d+0
        cpchc(4)=0.127950d+1
        cpchc(5)=0.969560d+0
        cpchc(6)=0.248730d+0
        xkhc(1) =    0.d0
        xkhc(2) =  833.d0
        xkhc(3) = 2289.d0
        xkhc(4) = 5009.d0
        xkhc(5) = 5982.d0
        xkhc(6) =17800.d0
        R=8.314371357587d0
c                           value from above    value from refprop
c                           at 300 K            at 300 K
        if (t2.lt.0.d0) h=(-88471.35339670102d0+45964.71449803960d0)/r
      elseif (icmb.eq.2) then
        ntc=1
        nte=5
        cpchc(1)=3.51808732d0            !Oxygen
        cpchc(2)=0.102323928D+01
        cpchc(3)=0.784357918D+00
        cpchc(4)=0.337183363D-02
        cpchc(5)=-.170864084D-01
        cpchc(6)=0.463751562D-01
        xkhc(1) =0.d0
        xkhc(2) =0.224632440D+04
        xkhc(3) =0.112599763D+05
        xkhc(4) =0.120126209D+04
        xkhc(5) =0.690089445D+02
        xkhc(6) =0.532805445D+04
        R=8.31434d0
        if (t2.lt.0.d0) h=(-56058.60590328777d0+8734.35384436554d0)/r
      elseif (icmb.eq.3) then
        ntc=1
        nte=5
        cpchc(1)=0.35d+01                !CO2
        cpchc(2)=1.99427042d0
        cpchc(3)=0.621052475d0
        cpchc(4)=0.411952928d0
        cpchc(5)=1.04028922d0
        cpchc(6)=0.0832767753d0
        xkhc(1) =   0.d0
        xkhc(2) = 958.49956d0
        xkhc(3) =1858.80115d0
        xkhc(4) =2061.10114d0
        xkhc(5) =3443.89908d0
        xkhc(6) =8238.20035d0
        R=8.31451d0
        if (t2.lt.0.d0) h=(-43458.10456571817d0+22372.0720622156d0)/r
      elseif (icmb.eq.4) then
        ntc=4
        nte=1
        cpchc(1)= 3.5d0                  !Nitrogen
        cpchc(2)= 3.066469d-6
        cpchc(3)= 4.70124d-9
        cpchc(4)=-3.987984d-13
        cpchc(5)= 0.1012941d1
        xkhc(1) = 0.d0
        xkhc(2) = 1.d0
        xkhc(3) = 2.d0
        xkhc(4) = 3.d0
        xkhc(5) = 3364.011d0
        R=8.31451d0
        if (t2.lt.0.d0) h=(-22898.14229042497d0+8723.88255738888d0)/r
      elseif (icmb.eq.5) then
        ntc=2
        nte=2
        cpchc(1)= 4.0d0                  !SO2
        cpchc(2)= 0.72453d-4
        cpchc(3)= 1.0620d0
        cpchc(4)= 1.9401d0
        xkhc(1) =    0.d0
        xkhc(2) =    1.d0
        xkhc(3) =  775.d0
        xkhc(4) = 1851.d0
        R=8.314472d0
        if (t2.lt.0.d0) h=(-28976.84624561172d0+26659.0278471527d0)/r
      endif
c
      do i=1,ntc
        xkhci=xkhc(i)
        xkhc1=xkhci+1.0d0
                        h=h+cpchc(i)*t1**xkhc1/xkhc1
        if (t2.ge.0.d0) h=h-cpchc(i)*t2**xkhc1/xkhc1
      enddo
      do i=1,nte
        j=i+ntc
        expui1=EXP(xkhc(j)/t1)
        expui2=EXP(xkhc(j)/t2)
        if (t2.ge.0.d0)
     &  h=h-cpchc(j)*(-0.5d0*xkhc(j))*(1.0d0+expui2)/(1.0d0-expui2)
        h=h+cpchc(j)*(-0.5d0*xkhc(j))*(1.0d0+expui1)/(1.0d0-expui1)
      enddo
      h=h*R
      end                                             !subroutine ENTHHC
c
c ======================================================================
c
      subroutine ISPURE (x,icomp)
c
c  Determine if the user has requested the properties of a pure fluid.
c  This happens if 1) nc=1, 2) PUREFLD has been called, or 3) one of the
c  compositions in the x array is one.
c
c  inputs:
c        x--composition [array of mol frac]
c
c  outputs:
c    icomp--index set to the pure fluid, which is 1 for nc=1 or 0 for
c           a mixture.  If PUREFLD has been called, icomp=icnc.
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  05-01-08 EWL, original version
c
      include 'commons.for'
      dimension x(ncmax)
c
      icomp=0
      if (nc.eq.1) then
        icomp=1
      elseif (icnc.gt.0) then
        icomp=icnc
      else
        do i=1,nc
          if (ABS(x(i)-1.d0).lt.1.d-12) then
            icomp=i
            RETURN
          endif
        enddo
      endif
      RETURN
      end                                             !subroutine ISPURE
c
c ======================================================================
c
      subroutine RIEM (t,rho,x,riemc)
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition [array of mol frac]
c
c  outputs:
c    riemc--RIEM is the thermodynamic curvature in cubic nanometers/molecule.
c    RIEM has magnitude of the correlation volume, is negative for attractive
c    interactions, and positive for repulsive interactions, except when its
c    magnitude gets smaller than the molecular volume.
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  08-01-11 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: RIEM
      include 'commons.for'
      dimension x(ncmax)
c
      riemc=0.d0
      if (rho.lt.1.d-8) return
      call REDX (x,t0,rho0)
      tau=t0/t
      del=rho/rho0
      a10=(PHI0(0,1,t,rho,x)+PHIX(0,1,tau,del,x))/del
      a20=(PHI0(0,2,t,rho,x)+PHIX(0,2,tau,del,x))/del**2
      a30=(PHI0(0,3,t,rho,x)+PHIX(0,3,tau,del,x))/del**3
      a11=(PHI0(1,1,t,rho,x)+PHIX(1,1,tau,del,x))/tau/del
      a21=(PHI0(1,2,t,rho,x)+PHIX(1,2,tau,del,x))/tau/del**2
      a12=(PHI0(2,1,t,rho,x)+PHIX(2,1,tau,del,x))/tau**2/del
      a02=(PHI0(2,0,t,rho,x)+PHIX(2,0,tau,del,x))/tau**2
      a03=(PHI0(3,0,t,rho,x)+PHIX(3,0,tau,del,x))/tau**3
      sum=-2.d0*       a02**2*a10
     &    +4.d0*del*   a03*a10*a11 +4.d0*del   *a02*a11**2
     &    -4.d0*del*   a02*a10*a12 -2.d0*del**2*a10*a12**2
     &    -4.d0*del*   a02**2*a20  +2.d0*del**2*a03*a11*a20
     &    -5.d0*del**2*a02*a12*a20 -     del**3*a12**2*a20
     &    +2.d0*del**2*a03*a10*a21 +4.d0*del**2*a02*a11*a21
     &    +     del**3*a03*a20*a21 +     del**3*a02*a21**2
     &    -     del**2*a02**2*a30  -     del**3*a02*a12*a30
      xna=6.0221367d+23
      riemc=1.d0/(2.d0*xna*del**2*rho0*a02**2*(2.d0*a10+del*a20)**2)*sum

      RETURN
      end                                               !subroutine RIEM
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file prop_sub.f
c ======================================================================
