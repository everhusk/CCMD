c  begin file realgas.f
c
c  This file contains the routines implementing the real-gas part of
c  the thermodynamic functions.  They call the corresponding "core"
c  routines for the specified component or mixture.
c
c  contained here are:
c     function PHIK (icomp,itau,idel,tau,del)
c     function PHIX (itau,idel,tau,del,x)
c     subroutine REDK (icomp,tred,Dred)
c     subroutine REDX (x,tred,Dred)
c
c ======================================================================
c ======================================================================
c
      function PHIK (icomp,itau,idel,tau,del)
c
c  compute reduced Helmholtz energy or a derivative as functions
c  of dimensionless temperature and density; calls the appropriate
c  core function
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
c
c        The Helmholtz energy consists of ideal and residual (real-gas)
c        terms; this routine calculates only the residual part.
c
c        This function computes pure component properties only.
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-07-96  MM, original version
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  03-05-98  MM, do not check mix model (heos) on branch
c  07-21-03 EWL, add Peng-Robinson check
c
      include 'commons.for'
c
      if (del.le.1.0d-10) then    !trivial solution at zero density
        phik=0.0d0                !for any and all derivatives
        RETURN
      end if
c
      if (hmxeos(icomp)(1:2).eq.'FE') then
        phik=PHIFEQ (icomp,itau,idel,tau,del)
      else if (hmxeos(icomp).eq.'QUI') then
        phik=PHIQUI (icomp,itau,idel,tau,del)
      else if (hmxeos(icomp).eq.'BWR') then
        phik=PHIBWR (icomp,itau,idel,tau,del)
      else if (hmxeos(icomp).eq.'ECS') then
        phik=PHIECS (icomp,itau,idel,tau,del)
      else if (hmxeos(icomp).eq.'PR') then
        phik=PHIPR (icomp,itau,idel,tau,del)
      else
c  model not found, but no way to return an error from here
c       write (*,*) ' PHIK--model not found: ',heos,' ',hmxeos(icomp)
        phik=-9.99d99
      end if
c      RETURN
      end                                                 !function PHIK
c
c ======================================================================
c
      function PHIX (itau,idel,tau,del,x)
c
c  compute reduced Helmholtz energy or a derivative as functions
c  of dimensionless temperature and density by calling the appropriate
c  mixture model
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
c  02-07-96  MM, original version
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-10-96  MM, if (nc = 1) call PHIK, rather than model-specific PHIxxx
c  10-30-02 EWL, add AGA8 mixture model
c  07-21-03 EWL, add Peng-Robinson check
c
      include 'commons.for'
      dimension x(ncmax)
c
      phix=0.0d0
      if (del.le.1.0d-10) RETURN  !trivial solution at zero density
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        phix=PHIK(icomp,itau,idel,tau,del)
      else
c  call HMX model
        if (heos.eq.'HMX') then
          phix=PHIHMX(itau,idel,tau,del,x)
c  call AGA8 model
        elseif (heos.eq.'AGA') then
          phix=PHIAGA(itau,idel,tau,del,x)
c  call Peng-Robinson model
        elseif (heos.eq.'PR') then
          phix=PHIPRX(itau,idel,tau,del,x)
        end if
      end if
c
      RETURN
      end                                                 !function PHIX
c
c ======================================================================
c
      subroutine REDK (icomp,tred,Dred)
c
c  returns reducing parameters associated with a pure fluid EOS;
c  used to calculate the 'tau' and 'del' which are the independent
c  variables in the EOS
c
c  N.B.  The reducing parameters are often, but not always, equal
c        to the critical temperature and density.
c
c  input:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c  outputs:
c     tred--reducing temperature [K]
c     Dred--reducing molar density [mol/L]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-27-96  MM, original version, adapted from REDFEQ and PHIK
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  03-05-98  MM, do not check mix model (heos) on branch
c
      include 'commons.for'
c
      if (hmxeos(icomp)(1:2).eq.'FE') then
        call REDFEQ (icomp,tred,Dred)
c       write (*,1001) icomp,tred,Dred
      else if (hmxeos(icomp).eq.'QUI') then
        call REDQUI (icomp,tred,Dred)
c       write (*,1001) icomp,tred,Dred
      else if (hmxeos(icomp).eq.'BWR') then
        call CRTBWR (icomp,tred,pc,Dred)
c       write (*,1001) icomp,tred,Dred
c1001   format (1x,' REDK--icomp,tred,Dred: ',i2,2e14.6)
      else if (hmxeos(icomp).eq.'ECS') then
        call CRTECS (icomp,tred,pc,Dred)
      else if (hmxeos(icomp).eq.'PR') then
        call CRTPR (icomp,tred,pc,Dred)
      else
c  model not found, but no way to return an error from here
c       write (*,1100) icomp,hmxeos(icomp)
c1100   format (1x,' REDK--model not found for icomp = ',i3,':  ',a3)
        tred=9.99d99
        Dred=9.99d99
      end if
c
      RETURN
      end                                               !subroutine REDK
c
c ======================================================================
c
      subroutine REDX (x,tred,Dred)
c
c  returns reducing parameters associated with mixture EOS;
c  used to calculate the 'tau' and 'del' which are the independent
c  variables in the EOS
c
c  input:
c        x--composition array [mol frac]
c  outputs:
c     tred--reducing temperature [K]
c     Dred--reducing molar density [mol/L]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-27-96  MM, original version; adapted from REDHMX and PHIX
c  03-11-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                (missed in original version)
c  03-22-96  MM, replace /MODEL/ with /EOSMOD/
c  10-10-96  MM, if (nc = 1) call REDK, rather than model-specific routine
c
      include 'commons.for'
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        call REDK (icomp,tred,Dred)
      else
        if (heos.eq.'HMX') then
          call REDHMX (x,tred,Dred)
        elseif (heos.eq.'AGA') then
          call REDHMX (x,tred,Dred)
        elseif (heos.eq.'PR') then
          call REDPR (x,tred,Dred)
        end if
      end if
c
      RETURN
      end                                               !subroutine REDX
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                     end file realgas.f
c ======================================================================
