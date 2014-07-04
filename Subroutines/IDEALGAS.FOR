c  begin file idealgas.f
c
c  This file contains the routines implementing the ideal-gas part of
c  the thermodynamic functions.  They call the corresponding "core"
c  routines for the specified component(s).
c
c  contained here are:
c     function CP0 (t,x)
c     function CPI (t,x)
c     function CPT (t,x)
c     function PHI0 (itau,idel,t,rho,x)
c     function CP0K (icomp,t)
c     function PHI0K (icomp,itau,idel,t,rho)
c     block data BDCNST
c
c  these routines set the values in the following common blocks
c     common /Gcnst/ R,tz(n0:nx),rhoz(n0:nx)
c
c  these routines use the following common blocks from other files
c
c ======================================================================
c ======================================================================
c
      function CP0 (t,x)
c
c  return mixture Cp0 calculated by appropriate core CP0xxx routine(s)
c
c  inputs:
c        t--temperature (K)
c        x--composition array (mol frac)
c  output (as function value):
c      cp0--ideal gas heat capacity, Cp0 (J/(mol-K))
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  07-24-95  MM, original version
c  10-03-95  MM, change /CPMOD/:  models specified by strings
c  11-08-95  MM, change to mixtures (x, rather than icomp, is input)
c  11-26-95  MM, rearrange argument list (x at end)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-22-96  MM, replace /CPMOD/ with /EOSMOD/
c  05-14-96  MM, add call to PH0 model (Helmholtz form)
c  06-17-96  MM, check only 'CP' rather than 'CPP' to allow CP1
c  12-24-02 EWL, ditto for 'PH' rather than 'PH0'
c  06-13-06 EWL, split into pure fluid and mixture sections
c
      include 'commons.for'
      dimension x(ncmax)
c
      cp0sum=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hmodcp(icomp)(1:2).eq.'CP') then
          cp0sum=CP0CPP(icomp,t)
        else if (hmodcp(icomp)(1:2).eq.'PH') then
          rho=0.0d0
          cp0sum=R*(1.0-PH0PH0(icomp,2,0,t,rho))
        else
          cp0sum=0.0d0
        end if
      else
        do i=1,nc
          if (hmodcp(i)(1:2).eq.'CP') then
c  polynomial fit
            cp0i=CP0CPP(i,t)
          else if (hmodcp(i)(1:2).eq.'PH') then
c  Helmholtz form ("fundamental equation")
            rho=0.0d0
            cp0i=R*(1.0-PH0PH0(i,2,0,t,rho))
          else
c           write (*,*) ' CP0:  ERROR--model input to CP0 not found'
            cp0i=0.0d0
          end if
          cp0sum=cp0sum+x(i)*cp0i
        enddo
      endif
      CP0=cp0sum
c
      RETURN
      end                                                  !function CP0
c
c ======================================================================
c
      function CPI (t,x)
c
c  return mixture integral of Cp0 over limits of Tref to T
c  calculated by appropriate core CPIxxx routine(s),
c  for use in enthalpy calculation
c
c  inputs:
c        t--temperature [K]
c        x--composition array [mol frac]
c  output (as function value):
c      cpi--integral of (Cp0 dT) over limits T-Tref [J/mol]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  07-24-95  MM, original version
c  10-03-95  MM, change /CPMOD/:  models specified by strings
c  11-08-95  MM, change to mixtures (x, rather than icomp, is input)
c  11-26-95  MM, rearrange argument list (x at end)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-22-96  MM, replace /CPMOD/ with /EOSMOD/
c  06-17-96  MM, check only 'CP' rather than 'CPP' to allow CP1
c  06-13-06 EWL, split into pure fluid and mixture sections
c
      include 'commons.for'
      dimension x(ncmax)
c
      cpisum=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hmodcp(icomp)(1:2).eq.'CP') then
          cpisum=CPICPP(icomp,t)
        else
          cpisum=0.0d0
        end if
      else
        do i=1,nc
          if (hmodcp(i)(1:2).eq.'CP') then
c  polynomial fit
            cpii=CPICPP(i,t)
          else
c           write (*,*) ' CPI:  ERROR--model input to CPI not found'
            cpii=0.0d0
          end if
          cpisum=cpisum+x(i)*cpii
        enddo
      endif
      CPI=cpisum
c
      RETURN
      end                                                  !function CPI
c
c ======================================================================
c
      function CPT (t,x)
c
c  return mixture integral of Cp0/T over limits of Tref to T
c  calculated by appropriate core CPTxxx routine(s),
c  for use in entropy calculation
c
c  inputs:
c        t--temperature [K]
c        x--composition array [mol frac]
c  output (as function value):
c      cpt--integral of (Cp0/T dT) over limits T-Tref [J/(mol-K)]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  07-24-95  MM, original version
c  10-03-95  MM, change /CPMOD/:  models specified by strings
c  11-08-95  MM, change to mixtures (x, rather than icomp, is input)
c  11-26-95  MM, rearrange argument list (x at end)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-22-96  MM, replace /CPMOD/ with /EOSMOD/
c  06-17-96  MM, check only 'CP' rather than 'CPP' to allow CP1
c  06-13-06 EWL, split into pure fluid and mixture sections
c
      include 'commons.for'
      dimension x(ncmax)
c
      cptsum=0.0d0
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hmodcp(icomp)(1:2).eq.'CP') then
          cptsum=CPTCPP(icomp,t)
        else
          cptsum=0.0d0
        end if
      else
        do i=1,nc
          if (hmodcp(i)(1:2).eq.'CP') then
c  polynomial fit
            cpti=CPTCPP(i,t)
          else
c           write (*,*) ' CPT:  ERROR--model input to CPT not found'
            cpti=0.0d0
          end if
          cptsum=cptsum+x(i)*cpti
        enddo
      endif
      CPT=cptsum
c
      RETURN
      end                                                  !function CPT
c
c ======================================================================
c
      function PHI0 (itau,idel,t,rho,x)
c
c  compute the ideal gas part of the reduced Helmholtz energy or a
c  derivative as functions of temperature and pressure for a mixture;
c  for use with the Helmholtz-explicit models (e.g. FEQ and HMX)
c
c  inputs:
c     itau--flag specifying order of temperature derivative to calc
c     idel--flag specifying order of density derivative to calculate
c           (the density derivatives are not used in the calculation
c           of any property, and are not implemented)
c           when itau = 0 and idel = 0, compute A0/RT
c           when itau = 1 and idel = 0, 1st temperature derivative
c           when itau = 2 and idel = 0, 2nd temperature derivative
c        t--temperature [K]
c      rho--density [mol/L]
c        x--composition array [mol frac]
c  output (as function value):
c     PHI0--ideal-gas part of the reduced Helmholtz energy (A/RT);
c           derivatives (as specified by itau and idel) are multiplied
c           by the corresponding power of tau; i.e. when itau = 1, the
c           quantity returned is tau*d(PHI0)/d(tau) and when itau = 2,
c           the quantity returned is tau*tau*d2(PHI0)/d(tau)**2,
c           where the tau's are the Tc/T evaluated for each component
c
c  N.B.  While the real-gas part of the Helmholtz energy is calculated
c        in terms of dimensionless temperature and density, the ideal-
c        gas part is calculated in terms of absolute temperature and
c        density.  (This distinction is necessary for mixtures.)
c
c        The Helmholtz energy consists of ideal-gas and residual
c        (real-gas) terms; this routine calculates only the ideal part.
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  08-04-95  MM, original version
c  08-21-95  MM, put saved variables into common (rather than save stmt)
c  10-03-95  MM, change /MODEL/ + /CPMOD/:  models specified by strings
c  11-08-95  MM, change to mixtures (x, rather than icomp, is input)
c  11-26-95  MM, rearrange argument list (x at end)
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-21-96  MM, delete reference to /MODEL/, not used
c  03-22-96  MM, replace /CPMOD/ with /EOSMOD/
c  04-19-96  MM, change input from pressure to density
c  05-14-96  MM, add call to PH0 model (Helmholtz form)
c  06-17-96  MM, check only 'CP' rather than 'CPP' to allow CP1
c  07-03-96  MM, bug fix:  x*log(x) terms apply only if itau = 0
c  07-05-96  MM, note that PH0xxx models return tau*phi_tau, etc.
c                (no change in this routine)
c  12-02-98 EWL, split into pure fluid and mixture sections
c
      include 'commons.for'
      dimension x(ncmax)
c
c     write (*,1002) itau,idel,t,rho
c1002 format (1x,' PHI0--itau,idel,t,rho: ',2i4,2e14.6)
c     write (*,1004) nc,(x(j),j=1,nc)
c1004 format (1x,' PHI0--nc,x(i): ',i4,5f14.8)
c  compute reduced Helmholtz energy
c  pure fluid calculation
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        if (hmodcp(icomp)(1:2).eq.'CP') then
c  polynomial fit of isobaric heat capacity for the ideal gas state
          phisum=PH0CPP(icomp,itau,idel,t,rho)
        else if (hmodcp(icomp)(1:2).eq.'PH') then
c  Helmholtz form ("fundamental equation")
          phisum=PH0PH0(icomp,itau,idel,t,rho)
        else
c  additional model
c         write (*,*) ' PHI0:  ERROR--model input to PHI0 not found'
          phisum=0.0d0
        end if
c       write (*,1018) i,phisum
c1018   format (1x,' PHI0--i,PHIsum:  ',i3,d25.15)
c  mixture calculation
      else
        phisum=0.0d0
        do i=1,nc
c  compute only if component composition >0 and <=1
          if (x(i).gt.1.0d-10 .and. x(i).le.1.0000000001d0) then
            if (hmodcp(i)(1:2).eq.'CP') then
c  polynomial fit of isobaric heat capacity for the ideal gas state
              phi0i=PH0CPP(i,itau,idel,t,rho)
            else if (hmodcp(i)(1:2).eq.'PH') then
c  Helmholtz form ("fundamental equation")
              phi0i=PH0PH0(i,itau,idel,t,rho)
            else
c  additional model
c             write (*,*) ' PHI0:  ERROR--model input to PHI0 not found'
              phi0i=0.0d0
            end if
            phisum=phisum+x(i)*phi0i
            if (itau.eq.0) then
              phisum=phisum+x(i)*log(x(i))
            end if
c           write (*,1018) i,x(i),phi0i,phisum
c1018       format (1x,' PHI0--i,x(i),PHIi,PHIsum:  ',i3,3d25.15)
          else if (x(i).gt.1.0d0) then
c           write (*,1020) i,x(i)
c1020       format (1x,' PHI0 ERROR--composition',i4,' out of range:',d16.6)
          end if
        enddo
      endif
      PHI0=phisum
c     write (*,1022) itau,t,rho,PHI0
c1022 format (1x,' PHI0:  itau,t,rho,output phi: ',i4,3e14.6)
c
      RETURN
      end                                                 !function PHI0
c
c ======================================================================
c
      function CP0K (icomp,t)
c
c  return pure fluid Cp0 calculated by appropriate core CP0xxx routine
c
c  inputs:
c    icomp--component number in mixture (1..nc)
c           0 for ECS reference fluid
c        t--temperature [K]
c  output (as function value):
c      cp0--ideal gas heat capacity, Cp0 [J/(mol-K)]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  12-12-95  MM, original version, adapted from function CP0
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-22-96  MM, replace /CPMOD/ with /EOSMOD/
c  05-14-96  MM, add call to PH0 model (Helmholtz form)
c  06-17-96  MM, check only 'CP' rather than 'CPP' to allow CP1
c  10-10-96  MM, this routine never called, comment out
c  10-15-96  MM, needed in some of the new transport routines, restore
c
      include 'commons.for'
c
      if (hmodcp(icomp)(1:2).eq.'CP') then
c  polynomial fit
        CP0K=CP0CPP(icomp,t)
      else if (hmodcp(icomp)(1:2).eq.'PH') then
c  Helmholtz form ("fundamental equation")
        rho=0.0d0
        CP0K=R*(1.0-PH0PH0(icomp,2,0,t,rho))
      else
c       write (*,*) ' CP0K:  ERROR--model input to CP0K not found'
        CP0K=0.0d0
      end if
c
      RETURN
      end                                                 !function CP0K
c
c ======================================================================
c
      function PHI0K (icomp,itau,idel,t,rho)
c
c  compute the ideal gas part of the reduced Helmholtz energy or a
c  derivative as functions of temperature and pressure for a specified
c  component
c
c  analogous to PHI0, except for component icomp, this is used by CVCPK
c  which, in turn, is used by transport routines to calculate Cv & Cp
c  for the reference fluid (component zero)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c     itau--flag specifying order of temperature derivative to calc
c     idel--flag specifying order of density derivative to calculate
c           (the density derivatives are not used in the calculation
c           of any property, and are not implemented)
c           when itau = 0 and idel = 0, compute A0/RT
c           when itau = 1 and idel = 0, 1st temperature derivative
c           when itau = 2 and idel = 0, 2nd temperature derivative
c        t--temperature [K]
c      rho--density [mol/L]
c  output (as function value):
c    PHI0K--ideal-gas part of the reduced Helmholtz energy (A/RT);
c           derivatives (as specified by itau and idel) are multiplied
c           by the corresponding power of tau; i.e. when itau = 1, the
c           quantity returned is tau*d(PHI0)/d(tau) and when itau = 2,
c           the quantity returned is tau*tau*d2(PHI0)/d(tau)**2,
c           where the tau's are the Tc/T evaluated for each component
c
c  N.B.  While the real-gas part of the Helmholtz energy is calculated
c        in terms of dimensionless temperature and density, the ideal-
c        gas part is calculated in terms of absolute temperature and
c        density.  (This distinction is necessary for mixtures.)
c
c        The Helmholtz energy consists of ideal-gas and residual
c        (real-gas) terms; this routine calculates only the ideal part.
c
c  written by M. McLinden, NIST Physical & Chem Properties Div, Boulder, CO
c  06-16-97  MM, original version; based on PHIO
c
      include 'commons.for'
c
      if (hmodcp(icomp)(1:2).eq.'CP') then
c  polynomial fit of isobaric heat capacity for the ideal gas state
        PHI0K=PH0CPP(icomp,itau,idel,t,rho)
      else if (hmodcp(icomp)(1:2).eq.'PH') then
c  Helmholtz form ("fundamental equation")
        PHI0K=PH0PH0(icomp,itau,idel,t,rho)
      else
c  additional model
c       write (*,*) ' PHI0K:  ERROR--model input to PHI0K not found'
        PHI0K=0.0d0
      end if
c     write (*,1022) itau,t,rho,PHI0K
c1022 format (1x,' PHI0K:  itau,t,rho,output phi: ',i4,3e14.6)
c
      RETURN
      end                                                !function PHI0K
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file idealgas.f
c ======================================================================
