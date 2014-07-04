c  begin file flash2.f
c
c  This file contains additional iterative routines which call the
c  intermediate level routines
c
c  contained here are:
c     subroutine THFLSH (t,h,z,kr,p,D,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
c     subroutine THFL1 (t,h,x,Dmin,Dmax,D,ierr,herr)
c     subroutine THFL2 (t,h,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
c    &                  p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine TSFLSH (t,s,z,kr,p,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
c     subroutine TSFL1 (t,s,x,Dmin,Dmax,D,ierr,herr)
c     subroutine TSFL2 (t,s,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
c    &                  p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine TEFLSH (t,e,z,kr,p,D,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
c     subroutine TEFL1 (t,e,x,Dmin,Dmax,D,ierr,herr)
c     subroutine TEFL2 (t,e,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
c    &                  p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine DHFLSH (D,h,z,t,p,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
c     subroutine DHFL1 (rho,h,x,t,ierr,herr)
c     subroutine DHFL2 (d,h,z,t,p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine DSFLSH (D,s,z,t,p,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
c     subroutine DSFL1 (rho,s,x,t,ierr,herr)
c     subroutine DSFL2 (d,s,z,t,p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine DEFLSH (D,e,z,t,p,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
c     subroutine DEFL1 (rho,e,x,t,ierr,herr)
c     subroutine DEFL2 (d,e,z,t,p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine HSFLSH (h,s,z,t,p,D,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
c     subroutine HSFL1 (h,s,x,Dmin,Dmax,t,D,ierr,herr)
c     subroutine ESFLSH (e,s,z,t,p,D,Dl,Dv,x,y,q,h,cv,cp,w,ierr,herr)
c
c     subroutine TQFLSH (t,q,z,kq,p,D,Dl,Dv,x,y,e,h,s,cv,cp,w,ierr,herr)
c     subroutine TQFL2 (t,q,z,kq,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
c    &                  p,Dl,Dv,x,y,ierr,herr)
c     subroutine PQFLSH (p,q,z,kq,t,D,Dl,Dv,x,y,e,h,s,cv,cp,w,ierr,herr)
c     subroutine PQFL2 (p,q,z,kq,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
c    &                  t,Dl,Dv,x,y,ierr,herr)
c     subroutine DQFL2 (d,q,z,kq,t,p,Dl,Dv,x,y,ierr,herr)
c
c     subroutine TBFLSH (t,b,z,kr,ab,p,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
c    &                   ierr,herr)
c     subroutine DBFLSH (D,b,z,ab,t,p,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
c    &                   ierr,herr)
c     subroutine ABFL1 (a,b,x,kph,ab,dmin,dmax,t,p,D,ierr,herr)
c     subroutine DBFL1 (d,b,x,ab,t,p,ierr,herr)
c     subroutine ABFL2 (a,b,z,kq,ksat,ab,
c    &                  tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
c    &                  t,p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine DBFL2 (d,b,z,kq,ab,t,p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine PTANC (t,p,q,b,bt,Dl,Dv)
c     subroutine CSTAR (t,p,v,x,cs,ts,Ds,ps,ws,ierr,herr)
c
c  these routines use the following common blocks from other files
c     common /Gcnst/ R,tz(n0:nx),rhoz(n0:nx)
c     common /HCHAR/ htab,hnull
c     common /FLAGS/ xnota,x2ph,xsubc,xsuph,xsupc,xinf,xerr,xnotd,xnotc
c
c  various arrays are dimensioned with parameter statements
c
c ======================================================================
c ======================================================================
c
      subroutine THFLSH (t,h,z,kr,p,D,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
c
c  flash calculation given temperature, bulk enthalpy, and bulk composition.
c  Often in the liquid, two solutions exist, one of them in the two phase.
c  If this is the case, call THFLSH with kr=2 to get the single-phase state.
c
c  inputs:
c        t--temperature [K]
c        h--overall (bulk) enthalpy [J/mol]
c        z--composition [array of mol frac]
c       kr--flag specifying desired root for multi-valued inputs:
c           1 = return lower density root
c           2 = return higher density root
c
c  outputs:
c        p--pressure [kPa]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see TBFLSH for others)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  03-26-98  MM, original version
c  09-25-98  MM, move kguess=0 outside if,then,else
c  01-11-00 EWL, remove ierr from line 1002 and several other places in flash2.for
c  07-07-00 EWL, check for other locations of hmax
c  01-18-01 EWL, remove code and call generic TBFLSH
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call TBFLSH (t,h,z,kr,'TH',p,D,Dl,Dv,x,y,q,e,hh,s,cv,cp,w,
     &             ierr,herr)
      RETURN
      end                                             !subroutine THFLSH
c
c ======================================================================
c
      subroutine THFL1 (t,h,x,Dmin,Dmax,D,ierr,herr)
c
c  flash calculation given temperature, enthalpy, and composition.  This
c  routine accepts only single-phase inputs, it is intended primarily for
c  use with the more general flash routine THFLSH.
c
c  inputs:
c        t--temperature [K]
c        h--enthalpy [J/mol]
c        x--composition [array of mol frac]
c     Dmin--lower bound on density [mol/L]
c     Dmax--upper bound on density [mol/L]
c
c  outputs:
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (t,h,x,0,'TH',dmin,dmax,tt,p,D,ierr,herr)
      RETURN
      end                                              !subroutine THFL1
c
c ======================================================================
c
      subroutine THFL2 (t,h,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given temperature, bulk enthalpy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general temperature-enthalpy flash routine
c  THFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than THFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        t--temperature [K]
c        h--overall (bulk) molar enthalpy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     pbub--bubble point pressure [kPa] at (t,x=z)
c     pdew--dew point pressure [kPa] at (t,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: THFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (t,h,z,1,ksat,'TH',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 tt,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine THFL2
c
c ======================================================================
c
      subroutine TSFLSH (t,s,z,kr,p,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
c
c  flash calculation given temperature, bulk entropy, and bulk composition
c
c  inputs:
c        t--temperature [K]
c        s--overall (bulk) entropy [J/mol-K]
c        z--composition [array of mol frac]
c       kr--flag specifying desired root for multi-valued inputs:
c           1 = return lower density root
c           2 = return higher density root
c
c  outputs:
c        p--pressure [kPa]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see TBFLSH for others)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  01-18-01 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TSFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call TBFLSH (t,s,z,kr,'TS',p,D,Dl,Dv,x,y,q,e,h,ss,cv,cp,w,
     &             ierr,herr)
      RETURN
      end                                             !subroutine TSFLSH
c
c ======================================================================
c
      subroutine TSFL1 (t,s,x,Dmin,Dmax,D,ierr,herr)
c
c  flash calculation given temperature, entropy, and composition.  This
c  routine accepts only single-phase inputs, it is intended primarily for
c  use with the more general flash routine TSFLSH.
c
c  inputs:
c        t--temperature [K]
c        s--entropy [J/mol-K]
c        x--composition [array of mol frac]
c     Dmin--lower bound on density [mol/L]
c     Dmax--upper bound on density [mol/L]
c
c  outputs:
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TSFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (t,s,x,0,'TS',dmin,dmax,tt,p,D,ierr,herr)
      RETURN
      end                                              !subroutine TSFL1
c
c ======================================================================
c
      subroutine TSFL2 (t,s,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given temperature, bulk entropy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general temperature-entropy flash routine
c  TSFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than TSFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        t--temperature [K]
c        h--overall (bulk) molar enthalpy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     pbub--bubble point pressure [kPa] at (t,x=z)
c     pdew--dew point pressure [kPa] at (t,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TSFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (t,s,z,1,ksat,'TS',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 tt,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine TSFL2
c
c ======================================================================
c
      subroutine TEFLSH (t,e,z,kr,p,D,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given temperature, bulk energy, and bulk composition
c
c  inputs:
c        t--temperature [K]
c        e--overall (bulk) internal energy [J/mol]
c        z--composition [array of mol frac]
c       kr--flag specifying desired root for multi-valued inputs:
c           1 = return lower density root
c           2 = return higher density root
c
c  outputs:
c        p--pressure [kPa]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see TBFLSH for others)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  01-18-01 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TEFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call TBFLSH (t,e,z,kr,'TE',p,D,Dl,Dv,x,y,q,ee,h,s,cv,cp,w,
     &             ierr,herr)
      RETURN
      end                                             !subroutine TEFLSH
c
c ======================================================================
c
      subroutine TEFL1 (t,e,x,Dmin,Dmax,D,ierr,herr)
c
c  flash calculation given temperature, energy, and composition.  This
c  routine accepts only single-phase inputs, it is intended primarily for
c  use with the more general flash routine TEFLSH.
c
c  inputs:
c        t--temperature [K]
c        e--energy [J/mol]
c        x--composition [array of mol frac]
c     Dmin--lower bound on density [mol/L]
c     Dmax--upper bound on density [mol/L]
c
c  outputs:
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TEFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (t,e,x,0,'TE',dmin,dmax,tt,p,D,ierr,herr)
      RETURN
      end                                              !subroutine TEFL1
c
c ======================================================================
c
      subroutine TEFL2 (t,e,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given temperature, bulk energy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general temperature-energy flash routine
c  TEFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than TEFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        t--temperature [K]
c        e--overall (bulk) molar energy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     pbub--bubble point pressure [kPa] at (t,x=z)
c     pdew--dew point pressure [kPa] at (t,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TEFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (t,e,z,1,ksat,'TE',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 tt,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine TEFL2
c
c ======================================================================
c
      subroutine DHFLSH (D,h,z,t,p,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
c
c  flash calculation given density, enthalpy, and bulk composition
c
c  This routine accepts both single-phase and two-phase states as the
c  input; for single-phase calculations, the subroutine DHFL1 is faster.
c
c  inputs:
c        D--overall (bulk) molar density [mol/L]
c        h--overall (bulk) enthalpy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                      210 = CRITP did not converge
c                      248 = iteration did not converge
c                      249 = H out of range
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  05-13-99 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DHFLSH
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFLSH (D,h,z,'DH',t,p,Dl,Dv,x,y,q,e,hh,s,cv,cp,w,ierr,herr)
      RETURN
      end                                             !subroutine DHFLSH
c
c ======================================================================
c
      subroutine DHFL1 (rho,h,x,t,ierr,herr)
c
c  iterate for single-phase temperature as a function of density, enthalpy,
c  and composition
c
c  inputs:
c      rho--molar density [mol/L]
c        h--enthalpy [J/mol]
c        x--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DHFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (rho,h,x,0,'DH',0.d0,0.d0,t,pp,dd,ierr,herr)
      RETURN
      end                                              !subroutine DHFL1
c
c ======================================================================
c
      subroutine DHFL2 (d,h,z,t,p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given bulk density, enthalpy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general density-enthalpy flash routine
c  DHFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than DHFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        d--overall (bulk) molar density [mol/L]
c        h--overall (bulk) molar enthalpy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DHFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFL2 (d,h,z,0,'DH',
     &                 t,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine DHFL2
c
c ======================================================================
c
      subroutine DSFLSH (D,s,z,t,p,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
c
c  flash calculation given density, entropy, and bulk composition
c
c  This routine accepts both single-phase and two-phase states as the
c  input; for single-phase calculations, the subroutine DSFL1 is faster.
c
c  inputs:
c        D--overall (bulk) molar density [mol/L]
c        s--overall (bulk) entropy [J/mol-K]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                      210 = CRITP did not converge
c                      248 = iteration did not converge
c                      249 = H out of range
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-19-01 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DSFLSH
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFLSH (D,s,z,'DS',t,p,Dl,Dv,x,y,q,e,h,ss,cv,cp,w,ierr,herr)
      RETURN
      end                                             !subroutine DSFLSH
c
c ======================================================================
c
      subroutine DSFL1 (rho,s,x,t,ierr,herr)
c
c  iterate for single-phase temperature as a function of density, entropy,
c  and composition
c
c  inputs:
c      rho--molar density [mol/L]
c        s--entropy [J/mol-K]
c        x--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (rho,s,x,0,'DS',0.d0,0.d0,t,pp,dd,ierr,herr)
      RETURN
      end                                              !subroutine DSFL1
c
c ======================================================================
c
      subroutine DSFL2 (d,s,z,t,p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given bulk density, entropy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general density-entropy flash routine
c  DSFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than DSFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        d--overall (bulk) molar density [mol/L]
c        s--overall (bulk) molar entropy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DSFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFL2 (d,s,z,0,'DS',
     &                 t,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine DSFL2
c
c ======================================================================
c
      subroutine DEFLSH (D,e,z,t,p,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given density, energy, and bulk composition
c
c  This routine accepts both single-phase and two-phase states as the
c  input; for single-phase calculations, the subroutine DHFL1 is faster.
c
c  inputs:
c        D--overall (bulk) molar density [mol/L]
c        e--overall (bulk) internal energy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                      210 = CRITP did not converge
c                      248 = iteration did not converge
c                      249 = H out of range
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-19-01 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DEFLSH
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFLSH (D,e,z,'DE',t,p,Dl,Dv,x,y,q,ee,h,s,cv,cp,w,ierr,herr)
      RETURN
      end                                             !subroutine DEFLSH
c
c ======================================================================
c
      subroutine DEFL1 (rho,e,x,t,ierr,herr)
c
c  iterate for single-phase temperature as a function of density, energy,
c  and composition
c
c  inputs:
c      rho--molar density [mol/L]
c        e--energy [J/mol]
c        x--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DEFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (rho,e,x,0,'DE',0.d0,0.d0,t,pp,dd,ierr,herr)
      RETURN
      end                                              !subroutine DEFL1
c
c ======================================================================
c
      subroutine DEFL2 (d,e,z,t,p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given bulk density, energy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general density-energy flash routine
c  DEFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than DEFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        d--overall (bulk) molar density [mol/L]
c        e--overall (bulk) molar energy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DEFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFL2 (d,e,z,0,'DE',
     &                 t,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine DEFL2
c
c ======================================================================
c
      subroutine HSFLSH (h,s,z,t,p,D,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
c
c  flash calculation given bulk enthalpy, entropy, and composition
c
c  inputs:
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c        z--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see TBFLSH for others)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  12-17-01 EWL, original version
c  05-08-04 OBD, (Conecpts NREC) add routine to determine 2-phase states
c  10-20-04 EWL, check for t1=0 before call to ENTRO
c  03-12-06 EWL, fix bug with e not being returned
c  03-07-11 EWL, initialize s1 and s2
c  05-16-11 EWL, add check for ierr>0 for liquid phase when T>Tsat and T<Tsat*1.01
c  03-01-12 EWL, after the call to SATH, change dmin from 0 to 1.d-4 so that the calculated entropy at dmin won't be so large
c  11-21-12 EWL, specify better values of q after call to HSFL1
c
cx DEC$ ATTRIBUTES DLLEXPORT :: HSFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
      call SATH (h,z,0,nroot,k1,t1,p1,d1,k2,t2,p2,d2,ierr,herr)
      dmin=1.d-4
      call LIMITS ('EOS',z,tmin,tmax,Dmax,pmax)
      call CRITP (z,tc,pc,rhoc,ierr,herr)
      if (nroot.eq.0) then
        q=999
        t=tc*1.1d0
        d=rhoc
      else
        s1=0
        s2=0
        if (t1.gt.0.d0) call ENTRO (t1,d1,z,s1)
        if (d2.gt.0.d0) call ENTRO (t2,d2,z,s2)
        if (s.lt.s1) then
          dmin=d1
          t=t1*1.01d0
          d=d1*1.01d0
          q=-998
        elseif (s.gt.s2 .and. nroot.eq.2) then
          dmax=d2
          t=t2
          d=d2/1.1d0
          q=998
        else
          p=p1
          do i=1,25
            call PSFLSH (p,s,z,t,D,Dl,Dv,x,y,q,ee,hh,cv,cp,w,ierr,herr)
            dh=hh-h
            if (abs(dh).le.1.d-5) goto 100
            p=max(0.2d0*(p+1d-3),p-dh*D)
          enddo
 100      continue
          if (ieflg.eq.0) e=ee
          if (ieflg.eq.1) e=hh
          if (ierr.eq.0 .and. i.lt.25) RETURN
          t=tc
          d=rhoc
          Dl=d
          Dv=d
          call THERM (t,D,z,p,e,hh,ss,cv,cp,w,hjt)
          ierr=200
          write (herr,1200) hnull
 1200     format ('[HSFLSH error 200] ',
     &          'h-s inputs are two-phase or out of bounds, iterative',
     &          ' routine is not available to find a solution.',a1)
          call ERRMSG (ierr,herr)
          RETURN
        endif
      endif
      call HSFL1 (h,s,z,Dmin,Dmax,t,D,ierr,herr)
      if (ierr.gt.0 .and. nroot.ne.0) then
        if (s.lt.s1) then
          dmin=d1
          t=t1
          d=d1
          q=-998
          call HSFL1 (h,s,z,Dmin,Dmax,t,D,ierr,herr)
        endif
      endif
      call THERM (t,D,z,p,e,hh,ss,cv,cp,w,hjt)
      if (ieflg.eq.1) e=hh
      Dl=d
      Dv=d
      if (p.gt.pc .and. t.gt.tc) then
        q=999.d0     !quality undefined
      elseif (p.gt.pc) then
        q=-998.d0    !subcooled
      elseif (t.gt.tc) then
        q=998.d0     !superheated
      elseif (d.gt.rhoc) then
        q=-998.d0    !subcooled
      elseif (d.lt.rhoc) then
        q=998.d0     !superheated
      end if
      if (ierr.eq.0)
     &   call LIMITX ('EOS',t,D,p,z,tmin,tmax,rhomax,pmax,ierr,herr)
      RETURN
      end                                             !subroutine HSFLSH
c
c ======================================================================
c
      subroutine HSFL1 (h,s,x,Dmin,Dmax,t,D,ierr,herr)
c
c  flash calculation given enthalpy, entropy, and composition.  This
c  routine accepts only single-phase inputs, it is intended primarily for
c  use with the more general flash routine HSFLSH.
c
c  inputs:
c        h--enthalpy [J/mol-K]
c        s--entropy [J/mol-K]
c        x--composition [array of mol frac]
c     Dmin--lower bound on density [mol/L]
c     Dmax--upper bound on density [mol/L]
c
c  outputs:
c        t--temperature [K]
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: HSFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      if (ieflg.ne.1) then
        call ABFL1 (s,h,x,0,'SH',dmin,dmax,t,p,D,ierr,herr)
      else
        call ABFL1 (s,h,x,0,'SE',dmin,dmax,t,p,D,ierr,herr)
      endif
      RETURN
      end                                              !subroutine HSFL1
c
c ======================================================================
c
      subroutine ESFLSH (e,s,z,t,p,D,Dl,Dv,x,y,q,h,cv,cp,w,ierr,herr)
c
c  flash calculation given bulk energy, entropy, and composition
c
c  inputs:
c        e--overall (bulk) energy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c        z--composition [array of mol frac]
c
c  outputs:
c        see HSFLSH for list of outputs
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  05-16-05 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ESFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      ieflg=1
      call HSFLSH (e,s,z,t,p,D,Dl,Dv,x,y,q,h,cv,cp,w,ierr,herr)
      ieflg=0
      RETURN
      end                                             !subroutine ESFLSH
c
c ======================================================================
c
      subroutine TQFLSH (t,q,z,kq,p,D,Dl,Dv,x,y,e,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given temperature, quality, and bulk composition
c
c  This routine accepts saturation or two-phase states as inputs.
c
c  inputs:
c        t--temperature [K]
c        q--vapor quality [basis specified by kq]
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q < 0 or q > 1 are not allowed and will result in warning
c        z--overall (bulk) composition [array of mol frac]
c       kq--flag specifying units for input quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c
c  outputs:
c        p--pressure [kPa]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, -9.992d6 is returned
c     ierr--error flag:   0 = successful
c                         1 = T < Tmin
c                         8 = x out of range
c                         9 = T and x out of range
c                       270 = CRITP did not converge
c                       271 = T > Tcrit
c                       275 = q out of range
c                       278 = TQFLSH did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  04-08-96  MM, original version, based on TPFLSH
c  05-16-97  MM, get special-case flags from /FLAGS/
c  07-14-97  MM, add check for q out of range; add call to LIMITS
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-05-97  MM, change warning -279 to error 279
c  09-25-98  MM, define kguess prior to call to TPRHO
c  09-02-99  MM, add check for x(i) = 1
c  05-08-00  MM, set alpha=1-qmole in mass conversion
c  12-16-02 EWL, add checks for pseudo-pure fluid, calculate p using q
c  01-20-10 EWL, initialize x and y
c  05-06-10 EWL, allow T>Tc for mixtures
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TQFLSH
c
      include 'commons.for'
      character*255 herr,herr2
      dimension z(ncmax),x(ncmax),y(ncmax)
      dimension xdew(ncmax),ybub(ncmax)
c
      ierr=0
      herr=' '
      p=0.d0
      D=0.d0
      Dl=0.d0
      Dv=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c
c  initialize x and y for nc=1
c
c  check that input conditions (in this case t and z) are within limits
c
      Ddum=0.0d0
      pdum=0.0d0
      call LIMITX ('EOS',t,Ddum,pdum,z,tmin,tmax,rhomax,pmax,ierr,herr2)
      if (ierr.gt.0) then
c  T and/or z are out of bounds, set error flag and return
        write (herr,1110) ierr,herr2(1:236),hnull
 1110   format ('[TQFLSH error',i3,'] ',a236,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      call CRITP (z,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
        ierr=270
        write (herr,1111) herr2(1:235),hnull
 1111   format ('[TQFLSH error 270] ',a235,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      if ((t.ge.tc .and. nc.eq.1) .or. t.gt.tc*1.5d0) then
c  supercritical state
        ierr=271
        write (herr,1271) t,tc,hnull
 1271   format ('[TQFLSH error 271] ',
     &          'temperature input to temperature-quality flash is ',
     &          'greater than critical temperature; T =',g12.5,
     &          ' K, Tcrit =',g12.5,' K.',a1)
        call ERRMSG (ierr,herr)
        RETURN
      else if (ABS(q).le.1.0d-8) then
c  saturated liquid
        call SATT (t,z,1,p,Dl,Dv,x,y,ierr,herr2)
        if (ianc(icomp).eq.1) call SATT (t,z,2,p,D,Dv,x,y,ierr,herr2)
        if (ierr.ne.0) then
          herr='[TQFLSH error] (sat liquid):  '//herr2
          call ERRMSG (ierr,herr)
          RETURN
        end if
        D=Dl
        call THERM (t,Dl,x,p,e,h,s,cv,cp,w,hjt)
      else if (ABS(1.0d0-q).le.1.0d-8) then
c  saturated vapor
        call SATT (t,z,2,p,Dl,Dv,x,y,ierr,herr2)
        if (ianc(icomp).eq.1) call SATT (t,z,1,p,Dl,D,x,y,ierr,herr2)
        if (ierr.ne.0) then
          herr='[TQFLSH error] (sat vapor):  '//herr2
          call ERRMSG (ierr,herr)
          RETURN
        end if
        D=Dv
        call THERM (t,Dv,y,p,e,h,s,cv,cp,w,hjt)
      else if (q.lt.0.0d0 .or. q.gt.1.0d0) then
c  quality out of range
        ierr=275
        write (herr,1275) q,hnull
 1275   format ('[TQFLSH error 275] ',
     &          'input quality is out of range; q =',g12.5,
     &          '; quality must be between 0 and 1 ',a1)
        call ERRMSG (ierr,herr)
        RETURN
      else
c  two-phase state
c  check if any component has mole fraction 1--if so it is pure
c  component (even if nc <> 1); [mixture 2-phase iteration will
c  not converge if x(i) = 1]
        lpure=.false.
        do i=1,nc
          if (ABS(z(i)-1.0d0).lt.1.0d-8) lpure=.true.
        enddo
        if (icomp.ne.0) lpure=.true.
        if (lpure .or. ianc(icomp).eq.1) then
c  special case--two-phase state for a pure fluid
          call SATT (t,z,1,p,Dl,Dv,x,ybub,ierr,herr2)
          pl=p
          pv=p
          if (ierr.ne.0) then
            herr='[TQFLSH error] (2-phase):  '//herr2
            call ERRMSG (ierr,herr)
            RETURN
          end if
        if (ianc(icomp).eq.1)call SATT (t,z,2,pv,D,Dv,xdew,y,ierr,herr2)
          call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
          call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
          cp=x2ph                 !Cp, w not defined for 2-phase
          w=x2ph
          cv=x2ph
c  bulk properties are weighted average of liquid and vapor phases
c  note that for a pure fluid quality on mass and molar basis is same
          alpha=1.0d0-q               !alpha is liq fraction,
          D=1.0d0/(alpha/Dl+q/Dv)     !q is vapor frac
          p=alpha*pl+q*pv
          e=alpha*el+q*ev
          h=alpha*hl+q*hv
          s=alpha*sl+q*sv
        else
c
c  general 2-phase mixture state
c  generate initial guesses for t, x, y by interpolating sat liq & vap
c
          call SATT (t,z,1,pbub,Dlbub,Dvbub,x,ybub,ierr,herr2)
          call SATT (t,z,2,pdew,Dldew,Dvdew,xdew,y,ierr,herr)
          if (ierr.ne.0) then
            herr='[TQFLSH error] (2-phase):  '//herr2
            call ERRMSG (ierr,herr)
            RETURN
          end if
          xsum=0.0d0
          ysum=0.0d0        !sums for normalization of compositions
          do i=1,nc
            x(i)=(1.0d0-q)*z(i)+q*xdew(i)
            y(i)=q*z(i)+(1.0d0-q)*ybub(i)
            xsum=xsum+x(i)
            ysum=ysum+y(i)
          enddo
          do i=1,nc
            x(i)=x(i)/xsum
            y(i)=y(i)/ysum
          enddo
          p=(1.0d0-q)*pbub+q*pdew  !initial guess for pressure
          Dl=Dlbub                 !initial guess for liquid density
          Dv=Dvdew                 !initial guess for vapor density
          ksat=0
          call TQFL2 (t,q,z,kq,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                p,Dl,Dv,x,y,ierr,herr)
c         write (*,*) 'TQFLSH--return from TQFL2'
c  compute 2-phase properties and load output variables
c  call TPRHO to ensure consistency of t, p, rho
          kguess=1   !use above as initial guess
          call TPRHO (t,p,x,1,kguess,Dl,ierr,herr)
          call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
          call TPRHO (t,p,y,2,kguess,Dv,ierr,herr)
          call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
c         ierr=279
c         herr='[TQFLSH error 279] temperature-quality flash '//
c    &         'calculations are not implemented for mixtures'//hnull
          cp=x2ph              !Cp, w not defined for 2-phase
          w=x2ph
          cv=x2ph
c  bulk properties are weighted average of liquid and vapor phases
          if (kq.eq.1) then
            qmole=q                 !qmole is vapor frac [molar basis]
            alpha=1.0d0-q           !alpha is liq fraction
          else
c  convert mass quality to molar quality
            wmliq=WMOL(x)
            wmvap=WMOL(y)
            qmole=q/wmvap/(q/wmvap+(1.0d0-q)/wmliq)
            alpha=1.0d0-qmole
          end if
          D=1.0d0/(alpha/Dl+qmole/Dv)
          e=alpha*el+qmole*ev
          h=alpha*hl+qmole*hv
          s=alpha*sl+qmole*sv
        end if
      end if
c
      RETURN
      end                                             !subroutine TQFLSH
c
c ======================================================================
c
      subroutine TQFL2 (t,q,z,kq,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  p,Dl,Dv,x,y,ierr,herr)
c
c  flash calculation given temperature, quality, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general temperature-quality flash routine
c  TQFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than TQFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        t--temperature [K]
c        q--vapor quality [basis specified by kq]
c        z--overall (bulk) composition [array of mol frac]
c       kq--flag specifying units for input quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     pbub--bubble point pressure [kPa] at (t,x=z)
c     pdew--dew point pressure [kPa] at (t,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TQFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (t,q,z,kq,ksat,'TQ',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 tt,p,Dl,Dv,x,y,qq,ierr,herr)
c
      RETURN
      end                                              !subroutine TQFL2
c
c ======================================================================
c
      subroutine PQFLSH (p,q,z,kq,t,D,Dl,Dv,x,y,e,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given pressure, quality, and bulk composition
c
c  This routine accepts saturation or two-phase states as inputs.
c
c  inputs:
c        p--pressure [kPa]
c        q--vapor quality [basis specified by kq]
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q < 0 or q > 1 are not allowed and will result in warning
c        z--overall (bulk) composition [array of mol frac]
c       kq--flag specifying units for input quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c
c  outputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, -9.992d6 is returned
c     ierr--error flag:   0 = successful
c                         4 = P < 0
c                         8 = x out of range
c                         9 = P and x out of range
c                       290 = CRITP did not converge
c                       291 = P > Pcrit
c                       295 = q out of range
c                       298 = PQFLSH did not converge
c                      -299 = 2-phase for mix not implemented yet
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  04-08-96  MM, original version, based on TQFLSH
c  04-23-96  MM, alpha = 1 - qmole, not 1 - q when q input as mass
c  05-16-97  MM, get special-case flags from /FLAGS/
c  07-14-97  MM, add check for q out of range; add call to LIMITS
c  10-01-97  MM, add compiler switch to allow access by DLL
c  12-05-97  MM, change warning -299 to error 299
c  09-25-98  MM, define kguess prior to call to TPRHO
c  09-02-99  MM, add check for x(i) = 1
c  12-16-02 EWL, add checks for pseudo-pure fluid, calculate p using q
c  01-20-10 EWL, initialize x and y
c  05-06-10 EWL, allow p>pc*1.5 for mixtures
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PQFLSH
c
      include 'commons.for'
      character*255 herr,herr2
      dimension z(ncmax),x(ncmax),y(ncmax)
      dimension xdew(ncmax),ybub(ncmax)
c
      ierr=0
      herr=' '
      D=0.d0
      Dl=0.d0
      Dv=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c
c  initialize x and y for nc=1
c
      call CRITP (z,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
        ierr=290
        t=300.0d0
        write (herr,1108) herr2(1:235),hnull
 1108   format ('[PQFLSH error 290] ',a235,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
c  check that input conditions (in this case p and z) are within limits
c
      Ddum=0.0d0
      tdum=0.8d0*tc
      call LIMITX ('EOS',tdum,Ddum,p,z,tmin,tmax,rhomax,pmax,ierr,herr2)
      if (ierr.gt.0) then
c  p and/or x are out of bounds, set error flag and return
c  set output temperature to "reasonable" value to avoid GUI crash
        t=0.8d0*tc
        write (herr,1110) ierr,herr2(1:236),hnull
 1110   format ('[PQFLSH error',i3,'] ',a236,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      if ((p.ge.pc .and. nc.eq.1) .or. p.gt.pc*1.5d0) then
c  supercritical state
c  set output temperature to critical value to avoid GUI crash
        t=tc
        ierr=291
        write (herr,1291) p/1000.0d0,pc/1000.0d0,hnull
 1291   format ('[PQFLSH error 291] ',
     &          'pressure input to pressure-quality flash is ',
     &          'greater than critical pressure; P =',g12.5,
     &          ' MPa, Pcrit =',g12.5,' MPa.',a1)
        call ERRMSG (ierr,herr)
        RETURN
      else if (ABS(q).le.1.0d-8 .and. ianc(icomp).eq.0) then
c  saturated liquid
        call SATP (p,z,1,t,Dl,Dv,x,y,ierr,herr2)
        if (ierr.ne.0) then
          herr='[PQFLSH error] (sat liquid):  '//herr2
          call ERRMSG (ierr,herr)
          RETURN
        end if
        D=Dl
        call THERM (t,Dl,x,ptherm,e,h,s,cv,cp,w,hjt)
      else if (ABS(1.0d0-q).le.1.0d-8 .and. ianc(icomp).eq.0) then
c  saturated vapor
        call SATP (p,z,2,t,Dl,Dv,x,y,ierr,herr2)
        if (ierr.ne.0) then
          herr='[PQFLSH error] (sat vapor):  '//herr2
          call ERRMSG (ierr,herr)
          RETURN
        end if
        D=Dv
        call THERM (t,Dv,y,ptherm,e,h,s,cv,cp,w,hjt)
      else if (q.lt.0.0d0 .or. q.gt.1.0d0) then
c  quality out of range
        ierr=295
        write (herr,1295) q,hnull
 1295   format ('[PQFLSH error 295] ',
     &          'input quality is out of range; q =',g12.5,
     &          '; quality must be between 0 and 1 ',a1)
        call ERRMSG (ierr,herr)
        RETURN
      else
c  two-phase state
c  check if any component has mole fraction 1--if so it is pure
c  component (even if nc <> 1); [mixture 2-phase iteration will
c  not converge if x(i) = 1]
        lpure=.false.
        do i=1,nc
          if (ABS(z(i)-1.0d0).lt.1.0d-8) lpure=.true.
        enddo
        if (icomp.ne.0) lpure=.true.
        if (lpure .or. ianc(icomp).eq.1) then
c  special case--two-phase state for a pure fluid
          call SATP (p,z,1,t,Dl,Dv,x,y,ierr,herr2)
          if (ierr.ne.0) then
            herr='[PQFLSH error] (2-phase):  '//herr2
            call ERRMSG (ierr,herr)
            RETURN
          end if
          if (ianc(icomp).eq.1) then
            tl=t
            call SATP (p,z,2,tv,D,Dv,x,y,ierr,herr2)
            t=(1.d0-q)*tl+q*tv
            if (ianc(icomp).eq.1) call PTANC (t,p,q,d,'Q',Dl,Dv)
          endif
          call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
          call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
          cp=x2ph                 !Cp, w not defined for 2-phase
          w=x2ph
          cv=x2ph
c  bulk properties are weighted average of liquid and vapor phases
c  note that for a pure fluid quality on mass and molar basis is same
          alpha=1.0d0-q               !alpha is liq fraction,
          D=1.0d0/(alpha/Dl+q/Dv)     !q is vapor frac
          e=alpha*el+q*ev
          h=alpha*hl+q*hv
          s=alpha*sl+q*sv
        else
c
          call SATP (p,z,1,tbub,Dlbub,Dvbub,x,ybub,ierr,herr2)
          call SATP (p,z,2,tdew,Dldew,Dvdew,xdew,y,ierr,herr2)
          if (ierr.ne.0) then
            herr='[PQFLSH error] (2-phase):  '//herr2
            call ERRMSG (ierr,herr)
            RETURN
          end if
          ksat=1   !dew & bubble point limits are provided
          call PQFL2 (p,q,z,kq,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                t,Dl,Dv,x,y,ierr,herr)
          call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
          call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
          cp=x2ph                   !Cp, w not defined for 2-phase
          w=x2ph
          cv=x2ph
c  bulk properties are weighted average of liquid and vapor phases
          if (kq.eq.2) then
c  convert mass quality to molar quality
            wmliq=WMOL(x)
            wmvap=WMOL(y)
            qmole=q/wmvap/(q/wmvap+(1.0d0-q)/wmliq)
          else
            qmole=q                 !qmole is vapor frac [molar basis]
          end if
          alpha=1.0d0-qmole         !alpha is liq fraction
          D=1.0d0/(alpha/Dl+qmole/Dv)
          e=alpha*el+qmole*ev
          h=alpha*hl+qmole*hv
          s=alpha*sl+qmole*sv
        end if
      end if
c
      RETURN
      end                                             !subroutine PQFLSH
c
c ======================================================================
c
      subroutine PQFL2 (p,q,z,kq,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,Dl,Dv,x,y,ierr,herr)
c
c  flash calculation given pressure, quality, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general pressure-quality flash routine
c  PQFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than PQFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  This routine calls TPFL2 within a secant-method iteration for
c  pressure to find a solution.  Initial guesses are based on liquid
c  density at the bubble point and vapor density at the dew point.
c
c  inputs:
c        p--pressure [kPa]
c        q--vapor quality [basis specified by kq]
c        z--overall (bulk) composition [array of mol frac]
c       kq--flag specifying units for input quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     tbub--bubble point temperature [K] at (p,x=z)
c     tdew--dew point temperature [K] at (p,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        t--temperature [K]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PQFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (p,q,z,kq,ksat,'PQ',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 t,pp,Dl,Dv,x,y,qq,ierr,herr)
c
      RETURN
      end                                              !subroutine PQFL2
c
c ======================================================================
c
      subroutine DQFL2 (d,q,z,kq,t,p,Dl,Dv,x,y,ierr,herr)
c
c  flash calculation given bulk density, quality, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general density-quality flash routine
c  DQFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits.
c
c  inputs:
c        d--overall (bulk) molar density [mol/L]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c        z--overall (bulk) composition [array of mol frac]
c       kq--flag specifying units for input quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DQFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      call DBFL2 (d,q,z,kq,'DQ',
     &                 t,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine DQFL2
c
c ======================================================================
c
      subroutine TBFLSH (t,b,z,kr,ab,p,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
     &                   ierr,herr)
c
c  flash calculation given temperature, bulk composition, and either
c  enthalpy, entropy, or energy
c
c  inputs:
c        t--temperature [K]
c        b--second property (energy, enthalpy, or entropy)
c        z--composition [array of mol frac]
c       kr--flag specifying desired root for multi-valued inputs:
c           1 = return lower density root
c           2 = return higher density root
c       ab--character*2 string defining the inputs: 'TH', 'TS', or 'TE'
c
c  outputs:
c        p--pressure [kPa]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                        4 = Pin < 0
c                        8 = x out of range (< 0 or > 1)
c                       12 = x out of range and P < 0
c     errors are in 230 range
c                      240 = CRITP did not converge
c                      241 = SATP did not converge at bubble point
c                      242 = SATP did not converge at dew point
c                      243 = SATP (bubble pt) did not converge for 2-ph
c                      244 = SATP (dew pt) did not converge  for 2-phase
c                      245 = TQFL2 did not converge
c                      246 = 2-phase iteration did not converge
c                      247 = TPRHO did not converge for single-phase
c                      248 = single-phase iteration did not converge
c                      249 = H out of range
c     herr--error string (character*255 variable if ierr<>0)
c
c  rewritten by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  01-18-01 EWL, original version, rewritten by combining code of McLinden
c                from the THFLSH routine to make one unified routine
c  12-16-02 EWL, calculate p using q for a pure or pseudo-pure fluid
c  03-23-05 EWL, add check for Dpmax>Dmax
c  03-15-12 EWL, change rho0 from 10^-10 to 10^-4 so that the inital entropy will not be so high (causing root failure)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TBFLSH
c
      include 'commons.for'
      character*1 bt
      character*2 ab
      character*255 herr,herr1
      dimension x(ncmax),y(ncmax),z(ncmax),xdew(ncmax),xbub(ncmax),
     &          ydew(ncmax),ybub(ncmax)
c
      bt=ab(2:2)
      ierr=0
      herr=' '
      p=0.d0
      D=0.d0
      Dl=0.d0
      Dv=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      q=998
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c
c  set output liquid and vapor compositions to input values
c  zero output values for undefined components
c     if (nc.lt.ncmax) then
c       do i=nc+1,ncmax
c         x(i)=0.0d0
c         y(i)=0.0d0
c       enddo
c     end if
c
      call CRITP (z,tc,pc,Dc,ierr,herr1)
      if (ierr.gt.0) then
        ierr=230
        write (herr,1002) ab,herr1(1:234),hnull
 1002   format ('[',a2,'FLSH error 230] ',a234,a1)
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
        RETURN
      end if
c
c  check that input conditions are within limits, get max D,p
c
      pdum=0.0d0
      Ddum=0.0d0
      call LIMITX ('EOS',t,Ddum,pdum,z,tmin,tmax,Dmax,pmax,ierr,herr1)
c  maximum value may be in several locations
      rho0=1.0d-4
      if (bt.eq.'H') then
        call ENTHAL (t,rho0,z,bmax)
        call ENTHAL (t,Dmax,z,bmax1)
      elseif (bt.eq.'E') then
        call ENERGY (t,rho0,z,bmax)
        call ENERGY (t,Dmax,z,bmax1)
      elseif (bt.eq.'S') then
        call ENTRO (t,rho0,z,bmax)
        call ENTRO (t,Dmax,z,bmax1)
      endif
      if (bmax1.gt.bmax) bmax=bmax1
      if (ierr.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ab,ierr,herr1(1:233),hnull
 1006   format ('[',a2,'FLSH warning',i4,'] ',a233,a1)
        call ERRMSG (ierr,herr)
      else
c  if inputs are outside limits set outputs and return (x = y = z)
        if (ierr.ge.1) then
          write (herr,1007) ab,ierr,herr1(1:235),hnull
 1007     format ('[',a2,'FLSH error',i4,'] ',a235,a1)
        else if (b.gt.bmax) then
          ierr=239
          write (herr,1008) ab,ierr,bt,b,bt,bmax,hnull
 1008     format ('[',a2,'FLSH error',i4,'] Input value is ',
     &            'outside limits; ',a1,' =',g12.5,', ',a1,'max =',
     &            g12.5,a1)
        end if
        if (ierr.ge.1) then
          call ERRMSG (ierr,herr)
          D=rho0
          Dl=rho0
          Dv=rho0
          p=R*t*rho0
          q=xerr
          e=xerr
          h=xerr
          s=xerr
          cv=xerr
          cp=xerr
          w=xerr
          RETURN
        end if
      end if
c
      kguess=0    !no initial guess provided
      if (t.ge.tc) then
c
c  super-critical state (must be single-phase)
c
c  find density and second input at t, p = pmax
        kph=1       !find liquid solution
        call TPRHO (t,pmax,z,kph,kguess,Dpmax,ierr,herr)
c  bpmax is b at pmax; it is close to minimum density
        if (bt.eq.'H') call ENTHAL (t,Dpmax,z,bpmax)
        if (bt.eq.'E') call ENERGY (t,Dpmax,z,bpmax)
        if (bt.eq.'S') call ENTRO (t,Dpmax,z,bpmax)
c       write (*,*) ' TBFLSH--b at rhomax:  ',bpmax,Dpmax
        if (b.lt.bpmax) then
c  two roots exist--find actual minimum b to bound solution
c  use general golden-section search routine (in utility.f file)
          neval=29   !reduce interval to 10**-6 of original
          call GOLD (rho0,Dpmax,neval,.false.,z,t,z3,z4,bt,
     &               Dbmin,bmin,ijunk)
c         write (*,*) ' TBFLSH--bmin at rho: ',bmin,Dbmin
c  check that input h is within bounds; if out of bounds, set outputs
c  and return
          if (b.lt.bmin) then
            ierr=239
            write (herr,1010) ab,ierr,bt,b,bt,bmin,hnull
 1010       format ('[',a2,'FLSH error',i4,'] Input value is ',
     &              'outside limits; ',a1,' =',g12.5,', ',a1,'min =',
     &              g12.5,a1)
            call ERRMSG (ierr,herr)
            D=rho0
            Dl=rho0
            Dv=rho0
            p=R*t*rho0
            q=xerr
            e=xerr
            h=xerr
            s=xerr
            cv=xerr
            cp=xerr
            w=xerr
            RETURN
          end if
c  issue warning that two roots exist
          ierr=-238
          if (kr.eq.2) then
c  set bounds to find higher-density root
            write (herr,1238) ab,hnull
 1238       format ('[',a2,'FLSH warning 238] two solutions exist for ',
     &           'the input values; the higher density root has been ',
     &           'calculated',a1)
            Dmin=Dbmin
            Dmax=Dpmax
          else
c  find lower-density root
            write (herr,1239) ab,hnull
 1239       format ('[',a2,'FLSH warning 238] two solutions exist for ',
     &           'the input values; the lower density root has been ',
     &           'calculated',a1)
            Dmin=rho0
            Dmax=Dbmin
          end if
        else
c  only a single root exists
          Dmin=rho0
          Dmax=Dpmax
        end if
        call ABFL1 (t,b,z,0,ab,Dmin,Dmax,tt,pp,D,ierr,herr)
c  set remaining outputs (x = y = z as initialized above)
        Dl=D
        Dv=D
        call THERM (t,D,z,p,e,h,s,cv,cp,w,hjt)
        q=998.d0                  !superheated
        if (p.gt.pc) q=999.d0     !quality undefined
c
      else
c
c  sub-critical state (possibility of solution in two-phase state)
c
c  first check if input h > h at dew point
        kph=2
        call SATT (t,z,kph,pdew,Dldew,Dvdew,xdew,ydew,ierr1,herr1)
        if (bt.eq.'H') call ENTHAL (t,Dvdew,z,bdew)
        if (bt.eq.'E') call ENERGY (t,Dvdew,z,bdew)
        if (bt.eq.'S') call ENTRO (t,Dvdew,z,bdew)
        if (b.ge.bdew) then
c  solution is single-phase vapor
          Dmin=rho0
          Dmax=Dvdew
          call ABFL1 (t,b,z,0,ab,Dmin,Dmax,tt,pp,D,ierr1,herr1)
          if (ierr1.ne.0) then
            ierr=ierr1
            herr=herr1
          end if
c  set remaining outputs (x = y = z as initialized above)
          Dl=D
          Dv=D
          q=998.0d0
          call THERM (t,D,z,p,e,h,s,cv,cp,w,hjt)
c
        else
c  solution is either compressed liquid or two-phase
          call TPRHO (t,pmax,z,kph,kguess,Dpmax,ierr,herr)
          if (ierr.eq.202 .and. Dpmax.gt.Dmax) Dpmax=Dmax
c  bpmax is b at pmax; it is close to minimum density
          if (bt.eq.'H') call ENTHAL (t,Dpmax,z,bpmax)
          if (bt.eq.'E') call ENERGY (t,Dpmax,z,bpmax)
          if (bt.eq.'S') call ENTRO (t,Dpmax,z,bpmax)
c         write (*,*) ' TBFLSH--b at rhomax:  ',bpmax,Dpmax
c  check for possible minimum in h in compressed liquid region
          if (icomp.ne.0 .and. ianc(icomp).eq.0) then
c  for pure component Dlbub = Dldew
            Dlbub=Dldew
            pbub=pdew
          else
c  find bubble point density, etc.
            kph=1
            call SATT (t,z,kph,pbub,Dlbub,Dvbub,xbub,ybub,ierr,herr)
          end if
          if (bt.eq.'H') call ENTHAL (t,Dlbub,z,bbub)
          if (bt.eq.'E') call ENERGY (t,Dlbub,z,bbub)
          if (bt.eq.'S') call ENTRO (t,Dlbub,z,bbub)
c         write (*,*) ' TBFLSH--bbub,bdew:  ',bbub,bdew
c  check slope of b vs. rho
          Dlbub1=Dlbub+1.0d-6
          if (bt.eq.'H') call ENTHAL (t,Dlbub1,z,bbub1)
          if (bt.eq.'E') call ENERGY (t,Dlbub1,z,bbub1)
          if (bt.eq.'S') call ENTRO (t,Dlbub1,z,bbub1)
          if (bbub1.lt.bbub) then
c  minimum exists in compressed liquid region, otherwise bmin is at bbub
c  use general golden-section search routine (in utility.f file)
c           write (*,*) ' TBFLSH--minimum in compressed liquid'
            neval=29   !reduce interval to 10**-6 of original
            call GOLD (Dlbub,Dpmax,neval,.false.,z,t,z3,z4,bt,
     &                 Dbmin,bmin,ijunk)
c           write (*,*) ' TBFLSH--bmin at rho: ',bmin,Dbmin
          else
            bmin=bbub
            Dbmin=Dlbub
          end if
c  check that input b is within bounds; if out of bounds, set outputs
c  and return
          if (b.lt.bmin) then
            ierr=239
            write (herr,1010) ab,ierr,bt,b,bt,bmin,hnull
            call ERRMSG (ierr,herr)
            D=rho0
            Dl=rho0
            Dv=rho0
            p=R*t*rho0
            q=xerr
            e=xerr
            s=xerr
            cv=xerr
            cp=xerr
            w=xerr
            RETURN
          else if (b.lt.bpmax) then
c  issue warning that two roots exist
            ierr=-238
            if (kr.eq.2) then
              write (herr,1238) ab,hnull
            else
              write (herr,1239) ab,hnull
            end if
          end if
c  determine if desired solution is liquid or two-phase
          if (b.lt.bbub .or. kr.eq.2) then
c  find liquid-phase solution
            if (kr.eq.2) then
              Dmin=Dbmin
              Dmax=Dpmax
            else
              Dmin=Dlbub
              Dmax=Dbmin
            end if
            call ABFL1 (t,b,z,0,ab,Dmin,Dmax,tt,pp,D,ierr1,herr1)
            if (ierr1.ne.0) then
              ierr=ierr1
              herr=herr1
            end if
c  set remaining outputs (x = y = z as initialized above)
            Dl=D
            Dv=D
            q=-998.0d0
            call THERM (t,D,z,p,e,h,s,cv,cp,w,hjt)
          else
c  find two-phase solution
c           write (*,*) ' TBFLSH--solution is 2-phase'
            if (icomp.ne.0) then
c  special case:  pure-fluid two-phase (x = y = z as set above)
              q=(b-bbub)/(bdew-bbub)
              Dl=Dlbub
              Dv=Dvdew
            else
c  general case:  mixture 2-phase
              q=0.5
              Dl=Dlbub
              Dv=Dvdew
              ksat=0
              call ABFL2 (t,b,z,1,ksat,ab,
     &                    tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                    tt,p,Dl,Dv,x,y,q,ierr,herr)
              if (ierr1.ne.0) then
c  two-phase iteration did not converge--error message written by TBFL2
                ierr=ierr1
                herr=herr1
                call ERRMSG (ierr,herr)
                q=999.d0     !quality undefined
                RETURN
              end if
c             write (*,1225) t,q,(x(i),i=1,3),(y(i),i=1,3)
c1225         format (1x,' TBFLSH--TBFL2 return   t,q,x,y = ',8f12.7)
            end if
c  compute remaining properties for 2-phase states
            call THERM (t,Dl,x,p,el,hl,sl,cvl,cp,w,hjt)
            call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
            alpha=1.0d0-q
            D=1.0d0/(alpha/Dl+q/Dv)
            if (ianc(icomp).eq.1) p=alpha*pbub+q*pdew
            e=alpha*el+q*ev
            h=alpha*hl+q*hv
            s=alpha*sl+q*sv
            w=xnotd       !Cp,w not defined for 2-phase states
            cp=xnotd
            cv=xnotd
          end if
        end if
      end if
c
      RETURN
      end                                             !subroutine TBFLSH
c
c ======================================================================
c
      subroutine DBFLSH (D,b,z,ab,t,p,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
     &                   ierr,herr)
c
c  flash calculation given density, bulk composition, and either
c  enthalpy, entropy, or energy
c
c  inputs:
c        D--overall (bulk) molar density [mol/L]
c        b--second property (energy, enthalpy, or entropy)
c        z--overall (bulk) composition [array of mol frac]
c       ab--character*2 string defining the inputs: 'DH', 'DS', or 'DE'
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                      210 = CRITP did not converge
c                      248 = iteration did not converge
c                      249 = b out of range
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  05-13-99 EWL, original version
c  01-19-01 EWL, converted from DHFLSH to DBFLSH and made it generic
c  08-14-02 EWL, if DBLF2 returns q<=0 or q>=1, then return an error
c  12-16-02 EWL, calculate p using q for a pure or pseudo-pure fluid
c  01-14-05 EWL, add special case for helium
c  11-21-12 EWL, specify better values of q after call to ABFL1
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DBFLSH
c
      include 'commons.for'
      character*1 bt
      character*2 ab
      character*255 herr,herr2,herrl
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      bt=ab(2:2)
      ierr=0
      herr=' '
      t=0.d0
      p=0.d0
      Dl=0.d0
      Dv=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      q=999.d0     !quality undefined
c
c  initialize output liquid and vapor compositions to input values
c  zero output values for undefined components
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c
c  check that input conditions are within limits
c
      call CRITP (z,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
        ierr=210
        write (herr,1008) ab,herr2(1:235),hnull
 1008   format ('[',a2,'FLSH error 210] ',a235,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      t=tc
      call LIMITX ('EOS',t,D,p,z,tmin,tmax,rhomax,pmax,ierrl,herrl)
      if (ierrl.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ab,ierrl,herrl(1:234),hnull
 1006   format ('[',a2,'FLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierrl,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierrl.gt.0) then
        write (herr,1007) ab,ierrl,herrl(1:236),hnull
 1007   format ('[',a2,'FLSH error',i3,'] ',a236,a1)
        call ERRMSG (ierrl,herr)
        ierr=ierrl
        RETURN
      end if
c
      call SATD (d,z,0,kr,t,p,rhol,rhov,x,y,ierr,herr)
      if (tc.lt.6) then   !special case for helium
        if (tc.gt.5 .and. icomp.ne.0 .and. d.gt.36.5d0) then
          call ABFL1 (d,b,z,0,ab,0.d0,0.d0,t,p,dd,ierr,herr2)
          bsat=b-.000001d0
          goto 110
        endif
      endif
      if (bt.eq.'H') call ENTHAL (t,d,z,bsat)
      if (bt.eq.'E') call ENERGY (t,d,z,bsat)
      if (bt.eq.'S') call ENTRO (t,d,z,bsat)
      if (b.ge.bsat) then
        call ABFL1 (d,b,z,0,ab,0.d0,0.d0,t,p,dd,ierr,herr2)
      else
        call DBFL2 (d,b,z,0,ab,t,p,Dl,Dv,x,y,q,ierr,herr)
        if (q.ge.1.d0 .or. q.le.0.d0) ierr=248
      endif
 110  continue
      if (ierr.ne.0) then
        ierr=215
        write (herr,1215) ab,herr2(1:183),hnull
 1215   format ('[',a2,'FLSH error 248] iteration ',
     &          'did not converge:  ',a183,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
      if (b.gt.bsat) then
        call THERM (t,d,z,p,e,h,s,cv,cp,w,hjt)
        if (p.gt.pc .and. t.gt.tc) then
          q=999.d0     !quality undefined
        elseif (p.gt.pc) then
          q=-998.d0    !subcooled
        elseif (t.gt.tc) then
          q=998.d0     !superheated
        elseif (d.gt.rhoc) then
          q=-998.d0    !subcooled
        elseif (d.lt.rhoc) then
          q=998.d0     !superheated
        end if
      else
c  compute remaining properties for 2-phase states
        call THERM (t,Dl,x,pl,el,hl,sl,cvl,cp,w,hjt)
        call THERM (t,Dv,y,pv,ev,hv,sv,cvv,cp,w,hjt)
        alpha=1.0d0-q
        D=1.0d0/(alpha/Dl+q/Dv)
        p=alpha*pl+q*pv
        e=alpha*el+q*ev
        h=alpha*hl+q*hv
        s=alpha*sl+q*sv
        w=xnotd       !Cp,w not defined for 2-phase states
        cp=xnotd
        cv=xnotd
      endif
c
c  if limits check resulted in warning (as opposed to error) return
c  that message; do this again in case intermediate iteration did not
c  converge (thereby overwriting any warning message from LIMITX)
c
      call LIMITX ('EOS',t,D,p,z,tmin,tmax,rhomax,pmax,ierrl,herrl)
      if (ierrl.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        ierr=ierrl
        write (herr,2006) ab,ierrl,herrl(1:234),hnull
 2006   format ('[',a2,'FLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierr,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierrl.gt.0) then
        write (herr,1007) ab,ierrl,herrl(1:236),hnull
        ierr=ierrl
        call ERRMSG (ierrl,herr)
        RETURN
      end if
c
      RETURN
      end                                             !subroutine DBFLSH
c
c ======================================================================
c
      subroutine ABFL1 (a,b,x,kph,ab,dmin,dmax,t,p,D,ierr,herr)
c
c  General single phase flash calculation given two inputs and composition.
c  Valid properties for the first input are temperature, pressure, and
c  density.  Valid properties for the second input are density, energy,
c  enthalpy, or entropy.  The character string ab specifies the inputs.
c  Note that the inputs TP and TD are not allowed here, but are done by
c  calling TPFLSH or TDFLSH and their associated routines.
c
c  This routine accepts only single-phase inputs, it is intended primarily
c  for use with the more general flash routine ABFLSH.
c
c  inputs:
c        a--first property (either temperature, pressure, or density)
c        b--second property (density, energy, enthalpy, or entropy)
c        x--composition [array of mol frac]
c      kph--phase flag:  1 = liquid
c                        2 = vapor
c       ab--character*2 string defining the inputs, e.g., 'TH' or 'PS'
c     Dmin--lower bound on density [mol/L]          (for t inputs)
c     Dmax--upper bound on density [mol/L]          (for t inputs)
c        t--initial guess for temperature [K]       (for P inputs)
c        D--initial guess for molar density [mol/L] (for P inputs)
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c                      247 = TPRHO did not converge
c                      248 = single-phase iteration did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  rewritten by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  01-18-01 EWL, original version, rewritten by combining code of McLinden
c                from several different versions of FL1 routines to make
c                one unified routine
c  07-17-06 EWL, change fzneg and fzpos to 1.d20, and only update these
c                if the new guess is closer than the current bounds
c  07-17-06 EWL, if TPRHO fails, call it up to twice more with increased T
c  07-17-06 EWL, after 100 iterations, do not go outside of zneg and zpos;
c                helps in convergence very close to the critical point
c  02-16-10 EWL, increase tolerance at it=180
c  02-11-11 EWL, after 10 failures, change z(j) by 1.1 instead of 1.01
c  09-15-11  MK, add additional code for Reguli-falsi
c  05-08-12 EWL, get liquid and vapor entropy after call to SATT and use it for region check.
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ABFL1
c
      include 'commons.for'
      character*1 at,bt
      character*2 ab
      character*255 herr,herr1
      logical lpos,lneg
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      dimension z(3),fz(2)      !z is current guess for temperature
c
      itmax=200   !large value needed to find low density roots
      at=ab(1:1)
      bt=ab(2:2)
      call CRITP (x,tc,pc,rhoc,ierr1,herr1)
c
      ierr=0
      herr=' '
      p=0
      ib=0
      if (at.eq.'T') t=a
      if (at.eq.'S') s=a
      if (at.eq.'P') p=a
      if (at.eq.'D') then
        call DBFL1 (a,b,x,ab,t,p,ierr,herr)
        D=a
        RETURN
      elseif (ab.eq.'PD') then
        call DBFL1 (b,a,x,'DP',t,p,ierr,herr)
        D=b
        RETURN
      endif
      tol=1.0d-8
      ispflag=0
      lRegFalsi=.false.
c
      j=1
      lpos=.false.              !initialize flags for reguli-false
      lneg=.false.
      zneg=0.0d0
      zpos=0.0d0
      fzneg=1.0d20
      fzpos=1.0d20
      delz=0.d0
c
      if (at.eq.'P' .or. at.eq.'S') then
        if (t.le.0d0) t=tc
        z(1)=t
      elseif (at.eq.'T') then
c  first & second guesses for density are the bounds
        z(1)=Dmin
        z(2)=Dmax
      endif
      do 200 it=1,itmax
      if (at.eq.'T') then
        D=z(j)
      elseif (at.eq.'P' .or. at.eq.'S') then
        t=z(j)
        t=max(t,1.d-12)
c  Do not call TPRHO giving it an initial guess (which comes from the previous
c  iteration).  This guess can often lead it into the two-phase and return
c  a bogus value.
        if (at.eq.'P') then
          call TPRHO (t,p,x,kph,0,D,ierr,herr1)  !find density
        elseif (at.eq.'S') then
          dmin1=dmin
          dmax1=dmax
          if (t.lt.tc) then
            call ENTRO (tc,rhoc,x,sc)
            i=1
            if (s.gt.sc) i=2
            call SATT (t,x,i,pp,Dl,Dv,xliq,xvap,ierr,herr1)
c 05-08-12  EWL  Add following two lines and change check for s<sc and s>sc to s<scl and s>scv
            call ENTRO (t,dl,x,scl)
            call ENTRO (t,dv,x,scv)
            if (s.lt.scl) dmin1=dl
            if (s.gt.scv) dmax1=dv
          endif
          call ABFL1x (t,s,x,kph,'TS',dmin1,dmax1,tt,pp,D,ierr,herr1)
c     write (*,'(20f10.5)') t,d,dmin1,dmax1,s,sc,ierr
          if (ierr.ne.0) then
            ib=ib+1
            z(j)=z(j)*1.01d0
            if (ib.gt.10) z(j)=z(j)*1.1d0
            goto 200
          endif
        endif
c  if TPRHO fails on first routine, try new initial guess
        if (ierr.ne.0 .and. it.eq.1 .and. at.eq.'P') then
          z(1)=300.0d0
          t=z(j)
          call TPRHO (t,p,x,kph,0,D,ierr,herr1)
        elseif (ierr.ne.0 .and. at.eq.'P') then
c  if TPRHO fails, try midpoint
          z(j)=(z(1)+z(2))/2.d0
          t=z(j)
          call TPRHO (t,p,x,kph,0,D,ierr,herr1)
          if (ierr.eq.203) then
            z(j)=t*1.001d0
            t=z(j)
            call TPRHO (t,p,x,kph,0,D,ierr,herr1)
            if (ierr.eq.203) then
              z(j)=t*1.001d0
              t=z(j)
              call TPRHO (t,p,x,kph,0,D,ierr,herr1)
c  if still failing, call SATP
              if (ierr.eq.203) then
                call SATP (p,x,kph,t,D,Dv,xliq,xvap,ierr,herr1)
                if (kph.eq.2 .or. kph.eq.-2) d=dv
              endif
            endif
          endif
        endif
        if (it.eq.80 .and. p.le.pc .and. at.eq.'P') then
          call SATP (p,x,kph,t,D,Dv,xliq,xvap,ierr,herr1)
          if (kph.eq.2 .or. kph.eq.-2) d=dv
          z(j)=t
          ispflag=1
        endif
        if (ierr.ne.0) then
          ierr=247
          write (herr,1247) ab,herr1(1:172),hnull
 1247     format ('[',a2,'FLSH error 247] density iteration for ',
     &            'single-phase state did not converge:  ',a172,a1)
          call ERRMSG (ierr,herr)
          RETURN
        end if
      endif
      if (bt.eq.'E') call ENERGY (t,D,x,bj) !find energy for current t
      if (bt.eq.'H') call ENTHAL (t,D,x,bj) !find enthalpy for current t
      if (bt.eq.'S') call ENTRO (t,D,x,bj)  !find entropy for current t
      fz(j)=b-bj
      if (ispflag.eq.1) then
        if (fz(j).lt.0.) fzneg=1.d20
        if (fz(j).gt.0.) fzpos=1.d20
        ispflag=0
      endif
c     write (*,1020) it,j,z(j),D,p,b,bj,fz(j)
c1020 format (1x,'ABFL1: it,j,t,D,p,b,bj,ft: ',2i4,f10.5,f12.7,4f15.6)
      b1=1.d0
      if (ABS(b).gt.1.d0) b1=b
      if (ABS(fz(j)/b1).lt.tol) then
        call PRESS (t,D,x,p)
        ierr=0
        herr=' '
        RETURN
      elseif (lRegFalsi) then
        if (fz(j).gt.0.d0) then
          zpos=z(j)
          fzpos=fz(j)
          z(1) =zneg
          fz(1)=fzneg
        else
          zneg=z(j)
          fzneg=fz(j)
          z(1) =zpos
          fz(1)=fzpos
        endif
c     elseif (fz(j).lt.0.0d0) then
      elseif (fz(j).lt.0.0d0 .and. abs(fz(j)).lt.abs(fzneg)) then
c  store "negative" guess for use in possible reguli-falsi iteration
        lneg=.true.
        zneg=z(j)
        fzneg=fz(j)
c     else
      elseif (fz(j).ge.0.0d0 .and. abs(fz(j)).lt.abs(fzpos)) then
c  store "positive" guess for use in possible reguli-falsi iteration
        lpos=.true.
        zpos=z(j)
        fzpos=fz(j)
      end if
      if (j.eq.1) then
c  define second guess for temperature
        if (at.eq.'P' .or. at.eq.'S') then
          call THERM (z(1),D,x,ptherm,e1,h1,s1,cv,cp,w,hjt)
          if (bt.eq.'H') delz=fz(1)/cp
          if (bt.eq.'E') delz=fz(1)/cv
          if (bt.eq.'S') delz=fz(1)/cv
          if (ABS(delz).lt.0.0005d0*z(1)) delz=sign(0.0005d0*z(1),delz)
          z(2)=z(1)+delz
        endif
        j=2
      else
c  general case--define next guess for temperature by secant method
        if (ABS(fz(2)-fz(1)).gt.1.0d-12) then  !check for divide by zero
          z(3)=z(2)-fz(2)*(z(2)-z(1))/(fz(2)-fz(1))
        else
          z(3)=z(2)+0.5d0*(z(2)-z(1))
        end if
        zz=1.01d0
        if (it.gt.100) zz=1
        if (it.eq.150) tol=tol*100
        if (it.eq.180) tol=tol*10
        if (lneg .and. lpos .and.
     &     (z(3).gt.max(zpos,zneg)*zz .or.
     &      z(3).lt.min(zpos,zneg)/zz)) then
c  secant method has yielded guess further from solution, use reguli-falsi
          z(3)=zpos-fzpos*(zpos-zneg)/(fzpos-fzneg)
          lRegFalsi=.true.
c         write (*,*) '      next temperature by reguli-falsi: ',z(3)
        end if
        if (at.eq.'T' .and.
     &     (d.lt.dmin*.98d0 .or. d.gt.dmax*1.02d0)) then
          if (ABS(z(3)).gt.1.d5) goto 210
          if (d.lt.dmin .and. z(1).lt.z(2)) then
            z(3)=(dmin+z(2))/2.d0
          elseif (d.lt.dmin .and. z(2).lt.z(1)) then
            z(3)=(dmin+z(1))/2.d0
          elseif (d.gt.dmax .and. z(1).gt.z(2)) then
            z(3)=(dmax+z(2))/2.d0
          elseif (d.gt.dmax .and. z(2).gt.z(1)) then
            z(3)=(dmax+z(1))/2.d0
          endif
        endif
        if (.not. lRegFalsi) then
          z(1)=z(2)
          z(2)=z(3)
          fz(1)=fz(2)
        else
          z(2)=z(3)
        endif
      end if
      if (at.ne.'T') then
        if (ABS(z(2)-z(1)).gt.0.25*z(1)) then
c  do not permit too large a step
          z(2)=z(1)+z(1)*sign(0.25d0,z(2)-z(1))
        end if
      endif
 200  continue
c
c  iteration has not converged
c
 210  continue
      ierr=248
      if (at.eq.'S') write (herr,1248) ab,z(j),fz(j),hnull
      if (at.eq.'P') write (herr,1248) ab,z(j),fz(j),hnull
      if (at.eq.'T') write (herr,1249) ab,z(j),fz(j),hnull
 1248 format ('[',a2,'FLSH error 248] single-phase iteration did not ',
     &        'converge, T, deltaT =',2(g12.5),' K.',a1)
 1249 format ('[',a2,'FLSH error 248] single-phase iteration did not ',
     &        'converge, D, deltaD =',2(g12.5),' mol/L.',a1)
      call ERRMSG (ierr,herr)
c
      RETURN
      end                                              !subroutine ABFL1
c
c ======================================================================
c
      subroutine ABFL1x (a,b,x,kph,ab,dmin,dmax,t,p,D,ierr,herr)
c
c  This is a duplicate of ABFL1, called while calculating H,S inputs.
c  Without it, ABFL1 would have to call itself once, recursively, which
c  is not allowed by some compilers.
c
c  07-07-10 EWL, add check for z(1)=z(3) in reguli-falsi method
c  02-11-11 EWL, check for stuck at high densities with root at low densities
c
      include 'commons.for'
      character*1 at,bt
      character*2 ab
      character*255 herr,herr1
      logical lpos,lneg
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      dimension z(3),fz(2)      !z is current guess for temperature
c
      itmax=200   !large value needed to find low density roots
      at=ab(1:1)
      bt=ab(2:2)
      call CRITP (x,tc,pc,rhoc,ierr1,herr1)
c
      ierr=0
      herr=' '
      p=0
      if (at.eq.'T') t=a
      if (at.eq.'S') s=a
      if (at.eq.'P') p=a
      if (at.eq.'D') then
        call DBFL1 (a,b,x,ab,t,p,ierr,herr)
        D=a
        RETURN
      elseif (ab.eq.'PD') then
        call DBFL1 (b,a,x,'DP',t,p,ierr,herr)
        D=b
        RETURN
      endif
      tol=1.0d-8
      lRegFalsi=.false.
c
      j=1
      lpos=.false.              !initialize flags for reguli-false
      lneg=.false.
      zneg=0.0d0
      zpos=0.0d0
      fzneg=1.0d20
      fzpos=1.0d20
      delz=0.d0
c
      if (at.eq.'P' .or. at.eq.'S') then
        if (t.le.0d0) t=tc
        z(1)=t
      elseif (at.eq.'T') then
c  first & second guesses for density are the bounds
        z(1)=Dmin
        z(2)=Dmax
      endif
      call ENTRO (tc,rhoc,x,sc)
      do 200 it=1,itmax
      if (at.eq.'T') then
        D=z(j)
c check for case where it is stuck at high densities, but root is at low densities
        if (it.gt.5 .and. bt.eq.'S') then
          if (bj.lt.sc .and. abs(z(1)-z(2)).lt.1.d-10) then
            if (fz(1).gt.1.d0) z(j)=z(j)/10.d0
            D=z(j)
          endif
        endif
      elseif (at.eq.'P' .or. at.eq.'S') then
        t=z(j)
        t=max(t,1.d-12)
c  Do not call TPRHO giving it an initial guess (which comes from the previous
c  iteration).  This guess can often lead it into the two-phase and return
c  a bogus value.
        if (at.eq.'P') then
          call TPRHO (t,p,x,kph,0,D,ierr,herr1)  !find density
        elseif (at.eq.'S') then
          dmin1=dmin
          dmax1=dmax
          if (t.lt.tc) then
            i=1
            if (s.gt.sc) i=2
            call SATT (t,x,i,pp,Dl,Dv,xliq,xvap,ierr,herr1)
            if (s.lt.sc) dmin1=dl
            if (s.gt.sc) dmax1=dv
          endif
          call TSFL1 (t,s,x,dmin1,dmax1,D,ierr,herr1)
c     write (*,'(20f10.5)') t,d,dmin1,dmax1,s,sc,ierr
          if (ierr.ne.0) then
            z(j)=z(j)*1.01d0
            goto 200
          endif
        endif
c  if TPRHO fails on first routine, try new initial guess
        if (ierr.ne.0 .and. it.eq.1 .and. at.eq.'P') then
          z(1)=300.0d0
          t=z(j)
          call TPRHO (t,p,x,kph,0,D,ierr,herr1)
        elseif (ierr.ne.0 .and. at.eq.'P') then
c  if TPRHO fails, try midpoint
          z(j)=(z(1)+z(2))/2.d0
          t=z(j)
          call TPRHO (t,p,x,kph,0,D,ierr,herr1)
          if (ierr.eq.203) then
            z(j)=t*1.001d0
            t=z(j)
            call TPRHO (t,p,x,kph,0,D,ierr,herr1)
            if (ierr.eq.203) then
              z(j)=t*1.001d0
              t=z(j)
              call TPRHO (t,p,x,kph,0,D,ierr,herr1)
            endif
          endif
        endif
        if (ierr.ne.0) then
          ierr=247
          write (herr,1247) ab,herr1(1:172),hnull
 1247     format ('[',a2,'FLSH error 247] density iteration for ',
     &            'single-phase state did not converge:  ',a172,a1)
          call ERRMSG (ierr,herr)
          RETURN
        end if
      endif
      if (bt.eq.'E') call ENERGY (t,D,x,bj) !find energy for current t
      if (bt.eq.'H') call ENTHAL (t,D,x,bj) !find enthalpy for current t
      if (bt.eq.'S') call ENTRO (t,D,x,bj)  !find entropy for current t
      fz(j)=b-bj
c     write (*,1020) it,j,z(j),D,p,b,bj,fz(j)
c1020 format (1x,'ABFL1: it,j,t,D,p,b,bj,ft: ',2i4,f10.5,f12.7,4f15.6)
      b1=1.d0
      if (ABS(b).gt.1.d0) b1=b
      if (ABS(fz(j)/b1).lt.tol) then
        call PRESS (t,D,x,p)
        ierr=0
        herr=' '
        RETURN
      elseif (lRegFalsi) then
        if (fz(j).gt.0.d0) then
          zpos=z(j)
          fzpos=fz(j)
          z(1) =zneg
          fz(1)=fzneg
        else
          zneg=z(j)
          fzneg=fz(j)
          z(1) =zpos
          fz(1)=fzpos
        endif
c     elseif (fz(j).lt.0.0d0) then
      elseif (fz(j).lt.0.0d0 .and. abs(fz(j)).lt.abs(fzneg)) then
c  store "negative" guess for use in possible reguli-falsi iteration
        lneg=.true.
        zneg=z(j)
        fzneg=fz(j)
c     else
      elseif (fz(j).ge.0.0d0 .and. abs(fz(j)).lt.abs(fzpos)) then
c  store "positive" guess for use in possible reguli-falsi iteration
        lpos=.true.
        zpos=z(j)
        fzpos=fz(j)
      end if
      if (j.eq.1) then
c  define second guess for temperature
        if (at.eq.'P' .or. at.eq.'S') then
          call THERM (z(1),D,x,ptherm,e1,h1,s1,cv,cp,w,hjt)
          if (bt.eq.'H') delz=fz(1)/cp
          if (bt.eq.'E') delz=fz(1)/cv
          if (bt.eq.'S') delz=fz(1)/cv
          if (ABS(delz).lt.0.0005d0*z(1)) delz=sign(0.0005d0*z(1),delz)
          z(2)=z(1)+delz
        endif
        j=2
      else
c  general case--define next guess for temperature by secant method
        if (ABS(fz(2)-fz(1)).gt.1.0d-12) then  !check for divide by zero
          z(3)=z(2)-fz(2)*(z(2)-z(1))/(fz(2)-fz(1))
        else
          z(3)=z(2)+0.5d0*(z(2)-z(1))
        end if
        zz=1.01d0
        if (it.gt.100) zz=1
        if (it.eq.150) tol=tol*2
        if (lneg .and. lpos .and.
     &     (z(3).gt.max(zpos,zneg)*zz .or.
     &      z(3).lt.min(zpos,zneg)/zz)) then
c  secant method has yielded guess further from solution, use reguli-falsi
c         write (*,*) 'ABFL1--using reguli-falsi; t by secant: ',z(3)
          z(3)=zpos-fzpos*(zpos-zneg)/(fzpos-fzneg)
          lRegFalsi=.true.
c         write (*,*) '      next temperature by reguli-falsi: ',z(3)
        end if
        if (at.eq.'T' .and.
     &     (d.lt.dmin*.98d0 .or. d.gt.dmax*1.02d0)) then
          if (ABS(z(3)).gt.1.d5) goto 210
          if (d.lt.dmin .and. z(1).lt.z(2)) then
            z(3)=(dmin+z(2))/2.d0
          elseif (d.lt.dmin .and. z(2).lt.z(1)) then
            z(3)=(dmin+z(1))/2.d0
          elseif (d.gt.dmax .and. z(1).gt.z(2)) then
            z(3)=(dmax+z(2))/2.d0
          elseif (d.gt.dmax .and. z(2).gt.z(1)) then
            z(3)=(dmax+z(1))/2.d0
          endif
        endif
        if (.not. lRegFalsi) then
          z(1)=z(2)
          z(2)=z(3)
          fz(1)=fz(2)
        else
          z(2)=z(3)
        endif
      end if
      if (at.ne.'T') then
        if (ABS(z(2)-z(1)).gt.0.25*z(1)) then
c  do not permit too large a step
          z(2)=z(1)+z(1)*sign(0.25d0,z(2)-z(1))
        end if
      endif
 200  continue
c
c  iteration has not converged
c
 210  continue
      ierr=248
      if (at.eq.'S') write (herr,1248) ab,z(j),fz(j),hnull
      if (at.eq.'P') write (herr,1248) ab,z(j),fz(j),hnull
      if (at.eq.'T') write (herr,1249) ab,z(j),fz(j),hnull
 1248 format ('[',a2,'FLSH error 248] single-phase iteration did not ',
     &        'converge, T, deltaT =',2(g12.5),' K.',a1)
 1249 format ('[',a2,'FLSH error 248] single-phase iteration did not ',
     &        'converge, D, deltaD =',2(g12.5),' mol/L.',a1)
      call ERRMSG (ierr,herr)
c
      RETURN
      end                                             !subroutine ABFL1x
c
c ======================================================================
c
      subroutine DBFL1 (d,b,x,ab,t,p,ierr,herr)
c
c  General single-phase calculation given density, composition, and either
c  pressure, energy, enthalpy, or entropy.  The character string ab
c  specifies the inputs.  This routine should ONLY be called by ABFL1.
c
c  inputs:
c        d--molar density [mol/L]
c        b--second property (pressure, energy, enthalpy, or entropy)
c        x--composition [array of mol frac]
c       ab--character*2 string defining the inputs: 'DH', 'DS', 'DE', or 'DP'
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c     ierr--error flag:  0 = successful
c                      200 = CRITP did not converge
c                      202 = liquid-phase iteration did not converge
c                      203 = vapor-phase iteration did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  rewritten by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  01-18-01 EWL, original version
c  02-27-12 EWL, reset ierr after call to CRITP
c  05-25-12 EWL, move calculation of p and dpdt inside IF block
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DBFL1
c
      include 'commons.for'
      character*1 bt
      character*2 ab
      character*255 herr,herr1
      dimension x(ncmax)
      parameter (itmax=30)
c
      bt=ab(2:2)
      tolr=1.0d-6
      p=b
      ierr=0
      herr=' '
c     write (*,1001) b,rho,(x(i),i=1,5)
c1001 format (1x,' DBFL1--input b,rho,x:           ',f10.5,f14.6,5f10.6)
c
      call Rmix (x)
      if (b.lt.1.0d-14 .and. bt.eq.'P') then
c  ideal-gas
        t=b/(R*d)
        RETURN
      end if
c
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      if (ierr.gt.0) then
        ierr=200
        write (herr,1200) herr1(1:236),hnull
 1200   format ('[TPRHO error 200] ',a236,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
      ierr=0
      herr=' '
c
c  set initial guess for temperature based on region
      t=tc
      if (bt.eq.'P' .and. b.gt.pc*100) t=tc*2
c     write (*,1082) t,b,d,(x(i),i=1,nc)
c1082 format (1x,' DBFL1--t,b,rho,x: ',5f10.5)
c
c  enter Newton's method iteration
      do it=1,itmax
        if (bt.eq.'P') then
          call PRESS (t,d,x,b1)
          call DPDT (t,d,x,dbdt)
        elseif (bt.eq.'H') then
          call DPDT (t,d,x,dpt)
          call CVCP (t,d,x,cv,cp)
          call ENTHAL (t,d,x,b1)
          dbdt=cv+R*p/d/R/t-p/d/t+dpt/d  !dh/dT at constant d
        elseif (bt.eq.'E') then
          call CVCP (t,d,x,cv,cp)
          call ENERGY (t,d,x,b1)
          dbdt=cv                        !de/dT at constant d
        elseif (bt.eq.'S') then
          call CVCP (t,d,x,cv,cp)
          call ENTRO (t,d,x,b1)
          dbdt=cv/t                      !ds/dT at constant d
        endif
c       write (*,1010) it,p/1000.0d0,d,x(1),t,p1
c1010   format (1x,'it,p,rho,x(1),t,p: ',i3,2f8.3,f8.4,2f14.8)
        t=t+(b-b1)/dbdt
        if (t.le.0) t=tc/2.d0
        b2=1.d0
        if (ABS(b).gt.1.d0) b2=b
        if (ABS((b-b1)/b2).lt.tolr) then
          call PRESS (t,d,x,p)
          RETURN
        endif
      enddo
c  iteration has not converged
      ierr=203
      write (herr1,1203) ab,d,t,(x(j),j=1,MIN(nc,5))
 1203 format ('[',a2,'FL1 error 203] iteration has not ',
     &        'converged for rho =',g12.5,' mol/L,',
     &        ' T (last guess) = ',g12.5,
     &        ' K, x (mol frac) =',5(0pf8.5))
      herr=herr1(1:254)//hnull
      call ERRMSG (ierr,herr)
c     write (*,*) ' DBFL1 final t (not converged): ',t
      t=0
      RETURN
c
      end                                              !subroutine DBFL1
c
c ======================================================================
c
      subroutine ABFL2 (a,b,z,kq,ksat,ab,
     &                  tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,p,Dl,Dv,x,y,q,ierr,herr)
c
c  General flash calculation given two inputs and composition.  Valid
c  properties for the first input are temperature and pressure.  Valid
c  properties for the second input are density, energy, enthalpy, entropy,
c  or quality.  The character string ab specifies the inputs.  Note that
c  the input TP is not allowed here, but is done by calling TPFLSH or
c  TPFL2.
c
c  This routine calls TPFL2 within a secant-method iteration for
c  pressure to find a solution.  Initial guesses are based on liquid
c  density at the bubble point and vapor density at the dew point.
c
c  inputs:
c        a--first property (either temperature or pressure)
c        b--second property (density, energy, enthalpy, entropy, or quality)
c        z--overall (bulk) composition [array of mol frac]
c       ab--character*2 string defining the inputs, e.g., 'TD' or 'PQ'
c       kq--flag specifying units for input quality when b=quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c  (for a=pressure):
c     tbub--bubble point temperature [K] at (p,x=z)
c     tdew--dew point temperature [K] at (p,y=z)
c  (for a=temperature):
c     pbub--bubble point pressure [kPa] at (t,x=z)
c     pdew--dew point pressure [kPa] at (t,y=z)
c  (for either case):
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c                      223 = bubble point calculation did not converge
c                      224 = dew point calculation did not converge
c                      225 = TPFL2 did not converge
c                      226 = 2-phase iteration did not converge
c                      227 = input is outside saturation limits
c                      228 = input is outside saturation limits
c     herr--error string (character*255 variable if ierr<>0)
c
c  rewritten by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  12-12-00 EWL, original version, rewritten by combining code of McLinden
c                from several different versions of FL2 routines to make
c                one unified routine
c  12-16-02 EWL, call PTANC upon completion for a pseudo-pure fluid
c  02-18-02 EWL, remove check for closeness to phase boundary for nc=1
c  09-02-04 EWL, add check for tbub-tdew<.001
c  05-28-08 EWL, move check for near azeotropes after call to PTANC
c  08-04-11 EWL, add check for d<dv
c
cx DEC$ ATTRIBUTES DLLEXPORT :: ABFL2
c
      include 'commons.for'
      character*1 at,bt
      character*2 ab
      character*255 herr,herr1
      dimension z(ncmax),x(ncmax),y(ncmax),aj(3),faj(2),
     &          xdew(ncmax),ydew(ncmax),xbub(ncmax),ybub(ncmax),
     &          x1(ncmax),y1(ncmax),xkg(ncmax),ykg(ncmax)
      parameter (itmax=30)
c
      at=ab(1:1)
      bt=ab(2:2)
      ajpos=0.d0
      ajneg=0.d0
      aratio=0.d0
      fajpos=0.d0
      fajneg=0.d0
      lRegFalsi=.false.
      hvap=0.d0
      aliq=0.d0
      avap=0.d0
      dl1=0.d0
      dv1=0.d0
      q1=0.d0
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif

c
c  calculate bubble and dew points to establish bounds on iteration;
c  store results for use in possible reguli-falsi
      if (at.eq.'T') t=a
      if (at.eq.'P') p=a
      if (ksat.ne.1) then
        if (at.eq.'T') then
          call SATT (t,z,1,pbub,Dlbub,Dvbub,xbub,ybub,ierr,herr1)
        elseif (at.eq.'P') then
          call SATP (p,z,1,tbub,Dlbub,Dvbub,xbub,ybub,ierr,herr1)
        endif
        if (ierr.ne.0) then
          ierr=223
          write (herr,1223) ab,herr1(1:164),hnull
 1223     format ('[',a2,'FLSH error 223] bubble point calculation did',
     &            ' not converge (2-phase initial guess):  ',a164,a1)
          call ERRMSG (ierr,herr)
          q=999.d0     !quality undefined
          RETURN
        end if
        if (at.eq.'T') then
          call SATT (t,z,2,pdew,Dldew,Dvdew,xdew,ydew,ierr,herr1)
        elseif (at.eq.'P') then
          call SATP (p,z,2,tdew,Dldew,Dvdew,xdew,ydew,ierr,herr1)
        endif
        if (ierr.ne.0) then
          ierr=224
          write (herr,1224) ab,herr1(1:167),hnull
 1224     format ('[',a2,'FLSH error 224] dew point calculation did ',
     &            'not converge (2-phase initial guess):  ',a167,a1)
          call ERRMSG (ierr,herr)
          q=999.d0     !quality undefined
          RETURN
        end if
      end if
      if (at.eq.'T') then
        tbub=t
        tdew=t
        ajpos=pbub
        ajneg=pdew
        aratio=pbub/pdew-1.0d0
      elseif (at.eq.'P') then
        ajpos=tbub
        ajneg=tdew
        aratio=tdew/tbub-1.0d0
      endif
      ajmax=max(ajpos,ajneg)
      ajmin=min(ajpos,ajneg)
c  base convergence tolerance on abub/adew ratio:  otherwise, near-
c  azeotropes present impossible iteration (yet initial guesses are
c  very good)
c         write (*,*) ' ABFL2--adjusted tolb:  ',tolb
      tolb=5.0d-6
      if (ABS(aratio).lt.1.0d-8) aratio=1.0d-8
      tolb=MAX(tolb/1.0d2,MIN(tolb*1.0d2,0.02d0*tolb/aratio))
      if (bt.eq.'D') then
        fajpos=b*(1.0d0/b-1.0d0/Dlbub)
        fajneg=b*(1.0d0/b-1.0d0/Dvdew)
      elseif (bt.eq.'H' .or. bt.eq.'S' .or. bt.eq.'E') then
        if (bt.eq.'H') then
          call ENTHAL (tbub,Dlbub,z,blbub)
          call ENTHAL (tdew,Dvdew,z,bvdew)
        elseif (bt.eq.'S') then
          call ENTRO (tbub,Dlbub,z,blbub)
          call ENTRO (tdew,Dvdew,z,bvdew)
        elseif (bt.eq.'E') then
          call ENERGY (tbub,Dlbub,z,blbub)
          call ENERGY (tdew,Dvdew,z,bvdew)
        endif
c  input is outside of saturation limits
        if (b.lt.blbub .or. b.gt.bvdew) then
          ierr=227
          if (b.gt.bvdew) ierr=228
          write (herr,1227) ab,ierr,hnull
 1227     format ('[',a2,'FLSH error ',i3,'] input is outside',
     &            ' saturation bounds',a1)
          call ERRMSG (ierr,herr)
          q=999.d0     !quality undefined
          RETURN
        endif
c  normalize by pseudo enthalpy of vaporization
        hvap=blbub-bvdew
        if (ABS(hvap).lt.1.d0) hvap=1.d0
        fajpos=(b-blbub)/hvap
        fajneg=(b-bvdew)/hvap
      elseif (bt.eq.'Q') then
        if (kq.eq.2) then
c  input is on mass basis
          call QMASS (0.d0,z,ybub,qkg,xkg,ykg,wliq,wvap,ierr1,herr1)
          fajpos=b-qkg
          call QMASS (1.d0,xdew,z,qkg,xkg,ykg,wliq,wvap,ierr1,herr1)
          fajneg=b-qkg
        else
          fajpos=b
          fajneg=b-1.d0
        end if
      endif
c  check to see if initial guess is near solution
      if (icomp.eq.0) then
        if (ABS(fajpos).lt.tolb .and. ksat.eq.0) then
          if (at.eq.'T') p=ajpos
          if (at.eq.'P') t=ajpos
          dl=Dlbub
          dv=Dvbub
          do i=1,nc
            x(i)=xbub(i)
            y(i)=ybub(i)
          enddo
          q=0
          RETURN
        elseif (ABS(fajneg).lt.tolb .and. ksat.eq.0) then
          if (at.eq.'T') p=ajneg
          if (at.eq.'P') t=ajneg
          dl=Dldew
          dv=Dvdew
          do i=1,nc
            x(i)=xdew(i)
            y(i)=ydew(i)
          enddo
          q=1.d0
          RETURN
        endif
      endif
      aj(1)=ajneg
      faj(1)=fajneg
      j=2
c     write (*,1004) ajpos,fajpos,ajneg,fajneg
c1004 format (1x,' ABFL2--p,fp bounds:  ',4f12.6)
c     write (*,1006) -1,0,ajpos,Dlbub,xnotd,z(1),ybub(1),fajpos
c     write (*,1006)  0,1,aj(1),xnotd,Dvdew,xdew(1),z(1),faj(1)
c1006 format (' ABFL2: it,j,aj,Dl,Dv,x,y,faj: ',2i3,5(1x,f12.6),f18.10)
c
c  initial guesses for quality, liquid & vapor composition
      if (bt.eq.'D') then
        q=(1.0d0/b-1.0d0/Dlbub)/(1.0d0/Dvdew-1.0d0/Dlbub)
      elseif (bt.eq.'H' .or. bt.eq.'S' .or. bt.eq.'E') then
        q=(b-blbub)/(bvdew-blbub)
      elseif (bt.eq.'Q') then
        q=b
      endif
      dl=Dlbub
      dv=Dvdew
      if (at.eq.'T') p=pbub
      if (at.eq.'P') t=tbub
      if (icomp.ne.0) then        !Finished for pure fluid
        if (ianc(icomp).eq.1) call PTANC (t,p,q,b,bt,Dl,Dv)
        RETURN
      endif
      if (at.eq.'P' .and. abs(tbub-tdew).lt.0.001d0) then
c  for very near azeotropes or when q is nearly 0 or 1, ABFL2 may not
c  converge so return a simple solution
        t=tbub*(1.d0-q)+q*tdew
        RETURN
      endif
c
      nfail=0
 230  continue
      if (q.gt.1.d0) q=0.95d0
      if (q.lt.0.d0) q=0.05d0
      xsum=0.0d0
      ysum=0.0d0
      do i=1,nc
        x(i)=(1.0d0-q)*z(i)+q*xdew(i)
        y(i)=q*z(i)+(1.0d0-q)*ybub(i)
        xsum=xsum+x(i)
        ysum=ysum+y(i)
      enddo
      do i=1,nc
        x(i)=x(i)/xsum
        y(i)=y(i)/ysum
        if (nfail.eq.0) then
          x1(i)=x(i)         !save initial guesses for use in case
          y1(i)=y(i)         !TPFL2 does not converge
        endif
      enddo
      ierr1=0
c
      if (at.eq.'T') then
        call SATT (t,x,1,pliq,Dlx,Dvx,xbub,ybub,ierr1,herr1)
        aliq=pliq
      elseif (at.eq.'P') then
        call SATP (p,x,1,tliq,Dlx,Dvx,xbub,ybub,ierr1,herr1)
        aliq=tliq
      endif
      if (ierr1.ne.0) then
        ierr=223
        write (herr,2223) ab,herr1(1:164),hnull
 2223   format ('[',a2,'FLSH error 223] bubble point calculation did ',
     &          'not converge within 2-phase iteration:  ',a164,a1)
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
        RETURN
      end if
      if (at.eq.'T') then
        call SATT (t,y,2,pvap,Dly,Dvy,xdew,ydew,ierr1,herr1)
        avap=pvap
      elseif (at.eq.'P') then
        call SATP (p,y,2,tvap,Dly,Dvy,xdew,ydew,ierr1,herr1)
        avap=tvap
      endif
      if (ierr1.ne.0) then
        ierr=224
        write (herr,2224) ab,herr1(1:167),hnull
 2224   format ('[',a2,'FLSH error 224] dew point calculation did ',
     &          'not converge within 2-phase iteration:  ',a167,a1)
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
        RETURN
      end if
      Dl=1.0d0/((1.0d0-q)/Dlx+q/Dly)  !initial guesses for use in TPFL2
      Dv=1.0d0/((1.0d0-q)/Dvx+q/Dvy)  !based on volumes
      aj(2)=(1.0d0-q)*aliq+q*avap
c     write (*,1244) aj(2),q,(x(i),i=1,3),(y(i),i=1,3)
c1244 format (1x,' ABFL2 initial guesses--p,q,x,y = ',8f12.7)
      aj1=aj(2)
      if (nfail.eq.0) then
        Dl1=Dl             !save initial guesses for use in case
        Dv1=Dv             !TPFL2 does not converge
        q1=q
      endif
      if (bt.eq.'D' .and. nfail.lt.5) then
        if (b.lt.dv) then
          nfail=nfail+1
          q=q*20.d0  !Initial q not very good (usually very small).
          goto 230
        endif
      endif
      do 200 it=2,itmax
      if (it.eq.7) tolb=tolb*10
      if (it.eq.10) tolb=tolb*10
      if (it.eq.15) tolb=tolb*10
      if (at.eq.'T') p=aj(j)
      if (at.eq.'P') t=aj(j)
      q2=q
c  find density and composition for each phase
c  note that the initial guesses for Dl,Dv,x,y,q are refined in TPFL2
      call TPFL2 (t,p,z,Dl,Dv,x,y,q,ierr1,herr1)
      if (ierr1.ne.0) then
c  TPFL2 has not converged
        nfail=nfail+1
c       write (*,1016) aj(j)/1000.d0,q,herr1
c1016   format (1x,' ABFL2--TPFL2 returns error for p = ',f14.6,
c    &             ' MPa, quality = ',f10.6,': ',a255)
        if (nfail.ge.25) then
c  TPFL2 has not converged several times--return the initial guesses
          do i=1,nc
            x(i)=x1(i)         !retrieve initial guesses
            y(i)=y1(i)
          enddo
          Dl=Dl1
          Dv=Dv1
          if (at.eq.'T') p=aj1
          if (at.eq.'P') t=aj1
          q=q1
          if (q.lt.0.005d0 .or. q.gt.0.995d0) then
c  initial guesses are excellent very close to saturation
            ierr=0
            herr=' '
c           write (*,*) ' ABFL2--returning initial guesses'
          else
            ierr=225
            write (herr,2225) ab,hnull
 2225       format ('[',a2,'FLSH error 225] TPFL2 did not converge',
     &          ' in two-phase iteration; returning initial guesses',
     &          ' based on dew and bubble points',a1)
            call ERRMSG (ierr,herr)
          end if
          RETURN
        end if
c  set next guess for pressure by moving 2 % of way towards q = 0.5
        q=q2+SIGN(0.02d0,0.5d0-q2)
c       write (*,1034) q
c1034   format (1x,' ABFL2--next guess for quality = ',33x,f10.6)
        goto 230
      end if
      if (bt.eq.'D') then
        Vj=(1.0d0-q)/Dl+q/Dv       !bulk volume for current iteration
        faj(j)=b*(1.0d0/b-Vj)      !objective function
      elseif (bt.eq.'H') then
        call ENTHAL (t,Dl,x,bl)
        call ENTHAL (t,Dv,y,bv)
        faj(j)=(b-(1.0d0-q)*bl-q*bv)/hvap    !objective function
      elseif (bt.eq.'S') then
        call ENTRO (t,Dl,x,bl)
        call ENTRO (t,Dv,y,bv)
        faj(j)=(b-(1.0d0-q)*bl-q*bv)/hvap    !objective function
      elseif (bt.eq.'E') then
        call ENERGY (t,Dl,x,bl)
        call ENERGY (t,Dv,y,bv)
        faj(j)=(b-(1.0d0-q)*bl-q*bv)/hvap    !objective function
      elseif (bt.eq.'Q') then
        if (kq.eq.2) then
c    input is on mass basis
          call QMASS (q,x,y,qkg,xkg,ykg,wliq,wvap,ierr1,herr1)
          faj(j)=b-qkg
        else
          faj(j)=b-q
        end if
c     write (*,1040) it,j,aj(j),Dl,Dv,x(1),y(1),faj(j)
c1040 format (' ABFL2: it,j,aj,Dl,Dv,x,y,faj: ',2i3,5(1x,f12.6),f18.10)
      endif
      if (ABS(faj(j)).lt.tolb) then
c  iteration has converged
        if (at.eq.'T') p=aj(j)
        if (at.eq.'P') t=aj(j)
        if (ierr1.ne.0) then
          ierr=ierr1
          herr=herr1
        endif
        RETURN
      elseif (lRegFalsi) then
        if (faj(j).gt.0.d0) then
          ajpos=aj(j)
          fajpos=faj(j)
          aj(1) =ajneg
          faj(1)=fajneg
        else
          ajneg=aj(j)
          fajneg=faj(j)
          aj(1) =ajpos
          faj(1)=fajpos
        endif
      else if (faj(j).lt.0.0d0) then
c  store "negative" guess for use in possible reguli-falsi iteration
        ajneg=aj(j)
        fajneg=faj(j)
      else
c  store "positive" guess for use in possible reguli-falsi iteration
        ajpos=aj(j)
        fajpos=faj(j)
      end if

c  define next guess for pressure by secant method
      if (ABS(faj(2)-faj(1)).gt.1.0d-12) then  !check: divide by zero
        aj(3)=aj(2)-faj(2)*(aj(2)-aj(1))/(faj(2)-faj(1))
      else
        aj(3)=aj(2)+0.5d0*(aj(2)-aj(1))
      end if
      amax=max(ajpos,ajneg)
      amin=min(ajpos,ajneg)
      if (aj(3).gt.amax*1.2d0 .or. aj(3).lt.amin*0.8d0) then
c  secant method has yielded guess further from solution, use reguli-falsi
c       write (*,*) 'ABFL2--using reguli-falsi; p by secant: ',aj(3)
        if (ABS(ajpos-ajneg).gt.0.01d0) then
          aj(3)=ajpos-fajpos*(ajpos-ajneg)/(fajpos-fajneg)
          lRegFalsi=.true.
        endif
c       write (*,*) '         next pressure by reguli-falsi: ',aj(3)
      end if
      if (aj(3).gt.ajmax) then
        aj(3)=(ajmax+amax)/2.d0
        if (ABS(ajmax-amax).lt.0.001) aj(3)=(amax+amin)/2.d0
      elseif (aj(3).lt.ajmin) then
        aj(3)=(ajmin+amin)/2.d0
        if (ABS(ajmin-amin).lt.0.001) aj(3)=(amax+amin)/2.d0
      end if
      if (.not. lRegFalsi) then
        aj(1)=aj(2)
        aj(2)=aj(3)
        faj(1)=faj(2)
      else
        aj(2)=aj(3)
      endif
      if (ABS(aj(2)-aj(1)).gt.0.25*aj(1))
c  do not permit too large a step
     &  aj(2)=aj(1)+aj(1)*SIGN(0.25d0,aj(2)-aj(1))
 200  continue
c
c  iteration has not converged
c
      if (at.eq.'P') t=aj(j)
      ierr=226
      write (herr,1226) ab,aj(j)/1000.d0,faj(j),hnull
 1226 format ('[',a2,'FLSH error 226] 2-phase iteration did not',
     &        ' converge, X, delta =',2(g12.5),a1)
      call ERRMSG (ierr,herr)
c
      RETURN
      end                                              !subroutine ABFL2
c
c ======================================================================
c
      subroutine DBFL2 (d,b,z,kq,ab,t,p,Dl,Dv,x,y,q,ierr,herr)
c
c  General flash calculation given density, composition, and either
c  energy, enthalpy, entropy, or quality.  The character string ab
c  specifies the inputs.
c
c  inputs:
c        d--overall (bulk) molar density [mol/L]
c        b--second property (energy, enthalpy, entropy, or quality)
c        z--overall (bulk) composition [array of mol frac]
c       kq--flag specifying units for input quality when b=quality
c           kq = 1 quality on MOLAR basis [moles vapor/total moles]
c           kq = 2 quality on MASS basis [mass vapor/total mass]
c       ab--character*2 string defining the inputs: 'DH', 'DS', 'DE', or 'DQ'
c
c  outputs:
c        t--temperature [K]
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c                      223 = bubble point calculation did not converge
c                      224 = dew point calculation did not converge
c                      225 = TPFL2 did not converge
c                      226 = 2-phase iteration did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  rewritten by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  12-13-00 EWL, original version
c  01-21-04 EWL, increase itmax to 50 for a pure fluid (helps in critical region)
c  01-21-04 EWL, add t=aj(j) at very bottom for nonconvergence
c  05-05-09 EWL, change the parameter x to z in the call to CRITP
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DBFL2
c
      include 'commons.for'
      character*2 ab,tb
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),aj(3),faj(2),
     &          xdew(ncmax),ydew(ncmax),xbub(ncmax),ybub(ncmax)
c
      itmax=20
      call ISPURE (z,icomp)
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
      if (icomp.ne.0) itmax=50
      tb=ab
      tb(1:1)='T'
      tt=300.d0
      pp=0.d0
      dd=0.d0
c   get upper and lower bounds on T, do not initially call the ABFL2 routine
c   at the lower bound because T may be too low.
      call LIMITX ('EOS',tt,dd,pp,z,tmin,tmax,Dmax,pmax,ierr,herr)
      call SATD (d,z,1,kr,tmax,p,Dldew,Dvbub,xbub,ydew,ierr,herr)
      if (ierr.ne.0) call CRITP (z,tmax,pc,rhoc,ierr,herr)
      ajpos=tmin
      ajneg=tmax
c     fajneg=0.d0
c     fajpos=0.d0
      tolb=1.0d-4
c  use initial guess 1/3 and 2/3 of the way from tmin to tmax
      aj(1)=tmin+(tmax-tmin)/3.d0
      aj(2)=tmin+(tmax-tmin)/3.d0*2.d0
      it=1
 300  continue
      call ABFL2 (aj(1),b,z,kq,0,tb,
     &            tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &            tt,p,Dl,Dv,x,y,q,ierr,herr)
      if (ierr.eq.227 .or. ierr.eq.228) then
        it=it+1
        if (it.gt.itmax) RETURN
        if (ierr.eq.227) then
          aj(1)=(aj(1)+tmin)/2.d0
        elseif (ierr.eq.228) then
          aj(2)=(aj(2)+tmax)/2.d0
          aj(1)=(aj(1)+aj(2))/2.d0
        endif
        goto 300
      endif
      faj(1)=d
      if (Dl.gt.0 .and. Dv.gt.0) faj(1)=d-1.d0/((1.0d0-q)/Dl+q/Dv)
      j=2
      do 200 it=2,itmax
        call ABFL2 (aj(2),b,z,kq,0,tb,
     &              tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &              tt,p,Dl,Dv,x,y,q,ierr,herr)
        if (ierr.eq.227) then
          aj(2)=(aj(2)+aj(1))/2.d0
          goto 200
        endif
        faj(j)=d-1.d0/((1.0d0-q)/Dl+q/Dv)
        if (ABS(faj(j)).lt.tolb .and. ierr.eq.0) then
          t=aj(j)
          RETURN
        else if (faj(j).lt.0.0d0) then
          ajneg=aj(j)
c         fajneg=faj(j)
        else
          ajpos=aj(j)
c         fajpos=faj(j)
        end if
c   define next guess for pressure by secant method
        if (ABS(faj(2)-faj(1)).gt.1.0d-12) then  !check: divide by zero
          aj(3)=aj(2)-faj(2)*(aj(2)-aj(1))/(faj(2)-faj(1))
        else
          aj(3)=aj(2)+0.5d0*(aj(2)-aj(1))
        end if
        amax=max(ajpos,ajneg)
        amin=min(ajpos,ajneg)
c   keep root within tmin to tmax
        if (aj(3).gt.tmax) then
          aj(3)=(tmax+amax)/2.d0
          if (ABS(tmax-amax).lt.0.001) aj(3)=(amax+amin)/2.d0
        elseif (aj(3).lt.tmin) then
          aj(3)=(tmin+amin)/2.d0
          if (ABS(tmin-amin).lt.0.001) aj(3)=(amax+amin)/2.d0
        end if
        if (aj(3).gt.amax*1.001d0 .or. aj(3).lt.amin*0.999d0) then
          aj(3)=(amax+amin)/2.d0
        endif
c   if not converging, try cutting bounds in half for next guess
        if (it.gt.7 .and. it.eq.INT(it/2)*2) aj(3)=(amax+amin)/2.d0
        aj(1)=aj(2)
        aj(2)=aj(3)
        faj(1)=faj(2)
 200  continue
      t=aj(j)
      if (ierr.ne.0) herr(2:2)='D'
      RETURN
      end                                              !subroutine DBFL2
c
c ======================================================================
c
      subroutine PTANC (t,p,q,b,bt,Dl,Dv)
c
c  For a pseudo-pure fluid, find a saturated pressure that agrees with
c  calculations given temperature as an input.  This problem occurs since
c  the pseudo-pure fluid equations cannot calculate equilibrium phases.
c  Any given input temperature has an associated pbub and pdew.  At a
c  pressure between these two, there are associated values of tbub and tdew.
c  Thus, there are four state points that could be attributed to the two-phase
c  input.  To maintain consistency, the two points associated with tbub
c  and tdew are used through out REFPROP, and an iterative solution is
c  required when p is one of the inputs to find the right state point.
c
c  arguments:
c        t--temperature [K] (output, but requires an initial guess)
c        p--pressure [kPa] (input)
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c        b--density, energy, enthalpy, or entropy, depending on bt
c       bt--input of second property (Q, D, E, H, or S)
c
c  outputs:
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  12-17-02 EWL, original version
c
      include 'commons.for'
      character*255 herr
      character*1 bt
      dimension z(ncmax),x(ncmax),y(ncmax)
c
      t1=t*1.001d0
      p1=p*1.01d0
      i=0
      z(1)=1.d0
 10   continue
        call SATT (t,z,1,pl,Dl,Dvbub,x,y,ierr,herr)
        call SATT (t,z,2,pv,Dldew,Dv,x,y,ierr,herr)
        if (bt.eq.'Q') then
        elseif (bt.eq.'D') then
          q=(1.0d0/b-1.0d0/Dl)/(1.0d0/Dv-1.0d0/Dl)
        else
          call THERM (t,Dl,z,pl,el,hl,sl,cv,cp,w,hjt)
          call THERM (t,Dv,z,pv,ev,hv,sv,cv,cp,w,hjt)
          if (bt.eq.'E') then
            q=(b-el)/(ev-el)
          elseif (bt.eq.'H') then
            q=(b-hl)/(hv-hl)
          elseif (bt.eq.'S') then
            q=(b-sl)/(sv-sl)
          endif
        endif
        pp=(1.d0-q)*pl+q*pv
        dt=t1-t
        dp=p1-pp
        t1=t
        p1=pp
        if (ABS(dp).gt.1.d-20) t=t+dt/dp*(p-pp)
        i=i+1
      if (i.lt.20 .and. abs(dp).gt.1.d-5) goto 10
      RETURN
      end                                              !subroutine PTANC
c
c ======================================================================
c
      subroutine CSTAR (t,p,v,x,cs,ts,Ds,ps,ws,ierr,herr)
c
c  Calculate the critical flow factor, C*, for nozzle flow of a gas
c  (subroutine was originally named CCRIT)
c
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        v--plenum velocity [m/s] (should generally be set to 0 for
c                                  calculating stagnation conditions)
c        x--composition [array of mol frac]
c
c  outputs:
c       cs--critical flow factor [dimensionless]
c       ts--nozzle throat temperature [K]
c       Ds--nozzle throat molar density [mol/L]
c       ps--nozzle throat pressure [kPa]
c       ws--nozzle throat speed of sound [m/s]
c     ierr--error flag:  0 = successful
c                      200 = CSTAR did not converge
c                      201 = Final state may be 2-phase
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  11-05-02 EWL, original version
c  12-19-07 EWL, implement new method proposed by Aaron Johnson
c
cx DEC$ ATTRIBUTES DLLEXPORT :: CSTAR
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
c
      cs=0.d0
      ts=0.d0
      Ds=0.d0
      ps=0.d0
      ws=0.d0
      itmax=20
      if (p.le.0.d0) RETURN
      wmm=WMOL (x)
      call CRITP (x,tc,pc,dc,ierr,herr)
c     call TPRHO (t,p,x,2,0,ds,ierr,herr)
c     call THERM (t,ds,x,pp,e,h,s,cv,cp,ws,hjt)
      call TPFLSH (t,p,x,ds,Dl,Dv,xliq,xvap,q,e,h,s,cv,cp,ws,ierr,herr)
      i=0
      ts=t

c...old method used in version 8.0, which didn't always converge
c     dmax=dc
c     if (t.gt.tc) dmax=4.d0*dc
c     if (t.lt.0.95*tc .and. ds.lt.dc) dmax=dc/2.d0
c     dh=0.d0
c10   continue
c       i=i+1
c       dhold=dh
c       dh=(ws**2-v**2)/2.d0*wmm/1000.d0    !calculate change in enthalpy
c       hs=h-dh
c       ss=s
c       call HSFL1 (hs,ss,x,0.d0,dmax,ts,ds,ierr,herr)   !flow is assumed isentropic, ds=0
c       call THERM (ts,ds,x,ps,e,hs,ss,cv,cp,ws,hjt)
c     if (abs(dh-dhold).gt.1.d-4 .and. i.lt.20 .and. ierr.eq.0) goto 10

c...new method (which is about 10 times faster than the old method)
      dt=-1.d0
      eta=0.d0
 10   continue
        i=i+1
        t1=ts
        eta1=eta
        ss=s         !flow is assumed isentropic, ds=0
        ts=ts+dt
        dmax=dc
        dmin=0.d0
        if (ts.gt.tc) dmax=4.d0*dc
        if (ts.lt.0.95*tc .and. ds.lt.dc) dmax=dc/2.d0
        if (ts.lt.tc .and. ds.gt.dc) then
          dmax=4.d0*dc
          dmin=dc
          if (ts.lt.0.95*tc) dmin=dc*2.d0
        endif
        call TSFL1 (ts,ss,x,dmin,dmax,ds,ierr,herr)
        q=0
        if (ierr.gt.0) then
          kr=1
          call TSFLSH (ts,ss,x,kr,ps,ds,Dl,Dv,xliq,xvap,q,e,hs,cv,cp,ws,
     &                 ierr,herr)
        else
          call THERM (ts,ds,x,ps,e,hs,ss,cv,cp,ws,hjt)
        endif
        if (ierr.gt.0) RETURN
        if ((q.gt.0.d0 .and. q.lt.1.d0) .or. ws.le.0.d0
     &                                  .or. ps.le.0.d0) then
          ierr=201
          herr='[CSTAR error 201] final state may be 2-phase'
          call ERRMSG (ierr,herr)
          RETURN
        endif
        dh=(ws**2-v**2)/2.d0*wmm/1000.d0    !calculate change from input state in enthalpy
        eta=(hs+dh)/h-1.d0
        if (i.gt.1) dt=-eta/((eta1-eta)/(t1-ts))    !Newton's first order method
c       write (*,'(i3,f12.7,8f12.4)') ierr,dt,ss,ts,ds,dmin,dmax
        if (abs(eta).lt.1.d-8) then
c  See NASA Tech Note D-2565 by R.C. Johnson for C* equation
c  see also Stewart, D.G., Watson, J.T.R., Vaidya, A.M., Flow Measurement and Instrumentation, 10:27-34, 1999.
          cs=ds*ws*sqrt(t*r*wmm/1000.d0)/p
          RETURN
        endif
      if (i.lt.itmax) goto 10

      ierr=200
      herr='[CSTAR error 200] iteration failed to converge'
      call ERRMSG (ierr,herr)
      RETURN
      end                                              !subroutine CSTAR
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                      end file flash2.f
c ======================================================================
