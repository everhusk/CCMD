c  begin file utility.f
c
c  This file contains various utility subroutines to retrieve information
c  about the components.
c
c  contained here are:
c     subroutine INFO (icomp,wmm,ttrp,tnbpt,tc,pc,Dc,Zc,acf,dip,Rgas)
c     subroutine NAME (icomp,hnam,hn80,hcasn)
c     function WMOL (x)
c     subroutine XMASS (xmol,xkg,wmix)
c     subroutine XMOLE (xkg,xmol,wmix)
c     subroutine LIMITX (htyp,t,D,p,x,tmin,tmax,Dmax,pmax,ierr,herr)
c     subroutine LIMITK (htyp,icomp,t,D,p,tmin,tmax,Dmax,pmax,ierr,herr)
c     subroutine LIMITS (htyp,x,tmin,tmax,Dmax,pmax)
c     subroutine ERRMSG (ierr,herr)
c     subroutine QMASS (qmol,xl,xv,qkg,xlkg,xvkg,wliq,wvap,ierr,herr)
c     subroutine QMOLE (qkg,xlkg,xvkg,qmol,xl,xv,wliq,wvap,ierr,herr)
c     subroutine GOLD (x0i,x1i,ncc,lmax,z,z2,z3,z4,bt,xopt,yopt,ierr)
c     subroutine DOTFILL (x,ptest,filrat,ierr,herr)
c     function CBRTX (x)
c     subroutine MINP (n,xPnts,yPnts,xval,ierr,herr)
c     subroutine POLYN (n,xPnts,yPnts,xval,yval,ierr,herr)
c     subroutine LUDECOMP (n,maxn,aMatrix,cMatrix,ierr,herr)
c     subroutine PIVOT (n,maxn,j,iord,aMatrix,sdecomp)
c     subroutine CUBIC (a,z)
c     subroutine FITL (n,maxn,xPnts,yPnts,xval,yval,cmatrix,ierr,herr)
c     function UCASE (b,k)
c     subroutine IDCRV (d,x,t,ierr,herr)
c     subroutine BLCRV (d,x,t,ierr,herr)
c     subroutine JTCRV (d,x,t,ierr,herr)
c     subroutine JICRV (d,x,t,ierr,herr)
c     subroutine CSPLINE (n,xi,fi,coeff)
c     subroutine TRIDIAG (j,d,e,c,b,z)
c     subroutine SPLNROOT (isp,iderv,f,a,ierr,herr)
c     subroutine SPLNVAL (isp,iderv,a,f,ierr,herr)
c     subroutine CSPLNVAL (n,xi,fi,coeff,x,f,ierr,herr)
c     subroutine MINEIGEN (n,a,u,xlamda,ierr,herr)
c     subroutine PASSCMN (hvr,iset,icomp,jcomp,
c    &                    hstr,ilng,dbl,arr,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      subroutine INFO (icomp,wmm,ttrp,tnbpt,tc,pc,Dc,Zc,acf,dip,Rgas)
c
c  provides fluid constants for specified component
c
c  input:
c    icomp--component number in mixture; 1 for pure fluid
c  outputs:
c      wmm--molecular weight [g/mol]
c     ttrp--triple point temperature [K]
c    tnbpt--normal boiling point temperature [K]
c       tc--critical temperature [K]
c       pc--critical pressure [kPa]
c       Dc--critical density [mol/L]
c       Zc--compressibility at critical point [pc/(Rgas*Tc*Dc)]
c      acf--acentric factor [-]
c      dip--dipole moment [debye]
c     Rgas--gas constant [J/mol-K]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  01-31-97  MM, original version
c  02-19-97  MM, add check that input icomp is within bounds
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  03-08-00 EWL, change names of inputs to avoid conflicts with CCON
c
cx DEC$ ATTRIBUTES DLLEXPORT :: INFO
c
      include 'commons.for'
c
      if (abs(icomp).le.nc) then
        wmm=wmas(icomp)
        ttrp=ttpn(icomp)
        tnbpt=tnbp(icomp)
        tc=tcrit(icomp)
        pc=pcrit(icomp)
        Dc=Dcrit(icomp)
        Zc=Zcrit(icomp)
        acf=accen(icomp)
        dip=dipole(icomp)
        Rgas=R
      else
        wmm=0.0d0
        ttrp=0.0d0
        tnbpt=0.0d0
        tc=0.0d0
        pc=0.0d0
        Dc=0.0d0
        Zc=0.0d0
        acf=0.0d0
        dip=0.0d0
        Rgas=R
      end if
c
      RETURN
      end                                               !subroutine INFO
c
c ======================================================================
c
      subroutine NAME (icomp,hnam,hn80,hcasn)
c
c  provides name information for specified component
c
c  input:
c    icomp--component number in mixture; 1 for pure fluid
c  outputs:
c     hnam--component name [character*12]
c     hn80--component name--long form [character*80]
c    hcasn--CAS (Chemical Abstracts Service) number [character*12]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-06-97  MM, original version
c  02-19-97  MM, add check that input icomp is within bounds
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-97  MM, add synonyms to /CNAM80/
c
cx DEC$ ATTRIBUTES DLLEXPORT :: NAME
c
      include 'commons.for'
      character*12 hnam,hcasn
      character*80 hn80
c
      if (abs(icomp).le.nc) then
        hnam=hname(icomp)
        hn80=hnam80(icomp)(1:80)
        hcasn=hcas(icomp)
      else
        hnam='not defined'
        hn80='not defined                             '//
     &       '                                        '
        hcasn='not defined'
      end if
c
      RETURN
      end                                               !subroutine NAME
c
c ======================================================================
c
      function WMOL (x)
c
c  molecular weight for a mixture of specified composition
c
c  input:
c        x--composition array [array of mol frac]
c
c  output (as function value):
c     WMOL--molar mass [g/mol], a.k.a. "molecular weight"
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  01-10-96  MM, original version
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c                add Zcrit to common /CCON/
c  03-19-96  MM, add dipole moment to /CCON/
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-02-98 EWL, remove compositional dependence for pure fluids
c
cx DEC$ ATTRIBUTES DLLEXPORT :: WMOL
c
      include 'commons.for'
      dimension x(ncmax)
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
        WMOL=wmas(icomp)
      else
        wsum=0.0d0
        do i=1,nc
          wsum=wsum+x(i)*wmas(i)
        enddo
        WMOL=wsum
      endif
c
      RETURN
      end                                                 !function WMOL
c
c ======================================================================
c
      subroutine XMASS (xmol,xkg,wmix)
c
c  converts composition on a mole fraction basis to mass fraction
c
c  input:
c     xmol--composition array [array of mol frac]
c  outputs:
c      xkg--composition array [array of mass frac]
c     wmix--molar mass of the mixture [g/mol], a.k.a. "molecular weight"
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  04-08-96  MM, original version
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c
cx DEC$ ATTRIBUTES DLLEXPORT :: XMASS
c
      include 'commons.for'
      dimension xmol(ncmax),xkg(ncmax),xsumi(ncmax)
c
      call ISPURE (xmol,icomp)
      if (icomp.ne.0) then
        wmix=wmas(icomp)
        do i=1,nc
          xkg(i)=0.d0
        enddo
        xkg(icomp)=1.d0
        RETURN
      endif
      xsum=0.0d0
      do i=1,nc
        xsumi(i)=xmol(i)*wmas(i)
        xsum=xsum+xsumi(i)
      enddo
      wmix=xsum
      xsinv=1.d0
      if (ABS(xsum).gt.1.0d-10) xsinv=1.0d0/xsum
      do i=1,nc
        xkg(i)=xsumi(i)*xsinv
      enddo
c
      RETURN
      end                                              !subroutine XMASS
c
c ======================================================================
c
      subroutine XMOLE (xkg,xmol,wmix)
c
c  converts composition on a mass fraction basis to mole fraction
c
c  input:
c      xkg--composition array [array of mass frac]
c  outputs:
c     xmol--composition array [array of mol frac]
c     wmix--molar mass of the mixture [g/mol], a.k.a. "molecular weight"
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  04-08-96  MM, original version
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  10-24-02 EWL, set xmol=1 for a pure fluid
c
cx DEC$ ATTRIBUTES DLLEXPORT :: XMOLE
c
      include 'commons.for'
      dimension xmol(ncmax),xkg(ncmax),xsumi(ncmax)
c
      call ISPURE (xkg,icomp)
      if (icomp.ne.0) then
        wmix=wmas(icomp)
        do i=1,nc
          xmol(i)=0.d0
        enddo
        xmol(icomp)=1.d0
        RETURN
      endif
      xsum=0.0d0
      wsum=0.0d0
      do i=1,nc
        xsumi(i)=xkg(i)/wmas(i)
        xsum=xsum+xsumi(i)
      enddo
      wmix=xsum
      xsinv=1.d0
      if (ABS(xsum).gt.1.0d-10) xsinv=1.0d0/xsum
      do i=1,nc
        xmol(i)=xsumi(i)*xsinv
        wsum=wsum+xmol(i)*wmas(i)
      enddo
      wmix=wsum
c
      RETURN
      end                                              !subroutine XMOLE
c
c ======================================================================
c
      subroutine LIMITX (htyp,t,D,p,x,tmin,tmax,Dmax,pmax,ierr,herr)
c
c  returns limits of a property model as a function of composition
c  and/or checks input t, D, p against those limits
c
c  Pure fluid limits are read in from the .fld files; for mixtures, a
c  simple mole fraction weighting in reduced variables is used.
c
c  Attempting calculations below the minimum temperature and/or above
c  the maximum density will result in an error.  These will often
c  correspond to a physically unreasonable state; also many equations of
c  state do not extrapolate reliably to lower T's and higher D's.
c
c  A warning is issued if the temperature is above the maximum but below
c  1.5 times the maximum; similarly pressures up to twice the maximum
c  result in only a warning. Most equations of state may be
c  extrapolated to higher T's and P's.  Temperatures and/or pressures
c  outside these extended limits will result in an error.
c
c  When calling with an unknown temperature, set t to -1 to avoid performing
c  the melting line check
c
c  inputs:
c     htyp--flag indicating which models are to be checked [character*3]
c           'EOS':  equation of state for thermodynamic properties
c           'ETA':  viscosity
c           'TCX':  thermal conductivity
c           'STN':  surface tension
c        t--temperature [K]
c        D--molar density [mol/L]
c        p--pressure [kPa]
c        x--composition array [mol frac]
c     N.B.--all inputs must be specified, if one or more are not
c           available, (or not applicable as in case of surface tension)
c           use reasonable values, such as:
c           t = tnbp
c           D = 0
c           p = 0
c  outputs:
c     tmin--minimum temperature for model specified by htyp [K]
c     tmax--maximum temperature [K]
c     Dmax--maximum density [mol/L]
c     pmax--maximum pressure [kPa]
c     ierr--error flag:  0 = all inputs within limits
c                      <>0 = one or more inputs outside limits:
c                       -1 = 1.5*tmax > t > tmax
c                        1 = t < tmin or t > 1.5*tmax
c                        2 = D > Dmax or D < 0
c                       -4 = 2*pmax > p > pmax
c                        4 = p < 0 or p > 2*pmax
c                        8 = component composition < 0 or > 1
c                            and/or composition sum < 0 or > 1
c                       16 = p>pmelt
c                      -16 = t<ttrp (important for water)
c           if multiple inputs are outside limits, ierr = abs[sum(ierr)]
c           with the sign determined by the most severe excursion
c           (ierr > 0 indicate an error--calculations not possible,
c            ierr < 0 indicate a warning--results may be questionable)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  05-30-96  MM, original version
c  06-03-96  MM, p > 2*pmax and t > 1.5*tmax result in error
c                add htyp to argument list
c  02-21-97  MM, add checks for viscosity and thermal conductivity
c  06-03-97  MM, initialize ierr = 0 and herr = hnull
c  06-04-97 EWL, zero delsum and xsum before do loop
c  06-17-97  MM, change format on xsum error
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-05-97  MM, x(i) missing in summation for max density
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-01-98 EWL, reduce Tmin for mixtures by 5 degrees
c  12-01-98 EWL, reduce Tmin by 1.0d-10 to avoid machine precision problems
c  12-22-98 EWL, add ammonia-water triple-point line
c  03-05-99 EWL, add checks for melting line pressure and sublimation pressure
c  09-02-99 EWL, increase check for p>pmelt by pmelt+.0001
c  10-12-99 EWL, add transport limits for the ECS model
c  10-20-99 EWL, change herrx to 140 to accommodate full string
c  10-20-99 EWL, increased the tolerance in sum(x)<>1 for setup with single prec.
c  10-22-99 EWL, call new subroutine LIMITS to get tmin,tmax,dmax, and pmax
c  01-25-00 EWL, do not allow p>pmax for parahydrogen (bad Younglove EOS)
c  01-25-00 EWL, do not allow t>tmax for krypton (bad Juza EOS)
c  02-25-00 EWL, do not check p>pmax when t=-1
c  05-25-00 EWL, reorganize ltemp logic
c  05-25-00 EWL, remove old code for calculating pmlt
c  07-11-00 EWL, remove check on t>tmax for krypton, equation was replaced
c  11-20-01 EWL, check for t<ttrp
c  11-20-01 EWL, allow mixtures to go below ttrp by 25 degrees
c  06-30-04 EWL, allow mixtures to go below ttrp by 25 degrees only if Tc's
c                differ by more than 50 (thus, air is not included)
c  03-28-06 EWL, allow p>pmax for parahydrogen with new Leachman equation
c  10-05-07  HH, remove common block WLMETA
c
cx DEC$ ATTRIBUTES DLLEXPORT :: LIMITX
c
      include 'commons.for'
      logical lerr,lwarn,ltemp
      dimension x(ncmax)
      character*3 htyp
      character*75 herrt,herrd,herrp
      character*140 herrx
      character*255 herr
c
c  initialize flags and strings
c
c     write (*,*) ' LIMITX--entering with htyp,t = ',htyp,t
      ierr=0
      ierrt=0
      ierrd=0
      ierrp=0
      ierrx=0
      xsum=0.0d0
      herr=' '
      herrt=' '
      herrd=' '
      herrp=' '
      herrx=' '
      nchart=1
      nchard=1
      ncharp=1
      ncharx=1
      lerr=.false.
      lwarn=.false.
      pmlt=1.0d15
      call ISPURE (x,icomp)
c
      call LIMITS(htyp,x,tmin,tmax,Dmax,pmax)
      if (ABS(p).gt.1.0d-10 .and. t.gt.0) call MELTT(t,x,pmlt,ierr,herr)
c
c  Set EOS and transport routines so that they cannot be extrapolated
c  to lower temps
      ltemp=.true.
      if (htyp.eq.'STN' .or. htyp.eq.'stn') then
        ltemp=.false. !surface tension may be extrapolated to lower T's
      end if
c
      if (icomp.eq.0) then
c  general mixture case
        xsum=0.0d0
        do i=1,nc
          xsum=xsum+x(i)
          if (x(i).lt.-1.0d-10 .or. x(i).gt.1.0000000001d0) then
            lerr=.true.
            ierrx=8
          end if
        enddo
        if (xsum.lt.0.999999d0 .or. xsum.gt.1.000001d0) then
          lerr=.true.
          ierrx=8
        end if
      end if
c
c  check inputs against limits
c
      if (t.lt.tmin-1.0d-10 .and. abs(t+1.0d0).gt.1.d-15) then
        pm=0
        ierr=0
        tsub=0.d0
        if (icomp.ne.0) then
          if (t.lt.ttpn(icomp)) call SUBLT (t,x,pm,ierr,herr)
        else
c  set error number if p>pmelt.  If p=0, also set error number
          do i=1,nc-1
            do j=i+1,nc
              if (abs(tcrit(i)-tcrit(j)).gt.50) tsub=25.d0
            enddo
          enddo
        endif
        if (tsub.gt.0.d0) then
          call REDX (x,tred,Dred)
          if (tred.lt.200) tsub=5.d0
        endif
        if (ierr.ne.0 .or. p.gt.pm+1.0d-10 .or. p.lt.1.d-20) then
          if (icomp.ne.0 .or. t.lt.tmin-tsub) then
            if (ltemp) then
              lerr=.true.
              ierrt=1
            else
              lwarn=.true.
              ierrt=-1
            end if
            write (herrt,1010) t,tmin
            nchart=72
 1010       format (' temperature below lower limit, T =',g12.5,
     &              ' K, Tmin =',g12.5,' K;')
          endif
        endif
      else if (t.gt.1.5d0*tmax) then
        lerr=.true.
        ierrt=1
        write (herrt,1011) t,tmax
        nchart=74
 1011   format (' temperature > 1.5 x upper limit, T =',g12.5,
     &          ' K, Tmax =',g12.5,' K;')
      else if (t.gt.tmax) then
c       if (hcas(icomp).eq.'7439-90-9' .and. icomp.ne.0) then
c         lerr=.true.
c         ierrt=1
c       else
          lwarn=.true.
          ierrt=-1
c       endif
        write (herrt,1012) t,tmax
        nchart=72
 1012   format (' temperature above upper limit, T =',g12.5,
     &          ' K, Tmax =',g12.5,' K;')
      end if
      if (D.lt.0.0d0) then
        lerr=.true.
        ierrd=2
        write (herrd,1020) D
        nchard=37
 1020   format (' density < 0, D =',g12.5,' mol/L;')
      else if (D.gt.Dmax) then
        lerr=.true.
        ierrd=2
        write (herrd,1021) D,Dmax
        nchard=72
 1021   format (' density > upper limit, D =',g12.5,
     &          ' mol/L, Dmax =',g12.5,' mol/L;')
      end if
      if (p.lt.0.0d0) then
        lerr=.true.
        ierrp=4
        write (herrp,1040) p/1000.0d0
        ncharp=35
 1040   format (' pressure < 0, P =',g12.5,' MPa;')
      else if (p.gt.2.0d0*pmax) then
        lerr=.true.
        ierrp=4
        write (herrp,1042) p/1000.0d0,pmax/1000.0d0
        ncharp=73
 1042   format (' pressure > 2 x upper limit, P =',g12.5,
     &          ' MPa, Pmax =',g12.5,' MPa;')
      else if (p.gt.pmax+1.0d-8) then
        lwarn=.true.
        ierrp=-4
        write (herrp,1044) p/1000.0d0,pmax/1000.0d0
        ncharp=73
 1044   format (' pressure above upper limit, P =',g12.5,
     &          ' MPa, Pmax =',g12.5,' MPa;')
      end if
      if (p*0.9995d0.gt.pmlt .and. abs(pmlt).gt.1.d-20) then
        lerr=.true.
        ierrp=16
        write (herrp,1050) p/1000.0d0,pmlt/1000.0d0
        ncharp=74
 1050   format (' pressure > melting pressure, P =',g12.5,
     &          ' MPa, Pmelt =',g12.5,' MPa;')
      end if
      if (ierrx.ne.0) then
c       write (*,1080) xsum,(x(i),i=1,nc)
        write (herrx,1080) xsum,(x(i),i=1,MIN(nc,5))
        ncharx=131
 1080   format (' composition(s) out of range, Xsum =',f13.10,
     &          ' mol frac, X(i) =',5f13.10)
      end if
      if (icomp.ne.0) then
        if (t.lt.ttpn(icomp)-1.d-10 .and. ierrt.eq.0.and.t.gt.0.d0) then
c  check for cases where the temperature is less than the triple point
c  temperature, but still in a valid liquid region (like water between
c  251.165 and 273.16 K.)
          lwarn=.true.
          ierrt=-16
          write (herrt,1090) t,ttpn(icomp)
          nchart=73
 1090     format (' temperature less than triple point,',
     &          ' T=',g12.5,' K, Ttrp=',g12.5,' K;')
          if (hcas(icomp).eq.'7732-18-5' .and. abs(p).gt.1.d-20) then
            call MLTH2O (t,p1,p2)
            if (p.lt.p2*0.9999d0 .or. p.gt.p1/0.9999d0) then
              lerr=.true.
              ierrt=16
              write (herrt,1095)
              nchart=34
 1095   format (' inputs are within the solid phase;')
            endif
          endif
        endif
      endif
c
c  compose error string and compute overall value of ierr
c
      if (lerr .or. lwarn) then
c       write (*,*) ' LIMITX--nchart,d,p,x:',nchart,nchard,nchard,ncharx
c       write (*,1999) ' LIMITX--herrt: ',herrt(1:nchart)
c       write (*,1999) ' LIMITX--herrd: ',herrd(1:nchard)
c       write (*,1999) ' LIMITX--herrp: ',herrp(1:ncharp)
c       write (*,1999) ' LIMITX--herrx: ',herrx(1:ncharx)
c1999   format (1x,a16,a80)
c       write (*,*) ' LIMITX--#char: t,d,p,x,sum:  ',nchart,nchard,
c    &              ncharp,ncharx,nchart+nchard+ncharp+ncharx
        herr='one or more inputs are out of range: '//herrt(1:nchart)
     &      //herrd(1:nchard)//herrp(1:ncharp)//herrx(1:ncharx)
     &      //hnull
        if (lerr) then
          ierr=abs(ierrt)+abs(ierrd)+abs(ierrp)+abs(ierrx)
        else if (lwarn) then
          ierr=-abs(ierrt)-abs(ierrd)-abs(ierrp)-abs(ierrx)
        end if
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                             !subroutine LIMITX
c
c ======================================================================
c
      subroutine LIMITK (htyp,icomp,t,D,p,tmin,tmax,Dmax,pmax,ierr,herr)
c
c  returns limits of a property model (read in from the .fld files) for
c  a mixture component and/or checks input t, D, p against those limits
c
c  This routine functions in the same manner as LIMITX except that the
c  composition x is replaced by the component number icomp.
c
c  Attempting calculations below the minimum temperature and/or above
c  the maximum density will result in an error.  These will often
c  correspond to a physically unreasonable state; also many equations of
c  state do not extrapolate reliably to lower T's and higher D's.
c
c  A warning is issued if the temperature is above the maximum but below
c  1.5 times the maximum; similarly pressures up to twice the maximum
c  result in only a warning. Most equations of state may be
c  extrapolated to higher T's and P's.  Temperatures and/or pressures
c  outside these extended limits will result in an error.
c
c  inputs:
c     htyp--flag indicating which models are to be checked [character*3]
c           'EOS':  equation of state for thermodynamic properties
c           'ETA':  viscosity
c           'TCX':  thermal conductivity
c           'STN':  surface tension
c    icomp--component number in mixture; 1 for pure fluid
c        t--temperature [K]
c        D--molar density [mol/L]
c        p--pressure [kPa]
c     N.B.--all inputs must be specified, if one or more are not
c           available, (or not applicable as in case of surface tension)
c           use reasonable values, such as:
c           t = tnbp
c           D = 0
c           p = 0
c  outputs:
c     tmin--minimum temperature for model specified by htyp [K]
c     tmax--maximum temperature [K]
c     Dmax--maximum density [mol/L]
c     pmax--maximum pressure [kPa]
c     ierr--error flag:  0 = all inputs within limits
c                      <>0 = one or more inputs outside limits:
c                       -1 = 1.5*tmax > t > tmax
c                        1 = t < tmin or t > 1.5*tmax
c                        2 = D > Dmax or D < 0
c                       -4 = 2*pmax > p > pmax
c                        4 = p < 0 or p > 2*pmax
c                       16 = p>pmelt
c                      -16 = t<ttrp (important for water)
c           if multiple inputs are outside limits, ierr = abs[sum(ierr)]
c           with the sign determined by the most severe excursion
c           (ierr > 0 indicate an error--calculations not possible,
c            ierr < 0 indicate a warning--results may be questionable)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  03-18-97  MM, original version; based on LIMITX
c  06-03-97  MM, initialize ierr = 0 and herr = hnull
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-01-98 EWL, reduce Tmin by 1.0d-10 to avoid machine precision problems
c  10-12-99 EWL, add transport limits for the ECS model
c  10-22-99 EWL, add checks for melting line pressure and sublimation pressure
c  08-16-00 EWL, add dimension x(ncmax)
c  11-20-01 EWL, check for t<ttrp
c
cx DEC$ ATTRIBUTES DLLEXPORT :: LIMITK
c
      include 'commons.for'
      include 'comtrn.for'
      logical lerr,lwarn,ltemp
      character*3 htyp
      character*75 herrt,herrd,herrp
      character*120 herrx
      character*255 herr
c
c  initialize flags and strings
c
c     write (*,*) ' LIMITK--entering with htyp,t = ',htyp,t
      ierr=0
      ierrt=0
      ierrd=0
      ierrp=0
      herr=' '
      herrt=' '
      herrd=' '
      herrp=' '
      herrx=' '
      nchart=1
      nchard=1
      ncharp=1
      ncharx=1
      lerr=.false.
      lwarn=.false.
      i=icomp
c
      pmlt=0.0d0
      if (htyp.eq.'EOS' .or. htyp.eq.'eos') then
c  equation of state
        ltemp=.true.       !EOS cannot be extrapolated to lower temps
        tmin=tmeos(i)
        tmax=txeos(i)
        Dmax=Deos(i)
        pmax=peos(i)
        if (d.gt.dtpn(icomp) .or. ABS(d).lt.1.0d-10) then
          if (t.gt.tmeos(icomp).and.ABS(p).gt.1.0d-10) then
            call MELTK (icomp,t,pmlt,ierr,herr)
          end if
        endif
      else if (htyp.eq.'STN' .or. htyp.eq.'stn') then
c  surface tension model
        ltemp=.false.     !STN may be extrapolated to lower temps
        tmin=tminst(i)
        tmax=tmaxst(i)
        Dmax=-9.99d99     !density and pressure limits not applicable
        pmax=-9.99d99     !for surface tension--set to large number
      else if (htyp.eq.'TCX' .or. htyp.eq.'tcx') then
c  thermal conductivity
        ltemp=.true.    !transport cannot be extrapolated to lower temps
        tmin=tmtcx(i)
        tmax=txtcx(i)
        Dmax=Dxtcx(i)
        pmax=pxtcx(i)
      else if (htyp.eq.'ETA' .or. htyp.eq.'eta') then
c  viscosity
        ltemp=.true.    !transport cannot be extrapolated to lower temps
        tmin=tmeta(i)
        tmax=txeta(i)
        Dmax=Dxeta(i)
        pmax=pxeta(i)
      else
c  unknown model specification--use EOS limits
        ltemp=.true.
        tmin=tmeos(i)
        tmax=txeos(i)
        Dmax=Deos(i)
        pmax=peos(i)
      end if
c
c  check inputs against limits
c
      if (t.lt.tmin-1.0d-10 .and. abs(t).gt.1.d-20) then
        ierr=0
        if (i.ne.0) call SUBLK (icomp,t,pm,ierr,herr)
c  set error number if p>pmelt.  If p=0, also set error number
        if (ierr.ne.0 .or. p.gt.pm+1.0d-10 .or. p.lt.1.d-20) then
          if (ltemp) then
            lerr=.true.
            ierrt=1
          else
            lwarn=.true.
            ierrt=-1
          end if
          write (herrt,1010) t,tmin
          nchart=72
 1010     format (' temperature below lower limit, T =',g12.5,
     &            ' K, Tmin =',g12.5,' K;')
        endif
      else if (t.gt.1.5d0*tmax) then
        lerr=.true.
        ierrt=1
        write (herrt,1011) t,tmax
        nchart=74
 1011   format (' temperature > 1.5 x upper limit, T =',g12.5,
     &          ' K, Tmax =',g12.5,' K;')
      else if (t.gt.tmax) then
c       if (hcas(icomp).eq.'7439-90-9') then
c         lerr=.true.
c         ierrt=1
c       else
          lwarn=.true.
          ierrt=-1
c       endif
        write (herrt,1012) t,tmax
        nchart=72
 1012   format (' temperature above upper limit, T =',g12.5,
     &          ' K, Tmax =',g12.5,' K;')
      end if
      if (D.lt.0.0d0) then
        lerr=.true.
        ierrd=2
        write (herrd,1020) D
        nchard=37
 1020   format (' density < 0, D =',g12.5,' mol/L;')
      else if (D.gt.Dmax) then
        lerr=.true.
        ierrd=2
        write (herrd,1021) D,Dmax
        nchard=72
 1021   format (' density > upper limit, D =',g12.5,
     &          ' mol/L, Dmax =',g12.5,' mol/L;')
      end if
      if (p.lt.0.0d0) then
        lerr=.true.
        ierrp=4
        write (herrp,1040) p/1000.0d0
        ncharp=35
 1040   format (' pressure < 0, P =',g12.5,' MPa;')
      else if (p.gt.2.0d0*pmax) then
        lerr=.true.
        ierrp=4
        write (herrp,1042) p/1000.0d0,pmax/1000.0d0
        ncharp=73
 1042   format (' pressure > 2 x upper limit, P =',g12.5,
     &          ' MPa, Pmax =',g12.5,' MPa;')
      else if (p.gt.pmax+1.0d-8) then
        if (hcas(icomp).eq.'1333-74-0p') then
          lerr=.true.
          ierrp=4
        else
          lwarn=.true.
          ierrp=-4
        endif
        write (herrp,1044) p/1000.0d0,pmax/1000.0d0
        ncharp=73
 1044   format (' pressure above upper limit, P =',g12.5,
     &          ' MPa, Pmax =',g12.5,' MPa;')
      end if
      if (p*0.9995d0.gt.pmlt .and. abs(pmlt).gt.1.d-20) then
        lerr=.true.
        ierrp=16
        write (herrp,1050) p/1000.0d0,pmlt/1000.0d0
        ncharp=74
 1050   format (' pressure > melting pressure, P =',g12.5,
     &          ' MPa, Pmelt =',g12.5,' MPa;')
      end if
c
      if (icomp.ne.0) then
        if (t.lt.ttpn(icomp)-1.d-10 .and. ierrt.eq.0.and.t.gt.0.d0) then
c  check for cases where the temperature is less than the triple point
c  temperature, but still in a valid liquid region (like water between
c  251.165 and 273.16 K.)
          lwarn=.true.
          ierrt=-16
          write (herrt,1090) t,ttpn(icomp)
          nchart=73
 1090     format (' temperature less than triple point,',
     &          ' T=',g12.5,' K, Ttrp=',g12.5,' K;')
          if (hcas(icomp).eq.'7732-18-5' .and. abs(p).gt.1.d-20) then
            call MLTH2O (t,p1,p2)
            if (p.lt.p2 .or. p.gt.p1) then
              lerr=.true.
              ierrt=16
              write (herrt,1095)
              nchart=34
 1095   format (' inputs are within the solid phase;')
            endif
          endif
        endif
      endif
c
c  compose error string and compute overall value of ierr
c
      if (lerr .or. lwarn) then
c       write (*,*) ' LIMITK--nchart,d,p,x:',nchart,nchard,nchard,ncharx
c       write (*,1999) ' LIMITK--herrt: ',herrt(1:nchart)
c       write (*,1999) ' LIMITK--herrd: ',herrd(1:nchard)
c       write (*,1999) ' LIMITK--herrp: ',herrp(1:ncharp)
c       write (*,1999) ' LIMITK--herrx: ',herrx(1:ncharx)
c1999   format (1x,a16,a80)
c       write (*,*) ' LIMITK--#char: t,d,p,x,sum:  ',nchart,nchard,
c    &              ncharp,ncharx,nchart+nchard+ncharp+ncharx
        herr='one or more inputs are out of range: '//herrt(1:nchart)
     &      //herrd(1:nchard)//herrp(1:ncharp)//herrx(1:ncharx)
     &      //hnull
        if (lerr) then
          ierr=abs(ierrt)+abs(ierrd)+abs(ierrp)
        else if (lwarn) then
          ierr=-abs(ierrt)-abs(ierrd)-abs(ierrp)
        end if
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                             !subroutine LIMITK
c
c ======================================================================
c
      subroutine LIMITS (htyp,x,tmin,tmax,Dmax,pmax)
c
c  returns limits of a property model as a function of composition
c
c  Pure fluid limits are read in from the .fld files; for mixtures, a
c  simple mole fraction weighting in reduced variables is used.
c
c  inputs:
c     htyp--flag indicating which models are to be checked [character*3]
c           'EOS':  equation of state for thermodynamic properties
c           'ETA':  viscosity
c           'TCX':  thermal conductivity
c           'STN':  surface tension
c        x--composition array [mol frac]
c  outputs:
c     tmin--minimum temperature for model specified by htyp [K]
c     tmax--maximum temperature [K]
c     Dmax--maximum density [mol/L]
c     pmax--maximum pressure [kPa]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  10-22-99 EWL, original version
c  04-21-08 EWL, do not subtract 5 K if tmin<10 K
c  02-28-13 EWL, add dmax = dens(Ttrp) for htyp='TRP'
c
cx DEC$ ATTRIBUTES DLLEXPORT :: LIMITS
c
      include 'commons.for'
      include 'comtrn.for'
      dimension x(ncmax)
      dimension tred(ncmax),Dred(ncmax)
      dimension tmn(ncmax),tmx(ncmax),Dmx(ncmax),pmx(ncmax)
      character*3 htyp
c
      if (htyp.eq.'EOS' .or. htyp.eq.'eos') then
c  equation of state
        do i=1,nc
          tmn(i)=tmeos(i)
          tmx(i)=txeos(i)
          Dmx(i)=Deos(i)
          pmx(i)=peos(i)
        enddo
      elseif (htyp.eq.'TRP' .or. htyp.eq.'trp') then
c  equation of state but with maximum density equal to the triple point density
        do i=1,nc
          tmn(i)=tmeos(i)
          tmx(i)=txeos(i)
          pmx(i)=peos(i)
          Dmx(i)=Deos(i)
          if (dtpn(i).gt.0.1d0) Dmx(i)=dtpn(i)
        enddo
      else if (htyp.eq.'STN' .or. htyp.eq.'stn') then
c  surface tension model
        do i=1,nc
          tmn(i)=tminst(i)
          tmx(i)=tmaxst(i)
          Dmx(i)=9.99d99     !density and pressure limits not applicable
          pmx(i)=9.99d99     !for surface tension--set to large number
c         write (*,*) ' LIMITX-STN; i,Tmin,Tmax: ',i,tmn(i),tmx(i)
        enddo
      else if (htyp.eq.'TCX' .or. htyp.eq.'tcx') then
c  thermal conductivity
        do i=1,nc
          tmn(i)=tmtcx(i)
          tmx(i)=txtcx(i)
          Dmx(i)=Dxtcx(i)
          pmx(i)=pxtcx(i)
          if (htcx(i).eq.'ECS') then
            tmn(i)=tmecst(i)
            tmx(i)=txecst(i)
            Dmx(i)=Dxecst(i)
            pmx(i)=pxecst(i)
          endif
        enddo
      else if (htyp.eq.'ETA' .or. htyp.eq.'eta') then
c  viscosity
        do i=1,nc
          tmn(i)=tmeta(i)
          tmx(i)=txeta(i)
          Dmx(i)=Dxeta(i)
          pmx(i)=pxeta(i)
          if (heta(i).eq.'ECS') then
            tmn(i)=tmecst(i)
            tmx(i)=txecst(i)
            Dmx(i)=Dxecst(i)
            pmx(i)=pxecst(i)
          endif
        enddo
      else
c  unknown model specification--use EOS limits
        do i=1,nc
          tmn(i)=tmeos(i)
          tmx(i)=txeos(i)
          Dmx(i)=Deos(i)
          pmx(i)=peos(i)
        enddo
      end if
c
      call ISPURE (x,icomp)
      if (icomp.ne.0) then
c  special case--pure component
        tmin=tmn(icomp)
        tmax=tmx(icomp)
        Dmax=Dmx(icomp)
        pmax=pmx(icomp)
        if (hcas(icomp).eq.'7732-18-5' .and. tmin.gt.251.165d0)
     &      tmin=251.165d0
      else
c  general mixture case
        taumin=0.0d0
        taumax=0.0d0
        delmax=0.0d0
        pmax=0.0d0
        do i=1,nc
          tred(i)=tz(i)
          Dred(i)=rhoz(i)
          if (tmn(i).gt.0.d0) taumin=taumin+x(i)*tred(i)/tmn(i)
          if (tmx(i).gt.0.d0) taumax=taumax+x(i)*tred(i)/tmx(i)
          if (Dred(i).gt.0.d0) delmax=delmax+x(i)*Dmx(i)/Dred(i)
          pmax=pmax+x(i)*pmx(i)
        enddo
        call REDX (x,trmix,Drmix)
        Dmax=Drmix*delmax
        if (taumin.le.0.d0) taumin=100.d0
        if (taumax.le.0.d0) taumax=300.d0
        if (taumax.gt.0.d0) tmax=trmix/taumax
        if (taumin.gt.0.d0) then
          tmin=trmix/taumin
          if (tmin.gt.10.d0) tmin=tmin-5.d0
        endif
        if (tmin.lt.0.d0) tmin=100.d0
        if (tmax.lt.0.d0) tmax=300.d0
        if (iamwat.gt.0) then
          i=iamwat
          if (x(i).lt.0.33367d0) then
            tmin=273.16d0*(1.0d0-0.3439823d0*x(i)
     &          -1.3274271d0*x(i)**2-274.973d0*x(i)**7)
          elseif (x(i).lt.0.58396d0) then
            tmin=193.549d0*(1.0d0-4.987368d0*(x(i)-0.5d0)**2)
          elseif (x(i).lt.0.81473d0) then
            tmin=194.380d0*(1.0d0-4.886151d0*(x(i)-2.0d0/3.0d0)**2
     &          +10.37298d0*(x(i)-2.0d0/3.0d0)**3)
          else
            tmin=195.495d0*(1.0d0-0.323998d0*(1.0d0-x(i))
     &          -15.87560d0*(1.0d0-x(i))**4)
          endif
        endif
      end if
c
      RETURN
      end                                             !subroutine LIMITS
c ======================================================================
c
      subroutine ERRMSG (ierr,herr)
c
c  write error messages to default output; this subroutine should be
c  called immediately after any call to a subroutine which potentially
c  can error out
c
c  inputs:
c     ierr--error flag:  0 = successful (no message will be written)
c                       <0 = warning
c                       >0 = error
c     herr--error string (character*255 variable)
c
c  outputs:
c     if iprnterr in common block prnterr is equal to zero:
c     error string written to default output
c
c     if iprnterr is equal to 1:
c     error string written to screen if ierr is positive
c
c     if iprnterr is equal to -1:
c     error string written to screen
c
c     if iprnterr is equal to 3, -3:
c     same as 1, -1, but program also pauses
c
c     Note:  no information should be written to the screen
c     when compiling the DLL.
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  09-11-95  MM, original version
c  07-30-96  MM, also write out the value of ierr
c  03-26-97 EWL, change name ERROR --> ERRMSG as 'error' is a standard
c                routine in Lahey Fortran90
c  10-02-97  MM, add compiler switch to allow access by DLL
c  12-01-98 EWL, add common block prterr
c  12-01-98 EWL, don't print extra spaces at end of herr
c  10-21-99 EWL, remove use of i outside of do loop
c  10-22-99 EWL, remove extra spaces before "K", "MPa", etc.
c  11-17-99  MM, new variable for printed message

cx DEC$ ATTRIBUTES DLLEXPORT :: ERRMSG
c
      include 'commons.for'
      character*255 herr,hout
      character*4 hstrg
c
      j=0
 100  continue
      i=index(herr,'   K')
      if (i.eq.0) i=index(herr,'   MPa')
      if (i.eq.0) i=index(herr,'   mol')
      if (i.eq.0) i=index(herr,'   J/mol')
      if (i.ne.0) then
        j=j+1
        herr=herr(1:i)//herr(i+3:255)
        if (j.lt.10) goto 100
      endif
      i=index(herr,'=0')
      if (i.ne.0) then
        j=j+1
        herr=herr(1:i)//' '//herr(i+1:255)
        if (j.lt.10) goto 100
      endif
      i=index(herr,';   ')
      if (i.ne.0) herr=herr(1:i-1)//herr(i+1:255)
      i=index(herr,'; '//hnull)
      if (i.ne.0) herr=herr(1:i-1)//herr(i+1:255)
      i=index(herr,';  '//hnull)
      if (i.ne.0) herr=herr(1:i-1)//herr(i+1:255)
c
      if (ierr.ne.0 .and. iprnterr.ne.0) then
        do i=255,1,-1
          if (herr(i:i).ne.' ' .and. herr(i:i).ne.hnull) then
            ilast=i
            goto 110
          end if
        enddo
        ilast=1
 110    continue
        write (hstrg,'(i4)') ierr
        if (ierr.gt.0 .or. iprnterr.lt.0) then
          hout=herr(1:ilast)//' (ierr='//hstrg//')'
          write (*,1000) hout
c         if (ABS(iprnterr).ge.2)  call error('Called')
c         if (ABS(iprnterr).ge.3)  pause
        end if
      end if
 1000 format (1x,a255)
c
      RETURN
      end                                             !subroutine ERRMSG
c
c ======================================================================
c
      subroutine QMASS (qmol,xl,xv,qkg,xlkg,xvkg,wliq,wvap,ierr,herr)
c
c  converts quality and composition on a mole basis to a mass basis
c
c  inputs:
c     qmol--molar quality [moles vapor/total moles]
c           qmol = 0 indicates saturated liquid
c           qmol = 1 indicates saturated vapor
c           0 < qmol < 1 indicates a two-phase state
c           qmol < 0 or qmol > 1 are not allowed and will result in warning
c       xl--composition of liquid phase [array of mol frac]
c       xv--composition of vapor phase [array of mol frac]
c  outputs:
c      qkg--quality on mass basis [mass of vapor/total mass]
c     xlkg--mass composition of liquid phase [array of mass frac]
c     xvkg--mass composition of vapor phase [array of mass frac]
c     wliq--molecular weight of liquid phase [g/mol]
c     wvap--molecular weight of vapor phase [g/mol]
c     ierr--error flag:  0 = all inputs within limits
c           -19:  input q < 0 or > 1
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-23-98  MM, original version, based on XMASS
c
cx DEC$ ATTRIBUTES DLLEXPORT :: QMASS
c
      include 'commons.for'
      dimension xl(ncmax),xv(ncmax),xlkg(ncmax),xvkg(ncmax)
      character*255 herr
      parameter (eps=1.0d-8)
c
      call XMASS (xl,xlkg,wliq)
      call XMASS (xv,xvkg,wvap)
      if (qmol.lt.-eps .or. qmol.gt.1.0d0+eps) then
        ierr=-19
        herr='[QMASS warning 19] input quality out of range'//hnull
        call ERRMSG (ierr,herr)
        qkg=qmol
      else
        ierr=0
        herr=' '
        qkg=qmol*wvap/(qmol*wvap+(1.0d0-qmol)*wliq)
      end if
c
      RETURN
      end                                              !subroutine QMASS
c
c ======================================================================
c
      subroutine QMOLE (qkg,xlkg,xvkg,qmol,xl,xv,wliq,wvap,ierr,herr)
c
c  converts quality and composition on a mass basis to a molar basis
c
c  inputs:
c      qkg--quality on mass basis [mass of vapor/total mass]
c           qkg = 0 indicates saturated liquid
c           qkg = 1 indicates saturated vapor
c           0 < qkg < 1 indicates a two-phase state
c           qkg < 0 or qkg > 1 are not allowed and will result in warning
c     xlkg--mass composition of liquid phase [array of mass frac]
c     xvkg--mass composition of vapor phase [array of mass frac]
c  outputs:
c     qmol--quality on mass basis [mass of vapor/total mass]
c       xl--molar composition of liquid phase [array of mol frac]
c       xv--molar composition of vapor phase [array of mol frac]
c     wliq--molecular weight of liquid phase [g/mol]
c     wvap--molecular weight of vapor phase [g/mol]
c     ierr--error flag:  0 = all inputs within limits
c           -19:  input q < 0 or > 1
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  02-23-98  MM, original version, based on XMOLE
c
cx DEC$ ATTRIBUTES DLLEXPORT :: QMOLE
c
      include 'commons.for'
      dimension xl(ncmax),xv(ncmax),xlkg(ncmax),xvkg(ncmax)
      character*255 herr
      parameter (eps=1.0d-8)
c
      call XMOLE (xlkg,xl,wliq)
      call XMOLE (xvkg,xv,wvap)
      if (qkg.lt.-eps .or. qkg.gt.1.0d0+eps) then
        ierr=-19
        herr='[QMOLE warning 19] input quality out of range'//hnull
        call ERRMSG (ierr,herr)
        qmol=qkg
      else
        ierr=0
        herr=' '
        qmol=qkg/wvap/(qkg/wvap+(1.0d0-qkg)/wliq)
      end if
c
      RETURN
      end                                              !subroutine QMOLE
c
c ======================================================================
c
      subroutine GOLD (x0i,x1i,ncc,lmax,z,z2,z3,z4,bt,xopt,yopt,ierr)
c
c  This subroutine carries out a Fibonnaci (golden) search
c  technique to locate an extremum (maximum or minimum) in a
c  function within a specified interval.
c
c  inputs:
c      x0i--lower bound of interval containing extremum
c      x1i--upper bound of interval containing extremum
c      ncc--number of function evaluations carried out;
c           the extremum is located with an interval of size:
c           (x1i - x0i)*0.618**ncc
c           (e.g. 29 evaluations required to reduce interval to 10**-6
c           of its original size)
c     lmax--logical flag; if lmax =
c           .true. -  locate maximum value
c           .false. - locate minimum value
c        z--composition array
c       z2--additional independent variable
c       z3--additional independent variable
c       z4--additional independent variable
c       bt--function type, 'H'-call ENTHAL, etc.
c  outputs:
c     xopt--location of extremum
c     yopt--value of function at extremum
c     ierr--integer output from external function (e.g. error flag)
c
c  taken from:
c    McLinden, M.O. (1988). Working fluid selection for space-based
c    two-phase heat transport systems. National Bureau of Standards,
c    NBSIR 88-3812.
c  by M. McLinden, NIST Chemical & Physical Properties Div, Boulder, CO
c  09-05-97  MM, transcribed from original reference, add additional comments
c                add z,z2,z3,z4 for passing to GEVAL
c  03-26-98  MM, make z an array (e.g. for passing composition)
c  01-19-01 EWL, remove external dependence
c
      include 'commons.for'
      logical lmax
      dimension z(ncmax)
      character*1 bt
      parameter (GR=0.61803398875d0)
c
c  routine always finds a maximum, so to locate a minimum multiply by -1
      ierr=0
      y38=0.d0
      if (lmax) then
        xmax=1.0d0
      else
        xmax=-1.0d0
      end if
      x0=x0i  !x0 stores the lower bound of interval containing extremum
      x1=x1i  !x0 stores the upper bound
      x62=x0+GR*(x1-x0)          !x62 is 62% of way across interval
      if (bt.eq.'H') then
        call ENTHAL (z2,x62,z,b)
      elseif (bt.eq.'E') then
        call ENERGY (z2,x62,z,b)
      elseif (bt.eq.'S') then
        call ENTRO (z2,x62,z,b)
      endif
      y62=xmax*b
c
      do ig=1,ncc
        x38=x0+(1.0d0-GR)*(x1-x0)  !x38 is 38% of way across interval
        if (bt.eq.'H') then
          call ENTHAL (z2,x38,z,b)
        elseif (bt.eq.'E') then
          call ENERGY (z2,x38,z,b)
        elseif (bt.eq.'s') then
          call ENTRO (z2,x38,z,b)
        endif
        y38=xmax*b
        if (y62.lt.y38) then
c  keep sub-interval containing maximum value of GEVAL
          x1=x62
          x62=x38
          y62=y38
        else
          x0=x1
          x1=x38
        end if
      enddo
c
      xopt=0.5d0*(x0+x1)
      yopt=xmax*MAX(y38,y62)
c
      RETURN
      end                                               !subroutine GOLD
c
c ======================================================================
c
      function UCASE (b,k)
c
c  Make all the characters in the string b uppercase, from the first
c  character to character k
c
      include 'commons.for'
      character ucase*255, a*255, b*(*)
c
      a=b
      do i=1,k
        j=ichar(a(i:i))
        if (j.gt.96 .and. j.le.122) j=j-32
        a(i:i)=char(j)
      enddo
      UCASE=a
      RETURN
      end                                                !function UCASE
c
c ======================================================================
c
      subroutine DOTFILL (x,ptest,filrat,ierr,herr)
c
c  Calculate filling ratio according to UN P200 document
c  Packing instructions for hazardous substances
c
c  Input
c    x         composition, mol fraction
c    ptest     test pressure, absolute kPa  (may also be output variable)
C
c  Output
c    filrat    filling ratio
c    ierr      error flag
c    herr      error message
c
c  Version 0.0   10.07.05
c
cx DEC$ ATTRIBUTES DLLEXPORT :: DOTFILL
c
      include 'commons.for'
      dimension x(ncmax),xliq(ncmax),xvap(ncmax),y(ncmax),z(ncmax)
      character herr*255
c
      herr=' '
      ierr=0
      rho0=999.10262d0 !kg/m3 at 15C, 0.101325 MPa (water)
      filrat=0.0d0
      ione=1
      ttest65=65.0d0+273.15d0
      ttest60=60.0d0+273.15d0
      ttest50=50.0d0+273.15d0

      if (iunflg(1).ne.1) then !do not allow calculations
        filrat=0.0d0
        ptest= 0.0d0
        ierr=1
        herr='User defined calculations not permitted for this fluid'
      else
c  obtain critical temperature to determine if fluid is high or low pressure
c  and get molecular weight of mixture
        call ISPURE (x,icomp)
        if (icomp.ne.0) then
          tcc=tcrit(icomp)
          wmx=wmas(icomp)
        else
          call CRTHMX (x,tcc,pcmix,Dcmix,ierr,herr)
          wmx=WMOL(x)
        endif
        tcc=tcc-273.15d0 !convert to C
c  get the saturation pressure at 65 C
        call SATT (ttest65,x,ione,psat65,dl,dv,xliq,xvap,ierr,herr)
c
c  begin procedure for low pressure fluids (tc<65C)
c  for these fluids, there is only one choice for test pressure- Psat at 65C
        if (tcc.gt.65.d0) then
c  obtain the density at 50C and saturation
          call SATT (ttest50,x,ione,p,dl,dv,xliq,xvap,ierr,herr)
          rhotest=0.95d0*dl !mol/L
c  perform first check; get sat liquid rho at 60C
          call SATT (ttest60,x,ione,p,dl,dv,xliq,xvap,ierr,herr)
          rhof60=dl !mol/L
c  select the lower of these two values as the fill density
          rhofill=MIN(rhof60,rhotest)
          rhofill=rhofill*wmx  !convert to kg/m3
          if (rho0.gt.0.d0) filrat=rhofill/rho0
          ptest=psat65 !kPa,abs remember to change to gauge for final results
        else
c  begin procedure for High pressure fluids (tc>65C)
          call TPFLSH (ttest65,ptest,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
     &                 ierr,herr)
          rhof=d*wmx    !convert to kg/m3
          if (rho0.gt.0.d0) filrat=rhof/rho0
        endif
c
c     adjust to match accepted un values
        if (ntrmun(1).gt.0) then
          if (icomp.ne.0) filrat=filrat*prmUN(icomp,icomp)
        endif  !won't work for mixtures, must revise
c
c  test pressure must exceed saturation pressure at 65C
        if (ptest.lt.psat65) then
          ierr=-1
          herr='Test pressure must exceed saturation pressure at 65 C'
          ptest=psat65
          filrat=-999
        endif
      endif
      RETURN
      end                                            !subroutine DOTFILL
c
c ======================================================================
c
      function CBRTX (x)
c
c  cube root function--allows negative arguments
c
c  input:
c        x--value to be acted upon
c  output (as function value):
c     CBRTX--cube root of x
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  08-25-97 MM, original version
c  11-06-01 MLH, changed name to CBRTX to be standard conforming.
c  09-29-04 MLH, fixed bug for neg arguments
c
      include 'commons.for'
c
      xa=ABS(x)
      xathrd=xa**thrd
      if (x.ge.0.0d0) then
        cbrtx=xathrd
      else
        cbrtx=-xathrd
      end if
c
      RETURN
      end                                                !function CBRTX
c
c ======================================================================
c
      subroutine MINP (n,xPnts,yPnts,xval,ierr,herr)
c
c  Find the minimum of a parabola.  If n=2, return x where y=0.
c
c  inputs:
c          n--number of data points
c      xPnts--array of n data points for the x-axis data
c      yPnts--array of n data points for the y-axis data
c  outputs:
c       xval--value of x where y is a minimum
c       ierr--error flag:   0 = successful
c       herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  05-18-10 EWL, original version
c
      include 'commons.for'
      dimension xPnts(3),yPnts(3)
      character herr*255
c
      ierr=0
      herr=' '
      nn=n
      xval=0.d0
 10   continue
      if (nn.eq.2) then
        if (yPnts(1).eq.yPnts(2)) then
          xval=xval/2.d0
          RETURN
        endif
        xval=xPnts(1)-yPnts(1)/(yPnts(1)-yPnts(2))*(xPnts(1)-xPnts(2))
      elseif (nn.eq.3) then
        xw=xPnts(2)-xPnts(3)
        xx=xPnts(3)-xPnts(1)
        xy=xPnts(1)-xPnts(2)
        If (xw*xx*xy.eq.0) goto 999
        xw=-(yPnts(1)*xw+yPnts(2)*xx+yPnts(3)*xy)/(xw*xx*xy)
        If (xy.eq.0 .or. xw.eq.0) goto 999
        xx=(yPnts(1)-yPnts(2))/xy-xw*(xPnts(1)+xPnts(2))
        xval=-xx/(2*xw)
c     Curvature is negative, so get root using linear solution
        if (xw.lt.0) then
          ierr=2
          herr='[MINP error 2] curvature of parabola is negative'//hnull
          goto 999
        endif
      else
        ierr=1
        herr='[MINP error 1] invalid inputs'//hnull
      endif
      if (xval.gt.100000) xval=100000
      if (xval.lt.-100000) xval=-100000
      RETURN
 999  continue
      nn=2
      goto 10
      end                                               !subroutine MINP
c
c ======================================================================
c
      subroutine POLYN (n,xPnts,yPnts,xval,yval,ierr,herr)
c
c  Calculate the polynomial given the n data points and return the value
c  yval at xval.
c
c  inputs:
c          n--number of data points
c      xPnts--array of n data points for the x-axis data
c      yPnts--array of n data points for the y-axis data
c       xval--value of x
c  outputs:
c       yval--value of y at x
c       ierr--error flag:   0 = successful
c       herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  11-28-12 EWL, original version
c
      include 'commons.for'
      dimension xPnts(*),yPnts(*),fdd(20,20)
      character herr*255
c
      ierr=0
      herr=' '
      yval=0.d0
      if (n.le.1 .or. n.gt.20) RETURN

      fdd(1:n,1)=ypnts(1:n)
      do j=2,n
        do i=1,n-j+1
          fdd(i,j)=0.d0
          xdiff=xpnts(i+j-1)-xpnts(i)
          if (abs(xdiff).gt.1.d-20)
     &        fdd(i,j)=(fdd(i+1,j-1)-fdd(i,j-1))/xdiff
        enddo
      enddo
      xt=1
      fa=fdd(1,1)
      do i=1,n-1
        xt=xt*(xval-xpnts(i))
        fa=fa+fdd(1,i+1)*xt
      enddo
      yval=fa
      RETURN
      end                                              !subroutine POLYN
c
c ======================================================================
c
      subroutine LUDECOMP (n,maxn,aMatrix,cMatrix,ierr,herr)
c
c  Use Lower-Upper decomposition to invert a 2-dimensional
c  array and solve for the roots, where [A][B]=[C]
c
c  inputs:
c          n--number of elements to solve
c       maxn--array size
c    aMatrix--input array [A] of size (maxn,maxn)
c    cMatrix--input array [C] of size (maxn)
c
c  outputs:
c    cMatrix--output array [B] of size (maxn)
c       ierr--error flag:   0 = successful
c       herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-01-92 EWL, original version
c  12-18-12 EWL, reverse order of Jacobian matrix
c
      include 'commons.for'
      dimension amatrix(maxn,maxn),cmatrix(maxn)
      dimension iord(100),ctemp(maxn),sdecomp(maxn)
      character herr*255
      common/iord/iord
c
      if (ierr.eq.-190) then
        iord(1)=1
        iord(2)=2
        iord(3)=3
        goto 10
      endif
      if (ierr.eq.-191) goto 10
      ierr=0
      herr=' '
      do i=1,n
        iord(i)=i
        sdecomp(i)=Abs(aMatrix(i,1))
        do j=2,n
          if (Abs(aMatrix(i,j)).gt.sdecomp(i))
     &        sdecomp(i)=Abs(aMatrix(i,j))
        enddo
C     Singular matrix
        if (sdecomp(i).eq.0) then
          ierr=1
          herr='[LUdecomp error 1] singular matrix'//hnull
          RETURN
        endif
      enddo

      j=1
      Call PIVOT(n,maxn,j,iord,aMatrix,sdecomp)
      if (aMatrix(iord(1),1).eq.0) then
        ierr=1
        RETURN
      endif
      do j=2,n
        aMatrix(iord(1),j)=aMatrix(iord(1),j)/aMatrix(iord(1),1)
      enddo
      do j=2,n-1
        do i=j,n
          sum=0
          do k=1,j-1
            sum=sum+aMatrix(iord(i),k)*aMatrix(iord(k),j)
          enddo
          aMatrix(iord(i),j)=aMatrix(iord(i),j)-sum
        enddo
        Call PIVOT(n,maxn,j,iord,aMatrix,sdecomp)
        do k=j+1,n
          sum=0
          do i=1,j-1
            sum=sum+aMatrix(iord(j),i)*aMatrix(iord(i),k)
          enddo
          If (aMatrix(iord(j),j).eq.0) aMatrix(iord(j),j)=1E+20
          aMatrix(iord(j),k)=(aMatrix(iord(j),k)-sum)/aMatrix(iord(j),j)
        enddo
      enddo
      sum=0
      do k=1,n-1
        sum=sum+aMatrix(iord(n),k)*aMatrix(iord(k),n)
      enddo
      aMatrix(iord(n),n)=aMatrix(iord(n),n)-sum
      If (aMatrix(iord(n),n).eq.0) aMatrix(iord(n),n)=1E+20

 10   continue
      ierr=0
      cMatrix(iord(1))=cMatrix(iord(1))/aMatrix(iord(1),1)
      do i=2,n
        sum=0
        do j=1,i-1
          sum=sum+aMatrix(iord(i),j)*cMatrix(iord(j))
        enddo
        cMatrix(iord(i))=(cMatrix(iord(i))-sum)/aMatrix(iord(i),i)
      enddo

      do i=n-1,1,-1
        sum=0
        do j=i+1,n
          sum=sum+aMatrix(iord(i),j)*cMatrix(iord(j))
        enddo
        cMatrix(iord(i))=cMatrix(iord(i))-sum
      enddo
      do i=1,n
        ctemp(i)=cMatrix(iord(i))
      enddo
      do i=1,n
        cMatrix(i)=ctemp(i)
      enddo
      end                                           !subroutine LUDECOMP
c
c ======================================================================
c
      subroutine PIVOT (n,maxn,j,iord,aMatrix,sdecomp)
c
c  Part of the LUdecomp routine
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-01-92 EWL, original version
c
      include 'commons.for'
      dimension amatrix(maxn,maxn),iord(100),sdecomp(maxn)
c
      ipivt=j
      big=Abs(aMatrix(iord(j),j)/sdecomp(iord(j)))
      do i=j+1,n
        dummy=Abs(aMatrix(iord(i),j)/sdecomp(iord(i)))
        If (dummy.gt.big) then
          big=dummy
          ipivt=i
        endif
      enddo
      idummy=iord(ipivt)
      iord(ipivt)=iord(j)
      iord(j)=idummy
      end                                              !subroutine PIVOT
c
c ======================================================================
c
      subroutine CUBIC (a,z)
c
c     General purpose routine to solve a cubic equation of the form
c       z**3 + a(3)*z**2 +a(2)*z + a(1) = 0
c
c     The smallest root is returned in z(1) and the largest in z(2).
c     The middle root, if it exists, is discarded.
c
c  written by NIST Thermophysics Division, Boulder, Colorado
c  09-24-04 original version
c  05-17-11 EWL, change r to rr to avoid conflicts with gcnst
c
      include 'commons.for'
      dimension a(3),z(3)
c
c  All of the digits are necessary for liquid densities with pressures very close to zero
      pi=3.14159265358979323846d0
      add1=2.d0*pi/3.d0
      add2=2.d0*add1
      a0=a(1)
      a1=a(2)
      a2=a(3)/3.d0
      q=a1/3.d0-a2*a2
      rr=(a1*a2-a0)/2.d0-a2*a2*a2
      test=q*q*q+rr*rr
      if (test.lt.0.d0) then       ! three distinct roots
        test=SQRT(-test)
        q=2.d0*CBRTX(SQRT(rr*rr+test*test))
        theta=pi
        if (abs(rr).gt.1.d-20) theta=ATAN(test/rr)
        if (theta.lt.0.d0) theta=theta+pi
        theta=theta/3.d0
        z(1)=q*COS(theta)-a2
        z(2)=q*COS(theta+add1)-a2
        z(3)=q*COS(theta+add2)-a2
      else                          ! either one real, or three real w/ 2 identical
        test=SQRT(test)
        s1=CBRTX(rr+test)
        s2=CBRTX(rr-test)
        z(1)=s1+s2-a2
        z(2)=z(1)
        z(3)=z(1)
        if (test.gt.0.d0) RETURN   ! only one real
        z(2)=-0.5d0*(s1+s2)-a2
        z(3)=z(2)                   ! three real, but 2 identical
      endif
      zmin=MIN(z(1),z(2),z(3))
      z(2)=MAX(z(1),z(2),z(3))
      z(1)=zmin
      if (zmin.lt.0.d0) z(1)=z(2)
      RETURN
      end                                              !subroutine CUBIC
c
c ======================================================================
c
      subroutine FITL (n,maxn,xPnts,yPnts,xval,yval,cmatrix,ierr,herr)
c
c  Linearly fit an equation to (x,y) data and return the value
c  at a specified x.
c
c  inputs:
c         n--number of data points
c      maxn--amatrix and cmatrix array sizes
c     xPnts--array of n data points for the x-axis data
c     yPnts--array of n data points for the y-axis data
c      xval--value of x where y will be calculated
c  outputs:
c      yval--value of y at xval
c   cmatrix--coefficients of the fit
c      ierr--error flag:   0 = successful
c      herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  07-06-11 EWL, original version
c
      include 'commons.for'
      dimension xPnts(*),yPnts(*)
      dimension amatrix(maxn,maxn),cmatrix(maxn)
      character herr*255
c
      ierr=0
      herr=' '
      yval=0.d0
      if (n.eq.1) then
        yval=yPnts(1)
      elseif (n.eq.2) then
        slope=(yPnts(1)-yPnts(2))/(xPnts(1)-xPnts(2))
        b=yPnts(1)-slope*xPnts(1)
        yval=slope*xval+b
      elseif (n.gt.2 .and. n.le.50) then
        do i=1,n
          do j=1,n
            aMatrix(i,j)=xPnts(i)**(j-1.d0)
          enddo
          cMatrix(i)=yPnts(i)
        enddo
        call LUdecomp (n,maxn,aMatrix,cMatrix,ierr,herr)
        yval=0.d0
        do j=1,n
          yval=yval+cMatrix(j)*xval**(j-1.d0)
        enddo
      else
        ierr=1
        herr='[FITL error 1] invalid inputs'//hnull
      endif
      RETURN
      end                                               !subroutine FITL
c
c ======================================================================
c
      subroutine IDCRV (d,x,t,ierr,herr)
      include 'commons.for'
      dimension x(ncmax)
      character herr*255
      ierr=0
      herr=' '
c
      z=1
      if (nc.eq.1) then
        j=1
      else
        j=0
        if (x(1).eq.1) j=1
        if (x(itwo2).eq.1) j=2
      endif
      if (j.eq.0) then
        ierr=1
        herr='IDCRV:  valid only for pure fluids'
        RETURN
      endif
      if (t.eq.0) t=tz(j)*1.5

c  New method using the slope dz/dT to get quick convergence with dT
      i=0
 10   i=i+1
      call PRESS (t,d,x,p)
      z=p/d/r/t-1.d0
      tau=tz(j)/t
      del=d/rhoz(j)
      da11=phik(j,1,1,tau,del)
      dzdt=-da11/t
      if (i.gt.1000 .or. dzdt.eq.0.d0) then
        t=0
        RETURN
      endif
      t=t-z/dzdt
      if (ABS(z).gt.1d-8) goto 10
      end                                              !subroutine IDCRV
c
c ======================================================================
c
      subroutine BLCRV (d,x,t,ierr,herr)
      include 'commons.for'
      dimension x(ncmax)
      character herr*255
      ierr=0
      herr=' '
c
      func=0
      if (nc.eq.1) then
        j=1
      else
        j=0
        if (x(1).eq.1) j=1
        if (x(itwo2).eq.1) j=2
      endif
      if (j.eq.0) stop 'BL:  not a pure fluid'
      if (t.le.0) t=tz(j)*1.5d0
      i=0
 10   i=i+1
      del=d/rhoz(j)
      tau=tz(j)/t
      phi01=PHIK(j,0,1,tau,del)
      phi02=PHIK(j,0,2,tau,del)
      phi11=PHIK(j,1,1,tau,del)
      phi12=PHIK(j,1,2,tau,del)
      func=phi01+phi02
      dfdt=-(phi11+phi12)/t
      if (i.gt.100 .or. dfdt.eq.0.d0) then
        t=0
        RETURN
      endif
      t=t-func/dfdt
      if (ABS(func).gt.1d-8) goto 10
      end                                              !subroutine BLCRV
c
c ======================================================================
c
      subroutine JTCRV (d,x,t,ierr,herr)
      include 'commons.for'
      dimension x(ncmax)
      character herr*255
      ierr=0
      herr=' '
c
      if (nc.eq.1) then
        j=1
      else
        j=0
        if (x(1).eq.1) j=1
        if (x(itwo2).eq.1) j=2
      endif
      if (j.eq.0) stop 'JTCRV:  not a pure fluid'
      if (t.eq.0) t=tz(j)*1.5d0

      i=0
 10   i=i+1
      if (t.le.0) RETURN
      del=d/rhoz(j)
      tau=tz(j)/t
      phi01=PHIK(j,0,1,tau,del)
      phi02=PHIK(j,0,2,tau,del)
      phi11=PHIK(j,1,1,tau,del)
      phi12=PHIK(j,1,2,tau,del)
      phi21=PHIK(j,2,1,tau,del)
c  Only the top part in the calculation of hjt is required, the other
c  parts do not go to zero and thus do not contribute to finding JT=0
      hjt=phi01+phi02+phi11
c  The derivative of phi11 with respect to T results in phi21+phi11,
c  thus the '2.d0*' is required below
      dfdt=-(2.d0*phi11+phi12+phi21)/t
      if (i.gt.500 .or. dfdt.eq.0.d0) then
        t=0
        RETURN
      endif
      t=t-hjt/dfdt
      if (ABS(hjt).gt.1d-8) goto 10
      end                                              !subroutine JTCRV
c
c ======================================================================
c
      subroutine JICRV (d,x,t,ierr,herr)
      include 'commons.for'
      dimension x(ncmax)
      character herr*255
      ierr=0
      herr=' '
c
      if (nc.eq.1) then
        j=1
      else
        j=0
        if (x(1).eq.1) j=1
        if (x(itwo2).eq.1) j=2
      endif
      if (j.eq.0) stop 'JICRV:  not a pure fluid'
      if (t.le.0) t=tz(j)*10

      i=0
      del=d/rhoz(j)
 10   i=i+1
      tau=tz(j)/t
c  JI -->    d(Z)/d(T) at constant rho
c     --->   del*d^2(alphar)/d(del)/d(T)
c     ----> -del*tau*d^2(alphar)/d(del)/d(tau)/T (can ingore the /T for finding JI=0)
      func =PHIK(j,1,1,tau,del)
c  d(JI)/dT  --> tau**2*del*d^3(alphar)/d(del)/d(tau)**2/T**2
c               (one of the /T must be removed to match the one removed in func)
      dfdt=-PHIK(j,2,1,tau,del)/t
      if (i.gt.500 .or. dfdt.eq.0) then
        t=0
        RETURN
      endif
      t=t-func/dfdt
      if (ABS(func).gt.1d-8 .and. t.gt.0) goto 10
      end                                              !subroutine JICRV
c
c ======================================================================
c
      subroutine CSPLINE (n,xi,fi,coeff)
c
c  cubic-spline approximation
c
c  inputs:
c        n --number of segments (nodes-1)
c     xi[n]--abscissa values of nodes
c     fi[n]--ordinate values of nodes
c
c  outputs:
c  coeff[n]--spline coefficients (second-order derivatives)
c
c  written by Matthias Kunick, NIST Thermophysics Division, Boulder, Colorado
c  09-15-11  MK, original version
c
      include 'commons.for'
      dimension xi(1000),fi(1000),coeff(1000)
      dimension g(1000),h(1000),d(1000),b(1000),c(1000)
c
c  assign the intervals and function differences
      do i=1,n
        h(i)=xi(i+1)-xi(i)
        g(i)=fi(i+1)-fi(i)
      enddo
c
c  evaluate the coefficient matrix elements
      do i=1,n-1
        d(i)=2.d0*(h(i+1)+h(i))
        b(i)=0.d0
        if (h(i).ne.0.d0 .and. h(i+1).ne.0.d0)
     &  b(i)=6.d0*(g(i+1)/h(i+1)-g(i)/h(i))
        c(i)=h(i+1)
      enddo
c
c  obtain the second-order derivatives
      call TRIDIAG (n-1,d,c,c,b,g)
      coeff(1)=0.d0
      coeff(n+1)=0.d0
      do i=2,n
        coeff(i)=g(i-1)
      enddo
c
      end                                            !subroutine CSPLINE
c
c ======================================================================
c
      subroutine TRIDIAG (j,d,e,c,b,z)
c
c  function to solve the tridiagonal linear equation set.
c
c  written by Matthias Kunick, NIST Thermophysics Division, Boulder, Colorado
c  09-15-11  MK, original version
c
      include 'commons.for'
      dimension d(1000),e(1000),c(1000),b(1000)
      dimension z(1000),y(1000),w(1000),v(1000),t(1000)
c
c  evaluate the elements in the LU decomposition
      w(1)=d(1)
      v(1)=c(1)
      t(1)=0.d0
      if (w(1).ne.0.d0) t(1)=e(1)/w(1)
      do i=2,j-1
        w(i)=d(i)-v(i-1)*t(i-1)
        v(i)=c(i)
        t(i)=0.d0
        if (w(i).ne.0.d0) t(i)=e(i)/w(i)
      enddo
      w(j)=d(j)-v(j-1)*t(j-1)
c
c  forward substitution to obtain y
      y(1)=0.d0
      if (w(1).ne.0.d0) y(1)=b(1)/w(1)
      do i=2,j
        y(i)=0.d0
        if (w(i).ne.0.d0) y(i)=(b(i)-v(i-1)*y(i-1))/w(i)
      enddo
c
c  backward substitution to obtain z
      z(j)=y(j)
      do i=j-1,1,-1
        z(i)=y(i)-t(i)*z(i+1)
      enddo
c
      end                                            !subroutine TRIDIAG
c
c ======================================================================
c
      subroutine CSPLNVAL (n,xi,fi,coeff,x,f,ierr,herr)
c
c  calculates the function value of a spline function.
c
c  inputs:
c       n --number of nodes
c    xi[n]--abscissa values of nodes
c    fi[n]--ordinate values of nodes
c coeff[n]--spline coefficients
c       x --independent variable (abscissa)
c  outputs:
c        f--function value (ordinate)
c     ierr--error flag:   0 = successful
c                       192 = x<xi(1) or x>xi(n)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by Matthias Kunick, NIST Thermophysics Division, Boulder, Colorado
c  09-15-11  MK, original version
c
      include 'commons.for'
      dimension xi(1000),fi(1000),coeff(1000)
      character*255 herr
c
      ierr=0
      herr=' '
      f=0.d0
c
      if (x.lt.xi(1) .or. x.gt.xi(n)) then
        ierr=192
        write (herr,1001) xi(1),x,xi(n)
        call ERRMSG (ierr,herr)
 1001   format ('[CSPLNVAL error 192] ',
     &          'x is out of range! xi(1)=', g12.5,
     &          ' x=',g12.5,' xi(n) =',g12.5)
        RETURN
      endif
c
      k=1
      dx=x-xi(1)
      do while (dx.ge.0.d0 .and. k.lt.n-1)
        k=k+1
        dx=x-xi(k)
      enddo
      k=k-1
      if (k.le.1) k=2
c
      dx=xi(k+1)-xi(k)
      if (dx.eq.0.) RETURN
      alpha=coeff(k+1)/(6.d0*dx)
      beta =-coeff(k)/(6.d0*dx)
      gamma=fi(k+1)/dx-dx*coeff(k+1)/6.d0
      eta  =dx*coeff(k)/6.d0-fi(k)/dx
      f    =alpha*(x-xi(k))*(x-xi(k))*(x-xi(k))
     &     +beta*(x-xi(k+1))*(x-xi(k+1))*(x-xi(k+1))
     &     +gamma*(x-xi(k))+eta*(x-xi(k+1))
c
      end                                           !subroutine CSPLNVAL
c
c ======================================================================
c
      subroutine SPLNVAL (isp,iderv,a,f,ierr,herr)
c
c  calculates the function value of a spline
c
c  inputs:
c      isp--indicator for which spline to use (1-nc: composition,
c           nc+1: temperature, nc+2: pressure, nc+3: density,
c           nc+4: enthalpy, nc+5: entropy)
c    iderv--values of -1 and -2 return lower and upper root values,
c           value of 0 returns spline function, value of 1 returns
c           derivative of spline function with respect to density
c        a--root value
c  outputs:
c        f--value of spline function at input root
c     ierr--error flag:   0 = successful
c     herr--error string (character*255)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  09-20-11 EWL, original version
c  11-27-12 EWL, add enthalpy and entropy
c
      include 'commons.for'
      dimension xPnts(1000),yPnts(1000),coefs(1000)
      character herr*255
      f=300
      if (narr.lt.2) then
        ierr=1
        herr='[SPLNVAL error 1] number of points set to zero'
        RETURN
      endif
      do i=1,narr
        xPnts(i)=xarr(  0,i)
        yPnts(i)=xarr(isp,i)
        if (isp.gt.0) coefs(i)=carr(isp,i)
      enddo
      if (iderv.eq.-1) then
        if (xPnts(1).lt.100.) f=exp(xPnts(1))
      elseif (iderv.eq.-2) then
        if (xPnts(narr).lt.100.) f=exp(xPnts(narr))
      elseif (iderv.eq.0) then
        if (a.le.0) RETURN
        call CSPLNVAL(narr,xPnts,yPnts,coefs,log(a),b,ierr,herr)
        f=b
        if (isp.le.nc+3 .and. b.lt.100.) f=exp(b)   !All values except h and s are on a log basis
      else
        da=0.001d0  !Numberical derivative with dx=0.001
        if (a-da.gt.0) then
          call CSPLNVAL(narr,xPnts,yPnts,coefs,log(a+da),bp,ierr,herr)
          call CSPLNVAL(narr,xPnts,yPnts,coefs,log(a-da),bn,ierr,herr)
        else
          call CSPLNVAL(narr,xPnts,yPnts,coefs,log(a+2.d0*da),bp,
     &                  ierr,herr)
          call CSPLNVAL(narr,xPnts,yPnts,coefs,log(a),bn,ierr,herr)
        endif
        if (da.ne.0.) then
          if (isp.le.nc+3) then
            if (bp.lt.100 .and. bn.lt.100) f=(exp(bp)-exp(bn))/(2.d0*da)
          else
            f=(bp-bn)/(2.d0*da)
          endif
        endif
      endif
      end                                            !subroutine SPLNVAL
c
c ======================================================================
c
      subroutine SPLNROOT (isp,iderv,f,a,ierr,herr)
c
c  calculates the root of a given value of a spline function
c
c  inputs:
c      isp--indicator for which spline to use (1-nc: composition,
c           nc+1: temperature, nc+2: pressure, nc+3: density,
c           nc+4: enthalpy, nc+5: entropy)
c    iderv--values of -1 and -2 return lower and upper root values,
c           value of 0 returns spline root, value of 1 returns
c           root where derivative of spline with respect to rho is equal to the
c           the value of f (set f=0 to find maximum or minimum)
c        f--value of spline function
c  outputs:
c        a--root value (initial value required since some splines can be doubled valued)
c     ierr--error flag:   0 = successful
c     herr--error string (character*255)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  09-20-11 EWL, original version
c
      include 'commons.for'
      character herr*255
c  get upper and lower bounds
      call SPLNVAL(isp,-1,a,alow,ierr,herr)
      call SPLNVAL(isp,-2,a,aupp,ierr,herr)
      a1=a
      a2=a1+0.1d0
      a3=a2
      call SPLNVAL(isp,iderv,a1,f1,ierr,herr)
      f2=1.d8
      j=0
      if (narr.lt.2) goto 20

c  setup bounds for false-position if needed
      al=alow
      au=aupp
      call SPLNVAL(isp,iderv,al,fl,ierr,herr)
      call SPLNVAL(isp,iderv,au,fu,ierr,herr)
      fl=fl-f
      fu=fu-f
      if (fl*fu.lt.0.d0 .and. fu.lt.fl) then
        ff=fu
        fu=fl
        fl=ff
        aa=au
        au=al
        al=aa
      endif

      do while (abs(f2).gt.1.d-7)
        call SPLNVAL(isp,iderv,a2,f2,ierr,herr)
        f2=f2-f
        if (f2.gt.fl .and. f2.lt.0.d0) then
          al=a2
          fl=f2
        elseif (f2.lt.fu .and. f2.gt.0.d0) then
          au=a2
          fu=f2
        endif
        j=j+1
        if (ierr.gt.0 .or. j.gt.100 .or. abs(f1-f2).lt.1.d-20) goto 10
        da=-f2*(a1-a2)/(f1-f2)
        a1=a2
        f1=f2
c  don't allow more than a 10% change
        if (abs(da).gt.abs(a1/10.d0)) da=da/10.d0
        if (abs(da).gt.abs(a1/10.d0)) da=da/10.d0
        if (abs(da).gt.abs(a1/10.d0)) da=da/10.d0
        a2=a1+da
        if (a2.lt.alow .or. a2.gt.aupp) a2=a1+da/2.d0
        if (a2.lt.alow .or. a2.gt.aupp) a2=a1+da/5.d0
        if (a2.lt.alow .or. a2.gt.aupp) a2=a1+da/10.d0
        if (a2.lt.alow .or. a2.gt.aupp) a2=a1+da/25.d0
        if (a2.lt.alow .or. a2.gt.aupp) a2=a1+da/100.d0
        if (a2.lt.alow .or. a2.gt.aupp) a2=a1+da/1000.d0
      enddo
      a=a2
      RETURN

 10   continue
c  iteration failed, try false-position
      a3=a2
      if (fl*fu.ge.0.d0) then
        a2=au/4.d0
        call SPLNVAL(isp,iderv,a2,f2,ierr,herr)
        f2=f2-f
        if (a.lt.a2) then
          au=a2
          fu=f2
        else
          al=a2
          fl=f2
        endif
      endif
      if (fl*fu.lt.0.d0) then
        j=0
        f2=fu
        a2=(al+au)/2.d0
        do while (abs(f2).gt.1.d-7)
          call SPLNVAL(isp,iderv,a2,f2,ierr,herr)
          f2=f2-f
          if (f2.gt.fl .and. f2.lt.0.d0) then
            al=a2
            fl=f2
            a2=au-fu*(al-au)/(fl-fu)
          elseif (f2.lt.fu .and. f2.gt.0.d0) then
            au=a2
            fu=f2
            a2=au-fu*(al-au)/(fl-fu)
          elseif (j.eq.0) then
            a2=0.9*al+0.1*au
          elseif (j.eq.1) then
            a2=0.1*al+0.9*au
          else
            goto 20
          endif
          j=j+1
          if (ierr.gt.0 .or. j.gt.200) goto 20
        enddo
        a=a2
        RETURN
      endif

20    continue
      ierr=1
      a=alow
      if (a3.gt.aupp) a=aupp
      end                                           !subroutine SPLNROOT
c
c ======================================================================
c
      subroutine MINEIGEN (n,a,u,xlamda,ierr,herr)
c
c  determine the smallest eigenvalue of the input array
c
c  inputs:
c         n--number of elements to solve
c         a--[ncmax x ncmax] array
c
c  outputs:
c         u--unit vector of size ncmax
c    xlamda--smallest eigenvalue
c      ierr--error flag
c      herr--error string (character*255 variable if ierr<>0)
c
c  written by G. Venkatarathnam, Indian Institute of Technology Madras
c  10-15-11  GV, original version
c  01-07-13 EWL, adapt code for use in REFPROP
c
      include 'commons.for'
      dimension a(ncmax,ncmax),u(ncmax),y(ncmax),a1(ncmax,ncmax)
      character*255 herr
      maxn=ncmax
      tol=1.d-12
      maxiters=20
      a1(1:n,1:n)=a(1:n,1:n)
      do i=1,n
        a1(i,i)=a1(i,i)-xlamda
      enddo
      y(1:n)=0.d0
      call LUdecomp (n,maxn,a1,y,ierr,herr)
      if (ierr.ne.0) RETURN
      iters=0
      xnorm=1.d0
      y(2:n)=0.d0
      y(1)=a1(1,1)
      do while (xnorm.gt.tol .and. iters.le.maxiters)
        iters=iters+1
        xlamdao=xlamda
        u(1:n)=y(1:n)/SQRT(SUM(y(1:n)**2))
        y(1:n)=u(1:n)
        ierr=-191
        call LUdecomp (n,maxn,a1,y,ierr,herr)
        if (ierr.ne.0) RETURN
        xlamda=DOT_PRODUCT(y(1:n),MATMUL(a(1:n,1:n),y(1:n)))/
     &       DOT_PRODUCT(y(1:n),y(1:n))
        xnorm=1.d0
        if (xlamdao.ne.0.d0) xnorm=(1.d0-xlamda/xlamdao)**2
      enddo
      if (u(1).lt.0.d0) u(1:n)=SIGN(u(1:n),-u(1:n))
      if (iters.gt.maxiters) then
        ierr=1
        herr='[MINEIGEN error 1] routine did not converge'
      endif
      RETURN
      end                                           !subroutine MINEIGEN
c
c ======================================================================
c
      subroutine PASSCMN (hvr,iset,icomp,jcomp,
     &                    hstr,ilng,dbl,arr,ierr,herr)
c
c  get or set values of variables in the common blocks
c
c  inputs:
c      hvr--character string with the common variable's name (lower case only)
c     iset--flag to indicate the get/set condition
c             0 -- > get variable value
c             1 -- > set variable value
c    icomp--component number
c    jcomp--second component number for binary mixture variables
c  inputs and outputs:
c     hstr--input or output for a character string
c     ilng--input or output for a long variable
c      dbl--input or output for a double precision variable
c      arr--input or output for a double precision array
c           note:  values in this array are not initialized to 0
c  outputs:
c     ierr--error flag:  0 = successful
c                        1 = unrecognized variable name
c     herr--error string (character*255 variable if ierr<>0)
c
c  Examples:
c  Call PASSCMN ('txeos',  0,3,0, h,i, tmx,z,   ierr,herr) - get tmax of component 3
c  Call PASSCMN ('deos',   1,2,0, h,i, dmx,z,   ierr,herr) - set dmax of component 2
c  Call PASSCMN ('tz',     0,1,0, h,i, tc, z,   ierr,herr) - get reducing temperature of component 1
c  Call PASSCMN ('ntermf', 0,1,0, h,nt,v,  z,   ierr,herr) - get number of terms in the Helmholtz equation for component 1
c  Call PASSCMN ('coefhmx',1,1,0, h,i, v,  cf,  ierr,herr) - set the coefficients in the Helmholtz equation for component 1
c  Call PASSCMN ('cpc0',   1,5,0, h,i, v,  cp0, ierr,herr) - set the coefficients in the cp0 equation for component 5
c  Call PASSCMN ('fprkij', 1,1,2, h,i, v,  fpr, ierr,herr) - set the PR coefficient for the 1,2 binary
c
c  written by Eric W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-16-13 EWL, original version
c
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PASSCMN"::PASSCMN
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PASSCMN
c
      include 'commons.for'
      character*255 herr,hvar,hstr,htemp
      character hvr*(*)
      dimension arr(100),a(100)
c
      ierr=0
      herr=' '
      l=.false.
      if (iset.eq.1) l=.true.
      if (abs(icomp).gt.ncmax .or. abs(jcomp).gt.ncmax) then
        ierr=2
        herr='[PASSCMN error] inputs out of bounds'
      endif
c
      ia=0
      ij=0
      v=0.d0
      a=0.d0
      htemp=' '
      if (.not.l) then
        hstr=' '
        ilng=0
        dbl=0.d0
      endif
c
      hvar=hvr
      i=index(hvr,char(0))
      if (i.gt.1) hvar=hvr(1:i-1)
c
c  character strings
      if (hvar.eq.'heos') then
        htemp=heos
        if (l) heos=hstr
c
c  long variables
      elseif (hvar.eq.'ntermf') then
        ij=ntermf(icomp)
        if (l) ntermf(icomp)=ilng
      elseif (hvar.eq.'ncrt') then
        ij=ncrt(icomp)
        if (l) ncrt(icomp)=ilng
      elseif (hvar.eq.'ntermc') then
        ij=ntermc(icomp)
        if (l) ntermc(icomp)=ilng
      elseif (hvar.eq.'nterme') then
        ij=nterme(icomp)
        if (l) nterme(icomp)=ilng
C
c  double precision variables
      elseif (hvar.eq.'accen') then
        v=accen(icomp)
        if (l) accen(icomp)=dbl
      elseif (hvar.eq.'dcrit') then
        v=Dcrit(icomp)
        if (l) Dcrit(icomp)=dbl
      elseif (hvar.eq.'deos') then
        v=Deos(icomp)
        if (l) Deos(icomp)=dbl
      elseif (hvar.eq.'dipole') then
        v=dipole(icomp)
        if (l) dipole(icomp)=dbl
      elseif (hvar.eq.'pcfeq') then
        v=pcfeq(icomp)
        if (l) pcfeq(icomp)=dbl
      elseif (hvar.eq.'pcrit') then
        v=pcrit(icomp)
        if (l) pcrit(icomp)=dbl
      elseif (hvar.eq.'peos') then
        v=peos(icomp)
        if (l) peos(icomp)=dbl
      elseif (hvar.eq.'r') then
        v=R
        if (l) R=dbl
      elseif (hvar.eq.'rho0feq') then
        v=rho0feq(icomp)
        if (l) rho0feq(icomp)=dbl
      elseif (hvar.eq.'rhocfeq') then
        v=rhocfeq(icomp)
        if (l) rhocfeq(icomp)=dbl
      elseif (hvar.eq.'rhoz') then
        v=rhoz(icomp)
        if (l) rhoz(icomp)=dbl
      elseif (hvar.eq.'t0feq') then
        v=t0feq(icomp)
        if (l) t0feq(icomp)=dbl
      elseif (hvar.eq.'tcfeq') then
        v=tcfeq(icomp)
        if (l) tcfeq(icomp)=dbl
      elseif (hvar.eq.'tcrit') then
        v=tcrit(icomp)
        if (l) tcrit(icomp)=dbl
      elseif (hvar.eq.'tmeos') then
        v=tmeos(icomp)
        if (l) tmeos(icomp)=dbl
      elseif (hvar.eq.'tnbp') then
        v=tnbp(icomp)
        if (l) tnbp(icomp)=dbl
      elseif (hvar.eq.'ttpn') then
        v=ttpn(icomp)
        if (l) ttpn(icomp)=dbl
      elseif (hvar.eq.'txeos') then
        v=txeos(icomp)
        if (l) txeos(icomp)=dbl
      elseif (hvar.eq.'tz') then
        v=tz(icomp)
        if (l) tz(icomp)=dbl
      elseif (hvar.eq.'wmas') then
        v=wmas(icomp)
        if (l) wmas(icomp)=dbl
      elseif (hvar.eq.'zcrit') then
        v=Zcrit(icomp)
        if (l) Zcrit(icomp)=dbl
      elseif (hvar.eq.'pcpr') then
        v=pcpr(icomp)
        if (l) pcpr(icomp)=dbl
      elseif (hvar.eq.'rhocpr') then
        v=rhocpr(icomp)
        if (l) rhocpr(icomp)=dbl
      elseif (hvar.eq.'tcpr') then
        v=tcpr(icomp)
        if (l) tcpr(icomp)=dbl
      elseif (hvar.eq.'acnpr') then
        v=acnpr(icomp)
        if (l) acnpr(icomp)=dbl
c
c  arrays
      elseif (hvar.eq.'afeq') then
        ia=ntermf(icomp)+ncrt(icomp)
        do i=1,ia
          a(i) = afeq(icomp,i)
          if (l) afeq(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'tifeq') then
        ia=ntermf(icomp)+ncrt(icomp)
        do i=1,ia
          a(i) = tifeq(icomp,i)
          if (l) tifeq(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'difeq') then
        ia=ntermf(icomp)+ncrt(icomp)
        do i=1,ia
          a(i) = difeq(icomp,i)
          if (l) difeq(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'dlif') then
        ia=ntermf(icomp)+ncrt(icomp)
        do i=1,ia
          a(i) = dlif(icomp,i)
          if (l) dlif(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'cpc0') then
        ia=ntermc(icomp)+nterme(icomp)
        do i=1,ia
          a(i) = cpc0(icomp,i)
          if (l) cpc0(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'xkc0') then
        ia=ntermc(icomp)+nterme(icomp)
        do i=1,ia
          a(i) = xkc0(icomp,i)
          if (l) xkc0(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'prcoef') then
        ia=20
        do i=1,ia
          a(i) = prcoef(icomp,i)
          if (l) prcoef(icomp,i)=arr(i)
        enddo
      elseif (hvar.eq.'fprkij') then
        ia=nmxprb
        do i=1,ia
          a(i) = fprkij(icomp,jcomp,i)
          if (l) fprkij(icomp,jcomp,i)=arr(i)
        enddo
c
      else
        ierr=1
        herr='[PASSCMN error] variable name not recognized'
      end if
c
      if (l) then
        tsvcp0(icomp)=0.d0
        call RESETA
      else
        if (ia.gt.0) then
          arr(1:ia)=a(1:ia)
        elseif (ij.ne.0) then
          ilng=ij
        elseif (v.ne.0.d0) then
          dbl=v
        elseif (htemp.ne.' ') then
          hstr=htemp
        endif
      endif
      end                                            !subroutine PASSCMN
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                     end file utility.f
c ======================================================================
