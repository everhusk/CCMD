c  begin file core_PH0.f
c
c  This file contains the functions implementing the ideal-gas part of
c  the reduced Helmholtz free energy form of the pure fluid equation of
c  state (the so-called "fundamental equation").
c
c  The Helmholtz energy consists of ideal and residual (real-gas) terms;
c  this routine calculates only the ideal part, and only for pure components.
c
c  contained here are:
c     subroutine SETPH0 (nread,icomp,hcasno,ierr,herr)
c     function PH0PH0 (icomp,itau,idel,t,rho)
c     block data SAVPH0
c
c  these routines use the following common blocks from other files
c     common /CREF/ tref(n0:nx),rhoref(n0:nx),href(n0:nx),sref(n0:nx)
c     common /HCHAR/ htab,hnull
c     common /Gcnst/ R,tz(n0:nx),rhoz(n0:nx)
c
c  various arrays are dimensioned with parameter statements
c     parameter (nph0mx=10)       !max number of terms in Cp0 polynomial
c
c ======================================================================
c ======================================================================
c
      subroutine SETPH0 (nread,icomp,hcasno,ierr,herr)
c
c  set up working arrays for ideal-gas part of the Helmholtz energy
c  implements an expression of the form:
c     phi0 = Sum[ai*log(tau**ti)] + Sum[aj*tau**tj]
c          + Sum[ak*log(1-EXP(bk*tau))]
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data (not currently implemented)
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (1..nc); 1 for pure fluid;
c           zero and negative numbers designate ECS reference fluids
c   hcasno--CAS number of component icomp
c           (not req'd if reading from file--included to maintain
c           parallel structure with other routines)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     coefficients, etc. returned via arrays in common /xxxPH0/
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  05-13-96  MM, original version, based (loosely) on SETCPP
c  06-14-96  MM, fix bug in reading/storing coefficients
c  11-13-97  MM, (re)initialize contents of /PH0SAV/ when a new fluid is read in
c  08-31-98 MEV, reverse order of indices in tsvph0(i,j)=0 and rhosvp(i,j)=0
c  08-13-98  MM, delete obsolete (unused) format statement
c  07-25-06 EWL, add cosh and sinh functions
c
      include 'commons.for'
      character*12 hcasno,hcasp
      character*255 herr
      common /CASPH0/ hcasp(mxph0)
c
c  (re)initialize contents of /PH0SAV/ when a new fluid is read in
      do i=n0,nx
        ph0sav(i)=0.0d0
        ph1sav(i)=0.0d0
        ph2sav(i)=0.0d0
        do j=0,2
          tsvph0(j,i)=0.0d0
          rhosvp(j,i)=0.0d0
        enddo
      enddo
c
      if (nread.le.0) then
c  get coefficients from block data
c  identify specified fluid with entries in database via match of CAS no
        do k=1,mxph0
          if (hcasno.eq.hcasp(k)) then
c           write (*,*)' SETPH0--ERROR, block data read not implemented'
            ierr=1
          herr='[SETPH0 error] Block data option not implemented'//hnull
            RETURN
          end if
        enddo
        ierr=1
        herr='[SETPH0 error] Input fluid (block data) not found'//hnull
        RETURN
      else
c  read data from file
c       write (*,*) ' SETPH0--read component',icomp,' from unit',nread
        read (nread,*) tminph(icomp)              !lower temperature limit
        read (nread,*) tmaxph(icomp)              !upper temperature limit
        read (nread,*) pmaxph(icomp)              !upper pressure limit
        read (nread,*) dmaxph(icomp)            !upper density limit
c  read number of terms for each of the various types
        if (verfl(icomp).ge.8.0d0) then
          read (nread,*) nlogp(icomp),ntaup(icomp),nexpp(icomp),
     &                   ncoshp(icomp),nsinhp(icomp),
     &                   nsp1p(icomp),nsp2p(icomp),nsp3p(icomp)  !spares
        else
          read (nread,*) nlogp(icomp),ntaup(icomp),nexpp(icomp)
          ncoshp(icomp)=0  !these terms not used in files prior to v8.0
          nsinhp(icomp)=0
          nsp1p(icomp)=0
          nsp2p(icomp)=0
          nsp3p(icomp)=0
        endif
        jterm=nlogp(icomp)+ntaup(icomp)+nexpp(icomp)+ncoshp(icomp)
     &        +nsinhp(icomp)+nsp1p(icomp)+nsp2p(icomp)+nsp3p(icomp)
        if (jterm.ge.1) then
c  read coefficients for terms of the form [ai*log(tau**ti)],
c  [ai*tau**ti], and [ai*log(1-EXP(bi*tau))]
          do i=1,jterm
            read (nread,*) aiph(icomp,i),tiph(icomp,i)
          enddo
        end if
        ierr=0
        herr=' '
      end if
c
      RETURN
      end                                             !subroutine SETPH0
c
c ======================================================================
c
      function PH0PH0 (icomp,itau,idel,t,rho)
c
c  compute the ideal gas part of the reduced Helmholtz energy or a
c  derivative as functions of temperature and pressure; for
c  use with a Helmholtz-explicit equation of state
c
c  inputs:
c    icomp--pointer specifying component (1..nc)
c     itau--flag specifying order of temperature derivative to calc
c     idel--flag specifying order of density derivative to calculate
c           (the density derivatives are not used in the calculation
c           of any property, and are not implemented)
c           when itau = 0 and idel = 0, compute A0/RT
c           when itau = 1 and idel = 0, 1st temperature derivative
c           when itau = 2 and idel = 0, 2nd temperature derivative
c        t--temperature (K)
c      rho--density (mol/L)
c  output (as function value):
c   ph0ph0--ideal-gas part of the Helmholtz energy in reduced form (A/RT)
c           derivatives (as specified by itau and idel) are multiplied
c           by the corresponding power of tau; i.e. when itau = 1, the
c           quantity returned is tau*d(ph0ph0)/d(tau) and when itau = 2,
c           the quantity returned is tau*tau*d2(ph0ph0)/d(tau)**2
c
c  Note: While the real-gas part of the Helmholtz energy is calculated
c        in terms of dimensionless temperature and density, the ideal-
c        gas part is calculated in terms of absolute temperature and
c        density.  (This distinction is necessary for mixtures.)
c
c        The Helmholtz energy consists of ideal-gas and residual
c        (real-gas) terms; this routine calculates only the ideal part.
c
c        This function computes pure component properties only.
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  05-13-96  MM, original version, based (loosely) on PH0CPP
c  06-14-96  MM, add rhosvp to /PH0SAV/
c  07-08-96  MM, change derivative outputs:  tau*d(phi)/d(tau), etc
c  08-20-97  MM, call ERRMSG if itau out of range; drop idel=idel
c  07-11-00 EWL, remove krypton pieces
c  07-25-06 EWL, add cosh and sinh functions
c  08-24-06 EWL, add check for PHG in hmodcp (add EOSMOD common block).
c                If it is used, multiple by R*/R as described in GERG-2004 eos (of Kunz and Wagner)
c
      include 'commons.for'
      character*255 herr
c
c  compute reduced Helmholtz; first check if t same as previous call
c
      PH0PH0=0.0d0         !initialize in case of error
      if (t.le.0) RETURN
      tau=tz(icomp)/t
      del=rho/rhoz(icomp)
      if (itau*idel.ne.0) then
        PH0PH0=0.0d0
      elseif (itau.eq.0 .and. idel.eq.0) then
        if (abs(t-tsvph0(0,icomp)).lt.1.0d-8 .and.
     &      abs(rho-rhosvp(0,icomp)).lt.1.0d-10) then
          PH0PH0=ph0sav(icomp)
c         write (*,*) ' PH0PH0--using saved phi for itau = 0, t =',t
        else
c         write (*,1030) icomp,t,rho,t0,D0,tau,del
c1030     format (1x,'PH0PH0--icomp,t,rho,t0,D0,tau,del: ',i4,6e14.6)
          iterm=0
          phisum=LOG(del)
c    &          +href(icomp)/R/t-sref(icomp)/R !see U. Idaho class notes
          if (nlogp(icomp).ge.1) then
c  sum terms of the form [ai*log(tau**ti)]
            do i=1,nlogp(icomp)
              iterm=iterm+1
             phisum=phisum+aiph(icomp,iterm)*LOG(tau**tiph(icomp,iterm))
c             write (*,1040) iterm,aiph(icomp,iterm),tiph(icomp,iterm),phisum
c1040         format (' PH0PH0--log term iterm,ai,ti,phisum: ',i3,3f12.6)
            enddo
          end if
          if (ntaup(icomp).ge.1) then
c  sum terms of the form [ai*tau**ti]
            do j=1,ntaup(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tau**tiph(icomp,iterm)
c             write (*,1060) iterm,aiph(icomp,iterm),tiph(icomp,iterm),phisum
c1060         format (' PH0PH0--tau term iterm,ai,ti,phisum: ',i3,3f12.6)
            enddo
          end if
          if (nexpp(icomp).ge.1) then
c  sum terms of the form [ai*log(1-EXP(bi*tau))]
c  (the bi coefficients are stored in the ti array)
            do k=1,nexpp(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)
     &              *LOG(1.0d0-EXP(tiph(icomp,iterm)*tau))
c             write (*,1080) iterm,aiph(icomp,iterm),tiph(icomp,iterm),phisum
c1080         format (' PH0PH0--exp term iterm,ai,ti,phisum: ',i3,3f12.6)
            enddo
          end if
          if (ncoshp(icomp).ge.1) then
c  sum terms of the form [ai*log(cosh(bi*tau))]
            do k=1,ncoshp(icomp)
              iterm=iterm+1
              ttau=tiph(icomp,iterm)*tau
              if (ttau.lt.700.d0) then
                phisum=phisum+aiph(icomp,iterm)*LOG(COSH(ttau))
              else
                phisum=phisum+aiph(icomp,iterm)*700.d0
              endif
            enddo
          end if
          if (nsinhp(icomp).ge.1) then
c  sum terms of the form [ai*log(sinh(bi*tau))]
            do k=1,nsinhp(icomp)
              iterm=iterm+1
              ttau=tiph(icomp,iterm)*tau
              if (ttau.lt.700.d0) then
                phisum=phisum+aiph(icomp,iterm)*LOG(SINH(ttau))
              else
                phisum=phisum+aiph(icomp,iterm)*700.d0
              endif
            enddo
          end if
          PH0PH0=phisum
c  save information for possible use on next call to function
          tsvph0(0,icomp)=t
          rhosvp(0,icomp)=rho
          ph0sav(icomp)=PH0PH0
        end if
c
c  compute derivative w.r.t. tau (dimensionless temperature)
c
      else if (itau.eq.1) then
        if (abs(t-tsvph0(1,icomp)).lt.1.0d-8 .and.
     &      abs(rho-rhosvp(1,icomp)).lt.1.0d-10) then
          PH0PH0=ph1sav(icomp)
        else
          iterm=0
          phisum=0.0d0
c         phisum=href(icomp)/(R*tz)          !see U. Idaho class notes
          if (nlogp(icomp).ge.1) then
c  sum terms of the form [ai*log(tau**ti)]
            do i=1,nlogp(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)/tau
            enddo
          end if
          if (ntaup(icomp).ge.1) then
c  sum terms of the form [ai*tau**ti]
            do j=1,ntaup(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)
     &              *tau**(tiph(icomp,iterm)-1.0d0)
            enddo
          end if
          if (nexpp(icomp).ge.1) then
c  sum terms of the form [ai*log(1-EXP(bi*tau))]
c  (the bi coefficients are stored in the ti array)
            do k=1,nexpp(icomp)
              iterm=iterm+1
              exptau=EXP(tiph(icomp,iterm)*tau)
              phisum=phisum-aiph(icomp,iterm)*tiph(icomp,iterm)
     &              *exptau/(1.0d0-exptau)
            enddo
          end if
          if (ncoshp(icomp).ge.1) then
c  sum terms of the form [ai*log(cosh(bi*tau))]
            do k=1,ncoshp(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)
     &              *(TANH(tiph(icomp,iterm)*tau))
            enddo
          end if
          if (nsinhp(icomp).ge.1) then
c  sum terms of the form [ai*log(sinh(bi*tau))]
            do k=1,nsinhp(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)
     &              /(TANH(tiph(icomp,iterm)*tau))
            enddo
          end if
c  save information for possible use on next call to function
          PH0PH0=phisum*tau       !return tau*d(ph0ph0)/d(tau)
          tsvph0(1,icomp)=t
          rhosvp(1,icomp)=rho
          ph1sav(icomp)=PH0PH0
        end if
c
c  compute 2nd derivative w.r.t. tau (dimensionless temperature)
c
      else if (itau.eq.2) then
        if (abs(t-tsvph0(2,icomp)).lt.1.0d-8 .and.
     &      abs(rho-rhosvp(2,icomp)).lt.1.0d-10) then
          PH0PH0=ph2sav(icomp)
        else
          iterm=0
          phisum=0.0d0
          if (nlogp(icomp).ge.1) then
c  sum terms of the form [ai*log(tau**ti)]
            do i=1,nlogp(icomp)
              iterm=iterm+1
              phisum=phisum-aiph(icomp,iterm)*tiph(icomp,iterm)/tau**2
            enddo
          end if
          if (ntaup(icomp).ge.1) then
c  sum terms of the form [ai*tau**ti]
            do j=1,ntaup(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)
     &              *(tiph(icomp,iterm)-1.0d0)
     &              *tau**(tiph(icomp,iterm)-2.0d0)
            enddo
          end if
          if (nexpp(icomp).ge.1) then
c  sum terms of the form [ai*log(1-EXP(bi*tau))]
c  (the bi coefficients are stored in the ti array)
            do k=1,nexpp(icomp)
              iterm=iterm+1
              exptau=EXP(tiph(icomp,iterm)*tau)
              phisum=phisum-aiph(icomp,iterm)*tiph(icomp,iterm)**2
     &              *exptau/(1.0d0-exptau)**2
            enddo
          end if
          if (ncoshp(icomp).ge.1) then
c  sum terms of the form [ai*log(cosh(bi*tau))]
            do k=1,ncoshp(icomp)
              iterm=iterm+1
              if (tiph(icomp,iterm)*tau.lt.500.d0) then
                phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)**2
     &                /(COSH(tiph(icomp,iterm)*tau))**2
              endif
            enddo
          end if
          if (nsinhp(icomp).ge.1) then
c  sum terms of the form [ai*log(sinh(bi*tau))]
            do k=1,nsinhp(icomp)
              iterm=iterm+1
              if (tiph(icomp,iterm)*tau.lt.500.d0) then
                phisum=phisum-aiph(icomp,iterm)*tiph(icomp,iterm)**2
     &                /(SINH(tiph(icomp,iterm)*tau))**2
              endif
            enddo
          end if
c  save information for possible use on next call to function
          PH0PH0=phisum*tau*tau
          tsvph0(2,icomp)=t
          rhosvp(2,icomp)=rho
c  return tau**2*d2(ph0ph0)/d(tau**2)
          ph2sav(icomp)=PH0PH0
        end if
      else if (itau.eq.3) then
          iterm=0
          phisum=0.0d0
          if (nlogp(icomp).ge.1) then
            do i=1,nlogp(icomp)
              iterm=iterm+1
              phisum=phisum+2.d0*aiph(icomp,iterm)
     &              *tiph(icomp,iterm)/tau**3
            enddo
          end if
          if (ntaup(icomp).ge.1) then
            do j=1,ntaup(icomp)
              iterm=iterm+1
              phisum=phisum+aiph(icomp,iterm)*tiph(icomp,iterm)
     &             *(tiph(icomp,iterm)-1.0d0)
     &             *(tiph(icomp,iterm)-2.0d0)
     &             *tau**(tiph(icomp,iterm)-3.0d0)
            enddo
          end if
          if (nexpp(icomp).ge.1) then
            do k=1,nexpp(icomp)
              iterm=iterm+1
              exptau=EXP(tiph(icomp,iterm)*tau)
              phisum=phisum-aiph(icomp,iterm)*tiph(icomp,iterm)**3*
     &              (exptau/(1.0d0-exptau)**2+
     &               2.d0*exptau**2/(1.0d0-exptau)**3)
            enddo
          end if
          if (ncoshp(icomp).ge.1) then
            do k=1,ncoshp(icomp)
              iterm=iterm+1
              phisum=phisum-2.d0*aiph(icomp,iterm)*tiph(icomp,iterm)**3
     &              /(COSH(tiph(icomp,iterm)*tau))**3
     &              *(SINH(tiph(icomp,iterm)*tau))
            enddo
          end if
          if (nsinhp(icomp).ge.1) then
            do k=1,nsinhp(icomp)
              iterm=iterm+1
              phisum=phisum+2.d0*aiph(icomp,iterm)*tiph(icomp,iterm)**3
     &              /(SINH(tiph(icomp,iterm)*tau))**3
     &              *(COSH(tiph(icomp,iterm)*tau))
            enddo
          end if
          PH0PH0=phisum*tau**3
      elseif (idel.eq.1) then
        PH0PH0=1.0d0
      elseif (idel.eq.2) then
        PH0PH0=-1.0d0
      elseif (idel.eq.3) then
        PH0PH0=2.0d0
      else
c
c  invalid value of itau
c
        ierr=99
        write (herr,1099) itau,idel,hnull
 1099   format ('[PH0PH0 warning] invalid input; itau =',i4,'; idel =',
     &          i4,a1)
        call ERRMSG (ierr,herr)
        PH0PH0=0.0d0
      end if
      if (hmodcp(icomp).eq.'PHG') then
        PH0PH0=8.31451D0/8.314472D0*PH0PH0
      endif
c
c     write (*,*) ' PH0PH0:  output phi: ',ph0ph0
c
      RETURN
      end                                               !function PH0PH0
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file core_PH0.f
c ======================================================================
