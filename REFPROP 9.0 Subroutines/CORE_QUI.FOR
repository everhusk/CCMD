c  begin file core_QUI.f
c
c  This file contains the routines implementing the Helmholtz form of
c  the pure fluid equation of state in the Quintic form.
c
c  contained here are:
c     function PHIQUI (icomp,itau,idel,tau,del)
c     subroutine CRTQUI (icomp,tc,pc,Dc)
c     subroutine REDQUI (icomp,tred,Dred)
c     subroutine SETQUI (nread,icomp,hcasno,ierr,herr)
c
c ======================================================================
c ======================================================================
c
      function PHIQUI (icomp,itau,idel,tau,del)
c
c  compute reduced Helmholtz energy or a derivative as functions
c  of dimensionless temperature and density for the Helmholtz-explicit
c  equation of state
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
c           itau  idel    output (dimensionless for all cases)
c             0    0      A/RT
c             1    0      tau*[d(A/RT)/d(tau)]
c             2    0      tau**2*[d**2(A/RT)/d(tau)**2]
c             0    1      del*[d(A/RT)/d(del)]
c             0    2      del**2*[d**2(A/RT)/d(del)**2]
c             1    1      tau*del*[d**2(A/RT)/d(tau)d(del)]
c                         etc.
c
c  written by D.E. Cristancho, NIST Thermophysics Division, Boulder, Colorado
c  06-18-09 DEC, original version
c
      include 'commons.for'
c
      phiqui=0.d0
      if (del.le.1.0d-10) RETURN  !trivial solution at zero density for
      if (tau.le.0.d0) RETURN    !  any and all derivatives
c
      ncode=idel*4+itau+1
      nterm=netaq(icomp)+nepsq(icomp)+nbbq(icomp)+ngamq(icomp)
     &     +nbetq(icomp)
      if (abs(tau-tausvq(icomp)).lt.1.0d-12 .and.
     &    abs(del-delsvq(icomp)).lt.1.0d-16) then
c  retrieve value from previous call
        if (drvflq(icomp)(ncode:ncode).eq.'1') then
          phiqui=drvsvq(icomp,ncode)
          RETURN
        endif
      else
c  otherwise, compute new values and save for possible future use
c  first compute needed powers of tau and del (and save for future use)
        drvflq(icomp)='0000000000000000'
        if (abs(tau-tausvq(icomp)).gt.1.0d-12) then
          elntau=log(tau)
          tausvq(icomp)=tau
          do j=1,nterm
            taupq(icomp,j)=tiqui(icomp,j)*elntau
          enddo
        end if
        if (abs(del-delsvq(icomp)).gt.1.0d-16) then
          elndel=log(del)
          delsvq(icomp)=del
          do j=1,nterm
            delpq(icomp,j)=diqui(icomp,j)*elndel
          enddo
        end if
c
        phisum=0.d0
        etaq=0.d0
        etaq10=0.d0
        etaq20=0.d0
        epsq=0.d0
        epsq10=0.d0
        epsq20=0.d0
        bbq00=0.d0
        bbq10=0.d0
        bbq20=0.d0
        gamq=0.d0
        gamq10=0.d0
        gamq20=0.d0
        betq=0.d0
        betq10=0.d0
        betq20=0.d0
        j=0
        do k=1,netaq(icomp)
          j=j+1
          etaqui=etaqui+aqui(icomp,j)*tau**tiqui(icomp,j)
          etaqui10=etaqui10
     &          +tiqui(icomp,j)*aqui(icomp,j)*tau**(tiqui(icomp,j)-1.d0)
          etaq20=etaq20+tiqui(icomp,j)*(tiqui(icomp,j)-1.d0)*
     &           aqui(icomp,j)*tau**(tiqui(icomp,j)-2.d0)
        enddo
        do k=1,nepsq(icomp)
          j=j+1
          epsq=epsq+aqui(icomp,j)*tau**tiqui(icomp,j)
          epsq10=epsq10
     &          +tiqui(icomp,j)*aqui(icomp,j)*tau**(tiqui(icomp,j)-1.d0)
          epsq20=epsq20+tiqui(icomp,j)*(tiqui(icomp,j)-1.d0)*
     &           aqui(icomp,j)*tau**(tiqui(icomp,j)-2.d0)
        enddo
        do k=1,nbbq(icomp)
          j=j+1
          bbq00=bbq00+aqui(icomp,j)*tau**tiqui(icomp,j)
          bbq10=bbq10
     &         +tiqui(icomp,j)*aqui(icomp,j)*tau**(tiqui(icomp,j)-1.d0)
          bbq20=bbq20+tiqui(icomp,j)*(tiqui(icomp,j)-1.d0)*
     &           aqui(icomp,j)*tau**(tiqui(icomp,j)-2.d0)
        enddo
        do k=1,ngamq(icomp)
          j=j+1
          gamq=gamq+aqui(icomp,j)*tau**tiqui(icomp,j)
          gamq10=gamq10
     &          +tiqui(icomp,j)*aqui(icomp,j)*tau**(tiqui(icomp,j)-1.d0)
          gamq20=gamq20+tiqui(icomp,j)*(tiqui(icomp,j)-1.d0)*
     &           aqui(icomp,j)*tau**(tiqui(icomp,j)-2.d0)
        enddo
        do k=1,nbetq(icomp)
          j=j+1
          betq=betq+aqui(icomp,j)*tau**tiqui(icomp,j)
         betq10=betq10
     &         +tiqui(icomp,j)*aqui(icomp,j)*tau**(tiqui(icomp,j)-1.d0)
          betq20=betq20+tiqui(icomp,j)*(tiqui(icomp,j)-1.d0)*
     &           aqui(icomp,j)*tau**(tiqui(icomp,j)-2.d0)
        enddo
        phisum=etaq*(del-log(1.d0-bbq00*del)/bbq00)
     &        -epsq*log((1.d0+gamq*del)/(1.d0-betq*del))
c         ex=taupq(icomp,k)+delpq(icomp,k)
c         if (ex.lt.100.d0 .and. ex.gt.-200.d0) then
c           phisvq(icomp,k)=aqui(icomp,k)*EXP(ex)
c         else
c           phisvq(icomp,k)=0.d0
c         endif
c         phisum=phisum+phisvq(icomp,k)
        phiqui=phisum
        drvflq(icomp)(1:1)='1'
        drvsvq(icomp,1)=phiqui
      end if
c
c  check if derivatives are requested, calculations make use of fact
c  that terms in derivative summations are very similar to A/RT terms
c
      if (idel.eq.1) then
c  compute derivative w.r.t. del (dimensionless density)
c  save individual terms for possible use in cross derivative
        phisum=0.d0
        phisum=etaq*(1.d0+1.d0/(1.d0-bbq00*del))
     &        -epsq*(gamq/(1.d0+gamq*del)+betq/(1.d0-betq*del))
        phiqui=phisum*del
c
      elseif (idel.eq.2) then
c  compute 2nd derivative w.r.t. del (dimensionless density)
c  save individual terms for possible use in cross derivative
        phisum=0.d0
c        do k=1,nterm
c          dik=diqui(icomp,k)
c          phiq02(k)=phisvq(icomp,k)*(dik**2-dik)
c           phisum+phiq02(k)
c          phisum=etaq*(1.d0+1.d0/(1.d0-bbq00*del))
c              epsq*(gamq/(1.d0+gamq*del)+betq/(1.d0-betq*del))
c         enddo
        phisum=etaq*bbq00/(1.d0-bbq00*del)**2
     &        -epsq*(-gamq**2/(1.d0+gamq*del)**2
     &        +betq**2/(1.d0-betq*del)**2)
        phiqui=phisum*del**2
c
      elseif (idel.eq.3) then
c  compute 3rd derivative w.r.t. del (dimensionless density)
        phisum=0.d0
c        do k=1,nterm
c          dik=diqui(icomp,k)
c          phiq03(k)=phisvq(icomp,k)
c    &     +6.0d0*dik-3.0d0*dik-3.0d0*dik**2
c          phisum=phisum+phiq03(k)
c        enddo
        phisum=2.d0*etaq*bbq00**2/(1.d0-bbq00*del)**3
     &        -2.d0*epsq*(gamq**3/(1.d0+gamq*del)**3
     &        +betq**3/(1.d0-betq*del)**3)
        phiqui=phisum*del**3
      end if
c
c
c   epsqi0,bbqio,gamqi0,betqi0,etaqi0 are the i tau derivatives of fitting
c   parameters
c
      if (itau.eq.1) then
c  compute derivative w.r.t. tau (dimensionless temperature)
c  save individual terms for possible use in cross derivative
        phisum=0.d0
          phisum=etaq10*(del-log(1.d0-bbq00*del)/bbq00)
     &        +etaq*bbq10/bbq00*(log(1.d0-bbq00*del)/bbq00
     &        +del/(1.d0-bbq00*del))
     &        -epsq10*log((1.d0+gamq*del)/(1.d0-betq*del))
     &        -epsq*del*(gamq10/(1.d0+gamq*del)
     &        +betq10/(1.d0-betq*del))
        phiqui=phisum*tau
c
      elseif (itau.eq.2) then
c  compute 2nd derivative w.r.t. tau (dimensionless temperature)
c  save individual terms for possible use in cross derivative
        phisum=0.d0
c        do k=1,nterm
c          tik=tiqui(icomp,k)
c          phiq20(k)=phisvq(icomp,k)*tik*(tik-1.0d0)
c          phisum=phisum+phiq20(k)
c        enddo
          phisum=etaq20*(del-log(1.d0-bbq00*del)/bbq00)
     &        +1.d0/bbq00*(2.d0*etaq10*bbq10+etaq*bbq20
     &        -etaq*bbq10**2/bbq00)
     &        *(log(1.d0-bbq00*del)/bbq00+del/(1.d0-bbq00*del))
     &        +etaq*bbq10**2/bbq00**2*(del*(2.d0*bbq00*del-1)/
     &        (1.d0-bbq00*del)**2-log(1.d0-bbq00*del)/bbq00)
     &        -epsq20*log((1.d0+gamq*del)/(1.d0-betq*del))
     &        -2.d0*epsq10*del*(gamq10/(1.d0+gamq*del)
     &        +betq10/(1.d0-betq*del))
     &        -epsq*del*(gamq20/(1.d0+gamq*del)
     &        +betq20/(1.d0-betq*del)-del*(gamq10**2/
     &        (1.d0+gamq*del)**2-betq10**2/(1.d0-betq*del)**2))

        phiqui=phisum*tau**2
c
C    not third derivative!!!!!
      elseif (itau.eq.3) then
c  compute 3rd derivative w.r.t. tau (dimensionless temperature)
        phisum=0.d0
        do k=1,nterm
          tik=tiqui(icomp,k)
          phisum=phisum+phisvq(icomp,k)*tik*(tik-1.d0)*(tik-2.d0)
        enddo
        phiqui=phisum
      end if
c
c
      if (itau.eq.1 .and. idel.eq.1) then
c  compute cross derivative using terms from 1st derivatives
        phisum=0.d0
          phisum=etaq10*(1.d0+1.d0/(1.d0-bbq00*del))
     &        +etaq*bbq10*del/(1.d0-bbq00*del)**2
     &        -epsq10*(gamq/(1.d0+gamq*del)+betq/(1.d0-betq*del))
     &        -epsq*(gamq10/(1.d0+gamq*del)
     &        +betq10/(1.d0-betq*del)-del*(gamq*gamq10/
     &        (1.d0+gamq*del)**2-betq*betq10/(1.d0-betq*del)**2))
        phiqui=phisum*del*tau
c
      elseif (itau.eq.2 .and. idel.eq.1) then
c  compute cross derivative using term from 1st derivative
        phisum=0.d0
C       do k=1,nterm
C         tik=tiqui(icomp,k)
C         phisum=phisum+(tik*tik-tik)*phiq01(k)
C       enddo
        phiqui=phisum
c
      elseif (itau.eq.1 .and. idel.eq.2) then
c  compute cross derivative using term from 2nd derivative
        phisum=0.d0
C       do k=1,nterm
C         phisum=phisum+tiqui(icomp,k)*phiq02(k)
C       enddo
        phiqui=phisum
c
      elseif (itau.eq.2 .and. idel.eq.2) then
c  compute cross derivative using terms from 2nd derivative
        phisum=0.d0
C       do k=1,nterm
C         tik=tiqui(icomp,k)
C         phisum=phisum+(tik*tik-tik)*phiq02(k)
C       enddo
        phiqui=phisum
c
      end if
c
      drvsvq(icomp,ncode)=phiqui
      drvflq(icomp)(ncode:ncode)='1'
c
      RETURN
      end                                               !function PHIQUI
c
c ======================================================================
c
      subroutine CRTQUI (icomp,tc,pc,Dc)
c
c  returns critical parameters associated with Fundamental EOS
c
c  input:
c    icomp--pointer specifying component (1..nc)
c  outputs:
c    tc--critical temperature (K)
c    pc--critical pressure (kPa)
c    Dc--molar density (mol/L) at critical point
c
c  written by D.E. Cristancho, NIST Thermophysics Division, Boulder, Colorado
c  06-18-09 DEC, original version
c
      include 'commons.for'
c
      tc=tcqui(icomp)
      pc=pcqui(icomp)
      Dc=rhocq(icomp)
c
      RETURN
      end                                             !subroutine CRTQUI
c
c ======================================================================
c
      subroutine REDQUI (icomp,tred,Dred)
c
c  returns reducing parameters associated with Fundamental EOS;
c  used to calculate the 'tau' and 'del' which are the independent
c  variables in the EOS
c
c  input:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c  outputs:
c     tred--reducing temperature (K)
c     Dred--reducing molar density (mol/L)
c
c  written by D.E. Cristancho, NIST Thermophysics Division, Boulder, Colorado
c  06-18-09 DEC, original version
c
      include 'commons.for'
c
      tred=t0qui(icomp)
      Dred=rho0q(icomp)
c
      RETURN
      end                                             !subroutine REDQUI
c
c ======================================================================
c
      subroutine SETQUI (nread,icomp,hcasno,ierr,herr)
c
c  set up working arrays for use with Fundamental equation of state
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
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in common /WCFQUI/
c
c  written by D.E. Cristancho, NIST Thermophysics Division, Boulder, Colorado
c  06-18-09 DEC, original version
c
      include 'commons.for'
      character*12 hcasno
      character*255 herr
c
      ierr=0
      herr=' '
c  (re)initialize contents of /QUISAV/ when a new fluid is read in
      do i=n0,nx
        delsvq(i)=0.d0
        tausvq(i)=0.d0
        do j=1,mxtrm
          phisvq(i,j)=0.d0
          taupq(i,j)=0.d0
          delpq(i,j)=0.d0
        enddo
      enddo
c
      if (nread.le.0 .or. hcasno.eq.' ') then
c  get coefficients from block data
c  identify specified fluid with entries in database via match of CAS no
        ierr=1
        herr='[SETQUI error] fluid input to SETQUI not found'//hnull
      else
c  read data from file
        read (nread,*) tminq(icomp)     !lower temperature limit
        read (nread,*) tmaxq(icomp)     !upper temperature limit
        read (nread,*) pmaxq(icomp)     !upper pressure limit
        read (nread,*) Deos(icomp)      !upper density limit
        read (nread,2003) hmodcp(icomp) !pointer to Cp0 model
        read (nread,*) wmas(icomp)      !molecular weight
        wmfq(icomp)=wmas(icomp)
        read (nread,*) ttpn(icomp)      !triple point temperature
        read (nread,*) pminq(icomp)     !pressure at triple point
        read (nread,*) rhotpq(icomp)    !density at triple point
        read (nread,*) tnbp(icomp)      !normal boiling point temperature
        read (nread,*) accen(icomp)     !acentric factor
        read (nread,*) tcqui(icomp),pcqui(icomp),rhocq(icomp) !critical par
        tcrit(icomp)=tcqui(icomp)
        pcrit(icomp)=pcqui(icomp)
        Dcrit(icomp)=rhocq(icomp)
        ptpn(icomp)=pminq(icomp)
        dtpn(icomp)=rhotpq(icomp)
        dtpv(icomp)=0.d0
        dnbpl(icomp)=0.d0
        dnbpv(icomp)=0.d0
        read (nread,*) t0qui(icomp),rho0q(icomp) !reducing parameters
        tz(icomp)=t0qui(icomp)
        rhoz(icomp)=rho0q(icomp)
        read (nread,*) Rqui(icomp)    !gas constant used in fit
        if (nc.eq.1 .and. icomp.eq.1) R=Rqui(icomp)
        Reos(icomp)=Rqui(icomp)
        Zcrit(icomp)=pcqui(icomp)/(Rqui(icomp)
     &              *tcqui(icomp)*rhocq(icomp))
        read(nread,*)netaq(icomp),nepsq(icomp),nbbq(icomp),ngamq(icomp),
     &                 nbetq(icomp)
        nterm=netaq(icomp)+nepsq(icomp)+nbbq(icomp)+ngamq(icomp)
     &       +nbetq(icomp)
        do j=1,nterm
          read (nread,*) aqui(icomp,j),tiqui(icomp,j),diqui(icomp,j)
        enddo
      end if
c
c  copy limits into /EOSLIM/ arrays
      tmeos(icomp)=tminq(icomp)
      txeos(icomp)=tmaxq(icomp)
      peos(icomp)=pmaxq(icomp)
c     Deos(icomp)=rhomaxq(icomp)
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETQUI
c
c ======================================================================
c
c ======================================================================
c                                                    end file core_QUI.f
c ======================================================================
