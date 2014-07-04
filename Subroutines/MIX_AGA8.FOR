c  begin file Mix_AGA8.f
c
c  This file contains the routines implementing the AGA8 equation of
c  state for hydrocarbon mixtures
c
c  contained here are:
c     function PHIAGA (itau,idel,tau,del,x)
c     subroutine SETAGA (ierr,herr)
c     subroutine UNSETAGA
c     subroutine SETAG (x)
c     block data AGA8CF
c
c ======================================================================
c ======================================================================
c
      function PHIAGA (itau,idel,tau,del,x)
c
c  compute reduced Helmholtz energy or a derivative as functions
c  of dimensionless temperature and density for the AGA8
c  equation of state
c
c  based on the DETAIL compressibility factor equation of:
c  Starling, K.E. and Savidge, J.L.
c  Compressibility Factors of Natural Gas and Other Related Hydrocarbon
c  Gases,
c  Transmission Measurement Committee Report No. 8, Catalog No. XQ9212,
c  American Gas Association, 1994.
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
c  output (as function value):
c      phi--residual (real-gas) part of the AGA8 equation, or one
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
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  10-31-02 EWL, original version
c
      include 'commons.for'
      dimension a0(64),x(ncmax)
c
      call SETAG(x)
      call REDX (x,t0,rho0)
      t=t0/tau
      rho=del*rho0
      D=Kaga**3*rho
      d1=D
      d2=D*D
      d3=d2*D
      d4=d3*D
      d5=d4*D
      d6=d5*D
      d7=d6*D
      d8=d7*D
      d9=d8*D
      e0=exp(0.d0)
      e1=exp(-d1)
      e2=exp(-d2)
      e3=exp(-d3)
      e4=exp(-d4)
      t5    =sqrt(t)
      t15   =t5*t
      t10   =t
      t20   =t*t
      t30   =t20*t
      t40   =t30*t
      t50   =t40*t
      t60   =t50*t
      t70   =t60*t
      t80   =t70*t
      t90   =t80*t
      t35   =t30*t5
      t45   =t40*t5
      t75   =t70*t5
      t95   =t90*t5
      t110  =t90*t20
      t120  =t110*t
      t125  =t120*t5
      t210  =t120*t90
      t220  =t210*t
      t230  =t220*t
      tn5   =1.d0/t5
      tn130 =1.d0/t120/t
      tn10  =1.d0/t
      tn60  =1.d0/t60
c
      a0(1) =D* bsaga(1)
      a0(2) =D* bsaga(2)          /t5
      a0(3) =D* bsaga(3)          /t10
      a0(4) =D* bsaga(4)          /t35
      a0(5) =D* bsaga(5)          /tn5
      a0(6) =D* bsaga(6)          /t45
      a0(7) =D* bsaga(7)          /t5
      a0(8) =D* bsaga(8)          /t75
      a0(9) =D* bsaga(9)          /t95
      a0(10)=D* bsaga(10)         /t60
      a0(11)=D* bsaga(11)         /t120
      a0(12)=D* bsaga(12)         /t125
      a0(59)=D*(bsaga(13)-cnsaga(13))/tn60
      a0(60)=D*(bsaga(14)-cnsaga(14))/t20
      a0(61)=D*(bsaga(15)-cnsaga(15))/t30
      a0(62)=D*(bsaga(16)-cnsaga(16))/t20
      a0(63)=D*(bsaga(17)-cnsaga(17))/t20
      a0(64)=D*(bsaga(18)-cnsaga(18))/t110
c
      a0(13)=cnsaga(13)/tn60 *d1*e3
      a0(14)=cnsaga(14)/t20  *d1*e2
      a0(15)=cnsaga(15)/t30  *d1*e2
      a0(16)=cnsaga(16)/t20  *d1*e2
      a0(17)=cnsaga(17)/t20  *d1*e4
      a0(18)=cnsaga(18)/t110 *d1*e4
      a0(19)=cnsaga(19)/tn5  *d2*e0
      a0(20)=cnsaga(20)/t5   *d2*e0
      a0(21)=cnsaga(21)      *d2*e2
      a0(22)=cnsaga(22)/t40  *d2*e2
      a0(23)=cnsaga(23)/t60  *d2*e2
      a0(24)=cnsaga(24)/t210 *d2*e4
      a0(25)=cnsaga(25)/t230 *d2*e4
      a0(26)=cnsaga(26)/t220 *d2*e4
      a0(27)=cnsaga(27)/tn10 *d2*e4
      a0(28)=cnsaga(28)/tn5  *d3*e0
      a0(29)=cnsaga(29)/t70  *d3*e1
      a0(30)=cnsaga(30)/tn10 *d3*e1
      a0(31)=cnsaga(31)/t60  *d3*e2
      a0(32)=cnsaga(32)/t40  *d3*e2
      a0(33)=cnsaga(33)/t10  *d3*e3
      a0(34)=cnsaga(34)/t90  *d3*e3
      a0(35)=cnsaga(35)/tn130*d3*e4
      a0(36)=cnsaga(36)/t210 *d3*e4
      a0(37)=cnsaga(37)/t80  *d3*e4
      a0(38)=cnsaga(38)/tn5  *d4*e0
      a0(39)=cnsaga(39)      *d4*e0
      a0(40)=cnsaga(40)/t20  *d4*e2
      a0(41)=cnsaga(41)/t70  *d4*e2
      a0(42)=cnsaga(42)/t90  *d4*e2
      a0(43)=cnsaga(43)/t220 *d4*e4
      a0(44)=cnsaga(44)/t230 *d4*e4
      a0(45)=cnsaga(45)/t10  *d5*e0
      a0(46)=cnsaga(46)/t90  *d5*e2
      a0(47)=cnsaga(47)/t30  *d5*e2
      a0(48)=cnsaga(48)/t80  *d5*e4
      a0(49)=cnsaga(49)/t230 *d5*e4
      a0(50)=cnsaga(50)/t15  *d6*e0
      a0(51)=cnsaga(51)/t50  *d6*e2
      a0(52)=cnsaga(52)/tn5  *d7*e0
      a0(53)=cnsaga(53)/t40  *d7*e2
      a0(54)=cnsaga(54)/t70  *d8*e1
      a0(55)=cnsaga(55)/t30  *d8*e2
      a0(56)=cnsaga(56)      *d8*e2
      a0(57)=cnsaga(57)/t10  *d9*e2
      a0(58)=cnsaga(58)      *d9*e2
c
      if (idel.eq.1) then
        a0(19)=a0(19)*2d0
        a0(20)=a0(20)*2d0
        a0(28)=a0(28)*3d0
        a0(38)=a0(38)*4d0
        a0(39)=a0(39)*4d0
        a0(45)=a0(45)*5d0
        a0(50)=a0(50)*6d0
        a0(52)=a0(52)*7d0
        a0(13)=a0(13)*(1d0-3d0*d3)
        a0(14)=a0(14)*(1d0-2d0*d2)
        a0(15)=a0(15)*(1d0-2d0*d2)
        a0(16)=a0(16)*(1d0-2d0*d2)
        a0(17)=a0(17)*(1d0-4d0*d4)
        a0(18)=a0(18)*(1d0-4d0*d4)
        a0(21)=a0(21)*(2d0-2d0*d2)
        a0(22)=a0(22)*(2d0-2d0*d2)
        a0(23)=a0(23)*(2d0-2d0*d2)
        a0(24)=a0(24)*(2d0-4d0*d4)
        a0(25)=a0(25)*(2d0-4d0*d4)
        a0(26)=a0(26)*(2d0-4d0*d4)
        a0(27)=a0(27)*(2d0-4d0*d4)
        a0(29)=a0(29)*(3d0-    d1)
        a0(30)=a0(30)*(3d0-    d1)
        a0(31)=a0(31)*(3d0-2d0*d2)
        a0(32)=a0(32)*(3d0-2d0*d2)
        a0(33)=a0(33)*(3d0-3d0*d3)
        a0(34)=a0(34)*(3d0-3d0*d3)
        a0(35)=a0(35)*(3d0-4d0*d4)
        a0(36)=a0(36)*(3d0-4d0*d4)
        a0(37)=a0(37)*(3d0-4d0*d4)
        a0(40)=a0(40)*(4d0-2d0*d2)
        a0(41)=a0(41)*(4d0-2d0*d2)
        a0(42)=a0(42)*(4d0-2d0*d2)
        a0(43)=a0(43)*(4d0-4d0*d4)
        a0(44)=a0(44)*(4d0-4d0*d4)
        a0(46)=a0(46)*(5d0-2d0*d2)
        a0(47)=a0(47)*(5d0-2d0*d2)
        a0(48)=a0(48)*(5d0-4d0*d4)
        a0(49)=a0(49)*(5d0-4d0*d4)
        a0(51)=a0(51)*(6d0-2d0*d2)
        a0(53)=a0(53)*(7d0-2d0*d2)
        a0(54)=a0(54)*(8d0-    d1)
        a0(55)=a0(55)*(8d0-2d0*d2)
        a0(56)=a0(56)*(8d0-2d0*d2)
        a0(57)=a0(57)*(9d0-2d0*d2)
        a0(58)=a0(58)*(9d0-2d0*d2)
c
      elseif (idel.eq.2) then
        do n=1,12
          a0(n)=0
        enddo
        do n=59,64
          a0(n)=0
        enddo
        a0(19)=a0(19)*2d0
        a0(20)=a0(20)*2d0
        a0(28)=a0(28)*6d0
        a0(38)=a0(38)*12d0
        a0(39)=a0(39)*12d0
        a0(45)=a0(45)*20d0
        a0(50)=a0(50)*30d0
        a0(52)=a0(52)*42d0
        a0(13)=a0(13)*((1d0-3d0*d3)*(   -3d0*d3)- 9d0*d3)
        a0(14)=a0(14)*((1d0-2d0*d2)*(   -2d0*d2)- 4d0*d2)
        a0(15)=a0(15)*((1d0-2d0*d2)*(   -2d0*d2)- 4d0*d2)
        a0(16)=a0(16)*((1d0-2d0*d2)*(   -2d0*d2)- 4d0*d2)
        a0(17)=a0(17)*((1d0-4d0*d4)*(   -4d0*d4)-16d0*d4)
        a0(18)=a0(18)*((1d0-4d0*d4)*(   -4d0*d4)-16d0*d4)
        a0(21)=a0(21)*((2d0-2d0*d2)*(1d0-2d0*d2)- 4d0*d2)
        a0(22)=a0(22)*((2d0-2d0*d2)*(1d0-2d0*d2)- 4d0*d2)
        a0(23)=a0(23)*((2d0-2d0*d2)*(1d0-2d0*d2)- 4d0*d2)
        a0(24)=a0(24)*((2d0-4d0*d4)*(1d0-4d0*d4)-16d0*d4)
        a0(25)=a0(25)*((2d0-4d0*d4)*(1d0-4d0*d4)-16d0*d4)
        a0(26)=a0(26)*((2d0-4d0*d4)*(1d0-4d0*d4)-16d0*d4)
        a0(27)=a0(27)*((2d0-4d0*d4)*(1d0-4d0*d4)-16d0*d4)
        a0(29)=a0(29)*((3d0-    d1)*(2d0-    d1)-     d1)
        a0(30)=a0(30)*((3d0-    d1)*(2d0-    d1)-     d1)
        a0(31)=a0(31)*((3d0-2d0*d2)*(2d0-2d0*d2)- 4d0*d2)
        a0(32)=a0(32)*((3d0-2d0*d2)*(2d0-2d0*d2)- 4d0*d2)
        a0(33)=a0(33)*((3d0-3d0*d3)*(2d0-3d0*d3)- 9d0*d3)
        a0(34)=a0(34)*((3d0-3d0*d3)*(2d0-3d0*d3)- 9d0*d3)
        a0(35)=a0(35)*((3d0-4d0*d4)*(2d0-4d0*d4)-16d0*d4)
        a0(36)=a0(36)*((3d0-4d0*d4)*(2d0-4d0*d4)-16d0*d4)
        a0(37)=a0(37)*((3d0-4d0*d4)*(2d0-4d0*d4)-16d0*d4)
        a0(40)=a0(40)*((4d0-2d0*d2)*(3d0-2d0*d2)- 4d0*d2)
        a0(41)=a0(41)*((4d0-2d0*d2)*(3d0-2d0*d2)- 4d0*d2)
        a0(42)=a0(42)*((4d0-2d0*d2)*(3d0-2d0*d2)- 4d0*d2)
        a0(43)=a0(43)*((4d0-4d0*d4)*(3d0-4d0*d4)-16d0*d4)
        a0(44)=a0(44)*((4d0-4d0*d4)*(3d0-4d0*d4)-16d0*d4)
        a0(46)=a0(46)*((5d0-2d0*d2)*(4d0-2d0*d2)- 4d0*d2)
        a0(47)=a0(47)*((5d0-2d0*d2)*(4d0-2d0*d2)- 4d0*d2)
        a0(48)=a0(48)*((5d0-4d0*d4)*(4d0-4d0*d4)-16d0*d4)
        a0(49)=a0(49)*((5d0-4d0*d4)*(4d0-4d0*d4)-16d0*d4)
        a0(51)=a0(51)*((6d0-2d0*d2)*(5d0-2d0*d2)- 4d0*d2)
        a0(53)=a0(53)*((7d0-2d0*d2)*(6d0-2d0*d2)- 4d0*d2)
        a0(54)=a0(54)*((8d0-    d1)*(7d0-    d1)-     d1)
        a0(55)=a0(55)*((8d0-2d0*d2)*(7d0-2d0*d2)- 4d0*d2)
        a0(56)=a0(56)*((8d0-2d0*d2)*(7d0-2d0*d2)- 4d0*d2)
        a0(57)=a0(57)*((9d0-2d0*d2)*(8d0-2d0*d2)- 4d0*d2)
        a0(58)=a0(58)*((9d0-2d0*d2)*(8d0-2d0*d2)- 4d0*d2)
c
      elseif (idel.eq.3) then
        do n=1,12
          a0(n)=0
        enddo
        do n=59,64
          a0(n)=0
        enddo
        a0(19)=0.d0
        a0(20)=0.d0
        a0(28)=a0(28)*6d0
        a0(38)=a0(38)*24d0
        a0(39)=a0(39)*24d0
        a0(45)=a0(45)*60d0
        a0(50)=a0(50)*120d0
        a0(52)=a0(52)*210d0
        d12=d6*d6
        a0(13)=a0(13)*(  0d0 - d3*24d0  + d6*81d0  - 27d0*d9)
        a0(14)=a0(14)*(  0d0 - d2*6d0   + d4*24d0  - 8d0 *d6)
        a0(15)=a0(15)*(  0d0 - d2*6d0   + d4*24d0  - 8d0 *d6)
        a0(16)=a0(16)*(  0d0 - d2*6d0   + d4*24d0  - 8d0 *d6)
        a0(17)=a0(17)*(  0d0 - d4*60d0  + d8*192d0 - 64d0*d12)
        a0(18)=a0(18)*(  0d0 - d4*60d0  + d8*192d0 - 64d0*d12)
        a0(21)=a0(21)*(  0d0 - d2*24d0  + d4*36d0  - 8d0 *d6)
        a0(22)=a0(22)*(  0d0 - d2*24d0  + d4*36d0  - 8d0 *d6)
        a0(23)=a0(23)*(  0d0 - d2*24d0  + d4*36d0  - 8d0 *d6)
        a0(24)=a0(24)*(  0d0 - d4*120d0 + d8*240d0 - 64d0*d12)
        a0(25)=a0(25)*(  0d0 - d4*120d0 + d8*240d0 - 64d0*d12)
        a0(26)=a0(26)*(  0d0 - d4*120d0 + d8*240d0 - 64d0*d12)
        a0(27)=a0(27)*(  0d0 - d4*120d0 + d8*240d0 - 64d0*d12)
        a0(29)=a0(29)*(  6d0 - d1*18d0  + d2*9d0   - 1d0 *d3)
        a0(30)=a0(30)*(  6d0 - d1*18d0  + d2*9d0   - 1d0 *d3)
        a0(31)=a0(31)*(  6d0 - d2*54d0  + d4*48d0  - 8d0 *d6)
        a0(32)=a0(32)*(  6d0 - d2*54d0  + d4*48d0  - 8d0 *d6)
        a0(33)=a0(33)*(  6d0 - d3*114d0 + d6*135d0 - 27d0*d9)
        a0(34)=a0(34)*(  6d0 - d3*114d0 + d6*135d0 - 27d0*d9)
        a0(35)=a0(35)*(  6d0 - d4*204d0 + d8*288d0 - 64d0*d12)
        a0(36)=a0(36)*(  6d0 - d4*204d0 + d8*288d0 - 64d0*d12)
        a0(37)=a0(37)*(  6d0 - d4*204d0 + d8*288d0 - 64d0*d12)
        a0(40)=a0(40)*( 24d0 - d2*96d0  + d4*60d0  - 8d0 *d6)
        a0(41)=a0(41)*( 24d0 - d2*96d0  + d4*60d0  - 8d0 *d6)
        a0(42)=a0(42)*( 24d0 - d2*96d0  + d4*60d0  - 8d0 *d6)
        a0(43)=a0(43)*( 24d0 - d4*312d0 + d8*336d0 - 64d0*d12)
        a0(44)=a0(44)*( 24d0 - d4*312d0 + d8*336d0 - 64d0*d12)
        a0(46)=a0(46)*( 60d0 - d2*150d0 + d4*72d0  - 8d0 *d6)
        a0(47)=a0(47)*( 60d0 - d2*150d0 + d4*72d0  - 8d0 *d6)
        a0(48)=a0(48)*( 60d0 - d4*444d0 + d8*384d0 - 64d0*d12)
        a0(49)=a0(49)*( 60d0 - d4*444d0 + d8*384d0 - 64d0*d12)
        a0(51)=a0(51)*(120d0 - d2*216d0 + d4*84d0  - 8d0 *d6)
        a0(53)=a0(53)*(210d0 - d2*294d0 + d4*96d0  - 8d0 *d6)
        a0(54)=a0(54)*(336d0 - d1*168d0 + d2*24d0  - 1d0 *d3)
        a0(55)=a0(55)*(336d0 - d2*384d0 + d4*108d0 - 8d0 *d6)
        a0(56)=a0(56)*(336d0 - d2*384d0 + d4*108d0 - 8d0 *d6)
        a0(57)=a0(57)*(504d0 - d2*486d0 + d4*120d0 - 8d0 *d6)
        a0(58)=a0(58)*(504d0 - d2*486d0 + d4*120d0 - 8d0 *d6)
      endif
c
      if (itau.eq.1) then
        do n=1,58
          a0(n)=a0(n)*unaga(n)
        enddo
        do n=59,64
          a0(n)=a0(n)*unaga(n-46)
        enddo
      elseif (itau.eq.2) then
        do n=1,58
          a0(n)=a0(n)*unaga(n)*(unaga(n)-1.d0)
        enddo
        do n=59,64
          a0(n)=a0(n)*unaga(n-46)*(unaga(n-46)-1.d0)
        enddo
      endif
c
      ar=0.d0
      do n=1,64
        ar=ar+a0(n)
      enddo
      phiaga=ar
c
      RETURN
      end                                               !function PHIAGA
c
c ======================================================================
c
      subroutine SETAGA (ierr,herr)
c
c  set up working arrays for use with AGA8 equation of state
c
c  input:
c  outputs:
c     ierr--error flag:  0 = successful
c                        1 = error (e.g. fluid not found)
c     herr--error string (character*255 variable if ierr<>0)
c     [fluid parameters, etc. returned via various common blocks]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  10-31-02 EWL, original version
c  09-30-11 EWL, delete calculation of href and sref so that h=0 and s=0
c                at 298.15 K and 101.325 kPa
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SETAGA
c
      include 'commons.for'
      character*255 herr
c
      ierr=0
      herr=' '
c
      do i=1,nc
        ifpa(i)=0
        if (hcas(i).eq.'74-82-8')    ifpa(i)=1   !Methane
        if (hcas(i).eq.'7727-37-9')  ifpa(i)=2   !Nitrogen
        if (hcas(i).eq.'124-38-9')   ifpa(i)=3   !Carbon Dioxide
        if (hcas(i).eq.'74-84-0')    ifpa(i)=4   !Ethane
        if (hcas(i).eq.'74-98-6')    ifpa(i)=5   !Propane
        if (hcas(i).eq.'7732-18-5')  ifpa(i)=6   !Water
        if (hcas(i).eq.'7783-06-4')  ifpa(i)=7   !Hydrogen Sulfide
        if (hcas(i).eq.'1333-74-0')  ifpa(i)=8   !Hydrogen
        if (hcas(i).eq.'1333-74-0p') ifpa(i)=8   !Hydrogen (para)
        if (hcas(i).eq.'630-08-0')   ifpa(i)=9   !Carbon Monoxide
        if (hcas(i).eq.'7782-44-7')  ifpa(i)=10  !Oxygen
        if (hcas(i).eq.'75-28-5')    ifpa(i)=11  !Isobutane
        if (hcas(i).eq.'106-97-8')   ifpa(i)=12  !Butane
        if (hcas(i).eq.'78-78-4')    ifpa(i)=13  !Isopentane
        if (hcas(i).eq.'109-66-0')   ifpa(i)=14  !Pentane
        if (hcas(i).eq.'110-54-3')   ifpa(i)=15  !Hexane
        if (hcas(i).eq.'142-82-5')   ifpa(i)=16  !Heptane
        if (hcas(i).eq.'111-65-9')   ifpa(i)=17  !Octane
        if (hcas(i).eq.'111-84-2')   ifpa(i)=18  !Nonane
        if (hcas(i).eq.'124-18-5')   ifpa(i)=19  !Decane
        if (hcas(i).eq.'7440-59-7')  ifpa(i)=20  !Helium
        if (hcas(i).eq.'7440-37-1')  ifpa(i)=21  !Argon
        if (hcas(i).eq.'463-82-1')   ifpa(i)=13  !Set neopentane as isopentane
        if (hcas(i).eq.'108-88-3')   ifpa(i)=16  !Set toluene as heptane
        if (hcas(i).eq.'71-43-2')    ifpa(i)=15  !Set benzene as hexane
        if (hcas(i).eq.'74-85-1')    ifpa(i)=4   !Set ethylene as ethane
        if (hcas(i).eq.'115-07-1')   ifpa(i)=5   !Set propylene as propane
        if (hcas(i).eq.'106-98-9')   ifpa(i)=12  !Set butene as butane
        if (ifpa(i).eq.0) then
          ierr=1
          herr='[SETAGA error 1] Not all requested fluids are '//
     &         'available in AGA8, '//hcas(i)//hnull
          call ERRMSG (ierr,herr)
          RETURN
        endif
      enddo
c
      if (heos.ne.'AGA') then
        hagasv=heos
        Rasv=R
        do i=1,nc
          Ragasv(i)   = Reos(i)
          wmagasv(i)  = wmas(i)
          tmagasv(i)  = tmincp(i)
          txagasv(i)  = tmaxcp(i)
          tragasv(i)  = tredcp(i)
          Cragasv(i)  = Credcp(i)
          nagacsv(i)  = ntermc(i)
          nagaesv(i)  = nterme(i)
          ncoshsv(i)  = ncoshc(i)
          nsinhsv(i)  = nsinhc(i)
          Tfagasv(i)  = tref(i)
          Dfagasv(i)  = rhoref(i)
          hfagasv(i)  = href(i)
          sfagasv(i)  = sref(i)
          do j=1,5
            xksv(i,j) = xkc0(i,j)
            xthsv(i,j)= x0th(i,j)
            xhsv(i,j) = xh00(i,j)
            cpcsv(i,j)= cpc0(i,j)
            cphsv(i,j)= cp0h(i,j)
          enddo
        enddo
      endif
c
      heos='AGA'
      R=8.31451d0
      do i=1,nc
        Reos(i)=8.31451d0
      enddo
c
      do i=1,21
        kb2a(i)=kbaga(i)**2.5d0
        eb2a(i)=ebaga(i)**2.5d0
      enddo
      do i=1,20
        do j=i+1,21
          kij2(i,j)=2.d0*(kija(j,i)**5-1.d0)*kb2a(i)*kb2a(j)
          uij2(i,j)=2.d0*(uija(j,i)**5-1.d0)*eb2a(i)*eb2a(j)
          gij2(i,j)=(gijs(j,i)-1.d0)*(gbaga(i)+gbaga(j))
          kij2(j,i)=kij2(i,j)
          uij2(j,i)=uij2(i,j)
          gij2(j,i)=gij2(i,j)
        enddo
      enddo
      do n=1,18
        do i=1,21
        do j=i,21
          eij=eijs(j,i)*sqrt(ebaga(i)*ebaga(j))
          bb=1.d0
          if (n.eq.5 .or. n.eq.6)  bb=gijs(j,i)*(gbaga(i)+gbaga(j))/2.d0
          if (n.eq.7 .or. n.eq.16) bb=qbaga(i)*qbaga(j)
          if (n.eq.8 .or. n.eq.9)  bb=sbaga(i)*sbaga(j)
          if (n.ge.10.and.n.le.12) bb=wbaga(i)*wbaga(j)
          if (n.eq.13)             bb=fbaga(i)*fbaga(j)
          if (i.ne.j) bb=2.d0*bb
          bs2a(n,i,j)=bb*eij**unaga(n)*(kbaga(i)*kbaga(j))**1.5d0
          bs2a(n,j,i)=bs2a(n,i,j)
        enddo
        enddo
      enddo
      do i=1,nc
        wmas(i)=mrbaga(ifpa(i))
        tmincp(i)=100.d0
        tmaxcp(i)=1000.d0
        tredcp(i)=1.d0
        Credcp(i)=4.184d0
        ntermc(i)=1
        nterme(i)=0
        ncoshc(i)=2
        nsinhc(i)=2
        cpc0(i,1)=bcp0(ifpa(i))
        xkc0(i,1)=0.d0
        do j=2,5
          xkc0(i,j)=-2.d0
          x0th(i,j)=-1.d0
          xh00(i,j)=-2.d0
        enddo
        j=ifpa(i)
        cpc0(i,2)=ecp0(j)*fcp0(j)**2
        cp0h(i,2)=fcp0(j)
        cpc0(i,3)=icp0(j)*jcp0(j)**2
        cp0h(i,3)=jcp0(j)
        cpc0(i,4)=ccp0(j)*dcp0(j)**2
        cp0h(i,4)=dcp0(j)
        cpc0(i,5)=gcp0(j)*hcp0(j)**2
        cp0h(i,5)=hcp0(j)
        tref(i)=298.15d0
        rhoref(i)=101.325d0/r/tref(i)
!       t=tref(i)
!        h=acp0(j)+bcp0(j)*t
!     &   +ccp0(j)*dcp0(j)/TANH(dcp0(j)/t)
!     &   -ecp0(j)*fcp0(j)*TANH(fcp0(j)/t)
!     &   +gcp0(j)*hcp0(j)/TANH(hcp0(j)/t)
!     &   -icp0(j)*jcp0(j)*TANH(jcp0(j)/t)
!        s=kcp0(j)+bcp0(j)*LOG(t)
!     &   +ccp0(j)*(dcp0(j)/t/TANH(dcp0(j)/t)-LOG(SINH(dcp0(j)/t)))
!     &   -ecp0(j)*(fcp0(j)/t*TANH(fcp0(j)/t)-LOG(COSH(fcp0(j)/t)))
!     &   +gcp0(j)*(hcp0(j)/t/TANH(hcp0(j)/t)-LOG(SINH(hcp0(j)/t)))
!     &   -icp0(j)*(jcp0(j)/t*TANH(jcp0(j)/t)-LOG(COSH(jcp0(j)/t)))
!        href(i)=0-h*4.184d0
!        sref(i)=0-s*4.184d0
        href(i)=0.d0
        sref(i)=0.d0
      enddo
c
      RETURN
      end                                             !subroutine SETAGA
c
c ======================================================================
c
      subroutine UNSETAGA
c
c  Load original values into arrays changed in the call to SETAGA.  This
c  routine resets the values back to those loaded when SETUP was called.
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-07-10 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: UNSETAGA
c
      include 'commons.for'
c
      call RESETA
      if (heos.eq.'AGA') then
        heos=hagasv
        R=Rasv
        do i=1,nc
          Reos(i)   = Ragasv(i)
          wmas(i)     = wmagasv(i)
          tmincp(i) = tmagasv(i)
          tmaxcp(i) = txagasv(i)
          tredcp(i) = tragasv(i)
          Credcp(i) = Cragasv(i)
          ntermc(i) = nagacsv(i)
          nterme(i) = nagaesv(i)
          ncoshc(i) = ncoshsv(i)
          nsinhc(i) = nsinhsv(i)
          tref(i)   = Tfagasv(i)
          rhoref(i) = Dfagasv(i)
          href(i)   = hfagasv(i)
          sref(i)   = sfagasv(i)
          do j=1,5
            xkc0(i,j) = xksv(i,j)
            x0th(i,j) = xthsv(i,j)
            xh00(i,j)  = xhsv(i,j)
            cpc0(i,j) = cpcsv(i,j)
            cp0h(i,j) = cphsv(i,j)
          enddo
        enddo
      endif
c
      RETURN
      end                                           !subroutine UNSETAGA
c
c ======================================================================
c
      subroutine SETAG (x)
c
c  set up working arrays for use with the AGA8 equation of state
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  10-31-02 EWL, original version
c
      include 'commons.for'
      double precision K3
      dimension x(ncmax),z(ncmax)
c
      sum=0.d0
      do i=1,nc
        sum=sum+x(i)
      enddo
      if (sum.le.0.d0) RETURN
      do i=1,nc
        z(i)=x(i)/sum
      enddo
      Kaga=0.d0
      F=0.d0
      U=0.d0
      Q=0.d0
      G=0.d0
      do i=1,nc
        Kaga=Kaga+z(i)*kb2a(ifpa(i))
        U=U+z(i)*eb2a(ifpa(i))
        F=F+z(i)**2*fbaga(ifpa(i))
        Q=Q+z(i)*qbaga(ifpa(i))
        G=G+z(i)*gbaga(ifpa(i))
      enddo
      Kaga=Kaga**2
      U=U**2
      do i=1,nc-1
        do j=i+1,nc
          xij=z(i)*z(j)
          Kaga=Kaga+xij*kij2(ifpa(i),ifpa(j))
          U=U+xij*uij2(ifpa(i),ifpa(j))
          G=G+xij*gij2(ifpa(i),ifpa(j))
        enddo
      enddo
      Kaga=Kaga**0.2d0
      U=U**0.2d0
c
      Q=Q**2
      K3=Kaga**3
      do n=1,18
        bsaga(n)=0.d0
        do i=1,nc
        do j=i,nc
          bsaga(n)=bsaga(n)+bs2a(n,ifpa(i),ifpa(j))*z(i)*z(j)
        enddo
        enddo
        bsaga(n)=bsaga(n)*anaga(n)/K3
      enddo
      do n=13,58
        cnsaga(n)=anaga(n)*U**unaga(n)
      enddo
      cnsaga(13)=cnsaga(13)*F
      cnsaga(16)=cnsaga(16)*Q
      cnsaga(25)=cnsaga(25)*G
      cnsaga(26)=cnsaga(26)*Q
      cnsaga(27)=cnsaga(27)*F
      cnsaga(28)=cnsaga(28)*Q
      cnsaga(29)=cnsaga(29)*G
      cnsaga(30)=cnsaga(30)*F
      cnsaga(32)=cnsaga(32)*G
      cnsaga(33)=cnsaga(33)*G
      cnsaga(34)=cnsaga(34)*G
      cnsaga(35)=cnsaga(35)*F
      cnsaga(37)=cnsaga(37)*Q
      cnsaga(42)=cnsaga(42)*Q
      cnsaga(47)=cnsaga(47)*Q
      cnsaga(49)=cnsaga(49)*Q
      cnsaga(51)=cnsaga(51)*G
      cnsaga(52)=cnsaga(52)*Q
      cnsaga(54)=cnsaga(54)*G
      cnsaga(56)=cnsaga(56)*G
      cnsaga(58)=cnsaga(58)*Q
c
      RETURN
      end                                              !subroutine SETAG
c
c ======================================================================
c
c     block data AGA8CF
c
c  AGA8 equation of state coefficients taken from the DETAIL
c  compressibility factor equation of:
c
c  Starling, K.E. and Savidge, J.L.
c  Compressibility Factors of Natural Gas and Other Related Hydrocarbon
c  Gases,
c  Transmission Measurement Committee Report No. 8, Catalog No. XQ9212,
c  American Gas Association, 1994.
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  10-31-02 EWL, original version
c
c     include 'commons.for'
c
c     data anaga/0.1538326d0,1.341953d0,-2.998583d0,-0.04831228d0,
c    & 0.3757965d0,-1.589575d0,-0.05358847d0,0.88659463d0,-0.71023704d0,
c    & -1.471722d0,1.32185035d0,-0.78665925d0,2.29129D-9,0.1576724d0,
c    & -0.4363864d0,-0.04408159d0,-0.003433888d0,0.03205905d0,
c    & 0.02487355d0,0.07332279d0,-0.001600573d0,0.6424706d0,
c    & -0.4162601d0,-0.06689957d0,0.2791795d0,-0.6966051d0,
c    & -0.002860589d0,-0.008098836d0,3.150547d0,0.007224479d0,
c    & -0.7057529d0,0.5349792d0,-0.07931491d0,-1.418465d0,
c    & -5.99905D-17,0.1058402d0,0.03431729d0,-0.007022847d0,
c    & 0.02495587d0,0.04296818d0,0.7465453d0,-0.2919613d0,7.294616d0,
c    & -9.936757d0,-0.005399808d0,-0.2432567d0,0.04987016d0,
c    & 0.003733797d0,1.874951d0,0.002168144d0,-0.6587164d0,
c    & 0.000205518d0,0.009776195d0,-0.02048708d0,0.01557322d0,
c    & 0.006862415d0,-0.001226752d0,0.002850908d0/
c     data bnaga/18*1,9*2,10*3,7*4,5*5,2*6,2*7,3*8,2*9/
c     data cnaga/12*0,6*1,2*0,7*1,0,9*1,2*0,5*1,0,4*1,0,1,0,6*1/
c     data knaga/12*0,3,3*2,2*4,2*0,3*2,4*4,0,2*1,2*2,2*3,3*4,2*0,3*2,2*4,
c    & 0,2*2,2*4,0,2,0,2,1,4*2/
c     data unaga/0d0,0.5d0,1d0,3.5d0,-0.5d0,4.5d0,0.5d0,7.5d0,9.5d0,6d0,
c    & 12d0,12.5d0,-6d0,2d0,3d0,2d0,2d0,11d0,-0.5d0,0.5d0,0d0,4d0,6d0,
c    & 21d0,23d0,22d0,-1d0,-0.5d0,7d0,-1d0,6d0,4d0,1d0,9d0,-13d0,21d0,
c    & 8d0,-0.5d0,0d0,2d0,7d0,9d0,22d0,23d0,1d0,9d0,3d0,8d0,23d0,1.5d0,
c    & 5d0,-0.5d0,4d0,7d0,3d0,0d0,1d0,0d0/
c     data gnaga/4*0,2*1,18*0,1,3*0,1,2*0,3*1,16*0,1,2*0,1,0,1,2*0/
c     data qnaga/6*0,1,8*0,1,9*0,1,0,1,8*0,1,4*0,1,4*0,1,0,1,2*0,1,5*0,1/
c     data fnaga/12*0,1,13*0,1,2*0,1,4*0,1,23*0/
c     data snaga/7*0,2*1,49*0/
c     data wnaga/9*0,3*1,46*0/
c
c     data mrbaga/16.043d0,28.0135d0,44.01d0,30.07d0,44.097d0,18.0153d0,
c    & 34.082d0,2.0159d0,28.01d0,31.9988d0,58.123d0,58.123d0,72.15d0,
c    & 72.15d0,86.177d0,100.204d0,114.231d0,128.258d0,142.285d0,
c    & 4.0026d0,39.948d0/
c     data ebaga/151.3183d0,99.73778d0,241.9606d0,244.1667d0,298.1183d0,
c    & 514.0156d0,296.355d0,26.95794d0,105.5348d0,122.7667d0,324.0689d0,
c    & 337.6389d0,365.5999d0,370.6823d0,402.636293d0,427.72263d0,
c    & 450.325022d0,470.840891d0,489.558373d0,2.610111d0,119.6299d0/
c     data kbaga/0.4619255d0,0.4479153d0,0.4557489d0,0.5279209d0,
c    & 0.583749d0,0.3825868d0,0.4618263d0,0.3514916d0,0.4533894d0,
c    & 0.4186954d0,0.6406937d0,0.6341423d0,0.6738577d0,0.6798307d0,
c    & 0.7175118d0,0.7525189d0,0.784955d0,0.8152731d0,0.8437826d0,
c    & 0.3589888d0,0.4216551d0/
c     data gbaga/0.d0,0.027815d0,0.189065d0,0.0793d0,
c    & 0.141239d0,0.3325d0,
c    & 0.0885d0,0.034369d0,0.038953d0,0.021d0,0.256692d0,0.281835d0,
c    & 0.332267d0,0.366911d0,0.289731d0,0.337542d0,0.383381d0,
c    & 0.427354d0,0.469659d0,0.d0,0.d0/
c     data qbaga/2*0.d0,0.69d0,2*0.d0,1.06775d0,0.633276d0,14*0.d0/
c     data fbaga/7*0.d0,1.d0,13*0.d0/
c     data sbaga/5*0.d0,1.5822d0,0.39d0,14*0.d0/
c     data wbaga/5*0.d0,1.d0,15*0.d0/
c
c  Binary interaction parameter values
c     data (eijs(1,j),j=1,21)/1.d0,0.97164d0,0.960644d0,1.d0,0.994635d0,
c    & 0.708218d0,0.931484d0,1.17052d0,0.990126d0,1.d0,1.01953d0,
c    & 0.989844d0,1.00235d0,0.999268d0,1.107274d0,0.88088d0,0.880973d0,
c    & 0.881067d0,0.881161d0,1.d0,1.d0/
c     data (eijs(2,j),j=1,21)/2*1.d0,1.02274d0,0.97012d0,0.945939d0,
c    & 0.746954d0,0.902271d0,1.08632d0,1.00571d0,1.021d0,0.946914d0,
c    & 0.973384d0,0.95934d0,0.94552d0,7*1.d0/
c     data (eijs(3,j),j=1,21)/3*1.d0,0.925053d0,0.960237d0,0.849408d0,
c    & 0.955052d0,1.28179d0,1.5d0,1.d0,0.906849d0,0.897362d0,0.726255d0,
c    & 0.859764d0,0.855134d0,0.831229d0,0.80831d0,0.786323d0,0.765171d0,
c    & 2*1.d0/
c     data (eijs(4,j),j=1,21)/4*1.d0,1.02256d0,0.693168d0,0.946871d0,
c    & 1.16446d0,3*1.d0,1.01306d0,1.d0,1.00532d0,7*1.d0/
c     data (eijs(5,j),j=1,21)/7*1.d0,1.034787d0,3*1.d0,1.0049d0,9*1.d0/
c     data (eijs(6,j),j=1,21)/21*1.d0/
c     data (eijs(7,j),j=1,21)/14*1.d0,1.008692d0,1.010126d0,1.011501d0,
c    & 1.012821d0,1.014089d0,2*1.d0/
c     data (eijs(8,j),j=1,21)/8*1.d0,1.1d0,1.d0,1.3d0,1.3d0,9*1.d0/
c     data (eijs(9,j),j=1,21)/21*1.d0/
c     data (eijs(10,j),j=1,21)/21*1.d0/
c     data (eijs(11,j),j=1,21)/21*1.d0/
c     data (eijs(12,j),j=1,21)/21*1.d0/
c     data (eijs(13,j),j=1,21)/21*1.d0/
c     data (eijs(14,j),j=1,21)/21*1.d0/
c     data (eijs(15,j),j=1,21)/21*1.d0/
c     data (eijs(16,j),j=1,21)/21*1.d0/
c     data (eijs(17,j),j=1,21)/21*1.d0/
c     data (eijs(18,j),j=1,21)/21*1.d0/
c     data (eijs(19,j),j=1,21)/21*1.d0/
c     data (eijs(20,j),j=1,21)/21*1.d0/
c     data (eijs(21,j),j=1,21)/21*1.d0/
c     data (uija(1,j),j=1,21)/1.d0,0.886106d0,0.963827d0,
c    & 1.d0,0.990877d0,
c    & 1.d0,0.736833d0,1.15639d0,3*1.d0,0.992291d0,1.d0,1.00367d0,
c    & 1.302576d0,1.191904d0,1.205769d0,1.219634d0,1.233498d0,2*1.d0/
c     data (uija(2,j),j=1,21)/2*1.d0,0.835058d0,0.816431d0,0.915502d0,
c    & 1.d0,0.993476d0,0.408838d0,3*1.d0,0.993556d0,9*1.d0/
c     data (uija(3,j),j=1,21)/3*1.d0,0.96987d0,2*1.d0,1.04529d0,1.d0,
c    & 0.9d0,5*1.d0,1.066638d0,1.077634d0,1.088178d0,1.098291d0,
c    & 1.108021d0,2*1.d0/
c     data (uija(4,j),j=1,21)/4*1.d0,1.065173d0,1.d0,0.971926d0,
c    & 1.61666d0,2*1.d0,4*1.25d0,7*1.d0/
c     data (uija(5,j),j=1,21)/21*1.d0/
c     data (uija(6,j),j=1,21)/21*1.d0/
c     data (uija(7,j),j=1,21)/14*1.d0,1.028973d0,1.033754d0,1.038338d0,
c    & 1.042735d0,1.046966d0,2*1.d0/
c     data (uija(8,j),j=1,21)/21*1.d0/
c     data (uija(9,j),j=1,21)/21*1.d0/
c     data (uija(10,j),j=1,21)/21*1.d0/
c     data (uija(11,j),j=1,21)/21*1.d0/
c     data (uija(12,j),j=1,21)/21*1.d0/
c     data (uija(13,j),j=1,21)/21*1.d0/
c     data (uija(14,j),j=1,21)/21*1.d0/
c     data (uija(15,j),j=1,21)/21*1.d0/
c     data (uija(16,j),j=1,21)/21*1.d0/
c     data (uija(17,j),j=1,21)/21*1.d0/
c     data (uija(18,j),j=1,21)/21*1.d0/
c     data (uija(19,j),j=1,21)/21*1.d0/
c     data (uija(20,j),j=1,21)/21*1.d0/
c     data (uija(21,j),j=1,21)/21*1.d0/
c
c     data (kija(1,j),j=1,21)/1.d0,1.00363d0,0.995933d0,1.d0,1.007619d0,
c    & 1.d0,1.00008d0,1.02326d0,3*1.d0,0.997596d0,1.d0,1.002529d0,
c    & 0.982962d0,0.983565d0,0.982707d0,0.981849d0,0.980991d0,2*1.d0/
c     data (kija(2,j),j=1,21)/2*1.d0,0.982361d0,1.00796d0,2*1.d0,
c    & 0.942596d0,1.03227d0,13*1.d0/
c     data (kija(3,j),j=1,21)/3*1.d0,1.00851d0,2*1.d0,1.00779d0,7*1.d0,
c    & 0.910183d0,0.895362d0,0.881152d0,0.86752d0,0.854406d0,2*1.d0/
c     data (kija(4,j),j=1,21)/4*1.d0,0.986893d0,1.d0,0.999969d0,
c    & 1.02034d0,13*1.d0/
c     data (kija(5,j),j=1,21)/21*1.d0/
c     data (kija(6,j),j=1,21)/21*1.d0/
c     data (kija(7,j),j=1,21)/14*1.d0,0.96813d0,0.96287d0,0.957828d0,
c    & 0.952441d0,0.948338d0,2*1.d0/
c     data (kija(8,j),j=1,21)/21*1.d0/
c     data (kija(9,j),j=1,21)/21*1.d0/
c     data (kija(10,j),j=1,21)/21*1.d0/
c     data (kija(11,j),j=1,21)/21*1.d0/
c     data (kija(12,j),j=1,21)/21*1.d0/
c     data (kija(13,j),j=1,21)/21*1.d0/
c     data (kija(14,j),j=1,21)/21*1.d0/
c     data (kija(15,j),j=1,21)/21*1.d0/
c     data (kija(16,j),j=1,21)/21*1.d0/
c     data (kija(17,j),j=1,21)/21*1.d0/
c     data (kija(18,j),j=1,21)/21*1.d0/
c     data (kija(19,j),j=1,21)/21*1.d0/
c     data (kija(20,j),j=1,21)/21*1.d0/
c     data (kija(21,j),j=1,21)/21*1.d0/
c
c
c     data (gijs(1,j),j=1,21)/2*1.d0,.807653d0,4*1.d0,1.95731d0,13*1.d0/
c     data (gijs(2,j),j=1,21)/2*1.d0,0.982746d0,18*1.d0/
c     data (gijs(3,j),j=1,21)/3*1.d0,0.370296d0,1.d0,1.67309d0,15*1.d0/
c     data (gijs(4,j),j=1,21)/21*1.d0/
c     data (gijs(5,j),j=1,21)/21*1.d0/
c     data (gijs(6,j),j=1,21)/21*1.d0/
c     data (gijs(7,j),j=1,21)/21*1.d0/
c     data (gijs(8,j),j=1,21)/21*1.d0/
c     data (gijs(9,j),j=1,21)/21*1.d0/
c     data (gijs(10,j),j=1,21)/21*1.d0/
c     data (gijs(11,j),j=1,21)/21*1.d0/
c     data (gijs(12,j),j=1,21)/21*1.d0/
c     data (gijs(13,j),j=1,21)/21*1.d0/
c     data (gijs(14,j),j=1,21)/21*1.d0/
c     data (gijs(15,j),j=1,21)/21*1.d0/
c     data (gijs(16,j),j=1,21)/21*1.d0/
c     data (gijs(17,j),j=1,21)/21*1.d0/
c     data (gijs(18,j),j=1,21)/21*1.d0/
c     data (gijs(19,j),j=1,21)/21*1.d0/
c     data (gijs(20,j),j=1,21)/21*1.d0/
c     data (gijs(21,j),j=1,21)/21*1.d0/
c
c  Cp0 coefficients given in:
c  McFall, R.L., M.S. Thesis, University of Oklahoma, 1984.
c  Aly, F.A. and Lee, L.L., Fluid Phase Equilib., 6:169, 1981.
c
c     data acp0/-29776.4d0,-3495.34d0,20.7307d0,-37524.4d0,-56072.1d0,
c    & -13773.1d0,-10085.4d0,-5565.6d0,-2753.49d0,-3497.45d0,-72387.d0,
c    & -72674.8d0,-91505.5d0,-83845.2d0,-94982.5d0,-103353.d0,
c    & -109674.d0,-122599.d0,-133564.d0,0d0,0d0/
c     data bcp0/7.95454d0,6.95587d0,6.96237d0,7.98139d0,8.14319d0,
c    & 7.97183d0,7.9468d0,6.66789d0,6.95854d0,6.96302d0,17.8143d0,
c    & 18.6383d0,21.3861d0,22.5012d0,26.6225d0,30.4029d0,34.0847d0,
c    & 38.5014d0,42.7143d0,4.968d0,4.968d0/
c     data ccp0/43.9417d0,0.272892d0,2.68645d0,24.3668d0,37.0629d0,
c    & 6.27078d0,-0.0838d0,2.33458d0,2.02441d0,2.40013d0,58.2062d0,
c    & 57.4178d0,74.341d0,69.5789d0,80.3819d0,90.6941d0,100.253d0,
c    & 111.446d0,122.173d0,0.d0,0.d0/
c     data dcp0/1037.09d0,662.738d0,500.371d0,752.32d0,735.402d0,
c    & 2572.63d0,433.801d0,2584.98d0,1541.22d0,2522.05d0,1787.39d0,
c    & 1792.73d0,1701.58d0,1719.58d0,1718.49d0,1669.32d0,1611.55d0,
c    & 1646.48d0,1654.85d0,100.d0,100.d0/
c     data ecp0/1.56373d0,-0.291318d0,-2.56429d0,3.5399d0,9.38159d0,
c    & 2.0501d0,2.85539d0,0.749019d0,0.096774d0,2.21752d0,40.7621d0,
c    & 38.6599d0,47.0587d0,46.2164d0,55.6598d0,63.2028d0,69.7675d0,
c    & 80.5015d0,90.2255d0,0.d0,0.d0/
c     data fcp0/813.205d0,-680.562d0,-530.443d0,272.846d0,247.19d0,
c    & 1156.72d0,843.792d0,559.656d0,3674.81d0,1154.15d0,808.645d0,
c    & 814.151d0,775.899d0,802.174d0,802.069d0,786.001d0,768.847d0,
c    & 781.588d0,785.564d0,100.d0,100.d0/
c     data gcp0/-24.9027d0,1.7898d0,3.91921d0,8.44724d0,13.4556d0,
c    & 0.d0,6.31595d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,
c    & 0.d0,0.d0,0.d0,0.d0/
c     data hcp0/1019.98d0,1740.06d0,500.198d0,1020.13d0,1454.78d0,
c    & 100.d0,1481.43d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,
c    & 100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0/
c     data icp0/-10.1601d0,0.d0,2.1329d0,-13.2732d0,-11.7342d0,
c    & 0.d0,-2.88457d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,
c    & 0.d0,0.d0,0.d0,0.d0,0.d0/
c     data jcp0/1070.14d0,100.d0,2197.22d0,869.51d0,984.518d0,
c    & 100.d0,1102.23d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,
c    & 100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0/
c     data kcp0/-20.0615d0,4.49823d0,5.81381d0,-22.401d0,-24.0426d0,
c    & -3.24989d0,-0.51551d0,-7.94821d0,6.23387d0,9.19749d0,-44.1341d0,
c    & -46.1938d0,-60.2474d0,-62.2197d0,-77.5366d0,-92.0164d0,
c    & -106.149d0,-122.444d0,-138.006d0,1.8198d0,8.6776d0/
c     end
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file MIX_AGA8.f
c ======================================================================
