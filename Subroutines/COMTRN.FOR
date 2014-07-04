c..begin file comtrn.for
      parameter (mxeta=40)   !max no. coefficients for viscosity
      parameter (mxetac=10)  !max number additional parameters for chung
      parameter (mxtck=40)   !max no. coefficients for t.c. crit
      parameter (mxtcx=40)   !max no. coefficients for thermal cond
      parameter (mxtcxc=10)  !max number additional parameters for chung
      parameter (metar=6)    !max add. residual viscosity params (chung)
      parameter (mtcxr=6)    !max add. residual tc parameters for chung
      parameter (mxtrn=10)   !max no. coefficients for psi, chi function
      parameter (mxomg=15)   !max no. coeffs for collision integral

c..Transport equations-------------------------------------------------
      character*3 hetahc,htcxhc
      character*3 hetacr,htcxcr,htcxcrecs
      character*3 hmdeta,hmdtcx
      character*3 hetamx,heta,htcxmx,htcx

c..pointer to hardcoded models
      common /HCMOD/   hetahc(nrf0:ncmax),htcxhc(nrf0:ncmax)
c..pointer to critical enhancement auxiliary functions
      common /CREMOD/  hetacr(nrf0:ncmax),htcxcr(nrf0:ncmax),
     &                 htcxcrecs(nrf0:nx)

c..Dilute gas
      common /WCFOM1/  comg(nrf0:nx,mxomg,2)
      common /WIFOM1/  ntomg(nrf0:nx),icomg(nrf0:nx,mxomg)
      common /WCFOM2/  comg2(nrf0:nx,mxomg,2)
      common /WIFOM2/  ntomg2(nrf0:nx),icomg2(nrf0:nx,mxomg)
      common /WCEUCK/  cEuck(nrf0:nx,mxtrn,4)
      common /OMGMOD/  hmdeta(nrf0:nx),hmdtcx(nrf0:nx)

c..Thermal conductivity-------------------------------------------------
c..Lennard-Jones parameters
      common /WLJTCX/  sigmat(nrf0:nx),epskt(nrf0:nx)
c..limits and reducing parameters
      common /WLMTCX/  tmtcx(nrf0:nx),txtcx(nrf0:nx),pxtcx(nrf0:nx),
     &                 Dxtcx(nrf0:nx)
      common /WRDTCX/  trddgt(nrf0:nx),tcxdgt(nrf0:nx),
     &                 trdbkt(nrf0:nx),Drdbkt(nrf0:nx),tcxbkt(nrf0:nx),
     &                 trdcrt(nrf0:nx),Drdcrt(nrf0:nx),tcxcrt(nrf0:nx)
c..numbers of terms for the various parts of the model:  numerator
c..and denominator for dilute gas and background parts
      common /WNTTCX/  ndgnum(nrf0:nx),ndgden(nrf0:nx),
     &                 nbknum(nrf0:nx),nbkden(nrf0:nx)
c..commons storing the (real and integer) coefficients to the thermal
c..conductivity model
      common /WCFTCX/  ctcx(nrf0:nx,mxtcx,4)
      common /WIFTCX/  itcx(nrf0:nx,mxtcx)

c..Thermal conductivity critical enhancement----------------------------
c..reducing parameters
      common /WLMTCK/  tmtck(nrf0:nx),txtck(nrf0:nx),pxtck(nrf0:nx),
     &                 Dxtck(nrf0:nx)
      common /WRDTCK/  trtck(nrf0:nx),Drtck(nrf0:nx),prtck(nrf0:nx),
     &                 tcxred(nrf0:nx),tredex(nrf0:nx),Dredex(nrf0:nx)
c..numbers of terms for the various parts of the model:
c..polynomial (numerator & denominator), exponential, spare
c..the "CO2" terms are stored in the "numerator" area
      common /WNTTCK/  nnumtck(nrf0:nx),ndentck(nrf0:nx),
     &                 nexptck(nrf0:nx),nsparek(nrf0:nx)
c..commons storing the (real and integer) coefficients to the model
      common /WCFTCKe/ ctcke(nrf0:nx,mxtck,5)
      common /WCFTCK/  ctck(nrf0:nx,mxtck,5)
      common /WIFTCK/  itck(nrf0:nx,mxtck,0:5)

c..Viscosity------------------------------------------------------------
      common /CRTENH/  tcmxec,pcmxec,dcmxec,etacal
      common /WLMETA/  tmeta(nrf0:nx),txeta(nrf0:nx),pxeta(nrf0:nx),
     &                 Dxeta(nrf0:nx)
c..commons storing the real coefficients to the viscosity model
      common /WCFETA/  ceta(nrf0:nx,mxeta,4)
      common /WIFETA/  ieta(nrf0:nx,mxeta)
      common /WLJETA/  sigmav(nrf0:nx),epskv(nrf0:nx)
c..limits and reducing parameters
      common /WRDETA/  trddge(nrf0:nx),etadge(nrf0:nx),
     &                 tredB2(nrf0:nx),etarB2(nrf0:nx),
     &                 tredeta(nrf0:nx),Dredeta(nrf0:nx),etared(nrf0:nx)
      common /WR3ETA/  trddg3(nrf0:nx),etadg3(nrf0:nx),
     &                 trdbk3(nrf0:nx),Drdbk3(nrf0:nx),etabk3(nrf0:nx),
     &                 trdcr3(nrf0:nx),Drdcr3(nrf0:nx),etacr3(nrf0:nx)
c..numbers of terms for the various parts of the model:  dilute gas,
c..second viscosity virial (initial density dependence), residual part
      common /WNTETA/  ndgeta(nrf0:nx),nB2eta(nrf0:nx),ndel0(nrf0:nx),
     &                 npoly(nrf0:nx),nnumeta(nrf0:nx),ndeneta(nrf0:nx),
     &                 nexpn(nrf0:nx),nexpd(nrf0:nx),
     &                 ndg2(nrf0:nx),ndg3(nrf0:nx),ndg4(nrf0:nx),
     &                 ndg5(nrf0:nx),ndg6(nrf0:nx)
      common /WN3ETA/  ndgnm3(nrf0:nx),ndgdn3(nrf0:nx),
     &                 nbknm3(nrf0:nx),nbkdn3(nrf0:nx)
c..common storing residual coefficients to the FT visc model
      common /TRNFT/   ASftm(nrf0:nx,0:4),BSftm(nrf0:nx,0:4),
     &                 CSftm(nrf0:nx,0:4),ABftm(nrf0:nx,0:4),
     &                 BBftm(nrf0:nx,0:4),CBftm(nrf0:nx,0:4),
     &                 DBftm(nrf0:nx,0:4),EBftm(nrf0:nx,0:4)

c..ECS transport--------------------------------------------------------
      common /TRNMOD/  hetamx,heta(nrf0:ncmax),htcxmx,htcx(nrf0:ncmax)
      common /TRNBIN/  xljs(nx,nx),xlje(nx,nx),xkij(nx,nx),xlij(nx,nx),
     &                 xaji(nx,nx),xkijk(nx,nx),xlijk(nx,nx),
     &                 xdij(nx,nx),xdij2(nx,nx)
      common /WLMTRN/  tmecst(nrf0:nx),txecst(nrf0:nx),pxecst(nrf0:nx),
     &                 Dxecst(nrf0:nx)
      common /WLJTRN/  sigtrn(nrf0:nx),epsktr(nrf0:nx)
      common /WCFTRN/  cpsi(nrf0:nx,mxtrn,4),cchi(nrf0:nx,mxtrn,4)
c     common /WIFTRN/  ipsi(nrf0:nx,0:mxtrn),ichi(nrf0:nx,0:mxtrn)
c..iLJflag:  flag for L-J parameters (if 0, estimate)
c..nEuck:  factor f_int in Eucken correlation
c..npsi (viscosity shape factor):  polynomial term, 2nd poly, spare
c..nchi (conductivity shape factor):  polynomial term, 2nd poly, spare
      common /WNTTRN/  iLJflag(nrf0:nx),nEuck(nrf0:nx),
     &                 npsi1(nrf0:nx),npsi2(nrf0:nx),npsi3(nrf0:nx),
     &                 nchi1(nrf0:nx),nchi2(nrf0:nx),nchi3(nrf0:nx)

c..Parameters for Chung method------------------------------------------
      common /CHUNGPk/ acchk(nrf0:nx),ddipk(nrf0:nx),sigchk(nrf0:nx),
     &                 epschk(nrf0:nx),cctcx(nrf0:nx,mxtcxc,mtcxr),
     &                 tcx0ch,hbkk(nrf0:nx),naddk(nrf0:nx)
      common /CHUNGPv/ acchv(nrf0:nx),ddipv(nrf0:nx),sigchv(nrf0:nx),
     &                 epschv(nrf0:nx),cceta(nrf0:nx,mxetac,metar),
     &                 eta0ch,hbvk(nrf0:nx),naddv(nrf0:nx)


!$omp threadprivate(/HCMOD/)
!$omp threadprivate(/CREMOD/)
!$omp threadprivate(/WCFOM1/)
!$omp threadprivate(/WIFOM1/)
!$omp threadprivate(/WCFOM2/)
!$omp threadprivate(/WIFOM2/)
!$omp threadprivate(/WCEUCK/)
!$omp threadprivate(/OMGMOD/)
!$omp threadprivate(/WLJTCX/)
!$omp threadprivate(/WLMTCX/)
!$omp threadprivate(/WRDTCX/)
!$omp threadprivate(/WNTTCX/)
!$omp threadprivate(/WCFTCX/)
!$omp threadprivate(/WIFTCX/)
!$omp threadprivate(/WLMTCK/)
!$omp threadprivate(/WRDTCK/)
!$omp threadprivate(/WNTTCK/)
!$omp threadprivate(/WCFTCKe/)
!$omp threadprivate(/WCFTCK/)
!$omp threadprivate(/WIFTCK/)
!$omp threadprivate(/CRTENH/)
!$omp threadprivate(/WLMETA/)
!$omp threadprivate(/WCFETA/)
!$omp threadprivate(/WIFETA/)
!$omp threadprivate(/WLJETA/)
!$omp threadprivate(/WRDETA/)
!$omp threadprivate(/WR3ETA/)
!$omp threadprivate(/WNTETA/)
!$omp threadprivate(/WN3ETA/)
!$omp threadprivate(/TRNFT/)
!$omp threadprivate(/TRNMOD/)
!$omp threadprivate(/TRNBIN/)
!$omp threadprivate(/WLMTRN/)
!$omp threadprivate(/WLJTRN/)
!$omp threadprivate(/WCFTRN/)
!$omp threadprivate(/WNTTRN/)
!$omp threadprivate(/CHUNGPk/)
!$omp threadprivate(/CHUNGPv/)
c!$omp threadprivate(/WIFTRN/)

c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file comtrn.for
c ======================================================================
