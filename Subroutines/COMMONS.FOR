c  begin file commons.for
      implicit double precision (a-h,o-z)
      implicit integer (i-k,m,n)
      implicit logical (l)

      parameter (ncmax=20)     !max number of components in mixture
      parameter (nrefmx=10)    !max number of fluids for transport ECS
      parameter (n0=-ncmax-nrefmx)
      parameter (mxtrm=56)     !max number of Helmholtz terms
      parameter (mxcrt=20)     !max number of critical terms in FEQ

c..Use for fitting pure fluids:
c     parameter (ncmax=1)
c     parameter (n0=1)
c     parameter (mxtrm=30)
c     parameter (mxcrt=10)


c..All variables in common blocks use 4 letters or more except nc, R, and tz

c..Blocks common to all routines
      parameter (nx=ncmax)
      parameter (thrd=1.0d0/3.0d0)
      character*1      htab,hnull
      common /NCOMP/   nc,icnc
      common /Gcnst/   R
      common /ZRED/    tz(n0:nx),rhoz(n0:nx)
      common /HCHAR/   htab,hnull
      common /PRNTERR/ iprnterr

c..Setup blocks
      parameter (nrefluids=4)  !number of ref fluids available for trans
      parameter (nrf0=n0)      !lower limit for transport ref fluid arrays
      character*3   hsvph,hsveqn,hsveqk,hsvvis,hsvvik,hsvcnd,hsvcnk
      character*3   hsvdil,hsvdik,hsvsrf,hsvsrk
      character*255 hsvfld,hsvmix,hpath
      character*255 hieos,hicp0,hieta,hietac,hitcx,hitcxc,histn,
     &              hidiel,himelt,hisubl,hips,hipl,hidl,hidv
      common /FLDPTH/  hpath
      common /VERS/    verfl(n0:nx),vermx    !fluid & mix file version
      common /RESETM/  lreset,lrst,linit,ncset !flag for reseting models
      common /ZEROST/  izero0,ione1,itwo2
      common /CITE/    hieos(n0:nx),hicp0(n0:nx),
     &                 hieta(nrf0:nx),hietac(nrf0:nx),
     &                 hitcx(nrf0:nx),hitcxc(nrf0:nx),histn(n0:nx),
     &                 hidiel(n0:nx),himelt(n0:nx),hisubl(n0:nx),
     &                 hips(n0:nx),hipl(n0:nx),hidl(n0:nx),hidv(n0:nx)
      common /IRFSAV/  ixfsav,ksetrf
      common /SETSAV/  hsvfld(n0:nx),hsvmix
      common /MODSAV/  hsvph,hsveqn,hsveqk(nx),hsvvis,hsvvik(nx),hsvcnd,
     &                 hsvcnk(nx),hsvsrf,hsvsrk(nx),hsvdil,hsvdik(nx)
      common /REFSAV/  x0sav(nx),h0sav,s0sav,t0sav,p0sav
      common /CREF/    tref(n0:nx),rhoref(n0:nx),href(n0:nx),sref(n0:nx)
      common /REFST/   hsvrfs,hrefdf(n0:nx)

c..CAS number, name, fluid constants
      character*3   hsvrfs,hrefdf
      character*12  hcas,hname
      character*60  hnam60
      character*255 hnam80,hsyn1,hsyn2
      character*255 family,UNNumb
      common /CCON/ tcrit(n0:nx),pcrit(n0:nx),Dcrit(n0:nx),Zcrit(n0:nx),
     &              ttpn(n0:nx),ptpn(n0:nx),dtpn(n0:nx),dtpv(n0:nx),
     &              tnbp(n0:nx),dnbpl(n0:nx),dnbpv(n0:nx),
     &              wmas(n0:nx),accen(n0:nx),dipole(n0:nx),Reos(n0:nx)
      common /CCAS/    hcas(n0:nx)
      common /HTCM/    hcmbst(n0:nx)
      common /CNAM/    hname(n0:nx)
      common /CNAM60/  hnam60(n0:nx)
      common /CNAM80/  hnam80(n0:nx),hsyn1(n0:nx),hsyn2(n0:nx)
      common /FAML/    family(n0:nx),UNNumb(n0:nx)  !fluid family type

c..Critical point, maxcondentherm, maxcondenbar
      common /MAXTP/ tmxt,tmxp,tmxD,pmxt,pmxp,pmxD,crtt,crtp,crtd,
     &               ierrcrt,ierrtmx,ierrpmx

c..VLE routines
      common /FXPNT/   d72l,d72v,
     &                 thmax,hmax,htpl,htpv,
     &                 temax,emax,etpl,etpv,
     &                 tsmax,smax,stpl,stpv,smin,sminm,tsmin,tsminm
      common /TSTSAV/  tsavt,psavt,dlsavt,dvsavt,
     &                 xsavt(ncmax),xlsavt(ncmax),xvsavt(ncmax),
     &                 kphsvt,icsavt
      common /PSTSAV/  tsavp,psavp,dlsavp,dvsavp,
     &                 xsavp(ncmax),xlsavp(ncmax),xvsavp(ncmax),
     &                 kphsvp,icsavp
      common /FSHSAV/  lsatt,lsatp

c..common block containing flags to GUI (initialized in BDSET in setup.f)
c..flags indicating 'not applicable', '2-phase', etc.
      common /FLAGS/  xnota,x2ph,xsubc,xsuph,xsupc,xinf,xerr,xnotd,xnotc
      common /FLAGS2/ iamwat,iwat,ianc(0:ncmax),igenfl(n0:nx),
     &                ieflg,iGERG04,iSatFlg,iSatFlg2,ialc

c..EOS blocks
      character*3 hpheq,heos,hmxeos,hmodcp
      common /EOSMOD/  hpheq,heos,hmxeos(n0:nx),hmodcp(n0:nx)
      common /EOSLIM/  tmeos(n0:nx),txeos(n0:nx),peos(n0:nx),Deos(n0:nx)

c..FEQ blocks
c..numbers of terms associated with the "normal" Helmholtz function
c..(plus numbers of unique powers of temperature, density, etc.),
c..the critical-region terms of Wagner, plus spare for future use
c..parameters associated with the critical-region terms of Wagner
      parameter (mxfeq=2)    !max number of FEQ EOS in block data
      logical lcrtflg(n0:nx)
      character*16 hdrvflg(n0:nx)
      common /WNTFEQ/  ntermf(n0:nx),ncoeff(n0:nx),
     &                 ntpf(n0:nx),ndpf(n0:nx),nlpf(n0:nx),
     &                 ncrt(n0:nx),ncfcrt(n0:nx),
     &                 nsparf(n0:nx),ncfsp(n0:nx)
      common /WCFFEQ/  afeq(n0:nx,mxtrm),
     &                 tifeq(n0:nx,mxtrm),difeq(n0:nx,mxtrm),
     &                 gif1(n0:nx,mxtrm),gif2(n0:nx,mxtrm),
     &                 dlif(n0:nx,mxtrm),tlif(n0:nx,mxtrm),
     &                 tpower(n0:nx,mxtrm),dpower(n0:nx,mxtrm),
     &                 dlpowr(n0:nx,mxtrm),
     &                 rho0feq(n0:nx),t0feq(n0:nx),
     &                 pcfeq(n0:nx),rhocfeq(n0:nx),tcfeq(n0:nx),
     &                 wmfeq(n0:nx),Rfeq(n0:nx),
     &                 pminfeq(n0:nx),rhotpfeq(n0:nx),tminfeq(n0:nx),
     &                 tmaxfeq(n0:nx),pmaxfeq(n0:nx)
      common /WCFFQ2/  alphaf(n0:nx,mxcrt),betaf(n0:nx,mxcrt),
     &                 gammaf(n0:nx,mxcrt),deltaf(n0:nx,mxcrt),
     &                 etaf(n0:nx,mxcrt),eidf(n0:nx,mxcrt),
     &                 eitf(n0:nx,mxcrt)
      common /WLFFEQ/  itpf(n0:nx,mxtrm),
     &                 idpf(n0:nx,mxtrm),ilpf(n0:nx,mxtrm)
      common /FEQSAV/  phisvf(n0:nx,mxtrm),delsvf(n0:nx),tausvf(n0:nx),
     &                 taup(n0:nx,mxtrm),delp(n0:nx,mxtrm),
     &                 delli(n0:nx,mxtrm),drvsav(n0:nx,16)
      common /TERMS/   phir01(mxtrm),phir10(mxtrm),phir02(mxtrm),
     &                 phir20(mxtrm),phir03(mxtrm),phir30(mxtrm)
      common /FEQSV2/  hdrvflg
      common /FEQSV3/  lcrtflg
      common /CRTSAV/  delb(n0:nx,mxcrt),taua(n0:nx,mxcrt),
     &                 txpf(n0:nx,mxcrt),hxpf(n0:nx,mxcrt),
     &                 axpf(n0:nx,mxcrt),dxpf(n0:nx,mxcrt),
     &                 extf(n0:nx,mxcrt),extd(n0:nx,mxcrt),
     &                 extt(n0:nx,mxcrt),extdt(n0:nx,mxcrt),
     &                 extt2(n0:nx,mxcrt),extd2(n0:nx,mxcrt)

c..BWR block
      parameter (mxbwr=2)    !max number of MBWR EOS in block data
      common /WCFBWR/  bbwr(n0:nx,32),
     &              pcbwr(n0:nx),rhocb(n0:nx),tcbwr(n0:nx),Rbwr(n0:nx),
     &              ptrb(n0:nx),rhotrb(n0:nx),ttrb(n0:nx),gammab(n0:nx),
     &              tmaxb(n0:nx),pmaxb(n0:nx)

c..ECS block for thermo
      parameter (nfhe=4)
      common /CFECS/   fecs(ncmax,nfhe,2),hecs(ncmax,nfhe,2),
     &                 fdecs(ncmax,nfhe,2),hdecs(ncmax,nfhe,2),
     &                 acfecs(ncmax),acfref(ncmax),Zcref(ncmax),
     &                 tminecs(ncmax),tmaxecs(ncmax),
     &                 pmaxecs(ncmax),dmaxecs(ncmax)
      common /ICFECS/  nfecs(ncmax),nhecs(ncmax),
     &                 nfdecs(ncmax),nhdecs(ncmax)

c..Quintic equation
      character*16 drvflq(n0:nx)
      common /WNTQUI/  netaq(n0:nx),nepsq(n0:nx),nbbq(n0:nx),
     &                 ngamq(n0:nx),nbetq(n0:nx)
      common /WCFQUI/  aqui(n0:nx,mxtrm),tiqui(n0:nx,mxtrm),
     &                 diqui(n0:nx,mxtrm),rho0q(n0:nx),t0qui(n0:nx),
     &                 pcqui(n0:nx),rhocq(n0:nx),tcqui(n0:nx),
     &                 wmfq(n0:nx),Rqui(n0:nx),
     &                 pminq(n0:nx),rhotpq(n0:nx),tminq(n0:nx),
     &                 tmaxq(n0:nx),pmaxq(n0:nx)
      common /QUISV2/  drvflq
      common /QUISAV/  phisvq(n0:nx,mxtrm),delsvq(n0:nx),tausvq(n0:nx),
     &                 taupq(n0:nx,mxtrm),delpq(n0:nx,mxtrm),
     &                 drvsvq(n0:nx,16)
      common /QUITRM/  etaq,etaq10,etaq20,epsq,epsq10,epsq20,
     &                 bbq00,bbq10,bbq20,gamq,gamq10,gamq20

c..CP0 blocks
      parameter (mxcpp=2)    !max number of fluids in block data for cp0
      parameter (mxph0=2)    !max number of fluids in block data for ph0
      parameter (ncppmx=20)  !max number of Cp0 terms
      parameter (nph0mx=10)  !max number of terms in phi0 function
      common /CPPSAV/  cp0sav(n0:nx),cpisav(n0:nx),cptsav(n0:nx),
     &                 tsvcp0(n0:nx)
      common /WNTCPP/  ntermc(n0:nx),nterme(n0:nx),
     &                 ncoshc(n0:nx),nsinhc(n0:nx),
     &                 nsp1c(n0:nx),nsp2c(n0:nx),nsp3c(n0:nx)
      common /WCPCPP/  cpc0(n0:nx,ncppmx),xkc0(n0:nx,ncppmx),
     &                 cp0h(n0:nx,ncppmx),x0th(n0:nx,ncppmx),
     &                                  xh00(n0:nx,ncppmx)
      common /WRDCPP/  tredcp(n0:nx),Credcp(n0:nx)
      common /WLMCPP/  tmincp(n0:nx),tmaxcp(n0:nx),
     &                 pmaxcp(n0:nx),dmaxcp(n0:nx)
      common /WNTPH0/  nlogp(n0:nx),ntaup(n0:nx),nexpp(n0:nx),
     &                 ncoshp(n0:nx),nsinhp(n0:nx),
     &                 nsp1p(n0:nx),nsp2p(n0:nx),nsp3p(n0:nx)
      common /PH0SAV/  ph0sav(n0:nx),ph1sav(n0:nx),ph2sav(n0:nx),
     &                 tsvph0(0:2,n0:nx),rhosvp(0:2,n0:nx)
      common /WLMPH0/  tminph(n0:nx),tmaxph(n0:nx),
     &                 pmaxph(n0:nx),dmaxph(n0:nx)
      common /WCFPH0/  aiph(n0:nx,nph0mx),tiph(n0:nx,nph0mx)

c..Mixtures
c..commons associated with the binary mixing rule(s)
c..the amix(i,j,k,1..3) are the coefs for the (general) mixing term
c..the fmix(i,j,1..nmxpar) are the parameters for the i-j binary
c..the namix(i,j) are the number of afeq(i,j,k,1..3) terms
      parameter (nbin=ncmax*(ncmax-1)/2)   !# possible binary pairs
      parameter (nmxpar=6)   !number of binary mixture parameters
      parameter (nmxcof=7)   !number of coeffs in binary mixing rule
      parameter (nmsav=55)   !number of mixture models
      parameter (nmxtrm=15)  !number of terms in binary mixing func
      parameter (nbrule=40)  !number of binary mixing rules
      parameter (nmxtc=11,nmxvc=11) !number of coeff for critical lines
      character*3   hmodmx,hmodtc,hmodvc,hprkij
      character*8   hbpar
      character*16  drvflh(nmsav)
      character*255 hmfile,hrule,hbin
      common /MIXMOD/  hmodmx(nx,nx)
      common /CFXHMX/  amix(nx,nx,nmxtrm,nmxcof),fmix(nx,nx,nmxpar)
      common /CFIHMX/  namix(nx,nx),namix2(nx,nx)
      common /HMXSV2/  drvflh
      common /HMXSAV/  phisvh(nmsav,0:nmxtrm),
     &                 tausvh(nmsav),delsvh(nmsav),
     &                 drvsvh(nmsav,16),dellih(nmsav,nmxtrm)
      common /MXTRMS/  phix01(nmxtrm),phix10(nmxtrm),phix02(nmxtrm),
     &                 phix20(nmxtrm),phix03(nmxtrm)
      common /CRTMOD/  hmodtc(nx,nx),hmodvc(nx,nx)
      common /CFXCRT/  ftcij(nx,nx,nmxtc),fvcij(nx,nx,nmxvc)
      common /CF2HMX/  fmix2(nx,nx,nmxpar)
      common /MXINFO/  hmfile(0:nbin),hrule(nbrule),hbpar(nbrule,nmxpar)
      common /HMXFLG/  ivrsnm
      common /PRMOD/   hprkij(nx,nx)
      common /MXINF1/  hbin(nbin)
      common /MXINF2/  bideal(nbrule,nmxpar)
      common /MXRULE/  nrule
      common /CREFDF/  tdef(n0:nx),pdef(n0:nx),hdef(n0:nx),sdef(n0:nx)


c..Ancillary equations
      parameter (ndlk=15)    !max number of terms in liquid density
      parameter (ndvk=15)    !max number of terms in vapor density
      parameter (npsk=15)    !max number of terms in vapor pressure
      parameter (nplk=15)    !max number of terms in liquid pressure
      parameter (nmltk=8)    !max number of terms in melting line
      parameter (nsblk=8)    !max number of terms in sublimation
      parameter (ndecf=15)   !max number of terms in dielectric constant
      parameter (nsigk=3)    !max number of terms in surface tension
      character*3 hpsa,hpsk,hpla,hplk,hdla,hdlk,hdva,hdvk
      character*3 hmelt,hmeltk,hsubl,hsublk
      character*3 hdiel,hdielk,hsten,hstenk

c     Vapor pressure
      common /PSMOD/   hpsa,hpsk(n0:nx)
      common /WLMPS/   pstmin(n0:nx),pstmax(n0:nx)
      common /WCFPS/   pska(n0:nx,npsk),psexp(n0:nx,npsk)
      common /WRDPS/   pstrd(n0:nx),psprd(n0:nx)
      common /WNTPS/   nps1(n0:nx),nps2(n0:nx),nps3(n0:nx),
     &                 nps4(n0:nx),nps5(n0:nx),nps6(n0:nx)

c     Liquid pressure (for PPF files)
      common /PLMOD/   hpla,hplk(n0:nx)
      common /WLMPL/   pltmin(n0:nx),pltmax(n0:nx)
      common /WCFPL/   plka(n0:nx,nplk),plexp(n0:nx,nplk)
      common /WRDPL/   pltrd(n0:nx),plprd(n0:nx)
      common /WNTPL/   npl1(n0:nx),npl2(n0:nx),npl3(n0:nx),
     &                 npl4(n0:nx),npl5(n0:nx),npl6(n0:nx)

c     Saturated liquid density
      common /DLMOD/   hdla,hdlk(n0:nx)
      common /WLMDL/   dltmin(n0:nx),dltmax(n0:nx)
      common /WCFDL/   dlka(n0:nx,ndlk),dlexp(n0:nx,ndlk)
      common /WRDDL/   dltrd(n0:nx),dldrd(n0:nx)
      common /WNTDL/   ndl1(n0:nx),ndl2(n0:nx),ndl3(n0:nx),
     &                 ndl4(n0:nx),ndl5(n0:nx),ndl6(n0:nx)

c     Saturated vapor density
      common /DVMOD/   hdva,hdvk(n0:nx)
      common /WLMDV/   dvtmin(n0:nx),dvtmax(n0:nx)
      common /WNTDV/   ndv1(n0:nx),ndv2(n0:nx),ndv3(n0:nx),
     &                 ndv4(n0:nx),ndv5(n0:nx),ndv6(n0:nx)
      common /WCFDV/   dvka(n0:nx,ndvk),dvexp(n0:nx,ndvk)
      common /WRDDV/   dvtrd(n0:nx),dvdrd(n0:nx)

c     Melting line
      common /MELTMOD/ hmelt,hmeltk(n0:nx)
      common /WLMMELT/ tminmt(n0:nx),tmaxmt(n0:nx)
      common /WCFMELT/ pmeltk(n0:nx,nmltk),pmltex(n0:nx,nmltk)
      common /WRDMELT/ trdmlt(n0:nx),prdmlt(n0:nx)
      common /WNTMELT/ ntrmm1(n0:nx),ntrmm2(n0:nx),ntrmm3(n0:nx),
     &                 ntrmm4(n0:nx),ntrmm5(n0:nx),ntrmm6(n0:nx)

c     Sublimation line
      common /SUBLMOD/ hsubl,hsublk(n0:nx)
      common /WLMSUBL/ tminsb(n0:nx),tmaxsb(n0:nx)
      common /WCFSUBL/ psublk(n0:nx,nsblk),psblex(n0:nx,nsblk)
      common /WRDSUBL/ trdsbl(n0:nx),prdsbl(n0:nx)
      common /WNTSUBL/ ntrmb1(n0:nx),ntrmb2(n0:nx),ntrmb3(n0:nx),
     &                 ntrmb4(n0:nx),ntrmb5(n0:nx),ntrmb6(n0:nx)

c     Dielectric constant
      common /DEMOD/   hdiel,hdielk(n0:nx)
      common /WLMDE/   tminde(n0:nx),tmaxde(n0:nx)
      common /WCFDE/   decf(n0:nx,ndecf),deexpt(n0:nx,ndecf),
     &                 deexpd(n0:nx,ndecf),deexpp(n0:nx,ndecf)
      common /WRDDE/   tredde(n0:nx),dredde(n0:nx),predde(n0:nx)
      common /WNTDE/   ntrmd1(n0:nx),ntrmd2(n0:nx),ntrmd3(n0:nx),
     &                 ntrmd4(n0:nx),ntrmd5(n0:nx),ntrmd6(n0:nx)

c     Surface tension
      common /STNMOD/  hsten,hstenk(n0:nx)
      common /WLMSTN/  tminst(n0:nx),tmaxst(n0:nx)
      common /WCFST1/  sigmak(n0:nx,nsigk),sigexp(n0:nx,nsigk)
      common /WNTST1/  ntermu(n0:nx)

c..Peng-Robinson equations
      parameter (nmxprb=6)   !number of coeff for PR eos
      character*3 hprsav,hmxsv
      character*16 drvflp(n0:nx)
      common /PRSV2/   drvflp
c     common /PRSAV/   delsvp(n0:nx),tausvp(n0:nx),drvsvp(n0:nx,16)
      common /WCFPR/   pcpr(n0:nx),rhocpr(n0:nx),tcpr(n0:nx),
     &                 wmfpr(n0:nx),Rprq(n0:nx),acnpr(n0:nx)
      common /CONPR/   prcoef(0:nx,20),iprflag(0:nx)
      common /PRHSAV/  hprsav,hmxsv(n0:nx)
      common /CFXPR/   fprkij(nx,nx,nmxprb)

c..Derivatives of the Helmholtz energy with respect to x and n.
      common /PHIDR/   daddn(ncmax),d2adnn(ncmax,ncmax),d2addn(ncmax),
     &                 d2adtn(ncmax),d2adbn(ncmax),
     &                 dadxiTV(ncmax),daddxiTV(ncmax),
     &                 dtdn(ncmax),dvdn(ncmax),dadxi(ncmax),
     &                 dpdn(ncmax),daddx(ncmax),dadtx(ncmax),
     &                 d2adxn(ncmax,ncmax),d2adxnTV(ncmax,ncmax),
     &                 dadxij(ncmax,ncmax),dlnphiidxj(ncmax,ncmax),
     &                 ddrdxn(ncmax,ncmax),dtrdxn(ncmax,ncmax),
     &                 dpdxi(ncmax),xlnfi(ncmax),
     &                 dphidp(ncmax),dphidt(ncmax),dphidxj(ncmax,ncmax),
     &                 dlnfinidT(ncmax),dlnfinidV(ncmax),
     &                 dphidnj(ncmax,ncmax),dmurdt(ncmax)

c..Variables for auxiliary spinodal-splines
      parameter (iptmax=1000)  !max number of nodes for spinodal calc
      parameter (narm=1000)
      dimension dspndl_l(iptmax),tspndl_l(iptmax),
     &          cspndl_l(iptmax),dspndl_v(iptmax),
     &          tspndl_v(iptmax),cspndl_v(iptmax)
      common /AUXFLGS/ ispndl_l,ispndl_v
      common /CSPNDL/  dspndl_l,tspndl_l,cspndl_l,
     &                 dspndl_v,tspndl_v,cspndl_v
      common /SATSPL/ xarr(0:ncmax+5,0:narm),carr(ncmax+5,narm),
     &                xspln(ncmax),narr,ispline


c..Variables for UN calculations
      common /CONUN/   tminUN(n0:nx),tmaxUN(n0:nx),pmaxUN(n0:nx),
     &                 rhomUN(n0:nx),prmUN(n0:nx,10),ntrmUN(n0:nx),
     &                 iunflg(n0:nx)

c..AGA8 equation of state constants and variables
      character*3 hagasv
      dimension qbaga(21),fbaga(21),sbaga(21),wbaga(21)
      dimension ifpa(ncmax)
      double precision Kaga
      double precision mrbaga(21),ebaga(21),kbaga(21),gbaga(21)
      double precision eijs(21,21),uija(21,21),kija(21,21),gijs(21,21)
      double precision kb2a(21),eb2a(21),kij2(21,21),uij2(21,21),
     &       gij2(21,21),bs2a(18,21,21)
      common /VAR/     Kaga,bsaga(18),cnsaga(58)
      common /INTMCF/  kb2a,eb2a,kij2,uij2,gij2,bs2a,ifpa
      common /AGASV1/  hagasv
      common /AGASV2/  nagacsv(ncmax),nagaesv(ncmax),ncoshsv(ncmax),
     &                 nsinhsv(ncmax)
      common /AGASV3/  Ragasv(ncmax),wmagasv(ncmax),tmagasv(ncmax),
     &                 txagasv(ncmax),tragasv(ncmax),Cragasv(ncmax),
     &                 Tfagasv(ncmax),Dfagasv(ncmax),hfagasv(ncmax),
     &                 sfagasv(ncmax),xksv(ncmax,5),xthsv(ncmax,5),
     &                 xhsv(ncmax,5),cpcsv(ncmax,5),cphsv(ncmax,5),Rasv

c     common /EOSCOEF/ anaga(58),unaga(58)
c     common /FLDCOEF/ mrbaga,ebaga,kbaga,gbaga,qbaga,fbaga,sbaga,wbaga,
c    &                 eijs,uija,kija,gijs
c     common /CP0COEF/ acp0(21),bcp0(21),ccp0(21),dcp0(21),ecp0(21),
c    &        fcp0(21),gcp0(21),hcp0(21),icp0(21),jcp0(21),kcp0(21)

      dimension anaga(58),unaga(58)
      parameter (anaga =
     & (/0.1538326d0,1.341953d0,-2.998583d0,-0.04831228d0,
     & 0.3757965d0,-1.589575d0,-0.05358847d0,0.88659463d0,-0.71023704d0,
     & -1.471722d0,1.32185035d0,-0.78665925d0,2.29129D-9,0.1576724d0,
     & -0.4363864d0,-0.04408159d0,-0.003433888d0,0.03205905d0,
     & 0.02487355d0,0.07332279d0,-0.001600573d0,0.6424706d0,
     & -0.4162601d0,-0.06689957d0,0.2791795d0,-0.6966051d0,
     & -0.002860589d0,-0.008098836d0,3.150547d0,0.007224479d0,
     & -0.7057529d0,0.5349792d0,-0.07931491d0,-1.418465d0,
     & -5.99905D-17,0.1058402d0,0.03431729d0,-0.007022847d0,
     & 0.02495587d0,0.04296818d0,0.7465453d0,-0.2919613d0,7.294616d0,
     & -9.936757d0,-0.005399808d0,-0.2432567d0,0.04987016d0,
     & 0.003733797d0,1.874951d0,0.002168144d0,-0.6587164d0,
     & 0.000205518d0,0.009776195d0,-0.02048708d0,0.01557322d0,
     & 0.006862415d0,-0.001226752d0,0.002850908d0/))
c     data bnaga/18*1,9*2,10*3,7*4,5*5,2*6,2*7,3*8,2*9/
c     data cnaga/12*0,6*1,2*0,7*1,0,9*1,2*0,5*1,0,4*1,0,1,0,6*1/
c     data knaga/12*0,3,3*2,2*4,2*0,3*2,4*4,0,2*1,2*2,2*3,3*4,2*0,3*2,2*4,
c    & 0,2*2,2*4,0,2,0,2,1,4*2/
      parameter (unaga =
     & (/0d0,0.5d0,1d0,3.5d0,-0.5d0,4.5d0,0.5d0,7.5d0,9.5d0,6d0,
     & 12d0,12.5d0,-6d0,2d0,3d0,2d0,2d0,11d0,-0.5d0,0.5d0,0d0,4d0,6d0,
     & 21d0,23d0,22d0,-1d0,-0.5d0,7d0,-1d0,6d0,4d0,1d0,9d0,-13d0,21d0,
     & 8d0,-0.5d0,0d0,2d0,7d0,9d0,22d0,23d0,1d0,9d0,3d0,8d0,23d0,1.5d0,
     & 5d0,-0.5d0,4d0,7d0,3d0,0d0,1d0,0d0/))
c     data gnaga/4*0,2*1,18*0,1,3*0,1,2*0,3*1,16*0,1,2*0,1,0,1,2*0/
c     data qnaga/6*0,1,8*0,1,9*0,1,0,1,8*0,1,4*0,1,4*0,1,0,1,2*0,1,5*0,1/
c     data fnaga/12*0,1,13*0,1,2*0,1,4*0,1,23*0/
c     data snaga/7*0,2*1,49*0/
c     data wnaga/9*0,3*1,46*0/
c
      parameter (mrbaga =
     & (/16.043d0,28.0135d0,44.01d0,30.07d0,44.097d0,18.0153d0,
     & 34.082d0,2.0159d0,28.01d0,31.9988d0,58.123d0,58.123d0,72.15d0,
     & 72.15d0,86.177d0,100.204d0,114.231d0,128.258d0,142.285d0,
     & 4.0026d0,39.948d0/))
      parameter (ebaga =
     & (/151.3183d0,99.73778d0,241.9606d0,244.1667d0,298.1183d0,
     & 514.0156d0,296.355d0,26.95794d0,105.5348d0,122.7667d0,324.0689d0,
     & 337.6389d0,365.5999d0,370.6823d0,402.636293d0,427.72263d0,
     & 450.325022d0,470.840891d0,489.558373d0,2.610111d0,119.6299d0/))
      parameter (kbaga =
     & (/0.4619255d0,0.4479153d0,0.4557489d0,0.5279209d0,
     & 0.583749d0,0.3825868d0,0.4618263d0,0.3514916d0,0.4533894d0,
     & 0.4186954d0,0.6406937d0,0.6341423d0,0.6738577d0,0.6798307d0,
     & 0.7175118d0,0.7525189d0,0.784955d0,0.8152731d0,0.8437826d0,
     & 0.3589888d0,0.4216551d0/))
      parameter (gbaga = (/0.d0,0.027815d0,0.189065d0,0.0793d0,
     & 0.141239d0,0.3325d0,
     & 0.0885d0,0.034369d0,0.038953d0,0.021d0,0.256692d0,0.281835d0,
     & 0.332267d0,0.366911d0,0.289731d0,0.337542d0,0.383381d0,
     & 0.427354d0,0.469659d0,0.d0,0.d0/))

      parameter (qbaga =
     & (/(0.d0,j=1,2),0.69d0,(0.d0,j=1,2),1.06775d0,0.633276d0,
     & (0.d0,j=1,14)/))
      parameter (fbaga = (/(0.d0,j=1,7),1.d0,(0.d0,j=1,13)/))
      parameter (sbaga =
     & (/(0.d0,j=1,5),1.5822d0,0.39d0,(0.d0,j=1,14)/))
      parameter (wbaga = (/(0.d0,j=1,5),1.d0,(0.d0,j=1,15)/))

c..Binary interaction parameter values
c..eijs,uija,kija and gijs are transposed (to be easily rewriten as parameter)
      parameter (eijs=reshape((/1.d0,0.97164d0,0.960644d0,1.d0,
     &0.994635d0,0.708218d0,0.931484d0,1.17052d0,0.990126d0,1.d0,
     &1.01953d0,0.989844d0,1.00235d0,0.999268d0,1.107274d0,0.88088d0,
     &0.880973d0,0.881067d0,0.881161d0,1.d0,1.d0,(1.d0,j=1,2),1.02274d0,
     &0.97012d0,0.945939d0,0.746954d0,0.902271d0,1.08632d0,1.00571d0,
     &1.021d0,0.946914d0,0.973384d0,0.95934d0,0.94552d0,(1.d0,j=1,7),
     &(1.d0,j=1,3),0.925053d0,0.960237d0,0.849408d0,0.955052d0,
     &1.28179d0,1.5d0,1.d0,0.906849d0,0.897362d0,0.726255d0,0.859764d0,
     &0.855134d0,0.831229d0,0.80831d0,0.786323d0,0.765171d0,
     &(1.d0,j=1,2),(1.d0,j=1,4),1.02256d0,0.693168d0,0.946871d0,
     &1.16446d0,(1.d0,j=1,3),1.01306d0,1.d0,1.00532d0,(1.d0,j=1,7),
     &(1.d0,j=1,7),1.034787d0,(1.d0,j=1,3),1.0049d0,(1.d0,j=1,9),
     &(1.d0,j=1,21),(1.d0,j=1,14),1.008692d0,1.010126d0,1.011501d0,
     &1.012821d0,1.014089d0,(1.d0,j=1,2),(1.d0,j=1,8),1.1d0,1.d0,1.3d0,
     &1.3d0,(1.d0,j=1,9),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     &(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     &(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     &(1.d0,j=1,21),(1.d0,j=1,21)/),(/21,21/)))

      parameter (uija=reshape((/1.d0,0.886106d0,0.963827d0,
     & 1.d0,0.990877d0,
     & 1.d0,0.736833d0,1.15639d0,(1.d0,j=1,3),0.992291d0,1.d0,1.00367d0,
     & 1.302576d0,1.191904d0,1.205769d0,1.219634d0,1.233498d0,
     & (1.d0,j=1,2),(1.d0,j=1,2),0.835058d0,0.816431d0,0.915502d0,
     & 1.d0,0.993476d0,0.408838d0,(1.d0,j=1,3),0.993556d0,(1.d0,j=1,9),
     & (1.d0,j=1,3),0.96987d0,(1.d0,j=1,2),1.04529d0,1.d0,
     & 0.9d0,(1.d0,j=1,5),1.066638d0,1.077634d0,1.088178d0,1.098291d0,
     & 1.108021d0,(1.d0,j=1,2),
     & (1.d0,j=1,4),1.065173d0,1.d0,0.971926d0,
     & 1.61666d0,(1.d0,j=1,2),(1.25d0,j=1,4),(1.d0,j=1,7),
     & (1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,14),1.028973d0,1.033754d0,1.038338d0,
     & 1.042735d0,1.046966d0,(1.d0,j=1,2),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21)/),(/21,21/)))

      parameter (kija=reshape(
     & (/1.d0,1.00363d0,0.995933d0,1.d0,1.007619d0,
     & 1.d0,1.00008d0,1.02326d0,(1.d0,j=1,3),0.997596d0,1.d0,1.002529d0,
     & 0.982962d0,0.983565d0,0.982707d0,0.981849d0,0.980991d0,
     & (1.d0,j=1,2),(1.d0,j=1,2),0.982361d0,1.00796d0,(1.d0,j=1,2),
     & 0.942596d0,1.03227d0,(1.d0,j=1,13),
     & (1.d0,j=1,3),1.00851d0,(1.d0,j=1,2),1.00779d0,(1.d0,j=1,7),
     & 0.910183d0,0.895362d0,0.881152d0,0.86752d0,0.854406d0,
     & (1.d0,j=1,2),(1.d0,j=1,4),0.986893d0,1.d0,0.999969d0,
     & 1.02034d0,(1.d0,j=1,13),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,14),0.96813d0,0.96287d0,0.957828d0,
     & 0.952441d0,0.948338d0,(1.d0,j=1,2),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21)
     & /),(/21,21/)))

      parameter (gijs=reshape(
     & (/(1.d0,j=1,2),.807653d0,(1.d0,j=1,4),1.95731d0,(1.d0,j=1,13),
     & (1.d0,j=1,2),0.982746d0,(1.d0,j=1,18),
     & (1.d0,j=1,3),0.370296d0,1.d0,1.67309d0,(1.d0,j=1,15),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),(1.d0,j=1,21),
     & (1.d0,j=1,21),(1.d0,j=1,21)/),(/21,21/)))

c..Cp0 coefficients given in:
c..McFall, R.L., M.S. Thesis, University of Oklahoma, 1984.
c..Aly, F.A. and Lee, L.L., Fluid Phase Equilib., 6:169, 1981.

      double precision acp0(21),bcp0(21),ccp0(21),dcp0(21),ecp0(21),
     &        fcp0(21),gcp0(21),hcp0(21),icp0(21),jcp0(21),kcp0(21)
      parameter (acp0 =
     & (/-29776.4d0,-3495.34d0,20.7307d0,-37524.4d0,-56072.1d0,
     & -13773.1d0,-10085.4d0,-5565.6d0,-2753.49d0,-3497.45d0,-72387.d0,
     & -72674.8d0,-91505.5d0,-83845.2d0,-94982.5d0,-103353.d0,
     & -109674.d0,-122599.d0,-133564.d0,0d0,0d0/))
      parameter (bcp0 =
     & (/7.95454d0,6.95587d0,6.96237d0,7.98139d0,8.14319d0,
     & 7.97183d0,7.9468d0,6.66789d0,6.95854d0,6.96302d0,17.8143d0,
     & 18.6383d0,21.3861d0,22.5012d0,26.6225d0,30.4029d0,34.0847d0,
     & 38.5014d0,42.7143d0,4.968d0,4.968d0/))
      parameter (ccp0 =
     & (/43.9417d0,0.272892d0,2.68645d0,24.3668d0,37.0629d0,
     & 6.27078d0,-0.0838d0,2.33458d0,2.02441d0,2.40013d0,58.2062d0,
     & 57.4178d0,74.341d0,69.5789d0,80.3819d0,90.6941d0,100.253d0,
     & 111.446d0,122.173d0,0.d0,0.d0/))
      parameter (dcp0 =
     & (/1037.09d0,662.738d0,500.371d0,752.32d0,735.402d0,
     & 2572.63d0,433.801d0,2584.98d0,1541.22d0,2522.05d0,1787.39d0,
     & 1792.73d0,1701.58d0,1719.58d0,1718.49d0,1669.32d0,1611.55d0,
     & 1646.48d0,1654.85d0,100.d0,100.d0/))
      parameter (ecp0 =
     & (/1.56373d0,-0.291318d0,-2.56429d0,3.5399d0,9.38159d0,
     & 2.0501d0,2.85539d0,0.749019d0,0.096774d0,2.21752d0,40.7621d0,
     & 38.6599d0,47.0587d0,46.2164d0,55.6598d0,63.2028d0,69.7675d0,
     & 80.5015d0,90.2255d0,0.d0,0.d0/))
      parameter (fcp0 =
     & (/813.205d0,-680.562d0,-530.443d0,272.846d0,247.19d0,
     & 1156.72d0,843.792d0,559.656d0,3674.81d0,1154.15d0,808.645d0,
     & 814.151d0,775.899d0,802.174d0,802.069d0,786.001d0,768.847d0,
     & 781.588d0,785.564d0,100.d0,100.d0/))
      parameter (gcp0 =
     & (/-24.9027d0,1.7898d0,3.91921d0,8.44724d0,13.4556d0,
     & 0.d0,6.31595d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,
     & 0.d0,0.d0,0.d0,0.d0/))
      parameter (hcp0 =
     & (/1019.98d0,1740.06d0,500.198d0,1020.13d0,1454.78d0,
     & 100.d0,1481.43d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,
     & 100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0/))
      parameter (icp0 =
     & (/-10.1601d0,0.d0,2.1329d0,-13.2732d0,-11.7342d0,
     & 0.d0,-2.88457d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,
     & 0.d0,0.d0,0.d0,0.d0,0.d0/))
      parameter (jcp0 = (/1070.14d0,100.d0,2197.22d0,869.51d0,984.518d0,
     & 100.d0,1102.23d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,
     & 100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0,100.d0/))
      parameter (kcp0 =
     & (/-20.0615d0,4.49823d0,5.81381d0,-22.401d0,-24.0426d0,
     & -3.24989d0,-0.51551d0,-7.94821d0,6.23387d0,9.19749d0,-44.1341d0,
     & -46.1938d0,-60.2474d0,-62.2197d0,-77.5366d0,-92.0164d0,
     & -106.149d0,-122.444d0,-138.006d0,1.8198d0,8.6776d0/))

c..use these when making a threadsafe library, turn them off when
c..compiling the fitter so that the variables are common to all threads
c!$omp threadprivate(/NCOMP/)
c!$omp threadprivate(/ZRED/)
c!$omp threadprivate(/HCHAR/)
c!$omp threadprivate(/PRNTERR/)
c!$omp threadprivate(/FLDPTH/)
c!$omp threadprivate(/VERS/)
c!$omp threadprivate(/RESETM/)
c!$omp threadprivate(/CITE/)
c!$omp threadprivate(/IRFSAV/)
c!$omp threadprivate(/SETSAV/)
c!$omp threadprivate(/MODSAV/)
c!$omp threadprivate(/REFSAV/)
c!$omp threadprivate(/CREF/)
c!$omp threadprivate(/REFST/)
c!$omp threadprivate(/CCON/)
c!$omp threadprivate(/CCAS/)
c!$omp threadprivate(/HTCM/)
c!$omp threadprivate(/CNAM/)
c!$omp threadprivate(/CNAM80/)
c!$omp threadprivate(/FAML/)
c!$omp threadprivate(/FXPNT/)
c!$omp threadprivate(/FLAGS/)
c!$omp threadprivate(/FLAGS2/)
c!$omp threadprivate(/EOSMOD/)
c!$omp threadprivate(/EOSLIM/)
c!$omp threadprivate(/WNTFEQ/)
c!$omp threadprivate(/WCFFEQ/)
c!$omp threadprivate(/WCFFQ2/)
c!$omp threadprivate(/WLFFEQ/)
c!$omp threadprivate(/FEQSV3/)
c!$omp threadprivate(/WCFBWR/)
c!$omp threadprivate(/CFECS/)
c!$omp threadprivate(/ICFECS/)
c!$omp threadprivate(/WNTQUI/)
c!$omp threadprivate(/WCFQUI/)
c!$omp threadprivate(/QUISV2/)
c!$omp threadprivate(/QUISAV/)
c!$omp threadprivate(/QUITRM/)
c!$omp threadprivate(/WNTCPP/)
c!$omp threadprivate(/WCPCPP/)
c!$omp threadprivate(/WRDCPP/)
c!$omp threadprivate(/WLMCPP/)
c!$omp threadprivate(/WNTPH0/)
c!$omp threadprivate(/WLMPH0/)
c!$omp threadprivate(/WCFPH0/)
c!$omp threadprivate(/MIXMOD/)
c!$omp threadprivate(/CFXHMX/)
c!$omp threadprivate(/CFIHMX/)
c!$omp threadprivate(/CRTMOD/)
c!$omp threadprivate(/CFXCRT/)
c!$omp threadprivate(/CF2HMX/)
c!$omp threadprivate(/MXINFO/)
c!$omp threadprivate(/HMXFLG/)
c!$omp threadprivate(/PRMOD/)
c!$omp threadprivate(/MXINF1/)
c!$omp threadprivate(/MXINF2/)
c!$omp threadprivate(/MXRULE/)
c!$omp threadprivate(/CREFDF/)
c!$omp threadprivate(/CFXPR/)
c!$omp threadprivate(/PSMOD/)
c!$omp threadprivate(/WLMPS/)
c!$omp threadprivate(/WCFPS/)
c!$omp threadprivate(/WRDPS/)
c!$omp threadprivate(/WNTPS/)
c!$omp threadprivate(/PLMOD/)
c!$omp threadprivate(/WLMPL/)
c!$omp threadprivate(/WCFPL/)
c!$omp threadprivate(/WRDPL/)
c!$omp threadprivate(/WNTPL/)
c!$omp threadprivate(/DLMOD/)
c!$omp threadprivate(/WLMDL/)
c!$omp threadprivate(/WCFDL/)
c!$omp threadprivate(/WRDDL/)
c!$omp threadprivate(/WNTDL/)
c!$omp threadprivate(/DVMOD/)
c!$omp threadprivate(/WLMDV/)
c!$omp threadprivate(/WNTDV/)
c!$omp threadprivate(/WCFDV/)
c!$omp threadprivate(/WRDDV/)
c!$omp threadprivate(/MELTMOD/)
c!$omp threadprivate(/WLMMELT/)
c!$omp threadprivate(/WCFMELT/)
c!$omp threadprivate(/WRDMELT/)
c!$omp threadprivate(/WNTMELT/)
c!$omp threadprivate(/SUBLMOD/)
c!$omp threadprivate(/WLMSUBL/)
c!$omp threadprivate(/WCFSUBL/)
c!$omp threadprivate(/WRDSUBL/)
c!$omp threadprivate(/WNTSUBL/)
c!$omp threadprivate(/DEMOD/)
c!$omp threadprivate(/WLMDE/)
c!$omp threadprivate(/WCFDE/)
c!$omp threadprivate(/WRDDE/)
c!$omp threadprivate(/WNTDE/)
c!$omp threadprivate(/STNMOD/)
c!$omp threadprivate(/WLMSTN/)
c!$omp threadprivate(/WCFST1/)
c!$omp threadprivate(/WNTST1/)
c!$omp threadprivate(/PRSV2/)
c!$omp threadprivate(/WCFPR/)
c!$omp threadprivate(/CONPR/)
c!$omp threadprivate(/PRHSAV/)
c!$omp threadprivate(/CONUN/)
c!$omp threadprivate(/VAR/)
c!$omp threadprivate(/INTMCF/)
c!$omp threadprivate(/AGASV1/)
c!$omp threadprivate(/AGASV2/)
c!$omp threadprivate(/AGASV3/)
c!$omp threadprivate(/AUXFLGS/)
c!$omp threadprivate(/CSPNDL/)
c!$omp threadprivate(/SATSPL/)
c!$omp threadprivate(/MAXTP/)
c!$omp threadprivate(/PRSAV/)


c..do not ever turn these off, they are needed in all cases, especially
c..when fitting equations of state
c..the following are used:  ncmax,n0,nx,mxtrm,mxcrt
!$omp threadprivate(/Gcnst/)
!$omp threadprivate(/TSTSAV/)
!$omp threadprivate(/PSTSAV/)
!$omp threadprivate(/FSHSAV/)
!$omp threadprivate(/FEQSAV/)
!$omp threadprivate(/TERMS/)
!$omp threadprivate(/FEQSV2/)
!$omp threadprivate(/CRTSAV/)
!$omp threadprivate(/CPPSAV/)
!$omp threadprivate(/PH0SAV/)

c..these are only needed when fitting mixtures
c..the additional following are used:  nmsav,nmxtrm
c!$omp threadprivate(/HMXSV2/)
c!$omp threadprivate(/HMXSAV/)
c!$omp threadprivate(/MXTRMS/)
c!$omp threadprivate(/PHIDR/)
c!$omp threadprivate(/PHI2DR/)

c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                   end file commons.for
c ======================================================================
