c ======================================================================
c  begin file pass_ftn.for
c
c  This file defines the DLL-callable routines which would be used by
c  Visual Basic, Excel, and other applications to access the main
c  Fortran code.  It is not needed when compiling Fortran applications.
c  It is only used when compiling the DLL and contains commands specific
c  to the Lahey/Fujitsu Fortran 95 Compiler.  When using other compilers,
c  the lines with 'dll_export' should be commented out.
c
c  The calling sequence is identical (except for the SETUPdll routine) to
c  the main Fortran routines, with the letters 'dll' appended to the routine
c  name.  See the Excel sample file or the Visual Basic 'SAMPLE.BAS'
c  file for usage information.
c
c ======================================================================
      subroutine RPVersion (v)
      include 'commons.for'
      character*3 hrf
      character*255 hfiles(ncmax),hfmix,herr,v
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "RPVersion"::RPVersion
cDEC$ ATTRIBUTES STDCALL, REFERENCE::RPVersion
c     dll_export RPVersion
      call SETUP (-1,hfiles,hfmix,hrf,ierr,herr)
      write (v,'(f6.4)') 1.d-4*ierr
      if (v(6:7).eq.'0 ') v(6:6)=' '
      if (v(5:6).eq.'0 ') v(5:5)=' '
      if (v(4:5).eq.'0 ') v(4:4)=' '
      end
c ======================================================================
      subroutine SETUPdll (i,hfld,hfm,hrf,ierr,herr)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETUPdll"::SETUPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETUPdll
c     dll_export SETUPdll
      integer*4 i
      character*10000 hfld
      character*255 hfm, herr
      character*3 hrf
      call SETUP0 (i,hfld,hfm,hrf,ierr,herr)
      end
c ======================================================================
      subroutine SETREFdll (hrf,ixflag,x0,h0,s0,t0,p0,ierr,herr)
      include 'commons.for'
      character*255 herr
      character*3 hrf
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETREFdll"::SETREFdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETREFdll
c     dll_export SETREFdll
      dimension x0(ncmax)
      call SETREF (hrf,ixflag,x0,h0,s0,t0,p0,ierr,herr)
      end
c ======================================================================
      subroutine SETMIXdll (hmxnme,hfmix,hrf,ncc,hfile,x,ierr,herr)
      include 'commons.for'
      character*255 hmxnme,hfmix,hfiles(ncmax),herr
      character hfile*10000,hrf*3
      dimension x(ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETMIXdll"::SETMIXdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETMIXdll
c     dll_export SETMIXdll
      call SETMIX (hmxnme,hfmix,hrf,ncc,hfiles,x,ierr,herr)
      hfile=hfiles(1)
      j=index(hfile,'   ')
      hfile=hfile(1:j-1)//'|'
      do i=2,ncc
        j=index(hfile,'   ')
        hfile=hfile(1:j-1)//hfiles(i)
        j=index(hfile,'   ')
        hfile=hfile(1:j-1)//'|'
      enddo
      end
c ======================================================================
      subroutine SETMODdll (ncomp,htype,hmix,hcomp2,ierr,herr)
      include 'commons.for'
      character*255 herr
      character*60 hcomp2
      character*3 htype,hmix,hcomp(1:ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETMODdll"::SETMODdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETMODdll
c     dll_export SETMODdll
      do i=1,ncmax
        hcomp(i)=hcomp2(i*3-2:i*3)
      enddo
      call SETMOD (ncomp,htype,hmix,hcomp,ierr,herr)
      end
c ======================================================================
      subroutine PUREFLDdll (icomp)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PUREFLDdll"::PUREFLDdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PUREFLDdll
c     dll_export PUREFLDdll
      call PUREFLD (icomp)
      end
c ======================================================================
      subroutine SETNCdll (ncomp)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETNCdll"::SETNCdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETNCdll
c     dll_export SETNCdll
      call SETNC (ncomp)
      end
c ======================================================================
      subroutine SETPATHdll (hpth)
      character hpth*255
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETPATHdll"::SETPATHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETPATHdll
c     dll_export SETPATHdll
      call SETPATH (hpth)
      end
c ======================================================================
      subroutine UNSETAGAdll
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "UNSETAGAdll"::UNSETAGAdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::UNSETAGAdll
c     dll_export UNSETAGAdll
      call UNSETAGA
      end
c ======================================================================
      subroutine CRITPdll (x,tc,pc,Dc,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CRITPdll"::CRITPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CRITPdll
c     dll_export CRITPdll
      dimension x(ncmax)
      call CRITP (x,tc,pc,Dc,ierr,herr)
      end
c ======================================================================
      subroutine MAXTdll (x,tm,pm,Dm,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "MAXTdll"::MAXTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::MAXTdll
c     dll_export MAXTdll
      dimension x(ncmax)
      call MAXT (x,tm,pm,Dm,ierr,herr)
      end
c ======================================================================
      subroutine MAXPdll (x,tm,pm,Dm,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "MAXPdll"::MAXPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::MAXPdll
c     dll_export MAXPdll
      dimension x(ncmax)
      call MAXP (x,tm,pm,Dm,ierr,herr)
      end
c ======================================================================
      subroutine SATSPLNdll (x,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATSPLNdll"::SATSPLNdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATSPLNdll
c     dll_export SATSPLNdll
      dimension x(ncmax)
      call SATSPLN (x,ierr,herr)
      end
c ======================================================================
      subroutine RMIX2dll (x,Rgas)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "RMIX2dll"::RMIX2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::RMIX2dll
c     dll_export RMIX2dll
      dimension x(ncmax)
      call RMIX2 (x,Rgas)
      end
c ======================================================================
      subroutine THERMdll (t,rho,x,p,e,h,s,cv,cp,w,hjt)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "THERMdll"::THERMdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::THERMdll
c     dll_export THERMdll
      dimension x(ncmax)
      call THERM (t,rho,x,p,e,h,s,cv,cp,w,hjt)
      end
c ======================================================================
      subroutine THERM0dll (t,rho,x,p,e,h,s,cv,cp,w,a,g)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "THERM0dll"::THERM0dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::THERM0dll
c     dll_export THERM0dll
      dimension x(ncmax)
      call THERM0 (t,rho,x,p,e,h,s,cv,cp,w,a,g)
      end
c ======================================================================
      subroutine THERM2dll (t,rho,x,p,e,h,s,cv,cp,w,Z,hjt,A,G,xkappa,
     &                   beta,dPdD,d2PdD2,dPdT,dDdT,dDdP,
     &                   d2PT2,d2PdTD,spare3,spare4)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "THERM2dll"::THERM2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::THERM2dll
c     dll_export THERM2dll
      dimension x(ncmax)
      call THERM2 (t,rho,x,p,e,h,s,cv,cp,w,Z,hjt,A,G,xkappa,beta,
     &                   dPdD,d2PdD2,dPdT,dDdT,dDdP,
     &                   d2PT2,d2PdTD,spare3,spare4)
      end
c ======================================================================
      subroutine THERM3dll (t,rho,x,
     &           xkappa,beta,xisenk,xkt,betas,bs,xkkt,thrott,pi,spht)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "THERM3dll"::THERM3dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::THERM3dll
c     dll_export THERM3dll
      dimension x(ncmax)
      call THERM3 (t,rho,x,
     &           xkappa,beta,xisenk,xkt,betas,bs,xkkt,thrott,pi,spht)
      end
c ======================================================================
      subroutine RESIDUALdll (t,rho,x,pr,er,hr,sr,cvr,cpr,Ar,Gr)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "RESIDUALdll"::RESIDUALdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::RESIDUALdll
c     dll_export RESIDUALdll
      dimension x(ncmax)
      call RESIDUAL (t,rho,x,pr,er,hr,sr,cvr,cpr,Ar,Gr)
      end
c ======================================================================
      subroutine ENTROdll (t,rho,x,s)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ENTROdll"::ENTROdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ENTROdll
c     dll_export ENTROdll
      dimension x(ncmax)
      call ENTRO (t,rho,x,s)
      end
c ======================================================================
      subroutine ENTHALdll (t,rho,x,h)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ENTHALdll"::ENTHALdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ENTHALdll
c     dll_export ENTHALdll
      dimension x(ncmax)
      call ENTHAL (t,rho,x,h)
      end
c ======================================================================
      subroutine CVCPdll (t,rho,x,cv,cp)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CVCPdll"::CVCPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CVCPdll
c     dll_export CVCPdll
      dimension x(ncmax)
      call CVCP (t,rho,x,cv,cp)
      end
c ======================================================================
      subroutine CVCPKdll (icomp,t,rho,cv,cp)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CVCPKdll"::CVCPKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CVCPKdll
c     dll_export CVCPKdll
      call CVCPK (icomp,t,rho,cv,cp)
      end
c ======================================================================
      subroutine GIBBSdll (t,rho,x,Ar,Gr)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "GIBBSdll"::GIBBSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::GIBBSdll
c     dll_export GIBBSdll
      dimension x(ncmax)
      call GIBBS (t,rho,x,Ar,Gr)
      end
c ======================================================================
      subroutine AGdll (t,rho,x,a,g)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "AGdll"::AGdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::AGdll
c     dll_export AGdll
      dimension x(ncmax)
      call AG (t,rho,x,a,g)
      end
c ======================================================================
      subroutine PRESSdll (t,rho,x,p)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PRESSdll"::PRESSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PRESSdll
c     dll_export PRESSdll
      dimension x(ncmax)
      call PRESS (t,rho,x,p)
      end
c ======================================================================
      subroutine DPDDdll (t,rho,x,dpdrho)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DPDDdll"::DPDDdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DPDDdll
c     dll_export DPDDdll
      dimension x(ncmax)
      call DPDD (t,rho,x,dpdrho)
      end
c ======================================================================
      subroutine DPDDKdll (icomp,t,rho,dpdrho)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DPDDKdll"::DPDDKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DPDDKdll
c     dll_export DPDDKdll
      call DPDDK (icomp,t,rho,dpdrho)
      end
c ======================================================================
      subroutine DPDD2dll (t,rho,x,d2PdD2)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DPDD2dll"::DPDD2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DPDD2dll
c     dll_export DPDD2dll
      dimension x(ncmax)
      call DPDD2 (t,rho,x,d2PdD2)
      end
c ======================================================================
      subroutine DPDTdll (t,rho,x,dpt)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DPDTdll"::DPDTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DPDTdll
c     dll_export DPDTdll
      dimension x(ncmax)
      call DPDT (t,rho,x,dpt)
      end
c ======================================================================
      subroutine DPDTKdll (icomp,t,rho,dpt)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DPDTKdll"::DPDTKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DPDTKdll
c     dll_export DPDTKdll
      call DPDTK (icomp,t,rho,dpt)
      end
c ======================================================================
      subroutine DDDPdll (t,rho,x,drhodp)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DDDPdll"::DDDPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DDDPdll
c     dll_export DDDPdll
      dimension x(ncmax)
      call DDDP (t,rho,x,drhodp)
      end
c ======================================================================
      subroutine DDDTdll (t,rho,x,drhodt)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DDDTdll"::DDDTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DDDTdll
c     dll_export DDDTdll
      dimension x(ncmax)
      call DDDT (t,rho,x,drhodt)
      end
c ======================================================================
      subroutine DHD1dll (t,rho,x,dhdt_d,dhdt_p,dhdd_t,dhdd_p,dhdp_t,
     &                    dhdp_d)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DHD1dll"::DHD1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DHD1dll
c     dll_export DHD1dll
      dimension x(ncmax)
      call DHD1 (t,rho,x,dhdt_d,dhdt_p,dhdd_t,dhdd_p,dhdp_t,dhdp_d)
      end
c ======================================================================
      subroutine FGCTYdll (t,rho,x,f)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "FGCTYdll"::FGCTYdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::FGCTYdll
c     dll_export FGCTYdll
      dimension x(ncmax),f(ncmax)
      call FGCTY (t,rho,x,f)
      end
c ======================================================================
      subroutine FGCTY2dll (t,rho,x,f,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "FGCTY2dll"::FGCTY2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::FGCTY2dll
c     dll_export FGCTY2dll
      dimension x(ncmax),f(ncmax)
      call FGCTY2 (t,rho,x,f,ierr,herr)
      end
c ======================================================================
      subroutine FUGCOFdll (t,rho,x,f,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "FUGCOFdll"::FUGCOFdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::FUGCOFdll
c     dll_export FUGCOFdll
      dimension x(ncmax),f(ncmax)
      call FUGCOF (t,rho,x,f,ierr,herr)
      end
c ======================================================================
      subroutine CHEMPOTdll (t,rho,x,u,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CHEMPOTdll"::CHEMPOTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CHEMPOTdll
c     dll_export CHEMPOTdll
      dimension x(ncmax),u(ncmax)
      call CHEMPOT (t,rho,x,u,ierr,herr)
      end
c ======================================================================
      subroutine ACTVYdll (t,rho,x,actv,gamma,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ACTVYdll"::ACTVYdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ACTVYdll
c     dll_export ACTVYdll
      dimension x(ncmax),actv(ncmax),gamma(ncmax)
      call ACTVY (t,rho,x,actv,gamma,ierr,herr)
      end
c ======================================================================
      subroutine VIRBdll (t,x,b)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "VIRBdll"::VIRBdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::VIRBdll
c     dll_export VIRBdll
      dimension x(ncmax)
      call VIRB (t,x,b)
      end
c ======================================================================
      subroutine B12dll (t,x,b)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "B12dll"::B12dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::B12dll
c     dll_export B12dll
      dimension x(ncmax)
      call B12 (t,x,b)
      end
c ======================================================================
      subroutine DBDTdll (t,x,b)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DBDTdll"::DBDTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DBDTdll
c     dll_export DBDTdll
      dimension x(ncmax)
      call DBDT (t,x,b)
      end
c ======================================================================
      subroutine VIRCdll (t,x,c)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "VIRCdll"::VIRCdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::VIRCdll
c     dll_export VIRCdll
      dimension x(ncmax)
      call VIRC (t,x,c)
      end
c ======================================================================
      subroutine VIRBAdll (t,x,b)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "VIRBAdll"::VIRBAdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::VIRBAdll
c     dll_export VIRBAdll
      dimension x(ncmax)
      call VIRBA (t,x,b)
      end
c ======================================================================
      subroutine VIRCAdll (t,x,c)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "VIRCAdll"::VIRCAdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::VIRCAdll
c     dll_export VIRCAdll
      dimension x(ncmax)
      call VIRCA (t,x,c)
      end
c ======================================================================
      subroutine HEATdll (t,rho,x,hg,hn,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "HEATdll"::HEATdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::HEATdll
c     dll_export HEATdll
      dimension x(ncmax)
      CALL HEAT (t,rho,x,hg,hn,ierr,herr)
      end
c ======================================================================
      subroutine SATGVdll (t,p,z,vf,b,ipv,ityp,isp,rhox,rhoy,x,y,
     &                     ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATGVdll"::SATGVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATGVdll
c     dll_export SATGVdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call SATGV (t,p,z,vf,b,ipv,ityp,isp,rhox,rhoy,x,y,ierr,herr)
      end
c ======================================================================
      subroutine SATTPdll (t,p,x,kph,iGuess,d,rhol,rhov,xliq,xvap,q,
     &                     ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATTPdll"::SATTPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATTPdll
c     dll_export SATTPdll
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      call SATTP (t,p,x,kph,iGuess,d,rhol,rhov,xliq,xvap,q,ierr,herr)
      end
c ======================================================================
      subroutine SATTdll (t,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATTdll"::SATTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATTdll
c     dll_export SATTdll
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      call SATT (t,x,kph,p,rhol,rhov,xliq,xvap,ierr,herr)
      end
c ======================================================================
      subroutine SATPdll (p,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATPdll"::SATPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATPdll
c     dll_export SATPdll
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      call SATP (p,x,kph,t,rhol,rhov,xliq,xvap,ierr,herr)
      end
c ======================================================================
      subroutine SATDdll (rho,x,kph,kr,t,p,rhol,rhov,xliq,xvap,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATDdll"::SATDdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATDdll
c     dll_export SATDdll
      dimension x(ncmax),xliq(ncmax),xvap(ncmax)
      call SATD (rho,x,kph,kr,t,p,rhol,rhov,xliq,xvap,i,herr)
      end
c ======================================================================
      subroutine SATHdll (h,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATHdll"::SATHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATHdll
c     dll_export SATHdll
      dimension x(ncmax)
      call SATH (h,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,i,herr)
      end
c ======================================================================
      subroutine SATEdll (e,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATEdll"::SATEdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATEdll
c     dll_export SATEdll
      dimension x(ncmax)
      call SATE (e,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,i,herr)
      end
c ======================================================================
      subroutine SATSdll (s,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,
     &                 k3,t3,p3,d3,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATSdll"::SATSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATSdll
c     dll_export SATSdll
      dimension x(ncmax)
      call SATS (s,x,kph,nroot,k1,t1,p1,d1,k2,t2,p2,d2,
     &                 k3,t3,p3,d3,ierr,herr)
      end
c ======================================================================
      subroutine SATTESTdll (t,x,kph,p,x2,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATTESTdll"::SATTESTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATTESTdll
c     dll_export SATTESTdll
      dimension x(ncmax),x2(ncmax)
      call SATTEST (t,x,kph,p,x2,ierr,herr)
      end
c ======================================================================
      subroutine SATPESTdll (p,x,kph,t,x2,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SATPESTdll"::SATPESTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SATPESTdll
c     dll_export SATPESTdll
      dimension x(ncmax),x2(ncmax)
      call SATPEST (p,x,kph,t,x2,ierr,herr)
      end
c ======================================================================
      subroutine CSATKdll (icomp,t,kph,p,rho,csat,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CSATKdll"::CSATKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CSATKdll
c     dll_export CSATKdll
      call CSATK (icomp,t,kph,p,rho,csat,ierr,herr)
      end
c ======================================================================
      subroutine DPTSATKdll (icomp,t,kph,p,rho,csat,dpt,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DPTSATKdll"::DPTSATKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DPTSATKdll
c     dll_export DPTSATKdll
      call DPTSATK (icomp,t,kph,p,rho,csat,dpt,ierr,herr)
      end
c ======================================================================
      subroutine CV2PKdll (icomp,t,rho,cv2p,csat,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CV2PKdll"::CV2PKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CV2PKdll
c     dll_export CV2PKdll
      call CV2PK (icomp,t,rho,cv2p,csat,ierr,herr)
      end
c ======================================================================
      subroutine PSATKdll (icomp,t,p,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PSATKdll"::PSATKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PSATKdll
c     dll_export PSATKdll
      call PSATK (icomp,t,p,ierr,herr)
      end
c ======================================================================
      subroutine DLSATKdll (icomp,t,d,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DLSATKdll"::DLSATKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DLSATKdll
c     dll_export DLSATKdll
      call DLSATK (icomp,t,d,ierr,herr)
      end
c ======================================================================
      subroutine DVSATKdll (icomp,t,d,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DVSATKdll"::DVSATKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DVSATKdll
c     dll_export DVSATKdll
      call DVSATK (icomp,t,d,ierr,herr)
      end
c ======================================================================
      subroutine TSATDdll (d,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TSATDdll"::TSATDdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TSATDdll
c     dll_export TSATDdll
      dimension x(ncmax)
      call TSATD (d,x,t,ierr,herr)
      end
c ======================================================================
      subroutine TPRHOdll (t,p,x,j,i,rho,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TPRHOdll"::TPRHOdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TPRHOdll
c     dll_export TPRHOdll
      dimension x(ncmax)
      call TPRHO (t,p,x,j,i,rho,ierr,herr)
      end
c ======================================================================
      subroutine TPRHOPRdll (t,p,x,rho1,rho2)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TPRHOPRdll"::TPRHOPRdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TPRHOPRdll
c     dll_export TPRHOPRdll
      dimension x(ncmax)
      call TPRHOPR (t,p,x,rho1,rho2)
      end
c ======================================================================
      subroutine TPFLSHdll (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TPFLSHdll"::TPFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TPFLSHdll
c     dll_export TPFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      if (t.gt.0.d0)
     &    call TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine TDFLSHdll (t,D,x,p,Dl,Dv,xl,xv,q,e,h,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TDFLSHdll"::TDFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TDFLSHdll
c     dll_export TDFLSHdll
      dimension x(ncmax),xl(ncmax),xv(ncmax)
      call TDFLSH (t,D,x,p,Dl,Dv,xl,xv,q,e,h,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine PDFLSHdll (p,D,z,t,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PDFLSHdll"::PDFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PDFLSHdll
c     dll_export PDFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call PDFLSH (p,D,z,t,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine PHFLSHdll (p,h,z,t,D,Dl,Dv,x,y,q,e,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PHFLSHdll"::PHFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PHFLSHdll
c     dll_export PHFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call PHFLSH (p,h,z,t,D,Dl,Dv,x,y,q,e,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine PSFLSHdll (p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PSFLSHdll"::PSFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PSFLSHdll
c     dll_export PSFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call PSFLSH (p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine PEFLSHdll (p,e,z,t,D,Dl,Dv,x,y,q,h,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PEFLSHdll"::PEFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PEFLSHdll
c     dll_export PEFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call PEFLSH (p,e,z,t,D,Dl,Dv,x,y,q,h,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine THFLSHdll (t,h,z,kr,p,D,Dl,Dv,x,y,q,e,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "THFLSHdll"::THFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::THFLSHdll
c     dll_export THFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call THFLSH (t,h,z,kr,p,D,Dl,Dv,x,y,q,e,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine TSFLSHdll (t,s,z,kr,p,D,Dl,Dv,x,y,q,e,h,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TSFLSHdll"::TSFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TSFLSHdll
c     dll_export TSFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call TSFLSH (t,s,z,kr,p,D,Dl,Dv,x,y,q,e,h,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine TEFLSHdll (t,e,z,kr,p,D,Dl,Dv,x,y,q,h,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TEFLSHdll"::TEFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TEFLSHdll
c     dll_export TEFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call TEFLSH (t,e,z,kr,p,D,Dl,Dv,x,y,q,h,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine DHFLSHdll (D,h,z,t,p,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DHFLSHdll"::DHFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DHFLSHdll
c     dll_export DHFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call DHFLSH (D,h,z,t,p,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine DSFLSHdll (D,s,z,t,p,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DSFLSHdll"::DSFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DSFLSHdll
c     dll_export DSFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call DSFLSH (D,s,z,t,p,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine DEFLSHdll (D,e,z,t,p,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DEFLSHdll"::DEFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DEFLSHdll
c     dll_export DEFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call DEFLSH (D,e,z,t,p,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine HSFLSHdll (h,s,z,t,p,D,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "HSFLSHdll"::HSFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::HSFLSHdll
c     dll_export HSFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call HSFLSH (h,s,z,t,p,D,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine ESFLSHdll (e,s,z,t,p,D,Dl,Dv,x,y,q,h,cv,cp,w,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ESFLSHdll"::ESFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ESFLSHdll
c     dll_export ESFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call ESFLSH (e,s,z,t,p,D,Dl,Dv,x,y,q,h,cv,cp,w,ierr,herr)
      end
c ======================================================================
      subroutine ABFL1dll (a,b,x,kph,ab,dmin,dmax,t,p,D,ierr,herr)
      include 'commons.for'
      character*2 ab
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ABFL1dll"::ABFL1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ABFL1dll
c     dll_export ABFL1dll
      dimension x(ncmax)
      call ABFL1 (a,b,x,kph,ab,dmin,dmax,t,p,D,ierr,herr)
      end
c ======================================================================
      subroutine DBFL1dll (d,b,x,ab,t,p,ierr,herr)
      include 'commons.for'
      character*2 ab
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DBFL1dll"::DBFL1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DBFL1dll
c     dll_export DBFL1dll
      dimension x(ncmax)
      call DBFL1 (d,b,x,ab,t,p,ierr,herr)
      end
c ======================================================================
      subroutine ABFL2dll (a,b,z,kq,ksat,ab,
     &                     tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                     t,p,Dl,Dv,x,y,q,ierr,herr)
      include 'commons.for'
      character*2 ab
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ABFL2dll"::ABFL2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ABFL2dll
c     dll_export ABFL2dll
      dimension z(ncmax),ybub(ncmax),xdew(ncmax),x(ncmax),y(ncmax)
      call ABFL2 (a,b,z,kq,ksat,ab,
     &            tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &            t,p,Dl,Dv,x,y,q,ierr,herr)
      end
c ======================================================================
      subroutine DBFL2dll (d,b,z,kq,ab,t,p,Dl,Dv,x,y,q,ierr,herr)
      include 'commons.for'
      character*2 ab
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DBFL2dll"::DBFL2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DBFL2dll
c     dll_export DBFL2dll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call DBFL2 (d,b,z,kq,ab,t,p,Dl,Dv,x,y,q,ierr,herr)
      end
c ======================================================================
      subroutine DQFL2dll (d,q,z,kq,t,p,Dl,Dv,x,y,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DQFL2dll"::DQFL2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DQFL2dll
c     dll_export DQFL2dll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call DQFL2 (d,q,z,kq,t,p,Dl,Dv,x,y,ierr,herr)
      end
c ======================================================================
      subroutine TQFLSHdll (t,q,z,kq,p,D,Dl,Dv,x,y,e,h,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TQFLSHdll"::TQFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TQFLSHdll
c     dll_export TQFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call TQFLSH (t,q,z,kq,p,D,Dl,Dv,x,y,e,h,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine PQFLSHdll (p,q,z,kq,t,D,Dl,Dv,x,y,e,h,s,cv,cp,w,i,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PQFLSHdll"::PQFLSHdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PQFLSHdll
c     dll_export PQFLSHdll
      dimension z(ncmax),x(ncmax),y(ncmax)
      call PQFLSH (p,q,z,kq,t,D,Dl,Dv,x,y,e,h,s,cv,cp,w,i,herr)
      end
c ======================================================================
      subroutine CSTARdll (t,p,v,x,cs,ts,Ds,ps,ws,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CSTARdll"::CSTARdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CSTARdll
c     dll_export CSTARdll
      dimension x(ncmax)
      call CSTAR (t,p,v,x,cs,ts,Ds,ps,ws,ierr,herr)
      end
c ======================================================================
      subroutine CCRITdll (t,p,v,x,cs,ts,Ds,ps,ws,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CCRITdll"::CCRITdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CCRITdll
c     dll_export CCRITdll
      dimension x(ncmax)
      call CSTAR (t,p,v,x,cs,ts,Ds,ps,ws,ierr,herr)
      end
c ======================================================================
      subroutine FPVdll (t,rho,p,x,f)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "FPVdll"::FPVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::FPVdll
c     dll_export FPVdll
      dimension x(ncmax)
      call FPV (t,rho,p,x,f)
      end
c ======================================================================
      subroutine CP0dll (t,x,cp)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "CP0dll"::CP0dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::CP0dll
c     dll_export CP0dll
      dimension x(ncmax)
      cp=CP0(t,x)
      end
c ======================================================================
      subroutine TRNPRPdll (t,rho,x,eta,tcx,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TRNPRPdll"::TRNPRPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TRNPRPdll
c     dll_export TRNPRPdll
      dimension x(ncmax)
      call TRNPRP (t,rho,x,eta,tcx,ierr,herr)
      if (tcx.gt.1.d50) tcx=0             !Avoid NaN (not a number)
      if (eta.gt.1.d50) eta=0             !Avoid NaN (not a number)
      end
c ======================================================================
      subroutine INFOdll (icomp,wmm,ttrp,tnbpt,tc,pc,Dc,Zc,acf,dip,Rgas)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "INFOdll"::INFOdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::INFOdll
c     dll_export INFOdll
      call INFO (icomp,wmm,ttrp,tnbpt,tc,pc,Dc,Zc,acf,dip,Rgas)
      end
c ======================================================================
      subroutine NAMEdll (icomp,hnam,hn80,hcasn)
      include 'commons.for'
      character*12 hnam,hcasn
      character*80 hn80
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "NAMEdll"::NAMEdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::NAMEdll
c     dll_export NAMEdll
      call NAME (icomp,hnam,hn80,hcasn)
      end
c ======================================================================
      subroutine XMASSdll (xmol,xkg,wmix)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "XMASSdll"::XMASSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::XMASSdll
c     dll_export XMASSdll
      dimension xmol(ncmax),xkg(ncmax)
      call XMASS (xmol,xkg,wmix)
      end
c ======================================================================
      subroutine XMOLEdll (xkg,xmol,wmix)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "XMOLEdll"::XMOLEdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::XMOLEdll
c     dll_export XMOLEdll
      dimension xmol(ncmax),xkg(ncmax)
      call XMOLE (xkg,xmol,wmix)
      end
c ======================================================================
      subroutine LIMITXdll (htyp,t,D,p,x,tmin,tmax,Dmax,pmax,ierr,herr)
      include 'commons.for'
      character*3 htyp
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "LIMITXdll"::LIMITXdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::LIMITXdll
c     dll_export LIMITXdll
      dimension x(ncmax)
      call LIMITX (htyp,t,D,p,x,tmin,tmax,Dmax,pmax,ierr,herr)
      end
c ======================================================================
      subroutine LIMITKdll (htyp,icomp,t,D,p,tmin,tmax,Dmax,pmax,i,herr)
      include 'commons.for'
      character*3 htyp
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "LIMITKdll"::LIMITKdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::LIMITKdll
c     dll_export LIMITKdll
      call LIMITK (htyp,icomp,t,D,p,tmin,tmax,Dmax,pmax,i,herr)
      end
c ======================================================================
      subroutine LIMITSdll (htyp,x,tmin,tmax,Dmax,pmax)
      include 'commons.for'
      character*3 htyp
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "LIMITSdll"::LIMITSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::LIMITSdll
c     dll_export LIMITSdll
      dimension x(ncmax)
      call LIMITS (htyp,x,tmin,tmax,Dmax,pmax)
      end
c ======================================================================
      subroutine QMASSdll (qmol,xl,xv,qkg,xlkg,xvkg,wliq,wvap,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "QMASSdll"::QMASSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::QMASSdll
c     dll_export QMASSdll
      dimension xl(ncmax),xv(ncmax),xlkg(ncmax),xvkg(ncmax)
      call QMASS (qmol,xl,xv,qkg,xlkg,xvkg,wliq,wvap,ierr,herr)
      end
c ======================================================================
      subroutine QMOLEdll (qkg,xlkg,xvkg,qmol,xl,xv,wliq,wvap,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "QMOLEdll"::QMOLEdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::QMOLEdll
c     dll_export QMOLEdll
      dimension xl(ncmax),xv(ncmax),xlkg(ncmax),xvkg(ncmax)
      call QMOLE (qkg,xlkg,xvkg,qmol,xl,xv,wliq,wvap,ierr,herr)
      end
c ======================================================================
      subroutine WMOLdll (x, wmm)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "WMOLdll"::WMOLdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::WMOLdll
c     dll_export WMOLdll
      dimension x(ncmax)
      wmm=WMOL(x)
      end
c ======================================================================
      subroutine DIELECdll (t,rho,x,de)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DIELECdll"::DIELECdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DIELECdll
c     dll_export DIELECdll
      dimension x(ncmax)
      call DIELEC (t,rho,x,de)
      end
c ======================================================================
      subroutine SURFTdll (t,rho,x,sigma,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SURFTdll"::SURFTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SURFTdll
c     dll_export SURFTdll
      dimension x(ncmax)
      call SURFT (t,rho,x,sigma,ierr,herr)
      end
c ======================================================================
      subroutine EXCESSdll (t,p,x,kph,rho,vE,eE,hE,sE,aE,gE,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "EXCESSdll"::EXCESSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::EXCESSdll
c     dll_export EXCESSdll
      dimension x(ncmax)
      call EXCESS (t,p,x,kph,rho,vE,eE,hE,sE,aE,gE,ierr,herr)
      end
c ======================================================================
      subroutine SURTENdll (t,rhol,rhov,xl,xv,sigma,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SURTENdll"::SURTENdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SURTENdll
c     dll_export SURTENdll
      dimension xl(ncmax),xv(ncmax)
      call SURTEN (t,rhol,rhov,xl,xv,sigma,ierr,herr)
      end
c ======================================================================
      subroutine PDFL1dll (p,rho,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PDFL1dll"::PDFL1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PDFL1dll
c     dll_export PDFL1dll
      dimension x(ncmax)
      call PDFL1 (p,rho,x,t,ierr,herr)
      end
c ======================================================================
      subroutine PHFL1dll (p,h,x,kph,t,D,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PHFL1dll"::PHFL1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PHFL1dll
c     dll_export PHFL1dll
      dimension x(ncmax)
      call PHFL1 (p,h,x,kph,t,D,ierr,herr)
      end
c ======================================================================
      subroutine PSFL1dll (p,s,x,kph,t,D,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PSFL1dll"::PSFL1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PSFL1dll
c     dll_export PSFL1dll
      dimension x(ncmax)
      call PSFL1 (p,s,x,kph,t,D,ierr,herr)
      end
c ======================================================================
      subroutine SETKTVdll (icomp,jcomp,hmodij,fij,hfmix,ierr,herr)
      include 'commons.for'
      character*3 hmodij
      character*255 hfmix,herr
      dimension fij(nmxpar)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETKTVdll"::SETKTVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETKTVdll
c     dll_export SETKTVdll
      call SETKTV (icomp,jcomp,hmodij,fij,hfmix,ierr,herr)
      end
c ======================================================================
      subroutine GETKTVdll (icomp,jcomp,hmodij,fij,hfmix,hfij2,hbinp,
     &                      hmxrul)
      include 'commons.for'
      character*3 hmodij
      character*8 hfij(nmxpar)
      character*255 hfmix,hmxrul,hbinp,hfij2
      dimension fij(nmxpar)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "GETKTVdll"::GETKTVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::GETKTVdll
c     dll_export GETKTVdll
      call GETKTV (icomp,jcomp,hmodij,fij,hfmix,hfij,hbinp,hmxrul)
      hfij2=hfij(1)//hfij(2)//hfij(3)//hfij(4)//hfij(5)//hfij(6)
      end
c ======================================================================
      subroutine GETFIJdll (hmodij,fij,hfij2,hmxrul)
      include 'commons.for'
      character*3 hmodij
      character*8 hfij(nmxpar)
      character*255 hmxrul,hfij2
      dimension fij(nmxpar)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "GETFIJdll"::GETFIJdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::GETFIJdll
c     dll_export GETFIJdll
      call GETFIJ (hmodij,fij,hfij,hmxrul)
      hfij2=hfij(1)//hfij(2)//hfij(3)//hfij(4)//hfij(5)//hfij(6)
      end
c ======================================================================
      subroutine MELTTdll (t,x,p,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "MELTTdll"::MELTTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::MELTTdll
c     dll_export MELTTdll
      dimension x(ncmax)
      call MELTT (t,x,p,ierr,herr)
      end
c ======================================================================
      subroutine MLTH2Odll (t,p1,p2)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "MLTH2Odll"::MLTH2Odll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::MLTH2Odll
c     dll_export MLTH2Odll
      call MLTH2O (t,p1,p2)
      end
c ======================================================================
      subroutine MELTPdll (p,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "MELTPdll"::MELTPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::MELTPdll
c     dll_export MELTPdll
      dimension x(ncmax)
      call MELTP (p,x,t,ierr,herr)
      end
c ======================================================================
      subroutine SUBLTdll (t,x,p,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SUBLTdll"::SUBLTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SUBLTdll
c     dll_export SUBLTdll
      dimension x(ncmax)
      call SUBLT (t,x,p,ierr,herr)
      end
c ======================================================================
      subroutine SUBLPdll (p,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SUBLPdll"::SUBLPdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SUBLPdll
c     dll_export SUBLPdll
      dimension x(ncmax)
      call SUBLP (p,x,t,ierr,herr)
      end
c ======================================================================
      subroutine PREOSdll (i)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PREOSdll"::PREOSdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PREOSdll
c     dll_export PREOSdll
      call PREOS (i)
      end
c ======================================================================
      subroutine SETAGAdll (ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SETAGAdll"::SETAGAdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SETAGAdll
c     dll_export SETAGAdll
      call SETAGA (ierr,herr)
      end
c ======================================================================
      subroutine GERG04dll (ncc,iflag,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "GERG04dll"::GERG04dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::GERG04dll
c     dll_export GERG04dll
      call GERG04 (ncc,iflag,ierr,herr)
      end
c ======================================================================
      subroutine REDXdll (x,t,rho)
      include 'commons.for'
      dimension x(ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "REDXdll"::REDXdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::REDXdll
c     dll_export REDXdll
      call REDX (x,t,rho)
      end
c ======================================================================
      subroutine DOTFILLdll (x,ptest,filrat,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DOTFILLdll"::DOTFILLdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DOTFILLdll
c     dll_export DOTFILLdll
      dimension x(ncmax)
      call DOTFILL (x,ptest,filrat,ierr,herr)
      end
c ======================================================================
      subroutine TPFL2dll (t,p,z,Dl,Dv,x,y,q,ierr,herr)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TPFL2dll"::TPFL2dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TPFL2dll
c     dll_export TPFL2dll
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax)
      call TPFL2 (t,p,z,Dl,Dv,x,y,q,ierr,herr)
      end
c ======================================================================
      subroutine DSFL1dll (rho,s,x,t,ierr,herr)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "DSFL1dll"::DSFL1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::DSFL1dll
c     dll_export DSFL1dll
      character*255 herr
      dimension x(ncmax)
      call DSFL1 (rho,s,x,t,ierr,herr)
      end
c ======================================================================
      subroutine LIQSPNDLdll (t,x,rhol,ierr,herr)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "LIQSPNDLdll"::LIQSPNDLdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::LIQSPNDLdll
c     dll_export LIQSPNDLdll
      character*255 herr
      dimension x(ncmax)
      call LIQSPNDL (t,x,rhol,ierr,herr)
      end
c ======================================================================
      subroutine VAPSPNDLdll (t,x,rhov,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "VAPSPNDLdll"::VAPSPNDLdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::VAPSPNDLdll
c     dll_export VAPSPNDLdll
      dimension x(ncmax)
      call VAPSPNDL (t,x,rhov,ierr,herr)
      end
c ======================================================================
      subroutine OMEGAdll (icomp,t,epsk,omg)
      include 'commons.for'
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "OMEGAdll"::OMEGAdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::OMEGAdll
c     dll_export OMEGAdll
      omg=OMEGA (icomp,t,epsk,'CI1')   !Assume model CI1
      end
c ======================================================================
      subroutine ETAK0dll (icomp,t,eta0,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ETAK0dll"::ETAK0dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ETAK0dll
c     dll_export ETAK0dll
      call ETAK0 (icomp,t,eta0,ierr,herr)
      end
c ======================================================================
      subroutine ETAK1dll (icomp,t,eta1,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ETAK1dll"::ETAK1dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ETAK1dll
c     dll_export ETAK1dll
      call ETAK1 (icomp,t,eta1,ierr,herr)
      end
c ======================================================================
      subroutine ETAKRdll (icomp,t,rho,etar,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ETAKRdll"::ETAKRdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ETAKRdll
c     dll_export ETAKRdll
      call ETAKR (icomp,t,rho,etar,ierr,herr)
      end
c ======================================================================
      subroutine ETAKBdll (icomp,t,rho,etab,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "ETAKBdll"::ETAKBdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::ETAKBdll
c     dll_export ETAKBdll
      call ETAKB (icomp,t,rho,etab,ierr,herr)
      end
c ======================================================================
      subroutine TCXK0dll (icomp,t,tcx0,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TCXK0dll"::TCXK0dll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TCXK0dll
c     dll_export TCXK0dll
      call TCXK0 (icomp,t,tcx0,ierr,herr)
      end
c ======================================================================
      subroutine TCXKBdll (icomp,t,rho,tcxb,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TCXKBdll"::TCXKBdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TCXKBdll
c     dll_export TCXKBdll
      call TCXKB (icomp,t,rho,tcxb,ierr,herr)
      end
c ======================================================================
      subroutine TCXKCdll (icomp,t,rho,tcxc,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "TCXKCdll"::TCXKCdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::TCXKCdll
c     dll_export TCXKCdll
      call TCXKC (icomp,t,rho,tcxc,ierr,herr)
      end
c ======================================================================
      subroutine SPLNVALdll (isp,iderv,a,f,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SPLNVALdll"::SPLNVALdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SPLNVALdll
c     dll_export SPLNVALdll
      call SPLNVAL (isp,iderv,a,f,ierr,herr)
      end
c ======================================================================
      subroutine SPLNROOTdll (isp,iderv,f,a,ierr,herr)
      include 'commons.for'
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "SPLNROOTdll"::SPLNROOTdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::SPLNROOTdll
c     dll_export SPLNROOTdll
      call SPLNROOT (isp,iderv,f,a,ierr,herr)
      end
c ======================================================================
      subroutine IDCRVdll (d,x,t,ierr,herr)
      include 'commons.for'
      dimension x(ncmax)
      character*255 herr
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "IDCRVdll"::IDCRVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::IDCRVdll
c     dll_export IDCRVdll
      call IDCRV (d,x,t,ierr,herr)
      end
c ======================================================================
      subroutine BLCRVdll (d,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "BLCRVdll"::BLCRVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::BLCRVdll
c     dll_export BLCRVdll
      call BLCRV (d,x,t,ierr,herr)
      end
c ======================================================================
      subroutine JTCRVdll (d,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "JTCRVdll"::JTCRVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::JTCRVdll
c     dll_export JTCRVdll
      call JTCRV (d,x,t,ierr,herr)
      end
c ======================================================================
      subroutine JICRVdll (d,x,t,ierr,herr)
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "JICRVdll"::JICRVdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::JICRVdll
c     dll_export JICRVdll
      call JICRV (d,x,t,ierr,herr)
      end
c ======================================================================
      subroutine PHIXdll (itau,idel,tau,del,x,PHIXX)
      include 'commons.for'
      dimension x(ncmax)
cDEC$ ATTRIBUTES DLLEXPORT, Decorate, Alias: "PHIXdll"::PHIXdll
cDEC$ ATTRIBUTES STDCALL, REFERENCE::PHIXdll
      PHIXX=PHIX (itau,idel,tau,del,x)
      end
c ======================================================================
