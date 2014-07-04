c  begin file trns_VIS.f
c
c  This file contains the core routines for viscosity
c
c  contained here are:
c     subroutine SETVS0 (nread,icomp,hcasno,ierr,herr)
c     function ETA0HC (icomp,t,rho,ierr,herr)
c     subroutine SETVS1 (nread,icomp,hcasno,ierr,herr)
c     function ETA1DG (icomp,t)
c     function ETA1B2 (icomp,t)
c     function ETA1RS (icomp,t,rho)
c     subroutine SETVS2 (nread,icomp,hcasno,ierr,herr)
c     function ETA2DG (icomp,t)
c     function ETA2RS (icomp,t,rho)
c     subroutine SETVS3 (nread,icomp,hcasno,ierr,herr)
c     function ETA3DG (icomp,t)
c     function ETA3RS (icomp,t,rho)
c     subroutine SETVS4 (nread,icomp,hcasno,ierr,herr)
c     function ETA4DG (icomp,t)
c     function ETA4RS (icomp,t,rho)
c     subroutine SETVS5 (nread,icomp,hcasno,ierr,herr)
c     function ETA5RS (icomp,t,rho)
c     subroutine SETVS6 (nread,icomp,hcasno,ierr,herr)
c     function ETAH2 (icomp,t,d)
c     function ETAH2mct (icomp,t,d)
c     function DELV (icomp,D1,T1,D2,T2)
c     function EXVDIL (DD,T)
c     function DILV (icomp,T)
c     function EXCESV (icomp,DD,T)
c     function ETAHE (icomp,t,rho)
c     function ETAETY (icomp,t,rho)
c     function ETANEO (icomp,t,rho)
c     function ETAR23 (icomp,t,rho)
c     function ETAH2O (icomp,t,rho)
c     function XI_FUN (t,rho,icomp)
c     function ETA_C2 (xi)
c     function ETAMEO (icomp,t,rho)
c     function ETASF6 (icomp,t,rho)
c     function ETAD2 (icomp,t,d)
c
c =====================================================================
c =====================================================================
c
      subroutine SETVS0 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #0; the model
c  which points to all the hardcoded equations.
c
c  inputs:
c    nread--file to read data from
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-29-00 EWL, original version
c  09-00-00 EWL, change hmdci to hmdeta and hmdtcx to avoid overlapping tcx.
c  11-20-07 MLH, removed unused commons and declarations
c  01-04-08 MLH, added additional slots to WNTETA
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
c  read data from file (should have been opened by SETUP)
c     write (*,*) ' SETVS0--read component',icomp,' from unit',nread
      write (herr,'(a12)') hcasno  !Use hcasno to avoid warning message
      read (nread,*) tmeta(icomp)              !lower temperature limit
      read (nread,*) txeta(icomp)              !upper temperature limit
      read (nread,*) pxeta(icomp)              !upper pressure limit
      read (nread,*) Dxeta(icomp)              !upper density limit
c
c  read in pointer to the hardcoded model
      read (nread,2003) hetahc(icomp)
c     write (*,*) ' SETVS0--will use model ',hetahc(icomp)
      read (nread,*) ndgeta(icomp),nB2eta(icomp),ndel0(icomp),
     &               npoly(icomp),nnumeta(icomp),ndeneta(icomp),
     &               nexpn(icomp),nexpd(icomp)
c     write (*,*) ' SETVS0--about to read ',ndgeta(icomp),' dilute terms'
      jterm=0                                 !term counter
      if (ndgeta(icomp).ge.1) then
        read (nread,2003) hmdeta(icomp)       !pointer to omega model
        read (nread,*) sigmav(icomp)           !L-J sigma
        read (nread,*) epskv(icomp)            !L-J epsilon/kappa
        read (nread,*) trddge(icomp),etadge(icomp)  !reducing par
        do j=1,ndgeta(icomp)                 !read dilute-gas terms
          jterm=jterm+1
          read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
        enddo
      end if
      nrsum=nB2eta(icomp)+ndel0(icomp)+npoly(icomp)+nnumeta(icomp)+
     &      ndeneta(icomp)+nexpn(icomp)+nexpd(icomp)
      if (nrsum.ge.1) then
c  read in reducing parameters
        read (nread,*) tredeta(icomp),Dredeta(icomp),etared(icomp)
        do j=1,nrsum
          jterm=jterm+1
          read (nread,*) (ceta(icomp,jterm,k),k=1,4),ieta(icomp,jterm)
        enddo
      end if
c
c  read in pointer to critical enhancement model
      read (nread,2003) hetacr(icomp)
c       write (*,*) ' SETVS1--will use critical model ',hetacr(icomp)
      ierr=0
      herr=' '
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETVS0
c
c ======================================================================
c
      function ETA0HC (icomp,t,rho,ierr,herr)
c
c  model for the hardcoded viscosity models
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c        d--molar density [mol/L]
c  output (as function value):
c   eta0hc--viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-29-00 EWL, original version
c  11-20-07 MLH, initialized herr
c  12-22-09 MLH, added D2, T2, HE3 models
c  04-06-11 MLH, added Altunin SF6 vis model
c  11-16-12 MLH, allow two VS0 models for viscosity
c  02-28-13 MLH, added d2
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      herr=''
      ETA0HC=0.0d0
      if (hetahc(icomp).eq.'ETY') THEN
        ETA0HC=ETAETY(icomp,t,rho)
      elseif (hetahc(icomp).eq.'NEO') THEN
        ETA0HC=ETANEO(icomp,t,rho)
      elseif (hetahc(icomp).eq.'R23') THEN
        ETA0HC=ETAR23(icomp,t,rho)
      elseif (hetahc(icomp).eq.'H2O') THEN
        ETA0HC=ETAH2O(icomp,t,rho)
      elseif (hetahc(icomp).eq.'HE') THEN
        ETA0HC=ETAHE(icomp,t,rho)
      elseif (hetahc(icomp).eq.'H2') THEN
        ETA0HC=ETAH2mct(icomp,t,rho)  ! old Mccarty model
      ELSEIF (hetahc(icomp).eq.'H2A') THEN
        ETA0HC=ETAH2(icomp,t,rho)     ! alternate h2 model of Muzny
      elseif (hetahc(icomp).eq.'D2') THEN
        ETA0HC=ETAD2(icomp,t,rho)     ! scaled h2 model of Muzny for D2
      elseif (hetahc(icomp).eq.'T2') THEN   !scale to h2 with theoretical limit for gas
        ETA0HC=ETAH2(icomp,t,rho)*SQRT(3.0d0)
      elseif (hetahc(icomp).eq.'HE3') THEN   !scale to he4 with theoretical limit for gas
        ETA0HC=ETAHE(icomp,t,rho)*SQRT(3.0d0/4.0d0)
      elseif (hetahc(icomp).eq.'MEO') THEN
        ETA0HC=ETAMEO(icomp,t,rho)
      elseif (hetahc(icomp).eq.'SF6') THEN
        ETA0HC=ETASF6(icomp,t,rho)
      else
        ierr=49
        herr='[ETA0HC error 49] unknown viscosity model specified'
     &        //hnull
        call ERRMSG (ierr,herr)
      endif
      RETURN
      end                                               !function ETA0HC
c
c ======================================================================
c
      subroutine SETVS1 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #1; this, the "composite model,"
c  is written in a general form with terms designed to include several
c  recent correlations including those of Fenghour (1995) for ammonia,
c  Krauss (1996) for R152a, and Laesecke (1997) for R134a.
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                      101 = error--block data option not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  01-16-97  MM, original version
c  02-26-97  MM, read pointer for critical enhancement model (future use)
c  08-19-97  MM, change error number for nread<=0; input hcasno is not array
c  09-27-01 MLH, add Laesecke's alternative formulation for del10 term (use neg# terms to flag it)
c  11-20-07 MLH, removed unused commons and declarations, add abs(ndel0)
c  01-04-08 MLH, added additional slots to WNTETA
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETVS1 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETVS1--read component',icomp,' from unit',nread
        read (nread,*) tmeta(icomp)              !lower temperature limit
        read (nread,*) txeta(icomp)              !upper temperature limit
        read (nread,*) pxeta(icomp)              !upper pressure limit
        read (nread,*) Dxeta(icomp)              !upper density limit
        jterm=0                                 !term counter
        ndg2(icomp)=0                           !numerator multiplicative terms
        ndg3(icomp)=0                           !denominator multiplicative terms
        ndg4(icomp)=0
        ndg5(icomp)=0
        ndg6(icomp)=0
        read (nread,*) ndgeta(icomp)               !# dilute-gas terms
c       write (*,*) ' SETVS1--about to read ',ndgeta(icomp),' dilute terms'
        if (ndgeta(icomp).ge.1) then
          read (nread,2003) hmdeta(icomp)       !pointer to omega model
          read (nread,*) sigmav(icomp)           !L-J sigma
          read (nread,*) epskv(icomp)            !L-J epsilon/kappa
          read (nread,*) trddge(icomp),etadge(icomp)  !reducing par
          do j=1,ndgeta(icomp)                 !read dilute-gas terms
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
        read (nread,*) nB2eta(icomp)               !# visc virial terms
c       write (*,*) ' SETVS1--about to read ',nB2eta(icomp),' virial terms'
        if (nB2eta(icomp).ge.1) then
          read (nread,*) tredB2(icomp),etarB2(icomp)  !reducing par
          do j=1,nB2eta(icomp)        !read viscosity virial terms
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
c
c  read the number of terms of the various parts of the residual model
c  these are in the order:
c    close-packed density function;
c    simple polynomials in T, rho, rho_0, exp(rho/rhoc);
c    numerator of rational polynomial; denominator of rational polynomial;
c    numerator of exponential term; denominator of exponential term;
c  the coefficients themselves are given in the order:
c    constant multiplier; temperature exponent (all terms);
c    density exponent; close-packed density exponent (all except del0 terms)
c    power of density inside exponential (0 indicates no exponential)
c
        read (nread,*) ndel0(icomp),npoly(icomp),nnumeta(icomp),
     &                 ndeneta(icomp)
     &                ,nexpn(icomp),nexpd(icomp)
        nrsum=ABS(ndel0(icomp))+npoly(icomp)+nnumeta(icomp)
     &       +ndeneta(icomp)
     &       +nexpn(icomp)+nexpd(icomp)
c       write (*,*) ' SETVS1--about to read ',nrsum,' residual terms'
        if (nrsum.ge.1) then
c  read in reducing parameters
          read (nread,*) tredeta(icomp),Dredeta(icomp),etared(icomp)
          if (ndel0(icomp).ge.1) then
            do j=1,ndel0(icomp)     !close-packed density term
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          elseif(ndel0(icomp).lt.0) then
c  close-packed density term; alternative form
            do j=1,ABS(ndel0(icomp))
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          end if
c
          if (npoly(icomp).ge.1) then
            do j=1,npoly(icomp)     !simple polynomial terms
              jterm=jterm+1
              read(nread,*)(ceta(icomp,jterm,k),k=1,4),ieta(icomp,jterm)
            enddo
          end if
          if (nnumeta(icomp).ge.1) then
            do j=1,nnumeta(icomp)      !numerator of rational polynomial
              jterm=jterm+1
              read(nread,*)(ceta(icomp,jterm,k),k=1,4),ieta(icomp,jterm)
            enddo
          end if
          if (ndeneta(icomp).ge.1) then
            do j=1,ndeneta(icomp)      !denominator of rational poly
              jterm=jterm+1
              read(nread,*)(ceta(icomp,jterm,k),k=1,4),ieta(icomp,jterm)
            enddo
          end if
          if (nexpn(icomp).ge.1) then
            do j=1,nexpn(icomp)     !numerator of exponential term
              jterm=jterm+1
              read(nread,*)(ceta(icomp,jterm,k),k=1,3),ieta(icomp,jterm)
            enddo
          end if
          if (nexpd(icomp).ge.1) then
            do j=1,nexpd(icomp)     !denominator of exponential term
              jterm=jterm+1
              read(nread,*)(ceta(icomp,jterm,k),k=1,3),ieta(icomp,jterm)
            enddo
          end if
        end if
c
c  read in pointer to critical enhancement model
        read (nread,2003) hetacr(icomp)
c       write (*,*) ' SETVS1--will use critical model ',hetacr(icomp)
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETVS1
c
c ======================================================================
c
      function ETA1DG (icomp,t)
c
c  dilute-gas contribution to the viscosity by the composite model (VS1)
c  and also the friction theory model (VS4)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta1dg--the dilute-gas part of the viscosity [uPa-s]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  01-17-97  MM, original version
c  03-28-97  MM, move calc of tau inside "if" (divide by zero if ndgeta(i)=0)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c   1-20-00 EWL, add check for absurd t, coming from the ECS model
c  11-20-07 MLH, removed unused commons and declarations
c  01-04-08 MLH, added additional slots to WNTETA
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      nterm=0                                    !term counter
      eta1dg=0.0d0
c
c  sum the dilute-gas terms
      eta1dg=0.0d0
      if (ndgeta(i).ge.1 .and. t.lt.1.d8) then
        tau=t/trddge(i)
c  first term is always the Chapman-Enskog term
        eta1dg=ceta(i,1,1)*SQRT(tau)/(sigmav(i)**2
     &        *OMEGA(i,t,epskv(i),hmdeta(i)))
        if (ndgeta(i).ge.2) then
c  possibility for additional, empirical terms
          do j=nterm+1,nterm+ndgeta(i)
            eta1dg=eta1dg+ceta(i,j,1)*tau**ceta(i,j,2)
          enddo
        end if
      end if
c
c  multiply by reducing parameter for viscosity (to convert units, etc.)
      eta1dg=eta1dg*etadge(i)
c      write (*,*) ' ETA1DG--d.g. visc:   ',eta1dg
      RETURN
      end                                               !function ETA1DG
c
c ======================================================================
c
      function ETA1B2 (icomp,t)
c
c  second viscosity "virial coefficient" by the composite model (VS1)
c  This model implements the initial-density dependence of the
c  Rainwater-Friend theory.  It returns a viscosity virial coefficient
c  which must be multiplied by the dilute-gas viscosity and the density
c  to yield the initial-density dependence of viscosity.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta1B2--the second viscosity virial coefficient [L/(mol-uPa-s)]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  01-17-97  MM, original version
c  03-28-97  MM, move calc of tau inside "if" (divide by zero if nB2eta(i)=0)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-20-07 MLH, removed unused commons and declarations
c  01-30-08 MLH, added additional slots to WNTETA and modify term counter
c
      include 'commons.for'
      include 'comtrn.for'
c
c     write (*,*) ' ETA1B2--tred,etared:  ',tredB2(icomp),etarB2(icomp)
      i=icomp
      nterm=ndgeta(i)+ndg2(i)+ndg3(i)           !term counter

c
c  sum the terms comprising the viscosity virial
      eta1B2=0.0d0
      if (nB2eta(i).ge.1) then
        tau=t/tredB2(i)
        do j=nterm+1,nterm+nB2eta(i)
          eta1B2=eta1B2+ceta(i,j,1)*tau**ceta(i,j,2)
c       write (*,*) ' ETA1B2--j,c1,ti:  ',j,ceta(i,j,1),ceta(i,j,2)
        enddo
c  multiply by reducing parameter (to convert units, etc.)
        eta1B2=eta1B2*etarB2(i)
      end if
c     write (*,*) ' ETA1B2--etaB2:  ',eta1B2
c
      RETURN
      end                                               !function ETA1B2
c
c ======================================================================
c
      function ETA1RS (icomp,t,rho)
c
c  residual contribution to the viscosity by the composite model (VS1)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   eta1rs--the background part of the viscosity [uPa-s]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  01-16-97  MM, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c   2-16-99 EWL, return if rho=0
c  09-27-01 MLH, allow Laesecke's alternative formulation for del0 term
c  11-20-07 MLH, remove unused commons and declarations, add abs(ndel0)
c  01-04-08 MLH, added additional slots to WNTETA
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      eta1rs=0.0d0
      if (rho.lt.1.0d-10) RETURN
c
      nsum=ABS(ndel0(i))+npoly(i)+nnumeta(i)+ndeneta(i)+nexpn(i)
     &    +nexpd(i)
      expdel=1.0d0    !initialize only
      tau=1.0d0
      del=1.0d0
      if (nsum.ge.1) then
c  compute tau,del only if terms present (otherwise reducing par not read in)
        tau=t/tredeta(i)
        del=rho/Dredeta(i)
c  define the density to be used in exponential multipliers
c  if the reducing density is 1.0, must divide by the critical density
        if (abs(Dredeta(i)-1.0d0).lt.0.001d0) then
          expdel=rho/Dcrit(i)
        else
          expdel=del
        end if
      end if
c
c  compute the various parts of the residual model
c  these are taken in the order:
c    close-packed density function;
c    simple polynomials in T, rho, rho_0, exp(rho/rhoc);
c    numerator of rational polynomial; denominator of rational polynomial;
c    numerator of exponential term; denominator of exponential term;
c  the coefficients themselves are given in the order:
c    constant multiplier; temperature exponent (all terms);
c    density exponent; close-packed density exponent (all except del0 terms)
c    power of density inside exponential (0 indicates no exponential)
c
      nterm=ndgeta(i)+nB2eta(i)                        !term counter
c  compute the close-packed density at given temperature, if applicable
      if (ndel0(i).ge.1) then
        del0=0.0d0
        do j=nterm+1,nterm+ndel0(i)
          del0=del0+ceta(i,j,1)*tau**ceta(i,j,2)
c       write (*,*) ' ETA1RS--j,close-packed term:  ',j,ceta(i,j,1)
        enddo
        nterm=nterm+ndel0(i)
      elseif (ndel0(i).lt.0) then   !alternative formulation for del10
        del0=1.0d0
        do j=nterm+2,nterm+ABS(ndel0(i))
          del0=del0+ceta(i,j,1)*tau**ceta(i,j,2)
        enddo
        del0=ceta(i,nterm+1,1)/del0
        nterm=nterm+ABS(ndel0(i))
      else
        del0=1.0d0
      end if
c     write (*,*) ' ETA1RS--icomp, # del0, del0:    ',i,ndel0(i),del0
c
c  sum the simple polynomial terms
      eta1rs=0.0d0
      if (npoly(i).ge.1) then
        do j=nterm+1,nterm+npoly(i)
          visci=ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
     &         *del0**ceta(i,j,4)
          if (ieta(i,j).ge.1) then
            visci=visci*exp(-expdel**ieta(i,j))
          end if
          eta1rs=eta1rs+visci
c       write (*,1220) j,ceta(i,j,1),eta1rs
c1220   format (1x,' ETA1RS--j,polynomial coeff,sum:',i3,2e14.6)
        enddo
c       write (*,*) ' ETA1RS--j,polynomial sum:      ',j,eta1rs
        nterm=nterm+npoly(i)
      end if
c
c  calculate the numerator of the rational polynomial
      if (nnumeta(i).ge.1) then
        xnum=0.0d0
        do j=nterm+1,nterm+nnumeta(i)
          xnum=xnum+ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
     &        *del0**ceta(i,j,4)
          if (ieta(i,j).ge.1) then
            xnum=xnum*exp(-expdel**ieta(i,j))
          end if
        enddo
c       write (*,*) ' ETA1RS--numerator term:       ',xnum
        nterm=nterm+nnumeta(i)
      else
        xnum=1.0d0
      end if
c
c  calculate the denominator of the rational polynomial
      if (ndeneta(i).ge.1) then
        xden=0.0d0
        do j=nterm+1,nterm+ndeneta(i)
          xden=xden+ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
     &        *del0**ceta(i,j,4)
          if (ieta(i,j).ge.1) then
            xden=xden*exp(-expdel**ieta(i,j))
          end if
        enddo
        nterm=nterm+ndeneta(i)
c       write (*,*) ' ETA1RS--denominator term:     ',xden
      else
        xden=1.0d0
      end if
c
c  combine the two parts of the rational polynomial, if applicable
      if (nnumeta(i).ge.1 .or. ndeneta(i).ge.1) then
c       write (*,*) ' ETA1RS--num,denom:  ',xnum,xden
        eta1rs=eta1rs+xnum/xden
      end if
c
c  calculate the numerator of the exponential term
      if (nexpn(i).ge.1) then
        xnum=0.0d0
        do j=nterm+1,nterm+nexpn(i)
          xnum=xnum+ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
     &        *del0**ceta(i,j,4)
        enddo
        nterm=nterm+nexpn(i)
      else
        xnum=1.0d0
      end if
c
c  calculate the denominator of the exponential term
      if (nexpd(i).ge.1) then
        xden=0.0d0
        do j=nterm+1,nterm+nexpd(i)
          xden=xden+ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
     &        *del0**ceta(i,j,4)
        enddo
        nterm=nterm+nexpd(i)
      else
        xden=1.0d0
      end if
c
c  combine the two parts of the exponential term, if applicable
      if (nexpn(i).ge.1 .or. nexpd(i).ge.1) then
        eta1rs=eta1rs+EXP(xnum/xden)
      end if
c
c  multiply by reducing parameter for viscosity (to convert units, etc.)
      eta1rs=eta1rs*etared(i)
c
      RETURN
      end                                               !function ETA1RS
c
c ======================================================================
c
      subroutine SETVS2 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #2; the hydrocarbon model of:
c  Younglove, B.A. and Ely, J.F. (1987). Thermophysical properties of
c  fluids. II. Methane, ethane, propane, isobutane and normal butane.
c  J. Phys. Chem. Ref. Data  16: 577-798.
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                      101 = error--block data option not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-21-97  MM, original version
c  02-26-97  MM, set pointer to critical enhancement to 'NUL' (not used)
c  08-19-97  MM, change error number for nread<=0; input hcasno is not array
c  07-01-98 EWL, read ceta(icomp,1,2) and ceta(icomp,e1,1) if version>6.01
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-20-07 MLH, removed unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETVS2 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETVS2--read component',icomp,' from unit',nread
        read (nread,*) tmeta(icomp)              !lower temperature limit
        read (nread,*) txeta(icomp)              !upper temperature limit
        read (nread,*) pxeta(icomp)              !upper pressure limit
        read (nread,*) Dxeta(icomp)              !upper density limit
        read (nread,2003) hmdeta(icomp)         !pointer to omega model
 2003   format (a3)
        read (nread,*) sigmav(icomp)             !L-J sigma
        read (nread,*) epskv(icomp)              !L-J epsilon/kappa
        read (nread,*) ceta(icomp,1,1)          !Chapman-Enskog term
        ceta(icomp,1,2)=0.50d0                  !Chapman-Enskog exponent
        if (verfl(icomp).ge.6.099d0) read (nread,*) ceta(icomp,1,2)
        do j=2,12                  !read residual viscosity terms
          read (nread,*) ceta(icomp,j,1)
        enddo
        ceta(icomp,13,1)=Dcrit(icomp)
        if (verfl(icomp).ge.6.099d0) read (nread,*) ceta(icomp,13,1)
        hetacr(icomp)='NUL'       !no critical enhancement in this model
        ierr=0
        herr=' '
      end if
c
      RETURN
      end                                             !subroutine SETVS2
c
c ======================================================================
c
      function ETA2DG (icomp,t)
c
c  dilute-gas contribution to the viscosity by the hydrocarbon model of:
c  Younglove, B.A. and Ely, J.F. (1987). Thermophysical properties of
c  fluids. II. Methane, ethane, propane, isobutane and normal butane.
c  J. Phys. Chem. Ref. Data  16: 577-798.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta2dg--the dilute-gas part of the viscosity [uPa-s]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-21-97  MM, original version
c  07-01-98 EWL, replace SQRT(tau) with tau**ceta(i,1,2)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-20-07 MLH, removed unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      tau=t
c  in this case, the dilute gas is simply the Chapman-Enskog term
      eta2dg=ceta(i,1,1)*tau**ceta(i,1,2)/
     &       (sigmav(i)**2*OMEGA(i,t,epskv(i),hmdeta(i)))
c     write (*,*) ' ETA2DG--dilute-gas viscosity:  ',eta2dg
c
      RETURN
      end                                               !function ETA2DG
c
c ======================================================================
c
      function ETA2RS (icomp,t,rho)
c
c  residual contribution to the viscosity by the hydrocarbon model of:
c  Younglove, B.A. and Ely, J.F. (1987). Thermophysical properties of
c  fluids. II. Methane, ethane, propane, isobutane and normal butane.
c  J. Phys. Chem. Ref. Data  16: 577-798.
c
c  Although this correlation has a separate initial density term, it is
c  not of the form required by ETAK1; thus the initial density term is
c  combined with the residual term.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   eta2rs--the background part of the viscosity [uPa-s]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-21-97  MM, original version
c  07-01-98 EWL, replace rhoc with ceta(icomp,13,1)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-20-07 MLH, removed unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
c  initial density term for viscosity
      eta1=rho*(ceta(icomp,2,1)+ceta(icomp,3,1)
     &    *(ceta(icomp,4,1)-LOG(t/ceta(icomp,5,1)))**2)           !Eq 21
c  now compute the residual viscosity (viscosity minus the dilute gas
c  and initial density terms)
      G=ceta(icomp,6,1)+ceta(icomp,7,1)/t                         !Eq 23
      H=SQRT(rho)*(rho-ceta(icomp,13,1))/ceta(icomp,13,1)         !Eq 25
      F=G+(ceta(icomp,8,1)+ceta(icomp,9,1)*t**(-1.5d0))*rho**0.1d0+
     &  (ceta(icomp,10,1)+ceta(icomp,11,1)/t+ceta(icomp,12,1)/(t*t))*H
      eta2=EXP(F)-EXP(G)                                          !Eq 22
      ETA2RS=eta1+eta2
c     write (*,*) ' ETA2RS--residual viscosity:    ',eta2rs
c
      RETURN
      end                                               !function ETA2RS
c
c ======================================================================
c
      subroutine SETVS3 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #3, minim subroutine SETTC1
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                       49 = error--model not implemented
c                      101 = error--block data option not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-16-99 EWL, mimic routines from SETTC1
c  11-20-07 MLH, removed unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
c  note: this model has no critical enhancement term
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETVS3 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETVS3--read component',icomp,' from unit',nread
        read (nread,*) tmeta(icomp)              !lower temperature limit
        read (nread,*) txeta(icomp)              !upper temperature limit
        read (nread,*) pxeta(icomp)              !upper pressure limit
        read (nread,*) Dxeta(icomp)              !upper density limit
        jterm=0                                 !term counter
c  read the number of terms in the numerator and denominator of the
c  dilute-gas function
        read (nread,*) ndgnm3(icomp),ndgdn3(icomp)
c       write (*,*) ' SETVS3--about to read ',ndgnm3(icomp),' +',
c    &              ndgdn3(icomp),' dilute terms'
        if (ndgnm3(icomp).ge.1) then
          read (nread,*) trddg3(icomp),etadg3(icomp)  !reducing par
          do j=1,ndgnm3(icomp)  !read dilute-gas terms (numerator)
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
        if (ndgdn3(icomp).ge.1) then
          do j=1,ndgdn3(icomp)  !read dilute-gas terms (denominator)
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
c
c  read the number of terms in the numerator and denominator of the
c  background model; the coefficients themselves are given in the order:
c    constant multiplier; temperature exponent; density exponent; spare
c
        read (nread,*) nbknm3(icomp),nbkdn3(icomp)
        nbksum=nbknm3(icomp)+nbkdn3(icomp)
c       write (*,*) ' SETVS3--about to read ',nbknm3(icomp),' +',
c    &              nbkdn3(icomp),' background terms'
        if (nbksum.ge.1) then
c  read in reducing parameters
          read (nread,*) trdbk3(icomp),Drdbk3(icomp),etabk3(icomp)
          if (nbknm3(icomp).ge.1) then
            do j=1,nbknm3(icomp)    !numerator of rational polynomial
              jterm=jterm+1
              read (nread,*) (ceta(icomp,jterm,k),k=1,4)
            enddo
          end if
          if (nbkdn3(icomp).ge.1) then
            do j=1,nbkdn3(icomp)    !denominator of rational poly
              jterm=jterm+1
             read (nread,*) (ceta(icomp,jterm,k),k=1,4)
            enddo
          end if
        end if
c
        ierr=0
        herr=' '
      end if
c
      RETURN
      end                                             !subroutine SETVS3
c
c ======================================================================
c
      function ETA3DG (icomp,t)
c
c  dilute-gas contribution to the viscosity by the composite model (VS3)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta3dg--the dilute-gas part of the viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-16-99 EWL, mimic routines from TCX1DG
c  11-20-07 MLH, removed unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      tau=1.0d0
      if ((ndgnm3(i)+ndgdn3(i)).ge.1) then
c  compute tau only if dilute-gas terms exist, otherwise treddg may
c  not be defined
        tau=t/trddg3(i)
c       write (*,*) 'ETA3DG--tred,tau:  ',trddg3(i),tau
      end if
c
      nterm=0                                    !term counter
c  sum the dilute-gas terms, first numerator then denominator
      eta3dg=0.0d0
      if (ndgnm3(i).ge.1) then
        do j=nterm+1,nterm+ndgnm3(i)
          eta3dg=eta3dg+ceta(i,j,1)*tau**ceta(i,j,2)
c         write (*,*) ' ETA3DG--j,eta_dg(j): ',j,eta3dg
        enddo
        nterm=nterm+ndgnm3(i)
      end if
      if (ndgdn3(i).ge.1) then
        denom=0.0d0
        do j=nterm+1,nterm+ndgdn3(i)
c       write (*,*) 'denom coeff:  ',ceta(i,j,1),ceta(i,j,2)
          denom=denom+ceta(i,j,1)*tau**ceta(i,j,2)
        enddo
c       write (*,*) ' ETA3DG--num,denom: ',eta3dg,denom
c  divide numerator by denominator
        eta3dg=eta3dg/denom
      end if
c
c  multiply by reducing parameter (to convert units, etc.)
      eta3dg=eta3dg*etadg3(i)
c
c     write (*,1000) icomp,tau,eta3dg
c1000 format (' ETA3DG--icomp,tau,dilute-gas tc:',i10,d14.6,14x,d14.6)
      RETURN
      end                                               !function ETA3DG
c
c ======================================================================
c
      function ETA3RS (icomp,t,rho)
c
c  background contribution to the viscosity by the composite model (VS3)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   eta3rs--the background part of the viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-16-99 EWL, mimic routines from TCX1BK
c  11-20-07 MLH, removed unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      tau=1.0d0
      del=1.0d0
      if ((nbknm3(i)+nbkdn3(i)).ge.1) then
c  compute tau only if residual terms exist, otherwise tredbk, Dredbk may
c  not be defined
        tau=t/trdbk3(i)
        del=rho/Drdbk3(i)
      end if
      nterm=ndgnm3(i)+ndgdn3(i)                        !term counter
c     write (*,*) ' ETA3RS--tau,del,coeff_1: ',tau,del,ceta(i,nterm+1,1)
c
c  sum the background terms, first numerator then denominator
      eta3rs=0.0d0
      if (nbknm3(i).ge.1) then
        do j=nterm+1,nterm+nbknm3(i)
          eta3rs=eta3rs+ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
        enddo
      end if
      if (nbkdn3(i).ge.1) then
        denom=0.0d0
        if (del.gt.0) then
          do j=nterm+nbknm3(i)+1,nterm+nbknm3(i)+nbkdn3(i)
            denom=denom+ceta(i,j,1)*tau**ceta(i,j,2)*del**ceta(i,j,3)
          enddo
        endif
c  divide numerator by denominator
        if (abs(denom).gt.1.d-20) eta3rs=eta3rs/denom
      end if
c
c  multiply by reducing parameter (to convert units, etc.)
      eta3rs=eta3rs*etabk3(i)
c
c     write (*,1000) icomp,tau,del,eta3rs
c1000 format (' ETA3RS--icomp,tau,del,background tc:  ',i4,3d14.6)
      RETURN
      end                                               !function ETA3RS
c
c ======================================================================
c
      subroutine SETVS4 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #4; this, the "friction theory model,"
c  S.E. Quinones-Cisneros and U.K. Deiters,
c  "Generalization of the Friction Theory for Viscosity Modeling,"
c  J. Phys. Chem. B 2006, 110,12820-12834.
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                      101 = error--block data option not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  12-26-06  MLH, original version
c  01-23-07  MLH, fix mixture bug
c  11-20-07  MLH, removed unused commons and declarations
c  01-04-08 MLH, added additional slots to WNTETA
c  04-28-08 MLH, increased array size in trnft to accommodate new h2 model
c  01-08-12 MLH, increased array size in trnft to accommodate new sf6 model
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETVS4 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETVS4--read component',icomp,' from unit',nread
        read (nread,*) tmeta(icomp)              !lower temperature limit
        read (nread,*) txeta(icomp)              !upper temperature limit
        read (nread,*) pxeta(icomp)              !upper pressure limit
        read (nread,*) Dxeta(icomp)              !upper density limit
        jterm=0
        ndg2(icomp)=0                           !numerator multiplicative terms
        ndg3(icomp)=0                           !denominator multiplicative terms
        ndg4(icomp)=0
        ndg5(icomp)=0
        ndg6(icomp)=0
        read (nread,*,err=10) ndgeta(icomp),ndg2(icomp),ndg3(icomp),
     &            ndg4(icomp),ndg5(icomp),ndg6(icomp)  !# dilute-gas terms
c       write (*,*) ' SETVS4--about to read ',ndgeta(icomp),' dilute terms'
  10    if (ndgeta(icomp).ge.1) then
          read (nread,2003) hmdeta(icomp)       !pointer to omega model
          read (nread,*) sigmav(icomp)           !L-J sigma
          read (nread,*) epskv(icomp)            !L-J epsilon/kappa
          read (nread,*) trddge(icomp),etadge(icomp)  !reducing par
          do j=1,ndgeta(icomp)                 !read dilute-gas terms
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
          if (ndg2(icomp).ge.1) then
            do j=1,ndg2(icomp)               !read dilute-gas terms
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          end if
          if (ndg3(icomp).ge.1) then
            do j=1,ndg3(icomp)               !read dilute-gas terms
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          end if
          if (ndg4(icomp).ge.1) then
            do j=1,ndg4(icomp)               !read dilute-gas terms
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          end if
          if (ndg5(icomp).ge.1) then
            do j=1,ndg5(icomp)               !read dilute-gas terms
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          end if
          if (ndg6(icomp).ge.1) then
            do j=1,ndg6(icomp)               !read dilute-gas terms
              jterm=jterm+1
              read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
            enddo
          end if
        end if
        read (nread,*) nB2eta(icomp)               !# visc virial terms bot presently used
c       write (*,*) ' SETVS4--about to read ',nB2eta(icomp),' virial terms'
        if (nB2eta(icomp).ge.1) then
          read (nread,*) tredB2(icomp),etarB2(icomp)  !reducing par
          do j=1,nB2eta(icomp)        !read viscosity virial terms
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
c
c  read the 18 parameters in the original generalized model
c  these are in the order: a(0:2),b(0:2),c(0:2),A(0:2),B(0:2),C(0:2)
c       write (*,*) ' SETVS4--about to read residual terms'
        if (verfl(icomp) .le.8.0) then
          READ(nread,*)(ASftm(icomp,j),j=0,2)
          READ(nread,*)(BSftm(icomp,j),j=0,2)
          READ(nread,*)(CSftm(icomp,j),j=0,2)
          READ(nread,*)(ABftm(icomp,j),j=0,2)
          READ(nread,*)(BBftm(icomp,j),j=0,2)
          READ(nread,*)(CBftm(icomp,j),j=0,2)
c  one additional higher order term needed for high pressures (>500 MPa)
          READ(nread,*)(DBftm(icomp,j),j=0,2)
        else ! more terms in v8.1 and above
          READ(nread,*)(ASftm(icomp,j),j=0,4)
          READ(nread,*)(BSftm(icomp,j),j=0,4)
          READ(nread,*)(CSftm(icomp,j),j=0,4)
          READ(nread,*)(ABftm(icomp,j),j=0,4)
          READ(nread,*)(BBftm(icomp,j),j=0,4)
          READ(nread,*)(CBftm(icomp,j),j=0,4)
          READ(nread,*)(DBftm(icomp,j),j=0,4)
        endif
        if (verfl(icomp).ge.9.0) then  !read additional coefficients
          READ(nread,*)(EBftm(icomp,j),j=0,4)
        else
          do j=0,4
          EBftm(icomp,j)=0.0d0
          end do
        endif
c
c
c  read in pointer to critical enhancement model
        read (nread,2003) hetacr(icomp)
c       write (*,*) ' SETVS4--will use critical model ',hetacr(icomp)
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETVS4
c
c ======================================================================
c
      function ETA4DG (icomp,t)
c
c  dilute-gas contribution to the viscosity by the friction theory
c  model (VS4)   allowing for denominator terms not present in VS1 model
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta4dg--the dilute-gas part of the viscosity [uPa-s]
c
c  12-10-07 MLH original version based on VS1
c  01-04-08 MLH, added additional slots to WNTETA
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      nterm=0                                    !term counter
      eta4dg=0.0d0
c

c  sum the dilute-gas terms
      eta4dg=0.0d0
      if (ndgeta(i).ge.1 .and. t.lt.1.d8) then
        tau=t/trddge(i)

c  first term is always the Chapman-Enskog term
        eta4dg=ceta(i,1,1)*SQRT(tau)/(sigmav(i)**2
     &        *OMEGA(i,t,epskv(i),hmdeta(i)))

        IF (ndgeta(i).ge.2) then
c  possibility for additional, empirical terms
          do j=nterm+1,nterm+ndgeta(i)
            eta4dg=eta4dg+ceta(i,j,1)*tau**ceta(i,j,2)
          enddo
        endif
      endif
      jterm=j
        IF(ndg2(i).gt.0)then
          sumnum = 0.0d0
          do j=jterm,jterm+ndg2(i)-1
            sumnum= sumnum +ceta(i,j,1)*tau**ceta(i,j,2)
          enddo
          eta4dg =eta4dg * sumnum
          jterm=j
        endif
        IF(ndg3(i).gt.0)then
          sumden = 0.0d0
          do j=jterm,jterm+ndg3(i)-1
            sumden = sumden +ceta(i,j,1)*tau**ceta(i,j,2)
          end do
          eta4dg = eta4dg / sumden
        endif

c  multiply by reducing parameter for viscosity (to convert units, etc.)
      eta4dg=eta4dg*etadge(i)
c     write (*,*) ' ETA4DG--d.g. visc:   ',eta1dg
      RETURN
      end                                               !function ETA4DG
c
c ======================================================================
c ======================================================================
c
      function ETA4RS (icomp,t,rho)
c
c  residual contribution to the viscosity for the friction theory model (VS4)
c  S.E. Quinones-Cisneros and U.K. Deiters,
c  "Generalization of the Friction Theory for Viscosity Modeling,"
c  J. Phys. Chem. B 2006, 110,12820-12834.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   eta4rs--the background part of the viscosity [uPa-s]
c
c  12-28-06 MLH, original version
c  01-18-07 MLH, put fgam =psi2 to accommodate new methane equation
c  01-23-07 MLH, fix mix bug
c  02-21-07 MLH, allow for higher order repulsive terms, check for 2-phase
c  11-20-07 MLH, removed unnecessary common blocks and declarations
c  04-28-08 MLH, increased array size in trnft to accommodate new h2 model
c  02-03-10 MLH, added HSQM term for n-hydrogen and parahydrogen only
c  12-28-10 MLH, added Boshkova -Deiters diameter
c  01-08-12 MLH, added d and e parameters
C  04-12-12 MLH, allowed extra terms except keep old methane form
c  07-11-12 MLH, added terms for 1234yf
c  11-06-12 MLH, revised to not use short equ at sat bdry
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension FEED (ncmax),xliq(ncmax),xvap(ncmax)
c
      i=icomp
      eta4rs=0.0d0

c     set up constants
      rgas= 0.08314472d0
      conb= 100.0d0
      avo=6.02214179d23
      bolt=1.3806504d-23
      planck=6.62606896d-34
      pi=3.14159265358979d0
      chs=3.0d0*SQRT(2.0d0)
      emm=3.34707d-27

      kph=1
      feed(1)= 1.0d0
      if (rho.lt.1.0d-10) RETURN

c     get value of pressure for the pure fluid at t,rho
      icc=icnc            !Save current value of icnc in case user has called PUREFLD
      call PUREFLD(i)     !obtain p of the pure at actual t, rho
      call PRESS (t,rho,feed,pkpa)
      p=pkpa/conb         !convert to bar
c
c
c     for normal or para hydrogen, use special case here with hard spheres quantum mechanical term
      IF(hcas(i).eq.'1333-74-0'.or.hcas(i).eq.'1333-74-0p')then  !normal or parahydrogen
        tr=t/tcrit(i)
        psi1= EXP(1.0d0/tr-1.0d0)-1.0d0
        psi2= EXP(1.0d0/(tr**2)-1.0d0)-1.0d0
        psi3=EXP(1.0d0/(tr**3)-1.0d0)-1.0d0         !march 1 normal h2 model
        psi4=(1.0d0/tr-1)

        sumkapa=(ASftm(i,0)+ASftm(i,1)*psi1 +ASftm(i,2)*psi2
     &                  +ASftm(i,3)*psi3+ASftm(i,4)*psi4)/(tr)
        sumkapaa=(ABftm(i,0)+ABftm(i,1)*psi1+ABftm(i,2)*psi2
     &                  +ABftm(i,3)*psi3+ABftm(i,4)*psi4)/tr
        sumkapr=(BSftm(i,0)+BSftm(i,1)*psi1 +BSftm(i,2)*psi2
     &                  +BSftm(i,3)*psi3+BSftm(i,4)*psi4)/(tr)
        sumkaprr=(BBftm(i,0)+BBftm(i,1)*psi1+BBftm(i,2)*psi2
     &                  +BBftm(i,3)*psi3+BBftm(i,4)*psi4)/tr
        sumkaprrr=(DBftm(i,0)+DBftm(i,1)*psi1+DBftm(i,2)*psi2
     &                  +DBftm(i,3)*psi3+DBftm(i,4)*psi4)/(tr**3.0)
        if(hcas(i).eq.'1333-74-0')then  !normal
          sumkapaaa=((9.750655931493327d-11)*psi2)/tr**3    !3/1/11 normal
        else                            !para
          sumkapaaa=((3.8478220629752735d-9)*psi2)/tr**3    !1/27/11 para
        endif
c the hard sphere quantum terms
        sigg0=2.97d-10              !m specific to hydrogen
        epsi=30.41d0
        tsquig=T/epsi               !revision for soft sphere
        dnumer=1.0d0+0.4378142*tsquig+0.009951898*tsquig**2
        ddenom=1.0d0+0.4745558*tsquig+0.01338717*tsquig**2
        sigg=0.925*dnumer*sigg0/ddenom
        ppsi=rho*1000.0d0*(pi/6.0d0)*avo*sigg**3
        xlam=planck/SQRT(2.0d0*pi*emm*bolt*t)
        term1=ppsi*xlam*chs*(1.0d0+ppsi-0.5d0*ppsi**2)/((1.0d0-ppsi)**4)
        term1=term1/(sigg*100.0d0)
        term2=(1.0d0+ppsi+ppsi**2-ppsi**3)/((1.0d0-ppsi)**3)
        zr=term1+term2
        prep=zr * rho * rgas* t    !bar
        patt = p - prep            !bar
        pid = rho * rgas * t       !bar
        delpr = prep - pid         !bar
        eta4rs = sumkapr*delpr +sumkapa*patt+sumkaprr*prep**2 +
     &         sumkapaa*patt**2
     &         +sumkaprrr*prep**3  !in upa.s
     &         +sumkapaaa*patt**3
        icnc=icc                   ! reset
        return
      else
      endif
c
c  for everything except normal hydrogen and parahydrogen
c  compute the residual term

      tr=tcrit(i)/t
      psi1= EXP(tr)-1.0d0
      psi2= EXP(tr**2)-1.0d0
      psi3= tr**(-0.25d0)
      fgam = psi2

      sumkapa=(ASftm(i,0)+ASftm(i,1)*psi1 +ASftm(i,2)*psi2)*tr
      sumkapaa=(ABftm(i,0)+ABftm(i,1)*psi1 +ABftm(i,2)*psi2)*tr**3
      sumkapr=(BSftm(i,0)+BSftm(i,1)*psi1 +BSftm(i,2)*psi2)*tr
      sumkaprr=(BBftm(i,0)+BBftm(i,1)*psi1 +BBftm(i,2)*psi2)*tr**3
      sumkapi=(CSftm(i,0)+CSftm(i,1)*psi1 +CSftm(i,2)*fgam)*tr
      sumkapii=(CBftm(i,0)+CBftm(i,1)*psi1 +CBftm(i,2)*psi2)*tr**3

      if (verfl(i) .gt.8.0) then ! additional terms
        sumkapa  = sumkapa  + ASftm(i,3)*psi3*tr
        sumkapaa = sumkapaa + ABftm(i,3)*psi3*tr**3
        sumkapr  = sumkapr  + BSftm(i,3)*psi3*tr
        sumkaprr = sumkaprr + BBftm(i,3)*psi3*tr**3
        sumkapi  = sumkapi  + CSftm(i,3)*psi3*tr
        sumkapii = sumkapii + CBftm(i,3)*psi3*tr**3
      else
      endif
      ! next compute dpdt at actual t, rho
      call DPDT (t,rho,feed,dpt)
      IF(t.gt.tcrit(i))then
        prep = t*dpt/conb         !bar
        patt = p - prep           !bar
        pid = rho * rgas * t
        delpr = prep - pid
        eta4rs = sumkapr*delpr +sumkapa*patt+sumkaprr*delpr**2 +
     &         sumkapaa*patt**2 +sumkapi*pid + sumkapii*pid**2
     &         + DBftm(i,0)*(prep**3)*(tr**2)
        IF(verfl(i).gt.8.0 .and. hcas(i).ne.'74-82-8') then !extra terms except methane
         eta4rs = eta4rs+
     &     (DBftm(i,0)+DBftm(i,1)*psi1+DBftm(i,2)*psi2+DBftm(i,3)*psi3)
     &     *(prep**3)*tr**DBftm(i,4)
     &     + (EBftm(i,0)+EBftm(i,1)*psi1+EBftm(i,2)*psi2)*(patt**3)
     &     *tr**EBftm(i,4)
        else
        endif
      ELSE   !could be in two-phase region
        !find sat rhol, rhov for this t and dpdt on the boundary
        !check if short sat equations are available  revision 9.1- don't use the short eq's
        if ((hdlk(i).ne.' ' .and. hdlk(i).ne.'NBS').and.
     &      (hdvk(i).ne.' ' .and. hdvk(i).ne.'NBS'))then
          !call DLSATK (i,t,rhol,ierr,herr)
          !call DVSATK (i,t,rhov,ierr,herr)
          !call PSATK (i,t,psatkpa,ierr,herr)
          call SATT (t,feed,kph,psatkpa,rhol,rhov,xliq,xvap,ierr,herr)
        else  !no short forms present; call full
          call SATT (t,feed,kph,psatkpa,rhol,rhov,xliq,xvap,ierr,herr)
        endif
        psat=psatkpa/conb         !convert to bar
      ! if in the two-phase region, interpolate liquid and vapor endpoints to avoid
      ! bizarre behavior caused by loops from the EOS
        IF((rho.gt.rhov).AND.(rho.lt.rhol)) then   ! it is two-phase region
          p=psat
          ! get viscosity on liquid side
          call DPDT (t,rhol,feed,dptl)
          prep = t*dptl/conb      !bar
          patt = p - prep         !bar
          pid = rhol * rgas * t
          delpr = prep - pid
          eta4rsl = sumkapr*delpr +sumkapa*patt+sumkaprr*delpr**2 +
     &         sumkapaa*patt**2 +sumkapi*pid + sumkapii*pid**2
     &         + DBftm(i,0)*(prep**3)*(tr**2)
          IF(verfl(i).gt.8.0 .and. hcas(i).ne.'74-82-8') then !extra terms
            eta4rsl = eta4rsl+
     &      (DBftm(i,0)+DBftm(i,1)*psi1+DBftm(i,2)*psi2+DBftm(i,3)*psi3)
     &      *(prep**3)*tr**DBftm(i,4)
     &      + (EBftm(i,0)+EBftm(i,1)*psi1+EBftm(i,2)*psi2)*(patt**3)
     &      *tr**EBftm(i,4)
          else
          endif
          ! get viscosity on vapor side
          call DPDT (t,rhov,feed,dptv)
          prep = t*dptv/conb      !bar
          patt = p - prep         !bar
          pid = rhov * rgas * t
          delpr = prep - pid
          eta4rsv = sumkapr*delpr +sumkapa*patt+sumkaprr*delpr**2 +
     &         sumkapaa*patt**2 +sumkapi*pid + sumkapii*pid**2
     &         + DBftm(i,0)*(prep**3)*(tr**2)
          IF(verfl(i).gt.8.0 .and. hcas(i).ne.'74-82-8') then !extra terms
            eta4rsv = eta4rsv+
     &      (DBftm(i,0)+DBftm(i,1)*psi1+DBftm(i,2)*psi2+DBftm(i,3)*psi3)
     &      *(prep**3)*tr**DBftm(i,4)
     &      + (EBftm(i,0)+EBftm(i,1)*psi1+EBftm(i,2)*psi2)*(patt**3)
     &      *tr**EBftm(i,4)
          else
          endif
          drat=(rhol-rho)/(rhol-rhov)
          eta4rs=eta4rsl-drat*(eta4rsl-eta4rsv)
        ELSE !not in two-phase
          prep = t*dpt/conb       !bar
          patt = p - prep         !bar
          pid = rho * rgas * t
          delpr = prep - pid
          eta4rs = sumkapr*delpr +sumkapa*patt+sumkaprr*delpr**2 +
     &         sumkapaa*patt**2 +sumkapi*pid + sumkapii*pid**2
     &         + DBftm(i,0)*(prep**3)*(tr**2)
          IF(verfl(i).gt.8.0 .and. hcas(i).ne.'74-82-8') then !extra terms
            eta4rs = eta4rs+
     &      (DBftm(i,0)+DBftm(i,1)*psi1+DBftm(i,2)*psi2+DBftm(i,3)*psi3)
     &      *(prep**3) *tr**DBftm(i,4)
     &      + (EBftm(i,0)+EBftm(i,1)*psi1+EBftm(i,2)*psi2)*(patt**3)
     &      *tr**EBftm(i,4)
          else
          endif
        ENDIF
      ENDIF
      eta4rs=eta4rs*1000.0d0 ! convert to uPa.s
      icnc=icc               ! reset
      RETURN
      end                                               !function ETA4RS
c
c ======================================================================
c
      subroutine SETVS5 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #5; this, the "Chung model,"
c  T-H. Chung, M. Ajlan, L.L. Lee, and K.E. Starling
c  "Generalized Multiparameter Correlation for Nonpolar and Polar Fluid Transport Properties"
c  Ind. Eng. Chem. Res. 1988, 27, 671-679.
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                      101 = error--block data option not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  11-19-07  MLH, original version
c  01-04-08 MLH, added additional slots to WNTETA
c  01-04-08 MLH, allow dilute and residual parameters to be independent
c
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETVS5 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETVS5--read component',icomp,' from unit',nread
        read (nread,*) tmeta(icomp)              !lower temperature limit
        read (nread,*) txeta(icomp)              !upper temperature limit
        read (nread,*) pxeta(icomp)              !upper pressure limit
        read (nread,*) Dxeta(icomp)              !upper density limit
        jterm=0                                 !term counter
        read (nread,*) ndgeta(icomp)               !# dilute-gas terms
c       write (*,*) ' SETVS5--about to read ',ndgeta(icomp),' dilute terms'
        if (ndgeta(icomp).ge.1) then
          read (nread,2003) hmdeta(icomp)       !pointer to omega model
          read (nread,*) sigchv(icomp)          !L-J sigma for chung viscosity model
          read (nread,*) epschv(icomp)          !L-J epsilon/kappa for chung viscosity model
          read (nread,*) trddge(icomp),etadge(icomp)  !reducing par
          do j=1,ndgeta(icomp)                 !read dilute-gas terms
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
        read (nread,*) nB2eta(icomp)               !# visc virial terms bot presently used
c       write (*,*) ' SETVS5--about to read ',nB2eta(icomp),' virial terms'
        if (nB2eta(icomp).ge.1) then
          read (nread,*) tredB2(icomp),etarB2(icomp)  !reducing par
          do j=1,nB2eta(icomp)        !read viscosity virial terms
            jterm=jterm+1
            read (nread,*) ceta(icomp,jterm,1),ceta(icomp,jterm,2)
          enddo
        end if
c
c       write (*,*) ' SETVS5--about to read Chung-specific terms'
        READ(nread,*)acchv(icomp),ddipv(icomp),hbvk(icomp)
c       set residual parameters
        cceta(icomp,1,1)=sigchv(icomp)
        cceta(icomp,1,2)=epschv(icomp)
        cceta(icomp,1,3)=acchv(icomp)
        cceta(icomp,1,4)=ddipv(icomp)
        cceta(icomp,1,5)=hbvk(icomp)
        READ(nread,*) naddv(icomp) !additional terms
c       use these terms if residual parameters are not the same as dilute gas
        jtc=0
        do j =1, naddv(icomp)
         jtc=jtc+1
         READ(nread,*) cceta(icomp,jtc,1),cceta(icomp,jtc,2),
     &    cceta(icomp,jtc,3),cceta(icomp,jtc,4),cceta(icomp,jtc,5)
        end do
c
c  read in pointer to critical enhancement model
        read (nread,2003) hetacr(icomp)
c       write (*,*) ' SETVS5--will use critical model ',hetacr(icomp)
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETVS5
c
c ======================================================================
c
      function ETA5DG (icomp,t)
c
c  dilute-gas contribution to the viscosity by the Chung model (VS5)
c  T-H. Chung, M. Ajlan, L.L. Lee, and K.E. Starling
c  "Generalized Multiparameter Correlation for Nonpolar and Polar Fluid Transport Properties"
c  Ind. Eng. Chem. Res. 1988, 27, 671-679. Also see Reid, Prausnitz and Poling Chapter 9.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   eta1dg--the dilute-gas part of the viscosity [uPa-s]
c
c  11-02-07 MLH original
c  01-04-08 MLH, added additional slots to WNTETA
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      nterm=0                                    !term counter
      eta5dg=0.0d0
c
c  sum the dilute-gas terms
      if (ndgeta(i).ge.1 .and. t.lt.1.0d8) then
        tau=t/epschv(i)
        vc = (sigchv(i)/8.09d0)**3
c  first term is always the Chapman-Enskog term
        eta5dg=0.00040785d0*SQRT(t*wmas(i))/(vc**(2./3.)
     &        *OMEGAS(2,2,tau))
        if (ndgeta(i).ge.2) then
c  possibility for additional, empirical terms
          do j=nterm+1,nterm+ndgeta(i)
            eta5dg=eta5dg+ceta(i,j,1)*tau**ceta(i,j,2)
          enddo
        end if
      end if
c
c  multiply by Chung factor
      fchung= 1.0d0 - 0.2756d0 * acchv(i) + 0.059035d0 * ddipv(i)**4
     &        + hbvk(i)
      eta5dg = eta5dg * fchung
      eta0ch = eta5dg
c  multiply by reducing parameter for viscosity (to convert units, etc.)
      eta5dg=eta5dg*etadge(i)
c     write (*,*) ' ETA5DG--d.g. visc:   ',eta5dg
      RETURN
      end                                               !function ETA5DG
c
c ======================================================================
c
      function ETA5RS (icomp,t,rho)
c
c  residual contribution to the viscosity for the Chung model (VS5)
c  T-H. Chung, M. Ajlan, L.L. Lee, and K.E. Starling
c  "Generalized Multiparameter Correlation for Nonpolar and Polar Fluid Transport Properties"
c  Ind. Eng. Chem. Res. 1988, 27, 671-679. Also see Reid, Prausnitz and Poling Chapter 9.
c
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   eta5rs--the background part of the viscosity [uPa-s]
c
c  note: this version allows the parameters for the residual piece to
c  be independent from the dilute gas and fit as free parameters
c  nomenclature for comparison with manuscript
c        sigchv(i)=cceta(i,1,1)
c        epschv(i)=cceta(i,1,2)
c        acchv(i)=cceta(i,1,3)
c        ddipv(i)=cceta(i,1,4)
c        hbvk(i)=cceta(i,1,5)
c
C     modified 6.14.12 MLH
c
      include 'commons.for'
      include 'comtrn.for'
      dimension A0(10),A1(10),A2(10),A3(10), AA(10)
c
      parameter (A0=
     & (/6.32402D0, 0.12102D-2, 5.28346D0, 6.62263D0, 19.74540D0,
     &   -1.89992D0, 24.2745D0, 0.79716D0, -0.23816D0, 0.68629D-1 /))
      parameter (A1=
     & (/50.4119D0, -0.11536D-2, 254.209D0, 38.0957D0, 7.63034D0,
     &   -12.5367D0, 3.44945D0, 1.11764D0, 0.67695D-1, 0.34793D0 /))
      parameter (A2=
     & (/-51.6801D0, -0.62571D-2, -168.481D0, -8.46414D0,
     &   -14.3544D0, 4.98529D0, -11.2913D0, 0.12348D-1, -0.8163D0,
     &   0.59256D0 /))
      parameter (A3=
     & (/1189.02D0, 0.37283D-1, 3898.27D0, 31.4178D0, 31.5267D0,
     &   -18.1507D0, 69.3466D0, -4.11661D0, 4.02528D0, -0.72663D0 /))
      i=icomp
      eta5rs=0.0D0
      twothr=2.0d0/3.0d0
      IF(rho.lt.1.0D-10) RETURN
c
      tstar=t/cceta(i,1,2)
      vc = (cceta(i,1,1)*10.0D0/0.809D0)**3 !compute vc in cm3/mol from Chung sigma
      fchung= 1.0D0 - 0.2756D0*cceta(i,1,3) + 0.059035D0*cceta(i,1,4)**4
     &        +  cceta(i,1,5)
c
      do j=1,10
        AA(J)=A0(j)+A1(j)*cceta(i,1,3)+A2(j)*cceta(i,1,4)**4+
     &        A3(j)*cceta(i,1,5)
      end do
c
      y = rho * vc/ 6.0D3
      g1=(1.0D0 -0.5D0*y)/((1.0D0-y)**3)
      g2=(AA(1)*(1.0D0- EXP(-AA(4)*y))/y+AA(2)*g1*EXP(AA(5)*y)+AA(3)*g1)
     &   /(AA(1)*AA(4)+AA(2)+AA(3))
c
c      etass = AA(7)*y*y*g2*EXP(AA(8)+AA(9)/tstar+AA(10)/tstar**2)
      omegav = omegas(2,2,tstar)
c     etas=SQRT(tstar)*(1.0D0/g2+AA(6)*y)*fchung/omegav + etass
c
c     eta5rs=etas
c    & *36.344D0*SQRT(wmas(i)*1.2593D0*cceta(i,1,2))/(vc**twothr)
c     eta5rs=eta5rs/10.0D0 -eta0ch !convert from upoise to upa.s and subtract off dg
c
      eta0chung=4.0785d-5*SQRT(wmas(i)*t)*fchung/(omegav*vc**twothr)
      etakap=eta0chung*(1.0d0/g2+aa(6)*y)
      etap=(36.344d-6*SQRT(wmas(i)*1.2593*cceta(i,1,2))/(vc**twothr))
     & *aa(7)*y*y*g2*
     &EXP(aa(8)+aa(9)/tstar+aa(10)/tstar**2)
      etach=etakap+etap  ! in poise
      eta5rs=(etach- eta0chung)*1d5 !convert to upa.s

      RETURN
      end                                               !function ETA5RS
c
c ======================================================================
c
      subroutine SETVS6 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid viscosity model #6
c
c  temporary place holder--this model is not yet implemented
c
c  inputs:
c    nread--file to read data from
c           <= 0 get data from block data
c           >0 read from logical unit nread (file should have already
c              been opened and pointer set by subroutine SETUP)
c    icomp--component number in mixture (0..nc)
c           1 for pure fluid; 0 for ECS reference fluid
c   hcasno--CAS number of component icomp (not req'd if reading from file)
c
c  outputs:
c     ierr--error flag:  0 = successful
c                       49 = error--model not implemented
c                      101 = error--block data option not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-18-96  MM, original version
c  08-19-97  MM, change error number for nread<=0; input hcasno is not array
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETVS6 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
        hetacr(icomp)='NUL'
        ierr=49
        herr='[SETUP error 49] viscosity model #6 specified in fluid '//
     &       'file but not implemented in code.'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                             !subroutine SETVS6
c
c ======================================================================
c
c     The following functions (through EXCESV) were taken from NIST12,
c     Version 3.1, and modified to work with the current version.
c
      function ETAH2mct (icomp,t,d)
c
c  model for the viscosity of para and normal hydrogen
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c        d--molar density [mol/L]
c  output (as function value):
c     eta5--viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  10-20-99 EWL, original version
c  09-13-02 EWL, removed code for d>27 and d>dtest
c
      include 'commons.for'
      include 'comtrn.for'
c
c  The code for d>27 and d>dtest caused vis to be constant at any pressure
c  above 27 or dtest.  These checks were once used because the viscosity
c  starts to drop as the temperature drops in the liquid at high densities.
c  By changing the maximum density of the formulation to 44 mol/L, the drops
c  are not significant, and the tests can be removed.
      IF (T.GT.100.0D0) THEN
c       IF (D.GT.27.D0) THEN
c         ETAH2=DILV(icomp,100.D0)+EXVDIL(27.D0,100.D0)
c    &        +DELV(icomp,D,100.D0,27.D0,100.D0)
c    &        +DELV(icomp,D,T,D,100.D0)
c       ELSE
          ETAH2mct=DILV(icomp,100.D0)+EXVDIL(D,100.0D0)
     &          +DELV(icomp,D,T,D,100.0D0)
c       ENDIF
      ELSE
c       DTEST=49.D0-0.2204D0*T
c       IF (D.LE.DTEST) THEN
          ETAH2mct=DILV(icomp,T)+EXVDIL(D,T)
c       ELSE
c         ETAH2mct=DILV(icomp,T)+EXVDIL(DTEST,T)+DELV(icomp,D,T,DTEST,T)
c       ENDIF
      ENDIF
      END

      function DELV (icomp,D1,T1,D2,T2)
      include 'commons.for'
      include 'comtrn.for'
c
      DELV=DILV(icomp,T1)+EXCESV(icomp,D1,T2)
     &    -DILV(icomp,T2)-EXCESV(icomp,D2,T2)
      END

      function EXVDIL (DD,T)
      include 'commons.for'
      include 'comtrn.for'
c
      EXVDIL=0.0D0
      IF (DD.LE.0) RETURN
      D=DD*2.0159D0/1000.0D0
      A=5.7694D0+LOG(D)+0.65D2*D**1.5D0- 6.0D-6*DEXP(127.2D0*D)
      A=DEXP(A)
      B=1.0D1+7.2D0*((D/0.07D0)**6-(D/0.07D0)**(3.0D0/2.0D0))
     &-17.63D0*DEXP(-58.75D0*(D/0.07D0)**3)
      EXVDIL=A*DEXP(B/T)*0.1D0
      END

      function DILV (icomp,T)
      include 'commons.for'
      include 'comtrn.for'
c
      SUM=0.0D0
      TF=T**(1.0D0/3.0D0)
      TFF=T**(-4.0D0/3.0D0)
      DO I=1,9
        TFF=TFF*TF
        SUM=SUM+ceta(icomp,i,1)*TFF
      ENDDO
      DILV=SUM*100.0D0
      END

      function EXCESV (icomp,DD,T)
      include 'commons.for'
      include 'comtrn.for'
      double precision EV(8)
c
      EV(1)=ceta(icomp,10,1)
      EV(2)=ceta(icomp,11,1)
      EV(3)=ceta(icomp,12,1)
      EV(4)=ceta(icomp,13,1)
      EV(5)=ceta(icomp,14,1)
      EV(6)=ceta(icomp,15,1)
      EV(7)=ceta(icomp,16,1)
      EV(8)=ceta(icomp,17,1)
      D=DD*2.0159D0/1000.0D0
      R2=D**0.5D0*(D-EV(8))/EV(8)
      RR=D**0.1D0
      X=EV(1)+EV(2)*R2+EV(3)*RR+EV(4)*R2/(T*T)+EV(5)*RR/T**1.5D0+EV(6)/T
     &+EV(7)*R2/T
      X1=EV(1)+EV(6)/T
      EXCESV=(DEXP(X)-DEXP(X1))*0.1D0
      END
c
c ======================================================================
      function ETAH2 (icomp,t,d)
c
c  model for the viscosity of normal hydrogen from symbolic regression
c  Muzny, C.D., Huber, M.L., and Kazakov, A.F.,
c  "Correlation for the Viscosity of normal hydrogen obtained from symbolic regression"
c  submitted to J. Chem. Eng. Data, 2013
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c        d--molar density [mol/L]
c  output (as function value):
c     etah2--viscosity [uPa-s]
c
c  11-16-12   mlh, original
c
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
      DIMENSION sma(0:4), beta(6)
      DATA sma(0), sma(1), sma(2), sma(3), sma(4)
     &  / 0.209630d0,-0.455274d0,
     &  0.143602d0, -0.335325d-01, 0.276981d-02 /     ! coeff to match moldover
      DATA beta(1), beta(2), beta(3),beta(4), beta(5), beta(6)
     &    /0.118757d-05, 0.536373d0, 0.759423d+01, 0.615343d+01,
     &    -0.193070d+01, 0.011d0 /
        ts=t/30.41d0
        sum1=0.0d0
        do i=0,4
        sum1=sum1+sma(i)*(LOG(Ts))**i
        end do
        sigmast=EXP(sum1)
        etadil=0.021357*SQRT(2.01588*t)/(0.297*0.297*sigmast)
        beta1=0.6022137*(-0.187+2.4871/(t/30.41)+3.7151/(t/30.41)**2
     &  -11.0972/(t/30.41)**3+9.0965/(t/30.41)**4-3.8292/(t/30.41)**5
     &  +0.5166/(t/30.41)**6)*(0.297**3)
        eta1=etadil*beta1*d  !uPa.s
        rhokg=d*2.01588
        visa=etadil+eta1
      IF(rhokg.gt.0)then
        tred=t/33.145
        dred=rhokg*0.011
        ! nov8 machine 37 optimized by andrei
        diff2 = 6.43449673d-03*dred**2*exp(4.56334068d-02*tred +
     &  2.32797868d-01/tred + 9.58326120d-01*dred**2/(1.27941189d-01
     &  + tred) +    3.63576595d-01*dred**6)
        etares=1000*diff2
      else
        etares=0
      endif
      etah2=visa+etares
      END
c
c
      function ETAHE (icomp,t,rho)
c
c  viscosity of helium taken from:
c    Arp, V.D., McCarty, R.D., and Friend, D.G.,
c    "Thermophysical Properties of Helium-4 from 0.8 to 1500 K with
c     Pressures to 2000 MPa,"
c    NIST Technical Note 1334, Boulder, CO 1998.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   etahe --viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  07-06-98 EWL, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  02-16-99 EWL, rename to ETA4RS
c  11-06-00 EWL, change to ETAHE using the hardcoded model
c  01-25-07 EWL, check for large value in the calculation of expe
c
      include 'commons.for'
      include 'comtrn.for'
c
      x=5.7037825d0
      if (t.le.300.0d0) x=log(t)
      dd=rho*wmas(icomp)/1000.0d0
      b=-47.5295259d0/x+87.6799309d0- 42.0741589d0*x
     &  +8.33128289d0*x**2-0.589252385d0*x**3
      c= 547.309267d0/x-904.870586d0+431.404928d0*x
     &  -81.4504854d0*x**2+5.37008433d0*x**3
      d=-1684.39324d0/x+3331.08630d0 -1632.19172d0*x
     &  +308.804413d0*x**2-20.2936367d0*x**3

      eta0a=exp(-0.135311743d0/x+1.00347841d0+1.20654649d0*x
     &     -0.149564551d0*x**2+0.0125208416d0*x**3)
      ee=b*dd+c*dd**2+d*dd**3
      if (ee.gt.100.d0) ee=100.0d0
      etae=exp(ee)
      if (t.gt.100.0d0) then
        eta0b=196.0d0*t**0.71938d0
     &       *exp(12.451d0/t-295.67d0/t**2-4.1249d0)
        if (t.lt.110.0d0) then
          eta0=eta0a+(eta0b-eta0a)*(t-100.0d0)/10.0d0
        else
          eta0=eta0b
        endif
        viscos=eta0a*etae+eta0-eta0a
      else
        viscos=eta0a*etae
      endif

      etahe=viscos/10.0d0
c     write (*,*) ' ETAHE--residual viscosity:    ',etahe
c
      RETURN
      end                                                !function ETAHE
c
c ======================================================================
c
      function ETAETY (icomp,t,rho)
c
c  viscosity model for ethylene by Holland et al. (1983)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   etaety--viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-29-00 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
      double precision gv(9)
c
      etaety=0.0d0
      gv(1)=-3.5098225018d6
      gv(2)= 2.5008406184d6
      gv(3)=-5.8365540744d5
      gv(4)= 4.5549146583d3
      gv(5)= 2.2881683403d4
      gv(6)=-4.7318682077d3
      gv(7)= 4.5022249258d2
      gv(8)=-2.1490688088d1
      gv(9)= 4.1649263233d-1
      v1=-4.8544486732d0
      v2= 1.3033585236d1
      v3= 2.7808928908d4
      v4=-1.8241971308d3
      v5= 1.5913024509d0
      v6=-2.0513573927d2
      v7=-3.9478454708d4
      d=rho*wmas(icomp)/1000.0d0
      dc=.221d0
      th=(d-dc)/dc
      tt=t**(1.0d0/3.0d0)
      eta0=gv(1)/t+gv(2)/tt**2+gv(3)/tt+gv(4)+gv(5)*tt
     &    +gv(6)*tt**2+gv(7)*t+gv(8)*tt**4+gv(9)*tt**5
      etapr=exp(v1+v4/t)*(exp(d**0.1d0*(v2+v3/t**1.5d0)
     &     +th*d**0.5d0*(v5+v6/t+v7/t**2))-1.0d0)
      etaety=(eta0+etapr)/10.0d0
c
      RETURN
      end                                               !function ETAETY
c
c ======================================================================
c
      function ETANEO (icomp,t,rho)
c
c  viscosity model for neon of:
c
c  Rabinovich, V.A., Vasserman, A.A., Nedostup, V.I. and Veksler, L.S.
c   "Thermophysical Properties of Neon, Argon, Krypton, and Xenon,"
c   Hemisphere Publishing Corp., 1988.
c
c  The numbers calculated here do not exactly match those given by Rabinovich.
c  The ECS model is currently the preferred model.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   etaneo--viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  03-27-00 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
c
      etaneo=0.0d0
      a0= 17.67484d0
      a1=-2.78751d0
      a2= 311498.7d0
      a3=-48826500d0
      a4= 3938774000d0
      a5=-1.654629d11
      a6= 2.86561d12
      tred=t/0.29944
      y=a0+a1*log10(tred)+a2/tred**2+a3/tred**3+a4/tred**4
     & +a5/tred**5+a6/tred**6
      y=y*0.68321d0
      etat=266.93d0*SQRT(t*wmas(icomp))/y
      a0= 1.03010d0
      a1=-0.99175d0
      a2= 2.47127d0
      a3=-3.11864d0
      a4= 1.57066d0
      b0= 0.48148d0
      b1=-1.18732d0
      b2= 2.80277d0
      b3=-5.41058d0
      b4= 7.04779d0
      b5=-3.76608d0
      om=rho/(1673.0d0/wmas(icomp))
c  The sign has been changed in Eq. 5.23 to negative
      s=a0+a1*om+a2*om**2+a3*om**3+a4*om**4
     &-(b0+b1*om+b2*om**2+b3*om**3+b4*om**4+b5*om**5)*log10(t/122.1d0)
      s=s*0.000000000305d0
      b1= 0.27676d0
      b2= 0.014355d0
      b3= 2.6480d0
      b4=-1.9643d0
      b5= 0.89161d0
      b=2.0d0/3.0d0*3.1415927d0*6.02221367d23*s**3
      brho=rho*1000.0d0*b
      etad=1.0d0+b1*brho+b2*brho**2+b3*brho**3+b4*brho**4+b5*brho**5
      etaneo=etad*etat/100.d0
c
      RETURN
      end                                               !function ETANEO
c
c ======================================================================
c
      function ETAR23 (icomp,t,rho)
c
c  viscosity model for R23
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   etaR23--viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  11-01-00 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
c
      etaR23=0.0d0
      rhoL=ceta(icomp,2,1)
      C1=ceta(icomp,3,1)
      C2=ceta(icomp,4,1)
      DG=ceta(icomp,5,1)
      etamax=ceta(icomp,6,1)
      drho=rhoL-rho
      del=rho-Dredeta(icomp)
      tau=T-tredeta(icomp)
      etadg=ETA1DG(icomp,t)*(drho/rhoL)**C1
      etars=(rho/rhoL)**C1*C2*rhoL**2/drho*t**0.5d0
     &     *EXP(rho/drho*DG/R/t)
      etacrt=4.0d0*etamax/(exp(del)+exp(-del))/(exp(tau)+exp(-tau))
      etaR23=etadg+etars+etacrt
c
      RETURN
      end                                               !function ETAR23
c
c ======================================================================
c
      function ETAH2O (icomp,t,rho)
c
c  viscosity model for water and heavy water
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   etaH2O--viscosity [uPa-s]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  11-06-00 EWL, original version
c  01-04-08 MLH, added additional slots to WNTETA
c  05-08-08 MLH, allow for IAPWS 2008 critical enhancement term
c
      include 'commons.for'
      include 'comtrn.for'
c
      etaH2O=0.0d0
      tr=t/tredeta(icomp)
      dr=rho/Dredeta(icomp)
      tau=1.0d0/tr-1.0d0
      del=dr-1.0d0
      if (ABS(tau).lt.1.0d-12 ) tau=1.0d-12
      if (ABS(del).lt.1.0d-12 ) del=1.0d-12
      s=0.0d0
      do i=1,ndel0(icomp)
        s=s+ceta(icomp,i,1)/tr**ceta(icomp,i,2)
      enddo
      eta0=SQRT(tr)/s
      s=0.0d0
      do i=1,npoly(icomp)
        j=i+ndel0(icomp)
        s=s+ceta(icomp,j,1)
     &        *tau**INT(ceta(icomp,j,2))*del**INT(ceta(icomp,j,3))
      enddo
      eta1=EXP(dr*s)
c     critical enhancement; default is none
      eta2=1.0d0
c
      IF(hetacr(icomp).eq.'I85')then
c     previous 1985 IAPWS critical enhancement
      if (nnumeta(icomp).gt.0) then
        if (tr.gt.0.997d0 .and. tr.lt.1.0082d0) then
          if (dr.gt.0.755d0 .and. dr.lt.1.29d0) then
            call DPDDK (icomp,t,rho,dpdrho)
            i=ndel0(icomp)+npoly(icomp)+1
            x=dr/dpdrho*ceta(icomp,i,1)/Dredeta(icomp)
            if (x.ge.ceta(icomp,i+1,3))
     &          eta2=ceta(icomp,i+1,1)*x**ceta(icomp,i+1,2)
          endif
        endif
      endif
c     2008 IAPWS standard critical enhancement
      ELSEIF(hetacr(icomp).eq.'I08')then
        IF(rho.gt.1.0d-12)then
        xival= xi_fun(t,rho,icomp)
        else
        xival=0.0d0
        endif
        eta2 = eta_c2(xival)
      endif
      etaH2O=etared(icomp)*eta0*eta1*eta2
c
      RETURN
      end                                               !function ETAH2O
c
      function XI_FUN (tk,rho,icomp)
c     auxiliary functions for computing the critical enhancement of viscosity of water
c     "New International Formulation for the viscosity of water"
c     Huber, M.L., Perkins, R.A., Laesecke, A., Friend, D.G., Sengers, J.V., Assael, M.J.,
c     Metaxa, I.M., Vogel, E., Mares, R. and Miyagawa, K.
c     for submission to JPCRD, 2008
c
c     05/08/08 MLH, based on code from RAP
c     09/20/10 MLH, remove extrap limit on chi
c
      include 'commons.for'
      include 'comtrn.for'
      dimension x(ncmax)
c     Input variables
c        tk      temperature,  K
c        rho     density,      mol/L
c     Output variables
c        xi_fun  delta_xi_star
c
      parameter (Tc=647.096D0, Denc=322.0D0)
      parameter (Pc=22.064D0, wmm=18.015268d0)
      parameter (xi0=0.13d-9, gammaplus=0.06d0)
      parameter (exnu=0.63d0, exgamma=1.239d0)
      do ii =1,ncmax
        x(ii)=0.0d0
      end do
      x(icomp)=1.0d0

c calculate background chi at 1.5 * Tc
      tr=1.5d0*Tc
      Denred=wmm*rho/Denc
c added 4/28/08 RP
      if (tr.lt.tk) tr = tk

c calculate isothermal compressibility at T and RHO in units (1/MPa)
      call DPDDK (icomp,tk,rho,dpdrho_kpa)
      call DPDDK (icomp,tk,rho,dpdrhok)
c     drhodp_kpa=1.0d0/dpdrhok
      dpdrho_mpa=dpdrho_kpa/1000.0d0
      tisocomp=1.0d0/rho/dpdrho_mpa
c calculate isothermal compressibility at TR and RHO in units (1/MPa)
      call PRESS (tr,rho,x,pkpa_r)
      !pmpa_r=pkpa_r/1000.0d0
c set reference compressibility to zero if P at TR >1000 MPa (EOS Pressure Limit)
      !if (pmpa_r.gt.1000.0d0) then
      !  tisocompr = 0.0d0
      !else
        call DPDDK (icomp,tr,rho,dpdrho_r_kpa)
        call DPDDK (icomp,tr,rho,dpdrhok)
c       drhodp_r_kpa=1.0d0/dpdrhok
        dpdrho_r_mpa=dpdrho_r_kpa/1000.0d0
        tisocompr=1.0d0/rho/dpdrho_r_mpa
      !end if
c calculate difference in reduced chi at (T,RHO) and (TR,RHO)
      delchi_red=Pc*Denred**2*(tisocomp-tisocompr*tr/tk)
      IF(delchi_red.lt.0.0d0) then
        delchi_red=0.0d0
        xi_fun=0.0d0
      else
c calculate correlation length in meters
        xi_fun=xi0*(delchi_red/gammaplus)**(exnu/exgamma)
      endif
      return
      end                                               !function XI_FUN
c
      function ETA_C2 (xi)
c     auxiliary functions for computing the critical enhancement of viscosity of water
c     "New International Formulation for the viscosity of water"
c     Huber, M.L., Perkins, R.A., Laesecke, A., Friend, D.G., Sengers, J.V., Assael, M.J.,
c     Metaxa, I.M., Vogel, E., Mares, R. and Miyagawa, K.
c     for submission to JPCRD, 2008
c
c     05/08/08 MLH based on code from RAP
c
      include 'commons.for'
      include 'comtrn.for'
c     data Tc,Denc,Pc/647.096D0, 322.0D0, 22.064D0/
c     data etabkc,lamdabkc/39.3d-6,0.197d0/
      parameter (qcinv=1.9d-9, qdinv=1.1d-9)
c
c     New theoretical exponent for viscosity
      parameter (exeta=0.0680d0)
      qc=1.0d0/qcinv
      qd=1.0d0/qdinv

      if (xi .le. 0.0d0) then
        eta_c2 = 1.0d0
        return
      end if

c calculate the crossover function
      psid=ACOS(SQRT(1.0d0/(1.0d0+(qd*xi)**2)))
      w=SQRT(ABS((qc*xi-1.0d0)/(qc*xi+1.0d0)))*TAN(psid/2.0d0)
      if (qc*xi.gt.1.0d0) then
        funL=LOG((1.0d0+w)/(1.0d0-w))
      else
        funL=2.0d0*ATAN(ABS(w))
      end if
      qcxi2=(qc*xi)**2
      qcxi3=(qc*xi)**3
      if (xi.le.0.3734351887d-9) then
        Hcross=qc*xi*(qd*xi)**5/5.0d0 *
     &  (1.0d0 - qc*xi + qcxi2 - (765.0d0/504.0d0)*(qd*xi)**2)
      else
        Hcross=1.0d0/12.0d0*sin(3.0d0*psid)-1.0d0/(4.0d0*qc*xi)*
     &  sin(2.0d0*psid)+1.0d0/qcxi2*(1.0d0- 1.25d0*qcxi2)*sin(psid)-
     &  1.0d0/qcxi3*((1.0d0- 1.5d0*qcxi2)*psid-
     &  abs((qcxi2-1.0d0))**1.5d0*funL)
      end if

c calculate eta/eta_background (the enhancement)
      eta_c2 = exp(exeta*Hcross)
      return
      end                                               !function ETA_C2
c
c ======================================================================
c
      function ETAMEO (icomp,t,rhom)
c
c  viscosity model for methanol
c  Xiang, H.W., Huber, M.L. and Laesecke, A., "A New Reference
c  Correlation for the Viscosity of Methanol",
c  J. Phys. Chem. Ref. Data V35, No.4, 2006, pp. 1597-1620.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rhom--molar density [mol/L]
c  output (as function value):
c   etaMEOH--viscosity [uPa-s]
c
c  11.09.05 version based on code from HWX
c  05.17.11 change r to rd to avoid conflicts with gcnst
c
      include 'commons.for'
      include 'comtrn.for'
      dimension h(9),d(7),e(9)
c
      parameter (h=
     & (/-19.572881d0,219.73999d0,-1015.3226d0,2471.01251d0,
     &  -3375.1717d0,2491.6597d0,-787.26086d0,14.085455d0,-.34664158d0
     &  /))
      parameter (d=
     & (/-1.181909d0,0.5031030d0,-0.6268461d0,0.5169312d0,
     &  -0.2351349d0,5.3980235d-02,-4.9069617d-03/))
      parameter (e=
     & (/4.018368d0,-4.239180d0,2.245110d0,-0.5750698d0,
     &  2.3021026d-02,2.5696775d-02,-6.8372749d-03,7.2707189d-04,
     & -2.9255711d-05/))
      etaMEO=0.0d0
C     constants used in the correlation
      av=6.0221415d23
      ak=8.314472d0/av
      amm=32.04216d0 !value used in the methanol EOS
      am=amm/1000.0d0/av
c
      tc=512.6d0
      rc=273.0d0

c     convert from mol/l to kg/m3
      rho=rhom*amm

c     0-density parameters
      da0p=.3408d-9
      tcep=577.87d0
      delt=0.4575d0

c     B-coefficient parameters, the same as those of 0-density
      da0s=.3408d-9
      tces=577.87d0

c     C-coefficient parameters
      da0t=.43d-9
      tcet=440.0d0
      da0t=da0p
      tcet=tcep

      vmc=da0p
c     vmc is defined as in the critical-like viscosity
      vmc=(6.0d0/3.14159265d0*amm/rc/av/1000.0d0)**(1.0d0/3.0d0)

      rd=rho  !in kg/m3
      rr=rho/rc
      tr=t/tc
      tres=t/tces
      trep=t/tcep
      tret=t/tcet
      cf=1.0d0/(1.0d0+exp(5.0d0*(rr-1.0d0)))

C     calculate the hard sphere diameter, da, eq.16
      da=0.0d0
      do kk=1,7
        da=da+d(kk)/tr**(kk-1)
      enddo
      do mm=1,9
        da=da+e(mm)*(rr)**(mm)
      enddo
      da=da*vmc

      rd=amm/rho/1.0d3
       ele0=1.16145d0/trep**.14874d0+.52487d0/exp(.77320d0*trep)
     & +2.16178d0/exp(2.43787d0*trep)
       eled=0.10225d0/trep**.97346d0+.10657d0/exp(.34528d0*trep)
     & -.44557d0/exp(2.58055d0*trep)
       ele0=ele0*(1.0d0+delt**2/(1.0d0+0.95976d-3*delt**6)*eled)
       u0=5.0d0/16.0d0/da0p**2*am**.5d0*ak**.5d0*(T/3.14159265d0)**.5d0
     &  /ele0

c      B-coefficient,subcript,s
       bnr=h(1)+h(2)/tres**.25d0+h(3)/tres**.5d0+h(4)/tres**.75d0
     &  +h(5)/tres +h(6)/tres**1.25d0+h(7)/tres**1.5d0+h(8)/tres**2.5d0
     &  +h(9)/tres**5.5d0
       bn=bnr*av*da0s**3

c      C-coefficient,subcript,t
      cetar=18.6222085d-4*EXP(9.990338d0/tret**0.5d0)*tret**3
      cetam=cetar*(av*da0t**3)**2
      paa0=u0*(1.0d0+bn*rho/amm*1000.0d0+cetam*(rho/amm*1000.0d0)**2)
      b=2.0d0*3.14159265d0*av*da**3/3.0d0
      ay=b/4.0d0/rd
      gd=(1.0d0 -.5d0*ay)/(1.0d0-ay)**3
      ue0=1.0d0/gd+0.8d0*b/rd+.761d0*gd*(b/rd)**2
      paa=ue0*u0
      paf=cf*paa0+(1.0d0-cf)*paa
      etaMEO=paf *(1.0d6)

      return
      end                                               !function ETAMEO
c
c ======================================================================
c
      function ETASF6 (icomp,t,rho)
      implicit double precision (a-h,o-z)
      DIMENSION a(1:5,0:1)
      ! t(K), rho (mol/l), etasf6 (upa.s)
      !  sf6 viscosity correlation from Altunin,V.V.
      ! Equation for the viscosity of sulfur hexafluoride for a broad range of
      ! temperatures and pressures, Tr. Mosk. Energ. Inst. 1984, 622, pp12-18
      ! note: copy is very poor and difficult to read the coefficients
      a(1,0)=0.911990483d0
      a(2,0)=-0.362880599d1
      a(3,0)=0.468880707d1
      a(4,0)=-0.204822413d1
      a(5,0)=0.299289894d0
      a(1,1)=-0.798662631d0
      a(2,1)=0.548041104d1
      a(3,1)=-0.615316823d1
      a(4,1)=0.258614144d1
      a(5,1)=-0.369541671d0
      ! convert from nol/l to kg/m3
      d=rho*146.0554192d0
      tau=t/318.7d0
      del=d/738
      eta0=0.486+0.17075d2*tau-0.13976d1*tau**2
      sumi=0.0d0
      do i=1,5
      do j=0,1
        sumi=sumi+(a(i,j)*del**(i))/tau**j
      end do
      end do
      etasf6=eta0*EXP(sumi)
      end                                               !function ETASF6
c
c ======================================================================
c
      function ETAD2 (icomp,t,d)
c
c  unpublished model for the viscosity of deuterium by scaling
c  the normal hydrogen correlation from symbolic regression-
c  Muzny, C.D., Huber, M.L., and Kazakov, A.F.,
c  "Correlation for the Viscosity of normal hydrogen obtained from symbolic regression"
c  submitted to J. Chem. Eng. Data, 2013
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c        d--molar density [mol/L]
c  output (as function value):
c     etad2--viscosity [uPa-s]
c
c  02-28-13   mlh, original
c
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
      DIMENSION sma(0:4), beta(6)
      DATA sma(0), sma(1), sma(2), sma(3), sma(4)
     &  / 0.209630d0,-0.455274d0,
     &  0.143602d0, -0.335325d-01, 0.276981d-02 /     ! coeff to match moldover
      DATA beta(1), beta(2), beta(3),beta(4), beta(5), beta(6)
     &    /0.118757d-05, 0.536373d0, 0.759423d+01, 0.615343d+01,
     &    -0.193070d+01, 0.011d0 /
        ts=t/30.41d0
        sum1=0.0d0
        do i=0,4
        sum1=sum1+sma(i)*(LOG(Ts))**i
        end do
        sigmast=EXP(sum1)
        etadil=0.021357*SQRT(2.01588*t)/(0.297*0.297*sigmast)
        etadil=etadil*SQRT(2.) !multiply dilute by sqrt(2) (mw ratio)
        beta1=0.6022137*(-0.187+2.4871/(t/30.41)+3.7151/(t/30.41)**2
     &  -11.0972/(t/30.41)**3+9.0965/(t/30.41)**4-3.8292/(t/30.41)**5
     &  +0.5166/(t/30.41)**6)*(0.297**3)
        eta1=etadil*beta1*d  !uPa.s
        rhokg=d*2.01588
        visa=etadil+eta1
      IF(rhokg.gt.0)then
        tred=t/33.145
        dred=rhokg*0.011
        ! nov8 machine 37 optimized by andrei
        diff2 = 6.43449673d-03*dred**2*exp(4.56334068d-02*tred +
     &  2.32797868d-01/tred + 9.58326120d-01*dred**2/(1.27941189d-01
     &  + tred) +    3.63576595d-01*dred**6)
        etares=1000*diff2*1.18  !factor added to match van itterbeek liquid
      else
        etares=0
      endif
      etad2=visa+etares
      END                                                !function ETAD2
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file trns_VIS.f
c ======================================================================
