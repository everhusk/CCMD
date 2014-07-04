c  begin file trns_TCX.f
c
c  This file contains the core routines for thermal conductivity
c
c  contained here are:
c     subroutine SETTC0 (nread,icomp,hcasno,ierr,herr)
c     function TCX0HC (icomp,t,rho,ierr,herr)
c     subroutine SETTC1 (nread,icomp,hcasno,ierr,herr)
c     function TCX1DG (icomp,t)
c     function TCX1BK (icomp,t,rho)
c     subroutine SETTC2 (nread,icomp,hcasno,ierr,herr)
c     function TCX2DG (icomp,t)
c     function TCX2BK (icomp,t,rho)
c     function TCX2CR (icomp,t,rho)
c     subroutine SETTC3 (nread,icomp,hcasno,ierr,herr)
c     function TCX3DG (icomp,t)
c     function TCX3BK (icomp,t,rho)
c     subroutine SETTC6 (nread,icomp,hcasno,ierr,herr)
c     subroutine SETTK1 (nread,icomp,hcasno,ierr,herr)
c     function TCX1CR (icomp,t,rho)
c     subroutine SETTK3 (nread,icomp,hcasno,ierr,herr)
c     function TCX3CR (icomp,t,rho)
c     subroutine SETTK4 (nread,icomp,hcasno,ierr,herr)
c     function TCX4CR (icomp,t,rho)
c     subroutine SETTC5 (nread,icomp,hcasno,ierr,herr)
c     function TCX5DG (icomp,t)
c     function TCX5BK (icomp,t,rho)
c     subroutine SETTK6 (nread,icomp,hcasno,ierr,herr)
c     function TCCNH3 (icomp,t,rho)
c     function TCCCH4 (icomp,t,rho)
c     FUNCTION TCXH2(ICOMP,T,D)
c       FUNCTION THERMX(DE,D0,THER)
c       FUNCTION DILT(ICOMP,T)
c       FUNCTION RCRIT(D,T)
c       FUNCTION RTHERM(ICOMP,DD,TIN)
c       FUNCTION CRITH2(D,T)
c       FUNCTION EXCSH2(ICOMP,D,T)
c       FUNCTION REXCES(D,T)
c     function TCXHE (icomp,t,rho)
c     function TCXETY (icomp,t,rho)
c     function TCXR23 (icomp,t,rho)
c     function TCXD2O (icomp,t,rho)
c     function TCXH2O (icomp,t,rho)
c     function TCCR_2010 (T,RHOmol,QD)
c     function TCXM1C (x,t,rho,ierr,herr)
c
c =====================================================================
c =====================================================================
c
      subroutine SETTC0 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid thermal conductivity model #0; the model
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
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
c  read data from file (should have been opened by SETUP)
c     write (*,*) ' SETTC0--read component',icomp,' from unit',nread
      write (herr,'(a12)') hcasno  !Use hcasno to avoid warning message
      read (nread,*) tmtcx(icomp)              !lower temperature limit
      read (nread,*) txtcx(icomp)              !upper temperature limit
      read (nread,*) pxtcx(icomp)              !upper pressure limit
      read (nread,*) Dxtcx(icomp)              !upper density limit
c
c  read in pointer to the hardcoded model
      read (nread,2003) htcxhc(icomp)
      read (nread,*) ndgnum(icomp),ndgden(icomp),
     &               nbknum(icomp),nbkden(icomp)
      jterm=0                                 !term counter
      nrsum=ndgnum(icomp)+ndgden(icomp)+nbknum(icomp)+nbkden(icomp)
      if (nrsum.ge.1) then
c  read in reducing parameters
        read (nread,*) trdbkt(icomp),Drdbkt(icomp),tcxbkt(icomp)
        do j=1,nrsum
          jterm=jterm+1
          read (nread,*) (ctcx(icomp,jterm,k),k=1,4),itcx(icomp,jterm)
        enddo
      end if
c
c  read in pointer to critical enhancement model
      read (nread,2003) htcxcr(icomp)
c     write (*,*) ' SETTC0--will use model ',htcxhc(icomp)
      ierr=0
      herr=' '
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETTC0
c
c ======================================================================
c
      function TCX0HC (icomp,t,rho,ierr,herr)
c
c  model for the hardcoded thermal conductivity models
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c        d--molar density [mol/L]
c  output (as function value):
c   tcx0hc--thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-29-00 EWL, original version
c  12-21-09 MLH, added D2, T2, HE3 models
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      herr=' '
      TCX0HC=0.0d0
      if (htcxhc(icomp).eq.'ETY') THEN
        TCX0HC=TCXETY(icomp,t,rho)
      elseif (htcxhc(icomp).eq.'R23') THEN
        TCX0HC=TCXR23(icomp,t,rho)
      elseif (htcxhc(icomp).eq.'D2O') THEN
        TCX0HC=TCXD2O(icomp,t,rho)
      elseif (htcxhc(icomp).eq.'H2O') THEN
        TCX0HC=TCXH2O(icomp,t,rho)
      elseif (htcxhc(icomp).eq.'HE') THEN
        TCX0HC=TCXHE(icomp,t,rho)
      elseif (htcxhc(icomp).eq.'H2') THEN
        TCX0HC=TCXH2(icomp,t,rho)
      elseif (htcxhc(icomp).eq.'D2') THEN
        IF(t.ge.tcrit(icomp))ratlam=0.031*rho*((tcrit(icomp)/t)**5)
     &    +1.0d0/SQRT(2.d0) !1/sqrt(2) is theoretical value for gas
        IF(t.lt.tcrit(icomp))ratlam=0.031*rho+1.0d0/SQRT(2.d0)
        IF(ratlam.gt.0.92)ratlam=0.92  !match Powers 1954 for liq
        TCX0HC=TCXH2(icomp,t,rho)*ratlam
      elseif (htcxhc(icomp).eq.'T2') THEN !scale to h2 with theoretical limit for gas
        TCX0HC=TCXH2(icomp,t,rho)/SQRT(3.0d0)
      elseif (htcxhc(icomp).eq.'HE3') THEN !scale to he4 with theoretical limit for gas
        TCX0HC=TCXHE(icomp,t,rho)/SQRT(3.0d0/4.0d0)
      else
        ierr=39
        herr='[TCX0HC error 39] unknown thermal conductivity model'//
     &       ' specified'//hnull
        call ERRMSG (ierr,herr)
      endif
      RETURN
      end                                               !function TCX0HC
c
c ======================================================================
c
      subroutine SETTC1 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid thermal conductivity model #1; this, the
c  "composite model," is written in a general form with terms designed
c  to include several recent correlations including those of Tufeu
c  (1984) for ammonia, Krauss (1996) for R152a, and Laesecke (1996) for
c  R123.
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
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-06-97  MM, original version
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
 1101   format ('[SETTC1 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTC1--read component',icomp,' from unit',nread
        read (nread,*) tmtcx(icomp)              !lower temperature limit
        read (nread,*) txtcx(icomp)              !upper temperature limit
        read (nread,*) pxtcx(icomp)              !upper pressure limit
        read (nread,*) Dxtcx(icomp)              !upper density limit
        jterm=0                                 !term counter
c  read the number of terms in the numerator and denominator of the
c  dilute-gas function
        read (nread,*) ndgnum(icomp),ndgden(icomp)
c       write (*,*) ' SETTC1--about to read ',ndgnum(icomp),' +',
c    &              ndgden(icomp),' dilute terms'
        if (ndgnum(icomp).ge.1) then
          read (nread,*) trddgt(icomp),tcxdgt(icomp)  !reducing par
          do j=1,ndgnum(icomp)  !read dilute-gas terms (numerator)
            jterm=jterm+1
            read (nread,*) ctcx(icomp,jterm,1),ctcx(icomp,jterm,2)
          enddo
        end if
        if (ndgden(icomp).ge.1) then
          do j=1,ndgden(icomp)  !read dilute-gas terms (denominator)
            jterm=jterm+1
            read (nread,*) ctcx(icomp,jterm,1),ctcx(icomp,jterm,2)
          enddo
        end if
c
c  read the number of terms in the numerator and denominator of the
c  background model; the coefficients themselves are given in the order:
c    constant multiplier; temperature exponent; density exponent; spare
c
        read (nread,*) nbknum(icomp),nbkden(icomp)
        nbksum=nbknum(icomp)+nbkden(icomp)
c       write (*,*) ' SETTC1--about to read ',nbknum(icomp),' +',
c    &              nbkden(icomp),' background terms'
        if (nbksum.ge.1) then
c  read in reducing parameters
          read (nread,*) trdbkt(icomp),Drdbkt(icomp),tcxbkt(icomp)
          if (nbknum(icomp).ge.1) then
            do j=1,nbknum(icomp)    !numerator of rational polynomial
              jterm=jterm+1
              read (nread,*) (ctcx(icomp,jterm,k),k=1,4)
            enddo
          end if
          if (nbkden(icomp).ge.1) then
            do j=1,nbkden(icomp)    !denominator of rational poly
              jterm=jterm+1
              read (nread,*) (ctcx(icomp,jterm,k),k=1,4)
            enddo
          end if
        end if
c
c  read in pointer to critical enhancement model
        read (nread,2003) htcxcr(icomp)
c       write (*,*) ' SETTC1--will use critical model ',htcxcr(icomp)
c
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETTC1
c
c ======================================================================
c
      function TCX1DG (icomp,t)
c
c  dilute-gas contribution to the thermal conductivity by the
c  composite model (TC1)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   tcx1dg--the dilute-gas part of the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-06-97  MM, original version
c  03-28-97  MM, compute tau only if d.g. terms exist
c  06-08-97  MM, add special term; power = -99: mult by (1 + coeff*(Cp0-2.5R))
c  11-17-98  MM, nterm not incremented properly, denominator not correct--
c                appears to affect only CO2
c  11-10-99 EWL, add special term; power = -96
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      tau=1.0d0
      i=icomp
      if ((ndgnum(i)+ndgden(i)).ge.1) then
c  compute tau only if dilute-gas terms exist, otherwise treddg may
c  not be defined
        tau=t/trddgt(icomp)
c       write t(*,*) 'TCX1DG--tred,tau:  ',trddgt(icomp),tau
      end if
c
      nterm=0                                    !term counter
c  sum the dilute-gas terms, first numerator then denominator
      tcx1dg=0.0d0
      if (ndgnum(i).ge.1) then
        do j=nterm+1,nterm+ndgnum(i)
          if (ctcx(i,j,2).lt.-90.0d0) then
            if (ABS(ctcx(i,j,2)+99.0d0).lt.1.0d-6) then
c  flag:  exponent -99 indicates: multiply numerator term by
c  [1 + coeff*(Cp0 - 2.5*R)]; this is the Cv_internal as used by
c  Vesovic, et al. (1990) for carbon dioxide
              cint=CP0K(icomp,t)-2.5d0*R
              cp01=1.0d0+ctcx(i,j,1)*cint
c           write (*,*) ' TCX1DG--will mult by Cp0 - 2.5*R: ',cint
              tcx1dg=tcx1dg*cp01
            elseif (ABS(ctcx(i,j,2)+98.0d0).lt.1.0d-6) then
c  flag:  exponent -98 indicates: add Cv0*eta0 to numerator
              call ETAK0(icomp,t,eta,ierr,herr)
              cint=CP0K(icomp,t)-R
              tcx1dg=tcx1dg+ctcx(i,j,1)*cint*eta
            elseif (ABS(ctcx(i,j,2)+97.0d0).lt.1.0d-6) then
c  flag:  exponent -97 indicates: add eta0 to numerator
              call ETAK0(icomp,t,eta,ierr,herr)
              tcx1dg=tcx1dg+ctcx(i,j,1)*eta
            elseif (ABS(ctcx(i,j,2)+96.0d0).lt.1.0d-6) then
c  flag:  exponent -96 indicates: add fint*eta0/M*(Cp0-2.5R)+15/4*R*eta0/M
c         where fint was created by the previous terms.
              cint=CP0K(icomp,t)/R-2.5d0
              call ETAK0(icomp,t,eta,ierr,herr)
              fint=tcx1dg
              tcx1dg=(fint*cint+15.0d0/4.0d0)*R*eta/wmas(i)
            endif
          else
            tcx1dg=tcx1dg+ctcx(i,j,1)*tau**ctcx(i,j,2)
c         write (*,*) ' TCX1DG--j,tcx_dg(j): ',j,tcx1dg
          end if
        enddo
        nterm=nterm+ndgnum(i)
      end if
      if (ndgden(i).ge.1) then
        denom=0.0d0
        do j=nterm+1,nterm+ndgden(i)
c         write (*,*) 'denom coeff:  ',ctcx(i,j,1),ctcx(i,j,2)
          denom=denom+ctcx(i,j,1)*tau**ctcx(i,j,2)
        enddo
c       write (*,*) ' TCX1DG--num,denom: ',tcx1dg,denom
c  divide numerator by denominator
        tcx1dg=tcx1dg/denom
      end if
c
c  multiply by reducing parameter (to convert units, etc.)
      tcx1dg=tcx1dg*tcxdgt(i)
c
c     write (*,1000) icomp,tau,tcx1dg
c1000 format (' TCX1DG--icomp,tau,dilute-gas tc:',i10,d14.6,14x,d14.6)
      RETURN
      end                                               !function TCX1DG
c
c ======================================================================
c
      function TCX1BK (icomp,t,rho)
c
c  background contribution to the thermal conductivity by the
c  composite model (TC1)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx1bk--the background part of the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-06-97  MM, original version
c  03-28-97  MM, compute tau,del only if residual terms exist
c  02-17-99 EWL, add nbknum(i) to the "DO" statement for the denominator loop
c  01-26-00 EWL, add special terms for methane
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      tau=1.0d0
      del=1.0d0
      if ((nbknum(i)+nbkden(i)).ge.1) then
c  compute tau only if residual terms exist, otherwise tredbk, Dredbk may
c  not be defined
        tau=t/trdbkt(i)
        del=rho/Drdbkt(i)
      end if
      nterm=ndgnum(i)+ndgden(i)                        !term counter
c     write (*,*) ' TCX1BK--tau,del,coeff_1: ',tau,del,ctcx(i,nterm+1,1)
c
c  sum the background terms, first numerator then denominator
      tcx1bk=0.0d0
      if (nbknum(i).ge.1) then
        do j=nterm+1,nterm+nbknum(i)
          if (ctcx(i,j,4).lt.-90) then                      !Special cases
            if (ABS(ctcx(i,j,4)+99.0d0).lt.1.0d-6) then     !Methane
              if (t.lt.trdbkt(i) .and. rho.lt.Drdbkt(i)) then
                th = (1.0d0-t/trdbkt(i))**(1.0d0/3.0d0)
                dsat=-1.8802840d0*th**1.062d0 -2.8526531d0*th**2.500d0
     &               -3.0006480d0*th**4.500d0 -5.2511690d0*th**7.500d0
     &               -13.191869d0*th**12.50d0 -37.553961d0*th**23.50d0
                tcx1bk=tcx1bk/EXP(dsat)
              endif
            endif
          else
            hexp=1
            if (abs(ctcx(i,j,4)).gt.1.d-20) hexp=EXP(-del**ctcx(i,j,4))
            tcx1bk=tcx1bk
     &            +ctcx(i,j,1)*tau**ctcx(i,j,2)*del**ctcx(i,j,3)*hexp
          endif
        enddo
      end if
      if (nbkden(i).ge.1) then
        denom=0.0d0
        if (del.gt.0.0d0) then
          do j=nterm+nbknum(i)+1,nterm+nbknum(i)+nbkden(i)
            denom=denom+ctcx(i,j,1)*tau**ctcx(i,j,2)*del**ctcx(i,j,3)
          enddo
        endif
c  divide numerator by denominator
        if (abs(denom).gt.1.d-20) tcx1bk=tcx1bk/denom
      end if
c
c  multiply by reducing parameter (to convert units, etc.)
      tcx1bk=tcx1bk*tcxbkt(i)
c
c     write (*,1000) icomp,tau,del,tcx1bk
c1000 format (' TCX1BK--icomp,tau,del,background tc:  ',i4,3d14.6)
      RETURN
      end                                               !function TCX1BK
c
c ======================================================================
c
      subroutine SETTC2 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid thermal conductivity model #2--the hydrocarbon
c  model of Younglove and Ely, JPCRD 16:577-798 (1987)
c
c  N.B.  Younglove and Ely use a special scaled equation of state to
c        compute derivatives for the critical enhancement; the default
c        EOS is used here
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
c                       39 = error--model not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-18-96  MM, original version (dummy placeholder)
c  10-16-96  MM, implement model of Younglove & Ely
c  08-19-97  MM, change error number for nread<=0; input hcasno is not array
c  09-00-00 EWL, change hmdci to hmdtcx and hmdeta to avoid overlapping vis.
c  03-08-00 EWL, do not read in enhancement info if pointer=NUL
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
 1101   format ('[SETTC2 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTC2--read component',icomp,' from unit',nread
        read (nread,*) tmtcx(icomp)              !lower temperature limit
        read (nread,*) txtcx(icomp)              !upper temperature limit
        read (nread,*) pxtcx(icomp)              !upper pressure limit
        read (nread,*) Dxtcx(icomp)              !upper density limit
        read (nread,2003) hmdtcx(icomp)         !pointer to omega model
        read (nread,*) ctcx(icomp,1,1)          !L-J sigma
        read (nread,*) ctcx(icomp,2,1)          !L-J epsilon/kappa
c  read constant in Eq 19 = 5/16*(k*MW/1000/pi/Na)**0.5*1.0d12
c  the factor of 1d12 is for sigma in nm and viscosity in micro-Pa-s
        read (nread,*) ctcx(icomp,3,1) !const in Eq 19
        do i=4,5
          read (nread,*) ctcx(icomp,i,1) !dilute-gas terms Gt(1)-Gt(2)
        enddo
        do i=6,13
          read (nread,*) ctcx(icomp,i,1) !background terms Et(1)-Et(8)
        enddo
c  read in pointer to critical enhancement model
        read (nread,2003) htcxcr(icomp)
        if (htcxcr(icomp).ne.'NUL') then
c       write (*,*) ' SETTC2--will use critical model ',htcxcr(icomp)
c  in the case of TC2, the critical enhancement is integral with the
c  model for the dilute gas and background contributions
        do i=14,17
          read (nread,*) ctcx(icomp,i,1) !critical enhancement X(1)-X(4)
        enddo
        read (nread,*) ctcx(icomp,18,1)!critical enhancement:  Z
        read (nread,*) ctcx(icomp,19,1)!critical enhancement:  k
c  following coefficients are for the viscosity function of Younglove &
c  Ely, which is also used in the thermal conductivity
        do i=20,23
          read (nread,*) ctcx(icomp,i,1) !initial rho terms:  Fv(1)-Fv(4)
        enddo
        do i=24,30
          read (nread,*) ctcx(icomp,i,1) !residual viscosity:  Ev(1)-Ev(7)
        enddo
        endif
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETTC2
c
c ======================================================================
c
      function TCX2DG (icomp,t)
c
c  dilute-gas contribution to the thermal conductivity by the
c  model of Younglove and Ely, JPCRD 16:577-798 (1987); Eqs 19, 27.
c
c  N.B. there are two terms missing from Eq 27 in the JPCRD article;
c       the 15R/4 is missing and a factor of 1/(mol wt) is needed to
c       convert units
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   tcx2dg--the dilute gas part of the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-16-96 MM, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-26-02 EWL, change Cp0 to Cp00 to avoid confusion with function Cp0
c
      include 'commons.for'
      include 'comtrn.for'
c
c  compute dilute-gas viscosity
      sigma=ctcx(icomp,1,1)
      epsk=ctcx(icomp,2,1)
      eta0=ctcx(icomp,3,1)*SQRT(t)/(OMEGA(icomp,t,epsk,hmdtcx(icomp))
     &    *sigma**2)
      Cp00=CP0K(icomp,t)    !ideal gas heat capacity by chosen EOS
      Gt1=ctcx(icomp,4,1)
      Gt2=ctcx(icomp,5,1)
      TCX2DG=1.0d-3*eta0/wmas(icomp)*(3.75d0*R+
     &       (Cp00-2.5d0*R)*(Gt1+Gt2*epsk/t))
c     write (*,1001) t,Cp0,TCX2ID
c1001 format (1x,' TCX2DG--t,Cp0,tcx_ideal: ',2f10.2,f12.6)
c
      RETURN
      end                                               !function TCX2DG
c
c ======================================================================
c
      function TCX2BK (icomp,t,rho)
c
c  background contribution to the thermal conductivity by the
c  model of Younglove and Ely, JPCRD 16:577-798 (1987); Eqs 26, 28-30.
c
c  N.B. the powers given in the JPCRD article are incorrect for Eqs 29
c       and 30; they should be (4 - n) and (7 - n), respectively; there
c       is an incorrect sign in Eq 26 [(1 + F2*rho), not (1 - F2*rho)]
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx2bk--the background part of the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-16-96 MM, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  11-21-07 MLH, remove unused commons and declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
c  compute functions given as Eqs 28-30
      tinv=1.0d0/t
      tinv2=tinv*tinv
      F0=ctcx(icomp,6,1)+ctcx(icomp,7,1)*tinv+ctcx(icomp,8,1)*tinv2
      F1=ctcx(icomp,9,1)+ctcx(icomp,10,1)*tinv+ctcx(icomp,11,1)*tinv2
      F2=ctcx(icomp,12,1)+ctcx(icomp,13,1)*tinv
c  background term is given by Eq 26 in Younglove & Ely
      TCX2BK=(F0+F1*rho)*rho/(1.0d0+F2*rho)
c     write (*,1001) t,rho,TCX2RS
c1001 format (1x,' TCX2RS--t,rho,tcx_resid: ',f10.2,f10.4,f12.6)
c
      RETURN
      end                                               !function TCX2BK
c
c ======================================================================
c
      function TCX2CR (icomp,t,rho)
c
c  critical enhancement to the thermal conductivity by the model of
c  Younglove and Ely, JPCRD 16:577-798 (1987); Eqs D1-D4
c
c  N.B.  there are numerous errors in the equations presented in the
c        Younglove & Ely paper; the present code is derived from the
c        code of Younglove used to generate the tables in JPCRD
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx2cr--the critical enhancement part of the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-28-96  MM, original version
c  11-14-97  MM, return zero if rho = 0 (avoid division by zero)
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  09-00-00 EWL, remove del and tau before alphar derivatives
c
      include 'commons.for'
      include 'comtrn.for'
c
c  the array ctcx contains, in order:
c       1-2:  Lennard-Jones sigma and epsilon/kappa
c         3:  constant in Eq 19 (5/16*(k*MW/1000/pi/Na)**0.5*1.0d12)
c       4-5:  dilute gas terms, Gt(1), Gt(2)
c      6-13:  background terms, Et(1) - Et(8)
c     14-17:  critical enhancement terms, X(1) - X(4)
c        18:  critical enhancement term  Z
c        19:  Boltzmann's constant, k
c  and the following terms from the Younglove & Ely viscosity model:
c     20-23:  initial density dependence terms, Fv(1) - Fv(4)
c     24-30:  residual viscosity terms, Ev(1) - Ev(7)
c
c  if density is approx zero, return zero for the critical enhancement
c  (avoid division by zero)
      if (rho.lt.1.0d-6) then
        TCX2CR=0.0d0
        RETURN
      end if
c
c  find derivatives dP/dD and dP/dT
      tau=tz(icomp)/t
      del=rho/rhoz(icomp)
      phi01=PHIK(icomp,0,1,tau,del)  !real-gas terms
      phi02=PHIK(icomp,0,2,tau,del)
      phi11=PHIK(icomp,1,1,tau,del)
c  factor 1.0d3 in next 3 lines converts kPa -> Pa
      dpdrho=R*t*(1.0d0+2.0d0*phi01+phi02)*1.0d3
      dpt=R*rho*(1.0d0+phi01-phi11)*1.0d3
      pc2=pcrit(icomp)*1.0d3
      xi=0.0d0
      if (dpdrho.gt.0.0d0)
     &xi=(pc2*rho/(Dcrit(icomp)**2*dpdrho))**ctcx(icomp,16,1)    !Eq D3
      dellam=ctcx(icomp,17,1)*ctcx(icomp,19,1)/pc2
     &      *(t*dpt*Dcrit(icomp)/rho)**2*xi                        !Eq D2
      delt=ABS(t-tcrit(icomp))/tcrit(icomp)
      delD=ABS(rho-Dcrit(icomp))/Dcrit(icomp)
      eterm=ctcx(icomp,14,1)*delt**4+ctcx(icomp,15,1)*delD**4
c  check that exponential term will not result in underflow
      if (eterm.gt.500.0d0) then
        eterm=EXP(-500.0d0)
      else
        eterm=EXP(-eterm)
      end if
c  now compute the viscosity, first the dilute gas contribution
      sigma=ctcx(icomp,1,1)
      epsk=ctcx(icomp,2,1)
      eta0=ctcx(icomp,3,1)*SQRT(t)/(OMEGA(icomp,t,epsk,hmdtcx(icomp))
     &    *sigma**2)
c  initial density term for viscosity
      eta1=rho*(ctcx(icomp,20,1)+ctcx(icomp,21,1)
     &    *(ctcx(icomp,22,1)-LOG(t/ctcx(icomp,23,1)))**2)         !Eq 21
c  now compute the residual viscosity (viscosity minus the dilute gas
c  and initial density terms)
      G=ctcx(icomp,24,1)+ctcx(icomp,25,1)/t                       !Eq 23
      H=SQRT(rho)*(rho-Dcrit(icomp))/Dcrit(icomp)                   !Eq 25
      F=G+(ctcx(icomp,26,1)+ctcx(icomp,27,1)*t**(-1.5d0))*rho**0.1d0+
     &  (ctcx(icomp,28,1)+ctcx(icomp,29,1)/t+ctcx(icomp,30,1)/(t*t))*H
      eta2=EXP(F)-EXP(G)                                          !Eq 22
      visc=(eta0+eta1+eta2)*1.0d-6       !factor of d-6 converts to Pa-s
c     write (*,1060) t,rho,eta0,eta1,eta2,visc
c1060 format (1x,' TCX2CR: t,rho,eta0,1,2,visc:  ',f8.3,f10.6,4e14.6)
c
c  combine all of the above to arrive at the critical enhancement
c  (the denominator is missing from the Younglove & Ely paper,
      TCX2CR=dellam*eterm/(6.0d0*3.141592654d0*ctcx(icomp,18,1)*visc)
c
      RETURN
      end                                               !function TCX2CR
c
c ======================================================================
c
      subroutine SETTC3 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid thermal conductivity model #3; the model of:
c  Younglove, B.A. (1982). Thermophysical properties of
c  fluids. I. Argon, ethylene, parahydrogen, nitrogen, nitrogen trifluoride,
c  and oxygen. J. Phys. Chem. Ref. Data, Volume 11, Supplement 1.
c
c  N.B.  Younglove and Ely use a special scaled equation of state to
c        compute derivatives for the critical enhancement; the default
c        EOS is used here
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
c  06-18-96  MM, original version
c  08-19-97  MM, error for nread<=0; input hcasno is not array
c  06-30-98 EWL, implement model of Younglove
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
 1101   format ('[SETTC3 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTC3--read component',icomp,' from unit',nread
        read (nread,*) tmtcx(icomp)              !lower temperature limit
        read (nread,*) txtcx(icomp)              !upper temperature limit
        read (nread,*) pxtcx(icomp)              !upper pressure limit
        read (nread,*) Dxtcx(icomp)              !upper density limit
        read (nread,*) ctcx(icomp,1,1)          !L-J sigma
        read (nread,*) ctcx(icomp,2,1)          !L-J epsilon/kappa
        read (nread,*) ctcx(icomp,3,1)          !leading coefficient
        read (nread,*) ctcx(icomp,3,2)          !exponent on tau
        do j=4,12        !read dilute gas thermal conductivity terms
          read (nread,*) ctcx(icomp,j,1)
        enddo
        do j=13,24       !read residual thermal conductivity terms
          read (nread,*) ctcx(icomp,j,1)
        enddo
        read (nread,*) ctcx(icomp,25,1)         !F
        read (nread,*) ctcx(icomp,26,1)         !rm
c  read in pointer to critical enhancement model
        read (nread,2003) htcxcr(icomp)
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETTC3
c
c ======================================================================
c
      function TCX3DG (icomp,t)
c
c  dilute-gas contribution to the thermal conductivity by the model of:
c  Younglove, B.A. (1982). Thermophysical properties of
c  fluids. I. Argon, ethylene, parahydrogen, nitrogen, nitrogen trifluoride,
c  and oxygen. J. Phys. Chem. Ref. Data, Volume 11, Supplement 1.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   tcx3dg--the dilute-gas part of the thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  06-30-98 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
c
      i=icomp
      tau=t
      omgsum=0.0d0
      ekt3=(ctcx(i,2,1)/t)**(1.0d0/3.0d0)
      do j=4,12
        omgsum=omgsum+ctcx(i,j,1)*ekt3**(7-j)
      enddo
      omgsum=1.0d0/omgsum
      tcx3dg=ctcx(i,3,1)*tau**ctcx(i,3,2)/(ctcx(i,1,1)**2*omgsum)
c     write (*,*) ' TCX3DG--dilute-gas thermal conductivity:  ',tcx3dg
c
      RETURN
      end                                               !function TCX3DG
c
c ======================================================================
c
      function TCX3BK (icomp,t,rho)
c
c  residual contribution to the thermal conductivity by the model of:
c  Younglove, B.A. (1982). Thermophysical properties of
c  fluids. I. Argon, ethylene, parahydrogen, nitrogen, nitrogen trifluoride,
c  and oxygen. J. Phys. Chem. Ref. Data, Volume 11, Supplement 1.
c
c  Although this correlation has a separate initial density term,
c  the initial density term is combined with the residual term.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx3bk--the background part of the thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  06-30-98 EWL, original version
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr

      tcx3bk=0.0d0
      if (rho.le.0.d0) RETURN
c
c  initial density term for thermal conductivity
      tcx1=rho*(ctcx(icomp,13,1)+ctcx(icomp,14,1)
     &    *(ctcx(icomp,15,1)-LOG(t/ctcx(icomp,16,1)))**2)         !Eq 21
c  now compute the residual thermal conductivity (thermal conductivity minus
c  the dilute gas and initial density terms)
      G=ctcx(icomp,17,1)+ctcx(icomp,18,1)/t                       !Eq 23
      H=SQRT(rho)*(rho-ctcx(icomp,24,1))/ctcx(icomp,24,1)         !Eq 25
      F=G+(ctcx(icomp,19,1)+ctcx(icomp,20,1)*t**(-1.5d0))*rho**0.1d0+
     &  (ctcx(icomp,21,1)+ctcx(icomp,22,1)/t+ctcx(icomp,23,1)/(t*t))*H
      tcx2=EXP(F)-EXP(G)                                          !Eq 22
c
      call DPDDK (icomp,t,rho,dpdrho)
      call DPDTK (icomp,t,rho,dpt)
      dpdrho=dpdrho/wmas(icomp)*1.0d7
      dpt=dpt*1.0d4
c
      ff=ctcx(icomp,25,1)
      rm=ctcx(icomp,26,1)
      an=6.0225D+23
      bk=1.38054D-16
      dd=rho*wmas(icomp)/1000.0d0
      call ETAK (icomp,t,rho,eta,ierr,herr)
      bl=ff*(rm**5*dd*an/wmas(icomp)*ctcx(icomp,2,1)/t)**0.5d0
      y=6.0d0*3.1415927d0*eta/100000.d0*bl
     &    *(bk*t*dd*an/wmas(icomp))**0.5d0
      dl=0.0d0
      if (dpdrho.ge.0) dl=bk*(t*dpt)**2/(dd*dpdrho)**0.5d0/y
      tcx3=dl*EXP(-4.25d0*((rho-Dcrit(icomp))/Dcrit(icomp))**4
     &    -18.66d0*((t-tcrit(icomp))/tcrit(icomp))**2)/1.0d5
      TCX3BK=tcx1+tcx2+tcx3
c     write (*,*) ' TCX3BK--residual thermal conductivity:    ',tcx3bk
c
      RETURN
      end                                               !function TCX3BK

c ======================================================================
c
      subroutine SETTC5 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid thermal conductivity model #5; the model of:
c  T-H. Chung, M. Ajlan, L.L. Lee, and K.E. Starling
c  "Generalized Multiparameter Correlation for Nonpolar and Polar Fluid Transport Properties"
c  Ind. Eng. Chem. Res. 1988, 27, 671-679. Also see Reid, Prausnitz and Poling Chapter 9.
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
c  11-19-07  MLH, original
c  01-04-08  MLH, allow dilute and residual parameters to be independent
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
 1101   format ('[SETTC5 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTC5--read component',icomp,' from unit',nread
        read (nread,*) tmtcx(icomp)              !lower temperature limit
        read (nread,*) txtcx(icomp)              !upper temperature limit
        read (nread,*) pxtcx(icomp)              !upper pressure limit
        read (nread,*) Dxtcx(icomp)              !upper density limit
        read (nread,*) sigchk(icomp)            !L-J sigma for chung k model
        read (nread,*) epschk(icomp)            !L-J epsilon/kappa for chung k model
        read (nread,*) acchk(icomp),ddipk(icomp),hbkk(icomp) !w, dip, kappa for chung dilute
c       set residual parameters
        cctcx(icomp,1,1)=sigchk(icomp)
        cctcx(icomp,1,2)=epschk(icomp)
        cctcx(icomp,1,3)=acchk(icomp)
        cctcx(icomp,1,4)=ddipk(icomp)
        cctcx(icomp,1,5)=hbkk(icomp)

        READ (nread,*) naddk(icomp)             !additional factors for chung;not presently used
c       use these terms if residual parameters are not the same as dilute gas
        jtc=0
        do j =1, naddk(icomp)
         jtc=jtc+1
         READ(nread,*) cctcx(icomp,jtc,1),cctcx(icomp,jtc,2),
     &    cctcx(icomp,jtc,3),cctcx(icomp,jtc,4),cctcx(icomp,jtc,5)
        end do

c  read in pointer to critical enhancement model
        read (nread,2003) htcxcr(icomp)
        ierr=0
        herr=' '
      end if
c
      RETURN
 2003 format (a3)
      end                                             !subroutine SETTC5
c
c ======================================================================
c
      function TCX5DG (icomp,t)
c
c  dilute-gas contribution to the thermal conductivity by the model of:
c  T-H. Chung, M. Ajlan, L.L. Lee, and K.E. Starling
c  "Generalized Multiparameter Correlation for Nonpolar and Polar Fluid Transport Properties"
c  Ind. Eng. Chem. Res. 1988, 27, 671-679. Also see Reid, Prausnitz and Poling Chapter 9.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   tcx5dg--the dilute-gas part of the thermal conductivity [W/m-K]
c
c   11-19-07 MLH  original version
c   01-04-08 MLH  added additional slots to cctcx
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      i=icomp
      r=8.314472D0
c     tstar=t/epschk(i)
c     vc = (sigchk(i)*10.0D0/0.809D0)**3 !compute vc in cm3/mol from Chung sigma
      call CVCPK (i,t,rho,cv,cp)
      alpha=cv/r-1.5D0
      beta=0.7862D0 -0.7109d0*acchk(i)+1.3168D0*acchk(i)**2
      tr = t/(1.2593d0*epschk(i))
      Z=2.0D0+10.5d0*tr**2
      psich=1.0D0+alpha*
     &    ((0.215D0+0.28288*alpha-1.061d0*beta+0.26665d0*Z)/
     &    (0.6366D0 +beta*Z+1.061D0*alpha*beta))
      call ETAK0(i,t,eta00,ierr,herr)
      eta00= eta00*1.0d-6   !convert from uPa.s to Pa.s
      tcx5dg=3.75D0*psich*eta00*r/(wmas(i)/1000.D0)
      tcx0ch=tcx5dg
c     write (*,*) ' TCX5DG--dilute-gas thermal conductivity:  ',tcx5dg
c
      RETURN
      end                                               !function TCX5DG
c
c ======================================================================
c
      function TCX5BK (icomp,t,rho)
c
c  residual contribution to the thermal conductivity by the model of:
c  T-H. Chung, M. Ajlan, L.L. Lee, and K.E. Starling
c  "Generalized Multiparameter Correlation for Nonpolar and Polar Fluid Transport Properties"
c  Ind. Eng. Chem. Res. 1988, 27, 671-679. Also see Reid, Prausnitz and Poling Chapter 9.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx5bk--the background part of the thermal conductivity [W/m-K]
c
c  note: this version allows the parameters for the residual piece to
c  be independent from the dilute gas and fit as free parameters
c  nomenclature for comparison with manuscript
c        sigchk(i)=cctcx(i,1,1)
c        epschk(i)=cctcx(i,1,2)
c        acchk(i)=cctcx(i,1,3)
c        ddipk(i)=cctcx(i,1,4)
c        hbkk(i)=cctcx(i,1,5)
c  11-19-07 MLH, original
c  01-04-08 MLH  added additional slots cctcx
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension B0(7), B1(7), B2(7), B3(7), BB(7)
      parameter (B0=(/2.41657D0, -0.50924D0, 6.61069D0, 14.5425D0,
     &                0.79274D0, -5.8634D0, 81.171D0 /))
      parameter (B1=(/0.74824D0, -1.50936D0, 5.62073D0, -8.91387D0,
     &                0.82019D0, 12.8005D0, 114.158D0 /))
      parameter (B2=(/-0.91858D0, -49.9912D0, 64.7599D0, -5.63794D0,
     &                -0.69369D0, 9.58926D0, -60.841D0 /))
      parameter (B3=(/121.721D0, 69.9834D0, 27.0389D0, 74.3435D0,
     &                6.31734D0, -65.52920D0, 466.775D0 /))

      r=8.314472D0
      i=icomp
      tcx5bk=0.0d0
      rho0 = 0.0d0
      if (rho.le.0.d0) RETURN
c     tstar=t/cctcx(i,1,2)
      vc = (cctcx(i,1,1)*10.0D0/0.809D0)**3 !compute vc in cm3/mol from Chung sigma

      do j=1,7
        BB(J)=B0(j)+B1(j)*cctcx(i,1,3)+B2(j)*cctcx(i,1,4)**4
     &       +B3(j)*cctcx(i,1,5)
      end do
c
      y = rho * vc/ 6.0D3
      g1=(1.0D0 -0.5D0*y)/((1.0D0-y)**3)
      g2 =( (BB(1)/y)*(1.0D0- EXP(-BB(4)*y)) +BB(2)*g1*EXP(BB(5)*y)+
     &       BB(3)*g1 )/ ( BB(1)*BB(4) + BB(2)+ BB(3) )
      call CVCPK (i,t,rho0,cv,cp)
      alpha=cv/r-1.5D0
      beta=0.7862D0 -0.7109*cctcx(i,1,3)+1.3168D0*cctcx(i,1,3)**2
      tr = t/(1.2593*cctcx(i,1,2))
      Z=2.0D0+10.5*tr**2
      psich=1.0D0+
     &    alpha*((0.215D0+0.28288d0*alpha-1.061d0*beta+0.26665*Z)/
     &    (0.6366D0 +beta*Z+1.061D0*alpha*beta))
      call ETAK0(i,t,eta00,ierr,herr)
      eta00=eta00*1.0d-6               !convert from uPa.s to Pa.s
      qq= 3.586d-3*
     &    SQRT(1.2593d0*cctcx(i,1,2)/(wmas(i)/1.0d3))/(vc**(2./3.))
      tcx5Bk=31.2*eta00 *psich*(1.0D0/g2+BB(6)*y)/(wmas(i)/1000.0D0)+
     &    qq*BB(7)*y*y*g2*SQRT(tr)
      tcx5bk=tcx5bk - tcx0ch
c     write (*,*) ' TCX5BK--residual thermal conductivity:    ',tcx5bk
c
      RETURN
      end                                               !function TCX5BK
c
c ======================================================================
c
      subroutine SETTC6 (nread,icomp,hcasno,ierr,herr)
c
c  initialize pure fluid thermal conductivity model #6
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
c                       39 = error--model not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  08-19-97  MM, error for nread<=0; input hcasno is not array
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
 1101   format ('[SETTC6 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
        htcxcr(icomp)='NUL'
        ierr=39
        herr='[SETUP error 39] thermal conductivity model #6 specified '
     &     //'in fluid file but not implemented in code.'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                             !subroutine SETTC6
c
c ======================================================================
c
      subroutine SETTK1 (nread,icomp,hcasno,ierr,herr)
c
c  initialize model #1 for the thermal conductivity critical enhancement--
c  the empirical model used by Perkins and Laesecke
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
c                        1 = error
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-24-97  MM, original version
c  03-27-97  MM, change powers in exp term to integer
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=39
        write (herr,1039) hcasno,hnull
        call ERRMSG (ierr,herr)
 1039   format ('[SETUP error 39] block data option for t.c. crit ',
     &          'model #1 specified for CAS # ',a12,' but not ',
     &          'implemented in code.',a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTK1--read component',icomp,' from unit',nread
        read (nread,*) tmtck(icomp)              !lower temperature limit
        read (nread,*) txtck(icomp)              !upper temperature limit
        read (nread,*) pxtck(icomp)              !upper pressure limit
        read (nread,*) Dxtck(icomp)              !upper density limit
        jterm=0                                 !term counter
c  # terms in numerator & denominator of polynomial multiplier,
c  exponential term, and spare for future use
        read (nread,*) nnumtck(icomp),ndentck(icomp),nexptck(icomp),
     &                 nsparek(icomp)
        if (nnumtck(icomp)+ndentck(icomp).ge.1) then  !read reducing pars
          read (nread,*) trtck(icomp),Drtck(icomp),tcxred(icomp)
          if (nnumtck(icomp).ge.1) then
            do j=1,nnumtck(icomp)              !read numerator terms
              jterm=jterm+1
              read (nread,*) (ctck(icomp,jterm,k),k=1,5),
     &                       itck(icomp,jterm,0)
            enddo
          end if
          if (ndentck(icomp).ge.1) then
            do j=1,ndentck(icomp)              !read denominator terms
              jterm=jterm+1
              read (nread,*) (ctck(icomp,jterm,k),k=1,5),
     &                       itck(icomp,jterm,0)
            enddo
          end if
        end if
        if (nexptck(icomp).ge.1) then
          read (nread,*) tredex(icomp),Dredex(icomp)
          do j=1,nexptck(icomp)                !read exponential terms
            jterm=jterm+1
            read(nread,*)(ctck(icomp,jterm,k),k=1,2),itck(icomp,jterm,2)
     &                    ,ctck(icomp,jterm,3),itck(icomp,jterm,3)
     &                    ,itck(icomp,jterm,0)
          enddo
        end if
c       if (nsparek(icomp).ge.1) then
c         do j=1,nsparek(icomp)              !read spare terms
c           jterm=jterm+1
c           read(nread,*)(ctck(icomp,jterm,k),k=1,1),itck(icomp,jterm,1)
c         enddo
c       end if
      end if
c
      RETURN
      end                                             !subroutine SETTK1
c
c ======================================================================
c
      function TCX1CR (icomp,t,rho)
c
c  model #1 for the thermal conductivity critical enhancement--
c  the empirical model used by Perkins and Laesecke
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx1cr--the critical enhancement to the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-24-97  MM, original version
c  03-27-97  MM, change powers in exp term to integer
c  01-25-00 EWL, set tcx1cr=0 if xexp<-500
c
      include 'commons.for'
      include 'comtrn.for'
c
c  compute the various parts of the critical enhancement
c  these are taken in the order:
c    rational polynomial in T, rho (first numerator, then denominator)
c    exponential term
c    spare for future use
c  the coefficients themselves are given in the order:
c    constant multiplier;
c    additive term to temperature, exponent for (T + Tadd)
c    additive term to density, exponent for (D + Dadd)
c    spare 1 [if = 99 for denominator take MAX(T, T+Tadd]
c    spare 2
c
      i=icomp
      nterm=0                                 !term counter
      tcx1cr=0.0d0
      if (nnumtck(i)+ndentck(i).ge.1) then
        if (trtck(i).le.0.0d0) then
          tau=-trtck(i)/t     !negative trtck indicates reverse order
        else
          tau=t/trtck(i)
        end if
        del=rho/Drtck(i)
        if (nnumtck(i).ge.1) then
          do j=nterm+1,nterm+nnumtck(i)              !numerator terms
            if (ABS(del).gt.1.d-12 .or. abs(ctck(i,j,5)).gt.1.d-12) then
              tcx1cr=tcx1cr+ctck(i,j,1)*(tau+ctck(i,j,2))**ctck(i,j,3)
     &              *(del+ctck(i,j,4))**ctck(i,j,5)
            else
              tcx1cr=tcx1cr+ctck(i,j,1)*(tau+ctck(i,j,2))**ctck(i,j,3)
            endif
          enddo
          nterm=nterm+nnumtck(i)
        end if
        if (ndentck(i).ge.1) then
          xden=0.0d0
          do j=nterm+1,nterm+ndentck(i)              !denominator terms
            if (itck(i,j,0).eq.99) then
c  flag for special form used by Perkins (MAX(T,2Tc-T))
              tcmax=MAX(tau,ctck(i,j,2)-tau)
              xden=xden+ctck(i,j,1)*tcmax**ctck(i,j,3)
     &            *(del+ctck(i,j,4))**ctck(i,j,5)
            else
              xden=xden+ctck(i,j,1)*(tau+ctck(i,j,2))**ctck(i,j,3)
     &            *(del+ctck(i,j,4))**ctck(i,j,5)
            end if
          enddo
          nterm=nterm+ndentck(i)
          tcx1cr=tcx1cr/xden
        end if
      end if
      if (nexptck(i).ge.1) then
        if (tredex(i).le.0.0d0) then
          tau=-tredex(i)/t   !negative tred indicates reverse order
        else
          tau=t/tredex(i)
        end if
        del=rho/Dredex(i)
        xexp=0.0d0
        do j=nterm+1,nterm+nexptck(i)              !exponential terms
          if (ABS(del).gt.1d-12 .or. itck(i,j,3).ne.0) then
            xexp=xexp+ctck(i,j,1)*(tau+ctck(i,j,2))**itck(i,j,2)
     &          *(del+ctck(i,j,3))**itck(i,j,3)
          else
            xexp=xexp+ctck(i,j,1)*(tau+ctck(i,j,2))**itck(i,j,2)
          endif
        enddo
        nterm=nterm+nexptck(i)
        if (xexp.lt.-500.0d0) then
          tcx1cr=0        !avoid underflow if exponential is almost one
        else
          tcx1cr=tcx1cr*EXP(xexp)
        end if
      end if
c     if (nsparek(i).ge.1) then
c       xspare=0.0d0
c       do j=nterm+1,nterm+nsparek(i)            !spare terms
c       enddo
c       tcx1cr=tcx1cr+xspare
c     end if
c     write (*,*) ' TCX1CR--xnum,xden,xexp: ',xnum,xden,xexp
c     write (*,*) ' TCX1CR--t,tcx_crit: ',t,tcx1cr
c
c  multiply by reducing parameter (to convert units, etc.)
      TCX1CR=tcx1cr*tcxred(i)
c
      RETURN
      end                                               !function TCX1CR
c
c ======================================================================
c
      subroutine SETTK3 (nread,icomp,hcasno,ierr,herr)
c
c  initialize model #3 for the thermal conductivity critical enhancement--
c  the simplified critical enhancement of Vesovic, et al for CO2:
c  Vesovic, V., Wakeham, W.A., Olchowy, G.A., Sengers, J.V., Watson, J.T.R.
c  and Millat, J. (1990). The transport properties of carbon dioxide.
c  J. Phys. Chem. Ref. Data  19: 763-808.
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
c                        1 = error
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-08-97  MM, original version
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=39
        write (herr,1039) hcasno,hnull
        call ERRMSG (ierr,herr)
 1039   format ('[SETUP error 39] block data option for t.c. crit ',
     &          'model #3 specified for CAS # ',a12,' but not ',
     &          'implemented in code.',a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTK3--read component',icomp,' from unit',nread
        read (nread,*) tmtck(icomp)              !lower temperature limit
        read (nread,*) txtck(icomp)              !upper temperature limit
        read (nread,*) pxtck(icomp)              !upper pressure limit
        read (nread,*) Dxtck(icomp)              !upper density limit
        jterm=0                                 !term counter
c  # terms in numerator & denominator of polynomial multiplier,
c  exponential term, and spare for future use
c  put CO2 terms into arrays used for "numerator," others for future use
        read (nread,*) nnumtck(icomp),ndentck(icomp),nexptck(icomp),
     &                 nsparek(icomp)
        if (nnumtck(icomp)+ndentck(icomp).ge.1) then
c  read reducing pars
          read (nread,*) trtck(icomp),Drtck(icomp),tcxred(icomp)
          if (nnumtck(icomp).ge.1) then
            do j=1,nnumtck(icomp)              !read "numerator" terms
c  as originally implemented for CO2, these terms are:
c  ctck(icomp,1,1) = gnu (universal exponent, approx 0.63)
c  ctck(icomp,2,1) = gamma (universal exponent, approx 1.2145)
c  ctck(icomp,3,1) = R0 (universal amplitude, 1.01 +/- 0.04)
c  ctck(icomp,4,1) = z (universal exponent, 0.065 +/- 0.005)
c  ctck(icomp,5,1) = c (visc const, approx 1.075, but often set to 1)
c  ctck(icomp,6,1) = xi0 (amplitude, order 1d-10 m)
c  ctck(icomp,7,1) = gam0 (amplitude, order 0.05 - 0.06)
c  ctck(icomp,8,1) = qd_inverse (cutoff diameter, order 10d-9 m)
c  ctck(icomp,9,1) = tref (reference temperature, 1.5 - 2.0 * Tc)
              jterm=jterm+1
              read (nread,*) ctck(icomp,jterm,1)
            enddo
          end if
        end if
      end if
c
      RETURN
      end                                             !subroutine SETTK3
c
c ======================================================================
c
      function TCX3CR (icomp,t,rho)
c
c  model #3 for the thermal conductivity critical enhancement--
c  the simplified critical enhancement of:
c  Olchowy, G.A. and Sengers, J.V. (1989). A simplified representation for
c  the thermal conductivity of fluids in the critical region.
c  Int. J. Thermophysics  10: 417-426.
c
c  also applied to CO2 by:
c  Vesovic, V., Wakeham, W.A., Olchowy, G.A., Sengers, J.V., Watson, J.T.R.
c  and Millat, J. (1990). The transport properties of carbon dioxide.
c  J. Phys. Chem. Ref. Data  19: 763-808.
c  equation numbers in comments refer to Vesovic paper
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx3cr--the critical enhancement to the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-08-97  MM, original version
c  06-16-97  MM, change DPDD, CVCP calls to DPDDK, CVCPK i.e. (t,x) to (icomp,t)
c  07-01-08  MLH, do not use viscosity critical enhancement in Olchowy-Sengers model
c  03-09-10  MLH, high t cutoff
c  04-13-10  MLH, do use viscosity critical enhancement in Olchowy-Sengers model
c  09-20-10  MLH, insert special code for new iapws water, more digits on piinv
c  11-20-10  MLH, replace call to etak with trnprp to remove problem w ecs only eta
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension xdummy(ncmax)
c
      xdummy(1)=1.0d0
      if (rho.lt.1.0d-6) then
c  critical enhancement is zero for low densities (avoid divide by zero)
        tcx3cr=0.0d0
        RETURN
      end if
c  recover the parameters from the storage array:
      gnu=ctck(icomp,1,1)      !nu (universal exponent, approx 0.63)
      gamma=ctck(icomp,2,1)    !gamma (universal exponent, approx 1.24)
      R0=ctck(icomp,3,1)       !R0 (universal amplitude, 1.01 +/- 0.04)
c  following two parameters not used here, but in file for future use
c     z=ctck(icomp,4,1)        !z (universal exponent, 0.065 +/- 0.005)
c     cvisc=ctck(icomp,5,1)    !visc const, approx 1.075, but often 1)
      xi0=ctck(icomp,6,1)      !xi0 (amplitude, order 1d-10 m)
      gam0=ctck(icomp,7,1)     !gam0 (amplitude, order 0.05 - 0.06)
      qd=1.0d0/ctck(icomp,8,1) !qd_inverse (cutoff dia, order 10d-9 m)
      treff=ctck(icomp,9,1)    !tref (reference temp, 1.5 - 2.0 * Tc)
c
c  if t is too high, no enhancement and return
      if (t.gt.treff) then
        tcx3cr=0.0d0
        return
      end if
c
c  check for water
      IF(hcas(icomp).eq.'7732-18-5')then
        tcx3cr= TCCR_2010 (T,RHO,QD)
        return
      endif
      call INFO (icomp,wmm,ttrp,tnbpt,tc,pc,Dc,Zc,acf,dip,Rgas)
      call DPDDK (icomp,t,rho,dpdrho)
c     write (*,*) ' TCX3CR--t,dpdrho: ',t,dpdrho
c  function chi (Eq 40 in Vesovic) evaluated at t,rho and tref,rho
c  Vesovic introduces a t/tc term which is absent in other papers
c     chi=pc/(Dc*Dc*tc)*rho*t/dpdrho               !Vesovic form
      chi=pc/(Dc*Dc)*rho/dpdrho
      call DPDDK (icomp,treff,rho,dpdrho)
c     chiref=pc/(Dc*Dc*tc)*rho*tref/dpdrho*tref/t  !Vesovic form
      chiref=pc/(Dc*Dc)*rho/dpdrho*treff/t
      delchi=chi-chiref
      if (delchi.le.0.0d0) then
c  delchi can go negative far from critical
c       write (*,*) ' TCX3CR--chi < chiref:  ',chi,chiref
        tcx3cr=0.0d0
        RETURN
      end if
c  function xi (Eq 46)
c  Olchowy, Vesovic put gam0 inside exponent, Krauss (R134a) puts this
c  term outside, but this yields incorrect values
      xi=xi0*(delchi/gam0)**(gnu/gamma)            !Vesovic form
c     xi=xi0/gam0*delchi**(gnu/gamma)              !Krauss' form
c     write (*,1150) chi,chiref,xi
c1150 format (1x,' TCX3CR--chi,chiref,xi:  ',3e14.6)
c  functions omega and omega_zero (Eqs 59 & 60)
c     write (*,1154) icomp,t,rho
c1154 format (1x,' TCX3CR--call CVCPK for icomp,t,rho =  ',i2,2e14.6)
      call CVCPK (icomp,t,rho,cv,cp)
c     write (*,1156) cv,cp
c1156 format (1x,' TCX3CR--cv,cp returned from CVCPK =  ',2e14.6)
      piinv=1.0d0/3.14159265358979d0
c     write (*,1160) cv,cp,qd,xi
c1160 format (1x,' TCX3CR--cv,cp,qd,xi:  ',4e14.6)
      xomg=2.0d0*piinv*((cp-cv)/cp*ATAN(qd*xi)+cv/cp*qd*xi)
      xomg0=2.0d0*piinv*(1.0d0-EXP(-1.0d0/(1.0d0/(qd*xi)
     &     +((qd*xi*Dc/rho)**2)/3.0d0)))
c     hetacrs =hetacr(icomp)
      !hetacr(icomp)=''       !if you want nul viscosity enhancement model in Olchowy-Sengers model
      call ETAK (icomp,t,rho,eta,ierr,herr)
      IF(ierr.ne.0)then
       ierr=0
       herr=''
       call TRNECS (t,rho,xdummy,eta,ttcx,ierr,herr) !if only have ecs for viscosity must call here
      else
      endif
      !If (hetahc(icomp).eq.'H2O') eta=etanorm  !hard coded water has enhancement in
      !hetacr(icomp)=hetacrs  !reset to original value
c     write (*,1162) xomg,xomg0,eta
c1162 format (1x,' TCX3CR--omega,omega_0,eta:  ',3e14.6)
      boltz=Rgas/6.0221367d23       !Boltzman's const
c  factor of 1d9 in next equation to convert from mol/L --> mol/m**3
c  and from micro-Pa-s to Pa-s
      tcx3cr=rho*1.0d9*cp*R0*boltz*t*piinv/(6.0d0*eta*xi)*(xomg-xomg0)
c  multiply by reducing parameter (to convert units, etc.)
      TCX3CR=tcx3cr*tcxred(icomp)
c  write (*,*) ' TCX3CR--tcx3cr: ',TCX3CR
c
      RETURN
      end                                               !function TCX3CR
c
c ======================================================================
c
      subroutine SETTK4 (nread,icomp,hcasno,ierr,herr)
c
c  initialize model #4 for the thermal conductivity critical enhancement--
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
c                       39 = error--model not implemented
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-24-97  MM, original version
c  08-19-97  MM, error for nread<=0; input hcasno is not array
c  02-22-99 EWL, implement TCX4CR model
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
        write (herr,1101) icomp,nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETTK4 error 101] illegal file specified for icomp =',
     &          i3,'; nread = ',i4,'; CAS no. = ',a12,a1)
      else
        read (nread,*) tmtck(icomp)              !lower temperature limit
        read (nread,*) txtck(icomp)              !upper temperature limit
        read (nread,*) pxtck(icomp)              !upper pressure limit
        read (nread,*) Dxtck(icomp)              !upper density limit
        read (nread,*) nnumtck(icomp),ndentck(icomp),nexptck(icomp),
     &                 nsparek(icomp)
        if (nnumtck(icomp)+ndentck(icomp).ge.1) then
c  read reducing pars
          read (nread,*) trtck(icomp),prtck(icomp),Drtck(icomp),
     &                   tcxred(icomp)
          if (nnumtck(icomp).ge.1) then
            do j=1,nnumtck(icomp)              !read "numerator" terms
              read (nread,*) ctck(icomp,j,1)
            enddo
          end if
        end if
      end if
c
      RETURN
      end                                             !subroutine SETTK4
c
c ======================================================================
c
      function TCX4CR (icomp,t,rho)
c
c  critical enhancement to the thermal conductivity by the model of
c  Younglove and Hanley, JPCRD, 15(4):1323, 1986
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcx4cr--the critical enhancement part of the thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-22-99 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
c  if density is approx zero, return zero for the critical enhancement
c  (avoid division by zero)
      if (rho.lt.1.0d-6) then
        TCX4CR=0.0d0
        RETURN
      end if
c
c  find derivatives dP/dD and dP/dT
      call DPDDK (icomp,t,rho,dpdrho)
      call DPDTK (icomp,t,rho,dpt)
      dc=Drtck(icomp)
      tc=trtck(icomp)
      pc=prtck(icomp)
      xi=pc*rho/(dc**2*dpdrho)
      if (xi.ge.0) xi=xi**ctck(icomp,3,1)
      dellam=ctck(icomp,1,1)*ctck(icomp,2,1)/pc
     &      *(t*dpt*dc/rho)**2*xi*1d21
      delt=ABS(t-tc)/tc
      delD=ABS(rho-dc)/dc
      eterm=ctck(icomp,4,1)*delt**2+ctck(icomp,5,1)*delD**4
c  check that exponential term will not result in underflow
      if (eterm.gt.500.0d0) then
        eterm=EXP(-500.0d0)
      else
        eterm=EXP(-eterm)
      end if
      call ETAK (icomp,t,rho,visc,ierr,herr)
c  combine all of the above to arrive at the critical enhancement
      TCX4CR=dellam*eterm/(6.0d0*3.141592654d0*ctck(icomp,6,1)*visc)
      TCX4CR=tcx4cr*tcxred(icomp)
c
      RETURN
      end                                               !function TCX4CR
c
c ======================================================================
c
      subroutine SETTK6 (nread,icomp,hcasno,ierr,herr)
c
c  initialize model #6 for the thermal conductivity critical enhancement--
c  to be used with ecs transport model. It is based on
c  the simplified critical enhancement of Vesovic, et al for CO2:
c  Vesovic, V., Wakeham, W.A., Olchowy, G.A., Sengers, J.V., Watson, J.T.R.
c  and Millat, J. (1990). The transport properties of carbon dioxide.
c  J. Phys. Chem. Ref. Data  19: 763-808.
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
c                        1 = error
c     herr--error string (character*255 variable if ierr<>0)
c     other quantities returned via arrays in commons
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-08-97  MM, original version
c  11-15-01 MLH, adapted for ecs model use
c  06-02-11 MLH, change common blocks to dimension statements
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
      dimension tmtcke(nrf0:nx),txtcke(nrf0:nx),pxtcke(nrf0:nx),
     &          Dxtcke(nrf0:nx)
      dimension trtcke(nrf0:nx),Drtcke(nrf0:nx),prtcke(nrf0:nx),
     &          tcxrede(nrf0:nx),tredexe(nrf0:nx),Dredexe(nrf0:nx)
      dimension nnumtcke(nrf0:nx),ndentcke(nrf0:nx),nexptcke(nrf0:nx),
     &          nspareke(nrf0:nx)
c
c     NOTE: only the ctcke parameters are needed, everything else is not
c           saved into common blocks in order to avoid overwriting any TK3
c           parameters loaded previously for dedicated models
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=39
        write (herr,1039) hcasno,hnull
        call ERRMSG (ierr,herr)
 1039   format ('[SETUP error 39] block data option for t.c. crit ',
     &          'model #3 specified for CAS # ',a12,' but not ',
     &          'implemented in code.',a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETTK6--read component',icomp,' from unit',nread
        read (nread,*) tmtcke(icomp)              !lower temperature limit
        read (nread,*) txtcke(icomp)              !upper temperature limit
        read (nread,*) pxtcke(icomp)              !upper pressure limit
        read (nread,*) Dxtcke(icomp)              !upper density limit
        jterm=0                                 !term counter
c  # terms in numerator & denominator of polynomial multiplier,
c  exponential term, and spare for future use
c  put CO2 terms into arrays used for "numerator," others for future use
        read (nread,*) nnumtcke(icomp),ndentcke(icomp),nexptcke(icomp),
     &                 nspareke(icomp)
        if (nnumtcke(icomp)+ndentcke(icomp).ge.1) then
c  read reducing pars
          read (nread,*) trtcke(icomp),Drtcke(icomp),tcxrede(icomp)
          if (nnumtcke(icomp).ge.1) then
            do j=1,nnumtcke(icomp)              !read "numerator" terms
c  as originally implemented for CO2, these terms are:
c  ctck(icomp,1,1) = gnu (universal exponent, approx 0.63)
c  ctck(icomp,2,1) = gamma (universal exponent, approx 1.2145)
c  ctck(icomp,3,1) = R0 (universal amplitude, 1.01 +/- 0.04)
c  ctck(icomp,4,1) = z (universal exponent, 0.065 +/- 0.005)
c  ctck(icomp,5,1) = c (visc const, approx 1.075, but often set to 1)
c  ctck(icomp,6,1) = xi0 (amplitude, order 1d-10 m)
c  ctck(icomp,7,1) = gam0 (amplitude, order 0.05 - 0.06)
c  ctck(icomp,8,1) = qd_inverse (cutoff diameter, order 10d-9 m)
c  ctck(icomp,9,1) = tref (reference temperature, 1.5 - 2.0 * Tc)
              jterm=jterm+1
              read (nread,*) ctcke(icomp,jterm,1)
            enddo
          end if
        end if
      end if
c
      RETURN
      end                                             !subroutine SETTK6
c
c ======================================================================
c
      function TCCNH3 (icomp,t,rho)
c
c  model for the thermal conductivity critical enhancement of ammonia
c  by the empirical model of Tufeu et al.
c
c  this is a special model only for ammonia with all constants "hardwired"
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c           [not used--included to maintain parallel structure]
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tccnh3--the critical enhancement to the thermal conductivity [W/m-K]
c
c  written by S.A. Klein & M. McLinden,
c  NIST Phys & Chem Properties Div, Boulder, CO
c  02-26-97  MM, original version (adapted from TCENHC of SAK)
c  07-07-98 EWL, changed variable dpdt to dpt
c  12-01-98 EWL, add Reos and triple point pressure and density to /CCON/
c  01-19-00 EWL, avoid divide by zero if tr=0
c  01-25-00 EWL, changed 253 to 235 (for rhoc) and fixed other errors
c  01-26-00 EWL, Tufeu formulation returns an infinite Tcx at Tc for any
c                density.  Outside of the critical region, the effect was
c                removed (between 404.4 and 406.5 K and rho<9.6 or rho>18.)
c
      include 'commons.for'
      include 'comtrn.for'
c
      rhokg=rho*wmas(icomp)               !correlation in mass units
      tr=abs(t-405.4d0)/405.4d0           !405.4 is Tc
      trr=tr
      if (t.gt.404.4d0 .and. t.lt.406.5d0) then
        if (rho.lt.9.6d0 .or. rho.gt.18.0d0) then
          trr=0.002
        endif
      endif
      etab=1.0d-5*(2.6d0+1.6d0*tr)        !viscosity--Eq 9
      dPT=1.0d5*(2.18d0- 0.12d0/EXP(17.8d0*tr))  !dP/dT for rho = rhoc
c  Eq 9 of Tufeu for conductivity along critical isochore
      if (abs(trr).lt.1.d-20) then
        tcrhoc=1.0d20
        dtcid=tcrhoc
        xcon=-1.0d20                         !Eq 12
      else
        tcrhoc=1.2d0*1.38066d-23*t**2*dPT**2*0.423d-8/(trr**1.24d0)*
     &    (1.0d0+1.429d0*tr**0.50d0)/(6.0d0*3.14159*etab*(1.34d-10/trr**
     &    (0.63d0)*(1.0d0+1.0d0*tr**0.50d0)))
        dtcid=tcrhoc*EXP(-36.0d0*tr**2)     !Eq 10
        xcon=0.61d0*235d0+16.5d0*log(trr)    !Eq 12
      endif
      if (rho/Dcrit(icomp) .lt. 0.6d0) then
c  Eq 14 for rho < 0.6*rhoc (141 = 0.6*rhoc)
        tccsw=dtcid*xcon**2/(xcon**2+(141.0d0- 0.96d0*235.0d0)**2)
        TCCNH3=tccsw*rhokg**2/141.0d0**2 !SAK had 253 in denominator
      else
c  Eq 11 for rho > 0.6*rhoc       (SAK had 253)
        TCCNH3=dtcid*xcon**2/(xcon**2+(rhokg-0.96d0*235.0d0)**2)
      end if
c
      RETURN
      end                                               !function TCCNH3
c
c ======================================================================
c
      function TCCCH4 (icomp,t,rho)
c
c  model for the thermal conductivity critical enhancement of methane
c  by the empirical model of Friend et al. (1989).
c
c  this is a special model only for methane with all constants "hardwired"
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c           [not used--included to maintain parallel structure]
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tccch4--the critical enhancement to the thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon
c  NIST Phys & Chem Properties Div, Boulder, CO
c  01-27-00 EWL, original version (adapted from TCENHC of SAK)
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      call DPDTK (icomp,t,rho,dpt)
      call DPDDK (icomp,t,rho,dpdrho)
      call ETAK (icomp,t,rho,eta,ierr,herr)
      tau=tcrit(icomp)/t
      del=rho/Dcrit(icomp)
      if (ABS(rho).gt.1d-12) then
        dpt=dpt/R/rho
      else
        dpt=1.0d0
      endif
      dpdrho=dpdrho/R/t
      ts=(tcrit(icomp)-t)/tcrit(icomp)
      ds=(Dcrit(icomp)-rho)/Dcrit(icomp)
      xt=0.28631d0*del*tau/dpdrho
      IF (xt.lt.0) xt=1.0d5
      ftd=2.646d0*SQRT(ABS(ts))+2.678d0*ds**2-0.637d0*ds
      ftd=exp(-ftd)
      TCCCH4=91.855d0/eta/tau**2*dpt**2*xt**0.4681d0*ftd*1.d-3
c
      RETURN
      end                                               !function TCCCH4
c
c ======================================================================
c
c
c     The following functions (through REXCES) were taken from NIST12,
c     Version 3.1, and modified to work with the current version.
c
      FUNCTION TCXH2(ICOMP,T,D)
c
c  model for the thermal conductivity of para and normal hydrogen
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c        d--molar density [mol/L]
c  output (as function value):
c     tcx5--thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  10-20-99 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension x(ncmax)
c
      do i=1,nc
        x(i)=0.0d0
      enddo
      x(icomp)=1.0d0
      THER1=0.D0
      IF (T.LE.90.0D0) THEN
        D0=D
        CALL PRESS (T,D,X,PE)
        IF(PE.GE.12000.0d0) CALL TPRHO (t,12000.0d0,X,1,0,D0,ierr,herr)
        THER1=DILT(ICOMP,T)+EXCSH2(ICOMP,D0,T)*1000.0D0+RCRIT(D0,T)
        IF(D.gt.0.0D0)THER1=THERMX(D,D0,THER1)
        TCXH2=THER1/1000.0d0
        IF(T.LT.70.0D0)RETURN
      ENDIF
      D0=D
      CALL PRESS (T,D,X,PE)
      IF(PE.GE.60000.0d0) CALL TPRHO (t,60000.0d0,X,1,0,D0,ierr,herr)
      THER2=RTHERM(ICOMP,D0,T)
      IF(D.gt.0.0D0)THER2=THERMX(D,D0,THER2)
      IF(T.GT.90.0D0)THEN
        TCXH2=THER2
      ELSE
        PER=(T-70.0D0)/20.0D0
        TCXH2=(THER1/1000.0D0)*(1.0D0-PER) + THER2*PER
      END IF
      END

      FUNCTION THERMX(DE,D0,THER)
      include 'commons.for'
      include 'comtrn.for'
c
      THERMX=DEXP(LOG(THER)+(DE-D0)*0.041758537D0)
      END

      FUNCTION DILT(ICOMP,T)
      include 'commons.for'
      include 'comtrn.for'
c
      TF=T**(1.0D0/3.0D0)
      TFF=T**(-4.0D0/3.0D0)
      SUM=0
      DO I=1,9
        TFF=TFF*TF
        SUM=SUM+ctcx(icomp,I,1)*TFF
      ENDDO
      DILT=SUM*100.0D0
      END

      FUNCTION RCRIT(D,T)
      include 'commons.for'
      include 'comtrn.for'
c
      DELT=ABS(T-32.938D0)
      DELD=D-15.556D0- 0.008229*DELT**1.5D0
      X=0.138D0*DELD
      RCRIT=(0.635363D-2 -0.5863D-4*T)*DEXP(-X**2)*1000.0D0
      IF(RCRIT.LE.0.0D0)RCRIT=0.0D0
      END

      FUNCTION RTHERM(ICOMP,DD,TIN)
C     INPUT, DENSITY MOL/L, TEMPERATURE K, OP PARA FRACTION
C     OUTPUT, THERMAL CONDUCTIVITY OF HYDROGEN, W/M.K, 4 FEB 84
C     FROM NBSIR 84-3006, HM RODER MAY 86
      include 'commons.for'
      include 'comtrn.for'
      DIMENSION TNZ(35),TPZ(35),TT(35),G(3)
      parameter (TNZ=(/.0505,.0568,.0632,.0695,.0763,.0829,.0896,
     &.0962,.1026,.1092,.1157,.1220,.1282,.1342,.1401,.1458,.1514,
     &.1569,.1622,.1674,.1725,.1774,.1823,.1870,.1917,.1962,.2012,
     &.2061,.2110,.2158,.2203,.2250,.2295,.2340,.2380/))
      parameter (TPZ=(/.0529,.0617,.0714,.0816,.0924,.1028,.1125,
     &.1213,.1294,.1365,.1427,.1482,.1530,.1574,.1614,.1651,.1687,
     &.1723,.1758,.1793,.1828,.1863,.1899,.1935,.1972,.2010,.2048,
     &.2088,.2129,.2169,.2208,.2249,.2290,.2330,.2370/))
      parameter (TT=(/70.,80.,90.,100.,110.,120.,130.,140.,150.,
     &160.,170.,180.,190.,200.,210.,220.,230.,240.,250.,260.,270.,
     &280.,290.,300.,310.,320.,330.,340.,350.,360.,370.,380.,390.,
     &400.,410./))
      parameter (G=(/.1584312604d-02,.3861103193d-04,.1066433014d-06/))
      OP=ctcx(icomp,10,1)
      DO I=1,35
        IF(TIN.LT.TT(I))GOTO 4
      ENDDO
      TCZN=0.2380d0+0.0045d0*(TIN-410.0D0)/10.0D0 !Correction by EWL to account
      TCZP=0.2370d0+0.0040d0*(TIN-410.0D0)/10.0D0 !for high temperatures
      GOTO 5
    4 CONTINUE
      TCZN=TNZ(I-1)+(TNZ(I)-TNZ(I-1))*(TIN-TT(I-1))/(TT(I)-TT(I-1))
      TCZP=TPZ(I-1)+(TPZ(I)-TPZ(I-1))*(TIN-TT(I-1))/(TT(I)-TT(I-1))
    5 CONTINUE
      OPDIFF=TCZP-TCZN
      TCZADJ=OPDIFF/.75*(OP-.25)
      FACTOR=1.D0-.028484+.000070588*TIN
      TCZ=TCZN*FACTOR+TCZADJ+CRITH2(DD,TIN)
      RTHERM=TCZ+G(1)*DD+(G(2)+G(3)*TIN)*(EXP(2.1*DD**.36)-1.0D0)
      END

      FUNCTION CRITH2(D,T)
      include 'commons.for'
      include 'comtrn.for'
c
      CRITH2=0.0D0
      IF(T.LE.77..OR.T.GE. 108.35658)RETURN
      AMPL=0.00635363D0- .00005863D0*T
      RHOCEN=15.556D0- .008229D0*(T-32.938D0)**1.5
      CRITH2=AMPL*EXP(-(0.138D0*(D-RHOCEN))**2)
      END

      FUNCTION EXCSH2(ICOMP,D,T)
c  12-23-03 EWL, add D.LT.0.001 to stop infinite contribution at very low p.
c  01-23-07 EWL, check for high exponent on R2
      include 'commons.for'
      include 'comtrn.for'
      dimension ET(8)
c
      EXCSH2=0
      IF (D.LE.0.D0) RETURN
      IF(T.GT.80.D0 .OR. D.LT.0.001D0)THEN
        EXCSH2=REXCES(D,T)
        RETURN
      ENDIF
      ET(1)=ctcx(icomp,11,1)
      ET(2)=ctcx(icomp,12,1)
      ET(3)=ctcx(icomp,13,1)
      ET(4)=ctcx(icomp,14,1)
      ET(5)=ctcx(icomp,15,1)
      ET(6)=ctcx(icomp,16,1)
      ET(7)=ctcx(icomp,17,1)
      ET(8)=ctcx(icomp,18,1)
      RR=D**0.1D0
      R2=0.d0
      IF ((D-ET(8))/ET(8).LT.1000.) R2=D**((D-ET(8))/ET(8))
      X=ET(1)+ET(2)*R2+ET(3)*RR+ET(4)*R2/(T*T)
     &+ET(5)*RR/T**1.5D0+ET(6)/T+ET(7)*R2/T
      X1=ET(1)+ET(6)/T
      IF (X.LT.3000.D0) EXCSH2=(DEXP(X)-DEXP(X1))/10.D0
      END

      FUNCTION REXCES(D,T)
      include 'commons.for'
      include 'comtrn.for'
c
      DEL=.38611D-4+.10664D-7*T
      REXCES=.15843D-2*D+DEL*(DEXP(2.1D0*D**.36D0)-1.0D0)
      END
c
c ======================================================================
c
      function TCXHE (icomp,t,rho)
c
c  calculate the thermal conductivity of helium using the model of:
c  Hands, B.A. and Arp, V.D.  A Correlation of Thermal Conductivity Data
c  for Helium.  Cryogenics, 21(12):697-703 (1981).
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  output (as function value):
c   tcxhe --thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  07-06-98 EWL, original version
c  11-06-00 EWL, replace the TC4 version with a TC0 version and combine
c                the various subroutines into one.
c  07-23-02 EWL, initialize tcxcr
c  09-18-03 EWL, change dcc from 69.58 to 69.158 (as given by Hands and Arp)
c  08-09-05 EWL, check for rho=0
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      bkt=0.0d0
      i=icomp
      tau=t/trdbkt(i)
      sum=0.0d0
      do j=2,ndgnum(i)
        sum=sum+ctcx(i,j,1)*tau**ctcx(i,j,2)
      enddo
      tcxdg=ctcx(i,1,1)*tau**ctcx(i,1,2)*EXP(sum)
c     write (*,*) ' TCXHE--dilute-gas thermal conductivity:  ',tcxdg
c
      tau=(t/trdbkt(i))**(1.0d0/3.0d0)
      del=rho/Drdbkt(i)
c  sum the background terms
      tcxbk=0.0d0
      do j=1,nbknum(i)
        j1=j+ndgnum(i)
        if (abs(ctcx(i,j1,4)).lt.1.d-12 .or. abs(del).lt.1.d-12) then
          tcxbk=tcxbk+ctcx(i,j1,1)*tau**ctcx(i,j1,2)*del**ctcx(i,j1,3)
        else
          tcxbk=tcxbk+ctcx(i,j1,1)*tau**ctcx(i,j1,2)*del**ctcx(i,j1,3)
     &          *LOG(del**ctcx(i,j1,4))
        endif
      enddo
c  add the critical enhancement contribution
      tcxcr=0.0d0
      IF (T.GE.3.5d0 .AND. T.LE.12.0d0) THEN
        x0=.392d0
        e1=2.8461d0
        e2=.27156d0
        beta=.3554d0
        gamma=1.1743d0
        delta=4.304d0
        dcc=69.158d0
        tcc=5.18992d0
        pcc=227460.d0
        rho1=rho*wmas(icomp)
        call DPDDK (icomp,t,rho,dpd)
        if (rho.gt.0.0d0) bkt=1.0d0/dpd/rho/1000.0d0
        deld=ABS((rho1-dcc)/dcc)
        delt=ABS((t-tcc)/tcc)
        r2=(delt/0.2d0)**2+(deld/0.25d0)**2
        if (r2.lt.1.0d0 .and. rho.gt.0.0d0) then
          xx=delt/deld**(1.0d0/beta)
          x1=(xx+x0)/x0
          x2b=x1**(2.0d0*beta)
          x2be=(1.0d0+e2*x2b)**((gamma-1.0d0)/2.0d0/beta)
          hh=e1*x1*x2be
          dhdx=e1*x2be/x0+e1*e2/x0*x2b*x2be/(1.0d0+e2*x2b)*(gamma-1.0d0)
          d2kt=(delta*hh-xx*dhdx/beta)*deld**(delta-1.0d0)
          bkt1=(dcc/rho1)**2/d2kt/pcc
          bkt=r2*bkt+(1.0d0-r2)*bkt1
        endif
        call DPDTK (icomp,t,rho,pdt)
        call ETAK (icomp,t,rho,eta,ierr,herr)
        eta=eta/1.0d6
        pdt=pdt*1.0d3
        bkcrit=0
        if (bkt.ge.0.0d0 .and. rho1.gt.0.0d0) then
          bkcrit=t**2*SQRT(bkt)/rho1/eta*pdt**2
     &          *EXP(-18.66d0*delt**2-4.25d0*deld**4)
        endif
        bkcrit=3.4685233d-17*bkcrit
        tcxcr=3.726229668d0*bkcrit
      ENDIF

c  multiply by reducing parameter (to convert units, etc.)
      tcxhe=(tcxdg+tcxbk+tcxcr)*tcxbkt(i)
c     write (*,*) ' TCXHE--residual thermal conductivity:    ',tcxhe
c
      RETURN
      end                                                !function TCXHE
c
c ======================================================================
c
      function TCXETY (icomp,t,rho)
c
c  thermal conductivity model for ethylene by Holland et al. (1983)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcxety--thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  02-29-00 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
      double precision gt(9)
c
      tcxety=0.0d0
      gt(1)=-2.9034235280d5
      gt(2)= 4.6806249520d5
      gt(3)=-1.8954783215d5
      gt(4)=-4.8262235392d3
      gt(5)= 2.2434093720d4
      gt(6)=-6.6206354818d3
      gt(7)= 8.9937717078d2
      gt(8)=-6.0559143718d1
      gt(9)= 1.6370306422d0
      t1=-1.3045033230d1
      t2= 1.8214616599d1
      t3=-9.9030224960d3
      t4= 7.4205216310d2
      t5=-3.0083271933d-1
      t6= 9.6456068829d1
      t7= 1.3502569620d4
      d=rho*wmas(icomp)/1000.0d0
      dc=.221d0
      th=(d-dc)/dc
      tt=t**(1.0d0/3.0d0)
      tcx0=gt(1)/t+gt(2)/tt**2+gt(3)/tt+gt(4)+gt(5)*tt
     &    +gt(6)*tt**2+gt(7)*t+gt(8)*tt**4+gt(9)*tt**5
      tcxpr=exp(t1+t4/t)*(exp(d**0.1d0*(t2+t3/t**1.5d0)
     &     +th*d**0.5d0*(t5+t6/t+t7/t**2))-1.0d0)
      tcxcr=0
c     if (rho.gt.0) then
c       an=6.0221367d23
c       xk=1.380658d-23
c       pi=3.1415927d0
c       tcc=282.34d0
c       pcc=5.039d0
c       beta=0.355d0
c       gamma=1.19d0
cc      x0=0.168d0
c       eta=ETAETY(icomp,t,rho)
c       call DPDTK (icomp,t,rho,dpt)
c       call DPDDK (icomp,t,rho,dpdrho)
c       xkt=SQRT(1.0d0/rho/dpdrho*1.d3)
c       dts=(t-tcc)/tcc
cc      b=x0**(-beta)
c       b=ABS(th)/ABS(dts)**gamma
cc      g=x0**gamma/(2.17d0*0.287d0**((gamma-1)/2.0d0/beta))
c       xts=d**2*xkt*pcc/dc**2
c       g=xts*ABS(dts)**gamma
c       xi=0.69d0/SQRT(b**2*pcc/g/xk/tcc)
c       f=EXP(-18.66d0*dts**2-4.25d0*th**4)
c       c=SQRT(wmas(icomp)/d/an/xk/t)
c       tcxcr=c*xk*t**2/6.0d0/pi/eta/xi*dpt**2*xkt*f*1.d5/sqrt(10)
c     endif
      tcxety=(tcx0+tcxpr+tcxcr)/1000.0D0
c
      RETURN
      end                                               !function TCXETY
c
c ======================================================================
c
      function TCXR23 (icomp,t,rho)
c
c  thermal conductivity model for R23
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcxr23--thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  11-01-00 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
c
      tcxR23=0.0d0
      rhoL=ctcx(icomp,1,1)
      B1=ctcx(icomp,2,1)
      B2=ctcx(icomp,3,1)
      C1=ctcx(icomp,4,1)
      C2=ctcx(icomp,5,1)
      DG=ctcx(icomp,6,1)
      tcxmax=ctcx(icomp,7,1)
      drho=rhoL-rho
      del=rho-Drdbkt(icomp)
      tau=t-trdbkt(icomp)
      tcxdg=(B1+B2*t)*(drho/rhoL)**C1
      tcxrs=(rho/rhoL)**C1*C2*rhoL**2/drho*t**0.5d0
     &     *EXP(rho/drho*DG/R/t)
      tcxct=4.0d0*tcxmax/(exp(del)+exp(-del))/(exp(tau)+exp(-tau))
      tcxR23=(tcxdg+tcxrs+tcxct)/1000.0d0
c
      RETURN
      end                                               !function TCXR23
c
c ======================================================================
c
      function TCXD2O (icomp,t,rho)
c
c  thermal conductivity model for heavy water
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcxd2o--thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  11-06-00 EWL, original version
c
      include 'commons.for'
      include 'comtrn.for'
c
      tcxD2O=0.0d0
      tr=t/trdbkt(icomp)
      dr=rho/Drdbkt(icomp)
      tau=tr/(ABS(tr-1.1d0)+1.1d0)
      tcx0=0.d0
      j=ndgnum(icomp)
      do i=1,j
        tcx0=tcx0+ctcx(icomp,i,1)*tr**ctcx(icomp,i,2)
      enddo
      tcxr=0.d0
      do i=1,nbknum(icomp)
        j=j+1
        tcxr=tcxr+ctcx(icomp,j,1)*dr**ctcx(icomp,j,2)
      enddo
      be=ctcx(icomp,j+1,1)
      b0=ctcx(icomp,j+2,1)
      c1=ctcx(icomp,j+3,1)
      c2=ctcx(icomp,j+4,1)
      ct1=ctcx(icomp,j+5,1)
      ct2=ctcx(icomp,j+6,1)
      cr1=ctcx(icomp,j+7,1)
      cr2=ctcx(icomp,j+8,1)
      cr3=ctcx(icomp,j+9,1)
      dr1=ctcx(icomp,j+10,1)
      d1=ctcx(icomp,j+11,1)
      f1=EXP(ct1*tr+ct2*tr**2)
      f2=EXP(cr1*(dr-1.0d0)**2)+cr2*EXP(cr3*(dr-dr1)**2)
      f3=1.0d0+EXP(60.0d0*(tau-1.0d0)+20.0d0)
      f4=1.0d0+EXP(100.0d0*(tau-1.0d0)+15.0d0)
      tcxr=b0*(1.d0-EXP(be*dr))+tcxr
      tcxc=c1*f1*f2*(1.0d0+f2**2*(c2*f1**4/f3+3.5d0*f2/f4))
      tcxl=d1*f1**1.2d0*(1.d0-EXP(-(dr/2.5d0)**10))
      tcxD2O=tcxbkt(icomp)*(tcx0+tcxr+tcxc+tcxl)
c
      RETURN
      end                                               !function TCXD2O
c
c ======================================================================
c
      function TCXH2O (icomp,t,rho)
c
c  thermal conductivity model for water
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c   tcxh2o--thermal conductivity [W/m-K]
c
c  written by E.W. Lemmon, NIST Phys & Chem Properties Div, Boulder, CO
c  11-07-00 EWL, original version
c  12-19-06 EWL, add check for large EXP(dr*s)
c  01-04-08 MLH, added additional slots to WNTETA
c  05-10-08 MLH, fix bug dreta instead of dr, allow for new vis model
c
      include 'commons.for'
      include 'comtrn.for'
c
      tcxH2O=0.0d0
      tr=t/trdbkt(icomp)
      dr=rho/Drdbkt(icomp)
      tau=1.0d0/tr-1.0d0
      del=dr-1.0d0
      if (ABS(tau).lt.1.0d-12 ) tau=1.0d-12
      if (ABS(del).lt.1.0d-12 ) del=1.0d-12
      s=0.0d0
      do i=1,ndgnum(icomp)
        s=s+ctcx(icomp,i,1)/tr**ctcx(icomp,i,2)
      enddo
      tcx0=SQRT(tr)/s
      s=0.0d0
      do i=1,nbknum(icomp)
        j=i+ndgnum(icomp)
        s=s+ctcx(icomp,j,1)
     &        *tau**INT(ctcx(icomp,j,2))*del**INT(ctcx(icomp,j,3))
      enddo
      tcx1=1.0d10
      if (dr*s.lt.100) tcx1=EXP(dr*s)
c  calculate water viscosity
      treta=t/tredeta(icomp)
      dreta=rho/Dredeta(icomp)
      taueta=1.0d0/treta-1.0d0
      deleta=dreta-1.0d0  !bug fix; was dr
      if (ABS(taueta).lt.1.0d-12 ) taueta=1.0d-12
      if (ABS(deleta).lt.1.0d-12 ) deleta=1.0d-12
      s=0.0d0
      do i=1,ndel0(icomp)
        s=s+ceta(icomp,i,1)/treta**ceta(icomp,i,2)
      enddo
      eta0=SQRT(treta)/s
      IF(verfl(icomp).gt.8.0)eta0=eta0/55.071d-2 !allow new viscosity model
      s=0.0d0
      do i=1,npoly(icomp)
        j=i+ndel0(icomp)
        s=s+ceta(icomp,j,1)
     &        *taueta**INT(ceta(icomp,j,2))*deleta**INT(ceta(icomp,j,3))
      enddo
      eta1=EXP(dreta*s)
c  critical region contribution to Tcx
C     skip this for beyond version 8.1 - use tk3
      IF(verfl(icomp).le.8.1)then
      call DPDTK (icomp,t,rho,dpt)
      call DPDDK (icomp,t,rho,dpdrho)
      tcx2=0.0d0
      if (dr.gt.0.0d0 .and. dpdrho.gt.0.0d0) then
        x=dr/dpdrho/Drdbkt(icomp)*22115.0d0
        tcx2=0.0013848d0*(t/dr)**2/eta0/eta1
     &      *(dpt/22115.d0)**2*x**0.4678d0*SQRT(dr)
     &      *EXP(-18.66d0*(tr-1.d0)**2-del**4)
      endif
      else
      tcx2=0.0d0 !will be added by crit routine later
      endif
      tcxH2O=tcxbkt(icomp)*(tcx0*tcx1+tcx2)
      RETURN
      end                                               !function TCXH2O
c ======================================================================
c
      function TCCR_2010 (T,RHOmol,QD)
c   critical enhancement term for water, based on simplified Olchowy Sengers
c   t--temperature K
c   rhomol--molar density mol/L
c   qd-- cutoff wave number, m
c   tccr--the critical enhancement to the thermal conductivity W/m-K
c
c   09-30-2010  based on code from R. Perkins
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension x(ncmax)
      parameter (Tc=647.096D0, Denc=322.0D0, Pc=22.064D0)
      parameter (xi0=0.13d-9, gammaplus=0.06d0)
      parameter (exnu=0.63d0, exgamma=1.239d0)
* boltz based on CODATA gas constant
*     parameter (RD=1.05d0, boltz=1.3806504d-23)
* boltz based on IAPWS95 (incorrect gas constant)
      parameter (RD=1.01d0, boltz=1.3806347766d-23)
      parameter (pi=3.14159265358979d0)
      wmwat=18.015268d0
      !qd=1.0d0/0.4d-9
      rhokg=rhomol*wmwat
      tbar=t/tc
      iii=1
      x(1)=1.0d0 !pure fluid
      if (rhomol.lt.1.0d-6) then
c  critical enhancement is zero for low densities (avoid divide by zero)
        tccr_2010=0.0d0
        RETURN
      end if
* calculate correlation length xi in m
      xi=xi_fun(T,RHOmol,iii)
      IF(xi.le.0.0d0) then
        tccr_2010=0.0d0
        return
      endif
*
* calculate cv and cp at T and RHO
      call THERM (t,rhomol,x,p,e,h,s,cv,cp,w,hjt)
      cpbar=(cp/wmwat)/0.46151805d0   !cp/R
      rhobar=rhokg/denc
      rkappa=cp/cv
      yval=qd*xi
      xomg_bar=2.0d0/pi*((1.0d0- 1.0d0/rkappa)*ATAN(yval)+yval/rkappa)
      xomg0_bar=2.0d0/pi*(1.0d0-EXP(-1.0d0/(1.0d0/yval
     &     +(yval**2)/(3.0d0*rhobar**2))))
      call ETAK (iii,t,rhomol,eta,ierr,herr) ! eta is in uPa-s
      etabar=eta
c smooth transition at low xi
      if (xi.gt.1.5d-12) then
        Z=1.0d0/yval*(xomg_bar-xomg0_bar)
      else
        Z=2.0d0/pi*
     &   (yval/2.0d0+yval**2*(1.0d0/3.0d0/rkappa-1.0d0/2.0d0)+
     &   yval**3*(1.0d0/3.0d0/rhobar**2+1.0d0/24.0d0)-yval**4*
     &   (1.0d0/3.0d0/rhobar**2+1.0d0/5.0d0/rkappa-3.0d0/120.0d0)+
     &   yval**5*(1.0d0/6.0d0/rhobar**2+1.0d0/270.0d0))
      end if
      tccr_2010=177.8514d0*rhobar*cpbar*tbar/etabar*Z  !dimensionless
      tccr_2010=tccr_2010*1.0d-3 !convert to w/m-k
      RETURN
      end                                           !function TCCR_2010
c
c ======================================================================
c
      function TCXM1C (x,t,rho,ierr,herr)
c
c  based on the simplified critical enhancement of:
c  Olchowy, G.A. and Sengers, J.V. (1989). A simplified representation for
c  the thermal conductivity of fluids in the critical region.
c  Int. J. Thermophysics  10: 417-426.
c
c  also applied to CO2 by:
c  Vesovic, V., Wakeham, W.A., Olchowy, G.A., Sengers, J.V., Watson, J.T.R.
c  and Millat, J. (1990). The transport properties of carbon dioxide.
c  J. Phys. Chem. Ref. Data  19: 763-808.
c  equation numbers in comments refer to Vesovic paper
c
c  Model has been adapted to use for a mixture.
c  All coefficients except qd are fixed. Qd is a mole fraction
c  sum of the components qd's. If qd has not been supplied, a value
c  of 0.5d-9 is used. The actual mixture T, rho  (not a scaled value) is used.
c
c  This routine is designed to be called from TRNS_ECS.FOR. The routine TRNS_ECS.FOR is
c  used to supply the viscosity of the fluid at T,rho by placing it in
c  common block critenh before a call to this subroutine is made.
c
c  inputs:
c      x--composition array [mol frac]
c      t--temperature [K]
c      rho--molar density [mol/L]
c  output (as function value):
c      TCXM1C--the critical enhancement to the thermal conductivity [W/m-K]
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  06-08-97  MM, original version
c  06-16-97  MM, change DPDD, CVCP calls to DPDDK, CVCPK i.e. (t,x) to (icomp,t)
c  11-20-01  MLH, adapted for one-fluid mixture model use.
c  07-01-02  MLH, set generalized values when model not declared
c  06-13-06  EWL, modify how test for pure fluids is done
c  10-06-08  MLH, prevent ridiculous numbers for qd in tk6 model
c  06-01-11  EWL, remove common block SHAPES which was not used
c
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension x(ncmax)
c
      gnu =0.0d0
      gamma = 0.0d0
      r0 = 0.0d0
      xi0 = 0.0d0
      gam0 =0.0d0
      qd=0.0d0
c
      if (rho.lt.1.0d-6) then
c  critical enhancement is zero for low densities (avoid divide by zero)
        TCXM1C=0.0d0
        RETURN
      end if

      call ISPURE (x,icomp)
c
c  determine the coefficients
      gnux=0.0d0
      gammax=0.0d0
      R0x=0.0d0
c     zx=0.0d0
c     cviscx=0.0d0
      xi0x=0.0d0
      gam0x=0.0d0
      qdx=0.0d0
      trefx=0.0d0
      Rgasx=0.0d0
c
c     check to see if it is exactly at critical point of a pure fluid
      if (icomp.ne.0) then
        if (abs(t-tcrit(icomp)).lt.1.d-20 .and.
     &      abs(rho-Dcrit(icomp)).lt.1.d-20) then
          ierr=-60
          write (herr,1001) ierr,hnull
          call ERRMSG (ierr,herr)
 1001   format ('[TCXM1C error',i3,'] pure fluid is exactly at the ',
     &          'critical point; thermal conductivity is infinite',a1)
c
        endif
      endif
      do ii=1,nc
        i=0
        if (icomp.ne.0) then
          i=icomp
        elseif (x(ii).gt.0.d0) then
          i=ii
        endif
        if (i.gt.0) then
        if(htcxcr(i).eq.'TK3') then !use coeff loaded for TK3 model
           gnu=ctck(i,1,1)     !nu (universal exponent, approx 0.63)
           gamma=ctck(i,2,1)   !gamma (universal exponent, approx 1.24)
           R0=ctck(i,3,1)      !R0 (universal amplitude, 1.01 +/- 0.04)
c          following two parameters not used, in file for future use
c          z=ctck(i,4,1)       !z (universal exponent, 0.065 +/- 0.005)
c          cvisc=ctck(i,5,1)   !visc const, approx 1.075, but often 1)
           xi0=ctck(i,6,1)     !xi0 (amplitude, order 1d-10 m)
           gam0=ctck(i,7,1)    !gam0 (amplitude, order 0.05 - 0.06)
           qd=1.0d0/ctck(i,8,1) !qd_inverse (cutoff dia, order 10d-9 m)
           treff=ctck(i,9,1)     !treff (reference temp, 1.5 - 2.0 * Tc)
c
        elseif(htcxcrecs(i).eq.'TK6') then ! coeff for ecs enhancement
           gnu=ctcke(i,1,1)     !nu (universal exponent, approx 0.63)
           gamma=ctcke(i,2,1)   !gamma (universal exponent, approx 1.24)
           R0=ctcke(i,3,1)      !R0 (universal amplitude, 1.01 +/- 0.04)
c          following two parameters not used, in file for future use
c          z=ctcke(i,4,1)       !z (universal exponent, 0.065 +/- 0.005)
c          cvisc=ctcke(i,5,1)   !visc const, approx 1.075, but often 1)
           xi0=ctcke(i,6,1)     !xi0 (amplitude, order 1d-10 m)
           gam0=ctcke(i,7,1)    !gam0 (amplitude, order 0.05 - 0.06)
           IF(ctcke(i,8,1).le.0)ctcke(i,8,1)=0.5d-9
           qd=1.0d0/ctcke(i,8,1) !qd_inverse (cutoff dia, order 10d-9 m)
           treff=ctcke(i,9,1)    !treff (reference temp, 1.5 - 2.0 * Tc)
        else
C       set to generalized values;
C       see Vesovik et al. J. Phys. Che, Ref Data 19(3):762-808 (1990).
           gnu =0.63d0
           gamma=1.239d0
           R0=1.03d0
c          z=0.063d0
c          cvisc=1.0d0
           xi0=1.94d-10
           gam0=0.0496d0
           qd=1.0d0/0.5d-9
           treff=1.5*tcrit(i)
        endif
        if(icomp.eq.0)then         !compute mixture values
          gnux=gnux+x(i)*gnu
          gammax=gammax+x(i)*gamma
          R0x=R0x+x(i)*R0
c         zx=zx+x(i)*z
c         cviscx=cviscx+x(i)*cvisc
          xi0x=xi0x+x(i)*xi0
          gam0x=gam0x+x(i)*gam0
          qdx=qdx+x(i)*qd
          trefx=trefx+x(i)*treff
          Rgasx=Rgasx+x(i)*Reos(i)
        endif
        endif
      enddo
c
      if(icomp.eq.0)then
        gnu=gnux
        gamma=gammax
        R0=R0x
c       z=zx
c       cvisc=cviscx
        xi0=xi0x
        gam0=gam0x
        qd=qdx
        treff=trefx
        Rgas=Rgasx
      endif

      if(icomp.ne.0) then !pure fluid case
        call INFO (icomp,wmm,ttrp,tnbpt,tc,pc,Dc,Zc,acf,dip,Rgas)
C       Rgas in J/mol.K
        call DPDDK (icomp,t,rho,dpdrho)
      else             !adaptation for mixture
        call CRITP (x,tc,pc,Dc,ierr,herr)
        if (ierr.lt.0) ierr=0
        call DPDD (t,rho,x,dpdrho)
      endif
c     write (*,*) ' TCXM1C--t,dpdrho: ',t,dpdrho
c     function chi (Eq 40 in Vesovic) evaluated at t,rho and tref,rho
c     Vesovic introduces a t/tc term which is absent in other papers
c
c     chi=pc/(Dc*Dc*tc)*rho*t/dpdrho               !Vesovic form
      chi=pc/(Dc*Dc)*rho/dpdrho
      if(icomp.ne.0)then
        call DPDDK (icomp,treff,rho,dpdrho)
      else
        call DPDD (treff,rho,x,dpdrho)
      endif
c     chiref=pc/(Dc*Dc*tc)*rho*tref/dpdrho*tref/t  !Vesovic form
      chiref=pc/(Dc*Dc)*rho/dpdrho*treff/t
      delchi=chi-chiref
c
      if (delchi.le.0.0d0) then
c       delchi can go negative far from critical
c       write (*,*) ' TCX3CR--chi < chiref:  ',chi,chiref
        TCXM1C=0.0d0
        RETURN
      end if
c     function xi (Eq 46)
c     Olchowy, Vesovic put gam0 inside exponent, Krauss (R134a) puts this
c     term outside, but this yields incorrect values
      xi=xi0*(delchi/gam0)**(gnu/gamma)            !Vesovic form
c     xi=xi0/gam0*delchi**(gnu/gamma)              !Krauss' form
c     write (*,1150) chi,chiref,xi
c1150 format (1x,' TCXM1C--chi,chiref,xi:  ',3e14.6)
c     functions omega and omega_zero (Eqs 59 & 60)
c     write (*,1154) icomp,t,rho
c1154 format (1x,' TCXM1C--call CVCPK for icomp,t,rho =  ',i2,2e14.6)
      if(icomp.ne.0)then
        call CVCPK (icomp,t,rho,cv,cp)
      else
        call CVCP (t,rho,x,cv,cp)
      endif
c     write (*,1156) cv,cp
c1156 format (1x,' TCXM1C--cv,cp returned from CVCPK =  ',2e14.6)
      piinv=1.0d0/3.141592654d0
c     write (*,1160) cv,cp,qd,xi
c1160 format (1x,' TCXM1C--cv,cp,qd,xi:  ',4e14.6)
      xomg=2.0d0*piinv*((cp-cv)/cp*ATAN(qd*xi)+cv/cp*qd*xi)
      xomg0=2.0d0*piinv*(1.0d0-EXP(-1.0d0/(1.0d0/(qd*xi)
     &     +((qd*xi*Dc/rho)**2)/3.0d0)))
c
      eta=etacal    !for tk6 and general coef. model
c     eta is passed in through common block critenh
C     it is computed in trns_ecs.for using ECS
      if(icomp.ne.0)then
       if (heta(icomp).ne.'ECS') then
c       use recommended pure fluid correlation instead of ecs
        call ETAK (icomp,t,rho,eta,ierr,herr) !for tk3 or other models
       endif
      endif
c
c     write (*,1162) xomg,xomg0,eta
c1162 format (1x,' TCXM1C--omega,omega_0,eta:  ',3e14.6)
      boltz=Rgas/6.0221367d23       !Boltzman's const
c     factor of 1d9 in next equation to convert from mol/L --> mol/m**3
c     and from micro-Pa-s to Pa-s
      TCXM1C=0.d0
      if (eta.gt.1.d-16) TCXM1C=rho*1.0d9*cp*R0*boltz*t*piinv
     &                         /(6.0d0*eta*xi)*(xomg-xomg0)
c
c     write (*,*) ' TCXM1C--TCXM1C: ',TCXM1C
c
      RETURN
      end                                               !function TCXM1C
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file trns_TCX.f
c ======================================================================
