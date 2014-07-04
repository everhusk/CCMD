c  begin file trnsp.f
c
c  This file contains the transport property routines
c
c  contained here are:
c     subroutine TRNPRP (t,rho,x,eta,tcx,ierr,herr)
c     subroutine ETAK (icomp,t,rho,eta,ierr,herr)
c     subroutine ETAK0 (icomp,t,eta0,ierr,herr)
c     subroutine ETAK1 (icomp,t,eta1,ierr,herr)
c     subroutine ETAKR (icomp,t,rho,etar,ierr,herr)
c     subroutine ETAKB (icomp,t,rho,etab,ierr,herr)
c     subroutine TCXK (icomp,t,rho,tcx,ierr,herr)
c     subroutine TCXK0 (icomp,t,tcx0,ierr,herr)
c     subroutine TCXKB (icomp,t,rho,tcxb,ierr,herr)
c     subroutine TCXKC (icomp,t,rho,tcxc,ierr,herr)
c     function OMEGA (icomp,t,epsk,hmodci)
c     subroutine SETCI1 (nread,icomp,hcasno,ierr,herr)
c     function OMEGA1 (icomp,t,epsk)
c     subroutine SETCI2 (nread,icomp,hcasno,ierr,herr)
c     function OMEGA2 (icomp,t,epsk)
c
c =====================================================================
c =====================================================================
c
      subroutine TRNPRP (t,rho,x,eta,tcx,ierr,herr)
c
c  compute the transport properties of thermal conductivity and
c  viscosity as functions of temperature, density, and composition
c
c  inputs:
c        t--temperature [K]
c      rho--molar density [mol/L]
c        x--composition array [mol frac]
c  outputs:
c      eta--viscosity (uPa.s)
c      tcx--thermal conductivity (W/m.K)
c     ierr--error flag:  0 = successful
c                      -31 = temperature out of range for conductivity
c                      -32 = density out of range for conductivity
c                      -33 = T and D out of range for conductivity
c                      -41 = temperature out of range for viscosity
c                      -42 = density out of range for viscosity
c                      -43 = T and D out of range for viscosity
c                      -51 = T out of range for both visc and t.c.
c                      -52 = D out of range for both visc and t.c.
c                      -53 = T and/or D out of range for visc and t.c.
c                       39 = model not found for thermal conductivity
c                       40 = model not found for thermal conductivity or viscosity
c                       49 = model not found for viscosity
c                       50 = ammonia/water mixture (no properties calculated)
c                       51 = exactly at t, rhoc for a pure fluid; k is infinite
c                  -58,-59 = ECS model did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-15-96  MM, original version, supercedes older routine of same name
c  02-21-97  MM, check input T,rho against limits for pures,
c                generate warning messages
c  08-20-97  MM, initialize visc and t.c. to flag indicating not calculated
c  10-01-97  MM, add compiler switch to allow access by DLL
c  02-06-98  MM, move ierret=0 so it is always initialized
c  08-14-98  MM, x array never dimensioned!?
c  01-25-00 EWL, switched "eta, tcxecs" to "etaecs, tcx" in one of the ECS calls
c  05-24-00 EWL, if no transport routines are loaded, do not calculate values.
c                this is important for D2, Fl, and NF3 which have no exp. data
c  11-01-01 EWL, black out ammonia/water mixtures
c  11-02-01 MLH, block out computation exactly at tc, rhoc for pure fluid k (its infinite)
c  09-08-04 MLH, names shortened to 6 characters
c  01-04-07 EWL, add check for mixture component with no equation
c  01-10-07 EWL, check for mixtures with water
c  01-18-07 MLH, do not switch to ECS if out of range of correlation
c  03-12-07 EWL, keep eta and tcx equal to xnota if no correlation instead of switching to xnotc at the very bottom
c  03-12-07 EWL, check for neither tcx or eta models at beginning and exit if so
c  11-16-07 MLH, updated error messages for ecs mixture failure
c  04-02-08 EWL, add check for ic<>0 with water mixtures (so that PUREFLD can be called)
c  04-21-08 EWL, add check for ic<>0 with any mixture (so that PUREFLD can be called)
c  05-08-08 MLH, add check for viscosity exactly at critical point
c  03-09-09 EWL, allow water mixtures if interaction parameters are available
c  10-28-10 EWL, remove composition dependence if nc=1
c  06-05-12 EWL, increase allowable water to 5%
c  01-24-13 MLH, don't allow transport with alcohol mixtures
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TRNPRP
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr,herrl,herrec,herrpf
      logical lecs
      dimension x(ncmax)
c
      ierr=0
      herr=' '
      call ISPURE (x,icomp)
      eta=xnotc
      tcx=xnotc
      if (icomp.ne.0) then
        if  ((heta(icomp).eq.'NUL' .or. heta(icomp).eq.'   ')
     &  .and.(htcx(icomp).eq.'NUL' .or. htcx(icomp).eq.'   ')) then
          ierr=40
          write (herr,1006) ierr,hnull
          call ERRMSG (ierr,herr)
          RETURN
        endif
      else
        do i=1,nc
          if (x(i).gt.0.d0) then
            if  ((heta(i).eq.'NUL' .or. heta(i).eq.'   ')
     &      .and.(htcx(i).eq.'NUL' .or. htcx(i).eq.'   ')) then
              ierr=40
              write (herr,1006) ierr,hnull
              call ERRMSG (ierr,herr)
              RETURN
            endif
          endif
        enddo
      endif
 1006 format ('[TRNPRP error',i3,'] transport equations are ',
     &        'not available for one or more of the fluids',a1)

      lecs=.false.
      if (icomp.eq.0) then
        iw1=1
        do i=1,nc-1
          if (x(i).gt.0.d0) then
            iw2=0
            do j=i+1,nc
              if (x(j).gt.0.d0) then
                if (abs( xljs(i,j)).gt.1.d-20) iw2=iw2+1
                if (abs( xlje(i,j)).gt.1.d-20) iw2=iw2+1
                if (abs( xkij(i,j)).gt.1.d-20) iw2=iw2+1
                if (abs(xkijk(i,j)).gt.1.d-20) iw2=iw2+1
                if (abs(xlijk(i,j)).gt.1.d-20) iw2=iw2+1
              endif
            enddo
            iw1=iw1*iw2
          endif
        enddo
        if (iw1.eq.0) then
          if (iamwat.eq.1) then
            ierr=50
            write (herr,1001) ierr,hnull
            call ERRMSG (ierr,herr)
 1001       format ('[TRNPRP error',i3,'] transport equations are ',
     &              'not available for the ammonia/water mixture',a1)
            RETURN
          end if
          if (ialc.ne.0) then
            ierr=50
            write (herr,1011) ierr,hnull
            call ERRMSG (ierr,herr)
 1011      format ('[TRNPRP error',i3,'] transport equations are ',
     &              'not available for mixtures with alcohols',a1)
            RETURN
          end if

          if (iwat.ne.0) then
            if (x(iwat).gt.0.05d0) then
              ierr=50
              write (herr,1005) ierr,hnull
              call ERRMSG (ierr,herr)
 1005         format ('[TRNPRP error',i3,'] transport equations are ',
     &                'not available for mixtures with water at ',
     &                'molar concentrations greater than 5%',a1)
              RETURN
            end if
          end if
        end if
      end if
c  set visc and t.c. to flags indicating 'not calculated' so that some
c  value is returned to GUI in event of failure of routines
c
      ierret=0         !initialize
      if (icomp.ne.0) then
c  no model was specified, so do not calculate any properties
        if (heta(icomp)(1:1).eq.' ' .or. heta(icomp).eq.'NUL') then
          eta=xnota
c  special case--pure fluid
        elseif (heta(icomp)(1:2).eq.'EC') then
c  call ECS subroutines
c         write (*,*) ' TRNPRP--about to call TRNECS for viscosity'
          call TRNECS (t,rho,x,eta,tcxecs,ierrec,herr)
          ierr=ierrec
          lecs=.true.  !set flag (no need to call TRNECS again for t.c.)
        else
c  pure fluid correlation
          pxx=0.0d0
          call LIMITX ('ETA',t,rho,pxx,x,tmin,tmax,Dmax,pmax,ierr,herrl)
c         write (*,1002) tmin,tmax,Dmax,ierr
c1002     format (1x,' TRNPRP--T,rho limits for visc: ',2f8.2,f10.4,
c    &               '; ierr = ',i3)
          if (ierr.ge.1) then
c  if out of range, call ECS model and generate warning message
            ierr=-40-ierr
            write (herr,1040) ierr,herrl(39:168),hnull
            call ERRMSG (ierr,herr)
            ierret=ierr
c           do NOT call ecs, just give warning
c            call TRNECS (t,rho,x,eta,tcxecs,ierrec,herrec)
c            lecs=.true.
c            if (ierrec.ne.0) then
c  if ECS method results in error, pass that as output, otherwise retain
c  the error from LIMITX
c              write (herr,1004) ierr,herrec(1:146),hnull
c             call ERRMSG (ierr,herr)
 1004         format ('[TRNPRP warning',i4,'] pure fluid correlation ',
     &                'out of range; attempted to use ECS method, it ',
     &                'generated error:  ',a146,a1)
c             ierr=ierrec
c            end if
          else
c  call pure fluid model
            lecs=.false.
c           check to see if viscosity is requested at exactly the crit point
            IF(abs(t-tcrit(icomp)).lt.1.d-20 .AND.
     &         abs(rho-Dcrit(icomp)).lt.1.d-20) then
              ierr=-51
              write (herr,1002) ierr,hnull
              call ERRMSG (ierr,herr)
 1002         format ('[TRNPRP warning ',i3,'] pure fluid ',
     &         'property is infinite at exactly Tc, rhoc',a1)
              eta=xnotc
            end if
            call ETAK (icomp,t,rho,eta,ierr,herr)
          end if
        end if
c
c  no model was specified, so do not calculate any properties
        if (htcx(icomp)(1:1).eq.' ' .or. htcx(icomp).eq.'NUL') then
          tcx=xnota
c  call ECS subroutines
        elseif (htcx(icomp)(1:2).eq.'EC') then
          if (lecs) then
            tcx=tcxecs       !use value computed on call above
          else
            call TRNECS (t,rho,x,etaecs,tcx,ierrec,herrec)
            if (ierrec.ne.0) then
c  if ECS method results in error, pass that as output, otherwise retain
c  the error from LIMITX
              write (herr,1004) ierr,herrec(1:146),hnull
              call ERRMSG (ierr,herr)
              ierr=ierrec
            end if
          end if
        else
c  pure fluid correlation
          pxx=0.0d0
          call LIMITX ('TCX',t,rho,pxx,x,tmin,tmx,Dmx,pmx,ierrl,herrl)
c         write (*,1003) tmin,tmx,Dmx,ierrl
c1003     format (1x,' TRNPRP--T,rho limits for t.c.: ',2f8.2,f10.4,
c    &               '; ierr = ',i3)
          if (ierrl.ge.1) then
c  out of range of thermal conductivity correlation, call ECS model
            if (ierret.ne.0) then
c  both thermal conductivity and viscosity are out of range
              if (ierret.eq.-41) then
                ierr=-50-ierrl
                write (herr,1050) ierr,herrl(39:153),hnull
                call ERRMSG (ierr,herr)
              else if (ierret.eq.-42) then
                if (ierrl.eq.2) then
                  ierr=-52
                  write (herr,1052) hnull
                  call ERRMSG (ierr,herr)
                else
                  ierr=-53
                  write (herr,1053) hnull
                  call ERRMSG (ierr,herr)
                end if
              else
                ierr=-53
                write (herr,1053) hnull
                call ERRMSG (ierr,herr)
              end if
            else
c  just thermal conductivity correlation is out of range
              ierr=-30-ierrl
              write (herr,1030) ierr,herrl(39:168),hnull
              call ERRMSG (ierr,herr)
            end if
            if (lecs) then
              tcx=tcxecs       !use value computed on call above
            else
c           do NOT switch to ECS
c             call TRNECS (t,rho,x,etaecs,tcx,ierrec,herrec)
c              if (ierrec.ne.0) then
c  if ECS method results in error, pass that as output, otherwise retain
c  the error from LIMITX
c                write (herr,1004) ierr,herrec(1:146),hnull
c                call ERRMSG (ierr,herr)
c              ierr=ierrec
c              end if
            end if
          else
c  call pure fluid model
            call TCXK (icomp,t,rho,tcx,ierrpf,herrpf)
            if (ierrpf.ne.0) then
c  if pure fluid model results in error, pass that as output, otherwise
c  retain the error from LIMITX
              ierr=ierrpf
              herr=herrpf
            end if
          end if
        end if
c       check to see if thermal conductivity is requested at exactly the crit point
        IF(abs(t-tcrit(icomp)).lt.1.d-20 .AND.
     &     abs(rho-Dcrit(icomp)).lt.1.d-20) then
          ierr=51
          write (herr,1002) ierr,hnull
          call ERRMSG (ierr,herr)
          tcx=xnotc
        end if


      else
c
c  general (mixture) case
c
        if (hetamx(1:2).eq.'EC') then
c  call ECS subroutines
          call TRNECS (t,rho,x,eta,tcxecs,ierr,herr)
          lecs=.true.  !set flag (no need to call TRNECS again for t.c.)
          IF(ierr.eq.1)then
            ierr=-54
            write (herr,1054) hnull
            call ERRMSG (ierr,herr)
          endif
        else
          lecs=.false.
c  [insert calls to additional mixture models here]
        end if
c
        if (htcxmx(1:2).eq.'EC') then
c  call ECS subroutines
          if (lecs) then
            tcx=tcxecs       !use value computed on call above
          else
            call TRNECS (t,rho,x,etaecs,tcx,ierr,herr)
            IF(ierr.eq.1)then
              ierr=-54
              write (herr,1054) hnull
              call ERRMSG (ierr,herr)
            endif
          end if
        else
          lecs=.false.
c  [insert calls to additional mixture models here]
        end if
      end if
      if (eta.lt.0 .and. eta.ne.xnota) eta=xnotc
      if (tcx.lt.0 .and. tcx.ne.xnota) tcx=xnotc
c
c     do i=1,nc      !Removed 4/21/08, should have been done earlier
c       if (heta(i).eq.'NUL' .or. heta(i).eq.'   ') eta=xnota
c       if (htcx(i).eq.'NUL' .or. htcx(i).eq.'   ') tcx=xnota
c     enddo
      RETURN
c
 1030 format ('[TRNPRP warning',i4,'] one or more inputs to the ',
     &        'thermal conductivity correlation are out of range; ',
     &        'results may be in error; ',a130,a1)
 1040 format ('[TRNPRP warning',i4,'] one or more inputs to the ',
     &        'viscosity correlation are out of range; ',
     &        'results may be in error; ',a130,a1)
 1050 format ('[TRNPRP warning',i4,'] one or more inputs to the ',
     &        'thermal conductivity and viscosity correlations are ',
     &        'out of range; results may be in error; ',a115,a1)
 1052 format ('[TRNPRP warning -52] the density input to the ',
     &        'thermal conductivity and viscosity correlations are ',
     &        'out of range; will use the ECS model. ',a1)
 1053 format ('[TRNPRP warning -53] temperature and/or density input ',
     &        'to the thermal conductivity and/or viscosity ',
     &        'correlations are out of range; will use the ECS model.',
     &         a1)
 1054 format ('[TRNPRP warning -54] temperature and/or density input ',
     &        'to the thermal conductivity and/or viscosity ',
     &        'ecs mixture model are out of range;',
     &        ' results may be in error.',
     &         a1)

      end                                             !subroutine TRNPRP
c
c ======================================================================
c
      subroutine ETAK (icomp,t,rho,eta,ierr,herr)
c
c  viscosity of a pure fluid as a function of temperature and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c      eta--viscosity (uPa.s)
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       49 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-15-96  MM, original version
c  10-30-96  MM, call intermediate ETAK0, etc routines rather than
c                core-level routines directly
c  01-17-97  MM, change initial-density dependence to visc virial coeff
c  11-19-07  MLH, initialized value, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      eta=0.0d0
      ierr=0
      herr=' '
c
c  the viscosity is the sum of the dilute-gas, initial-density, and
c  residual terms (separate functions for the three terms are used
c  because of the requirements of certain mixture models, e.g. ECS)
c
      call ETAK0(icomp,t,eta0,ierr,herr)
      call ETAK1(icomp,t,etaB2,ierr,herr)
      call ETAKR(icomp,t,rho,etar,ierr,herr)
      eta=eta0*(1.0d0+etaB2*rho)+etar


c     write (*,1000) icomp,t,rho,eta0,eta0*etaB2*rho,etar,eta
c1000 format (1x,' ETAK--icomp,t,rho,eta_0,B2,resid,sum:  ',i3,6f12.4)
c
      RETURN
      end                                               !subroutine ETAK
c
c ======================================================================
c
      subroutine ETAK0 (icomp,t,eta0,ierr,herr)
c
c  dilute-gas contribution to the viscosity of a pure fluid as a
c  function of temperature and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  outputs:
c     eta0--viscosity [uPa.s]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       49 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  03-07-00 EWL, add VS0 model
c  03-05-02 EWL, initialize eta0
c  03-21-02 MLH, modified VS0 model to have dilute gas piece
c  12-26-06 MLH, added vs4 model (generalized friction theory)
c  11-02-07 MLH, added vs5 model (Chung et al.)
c  12-10-07 MLH, added vs4 dilute call
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      eta0=0.d0
      herr=' '
c
c  call appropriate core viscosity model
      if (heta(icomp)(1:2).eq.'EC') then
        eta0=ETA0DG(icomp,t)
      else if (heta(icomp).eq.'VS0') then
        eta0=ETA0DG(icomp,t)
      else if (heta(icomp).eq.'VS1') then
        eta0=ETA1DG(icomp,t)
      else if (heta(icomp).eq.'VS2') then
        eta0=ETA2DG(icomp,t)
      else if (heta(icomp).eq.'VS3') then
        eta0=ETA3DG(icomp,t)
      else if (heta(icomp).eq.'VS4') then
        eta0=ETA4DG(icomp,t)
      else if (heta(icomp).eq.'VS5') then
        eta0=ETA5DG(icomp,t)
      else if (heta(icomp).eq.'VS6') then
c       eta0=ETA6DG(icomp,t)
        eta0=0.0d0                  !temporary
        ierr=49
        herr='[ETAK0 error 49] call to unimplemented model (VS6)'
        call ERRMSG (ierr,herr)
      else
        ierr=49
        herr='[ETAK0 error 49] unknown viscosity model specified'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                              !subroutine ETAK0
c
c ======================================================================
c
      subroutine ETAK1 (icomp,t,etaB2,ierr,herr)
c
c  the second viscosity virial coefficient (initial-density contribution)
c  of a pure fluid as a function of temperature
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  outputs:
c    etaB2--second viscosity virial coefficient [L/(mol-uPa-s)]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       49 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  01-21-97  MM, call ETA1B2 for model VS1
c  03-07-00 EWL, add VS0 model
c  03-06-02 EWL, initialize etaB2
c  12-26-06 MLH, added vs4 model
c  12-10-07 MLH, added possible etab2 for vs4 model
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      etaB2=0.0d0
      herr=' '
c
c  call appropriate core viscosity model
c  note that only the VS1 pure-fluid model presently implements an
c  explicit initial-density term
      if (heta(icomp).eq.'VS0') then
        etaB2=0.0d0
      else if (heta(icomp).eq.'VS1') then
        etaB2=ETA1B2(icomp,t)
      else if (heta(icomp).eq.'VS2') then
        etaB2=0.0d0                  !no initial-density term for Y+E
      else if (heta(icomp).eq.'VS3') then
        etaB2=0.0d0
      else if (heta(icomp).eq.'VS4') then
        etaB2=ETA1B2(icomp,t)
      else if (heta(icomp).eq.'VS5') then
        etaB2=0.0d0
      else if (heta(icomp).eq.'VS6') then
c       etaB2=ETA6B2(icomp,t)
        etaB2=0.0d0                  !temporary
      else
        ierr=49
        herr='[ETAK1 error 49] unknown viscosity model specified'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                              !subroutine ETAK1
c
c ======================================================================
c
      subroutine ETAKR (icomp,t,rho,etar,ierr,herr)
c
c  residual contribution to the viscosity of a pure fluid as a function
c  of temperature and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c     etar--viscosity [uPa.s]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       49 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  03-07-00 EWL, add VS0 model
c  03-05-02 EWL, initialize etar
c  03-21-02 MLH, modified VS0 model to have residual piece
c  12-26-06 MLH, added vs4 model
c  11-02-07 MLH, added Chung model
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      etar=0.0d0
      herr=' '
c
c  call appropriate core viscosity model
      if (heta(icomp).eq.'VS0') then
c       eta0hc provides full viscosities; subtract dilute to get resid
        etar=ETA0HC(icomp,t,rho,ierr,herr)-ETA0DG(icomp,t)
      else if (heta(icomp).eq.'VS1') then
        etar=ETA1RS(icomp,t,rho)
      else if (heta(icomp).eq.'VS2') then
        etar=ETA2RS(icomp,t,rho)
      else if (heta(icomp).eq.'VS3') then
        etar=ETA3RS(icomp,t,rho)
      else if (heta(icomp).eq.'VS4') then
        etar=ETA4RS(icomp,t,rho)
      else if (heta(icomp).eq.'VS5') then
        etar=ETA5RS(icomp,t,rho)
      else if (heta(icomp).eq.'VS6') then
c       etar=ETA6RS(icomp,t,rho)
        etar=0.0d0                  !temporary
        ierr=49
        herr='[ETAKR error 49] call to unimplemented model (VS6)'
        call ERRMSG (ierr,herr)
      else
        ierr=49
        herr='[ETAKR error 49] unknown viscosity model specified'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                              !subroutine ETAKR
c
c ======================================================================
c
      subroutine ETAKB (icomp,t,rho,etab,ierr,herr)
c
c  background contribution to the viscosity of a pure fluid as a
c  function of temperature and density; this includes both the residual
c  term and any initial density (second virial) term
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c     etab--background viscosity [uPa.s]
c     ierr--error flag:  0 = successful
c                      <>0 = error in underlying routine, passed up
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  02-04-97  MM, original version
c  11-19-07  MLH, added init values, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      etab=0.0d0
      ierr=0
      herr=' '
c
c  the viscosity is the sum of the dilute-gas, initial-density, and
c  residual terms (separate functions for the three terms are used
c  because of the requirements of certain mixture models, e.g. ECS);
c  this routine returns the sum of the initial-density and residual terms
c
      call ETAK0(icomp,t,eta0,ierr,herr)
      call ETAK1(icomp,t,etaB2,ierr,herr)
      call ETAKR(icomp,t,rho,etar,ierr,herr)
      etab=eta0*etaB2*rho+etar
c     write (*,1000) icomp,t,rho,eta0,eta0*etaB2*rho,etar,etab
c1000 format (1x,' ETAKB--icomp,t,rho,etao,B2,resid,sum:  ',i3,6f12.4)
c
      RETURN
      end                                              !subroutine ETAKB
c
c ======================================================================
c
      subroutine TCXK (icomp,t,rho,tcx,ierr,herr)
c
c  thermal conductivity of a pure fluid as a function of temp and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c      tcx--thermal conductivity [W/m-K]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       39 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-15-96  MM, original version
c  10-30-96  MM, call intermediate TCXK0, etc routines rather than
c                core-level routines directly
c  11-16-07  MLH, added initial values, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      herr=' '
      tcx=0.0d0
c
c  the thermal conductivity is the sum of the dilute-gas, background
c  (or residual), and critical enhancement terms (separate functions
c  for the three terms are used because of the requirements of certain
c  mixture models, e.g. ECS)
c
      call TCXK0(icomp,t,tcx0,ierr,herr)
      call TCXKB(icomp,t,rho,tcxb,ierr,herr)
      call TCXKC(icomp,t,rho,tcxc,ierr,herr)
      tcx=tcx0+tcxb+tcxc
c     write (*,1000) icomp,t,rho,tcx0,tcxb,tcxc,tcx
c1000 format (1x,' TCXK--icomp,t,rho,tcx_0,bk,crit,sum:  ',i3,6f12.6)
c
      RETURN
      end                                               !subroutine TCXK
c
c ======================================================================
c
      subroutine TCXK0 (icomp,t,tcx0,ierr,herr)
c
c  dilute-gas part of the thermal conductivity of a pure fluid as a
c  function of temp and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c  outputs:
c     tcx0--thermal conductivity [W/m-K]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       39 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  03-07-00 EWL, add TC0 model
c  11-05-07 MLH, add TC5 model, initial values
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      herr=' '
      tcx0=0.0d0
c
c  call appropriate core thermal conductivity model
      if (htcx(icomp).eq.'TC0') then
        tcx0=0.0d0
      else if (htcx(icomp).eq.'TC1') then
        tcx0=TCX1DG(icomp,t)
      else if (htcx(icomp).eq.'TC2') then
        tcx0=TCX2DG(icomp,t)
      else if (htcx(icomp).eq.'TC3') then
        tcx0=TCX3DG(icomp,t)
      else if (htcx(icomp).eq.'TC5') then
        tcx0=TCX5DG(icomp,t)
      else if (htcx(icomp).eq.'TC6') then
c       tcx0=TCX6DG(icomp,t)
        tcx0=0.0d0                   !temporary
        ierr=39
        herr='[TCXK0 error 39] call to unimplemented model (TC6)'
        call ERRMSG (ierr,herr)
      else
        ierr=39
        herr='[TCXK0 error 39] unknown thermal conductivity model'//
     &       ' specified'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                              !subroutine TCXK0
c
c ======================================================================
c
      subroutine TCXKB (icomp,t,rho,tcxb,ierr,herr)
c
c  background part of the thermal conductivity of a pure fluid as a
c  function of temp and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c     tcxb--thermal conductivity [W/m-K]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       39 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  03-07-00 EWL, add TC0 model
c  11-05-07 MLH, add TC5 model, initial values
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
c
      ierr=0
      herr=' '
      tcxb=0.0d0
c
c  call appropriate core thermal conductivity model
      if (htcx(icomp).eq.'TC0') then
        tcxb=TCX0HC(icomp,t,rho,ierr,herr)
      elseif (htcx(icomp).eq.'TC1') then
        tcxb=TCX1BK(icomp,t,rho)
      else if (htcx(icomp).eq.'TC2') then
        tcxb=TCX2BK(icomp,t,rho)
      else if (htcx(icomp).eq.'TC3') then
        tcxb=TCX3BK(icomp,t,rho)
      else if (htcx(icomp).eq.'TC5') then
        tcxb=TCX5BK(icomp,t,rho)
      else if (htcx(icomp).eq.'TC6') then
c       tcxb=TCX6BK(icomp,t,rho)
        tcxb=0.0d0                   !temporary
        ierr=39
        herr='[TCXKB error 39] call to unimplemented model (TC6)'
        call ERRMSG (ierr,herr)
      else
        ierr=39
        herr='[TCXKB error 39] unknown thermal conductivity model'//
     &       ' specified'//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                              !subroutine TCXKB
c
c ======================================================================
c
      subroutine TCXKC (icomp,t,rho,tcxc,ierr,herr)
c
c  critical enhancement for the thermal conductivity of a pure fluid as
c  a function of temp and density
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c      rho--molar density [mol/L]
c  outputs:
c     tcxc--thermal conductivity [W/m-K]
c     ierr--error flag:  0 = successful
c                        1 = did not converge
c                       39 = unknown model specified
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-30-96  MM, original version
c  02-24-97  MM, add /CREMOD/
c  11-06-00 EWL, add water
c  07-01-02 MLH, add TK6
c  11-19-07 MLH, added initial values, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*255 herr
      dimension xfeed(ncmax)
c
      ierr=0
      herr=' '
      tcxc=0.0d0
c
c  call appropriate core thermal conductivity model
c     write (*,1100) icomp,htcxcr(icomp),t,rho
c1100 format (1x,' TCXKC--icomp,htcxcr:  ',i2,2x,a3,'; at t,rho = ',
c    &            2e14.6)
      if (htcxcr(icomp).eq.'TK1') then
        tcxc=TCX1CR(icomp,t,rho)
      else if (htcxcr(icomp).eq.'TK2') then
c  hydrocarbon model of Younglove & Ely (critical part integral w/ TC2)
        tcxc=TCX2CR(icomp,t,rho)
      else if (htcxcr(icomp).eq.'TK3') then
c       write (*,*) ' TCXKC--call critical model TK3'
        tcxc=TCX3CR(icomp,t,rho)
c       write (*,*) ' TCXKC--critical enhancement from TK3: ',tcxc
      else if (htcxcr(icomp).eq.'TK4') then
        tcxc=TCX4CR(icomp,t,rho)
      else if (htcxcr(icomp).eq.'TK6') then
        do i=1,ncmax
          xfeed(i)=0.d0
        enddo
        xfeed(icomp)=1.0d0
        tcxc=TCXM1C(xfeed,t,rho,ierr,herr)
c       tcxc=0.0d0                   !temporary
c       ierr=39
c       herr='[TCXKC error 39] call to unimplemented model (TK6)'
c       call ERRMSG (ierr,herr)
      else if (htcxcr(icomp).eq.'NUL') then
c  no critical enhancement is used
        tcxc=0.0d0
      else if (htcxcr(icomp).eq.'NH3') then
c  special function for ammonia; model of Tufeu
        tcxc=TCCNH3(icomp,t,rho)
      else if (htcxcr(icomp).eq.'CH4') then
c  special function for methane; model of Friend et al.
        tcxc=TCCCH4(icomp,t,rho)
      else
        ierr=39
        herr='[TCXKC error 39] unknown thermal conductivity critical'//
     &       ' enhancement model specified: '//htcxcr(icomp)//hnull
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                              !subroutine TCXKC
c
c ======================================================================
c
      function OMEGA (icomp,t,epsk,hmodci)
c
c  collision integral for a pure fluid as a function of temperature and
c  the Lennard-Jones epsilon/k parameter
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c     epsk--Lennard-Jones epsilon/k parameter [K]
c  output (as function value):
c    omega--the collision integral
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-16-96  MM, original version
c  01-31-97  MM, add case of model = 'NUL'
c  03-07-00  EWL, add CI0 model
c  11-19-07  MLH, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
      character*3 hmodci
c
c  call appropriate function for collision integral
      omega=1.0d0
      if (hmodci.eq.'CI1') then
        OMEGA=OMEGA1(icomp,t,epsk)
      else if (hmodci.eq.'CI2') then
        OMEGA=OMEGA2(icomp,t,epsk)
      else if (hmodci.eq.'CI0') then
        if (abs(epsk).gt.1.d-20) OMEGA=OMEGAS(2,2,t/epsk)
      else if (hmodci.eq.'NUL') then
c  collision integral not used; 'NUL' is place-holder in fluid file
c  should not ever get here, but return value to avoid error
        OMEGA=1.0d0   !return 1, as omega appears in denominator
      else
c       write (*,*) '[OMEGA error] unknown col integral: ',hmodci
        OMEGA=1.0d0   !return 1, as omega appears in denominator
      end if
c
      RETURN
      end                                                !function OMEGA
c
c ======================================================================
c
      subroutine SETCI1 (nread,icomp,hcasno,ierr,herr)
c
c  initialize the collision integral; this is an empirical function in
c  log(T), the form used by several authors including Fenghour (1995) for
c  ammonia and Krauss (1996) for R152a; this same form is used for the
c  "reduced effective collision cross-section" used by Wilhelm & Vogel
c  (1995) and Laesecke (1997) for R134a
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
c  01-21-97  MM, original version (based on SETCI2)
c  05-14-97  MM, change power of T coefficient to integer, add to /WIFOMG/
c  08-19-97  MM, change error number for nread<=0; input hcasno is not array
c  12-02-97  MM, skip over limits on file read
c  08-13-98  MM, delete obsolete (unused) format statement
c
      include 'commons.for'
      include 'comtrn.for'
      character*12 hcasno
      character*255 herr
c
c  commons storing the coefficients to the fit for collision integral
c
      if (nread.le.0) then
c  get coefficients from block data--this option not implemented,
c  place holder to maintain parallel structure with EOS setup routines
        ierr=101
        write (herr,1101) nread,hcasno,hnull
        call ERRMSG (ierr,herr)
 1101   format ('[SETCI1 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETCI1--read component',icomp,' from unit',nread
c  limits are not used with collision integral, but retain for compatibility
        read (nread,*) !tmin       !lower temperature limit (dummy)
        read (nread,*) !tmax       !upper temperature limit (dummy)
        read (nread,*) !pjunk      !upper pressure limit (n/a)
        read (nread,*) !rhojnk     !upper density limit (n/a)
        read (nread,*) nterm       !number of terms
        ntomg(icomp)=nterm
        if (ntomg(icomp).ge.1) then
          do j=1,ntomg(icomp)
c  first is numerical coeff, second is power of T
            read (nread,*) comg(icomp,j,1),icomg(icomp,j)
          enddo
        end if
c       write (*,*) ' SETCI1--final coeff: ',comg(icomp,nterm,1)
        ierr=0
        herr=' '
      end if
c
      RETURN
      end                                             !subroutine SETCI1
c
c ======================================================================
c
      function OMEGA1 (icomp,t,epsk)
c
c  collision integral for a pure fluid as a function of temperature and
c  the Lennard-Jones epsilon/k parameter; this is an empirical function
c  of a form used by several authors including Fenghour (1995) for
c  ammonia, Krauss (1996) for R152a, Laesecke (1997) for R134a, and
c  Vesovic (1990) for carbon dioxide.
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c     epsk--Lennard-Jones energy (epsilon/k) parameter [K]
c  output (as function value):
c    omega--the collision integral
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  01-21-97  MM, original version (based on OMEGA2)
c  05-14-97  MM, change power of T coefficient to integer, add to /WIFOMG/
c  01-19-00  EWL, check for tstar=0 and avoid 0**0 error message
c  11-19-07  MLH, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
      omega1=1.0d0
      tstar=1.0d-20
      omgsum=0.0d0
      if (abs(epsk).gt.1.d-20) tstar=LOG(t/epsk)
      if (ABS(tstar).lt.1.0d-20) tstar=1.0d-20
      do n=1,ntomg(icomp)
        omgsum=omgsum+comg(icomp,n,1)*tstar**icomg(icomp,n)
      enddo
      OMEGA1=EXP(omgsum)
      RETURN
      end                                               !function OMEGA1
c
c ======================================================================
c
      subroutine SETCI2 (nread,icomp,hcasno,ierr,herr)
c
c  initialize the collision integral function of Younglove and Ely,
c  JPCRD 16:577-798 (1987)
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
c  10-16-96  MM, original version
c  01-21-97  MM, add /WIFOMG/ (store # terms) for parallel w/ OMEGA1
c  05-14-97  MM, add icomg to /WIFOMG/ (used in OMEGA1)
c  08-19-97  MM, change error number for nread<=0
c  12-02-97  MM, skip over limits on file read
c  08-13-98  MM, delete obsolete (unused) format statement
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
 1101   format ('[SETCI2 error 101] illegal file specified; nread = ',
     &          i4,'; CAS no. = ',a12,a1)
      else
c  read data from file (should have been opened by SETUP)
c       write (*,*) ' SETCI2--read component',icomp,' from unit',nread
c  limits are not used with collision integral, but retain for compatibility
        read (nread,*) !tmin       !lower temperature limit (dummy)
        read (nread,*) !tmax       !upper temperature limit (dummy)
        read (nread,*) !pjunk      !upper pressure limit (n/a)
        read (nread,*) !rhojnk     !upper density limit (n/a)
        read (nread,*) nterm       !number of terms
        ntomg2(icomp)=nterm
        if (nterm.ge.1) then
          do j=1,nterm
            read (nread,*) comg2(icomp,j,1)
c  the second element in coeff array is not used for this model
            comg2(icomp,j,2)=0.0d0
          enddo
        end if
c       write (*,*) ' SETCI1--final coeff: ',comg2(icomp,nterm,1)
        ierr=0
        herr=' '
      end if
c
      RETURN
      end                                             !subroutine SETCI2
c
c ======================================================================
c
      function OMEGA2 (icomp,t,epsk)
c
c  collision integral for a pure fluid as a function of temperature and
c  the Lennard-Jones epsilon/k parameter; based on Equation (20) in
c  Younglove and Ely, JPCRD 16:577-798 (1987).
c
c  N.B.  there is misprint in Younglove & Ely, the exponent
c        is ((4-n)/3) not ((n+2)/3)
c
c  inputs:
c    icomp--component number in mixture (1..nc); 1 for pure fluid
c        t--temperature [K]
c     epsk--Lennard-Jones energy (epsilon/k) parameter [K]
c  output (as function value):
c    omega--the collision integral
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  10-16-96  MM, original version
c  01-21-97  MM, add /WIFOMG/ to maintain parallel w/ OMEGA1
c  05-14-97  MM, add icomg to /WIFOMG/ (used in OMEGA1)
c  11-19-07  MLH, removed unused commons, declarations
c
      include 'commons.for'
      include 'comtrn.for'
c
      ekt3=1.0d0
      omega2=1.0d0
      omgsum=0.0d0
      if (t.gt.0.d0) ekt3=(epsk/t)**(1.0d0/3.0d0)
      do n=1,ntomg2(icomp)
        omgsum=omgsum+comg2(icomp,n,1)*ekt3**(4-n)
      enddo
      if (abs(omgsum).gt.1.d-20) OMEGA2=1.0d0/omgsum
      RETURN
      end                                               !function OMEGA2
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                       end file trnsp.f
c ======================================================================
