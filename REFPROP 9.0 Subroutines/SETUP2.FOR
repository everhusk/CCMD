c  begin file setup2.f
c
c  This file contains routine which initialize the models, fluid-
c  specific parameters and coefficients, etc.  The subroutine SETUP (in
c  file setup.f) must be called before any of the other property routines
c  are called.  Call(s) to SETMOD and SETREF are optional and may be used
c  to specify non-standard models and reference states.
c
c  If the transport models, dielectric models, and surface tension eqs.
c  (vis, tcx, st, de) are not required, and only the default values in the
c  fluid files are used, the following files do not have to be compiled and
c  linked with the other source code:  CORE_DE, CORE_STN,
c  SETUP2, TRNS_ECS, TRNS_TCX, TRNS_VIS, TRNSP.  In addition,
c  the line in SETUP.FOR calling STFLD2 must be commented out.
c  The file FLASH2.FOR is not required but contains additional property
c  calculation routines.
c
c  contained here are:
c     subroutine STFLD2 (nread,i,hcasn,hcite,heqn,hetemp,hfile,hflag,
c    &                    href,hstar,httemp,htype,leta,ltcx,ierr,herr)
c     subroutine GETMOD (icomp,htype,hcode,hcite)
c     subroutine SETKTV (icomp,jcomp,hmodij,fij,hfmix,ierr,herr)
c     subroutine GETKTV (icomp,jcomp,hmodij,fij,hfmix,hfij,hbinp,hmxrul)
c     subroutine GETFIJ (hmodij,fij,hfij,hmxrul)
c
c
c ======================================================================
c
      subroutine STFLD2 (nread,i,hcasn,hcite,heqn,hetemp,hfile,hflag,
     &                    hreff,hstar,httemp,htype,leta,ltcx,ierr,herr)
c
c  define transport, dielectric, and surface tension models
c  see comments to SETFLD for additional information
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  11-19-94  MM, original version (SETFLD)
c  09-01-00 EWL, remove pieces from SETFLD and create new SETFLD2 subroutine
c  11-16-01 MLH, activated TK6 model for ecs crit enh; added common block CREMOD2
c  01-23-02 EWL, split common block CITE into 2 pieces for some compilers
c  08-27-04 EWL, rename SETFLD2 to STFLD2 to conform with standard f77 code
c  07-22-05 MLH, allow multiple transport reference fluids
c  11-08-06 EWL, load TCX and ETA coefficients into negative arrays
c  12-26-06 MLH, add VS4 model
c  11-02-07 MLH, add Chung model
c
      include 'commons.for'
      include 'comtrn.for'
      character*1 hstar
      character*3 heqn
      character*3 hetemp,httemp
      character*3 hmaux
      character*3 hvs,htc
      character*3 hflag,htype
      character*12 hcasn(n0:nx)
      character*255 hfile(n0:nx),hreff
      character*255 herr,herr1
      character*251 hcite
      logical leta,ltcx
c
      ierrtc=0
      ierrvs=0
      ierrst=0
      hreff=' '
      ier=0
      ierr=0
      ierr1=0
      herr=' '
      herr1=' '
      if (hflag.eq.'AUX') then
c
c  set up auxiliary model(s)
c  (model(s) are specified in call to appropriate SETmod routine)
c
c       write (*,*) ' SETUP--aux model (',htype,') found in fld file '
c       write (*,*) ' SETUP--i,htype,hmodcp(i): ',i,htype,hmodcp(i)
        ierr1=0
        hmaux=hmodcp(i)    !possible use in error message
        if (i.ge.nrf0) then
c  allow calls to transport ref. fluids, i.lt.-ncmax
c  do not call collision integral model (or other auxiliary models
c  associated with the transport properties) if component number < 0
c  (there is only one reference fluid for transport props so arrays
c  are not dimensioned for the negative component numbers set up to
c  accommodate the multiple reference fluids in the ECS-thermo model)
          if (htype(1:2).eq.'CI') hmaux=htype       !possible use in
          if (htype(1:2).eq.'TK') hmaux=htcxcr(i)   !error message
          if (htype.eq.'CI1' .and. hmdeta(i).eq.htype) then
c  functions for collision integral (used in transport correlations)
c           write (*,*) ' SETUP--about to call SETCI1'
            call SETCI1 (nread,i,hcasn(i),ierr1,herr1)
          elseif (htype.eq.'CI1' .and. hmdtcx(i).eq.htype) then
c  functions for collision integral (used in transport correlations)
c           write (*,*) ' SETUP--about to call SETCI1'
            call SETCI1 (nread,i,hcasn(i),ierr1,herr1)
          else if (htype.eq.'CI2' .and. hmdeta(i).eq.htype) then
c           write (*,*) ' SETUP--about to call SETCI2'
            call SETCI2 (nread,i,hcasn(i),ierr1,herr1)
          else if (htype.eq.'CI2' .and. hmdtcx(i).eq.htype) then
c           write (*,*) ' SETUP--about to call SETCI2'
            call SETCI2 (nread,i,hcasn(i),ierr1,herr1)
c  thermal conductivity critical enhancement models
          else if (htype.eq.'TK1' .and. htcxcr(i).eq.htype) then
c           write (*,*) ' SETUP--about to call SETTK1'
            call SETTK1 (nread,i,hcasn(i),ierr1,herr1)
            hitcxc(i)=htype//hcite//hnull
c         else if (htype.eq.'TK2' .and. htcxcr(i).eq.htype) then
c  the TK2 model is linked with TC2 and is initialized directly by SETTC2
c           call SETTK2 (nread,i,hcasn(i),ierr1,herr1)
            hitcxc(i)=htype//hcite//hnull
          else if (htype.eq.'TK3' .and. htcxcr(i).eq.htype) then
c           write (*,*) ' SETUP--about to call SETTK3'
            call SETTK3 (nread,i,hcasn(i),ierr1,herr1)
            hitcxc(i)=htype//hcite//hnull
          else if (htype.eq.'TK4' .and. htcxcr(i).eq.htype) then
c           write (*,*) ' SETUP--about to call SETTK4'
            call SETTK4 (nread,i,hcasn(i),ierr1,herr1)
            hitcxc(i)=htype//hcite//hnull
          else if (htype.eq.'TK6' .and. htcxcrecs(i).eq.htype) then
c           write (*,*) ' SETUP--about to call SETTK6'
            call SETTK6 (nread,i,hcasn(i),ierr1,herr1)
            hitcxc(i)=htype//hcite//hnull
          end if
        end if
        if (ierr1.ne.0) then
          ierr=104
          write (herr,2104) hmaux,i,herr1(1:182),hnull
          call ERRMSG (ierr,herr)
        end if
      end if
c
c  transport property models
c
c  do not call transport models if component number is 0
      if (i.ge.nrf0) then
c
c  the 'TRN' models apply to both the viscosity and thermal conductivity
c
        if (hflag.eq.'TRN' .or. hflag.eq.'trn') then
          if (hstar.eq.'#') then
c  special case for NIST-recommended model
            if (nc.eq.1) then
              if (hetamx.eq.'NBS') hetamx=htype
              if (htcxmx.eq.'NBS') htcxmx=htype
            end if
            if (i.ge.1 .and. heta(i).eq.'NBS') then
              heta(i)=htype
            end if
            if (i.ge.1 .and. htcx(i).eq.'NBS') then
              htcx(i)=htype
            end if
c  in the case that the TRN model is the NIST-recommended one, the ECS
c  method is equivalent to a fluid-specific model for purposes of the
c  leta and ltcx flags
            leta=.true.
            ltcx=.true.
          else
c  load the 'TRN' model if a fluid-specific 'ETA' or 'TCX' has not yet
c  been loaded (e.g. if a fluid-specific correlation is not available);
c  this is indicated by the flags leta and ltcx; if a fluid-specific
c  correlation is encountered later in the file this will be overwritten;
c  this is necessary to cover the case of a fluid-specific (and NIST-rec)
c  model available for only one of the transport properties
            if (.not.leta) hetemp=htype
            if (.not.ltcx) httemp=htype
          end if
c
          if (htype.eq.'ECS') then
c  extended corresponding states model found in file
c  always load the ECS model (if any transport props are asked for)
c  for possible use if a pure fluid correlation is out of range
            if (hetamx.ne.'NUL' .or. htcxmx.ne.'NUL') then
c             write (*,*) ' SETUP--about to call SETTRN (ECS-transport)'
              if (hieta(i)(1:3).eq.'NUL') hieta(i)=htype//hcite//hnull
              if (hitcx(i)(1:3).eq.'NUL') hitcx(i)=htype//hcite//hnull
              call SETTRN(nread,i,hcasn(i),hreff,heqn,hvs,htc,ier,herr1)
              if (ier.ne.0) then
                ierr=104
                write (herr,2104) htype,i,herr1(1:182),hnull
                call ERRMSG (ierr,herr)
              end if
              if (i.ge.1) then
c  store info for ECS reference fluid
c  (but not if current fluid is the reference fluid)
c  add directory information to reference fluid file
                call RFFILE (hfile(i),hreff)
c               write (*,1160) heqn,hvs,htc
c1160           format ('  SETFLD--transport ref fluid mods:',3(1x,a3))
                hfile(izero0)=hreff         !.fld file for reference fluid
                hmxeos(izero0)=heqn        !model for ref fluid eqn of state
                heta(izero0)=hvs          !model for ref fluid viscosity
                htcx(izero0)=htc          !model for ref fluid conductivity
              end if
            end if
          else
c  unidentified model found in file
            ierr=-103
            write (herr,2103) htype,i,hnull
            call ERRMSG (ierr,herr)
          end if
        end if      !end of ECS-transport block
c
c  pure fluid viscosity models
c
        if (hflag.eq.'ETA' .or. hflag.eq.'eta' .or.
     &      hflag.eq.'VIS' .or. hflag.eq.'vis') then
          if (hstar.eq.'#') then
c  special case for NIST-recommended model
            if (nc.eq.1 .and. hetamx.eq.'NBS') then
              hetamx=htype
            end if
            if (i.ne.0 .and. heta(i).eq.'NBS') then
              heta(i)=htype
            end if
c           if (i.lt.-ncmax .and. heta(i).eq.'NBS') heta(i)=htype
          end if
c
c  viscosity model "0" found in .fld file
          if (htype.eq.'VS0') then
            if (heta(i).eq.'VS0') then
c             write (*,*) ' SETUP--about to call SETVS0'
              call SETVS0 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c  viscosity model "1" found in .fld file
          elseif (htype.eq.'VS1') then
            if (heta(i).eq.'VS1') then
c             write (*,*) ' SETUP--about to call SETVS1'
              call SETVS1 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c  viscosity model "2" found in .fld file
          else if (htype.eq.'VS2') then
            if (heta(i).eq.'VS2') then
c             write (*,*) ' SETUP--about to call SETVS2'
              call SETVS2 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c  viscosity model "3" found in .fld file
          else if (htype.eq.'VS3') then
            if (heta(i).eq.'VS3') then
c             write (*,*) ' SETUP--about to call SETVS3'
              call SETVS3 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c  viscosity model "4" found in .fld file
          else if (htype.eq.'VS4') then
            if (heta(i).eq.'VS4') then
c             write (*,*) ' SETUP--about to call SETVS4'
              call SETVS4 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c   viscosity model "5" found in .fld file
          else if (htype.eq.'VS5') then
            if (heta(i).eq.'VS5') then
c             write (*,*) ' SETUP--about to call SETVS5'
              call SETVS5 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c  viscosity model "6" found in .fld file
          else if (htype.eq.'VS6') then
            if (heta(i).eq.'VS6') then
c             write (*,*) ' SETUP--about to call SETVS6'
              call SETVS6 (nread,i,hcasn(i),ierrvs,herr1)
              leta=.true.   !flag that fluid-specific vis model loaded
              hieta(i)=htype//hcite//hnull
            end if
c  ignore the NUL model
          else if (htype.eq.'NUL') then
          else
c  unidentified model found in file
            ierr=-103
            write (herr,2103) htype,i,hnull
            call ERRMSG (ierr,herr)
          end if
        end if
        if (ierrvs.ne.0) then
          ierr=104
          write (herr,2104) heta(i),i,herr1(1:182),hnull
          call ERRMSG (ierr,herr)
        end if
c  end of viscosity block
c
c  pure fluid thermal conductivity models
c
        if (hflag.eq.'TCX' .or. hflag.eq.'tcx') then
          if (hstar.eq.'#') then
c  special case for NIST-recommended model
            if (nc.eq.1 .and. htcxmx.eq.'NBS') then
              htcxmx=htype
            end if
            if (i.ne.0 .and. htcx(i).eq.'NBS') then
              htcx(i)=htype
            end if
c           if (i.lt.-ncmax .and. htcx(i).eq.'NBS') htcx(i)=htype
          end if
c
c  thermal conductivity model "0" found in .fld file
          if (htype.eq.'TC0') then
            if (htcx(i).eq.'TC0') then
c             write (*,*) ' SETUP--about to call SETTC0'
              call SETTC0 (nread,i,hcasn(i),ierrtc,herr1)
              ltcx=.true.   !flag that fluid-specific t.c. model loaded
              hitcx(i)=htype//hcite//hnull
            end if
c  thermal conductivity model "1" found in .fld file
          elseif (htype.eq.'TC1') then
            if (htcx(i).eq.'TC1') then
c             write (*,*) ' SETUP--about to call SETTC1'
              call SETTC1 (nread,i,hcasn(i),ierrtc,herr1)
              ltcx=.true.   !flag that fluid-specific t.c. model loaded
              hitcx(i)=htype//hcite//hnull
            end if
c  thermal conductivity model "2" found in .fld file
          else if (htype.eq.'TC2') then
            if (htcx(i).eq.'TC2') then
c             write (*,*) ' SETUP--about to call SETTC2'
              call SETTC2 (nread,i,hcasn(i),ierrtc,herr1)
              ltcx=.true.   !flag that fluid-specific t.c. model loaded
              hitcx(i)=htype//hcite//hnull
            end if
c  thermal conductivity model "3" found in .fld file
          else if (htype.eq.'TC3') then
            if (htcx(i).eq.'TC3') then
c             write (*,*) ' SETUP--about to call SETTC3'
              call SETTC3 (nread,i,hcasn(i),ierr,herr1)
              ltcx=.true.   !flag that fluid-specific t.c. model loaded
              hitcx(i)=htype//hcite//hnull
            end if
c  thermal conductivity model "5" found in .fld file
          else if (htype.eq.'TC5') then
            if (htcx(i).eq.'TC5') then
c             write (*,*) ' SETUP--about to call SETTC5'
              call SETTC5 (nread,i,hcasn(i),ierr,herr1)
              ltcx=.true.   !flag that fluid-specific t.c. model loaded
              hitcx(i)=htype//hcite//hnull
            end if
c  thermal conductivity model "6" found in .fld file
          else if (htype.eq.'TC6') then
            if (htcx(i).eq.'TC6') then
c             write (*,*) ' SETUP--about to call SETTC6'
              call SETTC6 (nread,i,hcasn(i),ierrtc,herr1)
              ltcx=.true.   !flag that fluid-specific t.c. model loaded
              hitcx(i)=htype//hcite//hnull
            end if
c  ignore the NUL model
          else if (htype.eq.'NUL') then
          else
c  unidentified model found in file
            ierr=-103
            write (herr,2103) htype,i,hnull
            call ERRMSG (ierr,herr)
          end if
        end if
        if (ierrtc.ne.0) then
          ierr=104
          write (herr,2104) htcx(i),i,herr1(1:182),hnull
          call ERRMSG (ierr,herr)
        end if
c  end of thermal conductivity block
      end if          !end of overall transport properties block
c
c  pure fluid surface tension models
c
c  do not call surface tension models if component number less than 0
      if (i.ge.nrf0) then
        if (hflag.eq.'STN' .or. hflag.eq.'stn') then
          if (hstar.eq.'#') then
c  special case for NIST-recommended model
            if (nc.eq.1 .and. hsten.eq.'NBS') then
              hsten=htype
            end if
            if (i.ge.1 .and. hstenk(i).eq.'NBS') then
              hstenk(i)=htype
            end if
          end if
c
c  surface tension model "1" found in .fld file
          if (htype.eq.'ST1') then
            if (hstenk(i).eq.'ST1') then
c             write (*,*) ' SETUP--about to call SETST1'
              call SETST1 (nread,i,hcasn(i),ierrst,herr1)
              histn(i)=htype//hcite//hnull
            end if
c  surface tension model "2" found in .fld file
c         else if (htype.eq.'ST2') then
c           if (hstenk(i).eq.'ST2') then
c             write (*,*) ' SETUP--about to call SETST2'
c             call SETST2 (nread,i,hcasn(i),ierrst,herr1)
c             histn(i)=htype//hcite//hnull
c           end if
          else
c  unidentified model found in file
            ierr=-103
            write (herr,2103) htype,i,hnull
            call ERRMSG (ierr,herr)
          end if
        end if
        if (ierrst.ne.0) then
          ierr=104
          write (herr,2104) hstenk(i),i,herr1(1:182),hnull
          call ERRMSG (ierr,herr)
        end if
      end if          !end of surface tension block
c
c  pure fluid dielectric constant models
c
c  do not call dielectric constant models if component number less than 0
      if (i.ge.nrf0) then
        if (hflag.eq.'DE ' .or. hflag.eq.'de ') then
          if (hstar.eq.'#') then
c  special case for NIST-recommended model
            if (nc.eq.1 .and. hdiel.eq.'NBS') then
              hdiel=htype
            end if
            if (i.ge.1 .and. hdielk(i).eq.'NBS') then
              hdielk(i)=htype
            end if
          end if
c
          if (htype(1:2).eq.'DE') then
c  dielectric constant model found in .fld file
            if (hdielk(i)(1:2).eq.'DE') then
c             write (*,*) ' SETUP--about to call SETDE'
              call SETDE (nread,i,hcasn(i),ierrst,herr1)
              hidiel(i)=htype//hcite//hnull
            end if
          else
c  unidentified model found in file
            ierr=-103
            write (herr,2103) htype,i,hnull
            call ERRMSG (ierr,herr)
          end if
        end if
        if (ierrst.ne.0) then
          ierr=104
          write (herr,2104) hdielk(i),i,herr1(1:182),hnull
          call ERRMSG (ierr,herr)
        end if
      end if          !end of dielectric constant block
      RETURN
 2103 format ('[SETUP warning -103] unknown model (',a3,
     &        ') encountered in file for component #',i3,'.',a1)
 2104 format ('[SETUP error 104] error in setup of (',a3,
     &        ') model for component #',i3,':  ',a182,a1)
      end                                             !subroutine STFLD2
c
c ======================================================================
c
      subroutine GETMOD (icomp,htype,hcode,hcite)
c
c  retrieve citation information for the property models used
c
c  inputs:
c    icomp--pointer specifying component number
c           zero and negative values are used for ECS reference fluid(s)
c    htype--flag indicating which model is to be retrieved [character*3]
c           'EOS':  equation of state for thermodynamic properties
c           'CP0':  ideal part of EOS (e.g. ideal-gas heat capacity)
c           'ETA':  viscosity
c           'VSK':  viscosity critical enhancement
c           'TCX':  thermal conductivity
c           'TKK':  thermal conductivity critical enhancement
c           'STN':  surface tension
c           'DE ':  dielectric constant
c           'MLT':  melting line (freezing line, actually)
c           'SBL':  sublimation line
c           'PS ':  vapor pressure equation
c           'DL ':  saturated liquid density equation
c           'DV ':  saturated vapor density equation
c  outputs:
c    hcode--component model used for property specified in htype
c
c           some possibilities for thermodynamic properties:
c           'FEQ':  Helmholtz free energy model
c           'BWR':  pure fluid modified Benedict-Webb-Rubin (MBWR)
c           'ECS':  pure fluid thermo extended corresponding states
c
c           some possibilities for viscosity:
c           'ECS':  extended corresponding states (all fluids)
c           'VS1':  the 'composite' model for R134a, R152a, NH3, etc.
c           'VS2':  Younglove-Ely model for hydrocarbons
c           'VS4':  generalized friction theory of Quinones-Cisneros and Dieters
c           'VS5':  Chung et al model
c
c           some possibilities for thermal conductivity:
c           'ECS':  extended corresponding states (all fluids)
c           'TC1':  the 'composite' model for R134a, R152a, etc.
c           'TC2':  Younglove-Ely model for hydrocarbons
c           'TC5':  predictive model of Chung et al. (1988)
c
c           some possibilities for surface tension:
c           'ST1':  surface tension as f(tau); tau = 1 - T/Tc
c
c    hcite--component model used for property specified in htype;
c           the first 3 characters repeat the model designation of hcode
c           and the remaining are the citation for the source
c
c  written by M. McLinden, NIST Phys & Chem Properties Div, Boulder, CO
c  01-26-00  MM, original version
c  01-23-02 EWL, split common block CITE into 2 pieces for some compilers
c
cx DEC$ ATTRIBUTES DLLEXPORT :: GETMOD
c
      include 'commons.for'
      character*3 htype,hcode
      character*255 hcite
c
      if (htype.eq.'EOS' .or. htype.eq.'eos') then
c  equation of state specification
        hcite=hieos(icomp)
      else if (htype.eq.'CP0' .or. htype.eq.'cp0' .or.
     &         htype.eq.'Cp0' .or. htype.eq.'cpo') then
c  ideal-gas heat capacity
        hcite=hicp0(icomp)
      else if (htype.eq.'ETA' .or. htype.eq.'eta' .or.
     &         htype.eq.'VIS' .or. htype.eq.'vis') then
c  viscosity
        hcite=hieta(icomp)
      else if (htype.eq.'VSK' .or. htype.eq.'vsk') then
c  viscosity critical enhancement
c  note:   as of 01/2000, no models employ a viscosity critical enhancement
        hcite=hietac(icomp)
      else if (htype.eq.'TCX' .or. htype.eq.'tcx') then
c  thermal conductivity
        hcite=hitcx(icomp)
      else if (htype.eq.'TKK' .or. htype.eq.'tkk') then
c  thermal conductivity critical enhancement
        hcite=hitcxc(icomp)
      else if (htype.eq.'STN' .or. htype.eq.'stn') then
c  surface tension
        hcite=histn(icomp)
      else if (htype.eq.'DE ' .or. htype.eq.'de ') then
c  dielectric constant
        hcite=hidiel(icomp)
      else if (htype.eq.'MLT' .or. htype.eq.'mlt') then
c  melting line
        hcite=himelt(icomp)
      else if (htype.eq.'SBL' .or. htype.eq.'sbl') then
c  sublimation line
        hcite=hisubl(icomp)
      else if (htype.eq.'PS ' .or. htype.eq.'ps ') then
c  vapor pressure equation
        hcite=hips(icomp)
      else if (htype.eq.'DL ' .or. htype.eq.'dl ') then
c  saturated liquid density equation
        hcite=hidl(icomp)
      else if (htype.eq.'DV ' .or. htype.eq.'dV ') then
c  saturated vapor density equation
        hcite=hidv(icomp)
      end if
      hcode=hcite(1:3)
c
      RETURN
c
      end                                             !subroutine GETMOD
c
c ======================================================================
c
      subroutine SETKTV (icomp,jcomp,hmodij,fij,hfmix,ierr,herr)
c
c  set mixture model and/or parameters
c
c  This subroutine must be called after SETUP, but before any call to
c  SETREF; it need not be called at all if the default mixture
c  parameters (those read in by SETUP) are to be used.
c
c  inputs:
c    icomp--component i
c    jcomp--component j
c   hmodij--mixing rule for the binary pair i,j [character*3]
c           e.g. 'LJ1' (Lemmon-Jacobsen model)
c                'LM1' (modified Lemmon-Jacobsen model) or
c                'LIN' (linear mixing rules)
c           'RST' indicates reset all pairs to values from
c                 original call to SETUP (i.e. those read from file)
c                 [all other inputs are ignored]
c      fij--binary mixture parameters [array of dimension nmxpar;
c           currently nmxpar is set to 6]
c           the parameters will vary depending on hmodij;
c           for example, for the Lemmon-Jacobsen model (LJ1):
c             fij(1) = zeta
c             fij(2) = xi
c             fij(3) = Fpq
c             fij(4) = beta
c             fij(5) = gamma
c             fij(6) = 'not used'
c    hfmix--file name [character*255] containing generalized parameters
c           for the binary mixture model; this will usually be the same
c           as the corresponding input to SETUP (e.g.,':fluids:hmx.bnc')
c  outputs:
c     ierr--error flag:  0 = successful
c                        111 = error in opening mixture file
c                        112 = mixture file of wrong type
c                       -113 = illegal i,j specification
c                              (i = j or i > nc or j > nc)
c                       -114 = possibility of mismatch of interaction parameters
c     herr--error string (character*255 variable if ierr<>0)
c     [mixture parameters returned via various common blocks]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  05-07-96  MM, original version
c  05-08-96  MM, add /MXINFO/ and load corresponding information
c  11-04-96  MM, change nmxpar from 4 to 6
c  11-22-96  MM, 'RST' option now calls SETHMX, i.e. resets all pairs
c  11-25-96  MM, reorder such that 'RST' option is tested first
c  10-01-97  MM, add compiler switch to allow access by DLL
c  02-06-98  MM, initial use of icount uninitialized, use nbin instead
c  03-11-99 EWL, reset lsatt and lsatp to .false. when called
c  02-14-01 EWL, change dimension of amix from 3 to 4
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c  11-01-05 EWL, remove open statement, etc., to check for same model
c  10-23-06 EWL, read open statement
c  02-13-09 EWL, add check for fluid order in KW model (add fmix2)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: SETKTV
c
      include 'commons.for'
      character*3 hmodij,htype
      character*255 hfmix
      character*255 herr
      dimension fij(nmxpar)
      logical lij(ncmax,ncmax)
c
c  commons associated with the binary mixing rule(s)
c  the amix(i,j,k,1..3) are the coefs for the (general) mixing term
c  the fmix(i,j,1..nmxpar) are the parameters for the i-j binary
c  /MXINFO/ contains information on the mixing rules and parameters
c  hmfile specifies the files from which mixing rules originate
c  (stored in order 1,2  1,3  2,3; element zero is file called in SETUP)
c  hbin provides documentation for the current binary parameters
c  hrule contains descriptions of the currently available mixing rules
c  hbpar contains descriptions of the binary parameters (e.g. Kt, Kv)
c  associated with the currently available mixing rules
c  common to pass flags associated with reference state
c
      lsatt=.false.
      lsatp=.false.
      ierr=0
      herr=' '
c
c  set flag indicating that reference state needs to be recalculated
c  (for use with the ixflag = 2 option of SETREF)
      ixfsav=0
c  special case:  reset parameters to original values
      if (hmodij.eq.'RST' .or. hmodij.eq.'rst') then
c  retrieve file specified on original call to SETUP
        hfmix=hmfile(0)
        call SETHMX (hfmix,ierr,herr)
c       do i=1,icount
        do i=1,nbin
          hmfile(i)=hmfile(0)
        enddo
c       write (*,*) ' SETKTV--reset all parameters to original values'
c       write (*,*) '  i  j  mod     Kt        Kv        Fpq      alpha'
c       do i=i,nc
c         do j=1,nc
c           write (*,1010) i,j,hmodmx(i,j),(fmix(i,j,k),k=1,nmxpar)
c         enddo
c       enddo
c1010   format (1x,2i3,2x,a3,4f10.5)
        RETURN
      end if
c
c  check if icomp or jcomp > nc, also do not allow change of
c  pure fluid or i=j parameters
c
      if (icomp.gt.nc .or. jcomp.gt.nc) then
        ierr=-113
        herr='[SETKTV warning -113] i > nc and/or j > nc'//hnull
        call ERRMSG (ierr,herr)
c       write (*,*) herr
        RETURN
      else if (nc.le.1 .or. icomp.eq.jcomp) then
        ierr=-113
        herr='[SETKTV warning -113] binary parameters for i = j '//
     &       'cannot be changed.'//hnull
        call ERRMSG (ierr,herr)
c       write (*,*) herr
        RETURN
      end if
c
c  match input icomp,jcomp with binary pair number
      icount=0
      ibin=1      !initialize only
      do i=1,nc-1
        do j=i+1,nc
          icount=icount+1
          if ((i.eq.icomp .and. j.eq.jcomp) .or.
     &      (i.eq.jcomp .and. j.eq.icomp)) then
            ibin=icount
          end if
        enddo
      enddo
c
      i=0
      if (hmodij(1:2).eq.'KW') then
        i=1
        if  (abs(fmix (icomp,jcomp,1)-1.d0).gt.1.d-20
     &  .or. abs(fmix (icomp,jcomp,3)-1.d0).gt.1.d-20) i=2
        if  (abs(fmix2(icomp,jcomp,1)-1.d0).gt.1.d-20
     &  .or. abs(fmix2(icomp,jcomp,3)-1.d0).gt.1.d-20) i=3
      end if
      do k=1,nmxpar
        fmix(icomp,jcomp,k)=fij(k)
        fmix(jcomp,icomp,k)=fij(k)     !pair (j,i) is same as (i,j)
      enddo
      if (i.eq.1) then
        if (ierr.eq.0) then
          ierr=-114
          herr='[SETKTV warning -114] there is a possibility that '//
     &         'the mixture parameters are set incorrectly if the '//
     &         'fluid order is different from that in the '//
     &         'HMX.BNC file.'//hnull
          call ERRMSG (ierr,herr)
        endif
      elseif (i.eq.3) then
        fmix2(jcomp,icomp,1)=fmix(jcomp,icomp,1)   !Input pair is reverse of that in the hmx.bnc file
        fmix2(jcomp,icomp,3)=fmix(jcomp,icomp,3)
        fmix2(icomp,jcomp,1)=fmix(icomp,jcomp,1)
        fmix2(icomp,jcomp,3)=fmix(icomp,jcomp,3)
        fmix(jcomp,icomp,1)=1.d0
        fmix(jcomp,icomp,3)=1.d0
        fmix(icomp,jcomp,1)=1.d0
        fmix(icomp,jcomp,3)=1.d0
      endif
      hbin(ibin)='The binary parameter(s) for ('//
     &            hname(icomp)//'+'//hname(jcomp)//
     &           ') have been modified from their original values.'
c
c  check if the mixture model has changed for the binary pair, if so,
c  must read coefficients for that model
c
      if (hmodij.ne.hmodmx(icomp,jcomp)) then
        hmodmx(icomp,jcomp)=hmodij
        hmodmx(jcomp,icomp)=hmodij   !pair (j,i) is same as (i,j)
c
        nread=12             !logical unit for file reads
        call OPENFL (nread,hfmix,1,ierr,herr)
        if (ierr.ne.0) goto 999
        rewind (nread)
        read (nread,2003) htype
        read (nread,'(i2)') ivrsnm
c
c       write (*,1105) hfmix
c1105   format (/1x,' SETKTV--mixture parameters from file: (',a80,')')
c  store file containing mixture parameters
        hmfile(ibin)=hfmix
c       write (*,*) ' SETKTV--new file for ibin =',ibin,' ',hmfile(ibin)
        call RDMIX (nread,icomp,jcomp,hmodij,lij,ierr,herr)
c  rewind and close the file
        rewind (nread)
        close (nread)
      end if
c
c     write (*,*) ' SETKTV:  return'
c     write (*,*) '  i  j  mod     Kt        Kv        Fpq      alpha'
c     i=icomp
c     j=jcomp
c     write (*,1280) i,j,hmodmx(i,j),(fmix(i,j,k),k=1,nmxpar)
c1280 format (1x,2i3,2x,a3,6f10.5)
      RETURN
c
 999  continue
      ierr=111
      write (herr,2111) hfmix(1:80),hnull
 2111 format ('[SETKTV error 111] error in opening mixture file',
     &        '; filename = (',a80,').',a1)
      call ERRMSG (ierr,herr)
      RETURN
c
 2003 format (a3)
c
      end                                             !subroutine SETKTV
c
c ======================================================================
c
      subroutine GETKTV (icomp,jcomp,hmodij,fij,hfmix,hfij,hbinp,hmxrul)
c
c  retrieve mixture model and parameter info for a specified binary
c
c  This subroutine should not be called until after a call to SETUP.
c
c  inputs:
c    icomp--component i
c    jcomp--component j
c  outputs:
c   hmodij--mixing rule for the binary pair i,j (e.g. LJ1 or LIN)
c           [character*3]
c      fij--binary mixture parameters [array of dimension nmxpar;
c           currently nmxpar is set to 6]; the parameters will vary
c           depending on hmodij;
c    hfmix--file name [character*255] containing parameters for the
c           binary mixture model
c     hfij--description of the binary mixture parameters [character*8
c           array of dimension nmxpar]
c           for example, for the Lemmon-Jacobsen model (LJ1):
c             fij(1) = zeta
c             fij(2) = xi
c             fij(3) = Fpq
c             fij(4) = beta
c             fij(5) = gamma
c             fij(6) = 'not used'
c    hbinp--documentation for the binary parameters [character*255]
c           terminated with ASCII null character
c   hmxrul--description of the mixing rule [character*255]
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  05-08-96  MM, original version
c  08-01-96  MM, bug fix: ibin not defined for icomp=jcomp
c  11-04-96  MM, change nmxpar from 4 to 6
c  10-01-97  MM, add compiler switch to allow access by DLL
c  02-14-01 EWL, change dimension of amix from 3 to 4
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c  02-13-09 EWL, add check for fluid order in KW model (add fmix2)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: GETKTV
c
      include 'commons.for'
      character*3 hmodij
      character*8 hfij(nmxpar)
      character*255 hfmix,hmxrul
      character*255 hbinp
      character*255 herr
      dimension fij(nmxpar)
c
c  commons associated with the binary mixing rule(s)
c  the amix(i,j,k,1..3) are the coefs for the (general) mixing term
c  the fmix(i,j,1..nmxpar) are the parameters for the i-j binary
c  /MXINFO/ contains information on the mixing rules and parameters
c  hmfile specifies the files from which mixing rules originate
c  (stored in order 1,2  1,3  2,3; element zero is file called in SETUP)
c  hbin provides documentation for the current binary parameters
c  hrule contains descriptions of the currently available mixing rules
c  hbpar contains descriptions of the binary parameters (e.g. Kt, Kv)
c  associated with the currently available mixing rules
c
      hmodij=hmodmx(icomp,jcomp)
      do k=1,nrule
        if (hmodij.eq.hrule(k)(1:3)) then
          irule=k
          goto 100
        end if
      enddo
c     write (*,*) ' GETKTV ERROR--mixing rule not found: ',hmodij
      ierr=-117
      write (herr,2117) icomp,jcomp,hnull
 2117 format ('[GETKTV error 117] mixing rule not found for components',
     &        i3,' +',i3,'.',a1)
      call ERRMSG (ierr,herr)
      RETURN
 100  continue
      ibin=0
      if (icomp.eq.jcomp .or. nc.le.1) then
        hbinp='trivial case--identical components'//hnull
      else
c  match input icomp,jcomp with binary pair number
        icount=0
        do i=1,nc-1
          do j=i+1,nc
            icount=icount+1
            if ((i.eq.icomp .and. j.eq.jcomp) .or.
     &          (i.eq.jcomp .and. j.eq.icomp)) then
              ibin=icount
c  old GUI combines hbinp with 3-letter code, so can use only 251 char
              hbinp=hbin(ibin)(1:251)//hnull
            end if
          enddo
        enddo
      end if
c  file containing mixture parameters
c     write (*,*) ' GETKTV--file for ibin =',ibin,' ',hmfile(ibin)
      hfmix=hmfile(ibin)
      hmxrul=hrule(irule)(1:254)//hnull
      do k=1,nmxpar
        hfij(k)=hbpar(irule,k)
        fij(k)=fmix(icomp,jcomp,k)
        if (hmodij(1:2).eq.'KW') then
          if  (abs(fmix (icomp,jcomp,k)-1.d0).lt.1.d-20
     &   .and. abs(fmix2(icomp,jcomp,k)-1.d0).gt.1.d-20)
     &         fij(k)=fmix2(icomp,jcomp,k)
        end if
      enddo
c
c     write (*,1278) hfmix,hbinp,hmxrul
c1278 format (/1x,' GETKTV  hfile:  ',a80/
c    &         1x,'         hbinp:  ',a255/
c    &         1x,'        hmxrul:  ',a255)
c     write (*,1280) (hfij(k),k=1,nmxpar),
c    &  icomp,jcomp,hmodmx(icomp,jcomp),(fmix(icomp,jcomp,k),k=1,nmxpar)
c1280 format (1x,'  i  j  mod  ',6(a8,2x)/
c    &        1x,2i3,2x,a3,6f10.5)
      RETURN
c
      end                                             !subroutine GETKTV
c
c ======================================================================
c
      subroutine GETFIJ (hmodij,fij,hfij,hmxrul)
c
c  retrieve parameter info for a specified mixing rule
c
c  This subroutine should not be called until after a call to SETUP.
c
c  inputs:
c   hmodij--mixing rule for the binary pair i,j (e.g. LJ1 or LIN)
c           [character*3]
c  outputs:
c      fij--binary mixture parameters [array of dimension nmxpar;
c           currently nmxpar is set to 6]; the parameters will vary
c           depending on hmodij;
c     hfij--description of the binary mixture parameters [character*8
c           array of dimension nmxpar]
c   hmxrul--description of the mixing rule [character*255]
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-10-01  MM, original version
c  01-23-02 EWL, split common block MXINFO into 2 pieces for some compilers
c
cx DEC$ ATTRIBUTES DLLEXPORT :: GETFIJ
c
      include 'commons.for'
      character*3 hmodij
      character*8 hfij(nmxpar)
      character*255 hmxrul,herr
      dimension fij(nmxpar)
c
      do k=1,nrule
        if (hmodij.eq.hrule(k)(1:3)) then
          irule=k
          goto 100
        end if
      enddo
c     write (*,*) ' GETKTV ERROR--mixing rule not found: ',hmodij
      ierr=-117
      write (herr,2117) hnull
 2117 format ('[GETKTV error 117] mixing rule not found',a1)
      call ERRMSG (ierr,herr)
      RETURN
 100  continue
      hmxrul=hrule(irule)(1:254)//hnull
      do k=1,nmxpar
        hfij(k)=hbpar(irule,k)
        fij(k)=bideal(irule,k)
      enddo
      RETURN
c
      end                                             !subroutine GETFIJ
c
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                      end file setup2.f
c ======================================================================
