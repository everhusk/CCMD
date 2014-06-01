c  begin file flsh_sub.f
c
c  This file contains iterative flash routines which call the
c  intermediate-level routines
c
c  contained here are:
c     subroutine TPRHO (t,p,x,kph,kguess,rho,ierr,herr)
c     subroutine TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
c     subroutine TPFL2 (t,p,z,Dl,Dv,x,y,q,ierr,herr)
c     subroutine TDFLSH (t,D,z,p,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
c     subroutine TDFL2 (t,D,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
c    &                  p,Dl,Dv,x,y,q,ierr,herr)
c     subroutine PDFLSH (p,D,z,t,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
c     subroutine PDFL1 (p,rho,x,t,ierr,herr)
c     subroutine PDFL2 (p,d,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
c    &                  t,Dl,Dv,x,y,q,ierr,herr)
c     subroutine PHFLSH (p,h,z,t,D,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
c     subroutine PHFL1 (p,h,x,kph,t,D,ierr,herr)
c     subroutine PHFL2 (p,h,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
c    &                  t,Dl,Dv,x,y,q,ierr,herr)
c     subroutine PSFLSH (p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
c     subroutine PSFL1 (p,s,x,kph,t,D,ierr,herr)
c     subroutine PSFL2 (p,s,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
c    &                  t,Dl,Dv,x,y,q,ierr,herr)
c     subroutine PEFLSH (p,e,z,t,D,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
c     subroutine PEFL1 (p,e,x,kph,t,D,ierr,herr)
c     subroutine PEFL2 (p,e,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
c    &                  t,Dl,Dv,x,y,q,ierr,herr)
c     subroutine PBFLSH (p,b,z,ab,t,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
c    &                   ierr,herr)
c
c  these routines use the following common blocks from other files
c     common /Gcnst/ R,tz(n0:nx),rhoz(n0:nx)
c     common /HCHAR/ htab,hnull
c
c  various arrays are dimensioned with parameter statements
c
c ======================================================================
c ======================================================================
c
      subroutine TPRHO (t,p,x,kph,kguess,rho,ierr,herr)
c
c  iterate for density as a function of temperature, pressure, and
c  composition for a specified phase
c
c***********************************************************************
c  WARNING:
c  Invalid densities will be returned for T & P outside range of validity,
c  i.e., pressure > melting pressure, pressure less than saturation
c  pressure for kph=1, etc.
c
c***********************************************************************
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        x--composition [array of mol frac]
c      kph--phase flag:  1 = liquid
c                        2 = vapor
c                 N.B.:  0 = stable phase--NOT ALLOWED (use TPFLSH)
c                            (unless an initial guess is supplied for rho)
c                       -1 = force the search in the liquid phase (for metastable points)
c                       -2 = force the search in the vapor phase (for metastable points)
c   kguess--input flag:  1 = first guess for rho provided
c                        0 = no first guess provided
c      rho--first guess for molar density [mol/L], only if kguess = 1
c
c  outputs:
c      rho--molar density [mol/L]
c     ierr--error flag:  0 = successful
c                      200 = CRITP did not converge
c                      201 = illegal input (kph <= 0)
c                      202 = liquid-phase iteration did not converge
c                      203 = vapor-phase iteration did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  04-03-95  MM, original version
c  09-11-95  MM, add error string to argument list
c  10-11-95  MM, RETURN if any error detected
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  01-22-95  MM, check that input rho > 0 before using as initial guess
c  01-23-95  MM, use Rackett technique for liquid density initial guess
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  03-07-96  MM, change tolr from 1.0d-6 to 1.0d-8
c  10-16-96  MM, change call from DPRHO to DPDD
c  12-02-96  MM, add check that vapor density guess does not give p < 0
c  01-07-97  MM, error message bug (do not concatenate herr with itself)
c  02-11-97 EWL, reduced tolerance after 10 and after 15 iterations
c  04-22-97  MM, add additional iteration for low-p (allow p = 0)
c  05-16-97  MM, do not apply step limitation for t > 1.5*tc
c  06-03-97 EWL, add second order Newton's solution to increase convergence speed
c  06-05-97 EWL, remove checks in liquid solution for large jumps in density
c                change initial guess in liquid phase to 2*Dc
c  07-15-97  MM, renumber and add detail to error messages
c  09-22-97 EWL, modify test to select liquid or vapor iteration
c  09-23-97  MM, increase itmax from 20 to 30 to aid in near-critical convergence
c  10-01-97  MM, add compiler switch to allow access by DLL
c  11-13-97 EWL, revert to 1st order Newton's method if 2nd order gives bad result
c  11-18-97 EWL, ditto for liquid-phase iteration
c  12-31-97  MM, check derivative dP/dD for liquid-phase initial guess
c  02-11-98  MM, check for t > Tc before switching liq to vapor iteration
c  03-31-98 EWL, check for Dguess>Dmax in liquid iteration
c  08-14-98  MM, do not check input rho<0 unless kguess=1 (@stmt 10)
c  09-30-98 EWL, if liquid iteration fails and t>tc, call vapor iteration
c  12-18-98 EWL, increase max value of fvdpl to 2 on check of 1st order Newton's
c  12-22-98 EWL, recalculate R for mixtures based on values for pure fluids
c  02-02-99 EWL, increase max value of fvdpl to 25 on check of 1st order Newton's
c  11-09-00 EWL, allow kph=0 if an initial guess is supplied for rho
c  01-17-02 EWL, allow kph=-1 and -2 to force searches in liquid or vapor
c  02-13-02 EWL, change pressure bound where the ideal gas law kicks in
c  11-02-04 EWL, check for p=0 and kph=1
c  07-20-05 EWL, check for very large rho
c  10-03-05 EWL, change itmax from 30 to 40
c  06-07-06 EWL, slight changes to check for T=Tc or P=Pc
c  10-01-09 EWL, change 0.1d0 to 0.11d0 to avoid rho=rho2, resulting in unreported errors
c  04-12-10 EWL, add check in liquid phase iteration for p app. equal to p2
c  04-27-10 EWL, add check for d(p)/d(rho)/d(T)<0
c  05-11-10 EWL, at it=12 or 18, add 30% to liquid rho in case iteration is stuck in 2-phase
c  09-24-12 EWL, add checks on initial density for d2PdD2 and d2PdTD for liquid root solver
c  10-09-12 EWL, remove check on rho<1 after call to PR, and very small rho for kph=1
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TPRHO
c
      include 'commons.for'
      character*255 herr,herr1
      dimension x(ncmax)
c
      itmax=40
      tolr=1.0d-8
      ierr=0
      herr=' '
c     write (*,1001) t,p,(x(i),i=1,5)
c1001 format (1x,' TPRHO--input t,p,x:           ',f10.5,f14.6,5f10.6)
c
c  determine which iteration to use:
c  For liquids (or fluids above the critical pressure) the iteration is
c  carried out in transformed coordinates of log(V).  For vapor (or
c  fluids at supercritical temperatures but pressures below the critical
c  value) the iteration is in terms of log(V) and log(p).  For very low
c  pressure vapors a simple iteration in rho and p is used.  The iteration
c  has converged when the pressure calculated from the equation of state
c  agrees with the input pressure within a convergence tolerance.
c
      call Rmix (x)
      if (p.lt.1.0d-8) then
        if (kph.ne.1) then
c  ideal-gas
          if (t.gt.0.d0) rho=p/(R*t)
          RETURN
        elseif (ABS(p).lt.1.d-60) then   !EWL changed from 1.d-16 to 1.d-60, 6/18/07
          rho=0.d0
          RETURN
        endif
      end if
c
c  call Peng-Robinson routines if in use
c
      i=-1
      call PREOS(i)
      if (i.eq.1) then
        call TPRHOPR (t,p,x,rho1,rho2)
        if (rho1.gt.1.d8) goto 100
        if (rho2.lt.1.d-12) then
          rho=rho1
        else
          if (abs(kph).eq.1) then
            rho=rho1
          elseif (abs(kph).eq.2) then
            rho=rho2
          else
            if (ABS(rho-rho1).lt.ABS(rho-rho2)) then
              rho=rho1
            else
              rho=rho2
            endif
          endif
        endif
c  if the pressure is less than 1 kPa and the density is in the liquid phase,
c  continue on with iterative routine to make sure that the cubic solver got
c  the right answer
c       if (p.gt.1.d0 .or. rho.lt.1.d0 .or. kph.eq.2) RETURN
        if (p.gt.1.d0 .or. kph.eq.2) RETURN
        if (kph.eq.1 .and. rho.lt.1.d-6) rho=0.d0
      endif
c
      call CRITP (x,tc,pc,rhoc,ierr,herr1)
      rhom2=rhoc
      rhom1=0.d0
      if (ierr.gt.0) then
        ierr=200
        write (herr,1200) herr1(1:236),hnull
 1200   format ('[TPRHO error 200] ',a236,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
      ierr=0
      vc=1.0d0/rhoc
      if (vc.le.0.d0) vc=1.d0
      vclog=LOG(vc)
      if (kph.eq.-1) then
        lliq=.true.
      elseif (kph.eq.-2) then
        lliq=.false.
c  EWL modification to aid near-critical convergence;
c  following approximates the pressure along the critical isochore
      elseif (p.lt..99*pc*(1.0d0+7.0d0*(t/tc-1.0d0)) .and. t.gt.tc) then
c  use 'vapor phase' iteration
        lliq=.false.
      else if (p.ge.pc .or. t.ge.tc) then
c  use 'liquid phase' iteration
        lliq=.true.
c  possible new code for very close to critical but with rho<rhoc
c       call PRESS(t,rhoc,x,ptest)
c       if (p.lt.ptest) lliq=.false.
      else if (kph.le.0) then
        if (kguess.ne.0) then
          lliq=.true.
          if (rho.lt.rhoc) lliq=.false.
        else
c  illegal input to TPRHO (two-phase state)
          ierr=201
          herr='[TPRHO error 201] illegal input to TPRHO (kph <= 0); '
     &           //'use TPFLSH instead'//hnull
          call ERRMSG (ierr,herr)
          RETURN
        endif
      else
c  use input phase specification to find solution
        if (kph.eq.1) then
          lliq=.true.
        else
          lliq=.false.
        end if
      end if
c
c  set initial guess for density
      kgrho=1      !flag is set to 0 if input rho<0
      vlog=0.d0
 10   continue
      if (kguess.eq.1) then
        if (rho.gt.0.0d0) then
c  use input density as initial guess
          vlog=LOG(1.0d0/rho)
        else
c  set flag to trigger initial guess algorithm below
          kgrho=0
        end if
      end if
      if (kguess.eq.0 .or. kgrho.eq.0) then
c  initial guess based on region
        if (p.gt.pc .and. t.gt.tc) then
          vlog=LOG(0.5d0*vc)
        else if (lliq) then
c         vlog=LOG(0.5d0*vc)
c  initial guess for sub-critical liquid density based on modified
c  Rackett technique (Reid, Prausnitz, and Poling (1987), Properties
c  of Gases and Liquids, 4th edition) except that Zra = Zc
          Rtp=R*tc/pc
          Zc=vc/Rtp
c         vlog=LOG(Rtp*Zc**(1.0d0+(1.0d0-t/tc)**(2.0d0/7.0d0)))
          Dguess=1.0d0/(Rtp*Zc**(1.0d0+ABS(1.0d0-t/tc)**(2.0d0/7.0d0)))
          call LIMITS ('EOS',x,tmin,tmax,Dmax,pmax)
          if (Dguess.gt.Dmax) Dguess=Dmax
          iguess=1
 20       continue
          call THERM2 (t,Dguess,x,pp,e,h,s,cv,cp,w,Z,hjt,A,G,
     &                 xkappa,beta,dPdrho,d2PdD2,dPT,drhodT,drhodP,
     &                 d2PT2,d2PdTD,spare3,spare4)
          if (dpdrho.lt.0.d0 .or.
     &       (d2PdD2.lt.0.d0 .and. t.lt.tc) .or.
     &       (d2PdTD.lt.0.d0 .and. abs(d2PdTD+9999990.d0).gt..1)) then
c  initial guess is in two-phase region, set to higher density
            if (iguess.le.8) then
              Dguess=1.1d0*Dguess
              iguess=iguess+1
              goto 20
            endif
          end if
          vlog=LOG(1.0d0/Dguess)
          if (vlog.gt.LOG(0.5d0*vc)) vlog=LOG(0.5d0*vc)
        else
          vlog=LOG(R*t/p)
        end if
        if (.not.lliq) vlog=LOG(R*t/p)
      end if
c     write (*,1082) kph,kguess,t,p,1.0/EXP(vlog),(x(i),i=1,nc)
c1082 format (1x,' TPRHO--kph,kguess,t,p,rho,x: ',2i4,3e14.6,5f10.5)
c
c  enter Newton's method iteration, separate loops for liquid and vapor
c
      rho2=0.0d0
      if (lliq) then
c  liquid phase iteration
        rhom1=100.d0
        do it=1,itmax
          if (it.eq.12 .or. it.eq.18) then
            rho=rho*1.3d0
            vlog=LOG(1.d0/rho)
          endif
          vlog0=vlog
c         write (*,*) ' TPRHO (liquid) vlog: ',vlog
          rho=1.0d0/exp(vlog)
          if (rho.gt.1.d8) goto 100
          call PRESS (t,rho,x,p2)
          call DPDD (t,rho,x,dpd)
          call DPDD2 (t,rho,x,dpd2)
c  keep track of the last best guess (rhom1) for density for cases where no root is
c  available.  This point will be near the spinodal in the valid metastable region.
          if(dpd.le.0.d0 .and. rho.gt.rhom2) rhom2=rho
          if(dpd.gt.0.d0 .and. rho.lt.rhom1 .and. rho.gt.rhom2)rhom1=rho
c         write (*,1010) it,t,x(1),p,p2,dpd,rho
c1010     format (1x,'it,t,x(1),p,p2,dpd,rho (liquid): ',i3,2f8.3,4e16.8)
          if (dpd.le.0.0d0) then
c  unstable portion of two-phase region, make another guess
c           write (*,1012) it,1.0/exp(vlog)
c1012       format (1x,'unstable 2-phase in liquid, it, rho = ',i4,d16.8)
            vlog=vlog-0.10d0
          else
            dpd2=dpd2+dpd/rho              !2nd order Newton's method
            fvdp=(p-p2)/(dpd2*(p-p2)/2.0d0/dpd+dpd)/rho
            if (ABS(fvdp).gt.100.0d0) then
c  revert to 1st order Newton's method if too large step from 2nd order
              dpdlv=-rho*dpd
              fvdp=(p2-p)/dpdlv            !1st order Newton's method
            endif
c           write (*,1014) dpdlv,dpd,fvdp
c1014       format (1x,' TPRHO (liquid) dpdlv,dpd,fvdp: ',3d16.8)
c  if calculation has not converged after 10 or 15 iterations, loosen
c  tolerance (original tolr was too tight near critical)
            if (it.eq.10) tolr=tolr*10.d0
            if (it.eq.15) tolr=tolr*10.d0
c  The rho-rho2 check is only important for very low pressures on the
c  liquid surface (propane or R124).  In this case, the change in density
c  required to get the correct pressure is less than machine precision.
            if ((ABS(fvdp/p).lt.0.0001d0*tolr .or.
     &          ABS(rho-rho2).lt.1.0d-11) .and.
     &         abs(p-p2).lt.0.001d0) then
c  iteration has converged
              rho=1.0d0/exp(vlog-fvdp)
c             call PRESS (t,rho,x,p2)        !convergence testing only
c             write (*,1016) it,t,p,p2,rho
c1016         format (1x,8x,'% TPRHO converged (liq); it,t,p,p2,rho  ',
c    &              i3,f8.3,2e18.10,e18.10)
c             write (*,*) ' TPRHO final rho-liq: ',rho
              goto 999
            end if
c  next guess
            vlog=vlog0-fvdp
c  do not allow too great of a change in density between iterations
c  for liquid states
            if (ABS(vlog-vlog0).gt.0.1d0 .and. t.lt.1.5d0*tc) then
              vlog=vlog0+SIGN(0.11d0,vlog-vlog0)  !Do NOT use 0.1d0
            end if
            if (vlog.gt.vclog .and. t.lt.tc) then
              vlog=0.5d0*(vlog0+vclog)
            end if
c  switch to the "vapor" iteration
            if (vlog.lt.-5.0d0 .and. t.ge.tc) then
c               write (*,*) ' TPRHO--switching to the vapor iteration'
              lliq=.false.
              goto 10
            end if
            rho2=rho
          end if
        enddo
c  iteration has not converged
        rho=1.0d0/exp(vlog)
        if (t.ge.tc) then
          lliq=.false.
          goto 10
        endif
        goto 100
c
c      else if (lowp) then
c   low-pressure vapor iteration
c   disabled--not required
c        do it=1,itmax
c          call PRESS (t,rho,x,p2)
c          call DPDD (t,rho,x,dpd)
c          delp=p2-p
c          if (ABS(delp).lt.0.0001*tolr) then
c   iteration has converged
c            write (*,1024) it,t,p,p2,delp,rho
c 1024       format (9x,'% TPRHO converged (low-p); it,t,p,p2,delp,rho  ',
c     &              i3,f8.3,3e18.10,e18.10)
c            RETURN
c          else
c   next guess
c            rho=rho-delp/dpd
c          end if
c        enddo
c   iteration has not converged
c        rho=p/(R*t)
c        ierr=1
c        write (herr,1002) t,p,rho,hnull
c        call ERRMSG (ierr,herr)
c 1002   format (' ERROR--TPRHO has not converged (low-p),  t,p,rho =',
c     &          f8.3,2e14.6,a1)
c        RETURN
c
      else
c  vapor phase iteration
        plog=LOG(p)
        do it=1,itmax
          vlog0=vlog
          rho=1.0d0/exp(vlog)
          call PRESS (t,rho,x,p2)
          call DPDD (t,rho,x,dpd)
          call DPDD2 (t,rho,x,dpd2)
c  keep track of the last best guess (rhom1) for density for cases where no root is
c  available.  This point will be near the spinodal in the valid metastable region.
          if(dpd.le.0.d0 .and. rho.lt.rhom2) rhom2=rho
          if(dpd.gt.0.d0 .and. rho.gt.rhom1 .and. rho.lt.rhom2)rhom1=rho
c         write (*,1030) it,t,x(1),p,p2,dpd,rho
c1030 format (1x,'it,t,x(1),p,p2,dpd,rho (vapor):  ',i3,2f8.3,4e18.10)
          if (dpd.lt.0.0d0 .or. p2.le.0.0d0) then
c  unstable portion of two-phase region or p2<0, make another guess
c         write (*,1032) it,1.0/exp(vlog)
c1032     format (1x,'unstable 2-phase in vapor or p2 < 0, it, rho = ',
c    &            i4,d16.8)
            vlog=vlog+0.10d0
          else

            dpd2=dpd2/p2-(dpd/p2)**2+dpd/rho/p2
            fvdpl=(plog-LOG(p2))/(p2*dpd2*(plog-LOG(p2))
     &            /2.0d0/dpd+dpd/p2)/rho      !2nd order Newton's method
c  if 2nd order Newton's method gives unreasonable result, revert to
c  first order method (large value of fvdpl corresponds to huge change
c  in next guess for volume)
            if (ABS(fvdpl).gt.1.0d0) then
              dpdlv=-rho*dpd
              fvdpl=(LOG(p2)-plog)*p2/dpdlv   !1st order Newton's method
            endif
c  the following used was original set at '.gt.2.d0', but was changed to
c  '.gt.25.d0'.  Vapor densities were failing to converge for a mixture of
c  47% methane, 16% ethane, 7% propane, and 30% i-butane.  This may now cause
c  other areas to fail.
            if (ABS(fvdpl).gt.25.d0) fvdpl=0.1d-5
c  if calculation has not converged after 10 or 15 iterations, loosen
c  tolerance (original tolr was too tight near critical)
            if (it.eq.10) tolr=tolr*10.d0
            if (it.eq.15) tolr=tolr*10.d0
            if (ABS(fvdpl).lt.0.0001d0*tolr) then
c  iteration has converged
              rho=1.0d0/exp(vlog)
              call PRESS (t,rho,x,p2)        !convergence testing only
c              write (*,1034) it,t,p,p2,rho
c1034       format (1x,8x,'% TPRHO converged (vap); it,t,p,p2,rho  ',
c    &              i3,f8.3,2e18.10,e18.10)
c              write (*,*) ' TPRHO final rho-vap: ',rho
              goto 999
            else
c  next guess
              vlog=vlog-fvdpl
c             if (ABS(vlog-vlog0).gt.0.5d0) then
c               vlog=vlog0+SIGN(0.5d0,vlog-vlog0)
c             end if
c             if (vlog.lt.vclog .and. t.lt.tc) then
c               vlog=0.5d0*(vlog0+vclog)
c             end if
            end if
          end if
        enddo
c  iteration has not converged
c       write (*,*) ' TPRHO final rho-vap (not converged): ',rho
      end if
 103  continue
      rho=1.0d0/exp(vlog)
      if (rhom1.gt.0.d0) rho=rhom1
      ierr=203
      write (herr1,1203) t,p/1000.d0,rho,(x(j),j=1,MIN(nc,5))
 1203 format ('[TPRHO error 203] vapor iteration has not ',
     &        'converged for T =',g12.5,' K, P =',g12.5,
     &        ' MPa, rho (last guess) = ',g12.5,
     &        ' mol/L, x (mol frac) =',5(0pf8.5))
      herr=herr1(1:254)//hnull
      call ERRMSG (ierr,herr)
      RETURN
c
 100  continue
      if (rhom1.gt.0.d0) rho=rhom1
      ierr=202
      write (herr1,1202) t,p/1000.d0,rho,(x(i),i=1,MIN(nc,5))
 1202 format ('[TPRHO error 202] liquid iteration has not ',
     &        'converged for T =',g12.5,' K, P =',g12.5,
     &        ' MPa, rho (last guess) = ',g12.5,' mol/L,',
     &        ' compositions = ',5(0pf9.5))
      herr=herr1(1:196)//hnull
      call ERRMSG (ierr,herr)
c     write (*,*) ' TPRHO final rho-liq (not converged): ',rho
      RETURN
 999  if (t.lt.tc*1.2d0 .and.
     &    rho.gt.rhoc*0.2d0 .and. rho.lt.rhoc*2.0d0) then
c  if solution is less than Tc and the densities are within a region
c  that is semi-critical, check d(p)/d(rho) and d^2(p)/[d(rho)*d(T)]
c  to make sure that root is valid
        call THERM2 (t,rho,x,pp,e,h,s,cv,cp,w,Z,hjt,A,G,
     &               xkappa,beta,dPdrho,d2PdD2,dPT,drhodT,drhodP,
     &               d2PT2,d2PdTD,spare3,spare4)
        if (dpdrho.lt.0.d0 .or.
     &     (d2PdTD.lt.0.d0 .and. abs(d2PdTD+9999990.d0).gt..1)) then
          if (nc.eq.1 .and. hcas(1).eq.'124-38-9' .and.
     &        d2PdTD.gt.-23.d0) then
c           If CO2, real state points can go down to -0.023 J/mol-K
          else
            if (kph.eq.1) goto 100
            goto 103
          endif
        endif
      endif
c
      end                                              !subroutine TPRHO
c
c ======================================================================
c
      subroutine TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given temperature, pressure, and bulk composition
c
c  This routine accepts both single-phase and two-phase states as the
c  input; if the phase is known, the subroutine TPRHO is faster.
c
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                        1 = Tin < Tmin
c                        4 = Pin < 0
c                        5 = T and P out of range
c                        8 = x out of range (component and/or sum < 0
c                            or > 1)
c                        9 = x and T out of range
c                       12 = x out of range and P < 0
c                       13 = x and T and P out of range
c                      210 = CRITP did not converge
c                      211 = SATT did not converge at bubble point
c                      212 = SATT did not converge at dew point
c                      213 = TPRHO did not converge for liquid state
c                      214 = TPRHO did not converge for vapor state
c                      215 = TPRHO did not convg for supercritical state
c                      216 = TPRHO did not convg for liq in 2-phase it
c                      217 = TPRHO did not convg for vap in 2-phase it
c                      218 = liquid frac for 2-phase it did not converge
c                      219 = composition for 2-phase it did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  09-11-95  MM, original version
c  09-12-95  MM, change argument list (add e, h, s, etc.)
c  09-20-95  MM, define output xl, xv for single-component super-critical
c  09-25-95  MM, rearrange argument list (outputs in order rho, x, q)
c  10-11-95  MM, RETURN if any error detected
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-12-95  MM, restructure logic to accommodate mixtures
c                first guess for 2-phase mix from ratio of dew & bubble
c  12-13-95  MM, set undefined output compositions to zero
c                initial rho guess for 1-phase vapor based on p/pdew
c  12-14-95  MM, bug on call to CRITP:  pass z (not x)
c  12-28-95  MM, add full 2-phase mixture iteration using fugacities
c  12-29-95  MM, move 2-phase iteration to separate routine TPFL2
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  05-31-96  MM, check input t,p,z against limits
c  06-03-96  MM, add 'EOS' to calling list for LIMITX
c  01-07-97  MM, error message bug (do not concatenate herr with itself)
c  04-09-97  MM, initialize x,y to z
c  04-22-97  MM, use h, rather than s, to compute q (h converges at p = 0)
c  05-15-97  MM, use V to compute q for comp. liq. (T,h can be double-valued)
c                add q = 998 case for t > Tc, but p < Pc
c  07-15-97  MM, renumber and add detail to error messages
c                get flags for 'not defined' from common /FLAGS/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  02-11-98  MM, check that computed density is within bounds
c  09-15-98 EWL, if T>Tc-5, use critical parameters if call to SATT fails.
c  12-28-98 EWL, if SATT fails (vapor), check for liquid state before failing
c  03-05-99 EWL, add check for region below Ttriple in the vapor phase
c  10-21-99 EWL, do not check satt if pressure is less than .8pc at tc,
c                .1pc at .85tc and the area in between (on a log plot).
c  01-11-00 EWL, remove ierr from line 1008 and several other places in flsh_sub.for
c  01-02-01 EWL, add check for d<dl and d>dv in single phase routine
c  05-21-01 EWL, call PSATT if an ancillary vapor pressure eq. exists (for nc=1)
c  09-11-02 EWL, drop psat by factor of 10 to check for two phase before doing 1 phase
c  12-16-02 EWL, add checks for pseudo-pure fluid
c  12-30-08 EWL, change x to z in call to PSATT
c  04-29-10 EWL, add tcex to check if T>Tc*Tcex for mixtures since Tc is not upper limit
c  05-06-10 EWL, add pcex to check if p>pc*pcex for mixtures since Pc is not upper limit
c  05-06-10 EWL, return last best metastable rho when no root for input phase exists
c  07-29-10 EWL, decrease psat calculation at low temperatures
c  10-10-10 EWL, drop psat by an additional factor of 100 to check for two phase before doing 1 phase, very important for water/air mixtures
c  02-23-11 EWL, add check for CO2 with d2P/d(rho)/dT < 0 near the critical point
c  04-27-11 EWL, change tc=tc*1.1d0 to t=tc*1.1d0
c  08-04-11 EWL, set dlbub, dldew, dvbub, and dvdew to rhoc if SATT fails
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TPFLSH
c
      include 'commons.for'
      character*255 herr,herr1,herr2,herrl
      dimension z(ncmax),x(ncmax),y(ncmax)
      dimension xdew(ncmax),ydew(ncmax),ybub(ncmax),xbub(ncmax)
c
      call ISPURE (z,icomp)
      ierr=0
      pbub=0
      pdew=0
      herr=' '
      q=999.d0     !quality undefined
c
c  initialize output liquid and vapor compositions to input values
c  zero output values for undefined components
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
      if (p.lt.1.0d-14) then
c  ideal-gas
        D=p/(R*t)
        Dl=D
        Dv=D
        call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
      endif
c
c  check that input conditions are within limits
c
      rhodum=0.0d0
      call LIMITX ('EOS',t,rhodum,p,z,tmin,tmax,rhomax,pmax,ierrl,herrl)
      if (ierrl.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ierrl,herrl(1:234),hnull
 1006   format ('[TPFLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierrl,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierrl.gt.0) then
        write (herr,1007) ierrl,herrl(1:236),hnull
 1007   format ('[TPFLSH error',i3,'] ',a236,a1)
        call ERRMSG (ierrl,herr)
        D=0.0d0
        Dl=0.0d0
        Dv=0.0d0
        if (t.gt.0) call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
        q=999.d0     !quality undefined
        ierr=ierrl
        RETURN
      end if
c
      call CRITP (z,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
        ierr=210
        write (herr,1008) herr2(1:235),hnull
 1008   format ('[TPFLSH error 210] ',a235,a1)
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
        RETURN
      end if
c
 150  continue
      psat=0.d0
      tcex=1.d0
      pcex=1.d0
      if (icomp.eq.0) then
        tcex=1.5d0  !Extra amount to add to Tc for mixtures
        pcex=1.5d0
      endif
      if (t.le.tc) psat=exp(13.85d0*(t/tc)-14.073d0)*pc
      if (t.le.tc*0.7d0) psat=psat/10.d0  !Some fluids have lower pressures than this equation below 70% of Tc
      if (t.le.tc*0.6d0) psat=psat/100.d0
      if (t.le.tc*0.5d0) psat=psat/100.d0
      if (t.le.tc*0.4d0) psat=psat/100.d0
      if (icomp.ne.0 .and. t.le.tc) then
        ierr=1
        if (hpsk(icomp).ne.'NBS') call PSATT (t,z,psat2,ierr,herr)
        if (ierr.eq.0) psat=0.95d0*psat2 !Drop 5% to allow for error
      else
c  drop cutoff for single phase vapor check by factor of 10
        psat=psat/10.d0
        if (nc.gt.1) psat=psat/100.d0
      endif
      if (icomp.ne.0 .and. t.lt.ttpn(icomp)) then
c  temperature less than triple point temperature, but in the gas phase.
        kph=2
        kguess=1
        D=p/(R*t)
        call TPRHO (t,p,z,kph,kguess,D,ierr,herr2)
        call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
        Dl=D
        Dv=D
        if (ierr.ne.0) then
          ierr=214
          write (herr,1214) herr2(1:190),hnull
          call ERRMSG (ierr,herr)
          q=999.d0     !quality undefined
          RETURN
        end if
      elseif (t.ge.tc*tcex .or. p.lt.psat .or. p.gt.pc*pcex) then
c  supercritical and vapor states (x = y = z as set above)
c       write (*,*) ' TPFLSH--supercritical'
        kph=2
        if (t.lt.tc .and. p.gt.pc) kph=1
        kguess=0
        call TPRHO (t,p,z,kph,kguess,D,ierr,herr2)
        call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
        Dl=D
        Dv=D
        if (p.le.pc) then
          q=998.d0     !superheated, but cannot define quality
        elseif (p.gt.pc .and. t.lt.tc) then
          q=-998.d0    !subcooled, but cannot define quality
        else
          q=999.d0     !quality undefined
        end if
        if (ierr.ne.0) then
          ierr=215
          write (herr,1215) herr2(1:175),hnull
 1215     format ('[TPFLSH error 215] supercritical or vapor density ',
     &            'iteration did not converge:  ',a175,a1)
          call ERRMSG (ierr,herr)
c  call two phase states if t is close to tc for a mixture
          if (icomp.eq.0 .and. t.lt.tc*1.1d0) then
            t=tc*1.1d0
            goto 150
          endif
          RETURN
        end if
c       do i=1,nc
c         x(i)=z(i)
c         y(i)=z(i)
c       enddo
      else
c  subcritical state--call saturation routine to determine liq or vap
        i=iprnterr
        iprnterr=0
        call SATT (t,z,2,pdew,Dldew,Dvdew,xdew,ydew,ierr,herr2)
        iprnterr=i
        iflag=0
        if (ierr.ne.0 .and. t.gt.tc-5) then
          pdew=pc
          Dldew=rhoc
          Dvdew=rhoc
        elseif (ierr.ne.0 .and. icomp.ne.0) then
          ierr=212
          write (herr,1212) herr2(1:193),hnull
 1212     format ('[TPFLSH error 212] dew point calculation ',
     &            'did not converge:  ',a193,a1)
          call ERRMSG (ierr,herr)
          D=0.0d0
          Dl=0.0d0
          Dv=0.0d0
          q=999.d0     !quality undefined
          RETURN
        else if (ierr.ne.0) then
c  do not exit yet, check if valid liquid state
          iflag=1
          pdew=0
        end if
        if (p.lt.pdew) then
c  single-phase vapor (x = y = z as set above)
c         write (*,*) ' TPFLSH--single-phase vapor'
          kph=2
          kguess=1
          D=Dvdew*p/pdew                     !initial guess for density
          call TPRHO (t,p,z,kph,kguess,D,ierr,herr2)
          Dl=D
          Dv=D
          if (ierr.ne.0) then
            ierr=214
            write (herr,1214) herr2(1:190),hnull
 1214       format ('[TPFLSH error 214] vapor density iteration ',
     &              'did not converge:  ',a190,a1)
            call ERRMSG (ierr,herr)
            q=999.d0     !quality undefined
            RETURN
          end if
          call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
          q=998.d0
          if (ianc(icomp).eq.0 .and. t.lt.tc) then
            call ENTHAL (t,Dldew,xdew,hldew)
            call ENTHAL (t,Dvdew,z,hvdew)
            q=(h-hldew)/(hvdew-hldew)
            if (q.gt.0.0d0 .and. q.lt.1.0d0) q=998.d0
          endif
        else
          if (icomp.ne.0 .and. ianc(icomp).eq.0) then
c  special case:  pure component single-phase liquid
c           write (*,*) ' TPFLSH--pure fluid single-phase liquid'
            kph=1
            kguess=1
            D=Dldew
            call TPRHO (t,p,z,kph,kguess,D,ierr,herr2)
            Dl=D
            Dv=D
            if (ierr.ne.0) then
              ierr=213
              write (herr,1213) herr2(1:189),hnull
 1213         format ('[TPFLSH error 213] liquid density iteration ',
     &                'did not converge:  ',a189,a1)
              call ERRMSG (ierr,herr)
              q=999.d0     !quality undefined
              RETURN
            end if
            call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
c  compute quality based on volumes (possible for T,h to be
c  double-valued in compressed liquid)
            q=(1.0d0/D-1.0d0/Dldew)/(1.0d0/Dvdew-1.0d0/Dldew)
          else
c  mixture:  calculate bubble point to determine if liquid or 2-phase
            call SATT (t,z,1,pbub,Dlbub,Dvbub,xbub,ybub,ierr,herr2)
            if (ierr.ne.0 .and. t.gt.tc-5.0d0) then
              pbub=pc
              Dlbub=rhoc
              Dvbub=rhoc
            elseif (ierr.ne.0) then
              ierr=211
              write (herr,1211) herr2(1:190),hnull
 1211         format ('[TPFLSH error 211] bubble point calculation ',
     &                'did not converge:  ',a190,a1)
              call ERRMSG (ierr,herr)
              D=0.0d0
              Dl=0.0d0
              Dv=0.0d0
              q=999.d0     !quality undefined
              RETURN
            end if
            if (p.ge.pbub) then
c  mixture single-phase liquid
c             write (*,*) ' TPFLSH--mixture single-phase liquid'
              kph=1
              kguess=1
              D=Dlbub
              call TPRHO (t,p,z,kph,kguess,D,ierr,herr2)
              Dl=D
              Dv=D
              if (ierr.ne.0 .or. (d.lt.dlbub .and. d.gt.dvdew)) then
                ierr=213
                write (herr,1213) herr2(1:189),hnull
                call ERRMSG (ierr,herr)
                q=999.d0     !quality undefined
                RETURN
              end if
              call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
c  compute quality based on volumes (possible for T,h to be
c  double-valued in compressed liquid
              if (abs(dvdew-dlbub).gt.1.d-12)
     &            q=(1.0d0/D-1.0d0/Dlbub)/(1.0d0/Dvdew-1.0d0/Dlbub)
            else
c
c  two-phase mixture
c             write (*,*) ' TPFLSH--two-phase mixture'
c
c  generate initial guesses for x and y by interpolating input pressure
c  with dew and bubble point pressures
c
              if (iflag.eq.1) then
c  dew point did not converge, location of state point is either two-phase
c  or vapor phase
                ierr=212
                write (herr,1212) herr2(1:193),hnull
                call ERRMSG (ierr,herr)
                D=0.0d0
                Dl=0.0d0
                Dv=0.0d0
                q=999.d0     !quality undefined
                RETURN
              endif
              if (ABS(1.0d0-pbub/pdew).lt.1.0d-6
     &           .and. ianc(icomp).eq.0) then
c  possible azeotrope
                q=0.5d0
              else
                q=1.0d0-(p-pdew)/(pbub-pdew)
              end if
              Dl=Dlbub                 !initial guess for liquid density
              Dv=Dvdew                 !initial guess for vapor density
              if (icomp.eq.0 .or. ianc(icomp).eq.0) then
                xsum=0.0d0
                ysum=0.0d0       !sums for normalization of compositions
                do i=1,nc
                  x(i)=(1.0d0-q)*z(i)+q*xdew(i)
                  y(i)=q*z(i)+(1.0d0-q)*ybub(i)
                  xsum=xsum+x(i)
                  ysum=ysum+y(i)
                enddo
                do i=1,nc
                  x(i)=x(i)/xsum
                  y(i)=y(i)/ysum
                enddo
                call TPFL2 (t,p,z,Dl,Dv,x,y,q,ierr,herr)
                if (ierr.ne.0) then
c  two-phase iteration did not converge--error message written by TPFL2
                  q=999.d0     !quality undefined
                  d=rhoc
                  RETURN
                end if
c  compute 2-phase properties and load output variables
c  call TPRHO to ensure consistency of t, p, rho
                kguess=1
                call TPRHO (t,p,x,1,kguess,Dl,ierr1,herr1)
                call TPRHO (t,p,y,2,kguess,Dv,ierr1,herr1)
                if (ierr1.ne.0) then
                  herr2='[TPFLSH error] (mix, 2-phase):  '//herr1
                  call ERRMSG (ierr1,herr2)
                  RETURN
                end if
              endif
              call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
              call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
              cp=xnotd                   !Cp, w not defined for 2-phase
              w=xnotd
              cv=xnotd
c  bulk properties are weighted average of liquid and vapor phases
              alpha=1.0d0-q               !alpha is liq fraction,
              D=1.0d0/(alpha/Dl+q/Dv)     !q is vapor frac
              e=alpha*el+q*ev
              h=alpha*hl+q*hv
              s=alpha*sl+q*sv
            end if
          end if
        end if
      end if
c
c  if limits check resulted in warning (as opposed to error) return
c  that message; do this again in case intermediate iteration did not
c  converge (thereby overwriting any warning message from LIMITX)
c
      if (ierrl.lt.0) then
        ierr=ierrl
        write (herr,2006) ierrl,herrl(1:234),hnull
 2006   format ('[TPFLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierr,herr)
      end if
c
      if (D.gt.rhomax) then
c  check that computed (output) density is within bounds of EOS
        ierr=2
        write (herr,2008) ierr,t,p/1000.0d0,hnull
 2008   format ('[TPFLSH error',i3,']; input T,p correspond to ',
     &          'a density above the limit of the EOS; T =',g12.5,
     &          ' K, p =',g12.5,' MPa.',a1)
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                             !subroutine TPFLSH
c
c ======================================================================
c
      subroutine TPFL2 (t,p,z,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given temperature, pressure, and bulk composition
c
c  This routine accepts only two-phase states as input; if the phase is
c  not known use TPFLSH.  Use TPRHO for single-phase states.
c
c  inputs:
c        t--temperature [K]
c        p--pressure [kPa]
c        z--overall (bulk) composition [array of mol frac]
c       Dl--initial guess for molar density [mol/L] of the liquid phase
c       Dv--initial guess for molar density [mol/L] of the vapor phase
c        x--initial guess for composition of liquid phase
c           [array of mol frac]
c        y--initial guess for composition of vapor phase
c           [array of mol frac]
c        q--initial guess for vapor quality on a MOLAR basis
c           [moles vapor/total moles]
c
c  outputs:
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  (these are also passed up to TPFLSH for output)
c                        0 = successful
c                      216 = TPRHO did not converge for liquid
c                      217 = TPRHO did not converge for vapor
c                      218 = inner loop (liquid frac) did not converge
c                      219 = outer loop (composition) did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  12-29-95  MM, original version, extracted from TPFLSH
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  04-09-97  MM, fix bug:  converged value of q not returned
c  04-11-97  MM, fix divide by zero on saturation boundary
c                adjust tolr --> d-7; allow alpha outside 0,1 by d-5
c  07-15-97  MM, renumber and add detail to error messages
c                get flags for 'not defined' from common /FLAGS/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  11-01-00 EWL, change tol from -7 to -9 so that TPFLSH finds a better root.
c  12-13-00 EWL, changed the calculation of y from x*xk to y*fliq/fvap.  Before
c                this, sum(y) was always equal to one on the first iteration
c                and the program exited prematurely
c  02-20-01 EWL, changed the tolerance after 15 iterations to increase the
c                chance of converging
c  02-26-01 EWL, if dpdrho is too large, liquid root is no good,
c                use SATP to get dl
c  08-09-01 EWL, made the data statement for tolr into a regular executable line
c  09-11-02 EWL, increase itmax to 80
c  09-11-02 EWL, at every 10th iteration, use average of old and new compositions
c                to get next composition.  Helps when x and y are jumping back and forth
c  07-22-03 EWL, modify how alpha(3) is calculated
c  08-23-06 EWL, changed the tolerance after 50 iterations
c  12-04-09 EWL, add call to new routine of Diego Ortiz
c  12-04-12 EWL, add xinc and xdir to flip values of x(1) back and forth instead of just up
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TPFL2
c
      include 'commons.for'
      character*255 herr,herr2
      dimension z(ncmax),x(ncmax),y(ncmax),xs(ncmax),ys(ncmax)
      dimension xbub(ncmax),xdew(ncmax)
      dimension alpha(3),falpha(2),fliq(ncmax),fvap(ncmax),xk(ncmax)
      parameter (itmax=80)
c
      tolr=1.0d-9
      ierr=0
      herr=' '
c
c  call new code first to see if it finds a root
      if (iSatFlg.eq.0) then
        iFlash=0
        call SATTP(t,p,z,iFlash,1,d,Dl,Dv,x,y,q,ierr,herr)
        if (ierr.gt.0)
     &    call SATTP(t,p,z,iFlash,0,d,Dl,Dv,x,y,q,ierr,herr)
        if (ierr.eq.0) return
      endif
c
c  begin outer iteration loop for the vapor composition
c
      alpha(1)=1.0d0-q
      kguess=1
      j=1
      do i=1,nc
        ys(i)=y(i)
      enddo
      xinc=0.01d0
      xdir=1.d0
      do 200 iy=1,itmax
      if (iy.eq.12) tolr=tolr*100.d0
      if (iy.eq.24) tolr=tolr*100.d0
      if (iy.eq.50) tolr=tolr*100.d0
c  compute densities and fugacities for each phase
c     write (*,1001) t,p,(x(i),i=1,5)
c1001 format (1x,' TPFL2 call TPRHO; liq t,p,x:  ',f10.5,f14.6,5f10.6)
      call TPRHO (t,p,x,1,kguess,Dl,ierr,herr2)
      call DPDD (t,Dl,x,dpdrho)
      if (ABS(dpdrho).gt.1.d7) then
        call SATP (p,x,1,tt,Dl,Dvdew,xbub,xdew,ierr2,herr2)
      endif
      if (ierr.ne.0) then
c  try modifying the composition to get a workable solution.
        x(1)=x(1)+xdir*xinc
        do i=2,nc
          x(i)=x(i)-xdir*xinc/DBLE(nc-1)
        enddo
        xinc=xinc+0.01d0
        xdir=-xdir
        if (x(1).lt.1.d0 .and. x(1).gt.0.d0) goto 200
        ierr=216
        write (herr,1216) herr2(1:170),hnull
 1216   format ('[TPFLSH error 216] liquid density iteration for ',
     &          '2-phase state did not converge:  ',a170,a1)
        call ERRMSG (ierr,herr)
        goto 840
      end if
      xinc=0.01d0
      xdir=1.d0
      call FGCTY2 (t,Dl,x,fliq,ierr,herr)
c     write (*,1002) t,p,(y(i),i=1,5)
c1002 format (1x,' TPFL2 call TPRHO; vap t,p,x:  ',f10.5,f14.6,5f10.6)
      call TPRHO (t,p,y,2,kguess,Dv,ierr,herr2)
      if (ierr.ne.0) then
        do i=1,nc
          y(i)=0.8d0*y(i)+0.2d0*ys(i)
        enddo
        goto 200
c       ierr=217
c       write (herr,1217) herr2(1:170),hnull
c1217   format ('[TPFLSH error 217] vapor density iteration for ',
c    &          '2-phase state did not converge:  ',a170,a1)
c       call ERRMSG (ierr,herr)
c       goto 840
      end if
      call FGCTY2 (t,Dv,y,fvap,ierr,herr)
c  compute k-factors (assume constant for a given guess of y)
      do i=1,nc
        if (z(i).gt.0.d0 .and. x(i).gt.0.d0) then
          xk(i)=fliq(i)*y(i)/(x(i)*fvap(i))
        else
c  zero composition for component i
          xk(i)=0.0d0
        end if
      enddo
c
c     write (*,1004) iy,(xk(i),i=1,nc)
c1004 format (1x,' TPFL2 ity,k-factors: ',i4,5e18.10)
c
c  begin inner iteration loop for the liquid fraction (alpha)
c
      j=1                      !iteration flag
c     lfapos=.false.           !flags for reguli-falsi iteration
c     lfaneg=.false.
      do i=1,nc
        xs(i)=x(i)
      enddo
      do ix=1,itmax
c       write (*,1040) ix,(x(i),i=1,nc)
c1040 format (1x,' TPFL2 itx,old liq comps:  ',i4,10x,5f16.12)
        xsum=0.0d0
        do i=1,nc
          if (z(i).gt.0.0d0) then
c  compute new guess for liquid composition
            x(i)=z(i)/(alpha(j)+xk(i)*(1.0d0-alpha(j)))
          else
c  zero composition for component i
            x(i)=0.0d0
          end if
          xsum=xsum+x(i)
        enddo
c  normalize liquid composition
        do i=1,nc
          x(i)=x(i)/xsum
        enddo
c       write (*,1050) ix,xsum,(x(i),i=1,nc)
c1050 format (1x,' TPFL2 itx,xsum,new comps: ',i4,6f16.12)
        falpha(j)=1.0d0-xsum
c       write (*,1056) ix,alpha(j),xsum,(x(i),i=1,3)
c1056 format (1x,' TPFL2:',i4,35x,2f10.6,3f10.6)
        if (ABS(falpha(j)).lt.1.0d-2*tolr) then
c  inner loop has converged
c  (tolerance on inner loop must be tighter than outer loop)
          goto 640
        else
c  store variables for possible use in reguli-falsi
c  reguli-falsi actually seems to slow convergence--comment out
c       if (falpha(j).lt.0.0d0) then
c         lfaneg=.true.
c         aneg=alpha(j)
c         faneg=falpha(j)
c       else
c         lfapos=.true.
c         apos=alpha(j)
c         fapos=falpha(j)
        end if
c  generate next guess for liquid fraction
        if (j.eq.1) then
c  for second iteration, move 5 % of way to alpha = 1.0
c         alpha(2)=alpha(1)+0.05d0*(1.0d0-alpha(1))
c  above could result in alpha(2) = alpha(1) if alpha(1) = 1
c  try moving towards alpha = 0.5 !MM 04-11-97
          j=2
          alpha(2)=alpha(1)+SIGN(0.05d0,0.5d0-alpha(1))
        else
c  for subsequent iterations, use secant method
          if (ABS(falpha(2)-falpha(1)).gt.1.0d-2*tolr) then
            alpha(3)=alpha(2)-falpha(2)*(alpha(2)-alpha(1))/
     &               (falpha(2)-falpha(1))
          else
            alpha(3)=alpha(1)+SIGN(tolr,0.5d0-alpha(1))
          end if
c  new guess for alpha must lie within bounds of zero and one
c  but allow small tolerance for slop in TPRHO and thus fugacities.
c         write (*,1060) ix,(alpha(i),i=1,3),(falpha(i),i=1,2)
c1060   format (1x,' TPFL2 itx,alpha,falpha:   ',i4,5f16.12)
c  alpha(3) generally goes out of bounds when the slope of falpha vs. x
c  is near flat.  Use next guess of half of alpha(2).
          if (alpha(3).gt.1.0d0+1.0d2*tolr) then
            alpha(3)=alpha(2)/2.d0
          else if (alpha(3).lt.0.0d0-1.0d2*tolr) then
            alpha(3)=alpha(2)/2.d0
          end if
c  check that new guess is not outside previous bounds
c         if (lfaneg .and. lfapos .and. (alpha(3).gt.MAX(fapos,faneg)
c    &       .or. alpha(3).lt.MIN(fapos,faneg))) then
c  if so, use reguli-falsi
c           alpha(3)=apos-fapos*(apos-aneg)/(fapos-faneg)
c         end if
c  discard oldest iteration
          alpha(1)=alpha(2)
          alpha(2)=alpha(3)
          falpha(1)=falpha(2)
        end if
      enddo
c  inner iteration loop has not converged, issue warning and proceed
      ierr=-218
      write (herr,1218) hnull
 1218 format ('[TPFLSH warning 218] inner iteration loop for liquid ',
     &        'fraction in 2-phase state did not converge:  ',a1)
      call ERRMSG (ierr,herr)
c
c  end of inner iteration loop for liquid fraction
c
 640  continue
      ysum=0.0d0
c  compute next guess for vapor composition, using x(i) from inner loop
      do i=1,nc
c       y(i)=x(i)*xk(i)                !Old way, but incorrect
        if (fliq(i).eq.xerr .or. fvap(i).eq.xerr) goto 210
        ys(i)=y(i)
        if (fvap(i).ne.0.d0) y(i)=y(i)*fliq(i)/fvap(i)
        ysum=ysum+y(i)
      enddo
c  normalize vapor compositions
      do i=1,nc
        y(i)=y(i)/ysum
      enddo
c     write (*,1076) iy,t,Dl,Dv,ysum,(y(i),i=1,3)
c1076 format (1x,' TPFL2:',i4,f7.2,2e14.6,10x,f10.6,3f10.6)
      if (ABS(1.0d0-ysum).lt.tolr) then
c  outer loop has converged (also check for warning from inner loop)
        if (ierr.ne.0) then
          ierr=218
          write (herr,2218) hnull
 2218     format ('[TPFLSH error 218] inner iteration loop for liquid ',
     &            'fraction in 2-phase state did not converge:  ',a1)
          call ERRMSG (ierr,herr)
        end if
        if (iy.gt.1) goto 840
      end if
      if (int(iy/10)*10.eq.iy) then
        do i=1,nc
          x(i)=(x(i)+xs(i))/2.d0
          y(i)=(y(i)+ys(i))/2.d0
        enddo
      endif
 200  continue
c  outer iteration loop has not converged, issue error and return
 210  ierr=219
      write (herr,1219) hnull
 1219 format ('[TPFLSH error 219] outer iteration loop for ',
     &        'composition in 2-phase state did not converge:  ',a1)
      call ERRMSG (ierr,herr)
c
c  end of outer iteration loop for vapor composition
c
 840  continue
      q=1.0d0-alpha(j)
c
c  call new saturation solver to attempt to get convergence
c     if (ierr.gt.0 .and. nc.gt.1 .and. iSatFlg.eq.0) then
c       call SATTP(t,p,z,0,0,d,Dl,Dv,x,y,q,ierr,herr)
c     endif
c
      RETURN
      end                                              !subroutine TPFL2
c
c ======================================================================
c
      subroutine TDFLSH (t,D,z,p,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given temperature, bulk density, & bulk composition
c
c  This routine accepts both single-phase and two-phase states as input,
c  if the phase is known, then subroutine THERM is (much) faster.
c
c  inputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, xl = xv = x
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                        1 = Tin < Tmin
c                        2 = Din > Dmax or Din < 0
c                        3 = T and D out of range
c                        8 = x out of range (component and/or sum < 0
c                            or > 1)
c                        9 = x and T out of range
c                       10 = x and D out of range
c                       11 = x and T and D out of range
c                      220 = CRITP did not converge
c                      221 = SATT did not converge at bubble point
c                      222 = SATT did not converge at dew point
c                      223 = SATT (bubble pt) did not converge for 2-ph
c                      224 = SATT (dew pt) did not converge for 2-phase
c                      225 = TPFL2 did not converge
c                      226 = 2-phase iteration did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  09-20-95  MM, original version
c  09-25-95  MM, rearrange argument list (outputs in order rho, x, q)
c  10-11-95  MM, RETURN if any error detected
c  11-08-95  MM, patch to allow testing of GUI with mixtures
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-13-95  MM, set undefined output compositions to zero
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  01-07-97  MM, error message bug (do not concatenate herr with itself)
c  01-21-97  MM, return error message for mixtures (not yet implemented)
c  04-09-97  MM, add mixture calculations; restructure cases
c                add limits checks
c  04-22-97  MM, use h, rather than s, to compute q (h converges at p = 0)
c  05-15-97  MM, use V to compute q for comp. liq. (T,h can be double-valued),
c                add q = 998 case for t > Tc, but p < Pc
c  07-15-97  MM, renumber and add detail to error messages
c                get flags for 'not defined' from common /FLAGS/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  10-16-97  MM, fix bug--out of range errors not caught properly
c  12-01-97  MM, error code not passed correctly if inputs out of range
c  02-11-98  MM, check that computed pressure is within bounds
c  03-05-99 EWL, remove check on pressure, and call LIMITX to check for
c                p>pmax or p>melt
c  12-16-02 EWL, add checks for pseudo-pure fluid, calculate p using q
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TDFLSH
c
      include 'commons.for'
      character*255 herr,herr1,herr2,herrl
      dimension x(ncmax),y(ncmax),z(ncmax)
      dimension xbub(ncmax),xdew(ncmax),ybub(ncmax),ydew(ncmax)
c
      call ISPURE (z,icomp)
      ierr=0
      herr=' '
c
c  set output liquid and vapor compositions to input values
c  zero output values for undefined components
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c     if (nc.lt.ncmax) then
c       do i=nc+1,ncmax
c         x(i)=0.0d0
c         y(i)=0.0d0
c       enddo
c     end if
      if (d.lt.1.0d-14) then
c  ideal-gas
        p=D*R*t
        Dl=D
        Dv=D
        call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
        RETURN
      endif
c
c  check that input conditions are within limits
c
      pdum=0.0d0
      call LIMITX ('EOS',t,D,pdum,z,tmin,tmax,rhomax,pmax,ierrl,herrl)
      if (ierrl.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ierrl,herrl(1:234),hnull
 1006   format ('[TDFLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierrl,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierrl.gt.0) then
        write (herr,1007) ierrl,herrl(1:236),hnull
 1007   format ('[TDFLSH error',i3,'] ',a236,a1)
        call ERRMSG (ierrl,herr)
        q=999.d0     !quality undefined
        ierr=ierrl
        RETURN
      end if
c
      call CRITP (z,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
        ierr=220
        write (herr,1008) herr2(1:235),hnull
 1008   format ('[TDFLSH error 220] ',a235,a1)
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
        RETURN
      end if
c
      if (icomp.ne.0 .and. t.lt.ttpn(icomp)) then
c  temperature less than triple point temperature, but in the gas phase.
        call THERM (t,D,z,p,e,h,s,cv,cp,w,hjt)
        Dl=D
        Dv=D
      elseif (t.ge.tc .or. D.lt.1.0d-10) then
c  super-critical state or rho = 0 (x = y = z as set above)
        call THERM (t,D,z,p,e,h,s,cv,cp,w,hjt)
        Dl=D
        Dv=D
        if (p.le.pc) then
          q=998.d0     !superheated, but cannot define quality
        else
          q=999.d0     !quality undefined
        end if
      else
c  sub-critical state--call saturation routine to determine phase
        call SATT (t,z,2,pdew,Dldew,Dvdew,xdew,ydew,ierr,herr1)
        if (ierr.ne.0) then
          ierr=222
          write (herr,1222) herr1(1:193),hnull
 1222     format ('[TDFLSH error 222] dew point calculation ',
     &            'did not converge:  ',a193,a1)
          call ERRMSG (ierr,herr)
          D=0.0d0
          Dl=0.0d0
          Dv=0.0d0
          q=999.d0     !quality undefined
          RETURN
        end if
        if (icomp.ne.0 .and. ianc(icomp).eq.0) then
c  if pure, bubble point density provided by call to SATT at dew point
          Dlbub=Dldew
          Dvbub=Dvdew
          pbub=pdew
        else
c  if mixture, call saturation routine again at bubble point
          call SATT (t,z,1,pbub,Dlbub,Dvbub,xbub,ybub,ierr,herr1)
          if (ierr.ne.0) then
            ierr=221
            write (herr,1221) herr1(1:190),hnull
 1221       format ('[TPFLSH error 221] bubble point calculation ',
     &              'did not converge:  ',a190,a1)
            call ERRMSG (ierr,herr)
            D=0.0d0
            Dl=0.0d0
            Dv=0.0d0
            q=999.d0     !quality undefined
            RETURN
          end if
        end if
        if (D.le.Dvdew .or. D.ge.Dlbub) then
c  single-phase (liq or vapor) (pure or mixture) (x = y = z as set above)
          call THERM (t,D,z,p,e,h,s,cv,cp,w,hjt)
          Dl=D
          Dv=D
          call ENTHAL (t,Dvdew,z,hvdew)
c         call ENTHAL (t,Dlbub,z,hlbub)
c         q=(h-hlbub)/(hvdew-hlbub)
c  compute quality based on volumes (possible for T,h to be
c  double-valued in compressed liquid)
          if (abs(dvdew-dlbub).gt.1.d-12)
     &        q=(1.0d0/D-1.0d0/Dlbub)/(1.0d0/Dvdew-1.0d0/Dlbub)
        else
          if (icomp.ne.0) then
c  special case:  pure-fluid two-phase (x = y = z as set above)
            if (abs(dvdew-dlbub).gt.1.d-12)
     &          q=(1.0d0/D-1.0d0/Dlbub)/(1.0d0/Dvdew-1.0d0/Dlbub)
            p=q*pdew+(1.0d0-q)*pbub
            Dl=Dlbub
            Dv=Dvdew
          else
c  general case:  mixture 2-phase
            ksat=1   !bubble and dew point data provided to TDFL2
            call TDFL2 (t,D,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  p,Dl,Dv,x,y,q,ierr,herr)
            if (ierr.ne.0) then
c  two-phase iteration did not converge--error message written by TDFL2
              q=999.d0     !quality undefined
              RETURN
            end if
c           write (*,1225) p,q,(x(i),i=1,3),(y(i),i=1,3)
c1225       format (1x,' TDFLSH--TDFL2 return   p,q,x,y = ',8f12.7)
          end if
c  compute remaining properties for 2-phase states
          call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
          call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
          if (nc.ge.2) p=ptherm
          e=q*ev+(1.0d0-q)*el
          h=q*hv+(1.0d0-q)*hl
          s=q*sv+(1.0d0-q)*sl
          w=xnotd     !Cp,w not defined for 2-phase states
          cp=xnotd
          cv=xnotd
        end if
      end if
c
      call LIMITX ('EOS',t,D,p,z,tmin,tmax,rhomax,pmax,ierr,herrl)
      if (ierr.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ierr,herrl(1:234),hnull
        call ERRMSG (ierr,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierr.gt.0) then
        write (herr,1007) ierr,herrl(1:236),hnull
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
      end if
c
      RETURN
      end                                             !subroutine TDFLSH
c
c ======================================================================
c
      subroutine TDFL2 (t,D,z,ksat,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  p,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given temperature, bulk density, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general temperature-density flash routine
c  TDFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than TDFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     pbub--bubble point pressure [kPa] at (t,x=z)
c     pdew--dew point pressure [kPa] at (t,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        p--pressure [kPa]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: TDFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (t,d,z,1,ksat,'TD',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 tt,p,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine TDFL2
c
c ======================================================================
c
      subroutine PDFLSH (p,D,z,t,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given density, pressure, and bulk composition
c
c  This routine accepts both single-phase and two-phase states as the
c  input; for single-phase calculations, the subroutine PDFL1 is faster.
c
c  inputs:
c        D--overall (bulk) molar density [mol/L]
c        p--pressure [kPa]
c        z--overall (bulk) composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = 998 superheated vapor, but quality not defined (in most situations, t > Tc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                      210 = CRITP did not converge
c                      211 = SATT did not converge at bubble point
c                      212 = SATT did not converge at dew point
c                      213 = TPRHO did not converge for liquid state
c                      214 = TPRHO did not converge for vapor state
c                      215 = TPRHO did not convg for supercritical state
c                      216 = TPRHO did not convg for liq in 2-phase it
c                      217 = TPRHO did not convg for vap in 2-phase it
c                      218 = liquid frac for 2-phase it did not converge
c                      219 = composition for 2-phase it did not converge
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  03-03-99 EWL, original version
c  03-14-00 EWL, change name from DPFLSH to PDFLSH
c  05-25-00 EWL, set t=-1 in initial call to LIMITX
c  12-16-02 EWL, add checks for pseudo-pure fluid, calculate t using q
c  11-21-12 EWL, return q=-998 when p>pc and t<tc
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PDFLSH
c
      include 'commons.for'
      character*255 herr,herr2,herrl
      dimension z(ncmax),x(ncmax),y(ncmax)
      dimension xdew(ncmax),ydew(ncmax),ybub(ncmax),xbub(ncmax)
c
      call ISPURE (z,icomp)
      ierr=0
      herr=' '
      t=0.d0
      Dl=0.d0
      Dv=0.d0
      e=0.d0
      h=0.d0
      s=0.d0
      cv=0.d0
      cp=0.d0
      w=0.d0
      q=999.0d0
c
c  initialize output liquid and vapor compositions to input values
c  zero output values for undefined components
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c     if (nc.lt.ncmax) then
c       do i=nc+1,ncmax
c         x(i)=0.0d0
c         y(i)=0.0d0
c       enddo
c     end if

      if (p.lt.1.0d-14 .or. d.lt.1.0d-14) then
c  ideal-gas
        if (D.gt.0.d0) t=p/(R*D)
        RETURN
      end if
c
c  check that input conditions are within limits
c
      call CRITP (z,tc,pc,rhoc,ierr,herr2)
      if (ierr.gt.0) then
        ierr=210
        write (herr,1008) herr2(1:235),hnull
 1008   format ('[PDFLSH error 210] ',a235,a1)
        call ERRMSG (ierr,herr)
        RETURN
      end if
c
      t=-1
      call LIMITX ('EOS',t,D,p,z,tmin,tmax,rhomax,pmax,ierrl,herrl)
      if (ierrl.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ierrl,herrl(1:234),hnull
 1006   format ('[PDFLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierrl,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierrl.gt.0) then
        write (herr,1007) ierrl,herrl(1:236),hnull
 1007   format ('[PDFLSH error',i3,'] ',a236,a1)
        call ERRMSG (ierrl,herr)
        ierr=ierrl
        RETURN
      end if
c
      if (p.ge.pc) then
c  supercritical state (x = y = z as set above)
c       write (*,*) ' PDFLSH--supercritical'
        call PDFL1 (p,D,z,t,ierr,herr2)
        if (ierr.ne.0) then
          ierr=215
          write (herr,1215) herr2(1:183),hnull
 1215     format ('[PDFLSH error 215] supercritical density iteration ',
     &            'did not converge:  ',a183,a1)
          call ERRMSG (ierr,herr)
          RETURN
        end if
        Dl=D
        Dv=D
        call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
        if (t.le.tc) q=-998.d0
      else
c  subcritical state--call saturation routine to determine liq or vap
        i=iprnterr
        iprnterr=0
        ierr=0
        Dvdew=D
        Dldew=D
        tdew=0
c  do not call SATP if P<triple point pressure for pure fluids
        if (icomp.eq.0 .or. p.gt.ptpn(icomp) .or. ianc(icomp).ne.0) then
          call SATP (p,z,2,tdew,Dldew,Dvdew,xdew,ydew,ierr,herr2)
        endif
        iprnterr=i
        iflag=0
        if (ierr.ne.0 .and. icomp.ne.0) then
          ierr=212
          write (herr,1212) herr2(1:193),hnull
 1212     format ('[PDFLSH error 212] dew point calculation ',
     &            'did not converge:  ',a193,a1)
          call ERRMSG (ierr,herr)
          RETURN
        elseif (ierr.ne.0) then
c  do not exit yet, check if valid liquid state
          iflag=1
        end if
        if (D.le.Dvdew+1.0d-12) then
c  single-phase vapor (x = y = z as set above)
c         write (*,*) ' PDFLSH--single-phase vapor'
          call PDFL1 (p,D,z,t,ierr,herr2)
          if (ierr.ne.0) then
            ierr=214
            write (herr,1214) herr2(1:190),hnull
 1214       format ('[PDFLSH error 214] vapor density iteration ',
     &              'did not converge:  ',a190,a1)
            call ERRMSG (ierr,herr)
            t=0.0d0
            RETURN
          end if
          Dl=D
          Dv=D
          call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
          q=998.d0
          if (tdew.gt.0 .and. ianc(icomp).eq.0) then
            call ENTHAL (tdew,Dldew,xdew,hldew)
            call ENTHAL (tdew,Dvdew,z,hvdew)
            q=(h-hldew)/(hvdew-hldew)
          endif
        else
c  mixture:  calculate bubble point to determine if liquid or 2-phase
          if (icomp.ne.0 .and. ianc(icomp).eq.0) then
            Dlbub=Dldew
            Dvbub=Dvdew
            tbub=tdew
          else
            call SATP (p,z,1,tbub,Dlbub,Dvbub,xbub,ybub,ierr,herr2)
          endif
          if (ierr.ne.0) then
            ierr=211
            write (herr,1211) herr2(1:190),hnull
 1211       format ('[PDFLSH error 211] bubble point calculation ',
     &              'did not converge:  ',a190,a1)
            call ERRMSG (ierr,herr)
            RETURN
          end if
          if (D.gt.Dlbub) then
c  mixture single-phase liquid
c           write (*,*) ' TPFLSH--mixture single-phase liquid'
            call PDFL1 (p,D,z,t,ierr,herr2)
            if (ierr.ne.0) then
              ierr=213
              write (herr,1213) herr2(1:189),hnull
 1213         format ('[PDFLSH error 213] liquid density iteration ',
     &                'did not converge:  ',a189,a1)
              call ERRMSG (ierr,herr)
              t=0.0d0
              RETURN
            end if
            Dl=D
            Dv=D
            call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
c  compute quality based on volumes
            q=(1.0d0/D-1.0d0/Dlbub)/(1.0d0/Dvdew-1.0d0/Dlbub)
c           write (*,*) ' PDFLSH--comp liq q by volumes:  ',q
          else
c
c  two-phase pure fluid
            if (icomp.ne.0) then
              Dl=Dlbub
              Dv=Dvdew
              q=(1.0d0/D-1.0d0/Dl)/(1.0d0/Dv-1.0d0/Dl)
              t=(1.d0-q)*tbub+q*tdew
              if (ianc(icomp).eq.1) call PTANC (t,p,q,d,'D',Dl,Dv)
c  two-phase mixture
c             write (*,*) ' PDFLSH--two-phase mixture'
c
c  generate initial guesses for x and y by interpolating input pressure
c  with dew and bubble point pressures
c
            else
              if (iflag.eq.1) then
c  dew point did not converge, location of state point is either two-phase
c  or vapor phase
                ierr=212
                write (herr,1212) herr2(1:193),hnull
                call ERRMSG (ierr,herr)
                t=0.0d0
                RETURN
              endif
              ksat=1   !bubble and dew point data provided to PDFL2
              call PDFL2 (p,D,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                    t,Dl,Dv,x,y,q,ierr,herr)
              if (ierr.ne.0) then
c  two-phase iteration did not converge--error message written by PDFL2
                q=999.d0     !quality undefined
                RETURN
              end if
c             write (*,1225) p,q,(x(i),i=1,3),(y(i),i=1,3)
c1225         format (1x,' TDFLSH--TDFL2 return   p,q,x,y = ',8f12.7)
            endif
            call THERM (t,Dl,x,ptherm,el,hl,sl,cv,cp,w,hjt)
            call THERM (t,Dv,y,ptherm,ev,hv,sv,cv,cp,w,hjt)
            cp=xnotd                   !Cp, w not defined for 2-phase
            w=xnotd
            cv=xnotd
c  bulk properties are weighted average of liquid and vapor phases
            alpha=1.0d0-q               !alpha is liq fraction,
            e=alpha*el+q*ev
            h=alpha*hl+q*hv
            s=alpha*sl+q*sv
          end if
        end if
      end if
c
c  if limits check resulted in warning (as opposed to error) return
c  that message; do this again in case intermediate iteration did not
c  converge (thereby overwriting any warning message from LIMITX)
c
      call LIMITX ('EOS',t,D,p,z,tmin,tmax,rhomax,pmax,ierrl,herrl)
      if (ierrl.lt.0) then
c  one or inputs are outside limits--if just a warning proceed w/ calc
        ierr=ierrl
        write (herr,2006) ierrl,herrl(1:234),hnull
 2006   format ('[PDFLSH warning',i3,'] ',a234,a1)
        call ERRMSG (ierr,herr)
c  if error (as opposed to warning) set output density to zero and return
      else if (ierrl.gt.0) then
        write (herr,1007) ierrl,herrl(1:236),hnull
        ierr=ierrl
        call ERRMSG (ierrl,herr)
        RETURN
      end if
c
      RETURN
      end                                             !subroutine PDFLSH
c
c ======================================================================
c
      subroutine PDFL1 (p,rho,x,t,ierr,herr)
c
c  iterate for single-phase temperature as a function of density, pressure,
c  and composition
c
c  inputs:
c      rho--molar density [mol/L]
c        p--pressure [kPa]
c        x--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PDFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (rho,p,x,0,'DP',0.d0,0.d0,t,pp,dd,ierr,herr)
      RETURN
      end                                              !subroutine PDFL1
c
c ======================================================================
c
      subroutine PDFL2 (p,d,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given pressure, bulk density, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general pressure-density flash routine
c  PDFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than PDFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        p--pressure [kPa]
c        d--overall (bulk) density [mol/L]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     tbub--bubble point temperature [K] at (p,x=z)
c     tdew--dew point temperature [K] at (p,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        t--temperature [K]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PDFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (p,d,z,1,ksat,'PD',
     &                  tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,pp,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine PDFL2
c
c ======================================================================
c
      subroutine PHFLSH (p,h,z,t,D,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
c
c  flash calculation given pressure, bulk enthalpy, and bulk composition
c
c  inputs:
c        p--pressure [kPa]
c        h--overall (bulk) enthalpy [J/mol]
c        z--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see PBFLSH for others)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  09-20-95  MM, original version
c  09-25-95  MM, rearrange argument list (outputs in order rho, x, q)
c  10-11-95  MM, correct: t not returned for two-phase pure-component
c  10-11-95  MM, RETURN if any error detected
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-13-95  MM, set undefined output compositions to zero
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  01-07-97  MM, error message bug (do not concatenate herr with itself)
c  04-30-97  MM, restructure logic to parallel TDFLSH
c  05-15-97  MM, add q = -998 case for p > Pc, but t < Tc
c  05-21-97  MM, call ENTHAL at rho = 1d-10 to define hmax
c  07-15-97  MM, renumber and add detail to error messages
c                get flags for 'not defined' from common /FLAGS/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  10-16-97  MM, evaluate hmin at p = 100 kPa, rather than p = pmax
c  02-11-98  MM, check that computed density is within bounds
c  05-25-00 EWL, call limitx with tdum=-1 to avoid check on tmelt
c  05-26-00 EWL, check for p<ptrp after calling SATP
c  06-06-00  MM, input h may have been changed by call to THERM
c  08-20-00 EWL, change initial guess from tc to tc+10
c  01-18-01 EWL, remove code and call generic PBFLSH
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PHFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call PBFLSH (p,h,z,'PH',t,D,Dl,Dv,x,y,q,e,hh,s,cv,cp,w,ierr,herr)
      RETURN
      end                                             !subroutine PHFLSH
c
c ======================================================================
c
      subroutine PHFL1 (p,h,x,kph,t,D,ierr,herr)
c
c  flash calculation given pressure, enthalpy, and composition.  This routine
c  accepts only single-phase inputs, it is intended primarily for use with
c  the more general flash routine PHFLSH.
c
c  inputs:
c        p--pressure [kPa]
c        h--enthalpy [J/mol]
c        x--composition [array of mol frac]
c      kph--phase flag:  1 = liquid
c                        2 = vapor
c        t--initial guess for temperature [K]
c        D--initial guess for molar density [mol/L]
c
c  outputs:
c        t--temperature [K]
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PHFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (p,h,x,kph,'PH',0.d0,0.d0,t,pp,D,ierr,herr)
      RETURN
      end                                              !subroutine PHFL1
c
c ======================================================================
c
      subroutine PHFL2 (p,h,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given pressure, bulk enthalpy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general pressure-enthalpy flash routine
c  PHFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than PHFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        p--pressure [kPa]
c        h--overall (bulk) molar enthalpy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     tbub--bubble point temperature [K] at (p,x=z)
c     tdew--dew point temperature [K] at (p,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        t--temperature [K]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PHFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (p,h,z,1,ksat,'PH',
     &                  tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,pp,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine PHFL2
c
c ======================================================================
c
      subroutine PSFLSH (p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)
c
c  flash calculation given pressure, bulk entropy, and bulk composition
c
c  inputs:
c        p--pressure [kPa]
c        s--overall (bulk) entropy [J/mol-K]
c        z--composition array (mol frac)
c
c  outputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, xl = xv = x
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see PBFLSH for others)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by M. McLinden, NIST Thermophysics Division, Boulder, Colorado
c  09-22-95  MM, original version
c  09-25-95  MM, rearrange argument list (outputs in order rho, x, q)
c  10-11-95  MM, RETURN if any error detected
c  11-29-95  MM, variable lower limit on coefficient/constant arrays
c                to accommodate ECS reference fluid
c  12-13-95  MM, set undefined output compositions to zero
c  02-27-96  MM, parameter n0=-ncmax to accommodate ECS-thermo model
c  01-07-97  MM, error message bug (do not concatenate herr with itself)
c  05-01-97  MM, restructure logic to parallel TDFLSH, PHFLSH
c  05-15-97  MM, add q = -998 case for p > Pc, but t < Tc
c  07-15-97  MM, renumber and add detail to error messages
c                get flags for 'not defined' from common /FLAGS/
c  10-01-97  MM, add compiler switch to allow access by DLL
c  10-16-97  MM, evaluate smin at p = 100 kPa, rather than p = pmax
c  02-11-98  MM, check that computed density is within bounds
c  03-31-00 EWL, call limitx at very end to ensure that p<pmelt and d<dmax
c  03-31-00 EWL, if routine for p>pc fails, try another initial t before quitting
c  05-26-00 EWL, check for p<ptrp after calling SATP
c  06-06-00  MM, inputs may have been changed by call to THERM
c  01-18-01 EWL, remove code and call generic PBFLSH
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PSFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call PBFLSH (p,s,z,'PS',t,D,Dl,Dv,x,y,q,e,h,ss,cv,cp,w,ierr,herr)
      RETURN
      end                                             !subroutine PSFLSH
c
c ======================================================================
c
      subroutine PSFL1 (p,s,x,kph,t,D,ierr,herr)
c
c  flash calculation given pressure, entropy, and composition.  This routine
c  accepts only single-phase inputs, it is intended primarily for use with
c  the more general flash routine PSFLSH.
c
c  inputs:
c        p--pressure [kPa]
c        s--entropy [J/mol-K]
c        x--composition [array of mol frac]
c      kph--phase flag:  1 = liquid
c                        2 = vapor
c        t--initial guess for temperature [K]
c        D--initial guess for molar density [mol/L]
c
c  outputs:
c        t--temperature [K]
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PSFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (p,s,x,kph,'PS',0.d0,0.d0,t,pp,D,ierr,herr)
      RETURN
      end                                              !subroutine PSFL1
c
c ======================================================================
c
      subroutine PSFL2 (p,s,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given pressure, bulk entropy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general pressure-entropy flash routine
c  PSFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than PSFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        p--pressure [kPa]
c        s--overall (bulk) molar entropy [J/mol-K]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     tbub--bubble point temperature [K] at (p,x=z)
c     tdew--dew point temperature [K] at (p,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        t--temperature [K]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PSFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (p,s,z,1,ksat,'PS',
     &                 tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                 t,pp,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine PSFL2
c
c ======================================================================
c
      subroutine PEFLSH (p,e,z,t,D,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
c
c  flash calculation given pressure, bulk energy, and bulk composition
c
c  inputs:
c        p--pressure [kPa]
c        e--overall (bulk) internal energy [J/mol]
c        z--composition [array of mol frac]
c
c  outputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful (see PBFLSH)
c     herr--error string (character*255 variable if ierr<>0)
c
c  written by E.W. Lemmon, NIST Thermophysics Division, Boulder, Colorado
c  01-18-01 EWL, original version
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PEFLSH
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax),y(ncmax),z(ncmax)
c
      call PBFLSH (p,e,z,'PE',t,D,Dl,Dv,x,y,q,ee,h,s,cv,cp,w,ierr,herr)
      RETURN
      end                                             !subroutine PEFLSH
c
c ======================================================================
c
      subroutine PEFL1 (p,e,x,kph,t,D,ierr,herr)
c
c  flash calculation given pressure, energy, and composition.  This routine
c  accepts only single-phase inputs, it is intended primarily for use with
c  the more general flash routine PEFLSH.
c
c  inputs:
c        p--pressure [kPa]
c        e--energy [J/mol]
c        x--composition [array of mol frac]
c      kph--phase flag:  1 = liquid
c                        2 = vapor
c        t--initial guess for temperature [K]
c        D--initial guess for molar density [mol/L]
c
c  outputs:
c        t--temperature [K]
c        D--molar density [mol/L]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PEFL1
c
      include 'commons.for'
      character*255 herr
      dimension x(ncmax)
c
      call ABFL1 (p,e,x,kph,'PE',0.d0,0.d0,t,pp,D,ierr,herr)
      RETURN
      end                                              !subroutine PEFL1
c
c ======================================================================
c
      subroutine PEFL2 (p,e,z,ksat,tbub,tdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,Dl,Dv,x,y,q,ierr,herr)
c
c  flash calculation given pressure, bulk energy, and composition
c
c  This routine accepts only two-phase states as input; it is intended
c  primarily for use by the general pressure-energy flash routine
c  PEFLSH.  It may be called independently if the state is known to be
c  two-phase.  But beware--this routine does not check limits, and it
c  will be significantly faster than PEFLSH only if the bubble and dew
c  point limits can be provided (ksat = 1 option).
c
c  inputs:
c        p--pressure [kPa]
c        e--overall (bulk) molar energy [J/mol]
c        z--overall (bulk) composition [array of mol frac]
c     ksat--flag for bubble and dew point limits
c           0 = dew and bubble point limits computed here
c           1 = must provide values for the following:
c     tbub--bubble point temperature [K] at (p,x=z)
c     tdew--dew point temperature [K] at (p,y=z)
c    Dlbub--liquid density [mol/L] at bubble point
c    Dvdew--vapor density [mol/L] at dew point
c     ybub--vapor composition [array of mol frac] at bubble point
c     xdew--liquid composition [array of mol frac] at dew point
c
c  outputs:
c        t--temperature [K]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c        x--composition of liquid phase [array of mol frac]
c        y--composition of vapor phase [array of mol frac]
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c     ierr--error flag:  0 = successful
c     herr--error string (character*255 variable if ierr<>0)
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PEFL2
c
      include 'commons.for'
      character*255 herr
      dimension z(ncmax),x(ncmax),y(ncmax),xdew(ncmax),ybub(ncmax)
c
      call ABFL2 (p,e,z,1,ksat,'PE',
     &                  tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,pp,Dl,Dv,x,y,q,ierr,herr)
c
      RETURN
      end                                              !subroutine PEFL2
c
c ======================================================================
c
      subroutine PBFLSH (p,b,z,ab,t,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,
     &                   ierr,herr)
c
c  flash calculation given pressure, bulk composition, and either
c  enthalpy, entropy, or energy
c
c  inputs:
c        p--pressure [kPa]
c        b--second property (energy, enthalpy, or entropy)
c        z--composition array (mol frac)
c       ab--character*2 string defining the inputs: 'PH', 'PS', or 'PE'
c
c  outputs:
c        t--temperature [K]
c        D--overall (bulk) molar density [mol/L]
c       Dl--molar density [mol/L] of the liquid phase
c       Dv--molar density [mol/L] of the vapor phase
c           if only one phase is present, Dl = Dv = D
c        x--composition [array of mol frac] for liquid phase
c        y--composition [array of mol frac] for vapor phase
c           if only one phase is present, x = y = z
c        q--vapor quality on a MOLAR basis [moles vapor/total moles]
c           q < 0 indicates subcooled (compressed) liquid
c           q = 0 indicates saturated liquid
c           q = 1 indicates saturated vapor
c           q > 1 indicates superheated vapor
c           q = -998 subcooled liquid, but quality not defined (p > Pc)
c           q = 999 indicates supercritical state (t > Tc) and (p > Pc)
c        e--overall (bulk) internal energy [J/mol]
c        h--overall (bulk) enthalpy [J/mol]
c        s--overall (bulk) entropy [J/mol-K]
c       Cv--isochoric (constant V) heat capacity [J/mol-K]
c       Cp--isobaric (constant p) heat capacity [J/mol-K]
c        w--speed of sound [m/s]
c           Cp, w are not defined for 2-phase states
c           in such cases, a flag = -9.99998d6 is returned
c     ierr--error flag:  0 = successful
c                        4 = Pin < 0
c                        8 = x out of range (< 0 or > 1)
c                       12 = x out of range and P < 0
c                      240 = CRITP did not converge
c                      241 = SATP did not converge at bubble point
c                      242 = SATP did not converge at dew point
c                      243 = SATP (bubble pt) did not converge for 2-ph
c                      244 = SATP (dew pt) did not converge for 2-phase
c                      245 = TPFL2 did not converge
c                      246 = 2-phase iteration did not converge
c                      247 = TPRHO did not converge for single-phase
c                      248 = single-phase iteration did not converge
c                      249 = H out of range
c                      250 = S out of range
c     herr--error string (character*255 variable if ierr<>0)
c
c  rewritten by E.W. Lemmon, NIST Physical & Chemical Properties Div, Boulder, CO
c  01-18-01 EWL, original version, rewritten by combining code of McLinden
c                from PHFLSH and PSFLSH routines to make one unified routine
c  05-16-01 EWL, add checks for h and s in the single phase
c  11-07-02 EWL, check for bmin>bmax
c  12-16-02 EWL, add checks for pseudo-pure fluid, calculate t using q
c  02-12-09 EWL, add check for solid phase for pure fluid when t<tmin (below sublimation line)
c  02-17-09 EWL, add check between ttrp and tmelt
c  04-28-10 EWL, remove call to SATT with tbub for pure fluid
c  04-29-10 EWL, check for dldew=0 in call to SATP that occurs when p<p_triple
c  05-17-11 EWL, add check for ierr2>0 on first call to TPRHO
c  08-15-11 EWL, add check for b<bmin for water mixtures
c  11-21-12 EWL, specify better values of q after call to ABFL1
c  11-26-12 EWL, if point is vapor phase, skip calculation of bubble point
c
cx DEC$ ATTRIBUTES DLLEXPORT :: PBFLSH
c
      include 'commons.for'
      character*1 bt
      character*2 ab
      character*255 herr,herr1,herr2
      dimension x(ncmax),y(ncmax),z(ncmax),
     &          xdew(ncmax),ydew(ncmax),xbub(ncmax),ybub(ncmax)
c
      bt=ab(2:2)
      call ISPURE (z,icomp)
      ierr=0
      herr=' '
      D=0.d0
      Dl=0.d0
      Dv=0.d0
c
c  set output liquid and vapor compositions to input values
c  zero output values for undefined components
      if (icomp.eq.0) then
        do i=1,nc
          x(i)=z(i)
          y(i)=z(i)
        enddo
      else
        do i=1,nc
          x(i)=0.d0
          y(i)=0.d0
        enddo
        x(icomp)=1.d0
        y(icomp)=1.d0
      endif
c
      call CRITP (z,tc,pc,Dc,ierr,herr1)
      t=tc
      if (ierr.gt.0) then
        ierr=240
        write (herr,1002) ab,herr1(1:234),hnull
 1002   format ('[',a2,'FLSH error 240] ',a234,a1)
        call ERRMSG (ierr,herr)
        q=999.d0     !quality undefined
        RETURN
      end if
c
c  check that input conditions are within limits
c
      tdum=-1.0d0
      Ddum=0.0d0
      call LIMITX ('EOS',tdum,Ddum,p,z,tmin,tmax,Dmax,pmax,ierr,herr1)
c  calculate approx limits:
c  lower limit at (tmin,pmax); upper limit at (1.5tmax,rho = 0)
c  !MM, 10-16-97, because of curvature of isotherms, h at pmax is not
c  the minimum, use a lower pressure instead

      p0=100.0d0 !kPa
 10   continue
      call TPRHO (tmin,p0,z,1,0,D0,ierr2,herr2)
      if (ierr2.gt.0) then
        tmin=tmin+5
        goto 10
      endif
      if (bt.eq.'H') call ENTHAL (tmin,D0,z,bmin)
      if (bt.eq.'E') call ENERGY (tmin,D0,z,bmin)
      if (bt.eq.'S') call ENTRO  (tmin,D0,z,bmin)
      if (bt.eq.'S' .and. icomp.ne.0) then
c  check in small region below the triple point temperature and above the melting line
        tmlt=0.d0
        if (hcas(icomp).eq.'7732-18-5') tmlt=251.18d0 !water
        if (hcas(icomp).eq.'124-38-9' ) tmlt=244.8d0  !co2
        if (hcas(icomp).eq.'7664-41-7') tmlt=200.2d0  !ammonia
        if (hcas(icomp).eq.'7440-59-7') tmlt=2.178d0  !helium
        if (tmlt.gt.0.d0) then
          call MELTT (tmlt,z,pmlt,ierr2,herr2)
          if (ierr2.eq.0) then
            call TPRHO (tmlt,pmlt,z,1,0,D0,ierr2,herr2)
            if (ierr2.eq.0) then
              call ENTRO (tmlt,D0,z,bmin2)
              if (abs(bmin2).gt.1.d-20 .and. bmin2.lt.bmin) bmin=bmin2
            endif
          endif
        endif
      endif
      tmax=1.5d0*tmax
      rho0=1.0d-6
      if (bt.eq.'H') call ENTHAL (tmax,rho0,z,bmax)
      if (bt.eq.'S') call ENTRO  (tmax,rho0,z,bmax)
      if (bt.eq.'E') call ENERGY (tmax,rho0,z,bmax)
      if (bmin.gt.bmax) then
c  just in case tmin was so low that the EOS did weird things, raise tmin:
        tmin=tmin+20
        call TPRHO (tmin,p0,z,1,0,D0,ierr2,herr2)
        if (bt.eq.'H') call ENTHAL (tmin,D0,z,bmin)
        if (bt.eq.'S') call ENTRO  (tmin,D0,z,bmin)
        if (bt.eq.'E') call ENERGY (tmin,D0,z,bmin)
      endif
c  for mixtures with water, the H/E/S values can loop back on itself on a T-b
c  diagram, thus the minimum b will not be at the low T, but slightly higher.
c  this will not return the exact point since there will be even lower values
c  of b at high pressures
      if (b.lt.bmin) then
        tminw=tmin
 20     continue
        tminw=tminw+1.d0
        call TPRHO (tminw,p0,z,1,1,D0,ierr2,herr2)
        if (bt.eq.'H') call ENTHAL (tminw,D0,z,bminw)
        if (bt.eq.'S') call ENTRO  (tminw,D0,z,bminw)
        if (bt.eq.'E') call ENERGY (tminw,D0,z,bminw)
        if (bminw.lt.bmin .and. ierr2.eq.0) then
          bmin=bminw
          goto 20
        endif
        if (abs(tminw-tmin).gt.1.1d0) bmin=bmin-2.d0*(bminw-bmin)   !Decrease slightly to include saturated state points missed by using an increment of 1 K.
      endif

c     write (*,1004) tmin,tmax,bmin,bmax
c1004 format (1x,' ABFLSH--limits on t,b:  ',2f8.2,2f12.2)
c  if inputs are outside limits set outputs equal to critical
c  point values and return
      if (ierr.lt.-1) then     !ignore ierr = -1 (t out of range)
c  one or inputs are outside limits--if just a warning proceed w/ calc
        write (herr,1006) ab,ierr,herr1(1:233),hnull
 1006   format ('[',a2,'FLSH warning',i4,'] ',a233,a1)
        call ERRMSG (ierr,herr)
      else
        if (ierr.gt.1) then
c  ignore ierr=1 out of LIMITX (t out of range, but t not an input)
          write (herr,1007) ab,ierr,herr1(1:235),hnull
 1007     format ('[',a2,'FLSH error',i4,'] ',a235,a1)
        else if (b.lt.bmin .or. b.gt.bmax) then
          ierr=249
          write (herr,1008) ab,ierr,bt,b,bmin,bmax,hnull
 1008     format ('[',a2,'FLSH error',i4,'] Input value is ',
     &            'outside limits; ',a1,' =',g12.5,', min,max =',
     &            2(g12.5),a1)
        end if
        if (ierr.gt.1) then
          call ERRMSG (ierr,herr)
          t=tc
          D=Dc
          Dl=Dc
          Dv=Dc
          call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
          q=999.d0     !quality undefined
          RETURN
        end if
      end if
c
      iflag1=0
      if (p.ge.pc .or. p.lt.1.0d-12) then
        iflag1=1     !Single phase
      else
c  if the input enthalpy is greater than h at the critical temperature and zero
c  density, the inputs are single phase.
        if (bt.eq.'H') then
          call enthal(tc,0.d0,x,htc0)
          if (b.gt.htc0) iflag1=1
        endif
c  if the input entropy is greater than S at the critical temperature and given
c  pressure, the inputs are single phase.
        if (bt.eq.'S') then
          call TPRHO (tc,p,x,2,0,dtcp,ierr,herr)
          call entro(tc,dtcp,x,stcp)
          if (b.gt.stcp .and. ierr.eq.0) iflag1=1
        endif
      endif
      if (iflag1.eq.1) then
c  super-critical state or special case for p = 0 (x = y = z as set above)
        kph=2                          !use vapor iteration
        t=tc+10                        !initial guesses
        if (p.ge.pc) then
          D=Dc*2.d0
        else
          D=0.0d0
        end if
        call ENTHAL (tc,dc,z,h)
        call ENERGY (tc,dc,z,e)
        if (bt.eq.'H' .and. b.lt.h/2) t=tc*.8d0
        if (bt.eq.'E' .and. b.lt.e/2) t=tc*.8d0
        call ABFL1 (p,b,z,kph,ab,0.d0,0.d0,t,pp,D,ierr,herr)
        if (ierr.ne.0) then
c  single-phase iteration did not converge--error message written by ABFL1
c  try again with different initial input
          t=tc*1.2d0
          call ABFL1 (p,b,z,kph,ab,0.d0,0.d0,t,pp,D,ierr,herr)
          if (ierr.ne.0) then
            t=tc
            call ABFL1 (p,b,z,kph,ab,0.d0,0.d0,t,pp,D,ierr,herr)
            if (ierr.ne.0) then
              q=999.d0     !quality undefined
              RETURN
            end if
          endif
        end if
        call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
        Dl=D
        Dv=D
        if (p.gt.pc .and. t.gt.tc) then
          q=999.d0     !quality undefined
        elseif (p.gt.pc) then
          q=-998.d0    !subcooled
        elseif (t.gt.tc) then
          q=998.d0     !superheated
        elseif (d.gt.Dc) then
          q=-998.d0    !subcooled
        elseif (d.lt.Dc) then
          q=998.d0     !superheated
        end if
      else
c  sub-critical state--call saturation routine to determine phase
        call SATP (p,z,2,tdew,Dldew,Dvdew,xdew,ydew,ierr,herr1)
        if (dldew.le.0.d0 .and. tdew.lt.tmin .and. bt.eq.'H') then
          call ENTHAL (tdew,Dvdew,z,hdew)
          if (b.lt.hdew) then
            ierr=250
            write (herr,2009) ab,ierr,hnull
            call ERRMSG (ierr,herr)
            q=999.d0     !quality undefined
            RETURN
          endif
        endif
        if (dldew.le.0.d0) Dldew=Dvdew
        if (ierr.eq.2) then     !p<ptrp
          tdew=tmin
          call SATT (tdew,z,2,pp,Dldew,Dvdew,xdew,ydew,ierr,herr1)
        elseif (ierr.ne.0) then
c  if the dew point iteration fails, make a check to see if the point is
c  in the liquid phase below the liquid saturation state.
          call SATP (p,z,1,tbub,Dlbub,Dvbub,xbub,ybub,ierr2,herr2)
          if (ierr2.eq.0) then
            if (bt.eq.'H') then
              call ENTHAL (tbub,Dlbub,z,blbub)
            elseif (bt.eq.'S') then
              call ENTRO (tbub,Dlbub,z,blbub)
            elseif (bt.eq.'E') then
              call ENERGY (tbub,Dlbub,z,blbub)
            endif
            if (b.le.blbub) then
              kph=1
              t=tbub
              D=Dlbub
              call ABFL1 (p,b,z,kph,ab,0.d0,0.d0,t,pp,D,ierr2,herr2)
              call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
              Dl=D
              Dv=D
              if (ierr2.eq.0) goto 100
            endif
          endif
          ierr=242
          write (herr,1242) ab,herr1(1:192),hnull
          call ERRMSG (ierr,herr)
 1242     format ('[',a2,'FLSH error 242] dew point calculation ',
     &            'did not converge:  ',a192,a1)
          q=999.d0     !quality undefined
          RETURN
        end if
c  calculate b at the dew point
        if (bt.eq.'H') then
          call ENTHAL (tdew,Dvdew,z,bvdew)
        elseif (bt.eq.'S') then
          call ENTRO (tdew,Dvdew,z,bvdew)
        elseif (bt.eq.'E') then
          call ENERGY (tdew,Dvdew,z,bvdew)
        endif
        q=998
        if (b.lt.bvdew) then  !Don't need to do the following if state is single phase vapor
          if (icomp.ne.0 .and. ianc(icomp).eq.0) then
c  if pure, bubble point density provided by call to SATT at dew point
            Dlbub=Dldew
            tbub=tdew
          else
c  if mixture, call saturation routine again at bubble point
            call SATP (p,z,1,tbub,Dlbub,Dvbub,xbub,ybub,ierr,herr1)
            if (ierr.ne.0 .and. p.gt.pc*0.9d0) then
              tbub=tc
              Dlbub=dc
            elseif (ierr.ne.0) then
              ierr=241
              write (herr,1241) ab,herr1(1:188),hnull
 1241         format ('[',a2,'FLSH error 241] bubble point calculation',
     &                ' did not converge:  ',a188,a1)
              call ERRMSG (ierr,herr)
              RETURN
            end if
          end if
c  calculate b at the bubble point
          if (bt.eq.'H') then
            call ENTHAL (tbub,Dlbub,z,blbub)
          elseif (bt.eq.'S') then
            call ENTRO (tbub,Dlbub,z,blbub)
          elseif (bt.eq.'E') then
            call ENERGY (tbub,Dlbub,z,blbub)
          endif
          if (abs(bvdew-blbub).gt.1.d-20) q=(b-blbub)/(bvdew-blbub)
        endif
        if (b.le.blbub .or. b.ge.bvdew) then
c  single-phase (liq or vapor) (pure or mixture) (x = y = z as set above)
c  set initial guesses, depending on phase
          if (b.le.blbub) then
            kph=1                        !liquid
            t=tbub                       !initial guesses
            D=Dlbub
          else
            kph=2                        !vapor
            t=tdew                       !initial guesses
            D=Dvdew
          end if
          call ABFL1 (p,b,z,kph,ab,0.d0,0.d0,t,pp,D,ierr,herr)
          if (ierr.ne.0) then
c  single-phase iteration did not converge--error message written by ABFL1
            call ERRMSG (ierr,herr)
            q=999.d0     !quality undefined
            RETURN
          end if
          call THERM (t,D,z,ptherm,e,h,s,cv,cp,w,hjt)
          Dl=D
          Dv=D
        else
          if (icomp.ne.0) then
c  special case:  pure-fluid two-phase (x = y = z as set above)
            t=(1.d0-q)*tbub+q*tdew
            Dl=Dlbub
            Dv=Dvdew
            if (ianc(icomp).eq.1) call PTANC (t,p,q,b,bt,Dl,Dv)
            if (t.lt.tmin) then       !Sublimation line exists
              ierr=250
              write (herr,2009) ab,ierr,hnull
 2009         format ('[',a2,'FLSH error',i4,']; input state is',
     &                ' solid or two-phase solid-vapor',a1)
              call ERRMSG (ierr,herr)
              q=999.d0     !quality undefined
              RETURN
            endif
          else
c  general case:  mixture 2-phase
            ksat=1   !bubble and dew point data provided to ABFL2
            call ABFL2 (p,b,z,1,ksat,ab,
     &                  tbub,tdew,pbub,pdew,Dlbub,Dvdew,ybub,xdew,
     &                  t,pp,Dl,Dv,x,y,q,ierr,herr)
            if (ierr.ne.0) then
c  two-phase iteration did not converge--error message written by ABFL2
              call ERRMSG (ierr,herr)
              q=999.d0     !quality undefined
              RETURN
            end if
c           write (*,1225) t,q,(x(i),i=1,3),(y(i),i=1,3)
c1225       format (1x,' PBFLSH--ABFL2 return   t,q,x,y = ',8f12.7)
          end if
c  compute remaining properties for 2-phase states
          call THERM (t,Dl,x,ptherm,el,hl,sl,cvl,cp,w,hjt)
          call THERM (t,Dv,y,ptherm,ev,hv,sv,cvv,cp,w,hjt)
          alpha=1.0d0-q
          D=1.0d0/(alpha/Dl+q/Dv)
          e=alpha*el+q*ev
          h=alpha*hl+q*hv
          s=alpha*sl+q*sv
          w=xnotd       !Cp,w not defined for 2-phase states
          cp=xnotd
          cv=xnotd
        end if
      end if
c
c  check that computed output is within bounds of EOS
 100  continue
      call LIMITX ('EOS',t,d,p,z,tmin,tmax,Dmax,pmax,ierr,herr1)
      if (ierr.ne.0) then
        write (herr,2008) ab,ierr,herr1
 2008   format ('[',a2,'FLSH error',i3,']; ',a235)
        call ERRMSG (ierr,herr)
      end if
c
      RETURN
      end                                             !subroutine PBFLSH
c
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c ======================================================================
c                                                    end file flsh_sub.f
c ======================================================================
