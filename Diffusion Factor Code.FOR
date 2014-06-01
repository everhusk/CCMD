            !Diffuser Outlet Conditions
            !H04 = H03
            !Cu4 = Cu3*r3/r4
            !Wu4 = Wu3*r3/r4
            !Cr4 = Cr3 
            
            !Possible Diffuser Loss Correlations (Alternative method)
            !!DiffusionFactor = -4.8597*(phi**3) + 6.782*(phi**2) - 3.2895*phi + 0.7948 !Johnston & Dean (1966)
            !!DiffusionFactor = 0.0119*Alpha3 - 0.7035 !Yingkang & Sjolander Rotor A
            !!DiffusionFactor = 0.0004*Alpha3**2 - 0.046*Alpha3 + 1.6704 !Yingkang & Sjolander Rotor B
            !!DiffusionFactor = 0.0088*Alpha3 - 0.4924 !Yingkang & Sjolander Rotor C
            !!DiffusionFactor = 0.0148*Alpha3 - 0.8952 !Yingkang & Sjolander Rotor D
            !!DiffusionFactor = 0.0171*Alpha3 - 1.0389 !Yingkang & Sjolander Rotor E
            !!DiffusionFactor = 0.0002*(Alpha3**2) - 0.0242*Alpha3 + 0.6283 +1 

!            P04 = P03 - DiffuserLossFactor*(P03-P3)!kPa
!            CALL PHFLSH (P04,H04,zz,T04,rho04,Dl,Dv,x,y,q,e,S04,cv,cp,w,ierr,herr)
!
!            last=0
!            i=0
!            
!            DO WHILE (ABS((Cr4 - last)/Cr4) .GT. 0.000000001)
!                i=i+1
!                last=Cr4
!                C4=SQRT(Cr4**2+Cu4**2)
!                !Stagnation is an isentropic process
!                H4=H04-(C4**2)*wm/2000!J/mol
!                S4=S04
!                CALL HSFLSH (H4,S4,zz,T4,P4,rho4,Dl,Dv,x,y,q,e,cv,cp,a_4,ierr,herr)
!                rho4 = rho4*wm!kg/m^3
!                Cr4 = zMdot/(rho4*(2*PI*r4*b4))!Update Cm4
!                IF (i.GT.MaxIter) THEN
!                    WRITE(*,*) "Max iteration reached at Diffuser Outlet"
!                ELSE IF (ierr.NE.0) THEN
!                    WRITE(*,*) "Error Flag at Diffuser Outlet: ",ierr
!                    WRITE(*,*) "Error String: ",herr      
!                END IF
!            END DO