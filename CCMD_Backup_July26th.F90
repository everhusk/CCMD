! Last Update: June 27th, 2013
PROGRAM MEANLINE

!VARIABLE DECLARTIONS
IMPLICIT DOUBLE PRECISION (a-h,l,o-z)
IMPLICIT INTEGER (i-k,m,n)
PARAMETER (ncmax=20)   !max number of components in mixture
DIMENSION x(ncmax),xliq(ncmax),xvap(ncmax),f(ncmax)
CHARACTER hrf*3, herr*255, OutFile*30, a*54
CHARACTER*255 hf(ncmax)
CHARACTER*7 hfmix    

COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDiffAnalysis, iVoluteAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1, FlowCoefficient
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m, isolution,iDiffSteps
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2m, Beta2guess
COMMON / Losses         / LossesInternal, LossesParasitic, cf, Sigma, ShockLoss, LossIncidence, &
                          DiffusionLoss, ChokingLoss, BladeLoadingLoss, HSLoadingLoss, &
                          SkinFrictionLoss, TipClearanceLoss, LossMixing, SupercritMachNumberLoss

!INITIALIZE VARIABLES
MaxIter = 500
PI = 3.1415926535898
                                        
                                  
WRITE(*,*)"                    CENTRIFUGAL COMPRESSOR MEANLINE DESIGN"
WRITE(*,*)"               "
WRITE(*,*)"                                  CC"
WRITE(*,*)"                            CC CCC"
WRITE(*,*)"                       C CCCCCCCC"
WRITE(*,*)"                      CCCCCCCCCC           DDDD"
WRITE(*,*)"                     CCCCCCCCCC             DDDDDDD"
WRITE(*,*)"                    CCCCCCCCCC       MMM     DDDDDDDD"
WRITE(*,*)"                    CCCCCCCCC      MMMMMMM   DDDDDDDD"
WRITE(*,*)"                    CCCCCCCCC    MMMMMMMMMMM  DDDDDDDDD"
WRITE(*,*)"                    CCCCCCCCC   MMMMMMMMMMMMM  DDDDDDDDD"
WRITE(*,*)"                    CCCCCCCCC   MMMMMMMMMMMMM   DDDDDDDDD"
WRITE(*,*)"                     CCCCCCCCC  MMMMMMMMMMMMM   DDDDDDDDD"
WRITE(*,*)"                      CCCCCCCCC  MMMMMMMMMMM    DDDDDDDDD"
WRITE(*,*)"                       CCCCCCCCC   MMMMMMM     DDDDDDDDDD"
WRITE(*,*)"                        CCCCCCCCC    MMM       DDDDDDDDDD"
WRITE(*,*)"                          CCCCCCCC            DDDDDDDDDDD"
WRITE(*,*)"                              CCCCC          DDDDDDDDDDD"
WRITE(*,*)"                                            DDDDDDDD  D"
WRITE(*,*)"                                           DDD  DD   "
WRITE(*,*)"                                          DD "
WRITE(*,*)"                                    "
WRITE(*,*)"                    CARLETON UNIVERSITY GAS TURBINE PROJECT"
WRITE(*,*)"                         ...Press Enter To Begin..."
READ(*,*)
!MUST Be Named CompressorDimensions With .txt Extension
OPEN(UNIT=20,FILE='CompressorDimensions.txt',STATUS='OLD')

DO iread=1,48
READ(20,*)
END DO

!Read File Formatting (A = Character String, Field Width = 54)
12 FORMAT (A54,I8)    !I = Integer, Field Width = 8
13 FORMAT (A54,F14.7) !F = Real, Field Width = 14, and 7 Decimal Places9
14 FORMAT (A54,I3)    !I = Integer, Field Width = 2

!Read Rotor Dimensions From Text File
READ(20,14)  a,iAir
READ(20,14)  a,iCarbonDioxide
READ(20,*)
READ(20,*)
READ(20,*)
READ(20,13)  a,Target_PR
READ(20,12)  a,N
READ(20,13)  a,zMdot
READ(20,13)  a,T01
READ(20,13)  a,P01
READ(20,*)
READ(20,13)  a,r2
!READ(20,13)  a,LbImpeller
READ(20,13)  a,LbSplitter
!READ(20,13)  a,Lb
READ(20,13)  a,tb1!Inlet Blade Thickness
READ(20,13)  a,tb!Outlet Blade Thickness
READ(20,13)  a,RoughnessImp
READ(20,13)  a,TipClearance
READ(20,13)  a,Alpha1
READ(20,13)  a,CurveKm1
READ(20,*)
READ(20,14)  a,iDiffAnalysis
READ(20,13)  a,r4
READ(20,13)  a,b4
READ(20,14)  a,iVoluteAnalysis
READ(20,13)  a,VoluteLossFactor
READ(20,13)  a,r6
READ(20,13)  a,b6
READ(20,*)
READ(20,*)
READ(20,*)
READ(20,*)
READ(20,14)  a,iStep1!Check if 0
READ(20,*)
READ(20,13)  a,r1h
READ(20,13)  a,r1s
READ(20,13)  a,b2
READ(20,*)
READ(20,14)  a,iStep2!Check if 0
READ(20,*)
READ(20,14)  a,iFull
READ(20,14)  a,iSplitter
READ(20,*)
READ(20,14)  a,iTPcond!Check if 0
READ(20,*)   
READ(20,14)  a,iSteps
READ(20,14)  a,iDiffSteps
CLOSE(20)
!For Debugging The Inputs:

!WRITE(*,*)  iAir
!WRITE(*,*)  iCarbonDioxide
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  Target_PR
!WRITE(*,*)  N
!WRITE(*,*)  zMdot
!WRITE(*,*)  T01
!WRITE(*,*)  P01
!WRITE(*,*)
!WRITE(*,*)  r2
!WRITE(*,*)  LbImpeller
!WRITE(*,*)  LbSplitter
!WRITE(*,*)  Lb
!WRITE(*,*)  tb1!Inlet Blade Thickness
!WRITE(*,*)  tb!Outlet Blade Thickness
!WRITE(*,*)  RoughnessImp
!WRITE(*,*)  TipClearance
!WRITE(*,*)  Alpha1
!WRITE(*,*)
!WRITE(*,*)  iDVAnalysis
!WRITE(*,*)  DiffuserLossFactor
!WRITE(*,*)  r4
!WRITE(*,*)  b4
!WRITE(*,*)  VoluteLossFactor
!WRITE(*,*)  r6
!WRITE(*,*)  b6
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  r1h!Check if 0
!WRITE(*,*)  r1h_min
!WRITE(*,*)  r1h_max
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  r1s!Check if 0
!WRITE(*,*)  r1s_min
!WRITE(*,*)  r1s_max
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  iFull!Check if 0
!WRITE(*,*)  iFull_Min
!WRITE(*,*)  iFull_Max
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  iSplitter
!WRITE(*,*)  iSplitter_Min
!WRITE(*,*)  iSplitter_Max
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  DeltaZ!Check if 0
!WRITE(*,*)  DeltaZ_Min
!WRITE(*,*)  DeltaZ_Max
!WRITE(*,*)
!WRITE(*,*)
!WRITE(*,*)  b2!Check if 0
!WRITE(*,*)  b2_Min
!WRITE(*,*)  b2_Max
!WRITE(*,*)   
!WRITE(*,*)  isteps
!READ(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*) 'Input Design Flow Coefficient:'
READ (*,*) FlowCoefficient
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*) 'Please Enter Output File Name (i.e. ConceptOne.txt):'
READ (*,*) Outfile
OPEN(UNIT=10,FILE=OutFile,STATUS='NEW')
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*)
WRITE(*,*) "Calculating Compressor Performance..."
m = 0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                FULL ROTOR PERFORMANCE ANALYSIS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (iStep1 == 0 .AND. iStep2 == 0 .AND. iTPCond == 0) THEN
    
    iPerformance = 0
    
    WRITE(10,*) '                   Carleton University'
    WRITE(10,*) '    Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '          COMPRESSOR PERFORMANCE ANALYSIS RESULTS'
    WRITE(10,*) '                   Gas Turbine Project'
    WRITE(10,*) 
    
    CALL PERFORMANCE()
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    
END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                   INLET OPTIMIZATION CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (iStep1 == 1 .AND. iStep2 == 0 .AND. iTPCond == 0) THEN
    
    r1h_min = r2*0.05
    r1h_max = r2*0.5
    r1s_min = r1h_min*1.1
    r1s_max = r2*0.7
    b2_min = (r1s_min - r1h_min)*0.5
    b2_max = (r1s_max - r1h_min)*0.5
    step3 = (b2_max - b2_min)/isteps
    r1h = r1h_min
    
    m3 = 0
    isolution = 0
    
    iPerformance = 1
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                       INLET OPTIMIZATION DATA'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) 'r1h       r1s       b2        M1_tip    M2_rel    dH_m      Flow Co.  Work Co.   Efficiency'
    
    DO WHILE (b2 <= b2_max)
        
        
        b2 = b2_min + step3*m3
        m3 = m3 + 1
        m2 = 0
        
        DO WHILE (r1s <= r1s_max)
            
            r1s_min = r1h * 1.1
            step2 = ((r1s_max-r1s_min)/isteps)
            r1s = r1s_min + step2*m2
            m2 = m2 + 1
            m = 0
            
            DO WHILE (r1h <= r1h_max)
                
                r1h_max = r1s * 0.9
                step = ((r1h_max-r1h_min)/isteps)
                r1h = r1h_min + step*m
                m = m + 1
                isolution = isolution + 1
                
               CALL PERFORMANCE()

            END DO  
            r1h = r1h_min
            r1h_max = r2*0.5
        END DO
        r1s = r1s_min
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    r1h=0
    
END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                  NUMBER OF FULL BLADES CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (iStep1 == 0 .AND. iStep2 == 1 .AND. iTPCond == 0) THEN
    
    iPerformance = 2
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                    NUMBER OF FULL BLADES ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) '#Blades     M1_tip     M2_rel    dH_tip     dH_hub     dH_m    Flow Co.  Work Co.  Efficiency  Beta2'
    
    iFull = 5
    iFull_Max = 35

    DO WHILE (iFull <= iFull_max)
         
         CALL PERFORMANCE()
         iFull = iFull + 1
         
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    iFull=0
    
END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                 INLET CONDITIONS CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (iStep1 == 0 .AND. iStep2 == 0 .AND. iTPCond == 1) THEN
    
    iPerformance = 3
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                      OUTLET PASSAGE WIDTH ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) ' T01   P01    M2_rel      dH_tip     dH_hub     dH_m     Flow Coeff.    Work Co.    Efficiency'
    
    Tc = 304.25
    Pc = 7380.0
    T01max = T01
    P01max = P01
    
    !Step Sizes
    PStep = (P01 - Pc)/isteps
    TStep = (T01 - Tc)/isteps
        
    inewsteps = isteps + 5
    
    DO m=0,inewsteps
        
        T01 = T01max - m*TStep
        P01 = P01max - m*PStep
        CALL PERFORMANCE()
    
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    
END IF

END PROGRAM MEANLINE

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                     SUBROUTINE: Meanline Performance Analysis
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE PERFORMANCE()

IMPLICIT DOUBLE PRECISION (a-h,l,o-z)
IMPLICIT INTEGER (i-k,m,n)
PARAMETER (ncmax=20)   !max number of components in mixture
DIMENSION x(ncmax),xliq(ncmax),xvap(ncmax),f(ncmax)
CHARACTER hrf*3, herr*255, OutFile*30, a*54
CHARACTER*255 hf(ncmax)
CHARACTER*7 hfmix   

COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDiffAnalysis, iVoluteAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1, FlowCoefficient
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m, isolution,iDiffSteps
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2m, Beta2guess
COMMON / Losses         / LossesInternal, LossesParasitic, cf, Sigma, ShockLoss, LossIncidence, &
                          DiffusionLoss, ChokingLoss, BladeLoadingLoss, HSLoadingLoss, &
                          SkinFrictionLoss, TipClearanceLoss, LossMixing, SupercritMachNumberLoss

!Re-Initialize for unknown black magic reasons
r1m = 0
A1 = 0
A2 = 0
Z = 0
U1 = 0
U1s = 0
U1h = 0
U2 = 0
C1 = 0
C1s = 0
C1h = 0
C2 = 0
Cu1 = 0
Cu2 = 0
Cm1 = 0
Cm1s = 0
Cm1h = 0
Cr2 = 0
W1 = 0
W1s = 0
W1h = 0
W2 = 0
Wu2 = 0
Visc1 = 0
Visc2 = 0
rho01 = 0
rho02 = 0
rho1 = 0
rho2 = 0
P1 = 0
P02 = 0
P2 = 0
T1 = 0
T2 = 0
Beta1prime = 0
Alpha2 = 0
Beta1 = 0
Beta2 = 0
Beta2guess = 0
kbeta = 0

!SETUP & CALL REFPROP for CO2 Properties
IF (iAir == 1) THEN
    i=1
    hf(1)='AIR.ppf'
    hfmix='hmx.bnc'
    hrf='DEF'
    CALL SETUP (i,hf,hfmix,hrf,ierr,herr)
    IF (ierr.ne.0) WRITE (*,*) herr
    CALL INFO (1,wm,ttp,tnbp,tc,pc,dc,zc,acf,dip,rgas)
END IF

IF (iCarbonDioxide == 1) THEN
  i=1
  hf(1)='CO2.fld'
  hfmix='hmx.bnc'
  hrf='DEF'
  CALL SETUP (i,hf,hfmix,hrf,ierr,herr)
  if (ierr.ne.0) write (*,*) herr
  CALL INFO (1,wm,ttp,tnbp,tc,pc,dc,zc,acf,dip,rgas)
END IF

!VARY ROTOR DIMENSIONS FOR OPTIMIZATION

    !ROTOR INLET DIMENSIONS
    D1h = r1h*2!m
    D1s = r1s*2!m
    b1 = r1s - r1h
    A1=(PI*(r1s**2)-PI*(r1h**2))!m^2 / Rotor inlet area with blockage
    r1m=(0.5*(r1s**2)+0.5*(r1h**2))**0.5!Inlet Meanline Radius (m)
    D1=r1m*2

    !ROTOR OUTLET DIMENSIONS
    D2 = r2*2!Impeller Outlet Tip Diameter (m)
    A2=(2*PI*r2-(iFull+iSplitter)*tb)*b2!m^2 / Rotor outlet area with blockage

    !DESIGN POINT
    
    CALL TPFLSH (T01,P01,x,d,dl,dv,xliq,xvap,q,e,H01,S01,cv,cp,w,ierr,herr)
    R = rgas*1000/wm
    rho01=d*wm!kg/m^3
    U1=r1m*2*PI*N/60!Blade Speed (m/s)
    U2=r2*2*PI*N/60!Blade Speed (m/s)
    PRr=Target_PR+0.2!Rotor Pressure Ratio Guess
    Effr=0.9!Rotor Isentropic Efficiency Guess
    DeltaZ = D2*(0.014 + 0.023*D2/D1h + 1.58*FlowCoefficient) !Empirical estimate from Aungier (2000 pg 113)
      
    !ROTOR INLET
    
            !ROTOR INLET VELOCITY TRIANGLE
            !First iteration using stagnation conditions
            C1=zMdot/(rho01*A1*COS(Alpha1*PI/180))
            last=0
            j=0

            DO WHILE (ABS((C1 - last)/C1) .GT. 0.0000001)
                j=j+1
                last=C1
                !Stagnation is an isentropic process
                H1=H01-(C1**2)*wm/2000!J/mol
                S1=S01
                CALL HSFLSH (H1,S1,zz,T1,P1,rho1,Dl,Dv,x1,y,q,e,cv,cp,a_1,ierr,herr)
                rho1=rho1*wm!kg/m^3
                Angle = Alpha1*PI/180
                C1=zMdot/(rho1*A1*COS(Angle))!Update C1
                IF (j.GT.MaxIter) GOTO 1337
                IF (ierr.NE.0) THEN!GOTO 1337
                    C1 = 28
                    WRITE(*,*) "Error Flag at Rotor Inlet: ",ierr
                    WRITE(*,*) "Error String: ",herr      
                END IF
            END DO
            
            CALL TRNPRP (T1,rho1/wm,x1,eta,tcx,ierr,herr)
            IF (ierr.NE.0) THEN
                WRITE(*,*) "Viscosity Error Flag at Rotor Inlet: ",ierr
                WRITE(*,*) "Error String: ",herr
            END IF
            
            Visc1=eta/1000000!kg/m*s
            
            !CONVERGED ROTOR INLET VELOCITY TRIANGLES
            
            !Meanline
            Cm1 = C1 * COS(Alpha1*PI/180)
            Cu1 = C1 * SIN(Alpha1*PI/180)
            Wu1 = Cu1 - U1
            W1 = SQRT(Wu1**2+Cm1**2)
            Beta1 = ATAN(Wu1/Cm1)*(180/PI)
            U1m = r1m*2*PI*N/60
            zM1rel = W1/a_1
            
            !Shroud
            Cm1s = Cm1*(1-CurveKm1*b1/2)!Aungier(2000)
            U1s = r1s*2*PI*N/60
            Cu1s = Cu1 !Assume Cu1 = Cu1s
            Wu1s = Cu1s - U1s
            W1s = SQRT(Cm1s**2+Wu1s**2)
            C1s = SQRT(Cm1s**2+Cu1s**2)
            Beta1s = ATAN(Wu1s/Cm1s)*(180/PI)
            zM1rels = W1s/a_1
                        
            !Hub
            Cm1h = Cm1*(1+CurveKm1*b1/2)!Aungier(2000)
            U1h = r1h*2*PI*N/60
            Cu1h = Cu1 !Assume Cu1 = Cu1h
            Wu1h = Cu1h - U1h 
            W1h = SQRT(Cm1h**2+Wu1h**2)    
            C1h = SQRT(Cm1h**2+Cu1h**2)
            Beta1h = ATAN(Wu1h/Cm1h)*(180/PI)            
            zM1relh = W1h/a_1            
            
        !        IF(zM1rel.GT.1) THEN
        !            WRITE(*,*) "Inlet is choked at meridional."
        !            READ(*,*)
        !        ELSE IF(zM1rels.GT.1) THEN
        !            WRITE(*,*) "Inlet is choked at tip."
        !            READ(*,*)
        !        ELSE IF(zM1relh.GT.1) THEN
        !            WRITE(*,*) "Inlet is choked at hub."
        !            READ(*,*)    
        !        END IF

    PRrLast =0
    k=0

    DO WHILE (ABS((PRr - PRrLast)/PRr) .GT. 0.00000001)
        k=k+1
        PRrLast = PRr
        
        IF (k.GT.MaxIter) GOTO 1337

        !~~~~~~~~~~~~~~~~~~~~~~~~~~IMPELLER ANALYIS~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !The impeller analysis uses basic turbomachinery principles and
        !an optimum set of loss models (Oh et al.) to correct for real effects
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        kbeta = 0
        Beta2last = 100
        
        DO WHILE (ABS((Beta2 - Beta2last)/Beta2) .GT. 0.0000001)

            kbeta=kbeta+1
            Beta2last = Beta2
            
            IF (kbeta.GT.MaxIter) GOTO 1337
            
            !Estimate of mean camber line length (Currently an input)
            IF (ABS(Beta2) > 80 .AND.  ABS(Beta2) <= 90) THEN!Prevents divergence
              Beta2 = 40
            END IF
        
            Lb =  (DeltaZ - b2/2) + (D2 - D1)/(2*COS(Beta2*PI/180))!Sjolander
            Z = iFull + (LbSplitter/Lb)*iSplitter
        
            !~~~~~~~~~~~~~~~~~~~~~ROTOR OUTLET~~~~~~~~~~~~~~~~~~~!               
            EffrLast = 0
            jEFF = 0
            
            DO WHILE (ABS((Effr-EffrLast)/Effr).GT.0.000001)
                jEFF = jEFF+1
                EffrLast = Effr
                IF (jEFF.GT.MaxIter) GOTO 1337
                
                P02=PRr*P01!kPa
                !Ideal (isentropic) compression to P02
                S02ideal=S01!J/mol-K
                CALL PSFLSH (P02,S02ideal,zz,T02ideal,rho02ideal,Dl,Dv,x,y,q,e,H02ideal,cv,cp,w,ierr,herr)
                !Actual total enthalpy at rotor outlet
                H02=H01+(H02ideal-H01)/Effr!J/mol
                !Actual total conditions at rotor outlet
                CALL PHFLSH (P02,H02,zz,T02,rho02,Dl,Dv,x,y,q,e,S02,cv,cp,w,ierr,herr)
                rho02=rho02*wm!kg/m^3
                !Actual total enthalpy across rotor
                DeltaH0Rotor=(H02-H01)*1000/wm!J/kg or m/s
                !From the Euler turbine equation
                Cu2=(1/U2)*(DeltaH0Rotor+U1*Cu1)!m/s

                !ROTOR OUTLET VELOCITY TRIANGLE
                !First iteration using stagnation density
                Cr2=zMdot/(rho02*A2)
                last=0
                i=0
                C2 = 1
                DO WHILE (ABS((C2 - last)/C2) .GT. 0.0000001)
                    i=i+1
                    last=C2
                    C2=SQRT(Cr2**2+Cu2**2)
                    !Stagnation is an isentropic process
                    H2=H02-(C2**2)*wm/2000!J/mol
                    S2=S02
                    CALL HSFLSH (H2,S2,zz,T2,P2,rho2,Dl,Dv,x2,y,q,e,cv,cp,a_2,ierr,herr)
                    rho2=rho2*wm!kg/m^3
                    Cr2=zMdot/(rho2*A2)!Update Cr2
                    IF (i.GT.MaxIter) GOTO 1337
                    IF (ierr.NE.0) THEN!GOTO 1337
                        Cr2 = 15
                        WRITE(*,*) "Error Flag at Rotor Outlet Meanline: ",ierr
                        WRITE(*,*) "Error String: ",herr
                    END IF
                END DO
            
                !CONVERGED ROTOR OUTLET VELOCITY TRIANGLE
                C2=SQRT(Cr2**2+Cu2**2)
                Alpha2=ATAN(Cu2/Cr2)*(180/PI)
                Wu2=Cu2-U2
                W2=SQRT(Wu2**2+Cr2**2)
                Beta2=ATAN(Wu2/Cr2)*(180/PI)
                aM2=C2/a_2!Absolute Mach Number
                rM2=W2/a_2!Relative Mach Number
                
                CALL TRNPRP (T2,rho2/wm,x2,eta,tcx,ierr,herr)
                IF (ierr.NE.0) THEN
                    WRITE(*,*) "Viscosity Error Flag at Rotor Outlet: ",ierr
                    WRITE(*,*) "Error String: ",herr
                END IF
                Visc2 = eta/1000000!kg/m*s
                
                !CALCULATE LOSS MODELS
                CALL InternalLosses()
                
                !Calculate Actual Discharge Enthalpy
                H02real = H02ideal + LossesInternal*wm/1000 
                CALL HSFLSH (H02real,S02,zz,T02,P02,rho02,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
                Effr = (H02ideal - H01)/(H02real - H01) !Updated Rotor Efficiency
                    
            END DO
                
            !ROTOR DEHALLER NUMBER
            dHm=W2/W1
            dHh=W2/W1h
            dHt=W2/W1s

            Sigma = 1
            SigmaLast = 0
            Beta2m = Beta2
            !Estimate Metal Angle Based on Slip Factor (Aungier 2000)
            DO WHILE (ABS((Sigma-SigmaLast)/Sigma).GT.0.0000001)
            
                !Sigma = 1 - ((PI/Z)*COS(Beta2m*PI/180))/(1-phi*TAN(Beta2m*PI/180))!(Stodola)
                !Sigma = 1 - (0.63*PI/Z)/(1-phi*TAN(Beta2m*PI/180))!(Stanitz)
                Sigma = 1 - (SQRT(ABS(COS(Beta2m*PI/180)))*SIN(Alpha2*PI/180)/Z**.7)!(Wiesner/Busemann)
                
                SigmaStar = SIN(19+0.2*(Beta2m+90))
                E_limit = (Sigma - SigmaStar)/(1 - SigmaStar)
                
                IF (r1m/r2 .GT. E_limit) THEN
                Sigma = Sigma*(1 - (((r1m/r2)-E_limit)/(1-E_limit))**(SQRT(ABS(90+Beta2m)/10)))
                END IF

                SigmaLast = Sigma
                phi=Cr2/U2
                Cu2i = Cu2/Sigma
                Wu2i =  Cu2i - U2
                Beta2m = ATAN(Wu2i/Cr2)*(180/PI)
                Alpha2 = ATAN(Cu2i/Cr2)*(180/PI)
                
            END DO

            !FINAL ROTOR OUTLET VELOCITY TRIANGLE (with slip factor)
            Cu2= Cu2/Sigma
            Wu2 = Cu2 - U2
            Beta2m = ATAN(Wu2/Cr2)*180/PI
            C2 = SQRT(Cr2**2+Cu2**2)
            W2=SQRT(Wu2**2+Cr2**2)
            Alpha2=ATAN(Cu2/Cr2)*(180/PI)
            workcoeff = (H02real-H01)/(U2**2*wm/1000)
            FlowCoeff = zMdot/(rho01*PI*(r2**2)*U2)
            RotationalMach = U2/a_2

        END DO

        !~~~~~~~~~~~~~~~~~~~~IMPELLER PARASITIC LOSSES~~~~~~~~~~~~~~~~~~~~~~~!
        !  Follows the method overviewed in Aungier (2000)
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
                
        !~~~~~~~~~~~~~~~ Disk Friction Work (Daily and Nece) ~~~~~~~~~~~~~~~~!
        
        !First determine the flow conditions in the clearance gap as follows:
        r_bar = (r1m + r2)/2
        b_bar = (b1 + b2)/2
        !Average pressure difference across the blade in the clearance gap
        Pcl = zMdot*(r2*Cu2-r1m*Cu1)/(Z*r_bar*b_bar*Lb)
        !Velocity of the clearance gap leakage flow
        Ucl = 0.816*SQRT(2*Pcl/rho2)
        !Blade clearance gap leakage mass flow rate for all the blades
        clMdot = rho2*Ucl*(Z*TipClearance*Lb)
        
        Tcl = T2!Assume temperature at the rotor outlet is the same as clearance gap
        
        CALL TPFLSH (Tcl,Pcl,x,rhocl,dl,dv,xliq,xvap,q,e,hs,ss,cv,cp,w,ierr,herr)
        CALL TRNPRP (Tcl,rhocl,x,eta,tcx,ierr,herr)
        IF (ierr.NE.0) THEN
            WRITE(*,*) "Viscosity Error Flag for Disk Friction Calc: ",ierr
            WRITE(*,*) "Error String: ",herr
        END IF
        
        Visc_cl = eta/1000000 !kg/m*s

        ReDF = ABS(rho2*U2*r2/Visc_cl)

        IF (ReDF.LT.300000) THEN
             f_df = 2.67/(ReDF**0.5)
        ELSE
             f_df = 0.0622/(ReDF**0.2)
        END IF

        rho_bar = (rho1+rho2)/2
        DiskFriction = f_df * rho_bar * r2**2 * U2**3 / (4*zMDot)

        !~~~~~~~~~~~~~~~~~~~ Leakage Work (Aungier)~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        LeakageWork = clMdot*Ucl*U2/(2*zMdot)!Assuming an open impeller

        !~~~~~~~~~~~~~~~~~~~~~~~ Recirculation Work ~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        Df = 1-W2/W1s+(0.75*(U2*Cu2-U1*Cu1)/U2**2)/((W1s/W2)*((Z/PI)*(1-r1s/r2)+(2*r1s/r2)))

        RecirculationWork = 0.000008*SINH(3.5*(ATAN(Cu2/Cr2))**3)*Deq**2*U2**2

        !~~~~~~~~~~~~~~~ SUM PARASITIC LOSS COEFFICIENTS ~~~~~~~~~~~~~~~~~~~~~~!

        LossesParasitic = DiskFriction + LeakageWork + RecirculationWork

        PRr = P02/P01 !Rotor Pressure Ratio
        H02real = H02real + LossesParasitic*wm/1000 
        S02ideal = S01
        CALL PSFLSH (P02,S02ideal,zz,T02ideal,rho02ideal,Dl,Dv,x,y,q,e,H02ideal,cv,cp,w,ierr,herr)
        Effr = (H02ideal - H01)/(H02real - H01)
       
        !~~~~~~~~~~~~~~~~~~ END OF IMPELLER ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~~~~~!
        
        IF (iDiffAnalysis == 0 .AND. iVoluteAnalysis == 0) THEN !(If only impeller analysis is specified)
            PRs = PRr
            Effs = Effr
        END IF
        
        IF (iDiffAnalysis == 1) THEN
            
            !~~~~~~~~~~~~~~~~~~~~~~~~~~DIFFUSER ANALYIS~~~~~~~~~~~~~~~~~~~~~~~~~~!
            !The diffuser analysis currently just uses an inputted loss factor.
            !For a full analysis method see Aungier (2000), or Roberts (2003).
            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
            
            !Assume flow properties at rotor outlet are the same as diffuser inlet
            Cu3 = Cu2
            Cm3 = Cr2
            C3 = C2
            r3 = r2
            b3 = b2
            H03 = H02real
            H3 = H2
            S03 = S02
            S3 = S2
            P03 = P02
            P3 = P2
            T03 = T02
            T3 = T2
            rho03 = rho02
            rho3 = rho2
                        
            !Initial Conditions
            PLast = P3*1000
            P0 = P03*1000
            T = T3
            H0 = H03
            H = H3
            S0 = S03
            S = S3
            rho = rho3
            rhoLast = rho
            blast=b3
            rlast = r3
            Alpha = (ATAN((r4-r3)/((b3/2)-(b4/2))))*(180/PI)!Assuming constant taper. With respect to tangent.
            DiffuserLength = r4 - r2
            LengthM = DiffuserLength*SIN(Alpha*PI/180)
            DeltaM = LengthM/iDiffSteps
            CurrentLengthM = DeltaM
            C = C3
            CmLast = Cm3
            CuLast = Cu3
            CLast = C3
            Slast = 5
            
            !Initialize boundary layer information
            Del = b3*(1-(b3/r3)**0.15)/2 !Initial boundary layer thickness estimate
            rCue = rlast * CuLast !Assume this is conserved until boundary layers fill the passage
            Blockage = Del/(4*blast)
            Dmax = 0.4*((b3/LengthM)**0.35)*SIN(Alpha*PI/180) !Diffusion divergence parameter
            
            i=0
            DO WHILE (i /= iDiffSteps)
                
                !Get next downstream radius and diffuser width
                radius = DeltaM*SIN(Alpha*PI/180) + rLast
                b = b3 + (b4-b3)/(r4-r3)*(radius-r3)
                
                ! Calculate next downstream station whirl velocity
                !Cu = ABS(radius*CuLast/(radius+DeltaM)) - ABS(radius*C*Cu*cf*DeltaM/(b*Cm)) !S.Roberts
                Cu = -(DeltaM*rlast*CLast*CuLast*cf)/(radius*(blast*CmLast))+(rlast*CuLast/radius)
                
                IF (Cu.LT.0) THEN
                    Cu = 0
                END IF
                                
                !Estimate radial (meridional velocity) from continuity equation
                Cm = zMdot / (2*PI*radius*rho*b*(1-Blockage))
            
                    k=0
                    DO WHILE (ABS((Cm-CmLastIter)/Cm).GT.0.000001)
                        k=k+1
                        CmLastIter = Cm
                        C = SQRT(Cu**2+Cm**2)
                        H = H0 - (C**2)*wm/2000!J/mol
                        CALL HSFLSH (H,S,zz,T,P,rho,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
                        rho=rho*wm!kg/m^3
                        Cm = zMDot / (2*PI*radius*rho*b*(1-Blockage))
                        Slast=S
                        IF (k.GT.MaxIter) THEN
                            GOTO 1337
                            WRITE(*,*) "Max iteration reached in Diffuser - Cm"
                        ELSE IF (ierr.NE.0) THEN
                            WRITE(*,*) "Error Flag at Diffuser: ",ierr
                            WRITE(*,*) "Error String: ",herr
                        END IF
                    END DO

                    !Calculate next downstream station static pressure
                    P = DeltaM*rhoLast*((CuLast**2)*SIN(Alpha*PI/180)/rLast - CmLast*(Cm-CmLast)/DeltaM &
                    - CLast*CmLast*cf/blast & !- DeltaId/DeltaM - dIc (Aungier's additional loss terms)
                    ) + Plast
                    P = P/1000 !kPa

                    CALL PHFLSH (P,H,zz,T,rho,Dl,Dv,x,y,q,e,S,cv,cp,a_4,ierr,herr)
                    P = P*1000!Pa
                    rho=rho*wm!kg/m^3

                    !and the corresponding total conditions are
                    H0 = H + (C**2)*wm/2000!J/mol
                    S0 = S
                    CALL HSFLSH (H0,S0,zz,T0,P0,rho0,Dl,Dv,x,y,q,e,cv,cp,w,ierr,herr)
                    P0 = P0*1000!Pa
                    rho0 = rho0*wm
                
                ! Calculate boundary layer thickness at next downstream station:
                Del = 2.25*b*(1-(radius*Cu/rCue))
              
                ! Obviously, the boundary layers cannot be greater than the passage width or negative:
                IF (Del > b/2) THEN
                    Del = b/2
                END IF           
                IF (Del < 0) THEN
                    Del = 0
                END IF
                
                ! Calculate blockage factor at next downstream station:
                Blockage = Del/(4*b)
                IF (Del /= 0) THEN
                    CALL TRNPRP (T,rho/wm,zz,eta,tcx,ierr,herr)
                    IF (ierr.NE.0) THEN
                        WRITE(*,*) "Viscosity Error Flag at Diffuser: ",ierr
                        WRITE(*,*) "Error String: ",herr
                    END IF
                    ViscDiff = eta/1000000!kg/m*s
                    ! Calculate skin friction coefficient:
                    Re = rho*Cm*b/ViscDiff
                    dHdif = 2*Del
                    CALL SkinFrictionCoefficient(Re,dHdif,RoughnessImp)
                END IF                
                
                ! Store last velocities and other variables:
                CuLast = Cu
                CmLast = Cm
                CLast = C
                PLast = P
                rhoLast = rho
                rLast = radius
                dIdLast = dId
                blast = b
                CurrentLengthM = CurrentLengthM + DeltaM
                i = i+1
                    
            END DO

                H04 = H0
                S04 = S0
                P04 = P0/1000
                T04 = T0
                rho04 = rho0
                H4 = H
                S4 = S
                P4 = P/1000
                T4 = T
                rho4 =rho
                
                Cu4 = Cu
                Cm4 = Cm
                        
                C4=SQRT(Cm4**2+Cu4**2)
                Alpha4=ATAN(Cu4/Cm4)*(180/PI)
                aM4=C4/a_4!Absolute Mach Number
            
            PRd = P04/P01
            S04ideal = S01
            CALL PSFLSH (P04,S04ideal,zz,T04ideal,rho04ideal,Dl,Dv,x,y,q,e,H04ideal,cv,cp,w,ierr,herr)
            Effd = (H04ideal - H01)/(H04 - H01)
            
        END IF
        
        !~~~~~~~~~~~~~~~~~~~~~ END OF DIFFUSER ANALYIS ~~~~~~~~~~~~~~~~~~~~~~!    
        IF (iDiffAnalysis == 1 .AND. iVoluteAnalysis == 0) THEN !(If diffuser analysis is specified)
            PRs = PRd
            Effs = Effd
        END IF
        
        IF (iVoluteAnalysis == 1) THEN
        
        !~~~~~~~~~~~~~~~~~~~~~~~~~~VOLUTE ANALYIS~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !The volute analysis currently just uses an inputted loss factor.
        !For a full analysis method see Aungier (2000), or Roberts (2003).
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        
            !Assume flow properties at rotor outlet are the same as diffuser inlet
            Cu5 = Cu4
            Cr5 = Cr4
            r5 = r4
            H05 = H04
            P05 = P04
            P5 = P4
            !Diffuser Outlet Conditions
            H06 = H05
            Cu6 = Cu5*r5/r6
            Wu6 = Wu5*r5/r6
            Cr6 = Cr5

            P06 = P05 - VoluteLossFactor*(P05-P5)!kPa
            CALL PHFLSH (P06,H06,zz,T06,rho06,Dl,Dv,x,y,q,e,S06,cv,cp,w,ierr,herr)

            last=0
            i=0
            DO WHILE (ABS((Cr6 - last)/Cr6) .GT. 0.000000001)
                i=i+1
                last=Cr6
                C6=SQRT(Cr6**2+Cu6**2)
                !Stagnation is an isentropic process
                H6=H06-(C6**2)*wm/2000!J/mol
                S6=S06
                CALL HSFLSH (H6,S6,zz,T6,P6,rho6,Dl,Dv,x,y,q,e,cv,cp,a_6,ierr,herr)
                rho6 = rho6*wm!kg/m^3
                Cr6 = zMdot/(rho6*(2*PI*r6*b6))!Update Cm6
                IF (i.GT.MaxIter) THEN
                    WRITE(*,*) "Max iteration reached at Volute Outlet"
                ELSE IF (ierr.NE.0) THEN
                    WRITE(*,*) "Error Flag at Volute Outlet: ",ierr
                    WRITE(*,*) "Error String: ",herr
                END IF
            END DO

            C6=SQRT(Cr6**2+Cu6**2)
            Alpha6=ATAN(Cu6/Cr6)*(180/PI)
            Beta4=ATAN(Wu6/Cr6)*(180/PI)
            W6=SQRT(Wu6**2+Cr6**2)
            aM6=C6/a_6!Absolute Mach Number
            rM6=W6/a_6!Relative Mach Number
                        
            !Isentropic efficiency for P01 to P04
            PRs = P06/P01
            S06ideal = S01
            CALL PSFLSH (P06,S06ideal,zz,T06ideal,rho06ideal,Dl,Dv,x,y,q,e,H06ideal,cv,cp,w,ierr,herr)
            Effs = (H06ideal-H01)/(H06 - H01)
        END IF
        
        PRr = PRr*(Target_PR/PRs) !Update Rotor Pressure Ratio Until Stage Pressure Ratio Converges

    END DO

!~~~~~~~~~~~~~~~~~~~~~~~~~~ OUTPUT RESULTS ~~~~~~~~~~~~~~~~~~~~~~~~~~!
!Output is based on what is set in the input text file.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
IF(iPerformance == 0) THEN
12 FORMAT (A49,I8)    !I = Integer, Field Width = 8
13 FORMAT (A47,F14.7) !F = Real, Field Width = 14, and 7 Decimal Places
11 FORMAT (A35,F14.7,A12) !F = Real, Field Width = 14, and 7 Decimal Places

    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~~ COMPRESSOR DESIGN ~~~~~~~~~~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,12) "Compressor Rotational Speed [RPM].............|  ",N
    WRITE (10,13) "Mass Flow Rate [kg/s].........................|",zMdot
    WRITE (10,*) 
    WRITE (10,12) "Number of Full Blades.........................|  ",iFull
    WRITE (10,12) "Number of Splitter Blades.....................|  ",iSplitter
    WRITE (10,13) "Length of Full Blade [m]......................|",LbImpeller
    WRITE (10,13) "Length of Splitter Blade [m]..................|",LbSplitter
    WRITE (10,13) "Length of Meanline Path [m]...................|",Lb
    WRITE (10,*)
    WRITE (10,13) "Inlet Blade Thickness [m].....................|",tb1
    WRITE (10,13) "Inlet Hub Radius [m]..........................|",r1h
    WRITE (10,13) "Inlet Shroud Radius [m].......................|",r1s
    WRITE (10,13) "Inlet Meanline Radius [m].....................|",r1m
    WRITE (10,*)
    WRITE (10,13) "Outlet Blade Thickness[m].....................|",tb
    WRITE (10,13) "Outlet Passage Width[m].......................|",b2    
    WRITE (10,13) "Outlet Tip Radius [m].........................|",r2
    WRITE (10,*)
    WRITE (10,13) "Rotor Axial Length [m]........................|",DeltaZ
    WRITE (10,13) "Impeller Roughness............................|",RoughnessImp
    WRITE (10,13) "Tip Clearance [m].............................|",TipClearance
    IF (iDiffAnalysis == 1) THEN
        WRITE (10,13) "Diffuser Exit Radius [m]......................|",r4
        WRITE (10,13) "Diffuser Exit Passage Width [m]...............|",b4
        WRITE (10,13)
    ENDIF
    IF (iVoluteAnalysis == 1) THEN
        WRITE (10,13) "Volute Loss Factor............................|",VoluteLossFactor
        WRITE (10,13) "Volute Exit Radius [m]........................|",r6
        WRITE (10,13) "Volute Exit Passage Width [m].................|",b6
        WRITE (10,*)
    ENDIF
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~ THERMODYNAMIC PROPERTIES ~~~~~~~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~ ROTOR INLET - Station 1 ~~~~~~~~~~~~~~~~~"
    WRITE (10,11) "Total Enthalpy (H01):                  ",H01/wm," kJ/kg"
    WRITE (10,11) "Total Entropy (S01):                   ",S01/wm," kJ/kg-K"
    WRITE (10,11) "Total Temperature (T01):               ",T01," K"
    WRITE (10,11) "Total Pressure (P01):                  ",P01," kPa"
    WRITE (10,11) "Total Density (rho01):                 ",rho01," kg/m^3"
    WRITE(10,*)
    WRITE (10,11) "Static Enthalpy (H1):                  ",H1/wm," kJ/kg"
    WRITE (10,11) "Static Entropy (S1):                   ",S1/wm," kJ/kg-K"
    WRITE (10,11) "Static Temperature (T1):               ",T1," K"
    WRITE (10,11) "Static Pressure (P1):                  ",P1," kPa"
    WRITE (10,11) "Static Density (rho1):                 ",rho1," kg/m^3"
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~ ROTOR LOSSES - Between Stations 1 & 2 ~~~~~~~~~~"
    WRITE (10,11) "Incidence Loss:                        ", LossIncidence/1000," kJ/kg"
    WRITE (10,11) "Blade Loading Loss:                    ", BladeLoadingLoss/1000," kJ/kg"
    WRITE (10,11) "Skin Friction Loss:                    ", SkinFrictionLoss/1000," kJ/kg"
    WRITE (10,11) "Tip Clearance Loss:                    ", TipClearanceLoss/1000," kJ/kg"
    WRITE (10,11) "Mixing Loss:                           ", LossMixing/1000," kJ/kg"
    WRITE (10,11) "Disk Friction Loss:                    ", DiskFriction/1000," kJ/kg"
    WRITE (10,11) "Leakage Work Loss:                     ", LeakageWork/1000," kJ/kg"
    WRITE (10,11) "Recirculation Work Loss:               ", RecirculationWork/1000," kJ/kg"
    WRITE (10,*)
    WRITE (10,11) "Internal Loss Total:                   ", LossesInternal/1000," kJ/kg"
    WRITE (10,11) "Parasitic Loss Total:                  ", LossesParasitic/1000," kJ/kg"
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~ ROTOR OUTLET - Station 2 ~~~~~~~~~~~~~~~~"
    WRITE (10,11) "Total Enthalpy (H02):                  ",H02real/wm," kJ/kg"
    WRITE (10,11) "Total Entropy (S02):                   ",S02/wm," kJ/kg-K"
    WRITE (10,11) "Total Temperature (T02):               ",T02," K"
    WRITE (10,11) "Total Pressure (P02):                  ",P02," kPa"
    WRITE (10,11) "Total Density (rho02):                 ",rho02*wm," kg/m^3"
    WRITE(10,*)
    WRITE (10,11) "Static Enthalpy (H2):                  ",H2/wm," kJ/kg"
    WRITE (10,11) "Static Entropy (S2):                   ",S2/wm," kJ/kg-K"
    WRITE (10,11) "Static Temperature (T2):               ",T2," K"
    WRITE (10,11) "Static Pressure (P2):                  ",P2," kPa"
    WRITE (10,11) "Static Density (rho2):                 ",rho2," kg/m^3"
    WRITE(10,*)
    WRITE (10,11) "Rotor Efficiency:                      ",Effr," kg/m^3"
    WRITE(10,*)
    IF (iDiffAnalysis == 1) THEN
        WRITE (10,*) "~~~~~~~~~~~~~~~~~~ DIFFUSER OUTLET - Station 4 ~~~~~~~~~~~~~~~"
        WRITE (10,*) 
        WRITE (10,11) "Total Enthalpy (H04):                  ",H04/wm," kJ/kg"
        WRITE (10,11) "Total Entropy (S04):                   ",S04/wm," kJ/kg-K"
        WRITE (10,11) "Total Temperature (T04):               ",T04," K"
        WRITE (10,11) "Total Pressure (P04):                  ",P04," kPa"
        WRITE (10,11) "Total Density (rho04):                 ",rho04," kg/m^3"
        WRITE(10,*)
        WRITE (10,11) "Static Enthalpy (H4):                  ",H4/wm," kJ/kg"
        WRITE (10,11) "Static Entropy (S4):                   ",S4/wm," kJ/kg-K"
        WRITE (10,11) "Static Temperature (T4):               ",T4," K"
        WRITE (10,11) "Static Pressure (P4):                  ",P4," kPa"
        WRITE (10,11) "Static Density (rho4):                 ",rho4," kg/m^3"
        WRITE(10,*)
        WRITE (10,11) "Diffuser Efficiency:                   ",Effd," kg/m^3"
        WRITE(10,*)
    ENDIF
    IF (iVoluteAnalysis == 1) THEN
        WRITE (10,*) "~~~~~~~~~~~~~~~ COMPRESSOR OUTLET - Station 6 ~~~~~~~~~~~~~~~~"
        WRITE (10,*)
        WRITE (10,11) "Total Enthalpy (H06):                  ",H06/wm," kJ/kg"
        WRITE (10,11) "Total Entropy (S06):                   ",S06/wm," kJ/kg-K"
        WRITE (10,11) "Total Temperature (T06):               ",T06," K"
        WRITE (10,11) "Total Pressure (P06):                  ",P06," kPa"
        WRITE (10,11) "Total Density (rho06):                 ",rho06," kg/m^3"
        WRITE(10,*)
        WRITE (10,11) "Static Enthalpy (H6):                  ",H6/wm," kJ/kg"
        WRITE (10,11) "Static Entropy (S6):                   ",S6/wm," kJ/kg-K"
        WRITE (10,11) "Static Temperature (T6):               ",T6," K"
        WRITE (10,11) "Static Pressure (P6):                  ",P6," kPa"
        WRITE (10,11) "Static Density (rho6):                 ",rho6," kg/m^3"
        WRITE(10,*)
    END IF
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~~~~ VELOCITY TRIANGLES ~~~~~~~~~~~~~~~~~~~"

    WRITE(10,*)
    WRITE (10,11) "Imp. Inlet Velocity Triangle (Mean):   "
    WRITE (10,11) "Tangential Velocity (U1):                  ",U1," m/s"
    WRITE (10,11) "Axial Velocity (Cm1):                      ",Cm1," m/s"
    WRITE (10,11) "Relative Velocity (W1):                    ",W1," m/s"
    WRITE (10,11) "Relative Tang. Component (Wu1):            ",Wu1," m/s"
    WRITE (10,11) "Relative Flow Angle (Beta1):               ",Beta1," deg"
    WRITE (10,11) "Absolute Velocity (C1):                    ",C1," m/s"
    WRITE (10,11) "Absolute Tang. Component (Cu1):           ",Cu1," m/s"
    WRITE (10,11) "Inlet Swirl Angle (Alpha1):                ",Alpha1," deg"
    WRITE (10,11) "Inlet Mach Number (M1):                    ",zM1rel
    WRITE (10,*)
    WRITE (10,11) "Imp. Inlet Velocity Triangle (Hub):        "
    WRITE (10,11) "Tangential Velocity (U1h):                 ",U1h," m/s"
    WRITE (10,11) "Axial Velocity (Cm1h):                     ",Cm1h," m/s"
    WRITE (10,11) "Relative Velocity (W1h):                   ",W1h," m/s"
    WRITE (10,11) "Relative Tang. Component (Wu1h):           ",Wu1h," m/s"
    WRITE (10,11) "Relative Flow Angle (Beta1h):              ",Beta1h," deg"
    WRITE (10,11) "Absolute Velocity (C1h):                   ",C1h," m/s"
    WRITE (10,11) "Absolute Tang. Component (Cu1h):           ",Cu1h," m/s"
    WRITE (10,11) "Inlet Swirl Angle (Alpha1):                ",Alpha1," deg"
    WRITE (10,11) "Inlet Mach Number (M1h):                   ",zM1relh
    WRITE (10,*)
    WRITE (10,11) "Imp. Inlet Velocity Triangle (Tip):        "
    WRITE (10,11) "Tangential Velocity (U1t):                 ",U1s," m/s"
    WRITE (10,11) "Axial Velocity (Cm1t):                     ",Cm1s," m/s"
    WRITE (10,11) "Relative Velocity (W1t):                   ",W1s," m/s"
    WRITE (10,11) "Relative Tang. Component (Wu1t):           ",Wu1s," m/s"
    WRITE (10,11) "Relative Flow Angle (Beta1t):              ",Beta1s," deg"
    WRITE (10,11) "Absolute Velocity (C1t):                   ",C1s," m/s"
    WRITE (10,11) "Absolute Tang. Component (Cu1t):           ",Cu1s," m/s"
    WRITE (10,11) "Inlet Swirl Angle (Alpha1):                ",Alpha1," deg"
    WRITE (10,11) "Inlet Mach Number (M1t):                   ",zM1rels
    WRITE (10,*)
    WRITE (10,11) "Imp. Outlet Velocity Triangle:         "
    WRITE (10,11) "Tangential Velocity (U2):              ",U2," m/s"
    WRITE (10,11) "Radial Velocity (Cr2):                 ",Cr2," m/s"
    WRITE (10,11) "Absolute Velocity Tangential (Cu2):    ",Cu2," m/s"
    WRITE (10,11) "Absolute Velocity (C2):                ",C2," m/s"
    WRITE (10,11) "Swirl Angle (Alpha 2):                 ",Alpha2," deg"
    WRITE (10,11) "Relative Velocity Tangential (Wu2):    ",Wu2," m/s"
    WRITE (10,11) "Relative Velocity (W2):                ",W2," m/s"
    WRITE (10,11) "Flow Angle (Beta2):                    ",Beta2,"deg"
    WRITE (10,11) "Metal Angle (Beta2m):                  ",Beta2m,"deg"
    WRITE (10,11) "Absolute Mach Number (M2):             ",aM2
    WRITE (10,11) "Relative Mach Number (M2r):            ",rM2
    WRITE(10,*)
    IF (iDiffAnalysis == 1) THEN
        WRITE (10,11) "Diff. Outlet Velocity Triangle:        "
        WRITE (10,11) "Meridional Velocity (Cm4):             ",Cm4," m/s"
        WRITE (10,11) "Absolute Velocity Tangential (Cu4):    ",Cu4," m/s"
        WRITE (10,11) "Absolute Velocity (C4):                ",C4," m/s"
        WRITE (10,11) "Swirl Angle (Alpha 4):                 ",Alpha4," deg"
        WRITE (10,11) "Absolute Mach Number (M4):             ",aM4
    ENDIF
    IF (iVoluteAnalysis == 1) THEN
        WRITE (10,11) "Compressor Outlet Velocity Triangle:   "
        WRITE (10,11) "Radial Velocity (Cr6):                 ",Cr6," m/s"
        WRITE (10,11) "Absolute Velocity Tangential (Cu6):    ",Cu6," m/s"
        WRITE (10,11) "Absolute Velocity (C6):                ",C4," m/s"
        WRITE (10,11) "Swirl Angle (Alpha 6):                 ",Alpha6," deg"
        WRITE (10,11) "Relative Velocity Tangential (Wu6):    ",Wu6," m/s"
        WRITE (10,11) "Relative Velocity (W2):                ",W6," m/s"
        WRITE (10,11) "Metal Angle (Beta6):                   ",Beta6,"m/s"
        WRITE (10,11) "Absolute Mach Number (M6):             ",aM6
        WRITE (10,11) "Relative Mach Number (M6):            ",rM6
    ENDIF
    WRITE (10,*) "~~~~~~~~~~~~~~ NON-DIMENSIONAL DESIGN PARAMETERS ~~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,11) "de Haller number:                      ",dHm
    WRITE (10,11) "Flow Factor:                           ",phi
    WRITE (10,11) "Work Coefficient:                      ",workcoeff
    WRITE (10,11) "Flow Coefficient:                      ",FlowCoeff 
    WRITE (10,11) "Rotational Mach Number:                ",RotationalMach
    WRITE (10,*)
    WRITE(10,*)"*************************************************************"
    WRITE(10,11) "COMPRESSOR PRESSURE RATIO:                  ",PRs
    WRITE(10,11) "COMPRESSOR EFFICIENCY:                      ",Effs
    WRITE(10,*)"*************************************************************"
    WRITE(10,*) 

END IF

IF (iPerformance == 1) THEN

    15 FORMAT (F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6)
    WRITE(10,15)r1h,r1s,b2,zM1rels,rM2,dHm,FlowCoeff,workcoeff,Effs
    WRITE(*,*) "Solution ",isolution," Complete."

END IF

IF (iPerformance == 2) THEN

    35 FORMAT (I3,10X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,3X,F8.4,5X, F8.6)
    37 FORMAT (I3,A30,I3,A13)
    WRITE(10,35)iFull,zM1rels,rM2,dHt,dHh,dHm,FlowCoeff,workcoeff,Effs,Beta2m
    WRITE(*,37) m," Blades Solution Complete In: ",k,"Iterations."

END IF

IF (iPerformance == 3) THEN

    75 FORMAT (F8.5,4X,F8.4,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F7.6)
    77 FORMAT (A12,I3, A22,I3,A13)
    WRITE(10,75)T01,P01,zM1rels,rM2,dHt,dHh,dHm,FlowCoeff,workcoeff,Effs
    WRITE(*,77) "Solution #",m," Complete In: ",k,"Iterations."

END IF

    1337 CONTINUE
    
END SUBROUTINE PERFORMANCE

!~~~~~~~~~~~~~~~~~~~~~~~~ END OF MEANLINE PERFORMANCE ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~!

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                          SUBROUTINE: Internal Losses
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE InternalLosses()

IMPLICIT DOUBLE PRECISION (a-h,l,o-z)
IMPLICIT INTEGER (i-k,m,n)

COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDiffAnalysis, iVoluteAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1, FlowCoefficient
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m,isolution,iDiffSteps
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2m, Beta2guess
COMMON / Losses         / LossesInternal, LossesParasitic, cf, Sigma, ShockLoss, LossIncidence, &
                          DiffusionLoss, ChokingLoss, BladeLoadingLoss, HSLoadingLoss, &
                          SkinFrictionLoss, TipClearanceLoss, LossMixing, SupercritMachNumberLoss

    ! Calculate Skin Friction loss:  (Jansen)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Average Hydraulic Diameter
    PassageA1 = A1 * (ABS(1+COS(Beta1*PI/180))/2) / iFull
    ThroatArea = PassageA1-tb1*(r1s-r1h)

    IF (iSplitter.NE.0) THEN
        TipArea = b2*(2*PI*r2/iFull - (iFull+iSplitter)*tb) * COS(Beta2m*PI/180)
        TipPerimeter = 4*(b2+2*PI*r2/(iFull+iSplitter))
    ELSE
        TipArea = b2*(2*PI*r2/iFull - (iFull)*tb) * COS(Beta2m*PI/180)
        TipPerimeter = 2*(b2+2*PI*r2/(iFull))
    END IF

    ThroatPerimeter = 2*(r1s-r1h+PI*(r1s+r1h)/iFull)
    dHImpeller = 2*(ThroatArea/ThroatPerimeter + TipArea/TipPerimeter)
    
    !Mean Relative Velocity
    WBar = (C1*r1s/r1m + C2 + W1s + 2*W1h + 3*W2)/8

    !Average Inlet and Outlet Viscosity
    Visc = (Visc1+Visc2)/2 

    ReImpeller = (rho1+rho2)/2*WBar*dHImpeller/Visc

    CALL SkinFrictionCoefficient(ReImpeller, dHImpeller, RoughnessImp)

    SkinFrictionLoss = 2*cf*Lb/dHImpeller*Wbar**2

    ! Calculate Blade Work Input:
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    AR = TipArea/ThroatArea
    phi2 = zMDot / (rho2*A2*U2)

    IF (ABS((P02-P2)/P02).GT.0.001) THEN
        !Tip Blockage (Aungier, 2000)
        BB2 = SkinFrictionLoss * (P01*1000-P1*1000)/(P02*1000-P2*1000) * SQRT(ABS(W1*dHImpeller/(W2*b2))) + &
                    (0.3+(b2/Lb)**2) * AR**2 * rho2*b2/(rho1*Lb) +&
                    TipClearance/(2*b2)
        !Tip Distortion Factor
        Lambda = (1/(1-BB2))
        BladeWork = Sigma * (1+Lambda*phi2*TAN(Beta2m*PI/180)) - U1*Cu1/U2**2
    ELSE
        BladeWork = (U2*Cu2-U1*Cu1)/U2**2
    END IF

    ! Calculate Incidence Loss: (Whitfield & Baines)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !            !Calculate Incidence Loss: (Aungier)
        !            Wincm = 0.8*(1-(Cm1/(W1*SIN(Beta1*PI/180))))**2 +((iFull*tb1)/(2*PI*r1m*SIN(Beta1*PI/180)))**2
        !            Wincs = 0.8*(1-(Cm1s/(W1s*SIN(Beta1s*PI/180))))**2 +((iFull*tb1)/(2*PI*r1s*SIN(Beta1s*PI/180)))**2
        !            Winct = 0.8*(1-(Cm1h/(W1h*SIN(Beta1h*PI/180))))**2 +((iFull*tb1)/(2*PI*r1h*SIN(Beta1h*PI/180)))**2
        ! 
        !            Winc = (Wincs + 10*Wincm + Winct)/12
    
    Beta1opt = Beta1 !At Design Point
    Winc = (Wu1**2)*(SIN(ABS(Beta1-Beta1opt)*PI/180))**2
    LossIncidence = (Winc**2)/2 !Whitfield and Baines
    !Note: Some Authors multiply this by an Incidence Factor (of 0.5 to 0.7), i.e. Conrad et. al
    LossIncidence = 0 !At Design Point
    
    ! Calculate Blade Loading Loss:   (Coppage et. al.)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Z = iFull + (LbSplitter/Lb)*iSplitter

    Df = 1-W2/W1s+(0.75*(U2*Cu2-U1*Cu1)/U2**2)/((W1s/W2)*((Z/PI)*(1-r1s/r2)+(2*r1s/r2)))
            
    BladeLoadingLoss = 0.05*(Df**2)*(U2**2) !Corrected from 0.005

    ! Calculate Clearance Loss (Jansen)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    TipClearanceLoss = 0.6*(TipClearance/b2)*Cu2*SQRT(ABS(4*PI/b2/Z*((r1s**2-r1h**2)/((r2-r1s)*(1+rho2/rho1)))*Cu2*Cm1))

    ! Calculate Mixing Loss:  (Johnston & Dean)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Diffusion factor (Lieblein, 1959)
    DeltaW = 4*PI*r2*U2*BladeWork/(Z*Lb)
    Deq = (W1+W2+DeltaW)/(2*W2)

    !Size of wake
    IF (Deq.LE.2) THEN
        WSep = W2
    ELSE
        WSep = W2*Deq/2
    END IF

    CmWake = SQRT(ABS(WSep**2-Wu2**2))
    CmMix = (Cr2*A2)/(2*PI*r2*b2)
    E_wake=1-CmMix/CmWake
    !E_wake=((CmMix/CmWake)/W1)**2
    !LossMixing = ((CmMix/CmWake-b3/b2)/(CmMix/CmWake)*C2)**2/(2*(1+(Cu2/Cr2)**2))
    LossMixing = (1/(1+(TAN(Alpha2*PI/180))**2))*(((1-E_wake-b3/b2)/(1-E_wake))**2)*(C2**2/2)

    ! Assumptions (Add if necessary):
    ShockLoss = 0
    DiffusionLoss = 0
    ChokingLoss = 0
    HSLoadingLoss = 0
    SupercritMachNumberLoss = 0

    ! SUM INTERNAL LOSS COEFFICIENTS:
    LossesInternal = (ShockLoss + LossIncidence + DiffusionLoss + ChokingLoss + &
                      BladeLoadingLoss + HSLoadingLoss + SkinFrictionLoss + &
                      TipClearanceLoss + LossMixing + SupercritMachNumberLoss)
END SUBROUTINE InternalLosses

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                       SUBROUTINE: Skin Friction Coefficient
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE SkinFrictionCoefficient(Re, dH, E)

IMPLICIT DOUBLE PRECISION (a-h,l,o-z)
IMPLICIT INTEGER (i-k,m,n) 
    
COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDiffAnalysis, iVoluteAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1, FlowCoefficient
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m,isolution, iDiffSteps
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2m, Beta2guess
COMMON / Losses         / LossesInternal, LossesParasitic, cf, Sigma, ShockLoss, LossIncidence, &
                          DiffusionLoss, ChokingLoss, BladeLoadingLoss, HSLoadingLoss, &
                          SkinFrictionLoss, TipClearanceLoss, LossMixing, SupercritMachNumberLoss

!Laminar flow if (actually <2000 but incase transitional):
IF(Re.LT.4000) THEN
    cfl = 16/Re
    cf = cfl
!Turbulent flow if:
ELSE IF(Re.GE.2000) THEN
    X = 1
    last = 0
    !Turbulent flow over a smooth surfuace given by (Aungier, 2000)
    DO WHILE (ABS((last-X)/X).GT.0.0000001)
        last = X
        X = -2 * LOG10(2.51*X/(Re))
    END DO
    cfts = 0.25*X**(-2)
    !Turbulent flow over a fully rough surface given by (Aungier, 2000)
    X = -2 * LOG10(E/(3.71*dH))
    cftr = .25*X**(-2)
    !Surface roughness becomes significant when:
    Re_e = (Re-2000)*E/dH
    !is greater than 60.  So set cf turbulent to:
    IF(Re_e.LT.60) THEN
        cft = cfts
    ELSE
        cft = cfts + (cftr-cfts)*(1-60/Re_e)
    END IF
END IF

!If transitional from laminar to turbulent, set cf to a weighted average of the two given by (Aungier, 2000):
IF (Re < 4000 .AND. Re > 2000) THEN
    cf = cfl + (cft-cfl)*(Re/2000-1)
ELSE
    cf = cft
END IF

END SUBROUTINE SkinFrictionCoefficient

!                                                    ____
!                                         v        _(    )
!        _ ^ _                          v         (___(__)
!       '_\V/ `
!       ' oX`
!          X                            v
!          X             -HELP! I NEED A COMPRESSOR!
!          X                                                 .       -HAVE NO FEAR, CCMD IS HERE!
!          X        \O/                                      |\  \__O_
!          X.a##a.   M                                       |_\    o \
!       .aa########a.>>                                    __|__ __/_\____
!    .a################aa.                                 \ CCMD RESCUE /
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                     END OF CODE.                         -Sukhveer Sanghera