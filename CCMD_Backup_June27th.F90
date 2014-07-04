!     Last change:  FF   18 Nov 101    6:14 pm
PROGRAM MEANLINE

!VARIABLE DECLARTIONS
IMPLICIT DOUBLE PRECISION (a-h,l,o-z)
IMPLICIT INTEGER (i-k,m,n)
PARAMETER (ncmax=20)   !max number of components in mixture
DIMENSION x(ncmax),xliq(ncmax),xvap(ncmax),f(ncmax)
CHARACTER hrf*3, herr*255, OutFile*30, a*54
CHARACTER*255 hf(ncmax)
CHARACTER*7 hfmix    

COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDVAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2guess
COMMON / Losses         / LossesInternal, LossesParasitic, cf, Sigma, ShockLoss, LossIncidence, &
                          DiffusionLoss, ChokingLoss, BladeLoadingLoss, HSLoadingLoss, &
                          SkinFrictionLoss, TipClearanceLoss, LossMixing, SupercritMachNumberLoss

!INITIALIZE VARIABLES
MaxIter = 1000
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
WRITE(*,*)"                         ...Press Any Key To Begin..."
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
READ(20,14)  a,iDVAnalysis
READ(20,13)  a,DiffuserLossFactor
READ(20,13)  a,r4
READ(20,13)  a,b4
READ(20,13)  a,VoluteLossFactor
READ(20,13)  a,r6
READ(20,13)  a,b6
READ(20,*)
READ(20,*)
READ(20,*)
READ(20,*)
READ(20,*)
READ(20,13)  a,r1h!Check if 0
READ(20,13)  a,r1h_min
READ(20,13)  a,r1h_max
READ(20,*)
READ(20,*)
READ(20,13)  a,r1s!Check if 0
READ(20,13)  a,r1s_min
READ(20,13)  a,r1s_max
READ(20,*)
READ(20,*)
READ(20,14)  a,iFull!Check if 0
READ(20,14)  a,iFull_Min
READ(20,14)  a,iFull_Max
READ(20,*)
READ(20,*)
READ(20,14)  a,iSplitter
READ(20,14)  a,iSplitter_Min
READ(20,14)  a,iSplitter_Max
READ(20,*)
READ(20,*)
READ(20,13)  a,DeltaZ!Check if 0
READ(20,13)  a,DeltaZ_Min
READ(20,13)  a,DeltaZ_Max
READ(20,*)
READ(20,*)
READ(20,13)  a,b2!Check if 0
READ(20,13)  a,b2_Min
READ(20,13)  a,b2_Max
READ(20,*)   
READ(20,14)  a,isteps
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

IF (r1h/=0 .AND. r1s/=0 .AND. iFull/=0 .AND. DeltaZ/=0 .AND. b2/=0 .AND. iSplitter_Min==0 .AND. iSplitter_Max==0) THEN
    
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
!                                   INLET HUB RADIUS CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (r1h==0 .AND. r1s/=0 .AND. iFull/=0 .AND. DeltaZ/=0 .AND. b2/=0) THEN
    
    step = ((r1h_max-r1h_min)/isteps)
    iPerformance = 1
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                       INLET HUB RADIUS ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) ' r1h      M1_tip    M2_rel    dH_tip     dH_hub     dH_m      Phi    Work Co.  Efficiency'
    
    DO m=0,isteps
        
        r1h = r1h_min + step*m
        
       CALL PERFORMANCE()

    END DO  
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    r1h=0
    
END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                  INLET TIP DIAMETER CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (r1h/=0 .AND. r1s==0 .AND. iFull/=0 .AND. DeltaZ/=0 .AND. b2/=0) THEN
    
    step = ((r1s_max-r1s_min)/isteps)
    iPerformance = 2
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                      INLET SHROUD RADIUS ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) ' r1s      M1_tip    M2_rel    dH_tip     dH_hub     dH_m      Phi    Work Co.  Efficiency'
    
    DO m=0,isteps
        
        r1s = r1s_min + step*m
        CALL PERFORMANCE()
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    r1s=0

END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                  NUMBER OF FULL BLADES CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (r1h/=0 .AND. r1s/=0 .AND. iFull==0 .AND. DeltaZ/=0 .AND. b2/=0) THEN
    
    iPerformance = 3
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                    NUMBER OF FULL BLADES ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) '#Blades     M1_tip     M2_rel    dH_tip     dH_hub     dH_m      Phi    Work Co.  Efficiency'
    
    DO m=iFull_Min,iFull_Max
         
         iFull = iFull_min
         CALL PERFORMANCE()
         iFull_Min = iFull_Min+1
         
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    iFull=0
    
END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                  NUMBER OF SPLITTER BLADES CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (r1h/=0 .AND. r1s/=0 .AND. iFull/=0 .AND. iSplitter_min/=0 .AND. iSplitter_max/=0 .AND. DeltaZ/=0 .AND. b2/=0) THEN
    
    iPerformance = 6
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                   NUMBER OF SPLITTER BLADES ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) '#Blades     M1_tip     M2_rel    dH_tip     dH_hub     dH_m      Phi    Work Co.  Efficiency'
    
    DO m=iSplitter_Min,iSplitter_Max
         
         iSplitter = iSplitter_min
         CALL PERFORMANCE()
         iSplitter_Min = iSplitter_Min+1
         
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    iSplitter_min=0
    iSplitter_max=0
    
END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                  AXIAL LENGTH OF ROTOR CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (r1h/=0 .AND. r1s/=0 .AND. iFull/=0 .AND. DeltaZ==0 .AND. b2/=0) THEN
    
    step = ((DeltaZ_max-DeltaZ_min)/isteps)
    iPerformance = 4
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                      ROTOR AXIAL LENGTH ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) 'Length   M1_tip    M2_rel    dH_tip     dH_hub     dH_m      Phi    Work Co.  Efficiency'
    
    DO m=0,isteps
        
        DeltaZ = DeltaZ_min + step*m
        CALL PERFORMANCE()
    
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    DeltaZ = 0
    
END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                 OUTLET PASSAGE WIDTH CHOSEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF (r1h/=0 .AND. r1s/=0 .AND. iFull/=0 .AND. DeltaZ/=0 .AND. b2==0) THEN
    
    step = ((b2_max-b2_min)/isteps)
    iPerformance = 5
    
    WRITE(10,*) '                          Carleton University'
    WRITE(10,*) '           Department of Mechanical and Aerospace Engineering'
    WRITE(10,*) '                      OUTLET PASSAGE WIDTH ANALYSIS'
    WRITE(10,*) '                          Gas Turbine Project'
    WRITE(10,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    WRITE(10,*) ' b2     M2_rel      dH_tip     dH_hub     dH_m      Phi    Work Co.  Efficiency'
    
    DO m=0,isteps
        
        b2 = b2_min + step*m
        CALL PERFORMANCE()
    
    END DO
    
    CLOSE(10)
    WRITE(*,*)
    WRITE(*,*) "Analysis Complete! Please See Results File."
    WRITE(*,*) "Press Any Key To Continue..."
    READ(*,*)
    b2 = 0
    
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

COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDVAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2guess
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
    A1=PI*(r1s**2)-PI*(r1h**2)-(r1s-r1h)*tb1*Z!Rotor Inlet Area (m^2)
    r1m=(0.5*(r1s**2)+0.5*(r1h**2))**0.5!Inlet Meanline Radius (m)
    D1=r1m*2
    b1 = r1s - r1h

    !ROTOR OUTLET DIMENSIONS
    D2 = r2*2!Impeller Outlet Tip Diameter (m)
    A2=(2*PI*r2-(iFull+iSplitter)*tb)*b2!m^2

    !Lb = RadiusOfCurvature/(Alpha2-Alpha1) !Another option is Aungier, who uses the radius of curvature (rad/m) something like this..

    !DESIGN POINT

    CALL TPFLSH (T01,P01,x,d,dl,dv,xliq,xvap,q,e,H01,S01,cv,cp,w,ierr,herr)
    R = rgas*1000/wm
    rho01=d*wm!kg/m^3
    U1=r1m*2*PI*N/60!Blade Speed (m/s)
    U2=r2*2*PI*N/60!Blade Speed (m/s)
    PRr=2.7!Rotor Pressure Ratio Guess
    Effr=0.9!Rotor Isentropic Efficiency Guess

    !Diffuser Dimensions
    !iSteps = 1000 !Number of steps in diffuser analysis
    !DiffuserLength = .0244
    !b3 = .00625
    !r3 = r2
    !r4 = r3+DiffuserLength
    !b4 = .0042

    !Volute Dimensions
    !VoluteExitDiameter = 0.045
    !EVolute = .0001
    !r5 = r4

    PRrLast =0
    k=0

    DO WHILE (ABS((PRr - PRrLast)/PRr) .GT. 0.0000000001)
        k=k+1
        PRrLast = PRr
        
        IF (k.GT.MaxIter) THEN
            WRITE(*,*) "Error 1.0. Max iteration reached."
        ENDIF

        !~~~~~~~~~~~~~~~~~~~~~~~~~~IMPELLER ANALYIS~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !The impeller analysis uses basic turbomachinery principles and
        !an optimum set of loss models (Oh et al.) to correct for real effects
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        kbeta = 0
        Beta2last = 100
        
        DO WHILE (ABS((Beta2 - Beta2last)/Beta2) .GT. 0.00000001)

            kbeta=kbeta+1
            Beta2last = Beta2
            
            IF (kbeta.GT.MaxIter) THEN
                WRITE(*,*) "Error 1.1. Max iteration reached."
            ENDIF
            
            !Estimate of mean camber line length (Currently an input)
            IF (ABS(Beta2) > 80 .AND.  ABS(Beta2) <= 90) THEN
              Beta2 = 50
            END IF
        
            Lb =  (DeltaZ - b2/2) + (D2 - D1)/(2*COS(Beta2*PI/180))!Sjolander
            Z = iFull + (LbSplitter/Lb)*iSplitter

                
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
                C1=zMdot/(rho1*A1*COS(Alpha1*PI/180))!Update C1
                IF (j.GT.MaxIter) THEN
                    WRITE(*,*) "Max iteration reached at Rotor Inlet"
                ELSE IF (ierr.NE.0) THEN
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
            Cu1 = C1 * SIN(Alpha1*PI/180)
            Wu1 = Cu1 - U1
            W1 = SQRT((Wu1)**2+(Cm1)**2)
            Beta1 = ACOS(Cm1/W1)*(180/PI)
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

         
            !~~~~~~~~~~~~~~~~~~~~~ROTOR OUTLET~~~~~~~~~~~~~~~~~~~!               
            EffrLast = 0
            jEFF = 0
            
            DO WHILE (ABS((Effr-EffrLast)/Effr).GT.0.00001)
                jEFF = jEFF+1
                EffrLast = Effr
                IF (jEFF.GT.MaxIter) THEN
                     WRITE(*,*) "Rotor Outlet Meanline - Max iteration Reached." 
                END IF
                
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
                DO WHILE (ABS((C2 - last)/C2) .GT. 0.000000001)
                    i=i+1
                    last=C2
                    C2=SQRT(Cr2**2+Cu2**2)
                    !Stagnation is an isentropic process
                    H2=H02-(C2**2)*wm/2000!J/mol
                    S2=S02
                    CALL HSFLSH (H2,S2,zz,T2,P2,rho2,Dl,Dv,x2,y,q,e,cv,cp,a_2,ierr,herr)
                    rho2=rho2*wm!kg/m^3
                    Cr2=zMdot/(rho2*A2)!Update Cr2
                    IF (i.GT.MaxIter) THEN
                        WRITE(*,*) "Max iteration Reached at Rotor Outlet Meanline"
                    ELSE IF (ierr.NE.0) THEN
                        WRITE(*,*) "Error Flag at Rotor Outlet Meanline: ",ierr
                        WRITE(*,*) "Error String: ",herr
                    END IF
                END DO
            
                !CONVERGED ROTOR OUTLET VELOCITY TRIANGLE
                C2=SQRT(Cr2**2+Cu2**2)
                Alpha2=ACOS(Cr2/C2)*(180/PI)
                Wu2=U2-Cu2
                W2=SQRT(Wu2**2+Cr2**2)
                Beta2=ACOS(Cr2/W2)*(180/PI)
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
            
            !Estimate Metal Angle Based on Slip Factor (Aungier 2000)
            DO WHILE (ABS((Sigma-SigmaLast)/Sigma).GT.0.00000001)
            
                !Sigma = 1 - ((PI/Z)*COS(Beta2*PI/180))/(1-phi*TAN(Beta2*PI/180))!(Stodola)
                !Sigma = 1 - (0.63*PI/Z)/(1-phi*TAN(Beta2*PI/180))!(Stanitz)
                Sigma = 1 - SQRT(ABS(SIN(Beta2*PI/180)))*SIN(Alpha2*PI/180)/Z**0.7!(Wiesner/Busemann)
                
                SigmaStar = SIN((19+0.2*Beta2)*PI/180)
                E_limit = (Sigma - SigmaStar)/(1 - SigmaStar)
                
                IF (r1m/r2 .GT. E_limit) THEN
                Sigma = Sigma*(1 - (((r1m/r2)-E_limit)/(1-E_limit))**((Beta2*PI/180)/10))
                END IF

                SigmaLast = Sigma
                phi=Cr2/U2
                Cu2i = Cu2/Sigma
                Wu2i =  U2 - Cu2i
                Beta2 = ATAN(Wu2i/Cr2)*180/PI
                Alpha2 = ATAN(Cu2i/Cr2)*(180/PI)
                
            END DO

            !FINAL ROTOR OUTLET VELOCITY TRIANGLE (with slip factor)
            Cu2= Cu2/Sigma
            Wu2 = U2-Cu2
            Beta2 = ATAN(Wu2/Cr2)*180/PI
            C2 = SQRT(Cr2**2+Cu2**2)
            W2=SQRT(Wu2**2+Cr2**2)
            Alpha2=ATAN(Cu2/Cr2)*(180/PI)
            workcoeff = (H02real-H01)/(U2**2*wm/1000)
            AungierFlowCoeff = zMdot/(rho01*PI*(r2**2)*U2)
            RotationalMach = U2/a_1

        END DO

        !~~~~~~~~~~~~~~~~~~~~IMPELLER PARASITIC LOSSES~~~~~~~~~~~~~~~~~~~~~~~!
        ! These are calculated using the method overviewed
        ! in S.Sanghera (2013) Gas Turbine DR.
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
        
        CALL TPFLSH (Tcl,Pcl,x,rhocl,dl,dv,xliq,xvap,q,e,h,s,cv,cp,w,ierr,herr)
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

        RecirculationWork = 0.000008*SINH(3.5*(Alpha2*PI/180)**3)*Df**2*U2**2

        !~~~~~~~~~~~~~~~ SUM PARASITIC LOSS COEFFICIENTS ~~~~~~~~~~~~~~~~~~~~~~!

        LossesParasitic = DiskFriction + LeakageWork + RecirculationWork

        IF (iDVAnalysis == 0) THEN
            PRs = P02/P01 !Rotor Pressure Ratio
            H02real = H02real + LossesParasitic*wm/1000 
            S02ideal = S01
            CALL PSFLSH (P02,S02ideal,zz,T02ideal,rho02ideal,Dl,Dv,x,y,q,e,H02ideal,cv,cp,w,ierr,herr)
            Effs = (H02ideal - H01)/(H02real - H01)
            
        END IF
        
        !~~~~~~~~~~~~~~~~~~ END OF IMPELLER ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~~~~~!
        
        IF (iDVAnalysis == 1) THEN
            PRr = P02/P01 !Rotor Pressure Ratio
            H02real = H02real + LossesParasitic*wm/1000 
            S02ideal = S01
            CALL PSFLSH (P02,S02ideal,zz,T02ideal,rho02ideal,Dl,Dv,x,y,q,e,H02ideal,cv,cp,w,ierr,herr)
            Effr = (H02ideal-H01)/(H02real - H01) !Rotor Efficiency
            
            !~~~~~~~~~~~~~~~~~~~~~~~~~~DIFFUSER ANALYIS~~~~~~~~~~~~~~~~~~~~~~~~~~!
            !The diffuser analysis currently just uses an inputted loss factor.
            !For a full analysis method see Aungier (2000), or Roberts (2003).
            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
            !
            !Assume flow properties at rotor outlet are the same as diffuser inlet
            Cu3 = Cu2
            Cr3 = Cr2
            r3 = r2
            H03 = H02real
            P03 = P02
            P3 = P2
            !Diffuser Outlet Conditions
            H04 = H03
            Cu4 = Cu3*r3/r4
            Wu4 = Wu3*r3/r4
            Cr4 = Cr3

            !Possible Diffuser Loss Correlations
            !!DiffusionFactor = -4.8597*(phi**3) + 6.782*(phi**2) - 3.2895*phi + 0.7948 !Johnston & Dean (1996)
            !!DiffusionFactor = 0.0119*Alpha3 - 0.7035 !Yingkang & Sjolander Rotor A
            !!DiffusionFactor = 0.0004*Alpha3**2 - 0.046*Alpha3 + 1.6704 !Yingkang & Sjolander Rotor B
            !!DiffusionFactor = 0.0088*Alpha3 - 0.4924 !Yingkang & Sjolander Rotor C
            !!DiffusionFactor = 0.0148*Alpha3 - 0.8952 !Yingkang & Sjolander Rotor D
            !!DiffusionFactor = 0.0171*Alpha3 - 1.0389 !Yingkang & Sjolander Rotor E
            !!DiffusionFactor = 0.0002*(Alpha3**2) - 0.0242*Alpha3 + 0.6283 +1 

            P04 = P03 - DiffuserLossFactor*(P03-P3)!kPa
            CALL PHFLSH (P04,H04,zz,T04,rho04,Dl,Dv,x,y,q,e,S04,cv,cp,w,ierr,herr)

            last=0
            i=0
            
            DO WHILE (ABS((Cr4 - last)/Cr4) .GT. 0.000000001)
                i=i+1
                last=Cr4
                C4=SQRT(Cr4**2+Cu4**2)
                !Stagnation is an isentropic process
                H4=H04-(C4**2)*wm/2000!J/mol
                S4=S04
                CALL HSFLSH (H4,S4,zz,T4,P4,rho4,Dl,Dv,x,y,q,e,cv,cp,a_4,ierr,herr)
                rho4 = rho4*wm!kg/m^3
                Cr4 = zMdot/(rho4*(2*PI*r4*b4))!Update Cm4
                IF (i.GT.MaxIter) THEN
                    WRITE(*,*) "Max iteration reached at Diffuser Outlet"
                ELSE IF (ierr.NE.0) THEN
                    WRITE(*,*) "Error Flag at Diffuser Outlet: ",ierr
                    WRITE(*,*) "Error String: ",herr      
                END IF
            END DO

            C4=SQRT(Cr4**2+Cu4**2)
            Alpha4=ATAN(Cu4/Cr4)*(180/PI)
            Beta4=ATAN(Wu4/Cr4)*(180/PI)
            W4=SQRT(Wu4**2+Cr4**2)
            aM4=C4/a_4!Absolute Mach Number
            rM4=W4/a_4!Relative Mach Number
                        
            !Isentropic efficiency for P01 to P04
            !PRd = P04/P01
            !S04ideal = S01
            !CALL PSFLSH (P04,S04ideal,zz,T04ideal,rho04ideal,Dl,Dv,x,y,q,e,H04ideal,cv,cp,w,ierr,herr)
            !Effd = (H04ideal-H01)/(H04 - H01)
                
            !~~~~~~~~~~~~~~~~~~~~~ END OF DIFFUSER ANALYIS ~~~~~~~~~~~~~~~~~~~~~~!    
                
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
            H06real = H06 + LossesParasitic*wm/1000 !(Add losses parasitic at the end so it doesn't effect aerodynamic properties)
            Effs = (H06ideal-H01)/(H06real - H01)
        END IF
        
        PRr = PRr*(Target_PR/PRs) !Update Rotor Pressure Ratio Until Stage Pressure Ratio Converges
        
    END DO

!~~~~~~~~~~~~~~~~~~~~~~~~~~ OUTPUT RESULTS ~~~~~~~~~~~~~~~~~~~~~~~~~~!
!Output is based on what is set in the input text file.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
IF(iPerformance == 0) THEN
12 FORMAT (A47,I8)    !I = Integer, Field Width = 8
13 FORMAT (A47,F14.7) !F = Real, Field Width = 14, and 7 Decimal Places
14 FORMAT (A47,I3)    !I = Integer, Field Width = 2
11 FORMAT (A35,F14.7,A12) !F = Real, Field Width = 14, and 7 Decimal Places
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~~~COMPRESSOR DESIGN~~~~~~~~~~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,12) "Compressor Rotational Speed [RPM].............|",N
    WRITE (10,13) "Mass Flow Rate [kg/s].........................|",zMdot
    WRITE (10,*) 
    WRITE (10,14) "Number of Full Blades.........................|",iFull
    WRITE (10,14) "Number of Splitter Blades.....................|",iSplitter
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
    IF (iDVAnalysis == 1) THEN
        WRITE (10,13) "Diffusion Loss Factor ....................|",DiffuserLossFactor
        WRITE (10,13) "Diffuser Exit Radius [m]..................|",r4
        WRITE (10,13) "Diffuser Exit Passage Width [m]...........|",b4
        WRITE (10,13)
        WRITE (10,13) "Volute Loss Factor .......................|",VoluteLossFactor
        WRITE (10,13) "Volute Exit Radius [m]....................|",r6
        WRITE (10,13) "Volute Exit Passage Width [m].............|",b6
        WRITE (10,*)
    END IF
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~ROTOR INLET THERMODYNAMIC PROPERTIES~~~~~~~~~~~~"
    WRITE (10,*)
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
    WRITE(10,*)
    !WRITE (10,*) "Ideal Enthalpy :         ",H02ideal*1000/wm," J/kg"
    !WRITE (10,*) "____________________________________________________"
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~~~IMPELLER LOSSES~~~~~~~~~~~~~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,11) "Incidence Loss:                        ", LossIncidence," J/kg"
    WRITE (10,11) "Blade Loading Loss:                    ", BladeLoadingLoss," J/kg"
    WRITE (10,11) "Skin Friction Loss:                    ", SkinFrictionLoss," J/kg"
    WRITE (10,11) "Tip Clearance Loss:                    ", TipClearanceLoss," J/kg"
    WRITE (10,11) "Mixing Loss:                           ", LossMixing," J/kg"
    WRITE (10,11) "Disk Friction Loss:                    ", DiskFriction," J/kg"
    WRITE (10,11) "Leakage Work Loss:                     ", LeakageWork," J/kg"
    WRITE (10,11) "Recirculation Work Loss:               ", RecirculationWork," J/kg"
    WRITE (10,*)
    WRITE (10,11) "Internal Loss Total:                   ", LossesInternal," J/kg"
    WRITE (10,11) "Parasitic Loss Total:                  ", LossesParasitic," J/kg"
    WRITE (10,*)
    WRITE (10,*) "~~~~~~~~~~~~~ROTOR OUTLET THERMODYNAMIC PROPERTIES~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,11) "Total Enthalpy (H02):                  ",H02/wm," kJ/kg"
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
    WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~~AERODYNAMIC PROPERTIES~~~~~~~~~~~~~~~~~~"  
    WRITE(10,*)
    WRITE (10,11) "Inlet Relative Velocity (hub):         ",W1h
    WRITE (10,11) "Inlet Relative Velocity (meanline):    ",W1
    WRITE (10,11) "Inlet Relative Velocity (shroud):      ",W1s
    WRITE (10,11) "Inlet Swirl Angle [deg]                ",Alpha1," deg"
    WRITE (10,*)
    WRITE (10,11) "Inlet Mach Number (hub):               ",zM1relh
    WRITE (10,11) "Inlet Mach Number (meanline):          ",zM1rel
    WRITE (10,11) "Inlet Mach Number (shroud):            ",zM1rels
    WRITE (10,*)
    WRITE (10,11) "Inlet Flow Angle (hub):                ",Beta1h," deg"
    WRITE (10,11) "Inlet Flow Angle (meanline):           ",Beta1," deg"
    WRITE (10,11) "Inlet Flow Angle (shroud):             ",Beta1s," deg"
    WRITE (10,*)
    WRITE (10,11) "Outlet Flow Angle (beta2m):            ",Beta2," deg"
    WRITE (10,11) "Swirl Angle (Alpha 2):                 ",Alpha2," deg"
    WRITE (10,11) "Absolute Velocity Tangential (Cu2):    ",Cu2," m/s"
    WRITE (10,11) "Absolute Velocity (C2):                ",C2," m/s"
    WRITE (10,11) "Relative Velocity Tangential (Wu2):    ",Wu2," m/s"
    WRITE (10,11) "Relative Velocity (W2):                ",W2," m/s"
    WRITE (10,11) "Outlet Relative Mach Number (m):       ",rM2
    WRITE(10,*)
    WRITE (10,*) "~~~~~~~~~~~~~~~NON-DIMENSIONAL DESIGN PARAMETERS~~~~~~~~~~~~~"
    WRITE (10,*)
    WRITE (10,11) "de Haller at tip:                      ",dHt
    WRITE (10,11) "de Haller at hub:                      ",dHh
    WRITE (10,11) "de Haller at mer:                      ",dHm
    WRITE (10,11) "Flow Factor:                           ",phi
    WRITE (10,11) "Work Coefficient:                      ",workcoeff
    WRITE (10,11) "Flow Coefficient:                      ",AungierFlowCoeff 
    WRITE (10,11) "Rotational Mach Number:                ",RotationalMach
    IF (iDVAnalysis == 1) THEN
        WRITE (10,*) "~~~~~~~~~~~~~~~~~~~~DIFFUSER OUTLET CONDTIONS~~~~~~~~~~~~~~~~~~~"
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
        WRITE(10,11)
        WRITE(10,11) "C4:                                     ",C4," m/s"
        WRITE(10,11) "Cr4:                                    ",Cr4," m/s"
        WRITE(10,11) "Cu4:                                    ",Cu4," m/s"
        WRITE(10,11) "Alpha4:                                 ",Alpha4," deg"
        WRITE(10,*)
        WRITE (10,*) "~~~~~~~~~~~~~~~~~~~COMPRESSOR OUTLET CONDTIONS~~~~~~~~~~~~~~~~~~"
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
        WRITE(10,11) "C6:                                     ",C6," m/s"
        WRITE(10,11) "Cr6:                                    ",Cr6," m/s"
        WRITE(10,11) "Cu6:                                    ",Cu6," m/s"
        WRITE(10,11) "Alpha6:                                 ",Alpha6," deg"
    END IF
    WRITE (10,*)
    WRITE(10,*)"*************************************************************"
    WRITE(10,11) "COMPRESSOR PRESSURE RATIO:                  ",PRs
    WRITE(10,11) "COMPRESSOR EFFICIENCY:                      ",Effs
    WRITE(10,*)"*************************************************************"
    WRITE(10,*) 

END IF

IF (iPerformance == 1) THEN

    15 FORMAT (F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F7.6)
    17 FORMAT (A12,I3, A22,I3,A13)
    WRITE(10,15)r1h,zM1rels,rM2,dHt,dHh,dHm,phi,workcoeff,Effs
    WRITE(*,17) "Radius #",m," Complete In: ",k,"Iterations."

END IF

IF (iPerformance == 2) THEN

    25 FORMAT (F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F7.6)
    27 FORMAT (A12,I3, A22,I3,A13)
    WRITE(10,25)r1s,zM1rels,rM2,dHt,dHh,dHm,phi,workcoeff,Effs
    WRITE(*,27) "Solution #",m," Complete In: ",k,"Iterations."

END IF

IF (iPerformance == 3) THEN

    35 FORMAT (I3,10X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,3X,F7.6)
    37 FORMAT (I3,A30,I3,A13)
    WRITE(10,35)iFull,zM1rels,rM2,dHt,dHh,dHm,phi,workcoeff,Effs
    WRITE(*,37) m," Blades Solution Complete In: ",k,"Iterations."

END IF

IF (iPerformance == 4) THEN

    45 FORMAT (F8.6,5X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F7.6)
    47 FORMAT (A12,I3, A22,I3,A13)
    WRITE(10,45)DeltaZ,zM1rels,rM2,dHt,dHh,dHm,phi,workcoeff,Effs
    WRITE(*,47) "Solution #",m," Complete In: ",k,"Iterations."

END IF

IF (iPerformance == 5) THEN

    55 FORMAT (F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F8.6,2X,F7.6)
    57 FORMAT (A12,I3, A22,I3,A13)
    WRITE(10,55)b2,zM1rels,rM2,dHt,dHh,dHm,phi,workcoeff,Effs
    WRITE(*,57) "Solution #",m," Complete In: ",k,"Iterations."

END IF
    
END SUBROUTINE PERFORMANCE

!~~~~~~~~~~~~~~~~~~~~~~~~ END OF MEANLINE PERFORMANCE ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~!

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                          SUBROUTINE: Internal Losses
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE InternalLosses()

IMPLICIT DOUBLE PRECISION (a-h,l,o-z)
IMPLICIT INTEGER (i-k,m,n)

COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDVAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2guess
COMMON / Losses         / LossesInternal, LossesParasitic, cf, Sigma, ShockLoss, LossIncidence, &
                          DiffusionLoss, ChokingLoss, BladeLoadingLoss, HSLoadingLoss, &
                          SkinFrictionLoss, TipClearanceLoss, LossMixing, SupercritMachNumberLoss

    ! Calculate Skin Friction loss:  (Jansen)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Average Hydraulic Diameter
    PassageA1 = A1 * (ABS(1+COS(Beta1*PI/180))/2) / iFull
    ThroatArea = PassageA1-tb1*(r1s-r1h)

    IF (iSplitter.NE.0) THEN
        TipArea = b2*(2*PI*r2/iFull - (iFull+iSplitter)*tb) * COS(Beta2*PI/180)
        TipPerimeter = 4*(b2+2*PI*r2/(iFull+iSplitter))
    ELSE
        TipArea = b2*(2*PI*r2/iFull - (iFull)*tb) * COS(Beta2*PI/180)
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
        BladeWork = Sigma * (1+Lambda*phi2*TAN(Beta2*PI/180)) - U1*Cu1/U2**2
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
            
    BladeLoadingLoss = 0.05*(Df*U2)**2 !Corrected from 0.005

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
    
COMMON / Constant 	    / PI, Target_PR, isteps, iPerformance,iAir,iCarbonDioxide, iDVAnalysis
COMMON / Geometric 	    / r1h, r1s, r1m, r2, r3, r4, r6, A1, A2, tb1, tb, b1, b2, b3, b4, b6, &
                          LbImpeller, TipClearance, RoughnessImp, &
                          iFull, iSplitter, LbSplitter, Z, Lb, DeltaZ, CurveKm1
COMMON / Geometric2     / r1h_min, r1h_max, r1s_min, r1s_max, r2_min, r2_max                   
COMMON / Velocity 	    / U1, U2, C1, C2, Cu1, Cu2, Cm1, Cr2, W1, W2, W1s, W1h, Wu2
COMMON / Properties 	/ Visc1, Visc2, zMdot, N, MaxIter, m
COMMON / Thermodynamic	/ rho01, rho02, rho1, rho2, P01, P1, P02, P2, T01, T2, T1
COMMON / FlowAngles     / Beta1Prime, Alpha1, Alpha2, DiffuserLossFactor, VoluteLossFactor
COMMON / BladeAngles    / Beta1, Beta2, Beta2guess
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

!...And they all lived happily ever after... The End.