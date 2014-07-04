                     !This commented out section is an implementation of Aungier's "Diffusion Loss" correlation term based
                     !on 35 compressor stage tests.  Without it, the analysis is similar to Johnston & Dean, 1966.
                     
                    !Calculate Diffusion Losses (Reneau et al (1967))
                    D = -(b/C)*(C-CLast)/DeltaM
                    
                    !Correct using empricial diffusion efficiency model (Aungier (2000))
                    IF (D <= 0) THEN
                        Emp = 1
                    ELSEIF (D > 0 .AND. D < Dmax) THEN
                        Emp = 1 - 0.2*(D/Dmax)**2
                    ELSEIF (D >= Dmax) THEN
                        Emp = 0.8*SQRT(Dmax/D)
                    ENDIF

                    ! Streamwise Diffusion Loss term is then given by Aungier (2000)
                    dId = -2*(P0-PLast)*(1-Emp)*(1/(rhoLast*CLast))*(C-CLast) + dIdLast

                    !Check if higher losses are caused by excessive meridional gradient
                    rbMax = r3*b3*(1+0.16*CurrentLengthM/b3) !Maximum, stall free, local area Aungier (2000)
                    dIdMax = 0.65*(P0-PLast)*(1-(rbMax/(radius*b)))/rhoLast

                    !Integral of all diffusion losses (sum of previous dId's)
                    dIdSum = dIdSum + dId

                    IF (dIdMax <= dIdSum) THEN
                        DeltaId = dId
                    ELSEIF (dIdMax > dIdSum) THEN
                        DeltaId = dIdMax - dIdSum
                    END IF

                    !Calculate losses due to curvature
                    !Add dIc = K*(P0-P)*Cmax/(13*rho*C) for crossover bends Aungier (2000)
                    dIc = 0