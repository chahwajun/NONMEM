
$PROBLEM SB15 VIT AQ FOCE like 2 comp by MID
$INPUT IDD	TIME	AMT	DV	MDV	LEFT	CMT	SITE	DRUG	DAY	WEEK	BLQ	TYPE	DOSE	GROUP	ROWC	ROWD	ROWNN	NID	ID	
$DATA SB15_VAROIS_COMBID_DOSE_SERUM_NEW.csv IGNORE=@  IGNORE(TYPE.EQ.3) 

$SUBROUTINE ADVAN13 TOL=3

$MODEL
  NCOMP=6
  COMP=(VIT)        
  COMP=(AQ)         
  COMP=(RET)        
  COMP=(SERUM)      
  COMP=(OPT)        
  COMP=(IRIS)       

$PK

  CL1 = THETA(1) * EXP(ETA(1))   
  V1  = THETA(2) * EXP(ETA(2))   
  Q12 = THETA(3) * EXP(ETA(3))   
  V2  = THETA(4) * EXP(ETA(4))   
  Q13 = THETA(5) * EXP(ETA(5))   
  V3  = THETA(6) * EXP(ETA(6))   
  Q24 = THETA(7) * EXP(ETA(7))   
  V4  = THETA(8) * EXP(ETA(8))   
  Q34 = THETA(9) * EXP(ETA(9))   
  CL4 = THETA(10) * EXP(ETA(10)) 
  Q14 = THETA(11) * EXP(ETA(11)) 
  
  Q41 = THETA(12) * EXP(ETA(12)) 
  
  Q15 = THETA(13) * EXP(ETA(13)) 
  V5  = THETA(14) * EXP(ETA(14)) 
  Q54 = THETA(15) * EXP(ETA(15)) 
  
  Q16 = THETA(16) * EXP(ETA(16)) 
  V6  = THETA(17) * EXP(ETA(17)) 
  Q26 = THETA(18) * EXP(ETA(18)) 
  Q62 = THETA(19) * EXP(ETA(19)) 
  
  Q42 = THETA(20) * EXP(ETA(20)) 

  S1 = V1 / 1000   
  S2 = V2 / 1000   
  S3 = V3 / 1000   
  S4 = V4 / 1000   
  S5 = V5 / 1000   
  S6 = V6 / 1000   


$DES
  K10 = CL1 / V1   
  K12 = Q12 / V1   
  K21 = Q12 / V2   
  K13 = Q13 / V1   
  K31 = Q13 / V3   
  K24 = Q24 / V2   
  K42 = Q42 / V4   
  K34 = Q34 / V3
  K40 = CL4 / V4   
  
  K14 = Q14/ V1    
  K41 = Q41/ V4    
  
  K15 = Q15/ V1    
  K51 = Q15/ V5    
  
  K54 = Q54/ V5    

  K16 = Q16 / V1   
  K61 = Q16 / V6   
    
  K26 = Q26 / V2   
  K62 = Q62 / V6   
  
 

  DADT(1) = - (K10 + K12 + K13 + K14 + K15 + K16) * A(1) + K21 * A(2) + K31 * A(3) + K41 * A(4) + K51 * A(5) + K61 * A(6)
  DADT(2) = K12 * A(1) - (K21 + K24 + K26) * A(2) + K42 * A(4) + K62 * A(6)
  DADT(3) = K13 * A(1) - K31 * A(3) - K34 * A(3)
  DADT(4) = (K24 * A(2) + K34 * A(3) + K14 * A(1)) - (K40 * A(4) + K42 * A(4)) + K54 * A(5) 

  DADT(5) = K15 * A(1) - (K51 * A(5) + K54 * A(5))

  DADT(6) = K16 * A(1) + K26 * A(2) - (K61 + K62) * A(6)


  
  

$ERROR
  IF (CMT.EQ.1) THEN
    IPRED  = F                
    W      = SQRT(THETA(21)**2 + (THETA(22) * IPRED)**2)
    IRES   = DV - IPRED         
    IWRES  = IRES / W           
    Y      = IPRED + W * EPS(1) 
  ENDIF

  IF (CMT.EQ.2) THEN
    IPRED  = F                 
    W      = SQRT(THETA(23)**2 + (THETA(24) * IPRED)**2)  
    IRES   = DV - IPRED         
    IWRES  = IRES / W           
    Y      = IPRED + W * EPS(2) 
  ENDIF

  IF (CMT.EQ.3) THEN
    IPRED  = F                  
    W      = SQRT(THETA(25)**2 + (THETA(26) * IPRED)**2)  
    IRES   = DV - IPRED         
    IWRES  = IRES / W          
    Y      = IPRED + W * EPS(3) 
  ENDIF

  IF (CMT.EQ.4) THEN
    IPRED  = F                  
    W      = SQRT(THETA(27)**2 + (THETA(28) * IPRED)**2)  
    IRES   = DV - IPRED         
    IWRES  = IRES / W           
    Y      = IPRED + W * EPS(4) 
  ENDIF

  IF (CMT.EQ.5) THEN
    IPRED  = F                  
    W      = SQRT(THETA(29)**2 + (THETA(30) * IPRED)**2)  
    IRES   = DV - IPRED         
    IWRES  = IRES / W           
    Y      = IPRED + W * EPS(5) 
  ENDIF
  
  
  IF (CMT.EQ.6) THEN
  IPRED  = F              
  W      = SQRT(THETA(31)**2 + (THETA(32) * IPRED)**2)  
  IRES   = DV - IPRED         
  IWRES  = IRES / W           
  Y      = IPRED + W * EPS(6) 
 ENDIF

$THETA
0 FIX         
0.0015   FIX    
6.00E-06 FIX  
0.000287 FIX  
6.06E-07 FIX  
              
6.86E-05  FIX 
(0, 0.0001)   
1.3 FIX       
1.84E-06 FIX  
8.81E-03 FIX 
1.94E-06    
        
 1.24E-07 
 
 (0, 0.000001)                                                                   
 0.0002 FIX    
 (0, 0.00001)  
 
 (0, 0.00000001)
 (0, 0.00001)    
 (0, 0.0000005) 
 (0, 0.000001)  
 
 (0, 0.00005)       
        
              
 10 
 (0, 0.3)   
 10
 (0, 0.3)   
 10
 (0, 0.3)   
 5  
 (0, 0.3)    
 
 10
 (0, 0.3)   
 
 10
 (0, 0.3)    
 


 


$OMEGA
  0 FIX 
  0 FIX 
  0 FIX 
  0 FIX 
  0 FIX 
  
  0 FIX 
  0 FIX 
  0 FIX 
  0 FIX
  0 FIX
  
  0 FIX
  0 FIX
  0 FIX
  0 FIX
  0 FIX
  
  0 FIX
  0 FIX
  0 FIX
  0 FIX
  0 FIX

$SIGMA
  0.2
  0.2
  0.2
  0.2
  0.2
  0.2


$ESTIMATION  MAXEVAL=9999 PRINT=10 SIGDIGITS=2  METHOD=1 INTER ;METHOD=0 


$TABLE ID	TIME	AMT	DV	MDV	LEFT	CMT	SITE	DRUG	DAY	WEEK	BLQ	TYPE	DOSE	GROUP	ROWC	ROWD	ROWNN	NID	IDD	PRED IPRED CWRES IWRES     ONEHEADER NOPRINT FILE = sdtab190
