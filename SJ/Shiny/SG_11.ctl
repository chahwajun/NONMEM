$PROBLEM SB15 GC100.ctl -> human + SB15 323.ctl human sim1
;$INPUT IDD	TIME	AAMT	DV	MDV	LEFT	CMT	SITE	DRUG	DAY	WEEK	BLQ	TYPE	DOSE	GROUP	ROWC	ROWD	ROWNN	NID	ID	LGAMT	LOGDOSE	AMT LNDOSE ROW	;GGROUP
$INPUT ID	TIME	AAMT	DV	MDV	LEFT	CMT	SITE	DRUG	DAY	WEEK	BLQ	TYPE	DOSE	GROUP	LGAMT	LOGDOSE	AMT	LNDOSE	ROW
$DATA ..//G001_HSIM_2.csv IGNORE=@ ;IGNORE(GROUP.EQ.8) 

$SUBROUTINE ADVAN13 TOL=3

$MODEL 
  ;NCOMP=10
  COMP=(VIT_GC)      ; Vitreous_GC (Dosing compartment)
  COMP=(RET_GC)      ; Retina_GC
  COMP=(OPT_GC)      ; Optic Nerve_GC
  COMP=(IRIS_GC)     ; Iris_GC

  COMP=(VIT)         ; Vitreous
  COMP=(AQ)          ; Aqueous
  COMP=(RET)         ; Retina
  COMP=(SERUM)       ; Serum
  COMP=(OPT)         ; Optic nerve
  COMP=(IRIS)        ; Iris  
  
  COMP=(TRAN1)      ;RET TRANSIT
  COMP=(TRAN2)      ;VIT TRANSIT
  COMP=(TRAN3)      ;IRIS TRANSIT

$PK
;GENE COPY 
  ; Inter-compartmental clearances and volumes of distribution
  Q12 = THETA(1) ;* EXP(ETA(1))  ; Clearance between VIT and RET
  Q21 = THETA(2) ;* EXP(ETA(1))  ; Clearance between VIT and RET
  Q23 = THETA(3) ;* EXP(ETA(2))  ; Clearance between RET and OPT
  Q14 = THETA(4) ;* EXP(ETA(3))  ; Clearance between VIT and IRIS
  Q41 = THETA(5) ;* EXP(ETA(4))  ; Clearance between VIT and IRIS
  Q32 = THETA(6) ;* EXP(ETA(5))  ; Clearance between OPT and RET


  ; Volumes of distribution for each compartment
  V1  = THETA(7) ;* EXP(ETA(6))  ; Volume of distribution for VIT
  V2  = THETA(8) ;* EXP(ETA(7))  ; Volume of distribution for RET
  V3  = THETA(9) ;* EXP(ETA(8))  ; Volume of distribution for OPT
  V4  = THETA(10) ;* EXP(ETA(9))  ; Volume of distribution for IRIS

 ; Define bioavailability scaling factors for each dose group

  ;F1 = 0.0625*LNDOSE - 1.1177  ;GC100.CTL r2=0.8525
  F1 = 0.0633*LNDOSE - 1.1238   ;G 492.CTL r2=0.9345 (est G5 F1)


;SB15
  ; Parameter definitions for each compartment
  ;CL1 = THETA(16) * EXP(ETA(1))   ; Clearance from the VIT compartment
  V5  = THETA(11); * EXP(ETA(2))   ; Volume of distribution in the VIT compartment
  Q56 = THETA(12) ;* EXP(ETA(3))   ; Inter-compartmental clearance between VIT and AQ
  V6  = THETA(13) ;* EXP(ETA(4))   ; Volume of distribution in the AQ compartment
  Q57 = THETA(14) ;* EXP(ETA(5))   ; Inter-compartmental clearance between VIT and RET
  V7  = THETA(15) ;* EXP(ETA(6))   ; Volume of distribution in the RET compartment
  Q68 = THETA(16) ;* EXP(ETA(7))   ; CL from AQ to SERUM
  V8  = THETA(17) ;* EXP(ETA(8))   ; Volume of distribution in the SERUM compartment
  Q78 = THETA(18) ;* EXP(ETA(9))   ; CL from RET to SERUM 
  CL8 = THETA(19); * EXP(ETA(10)) ; First-order clearance from the SERUM compartment
  Q58 = THETA(20); * EXP(ETA(11)) ; Inter-compartmental clearance between VIT and SERUM
  Q85 = THETA(21) ;* EXP(ETA(12)) ; Inter-compartmental clearance between VIT and SERUM
  Q79 = THETA(22) ;* EXP(ETA(13)) ; Inter-compartmental clearance between RET and OPT;;
  V9  = THETA(23) ;* EXP(ETA(14)) ; Volume of distribution in the OPT compartment
  Q98 = THETA(24) ;* EXP(ETA(15)) ; CL from OPT to SERUM 
  Q510= THETA(25) ;* EXP(ETA(16)) ; Inter-compartmental clearance between VIT and IRIS
  V10 = THETA(26) ;* EXP(ETA(17)) ; Volume of distribution in the IRIS compartment
  Q610= THETA(27) ;* EXP(ETA(18)) ; CL from AQ and IRIS
  Q106= THETA(28) ;* EXP(ETA(19)) ; CL from IRIS to AQ
  Q86 = THETA(29) ;* EXP(ETA(20)) ; CL from SERUM to AQ
              
  Q108 = THETA(30); * EXP(ETA(21)) ; CL from IRIS to SERUM 
  Q97  = THETA(31) ;* EXP(ETA(13)) ; Inter-compartmental clearance between RET and OPT;;
              
  Q65  = THETA(32) ;* EXP(ETA(3))  ; CL from OPT to RET 
  Q75  = THETA(33) ;* EXP(ETA(5))  ; CL from OPT to RET 
  Q105 = THETA(34) ;* EXP(ETA(16)) ; CL from OPT to RET 


  ; Clearance for each compartment
  KTR3  = THETA(35) * EXP(ETA(1))  ;VIT  ;CL15 = THETA(36) * EXP(ETA(1))  ; Clearance from VIT compartment
  KTR   = THETA(36) * EXP(ETA(2))  ;RET  ;CL27 = THETA(37) * EXP(ETA(2))  ; Clearance from RET compartment
  CL39  = THETA(37) * EXP(ETA(3))  ;OPT                                   ; Clearance from OPT compartment
  KTR5  = THETA(38) * EXP(ETA(4))  ;IRIS ;CL410= THETA(39) * EXP(ETA(4))  ; Clearance from IRIS compartment  
  
  KTR2  = THETA(39) * EXP(ETA(2)) ;RET
  KTR4  = THETA(40) * EXP(ETA(1)) ;VIT
  KTR6  = THETA(41) * EXP(ETA(4)) ;IRIS



 ; Scaling factors for concentrations in each compartment
  S1 = V1/1000  ; Scaling factor for VIT
  S2 = V2/1000  ; Scaling factor for RET
  S3 = V3/1000  ; Scaling factor for OPT
  S4 = V4/1000  ; Scaling factor for IRIS
  ; Scaling factors for concentrations in each compartment
  S5 = V5/ 1000   ; Scaling factor for VIT compartment (ng/mL)
  S6 = V6/ 1000   ; Scaling factor for AQ compartment (ng/mL)
  S7 = V7/ 1000   ; Scaling factor for RET compartment (ng/mL)
  S8 = V8/ 1000   ; Scaling factor for SERUM compartment (ng/mL)
  S9 = V9/ 1000   ; Scaling factor for OPT compartment (ng/mL) 
  S10= V10/ 1000   ; Scaling factor for OPT compartment (ng/mL) 


  ; GC Rate constants calculated from inter-compartmental clearances and volumes
  K12 = Q12 / V1  
  K21 = Q21 / V2   
  K23 = Q23 / V2   
  K14 = Q14 / V1  
  K41 = Q41 / V4  
  K32 = Q32 / V3  
  
  K39 = CL39 / V3 ;K15 = CL15 / V1 ;K27 = CL27 / V2   ;K410= CL410/ V4


  ; SB15 Define rate constants
  ;K50 = CL5 / V5       ; Elimination rate constant from the VIT compartment
  K56 = Q56 / V5       ; Distribution rate constant from VIT to AQ                
  K57 = Q57 / V5       ; Distribution rate constant from VIT to RET               
  K68 = Q68 / V6       ; Distribution rate constant from AQ to SERUM              
  K78 = Q78 / V7       ; Distribution rate constant from RET to SERUM 
  
  K80 = CL8/ V8       ; First-order elimination rate constant from the SERUM compartment
  K58 = Q58/ V5        ; Distribution rate constant from VIT to SERUM  
  K85 = Q85/ V8        ; Distribution rate constant from SERUM to VIT  
  K79 = Q79/ V7        ; Distribution rate constant from VIT to OPT                 
 
  K98 = Q98/ V9        ; Distribution rate constant from OPT to SERUM
  K510 = Q510/ V5       ; Distribution rate constant from VIT to IRIS               
  
  K610 = Q610 / V6      ; Distribution rate constant from AQ to IRIS
  K106 = Q106 / V10     ; Distribution rate constant from IRIS to AQ
  
  K86 = Q86   / V8       ; Distribution rate constant from SERUM to AQ
  K108 = Q108 / V10     ; Distribution rate constant from IRIS to SERUM

  K97 = Q97/ V9        ; Distribution rate constant from OPT to SER  
  
  K65 = Q65 / V6       ; Distribution rate constant from AQ to VIT          <=CORRECTED 
  K75 = Q75 / V7       ; Distribution rate constant from RET to VIT
  K105= Q105/ V10      ; Distribution rate constant from IRIS to VIT 


$DES

  ;Gene copy model 
  DADT(1) = - (K12 + K14) * A(1) + K21 * A(2) + K41 * A(4) - KTR3*A(1)   ; VIT compartment
  DADT(2) = K12 * A(1) - K21 * A(2) - K23 * A(2) + K32 * A(3) - KTR*A(2) ; RET compartment
  DADT(3) = K23 * A(2) - (K39*0.00000001 + K32) * A(3)                   ; OPT compartment
  DADT(4) = K14 * A(1) - (K41) * A(4) - KTR5 * A(4)                      ; IRIS compartment

  DADT(11) = KTR*A(2) - KTR2*A(11)   ;RET TRANSIT
  DADT(12) = KTR3*A(1) - KTR4*A(12)  ;VIT TRANSIT
  DADT(13) = KTR5*A(4) - KTR6*A(13)  ;IRIS TRANSIT



  ;SB15 model 
  DADT(5) = - (K56 + K57 + K58 + K510) * A(5) + K65 * A(6) + K75 * A(7) + K85 * A(8) + K105 * A(10) + KTR4*A(12)
  DADT(6) = K56 * A(5) - (K65 + K68 + K610) * A(6) + K86 * A(8) + K106 * A(10)
  DADT(7) = K57 * A(5) - K75 * A(7) - K78 * A(7) - K79 * A(7) + K97 * A(9) + KTR2*A(11)
  DADT(8) = (K68 * A(6)  + K58 * A(5)) - (K80 * A(8) + K86 * A(8)) - K85 * A(8) + K78 * A(7)  + K98 * A(9) + K108 * A(10) 
  DADT(9) = K79 * A(7) - (K97 * A(9) + K98 * A(9)) + K39*0.00000001 * A(3)
  DADT(10) = K510 * A(5) + K610 * A(6) - (K105 + K106 + K108) * A(10) + KTR6*A(13)
  

$ERROR
  IF (CMT.EQ.1) THEN
    IPRED  = F                  ; Individual prediction for VIT
    W      = SQRT(THETA(42)**2 + (THETA(43) * IPRED)**2)  ; Combined error for VIT
    IRES   = DV - IPRED         ; Residuals for VIT
    IWRES  = IRES / W           ; Weighted residuals for VIT
    Y      = IPRED + W * EPS(1) ; Observed value for VIT
  ENDIF

  IF (CMT.EQ.2) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(44)**2 + (THETA(45) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(2) ; Observed value for AQ
  ENDIF

  IF (CMT.EQ.3) THEN
    IPRED  = F                  ; Individual prediction for VIT
    W      = SQRT(THETA(46)**2 + (THETA(47) * IPRED)**2)  ; Combined error for VIT
    IRES   = DV - IPRED         ; Residuals for VIT
    IWRES  = IRES / W           ; Weighted residuals for VIT
    Y      = IPRED + W * EPS(3) ; Observed value for VIT
  ENDIF

  IF (CMT.EQ.4) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(48)**2 + (THETA(49) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(4) ; Observed value for AQ
  ENDIF

  IF (CMT.EQ.5) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(50)**2 + (THETA(51) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(5) ; Observed value for AQ
  ENDIF


  IF (CMT.EQ.6) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(52)**2 + (THETA(53) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(6) ; Observed value for AQ
  ENDIF
  
  
  IF (CMT.EQ.7) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(54)**2 + (THETA(55) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(7) ; Observed value for AQ
  ENDIF
  
  
  IF (CMT.EQ.8) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(56)**2 + (THETA(57) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(8) ; Observed value for AQ
  ENDIF
  
  
  IF (CMT.EQ.9) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(58)**2 + (THETA(59) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(9) ; Observed value for AQ
  ENDIF
  
  
  IF (CMT.EQ.10) THEN
    IPRED  = F                  ; Individual prediction for AQ
    W      = SQRT(THETA(60)**2 + (THETA(61) * IPRED)**2)  ; Combined error for AQ
    IRES   = DV - IPRED         ; Residuals for AQ
    IWRES  = IRES / W           ; Weighted residuals for AQ
    Y      = IPRED + W * EPS(10) ; Observed value for AQ
  ENDIF
      


$THETA

;GC 100.CTL -> Human
0.000006 FIX;Q12   0.000002 	FIX  1
0.000020 FIX;Q21   0.000006 	FIX  
0.000060 FIX;Q23   0.000005 	FIX  
0.000015 FIX;Q14   0.000049 	FIX  
0.000482 FIX;Q41   0.000024 	FIX  
0.000239 FIX;Q32   0.001500 	FIX  

0.0045   FIX;V1    0.000069 	FIX  7
0.000686 FIX;V2    0.000043 	FIX  
0.000428 FIX;V3    0.000012 	FIX  
0.000118 FIX;V4    0.06 	FIX      



;SB15 323.CTL human /                  

0.0045        FIX     ;0.0045   FIX     ;V5   11
0.516000      FIX     ;0.000018 FIX     ;Q56       
0.000287      FIX     ;0.000287 FIX     ;V6        
0.000002      FIX     ;0.000003 FIX     ;Q57       
0.000686      FIX     ;0.000686 FIX     ;V7        
0.000044      FIX     ;0.000114 FIX     ;Q68       
3.37000       FIX     ;3.37000  FIX     ;V8        
0.000015      FIX     ;0.000010 FIX     ;Q78       
0.083600      FIX     ;0.022838 FIX     ;CL8       
0.000005      FIX     ;0.000009 FIX     ;Q58  20   
0.000000      FIX     ;0.000000 FIX     ;Q85       
0.000054      FIX     ;0.000098 FIX     ;Q79       
0.0000428     FIX     ;0.000428 FIX     ;V9        
0.000035      FIX     ;0.000040 FIX     ;Q98       
0.00000000030 FIX     ;0.00000000010 FIX;Q510      
0.000118      FIX     ;0.000118 FIX     ;V10       
0.000003      FIX     ;0.000038 FIX     ;Q610      
0.000007      FIX     ;0.000374 FIX     ;Q106      
0.000089      FIX     ;0.000303 FIX     ;Q86       
0.000007      FIX     ;0.000069 FIX     ;Q108 30   
0.000710      FIX     ;0.000091 FIX     ;Q97       
3.200000      FIX     ;0.000006 FIX     ;Q65       
0.000006      FIX     ;0.000010 FIX     ;Q75       
0.00000000100 FIX     ;0.00000000010    ;Q105    

0.0000060  FIX   ;KTR3 35
0.0001100  FIX   ;KTR 
0.00000025 FIX   ;CL39
0.0111000  FIX   ;KTR5
     
0.0008550  FIX   ;KTR2 39
0.0023500  FIX   ;KTR4
0.0008470  FIX   ;KTR6


;42-61
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop
 0 FIX ;0.0001 FIX  ;add 
 0 FIX ;(0, 0.3)    ;prop


 
$OMEGA
 0 FIX ;KTR3 VIT
 0 FIX ;KTR  RET
 0 FIX ;CL39 OPT
 0 FIX ;KTR5 IRIS
 
$SIGMA
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;
  0 FIX ;0.04;


$SIM (300000) nsub=1 ONLYSIM

$TABLE ID	TIME	AAMT	DV	MDV	LEFT	CMT	SITE	DRUG	DAY	WEEK	BLQ	TYPE	DOSE	GROUP	LGAMT	LOGDOSE	AMT	LNDOSE	ROW F1 ONEHEADER NOPRINT FILE = hsim11



;$ESTIMATION  MAXEVAL=9999 PRINT=10 SIGDIGITS=2  METHOD=1 INTER ;METHOD=0 ;
;;$COVARIANCE PRINT=E


;$TABLE ID	TIME	AMT	DV	MDV	LEFT	CMT	SITE	DRUG	DAY	WEEK	BLQ	TYPE	DOSE	GROUP	ROWC	ROWD	ROWNN	NID	IDD	LGAMT	LOGDOSE	LNDOSE ROW PRED IPRED CWRES IWRES     ONEHEADER NOPRINT FILE = sdtab492

;$TABLE ID CL1 CL2 CL3 CL4 Q12 Q13 Q14 V1 V2 V3 V4 MDV DOSE GROUP TYPE CMT                                                                             ONEHEADER NOPRINT NOAPPEND FILE = patab20

