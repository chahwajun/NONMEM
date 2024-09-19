;; 1. Based on: 1
$PROBLEM SB vitreous 2comp

$INPUT ID	TIME	DV	MDV	CMT	AMT	GROUP=SKIP	LEFT	SITE	DAY	WEEK	ID2=SKIP	IDD=SKIP


$DATA SB.csv IGNORE=@

$SUBROUTINES ADVAN3 TRANS4


$PK
 CL   = THETA(1)*EXP(ETA(1))
 V1   = THETA(2)*EXP(ETA(2))
 Q    = THETA(3)*EXP(ETA(3))
 V2   = THETA(4)*EXP(ETA(4))
 
 S1   = V1/1000   ; dose: mg   V: ng/mL  
 
$ERROR

  IPRED   = F
  W       = SQRT(THETA(5)**2 + THETA(6)**2 * IPRED**2)
  IRES    = DV - IPRED
  IWRES   = IRES / W
  Y       = IPRED + W * EPS(1)

  
$THETA
 (0,0.000004)      ; CL
 (0,0.0004)         ; V1
 (0,0.000002)      ; Q
 (0,0.0001)       ; V2
 0.000001  FIX  ; Additive Residual Variability
 (0,0.1)        ; Proportional Residual Variability
$OMEGA 
0 FIX
0 FIX
0 FIX
0 FIX
$SIGMA 1 FIX 

$COVARIANCE PRINT = E

$ESTIMATION NOABORT MAXEVALS=9999 METHOD=1 INTER PRINT = 10


$TABLE ID TIME DV MDV CMT AMT LEFT SITE DAY WEEK IWRES IRES IPRED CWRES                                          NOPRINT ONEHEADER          FILE=sdtab20
$TABLE ID CL V1 V2 Q ETA1 ETA2 ETA3 ETA4                                                                                         NOPRINT ONEHEADER NOAPPEND FILE=patab20
