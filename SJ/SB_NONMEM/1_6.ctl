;; 1. Based on: 1
$PROBLEM SB vitreous with IIV CL without error
;; 2. Description: SB vitreous with IIV CL without error

$INPUT ID	TIME	DV	MDV	CMT	AMT	GROUP=SKIP	LEFT	SITE	DAY	WEEK	ID2=SKIP	IDD=SKIP


$DATA SB.csv IGNORE=@

$SUBROUTINES ADVAN1 TRANS2

$PK
 CL   = THETA(1)*EXP(ETA(1))
 V    = THETA(2)*EXP(ETA(2))
 
 S1   = v/1000   ; dose: mg   V: ng/mL  
 
$ERROR

  IPRED   = F
  W       = SQRT(THETA(3)**2 + THETA(4)**2 * IPRED**2)
  IRES    = DV - IPRED
  IWRES   = IRES / W
  Y       = IPRED + W * EPS(1)

  
$THETA
 (0,0.000004)      ; CL
 (0,0.0004)         ; V
 0.000001  FIX  ; Additive Residual Variability
 0.000001 FIX        ; Proportional Residual Variability
$OMEGA 
0.1
0 FIX

$SIGMA 1 FIX 

$COVARIANCE PRINT = E

$ESTIMATION NOABORT MAXEVALS=9999 METHOD=1 INTER PRINT = 10


$TABLE ID TIME DV MDV CMT AMT LEFT SITE DAY WEEK IWRES IRES IPRED CWRES                                          NOPRINT ONEHEADER          FILE=sdtab6
$TABLE ID CL V ETA1 ETA2                                                                                        NOPRINT ONEHEADER NOAPPEND FILE=patab6
