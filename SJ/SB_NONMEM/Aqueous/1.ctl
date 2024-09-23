$PROBLEM SB aqueous 1comp with no IIV
;; 2. Description: SB aqueous 1comp with no IIV

$INPUT ID	TIME	DV	MDV	CMT	AMT	GROUP=SKIP	LEFT	SITE	DAY	WEEK IDD=SKIP


$DATA aqueous.csv IGNORE=@

$SUBROUTINES ADVAN1 TRANS2

$PK
 CL   = THETA(1)*EXP(ETA(1))
 V    = THETA(2)*EXP(ETA(2))
 
 S1   = V/1000   ; dose: mg   V: ng/mL  
 
$ERROR

  IPRED   = F
  W       = SQRT(THETA(3)**2 + THETA(4)**2 * IPRED**2)
  IRES    = DV - IPRED
  IWRES   = IRES / W
  Y       = IPRED + W * EPS(1)

  
$THETA
 (0,0.0002)      ; CL
 (0,0.004)        ; V
 0.000001  FIX  ; Additive Residual Variability
 (0,0.1)        ; Proportional Residual Variability
$OMEGA 
0 FIX
0 FIX

$SIGMA 1 FIX 

$COVARIANCE PRINT = E

$ESTIMATION NOABORT MAXEVALS=9999 METHOD=1 INTER PRINT = 10


$TABLE ID TIME DV MDV CMT AMT LEFT SITE DAY WEEK IWRES IRES IPRED CWRES                                          NOPRINT ONEHEADER          FILE=sdtab1
$TABLE ID CL V ETA1 ETA2                                                                                         NOPRINT ONEHEADER NOAPPEND FILE=patab1
