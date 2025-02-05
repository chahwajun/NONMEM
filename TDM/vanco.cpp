$PARAM 
t_CL = 2.82,   // Typical CL
  t_V1 = 31.8,   // Typical V1
  t_Q  = 11.7,   // Typical Q
  t_V2 = 75.4;   // Typical V2

$CMT central peri

$OMEGA @diagonal
0.685,   // between-subject variability for CL (ETA(1))
0.216;   // between-subject variability for V2 (ETA(2))

$SIGMA @diagonal
1e-06,   // additive residual error (EPS(1)) -> fixed near 0
0.284;   // proportional residual error (EPS(2))

$MAIN
// Compute individual parameters using exponentiation for log-normal variability:
double CL = t_CL * exp(ETA(1));
double V1 = t_V1;
double Q  = t_Q;
double V2 = t_V2 * exp(ETA(2));

// Calculate rate constants:
double k1 = Q / V1;
double k2 = Q / V2;
double ke = CL / V1;

$ODE
// Differential equations (DES):
dxdt_central = peri * k2 - central * (ke + k1);
dxdt_peri    = central * k1 - peri * k2;

$TABLE
// Model-predicted concentration:
double ipred = central / V1;
// Combined residual error model: additive + proportional error
double DV = ipred + EPS(1) + ipred * EPS(2);

// (필요시 추가로 캡처할 변수 지정 가능)
capture(ipred, DV);
