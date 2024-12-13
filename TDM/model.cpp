$PROB
# One-compartment PK model with first-order absorption
# Designed for antimalarial drugs

$PARAM
KA = 0.5    // Absorption rate constant (1/hour)
CL = 5.0    // Clearance (L/hour)
V  = 50.0   // Volume of distribution (L)
F1 = 1.0    // Bioavailability for oral dosing (fraction)
Lag = 0.5   // Lag time (hours)

$CMT GUT CENT

$MAIN
// Elimination rate constant
double KEL = CL / V;

$ODE
// Differential equations for drug absorption and elimination
dxdt_GUT  = -KA * GUT;
dxdt_CENT =  KA * GUT - KEL * CENT;

$TABLE
double CP = CENT / V;  // Plasma concentration (mg/L)

$CAPTURE CP
