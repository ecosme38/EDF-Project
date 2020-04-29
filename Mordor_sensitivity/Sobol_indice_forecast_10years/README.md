Sobol_indice_forecast_10years

This code compute the sensitivity of the MORDOR model to the snow amount (Sobol indice)
by using simulations over 10 years.

following the Saltelli method (Saltelli, 2002)
#########

A loop over ten years (starting at line 276) and beginning in 2008
will perturb snow amount in initial condition (end of winter simulation on 31th march).
Perturbations are made only on the 10 snow amount on each level (in function make_perturb, line 219).
Uniforms perturbations are done, relatively to the initial snow amount. This relative (in %) value
is given by "pourcent" variable (line 239).

This perturbed initialisation is used to built A matrice, B matrice and C matrice
following the Saltelli method (Saltelli, 2002).
shapes of the matrices :
A[K, Ne]
B[K, Ne]
C[K, K, Ne]
with K the nb of initial conditions (72), and Ne the number of perturbations.

Then runs (with known forcings, as in winter) are done in summer (from first April to 31th August)
using initial conditions provided by the 3 matrices:
-Ne simulations with A
-Ne simulations with B
-K*Ne simulations with C

Using the different results of cumulative water flow simulated (and equations provided bu Saltelli, 2002),
variances of Mordor results due to perturbations on each initial conditions parameter is calculate (var_i[K])
and the mean (calculated with the mean of simulations done with the A matrice), mean_i[K]

Values are stored in a NetCdf file (one by year), of name save_file_n + "_" + str(annee_previ) + ".nc"

Finally, fast plot of the (np.sqrt(var_i[ind]) / mean_i[ind]) of the last year is shown.

