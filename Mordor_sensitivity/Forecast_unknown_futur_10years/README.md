forecast_unknown_futur_10years

Created and from Sobol_indice_forecast_10years.py,

This code compute the sensitivity of the MORDOR model to unknown forcings
by using simulations over 10 years.

#########

A loop over ten years will simulates unknown futur over 10 differents years (first in 2008).
Unknown forcings (in summer) come from 50 old forcings (1958 to 2007).

Cumulative flow rate on summer period is calculated for each simulations (50),
and all outputs simulations are stored in a NetCdf file of name save_file_n + "_" + str(annee_previ) + ".nc"
(one file by year).



