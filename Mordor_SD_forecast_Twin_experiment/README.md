Mordor Forecast Twin experiment

In the context of studies carried out with twin experiments, this code performed forecasts on each truth
by using old observations as unknown forcings in the MORDOR-SD model, for an analyse coming
from Mordor_SD_Assimilation_ensemble_Twin_experiment.py

By defining the start date of the simulation (which must start at least 2 years before assimilation
to consider a stabilization period), start of assimilation and end, this code will generate
N_prev simulations with old forcings


A NetCDF is built in ouptut conainting all forecasts, one file by truth.


##########
The Initialization interface (beginning line 258) groupes
All parameters to modify

# year of the end of assimilation
year = 2017

# date of beginning of simulation
d_ini_known   = str(year-3) + "-09-01"
# date of the end of assimilation
d_end_known   = str(year)   + "-03-31"

# Nb of truth
N_verites   = 12

# file with initial condition
save_file_n = "Assimilation_sans_FP_"+str(year)

###### forecast
# Nb of old years to use as unknown forcings
N_prev=50

#fisrt year of old forcings
first_year = 1968

# start and end dates of forcast
date_deb_fcst = "04-01"
date_fin_fcst = "08-31"

# NetCdf output file name
save_file_out = "fichier_sauv_previ_assimQ_"+str(year)



# # Parameters and observations files
###
data       = "QPThisto_J_isere@bvi_coche_ponserand_valid.txt"
data2      = "SWE_NRC_NDAout.txt"
file_param = "isere_FINAL_J_SIM.res"#"FINAL_J_SIM.res"



# parameters reading
pmtdict = res2pmt(file_param)

# output mode of Mordor (specific to the wrapper)
pmtdict["mod_out"] = 3


#############

As ouptut, the codes will create a NetCDF (one by true state) file containing:
(of name save_file_n + "_" + str(i_ver) + ".nc" with i_ver the number of the true state)
-all simulations performed by the model for each particle (Q, S, S on each level, fneige)


Figure with observed flow rate and resampled flow rate is also built.




