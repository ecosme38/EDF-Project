Simulation ensemble for twin experiments

In the context of studies carried out with twin experiments, this code generates 
the starting set which will be used as a set of truths for assimilation 
(using the code Mordor_SD_Assimilation_PF_Twin_experiment.py).

This set is saved in an output NetCDF, which can be read 
as input to Mordor_SD_Assimilation_PF_Twin_experiment.py.


##########
The Initialization interface (beginning line 208) groupes
All parameters to modify

# year of the end of assimilation
year = 2017

# date of start and end 
d_ini_known   = str(year-3) + "-09-01"
d_end_known   = str(year)   + "-08-31"

#parameters used for AR1
phi_P= 0.25
phi_T= 0.95
# variance of perturbations 0.18
std_P= 0.35 * 1.
std_T= 0.55 * 1.

# Ensemble size
N_p = 50 #50
###


#output filename (which will contain the outputs of Mordor)

save_file_n = "Ensemble_50_verites_"+str(year)+".nc"


# parameters reading
pmtdict = res2pmt(file_param)

# output mode of Mordor (specific to the wrapper)
pmtdict["mod_out"] = 3

#######

In ouput, the ensemble is stored in a NetCDF file with name save_file_n.

And several figures showing Q, S, PS and TS of the ensemble are done.

