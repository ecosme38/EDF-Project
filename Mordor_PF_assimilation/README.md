Particle Filter Assimilation

This code performed Q and/or S assimilation by using Particle filter in the MORDOR-SD model.

By defining the start date of the simulation (which must start at least 2 years before assimilation
to consider a stabilization period), start of assimilation and end, this code will generate
N_p particles with perturbed forcings (with a auto-regressive model of order 1) 
which will be re-sample with a particulate filter.
This particle filter is based on weights for each particle, which are calculated with 
an RMSE between simulation and observation. The closer a simulation is to the observation, 
the higher the weight of the particle will be.

A NetCDF is built in ouptut conainting resampled forcing and the analysed MORDOR initialisation.


##########
The Initialization interface (beginning line 305) groupes
All parameters to modify

# year of the end of assimilation
year = 2017

# date of beginning of simulation
d_ini_known   = str(year-3) + "-09-01"

# date of the beginning of assimilation
d_ini_FP      = str(year-1) + "-12-01"
# date of the end of assimilation
d_end_known   = str(year)   + "-03-31"


# Error for weights calculation (ratio of obs values) of particles
R_Q = 0.05
R_S = 0.2

# size of assimilation windows (in days) 
days_step     = 10

# Parameters for AR1 model, phi of precipitation and temperature:
phi_P= 0.25
phi_T= 0.95

# Parameters for AR1 model, std of precipitation and temperature:
std_P= 0.35
std_T= 0.33

# Size of ensemble used
N_p = 50

# Duplicated particles will be perturbed with AR1 model
# but amplitudes can attenuated with this coefficient
facteur_perturb_duplique = 0.2

# Flag to enable/disable particle filter with:
# Flow rate observations:
FP_Q = True #True
# Snow amount observations:
FP_S = False #True


# Level to use to compare to snow observation (in calculation of weights) between 1 and 10
num_bande = 8

### Output name file
save_file_n = "Test_PF_Mordor_"+str(year)

#---------
# Parameters and observations files
data       = "QPThisto_J_isere@bvi_coche_ponserand_valid.txt"
data2      = "SWE_NRC_NDAout.txt"
file_param = "isere_FINAL_J_SIM.res"


#
pmtdict = res2pmt(file_param)
# output mode of Mordor (specific to the wrapper)
pmtdict["mod_out"] = 3

#############

As ouptut, the codes will create a NetCDF file containing: (of name "save_file_n")
-all simulations performed by the model for each particle (Q, S, S on each level, fneige)
-resampled forcings
-resampled initialization of Mordor (analyse) (that can be used to initialize MORDOR with unknown futur)

Figure with observed flow rate and resampled flow rate is also built.


