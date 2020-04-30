This folder contains all python codes developed around MORDOR-SD.
Each sub_folder will contains the code and a readme file.
We can found:

########
Mordor_python_library:
The librairy to use the python-Wrapper, and all the fortran codes.


########
Mordor_sensitivity:
    The purpose of these codes is to study Mordor's sensitivity to 
    the initial snow stock and to the error of unknown forcings
    #
    Sobol_indice_forecast_10years:
    to study Mordor's sensitivity to the initial snow stock over 10 years
    (Sobol indice calculation following the Saltelli 2002 method)

    #
    Forecast_unknown_futur_10years:
    to study Mordor's sensitivity to the error of unknown forcings over 10 years

    #
    Plot_Sobol_indice:
    Constructs graphs with data from Sobol_indice_forecast_10years.py

    #
    Plot_unknown_futur:
    Constructs graphs with data from Forecast_unknown_futur_10years.py
    

########
Autocorrelation:
This code study the autocorrelation of precipitations and temperatures observations.
Results can be used for the AR1.


########
In the context of studies carried out with twin experiments
##
Mordor_SD_simulation_ensemble_Twin_experiment:
Make ensemble simulations with perturbed forcings
This ensemble provides the truths ensemble.

##
Mordor_SD_Assimilation_ensemble_Twin_experiment:
Use the ensemble coming from Mordor_SD_simulation_ensemble_Twin_experiment.py
and will perform assimilation of Q or S or not for each truth.


##
Mordor_SD_forecast_Twin_experiment:
Use results from assimilation coming from Mordor_SD_Assimilation_ensemble_Twin_experiment.py
Perform forecast simulation with unknown forcings for each truth.

##
Plot_RMSE:
Use results from Mordor_SD_forecast_Twin_experiment.py
and make several grahs of RMSE, ratio of RMSE...



########
Simple codes to perfom Particle Filter assimilation and forecast
##
Mordor_PF_assimilation:
Perform Particle Filter assimilation of Q or S or not
(the ensemble simulation used for PF is done directly in the code)

##
Mordor_forecast:
Perform forecast simulations with unknown forcings,
by using initialisation coming from Mordor_PF_assimilation.py


