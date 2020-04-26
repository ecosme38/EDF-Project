Autocorrelation

The autocorrelation function quantifies the linear influence of the time lag between two observations of the process
by a classical correlation calculation.

This code reads the QPT data file and will explore the autocorrelation of the precipitation data and temperature data.

For each variable, statsmodels.tsa.stattools.acf function estimates the degree of autocorrelation.
And a scatter plot of DATA(t+1) vs DATA(t) is done.

In imput:
this code needs start and end dates, as example:
date_ini      = "2014-09-01"
date_fin      = "2017-03-31"

and the name of the data file to read:
data     = "QPThisto_J_isere@bvi_coche_ponserand_valid.txt"

----
After that, a similar process is done but by performing smoothing on data before.
To remove seasonality, smoothing on the temperature data is done by calculating their average over 10 years.

In imput: this part of the code needs start and end dates:
date_ini      = "2007-01-01"
date_fin      = "2016-12-31"

and also the first year to consider :
first_year     = 2007

After the smoothing process, the same autocorrelation analyses is done as previously.
