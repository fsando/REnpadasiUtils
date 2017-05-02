# REnpadasiUtils
R package containing various minor utilities for the ENPADASI project
This package contains currently one utility to help with uploading data to the Phenotype Database (also known as dbNP)

https://github.com/PhenotypeFoundation/PhenotypeDatabase

# How to use
There is currently only one function that will take a _long format_ data file and convert into a data file specially formatted for upload to dbNP

    dbnp_generate_wide_save_file(x,id_var="ID",treat_var="treatment",
                  visit_var="visit",time_var="time",visit_units="weeks",
                  time_units="minutes",file="dbnp-wide-format.csv")
    
The input data file must have a format similar to the following. 
It is important that both **visit** and **time** are numeric variable that indicate the time of measurements.
If visit number does not correspond to actual time another variable (visit_time) must be provided that indicates the actual time.

    ID  visit  time       treatment
    1     1       0         control
    1     1      20         control
    1     1      40         control
    1     1      60         control
    2     1       0    intervention
    2     1      20    intervention
    2     1      40    intervention
    2     1      60    intervention
    3     1       0         control
    3     1      20         control
    3     1      40         control
    3     1      60         control
    ...  ...    ...             ...
