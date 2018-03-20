# parameters
# Specify if raw daily data files should be updated - getting raw climate data on a daily level takes a long time!!
if_update_clim_data <- FALSE
# if_update_clim_data <- TRUE

# Specify climate indicators, for which to get data
data_types_list     <- c("MMXT","TPCP","DP01","MNTM","DT90","MMNT","DT32")
daily_datatypes     <- c("TMIN","TMAX","TAVG","SNWD","SNOW","PRCP")
sel_data_types_list <- c("MMXT","TPCP","DP01","MNTM","MMNT")

# Specify, which climate indicators cannot be negative
types_pos <- c("TPCP","DP01","DT90","DT32")
daily_types_pos <- c("SNWD","SNOW","PRCP")

# Specify, which climate output files to create (climate data can be downloaded for all relevant stations)
if_IFM_data <- TRUE # Specify if climate files for IFM data (80 countries + 10 CH/IN regions) should be created
if_travel_data <- TRUE # Specify if climate files for Travel data (210 countries) should be created
if_cities_data <- TRUE  # Specify if climate files for City sizes data (1050 Passport cities) should be created

# Select the last year for prediction
last_pred_yr <- 2030

monthList <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

