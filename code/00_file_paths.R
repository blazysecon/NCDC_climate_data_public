filedir <- file.path(getwd())
# filedir <- file.path(getwd(),"data","climate")

# input
cities_list_f<- file.path(filedir,"data/city_list_passport.csv") # The list of Cities in 80 Euromonitor countries
EMI_countries_f<- file.path(filedir,"data/EMI_country_list.csv") # The list of 210 Euromonitor countries
city_lats_f<- file.path(filedir,"data/ne_10m_populated_places.dbf") # Data on City pop, source (Populated Places): http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
city_lats_additional_f<- file.path(filedir,"data/city_location_data_additional.csv") # Additional cities coordinates missing from the main file
pop_f<- file.path(filedir,"data/city_pop.csv") # For 1150 cities from Passport
regions_f<- file.path(filedir,"data/regions.csv") # To match Syndicated Passport data regions to regions in the city_lats_f
city_regions_f<- file.path(filedir,"data/city_regions.csv") # City and regions matching file from Kasparas
alt_city_names_f<- file.path(filedir,"data/alt_city_names.csv") # File to match city names to Passport
us_state_codes_f<- file.path(filedir,"data/US_state_codes.csv") # From Wikipedia
type_desc_f <- file.path(filedir,"data/type_description.csv") # Description of climate indicators
API_keys <- file.path(filedir,"data/API_keys.csv")         # Use multiple API keys ( connected to different emails ), since there are download limits per day

output_dir <- file.path( filedir, "output" )

# output
city_coordinates_f<- file.path(output_dir,"CitiesCoord.csv")
city_data_f<- file.path(output_dir,"city_data.csv")
clim_dat_file_rg <- file.path(output_dir,paste0("clim_dat_Passport_countries_regions_", Sys.Date(), ".csv" )) # Data for forecast demand models
clim_dat_file_cn210 <- file.path(output_dir,paste0("clim_dat_Countries210_", Sys.Date(), ".csv" )) # Data for tourism project
clim_dat_file_cn <- file.path(output_dir,paste0("clim_dat_Countries80_", Sys.Date(), ".csv" )) # Data for city market sizes project
clim_dat_file_city <- file.path(output_dir,paste0("clim_dat_Cities_", Sys.Date(), ".csv" )) # Data for city market sizes project

# Supplementary files --
city_data_f <- file.path(output_dir,"city_data.csv")
station_data_output_f <- file.path(output_dir,"stations_clim_dat.csv")
station_data_output_prelim_f <- file.path(output_dir,"stations_clim_daily_prelim.csv")
station_data_output_clean_f <- file.path(output_dir,"stations_clim_daily_clean.csv")
station_data_interim_f <- file.path(output_dir,"stations_clim_dat_interim.rds")
station_data_output_M_f <- file.path(output_dir,"stations_clim_M_data.csv")
stations_f <- file.path(output_dir,"noaa_stations.csv")
stations_m_from_d_f <- file.path(output_dir,"stations_m_from_d.csv")
city_data_clim_clean_f <- file.path(output_dir,"city_data_clim_clean.csv")
