## This codes gets climate data from NOAA NCDC server 
## Preliminaries -----
rm ( list = ls()) # clear environment

# Sys.setlocale ( category = "LC_ALL", locale = "English_United States.1252" )

# turn off scientific notation in most output
options(scipen = 15)
# increase the max size of output file
options(max.print=10E6)

cat("\014") # clear console

if (!require("pacman")) install.packages("pacman")
# pacman::p_load(Hmisc, rnoaa, fossil, data.table, tidyverse, stringr,open.xlsx)
pacman::p_load(Hmisc, rnoaa, data.table, tidyverse, stringr, lubridate, fossil)

source("code/climate_fn.R")

# Load paths
enablePaths('code/00_file_paths.R', 'paths')

# if the analysis output directory doesn't exist, create it
dir.create ( output_dir, showWarnings = FALSE )

enablePaths('code/00_parameters.R', 'parameters')

# https://www.ncei.noaa.gov/data/gsom/access/

# ## Check ftp server access
# url <- 'https://www.ncei.noaa.gov/data/gsom/access/ASN00041080.csv'
# url <- 'https://www.ncei.noaa.gov/data/gsom/access/LH000026730.csv'
# temp_csv <- read.csv(file=url,stringsAsFactors=FALSE, header = TRUE)
# str(temp_csv)
# tail(temp_csv)
# rm(temp_csv)

station_data_output_part_f <- file.path(output_dir,"stations_clim_dat_part")
if (!file.exists(paste0(station_data_output_part_f,"1.csv.gz"))) parameters$if_update_clim_data <- TRUE

ptm0 <- Sys.time()
cat("\n","Downloading and preparing data for", daily_datatypes, "from NOAA, started at", format( ptm0 ), "\n")

#Load 210 EMI country list
EMI_countries <- sort(read.csv(file=EMI_countries_f,stringsAsFactors=FALSE, header = FALSE)$V1)
#Load 80 EMI country list
EMI_countries80 <- sort(unique(read.csv(file=cities_list_f,stringsAsFactors=FALSE, header = TRUE)$Country))

# Load city coordinates (created using 01_create_city_coords.R)
if (!file.exists(city_data_f)) source(file.path(filedir,"code/01_create_city_coords.R"))
DT_city_data <- fread(city_data_f)
head(DT_city_data)
sumstats(DT_city_data)

# DT_lats <- subset( DT_city_data, Country %in% EMI_countries80, select = c( "Country","cityID","lat","lon" )) # 80 Countries
# DT_lats <- subset( DT_city_data, !is.na(Type), select = c( "Country","cityID","lat","lon" )) # Passport Cities

DT_lats <- subset( DT_city_data, select = c( "Country","cityID","lat","lon" )) # All cities
setnames( DT_lats, c( "lat","lon" ), c( "lat","long" ))
sumstats(DT_lats)
head(DT_lats)

## Download daily NOAA data from NCDS website ----

# Get an API key ( = token ) at http://www.ncdc.noaa_gov/cdo-web/token
# APIkey <- "Your Key"
APIkeys <- as.list ( as.matrix ( read.table ( file = API_keys, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE, na.strings = "-9999999" )))
# APIkeys <- as.list(c("reHxDjispNbpVrAtSiyiDqqEZTqvUNQN","ZGmOKYYRTqzHscuvAMwnPGWxmGlaLDbg"))
# Test API keys, count how many are working
APIkeyWorking <- test_APIkeys(APIkeys)
# Select the first working API key
options( noaakey = APIkeyWorking[[ 1 ]])
# paste0( "Use NOAA API key = ", APIkeyWorking[[ 1 ]])

# Get the list of all noaa stations - update the file if its older than 100 days
# if ( !file.exists ( stations_f ) |
#      difftime ( Sys.time(), file.info ( stations_f) [, "mtime" ], units = "days" ) > 100 ) {
#     station_data <- ghcnd_stations()
#     write.csv ( station_data, file = stations_f, row.names = FALSE, na = "" )
# } else {
#     station_data <- fread(stations_f)
# }
station_data <- fread(stations_f)

table(station_data$element)

DT_noaa_datatypes_daily <- ncdc_datatypes ( datasetid = 'GHCND', limit = 1000 )$data

# Check weather data availability by indicator type
data_types <- check_data_type_avail(station_data,first_year_lim=2015,
                                    last_year_lim=2016,obs_lim=500) 
head(data_types,10)
# write.csv ( data_types, file = type_desc_f, row.names = FALSE, na = "" )

# keep only stations with data at least until 2016 for temperature, Snow, or rainfall
station_data_nonmiss <- station_data %>% 
    filter(element %in% daily_datatypes) %>% 
    filter(first_year<=2015 & last_year>=2016) %>% 
    select(id,latitude,longitude) %>% 
    arrange(id) %>% 
    distinct()
sumstats( station_data_nonmiss )
tail( station_data_nonmiss,1 )

## Identify closest 10 noaa stations for all cities within 1000 km radius
closest3_list <- list()
n <- nrow(DT_lats)
for (ij in 1:n) {
    cat(paste0(round(ij / n * 100,1), '% completed'))
    lat <- DT_lats[ij,]$lat
    long <- DT_lats[ij,]$long
    cityID <- DT_lats[ij,]$cityID
    closest3_list[[cityID]] <- meteo_distance(station_data_nonmiss, lat, long,
                                              radius = 1000, limit = 10) %>% 
        mutate(cityID=cityID)
    if (ij == n) cat(': Done')
    else cat('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b')
}
DT_closest3 <- bind_rows(closest3_list)
rm(closest3_list)

## Get the codes of the closest identified stations
stcodes_list <- as.list( unique( sort( DT_closest3$id )))
# Check for how many stations data will be downloaded, and for how many not
cat("Total stations available",nrow(station_data_nonmiss),", number of stations to download",length(stcodes_list),"\n")

## Identify closest 4+1 noaa stations for each station within 50km radio for data imputation (including station itself)
DT_closest_extra <- DT_closest3 %>% 
    select(id,latitude,longitude) %>% 
    distinct() %>% 
    arrange(id)

closest_extra_list <- list()
n <- nrow(DT_closest_extra)
for (ij in 1:n) {
    cat(paste0(round(ij / n * 100,1), '% completed'))
    lat <- DT_closest_extra[ij,]$latitude
    long <- DT_closest_extra[ij,]$longitude
    stationID <- DT_closest_extra[ij,]$id
    closest_extra_list[[stationID]] <- meteo_distance(station_data_nonmiss, lat, long,
                                                      radius = 50, limit = 10) %>% 
        rename(sim_id=id) %>% 
        mutate(id=stationID)
    if (ij == n) cat(': Done')
    else cat('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b')
}
DT_closest_extra <- bind_rows(closest_extra_list)
rm(closest_extra_list)
head(DT_closest_extra)

## Get the codes of the closest identified stations
stcodes_list2 <- as.list( unique( sort( union(DT_closest_extra$id,DT_closest_extra$sim_id) )))
# Check for how many stations data will be downloaded, and for how many not
cat("Total stations available",nrow(station_data_nonmiss),", number of stations to download",length(stcodes_list2),"\n")

# full_data <- stcodes_list2
# 
# # Add future years for prediction
# DT_full_p1 <- data.frame(id=unlist(stcodes_list2))
# DT_full_p2 <- expand.grid(year = seq(lastYear, last_pred_yr, 1),
#                             month = seq(1, 12, 1),
#                             data_type = sel_data_types_list, 
#                             stringsAsFactors = FALSE) %>% 
#     filter(year*100+month>last_year_mn)
# #Cartesian product of combinations
# DT_future <- merge(DT_future_p1,DT_future_p2) %>% 
#     data.table(key=c("Country","City","data_type","year","month"))
# if( if_update_clim_data ) {
#     ## Download climate data in k parts (increase k if your computer memory is smaller)
#     k=10
#     start_date <- "1960-01-01"
#     n <- length(stcodes_list2)
#     step <- ceiling(n/k)
#     ptm <- Sys.time()
#     for (ij in 1:k) {
#         start = (ij-1)*step+1
#         end = min(ij*step,n)
#         stcodes_list_part <- stcodes_list2[start:end]
#         DT_stations_dat_part <- get_daily_clim(stcodes_list=stcodes_list_part,
#                                               start_date=start_date,
#                                               datatypes=daily_datatypes) #This generaly takes a long time
#         ## Save data
#         if (nrow(DT_stations_dat_part) > 0) {
#             fwrite(DT_stations_dat_part, file = paste0(station_data_output_part_f,ij,".csv"), quote=TRUE )
#             system(paste0("gzip -f ",paste0(station_data_output_part_f,ij,".csv")))
#         }
#     }
#     time<-Sys.time()-ptm
#     cat("\nFinished in", time[[1]], attr(time,"units"),"\n" )
# }

# if( if_update_clim_data ) {
## Combine all downloaded climate data parts
data_parts <- sort(as.numeric(str_extract(
    list.files(output_dir, "stations_clim_dat_part"), "[0-9]+"
)))
for (ij in data_parts) {
    DT_stations_dat_part <-
        data.table::fread(paste0("gzip -dc ", station_data_output_part_f, ij, ".csv"))
    # Check if variables exist, if not create them
    for (var in c(
        "qflag_prcp", "qflag_snow", "qflag_snwd",
        "qflag_tmax", "qflag_tmin", "qflag_tavg",
        "prcp", "snow", "snwd", "tmax", "tmin", "tavg"
    )) {
        if (!(var %in% names(DT_stations_dat_part))) {
            DT_stations_dat_part[, var] <- NA_real_
        }
    }
    DT_stations_dat_part <- DT_stations_dat_part %>%
        # remove inconsistent data points and outliers, remove quality flags
        mutate(prcp = ifelse(qflag_prcp %in% c("K", "X"), NA, prcp / 10)) %>%
        mutate(tavg = ifelse(qflag_tavg %in% c("K", "X"), NA, tavg / 10)) %>%
        mutate(tmax = ifelse(qflag_tmax %in% c("K", "X"), NA, tmax / 10)) %>%
        mutate(tmin = ifelse(qflag_tmin %in% c("K", "X"), NA, tmin / 10)) %>%
        mutate(prcp = ifelse(prcp < 0 | prcp > 200 , NA, prcp)) %>%
        mutate(tmax = ifelse(tmax < (-70) | tmax > 60 , NA, tmax)) %>%
        mutate(tmin = ifelse(tmin < (-70) | tmin > 60 , NA, tmin)) %>%
        mutate(tavg = ifelse(tavg < (-70) | tavg > 60 , NA, tavg)) %>%
        mutate(tmax = ifelse(tmax < tavg & tmax <= tmin, NA, tmax)) %>%
        mutate(tmin = ifelse(tmin > tavg & tmin >= tmax, NA, tmin)) %>%
        select(id, date, prcp, tavg, tmax, tmin) %>% #do we need snow data?
        mutate(date = ymd(date)) %>%
        mutate(month = month(date)) %>%
        # filter(rowSums(!is.na(.)) > 2L) %>%
        group_by(id, month) %>% #restrict potential outliers
        mutate(
            qn_9_prcp = quantile(prcp, c(.999), na.rm = TRUE, type = 7),
            qn_1_tmax = quantile(tmax, c(.001), na.rm = TRUE, type = 7),
            qn_9_tmax = quantile(tmax, c(.999), na.rm = TRUE, type = 7),
            qn_1_tmin = quantile(tmin, c(.001), na.rm = TRUE, type = 7),
            qn_9_tmin = quantile(tmin, c(.999), na.rm = TRUE, type = 7),
            qn_1_tavg = quantile(tavg, c(.001), na.rm = TRUE, type = 7),
            qn_9_tavg = quantile(tavg, c(.999), na.rm = TRUE, type = 7)
        ) %>%
        ungroup() %>%
        mutate(prcp = ifelse(prcp > qn_9_prcp &
                                 qn_9_prcp > 0, qn_9_prcp, prcp)) %>%
        mutate(tmax = ifelse(tmax > qn_9_tmax, qn_9_tmax, tmax)) %>%
        mutate(tmax = ifelse(tmax < qn_1_tmax, qn_1_tmax, tmax)) %>%
        mutate(tmin = ifelse(tmin > qn_9_tmin, qn_9_tmin, tmin)) %>%
        mutate(tmin = ifelse(tmin < qn_1_tmin, qn_1_tmin, tmin)) %>%
        mutate(tavg = ifelse(tavg > qn_9_tavg, qn_9_tavg, tavg)) %>%
        mutate(tavg = ifelse(tavg < qn_1_tavg, qn_1_tavg, tavg)) %>%
        select(-starts_with("qn_"))
    # keep stations, which have at least 1000 daily data points
    select_stations <- DT_stations_dat_part %>%
        group_by(id) %>%
        summarise(nonmiss_obs = sum(!is.na(prcp) + !is.na(tmin) +
                                        !is.na(tmax) + !is.na(tavg))) %>%
        filter(nonmiss_obs > 200) %>%
        pull(id)
    DT_stations_dat_part <- DT_stations_dat_part %>%
        filter(id %in% select_stations)
    if (ij == 3) print(paste0(ij,", number of data poins for LH000026730:  ",
        nrow(DT_stations_dat_part[DT_stations_dat_part$id=="LH000026730",])))
    if (ij == 1)
        DT_stations_dat_all <- DT_stations_dat_part
    else
        DT_stations_dat_all <-
        rbind(DT_stations_dat_all, DT_stations_dat_part)
}

rm(DT_stations_dat_part)
setDT(DT_stations_dat_all, key = c("id", "date"))
summary(DT_stations_dat_all)
# print(sumstats(DT_stations_dat_all, TRUE))

if (exists("DT_stations_dat_all") & nrow(DT_stations_dat_all) > 0) {
    fwrite(DT_stations_dat_all, file = station_data_output_prelim_f, quote =
               TRUE)
    system(paste0("gzip -f ", station_data_output_prelim_f))
}

### Clean climate data, aggregate to monthly ----

if (!exists("DT_stations_dat_all")) {
    c <-
        fread(paste0("gzip -dc ", station_data_output_prelim_f))
}

# Use up to 10 closest stations within 50 km to impute missing climate data
DT_closest_extra2 <- DT_closest_extra %>%
    group_by(id) %>%
    filter(!(id == sim_id)) %>% #remove distances to itself
    mutate(closest_rank = rank(distance, ties.method = "first")) %>%
    ungroup() %>%
    arrange(id, closest_rank)
sumstats(DT_closest_extra2)
table(DT_closest_extra2$closest_rank)

# Reduce number of years to make the sample more managable !!!!!
nrow(DT_stations_dat_all)
DT_stations_dat_all <- DT_stations_dat_all %>% 
    filter(date > ymd('1980-01-01'))
nrow(DT_stations_dat_all)

# make a square dataset
station_set = unique(DT_stations_dat_all$id)
date_set = unique(DT_stations_dat_all$date)
full_set <- CJ(id = station_set, date = date_set)
full_set[,month := month(date)]

mem_used()
nrow(DT_stations_dat_all)
DT_stations_data_prep <- DT_stations_dat_all %>%
    full_join(full_set,
              by = c("id", "date", "month"))
setDT(DT_stations_data_prep, key = c("id", "date"))
rm(full_set)

mem_used()
summary(DT_stations_data_prep)

y <- sapply(ls(), function(x) object.size(get(x)))
print(cbind(round(y[order(-y)][1:5] / 1e6)))

closest_rank_list <- DT_closest_extra2 %>%
    pull(closest_rank) %>% unique() %>% sort()

for (rank_sel in closest_rank_list) {
    DT_closest_extra3 <- DT_closest_extra2 %>%
        filter(closest_rank == rank_sel) %>%
        select(id, sim_id) %>%
        na.omit() %>%
        distinct() %>%
        data.table(key = "id")
    cat("Iter", rank_sel, "Number of close stations:", 
        nrow(DT_closest_extra3),"\n")
    DT_stations_data_prep <- DT_stations_data_prep %>%
        left_join(DT_closest_extra3, by = c("id")) %>%
        left_join((
            DT_stations_dat_all %>%
                rename(
                    sim_id = id,
                    sim_prcp = prcp,
                    sim_tavg = tavg,
                    sim_tmax = tmax,
                    sim_tmin = tmin
                )
        ),
        by = c("sim_id", "date", "month")) %>%
        group_by(id, month) %>%
        mutate(
            obs_prcp = sum(!is.na(prcp)),
            #Check how many precipitation observations per station
            sum_prcp = sum(prcp),
            obs_sim_prcp = sum(!is.na(prcp)),
            sum_sim_prcp = sum(sim_prcp)
        ) %>%
        mutate(
            prcp = ifelse(obs_prcp == 0, sim_prcp, prcp),
            prcp = ifelse(obs_prcp <= 3 & obs_sim_prcp > 3 & 
                              sum_sim_prcp > sum_prcp,
                          sim_prcp, prcp),
            prcp = ifelse(obs_prcp <= 4 & 
                              obs_sim_prcp > 5 & sum_sim_prcp > sum_prcp * 1.5,
                          sim_prcp, prcp),
            tmax = ifelse(is.na(tmax), sim_tmax, tmax),
            tmin = ifelse(is.na(tmin), sim_tmin, tmin),
            tavg = ifelse(is.na(tavg), sim_tavg, tavg)
        ) %>%
        ungroup() %>%
        select(-starts_with("sim_"),
               -starts_with("obs_"),
               -starts_with("sum_"))
    print(mem_used())
    print(summary(DT_stations_data_prep))
}

if (exists("DT_stations_data_prep") & nrow(DT_stations_data_prep) > 0) {
    fwrite(DT_stations_data_prep, file = station_data_output_clean_f, quote =
               TRUE)
    system(paste0("gzip -f ", station_data_output_clean_f))
}
if (!exists("DT_stations_data_prep")) {
    DT_stations_data_prep <-
        fread(paste0("gzip -dc ", station_data_output_clean_f))
}


rm(DT_stations_dat_all)

# sumstats(DT_stations_data_prep, TRUE)
# Check and correct internal consistencies between indicators
DT_stations_data_prep <- DT_stations_data_prep %>%
    group_by(id, month) %>%
    mutate(
        qn_low_tmax = quantile(tmax, c(.1), na.rm = TRUE, type = 7),
        qn_8_tmin = quantile(tmin, c(.9), na.rm = TRUE, type = 7)
    ) %>%
    mutate(tmax = ifelse(tmax < tmin & tmax < qn_low_tmax, NA, tmax)) %>%
    mutate(tmin = ifelse(tmin > tmax & tmin > qn_8_tmin, NA, tmin)) %>%
    mutate(tmax = ifelse(tmax < tavg & tmax < qn_low_tmax, NA, tmax)) %>%
    mutate(tmin = ifelse(tmin > tavg & tmin > qn_8_tmin, NA, tmin)) %>%
    mutate(tavg = ifelse(tavg > tmax, NA, tavg)) %>%
    mutate(tavg = ifelse(tavg < tmin, NA, tavg)) %>%
    mutate(tmin = ifelse(tmin > tmax, NA, tmin)) %>%
    mutate(tavg = ifelse(is.na(tavg), (tmin + tmax) / 2, tavg)) %>%
    ungroup %>%
    select(-starts_with("qn_"), -month)
# filter(rowSums(!is.na(.)) > 2L)
# sumstats(DT_stations_data_prep, TRUE)
if (nrow(DT_stations_data_prep) > 0) {
    fwrite(DT_stations_data_prep, file = station_data_output_clean_f, quote =
               TRUE)
    system(paste0("gzip -f ", station_data_output_clean_f))
}
## Load daily clim dataset, aggregate to monthly
if (!exists("DT_stations_data_prep")) {
    DT_stations_data_prep <-
        fread(paste0("gzip -dc ", station_data_output_clean_f))
}

# Estimate monthly averages
DT_m_from_daily <- DT_stations_data_prep %>%
    filter(rowSums(!is.na(.)) > 2L) %>% 
    group_by(id) %>%
    mutate(last_date = max(date)) %>% # Identify the last date of nonmissing observations by station
    mutate(year = year(date), month = month(date)) %>%
    group_by(id, year, month) %>%
    summarise(
        last_date = max(last_date, na.rm = TRUE),
        DP01 = sum(prcp >= 2.5, na.rm = TRUE),
        #>0.1 inch in mm
        TPCP = sum(prcp, na.rm = TRUE),
        obs_TPCP = sum(!is.na(prcp)),
        # count non-missing daily observations within a month
        MMNT = mean(tmin, na.rm = TRUE),
        obs_MMNT = sum(!is.na(tmin)),
        MMXT = mean(tmax, na.rm = TRUE),
        obs_MMXT = sum(!is.na(tmax)),
        MNTM = mean(tavg, na.rm = TRUE),
        obs_MNTM = sum(!is.na(tavg))
    ) %>%
    ungroup()

str(DT_m_from_daily)
sumstats(DT_m_from_daily, TRUE)
sumstats(DT_m_from_daily %>% filter(year >= 2016), TRUE)
print(head(DT_m_from_daily))
if (nrow(DT_m_from_daily) > 0) {
    fwrite(DT_m_from_daily, file = stations_m_from_d_f, quote = TRUE)
    system(paste0("gzip -f ", stations_m_from_d_f))
}

rm(DT_stations_data_prep)
# }


## Work with monthly climate dataset from daily data  ----

if (!exists("DT_m_from_daily")) {
    DT_m_from_daily <- fread(paste0("gzip -dc ",stations_m_from_d_f))
}
summary(DT_m_from_daily)
subset(DT_m_from_daily,id=="SWE00138886" & year==2010)
subset(DT_m_from_daily,id=="SWM00002485" & year==2010)
print(mem_used())

sel_m_data_types_list <- c("TPCP","DP01","MMNT","MMXT","MNTM")

# Check the share of station for which no data was downloaded/loaded
stations_with_daily_data <- DT_m_from_daily %>% 
    pull(id) %>% unique() %>% sort()
stcodes <- unlist(stcodes_list2)
stations_no_data <- setdiff(stcodes,stations_with_daily_data)
stations_no_data[grepl('^LH.*', stations_no_data)]
# "LH000026629" "LH000026730"
dat_part <- get_daily_clim(stcodes_list=list('LH000026730'))
                               
cat("Share of stations with no data:",
    length(stations_no_data)/length(stcodes_list)*100,"%")

# Compare stations between identified closest stations and stations in the daily data file
compare_station_ids(stcodes,stations_with_daily_data)

DT_stations_dat_all_w0 <- DT_m_from_daily %>% 
    mutate(last_date=ymd(last_date),
           date=make_date(year= year, month = month, day = 1)) %>% 
    group_by(id,month) %>% 
    mutate(MMNT=ifelse(obs_MMNT<3,NA,MMNT),  # set monthly indicators to NA if estimated from <3 daily observations
           MMXT=ifelse(obs_MMXT<3,NA,MMXT),
           MNTM=ifelse(obs_MNTM<3,NA,MNTM),
           TPCP=ifelse(obs_TPCP>4 | (obs_MMNT>17 | obs_MMXT>18 | obs_MNTM>17) &
                           obs_TPCP>0,TPCP,NA), # require prcp indicator to have good coverage, unless other climate indicators have good coverage
           DP01=ifelse(obs_TPCP>4 | (obs_MMNT>17 | obs_MMXT>18 | obs_MNTM>17) &
                           obs_TPCP>0,DP01,NA)) %>% 
    mutate(TPCP=ifelse(last_date-date < 27 & month==month(last_date), NA, TPCP),  # set to NA in the last month if last day is of observations is less than 27 days ago
           DP01=ifelse(last_date-date < 27 & month==month(last_date), NA, DP01)) %>% 
    select(-last_date,-date,-starts_with("obs_")) %>% 
    select(id, year, month, DP01, TPCP, MMNT, MMXT, MNTM) %>% 
    data.table(key=c("id","year","month"))
sumstats(DT_stations_dat_all_w0, TRUE)

# get old monthly data
DT_stations_dat_month_all <- data.table::fread(paste0("gzip -dc ",station_data_output_M_f))
sumstats(DT_stations_dat_month_all, TRUE)
DT_monthly <- DT_stations_dat_month_all %>% 
    filter(year>=1980) %>%
    filter(datatype %in% sel_m_data_types_list) %>% 
    mutate(id = str_split_fixed(station, ":", 2)[,2]) %>% 
    select(-station) %>% 
    spread(datatype,value) %>% 
    mutate(TPCP = TPCP/10, #Precipitation is are reported with one significant digit in mm
           MMNT = MMNT/10, #Temperature values are reported with one significant digit in Celsius
           MMXT = MMXT/10,
           MNTM = MNTM/10) %>% 
    data.table(key=c("id","year","month"))
rm(DT_stations_dat_month_all)
sumstats(DT_monthly, TRUE)

## Combine daily to month and monthly data for comparative plotting
DT_for_plots <- DT_stations_dat_all_w0 %>%
    mutate(source = "daily") %>%
    bind_rows(DT_monthly %>% mutate(source = "monthly")) %>%
    gather(indicator, value, DP01:MNTM) %>%
    mutate(date = make_date(year= year, month = month, day = 1))

head(DT_for_plots)

# Use a subset - stations_with_daily_data[100:120]
# select_station = "SWM00002485"
# select_station = "TI000038944"

station_set <- stations_with_daily_data[grepl('^LH.*',stations_with_daily_data)]
# "LH000026502" "LH000026509" "LH000026518" "LH000026531" "LH000026633"
# "LH000026730"
# station_set <- stations_with_daily_data[501:1000]

filename <- "plots/weather_LT.pdf"
pdf(filename)
for (select_station in station_set){
    print(ggplot(
    data = DT_for_plots %>%
        filter(id %in% select_station ) %>%
        filter(date > '2010-01-01')
    , aes(x = date, y = value, colour = source, group = source)
    ) +
        geom_line() + facet_grid(indicator ~ ., scales = "free_y") +
        ggtitle(paste0("Monthly indicators for station=", select_station)) +
        xlab("Date"))
}
dev.off()

# Use monthly data, where daily data is missing
DT_stations_dat_all_w <- DT_stations_dat_all_w0 %>%
    full_join(DT_monthly %>% rename(m_TPCP=TPCP,m_DP01=DP01,m_MMNT=MMNT,
                                    m_MMXT=MMXT,m_MNTM=MNTM), 
              by=c("id","year","month")) %>%
    filter(id %in% stcodes) 

sumstats(DT_stations_dat_all_w, TRUE) 

DT_stations_dat_all_w <- DT_stations_dat_all_w %>% #keep only relevant stations 
    mutate(TPCP=ifelse(!is.na(m_TPCP) & (is.na(TPCP) | TPCP==0), m_TPCP,TPCP),
           DP01=ifelse(!is.na(m_DP01) & (is.na(DP01) | TPCP==0), m_DP01,DP01),
           MMNT=ifelse(!is.na(m_MMNT) & is.na(MMNT), m_MMNT, MMNT),
           MMXT=ifelse(!is.na(m_MMXT) & is.na(MMXT), m_MMXT, MMXT),
           MNTM=ifelse(!is.na(m_MNTM) & is.na(MNTM), m_MNTM, MNTM),
           DP01=ifelse(m_DP01>DP01 & !is.na(m_DP01),m_DP01,DP01),
           TPCP=ifelse(m_TPCP>TPCP & !is.na(m_TPCP) & 
                           m_TPCP>0 & m_TPCP<2500,m_TPCP,TPCP)) %>% 
    select(-starts_with("m_")) %>% 
    data.table(key=c("id","year","month"))

sumstats(DT_stations_dat_all_w, TRUE)
head(DT_stations_dat_all_w)

## Identify last year-month with nonmissing data for all stations/indicators
DT_stations_dat_all_w[ , yearmn := year*100+month]
DT_stations_dat_all_w$nNmiss <- 
    rowSums(DT_stations_dat_all_w[,lapply(.SD,countNonMiss),
                                  .SDcols=sel_m_data_types_list])
DT_stations_dat_all_w[yearmn<2010 , nNmiss := N]
last_year_mn <- max(subset(DT_stations_dat_all_w,nNmiss>0,yearmn))
set( DT_stations_dat_all_w, j = c( "yearmn" ), value = NULL )
DT_stations_dat_all_w[ , nNmiss2 := sum(nNmiss,na.rm=TRUE), by=.(id)]
hist(subset(DT_stations_dat_all_w,nNmiss2<900)$nNmiss2)

## Remove stations which have weather indicators available for less 
#  than 150 indicator-months since 2010
missing_stations <- sort(unique(subset(DT_stations_dat_all_w,nNmiss2<150)$id))
length(missing_stations)
DT_stations_dat_all_w <- subset( DT_stations_dat_all_w,
                                 !( id %in% missing_stations ))
stations_with_data <- unique( DT_stations_dat_all_w$id )
set( DT_stations_dat_all_w, j = c( "nNmiss","nNmiss2"), value = NULL )

sumstats(DT_stations_dat_all_w, TRUE)

# Use up to 10+1 closest stations within 150 km to impute missing monthly climate data
station_data_m_nonmiss <- station_data_nonmiss %>% 
    filter(id %in% stations_with_data)
closest_m_list <- list()
n <- nrow(station_data_m_nonmiss)
for (ij in 1:n) {
    cat(paste0(round(ij / n * 100,1), '% completed'))
    lat <- station_data_m_nonmiss[ij,]$latitude
    long <- station_data_m_nonmiss[ij,]$longitude
    stationID <- station_data_m_nonmiss[ij,]$id
    closest_m_list[[stationID]] <- meteo_distance(
        station_data_m_nonmiss, lat, long, radius = 150, limit = 10) %>% 
        rename(sim_id=id) %>% 
        mutate(id=stationID)
    if (ij == n) cat(': Done')
    else cat('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b')
}
station_data_m_nonmiss <- bind_rows(closest_m_list) %>% 
    group_by(id) %>% 
    filter(!(id==sim_id)) %>% #remove distances to itself
    mutate(closest_rank = rank(distance,ties.method= "first")) %>% 
    ungroup() %>% 
    arrange(id,closest_rank)
rm(closest_m_list)
sumstats(station_data_m_nonmiss, TRUE)
table(station_data_m_nonmiss$closest_rank)

DT_stations_data_prep_w <- DT_stations_dat_all_w 
closest_rank_list <- station_data_m_nonmiss %>% 
    pull(closest_rank) %>% unique() %>% sort()
for (rank_sel in closest_rank_list) {
    station_data_m_nonmiss2 <- station_data_m_nonmiss %>% 
        filter(closest_rank==rank_sel) %>% 
        select(id,sim_id) %>% 
        na.omit() %>%
        distinct() %>% 
        data.table(key="id")
    cat("Iter",rank_sel,"Number of close stations:",
        nrow(station_data_m_nonmiss2),"\n")
    DT_stations_data_prep_w <- DT_stations_data_prep_w %>% 
        left_join(station_data_m_nonmiss2,by=c("id")) %>% 
        left_join((DT_stations_dat_all_w %>% 
                       rename(sim_id=id,sim_DP01=DP01,sim_TPCP=TPCP,
                              sim_MMXT=MMXT,sim_MMNT=MMNT,sim_MNTM=MNTM)),
                  by=c("sim_id","year","month")) %>% 
        mutate(DP01=ifelse(is.na(DP01),sim_DP01,DP01),
               TPCP=ifelse(is.na(TPCP),sim_TPCP,TPCP),
               MMNT=ifelse(is.na(MMNT),sim_MMNT,MMNT),
               MMXT=ifelse(is.na(MMXT),sim_MMXT,MMXT),
               MNTM=ifelse(is.na(MNTM),sim_MNTM,MNTM)) %>% 
        select(-starts_with("sim_"),-sim_id)
}
sumstats(DT_stations_dat_all_w, TRUE)
sumstats(DT_stations_data_prep_w, TRUE)

# Create an empty data_set for the whole data period
DT_period <- expand.grid(id = unique(DT_stations_data_prep_w$id), 
                         year = unique(DT_stations_data_prep_w$year), 
                         month = unique(DT_stations_data_prep_w$month),
                         data_type = sel_data_types_list, 
                         stringsAsFactors = FALSE) %>% 
    filter(year*100+month<=last_year_mn) %>% 
    data.table(key=c("id","year","month","data_type"))

## Reshape dataset to long format !!
# Use long-term month average for each indicator if at least data for 3 months is available
DT_stations_data_long <- DT_stations_data_prep_w %>% 
    gather(data_type,value,sel_data_types_list) %>% 
    data.table(key=c("id","year","month","data_type")) %>% 
    full_join(DT_period,by=c("id","year","month","data_type")) %>% 
    group_by(id,data_type,month) %>% 
    mutate(mean_value = mean(value,na.rm=TRUE)) %>% # estimate long run monthly averages
    mutate(obs_value = sum(!is.na(value))) %>% 
    mutate(value=ifelse(is.na(value) & obs_value>=3,mean_value,value)) %>% 
    # select(-obs_value,-mean_value,-val1) %>%
    select(-obs_value,-mean_value) %>% 
    ungroup()
sumstats(DT_stations_data_long, TRUE)
subset(DT_stations_data_long,id=="TI000038944" & data_type=="MMNT" & year==1991)

subset(DT_stations_data_long,id=="SWE00138886" & data_type=="TPCP" & year==2010)
subset(DT_stations_data_long,id=="SWM00002485" & data_type=="TPCP" & year==2010)

subset(DT_stations_dat_all_w,id=="SWE00138886" & year==2010)
subset(DT_stations_dat_all_w,id=="SWM00002485" & year==2010)

subset(DT_stations_dat_all_w,id=="LH000026730" & year==2016)

# Identify closest 2 stations with nonmissing data for each weather indicator
# If midpoint between the two stations is significantly closer than any of the 
#   stations separately, use an average of the two station for each city
for (clim_ind in sel_m_data_types_list) {
    cat("\n Working with",clim_ind,"\n\n")
    stations_with_m_data <- DT_stations_data_long %>% 
        filter(data_type==clim_ind) %>% 
        group_by(id) %>% 
        summarise(count_miss = sum(is.na(value))) %>% 
        filter(count_miss==0) %>% #all data for that indicator and station should be non-missing
        pull(id) %>% unique() %>% sort()
    station_data_m_nonmiss <- station_data_nonmiss %>% 
        filter(id %in% stations_with_m_data)
    closest2_list <- list()
    n <- nrow(DT_lats)
    for (ij in 1:n) {
        cat(paste0(round(ij / n * 100,1), '% completed'))
        lat <- DT_lats[ij,]$lat
        long <- DT_lats[ij,]$long
        cityID <- DT_lats[ij,]$cityID
        closest2_list[[cityID]] <- meteo_distance(station_data_m_nonmiss, lat, long, limit = 2) %>% 
            mutate(cityID=cityID)
        if (ij == n) cat(': Done')
        else cat('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b')
    }
    DT_closest2 <- bind_rows(closest2_list) %>% 
        group_by(cityID) %>% 
        mutate(closest_rank = rank(distance,ties.method= "random")) %>% 
        ungroup() 
    DT_closest2w <- (DT_closest2 %>% filter(closest_rank==1) %>% 
                         rename_at(.vars=c("id","latitude","longitude","distance"), funs(paste0("st1_", .))) %>% 
                         select(-closest_rank)) %>% 
        left_join( (DT_closest2 %>% filter(closest_rank==2) %>% 
                        rename_at(.vars=c("id","latitude","longitude","distance"), funs(paste0("st2_", .))) %>% 
                        select(-closest_rank)),
                   by=c("cityID")) %>% 
        mutate(dist_st1_to_st2 = fossil::deg.dist( st1_longitude, st1_latitude, st2_longitude, st2_latitude )) %>%
        mutate(dist_st1_to_st2 = pmin(dist_st1_to_st2,st1_distance+st2_distance)) %>% 
        #get distance between the city and the midpoint between two stations, using Apollonius' theorem
        mutate(dist_to_midpoint = sqrt (( 2*st1_distance^2+2*st2_distance^2-dist_st1_to_st2^2 )/4 )) %>% 
        mutate(if_use_midpoint = as.integer(dist_to_midpoint<st1_distance), 
               closest_dist    = pmin(st1_distance, dist_to_midpoint)) 
    # mutate(if_use_midpoint = ifelse(st1_distance<30 | st1_distance < (dist_to_midpoint-20),
    #                                 0,if_use_midpoint),
    #        closest_dist    = ifelse(st1_distance<30 | st1_distance < (dist_to_midpoint-20),
    #                                 st1_distance,closest_dist ))
    DT_stations_p1 <- DT_stations_data_long %>% 
        filter(data_type==clim_ind) %>% 
        filter(id %in% DT_closest2w$st1_id) %>% 
        rename(st1_id=id,st1_value=value)
    DT_stations_p2 <- DT_stations_data_long %>% 
        filter(data_type==clim_ind) %>% 
        filter(id %in% DT_closest2w$st2_id) %>% 
        rename(st2_id=id,st2_value=value)
    data <- DT_lats %>%     
        left_join(DT_closest2w %>% 
                      select(cityID,st1_id,st2_id,if_use_midpoint),
                  by=c("cityID")) %>% 
        left_join(DT_stations_p1,by=c("st1_id")) %>% 
        left_join(DT_stations_p2,by=c("st2_id","year","month","data_type")) %>% 
        mutate(value=ifelse(if_use_midpoint,st1_value %+ana% st2_value,st1_value)) %>% 
        select(Country,cityID,year,month,data_type,value)
    if (clim_ind == sel_m_data_types_list[1]) {
        DT_city_data_clim <- data
    } else {
        DT_city_data_clim <- DT_city_data_clim %>% 
            bind_rows(data)
    }
    rm(data)
}

sumstats(DT_city_data_clim, TRUE)
head(DT_city_data_clim)

sumstats(DT_closest2w, TRUE)

if (nrow(DT_city_data_clim) > 0) {
    fwrite(DT_city_data_clim, file = city_data_clim_clean_f, quote=TRUE )
    system(paste0("gzip -f ",city_data_clim_clean_f))
}

# Check cities with furthest stations
DT_city_data %>% 
    left_join(DT_closest2w, by=c("cityID")) %>% 
    select(Country,City,st1_distance,if_use_midpoint,closest_dist) %>% 
    arrange(-closest_dist) %>%
    head(50) 

# Check random cities
DT_city_data %>% 
    left_join(DT_closest2w, by=c("cityID")) %>% 
    select(Country,City,st1_distance,if_use_midpoint,closest_dist) %>% 
    filter(City %in% c("Stockholm","Moscow","Vilnius","Milan"))
DT_city_data %>% 
    left_join(DT_closest2w, by=c("cityID")) %>% 
    filter(City %in% c("Stockholm"))

## Load monthly climate dataset for cities, prepare final output ----
if (!exists("DT_city_data_clim")) {
    DT_city_data_clim <- fread(paste0("gzip -dc ",city_data_clim_clean_f))
}

## Create aggregated data separately for countries, cities and China/India regions 
lastYear <- floor(last_year_mn/100)
lastMonth <- last_year_mn %% 100

## Climate Data for countries <><><><><><><><><><><><><><><><><><>
DT_city_data_cn <- DT_city_data_clim %>% 
    left_join(DT_city_data %>% select(Country,cityID,pop), by=c("Country","cityID")) %>% 
    group_by(Country,year,month,data_type) %>% 
    mutate(pop2 = pop*(!is.na(value)),
           value_pop = pop*value) %>% 
    summarise(value_w = sum(value_pop,na_rm = TRUE),
              pop_sum = sum(pop2,na_rm = TRUE)) %>% 
    mutate(value = value_w/pop_sum) %>% 
    select(-value_w,-pop_sum) %>% 
    data.table(key=c("Country","data_type","year","month"))
sumstats(DT_city_data_cn, TRUE)

# Add future years for prediction
DT_future <- expand.grid(Country = unique(DT_city_data_cn$Country), 
                         year = seq(lastYear, last_pred_yr, 1),
                         month = seq(1, 12, 1),
                         data_type = sel_data_types_list, 
                         stringsAsFactors = FALSE) %>% 
    filter(year*100+month>last_year_mn) %>% 
    data.table(key=c("Country","data_type","year","month"))

DT_city_data_cn <- DT_city_data_cn %>% 
    full_join(DT_future, by=c("Country","data_type","year","month")) %>% 
    group_by(Country, data_type, month) %>% 
    mutate(value = predict_w_mean(value)) %>% 
    data.table(key=c("Country","data_type","year","month"))

# Add annual average
DT_city_data_cn_ann <- DT_city_data_cn %>% 
    group_by(Country, data_type, year) %>% 
    summarise(value = mean ( value, na.rm = FALSE ), month = 13L)

DT_city_data_cn <- DT_city_data_cn %>% 
    bind_rows(DT_city_data_cn_ann) %>% 
    spread(month, value) %>% 
    rename(Year=year) %>% 
    data.table(key=c("Country","data_type","Year"))
setnames( DT_city_data_cn, paste0( 1:13 ), c( monthList,"ANN" ))

length( table( DT_city_data_cn$Country ))
check_EMI_cn(as.character(DT_city_data_cn$Country),EMI_countries)
cbind( table( DT_city_data_cn$data_type ))
length( table( DT_city_data_cn$Year ))
head(DT_city_data_cn)
sumstats ( DT_city_data_cn )

if (if_travel_data) {
    write.table( DT_city_data_cn, file = clim_dat_file_cn210, sep = ",", row.names = FALSE, na = "" )
}

# Climate Data for main research countries <><><><><><><><><><><><><><><><><><>
DT_fin1_clim_dat <- subset(DT_city_data_cn,Country %in% EMI_countries80)
sumstats ( DT_fin1_clim_dat )
length(unique(DT_fin1_clim_dat$Country))

if (if_cities_data) {
    write.table( DT_fin1_clim_dat, file = clim_dat_file_cn, sep = ",", row.names = FALSE, na = "" )
}

## Climate Data for cities <><><><><><><><><><><><><><><><><><>
DT_city_data_city <- DT_city_data_clim %>% 
    left_join(DT_city_data %>% select(Country,City,cityID,Type), by=c("Country","cityID")) %>% 
    filter(Type %in% c("tier1","tier2")) %>% 
    select(Country,City,data_type,year,month,value) %>% 
    data.table(key=c("Country","City","data_type","year","month"))

# Add future years for prediction
DT_future_p1 <- data.frame(unique(DT_city_data_city[,c("Country","City")]))
DT_future_p2 <- expand.grid(year = seq(lastYear, last_pred_yr, 1),
                            month = seq(1, 12, 1),
                            data_type = sel_data_types_list, 
                            stringsAsFactors = FALSE) %>% 
    filter(year*100+month>last_year_mn)
#Cartesian product of combinations
DT_future <- merge(DT_future_p1,DT_future_p2) %>% 
    data.table(key=c("Country","City","data_type","year","month"))

DT_city_data_city <- DT_city_data_city %>% 
    full_join(DT_future, by=c("Country","City","data_type","year","month")) %>% 
    group_by(Country, City, data_type, month) %>% 
    mutate(value = predict_w_mean(value)) %>% 
    data.table(key=c("Country","City","data_type","year","month"))

# Add annual average
DT_city_data_city_ann <- DT_city_data_city %>% 
    group_by(Country, City, data_type, year) %>% 
    summarise(value = mean ( value, na.rm = FALSE ), month = 13L)

DT_city_data_city <- DT_city_data_city %>% 
    bind_rows(DT_city_data_city_ann) %>% 
    spread(month, value) %>% 
    rename(Year=year) %>% 
    data.table(key=c("Country","City","data_type","Year"))
setnames( DT_city_data_city, paste0( 1:13 ), c( monthList,"ANN" ))

if (if_cities_data) {
    write.table( DT_city_data_city, file = clim_dat_file_city, sep = ",", row.names = FALSE, na = "" )
}


# temp <- subset(DT_city_data_cn, Country=="Lithuania" & Year >= 2015 & Year <= 2019)



# ## Climate Data for forecast demand models with China and India regions <><><><><><><><><><><><><><><><><><>
# DT_city_data_rg <- subset(DT_city_data_temp,(!is.na(IMISRegion) & !IMISRegion==""),select=-c(Type))
# sumstats ( DT_city_data_rg )
# length(unique(DT_city_data_rg$Country))
# # Get pop weighted country averages (use only cities with nonmissing data)
# for ( datatype in sel_m_data_types_list ) {
#     setnames( DT_city_data_rg, datatype, "tmp" )
#     DT_city_data_rg [ , PopSum:= sum ( as.integer(!is.na(tmp))*pop )
#                       , by = c( "Country","IMISRegion","year","month" )]
#     DT_city_data_rg [ , tmpWgt:= tmp*pop/PopSum ]
#     DT_city_data_rg [ , tmp2:= sum ( !is.na(tmp) ), by = c( "Country","IMISRegion","year","month" )]
#     DT_city_data_rg [ , tmp:= sum ( tmpWgt , na.rm=TRUE), by = c( "Country","IMISRegion","year","month" )]
#     DT_city_data_rg [tmp2==0, tmp:= NA] #Data in all cities for this country/year/month missing
#     set ( DT_city_data_rg, j = c( "tmp2","tmpWgt","PopSum" ), value = NULL )
#     setnames( DT_city_data_rg, "tmp", datatype )
# }
# set ( DT_city_data_rg, j = c( "City","pop" ), value = NULL )
# DT_city_data_rg <- unique ( DT_city_data_rg )
# setkey(DT_city_data_rg,Country,IMISRegion,year,month)
# DT_city_data_rg <- unique ( DT_city_data_rg )
# subset( DT_city_data_rg, year == 2014 & month == 12 )
# sumstats( DT_city_data_rg )
# head ( subset( DT_city_data_rg, year == 2010 ))
# # Reshape country data so that months are columns 
# DT_fin3_clim_dat <- melt ( DT_city_data_rg, id = c( "Country","IMISRegion","year","month" ), variable = "data_type" )
# DT_fin3_clim_dat [ IMISRegion != "Country", Country:= IMISRegion ]
# set ( DT_fin3_clim_dat, j = c( "IMISRegion" ), value = NULL )
# DT_fin3_clim_dat[,type:=as.character(type)]
# # Add future years for prediction
# DT_add <- data.table(expand.grid(Country=unique(DT_fin3_clim_dat$Country)
#                                  , year = seq(lastYear, last_pred_yr, 1)
#                                  , month = seq(1, 12, 1)
#                                  , type=unique(DT_fin3_clim_dat$type)
# ))
# DT_fin3_clim_dat <- merge(DT_fin3_clim_dat,DT_add,by=names(DT_add),all=TRUE)
# setkey(DT_fin3_clim_dat,Country,type,month,year)
# # DT_fin3_clim_dat[,("value") := lapply(.SD, predict_w_trend), .SDcols = c("value")
# #                    ,by=c("Country","data_type","month")]
# DT_fin3_clim_dat[,("value") := lapply(.SD, predict_w_mean), .SDcols = c("value")
#                  ,by=c("Country","data_type","month")]
# DT_fin3_clim_dat[data_type %in% types_pos, ("value") := lapply(.SD, neg_to_zero), .SDcols =  c("value")]
# # Find an annual average
# DT_fin3_clim_dat_ann <- DT_fin3_clim_dat[, .( value = mean ( value, na.rm = FALSE ), month = 13L )
#                                          , by = .( Country,type,year )]
# DT_fin3_clim_dat <- merge ( DT_fin3_clim_dat, DT_fin3_clim_dat_ann
#                             , by = names ( DT_fin3_clim_dat ), all = TRUE )
# setnames( DT_fin3_clim_dat, "year","Year" )
# 
# sumstats(DT_fin3_clim_dat)
# length(unique(DT_fin3_clim_dat$Country))
# 
# DT_fin3_clim_dat <- data.table( dcast ( DT_fin3_clim_dat, Country+Year+type ~ month ))
# setnames( DT_fin3_clim_dat, paste0( 1:13 ), c( monthList,"ANN" ))
# setkeyv( DT_fin3_clim_dat, c( "Country","data_type","Year" ))
# sumstats( DT_fin3_clim_dat )
# str( DT_fin3_clim_dat )
# cbind(table( DT_fin3_clim_dat$Country ))
# cbind( table( DT_fin3_clim_dat$type ))
# length( table( DT_fin3_clim_dat$Year ))
# 
# # Combine regional data and 80 countries data together
# sumstats ( DT_fin3_clim_dat )
# DT_fin3_clim_dat <- merge ( DT_fin3_clim_dat, DT_fin1_clim_dat, by = names ( DT_fin1_clim_dat ), all = TRUE )
# sumstats ( DT_fin3_clim_dat )
# length(unique(DT_fin3_clim_dat$Country))
# 
# if (if_IFM_data) {
#     write.table( DT_fin3_clim_dat, file = clim_dat_file_rg, sep = ",", row.names = FALSE, na = "" )
# }
# 
# 

# table(subset(DT_fin2_clim_dat,is.na(MAY),select=c(Country,type)))

# Clean up
# rm( DT_fin_clim_dat_ann, DT_city_data_cn, DT_merg_clim_temp
# , DTcitySt1, DTmergedSt1, DTcityMidpoint, DT_merged_midpoint, DT_city_data2, DT_stations_dat_all_w )

## Explore NCDC NOAA data ----
# ncdc_datasets()$data
# #                     uid    mindate    maxdate                        name datacoverage         id
# # 1  gov.noaa.ncdc:C00861 1763-01-01 2017-08-04             Daily Summaries         1.00      GHCND
# # 2  gov.noaa.ncdc:C00946 1763-01-01 2017-07-01 Global Summary of the Month         1.00       GSOM
# # 3  gov.noaa.ncdc:C00947 1763-01-01 2017-01-01  Global Summary of the Year         1.00       GSOY
# # 4  gov.noaa.ncdc:C00345 1991-06-05 2017-08-04    Weather Radar (Level II)         0.95    NEXRAD2
# # 5  gov.noaa.ncdc:C00708 1994-05-20 2017-08-01   Weather Radar (Level III)         0.95    NEXRAD3
# # 6  gov.noaa.ncdc:C00821 2010-01-01 2010-01-01     Normals Annual/Seasonal         1.00 NORMAL_ANN
# # 7  gov.noaa.ncdc:C00823 2010-01-01 2010-12-31               Normals Daily         1.00 NORMAL_DLY
# # 8  gov.noaa.ncdc:C00824 2010-01-01 2010-12-31              Normals Hourly         1.00 NORMAL_HLY
# # 9  gov.noaa.ncdc:C00822 2010-01-01 2010-12-01             Normals Monthly         1.00 NORMAL_MLY
# # 10 gov.noaa.ncdc:C00505 1970-05-12 2014-01-01     Precipitation 15 Minute         0.25  PRECIP_15
# # 11 gov.noaa.ncdc:C00313 1900-01-01 2014-01-01        Precipitation Hourly         1.00 PRECIP_HLY

# ncdc_datacats(datasetid = "GHCND")$data
# # name     id
# # 1        Evaporation   EVAP
# # 2               Land   LAND
# # 3      Precipitation   PRCP
# # 4 Sky cover & clouds    SKY
# # 5           Sunshine    SUN
# # 6    Air Temperature   TEMP
# # 7              Water  WATER
# # 8               Wind   WIND
# # 9       Weather Type WXTYPE
# 
# ncdc_datacats(datasetid = "NORMAL_MLY")$data
# # name      id
# # 1 Computed Agricultural COMPAGR
# # 2           Degree Days      DD
# # 3         Precipitation    PRCP
# # 4       Air Temperature    TEMP
# 
# ncdc_datacats(datasetid = "GSOM")$data
# #              name   id
# # 1        Computed COMP
# # 2     Evaporation EVAP
# # 3            Land LAND
# # 4   Precipitation PRCP
# # 5        Sunshine  SUN
# # 6 Air Temperature TEMP
# # 7            Wind WIND

# ncdc(datasetid='GHCND', stationid = "LH000026730",
#      startdate = '2016-10-01', enddate = '2017-08-01')
# ncdc(datasetid='GSOM', stationid = 'LH000026730',
#      startdate = '2008-10-01', enddate = '2017-08-01')
# ncdc(datasetid='NORMAL_MLY', stationid = 'LH000026730',
#      startdate = '2008-10-01', enddate = '2017-08-01')
# # Sorry, no data found 

# df %>% group_by(x, y) %>% slice(from = 1, to = 1)
# meteo_pull_monitors(monitors, keep_flags = FALSE, date_min = NULL,
#                     date_max = NULL, var = "all")
# meteo_spherical_distance(lat1, long1, lat2, long2, units = "deg")
# vilnius_data <- meteo_tidy_ghcnd(stationid = "LH000026730", keep_flags = FALSE, var = daily_datatypes,
#                  date_min = "1999-01-01", date_max = NULL)

# url <- 'https://www.ncei.noaa.gov/data/daily-summaries/access/LH000026730.csv'
# temp <- read.csv(file=url,stringsAsFactors=FALSE, header = TRUE)

# url <- 'https://www.ncei.noaa.gov/data/gsom/access/LH000026730.csv'
# temp <- read.csv(file=url,stringsAsFactors=FALSE, header = TRUE)

# wrapr::let()
# let(c(IDCOL        = idCol,
#       CATEGORYCOL  = categoryCol),
#     d %>% group_by(IDCOL)
# )
# memoise function to speed up by caching results

# stations_with_monthly_data <- DT_monthly %>%
#     pull(id) %>% unique() %>% sort()
# 
# # Compare stations between daily and monthly files
# compare_station_ids(stations_with_daily_data, stations_with_monthly_data)
# 
# stations_not_in_daily <- setdiff(stations_with_monthly_data,stations_with_daily_data)
# stations_not_in_monthly <- setdiff(stations_with_daily_data,stations_with_monthly_data)
# 
# # # Some more comparison checks
# # DT_closest3 %>%
# #     filter(id %in% stations_not_in_monthly) %>%
# #     sumstats()
# # station_data %>%
# #     filter(id %in% stations_not_in_daily) %>%
# #     filter(last_year>=2016) %>%
# #     filter(first_year<=2015) %>%
# #     sumstats()
# # station_data %>%
# #     filter(id %in% stations_not_in_monthly) %>%
# #     filter(last_year>=2016) %>%
# #     filter(first_year<=2015) %>%
# #     sumstats()
# DT_comparison <- DT_stations_dat_all_w %>%
#     full_join(DT_monthly, by=c("id","year","month")) %>%
#     filter(year>=2008 & year <=2015) #focus on recent years
#  
# sumstats(DT_comparison)
#  
# write.xlsx(DT_comparison,"output/comparison_test.xlsx")
# 
# DT_comparison_diff_MMNT <- DT_comparison %>% 
#     filter(abs(MMNT.x-MMNT.y)>2) 
# sumstats(DT_comparison_diff_MMNT)
# DT_comparison_diff_MMXT <- DT_comparison %>% 
#     filter(abs(MMXT.x-MMXT.y)>2) 
# sumstats(DT_comparison_diff_MMXT)
# station_diff_MMXT <- unique(DT_comparison_diff_MMXT$id)
# 
# rm(DT_comparison)
# 
# DT_daily_diff_MMXT <- DT_stations_dat_all %>% 
#     filter(id %in% station_diff_MMXT) %>% 
#     mutate(date = ymd(date)) %>% 
#     mutate(year = year(date),month = month(date)) %>% 
#     filter(year>=2003 & year <=2015) %>% #focus on recent years
#     inner_join(DT_comparison_diff_MMXT,by=c("id","year","month"))
# head(DT_daily_diff_MMXT)
# sumstats(DT_daily_diff_MMXT)
# write.xlsx(DT_daily_diff_MMXT,"output/DT_daily_diff_MMXT.xlsx")


# temp_lt <- DT_stations_data_prep %>% 
#     filter(grepl("LH0",id)) %>% 
#     mutate(year = year(date)) %>% 
#     filter(year>=2015) 

# temp <- DT_m_from_daily %>% 
#     filter(year>=2015) %>% 
#     filter(grepl("LH",id))
# 
# DT_m_from_daily %>% 
#     filter(year<=2016) %>% 
#     filter(TPCP==0) %>% 
#     pull(obs_TPCP) %>% 
#     table()
# 
# DT_m_from_daily %>% 
#     filter(year<=2016) %>% 
#     pull(obs_MMXT) %>% 
#     hist()

# Sys.time()-ptm0

