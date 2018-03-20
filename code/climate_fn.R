sourceEnv <- function (file_path) {
    path <- new.env()
    sys.source(file_path, envir = path)
    path
}

enablePaths <- function (paths_location, env_name) {
    assign(env_name, sourceEnv(paths_location), envir = parent.frame())
    if (env_name %in% searchpaths()) {
        warning(paste0("There is aleary \"", env_name, "\" in the search path. Detaching it."))
        expr <- do.call("substitute", list(expression(detach(env))[[1]], 
                                           list(env = as.symbol(env_name))))
        eval(expr)
    }
    expr <- do.call("substitute", list(expression(attach(env))[[1]], 
                                       list(env = as.symbol(env_name))))
    eval(expr, envir = parent.frame())
}

negtozero <- function(x) (abs(x)+x)/2
# library(compiler)
# negtozero.c <- cmpfun(negtozero) #C

predict_w_trend <- function (x) {
    year=1:length(x)
    if ( sum(!is.na(x))>2 ) {
        cl_model <- lm(x ~ year) #Simple model with trend
        new <- data.frame(year = year)
        newx <- predict(cl_model,new)
        newx[!is.na(x)]<-x[!is.na(x)]
        newx
    } else if ( sum(!is.na(x))>0 ){
        newx[is.na(x)]<-mean(x,na.rm=TRUE)
        newx
    } else {
        x
    }
}  

predict_w_mean <- function (x) {
    if ( sum(!is.na(x))>0 ){
        x[is.na(x)]<-mean(x,na.rm=TRUE)
        x
    } else {
        x
    }
}  

`%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}
`%+ana%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, (x+y)/2) )}

countNonMiss <- function(x){as.integer(!is.na(x))}

remove_special <- function (x) {
    x <- iconv(enc2utf8(x),sub="byte")
    x <- gsub("\xe9|<e9>", "e", x)
    x <- gsub("\xe8|<e8>", "e", x)
    x <- gsub("\xf2|\xf3|\xf4|\xf5|\xf6|<f2>|<f3>|<f4>|<f5>|<f6>", "o", x)
    x <- gsub("\xd2|\xd3|\xd4|\xd5|\xd6|<d2>|<d3>|<d4>|<d5>|<d6>", "O", x)
    x <- gsub("\xe1|\xe2|\xe3|\xe4|\xe5|<e1>|<e2>|<e3>|<e4>|<e5>", "a", x)
    x <- gsub("\xE1|\xE2|\xE3|\xE4|\xE5|<E1>|<E2>|<E3>|<E4>|<E5>", "A", x)
    x <- gsub("\xed|<ed>", "i", x)
    x <- gsub("-|,|\\.", " ", x)
    x <- gsub("'", "", x)
    x <- gsub("\xfc|\xfa|<fc>|<fa>", "u", x)
    x <- gsub("\x8a|<8a>", "S", x)
    x <- gsub("\x9a|<9a>", "s", x)
    x <- gsub("\x9e|<9e>", "z", x)
    x <- gsub("\x8e|<8e>", "Z", x)
    x <- gsub("\xf1|<f1>", "n", x)
    x <- gsub("<fc>", "u", x)
    x <- gsub("<dc>", "U", x)
    trim(gsub("  ", " ", x))
}

# Standardize city names
stand_city_names <- function(Country, City) {
    City <- ifelse(trim(City)=="Baranovichi", "Baranovich", City)
    City <- ifelse(trim(City)=="Babruysk", "Bobruysk", City)
    City <- ifelse(trim(City)=="Harbin", "Harbin (Haerbin)", City)
    City <- ifelse(trim(City)=="Essen", "Essen-Oberhausen", City)
    City <- ifelse(trim(City)=="N\xfcrnberg", "N\xfcrnberg-F\xfcrth", City)
    City <- ifelse(Country=="India" & trim(City)=="Hyderabad", "Hyderabad IN", City)
    City <- ifelse(trim(City)=="Thriruvanthapuram", "Thiruvananthapuram", City)
    City <- ifelse(trim(City)=="Visakhapatnam", "Vishakhapatnam", City)
    City <- ifelse(trim(City)=="Klaip?da", "Klaipeda", City)
    City <- ifelse(trim(City)=="Panev?\x9eys", "Paneve\x9eys", City)
    City <- ifelse(trim(City)=="Panev?<9e>ys", "Paneve<9e>ys", City)
    City <- ifelse(trim(City)=="George Town", "Georgetown", City)
    City <- ifelse(trim(City)=="Islamabad-Rawalpindi", "Rawalpindi", City)
    City <- ifelse(trim(City)=="Novi Sad", "Novi sad", City)
    City <- ifelse(trim(City)=="Saragossa", "Saragosa", City)
    City <- ifelse(trim(City)=="El paso, TX", "El Paso, TX", City)
    City <- ifelse(trim(City)=="Lvov", "Lviv", City)
    City <- ifelse(trim(City)=="Vinnytsia", "Vinnytsya", City)
    City <- ifelse(trim(City)=="Zaporozhye", "Zaporizhya", City)
    City <- ifelse(trim(City)=="Cologne", "K\xf6ln", City)
}

## returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Check if supplied country names vector corresponds to EMI country list
check_EMI_cn <- function(x,y=EMI_countries) {
    if(nargs()<2 & !exists("EMI_countries")) stop('EMI_countries should be loaded, or you should supply this list as a second argument')
    if (!(is.vector(x) && mode(x) %in% c("character"))) stop('The first function input should be a character vector of country names')
    if(!(is.vector(y) && mode(y) %in% c("character"))) stop('The second function input should be a character vector of country names')
    if(!(length(y)==210)) warning('EMI_countries vector is not equal to 210 countries')
    z <- sort(setdiff(y,x))
    if (length(z)>0) cat("---------\nThe following",length(z),"country(-ies) missing from 210 EMI country list:\n",paste(z,collapse=", "),"\n")
    z <- sort(setdiff(x,y))
    if (length(z)>0) cat("---------\nThe following",length(z),"country(-ies) not among 210 EMI country list:\n",paste(z,collapse=", "),"\n") 
}

## Get daily climate data for a list of stations 
get_daily_clim <- function(stcodes_list,
                           start_date="1980-01-01", 
                           datatypes=c("TMIN","TMAX","TAVG","SNWD","SNOW","PRCP"), 
                           keep_flags=TRUE) {
    require(rnoaa)
    if (nargs()==0 || !is.list(stcodes_list)) stop("you should provide a list of station codes: e.g. stcodes_list = list('AE000041196')")
    n <- length(stcodes_list) 
    stcode <- stcodes_list[[1]]
    DT_stations_dat_all <- meteo_tidy_ghcnd(stationid = stcode, keep_flags = keep_flags, 
                                            var = datatypes, date_min = start_date, 
                                            date_max = NULL) %>% 
        select(-starts_with("sflag_"),-starts_with("mflag_"))     # remove data source and measurement flags
    if (n>=2) {
        for (ij in 2:n) {
            cat(paste0(round(ij / n * 100,1), '% completed'))
            stcode <- stcodes_list[[ij]]
            data <- try(meteo_tidy_ghcnd(stationid = stcode, keep_flags = keep_flags, 
                                         var = datatypes, date_min = start_date, 
                                         date_max = NULL), silent = T)
            if (any(class(data)=="try-error")) { # try again
                data <- try(meteo_tidy_ghcnd(stationid = stcode, keep_flags = keep_flags, 
                                             var = datatypes, date_min = start_date, 
                                             date_max = NULL), silent = T)
                
            }
            if (any(class(data)=="data.frame")) DT_stations_dat_all <- DT_stations_dat_all %>% bind_rows(data %>% select(-starts_with("sflag_"),-starts_with("mflag_")))
            if (ij == n) cat(': Done')
            else cat('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b')
        }
    }
    meteo_clear_cache() # Clear cashed csv files from NOAA ftp server (from meteo_tidy_ghcnd)
    return(DT_stations_dat_all)
}

# Test API keys, count how many are working
test_APIkeys <- function(APIkeys) {
    APIkeyWorking <- list()
    keynum <- 1
    for ( key in APIkeys ) {
        BREAKFLAG <- FALSE
        options( noaakey = key )
        xdata <- try(ncdc_datasets(), silent = T)
        if (any(class(xdata$meta)=="NULL")) BREAKFLAG <- TRUE
        if ( !BREAKFLAG ) {
            APIkeyWorking [ keynum ] = key
            keynum = keynum+1
        }
    }
    cat("At the moment (", Sys.time(), ") there are", length( APIkeyWorking )
           , "API keys working out of", length( APIkeys ))
    return(APIkeyWorking)
}

# Check weather data availability by indicator type
check_data_type_avail <- function(station_data,
                                  first_year_lim=2015,
                                  last_year_lim=2016,
                                  obs_lim=1000) {
    station_data %>% 
        filter(first_year<=first_year_lim & last_year>=last_year_lim) %>% 
        group_by(element) %>% 
        summarise(obs = n()) %>% 
        filter(obs > obs_lim) %>% 
        arrange(-obs) %>% 
        left_join(DT_noaa_datatypes_daily %>% rename(element=id), by=c("element"))
}

compare_station_ids <- function(x,y) {
    if(nargs()<2 || !is.character(x) || !is.character(y)) stop('This function compares two character sets, which you have to include')
    cat("---------\n")
    cat(length(intersect(y,x)),"stations are in both lists\n")
    if (length(setdiff(y,x))>0) cat(length(setdiff(y,x)),"stations in the 2nd list are missing from the 1st list\n")
    if (length(setdiff(x,y))>0) cat(length(setdiff(x,y)),"stations in the 1st list are missing from the 2nd list\n")
    if (setequal(x,y)) cat("Stations in both lists are the same\n")
}

## Custom function to get city coordinates, using ggmap library
# require(ggmap)
get_coord_ggmap <- function(data) {
    Country <- as.character(data$Country)
    City <- as.character(data$City)
    cityName <- paste0(remove_special(City),", ",remove_special(Country))
    data.frame(Country=Country,City=City,(geocode(cityName))) # Uses ggmap library
}
get_coordinates <- function(city){
    data <- try( get_coord_ggmap(city), silent = T )        
    if ( class(data) == "try-error" ) {
        Sys.sleep(0.5) # wait 0.5 seconds before trying downloading again
        data <- try ( get_coord_ggmap(city), silent = T )        
        if ( class(data) == "try-error" ) {
            cat ( "\n","Country = ", city$Country, ", City = ",
                  city$City, ": downloading FAILED, skipping","\n" )
            data <- data.frame(Country = city$Country, City = city$City, 
                               lon = NA_real_, lat = NA_real_)
        }
    }
    return(data)
}

# Create a nice summary table with number of observations
sumstats = function(x, detailed = FALSE) {
    if ("data.frame" %in% class(x) && nrow(x)>0) {
        sumtable = cbind(
            sapply(x, obs.k),
            if (detailed) sapply(x, miss.k),
            if (detailed) sapply(x, uniq.k),
            if (detailed) substr(sapply(x, class), start=1, stop=3),
            # if (detailed) t(x[1, ]),
            sapply(x, mean.k),
            sapply(x, median.k),
            sapply(x, sd.k),
            sapply(x, min.k),
            sapply(x, max.k)
        )
        sumtable <- as.data.frame(sumtable)
        if (detailed) {
            names(sumtable) = c("Obs", "Miss", "Uniq.Val", "Type", 
                                "Mean", "Median", "Std.Dev", "Min", "Max")
        } else {
            names(sumtable) = c("Obs", "Mean", "Median", "Std.Dev", "Min", "Max")
        }
        return(sumtable)
    } else {
        return(names(x))
    }
} 
# sub-functions for ## sumstats ## function
pretty_digits <- function(x, # format long numbers as scientific
                          dig1 = 5,
                          dig2 = 3,
                          thresh = 1e6) {
    if (is.na(x) || !is.numeric(x)) {
        x
    } else if (abs(x) > thresh/100 & abs(x) <= thresh) {
        prettyNum(x, digits = dig1+1)
    } else if (abs(x) > 0.1 & abs(x) <= thresh/100) {
        prettyNum(x, digits = dig1)
    } else if (abs(x) > (100 / thresh) & abs(x) <= 0.1) {
        prettyNum(x, digits = 2)
    } else if (abs(x) > (1 / thresh) & abs(x) <= (100 / thresh)) {
        prettyNum(x, digits = 1)
    } else {
        prettyNum(x, digits = dig2, scientific = TRUE)
    }
}
obs.k = function(x) { # number of nonmissing observations
    if (is.numeric(x))
        sum(as.integer(!is.na(x)))
    else
        sum(as.integer(!(as.character(x) %in% 
                             c(""," ","NA","NAN",
                               "NULL","na","nan","NaN") | is.na(x))))
}
miss.k = function(x) { # number of missing observations
    if (is.numeric(x))
        sum(as.integer(is.na(x)))
    else
        sum(as.integer((as.character(x) %in% 
                            c(""," ","NA","NAN",
                              "NULL","na","nan","NaN") | is.na(x))))
}
uniq.k = function(x) { # number of unique values
    length(unique(x[!is.na(x)]))
}
mean.k = function(x) {
    if (is.numeric(x))
        suppressWarnings(pretty_digits(mean(x, na.rm = TRUE)))
    else
        "N*N"
}
median.k = function(x) {
    if (is.numeric(x))
        suppressWarnings(pretty_digits(median(x, na.rm = TRUE)))
    else
        "N*N"
}
sd.k = function(x) {
    if (is.numeric(x))
        suppressWarnings(pretty_digits(sd(x, na.rm = TRUE)))
    else
        "N*N"
}
min.k = function(x) {
    if (is.numeric(x))
        suppressWarnings(pretty_digits(min(x, na.rm = TRUE)))
    else
        suppressWarnings(substr(min(as.character(x), na.rm = TRUE), start=1, stop=6))
}
max.k = function(x) {
    if (is.numeric(x))
        suppressWarnings(pretty_digits(max(x, na.rm = TRUE)))
    else
        suppressWarnings(substr(max(as.character(x), na.rm = TRUE), start=1, stop=6))
}

mem_used <- function() {
    show_bytes(sum(gc()[, 1] * c(node_size(), 8)))
}
show_bytes <- function(x) {
    structure(x, class = "bytes")
}
node_size <- function() {
    bit <- 8L * .Machine$sizeof.pointer
    if (!(bit == 32L || bit == 64L)) {
        stop("Unknown architecture", call. = FALSE)
    }
    
    if (bit == 32L) 28L else 56L
}
print.bytes <- function(x, digits = 3, ...) {
    power <- min(floor(log(abs(x), 1000)), 4)
    if (power < 1) {
        unit <- "B"
    } else {
        unit <- c("kB", "MB", "GB", "TB")[[power]]
        x <- x / (1000 ^ power)
    }
    
    formatted <- format(signif(x, digits = digits), big.mark = ",",
                        scientific = FALSE)
    
    cat(formatted, " ", unit, "\n", sep = "")
}