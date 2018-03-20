## This code creates city_data.csv with city coordinates for 210 Passport countries 
## Preliminaries -----
rm ( list = ls())

# turn off scientific notation in most output
options(scipen = 15)
# increase the max size of output file
options(max.print=10E6)
        
cat("\014") # clear console

if (!require("pacman")) install.packages("pacman")
pacman::p_load(foreign, data.table, tidyverse, stringr)

source("code/climate_fn.R")

# Load paths
enablePaths('code/00_file_paths.R', 'paths')

# if the analysis output directory doesn't exist, create it
dir.create ( output_dir, showWarnings = FALSE )

#Load 210 EMI country list
EMI_countries <- sort(read.csv(file=EMI_countries_f,stringsAsFactors=FALSE, header = FALSE)$V1)
#Load 80 EMI country list
EMI_countries80 <- sort(unique(read.csv(file=cities_list_f,stringsAsFactors=FALSE, header = TRUE)$Country))

## Get coordinates for cities in Passport City database using google ----
## (this the alternative to Populated Places database)
UPDATE_COORDINATES <- FALSE # Run this only if needed as it takes a long time
if (!file.exists(city_coordinates_f)) UPDATE_COORDINATES <- TRUE
if (UPDATE_COORDINATES) {
    pacman::p_load(ggmap)  # to get city coordinates
    DT_cities_list <- data.table( read.table( file = cities_list_f, header = TRUE
                                            , sep = ",", quote = "\"", 
                                            dec = ".", fill = TRUE, comment.char = "", 
                                            stringsAsFactors = FALSE, na.strings = "-99" ))
    head(DT_cities_list)
    sumstats(DT_cities_list)
    # Check if custom function getCoordinates is working
    get_coordinates(data_frame(Country="Lithuania",City="Vilnius"))
    DT_city_coord <- DT_cities_list %>% 
        get_coordinates()
        # rowwise() %>% map_df(get_coordinates(.))
    if (nrow(DT_city_coord)>0) write.table(DT_city_coord, file = city_coordinates_f, sep = ",", row.names = FALSE, na = "" )
}

DT_city_coord <- fread(city_coordinates_f)
DT_city_coord[, City:=stand_city_names(Country,City)] # Standardize city names
sumstats(DT_city_coord)
check_EMI_cn(DT_city_coord$Country)

# Add city regions data from Kasparas
DT_city_regs <- fread(city_regions_f)
sumstats(DT_city_regs)
DT_city_regs[CountryName=="UK",CountryName:="United Kingdom"]
DT_city_regs[CountryName=="UAE",CountryName:="United Arab Emirates"]
DT_city_regs[, CityName:=stand_city_names(CountryName,CityName)] # Standardize city names
check_EMI_cn(DT_city_regs$CountryName)
DT_city_regs[,':='(CitySh=remove_special(CityName),CountrySh=remove_special(CountryName))]
DT_city_regs[CitySh=="xi an",':='(CitySh="xian")]
# Clean up city names
DT_alt_city_names <- fread(alt_city_names_f)
DT_alt_city_names[,':='(CountrySh=remove_special(Country),CitySh=remove_special(ALTname))]
set( DT_alt_city_names, j = c( "Country","ALTname" ), value = NULL )
if (nrow(DT_alt_city_names[duplicated(DT_alt_city_names,by=c("CountrySh","CitySh")),])>0) stop("DT_alt_city_names should not have duplicated rows")
DT_city_regs[,':='(CitySh=remove_special(CityName),CountrySh=remove_special(CountryName))]
DT_city_regs <- merge(DT_city_regs,DT_alt_city_names,by=c("CountrySh","CitySh"),all.x=TRUE)
DT_city_regs[EMIname!="",CityName:=EMIname]
set( DT_city_regs, j = c( "EMIname"), value = NULL )
DT_city_regs[CountryName=="Brazil",CityName:=gsub(" metro", "", CityName)]
# Leave data only for 80 Passport countries
DT_city_regs <- subset(DT_city_regs,CountryName %in% EMI_countries80)
DT_city_regs[,':='(CitySh=remove_special(CityName),CountrySh=remove_special(CountryName))]
DT_city_coord[,':='(CitySh=remove_special(City),CountrySh=remove_special(Country))]
DT_city_coord[CitySh=="xi an",':='(CitySh="xian")]
DT_city_coord <- merge(DT_city_coord,DT_city_regs,by=c("CountrySh","CitySh"),all=TRUE)
DT_city_coord[Country %in% c("Hong Kong, China","Singapore"), Region:=Country]
sumstats(DT_city_coord)

if (nrow(DT_city_coord[is.na(Country),])>0) {
  stop("Not all city names were matched")
  print(DT_city_coord[is.na(Country),])
}
DT_city_coord[is.na(CountryCode),] #Should be only Hong Kong and Singapore with some data missing

# Add pop numbers
DT_city_pop <- fread( pop_f, header= TRUE )
table(DT_city_pop$Subcategory)
DT_city_pop[,pop:=get("2014")*get("Unit Multiplier")]
DT_city_pop <- subset(DT_city_pop,Region %in% EMI_countries80
                      & trim(Subcategory)=="Population: National Estimates at January 1st"
                      , select=c(Region,Country,pop))
setnames(DT_city_pop,c("Region","Country"),c("Country","City"))
DT_city_pop[City=="St Petersburg",City:="Saint Petersburg"]
DT_city_pop[,':='(Country=trim(Country),City=trim(City))]
DT_city_pop[, City:=stand_city_names(Country,City)] # Standardize city names
DT_city_coord[,':='(Country=trim(Country),City=trim(City))]
DT_city_coord <- merge(DT_city_coord,DT_city_pop,by=c("Country","City"),all=TRUE)
sumstats(DT_city_coord)

## Prepare City data from alternative source
## Data on City pop for all countries int the world ----------
## source (Populated Places): http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
DT_city_data <- data.table(foreign::read.dbf(city_lats_f))
str(DT_city_data)

setnames( DT_city_data, c( "ADM0NAME","ADM1NAME" ), c( "Country","Region" ))
DT_city_data$Country <- as.character(DT_city_data$Country)
# Match country names to Passport
altCnNames <- c( "Hong Kong S.A.R.","United States of America","Bosnia and Herzegovina","Necw Zealand",
                 "Congo (Brazzaville)","United States Virgin Islands","Saint Lucia","Saint Kitts and Nevis",
                 "Congo (Kinshasa)","Macau S.A.R","Guinea Bissau","Sao Tome and Principe",
                 "Antigua and Barbuda","Saint Vincent and the Grenadines","The Bahamas","The Gambia",
                 "Ivory Coast","Korea, South","Korea, North","Kingdom of Norway","Kingdom of Spain",
                 "Kingdom of the Netherlands","Republic of Serbia","Bahamas, The","Gambia, The",
                 "United States","United Republic of Tanzania","French Republic",
                 "Aland","Palestine","Faroe Islands","Greenland")
EMICnNames <- c( "Hong Kong, China","USA","Bosnia-Herzegovina","New Zealand"
                 ,"Congo-Brazzaville","US Virgin Islands","St Lucia","St Kitts",
                 "Congo, Democratic Republic","Macau","Guinea-Bissau","Sao Tom\xe9 e Pr\xedncipe",
                 "Antigua","St Vincent and the Grenadines","Bahamas","Gambia",
                 "C\xf4te d'Ivoire","South Korea","North Korea","Norway","Spain",
                 "Netherlands","Serbia","Bahamas","Gambia",
                 "USA","Tanzania","France","Finland","Israel","Denmark","Denmark")
# EMICnNames <- c( "Hong Kong, China","USA","Bosnia-Herzegovina","New Zealand"
#                  ,"Congo-Brazzaville","US Virgin Islands","St Lucia","St Kitts",
#                  "Congo, Democratic Republic","Macau","Guinea-Bissau","Sao Tomé e Príncipe",
#                  "Antigua","St Vincent and the Grenadines","Bahamas","Gambia",
#                  "Côte d'Ivoire","South Korea","North Korea","Norway","Spain",
#                  "Netherlands","Serbia","Bahamas","Gambia",
#                  "USA","Tanzania","France","Finland","Israel","Denmark","Denmark")

for (ij in seq(1,length(altCnNames),1)) {
    DT_city_data[Country == altCnNames[ij], ':='( Country = EMICnNames[ij] )]
    DT_city_data[SOV0NAME == altCnNames[ij], ':='( SOV0NAME = EMICnNames[ij] )]
}
test1 <- unique(subset(DT_city_data,!(Country %in% EMI_countries) & (SOV0NAME %in% EMI_countries),select=c(Country,SOV0NAME)))
cat(paste0("\"",test1$SOV0NAME,"\"",collapse=","))
check_EMI_cn(DT_city_data$Country)
check_EMI_cn(as.character(DT_city_data$SOV0NAME))

## Additional cities coordinates missing from the main file
DT_city_data_add <- fread(city_lats_additional_f)
DT_city_data_add
DT_city_data_add[, ADM1NAME:=ADM0NAME]
setnames( DT_city_data_add, c( "ADM0NAME","ADM1NAME" ), c( "Country","Region" ))
DT_city_data <- merge(DT_city_data,DT_city_data_add,by=names(DT_city_data_add),all=TRUE)

nrow(DT_city_data)
subset(DT_city_data, Country=="Nauru")
DT_city_data <- subset( DT_city_data, POP_MAX>1000 ) ## Select cities with certain pop size
nrow(DT_city_data)

#Check the size of remaining pop ( not in 210 countries )
paste0( "pop in towns in the 210 Passport research country list: ", signif ( sum ( as.numeric ( subset( DT_city_data, Country %in% EMI_countries, select = c( "POP_MAX" ))$POP_MAX))/1000000, digits = 5 ), " mln" )
paste0( "pop in towns in other countries: ", signif ( sum ( as.numeric ( subset( DT_city_data, !(Country %in% EMI_countries), select = c( "POP_MAX" ))$POP_MAX))/1000000, digits = 3 ), " mln" )

check_EMI_cn(DT_city_data$Country)

## Keep only 210 Passport countries
DT_city_data <- subset( DT_city_data, Country %in% EMI_countries)
nrow(DT_city_data)
if (nrow(subset( DT_city_data, is.na ( ADM0_A3 ), select=c(Country,NAMEASCII,POP_MAX)))>0) warning("ADM0_A3 should have no missing values") #should be empty

# Clean up city names
DT_US_states <- fread(us_state_codes_f, header = TRUE)
DT_city_data <- merge(DT_city_data,DT_US_states,by=c("Country","Region"),all.x=TRUE)
setnames(DT_city_data,c("Country","NAMEASCII","Region"),c("Country2","CityAltern","RegionAlt"))
DT_city_data[!is.na(StateCode),CityAltern:=paste0(CityAltern,", ",StateCode)]
# Rename not to have the same city names in the same country
DT_city_data[Country2=="Canada" & RegionAlt=="Nova Scotia" & CityAltern=="Windsor",CityAltern:="Windsor2"]
DT_city_data[Country2=="Brazil" & RegionAlt=="Amazonas" & CityAltern=="Natal",CityAltern:="Natal2"]
DT_city_data[Country2=="China" & RegionAlt=="Anhui" & CityAltern=="Suzhou",CityAltern:="Suzhou2"]
nrow(DT_city_data)
# Remove this small town, as there are problems with renaming it
DT_city_data <- subset(DT_city_data,!(Country2=="Russia" & RegionAlt=="Yaroslavl'" & CityAltern=="Rostov"))
# Remove these small towns with the same names as larger towns in different regions in the same Country
DT_city_data <- subset(DT_city_data,!(Country2=="Russia" & RegionAlt=="Yamal-Nenets" & CityAltern=="Nakhodka"))
DT_city_data <- subset(DT_city_data,!(Country2=="Russia" & RegionAlt=="Evenk" & CityAltern=="Noginsk"))
DT_city_data <- subset(DT_city_data,!(Country2=="Brazil" & RegionAlt=="Amazonas" & CityAltern=="Crato"))
DT_city_data <- subset(DT_city_data,!(Country2=="Brazil" & RegionAlt=="Amapi" & CityAltern=="Vila Velha"))
nrow(DT_city_data)
DT_city_data[,':='(CitySh=remove_special(CityAltern),CountrySh=remove_special(Country2))]
DT_city_data <- merge(DT_city_data,DT_alt_city_names,by=c("CountrySh","CitySh"),all.x=TRUE)
head(subset(DT_city_data,EMIname!="",select=c(Country2,EMIname,CityAltern,CitySh)))
DT_city_data[EMIname!="",CityAltern:=EMIname]
set( DT_city_data, j = c( "EMIname"), value = NULL )

DT_city_data2 <- subset(DT_city_data,select=c(CountrySh,CitySh,Country2,RegionAlt,CityAltern,ADM0_A3,LATITUDE,LONGITUDE,POP_MAX))
sumstats(DT_city_data2)
# DT_city_data2[is.na(RegionAlt),]
# DT_city_data[Country2=="Russia" & grepl("Yaroslavl", RegionAlt) & CityAltern=="Rostov-on-Don",c(CountrySh,CitySh,Country2,RegionAlt,CityAltern,ADM0_A3,LATITUDE,LONGITUDE,POP_MAX)]
DT_city_data2[, CityAltern:=stand_city_names(Country2,CityAltern)]
DT_city_data2[,':='(CitySh=remove_special(CityAltern),CountrySh=remove_special(Country2))]
sumstats(DT_city_data2)

# Combine city data from two data sources
DT_city_data3 <- merge(DT_city_data2,DT_city_coord,by=c("CountrySh","CitySh"),all=TRUE)
sumstats(DT_city_data3)
head(subset(DT_city_data3,is.na(Country2)))
# There are multiple cities with the same name in the same country (but different region) - Create new names for them
DT_city_data4 <- merge(DT_city_data3,DT_city_data3[duplicated(DT_city_data3,by=c("CountrySh","CitySh")),c("CountrySh","CitySh"),with=FALSE],by=c("CountrySh","CitySh"))
sumstats(DT_city_data4)
 
setkey(DT_city_data4, NULL)
DT_city_data4 <- unique(DT_city_data4)
DT_city_data4[,nameRank:=rank(RegionAlt,ties.method = "first" ),by=c("CountrySh","CitySh")]

DT_city_data3 <- merge(DT_city_data3,subset(DT_city_data4,select=c("CountrySh","CitySh","RegionAlt","nameRank")),by=c("CountrySh","CitySh","RegionAlt"),all.x=TRUE)

subset(DT_city_data3,is.na(LATITUDE),select=c(CountrySh,CitySh,POP_MAX,pop))
# subset(DT_city_data3,CountrySh=="France",select=c(CountrySh,CitySh,POP_MAX,pop))

DT_city_data3[,CitySh:=paste0(CitySh,ifelse(is.na(nameRank) | nameRank==1,"",nameRank))]
DT_city_data3[,CityAltern:=paste0(CityAltern,ifelse(is.na(nameRank) | nameRank==1,"",nameRank))]
DT_city_data3$nameRank<-NULL

# write.table(DT_city_data3, file = "output/test3d.csv", sep = ",", row.names = FALSE, na = "" )

DT_city_data <- DT_city_data3
sumstats(DT_city_data)
# Combine variables to one dataset
DT_city_data[is.na(Country),':='(Country=Country2,City=CityAltern,Region=RegionAlt,lon=LONGITUDE,lat=LATITUDE,pop=as.numeric(POP_MAX))]
sumstats(DT_city_data)

subset(DT_city_data,is.na(Country) & !is.na(CountrySh))

subset(DT_city_data,CountrySh=="Lithuania")

# Add IMIS regions
DT_regions <- fread(regions_f, header = TRUE)
DT_city_data <- merge(DT_city_data,subset(DT_regions,Region!=""),by=c("Country","Region"),all.x=TRUE)
sumstats(subset(DT_city_data,Country %in% c("China","India")))

DT_city_data <- subset(DT_city_data, select=c(Country,City,Region,lon,lat,pop,CityCodeID,Type,IMISRegion))
DT_city_data[is.na(Region),Region:=City]

DT_city_data[Country %in% c("Singapore","Hong Kong, China"),Type:="City-Country"]

# Create city ID's
DT_city_data[, cityRank:= rank ( -pop, ties.method = "first" ), by = c( "Country" )]
DT_city_data[, cityID:= paste0( str_sub(remove_special(Country), 1, 3),str_sub(remove_special(Country), -3, -1), str_sub ( "0000", 1+nchar ( cityRank )), cityRank )]
set ( DT_city_data, j = c( "cityRank" ), value = NULL )
head(DT_city_data)
tail(DT_city_data)
DT_city_data[Country=="Syria",]
if (length(which(duplicated(DT_city_data$cityID)))>0) stop('City IDs should be unique')
sumstats(DT_city_data)
write.table(DT_city_data, file = city_data_f, sep = ",", row.names = FALSE, na = "" )
check_EMI_cn(DT_city_data$Country)

detach(paths)
