################################################################################
##### CS 545 Group Project
##### Maggie Buffum
################################################################################

################################################################################
##### Prepare Workspace
################################################################################

## Clear Workspace
rm(list = ls())

## Load packages
library(data.table)
library(fasttime)

## Set directories
local.path <- "C:/Users/maggi/Documents/Masters - Local/CS 545/Group Project"
rawdata.path <- file.path(local.path,"Raw Data")
rData.path <- file.path(local.path,"R Data")


################################################################################
##### Prepare Data
################################################################################

## Import training data --------------------------------------------------------
train.dat <- fread(file = file.path(rawdata.path,"train.csv"))

## Adjust time stamp -- convert to date format
train.dat[,DateTime := fastPOSIXct(timestamp) + 8*60*60]
train.dat[,timestamp := NULL]

## Label the meter types
train.dat[meter == 0,MeterType := "Electricity" ]
train.dat[meter == 1,MeterType := "ChilledWater"]
train.dat[meter == 2,MeterType := "Steam"       ]
train.dat[meter == 3,MeterType := "HotWater"    ]

train.dat[,MeterType := as.factor(MeterType)]
levels(train.dat$MeterType)

## How much missing data is there for each building and meter type?
NA.fn <- function(x) length(which(is.na(x)))/length(x)
missing.dat <- train.dat[,lapply(.SD,NA.fn),
                         by = .(building_id,MeterType),
                         .SDcols = c("meter_reading")]
missing.dat2 <- missing.dat[,.(propNA = mean(meter_reading)),
                            by=MeterType]
  # None missing.

## How many buildings have each meter?
meter.count <- train.dat[,.N,by=.(building_id,MeterType)]
table(meter.count$MeterType)          # 1,413 /w electricity
length(unique(train.dat$building_id)) # 1,449 total


## Import building metadata ----------------------------------------------------
building.dat <- fread(file = file.path(rawdata.path,"building_metadata.csv"))

## which fields look useful?
str(building.dat)
missing.dat <- building.dat[,lapply(.SD,NA.fn),
                         by = .(building_id),
                         .SDcols = c("primary_use",
                                     "square_feet",
                                     "year_built" ,
                                     "floor_count")]
summary(missing.dat)
  # year_built and floor_count are missing for a majority of buildings; let's
  # not include these in the analysis.

## Merge onto training data
train_build.dat <- train.dat[building.dat   ,
                             .(building_id  ,
                               site_id      ,
                               primary_use  ,
                               square_feet  ,
                               MeterType    ,
                               DateTime     ,
                               meter_reading),
                             on="building_id"]

NROW(train_build.dat)==NROW(train.dat)

## Import weather data ---------------------------------------------------------
weather.dat <- fread(file = file.path(rawdata.path,"weather_train.csv"))

## Adjust time stamp -- convert to date format
weather.dat[,DateTime := fastPOSIXct(timestamp) + 8*60*60] # convert to utc
weather.dat[,timestamp := NULL]

## which fields look useful?
str(weather.dat)
missing.dat <- weather.dat[,lapply(.SD,NA.fn),
                            by = .(site_id),
                            .SDcols = c("air_temperature"   ,
                                        "cloud_coverage"    ,
                                        "dew_temperature"   ,
                                        "precip_depth_1_hr" ,
                                        "sea_level_pressure",
                                        "wind_direction"    ,
                                        "wind_speed"        )]
summary(missing.dat)
  # air temperature, dew temp, sea level pressure, wind direction, and wind
  # speed all look OK, but cloud coverage and precipitation depth are missing
  # for a lot of sites.

## Merge onto training data
train_weather.dat <- merge(
  train_build.dat,
  weather.dat[,.(DateTime          ,
                 site_id           ,
                 air_temperature   ,
                 dew_temperature   ,
                 sea_level_pressure,
                 wind_direction    ,
                 wind_speed        )],
  by=c("site_id","DateTime"),
  all.x=T
)

NROW(train_weather.dat)==NROW(train.dat)


################################################################################
##### Save Preliminary Data
################################################################################

saveRDS(train_weather.dat,file.path(rData.path,"merged_train.rds"))




















