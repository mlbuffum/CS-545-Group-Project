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
library(rpart)
library(rpart.plot)

## Set directories
local.path <- "C:/Users/maggi/Documents/Masters - Local/CS 545/Group Project"
rawdata.path <- file.path(local.path,"Raw Data")
rData.path <- file.path(local.path,"R Data")


################################################################################
##### Prepare Workspace
################################################################################

train.dat <- readRDS(file.path(rData.path,"merged_train.rds"))

sub.dat <- train.dat

sub.dat.wide <- dcast(
  sub.dat,
  site_id + DateTime + building_id + primary_use +
    square_feet + air_temperature + dew_temperature + sea_level_pressure +
    wind_direction + wind_speed ~
    MeterType, value.var = "meter_reading"
)

sub.dat.wide[,month := strftime(DateTime,format = "%m")]
sub.dat.wide[,day   := strftime(DateTime,format = "%j")]
sub.dat.wide[,HB    := strftime(DateTime,format = "%H")]
sub.dat.wide[,DayOfWeek := strftime(DateTime,format = "%A")]

# merge on metertypes indicator
meters_in_buildings <- unique(sub.dat[,.(building_id,MeterType)])
meters_in_buildings[,ElectricityMeter  := ifelse(MeterType == "Electricity" ,1,0)]
meters_in_buildings[,ChilledWaterMeter := ifelse(MeterType == "ChilledWater",1,0)]
meters_in_buildings[,HotWaterMeter     := ifelse(MeterType == "HotWater"    ,1,0)]
meters_in_buildings[,SteamMeter        := ifelse(MeterType == "Steam"       ,1,0)]

meters_in_buildings.unique <- meters_in_buildings[,.(ElectricityMeter =sum(ElectricityMeter),
                                                     ChilledWaterMeter=sum(ChilledWaterMeter),
                                                     HotWaterMeter    =sum(HotWaterMeter),
                                                     SteamMeter       =sum(SteamMeter)),
                                                  by=building_id]

sub.dat.type <- merge(
  sub.dat.wide,
  meters_in_buildings.unique,
  by="building_id",
  all.x=T
)

# sub.dat.type[HotWaterMeter     == 0,HotWater     := 0]
# sub.dat.type[ChilledWaterMeter == 0,ChilledWater := 0]
# sub.dat.type[SteamMeter        == 0,Steam        := 0]

tree1 = rpart(
  Electricity ~
    air_temperature +
    primary_use     +
    square_feet     +
    HB              +
    month           +
    DayOfWeek       +
    ChilledWaterMeter    +
    SteamMeter           +
    HotWaterMeter        ,
  data = sub.dat.type)

prp(tree1)



printcp(tree1)
plotcp(tree1)
































