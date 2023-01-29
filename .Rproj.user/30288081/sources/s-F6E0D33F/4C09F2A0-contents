pkgs <- c("sf", "sp", "rgdal", "spatialEco", "readxl", "dplyr")
sapply(pkgs, require, character.only = T) #load
rm(pkgs)

options(digits = 15)

destination_folder = "C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/Built Environment/Traffic/car traffic/jan-dec2019weekday"
destination_folder = "C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/Built Environment/Traffic/car traffic/jan-dec2019workday"
destination_folder = "C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/Built Environment/Traffic/car traffic/may19-march2020weekday"
setwd(destination_folder)

files = list.files(path = destination_folder, pattern = "*.xlsx", all.files = FALSE,
                   full.names = FALSE, recursive = FALSE,
                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

x = 0
for(file in files){
  if(x == 0){
    stations = read_excel(file, sheet = 1)
    stat_colnames = stations[5,]
    colnames(stations) = stat_colnames
    traffic_intensity = read_excel(file, sheet = 2)
    intens_colnames = traffic_intensity[4,]
    colnames(traffic_intensity) = intens_colnames
    traffic_speed = read_excel(file, sheet = 3)
    speed_colnames = traffic_speed[4,]
    colnames(traffic_speed) = speed_colnames

  }
  else{
    stations_new = read_excel(file, sheet = 1)
    colnames(stations_new) = stat_colnames
    traffic_intensity_new = read_excel(file, sheet = 2)
    colnames(traffic_intensity_new) = intens_colnames
    traffic_speed_new = read_excel(file, sheet = 3)
    colnames(traffic_speed_new) = speed_colnames
    stations = rbind(stations, stations_new)
    traffic_intensity = rbind(traffic_intensity, traffic_intensity_new)
    traffic_speed = rbind(traffic_speed, traffic_speed_new)
  }
  x=1
}
remove(stations_new, traffic_intensity_new, traffic_speed_new)


#clean the datasets
stations = stations[!is.na(stations$Naam),]
stations = stations[stations$Volgnummer != "Volgnummer",]


traffic_intensity = traffic_intensity[!is.na(traffic_intensity$Intensiteit),]
traffic_speed = traffic_speed[!is.na(traffic_speed$`Gemiddelde snelheid`),]

int_intensity=0
int_speed = 0
traffic_intensity$station = ""
traffic_speed$station = ""
for(id in stations$ID){
  traffic_intensity$station[(1+int_intensity):(26+int_intensity)] = id
  traffic_speed$station[(1+int_speed):(25+int_speed)] = id
  int_intensity = int_intensity+26
  int_speed = int_speed + 25
}

traffic_intensity = traffic_intensity[!is.na(traffic_intensity$Intensiteit),]
traffic_intensity = traffic_intensity[traffic_intensity$`uur op de dag` != "uur op de dag",]
traffic_speed = traffic_speed[!is.na(traffic_speed$`Gemiddelde snelheid`),]
traffic_speed = traffic_speed[traffic_speed$`uur op de dag` != "uur op de dag",]

## merge the dataframes

traffic_stats = merge(traffic_intensity, traffic_speed, by = c("uur op de dag", "station"), all.x = T)
colnames(stations)[2] = "station"
traffic_stats = merge(traffic_stats, stations, by = "station", all.x = T)
colnames(traffic_stats)[c(2,3,10)] = c("hour", "traffic_volume", "mean_traffic_speed")
traffic_stats = traffic_stats[order(traffic_stats$station, traffic_stats$hour),]
traffic_stats$timespan = "01.01.2019-31.12.2019"
traffic_stats$timespan = "01.05.2019-31.3.2020"
traffic_stats$weekdays = "workdays - Monday to Friday"
traffic_stats$weekdays = "weekdays - Monday to Sunday"
write.csv(traffic_stats, "traffic_stats_jandec2019_weekday.csv")
write.csv(traffic_stats, "traffic_stats_may19mar20_workday.csv")


## plot station locations
xy = stations[,c(5,4)]
str(xy)
xy$Lengtegraad = as.numeric(as.character(xy$Lengtegraad))
xy$Breedtegraad = as.numeric(as.character(xy$Breedtegraad))
stations = SpatialPointsDataFrame(coords = xy, data = stations,
                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
crs = "+init=EPSG:28992" #Amersfoort / RD New
stations = spTransform(stations, CRSobj = crs)

Amsterdam_Roads = readOGR(dsn="C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/Built Environment/Transport Infrastructure" ,layer="Amsterdam_roads_RDNew")
plot(Amsterdam_Roads, col = "azure4")
plot(stations, add=T, col = "red")

writeOGR(stations, dsn=destination_folder ,layer= "measurementstations",driver="ESRI Shapefile")

