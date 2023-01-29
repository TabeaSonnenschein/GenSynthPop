#criteria

#1 Population density
#2 Density of retail and service destinations (retail environment)
#3 Land-use mix (commercial, cultural) Shannon Entropy
#4 Street connectivity (intersection density)
#5 Green space
#6 Side walk density
#7 Public Transport Density


#optional criteria

# Safety from crime
# Traffic safety
# Traffic volume and speed
# Pedestrian crossing availability
# Aesthetics
# Air quality
# Shade or sun in appropriate seasons
# Street furniture
# Wind conditions
# Specific walking destinations such as light rail stops and bus stops (Brown et al., 2009)
# Job density


pkgs = c("maptools","raster", "rgdal","sp", "sf", "jpeg", "data.table", "purrr", "rgeos" , "leaflet", "RColorBrewer",
         "ggplot2", "lattice",  "raster",  "spatialEco", "rjson", "jsonlite","EconGeo", "dplyr",
         "rstan", "boot",  "concaveman", "data.tree", "DiagrammeR", "networkD3", "rgexf", "tidytree", "exactextractr", "terra")
sapply(pkgs, require, character.only = T) #load
rm(pkgs)

dataFolder= "C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam"

crs = "+init=EPSG:28992" #Amersfoort / RD New
crs_name = "RDNew"
CRS_defin = "+towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725"
extent = readOGR(dsn=paste(dataFolder, "/SpatialExtent", sep = "" ),layer="Amsterdam Diemen Oude Amstel Extent")
extent = spTransform(extent, CRSobj = crs)
city = "Amsterdam"
raster_size = 200
setwd(paste(dataFolder, "/Built Environment/Transport Infrastructure", sep = ""))
walkability_grid = readOGR(dsn=getwd(),layer=paste("walkability_grid_",raster_size, sep=""))
colnames(walkability_grid@data) = c("unique_id",  "int_id", "population_density", "retail_density" ,
                                    "green_coverage_fraction", "public_trans_density", "street_intersection_density",
                                    "Nr_Traffic_accidents", "Nr_Traffic_Pedestrian_Accidents", "Nr_Trees",
                                    "averageTrafficVolume", "sumTrafficVolume", "MetersMajorStreets",
                                    "Pedestrian_Street_Width",  "Pedestrian_Streets_Length", "len_intersec_bikeroute",
                                    "dist_CBD", "retail_diversity", "MaxSpeedLimit", "MinSpeedLimit", "MeanSpeedLimit",
                                    "Nr_StreetLights", "crime_incidence", "max_noise_day",  "max_noise_night",
                                    "openspace_fraction",  "Nr_ParkingSpaces", "perc_non_western", "perc_welfare_dependent",
                                    "ParkingPrices_pre", "ParkingPrices_post")
csv = as.data.frame(walkability_grid)
write.csv(csv, "walkability_measures2.csv" )

walkability_grid_raster <- raster("walkability_grid.tif")


########################################
# Making a grid of size: 100mx100m
########################################
walkability_grid_raster = raster(xmn=extent(extent)[1], xmx=extent(extent)[2], ymn=extent(extent)[3], ymx=extent(extent)[4])
raster::res(walkability_grid_raster) <- raster_size
projection(walkability_grid_raster) = crs
ncells = ncell(walkability_grid_raster)
dim(walkability_grid_raster)
walkability_grid = rasterToPolygons(walkability_grid_raster)
walkability_grid@data[("unique_id")] = paste("id_", 1:ncells, sep = "")
plot(walkability_grid,border='black', lwd=1)
walkability_grid$int_id = gsub("id_", "", walkability_grid$unique_id)
as.numeric(walkability_grid$int_id)
raster::values(walkability_grid_raster) = as.numeric(walkability_grid$int_id)

?raster
########################################
#1 Population density
########################################
setwd(paste(dataFolder, "/Population/Worldpop", sep = ""))
Population = raster("nld_ppp_2020_UNadj_constrained.tif") #Warsaw
Population = projectRaster(Population,
              crs = crs)
Population_extent = crop(Population, extent(extent))
Population_extent = rasterToPolygons(Population_extent)
pop_cells = length(Population_extent)
Population_extent@data[("unique_id_pop")] = paste("popgrid_", 1:pop_cells, sep = "")
summary(Population_extent)
Population_extent_centroids = gCentroid(Population_extent, byid= T, id= Population_extent$unique_id_pop)
Population_extent_centroids$pop = Population_extent$nld_ppp_2020_UNadj_constrained
population_gridjoin = point.in.poly(Population_extent_centroids, walkability_grid)

walkability_grid$population_density =0
for(x in walkability_grid$unique_id){
 walkability_grid$population_density[which(walkability_grid$unique_id == x)] = sum(population_gridjoin@data[which(population_gridjoin$unique_id == x), c("pop")])
}

#analysis
summary(walkability_grid$population_density)
plot(walkability_grid, col= walkability_grid$population_density)
plot(Population_extent, add=T)


########################################
#2 Retail density and diversity
########################################
setwd(paste(dataFolder, "/Built Environment/Facilities", sep = ""))

Fsq_Arts = read.csv(paste(city, "_Foursquarevenues_ArtsEntertainment.csv", sep = ""))
Fsq_Food = read.csv(paste(city, "_Foursquarevenues_Food.csv", sep = ""))
Fsq_Nightlife = read.csv(paste(city, "_Foursquarevenues_Nightlife.csv", sep = ""))
Fsq_Shops = read.csv(paste(city, "_Foursquarevenues_ShopsServ.csv", sep = ""))

Fsq_retail= rbind(Fsq_Arts, Fsq_Food, Fsq_Nightlife, Fsq_Shops)
Fsq_retail= unique(subset(Fsq_retail, select= -c(X)))
coordinates(Fsq_retail)= ~lon+lat
proj4string(Fsq_retail)=CRS("+proj=longlat +datum=WGS84")
Fsq_retail = spTransform(Fsq_retail, CRSobj = crs)
Fsq_retail_extent = raster::crop(Fsq_retail, extent(extent))
Fsq_retail_gridjoin = point.in.poly(Fsq_retail_extent, walkability_grid)

#Density
walkability_grid$retail_density =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$retail_density[which(walkability_grid$unique_id == x)] = length(which(Fsq_retail_gridjoin$unique_id == x))
}

#Analysis
summary(walkability_grid$retail_density)

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$retail_density), size= 0.01)+
  scale_fill_viridis_c(option = "C")

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$retail_density), size= 0.01)+
  scale_fill_viridis_c(option = "C") +
  geom_point(data = st_as_sf(Fsq_retail_extent), aes(colour = "red", size = 1))

?geom_point

# Diversity
entropy = function(mat) {
  freqs = mat/rowSums (mat)
  entropy = - rowSums (freqs * log2(freqs+0.000000001))
  entropy = round (entropy, digits = 3)
  return (entropy)
}

venuecategories = unique(Fsq_retail_gridjoin@data$categoryid)
uniqids = unique(Fsq_retail_gridjoin@data$unique_id)
venuecat_matrix = matrix(data = 0, nrow = length(uniqids), ncol = length(venuecategories))
colnames(venuecat_matrix) = venuecategories
for(i in 1:length(venuecategories)){
  x = Fsq_retail_gridjoin@data[Fsq_retail_gridjoin@data$categoryid == venuecategories[i],]
  y = x %>% count(unique_id)
  venuecat_matrix[which(uniqids %in% y$unique_id),i] = y$n
}
uniqids = as.data.frame(uniqids)
uniqids$amen_entropy = entropy(venuecat_matrix)


walkability_grid$retail_diversity =0
for(x in uniqids$uniqids){
  walkability_grid$retail_diversity[which(walkability_grid$unique_id == x)] = uniqids[which(uniqids$uniqids== x),"amen_entropy"]
}


#Analysis
summary(walkability_grid$retail_density)

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$retail_diversity), size= 0.01)+
  scale_fill_viridis_c(option = "D")


plot(Fsq_retail_extent, add = T)


################## needs redoing
#######################################
#4 Street connectivity (intersection density) #
#######################################
setwd(paste(dataFolder, "/Built Environment/Transport Infrastructure", sep = ""))
Streets = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure", sep = "" ),layer="Amsterdam_roads_RDNew")
Streets = spTransform(Streets, CRSobj = crs)
plot(Streets, add = T)
Streets = st_as_sfc(Streets)
street_intersections = st_intersection(Streets)
street_intersections_df = as.data.frame(street_intersections)
street_intersections_df$nr_coordinates = nchar(street_intersections_df$geometry)/16
street_intersections_df = street_intersections_df[which(street_intersections_df$nr_coordinates < 3),]

intersection_points_data = as.data.frame(matrix(NA, nrow = length(intersection_points)))
intersection_points = as(street_intersections_df$geometry, "Spatial")
intersection_points= SpatialPointsDataFrame(intersection_points, data = intersection_points_data)
writeOGR(intersection_points, dsn=getwd() ,layer= "intersection_points",driver="ESRI Shapefile")

plot(Streets)
plot(intersection_points, col= "Red", add= T)

street_intersections_gridjoin = point.in.poly(intersection_points, walkability_grid)

walkability_grid$street_intersection_density =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$street_intersection_density[which(walkability_grid$unique_id == x)] = length(which(street_intersections_gridjoin$unique_id == x))
}

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$street_intersection_density), size= 0.01)+
  scale_fill_viridis_c(option = "D")
summary(walkability_grid$street_intersection_density)


#######################################
#5 Green space
#######################################
#official parks
GreenSpaces = readOGR(dsn=paste(dataFolder, "/Built Environment/Green Spaces", sep = "" ),layer="Green Spaces")
#OSM green spaces
GreenSpaces = readOGR(dsn=paste(dataFolder, "/Built Environment/Green Spaces", sep = "" ),layer="Green_Spaces_OSM_Amsterdam_dissolved")

GreenSpaces = spTransform(GreenSpaces, CRSobj = crs)
GreenSpaces = aggregate(GreenSpaces, dissolve = T)
GreenSpaces_sfc = st_as_sfc(GreenSpaces)
plot(GreenSpaces,col="green", add = T)

green_space_raster = coverage_fraction(walkability_grid_raster, GreenSpaces_sfc)[[1]]
plot(green_space_raster)
green_space_raster= rasterToPolygons(green_space_raster)
walkability_grid@data$green_coverage_fraction = green_space_raster@data$layer
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$green_coverage_fraction), size= 0.01)+
  scale_fill_viridis_c(option = "D")
summary(walkability_grid$green_coverage_fraction)




#######################################
#7 Public Transport Density
#######################################
PT_stations = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/public transport", sep = "" ),layer="Tram_n_Metrostations")
PT_stations = spTransform(PT_stations, CRSobj = crs)
PT_stations_gridjoin = point.in.poly(PT_stations, walkability_grid)

walkability_grid$public_trans_density =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$public_trans_density[which(walkability_grid$unique_id == x)] = length(which(PT_stations_gridjoin$unique_id == x))
}

plot(walkability_grid, col= walkability_grid$public_trans_density)
summary(walkability_grid$public_trans_density)




#######################################
#8 Traffic Accidents
#######################################
setwd(paste(dataFolder, "/Built Environment/Traffic Accidents", sep = ""))
Traffic_Accidents = readOGR(dsn=paste(dataFolder, "/Built Environment/Traffic Accidents", sep = ""),layer="TrafficAccidentsAmsterdamRegion")
Traffic_Accidents = spTransform(Traffic_Accidents, CRSobj = crs)
Traffic_Accidents = crop(Traffic_Accidents, extent(extent))
writeOGR(Traffic_Accidents, dsn=getwd() ,layer="TrafficAccidentsAmsterdam",driver="ESRI Shapefile")

Traffic_Accidents_gridjoin = point.in.poly(Traffic_Accidents, walkability_grid)

unique(Traffic_Accidents@data$AOL_ID)

walkability_grid$Nr_Traffic_accidents =0
walkability_grid$Nr_Traffic_Pedestrian_Accidents =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$Nr_Traffic_accidents[which(walkability_grid$unique_id == x)] = length(which(Traffic_Accidents_gridjoin$unique_id == x))
  walkability_grid$Nr_Traffic_Pedestrian_Accidents[which(walkability_grid$unique_id == x)] = length(which(Traffic_Accidents_gridjoin$unique_id == x & Traffic_Accidents_gridjoin$AOL_ID == "Voetganger"))
}

plot(walkability_grid, col= walkability_grid$Nr_Traffic_accidents)
summary(walkability_grid$Nr_Traffic_accidents)

plot(walkability_grid, col= walkability_grid$Nr_Traffic_Pedestrian_Accidents)
summary(walkability_grid$Nr_Traffic_Pedestrian_Accidents)


#######################################
#9 Trees
#######################################
setwd(paste(dataFolder, "/Built Environment/Trees", sep = ""))
Trees = readOGR(dsn=paste(dataFolder, "/Built Environment/Trees", sep = "" ),layer="Amsterdam Trees")
Trees = spTransform(Trees, CRSobj = crs)
Trees_gridjoin = point.in.poly(Trees, walkability_grid)

walkability_grid$Nr_Trees =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$Nr_Trees[which(walkability_grid$unique_id == x)] = length(which(Trees_gridjoin$unique_id == x))
}

plot(walkability_grid, col= walkability_grid$Nr_Trees)
summary(walkability_grid$Nr_Trees)


#######################################
#10 Traffic Volume
#######################################
CarStreets = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/cars", sep = ""),layer="Car Traffic_RDNew")
CarStreets = spTransform(CarStreets, CRSobj = crs)
plot(CarStreets)
CarStreet_gridjoin = intersect(CarStreets, walkability_grid)


walkability_grid$averageTrafficVolume =0
walkability_grid$sumTrafficVolume =0
for(x in walkability_grid@data$unique_id){
  walkability_ids = which(CarStreet_gridjoin$unique_id == x)
  if(length(walkability_ids)>0){
    walkability_grid$averageTrafficVolume[which(walkability_grid$unique_id == x)] = mean(as.integer(CarStreet_gridjoin$etmaal[walkability_ids]))
    walkability_grid$sumTrafficVolume[which(walkability_grid$unique_id == x)] = sum(as.integer(CarStreet_gridjoin$etmaal[walkability_ids]))
  }
}

plot(walkability_grid, col= walkability_grid$averageTrafficVolume)
summary(walkability_grid$averageTrafficVolume)

plot(walkability_grid, col= walkability_grid$sumTrafficVolume)
summary(walkability_grid$sumTrafficVolume)

#######################################
#11 Urban Highway Length
#######################################
CarStreets = CarStreets[which(CarStreets$etmaal != 0),]
CarStreets_sf= st_as_sf(CarStreets)
walkability_grid_sf = st_as_sf(walkability_grid)
intersection <- st_intersection(walkability_grid_sf, CarStreets_sf) %>%
  mutate(lenght = st_length(.)) %>%
  st_drop_geometry() # complicates things in joins later on


walkability_grid$MetersMajorStreets =0
for(x in walkability_grid$unique_id){
  walkability_grid$MetersMajorStreets[which(walkability_grid$unique_id == x)] = sum(intersection$lenght[which(intersection$unique_id == x)])
}

plot(walkability_grid, col= walkability_grid$MetersMajorStreets)
summary(walkability_grid$MetersMajorStreets)


#######################################
#12 Pedestrian Pathway Width
#######################################
setwd(paste(dataFolder, "/Built Environment/Transport Infrastructure/pedestrian", sep = ""))
Pedestrian_streets = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/pedestrian", sep = "" ),layer="Pedestrian Network_Amsterdam")
Pedestrian_streets = spTransform(Pedestrian_streets, CRSobj = crs)

Pedestrian_streets_sf= st_as_sf(Pedestrian_streets)
inter <- st_intersects(Pedestrian_streets_sf,Pedestrian_streets_sf, sparse = TRUE)
Pedestrian_streets_sf <- Pedestrian_streets_sf[lengths(inter)>2,] #select lines intersecting with more than themselves
Pedestrian_streets_new = as_Spatial(Pedestrian_streets_sf)
writeOGR(Pedestrian_streets_new, dsn=getwd() ,layer="Pedestrian_Network_Amsterdam_clean",driver="ESRI Shapefile")

walkability_grid_sf = st_as_sf(walkability_grid)
Pedestrian_streets_gridjoin = st_intersection(Pedestrian_streets_sf, walkability_grid_sf)

walkability_grid$Pedestrian_Street_Width =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$Pedestrian_Street_Width[which(walkability_grid$unique_id == x)] = mean(na.omit(Pedestrian_streets_gridjoin$gewogen_ge[which(Pedestrian_streets_gridjoin$unique_id == x)]))
  }


plot(walkability_grid, col= walkability_grid$Pedestrian_Street_Width)
summary(walkability_grid$Pedestrian_Street_Width)


#######################################
#13 Pedestrian Pathway length
#######################################

intersection <- st_intersection(walkability_grid_sf, Pedestrian_streets_sf) %>%
  mutate(lenght = st_length(.)) %>%
  st_drop_geometry() # complicates things in joins later on

walkability_grid$Pedestrian_Streets_Length = 0
for(x in walkability_grid$unique_id){
  walkability_grid$Pedestrian_Streets_Length[which(walkability_grid$unique_id == x)] = sum(intersection$lenght[which(intersection$unique_id == x)])
}

plot(walkability_grid, col= walkability_grid$Pedestrian_Streets_Length)
summary(walkability_grid$Pedestrian_Streets_Length)

#######################################
#14 Bikelane Length
#######################################
setwd(paste(dataFolder, "/Built Environment/Transport Infrastructure/bike", sep = ""))
Bike_lanes = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/bike", sep = "" ),layer="Fietsknooppuntnetwerken_Amsterdam")
Bike_lanes = spTransform(Bike_lanes, CRSobj = crs)

Bike_lanes_sf= st_as_sf(Bike_lanes)
walkability_grid_sf = st_as_sf(walkability_grid)

intersection <- st_intersection(walkability_grid_sf, Bike_lanes_sf) %>%
  mutate(lenght = st_length(.)) %>%
  st_drop_geometry() # complicates things in joins later on

walkability_grid$len_intersec_bikeroute = 0
for(x in walkability_grid@data$unique_id){
  walkability_grid$len_intersec_bikeroute[which(walkability_grid$unique_id == x)] = sum(intersection$lenght[which(intersection$unique_id == x)])
}

plot(walkability_grid, col= walkability_grid$len_intersec_bikeroute)
summary(walkability_grid$len_intersec_bikeroute)


#######################################
#15 Distance to CBD
#######################################

walkability_grid_centroids = gCentroid(walkability_grid, byid= T, id= walkability_grid$unique_id)
walkability_grid_centroids = spTransform(walkability_grid_centroids, CRSobj = CRS("+proj=longlat +datum=WGS84") )
CBD = walkability_grid_centroids[which(walkability_grid$retail_density == max(walkability_grid$retail_density))]

plot(walkability_grid)
plot(CBD, add= T)

walkability_grid$dist_CBD = 0
x = as.data.frame(spDists(walkability_grid_centroids, CBD, longlat = F))
walkability_grid$dist_CBD = x$V1
?spDists

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$dist_CBD), size= 0.01)+
  scale_fill_viridis_c(option = "D")
summary(walkability_grid$dist_CBD)


#######################################
#16 Car Speed Limits
#######################################
StreetLimits = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/cars", sep = ""),layer="Speedlimit_Amsterdam_RDnew")
StreetLimits = spTransform(StreetLimits, CRSobj = crs)

StreetLimits_sf= st_as_sf(StreetLimits)
walkability_grid_sf = st_as_sf(walkability_grid)
StreetLimits_gridjoin = st_intersection(StreetLimits_sf, walkability_grid_sf)
StreetLimits_gridjoin$wettelijke = as.numeric(StreetLimits_gridjoin$wettelijke)

walkability_grid$MaxSpeedLimit =0
walkability_grid$MinSpeedLimit =0
walkability_grid$MeanSpeedLimit =0
for(x in walkability_grid@data$unique_id){
  if(length(which(StreetLimits_gridjoin$unique_id == x))>0){
    walkability_grid$MaxSpeedLimit[which(walkability_grid$unique_id == x)] = max(na.omit(StreetLimits_gridjoin$wettelijke[which(StreetLimits_gridjoin$unique_id == x)]))
    walkability_grid$MeanSpeedLimit[which(walkability_grid$unique_id == x)] = mean(na.omit(StreetLimits_gridjoin$wettelijke[which(StreetLimits_gridjoin$unique_id == x)]))
    walkability_grid$MinSpeedLimit[which(walkability_grid$unique_id == x)] = min(na.omit(StreetLimits_gridjoin$wettelijke[which(StreetLimits_gridjoin$unique_id == x)]))
  }
}

summary(walkability_grid$MaxSpeedLimit)
summary(walkability_grid$MinSpeedLimit)
summary(walkability_grid$MeanSpeedLimit)

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$MaxSpeedLimit), size= 0.01)+
  scale_fill_viridis_c(option = "D")

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$MinSpeedLimit), size= 0.01)+
  scale_fill_viridis_c(option = "D")

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$MeanSpeedLimit), size= 0.01)+
  scale_fill_viridis_c(option = "D")


##########################################
# Street Lights
##########################################

StreetLights = readOGR(dsn=paste(dataFolder, "/Built Environment/Street Lighting", sep = "" ),layer="StreetLights_RDnew")
StreetLights = spTransform(StreetLights, CRSobj = crs)
StreetLights_gridjoin = point.in.poly(StreetLights, walkability_grid)

walkability_grid$Nr_StreetLights = 0
for(x in walkability_grid@data$unique_id){
  walkability_grid$Nr_StreetLights[which(walkability_grid$unique_id == x)] = length(which(StreetLights_gridjoin$unique_id == x))
}

summary(walkability_grid$Nr_StreetLights)
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$Nr_StreetLights), size= 0.01)+
  scale_fill_viridis_c(option = "D")


##########################################
# Crime
##########################################
crime = readOGR(dsn=paste(dataFolder, "/crime", sep = ""),layer="crime2021Amsterdam_neighborhoods")
crime = spTransform(crime, CRSobj = crs)
plot(crime)
walkability_grid_centroids = gCentroid(walkability_grid, byid= T, id= walkability_grid$unique_id)

?point.in.poly

crime_gridjoin = point.in.poly(walkability_grid_centroids, crime)
crime_gridjoin$crime_inci[which(is.na(crime_gridjoin$crime_inci))] = 0
walkability_grid$crime_incidence = as.numeric(crime_gridjoin@data$crime_inci)

summary(walkability_grid$crime_incidence)
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$crime_incidence), size= 0.01)+
  scale_fill_viridis_c(option = "D")

ggplot(data = st_as_sf(crime)) +
  geom_sf(aes(fill = as.numeric(crime$crime_inci)), size= 0.01)+
  scale_fill_viridis_c(option = "D")

##########################################
# Noise
##########################################
noise_day = readOGR(dsn=paste(dataFolder, "/Environmental Stressors/Noise", sep = ""),layer="PDOK_NoiseMap2016_Lden_RDNew")
noise_night = readOGR(dsn=paste(dataFolder, "/Environmental Stressors/Noise", sep = ""),layer="PDOK_NoiseMap2016_Lnight_RDNew")
noise_day = spTransform(noise_day, CRSobj = crs)
noise_night = spTransform(noise_night, CRSobj = crs)
walkability_grid_centroids = gCentroid(walkability_grid, byid= T, id= walkability_grid$unique_id)

noise_dayjoin = point.in.poly(walkability_grid_centroids, noise_day)
noise_night_join = point.in.poly(walkability_grid_centroids, noise_night)
noise_dayjoin$bovengrens[which(is.na(noise_dayjoin$bovengrens))] = 0
noise_night_join$bovengrens[which(is.na(noise_night_join$bovengrens))] = 0

walkability_grid$max_noise_day = as.numeric(noise_dayjoin@data$bovengrens)
walkability_grid$max_noise_night = as.numeric(noise_night_join@data$bovengrens)


summary(walkability_grid$max_noise_day)
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$max_noise_day), size= 0.01)+
  scale_fill_viridis_c(option = "D")

summary(walkability_grid$max_noise_night)
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$max_noise_night), size= 0.01)+
  scale_fill_viridis_c(option = "D")

##########################################
# Land Use/ Open Space
##########################################

LandUse = readOGR(dsn=paste(dataFolder, "/Land Use/dataamsterdam", sep = ""),layer="GRONDGEBRUIK_2017")
LandUse = spTransform(LandUse, CRSobj = crs)
OpenSpace = LandUse[which(!(LandUse@data$CBScode1_O %in% c("Bebouwd terrein", "Verkeersterrein"))), ]

LandUse = readOGR(dsn=paste(dataFolder, "/Land Use/CBS_Publicatiebestand_BBG2017_v1_SHP", sep = ""),layer="CBS_Publicatiebestand_BBG2017_v1")
LandUse = spTransform(LandUse, CRSobj = crs)
LandUse@data$CBScode1_O = substr(LandUse@data$BG2017, 1, 1)
OpenSpace = LandUse[which(!(LandUse@data$CBScode1_O %in% c("1", "2"))), ]
plot(OpenSpace)
writeOGR(OpenSpace, dsn=paste(dataFolder, "/Land Use", sep = "") ,layer= paste("OpenSpace", sep=""),driver="ESRI Shapefile")
OpenSpace = readOGR(dsn=paste(dataFolder, "/Land Use", sep = ""),layer="OpenSpace_Amsterdam")

OpenSpace = aggregate(OpenSpace, dissolve = T)
OpenSpace_sfc = st_as_sfc(OpenSpace)
plot(OpenSpace,col="yellow", add = T)

open_space_raster = coverage_fraction(walkability_grid_raster, OpenSpace_sfc)[[1]]
plot(open_space_raster)
open_space_raster= rasterToPolygons(open_space_raster)
walkability_grid@data$openspace_fraction = open_space_raster@data$layer
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$openspace_fraction), size= 0.01)+
  scale_fill_viridis_c(option = "D")
summary(walkability_grid$openspace_fraction)


##########################################
# Neighborhood Socioeconomic Status
##########################################
Neigh_stats = readOGR(dsn=paste(dataFolder, "/Administrative Units", sep = ""),layer="CBS_pc4_2019_v2")
Neigh_stats = spTransform(Neigh_stats, CRSobj = crs)
walkability_grid_centroids = gCentroid(walkability_grid, byid= T, id= walkability_grid$unique_id)
neighstat_join = point.in.poly(walkability_grid_centroids, Neigh_stats)

## percentage non-western background
neighstat_join$P_NW_MIG_A[which(neighstat_join$P_NW_MIG_A == "-99997")] = 0
neighstat_join$P_NW_MIG_A[which(is.na(neighstat_join$P_NW_MIG_A))] = 0

walkability_grid$perc_non_western = as.numeric(neighstat_join@data$P_NW_MIG_A)
summary(walkability_grid$perc_non_western)
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$perc_non_western), size= 0.01)+
  scale_fill_viridis_c(option = "D")

# percentage of people dependent on social welfare support
neighstat_join$UITKMINAOW[which(neighstat_join$UITKMINAOW == "-99997")] = 0
neighstat_join$perc_welfare_dependent = as.numeric(neighstat_join$UITKMINAOW)/as.numeric(neighstat_join$INWONER)
neighstat_join$perc_welfare_dependent[which(is.na(neighstat_join$perc_welfare_dependent))] = 0
walkability_grid$perc_welfare_dependent = as.numeric(neighstat_join@data$perc_welfare_dependent)

summary(walkability_grid$perc_welfare_dependent)
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$perc_welfare_dependent), size= 0.01)+
  scale_fill_viridis_c(option = "D")

##########################################
# Parking Availability
##########################################
ParkingSpaces = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/parking", sep = ""),layer="all_parkingspaces_amsterdam")
ParkingSpaces = spTransform(ParkingSpaces, CRSobj = crs)
ParkingSpaces_centroids = gCentroid(ParkingSpaces, byid= T, id= ParkingSpaces$id)

ParkingSpaces_centroids_gridjoin = point.in.poly(ParkingSpaces_centroids, walkability_grid)

walkability_grid$Nr_ParkingSpaces =0
for(x in walkability_grid@data$unique_id){
  walkability_grid$Nr_ParkingSpaces[which(walkability_grid$unique_id == x)] = length(which(ParkingSpaces_centroids_gridjoin$unique_id == x))
}

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$Nr_ParkingSpaces), size= 0.01)+
  scale_fill_viridis_c(option = "D")

summary(walkability_grid$Nr_ParkingSpaces)

##########################################
# Parking Prices
##########################################
ParkingPrices = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/parking", sep = ""),layer="parkingprices_prepostintervention")
ParkingPrices = spTransform(ParkingPrices, CRSobj = crs)

walkability_grid_centroids = gCentroid(walkability_grid, byid= T, id= walkability_grid$unique_id)
ParkingPrices_join = point.in.poly(walkability_grid_centroids, ParkingPrices)
ParkingPrices_join = ParkingPrices_join[!is.na(ParkingPrices_join$pt.ids),]

walkability_grid$ParkingPrices_pre =0
walkability_grid$ParkingPrices_post =0
for(x in 1:length(walkability_grid@data$unique_id)){
  walkability_grid$ParkingPrices_pre[x] = max(ParkingPrices_join$h_cost_pre[(ParkingPrices_join$pt.ids == x)])
  walkability_grid$ParkingPrices_post[x] = max(ParkingPrices_join$h_costpost[(ParkingPrices_join$pt.ids == x)])
}

summary(walkability_grid$ParkingPrices_pre)
summary(walkability_grid$ParkingPrices_post)

ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$ParkingPrices_pre), size= 0.01)+
  scale_fill_viridis_c(option = "D")
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$ParkingPrices_post), size= 0.01)+
  scale_fill_viridis_c(option = "D")

#####################################
# Sidewalk Coverage
#####################################

Sidewalks = readOGR(dsn=paste(dataFolder, "/Built Environment/Transport Infrastructure/pedestrian", sep = ""),layer="BGT_pedway_polygons_geomattr2")
Sidewalks = spTransform(Sidewalks, CRSobj = crs)
Sidewalks = aggregate(Sidewalks, dissolve = T)
Sidewalks = st_as_sfc(Sidewalks)
plot(Sidewalks,col="yellow", add = T)

Sidewalks_raster = coverage_fraction(walkability_grid_raster, Sidewalks)[[1]]
Sidewalks_raster= rasterToPolygons(Sidewalks_raster)
walkability_grid@data$sidewalk_coverage = Sidewalks_raster@data$layer
ggplot(data = st_as_sf(walkability_grid)) +
  geom_sf(aes(fill = walkability_grid$sidewalk_coverage), size= 0.01)+
  scale_fill_viridis_c(option = "D")
summary(walkability_grid$sidewalk_coverage)


#####################################
# Saving the grid data
#####################################

setwd(paste(dataFolder, "/Built Environment/Transport Infrastructure", sep = ""))

colnames(walkability_grid@data)
c("layer", "unique_id",  "population_density", "retail_density" , "green_coverage_fraction", "public_trans_density"  , "street_intersection_density", "int_id", "Nr_Traffic_accidents", "Nr_Traffic_Pedestrian_Accidents", "Nr_Trees", "averageTrafficVolume", "sumTrafficVolume", "MetersMajorStreets", "Pedestrian_Street_Width",  "Pedestrian_Streets_Length", "len_intersec_bikeroute", "dist_CBD")
colnames(walkability_grid@data) =c("layer", "unqId",  "popDns", "retaiDns" , "greenCovr", "pubTraDns"  , "RdIntrsDns", "Intid", "TrafAccid", "AccidPedes", "NrTrees", "MeanTraffV", "SumTraffVo", "HighwLen", "PedStrWidt", "PedStrLen", "LenBikRout", "DistCBD")

walkability_grid@data = walkability_grid@data[c("unqId", "Intid", "popDns", "retaiDns" , "greenCovr", "pubTraDns"  , "RdIntrsDns", "TrafAccid", "AccidPedes", "NrTrees", "MeanTraffV", "SumTraffVo", "HighwLen", "PedStrWidt", "PedStrLen", "LenBikRout", "DistCBD")]

writeOGR(walkability_grid, dsn=getwd() ,layer= paste("walkability_grid_",raster_size, sep=""),driver="ESRI Shapefile")
writeRaster(walkability_grid_raster, "walkability_grid.tif", format = "GTiff", overwrite = T)
walkability_measures = as.data.frame(walkability_grid)
walkability_measures= subset(walkability_measures, select= -c(layer))
write.csv(walkability_measures, "walkability_measures.csv", row.names = F, quote=FALSE)

walkability_grid = readOGR(dsn=getwd(),layer=paste("walkability_grid_",raster_size, sep=""))
colnames(walkability_grid@data) = c("unqId", "Intid", "popDns", "retaiDns" , "greenCovr", "pubTraDns",
                                    "RdIntrsDns", "TrafAccid", "AccidPedes", "NrTrees", "MeanTraffV",
                                    "SumTraffVo", "HighwLen", "PedStrWidt", "PedStrLen", "LenBikRout",
                                    "DistCBD", "retailDiv", "MaxSpeed", "MinSpeed", "MeanSpeed", "NrStrLight",
                                    "CrimeIncid", "MaxNoisDay", "MxNoisNigh", "OpenSpace", "NrParkSpac",
                                    "PNonWester", "PWelfarDep", "PrkPricPre", "PrkPricPos")

colnames(walkability_grid@data) = c("unique_id",  "int_id", "population_density", "retail_density" ,
                                    "green_coverage_fraction", "public_trans_density", "street_intersection_density",
                                    "Nr_Traffic_accidents", "Nr_Traffic_Pedestrian_Accidents", "Nr_Trees",
                                    "averageTrafficVolume", "sumTrafficVolume", "MetersMajorStreets",
                                    "Pedestrian_Street_Width",  "Pedestrian_Streets_Length", "len_intersec_bikeroute",
                                    "dist_CBD", "retail_diversity", "MaxSpeedLimit", "MinSpeedLimit", "MeanSpeedLimit",
                                    "Nr_StreetLights", "crime_incidence", "max_noise_day",  "max_noise_night",
                                    "openspace_fraction", "Nr_ParkingSpaces", "perc_non_western", "perc_welfare_dependent",
                                    "ParkingPrices_pre", "ParkingPrices_post")

writeOGR(walkability_grid, dsn=getwd() ,layer= paste("ModalChoice_determ_",raster_size, sep=""),driver="ESRI Shapefile")
walkability_grid = readOGR(dsn=getwd(),layer=paste("ModalChoice_determ_",raster_size, sep=""))

summary(walkability_grid$popDns)
summary(walkability_grid$greenCovr)
summary(walkability_grid$retaiDns)
summary(walkability_grid$pubTraDns)
summary(walkability_grid$RdIntrsDns)


#####################################
#### Joining to PC6 spatial layer
######################################
PC6_centroids = readOGR(dsn=paste(dataFolder, "/Administrative Units", sep = ""),layer="CBS_PC6_2019_Amsterdam_centroids")
PC6_centroids_gridjoin = point.in.poly(PC6_centroids, walkability_grid)

PC6_env_behav_determ = as.data.frame(PC6_centroids_gridjoin)
write.csv(PC6_env_behav_determ, "PC6_env_behav_determ.csv", row.names = F)

PC6_polygon = readOGR(dsn=paste(dataFolder, "/Administrative Units", sep = ""),layer="CBS_PC6_2019_Amsterdam")
PC6_polygon = PC6_polygon[,c("PC6")]
PC6_polygon = merge(PC6_polygon, PC6_env_behav_determ, by = "PC6")
writeOGR(PC6_polygon, dsn=getwd() ,layer= "PC6_polygon_behav_determ",driver="ESRI Shapefile")


