#########################
### Load packages #######
#########################
pkgs = c("GA","bnlearn", "stats", "rgdal","sp", "sf", "rgeos" , "ggplot2",  "raster",  "dplyr")
sapply(pkgs[which(sapply(pkgs, require, character.only = T) == FALSE)], install.packages, character.only = T)
sapply(pkgs, require, character.only = T) #load
rm(pkgs)

#########################
### read data ###########
#########################
## ODIN
setwd("C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/ODIN/2018")
ODIN = read.csv("ODIN2018_Studyarea_PC6_sample.csv")
colnames(ODIN)

dutchnames = c("VerplID", "VertPC", "AankPC", "Hvm", "OPID", "Geslacht", "Leeftijd",
               "Herkomst", "HHAuto", "HHGestInkG",
               "BetWerk", "Opleiding", "HHPers",
               "HHLft1", "HHLft2", "HHLft3", "MotiefV")

englishnames = c("TripID", "orig_postcode", "dest_postcode", "modal_choice", "Person_ID", "sex", "age",
                 "migration_background", "Nr_cars_hh",
                 "income",  "employment_status",
                 "education_level", "HH_size",  "nr_children_yonger6",
                 "nr_child_6_11", "nr_child_12_17", "trip_purpose")

ODIN = ODIN[, c("orig_PC6","dest_PC6",englishnames)]


mode_names = c("Personenauto",	"Trein",	"Bus",	"Tram",	"Metro",	"Speedpedelec",
"Elektrische fiets",	"Niet-elektrische fiets", "Te voet","Touringcar",
"Bestelauto", "Vrachtwagen","Camper,Taxi/Taxibusje","Landbouwvoertuig",
"Motor","Bromfiets","Snorfiets","Gehandicaptenvervoermiddel met motor",
"Gehandicaptenvervoermiddel zonder motor","Skates/skeelers/step",
"Boot","Anders met motor","Anders zonder motor")

ODIN$mode_name = ODIN$modal_choice
for(i in 1:length(mode_names)){
  ODIN$mode_name[ODIN$mode_name == i] = mode_names[i]
}

table(ODIN$mode_name)

ODIN$modes_simple = ""
ODIN$modes_simple[ODIN$mode_name %in% c("Personenauto", "Bestelauto", "Vrachtwagen", "Landbouwvoertuig", "Gehandicaptenvervoermiddel met motor")] = "car"
ODIN$modes_simple[ODIN$mode_name %in% c("Trein", "Bus", "Tram", "Metro")] = "publicTransport"
ODIN$modes_simple[ODIN$mode_name %in% c("Elektrische fiets","Niet-elektrische fiets")] = "bike"
ODIN$modes_simple[ODIN$mode_name %in% c("Te voet", "Gehandicaptenvervoermiddel zonder motor", "Anders zonder motor")] = "walk"
ODIN$modes_simple[ODIN$mode_name %in% c("Snorfiets", "Bromfiets")] = "scooter"

simple_mode_names = unique(ODIN$modes_simple)
ODIN$WalkTrip = 0
ODIN$WalkTrip[ODIN$modes_simple == "walk"] = 1
ODIN$BikeTrip = 0
ODIN$BikeTrip[ODIN$modes_simple == "bike"] = 1
ODIN$CarTrip = 0
ODIN$CarTrip[ODIN$modes_simple == "car"] = 1
ODIN$PubTransTrip = 0
ODIN$PubTransTrip[ODIN$modes_simple == "publicTransport"] = 1


## Environmental Behavior Determinants
setwd("C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/Built Environment/Transport Infrastructure")
PC6_env_behav_determ = read.csv("PC6_env_behav_determ.csv")
colnames(PC6_env_behav_determ)
env_behav_colnames = c("PC6", "area", "perimeter", "unqId", "Intid", "popDns", "retaiDns" , "greenCovr", "pubTraDns",
             "RdIntrsDns", "TrafAccid", "AccidPedes", "NrTrees", "MeanTraffV",
             "SumTraffVo", "HighwLen", "PedStrWidt", "PedStrLen", "LenBikRout",
             "DistCBD", "retailDiv", "MaxSpeed", "MinSpeed", "MeanSpeed", "NrStrLight",
             "CrimeIncid", "MaxNoisDay", "MxNoisNigh", "OpenSpace", "NrParkSpac",
             "PNonWester", "PWelfarDep", "coords.x1","coords.x2")

PC6_polygon = readOGR(dsn=getwd(),layer="PC6_polygon_behav_determ")
PC6_polygonsf = st_as_sf(PC6_polygon)

crs = "+init=EPSG:28992" #Amersfoort / RD New
crs_name = "RDNew"
CRS_defin = "+towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725"


##################################################
### joining ODIN data with environmental data ####
##################################################
#variables that have to be joined with the trip origin PC6
orig_variables = c("DistCBD", "pubTraDns", "coords.x1","coords.x2")
orig_vardata = PC6_env_behav_determ[,c("PC6", orig_variables)]
colnames(orig_vardata)[2:ncol(orig_vardata)] = paste0(orig_variables,".orig")

#variables that have to be averaged over the trip route
route_variables = c("popDns", "retaiDns" , "greenCovr", "pubTraDns", "RdIntrsDns", "TrafAccid", "AccidPedes", "NrTrees", "MeanTraffV",
                    "SumTraffVo", "HighwLen", "PedStrWidt", "PedStrLen", "LenBikRout","retailDiv", "MaxSpeed", "MinSpeed", "MeanSpeed", "NrStrLight",
                    "CrimeIncid", "MaxNoisDay", "MxNoisNigh", "OpenSpace", "PNonWester", "PWelfarDep")

route_variables_later = paste0(route_variables, ".route")

#variables that have to be joined with the trip destination PC6
dest_variables = c("NrParkSpac","coords.x1","coords.x2")
dest_vardata = PC6_env_behav_determ[,c("PC6", dest_variables)]
colnames(dest_vardata)[2:ncol(dest_vardata)] = paste0(dest_variables,".dest")

ODIN_orig = base::merge(ODIN, orig_vardata, by.x = "orig_PC6", by.y = "PC6")
ODIN_orig_dest = base::merge(ODIN_orig, dest_vardata, by.x = "dest_PC6", by.y = "PC6")

ODIN_orig_dest[,route_variables_later] = NA
ODIN_orig_dest$trip_distance = NA
for(x in 1:nrow(ODIN)){
  orig_dest_line = st_as_sf(SpatialLines(list(Lines(Line(list(as.numeric(ODIN_orig_dest[x,c("coords.x1.orig","coords.x1.dest")]), as.numeric(ODIN_orig_dest[x,c("coords.x2.orig","coords.x2.dest")]))), ID = ODIN_orig_dest$TripID[x])), proj4string = CRS(crs)))
  ODIN_orig_dest$trip_distance[x] = st_length(orig_dest_line)
  route_stats = st_intersection(orig_dest_line, PC6_polygonsf) %>%
    st_drop_geometry() %>%
    select(route_variables)
  route_stats <- sapply(route_stats, as.numeric )
  ODIN_orig_dest[x,route_variables_later] = colMeans(route_stats, na.rm = T)
}

plot(PC6_polygon[which(PC6_polygon$PC6 %in% route_stats$PC6),])
plot(orig_dest_line, add = T, col = "red")
st_length(orig_dest_line)

#####################################################
### Initial Regression Exploration ##################
#####################################################

walk_lm <- lm(WalkTrip ~ sex + age +  migration_background +
                Nr_cars_hh +
                income +
                employment_status +
                education_level +
                HH_size +
                nr_children_yonger6 +
                DistCBD.orig +
                pubTraDns.orig +
                NrParkSpac.dest +
                popDns.route +
                retaiDns.route +
                greenCovr.route +
                pubTraDns.route +
                RdIntrsDns.route +
                TrafAccid.route +
                AccidPedes.route +
                NrTrees.route +
                MeanTraffV.route +
                HighwLen.route +
                PedStrWidt.route +
                PedStrLen.route +
                LenBikRout.route +
                retailDiv.route +
                MaxSpeed.route +
                NrStrLight.route +
                CrimeIncid.route +
                MaxNoisDay.route +
                OpenSpace.route +
                PNonWester.route +
                PWelfarDep.route , data = ODIN_orig_dest)

summary(walk_lm)


bike_lm <- lm(BikeTrip ~ sex + age + migration_background +
                Nr_cars_hh +
                income +
                employment_status +
                education_level +
                HH_size +
                nr_children_yonger6 +
                DistCBD.orig +
                pubTraDns.orig +
                NrParkSpac.dest +
                popDns.route +
                retaiDns.route +
                greenCovr.route +
                pubTraDns.route +
                RdIntrsDns.route +
                TrafAccid.route +
                AccidPedes.route +
                NrTrees.route +
                MeanTraffV.route +
                HighwLen.route +
                PedStrWidt.route +
                PedStrLen.route +
                LenBikRout.route +
                retailDiv.route +
                MaxSpeed.route +
                NrStrLight.route +
                CrimeIncid.route +
                MaxNoisDay.route +
                OpenSpace.route +
                PNonWester.route +
                PWelfarDep.route , data = ODIN_orig_dest)

summary(bike_lm)


car_lm <- lm(CarTrip ~ sex + age + migration_background +
               Nr_cars_hh +
               income +
               employment_status +
               education_level +
               HH_size +
               nr_children_yonger6 +
               DistCBD.orig +
                pubTraDns.orig +
                NrParkSpac.dest +
                popDns.route +
                retaiDns.route +
                greenCovr.route +
                pubTraDns.route +
                RdIntrsDns.route +
                TrafAccid.route +
                AccidPedes.route +
                NrTrees.route +
                MeanTraffV.route +
                HighwLen.route +
                PedStrWidt.route +
                PedStrLen.route +
                LenBikRout.route +
                retailDiv.route +
                MaxSpeed.route +
                NrStrLight.route +
                CrimeIncid.route +
                MaxNoisDay.route +
                OpenSpace.route +
                PNonWester.route +
                PWelfarDep.route , data = ODIN_orig_dest)

summary(car_lm)


############################################
### Behavioral Model Replica ###############
############################################



###########need to subselect based on pre-intervention date
 date < 2019 #April

# data preparation for heterogenous weight calibration
ODIN_orig_dest$age_group = 0
ODIN_orig_dest$age_group[ODIN_orig_dest$age %in% 0:10] = 0
ODIN_orig_dest$age_group[ODIN_orig_dest$age %in% 10:17] = 1
ODIN_orig_dest$age_group[ODIN_orig_dest$age %in% 18:35] = 2
ODIN_orig_dest$age_group[ODIN_orig_dest$age %in% 35:50] = 3
ODIN_orig_dest$age_group[ODIN_orig_dest$age %in% 50:65] = 4
ODIN_orig_dest$age_group[ODIN_orig_dest$age %in% 65:100] = 5


## parameters for calibration
income_weight_car = 0
trip_dist_weight_car_age = c(0,0,0,0,0)
parking_space_weight_car = 0
car_access_weight = 0


ODIN_orig_dest$driving_utility = (income_weight_car * ODIN_orig_dest$income) +
                                 (trip_dist_weight_car_age[ODIN_orig_dest$age_group] * ODIN_orig_dest$trip_distance) +
                                 (parking_space_weight_car * ODIN_orig_dest$NrParkSpac.dest) +
                                 (parking_price_weight_car * ODIN_orig_dest$ParkingPrice.dest) +
                                  (car_access_weight)

season (weather)

walking_utility <- (affordability_weight * float(affordability["perc_budget_walk"]))
+ (pop_density_weight_walk * float(assumed_quality_infrastructure["pop_density"]))
+ (retail_density_weight_walk * float(assumed_quality_infrastructure["retail_density"]))
+ (greenCoverage_weight_walk * float(assumed_quality_infrastructure["greenCoverage"]))
+ (public_Transport_density_weight_walk * float(assumed_quality_infrastructure["public_Transport_density"]))
+ (road_intersection_density_weight_walk * float(assumed_quality_infrastructure["road_intersection_density"]))
+ (traveltime_weight * float(assumed_traveltime["traveltime_walk"]))
+ (tripdistance_weight_age_walk[agegroup] * tripdistance_weight_BMI_walk[weightgroup] * (trip_distance/1000))

biking_utility <- (affordability_weight * float(affordability["perc_budget_bike"]))
+ (pop_density_weight_bike * float(assumed_quality_infrastructure["pop_density"]))
+ (retail_density_weight_bike * float(assumed_quality_infrastructure["retail_density"]))
+ (greenCoverage_weight_bike * float(assumed_quality_infrastructure["greenCoverage"]))
+ (public_Transport_density_weight_bike * float(assumed_quality_infrastructure["public_Transport_density"]))
+ (road_intersection_density_weight_bike * float(assumed_quality_infrastructure["road_intersection_density"]))
+ (traveltime_weight * float(assumed_traveltime["traveltime_bike"]))
+ (tripdistance_weight_age_bike[agegroup] * tripdistance_weight_BMI_bike[weightgroup] * (trip_distance/1000))






## behavioral model replica

fitness <- function(x)
{
  f <- -f(x)                         # we need to maximise -f(x)
  pen <- sqrt(.Machine$double.xmax)  # penalty term
  penalty1 <- max(c1(x),0)*pen       # penalisation for 1st inequality constraint
  penalty2 <- max(c2(x),0)*pen       # penalisation for 2nd inequality constraint
  f - penalty1 - penalty2            # fitness function value
}

GA <- ga("real-valued", fitness = fitness,
         lower = c(0,0), upper = c(1,13),
         # selection = GA:::gareal_lsSelection_R,
         maxiter = 1000, run = 200, seed = 123)
summary(GA)

hill.climbing.search(attributes, eval.fun)

driving_utility <- (affordability_weight * float(affordability["perc_budget_car"]))
  + (traveltime_weight * float(assumed_traveltime["traveltime_car"]))
  + (tripdistance_weight_age_car[agegroup] * tripdistance_weight_BMI_car[weightgroup] * (trip_distance/1000))


walking_utility <- (affordability_weight * float(affordability["perc_budget_walk"]))
  + (pop_density_weight_walk * float(assumed_quality_infrastructure["pop_density"]))
  + (retail_density_weight_walk * float(assumed_quality_infrastructure["retail_density"]))
  + (greenCoverage_weight_walk * float(assumed_quality_infrastructure["greenCoverage"]))
  + (public_Transport_density_weight_walk * float(assumed_quality_infrastructure["public_Transport_density"]))
  + (road_intersection_density_weight_walk * float(assumed_quality_infrastructure["road_intersection_density"]))
  + (traveltime_weight * float(assumed_traveltime["traveltime_walk"]))
  + (tripdistance_weight_age_walk[agegroup] * tripdistance_weight_BMI_walk[weightgroup] * (trip_distance/1000))

biking_utility <- (affordability_weight * float(affordability["perc_budget_bike"]))
  + (pop_density_weight_bike * float(assumed_quality_infrastructure["pop_density"]))
  + (retail_density_weight_bike * float(assumed_quality_infrastructure["retail_density"]))
  + (greenCoverage_weight_bike * float(assumed_quality_infrastructure["greenCoverage"]))
  + (public_Transport_density_weight_bike * float(assumed_quality_infrastructure["public_Transport_density"]))
  + (road_intersection_density_weight_bike * float(assumed_quality_infrastructure["road_intersection_density"]))
  + (traveltime_weight * float(assumed_traveltime["traveltime_bike"]))
  + (tripdistance_weight_age_bike[agegroup] * tripdistance_weight_BMI_bike[weightgroup] * (trip_distance/1000))

if(trip_distance <= distance_willing_travel["walk"]){
    if(car_owner = 1){
      modalchoice <- ["car", "walk", "bike"] at ([driving_utility, walking_utility, biking_utility] index_of  max([driving_utility, walking_utility, biking_utility]));
    }
    else{
      modalchoice <- ["walk", "bike"] at ([walking_utility, biking_utility] index_of  max([walking_utility, biking_utility]));
    }
  }
  else if(trip_distance <= distance_willing_travel["bike"]){
    if(car_owner = 1){
      modalchoice <- ["car","bike"] at ([driving_utility, biking_utility] index_of  max([driving_utility, biking_utility]));
    }
    else{
      modalchoice <- "bike";
    }
  }
  else{
    if(car_owner = 1){
      modalchoice <- "car";
    }
    else{
      modalchoice <- "bike";
    }
  }
  write string(trip_distance) + " " + modalchoice ;
}
