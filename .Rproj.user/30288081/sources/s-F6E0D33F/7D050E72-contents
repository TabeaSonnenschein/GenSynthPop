pkgs = c("rgdal","sp", "sf",  "dplyr", "bigmemory")
sapply(pkgs[which(sapply(pkgs, require, character.only = T) == FALSE)], install.packages, character.only = T)
sapply(pkgs, require, character.only = T) #load
rm(pkgs)

memory.limit(size=16000)

dataFolder= "C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam"

## reading ODIN data
setwd(paste(dataFolder, "/ODIN/2019", sep = ""))
ODIN2019= read.csv("ODiN2019_Databestand_v2.0.csv", sep = ";")
setwd(paste(dataFolder, "/ODIN/2018", sep = ""))
ODIN2018= read.csv("ODiN2018_Databestand_v2.0.csv", sep = ",")
colname_description = read.csv("ODiN2018_Codeboek_prepared.csv")


## reading postcode data
setwd(paste(dataFolder, "/Administrative Units", sep = ""))
PC6 = read.csv("pc6hnr20190801_gwb.csv")  # has all postcode 6 and housenumbers in the netherlands
write.csv(PC6_small, "PC6_small.csv", row.names = F)
# Gemeente AMsterdam = 363; Diemen = 384; Ouder-Amstel = 437

PC6_StudyArea = PC6[which(PC6$Gemeente2019 %in% c("363", "384", "437")), ]
PC6_StudyArea$PC4 = substr(PC6_StudyArea$PC6, 1, 4)
count_per_PC4 = PC6_StudyArea %>% count(PC4)
colnames(count_per_PC4) = c("PC4", "number_of_housenumbers")
count_per_PC6 = PC6_StudyArea %>% count(PC6)
colnames(count_per_PC6) = c("PC6", "number_of_housenumbers")

#remove housenumber information
PC6_StudyArea = unique(PC6_StudyArea[,c("PC4", "PC6", "Buurt2019", "Wijk2019", "Gemeente2019")])
count_per_PC4$number_of_PC6 = as.data.frame(PC6_StudyArea %>% count(PC4))[2]
count_per_PC4$number_of_PC6 = count_per_PC4$number_of_PC6$n
write.csv(count_per_PC4, "PC4_PC6_address_density_stats.csv", row.names = F)
write.csv(count_per_PC6, "PC6_address_density_stats.csv", row.names = F)


summary(count_per_PC4$number_of_PC6) # Distribution of PC6 across PC4
summary(count_per_PC4$number_of_housenumbers) # Distribution of number of housenumbers per PC4
paste("Number of PC4 in Study Area:", nrow(count_per_PC4))
summary(count_per_PC6$number_of_housenumbers) # Distribution of number of housenumbers per PC6
paste("Number of PC6 in Study Area:", nrow(count_per_PC6))

uniq_PC4 = unique(PC6_StudyArea$PC4)

##########################################################################
## select ODIN trips that start from or end in study area
ODIN2018_Studyarea = ODIN2018[which(ODIN2018$VertPC %in% uniq_PC4 | ODIN2018$AankPC %in% uniq_PC4),]
ODIN2019_Studyarea = ODIN2019[which(ODIN2019$VertPC %in% uniq_PC4 | ODIN2019$AankPC %in% uniq_PC4),]


## summary statistics
paste("Nr of uniq people with trip records in studyarea from 2018:",length(unique(ODIN2018_Studyarea$OPID)))
paste("Nr of uniq people with trip records in studyarea from 2019:",length(unique(ODIN2019_Studyarea$OPID)))

paste("Nr of trip records in studyarea from 2019:",nrow(ODIN2019_Studyarea))
paste("Nr of trip records in studyarea from 2018:",nrow(ODIN2018_Studyarea))

paste("Nr of trips starting outside studyarea in 2018:",nrow(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$VertPC %in% uniq_PC4)),]))
paste("Nr of trips starting outside studyarea in 2019:",nrow(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$VertPC %in% uniq_PC4)),]))

paste("Nr of trips ending outside studyarea in 2018:",nrow(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$AankPC %in% uniq_PC4)),]))
paste("Nr of trips ending outside studyarea in 2019:",nrow(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$AankPC %in% uniq_PC4)),]))

paste("Nr of people living outside studyarea in 2018:",length(unique(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$WoPC %in% uniq_PC4)),c("OPID")])))
paste("Nr of people living outside studyarea in 2019:",length(unique(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$WoPC %in% uniq_PC4)),c("OPID")])))

paste("Nr of unique people traveling outside studyarea in 2018:",length(unique(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$AankPC %in% uniq_PC4 & ODIN2018_Studyarea$VertPC %in% uniq_PC4)),c("OPID")])))
paste("Nr of unique people traveling outside studyarea in 2019:",length(unique(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$AankPC %in% uniq_PC4 & ODIN2019_Studyarea$VertPC %in% uniq_PC4)),c("OPID")])))

paste("Percentage of people that travel outside that live outside 2018:",(length(unique(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$WoPC %in% uniq_PC4)),c("OPID")]))/length(unique(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$AankPC %in% uniq_PC4 & ODIN2018_Studyarea$VertPC %in% uniq_PC4)),c("OPID")]))))
paste("Percentage of people that travel outside that live outside 2019:",(length(unique(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$WoPC %in% uniq_PC4)),c("OPID")]))/length(unique(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$AankPC %in% uniq_PC4 & ODIN2019_Studyarea$VertPC %in% uniq_PC4)),c("OPID")]))))

# Distribution of number of subsections along trips in 2018
summary((ODIN2018_Studyarea %>% count(VerplID))[[2]])
trips = ODIN2018_Studyarea %>% count(VerplID)
paste("Percentage of trips that have more than one subsection in 2018:", nrow(trips[trips$n > 1,])/nrow(trips))

summary((ODIN2019_Studyarea %>% count(VerplID))[[2]])
trips = ODIN2019_Studyarea %>% count(VerplID)
paste("Percentage of trips that have more than one subsection in 2019:", nrow(trips[trips$n > 1,])/nrow(trips))


setwd("C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/ODIN/2019")
write.csv(ODIN2019_Studyarea, "ODIN2019_Studyarea_with_outsidetrips.csv", row.names = F)

setwd("C:/Users/Tabea/Documents/PhD EXPANSE/Data/Amsterdam/ODIN/2018")
write.csv(ODIN2018_Studyarea, "ODIN2018_Studyarea_with_outsidetrips.csv", row.names = F)

#############################################################################
## select ODIN trips that start from AND end in study area
ODIN2018_Studyarea = ODIN2018[which((ODIN2018$VertPC %in% uniq_PC4) & (ODIN2018$AankPC %in% uniq_PC4)),]
ODIN2019_Studyarea = ODIN2019[which((ODIN2019$VertPC %in% uniq_PC4) & (ODIN2019$AankPC %in% uniq_PC4)),]
colnames(ODIN2018_Studyarea)[1] = "OP"

## summary statistics
paste("Nr of uniq people with trip records in studyarea from 2018:",length(unique(ODIN2018_Studyarea$OPID)))
paste("Nr of uniq people with trip records in studyarea from 2019:",length(unique(ODIN2019_Studyarea$OPID)))

paste("Nr of trip records in studyarea from 2019:",nrow(ODIN2019_Studyarea))
paste("Nr of trip records in studyarea from 2018:",nrow(ODIN2018_Studyarea))

paste("Nr of people living outside studyarea in 2018:",length(unique(ODIN2018_Studyarea[which(!(ODIN2018_Studyarea$WoPC %in% uniq_PC4)),c("OPID")])))
paste("Nr of people living outside studyarea in 2019:",length(unique(ODIN2019_Studyarea[which(!(ODIN2019_Studyarea$WoPC %in% uniq_PC4)),c("OPID")])))

# Distribution of number of subsections along trips
summary((ODIN2018_Studyarea %>% count(VerplID))[[2]])
trips = ODIN2018_Studyarea %>% count(VerplID)
paste("Percentage of trips that have more than one subsection in 2018:", nrow(trips[trips$n > 1,])/nrow(trips))

summary((ODIN2019_Studyarea %>% count(VerplID))[[2]])
trips = ODIN2019_Studyarea %>% count(VerplID)
paste("Percentage of trips that have more than one subsection in 2019:", nrow(trips[trips$n > 1,])/nrow(trips))


# within ODIN behavior determinants
c("Geslacht", "Leeftijd", "Herkomst", "HHAuto", "HHGestInkG",
  "BetWerk", "Opleiding", "HHPers", "HHLft1", "HHLft2", "HHLft3", "MotiefV")

c("sex", "age", "migration_background", "car_ownership", "income",
  "employment_status", "education_level", "HH_size",  "nr_children_yonger6",
  "nr_child_6_11", "nr_child_12_17", "trip_purpose")

#not found = "BMI", "car_access"
# modal choice outcome = "Hvm"

## restructuring and relabeling dataset
dutchnames = c("VerplID", "VertPC", "AankPC", "Hvm", "OPID", "Geslacht", "Leeftijd",
               "Herkomst", "HHAuto", "HHGestInkG",
               "BetWerk", "Opleiding", "HHPers",
               "HHLft1", "HHLft2", "HHLft3", "MotiefV")

englishnames = c("TripID", "orig_postcode", "dest_postcode", "modal_choice", "Person_ID", "sex", "age",
                 "migration_background", "Nr_cars_hh",
                 "income",  "employment_status",
                 "education_level", "HH_size",  "nr_children_yonger6",
                 "nr_child_6_11", "nr_child_12_17", "trip_purpose")

ODIN2018_Studyarea = ODIN2018_Studyarea[, c(match(c(dutchnames),colnames(ODIN2018_Studyarea)),
                                            which(!(colnames(ODIN2018_Studyarea) %in% dutchnames)))]
ODIN2019_Studyarea = ODIN2019_Studyarea[, c(match(c(dutchnames),colnames(ODIN2019_Studyarea)),
                                            which(!(colnames(ODIN2019_Studyarea) %in% dutchnames)))]

for(indx in 1:length(dutchnames)){
  colnames(ODIN2018_Studyarea)[which(colnames(ODIN2018_Studyarea) == dutchnames[indx])] = englishnames[indx]
  colnames(ODIN2019_Studyarea)[which(colnames(ODIN2019_Studyarea) == dutchnames[indx])] = englishnames[indx]
}


setwd(paste(dataFolder, "/ODIN/2019", sep = ""))
write.csv(ODIN2019_Studyarea, "ODIN2019_Studyarea.csv", row.names = F)

setwd(paste(dataFolder, "/ODIN/2018", sep = ""))
write.csv(ODIN2018_Studyarea, "ODIN2018_Studyarea.csv", row.names = F)


# permutations of PC6 versions of PC4 origin destinations
setwd(paste(dataFolder, "/ODIN/2019", sep = ""))
ODIN2019_Studyarea = read.csv("ODIN2019_Studyarea.csv")

ODIN2018_Studyarea = ODIN2018_Studyarea[, englishnames]

setwd(paste(dataFolder, "/ODIN/2018", sep = ""))
ODIN2018_Studyarea = read.csv("ODIN2018_Studyarea.csv")
remove(PC6, PC6_StudyArea)

PC6_small = PC6_StudyArea[, c("PC4", "PC6")]
colnames(PC6_small) = c("orig_postcode", "orig_PC6")


ODIN2018_Studyarea_origPC6 = merge(ODIN2018_Studyarea, PC6_small, by= "orig_postcode", all.x = T, all.y = F)
write.csv(ODIN2018_Studyarea_origPC6, "ODIN2018_Studyarea_origPC6.csv", row.names = F)
ODIN2019_Studyarea_origPC6 = merge(ODIN2019_Studyarea, PC6_small, by= "orig_postcode", all.x = T, all.y = F)

ODIN2018_Studyarea_origPC6 = ODIN2018_Studyarea_origPC6[sample(1:nrow(ODIN2018_Studyarea_origPC6), size= 1000), ]
ODIN2019_Studyarea_origPC6 = ODIN2019_Studyarea_origPC6[sample(1:nrow(ODIN2019_Studyarea_origPC6), size= 1000), ]

colnames(PC6_small) = c("dest_postcode", "dest_PC6")

i=1

ODIN2018_Studyarea_PC6 = merge(ODIN2018_Studyarea_origPC6[((i-1)*10000):(i*10000),], PC6_small, by= "dest_postcode", all.x = T, all.y = F)

for(i in 2:as.integer(nrow(ODIN2018_Studyarea_origPC6)/5000)){
  print(paste("computing rows:", as.character((i-1)*5000), "to", as.character(i*5000)))
  ODIN2018_Studyarea_PC61 = merge(ODIN2018_Studyarea_origPC6[((i-1)*5000):(i*5000),], PC6_small, by= "dest_postcode", all.x = T, all.y = F)
  ODIN2018_Studyarea_PC6 = rbind(ODIN2018_Studyarea_PC6, ODIN2018_Studyarea_PC61)
}


ODIN2018_Studyarea_PC61 = merge(ODIN2018_Studyarea_origPC61[0:10000,], PC6_small, by= "dest_postcode", all.x = T, all.y = F)
ODIN2019_Studyarea_PC6 = merge(ODIN2019_Studyarea_origPC6, PC6_small, by= "dest_postcode", all.x = T, all.y = F)


ODIN2018_Studyarea_PC6 = ODIN2018_Studyarea_PC6[sample(1:nrow(ODIN2018_Studyarea_PC6), size= 1000), ]
ODIN2019_Studyarea_PC6 = ODIN2019_Studyarea_PC6[sample(1:nrow(ODIN2019_Studyarea_PC6), size= 1000), ]


ODIN2018_Studyarea_PC6 = ODIN2018_Studyarea_PC6[, c(match(c("orig_PC6", "dest_PC6"),colnames(ODIN2018_Studyarea_PC6)),
                                            which(!(colnames(ODIN2018_Studyarea_PC6) %in% c("orig_PC6", "dest_PC6"))))]
ODIN2019_Studyarea_PC6 = ODIN2019_Studyarea_PC6[, c(match(c("orig_PC6", "dest_PC6"),colnames(ODIN2019_Studyarea_PC6)),
                                            which(!(colnames(ODIN2019_Studyarea_PC6) %in% c("orig_PC6", "dest_PC6"))))]

setwd(paste(dataFolder, "/ODIN/2019", sep = ""))
write.csv(ODIN2019_Studyarea_PC6, "ODIN2019_Studyarea_PC6_sample.csv", row.names = F)

setwd(paste(dataFolder, "/ODIN/2018", sep = ""))
write.csv(ODIN2018_Studyarea_PC6, "ODIN2018_Studyarea_PC6_sample.csv", row.names = F)


# environmental behavior determinants
"noise"
"destin_density"
"destin_diversity"
"dist_to_CBD"
"neigh_SES"
"streetlights"
"greenery"
