library(corrplot)

Data <- read.csv("Geo-Fence Analytics.csv")

#### Create a new column imp_large #####

for (i in 1:nrow(Data)){
if (Data$imp_size[i] == 728*90) {Data$imp_large[i] <- 1
} else {Data$imp_large[i] <- 0
}
}

for (i in 1:nrow(Data)){
  if (Data$app_topcat[i] == "IAB1" || Data$app_topcat[i] == "IAB2" || Data$app_topcat[i] == "IAB3" || Data$app_topcat[i] == "IAB4" || Data$app_topcat[i] == "IAB5" || Data$app_topcat[i] == "IAB6" ) {Data$cat_entertainment[i] <- 1
} else {Data$cat_entertainment[i] <- 0
}
}


for (i in 1:nrow(Data)){
  if (Data$app_topcat[i] == "IAB14" ) {Data$cat_social[i] <- 1
} else {Data$cat_social[i] <- 0
}
}


for (i in 1:nrow(Data)){
  if (Data$app_topcat[i] == "IAB6" || Data$app_topcat[i] == "IAB7" || Data$app_topcat[i] == "IAB8" || Data$app_topcat[i] == "IAB9" || Data$app_topcat[i] == "IAB5" || Data$app_topcat[i] == "IAB10" || Data$app_topcat[i] == "IAB11"|| Data$app_topcat[i] == "IAB12"|| Data$app_topcat[i] == "IAB13"|| Data$app_topcat[i] == "IAB14"|| Data$app_topcat[i] == "IAB15"|| Data$app_topcat[i] == "IAB16"|| Data$app_topcat[i] == "IAB17"|| Data$app_topcat[i] == "IAB18"|| Data$app_topcat[i] == "IAB19" ) {Data$cat_tech[i] <- 1
} else {Data$cat_tech[i] <- 0
}
}


for (i in 1:nrow(Data)){
  if (Data$device_os[i] == "iOS") {Data$os_ios[i] <- 1
} else {Data$os_ios[i] <- 0
}
}


haversine <- function(lat1, lon1, lat2, lon2){
  dlat <- (lat2 - lat1)*pi/180
  dlon <- (lon2 - lon1)*pi/180
  
  lat1 <- (lat1*pi/180)
  lat2 <- (lat2*pi/180)
  
  D = (sin(dlat/2)**2 + (sin(dlon/2)**2) * (cos(lat1)) * cos(lat2))
  
  rad = 6371
  
  c = 2 * asin(sqrt(D))
  
  return(rad * c)
}


for (i in 1:nrow(Data)){
  Data$distance[i] <- haversine(Data$device_lat[i], Data$device_lon[i], Data$geofence_lat[i], Data$geofence_lon[i])
}

for(i in 1:nrow(Data)){
  Data$distance_sq[i] <- Data$distance[i]*Data$distance[i]
}

for(i in 1:nrow(Data)){
  Data$ln_app_review_vol[i] <- log(Data$app_review_vol[i])
}

summary(Data$didclick)

summary(Data$distance)

summary(Data$imp_large)

summary(Data$cat_entertainment)

summary(Data$cat_social)
          
summary(Data$cat_tech)

summary(Data$os_ios)

summary(Data$ln_app_review_vol)

summary(Data$app_review_val)

Dataframe <- as.data.frame(Data)

correln <- cor(Dataframe[sapply(Dataframe, is.numeric)])

corrplot(correln)

plot(Data$distance, Data$didclick)

smp_size <- floor(.75*nrow(Data))
traindata <- Dataframe[1:smp_size,] 
testdata <- Dataframe[(smp_size+1):nrow(Data),]

LogitMod <- glm(traindata$didclick ~ traindata$distance + traindata$imp_large + traindata$cat_entertainment + traindata$os_ios + traindata$cat_tech)

summary(LogitMod)
