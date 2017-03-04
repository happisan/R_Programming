#attempt to plot the average temperatuere of major citiies on google map

#load the library
library(ggmap)
library(ggplot2)

#Import the major city data
mcity <- read.csv("GlobalLandTemperaturesByMajorCity.csv")
#remove the N,E,W,S from lat & long
mcity$Longitude = gsub('W','',mcity$Longitude)
mcity$Longitude = gsub('E','',mcity$Longitude)
mcity$Latitude = gsub('N','',mcity$Latitude)
mcity$Latitude = gsub('S','',mcity$Latitude)

#Convert to respective data formats
mcity$City = as.character(mcity$City)
mcity$Country = as.character(mcity$Country)
mcity$dt = as.Date.factor(mcity$dt)
mcity$Latitude = as.numeric(mcity$Latitude)
mcity$Longitude = as.numeric(mcity$Longitude)


#aggregate the average temperature for every year across the country,city
agg_temp <- aggregate(mcity,by = list(format(mcity$dt,'%Y'),mcity$Country,mcity$City),mean)
#remove unwanted columns
agg_temp$City <- NULL
agg_temp$Country <- NULL

#Rename the groupped columns
colnames(agg_temp)[1] <- 'Year'
colnames(agg_temp)[2] <- 'Country'
colnames(agg_temp)[3] <- 'City'

#for now, remove where avg. temp data is absent
agg_temp <- agg_temp[!is.na(agg_temp$AverageTemperature),]

#creae a data frame for India
Ind_temp <- agg_temp[agg_temp$Country == "India"& agg_temp$Year == 2012,]

#set the basemap as India
ind_center <- as.numeric(geocode("India"))

#get India Map
IndMap <- ggmap(get_googlemap(center = ind_center,scale = 2,zoom = 4),extent = 'normal')

#add the points to the map using Avg Temp & add labels
IndMap + geom_point(aes(x=Longitude,y=Latitude), data = Ind_temp, col='red',alpha = 0.4
                    , size = Ind_temp$AverageTemperature*0.1) + 
  scale_size_continuous(range = range(Ind_temp$AverageTemperature)) + 
  geom_text(aes(x=Longitude, y=Latitude, label = round(Ind_temp$AverageTemperature,digits = 1))
      ,data = Ind_temp
      ,color = 'black')

