earthquake<-read.csv('C:\\Users\\mina\\Desktop\\R\\earthquake.csv')
summary(earthquake)
class(earthquake)
######## Dicovery el data#########


names(earthquake)

head(earthquake)

levels(earthquake$country)

levels(earthquake$city)

nrow(earthquake)
ncol(earthquake)

sort(earthquake$richter)
sort(earthquake$richter,decreasing=TRUE)

length(which(earthquake$richter==5))
length(which(earthquake2$country=="turkey"))
###### cleaning ##############


#bat2kd en mafish NA values fel  columns low feh Na values yb2a hailt3li NA fel output
mean(earthquake$richter)

mean(earthquake$depth)

#hna kda tel3li NA values
mean(earthquake$dist)
#kda shelt el missing value
earthquake$dist=ifelse(is.na(earthquake$dist),ave(earthquake$dist,FUN =function(x)mean(x,na.rm = TRUE)),earthquake$dist)

#mean(earthquake$dist,na.rm=TRUE)


#kda brdo mafish moshkla fel lat wel long
mean(earthquake$lat)
mean(earthquake$long)

#taking care of missing value
earthquake$dist=ifelse(is.na(earthquake$dist),ave(earthquake$dist,FUN =function(x)mean(x,na.rm = TRUE)),earthquake$dist)


#low 3mlt el 5atwa deh 2abl el satrin elli fo2 haitb2a 1731 rows we low ba3dhom haib2a 3ndna 10000 rows
earthquake2<-na.omit(earthquake)

summary(earthquake2)


######### visualization###########

# Van city has the largest number of earthquakes
citywithlargestnumbersofearthquakes<-table(sort(earthquake$city[earthquake$richter>0]))
plot(citywithlargestnumbersofearthquakes)
citywithlargestnumbersofearthquakes
#but which city has dangerous earhquakes
#mugla city has 7 
citySortedFromgigtosmall<-table(sort(earthquake$city[earthquake$richter>=5.5]))
plot(citySortedFromgigtosmall)
citySortedFromgigtosmall

length(which(earthquake$city=="mugla"))

#area most affected with dangerous earthquakes
areaSorted<-table(sort(earthquake$area[earthquake$city=="Izmir"]))
areaSorted[earthquake$richter>=5.5]
areaSorted

# fa kda haitla3 bel zero ya3ni mafish zelzal
hist(earthquake2$richter,col="lightblue",xlab="richter",main="richter frequency")

# mn 3'ir zero
hist(earthquake2$richter[!earthquake2$richter==0],col="lightblue",xlab="richter",main="richter frequency")

#density plot
richter<-density(earthquake2$richter[!earthquake2$richter==0])
plot(richter)
##########


#density over histogram
hist(earthquake2$richter[!earthquake2$richter==0],col="lightblue",xlab="richter",main="richter frequency",freq = F)

lines(richter)
######
plot(earthquake2$richter[!earthquake2$richter==0])
plot(earthquake2$city,earthquake2$richter,xlab="city",ylab="richter")

#how many earthquakes in each city
counts<-table(earthquake2$city[earthquake2$richter!=0])
barplot(counts,xlab="city" , beside = T )



#timeseries=subset(earthquake2,select =-c(id,country,city,area,direction,xm,md,mw,ms,mb,dist))
#timeseries
#class(timeseries)
depthdestRichter<-data.frame("depth"=earthquake2$depth,"dist"=earthquake2$dist,
                              "Richter"=earthquake2$richter)

pairs(depthdestRichter)
##############################visualization with ggplot2
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

plot(earthquake2$richter[!earthquake2$richter==0])
earth=(earthquake2$richter[!earthquake2$richter==0])
qplot(richter,data=earthquake2,bins=30)
qplot(richter,data=earthquake2,bins=97)
qplot(richter,data=earthquake2,bins=7)
#qplot(richter,data=earthquake,bins=30)

earthquake2$scale[earthquake2$richter >= 8] <- "Great Damage"
earthquake2$scale[earthquake2$richter >= 7 & earthquake2$richter<= 7.9]<-"Serious Damage"

earthquake2$scale[earthquake2$richter >=6.1 & earthquake2$richter<=6.9]<-"Severe Damage"
earthquake2$scale[earthquake2$richter >=5.5 & earthquake2$richter<=6]<-"Slight Damage"
earthquake2$scale[earthquake2$richter >=2.5 & earthquake2$richter<=5.4]<-"Minor Damage"
earthquake2$scale[earthquake2$richter <= 2.5] <- "Limited Damage"

head(earthquake2$scale)

levels(earthquake2$scale) <- c("Great Damage", "Serious Damage", "Severe Damage", "Slight Damage","Minor Damage", "Limited Damage")

levels(earthquake2$scale)

earthquake_scale<-count(earthquake2,scale)
earthquake_scale<-na.omit(earthquake_scale)
earthquake_scale

ggplot(data=earthquake_scale, aes(x = scale, y = n, fill = scale)) +
     geom_bar(stat = "identity") +
     xlab("Earthquake Magnitude by Scale") +
     ylab("Total Earthquakes") +
     ggtitle("Global Earthquake Magnitude Scale") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
     theme(plot.title = element_text(hjust = 0.5))

levels(earthquake2$country)
length(which(earthquake2$country=="turkey"))


# maps latitude and longitude columns
install.packages("rworldmap")
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
points(earthquake2$long[earthquake2$richter>6], earthquake2$lat[earthquake2$richter>6], col = "red", cex = .6)
############# time series analysis###############
#x<-earthquake2$date[1:20]
#rdate<-as.Date(earthquake2$date,"%Y.%m.%d")
#plot(earthquake2$richter~rdate,type="l",col="red" )
