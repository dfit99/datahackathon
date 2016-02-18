sample.data = read.csv("/Users/yilinshang/Documents/hackthon/Cornell Hackthon/sample data.csv",header = T)

pairs(as.numeric(sample.data$Fare_amount)~as.numeric(sample.data$Trip_distance)+sample.data$TAVG+factor(sample.data$Payment_type)+factor(sample.data$time))

# Read in the precipitation data
precip.data = read.csv("/Users/yilinshang/Documents/hackthon/Cornell Hackthon/precipitation.csv",header = T)
precip.data$DATE = as.character(precip.data$DATE)

# Merge precipitation data with taxi data and find some relationships with precipitation
sample.data.precip = merge(sample.data,precip.data,by.x = "datee",by.y = "DATE",all.x = T)
fit.distance.precip = lm(as.numeric(sample.data.precip$Trip_distance)~sample.data.precip$snow+sample.data.precip$rain)
summary(fit.distance.precip)
fit.amount.precip = lm(as.numeric(sample.data.precip$Fare_amount)~sample.data.precip$snow+sample.data.precip$rain)
summary(fit.amount.precip)

# Read in the coordinate and zip code transformation data
zipcode = read.csv("/Users/yilinshang/Documents/hackthon/Cornell Hackthon/output4.csv",sep = ",",header = F)
names(zipcode) = c("zipcodes","latitude","longitude")

sample.data.precip$Pickup_longitude = as.numeric(sample.data.precip$Pickup_longitude)
sample.data.precip$Pickup_latitude = as.numeric(sample.data.precip$Pickup_latitude)

# Merge the sample data set with the zipcode data
sample.precip.zip = cbind(zipcode[order(zipcode$latitude,zipcode$longitude),],sample.data.precip[order(sample.data.precip$Pickup_latitude,sample.data.precip$Pickup_longitude),])

# Regression and estimation
fit.distance.zipcodes_precip = lm(as.numeric(sample.precip.zip$Trip_distance)~sample.precip.zip$snow+sample.precip.zip$rain+factor(sample.precip.zip$zipcodes))
summary(fit.distance.zipcodes_precip)
coeff=coefficients(fit.distance.zipcodes_precip)
xx = fitted.values(fit.distance.zipcodes_precip)
result = data.frame(zipcode=sample.precip.zip$zipcodes,snow = sample.precip.zip$snow,rain = sample.precip.zip$rain,fitted_distance=xx)
unique_result = unique(result)
write.csv(unique_result,"/Users/yilinshang/Documents/hackthon/Cornell Hackthon/fitted values.csv")

# Difference between fitted value and original data
fit.fare.distance = lm(sample.precip.zip$Fare_amount~sample.precip.zip$Trip_distance)
plot(xx~sample.precip.zip$Trip_distance,xlab = "distance",ylab = "fitted")
x=1:30
y=x
lines(x,y)

# Relation between fare amount and trip distance
sample.precip.zip=sample.precip.zip[(sample.precip.zip$Fare_amount>0 & sample.precip.zip$Fare_amount<250),]
plot(sample.precip.zip$Fare_amount~sample.precip.zip$Trip_distance,xlab = "Trip_distance",ylab = "Fare_amount")
fit.amount.distance = lm(sample.precip.zip$Fare_amount~sample.precip.zip$Trip_distance)
lines(sample.precip.zip$Trip_distance,fitted.values(fit.amount.distance))


