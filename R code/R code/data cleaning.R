library(data.table)

# Create a dataset containing all the NYC taxi data in 2015 
fnames=list.files("~/Documents/hackthon/Cornell Hackthon/2015 green taxi data")
taxi_data_2015 = data.frame()
for (names in fnames) {
  data = fread(names,sep = ',',header = F,drop = c(1,4,5,17,22,23))
  taxi_data_2015 = rbind(taxi_data_2015,data)
}
taxi_data_2015 = taxi_data_2015[-1,]
names(taxi_data_2015) = c("lpep_pickup_datetime","Lpep_dropoff_datetime","Pickup_longitude","Pickup_latitude","Dropoff_longitude","Dropoff_latitude","Passenger_count","Trip_distance","Fare_amount","Extra","MTA_tax","Tip_amount","Tolls_amount","improvement_surcharge","Total_amount","Payment_type","Trip_type")

# Read in the temperature data
weather = fread("~/Documents/hackthon/Cornell Hackthon/WeatherData.csv",sep = ',')
weather$DATE=as.character(weather$DATE)

# Get the date 
taxi_data_2015$datee = gsub("-","",substr(taxi_data_2015$lpep_pickup_datetime,1,10))
taxi_data_2015$Total_amount = as.numeric(taxi_data_2015$Total_amount)
taxi_data_2015$Trip_distance = as.numeric(taxi_data_2015$Trip_distance)

# Merge the taxi data with temperature data
taxi_15_weather = merge(taxi_data_2015,weather,by.x = "datee",by.y = "DATE",all.x = T)

# Save the data
save(taxi_15_weather,file = "~/Documents/hackthon/Cornell Hackthon/taxi_weather_15.Rdata")

# Some linear regression
fit.amount.avgT_distance = lm(taxi_15_weather$Total_amount~as.numeric(taxi_15_weather$TAVG)+as.numeric(taxi_15_weather$Trip_distance))
fit.amount.avgT = lm(taxi_15_weather$Total_amount~as.numeric(taxi_15_weather$TAVG))
fit.amount.latitude = lm(taxi_15_weather$Total_amount~as.numeric(taxi_15_weather$Pickup_latitude))
plot(weather$TAVG)

# Analysis of daily average
daily_avg = tapply(taxi_15_weather$Total_amount,taxi_15_weather$datee,mean)
daily_distance_avg = tapply(as.numeric(taxi_15_weather$Trip_distance),taxi_15_weather$datee,mean)
daily_avg = data.frame(avg_total_amount=as.vector(daily_avg),TAVG = weather$TAVG,date = weather$DATE)
daily_avg$avg_distance = as.vector(daily_distance_avg)
plot(avg_total_amount~TAVG,data = daily_avg)
library(car)
pairs(daily_avg$avg_total_amount~daily_avg$TAVG+daily_avg$avg_distance)
fit.avgamount.avgT = lm(avg_total_amount~TAVG,data = daily_avg)

# Relationships between amount and the time (hour)
load("/Users/yilinshang/Documents/hackthon/Cornell Hackthon/taxi_weather_15.Rdata")
taxi_15_weather$time = as.numeric(substr(taxi_15_weather$lpep_pickup_datetime,12,13))
fit.amount.hour = lm(taxi_15_weather$Total_amount~taxi_15_weather$time)
fit.amount.hour_distance = lm(taxi_15_weather$Total_amount~taxi_15_weather$time+taxi_15_weather$Trip_distance)

# Some statistics
stat = summary(taxi_15_weather[c(9,10,11,12,13,14,15,16,24),])
sd(taxi_15_weather$Trip_distance)
sd(taxi_15_weather$Fare_amount)
sd(taxi_15_weather$Extra)
sd(taxi_15_weather$MTA_tax)
sd(taxi_15_weather$Tip_amount)
sd(taxi_15_weather$Tolls_amount)
sd(taxi_15_weather$improvement_surcharge)
sd(taxi_15_weather$Total_amount)

# Grab a sample data
set.seed(1234)
random.number = sample(seq(1:19233763),10000,replace = F)
sample.data = taxi_15_weather[random.number,]
write.csv(sample.data,"/Users/yilinshang/Documents/hackthon/Cornell Hackthon/sample data.csv")

