getwd()
setwd('C:/Users/Anish/Desktop/Research Project');
library(lubridate)
load("reset_CS_Data.RData")
load("resetData.RData")
solar_model<-resetData
solar_cs_model<-reset_CS_data
sunrise_set_data<-read.csv('sunrise_set_master.csv')
sunrise_set_data$SunRise_Timestamp<-parse_date_time2(as.character(sunrise_set_data$SunRise_Timestamp),orders = "%Y-%m-%d %H:%M:%S",tz="UTC")
sunrise_set_data$SunSet_Timestamp<-parse_date_time2(as.character(sunrise_set_data$SunSet_Timestamp),orders = "%Y-%m-%d %H:%M:%S",tz="UTC")



create_season_model<-function(season,data){
  months<-seq(1:12)
  result<-data[1,]
  if(season=="spring"){
    spring_data<-data[data$Month==months[3]|
                      data$Month==months[4]|
                      data$Month==months[5],]
    result<-spring_data
  }
  if(season=="summer"){
    summer_data<-data[data$Month==months[6]|
                      data$Month==months[7]|
                      data$Month==months[8],]
    result<-summer_data
  }
  if(season=="winter"){
    winter_data<-data[data$Month==months[1]|
                      data$Month==months[2]|
                      data$Month==months[12],]
    result<-winter_data
  }
  
  if(season=="autumn"){
    autumn_data<-data[data$Month==months[9]|
                      data$Month==months[10]|
                      data$Month==months[11],]
    result<-autumn_data
  }
  return(result)
}


solar_spring_model<-create_season_model("spring",solar_model)
solar_summer_model<-create_season_model("summer",solar_model)
solar_autumn_model<-create_season_model("autumn",solar_model)
solar_winter_model<-create_season_model("winter",solar_model)
cs_solar_spring_model<-create_season_model("spring",solar_cs_model)
cs_solar_summer_model<-create_season_model("summer",solar_cs_model)
cs_solar_autumn_model<-create_season_model("autumn",solar_cs_model)
cs_solar_winter_model<-create_season_model("winter",solar_cs_model)

get_sunrise_average<-function(month,year){
  average_hour<-mean(sunrise_set_data[sunrise_set_data$Month==month&
                                                      sunrise_set_data$Year==year,"Sunrise_Hour"])
  average_minute<-mean(sunrise_set_data[sunrise_set_data$Month==month&
                                               sunrise_set_data$Year==year,"Sunrise_Minute"])
  sunrise_average<-average_hour+average_minute/60
  return(sunrise_average)
}

get_sunset_average<-function(month,year){
  average_hour<-mean(sunrise_set_data[sunrise_set_data$Month==month&
                                        sunrise_set_data$Year==year,"Sunset_Hour"])
  average_minute<-mean(sunrise_set_data[sunrise_set_data$Month==month&
                                          sunrise_set_data$Year==year,"Sunset_Minute"])
  sunset_average<-average_hour+average_minute/60
  return(sunset_average)
}

# print(get_sunrise_average(9,2008))
# print(get_sunset_average(9,2008))

# temp1<-sunrise_set_data$Sunrise_Average
# temp2<-sunrise_set_data$Sunset_Average

# for(i in seq(1:nrow(sunrise_set_data))){
#   sunrise_set_data[i,"Sunrise_Average"]<-get_sunrise_average(sunrise_set_data[i,"Month"],sunrise_set_data[i,"Year"])
#   sunrise_set_data[i,"Sunset_Average"]<-get_sunset_average(sunrise_set_data[i,"Month"],sunrise_set_data[i,"Year"])
# }
solar_autumn_model$Hour
get_true_solar_data<-function(year,month,data){
  average_sunrise<-get_sunrise_average(month = month, year = year)
  average_sunset<-get_sunset_average(month = month,year = year)
  true_solar_data<-data[data$Month == month &
                          as.numeric(data$Hour) >  average_sunrise &
                          as.numeric(data$Hour)< average_sunset &
                          as.numeric(as.character(data$Year)) == year,]
  return(true_solar_data)

}  


true_solar_autumn_model<-solar_autumn_model[1,]
true_solar_autumn_model<-true_solar_autumn_model[-1,]
true_solar_spring_model<-solar_spring_model[1,]
true_solar_spring_model<-true_solar_spring_model[-1,]
true_solar_summer_model<-solar_summer_model[1,]
true_solar_summer_model<-true_solar_summer_model[-1,]
true_solar_winter_model<-solar_winter_model[1,]
true_solar_winter_model<-true_solar_winter_model[-1,]
true_cs_solar_autumn_model<-cs_solar_autumn_model[1,]
true_cs_solar_autumn_model<-true_cs_solar_autumn_model[-1,]
true_cs_solar_spring_model<-cs_solar_spring_model[1,]
true_cs_solar_spring_model<-true_cs_solar_spring_model[-1,]
true_cs_solar_summer_model<-cs_solar_summer_model[1,]
true_cs_solar_summer_model<-true_cs_solar_summer_model[-1,]
true_cs_solar_winter_model<-cs_solar_winter_model[1,]
true_cs_solar_winter_model<-true_cs_solar_winter_model[-1,]
system.time(
for(year in seq(2008,2018)){
  for(month in seq(1,12)){
    if(month==9|month==10|month==11){
    true_solar_autumn_model<-rbind(true_solar_autumn_model,get_true_solar_data(year,month,data = solar_autumn_model))
    true_cs_solar_autumn_model<-rbind(true_cs_solar_autumn_model,get_true_solar_data(year,month,data = cs_solar_autumn_model))
    }
    if(month==3|month==4|month==5){
    true_solar_spring_model<-rbind(true_solar_spring_model,get_true_solar_data(year,month,data = solar_spring_model))
    true_cs_solar_spring_model<-rbind(true_cs_solar_spring_model,get_true_solar_data(year,month,data = cs_solar_spring_model))
    }
    if(month==6|month==7|month==8){
    true_solar_summer_model<-rbind(true_solar_summer_model,get_true_solar_data(year,month,data = solar_summer_model))
    true_cs_solar_summer_model<-rbind(true_cs_solar_summer_model,get_true_solar_data(year,month,data = cs_solar_summer_model))
    }
    if(month==1|month==2|month==12){
    true_solar_winter_model<-rbind(true_solar_winter_model,get_true_solar_data(year,month,data = solar_winter_model))
    true_cs_solar_winter_model<-rbind(true_cs_solar_winter_model,get_true_solar_data(year,month,data = cs_solar_winter_model))
    }
  }
}
)
# user  system elapsed 
# 267.12   57.91  435.32 

################################SEASON MODELS CREATED#############
save(true_cs_solar_autumn_model, file="true_cs_solar_autumn_model.RData")
save(true_cs_solar_spring_model, file="true_cs_solar_spring_model.RData")
save(true_cs_solar_summer_model, file="true_cs_solar_summer_model.RData")
save(true_cs_solar_winter_model, file="true_cs_solar_winter_model.RData")
save(true_solar_autumn_model, file="true_solar_autumn_model.RData")
save(true_solar_spring_model, file="true_solar_spring_model.RData")
save(true_solar_summer_model, file="true_solar_summer_model.RData")
save(true_solar_winter_model, file="true_solar_winter_model.RData")
##########################################################################

