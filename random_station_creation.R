getwd();
setwd('C:/Users/Anish/Desktop/Research Project');
#install.packages('stringr')
#install.packages('lettercase')
#install.packages('dplyr')
library('stringr')
library('lettercase')
library('dplyr')

Station_Details<-read.csv('StationDetails.csv',sep= ",",skip = 1)
summary(Station_Details)
Station_Details$Close.Year<-ifelse(Station_Details$Close.Year==0,NA,Station_Details$Close.Year)
Open_Station_Details <- Station_Details[is.na(Station_Details$Close.Year),]

county_names<-unique(Open_Station_Details$County)
county_names<-county_names[order(county_names)]
summary(county_names)


get_random_records<-function(county){
  stations<-Open_Station_Details[Open_Station_Details$County==county,]  
  station_sample<-stations
  if(county!='Longford'){
    set.seed(16146107)
    station_sample <- sample_n(stations,5)
  }
  return(station_sample)
}

for(i in seq(1,length(county_names))){
  if(i==1){
    random_records<-get_random_records(county = county_names[i])
  }
  else
  {
    random_records<-rbind(random_records,get_random_records(county = county_names[i]))
  }
}
summary(random_records$County)  

write.csv(random_records,file='random_station_details.csv',row.names = FALSE)