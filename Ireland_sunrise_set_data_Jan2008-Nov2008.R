setwd('C:/Users/Anish/Desktop/Research Project');
#getwd()
library(htmltab)
library(lettercase)
library(stringr)
library(tm)
 trim <- function (x) {
   gsub("^\\s+|\\s+$", "", x)
 }
create_url<-function(month,year){
  url<-paste("https://www.timeanddate.com/sun/@2963597?month=",month,"&year=",year,sep="")
  return(url)
}


get_sunrise_set_times<-function(url,month,year){
solar_sun_rise_set <- htmltab(doc=url, which=1)
sunrise_set_data<-subset(solar_sun_rise_set,select = c(1:3))
sunrise_set_data<-sunrise_set_data[-1,]
if(nrow(solar_sun_rise_set>2)){
colnames(sunrise_set_data)[1]<-"Day"
colnames(sunrise_set_data)[2]<-"Sunrise_Time"
colnames(sunrise_set_data)[3]<-"Sunset_Time"

sunrise_set_data$Day<-as.numeric(sunrise_set_data$Day)
sunrise_set_data$Sunrise_Time<-gsub(" ","_",sunrise_set_data$Sunrise_Time)
sunrise_set_data$Sunrise_Time<-trim(gsub("_.*","",sunrise_set_data$Sunrise_Time))

sunrise_set_data$Sunset_Time<-gsub(" ","_",sunrise_set_data$Sunset_Time)
sunrise_set_data$Sunset_Time<-trim(gsub("_.*","",sunrise_set_data$Sunset_Time))
sunrise_set_data$Year<-year
sunrise_set_data$Month<-month

temp_rise<-str_split(sunrise_set_data$Sunrise_Time,":")
sunrise_set_data$Sunrise_Hour<-as.numeric(temp_rise[[1]][[1]])
sunrise_set_data$Sunrise_Minute<-as.numeric(temp_rise[[1]][[2]])

temp_set<-str_split(sunrise_set_data$Sunset_Time,":")
sunrise_set_data$Sunset_Hour<-as.numeric(temp_set[[1]][[1]])
sunrise_set_data$Sunset_Minute<-as.numeric(temp_set[[1]][[2]])
temp_row<-sunrise_set_data[1,]
temp_row$Day<-1
sunrise_set_data<-rbind(temp_row,sunrise_set_data)
}
return(sunrise_set_data)
}

sunrise_set_master<-data.frame(Day=numeric(),Sunrise_Time=character(),Sunset_Time=character(),Year=numeric(),Month=numeric(),Sunrise_Hour=numeric(),Sunrise_Minute=numeric(),Sunset_Hour=numeric(),Sunset_Minute=numeric())
for(i in seq(2008,2018)){
  for(j in seq(1,12)){
    get_url<-create_url(j,i)
    sunrise_set_df<-get_sunrise_set_times(get_url,j,i)
    sunrise_set_master<-rbind(sunrise_set_master,sunrise_set_df)
    rm(sunrise_set_df)
    rm(get_url)
  }

}

summary(sunrise_set_master)
na_rows<-is.na(sunrise_set_master)
na_data<-sunrise_set_master[na_rows,]
sunrise_set_master<-na.omit(sunrise_set_master)

sunrise_set_master$SunRise_Timestamp<-ISOdatetime(sunrise_set_master$Year,sunrise_set_master$Month,sunrise_set_master$Day,sunrise_set_master$Sunrise_Hour,sunrise_set_master$Sunrise_Minute,sec=0)
sunrise_set_master$SunSet_Timestamp<-ISOdatetime(sunrise_set_master$Year,sunrise_set_master$Month,sunrise_set_master$Day,sunrise_set_master$Sunset_Hour,sunrise_set_master$Sunset_Minute,sec=0)

write.csv(sunrise_set_master,file='sunrise_set_master.csv',row.names = FALSE)
