library(rjson)
url<-"http://erddap.marine.ie/erddap/tabledap/IWBNetwork.json?station_id,longitude,latitude,time,AtmosphericPressure,WindDirection,WindSpeed,Gust,WaveHeight,WavePeriod,MeanWaveDirection,Hmax,AirTemperature,DewPoint,SeaTemperature,RelativeHumidity,QC_Flag"
WthrJson <- jsonlite::fromJSON(url, flatten = TRUE)
str(WthrJson)
head(WthrJson)
max(WthrJson$table$rows[,4])

weatherData<-data.frame(station_id=WthrJson$table$rows[,1],
                        longitude=as.numeric(WthrJson$table$rows[,2]),
                        latitude=as.numeric(WthrJson$table$rows[,3]),
                        time=WthrJson$table$rows[,4],
                        AtmosphericPressure=as.numeric(WthrJson$table$rows[,5]),
                        WindDirection=as.numeric(WthrJson$table$rows[,6]),
                        WindSpeed=as.numeric(WthrJson$table$rows[,7]),
                        Gust=as.numeric(WthrJson$table$rows[,8]),
                        WaveHeight=as.numeric(WthrJson$table$rows[,9]),
                        WavePeriod=as.numeric(WthrJson$table$rows[,10]),
                        MeanWaveDirection=as.numeric(WthrJson$table$rows[,11]),
                        Hmax=as.numeric(WthrJson$table$rows[,12]),
                        AirTemperature=as.numeric(WthrJson$table$rows[,13]),
                        DewPoint=as.numeric(WthrJson$table$rows[,14]),
                        SeaTemperature=as.numeric(WthrJson$table$rows[,15]),
                        RelativeHumidity=as.numeric(WthrJson$table$rows[,16]),
                        QC_Flag=as.double(WthrJson$table$rows[,17]))
WthrJson$table$columnTypes
summary(combined_data)

getISO_Time<-function(ISO_date){
  date<-strsplit(as.character(ISO_date),'T')[[1]][1]
  time<-gsub("Z","",strsplit(as.character(ISO_date),'T')[[1]][2])
  finalDate<-ISOdatetime(as.numeric(strsplit(as.character(date),'-')[[1]][1]),
                         as.numeric(strsplit(as.character(date),'-')[[1]][2]),
                         as.numeric(strsplit(as.character(date),'-')[[1]][3]),
                         as.numeric(strsplit(as.character(time),':')[[1]][1]),
                         as.numeric(strsplit(as.character(time),':')[[1]][2]),
                         as.numeric(strsplit(as.character(time),':')[[1]][3])) 
  return(ISO_date)
}

weatherData$time<-as.POSIXct(getISO_Time(weatherData$time))

combined_data<-weatherData[weatherData$time %in% solar_data_main$Timestamp,]

##########AEROSOL DATA###############
aerosol<-c("Observation_time_stamp_UT"
           ,"AOD_Total_Aerosol_Optical_Depth"
           ,"AOD_1240_nm"
           ,"AOD_Black_Carbon"
           ,"AOD_Dust"
           ,"AOD_Organic_Matter"
           ,"AOD_Sea_Salt"
           ,"AOD_Sulfate")
aerosol_main_data<-read.csv('dublin-aerosol-values-May2008-May2018.csv',sep=";",skip=26,col.names = aerosol)
aerosol_main_data$County<-"Dublin"

aerosol_file_name<-paste(county_names[i],'-aerosol-values-May2008-May2018.csv',sep="")
aerosol_data<-read.csv(aerosol_file_name,sep=";",skip=26,col.names = aerosol)
aerosol_data$County<-county_names[i]
aerosol_main_data<-rbind(aerosol_main_data,aerosol_main_data)
rm(aerosol_data)