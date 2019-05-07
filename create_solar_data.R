getwd();
setwd('C:/Users/Anish/Desktop/Research Project');
#install.packages("GGally")
#install.packages('RNetCDF')
#install.packages('ncdf4')
#install.packages('data.table')
#install.packages('stringr')
#install.packages('lettercase')
#install.packages('dplyr')
library('RNetCDF')
library('ncdf4')
library('data.table')
library('stringr')
library('lettercase')
library('dplyr')
library('GGally')

random_records<-read.csv('random_station_details.csv')
county_names<-unique(random_records$County)
county_names<-county_names[order(county_names)]
summary(county_names)
get_solar_records<-function(county,file_count,clear_sky){
  file_name<-""
  print(paste("County: ",county))
  if(file_count==0)
  {
    file_name<-paste(str_decapitalize(county),"irradiation-hourly-May2008-May2018.nc",sep="-")
    
  }
  if(file_count!=0)
  {
    file_name<-paste(str_decapitalize(county),file_count,"irradiation-hourly-May2008-May2018.nc",sep="-")
  }
  
  
  nc<-nc_open(file_name , write=FALSE, readunlim=TRUE, verbose=FALSE, 
              auto_GMT=FALSE, suppress_dimvals=TRUE)
  
  if(!clear_sky)
  {
    print(print(paste("Reading: Solar ",file_name,sep="-" )) )
    solar_data_GHI<-ncvar_get(nc, varid=nc$var$GHI, start=NA, count=NA, verbose=FALSE,
                              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
    solar_data_BNI<-ncvar_get(nc, varid=nc$var$BNI, start=NA, count=NA, verbose=FALSE,
                              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
    solar_data_DHI<-ncvar_get(nc, varid=nc$var$DHI, start=NA, count=NA, verbose=FALSE,
                              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
    solar_data_BHI<-ncvar_get(nc, varid=nc$var$BHI, start=NA, count=NA, verbose=FALSE,
                              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
  }
  if(clear_sky)
  {
    print(print(paste("Reading: Clear-Sky ",file_name,sep=" " )) )
    solar_data_CSKY_GHI<-ncvar_get(nc, varid=nc$var$CLEAR_SKY_GHI, start=NA, count=NA, verbose=FALSE,
                                   signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
    solar_data_CSKY_BNI<-ncvar_get(nc, varid=nc$var$CLEAR_SKY_BNI, start=NA, count=NA, verbose=FALSE,
                                   signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
    solar_data_CSKY_DHI<-ncvar_get(nc, varid=nc$var$CLEAR_SKY_DHI, start=NA, count=NA, verbose=FALSE,
                                   signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
    solar_data_CSKY_BHI<-ncvar_get(nc, varid=nc$var$CLEAR_SKY_BHI, start=NA, count=NA, verbose=FALSE,
                                   signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
  }
  
  solar_data_date<-ISOdatetime(
    ncvar_get(nc, varid=nc$var$ut_year, start=NA, count=NA, verbose=FALSE,
              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ),
    ncvar_get(nc, varid=nc$var$ut_month, start=NA, count=NA, verbose=FALSE,
              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ),
    ncvar_get(nc, varid=nc$var$ut_day, start=NA, count=NA, verbose=FALSE,
              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ),
    ncvar_get(nc, varid=nc$var$ut_hour, start=NA, count=NA, verbose=FALSE,
              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ),
    ncvar_get(nc, varid=nc$var$ut_minute, start=NA, count=NA, verbose=FALSE,
              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ),
    ncvar_get(nc, varid=nc$var$ut_second, start=NA, count=NA, verbose=FALSE,
              signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ))
  
  solar_data_G0<- ncvar_get(nc, varid=nc$var$G0, start=NA, count=NA, verbose=FALSE,
                            signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE )
  solar_data_Year<-as.factor(ncvar_get(nc, varid=nc$var$ut_year, start=NA, count=NA, verbose=FALSE,
                                       signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ))
  solar_data_Month<-as.factor(ncvar_get(nc, varid=nc$var$ut_month, start=NA, count=NA, verbose=FALSE,
                                        signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ))
  solar_data_Day<-as.factor(ncvar_get(nc, varid=nc$var$ut_day, start=NA, count=NA, verbose=FALSE,
                                      signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ))
  solar_data_Hour<-as.factor(ncvar_get(nc, varid=nc$var$ut_hour, start=NA, count=NA, verbose=FALSE,
                                       signedbyte=TRUE, collapse_degen=TRUE, raw_datavals=FALSE ))
  nc_close(nc)
  rm(nc)
  print(paste("File Closed",file_name,sep=" "))
  ##########SOLAR DATA###############
  if(!clear_sky)
  {
    solar_data_df<-data.frame(County=county,Timestamp=solar_data_date,
                              Year=solar_data_Year,Month=solar_data_Month,Day=solar_data_Day,
                              Hour=solar_data_Hour,G0=solar_data_G0,
                              GHI=solar_data_GHI,DHI=solar_data_DHI,
                              BHI=solar_data_BHI,BNI=solar_data_BNI)
    return(solar_data_df)
  }
  if(clear_sky){
    solar_data_cs_df<-data.frame(County=county_names[i],Timestamp=solar_data_date,
                                 Year=solar_data_Year,Month=solar_data_Month,Day=solar_data_Day,
                                 Hour=solar_data_Hour,G0=solar_data_G0,
                                 CS_GHI=solar_data_CSKY_GHI,CS_DHI=solar_data_CSKY_DHI,
                                 CS_BHI=solar_data_CSKY_BHI,CS_BNI=solar_data_CSKY_BNI)
    return(solar_data_cs_df)
  }
  
}
#test<-get_solar_records(county_names[1],5,FALSE)

##########SOLAR DATA###############



#temporary solution
#sapply(solar_data_main,function(x) sum(is.na(x)))
#solar_data_main<-na.omit(solar_data_main)
for(i in 1:length(county_names)){
  max_limit<-5
  if(county_names[i]=="Longford"){
    max_limit<-3}
  for(j in 0:max_limit){
    if(i==1 & j==0){
      solar_data_main<-get_solar_records(county = county_names[i],file_count = j,clear_sky = FALSE)
      solar_data_cs_main<-get_solar_records(county = county_names[i],file_count = j,clear_sky = TRUE)
    }
    else{
      solar_data_df<-get_solar_records(county = county_names[i],file_count = j,clear_sky = FALSE)
      solar_data_cs_df<-get_solar_records(county = county_names[i],file_count = j,clear_sky = TRUE)
      solar_data_main<-rbind(solar_data_main,solar_data_df)
      solar_data_cs_main<-rbind(solar_data_cs_main,solar_data_cs_df)
    }
  }
  rm(solar_data_df)
  rm(solar_data_cs_df)
}
#summary(nc)
summary(solar_data_main)
summary(solar_data_cs_main)
resetData<-solar_data_main
reset_CS_data<-solar_data_cs_main
save(resetData, file="resetData.RData")
save(reset_CS_data, file="reset_CS_Data.RData")