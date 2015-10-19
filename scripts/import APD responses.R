# ---------------------------------------------------------------------------------------------------

sapply(c("ggplot2", "dplyr", "lubridate", "reshape2", "magrittr", "stringi", 
         "tidyr", "ggmap", "ggthemes", "scales", "RSocrata"), require, character.only=TRUE)  
# Read them

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#               APD Incident Extract YTD                #
# https://data.austintexas.gov/resource/b4y9-5x39.csv   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Required packages
sapply(c("dplyr", "lubridate", "reshape2", "RSocrata"), require, character.only=TRUE)  

# Load data set
ReportsAPD <- RSocrata::read.socrata("https://data.austintexas.gov/resource/b4y9-5x39.csv")

# Select useful columns & rename
APD = select(ReportsAPD, - Location.1, -LOCATION_TYPE) %>% 
  rename(ID = Incident.Report.Number, Crime = Crime.Type) %>%
  mutate(Date = mdy(Date),
         Hour = hours(Time %/% 100),
         Minutes = minutes(Time-((Time %/% 100)*100)),
         Date  = Date+Hour+Minutes) %>% 
  select(-Minutes, -Hour, -Time) 
APD$Crime <- factor(APD$Crime)

qmap(location = "Austin, TX", maptype = "toner", source = "stamen", zoom=13) + 
  
  # ggplot(df,aes(x=x,y=y))+
  # stat_density2d(aes(x=x,y=y, alpha=..level..), geom="polygon") +
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
  # geom_point(colour="red",alpha=0.02)+
# qmap(location = "78705", maptype = "toner", source = "stamen") + 
  stat_density2d(aes(x = LONGITUDE, y = LATITUDE, colour = Crime, alpha=..level..), data = APD, alpha = .3) +# facet_grid(. ~ month(Date)) + 
  guides(color=FALSE)




APD.Driving = filter(APD, grepl("CRASH",Crime) | grepl("DWI",Crime) | grepl("DUI",Crime)) 
APD.Felony  = filter(APD, grepl("AGG",Crime) | grepl("RAPE",Crime) | grepl("FAM/DATE",Crime)) 
APD.SexDrug = filter(APD, grepl("PROSTITUTION",Crime) | grepl("ESCORT",Crime) | grepl("POSS",Crime)) 
APD.Guns    = filter(APD, grepl("FIREARMS",Crime) | grepl("SHOTS FIRED",Crime)) 
APD.Cars    = filter(APD, grepl("PARKING VIOLATION",Crime) | grepl("ABANDONED",Crime)) 
APD.Steal   = filter(APD, grepl("THEFT",Crime) | grepl("ROBBERY",Crime)) 
APD.df      = filter(APD, grepl("CRIMINAL",Crime) | grepl("SUICIDE",Crime)) 
  
  nrow()  / nrow(APD)




# 33 I
# 88 N
# 50 F
# 11 J
austin_map <- qmap(location = "Austin, TX", maptype = "toner", source = "stamen", zoom=13) 
austin_map + stat_density2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), 
                              size = 2, bins = 4, geom = "polygon",
                            data = APD.Steal) + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data=APD.Steal, alpha=I(0.5)) + 
  scale_fill_continuous(low="yellow", high = "red") + facet_wrap("Crime")
