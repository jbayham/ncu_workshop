#This script reads in exercise activity data and converts it to time stamped coordinates for plotting with kepler.gl

library(pacman)
p_load(tidyverse,gpx,trackeR,measurements)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_03/")
###################################
#Read data using trackeR
dat <- read_container("inputs/Shimanami_Kaido_Day_1.gpx",type = "gpx")

#Display the units of the data read in
attr(dat,"units")

#Convert zoo object to dataframe
df <- as.data.frame(dat) %>%
  mutate(time = with_tz(time,"Japan"), #convert from UTC/GMT to Japan time +9
         speed = conv_unit(speed,"m_per_sec","kph"), #convert from meters per second to kilometers per hour
         distance = conv_unit(distance,"m","km")) #convert from meters to kilometers

#Write out csv for plotting
write_csv(select(df,session,time,latitude,longitude,
                 altitude_m=altitude,
                 speed_kph=speed,
                 heart_rate_bpm=heart_rate),"inputs/sk_ride.csv")
