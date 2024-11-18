#This is an introduction to working with spatial data in R

library(pacman) #if you have never used this package, you need to install it: install.packages("pacman)
pacman::p_load(tidyverse,sf,tmap)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_01")

######################
#Read in Japan political boundaries
jp_boundaries <- st_read("inputs/gm-jpn-all_u_2_2/polbnda_jpn.shp")


#Create a quick interactive map
jp_map <- tm_basemap("CartoDB.Positron") + #start with a basemap
  tm_shape(jp_boundaries) +
  tm_polygons(alpha = .1,col="red") #define red and semi-transparent fill color

#Display as interactive leaflet
tmap_leaflet(jp_map)

#############################
#Define a point in a dataframe - lat is like the y coord and lon is like the x
ncu <- data.frame(lat=35.138496, lon=136.925901)

#Convert the dataframe into a spatial object using st_as_sf() - it is important that lon is first (x,y)
ncu_geo <- st_as_sf(ncu,coords = c("lon","lat"), crs=4326) 
print (ncu_geo)

ncu_geo <- ncu_geo %>%
  st_transform(2448)

ncu_map <- tm_basemap("CartoDB.Positron") + #start with a basemap
  tm_shape(ncu_geo) + 
  tm_symbols(col="red")

tmap_leaflet(ncu_map)


#Buffer the point creating a circle with 1km radius around the point
ncu_buffered <- ncu_geo %>%
  st_buffer(dist=1000) #the buffer radius is always in the units of the projection - meters in this case


#Plot the point and buffer
ncu_map <- tm_basemap("CartoDB.Positron") + #start with a basemap
  tm_shape(ncu_geo) + 
  tm_symbols(col="red",size = .001)  +
  tm_shape(ncu_buffered) +
  tm_borders(col="darkred")

tmap_leaflet(ncu_map)
#####################################

library(estatapi)

#Call API Key from environmental variables - usethis::edit_r_environ() to set the environmental variables
api_key=Sys.getenv("STATISTICS_JAPAN_API_KEY")

#Query all tables available
#stat_tab <- estat_getStatsList(appId = api_key, lang = "E", searchWord = "business conditions")

#Get data from table 0004008528. The arguments cdCat** subset the entire table based on some criteria
estat_query <- estat_getStatsData(
  appId = api_key,
  lang = "E",
  statsDataId = "0004008528",
  cdCat01 = "0",
  cdCat02 = "0",
  cdCat03 = "0",
  cdCat04 = "13"
  #cdCat01 = c("002")
)

labor_dat <- estat_query %>%
  select(area_code,desc=`Area classification`,value) #select three columns and rename the second one in the process

##########################
#Joining data
jp_labor <- inner_join(x=jp_boundaries,y=labor_dat,by=c("adm_code"="area_code")) 

class(jp_labor)

# inner_join(labor_dat,jp_boundaries,by=c("area_code"="adm_code")) %>%
#   class()

# Create map using qtm (quick thematic map)
qtm(jp_labor,fill="value")

