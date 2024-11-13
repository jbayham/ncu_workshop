

library(pacman)
p_load(tidyverse,raster)

jp_lc <- raster("~/Downloads/JAXA_HRLULC_Japan_v23.12_250m.tif")

mapview::mapview(jp_lc)

e <- extent(jp_lc)
e@xmin=136.914
e@xmax=136.93696
e@ymin=35.129
e@ymax=35.14763

mapview::mapview(crop(jp_lc,e))

#############################
#Define a point in a dataframe - lat is like the y coord and lon is like the x
ncu <- data.frame(lat=35.138496, lon=136.925901)

#Convert the dataframe into a spatial object using st_as_sf() - it is important that lon is first (x,y)
ncu_geo <- st_as_sf(ncu,coords = c("lon","lat"), crs=4326) 
print (ncu_geo)


ncu_map <- tm_basemap("CartoDB.Positron") + #start with a basemap
  tm_shape(ncu_geo) + 
  tm_symbols(col="red")

tmap_leaflet(ncu_map)

#Buffer the point creating a circle with 1km radius around the point
ncu_buffered <- ncu_geo %>%
  st_buffer(dist=50000) #the buffer radius is always in the units of the projection - meters in this case


#Plot the point and buffer
ncu_map <- tm_basemap("CartoDB.Positron") + #start with a basemap
  tm_shape(ncu_geo) + 
  tm_symbols(col="red",size = .001)  +
  tm_shape(ncu_buffered) +
  tm_borders(col="darkred")

tmap_leaflet(ncu_map)

e <- extent(ncu_buffered)
jp_lc_crop <- crop(jp_lc,e)

#https://www.eorc.jaxa.jp/ALOS/en/dataset/lulc/lulc_v2312_e.htm
# Define the categories and their respective colors
categories <- c("Water bodies", "Built-up", "Paddy field", "Cropland", "Grassland",
                "Deciduous Broad-leaved Forest (DBF)", "Deciduous Needle-leaved Forest (DNF)",
                "Evergreen Broad-leaved Forest (EBF)", "Evergreen Needle-leaved Forest (ENF)", 
                "Bare", "Bamboo forest", "Solar panel", "Wetland", "Greenhouse")

# Define a color palette for each category
colors <- c(
  "#76C1FF",  # Water bodies (light blue)
  "#D3D3D3",  # Built-up (gray)
  "#FFD966",  # Paddy field (golden yellow)
  "#FFA07A",  # Cropland (salmon)
  "#98FB98",  # Grassland (pale green)
  "#228B22",  # Deciduous Broad-leaved Forest (DBF) (forest green)
  "#556B2F",  # Deciduous Needle-leaved Forest (DNF) (dark olive green)
  "#006400",  # Evergreen Broad-leaved Forest (EBF) (dark green)
  "#2E8B57",  # Evergreen Needle-leaved Forest (ENF) (sea green)
  "#F5F5DC",  # Bare (beige)
  "#8B4513",  # Bamboo forest (saddle brown)
  "#FFD700",  # Solar panel (bright yellow)
  "#4682B4",  # Wetland (steel blue)
  "#ADFF2F"   # Greenhouse (yellow-green)
)

# Set the raster categories
levels(jp_lc_crop) <- data.frame(ID = 1:length(categories), label = categories)

ncu_map_rast <- ncu_map +
  tm_shape(jp_lc_crop) +
  tm_raster(palette = colors,alpha = .5,style = "cat" )

tmap_leaflet(ncu_map_rast)


