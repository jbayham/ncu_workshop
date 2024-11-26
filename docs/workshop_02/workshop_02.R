#This script will process landcover data in Japan prefectures

library(pacman)
p_load(tidyverse,sf,raster,terra,exactextractr,tmap)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_02/")
#######################

# Read in land cover raster data
jp_lc <- terra::rast("inputs/JAXA_HRLULC_Japan_v23.12_250m.tif")

# Define a point with latitude and longitude
ncu_point <- data.frame(lat = 35.138496, lon = 136.925901) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) # Convert to spatial object

# Create a 50 km buffer around the point
ncu_buffered <- ncu_point %>%
  st_buffer(dist = 50000) # Distance is in meters for CRS 4326


# Crop raster layer to the buffered area
jp_lc_cropped <- terra::crop(jp_lc, ncu_buffered)

# Create a map visualization
ncu_map_rast <- tm_basemap("CartoDB.Positron") +  # Add a basemap
  tm_basemap("Esri.WorldImagery") +              # Add satellite imagery
  tm_shape(ncu_point) +                          # Add the point
  tm_symbols(col = "red", size = 0.001) +        # Style the point
  tm_shape(ncu_buffered) +                       # Add the buffer
  tm_borders(col = "darkred") +                  # Style the buffer
  tm_shape(jp_lc_cropped) +                      # Add the cropped raster
  tm_raster()                                    # Display the raster

# Render the map
tmap_leaflet(ncu_map_rast)

# Define categories and colors
categories <- c("Water bodies", "Built-up", "Paddy field", "Cropland", "Grassland",
                "Deciduous Broad-leaved Forest (DBF)", "Deciduous Needle-leaved Forest (DNF)",
                "Evergreen Broad-leaved Forest (EBF)", "Evergreen Needle-leaved Forest (ENF)", 
                "Bare", "Bamboo forest", "Solar panel", "Wetland", "Greenhouse")

colors <- c("#76C1FF", "#D3D3D3", "#FFD966", "#FFA07A", "#98FB98",
            "#228B22", "#556B2F", "#006400", "#2E8B57", "#F5F5DC",
            "#8B4513", "#D01A11", "#4682B4", "#ADFF2F")

# Assign categories to the raster
cat_ref <- data.frame(ID = 1:length(categories), label = categories)

levels(jp_lc_cropped) <- cat_ref

# Update map with color-coded raster
ncu_map_rast <- tm_basemap("CartoDB.Positron") +  # Add basemap
  tm_basemap("Esri.WorldImagery") +
  tm_shape(ncu_point) + 
  tm_symbols(col = "red", size = 0.001) +
  tm_shape(ncu_buffered) +
  tm_borders(col = "darkred") +
  tm_shape(jp_lc_cropped) +
  tm_raster(palette = colors, alpha = 0.5, style = "cat")

# Render the updated map
tmap_leaflet(ncu_map_rast)

