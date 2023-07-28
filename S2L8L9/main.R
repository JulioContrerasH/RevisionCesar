# /*
# MIT License
#
# Copyright (c) [2023] [AndesDataCube team]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.




# Load required libraries -------------------------------------------------
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling



# Load the utility functions from 'utils.R' file --------------------------
source("utils_v2.R")



# Initialize EE -----------------------------------------------------------
ee_Initialize(user = "julio.contreras1@unmsm.edu.pe", drive = T)


# Snippets for sensors and level
snippets <- list(
  "COPERNICUS/S2_HARMONIZED",
  "LANDSAT/LC08/C02/T1_TOA",
  "LANDSAT/LC08/C02/T2_TOA",
  "LANDSAT/LC09/C02/T1_TOA",
  "LANDSAT/LC09/C02/T2_TOA"
)



# Load initial dataset ----------------------------------------------------
metadata <- read_sf("Data/s2landsatpairs.geojson")



# Create metadata table, difference 10 seconds ----------------------------
container <- list()
for (index in 1:nrow(metadata)) { # 1000
  
  # Print the index value
  print(index)
  
  # Get the coordinate data for the current row
  coordinate <- metadata[index, ]
  
  # Get metadata for satellite images
  img_metadata <- get_metadata_try(
    point = coordinate,
    sensors = c("COPERNICUS", "LANDSAT"),
    snippets = snippets,
    units = "secs",
    scale = 10, 
    side = 10240,
    timediff = 10
  )
  container[[index]] <- img_metadata
}

id_metadata <- do.call(rbind, container)
id_metadata <- id_metadata[!is.na(id_metadata$mss_id),]
# write.csv("exports/metadata.csv")



# Display -----------------------------------------------------------------
df <- read.csv("exports/metadata.csv")
df <- df[order(df$cloud_tile), ]
row <- df[5000, ]
DisplayTransform(row = row, 
                 mode = "points", # comparison
                 max = 0.7)



# Download satellite images -----------------------------------------------
for (x in 1:nrow(df)) {
  row = df[x, ]
  download(
    row = row,
    sensors = c("MSI", "OLI"),
    side = 10240,
    output = "results"
  )
}



