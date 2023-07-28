# Load required libraries
library(lubridate) # For date and time manipulation
library(rgeeExtra) # Extended functionality for Google Earth Engine
library(dplyr) # Data manipulation and transformation
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling



#' Check if a dataframe is empty.
#'
#' This function checks whether the provided dataframe is empty, i.e., it does not contain any rows.
#'
#' @param df A dataframe. It should be an object of class 'data.frame'.
#'
#' @return A logical value. TRUE if the dataframe is empty (contains no rows), and FALSE otherwise.
#'
#' @examples
#' df_empty <- data.frame()
#' df_non_empty <- data.frame(a = 1:3, b = c('A', 'B', 'C'))
#' check_01(df_empty)
#' # [1] TRUE
#' check_01(df_non_empty)
#' # [1] FALSE
#'
#' @seealso \code{\link{nrow}}
#'
#' @keywords dataframe manipulation check empty
#' @export
check_01 <- function(df) {
  if (nrow(df) == 0) {
    TRUE
  } else {
    FALSE
  }
}



#' Get the projection of a specified band from an image.
#'
#' This function takes an image object and retrieves the projection information for a specified band.
#'
#' @param image An image object representing raster data.
#'   This should be a valid image object that supports the `select()` and `projection()` methods.
#'
#' @return The projection information for the specified band as returned by the `projection()` function.
#'
#' @examples
#' # Assuming 'image' is a valid image object
#' proj_info <- ImageProj(image)
#' print(proj_info)
#'
#' @seealso \code{\link{select}}, \code{\link{projection}}
#'
#' @keywords image processing projection band
#' @export
ImageProj <- function(image) {
  image$select("B2")$projection()
}



#' Get the coordinates of a point feature in a specified projection.
#'
#' This function takes a point feature object and converts its coordinates to the specified projection.
#'
#' @param point An Earth Engine point feature object.
#'   This should be a valid point feature object representing a geographic point.
#'   It must be compatible with the Earth Engine API and support the `transform()`, `geometry()`, and `coordinates()` methods.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `crs()` method.
#'
#' @return The coordinates of the point feature in the specified projection.
#'
#' @examples
#' # Assuming 'point' and 'proj' are valid objects
#' coords <- PointCoordinates(point, proj)
#' print(coords)
#'
#' @seealso \code{\link{ee$Feature}}, \code{\link{transform}}, \code{\link{geometry}}, \code{\link{coordinates}}
#'
#' @keywords point coordinates projection Earth Engine
#' @export
PointCoordinates <- function(point, proj) {
  Coords <- ee$Feature(point)$
    transform(proj$crs())$
    geometry()$
    coordinates()
}



#' Initialize coordinate values for image axis.
#'
#' This function initializes coordinate values for the specified image axis based on the provided projection.
#'
#' @param axis A character indicating the axis to initialize. Should be either "x" or "y".
#' 
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `transform()`, `split()`, and `get()` methods.
#'
#' @return A numeric value representing the coordinate value for the specified axis.
#'
#' @examples
#' # Assuming 'axis' is either "x" or "y", and 'proj' is a valid projection object
#' coord_value <- InitCoordImage(axis, proj)
#' print(coord_value)
#'
#' @export
InitCoordImage <- function(axis, proj) {
  numero <- NULL
  separador <- NULL
  
  if (axis == "x") {
    numero <- 4
    separador <- "]"
  } else if (axis == "y") {
    numero <- 6
    separador <- "]]"
  } else {
    stop("Axis must be 'x' or 'y'")
  }
  
  ee$Number$parse(
    ee$String(
      ee$String(
        proj$
          transform()$
          split(", \n  ")$
          get(numero))$
        split(",")$
        get(1))$
      split(separador)$
      get(0))
}



#' Transfer coordinates based on initial coordinate and constant.
#'
#' This function transfers the given coordinates based on an initial coordinate and a constant factor.
#'
#' @param coord A numeric vector representing the coordinates to transfer.
#'
#' @param coord_ini A numeric vector representing the initial coordinates.
#' 
#' @param const A numeric constant factor to apply during the coordinate transfer.
#'
#' @return A numeric vector containing the transferred coordinates.
#'
#' @export
CoordTransfer <- function(coord, coord_ini, scale) {
  coord_ini$
    add(coord$
          add(coord_ini$multiply(-1))$
          multiply(1 / scale)$
          round()$
          multiply(scale))$
    add(scale / 2)
}



#' Create an Earth Engine point geometry.
#'
#' This function creates an Earth Engine point geometry with the given x and y coordinates.
#'
#' @param x The x-coordinate of the point.
#'
#' @param y The y-coordinate of the point.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'
#' @return An Earth Engine point geometry object.
#'
#' @examples
#' # Assuming 'x' and 'y' are valid numeric coordinates, and 'proj' is a valid projection object
#' point_geometry <- CreatePoint(x, y, proj)
#' print(point_geometry)
#'
#' @export
CreatePoint <- function(x, y, proj) {
  ee$Geometry$Point(list(x, y), proj$crs())
}



#' Create a region of interest (ROI) around a point.
#'
#' This function creates a region of interest (ROI) around the specified point with a given side length.
#'
#' @param point An Earth Engine point geometry representing the center of the ROI.
#'
#' @param side The side length of the ROI.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'
#' @return An Earth Engine geometry object representing the region of interest.
#'
#' @examples
#' # Assuming 'point' is a valid Earth Engine point geometry, 'side' is a valid numeric side length, and 'proj' is a valid projection object
#' roi_geometry <- CreateROI(point, side, proj)
#' print(roi_geometry)
#'
#' @export
CreateROI <- function(point, side, proj) {
  point$buffer(
    distance = side / 2,
    proj = proj$crs())$
    bounds(proj = proj$crs())
}



#' Generate a study region (ROI) around a point for tile-based analysis.
#'
#' This function generates a study region (ROI) around the specified point for tile-based analysis.
#'
#' @param point An Earth Engine point geometry representing the center of the study region.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `transform()`, `split()`, and `get()` methods.
#'
#' @param scale A numeric value representing the scale at which to analyze the region.
#'   It is used in the `CoordTransfer` function to adjust the coordinates based on the scale.
#'
#' @param side The side length of the study region.
#'
#' @return An Earth Engine geometry object representing the study region (ROI).
#'
#' @seealso \code{\link{PointCoordinates}}, \code{\link{InitCoordImage}}, \code{\link{CoordTransfer}}, \code{\link{CreatePoint}}, \code{\link{CreateROI}}
#'
#' @export
TileStudy <- function(point, proj, scale, side) {
  Coords <- PointCoordinates(point, proj)
  x <- ee$Number(Coords$get(0))
  y <- ee$Number(Coords$get(1))
  x_ini <- InitCoordImage("x", proj)
  y_ini <- InitCoordImage("y", proj)
  new_x <- CoordTransfer(x, x_ini, scale)
  new_y <- CoordTransfer(y, y_ini, scale)
  ee_new_point <- CreatePoint(new_x, new_y, proj)
  CreateROI(ee_new_point, side, proj)
}



#' Generate a quality mask for cloud-free pixels.
#'
#' This function generates a quality mask for cloud-free pixels based on the 'QA_PIXEL' band in the image.
#'
#' @param image An Earth Engine image object.
#'   This should be a valid image object with the 'QA_PIXEL' band to generate the quality mask.
#'
#' @return An Earth Engine image object representing the quality mask for cloud-free pixels.
#'
#' @export
QualityMask <- function(image) { 
  image$select('QA_PIXEL')$
    bitwiseAnd(strtoi('11000', base = 2))$
    eq(0)
}



#' Count the number of cloud-free pixels in an image within a region of interest (ROI).
#'
#' This function counts the number of cloud-free pixels in an image within a specified region of interest (ROI).
#'
#' @param image An Earth Engine image object.
#'   This should be a valid image object containing the pixels to count within the ROI.
#'
#' @param roi An Earth Engine geometry object representing the region of interest (ROI).
#'
#' @param scale A numeric value representing the scale at which to perform the reduction operation.
#'
#' @return A numeric value representing the count of cloud-free pixels within the ROI.
#'
#' @export
CloudFreeCount <- function(image, roi, scale) {
  image$reduceRegion(
    reducer = ee$Reducer$sum(),
    geometry = roi,
    scale = scale
  )
}



#' Calculate cloud percentage based on the number of cloud-free pixels.
#'
#' This function calculates the cloud percentage based on the number of cloud-free pixels
#' within a given number of pixels and the scale of analysis.
#'
#' @param pixels A numeric value representing the count of cloud-free pixels.
#'
#' @param scale A numeric value representing the scale of analysis.
#'
#' @param side The side length of the study region.
#'
#' @return A numeric value representing the cloud percentage.
#'
#' @export
CloudPerCal <- function(pixels, scale, side) {
  ee$Number(1)$
    add(pixels$
          getNumber("QA_PIXEL")$
          multiply(1 / ((side / scale)** 2))$
          multiply(-1))$
    multiply(100)
}



#' Calculate cloud percentage for a tile around a given point.
#'
#' This function calculates the cloud percentage for a tile around the specified point based on the given scale and side length.
#'
#' @param point An Earth Engine point geometry representing the center of the tile.
#'
#' @param scale A numeric value representing the scale of analysis.
#'
#' @param side The side length of the study region.
#'
#' @return A function that calculates the cloud percentage for the tile when applied to an Earth Engine image.
#'
#' @seealso \code{\link{ImageProj}}, \code{\link{TileStudy}}, \code{\link{QualityMask}}, \code{\link{CloudFreeCount}}, \code{\link{CloudPerCal}}
#'
#' @export
CloudTile <- function(point, scale, side){
  ImgCloudTile <- function(image) {
    projection <- ImageProj(image)
    ee_roi <- TileStudy(point, projection, scale, side)
    cloud_mask <- QualityMask(image)
    count <- CloudFreeCount(cloud_mask, ee_roi, scale)
    prop_cloud <- CloudPerCal(count, scale, side)
    image$set(list("cloud_tile" = prop_cloud))
  }
  ImgCloudTile
}



#' Filter image collection by geographic bounds around a point.
#'
#' This function filters an Earth Engine image collection to include only images that intersect the geographic bounds around a specified point.
#'
#' @param snippet An Earth Engine image collection snippet.
#'
#' @param point An Earth Engine point geometry representing the center of the geographic bounds.
#'
#' @return An Earth Engine image collection filtered by geographic bounds around the point.
#'
#' @export
ImagesBounds <- function(snippet, point) {
  ee$ImageCollection(snippet) %>%
    ee$ImageCollection$filterBounds(point)
}



#' Merge image collections and get the date intervals.
#'
#' This function merges a list of Earth Engine image collections and retrieves the date intervals for the resulting merged image collection.
#'
#' @param listing A list of Earth Engine image collections to be merged.
#'
#' @return An Earth Engine image collection representing the merged images with date intervals.
#'
#' @seealso \code{\link{ee_get_date_ic}}
#'
#' @export
IDatesIC <- function(listing) {
  IC <- NA
  for (i in 1:length(listing)) {
    if(i == 1) {
      IC <- listing[[i]]
    } else {
      IC <- IC$merge(listing[[i]])
    }
  }
  IC %>% ee_get_date_ic()
}



#' Filter image dates with a specific pattern in their IDs.
#'
#' This function filters the input image dates to include only those with a specific pattern in their IDs.
#'
#' @param idates An Earth Engine image collection containing image dates with IDs.
#'
#' @param pattern A character pattern to match in the IDs of the image dates.
#'
#' @return An Earth Engine image collection filtered based on the specified pattern in the IDs.
#'
#' @export
SensorPattern <- function(idates, pattern) {
  IDateS <- idates[grepl(pattern, idates$id),]
  return(IDateS)
}



#' Calculate time difference and index of collocation between two sensors.
#'
#' This function calculates the time difference and the index of collocation between two sensors based on their start times.
#'
#' @param sensor1 An Earth Engine image collection with 'time_start' property representing the start times of the first sensor.
#'
#' @param sensor2 An Earth Engine image collection with 'time_start' property representing the start times of the second sensor.
#'
#' @param units The time units to use for the time difference calculation (e.g., "hours", "days").
#'
#' @return A data frame with two columns: 'time' representing the time difference and 'index' representing the index of collocation.
#'
#' @export
TimeBetween <- function(sensor1, sensor2, units) {
  r_collocation <- sapply(
    X = sensor1$time_start,
    FUN = function(x) {
      vresults <- abs(as.numeric(sensor2$time_start - x, units = units))
      c(min(vresults), which.min(vresults))
    }
  )
  return(data.frame(time = r_collocation[1, ], 
                    index = r_collocation[2, ]))
}



#' Get Satellite Metadata
#'
#' Retrieves metadata from different image collections based on specified parameters.
#'
#' @param timediff The time difference in seconds used for image filtering.
#' @param point The spatial point used as the filter bounds.
#' @return A data frame containing the retrieved metadata, including MSS and TM ID, Tier, ROI ID, and time difference.
get_metadata <- function(point, snippets, sensors, units, scale, side, timediff, max_ob) { 
  
  # Generation of dates and id by image
  ee_point <- sf_as_ee(point$geometry)
  listing <- lapply(snippets, ImagesBounds, ee_point)
  all_together_db <- IDatesIC(listing)

  # Filter First Sensor ID
  sensor1 <- SensorPattern(all_together_db, sensors[1])
  
  # Check if there is any image in the collection
  if (check_01(sensor1)) {
    return(NA)
  }
  
  # Filter Second Sensor ID
  sensor2 <- SensorPattern(all_together_db, sensors[2])
  
  # Check if there is any image in the collection
  if (check_01(sensor2)) {
    return(NA)
  }
  
  # Get time interval between images from different sensors
  Tindex <- TimeBetween(sensor1, sensor2, units)
  difft <-  Tindex$time < timediff
  
  # Check if the images are on the time interval 
  if (sum(difft) == 0) {
    return(NA)
  }

  # Filter IDs of each sensor in set time condition
  final_tm <- sensor1[difft, ]
  final_time <- Tindex$time[difft]
  final_mss <- sensor2[Tindex$index[difft], ]
  
  # Characteristics of the first sensor
  Sat_tm <- substr(final_tm$id, 9, 12)
  Mission_tm <- paste0(substr(Sat_tm, 1, 1), substr(Sat_tm, nchar(Sat_tm), nchar(Sat_tm)))
  Tier_tm <- substr(final_tm$id, 18, 19)
  
  # Second Sensor Characteristics
  Sat_mss <- substr(final_mss$id, 9, 12)
  Mission_mss <- paste0(substr(Sat_mss, 1, 1), substr(Sat_mss, nchar(Sat_mss), nchar(Sat_mss)))
  Tier_mss <- substr(final_mss$id, 18, 19)
  
  

  # Generation of metadata table and characteristics
  df <- data.frame(
    mss_id = final_mss$id,
    Mission_mss = Mission_mss,
    Tier_mss = Tier_mss,
    tm_id = final_tm$id,
    Mission_tm = Mission_tm,
    Tier_tm = Tier_tm,
    dif_time = round(final_time, 10),
    roi_id = point$roi_id,
    roi_x = st_coordinates(point)[1],
    roi_y = st_coordinates(point)[2]
  )

  # Filter by maximum number of IDs established per point
  if(max_ob) {
    n <- min(max_ob, nrow(df)) 
    random_indices <- sample(1:nrow(df), n, replace = FALSE)
    df <- df[random_indices, ]
  }

  # Percentage generation of clouds and shadows by tiles
  Imgv <- c()
  for(i in 1:length(df$tm_id)) {
    Imgv <- c(Imgv, ee$Image(df$tm_id[i]))
  }
  Imgs <- ee$ImageCollection(Imgv)
  column_cloud <- Imgs$map(CloudTile(ee_point, scale, side))$aggregate_array("cloud_tile")$getInfo()
  df$cloud_tile <-  unlist(column_cloud)
  
  return(df)
}



#' Convert Digital Number (DN) values to Top-of-Atmosphere (TOA) reflectance for Landsat MSS imagery.
#'
#' This function converts Landsat MSS imagery from Digital Number (DN) values to Top-of-Atmosphere (TOA) reflectance.
#'
#' @param img An Earth Engine image object representing the Landsat MSS imagery.
#'   This should be a valid image object with bands "B1", "B2", "B3", "B4", and "QA_PIXEL".
#'
#' @return An Earth Engine image object with TOA reflectance bands "B1", "B2", "B3", "B4", and "QA_PIXEL".
#'
#' @examples
#' # Assuming 'img' is a valid Earth Engine image object
#' toa_img <- MSS_TOA(img)
#' print(toa_img)
#'
#' @export
MSS_TOA <- function(img) {
  dnImg <- img$select(c("B1", "B2", "B3", "B4"))
  Qa <- img$select(c("QA_PIXEL"))
  gainBands <- ee$List(img$propertyNames())$filter(ee$Filter$stringContains("item", "REFLECTANCE_MULT_BAND"))$sort()
  biasBands <- ee$List(img$propertyNames())$filter(ee$Filter$stringContains("item", "REFLECTANCE_ADD_BAND"))$sort()
  gainImg <- ee$Image$cat(
    ee$Image$constant(img$get(gainBands$get(0))),
    ee$Image$constant(img$get(gainBands$get(1))),
    ee$Image$constant(img$get(gainBands$get(2))),
    ee$Image$constant(img$get(gainBands$get(3)))
  )
  biasImg <- ee$Image$cat(
    ee$Image$constant(img$get(biasBands$get(0))),
    ee$Image$constant(img$get(biasBands$get(1))),
    ee$Image$constant(img$get(biasBands$get(2))),
    ee$Image$constant(img$get(biasBands$get(3)))
  )
  dnImg$multiply(gainImg)$
    add(biasImg)$
    addBands(Qa)$
    rename(c("B1", "B2", "B3", "B4", "QA_PIXEL"))
}



#' Download Landsat image data for a specific location.
#'
#' This function downloads Landsat image data for a specific location defined by a given row.
#'
#' @param row A data frame representing the information about the location.
#'   It should contain the following columns: "roi_x", "roi_y", "roi_id", and the columns defined in the 'options' parameter.
#'
#' @param sensors A character vector containing two sensor names to download data from.
#'
#' @param output A character string representing the output directory where the downloaded data will be stored.
#'
#' @return The function does not return anything; it downloads Landsat image data as GeoTIFF files.
#'
#' @examples
#' # Assuming 'row' is a data frame with location information and 'sensors' is a character vector of sensor names,
#' # and 'output' is a character string representing the output directory
#' download(row, sensors, output)
#'
#' @importFrom sf st_as_sf st_transform st_point st_buffer
#' @importFrom raster sf_as_ee ee_as_rast
#' @importFrom rgdal readOGR
#' @export
download <- function(row, sensors, side, output) {
  # Approach of previous data
  options <- list(LT = c("tm_id", 30), LM = c("mss_id", 60), MSI = c("msi_id", 10), OLI = c("oli_id", 30))
  dir.create(sprintf("%s/%s", output, sensors[1]), 
             showWarnings = FALSE, 
             recursive = TRUE)
  dir.create(sprintf("%s/%s", output, sensors[2]), 
             showWarnings = FALSE, 
             recursive = TRUE)
  scale <- as.numeric(options[[sensors[1]]][2])
  
  # Entering the point and ID of the images
  point <- st_as_sf(x = row, coords = c("roi_x", "roi_y"), crs = 4326)
  img1 <- as.character(row[options[[sensors[1]]][1]])
  img2 <- as.character(row[options[[sensors[2]]][1]])
  
  # Selection of bands per image, scaling and conversion to integer 16
  ImageC1 <- ee$Image(img1)$
    select(c("B2", "B3", "B4"))$ 
    multiply(10000)$
    int16()$
    unmask(-99, sameFootprint = FALSE)
  
  ImageC2 <- MSS_TOA(ee$Image(img2))$ 
    select(c("B1", "B2", "B3", "B4"))$
    multiply(10000)$
    int16()$
    unmask(-99, sameFootprint = FALSE)

  # Obtain proj metadata
  proj_metadata <- ImageC1$select("B2")$
    projection()$
    getInfo()
  
  # Generation of crs and transform
  proj_transform <- proj_metadata$transform
  proj_transform[1] <- scale * 2
  proj_transform[5] <- - scale * 2
  proj_crs <- proj_metadata$crs

  # Move the pixel to align to the geotransform
  geom <- point$geometry
  geom_utm <- st_transform(geom, proj_crs)
  x <- geom_utm[[1]][1]
  y <- geom_utm[[1]][2]
  new_x <- proj_transform[3] + round((x - proj_transform[3]) / scale) * scale + scale / 2
  new_y <- proj_transform[6] + round((y - proj_transform[6]) / scale) * scale + scale / 2
  new_geom_utm <- st_sfc(st_point(c(new_x, new_y)), crs = proj_crs)

  # Make that both images have the same CRS
  proj_transform[3] <- proj_transform[3] - scale / 2
  proj_transform[6] <- proj_transform[6] + scale / 2
  ImageC2_crs <- ImageC2$reproject(proj_crs, proj_transform) 
  
  # Create ROI
  roi <- new_geom_utm %>%
    st_buffer(side / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)
  
  # Download
  output_file1 <- sprintf("%s/%s/%s__%s.tif", output, sensors[1], row$roi_id, basename(img1))
  if (!file.exists(output_file1)) {
    lr_image <- ee_as_rast(
      image = ImageC1,
      region = ee_roi,
      scale = scale,
      dsn = output_file1
    )
  }

  output_file2 <- sprintf("%s/%s/%s__%s.tif", output, sensors[2], row$roi_id, basename(img2))
  if (!file.exists(output_file2)) {
    hr_image <- ee_as_rast(
      image = ImageC2_crs,
      region = ee_roi,
      scale = scale,
      dsn = output_file2
    )
  }
}




#' Display Transformed Images
#'
#' This function retrieves and displays transformed images based on specified parameters.
#'
#' @param row A row containing metadata for MSS and TM images.
#' @param max The maximum value for image display (default = 0.3).
#' @param factor The factor used for scaling the max value (default = 2).
#' @return None (displays images on the map).
DisplayTransform <- function(row, mode, max = 0.3, factor = 2) {
  img_tm <-  ee$Image(row$tm_id)
  proj_metadata <- img_tm$select("B2")$
    projection()$
    getInfo()
  proj_transform <- proj_metadata$transform
  proj_transform[1] <- 60
  proj_transform[5] <- -60
  proj_crs <- proj_metadata$crs
  point <- st_as_sf(x = row, coords = c("roi_x", "roi_y"), crs = 4326)
  geom_utm <- st_transform(point$geometry, proj_crs)
  ee_point <- sf_as_ee(geom_utm)
  x <- geom_utm[[1]][1]
  y <- geom_utm[[1]][2]
  new_x <- proj_transform[3] + round((x - proj_transform[3]) / 30) * 30 + 30 / 2
  new_y <- proj_transform[6] + round((y - proj_transform[6]) / 30) * 30 + 30 / 2
  new_geom_utm <- st_sfc(st_point(c(new_x, new_y)), crs = proj_crs)
  ee_point_transform <- sf_as_ee(new_geom_utm)
  roi <- new_geom_utm %>% st_buffer(30720 / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)
  eeimg1 <- img_tm$
    clip(ee_roi)
  proj_transform[3] <- proj_transform[3] - 15
  proj_transform[6] <- proj_transform[6] + 15
  eeimg2 <- MSS_TOA(ee$Image(row$mss_id))$
    reproject(proj_crs, proj_transform)$
    clip(ee_roi)
  eel2 <- list(min = 0, max = max/factor, bands = c("B3", "B2", "B1"))
  eel1 <- list(min = 0, max = max, bands = c("B4", "B3", "B2"))
  print(sprintf("Diferencia de %s segundos entre MSS: %s y TM: %s", row$dif_time, row$mss_id, row$tm_id))
  Map$centerObject(eeimg1)
  if (mode == "points") {
    Map$addLayer(eeimg1, eel1, basename(row$tm_id)) +
      Map$addLayer(eeimg2, eel2, basename(row$mss_id)) +
      Map$addLayer(ee_point, list(color = "#E4FF00"), "Target point (yellow)") +
      Map$addLayer(ee_point_transform, list(color = "#00FFF7"), "Transformed point (cyan)")
  } else if (mode == "comparison") {
    Map$addLayer(eeimg1, eel1, basename(row$tm_id)) | Map$addLayer(eeimg2, eel2, basename(row$mss_id))
  } else {
    stop("Modo no válido. Debe ser 'points' o 'comparison'.")
  }
}



#' Display clipped Multispectral Scanner (MSS) and Thematic Mapper (TM) satellite imagery tiles with adjustable contrast.
#'
#' This function displays the clipped MSS and TM satellite imagery tiles corresponding to the given row data. It converts
#' the Digital Numbers (DN) of the MSS tile to top-of-atmosphere (TOA) reflectance using the `MSS_TOA` function. The function
#' then clips the images to the specified region of interest (ROI) before displaying them on the map with adjustable contrast
#' for better visualization.
#'
#' @param row A data frame row containing information about the tiles to be displayed.
#' @param max The maximum value for contrast adjustment. Defaults to 0.3.
#' @param factor A factor used to adjust the contrast of the MSS tile. Defaults to 2.
#' @return None (displays images on the map).
DisplayMask <- function(row, sensor, max = 0.3) {
  if(sensor == "tm") {
    img <-  ee$Image(row$tm_id)
    eel <- list(min = 0, max = max, bands = c("B4", "B3", "B2"))
    name <- row$tm_id
    print(sprintf("Esta imagen tiene un %s porciento de nube-sombra y su ID es %s ", row$cloud_tile, row$tm_id))
  } else if(sensor == "mss") {
    img <- MSS_TOA(ee$Image(row$mss_id))
    eel <- list(min = 0, max = max, bands = c("B3", "B2", "B1"))
    name <- row$mss_id
    print(sprintf("Esta imagen tiene un %s porciento de nube-sombra y su ID es %s ", row$cloud_tile, row$mss_id))
  } else {
    img <-  ee$Image(row$tm_id)
    eel <- list(min = 0, max = max, bands = c("B4", "B3", "B2"))
    name <- row$tm_id
    print("By default, the TM image is displayed")
    print(sprintf("Esta imagen tiene un %s porciento de nube-sombra y su ID es %s ", row$cloud_tile, row$tm_id))
  }
  proj_metadata <- img$select("B2")$
    projection()$
    getInfo()
  proj_transform <- proj_metadata$transform
  proj_crs <- proj_metadata$crs
  point <- st_as_sf(x = row, coords = c("roi_x", "roi_y"), crs = 4326)
  geom_utm <- st_transform(point$geometry, proj_crs)
  x <- geom_utm[[1]][1]
  y <- geom_utm[[1]][2]
  new_x <- proj_transform[3] + round((x - proj_transform[3]) / 60) * 60 
  new_y <- proj_transform[6] + round((y - proj_transform[6]) / 60) * 60
  new_geom_utm <- st_sfc(st_point(c(new_x, new_y)), crs = proj_crs)
  roi <- new_geom_utm %>% st_buffer(30720 / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)
  eeimg1 <- img$clip(ee_roi)
  img_mask <- eeimg1$
    select('QA_PIXEL')$
    bitwiseAnd(strtoi('11000', base = 2))$
    eq(0)
  eeimg2 <- eeimg1$updateMask(img_mask)
  Map$centerObject(eeimg1)
  Map$addLayer(eeimg1, eel, name) | Map$addLayer(eeimg2, eel, "Mask")
}



#' Get metadata for a given point and sensor, with retry mechanism on failure.
#'
#' This function retrieves metadata for a specific point and sensor using the `get_metadata` function. If an error occurs
#' during the metadata retrieval, the function will retry up to four additional times (by default) before raising a
#' custom error message indicating possible internet connection issues.
#'
#' @param point A spatial point object representing the location of interest.
#' @param sensor A character string specifying the sensor for which metadata is requested.
#' @param timediff The time difference to consider when fetching the metadata.
#' @param counter An optional integer specifying the retry counter. Defaults to 1.
#' @return The metadata retrieved using the `get_metadata` function, or an error message if retries fail.
get_metadata_try <- function(point, snippets, sensors, units, scale, side, timediff = 20, max_ob = F, counter = 1) {
  results <- try(
    get_metadata(point = point, snippets = snippets, sensors = sensors, units = units, scale = scale, side = side, timediff = timediff, max_ob = max_ob)
    )
  if (inherits(class(results), "try-error")) {
    counter <- counter + 1
    if (counter == 5) stop("Probably internet connection lost")
    get_metadata_try(point = point, 
                     snippets = snippets, 
                     sensors = sensors,
                     units = units, 
                     scale = scale, 
                     side = side,
                     timediff = timediff, 
                     max_ob = max_ob,
                     counter = counter)
  }

  results
}
