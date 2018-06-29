# Functions to make data viewing and processing easier with DHS

# inputs: x - dataframe, ... - variables to summarize

desc_vars <- function(x, ...) {
  
  # the list of variables entered in the function
    varlist <- quos(...) 
  
  x %>% 
    select(!!!varlist) %>% 
    describe()
}


# Function to call when you get following error message
# Error: `x` and `labels` must be same type

labelDataset <- function(data) {
  correctLabel <- function(x) {
    
    if(!is.null(attributes(x)$labels)) {
      class(attributes(x)$labels) <- typeof(x)
    }
    return(x)
  }
  for(i in colnames(data)) {
    data[, i] <- correctLabel(data[, i])
  }
  return(data)
}


# You can supply functions operating on strings:
# http://ggplot2.tidyverse.org/reference/labeller.html
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

# Function for writing captions on graphs
caption_graph <- function(x, y, padding = 20) {
  # x = GeoCenter Date
  # y = Source: DHS 200X
  
  padding <-  str_pad('', padding, 'right')
  caption <-  str_c(x, padding, y, sep = " ")
  return(caption)
}

# Clean up strings
str_clean = function(string, replacement) {
  str_to_lower(str_replace_all(string, c("\\'" = replacement,
                                         "\\-" = replacement,
                                         "\\," = replacement,
                                         '\\"' = replacement,
                                         "\\!"= replacement)))
}

# Convert degrees


degMinSec2decimal = function (deg, min, sec) {
  degrees = as.numeric(deg)
  minutes = as.numeric(min)
  seconds = as.numeric(sec)
  
  if(degrees < 0){
    return(degrees - minutes/60 - seconds/3600)
  } else {
    return(degrees + minutes/60 + seconds/3600)
  }
}

degMin2decimal = function (deg, min, min_thousandths = NA) {
  degrees = as.numeric(deg)
  minutes = as.numeric(min)
  
  if(!is.na(min_thousandths)){
    thousandths = as.numeric(min_thousandths)
    
    minutes = minutes + thousandths/1000
  }
  
  if(degrees < 0){
    return(degrees - minutes/60)
  } else {
    return(degrees + minutes/60)
  }
}

# Look up CRS

lookup_crs  = function(ctry) {
  # convert to all lower case, and use exact regex matching
  sel_ctry = paste0("^", str_clean(ctry, ' '), "$")
  
  # convert lookup table to a long format:
  lookup  = ctry_codes %>%
    gather(code_type, country, -REGION, -capital, -crs) %>%
    mutate(country_clean = str_clean(country, ' '))
  
  # search for a match
  matched = lookup %>%
    filter(str_detect(country_clean, sel_ctry)) %>%
    group_by(crs) %>%
    summarise(country_names = list(unique(country)))
  
  if (nrow(matched) == 1) {
    
    if(length(matched$country_names[[1]]) > 1) {
      print(paste0("Found a match for ", ctry, ", a.k.a. ", paste(setdiff(matched$country_names[[1]], ctry), collapse = ', ')))
    } else {
      print(paste0("Found a match for ", ctry))
    }
    
    return(matched$crs)
    
  } else if (nrow(matched) > 1){
    print("Multiple countries found. Which did you mean?")
    
    
    lapply(1:nrow(matched), function(x) print(paste0("Found a match for ", ctry, ", a.k.a. ",
                                                     paste(setdiff(matched$country_names[[x]], ctry), collapse = ', '))))
  } else {
    print("No matches found.")
    
    inexact = lookup %>%
      filter(str_detect(country_clean, str_clean(ctry, ' '))) %>%
      distinct(country) %>%
      pull(country)
    
    if(length(inexact > 0)) {
      print(paste0("Did you mean ", inexact, "?"))
    }
  }
  
}

# Plot a basic map

plot_map = function(df,
                    fill_var,
                    stroke_size = 0.15,
                    stroke_colour = 'white',
                    file_name = NA,
                    plot_width = 5,
                    plot_height = 5
) {
  p = ggplot(df, aes_string(x = 'long', y = 'lat',
                            group = 'group', order = 'order',
                            fill = fill_var)) +
    geom_polygon() +
    geom_path(size = stroke_size,
              colour = stroke_colour) +
    coord_equal() +
    theme_void() +
    theme(legend.position = 'none')
  
  if(!is.na(file_name)) {
    llamar::save_plot(file_name, width = plot_width, height = plot_height)
  }
  
  return(p)
}



#' Create leaflet map to check spatial join
#' @description after performing a spatial join using spatial_join,
#' creates an interactive leaflet map to check that the polygon/point matching looks correct
#' @import leaflet llamar
#' @export

plot_sp_join = function (df, shp,
                         base_map = providers$Esri.WorldGrayCanvas,
                         radius = 100) {
  
  # define categorical color palette
  colPal = leaflet::colorFactor(palette = rep(category20, 5), levels = 1:nrow(shp))
  
  # create leaflet map
  leaflet() %>%
    addProviderTiles(base_map) %>%
    addPolygons(data = shp, color = ~colPal(obj_id), weight = 1) %>%
    addCircles(data = df, color = ~colPal(obj_id),
               radius = radius, fillOpacity = 1, stroke = FALSE)
}


# -- Function to import shapefiles --
read_shp = function(baseDir = getwd(),
                    folderName = NULL,
                    layerName) {
  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd
  
  # Log the current working directory, to change back at the end.
  currentDir = getwd()
  
  # Change directory to the file folder containing the shape file
  setwd(paste0(baseDir, folderName))
  
  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = rgdal::readOGR(dsn = ".", layer = layerName)
  
  setwd(currentDir)
  
  return(rawShp)
}

# -- Function to import shapefiles and convert to a ggplot-able object --
shp2df = function(baseDir = getwd(),
                  folderName = NULL,
                  layerName,
                  exportData = TRUE,
                  fileName = layerName,
                  getCentroids = TRUE,
                  labelVar = NA,
                  reproject = TRUE, projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
  
  # Check that the layerName doesn't contain any extensions
  # Check that layerName exists within the wd
  
  # Log the current working directory, to change back at the end.
  currentDir = getwd()
  
  # -- READ in the raw shapefile --
  rawShp = read_shp(baseDir = baseDir, folderName = folderName, layerName = layerName)
  
  if (reproject == TRUE) {
    # -- REPRPROJECT the data --
    projectedShp = sp::spTransform(rawShp, sp::CRS(projection))
  } else {
    projectedShp = rawShp
  }
  
  # -- CONVERT to LAT/LON --
  df = sp2df(projectedShp)
  
  # -- FIND CENTROIDS --
  if (getCentroids == TRUE){
    # Pull out the centroids and the associated names.
    centroids = data.frame(coordinates(projectedShp)) %>% rename(long = X1, lat = X2)
    
    if (!is.na(labelVar)) {
      if (labelVar %in% colnames(projectedShp@data)) {
        # Merge the names with the centroids
        centroids = cbind(centroids, projectedShp@data[labelVar]) %>% rename_(label = labelVar)  # rename the column
      } else {
        warning("label variable for the centroids is not in the raw shapefile")
      }
    }
    
    # -- SAVE --
    # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
    if (exportData == TRUE) {
      write.csv(df, paste0(baseDir, "/", fileName, ".csv"))
      write.csv(centroids, paste0(baseDir, "/", fileName, "_centroids.csv"))
    }
    
    
    # -- RETURN --
    # Return the dataframe containing the coordinates and the centroids
    return(list(df = df, centroids = centroids))
  } else {
    # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
    if (exportData == TRUE) {
      write.csv(df, paste0(baseDir, "/", fileName, ".csv"))
    }
    
    # Reset the working directory
    setwd(currentDir)
    
    # Return the dataframe containing the coordinates and the centroids
    return(df)
  }
}

sp2df = function(sp_df){
  # pull out the row names from the data and save it as a new column called 'id'
  sp_df@data$id = rownames(sp_df@data)
  
  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(sp_df, region = "id")
  
  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, sp_df@data, by = "id")
}

spatial_join = function (shp,
                         df,
                         proj_crs,
                         lat_var = 'lat',
                         lon_var = 'lon',
                         df_crs = '+proj=longlat +datum=WGS84 +no_defs',
                         binary_var = 'inPolygon',
                         merge2shp = TRUE,
                         plot_results = TRUE) {
  
  # Part 1. -- Shapefile import --
  # Check if shp is an imported sf object
  if(grepl('\\.shp', shp)) {
    # Read in the shapefile
    shp = st_read(shp)
  } else if('geometry' %in% colnames(shp)) {
    if (!is.list(shp$geometry)){
      stop('unknown shapefile input.  Please input either the name of a shapefile,
           or an sf-imported spatial data frame using sf::st_read')
    }
    } else {
      stop('unknown shapefile input.  Please input either the name of a shapefile,
           or an sf-imported spatial data frame using sf::st_read')
  }
  
  
  
  #' Spatial join a data frame to polygons
  #'
  #' @description Imports a shp file and a data frame loaded into R, and does a spatial
  #' join between the points in the data frame and polygons in the shapefile
  #'
  #' @import sf sp dplyr purrr
  #' @export
  #'
  #' @param shp either a shapefile file location, or a spatial data frame imported using sf::st_read
  #' @param df data frame containing latitudes and longtitudes of the points to spatially join to `shp`. Assumes the data are organized in a data frame containing columns lat_var and lon_var
  #' @param lat_var string specifying column in `df` that contains the latitude coordinates.
  #' @param lon_var string specifying column in `df` that contains the longitude coordinates.
  #' @param proj_crs PROJ4 string specifying the desired projection to use in spatial join. For help choosing projection see http://projectionwizard.org/
  #' @param df_crs PROJ4 string specifying the coordinate reference system for `df`. Assumed to be WGS84 if not specified.
  #' @param binary_var string specifiying the name of a new binary variable created in the output for whether the point is within any polygon in `shp` or not
  #' @param merge2shp TRUE/FALSE if `df` should be merged with the contents of `shp`
  #' @param plot_results TRUE/FALSE if a leaflet map of the join should be plotted
  #'
  #'
  #' @examples
  #' n = 2000
  #' df = data.frame(lon = sample(round(32.67152*100):round(35.91505*100), n, replace = TRUE)/100, lat = sample(round(-17.12721*100):round(-13.34228*100), n, replace = TRUE)/100)
  
  
  shp = shp %>%
    # create an object id to later merge the joined points to
    mutate(obj_id = 1:nrow(shp)) %>%
    # transform to the intended projection
    st_transform(proj_crs)
  
  
  # Part 2. -- Define data frame as a spatial object --
  if(!lat_var %in% colnames(df) | !lon_var %in% colnames(df)) {
    error('Latitutde and/or longitude variables are not found in `df`. Do you need to change `lat_var` or `lon_var`?')
  }
  
  # using sp package, define the variables that contain lat/long to convert to spatial object
  coordinates(df) = as.formula(paste0('~', lon_var, '+', lat_var))
  # convert from sp spatial obj to sf spatial obj
  df = st_as_sf(df)
  # define the projection for the coordinates.
  st_crs(df) = df_crs
  # re-project to common projection system
  df = st_transform(df, proj_crs)
  
  # Part 3. -- Do the spatial join to id which polygons are associated with which points. --
  isect = st_intersects(df, shp)
  
  # first replace all points which didn't join to a polygon as NA;
  # rest are associated with the obj_id (row number) of the polygon they match.
  # then bind the obj_id to the existing data frame
  df = bind_cols(df, data.frame(obj_id = unlist(lapply(isect,
                                                       function(x) {
                                                         ifelse(is_empty(x), NA, x)})))) %>%
    # create a binary variable, which is whether the point is located in the polygon
    mutate_(.dots = setNames('!is.na(obj_id)', binary_var))
  
  # Part 4. -- Merge the df to the shapefile --
  # ignore the actual coordinates of the polygon
  if(merge2shp == TRUE) {
    df = df %>%
      left_join(shp %>% as_data_frame() %>% select(-geometry), by = 'obj_id')
  }
  
  # run leaflet function
  if(plot_results == TRUE){
    print(plot_sp_join(df, shp))
  }
  
  return(df)
  
    }
