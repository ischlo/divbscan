# this scripts holdas the main parameters defining a single run:

# City name
city <- 'lille' 

# resolution of the h3 grid
# for very large areas, choose a smaller values, around 9 for computational capacity reasons. 
# for small areas, 10 seems good. 
h3_res <- 9

# isodistance limit

# different tested values:
# 200- not intresting
# 650 - big areas
# 350-600 - seems intresting 

iso_dist <- 450

# concacity of the isodists: the greater, the rounder will be the areas. 
concavity <- .4

# parameter controling how many neighbours minimum does a cell need to qualify for a local max
# from 0 to 6, the bigger, the less likely it is to have isolated hexs assigned to a local max.
grid_param_nn <- 0

# number of nearest neighbours to consider to decide which is the max, 3 or 4 seems good usually
nn_neighbourhood <- 2

# smoothing_dist <- 1200

# source('divbscan_ny.R')

overwrite <- FALSE

######## FILENAMES to which things are saved

network_filename <- paste0('data/networks/',city,'_all.rds')

hex_filename <- paste('data/hexagons',city,h3_res,'.geojson',sep = '_')

amenities_clean_filename <- paste0('data/',city,'_amenities_clean.geojson')

filename_isochrones <- paste0('data/isochrones_',city,'_',h3_res,'_',iso_dist,'.rds')

out_filename <- paste0('data/neighbourhoods_',h3_res,'_',city,'_iso_',iso_dist,'.rds')

web_filename <- paste0('data/web/',city,'_',h3_res,'_',nn_neighbourhood,'_',iso_dist,'.gpkg')

map_file <- paste0('data/leaflet_map_',city,'_',h3_res,'.rds')


#######

filenames <- list(hex_filename
                  ,network_filename
                  ,filename_isochrones
                  ,out_filename
                  ,amenities_clean_filename
                  ,web_filename
                  ,map_file
                  )

if(overwrite) {
  confirmation <- readline(prompt = 'Overwrite is set to TRUE, \n This will erase all files associated to a city. [y/n]: ') |> 
    tolower()
  if (grepl(pattern = '(yes)|(y)',x=confirmation)) { 
    cat('Deleting existing files before running.\n')
    sapply(filenames,\(fn) file.remove(fn)) 
  } else {
    cat('Consider changing the value of the `overwrite` parameter to FALSE.') 
  }
}


