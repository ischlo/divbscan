# divbscan new york

library(sf)
library(Btoolkit)
library(data.table)
library(osmdata)
library(cppRouting)
library(parallel)
library(tidyverse)
library(foreach)
library(h3jsr)
library(h3)
library(dbscan)
library(leaflet)
library(reticulate)

####

city <- 'paris'

source('divbscan_functions.R')
source('network_import.R')

source('sf_net_setup.R')
source('amenities_key_val.R')

#### Set up the city

# bbox from osmdata, it's quite big
# ny_bb <- osmdata::getbb(city)
# ny_bb_sf <- ny_bb |> Btoolkit::make_poly()

# c(-74.0623,40.5696,-73.7972,40.8767) my own bbox
# paris small: c(2.2433,48.8109,2.4218,48.9058)
# paris medium: c(2.1880,48.7578,2.5162,48.9523)


#  add check for null here
bbox <- py$bbox_of_interest[[city]]
                       
bbox <- c(bbox$west,bbox$south,bbox$east,bbox$north)

ny_bb <- bbox |> matrix(ncol = 2,byrow = FALSE)
ny_bb_sf <- Btoolkit::make_poly(ny_bb)

tmap::tmap_mode('view')
ny_bb_sf |> tmap::qtm(fill.alpha=.6)

h3_res <- 9

###
## the set of amenities that was used for london
# unique_amenities <- rlist::list.load('/Users/ivannschlosser/Documents/deconstructing_neighbourhoods/unique_amens.rds')

# 
# # amen <- getOSMdata(bb=ny_bb,k='amenity')
amenities_filename <- paste0('data/',city,'_amenities.rds')

if(!file.exists(amenities_filename)){
  print('Loading amenities from OSM')
  amenities <- parallel::mcmapply(mc.cores=3
                                  ,SIMPLIFY = FALSE
                                  ,keys,val_of_interest, FUN = \(key,v) getOSMdata(bb=ny_bb,k=key,val = v))
  
  amenities |> rlist::list.save(amenities_filename)
} else {
  print('Reading in existing file')
  amenities <- rlist::list.load(amenities_filename)
}

##### cleaning amenities

amenities_clean <- parallel::mcmapply(amenities,keys,mc.cores = 3,SIMPLIFY = FALSE,FUN = \(x,key) osm_group_points(x,k=key))

amenities_to_remove <- c("bench","parking_entrance","motorcycle_parking"
                         ,"parking_space","parking","bicycle_parking"
                         ,"disused:restaurant","disused:pub","disused"
                         ,"dog_parking","yes")

amenities <- foreach::foreach(data = amenities_clean
                              ,.combine = dplyr::bind_rows
                              ,.final = sf::st_as_sf) %do% { 
                                data |> 
                                  filter(!duplicated(osm_id)) |> 
                                  filter(!(amenity %in% amenities_to_remove)) |> 
                                  drop_na(amenity)
                              }

amenities <- amenities |> 
  # filter(amenity %in% unique_amenities) |> # to only use the set of amenities from london
  filter(as.logical(sf::st_intersects(geometry,ny_bb_sf,sparse=FALSE)))

amenities_clean_filename <- paste0('data/',city,'_amenities_clean.geojson')

if(!file.exists(amenities_clean_filename)) {
  print('Saving amenities locally')
  amenities |> sf::st_write(amenities_clean_filename,delete_dsn = TRUE,delete_layer = TRUE)
} else if (!exists('amenities')){
  print('reading in clean amenities')
  amenities <- sf::st_read(amenities_clean_filename)
} else print('amenity file and object exist')

#####

amenity_cat <- amenities |> pull(amenity) |> unique()

amenity_cat |> length()

####

s_max <- log(amenity_cat |> length())

#### road network setup

if(!file.exists(paste0('data/networks/',city,'_all.rds'))){
  source('sf_net_setup.R')
} else if (!exists('sf_all')) {
  print('loading network')
  network_ <- rlist::list.load(paste0('data/networks/',city,'_all.rds'))
  sf_all <- network_$graph
  sf_all_ch <- network_$contracted
}

#### setting up the H3 grid

# https://crazycapivara.github.io/h3-r/articles/h3.html

#alternatively, for faster, but less accurate results use: amenities
h3_index <- h3::geo_to_h3(sf_all$coords |> sf::st_as_sf(coords=c('x','y'),crs=4326) #amenities
                          ,res = h3_res) |> unique()

tbl <- table(h3_index)  |>
  tibble::as_tibble()

hexagons_ <- h3_to_geo_boundary_sf(tbl$h3_index)

# hexagons_ <- h3::polyfill(ny_bb_sf,res = h3_res) |> h3::h3_to_geo_boundary_sf()

area <- hexagons_[1,'geometry'] |> sf::st_area() |> units::drop_units()

d <- 2*sqrt(2*area/(3*sqrt(3)))
d

nrow(hexagons_)

head(hexagons_)

# leaflet::leaflet(hexagons) |> 
#   leaflet::addTiles() |> 
#   leaflet::addPolygons(data=hexagons
#                         ,popup = h3_index
#                         ,color = 'red'
#                         ,opacity=1
#                         ,fillColor = 'darkblue'
#                         ,fillOpacity = .4
#                         ,weight=1)

hexagons <- hexagons_ |> mutate(centroid=sf::st_centroid(geometry)) |> 
  as.data.table()

#### computing isochrones. 

# distributing other cores:

# ind <- sf::st_intersects(hexagons$geometry
#                          ,nodes |> sf::st_as_sf(coords=c(2,3),crs=4326)
#                          )
# n_cores <- 6
# k <- trunc(nrow(hexagons)/n_cores)
# doParallel::registerDoParallel(cores = n_cores)
# res1 <- foreach::foreach(i = 1:n_cores
#           ,.combine = c
#   ) %dopar% { #
#     network$find_nearest_node_on_graph(sf_all,points_data = hexagons[((i-1)*k+1):(i*k),'centroid'])
# }
# 
# doParallel::stopImplicitCluster()
# 
# if(k*n_cores != nrow(hexagons)) {
#   res2 <- network$find_nearest_node_on_graph(sf_all,points_data = hexagons[(k*n_cores+1):nrow(hexagons),'centroid'])
# } else {res2 <- NULL}
# 
# hexagons$node <- append(res1,res2)

source('db_.R')

hexagons <- merge(hexagons,nn_search,by='h3_index',all = TRUE)

hex_filename <- paste('data/hexagons',city,h3_res,'.geojson',sep = '_')

if(!file.exists(hex_filename)){
  print('Saving hexagons locally')
  hexagons |> mutate(centroid=sf::st_as_text(centroid)) |> sf::st_write(hex_filename,delete_dsn=TRUE,delete_layer=TRUE)
} else if(!exists('hexagons')){
  print('reading hexagons from local file')
  hexagons <- sf::st_read(hex_filename)
}

#####

# different tested values:
# 200- not intresting
# 650 - big areas
# 350-600 - seems intresting 

iso_dist <- 500

isodist_nodes <- cppRouting::get_isochrone(sf_all,from = hexagons$node,lim = iso_dist)

isodist_nodes |> sapply(FUN = length) |> summary()

to_text_poly <- \(x) {
  x |> 
    apply(MARGIN = 1,FUN = \(x) paste0(x,collapse = ' ')) |> 
    sapply(FUN=\(y) paste0('(',y,')',collapse = ' ')) |> 
    paste(collapse = ', ') |> 
    sapply(FUN = \(x) paste0("'MULTIPOINT (",x,")'"),USE.NAMES = FALSE)
}

multipoints <- parallel::mclapply(isodist_nodes
                                  ,mc.cores = 6
                                  ,FUN=\(ns) {
                                    sf_all$coords[match(ns,osmid),.(x,y)] |> to_text_poly()
                                  })

iso_points <- data.frame('ind'=1:length(multipoints),'geometry'=unlist(multipoints))

isochrone_poly <- parallel::mclapply(multipoints
                                     ,mc.cores = 1
                                     ,\(x){
                                       DBI::dbGetQuery(conn=conn
                                                       ,statement = paste0('SELECT ST_AsText(ST_ConcaveHull(ST_GeomFromText('
                                                                           ,x
                                                                           ,',4326),0.3,false));'))
                                     })

# # old method that did not use the database, MUCH slower
# isochrone_poly <- parallel::mclapply(isodist_nodes
#                                      ,mc.cores = 6
#                                      ,FUN=\(ns) {
#                                        sf_all$coords[match(ns,osmid),list('x'=x,'y'=y,'group'=1)] |>
#                                                 sf::st_as_sf(coords=c(1,2),crs=4326) |>
#                                                 sf::st_combine() |>
#                                                 sf::st_concave_hull(ratio = 0.4) |>
#                                                 sf::st_geometry()
#                                      })

isochrone_poly <- isochrone_poly |> 
  sapply(FUN = \(x) x[[1]],USE.NAMES = FALSE)

isochrones <- data.frame('x'=isochrone_poly |> 
                           unlist(recursive = FALSE)) |> 
  sf::st_as_sf(wkt=1,crs=4326)

####

isochrones[(sf::st_geometry_type(isochrones$x) %in% c('POINT','LINESTRING')),] <- 
  isochrones[(sf::st_geometry_type(isochrones$x) %in% c('POINT','LINESTRING')),] |> 
  sf::st_buffer(dist = 50)

isochrones <- isochrones |> sf::st_make_valid()

leaflet::leaflet(isochrones |> sf::st_as_sf() |> samp_dt(.01)
                 ) |> 
  leaflet::addTiles() |> 
  leaflet::addPolygons(fillColor = 'dimgray'
                       ,fillOpacity = .6
                       ,weight = 1)

isochrones |> sf::st_area() |> sqrt() |> hist(breaks = 100)
isochrones |> sf::st_area() |> sqrt() |> mean()

# isochrone_poly <- isochrone_poly |> sf::st_as_sf() |> mutate(id=1:length(geometry))
filename_isochrones <- paste0('data/isochrones_',city,'_',h3_res,'.rds')

if(!file.exists(filename_isochrones)){
  message('Saving isochrones locally')
  rlist::list.save(isochrones,filename_isochrones)
} else if(!exists('isochrones')){
  message('Reading local file')
  isochrones <- rlist::list.load(filename_isochrones)
} else print('file exists everywhere')

####

diversity <- neighbourhoods$entropy(amenities
                                    ,iso = isochrones
                                    ,cor_num = 6)

if(!(nrow(diversity)==nrow(hexagons))) { print('Missmatch between hexs and diversity score')}

sf_grid <- cbind(hexagons,diversity)

sf_grid[,max(entropy)]
# s_max
sf_grid[,entropy:=entropy/s_max]

##### Reading or saving the data

out_filename <- paste0('data/neighbourhoods_',h3_res,'_',city,'_iso_',iso_dist,'.rds')

if(!file.exists(out_filename)){
  message('saving the grid locally')
  rlist::list.save(list('sf_grid'=sf_grid
                        ,'iso_'=isochrones),out_filename)
} else if(!exists('sf_grid')){
  message('reading local file with grid')
  level_data <- rlist::list.load(out_filename)
  #
  sf_grid <- level_data$sf_grid
  #
  # isochrones <- level_data$iso_
}

#### Local Max
no_na <- function(x) {
  return(x[!is.na(x)])
}

nn_hex <- \(neighbors, k = 1) {
  # this function in the future could be replace by network voronoi style one
  if(k%in% c(0,1)){ return(neighbors) }
  for(i in 2:k){
    neighbors <- lapply(neighbors,FUN = \(x) touching_filt[x] |> unlist() |> unique() |> no_na())
  }
  return(neighbors)
}

sf_grid <- sf_grid |> sf::st_as_sf()

# the neighbours of each hex
touching <- st_touches(sf_grid,sf_grid)

# parameter controoling how many neighbours minimum does a cell need to qualify for a local max
grid_param_nn <- 0

touching_filt <- lapply(touching,FUN = \(x) if(length(x)>grid_param_nn) x else NA)

hist(sf_grid$entropy[sf_grid$entropy>0],breaks = 100)

# min entropy to qualify for local max 
min_neighb_entropy <- summary(sf_grid$entropy[sf_grid$entropy>0])[['3rd Qu.']]

min_neighb_entropy

# number of nearest neighbours to consider to decide which is the max, 3 or 4 seems good usually
nn_neighbourhood <- 2

local_max <- mcmapply(sf_grid$entropy
                      ,nn_hex(touching_filt,k=nn_neighbourhood)
                      ,SIMPLIFY = TRUE
                      ,FUN = \(val,neighb) { 
                        if(any(is.null(neighb),is.na(neighb))) { return(FALSE)}
                        else if (all(val>=sf_grid$entropy[neighb]) && val>min_neighb_entropy) {TRUE }
                        else FALSE })

sum(local_max)

# this will be further read by the python script.
local_max_nodes <- hexagons$node[local_max]

# reticulate::r_to_py(local_max_nodes)
# reticulate::r_to_py(sf_all$data)

####

plot_nn <- function(id,k=2){
  
  tmap::tmap_mode('view')
  sf_grid[nn_hex(touching_filt,k=k)[[which(sf_grid$h3_index==id)]],] |> 
    tmap::qtm(fill='red'
              ,fill.alpha=.6)
  
}

## plot an example neighbourhood of hexs
plot_nn(id=sf_grid |> samp_dt(1) |> pull(h3_index)
        ,k=nn_neighbourhood)

#### 

entropy_col <- leaflet::colorNumeric('viridis',domain = range(sf_grid$entropy))

size_col <- leaflet::colorNumeric('magma',domain = range(sf_grid$size))

leaf_map <- leaflet::leaflet(sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')) |>
  leaflet::addTiles() |>
  addMapPane("max", zIndex = 430) |> 
  addMapPane("layer", zIndex = 420) |> 
  leaflet::addPolygons(# sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
    fillColor = ~entropy_col(entropy)
    ,fillOpacity = .8
    ,opacity = 0
    ,group = 'entropy'
    ,options = pathOptions(pane = "layer")) |> 
  leaflet::addPolygons(# sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
    fillColor = ~size_col(size)
    ,fillOpacity = .8
    ,opacity = 0
    ,group = 'size'
    ,options = pathOptions(pane = "layer")) |> 
  leaflet::addPolygons(data=sf_grid[local_max,]
                       ,color = 'red'
                       ,fillOpacity = 0
                       ,opacity = 1
                       ,weight = 3
                       ,popup =~paste0('Entropy: ',entropy,' Size: ',size,'\t','ID: ',h3_index)
                       ,group = 'local_max'
                       ,options = pathOptions(pane = "max")
  ) |>
  addLayersControl(
    baseGroups = c("size", "entropy"),
    overlayGroups = c("local_max"),
    options = layersControlOptions(collapsed = FALSE)
  ) |> 
  addScaleBar(position = 'bottomleft',options = list(maxWidth=500))

leaf_map

map_file <- paste0('data/leaflet_map_',city,'_',h3_res,'.rds')

if(!file.exists(map_file)) {
  print('Saving map locally')
  rlist::list.save(leaf_map,map_file)
}

#### plotly visualisation

fig <- plotly::plot_ly(x = sf_grid$size, y = sf_grid$entropy)
fig2 <- plotly::subplot(
  fig |> plotly::add_markers(alpha = 0.2)
  # fig |> plotly::add_histogram2d()
)

fig2

# plot(sf_grid$size,sf_grid$entropy,pch=20)

#### Netowrk Voronoi

net_vor <- py$net_vor_dict

str(net_vor)
ns <- net_vor[[30]]
ns

neighb <- parallel::mclapply(net_vor
                             ,mc.cores = 6
                             ,FUN=\(ns) {
                               sf_all$coords[match(ns,osmid),.(x,y)] |>
                                 sf::st_as_sf(coords=c(1,2),crs=4326) |>
                                 sf::st_combine() |>
                                 sf::st_concave_hull(ratio = 0.7) |>
                                 sf::st_geometry()
                             })


# DON't execute further 
#### Clustering neighbourhoods

neighb <- neighbourhoods$iso(sf_grid |> select(h3_index,entropy,centroid) |> sf::st_as_sf(sf_column_name = 'centroid')
                             ,isochrones
                             ,cores = 6)

neighb |> unique() |> length()

sf_grid$neighb <- neighb

# hexagons

neighbourhood <- sf_grid[,list(size=.N,'geometry'=sf::st_union(geometry)),by='neighb'][size>5,]

tmap::tmap_mode('view')
neighbourhood |> sf::st_as_sf() |> 
  tmap::qtm(fill.alpha=.5
            ,fill = 'MAP_COLORS')

# sf_grid[,plot(size,entropy,log='x')]

####

size_pal <- leaflet::colorNumeric('plasma',domain = range(log(sf_grid[,size])))

leaflet(neighbourhood |> sf::st_as_sf()) |> 
  addTiles() |> 
  addPolygons(color = 'red'
              ,fillOpacity = 0
              ,opacity = 1
              ,weight = 2) |>
  leafgl::addGlPolygons(sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
                        ,fillColor = ~entropy_col(entropy)
                        # ,fillColor = ~size_pal(log(size))
                        ,popup = ~size
                        ,fillOpacity = .7) 
  # leafgl::addGlPoints(amenities # |> filter(amenity == 'yes')
  #                     ,radius = 10
  #                     ,fillOpacity = 1
  #                     ,opacity=1
  #                     ,popup = ~amenity)


####

leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  leafgl::addGlPoints(amenities
                      ,radius = 10
                      ,fillOpacity = 1
                      ,opacity=1
                      ,popup = ~amenity)
