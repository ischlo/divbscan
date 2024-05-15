# divbscan new york
# depends on the divbscan run for oxford from the divbscan project.

# 
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
library(RANN)

####
reticulate::use_python('/opt/homebrew/bin/python3.11')

source('functions.R')
source('saved_bboxes.R')

cat('Available cities: '
    ,paste(names(bbox_of_interest),collapse = ', ')
    ,'\n Select one before running \n')

source('params.R')
cat('parameters read;\n')
# file with amenities
cur_pbf <- bbox_of_interest[[city]]$src

#  add check for null here
bbox <- bbox_of_interest[[city]]$bbox

bbox <- c(bbox[['west']],bbox[['south']],bbox[['east']],bbox[['north']])

ny_bb <- bbox |> matrix(ncol = 2,byrow = FALSE)
ny_bb_sf <- Btoolkit::make_poly(ny_bb)

tmap::tmap_mode('view')
ny_bb_sf |> tmap::qtm(fill.alpha=.3)

centroid <- sf::st_centroid(ny_bb_sf) |> sf::st_coordinates()

source('amenities_key_val.R')
cat('amenities extracted and concatenated;\n')

amenities |> Btoolkit::samp_dt(.2) |> tmap::qtm(scale = 1
                                                ,dots.col = 'black')

nrow(amenities)
summary(amenities)

# import the network: add network read from '.pbf' file.
source('network_import.R')
cat('network set up; \n')

###
## the set of amenities that was used for london
# unique_amenities <- rlist::list.load('/Users/ivannschlosser/Documents/deconstructing_neighbourhoods/unique_amens.rds')

####

amenity_cat <- amenities |> pull(amenity) |> unique()

s_max <- log(amenity_cat |> length())

#### road network setup

if(!file.exists(network_filename)){
  source('sf_net_setup.R')
} else if (!exists('sf_all')) {
  print('loading network')
  network_ <- rlist::list.load(paste0('data/networks/',city,'_all.rds'))
  sf_all <- network_$graph
  sf_all_ch <- network_$contracted
}

#####
if(!file.exists(hex_filename)){
  #### setting up the H3 grid
  # https://crazycapivara.github.io/h3-r/articles/h3.html
  if(is.numeric(h3_res)){
    #alternatively, for faster, but less accurate results use: amenities
    h3_index <- h3::geo_to_h3(sf_all$coords[,c(3,2)] # latitude ,longitude : in that order
                              ,res = h3_res) |> unique()
    
    tbl <- table(h3_index)  |>
      tibble::as_tibble()
    
    hexagons_ <- h3_to_geo_boundary_sf(tbl$h3_index)
    
    # hexagons_ <- h3::polyfill(ny_bb_sf,res = h3_res) |> h3::h3_to_geo_boundary_sf()
    
    nrow(hexagons_)
    
    head(hexagons_)
    
    # leaflet::leaflet(hexagons_) |>
    #   leaflet::addTiles() |>
    #   leaflet::addPolygons(data=hexagons_
    #                         ,popup = h3_index
    #                         ,color = 'red'
    #                         ,opacity=1
    #                         ,fillColor = 'darkblue'
    #                         ,fillOpacity = .4
    #                         ,weight=1)
    
    hexagons <- hexagons_ |> mutate(centroid=sf::st_centroid(geometry)) |> 
      as.data.table()
    
  } else if(h3_res=='custom') {
    cat('loading custom file london_hex.geojson')
    # alternatively, use the modified grid from the original work: 
    hexagons <- sf::st_read('data/london_hex.geojson') |> 
      sf::st_transform(4326)
    
    hexagons <- hexagons |> dplyr::mutate(centroid = sf::st_centroid(geometry)) |> data.table::as.data.table()
    
    # artificially creating the h3_index column
    hexagons$h3_index <- 1:nrow(hexagons)
    
    hexagons <- as.data.table(hexagons)  
  }
  
  #### computing isochrones. 
  
  # source('db_.R')
  # hexagons <- merge(hexagons,nn_search,by='h3_index',all = TRUE)
  
  nn <- Btoolkit::cppr$fnearest_nodes(sf_all
                                      ,hexagons$centroid
                                      ,local_crs = 27700)
  
  hexagons$node <- sf_all$dict$ref[nn$nn.idx]
  
  
  # nrow(hexagons)
  print('Saving hexagons locally')
  hexagons |> mutate(centroid=sf::st_as_text(centroid)) |> sf::st_write(hex_filename,delete_dsn=TRUE,delete_layer=TRUE)
  
} else if(!exists('hexagons')){
  print('reading hexagons from local file')
  hexagons <- sf::st_read(hex_filename) |> as.data.table()
}
cat('hexagons set up; \n')
#####

if(!file.exists(filename_isochrones)){
  
  isodist_nodes <- cppRouting::get_isochrone(sf_all,from = hexagons$node,lim = iso_dist)
  
  isodist_nodes |> sapply(FUN = length) |> summary()
  
  # function definition
  to_text_poly <- \(x) {
    x |> 
      apply(MARGIN = 1,FUN = \(x) paste0(x,collapse = ' ')) |> 
      sapply(FUN=\(y) paste0('(',y,')',collapse = ' ')) |> 
      paste(collapse = ', ') |> 
      sapply(FUN = \(x) paste0("MULTIPOINT (",x,")"),USE.NAMES = FALSE)
  }

  # This code makes smooth isodists out of all the points reachable in the network.
  # multipoints_smooth <- parallel::mclapply(isodist_nodes
  #                                   ,mc.cores = 6
  #                                   ,FUN=\(ns) {
  #                                     sf_all$coords[match(ns,osmid),.(x,y)] |> 
  #                                       to_text_poly()
  #                                   }) |>
  #   unlist(recursive=FALSE) |>
  #   as.data.frame() |>
  #   `colnames<-`('geom_wkt') |>
  #   sf::st_as_sf(wkt = 1,crs=4326) |> 
  #   sf::st_buffer(dist = 10,nQuadSegs = 1) |> 
  #   sf::st_concave_hull(ratio = concavity)
  # 
  # isochrones_smooth <- multipoints_smooth |> sf::st_as_sf()
  

  # this code instead of constructing isodistances from individual nodes
  # , clusters them according to which hexagon centroid they reach
  # and assembles isodistances out of hexagons
  # 
  #
  multipoints_ <- parallel::mclapply(isodist_nodes#[1:10000]
                                     ,mc.cores = 1
                                     ,FUN=\(ns) {
                                       hexagons[match(ns,node),][!is.na(node),geometry] |>
                                         sf::st_union()

                                     }) |>
    unlist(recursive=FALSE) |>
    sf::st_sfc(crs=4326)

  isochrones <- multipoints_ |> sf::st_as_sf()

  # if running the old school way with all points in the isodist.
  # sf::st_write(multipoints_,dsn=conn,layer = 'multipoints',layer_options = c("OVERWRITE=yes", "LAUNDER=true"))
  # # 
  # isodist_concave_query <- paste0('SELECT ST_AsText(ST_ConcaveHull(geom_wkt,',concavity,',false)) as geom from multipoints;')
  # 
  # res <- DBI::dbGetQuery(conn = conn,statement = isodist_concave_query)
  # 
  # # res[950,]
  # 
  # isochrones <- data.frame('x'=res |>
  #                          unlist(recursive = FALSE)) |>
  # sf::st_as_sf(wkt=1,crs=4326)
  # 
  # summary(sf::st_is_valid(isochrones))
  # summary(sf::st_is_empty(isochrones))
  # 
  # sum(sf::st_geometry_type(isochrones)=='POLYGON')
  
  # res[(sf::st_geometry_type(isochrones$geometry) %in% c('POINT','LINESTRING')),]|>data.frame()|>sf::st_as_sf(wkt=1,crs=4326)|>tmap::qtm()
  
  ####
  # 
  # isochrones[(sf::st_geometry_type(isochrones) %in% c('POINT','LINESTRING')),] <-
  #   isochrones[(sf::st_geometry_type(isochrones) %in% c('POINT','LINESTRING')),] |>
  #   sf::st_buffer(dist = 50,nQuadSegs = 1)
  # 
  # 
  # if(any(sum(sf::st_is_empty(isochrones))>0
  #        ,sum(!sf::st_is_valid(isochrones))>0)) warning('Some isochrones are empty.')
  # 
  # isochrones[!sf::st_is_valid(isochrones),] <- isochrones[!sf::st_is_valid(isochrones),] |> sf::st_make_valid()
  
  
  
  samp_size <- min(200,nrow(isochrones)*0.02) |> round()
  
  leaflet::leaflet(isochrones |> sf::st_as_sf() |> samp_dt(samp_size)
  ) |> 
    leaflet::addTiles() |> 
    leaflet::addPolygons(fillColor = 'dimgray'
                         ,fillOpacity = .6
                         ,weight = 1)
  
  isochrones |> sf::st_area() |> sqrt() |> hist(breaks = 100, xlab = expression(sqrt(A)))
  isochrones |> sf::st_area() |> sqrt() |> mean()
  
  # isochrone_poly <- isochrone_poly |> sf::st_as_sf() |> mutate(id=1:length(geometry))
  
  message('Saving isochrones locally')
  rlist::list.save(isochrones,filename_isochrones)
} else if(!exists('isochrones')){
  message('Reading local file')
  isochrones <- rlist::list.load(filename_isochrones)
} else print('file exists everywhere')

# starting from here, the robustness can be implemented.

####

if(!file.exists(out_filename)){

  source('entropy_iso.R')
  
  # grouping variable, add as parameter to the function
  by_ <- 'amenity'
  
  diversity <- entropy_iso(d=amenities
                           ,iso = isochrones
                           ,cor_num = 6
                           # ,by_ = 'amenity'
  )
  
  summary(diversity)
  
  if(!(nrow(diversity)==nrow(hexagons))) warning('Missmatch between hexs and diversity score')
  
  # diversity$entropy
  
  sf_grid <- cbind(hexagons,diversity) |> as.data.table()
  
  sf_grid[,summary(entropy)]
  
  # s_max
  sf_grid[,entropy:=entropy/s_max]
  
  noise <- runif(nrow(sf_grid))/1000
  
  sf_grid$entropy <- sf_grid$entropy + noise
  
  sf_grid$size <- sf_grid$size + noise
  
  ##### Reading or saving the data
  
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

# if(!file.exists(web_filename) & overwrite){
#   cat('Saving the grid for the web. \n')
#   sf::st_write(obj=sf_grid
#                ,dsn = web_filename
#                ,layer = 'grid'
#                ,delete_dsn = FALSE
#                ,delete_layer = TRUE)
# }


#### Local Max
sf_grid <- sf_grid |> sf::st_set_geometry('geometry')

#### keep running from here to compute diversity clusters
# or switch to dbsccan_local to compute density clusters

cat('grid set up; \n')

# the neighbours of each hex
touching <- sf::st_touches(sf_grid,sf_grid)

touching_filt <- lapply(touching,FUN = \(x) if(length(x)>grid_param_nn) x else NA)

hist(sf_grid$entropy[sf_grid$entropy>0],breaks = 100)

# min entropy to qualify for local max 
summary(sf_grid$entropy[sf_grid$entropy>=0])

min_neighb_entropy <- summary(sf_grid$entropy[sf_grid$entropy>=0])[['Mean']]

min_neighb_entropy

local_max <- parallel::mcmapply(sf_grid$entropy
                                ,nn_hex(touching_filt,k=1)
                                ,SIMPLIFY = TRUE
                                ,mc.cores = 6
                                ,FUN = \(val,neighb) { 
                                  
                                  # print(all(val>=sf_grid$entropy[neighb]))
                                  if(is.na(val)) FALSE
                                  else if(any(is.null(neighb),is.na(neighb))) FALSE
                                  else if (all(val>=sf_grid$entropy[neighb],na.rm = TRUE) && val>=min_neighb_entropy) TRUE
                                  else FALSE
                                })

sum(local_max)

##### constructing neighbourhoods #####

area <- sf_grid['geometry'] |> 
  sf::st_area() |> 
  units::drop_units() |> 
  mean()

d <- 2*sqrt(2*area/(3*sqrt(3)))
d

# When changing the smoothing parameter run from here:

smoothing_isodist <- cppRouting::get_isochrone(sf_all
                                               ,from = sf_grid$node[local_max]
                                               ,lim = nn_neighbourhood*d)

# when changing concavity run from here
smoothing_multipoints <- parallel::mclapply(smoothing_isodist
                                            ,mc.cores = 6
                                            ,FUN=\(ns) {
                                              sf_all$coords[match(ns,osmid),.(x,y)] |> 
                                                sf::st_as_sf(coords=c(1,2)
                                                             ,crs=4326) |> 
                                                sf::st_combine() |> 
                                                sf::st_concave_hull(ratio = concavity) |> 
                                                sf::st_geometry()
                                            }) |> 
  unlist(recursive = FALSE) |> 
  sf::st_sfc(crs=4326) |> 
  sf::st_as_sf()

int <- sf::st_intersects(sf_grid[local_max,],smoothing_multipoints)

######

smooth_local_max_ <- parallel::mcmapply(sf_grid$entropy[local_max]
                                        ,int
                                        ,SIMPLIFY = TRUE
                                        ,mc.cores = 6
                                        ,FUN = \(val,neighb) { 
                                          
                                          # print(all(val>=sf_grid$entropy[neighb]))
                                          if(is.na(val)) FALSE
                                          else if(any(is.null(neighb),is.na(neighb))) FALSE
                                          else if (all(val>=sf_grid$entropy[local_max][neighb],na.rm = TRUE) && val>=min_neighb_entropy) TRUE
                                          else FALSE
                                        })

# some of the local maxes are redundant and we need to recompute them. 
summary(smooth_local_max_)

# this will be further read by the python script.
local_max_nodes <- sf_grid$node[local_max][smooth_local_max_]

nx_graph <- sf_all$data

py_node_id <- sf_all$dict$id[match(local_max_nodes,sf_all$dict$ref)]

# reticulate::r_to_py(local_max_nodes)
reticulate::r_to_py(py_node_id)
reticulate::r_to_py(nx_graph)

####
# 
# plot_nn <- function(id,k=2){
#   
#   tmap::tmap_mode('view')
#   sf_grid[nn_hex(touching_filt,k=k)[[which(sf_grid$h3_index==id)]],] |> 
#     tmap::qtm(fill='red'
#               ,fill.alpha=.6)
#   
# }
# 
## plot an example neighbourhood of hexs
# plot_nn(id=sf_grid |> samp_dt(1) |> dplyr::pull(h3_index)
#         ,k=nn_neighbourhood)

##### Netowrk Voronoi

#######

reticulate::py_run_file('network_voronoi.py')

net_vor <- py$net_vor_dict

# str(net_vor)

net_vor_ids <- lapply(net_vor,\(nl) sf_all$dict$ref[nl])

sum(sapply(net_vor,USE.NAMES = FALSE,simplify = TRUE,FUN = length))

node_match <- sf_all$dict$id[match(sf_grid$node,sf_all$dict$ref)]
# hexagons
sf_grid$nn <- node_match

neighb <- lapply(net_vor,FUN = \(nodes) { 
  sf_grid[match(nodes,sf_grid$nn),] |> sf::st_union() }) |> 
  do.call(what = rbind) |>  
  sf::st_sfc(crs=4326) |> 
  sf::st_as_sf()

neighb |> sf::st_is_valid() |> summary()
neighb |> sf::st_is_empty() |> summary()


# sf::st_write(obj=neighb
#              ,dsn = web_filename
#              ,layer = 'neighbourhood'
#              ,delete_dsn = FALSE
#              ,delete_layer = TRUE)

# sf::st_layers(web_filename) |> length()

#### Summary stats of neighbourhoods:

typ_size <- neighb |> 
  sf::st_area() |> 
  units::drop_units() |> 
  sqrt()

m_typ_size <- mean(typ_size)
m_typ_size
sd_typ_dist <- sd(typ_size)
sd_typ_dist

typ_size |> 
  density() |> 
  plot(main = paste0('Typical size distribution, N=',nrow(neighb))
       ,xlab = 'Typical size'
       ,ylab = expression(rho)
       ,cex.lab=1.3
       )
legend(x='right',legend = paste0('mean=',round(m_typ_size),'; sd=',round(sd_typ_dist)))


#######

provider_tile <- providers$CartoDB.Positron

entropy_col <- leaflet::colorNumeric('viridis'
                                     ,domain = range(sf_grid$entropy,na.rm = TRUE)
                                     ,na.color = 'black')

size_col <- leaflet::colorNumeric('magma',domain = log1p(range(sf_grid$size)))

leaf_map <- leaflet::leaflet(sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
                             ,options=leafletOptions(zoomControl = FALSE)) |>
  # addProviderTiles(provider = provider_tile) |> 
  addTiles() |> 
  addMapPane("max", zIndex = 430) |> 
  addMapPane("layer", zIndex = 420) |> 
  addMapPane('intermediate',zIndex = 425) |> 
  # entropy grid
  leaflet::addPolygons(# sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
    fillColor = ~entropy_col(entropy)
    ,fillOpacity = .6
    ,opacity = 0
    ,group = 'entropy'
    ,options = pathOptions(pane = "layer")) |> 
  # size grid
  leaflet::addPolygons(# sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
    fillColor = ~size_col(log1p(size))
    ,fillOpacity = .8
    ,opacity = 0
    ,group = 'size'
    ,options = pathOptions(pane = "layer")) |>
  # local maxes
  leaflet::addPolygons(data=sf_grid[local_max,][smooth_local_max_,]
                       ,color = 'red'
                       ,fillOpacity = 0
                       ,opacity = 1
                       ,weight = 3
                       ,popup =~paste0('Entropy: ',round(entropy,3)
                                       ,' Size: ',round(size),'\t'
                                       ,'ID: ',h3_index)
                       ,group = 'local_max'
                       ,options = pathOptions(pane = "max")
  ) |>
  # neighbourhood boundaries
  leaflet::addPolygons(data=neighb
                       ,fillOpacity = 0
                       ,fillColor = 'darkblue'
                       ,opacity = 1
                       ,color = 'black'
                       ,weight = 2
                       ,group = 'boundaries'
                       ,options = pathOptions(pane = "intermediate")) |>
  # layer controls
  addLayersControl(
    baseGroups = c("size", "entropy"),
    overlayGroups = c("local_max","boundaries"),
    options = layersControlOptions(collapsed = TRUE)
  ) |>
  addScaleBar(position = 'bottomleft',options = list(maxWidth=500)) |> 
  setView(lat = centroid[2]
          ,lng = centroid[1]
          ,zoom = 13)

leaf_map

if(!file.exists(map_file)) {
  # leaf_map
  print('Saving map locally')
  rlist::list.save(leaf_map,map_file)
} else if (file.exists(map_file)) {
  source('params.R')
  leaf_map <- rlist::list.load(map_file)
}

# leaf_map

#### plotly visualisation

fig <- plotly::plot_ly(x = sf_grid$size, y = sf_grid$entropy) |> 
  plotly::layout(xaxis = list(type = "log"))
fig2 <- plotly::subplot(
  fig |> plotly::add_markers(alpha = 0.4)
  # fig |> plotly::add_histogram2d()
)

fig2
 
