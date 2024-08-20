
source('functions.R')
# source('params.R')

###
# add import of relevant data sets here
#  personaalized version of dbscan:

sf_grid <- sf_grid |> sf::st_as_sf()

# the neighbours of each hex
touching <- sf::st_touches(sf_grid,sf_grid)

touching_filt <- lapply(touching,FUN = \(x) if(length(x)>grid_param_nn) x else NA)

density(sf_grid$size[sf_grid$size>0]) |> plot()

# min size to qualify for local max 
summary(sf_grid$size[sf_grid$size>=0])

min_neighb_size <- summary(sf_grid$size[sf_grid$size>0])[['3rd Qu.']]

min_neighb_size

local_max <- parallel::mcmapply(sf_grid$size
                                ,nn_hex(touching_filt,k=1)
                                ,SIMPLIFY = TRUE
                                ,mc.cores = 6
                                ,FUN = \(val,neighb) { 
                                  
                                  # print(all(val>=sf_grid$size[neighb]))
                                  if(is.na(val)) FALSE
                                  else if(any(is.null(neighb),is.na(neighb))) FALSE
                                  else if (all(val>=sf_grid$size[neighb],na.rm = TRUE) && val>=min_neighb_size) TRUE
                                  else FALSE
                                })

sum(local_max)


#### smoothing out local maxes

area <- sf_grid[1,'geometry'] |> sf::st_area() |> units::drop_units()

d <- 2*sqrt(2*area/(3*sqrt(3)))
d

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

# sapply(int,FUN=\(x) length(x))

smooth_local_max_ <- parallel::mcmapply(sf_grid$size[local_max]
                                        ,int
                                        ,SIMPLIFY = TRUE
                                        ,mc.cores = 6
                                        ,FUN = \(val,neighb) { 
                                          
                                          # print(all(val>=sf_grid$size[neighb]))
                                          if(is.na(val)) FALSE
                                          else if(any(is.null(neighb),is.na(neighb))) FALSE
                                          else if (all(val>=sf_grid$size[local_max][neighb],na.rm = TRUE) && val>=min_neighb_size) TRUE
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

str(net_vor)

net_vor_ids <- lapply(net_vor,\(nl) sf_all$dict$ref[nl])

sum(sapply(net_vor,USE.NAMES = FALSE,simplify = TRUE,FUN = length))

# neighb <- parallel::mclapply(net_vor
#                              ,mc.cores = 6
#                              ,FUN=\(ns) {
#                                sf_all$coords[ns+1,.(x,y)] |>
#                                  sf::st_as_sf(coords=c(1,2),crs=4326) |>
#                                  sf::st_combine() |>
#                                  sf::st_concave_hull(ratio = concavity) |>
#                                  sf::st_geometry()
#                              }) |> 
#   unlist(recursive = FALSE) |> 
#   sf::st_sfc(crs=4326) |> 
#   sf::st_as_sf()

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

# ##### map with neighbourhoods based on density

entropy_col <- leaflet::colorNumeric('viridis',domain = range(sf_grid$entropy,na.rm = TRUE),na.color = 'black')

size_col <- leaflet::colorNumeric('magma',domain = log1p(range(sf_grid$size)))

leaf_map_density <- leaflet::leaflet(sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')) |>
  leaflet::addTiles() |>
  addMapPane("max", zIndex = 430) |> 
  addMapPane("layer", zIndex = 420) |> 
  addMapPane('intermediate',zIndex = 425) |> 
  leaflet::addPolygons(# sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
    fillColor = ~entropy_col(entropy)
    ,fillOpacity = .8
    ,opacity = 0
    ,group = 'entropy'
    ,options = pathOptions(pane = "layer")) |> 
  leaflet::addPolygons(# sf_grid |> sf::st_as_sf(sf_column_name = 'geometry')
    fillColor = ~size_col(log1p(size))
    ,fillOpacity = .8
    ,opacity = 0
    ,group = 'size'
    ,options = pathOptions(pane = "layer")) |> 
  leaflet::addPolygons(data=sf_grid[local_max,][smooth_local_max_,]
                       ,color = 'red'
                       ,fillOpacity = 0
                       ,opacity = 1
                       ,weight = 3
                       ,popup =~paste0('Entropy: ',entropy,' Size: ',size,'\t','ID: ',h3_index)
                       ,group = 'local_max'
                       ,options = pathOptions(pane = "max")
  ) |>
  leaflet::addPolygons(data=neighb
                       ,fillOpacity = 0
                       ,fillColor = 'darkblue'
                       ,opacity = 1
                       ,color = 'orange'
                       ,weight = 2
                       ,group = 'boundaries'
                       ,options = pathOptions(pane = "intermediate")) |> 
  addLayersControl(
    baseGroups = c("size", "entropy"),
    overlayGroups = c("local_max","boundaries"),
    options = layersControlOptions(collapsed = FALSE)
  ) |> 
  addScaleBar(position = 'bottomleft',options = list(maxWidth=500))

leaf_map_density


