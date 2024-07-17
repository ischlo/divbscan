library(rosmium)
library(stringr)
library(foreach)
# the keys and values of amenities to use.

keys <- c('amenity','tourism','shop')

amenityofinterest <- c("cafe","college","dentist","fire_station","hospital","nightclub","pharmacy","police","pub","recycling","studio"
                       ,"childcare","community_centre","embassy","food_court","marketplace","nursing_home","townhall","veterinary","bank"
                       ,"cinema","clinic","conference_centre","courthouse","crematorium","doctors","events_venue","fast_food","gym"
                       ,"ice_cream","internet_cafe","kindergarten","language_school","library","monastery","music_school","place_of_worship"
                       ,"planetarium","post_office","public_building","public_bookcase","restaurant","school","taxi","theatre","university"
)

shopofinterest <- c("alcohol","appliance","art","bakery","beauty","beverages","bicycle","bookmaker","books","boutique"
                    ,"butcher","camera","cheese","chemist","chocolate","clothes","coffee","computer","convenience"
                    ,"deli","department_store","doityourself","florist","general","greengrocer","hairdresser"
                    ,"health_food","hifi","jewelry","model","mobile_phone","musical_instrument","optician"
                    ,"organic","paint","pasta","pastry","pawnbroker","perfumery","photo","seafood","shoes"
                    ,"spices","sports","supermarket","tea","tobacco","toys")

tourismofinterest <- c("artwork","gallery","hotel","museum")

val_of_interest <- list("amenity" = amenityofinterest,'tourism' = tourismofinterest,'shop' = shopofinterest)

amenities_to_remove <- c("bench","parking_entrance","motorcycle_parking"
                         ,"parking_space","parking","bicycle_parking"
                         ,"disused:restaurant","disused:pub","disused"
                         ,"dog_parking","yes")

output_path <- rosmium::extract(
  cur_pbf,
  sf::st_as_sf(ny_bb_sf),
  tempfile(fileext = ".osm.pbf"),
  strategy = 'complete_ways',
  spinner = FALSE
)


# SOOOOOO GOOOOOOD
# amenities <- cppRnet::extract_data(output_path)

# amenities[!(value %in% amenities_to_remove),] |> cppRnet::construct_geom()

#  Rosmium 
if (!is.null(cur_pbf)) {
  
  # amenities_clean_filename <- paste0('data/',city,'_amenities_clean.geojson')
  
  if (!file.exists(amenities_clean_filename)) {
    
    ##
    sf::st_layers(cur_pbf)
    
    # get all nodes for the keys
    output <- lapply(paste0('nw/',keys)
                     ,\(x) {
                       tags_filter(
                         output_path,
                         x,
                         tempfile(fileext = ".osm.pbf"),
                         omit_referenced = TRUE,
                         spinner = FALSE
                       )
                     })
    
    lapply(output,\(x) sf::st_layers(x))
    
    layer_name <- 'points'
    
    nodes <- lapply(output,\(x) sf::st_read(x, layer = layer_name, quiet = TRUE))
    
    values <- mapply(nodes
                     ,keys
                     # ,mc.cores = 3
                     ,SIMPLIFY = TRUE
                     ,FUN = \(dat,key_) {
                       
                       dat$other_tags |>
                         stringr::str_split(',') |> 
                         lapply(FUN = \(x) {
                           if (any(stringr::str_detect(x,key_))) x[stringr::str_detect(x,paste0(key_,'\"=>\"'))] |> 
                             stringr::str_split('\"=>\"') |>
                             sapply(FUN = \(x) x[2]) |>
                             stringr::str_extract('[^\\"]*')
                           else NA
                         } 
                         )
                     })
    
    all(sapply(nodes,nrow) == sapply(values,length))
    
    amenities <-  mapply(nodes
                         ,values
                         ,SIMPLIFY = FALSE
                         ,FUN = \(dat,val) dat |> 
                           dplyr::select(osm_id,name,geometry) |> 
                           dplyr::mutate(amenity = as.character(val)))
    
    #####
    
    amenities <- lapply(amenities
                        ,FUN = \(dat)  {
                          dat |> 
                            dplyr::filter(!duplicated(osm_id)) |> 
                            dplyr::filter(!(amenity %in% amenities_to_remove)) |> 
                            # dplyr::filter(amenity %in% unlist(val_of_interest,use.names = FALSE)) |> 
                            tidyr::drop_na(amenity)
                        })
    
    amenities <- dplyr::bind_rows(amenities)
    
    amenities <- amenities |> 
      dplyr::filter(as.logical(sf::st_intersects(geometry,ny_bb_sf,sparse = FALSE)))
    
    
    #####
    
    amenity_cat <- amenities |> pull(amenity) |> unique()
    
    if(length(amenity_cat)>300) rlang::warn(message = 'Likely combined types of amenities'
                                            ,class = 'warning')
    
    amenities |> sf::st_write(dsn=amenities_clean_filename,delete_dsn=TRUE,delete_layer=TRUE)
    
    
  } else amenities <- sf::st_read(amenities_clean_filename)
}

###### code using osmdata but it's not stable
# 
# # amen <- getOSMdata(bb=ny_bb,k='amenity')
# amenities_filename <- paste0('data/',city,'_amenities.rds')
# 
# if(!file.exists(amenities_filename)){
#   print('Loading amenities from OSM')
#   amenities <- parallel::mcmapply(mc.cores=3
#                                   ,SIMPLIFY = FALSE
#                                   ,keys,val_of_interest, FUN = \(key,v) getOSMdata(bb=ny_bb,k=key,val = v))
#   
#   amenities |> rlist::list.save(amenities_filename)
# } else {
#   print('Reading in existing file')
#   amenities <- rlist::list.load(amenities_filename)
# }

##### cleaning amenities
# amenities_clean_filename <- paste0('data/',city,'_amenities_clean.geojson')
# 
# if(!file.exists(amenities_clean_filename)) {
#   amenities_clean <- parallel::mcmapply(amenities,keys,mc.cores = 3,SIMPLIFY = FALSE,FUN = \(x,key) osm_group_points(x,k=key))
#   
#   amenities_to_remove <- c("bench","parking_entrance","motorcycle_parking"
#                            ,"parking_space","parking","bicycle_parking"
#                            ,"disused:restaurant","disused:pub","disused"
#                            ,"dog_parking","yes")
#   
#   amenities <- foreach::foreach(data = amenities_clean
#                                 ,.combine = dplyr::bind_rows
#                                 ,.final = sf::st_as_sf) %do% { 
#                                   data |> 
#                                     filter(!duplicated(osm_id)) |> 
#                                     filter(!(amenity %in% amenities_to_remove)) |> 
#                                     drop_na(amenity)
#                                 }
#   
#   amenities <- amenities |> 
#     # filter(amenity %in% unique_amenities) |> # to only use the set of amenities from london
#     filter(as.logical(sf::st_intersects(geometry,ny_bb_sf,sparse=FALSE)))
#   
#   print('Saving amenities locally')
#   amenities |> sf::st_write(amenities_clean_filename,delete_dsn = TRUE,delete_layer = TRUE)
# } else if (!exists('amenities')){
#   print('reading in clean amenities')
#   amenities <- sf::st_read(amenities_clean_filename)
# } else print('amenity file and object exist')



