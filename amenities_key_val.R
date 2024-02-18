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

val_of_interest <- list("amenity"=amenityofinterest,'tourism'=tourismofinterest,'shop'=shopofinterest)

amenities_to_remove <- c("bench","parking_entrance","motorcycle_parking"
                         ,"parking_space","parking","bicycle_parking"
                         ,"disused:restaurant","disused:pub","disused"
                         ,"dog_parking","yes")

#  Rosmium 
if(FALSE) {
  
  cur_pbf <- "data/osm_extracts/france-latest.osm.pbf"
  sf::st_layers(cur_pbf)
  # get all amenity nodes
  # output <- tags_filter(cur_pbf, "n/amenity", tempfile(fileext = ".osm.pbf"))
  
  output_path <- extract(
    cur_pbf,
    sf::st_as_sf(ny_bb_sf),
    tempfile(fileext = ".osm.pbf"),
    spinner = FALSE
  )
  
  # get all objects (nodes, ways or relations) with an addr:* tag
  output <- lapply(paste0('n/',keys)
                   ,\(x) {
                     tags_filter(
                       output_path,
                       x,
                       tempfile(fileext = ".osm.pbf"),
                       omit_referenced = TRUE,
                       spinner = FALSE
                     )
                   })
  
  
  lapply(output,\(x)sf::st_layers(x))
  
  nodes <- lapply(output,\(x) sf::st_read(x, layer = "points", quiet = TRUE))
  
  values <- lapply(nodes,\(dat) dat$other_tags |>
                     str_split(',') |> 
                     sapply(FUN=\(x) x[1]) |> 
                     str_split('\"=>\"') |>
                     sapply(FUN = \(x) x[2]) |> 
                     str_extract('[^\\"]*'))
  
  all(sapply(nodes,nrow)==sapply(values,length))
  
  # nodes <- nodes |> dplyr::mutate(amenity=values)
  
  amenities <-  mapply(nodes
                       ,values
                       ,SIMPLIFY = FALSE
                       ,FUN=\(dat,val) dat |> 
                         dplyr::select(osm_id,name,geometry) |> 
                         dplyr::mutate(amenity=val))
  #####
  
  amenities <- lapply(amenities
                      ,FUN=\(dat)  {
                        dat |> 
                          dplyr::filter(!duplicated(osm_id)) |> 
                          dplyr::filter(!(amenity %in% amenities_to_remove)) |> 
                          dplyr::filter(amenity %in% unlist(val_of_interest,use.names = FALSE)) |> 
                          tidyr::drop_na(amenity)
                      })
  
  amenities <- dplyr::bind_rows(amenities)
  
  amenities <- amenities |> 
    dplyr::filter(as.logical(sf::st_intersects(geometry,ny_bb_sf,sparse=FALSE)))
  
  amenities |> sf::st_write(dsn=amenities_clean_filename,delete_dsn=TRUE,delete_layer=TRUE)

}
