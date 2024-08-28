# plot_support
# 
# 
# intermediate plots for oxford interview
# 

source("functions.R")

library(leaflet.providers)

provider_tile <- providers$CartoDB.Positron


# leaflet params

centroid <- sf::st_centroid(ny_bb_sf) |> sf::st_coordinates()

zoom_ <- 11

# bbox 

plot_base_map(data = ny_bb_sf,zoom_ = zoom_) |> 
  addPolygons(fillOpacity = .3,fillColor = 'gray',color = 'black',opacity = 1,weight = .5)

# amenities

samp_size_amen <- min(10000,nrow(amenities))

amenities_for_plotting <- amenities |> 
  dplyr::filter(!(amenity %in% c("parking","bench")))

amen_colors <- colorFactor('Set1'
                           ,domain = amenities_for_plotting$amenity
                           ,levels = unique(amenities_for_plotting$amenity)
                           )

amenities_for_plotting |> 
  Btoolkit::samp_dt(samp_size_amen) |> 
  plot_base_map(zoom_ = zoom_) |> 
  addPolygons(data = ny_bb_sf
              ,fillOpacity = 0
              ,fillColor = 'gray'
              ,color = 'black'
              ,opacity = 1
              ,weight = .5) |>
  addCircles(radius = 20
             ,weight = 1
             ,color = 'black'
             ,opacity = .3
             ,fillColor = ~amen_colors(amenity)
             ,fillOpacity = 1
             ,popup = ~amenity)

# roads

sf_edges[highway %in% c('primary','secondary','tertiary','residential'),"geom"] |> 
  sf::st_as_sf(sf_column_name = "geom") |> 
  dplyr::filter(sf::st_intersects(geom,ny_bb_sf,sparse = FALSE)) |> 
  plot_base_map(zoom_ = zoom_) |> 
  addPolylines(weight = 1,color = 'black',opacity = 1) |> 
  addPolygons(data = ny_bb_sf
              ,fillOpacity = 0
              ,fillColor = 'gray'
              ,color = 'black'
              ,opacity = 1
              ,weight = .5)


# hexagons

hexagons |> sf::st_as_sf() |> 
  plot_base_map(zoom_ = zoom_) |> 
  addPolygons(fillOpacity = .3
              ,fillColor = 'gray'
              ,color = 'black'
              ,opacity = 1
              ,weight = .5
              ,popup = ~h3_index) |> 
  addPolygons(data = ny_bb_sf
              ,fillOpacity = 0
              ,fillColor = 'gray'
              ,color = 'black'
              ,opacity = 1
              ,weight = .5)

# isodists

isochrones$id <- 1:nrow(isochrones)

isochrones_smooth |> 
  dplyr::filter(as.logical(sf::st_intersects(geom_wkt,ny_bb_sf,sparse = FALSE))) |> 
  plot_base_map(zoom_ = zoom_) |> 
  leaflet::addPolygons(fillColor = 'dimgray'
                       ,fillOpacity = .6
                       ,weight = 1
                       ,popup = ~paste0('isodist id: ',id)) |> 
  addPolygons(data = ny_bb_sf
              ,fillOpacity = 0
              ,fillColor = 'gray'
              ,color = 'black'
              ,opacity = 1
              ,weight = .5)

# amenities in isodist

plot_base_map(isochrones_smooth) |> 
  addPolygons(popup = ~paste0(id))
  
isodist_selected <- 453

amenities_selected <- amenities_for_plotting |> sf::st_filter(isochrones_smooth[isodist_selected,])

centroid_isodist <- isochrones_smooth[isodist_selected,] |> 
  sf::st_centroid() |> 
  sf::st_coordinates()

isochrones_smooth[isodist_selected,] |> 
  plot_base_map(zoom_ = 17,centroid_ = centroid_isodist) |> 
  addPolygons(fillOpacity = .3
              ,fillColor = 'gray'
              ,color = 'black'
              ,opacity = 1
              ,weight = .5) |>
  addCircles(data = amenities_selected
             ,radius = 4
             ,weight = 1
             ,color = 'black'
             ,opacity = 1
             ,fillColor = ~amen_colors(amenity) #'orange'
             ,fillOpacity = 1
             ,popup = ~amenity) |>
  addCircles(data = hexagons_samp[isodist_selected,"centroid"] |> sf::st_drop_geometry() |> sf::st_as_sf(wkt = 1,crs = 4326)
             ,radius = 7
             ,weight = .1
             ,fillOpacity = 1
             ,fillColor = 'black'
             ,color = 'black')