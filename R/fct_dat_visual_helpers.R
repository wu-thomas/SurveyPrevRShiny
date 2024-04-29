###############################################################
### interactive map for country boundaries
###############################################################

#' @description produce interactive map for country boundaries
#'
#' @param gadm.level administrative level, c('National','Admin-1','Admin-2')[2]
#'
#' @param gadmData polygon file for plotting
#'
#' @return leaflet map object
#'
#' @noRd
#'

country.boundary.leaflet <-function(gadm.level,gadmData){

  gadmData <- sf::st_as_sf(gadmData)

  gadm.level.num <- admin_to_num(gadm.level)

  if(gadm.level=='National'){
    hover_labels=NA}else{

      gadmData$region.name = gadmData[[paste0('NAME_',gadm.level.num)]]
      if(gadm.level.num>1){
        gadmData$upper.adm.name = gadmData[[paste0('NAME_',gadm.level.num-1)]]
      }

      hover_labels <- gadmData %>%
        dplyr::rowwise() %>%
        dplyr::mutate(hover_label = {
          label <- paste0('Region: ', region.name, '<br/>')
          if(gadm.level.num>1){
            label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
          }
          htmltools::HTML(label)  # Ensure that HTML rendering is applied
        }) %>%
        dplyr::ungroup() %>%
        dplyr::pull(hover_label)

    }


  country.map <- gadmData  %>% leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.1)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      weight = 1,
      #color = "gray",
      #fillOpacity = 1,
      opacity = 1,
      label = ~ hover_labels, # display hover label
      labelOptions = leaflet::labelOptions(
        style = list("color" ="black"),  # Text color
        direction = "auto",
        textsize = "15px",
        noHide = F,  # Label disappears when not hovering
        offset = c(0,0)  # Adjust label position if necessary
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.75,
        bringToFront = TRUE,
        sendToBack=T)
    )

  return(country.map)

}


#' @example

if(FALSE){
  country_gadm <- readRDS('data/GADM_shp/BEN/BEN_GADM_display.rds')


  gadm.level <- 'National' # CountryInfo$GADM_display_selected_level()

  base.map <- country.boundary.leaflet(gadm.level=gadm.level,
                                       gadmData=country_gadm[[gadm.level]])
}

###############################################################
### static map for country boundaries
###############################################################

if(FALSE){

map_plot <- ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = "osm") +
  ggplot2::geom_sf(data = country_gadm[[gadm.level]], color = "#00008B", size = 2) +
  ggplot2::theme_minimal()


  country_gadm <- readRDS('data/GADM_shp/BEN/BEN_GADM_display.rds')


  gadm.level <- 'Admin-2' # CountryInfo$GADM_display_selected_level()

  base.map <- country.boundary.leaflet(gadm.level=gadm.level,
                                       gadmData=country_gadm[[gadm.level]])
}



### open street map server precaucious check

if(FALSE){
tile_url <- "https://tile.openstreetmap.org/6/32/21.png"

# Make the HTTP GET request
response <- httr::GET(tile_url)

# Check the status code of the response
if (httr::status_code(response) == 200) {
  print("The tile server is responding normally.")
} else {
  print("Failed to reach the tile server.")
}
}

###############################################################
### static map for number of clusters
###############################################################

#' @description produce static map for number of clusters in each region
#'
#' @param gadm.level administrative level, c('National','Admin-1','Admin-2')[2]
#'
#' @param gadm.list list of polygons for all admin levels
#'
#' @param cluster.geo cluster location GPS
#'
#' @return ggplot of map and number of clusters without data
#'
#' @noRd
#'

ncluster.map.static <-function(gadm.level,
                               gadm.list,
                               cluster.geo){

  gadm.level.num=admin_to_num(gadm.level)

  ### make plot for admin-1
  if(gadm.level.num==1){

    adm.sf <- gadm.list[[paste0('Admin-',1)]]
    adm.sf$admin1.name <- adm.sf[[paste0("NAME_",1)]]

    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',1)]],
                                            by.adm1 = paste0("NAME_",1),
                                            by.adm2 = paste0("NAME_",1))

    adm.sf <- adm.sf %>%
      dplyr::left_join(cluster.info$data %>% dplyr::group_by(admin1.name) %>%
                         dplyr::summarise(n.clusters=dplyr::n()))

  }

  ### make plot for admin-2 or finer spatial scale

  if(gadm.level.num>1){

    adm.sf <- gadm.list[[paste0('Admin-',gadm.level.num)]]
    upper.adm.sf <- gadm.list[[paste0('Admin-',gadm.level.num-1)]]

    adm.sf$region.name <- adm.sf[[paste0("NAME_",gadm.level.num)]]
    adm.sf$upper.adm.name <- adm.sf[[paste0("NAME_",gadm.level.num-1)]]

    adm.sf <- adm.sf %>%
      dplyr::mutate(admin2.name.full = paste0(upper.adm.name, "_", region.name))


    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',gadm.level.num-1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',gadm.level.num)]],
                                            by.adm1 = paste0("NAME_",gadm.level.num-1),
                                            by.adm2 = paste0("NAME_",gadm.level.num))
    check.dat <-cluster.info$data




    adm.sf <- adm.sf %>%
      dplyr::left_join(cluster.info$data %>% dplyr::group_by(admin2.name.full) %>%
                         dplyr::summarise(n.clusters=dplyr::n()))


  }

  cluster.map.static <- adm.sf %>%
    ggplot2::ggplot() +
    #ggspatial::annotation_map_tile(type = "osm",zoomin=0) +
    ggplot2::geom_sf(ggplot2::aes(geometry=geometry, fill=n.clusters), colour=NA) +
    ggplot2::geom_sf(data=adm.sf, ggplot2::aes(geometry=geometry), lwd=0.5, fill=NA) +
    ggplot2::scale_fill_distiller(palette="Blues", direction=1,name='Number of \n Clusters') +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position="right")+
    ggplot2::theme(
      legend.position = "right",  # Position of the legend
      legend.text = ggplot2::element_text(size = 12),  # Larger text for the legend
      legend.title = ggplot2::element_text(size = 14),  # Larger title for the legend
      legend.key.size = ggplot2::unit(1, "cm")  # Larger key size
    )


  num.region <- dim(adm.sf)[1]
  num.no.cluster <- sum(is.na(adm.sf$n.clusters))

  return(list(map=cluster.map.static,
              num.region=num.region,
              num.no.cluster=num.no.cluster))

}

### example
if(FALSE){
country_gadm <- readRDS('data/GADM_shp/ZMB/ZMB_GADM_display.rds')
gadm.level <- 'Admin-2' # CountryInfo$GADM_display_selected_level()

cluster.geo= zmb.ex.GPS
gadm.list=country_gadm
gadm.level.num=admin_to_num(gadm.level)

tmp.res.obj <- ncluster.map.static(gadm.level='Admin-1',
                                   gadm.list=country_gadm,
                                   cluster.geo=zmb.ex.GPS)


}


###############################################################
### interactive map for number of clusters
###############################################################

#' @description produce interactive map for number of clusters in each region
#'
#' @param gadm.level administrative level, c('National','Admin-1','Admin-2')[2]
#'
#' @param gadm.list list of polygons for all admin levels
#'
#' @param cluster.geo cluster location GPS
#'
#' @return ggplot of map and number of clusters without data
#'
#' @noRd
#'
#'


ncluster.map.interactive <-function(gadm.level,
                               gadm.list,
                               cluster.geo){

  gadm.level.num=admin_to_num(gadm.level)

  ### make plot for admin-1
  if(gadm.level.num==1){

    adm.sf <- gadm.list[[paste0('Admin-',1)]]
    adm.sf$admin1.name <- adm.sf[[paste0("NAME_",1)]]

    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',1)]],
                                            by.adm1 = paste0("NAME_",1),
                                            by.adm2 = paste0("NAME_",1))

    adm.sf <- adm.sf %>%
      dplyr::left_join(cluster.info$data %>% dplyr::group_by(admin1.name) %>%
                         dplyr::summarise(n.clusters=dplyr::n()))

    hover_labels <- adm.sf %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hover_label = {
        label <- paste0('Region: ', admin1.name, '<br/>')
        label <- paste0(label,
                        'Number of clusters: ',n.clusters ,'<br/>')
        htmltools::HTML(label)  # Ensure that HTML rendering is applied
      }) %>%
      dplyr::ungroup() %>%
      dplyr::pull(hover_label)



  }

  ### make plot for admin-2 or finer spatial scale

  if(gadm.level.num>1){

    adm.sf <- gadm.list[[paste0('Admin-',gadm.level.num)]]
    upper.adm.sf <- gadm.list[[paste0('Admin-',gadm.level.num-1)]]

    adm.sf$region.name <- adm.sf[[paste0("NAME_",gadm.level.num)]]
    adm.sf$upper.adm.name <- adm.sf[[paste0("NAME_",gadm.level.num-1)]]

    adm.sf <- adm.sf %>%
      dplyr::mutate(admin2.name.full = paste0(upper.adm.name, "_", region.name))


    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',gadm.level.num-1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',gadm.level.num)]],
                                            by.adm1 = paste0("NAME_",gadm.level.num-1),
                                            by.adm2 = paste0("NAME_",gadm.level.num))
    check.dat <-cluster.info$data




    adm.sf <- adm.sf %>%
      dplyr::left_join(cluster.info$data %>% dplyr::group_by(admin2.name.full) %>%
                         dplyr::summarise(n.clusters=dplyr::n()))


    hover_labels <- adm.sf %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hover_label = {
        label <- paste0('Region: ', region.name, '<br/>')
        if(gadm.level.num>1){
          label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
        }
        if(is.na(n.clusters)){n.clusters=0}
        label <- paste0(label,
                        'Number of clusters: ',n.clusters ,'<br/>')
        htmltools::HTML(label)  # Ensure that HTML rendering is applied
      }) %>%
      dplyr::ungroup() %>%
      dplyr::pull(hover_label)



  }

  palette_colors <- RColorBrewer::brewer.pal(9, "Blues")

  pal <- leaflet::colorNumeric(palette = palette_colors,
                               domain = adm.sf$n.clusters,
                               na.color = '#AEAEAE')

  #hover_labels <- NA

  num_bins <- min(max(adm.sf$n.clusters,na.rm=T)-min(adm.sf$n.clusters,na.rm=T),7)


  cluster.map.interactive <- adm.sf  %>% leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.1)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~pal(n.clusters),
      weight = 1,
      color = "gray",
      fillOpacity = 1,
      opacity = 1,
      label = ~ hover_labels, # display hover label
      labelOptions = leaflet::labelOptions(
        style = list("color" ="black"),  # Text color
        direction = "auto",
        textsize = "15px",
        noHide = F,  # Label disappears when not hovering
        offset = c(0,0)  # Adjust label position if necessary
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.75,
        bringToFront = TRUE,
        sendToBack=T)
    )

  legend.label = paste0('Number of<br>', 'Clusters')
  ### add legend
  cluster.map.interactive <- cluster.map.interactive %>%
    leaflegend::addLegendNumeric(pal = pal, values = ~n.clusters, title =  htmltools::HTML(legend.label),
                                 orientation = 'vertical', fillOpacity = .7,
                                 position = 'bottomright', group = 'Symbols',
                                 width=25,height=150,naLabel ='No Data',
                                 data=adm.sf,
                                 decreasing=T,
                                 bins = num_bins, # Custom tick positions
                                 )



  num.region <- dim(adm.sf)[1]
  num.no.cluster <- sum(is.na(adm.sf$n.clusters))

  return(list(map=cluster.map.interactive,
              num.region=num.region,
              num.no.cluster=num.no.cluster))

}
#############
### example
#############

if(FALSE){
  country_gadm <- readRDS('data/GADM_shp/ZMB/ZMB_GADM_display.rds')
  #gadm.level <- 'Admin-2' # CountryInfo$GADM_display_selected_level()

  #cluster.geo= zmb.ex.GPS
  #gadm.list=country_gadm
 # gadm.level.num=admin_to_num(gadm.level)

  tmp.res.obj <- ncluster.map.interactive(gadm.level='Admin-2',
                                     gadm.list=country_gadm,
                                     cluster.geo=zmb.ex.GPS)

  tmp.res.obj$map
}








###############################################################
### experiments
###############################################################
if(FALSE){


  ###############################################################
  ### interactive map for country boundaries
  ###############################################################
  country_gadm <- readRDS('data/GADM_shp/MDG/MDG_GADM_display.rds')


  gadm.level <- 'Admin-2' # CountryInfo$GADM_display_selected_level()
  gadm.level.num <- admin_to_num(gadm.level)
  gadmData <- country_gadm[['Admin-2']]
  #gadmData <- CountryInfo$GADM_display_selected()

  country.boundary.leaflet <-function(gadm.level,gadmData){

    gadm.level.num <- admin_to_num(gadm.level)

  if(gadm.level=='National'){
  hover_labels='Yes'}else{

    gadmData$region.name = gadmData[[paste0('NAME_',gadm.level.num)]]
    if(gadm.level.num>1){
    gadmData$upper.adm.name = gadmData[[paste0('NAME_',gadm.level.num-1)]]
    }

  hover_labels <- gadmData %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hover_label = {
      label <- paste0('Region: ', region.name, '<br/>')
      if(gadm.level.num>1){
        label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
      }
      htmltools::HTML(label)  # Ensure that HTML rendering is applied
    }) %>%
    dplyr::ungroup() %>%
    dplyr::pull(hover_label)

  }


  country.map <- gadmData  %>% leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.1)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      weight = 1,
      #color = "gray",
      #fillOpacity = 1,
      opacity = 1,
      label = ~ hover_labels, # display hover label
      labelOptions = leaflet::labelOptions(
        style = list("color" ="black"),  # Text color
        direction = "auto",
        textsize = "15px",
        noHide = F,  # Label disappears when not hovering
        offset = c(0,0)  # Adjust label position if necessary
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.75,
        bringToFront = TRUE,
        sendToBack=T)
    )

  return(country.map)

  }

###############################################################
### static map for boundaris
###############################################################

tmp.boundary <- mdg.ex.GADM.list[['National']]

bbox <- sf::st_bbox(tmp.boundary)
lon_range <- c(bbox["xmin"], bbox["xmax"])
lat_range <- c(bbox["ymin"], bbox["ymax"])

base_map <- get_stamenmap(bbox = c(left = lon_range[1], bottom = lat_range[1], right = lon_range[2], top = lat_range[2]),
                          zoom = auto, maptype = "terrain-background")


###############################################################
### scatter plot for pointwise comparison
###############################################################



res.obj <-mdg.ex.model.res$FH$`Admin-2`
gadm.shp <- mdg.ex.GADM.list[["Admin-2"]]
model.gadm.level <- 2
strata.gadm.level <- 2
color_palette <- "viridis" # color.palette ='YlOrRd'
value.to.plot <- 'mean'
legend.label <- 'Estimates'
map.title <- NULL  # map.title= 'Prevalence for xxx'

}
