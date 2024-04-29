###############################################################
### required packages
###############################################################

# library(leaflet)
#devtools::install_github("statnmap/HatchedPolygons")
# devtools::install_github("tomroh/leaflegend")


###############################################################
### harmonize column names
###############################################################
#' visualization_helpers
#'
#' @description make the naming for one column the same for different methods
#'
#' @param survey.res result summary data.frame
#'
#' @param from_col possible column names for a feature, such as c("direct.est", "mean")
#'
#' @param to_col harmonized name for the column
#'
#' @return data.frame with modified column names
#'
#' @noRd
#'
#'

harmonize_one_col <- function(survey.res,
                              from_col,
                              to_col){

  # Find the first matching column in the list
  existingCol <- from_col[from_col %in% names(survey.res)][1]

  # Create harmonized column if found match, else assign NA
  survey.res[to_col] <- if (!is.null(existingCol)) survey.res[[existingCol]] else NA


  return(survey.res)
}


###############################################################
### harmonize column names
###############################################################
#' visualization_helpers
#'
#' @description make the naming for all columns the same for different methods and admin levels
#'
#' @param survey.res result summary data.frame
#'
#' @return data.frame with modified column names
#'
#' @noRd
#'
#'

harmonize_all_cols <- function(survey.res){

  ###### harmonize summary statistics

  stat_var <- c('mean','median','sd','var','lower','upper','CI.width','cv')
  ### mean
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.est','mean'),
                                 to_col = 'mean')

  ### sd
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.se','sd'),
                                 to_col = 'sd')

  ### var
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.var','var'),
                                 to_col = 'var')

  ### coefficient of variation
  survey.res$cv <- survey.res$sd/survey.res$mean


  ### lower CI
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.lower','lower'),
                                 to_col = 'lower')

  ### upper CI
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.upper','upper'),
                                 to_col = 'upper')


  ### CI width
  survey.res$CI.width <- survey.res$upper-survey.res$lower


  ### set problematic uncertainties to NA
  survey.res <- survey.res %>%
    dplyr::mutate(var = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,var),
                  lower = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,lower),
                  upper = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,upper),
                  cv = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,cv),
                  CI.width = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,CI.width)
    )
  survey.res <- survey.res %>%
    dplyr::mutate(sd = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,sd))


  ### for direct estimates, mean is median
  if(!'median' %in% names(survey.res)){
    survey.res$median <- survey.res$mean
  }

  ###### harmonize region variables

  ### national estimates
  if(!'admin1.name' %in% names(survey.res)& !'admin2.name.full'%in% names(survey.res)){

    survey.res <- survey.res[, stat_var[stat_var %in% names(survey.res)], drop = FALSE]

    return(survey.res)
  }


  ### estimates not finer than stratification level

  if(!'admin2.name.full' %in% names(survey.res)){

    survey.res$region.name <- survey.res$admin1.name

    res.var <- c('region.name',stat_var)

    survey.res <- survey.res[, res.var[res.var %in% names(survey.res)], drop = FALSE]

    return(survey.res)
  }

  ### estimates finer than stratification level

  if('admin2.name.full' %in% names(survey.res)){

    if('admin1.name' %in% names(survey.res)){
      survey.res$region.name <-  survey.res$admin2.name
      survey.res$upper.adm.name <- survey.res[['admin1.name']]
    }else{
      survey.res <- survey.res %>%
        tidyr::separate(admin2.name.full, into = c("upper.adm.name", "region.name"), sep = "_", remove = FALSE)
    }

    survey.res$region.name.full <- survey.res[['admin2.name.full']]

    res.var <- c('region.name',stat_var,'upper.adm.name','region.name.full')

    survey.res <- survey.res[, res.var[res.var %in% names(survey.res)], drop = FALSE]

    return(survey.res)
  }

}



###############################################################
### format the columns with numerical values
###############################################################
#' visualization_helpers
#'
#' @description keep two decimal places
#'
#' @param survey.res result summary data.frame
#'
#' @return data.frame with rounded numbers
#'
#' @noRd
#'
#'

format_tab_num <- function(survey.res){

  format_numbers <- function(x) {
    ifelse(x < 0.01, sprintf("%.2g", x), sprintf("%.2f", x))
  }

  # Apply the function to each numerical column
  survey.res.formatted <- survey.res %>%
    dplyr::mutate(across(where(is.numeric), ~as.numeric(format_numbers(.))))

  return(survey.res.formatted)
}



###############################################################
### produce leaflet prevalence map for any subnational level
###############################################################
#' visualization_helpers
#'
#' @description produce interactive map for any subnational level
#'
#' @param res.obj result object from surveyPrev
#'
#' @param gadm.shp polygon file for plotting
#'
#' @param strata.gadm.level which GADM admin level is used as the stratification in DHS
#'
#' @param model.gadm.level which GADM admin level to produce maps for
#'
#' @param color.palette which palette to use for plotting
#'
#' @param color.reverse whether to use reverse color scale
#'
#' @param legend.label label for the legend, such as 'Coefficient of Variation'
#'
#' @param no.hatching whether to hatch region with problematic uncertainties, recommend F
#'
#' @param map.title title for the map
#'
#' @return leaflet map object
#'
#' @noRd
#'
#'
#'


prevMap.leaflet <- function(res.obj,
                            gadm.shp ,
                            model.gadm.level,
                            strata.gadm.level =1,
                            color.palette = "viridis",
                            value.to.plot = 'mean',
                            legend.label = 'Estimates',
                            map.title = NULL,
                            color.reverse = T,
                            no.hatching= F,
                            hatching.density=12){

  ########################################################
  ### initialize parameters
  ########################################################

  gadm.shp <- sf::st_as_sf(gadm.shp)

  ### get pseudo level
  if(model.gadm.level > strata.gadm.level){pseudo_level=2}else{pseudo_level=1}

  ########################################################
  ### prepare to.plot data set
  ########################################################

  survey.res <- res.obj[[paste0('res.admin',pseudo_level)]]

  res.to.plot <- harmonize_all_cols(survey.res=survey.res)
  #res.to.plot <- format_tab_num(survey.res=res.to.plot)
  #res.to.plot$value <- res.to.plot[[value.to.plot]] ### name the variable to plot as value

  ########################################################
  ### merge results with spatial dataset
  ########################################################

  if(pseudo_level==1){
    gadm.shp$full_name <- paste0(gadm.shp[[paste0('NAME_',model.gadm.level)]])
    gadm.with.res <- gadm.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name

  }else{
    gadm.shp$full_name <- paste0(gadm.shp[[paste0('NAME_',model.gadm.level-1)]],
                                 '_',
                                 gadm.shp[[paste0('NAME_',model.gadm.level)]])
    gadm.with.res <- gadm.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))

    gadm.with.res$region.name <- gadm.with.res[[paste0('NAME_',model.gadm.level)]]
    gadm.with.res$upper.adm.name <- gadm.with.res[[paste0('NAME_',model.gadm.level-1)]]

  }


  ### modify the format of numeric values and add warning messages
  gadm.with.res$warnings <- NA

  ### problematic sd, warnings and set uncertainty related measure to NA
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(!is.na(mean) &is.na(sd),
                                            "Data in this region are insufficient for <br/> reliable estimates with the current method.",
                                            NA)
    )

  ### no data warning
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(is.na(mean),
                                            "No data in this region",
                                            warnings))

  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value

  ### cv to %
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(cv = sprintf("%.1f%%", cv * 100))

  ### formatting numeric variables to 2 decimal places

  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(across(c(mean, var, lower, upper,CI.width), ~sprintf("%.2f", .)))

  ########################################################
  ### hatching for problematic sd
  ########################################################

  hatching.ind <- T

  hatching.gadm <- gadm.with.res %>%
    subset( is.na(sd) & (!is.na(value)))

  ### no hatching if all regions have reasonable sd or manually set
  if(dim(hatching.gadm)[1]==0 | (no.hatching)){
    hatching.ind <- F
  }else{

    ### setup hatching polygons
    hatching.regions <- HatchedPolygons::hatched.SpatialPolygons(hatching.gadm,
                                                                 density = c(hatching.density), angle = c(45))

    ### setup hatching legend
    warning.icon <- leaflet::awesomeIconList(
      'Sparse Data' =leaflet::makeAwesomeIcon(icon = "align-justify", library = "glyphicon",
                                              iconColor = 'gray',
                                              markerColor = 'white',
                                              squareMarker = TRUE, iconRotate = 135)
    )
  }

  #############################################
  ### parameters for color scale and breaks
  #############################################

  ### color palette
  pal <- leaflet::colorNumeric(palette = color.palette,
                               domain = gadm.with.res$value,
                               na.color = '#AEAEAE',
                               reverse = color.reverse)

  ### number of ticks on the legend
  num_bins <- min(round( (max(gadm.with.res$value,na.rm=T)-min(gadm.with.res$value,na.rm=T))/0.1),6)
  num_bins <- max(4,num_bins)

  numberFormat = function(x) {
    prettyNum(x, format = "f", big.mark = ",", digits =
                3, scientific = FALSE)
  }

  if(value.to.plot=='cv'){

    numberFormat = function(x) {
      paste0(formatC(100 * x, format = 'f', digits = 1), "%")
    }

  }

  ###############################################
  ### hovering effect, information to display
  ###############################################

  hover_labels <- gadm.with.res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hover_label = {
      label <- paste0('Region: ', region.name, '<br/>')
      if(pseudo_level==2){
        label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
      }
      label <- paste0(label,
                      'Mean (95% CI): ', mean, ' (', lower, ', ', upper, ')', '<br/>',
                      'Coefficient of Variation: ', cv, '<br/>')
      if (!is.na(warnings) && warnings != "") {
        label <- paste0(label, '<span style="color: red;">Warning: ', warnings, '</span><br/>')
      }
      htmltools::HTML(label)  # Ensure that HTML rendering is applied
    }) %>%
    dplyr::ungroup() %>%
    dplyr::pull(hover_label)



  ###############################################
  ### assemble
  ###############################################

  ### base map
  adm.map <- gadm.with.res  %>% leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.1)) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~pal(value),
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

  ### add legend
  adm.map <- adm.map %>%
    leaflegend::addLegendNumeric(pal = pal, values = ~value, title =  htmltools::HTML(legend.label),
                                 orientation = 'vertical', fillOpacity = .7,
                                 position = 'bottomright', group = 'Symbols',
                                 width=25,height=150,naLabel ='No Data',
                                 data=gadm.with.res,
                                 bins = num_bins, # Custom tick positions
                                 numberFormat=numberFormat,
                                 decreasing=T
    )

  if(hatching.ind){

    adm.map <- adm.map %>% leaflet::addPolylines(
      data = hatching.regions,
      color = c( "gray"),
      weight = 2.0,
      opacity = 0.8
    )
    adm.map <- adm.map %>% leaflegend::addLegendAwesomeIcon(iconSet = warning.icon,
                                                            title = 'Interpret with caution:',
                                                            position = 'bottomright')

  }

  ### add title

  if(!is.null(map.title)){
    tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.65);
    font-weight: bold;
    font-size: 20px;
    }
    "))

    title <- tags$div(
      tag.map.title, HTML(paste0(map.title))
    )

    adm.map <- adm.map %>%
      leaflet::addControl(title, position = "topleft", className="map-title")
  }

  return(adm.map)
}



### example

if(FALSE){
  ############# parameter
  res.obj <-mdg.ex.model.res$FH$`Admin-2`
  gadm.shp <- mdg.ex.GADM.list[["Admin-2"]]
  model.gadm.level <- 2
  strata.gadm.level <- 2
  color_palette <- "viridis" # color.palette ='YlOrRd'
  value.to.plot <- 'mean'
  legend.label <- 'Estimates'
  map.title <- NULL  # map.title= 'Prevalence for xxx'

  tmp.plot1 <- prevMap.leaflet(res.obj = mdg.ex.model.res$Direct$`Admin-2`,
                               gadm.shp = mdg.ex.GADM.list[["Admin-2"]],
                               model.gadm.level = 2,
                               strata.gadm.level = 2,
                               value.to.plot ='mean',
                               legend.label = 'mean',
                               map.title='this is a long longlong title')

  tmp.plot2 <- prevMap.leaflet(res.obj = mdg.ex.model.res$Direct$`Admin-3`,
                               gadm.shp = mdg.ex.GADM.list[["Admin-3"]],
                               model.gadm.level = 3,
                               strata.gadm.level = 2,
                               value.to.plot ='mean',
                               legend.label = 'mean',
                               map.title='this is a long longlong title')

  sync.plot <- leafsync::latticeView(tmp.plot1, tmp.plot2,
                                     tmp.plot2,tmp.plot2,tmp.plot2,
                                     ncol = 2, sync = "none")

}



#htmlwidgets::saveWidget(tmp.plot, 'mymap.html', selfcontained = TRUE)

#webshot2::webshot('mymap.html', file = 'mymap.png')
###############################################################
### static prevalence map
###############################################################



prevMap.static <- function(res.obj,
                           gadm.shp ,
                           model.gadm.level,
                           strata.gadm.level =1,
                           value.to.plot = 'mean',
                           map.title = NULL,
                           legend.title = NULL,
                           ...){


  ########################################################
  ### initialize parameters
  ########################################################

  gadm.shp <- sf::st_as_sf(gadm.shp)

  ### get pseudo level
  if(model.gadm.level > strata.gadm.level){pseudo_level=2}else{pseudo_level=1}

  ########################################################
  ### prepare to.plot data set
  ########################################################

  survey.res <- res.obj[[paste0('res.admin',pseudo_level)]]

  res.to.plot <- harmonize_all_cols(survey.res=survey.res)

  ########################################################
  ### merge to spatial data
  ########################################################

  if(pseudo_level==1){
    gadm.shp$full_name <- paste0(gadm.shp[[paste0('NAME_',model.gadm.level)]])
    gadm.with.res <- gadm.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))

  }else{
    gadm.shp$full_name <- paste0(gadm.shp[[paste0('NAME_',model.gadm.level-1)]],
                                 '_',
                                 gadm.shp[[paste0('NAME_',model.gadm.level)]])
    gadm.with.res <- gadm.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))
  }


  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value
  gadm.with.res$method <- ''

  static.map <- SUMMER::mapPlot(gadm.with.res, variables = "method", values = "value",
                                by.data = "full_name", geo = gadm.shp,
                                by.geo = "full_name", is.long = TRUE,
                                removetab = T,...)+
    ggplot2::theme (legend.text=ggplot2::element_text(size=12),
                    legend.title = ggplot2::element_text(size=14),
                    strip.text.x = ggplot2::element_text(size = 12),
                    legend.key.height = ggplot2::unit(1,'cm') )

  return(static.map)

}

#### examples

if(FALSE){
  res.obj = mdg.ex.model.res$Direct$`Admin-3`
  gadm.shp = mdg.ex.GADM.list[["Admin-3"]]
  model.gadm.level = 3
  strata.gadm.level = 2
  method_des = NA
  value.to.plot ='mean'
  legend.label = 'mean'
  map.title='this is a long longlong title'

  tmp.plot <- prevMap.static(res.obj = mdg.ex.model.res$Unit$`Admin-3`,
                             gadm.shp = mdg.ex.GADM.list[["Admin-3"]],
                             model.gadm.level = 3,
                             strata.gadm.level = 2,
                             value.to.plot ='mean',
                             legend.title = 'Coefficient of Variation',
                             direction=-1)

  if(FALSE){
    tmp.plot <-  tmp.plot + ggplot2::theme (legend.position = 'bottom',legend.key.height= ggplot2::unit(0.5,'cm'),
                                            legend.text=ggplot2::element_text(size=12),
                                            legend.key.width = ggplot2::unit(2,'cm'),legend.title = ggplot2::element_text(size=14),
                                            strip.text.x = ggplot2::element_text(size = 12))+
      ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top",
                                                      title.hjust = .5,
                                                      title=legend.title,
                                                      label.position = "bottom"))
  }

  res.to.plot$method <-''

  p1 <- SUMMER::mapPlot(res.to.plot, variables = "method", values = "mean",
                        by.data = "region.name.full", geo = gadm.shp,
                        by.geo = "full_name", is.long = TRUE,
                        removetab = T)+
    ggplot2::theme (legend.position = 'none')

  p1 <- SUMMER::mapPlot(gadm.with.res2, variables = "method", values = "mean",
                        by.data = "full_name", geo = gadm.with.res,
                        by.geo = "full_name", is.long = TRUE,
                        removetab = T)+
    ggplot2::theme (legend.position = 'none')


  p1 <- tmp.plot
  plot_grid <- p1 + p1 + p1 + p1 +
    patchwork::plot_layout(ncol = 2) # Adjust the number of columns to suit your needs

  # Reduce the spacing between plots
  plot_grid + patchwork::plot_layout(guides = 'collect') & ggplot2::theme(
    legend.position = "bottom",
    plot.margin = ggplot2::unit(c(1, 1, 1, 1), "mm"))



}




###############################################################
### scatter plot
###############################################################
#'
#' @description interactive scatter plot comparing estimates from two methods for the same admin level
#'
#' @param res.obj.x result object from surveyPrev
#'
#' @param res.obj.y result object from surveyPrev
#'
#' @param strata.gadm.level which GADM admin level is used as the stratification in DHS
#'
#' @param model.gadm.level which GADM admin level to produce maps for
#'
#' @param value.to.plot which statistics to plot 'mean'
#'
#' @param label.x label on x-axis
#'
#' @param label.y label on y-axis
#'
#' @param plot.title title for the plot
#'
#' @param interactive whether to render interactive or static plot
#'
#' @return plotly or ggplot2 object


scatter.plot <- function(res.obj.x,
                         res.obj.y ,
                         model.gadm.level,
                         strata.gadm.level =1,
                         value.to.plot = 'mean',
                         label.x = 'Method 1 Estimates',
                         label.y = 'Method 2 Estimates',
                         plot.title=NULL,
                         interactive=T){


  ### get pseudo level
  if(model.gadm.level > strata.gadm.level){
    pseudo_level=2
    by.res = 'region.name.full'
  }else{
    pseudo_level=1
    by.res = 'region.name'
  }

  ########################################################
  ### prepare to.plot data set
  ########################################################

  ### prepare plot data
  survey.res.x <- res.obj.x[[paste0('res.admin',pseudo_level)]]
  if(is.null(survey.res.x)){return(NULL)} # do not plot if model not fitted
  res.to.plot.x <- harmonize_all_cols(survey.res=survey.res.x)

  survey.res.y <- res.obj.y[[paste0('res.admin',pseudo_level)]]
  if(is.null(survey.res.y)){return(NULL)} # do not plot if model not fitted
  res.to.plot.y <- harmonize_all_cols(survey.res=survey.res.y)


  ### label x and y
  res.to.plot.x$value_x <- res.to.plot.x[[value.to.plot]]
  res.to.plot.x <- res.to.plot.x[!is.na(res.to.plot.x$value_x),]
  res.to.plot.x$hover_x <- res.to.plot.x[[value.to.plot]] # in the case of missing data, to distinguish hover effect
  res.to.plot.x$label.x <- label.x

  res.to.plot.y$value_y <- res.to.plot.y[[value.to.plot]]
  res.to.plot.y <- res.to.plot.y[!is.na(res.to.plot.y$value_y),]
  res.to.plot.y$hover_y <- res.to.plot.y[[value.to.plot]] # in the case of missing data, to distinguish hover effect
  res.to.plot.y$label.y <- label.y


  ### color missing data
  if(length(res.to.plot.x$value_x)<length(res.to.plot.y$value_y)){
    missing <- subset(res.to.plot.y, !res.to.plot.y[[by.res]]%in% res.to.plot.x[[by.res]])

    if(value.to.plot %in% c('mean')){
      ### mean, missing on the left
    missing$value_x=rep(min(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }else{
      ### cv, CI.width, missing on the right
      missing$value_x=rep(max(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }

    missing$hover_x=rep(NA,dim(missing)[1])
    missing$hover_y=missing$value_y

  }else if( length( res.to.plot.x$value_x)>length(res.to.plot.y$value_y)){
    missing<-subset(res.to.plot.x, !res.to.plot.x[[by.res]] %in% res.to.plot.y[[by.res]])

    if(value.to.plot %in% c('mean')){
    missing$value_y=rep(min(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }else{
      missing$value_y=rep(max(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }

    missing$hover_x=missing$value_x
    missing$hover_y=rep(NA,dim(missing)[1])

  }else if(length(res.to.plot.x$value_x)==length(res.to.plot.y$value_y)){

  }

  ### merge to one dataset
  if(model.gadm.level > strata.gadm.level){
    res.to.plot.y <- res.to.plot.y[,!colnames(res.to.plot.y)%in% c('region.name','upper.adm.name')] # avoid same names
  }

  res.to.plot.all <- merge(res.to.plot.x, res.to.plot.y, by=by.res)


  ### make the plot
  if(interactive==F){dot.size = 3}else{dot.size=1.5}


  if(length(res.to.plot.x$value_x)==length(res.to.plot.y$value_y)){

    # missingness

    lim <- range(c(res.to.plot.all$value_x, res.to.plot.all$value_y), na.rm = TRUE)
    static.plot <- ggplot2::ggplot(res.to.plot.all, ggplot2::aes(x=value_x,y=value_y)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
      ggplot2::geom_point(alpha = 0.5, color = "royalblue",size=dot.size) +
      ggplot2::labs(title = plot.title)+
      ggplot2::xlab(label.x)+
      ggplot2::ylab(label.y)+
      ggplot2::xlim(lim) +
      ggplot2::ylim(lim) +
      ggplot2::theme_bw()

  }else{
    # no missing

    lim <- range(c(res.to.plot.all$value_x, res.to.plot.all$value_y), na.rm = TRUE)
    static.plot <- ggplot2::ggplot(res.to.plot.all,  ggplot2::aes(x=value_x,y=value_y)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
      ggplot2::geom_point(alpha = 0.5, color = "royalblue",size=dot.size) +
      ggplot2::geom_point(data = missing,  ggplot2::aes(x=value_x,y=value_y), color = "red", shape=17,size=dot.size)+
      ggplot2::labs(title = plot.title)+
      ggplot2::xlab(label.x)+
      ggplot2::ylab(label.y)+
      ggplot2::xlim(lim) +
      ggplot2::ylim(lim) +
      ggplot2::theme_bw()
  }


  ### percentage if coefficient of variation
  if(value.to.plot=='cv'){
    static.plot <- static.plot+
      ggplot2::scale_y_continuous(labels =  scales::percent,limits = lim)+
      ggplot2::scale_x_continuous(labels =  scales::percent,limits = lim)

  }


  if(interactive==F){

    static.plot <- static.plot+ggplot2::coord_fixed(ratio = 1)+
      ggplot2::theme(axis.text= ggplot2::element_text(size=14),
            axis.title= ggplot2::element_text(size=15))



    return(static.plot)
  }

  ### hover effect
  if(pseudo_level ==2){

    if(value.to.plot=='cv'){
      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            'Upper admin name: ',upper.adm.name,
                                                            "</br>",
                                                            label.x,": ", round(100*hover_x,digits = 1),'%',
                                                            "</br>",
                                                            label.y,": ", round(100*hover_y,digits = 1),'%') )

    }else{

      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            'Upper admin name: ',upper.adm.name,
                                                            "</br>",
                                                            label.x,": ", round(hover_x,digits = 3),
                                                            "</br>",
                                                            label.y,": ", round(hover_y,digits = 3)) )
    }


  }else{
    if(value.to.plot=='cv'){

      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            label.x,": ", round(100*hover_x,digits = 1),'%',
                                                            "</br>",
                                                            label.y,": ",  round(100*hover_y,digits = 1),'%') )

    }else{
    static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                          "</br></br>",
                                                          label.x,": ", round(hover_x,digits = 3),
                                                          "</br>",
                                                          label.y,": ", round(hover_y,digits = 3)) )

    }

  }

  ### make the plot interactive

  interactive.plot <- plotly::ggplotly(static.plot,
                                       tooltip = "text",height = 400, width = 500)%>%
    plotly::layout(margin= list(
      l = 80,
      r = 80,
      b = 20,
      t = 20,
      pad = 4
    ))


  return(interactive.plot)

}

### example

if(FALSE){

  tmp.scatter <- scatter.plot( res.obj.x = mdg.ex.model.res$Unit$`Admin-2`,
                            res.obj.y = mdg.ex.model.res$Direct$`Admin-2`,
                            value.to.plot = 'cv',
                            model.gadm.level = 2,
                            strata.gadm.level = 2,
                            label.x = 'M1',
                            label.y = 'M2',
                            plot.title=NULL,
                            interactive=F)





tmp.interval <- surveyPrev::intervalPlot(admin = 2, compare = TRUE, model = list(
  "Direct estimate model"= res1,
  "Fay-Herriot model"= res2,
  "Unstratified Cluster-level model"= res3))




}
