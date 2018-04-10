visualize.make_circle_map <- function(viz){
  deps <- readDepends(viz)
  circle_sp <- deps[["sp_circles"]]
  
  crs <- viz[["visualize_args"]][["crs"]]
  wu_type <- viz[["visualize_args"]][["wu_type"]]
  radius_col_name <- paste0("radius_", wu_type)
  
  # reorder to plot biggest circles in the back
  # need na.last as either T or F or they are dropped!!!
  ordered_i <- sort(circle_sp@data[[radius_col_name]], decreasing = TRUE, 
                    index.return = TRUE, na.last=FALSE)$ix
  circle_sp <- circle_sp[ordered_i,]
  
  # transform map data
  circle_sp_transf <- sp::spTransform(circle_sp, sp::CRS(crs))
  map_data_usa <- sf::st_as_sf(maps::map('usa', fill=TRUE, plot = FALSE))
  map_data_usa <- sf::st_transform(map_data_usa, crs)
  map_data_county <- sf::st_as_sf(maps::map('county', fill=TRUE, plot = FALSE))
  map_data_county <- sf::st_transform(map_data_county, crs)
  
  cols <- color_by_wu_type(wu_type)
  
  png(viz[["location"]], width = 10, height = 6.0, res=450, units = 'in')

  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  plot(sf::st_geometry(map_data_usa), col = "#f1f1f1", border = "white")
  plot(sf::st_geometry(map_data_county), col = NA, border = "white", add=TRUE)
  points(circle_sp_transf, pch=21, 
         col = cols[["outline"]], bg = cols[["fill"]],
         cex = circle_sp_transf@data[[radius_col_name]])
  
  dev.off()
}

visualize.make_pie_map <- function(viz){
  deps <- readDepends(viz)
  circle_sp <- deps[["sp_circles"]]
  rad_multiplier <- 8500 # this is projection-specific. 
  
  crs <- viz[["visualize_args"]][["crs"]]
  wu_type <- viz[["visualize_args"]][["wu_type"]]
  radius_col_name <- paste0("radius_", wu_type)
  
  
  # reorder to plot biggest circles in the back
  # need na.last as either T or F or they are dropped!!!
  ordered_i <- sort(circle_sp@data[[radius_col_name]], decreasing = TRUE, 
                    index.return = TRUE, na.last=FALSE)$ix
  circle_sp <- circle_sp[ordered_i,]
  
  # transform map data
  circle_sp_transf <- sp::spTransform(circle_sp, sp::CRS(crs))
  map_data_usa <- sf::st_as_sf(maps::map('usa', fill=TRUE, plot = FALSE))
  map_data_usa <- sf::st_transform(map_data_usa, crs)
  map_data_state <- sf::st_as_sf(maps::map('state', fill=TRUE, plot = FALSE)) %>% 
    sf::st_transform(crs)
  map_data_county <- sf::st_as_sf(maps::map('county', fill=TRUE, plot = FALSE)) %>% 
    sf::st_transform(crs)
  
  
  png(viz[["location"]], width = 10, height = 6.0, res=450, units = 'in')
  
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  plot(sf::st_geometry(map_data_usa), col = "grey88", border = NA)
  plot(sf::st_geometry(map_data_county), col = NA, border = "grey93", add=TRUE, lwd = 0.5)
  plot(sf::st_geometry(map_data_state), col = NA, border = "white", add=TRUE, lwd = 0.75)

  # start w/ irrigation, fill around the categories (make sure they add up!)
  # note: they don't add up because we aren't including all
  categories <- c("irrigation", "industrial", "thermoelectric", "publicsupply")

  for (j in seq_len(length(circle_sp_transf))){
    # this is in a loop because it is throwaway code:
    orig_ang <- 0
    r <- circle_sp_transf$radius_total[j] *rad_multiplier
    c.x <- coordinates(circle_sp_transf)[j, ][['x']]
    c.y <- coordinates(circle_sp_transf)[j, ][['y']]
    for (cat in categories){
      cat_angle <- circle_sp_transf[[cat]][j] / circle_sp_transf[['total']][j]*2*pi
      if (cat == head(categories, 1L)){
        # start the first category mirror relative to the top
        angle_from <- pi/2 - cat_angle/2
        orig_ang <- angle_from
      } else {
        angle_from <- angle_to
      }
      angle_to <- angle_from + cat_angle
      if (!is.na(cat_angle) & cat_angle > 0.01){
        segments <- make_arc(c.x, c.y, r = r, angle_from, angle_to)
        polygon(c(c.x, segments$x, c.x), c(c.y, segments$y, c.y),
                border = NA,
                col = color_by_wu_type(cat)$fill, lwd=0.25)
        lines(segments$x, segments$y, lwd=0.25, col = color_by_wu_type(cat)$outline)
      }
    }
    if (!is.na(r) & cat == tail(categories, 1L) & angle_to < 2*pi + orig_ang){

      segments <- make_arc(c.x, c.y, r = r, angle_to, 2*pi + orig_ang)
      polygon(c(c.x, segments$x, c.x), c(c.y, segments$y, c.y),
              border = NA,
              col = color_by_wu_type('other')$fill, lwd=0.25)
      lines(segments$x, segments$y, lwd=0.25, col = color_by_wu_type('other')$outline)
    }
    if (circle_sp_transf@data$STATE[j] == "IN" && circle_sp_transf@data$COUNTY[j] == "Lake County"){
      lines_final1 <- make_arc(c.x, c.y, r = r, 0, 2*pi)
    }
    if (circle_sp_transf@data$STATE[j] == "NC" && circle_sp_transf@data$COUNTY[j] == "Macon County"){
      lines_final2 <- make_arc(c.x, c.y, r = r, 0, 2*pi)
    }
  }

  strt_x <- -1990000
  strt_y <- -2000000
  box_w <- 60000
  y_bump <- 30000
  text_st <- 0

  categories <- c('other', rev(categories))
  for (cat in categories){
    polygon(c(strt_x, strt_x+box_w, strt_x+box_w, strt_x, strt_x),
            c(strt_y, strt_y, strt_y+box_w, strt_y+box_w, strt_y),
            col = color_by_wu_type(cat)$fill,
            border = color_by_wu_type(cat)$outline,
            lwd=0.5)
    text(x = strt_x+box_w+text_st, y = strt_y+box_w/2, labels = cat, cex = 1.0, pos = 4)
    strt_y <- strt_y+y_bump+box_w
  }
  
  lines(lines_final1$x, lines_final1$y, col = 'black', lwd=2)
  lines(lines_final2$x, lines_final2$y, col = 'black', lwd=2)
  #plot(sf::st_geometry(filter(map_data_county, ID %in% c("indiana,lake","north carolina,macon"))), col = NA, border = "black", add=TRUE, lwd = 1)
  
  dev.off()
}

make_arc <- function(x0, y0, r, from_angle, to_angle){
  theta <- seq(from_angle, to_angle, by = 0.002)
  x_out <- rep(NA, length.out = length(theta))
  y_out <- rep(NA, length.out = length(theta))
  for (i in 1:length(theta)){
    x_out[i] = x0 + r*cos(theta[i])
    y_out[i] = y0 + r*sin(theta[i])
  }
  return(list(x = x_out, y = y_out))
}
# this will probably end up in CSS somehow?
color_by_wu_type <- function(wu_type) {
  switch(wu_type,
         "total" = list(outline = rgb(36/255, 107/255, 136/255),
                        fill = rgb(46/255, 134/255, 171/255, 0.8)),
         "thermoelectric" = list(outline = rgb(237, 201, 72, maxColorValue = 255),
                                 fill = rgb(237, 201, 72, alpha = 204, maxColorValue = 255)),
         "publicsupply" = list(outline = rgb(118, 183, 178, maxColorValue = 255),
                               fill = rgb(118, 183, 178, alpha = 204, maxColorValue = 255)),
         "irrigation" = list(outline = rgb(89, 161, 79, maxColorValue = 255),
                             fill = rgb(89, 161, 79, alpha = 204, maxColorValue = 255)),
         "industrial" = list(outline = rgb(225, 87, 89, maxColorValue = 255),
                             fill = rgb(225, 87, 89, alpha = 204, maxColorValue = 255)),
         "other" = list(outline = rgb(169, 169, 169, maxColorValue = 255),
                             fill = rgb(169, 169, 169, alpha = 180, maxColorValue = 255)))
}
