

options(error = recover)

packages <- c("eurostat", "dplyr", "tidyr", "tibble", "ggplot2", "readxl", "stringr",
              "png", "grid", "gridExtra", "forcats", "spdep", "data.table",
              "purrr", "readxl", "gplots", "reshape2", "Matrix",
              "geosphere", "viridis", "Matrix", "scales", "profvis", #viridis only scale color I found to work with geom_point, TODO to fix
              "ggthemes", "paletteer", "RColorBrewer", "hexbin",
              "stringr", "boot", "ggExtra", "oompaBase",
              "R.matlab", "igraph", "stars", "moments")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

library(INLA,  lib="/export/home/mrogoopa/R/x86_64-pc-linux-gnu-library/4.0")

# source("general.R")
source("functions_plotting.R")
# source("functions_sentiment_analysis.R")







table0 <- function(x) {
  table(x, useNA="ifany")
}









geo_to_neighs <- function(map, use_snap=TRUE, use_queen=TRUE) {
  if (!"geo" %in% names(map)) {
    
    if ("sfc" %in% class(map)) {
      warning("no geo column")
      
      map_geometry <- map
      map <- tibble(geo=1:length(map_geometry), geometry=map_geometry)
    } else if ("sfc" %in% class(map$geometry)) {
      warning("no geo column")
      
      map <- map %>% dplyr::select(geometry) %>% mutate(geo=1:nrow(map))
    } else {
      stop("no valid geometry column")
    }
  }
  
  n_geo <- map$geo %>% unique() %>% length()
  if (n_geo != nrow(map))
    stop('duplicate geometries in map')
  shapes <- as_Spatial(map$geometry)
  # nb_overlap <- st_overlaps(map$geometry, sparse = FALSE)
  
  if (use_snap) {
    nb_map <- poly2nb(shapes, snap=sqrt(.Machine$double.eps)*5e5,
                      queen = use_queen) 
  } else {
    nb_map <- poly2nb(shapes, queen = use_queen) 
  }
  coords <- coordinates(shapes)
  
  return(list(geo = map$geo,
              shapes = shapes,
              nb_map = nb_map,
              n_geo = n_geo,
              coords = coords))
}







plot_geo_neighs <- function(geo_neigh, filename = "", selection_nodes = NULL) {
  coords_sp <- SpatialPoints(geo_neigh$coords)
  
  if (filename != "")
    pdf(filename)
  plot(geo_neigh$shapes, border = gray(.5)) 
  plot(geo_neigh$nb_map, geo_neigh$coords, add = TRUE) 
  plot(coords_sp, add = TRUE, col = "red", pch = 20, cex = 0.75) 
  if (!is.null(selection_nodes))
    plot(coords_sp[selection_nodes], add = TRUE, col = "green", pch = 20, cex = 0.75)
  if (filename != "")
    dev.off()
}







plot_table <- function(stats, feature1, feature2, rescale = FALSE,
                       title = "") {
  table_features <- as.data.frame(table(stats[[feature1]], stats[[feature2]]))
  colnames(table_features)[1:2] <- c(feature1, feature2)
  
  if (nrow(table_features) == 4) {
    str_conditional <- 
      sprintf(paste0("feat2=0: %.2f, ",
                     "feat2=1: %.2f, ",
                     "feat1=0: %.2f, ",
                     "feat1=1: %.2f"),
              table_features$Freq[2] / sum(table_features$Freq[1:2]),
              table_features$Freq[4] / sum(table_features$Freq[3:4]),
              table_features$Freq[3] / sum(table_features$Freq[c(1,3)]),
              table_features$Freq[4] / sum(table_features$Freq[c(2,4)]))
  } else {
    str_conditional <- "nrow < 4"
  }
  
  if (!rescale) {
    
    p <- ggplot(table_features, 
                aes_string(x = feature1, y = "Freq", fill = feature2)) + 
      labs(title = paste0(title, "_", str_conditional)) + 
      geom_bar(stat="identity", colour="black")
  } else if (nrow(table_features) == 4) {
    
    table_features <- table_features %>% arrange(!!sym(feature1))
    
    table_feature1 <- c(sum(table_features$Freq[1:2]), 
                        sum(table_features$Freq[3:4]))
    if (table_feature1[2] < table_feature1[1]) {
      bar_width_scaling <- table_feature1[2] / table_feature1[1]
      bar_height_scaling <- c(1, 1 /  bar_width_scaling)
      width <- c(1, 1, bar_width_scaling, bar_width_scaling)
    } else {
      bar_width_scaling <- table_feature1[1] / table_feature1[2]
      bar_height_scaling <- c(1 /  bar_width_scaling, 1)
      width <- c(bar_width_scaling, bar_width_scaling, 1, 1)
    }
    table_features_resc <- table_features
    table_features_resc[1:2,]$Freq <- table_features_resc[1:2,]$Freq * 
      bar_height_scaling[1]
    table_features_resc[3:4,]$Freq <- table_features_resc[3:4,]$Freq * 
      bar_height_scaling[2]
    
    p <- 
      ggplot(table_features_resc, 
             aes_string(x = feature1, y = "Freq", fill = feature2,
                        width = 2 * width)) + 
      labs(title = paste0(title, "_rescaled_", str_conditional)) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) + 
      geom_bar(stat="identity", colour="black")
  } else {
    p <- plot_scatter(data.frame(x = 1, y = 1), 
                      x_column = "x", y_column = "y")
  }
  
  return(p)
}








# Save plots to pdf -------------------------------------------------------





#pdf_scaled: save plots to pdf with high resolution
pdf_scaled <- function(file,
                       pdf_sc = NULL) {
  if (!is_null(pdf_sc) && !(pdf_sc < 10 && pdf_sc > 0)) 
    stop("pdf_sc should lie between 0 and 10")
  
  basic_scaling <- list(width = 15, height = 10, psize = 20)
  if (is_null(pdf_sc)) {
    pdf_sc <- basic_scaling
  } else if(is.numeric(pdf_sc)) {
    pdf_sc <- lapply(basic_scaling, function(x) x * pdf_sc)
  }
  pdf(file = file, 
      width = pdf_sc$width,
      height = pdf_sc$height,
      pointsize = pdf_sc$psize)
}







save_to_pdf_by_groups <- function(ps, file_pdf, groups) {
  list_ps <- list()
  for (j in seq_along(groups)) {
    ids <- groups[[j]]
    list_ps[[j]] <- list(figure = ps[ids], is_single_figure = length(ids)==1)
  }
  
  save_to_pdf_via_png(list_ps, file_pdf)
}





save_to_excel <- function(tb, file_excel, n_round=NULL) {
  if (!is.null(n_round)) {
    tb <- tb %>% mutate(across(where(is.numeric), ~ round(.x, n_round)))
  }
  write.xlsx(tb, file=file_excel, append=FALSE)
}










save_to_pdf_via_png <- function(ps, file_pdf) {
  timestamp <- sprintf("timestamp_%.0f", proc.time() %>% `[[`(3) * 10)
  file_png <- paste0("Output/Plots/png/png_for_pdf_", timestamp, ".png")
  
  png_device <- which(names(dev.list())  == "png") %>% 
    (function(i) {dev.list()[i]})
  if (!is.null(png_device))
    sapply(png_device, function(i) dev.off(i))
  
  pdf_scaled(file_pdf)
  for (j in 1:length(ps)) {
    cat(sprintf("Saving plot %d (of %d)\n", j, length(ps)))
    
    n_figures <- length(ps[[j]]$figure)
    if (n_figures < 10) {
      png(file_png, width=1200, height=800)
    } else {
      png(file_png, width=1200*1.5, height=800*1.5)
    }
    
    if (ps[[j]]$is_single_figure) {
      ps[[j]]$figure %>% print()
    } else {
      if (n_figures == 3) {
        n_col <- 2
      } else {
        n_col <- floor(sqrt(n_figures))
      }
      do.call(grid.arrange, c(ps[[j]]$figure, ncol = n_col, as.table = FALSE)) %>% print()
    }
    
    png_device <- which(names(dev.list())  == "png") %>% 
      (function(i) {dev.list()[i]})
    sapply(png_device, function(i) dev.off(i))
    
    r <- rasterGrob(readPNG(file_png, native = FALSE),
                    interpolate = FALSE)
    grid.arrange(r)
  }
  dev.off()
}





save_to_pdf_simple <- function(ps, file_pdf, is_single_figure = TRUE) {
  list_ps <- list()
  for (j in seq_along(ps)) {
    list_ps[[j]] <- list(figure = ps[[j]], is_single_figure = is_single_figure)
  }
  
  save_to_pdf_via_png(list_ps, file_pdf)
}
save_to_pdf_by_groups <- function(ps, file_pdf, groups) {
  list_ps <- list()
  for (j in seq_along(groups)) {
    ids <- groups[[j]]
    list_ps[[j]] <- list(figure = ps[ids], is_single_figure = length(ids)==1)
  }
  
  save_to_pdf_via_png(list_ps, file_pdf)
}



save_to_pdf_via_png0 <- function(step = NULL, options, inner_step = NULL,
                                 resolution_scaling = 1) {
  #input, step 1: options$file_pdf, step 2-3: options from step 1
  png_device <- which(names(dev.list())  == "png") %>% 
    (function(i) {dev.list()[i]})
  if (!is.null(png_device))
    sapply(png_device, function(i) dev.off(i))
  
  if (!is.null(step)) {
    if (sum(names(dev.list()) == "pdf") > 1)
      warning("use inner_step if writing to several pdfs in parallel")
    if (step == 1) { #initialize, before first plot
      inner_steps <- 1:2
    } else if (step == 2) { #between plots
      inner_steps <- c(3,2)
    } else if (step == 3) { #after last plot
      inner_steps <- 3:4
    }
  } else if (!is.null(inner_step)) {
    inner_steps <- inner_step
  } else {
    stop("step error")
  }
  
  for (inner_step in inner_steps) {
    if (inner_step == 1) {
      
      #---Setup
      current_dev <- dev.list()
      pdf_scaled(options$file_pdf)
      new_dev <- dev.list()
      options$dev_nr <- setdiff(new_dev, current_dev)
      
      timestamp <- sprintf("timestamp_%.0f", proc.time() %>% `[[`(3) * 10)
      options$file_png <- paste0("Output/Plots/png/png_for_pdf_", timestamp, ".png")
    } else if (inner_step == 2) {
      
      #---Open png connection
      png(options$file_png, 
          width=1200 * resolution_scaling, 
          height=800 * resolution_scaling) #res = ...
      
    } else if (inner_step == 3) {
      
      #---Load png (print figures to png connection between step 2 and step 3)
      r <- rasterGrob(readPNG(options$file_png, native = FALSE),
                      interpolate = FALSE)
      
      dev.set(options$dev_nr)
      grid.arrange(r)
    } else if (inner_step == 4) {
      
      #---Close connection
      dev.off(options$dev_nr)
    }
  }
  
  # options <- list(file_pdf = file_pdf)
  # options <- save_to_pdf_via_png0(step = 1, options)
  # plot(movav_acc_u)
  # options <- save_to_pdf_via_png0(step = 2, options)
  # hist(intercept[-burnin])
  # options <- save_to_pdf_via_png0(step = 2, options)
  # plot(log(tau[-burnin] + 1e-16), intercept[-burnin])
  # options <- save_to_pdf_via_png0(step = 2, options)
  # plot(log(tau[-burnin] + 1e-16), uQu[-burnin])
  # options <- save_to_pdf_via_png0(step = 3, options)
  
  return(options)
}







record_info_to_text_file <- function(file, str_info) {
  cat(str_info)
  write(str_info, file = file, append = TRUE)
}







rasterize_geometry <- function(geometry, n = 100) {
  r <- st_sf(geometry) %>% 
    st_rasterize(st_as_stars(st_bbox(geometry), nx = n, ny = n, 
                             values = NA_real_))
  r <- as.data.frame(r)
  r <- r %>% filter(ID == TRUE)
  
  return(r)
}







compute_intersections_basic <- function(map) {
  # if (!identical(map$geo_idx, 1:nrow(map)))
  # warning("geo_idx error")
  
  ind_intersections <- st_intersects(map$geometry)
  
  intersection_num <- ind_intersections %>% lapply(length) %>% unlist()
  cat("intersections table:\n")
  intersection_num %>% table() %>% print()
  intersects_mult_ind <- which(intersection_num != 1)
  
  cat("\nintersections plot:\n")
  plot_sf(map %>% 
            add_column(log_intersection_num = log(intersection_num)),
          feature = "log_intersection_num") %>% print()
  
  return(list(intersection_inds = ind_intersections,
              intersects_mult_ind = intersects_mult_ind,
              intersection_num = intersection_num))
}







compute_intersection_geometries <- function(map, list_intersections_basic) {
  # if (!identical(map$geo_idx, 1:nrow(map)))
  # stop("geo_idx error")
  
  intersection_inds <- list_intersections_basic$intersection_inds
  intersects_mult_ind <- list_intersections_basic$intersects_mult_ind
  intersection_num <- list_intersections_basic$intersection_num
  n_geo <- nrow(map)
  # if (length(unique(map$geo)) != n_geo)
  # stop("map, geo error")
  
  n_intersections_ulim <- sum(intersection_num) - n_geo #not using intersection of region with itself
  
  tb_intersections <- 
    compute_intersection_geometries_inner(map, intersection_inds, 
                                          n_intersections_ulim, n_geo)
  
  if (!is.null(tb_intersections$geometry[[1]])) {
    shapes <- as_Spatial(tb_intersections$geometry)
    coords <- coordinates(shapes)
    tb_intersections <- tb_intersections %>% 
      add_column(longitude = coords[,1], latitude = coords[,2])
  }
  
  return(tb_intersections)
}







compute_intersection_geometries_inner <- function(map, intersection_inds, 
                                                  n_intersect_ulim, n_geo) {
  tb_intersections <- tibble(id = 1:n_intersect_ulim, 
                             parent = NA, neighbour = NA,
                             geometry_list = 
                               vector(mode = "list", length = n_intersect_ulim),
                             geometry = 
                               vector(mode = "list", length = n_intersect_ulim),
                             area = NA,
                             rel_area = NA)
  
  count <- 0
  for (j in 1:n_geo) {
    inds <- intersection_inds[[j]]
    for (k in inds) {
      if (k == j)
        next
      
      intersection <- st_intersection(map$geometry[j], 
                                      map$geometry[k])
      if (!is_empty(st_is_empty(intersection))) {
        if (!st_is_empty(intersection) &&
            as.numeric(st_area(intersection)) > 0) {
          if (length(intersection) != 1)
            stop("intersection length error")
          count <- count + 1
          tb_intersections$parent[count] <- map$geo_idx[j]
          tb_intersections$neighbour[count] <- map$geo_idx[k]
          tb_intersections$geometry_list[[count]] <-
            intersection
          tb_intersections$area[count] <- st_area(intersection)
          tb_intersections$rel_area[count] <- 
            as.numeric(tb_intersections$area[count] / st_area(map$geometry[j]))
          cat(paste0(sprintf("Intersection area: %.2f\n", 
                             tb_intersections$area[count])))
          if (count == 1) {
            geometry_sf <- st_sf(intersection)
          } else {
            geometry_sf <- rbind(geometry_sf, st_sf(intersection))
          }
        }
      }
    }
  }
  tb_intersections <- tb_intersections[1:count,]
  if (count > 0) {
    tb_intersections$geometry <- geometry_sf$intersection
  }
  
  return(tb_intersections)
}








