# Plotting --------------------------------------------------------------



color_palette <- function(palette_idx = 1, 
                          is_continuous = TRUE,
                          n_discrete = 10,
                          invert_colors = FALSE,
                          cap_colors = NULL,
                          color_range = NULL,
                          color_limits = NULL,
                          transformation = NULL,
                          manual_palette = NULL,
                          replace_first_color = NULL, 
                          replace_last_color = NULL) {
  
  if (is.null(manual_palette)) {
    col_palette <- color_palette_inner(palette_idx, n_discrete, is_continuous=is_continuous,
                                       replace_first_color = replace_first_color, 
                                       replace_last_color = replace_last_color)
  } else {
    col_palette <- manual_palette
  }
  
  if (invert_colors) {
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  }
  
  if (!is.null(cap_colors[1])) {
    if (is.numeric(cap_colors) && length(cap_colors) == 2) {
      n <- length(col_palette)
      lims <- c(round(n * cap_colors[1]), round(n * cap_colors[2]))
      col_palette <- col_palette[lims[1]:lims[2]] 
    } else if (cap_colors == "from_below")  {
      cap_colors = c(0.25,1)
      n <- length(col_palette)
      lims <- c(round(n * cap_colors[1]), round(n * cap_colors[2]))
      col_palette <- col_palette[lims[1]:lims[2]]
    } else if (cap_colors == "from_above") {
      cap_colors = c(0,0.75)
      n <- length(col_palette)
      lims <- c(round(n * cap_colors[1]), round(n * cap_colors[2]))
      col_palette <- col_palette[lims[1]:lims[2]]
    } else {
      stop("cap_color error")
    }
  }
  
  if (!is.null(color_range[1])) {
    if (is.numeric(color_range) && length(color_range) == 2) {
      n <- length(col_palette)
      len <- color_range[2] - color_range[1]
      n_ext <- ceiling(n / len)
      lower_lim <- round(n_ext * color_range[1])
      upper_lim <- (lower_lim + n - 1)
      col_pal_ext <- rep(0, n_ext)
      col_pal_ext[lower_lim:upper_lim] <- col_palette
      if (lower_lim > 1) {
        col_pal_ext[1:(lower_lim-1)] <- col_palette[1]
      } 
      if (upper_lim < n_ext) {
        col_pal_ext[(upper_lim+1):n_ext] <- col_palette[n]
      }
      
      col_palette <- col_pal_ext
      
    } else {
      stop("color_range error")
    }
  }
  
  if (is_continuous) {
    if (is.null(color_limits)) {
      if (!is.null(transformation)) {
        scale_fill <- scale_fill_gradientn(colours = col_palette, trans = transformation)
        scale_col <- scale_color_gradientn(colours = col_palette, trans = transformation)
      } else {
        scale_fill <- scale_fill_gradientn(colours = col_palette)
        scale_col <- scale_color_gradientn(colours = col_palette)
      }
    } else {
      if (is.numeric(color_limits) && length(color_limits) == 2) {
        scale_fill <- scale_fill_gradientn(colours = col_palette, 
                                           limits = color_limits)
        scale_col <- scale_color_gradientn(colours = col_palette,
                                           limits = color_limits)
      } else {
        stop("color_limits error")
      }
    }
  } else {
    col_palette <- colorRampPalette(col_palette)(n_discrete)
    if (n_discrete == 3 && palette_idx == 1 && is.null(manual_palette)) {
      col_palette[2] <- "#abab1a"
    } else if (n_discrete == 3 && palette_idx == 4) {
      col_palette[2] <- "#82827a"
    }
    scale_fill <- scale_fill_manual(values = col_palette)
    scale_col <- scale_color_manual(values = col_palette)
  }
  
  # g <- ggplot_build(p)
  # g$data[[1]]$colour %>% unique()
  
  return(list(scale = list(fill = scale_fill, 
                           col = scale_col), 
              col_palette = col_palette))
}
color_palette_inner <- function(palette_idx, n_discrete=10, 
                                replace_first_color = NULL, 
                                replace_last_color = NULL,
                                is_continuous=TRUE) {
  if (palette_idx == 1) {
    col_palette <- brewer.pal(11, "RdYlBu")
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 100) {
    col_palette <- brewer.pal(11, "RdYlBu")
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
    col_palette <- col_palette[-(5:7)]
  } else if (palette_idx == 2) {
    if (is_continuous) {
      col_palette <- jetColors(500)
    } else {
      col_palette <- jetColors(n_discrete)
    }
  } else if (palette_idx == 200) {
    if (is_continuous) {
      col_palette <- jetColors(500)
      col_palette <- col_palette[-c(175:239,261:325)]
    } else {
      col_palette <- jetColors(n_discrete)
    }
  } else if (palette_idx == 3) {
    if (is_continuous) {
      colfunc <- jcolors_contin("rainbow")
      col_palette <- colfunc(500)
    } else {
      col_palette <- jcolors("rainbow")
    }
  } else if (palette_idx == 300) {
    if (is_continuous) {
      colfunc <- jcolors_contin("rainbow")
      col_palette <- colfunc(500)
      col_palette <- col_palette[-(400:500)]
    } else {
      col_palette <- jcolors("rainbow")
    }
  } else if (palette_idx == 301) {
    if (is_continuous) {
      colfunc <- jcolors_contin("rainbow")
      col_palette <- colfunc(500)
      col_palette <- col_palette[-(400:500)]
    } else {
      col_palette <- jcolors("rainbow")
    }
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 4) {
    col_palette <- brewer.pal(11, "RdYlBu")
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 5) {
    col_palette <- paletteer_c("grDevices::Blues 3", 30)
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 6) {
    col_palette <- paletteer_c("grDevices::Blues 3", 30)
  } else if (palette_idx == 7) {
    col_palette <- paletteer_c("grDevices::Blues 3", 30)
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
    col_palette <- col_palette[9:length(col_palette)]
  } else if (palette_idx == 8) {
    col_palette <- paletteer_c("grDevices::Blues 3", 30)
    col_palette <- col_palette[1:(length(col_palette)-7)]
  } else if (palette_idx == 9) {
    col_palette <- paletteer_c("grDevices::Inferno", 30)
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 10) {
    col_palette <- brewer.pal(12, "Paired")
    if (!is_continuous) {
      col_palette <- rep(col_palette, length.out = n_discrete)
    }
  } else if (palette_idx == 11) {
    col_palette <- paletteer_d("colorBlindness::PairedColor12Steps")
    if (!is_continuous) {
      col_palette <- rep(col_palette, length.out = n_discrete)
    }
  }  else if (palette_idx == 12) {
    col_palette <- paletteer_d("colorBlindness::SteppedSequential5Steps")
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
    if (!is_continuous) {
      col_palette <- rep(col_palette, length.out = n_discrete)
    }
  } else if (palette_idx == 13) {
    col_palette <- paletteer_c("grDevices::Berlin", 30)
    col_palette <- col_palette[-c(10:14,16:20)]
  } else if (palette_idx == 14) {
    if (!is_continuous) {
      n <- n_discrete
      if (n>7) {
        n <- n+2
        col_palette <- pals::ocean.balance(n)
        col_palette <- col_palette[2:(n-1)]
      } else {
        col_palette <- pals::ocean.balance(n)
      }
    } else {
      n <- 30
      n <- n+4
      col_palette <- pals::ocean.balance(n)
      col_palette <- col_palette[3:(n-2)]
    }
  } else if (palette_idx == 140) {
    if (!is_continuous) {
      if (n_discrete<5) {
        n <- n_discrete
        n <- n+2
        col_palette <- pals::ocean.balance(n)
        col_palette <- col_palette[2:(n-1)]
      } else {
        n <- n_discrete
        n <- n+4
        col_palette <- pals::ocean.balance(n)
        col_palette <- col_palette[3:(n-2)]
      }
    } else {
      n <- 30
      n <- n+6
      col_palette <- pals::ocean.balance(n)
      col_palette <- col_palette[(4:n)-3]
    }
  } else if (palette_idx == 15) {
    col_palette <- paletteer_c("grDevices::Grays", 30)
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 16) {
    col_palette <- paletteer_c("grDevices::Light Grays", 30)
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 17) {
    col_palette <- brewer.pal(12, "Set3")
    if (!is_continuous) {
      col_palette <- rep(col_palette, length.out = n_discrete)
    }
  } else if (palette_idx == 18) {
    # col_palette <- color_palette_stepped(n_colors=n_discrete-1, n_steps=1)
    # col_palette <- c("#000000", col_palette)
    col_palette <- c("#000000", "#47A0B2", "#A87630", "#707070", "#B28547", "#3D0F99", "#54990F")
    col_palette <- col_palette[1:n_discrete]
  } else if (palette_idx == 19) {
    col_palette <- watlington(n_discrete)
  } else if (palette_idx == 20) {
    col_palette <- pals::ocean.amp(n_discrete+3)
    col_palette <- col_palette[2:(n_discrete+1)]
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 21) {
    col_palette <- pals::ocean.turbid(n_discrete+3)
    col_palette <- col_palette[2:(n_discrete+1)]
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  } else if (palette_idx == 22) {
    col_palette <- pals::ocean.matter(n_discrete+3)
    col_palette <- col_palette[2:(n_discrete+1)]
    col_palette <- col_palette[seq(length(col_palette),1,-1)]
  }
  # col_palette %>% print()
  
  # replace_first_color <- "gray50"
  if (!is.null(replace_first_color)) {
    col_palette <- add_color_to_colormap(col_palette, replace_first_color, "first")
  }
  if (!is.null(replace_last_color)) {
    col_palette <- add_color_to_colormap(col_palette, replace_last_color, "last")
    
  }
  
  return(col_palette)
}
color_palette_stepped <- function(n_colors=2, n_steps=4, palette_idx=1) {
  if (palette_idx==1) {
    palette_fun <- stepped
  } else if (palette_idx==2) {
    palette_fun <- stepped2
  } else if (palette_idx==3) {
    palette_fun <- stepped3
  }
  
  cols <- palette_fun(n_colors*4)
  idx <- 0:(n_colors-1)*4
  cols_left <- idx+1
  cols_right <- idx+4
  col_palette <- NULL
  for (j in 1:n_colors) {
    cols_curr <- cols[c(cols_left[j], cols_right[j])]
    col_palette <- c(col_palette, colorRampPalette(cols_curr)(n_steps))
  }
  
  return(col_palette)
}
add_color_to_colormap <- function(col_palette, color_to_add, position="last") {
  col_rgb <- col2rgb(color_to_add)
  col_hex <- rgb(col_rgb[1], col_rgb[2], col_rgb[3], maxColorValue=255)
  if (position=="last") {
    col_palette <- c(col_palette, col_hex)
  } else if (position=="first") {
    col_palette <- c(col_hex, col_palette)
  } else {
    stop("not implemented")
  }
  
  return(col_palette)
}
color_palette_manual_by_idx <- function(manual_palette_idx, n_discrete,
                                        reorder=NULL) {
  if (manual_palette_idx == 14) {
    if (n_discrete != 3)
      stop("this color palette is only for n_discrete==3")
    
    col_palette_manual <- color_palette_inner(palette_idx=14, n_discrete=25, is_continuous=FALSE)
    # col_palette_manual <- col_palette_manual[3]
    col_palette_manual <- col_palette_manual[5]
    col_palette_manual <- add_color_to_colormap(col_palette_manual, "gray60", "first")
    col_palette_manual <- add_color_to_colormap(col_palette_manual, "gray15", "first")
  }
  
  if (!is.null(reorder))
    col_palette_manual <- col_palette_manual[reorder]
  
  col_pal <- color_palette(manual_palette=col_palette_manual, n_discrete=n_discrete,
                           is_continuous=FALSE)
  
  return(col_pal)
}







plot_sf_facet <- function(map, feature_column, facet_column, title="") {
  if (title != "") {
    title <- paste0(title, ": ")
  }
  
  ps <- list()
  
  unique_facet_vals <- unique(map[[facet_column]])
  count <- 0
  for (val in unique_facet_vals) {
    map_curr <- map %>% filter(!!sym(facet_column) == val)
    
    count <- count + 1
    ps[[count]] <- plot_sf(map_curr, feature_column, 
                           title = paste0(title, facet_column, " = ", val))
  }
  
  return(ps)
}







plot_sf0 <- function(map, column, title="") {
  plot_sf(map %>% add_column(vals = column), feature = "vals", title = title)
}








# background_color="aliceblue", edge_color="white", opacity=0.4, feature_title_length=30, palette_idx=1
# background_color="white", edge_color = "black", opacity=1, feature_title_length = 9, palette_idx=14
plot_sf <- function(map, feature, title = "", subtitle = "",
                    opacity = 1,
                    xlim = NA, ylim = NA,
                    palette_idx = 14,
                    invert_colors = FALSE,
                    cap_colors = NULL,
                    color_limits = NULL,
                    color_breaks = NULL,
                    color_transformation = NULL,
                    feature_title_length = 30,
                    background_color = "white",
                    edge_color = "black",
                    plot_larger = FALSE,
                    theme_paper = TRUE,
                    manual_palette = NULL) {
  if (plot_larger) {
    feature_title_length = 9
  }
  
  if (is.character(opacity)) {
    geom_sf_ <- geom_sf(color = alpha(edge_color,1/3), #edges, use NA for no edges
                        aes(alpha = !!sym(opacity))) #fill
  } else if (is.double(opacity)) {
    geom_sf_ <- geom_sf(color = alpha(edge_color,1/3), #edges
                        alpha = opacity) #fill
  } else {
    stop("invalid opacity value")
  }
  if (any(is.na(xlim))) {
    lims <- compute_boundary_of_sf(map, compute_x = TRUE, compute_y = FALSE)
    xlim <- lims$xlim
  }
  if (any(is.na(ylim))) {
    lims <- compute_boundary_of_sf(map, compute_x = FALSE, compute_y = TRUE)
    ylim <- lims$ylim
  }
  
  cat(paste0("plotting feature: ", feature, "\n"))
  
  title_feature <- feature
  len <- nchar(title_feature)
  if (len > feature_title_length) {
    title_feature <- substr(title_feature, 1, feature_title_length)
  } else if (len < feature_title_length) {
    title_feature <- paste0(title_feature, strrep(" ", feature_title_length-len))
  }
  if (subtitle == "") {
    plot_labs <- labs(title = title, fill = title_feature)
  } else {
    plot_labs <- labs(title = title, subtitle = subtitle,
                      fill = title_feature)
  }
  
  n_cols <- length(setdiff(unique(map[[feature]]), NA))
  if (n_cols <= 3) {
    n_discrete <- n_cols
    map[[feature]] <- map[[feature]] %>% as.factor()
  } else {
    n_discrete <- NA
  }
  if (is.factor(map[[feature]]) || is.logical(map[[feature]]) ||
      is.character(map[[feature]])) {
    is_continuous <- FALSE
    if (n_cols <= 25) {
      n_discrete <- n_cols
      map[[feature]] <- map[[feature]] %>% as.factor()
    }
  } else {
    is_continuous <-TRUE
  }
  
  if (is.null(manual_palette)) {
    cols <- color_palette(palette_idx, 
                          invert_colors = invert_colors,
                          n_discrete = n_discrete,
                          is_continuous = is_continuous,
                          color_limits = color_limits,
                          transformation = color_transformation)
  } else {
    cols <- manual_palette
  }
  col <- cols$scale$fill
  
  # if (is.logical(map[[feature]]))
  #   map[[feature]] <- as.double(map[[feature]])
  p <- ggplot(map, aes(fill = !!sym(feature), geometry = geometry)) +
    col +
    geom_sf_ +
    xlim(xlim) + ylim(ylim) +
    plot_labs +
    theme(panel.background = element_rect(fill = background_color)) +
    theme(plot.title = element_text(size=10))
  
  if (theme_paper) {
    p <- p + theme_bw()
  }
  
  if (plot_larger) {
    p <- p + theme(legend.key.width = unit(1.25, "cm"),
                   legend.key.heigh = unit(1.5, "cm")) + 
      theme(text = element_text(size = 24))
  }
  
  return(p)
}


compute_boundary_of_sf <- function(map, compute_x = TRUE, compute_y = TRUE) {
  if (compute_x) {
    xmin <- Inf; xmax <- -Inf
    for (j in seq_along(map$geometry)) {
      b <- st_bbox(map[[j,"geometry"]])
      xmin <- min(xmin, b$xmin)
      xmax <- max(xmax, b$xmax)
    }
    xlim <- c(xmin, xmax)
  } else {
    xlim <- c(NaN, NaN)
  }
  
  if (compute_y) {
    ymin <- Inf; ymax <- -Inf
    for (j in seq_along(map$geometry)) {
      b <- st_bbox(map[[j,"geometry"]])
      ymin <- min(ymin, b$ymin)
      ymax <- max(ymax, b$ymax)
    }
    ylim <- c(ymin, ymax)
  } else {
    ylim <- c(NaN, NaN)
  }
  return(list(xlim = xlim, ylim = ylim))
}








# palette_idx=4, nchar_legend_title=20
# palette_idx=14, nchar_legend_title=9
plot_scatter_lines <- function(tb, x_column, y_column, 
                               color_column = 1,
                               facet_column = NULL,
                               size_column = NULL,
                               x_segment_columns = NULL,
                               y_segment_columns = NULL,
                               plot_scatter = TRUE,
                               plot_lines = TRUE,
                               plot_smooth = FALSE,
                               plot_ribbon = FALSE,
                               plot_contours = FALSE,
                               plot_segments = FALSE,
                               plot_marginals = FALSE,
                               plot_diagonal = FALSE,
                               plot_constant_lines_y = NULL,
                               plot_constant_lines_x = NULL,
                               n_cutoff = 0,
                               n_contours = 10,
                               n_marginal_bins = 100,
                               smooth_span = NULL, #0.1
                               smooth_standard_span = NULL,
                               add_segment_arrows = FALSE,
                               point_size = 4,
                               line_size = 1,
                               palette_idx = 14,
                               manual_palette = NULL,
                               opacity = 0.8, #0.8
                               invert_colors = 1,
                               cap_colors = TRUE,
                               facet_free_x = FALSE, facet_free_y = FALSE,
                               logx = FALSE, logy = FALSE,
                               log_transform = FALSE,
                               color_transformation = NULL,
                               xlim = NULL, ylim = NULL,
                               title = "",
                               title_size = 15,
                               nchar_legend_title = 20,
                               n_legend_cutoff = 30,
                               is_continuous = NULL,
                               coord_flip = FALSE,
                               aspect_ratio = NULL,
                               plot_larger = FALSE,
                               replace_first_color = NULL,
                               replace_last_color = NULL) {
  if (plot_larger) {
    nchar_legend_title <- 9
  }
  #---Setup
  if (nrow(tb) > n_cutoff && n_cutoff != 0)
    warning("nrow(tb) > n_cutoff")
  
  if (!is.null(plot_constant_lines_y) || !is.null(plot_constant_lines_x)) {
    plot_constant_line <- TRUE
  }
  
  if (log_transform) { 
    if (logx && min(tb[[x_column]]) <= 0) {
      tb[[x_column]] <- apply(cbind(tb[[x_column]],0), 1, max) + 0.5
      title <- paste0(title, ", log transf. x")
      transformed_x <- TRUE
      cat("some x-values are <= 0, transforming for log scale\n")
    } else {
      transformed_x <- FALSE
    }
    if (logy && min(tb[[y_column]]) <= 0) {
      tb[[y_column]] <- apply(cbind(tb[[y_column]],0), 1, max) + 0.5
      title <- paste0(title, ", log transf. y")
      transformed_y <- TRUE
      cat("some y-values are <= 0, transforming for log scale\n")
    } else {
      transformed_y <- FALSE
    }
    if (transformed_x || transformed_y) {
      if (plot_diagonal) {
        cat(paste0("plot_diagonal is incompatible with log_transform", 
                   ", setting plot_diagonal to FALSE\n"))
        plot_diagonal <- FALSE
      }
      if (!is.null(plot_constant_line)) {
        cat(paste0("plot_constant_line is incompatible with log_transform", 
                   ", setting plot_constant_line to FALSE\n"))
        plot_constant_line <- NULL
      }
      
    }
  }
  
  if (!is.null(color_column)) {
    color <- color_column
    if (length(unique(tb[[color_column]])) == 1) {
      tb[[color_column]] <- tb[[color_column]] %>% as.factor()
    }
    
    if (is.null(is_continuous)) {
      if (is.factor(tb[[color_column]]) || is.logical(tb[[color_column]]) ||
          is.character(tb[[color_column]])) {
        is_continuous = FALSE
      } else {
        is_continuous = TRUE
      }
    } else if (!is_continuous && 
               !(is.factor(tb[[color_column]]) || is.logical(tb[[color_column]]) ||
                 is.character(tb[[color_column]]))) {
      warning("turning column into factor column")
      
      tb[[color_column]] <- as.factor(tb[[color_column]])
    }
    
    title_legend <- color_column
    n <- nchar(title_legend)
    if (n < nchar_legend_title) {
      str_spaces <- strrep(" ", nchar_legend_title - n)
      title_legend <- paste0(title_legend, str_spaces)
    } else if (n > nchar_legend_title) {
      title_legend <- substr(title_legend, 1, nchar_legend_title)
    }
  }
  
  if (plot_contours) {
    if (is.factor(tb[[x_column]])) {
      stop("x is a factor, not compatible with contours")
      plot_contours <- FALSE
    }
    if (is.factor(tb[[y_column]])) {
      stop("y is a factor, not compatible with contours")
      plot_contours <- FALSE
    }
    
    #geom_density_2d doesn't work if quantiles are not unique
    if (is.null(color_column)) {
      xq <- quantile(tb[[x_column]], c(0.25,0.5,0.75), na.rm = TRUE)
      if (length(unique(xq)) < 3) {
        warning("x-quantiles are not unique, not plotting contours")
        plot_contours <- FALSE
        title <- paste0(title, "__non-unique quantiles of x")
      }
      
      yq <- quantile(tb[[y_column]], c(0.25,0.5,0.75), na.rm = TRUE)
      if (length(unique(yq)) < 3) {
        warning("y-quantiles are not unique, not plotting contours")
        plot_contours <- FALSE
        title <- paste0(title, "__non-unique quantiles of y")
      }
      
    } else {
      color_vals <- unique(tb[[color_column]])
      for (color_val0 in color_vals) {
        tb_group <- tb %>% filter(!!sym(color_column) == color_val0)
        xq <- quantile(tb_group[[x_column]], c(0.25,0.5,0.75), na.rm = TRUE)
        if (length(unique(xq)) < 3) {
          str_color_val <- sprintf("color %s: %s", color_column, color_val0)
          warning(paste0("x-quantiles are not unique, ",
                         str_color_val, 
                         ", not plotting contours\n"))
          plot_contours <- FALSE
          title <- paste0(title, "__non-unique quantiles of x",
                          str_color_val)
        }
        
        yq <- quantile(tb_group[[y_column]], c(0.25,0.5,0.75), na.rm = TRUE) 
        if (length(unique(yq)) < 3) {
          str_color_val <- sprintf("color %s: %s", color_column, color_val0)
          warning(paste0("y-quantiles are not unique, ",
                         str_color_val, 
                         ", not plotting contours\n"))
          plot_contours <- FALSE
          title <- paste0(title, "__non-unique quantiles of y",
                          str_color_val)
        }
      }
    }
  }
  
  if (plot_segments) {
    
    if (is.null(x_segment_columns)) {
      
      x_segment_columns$start <- x_column
      x_segment_columns$end <- paste0(x_column, "_end")
      tb[[x_segment_columns$end]] <- c(unlist(tb[2:nrow(tb),x_column]), NA)
    } 
    if (is.null(y_segment_columns)) {
      
      y_segment_columns$start <- y_column
      y_segment_columns$end <- paste0(y_column, "_end")
      tb[[y_segment_columns$end]] <- c(unlist(tb[2:nrow(tb),y_column]), NA)
    } 
    
  }
  
  
  #---Plotting
  if (is.null(color_column)) {
    p <- ggplot(tb, aes_string(x = x_column, y = y_column,
                               alpha = opacity))
  } else {
    p <- ggplot(tb, aes_string(x = x_column, y = y_column, 
                               color = color,
                               alpha = opacity))
  }
  
  if (coord_flip) {
    p <- p + coord_flip()
  }
  
  if (plot_scatter && (nrow(tb) <= n_cutoff || n_cutoff == 0)) {
    if (!is.null(size_column)) {
      size_scaling <- tb[[size_column]]
    } else {
      size_scaling <- 1
    }
    p <- p + geom_point(size = point_size * size_scaling)
  }
  
  if (plot_lines && (nrow(tb) <= n_cutoff || n_cutoff == 0)) {
    p <- p + geom_line(size = line_size)
  }
  
  if (plot_contours) {  
    p <- p + geom_density_2d(bins = n_contours)
  }
  
  if (plot_segments) {
    if (add_segment_arrows) {
      # p <- p + geom_segment(data = tibble(x = x_segment_columns$start, #if segment columns are supplied externally
      #                                     y = y_segment_columns$start,
      #                                     !!sym(color_column) := NA),
      #                       aes_string(x = x_segment_columns$start,
      #                                  xend = x_segment_columns$end, 
      #                                  y = y_segment_columns$start, 
      #                                  yend = y_segment_columns$end),
      #                       arrow=arrow(length=unit(0.4,"cm")))
      
      p <- p + geom_segment(data = tibble(!!sym(x_segment_columns$start) := tb[[x_segment_columns$start]],
                                          !!sym(y_segment_columns$start) := tb[[y_segment_columns$start]],
                                          !!sym(x_segment_columns$end) := tb[[x_segment_columns$end]],
                                          !!sym(y_segment_columns$end) := tb[[y_segment_columns$end]],
                                          !!sym(color_column) := tb[[color_column]]),
                            aes_string(x = x_segment_columns$start,
                                       xend = x_segment_columns$end, 
                                       y = y_segment_columns$start, 
                                       yend = y_segment_columns$end),
                            arrow=arrow(length=unit(0.4,"cm")))
    } else {
      # p <- p + geom_segment(data = tibble(x = x_segment_columns$start, #if segment columns are supplied externally
      #                                     y = y_segment_columns$start,
      #                                     !!sym(color_column) := NA),
      #                       aes_string(x = x_segment_columns$start,
      #                                  xend = x_segment_columns$end, 
      #                                  y = y_segment_columns$start, 
      #                                  yend = y_segment_columns$end))
      
      segment_df <- tibble(!!sym(x_segment_columns$start) := tb[[x_segment_columns$start]],
                           !!sym(y_segment_columns$start) := tb[[y_segment_columns$start]],
                           !!sym(x_segment_columns$end) := tb[[x_segment_columns$end]],
                           !!sym(y_segment_columns$end) := tb[[y_segment_columns$end]],
                           !!sym(color_column) := tb[[color_column]]) %>% mutate(local_idx=1:n())
      idx <- segment_df %>% filter(!!sym(x_segment_columns$start) > !!sym(x_segment_columns$end)) %>% 
        pull(local_idx)
      segment_df <- segment_df[-idx,]
      p <- p + geom_segment(data = segment_df,
                            aes_string(x = x_segment_columns$start,
                                       xend = x_segment_columns$end, 
                                       y = y_segment_columns$start, 
                                       yend = y_segment_columns$end),
                            alpha = 1,
                            size = 1.25)
    }
  }
  
  if (plot_marginals) {
    p <- ggMarginal(p, type = "histogram", bins = n_marginal_bins)
  }
  
  
  
  #---Facets
  if (is.character(facet_column)) {
    if (facet_free_y && facet_free_x) {
      scales = "free"
    } else if (facet_free_x) {
      scales = "free_x"
    } else if (facet_free_y) {
      scales = "free_y"
    } else {
      scales = "fixed"
    }
    p <- p + facet_wrap(facet_column, labeller = label_both, 
                        scales = scales)
  }
  
  
  
  #---Colors 
  if (!is.null(color_column)) {
    n_groups <- tb[,color_column] %>% unlist() %>% unique() %>% length()
    cat(sprintf("color groups: %d\n", n_groups))
    
    if (!is_continuous) {
      n_discrete <- length(unique(tb[[color_column]]))
      if (n_discrete == 1) { 
        palette_idx <- 1
      }
    } else {
      n_discrete <- NULL
    }
    # cols <- color_palette(palette_idx,
    #                       invert_colors = invert_colors,
    #                       cap_colors = cap_colors,
    #                       is_continuous = is_continuous,
    #                       n_discrete = n_discrete)
    
    if (is.null(manual_palette)) {
      cols <- color_palette(palette_idx, 
                            invert_colors = invert_colors,
                            n_discrete = n_discrete,
                            is_continuous = is_continuous,
                            transformation = color_transformation,
                            replace_first_color = replace_first_color,
                            replace_last_color = replace_last_color)
    } else {
      cols <- manual_palette
    }
    
    p <- p + cols$scale$col
  }
  
  
  
  #---axes
  if (!is.null(xlim) && !is.null(ylim)) {
    p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
  } else if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim)
  } else if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }
  
  
  if (!logx && is.numeric(unlist(tb[,x_column]))) {
    p <- p + scale_x_continuous(breaks = pretty_breaks())
  }
  
  if (logx && logy) {
    p <- p + coord_trans(x = "log10", y = "log10")
  } else if (logx) {
    p <- p + coord_trans(x = "log10")
  } else if (logy) {
    p <- p + coord_trans(y = "log10")
  }
  
  if (plot_smooth) {
    if (logy) {
      p <- p + ylim(min(tb[[y_column]]), max(tb[[y_column]]))
      # p <- p + geom_smooth(method = mgcv::gam, formula = log(y)~log(x),
      #                      method.args = list(family = gaussian)) #doesn't work
    }
    
    if (is.null(smooth_standard_span)) {
      p <- p + geom_smooth()
    } else {
      # p <- p + geom_smooth(method="loess", span = smooth_standard_span) #doesn't work, actually makes geom_smooth not use method loess...
      p <- p + geom_smooth(span = smooth_standard_span)
    }
    if (nrow(tb) > n_cutoff && n_cutoff !=0 && nrow(tb) <= 1e5) {
      p <- p + geom_smooth(span = 0.1)
    }
    if (!is.null(smooth_span)) {
      p <- p + geom_smooth(span = smooth_span)
    }
  }
  # if (plot_ribbon) {
  #   p <- p + geom_ribbon() #needs min_y, max_y
  # }
  if (plot_diagonal) {
    x_range <- range(tb[[x_column]])
    y_range <- range(tb[[y_column]])
    start_val <- max(x_range[1], y_range[1])
    end_val <- min(x_range[2], y_range[2])
    p <- p + geom_segment(aes(x = start_val, y = start_val, 
                              xend = end_val, yend = end_val),
                          color = "orange")
  }
  if (!is.null(plot_constant_lines_y)) {
    x_range <- range(tb[[x_column]])
    for (j in 1:length(plot_constant_lines_y)) {
      p <- p + geom_segment(aes(x = x_range[1], y = plot_constant_lines_y[j], 
                                xend = x_range[2], yend = plot_constant_lines_y[j]),
                            color = "orange")
    }
  }
  if (!is.null(plot_constant_lines_x)) {
    y_range <- range(tb[[y_column]])
    for (j in 1:length(plot_constant_lines_x)) {
      p <- p + geom_segment(aes(y = y_range[1], x = plot_constant_lines_x[j], 
                                yend = y_range[2], xend = plot_constant_lines_x[j]),
                            color = "orange")
    }
  }
  
  
  #---Aspect ratio
  if (!is.null(aspect_ratio)) {
    p <- p + coord_fixed(ratio=aspect_ratio)
  }
  
  
  
  #---Title
  if (!is.null(size_column)) {
    if (title != "") {
      title <- paste0(title, ", ")
    }
    title <- paste0(title, "size: ", size_column)
  }
  if (!is.null(color_column)) {
    p_labs <- labs(title = title, color = title_legend)
  } else {
    p_labs <- labs(title = title)
  }
  p <- p + p_labs
  
  if (plot_larger) {
    p <- p + theme(legend.key.width = unit(1.25, "cm"),
                   legend.key.height = unit(1.5, "cm")) +
      theme_bw() +
      theme(text = element_text(size = 24))
  } else {
    p <- p + theme(legend.key.width = unit(1.25, "cm"),
                   legend.key.height = unit(1.5, "cm")) +
      theme_bw() +
      theme(plot.title = element_text(size = title_size)) +
      theme(text = element_text(size = 15))
  }
  
  if (!is.null(color_column)) {
    if (n_groups > n_legend_cutoff && n_legend_cutoff != 0 && !is_continuous) {
      p <- p + theme(legend.position = "none")
    }
  }
  
  return(p)
}







plot_violin <- function(df, x_column, y_column, title="", palette_idx=140) {
  cols <- color_palette(palette_idx, 
                        n_discrete = length(unique(df[[x_column]])),
                        is_continuous = FALSE,
                        replace_first_color = "gray50",
                        replace_last_color = "gray95")
  
  browser()
  p <- ggplot(df, aes_string(x=x_column, y=y_column, fill=x_column)) +
    geom_violin() + 
    theme(legend.key.width = unit(1.25, "cm"),
          legend.key.height = unit(1.5, "cm")) +
    theme_bw() +
    theme(text = element_text(size = 24)) +
    cols$scale$fill +
    labs(title = title) #+ 
  # coord_flip()
  
  return(p)
}

plot_boxplot <- function(df, x_column, y_column, title="", color="blue", opacity=0.8) {
  p <- ggplot(df, aes_string(x=x_column, y=y_column)) + 
    geom_boxplot(
      
      # custom boxes
      color="blue",
      fill="blue",
      alpha=0.2,
      
      notch=TRUE,
      notchwidth = 0.8,
      
      # custom outliers
      outlier.colour="black",
      outlier.fill="black",
      outlier.size=3
      
    ) + 
    theme(legend.key.width = unit(1.25, "cm"),
          legend.key.height = unit(1.5, "cm")) +
    theme_bw() +
    theme(text = element_text(size = 24)) +
    labs(title = title)
  
  return(p)
}






size_transform <- function(vec, start=0.7, end=1.3) {
  width <- end - start
  vec <- vec - min(vec, na.rm=TRUE) 
  vec <- vec / max(vec, na.rm=TRUE) * width + start
  
  return(vec)
}














# plot_histogram <- function(tb, x_column, 
#                            facet_column = NULL,
#                            logx = FALSE,
#                            count = FALSE,
#                            n_bins = 100,
#                            title = "", title_size = 8,
#                            plot_larger = TRUE, 
#                            aspect_ratio = NULL) {
#   
#   if (logx) {
#     title <- paste0(title, ", LOG")
#     
#     x_column_prim <- paste0(x_column, "_LOG")
#     tb <- tb %>% rename(!!x_column_prim := x_column)
#     x_column <- x_column_prim
#     
#     tb[,x_column] <- log(tb[,x_column])
#   }
#   p <- ggplot(tb, aes_string(x = x_column)) +
#     labs(title = title) +
#     theme(plot.title = element_text(size = title_size))
#   
#   if (count) {
#     p <- p + geom_histogram(stat = "count") 
#   } else {
#     p <- p + geom_histogram(bins = n_bins) 
#   }
#   if (is.character(facet_column)) {
#     p <- p + facet_wrap(facet_column, labeller = label_both)
#   }
#   if (plot_larger) {
#     p <- p + theme(legend.key.width = unit(1.25, "cm"),
#                    legend.key.heigh = unit(1.5, "cm")) + 
#       theme(text = element_text(size = 24))
#   }
#   if (!is.null(aspect_ratio)) {
#     p <- p + coord_fixed(ratio=aspect_ratio)
#   }
#   
#   return(p)
# }
plot_histogram <- function(tb, x_column, 
                           facet_column = NULL,
                           logx = FALSE,
                           pow = NULL,
                           count = FALSE,
                           n_bins = 100,
                           title = "", title_size = 8,
                           xlim = NULL) {
  
  if (logx) {
    title <- paste0(title, ", LOG")
    
    x_column_prim <- paste0(x_column, "_LOG")
    tb <- tb %>% rename(!!x_column_prim := x_column)
    x_column <- x_column_prim
    
    tb[,x_column] <- log(tb[,x_column])
  }
  if (!is.null(pow)) {
    title <- paste0(title, ", POW", pow)
    
    x_column_prim <- paste0(x_column, "_POW", pow)
    tb <- tb %>% rename(!!x_column_prim := x_column)
    x_column <- x_column_prim
    
    tb[,x_column] <- tb[,x_column]^pow
  }
  
  p <- ggplot(tb, aes_string(x = x_column)) +
    labs(title = title) +
    theme(plot.title = element_text(size = title_size))
  
  if (count) {
    p <- p + geom_histogram(stat = "count") 
  } else {
    p <- p + geom_histogram(bins = n_bins) 
  }
  
  if (!is.null(xlim)) {
    p <- p + coord_cartesian(xlim = xlim)
  }
  
  
  if (is.character(facet_column)) {
    p <- p + facet_wrap(facet_column, labeller = label_both)
  }
  
  return(p)
}







plot_matrix <- function(x, reorder = TRUE, 
                        reorder_rows = FALSE, reorder_cols = FALSE,
                        title = "", palette_idx = 2, col_breaks = NULL,
                        title_font_sc = 0.75, 
                        label_font_size = NULL,
                        color_range = NULL) {
  # if (palette_idx == 1) {
  #   pal <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
  #                             "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  # } else {
  #   pal <- colorRampPalette(c(rgb(0.96,0.96,1), rgb(0.1,0.1,0.9)), space = "rgb")
  # }
  cols <- color_palette(palette_idx, color_range = color_range)[["col_palette"]]
  pal <- colorRampPalette(cols)(50)
  
  
  title_font_par <- par()$cex.main
  title_font_matrix <- title_font_par * title_font_sc
  par(cex.main = title_font_matrix)
  
  if (is.null(label_font_size)) {
    label_font_size_rows <- 0.2 + 1/log10(nrow(x))
    label_font_size_cols <- 0.2 + 1/log10(ncol(x))
  } else {
    label_font_size_rows <- label_font_size
    label_font_size_cols <- label_font_size
  }
  
  #Plot the matrix
  if (reorder) {
    if (is.null(col_breaks)) {
      x_hm <- heatmap.2(x, scale="none",
                        main=title, 
                        xlab="Columns", ylab="Rows", 
                        col=pal, tracecol="#303030", trace="none",  
                        keysize = 1.5, margins=c(4, 4), symbreaks=FALSE,
                        cexRow = label_font_size_rows, cexCol = label_font_size_cols)
    } else {
      x_hm <- heatmap.2(x, scale="none", 
                        Rowv=FALSE, Colv=FALSE, dendrogram="none",
                        main=title,
                        xlab="Columns", ylab="Rows",
                        col=pal, tracecol="#303030", trace="none",
                        breaks = col_breaks,
                        keysize = 1.5, margins=c(4, 4),
                        cexRow = label_font_size_rows, cexCol = label_font_size_cols)
    }
  } else {
    
    if (reorder_rows) {
      Rowv = TRUE
      Colv = FALSE
    } else if (reorder_cols) {
      Rowv = FALSE
      Colv = TRUE
    } else {
      Rowv = FALSE
      Colv = FALSE
    }
    
    if (is.null(col_breaks)) {
      x_hm <- heatmap.2(x, scale="none", 
                        Rowv=Rowv, Colv=Colv, dendrogram="none",
                        main=title,
                        xlab="Columns", ylab="Rows",
                        col=pal, tracecol="#303030", trace="none",
                        keysize = 1.5, margins=c(4, 4),
                        cexRow = label_font_size_rows, cexCol = label_font_size_cols)
    } else {
      x_hm <- heatmap.2(x, scale="none", 
                        Rowv=Rowv, Colv=Colv, dendrogram="none",
                        main=title,
                        xlab="Columns", ylab="Rows",
                        col=pal, tracecol="#303030", trace="none",
                        breaks = col_breaks,
                        keysize = 1.5, margins=c(4, 4),
                        cexRow = label_font_size_rows, cexCol = label_font_size_cols)
    }
  }
  
  par(cex.main = title_font_par)
  
  # library("fields")
  # image.plot(t(x))
  return(x_hm)
}







plot_hex <- function(tb, feature1, feature2, title = "",
                     palette_idx = 1, 
                     log_scale = 
                       list(x = FALSE, y = FALSE, count = TRUE),
                     xlim = NULL, ylim = NULL) {
  hex <- hex_tibble(tb, feature1, feature2, 
                    log_scale = log_scale, xbins = 60)
  n_cols <- unique(hex$tb[[hex$str$count]])
  p <- plot_scatter(hex$tb, 
                    x_column = hex$str$x, 
                    y_column = hex$str$y, 
                    color_column = hex$str$count,
                    title = title,
                    palette_idx = palette_idx,
                    xlim = xlim, ylim = ylim)
  return(p)
}

hex_tibble <- function(tb, feature1, feature2, group_column = NULL, 
                       xbins = 60, log_scale = 
                         list(x = FALSE, y = FALSE, count = FALSE)) {
  
  if (length(unique(tb[[feature2]])) > 1) {
    
    if (is.null(group_column)) {
      tb <- tb %>% add_column(iter_group = 1)
      group_column <- "iter_group"
      n_groups <- 1
    } else {
      n_groups <- length(unique(tb[[group_column]]))
      if (n_groups > 20)
        stop("too many groups")
    }
    
    hex_by_groups <- vector("list", n_groups)
    group_vals <- unique(tb[[group_column]])
    for (j in 1:n_groups) {
      group_val <- group_vals[[j]]
      tb_group <- tb %>% filter(!!sym(group_column) == group_val) 
      
      if (log_scale$x) {
        x_vals <- log10(tb_group[[feature1]])
      } else {
        x_vals <- tb_group[[feature1]]
      }
      if (log_scale$y) {
        y_vals <- log10(tb_group[[feature2]])
      } else {
        y_vals <- tb_group[[feature2]]
      }
      if (length(unique(x_vals)) > 1 && length(unique(y_vals)) > 1) {
        hex_by_groups[[j]] <- hexbin(x_vals, y_vals,
                                     xbins = xbins)
      } else {
        hex_by_groups[[j]] <- tibble(x = 
                                       seq(from = min(x_vals), 
                                           to = max(x_vals),
                                           length.out = xbins),
                                     y = unique(y_vals),
                                     count = -1,
                                     iter_group = j)
      }
    }
    
    inds <- lapply(hex_by_groups, is.null) %>% unlist() %>% which()
    if (!is_empty(inds)) {
      hex_by_groups <- hex_by_groups[-inds]
      n_groups <- length(hex_by_groups)
    }
    
    tb_hex <- tibble(x = numeric(), y = numeric(),
                     count = numeric(), iter_group = numeric())
    for (j in 1:n_groups) {
      
      if (class(hex_by_groups[[j]]) == "hexbin") {
        
        if (log_scale$count) {
          count <- log10(hex_by_groups[[j]]@count)
        } else {
          count <- hex_by_groups[[j]]@count
        }
        tb_hex <- tb_hex %>% 
          add_row(tibble(x = hex_by_groups[[j]]@xcm, 
                         y = hex_by_groups[[j]]@ycm,
                         count = count,
                         iter_group = j))
      } else if (is.data.frame(hex_by_groups[[j]])) {
        
        tb_hex <- tb_hex %>%  
          add_row(hex_by_groups[[j]])
      }
    }
    
    if (log_scale$x) {
      feature1 <- paste0(feature1, "_log10")
    }
    if (log_scale$y) {
      feature2 <- paste0(feature2, "_log10")
    }
    if (log_scale$count) {
      str_count <- "count_log"
    } else {
      str_count <- "count"
    }
    
    feature1 <- paste0(feature1, "_hex")
    feature2 <- paste0(feature2, "_hex")
    
    tb_hex <- tb_hex %>% 
      rename(!!feature1 := x, !!feature2 := y, !!str_count := count)
  } else {
    
    tb_hex <- tibble(!!feature1 := 
                       seq(from = min(tb[[feature1]]), 
                           to = max(tb[[feature1]]),
                           length.out = xbins),
                     !!feature2 := unique(tb[[feature2]]),
                     count := -1)
    str_count <- "count"
    n_groups <- 1
    group_vals <- -1
  }
  
  return(list(tb = tb_hex, 
              str = list(x = feature1, y = feature2, count = str_count),
              group_vals = 
                tibble(iter_group = 1:n_groups, group = group_vals)))
}






plot_geometry <- function(map, color="yellow", opacity=0.5, add=FALSE) {
  plot(st_geometry(map %>% pull(geometry)), add=add, col=scales::alpha(color, opacity))
}






