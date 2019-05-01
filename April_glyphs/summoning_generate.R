library(tidyverse)
library(rap)

summon <- function(seed_probs) {
  makedf_seed <- function(seed_shape, seed_r) {
    switch(seed_shape,
           "none" = {
             seed_df <- tibble(x = 0, y = 0, id = 1, type = "polygon")
           },
           "circle" = {
             seed_df <- tibble(angle = seq(0, 2*pi, length.out = 200), x = seed_r*cos(angle), y = seed_r*sin(angle), id = 1, type = "polygon")
           },
           "diamond" = {
             seed_df <- tibble(angle = seq(0, 2*pi, length.out = 5), x = seed_r*cos(angle), y = seed_r*sin(angle), id = 1, type = "polygon")
           },
           "square" = {
             seed_df <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = seed_r*cos(angle), y = seed_r*sin(angle), id = 1, type = "polygon")
           }
    )
    return(seed_df)
  }
  
  makedf_line <- function(shape, r, line, width) {
    switch(shape,
           "none" = {
             df <- tibble(x = 0, y = 0, id = 1, type = "path", linetype = line, linewidth = width)
           },
           "circle" = {
             df <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
           },
           "diamond" = {
             df <- tibble(angle = seq(0, 2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
           },
           "square" = {
             df <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
           },
           "square_left" = {
             df <- tibble(angle = seq((67.5*pi/180), (67.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
           },
           "square_right" = {
             df <- tibble(angle = seq((22.5*pi/180), (22.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "path", linetype = line, linewidth = width)
           }
    )
    return(df)
  }
  
  makedf_inscribed_planets <- function(shape, r, size) {
    switch(shape,
           "none" = {
             df <- tibble(x = 0, y = 0, id = 1, type = "point", size = size)
           },
           "circle" = {
             df <- tibble(angle = seq(0, 2*pi, length.out = 200), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
           },
           "diamond" = {
             df <- tibble(angle = seq(0, 2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
           },
           "square" = {
             df <- tibble(angle = seq(pi/4, pi/4+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
           },
           "square_left" = {
             df <- tibble(angle = seq((67.5*pi/180), (67.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
           },
           "square_right" = {
             df <- tibble(angle = seq((22.5*pi/180), (22.5*pi/180)+2*pi, length.out = 5), x = r*cos(angle), y = r*sin(angle), id = 1, type = "point", size = size)
           }
    )
    return(df)
  }
  
  makedf_outlines <- function(nlines, r, shapes, linetype, width) {
    df <- tibble(id = 1:nlines, r = r, shape = shapes, linetype = linetype, linewidth = width) %>%
      nest(r, shape, linetype, linewidth) %>%
      mutate(points = map(data, ~makedf_line(shape = .x$shape, r = .x$r, line = .x$linetype, width = .x$linewidth))) %>%
      unnest(points, .id = "parent")
    
    return(df)
    
  }
  
  makedf_orbits <- function(nlines, r, linetype, linewidth) {
    df <- tibble(id = 1:nlines, r = r, linetype = linetype, linewidth = linewidth) %>%
      nest(r, linetype, linewidth) %>%
      mutate(points = map(data, ~makedf_line(r = .x$r, shape = "circle", line = .x$linetype, width = .x$linewidth))) %>%
      unnest(points, .id = "parent")
    
    return(df)
  }
  
  makedf_planets <- function(cen_x, cen_y, r) {
    tibble(angle = seq(0, 2*pi, length.out = 50), x = cen_x + r*cos(angle), y = cen_y + r*sin(angle))
  }
  
  choose_inscribed <- function(i, probs) {
    rand <- sample(0:1, 1, prob = probs)
    
    if(rand > 0) {
      tibble(planet = 4, num = i)
    } else {
      tibble(planet = 2, num = i)
    }
  }
  
  calc_inscribed_r <- function(shape1, shape2, r1) {
    if(shape1 == "circle") {
      r <- r1
    }
    if(shape1 == "diamond") {
      #radius of circle inscribed in diamond
      #diameter is length of side of diamond
      #chord length (diamond side) is 2*r*sin(angle/2)
      r <- (2*r1*sin((pi/2)/2)/2)
    }
    if(shape1 == "square") {
      r <- 0.5*sqrt(2)*r1 #45 45 90 triangle given hyp is r1
    }
    return(r)
  }
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  #make seed df
  seed_opts <- c("none", "circle", "diamond", "square")
  seed_probs <- seed_probs
  seed_shape <- sample(seed_opts, 1, prob = seed_probs)
  seed_r <- runif(1, 0.1, 0.25)
  seed <- makedf_seed(seed_shape, seed_r)
  
  #choose outlines for seed
  num_seed_outlines <- sample(1:3, 1)
  seed_outline_bumps <- c(0.02, 0.03, 0.04)[1:num_seed_outlines]
  seed_outline_r <- rep(seed_r, times = num_seed_outlines) + seed_outline_bumps[1:num_seed_outlines]
  seed_outline_linetype <- sample(c("solid", "dotted"), num_seed_outlines, replace = TRUE, prob = c(0.8, 0.2))
  seed_outline_width <- c(0.2, 0.2, 0.18)[1:num_seed_outlines]
  seed_outline_shapes <- seed_shape
  seed_outlines <- makedf_outlines(num_seed_outlines, seed_outline_r, seed_outline_shapes, seed_outline_linetype, seed_outline_width)
  
  #choose orbit params
  num_orbits <- sample(1:4, 1)
  orbit_bumps <- c(0, 0.03, 0.1, 0.13)[1:num_orbits]
  orbit_params <- tibble(num = 1:num_orbits, orbit_bumps)
  orbit_params$orbit_widths <- c(sample(seq(0.15, 0.2, by = 0.01), 1), sample(seq(0.2, 0.25, by = 0.01), 1), sample(seq(0.2, 0.25, by = 0.01), 1), sample(seq(0.15, 0.2, by = 0.01), 1))[1:num_orbits]
  orbit_params$orbit_linetype <- sample(c("solid", "dotted"), num_orbits, replace = TRUE, prob = c(0.8, 0.2))
  thick_outline <- sample(1:8, 1)
  
  #adjust orbits if one is thick
  if(thick_outline %in% orbit_params$num) {
    orbit_params$orbit_widths[thick_outline] <- sample(seq(0.7, 1.5, by = 0.01), 1)
    orbit_params$orbit_linetype[thick_outline] <- "solid"
    
    orbit_bump_big <-
      orbit_params %>%
      filter(num > thick_outline) %>%
      mutate(orbit_bumps = orbit_bumps + 0.02) %>%
      pull(orbit_bumps)
    
    orbit_bump_small <-
      orbit_params %>%
      filter(num < thick_outline) %>%
      mutate(orbit_bumps = orbit_bumps - 0.02) %>%
      pull(orbit_bumps)
    
    orbit_params$orbit_bumps <- c(orbit_bump_small, orbit_params$orbit_bumps[thick_outline], orbit_bump_big)
  }
  
  orbit_r <- rep(1, times = num_orbits) + orbit_params$orbit_bumps
  orbits <- makedf_orbits(num_orbits, orbit_r, orbit_params$orbit_linetype, orbit_params$orbit_widths)
  
  #choose inscribed shapes
  num_inscribed <- sample(0:3, 1, prob = c(0.2, 0.3, 0.3, 0.2))
  inscribed_opts <- c("circle", "diamond", "square")
  if(num_inscribed > 0) {
    inscribed_shape1 <- sample(c("diamond", "square"), 1)
    inscribed_r1 <- min(orbit_r)
    inscribed_shape <- inscribed_shape1
    inscribed_r <- inscribed_r1
  } else {
    inscribed_shape1 = "none"
  }
  if(num_inscribed > 1) {
    inscribed_shape2 <- sample(inscribed_opts[inscribed_opts %!in% inscribed_shape1], 1)
    inscribed_r2 <- calc_inscribed_r(inscribed_shape1, inscribed_shape2, inscribed_r1)
    inscribed_shape <- c(inscribed_shape, inscribed_shape2)
    inscribed_r <- c(inscribed_r, inscribed_r2)
  }
  if(num_inscribed > 2) {
    inscribed_shape3 <- sample(inscribed_opts[inscribed_opts %!in% inscribed_shape2], 1)
    inscribed_r3 <- calc_inscribed_r(inscribed_shape2, inscribed_shape3, inscribed_r2)
    inscribed_shape <- c(inscribed_shape, inscribed_shape3)
    inscribed_r <- c(inscribed_r, inscribed_r3)
  }
  if(num_inscribed > 0) {
    if(inscribed_shape1 == "diamond") {
      sec_shape <- sample(c("square", "none"), 1, prob = c(0.4, 0.6))
      sec_r <- inscribed_r1
      sec_linetype <- sample(c("solid", "dotted"), 1, prob = c(0.2, 0.8))
      sec_width <- 0.15
      sec_df <- makedf_outlines(nlines = 1, r = sec_r, shapes = sec_shape, linetype = sec_linetype, width = sec_width)
    }
    if(inscribed_shape1 == "square") {
      sec_shape <- sample(c("diamond", "none"), 1, prob = c(0.4, 0.6))
      sec_r <- inscribed_r1
      sec_linetype <- sample(c("solid", "dotted"), 1, prob = c(0.2, 0.8))
      sec_width <- 0.15
      sec_df <- makedf_outlines(nlines = 1, r = sec_r, shapes = sec_shape, linetype = sec_linetype, width = sec_width)
    }
  } else {
    sec_df <- makedf_outlines(nlines = 1, r = 1, shapes = "none", linetype = "solid", width = 1)
  }
  if(num_inscribed > 0) {
    if(sec_shape != "none") {
      third_index <- sample(c(TRUE, FALSE), 1, prob = c(0.5, 0.5))
      if(third_index) {
        third_shapes <- c("square_left", "square_right")
        third_r <- inscribed_r1
        third_widths <- 0.15
        third_linetypes <- sec_linetype
        third_df <- makedf_outlines(nlines = 2, r = third_r, shapes = third_shapes, linetype = third_linetypes, width = third_widths)
      } else {
        third_df <- makedf_outlines(nlines = 1, r = 1, shapes = "none", linetype = "solid", width = 0.1)
      }
    } else {
      third_df <- makedf_outlines(nlines = 1, r = 1, shapes = "none", linetype = "solid", width = 0.1)
    }
  } else {
    third_df <- makedf_outlines(nlines = 1, r = 1, shapes = "none", linetype = "solid", width = 0.1)
  }
  
  inscribed_linetypes <- sample(c("solid", "dotted"), num_inscribed, replace = TRUE, prob = c(0.2, 0.8))
  inscribed_width <- 0.15
  if(num_inscribed > 0) {
    inscribed_df <- makedf_outlines(nlines = num_inscribed, r = inscribed_r, shapes = inscribed_shape, linetype = inscribed_linetypes, width = inscribed_width)
  } else {
    inscribed_df <- makedf_outlines(nlines = num_inscribed, r = 1, shapes = "none", linetype = "solid", width = 0)
  }
  
  
  #choose inscribed planets
  if(num_inscribed > 0) {
    inscribed_planets <- list()
    #inscribed_planets[[1]] <- choose_inscribed(i = 1, probs = c(0.2, 0.8))
    #need to change this to pick 2 or 4 for the first one, then if 2 on first pick 4 on second
    for(i in 1:num_inscribed) {
      inscribed_planets[[i]] <- choose_inscribed(i = i, probs = c(0, 1))
    }
    
    #have to make inscribed params df
    inscribed_params <- tibble(num = 1:num_inscribed, shape = inscribed_shape, r = inscribed_r)
    
    inscribed_planets_join <- 
      map(inscribed_planets, ~ left_join(.x, inscribed_params, by = "num")) %>%
      keep(., ~ .x$shape[1] != "circle") %>%
      bind_rows()
    
    if(nrow(inscribed_planets_join) > 0) {
      num_keep <- sample(1:nrow(inscribed_planets_join), 1)
      inscribed_planets_join <- inscribed_planets_join[1:num_keep, ]
      
      inscribed_planets_pos <- 
        inscribed_planets_join %>%
        rap(points = ~makedf_inscribed_planets(shape = shape, r = r, size = sample(seq(2, 3.5, by = 0.1), 1))) %>%
        unnest(.id = "id") %>%
        select(id, x, y, size) %>%
        mutate(color = "white")
    } else {
      inscribed_planets_pos <- data.frame(id = 1, x = 1, y = 1, size = 0, color = "black")
    }
    
    
  } else {
    inscribed_planets_pos <- data.frame(id = 1, x = 1, y = 1, size = 0, color = "black")
  }
  
  #put it all in a list and plot
  final_dat <- 
    list(seed = seed, 
       seed_outlines = seed_outlines, 
       orbits = orbits, 
       inscribed = inscribed_df, 
       sec = sec_df, 
       third = third_df, 
       inscribed_planets = inscribed_planets_pos
  )
 
  ggplot() +
    geom_polygon(data = final_dat[["seed"]], aes(x = x, y = y, group = id), fill = "white") +
    geom_path(data = final_dat[["seed_outlines"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["seed_outlines"]]$linetype, color = "white") +
    geom_path(data = final_dat[["orbits"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["orbits"]]$linetype, color = "white") +
    geom_path(data = final_dat[["inscribed"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["inscribed"]]$linetype, color = "white") +
    geom_path(data = final_dat[["sec"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["sec"]]$linetype, color = "white") +
    geom_path(data = final_dat[["third"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["third"]]$linetype, color = "white") +
    geom_point(data = final_dat[["inscribed_planets"]], aes(x = x, y = y, size = size, color = color)) +
    scale_size_identity() +
    scale_color_identity() +
    theme_void() +
    coord_equal() +
    theme(panel.background = element_rect(fill = "#141414")) 
}

seed_probs <- c(0.3, 0.5, 0.1, 0.1)

summon(seed_probs)

ggsave("summon_5.png", device = "png", type = "cairo")
