library(tidyverse)
library(rap)
library(EnvStats)

generate_orbit <- function(seed_probs = c(0.3, 0.5, 0.1, 0.1), planet_probs = c(0.4, 0.3, 0.2, 0.2), pareto2_prob = c(0.2, 0.8)) {
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
  
  #choose seed outlines
  num_seed_outlines <- sample(1:3, 1)
  seed_outline_bumps <- c(0.02, 0.03, 0.04)[1:num_seed_outlines]
  seed_outline_r <- rep(seed_r, times = num_seed_outlines) + seed_outline_bumps[1:num_seed_outlines]
  seed_outline_linetype <- sample(c("solid", "dotted"), num_seed_outlines, replace = TRUE, prob = c(0.8, 0.2))
  seed_outline_width <- c(0.2, 0.2, 0.18)[1:num_seed_outlines]
  seed_outline_shapes <- seed_shape
  seed_outlines <- makedf_outlines(num_seed_outlines, seed_outline_r, seed_outline_shapes, seed_outline_linetype, seed_outline_width)
  
  #choose orbits
  num_orbits <- sample(1:4, 1)
  orbit_bumps <- c(0, 0.03, 0.1, 0.13)[1:num_orbits]
  orbit_params <- tibble(num = 1:num_orbits, orbit_bumps)
  orbit_params$orbit_widths <- c(sample(seq(0.15, 0.2, by = 0.01), 1), sample(seq(0.2, 0.25, by = 0.01), 1), sample(seq(0.2, 0.25, by = 0.01), 1), sample(seq(0.15, 0.2, by = 0.01), 1))[1:num_orbits]
  orbit_params$orbit_linetype <- sample(c("solid", "dotted"), num_orbits, replace = TRUE, prob = c(0.8, 0.2))
  thick_outline <- sample(1:8, 1)
  
  #adjust orbits it one is thick
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
  
  #choose planets
  planets <- list()
  for(i in 1:num_orbits) {
    planets[[i]] <- tibble(planet = 0:sample(0:3, 1, prob = planet_probs), num = i)
  }
  
  planets_join <- 
    map(planets, ~ left_join(.x, orbit_params, by = "num")) %>%
    #map(., ~ filter(.x, planet > 0)) %>%  do this if we really want to have 0 at any point
    #keep(., ~nrow(.x) > 0)
    map(., ~ mutate(.x, r = 1 + orbit_bumps)) %>%
    keep(., ~ .x$orbit_widths[1] < 0.5) %>%
    keep(., ~ .x$orbit_linetype[1] == "solid") %>%
    bind_rows() 
  
  if(nrow(planets_join) > 0) {
    planets_join2 <-
      planets_join %>%
      mutate(cen_angle = sample(seq(0, 2*pi, by = 0.01), nrow(planets_join)), cen_x = r*cos(cen_angle), cen_y = r*sin(cen_angle), planet_r = sample(seq(0.03, 0.05, by = 0.001), size = nrow(planets_join)))
    
    planets_pos <- 
      planets_join2 %>%
      rap(points = ~makedf_planets(cen_x = cen_x, cen_y = cen_y, r = planet_r)) %>%
      unnest(.id = "id") %>%
      select(id, x, y)
    
    #choose planet orbits
    planets_orbits <-
      planets_join2 %>%
      mutate(num_plan_orbits = sample(0:3, nrow(planets_join2), replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.1))) %>%
      uncount(num_plan_orbits, .id = "orbit_num") %>%
      mutate(r = case_when(
        orbit_num == 1 ~ planet_r + 0.015,
        orbit_num == 2 ~ planet_r + 0.03,
        orbit_num == 3 ~ planet_r + 0.04
      )) 
    
    if(nrow(planets_orbits) > 0) {
      planets_orbits <- 
        planets_orbits %>%
        rap(points = ~makedf_planets(cen_x = cen_x, cen_y = cen_y, r = r)) %>%
        unnest(.id = "id") %>%
        select(id, x, y) %>%
        mutate(id = as.character(id))
    } else {
      planets_orbits <- data.frame(id = "1", x = 0, y = 0)
    }
    
    planets_linesize <- 
      data.frame(id = as.character(unique(planets_orbits$id)), linewidth = sample(seq(0.1, 0.16, by = 0.01), length(unique(planets_orbits$id)), replace = TRUE)) 
    
    suppressWarnings(planets_orbits <- left_join(planets_orbits, planets_linesize, by = "id"))
  } else {
    planets_pos <- data.frame(id = 1, x = 0, y = 0)
    planets_orbits <- data.frame(id = 1, x = 0, y = 0)
  }
  
  if(nrow(planets_orbits) == 0) {
    planets_orbits <- data.frame(x = 0, y = 0, id = 1, linesize = 0)
  }
  
  pareto_start <- sample(0.35:0.6, 1)
  num_paretos <- sample(3:50, 1)
  pareto_r <- rpareto(num_paretos, pareto_start, shape = sample(3:4, 1))
  pareto_linetype <- sample(c("solid", "dotted"), num_paretos, replace = TRUE, prob = c(0.9, 0.1))
  pareto_width <- sample(seq(0.1, 0.25, by = 0.01), num_paretos, replace = TRUE)
  
  pareto_orbits <- makedf_orbits(num_paretos, r = pareto_r, linetype = pareto_linetype, linewidth = pareto_width)
  
  pareto_2 <- sample(c(TRUE, FALSE), 1, prob = pareto2_prob)
  if(pareto_2) {
    pareto_start2 <- sample(1:1.5, 1)
    num_paretos2 <- sample(1:20, 1)
    pareto_r2 <- rpareto(num_paretos, pareto_start, shape = sample(3:4, 1))
    pareto_linetype2 <- sample(c("solid", "dotted"), num_paretos, replace = TRUE, prob = c(0.9, 0.1))
    pareto_width2 <- sample(seq(0.1, 0.4, by = 0.01), num_paretos, replace = TRUE)
    
    pareto_orbits2 <- makedf_orbits(num_paretos, r = pareto_r, linetype = pareto_linetype, linewidth = pareto_width)
  } else {
    pareto_orbits2 <- data.frame(x = 0, y = 0, parent = 1, linewidth = 0, linetype = "solid")
  }
  
  #put it all in a list and plot
  final_dat <- 
    list(seed = seed, 
       seed_outlines = seed_outlines, 
       orbits = orbits, 
       planets = planets_pos,
       planet_orbits = planets_orbits, 
       pareto1 = pareto_orbits, 
       pareto2 = pareto_orbits2
  )
  
  plot <- 
    ggplot() +
      geom_polygon(data = final_dat[["seed"]], aes(x = x, y = y, group = id), fill = "white") +
      geom_path(data = final_dat[["seed_outlines"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["seed_outlines"]]$linetype, color = "white") +
      geom_path(data = final_dat[["pareto1"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["pareto1"]]$linetype, color = "white") +
      geom_path(data = final_dat[["pareto2"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["pareto2"]]$linetype, color = "white") +
      geom_path(data = final_dat[["orbits"]], aes(x = x, y = y, group = parent, size = linewidth), linetype = final_dat[["orbits"]]$linetype, color = "white") +
      geom_polygon(data = final_dat[["planets"]], aes(x = x, y = y, group = id), fill = "white") +
      geom_path(data = final_dat[["planet_orbits"]], aes (x = x, y = y, group = id, size = linesize), color = "white", size = 0.13) +
      scale_size_identity() +
      scale_color_identity() +
      theme_void() +
      coord_equal() +
      theme(panel.background = element_rect(fill = "#141414"))
  
  suppressWarnings(print(plot))
  
}


generate_orbit(seed_probs)

#ggsave("orbit_pareto_4.png", device = "png")
