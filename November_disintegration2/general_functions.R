#########################
### general functions ###
#########################
rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
  bounds <- pnorm(c(min, max), mean, sd)
  u <- runif(n, bounds[1], bounds[2])
  qnorm(u, mean, sd)
}

#split the edges and add inertia + diff values
splitter <- function(data, seed, wind_angle = 0, inertia_mean = 1, inertia_sd = 5, inertia_min = 1, inertia_max = 100) {
  if(is.null(seed$split)) {
    if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
      data %>%
        mutate(inertia2 = seed$inertia2, inertia_group = seed$inertia2) %>%
        select(id, x, y, inertia2, inertia_group) %>%
        group_by(inertia_group) %>%
        arrange(desc(x)) %>%
        group_map( ~ mutate(., id = 1:nrow(.))) %>%
        map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
        bind_rows(.id = "group_id") %>%
        mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
        ungroup() 
    } else {
      data %>%
        mutate(inertia2 = seed$inertia2, inertia_group = seed$inertia2) %>%
        select(id, x, y, inertia2, inertia_group) %>%
        group_by(inertia_group) %>%
        arrange(y) %>%
        group_map( ~ mutate(., id = 1:nrow(.))) %>%
        map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
        bind_rows(.id = "group_id") %>%
        mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
        ungroup() 
    }
  } else {
    if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
      data %>%
        mutate(split = seed$split, inertia2 = seed$inertia2) %>%
        select(id, x, y, split, inertia2) %>%
        group_by(split) %>%
        arrange(desc(x)) %>%
        group_map( ~ mutate(., id = 1:nrow(.))) %>%
        map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
        bind_rows(.id = "group_id") %>%
        mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
        ungroup() 
    } else {
      data %>%
        mutate(split = seed$split, inertia2 = seed$inertia2) %>%
        select(id, x, y, split, inertia2) %>%
        group_by(split) %>%
        arrange(y) %>%
        group_map( ~ mutate(., id = 1:nrow(.))) %>%
        map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
        bind_rows(.id = "group_id") %>%
        mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
        ungroup() 
    }
  }
}

#add curl noise distortion to points
unfold_tempest <- function(data, iterations = 6, scale = .02, scatter = FALSE, type = "curl", seed = 123, output1 = "time", output2 = "order") {
  # check for seed value
  if(scatter == FALSE) seed <- seed
  if(scatter == TRUE)  seed <- NULL
  
  n <- nrow(data)
  data[[output1]] <- 1    # initial value for the "time" output
  data[[output2]] <- 1:n  # initial values for the "order" output
  
  static <- data %>% filter(inertia == 0) %>% select(-time, -order)
  data2 <- data %>% filter(inertia != 0)
  
  curl_step <- function(data, time) {
    if(type == "curl") {
      # generate the curl noise
      noise <- ambient::curl_noise(
        generator = ambient::gen_simplex,
        x = data$x,
        y = data$y,
        seed = seed
      )
    } else if(type == "worley") {
      noise <- ambient::curl_noise(
        generator = ambient::gen_worley,
        x = data$x,
        y = data$y,
        seed = seed
      )
    }
    
    if(is.null(data$scale)) {
      # update the co-ordinates
      data$x <- data$x + noise$x * scale
      data$y <- data$y + noise$y * scale
    } else {
      # update the co-ordinates
      data$x <- data$x + noise$x * data$scale
      data$y <- data$y + noise$y * data$scale
    }
    
    # increment the "time" dimension
    data[[output1]] <- time
    
    # calculate the "ordering"
    data[[output2]] <- order(sqrt(noise$x^2 + noise$y^2))
    
    return(data)
  }
  
  # accumulate
  ribbon <- purrr::accumulate(
    .x = (1:iterations) + 1,
    .f = curl_step,
    .init = data2
  )
  
  ribbon <- dplyr::bind_rows(ribbon) %>%
    filter(time == iterations) %>%
    select(-time, -order)
  
  
  rbind(ribbon, static)
  # ribbon <- bind_rows(ribbon)
  # return(ribbon)
}

#fray the edges of the shape
fray <- function(data, min_fray = 20, max_fray = 100, num_fray = 3) {
  frayer <- function(dat, start, end) { 
    dat %>%
      filter(inertia != 0) %>%
      mutate(group_id2 = group_id) %>%
      group_by(group_id2) %>%
      group_map( ~ filter(., id %in% start:end | id %in% (max(id)-start):(max(id)-end))) %>%
      bind_rows() %>%
      ungroup() %>%
      mutate(inertia2 = sample(c(0, 1), nrow(.), replace = TRUE, prob = c(1, 1)),
             inertia = ifelse(inertia2 == 0, 0, inertia))
  }
  
  fray_params <- list()
  for(i in 1:num_fray) {
    fray_params$start[[i]] <- ifelse(length(fray_params) == 0, 1, fray_params$end[[i - 1]])
    fray_params$end[[i]] <- sample(min_fray:max_fray, 1)
  }
  
  frayed <- list()
  for(i in 1:num_fray) {
    frayed[[i]] <- frayer(data, start = fray_params$start[[i]], end = fray_params$end[[i]])
  }
  
  frayed <- bind_rows(frayed)
  
  full_join(data, frayed, by = c("group_id", "id")) %>%
    mutate(inertia = ifelse(is.na(inertia.y), inertia.x, inertia.y)) %>%
    select(group_id, id, x = x.x, y = y.x, inertia, diff = diff.x)
  
}

#transform points to regons
regonize <- function(data, min_r = 1, max_r = 5, pal) {
  static <- data %>% filter(inertia == 0)
  data <- data %>% filter(inertia != 0)
  
  data <- 
    data %>%
    mutate(r = sample(seq(min_r, max_r, by = 0.1), nrow(data), replace = TRUE), 
           color = sample(pal, nrow(data), replace = TRUE),
           angle = sample(seq(0, 2*pi, by = 0.1), nrow(data), replace = TRUE))
  
  return(list(regons = data, static = static))
}

#turn particles into bubbles
bubbleize <- function(data, base_size = 0.1, base_color = "black", min_r = 0.1, max_r = 1.5, pal) {
data %>%
    mutate(size = ifelse(inertia == 0, base_size, sample(seq(min_r, max_r, by = 0.1), nrow(data), replace = TRUE)), 
           color = ifelse(inertia == 0, base_color, sample(pal, nrow(data), replace = TRUE)))
  
}

#move points according to force, inertia, jitter, etc
gust <- function(data, angle = 0, force = 100, diff_mod = 100, inertia_mod = 50, jitter_min = 0.1, jitter_max = 1, jitter_mod = 1) {
  a <- angle * pi / 180
  perp <- ifelse((((angle >= 315 & angle <= 360) | (angle >= 0 & angle <= 45)) | (angle >= 45 & angle <= 135)), a + ((3 * pi) / 2), a + (pi / 2))
  
  data_jittered <- 
    data %>%
    mutate(jitter = rtnorm(nrow(.), mean = 0, sd = 2, min = -10, max = 15) * jitter_mod) 
  
  
  pts_new <- 
    data_jittered %>%
    mutate(x = ifelse(inertia == 0, x + cos(a)*force, (x + (cos(a) * (force * inertia))) + 
                        (cos(perp) * ((diff * diff_mod)  * (inertia * inertia_mod) * sample(seq(jitter_min, jitter_max, 0.01), 1))) +
                        (cos(perp) * jitter)),
           y = ifelse(inertia == 0, y + sin(a)*force, (y + (sin(a) * (force * inertia))) + 
                        (sin(perp) * ((diff * diff_mod) * (inertia * inertia_mod) * sample(seq(jitter_min, jitter_max, 0.01), 1))) +
                        (sin(perp) * jitter)))
  
  return(pts_new)
}

#################
### for lines ###
#################

#generate random points between endpoints for a set of edges
sand_paint <- function(edges, n_grains = 100) {
  # Function for sand painting a single edge
  sand_paint_edge <- function(edge_num) {
    a <- as.numeric(edges[edge_num, c("x", "y")])
    b <- as.numeric(edges[edge_num, c("xend", "yend")])
    data.frame(alpha = runif(n_grains)) %>%
      dplyr::mutate(x = a[1] * (1 - alpha) + b[1] * alpha,
                    y = a[2] * (1 - alpha) + b[2] * alpha) %>%
      dplyr::select(-alpha)
  }
  # Sand paint all the edges
  1:nrow(edges) %>%
    purrr::map_df(~sand_paint_edge(.x), id = "id")
}

#paint a line from endpoints w/ sandpaint
paint <- function(line, grains, wind_angle = 0) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand_paint(line, n_grains = grains) %>%
      arrange(desc(x)) %>%
      mutate(id = 1:nrow(.))
  } else {
    sand_paint(line, n_grains = grains) %>%
      arrange(y) %>%
      mutate(id = 1:nrow(.))
  }
}

#generate a seed line with splits
gen_seed_line <- function(seed, n_grains = 10000, wind_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1, 
                          gaps = NULL) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand <- sand_paint(seed, n_grains = n_grains) %>%
      arrange(desc(x)) %>%
      mutate(id = 1:nrow(.))
  } else {
    sand <- sand_paint(seed, n_grains = n_grains) %>%
      arrange(y) %>%
      mutate(id = 1:nrow(.))
  }
  
  if(is.null(gaps)) {
    splits <- sand %>% sample_n(nrow(.) / split_mod)
    
    sand_split <- 
      sand %>%
      left_join(splits, by = "id") %>%
      mutate(split = ifelse(is.na(x.y), NA, id))
    
    sand_split$split[1] <- 0
    split2 <- na.locf(sand_split$split)
    
    sand_split %>%
      mutate(split = split2, group_split = split2) %>%
      select(id, x = x.x, y = y.x, split, group_split) %>%
      group_by(group_split) %>%
      group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
      map( ~ mutate(., id = 1:nrow(.))) %>%
      map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
      bind_rows(.id = "group_id")
  } else {
    splits <- rep(0, n_grains)
    
    for(i in 1:length(gaps)) {
      start <- gaps[[i]][1]
      end <- gaps[[i]][2]
      
      splits[start:end] <- NA
    }
    
    sand %>%
      mutate(inertia2 = splits) %>%
      select(id, x, y, inertia2)
  }
}

###################
### for circles ###
###################

#draw a circle
circle <- function(points = 10000, r = 30, x0 = 0, y0 = 0) {
  tibble(angle = seq(0, 2*pi, length.out = points), x = x0 + r*cos(angle), y = y0 + r*sin(angle)) %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, x, y)
}

#generate a seed for circles with splits
gen_seed_cir <- function(n_grains = 10000, r = 30, wind_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1,
                         gaps = NULL) {
  if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
    sand <- circle(n_grains, r) %>%
      arrange(desc(x)) %>%
      mutate(id = 1:nrow(.))
  } else {
    sand <- circle(n_grains, r) %>%
      arrange(y) %>%
      mutate(id = 1:nrow(.))
  }
  
  if(is.null(gaps)) {
    splits <- sand %>% sample_n(nrow(.) / split_mod)
    
    sand_split <- 
      sand %>%
      left_join(splits, by = "id") %>%
      mutate(split = ifelse(is.na(x.y), NA, id))
    
    sand_split$split[1] <- 0
    split2 <- na.locf(sand_split$split)
    
    sand_split %>%
      mutate(split = split2, group_split = split2) %>%
      select(id, x = x.x, y = y.x, split, group_split) %>%
      group_by(group_split) %>%
      group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
      map( ~ mutate(., id = 1:nrow(.))) %>%
      map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
      bind_rows(.id = "group_id")
  } else {
    splits <- rep(0, n_grains)
    
    for(i in 1:length(gaps)) {
      start <- gaps[[i]][1]
      end <- gaps[[i]][2]
      
      splits[start:end] <- NA
    }
    
    sand %>%
      mutate(inertia2 = splits) %>%
      select(id, x, y, inertia2)
  }
}


##################
### for regons ###
##################

#draw a regon
regon_pts <- function(points = 10000, edges = 3, start_angle = 0, r = 10, wind_angle = 0, cx = 0, cy = 0) {
  a = start_angle * (pi / 180)
  n = floor(points / edges)
  
  corners <- tibble(angle = seq(0 + a, 2*pi + a, length.out = edges + 1), x = r*cos(angle) + cx, y = r*sin(angle) + cy)
  
  edges_list <- list()
  
  for(i in 1:edges) {
    edges_list[[i]] <- tibble(id = 1:n) %>%
      mutate(d = id / n, 
             x = (1 - d) * corners$x[i] + d * corners$x[i + 1],
             y = (1 - d) * corners$y[i] + d * corners$y[i + 1])
  }
  
  bind_rows(edges_list)
  
}

#generate a seed for regon with splits
gen_seed_regon <- function(n_grains = 10000, r = 30, edges = 3, start_angle = 0, cx = 0, cy = 0, wind_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1,
                           gaps = NULL) {
  sand <- regon_pts(points = n_grains, r = r, edges = edges, start_angle = start_angle, cx = cx, cy = cy) %>%
    mutate(id = 1:nrow(.))
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  if(is.null(gaps)) {
    if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
      sand_split %>%
        mutate(split = split2, group_split = split2) %>%
        select(id, x = x.x, y = y.x, split, group_split) %>%
        group_by(group_split) %>%
        arrange(desc(x)) %>%
        group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
        map( ~ mutate(., id = 1:nrow(.))) %>%
        map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
        bind_rows(.id = "group_id")
    } else {
      sand_split %>%
        mutate(split = split2, group_split = split2) %>%
        select(id, x = x.x, y = y.x, split, group_split) %>%
        group_by(group_split) %>%
        arrange(y) %>%
        group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
        map( ~ mutate(., id = 1:nrow(.))) %>%
        map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
        bind_rows(.id = "group_id")
    }
  } else {
    splits <- rep(0, n_grains)
    
    for(i in 1:length(gaps)) {
      start <- gaps[[i]][1]
      end <- gaps[[i]][2]
      
      splits[start:end] <- NA
    }
    
    if((wind_angle >= 45 & wind_angle <= 135) | (wind_angle >= 225 & wind_angle < 315)) {
      sand %>%
        arrange(desc(x)) %>%
        mutate(id = 1:nrow(.), inertia2 = splits) %>%
        select(id, x, y, inertia2)
    } else {
      sand %>%
        arrange(y) %>%
        mutate(id = 1:nrow(.), inertia2 = splits) %>%
        select(id, x, y, inertia2)
    }
  }
}
