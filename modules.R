load_pitcher_pool = function(season) {
  pitches = read.csv("./data/hittr_pitcher_pool.csv", stringsAsFactors = FALSE)
  return(pitches)
}

load_batter_pool = function(season) {
  pitches = read.csv("./data/hittr_batter_pool.csv", stringsAsFactors = FALSE)
  return(pitches)
}

load_bip = function(season) {
  pitches = read.csv("./data/hittr_bip.csv", stringsAsFactors = FALSE)
  return(pitches)
}

get_similarity = function(name, hand, pitch, ratio, pitches_pool, type) {
  if (type == "pitcher") {
    characteristics = pitches_pool %>% 
      filter(game_year == 0,
             pitcher == name, 
             pitch_type == pitch,
             p_throws == hand) %>% 
      select(release_speed, pfx_x, pfx_z, release_spin_rate,
             pitch_launch_h_c, pitch_launch_v_c,
             release_pos_x, release_pos_y, release_pos_z)
    
    pool = pitches_pool %>% 
      filter(game_year != 0,
             p_throws == hand,
             pitch_type == pitch,
             pitcher != name)
    
    pitcher = pool$pitcher
    game_year = pool$game_year
    n = pool$n
    
    pool = pool %>% 
      select(release_speed, pfx_x, pfx_z, release_spin_rate,
             pitch_launch_h_c, pitch_launch_v_c,
             release_pos_x, release_pos_y, release_pos_z)
    
    stuff = which(colnames(pool) %in% c("release_speed", "release_spin_rate", "pfx_x", "pfx_z"))
    not_stuff = which(colnames(pool) %in% c("release_pos_x", "release_pos_y", "release_pos_z", "pitch_launch_h_c", "pitch_launch_v_c"))
  } else {
    characteristics = pitches_pool %>% 
      filter(game_year == 0,
             batter == name, 
             pitch_type == pitch,
             stand == hand) %>%
      select(launch_speed, launch_angle, pull, cent, oppo)
    
    pool = pitches_pool %>% 
      filter(game_year != 0,
             stand == hand,
             pitch_type == pitch,
             batter != name)
    
    batter = pool$batter
    game_year = pool$game_year
    n = pool$n
    
    pool = pool %>% select(launch_speed, launch_angle, pull, cent, oppo)
    
    stuff = which(colnames(pool) %in% c("launch_angle", "launch_speed"))
    not_stuff = which(colnames(pool) %in% c("pull", "cent", "oppo"))
  }
  
  m = diag(1, ncol(pool), ncol(pool))
  for (s in stuff) {
    m[s, s] = ratio * ncol(pool) / length(stuff)
  }
  
  for (s in not_stuff) {
    m[s, s] = (1 - ratio) * ncol(pool) / length(not_stuff)
  }
  
  pool$similarity = apply(pool, 1, function(p) {
    temp = matrix(as.numeric(characteristics - p), byrow = TRUE)
    distance = sqrt(t(temp) %*% m %*% temp)
    exp(-distance)})
  
  master = pool %>% 
    select(similarity) %>%
    mutate(game_year = game_year,
           pitch_type = pitch,
           n = n)
  
  if (type == "pitcher") {
    master$pitcher = pitcher
  } else {
    master$batter = batter
  }
  
  return(master)
}

get_density = function(pitcher_name, p_hand, batter_name, b_hand, pitch, weights, pitches_bip, type) {
  # join each batted ball with weights
  if (type == "pitcher") {
    bip = get_bip(pitcher_name = "all", batter_name, b_hand, pitch, pitches_bip) %>%
      filter(p_throws == p_hand) %>%
      inner_join(weights, by = c("pitcher", "game_year", "pitch_type")) %>%
      filter(!is.na(similarity))
  } else if (type == "batter") {
    bip = get_bip(pitcher_name, batter_name = "all", b_hand, pitch, pitches_bip) %>%
      inner_join(weights, by = c("batter", "game_year", "pitch_type")) %>%
      filter(!is.na(similarity))
  } else {
    # for when a batter is added to his own similarity, see synthetic()
    bip = get_bip(pitcher_name, batter_name, b_hand, pitch, pitches_bip) %>%
      mutate(similarity = 1)
  }
  
  # return empty if not enough bip
  if (nrow(bip) < 2) {
    return(empty_density())
  }
  
  # rescale weights
  bip$weight = bip$similarity / sum(bip$similarity)
  est = kde2d.weighted(bip$hc_x, bip$hc_y, n = 100, lims = c(-150, 150, 0, 200), w = bip$weight)
  density = expand.grid(x = est$x, y = est$y)
  density$z = as.vector(est$z)
  
  return(density)
}

get_bip = function(pitcher_name, batter_name, b_hand, pitch, pitches_bip) {
  pitches_batter = pitches_bip %>% filter(stand == b_hand)
  
  if (batter_name != "all") {
    pitches_batter = pitches_batter %>% filter(batter == batter_name)
  }
  
  if (pitcher_name != "all") {
    pitches_batter = pitches_batter %>% filter(pitcher == pitcher_name)
  }
  
  if (pitch != "all") {
    pitches_batter = pitches_batter %>% filter(pitch_type == pitch)
  }
  
  return(pitches_batter)
}

bin_pitches = function(pitches_bip, synth_master) {
  pitches_bin = read.csv("./data/pitches_bin.csv", stringsAsFactors = FALSE) %>%
    mutate(bin_all = as.character(bin_all))
  
  density1 = synth_master %>%
    mutate(bin_x = cut(x, breaks = seq(-160, 160, 10), labels = FALSE),
           bin_y = cut(y, breaks = seq(-10, 210, 10), labels = FALSE),
           bin_all = sprintf("%02d.%02d",bin_x,bin_y)) %>%
    select(x, y, z, bin_all)
  
  agg_bin = density1 %>% 
    filter(bin_all %in% unique(pitches_bip$bin_all)) %>%
    group_by(bin_all) %>% 
    summarise(z_bin = sum(z)) %>% 
    mutate(z_bin = z_bin / sum(z_bin),
           bin_all = as.character(bin_all))
  
  density_sum = agg_bin %>%
    inner_join(pitches_bin, by = "bin_all") %>%
    mutate(sum_single = single_prob * z_bin,
           sum_double = double_prob * z_bin,
           sum_triple = triple_prob * z_bin,
           sum_home_run = home_run_prob * z_bin,
           sum_out = out_prob * z_bin)
  
  density_agg = density_sum %>%
    summarise(singles = sum(sum_single) * 100,
              doubles = sum(sum_double) * 100,
              triples = sum(sum_triple) * 100,
              home_runs = sum(sum_home_run) * 100,
              outs = sum(sum_out) * 100,
              
              xbabip = (singles + doubles + triples) / (singles + doubles + triples + outs),
              xbscon = (singles + 2 * doubles + 3 * triples + 4 * home_runs) / (singles + doubles + triples + home_runs + outs))
  
  return(density_agg)
}

graph_field = function(data, title) {
  data_f = data %>% filter(z > quantile(z, 0.6))
  g = ggplot(data_f) + 
    geom_point(aes(x, y, color = z)) +
    scale_color_gradient2(mid = "white", low = "blue", high = "red", midpoint = (max(data$z) + min(data$z)) / 2) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    geom_segment(aes(x = 0, y = 0, xend = -100, yend = 100), color = "black") +
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 100), color = "black") +
    geom_curve(aes(x = -50, y = 50, xend = 50, yend = 50), color = "black", curvature = -0.8) +
    geom_curve(aes(x = -100, y = 100, xend = 100, yend = 100), color = "black", curvature = -0.8) +
    xlim(-150, 150) + ylim(0, 210) + ggtitle(title) + coord_fixed() +
    xlab("") + ylab("") + 
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(size = 20))
  
  return(g)
}

empty_density = function() {
  return(expand.grid(x = seq(-150, 150, length.out = 100), 
                     y = seq(0, 200, length.out = 100)) %>% mutate(z = 0))
}

synthetic = function(pitcher_name, batter_name, b_hand, pitcher_pool, batter_pool, pitches_bip, p_ratio, b_ratio) {
  p_hand = pitcher_pool %>% filter(pitcher == pitcher_name)
  p_hand = unique(p_hand$p_throws)
  
  # pitcher-pitch type averages
  pitcher_avg = pitches_bip %>%
    filter(pitcher == pitcher_name) %>%
    group_by(pitch_type) %>%
    summarise(n = n())
  
  pitcher_avg$weight = pitcher_avg$n / sum(pitcher_avg$n)
  pitcher_avg = pitcher_avg %>% filter(weight > .05)
  
  pitcher_pool = pitcher_pool %>%
    filter(n >= 10) %>%
    group_by(pitcher, game_year) %>%
    filter(sum(pitcher_avg$pitch_type %in% unique(pitch_type)) >= ceiling(nrow(pitcher_avg) / 2)) %>%
    ungroup()
  
  synth_pitcher = empty_density()
  synth_batter = empty_density()
  synth_master = empty_density()
  
  pitcher_similarities_master = list()
  batter_similarities_master = list()
  
  for (pitch in pitcher_avg$pitch_type) {
    # pitcher density
    pitcher_similarities = get_similarity(pitcher_name, p_hand, pitch, p_ratio, pitcher_pool, "pitcher")
    pitcher_density = get_density(pitcher_name, p_hand, batter_name, b_hand, pitch, pitcher_similarities, pitches_bip, "pitcher")
    pitcher_similarities_master[[pitch]] = pitcher_similarities %>% mutate(pitch_type = pitch)
    synth_pitcher$z = synth_pitcher$z + pitcher_density$z * pitcher_avg$weight[pitcher_avg$pitch_type == pitch]
    n_p = sum(pitcher_similarities$similarity^2 * pitcher_similarities$n)
    
    # real matchup density
    batter_similarities = get_similarity(batter_name, b_hand, pitch, b_ratio, batter_pool, "batter")
    real_bip = get_bip(pitcher_name, batter_name, b_hand, pitch, pitches_bip)
    n = nrow(real_bip)
    
    # add real matchup back to own batter similarities if not enough observations
    if (nrow(real_bip) < 2) {
      real_density = data.frame()
      
      if (nrow(real_bip) == 1) {
        real_bip = real_bip %>%
          mutate(similarity = 1) %>%
          select(similarity, game_year, pitch_type, batter) %>%
          mutate(n = 1)
        
        batter_similarities = rbind(batter_similarities, real_bip)
      }
    } else {
      real_density = get_density(pitcher_name, p_hand, batter_name, b_hand, pitch, data.frame(), pitches_bip, "real")
    }
    
    # batter density
    bip = get_bip(pitcher_name, batter_name = "all", b_hand, pitch, pitches_bip)
    batter_density = get_density(pitcher_name, p_hand, batter_name, b_hand, pitch, batter_similarities, pitches_bip, "batter")
    batter_similarities_master[[pitch]] = batter_similarities %>% mutate(pitch_type = pitch)
    synth_batter$z = synth_batter$z + batter_density$z * pitcher_avg$weight[pitcher_avg$pitch_type == pitch]
    
    n_b = sum(batter_similarities$similarity^2 * batter_similarities$n)
    if (nrow(bip) < 10) {
      n_b = 0
    }
    
    # calculate lambda
    lambda = sqrt(n) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
    lambda_p = sqrt(n_p) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
    lambda_b = sqrt(n_b) / (sqrt(n) + sqrt(n_p) + sqrt(n_b))
    
    # calculate master
    pitch_z = pitcher_density$z * lambda_p + batter_density$z * lambda_b
    if (nrow(real_density) != 0) {
      pitch_z = pitch_z + real_density$z * lambda
    }
    
    if (lambda != 1 && lambda_p != 1 && lambda_b != 1) {
      synth_master$z = synth_master$z + pitch_z * pitcher_avg$weight[pitcher_avg$pitch_type == pitch]
    }
  }
  
  # master pitcher weights
  fangraphs_pitchers = read.csv("./data/fangraphs_pitchers.csv", stringsAsFactors = FALSE)
  pitcher_similarities_master = bind_rows(pitcher_similarities_master) %>%
    left_join(pitcher_avg, by = "pitch_type") %>%
    group_by(pitcher, game_year) %>%
    summarise(similarity = weighted.mean(similarity, weight)) %>%
    inner_join(fangraphs_pitchers, by = c("pitcher" = "Name", "game_year" = "Season")) %>%
    rename(Name = pitcher, 
           Season = game_year,
           Similarity = similarity,
           `K%` = `K.`,
           `BB%` = `BB.`) %>%
    mutate(ERA = format(ERA, nsmall = 2),
           xFIP = format(xFIP, nsmall = 2),
           fWAR = format(round(WAR, 1), nsmall = 1)) %>%
    select(Name, Season, Similarity, IP, ERA, `K%`, `BB%`, xFIP, fWAR)
  
  # master batter weights
  fangraphs_batters = read.csv("./data/fangraphs_batters.csv", stringsAsFactors = FALSE)
  batter_similarities_master = bind_rows(batter_similarities_master) %>%
    filter(n >= 10) %>%
    left_join(pitcher_avg, by = "pitch_type") %>%
    group_by(batter, game_year) %>%
    filter(all(pitcher_avg$pitch_type %in% unique(pitch_type))) %>%
    summarise(similarity = weighted.mean(similarity, weight)) %>%
    inner_join(fangraphs_batters, by = c("batter" = "Name", "game_year" = "Season")) %>%
    rename(Name = batter, 
           Season = game_year,
           Similarity = similarity,
           `wRC+` = `wRC.`) %>%
    mutate(AVG = gsub("^0(.*)", "\\1", format(AVG, nsmall = 3)),
           OBP = gsub("^0(.*)", "\\1", format(OBP, nsmall = 3)),
           SLG = gsub("^0(.*)", "\\1", format(SLG, nsmall = 3)),
           fWAR = format(round(WAR, 1), nsmall = 1)) %>%
    select(Name, Season, Similarity, PA, AVG, OBP, SLG, `wRC+`, fWAR)
  
  # traditional batter
  bip = get_bip(pitcher_name = "all", batter_name, b_hand, pitch = "all", pitches_bip)
  est = MASS::kde2d(bip$hc_x, bip$hc_y, n = 100, lims = c(-150, 150, 0, 200))
  traditional = expand.grid(x = est$x, y = est$y)
  traditional$z = as.vector(est$z)
  
  bins = bin_pitches(pitches_bip, synth_master)
  
  return(list(traditional = traditional, 
              bins = bins,
              synth_pitcher = synth_pitcher,
              synth_pitcher_similarities = pitcher_similarities_master,
              synth_batter = synth_batter, 
              synth_batter_similarities = batter_similarities_master,
              synth_master = synth_master))
}

