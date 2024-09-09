# function to add level of traffic stress to network, based on cycleway type,
# highway class, traffic (ADT) and speed, and related impedance

# traffic volumes (eg ADT 10000) are for two-way traffic, so are halved
# (eg 10000 / 2) in order to apply to the one-way links in edges_current

addLTS <- function(nodes_current, edges_current,
                   excludeInadequateLanes = F) {
 
  # testing
  # nodes_current <- networkTraffic[[1]]
  # edges_current <- networkTraffic[[2]]
  # excludeInadequateLanes = T
  
  # optional step to convert on-road lanes to mixed traffic if they are not 
  # wide enough, or if parking is allowed on them (requires use of specific
  # tags dealing with cycle lane width, traffic and parking conditions)
  if (excludeInadequateLanes) {
    inadequate.lane.links <- getInadequateLaneLinks(edges_current)
    edges_current <- edges_current %>%
      mutate(cycleway_orig = cycleway,
             cycleway = ifelse(link_id %in% inadequate.lane.links,
                               "inadequate_lane", cycleway))
  }

  # assign LTS to edges 
  # '1' to '4' are categories of increasing stress, as per table below]
  
  # road groups
  local <- c("residential", "road", "unclassified", "living_street", "service")
  tertiary <- c("tertiary", "tertiary_link")
  secondary <- c("secondary", "secondary_link")
  primary <- c("primary", "primary_link")
  
  
  # Calculate edge LTS based on cycleway type, highway classification (highway), 
  # speed (freespeed) and traffic (ADT)
  edges_current <- edges_current %>%
    # make speed field (rounded, to avoid floating point issues)
    mutate(speed = round(freespeed * 3.6)) %>%
    # add LTS 
    mutate(lvl_traf_stress = case_when(
      
      # LTS 1 - off-road paths
      cycleway %in% c("bikepath", "shared_path")                   ~ 1,
      highway %in% c("cycleway", "track", "pedestrian", 
                     "footway", "path", "corridor", "steps")       ~ 1,
      
      # LTS 1 - separated cycle lanes
      cycleway == "separated_lane" & speed <= 50                   ~ 1,
      
      # LTS 1 - on-road cycle lanes
      cycleway == "simple_lane" & 
        highway %in% c(local, tertiary, secondary) &
        ADT <= 10000 /2 & speed <= 30                              ~ 1,
      
      # LTS 1 - mixed traffic
      highway %in% local & ADT <= 2000 / 2 & speed <= 30           ~ 1,
      
      # LTS 2 - separated cycle lanes
      cycleway == "separated_lane" & speed <= 60                   ~ 2,
      
      # LTS 2 - on-road cycle lanes
      cycleway == "simple_lane" &
        highway %in% c(local, tertiary, secondary) &
        ADT <= 10000 / 2 & speed <= 50                             ~ 2,
      cycleway == "simple_lane" &
        (highway %in% primary | 
           (highway %in% c(local, tertiary, secondary) & 
              ADT > 10000 / 2)) &
        speed <= 40                                                ~ 2,
      
      # LTS 2 - mixed traffic
      highway %in% local & ADT <= 750 / 2 & speed <= 50            ~ 2,
      highway %in% local & ADT <= 2000 / 2 & speed <= 40           ~ 2,
      highway %in% c(local, tertiary) & ADT <= 3000 / 2 & speed <= 30  ~ 2,
      
      # LTS 3 - on-road cycle lanes
      cycleway == "simple_lane" & speed <= 60                      ~ 3,
      
      # LTS 3 - mixed traffic
      highway %in% local & ADT <= 750 / 2 & speed <= 60                ~ 3,
      highway %in% c(local, tertiary) & ADT <= 3000 / 2 & speed <= 50  ~ 3,
      (highway %in% c(secondary, primary) |
         (highway %in% c(local, tertiary) & ADT > 3000 / 2)) &
        speed <= 40                                                ~ 3,
        
      # LTS 4 - everything not covered above
      TRUE                                                         ~ 4
    ))
  
  # check to test how many in each category
  # LTS_table <- edges_current %>%
  #   st_drop_geometry() %>%
  #   group_by(highway, lvl_traf_stress) %>%
  #   summarise(n = n())
  

  # Assign LTS to nodes, based on highest 
  # begin with all nodes (from and to) and the LTS level of the associated link
  node_max_lookup <- rbind(edges_current %>%
                             st_drop_geometry() %>%
                             dplyr::select(id = from_id, LTS = lvl_traf_stress),
                           edges_current %>%
                             st_drop_geometry() %>%
                             dplyr::select(id = to_id, LTS = lvl_traf_stress)) %>%
    group_by(id) %>%
    # find highest level of LTS for links connecting with the node
    summarise(max_LTS = max(LTS)) %>%
    ungroup()
  
  
  # Calculate impedance  for intersection, and total impedance
  
  # Impedance for intersection applies to the to-node (the intersection
  # that the link arrives at), and only if it's unsignalised
  # penalty is calculated as: 
  #     penaltya = (Buffb – Buffa) * (IFb – 1)for
  # where 
  #   a is the link for which the penalty (penaltya) is being calculated
  #   b is the highest-ranked other link at the relevant intersection
  #   Buffa and Buffb are the buffer distances for a and b, where the buffer 
  #     distance is 0, 5, 10 or 25m for a link of LTS 1, 2, 3 or 4 respectively
  #   IFb is the impedance factor for b, where the impedance factor is 1.00, 
  #     1.05, 1.10 or 1.15 for a link of LTS 1, 2, 3 or 4 respectively
  
  # LTS impedance, which is to be added to the length of the link and any other 
  # impedances (outside this function) to create the weight for the link,
  # is the length-based impedance for the link plus its intersection impedance.
  # - Length-based impedance is the link multiplied by its impedance factor
  #   minus 1 (that is, subtracting 1 so it os only the additional impedance,
  #   not the length itself:
  #     total_imped = length * (IFa - 1)
  #   where IFa is the impedance factor for a, where the impedance factor is 1.00, 
  #   1.05, 1.10 or 1.15 for a link of LTS 1, 2, 3 or 4 respectively
  # - Intersection impedance is calcualted as above
  
  buff_imped_df <- data.frame(cbind(LTS = c(1, 2, 3, 4),
                              buffer = c(0, 5, 10, 25),
                              imped = c(1, 1.05, 1.10, 1.15)))
  
  edges_current <- edges_current %>%
    # join node intersection details for the to-node
    left_join(., nodes_current %>%
                st_drop_geometry() %>%
                dplyr::select(id, type),
              by = c("to_id" = "id")) %>%
    # join the node max LTS buffer & impedance details for the to-node 
    left_join(., node_max_lookup, by = c("to_id" = "id")) %>%
    left_join(., buff_imped_df, by = c("lvl_traf_stress" = "LTS")) %>%
    # and the buff_imped_df details for the max LTS
    left_join(., buff_imped_df, by = c("max_LTS" = "LTS"), suffix = c(".a", ".b")) %>%
    
    # calculate intersection impedance, using formula above (unsignalised only)
    mutate(intersec_imped = ifelse(type %in% c("simple_intersection", 
                                                "simple_roundabout"),
                                    (buffer.b - buffer.a) * (imped.b - 1),
                                    0)) %>%
    # calculate total LTS impedance (to be added to length along with other impedances)
    mutate(LTS_imped = (length * (imped.a - 1)) + intersec_imped) %>%
    
    # remove unwanted fields
    dplyr::select(-speed, -type, -max_LTS, -buffer.a, -buffer.b, 
                  -imped.a, -imped.b, -intersec_imped)
  
  # restore original cycleway tags if they had been altered by 'excludeInadequateLanes'
  if (excludeInadequateLanes) {
    edges_current <- edges_current %>%
      mutate(cycleway_orig = cycleway_orig) %>%
  dplyr::select(-cycleway_orig)
  }
  
  return(list(nodes_current, edges_current))
}


# alternative function which invokes 'addAssumedTraffic' to include assumed
# traffic volumes where simulated volumes aren't available, and then applies
# 'addLTS'

addLTSAssumedTraffic <- function(input.network,
                                 excludeInadequateLanes = F) {
  
  # input.network <- networkOneway
  
  cat(paste0(as.character(Sys.time()), ' | ', 
             "Adding level of traffic stress, based on assumed traffic volumes\n"))  

  input.nodes <- input.network[[1]]
  input.links <- input.network[[2]]
  
  input.links.with.assumed.traffic <- addAssumedTraffic(input.nodes,
                                                        input.links)
  
  network.with.LTS <- addLTS(input.links.with.assumed.traffic[[1]],
                             input.links.with.assumed.traffic[[2]],
                             excludeInadequateLanes)
  
  return(list(network.with.LTS[[1]],
              network.with.LTS[[2]]))
  
}

# function to add assumed traffic, where actual/simulated figures
# aren't available, being the lowest volume for each category where used in 'addLTS'
addAssumedTraffic <- function(network.nodes, network.links) {
  
  # road groups
  local <- c("residential", "road", "unclassified", "living_street", "service")
  tertiary <- c("tertiary", "tertiary_link")
  secondary <- c("secondary", "secondary_link")
  
  # add assumed traffic - figures from 'addLTS.R' grid, divided by 2 because the grid
  # uses 2-way traffic and these are figures applied to 1-way links
  links.with.traffic <- network.links %>%
    mutate(ADT = case_when(
      highway %in% local     ~ 750 / 2,
      highway %in% tertiary  ~ 3000 / 2,
      highway %in% secondary ~ 10000 / 2,
      TRUE                   ~ NA
    ))
  
  return(list(network.nodes, links.with.traffic))
}



# function to exclude on-road cycle lanes that don't meet width and parking
# separation criteria (so they will be treated as mixed traffic instead)

getInadequateLaneLinks <- function(edges_current) {
  
  # edges_current <- networkOneway[[2]]
  
  # test for adequacy for bikelane width and parking separation, for simple lanes
  ## summary of inadequate lanes:
  ## - if bikelane width + buffer from traffic + buffer from parking less than:
  ##   - 1.2m if no adjacent parking
  ##   - 1.8m if adjacent parking
  ## - if parking allowed on lane

  
  requiredtags <- c("bikelane_left_width", "bikelane_right_width",
                    "bikelane_left_buff_left", "bikelane_left_buff_right", 
                    "bikelane_left_traf_left", "bikelane_left_traf_right",
                    "bikelane_right_buff_left", "bikelane_right_buff_right",
                    "bikelane_right_traf_left", "bikelane_right_traf_right",
                    "bikelane_left_lane", "bikelane_right_lane")
 
  if (all(requiredtags %in% colnames(edges_current))) {
    
    # calculate adequacy
    inadequate.lane.links <- edges_current %>%
      
      # extract cycle lanes and their required tags
      st_drop_geometry() %>%
      filter(cycleway == "simple_lane") %>%
      dplyr::select(link_id, cycleway, all_of(requiredtags)) %>%
      
      # part 1 - adequacy of lane width plus any buffers
      
      # calculate total widths of lanes plus buffer (note that NAs become zero)
      mutate(width_left = rowSums(pick(bikelane_left_width, bikelane_left_buff_left,
                                       bikelane_left_buff_right), na.rm = T),
             width_right = rowSums(pick(bikelane_right_width, bikelane_right_buff_left,
                                        bikelane_right_buff_right), na.rm = T)) %>%
      
      # calculate required widths (1.8 if adjacent parking, otherwise 1.2)
      mutate(required_width_left = if_else(
        coalesce(bikelane_left_traf_left == "parking", FALSE) |
          coalesce(bikelane_left_traf_right == "parking", FALSE),
        1.8, 1.2),
        required_width_right = if_else(
          coalesce(bikelane_right_traf_left == "parking", FALSE) |
            coalesce(bikelane_right_traf_right == "parking", FALSE),
          1.8, 1.2)) %>%
      
      # determine whether lane is present and adequate (noting that 0 width is unspecified)
      mutate(adequate_width_left = case_when(
        width_left >= required_width_left ~ "adequate",
        width_left > 0 & width_left < required_width_left ~ "inadequate",
        .default = "unspecified"
      ), adequate_width_right = case_when(
        width_right >= required_width_right ~ "adequate",
        width_right > 0 & width_right < required_width_right ~ "inadequate",
        .default = "unspecified"
      )) %>%
      
      # determine adequacy of width - determined by left lane (that is, we assume
      # most lanes are on the left), unless left is inadequate and right is 
      # present and adequate - use 0 for inadequate, 1 for adequate
      # or undetermined
      mutate(adequate_width = ifelse(adequate_width_left == "inadequate" & 
                                       !adequate_width_right == "adequate",
                                     0, 1)) %>%
      
      # part 2 - parking allowed
      
      # determine whether there is shared paraking on left and right lanes - 
      # inadequate if 'share_parking'' adequate if some other entry (most are
      # 'exclusive', thought here are others; unspecified if NA)
      mutate(
        adequate_shareparking_left = case_when(
          is.na(bikelane_left_lane) ~ "unspecified",
          bikelane_left_lane == "share_parking" ~ "inadequate",
          .default = "adequate",
        ),
        adequate_shareparking_right = case_when(
          is.na(bikelane_right_lane) ~ "unspecified",
          bikelane_right_lane == "share_parking" ~ "inadequate",
          .default = "adequate")) %>%
    
      # determine adequacy of shared parking position - determined by left lane
      # (that is, we assume most lanes are on the left), unless left is inadequate
      # and right is present and adequate - use 0 for inadequate, 1 for
      # adequate or undetermined
      mutate(adequate_shareparking = ifelse(adequate_shareparking_left == "inadequate" &
                                              !adequate_shareparking_right == "adequate",
                                            0, 1)) %>%
      
      # determine overall adequacy - fails if either width or shareparking is 
      # inadequate - use 1 for inadequate and 0 for adequate
      mutate(inadequate_lane = ifelse(adequate_width == 0 | adequate_shareparking == 0,
                                      1, 0)) %>%
      
      # keep the link_ids of the inadequate lanes
      filter(inadequate_lane == 1) %>%
      .$link_id
    
    return(inadequate.lane.links)

  } else {
    message("Tags needed to calculate adequacy of bikeline width and parking separation are not present; skipping.")
    return(c())
  }
  
}
