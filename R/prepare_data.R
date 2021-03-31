# library(osmdata)
library(dplyr)
library(sf)
library(here)
library(stringr)
library(hereR)

# source(here("R", "env.R"))
# hereR::set_key(here_key)

# ----------------------------------------------------- #
# prepare data ----------------------------

streets_data <- function(pbf_file, geo_selection_file) {
  streets <- read_sf(here("data", pbf_file),
    layer = "lines",
    query = "SELECT * FROM lines WHERE highway <> ''"
  )
  barriers <- read_sf(here("data", pbf_file),
    layer = "points",
    query = "SELECT * FROM points WHERE BARRIER <> '' "
  )

  if (!is.na(geo_selection_file)) {
    geo_selection <- read_sf(here("data", geo_selection_file))
    streets <- streets[st_intersects(streets, geo_selection, sparse = F)[, 1], ]
    barriers <- barriers[st_intersects(barriers, geo_selection, sparse = F)[, 1], ]
  }

  streets$bicycle <- str_match(streets$other_tags, '\"bicycle\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway <- str_match(streets$other_tags, '\"cycleway\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_right <- str_match(streets$other_tags, '\"cycleway:right\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_left <- str_match(streets$other_tags, '\"cycleway:left\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_both <- str_match(streets$other_tags, '\"cycleway:both\"=>\"\\s*(.*?)\\s*\"')[, 2]
  # streets$cycleway_foot <- str_match(streets$other_tags, '\"cycleway:foot\"=>\"\\s*(.*?)\\s*\"')[,2]
  streets$motorroad <- str_match(streets$other_tags, '\"motorroad\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$surface <- str_match(streets$other_tags, '\"surface\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_surface <- str_match(streets$other_tags, '\"cycleway:surface\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_right_surface <- str_match(streets$other_tags, '\"cycleway:surface:right\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_left_surface <- str_match(streets$other_tags, '\"cycleway:surface:left\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_both_surface <- str_match(streets$other_tags, '\"cycleway:surface:both\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$smoothness <- str_match(streets$other_tags, '\"smoothness\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_smoothness <- str_match(streets$other_tags, '\"cycleway:smoothness\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_right_smoothness <- str_match(streets$other_tags, '\"cycleway:right:smoothness\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_left_smoothness <- str_match(streets$other_tags, '\"cycleway:left:smoothness\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_both_smoothness <- str_match(streets$other_tags, '\"cycleway:both:smoothness\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$access <- str_match(streets$other_tags, '\"access\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$oneway <- str_match(streets$other_tags, '\"oneway\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$oneway_bicycle <- str_match(streets$other_tags, '\"oneway:bicycle\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_right_oneway <- str_match(streets$other_tags, '\"cycleway:right:oneway\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_left_oneway <- str_match(streets$other_tags, '\"cycleway:left:oneway\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$is_sidepath <- str_match(streets$other_tags, '\"is_sidepath\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$use_sidepath <- str_match(streets$other_tags, '\"use_sidepath\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$bicycle_road <- str_match(streets$other_tags, '\"bicycle_road\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$embedded_rails <- str_match(streets$other_tags, '\"embedded_rails\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$maxspeed <- str_match(streets$other_tags, '\"maxspeed\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$incline_across <- str_match(streets$other_tags, '\"incline:across\"=>\"\\s*(.*?)\\s*\"')[, 2] # proposed tag
  streets$traffic <- str_match(streets$other_tags, '\"traffic\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$segregated <- str_match(streets$other_tags, '\"segregated\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$width <- str_match(streets$other_tags, '\"width\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_width <- str_match(streets$other_tags, '\"cycleway:width\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_right_width <- str_match(streets$other_tags, '\"cycleway:right:width\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_left_width <- str_match(streets$other_tags, '\"cycleway:left:width\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$cycleway_both_width <- str_match(streets$other_tags, '\"cycleway:both:width\"=>\"\\s*(.*?)\\s*\"')[, 2]
  streets$width_cleaned <- gsub("[^0-9.-]", "", streets$width) %>% as.numeric()
  streets$cycleway_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_width) %>% as.numeric()
  streets$cycleway_right_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_right_width) %>% as.numeric()
  streets$cycleway_left_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_left_width) %>% as.numeric()
  streets$cycleway_both_width_cleaned <- gsub("[^0-9.-]", "", streets$cycleway_both_width) %>% as.numeric()
  streets$maxspeed_cleaned <- gsub("[^0-9.-]", "", streets$maxspeed) %>% as.numeric()

  # combine all cycleway variables to one
  streets <- streets %>%
    mutate(
      cycleway_combined = coalesce(cycleway, cycleway_right, cycleway_left, cycleway_both),
      cycleway_width_combined = coalesce(
        cycleway_width_cleaned, cycleway_right_width_cleaned,
        cycleway_left_width_cleaned, cycleway_both_width_cleaned
      )
    )
  streets[streets$highway == "cycleway", ]$cycleway_width_combined <- streets[streets$highway == "cycleway", ]$width_cleaned

  # all road types rated with "0" (not allowed / possible to ride on for cargo bikes)
  streets <- streets %>%
    filter(
      !bicycle %in% c("no", "private") | is.na(bicycle), # bikes not allowed
      !cycleway_combined %in% c("separate", "use_sidepath") | is.na(cycleway_combined), # seperate or use_sidepath indicates own line for cycleway
      !access %in% c(
        "agricultural", "customers", "delivery", "private",
        "permit", "bus", "public_transport", "emergency", "forestry"
      ),
      highway != "proposed",
      highway != "motorway", # Autobahn
      highway != "motorway_link", # Autobahnauffahrt
      (highway != "trunk") | (bicycle == "yes"), # Schnellstraße
      (highway != "trunk_link") | (bicycle == "yes"), # Schnellstraße Auffahrt
      highway != "bus_guideway", # Suprbus-Strecke
      highway != "escape", # Notbresmweg
      (highway != "pedestrian") | (bicycle == "yes"), # Fußgängerzone
      (highway != "footway") | (bicycle == "yes"), # Fußweg
      (highway != "bridleway") | (bicycle == "yes"), # Reitweg
      highway != "steps",
      highway != "corridor", # Gang in inneren eines Gebäudes
      (motorroad != "yes") | is.na(motorroad) # keine Kraftfahrstraße
    )

  ### Parks ###
  # TODO: upgrade if road in park?
  # parks <- opq(bbox = city) %>%
  #   add_osm_feature(key = 'leisure', value = 'park') %>%
  #   osmdata_sf ()

  ### Incline across (Querneigung) ###
  # TODO

  ### Barriers ###
  barriers$kerb <- str_match(barriers$other_tags, '\"kerb\"=>\"\\s*(.*?)\\s*\"')[, 2]
  barriers$maxwidth <- str_match(barriers$other_tags, '\"maxwidth\"=>\"\\s*(.*?)\\s*\"')[, 2]
  barriers$maxwidth_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth) %>% as.numeric()
  barriers$maxwidth_physical <- str_match(barriers$other_tags, '\"maxwidth:physical\"=>\"\\s*(.*?)\\s*\"')[, 2]
  barriers$maxwidth_physical_cleaned <- gsub("[^0-9.-]", "", barriers$maxwidth_physical) %>% as.numeric()
  barriers <- barriers %>%
    mutate(maxwidth_barriers_combined = pmin(maxwidth_cleaned, maxwidth_physical_cleaned, na.rm = T))

  cycle_barriers <- barriers %>%
    filter(barrier == "cycle_barrier") %>%
    st_transform(3035)

  bollards <- barriers %>%
    filter(barrier == "bollard") %>%
    st_transform(3035)

  blocks <- barriers %>%
    filter(barrier == "block") %>%
    st_transform(3035)

  lift_gates <- barriers %>%
    filter(barrier == "lift_gate") %>%
    st_transform(3035)

  kerbs <- barriers %>%
    filter(barrier == "kerb") %>%
    st_transform(3035)

  streets_3035 <- st_transform(streets, 3035)
  # get streets with barriers
  streets$has_cycle_barrier <- st_intersects(streets_3035, st_union(cycle_barriers), sparse = F)[, 1]
  streets$has_bollard <- st_intersects(streets_3035, st_union(bollards), sparse = F)[, 1]
  streets$has_block <- st_intersects(streets_3035, st_union(blocks), sparse = F)[, 1]
  streets$has_lift_gate <- st_intersects(streets_3035, st_union(lift_gates), sparse = F)[, 1]
  streets$has_kerb <- st_intersects(streets_3035, st_union(kerbs), sparse = F)[, 1]

  streets_3035 <- st_transform(streets, 3035)

  # get width of barrier for streets (to intersect everything in one step is too large of an operation)
  cycle_barrier_widths <- streets_3035 %>%
    filter(has_cycle_barrier) %>%
    st_intersection(select(cycle_barriers, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_cycle_barrier = min(maxwidth_barriers_combined)) %>%
    ungroup()

  bollard_barrier_widths <- streets_3035 %>%
    filter(has_bollard) %>%
    st_intersection(select(bollards, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_bollard = min(maxwidth_barriers_combined)) %>%
    ungroup()

  block_barrier_widths <- streets_3035 %>%
    filter(has_block) %>%
    st_intersection(select(blocks, c(barrier, maxwidth_barriers_combined))) %>%
    st_drop_geometry() %>%
    select(osm_id, maxwidth_barriers_combined) %>%
    group_by(osm_id) %>%
    summarise(width_block = min(maxwidth_barriers_combined)) %>%
    ungroup()

  kerb_height <- streets_3035 %>%
    filter(has_kerb) %>%
    st_intersection(select(kerbs, c(barrier, kerb))) %>%
    st_drop_geometry() %>%
    select(osm_id, kerb) %>%
    group_by(osm_id) %>%
    summarise(kerb = min(kerb)) %>%
    ungroup()

  streets <- streets %>%
    left_join(cycle_barrier_widths, by = "osm_id") %>%
    left_join(bollard_barrier_widths, by = "osm_id") %>%
    left_join(block_barrier_widths, by = "osm_id") %>%
    left_join(kerb_height, by = "osm_id")


  # create own variables out of tags ----------------------------

  # do you have to cycle on the car road here?
  streets <- streets %>%
    mutate(cycle_on_road = case_when(
      highway %in% c("path", "footway", "pedestrian", "bridleway") ~ F,
      # bicycle %in% c("designated", "use_sidepath") ~ F,
      cycleway_combined %in% c("no", "none", "shared", "shared_lane") ~ T,
      (!is.na(cycleway_combined)) ~ F,
      TRUE ~ T
    ))

  # get one surface variable that combines all options for surface (cycleway:surface, cyclway:right:surface, ...)
  streets <- streets %>%
    mutate(
      cycleway_surface_combined = coalesce(cycleway_surface, cycleway_right_surface, cycleway_left_surface),
      cycleway_smoothness_combined = coalesce(cycleway_smoothness, cycleway_right_smoothness, cycleway_left_smoothness)
    ) %>%
    mutate(
      surface_combined = case_when(
        highway == "cycleway" ~ surface,
        !is.na(cycleway_surface_combined) ~ cycleway_surface_combined,
        cycle_on_road | (highway %in% c("path", "pedestrian", "footway", "bridleweay")) ~ surface, # if shared street use same surface
        cycleway_combined %in% c("lane", "opposite_lane") ~ surface, # if lane on road assume same surface as street
      ),
      smoothness_combined = case_when(
        highway == "cycleway" ~ smoothness,
        !is.na(cycleway_smoothness_combined) ~ cycleway_smoothness_combined,
        cycle_on_road | (highway %in% c("path", "pedestrian", "footway", "bridleweay")) ~ smoothness, # if shared street use same surface
        cycleway_combined %in% c("lane", "opposite_lane") ~ smoothness, # if lane on road assume same surface as street
      )
    )

  # label barriers
  streets <- streets %>%
    mutate(
      which_barrier = case_when(
        has_cycle_barrier ~ "Umlaufsperre",
        has_bollard ~ "Poller",
        has_block ~ "Block",
        has_kerb ~ "Bordstein",
        has_lift_gate ~ "Schranke",
      ),
      maxwidth_combined = coalesce(width_cycle_barrier, width_bollard, width_block)
    )

  # --------------------------------------------------------- #
  # further data sources -------------------------------------

  ## Car traffic
  # for testing only get traffic for xhain
  # xhain <- read_sf(here("data", "bezirke.geojson")) %>%
  #   filter(name == "Friedrichshain-Kreuzberg")
  # traffic_xhain <- hereR::flow(xhain)
  # traffic_xhain <- traffic_xhain %>%
  #   mutate(traffic_index = 10 - JF) %>%
  #   st_transform(3035) %>%
  #   st_buffer(5) # set 5 m buffer

  # TODO: map car traffic onto osm streets
  # streets_with_traffic <- streets %>%
  #   st_transform(3035) %>%
  #   st_within(traffic_xhain, sparse = F)
  #
  # streets_with_traffic <- streets[streets_with_traffic[,1], ]%>%
  #   select(osm_id, traffic_index) %>%
  #   st_drop_geometry()
  #
  # streets <- streets %>%
  #   left_join(streets_with_traffic, by = "osm_id")


  ## pedestrian traffic
  # TODO:
  # - get traffic on shared streets
  # - include market places & opening hours
  # - use heuristics: avoid saturday / (sundays -> in parks) afternoons on pedestrian roads

  # st_write(streets, here("data", "streets_berlin.gpkg"))
  # streets <- read_sf(here("data", "streets_berlin.gpkg"))


  # --------------------------------------------------------- #
  # set "cargo bikability" values ----------------------------

  streets <- streets %>%
    mutate(
      cbindex_cycleways = case_when(
        bicycle_road == "yes" ~ 5,
        width_cleaned >= 2 & highway == "cycleway" ~ 5,
        width_cleaned >= 1.6 & width_cleaned <= 2 & highway == "cycleway" ~ 4,
        width_cleaned >= 1.2 & width_cleaned <= 1.6 & highway == "cycleway" ~ 3,
        width_cleaned < 1.2 & highway == "cycleway" ~ 1,
        highway == "cycleway" ~ 3, # default ohne width
        # TODO: cycleway separat für beide Richtungen darstellen
        cycleway_width_cleaned >= 2 & cycleway_combined %in% c("track", "lane") ~ 5,
        cycleway_width_cleaned >= 1.6 & cycleway_width_cleaned <= 2 & cycleway_combined %in% c("track", "lane") ~ 4,
        cycleway_width_cleaned >= 1.2 & width_cleaned <= 1.6 & cycleway_combined == "lane" ~ 4,
        cycleway_width_cleaned >= 1.2 & width_cleaned <= 1.6 & cycleway_combined == "track" ~ 3, # schmalerer track schlechter als schmale lane
        width_cleaned < 1.2 & cycleway_combined == "lane" ~ 2,
        width_cleaned < 1.2 & cycleway_combined == "track" ~ 1,
        cycleway_combined == "lane" ~ 4,
        cycleway_combined == "track" ~ 3,
        cycleway_combined == "opposite_lane" ~ 5, # eigene Spur für Gegenrichtung in Einbahnstraße
        cycleway_combined == "opposite" ~ 2, # keine Spur für Gegenrichtung in der Einbahnstraße
        cycleway_combined == "share_busway" ~ 3,
        # oneway_bicycle == "yes" ~ 8, #  how should this be rated?
        maxspeed <= 30 | highway == "residential" | highway == "living_street" ~ 4, # 30 kmh Zonen
        highway == "path" & segregated == "yes" ~ 4, # gibt es einen separaten Radweg?
        highway == "footway" & segregated == "yes" ~ 4,
        highway == "pedestrian" & segregated == "yes" ~ 4,
        highway == "path" ~ 2,
        highway == "footway" ~ 2,
        highway == "pedestrian" ~ 2,
        highway == "track" ~ 1,
        highway == "service" ~ 2,
        highway == "trunk" ~ 2,
        highway == "trunk_link" ~ 2,
        highway == "primary" ~ 1, # Hauptstraße ohne Radwege
        highway == "primary_link" ~ 1,
        highway == "secondary" ~ 2,
        highway == "secondary_link" ~ 2,
        highway == "tertiary" ~ 3,
        highway == "tertiary_link" ~ 3,
        highway == "bridleway" ~ 1,
        highway == "road" ~ 2,
        highway == "unclassified" ~ 2,
        maxspeed >= 50 ~ 1,
        TRUE ~ 3
        # TODO: two way cycleways?
      ),
      # TODO: properly distinguish between cycleway, right, left and road
      cbindex_surface = case_when( # first: test if cycleway attributes are present. Then check general smoothness
        smoothness_combined == "excellent" ~ 5,
        smoothness_combined == "good" ~ 4,
        smoothness_combined == "intermediate" ~ 3,
        smoothness_combined == "bad" ~ 2,
        smoothness_combined == "very bad" ~ 1,
        smoothness_combined == "horrible" ~ 0,
        smoothness_combined == "very horrible" ~ 0,
        smoothness_combined == "impassable" ~ 0,
        surface_combined == "paved" ~ 4,
        surface_combined == "asphalt" ~ 5,
        surface_combined == "paving_stones" ~ 4,
        surface_combined == "concrete" ~ 4,
        surface_combined == "sett" ~ 2,
        surface_combined == "cobblestone" | surface_combined == "cobblestone:flattened" ~ 2,
        surface_combined == "unhewn_cobblestone" ~ 1,
        surface_combined == "compacted" ~ 3,
        surface_combined == "fine_gravel" ~ 2,
        surface_combined == "metal" ~ 3,
        surface_combined == "rock" ~ 0,
        surface_combined == "sand" ~ 0,
        surface_combined == "mud" ~ 0,
        surface_combined %in% c(
          "unpaved", "grass", "ground", "gravel", "dirt",
          "pebblestone", "earth", "grass_paver", "woodchips"
        ) ~ 1
      ) %>% as.numeric(),
      cbindex_barrier = case_when(
        has_cycle_barrier ~ 0,
        has_lift_gate ~ 0,
        has_bollard & width_bollard < 0.9 ~ 0,
        has_bollard & width_bollard < 1.0 ~ 1,
        has_bollard & width_bollard < 1.2 ~ 2,
        has_bollard & width_bollard < 1.5 ~ 4,
        has_bollard & width_bollard >= 1.5 ~ 5,
        has_bollard ~ 4, # default: bollard is passable if no width given
        has_block & width_block < 0.9 ~ 0, # default: block is passable if no width given
        has_block & width_block < 1.0 ~ 1,
        has_block & width_block < 1.2 ~ 2,
        has_block & width_block < 1.5 ~ 4,
        has_block & width_block >= 1.5 ~ 5,
        has_block ~ 4,
        has_kerb & kerb == "flush" ~ 5,
        has_kerb & kerb == "lowered" ~ 3,
        has_kerb & kerb == "raised" ~ 1,
        has_kerb ~ 2
        # has_cycle_barrier & width_cycle_barrier > 1.5 ~ 8
      ),
      cbindex_pedestrians = case_when(
        bicycle == "dismount" ~ 1,
        highway == "pedestrian" & (segregated != "yes" | is.na(segregated)) ~ 5, # Fußgängerzone
        highway == "footway" & (segregated != "yes" | is.na(segregated)) ~ 5, # Gehweg Fahrrad frei
        segregated == "no" ~ 5 # keine Trennung von Fuß- und Gehweg
      )
      # cbindex_traffic = ifelse(cycle_on_road, traffic_index, NA),
    )

  # ----------------------------------------------------- #
  # combine to a single index ----------------------------

  streets <- streets %>%
    mutate(
      cbindex_street_quality = case_when(
        !is.na(cbindex_surface) & !is.na(cbindex_cycleways) ~ round(sqrt(cbindex_surface * cbindex_cycleways), 1),
        is.na(cbindex_surface) ~ cbindex_cycleways,
        is.na(cbindex_cycleways) ~ cbindex_surface
      ),
      cbindex = case_when(
        !is.na(cbindex_street_quality) & !is.na(cbindex_barrier) ~ round(sqrt(cbindex_surface * cbindex_cycleways), 1),
        is.na(cbindex_street_quality) ~ cbindex_barrier,
        is.na(cbindex_barrier) ~ cbindex_street_quality
      )
    )

  return(streets %>%
    select(-waterway, -aerialway, -barrier, -man_made, -z_order, -other_tags))
}
