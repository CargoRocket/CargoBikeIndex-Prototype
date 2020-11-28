library(osmdata)
library(mapview)
library(dplyr)
library(mapdeck)
library(sf)

set_token("TOKEN")

# get data once and store to file
#city <- 'cologne germany'
#city <- 'stuttgart germany'
#city <- 'tübingen germany'
city <- 'freiburg im breisgau germany'


center <- case_when(city == "cologne germany" ~ c(6.9598, 50.9412),
                    city == "stuttgart germany" ~ c(9.177, 48.7766),
                    city == "tübingen germany" ~ c(9.0520, 48.5189),
                    city == "freiburg im breisgau germany" ~ c(7.8353, 47.9977))

roads <- opq(bbox = city) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf ()

roads_line <- roads$osm_lines

saveRDS(roads_line, "osm_roads")
roads_line_raw <- readRDS("osm_roads")
#saveRDS(roads_line, "osm_roads_stgt")
roads_line_raw <- readRDS("osm_roads_stgt")

roads_line <- roads_line_raw

roads_line <- roads_line %>% 
  filter(highway != "motorway") %>% 
  filter(highway != "motorway_link") %>% 
  filter(highway != "trunk") %>% 
  filter(highway != "trunk_link") %>% 
  filter((highway != "pedestrian") | (bicycle == "yes")) %>% 
  filter(highway != "footway" | bicycle == "yes") %>% 
  filter(highway != "steps") %>% 
  filter(highway != "corridor") %>% 
  filter(motorroad != "yes" | is.na(motorroad))

parks <- opq(bbox = city) %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf ()

barriers <- opq(bbox = city) %>%
  add_osm_feature(key = 'barrier', value = c('cycle_barrier')) %>% #, 'bollard'
  osmdata_sf ()
  
# road width
road_width <- roads_line$width %>% table
roads_line["width_cleaned"] <- gsub("[^0-9.-]", "",  roads_line$width) %>% as.numeric()

roads_line <- roads_line %>%
  mutate(
    cargoindex_width = case_when(
      width_cleaned >= 2 & highway == "cycleway" ~ 1,
      width_cleaned >= 1.5 & width_cleaned <=2 & highway == "cycleway" ~ 0.5,
      width_cleaned < 1.5 ~ 0,
    )
  )

roads_line <- roads_line %>%
  mutate(
    cargoindex_surface = case_when(
      smoothness == "excellent" ~ 1,
      smoothness == "good" ~ 1,
      smoothness == "intermediate" ~ 0.5,
      smoothness == "bad" ~ 0,
      smoothness == "very bad" ~ 0,
      smoothness == "horrible" ~ 0,
      smoothness == "very horrible" ~ 0,
      smoothness == "impassable" ~ 0,
      surface == "asphalt" ~ 1,
      surface == "paving_stones" ~ 1,
      surface == "concrete" ~ 0.8,
      surface == "cobblestone" ~ 0.1,
      surface == "sett" ~ 0.1,
      surface == "sand" ~ 0.1,
      surface == "ground" ~ 0,
      surface == "unpaved" ~ 0,
      TRUE ~ 0.5
    )
  )

# is this a oneway street?
roads_line[roads_line$oneway == "yes" & 
             roads_line$oneway.oneway.bicycle == "no" , "cargoindex_oneway"] <- 0.2

### prefer bike lanes
roads_line[identical(roads_line$highway, "cycleway"), "cargoindex_cycleway"] <- 1
roads_line[identical(roads_line$bicycle, "designated"), "cargoindex_cycleway"] <- 1
roads_line[!is.na(roads_line$cycleway), "cargoindex_cycleway"] <- 0.8
roads_line[is.na(roads_line$cargoindex_cycleway), "cargoindex_cycleway"] <- 0.5

# prefer roads in parks
roads_line["in_park"] <- st_intersects(roads_line, st_union(parks$osm_multipolygons), sparse = F)
roads_line[roads_line$roads_line, "cargoindex_parkroad"] <- 1.2

roads_line <- roads_line %>% 
  rowwise() %>% 
  mutate(cargoindex = prod(c(cargoindex_width, cargoindex_surface, cargoindex_oneway, 
                             cargoindex_cycleway, cargoindex_parkroad, 100), na.rm = T)) %>% 
  st_as_sf() %>% 
  select(highway, width, surface, cycleway, oneway, oneway.bicycle, cargoindex_width, 
         cargoindex_surface, cargoindex_oneway, cargoindex_cycleway, cargoindex_parkroad, cargoindex)


mapdeck(style = mapdeck_style("light"), zoom = 12, location = center) %>% 
  add_path(data = roads_line, stroke_colour = "cargoindex", stroke_width = 5, legend = T, tooltip = "surface", update_view = F) %>% 
  add_scatterplot(data = barriers$osm_points, radius = 20, fill_colour = "#b30000", update_view = F)
