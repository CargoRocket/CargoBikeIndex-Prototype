city_name <- "stgt"
# delete_cache <- F

# pbf_file <- dplyr::case_when(
#   # Stuttgart pbf file: https://download.geofabrik.de/europe/germany/baden-wuerttemberg/stuttgart-regbez.html
#   city_name == "stuttgart" ~ "stuttgart-regbez-latest.osm.pbf",
#   city_name == "bw" ~ "baden-wuerttemberg-latest.osm.pbf",
#   # Berlin pbf file: https://download.geofabrik.de/europe/germany/berlin.html
#   city_name == "xhain" ~ "berlin-latest.osm.pbf",
#   city_name == "augsburg" ~ "schwaben-latest.osm.pbf"
# )
# geo_selection_file <- dplyr::case_when(
#   city_name == "stuttgart" ~ "stuttgart.geojson",
#   city_name == "bw" ~ "",
#   city_name == "xhain" ~ "xhain.geojson",
#   city_name == "augsburg" ~ "augsburg.geojson"
# )
