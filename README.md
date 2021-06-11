<div align="center">
  <img src="https://cologne.xatellite.io/logo-pfade.svg" alt="Logo" height="100px" />

  CargoRocket CargoBikeIndex
  ---
</div>
**DEPRECATED:Map with CargoBikeIndex is now directly integrated into [the website](https://github.com/CargoRocket/website/blob/master/assets/js/cargobikeindex.js)**


R ShinyApp to create and visualize the CargoBikeIndex.

See live demo: [Stuttgart](https://cargorocket.shinyapps.io/index_stuttgart/)

### Score Interpretation

A score of 0 means not usable for cargo bikes. 
A score of 5 means perfect conditions.

### Run the CargoBikeIndex app

Run `app.R` to create a ShinyApp with a CargoBikeIndex Map. It shows an overall CargoBikeIndex, as well as the underlying parameters, such as street quality, surface, road type, barriers, traffic and incline.

To get a proper Mapbox Basemap, insert your Mapbox API Token `mapbox_key` into `R/env.R`.

The App runs for any desired city - given the data is provided. 
Within `shinyapp/R/config.R` a `city_name` with an according `pbf_file` and `geo_selection_file` can be set.
The corresponding OSM pbf file and geo_selection_file needs to be places in the `data` folder (placed in the root folder).
E.g., if you want to run the App for Friedrichshain-Kreuzberg, you can use the the pbf file for Berlin and store the outline of Friedrichshain-Kreuzberg as a geojson (or any other geodata format) and set the name of the `geo_selection_file` accordingly. If `geo_selection_file` is set to `NA` then the entire pbf file will be used.
If the dataset is too large this might get very slow! 
