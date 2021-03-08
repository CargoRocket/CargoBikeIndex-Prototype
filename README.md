**DEPRECATED - a new version will follow soon!**

<div align="center">
  <img src="https://cologne.xatellite.io/logo-pfade.svg" alt="Logo" height="100px" />

  CargoRocket Cargobike Index
  ---
</div>

R query to generate Cargobike Index maps

See live demo: [cargobike.xatellite.io](https://cargobike.xatellite.io)


Run `cargoindex.R ` to create a CargoBikeIndex Map for your city.
Set the parameter `city` to the desired city.
To get a proper Mapbox Basemap, insert your Mapbox API Token.

The script queries streets, parks and barriers from OSM.

Based on street type (seperate cycleway, cycleway on the road), street width, road surface, road smoothness, wether the street is located in a park or running in the opposite direction of a oneway street a cargoBikeIndex is computed for each street and displayed on a map.
Barriers are displayed as red points.

A score of 0 means not usable for cargo bikes (e.g. smoothness is bad or cyclepath width < 1.5 m). 
A score of 1 means very well suited.
