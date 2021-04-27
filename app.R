library(shiny)
library(shinyjs)
library(leaflet)
library(leafem)
library(sf)
library(dplyr)
library(data.table)
library(htmltools)
library(readr)
library(shinyBS)
library(rgdal)
library(here)
library(tidyr)
# library(RSQLite)

source(here("R", "config.R"))
source(here("R", "env.R"))
source(here("R", "text_fields.R"))

ui <- bootstrapPage(
  title = "CargoBikeIndex",
  useShinyjs(),
  # titlePanel(title=div(id = "title", img(id = "logo", src=img))),
  tags$head(
    HTML("<link rel='preconnect' href='https://fonts.gstatic.com'><link href='https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap' rel='stylesheet'>"),
    includeCSS(here("R", "style.css"))
  ),
  div(
    class = "outer",
    leafletOutput("map", height = "100%"),
    absolutePanel(
      id = "input_box",
      top = 25,
      right = 20,
      tags$h3(textOutput("title")),
      textOutput("info_text"),
      radioButtons("index_radioB",
        label = "Daten für:",
        choices = c(
          "CargoBikeIndex" = "index", "Straßenqualität" = "street_quality",
          "Barrieren" = "barriers", "Verkehr" = "traffic"
        ),
        selected = "index"
      ),
      radioButtons("subgroup_radioB", label = "", choices = c("")),
      shinyWidgets::setShadow(id = "input_box")
    ),
    absolutePanel(
      bottom = 20, right = 20, style = "z-index:500; text-align: right;", fixed = T,
      tags$p(HTML('<a href = "https://cargorocket.de">CargoRocket Webseite</a>')),
    )
  )
)

server <- function(input, output, session) {
  streets_path <- here("data", paste0(city_name, ".Rds"))
  markets_path <- here("data", paste0(city_name, "_markets.Rds"))
  streets <- readRDS(streets_path)
  markets <- readRDS(markets_path)
  
  streets <- streets %>% 
    mutate(cycleway_string = case_when(
      cycleway_combined == "track" ~ "Radweg",
      cycleway_combined == "lane" ~ "Radspur / Schutzstreifen",
      cycleway_combined == "opposite_track" ~ "Radspur entgegen der Fahrtrichtung",
      cycleway_combined == "share_busway" ~ "geteilte Busspur"
    ),
    cycleway_width_string = ifelse(is.na(cycleway_width_combined), NA, paste("Breite:", cycleway_width_combined, "m")),
    cycleway_oneway_string = ifelse(cycleway_oneway_combined == "yes", "Zweirichtungsradweg", NA),
    dismount_string = ifelse(dismount_necessary, "absteigen", NA),
    segregated_string = ifelse(segregated %in% c("no"), "geteilt mit Fußweg", NA),
    bicycle_road_string = ifelse(bicycle_road %in% c("yes"), "Fahrradstraße", NA)) %>% 
    unite("display_label_cycleway", 
                 c(bicycle_road_string, cycleway_string, cycleway_width_string, cycleway_oneway_string,
                   segregated_string, dismount_string, surface_combined, smoothness_combined), na.rm = T, sep = ", ", remove = F) %>% 
    unite("display_label_barrier", 
          c(which_barrier, maxwidth_combined), na.rm = T, sep = ", ", remove = F)
  
  # TODO: better integrate preprocessing without duplicating script
  # if (file.exists(streets_path) & !delete_cache) {
  #   streets <- readRDS(here("data", paste0(city_name, ".Rds")))
  # } else {
  #   source(here("R", "prepare_data.R"))
  #   streets <- streets_data(pbf_file, geo_selection_file)
  #   dir.create(here("data"), showWarnings = F)
  #   saveRDS(streets, streets_path)
  # }

  GrYlRd <- c("#0F423E", "#479E8F", "#f4d03f", "#E76F51", "#B60202", "#6E1511") # darkgreen, green, yellow, orange, red, darkred
  palette <- colorBin(GrYlRd, domain = c(0:6), reverse = T, bins = 6)
  palette_no_na <- colorBin(GrYlRd, domain = c(0:6), reverse = T, bins = 6, na.color = "transparent")
  palette_pedestrian_traffic <- colorFactor(c("#E76F51"), domain = NULL, na.color = "transparent")


  output$map <- renderLeaflet({
    leaflet(streets) %>%
      addLogo("https://cargorocket.de/assets/images/cargorocket-logo.svg",
        src = "remote",
        position = "topleft",
        width = "150px",
      ) %>%
      fitBounds(st_bbox(streets)[[1]], st_bbox(streets)[[2]], st_bbox(streets)[[3]], st_bbox(streets)[[4]]) %>%
      # setView(lat =  52.5132, lng = 13.3874, zoom = 11) %>%
      addTiles(
        urlTemplate = paste0("https://api.mapbox.com/styles/v1/mapbox/light-v10/tiles/512/{z}/{x}/{y}?access_token=", mapbox_key),
        attribution = '\u00a9 <a href="https://www.mapbox.com/about/maps/">Mapbox</a> \u00a9 <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
        group = "Mapbox Light"
      ) %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addLayersControl(baseGroups = c("Mapbox Light", "OpenStreetMap"), position = "topleft")
  })


  observeEvent(input$index_radioB, {
    if (input$index_radioB == "index") {
      hide("subgroup_radioB")

      output$title <- renderText({
        "CargoBikeIndex"
      })
      output$info_text <- renderText({
        index_info
      })

      leafletProxy("map") %>%
        clearGroup("streets") %>%
        clearGroup("markets") %>%
        removeControl("legend2") %>% 
        addPolylines(
          data = streets,
          group = "streets",
          popup = ~ paste(
            name,
            "<br>","<br>",
            "<b>Index:", cbindex, "</b>",
            "<br>", "Index Straßenqualität:", cbindex_street_quality,
            "<br>", display_label_cycleway,
            "<br>",
            "<br>", "Index Barrieren:", cbindex_barrier,
            "<br>", display_label_barrier
          ),
          color = ~ palette(cbindex),
          opacity = 0.9,
          weight = 3
        ) %>%
        addLegend(
          layerId = "legend",
          labels = c(
            "0: nicht passierbar",
            "1: sehr schlecht",
            "2: schlecht",
            "3: mittelmäßig",
            "4: gut",
            "5: optimal"
          ),
          colors = palette(c(0:5)),
          opacity = 0.8,
          position = "bottomleft", title = "Straßenqualität gesamt"
        )
    } else if (input$index_radioB == "street_quality") {
      shinyjs::show("subgroup_radioB")

      # force update
      updateRadioButtons(session, "subgroup_radioB", label = "", choices = c(""), selected = "")

      updateRadioButtons(session, "subgroup_radioB",
        label = "",
        choices = c(
          "Straßenqualität gesamt" = "street_quality_combined",
          "Straßentyp" = "type_of_road",
          "Oberfläche" = "surface"
          #"Querneigung" = "incline_across"
        ),
        selected = "street_quality_combined"
      )
    } else if (input$index_radioB == "barriers") {
      hide("subgroup_radioB")

      output$title <- renderText({
        "Barrieren"
      })
      output$info_text <- renderText({
        barriers_info
      })
      leafletProxy("map") %>%
        clearGroup("streets") %>%
        clearGroup("markets") %>%
        removeControl("legend2") %>% 
        addPolylines(
          data = filter(streets, !is.na(which_barrier)),
          group = "streets",
          popup = ~ paste("<b>Barriere:", which_barrier, "</b><br>", "Max. Breite / Bordsteinhöhe:", maxwidth_combined, "<br>"),
          color = ~ palette(cbindex_barrier),
          opacity = 0.9,
          weight = 5
        ) %>%
        addLegend(
          layerId = "legend",
          labels = c(
            "<b>0: nicht passierbar</b> Schranken | Umlaufgitter | Poller < 0.9 m",
            "<b>1: sehr schlecht passierbar</b> Poller < 1.0 m | nicht gesenkter Bordstein",
            "<b>2: schlecht passierbar</b> Poller < 1.2 m |default Bordstein (ohne Höhenangabe)",
            "<b>3: mittelmäßig passierbar</b> abgesenkter Bordstein",
            "<b>4: gut passierbar</b> Poller < 1.5 m |default Poller (keine Breitenangabe)",
            "<b>5: problemlos passierbar</b> Poller >= 1.5 m | ebenerdiger Bordstein"
          ),
          colors = palette(c(0:5)),
          opacity = 0.8,
          position = "bottomleft", title = "Straßen mit Barrieren"
        )
    } else if (input$index_radioB == "traffic") {
      shinyjs::show("subgroup_radioB")

      updateRadioButtons(session, "subgroup_radioB",
        label = "Verkehr Subgruppen:",
        choices = c("Autoverkehr", "Fußverkehr"),
        selected = "Autoverkehr"
      )
    }
  })

  observeEvent(input$subgroup_radioB,
    {
      if (input$subgroup_radioB == "street_quality_combined") {
        output$title <- renderText({
          "Straßenqualität"
        })
        output$info_text <- renderText({
          street_quality_info
        })

        leafletProxy("map") %>%
          clearGroup("streets") %>%
          clearGroup("markets") %>%
          removeControl("legend2") %>% 
          addPolylines(
            data = streets,
            group = "streets",
            popup = ~ paste(
              "<b>Straßenqualität:", cbindex_street_quality, "</b>",
              "<br>", "Straßentyp:", highway,
              "<br>", "Radweg:", cycleway_combined,
              "<br>", "Radwegs-Breite:", cycleway_width_combined,
              "<br>", "Fahrradstraße:", bicycle_road,
              "<br>", "Höchstgeschwindigkeit:", maxspeed,
              "<br>", "Getrennter Radweg:", segregated,
              "<br>", "Oberfläche:", surface_combined,
              "<br>", "Qualität:", smoothness_combined
            ),
            color = ~ palette(cbindex_street_quality),
            opacity = 0.9,
            weight = 3
          ) %>%
          addLegend(
            layerId = "legend",
            labels = c(
              "0: nicht passierbar",
              "1: sehr schlecht",
              "2: schlecht",
              "3: mittelmäßig",
              "4: gut",
              "5: optimal"
            ),
            colors = palette(c(0:5)),
            opacity = 0.8,
            position = "bottomleft", title = "Straßenqualität gesamt"
          )
      } else if (input$subgroup_radioB == "type_of_road") {
        output$title <- renderText({
          "Straßentyp"
        })
        output$info_text <- renderText({
          street_type_info
        })

        leafletProxy("map") %>%
          clearGroup("streets") %>%
          clearGroup("markets") %>%
          removeControl("legend2") %>% 
          addPolylines(
            data = streets,
            group = "streets",
            popup = ~ paste(
              "<b>Straßentyp:", highway, "</b>", 
              "<br>", "Fahrradstraße:", bicycle_road,
              "<br>", "Radweg:", cycleway_combined,
              "<br>", "Radwegs-Breite:", cycleway_width_combined,
              "<br>", "Getrennter Radweg:", segregated,
              "<br>", "Absteigen notwendig:", dismount_necessary,
              "<br>", "Höchstgeschwindigkeit:", maxspeed
            ),
            color = ~ palette(cbindex_cycleways),
            opacity = 0.9,
            weight = 3
          ) %>%
          addLegend(
            layerId = "legend",
            labels = c(
              "<b>1: sehr schlecht</b> Hauptstraßen (primary) ohne Radweg | Radweg < 1.2m | absteigen notwendig",
              "<b>2: schlecht</b> Landesstraße (secondary) ohne Radweg | Radspur < 1.2m | Wege mit Fußverkehr geteilt",
              "<b>3: mittelmäßig</b> default Radweg ohne Breite | Landesstraße (tertiary) ohne Radweg",
              "<b>4: gut</b> Radweg min 1.6m | Radspur mind. 1.2m | default Radspur ohne Breite | Wohngebiete",
              "<b>5: optimal</b> Fahrradstraße | min. 2m Radweg"
            ),
            colors = palette(c(1:5)),
            opacity = 0.8,
            position = "bottomleft", title = "Straßentyp"
          )
      } else if (input$subgroup_radioB == "surface") {
        output$title <- renderText({
          "Oberfläche"
        })
        output$info_text <- renderText({
          surface_info
        })

        leafletProxy("map") %>%
          clearGroup("streets") %>%
          clearGroup("markets") %>%
          removeControl("legend2") %>% 
          addPolylines(
            data = streets,
            group = "streets",
            popup = ~ paste("<b>Oberfläche:", surface_combined, "</b><br>", "Qualität:", smoothness_combined),
            color = ~ palette(cbindex_surface),
            opacity = 0.9,
            weight = 3
          ) %>%
          addLegend(
            layerId = "legend",
            labels = c(
              "0: nicht passierbar (Sand, Steine, Matsch)",
              "1: sehr schlecht (unbefestigter Untergrund)",
              "2: schlecht (Kopfsteinpflaster, Kiesel)",
              "3: mittelmäßig (unebener Asphalt / Pflastersteine, Schlaglöcher und Wurzeln, unbefestigt aber verdichtet z.B. im Park)",
              "4: gut (gepflastert)",
              "5: optimal (ebener Asphalt)",
              "keine Daten"
            ),
            colors = palette(c(0:5, NA)),
            opacity = 0.8,
            position = "bottomleft", title = "Straßenqualität"
          )
      # } else if (input$subgroup_radioB == "incline_across") {
      #   output$title <- renderText({
      #     "Querneigung"
      #   })
      #   output$info_text <- renderText({
      #     incline_across_info
      #   })
      # 
      #   leafletProxy("map") %>%
      #     clearGroup("streets") %>%
      #     removeControl("legend")
      } else if (input$subgroup_radioB == "Autoverkehr") {
        output$title <- renderText({
          "Autoverkehr"
        })

        leafletProxy("map") %>%
          clearGroup("streets") %>%
          clearGroup("markets") %>% 
          removeControl("legend") %>%
          removeControl("legend2") %>% 
          addPolylines(
            data = streets,
            group = "streets",
            color = ~ palette_no_na(car_traffic),
            opacity = 0.9,
            weight = 3
          ) %>% 
          addLegend(
            layerId = "legend",
            labels = c("geteilte Hauptstraßen (primary, trunk)",
                       "geteilte Landesstraße (secondary)",
                       "geteilte Landesstraße (tertiary)",
                       "geteilte Wohnstraßen"),
            colors = palette_no_na(c(1:4)),
            opacity = 0.8,
            position = "bottomleft", title = "Geteilte Straßen mit Autoverkehr"
          )
        
          
        output$info_text <- renderText({
          cartraffic_info
        })
      } else if (input$subgroup_radioB == "Fußverkehr") {
        
        output$title <- renderText({
          "Fußverkehr"
        })
        output$info_text <- renderText({
          pedestrian_info
        })

        leafletProxy("map") %>%
          clearGroup("streets") %>%
          clearGroup("markets") %>% 
          removeControl("legend") %>%
          removeControl("legend2") %>% 
          addPolylines(
            data = streets,
            group = "streets",
            color = ~ palette_pedestrian_traffic(pedestrian_traffic),
            opacity = 0.9,
            weight = 3
          ) %>% 
          addCircleMarkers(
            data = markets,
            group = "markets",
            popup = ~ paste(
              "Wochenmarkt:", name,
              "<br>", "Öffnungszeiten:", opening_hours),
            color = "transparent",
            fillColor = "#B60202",
            fillOpacity = 0.8,
            radius = 5
          ) %>% 
          addLegend(
            layerId = "legend",
            labels = c("geteite Wege"),
            colors = c("#E76F51"),
            opacity = 0.8,
            position = "bottomleft", title = "Geteilte Wege mit Fußgänger:innen (Linien)"
            ) %>% 
              addLegend(
                layerId = "legend2",
                labels = c("Wochenmärkte"),
                colors = c("#B60202"),
            opacity = 0.8,
            position = "bottomleft", title = "Wochenmärkte (Punkte)"
            )
          
      }
    },
    ignoreInit = T
  )
}

shinyApp(ui, server)
