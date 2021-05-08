library(shiny)
library(shinyjs)
library(leaflet)
#library(leafem)
library(sf)
library(dplyr)
library(data.table)
library(htmltools)
library(readr)
library(shinyBS)
library(rgdal)
library(here)
library(tidyr)
library(mapdeck)
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
    mapdeckOutput("map", height = "100%"),
    absolutePanel(
      id = "input_box",
      top = 25,
      left = 20,
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
      tags$p(HTML('<a href = "https://cargorocket.de/impress">Impressum</a>')),
    )
  )
)

server <- function(input, output, session) {
  streets_path <- here("data", paste0(city_name, "_streets.Rds"))
  barriers_path <- here("data", paste0(city_name, "_barriers.Rds"))
  markets_path <- here("data", paste0(city_name, "_markets.Rds"))
  streets <- readRDS(streets_path)
  barriers <- readRDS(barriers_path)
  markets <- readRDS(markets_path)
  
  GrYlRd <- c("#0F423E", "#479E8F", "#f4d03f", "#E76F51", "#B60202", "#6E1511") # darkgreen, green, yellow, orange, red, darkred
  GrYlRd_4 <- c( "#479E8F", "#f4d03f", "#E76F51", "#B60202") # darkgreen, green, yellow, orange, red, darkred
  barrier_colors <- c("#650E59", "#AE0D3A", "#E4114C", "#231F70", "#415AC7", "#5A9AE5")
  palette <- colorBin(GrYlRd, domain = c(0:6), reverse = T, bins = 6)
  palette_0_to_1 <- colorFactor(GrYlRd, domain = c(0,0.2,0.4,0.6,0.8,1), na.color = "transparent", reverse = T)
  palette_no_na <- colorFactor(GrYlRd_4, domain = c(0.2,0.4,0.6,0.8), reverse = T, na.color = "transparent")
  palette_barriers <- colorFactor(barrier_colors, domain = c("cycle_barrier", "bollard", "block", "lift_gate", "kerb", "traffic_calming"), 
                                  na.color = "transparent", ordered = T)
  
  streets <- preprocess_display_labels(streets, palette, palette_0_to_1, palette_no_na, palette_barriers)
  barriers <- barriers %>% 
    mutate(display_label = case_when(
      barrier == "cycle_barrier" ~ "Umlaufsperre",
      barrier == "bollard" ~ "Poller",
      barrier == "block" ~ "Block",
      barrier == "lift_gate" ~ "Schranke",
      barrier == "traffic_calming" ~ "Bodenwelle",
      barrier == "kerb" ~ "Bordstein"
    ),html_barrier_label = paste(
            "Barriere:", display_label,
            "<br>", "Max. Breite:", maxwidth_barriers_combined,
            "<br>", "Bordsteinhöhe:", kerb,
            "<br>", "Bordsteinhöhe [m]:", height_cleaned)
    )
  
  markets <- markets %>% 
    mutate(html_market_label = paste(
      "Wochenmarkt:", name,
      "<br>", "Öffnungszeiten:", opening_hours))

   loc_lon <- (st_bbox(streets)[[1]] +  st_bbox(streets)[[3]]) / 2
   loc_lat <- (st_bbox(streets)[[2]] + st_bbox(streets)[[4]]) / 2

    output$map <- renderMapdeck({
     mapdeck(data = streets, token = mapbox_key, 
             location = c(loc_lon, loc_lat),
             zoom = 11, min_zoom = 7, max_zoom = 15) 
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

      mapdeck_update(map_id = "map") %>% 
        clear_scatterplot("barriers") %>% 
        clear_scatterplot("markets") %>% 
        add_path(
          id = "streets",
          data = streets,
          tooltip = "html_index_label",
          stroke_colour = "index_color",
          stroke_width = "index_stroke_width",
          stroke_opacity = 0.9,
          update_view = F,
          legend = mapdeck_legend(legend_element(
            variables = c(
              "0: nicht passierbar",
              "1: sehr schlecht",
              "2: schlecht",
              "3: mittelmäßig",
              "4: gut",
              "5: optimal"
            )
            , colours = palette(c(0:5))
            , colour_type = "stroke"
            , variable_type = "category"
            , title = "CargoBikeIndex"
          )))
      
 
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

      mapdeck_update(map_id = "map") %>% 
      clear_scatterplot("markets") %>% 
        add_path(
          id = "streets",
          data = filter(streets, which_barrier != "" ),
          tooltip = "html_barrier_label",
          stroke_colour = "barrier_color",
          stroke_width = 10,
          stroke_opacity = 0.9,
          update_view = F,
          legend = mapdeck_legend(legend_element(
            variables = c(
              "<b>0: nicht passierbar</b> Umlaufgitter | Poller < 0.9 m",
              "<b>0.2: sehr schlecht passierbar</b> Poller < 1.0 m | nicht gesenkter Bordstein",
              "<b>0.4: schlecht passierbar</b> Poller < 1.2 m | default Bordstein (ohne Höhenangabe)",
              "<b>0.6: mittelmäßig passierbar</b> abgesenkter Bordstein",
              "<b>0.8: gut passierbar</b> Poller < 1.5 m | default Poller (keine Breitenangabe)",
              "<b>1: problemlos passierbar</b> Poller >= 1.5 m | ebenerdiger Bordstein"
            ),
            , colours = palette_0_to_1(c(0, 0.2, 0.4, 0.6, 0.8, 1))
            , colour_type = "stroke"
            , variable_type = "category"
            , title = "Straßen mit Barrieren"
          ))) %>% 
        add_scatterplot(
          id = "barriers",
          update_view = F,
          data = barriers,
          tooltip = "html_barrier_label",
          fill_colour = "#B60202",
          radius = 10,
          legend = mapdeck_legend(legend_element(
            variables = c("Umlaufgitter", "Poller", "Block", "Schranke", "Bordstein", "Bodenwelle")
            , colours = palette_barriers(c("cycle_barrier", "bollard", "block", "lift_gate", "kerb", "traffic_calming"))
            , colour_type = "fill"
            , variable_type = "category"
            , title = "Barrieren"
          ))) 
      
          
                  
    } else if (input$index_radioB == "traffic") {
      shinyjs::show("subgroup_radioB")

      updateRadioButtons(session, "subgroup_radioB",
        label = "",
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
        
        mapdeck_update(map_id = "map") %>% 
          clear_scatterplot("barriers") %>% 
          clear_scatterplot("markets") %>% 
          add_path(
            id = "streets",
            data = streets,
            tooltip = "html_street_quality_label",
            stroke_colour = "streetquality_color",
            stroke_width = "streetquality_stroke_width",
            stroke_opacity = 0.9,
            update_view = F,
            legend = mapdeck_legend(legend_element(
              variables = c(
                "0: nicht passierbar",
                "1: sehr schlecht",
                "2: schlecht",
                "3: mittelmäßig",
                "4: gut",
                "5: optimal"
              ),
              , colours = palette(c(0:5))
              , colour_type = "stroke"
              , variable_type = "category"
              , title = "Straßenqualität gesamt"
            )))
        
        
      } else if (input$subgroup_radioB == "type_of_road") {
        output$title <- renderText({
          "Straßentyp"
        })
        output$info_text <- renderText({
          street_type_info
        })

        mapdeck_update(map_id = "map") %>% 
          clear_scatterplot("barriers") %>% 
          clear_scatterplot("markets") %>% 
          add_path(
            id = "streets",
            data = streets,
            tooltip = "html_streettype_label",
            stroke_colour = "streettype_color",
            stroke_width = "streettype_stroke_width",
            stroke_opacity = 0.9,
            update_view = F,
            legend = mapdeck_legend(legend_element(
              variables = c(
                "<b>1: sehr schlecht</b> Bundestraßen (primary) ohne Radweg | Radweg < 1.2m | absteigen notwendig",
                "<b>2: schlecht</b> Landesstraßen (secondary) ohne Radweg | Radspur < 1.2m | Wege mit Fußverkehr geteilt",
                "<b>3: mittelmäßig</b> default Radweg ohne Breite | Vorfahrtstraßen (tertiary) ohne Radweg",
                "<b>4: gut</b> Radweg min 1.6m | Radspur mind. 1.2m | default Radspur ohne Breite | Wohngebiete",
                "<b>5: optimal</b> Fahrradstraße | min. 2m Radweg"
              ),
              , colours = palette(c(1:5))
              , colour_type = "stroke"
              , variable_type = "category"
              , title = "Straßentyp"
            )))
        
        
      } else if (input$subgroup_radioB == "surface") {
        output$title <- renderText({
          "Oberfläche"
        })
        output$info_text <- renderText({
          surface_info
        })

        mapdeck_update(map_id = "map") %>% 
          clear_scatterplot("barriers") %>% 
          clear_scatterplot("markets") %>% 
          add_path(
            id = "streets",
            data = streets,
            tooltip = "html_surface_label",
            stroke_colour = "surface_color",
            stroke_width = "surface_stroke_width",
            stroke_opacity = 0.9,
            update_view = F,
            legend = mapdeck_legend(legend_element(
              variables = c(
                      "0: nicht passierbar (Sand, Steine, Matsch)",
                      "1: sehr schlecht (unbefestigter Untergrund)",
                      "2: schlecht (Kopfsteinpflaster, Kiesel)",
                      "3: mittelmäßig (unebener Asphalt / Pflastersteine)",
                      "4: gut (gepflastert)",
                      "5: optimal (ebener Asphalt)"
                    ),
              , colours = palette(c(0:5))
              , colour_type = "stroke"
              , variable_type = "category"
              , title = "Straßenoberfläche"
            )))
        
        
      # } else if (input$subgroup_radioB == "incline_across") {
      #   output$title <- renderText({
      #     "Querneigung"
      #   })
      #   output$info_text <- renderText({
      #     incline_across_info
      #   })
        
        
      } else if (input$subgroup_radioB == "Autoverkehr") {
        output$title <- renderText({
          "Autoverkehr"
        })

        mapdeck_update(map_id = "map") %>% 
          clear_scatterplot("markets") %>% 
          clear_scatterplot("barriers") %>% 
          add_path(
            id = "streets",
            data = filter(streets, !is.na(car_traffic)),
            stroke_colour = "car_traffic_color",
            tooltip = "highway",
            stroke_width = 10,
            stroke_opacity = 0.9,
            update_view = F,
            legend = mapdeck_legend(legend_element(
              variables = c("geteilte Bundesstraße (primary, trunk)",
                           "geteilte Landesstraße (secondary)",
                           "geteilte Vorfahrtstraße (tertiary)",
                           "geteilte Straße im Wohngebiet (living_street, residential)")
              , colours = palette_no_na(c(0.2,0.4,0.6, 0.8))
              , colour_type = "stroke"
              , variable_type = "category"
              , title = "Geteilte Straßen mit Autoverkehr"
            )))
        
        
          
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
        
        mapdeck_update(map_id = "map") %>% 
          clear_scatterplot("barriers") %>% 
          add_path(
            id = "streets",
            data = filter(streets, !is.na(pedestrian_traffic)),
            stroke_colour = "#E76F51",
            tooltip = "highway",
            stroke_width = 10,
            stroke_opacity = 0.9,
            update_view = F,
            legend = mapdeck_legend(legend_element(
              variables = c("geteite Wege")
              , colours = "#E76F51"
              , colour_type = "stroke"
              , variable_type = "category"
              , title = "Geteilte Wege mit Fußgänger:innen"
            ))) %>% 
          add_scatterplot(
            id = "markets",
            update_view = F,
            data = markets,
            tooltip = "html_market_tootltip",
            fill_colour = "#B60202",
            radius = 20,
            legend = mapdeck_legend(legend_element(
              variables = c("Wochenmarkt")
              , colours = "#B60202"
              , colour_type = "fill"
              , variable_type = "category"
              , title = "Wochenmärkte"
            ))) 
        
      }
    },
    ignoreInit = T
  )
}


shinyApp(ui, server)

