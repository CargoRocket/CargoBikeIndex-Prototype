index_info <- "Der Index berechnet sich aus den Werten zur Straßenqualität und Barrieren. 
Er reicht von 0 - für Lastenräder nicht passierbar, bis 5 - optimale Bedingungen für Lastenräder.
Informationen zum Vekehr sind im Index NICHT berücksichtigt, sondern werden hier nur zur Information angezeigt."

street_quality_info <- "Die Straßenqualität setzt sich aus der Art der Straße (z.B. Radweg, Hauptstraße),
der Oberflächenbeschaffenheit (z.B. gute oder schlechte Qualität, Asphalt oder Kopfsteinplaster) zusammen."

street_type_info <- "Der Straßentyp beschreibt, wie gut die Straße für ein Lastenrad geeignet ist.
Dabei ist Radinfrastruktur (ein Radweg `track`, eine Spur / Schutzstreifen `lane`), die mind. 2 Meter breit ist, am besten.
Ebenfalls gut sind Wohngebiete bewertet.
Schmale Radwege, Hauptstraßen ohne Radinfrastruktur und Wege, die mit Fußgängern geteilt werden, sind schlechter bewertet.
Einbahnstraßen ohne eigene Spur für Radwege im Gegenverkehr sind aufgrund der häufig geringen Breite ebenfalls schlechter gewertet.
Wege, auf denen das Fahrrad nur geschoben werden darf sind ebenfalls verzeichnet als sehr schlecht bewertet."

surface_info <- "Die Oberfläche ergibt sich aus dem Material der Oberfläche `surface` (Asphalt, Kopfsteinplaster, Kies, etc.) und
der Qualität des Belags `smoothness` (gut, mittelmäßig, schlecht, etc.). Einzig Asphalt kann hier die beste Kategorie erlangen.
Die Qualität ist häufig nicht getaggt. In diesem Fall wird nur anhand des Materials auf die Qualität geschlossen."

street_width_info <- "Die Radwegbreite bestimmt bei Lastenrädern noch mehr als bei anderen Fahrrädern den Fahrkomfort sowie die Geschwindigkeit.
Auf schmalen Radwegen muss langsamer gefahren werden - weil nicht überholt werden kann und weil dies häufig in Zusammenhang mit einer schlechten Qualität des Radwegs oder
mit Hindernissen (Mülltonnen, Lieferverkehr) auf dem Radweg steht.
Leider ist diese Information kaum in OSM vergeben."

barriers_info <- "Barrieren können die Fahrt behindern oder das Durchkommeng ganz unmöglich machen.
Hier wird davon ausgegangen, dass Umlaufsperren für Lastenräder nicht passierbar sind. Poller und andere Barrieren können je nach
maximal durchlässiger Breite kein Problem oder nicht passierbar sein; falls keine Breite angegeben ist wird davon ausgeganen, 
dass diese passierbar sind. Nicht abgesenkte Bordsteine
werden als eine große - jedoch nicht unmögliche Hürde betrachtet."

cartraffic_info <- "Straßen, auf denen kein eigener Radweg vorhanden ist und die Fahrbahn sich somit mit dem Autoverkehr geteilt werden muss,
stellen potenziell eine Staugefahr für Lastenräder dar. Der Verkehr ist hier stark variabel und lässt sich schwer in ein allgemeines Ranking integrieren.
Hier sind alle Straßen dargestellt, auf denen sich Autos und Fahrräder die Fahrbahn teilen, wobei Hauptstraßen als besonders kritisch gesehen werden,
während Wohngebiete weniger kritisch bewertet sind.
Hauptstraßen ohne eigenen Radweg sind über die Straßenqualität bereits schlechter bewertet. 
Darüber hinaus wird der Autoverkehr NICHT weiter in der Indexberechnung berücksichtigt, sondern hier nur zur Information dargestellt."

pedestrian_info <- "Fußverkehr stellt für Lastenräder besonders in zwei Fällen ein Hindernis dar: 
1. bei Veranstaltungen: Hier sind vor allem Wochenmärkte regelmäßige Veranstaltungen, bei denen Straßen gesperrt sind.

2. stark frequentierte, geteilte Wege: Beispielsweise ist an einem sonnigen Wochenende in vielen Parks für Lastenräder fast nur Schrittgeschwindigkeit möglich. 

Hier sind alle Wege, die mit Fußgänger:innen geteilt werden und potenziell von Fußverkehr beeinträchtigt sind dargestellt. 
Wege, auf denen ohnehin abgestiegen werden muss, sind nicht dargestellt, da die langsame Geschwindigkeit hier unabhängig von der Anzahl an Fußgänger:innen ist.

Beide Fälle sind stark zeitabhängig. Mit Fußgänger:innen geteilte Wege sind über die Straßenqualität bereits schlechter bewertet.
Darüber hinaus wird der Fußverkehr NICHT weiter in der Indexberechnung berücksichtigt, sondern hier nur zur Information dargestellt."


preprocess_display_labels <- function(streets, palette, palette_0_to_1, palette_no_na, palette_barriers) {
  
  streets <- streets %>% 
    mutate(cycleway_string = case_when(
      cycleway_combined == "track" ~ "Radweg",
      cycleway_combined == "lane" ~ "Radfahrstreifen",
      cycleway_combined == "opposite_track" ~ "Radspur entgegen der Fahrtrichtung",
      cycleway_combined == "share_busway" ~ "geteilte Busspur"
    ),
    cycleway_width_string = ifelse(is.na(cycleway_width_combined), NA, paste("Breite:", cycleway_width_combined, "m")),
    cycleway_oneway_string = ifelse(cycleway_oneway_combined == "yes", "Zweirichtungsradweg", NA),
    dismount_string = ifelse(dismount_necessary, "absteigen", NA),
    segregated_string = ifelse(segregated %in% c("no"), "geteilt mit Fußweg", NA),
    bicycle_road_string = ifelse(bicycle_road %in% c("yes"), "Fahrradstraße", NA),
    highway_german = case_when(
      highway == "cycleway" ~ "Radweg",
      highway == "bridleway" ~ "Reitweg",
      highway == "bus_guideway" ~ "Spurbus-Strecke",
      highway == "corridor" ~ "Gang",
      highway == "escape" ~ "Notbremsweg",
      highway == "footway" ~ "Fußweg",
      highway == "living_street" ~ "Spielstraße",
      highway == "motorway" ~ "Schnellstraße",
      highway == "motorway_link" ~ "Schnellstraßenzubringer",
      highway == "path" ~ "Weg",
      highway == "pedestrian" ~ "Fußgängerzone",
      highway == "platform" ~ "Haltestelle",
      highway == "primary" ~ "Bundesstraße",
      highway == "primary_link" ~ "Bundesstraßen-Zubringer",
      highway == "residential" ~ "Straße im Wohngebiet",
      highway == "road" ~ "Straße (keine nähere Spezifikation)",
      highway == "secondary" ~ "Landesstraße",
      highway == "service" ~ "Erschließungsweg",
      highway == "steps" ~ "Stufen",
      highway == "tertiary" ~"Vorfahrtstraße",
      highway == "tertiary_link" ~"Vorfahrtstraßen-Zubringer",
      highway == "track" ~"Feldweg",
      highway == "trunk" ~"Kraftfahrstraße",
      highway == "trunk_link" ~"Kraftfahrstraße-Zubringer",
      highway == "unclassified" ~"befahrbare Nebenstraße",
      T ~ highway
    ),
    smoothness_german = case_when(
      smoothness_combined == "excellent" ~ "ausgezeichnet",
      smoothness_combined == "good" ~ "gut",
      smoothness_combined == "intermediate" ~ "mittelmäßig",
      smoothness_combined == "bad" ~ "schlecht",
      smoothness_combined == "very bad" ~ "sehr schlecht",
      smoothness_combined == "horrible" ~ "schrecklich",
      smoothness_combined == "very horrible" ~ "sehr schrecklich",
      smoothness_combined == "impassable" ~ "nicht passierbar",
    ),
    surface_german = case_when (
      surface_combined == "paved" ~ "befestigt",
      surface_combined == "asphalt" ~ "Asphalt",
      surface_combined == "paving_stones" ~ "Pflastersteine",
      surface_combined == "concrete" ~ "Beton",
      surface_combined == "concrete:plates" ~ "Betonplatten",
      surface_combined == "concrete:lanes" ~ "Betonspurbahnen",
      surface_combined == "sett" ~ "ebenes Kopfsteinplaster",
      surface_combined == "cobblestone" ~ "Kopfsteinpflaster",
      surface_combined == "cobblestone:flattened" ~ "ebenes Kopfsteinplaster",
      surface_combined == "unhewn_cobblestone" ~ "rohes Kopfsteinpflaster",
      surface_combined == "unpaved" ~ "unbefestigt",
      surface_combined == "compacted" ~ "verdichtete Deckschicht",
      surface_combined == "dirt" ~ "unbefestigte Straße",
      surface_combined == "earth" ~ "Trampelpfad",
      surface_combined == "fine_gravel" ~ "Splitt",
      surface_combined == "gravel" ~ "Schotter",
      surface_combined == "grass" ~ "Gras",
      surface_combined == "grass_paver" ~ "Rasengittersteine",
      surface_combined == "ground" ~ "Trampelpfad",
      surface_combined == "metal" ~ "Metall",
      surface_combined == "mud" ~ "Schlamm",
      surface_combined == "pebblestone" ~ "Kies",
      surface_combined == "salt" ~ "Salzsee",
      surface_combined == "sand" ~ "Sand",
      surface_combined == "rock" ~ "Steine",
      surface_combined == "wood" ~ "Holz"
    ),
    kerb_german = case_when(
      as.character(kerb) == "flush" ~ "ebenerdig",
      as.character(kerb) == "lowered" ~ "abgesenkt",
      as.character(kerb) == "raised" ~ "nicht abgesenkt",
      T ~ as.character(kerb)
    ),
    kerb_height_m = ifelse(is.na(kerb_height), NA, paste(kerb_height, "m"))
    ) %>% 
    unite("display_label_cycleway", 
          c(highway_german, bicycle_road_string, cycleway_string, cycleway_width_string, cycleway_oneway_string,
            segregated_string, dismount_string, surface_german, smoothness_german), na.rm = T, sep = ", ", remove = F) %>% 
    unite("display_label_barrier", 
          c(which_barrier, min_maxwidth), na.rm = T, sep = ", ", remove = F) %>% 
    unite("display_label_kerb", 
          c(kerb_german, kerb_height_m), na.rm = T, sep = ": ", remove = F) %>% 
    mutate(html_index_label = paste(
      "<b>", name, "- Index:", cbindex, "</b>",
      "<br>",
      "<br>", "Sub-Index Straßenqualität:", cbindex_street_quality,
      "<br>", "<i>", display_label_cycleway, "</i>",
      "<br>",
      "<br>", "Sub-Index Barrieren:", cbindex_barrier,
      "<br>", "<i>", display_label_barrier, "</i>"
    ), 
    index_color = palette(cbindex), 
    index_stroke_width = ifelse(cbindex < 1, 1, 2*cbindex),
    html_streetquality_label = paste(
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
    streetquality_color = palette(cbindex_street_quality), 
    streetquality_stroke_width = ifelse(cbindex_street_quality < 1, 1, 2*cbindex_street_quality),
    html_streettpye_label = paste(
      "<b>Straßentyp Bewertung:", cbindex_cycleways,"</b>",
        "<br>", "Straßentyp:", highway,
        "<br>", "Fahrradstraße:", bicycle_road,
        "<br>", "Radweg:", cycleway_combined,
        "<br>", "Radwegs-Breite:", cycleway_width_combined,
        "<br>", "Getrennter Radweg:", segregated,
        "<br>", "Absteigen notwendig:", dismount_necessary,
        "<br>", "Höchstgeschwindigkeit:", maxspeed
      ),
    streettype_color = palette(cbindex_cycleways), 
    streettype_stroke_width = ifelse(cbindex_cycleways < 1, 1, 2*cbindex_cycleways),
    html_barrier_label = paste(
      "<b>Barriere Bewertung:", cbindex_barrier,"</b>","<br>",
      "Barriere:", which_barrier, "<br>",
      "Max. Breite:", min_maxwidth, "<br>",
      "Bordsteinhöhe:", display_label_kerb, "<br>"),
    barrier_color = palette_0_to_1(cbindex_barrier), 
    html_surface_label = paste(
      "<b>Oberflächenqualität:", cbindex_surface,"</b>",
      "<br>", "Oberfläche:", surface_combined, 
      "<br>", "Qualität:", smoothness_combined),
    surface_color = palette(cbindex_surface), 
    surface_stroke_width = ifelse(cbindex_surface < 1, 1, 2*cbindex_surface),
    car_traffic_color = palette_no_na(car_traffic)
    )
  
  return(streets)
}
