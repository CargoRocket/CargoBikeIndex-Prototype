index_info <- "Der Index berechnet sich aus den Werten zur Straßenqualität und Barrieren.
Aktuell sind Verkehr und Steigung NOCH NICHT einbezogen."

street_quality_info <- "Die Straßenqualität setzt sich aus der Art der Straße (Radweg, Hauptstraße, etc.),
der Oberflächenbeschaffenheit (Asphalt in guter oder mittlerer Qualität, Kopfsteinplaster,etc.) zusammen.
(Querneigung ist bisher nicht berücksichtigt)"

street_type_info <- "Der Straßentyp beschreibt, wie gut die Straße für ein Lastenrad geeignet ist.
Dabei ist Radinfrastruktur (ein Radweg `track`, eine Spur / Schutzstreifen `lane`), die mind. 2 Meter breit ist, am besten.
Ebenfalls gut sind 30-er Zonen und Wohngebiete bewertet.
Schmale Radwege, Hauptstraßen ohne Radinfrastruktur und Wege, die mit Fußgängern geteilt werden, sind schlechter bewertet.
Einbahnstraßen ohne eigene Spur für Radwege im Gegenverkehr sind aufgrund der häufig geringen Breite ebenfalls schlechter gewertet."

surface_info <- "Die Oberfläche ergibt sich aus dem Material der Oberfläche `surface` (Asphalt, Kopfsteinplaster, Kies, etc.) und
der Qualität des Belags `smoothness` (gut, mittelmäßig, schlecht, etc.). Einzig Asphalt kann hier die beste Kategorie erlangen.
Die Qualität ist häufig nicht getaggt. In diesem Fall wird nur anhand des Materials auf die Qualität geschlossen."

street_width_info <- "Die Radwegbreite bestimmt bei Lastenrädern noch mehr als bei anderen Fahrrädern den Fahrkomfort, sowie die Geschwindigkeit.
Auf schmalen Radwegen muss langsamer gefahren werden - weil nicht überholt werden kann und weil dies häufig in Zusammenhang mit einer schlechten Qualität des Radwegs oder
mit Hindernissen (Mülltonnen, Lieferverkehr) auf dem Radweg steht.
Leider ist diese Information kaum in OSM vergeben."

incline_across_info <- "NOCH NICHT IMPLEMENTIERT.
Die Querneigung gibt an, wie stark die Straße sich seitwärts neigt. Dies ist besonders für mehrspurige Lastenräder ein Problem,
da diese instabil werden und zu kippen drohen. Leider gibt es hierzu bisher noch keine Daten."

barriers_info <- "Barrieren können die Fahrt behindern oder das Durchkommeng ganz unmöglich machen.
Hier wird davon ausgegangen, dass Umlaufsperren und Schranken für Lastenräder nicht passierbar sind. Poller können je nach
maximal durchlässiger Breite kein Problem oder nicht passierbar sein. Nicht abgesenkte Bordsteine
werden als eine große - jedoch nicht unmögliche Hürde betrachtet."

traffic_info <- "NOCH NICHT IMPLEMENTIERT. Verkehr ..."

cartraffic_info <- "NOCH NICHT IMPLEMENTIERT. Autoverkehr ..."

pedestrian_info <- "NOCH NICHT IMPLEMENTIERT. Fußgänger ..."

incline_info <- "NOCH NICHT IMPLEMENTIERT. Steigung ..."
