index_info <- "Der Index berechnet sich aus den Werten zur Straßenqualität und Barrieren. 
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
<br>Beide Fälle sind stark zeitabhängig. Mit Fußgänger:innen geteilte Wege sind über die Straßenqualität bereits schlechter bewertet.
Darüber hinaus wird der Fußverkehr NICHT weiter in der Indexberechnung berücksichtigt, sondern hier nur zur Information dargestellt."
