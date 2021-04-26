index_info <- "Der Index berechnet sich aus den Werten zur Straßenqualität und Barrieren. 
Information zu Fuß- und Autoverkehr ist im Index NICHT berücksichtigt, sondern nur zur Information angezeigt."

street_quality_info <- "Die Straßenqualität setzt sich aus der Art der Straße (Radweg, Hauptstraße, etc.),
der Oberflächenbeschaffenheit (Asphalt in guter oder mittlerer Qualität, Kopfsteinplaster,etc.) zusammen."

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

traffic_info <- "Auf Straßen und Wegen die mit Autos und Fußgänger:innen geteilt werden, sind Lastenräder werden Lastenräder durch diese potenziell ausgebremst. 
Straßen mit Stau oder volle Parks am Wochenende erlauben Lastenrädern nur kaum das Überholen, wenn kein eigener Radweg vorhanden ist. 
Ebenso können Wochenmärkte das Durchkommen stark erschweren. 
Hauptstraßen und Fußwege ohne eigenen Radweg sind in der Evaluation der Straßenqualität bereits negativer bewertet. 
Da der Verkehr je nach Uhrzeit und Wochentag stark schwankt und sich je nach Straße nochmals stark unterscheiden kann,
ist ein zusätzlicher Einbezug in einen allgemeinen Index nur schwer möglich.
Zu Informationszwecken werden diese Straßen hier angezeigt, sind jedoch nicht zusätzlich in der Indexberechnung berücksichtigt."

cartraffic_info <- "NOCH NICHT IMPLEMENTIERT. Autoverkehr ..."

pedestrian_info <- "NOCH NICHT IMPLEMENTIERT. Fußgänger ..."
