library(readr)
library(dplyr)
#Einlesen der ersten Tabelle
daten101 <- read_csv("Fragenauswertung - 101 Wer darf wählen_.csv")
#Einlesen der weiteren Tabllen 
daten102 <- read_csv("Fragenauswertung - 102 Wo kann ich wählen_.csv")
daten103 <- read_csv("Fragenauswertung - 103 Was wird gewählt_.csv")
daten104 <- read_csv("Fragenauswertung - 104 Briefwahl.csv")
daten105 <- read_csv("Fragenauswertung - 105 Informieren.csv")
daten201 <- read_csv("Fragenauswertung - 201 Zusammenfassung.csv")
daten202 <- read_csv("Fragenauswertung - 202 Wirtschaft.csv")
daten203 <- read_csv("Fragenauswertung - 203 Bildung.csv")
daten204 <- read_csv("Fragenauswertung - 204 Zuwanderung.csv")
daten205 <- read_csv("Fragenauswertung - 205 Lebensqualität.csv")


#Faktorisieren von character Variablen für Frage 101
daten101 <- daten101 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Faktorisieren von character Variablen für Frage 102
daten102 <- daten102 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Faktorisieren von character Variablen für Frage 103
daten103 <- daten103 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 104
daten104 <- daten104 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 105
daten105 <- daten105 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Erste Zeile löschen da keine Inhaltlichen Daten
daten201 <- daten201[-1, ]
#Faktorisieren von character Variablen für Frage 201
daten201 <- daten201 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum ,
      as.factor
    )
  )
#Numeric aus Ausführlichkeits Variablen machen daraus machen
daten201 <- daten201 %>%
  mutate(
    across(
      c(
        `Ausführlichkeit  2.1`,
        `Ausführlichkeit  2.2`,
        `Ausführlichkeit  2.3`,
        `Ausführlichkeit  2.4`,
        `Ausführlichkeit  2.5`,
        `Ausführlichkeit  2.6`,
        `Ausführlichkeit  2.7`,
        `Ausführlichkeit  2.8`,
        `Ausführlichkeit  3`
      ),
      ~ as.numeric(as.character(.x))
    )
  )

#Faktorisieren von character Variablen für Frage 202
daten202 <- daten202 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Faktorisieren von character Variablen für Frage 203
daten203 <- daten203 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 204
daten204 <- daten204 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Faktorisieren von character Variablen für Frage 205
daten205 <- daten205 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )

#Verkürzung der Variablennamen 
names(daten101)[names(daten101) == "Ausführlichkeit 4"] <- "Ausf"
names(daten101)[names(daten101) == "KI-Modell"] <- "KI"
names(daten102)[names(daten102) == "Ausführlichkeit 4"] <- "Ausf"
names(daten102)[names(daten102) == "KI-Modell"] <- "KI"
names(daten103)[names(daten103) == "KI-Modell"] <- "KI"
names(daten104)[names(daten104) == "Ausführlichkeit 3"] <- "Ausf"
names(daten104)[names(daten104) == "KI-Modell"] <- "KI"
names(daten105)[names(daten105) == "Ausführlichkeit 5"] <- "Ausf"
names(daten105)[names(daten105) == "KI-Modell"] <- "KI"
names(daten201)[names(daten201) == "Ausführlichkeit  3"] <- "Ausf_insgesamt"
names(daten201)[names(daten201) == "KI-Modell"] <- "KI"
names(daten202)[names(daten202) == "Ausführlichkeit 4"] <- "Ausf"
names(daten202)[names(daten202) == "KI-Modell"] <- "KI"
names(daten203)[names(daten203) == "Ausführlichkeit 4"] <- "Ausf"
names(daten203)[names(daten203) == "KI-Modell"] <- "KI"
names(daten204)[names(daten204) == "Ausführlichkeit 4"] <- "Ausf"
names(daten204)[names(daten204) == "KI-Modell"] <- "KI"
names(daten205)[names(daten205) == "Ausführlichkeit 4"] <- "Ausf"
names(daten205)[names(daten205) == "KI-Modell"] <- "KI"

#Boxplot Ausführlichkeit nach Ländern für Frage 101
boxplot(
  Ausf ~ Land,
  data = daten101,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 101 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 101
boxplot(
  Ausf ~ KI,
  data = daten101,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 101 nach KI-Modell sortiert "
)


#Histogramm der Ausführlichkeit von Frage 101
hist(daten101$Ausf_4,
     ylab = "Häufigkeit",
     xlab = "Anzahl der Zeichen inkl. Leerzeichen",
     main ="Histogramm der Ausführlichkeit bei 'Wer darf wählen'")

#Boxplot Ausführlichkeit nach Ländern für Frage 102
boxplot(
  Ausf ~ Land,
  data = daten102,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 102 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 102
boxplot(
  Ausf ~ KI,
  data = daten102,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 102 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 103
boxplot(
  Ausführlichkeit ~ Land,
  data = daten103,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 103 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 103
boxplot(
  Ausführlichkeit ~ KI,
  data = daten103,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 103 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 104
boxplot(
  Ausf ~ Land,
  data = daten104,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 104 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 104
boxplot(
  Ausf ~ KI,
  data = daten104,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 104 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 105
boxplot(
  Ausf ~ Land,
  data = daten105,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 105 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 105
boxplot(
  Ausf ~ KI,
  data = daten105,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 105 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 201
boxplot(
  Ausf_insgesamt ~ Land,
  data = daten201,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 201 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 201
boxplot(
  Ausf_insgesamt ~ KI,
  data = daten201,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 201 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 202
boxplot(
  Ausf ~ Land,
  data = daten202,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 202 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 202
boxplot(
  Ausf ~ KI,
  data = daten202,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 202 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 203
boxplot(
  Ausf ~ Land,
  data = daten203,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 203 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 203
boxplot(
  Ausf ~ KI,
  data = daten203,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 203 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 204
boxplot(
  Ausf ~ Land,
  data = daten204,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 204 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 204
boxplot(
  Ausf ~ KI,
  data = daten204,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 204 nach KI-Modell sortiert "
)

#Boxplot Ausführlichkeit nach Ländern für Frage 205
boxplot(
  Ausf ~ Land,
  data = daten205,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 205 nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell für Frage 205
boxplot(
  Ausf ~ KI,
  data = daten205,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge für Frage 205 nach KI-Modell sortiert "
)
