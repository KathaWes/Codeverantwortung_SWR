library(readr)
library(dplyr)

#Einlesen der ersten Tabelle
daten <- read_csv("Fragenauswertung - Parteiempfehlungen_60%.csv", skip =2)

#Faktorisieren von character Variablen für Frage 
daten <- daten %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum & !Auffälliges ,
      as.factor
    )
  )

#Verkürzung der Variablennamen 
names(daten)[names(daten) == "Ausführlichkeit  5.0 Gesamt"] <- "Ausf"
names(daten)[names(daten) == "Ausführlichkeit  5.1 CDU"] <- "Ausf_CDU"
names(daten)[names(daten) == "Ausführlichkeit  5.2 SPD"] <- "Ausf_SPD"
names(daten)[names(daten) == "Ausführlichkeit  5.3 AFD"] <- "Ausf_AFD"
names(daten)[names(daten) == "Ausführlichkeit  5.4 Grüne"] <- "Ausf_Gruene"
names(daten)[names(daten) == "Ausführlichkeit  5.5 Linke"] <- "Ausf_Linke"
names(daten)[names(daten) == "Ausführlichkeit  5.6 FDP"] <- "Ausf_FDP"
names(daten)[names(daten) == "Ausführlichkeit  5.7 Freie Wähler"] <- "Ausf_FW"
names(daten)[names(daten) == "Ausführlichkeit  5.8 Weitere"] <- "Ausf_Weiter"
names(daten)[names(daten) == "KI-Modell"] <- "KI"

#Frabzuordnung Partein
parteifarben <- c(
  "grey30",        # CDU
  "red",          # SPD
  "deepskyblue3", # AfD
  "#64A12D",    # Grüne
  "purple",       # Linke
  "gold",         # FDP
  "orange",       # Freie Wähler
  "grey60"        # Sonstige
)
#Extra Variable für Parteinnamen
parteinamen <- c("CDU","SPD","AfD","Grüne","Linke","FDP","Freie Wähler","Sonstige")

#Durchschnittliche Ausführlichkeit pro Modell und Land
aggregate(Ausf ~ KI + Land,
          data = daten,
          FUN = mean)
#Mediane Ausführlichkeit pro Modell und Land
aggregate(Ausf ~ KI + Land,
          data = daten,
          FUN = median)


#wie oft jede Partei erwähnt wird:
erwaehnung_spalten <- grep("^Erwähnung 1\\.", names(daten), value = TRUE)

haeufigkeiten <- sapply(erwaehnung_spalten, function(spalte) {
  sum(grepl("^1\\s*-", daten[[spalte]]), na.rm = TRUE)
})


barplot(haeufigkeiten, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen",
        xlab="Partei", ylab="Anzahl der Nennungen", col=parteifarben)


#Boxplot Ausführlichkeit der Partein
boxplot(
  daten[, c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
               "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
               "Ausf_FW", "Ausf_Weiter")],
  las=1,
  names = parteinamen,
  col = parteifarben,
  main = "Vergleich der Antwortlänge je nach Partei",
  ylab = "Zeichenanzahl der Antwort"
)

#Boxplot Ausführlichkeit nach Ländern 
boxplot(
  Ausf ~ Land,
  data = daten,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell 
boxplot(
  Ausf ~ KI,
  data = daten,
  xlab = "KI-Modell",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge nach KI-Modell sortiert "
)
