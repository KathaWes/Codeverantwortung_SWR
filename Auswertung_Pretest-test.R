library(readr)
library(dplyr)
#Einlesen der ersten Tabelle
daten101 <- read_csv("Fragenauswertung - 101 Wer darf wählen_.csv")

#Faktorisieren von character Variablen
daten101 <- daten101 %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum,
      as.factor
    )
  )
#Verkürzung der Variablennamen
names(daten101)[names(daten101) == "Ausführlichkeit 4"] <- "Ausf_4"
names(daten101)[names(daten101) == "KI-Modell"] <- "KI"

#Boxplot Ausführlichkeit nach Ländern 
boxplot(
  Ausf_4 ~ Land,
  data = daten101,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge nach Land sortiert "
)

#Boxplot Ausführlichkeit nach KI-Modell
boxplot(
  Ausf_4 ~ KI,
  data = daten101,
  xlab = "Land",
  ylab = "Zeichenanzahl der Antwort inkl. Leerzeichen",
  main= "Antwortlänge nach Land sortiert "
)


#Histogramm der Ausführlichkeit von 
hist(daten101$Ausf_4,
     ylab = "Häufigkeit",
     xlab = "Anzahl der Zeichen inkl. Leerzeichen",
     main ="Histogramm der Ausführlichkeit bei 'Wer darf wählen'")
