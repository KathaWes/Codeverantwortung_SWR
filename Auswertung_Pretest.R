#--------------------------------------------

# Plots in Pdf speichern
pdf("KIPlots.pdf", width = 8, height = 5)
par("mar" = c(5, 4, 1.5, 2))
par(cex.lab = 1.3, cex.axis = 1.3)
#-----------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
print(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

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
#Eindeutiger Bennenung der ausführlichkeit bei 201
names(daten201)[names(daten201) == "Ausführlichkeit  2.1"] <- "Ausf_CDU"
names(daten201)[names(daten201) == "Ausführlichkeit  2.2"] <- "Ausf_SPD"
names(daten201)[names(daten201) == "Ausführlichkeit  2.3"] <- "Ausf_AFD"
names(daten201)[names(daten201) == "Ausführlichkeit  2.4"] <- "Ausf_Grüne"
names(daten201)[names(daten201) == "Ausführlichkeit  2.5"] <- "Ausf_Linke"
names(daten201)[names(daten201) == "Ausführlichkeit  2.6"] <- "Ausf_FDP"
names(daten201)[names(daten201) == "Ausführlichkeit  2.7"] <- "Ausf_FW"
names(daten201)[names(daten201) == "Ausführlichkeit  2.8"] <- "Ausf_sonsitige"
names(daten201)[names(daten201) == "Ausführlichkeit  3"] <- "Ausf_insgesamt"
names(daten201)[names(daten201) == "KI-Modell"] <- "KI"
#Weitere Verkürzung der Variablennamen 
names(daten202)[names(daten202) == "Ausführlichkeit 4"] <- "Ausf"
names(daten202)[names(daten202) == "KI-Modell"] <- "KI"
names(daten203)[names(daten203) == "Ausführlichkeit 4"] <- "Ausf"
names(daten203)[names(daten203) == "KI-Modell"] <- "KI"
names(daten204)[names(daten204) == "Ausführlichkeit 4"] <- "Ausf"
names(daten204)[names(daten204) == "KI-Modell"] <- "KI"
names(daten205)[names(daten205) == "Ausführlichkeit 4"] <- "Ausf"
names(daten205)[names(daten205) == "KI-Modell"] <- "KI"

#=========== Frage 101 ===============
#Durchschnittliche Ausführlichkeit pro Modell und Land
aggregate(Ausf ~ KI + Land,
          data = daten101,
          FUN = mean)
#             KI              Land     Ausf
# 1 Chat-GPT 5.1 Baden-Württenberg 1466.400
# 2  Gemini FAST Baden-Württenberg 1580.750
# 3 Chat-GPT 5.1   Rheinland-Pfalz 1474.200
# 4  Gemini FAST   Rheinland-Pfalz 1527.167

# Häufigkeit korrekter Antworten
table(daten101$KI, daten101$'Korrekteit 2.1')

  #              1 - diese Antwort ist richtig
  # Chat-GPT 5.1                             7
  # Gemini FAST                             10
  #             
  #              2 - diese Antwort ist falsch
  # Chat-GPT 5.1                            3
  # Gemini FAST                             0

# Zusammenhang zwischen Modell und Korrektheit
mosaicplot(~ KI + `Korrekteit 2.1`,
           data=daten101,
           color=TRUE,
           main="Zusammenhang zwischen Modell und Korrektheit - Frage 101")

#Antwortqualität nach Modell und Land
ggplot(daten101,
       aes(x=`Korrekteit 2.1`, fill=KI)) +
  geom_bar(position="dodge") +
  facet_wrap(~Land) +
  labs(title="Antwortqualität nach Modell und Land - Frage 101",
       x="Bewertung der Antwort", y="Anzahl")

# Zusammenhang zwischen Korrektheit und Ausführlichkeit
boxplot(Ausf ~ `Korrekteit 2.1`,
        data = daten101,
        xlab="Korrektheit",
        ylab="Zeichenanzahl",
        main="Antwortlänge nach Richtigkeit")

daten101$Korr_num <- ifelse(daten101$`Korrekteit 2.1` == 
                              "1 - diese Antwort ist richtig", 1, 0)
cor(daten101$Ausf, daten101$Korr_num)
# [1] 0.3493144
#-> Längere Antworten sind tendenziell korrekter.

#Häufigkeit genannter Kontexte
for (var in c("Kontext 3.1","Kontext 3.2","Kontext 3.3",
              "Kontext 3.4","Kontext 3.5")) {
  cat("\n", var, "\n")
  print(table(daten101[[var]]))
}


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
hist(daten101$Ausf,
     ylab = "Häufigkeit",
     xlab = "Anzahl der Zeichen inkl. Leerzeichen",
     main ="Histogramm der Ausführlichkeit bei 'Wer darf wählen'")

#=========== Frage 102 ===============
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
#=========== Frage 103 ===============

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

#=========== Frage 104 ===============

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

#=========== Frage 105 ===============
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
#=========== Frage 201 ===============
# wie oft jede Partei erwähnt wird:
parteinamen <- c("CDU","SPD","AfD","Grüne","Linke","FDP","Freie Wähler","Sonstige")
haeufigkeiten <- sapply(parteinamen,
                        function(p) sum(grepl("1", daten201[[paste0("Antwort 1.", which(parteinamen==p))]])))
barplot(haeufigkeiten, names.arg=parteinamen,
        main="Häufigkeit der Parteinennungen",
        xlab="Partei", ylab="Anzahl der Nennungen", col="steelblue")

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
#=========== Frage 202 ===============
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

#=========== Frage 203 ===============
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

#=========== Frage 204 ===============
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

#=========== Frage 205 ===============
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

#------------------------------
dev.off() #Graphik-Fenster schließen (wichtig für PDF)