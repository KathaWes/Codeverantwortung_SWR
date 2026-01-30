library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#Einlesen der ersten Tabelle
daten <- read_csv("Fragenauswertung - Parteiempfehlungen66.csv", skip =2) 

#Faktorisieren von character Variablen für Frage 
daten <- daten %>%
  mutate(
    across(
      where(is.character) & !Link & !Anmerkungen & !Datum & !Auffälliges ,
      as.factor
    )
  )

##############Verkürzung der Variablennamen 
# Erwähnungen
names(daten)[names(daten) == "Erwähnung 1.1 CDU"] <- "Erw_CDU"
names(daten)[names(daten) == "Erwähnung 1.2 SPD"] <- "Erw_SPD"
names(daten)[names(daten) == "Erwähnung 1.3 AFD"] <- "Erw_AFD"
names(daten)[names(daten) == "Erwähnung 1.4 Grüne"] <- "Erw_Gruene"
names(daten)[names(daten) == "Erwähnung 1.5 Linke"] <- "Erw_Linke"
names(daten)[names(daten) == "Erwähnung 1.6 FDP"] <- "Erw_FDP"
names(daten)[names(daten) == "Erwähnung 1.7 Freie Wähler"] <- "Erw_FW"
names(daten)[names(daten) == "Erwähnung 1.8 weitere"] <- "Erw_Weitere"
# Empfehlungen
names(daten)[names(daten) == "Empfehlung 2.1 CDU"] <- "Empf_CDU"
names(daten)[names(daten) == "Empfehlung 2.2 SPD"] <- "Empf_SPD"
names(daten)[names(daten) == "Empfehlung 2.3 AFD"] <- "Empf_AFD"
names(daten)[names(daten) == "Empfehlung 2.4 Grüne"] <- "Empf_Gruene"
names(daten)[names(daten) == "Empfehlung 2.5 Linke"] <- "Empf_Linke"
names(daten)[names(daten) == "Empfehlung 2.6 FDP"] <- "Empf_FDP"
names(daten)[names(daten) == "Empfehlung 2.7 Freie Wähler"] <- "Empf_FW"
names(daten)[names(daten) == "Empfehlung 2.8 weitere"] <- "Empf_Weitere"
names(daten)[names(daten) == "Empfehlung 2.9 Anzahl"] <- "Empf_Anzahl"
# Konstruktionen
names(daten)[names(daten) == "Konstruktion 3.1 CDU"] <- "K_CDU"
names(daten)[names(daten) == "Konstruktion 3.2 SPD"] <- "K_SPD"
names(daten)[names(daten) == "Konstruktion 3.3. AFD"] <- "K_AFD"
names(daten)[names(daten) == "Konstruktion 3.4 Grüne"] <- "K_Gruene"
names(daten)[names(daten) == "Konstruktion 3.5 Linke"] <- "K_Linke"
names(daten)[names(daten) == "Konstruktion 3.6 FDP"] <- "K_FDP"
names(daten)[names(daten) == "Konstruktion 3.7 Freie Wähler"] <- "K_FW"
names(daten)[names(daten) == "Konstruktion 3.8 weitere"] <- "K_Weitere"
#Nennungen
names(daten)[names(daten) == "Nennung 1"] <- "N1"
names(daten)[names(daten) == "Nennung 2"] <- "N2"
names(daten)[names(daten) == "Nennung 3"] <- "N3"
names(daten)[names(daten) == "Nennung 4"] <- "N4"
names(daten)[names(daten) == "Nennung 5"] <- "N5"
names(daten)[names(daten) == "Nennung 6"] <- "N6"
names(daten)[names(daten) == "Nennung 7"] <- "N7"
names(daten)[names(daten) == "Nennung 8"] <- "N8"
#Kontext
names(daten)[names(daten) == "Kontext 4.1"] <- "Kontext_1"
names(daten)[names(daten) == "Kontext 4.2"] <- "Kontext_2"
#Ausführlichkeit
names(daten)[names(daten) == "Ausführlichkeit  5.0 Gesamt"] <- "Ausf"
names(daten)[names(daten) == "Ausführlichkeit  5.1 CDU"] <- "Ausf_CDU"
names(daten)[names(daten) == "Ausführlichkeit  5.2 SPD"] <- "Ausf_SPD"
names(daten)[names(daten) == "Ausführlichkeit  5.3 AFD"] <- "Ausf_AFD"
names(daten)[names(daten) == "Ausführlichkeit  5.4 Grüne"] <- "Ausf_Gruene"
names(daten)[names(daten) == "Ausführlichkeit  5.5 Linke"] <- "Ausf_Linke"
names(daten)[names(daten) == "Ausführlichkeit  5.6 FDP"] <- "Ausf_FDP"
names(daten)[names(daten) == "Ausführlichkeit  5.7 Freie Wähler"] <- "Ausf_FW"
names(daten)[names(daten) == "Ausführlichkeit  5.8 Weitere"] <- "Ausf_Weiter"
#Weiteres
names(daten)[names(daten) == "KI-Modell"] <- "KI"

#Entfernen von leeren Zeilen wo weder Land noch KI angegeben sind
daten <- daten %>%
  filter(!(is.na(Land) & is.na(KI)))

######### SPÄTER ENTFERNEN HOFFENTLICH ############
#Entfernen von noch nicht codierten Zeilen
daten <- daten %>% filter(!(is.na(Codierung)))

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
erwaehnung_spalten <- grep("^Erw_", names(daten), value = TRUE)

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

#Häufigkeitstabellen
table(daten$Empf_CDU)
table(daten$K_CDU)

# Häufigkeit Persona bezogene Antwort
barplot(table(daten$`nur Thema der Persona`), col="steelblue", 
        names.arg = c('Nein', 'Ja'),
        main="Antwort bezieht sich ausschlielich auf Persona")

# Mittelwerte der Ausführlichkeit je Partei und KI-Modell
ausfuerlichkeit_long <- daten %>%
  select(KI, starts_with("Ausf_")) %>%
  pivot_longer(cols = starts_with("Ausf_"),
               names_to = "Partei",
               values_to = "Ausf") %>%
  mutate(Partei = str_remove(Partei, "Ausf_"))

ggplot(ausfuerlichkeit_long, aes(x = Partei, y = Ausf, fill = KI)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Antwortlänge nach Partei und KI-Modell",
       x = "Partei", y = "Wortanzahl der Antwort") +
  theme_minimal()



########### Häufigkeiten der Konstruktionen für verscheidene Partein###########
# Häufigkeit Konstruktionen CDU
daten %>%
  select(K_CDU) %>%
  separate_rows(K_CDU, sep = ",") %>%
  mutate(K_CDU = str_trim(K_CDU)) %>%
  count(K_CDU, sort = TRUE)

# Häufigkeit Konstruktionen SPD
daten %>%
  select(K_SPD) %>%
  separate_rows(K_SPD, sep = ",") %>%
  mutate(K_SPD = str_trim(K_SPD)) %>%
  count(K_SPD, sort = TRUE)

# Häufigkeit Konstruktionen AFD
daten %>%
  select(K_AFD) %>%
  separate_rows(K_AFD, sep = ",") %>%
  mutate(K_AFD = str_trim(K_AFD)) %>%
  count(K_AFD, sort = TRUE)

# Häufigkeit Konstruktionen Grüne
daten %>%
  select(K_Gruene) %>%
  separate_rows(K_Gruene, sep = ",") %>%
  mutate(K_Gruene = str_trim(K_Gruene)) %>%
  count(K_Gruene, sort = TRUE)

# Häufigkeit Konstruktionen Linke
daten %>%
  select(K_Linke) %>%
  separate_rows(K_Linke, sep = ",") %>%
  mutate(K_Linke = str_trim(K_Linke)) %>%
  count(K_Linke, sort = TRUE)

# Häufigkeit Konstruktionen FDP
daten %>%
  select(K_FDP) %>%
  separate_rows(K_FDP, sep = ",") %>%
  mutate(K_FDP = str_trim(K_FDP)) %>%
  count(K_FDP, sort = TRUE)

# Häufigkeit Konstruktionen FW
daten %>%
  select(K_FW) %>%
  separate_rows(K_FW, sep = ",") %>%
  mutate(K_FW = str_trim(K_FW)) %>%
  count(K_FW, sort = TRUE)

# Häufigkeit Konstruktionen Weitere
daten %>%
  select(K_Weitere) %>%
  separate_rows(K_Weitere, sep = ",") %>%
  mutate(K_Weitere = str_trim(K_Weitere)) %>%
  count(K_Weitere, sort = TRUE)
############################################################
#Score Nennungen 


# Daten in langes Format umwandeln
points_vec <- 8:1
df_long <- daten %>%
  select(N1:N8) %>%   # nur N1 bis N8
  mutate(id = row_number()) %>%
  pivot_longer(cols = N1:N8, names_to = "Position", values_to = "Partei") %>%
  mutate(Punkte = points_vec[as.numeric(sub("N", "", Position))])
# Liste der unerwünschten Antworten
ungültige_parteien <- c(
  "es wird keine weitere Partei genannt",
  "50 - ChatBot verweigert die Aussage",
  "99 - Antwort uneindeutig"
)
# Punkte pro Partei summieren, aber unerwünschte Antworten entfernen
partei_scores <- df_long %>%
  filter(!Partei %in% ungültige_parteien) %>%   # filtert die "Fake-Parteien" raus
  group_by(Partei) %>%
  summarise(Score = sum(Punkte)) %>%
  arrange(desc(Score))


ggplot(partei_scores, aes(x = reorder(Partei, Score), y = Score)) +
  geom_col(fill = "steelblue") +       # Balken
  coord_flip() +                       # horizontale Balken
  labs(title = "Partei-Scores", x = "Partei", y = "Score") +
  theme_minimal()



#########################################
# Analyse als Funktion, um verschiedene Datensätze (von Personas) zu analysieren

analyse_partei_daten <- function(df, person_name = "Gesamt"){
  
  #-------------------------------
  # Vorbereitung: Farben & Parteinamen
  parteifarben <- c(
    "grey30", "red", "deepskyblue3", "#64A12D",
    "purple", "gold", "orange", "grey60"
  )
  parteinamen <- c("CDU","SPD","AfD","Grüne","Linke","FDP","Freie Wähler","Sonstige")
  
  #-------------------------------
  # Basisstatistiken
  
  cat("\n📊 Durchschnittliche Ausführlichkeit pro KI und Land:\n")
  print(aggregate(Ausf ~ KI + Land, data = df, FUN = mean))
  
  cat("\n📈 Median-Ausführlichkeit pro KI und Land:\n")
  print(aggregate(Ausf ~ KI + Land, data = df, FUN = median))
  
  
  #-------------------------------
  # Häufigkeit der Erwähnungen
  
  erwaehnung_spalten <- grep("^Erw_", names(df), value = TRUE)
  
  haeufigkeiten <- sapply(erwaehnung_spalten, function(spalte) {
    sum(grepl("^1\\s*-", df[[spalte]]), na.rm = TRUE)
  })
  
  p_bar_erw <- barplot(
    haeufigkeiten,
    names.arg=parteinamen,
    main= paste("Häufigkeit der Parteinennungen","(",person_name,")"),
    xlab="Partei",
    ylab="Anzahl der Nennungen",
    col=parteifarben
  )
  
  
  #-------------------------------
  # Boxplots zur Ausführlichkeit
  
  boxplot(
    df[, c("Ausf_CDU", "Ausf_SPD", "Ausf_AFD",
           "Ausf_Gruene", "Ausf_Linke", "Ausf_FDP",
           "Ausf_FW", "Ausf_Weiter")],
    las=1,
    names = parteinamen,
    col = parteifarben,
    main = paste("Vergleich der Antwortlänge je nach Partei","(",person_name,")"),
    ylab = "Wörteranzahl der Antwort"
  )
  
  
  p_box_land <- ggplot(df, aes(x=Land, y=Ausf)) +
    geom_boxplot(fill="steelblue") +
    labs(title= paste("Antwortlänge nach Land sortiert","(",person_name,")"),
         x="Land", y="Wörteranzahl") +
    theme_minimal()
  
  print(p_box_land)
  
  
  p_box_ki <- ggplot(df, aes(x=KI, y=Ausf)) +
    geom_boxplot(fill="steelblue") +
    labs(title= paste("Antwortlänge nach KI-Modell sortiert","(",person_name,")"),
         x="KI-Modell", y="Wörteranzahl") +
    theme_minimal()
  
  print(p_box_ki)
  
  
  #-------------------------------
  # Partei-Scores (gewichtete Nennungen nach Position)
  
  points_vec <- c(8,7,6,5,4,3,2,1)   # Punkte für Positionen N1–N8
  
  df_long <- df %>%
    select(N1:N8) %>%
    mutate(id=row_number()) %>%
    pivot_longer(cols=N1:N8,names_to="Position",values_to="Partei") %>%
    mutate(Punkte=points_vec[as.numeric(sub("N","",Position))])
  
  ungültige_parteien <- c(
    "es wird keine weitere Partei genannt",
    "50 - ChatBot verweigert die Aussage",
    "99 - Antwort uneindeutig"
  )
  
  partei_scores <- df_long %>%
    filter(!Partei %in% ungültige_parteien) %>%
    group_by(Partei) %>%
    summarise(Score=sum(Punkte)) %>%
    arrange(desc(Score))
  
  p_score <- ggplot(partei_scores,aes(x=reorder(Partei,Score),y=Score))+
    geom_col(fill="steelblue")+
    coord_flip()+
    labs(title=paste("Partei-Scores (gewichtete Nennungen)","(",person_name,")"),
         x="Partei",y="Score")+
    theme_minimal()
  
  print(p_score)
  
  
  #-------------------------------
  # Durchschnittliche Antwortlänge pro Partei und KI
  
  ausf_long <- df %>%
    select(KI, starts_with("Ausf_")) %>%
    pivot_longer(cols=starts_with("Ausf_"),
                 names_to="Partei",
                 values_to="Ausf") %>%
    mutate(Partei=str_remove(Partei,"Ausf_"))
  
  p_ausflang <- ggplot(ausf_long,aes(x=Partei,y=Ausf,fill=KI))+
    geom_boxplot()+
    scale_fill_brewer(palette="Set2")+
    labs(title=paste("Antwortlänge nach Partei und KI-Modell","(",person_name,")"),
         x="Partei",y="Wörteranzahl")+
    theme_minimal()
  
  print(p_ausflang)
  
  
  #-------------------------------
 
  cat("\n✅ Analyse abgeschlossen!\n")
}

# Allgemein
analyse_partei_daten(daten[daten$Persona == "1 - Allgemein",])

#Jan
analyse_partei_daten(daten[daten$Persona == "2 - Jan.",])

#Peter
analyse_partei_daten(daten[daten$Persona == "3 - Peter",])

#Anna
analyse_partei_daten(daten[daten$Persona == "4 - Anna",])

#Sabine
analyse_partei_daten(daten[daten$Persona == "5 - Sabine",])

# Lukas
analyse_partei_daten(daten[daten$Persona == "6 - Lukas",])

# Thomas
analyse_partei_daten(daten[daten$Persona == "7- Thomas",])

#Mia
analyse_partei_daten(daten[daten$Persona == "8- Mia",])

