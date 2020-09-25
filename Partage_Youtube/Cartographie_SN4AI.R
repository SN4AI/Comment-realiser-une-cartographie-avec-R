#********************************** Cartographie sur R *******************************

#####################################################
# Projet : Analyse COVID-19 au Sénégal: partie 1    #
# Auteur : Ousmane Sy Bodian,                       #
# Profil : Ingénieur statisticien, Data scientist   # 
# Date début : 25/09/2020                           #
#####################################################


#----------------------- Librairies requises dans ce projet ----------------------

# Chargement des packages pour les données géométriques

#*** Pour installer les packages
install.packages("tidyverse", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)
#..................................................

# Vous faites la même chose pour le reste des packages 


#***--- Chargement des librairies 
library(tidyverse) # tidyverse data visualization package
library(sf)        # Permet de lire les données géométriques
library(raster)    # Le fichier 'shapefile'
# Les packages de visualisation
library(tmap)      # for static and interactive maps
library(viridis)   # palette de couleur gradient
library(leaflet)   # for interactive maps
library(mapview)   # for interactive maps
# library(ggplot2) # tidyverse data visualization package
library(shiny)     # for web applications




#---------------------- Importation de la base de données -----------------------


# La base de données est mis à jours tous les jours
# Celle que nous affichons ici a été télécharger le 30-08-2020


# Jeux de données 'regions_cas18-08-2020' : 
# Nbre de cas confirmés du covid selon la région
region <- read.csv("regions_cas_30-08-2020.csv", header = T, sep = ",") 

# Structure des variables
str(region)


#----------------------- Reconstitution de la base de données --------------------


# Renommer le nom des régions en format 'nom de famille'
# Càd la première lettre en Majuscule et le reste en minuscule
names(region) <- str_to_title(names(region))   

# Consultons la structure des variables
str(region)



#*** Renommer les noms de région avec un accent
names(region)[7] <- "Kédougou"
names(region)[11] <- "Saint-Louis"
names(region)[12] <- "Sédhiou"
names(region)[14] <- "Thiès"

# Consultons la structure des variables
str(region)




# Objectif dans cette partie est de faire comprendre à R 
# que la variable 'Date' est de type 'date' et non une variable 
# catégorielle comme le fait croire R

# Pour y remédier on utilise la fonction de parsing 'as.POSIXct()'
# qui permet de convertir cette variable en classe 'date'



#*** Conversion en type date 'POSIXct()'
region <- region %>%
  mutate(Date = as.POSIXct(Date)) %>%
  arrange(Date)

# Vérification de la structure de date
str(region) 




#*** Extraction du nbre cummulé de cas confirmés

# le Nre cumulés de cas 'confirmés' du covid
confirmes <- region[region$Date == max(region$Date),][-1]

# Convertissons ce vecteur en valeur entières
confirmes2 <- as.integer(confirmes)

# Regroupons le vecteur 'Regions' et ' confirmes' dans une seule base en colonnes
covid19_region <- data.frame(Regions = as.character(names(confirmes)), confirmes = confirmes2)




#--------------------------- Importation du 'fichier shapefile -----------------------

# Un fichier 'shapefile' est le fichier qui contient les formes 
# géométrique de la carte. 
# Entre autres, c'est le fichier qui contient les positions
# géographiques cad les latitudes et les longitudes


# Shapefiles du Sénégal avec les 14 régions administratives
# site : http://www.diva-gis.org/gdata

senegal <- st_read("SEN_adm1.shp", layer = "SEN_adm1", stringsAsFactors = F)


#--------------------------- Reconstitution du 'fichier shapefile' ----------------------

# Dans le fichier shapefiles, nous avons besoin que deux variables
# le nom des régions et les données géographiques

# On crée une nouvelle variable
# qui va contenir le nom des régions
senegal$ID <- senegal$NAME_1

# Restreindre le jeux de données 'senegal' à deux variables
# les régions et les données géométriques
senegal <- senegal[c(11, 10)]


#********** Fusion entre les données Géographiques 'senegal' 
# et le jeux de données 'covid19_region'
MapDataCovid <- inner_join(senegal, covid19_region, by = c("ID" = "Regions")) 


# Attention!!! car les commandes qui générent la carte
# ne prennent que les objets 'sf'
# Vérifions la classe
class(MapDataCovid)



# A présent tout est OK pour Visualiser les carte



#----------------------------------- Visualisation des Cartes --------------------------------


#------------------************* Les cartes à ronds proportionnels ************--------------

# Librairie
library(ggplot2)


#*****----------- Première Catre avec 'ggplot' --------------


# Etape 1 : Première carte basique
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes))











# Etape 2 : Ajout des cercles proportionels aux nbre de cas confirmés
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red", 
                      shape = 20, alpha = 0.6)

# size : taille proportionnelle aux nbre de cas confirmés
# fill : repmlissage de la couleur des régions en fonction du nbre de cas confirmés
# shape : la forme , 20 = cercle  et 22 = carré
# alpha : la transparence des cercles rouges (saturation)









# Etape 3 : Modification à l'échelle gradient du repmlissage des couleurs :  
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red",
                      shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "lightcyan",
                       mid = "slategray1", high = "darkred")

# couleur claire quand le nbre de cas confirmés est faible ;
# couleur rouge sombre quand le nbre de cas confirmés est élevé








# Etape 4 : Modification de la taille des cercles proportionnelles
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red",
                      shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "lightcyan",
                       mid = "slategray1", high = "darkred") +
  scale_size_area(name = "confirmés", max_size = 25)






# Etape 5 : Ajout de titre + changement de theme
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red",
                      shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "Nbre de cas confirmés", low = "lightcyan",
                       mid = "slategray1", high = "darkred") +
  scale_size_area(name = "confirmés", max_size = 25) +
  ggtitle("Nombre de cas Confirmés au Sénégal\n jusqu'à ce jour 30 Août 2020") +
  theme_minimal() # theme du fond





# Etape 6 : Ajout de l'étiquette des différentes régions administratives
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red",
                      shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "confirmés", low = "lightcyan",
                       mid = "slategray1", high = "darkred") +
  scale_size_area(name = "confirmés", max_size = 25) +
  ggtitle("Nombre de cas Confirmés au Sénégal\n jusqu'à ce jour 30 Août 2020") +
  theme_minimal() +
  geom_sf_text(aes(label = ID), vjust = -0.5, check_overlap = T,
               fontface = "italic", colour = "black")





# Etape 7 :

#------- Ajoutons le Nbre de cas confirmés au nom des régions
# Région + Nbre de cas

MapDataCovid <- MapDataCovid %>%
  mutate(char1 = as.character(ID),
         char2=  as.character(confirmes), 
         ID2 = paste(char1, char2, sep = "\n"))

# Affichage
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red",
                      shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "confirmés", low = "lightcyan",
                       mid = "slategray1", high = "darkred") +
  scale_size_area(name = "confirmés", max_size = 25) +
  ggtitle("Nombre de cas Confirmés au Sénégal\n jusqu'à ce jour 18 Août 2020") +
  theme_minimal() +
  geom_sf_text(aes(label = ID2), vjust = -0.5, check_overlap = T,
               fontface = "italic", colour = "black")









# Etape 8 : Elimination des axes et de leurs étiquettes
ggplot(MapDataCovid) +
  geom_sf(aes(fill = confirmes)) +
  stat_sf_coordinates(aes(size = confirmes, fill = confirmes), color = "red",
                      shape = 20, alpha = 0.6) +
  scale_fill_gradient2(name = "confirmés", low = "lightcyan",
                       mid = "slategray1", high = "darkred") +
  scale_size_area(name = "confirmés", max_size = 25) +
  ggtitle("Nombre de cas Confirmés au Sénégal\n jusqu'à ce jour 30 Août 2020") +
  theme_minimal() +
  geom_sf_text(aes(label = ID2), vjust = -0.5, check_overlap = T,
               fontface = "italic", colour = "black") +
  theme(axis.title.x = element_blank(), # Supprimer l'étiquette de l'axe des X
        axis.title.y = element_blank(), # Supprimer l'étiquette de l'axe des Y
        axis.text = element_blank(),    # Supprimer les axes des X et Y
        legend.position = "bottom")     # Position de la légende en bas










#*****-------------------------- Deuxième Catre (Interactive) avec 'tm_shape()' --------------------------------

# Avec la librairie tmap
library(tmap) # for static and interactive maps

#*******------- Carte Interractive 



# Etape 1 : Importation de la carte du Sénégal
tm_shape(MapDataCovid) + 
  tm_polygons()






# Etape 2 : Remplissage des régions selon le nbr de cas confirmés
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes")
# Rendre interractive la Carte
tmap_mode("view")
tmap_last()







# Etape 3 : Ajout de paramètres (arguments)
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirmés") 

# title : Titre









# Etape 4 : Modification de l'echelle de remplissage des couleurs

# Notre propre échelle
breaks = c(0, 0.5, 1, 2, 4, 5, 10, 20, 80, 90) * 100
# Carte
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirmés", 
              breaks = breaks)

# breaks : permet de changer l'echelle de remplissage des couleurs












# Etape 5 : Ajout de l'étiquette des noms de régions
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirmés", 
              breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T)  # ajout des noms de région

# scale : taille de la police de caratères












# Etape 6 : Ajout de cercles à rond proportionnel aux nbr de cas confirmés
tm_shape(MapDataCovid) + 
  tm_polygons("confirmes", id = "ID2",
              title="Nombre de cas Confirmés", 
              breaks = breaks) +
  tm_text("ID2", scale = 1.3, shadow = T) +
  tm_bubbles(size = "confirmes", col = "red", alpha = .5, scale = 5, shape = 20)

# size : taille suivant le nbr de cas confirmés
# alpha : la transparence ou saturation
# shape = 20 : pour la forme ciculaire



