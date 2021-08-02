# Application RShiny de visualisation des données de chimie de la base de données DYLAQ

***

Auteur : David Funosas - INRAE

Date de rédaction : juin 2021

Copyright : © INRAE, 2021

Application distribuée sous licence **Licence Ouverte**.

***

Cette application web intéractive a été développée pour accéder, visualiser et manipuler facilement des données physico-chimiques de la base de données DYLAQ. Elle est disponible en français et en anglais et inclut les 4 modules suivants :

## Carte des stations

Ce module montre toutes les stations d'échantillonnage sur une carte d'OpenStreetMap afin que l'utilisateur puisse visualiser facilement leur localisation précise. En cliquant sur un marqueur, des informations basiques sur la station sont présentées, comme son nom, ses coordonnées géographiques exactes, ainsi que les références aux rapports scientifiques à partir desquels des données d'échantillonnage ont été obtenues. 


## Prélèvements

Dans ce module, l'utilisateur peut sélectionner un prélèvement dans la base de données, dont les résultats physico-chimiques vont être présentés dans un tableau de données. 

Il permet de visualiser facilement les mesures physico-chimiques, par exemple les concentrations de nutriments ou l'abondance de la chlorophylle, effectuées à une date et dans une station d'échantillonnage données. La liste des prélèvements peut être filtrée par lac, schéma de la base de données DYLAQ (chaque schéma correspond à un maillon ou une partie de maillon), ou par type de prélèvement.

Une fois le tableau généré, l'utilisateur peut trier et filtrer les résultats par n'importe quelle colonne. Ces résultats incluent les valeurs obtenues pour chaque mesure physico-chimique dans le prélèvement, ainsi que l'unité de mesure correspondante, la profondeur et  le substrat échantillonné. Les données présentées peuvent être exportées en format CSV.

Deux options supplémentaires relatives aux stations d'échantillonnage sont également disponibles, et peuvent être rendues visibles en cliquant sur le bouton **Paramètres** du coin supérieur droit de l'écran :

- *Limiter la sélection aux stations géolocalisées* permet à l'utilisateur de filtrer les résultats afin de n'afficher que les données obtenues dans les stations géolocalisées précisément.
- *Homogénéiser les unités* permet de choisir entre montrer les valeurs originales, telles qu'elles figurent dans les rapports scientifiques, ou homogénéiser les unités de mesure quand c'est possible (par exemple, en transformant toutes les valeurs mesurées en mg(NO2)/L, mg(NO3)/L, mg(NH4)/L... en mg(N)/L) pour faciliter les comparaisons entre les différents paramètres physico-chimiques.

## Dynamiques physico-chimiques au fil du temps

Ce module permet à l'utilisateur de choisir plusieurs paramètres physico-chimiques et générer un diagramme de dispersion avec les moyennes annuelles des variables sélectionnées. Les valeurs sont tracées sur une chronologie qui comprend par défaut toute la période pour laquelle il existedes prélèvements dans la base de données, c'est à dire de 1963 à 2019. Ce graphique offre une vision globale de l'évolution des valeurs physico-chimiques au fil du temps dans les lacs échantillonnés.

Comme dans le module précédent, il est également possible de sélectionner un schéma ou un ou plusieurs lacs, stations d'échantillonnage, types de prélèvement et, additionnellement, substrats (p.ex. de l'eau ou des sédiments) . De même, en cliquant sur le bouton de **Paramètres**, les options de visualisation suivantes sont présentées: 

  - Montrer toutes les valeurs ou juste les moyennes annuelles
  - Montrer un diagramme de dispersion ou un diagramme en boîtes
  - Montrer les résultats sur une échelle linéaire ou sur une échelle logarithmique (binaire ou décimale). 
  - Montrer toutes les valeurs ou juste les valeurs dans un certain intervalle. Cette option permet de filtrer les valeurs extrêmes.
  - Limiter les résultats aux prélèvements effectués entre deux jours de l'année. Cela peut éliminer la variabilité saisonnière comme facteur de confusion et permettre une meilleure comparaison entre des paramètres saisonniers au fil des années. Les dates peuvent être précisées de manière automatique en sélectionnant une saison (p.ex. du 21 Décembre au 20 Mars pour l'hiver) ou manuellement si d'autres dates sont préférées.
  - Limiter les résultats aux échantillons prélevés dans un certain intervalle de profondeur
  - Limiter les résultats aux échantillons prélevés dans un certain intervalle d'années (p.ex. de 1968 à 1973)
  - Montrer ou cacher la ligne de regression dans le diagramme de dispersion
  - Les deux options *Homogénéiser les unités* et *Limiter la sélection aux stations géolocalisées* décrites dans le module précédent
  - Montrer des informations sur la station actuellement sélectionnée (au cas où une seule station est sélectionnée) au dessous du filtre de sélection des stations.
  
Les résultats affichés sous forme de graphique peuvent être exportés en CSV.

## Exportation en CSV

Le module d'**Exportation en CSV**, bien qu'il offre des outils de recherche et filtrage similaires à ceux du module de **Dynamiques physico-chimiques au fil du temps**, a un objectif très différent. L'idée derrière ce module est d'offrir à l'utilisateur la possibilité de sélectionner, filtrer et exporter des données physico-chimiques pour qu'il puisse les étudier *a posteriori*. Par conséquent, les résultats sélectionnés et les variables exportées sont hautement personnalisables. En plus de plusieurs des options du module précédent, le panneau **Paramètres** offre la possibilité de choisir les variables que l'utilisateur veut exporter conjointement avec le paramètre physico-chimique analysé, l'unité de mesure utilisée et la valeur obtenue.

