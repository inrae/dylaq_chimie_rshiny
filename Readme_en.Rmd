

# RShiny application to visualize chimical data of DYLAQ database 
***

Author: David Funosas - INRAE

Date: 2021, June

Copyright: © INRAE, 2021

Application distributed under license **Open License**.

***

This is an interactive web app developed as a tool to easily access, visualize and manipulate physico-chemical data from the DYLAQ database. The app is available both in English and in French and includes the following 4 modules: 

## Stations map

This module shows all sampling stations on an OpenStreetMap leaflet so that the user can easily visualize each station's precise location. By clicking on a station marker, basic information about the station is presented, including its name and precise coordinates, as well as the references to scientific reports from which sample data have been obtained for the selected station.

## Sample data

In this module, the user can select any sample found in the database, whose physico-chemical results will be shown in a data table. This allows for an easy access to and visualization of physico-chemical measurements, e.g. nutrient concentrations or chlorophyll abundances, conducted in a given date and at a given sampling station. The list of samples available to choose from can be filtered by selecting a schema from the DYLAQ database, as well as one or more lakes, sampling stations and/or sample types in the filters in the top-left corner of the screen. This possibility makes it easier to access the data of a particular schema, lake, station or sample type the user might be interested in. 

Once the table is generated, the user can sort and filter its results by any column. These results include the values obtained for each physico-chemical measurement in the sample, along with the corresponding unit of measurement and the depth and substrate where the sample was taken. The presented data can also be exported in CSV format to facilitate its further study. 

Two additional options related to sampling stations are available as well, and can be rendered visible or invisible by clicking on the **Settings** button in the top-right corner. The first of these options, *Restrict selection to geolocalized stations*, allows the user to filter results so that only data coming from sampling stations whose precise spatial coordinates are known can be selected. It should be noted that, by activating this option, sample types and samples will be filtered accordingly, i.e. samples taken in non-geolocalized stations will not be available to select. 
  
The second option, *Homogenize units of measurement*, allows to choose whether to show values as originally found in the scientific reports or to homogenize units of measurement when possible (e.g. transforming values measured in mg(NO2)/L, mg(NO3)/L, mg(NH4)/L... to mg(N)/L) in order to facilitate comparisons between different physico-chemical properties.


## Physico-chemical dynamics over time

This module allows the user to choose multiple physico-chemical properties and then generates a scatter plot with the average yearly values for the selected variables. The values are plotted on an editable timeline set by default to comprise the whole period for which samples are included in the database, i.e. from 1963 to 2019. This plot provides the user with a basic picture of how physico-chemical properties have evolved over time in the sampled lakes. 

As in the previous module, it is also possible to select a schema and one or more lakes, sampling stations, sample types and, additionally, sampling substrates (e.g. water, sediments) to filter out results that are not of interest to the user. Likewise, by clicking at the **Settings** button, several visualization options are displayed, which include:

  - Showing all abundance values or only yearly averages
  - Showing a scatter plot or a box plot
  - Showing the results on a linear scale or on a logarithmic scale. Both the binary and decimal logarithms are offered as options.
  - Showing all values or only values within a certain range. This option allows to filter out extreme values.
  - Limiting results to samples taken between two specific days of the year. This can eliminate seasonal variability as a confounding factor and allow for a better comparison of seasonally variable properties over the years. Dates can be defined either automatically, by selecting a season (e.g. December 21st to March 20th for winter), or manually if different dates are preferred.
  - Limiting results to samples taken within a certain water depth range
  - Limiting results to samples taken within a certain year range (e.g. 1968 to 1973)
  - Showing or hiding the regression line in the scatter plot
  - The *Homogenize units of measurement* and *Restrict selection to geolocalized stations* options already described in the **Sample data** section
  - Displaying information about the currently selected station (provided only one station is selected) under the station selection filter. 
  
The plotted results can be exported as a CSV file as well. 

## CSV Export

The **CSV Export** module, despite providing similar search and filtering tools as the **Physico-chemical dynamics over time** module, has as a very different purpose. The idea behind this module is helping the user select, filter and export physico-chemical data they might be interested in working with later. Hence, both the selected results and exported variables are highly customizable. In addition to several of the options already described in the previous module, the **Settings** panel offers the possibility of choosing the variables the user wants to export along with the physico-chemical property measured, the unit of measurement used and the value obtained. 
