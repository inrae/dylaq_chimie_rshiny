# rm(list=ls())

# library(rsconnect)
# rsconnect::deployApp('./')

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(lubridate)
library(leaflet)
library(htmltools)
library(shinyFiles)
library(rgdal)
library(shiny.i18n)
library(DT)
library(RPostgreSQL)
library(stringr)

Sys.setenv(LANG = "en")


# Setting current language and database connection
translator <- Translator$new(translation_json_path = "translations/dictionary.json")
global_config_file_path <- file.path("global_config.csv")
global_config <- read.csv2(global_config_file_path, encoding = 'UTF-8')

current_language <- global_config$current_language[1]
db_user <- global_config$db_user[1]
db_password <- global_config$db_password[1]
db_server <- global_config$db_server[1]
db_port <- global_config$db_port[1]
db_name <- global_config$db_name[1]
connect_to_DB <- global_config$connect_to_db[1]

language <- reactiveValues(x=current_language)
translator$set_translation_language(current_language)

#--------------------------------------LOADING DATA-------------------------------------------

if (connect_to_DB) {
  # Database connection settings
  pw <- {db_password}
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db_name,
                   host = db_server, port = db_port,
                   user = db_user, password = pw)
  on.exit(dbDisconnect(con))
  on.exit(dbUnloadDriver(drv), add = TRUE)
  
  chimie_sample <- dbGetQuery(con, "SELECT * FROM chimie.sample") %>%
    mutate(schema = 'chimie')
  
  phyto_sample <- dbGetQuery(con, "SELECT A.obs_pc_id AS sample_id, EXTRACT(YEAR FROM C.date_obs) AS sampling_year, EXTRACT(MONTH FROM C.date_obs) AS sampling_month, 
  C.date_obs AS sampling_date, A.heure_pc AS start_time, NULL::time without time zone AS end_time, D.cd_sandre_zone AS sampling_zone_sandre_code,
  B.prof_debut AS min_sampling_depth, B.prof_fin AS max_sampling_depth, C.bibliography_id, C.camp_id AS campaign_id, C.station_id
  FROM phyto.obs_phychim A
  LEFT JOIN phyto.prelevement B ON B.prel_id = A.prel_id
  LEFT JOIN phyto.liste_observation C ON C.obs_id = B.obs_id
  LEFT JOIN phyto.zone_prel D ON D.zone_prel_id = B.zone_prel_id") %>%
    mutate(schema = 'phyto')
  
  plan_deau_sample <- dbGetQuery(con, "SELECT A.id_prelev AS sample_id, EXTRACT(YEAR FROM A.date_debut_prel) AS sampling_year, EXTRACT(MONTH FROM A.date_debut_prel) AS sampling_month, 
  A.date_debut_prel::date AS sampling_date, A.heure_debut_prel::time AS start_time, A.heure_fin_prel::time AS end_time, A.cd_zone AS sampling_zone_sandre_code,
  CASE
    WHEN A.cd_zone LIKE '1' THEN 0
    ELSE A.profondeur
  END AS min_sampling_depth, A.profondeur AS max_sampling_depth, NULL::integer AS bibliography_id, A.id_campagne_diag AS campaign_id,
  A.id_point_prelev AS station_id
  FROM plan_deau.prelevement_physico_chimique A;") %>%
    mutate(schema = 'plan_deau')
  
  naiades_sample <- dbGetQuery(con, "SELECT A.sampling_id AS sample_id, NULL::integer AS campaign_id, A.station_id, NULL::integer AS bibliography_id, EXTRACT(YEAR FROM A.sampling_date)::int AS sampling_year, EXTRACT(MONTH FROM A.sampling_date)::int AS sampling_month, 
A.sampling_date, NULL::time AS start_time, NULL::time AS end_time, B.vertical_zone_sandre_code AS sampling_zone_sandre_code, A.depth AS min_sampling_depth, A.depth AS max_sampling_depth
FROM naiadechimie.sampling A
LEFT JOIN naiadechimie.vertical_zone B ON B.vertical_zone_id = A.vertical_zone_id;") %>%
    mutate(schema = 'naiadechimie')
  
  v_bibliography <- dbGetQuery(con, "SELECT * FROM public.v_bibliography") %>%
                    replace(is.na(.), '')
  
  lake <- dbGetQuery(con, "SELECT * FROM public.lake") %>%
    mutate(lake_name = gsub(' \\(ÉTANG DE \\)', '', gsub(" \\(ÉTANG D'\\)", '', gsub(' \\(ETANG DU \\)', '', gsub(" \\(ÉTANG D' \\)", '', gsub(' \\(ÉTANG DU \\)', '', toupper(lake_name))))))) %>%
    arrange(lake_name)
  
  chimie_station <- dbGetQuery(con, "SELECT * FROM chimie.station") %>%
    mutate(schema = 'chimie',
           projection_code = 26)
  
  phyto_station <- dbGetQuery(con, "SELECT * FROM phyto.station") %>%
    mutate(schema = 'phyto',
           projection_code = 26)
  
  plan_deau_station <- dbGetQuery(con, "SELECT * FROM plan_deau.point_prel_eaux_surf") %>%
    mutate(schema = 'plan_deau',
           geom = NA,
           station_name = as.character(id_point_prelev)) %>%
    rename('station_id' = 'id_point_prelev', 
           'x_coord' = 'coord_x',
           'y_coord' = 'coord_y',
           'lake_code' = 'code_lac',
           'projection_code' = 'cd_proj') %>%
    left_join(lake %>%
                select(lake_id, lake_code),
              by='lake_code') %>%
    select(station_id, station_name, x_coord, y_coord, geom, lake_id, schema, projection_code)
  
  naiades_station <- dbGetQuery(con, "SELECT station_id, station_name, coordx AS x_coord, coordy AS y_coord, NULL::geometry(point,2154) AS geom, lake_id
    FROM naiadechimie.station;") %>%
    mutate(schema = 'naiadechimie',
           projection_code = 26)
  
  result <- dbGetQuery(con, "SELECT * FROM public.v_res_homogeneises_global") %>%
    mutate(value = as.numeric(value),
           original_value = as.numeric(original_value),
           parameter_id = as.integer(parameter_id))
  
  substrate <- dbGetQuery(con, "SELECT * FROM public.substrate") %>%
    arrange(substrate_name)
  
  parameter <- dbGetQuery(con, "SELECT * FROM public.parameter") %>%
    arrange(parameter_name)
  
  unit  <- dbGetQuery(con, "SELECT * FROM public.unit") %>%
    arrange(unit_name)
  
  sample_type <- dbGetQuery(con, "SELECT * FROM sandre.tsa_n430_zone_vertic_prospect") %>%
    select(cd, mnemo) %>%
    filter(cd != '0') %>%
    rename('sample_type_id' = 'cd',
           'sample_type_name' = 'mnemo') %>%
    mutate(sample_type_id = as.integer(sample_type_id))
  
  # Closing database connection
  dbDisconnect(con)
  dbUnloadDriver(drv)

  # lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
} else {
  chimie_sample_file_path <- file.path("data", "chimie_sample.csv")
  chimie_sample <- read.csv2(chimie_sample_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'chimie')
  
  phyto_sample_file_path <- file.path("data", "phyto_sample.csv")
  phyto_sample <- read.csv2(phyto_sample_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'phyto')
  
  plan_deau_sample_file_path <- file.path("data", "plan_deau_sample.csv")
  plan_deau_sample <- read.csv2(plan_deau_sample_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'plan_deau')
  
  naiades_sample_file_path <- file.path("data", "naiades_sample.csv")
  naiades_sample <- read.csv2(naiades_sample_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'naiadechimie')
  
  v_bibliography_file_path <- file.path("data", "v_bibliography.csv")
  v_bibliography <- read.csv2(v_bibliography_file_path, encoding = 'UTF-8') %>%
    replace(is.na(.), '')
  
  lake_file_path <- file.path("data", "lake.csv")
  lake <- read.csv2(lake_file_path, encoding = 'UTF-8') %>%
    mutate(lake_name = gsub(' \\(ÉTANG DE \\)', '', gsub(" \\(ÉTANG D'\\)", '', gsub(' \\(ETANG DU \\)', '', gsub(" \\(ÉTANG D' \\)", '', gsub(' \\(ÉTANG DU \\)', '', toupper(lake_name))))))) %>%
    arrange(lake_name)
  
  chimie_station_file_path <- file.path("data", "chimie_station.csv")
  chimie_station <- read.csv2(chimie_station_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'chimie',
           projection_code = 26)
  
  phyto_station_file_path <- file.path("data", "phyto_station.csv")
  phyto_station <- read.csv2(phyto_station_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'phyto',
           projection_code = 26)
  
  plan_deau_station_file_path <- file.path("data", "plan_deau_station.csv")
  plan_deau_station <- read.csv2(plan_deau_station_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'plan_deau',
           geom = NA,
           station_name = as.character(id_point_prelev)) %>%
    rename('station_id' = 'id_point_prelev', 
           'x_coord' = 'coord_x',
           'y_coord' = 'coord_y',
           'lake_code' = 'code_lac',
           'projection_code' = 'cd_proj') %>%
    left_join(lake %>%
                select(lake_id, lake_code),
              by='lake_code') %>%
    select(station_id, station_name, x_coord, y_coord, geom, lake_id, schema, projection_code)
  
  naiades_station_file_path <- file.path("data", "naiades_station.csv")
  naiades_station <- read.csv2(naiades_station_file_path, encoding = 'UTF-8') %>%
    mutate(schema = 'naiadechimie',
           projection_code = 26)
  
  result_file_path <- file.path("data", "result.csv")
  result <- read.csv2(result_file_path, encoding = 'UTF-8') %>%
    mutate(value = as.numeric(value),
           original_value = as.numeric(original_value),
           parameter_id = as.integer(parameter_id))
  
  substrate_file_path <- file.path("data", "substrate.csv")
  substrate <- read.csv2(substrate_file_path, encoding = 'UTF-8') %>%
    arrange(substrate_name)
  
  parameter_file_path <- file.path("data", "parameter.csv")
  parameter <- read.csv2(parameter_file_path, encoding = 'UTF-8') %>%
    arrange(parameter_name)
  
  unit_file_path <- file.path("data", "unit.csv")
  unit <- read.csv2(unit_file_path, encoding = 'UTF-8') %>%
    arrange(unit_name)
  
  sample_type_file_path <- file.path("data", "sample_type.csv")
  sample_type <- read.csv2(sample_type_file_path, encoding = 'UTF-8') %>%
    select(cd, mnemo) %>%
    filter(cd != '0') %>%
    rename('sample_type_id' = 'cd',
           'sample_type_name' = 'mnemo') %>%
    mutate(sample_type_id = as.integer(sample_type_id))
}


sample <- do.call("rbind", list(chimie_sample, phyto_sample, plan_deau_sample, naiades_sample)) %>%
  mutate(sampling_date = as.character(sampling_date),
         sampling_zone_sandre_code = as.integer(sampling_zone_sandre_code),
         min_sampling_depth = case_when(min_sampling_depth != '' & min_sampling_depth != '999' ~ as.numeric(min_sampling_depth),
                                        TRUE ~ as.numeric(NA)),
         max_sampling_depth = case_when(max_sampling_depth != '' & max_sampling_depth != '999' ~ as.numeric(max_sampling_depth),
                                        TRUE ~ as.numeric(NA))) %>%
  arrange(schema,sample_id)
sample$internal_sample_id <- seq.int(nrow(sample)) 

#Additional variable to use inside ggplot
sample_table <- sample

# Only selecting stations with samples
station <- do.call("rbind", list(chimie_station, phyto_station, plan_deau_station, naiades_station)) %>%
  left_join(lake,
            by="lake_id") %>%
  mutate(station_name= paste(str_replace(lake_name,"_"," "),station_name,sep=" - ")) %>%
  inner_join(sample %>%
               select(sample_id, schema, station_id),
             by=c('station_id', 'schema')) %>%
  select(-sample_id,-lake_name,-lake_code,-geom.x,-geom.y,-watershed) %>%
  distinct() %>%
  arrange(station_name)
station$internal_station_id <- seq.int(nrow(station)) 

# Only allowing to select lakes with stations in at least one of the three schemas
lake <- lake %>%
  inner_join(station %>%
               select(lake_id),
             by="lake_id") %>%
  distinct() %>%
  select(-geom,-watershed)

# Only allowing to select substrates with results in at least one of the three schemas
substrate <- substrate %>%
  inner_join(result %>%
               select(substrate_id),
             by='substrate_id') %>%
  distinct()

# Only allowing to select sample types with results in at least one of the three schemas
sample_type <- sample_type %>%
  inner_join(sample %>%
               select(sampling_zone_sandre_code) %>%
               rename('sample_type_id' = 'sampling_zone_sandre_code'),
             by='sample_type_id') %>%
  distinct()

schemas <- data.frame(c(1,2,3,4),c('chimie','phyto','plan_deau','naiadechimie'))
colnames(schemas) <- c('schema_id','schema_name')

#--------------------------FUNCTION DEFINITIONS: GETTING OPTIONS FOR SELECT_INPUTS--------------------------

get_lake_choices <- function(lakes, schema_id) {
  if (schema_id == 0) {
    lakes_by_schema <- lakes
  } else {
    lakes_by_schema <- lakes %>%
                        inner_join(station %>%
                                     select(lake_id,schema),
                                   by='lake_id') %>%
                        filter(schema == schemas[schema_id,]$schema_name) %>%
                        distinct()
  }
  
  t_lakes <- t(lakes_by_schema %>%
                           select(lake_id))
  t_lake_names <- t(lakes_by_schema %>%
                                mutate(lake_name = case_when(!is.na(lake_name) ~ lake_name,
                                                             TRUE ~ paste("NA - ",lake_id))) %>% 
                                select(lake_name))
  
  names(t_lakes) <- t_lake_names
  rownames(t_lakes) <- NULL
  
  return(t_lakes)
}

get_station_choices <- function(stations, schema_id) {
  if (schema_id == 0) {
    stations_by_schema <- stations
  } else {
    stations_by_schema <- stations %>%
      filter(schema == schemas[schema_id,]$schema_name)
  }
  
  t_stations <- t(stations_by_schema %>%
                              select(internal_station_id))
  t_station_names <- t(stations_by_schema %>%
                                   mutate(station_name = case_when(!is.na(station_name) ~ station_name,
                                                                   TRUE ~ paste("NA - ",station_id))) %>% 
                                   select(station_name))
  
  names(t_stations) <- t_station_names
  rownames(t_stations) <- NULL
  
  return(t_stations)
}

get_substrate_choices <- function(substrates, schema_id) {
  if (schema_id == 0) {
    substrates_by_schema <- substrates
  } else {
    substrates_by_schema <- substrates %>%
      inner_join(result %>%
                   select(substrate_id,schema) %>%
                   filter(schema == schemas[schema_id,]$schema_name) %>% 
                   distinct(),
                 by='substrate_id') %>%
      distinct()
  }
  
  t_substrates <- t(substrates_by_schema %>%
                                select(substrate_id) %>%
                                distinct())
  t_substrate_names <- t(substrates_by_schema %>%
                                     mutate(substrate_name = case_when(!is.na(substrate_name) ~ substrate_name,
                                                                       TRUE ~ paste("NA - ",substrate_id))) %>% 
                                     select(substrate_name))
  
  names(t_substrates) <- t_substrate_names
  rownames(t_substrates) <- NULL
  
  return(t_substrates)
}

get_sample_choices <- function(samples, schema_id) {
  if (schema_id == 0) {
    samples_by_schema <- samples
  } else {
    samples_by_schema <- samples %>%
      filter(schema == schemas[schema_id,]$schema_name)
  }
  
  t_samples <- t(samples_by_schema %>% 
                             select(internal_sample_id))
  t_sample_names <- t(samples_by_schema %>%
                                  mutate(sampling_date = case_when(sampling_date != "" ~ paste(sampling_date, " - Id: ",schema,' - ',sample_id),
                                                                   TRUE ~ paste(translator$t("Unknown date"),"- Id: ",schema,' - ',sample_id))) %>% 
                                  select(sampling_date))
  
  names(t_samples) <- t_sample_names
  rownames(t_samples) <- NULL
  
  return(t_samples)
}

get_unit_choices <- function(units, schema_id) {
  if (schema_id == 0) {
    units_by_schema <- units
  } else {
    units_by_shema <- units %>%
                        inner_join(result %>%
                                     select(unit_id,schema) %>%
                                     filter(schema == schemas[schema_id,]$schema_name) %>%
                                     distinct(),
                                   by='unit_id')
  }
  
  t_units <- t(bind_rows(data.frame(unit_id=c(0)),
                         units_by_schema %>% 
                           select(unit_id) %>%
                           filter(!is.na(unit_id))))
  t_unit_names <- t(bind_rows(data.frame(unit_name=c(translator$t("Unit"))),
                                  units_by_schema %>%
                                          select(unit_name) %>%
                                          filter(!is.na(unit_name))))
  
  names(t_units) <- t_unit_names
  rownames(t_units) <- NULL
  
  return(t_units)
}

get_parameter_choices <- function(parameters, schema_id) {
  if (schema_id == 0) {
    parameters_by_schema <- parameters
  } else {
    parameters_by_schema <- parameters %>%
      inner_join(result %>%
                   select(parameter_id,schema) %>%
                   filter(schema == schemas[schema_id,]$schema_name) %>%
                   distinct(),
                 by='parameter_id')
  }
  
  t_parameters <- t(parameters_by_schema %>% 
                                select(parameter_id) %>%
                                filter(!is.na(parameter_id)))
  t_parameter_names <- t(parameters_by_schema %>%
                                     select(parameter_name) %>%
                                     filter(!is.na(parameter_name)))
  
  names(t_parameters) <- t_parameter_names
  rownames(t_parameters) <- NULL
  
  return(t_parameters)
}

get_sample_type_choices <- function(sample_types, schema_id) {
  if (schema_id == 0) {
    sample_types_by_schema <- sample_types
  } else {
    sample_types_by_schema <- sample_types %>% 
      inner_join(sample %>%
                   filter(schema == schemas[schema_id,]$schema_name) %>%
                   select(sampling_zone_sandre_code),
                 by=c('sample_type_id' = 'sampling_zone_sandre_code')) %>%
      distinct()
  }
  
  t_sample_types <- t(sample_types_by_schema %>% 
                                  select(sample_type_id) %>%
                                  filter(!is.na(sample_type_id)))
  t_sample_type_names <- t(sample_types_by_schema %>%
                                     select(sample_type_name) %>%
                                     filter(!is.na(sample_type_name)))
  names(t_sample_types) <- t_sample_type_names
  rownames(t_sample_types) <- NULL
  
  return(t_sample_types)
}

get_data_grouping_choices <- function() {
  row_numbers <- c(0,1)
  values <- c(translator$t("Yearly averages"), translator$t("All values"))
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_number_properties_choices <- function() {
  row_numbers <- c(0,1,2,3)
  values <- c("1", "2", "3", "4")
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_plot_type_choices <- function() {
  row_numbers <- c(0,1)
  values <- c(translator$t("Scatter plot"), translator$t("Box plot"))
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_scale_choices <- function() {
  row_numbers <- c(0,1,2)
  values <- c(translator$t("Normal"), translator$t("Log 2"), translator$t("Log 10"))
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_plotted_values_choices <- function() {
  row_numbers <- c(0,1)
  values <- c(translator$t("All values"), translator$t("Values within a range"))
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_month_choices <- function() {
  row_numbers <- c(0,1,2,3,4,5,6,7,8,9,10,11)
  values <- c(translator$t("January"), translator$t("February"), translator$t("March"), translator$t("April"), translator$t("May"), translator$t("June"), translator$t("July"), translator$t("August"), translator$t("September"), translator$t("October"), translator$t("November"), translator$t("December"))
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_season_choices <- function() {
  row_numbers <- c(0,1,2,3,4)
  values <- c(translator$t("All seasons"), translator$t("Winter"), translator$t("Spring"), translator$t("Summer"), translator$t("Fall"))
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

get_schema_choices <- function() {
  row_numbers <- c(0,1,2,3,4)
  values <- c(translator$t('All schemas'),'Chimie','Phyto','Plan_deau','Naïades')
  names(row_numbers) <- values
  rownames(row_numbers) <- NULL
  
  return(row_numbers)
}

#--------------------------FUNCTION DEFINITIONS: UPDATING OPTIONS FOR SELECT_INPUTS--------------------------

update_station_choices_by_lake <- function(session, lakes, schema_id, input_label, only_geolocalized_stations) {
  station_choices <- get_station_choices(sample %>% 
                                           left_join(station,
                                                       by=c('station_id', 'schema')) %>%
                                           filter(is_empty(lakes) | lake_id %in% lakes,
                                                  !is.na(station_id),
                                                  !only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != "")) %>%
                                           select(internal_station_id,station_id,station_name,schema) %>%
                                           distinct() %>%
                                           arrange(station_name), schema_id)  
  
  updateSelectInput(session, input_label,
                    label = translator$t("Sampling station"),
                    choices = station_choices,
                    selected = NULL)
}

update_substrate_choices_by_lake_station_and_type <- function(session, lakes, stations, sample_types, schema_id, input_label, only_geolocalized_stations) {
  
  substrate_choices <- get_substrate_choices(result %>%
                                                 left_join(sample,
                                                           by=c('sample_id', 'schema')) %>% 
                                                 left_join(station,
                                                           by=c('station_id', 'schema')) %>% 
                                                 filter(is_empty(stations) | internal_station_id %in% stations,
                                                        !only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
                                                        is_empty(lakes) | lake_id %in% lakes,
                                                        is_empty(sample_types) | sampling_zone_sandre_code %in% sample_types,
                                                        !is.na(substrate_id)) %>%
                                                 left_join(substrate,
                                                           by="substrate_id") %>%
                                                 select(substrate_id,substrate_name) %>%
                                                 distinct(), schema_id)

  
  updateSelectInput(session, input_label,
                    label = translator$t("Substrate"),
                    choices = substrate_choices,
                    selected = NULL)
}

update_sample_type_choices_by_lake_and_station <- function(session, lakes, stations, schema_id, input_label, only_geolocalized_stations) {
  
  sample_type_choices <- get_sample_type_choices(sample %>% 
                                                 left_join(station,
                                                           by=c('station_id', 'schema')) %>% 
                                                 filter(is_empty(stations) | internal_station_id %in% stations,
                                                        is_empty(lakes) | lake_id %in% lakes,
                                                        !is.na(sampling_zone_sandre_code),
                                                        !only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != "")) %>%
                                                 inner_join(sample_type,
                                                           by=c('sampling_zone_sandre_code' = "sample_type_id")) %>%
                                                   rename('sample_type_id' = 'sampling_zone_sandre_code') %>%
                                                 select(sample_type_id,sample_type_name) %>%
                                                 distinct(), schema_id)
  
  updateSelectInput(session, input_label,
                    label = translator$t("Sample type"),
                    choices = sample_type_choices,
                    selected = NULL)
}

update_sample_choices_by_lake_station_and_type <- function(session, lakes, stations, sample_types, schema_id, input_label, only_geolocalized_stations) {
  
  sample_choices <- sample %>% 
                    left_join(station,
                                by=c('station_id', 'schema'))
  
  if (!is_empty(stations)) {
    sample_choices <- sample_choices %>% 
      filter(internal_station_id %in% stations)
  }  
  if (only_geolocalized_stations != 0) {
    sample_choices <- sample_choices %>% 
      filter(!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != "")
  }
  if (!is_empty(lakes)) {
    sample_choices <- sample_choices %>%
      filter(lake_id %in% lakes)
  }
  if (!is_empty(sample_types)) {
    sample_choices <- sample_choices %>%
      filter(sampling_zone_sandre_code %in% sample_types)
  }

  sample_choices <- get_sample_choices(sample_choices %>%
    select(internal_sample_id,sample_id,sampling_date,schema) %>%
      arrange(internal_sample_id), schema_id)
  
  updateSelectInput(session, input_label,
                    label = translator$t("Sample"),
                    choices = sample_choices,
                    selected = NULL)
  
}

update_unit_choices_by_sample_and_substrate <- function(session, sample__id, substrate__id, schema_id, input_label, only_geolocalized_stations) {
  
  unit_choices <- get_unit_choices(sample %>%
                                       left_join(station,
                                                 by=c('station_id', 'schema')) %>%
                                       left_join(result,
                                                 by=c('sample_id', 'schema')) %>%
                                       left_join(unit,
                                                 by="unit_id") %>%
                                       filter(sample__id == 0 | internal_sample_id == sample__id,
                                              substrate_id == 0 | substrate_id == substrate__id,
                                              !only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != "")) %>%
                                       select(unit_id,unit_name) %>%
                                       distinct(), schema_id)

  updateSelectInput(session, input_label,
                    label = translator$t("Unit of measurement"),
                    choices = unit_choices,
                    selected = NULL)
}

update_unit_choices_by_lake_station_and_substrate <- function(session, lake__id, station__id, substrate__id, schema_id, input_label, only_geolocalized_stations) {
  
  unit_choices <- sample %>%
                         left_join(station,
                                   by=c('station_id', 'schema')) %>%
                         left_join(result,
                                   by=c('sample_id', 'schema')) %>%
                         left_join(unit,
                                   by="unit_id")

  if (lake__id != 0) {
    unit_choices <- unit_choices %>%
      filter(lake_id == lake__id)
  }
  if(station__id != 0) {
    unit_choices <- unit_choices %>%
      filter(internal_station_id == station__id)
  }  
  if (only_geolocalized_stations != 0) {
    unit_choices <- unit_choices %>% 
      filter(!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != "")
  }
  if (substrate__id != 0) {
    unit_choices <- unit_choices %>%
      filter(substrate_id == substrate__id)
  }

  unit_choices <- get_unit_choices(unit_choices %>%
                                     select(unit_id,unit_name) %>%
                                              distinct(), schema_id)

  updateSelectInput(session, input_label,
                    label = translator$t("Unit of measurement"),
                    choices = unit_choices,
                    selected = NULL)
}

update_parameter_choices_by_lake_station_type_and_substrate <- function(session, lakes, stations, sample_types, substrates, schema_id, input_name, only_geolocalized_stations) {
  
  parameter_choices <- parameter %>%
                          left_join(result,
                                    by="parameter_id") %>%
                          left_join(sample,
                                   by=c('sample_id', 'schema')) %>% 
                          left_join(station,
                                    by=c('station_id', 'schema')) %>%
                          filter(!is.na(sampling_date)) 
  
  if (!is_empty(lakes)) {
    parameter_choices <- parameter_choices %>%
      filter(lake_id %in% lakes)
  } 
  if(!is_empty(stations)) {
    parameter_choices <- parameter_choices %>%
                        filter(internal_station_id %in% stations)
  }   
  if (only_geolocalized_stations != 0) {
    parameter_choices <- parameter_choices %>% 
      filter(!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != "")
  }
  if (!is_empty(sample_types)) {
    parameter_choices <- parameter_choices %>%
      filter(sampling_zone_sandre_code %in% sample_types)
  }
  if (!is_empty(substrates)) {
    parameter_choices <- parameter_choices %>%
                        filter(substrate_id %in% substrates)
  }

  parameter_choices <- get_parameter_choices(parameter_choices %>%
    select(parameter_id,parameter_name) %>%
      distinct(), schema_id)
  
  updateSelectInput(session, input_name,
                      label = translator$t("Physico-chemical properties"),
                      choices = parameter_choices,
                      selected = NULL)
}

update_lake_choices_by_schema <- function(session, schema_id, input_label) {
  
  lake_choices <- get_lake_choices(lake, schema_id)  
  
  updateSelectInput(session, input_label,
                    label = translator$t("Lake"),
                    choices = lake_choices,
                    selected = NULL)
}

#-------------------------------SETTING OPTIONS FOR SELECT_INPUTS----------------------------------

lake_choices <- get_lake_choices(lake,0)
station_choices <- get_station_choices(station,0)
substrate_choices <- get_substrate_choices(substrate,0)
sample_choices <- get_sample_choices(sample,0)
unit_choices <- get_unit_choices(unit,0)
parameter_choices <- get_parameter_choices(parameter,0)
sample_type_choices <- get_sample_type_choices(sample_type,0)
schema_choices <- get_schema_choices()

data_grouping_choices <- get_data_grouping_choices()
number_properties_choices <- get_number_properties_choices()
pt_plot_type_choices <- get_plot_type_choices()
pt_scale_choices <- get_scale_choices()
pt_plotted_values_choices <- get_plotted_values_choices()
month_choices <- get_month_choices()
season_choices <- get_season_choices()

current_sd_schema <- 0
current_pt_schema <- 0
current_csv_schema <- 0

current_length_sd_lakes <- 0
current_length_pt_lakes <- 0
current_length_csv_lakes <- 0

current_length_sd_stations <- 0
current_length_pt_stations <- 0
current_length_csv_stations <- 0

current_length_sd_sample_types <- 0
current_length_pt_sample_types <- 0
current_length_csv_sample_types <- 0

current_length_pt_substrates <- 0
current_length_csv_substrates <- 0

updateAllChoices <- function(session, lang) {
  translator$set_translation_language(lang)
  
  names_list <- c('sd_schema', 'sd_sample_types', 'pt_schema', 'pt_data_grouping', 'pt_plot_type', 'pt_scale', 'pt_plotted_values', 'pt_season', 'pt_initial_month', 'pt_final_month', 'csv_schema', 'csv_season', 'csv_initial_month', 'csv_final_month', 'csv_data_grouping')
  labels_list <- c(translator$t("Schema"), translator$t("Sample type"), translator$t("Schema"), translator$t("Plot"), translator$t("Plot type"), translator$t("Scale"), translator$t("Plotted values"), list(NULL), "", "", translator$t("Schema"), list(NULL), "", "", translator$t("Data grouping"))
  
  choices_list <- list()
  choices_list[[1]] <- get_schema_choices()
  choices_list[[2]] <- get_sample_type_choices(sample_type,0)
  choices_list[[3]] <- get_schema_choices()
  choices_list[[4]] <- get_data_grouping_choices()
  choices_list[[5]] <- get_plot_type_choices()
  choices_list[[6]] <- get_scale_choices()
  choices_list[[7]] <- get_plotted_values_choices()
  choices_list[[8]] <- get_season_choices()
  choices_list[[9]] <- get_month_choices()
  choices_list[[10]] <- get_month_choices()
  choices_list[[11]] <- get_schema_choices()
  choices_list[[12]] <- get_season_choices()
  choices_list[[13]] <- get_month_choices()
  choices_list[[14]] <- get_month_choices()
  choices_list[[15]] <- get_data_grouping_choices()

  for(i in 1:(length(names_list)-1)) {
    updateSelectInput(session, names_list[i],
                      label = labels_list[i],
                      choices = choices_list[[i]],
                      selected = NULL)
  }
  
  # Setting "All values" by default to the csv_data_grouping select input
  updateSelectInput(session, names_list[15],
                    label = labels_list[15],
                    choices = choices_list[[15]],
                    selected = 1)
}


#--------------------------------USER INTERFACE----------------------------------------

ui <- function(req) {
  translator$set_translation_language(isolate(language$x))
  
  fluidPage(
    navbarPage(
        id = 'navBar',
        title = div(
          div(
            tags$button(
              id = "info_logo",
              class = "btn action-button lang_logo",
              img(src = "images/info_logo.png",
                  height = "25px",
                  width = "25px")
            ),
            tags$button(
              id = "lang_logo_en",
              class = "btn action-button lang_logo",
              img(src = "images/en_logo.png",
                  height = "25px",
                  width = "25px")
            ),
            tags$button(
              id = "lang_logo_fr",
              class = "btn action-button lang_logo",
              img(src = "images/fr_logo.png",
                  height = "25px",
                  width = "25px")
            )
          )
        ),
        windowTitle = translator$t("Chimie database"),
        tabPanel(
          value = 'home_panel',
          title = translator$t("Home"),
          class = 'angle-bg',
          dashboardBody(
                    tags$div(class="row v-center",
                          tags$div(class="col-sm-6 home-column left-column",
                                   tags$h2(translator$t("Chimie database")),
                                   tags$p(id='app-description',
                                          translator$t("Interactive web app developed as a tool to easily access, visualize and manipulate physico-chemical data from the DYLAQ database")),
                                   tags$div(class="card my-3 shadow card-lift--hover shadow-lg--hover border-0",
                                            id="h_stations_map_card",
                                            tags$div(class="card-header",
                                                     tags$div(class="row align-items-center m-2",
                                                              tags$h5(class="text-primary text-uppercase my-0 mx-1",
                                                                      translator$t("Sampling stations map"))
                                                     ),
                                                     tags$div(class="card-body",
                                                              tags$p(class="description",
                                                                     translator$t("This module shows all sampling stations on an OpenStreetMap leaflet so that the user can easily visualize each station's precise location.")),
                                                              br(),
                                                              tags$a(img(class="img-fluid",
                                                                         src="images/stations_map_module.png",
                                                                         height = "100%",
                                                                         width = "100%")
                                                              )
                                                     )
                                            )
                                   ),
                                   tags$div(class="card my-3 shadow card-lift--hover shadow-lg--hover border-0",
                                            id="h_physicochemical_dynamics_card",
                                            tags$div(class="card-header",
                                                     tags$div(class="row align-items-center m-2",
                                                              tags$h5(class="text-primary text-uppercase my-0 mx-1",
                                                                      translator$t("Physico-chemical dynamics over time"))
                                                     ),
                                                     tags$div(class="card-body",
                                                              tags$p(class="description",
                                                                     translator$t("This module allows you to choose multiple physico-chemical properties and then generates a scatter plot with the average yearly values for the selected variables. Multiple filtering and visualization options are available.")),
                                                              br(),
                                                              tags$a(img(class="img-fluid",
                                                                         src="images/physicochemical_results_module.png",
                                                                         height = "100%",
                                                                         width = "100%")
                                                              )
                                                     )
                                            )
                                   )
                                   ),
                          tags$div(class="col-sm-6 home-column right-column",
                                   img(src = "images/inrae-vector-logo.svg",
                                       id = "inrae_logo",
                                       height = "30%",
                                       width = "30%"),
                                   img(src = "images/aeag_logo.png",
                                       id = "aeag_logo",
                                       height = "30%",
                                       width = "30%"),
                                  tags$div(class="card my-3 shadow card-lift--hover shadow-lg--hover border-0",
                                           id="h_sample_data_card",
                                           tags$div(class="card-header",
                                                    tags$div(class="row align-items-center m-2",
                                                             tags$h5(class="text-primary text-uppercase my-0 mx-1",
                                                                     translator$t("Sample data"))
                                                    ),
                                                    tags$div(class="card-body",
                                                             tags$p(class="description",
                                                                    translator$t("In this module you can select any sample found in the database, whose physico-chemical results will be shown in a data table.")),
                                                             br(),
                                                             tags$a(img(class="img-fluid",
                                                                        src="images/sample_data_module.png",
                                                                        height = "100%",
                                                                        width = "100%")
                                                             )
                                                    )
                                           )
                                  ),
                                  tags$div(class="card my-3 shadow card-lift--hover shadow-lg--hover border-0",
                                           id="h_csv_export_card",
                                           tags$div(class="card-header",
                                                    tags$div(class="row align-items-center m-2",
                                                             tags$h5(class="text-primary text-uppercase my-0 mx-1",
                                                                     translator$t("CSV export"))
                                                    ),
                                                    tags$div(class="card-body",
                                                             tags$p(class="description",
                                                                    translator$t("This module allows you to select, filter and export physico-chemical data in CSV format.")),
                                                             br(),
                                                             tags$a(img(class="img-fluid",
                                                                        src="images/csv_export_module.png",
                                                                        height = "100%",
                                                                        width = "100%")
                                                             )
                                                    )
                                           )
                                  )
                          )
                    )
          )
        ),
        tabPanel(
          value = 'sm_panel',
          title = translator$t("Sampling stations map"),
          dashboardBody(
            tags$style(type = "text/css", "#sm_map {height: calc(100vh - 80px) !important;}"),
            leafletOutput("sm_map")
          )
        ),
        tabPanel(
          value = 'sd_panel',
          title = translator$t("Sample data"),
          tags$head(
            tags$style(
              HTML("
                  @import url('css/argon.min.css');
      
                  .shiny-notification {
                    position: fixed;
                    top: 0;
                    bottom: unset;
                    left: 0;
                    right: 0;
                    margin-left: auto;
                    margin-right: auto;
                    width: 100%;
                    max-width: 250px;
                  }
                   .lang_logo{
                      position: fixed;
                      top: 10px;
                      background-color:transparent;
                      padding: 4px;
                   }
                     
                     #info_logo{
                      right: 81px;
                     }
                     
                   #lang_logo_en{
                      right: 15px;
                    } 
                   
                   #lang_logo_fr{
                      right: 48px;
                   }
                    
                  .modal-body {
                      padding-left: 8% !important;
                      padding-right: 8% !important;
                  }
                   
                   .wrapper {
                      display: flex;
                      flex-direction: column;
                    }
                   
                   #bibliography_info_NA {
                      color: #333;
                      cursor: default;
                   }
                   
                   #bibliography_info_NA:focus, #bibliography_info_NA:hover, #bibliography_info_NA:active, #bibliography_info_NA:focus.visited {
                        text-decoration: none !important;
                        outline: 0px !important;
                   }
                    
                   #sd_show_homogenized_units_icon, #pt_show_homogenized_units_icon, #csv_show_homogenized_units_icon {
                      margin-left: 3px;
                   }
                    
                    .v-center {
                        margin-top: 50px;
                    }
                    
                    .left-column h2 {
                        font-size: 40px;
                        font-family: sans-serif;
                        font-weight: bold;
                    }
                    
                    #app-description {
                        font-family: sans-serif;
                        line-height: 2;
                        margin-top: 2.5rem;
                        margin-bottom: 5.5rem;
                    }
                    
                    .home-column {
                        padding-left: 5%;
                        padding-right: 5%;
                    }
                    
                    #inrae_logo, #aeag_logo {
                        margin-top: -3%;
                        margin-bottom: 2em;
                    }
                    
                    #inrae_logo {
                        margin-left: 18%;
                    }
                    
                    #aeag_logo {
                        margin-left: 5%;
                    }
                    
                    .text-primary {
                      font-weight: 600;
                    }
                    
                    .card {
                      margin-bottom: 5% !important;
                    }
                    
                    #pt_plot_tooltip {
                      position: absolute;
                      width: 100px;
                      z-index: 100;
                      padding: 0;
                    }
                   "
              )
            )
          ),
          
          tags$script('
            $(document).ready(function() {
              // id of the plot
              $("#pt_plot").mousemove(function(e) { 
        
                // ID of uiOutput
                $("#pt_plot_tooltip").show();         
                $("#pt_plot_tooltip").css({             
                  top: (e.pageY - 60) + "px",             
                  left: (e.pageX - 20) + "px"         
                });     
              });     
            });
          '),
          
          sidebarPanel(
            selectInput('sd_schema', translator$t("Schema"), schema_choices),
            selectizeInput('sd_lakes', translator$t("Lake"), lake_choices, multiple = TRUE),
            selectizeInput('sd_stations', translator$t("Sampling station"), station_choices, multiple = TRUE),
            selectizeInput('sd_sample_types', translator$t("Sample type"), sample_type_choices, multiple = TRUE),
            selectizeInput('sd_sample', translator$t("Sample"), sample_choices, selected = NULL, options = list(maxOptions = 200, maxItems = 1L, placeholder = "", onInitialize = I('function() { this.setValue(""); }'))),
            hidden(
              uiOutput('sd_sample_info')
            ),
            hidden(
              uiOutput('sd_sample_description')
            ),
            
            div(
               id = "sd_settings_panel",
               h3(translator$t("General settings")),
               checkboxInput("sd_show_sample_info", translator$t("Show sample information"), value = FALSE),
               checkboxInput("sd_show_only_geolocalized_stations", translator$t("Restrict selection to geolocalized stations"), value = FALSE),
               checkboxInput("sd_show_homogenized_units", span(translator$t("Homogenize units of measurement"), span(id = 'sd_show_homogenized_units_icon', icon("info-circle"))), value = FALSE),
               br(),
               h3(translator$t("Exported variables")),
               div(style="display: inline-block; vertical-align:top;",
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("sd_include_schema", translator$t("Schema"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("sd_include_sample_id", translator$t("Sample id"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("sd_include_year", translator$t("Year"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("sd_include_month", translator$t("Month"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("sd_include_date", translator$t("Date"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("sd_include_lake", translator$t("Lake"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("sd_include_station", translator$t("Station"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("sd_include_substrate", translator$t("Substrate"), value = TRUE)),
                   div(style = "display: inline-block; vertical-align:top; width: 50%;", checkboxInput("sd_include_depth", translator$t("Depth"), value = TRUE))
               ),
            ),
            br(),
            actionButton(inputId = "sd_show_table", 
                         label = translator$t("Show data"),
                         style="color: #fff; background-color: #4390c4; border-color: #2e6da4"),
            br(),
            br(),
            hidden(
              downloadButton('sd_download_csv', translator$t("Download data in CSV format"))
            )
          ),
          
          mainPanel(
            actionButton(inputId = "sd_settings", 
                         label = translator$t("Settings"), 
                         icon = icon("cog", lib = "glyphicon"),
                         style = "float: right"), 
            DT::DTOutput('sd_sample_data_table')
          ),
          useShinyjs(),
          
          
          tags$head(tags$style(".shiny-notification {
                               position:fixed;
                               top: calc(10%);
                               left: calc(20%);
                             }"
          )
          )
        ),
        tabPanel(
          value = 'pt_panel',
          title = translator$t("Physico-chemical dynamics over time"),
          column(12, 
                 div(sidebarPanel(
                   selectInput('pt_schema', translator$t("Schema"), schema_choices),
                   selectizeInput('pt_lakes', translator$t("Lake"), lake_choices, multiple = TRUE),
                   selectizeInput('pt_stations', translator$t("Sampling station"), station_choices, multiple = TRUE),
                   hidden(
                     h5(id='pt_station_coord_label', translator$t("Coordinates:")),
                     uiOutput('pt_station_coordinates'),
                     h5(id='pt_station_biblio_label',translator$t("Bibliography:")),
                     uiOutput('pt_station_zotero'),
                     p(id='pt_station_line_break',br())
                   ),
                   selectizeInput('pt_sample_types', translator$t("Sample type"), sample_type_choices, multiple = TRUE),
                   selectizeInput('pt_substrates', translator$t("Substrate"), substrate_choices, multiple = TRUE),
                   selectizeInput('pt_parameters', translator$t("Physico-chemical properties"), parameter_choices, multiple = TRUE, options = list(maxItems = 4L)),
                   hidden(
                     uiOutput('pt_units1'),
                     uiOutput('pt_units2'),
                     uiOutput('pt_units3'),
                     uiOutput('pt_units4')
                   ),
                   hidden(
                     div(id='pt_units_color_legend',
                         span(style="font-weight: bold", translator$t("Substrate: ")),
                         span(style="color: #005ec8", translator$t("Water - W, ")),
                         span(style="color: #92644b", translator$t("Sediments - S, ")),
                         span(style="color: #4c824f", translator$t("Interstitial water - IW, ")),
                         span(style="color: #705ab5", translator$t("Air - A, ")),
                         span(style="color: #A72529", translator$t("Suspended matter - SM")),
                         br()
                     )
                   ),
                   br(),
                   actionButton(inputId = 'pt_show_results',
                                label = translator$t("Plot data"),
                                style="color: #fff; background-color: #4390c4; border-color: #2e6da4"),
                   br(),
                   br(),
                   hidden(
                     downloadButton('pt_download_csv', translator$t("Download data in CSV format"))
                   )
                 ),
                 mainPanel(
                   actionButton(inputId = "pt_settings", 
                                label = translator$t("Settings"), 
                                icon = icon("cog", lib = "glyphicon"),
                                style = "float: right"), 
                   br(),
                   br()
                 ),
                 sidebarPanel(style="width: 140%;",
                              id = "pt_settings_panel",
                              h3(translator$t("Plot settings")),
                              selectInput('pt_data_grouping', translator$t("Plot"), data_grouping_choices),
                              selectInput('pt_plot_type', translator$t("Plot type"), pt_plot_type_choices),
                              selectInput('pt_scale', translator$t("Scale"), pt_scale_choices),
                              selectInput('pt_plotted_values', translator$t("Plotted values"), pt_plotted_values_choices),
                              tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                                 .inline .form-group{display: table-row;}"),
                              div(class='inline',style="display: inline-block;vertical-align:top; width: 160px;",textInput("pt_min_value", div(style="font-weight: 100 !important; width: 80px;",translator$t("Min. value")), value = '')),
                              div(class='inline',style="display: inline-block;vertical-align:top; width: 160px; margin-left: 20px; margin-bottom: 20px;",textInput("pt_max_value", div(style="font-weight: 100 !important; width: 80px;",translator$t("Max. value")), value = '')),
                              div(style="display: block; margin-bottom: -10px;",tags$b(translator$t("Seasonal selection"))),
                              selectInput('pt_season', label = NULL, season_choices),
                              div(style="display: inline-block;vertical-align:top; width: 90px;",textInput("pt_initial_day", div(style="font-weight: 100 !important;",translator$t("First date")), value = '1')),
                              div(style="display: inline-block;vertical-align:top; width: 120px; margin-top: 5px;",selectInput("pt_initial_month", "", month_choices)),
                              br(),
                              div(style="display: inline-block;vertical-align:top; width: 90px; margin-top: -10px;",textInput("pt_final_day", div(style="font-weight: 100 !important;",translator$t("Last date")), value = '31')),
                              div(style="display: inline-block;vertical-align:top; width: 120px; margin-top: -5px;",selectInput("pt_final_month", "", month_choices, selected = 11)),
                              br(),
                              p(style="font-weight: 700;", translator$t("Depth selection")),
                              div(class='inline',style="display: inline-block;vertical-align:top; width: 180px;",textInput("pt_min_depth", div(style="font-weight: 100 !important; width: 100px;",translator$t("Min. depth (m)")), value = '')),
                              div(class='inline',style="display: inline-block;vertical-align:top; width: 180px; margin-left: 20px; margin-bottom: 20px;",textInput("pt_max_depth", div(style="font-weight: 100 !important; width: 100px;",translator$t("Max. depth (m)")), value = '')),
                              br(),
                              sliderInput("pt_year_range", translator$t("Year range"), min = 1963, 
                                          max = 2019, value = c(1963, 2019)),
                              checkboxInput("pt_show_regression_line", translator$t("Show regression line"), value = TRUE),
                              br(),
                              h3(translator$t("Other settings")),
                              checkboxInput("pt_show_homogenized_units", span(translator$t("Homogenize units of measurement"), span(id = 'pt_show_homogenized_units_icon', icon("info-circle"))), value = TRUE),
                              checkboxInput("pt_show_only_geolocalized_stations", translator$t("Restrict selection to geolocalized stations"), value = FALSE),
                              checkboxInput("pt_show_station_info", translator$t("Show station information"), value = FALSE)
                 )
                 ),
                 
                 plotOutput("pt_plot",
                             hover = "pt_plot_hover"),
                 uiOutput("pt_plot_tooltip")
          )
        ),
        tabPanel(
          value = 'csv_panel',
          title = translator$t("CSV export"),
          column(12, 
                 div(sidebarPanel(
                   selectInput('csv_schema', translator$t("Schema"), schema_choices),
                   selectizeInput('csv_lakes', translator$t("Lake"), lake_choices, multiple = TRUE),
                   selectizeInput('csv_stations', translator$t("Sampling station"), station_choices, multiple = TRUE),
                   hidden(
                     h5(id='csv_station_coord_label', translator$t("Coordinates:")),
                     uiOutput('csv_station_coordinates'),
                     h5(id='csv_station_biblio_label',translator$t("Bibliography:")),
                     uiOutput('csv_station_zotero'),
                     p(id='csv_station_line_break',br())
                   ),
                   selectizeInput('csv_sample_types', translator$t("Sample type"), sample_type_choices, multiple = TRUE),
                   selectizeInput('csv_substrates', translator$t("Substrate"), substrate_choices, multiple = TRUE),
                   selectizeInput('csv_parameters', translator$t("Physico-chemical properties"), parameter_choices, multiple = TRUE, options = list(maxItems = 4L)),
                   hidden(
                     uiOutput('csv_units1'),
                     uiOutput('csv_units2'),
                     uiOutput('csv_units3'),
                     uiOutput('csv_units4')
                   ),
                   hidden(
                     div(id='csv_units_color_legend',
                         span(style="font-weight: bold", translator$t("Substrate: ")),
                         span(style="color: #005ec8", translator$t("Water - W, ")),
                         span(style="color: #92644b", translator$t("Sediments - S, ")),
                         span(style="color: #4c824f", translator$t("Interstitial water - IW, ")),
                         span(style="color: #705ab5", translator$t("Air - A, ")),
                         span(style="color: #A72529", translator$t("Suspended matter - SM")),
                         br(),
                         br(),
                     )
                   ),
                   sliderInput("csv_year_range", translator$t("Year range"), min = 1963, 
                               max = 2019, value = c(1963, 2019)),
                   br(),
                   actionButton('csv_download_csv', translator$t("Download data in CSV format"), icon = icon("download")),
                   downloadButton('csv_download_btn', translator$t("Download data in CSV format"), style = "visibility: hidden;"),
                   br(),
                   br()
                 ),
                 mainPanel(
                   actionButton(inputId = "csv_settings", 
                                label = translator$t("Settings"), 
                                icon = icon("cog", lib = "glyphicon"),
                                style = "float: right"), 
                   br(),
                   br()
                 ),
                 sidebarPanel(style="width: 140%;",
                              id = "csv_settings_panel",
                              h3(translator$t("Data selection settings")),
                              selectInput('csv_data_grouping', translator$t("Data grouping"), data_grouping_choices),
                              div(style="display: block; margin-bottom: -10px;",tags$b(translator$t("Seasonal selection"))),
                              selectInput('csv_season', label = NULL, season_choices),
                              div(style="display: inline-block;vertical-align:top; width: 90px;",textInput("csv_initial_day", div(style="font-weight: 100 !important;",translator$t("First date")), value = '1')),
                              div(style="display: inline-block;vertical-align:top; width: 120px; margin-top: 5px;",selectInput("csv_initial_month", "", month_choices)),
                              br(),
                              div(style="display: inline-block;vertical-align:top; width: 90px; margin-top: -10px;",textInput("csv_final_day", div(style="font-weight: 100 !important;",translator$t("Last date")), value = '31')),
                              div(style="display: inline-block;vertical-align:top; width: 120px; margin-top: -5px;",selectInput("csv_final_month", "", month_choices, selected = 11)),
                              br(),
                              p(style="font-weight: 700;", translator$t("Depth selection")),
                              div(class='inline',style="display: inline-block;vertical-align:top; width: 180px;",textInput("csv_min_depth", div(style="font-weight: 100 !important; width: 100px;",translator$t("Min. depth (m)")), value = '')),
                              div(class='inline',style="display: inline-block;vertical-align:top; width: 180px; margin-left: 20px; margin-bottom: 20px;",textInput("csv_max_depth", div(style="font-weight: 100 !important; width: 100px;",translator$t("Max. depth (m)")), value = '')),
                              br(),
                              h3(translator$t("Exported variables")),
                              div(style="display: inline-block; vertical-align:top;",
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("csv_include_schema", translator$t("Schema"), value = FALSE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("csv_include_sample_id", translator$t("Sample id"), value = FALSE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("csv_include_year", translator$t("Year"), value = TRUE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("csv_include_month", translator$t("Month"), value = FALSE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("csv_include_date", translator$t("Date"), value = TRUE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("csv_include_lake", translator$t("Lake"), value = FALSE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px; width: 50%;", checkboxInput("csv_include_station", translator$t("Station"), value = FALSE)),
                                div(style = "display: inline-block; vertical-align:top; margin-bottom: -10px;", checkboxInput("csv_include_substrate", translator$t("Substrate"), value = FALSE)),
                                div(style = "display: inline-block; vertical-align:top; width: 50%;", checkboxInput("csv_include_depth", translator$t("Depth"), value = FALSE))
                              ),
                              br(),
                              h3(translator$t("Other settings")),
                              checkboxInput("csv_show_homogenized_units", span(translator$t("Homogenize units of measurement"), span(id = 'csv_show_homogenized_units_icon', icon("info-circle"))), value = TRUE),
                              checkboxInput("csv_show_only_geolocalized_stations", translator$t("Restrict selection to geolocalized stations"), value = FALSE),
                              checkboxInput("csv_show_station_info", translator$t("Show station information"), value = FALSE)
                 )
                 )
          )
        )
      )
  )
}



#--------------------------------------SERVER---------------------------------------

server <- function(input, output, session) {
  
  #-------------------------------------GLOBAL FUNCTIONS-----------------------------------------
  
  change_language <- function(lang) {
    translator$set_translation_language(lang)
    
    global_config_fp <- file.path("global_config.csv")
    global_config <- global_config %>%
      mutate(current_language = lang)
    write.csv2(x=global_config, na="", file=global_config_fp)
    
    session$reload()
  }
  
  observeEvent(input$lang_logo_en, {
    if (language$x == "fr") {
      language$x <- "en"
      change_language(language$x)
    } 
  })
  
  observeEvent(input$lang_logo_fr, {
    if (language$x == "en") {
      language$x <- "fr"
      change_language(language$x)
    } 
  })
  
  observeEvent(input$info_logo, {
    showModal(modalDialog(
      title = translator$t("About the app"),
      includeMarkdown(case_when(language$x == 'fr' ~ './Readme_fr.Rmd',
                                TRUE ~ './Readme_en.Rmd')),
      footer = modalButton(translator$t("Close")),
      easyClose = TRUE
    ))
  })
  
  updateSeasonalFilterDates <- function(session,season_input,initial_day_input_name,initial_month_input_name,final_day_input_name,final_month_input_name) {
    if(season_input == 1) { # Winter
      updateTextInput(session, initial_day_input_name, value = '21')
      updateSelectInput(session, initial_month_input_name, selected = 11)
      updateTextInput(session, final_day_input_name, value = '20')
      updateSelectInput(session, final_month_input_name, selected = 2)
    }
    else if(season_input == 2) { # Spring
      updateTextInput(session, initial_day_input_name, value = '21')
      updateSelectInput(session, initial_month_input_name, selected = 2)
      updateTextInput(session, final_day_input_name, value = '20')
      updateSelectInput(session, final_month_input_name, selected = 5)
    }
    else if(season_input == 3) { # Summer
      updateTextInput(session, initial_day_input_name, value = '21')
      updateSelectInput(session, initial_month_input_name, selected = 5)
      updateTextInput(session, final_day_input_name, value = '22')
      updateSelectInput(session, final_month_input_name, selected = 8)
    }
    else if(season_input == 4) { # Fall
      updateTextInput(session, initial_day_input_name, value = '23')
      updateSelectInput(session, initial_month_input_name, selected = 8)
      updateTextInput(session, final_day_input_name, value = '20')
      updateSelectInput(session, final_month_input_name, selected = 11)
    }
    else if(season_input == 0) { # All seasons
      updateTextInput(session, initial_day_input_name, value = '1')
      updateSelectInput(session, initial_month_input_name, selected = 0)
      updateTextInput(session, final_day_input_name, value = '31')
      updateSelectInput(session, final_month_input_name, selected = 11)
    }
  }
  
  showStation <- function(station_coords) {
    updateNavbarPage(session, "navBar",
                     selected = "sm_panel")
    
    focused_leaflet <- sm_leaflet %>%
      setView(station_coords[1,]$wgs84_lon, station_coords[1,]$wgs84_lat, zoom = 17)
    
    output$sm_map <- renderLeaflet({focused_leaflet})
  }
  
  show_station_info <- function(tab, chosen_input = 'station') {
    # It just describes the station when there's exactly one station selected
    if(tab == 'pt') {
      input_show_station_info <- input$pt_show_station_info
      if (length(input$pt_stations) != 1) {
        input_station <- 0
      } else {
        input_station <- input$pt_stations[1]
      }
    }
    else if(tab == 'csv') {
      input_show_station_info <- input$csv_show_station_info
      if (length(input$csv_stations) != 1) {
        input_station <- 0
      } else {
        input_station <- input$csv_stations[1]
      }
    }
    
    if(input_show_station_info & input_station != 0) {
      shinyjs::show(paste0(tab,'_station_coord_label'))
      shinyjs::show(paste0(tab,'_station_coordinates'))
      shinyjs::show(paste0(tab,'_station_line_break'))
      shinyjs::show(paste0(tab,'_station_biblio_label'))
      shinyjs::show(paste0(tab,'_station_zotero'))
      
      bibliographies_by_sample_list <- sample %>% 
        left_join(station %>%
                    select(internal_station_id,station_id,schema),
                  by=c('station_id', 'schema')) %>%
        filter(internal_station_id == input_station) %>%
        inner_join(v_bibliography,
                   by='bibliography_id') %>%
        select(bibliography_id, authors, call_number, edition, container_title, issue, title, event, accessed, issued, season, archive_location, publisher_place, archive, issn, number, volume, url, journal_abbreviation, genre, isbn, note, publisher, abstract, event_place, number_of_pages, source, type, doi, language) %>%
        distinct()
      
      output_station_zotero <- renderUI({
        bibliography_list <- lapply(1:NROW(bibliographies_by_sample_list), function(i) {
          actionLink(paste0('bibliography_info_',bibliographies_by_sample_list[i,]$bibliography_id),
                     h6(case_when(is.na(bibliographies_by_sample_list[i,]$bibliography_id) ~ translator$t("Unknown "),
                                  TRUE ~ bibliographies_by_sample_list[i,]$title)))
        })
        do.call(tagList, bibliography_list)
      })
      
      for(i in 1:nrow(bibliographies_by_sample_list)) {
        local({
          id <- paste0('bibliography_info_',bibliographies_by_sample_list[i,]$bibliography_id)
          if (id != 'bibliography_info_' & id != 'bibliography_info_NA') {
            current_bibliography <- bibliographies_by_sample_list[i,]
            shinyjs::onclick(id, showBibliographyModal(current_bibliography))
          }
        })
      }
      
      
      station_coords <- stations_with_coords %>%
        filter(internal_station_id == input_station) %>%
        select(internal_station_id,station_id,wgs84_lon, wgs84_lat)
      
      
      if(!is.na(station_coords[1,]$wgs84_lat) & !is.na(station_coords[1,]$wgs84_lon)) {
        output_station_coordinates <- renderUI({
          tagList(h6(a(paste(translator$t("Latitude: "),station_coords[1,]$wgs84_lat,translator$t(", Longitude: "), station_coords[1,]$wgs84_lon,sep=''), href='#')))
        })
        onclick(paste0(tab,"_station_coordinates"), showStation(station_coords))
      }
      else {
        output_station_coordinates <- renderUI({
          tagList(h6(paste(translator$t("Latitude: "),station_coords[1,]$wgs84_lat,translator$t(", Longitude: "), station_coords[1,]$wgs84_lon,sep='')))
        })
        onclick(paste0(tab,"_station_coordinates"), return())
      }
      
      if(tab == 'pt') {
        output$pt_station_zotero <- output_station_zotero
        output$pt_station_coordinates <- output_station_coordinates
      }
      else if(tab == 'csv') {
        output$csv_station_zotero <- output_station_zotero
        output$csv_station_coordinates <- output_station_coordinates
      }
    }
    else {
      shinyjs::hide(paste0(tab,'_station_coord_label'))
      shinyjs::hide(paste0(tab,'_station_coordinates'))
      shinyjs::hide(paste0(tab,'_station_biblio_label'))
      shinyjs::hide(paste0(tab,'_station_zotero'))
      shinyjs::hide(paste0(tab,'_station_line_break'))
    }
  }

  show_homogenized_units_info <- function() {
    delay(500, 
          showModal(modalDialog(
            title = tags$b(translator$t("Homogenize units of measurement")),
            translator$t("Units of measurement are homogenized when possible (e.g. values measured in mg(NO2)/L, mg(NO3)/L, mg(NH4)/L... are all converted to mg(N)/L) in order to facilitate comparisons between different physico-chemical properties."),
            footer = modalButton(translator$t("Close")),
            easyClose = TRUE
          ))
    ) 
  }
  
  get_results <- function(tab) {
    if (case_when(tab == 'sd' ~ !input$sd_show_homogenized_units,
                  tab == 'pt' ~ !input$pt_show_homogenized_units,
                   tab == 'csv' ~ !input$csv_show_homogenized_units,
                   TRUE ~ TRUE)) {
      return(result %>%
        select(-unit_id,-value) %>%
        rename('unit_id' = 'original_unit_id',
               'value' = 'original_value'))
    } 
    else if (tab == 'sd') {
      return(result)
    }
    else {
      return(result %>%
               select(-original_unit_id,-original_value))
    }
  }
  
  showBibliographyModal <- function(current_bibliography) {
    showModal(modalDialog(
      title = tags$b(translator$t("Bibliographic reference")),
      tags$b(translator$t('Title: ')),
      current_bibliography$title,
      br(),
      tags$b(translator$t('Authors: ')),
      current_bibliography$authors,
      br(),
      tags$b(translator$t('Publication date: ')),
      current_bibliography$issued,
      br(),
      tags$b(translator$t('Publisher: ')),
      current_bibliography$publisher,
      br(),
      tags$b(translator$t('Genre: ')),
      current_bibliography$genre,
      br(),
      tags$b(translator$t('Language: ')),
      current_bibliography$language,
      br(),
      tags$b(translator$t('Archive location: ')),
      case_when(!is.na(current_bibliography$archive) & current_bibliography$archive != '' ~  paste(current_bibliography$archive,'-',current_bibliography$archive_location),
                TRUE ~ current_bibliography$archive_location),
      br(),
      tags$b(translator$t('Number: ')),
      current_bibliography$number,
      br(),
      tags$b(translator$t('Volume: ')),
      current_bibliography$volume,
      br(),
      tags$b(translator$t('URL: ')),
      current_bibliography$url,
      br(),
      tags$b(translator$t('Note: ')),
      current_bibliography$note,
      footer = modalButton(translator$t("Close")),
      easyClose = TRUE
    ))
  }
  
  showSampleInfoModal <- function(current_sample) {
    showModal(modalDialog(
      title = tags$b(translator$t("Sample information")),
      sd_sample_description_span(current_sample),
      footer = modalButton(translator$t("Close")),
      easyClose = TRUE
    ))
  }
  
  #-----------------------------------------HOME---------------------------------------------
  
  shinyjs::onclick("h_stations_map_card",updateNavbarPage(session, "navBar",
                                                          selected = "sm_panel"))
  
  shinyjs::onclick("h_sample_data_card",updateNavbarPage(session, "navBar",
                                                          selected = "sd_panel"))
  
  shinyjs::onclick("h_physicochemical_dynamics_card",updateNavbarPage(session, "navBar",
                                                          selected = "pt_panel"))
  
  shinyjs::onclick("h_csv_export_card",updateNavbarPage(session, "navBar",
                                                          selected = "csv_panel"))
  
  #----------------------------------SAMPLE DATA: INPUT--------------------------------------
  
  sd_lake_station_inputs <- reactive({
    list(input$sd_lakes,input$sd_stations)
  })
  
  sd_lake_station_type_inputs <- reactive({
    list(input$sd_lakes,input$sd_stations,input$sd_sample_types)
  })
  
  observeEvent(input$sd_schema, {
    if (input$sd_schema != current_sd_schema) {
      update_lake_choices_by_schema(session,input$sd_schema,"sd_lakes")
      update_station_choices_by_lake(session,input$sd_lakes,input$sd_schema,"sd_stations",input$sd_show_only_geolocalized_stations)
      update_sample_type_choices_by_lake_and_station(session,input$sd_lakes,input$sd_stations,input$sd_schema,"sd_sample_types",input$sd_show_only_geolocalized_stations)
      update_sample_choices_by_lake_station_and_type(session,input$sd_lakes,input$sd_stations,input$sd_sample_types,input$sd_schema,"sd_sample",input$sd_show_only_geolocalized_stations)
      current_sd_schema <<- input$sd_schema
    }
  })
  
  observeEvent(input$sd_lakes, {
    if (length(input$sd_lakes) != current_length_sd_lakes) {
      update_station_choices_by_lake(session,input$sd_lakes,input$sd_schema,"sd_stations",input$sd_show_only_geolocalized_stations)
    }
    
    # For translation sake
    if (input$sd_schema == 0 & is_empty(input$sd_lakes) & is_empty(input$sd_stations) & is_empty(input$sd_sample_type) & input$sd_sample == '') {
      updateAllChoices(session,language$x)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$sd_stations, {
    if (length(input$sd_stations) != current_length_sd_stations) {
      update_sample_type_choices_by_lake_and_station(session,input$sd_lakes,input$sd_stations,input$sd_schema,"sd_sample_types",input$sd_show_only_geolocalized_stations)
      show_station_info('sd')
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(sd_lake_station_inputs(), {
    if (length(input$sd_lakes) != current_length_sd_lakes | length(input$sd_stations) != current_length_sd_stations) {
      update_sample_type_choices_by_lake_and_station(session,input$sd_lakes,input$sd_stations,input$sd_schema,"sd_sample_types",input$sd_show_only_geolocalized_stations)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$sd_show_only_geolocalized_stations, {
    update_station_choices_by_lake(session,input$sd_lakes,input$sd_schema,"sd_stations",input$sd_show_only_geolocalized_stations)
    shinyjs::hide("sd_download_csv")
  })
  
  observeEvent(sd_lake_station_type_inputs(), {
    if (length(input$sd_lakes) != current_length_sd_lakes | length(input$sd_stations) != current_length_sd_stations | length(input$sd_sample_types) != current_length_sd_sample_types) {
      update_sample_choices_by_lake_station_and_type(session,input$sd_lakes,input$sd_stations,input$sd_sample_types,input$sd_schema,"sd_sample",input$sd_show_only_geolocalized_stations)
      shinyjs::hide("sd_download_csv")
      current_length_sd_lakes <<- length(input$sd_lakes)
      current_length_sd_stations <<- length(input$sd_stations)
      current_length_sd_sample_types <<- length(input$sd_sample_types)
    }
  }, ignoreNULL = FALSE)
  
  sd_sample_info_inputs <- reactive({
    list(input$sd_sample,input$sd_show_sample_info)
  })
  
  observeEvent(sd_sample_info_inputs(), {
    sd_show_sample_info()
  })
  
  #----------------------------------SAMPLE DATA: OUTPUT--------------------------------------
  
  shinyjs::hide("sd_settings_panel")
  
  sd_selectedData <- eventReactive(input$sd_show_table, {
    if(!input$sd_show_homogenized_units) { 
      return(get_results('sd') %>%
               left_join(substrate,
                         by='substrate_id') %>%
               left_join(parameter,
                         by="parameter_id") %>%
               left_join(unit,
                         by="unit_id") %>%
               left_join(sample,
                         by=c('sample_id', 'schema')) %>%
               filter(internal_sample_id == input$sd_sample) %>%
               select(min_sampling_depth, max_sampling_depth,substrate_name,parameter_name,symbol,value) %>%
               arrange(parameter_name) %>%
               rename(!!translator$t("Unit") := symbol,
                      !!translator$t("Min. Depth (m)") := min_sampling_depth,
                      !!translator$t("Max. Depth (m)") := max_sampling_depth,
                      !!translator$t("Substrate") := substrate_name,
                      !!translator$t("Parameter") := parameter_name,
                      !!translator$t("Value") := value))
    }
    else {
      return(get_results('sd') %>%
               left_join(substrate,
                         by='substrate_id') %>%
               left_join(parameter,
                         by="parameter_id") %>%
               left_join(unit,
                         by="unit_id") %>%
               left_join(unit,
                         by=c("original_unit_id" = "unit_id")) %>%
               left_join(sample,
                         by=c('sample_id', 'schema')) %>%
               filter(internal_sample_id == input$sd_sample) %>%
               select(min_sampling_depth, max_sampling_depth,substrate_name,parameter_name,symbol.y,original_value,symbol.x,value) %>%
               arrange(parameter_name) %>%
               rename(!!translator$t("Converted unit") := symbol.x,
                      !!translator$t("Original unit") := symbol.y,
                      !!translator$t("Min. Depth (m)") := min_sampling_depth,
                      !!translator$t("Max. Depth (m)") := max_sampling_depth,
                      !!translator$t("Substrate") := substrate_name,
                      !!translator$t("Parameter") := parameter_name,
                      !!translator$t("Converted value") := value,
                      !!translator$t("Original value") := original_value))
    }
  })
  
  
  sd_exportData <- function() {
    if(!input$sd_show_homogenized_units) { 
      export_data <- get_results('sd') %>%
          left_join(substrate,
                    by='substrate_id') %>%
          left_join(parameter,
                    by="parameter_id") %>%
          left_join(unit,
                    by="unit_id") %>%
          left_join(sample,
                    by=c('sample_id', 'schema')) %>%
          filter(internal_sample_id == input$sd_sample) %>%
          left_join(station,
                    by=c('station_id', 'schema')) %>%
          left_join(lake,
                    by=c('lake_id')) %>%
          select(schema, sample_id, lake_name, station_name, sampling_year, sampling_month, sampling_date, min_sampling_depth, max_sampling_depth,substrate_name,parameter_name,symbol,value)
    }
    else {
      export_data <- get_results('sd') %>%
        left_join(substrate,
                  by='substrate_id') %>%
        left_join(parameter,
                  by="parameter_id") %>%
        left_join(unit,
                  by="unit_id") %>%
        left_join(unit,
                  by=c("original_unit_id" = "unit_id")) %>%
        left_join(sample,
                  by=c('sample_id', 'schema')) %>%
        filter(internal_sample_id == input$sd_sample) %>%
        left_join(station,
                  by=c('station_id', 'schema')) %>%
        left_join(lake,
                  by=c('lake_id')) %>%
        select(schema, sample_id, lake_name, station_name, sampling_year, sampling_month, sampling_date, min_sampling_depth, max_sampling_depth,substrate_name,parameter_name,symbol.y,original_value,symbol.x,value)
    }
    
    export_data <- export_data %>%
      rename(!!translator$t("Schema") := schema,
             !!translator$t("Sample id") := sample_id,
             !!translator$t("Lake") := lake_name,
             !!translator$t("Station") := station_name,
             !!translator$t("Year") := sampling_year,
             !!translator$t("Month") := sampling_month,
             !!translator$t("Date") := sampling_date,
             !!translator$t("Min. Depth (m)") := min_sampling_depth,
             !!translator$t("Max. Depth (m)") := max_sampling_depth,
             !!translator$t("Substrate") := substrate_name,
             !!translator$t("Parameter") := parameter_name)
      
    export_columns <- c()
    
    if(!input$sd_include_schema) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Schema"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Schema"))
    }
    
    if(!input$sd_include_year) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Year"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Year"))
    }
    
    if(!input$sd_include_month) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Month"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Month"))
    }
    
    if(!input$sd_include_date) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Date"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Date"))
    }
    
    if(!input$sd_include_lake) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Lake"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Lake"))
    }
    
    if(!input$sd_include_station) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Station"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Station"))
    }
    
    if(!input$sd_include_substrate) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Substrate"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Substrate"))
    }
    
    if(!input$sd_include_depth) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Min. Depth (m)"), translator$t("Max. Depth (m)"))))
    }
    else {
      export_columns <- append(export_columns, c(translator$t("Min. Depth (m)"),translator$t("Max. Depth (m)")))
    }
    
    if(!input$sd_include_sample_id) {
      export_data <- export_data %>%
        select_at(vars(-one_of(translator$t("Sample id"))))
    }
    else {
      export_columns <- append(export_columns, translator$t("Sample id"))
    }
    
    if(!input$sd_show_homogenized_units) {
      export_columns <- append(export_columns, c(translator$t("Parameter"),translator$t('Unit'),translator$t('Value')))
      export_data <- export_data %>%
        rename(!!translator$t("Unit") := symbol,
               !!translator$t("Value") := value)
    }
    else {
      export_columns <- append(export_columns, c(translator$t("Parameter"),translator$t('Original unit'),translator$t('Original value'),translator$t('Converted unit'),translator$t('Converted value')))
      export_data <- export_data %>%
        rename(!!translator$t("Converted unit") := symbol.x,
               !!translator$t("Original unit") := symbol.y,
               !!translator$t("Converted value") := value,
               !!translator$t("Original value") := original_value)
    }
    
    export_data <- export_data %>%
      select_at(export_columns)
    
    return(export_data)
  }
  
  data_table_translation_url <- reactive({
    case_when(language$x == "fr" ~ '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json',
              TRUE ~ '//cdn.datatables.net/plug-ins/1.10.11/i18n/English.json')
  })

  output$sd_sample_data_table <- renderDT(sd_selectedData(), options = list(pageLength = 100, language = list(url = data_table_translation_url())), filter = 'bottom', rownames = FALSE)
  
 observeEvent(input$sd_show_table, {
    if(nrow(sd_selectedData()) > 0) {
      shinyjs::show("sd_sample_data_table")
      shinyjs::show("sd_download_csv")
    } else {
      shinyjs::hide("sd_sample_data_table")
      showNotification(translator$t("Please select a sample"),type="error")
      shinyjs::hide("sd_download_csv")
    }
  })
  
  observeEvent(input$sd_settings, {
    if(input$sd_settings %% 2 != 0) {
      shinyjs::show("sd_settings_panel")
    } else {
      shinyjs::hide("sd_settings_panel")
    }
  })
  
  sd_sample_description_span <- function(sample_info) {
    return(
      span(
        tags$b(translator$t('Schema: ')),
        sample_info$schema,
        br(),
        tags$b(translator$t('Lake: ')),
        sample_info$lake_name,
        br(),
        tags$b(translator$t('Sampling station: ')),
        sample_info$station_name,
        br(),
        div(
          tags$b(translator$t('Coordinates: '), style = "display: inline-block;"),
          uiOutput('sd_modal_station_coordinates', style = "display: inline-block;")
        ),
        tags$b(translator$t('Sample type: ')),
        sample_info$sample_type_name,
        br(),
        tags$b(translator$t('Sample id: ')),
        sample_info$sample_id,
        br(),
        tags$b(translator$t('Sampling date: ')),
        case_when(is.na(sample_info$sampling_date) ~  paste(sample_info$sampling_month,'/',sample_info$sampling_year),
                  TRUE ~ sample_info$sampling_date),
        br(),
        tags$b(translator$t('Start time: ')),
        sample_info$start_time,
        br(),
        tags$b(translator$t('End time: ')),
        sample_info$end_time,
        br(),
        tags$b(translator$t('Minimum depth (m): ')),
        sample_info$min_sampling_depth,
        br(),
        tags$b(translator$t('Maximum depth (m): ')),
        sample_info$max_sampling_depth,
        br(),
        div(
          tags$b(translator$t('Bibliography: '), style = "display: inline-block;"),
          uiOutput('sd_modal_sample_bibliography', style = "display: inline-block;")
        )
      )
    )
  }
  
  sd_show_sample_info <- function() {
    if (length(input$sd_sample) != 1 | input$sd_sample == '') {
      selected_sample <- 0
    } else {
      selected_sample <- input$sd_sample[1]
    }
    
    if (selected_sample > 0) {
      #Sample information
      shinyjs::show('sd_sample_info')
      
      sample_info <- sample %>%
        left_join(station,
                  by=c('station_id', 'schema')) %>%
        left_join(stations_with_coords %>%
                    select(internal_station_id, wgs84_lat, wgs84_lon),
                  by='internal_station_id') %>%
        left_join(lake,
                  by='lake_id') %>%
        left_join(sample_type,
                  by=c('sampling_zone_sandre_code' = 'sample_type_id')) %>%
        filter(internal_sample_id == selected_sample) %>%
        select(schema, sample_id, sampling_date, sampling_month, sampling_year, internal_station_id, station_id, station_name, wgs84_lat, wgs84_lon, lake_name, bibliography_id, start_time, end_time, min_sampling_depth, max_sampling_depth, sample_type_name) %>% 
        replace(is.na(.), '') %>%
        mutate(bibliography_id = as.integer(bibliography_id))
      
      info_id <- paste0('sample_info_',selected_sample)
      output_sample_info <- renderUI({
          actionLink(info_id,
              span(h5(translator$t("Sample information"), HTML('&nbsp;'), icon("external-link-alt"), style = "display: inline;"))
          )
      })
      
      shinyjs::onclick(info_id, showSampleInfoModal(sample_info))
      output$sd_sample_info <- output_sample_info
      
      
      if(input$sd_show_sample_info) {
        output_sample_description <- renderUI({
          tags$div(
            br(),
            sd_sample_description_span(sample_info)
          )
        })
        output$sd_sample_description <- output_sample_description
        
        shinyjs::show('sd_sample_description')
      } 
      else {
        shinyjs::hide('sd_sample_description')
      }
      
      
      #Sampling station coordinates
      station_coords <- sample_info %>%
        select(internal_station_id, station_id, wgs84_lon, wgs84_lat)
      
      coord_id <- 'sd_modal_station_coordinates'
      shinyjs::show(coord_id)
      
      if(sample_info$wgs84_lat != '' & sample_info$wgs84_lon != '') {
        output_station_coordinates <- renderUI({
          tagList(h5(a(paste(translator$t("Latitude: "),sample_info$wgs84_lat,translator$t(", Longitude: "), sample_info$wgs84_lon,sep=''), href='#')))
        })
        onclick(coord_id, showStation(station_coords))
      }
      else {
        output_station_coordinates <- renderUI({
          tagList(h5(''))
        })
        onclick(coord_id, return())
      }
      
      output$sd_modal_station_coordinates <- output_station_coordinates
      
      #Sample bibliography
      sample_bibliography <- sample_info %>% 
        inner_join(v_bibliography,
                   by='bibliography_id') %>%
        select(bibliography_id, authors, call_number, edition, container_title, issue, title, event, accessed, issued, season, archive_location, publisher_place, archive, issn, number, volume, url, journal_abbreviation, genre, isbn, note, publisher, abstract, event_place, number_of_pages, source, type, doi, language) %>%
        distinct()
      
      biblio_id <- 'sd_modal_sample_bibliography'
      shinyjs::show(biblio_id)
      
      if (!is.na(sample_info$bibliography_id)) {
        output_sample_bibliography <- renderUI({
          tagList(h5(a(sample_bibliography$title, href='#')))
        })
        onclick(biblio_id, showBibliographyModal(sample_bibliography))
      }
      else {
        output_sample_bibliography <- renderUI({
          tagList(h5(translator$t("Unknown ")))
        })
        onclick(biblio_id, return())
      }
      
      output$sd_modal_sample_bibliography <- output_sample_bibliography
    }
    else {
      shinyjs::hide('sd_sample_info')
      shinyjs::hide('sd_sample_description')
      shinyjs::hide('sd_modal_sample_bibliography')
      shinyjs::hide('sd_modal_station_coordinates')
    }
  }
  
  output$sd_download_csv <- downloadHandler(
    filename = function() {
      gsub("/","-",gsub(">","_",gsub("<","_",gsub(":","",paste(sample %>%
                                                    filter(internal_sample_id == input$sd_sample) %>%
                                                    left_join(station,
                                                              by=c('station_id', 'schema')) %>%
                                                    left_join(lake,
                                                              by="lake_id") %>%
                                                    select(lake_name),
                                                  ", ",
                                                  sample %>%
                                                    filter(internal_sample_id == input$sd_sample) %>%
                                                    select(sampling_date),
                                                  translator$t(" (Sample id "),
                                                  sample %>%
                                                    filter(internal_sample_id == input$sd_sample) %>%
                                                    mutate(schema_plus_id = paste0(sample_id, ' - ', schema)) %>% 
                                                    select(schema_plus_id),
                                                  ").csv",
                                                  sep = "")))))
    },
    content = function(filename) {
      write.csv2(sd_exportData(), file=filename)
    }
  )
  
  onevent('mouseover','sd_show_homogenized_units_icon',{
    show_homogenized_units_info()
  }, T)
  
  #----------------------------------PHYSICO-CHEMICAL DYNAMICS: INPUT------------------------------------
  
  pt_lake_station_inputs <- reactive({
    list(input$pt_lakes,input$pt_stations)
  })
  
  pt_lake_station_type_inputs <- reactive({
    list(input$pt_lakes,input$pt_stations,input$pt_sample_types)
  })
  
  pt_lake_station_type_substrate_inputs <- reactive({
    list(input$pt_lakes,input$pt_stations,input$pt_sample_types,input$pt_substrates)
  })
  
  pt_substrate_parameter_inputs <- reactive({
    list(input$pt_substrates,input$pt_parameters)
  })
  
  pt_plot_options <- reactive({
    list(input$pt_data_grouping,input$pt_min_value,input$pt_max_value,input$pt_min_depth,input$pt_max_depth,input$pt_initial_day,input$pt_initial_month,input$pt_final_day,input$pt_final_month,input$pt_scale)
  })
  
  observeEvent(input$pt_schema, {
    if (input$pt_schema != current_pt_schema) {
      update_lake_choices_by_schema(session,input$pt_schema,"pt_lakes")
      update_station_choices_by_lake(session,input$pt_lakes,input$pt_schema,"pt_stations",input$pt_show_only_geolocalized_stations)
      update_sample_type_choices_by_lake_and_station(session,input$pt_lakes,input$pt_stations,input$pt_schema,"pt_sample_types",input$pt_show_only_geolocalized_stations)
      update_substrate_choices_by_lake_station_and_type(session,input$pt_lakes,input$pt_stations,input$pt_sample_types,input$pt_schema,"pt_substrates",input$pt_show_only_geolocalized_stations)
      update_parameter_choices_by_lake_station_type_and_substrate(session,input$pt_lakes,input$pt_stations,input$pt_sample_types,input$pt_substrates,input$pt_schema,"pt_parameters",input$pt_show_only_geolocalized_stations)
      shinyjs::hide("pt_plot")
      shinyjs::hide("pt_download_csv")
      current_pt_schema <<- input$pt_schema
    }
  })
  
  observeEvent(input$pt_lakes, {
    if (length(input$pt_lakes) != current_length_pt_lakes) {
      update_station_choices_by_lake(session,input$pt_lakes,input$pt_schema,"pt_stations",input$pt_show_only_geolocalized_stations)
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$pt_stations, {
    show_station_info('pt')
  }, ignoreNULL = FALSE)
  
  observeEvent(pt_lake_station_inputs(), {
    if (length(input$pt_lakes) != current_length_pt_lakes | length(input$pt_stations) != current_length_pt_stations) {
      update_sample_type_choices_by_lake_and_station(session,input$pt_lakes,input$pt_stations,input$pt_schema,"pt_sample_types",input$pt_show_only_geolocalized_stations)
    }
  })
  
  observeEvent(pt_lake_station_type_inputs(), {
    if (length(input$pt_lakes) != current_length_pt_lakes | length(input$pt_stations) != current_length_pt_stations | length(input$pt_sample_types) != current_length_pt_sample_types) {
      update_substrate_choices_by_lake_station_and_type(session,input$pt_lakes,input$pt_stations,input$pt_sample_types,input$pt_schema,"pt_substrates",input$pt_show_only_geolocalized_stations)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(pt_lake_station_type_substrate_inputs(), {
    if (length(input$pt_lakes) != current_length_pt_lakes | length(input$pt_stations) != current_length_pt_stations | length(input$pt_sample_types) != current_length_pt_sample_types | length(input$pt_substrates) != current_length_pt_substrates) {
      update_parameter_choices_by_lake_station_type_and_substrate(session,input$pt_lakes,input$pt_stations,input$pt_sample_types,input$pt_substrates,input$pt_schema,"pt_parameters",input$pt_show_only_geolocalized_stations)
      shinyjs::hide("pt_plot")
      shinyjs::hide("pt_download_csv")
      current_length_pt_lakes <<- length(input$pt_lakes)
      current_length_pt_stations <<- length(input$pt_stations)
      current_length_pt_sample_types <<- length(input$pt_sample_types)
      current_length_pt_substrates <<- length(input$pt_substrates)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$pt_season, {
    updateSeasonalFilterDates(session,input$pt_season,'pt_initial_day','pt_initial_month','pt_final_day','pt_final_month')
  })
  
  observeEvent(input$pt_show_only_geolocalized_stations, {
    update_station_choices_by_lake(session,input$pt_lakes,input$pt_schema,"pt_stations",input$pt_show_only_geolocalized_stations)
    shinyjs::hide("pt_plot")
    shinyjs::hide("pt_download_csv")
  })
  
  observeEvent(input$pt_plot_type, {
    if(input$pt_plot_type == 1) {
      updateSelectInput(session, 'pt_data_grouping', selected = 1)
      updateCheckboxInput(session, 'pt_show_regression_line', value = FALSE)
      
      shinyjs::disable("pt_data_grouping")
      shinyjs::disable("pt_show_regression_line")
    }
    else {
      shinyjs::enable("pt_data_grouping")
      shinyjs::enable("pt_show_regression_line")
    }
    
    if(nrow(pt_selectedData() > 0)) {
      click("pt_show_results")
    }
  })
  
  observeEvent(input$pt_plotted_values, {
    if(input$pt_plotted_values == 0) {
      shinyjs::hide("pt_min_value")
      shinyjs::hide("pt_max_value")
    }
    else {
      shinyjs::show("pt_min_value")
      shinyjs::show("pt_max_value")
    }
  })
  
  observeEvent(input$pt_show_station_info, {
    show_station_info('pt')
  })
  
  observeEvent(pt_plot_options(), {
    if(nrow(pt_selectedData() > 0)) {
      click("pt_show_results")
    }
  })

  #----------------------------------PHYSICO-CHEMICAL DYNAMICS: OUTPUT------------------------------------

  shinyjs::hide("pt_settings_panel")
  
  pt_selectedData <- eventReactive({list(input$pt_show_results,input$pt_data_grouping,input$pt_year_range,input$pt_scale,input$pt_plotted_values)}, {
    possible_units <- data.frame()
    selected_units <- data.frame(matrix(ncol = 2, nrow = 0))
    selected_results <- get_results('pt')
    
    if(!is_empty(input$pt_parameters)) {
      # Maximum different number of parameters: 4
      n_parameters <- min(4,length(input$pt_parameters))
      
      for(i in 1:n_parameters) {
        if(input[['pt_parameters']][i] != 0) {
          parameter_possible_units <- selected_results %>%
            filter(parameter_id == input[['pt_parameters']][i]) %>%
            left_join(unit,
                      by='unit_id') %>%
            left_join(sample,
                      by=c('sample_id', 'schema')) %>%
            left_join(station,
                      by=c('station_id', 'schema')) %>%
            filter(input$pt_schema == 0 | schema == schemas[input$pt_schema,]$schema_name,
                   is_empty(input$pt_substrates) | substrate_id %in% input$pt_substrates,
                   is_empty(input$pt_stations) | internal_station_id %in% input$pt_stations,
                   !input$pt_show_only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
                   is_empty(input$pt_lakes) | lake_id %in% input$pt_lakes,
                   is_empty(input$pt_sample_types) | sampling_zone_sandre_code %in% input$pt_sample_types) %>%
            select(unit_id,symbol) %>%
            rename('unit__id' = 'unit_id') %>%
            mutate(parameter_number = i) %>%
            distinct()
          
          possible_units <- rbind(possible_units, parameter_possible_units)
          
          for(j in 1:NROW(parameter_possible_units)) {
            if(input[[paste0("pt_possible_unit_", as.character(i), "_", parameter_possible_units[j,]$unit__id)]]) {
              selected_units <- rbind(selected_units, c(i, parameter_possible_units[j,]$unit__id))
            }
          }
        }
      }
    } else {
      return(data.frame())
    }
    colnames(selected_units) <- c("parameter_number", "unit__id")
      
    initial_month <- as.numeric(input$pt_initial_month)+1
    final_month <- as.numeric(input$pt_final_month)+1
    initial_day <- as.numeric(input$pt_initial_day)
    final_day <- as.numeric(input$pt_final_day)
    
    plot_data <- selected_results %>%
      left_join(sample,
                by=c('sample_id', 'schema')) %>%
      left_join(parameter,
                by="parameter_id") %>%
      left_join(unit,
                by="unit_id") %>%
      left_join(station,
                by=c('station_id', 'schema')) %>%
      filter(input$pt_schema == 0 | schema == schemas[input$pt_schema,]$schema_name,
             is_empty(input$pt_lakes) | lake_id %in% input$pt_lakes,
             is_empty(input$pt_stations) | internal_station_id %in% input$pt_stations,
             !input$pt_show_only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
             is_empty(input$pt_substrates) | substrate_id %in% input$pt_substrates,
             is_empty(input$pt_sample_types) | sampling_zone_sandre_code %in% input$pt_sample_types,
             (parameter_id == input$pt_parameters[1] & unit_id %in% (selected_units %>% filter(parameter_number == 1))$unit__id)
             | (length(input$pt_parameters) > 1 & parameter_id == input$pt_parameters[2] & unit_id %in% (selected_units %>% filter(parameter_number == 2))$unit__id)
             | (length(input$pt_parameters) > 2 & parameter_id == input$pt_parameters[3] & unit_id %in% (selected_units %>% filter(parameter_number == 3))$unit__id)
             | (length(input$pt_parameters) > 3 & parameter_id == input$pt_parameters[4] & unit_id %in% (selected_units %>% filter(parameter_number == 4))$unit__id),
             ((!grepl("\\d", input$pt_min_depth) | (min_sampling_depth < 999 & min_sampling_depth >= as.numeric(input$pt_min_depth))) & 
                 (!grepl("\\d", input$pt_max_depth) | max_sampling_depth <= as.numeric(input$pt_max_depth)))
             ) %>%
      rename('year' = 'sampling_year',
             'month' = 'sampling_month') %>%
      mutate(day = as.numeric(format(as.Date(sampling_date, format="%Y-%m-%d"),"%d"))) %>%
      mutate(parameter_name = case_when((parameter_id == input$pt_parameters[1] & unit_id %in% (selected_units %>% filter(parameter_number == 1))$unit__id) ~ paste(parameter_name,'-',symbol),
                                          (length(input$pt_parameters) > 1 & parameter_id == input$pt_parameters[2] & unit_id %in% (selected_units %>% filter(parameter_number == 2))$unit__id) ~ paste(parameter_name,'-',symbol),
                                          (length(input$pt_parameters) > 2 & parameter_id == input$pt_parameters[3] & unit_id %in% (selected_units %>% filter(parameter_number == 3))$unit__id) ~ paste(parameter_name,'-',symbol),
                                          TRUE ~ paste(parameter_name,'-',symbol))) %>%
      filter(case_when(initial_month < final_month | (initial_month == final_month & initial_day <= final_day) ~ 
                         ((month > initial_month | (month == initial_month & day >= initial_day)) & (month < final_month | (month == final_month & day <= final_day))),
                       TRUE ~ (month > initial_month | (month == initial_month & day >= initial_day) | month < final_month | (month == final_month & day <= final_day)))) %>%
      select(year,value,parameter_name,symbol) %>%
      rename('unit'='symbol') %>%
      filter(year >= input$pt_year_range[1],
             year <= input$pt_year_range[2],
             input$pt_plotted_values == 0 | 
               ((!grepl("\\d", input$pt_min_value) | value >= as.numeric(input$pt_min_value)) & 
               (!grepl("\\d", input$pt_max_value) | value <= as.numeric(input$pt_max_value)))
             ) %>%
      mutate(value = case_when(input$pt_scale == 1 ~ log2(case_when(value <= 0 ~ 0.001, # It's not possible to calculate the logarithm of 0 or a negative number
                                                                    TRUE ~ value)),
                               input$pt_scale == 2 ~ log10(case_when(value <= 0 ~ 0.001,
                                                                     TRUE ~ value)),
                               TRUE ~ value))
    
    if (input$pt_data_grouping == 0) {
      plot_data <- plot_data %>%
        group_by(year, parameter_name, unit) %>%
        summarise_at(vars(value), funs(mean(., na.rm=TRUE)))
    }
    
    return(plot_data)
  }, ignoreInit = TRUE)

  output$pt_plot <- renderPlot({
    if(nrow(pt_selectedData()) == 0) {
      return()
    }
    
    parameter_list <- pt_selectedData() %>%
      mutate(parameter = str_sub(parameter_name, end=-(nchar(unit)+4))) %>%
      ungroup() %>%
      select(parameter) %>%
      distinct()
    
    if(nrow(parameter_list) < 2) {
      plot_title <- paste(parameter_list[1,], translator$t("values over time"))
    }
    else {
      plot_title <- translator$t("Physico-chemical values over time")
    }
    
    if(input$pt_plot_type == 0) {
      plot_data <- pt_selectedData()
      
      p <- ggplot(data=plot_data, aes(x=year,y=value,color=parameter_name)) +
        geom_point() +
        expand_limits(x=c(input$pt_year_range[1],input$pt_year_range[2])) +
        scale_x_continuous(breaks = seq(input$pt_year_range[1],input$pt_year_range[2],by=2))
      
      min_value <- min(plot_data$value)
      max_value <- max(plot_data$value)
      
      if (min_value != max_value) {
        
        y_axis_break <- 0.5*10^(round(log10(max_value-min_value),0)-1)
        
        while (((max_value-min_value) / y_axis_break) > 10) {
          y_axis_break <- y_axis_break * 2
        }
        while (((max_value-min_value) / y_axis_break) < 5) {
          y_axis_break <- y_axis_break / 2
        }
        
        p <- p +
        scale_y_continuous(breaks = seq(0,(max_value %/% y_axis_break + 1)*y_axis_break,by=(y_axis_break)))
      }
      
      if(input$pt_show_regression_line) {
        p <- p + 
          geom_smooth(method=lm, se=FALSE)
      }
    }
    else {
      plot_data <- pt_selectedData() %>%
        mutate(year = as.factor(year))
      
      p <- ggplot(data=plot_data, aes(x=year,y=value,fill=parameter_name)) + 
        geom_boxplot()
    }
    
    p <- p +
      labs(title=plot_title,
           x=translator$t("Year"),
           y=translator$t("Value"),
           color=translator$t("Property"),
           fill=translator$t("Property")) +
      theme(text = element_text(size=16), axis.text.x = element_text(angle = 90, vjust=0.5),
            panel.grid.minor = element_blank())
    
    min_value <- min(plot_data$value)
    max_value <- max(plot_data$value)
    
    if (min_value != max_value) {
      
      y_axis_break <- 0.5*10^(round(log10(max_value-min_value),0)-1)
      
      while (((max_value-min_value) / y_axis_break) > 10) {
        y_axis_break <- y_axis_break * 2
      }
      while (((max_value-min_value) / y_axis_break) < 5) {
        y_axis_break <- y_axis_break / 2
      }
      
      p <- p +
        scale_y_continuous(breaks = seq(0,(max_value %/% y_axis_break + 1)*y_axis_break,by=(y_axis_break)))
    }
    
    return(p)
  })
  
  pt_plot_displayed_text <- reactive({
    req(input$pt_plot_hover)
    hover <- input$pt_plot_hover
    selected_data <- pt_selectedData()
    max_value_difference <- max(selected_data$value) - min(selected_data$value)
    max_year_difference <- input$pt_year_range[2] - input$pt_year_range[1]
    dist <- sqrt(((hover$x - selected_data$year)/max_year_difference)^2 + ((hover$y - selected_data$value)/max_value_difference)^2)

    if(min(dist) < 0.01) {
      return(selected_data$value[which.min(dist)])
    } else {
      return(NULL)
    }
  })

  output$pt_plot_hover_info <- renderText({
    req(pt_plot_displayed_text())
    pt_plot_displayed_text()
  })
  
  output$pt_plot_tooltip <- renderUI({
    req(pt_plot_displayed_text())
    verbatimTextOutput("pt_plot_hover_info")
  })
  
  

  observeEvent(input$pt_show_results, {
    if(nrow(pt_selectedData() > 0)) {
      shinyjs::show("pt_plot")
      shinyjs::show("pt_download_csv")
    } else {
      shinyjs::hide("pt_plot")
      showNotification(translator$t("No data to show. Please make sure to select a physico-chemical property and a unit of measurement and that visualization options are not too restrictive."),type="error",duration=8)
      shinyjs::hide("pt_download_csv")
    }
  })
  
  observeEvent(input$pt_settings, {
    if(input$pt_settings %% 2 != 0) {
      shinyjs::show("pt_settings_panel")
    } else {
      shinyjs::hide("pt_settings_panel")
    }
  })
  
  pt_update_possible_units <- function(parameter_number) {
    selected_results <- get_results('pt')
    
    # To avoid errors when the parameter list is emptied
    parameter_list <- case_when(!is_empty(input[['pt_parameters']]) ~ input[['pt_parameters']],
                                TRUE ~ c('0'))
    
    output[[paste0('pt_units', as.character(parameter_number))]] <- renderUI({
      possible_units <- selected_results %>%
        filter(parameter_id == parameter_list[parameter_number]) %>%
        left_join(unit,
                  by='unit_id') %>%
        left_join(sample,
                  by=c('sample_id', 'schema')) %>%
        left_join(station,
                  by=c('station_id', 'schema')) %>%
        filter(input$pt_schema == 0 | schema == schemas[input$pt_schema,]$schema_name,
               is_empty(input$pt_substrates) | substrate_id %in% input$pt_substrates,
               is_empty(input$pt_stations) | internal_station_id %in% input$pt_stations,
               !input$pt_show_only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
               is_empty(input$pt_lakes) | lake_id %in% input$pt_lakes,
               is_empty(input$pt_sample_types) | sampling_zone_sandre_code %in% input$pt_sample_types) %>%
        select(unit_id,symbol,substrate_id) %>%
        distinct() %>%
        group_by(unit_id,symbol)
      
      if(isolate(language$x) == "fr") {
        possible_units <- possible_units %>% 
          mutate(substrate_id_list = paste0(substrate_id, collapse = ","),
                 substrate_list = gsub('7','A',gsub('4','MES',gsub('3','EI',gsub('2','S',gsub('1','E',substrate_id_list))))),
                 symbol = paste0(symbol, ' - ', substrate_list))
      } 
      else {
        possible_units <- possible_units %>% 
          mutate(substrate_id_list = paste0(substrate_id, collapse = ","),
                 substrate_list = gsub('7','A',gsub('4','SM',gsub('3','IW',gsub('2','S',gsub('1','W',substrate_id_list))))),
                 symbol = paste0(symbol, ' - ', substrate_list))
      }
      
      # Units of measurement will be colored according to the substrate
      possible_units <- possible_units %>%
        mutate(color = case_when(substrate_id_list == '1' ~ '#005ec8',
                                 substrate_id_list == '2' ~ '#92644b',
                                 substrate_id_list == '3' ~ '#4c824f',
                                 substrate_id_list == '7' ~ '#705ab5',
                                 substrate_id_list == '4' ~ '#Ba282e',
                                 TRUE ~ '#2b2b2b')) %>%
        select(-substrate_id,-substrate_id_list,-substrate_list) %>%
        distinct()
      
      if(NROW(possible_units) > 0) {
        lapply(0:NROW(possible_units), function(i) {
          if(i == 0) {
            div(style="display: block; margin-top: 10px; margin-bottom: 20px;",tags$b(paste0(parameter %>%
                                                                                              filter(parameter_id == parameter_list[parameter_number]) %>%
                                                                                              select(parameter_name), ' - ', translator$t("Unit of measurement"))))
          }
          else {
            div(style=paste0("display: inline-block; margin-top: -20px; margin-bottom: 10px; vertical-align: top; color: ", possible_units[i,]$color, "; ", case_when(i %% 2 == 1 ~ "width: 50%;", TRUE ~ "")),
                checkboxInput(inputId = paste0("pt_possible_unit_", as.character(parameter_number), "_", possible_units[i,]$unit_id), label = possible_units[i,]$symbol, value = TRUE))
          }
        })
      }
    })
  }
  
  pt_parameter_or_option_change <- reactive({
    list(input$pt_parameters,input$pt_show_homogenized_units)
  })
  
  observeEvent(pt_parameter_or_option_change(), {
    # Maximum different number of parameters: 4
    n_parameters <- min(4,length(input$pt_parameters))
    
    if (is_empty(input$pt_parameters)) {
      shinyjs::hide('pt_units1')
      shinyjs::hide('pt_units2')
      shinyjs::hide('pt_units3')
      shinyjs::hide('pt_units4')
    } else {
      for (i in 1:n_parameters) {
        pt_update_possible_units(i)
      }
      if (length(input$pt_parameters) == 1) {
        shinyjs::show('pt_units1')
        shinyjs::hide('pt_units2')
        shinyjs::hide('pt_units3')
        shinyjs::hide('pt_units4')
      } else if (length(input$pt_parameters) == 2) {
        shinyjs::show('pt_units1')
        shinyjs::show('pt_units2')
        shinyjs::hide('pt_units3')
        shinyjs::hide('pt_units4')
      } else if (length(input$pt_parameters) == 3) {
        shinyjs::show('pt_units1')
        shinyjs::show('pt_units2')
        shinyjs::show('pt_units3')
        shinyjs::hide('pt_units4')
      } else if (length(input$pt_parameters) == 4) {
        shinyjs::show('pt_units1')
        shinyjs::show('pt_units2')
        shinyjs::show('pt_units3')
        shinyjs::show('pt_units4')
      }
    }
  }, ignoreInit = TRUE, ignoreNULL= FALSE)
  
  observeEvent(pt_substrate_parameter_inputs(), {
    if(length(input[['pt_substrates']]) != 1 & !is_empty(input[['pt_parameters']])) {
      shinyjs::show('pt_units_color_legend')
    }
    else {
      shinyjs::hide('pt_units_color_legend')
    }
  }, ignoreInit = TRUE, ignoreNULL= FALSE)
  
  output$pt_download_csv <- downloadHandler(
    filename = function() {
      gsub(">","_",gsub("<","_",gsub(":","",paste(
                  ifelse(length(input$pt_parameters) > 1, translator$t("Physico-chemical values"),
                   parameter %>%
                    filter(parameter_id == input$pt_parameters[1]) %>%
                    select(parameter_name)),
                  ".csv",
                  sep = ""))))
    },
    content = function(filename) {
      data_to_write <- pt_selectedData() %>%
        rowwise() %>%
        mutate(parameter_name = gsub(paste0(' - ',gsub('\\+','',
                                                       gsub('\\(','',
                                                            gsub('\\)','',unit)
                                                            )
                                                       )
                                            ), '', gsub('\\+','',
                                                        gsub('\\(','',
                                                             gsub('\\)','',parameter_name)
                                                             )
                                                        )
                                     )
               ) %>%
        rename(!!translator$t("Year") := year,
               !!translator$t("Property") := parameter_name,
               !!translator$t("Unit") := unit,
               !!translator$t("Value") := value)
      
      if(input$pt_data_grouping == 0) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Yearly average") := !!translator$t("Value"))
      }
      
      write.csv2(data_to_write, file=filename)
    }
  )
  
  onevent('mouseover','pt_show_homogenized_units_icon',{
    show_homogenized_units_info()
  }, T)

  #----------------------------------CSV EXPORT: INPUT------------------------------------
  
  csv_lake_station_inputs <- reactive({
    list(input$csv_lakes,input$csv_stations)
  })
  
  csv_lake_station_type_inputs <- reactive({
    list(input$csv_lakes,input$csv_stations,input$csv_sample_types)
  })
  
  csv_substrate_parameter_inputs <- reactive({
    list(input$csv_substrates,input$csv_parameters)
  })
  
  csv_lake_station_type_substrate_inputs <- reactive({
    list(input$csv_lakes,input$csv_stations,input$csv_sample_types,input$csv_substrates)
  })
  
  observeEvent(input$csv_schema, {
    if (input$csv_schema != current_csv_schema) {
      update_lake_choices_by_schema(session,input$csv_schema,"csv_lakes")
      update_station_choices_by_lake(session,input$csv_lakes,input$csv_schema,"csv_stations",input$csv_show_only_geolocalized_stations)
      update_sample_type_choices_by_lake_and_station(session,input$csv_lakes,input$csv_stations,input$csv_schema,"csv_sample_types",input$csv_show_only_geolocalized_stations)
      update_substrate_choices_by_lake_station_and_type(session,input$csv_lakes,input$csv_stations,input$csv_sample_types,input$csv_schema,"csv_substrates",input$csv_show_only_geolocalized_stations)
      update_parameter_choices_by_lake_station_type_and_substrate(session,input$csv_lakes,input$csv_stations,input$csv_sample_types,input$csv_substrates,input$csv_schema,"csv_parameters",input$csv_show_only_geolocalized_stations)
      current_csv_schema <<- input$csv_schema
    }
  })
  
  observeEvent(input$csv_lakes, {
    if (length(input$csv_lakes) != current_length_csv_lakes) {
      update_station_choices_by_lake(session,input$csv_lakes,input$csv_schema,"csv_stations",input$csv_show_only_geolocalized_stations)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$csv_stations, {
    show_station_info('csv')
  }, ignoreNULL = FALSE)
  
  observeEvent(csv_lake_station_inputs(), {
    if (length(input$csv_lakes) != current_length_csv_lakes | length(input$csv_stations) != current_length_csv_stations) {
      update_sample_type_choices_by_lake_and_station(session,input$csv_lakes,input$csv_stations,input$csv_schema,"csv_sample_types",input$csv_show_only_geolocalized_stations)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(csv_lake_station_type_inputs(), {
    if (length(input$csv_lakes) != current_length_csv_lakes | length(input$csv_stations) != current_length_csv_stations | length(input$csv_sample_types) != current_length_csv_sample_types) {
      update_substrate_choices_by_lake_station_and_type(session,input$csv_lakes,input$csv_stations,input$csv_sample_types,input$csv_schema,"csv_substrates",input$csv_show_only_geolocalized_stations)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(csv_lake_station_type_substrate_inputs(), {
    if (length(input$csv_lakes) != current_length_csv_lakes | length(input$csv_stations) != current_length_csv_stations | length(input$csv_sample_types) != current_length_csv_sample_types | length(input$csv_substrates) != current_length_csv_substrates) {
      update_parameter_choices_by_lake_station_type_and_substrate(session,input$csv_lakes,input$csv_stations,input$csv_sample_types,input$csv_substrates,input$csv_schema,"csv_parameters",input$csv_show_only_geolocalized_stations)
      current_length_csv_lakes <<- length(input$csv_lakes)
      current_length_csv_stations <<- length(input$csv_stations)
      current_length_csv_sample_types <<- length(input$csv_sample_types)
      current_length_csv_substrates <<- length(input$csv_substrates)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$csv_season, {
    updateSeasonalFilterDates(session,input$csv_season,'csv_initial_day','csv_initial_month','csv_final_day','csv_final_month')
  })
  
  observeEvent(input$csv_show_only_geolocalized_stations, {
    update_station_choices_by_lake(session,input$csv_lakes,input$csv_schema,"csv_stations",input$csv_show_only_geolocalized_stations)
  })
  
  observeEvent(input$csv_show_station_info, {
    show_station_info('csv')
  })
  
  observeEvent(input$csv_data_grouping, {
    if(input$csv_data_grouping == 1) {
      shinyjs::enable("csv_include_sample_id")
      shinyjs::enable("csv_include_year")
      shinyjs::enable("csv_include_month")
      shinyjs::enable("csv_include_date")
      shinyjs::enable("csv_include_depth")
    }
    else {
      updateCheckboxInput(session, 'csv_include_sample_id', value = FALSE)
      updateCheckboxInput(session, 'csv_include_year', value = TRUE)
      updateCheckboxInput(session, 'csv_include_month', value = FALSE)
      updateCheckboxInput(session, 'csv_include_date', value = FALSE)
      updateCheckboxInput(session, 'csv_include_depth', value = FALSE)
      shinyjs::disable("csv_include_sample_id")
      shinyjs::disable("csv_include_year")
      shinyjs::disable("csv_include_month")
      shinyjs::disable("csv_include_date")
      shinyjs::disable("csv_include_depth")
    }
  })
  
  #----------------------------------CSV EXPORT: OUTPUT------------------------------------
  
  shinyjs::hide("csv_settings_panel")
  
  csv_selectedData <- function() {
    possible_units <- data.frame()
    selected_units <- data.frame(matrix(ncol = 2, nrow = 0))
    selected_results <- get_results('csv')
    
    if(!is_empty(input$csv_parameters)) {
      # Maximum different number of parameters: 4
      n_parameters <- min(4,length(input$csv_parameters))
      
      for(i in 1:n_parameters) {
        if(input[['csv_parameters']][i] != 0) {
          parameter_possible_units <- selected_results %>%
            filter(parameter_id == input[['csv_parameters']][i]) %>%
            left_join(unit,
                      by='unit_id') %>%
            left_join(sample,
                      by=c('sample_id', 'schema')) %>%
            left_join(station,
                      by=c('station_id', 'schema')) %>%
            filter(input$csv_schema == 0 | schema == schemas[input$csv_schema,]$schema_name,
                   is_empty(input$csv_substrates) | substrate_id %in% input$csv_substrates,
                   is_empty(input$csv_stations) | internal_station_id %in% input$csv_stations,
                   !input$csv_show_only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
                   is_empty(input$csv_lakes) | lake_id %in% input$csv_lakes,
                   is_empty(input$csv_sample_types) | sampling_zone_sandre_code %in% input$csv_sample_types) %>%
            select(unit_id,symbol) %>%
            mutate(parameter_number = i) %>%
            distinct()
          
          possible_units <- rbind(possible_units, parameter_possible_units)
          for(j in 1:NROW(parameter_possible_units)) {
            if(input[[paste0("csv_possible_unit_", as.character(i), "_", parameter_possible_units[j,]$unit_id)]]) {
              selected_units <- rbind(selected_units, c(i, parameter_possible_units[j,]$unit_id))
            }
          }
        }
      }
    } else {
      return(data.frame())
    }
    colnames(selected_units) <- c("parameter_number", "unit_id")
    
    if(NROW(selected_units) == 0) {
      export_data <- data.frame()
    }
    else {
      initial_month <- as.numeric(input$csv_initial_month)+1
      final_month <- as.numeric(input$csv_final_month)+1
      initial_day <- as.numeric(input$csv_initial_day)
      final_day <- as.numeric(input$csv_final_day)
      
      selected_results <- get_results('csv')
      
      export_data <- selected_results %>%
        left_join(sample,
                  by=c('sample_id', 'schema')) %>%
        left_join(parameter,
                  by="parameter_id") %>%
        left_join(unit,
                  by="unit_id") %>%
        left_join(station,
                  by=c('station_id', 'schema')) %>%
        left_join(lake,
                  by="lake_id") %>%
        left_join(substrate,
                  by="substrate_id") %>%
        filter(input$csv_schema == 0 | schema == schemas[input$csv_schema,]$schema_name,
               is_empty(input$csv_lakes) | lake_id %in% input$csv_lakes,
               is_empty(input$csv_stations) | internal_station_id %in% input$csv_stations,
               !input$csv_show_only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
               is_empty(input$csv_substrates) | substrate_id %in% input$csv_substrates,
               is_empty(input$csv_sample_types) | sampling_zone_sandre_code %in% input$csv_sample_types,
               (parameter_id == input$csv_parameters[1] & unit_id %in% (selected_units %>% filter(parameter_number == 1))$unit_id)
               | (length(input$csv_parameters) > 1 & parameter_id == input$csv_parameters[2] & unit_id %in% (selected_units %>% filter(parameter_number == 2))$unit_id)
               | (length(input$csv_parameters) > 2 & parameter_id == input$csv_parameters[3] & unit_id %in% (selected_units %>% filter(parameter_number == 3))$unit_id)
               | (length(input$csv_parameters) > 3 & parameter_id == input$csv_parameters[4] & unit_id %in% (selected_units %>% filter(parameter_number == 4))$unit_id),
               ((!grepl("\\d", input$csv_min_depth) | (min_sampling_depth < 999 & min_sampling_depth >= as.numeric(input$csv_min_depth))) & 
                   (!grepl("\\d", input$csv_max_depth) | max_sampling_depth <= as.numeric(input$csv_max_depth)))
               ) %>%
        rename('year' = 'sampling_year',
               'month' = 'sampling_month') %>%
        mutate(day = as.numeric(format(as.Date(sampling_date, format="%Y-%m-%d"),"%d"))) %>%
        mutate(parameter_number = case_when((parameter_id == input$csv_parameters[1] & unit_id %in% (selected_units %>% filter(parameter_number == 1))$unit_id) ~ 1,
                                            (length(input$csv_parameters) > 1 & parameter_id == input$csv_parameters[2] & unit_id %in% (selected_units %>% filter(parameter_number == 2))$unit_id) ~ 2,
                                            (length(input$csv_parameters) > 2 & parameter_id == input$csv_parameters[3] & unit_id %in% (selected_units %>% filter(parameter_number == 3))$unit_id) ~ 3,
                                            TRUE ~ 4),
               parameter_name = paste(as.character(parameter_number),'-',parameter_name)) %>%
        filter(case_when(initial_month < final_month | (initial_month == final_month & initial_day <= final_day) ~ 
                           ((month > initial_month | (month == initial_month & day >= initial_day)) & (month < final_month | (month == final_month & day <= final_day))),
                         TRUE ~ (month > initial_month | (month == initial_month & day >= initial_day) | month < final_month | (month == final_month & day <= final_day)))) %>%
        select(schema,sample_id,lake_code,station_name,year,month,sampling_date,substrate_name,min_sampling_depth,max_sampling_depth,parameter_name,symbol,value) %>%
        rename('unit'='symbol', 'lake'='lake_code', 'parameter'='parameter_name', 'station'='station_name', 'substrate'='substrate_name') %>%
        filter(year >= input$csv_year_range[1],
               year <= input$csv_year_range[2]
               )
      
      export_columns <- c()
      
      if(!input$csv_include_schema) {
        export_data <- export_data %>%
          select(-schema)
      }
      else {
        export_columns <- append(export_columns, 'schema')
      }
      export_columns <- append(export_columns, 'year')
      if(!input$csv_include_year & input$csv_data_grouping == 1) {
        export_data <- export_data %>%
          select(-year)
      }
      if(!input$csv_include_month | input$csv_data_grouping == 0) {
        export_data <- export_data %>%
          select(-month)
      }
      else {
        export_columns <- append(export_columns, 'month')
      }
      if(!input$csv_include_date | input$csv_data_grouping == 0) {
        export_data <- export_data %>%
          select(-sampling_date)
      }
      else {
        export_columns <- append(export_columns, 'sampling_date')
      }
      if(!input$csv_include_lake) {
        export_data <- export_data %>%
          select(-lake)
      }
      else {
        export_columns <- append(export_columns, 'lake')
      }
      if(!input$csv_include_station) {
        export_data <- export_data %>%
          select(-station)
      }
      else {
        export_columns <- append(export_columns, 'station')
      }
      if(!input$csv_include_substrate) {
        export_data <- export_data %>%
          select(-substrate)
      }
      else {
        export_columns <- append(export_columns, 'substrate')
      }
      if(!input$csv_include_depth) {
        export_data <- export_data %>%
          select(-min_sampling_depth, -max_sampling_depth)
      }
      else {
        export_columns <- append(export_columns, c('min_sampling_depth','max_sampling_depth'))
      }
      if(!input$csv_include_sample_id) {
        export_data <- export_data %>%
          select(-sample_id)
      }
      else {
        export_columns <- append(export_columns, 'sample_id')
      }
      export_columns <- append(export_columns, c('parameter','unit'))
      
      export_data <- export_data %>%
        group_by_at(export_columns) %>%
        distinct()
      
      if (input$csv_data_grouping == 0) {
        export_data <- export_data %>%
          summarise_at(vars(value), funs(mean(., na.rm=TRUE)))
      }
    }
    
    return(export_data)
  }
  
  csv_update_possible_units <- function(parameter_number) {
    selected_results <- get_results('csv')
    
    # To avoid errors when the parameter list is emptied
    parameter_list <- case_when(!is_empty(input[['csv_parameters']]) ~ input[['csv_parameters']],
                                TRUE ~ c('0'))
    
    output[[paste0('csv_units', as.character(parameter_number))]] <- renderUI({
      possible_units <- selected_results %>%
        filter(parameter_id == parameter_list[parameter_number]) %>%
        left_join(unit,
                  by='unit_id') %>%
        left_join(sample,
                  by=c('sample_id', 'schema')) %>%
        left_join(station,
                  by=c('station_id', 'schema')) %>%
        filter(input$csv_schema == 0 | schema == schemas[input$csv_schema,]$schema_name,
               is_empty(input$csv_substrates) | substrate_id %in% input$csv_substrates,
               is_empty(input$csv_stations) | internal_station_id %in% input$csv_stations,
               !input$csv_show_only_geolocalized_stations | (!is.na(x_coord) & x_coord != "" & !is.na(y_coord) & y_coord != ""),
               is_empty(input$csv_lakes) | lake_id %in% input$csv_lakes,
               is_empty(input$csv_sample_types) | sampling_zone_sandre_code %in% input$csv_sample_types) %>%
        select(unit_id,symbol,substrate_id) %>%
        distinct() %>%
        group_by(unit_id,symbol)
      
      if(isolate(language$x) == "fr") {
        possible_units <- possible_units %>% 
          mutate(substrate_id_list = paste0(substrate_id, collapse = ","),
                 substrate_list = gsub('7','A',gsub('4','MS',gsub('3','EI',gsub('2','S',gsub('1','E',substrate_id_list))))),
                 symbol = paste0(symbol, ' - ', substrate_list))
      } 
      else {
        possible_units <- possible_units %>% 
          mutate(substrate_id_list = paste0(substrate_id, collapse = ","),
                 substrate_list = gsub('7','A',gsub('4','SM',gsub('3','IW',gsub('2','S',gsub('1','W',substrate_id_list))))),
                 symbol = paste0(symbol, ' - ', substrate_list))
      }
      
      # Units of measurement will be colored according to the substrate
      possible_units <- possible_units %>%
        mutate(color = case_when(substrate_id_list == '1' ~ '#005ec8',
                                 substrate_id_list == '2' ~ '#92644b',
                                 substrate_id_list == '3' ~ '#4c824f',
                                 substrate_id_list == '7' ~ '#705ab5',
                                 substrate_id_list == '4' ~ '#Ba282e',
                                 TRUE ~ '#2b2b2b')) %>%
        select(-substrate_id,-substrate_id_list,-substrate_list) %>%
        distinct()
      
      if(NROW(possible_units) > 0) {
        lapply(0:NROW(possible_units), function(i) {
          if(i == 0) {
            div(style="display: block; margin-top: 10px; margin-bottom: 20px;",tags$b(paste0(parameter %>%
                                                                                               filter(parameter_id == parameter_list[parameter_number]) %>%
                                                                                               select(parameter_name), ' - ', translator$t("Unit of measurement"))))
          }
          else {
            div(style=paste0("display: inline-block; margin-top: -20px; margin-bottom: 10px; vertical-align: top; color: ", possible_units[i,]$color, "; ", case_when(i %% 2 == 1 ~ "width: 50%;", TRUE ~ "")),
                checkboxInput(inputId = paste0("csv_possible_unit_", as.character(parameter_number), "_", possible_units[i,]$unit_id), label = possible_units[i,]$symbol, value = TRUE))
          }
        })
      }
    })
  }
  
  csv_parameter_or_option_change <- reactive({
    list(input$csv_parameters,input$csv_show_homogenized_units)
  })
  
  observeEvent(csv_parameter_or_option_change(), {
    # Maximum different number of parameters: 4
    n_parameters <- min(4,length(input$csv_parameters))
    
    if (is_empty(input$csv_parameters)) {
      shinyjs::hide('csv_units1')
      shinyjs::hide('csv_units2')
      shinyjs::hide('csv_units3')
      shinyjs::hide('csv_units4')
    } else {
      for (i in 1:n_parameters) {
        csv_update_possible_units(i)
      }
      if (length(input$csv_parameters) == 1) {
        shinyjs::show('csv_units1')
        shinyjs::hide('csv_units2')
        shinyjs::hide('csv_units3')
        shinyjs::hide('csv_units4')
      } else if (length(input$csv_parameters) == 2) {
        shinyjs::show('csv_units1')
        shinyjs::show('csv_units2')
        shinyjs::hide('csv_units3')
        shinyjs::hide('csv_units4')
      } else if (length(input$csv_parameters) == 3) {
        shinyjs::show('csv_units1')
        shinyjs::show('csv_units2')
        shinyjs::show('csv_units3')
        shinyjs::hide('csv_units4')
      } else if (length(input$csv_parameters) == 4) {
        shinyjs::show('csv_units1')
        shinyjs::show('csv_units2')
        shinyjs::show('csv_units3')
        shinyjs::show('csv_units4')
      }
    }
  }, ignoreInit = TRUE, ignoreNULL= FALSE)
  
  observeEvent(csv_substrate_parameter_inputs(), {
    if(length(input[['csv_substrates']]) != 1 & !is_empty(input[['csv_parameters']])) {
      shinyjs::show('csv_units_color_legend')
    }
    else {
      shinyjs::hide('csv_units_color_legend')
    }
  }, ignoreInit = TRUE, ignoreNULL= FALSE)
  
  observeEvent(input$csv_download_csv, {
    if(nrow(csv_selectedData()) == 0) {
      showNotification(translator$t("No data to download. Please make sure to select a physico-chemical property and a unit of measurement and that data selection settings are not too restrictive."),type="error",duration=8)
      return()
    }
    else {
      shinyjs::runjs("document.getElementById('csv_download_btn').click();")
    }
  })
  
  output$csv_download_btn <- downloadHandler(
    filename = function() {
      gsub(">","_",gsub("<","_",gsub(":","",paste(
        ifelse(length(input$csv_parameters) > 1, translator$t("Physico-chemical values"),
               parameter %>%
                 filter(parameter_id == input$csv_parameters[1]) %>%
                 select(parameter_name)),
        ".csv",
        sep = ""))))
    },
    content = function(filename) {
      data_to_write <- csv_selectedData() %>%
        mutate(parameter = gsub('1 - ', '', gsub('2 - ', '', gsub('3 - ', '', gsub('4 - ', '', parameter))))) %>%
        rename(!!translator$t("Property") := parameter,
               !!translator$t("Unit") := unit,
               !!translator$t("Value") := value)
      
      if(input$csv_data_grouping == 0) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Yearly average") := !!translator$t("Value"))
      }
      if(input$csv_include_year) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Year") := year)
      }
      if(input$csv_include_month) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Month") := month)
      }
      if(input$csv_include_date) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Date") := sampling_date)
      }
      
      if(input$csv_include_lake) {
        if(input$csv_include_station) {
          data_to_write <- data_to_write %>%
            left_join(lake,
                      by=c('lake' = 'lake_code')) %>%
            rowwise() %>%
            mutate(station = gsub(paste0(lake_name, ' - '), '', station)) %>%
            select(-lake_name,-lake_id) %>%
            rename(!!translator$t("Station") := station)
        }
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Lake") := lake)
      }
      else if(input$csv_include_station) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Station") := station)
      }
      
      if(input$csv_include_substrate) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Substrate") := substrate)
      }
      if(input$csv_include_depth) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Min. depth (m)") := min_sampling_depth,
                 !!translator$t("Max. depth (m)") := max_sampling_depth)
      }
      if(input$csv_include_schema) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Schema") := schema)
      }
      if(input$csv_include_sample_id) {
        data_to_write <- data_to_write %>%
          rename(!!translator$t("Sample id") := sample_id)
      }
      
      write.csv2(data_to_write, file=filename)
    }
  )
  
  observeEvent(input$csv_settings, {
    if(input$csv_settings %% 2 != 0) {
      shinyjs::show("csv_settings_panel")
    } else {
      shinyjs::hide("csv_settings_panel")
    }
  })
  
  onevent('mouseover','csv_show_homogenized_units_icon',{
    show_homogenized_units_info()
  }, T)
  
  
  #----------------------------------STATIONS MAP: OUTPUT------------------------------------
  
  #Transforming the coordinates from different coordinate systems to WGS 84 (projection_code 0 = unknown)
  stations_with_coords <- station %>%
    filter(!is.na(x_coord) & !is.na(y_coord) & x_coord != "" & y_coord != "" & projection_code != 0) %>%
    mutate(x_coord = as.double(as.character(x_coord)), y_coord = as.double(as.character(y_coord)))
  
  station_coordinates <- data.frame(lon=stations_with_coords$x_coord, lat=stations_with_coords$y_coord, proj=stations_with_coords$projection_code)
  
  #Lambert 93
  station_coordinates_26 <- station_coordinates %>%
    filter(proj == 26)
  coordinates(station_coordinates_26) <- c("lon", "lat")
  proj4string(station_coordinates_26) <- CRS("+init=epsg:2154")
  
  CRS.new <- CRS("+init=epsg:4326")
  new_station_coordinates_26 <- spTransform(station_coordinates_26, CRS.new)
  
  station_bibliographies <- sample %>% 
    left_join(station %>%
                select(internal_station_id,station_id,schema),
              by=c('station_id', 'schema')) %>%
    inner_join(v_bibliography,
               by='bibliography_id') %>%
    filter(!is.na(bibliography_id)) %>%
    select(internal_station_id,station_id,schema,bibliography_id,title) %>% 
    distinct() %>%
    group_by(internal_station_id,station_id,schema) %>% 
    mutate(link_list = paste0('<a id="bibliography_info_', bibliography_id, '" href="#" class="action-button shiny-bound-input" onclick = "Shiny.onInputChange(\'bibliography_link_clicked\',  ', bibliography_id, ')")>
                                                           <h6>', title, '</h6>
                                                           </a>',collapse="")) %>%
    select(internal_station_id,station_id,schema,link_list) %>%
    distinct()
  
  stations_with_coords_26 <- cbind(stations_with_coords %>%
                                     filter(projection_code == 26),new_station_coordinates_26$lon,new_station_coordinates_26$lat) %>%
    rename(wgs84_lon = "new_station_coordinates_26$lon",
           wgs84_lat = "new_station_coordinates_26$lat")
  
  stations_with_coords_31 <- stations_with_coords %>%
                                     filter(projection_code == 31) %>%
    mutate(wgs84_lon = x_coord,
           wgs84_lat = y_coord)
  
  
  stations_with_coords <- rbind(stations_with_coords_26, stations_with_coords_31) %>%
    left_join(station_bibliographies,
              by=c('internal_station_id','station_id', 'schema'))
  
  observeEvent(input$bibliography_link_clicked, {
    current_bibliography <- (v_bibliography %>%
      filter(bibliography_id == input$bibliography_link_clicked))[1,]
    
    showBibliographyModal(current_bibliography)
  })
  
  sm_leaflet <- leaflet(stations_with_coords) %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(~wgs84_lon, ~wgs84_lat, popup = ~paste0("<b>", station_name, "</b>", 
                                                       "<br> Latitude: ", wgs84_lat, 
                                                       "<br> Longitude: ", wgs84_lon, 
                                                       case_when(isolate(language$x) == "en" ~ "<br> Bibliography: <br>",
                                                                 TRUE ~ "<br> Bibliographie: <br>"), 
                                                       case_when(is.na(link_list) ~ case_when(isolate(language$x) == "en" ~ "Unknown",
                                                                                                               TRUE ~ "Inconnue"),
                                                                 TRUE ~ link_list))
               ) %>%
    addScaleBar()

  output$sm_map <- renderLeaflet({sm_leaflet})

  #-------------------------------------------------------------------------------------------
  
}

shinyApp(ui, server)
