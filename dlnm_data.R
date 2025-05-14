# --- dlnm_data.R ---

source("../db_connect.R")
source("../queries/extract_procedures.R")

library(dplyr)
library(dlnm)
library(splines)
library(splines2)

# Prepare data for a single procedure
prepare_data <- function(conn, procedure_concept_id, start_date, end_date) {
  conn <- connect_db()
  procedure_data <- get_procedures(conn, procedure_concept_id, start_date, end_date)
  
  procedure_data <- procedure_data %>%
    rename(Date = date, Procedure_count = procedure_count) %>%
    mutate(Date = as.Date(Date))
  disconnect_db(conn)
  
  # Map care sites to associated city names for merging the temperature
  care_site_to_city <- c(
    "Tartu Ülikooli Kliinikum SA" = "Tartu",
    "Ida-Tallinna Keskhaigla AS" = "Tallinn",
    "Põhja-Eesti Regionaalhaigla SA" = "Tallinn",
    "Lääne-Tallinna Keskhaigla AS" = "Tallinn",
    "Tallinna Lastehaigla SA" = "Tallinn",
    "Pärnu Haigla SA" = "Parnu",
    "Läänemaa Haigla SA" = "Haapsalu",
    "Hiiumaa Haigla SA" = "Hiiumaa",
    "Ida-Viru Keskhaigla SA" = "Johvi",
    "Rakvere Haigla AS" = "Kunda_Rakvere",
    "Kuressaare Haigla SA" = "Kuressaare",
    "Narva Haigla SA" = "Narva",
    "Järvamaa Haigla AS" = "Turi",
    "Valga Haigla AS" = "Valga",
    "Viljandi Haigla SA" = "Viljandi",
    "Lõuna-Eesti Haigla AS" = "Voru",
    "Jõgeva Haigla SA" = "Jogeva",
    "Raplamaa Haigla SA" = "Rapla",
    "Rapla Maakonnahaigla SA" = "Rapla"
  )
  
  procedure_data <- procedure_data %>%
    mutate(City = care_site_to_city[care_site_name])
  
  # The same hospital, but has two names in the data over the past years
  procedure_data <- procedure_data %>%
    mutate(care_site_name = ifelse(
      care_site_name == "Rapla Maakonnahaigla SA",
      "Raplamaa Haigla SA",
      care_site_name
    ))
  
  # Load daily average temperature data per city
  temp_files <- list.files("../temperature", pattern = "_avg.csv", full.names = TRUE)
  
  temperature_data <- do.call(rbind, lapply(temp_files, function(file) {
    temp <- read.csv(file)
    city <- tools::file_path_sans_ext(basename(file)) %>% gsub("_avg", "", .)
    temp$City <- city
    
    # Apply special date parsing only for Narva and Turi
    if (city %in% c("Narva", "Turi")) {
      temp$Date <- as.Date(temp$Date, format = "%d/%m/%Y")
    } else {
      temp$Date <- as.Date(temp$Date)  # Default parsing for others
    }
    
    return(temp)
  }))
  
  
  # Convert dates to Date format
  procedure_data$Date <- as.Date(procedure_data$Date)
  temperature_data$Date <- as.Date(temperature_data$Date)
  
  # Merge procedure and temperature data by Date and City
  combined_data <- procedure_data %>%
    left_join(temperature_data, by = c("Date", "City"))
  
  
  return(combined_data)
}


# Prepare data for multiple procedures for faster heatmap calculation
prepare_all_procedure_data <- function(conn, procedure_ids, start_date, end_date) {
  if (length(procedure_ids) == 0) return(NULL)
  
  proc_string <- paste(procedure_ids, collapse = ", ")
  
  procedure_data <- get_multiple_procedures(conn, procedure_ids, start_date, end_date)
  
  procedure_data <- procedure_data %>%
    mutate(Date = as.Date(date),
           Procedure_count = procedure_count) %>%
    mutate(care_site_name = ifelse(
      care_site_name %in% c("Raplamaa Haigla SA", "Rapla Maakonnahaigla SA"),
      "Raplamaa Haigla SA", care_site_name)) %>%
    select(procedure_concept_id, Date, Procedure_count, care_site_name)
  
  # Map hospital names to city names (same mapping as in the single-procedure case)
  care_site_to_city <- c(
    "Tartu Ülikooli Kliinikum SA" = "Tartu",
    "Ida-Tallinna Keskhaigla AS" = "Tallinn",
    "Põhja-Eesti Regionaalhaigla SA" = "Tallinn",
    "Lääne-Tallinna Keskhaigla AS" = "Tallinn",
    "Tallinna Lastehaigla SA" = "Tallinn",
    "Pärnu Haigla SA" = "Parnu",
    "Läänemaa Haigla SA" = "Haapsalu",
    "Hiiumaa Haigla SA" = "Hiiumaa",
    "Ida-Viru Keskhaigla SA" = "Johvi",
    "Rakvere Haigla AS" = "Kunda_Rakvere",
    "Kuressaare Haigla SA" = "Kuressaare",
    "Narva Haigla SA" = "Narva",
    "Järvamaa Haigla AS" = "Turi",
    "Valga Haigla AS" = "Valga",
    "Viljandi Haigla SA" = "Viljandi",
    "Lõuna-Eesti Haigla AS" = "Voru",
    "Jõgeva Haigla SA" = "Jogeva",
    "Raplamaa Haigla SA" = "Rapla"
  )
  
  procedure_data$City <- care_site_to_city[procedure_data$care_site_name]
  
  # Load temperature data
  temp_files <- list.files("../temperature", pattern = "_avg.csv", full.names = TRUE)
  
  temperature_data <- do.call(rbind, lapply(temp_files, function(file) {
    temp <- read.csv(file)
    city <- tools::file_path_sans_ext(basename(file)) %>% gsub("_avg", "", .)
    temp$City <- city
    
    # Apply special date parsing only for Narva and Turi
    if (city %in% c("Narva", "Turi")) {
      temp$Date <- as.Date(temp$Date, format = "%d/%m/%Y")
    } else {
      temp$Date <- as.Date(temp$Date)  # Default parsing for others
    }
    
    return(temp)
  }))
  
  if (is.null(temperature_data) || !is.data.frame(temperature_data)) {
    stop("Failed to load temperature data.")
  }
  
  # Merge procedure and temperature data by Date and City
  combined_data <- procedure_data %>%
    left_join(temperature_data, by = c("Date", "City"))
  
  return(combined_data)
}

