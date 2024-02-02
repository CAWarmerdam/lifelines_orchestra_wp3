#!/usr/bin/env Rscript

## ----
## Author:  C.A. (Robert) Warmerdam
## Email:   c.a.warmerdam@umcg.nl
##
## Copyright (c) C.A. Warmerdam, 2021
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## A copy of the GNU General Public License can be found in the LICENSE file in the
## root directory of this source tree. If not, see <https://www.gnu.org/licenses/>.
## ----

# Load libraries
library("data.table")
library("argparse")
library("rjson")
library("tidyverse")
library("lubridate")
library("readxl")
library("dtplyr")

source("Snippets.R")


# Declare constants
timepoint_labels <- c(
  "covt01", "covt02", "covt03", "covt04", "covt05", "covt06", 
  "covt07", "covt08", "covt09", "covt10", "covt11", "covt12", 
  "covt13", "covt14", "covt15", "covt15b", "covt16", "covt16b", 
  "covt17", "covt18", "covt19", "covt20", "covt21", "covt22", 
  "covt23", "covt24", "covt25", "covt26", "covt27", "covt28", "covt29")


# Declare function definitions
# Function to generate mapping between timepoints and labels
#' @param label The label to be used in generating new column names.
#' @param timepoints A vector of timepoints.
#' @param new The new label for the generated column names.
#' @return A data frame mapping timepoints to specific labels.
generate_mapping <- function(label, timepoints, new) {
  return(data.frame(t = timepoints, q = paste(timepoints, label, sep="_"), qnew = new))
}

# Function to harmonize questionnaire data based on snippets
#' @param q_data The input data frame representing questionnaire data.
#' @param snippets A list of snippets containing information about columns and harmonization functions.
#' @return The harmonized data frame.
q_specific_harmonization <- function(q_data, snippets) {
  # Wrapper function to add optional columns based on conditions
  # Optional columns are removed after the function as been run.
  wrapper <- function(q_data, optional, snippet) {
    # Which columns to add?
    columns_to_add <- c()
    if (!is.null(optional) & length(optional) > 0) {
      columns_to_add <- optional[setdiff(names(optional), names(q_data))]
      q_data <- add_column(q_data, !!!columns_to_add)
    }
    # Add columns
    q_data <- snippet(q_data)
    # Remove columns that have been added
    if (length(columns_to_add) > 0) {
      q_data <- q_data %>% select(-all_of(!!!names(columns_to_add)))
    }
    return(q_data)
  }
  # Iterate through each snippet and apply harmonization
  for (snippet in snippets) {
    # are all required columns in there
    if (all(snippet$cols %in% colnames(q_data))) {
      q_data <- wrapper(q_data, snippet$optional, snippet$func)
    }
  }
  return(q_data)
}

verify_mapping <- function(q_data, named_mapping_vector) {
  # For each named
}

# Function to calculate the number of COVID infections
#' @param q_data_list A list of data frames representing questionnaire data for different timepoints.
#' @return A tibble summarizing the number of COVID infections.
number_of_covid_infections <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("infection_adu_q_1_a", timepoint_labels[1:5], "covid_infections_1_15"),
    generate_mapping("infection_adu_q_2_a", timepoint_labels[6:15], "covid_infections_1_15"),
    generate_mapping("coronatest_adu_q_1_a", timepoint_labels[16:21], "covid_infection_15b_19"),
    generate_mapping("coronatest_adu_q_1_b", timepoint_labels[16:21], "covid_infection_ggd_15b_19"),
    generate_mapping("accesstest_adu_q_1_a", timepoint_labels[22:28], "covid_accesstest_20_26"),
    generate_mapping("ggdtest_adu_q_1_a", timepoint_labels[22:31], "covid_ggdtest_20_29"),
    generate_mapping("othertest_adu_q_1_a", timepoint_labels[22:31], "covid_othertest_20_29"),
    generate_mapping("selftest_adu_q_1_a", timepoint_labels[22:31], "covid_selftest_20_29"),
    generate_mapping("worktest_adu_q_1_a", timepoint_labels[22:31], "covid_worktest_20_29"),
    generate_mapping("responsedate_adu_q_1", timepoint_labels[1:31], "responsedate"))
  
  # Return a tibble with a column for infection (y/n), 
  # a column for type (ggd/access/self/work/other/part/unknown)
   
  get_covid_infection <- list(
    list(cols = c("covid_infections_1_15"), 
         optional = c(), 
         func = .func_covid_infections_1_15),
    list(cols = c("covid_infection_15b_19", "covid_infection_ggd_15b_19"), 
         optional = c(), 
         func = .func_covid_infection_15b_19), 
    list(cols = c("covid_ggdtest_20_29", "covid_othertest_20_29", "covid_selftest_20_29", "covid_worktest_20_29"), 
         optional = c("covid_accesstest_20_26" = NA_integer_), 
         func = .func_covid_infections_20_29)
  )
  
  # Per questionnaire, replace all the columns from above.
  data_list_renamed <- mapply(function(q_data, t_id) {
    named_mapping_vector <- mapping %>% 
      filter(t == t_id) %>% select(qnew, q) %>% deframe()
    
    message(sprintf("Processing %s", t_id))
    
    return(q_specific_harmonization(
      q_data %>% rename(named_mapping_vector) %>% as_tibble(), get_covid_infection) %>% 
        select(project_pseudo_id, covid_infection, responsedate))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  # Filter rows where tested postive ()
  out_table <- bind_rows(data_list_renamed) %>%
    mutate(project_pseudo_id = factor(project_pseudo_id)) %>%
    group_by(project_pseudo_id, .drop=F) %>%
    arrange(responsedate) %>%
    mutate(
      meandate = as_date(rowMeans(cbind(lag(responsedate), responsedate), na.rm=T))) %>%
    filter(!is.na(covid_infection) & covid_infection) %>%
    mutate(new_positive_test_date = case_when(
      is.na(lag(meandate)) & !is.na(meandate) ~ T,
      meandate - lag(meandate) > 90 ~ T,
      TRUE ~ F
    )) %>%
    summarise(nbCovInfections = sum(new_positive_test_date, na.rm = T))
  
}


number_of_long_covid_symptoms <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("scl90som07_adu_q_2", timepoint_labels[31], "symptoms_breath"),
    generate_mapping("fatigue_adu_q_2_a", timepoint_labels[31], "symptoms_tired"),
    generate_mapping("symptoms_adu_q_1_s", timepoint_labels[31], "symptoms_activity"),
    generate_mapping("symptoms_adu_q_2_f", timepoint_labels[31], "tmp_dry_cough"),
    generate_mapping("symptoms_adu_q_2_g", timepoint_labels[31], "tmp_wet_cough"),
    generate_mapping("symptoms_adu_q_2_i1", timepoint_labels[31], "tmp_diarrhea"),
    generate_mapping("symptoms_adu_q_2_i2", timepoint_labels[31], "tmp_stomach_pain"),
    generate_mapping("symptoms_adu_q_1_r", timepoint_labels[31], "symptoms_palpitations"),
    generate_mapping("scl90som06_adu_q_2", timepoint_labels[31], "symptoms_joint_muscle_pain"),
    generate_mapping("scl90som09_adu_q_2", timepoint_labels[31], "symptoms_tingling"),
    generate_mapping("minia3b_adu_q_2", timepoint_labels[31], "symptoms_sleep"),
    generate_mapping("scl90som02_adu_q_2", timepoint_labels[31], "symptoms_dizziness"),
    generate_mapping("symptoms_adu_q_1_m", timepoint_labels[31], "symptoms_rash"),
    generate_mapping("symptoms_adu_q_2_j", timepoint_labels[31], "symptoms_smell_taste")
    )
  
  # Per questionnaire, replace all the columns from above.
  q_data_list <- q_data_list["covt29"]
  
  data_list_renamed <- mapply(function(q_data, t_id) {
    named_mapping_vector <- mapping %>% 
      filter(t == t_id) %>% select(q, qnew) %>% deframe()
    
    return(q_data %>% rename(named_mapping_vector))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  bind_rows(data_list_renamed) %>% 
    mutate(across(c("symptoms_breath", "symptoms_activity", 
                    "tmp_dry_cough", "tmp_wet_cough", 
                    "symptoms_palpitations", "tmp_stomach_pain",
                    "tmp_diarrhea", "symptoms_joint_muscle_pain", 
                    "symptoms_tingling",
                    "symptoms_dizziness", "symptoms_rash", 
                    "symptoms_smell_taste"), ~ .x > 2),
           across(c("symptoms_tired"), ~ .x < 4),
           across(c("symptoms_sleep"), ~ case_when(.x == 1 ~ T, .x == 2 ~ F)),
           symptom_cough = (!is.na(tmp_dry_cough) & tmp_dry_cough) 
                            | (!is.na(tmp_wet_cough) & tmp_wet_cough),
           symptoms_abdominalpain_diarrhoea = (!is.na(tmp_dry_cough) & tmp_dry_cough) 
                            | (!is.na(tmp_wet_cough) & tmp_wet_cough)) %>%
    select(-starts_with("tmp_"))
             
    
  
}


# Main

#' Execute main
#' 
#' @param argv A vector of arguments normally supplied via command-line.
main <- function(argv=NULL) {
  if (is.null(argv)) {
    argv <- commandArgs(trailingOnly = T)
  }
  # Process input
  configurationFilePath <- "config.orchestra_wp3.json"
  configuration <- fromJSON(file = configurationFilePath)
  covidResFilePerWeek <- configuration$covidResFilePerWeek
  
  # Read every questionnaire
  q_data_list <- list()
  
  for (weekLabel in names(covidResFilePerWeek)) {
    print(paste0("Loading: ", weekLabel, ", ", covidResFilePerWeek[weekLabel]))
    
    q_data_list[[weekLabel]] <- fread(
      as.character(covidResFilePerWeek[weekLabel]),
      quote="\"", na.strings = c('"$4"', '"$5"', '"$6"', '"$7"'))

  }
  
  names(q_data_list) <- timepoint_labels
  
  # 
  number_of_covid_infections_tibble <- number_of_covid_infections(q_data_list)
  number_of_long_covid_symptoms <- number_of_long_covid_symptoms(q_data_list)
  
  # Perform method
  # Process output
}

if (sys.nframe() == 0 && !interactive()) {
  main()
}