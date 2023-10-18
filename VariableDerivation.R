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
    if (all(snippet$cols %in% names(q_data))) {
      q_data <- wrapper(q_data, snippet$optional, snippet$func)
    }
  }
  return(q_data)
}

# Function to calculate the number of COVID infections
#' @param q_data_list A list of data frames representing questionnaire data for different timepoints.
#' @return A tibble summarizing the number of COVID infections.
number_of_covid_infections <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("infection_adu_q_1_a", timepoint_labels[1:6], "covid_infections_1_15"),
    generate_mapping("infection_adu_q_2_a", timepoint_labels[7:15], "covid_infections_1_15"),
    generate_mapping("infection_adu_q_1_a", timepoint_labels[16:21], "covid_infection_15b_19"),
    generate_mapping("coronatest_adu_q_1_b", timepoint_labels[16:21], "covid_infection_ggd_15b_19"),
    generate_mapping("accesstest_adu_q_1_a", timepoint_labels[22:28], "covid_accesstest_20_26"),
    generate_mapping("ggdtest_adu_q_1_a", timepoint_labels[22:31], "covid_ggdtest_20_29"),
    generate_mapping("othertest_adu_q_1_a", timepoint_labels[22:31], "covid_othertest_20_29"),
    generate_mapping("selftest_adu_q_1_a", timepoint_labels[22:31], "covid_selftest_20_29"),
    generate_mapping("worktest_adu_q_1_a", timepoint_labels[22:31], "covid_worktest_20_29"))
  
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
  data_list_renamed <- mapply(function(q_data, q) {
    return(q_specific_harmonization(
      q_data %>% rename(mapping %>% select(q, qnew) %>% deframe()), get_covid_infection))
  }, q_data_list, names(data_list), SIMPLIFY=F)
  
  # Filter rows where tested postive ()
  out_table <- bind_rows(data_list_renamed) %>%
    group_by(project_pseudo_id) %>%
    arrange(responsedate) %>%
    mutate(
      meandate = mean(lag(responsedate), responsedate)) %>%
    filter(covid_infection) %>%
    mutate(positive_test_dates_filtered = accumulate(
      meandate, function(x, y) { if(y > x + 90) { return(y) } else { return(x) }}, .init = -Inf)) %>%
    summarise(nbCovInfections = sum(positive_test_dates_filtered, na.rm = T))
  
}


number_of_long_covid_symptoms <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("scl90som07_adu_q_1", timepoint_labels[1:6], "difficulty_breathing"),
    generate_mapping("scl90som07_adu_q_2", timepoint_labels[c(7:9,11:31)], "difficulty_breathing"),
    generate_mapping("fatigue_adu_q_1_a", timepoint_labels[1:6], "felt_tired"),
    generate_mapping("fatigue_adu_q_2_a", timepoint_labels[c(7:9,11:31)], "felt_tired"),
    generate_mapping("fatigue_adu_q_1_f", timepoint_labels[1:6], "dry_cough"),
    generate_mapping("fatigue_adu_q_2_f", timepoint_labels[c(7:9,11:31)], "dry_cough"),
    generate_mapping("fatigue_adu_q_1_g", timepoint_labels[1:6], "wet_cough"),
    generate_mapping("fatigue_adu_q_2_g", timepoint_labels[c(7:9,11:31)], "wet_cough"),
    generate_mapping("symptoms_adu_q_1_i2", timepoint_labels[1:6], "stomach_pain"),
    generate_mapping("symptoms_adu_q_2_i2", timepoint_labels[c(7:9, 11:31)], "stomach_pain"),
    generate_mapping("symptoms_adu_q_1_i2", timepoint_labels[1:6], "stomach_pain"),
    generate_mapping("symptoms_adu_q_2_i2", timepoint_labels[c(7:9, 11:31)], "stomach_pain")
    )
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
  
  number_of_covid_infections_tibble <- number_of_covid_infections(q_data_list)
  number_of_long_covid_symptoms <- number_of_long_covid_symptoms(q_data_list)
  
  # Perform method
  # Process output
}

if (sys.nframe() == 0 && !interactive()) {
  main()
}