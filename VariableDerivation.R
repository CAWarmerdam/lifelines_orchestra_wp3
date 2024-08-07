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
      q_data <- q_data %>% dplyr::select(-all_of(!!!names(columns_to_add)))
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
      filter(t == t_id) %>% dplyr::select(qnew, q) %>% deframe()
    
    message(sprintf("Processing %s", t_id))
    
    return(q_specific_harmonization(
      q_data %>% rename(named_mapping_vector) %>% as_tibble(), get_covid_infection) %>% 
        dplyr::select(project_pseudo_id, covid_infection, responsedate))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  # Filter rows where tested postive ()
  out_table <- bind_rows(data_list_renamed) %>%
    mutate(project_pseudo_id = factor(project_pseudo_id)) %>%
    group_by(project_pseudo_id, .drop=F) %>%
    arrange(responsedate) %>%
    mutate(
      meandate = as_date(rowMeans(cbind(lag(responsedate), responsedate), na.rm=T)),
      covid_row = !is.na(covid_infection) & covid_infection,
      any_covid_rows = any(covid_row)) %>%
    group_by(project_pseudo_id, covid_row, .drop=F) %>%
    mutate(new_positive_test_date = case_when(
      covid_row & is.na(lag(meandate)) & !is.na(meandate) ~ T,
      covid_row & meandate - lag(meandate) > 90 ~ T,
      TRUE ~ F
    )) %>%
    ungroup(covid_row) %>%
    summarise(nbCovInfections = sum(new_positive_test_date, na.rm = T), CovInfectionDates = list(meandate[covid_row]),
              AGGREGATED_1280 = nbCovInfections > 0)
  
  return(out_table)
}


number_of_long_covid_symptoms <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("scl90som07_adu_q_1", timepoint_labels[1:6], "SCT_267036007_"),
    generate_mapping("scl90som07_adu_q_2", timepoint_labels[c(7:9,11:31)], "SCT_267036007_"),
    generate_mapping("fatigue_adu_q_1_a", timepoint_labels[1:6], "SCT_84229001_367391008_"),
    generate_mapping("fatigue_adu_q_2_a", timepoint_labels[c(7:9,11:31)], "SCT_84229001_367391008_"),
    generate_mapping("scl90som01_adu_q_1", timepoint_labels[1:6], "SCT_25064002_"),
    generate_mapping("scl90som01_adu_q_2", timepoint_labels[c(7:9,11:31)], "SCT_25064002_"),
    generate_mapping("symptoms_adu_q_1_s", timepoint_labels[27:31], "LN_75325_1_WORSENING"),
    generate_mapping("symptoms_adu_q_1_f", timepoint_labels[1:6], "tmp_dry_cough"),
    generate_mapping("symptoms_adu_q_2_f", timepoint_labels[c(7:9,11:31)], "tmp_dry_cough"),
    generate_mapping("symptoms_adu_q_1_g", timepoint_labels[1:6], "tmp_wet_cough"),
    generate_mapping("symptoms_adu_q_2_g", timepoint_labels[c(7:9,11:31)], "tmp_wet_cough"),
    generate_mapping("symptoms_adu_q_1_i", timepoint_labels[1:2], "tmp_diarrhea"),
    generate_mapping("symptoms_adu_q_1_i1", timepoint_labels[3:6], "tmp_diarrhea"),
    generate_mapping("symptoms_adu_q_2_i1", timepoint_labels[c(7:9,11:31)], "tmp_diarrhea"),
    generate_mapping("symptoms_adu_q_1_i2", timepoint_labels[3:6], "tmp_stomach_pain"),
    generate_mapping("symptoms_adu_q_2_i2", timepoint_labels[c(7:9,11:31)], "tmp_stomach_pain"),
    generate_mapping("symptoms_adu_q_1_r", timepoint_labels[23:31], "SCT_248657009"),
    generate_mapping("scl90som06_adu_q_1", timepoint_labels[1:6], "SCT_57676002_"),
    generate_mapping("scl90som06_adu_q_2", timepoint_labels[c(7:9,11:31)], "SCT_57676002_"),
    generate_mapping("scl90som09_adu_q_1", timepoint_labels[1:6], "SCT_62507009"),
    generate_mapping("scl90som09_adu_q_2", timepoint_labels[c(7:9,11:31)], "SCT_62507009"),
    generate_mapping("minia3b_adu_q_1", timepoint_labels[1:6], "SCT_106168000"),
    generate_mapping("minia3b_adu_q_2", timepoint_labels[c(7:11,13:31)], "SCT_106168000"),
    generate_mapping("scl90som02_adu_q_1", timepoint_labels[1:6], "SCT_404640003"),
    generate_mapping("scl90som02_adu_q_2", timepoint_labels[c(7:9,11:31)], "SCT_404640003"),
    generate_mapping("symptoms_adu_q_1_m", timepoint_labels[c(7:9,11:31)], "SCT_271807003_"),
    generate_mapping("symptoms_adu_q_1_j", timepoint_labels[1:6], "SCT_44169009_"),
    generate_mapping("symptoms_adu_q_2_j", timepoint_labels[c(7:9,11:31)], "SCT_44169009_"),
    generate_mapping("responsedate_adu_q_1", timepoint_labels[1:31], "responsedate")
    )
  
  q_data_list <- q_data_list["covt29"]

  # Per questionnaire, replace all the columns from above.
  data_list_renamed <- mapply(function(q_data, t_id) {
    named_mapping_vector <- mapping %>% 
      filter(t == t_id) %>% select(qnew, q) %>% deframe()
    
    message(sprintf("Processing %s", t_id))
    
    return(q_data %>% rename(named_mapping_vector) %>% as_tibble() %>%
             select(all_of(names(named_mapping_vector)), project_pseudo_id, responsedate))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  out_table <- bind_rows(data_list_renamed) %>% 
    mutate(across(c("SCT_267036007_", "LN_75325_1_WORSENING",
                    "tmp_dry_cough", "tmp_wet_cough", 
                    "SCT_248657009", "tmp_stomach_pain",
                    "tmp_diarrhea", "SCT_57676002_", 
                    "SCT_62507009", "SCT_25064002_",
                    "SCT_404640003", "SCT_271807003_", 
                    "SCT_44169009_"), ~ .x > 2),
           across(c("SCT_84229001_367391008_"), ~ .x < 4),
           across(c("SCT_106168000"), ~ case_when(.x == 1 ~ T, .x == 2 ~ F)),
           SCT_49727002_ = (!is.na(tmp_dry_cough) & tmp_dry_cough) 
                            | (!is.na(tmp_wet_cough) & tmp_wet_cough),
           SCT_62315008_ = (!is.na(tmp_diarrhea) & tmp_diarrhea) 
                            | (!is.na(tmp_stomach_pain) & tmp_stomach_pain)) %>%
    select(-starts_with("tmp_"), -responsedate)
  
  return(out_table)
}


loneliness_scale <- function(q_data_list) {
    mapping <- bind_rows(
      generate_mapping("isolation_adu_q_2_c", timepoint_labels[31], "isolation_adu_q_2_c"),
      generate_mapping("isolation_adu_q_2_a", timepoint_labels[31], "isolation_adu_q_2_a"),
      generate_mapping("isolation_adu_q_2_b", timepoint_labels[31], "isolation_adu_q_2_b"),
    )
    
    # Per questionnaire, replace all the columns from above.
    q_data_list <- q_data_list["covt29"]
    
    data_list_renamed <- mapply(function(q_data, t_id) {
      named_mapping_vector <- mapping %>% 
        filter(t == t_id) %>% select(qnew, q) %>% deframe()
      
      message(sprintf("Processing %s", t_id))
      
      return(q_data %>% rename(named_mapping_vector) %>% as_tibble() %>% 
               select(all_of(names(named_mapping_vector)), project_pseudo_id))
    }, q_data_list, names(q_data_list), SIMPLIFY=F)
    
    out_table <- bind_rows(data_list_renamed) %>% 
      mutate(UCLA_LONELINESS_SCALE_UCLA = rowSums(across(starts_with("isolation")))) %>%
      select(project_pseudo_id, UCLA_LONELINESS_SCALE_UCLA, starts_with("isolation"))
    
    return(out_table)
}


sleep_quality <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("sleeptimes_adu_q_1_d", timepoint_labels[30], "SLEEP_ACTUALHOURS"),
    generate_mapping("sleeptimes_adu_q_1_c", timepoint_labels[30], "sleeptimes_end"),
    generate_mapping("sleeptimes_adu_q_1_a", timepoint_labels[30], "sleeptimes_start"),
    generate_mapping("sleeptimes_adu_q_1_b", timepoint_labels[30], "SLEEP_TIMETOSLEEP"),
    generate_mapping("sleepquality_adu_q_1_a", timepoint_labels[30], "SLEEP_PROBLEMS_TIME"),
    generate_mapping("sleepquality_adu_q_1_b", timepoint_labels[30], "SLEEP_PROBLEMS_WAKINGUP"),
    generate_mapping("sleepquality_adu_q_1_c", timepoint_labels[30], "SLEEP_PROBLEMS_BREATHING"),
    generate_mapping("sleepquality_adu_q_1_d", timepoint_labels[30], "SLEEP_PROBLEMS_COUGH_SNORE"),
    generate_mapping("sleepquality_adu_q_1_e", timepoint_labels[30], "SLEEP_PROBLEMS_FEELING_HOT"),
    generate_mapping("sleepquality_adu_q_1_f", timepoint_labels[30], "SLEEP_PROBLEMS_BAD_DREAMS"),
    generate_mapping("sleepquality_adu_q_1_g", timepoint_labels[30], "SLEEP_PROBLEMS_PAIN"),
    generate_mapping("sleepquality_adu_q_1_h", timepoint_labels[30], "SLEEP_TROUBLE_STAYINGAWAKE"),
    generate_mapping("sleepquality_adu_q_1_i", timepoint_labels[30], "SLEEP_LACKING_ENTHUSIASM")
  )
  
  # Per questionnaire, replace all the columns from above.
  q_data_list <- q_data_list["covt28"]
  
  data_list_renamed <- mapply(function(q_data, t_id) {
    named_mapping_vector <- mapping %>% 
      filter(t == t_id) %>% dplyr::select(qnew, q) %>% deframe()
    
    message(sprintf("Processing %s", t_id))
    
    return(q_data %>% rename(named_mapping_vector) %>% as_tibble() %>% 
             dplyr::select(all_of(names(named_mapping_vector)), project_pseudo_id))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  PSQI_variables <- c("SLEEP_ACTUALHOURS.cat",
    "SLEEP_EFFICIENCY.cat",
    "SLEEP_DISTURBANCE.cat",
    "DAYTIME_DYSFUNCTION.cat", 
    "SLEEP_LATENCY.cat")
  
  MIDDAY <- hms("12:00:00")
  ONE_O_CLOCK <- hms("13:00:00")
  SIX_MORNING <- hms("06:00:00")
  
  outlier_function <- function(x) (quantile(x, 0.75, na.rm=T) - quantile(x, 0.25, na.rm=T)) * 3 + quantile(x, 0.75, na.rm=T)
  
  out_table <- bind_rows(data_list_renamed) %>% 
    mutate(SLEEP_ACTUALHOURS.cat = case_when(SLEEP_ACTUALHOURS >= 7 ~ 0,
                                             SLEEP_ACTUALHOURS >= 6 ~ 1,
                                             SLEEP_ACTUALHOURS >= 5 ~ 2,
                                             SLEEP_ACTUALHOURS < 5 ~ 3),
           sleeptimes_end_hms = hms(sleeptimes_end),
           sleeptimes_start_hms = hms(sleeptimes_start),
           sleeptimes_start_hms = if_else(sleeptimes_end_hms < MIDDAY & sleeptimes_start_hms > SIX_MORNING & sleeptimes_start_hms < ONE_O_CLOCK,
                                          sleeptimes_start_hms - MIDDAY,
                                          sleeptimes_start_hms),
           HOURS_IN_BED = if_else(sleeptimes_start_hms < sleeptimes_end_hms, 
                                  time_length(sleeptimes_end_hms - sleeptimes_start_hms, unit="hours"),
                                  time_length(sleeptimes_end_hms - sleeptimes_start_hms, unit="hours") + 24),
           SLEEP_EFFICIENCY = SLEEP_ACTUALHOURS / HOURS_IN_BED * 100,
           SLEEP_EFFICIENCY.cat = case_when(SLEEP_EFFICIENCY >= 85 ~ 0,
                                             SLEEP_EFFICIENCY >= 75 ~ 1,
                                             SLEEP_EFFICIENCY >= 65 ~ 2,
                                             SLEEP_EFFICIENCY < 65 ~ 3),
           SLEEP_DISTURBANCE = rowSums(across(starts_with("SLEEP_PROBLEMS"))),
           SLEEP_DISTURBANCE.cat = case_when(SLEEP_DISTURBANCE == 0 ~ 0,
                                             SLEEP_DISTURBANCE <= 6 ~ 1,
                                             SLEEP_DISTURBANCE <= 13 ~ 2,
                                             SLEEP_DISTURBANCE > 13 ~ 3),
           DAYTIME_DYSFUNCTION = SLEEP_TROUBLE_STAYINGAWAKE + SLEEP_LACKING_ENTHUSIASM,
           DAYTIME_DYSFUNCTION.cat = case_when(DAYTIME_DYSFUNCTION == 0 ~ 0,
                                           DAYTIME_DYSFUNCTION <= 2 ~ 1,
                                           DAYTIME_DYSFUNCTION <= 4 ~ 2,
                                           DAYTIME_DYSFUNCTION > 4 ~ 3),
           SLEEP_TIMETOSLEEP = case_when(SLEEP_TIMETOSLEEP > outlier_function(SLEEP_TIMETOSLEEP)  ~ NA_integer_,
                                         TRUE ~ SLEEP_TIMETOSLEEP),
           SLEEP_TIMETOSLEEP_cat = case_when(SLEEP_TIMETOSLEEP < 15 ~ 0,
                                             SLEEP_TIMETOSLEEP < 30 ~ 1,
                                             SLEEP_TIMETOSLEEP < 60 ~ 2,
                                             SLEEP_TIMETOSLEEP > 60 ~ 3),
           SLEEP_LATENCY = (SLEEP_TIMETOSLEEP_cat + SLEEP_PROBLEMS_TIME),
           SLEEP_LATENCY.cat = case_when(SLEEP_LATENCY == 0 ~ 0,
                                         SLEEP_LATENCY <= 2 ~ 1,
                                         SLEEP_LATENCY <= 4 ~ 2,
                                         SLEEP_LATENCY > 4 ~ 3),
           PSQI_score = rowSums(across(all_of(
             PSQI_variables))),
           PSQI_score.cat = case_when(PSQI_score > 4 ~ "Poor sleep quality",
                                      PSQI_score <= 4 ~ "Good sleep quality")) %>%
    select(project_pseudo_id, starts_with("SLEEP"), HOURS_IN_BED, starts_with("DAYTIME_DYSFUNCTION"), starts_with("PSQI"))
  
  return(out_table)
}

depression <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("minia1_adu_q_1", timepoint_labels[1:6], "minia1_adu_q_2"),
    generate_mapping("minia1_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minia1_adu_q_2"),
    generate_mapping("minia3d_adu_q_1", timepoint_labels[c(10:11, 13:31)], "DecreasedInterestPleasure"),
    generate_mapping("minia2_adu_q_1", timepoint_labels[1:6], "FatigueLossEnergy"),
    generate_mapping("minia2_adu_q_2", timepoint_labels[c(7:11, 13:31)], "FatigueLossEnergy"),
    generate_mapping("minia3a_adu_q_1", timepoint_labels[1:6], "minia3a_adu_q_2"),
    generate_mapping("minia3a_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minia3a_adu_q_2"),
    generate_mapping("minia3f_adu_q_1", timepoint_labels[1:6], "minia3f_adu_q_2"),
    generate_mapping("minia3f_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minia3f_adu_q_2"),
    generate_mapping("minia3b_adu_q_1", timepoint_labels[1:6], "minia3b_adu_q_2"),
    generate_mapping("minia3b_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minia3b_adu_q_2"),
    generate_mapping("minia3e_adu_q_1", timepoint_labels[1:6], "Worthlessness"),
    generate_mapping("minia3e_adu_q_2", timepoint_labels[c(7:11, 13:31)], "Worthlessness"),
    generate_mapping("minia3c_adu_q_1", timepoint_labels[1:6], "AgitationRetardation"),
    generate_mapping("minia3c_adu_q_2", timepoint_labels[c(7:11, 13:31)], "AgitationRetardation"),
    generate_mapping("minio1a_adu_q_1", timepoint_labels[1:6], "minio1a_adu_q_2"),
    generate_mapping("minio1a_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minio1a_adu_q_2"),
    generate_mapping("minio3a_adu_q_1", timepoint_labels[1:6], "minio3a_adu_q_2"),
    generate_mapping("minio3a_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minio3a_adu_q_2"),
    generate_mapping("minio3e_adu_q_1", timepoint_labels[1:6], "minio3e_adu_q_2"),
    generate_mapping("minio3e_adu_q_2", timepoint_labels[c(7:11, 13:31)], "minio3e_adu_q_2"),
    generate_mapping("responsedate_adu_q_1", timepoint_labels[1:31], "responsedate")
  )
  
  q_data_list <- q_data_list[names(q_data_list) != "covt12"]
  
  # Per questionnaire, replace all the columns from above.
  data_list_renamed <- mapply(function(q_data, t_id) {
    named_mapping_vector <- mapping %>% 
      filter(t == t_id) %>% select(qnew, q) %>% deframe()
    
    message(sprintf("Processing %s", t_id))
    
    return(q_data %>% rename(named_mapping_vector) %>% as_tibble() %>% 
             select(all_of(names(named_mapping_vector)), project_pseudo_id, responsedate))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  out_table <- bind_rows(data_list_renamed) %>% 
    mutate(project_pseudo_id = factor(project_pseudo_id)) %>%
    group_by(project_pseudo_id, .drop=F) %>%
    mutate(mandatory_symptoms = minia1_adu_q_2 == 1 | DecreasedInterestPleasure == 1,
           sum_of_symptoms = rowSums(across(all_of(c("minia1_adu_q_2", "DecreasedInterestPleasure", "FatigueLossEnergy", "minia3a_adu_q_2", "minia3f_adu_q_2", "minia3b_adu_q_2", "Worthlessness", "AgitationRetardation")))),
           MDD = case_when(mandatory_symptoms & sum_of_symptoms >= 4 ~ "Yes", !is.na(mandatory_symptoms) & !is.na(sum_of_symptoms) ~ "No")) %>%
    fill(MDD) %>%
    slice_max(responsedate, with_ties=F) %>%
    select(project_pseudo_id, MDD, starts_with("mini"))
  
  return(out_table)
}

demographics <- function(q_data_list) {
  mapping <- bind_rows(
    generate_mapping("responsedate_adu_q_1", timepoint_labels[1:31], "responsedate"),
    generate_mapping("maritalstatus_adu_q_1", timepoint_labels[c(26,28,30)], "MaritalStatus"),
    generate_mapping("employment_adu_q_4", timepoint_labels[c(28,30)], "WorkStatus"),
    generate_mapping("migration_adu_q_1", timepoint_labels[c(28)], "migration.status"),
    generate_mapping("activity_adu_q_1_b", timepoint_labels[c(1:6)], "physical.activity"),
    generate_mapping("activity_adu_q_2_b", timepoint_labels[c(7:9,11,14:19,21,23,25,26,27,29,31)], "physical.activity"),
    generate_mapping("smoking_adu_q_1", timepoint_labels[c(1:6)], "smoking.status"),
    generate_mapping("smoking_adu_q_2", timepoint_labels[c(7:9,11,14,25,26,27,29,31)], "smoking.status")
  )
  
  # Per questionnaire, replace all the columns from above.
  data_list_renamed <- mapply(function(q_data, t_id) {
    named_mapping_vector <- mapping %>% 
      filter(t == t_id) %>% select(qnew, q) %>% deframe()
    
    message(sprintf("Processing %s", t_id))
    
    return(q_data %>% rename(named_mapping_vector) %>% as_tibble() %>% 
             select(all_of(names(named_mapping_vector)), project_pseudo_id, age, gender))
  }, q_data_list, names(q_data_list), SIMPLIFY=F)
  
  out_table <- bind_rows(data_list_renamed) %>% 
    mutate(project_pseudo_id = factor(project_pseudo_id)) %>%
    group_by(project_pseudo_id, .drop=F) %>%
    summarise(DEMOGRAPHICS_45.imp = case_when(all(na.omit(gender) == "MALE") ~ "Male", all(na.omit(gender) == "FEMALE") ~ "Female"),
              DEMOGRAPHICS_45 = DEMOGRAPHICS_45.imp,
           DEMOGRAPHICS_46 = max(age, na.rm = T),
           DEMOGRAPHICS_47 = last(na.omit(MaritalStatus)),
           DEMOGRAPHICS_61 = last(na.omit(WorkStatus)),
           migration.status = last(na.omit(migration.status)),
           physical.activity = last(na.omit(physical.activity)),
           smoking.status = last(na.omit(smoking.status))
    ) %>%
    mutate(
           DEMOGRAPHICS_47 = case_when(
             DEMOGRAPHICS_47 == 1 ~ "Single",
             DEMOGRAPHICS_47 == 2 ~ "Married",
             DEMOGRAPHICS_47 == 3 ~ "Registered partnership",
             DEMOGRAPHICS_47 == 4 ~ "Divorced",
             DEMOGRAPHICS_47 == 5 ~ "Widowed",
             DEMOGRAPHICS_47 == 6 ~ "Other status - Please specify"
             ),
           DEMOGRAPHICS_61 = case_when(
             DEMOGRAPHICS_61 == 1 ~ "Full-time employed",
             DEMOGRAPHICS_61 == 2 ~ "Part-time employed",
             DEMOGRAPHICS_61 == 3 ~ "Self-employed or working for own family business",
             DEMOGRAPHICS_61 == 4 ~ "Unemployed",
             DEMOGRAPHICS_61 == 5 ~ "In vocational training/retraining/education",
             DEMOGRAPHICS_61 == 6 ~ "Parental leave",
             DEMOGRAPHICS_61 == 7 ~ "In retirement or early retirement",
             DEMOGRAPHICS_61 == 8 ~ "Permanently sick or disabled",
             DEMOGRAPHICS_61 == 9 ~ "Looking after home or family",
             DEMOGRAPHICS_61 == 10 ~ "Short-time working b",
             DEMOGRAPHICS_61 == 11 ~ "Other, please specify:"
           ),
           work.status = case_when(
             DEMOGRAPHICS_61 %in% c("Full-time employed", "Part-time employed", "Self-employed or working for own family business", "Parental leave", "In vocational training/retraining/education") ~ "Employed",
             DEMOGRAPHICS_61 %in% c("Unemployed", "Permanently sick or disabled", "Looking after home or family", "Short-time working b") ~ "Unemployed",
             DEMOGRAPHICS_61 %in% c("In retirement or early retirement") ~ "Retired"
           ),
           migration.status = case_when(
             migration.status %in% c(1,2) ~ "First generation immigrant",
             migration.status %in% c(3,4) ~ "Second generation immigrant",
             migration.status == 5 ~ "Native born"
           )) %>%
    select(project_pseudo_id,  DEMOGRAPHICS_45.imp,  DEMOGRAPHICS_46, DEMOGRAPHICS_47, DEMOGRAPHICS_61, work.status, migration.status, physical.activity, smoking.status)
  
  return(out_table)
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
  phenotypes <- configuration$phenotypes
  
  q_data_list_general <- list()
  
  for (qLabel in names(phenotypes)) {
    print(paste0("Loading: ", qLabel, ", ", phenotypes[qLabel]))
    
    q_data_list_general[[qLabel]] <- fread(
      as.character(phenotypes[qLabel]),
      quote="\"", na.strings = c('"$4"', '"$5"', '"$6"', '"$7"'))
    
  }
  
  education_table <- bind_rows(q_data_list_general, .id = "questionnaire") %>% 
    mutate(date = as.Date(paste0(date, "-01"), "%Y-%m-%d")) %>%
    group_by(project_pseudo_id) %>%
    arrange(date) %>%
    summarise(education = last(na.omit(educational_attainment_adu_c_1))) %>%
    mutate(education = case_when(education == 1 ~ "Basic", education == 2 ~ "Intermediate", education == 3 ~ "Advanced"))
  
  # Read every questionnaire
  q_data_list <- list()
  
  for (weekLabel in names(covidResFilePerWeek)) {
    print(paste0("Loading: ", weekLabel, ", ", covidResFilePerWeek[weekLabel]))
    
    q_data_list[[weekLabel]] <- fread(
      as.character(covidResFilePerWeek[weekLabel]),
      quote="\"", na.strings = c('"$4"', '"$5"', '"$6"', '"$7"'))

  }
  
  names(q_data_list) <- timepoint_labels
  
  number_of_covid_infections_tib <- number_of_covid_infections(q_data_list)
  number_of_long_covid_symptoms_tib <- number_of_long_covid_symptoms(q_data_list)
  loneliness_scale_tib <- loneliness_scale(q_data_list)
  sleep_quality_tib <- sleep_quality(q_data_list)
  depression_tib <- depression(q_data_list)
  demographic_tib <- demographics(q_data_list)
  demographic_tib <- demographic_tib %>%
    left_join(education_table %>% collect(), by = c("project_pseudo_id"))
  print(table(education_table %>% pull(education)))
  print(table(demographic_tib %>% pull(education)))
  
  derived <- purrr::reduce(list(number_of_covid_infections_tib, number_of_long_covid_symptoms_tib, loneliness_scale_tib, sleep_quality_tib, depression_tib, demographic_tib), 
                dplyr::full_join, by = 'project_pseudo_id')  
  
  fwrite(derived, "../out/derived_variables_RQ2.tsv", sep="\t", quote=F, row.names=F, col.names=T)
  save(derived, file="../out/derived_variables_RQ2.Rdata")
  
  # Perform method
  # Process output
}

if (sys.nframe() == 0 && !interactive()) {
  main()
}
