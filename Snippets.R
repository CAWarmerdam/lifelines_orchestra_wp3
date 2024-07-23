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

# Declare constants

# Declare function definitions

# Snippet function to handle COVID infections for the timepoints 1-15
#' @param q_data The input data frame representing questionnaire data.
#' @return The data frame with added columns indicating COVID infections and test type.
.func_covid_infections_1_15 <- function(q_data) {
  q_data %>% mutate(covid_infection = case_when(covid_infections_1_15 == 1 ~ T, 
                                                covid_infections_1_15 == 2 ~ F),
                    covid_test_type = "unknown")
}

# Snippet function to handle COVID infections for the timepoints 15b-19
#' @param q_data The input data frame representing questionnaire data.
#' @return The data frame with added columns indicating COVID infections and test type.
.func_covid_infection_15b_19 <- function(q_data) {

  q_data %>% mutate(covid_infection = case_when(covid_infection_15b_19 == 1 ~ T,
                                                covid_infection_15b_19 == 2 ~ F),
                    covid_test_type = case_when(covid_infection_ggd_15b_19 == 1 ~ "ggd",
                                                covid_infection_ggd_15b_19 == 2 ~ "part",
                                                covid_infection_ggd_15b_19 == 3 ~ "no"))
}

# Snippet function to handle COVID infections for the timepoints 20-29
#' @param q_data The input data frame representing questionnaire data.
#' @return The data frame with added columns indicating COVID infections and test type.
.func_covid_infections_20_29 <- function(q_data) {
  q_data %>% mutate(
    covid_infection_tmp = rowSums(
      dplyr::select(., covid_accesstest_20_26, 
           covid_ggdtest_20_29, 
           covid_selftest_20_29, 
           covid_worktest_20_29, 
           covid_othertest_20_29) == 1, na.rm = TRUE),
    covid_infection = case_when(
      covid_infection_tmp >= 1 ~ T,
      !is.na(covid_accesstest_20_26) ~ F,
      !is.na(covid_ggdtest_20_29) ~ F,
      !is.na(covid_selftest_20_29) ~ F,
      !is.na(covid_worktest_20_29) ~ F,
      !is.na(covid_othertest_20_29) ~ F,
    ),
    covid_test_type = case_when(
      covid_infection_tmp > 1 ~ "part",
      covid_accesstest_20_26 == 1 ~ "access",
      covid_ggdtest_20_29 == 1 ~ "ggd",
      covid_selftest_20_29 == 1 ~ "self",
      covid_worktest_20_29 == 1 ~ "work",
      covid_othertest_20_29 == 1 ~ "other")
  ) %>% dplyr::select(-covid_infection_tmp)
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
  # Perform method
  # Process output
}

if (sys.nframe() == 0 && !interactive()) {
  main()
}