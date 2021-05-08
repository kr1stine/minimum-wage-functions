library(tidyr)
library(dplyr)

# Functions to find selected social indicators
# from the EUROMOD output files


# The gender pay gap

hourly_gross_pay_gap <- function(data) {
  data$yivwg <- as.numeric(sub(",",".",data$yivwg, fixed=TRUE))
  data$dwt <- as.numeric(sub(",",".",data$dwt, fixed=TRUE))
  data$dag <- as.numeric(sub(",",".",data$dag, fixed=TRUE))
  
  hourly_men <- data[data[,"liwmy"] > 0 & data[,"dgn"]==1, c("yivwg", "dwt")]
  hourly_women <- data[data[,"liwmy"] > 0 & data[,"dgn"]==0, c("yivwg", "dwt")]
  avg_hourly_men <- weighted.mean(hourly_men$yivwg, hourly_men$dwt)
  avg_hourly_women <- weighted.mean(hourly_women$yivwg, hourly_women$dwt)

  pay_gap <- (avg_hourly_men - avg_hourly_women)/avg_hourly_men * 100
  return(pay_gap)
}


# Adds equivalized disposable income to the data

add_equivalize_income <- function(data) {
  data$ils_dispy <- as.numeric(sub(",",".",data$ils_dispy, fixed=TRUE))
  data$dag <- as.numeric(sub(",",".",data$dag, fixed=TRUE))
  
  # OECD levels
  first_adult <- 1
  next_adult <- 0.5
  child <- 0.3
  child_age <- 14
  data$oecd_weight <- ifelse(data$idperson == data$tu_sapehh_ee_HeadID, first_adult, ifelse(data$dag >= child_age, next_adult, child))
  
  by_hh <- data %>% group_by(idhh)
  total_income <- by_hh %>% summarise(
    total_dispy = sum(ils_dispy),
    total_weight = sum(oecd_weight),
    eq_dispy = total_dispy/total_weight,
    hh_adults_count = sum(tu_sapehh_ee_IsDependentChild == 0 & dag < 65),
    hh_children_count = sum(tu_sapehh_ee_IsDependentChild == 1),
    hh_pensioners_count = sum(dag >= 65),
    dgn_help = sum(dgn),
    n = n(),
  ) 
  total_income$hh_type <- case_when(
    total_income$hh_pensioners_count > 0 | total_income$hh_adults_count > 2 ~ "other",
    total_income$hh_adults_count == 1 & total_income$hh_children_count == 0 & total_income$dgn_help == 1 ~ "single_man",
    total_income$hh_adults_count == 1 & total_income$hh_children_count == 0 & total_income$dgn_help == 0 ~ "single_woman",
    total_income$hh_adults_count == 1 & total_income$hh_children_count > 0 ~ "single_parent",
    total_income$hh_adults_count == 2 & total_income$hh_children_count == 0 ~ "couple_no_children",
    total_income$hh_adults_count == 2 & total_income$hh_children_count == 1 ~ "couple_one_child",
    total_income$hh_adults_count == 2 & total_income$hh_children_count == 2 ~ "couple_two_children",
    total_income$hh_adults_count == 2 & total_income$hh_children_count > 2 ~ "couple_many_children",
    TRUE ~ "other"
  ) 
  
  data <- left_join(data, total_income, by="idhh")
  
  return(data)
}


# Gender disposable income gap

disposable_income_gap <- function(data) {
  data$ils_dispy <- as.numeric(sub(",",".",data$ils_dispy, fixed=TRUE))
  data$dwt <- as.numeric(sub(",",".",data$dwt, fixed=TRUE))
  
  disp_men <- data[data[,"ils_dispy"] > 0 & data[,"dgn"]==1, c("ils_dispy", "dwt")]
  disp_women <- data[data[,"ils_dispy"] > 0 & data[,"dgn"]==0, c("ils_dispy", "dwt")]
  
  avg_disp_men <- weighted.mean(disp_men$ils_dispy, disp_men$dwt)
  avg_disp_women <- weighted.mean(disp_women$ils_dispy, disp_women$dwt)
  
  pay_gap <- (avg_disp_men - avg_disp_women)/avg_disp_men * 100
  return(pay_gap)
}

# Gender equivalized disposable income gap

equivalized_disposable_income_gap <- function(data) {
  variable <- "eq_dispy"
  data[,variable] <- as.numeric(sub(",",".",data[,variable], fixed=TRUE))
  data$dwt <- as.numeric(sub(",",".",data$dwt, fixed=TRUE))
  
  disp_men <- data[data[,variable] > 0 & data[,"dgn"]==1, c(variable, "dwt")]
  disp_women <- data[data[,variable] > 0 & data[,"dgn"]==0, c(variable, "dwt")]
  
  avg_disp_men <- weighted.mean(disp_men[,variable], disp_men$dwt)
  avg_disp_women <- weighted.mean(disp_women[,variable], disp_men$dwt)
  
  pay_gap <- (avg_disp_men - avg_disp_women)/avg_disp_men * 100
  return(pay_gap)
}


# Absolute poverty rate
# Ratio of people who fall under the poverty line

absolute_poverty_rate <- function(poverty_line, data, household = NULL) {
  data$dwt <- as.numeric(sub(",",".",data$dwt, fixed=TRUE))
 
  if (is.null(household)) {
    total_weight <- sum(data$dwt)
    poor_weight <- sum(data[data$eq_dispy <= poverty_line,"dwt"])
  } else {
    total_weight <- sum(data[data$hh_type == household, "dwt"])
    poor_weight <- sum(data[data$hh_type == household & data$eq_dispy <= poverty_line, "dwt"])
  }
  
  return(round((poor_weight * 100)/total_weight,2))
}

# Relative poverty rate
# Ratio of people whose equivalent disposable income
# falls under the poverty line

relative_poverty_line <- function(data) {
  med <- median(rep(data$eq_dispy, times=data$dwt))
  return((60*med)/100)
}

relative_poverty_rate <- function(poverty_line, data, household = NULL) {
  data$dwt <- as.numeric(sub(",",".",data$dwt, fixed=TRUE))
  
  if (!is.null(household)) {
    total_weight <- sum(data[data$hh_type == household,"dwt"])
    poor_weight <- sum(data[data$hh_type == household & data$eq_dispy <= poverty_line,"dwt"])
  } else {
    total_weight <- sum(data$dwt)
    poor_weight <- sum(data[data$eq_dispy <= poverty_line,"dwt"])
  }
  
  return(round((poor_weight * 100)/total_weight, 2))
}

# In-work poverty rate
# Ratio of people who are employed and whose equivalent disposable income
# falls under the poverty line

in_work_poverty_rate <- function(poverty_line, data) {
  data$dwt <- as.numeric(sub(",",".",data$dwt, fixed=TRUE))
  data$liwmy <- as.numeric(sub(",",".",data$liwmy, fixed=TRUE))
  
  total_weight <- sum(data[data$liwmy > 0 ,"dwt"])
  poor_weight <- sum(data[data$liwmy > 0 & data$eq_dispy <= poverty_line,"dwt"])

  return(round((poor_weight * 100)/total_weight, 2))
}


# D5/D1 
deciles <- quantile(data$eq_dispy, prob=seq(0,1,length=11), type=5)
d5d1 <- deciles[[6]] / deciles[[2]]

# Gini coefficient on equivalized disposable income
library(laeken)

gini_disp <- gini(data$eq_dispy)
