# Webscraping and binding data sets

# Load libraries
library(rvest)
library(tidyverse)
library(magrittr)
library(janitor)
library(lubridate)
library(patchwork)


# Webscraping annual report sites
all_sites <- read_html("https://www.udi.no/en/statistics-and-analysis/facts-and-figures/facts-and-figures-from-previous-years/") %>% 
  html_nodes(".nav-tile") %>% html_attr('href')

# Removing the last two sites (not important ones)
all_sites <- all_sites[1:(length(all_sites) - 2)]

# Function scraping information from sites
tables_webscraped <- function (i) {
    
  # Create correct url link
  one_step_further <- paste0("https://www.udi.no", all_sites[i])
  
  # In all, but first site, there are additional links beneath the first link. Retrieve those links
  if (i != 1) {
  one_step_further %<>%
    read_html() %>% 
    html_nodes(".nav-tile") %>% html_attr('href') %>% 
    paste0("https://www.udi.no", .)
  } 
  
  # When there are two links use the one containing 'arstabeller' and scrape links on this site
  if (length(one_step_further) == 2) {
    all_data_sets <- one_step_further[str_which(one_step_further, "arstabeller")] %>% 
      read_html() %>% 
      html_nodes("a") %>% 
      html_attr("href") 
  } else { # Else scrape available links 
  all_data_sets <- one_step_further[1] %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href") 
  }
  
  # Retrieve links with data about family reunification- consisting of 'familie' word
  three_data_sets_per_year <- all_data_sets[str_which(all_data_sets, "familie")]
  
  # A function to read a table from provided site 
  final_data_sets <- function(x) {
    table_n1 <- three_data_sets_per_year[x] %>% 
      paste0("https://www.udi.no", .) %>% 
      read_html() %>% 
      html_nodes(xpath = '//*[@id="table-id"]') %>% 
      html_table()
  
  # Read only the first list
  table_n1[[1]]
  }
  
  # Retrieve data from website
  lapply(1:length(three_data_sets_per_year), final_data_sets)
}

# Connect all sites into list
webscarping_test <- lapply(1:length(all_sites), tables_webscraped)


# Retrieve data from all lists from the first table- Family immigration permits by applicant's citizenship, age group and gender
FIP_1 <- function(i) {
  webscarping_test[[i]][[1]] %>% as.data.frame() %>% 
    mutate(year = 2021 - i)
}

# Apply the function to 6 sites
FIP_age_gender <- lapply(1:6, FIP_1)

# Create clean column names
colnames <- make_clean_names(c("Citizenship", "Women in total", "Men in total", "Total", "Women (girls) 0 - 5 years", "Women (girls) 6 - 10 years", "Women (girls) 11 - 17 years", "Female adults", "Men (boys) 0 - 5 years", "Men (boys) 6 - 10 years", "Men (boys) 11 - 17 years", "Adult men", "Year"))

# Bidn rows to create a data frame on Family immigration permits by applicant's citizenship, age group and gender by year
FIP_age_gender_df <- do.call(rbind.data.frame, FIP_age_gender) %>% 
  set_colnames(colnames)

# Write data frame
write_csv(FIP_age_gender_df, "Processed_data/FIP_age_gender_df.csv")

# Load 2021 data downloaded from the website
FIP_age_gender_df_2021 <- read_csv("Raw_data/FIP_age_gender_df_2021.csv") %>% 
  set_colnames(colnames(FIP_age_gender_df)) %>% 
  mutate(year = 2021)

# Add 2021 data to the other years data. This is full data on Family immigration permits by applicant's citizenship, age group and gender
FIP_age_gender_df <- FIP_age_gender_df_2021 %>% 
  rbind(FIP_age_gender_df)

# Write full data set on Family immigration permits by applicant's citizenship, age group and gender by year
write_csv(FIP_age_gender_df, "Processed_data/FIP_age_gender_df_full.csv")


# Retrieve data from all lists from the second table- Family immigration permits according to the applicant's citizenship and the person's basis of residence in Norway
FIP_2 <- function(i) {
  webscarping_test[[i]][[2]] %>% as.data.frame() %>%
    mutate(year = 2021 - i)
}

# Apply the function to 7 sites
FIP_residence_basis <- lapply(1:7, FIP_2)

# Create clean column names
colnames <- make_clean_names(c("Citizenship", "Norwegian/Nordic", "Permanent residence permit", "Escape", "Work", "Education", "Family", "Other", "Total", "Year"))

# Bind rows to create a data frame on Family immigration permits according to the applicant's citizenship and the person's basis of residence in Norway by year
FIP_residence_basis_df <- do.call(rbind.data.frame, FIP_residence_basis) %>% 
  set_colnames(colnames)

# Write data frame
write_csv(FIP_residence_basis_df, "Processed_data/FIP_residence_basis_df.csv")

# Load 2021 data frame
FIP_residence_basis_df_2021 <- read_csv("Raw_data/FIP_residence_basis_df_2021.csv") %>% 
  set_colnames(colnames(FIP_residence_basis_df)) %>% 
  mutate(year = 2021)

# Add 2021 data to the other years data. This is full data on Family immigration permits according to the applicant's citizenship and the person's basis of residence in Norway by year
FIP_residence_basis_df <- FIP_residence_basis_df_2021 %>% 
  rbind(FIP_residence_basis_df)

# Write full data set on Family immigration permits according to the applicant's citizenship and the person's basis of residence in Norway by year
write_csv(FIP_residence_basis_df, "Processed_data/FIP_residence_basis_df_full.csv")


# Retrieve data from all lists from the third table- First-time family immigration permits according to the applicant's citizenship and relationship to the person in Norway
FIP_3 <- function(i) {
  if (i != 7) {
    webscarping_test[[i]][[3]] %>% as.data.frame() %>% 
      set_colnames(c(1:length(.))) %>% 
      mutate(year = 2021 - i)
  } else {
    webscarping_test[[i]][[1]] %>% as.data.frame() %>% 
      set_colnames(c(1:length(.))) %>% 
      mutate(year = 2021 - i)
  }
}

# Apply the function to 7 sites
FIP_relationship <- lapply(1:7, FIP_3)

# Make clean column names
colnames <- make_clean_names(c("Citizenship", "Spouse/partner", "Children", "Parents", "Other", "Total", "Approval percentage", "Year"))

# Bind rows to create a data frame on First-time family immigration permits according to the applicant's citizenship and relationship to the person in Norway by year
FIP_relationship_df <- do.call(rbind.data.frame, FIP_relationship) %>% 
  set_colnames(colnames) %>% 
  # Change approval rate to percentages
  mutate(
    approval_percentage = ifelse(str_length(approval_percentage) > 0, approval_percentage, "NA"),
    approval_percentage = str_sub(approval_percentage, 1, 2),
    approval_percentage = as.numeric(approval_percentage))

# Write data frame
write_csv(FIP_relationship_df, "Processed_data/FIP_relationship_df.csv")

# Load 2021 data
FIP_relationship_df_2021 <- read_csv("Raw_data/FIP_relationship_df_2021.csv") %>% 
  set_colnames(colnames(FIP_relationship_df)) %>% 
  mutate(year = 2021) %>% 
  # Change approval rate to percentages
  mutate(
    approval_percentage = ifelse(str_length(approval_percentage) > 0, approval_percentage, "NA"),
    approval_percentage = str_sub(approval_percentage, 1, 2),
    approval_percentage = as.numeric(approval_percentage),
    across(spouse_partner:total, ~ str_remove(., " ")),
    across(spouse_partner:total, as.numeric)
    )

# Add 2021 data to the other years data. This is full data on First-time family immigration permits according to the applicant's citizenship and relationship to the person in Norway by year
FIP_relationship_df <- FIP_relationship_df_2021 %>% 
  rbind(FIP_relationship_df) %>% 
  mutate(total = as.numeric(total),
         approval_percentage = as.numeric(approval_percentage))

# Write full data set on First-time family immigration permits according to the applicant's citizenship and relationship to the person in Norway by year
write_csv(FIP_relationship_df, "Processed_data/FIP_relationship_df_full.csv")
