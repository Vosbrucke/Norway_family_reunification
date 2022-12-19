# Plots on Norway family reunification

# Load libraries
library(tidyverse)
library(magrittr)
library(janitor)
library(lubridate)
library(patchwork)
library(scales)


# Read data frame on Family immigration permits by applicant's citizenship, age group and gender by year
FIP_age_gender_df <- read_csv("Processed_data/FIP_age_gender_df_full.csv")

# Read data frame on Family immigration permits according to the applicant's citizenship and the person's basis of residence in Norway by year
FIP_residence_basis_df <- read_csv("Processed_data/FIP_residence_basis_df_full.csv")

# Read data frame on First-time family immigration permits according to the applicant's citizenship and relationship to the person in Norway by year
FIP_relationship_df <- read_csv("Processed_data/FIP_relationship_df_full.csv")

# Total family immigration permits in Norway and their approval rate
FIP_relationship_df %>% 
  filter(citizenship == "Total") %>% 
  ggplot(aes(year, total)) +
  geom_line() +
  geom_point(aes(size = approval_percentage)) +
  scale_size_continuous(name = "Approval rate:") +
  labs(
    x = NULL,
    y = NULL,
    title = "Total family immigration permits in Norway\nand their approval rate, 2014-2020",
    caption = "Date source: UDI.no"
  ) +
  scale_y_continuous(breaks = seq(8000, 16000, 2000), labels = paste0(seq(8, 16, 2), ".000"), limits = c(8000, 16000)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  ) 

# Save the plot
ggsave("Plots/Total family immigration permits in Norway and their approval rate.png", dpi = 900, bg = "white")


# Examine countries that received the most immigrants in the whole period of 2014-2021. Pull those countries
top_10_countries <- FIP_relationship_df %>% 
  filter(citizenship != "Total") %>% 
  group_by(citizenship) %>% 
  summarise(sum = sum(total)) %>% 
  top_n(10, sum) %>% 
  pull(citizenship)

# Total family immigration permits among top 10 countries of applicant citizenship
FIP_relationship_df %>% 
  select(-approval_percentage) %>% 
  filter(citizenship %in% top_10_countries) %>% 
  group_by(citizenship, year) %>%
  summarise(sum = sum(total)) %>%
  mutate(
    total_sum = sum(sum)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(sum, fct_reorder(citizenship, total_sum), fill = factor(year, levels = 2021:2014))) +
  geom_col() +
  labs(
    x = NULL,
    y = NULL,
    title = "Family immigration permits in Norway among 10 countries that received\nthe most permits in 2014-2021, by applicant citizenship",
    caption = "Data source: UDI.no"
  ) +
  scale_x_continuous(breaks = seq(0, 10000, 2500), limits = c(0, 11000), expand = c(0, 0)) +
  scale_fill_viridis_d(name = "", option = "viridis", direction = -1) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    fill = guide_legend(
      nrow = 1, 
      reverse = T, 
      label.position = "bottom", 
      keywidth = unit(1.5, "cm"), 
      keyheight = unit(0.5, "cm")
      )
    )

# Save the plot
ggsave("Plots/Total family immigration permits among top 10 countries of applicant citizenship.png", dpi = 900, bg = "white", width = 20, height = 17, unit = "cm")


# Create a pallet of 10 colors
palette <- wesanderson::wes_palette("GrandBudapest1", n = 10, "continuous")

# Calculate the total number of family reunification permits granted for selected top 10 countries 
total_sum <- FIP_relationship_df %>% 
  select(-approval_percentage) %>% 
  filter(citizenship %in% top_10_countries) %>% 
  group_by(citizenship, year) %>%
  summarise(sum = sum(total)) %>%
  ungroup() %>%
  summarise(sum = sum(sum))

# Calculate the total number of family reunification permits granted for all countries
total_permits <- FIP_relationship_df %>% 
  select(-approval_percentage) %>% 
  filter(citizenship != "Total") %>% 
  group_by(citizenship, year) %>%
  summarise(sum = sum(total)) %>%
  ungroup() %>%
  summarise(sum = sum(sum))

# Check the percentage rate of permits granted to the top 10 countries
total_sum / total_permits

# Calculate which country out of the chosen ones had the hights approval rate in 2021
FIP_relationship_df_10 <- FIP_relationship_df %>% 
  filter(citizenship %in% top_10_countries) %>% 
  group_by(citizenship) %>% 
  mutate(
    year = as.Date(paste0(year, "-01-01")),
    final_sum = first(approval_percentage)) %>%
  arrange(desc(final_sum)) %>% 
  ungroup()

# Pull country names
FIP_relationship_df_10$citizenship %>% unique()

# Create a structured way to present countries (descending way in two rows)
levels_citizenships <- c("India", "Russland", "Serbia", "Syria", "Filippinene", "Eritrea", "Thailand", "Pakistan", "USA", "Somalia")

# Approval percentage in Norway among top 10 countries of applicant citizenship
FIP_relationship_df_10 %>% 
  mutate(
    citizenship = factor(citizenship, levels = levels_citizenships)
  ) %>% 
  ggplot(aes(year, approval_percentage)) +
  geom_hline(yintercept = 60, size = 0.5) +
  geom_line(show.legend = F, size = 0.75) +
  scale_y_continuous(
    limits = c(60, 102), 
    breaks = seq(60, 100, 10),
    labels = paste0(seq(60, 100, 10), "%"), 
    expand = c(0, 0)
    ) +
  scale_x_date(
    breaks = c(as.Date("2015-01-01"), as.Date("2020-01-01")),
    date_labels = "%Y",
    date_minor_breaks = seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "1 year")
    ) +
  labs(
    x = "",
    y = "",
    title = "Approval percentage in Norway among 10 countries that received\nthe most permits in 2014-2021, by applicant citizenship",
    caption = "Data source: UDI.no"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    panel.grid.minor.x = element_line(linetype = "dotted", color  = "lightgrey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title= element_text(face = "bold"),
  ) +
  facet_wrap(~citizenship, nrow = 2, ncol = 5, dir = "v")

# Save the plot
ggsave("Plots/Approval percentage in Norway among top 10 countries of applicant citizenship.png", dpi = 900, bg = "white", width = 20, height = 17, unit = "cm")


# Set color palette
set.seed(105)
palette <- wesanderson::wes_palette("Darjeeling1", 4)
palette <- sample(palette, 4)

# Family immigration permits according to the applicant's relationship to the person in Norway
FIP_relationship_df %>% 
  filter(citizenship != "Total") %>%
  select(-c(citizenship, approval_percentage, total)) %>% 
  mutate(
    year = as.Date(paste0(year, "-01-01"))
  ) %>% 
  group_by(year) %>% 
  summarise(across(spouse_partner:other, sum)) %>% 
  pivot_longer(cols = spouse_partner:other) %>% 
  mutate(
    name = factor(name, levels = c("spouse_partner", "children", "other", "parents"), labels = c("spouse / partner", "children", "other", "parents"))
  ) %>% 
  ggplot(aes(year, value)) +
  geom_line(aes(color = name)) +
  geom_point(aes(fill = name), size = 1.5, shape = 21, color = "white", stroke = 1) +
  geom_hline(yintercept = 0, size = 1) +
  scale_color_manual(values = palette, name = "") +
  scale_fill_manual(values = palette, name = "") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 8500)) +
  labs(
    x = "",
    y = "",
    title = "Family immigration permits according to the\napplicant's relationship to the person in Norway, 2014-2021",
    caption = "Data source: UDI.no" 
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title= element_text(face = "bold"),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "bottom"
  )

# Save the plot
ggsave("Plots/Family immigration permits according to the applicant's relationship to the person in Norway.png", dpi = 900, bg = "white")


# Make a plot for parents family reunification 
FIP_relationship_df_parents <- FIP_relationship_df %>% 
  filter(citizenship != "Total") %>%
  select(-c(citizenship, approval_percentage, total)) %>% 
  mutate(
    year = as.Date(paste0(year, "-01-01"))
  ) %>% 
  group_by(year) %>% 
  summarise(across(spouse_partner:other, sum)) %>% 
  pivot_longer(cols = spouse_partner:other) %>% 
  filter(name == "parents") %>% 
  summarise(year, value, mean = mean(value)) %>% 
  mutate(mean = round(mean))

# Family reunification permits granted to parents of a sponsor in Norway
FIP_relationship_df_parents %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(color = palette[4]) +
  geom_point(size = 1.5, shape = 21, fill = palette[4], color = "white", stroke = 1) +
  geom_hline(yintercept = 0, size = 1) +
  scale_y_continuous(limits = c(0,640), breaks = seq(0, 600, 100), expand = c(0, 0)) +
  labs(
    x = "", 
    y = "", 
    title = "Family reunification permits granted to\nparents of a sponsor in Norway, 2014-2021",
    caption = "Data source: UDI.no" 
    ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title= element_text(face = "bold"),
    axis.ticks.x = element_line(color = "black")
  )

# Save the plot
ggsave("Plots/Family reunification permits granted to parents of a sponsor in Norway.png", dpi = 900, bg = "white")


# Create a palette of 8 colors
palette <- wesanderson::wes_palette("Zissou1", n = 8, "continuous")

# Pivot data on applicants age group and gender
FIP_age_gender_df_pivots <- FIP_age_gender_df %>% 
  filter(citizenship == "Totalt") %>% 
  mutate(
    year = as.Date(paste0(year, "-01-01"))
  ) %>% 
  select(-c(1:4)) %>% 
  group_by(year) %>% 
  mutate(
    across(women_girls_0_5_years:adult_men, ~ str_remove(., " ")),
    across(women_girls_0_5_years:adult_men, as.integer)
  ) %>% 
  summarise(across(women_girls_0_5_years:adult_men, sum)) %>% 
  pivot_longer(women_girls_0_5_years:adult_men) %>% 
  mutate(
    group = case_when(
    str_detect(name, "girls") ~ "girls",
    str_detect(name, "boys") ~ "boys",
    TRUE ~ "adults"
    ),
    labels = str_replace_all(name, "_", " "),
    labels = factor(labels, levels = c(unique(labels)))
  ) %>% 
  filter(group %in% "adults")

# Family immigration permits according to the applicant's age group
FIP_age_gender_df_pivots %>% 
  ggplot(aes(as.Date(year))) +
  geom_line(aes(y = value, color = labels)) +
  geom_point(aes(y = value, fill = labels), size = 1.5, shape = 21, color = "white", stroke = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  labs(
    x = "",
    y = "",
    title = "Family immigration permits according to the\napplicant's age group, 2015-2021",
    caption = "Data source: UDI.no"
  ) +
  scale_y_continuous(breaks = seq(0, 7000, 1000), limits = c(0, 7100), expand = c(0, 0)) +
  scale_x_date(
    breaks = "2 years",
    date_labels = "%Y"
    ) +
  scale_color_manual(values = palette, name = "") +
  scale_fill_manual(values = palette, name = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(color = "black")
  ) +
  facet_wrap(~ group)

# Save the plot
ggsave("Plots/Family immigration permits according to the applicant's age group.png", dpi = 900, bg = "white")
