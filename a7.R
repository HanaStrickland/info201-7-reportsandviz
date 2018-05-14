library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(httr)
library(jsonlite)
library(maps)
library(plotly)

survey_data <- read.csv("data/intro_survey.csv", stringsAsFactors = FALSE)

#############################
### 1. Summary Statistics ###
#############################
exp_by_info_interest <- survey_data %>% 
  group_by(info_interest) %>%  
  summarize(mean = mean(programming_exp, na.rm = TRUE), 
            median = median(programming_exp,na.rm = TRUE),
            minimum = min(programming_exp, na.rm = TRUE),
            maximum = max(programming_exp, na.rm = TRUE)
            )

median_no_info <- exp_by_info_interest %>% 
  filter(info_interest == "No") %>% 
  select(median)

median_yes_info <- exp_by_info_interest %>% 
  filter(info_interest == "Yes") %>% 
  select(median)

median_unsure_info <- exp_by_info_interest %>% 
  filter(info_interest == "Not sure") %>% 
  select(median)

mean_no_info <- exp_by_info_interest %>% 
  filter(info_interest == "No") %>% 
  select(mean) %>% 
  round(2)

mean_yes_info <- exp_by_info_interest %>% 
  filter(info_interest == "Yes") %>% 
  select(mean) %>% 
  round(2)


mean_unsure_info <- exp_by_info_interest %>% 
  filter(info_interest == "Not sure") %>% 
  select(mean) %>% 
  round(2)

exp_by_os <- survey_data %>% 
  filter(os %in% c("Windows", "Linux (any)", "Mac OS X")) %>% 
  group_by(os) %>%  
  summarize(mean = mean(programming_exp, na.rm = TRUE), 
            median = median(programming_exp,na.rm = TRUE),
            minimum = min(programming_exp, na.rm = TRUE),
            maximum = max(programming_exp, na.rm = TRUE)
  )

median_mac <- exp_by_os %>% 
  filter(os == "Mac OS X") %>% 
  select(median)

median_windows <- exp_by_os %>% 
  filter(os == "Windows") %>% 
  select(median)

median_linux <- exp_by_os %>% 
  filter(os == "Linux (any)") %>% 
  select(median)

mean_mac <- exp_by_os %>% 
  filter(os == "Mac OS X") %>% 
  select(mean) %>% 
  round(2)

mean_windows <- exp_by_os %>% 
  filter(os == "Windows") %>% 
  select(mean) %>% 
  round(2)

mean_linux <- exp_by_os %>% 
  filter(os == "Linux (any)") %>% 
  select(mean) %>% 
  round(2)

###############################
### 3. First Visualizations ###
###############################

cli_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = cli_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = cli_exp, y = programming_exp), color = "red", method = "") +
  labs(x = "Command-Line/Terminal Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

vcs_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = vcs_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = vcs_exp, y = programming_exp), color = "blue") +
  labs(x = "Version Control Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

md_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = md_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = md_exp, y = programming_exp), color = "green") +
  labs(x = "Markdown Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

r_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = r_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = r_exp, y = programming_exp), color = "orange") +
  labs(x = "R Programming Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)

web_experience_plot_2 <- ggplot(data = survey_data) +
  geom_point(mapping = aes(x = web_exp, y = programming_exp)) +
  geom_smooth(mapping = aes(x = web_exp, y = programming_exp), color = "pink") +
  labs(x = "Web Programming Experience", y = "Overall Programming Experience") +
  facet_wrap(~info_interest)
