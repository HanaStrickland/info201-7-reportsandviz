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

mean_na_info <- exp_by_info_interest %>%
  filter(info_interest == "N/A") %>%
  select(mean) %>%
  round(2)

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

####################################
### 2. Response Statistics Table ###
####################################

# This section will include a table reporting the break-downs 
# of technical experience among the survey respondents. This table will report the 
# total number of people who gave each of the different responses to the survey, 
# as well as any relevant aggregate data.

unique(survey_data$programming_exp)

colnames(survey_data)
rows <- c("Command Line", "Version Control", "Markdown", "R", "Web")
cols <- c("never", "few", "intermediate", "expert")

tech_exp <- survey_data %>% 
  select(cli_exp, vcs_exp, md_exp, r_exp, web_exp)

all_exp <- colnames(tech_exp)
row <- c("0")

never <- row
few <- row
intermediate <- row
expert <- row

new_frame <- data.frame(all_exp)

count <- nrow(tech_exp)

# This function counts the total number of people with the certain level of
# experience for each category

tech_exp_count <- function(exp_type, n) {
  x <- select(tech_exp, exp_type)
  data_type <- rlang::sym(exp_type)
  y <- filter(x, !!data_type == n) %>% 
    nrow() / count * 100
  return(round(y, 0))
}

cl <- tech_exp_count("cli_exp", 0)
vc <- tech_exp_count("vcs_exp", 0)
md <- tech_exp_count("md_exp", 0)
r <- tech_exp_count("r_exp", 0)
web <- tech_exp_count("web_exp", 0)

never <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, never = never)

cl <- tech_exp_count("cli_exp", 1)
vc <- tech_exp_count("vcs_exp", 1)
md <- tech_exp_count("md_exp", 1)
r <- tech_exp_count("r_exp", 1)
web <- tech_exp_count("web_exp", 1)

few <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, few = few)

cl <- tech_exp_count("cli_exp", 2)
vc <- tech_exp_count("vcs_exp", 2)
md <- tech_exp_count("md_exp", 2)
r <- tech_exp_count("r_exp", 2)
web <- tech_exp_count("web_exp", 2)

intermediate <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, intermediate = intermediate)

cl <- tech_exp_count("cli_exp", 3)
vc <- tech_exp_count("vcs_exp", 3)
md <- tech_exp_count("md_exp", 3)
r <- tech_exp_count("r_exp", 3)
web <- tech_exp_count("web_exp", 3)

expert <- c(cl, vc, md, r, web)
new_frame <- mutate(new_frame, expert = expert)
new_frame <- new_frame[, c(2,3,4,5)]

colnames(new_frame) <- c("Never", "A Few Times", "Intermediate", "Expert")
rownames(new_frame) <- c("Command Line", "Version Control", "Markdown", "R", "Web")

###############################
### 3. First Visualizations ###
###############################

cli_experience_plot <- ggplot(data = survey_data) +
  geom_jitter(mapping = aes(x = programming_exp, y = cli_exp), colour = "orange") +
  labs(x = "Overall Programming Experience Level", y = "Command Line/Terminal Experience Level") +
  facet_grid(. ~ info_interest, labeller = label_bquote(cols = paste(.(info_interest), " Informatics Interst")))

vcs_experience_plot <- ggplot(data = survey_data) +
  geom_jitter(mapping = aes(x = programming_exp, y = vcs_exp), colour = "green") +
  labs(x = "Overall Programming Experience Level", y = "Version Control/GitHub Experience Level") +
  facet_grid(. ~ info_interest, labeller = label_bquote(cols = paste(.(info_interest), " Informatics Interst")))

md_experience_plot <- ggplot(data = survey_data) +
  geom_jitter(mapping = aes(x = programming_exp, y = md_exp), colour = "blue") +
  labs(x = "Overall Programming Experience Level", y = "Markdown Experience Level") +
  facet_grid(. ~ info_interest, labeller = label_bquote(cols = paste(.(info_interest), " Informatics Interst")))

r_experience_plot <- ggplot(data = survey_data) +
  geom_jitter(mapping = aes(x = programming_exp, y = r_exp), colour = "red") +
  labs(x = "Overall Programming Experience Level", y = "R Programming Experience Level") +
  facet_grid(. ~ info_interest, labeller = label_bquote(cols = paste(.(info_interest), " Informatics Interst")))

web_experience_plot <- ggplot(data = survey_data) +
  geom_jitter(mapping = aes(x = programming_exp, y = web_exp), colour = "pink") +
  labs(x = "Overall Programming Experience Level", y = "Web Programming (HTML/CSS) Experience Level") +
  facet_grid(. ~ info_interest, labeller = label_bquote(cols = paste(.(info_interest), " Informatics Interst")))
