

# HEALTHCARE PROFESSIONAL SURVEY ANALYSIS
# Author : Martin Vu
# Date   : 24 April 2026
# Purpose: Analysis HCP data 


# SETUP --------------------------------------------------------------------

rm(list = ls()) 
cat("\14")

# Load required packages

library(dplyr)
library(forcats)
library(ggh4x)
library(ggplot2)
library(ggtext)
library(patchwork)
library(readxl)
library(stringr)
library(tableone)
library(tidytext)
library(tidyr)


# File paths

path_data <- "C:/Users/wpw478/University College London/Generation Study_Evaluation Team - General/Study 3 Gathering wider professional viewpoints/Survey Data"
path_out  <- "C:/Users/wpw478/University College London/Generation Study_Evaluation Team - General/Study 6 Cost effectiveness evaluation/HCP survey/Outputs/"
path_proj <- "C:/Users/wpw478/University College London/Generation Study_Evaluation Team - General/Study 6 Cost effectiveness evaluation/HCP survey"

setwd(path_data)

# Load data

data_imp <- read.csv(file = "ViewsOnOfferingGenom_DATA_2026-04-18_1403.csv")

# Source project helper scripts

source(file.path(path_proj, "1. function plot.R"))
source(file.path(path_proj, "2. list variables and labels.R"))
source(file.path(path_proj, "3. cleaning variables.R"))


# FILTER TO ANALYTICAL SAMPLE ----------------------------------------------

nrow(data_imp)
table(data_imp$consent, useNA = "ifany")

data_imp <- data_imp %>%
  filter(consent == 1)

nrow(data_imp)

# Remove respondents who answered no questions 
# Columns 3 onward are the substantive survey items

n_empty <- sum(rowSums(!is.na(data_imp[, 3:ncol(data_imp)])) == 0)
n_empty
rm(n_empty)

# View(data_imp[rowSums(!is.na(data_imp[, 3:ncol(data_imp)])) == 0, ])

data_imp <- data_imp %>%
  filter(rowSums(!is.na(across(3:ncol(data_imp)))) > 0)

nrow(data_imp)


# Remove respondents missing key variables 
# Professional role and awareness of the Generation Study are both required for the planned analyses

n_missing_role      <- sum(is.na(data_imp$professional_role))
n_missing_awareness <- sum(is.na(data_imp$generation_study_aware))
n_missing_gender    <- sum(is.na(data_imp$gender))
n_missing_age       <- sum(is.na(data_imp$age))
n_missing_ethnicity <- sum(is.na(data_imp$ethnicity))
n_missing_years     <- sum(is.na(data_imp$years_experience))
n_missing_gmsa      <- sum(is.na(data_imp$gmsa_region))
n_missing_any       <- sum(is.na(data_imp$professional_role) |
    is.na(data_imp$generation_study_aware) |
    is.na(data_imp$gender) |
    is.na(data_imp$age) |
    is.na(data_imp$ethnicity) |
    is.na(data_imp$years_experience) |
    is.na(data_imp$gmsa_region))

cat("Missing professional_role: ", n_missing_role, "\n")
cat("Missing generation_study_aware: ", n_missing_awareness, "\n")
cat("Missing gender: ", n_missing_gender, "\n")
cat("Missing age: ", n_missing_age, "\n")
cat("Missing ethnicity: ", n_missing_ethnicity, "\n")
cat("Missing years_experience: ", n_missing_years, "\n")
cat("Missing gmsa_region: ", n_missing_gmsa, "\n")
cat("Participants with ≥1 missing key variable:", n_missing_any, "\n")

data_imp <- data_imp %>%
  filter(!is.na(professional_role), !is.na(generation_study_aware),
         !is.na(gender), !is.na(age),
         !is.na(ethnicity), !is.na(years_experience),
         !is.na(gmsa_region))

nrow(data_imp)

rm(n_missing_role, n_missing_awareness, n_missing_gender, n_missing_age, n_missing_ethnicity, n_missing_years, n_missing_gmsa)


# write.csv(data_imp, file = paste0(path_data, "/HCP survey cleaned data.csv"), row.names = FALSE)


# SUBGROUPS  ----------------------------------------------

# Link professional groupings

data_prof <- read.csv(file = paste0(path_proj, "/genetic profession categories_mh.csv"))

data_imp <- merge(data_imp, data_prof, by.x = c("professional_role", "spec"), all.x = TRUE)

data_imp <- data_imp %>%
  select(record_id, consent, professional_role, spec, new_profession_grouping, genetics_background, everything()) %>%
  mutate(new_profession_grouping = if_else(new_profession_grouping == "", NA, new_profession_grouping),
         new_profession_grouping = if_else(is.na(new_profession_grouping), professional_role, new_profession_grouping)) %>%
  select(-professional_role, spec) %>%
  rename(professional_role = new_profession_grouping) %>%
  mutate(genetics_background = if_else(professional_role == "Clinical geneticist", 1, genetics_background))

unique(data_imp$professional_role)
data_imp$professional_role[data_imp$professional_role == "Clinical Scientist"]         <- "Clinical scientist"
data_imp$professional_role[data_imp$professional_role == "Midwife "]                   <- "Midwife"
data_imp$professional_role[data_imp$professional_role == "GP"]                         <- "General practitioner"
data_imp$professional_role[data_imp$professional_role == "Paeditrician"]               <- "Paediatrician"
data_imp$professional_role[data_imp$professional_role == "Obsterician"]                <- "Obstetrician"
data_imp$professional_role[data_imp$professional_role == "Health Visitor"]             <- "Health visitor"
data_imp$professional_role[data_imp$professional_role == "Allied Health Professional"] <- "Allied health professional"
data_imp$professional_role[data_imp$professional_role == "Midwifery Support Worker"]   <- "Midwifery support worker"

# Set professional subgroup

table(data_imp$genetics_background, useNA = "ifany")

# Set Generation Study involvement

unique(data_imp$generation_study_aware)

data_imp <- data_imp %>%
  mutate(gs_involvement = ifelse(generation_study_aware == "Yes - will be involved" | generation_study_aware == "Yes - involved", 1, 0)) %>%
  relocate(generation_study_aware, .after = genetics_background) %>%
  relocate(gs_involvement, .after = generation_study_aware)

data_imp <- data_imp %>%
  mutate(genetics_background = if_else(professional_role == "Clincial geneticist", 1, genetics_background))

unique(data_imp$professional_role)

# UPDATED DATA  ----------------------------------------------
# Updated data after review of professions and GMSA region 

# MANUAL DATA INPUT

genetic <- data_imp %>%
  distinct(genetics_background, professional_role, spec)%>%
  arrange(genetics_background, professional_role, spec)

rm(geneti)

# SECTION 1 - DEMOGRAPHICS  ----------------------------------------------

data_demo <- data_imp %>%
  select(all_of(c("record_id", "genetics_background", "gs_involvement", vars_demo))) %>%
  mutate(gmsa_region = if_else(gmsa_region %in% c("Not applicable", "Not sure"),
                       "Not applicable / specified", gmsa_region)) %>%
  mutate(gmsa_region = if_else(gmsa_region %in% c("Scotland", "Wales"),
                               "Non-English region", gmsa_region))

table_demo <- CreateTableOne(vars = c(vars_demo, "genetics_background", "gs_involvement"), data = data_demo, factorVars = c(vars_demo, "genetics_background", "gs_involvement"), includeNA = TRUE)
table_demo <- print(table_demo, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
table_demo

table_demo_gen <- CreateTableOne(vars = vars_demo, strata = c("genetics_background"), data = data_demo, factorVars = vars_demo, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_demo_gen <- print(table_demo_gen, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
table_demo_gen

table_demo_par <- CreateTableOne(vars = vars_demo, strata = c("gs_involvement"), data = data_demo, factorVars = vars_demo, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_demo_par <- print(table_demo_par, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
table_demo_par

write.csv(print(table_demo), file = paste0(path_out, "table_demo.csv"))
write.csv(print(table_demo_gen), file = paste0(path_out, "table_demo_gen.csv"))
write.csv(print(table_demo_par), file = paste0(path_out, "table_demo_par.csv"))

rm(data_demo)

data_demo <- data_imp %>%
  select(all_of(c("record_id", "genetics_background", "gs_involvement", vars_demo, "professional_role"))) %>%
  mutate(gmsa_region = if_else(gmsa_region %in% c("Not applicable", "Not sure", "Scotland", "Wales"),
                               "Not applicable / specified", gmsa_region))

p_gmsa <- data_demo %>%
  count(gmsa_region) %>%
  mutate(gmsa_region = fct_reorder(gmsa_region, n, .desc = TRUE),
    gmsa_region = fct_rev(gmsa_region)) %>%
  ggplot(aes(x = gmsa_region, y = n)) +
  coord_flip() + 
  geom_col(fill = "#1a9850", width = 0.6) +
  labs(x = "GMSA region", y = "Count") +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 1, b = 0), hjust = 0.5, face = 2),
        axis.title.y     = element_text(size = 15 * 0.8, margin = margin(t = 5, b = 0, l = 5), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p_gmsa

p_profession <- data_demo %>%
  count(professional_role) %>%
  mutate(professional_role = fct_reorder(professional_role, n, .desc = TRUE),
         professional_role = fct_relevel(professional_role, "Other", after = Inf),
         professional_role = fct_rev(professional_role)) %>%
  ggplot(aes(x = professional_role, y = n)) +
  coord_flip() + 
  geom_col(fill = "#1a9850", width = 0.6) +
  labs(x = "Profession", y = "Count") +
  scale_y_continuous(breaks = seq(0, 150, 10)) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 1, b = 0), hjust = 0.5, face = 2),
        axis.title.y     = element_text(size = 15 * 0.8, margin = margin(t = 5, b = 0, l = 5), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p_profession

ggsave(filename = paste0(path_out, "plot_profession.png"), plot = p_profession, width = 11, height = 5)

p_genetics <- data_demo %>%
  count(genetics_background) %>%
  mutate(genetics_background = recode(genetics_background,
                         `0` = "No genetics background",
                         `1` = "Genetics background")) %>%
  ggplot(aes(x = genetics_background, y = n)) +
  geom_col(fill = "#1a9850", width = 0.6) +
  labs(x = "Profession", y = "Count") +
  scale_y_continuous(breaks = seq(0, 650, 50)) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 5, b = 0), hjust = 0.5, face = 2),
        axis.title.y     = element_text(size = 15 * 0.8, margin = margin(t = 5, b = 0, r = 5), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p_genetics

p_gs <- data_demo %>%
  count(gs_involvement) %>%
  mutate(gs_involvement = recode(gs_involvement,
                                      `0` = "Not involved in Generation Study",
                                      `1` = "Involved in delivering Generation Study")) %>%
  ggplot(aes(x = gs_involvement, y = n)) +
  geom_col(fill = "#1a9850", width = 0.6) +
  labs(x = "Generation Study involvement", y = "Count") +
  scale_y_continuous(breaks = seq(0, 650, 50)) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 5, b = 0), hjust = 0.5, face = 2),
        axis.title.y     = element_text(size = 15 * 0.8, margin = margin(t = 5, b = 0, r = 5), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p_gs

# SECTION 2 - AWARENESS ----
# **** AWARENESS ----
data_aware <- data_imp %>%
  filter(complete.cases(select(., all_of(vars_aware))))

table_aware <- CreateTableOne(vars = vars_aware, data = data_aware, factorVars = vars_aware, includeNA = TRUE)
table_aware <- print(table_aware, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
table_aware

table_aware_gen <-  CreateTableOne(vars = vars_aware, strata = c("genetics_background"), data = data_aware, factorVars = vars_aware, includeNA = TRUE)
table_aware_gen <- print(table_aware_gen, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
table_aware_gen

table_aware_par <- CreateTableOne(vars = vars_aware, strata = c("gs_involvement"), data = data_aware, factorVars = vars_aware, includeNA = TRUE)
table_aware_par <- print(table_aware_par, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
table_aware_par

write.csv(print(table_aware), file = paste0(path_out, "table_aware.csv"))
write.csv(print(table_aware_gen), file = paste0(path_out, "table_aware_gen.csv"))
write.csv(print(table_aware_par), file = paste0(path_out, "table_aware_par.csv"))


levels_inform_gs <- c("Missing", "Prefer not to say", "Do not know",
                      "Not important at all", "Not very important", "Quite important",
                      "Very important", "Extremely important")

data_inform <- data_imp %>%
  mutate(profession_inform_gs = as.factor(profession_inform_gs),
         profession_inform_gs = fct_na_value_to_level(profession_inform_gs, level = "Missing"),
         profession_inform_gs = factor(profession_inform_gs, levels = levels_inform_gs),
         genetics_background = factor(case_when(genetics_background == 1 ~ "Genetics background",
                                                genetics_background == 0 ~ "No genetics background",
                                                TRUE                     ~ NA_character_),
                                      levels = c("No genetics background", "Genetics background")),
         gs_involvement = factor(case_when(gs_involvement == 1 ~ "Involved in delivering Generation Study",
                                           gs_involvement == 0 ~ "Not involved in Generation Study",
                                           TRUE                  ~ NA_character_),
                                 levels = c("Not involved in Generation Study", "Involved in delivering Generation Study")))

p1 <- data_inform %>%
  count(profession_inform_gs) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ggplot(aes(y = profession_inform_gs, x = pct)) +
  geom_col(fill = "#1a9850", width = 0.4) +
  labs(title = "Overall", x = "Percentage (%)", y = NULL) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 2, b = 0), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p2 <- data_inform %>%
  count(profession_inform_gs, genetics_background) %>%
  complete(profession_inform_gs, genetics_background, fill = list(n = 0)) %>%
  group_by(genetics_background) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(y = profession_inform_gs, x = pct, fill = genetics_background)) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("No genetics background" = "#f46d43",
                               "Genetics background"    = "#1a9850")) +
  labs(title = "Genetics background", x = "Percentage (%)", y = NULL, fill = NULL) +
  personal_theme() +
  theme(axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 2, b = 0), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p3 <- data_inform %>%
  count(profession_inform_gs, gs_involvement) %>%
  complete(profession_inform_gs, gs_involvement, fill = list(n = 0)) %>%
  group_by(gs_involvement) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(y = profession_inform_gs, x = pct, fill = gs_involvement)) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Not involved in Generation Study" = "#f46d43",
                               "Involved in delivering Generation Study"     = "#1a9850")) +
  labs(title = "Generation Study involvement", x = "Percentage (%)", y = NULL, fill = NULL) +
  personal_theme() +
  theme(axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.title.x     = element_text(size = 15 * 0.8, margin = margin(t = 2, b = 0), hjust = 0.5, face = 2),
        plot.title       = element_text(size = 15, margin = margin(b = 5), face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p2 <- p2 + theme(legend.position = "bottom")
p3 <- p3 + theme(legend.position = "bottom")

p_importance <- p1 | p2 | p3
p_importance

ggsave(filename = paste0(path_out, "plot_importance.png"), plot = p_importance, width = 14, height = 5)


# # **** INVOLVEMENT ----
# table_involve <- data_imp %>%
#   filter(complete.cases(select(., all_of(vars_aware)))) %>%
#   mutate(none_selected = ifelse(rowSums(across(all_of(vars_involve)) == 1, na.rm = TRUE) == 0, 1, 0)) %>%
#   summarise(across(all_of(vars_involve), ~ sum(.x == 1, na.rm = TRUE))) %>%
#   pivot_longer(everything(), names_to = "option", values_to = "count") %>%
#   mutate(proportion = count / nrow(data_aware) * 100,
#          option = case_when(
#            option == "involvement_gs___1" ~ "Consultation on conditions",
#            option == "involvement_gs___2" ~ "Participation in working groups",
#            option == "involvement_gs___3" ~ "Input into study materials",
#            option == "involvement_gs___4" ~ "Other, please describe",
#            option == "involvement_gs___5" ~ "Don't know")) %>%
#   mutate(value = paste0(count, " (", round(proportion, 1), ")")) %>%
#   select(option, value) %>%
#   mutate(variable = "Involvement in Generation Study") %>%
#   select(variable, option, value)
# 
# write.csv(print(table_involve), file = paste0(path_out, "table_involve.csv"), row.names = FALSE)


# SECTION 3 - NBS KNOWLEDGE ----

data_nbs <- data_imp %>%
  select(all_of(c("record_id", "genetics_background", "gs_involvement", vars_nbs))) %>%
  filter(complete.cases(select(., all_of(vars_nbs))))

n_total   <- nrow(data_imp)
n_section <- nrow(data_nbs)
n_missing <- data_imp %>%
  select(all_of(vars_nbs)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

table_nbs <- CreateTableOne(vars = vars_nbs, data = data_nbs, factorVars = vars_nbs, includeNA  = TRUE)
table_nbs <- print(table_nbs, showAllLevels = TRUE, noSpaces = TRUE)
table_nbs[1, 2] <- paste0("n = ", n_section, " / ", n_total)
write.csv(print(table_nbs), file = paste0(path_out, "table_nbs.csv"), quote = FALSE)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_nbs$genetics_background, useNA = "ifany")

table_nbs_gen <- CreateTableOne(vars = vars_nbs, strata = c("genetics_background"), data = data_nbs, factorVars = vars_nbs, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_nbs_gen <- print(table_nbs_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_nbs_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_nbs_gen[1, 3] <- paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_nbs_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_involvement, useNA = "ifany")
s_par <- table(data_nbs$gs_involvement, useNA = "ifany")

table_nbs_par <- CreateTableOne(vars = vars_nbs, strata = c("gs_involvement"), data = data_nbs, factorVars = vars_nbs, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_nbs_par <- print(table_nbs_par, showAllLevels = TRUE, noSpaces = TRUE)
table_nbs_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_nbs_par[1, 3] <-  paste0("n = ", s_par["0"], " / ", t_par["0"])
table_nbs_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

write.csv(print(table_nbs_gen), file = paste0(path_out, "table_nbs_gen.csv"), quote = FALSE)
write.csv(print(table_nbs_par), file = paste0(path_out, "table_nbs_par.csv"), quote = FALSE)



subgroup_vars <- c("genetics_background", "gs_involvement")

response_levels <- c("Not relevant to my role",
                     "Not at all confident", "Not confident",
                     "Neutral- neither confident nor not confident",
                     "Confident", "Strongly confident")

group_levels <- c("Overall",
                  "Clinical genetics background",
                  "No clinical genetics background",
                  "Involved in the Generation Study",
                  "Not involved in the Generation Study")

nbs_long <- data_nbs %>%
  mutate(across(all_of(subgroup_vars), as.character)) %>%
  pivot_longer(all_of(vars_nbs), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response))

nbs_overall <- nbs_long %>% mutate(group = "Overall")

nbs_sub <- nbs_long %>%
  pivot_longer(all_of(subgroup_vars), names_to = "subgroup", values_to = "level") %>%
  filter(!is.na(level)) %>%
  mutate(group = case_when(
    subgroup == "genetics_background" & level == "1" ~ "Clinical genetics background",
    subgroup == "genetics_background" & level == "0" ~ "No clinical genetics background",
    subgroup == "gs_involvement"      & level == "1" ~ "Involved in the Generation Study",
    subgroup == "gs_involvement"      & level == "0" ~ "Not involved in the Generation Study"
  ))

plot_nbs <- bind_rows(nbs_overall, nbs_sub) %>%
  group_by(group) %>%
  mutate(n_group = n_distinct(record_id)) %>%
  group_by(group, n_group, variable, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop     = n / n_group * 100,
         group    = factor(group, levels = group_levels),
         response = factor(response, levels = response_levels))

# Order items by overall % confident/strongly confident
var_order <- plot_nbs %>%
  filter(group == "Overall",
         response %in% c("Strongly confident", "Confident")) %>%
  group_by(variable) %>%
  summarise(green_total = sum(prop), .groups = "drop") %>%
  arrange(desc(green_total)) %>%
  pull(variable)

plot_nbs$variable <- factor(plot_nbs$variable, levels = rev(var_order))

# Facet labels with subgroup n
facet_labs <- plot_nbs %>%
  distinct(group, n_group) %>%
  mutate(lab = paste0(group, " (n = ", n_group, ")"))
facet_labs <- setNames(facet_labs$lab, as.character(facet_labs$group))

nbs_labels <- c(
  families_offered      = "Which families are offered newborn blood spot screening",
  blood_taken           = "How blood samples are taken",
  technology_blood      = "The technology used to test the blood samples",
  conditions_looked_for = "Which conditions the test looks for",
  results_returned      = "How results are returned when no condition is suspected",
  signs_restults        = "How results are returned when a condition is suspected",
  clinical_pathway      = "The clinical pathway when there is a condition suspected result",
  false_positive        = "Meaning of a 'false positive', 'false negative' or 'uncertain' result",
  uncertain_outcomes    = "Potential for uncertain outcomes following a positive screening result",
  com_uncertain         = "Communicating an uncertain outcome following a positive screening result"
)

p_nbs <- ggplot(plot_nbs, aes(x = prop, y = variable, fill = response)) +
  geom_col(position = "stack") +
  facet_wrap(~ group, ncol = 1, labeller = as_labeller(facet_labs)) +
  scale_y_discrete(labels = nbs_labels) +
  scale_x_continuous(breaks = seq(0, 100, 25), expand = c(0, 0)) +
  labs(x = "Percentage", y = NULL, fill = "Confidence level",
       caption = paste0("Missing data: ", n_missing, "/", n_total,
                        " participants had ≥1 missing response")) +
  personal_theme() +
  scale_fill_manual(
    values = c("Strongly confident"                           = "#1a9850",
               "Confident"                                    = "#66bd63",
               "Neutral- neither confident nor not confident" = "#fee08b",
               "Not confident"                                = "#f46d43",
               "Not at all confident"                         = "#d73027",
               "Not relevant to my role"                      = "#bdbdbd"),
    breaks = rev(response_levels)) +
  theme(legend.position = "bottom",
        legend.box      = "vertical",
        legend.margin   = margin(t = 10, r = 10, b = 10, l = 10),
        panel.spacing.y = unit(1, "lines"),
        strip.text      = element_text(hjust = 0.5, face = "bold"),
        plot.margin     = margin(5, 10, 40, 5))

ggsave(filename = paste0(path_out, "plot_nbs.png"), plot = p_nbs, width = 17, height = 18)

rm(nbs_long, nbs_overall, nbs_sub, plot_nbs, p_nbs, facet_labs, var_order)


# SECTION 4 - GS KNOWLEDGE ----

data_gs <- data_imp %>%
  select(all_of(c("record_id", "genetics_background", "gs_involvement", vars_gs))) %>%
  filter(complete.cases(select(., all_of(vars_gs))))

n_total   <- nrow(data_imp)
n_section <- nrow(data_gs)
n_missing <- data_imp %>%
  select(all_of(vars_gs)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

table_gs <- CreateTableOne(vars = vars_gs, data = data_gs, factorVars = vars_gs, includeNA  = TRUE)
table_gs <- print(table_gs, showAllLevels = TRUE, noSpaces = TRUE)
table_gs[1, 2] <- paste0("n = ", n_section, " / ", n_total)
write.csv(print(table_gs), file = paste0(path_out, "table_gs.csv"), quote = FALSE)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_gs$genetics_background, useNA = "ifany")

table_gs_gen <- CreateTableOne(vars = vars_gs, strata = c("genetics_background"), data = data_gs, factorVars = vars_gs, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_gs_gen <- print(table_gs_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_gs_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_gs_gen[1, 3] <- paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_gs_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_participation, useNA = "ifany")
s_par <- table(data_gs$gs_participation, useNA = "ifany")

table_gs_par <- CreateTableOne(vars = vars_gs, strata = c("gs_involvement"), data = data_gs, factorVars = vars_gs, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_gs_par <- print(table_gs_par, showAllLevels = TRUE, noSpaces = TRUE)
table_gs_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_gs_par[1, 3] <-  paste0("n = ", s_par["0"], " / ", t_par["0"])
table_gs_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

subgroup_vars <- c("genetics_background", "gs_involvement")

response_levels <- c("Not relevant to my role",
                     "Not at all confident", "Not confident",
                     "Neutral- neither confident nor not confident",
                     "Confident", "Strongly confident")

group_levels <- c("Overall",
                  "Clinical genetics background",
                  "No clinical genetics background",
                  "Involved in the Generation Study",
                  "Not involved in the Generation Study")

gs_long <- data_gs %>%
  mutate(across(all_of(subgroup_vars), as.character)) %>%
  pivot_longer(all_of(vars_gs), names_to = "variable", values_to = "response") %>%
  filter(!is.na(response))

gs_overall <- gs_long %>% mutate(group = "Overall")

gs_sub <- gs_long %>%
  pivot_longer(all_of(subgroup_vars), names_to = "subgroup", values_to = "level") %>%
  filter(!is.na(level)) %>%
  mutate(group = case_when(
    subgroup == "genetics_background" & level == "1" ~ "Clinical genetics background",
    subgroup == "genetics_background" & level == "0" ~ "No clinical genetics background",
    subgroup == "gs_involvement"      & level == "1" ~ "Involved in the Generation Study",
    subgroup == "gs_involvement"      & level == "0" ~ "Not involved in the Generation Study"
  ))

plot_gs <- bind_rows(gs_overall, gs_sub) %>%
  group_by(group) %>%
  mutate(n_group = n_distinct(record_id)) %>%
  group_by(group, n_group, variable, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop     = n / n_group * 100,
         group    = factor(group, levels = group_levels),
         response = factor(response, levels = response_levels))

var_order <- plot_gs %>%
  filter(group == "Overall",
         response %in% c("Strongly confident", "Confident")) %>%
  group_by(variable) %>%
  summarise(green_total = sum(prop), .groups = "drop") %>%
  arrange(desc(green_total)) %>%
  pull(variable)

plot_gs$variable <- factor(plot_gs$variable, levels = rev(var_order))

facet_labs <- plot_gs %>%
  distinct(group, n_group) %>%
  mutate(lab = paste0(group, " (n = ", n_group, ")"))
facet_labs <- setNames(facet_labs$lab, as.character(facet_labs$group))

gs_labels <- c(
  eligibility          = "Which families are eligible to take part in the Generation Study",
  samples_taken        = "How blood samples are taken",
  technology_test      = "The technology used to test the blood samples",
  conditions_looked    = "Which conditions the test looks for",
  results_return_no    = "How results are returned when no condition is suspected",
  results_return_signs = "How results are returned when a condition is suspected",
  pathway_cs           = "The clinical pathway when there is a condition suspected result",
  false_results        = "Meaning of a 'false positive', 'false negative' or 'uncertain' result",
  uncertain_pot        = "Potential for an uncertain outcome following genomic newborn screening",
  uncertain_com        = "Communicating an uncertain outcome following a positive screening result",
  use_gd               = "How genomic information will be used for future research",
  storage_gd           = "Storage of genomic data"
)

p_gs <- ggplot(plot_gs, aes(x = prop, y = variable, fill = response)) +
  geom_col(position = "stack") +
  facet_wrap(~ group, ncol = 1, labeller = as_labeller(facet_labs)) +
  scale_y_discrete(labels = gs_labels) +
  scale_x_continuous(breaks = seq(0, 100, 25), expand = c(0, 0)) +
  labs(x = "Percentage", y = NULL, fill = "Confidence level",
       caption = paste0("Missing data: ", n_missing, "/", n_total,
                        " participants had ≥1 missing response")) +
  personal_theme() +
  scale_fill_manual(
    values = c("Strongly confident"                           = "#1a9850",
               "Confident"                                    = "#66bd63",
               "Neutral- neither confident nor not confident" = "#fee08b",
               "Not confident"                                = "#f46d43",
               "Not at all confident"                         = "#d73027",
               "Not relevant to my role"                      = "#bdbdbd"),
    breaks = rev(response_levels)) +
  theme(legend.position = "bottom",
        legend.box      = "vertical",
        legend.margin   = margin(t = 10, r = 10, b = 10, l = 10),
        panel.spacing.y = unit(1, "lines"),
        strip.text      = element_text(hjust = 0.5, face = "bold"),
        plot.margin     = margin(5, 10, 40, 5))

ggsave(filename = paste0(path_out, "plot_gs.png"), plot = p_gs, width = 17, height = 22)

rm(gs_long, gs_overall, gs_sub, plot_gs, p_gs, facet_labs, var_order)


# SECTION 5 - gNBS ATTITUDE ----

data_attitude <- data_imp %>%
  select(all_of(c("record_id", "genetics_background", "gs_involvement", vars_attitude))) %>%
  filter(complete.cases(select(., all_of(vars_attitude))))

data_attitude <- data_attitude %>%
  mutate(across(all_of(vars_attitude), as.numeric),
         offering_gnb_att = rowSums(across(c(offering_gs_scale_2, offer_genomic_gs, genomic_newborn_good_bad, genomic_newborn_good_bad_2)), na.rm = TRUE),
         gen_data_att     = rowSums(across(c(future_research_benefit, important_gs, good_gs, good_gs_2)), na.rm = TRUE))

n_total   <- nrow(data_imp)
n_section <- nrow(data_attitude)
n_missing <- data_imp %>%
  select(all_of(vars_attitude)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

table_attitude <- CreateTableOne(vars = c("offering_gnb_att", "gen_data_att"), data = data_attitude, includeNA = TRUE)
table_attitude <- print(table_attitude, showAllLevels = TRUE, noSpaces = TRUE)
table_attitude[1, 2] <- paste0("n = ", n_section, " / ", n_total)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_attitude$genetics_background, useNA = "ifany")

table_attitude_gen <- CreateTableOne(vars = c("offering_gnb_att", "gen_data_att"), strata = c("genetics_background"), data = data_attitude, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_attitude_gen <- print(table_attitude_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_attitude_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_attitude_gen[1, 3] <-  paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_attitude_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_involvement, useNA = "ifany")
s_par <- table(data_attitude$gs_involvement, useNA = "ifany")

table_attitude_par <- CreateTableOne(c("offering_gnb_att", "gen_data_att"), strata = c("gs_involvement"), data = data_attitude, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_attitude_par <- print(table_attitude_par, showAllLevels = TRUE, noSpaces = TRUE)
table_attitude_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_attitude_par[1, 3] <-  paste0("n = ", s_par["0"], " / ", t_par["0"])
table_attitude_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

tt_gen_off <- t.test(offering_gnb_att ~ genetics_background, data = data_attitude, var.equal = TRUE)
tt_gen_dat <- t.test(gen_data_att ~ genetics_background, data = data_attitude, var.equal = TRUE)

tt_par_off <- t.test(offering_gnb_att ~ gs_involvement, data = data_attitude, var.equal = TRUE)
tt_par_dat <- t.test(gen_data_att ~ gs_involvement, data = data_attitude, var.equal = TRUE)

md_gen_off <- sprintf("%.2f (%.2f to %.2f)", tt_gen_off$estimate[2] - tt_gen_off$estimate[1], -tt_gen_off$conf.int[2], -tt_gen_off$conf.int[1])
md_gen_dat <- sprintf("%.2f (%.2f to %.2f)", tt_gen_dat$estimate[2] - tt_gen_dat$estimate[1], -tt_gen_dat$conf.int[2], -tt_gen_dat$conf.int[1])
md_par_off <- sprintf("%.2f (%.2f to %.2f)", tt_par_off$estimate[2] - tt_par_off$estimate[1], -tt_par_off$conf.int[2], -tt_par_off$conf.int[1])
md_par_dat <- sprintf("%.2f (%.2f to %.2f)", tt_par_dat$estimate[2] - tt_par_dat$estimate[1], -tt_par_dat$conf.int[2], -tt_par_dat$conf.int[1])

table_attitude_gen <- cbind(table_attitude_gen, "Mean diff (95% CI)" = c("", md_gen_off, md_gen_dat))
table_attitude_par <- cbind(table_attitude_par, "Mean diff (95% CI)" = c("", md_par_off, md_par_dat))

table_attitude_gen
table_attitude_par

write.csv(print(table_attitude), file = paste0(path_out, "table_attitude.csv"))
write.csv(print(table_attitude_gen), file = paste0(path_out, "table_attitude_gen.csv"))
write.csv(print(table_attitude_par), file = paste0(path_out, "table_attitude_par.csv"))

data_attitude <- data_attitude %>%
  select(-genetics_background, -gs_involvement) %>%
  pivot_longer(cols      = all_of(vars_attitude),
               names_to  = "item",
               values_to = "response")

item_labels <- c(offering_gs_scale_2        = "Offering genomic newborn screening (5 = benefit vs 1 = harm)",
                 offer_genomic_gs           = "Offering genomic newborn screening (5 = importance vs 1 = unimportant)",
                 genomic_newborn_good_bad   = "Offering genomic newborn screening (5 = good vs 1 = bad)",
                 genomic_newborn_good_bad_2 = "Offering genomic newborn screening (5 = helpful vs 1 = not helpful)",
                 future_research_benefit    = "Use of genomic data for research (5 = benefit vs 1 = harm)",
                 important_gs               = "Use of genomic data for research (5 = importance vs 1 = unimportant)",
                 good_gs                    = "Use of genomic data for research (5 = good vs bad)",
                 good_gs_2                  = "Use of genomic data for research (5 = helpful vs 1 = not helpful)")

data_attitude <- data_attitude %>%
  group_by(item, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

item_order <- names(item_labels)

data_attitude$item <- factor(data_attitude$item, levels = item_order)

data_attitude$response <- factor(
  data_attitude$response,
  levels = c(1, 2, 3, 4, 5))

response_cols <- c("1" = "#d73027",
                   "2" = "#f46d43",
                   "3" = "#fee08b",
                   "4" = "#66bd63",
                   "5" = "#1a9850")

data_attitude$item <- forcats::fct_rev(data_attitude$item)

p_attitude <- ggplot(data_attitude, aes(x = item, y = prop, fill = response)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(prop * 100), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = response_cols,
    name = "Response level",
    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = item_labels) +
  labs(x = NULL, y = "Proportion",
       caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had ≥1 missing response")) +
  personal_theme()

p_attitude

ggsave(filename = paste0(path_out, "plot_attitude.png"), plot = p_attitude, width = 14, height = 3)

# SECTION 6 - ACCEPTABILITY ----

data_accept <- data_imp %>%
  select(all_of(c("record_id", "genetics_background", "gs_involvement", vars_accept))) %>%
  filter(complete.cases(select(., all_of(vars_accept))))

n_total   <- nrow(data_imp)
n_section <- nrow(data_accept)
n_missing <- data_imp %>%
  select(all_of(vars_accept)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

likert_map <- c("Completely disagree"        = 1,
                "Disagree"                   = 2,
                "Neither agree nor disagree" = 3,
                "Agree"                      = 4,
                "Completely agree"           = 5)

data_accept <- data_accept %>%
  mutate(across(all_of(vars_accept), ~ recode(.x, !!!likert_map))) %>%
  filter(if_all(all_of(vars_accept), ~ !is.na(.))) %>%
  mutate(total_accept_score = rowSums(across(all_of(vars_accept))))

table_accept <- CreateTableOne(vars = c("total_accept_score"), data = data_accept, includeNA = TRUE)
table_accept <- print(table_accept, showAllLevels = TRUE, noSpaces = TRUE)
table_accept[1, 2] <- paste0("n = ", n_section, " / ", n_total)
write.csv(print(table_accept, showAllLevels = TRUE, noSpaces = TRUE), file = paste0(path_out, "table_accept.csv"), quote = FALSE)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_accept$genetics_background, useNA = "ifany")

table_accept_gen <- CreateTableOne(vars = c("total_accept_score"), strata = c("genetics_background"),  data = data_accept, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_accept_gen <- print(table_accept_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_accept_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_accept_gen[1, 3] <- paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_accept_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_involvement, useNA = "ifany")
s_par <- table(data_accept$gs_involvement, useNA = "ifany")

table_accept_par <- CreateTableOne(vars = c("total_accept_score"), strata = c("gs_involvement"),  data = data_accept, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_accept_par <- print(table_accept_par, showAllLevels = TRUE, noSpaces = TRUE)
table_accept_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_accept_par[1, 3] <-  paste0("n = ", s_par["0"], " / ", t_par["0"])
table_accept_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

tt_gen_acc <- t.test(total_accept_score ~ genetics_background, data = data_accept, var.equal = TRUE)
tt_par_acc <- t.test(total_accept_score ~ gs_involvement, data = data_accept, var.equal = TRUE)

md_gen_acc <- sprintf("%.2f (%.2f to %.2f)", tt_gen_acc$estimate[2] - tt_gen_acc$estimate[1], -tt_gen_acc$conf.int[2], -tt_gen_acc$conf.int[1])
md_par_acc <- sprintf("%.2f (%.2f to %.2f)", tt_par_acc$estimate[2] - tt_par_acc$estimate[1], -tt_par_acc$conf.int[2], -tt_par_acc$conf.int[1])

table_accept_gen <- cbind(table_accept_gen, "Mean diff (95% CI)" = c("", md_gen_acc))
table_accept_par <- cbind(table_accept_par, "Mean diff (95% CI)" = c("", md_par_acc))

table_accept_gen
table_accept_par

write.csv(print(table_accept), file = paste0(path_out, "table_accept.csv"))
write.csv(print(table_accept_gen), file = paste0(path_out, "table_accept_gen.csv"))
write.csv(print(table_accept_par), file = paste0(path_out, "table_accept_par.csv"))


# plot_accept <- data_accept %>%
#   pivot_longer(all_of(vars_accept), names_to = "variable", values_to = "response") %>%
#   filter(!is.na(response)) %>%
#   mutate(variable = factor(variable, levels = rev(vars_accept)),
#          response = factor(response,
#                            levels = c("Completely disagree", "Disagree", "Neither agree nor disagree",
#                                       "Agree", "Completely agree"))) %>%
#   group_by(variable, response) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   group_by(variable) %>%
#   mutate(prop = n / n_section)
# 
# var_order <- plot_accept %>%
#   filter(response %in% c("Completely agree", "Agree")) %>%
#   group_by(variable) %>%
#   summarise(green_total = sum(prop), .groups = "drop") %>%
#   arrange(desc(green_total)) %>%
#   pull(variable)
# 
# plot_accept$variable <- factor(plot_accept$variable, levels = rev(var_order))
# 
# p_accept <- ggplot(plot_accept, aes(x = prop, y = variable, fill = response)) +
#   geom_col(position = "stack") +
#   labs(x = "Percentage", y = NULL, fill = "Agreement level", 
#        caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had ≥1 missing response")) +  
#   personal_theme() + 
#   scale_fill_manual(
#     values = c("Completely agree"           = "#1a9850",
#                "Agree"                      = "#66bd63",
#                "Neither agree nor disagree" = "#fee08b",
#                "Disagree"                   = "#f46d43",
#                "Completely disagree"        = "#d73027"),
#     breaks = c("Completely agree", "Agree",
#                "Neither agree nor disagree",
#                "Disagree", "Completely disagree",
#                "Missing"))
# 
# p_accept
# 
# ggsave(filename = paste0(path_out, "plot_accept.png"), plot = p_accept, width = 14, height = 3)


# SECTION 7 AND 8 - BENEFIT AND RISK ----
# **** BENEFIT ----
data_benefit <- data_imp %>%
  select(record_id, genetics_background, gs_involvement, all_of(vars_benefit)) %>%
  mutate(across(all_of(vars_benefit), ~ as.character(.))) %>%
  mutate(across(all_of(vars_benefit), ~ dplyr::recode(.,
    "Not a benefit" = "1",
    "Extremely important benefit" = "10"))) %>%
  mutate(across(all_of(vars_benefit), as.numeric)) %>%
  filter(complete.cases(select(., all_of(vars_benefit))))

n_total   <- nrow(data_imp)
n_section <- nrow(data_benefit)
n_missing <- data_imp %>%
  select(all_of(vars_benefit)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

table_benefit <- CreateTableOne(vars = vars_benefit, data = data_benefit, includeNA = TRUE)
table_benefit <- print(table_benefit, showAllLevels = TRUE, noSpaces = TRUE)
table_benefit[1, 2] <- paste0("n = ", n_section, " / ", n_total)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_benefit$genetics_background, useNA = "ifany")

table_benefit_gen <- CreateTableOne(vars = vars_benefit, strata = c("genetics_background"),  data = data_benefit, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_benefit_gen <- print(table_benefit_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_benefit_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_benefit_gen[1, 3] <-  paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_benefit_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_involvement, useNA = "ifany")
s_par <- table(data_benefit$gs_involvement, useNA = "ifany")

table_benefit_par <- CreateTableOne(vars = vars_benefit, strata = c("gs_involvement"),  data = data_benefit, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_benefit_par <- print(table_benefit_par, showAllLevels = TRUE, noSpaces = TRUE)
table_benefit_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_benefit_par[1, 3] <- paste0("n = ", s_par["0"], " / ", t_par["0"])
table_benefit_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

write.csv(print(table_benefit), file = paste0(path_out, "table_benefit.csv"))
write.csv(print(table_benefit_gen), file = paste0(path_out, "table_benefit_gen.csv"))
write.csv(print(table_benefit_par), file = paste0(path_out, "table_benefit_par.csv"))

benefit_labels <- c(knowledge                 = "Knowledge for parents to prepare for the condition",
                    knowledge_morechildren    = "Knowledge for parents to make family planning decisions",
                    knowledge_reproductive    = "Knowledge for parents to inform family members",
                    monitoring_outcome        = "Monitoring for timely treatment",
                    avoid_diagnostic_delay    = "Avoid diagnostic delay",
                    better_understanding_symp = "Understand condition progression",
                    reduce_costs               = "Reduce healthcare costs",
                    less_inapprorpriate_test  = "Less inappropriate treatment/testing",
                    opportunity_store         = "Store genome across lifetime",
                    opportunity_use           = "Future research use")

item_order <- data_benefit %>%
  select(all_of(vars_benefit)) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "mean_rating") %>%
  arrange(mean_rating) %>%
  pull(variable)

p_benefit <- data_benefit %>%
  pivot_longer(all_of(vars_benefit), names_to = "variable", values_to = "rating") %>%
  group_by(variable) %>%
  summarise(mean_rating = mean(rating), .groups = "drop") %>%
  mutate(variable = factor(variable, levels = item_order))

p_benefit <- ggplot(p_benefit, aes(x = mean_rating, y = variable)) +
  geom_point(size = 3, shape = 15) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_discrete(labels = benefit_labels) +
  labs(x = NULL, y = NULL,
       caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had \u22651 missing response")) +
  personal_theme() +
  theme(legend.position  = "bottom",
        legend.direction = "vertical")

p_benefit

p_benefit_gen <- data_benefit %>%
  select(genetics_background, all_of(vars_benefit)) %>%
  pivot_longer(all_of(vars_benefit), names_to = "variable", values_to = "rating") %>%
  group_by(variable, genetics_background) %>%
  summarise(mean_rating = mean(rating), .groups = "drop") %>%
  mutate(genetics_background = recode(as.character(genetics_background),
                                      "0" = "No genetics background",
                                      "1" = "Genetics background")) %>%
  mutate(variable = factor(variable, levels = item_order))

segment_data <- p_benefit_gen %>%
  pivot_wider(names_from = genetics_background, values_from = mean_rating)

p_benefit_gen <- ggplot(p_benefit_gen, aes(x = mean_rating, y = variable)) +
  geom_segment(data = segment_data,
               aes(x = `No genetics background`, xend = `Genetics background`,
                   y = variable, yend = variable),
               inherit.aes = FALSE, colour = "#c3c2b7", linewidth = 1) +
  geom_point(aes(colour = genetics_background), size = 3.5, shape = 15) +
  scale_colour_manual(values = c("No genetics background" = "#d73027",
                                 "Genetics background"    = "#1a9850")) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_discrete(labels = benefit_labels) +
  labs(x = "Perceived benefit (0 = ‘I do not consider this to be a benefit; 10 = ‘I consider this to be an extremely important benefit)", y = NULL, colour = NULL) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 13, hjust = 0.5, face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p_benefit_gen

p_benefit_par <- data_benefit %>%
  select(gs_involvement, all_of(vars_benefit)) %>%
  pivot_longer(all_of(vars_benefit), names_to = "variable", values_to = "rating") %>%
  group_by(variable, gs_involvement) %>%
  summarise(mean_rating = mean(rating), .groups = "drop") %>%
  mutate(gs_involvement = recode(gs_involvement,
                                 `0` = "Not involved in Generation Study",
                                 `1` = "Involved in delivering Generation Study")) %>%
  mutate(variable = factor(variable, levels = item_order))

segment_data <- p_benefit_par %>%
  pivot_wider(names_from = gs_involvement, values_from = mean_rating)

p_benefit_par <- ggplot(p_benefit_par, aes(x = mean_rating, y = variable)) +
  geom_segment(data = segment_data,
               aes(x = `Not involved in Generation Study`, xend = `Involved in delivering Generation Study`,
                   y = variable, yend = variable),
               inherit.aes = FALSE, colour = "#c3c2b7", linewidth = 1) +
  geom_point(aes(colour = gs_involvement), size = 3.5, shape = 15) +
  scale_colour_manual(values = c("Not involved in Generation Study"        = "#d73027",
                                 "Involved in delivering Generation Study" = "#1a9850")) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_discrete(labels = benefit_labels) +
  labs(x = NULL, y = NULL, colour = NULL) +
  personal_theme() +
  theme(legend.position  = "bottom",
        legend.direction = "vertical")

p_benefit_par

p_benefit_tot <- p_benefit + p_benefit_gen + p_benefit_par +
  plot_layout(axes = "collect") 

p_benefit_tot

ggsave(filename = paste0(path_out, "plot_benefit.png"), plot = p_benefit_tot, width = 15, height = 7)

# **** RISK ----
data_risk <- data_imp %>%
  select(record_id, genetics_background, gs_involvement, all_of(vars_risk)) %>%
  mutate(across(all_of(vars_risk), ~ as.character(.))) %>%
  mutate(across(all_of(vars_risk), ~ dplyr::recode(.,
                                                   "Not a risk" = "1",
                                                   "Extremely important risk" = "10"))) %>%
  mutate(across(all_of(vars_risk), as.numeric)) %>%
  filter(complete.cases(select(., all_of(vars_risk))))

n_total   <- nrow(data_imp)
n_section <- nrow(data_risk)
n_missing <- data_imp %>%
  select(all_of(vars_risk)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

table_risk <- CreateTableOne(vars = vars_risk, data = data_risk, includeNA = TRUE)
table_risk <- print(table_risk, showAllLevels = TRUE, noSpaces = TRUE)
table_risk[1, 2] <- paste0("n = ", n_section, " / ", n_total)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_risk$genetics_background, useNA = "ifany")

table_risk_gen <- CreateTableOne(vars = vars_risk, strata = c("genetics_background"),  data = data_risk, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_risk_gen <- print(table_risk_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_risk_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_risk_gen[1, 3] <-  paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_risk_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_involvement, useNA = "ifany")
s_par <- table(data_risk$gs_involvement, useNA = "ifany")

table_risk_par <- CreateTableOne(vars = vars_risk, strata = c("gs_involvement"),  data = data_risk, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_risk_par <- print(table_risk_par, showAllLevels = TRUE, noSpaces = TRUE)
table_risk_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_risk_par[1, 3] <- paste0("n = ", s_par["0"], " / ", t_par["0"])
table_risk_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

write.csv(print(table_risk), file = paste0(path_out, "table_risk.csv"))
write.csv(print(table_risk_gen), file = paste0(path_out, "table_risk_gen.csv"))
write.csv(print(table_risk_par), file = paste0(path_out, "table_risk_par.csv"))

risk_labels <- c(contradictory_result    = "False positive results",
                 uncertain_inconclusive  = "Long-term impact of an uncertain/inconclusive result",
                 patient_before_symp     = "Child perceived as a 'patient in waiting'",
                 overly_reassured        = "False reassurance from a 'no condition suspected' result",
                 misattributed_paternity = "Misattributed paternity",
                 financial_cost          = "Financial cost to the NHS",
                 babies_consent          = "Babies unable to consent for themselves",
                 control_confidential    = "Long-term data security risks",
                 future_discrim          = "Future discrimination based on genomic information",
                 inequity_access_ethnic  = "Inequitable access for ethnic minorities",
                 inequity_utility        = "Inequitable clinical utility for ethnic minorities",
                 increased_contact       = "Increased parent-initiated healthcare contacts",
                 uncertain_increases     = "More uncertain outcomes as conditions screened increases")

item_order <- data_risk %>%
  select(all_of(vars_risk)) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "mean_rating") %>%
  arrange(mean_rating) %>%
  pull(variable)

p_risk <- data_risk %>%
  pivot_longer(all_of(vars_risk), names_to = "variable", values_to = "rating") %>%
  group_by(variable) %>%
  summarise(mean_rating = mean(rating), .groups = "drop") %>%
  mutate(variable = factor(variable, levels = item_order))

p_risk <- ggplot(p_risk, aes(x = mean_rating, y = variable)) +
  geom_point(size = 3, shape = 15) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_discrete(labels = risk_labels) +
  labs(x = NULL, y = NULL,
       caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had \u22651 missing response")) +
  personal_theme() +
  theme(legend.position  = "bottom",
        legend.direction = "vertical")

p_risk

p_risk_gen <- data_risk %>%
  select(genetics_background, all_of(vars_risk)) %>%
  pivot_longer(all_of(vars_risk), names_to = "variable", values_to = "rating") %>%
  group_by(variable, genetics_background) %>%
  summarise(mean_rating = mean(rating), .groups = "drop") %>%
  mutate(genetics_background = recode(as.character(genetics_background),
                                      "0" = "No genetics background",
                                      "1" = "Genetics background")) %>%
  mutate(variable = factor(variable, levels = item_order))

segment_data <- p_risk_gen %>%
  pivot_wider(names_from = genetics_background, values_from = mean_rating)

p_risk_gen <- ggplot(p_risk_gen, aes(x = mean_rating, y = variable)) +
  geom_segment(data = segment_data,
               aes(x = `No genetics background`, xend = `Genetics background`,
                   y = variable, yend = variable),
               inherit.aes = FALSE, colour = "#c3c2b7", linewidth = 1) +
  geom_point(aes(colour = genetics_background), size = 3.5, shape = 15) +
  scale_colour_manual(values = c("No genetics background" = "#d73027",
                                 "Genetics background"    = "#1a9850")) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_discrete(labels = risk_labels) +
  labs(x = "Perceived risk (0 = ‘I do not consider this to be a risk; 10 = ‘I consider this to be an extremely important risk)", y = NULL, colour = NULL) +
  personal_theme() +
  theme(axis.title.x     = element_text(size = 13, hjust = 0.5, face = 2),
        legend.position  = "bottom",
        legend.direction = "vertical")

p_risk_gen

p_risk_par <- data_risk %>%
  select(gs_involvement, all_of(vars_risk)) %>%
  pivot_longer(all_of(vars_risk), names_to = "variable", values_to = "rating") %>%
  group_by(variable, gs_involvement) %>%
  summarise(mean_rating = mean(rating), .groups = "drop") %>%
  mutate(gs_involvement = recode(gs_involvement,
                                 `0` = "Not involved in Generation Study",
                                 `1` = "Involved in delivering Generation Study")) %>%
  mutate(variable = factor(variable, levels = item_order))

segment_data <- p_risk_par %>%
  pivot_wider(names_from = gs_involvement, values_from = mean_rating)

p_risk_par <- ggplot(p_risk_par, aes(x = mean_rating, y = variable)) +
  geom_segment(data = segment_data,
               aes(x = `Not involved in Generation Study`, xend = `Involved in delivering Generation Study`,
                   y = variable, yend = variable),
               inherit.aes = FALSE, colour = "#c3c2b7", linewidth = 1) +
  geom_point(aes(colour = gs_involvement), size = 3.5, shape = 15) +
  scale_colour_manual(values = c("Not involved in Generation Study"        = "#d73027",
                                 "Involved in delivering Generation Study" = "#1a9850")) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_discrete(labels = risk_labels) +
  labs(x = NULL, y = NULL, colour = NULL) +
  personal_theme() +
  theme(legend.position  = "bottom",
        legend.direction = "vertical")

p_risk_par

p_risk_tot <- p_risk + p_risk_gen + p_risk_par +
  plot_layout(axes = "collect") 

p_risk_tot

ggsave(filename = paste0(path_out, "plot_risk.png"), plot = p_risk_tot, width = 15, height = 7)


# SECTION 9 - WORKLOAD  ----
data_work <- data_imp %>%
  select(genetics_background, gs_involvement, all_of(vars_work)) %>%
  filter(complete.cases(select(., all_of(vars_work))))

n_total   <- nrow(data_imp)
n_section <- nrow(data_work)
n_missing <- data_imp %>%
  select(all_of(vars_work)) %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  summarise(n = sum(any_missing)) %>%
  pull(n)

table_work <- CreateTableOne(vars = vars_work, data = data_work, factorVars = vars_work, includeNA = TRUE)
table_work <- print(table_work, showAllLevels = TRUE, noSpaces = TRUE)
table_work[1, 2] <- paste0("n = ", n_section, " / ", n_total)
write.csv(print(table_work, showAllLevels = TRUE, noSpaces = TRUE), file = paste0(path_out, "table_work.csv"), quote = FALSE)

t_gen <- table(data_imp$genetics_background, useNA = "ifany")
s_gen <- table(data_work$genetics_background, useNA = "ifany")

table_work_gen <- CreateTableOne(vars = vars_work, strata = c("genetics_background"),  data = data_work, factorVars = vars_work, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_work_gen <- print(table_work_gen, showAllLevels = TRUE, noSpaces = TRUE)
table_work_gen[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_work_gen[1, 3] <-  paste0("n = ", s_gen["0"], " / ", t_gen["0"])
table_work_gen[1, 4] <- paste0("n = ", s_gen["1"], " / ", t_gen["1"])

t_par <- table(data_imp$gs_involvement, useNA = "ifany")
s_par <- table(data_work$gs_involvement, useNA = "ifany")

table_work_par <- CreateTableOne(vars = vars_work, strata = c("gs_involvement"),  data = data_work, factorVars = vars_work, addOverall = TRUE, includeNA = TRUE, test = TRUE)
table_work_par <- print(table_work_par, showAllLevels = TRUE, noSpaces = TRUE)
table_work_par[1, 2] <- paste0("n = ", n_section, " / ", n_total)
table_work_par[1, 3] <- paste0("n = ", s_par["0"], " / ", t_par["0"])
table_work_par[1, 4] <- paste0("n = ", s_par["1"], " / ", t_par["1"])

write.csv(print(table_work), file = paste0(path_out, "table_work.csv"))
write.csv(print(table_work_gen), file = paste0(path_out, "table_work_gen.csv"))
write.csv(print(table_work_par), file = paste0(path_out, "table_work_par.csv"))

cat_levels <- rev(c("Large decrease", "Small decrease", "No change",
  "Small increase", "Large increase",
  "Don't know", "Prefer not to say"))

plot_work <- data_imp %>%
  select(changes_workload, expect_workload, genetics_background, gs_involvement) %>%
  filter(complete.cases(.)) %>%
  mutate(
    changes_workload = trimws(changes_workload),
    expect_workload  = trimws(expect_workload),
    changes_workload = case_when(
      changes_workload == "Yes, I have observed a small increase in workload" ~ "Small increase",
      changes_workload == "Yes, I have observed a large increase in workload" ~ "Large increase",
      changes_workload == "Yes, I have observed a small decrease in workload" ~ "Small decrease",
      changes_workload == "Yes, I have observed a large decrease in workload" ~ "Large decrease",
      changes_workload == "No, I have not observed any changes in workload"   ~ "No change",
      changes_workload == "Don't know"        ~ "Don't know",
      changes_workload == "Prefer not to say" ~ "Prefer not to say",
      TRUE ~ NA_character_
    ),
    expect_workload = case_when(
      expect_workload == "Yes, I expect there to be a small increase in workload" ~ "Small increase",
      expect_workload == "Yes, I expect there to be a large increase in workload" ~ "Large increase",
      expect_workload == "Yes, I expect there to be a small decrease in workload" ~ "Small decrease",
      expect_workload == "Yes, I expect there to be a large decrease in workload" ~ "Large decrease",
      expect_workload == "No, I would not expect any changes to workload"        ~ "No change",
      expect_workload == "Don't know"        ~ "Don't know",
      expect_workload == "Prefer not to say" ~ "Prefer not to say",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(changes_workload), !is.na(expect_workload))

plot_work <- bind_rows(plot_work %>%  mutate(group = "Overall"),
  plot_work %>% filter(genetics_background == 1) %>% mutate(group = "Clinical genetics background"),
  plot_work %>% filter(genetics_background == 0) %>% mutate(group = "No clinical genetics background"),
  plot_work %>% filter(gs_involvement == 1) %>% mutate(group = "Involved in Generation Study"),
  plot_work %>% filter(gs_involvement == 0) %>% mutate(group = "Not involved in Generation Study"))

obs_data <- plot_groups %>%
  count(group, changes_workload, name = "n") %>%
  group_by(group) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup() %>%
  transmute(group,
            category = factor(changes_workload, levels = cat_levels),
            prop,
            type = "Observed workload change")

exp_data <- plot_groups %>%
  count(group, expect_workload, name = "n") %>%
  group_by(group) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup() %>%
  transmute(group,
            category = factor(expect_workload, levels = cat_levels),
            prop,
            type = "Expected workload change")

plot_work <- bind_rows(obs_data, exp_data)

p_overall <- ggplot(filter(plot_work, group == "Overall"), aes(x = category, y = prop)) +
  geom_col(data = ~ filter(.x, type == "Observed workload change"),
           aes(fill = type), width = 0.6) +
  geom_col(data = ~ filter(.x, type == "Expected workload change"),
           aes(colour = type), fill = NA, linewidth = 1, width = 0.6) +
  scale_fill_manual(values = c("Observed workload change" = "#1a9850"), name = NULL) +
  scale_colour_manual(values = c("Expected workload change" = "#d7301f"), name = NULL) +
  scale_x_discrete(limits = cat_levels) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) +
  labs(title = "Overall", x = NULL, y = NULL) +
  coord_flip() +
  personal_theme() +
  theme(plot.title  = element_text(size = 12, margin = margin(b = 10), face = 2),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0, l = 0.5, "cm"))

p_cg <- ggplot(filter(plot_work, group == "Clinical genetics background"), aes(x = category, y = prop)) +
  geom_col(data = ~ filter(.x, type == "Observed workload change"),
           aes(fill = type), width = 0.6) +
  geom_col(data = ~ filter(.x, type == "Expected workload change"),
           aes(colour = type), fill = NA, linewidth = 1, width = 0.6) +
  scale_fill_manual(values = c("Observed workload change" = "#1a9850"), name = NULL) +
  scale_colour_manual(values = c("Expected workload change" = "#d7301f"), name = NULL) +
  scale_x_discrete(limits = cat_levels) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) +
  labs(title = "Clinical genetics background", x = NULL, y = NULL) +
  coord_flip() +
  personal_theme()+
  theme(plot.title  = element_text(size = 12, margin = margin(b = 10), face = 2),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0, l = 0.5, "cm"))

p_no_cg <- ggplot(filter(plot_work, group == "No clinical genetics background"), aes(x = category, y = prop)) +
  geom_col(data = ~ filter(.x, type == "Observed workload change"),
           aes(fill = type), width = 0.6) +
  geom_col(data = ~ filter(.x, type == "Expected workload change"),
           aes(colour = type), fill = NA, linewidth = 1, width = 0.6) +
  scale_fill_manual(values = c("Observed workload change" = "#1a9850"), name = NULL) +
  scale_colour_manual(values = c("Expected workload change" = "#d7301f"), name = NULL) +
  scale_x_discrete(limits = cat_levels) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) +
  labs(title = "No clinical genetics background", x = NULL, y = NULL) +
  coord_flip() +
  personal_theme() +
  theme(plot.title  = element_text(size = 12, margin = margin(b = 10), face = 2),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0, l = 0.5, "cm"))

p_involved <- ggplot(filter(plot_work, group == "Involved in Generation Study"), aes(x = category, y = prop)) +
  geom_col(data = ~ filter(.x, type == "Observed workload change"),
           aes(fill = type), width = 0.6) +
  geom_col(data = ~ filter(.x, type == "Expected workload change"),
           aes(colour = type), fill = NA, linewidth = 1, width = 0.6) +
  scale_fill_manual(values = c("Observed workload change" = "#1a9850"), name = NULL) +
  scale_colour_manual(values = c("Expected workload change" = "#d7301f"), name = NULL) +
  scale_x_discrete(limits = cat_levels) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) +
  labs(title = "Involved in Generation Study", x = NULL, y = NULL) +
  coord_flip() +
  personal_theme() +
  theme(plot.title  = element_text(size = 12, margin = margin(b = 10), face = 2),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0, l = 0.5, "cm"))

p_not_involved <- ggplot(filter(plot_work, group == "Not involved in Generation Study"), aes(x = category, y = prop)) +
  geom_col(data = ~ filter(.x, type == "Observed workload change"),
           aes(fill = type), width = 0.6) +
  geom_col(data = ~ filter(.x, type == "Expected workload change"),
           aes(colour = type), fill = NA, linewidth = 1, width = 0.6) +
  scale_fill_manual(values = c("Observed workload change" = "#1a9850"), name = NULL) +
  scale_colour_manual(values = c("Expected workload change" = "#d7301f"), name = NULL) +
  scale_x_discrete(limits = cat_levels) +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, 10)) +
  labs(title = "Not involved in Generation Study", x = NULL, y = NULL) +
  coord_flip() +
  personal_theme() +
  theme(plot.title  = element_text(size = 12, margin = margin(b = 10), face = 2),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0, l = 0.5, "cm"))

row_2 <- (p_cg | p_no_cg) + plot_layout(axes = "collect_y")
row_3 <- (p_involved | p_not_involved) + plot_layout(axes = "collect_y")

p_xlab <- ggplot() +
  annotate("text", x = 0, y = 0, label = "Percentage", size = 4.6, fontface = "bold") +
  theme_void()

p_work <- (p_overall / row_2 / row_3 / p_xlab) +
  plot_layout(heights = c(1, 1, 1, 0.06), guides = "collect") +
  plot_annotation(
    caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had \u22651 missing response"),
    theme   = theme(plot.caption = element_text(size = 10, hjust = 0))) &
  theme(legend.position = "bottom")

p_work

ggsave(paste0(path_out, "plot_work.png"), plot = p_work, width = 10, height = 9)

# SECTION 10 - IMPLEMENTATION ---- 

n_section <- data_imp %>% 
  select(all_of(vars_impl)) %>%
  summarise(n_section = sum(rowSums(as.matrix(.) == 1, na.rm = TRUE) > 0),
            n_none_selected = sum(rowSums(as.matrix(.) == 1, na.rm = TRUE) == 0),
            n_total = n()) %>%
  pull(n_section)

n_missing <- data_imp %>% 
  select(all_of(vars_impl)) %>%
  summarise(n_section = sum(rowSums(as.matrix(.) == 1, na.rm = TRUE) > 0),
            n_none_selected = sum(rowSums(as.matrix(.) == 1, na.rm = TRUE) == 0),
            n_total = n()) %>%
  pull(n_none_selected)

n_total <- data_imp %>% 
  select(all_of(vars_impl)) %>%
  summarise(n_section = sum(rowSums(as.matrix(.) == 1, na.rm = TRUE) > 0),
            n_none_selected = sum(rowSums(as.matrix(.) == 1, na.rm = TRUE) == 0),
            n_total = n()) %>%
  pull(n_total)

data_impl <- data_imp %>%
  select(all_of(vars_impl)) %>%
  mutate(none_selected = ifelse(rowSums(across(all_of(vars_impl)) == 1, na.rm = TRUE) == 0, 1, 0)) %>%
  filter(none_selected == 0) %>%
  pivot_longer(cols = everything(),
               names_to = "option",
               values_to = "selected") %>%
  filter(selected == 1) %>%
  mutate(option = recode(option, !!!impl_labels),
         option = factor(option, levels = unname(impl_labels))) %>%
  count(option, name = "n") %>%
  mutate(pct = n / n_section,
         value = paste0(n, " (", round(pct * 100, 1), ")")) %>%
  select(-n, -pct)

print(data_impl, showAllLevels = TRUE, noSpaces = TRUE)
colnames(data_impl)[2] <- paste0("n = ", n_section, " / ", nrow(data_imp))
write.csv(data_impl, file = paste0(path_out, "table_impl.csv"), row.names = FALSE)

plot_impl <- data_imp %>% 
  select(all_of(vars_impl)) %>%
  mutate(none_selected = ifelse(rowSums(across(all_of(vars_impl)) == 1, na.rm = TRUE) == 0, 1, 0)) %>%
  filter(none_selected == 0) %>%
  select(-none_selected) %>%
  pivot_longer(cols = everything(), names_to = "option", values_to = "n") %>% 
  mutate(option = recode(option, !!!impl_labels),
         option = factor(option, levels = unname(impl_labels))) %>% 
  group_by(option) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = n / n_section)

plot_impl$option <- factor(plot_impl$option, levels = plot_impl$option[order(plot_impl$pct, decreasing = FALSE)])

p_impl <- ggplot(plot_impl, aes(x = pct, y = option)) + 
  geom_col(fill = "#1a9850") + 
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels = function(x) x * 100) +
  labs(x = "Percentage", y = NULL,
       caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had \u22651 missing response")) +
  personal_theme()

p_impl

ggsave(paste0(path_out, "plot_impl.png"), plot = p_impl, width = 10, height = 5)


# SECTION 11 - EDUCATION ----
data_edu <- data_imp %>%
  mutate(miss_edu          = rowSums(as.matrix(across(all_of(vars_edu)))  == 1, na.rm = TRUE) == 0,
         miss_know         = rowSums(as.matrix(across(all_of(vars_know))) == 1, na.rm = TRUE) == 0,
         miss_need         = rowSums(as.matrix(across(all_of(vars_need))) == 1, na.rm = TRUE) == 0,
         any_block_missing = miss_edu | miss_know | miss_need)

n_total   <- nrow(data_imp)
n_missing <- sum(data_edu$any_block_missing)
n_section <- n_total - n_missing

data_edu <- data_edu %>% filter(any_block_missing == FALSE)

p_edu_data <- data_edu %>%
  select(all_of(vars_edu)) %>%
  pivot_longer(cols = everything(), names_to = "option", values_to = "selected") %>%
  filter(selected == 1) %>%
  mutate(option = recode(option, !!!edu_labels),
         option = factor(option, levels = unname(edu_labels))) %>%
  count(option, name = "n") %>%
  mutate(pct = n / n_section,
         section = "Genetics or genomic education attended in the last year")

table_edu <- p_edu_data %>%
  mutate(value = paste0(n, " (", round(pct * 100, 1), ")")) %>%
  select(option, value)
colnames(table_edu)[2] <- paste0("n = ", n_section, " / ", n_total)
print(table_edu, showAllLevels = TRUE, noSpaces = TRUE)
write.csv(table_edu, file = paste0(path_out, "table_edu.csv"), row.names = FALSE)

p_know_data <- data_edu %>%
  select(all_of(vars_know)) %>%
  pivot_longer(cols = everything(), names_to = "option", values_to = "selected") %>%
  filter(selected == 1) %>%
  mutate(option = recode(option, !!!know_labels),
         option = factor(option, levels = unname(know_labels))) %>%
  count(option, name = "n") %>%
  mutate(pct = n / n_section,
         section = "Topics wanted to know more about if gNBS was offered in NHS clinical practice")

table_know <- p_know_data %>%
  mutate(value = paste0(n, " (", round(pct * 100, 1), ")")) %>%
  select(option, value)
colnames(table_know)[2] <- paste0("n = ", n_section, " / ", n_total)
print(table_know, showAllLevels = TRUE, noSpaces = TRUE)
write.csv(table_know, file = paste0(path_out, "table_know.csv"), row.names = FALSE)

p_need_data <- data_edu %>%
  select(all_of(vars_need)) %>%
  pivot_longer(cols = everything(), names_to = "option", values_to = "selected") %>%
  filter(selected == 1) %>%
  mutate(option = recode(option, !!!need_labels),
         option = factor(option, levels = unname(need_labels))) %>%
  count(option, name = "n") %>%
  mutate(pct = n / n_section,
         section = "Preferred education format")

table_need <- p_need_data %>%
  mutate(value = paste0(n, " (", round(pct * 100, 1), ")")) %>%
  select(option, value)
colnames(table_need)[2] <- paste0("n = ", n_section, " / ", n_total)
print(table_need, showAllLevels = TRUE, noSpaces = TRUE)
write.csv(table_need, file = paste0(path_out, "table_need.csv"), row.names = FALSE)

section_levels <- c("Genetics or genomic education attended in the last year",
                    "Topics wanted to know more about if gNBS was offered in NHS clinical practice",
                    "Preferred education format")

plot_edu_combined <- bind_rows(p_edu_data, p_know_data, p_need_data) %>%
  mutate(section = factor(section, levels = section_levels),
         option  = reorder_within(option, pct, section))

p_edu_combined <- ggplot(plot_edu_combined, aes(x = pct, y = option)) +
  geom_col(fill = "#1a9850", width = 0.7) +
  facet_wrap(~ section, ncol = 1, scales = "free_y") +
  scale_y_reordered() +
  force_panelsizes(rows = c(14, 10, 8)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Percentage", y = NULL,
       caption = paste0("Missing data: ", n_missing, "/", n_total, " participants had \u22651 missing response")) +
  personal_theme() +
  theme(strip.text = element_text(face = "bold", size = 11))

p_edu_combined

ggsave(paste0(path_out, "plot_education_combined.png"), plot = p_edu_combined, width = 20, height = 10)

