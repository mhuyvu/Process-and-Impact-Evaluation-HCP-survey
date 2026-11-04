

# Check initial sample size
cat("Initial sample size:", nrow(data_imp), "\n");

# Inspect consent variable distribution
table(data_imp$consent, useNA = "ifany");
# View(data_imp %>% filter(is.na(consent)));

# Filter out participants without explicit consent
data_imp <- data_imp %>% filter(consent == 1);

# Check sample size after consent filter
cat("Sample size after consent filter:", nrow(data_imp), "\n");
table(data_imp$consent, useNA = "ifany");

# Count and remove completely empty responses (from column 3 onwards)
sum(rowSums(!is.na(data_imp[, 3:ncol(data_imp)])) == 0);
# View(data_imp[rowSums(!is.na(data_imp[, 3:ncol(data_imp)])) == 0, ]);

data_imp <- data_imp[rowSums(!is.na(data_imp[, 3:ncol(data_imp)])) > 0, ];
cat("Sample size after removing empty responses:", nrow(data_imp), "\n");

# Count and remove participants with missing demographics
vars_demo <- c("professional_role", "years_experience", "gmsa_region", "gender",
               "age", "ethnicity", "languages");

demo_na_count <- rowSums(is.na(data_imp[, vars_demo, drop = FALSE]));
table(demo_na_count);

sum(rowSums(is.na(data_imp[, vars_demo])) > 0);
# View(data_imp[rowSums(is.na(data_imp[, vars_demo])) > 0, ]);

data_imp <- data_imp[rowSums(is.na(data_imp[, vars_demo])) == 0, ];

# sum(is.na(data_imp$professional_role));
# data_imp <- subset(data_imp, !is.na(professional_role));
cat("Sample size after demographic filter:", nrow(data_imp), "\n");

# Count and remove participants with missing awareness data
vars_aware <- c("blood_spot_test_aware", "heard_screening_before", 
                "generation_study_aware", "attended_sessions_gs", "profession_inform_gs");

aware_na_count <- rowSums(is.na(data_imp[, vars_aware, drop = FALSE]));
table(aware_na_count);

sum(rowSums(is.na(data_imp[, vars_aware])) > 0);
# View(data_imp[rowSums(is.na(data_imp[, vars_aware])) > 0, ]);

data_imp <- data_imp[rowSums(is.na(data_imp[, vars_aware])) == 0, ];
cat("Final sample size after all exclusions:", nrow(data_imp), "\n");

# For checkbox/specific question blocks, recode remaining NAs to 0
data_imp <- data_imp %>%
  mutate(across(contains("__"), ~ ifelse(is.na(.), 0, .)));

# Count and remove participants with missing NBS knowledge data
vars_nbs <- c("families_offered", "blood_taken", "technology_blood", "conditions_looked_for",
              "results_returned", "signs_restults", "false_positive", "clinical_pathway",
              "uncertain_outcomes", "com_uncertain");

nbs_na_count <- rowSums(is.na(data_imp[, vars_nbs, drop = FALSE]));
table(nbs_na_count);

sum(rowSums(is.na(data_imp[, vars_nbs])) > 0);
# View(data_imp[rowSums(is.na(data_imp[, vars_nbs])) > 0, ]);

data_imp <- data_imp[rowSums(is.na(data_imp[, vars_nbs])) == 0, ];
cat("Final sample size after all exclusions:", nrow(data_imp), "\n");

# Count and remove participants with missing GS knowledge data
vars_gs <- c("eligibility", "samples_taken", "technology_test", "conditions_looked",
             "results_return_no", "results_return_signs", "false_results", "pathway_cs",
             "uncertain_pot", "uncertain_com", "use_gd", "storage_gd");

gs_na_count <- rowSums(is.na(data_imp[, vars_gs, drop = FALSE]));
table(gs_na_count);

sum(rowSums(is.na(data_imp[, vars_gs])) > 0);
# View(data_imp[rowSums(is.na(data_imp[, vars_gs])) > 0, ]);

data_imp <- data_imp[rowSums(is.na(data_imp[, vars_gs])) == 0, ];
cat("Final sample size after all exclusions:", nrow(data_imp), "\n");

# Count and remove participants with missing gNBS attitude data
vars_attitude <- c("offering_gs_scale_2", "offer_genomic_gs", "genomic_newborn_good_bad", "genomic_newborn_good_bad_2",
                   "future_research_benefit", "important_gs", "good_gs", "good_gs_2");

attitude_na_count <- rowSums(is.na(data_imp[, vars_attitude, drop = FALSE]));
table(attitude_na_count);

sum(rowSums(is.na(data_imp[, vars_attitude])) > 0);
# View(data_imp[rowSums(is.na(data_imp[, vars_attitude])) > 0, ]);

data_imp <- data_imp[rowSums(is.na(data_imp[, vars_attitude])) == 0, ];
cat("Final sample size after all exclusions:", nrow(data_imp), "\n");

# Count and remove participants with missing acceptability data
vars_accept <- c("newborn_approval", "gs_appealing", "gs_clinical_practice", "welcome_gs_nhs");

accept_na_count <- rowSums(is.na(data_imp[, vars_accept, drop = FALSE]));
table(accept_na_count);

sum(rowSums(is.na(data_imp[, vars_accept])) > 0);
# View(data_imp[rowSums(is.na(data_imp[, vars_accept])) > 0, ]);

data_imp <- data_imp[rowSums(is.na(data_imp[, vars_accept])) == 0, ];
cat("Final sample size after all exclusions:", nrow(data_imp), "\n");

rm(list = setdiff(ls(), c("data_imp", "path_out", "path_proj", "personal_theme")));
