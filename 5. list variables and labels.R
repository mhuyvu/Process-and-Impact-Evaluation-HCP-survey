# SECTION 1 - DEMOGRAPHICS ----
vars_demo <- c("gender", "age", "ethnicity", "years_experience", "gmsa_region", "nhs_trusts");

# SECTION 2 - AWARENESS ----
vars_aware <- c("blood_spot_test_aware", "heard_screening_before", 
                "generation_study_aware", "attended_sessions_gs", "profession_inform_gs");

vars_involve <- c("involvement_gs___1", "involvement_gs___2", "involvement_gs___3", 
                  "involvement_gs___4", "involvement_gs___5");

# SECTION 3 - NBS KNOWLEDGE ----
vars_nbs <- c("families_offered", "blood_taken", "technology_blood", "conditions_looked_for",
              "results_returned", "signs_restults", "false_positive", "clinical_pathway",
              "uncertain_outcomes", "com_uncertain");

# SECTION 4 - GS KNOWLEDGE ----
vars_gs <- c("eligibility", "samples_taken", "technology_test", "conditions_looked",
             "results_return_no", "results_return_signs", "false_results", "pathway_cs",
             "uncertain_pot", "uncertain_com", "use_gd", "storage_gd");

# SECTION 5 - gNBS ATTITUDE ----
vars_attitude <- c("offering_gs_scale_2", "offer_genomic_gs", "genomic_newborn_good_bad", "genomic_newborn_good_bad_2",
                   "future_research_benefit", "important_gs", "good_gs", "good_gs_2");

# SECTION 6 - ACCEPTABILITY ----
vars_accept <- c("newborn_approval", "gs_appealing", "gs_clinical_practice", "welcome_gs_nhs");

# SECTION 7 AND 8 - BENEFIT AND RISK ----
vars_benefit <- c("knowledge", "knowledge_morechildren", "knowledge_reproductive",
                  "monitoring_outcome", "avoid_diagnostic_delay", "better_understanding_symp",
                  "reduce_costs", "less_inapprorpriate_test", "opportunity_store", "opportunity_use");

vars_risk <- c("contradictory_result", "uncertain_inconclusive",
               "patient_before_symp", "overly_reassured", "misattributed_paternity",
               "financial_cost", "babies_consent", "control_confidential",
               "future_discrim", "inequity_access_ethnic", "inequity_utility",
               "increased_contact", "uncertain_increases");

# SECTION 9 - WORKLOAD  ----
vars_work <- c("changes_workload", "expect_workload");

# SECTION 10 - IMPLEMENTATION ----
vars_impl <- c("rank_opt___1", "rank_opt___2", "rank_opt___3", "rank_opt___4", 
               "rank_opt___5", "rank_opt___6", "rank_opt___7", "rank_opt___8", 
               "rank_opt___9", "rank_opt___10", "rank_opt___11", "rank_opt___12", 
               "rank_opt___13", "rank_opt___14", "rank_opt___15", "rank_opt___16");

impl_labels <- c("rank_opt___1"  = "Public resources",
                 "rank_opt___2"  = "Resources available to workforce",
                 "rank_opt___3"  = "Equitable access",
                 "rank_opt___4"  = "gNBS nationally managed",
                 "rank_opt___5"  = "Pre-test counselling",
                 "rank_opt___6"  = "Health system capcity to analyse",
                 "rank_opt___7"  = "Results returned in timely manner",
                 "rank_opt___8"  = "Clear pathways for handover of results",
                 "rank_opt___9"  = "Post-test counselling",
                 "rank_opt___10" = "Genomic data stored for research",
                 "rank_opt___11" = "Genomic data stored for future use by family",
                 "rank_opt___12" = "Workforce upskilled for consent",
                 "rank_opt___13" = "Workforce upskilled for parents' concern and uncertainity",
                 "rank_opt___14" = "Workforce upskilled for results interpretation",
                 "rank_opt___15" = "Clinical services appropriately staffed",
                 "rank_opt___16" = "Resources for uncertain results");

# SECTION 11 - EDUCATION ----
vars_edu <- c("last_year_education___1", "last_year_education___2", "last_year_education___3", "last_year_education___4", 
              "last_year_education___5", "last_year_education___6", "last_year_education___7", "last_year_education___8", 
              "last_year_education___9", "last_year_education___10", "last_year_education___11", "last_year_education___12", 
              "last_year_education___13", "last_year_education___14");

edu_labels <- c("last_year_education___1"  = "Webinars / face-to-face training -  NHS Genomics / NHS England & NHS Improvement",
                "last_year_education___2"  = "Webinars / face-to-face training -  Royal Colleges or another professional body",
                "last_year_education___3"  = "Guidelines from professional bodies",
                "last_year_education___4"  = "Emails from NHS Genomics or the testing laboratories",
                "last_year_education___5"  = "Participating in multidisciplinary meetings",
                "last_year_education___6"  = "GMSA/GLH websites",
                "last_year_education___7"  = "Training from local geneticist/genetic counsellor",
                "last_year_education___8"  = "Journal articles",
                "last_year_education___9"  = "Genetic or genomic seminars or conferences",
                "last_year_education___10" = "Word of mouth from colleagues",
                "last_year_education___11" = "Public events",
                "last_year_education___12" = "Media/Social media",
                "last_year_education___13" = "I don't currently keep updated about genomic medicine",
                "last_year_education___14" = "Other");

vars_know <- c("know_more_gs___1", "know_more_gs___2", "know_more_gs___3", "know_more_gs___4", 
               "know_more_gs___5", "know_more_gs___6", "know_more_gs___7", "know_more_gs___8", 
               "know_more_gs___9", "know_more_gs___10");

know_labels <- c("know_more_gs___1" = "Basic science principles of genetics and genomics",
                 "know_more_gs___2"  = "Recognition and management of genetic conditions in babies and children including the appropriate NHS clinical care",
                 "know_more_gs___3"  = "Genetic counselling skills",
                 "know_more_gs___4"  = "Similarities and differences between genomic diagnostic and screening tests",
                 "know_more_gs___5"  = "Technical aspects of genome sequencing and laboratory interpretation",
                 "know_more_gs___6"  = "Practical aspects of offering genomic newborn screening",
                 "know_more_gs___7"  = "Interpretation of a genomic newborn screening report",
                 "know_more_gs___8"  = "Third-use of data for research and scientific purposes",
                 "know_more_gs___9"  = "Support services that can be offered to parents of babies and children with genetic conditions",
                 "know_more_gs___10" = "Other topic");

vars_need <- c("educational_needs___1", "educational_needs___2", "educational_needs___3", "educational_needs___4", 
               "educational_needs___5", "educational_needs___6", "educational_needs___7", "educational_needs___8");

need_labels <- c("educational_needs___1"  = "Webinars",
                 "educational_needs___2"  = "Written information",
                 "educational_needs___3"  = "Face-to-face training",
                 "educational_needs___4"  = "Group discussion / reflection",
                 "educational_needs___5"  = "Inclusion in mandatory training at your hospital",
                 "educational_needs___6"  = "Self-paced e-learning course/module",
                 "educational_needs___7"  = "Hands-on learning",
                 "educational_needs___8"  = "Other");

# SECTION 12 - PARTICIPATE IN GS ----
vars_par <- c("involved_gs", "last_3_month_tasks___1", "last_3_month_tasks___2",
              "last_3_month_tasks___3", "last_3_month_tasks___4", "last_3_month_tasks___5",
              "last_3_month_tasks___6", "last_3_month_tasks___7", "last_3_month_tasks___8",
              "last_3_month_tasks___9", "last_3_month_tasks___10", "eligibility_gs",
              "discuss_gs", "consent_families", "taking_samples", "samples_testing",
              "deviations_process", "results_families", "integrating_test", 
              "statements_recruit___1", "statements_recruit___2",
              "statements_recruit___3", "statements_recruit___4", "statements_recruit___5",
              "statements_recruit___6", "statements_recruit___7", "statements_recruit___8",
              "statements_recruit___9", "strategies_diverse", 
              "reasons_decline___1", "reasons_decline___2",
              "reasons_decline___3", "reasons_decline___4", "reasons_decline___5",
              "reasons_decline___6", "reasons_decline___7", "reasons_decline___8",
              "reasons_decline___9", "reasons_decline___11", "reasons_decline___12",
              "reasons_decline___13", "reasons_decline___14", "reasons_decline___15",
              "reasons_decline___16", "reasons_decline___17", "reasons_decline___18",
              "reasons_decline___19");

vars_tasks <- c("last_3_month_tasks___1", "last_3_month_tasks___2", "last_3_month_tasks___3", "last_3_month_tasks___4", 
                "last_3_month_tasks___5", "last_3_month_tasks___6", "last_3_month_tasks___7", "last_3_month_tasks___8",
                "last_3_month_tasks___9", "last_3_month_tasks___10");

task_labels <- c("last_3_month_tasks___1"  = "Deciding whether or not a parent is eligible for the Generation Study",
                 "last_3_month_tasks___2"  = "Discussing the Generation Study with families",
                 "last_3_month_tasks___3"  = "Consenting families to take part in the Generation Study",
                 "last_3_month_tasks___4"  = "Taking samples from newborns",
                 "last_3_month_tasks___5"  = "Sending samples for testing",
                 "last_3_month_tasks___6"  = "Dealing with deviations in processes (e.g. a missed sample or extra sample taken)",
                 "last_3_month_tasks___7"  = "Discussing results with families",
                 "last_3_month_tasks___8"  = "Integrating test results into the care of patients and their families",
                 "last_3_month_tasks___9"  = "None of the above",
                 "last_3_month_tasks___10"  = "Other");

vars_conf <- c("eligibility_gs", "discuss_gs", "consent_families", "taking_samples", "samples_testing",
               "deviations_process", "results_families", "integrating_test");

vars_recr <- c("statements_recruit___1", "statements_recruit___2",
               "statements_recruit___3", "statements_recruit___4", "statements_recruit___5",
               "statements_recruit___6", "statements_recruit___7", "statements_recruit___8",
               "statements_recruit___9")

recr_labels <- c("statements_recruit___1"  = "Posters and leaflets have been placed in waiting areas",
                 "statements_recruit___2"  = "Information has been mailed out to all booked parents",
                 "statements_recruit___3"  = "Parents have been approached when attending routine clinical appointments",
                 "statements_recruit___4"  = "Information has been sent to parents via SMS texts",
                 "statements_recruit___5"  = "Information has been sent to parents via email",
                 "statements_recruit___6"  = "Information has been sent to parents via app notifications or social media",
                 "statements_recruit___7"  = "Information has been sent to parents via social media",
                 "statements_recruit___8"  = "Other",
                 "statements_recruit___9"  = "Not relevant to my current role",
                 "none_selected" = "Missing");

vars_decl <- c("reasons_decline___1", "reasons_decline___2",
               "reasons_decline___3", "reasons_decline___4", "reasons_decline___5",
               "reasons_decline___6", "reasons_decline___7", "reasons_decline___8",
               "reasons_decline___9", "reasons_decline___11", "reasons_decline___12",
               "reasons_decline___13", "reasons_decline___14", "reasons_decline___15",
               "reasons_decline___16", "reasons_decline___17", "reasons_decline___18",
               "reasons_decline___19");

decl_labels <- c("reasons_decline___1"  = "Concerns around the baby being part of a long term study",
                 "reasons_decline___2"  = "Not interested in healthcare research",
                 "reasons_decline___3"  = "Mistrust of healthcare research",
                 "reasons_decline___4"  = "Privacy (data security) concerns around providing genomic/health/personal data",
                 "reasons_decline___5"  = "Concerns around insurance implications",
                 "reasons_decline___6"  = "Don’t see the value of genomic newborn screening as newborn blood spot screening already in place",
                 "reasons_decline___7"  = "Perceived irrelevance due to lack of family history of genetic conditions",
                 "reasons_decline___8"  = "Concern around false positives and/or uncertain findings",
                 "reasons_decline___9"  = "Concerns around emotional impact of a positive result",
                 "reasons_decline___10" = "Treatment/intervention concerns",
                 "reasons_decline___11" = "Concerns around the ‘newness’ of genomic newborn screening",
                 "reasons_decline___12" = "Religious/ethical/cultural beliefs",
                 "reasons_decline___13" = "Ethical concerns related to child lacking capacity to consent for themselves",
                 "reasons_decline___14" = "Partner/other family members don’t want to",
                 "reasons_decline___15" = "Lack of time/wrong time",
                 "reasons_decline___16" = "Want to avoid the anxiety of waiting for a result",              
                 "reasons_decline___17" = "Previous negative healthcare experience",
                 "reasons_decline___18" = "Other",
                 "reasons_decline___19" = "Don't know / Not relevant to my role");
