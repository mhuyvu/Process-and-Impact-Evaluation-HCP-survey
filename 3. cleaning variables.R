# Cleaning variables
data_imp <- data_imp %>%
  # 1. Safely convert 0 and empty strings to NA without destroying factor/numeric classes
  mutate(across(where(is.numeric) & !all_of(c("heard_screening_before")), ~ na_if(.x, 0))) %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  
  # 2. Main Variable Transformations
  mutate(
    professional_role = case_when(
      professional_role == 1 ~ "Nurse",
      professional_role == 2 ~ "Clinical nurse specialist",
      professional_role == 3 ~ "Research nurse",
      professional_role == 4 ~ "Genetic counsellor",
      professional_role == 5 ~ "Clincial geneticist",
      professional_role == 6 ~ "Health visitor",
      professional_role == 7 ~ "Obsterician",
      professional_role == 8 ~ "Fetal medicine doctor",
      professional_role == 9 ~ "Midwife",
      professional_role == 10 ~ "Fetal medicine midwife",
      professional_role == 11 ~ "Research midwife",
      professional_role == 12 ~ "Paeditrician",
      professional_role == 13 ~ "Neonatologist",
      professional_role == 14 ~ "General practitioner",
      professional_role == 15 ~ "Administrator",
      professional_role == 16 ~ "Data manager",
      professional_role == 17 ~ "Other"),
    
    years_experience = case_when(
      years_experience == 1 ~ "<1 year",
      years_experience == 2 ~ "1-5 years",
      years_experience == 3 ~ "6-10 years",
      years_experience == 4 ~ "11-15 years",
      years_experience == 5 ~ "16-20 years",
      years_experience == 6 ~ ">20 years"),
    
    years_experience = factor(
      years_experience,
      levels = c("<1 year",
                 "1-5 years",
                 "6-10 years",
                 "11-15 years",
                 "16-20 years",
                 ">20 years")),
    
    gmsa_region = case_when(
      gmsa_region == 1 ~ "Central and South",
      gmsa_region == 2 ~ "East",
      gmsa_region == 3 ~ "North East and Yorkshire",
      gmsa_region == 4 ~ "North Thames",
      gmsa_region == 5 ~ "North West",
      gmsa_region == 6 ~ "South East",
      gmsa_region == 7 ~ "South West",
      gmsa_region == 8 ~ "Not sure"),
    
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      gender == 3 ~ "Transgender",
      gender == 4 ~ "Non-binary",
      gender == 5 ~ "Other",
      gender == 6 ~ "Prefer not to say"),
    
    age = factor(case_when(
      age == 1 ~ "18-29",
      age == 2 ~ "30-39",
      age == 3 ~ "40-49",
      age == 4 ~ "50-59",
      age == 5 ~ "60-69",
      age == 6 ~ ">70",
      age == 7 ~ "Prefer not to say"),
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", ">70", "Prefer not to say"), exclude = NULL),
    
    # Fixed: Numbers are no longer wrapped in quotes to match an integer structure
    ethnicity = case_when(
      ethnicity == 1      ~ "Asian/Asian British",
      ethnicity == "Black" ~ "Black/Black British", # Preserved just in case it is raw text
      ethnicity == 2      ~ "Mixed/multiple",
      ethnicity == 3      ~ "White/White British",
      ethnicity == 4      ~ "Other",
      ethnicity == 5      ~ "Prefer not to say"), 
    
    ethnicity = factor(
      ethnicity,
      levels = c("White/White British",
                 "Asian/Asian British",
                 "Black/Black British",
                 "Mixed/multiple",
                 "Other",
                 "Prefer not to say")),
    
    # Fixed: Numbers are no longer wrapped in quotes
    languages = factor(case_when(
      languages == 1 ~ "Yes",
      languages == 2 ~ "No",
      languages == 3 ~ "Prefer not to say"),
      levels = c("Yes", "No", "Prefer not to say"), exclude = NULL),
    
    blood_spot_test_aware = factor(case_when(
      blood_spot_test_aware == 1 ~ "Yes",
      blood_spot_test_aware == 2 ~ "No"),
      levels = c("Yes", "No"), exclude = NULL),
    
    heard_screening_before = factor(case_when(
      heard_screening_before == 1 ~ "Yes",
      heard_screening_before == 0 ~ "No"),
      levels = c("Yes", "No"), exclude = NULL),
    
    generation_study_aware = factor(case_when(
      generation_study_aware == 1 ~ "Yes - involved",
      generation_study_aware == 2 ~ "Yes - will be involved",
      generation_study_aware == 3 ~ "Yes - am not and will not be involved",
      generation_study_aware == 4 ~ "No"),
      levels = c("Yes - involved", "Yes - will be involved", "Yes - am not and will not be involved", "No"), exclude = NULL),
    
    attended_sessions_gs = factor(case_when(
      attended_sessions_gs == 1 ~ "Yes",
      attended_sessions_gs == 2 ~ "No",
      attended_sessions_gs == 3 ~ "Do not know"),
      levels = c("Yes", "No", "Do not know"), exclude = NULL),
    
    profession_inform_gs = factor(case_when(
      profession_inform_gs == 1 ~ "Extremely important",
      profession_inform_gs == 2 ~ "Very important",
      profession_inform_gs == 3 ~ "Quite important",
      profession_inform_gs == 4 ~ "Not very important",
      profession_inform_gs == 5 ~ "Not important at all",
      profession_inform_gs == 6 ~ "Do not know",
      profession_inform_gs == 7 ~ "Prefer not to say"),
      levels = c("Extremely important", "Very important", "Quite important", "Not very important", "Not important at all", "Do not know", "Prefer not to say"), exclude = NULL),
    
    changes_workload = factor(case_when(
      changes_workload == 1 ~ "Yes, I have observed a small increase in workload",
      changes_workload == 2 ~ "Yes, I have observed a large increase in workload",
      changes_workload == 3 ~ "Yes, I have observed a small decrease in workload",
      changes_workload == 4 ~ "Yes, I have observed a large decrease in workload",
      changes_workload == 5 ~ "No, I have not observed any changes in workload",
      changes_workload == 6 ~ "Don't know",
      changes_workload == 7 ~ "Prefer not to say"),
      levels = c("Yes, I have observed a small increase in workload",
                 "Yes, I have observed a large increase in workload",
                 "Yes, I have observed a small decrease in workload",
                 "Yes, I have observed a large decrease in workload",
                 "No, I have not observed any changes in workload",
                 "Don't know",
                 "Prefer not to say"), exclude = NULL),
    
    expect_workload = factor(case_when(
      expect_workload == 1 ~ "Yes, I expect there to be a small increase in workload",
      expect_workload == 2 ~ "Yes, I expect there to be a large increase in workload",
      expect_workload == 3 ~ "Yes, I expect there to be a small decrease in workload",
      expect_workload == 4 ~ "Yes, I expect there to be a large decrease in workload",
      expect_workload == 5 ~ "No, I would not expect any changes to workload",
      expect_workload == 6 ~ "Don't know",
      expect_workload == 7 ~ "Prefer not to say"),
      levels = c("Yes, I expect there to be a small increase in workload",
                 "Yes, I expect there to be a large increase in workload",
                 "Yes, I expect there to be a small decrease in workload",
                 "Yes, I expect there to be a large decrease in workload",
                 "No, I would not expect any changes to workload",
                 "Don't know",
                 "Prefer not to say"), exclude = NULL),
    
    involved_gs = factor(case_when(
      involved_gs == 1 ~ "Yes",
      involved_gs == 2 ~ "No",
      involved_gs == 3 ~ "Not sure"),
      levels = c("Yes", "No", "Not sure"), exclude = NULL)
  ) %>%
  
  # 3. Handle Batch Vector Transformations (Confidence & Attitudes)
  mutate(across(all_of(c(vars_nbs, vars_gs, vars_conf)), ~ factor(
    case_when(
      .x == 1 ~ "Not at all confident",
      .x == 2 ~ "Not confident",
      .x == 3 ~ "Neutral- neither confident nor not confident",
      .x == 4 ~ "Confident",
      .x == 5 ~ "Strongly confident",
      .x == 6 ~ "Not relevant to my role"
    ),
    levels = c(
      "Not at all confident",
      "Not confident",
      "Neutral- neither confident nor not confident",
      "Confident",
      "Strongly confident",
      "Not relevant to my role"
    ), exclude = NULL))) %>%
  
  mutate(across(all_of(vars_attitude), ~ factor(
    case_when(
      .x == 1 ~ "1",
      .x == 2 ~ "2",
      .x == 3 ~ "3",
      .x == 4 ~ "4",
      .x == 5 ~ "5"
    ),
    levels = c("1", "2", "3", "4", "5")))) %>%
  
  mutate(across(all_of(vars_accept), ~ factor(
    case_when(
      .x == 1 ~ "Completely disagree",
      .x == 2 ~ "Disagree",
      .x == 3 ~ "Neither agree nor disagree",
      .x == 4 ~ "Agree",
      .x == 5 ~ "Completely agree"
    ),
    levels = c("Completely disagree", "Disagree", "Neither agree nor disagree",
               "Agree", "Completely agree")))) %>%
  
  mutate(across(all_of(vars_benefit),
                ~ factor(.x,
                         levels = 1:10,
                         labels = c("Not a benefit", "2", "3", "4", "5",
                                    "6", "7", "8", "9", "Extremely important benefit"),
                         ordered = TRUE))) %>%
  
  mutate(across(all_of(vars_risk),
                ~ factor(.x,
                         levels = 1:10,
                         labels = c(
                           "Not a risk", "2", "3", "4", "5",
                           "6", "7", "8", "9", "Extremely important risk"),
                         ordered = TRUE))) %>%
  
  # 4. Coalesce Specialty Fields & Column Selection
  mutate(spec = coalesce(cns_spec, sub_spec, role_other),
         spec = na_if(spec, "")) %>%
  select(-redcap_survey_identifier, -my_first_instrument_timestamp, -cns_spec, -sub_spec, -role_other,
         -gender_other, -ethnicity_other) %>%
  select(record_id, consent, professional_role, spec, everything())
