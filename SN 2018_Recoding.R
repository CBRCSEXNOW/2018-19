### ====================================================
### DATA CLEANING
### ====================================================
# Load packages and set working directory
# ======================================================
library(tidyverse)
library(dplyr)
library(readxl)

# ======================================================
# Read in data; for now, treat all columns as text since some numeric columns have (invalid) text data
# ======================================================
raw_df <- readxl::read_xlsx(path = "\\2018 Sex Now Data Cleaning\\SN 2018_Raw Data.xlsx", sheet = "Data", col_names = TRUE, col_types = "text")

# ----------------------
# Merge with edited data frame (i.e., recodes of other)
# ----------------------
backcoded_df <- readxl::read_xlsx(path = "\\2018 Sex Now Data Cleaning\\SN 2018_Edited Data.xlsx", col_names = TRUE, col_types = "text")

merged_df <- backcoded_df %>%
  left_join(y = raw_df, by = "Q0_anon_code", suffix = c("", "_delete")) %>%
  select_if(.predicate = !(str_detect(string = colnames(.), pattern = "_delete"))) %>%
  bind_rows(
    filter(raw_df, !(Q0_anon_code %in% .$Q0_anon_code))
  )

df <- merged_df

# ----------------------
# Merge with confirmed lab data frame (i.e., reactives confirmed)
# ----------------------
revised_lab_df <- readxl::read_xlsx(path = "\\2018 Sex Now Data Cleaning\\SN 2018_HIV Reactives.xlsx", col_names = TRUE, col_types = "text")

merged_lab_df <- df %>%
  left_join(y = revised_lab_df , by = "Q0_anon_code", suffix = c("", "_delete")) %>%
  select_if(.predicate = !(str_detect(string = colnames(.), pattern = "_delete"))) %>%
  bind_rows(
    filter(raw_df, !(Q0_anon_code %in% .$Q0_anon_code))
  )

df <- merged_lab_df

df$Q0_eligible <- NA
df$Q2_ethnicity_other <- NA
df$Q7_sex_orientation_other <- NA
df$Q25_delays_other <- NA
df$Q26_STI_other <- NA
df$Q52_PrEP_not_interested_other <- NA
df$Q53_HIRI_total_calculated <- NA
df$Q57_depression <- NA
df$Q57_anxiety <- NA
df$Q58_want_help_other <- NA
df$Q61_substances_none <- NA
df$Q63_substance_services_other <- NA
df$Q81_enjoy_most_other <- NA
df$W9_delay_HIV_other <- NA
df$W11_why_substances_other <- NA
df$Q0_has_dbs_labs <- NA
df$W18_apps_other <- NA
df$Q21_recent_sex_other <- NA

# ======================================================
# Eliminate Inelgible Participants
# ======================================================
# ----------------------
# Remove People not 16 Years or Older
# ----------------------
summary(df$Q11_age)
table(df$Q11_age)
df$Q11_age[df$Q11_age == "35-50"] <- 42
df$Q11_age[df$Q11_age == "40+"] <- 40
df$Q11_age[df$Q11_age == "36 (illegible)"] <- 36
df$Q11_age[df$Q11_age == "41.5"] <- 41
df$Q11_age[is.na(df$Q11_age)] <- "9999: True Missing"
df$Q11_age[df$Q11_age == "0"] <- "7777: Poor Data Quality"
df$Q11_age[df$Q11_age == "10"] <- "7777: Poor Data Quality"
df$Q0_eligible <- NA
df$Q0_eligible[df$Q11_age < 16] <- "No"

# ----------------------
# Remove People not living in Canada
# ----------------------
df$Q0_eligible[df$Q1_province == "I don't live in Canada"] <- "No"

# ----------------------
# Remove Non-MSM
# ----------------------
table(df$Q7_sex_orientation_straight)
table(df$Q77_sex_w_men_number)
table(df$Q19_num_sex_partners_6_months)
table(df$Q76_sex_w_men_ever)
summary(df$Q77_sex_w_men_number)

df$Q19_num_sex_partners_6_months[df$Q19_num_sex_partners_6_months == "30-40"] <- 35
df$Q77_sex_w_men_number[df$Q19_num_sex_partners_6_months == 0 ] <- 0
df$Q77_sex_w_men_number[df$Q76_sex_w_men_ever == "I've never had sex with a man (skip to #101)"] <- 0
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "NONE" | df$Q77_sex_w_men_number == "none" | df$Q77_sex_w_men_number == "No" | df$Q77_sex_w_men_number == "N/A"] <- 0
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "1-2" | df$Q77_sex_w_men_number == "1/2??"] <- 2
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "1 (illegible)"] <- 1
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "10-15"] <- 12
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "15 - 16" | df$Q77_sex_w_men_number == "15+"] <- 15
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "100+"] <- 100
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "2-5"] <- 3
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "3-4"] <- 3
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "6-8"] <- 7
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "5-6" | df$Q77_sex_w_men_number == "5 or 6"] <- 5
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "30+"] <- 30
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "40-50"] <- 45
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "40+"] <- 40
df$Q77_sex_w_men_number[df$Q77_sex_w_men_number == "a few" | df$Q77_sex_w_men_number == "a lot" | df$Q77_sex_w_men_number == "lots" | df$Q77_sex_w_men_number == "A lot" ] <- 50

df$Q76_sex_w_men_ever[df$Q76_sex_w_men_ever == ".... years old"] <- "Yes"
df$Q76_sex_w_men_ever[df$Q76_sex_w_men_ever == "I've never had sex with a man (skip to #101)"] <- "No"
df$Q76_sex_w_men_ever[is.na(df$Q76_sex_w_men_ever)] <- "9999: True Missing"

df$Q0_eligible[df$Q7_sex_orientation_straight == "Straight" &  df$Q76_sex_w_men_ever == "No"] <- "No"

# ----------------------
# Remove Observations of Women
# ----------------------
table(df$Q9_gender_ID)
table(df$Q9_gender_ID_other_text)
df$Q0_eligible[df$Q9_gender_ID == "Woman"] <- "No"


# ----------------------
# Remove Observations without Surveys
# ----------------------
df$Q0_has_survey <- NA
df$Q0_has_survey[is.na(df$Q0_surveymonkey_ID)] <- "No"
df$Q0_has_survey[!is.na(df$Q0_surveymonkey_ID)] <- "Yes"
table(df$Q0_has_survey)
df$Q0_eligible[df$Q0_has_survey == "No"] <- "No"

# ======================================================
# Filter out inelgible Surveys
# ======================================================
df$Q0_eligible[is.na(df$Q0_eligible)] <- "Yes"
table(df$Q0_eligible)
df <- filter(.data = df, Q0_eligible == "Yes")
uncoded_df <- df

# ======================================================
# Q0_site
# ======================================================
table(df$Q0_site)

# ======================================================
# Q1_province
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q1_province)

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q1_province[df$Q1_province == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Missing
# ----------------------
df$Q1_province[is.na(df$Q1_province)] <- "9999: True Missing"

# ======================================================
# Q0_language
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q0_language)

# ----------------------
# Missing
# ----------------------
df$Q0_language[is.na(df$Q0_language)] <- "9999: True Missing"



# ======================================================
# Q2_ethnicity
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q2_ethnicity_african)
table(df$Q2_ethnicity_arab)
table(df$Q2_ethnicity_asian)
table(df$Q2_ethnicity_indigenous)
table(df$Q2_ethnicity_latin)
table(df$Q2_ethnicity_south_asian)
table(df$Q2_ethnicity_white)
table(df$Q2_ethnicity_other_text)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q2_ethnicity_other <- NA
df$Q2_ethnicity_other[is.na(df$Q2_ethnicity_other_text)] <- "No"
df$Q2_ethnicity_other[!is.na(df$Q2_ethnicity_other_text)] <- "Yes"
table(df$Q2_ethnicity_other)
df$Q2_ethnicity_other_text[df$Q2_ethnicity_other == "No"] <- "8888: Other Not Selected"
df$Q2_ethnicity_other_text[is.na(df$Q2_ethnicity_other_text) & df$Q2_ethnicity_other == "Yes"] <- "9999: True Missing"
table(df$Q2_ethnicity_other)

# ----------------------
# Imputation: If a respondent answered Q3 with anything other than "None", then add "Indigenous" for Q2
#             If a respondent answered Q6 with "Yes", then add "Indigenous" for Q2
# ----------------------
df$Q2_ethnicity_indigenous[is.na(df$Q2_ethnicity_indigenous) & df$Q3_indigenous %in% c("Métis", "First Nations", "Inuk")] <- "Indigenous"
df$Q2_ethnicity_indigenous[is.na(df$Q2_ethnicity_indigenous) & df$Q6_indigenous_status == "Yes"] <- "Indigenous"
df$Q2_ethnicity_indigenous[is.na(df$Q2_ethnicity_indigenous) & df$Q6_indigenous_status == "Yes"] <- "Indigenous"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q2_ethnicity_valid <- NA
df$Q2_ethnicity_valid[df$Q2_ethnicity_african == "African, Caribbean, Black" |
                        df$Q2_ethnicity_arab == "Arab, West Asian (e.g. Iranian, Afghan)" |
                        df$Q2_ethnicity_asian == "East or Southeast Asian (e.g. Chinese, Japanese, Korean)" |
                        df$Q2_ethnicity_indigenous == "Indigenous" |
                        df$Q2_ethnicity_latin == "Latin American, Hispanic" |
                        df$Q2_ethnicity_south_asian == "South Asian (e.g. East Indian, Pakistani, Sri Lankan)" |
                        df$Q2_ethnicity_white == "White" |
                        df$Q2_ethnicity_other == "Yes"] <- "Yes"
df$Q2_ethnicity_valid[is.na(df$Q2_ethnicity_valid)] <- "No"
table(df$Q2_ethnicity_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q2_ethnicity_african[df$Q2_ethnicity_african == "African, Caribbean, Black"] <- "Yes"
df$Q2_ethnicity_arab[df$Q2_ethnicity_arab == "Arab, West Asian (e.g. Iranian, Afghan)"] <- "Yes"
df$Q2_ethnicity_asian[df$Q2_ethnicity_asian == "East or Southeast Asian (e.g. Chinese, Japanese, Korean)"] <- "Yes"
df$Q2_ethnicity_indigenous[df$Q2_ethnicity_indigenous == "Indigenous"] <- "Yes"
df$Q2_ethnicity_latin[df$Q2_ethnicity_latin == "Latin American, Hispanic"] <- "Yes"
df$Q2_ethnicity_south_asian[df$Q2_ethnicity_south_asian == "South Asian (e.g. East Indian, Pakistani, Sri Lankan)"] <- "Yes"
df$Q2_ethnicity_white[df$Q2_ethnicity_white == "White"] <- "Yes"
df$Q2_ethnicity_other[df$Q2_ethnicity_other == "Yes"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q2_ethnicity_african[is.na(df$Q2_ethnicity_african) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_arab[is.na(df$Q2_ethnicity_arab) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_asian[is.na(df$Q2_ethnicity_asian) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_indigenous[is.na(df$Q2_ethnicity_indigenous) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_latin[is.na(df$Q2_ethnicity_latin) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_south_asian[is.na(df$Q2_ethnicity_south_asian) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_white[is.na(df$Q2_ethnicity_white) & df$Q2_ethnicity_valid == "Yes"] <- "No"
df$Q2_ethnicity_other[is.na(df$Q2_ethnicity_other) & df$Q2_ethnicity_valid == "Yes"] <- "No"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q2_ethnicity_african[is.na(df$Q2_ethnicity_african) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_arab[is.na(df$Q2_ethnicity_arab) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_asian[is.na(df$Q2_ethnicity_asian) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_indigenous[is.na(df$Q2_ethnicity_indigenous) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_latin[is.na(df$Q2_ethnicity_latin) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_south_asian[is.na(df$Q2_ethnicity_south_asian) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_white[is.na(df$Q2_ethnicity_white) & df$Q2_ethnicity_valid == "No"] <- "9999: True Missing"
df$Q2_ethnicity_other[df$Q2_ethnicity_white == "9999: True Missing"] <- "9999: True Missing" ##Correct for the way this was constructed by back coding the missingness using missingness in last variable


# ======================================================
# Q3_indigenous
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q3_indigenous)

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q3_indigenous[df$Q3_indigenous == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# If respondent did not select "Indigenous" in Q2 above, code as 8888: Not eligible
# ----------------------
df$Q3_indigenous[df$Q2_ethnicity_indigenous != "Yes"] <- "8888: Not Indigenous"

# ----------------------
# Recode "None" as Not Indigenous
# ----------------------
df$Q3_indigenous[df$Q3_indigenous == "None"] <- "8888: Not Indigenous"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q3_indigenous[is.na(df$Q3_indigenous)] <- "9999: True Missing"


# ======================================================
# Q4_two_spirit
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q4_two_spirit)

# ----------------------
# Code Non-Indigenous Persons as Not Two-Spirited
# ----------------------
df$Q4_two_spirit[df$Q2_ethnicity_indigenous == "No"] <- "8888: Not Indigenous"
df$Q4_two_spirit[df$Q3_indigenous == "No"] <- "8888: Not Indigenous"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q4_two_spirit[df$Q4_two_spirit == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# E0206 wrote "Two-spirited" in the "other" field in Q7. Code Q4 as "Yes"
# ----------------------
df$Q4_two_spirit[df$Q0_anon_code == "E0206"] <- "Yes"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q4_two_spirit[is.na(df$Q4_two_spirit)] <- "9999: True Missing"

# ======================================================
# Q5_indigenous_community
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q5_indigenous_community)

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q5_indigenous_community[df$Q5_indigenous_community == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Recode Non-indigenous respondents as Not Indigenous
# ----------------------
df$Q5_indigenous_community[df$Q3_indigenous == "8888: Not Indigenous"] <- "8888: Not Indigenous"
df$Q5_indigenous_community[df$Q2_ethnicity_indigenous == "No"] <- "8888: Not Indigenous"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q5_indigenous_community[is.na(df$Q5_indigenous_community)] <- "9999: True Missing"


# ======================================================
# Q6_indigenous_status
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q6_indigenous_status)
# ----------------------
# Recode Non-indigenous respondents as Not Indigenous
# ----------------------
df$Q6_indigenous_status[df$Q3_indigenous == "8888: Not Indigenous"] <- "8888: Not Indigenous"
df$Q6_indigenous_status[df$Q2_ethnicity_indigenous == "No"] <- "8888: Not Indigenous"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q6_indigenous_status[df$Q6_indigenous_status == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Recode Not Applicable to No
# ----------------------
df$Q6_indigenous_status[df$Q6_indigenous_status == "Not applicable"] <- "No"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q6_indigenous_status[is.na(df$Q6_indigenous_status)] <- "9999: True Missing"

# ======================================================
# Q7_sex_orientation
# ======================================================
table(df$Q7_sex_orientation_gay)
table(df$Q7_sex_orientation_asexual)
table(df$Q7_sex_orientation_straight)
table(df$Q7_sex_orientation_bisexual)
table(df$Q7_sex_orientation_pansexual)
table(df$Q7_sex_orientation_queer)
table(df$Q7_sex_orientation_heteroflexible)
table(df$Q7_sex_orientation_other_text)
# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q7_sex_orientation_other <- NA
df$Q7_sex_orientation_other[!is.na(df$Q7_sex_orientation_other_text)] <- "Yes"
df$Q7_sex_orientation_other[is.na(df$Q7_sex_orientation_other_text)] <- "No"
table(df$Q7_sex_orientation_other)


df$Q7_sex_orientation_other_text[df$Q7_sex_orientation_other == "No"] <- "8888: Other Not Selected"
df$Q7_sex_orientation_other_text[is.na(df$Q7_sex_orientation_other_text) & df$Q7_sex_orientation_other == "Yes"] <- "9999: True Missing"
table(df$Q7_sex_orientation_other_text)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q2_sex_orientation_valid <- NA
df$Q2_sex_orientation_valid[df$Q7_sex_orientation_gay == "Gay" |
                              df$Q7_sex_orientation_asexual == "Asexual" |
                              df$Q7_sex_orientation_straight == "Straight" |
                              df$Q7_sex_orientation_bisexual == "Bi (bisexual)" |
                              df$Q7_sex_orientation_pansexual == "Pansexual" |
                              df$Q7_sex_orientation_queer == "Queer" |
                              df$Q7_sex_orientation_heteroflexible == "Heteroflexible" |
                              df$Q7_sex_orientation_other == "Yes"] <- "Yes"
df$Q2_sex_orientation_valid[is.na(df$Q2_sex_orientation_valid)] <- "No"
table(df$Q2_sex_orientation_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q7_sex_orientation_gay[df$Q7_sex_orientation_gay == "Gay"] <- "Yes"
df$Q7_sex_orientation_asexual[df$Q7_sex_orientation_asexual == "Asexual"] <- "Yes"
df$Q7_sex_orientation_straight[df$Q7_sex_orientation_straight == "Straight"] <- "Yes"
df$Q7_sex_orientation_bisexual[df$Q7_sex_orientation_bisexual == "Bi (bisexual)"] <- "Yes"
df$Q7_sex_orientation_pansexual[df$Q7_sex_orientation_pansexual == "Pansexual"] <- "Yes"
df$Q7_sex_orientation_queer[df$Q7_sex_orientation_queer == "Queer"] <- "Yes"
df$Q7_sex_orientation_heteroflexible[df$Q7_sex_orientation_heteroflexible == "Heteroflexible"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q7_sex_orientation_gay[is.na(df$Q7_sex_orientation_gay) & df$Q2_sex_orientation_valid == "Yes"] <- "No"
df$Q7_sex_orientation_asexual[is.na(df$Q7_sex_orientation_asexual) & df$Q2_sex_orientation_valid == "Yes"] <- "No"
df$Q7_sex_orientation_straight[is.na(df$Q7_sex_orientation_straight) & df$Q2_sex_orientation_valid == "Yes"] <- "No"
df$Q7_sex_orientation_bisexual[is.na(df$Q7_sex_orientation_bisexual) & df$Q2_sex_orientation_valid == "Yes"] <- "No"
df$Q7_sex_orientation_pansexual[is.na(df$Q7_sex_orientation_pansexual) & df$Q2_sex_orientation_valid == "Yes"] <- "No"
df$Q7_sex_orientation_queer[is.na(df$Q7_sex_orientation_queer) & df$Q2_sex_orientation_valid == "Yes"] <- "No"
df$Q7_sex_orientation_heteroflexible[is.na(df$Q7_sex_orientation_heteroflexible) & df$Q2_sex_orientation_valid == "Yes"] <- "No"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q7_sex_orientation_gay[is.na(df$Q7_sex_orientation_gay) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_asexual[is.na(df$Q7_sex_orientation_asexual) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_straight[is.na(df$Q7_sex_orientation_straight) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_bisexual[is.na(df$Q7_sex_orientation_bisexual) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_pansexual[is.na(df$Q7_sex_orientation_pansexual) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_queer[is.na(df$Q7_sex_orientation_queer) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_heteroflexible[is.na(df$Q7_sex_orientation_heteroflexible) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"
df$Q7_sex_orientation_other[is.na(df$Q7_sex_orientation_heteroflexible) & df$Q2_sex_orientation_valid == "No"] <- "9999: True Missing"


# ======================================================
# Q8_out
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q8_out)

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q8_out[is.na(df$Q8_out)] <- "9999: True Missing"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q8_out[df$Q8_out == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q8_out[df$Q8_out == "Not at all open (out) 1"] <- "1 - Not at all open (out)"
df$Q8_out[df$Q8_out == "2"] <- "2 - "
df$Q8_out[df$Q8_out == "3"] <- "3 - "
df$Q8_out[df$Q8_out == "4"] <- "4 - "
df$Q8_out[df$Q8_out == "Open (out) to all or most people I know 5"] <- "5 - Open to all or most people I know"

# ======================================================
# Q9_gender
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q9_gender_ID)
table(df$Q9_gender_ID_other_text)

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q9_gender_ID[is.na(df$Q9_gender_ID)] <- "9999: True Missing"

# ----------------------
# For respondents who selected multiple responses, code as "Neither. I prefer to self-describe as"
# ----------------------
df$Q9_gender_ID[df$Q9_gender_ID == "Multiple answers selected"] <- "Neither. I prefer to self-describe as"
df$Q9_gender_ID[df$Q9_gender_ID == "Neither. I prefer to self-describe as"] <- "Other"

df$Q9_gender_ID_other_text[df$Q9_gender_ID == "Other" & is.na(df$Q9_gender_ID_other_text) ] <- "9999: True Missing"
df$Q9_gender_ID_other_text[df$Q9_gender_ID != "Other" & is.na(df$Q9_gender_ID_other_text) ] <- "8888: Other Not Selected"




# ======================================================
# Q10_trans
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q10_trans)

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q10_trans[is.na(df$Q10_trans)] <- "9999: True Missing"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q10_trans[df$Q10_trans == "Multiple answers selected"] <- "7777: Poor Data Quality"


# ======================================================
# Q11_age
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q11_age)

# ======================================================
# Q12_education
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q12_education)

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q12_education[df$Q12_education == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q12_education[is.na(df$Q12_education)] <- "9999: True Missing"



# ======================================================
# Q13_born_Canada
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q13_born_Canada)

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q13_born_Canada[is.na(df$Q13_born_Canada)] <- "9999: True Missing"

# ======================================================
# Q14_FSA
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q14_FSA)

# ----------------------
# Address Data Quality Issues
# ----------------------
df$Q14_FSA[df$Q14_FSA == "?" | df$Q14_FSA == "H [ILLEGIBLE]"] <- "7777: Poor Data Quality"
df$Q14_FSA[df$Q14_FSA == "0A4" | 
             df$Q14_FSA == "0E1" |
             df$Q14_FSA == "0E7" |
             df$Q14_FSA == "0K3" |
             df$Q14_FSA == "0M3" |
             df$Q14_FSA == "198" |
             df$Q14_FSA == "1C7" |
             df$Q14_FSA == "1H1" |
             df$Q14_FSA == "1H9" |
             df$Q14_FSA == "1M0" |
             df$Q14_FSA == "1J5" |
             df$Q14_FSA == "1N3" |
             df$Q14_FSA == "1N6" |
             df$Q14_FSA == "1N8" |
             df$Q14_FSA == "1R7" |
             df$Q14_FSA == "1V3" |
             df$Q14_FSA == "1X2" |
             df$Q14_FSA == "1X8" |
             df$Q14_FSA == "1X9" |
             df$Q14_FSA == "1Z1" |
             df$Q14_FSA == "227" |
             df$Q14_FSA == "2C8" |
             df$Q14_FSA == "2G5" |
             df$Q14_FSA == "2H5" |
             df$Q14_FSA == "2K6" |
             df$Q14_FSA == "2L4" |
             df$Q14_FSA == "2L8" |
             df$Q14_FSA == "2P9" |
             df$Q14_FSA == "2TW" |
             df$Q14_FSA == "2W5" |
             df$Q14_FSA == "3A4" |
             df$Q14_FSA == "3A7" |
             df$Q14_FSA == "3AB" |
             df$Q14_FSA == "3B2" |
             df$Q14_FSA == "3G7" |
             df$Q14_FSA == "3H3" |
             df$Q14_FSA == "3J6" |
             df$Q14_FSA == "3A7" |
             df$Q14_FSA == "3AB" |
             df$Q14_FSA == "3B2" |
             df$Q14_FSA == "3G7" |
             df$Q14_FSA == "3H3" |
             df$Q14_FSA == "3J6" |
             df$Q14_FSA == "3K3" |
             df$Q14_FSA == "3M2" |
             df$Q14_FSA == "3W3" |
             df$Q14_FSA == "3X5" |
             df$Q14_FSA == "3Y4" |
             df$Q14_FSA == "3Z1" |
             df$Q14_FSA == "4E4" |
             df$Q14_FSA == "4M4" |
             df$Q14_FSA == "63" |
             df$Q14_FSA == "1R0" |
             df$Q14_FSA == "140" |
             df$Q14_FSA == "4T4" |
             df$Q14_FSA == "4X3" |
             df$Q14_FSA == "5K1" |
             df$Q14_FSA == "5N8" |
             df$Q14_FSA == "5R9" |
             df$Q14_FSA == "69T" |
             df$Q14_FSA == "6P5" |
             df$Q14_FSA == "6R2" |
             df$Q14_FSA == "6VG" |
             df$Q14_FSA == "7B1" |
             df$Q14_FSA == "7B4"] <- "7777: Poor Data Quality"
df$Q14_FSA[df$Q14_FSA == "M6P 3J4"] <- "M6P"
df$Q14_FSA[df$Q14_FSA == "H4A 3R5"] <- "H4A"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q14_FSA[is.na(df$Q14_FSA)] <- "9999: True Missing"


# ======================================================
# Q15_money
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q15_money)


# ----------------------
# Clarify Ordering
# ----------------------
df$Q15_money[df$Q15_money == "Comfortable, with extra"] <- "4 - Comfortable, with extra"
df$Q15_money[df$Q15_money == "Enough, but no extra"] <- "3 - Enough, but no extra"
df$Q15_money[df$Q15_money == "Have to cut back"] <- "2 - Have to cut back"
df$Q15_money[df$Q15_money == "Cannot make ends meet"] <- "1 - Cannot make ends meet"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q15_money[df$Q15_money == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q15_money[is.na(df$Q15_money)] <- "9999: True Missing"


# ======================================================
# Q16_relationship
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q16_relationship)

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q16_relationship[df$Q16_relationship == "Multiple answers selected" ] <- "7777: Poor Data Quality"

# ----------------------
# Rename Level
# ----------------------
df$Q16_relationship[df$Q16_relationship == "No (skip to #18)"] <- "No, Single"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q16_relationship[is.na(df$Q16_relationship) | df$Q16_relationship == "Prefer not to answer"] <- "9999: True Missing"


# ======================================================
# Q16_poly_num_men, Q16_poly_num_women, Q16_poly_num_non_bin
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q16_poly_num_men)
table(df$Q16_poly_num_women)
table(df$Q16_poly_num_non_bin)


# ----------------------
# Account for Skip Logic
# ----------------------
df$Q16_poly_num_men[df$Q16_relationship != "Yes, with more than 1 person (polyamorous)."] <- "8888: Not in a Polyamourous Relationship"
df$Q16_poly_num_women[df$Q16_relationship != "Yes, with more than 1 person (polyamorous)."] <- "8888: Not in a Polyamourous Relationship"
df$Q16_poly_num_non_bin[df$Q16_relationship != "Yes, with more than 1 person (polyamorous)."] <- "8888: Not in a Polyamourous Relationship"

# ----------------------
# Data cleaning
# ----------------------
df$Q16_poly_num_women[df$Q16_poly_num_women == "infinity symbol" ] <- "7777: Poor Data Quality"

# ----------------------
# Data cleaning
# ----------------------
df$Q16_poly_num_men[is.na(df$Q16_poly_num_men)] <- "9999: True Missing"
df$Q16_poly_num_women[is.na(df$Q16_poly_num_women)] <- "9999: True Missing"
df$Q16_poly_num_non_bin[is.na(df$Q16_poly_num_non_bin)] <- "9999: True Missing"

# ======================================================
# Q17_relationship_exclusive
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q17_relationship_exclusive)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q17_relationship_exclusive[df$Q16_relationship == "No, Single"] <- "8888: Not in a Relationship"

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q17_relationship_exclusive[df$Q17_relationship_exclusive == "Multiple answers selected" ] <- "7777: Poor Data Quality"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q17_relationship_exclusive[is.na(df$Q17_relationship_exclusive)] <- "9999: True Missing"

# ======================================================
# Q18_sex_with_women
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q18_sex_with_women)

# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q18_sex_with_women[df$Q18_sex_with_women == "Multiple answers selected" ] <- "7777: Poor Data Quality"
df$Q18_sex_with_women[df$Q20_newest_partner_when == "I have never had sex" 
                                 & (df$Q18_sex_with_women != "No, never" |
                                      df$Q18_sex_with_women != "Unsure")] <- "7777: Poor Data Quality"

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q18_sex_with_women[is.na(df$Q18_sex_with_women)] <- "9999: True Missing"


# ======================================================
# Q19_num_sex_partners_3_months, Q19_num_sex_partners_6_months
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q19_num_sex_partners_6_months)
table(df$Q19_num_sex_partners_3_months)

# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q19_num_sex_partners_3_months[is.na(df$Q19_num_sex_partners_3_months)] <- "9999: True Missing"
df$Q19_num_sex_partners_6_months[is.na(df$Q19_num_sex_partners_6_months)] <- "9999: True Missing"

# ----------------------
# If no partners in past 6 months then no partners in past three months
# ----------------------
df$Q19_num_sex_partners_3_months[df$Q19_num_sex_partners_6_months == 0] <- 0

# ----------------------
# Manage Conflicts with "I have never had sex" in Q20_newest_partner_when
# ----------------------
df$Q19_num_sex_partners_3_months[df$Q20_newest_partner_when == "I have never had sex" 
                                 & df$Q19_num_sex_partners_3_months != 0] <- "7777: Poor Data Quality"

df$Q19_num_sex_partners_6_months[df$Q20_newest_partner_when == "I have never had sex" 
                                 & df$Q19_num_sex_partners_6_months != 0] <- "7777: Poor Data Quality"

df$Q19_num_sex_partners_3_months[df$Q21_recent_sex_none == "None of the above. I have not had sex in the past 6 months."
                                 & df$Q19_num_sex_partners_3_months != 0] <- "7777: Poor Data Quality"

df$Q19_num_sex_partners_6_months[df$Q21_recent_sex_none == "None of the above. I have not had sex in the past 6 months."
                                 & df$Q19_num_sex_partners_6_months != 0] <- "7777: Poor Data Quality"

# ----------------------
# Deal with conflicts between 6 months and 3 month reports.
# ----------------------

df$Q19_num_sex_partners_6_months[df$Q19_num_sex_partners_3_months > df$Q19_num_sex_partners_6_months
                                 & !is.na(df$Q19_num_sex_partners_6_months) 
                                 & !is.na(df$Q19_num_sex_partners_3_months)] <- "7777: Poor Data Quality"

df$Q19_num_sex_partners_3_months[df$Q19_num_sex_partners_3_months > df$Q19_num_sex_partners_6_months
                                 & !is.na(df$Q19_num_sex_partners_6_months) 
                                 & !is.na(df$Q19_num_sex_partners_3_months)] <- "7777: Poor Data Quality"

# ======================================================
# Q20_newest_partner_when
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q20_newest_partner_when)
# ----------------------
# If a respondent selected multiple answers, code as 7777: Poor Data Quality
# ----------------------
df$Q20_newest_partner_when[df$Q20_newest_partner_when == "Multiple answers selected" | df$Q20_newest_partner_when == "\"no new\""] <- "7777: Poor Data Quality"
# ----------------------
# Replace blanks with code 9999: true missing
# ----------------------
df$Q20_newest_partner_when[is.na(df$Q20_newest_partner_when)] <- "9999: True Missing"
# ----------------------
# Data Quality for "I have never had sex"
# ----------------------
df$Q20_newest_partner_when[df$Q20_newest_partner_when == "I have never had sex" &
                           (df$Q19_num_sex_partners_6_months != 0 | 
                           df$Q19_num_sex_partners_3_months != 0 |
                           df$Q18_sex_with_women == "Yes, in the past 6 months" |
                             df$Q18_sex_with_women == "Yes, but longer than 6 months ago")] <- "7777: Poor Data Quality"

# ======================================================
# Q21 Sexual behaviours
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q21_recent_sex_mutual_mast)
table(df$Q21_recent_sex_oral)
table(df$Q21_recent_sex_fingering)
table(df$Q21_recent_sex_fisting)
table(df$Q21_recent_sex_anal_bottom_condom)
table(df$Q21_recent_sex_anal_bottom_no_condom)
table(df$Q21_recent_sex_anal_top_condom)
table(df$Q21_recent_sex_anal_top_no_condom)
table(df$Q21_recent_sex_resp_vagina_condom)
table(df$Q21_recent_sex_resp_vagina_no_condom)
table(df$Q21_recent_sex_partner_vagina_condom)
table(df$Q21_recent_sex_partner_vagina_no_condom)
table(df$Q21_recent_sex_toys)
table(df$Q21_recent_sex_online)
table(df$Q21_recent_sex_threesome)
table(df$Q21_recent_sex_group)
table(df$Q21_recent_sex_none)
table(df$Q21_recent_sex_other_text)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q21_recent_sex_other <- NA
df$Q21_recent_sex_other[is.na(df$Q21_recent_sex_other_text)] <- "No"
df$Q21_recent_sex_other[!is.na(df$Q21_recent_sex_other_text)] <- "Yes"
table(df$Q21_recent_sex_other)

df$Q21_recent_sex_other_text[df$Q21_recent_sex_other == "No"] <- "8888: Other Not Selected"
df$Q21_recent_sex_other_text[is.na(df$Q21_recent_sex_other_text) & df$Q21_recent_sex_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q21_recent_sex_valid <- NA
df$Q21_recent_sex_none[df$Q19_num_sex_partners_6_months == "0"] <- "7777: Poor Data Quality"

df$Q21_recent_sex_valid[df$Q21_recent_sex_mutual_mast == "Mutual masturbation" |
                          df$Q21_recent_sex_oral == "Oral sex" |
                          df$Q21_recent_sex_fingering == "Fingering (sex using fingers)" |
                          df$Q21_recent_sex_fisting == "Fisting (sex using fists)" |
                          df$Q21_recent_sex_anal_bottom_condom == "Anal sex as bottom (receptive partner) with a condom" |
                          df$Q21_recent_sex_anal_bottom_no_condom == "Anal sex as bottom (receptive partner) without a condom" |
                          df$Q21_recent_sex_anal_top_condom == "Anal sex as top (insertive partner) with a condom" |
                          df$Q21_recent_sex_anal_top_no_condom == "Anal sex as top (insertive partner) without a condom" |
                          df$Q21_recent_sex_resp_vagina_condom == "Sex in my vagina or internal genitals with a condom" |
                          df$Q21_recent_sex_resp_vagina_no_condom == "Sex in my vagina or internal genitals without a condom" |
                          df$Q21_recent_sex_partner_vagina_condom == "Sex in my partner's vagina or internal genitals with a condom" |
                          df$Q21_recent_sex_partner_vagina_no_condom == "Sex in my partner's vagina or internal genitals without a condom" |
                          df$Q21_recent_sex_toys == "Sex with prosthetics or sex toys" |
                          df$Q21_recent_sex_online == "Online sex (camming, sexting)" |
                          df$Q21_recent_sex_threesome == "Threesome (sex between 3 people)" |
                          df$Q21_recent_sex_group == "Group sex (sex between 4+ people)" |
                          df$Q21_recent_sex_none == "None of the above. I have not had sex in the past 6 months." |
                          df$Q21_recent_sex_none == "7777: Poor Data Quality" |
                          df$Q21_recent_sex_other == "Yes"] <- "Yes"
df$Q21_recent_sex_valid[is.na(df$Q21_recent_sex_valid)] <- "No"
# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q21_recent_sex_mutual_mast[df$Q21_recent_sex_mutual_mast == "Mutual masturbation"] <- "Yes"
df$Q21_recent_sex_oral[df$Q21_recent_sex_oral == "Oral sex"] <- "Yes"
df$Q21_recent_sex_fingering[df$Q21_recent_sex_fingering == "Fingering (sex using fingers)"] <- "Yes"
df$Q21_recent_sex_fisting[df$Q21_recent_sex_fisting == "Fisting (sex using fists)"] <- "Yes"
df$Q21_recent_sex_anal_bottom_condom[df$Q21_recent_sex_anal_bottom_condom == "Anal sex as bottom (receptive partner) with a condom"] <- "Yes"
df$Q21_recent_sex_anal_bottom_no_condom[df$Q21_recent_sex_anal_bottom_no_condom == "Anal sex as bottom (receptive partner) without a condom"] <- "Yes"
df$Q21_recent_sex_anal_top_condom[df$Q21_recent_sex_anal_top_condom == "Anal sex as top (insertive partner) with a condom"] <- "Yes"
df$Q21_recent_sex_anal_top_no_condom[df$Q21_recent_sex_anal_top_no_condom == "Anal sex as top (insertive partner) without a condom"] <- "Yes"
df$Q21_recent_sex_resp_vagina_condom[df$Q21_recent_sex_resp_vagina_condom == "Sex in my vagina or internal genitals with a condom"] <- "Yes"
df$Q21_recent_sex_resp_vagina_no_condom[df$Q21_recent_sex_resp_vagina_no_condom == "Sex in my vagina or internal genitals without a condom"] <- "Yes"
df$Q21_recent_sex_partner_vagina_condom[df$Q21_recent_sex_partner_vagina_condom == "Sex in my partner's vagina or internal genitals with a condom"] <- "Yes"
df$Q21_recent_sex_partner_vagina_no_condom[df$Q21_recent_sex_partner_vagina_no_condom == "Sex in my partner's vagina or internal genitals without a condom"] <- "Yes"
df$Q21_recent_sex_toys[df$Q21_recent_sex_toys == "Sex with prosthetics or sex toys"] <- "Yes"
df$Q21_recent_sex_online[df$Q21_recent_sex_online == "Online sex (camming, sexting)"] <- "Yes"
df$Q21_recent_sex_threesome[df$Q21_recent_sex_threesome == "Threesome (sex between 3 people)"] <- "Yes"
df$Q21_recent_sex_group[df$Q21_recent_sex_group == "Group sex (sex between 4+ people)"] <- "Yes"
df$Q21_recent_sex_none[df$Q21_recent_sex_none == "None of the above. I have not had sex in the past 6 months."] <- "Yes"
df$Q21_recent_sex_other[df$Q21_recent_sex_other == "Yes"] <- "Yes"


# ----------------------
# Creating "No" Level
# ----------------------
df$Q21_recent_sex_mutual_mast[is.na(df$Q21_recent_sex_mutual_mast) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_oral[is.na(df$Q21_recent_sex_oral) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_fingering[is.na(df$Q21_recent_sex_fingering) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_fisting[is.na(df$Q21_recent_sex_fisting) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_anal_bottom_condom[is.na(df$Q21_recent_sex_anal_bottom_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_anal_bottom_no_condom[is.na(df$Q21_recent_sex_anal_bottom_no_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_anal_top_condom[is.na(df$Q21_recent_sex_anal_top_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_anal_top_no_condom[is.na(df$Q21_recent_sex_anal_top_no_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_resp_vagina_condom[is.na(df$Q21_recent_sex_resp_vagina_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_resp_vagina_no_condom[is.na(df$Q21_recent_sex_resp_vagina_no_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_partner_vagina_condom[is.na(df$Q21_recent_sex_partner_vagina_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_partner_vagina_no_condom[is.na(df$Q21_recent_sex_partner_vagina_no_condom) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_toys[is.na(df$Q21_recent_sex_toys) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_online[is.na(df$Q21_recent_sex_online) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_threesome[is.na(df$Q21_recent_sex_threesome) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_group[is.na(df$Q21_recent_sex_group) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_none[is.na(df$Q21_recent_sex_none) & df$Q21_recent_sex_valid == "Yes"] <- "No"
df$Q21_recent_sex_other[is.na(df$Q21_recent_sex_other) & df$Q21_recent_sex_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q21_recent_sex_mutual_mast[df$Q21_recent_sex_mutual_mast == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_oral[df$Q21_recent_sex_oral == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_fingering[df$Q21_recent_sex_fingering == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_fisting[df$Q21_recent_sex_fingering == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_anal_bottom_condom[df$Q21_recent_sex_anal_bottom_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_anal_bottom_no_condom[df$Q21_recent_sex_anal_bottom_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_anal_top_condom[df$Q21_recent_sex_anal_top_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_anal_top_no_condom[df$Q21_recent_sex_anal_top_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_resp_vagina_condom[df$Q21_recent_sex_resp_vagina_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_resp_vagina_no_condom[df$Q21_recent_sex_resp_vagina_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_partner_vagina_condom[df$Q21_recent_sex_partner_vagina_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_partner_vagina_no_condom[df$Q21_recent_sex_partner_vagina_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_toys[df$Q21_recent_sex_toys == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_online[df$Q21_recent_sex_online == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_threesome[df$Q21_recent_sex_threesome == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_group[df$Q21_recent_sex_group == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_other[df$Q21_recent_sex_other == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"

df$Q21_recent_sex_none[df$Q21_recent_sex_mutual_mast == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_oral == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_fingering == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_fingering == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_anal_bottom_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_anal_bottom_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_anal_top_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_anal_top_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_resp_vagina_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_resp_vagina_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_partner_vagina_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_partner_vagina_no_condom == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_toys == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_online == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_threesome == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_group == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"
df$Q21_recent_sex_none[df$Q21_recent_sex_other == "Yes" & df$Q21_recent_sex_none == "Yes"] <- "7777: Poor Data Quality"


# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q21_recent_sex_mutual_mast[is.na(df$Q21_recent_sex_mutual_mast) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_oral[is.na(df$Q21_recent_sex_oral) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_fingering[is.na(df$Q21_recent_sex_fingering) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_fisting[is.na(df$Q21_recent_sex_fisting) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_bottom_condom[is.na(df$Q21_recent_sex_anal_bottom_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_bottom_no_condom[is.na(df$Q21_recent_sex_anal_bottom_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_top_condom[is.na(df$Q21_recent_sex_anal_top_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_top_no_condom[is.na(df$Q21_recent_sex_anal_top_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_resp_vagina_condom[is.na(df$Q21_recent_sex_resp_vagina_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_resp_vagina_no_condom[is.na(df$Q21_recent_sex_resp_vagina_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_partner_vagina_condom[is.na(df$Q21_recent_sex_partner_vagina_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_partner_vagina_no_condom[is.na(df$Q21_recent_sex_partner_vagina_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_toys[is.na(df$Q21_recent_sex_toys) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_online[is.na(df$Q21_recent_sex_online) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_threesome[is.na(df$Q21_recent_sex_threesome) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_group[is.na(df$Q21_recent_sex_group) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_none[is.na(df$Q21_recent_sex_none) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_other[df$Q21_recent_sex_none == "9999: True Missing"] <- "9999: True Missing"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q21_recent_sex_mutual_mast[is.na(df$Q21_recent_sex_mutual_mast) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_oral[is.na(df$Q21_recent_sex_oral) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_fingering[is.na(df$Q21_recent_sex_fingering) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_fisting[is.na(df$Q21_recent_sex_fisting) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_bottom_condom[is.na(df$Q21_recent_sex_anal_bottom_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_bottom_no_condom[is.na(df$Q21_recent_sex_anal_bottom_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_top_condom[is.na(df$Q21_recent_sex_anal_top_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_anal_top_no_condom[is.na(df$Q21_recent_sex_anal_top_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_resp_vagina_condom[is.na(df$Q21_recent_sex_resp_vagina_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_resp_vagina_no_condom[is.na(df$Q21_recent_sex_resp_vagina_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_partner_vagina_condom[is.na(df$Q21_recent_sex_partner_vagina_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_partner_vagina_no_condom[is.na(df$Q21_recent_sex_partner_vagina_no_condom) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_toys[is.na(df$Q21_recent_sex_toys) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_online[is.na(df$Q21_recent_sex_online) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_threesome[is.na(df$Q21_recent_sex_threesome) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_group[is.na(df$Q21_recent_sex_group) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_none[is.na(df$Q21_recent_sex_none) & df$Q21_recent_sex_valid == "No"] <- "9999: True Missing"
df$Q21_recent_sex_other[df$Q21_recent_sex_none == "9999: True Missing"] <- "9999: True Missing"

# ======================================================
# Q22 Partner Type
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q22_recent_sex_partner_bought_sex)
table(df$Q22_recent_sex_partner_sold_sex)
table(df$Q22_recent_sex_partner_trans_man)
table(df$Q22_recent_sex_partner_trans_woman)
table(df$Q22_recent_sex_partner_non_binary)
table(df$Q22_recent_sex_partner_none)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q21_recent_sex_partner_valid <- NA
df$Q21_recent_sex_partner_valid[df$Q22_recent_sex_partner_bought_sex == "A partner who gave me money, goods or services for sex" |
                                  df$Q22_recent_sex_partner_sold_sex == "A partner I gave money, goods or services for sex" |
                                  df$Q22_recent_sex_partner_trans_man == "A trans man" |
                                  df$Q22_recent_sex_partner_trans_woman == "A trans woman" |
                                  df$Q22_recent_sex_partner_non_binary == "A non-binary person" |
                                  df$Q22_recent_sex_partner_non_binary == "Multiple answers selected" |
                                  df$Q22_recent_sex_partner_none == "None of the above"] <- "Yes"
df$Q21_recent_sex_partner_valid[is.na(df$Q21_recent_sex_partner_valid)] <- "No"
table(df$Q21_recent_sex_partner_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q22_recent_sex_partner_bought_sex[df$Q22_recent_sex_partner_bought_sex == "A partner who gave me money, goods or services for sex"] <- "Yes"
df$Q22_recent_sex_partner_sold_sex[df$Q22_recent_sex_partner_sold_sex == "A partner I gave money, goods or services for sex"] <- "Yes"
df$Q22_recent_sex_partner_trans_man[df$Q22_recent_sex_partner_trans_man == "A trans man"] <- "Yes"
df$Q22_recent_sex_partner_trans_woman[df$Q22_recent_sex_partner_trans_woman == "A trans woman"] <- "Yes"
df$Q22_recent_sex_partner_non_binary[df$Q22_recent_sex_partner_non_binary == "A non-binary person"] <- "Yes"
df$Q22_recent_sex_partner_none[df$Q22_recent_sex_partner_none == "None of the above"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q22_recent_sex_partner_bought_sex[is.na(df$Q22_recent_sex_partner_bought_sex) & df$Q21_recent_sex_partner_valid == "Yes"] <- "No"
df$Q22_recent_sex_partner_sold_sex[is.na(df$Q22_recent_sex_partner_sold_sex) & df$Q21_recent_sex_partner_valid == "Yes"] <- "No"
df$Q22_recent_sex_partner_trans_man[is.na(df$Q22_recent_sex_partner_trans_man) & df$Q21_recent_sex_partner_valid == "Yes"] <- "No"
df$Q22_recent_sex_partner_trans_woman[is.na(df$Q22_recent_sex_partner_trans_woman) & df$Q21_recent_sex_partner_valid == "Yes"] <- "No"
df$Q22_recent_sex_partner_non_binary[is.na(df$Q22_recent_sex_partner_non_binary) & df$Q21_recent_sex_partner_valid == "Yes"] <- "No"
df$Q22_recent_sex_partner_none[is.na(df$Q22_recent_sex_partner_none) & df$Q21_recent_sex_partner_valid == "Yes"] <- "No"


# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q22_recent_sex_partner_bought_sex[df$Q22_recent_sex_partner_bought_sex == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_none[df$Q22_recent_sex_partner_bought_sex == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_sold_sex[df$Q22_recent_sex_partner_sold_sex == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_none[df$Q22_recent_sex_partner_sold_sex == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_trans_man[df$Q22_recent_sex_partner_trans_man == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_none[df$Q22_recent_sex_partner_trans_man == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_trans_woman[df$Q22_recent_sex_partner_trans_woman == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_none[df$Q22_recent_sex_partner_trans_woman == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_non_binary[df$Q22_recent_sex_partner_non_binary == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_none[df$Q22_recent_sex_partner_non_binary == "Yes" & df$Q22_recent_sex_partner_none == "Yes"] <- "7777: Poor Data Quality"
df$Q22_recent_sex_partner_non_binary[df$Q22_recent_sex_partner_non_binary == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q22_recent_sex_partner_bought_sex[is.na(df$Q22_recent_sex_partner_bought_sex) & df$Q21_recent_sex_partner_valid == "No"] <- "9999: True Missing"
df$Q22_recent_sex_partner_sold_sex[is.na(df$Q22_recent_sex_partner_sold_sex) & df$Q21_recent_sex_partner_valid == "No"] <- "9999: True Missing"
df$Q22_recent_sex_partner_trans_man[is.na(df$Q22_recent_sex_partner_trans_man) & df$Q21_recent_sex_partner_valid == "No"] <- "9999: True Missing"
df$Q22_recent_sex_partner_trans_woman[is.na(df$Q22_recent_sex_partner_trans_woman) & df$Q21_recent_sex_partner_valid == "No"] <- "9999: True Missing"
df$Q22_recent_sex_partner_non_binary[is.na(df$Q22_recent_sex_partner_non_binary) & df$Q21_recent_sex_partner_valid == "No"] <- "9999: True Missing"
df$Q22_recent_sex_partner_none[is.na(df$Q22_recent_sex_partner_none) & df$Q21_recent_sex_partner_valid == "No"] <- "9999: True Missing"

# ======================================================
# Q23_STI_test_when
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q23_STI_test_when)

# ----------------------
# Relevel Data
# ----------------------
df$Q23_STI_test_when[df$Q23_STI_test_when == "Never (skip to #29 below)"] <- "Never"
df$Q23_STI_test_when[df$Q23_STI_test_when == "Don't know (skip to #29 below)"] <- "Don't know"
df$Q23_STI_test_when[df$Q23_STI_test_when == "Multiple answers selected"] <- "7777: Poor Data Quality"

# Replace blanks with code 9999: true missing
# ----------------------
df$Q23_STI_test_when[is.na(df$Q23_STI_test_when)] <- "9999: True Missing"


# ======================================================
# Q24 STI Tests
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q24_STI_test_urine)
table(df$Q24_STI_test_blood)
table(df$Q24_STI_test_throat)
table(df$Q24_STI_test_rectal)
table(df$Q24_STI_test_none)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q24_STI_test_valid <- NA
df$Q24_STI_test_valid[df$Q24_STI_test_urine == "Urine test" |
                        df$Q24_STI_test_blood == "Blood sample" |
                        df$Q24_STI_test_throat == "Throat swab" |
                        df$Q24_STI_test_rectal == "Rectal swab (in your bum)" |
                        df$Q24_STI_test_none == "None of the above"] <- "Yes"
df$Q24_STI_test_valid[df$Q23_STI_test_when == "Never" | 
                        df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "Yes"
df$Q24_STI_test_valid[is.na(df$Q24_STI_test_valid)] <- "No"
table(df$Q24_STI_test_valid)


# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q24_STI_test_urine[df$Q24_STI_test_urine == "Urine test"] <- "Yes"
df$Q24_STI_test_blood[df$Q24_STI_test_blood == "Blood sample"] <- "Yes"
df$Q24_STI_test_throat[df$Q24_STI_test_throat == "Throat swab"] <- "Yes"
df$Q24_STI_test_rectal[df$Q24_STI_test_rectal == "Rectal swab (in your bum)"] <- "Yes"
df$Q24_STI_test_none[df$Q24_STI_test_none == "None of the above"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q24_STI_test_urine[is.na(df$Q24_STI_test_urine) & df$Q24_STI_test_valid == "Yes"] <- "No"
df$Q24_STI_test_blood[is.na(df$Q24_STI_test_blood) & df$Q24_STI_test_valid == "Yes"] <- "No"
df$Q24_STI_test_throat[is.na(df$Q24_STI_test_throat) & df$Q24_STI_test_valid == "Yes"] <- "No"
df$Q24_STI_test_rectal[is.na(df$Q24_STI_test_rectal) & df$Q24_STI_test_valid == "Yes"] <- "No"
df$Q24_STI_test_none[is.na(df$Q24_STI_test_none) & df$Q24_STI_test_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q24_STI_test_urine[df$Q24_STI_test_urine == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"
df$Q24_STI_test_none[df$Q24_STI_test_urine == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"

df$Q24_STI_test_blood[df$Q24_STI_test_blood == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"
df$Q24_STI_test_none[df$Q24_STI_test_blood == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"

df$Q24_STI_test_throat[df$Q24_STI_test_throat == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"
df$Q24_STI_test_none[df$Q24_STI_test_throat == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"

df$Q24_STI_test_rectal[df$Q24_STI_test_rectal == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"
df$Q24_STI_test_none[df$Q24_STI_test_rectal == "Yes" & df$Q24_STI_test_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Accounting For Skip Logic
# ----------------------
df$Q24_STI_test_urine[df$Q23_STI_test_when == "Never"] <- "8888: No Last Test"
df$Q24_STI_test_blood[df$Q23_STI_test_when == "Never"] <- "8888: No Last Test"
df$Q24_STI_test_throat[df$Q23_STI_test_when == "Never"] <- "8888: No Last Test"
df$Q24_STI_test_rectal[df$Q23_STI_test_when == "Never"] <- "8888: No Last Test"
df$Q24_STI_test_none[df$Q23_STI_test_when == "Never"] <- "8888: No Last Test"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q24_STI_test_urine[is.na(df$Q24_STI_test_urine) & df$Q24_STI_test_valid == "No"] <- "9999: True Missing"
df$Q24_STI_test_blood[is.na(df$Q24_STI_test_blood) & df$Q24_STI_test_valid == "No"] <- "9999: True Missing"
df$Q24_STI_test_throat[is.na(df$Q24_STI_test_throat) & df$Q24_STI_test_valid == "No"] <- "9999: True Missing"
df$Q24_STI_test_rectal[is.na(df$Q24_STI_test_rectal) & df$Q24_STI_test_valid == "No"] <- "9999: True Missing"
df$Q24_STI_test_none[is.na(df$Q24_STI_test_none) & df$Q24_STI_test_valid == "No"] <- "9999: True Missing"


# ======================================================
# Q25 Delays in Testing
# ======================================================
# ----------------------
# Explore
# ----------------------
table(df$Q25_delays_busy)
table(df$Q25_delays_distance)
table(df$Q25_delays_hours)
table(df$Q25_delays_privacy)
table(df$Q25_delays_sensitivity)
table(df$Q25_delays_stressed)
table(df$Q25_delays_cost)
table(df$Q25_delays_wait)
table(df$Q25_delays_language)
table(df$Q25_delays_other_text)
table(df$Q25_delays_none)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q25_delays_other <- NA
df$Q25_delays_other[is.na(df$Q25_delays_other_text)] <- "No"
df$Q25_delays_other[!is.na(df$Q25_delays_other_text)] <- "Yes"
table(df$Q25_delays_other)

df$Q25_delays_other_text[df$Q25_delays_other == "No"] <- "8888: Other Not Selected"
df$Q25_delays_other_text[is.na(df$Q25_delays_other_text) & df$Q25_delays_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q25_delays_valid <- NA
df$Q25_delays_valid[df$Q25_delays_busy == "I was late, busy or preoccupied" |
                      df$Q25_delays_busy == "Too busy" |
                      df$Q25_delays_distance == "Services too far away" |
                      df$Q25_delays_hours == "Hours inconvenient" |
                      df$Q25_delays_privacy == "Lack of privacy" |
                      df$Q25_delays_sensitivity == "Lack of professional sensitivity to gay, bi or queer men's health" |
                      df$Q25_delays_stressed == "stressed out, anxious or depressed" |
                      df$Q25_delays_stressed == "Stressed out, anxious or depressed" |
                      df$Q25_delays_cost == "the cost (e.g. no health insurance)" |
                      df$Q25_delays_cost == "The cost (e.g. no health insurance)" |
                      df$Q25_delays_wait == "Wait time for appointment too long" |
                      df$Q25_delays_language == "Services not in my preferred language" |
                      df$Q25_delays_none == "No delays or skipped STI testing" |
                      df$Q25_delays_other == "Yes"] <- "Yes"
df$Q25_delays_valid[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "Yes"
df$Q25_delays_valid[is.na(df$Q25_delays_valid)] <- "No"

table(df$Q25_delays_valid)


# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q25_delays_busy[df$Q25_delays_busy == "I was late, busy or preoccupied"] <- "Yes"
df$Q25_delays_busy[df$Q25_delays_busy == "Too busy"] <- "Yes"
df$Q25_delays_distance[df$Q25_delays_distance == "Services too far away"] <- "Yes"
df$Q25_delays_hours[df$Q25_delays_hours == "Hours inconvenient"] <- "Yes"
df$Q25_delays_privacy[df$Q25_delays_privacy == "Lack of privacy"] <- "Yes"
df$Q25_delays_sensitivity[df$Q25_delays_sensitivity == "Lack of professional sensitivity to gay, bi or queer men's health"] <- "Yes"
df$Q25_delays_stressed[df$Q25_delays_stressed == "stressed out, anxious or depressed"] <- "Yes"
df$Q25_delays_stressed[df$Q25_delays_stressed == "Stressed out, anxious or depressed"] <- "Yes"
df$Q25_delays_cost[df$Q25_delays_cost == "the cost (e.g. no health insurance)"] <- "Yes"
df$Q25_delays_cost[df$Q25_delays_cost == "The cost (e.g. no health insurance)"] <- "Yes"
df$Q25_delays_wait[df$Q25_delays_wait == "Wait time for appointment too long"] <- "Yes"
df$Q25_delays_language[df$Q25_delays_language == "Services not in my preferred language"] <- "Yes"
df$Q25_delays_none[df$Q25_delays_none == "No delays or skipped STI testing"] <- "Yes"
df$Q25_delays_other[df$Q25_delays_other == "Yes"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q25_delays_busy[is.na(df$Q25_delays_busy) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_distance[is.na(df$Q25_delays_distance) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_hours[is.na(df$Q25_delays_hours) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_privacy[is.na(df$Q25_delays_privacy) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_sensitivity[is.na(df$Q25_delays_sensitivity) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_stressed[is.na(df$Q25_delays_stressed) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_cost[is.na(df$Q25_delays_cost) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_wait[is.na(df$Q25_delays_wait) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_language[is.na(df$Q25_delays_language) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_none[is.na(df$Q25_delays_none) & df$Q25_delays_valid == "Yes"] <- "No"
df$Q25_delays_other[is.na(df$Q25_delays_other) & df$Q25_delays_valid == "Yes"] <- "No"


# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q25_delays_busy[df$Q25_delays_busy == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_busy == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_distance[df$Q25_delays_distance == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_distance == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_hours[df$Q25_delays_hours == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_hours == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_privacy[df$Q25_delays_privacy == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_privacy == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_sensitivity[df$Q25_delays_sensitivity == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_sensitivity == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_stressed[df$Q25_delays_stressed == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_stressed == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_cost[df$Q25_delays_cost == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_cost == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_wait[df$Q25_delays_wait == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_wait == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_language[df$Q25_delays_language == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_language == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

df$Q25_delays_other[df$Q25_delays_other == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"
df$Q25_delays_none[df$Q25_delays_other == "Yes" & df$Q25_delays_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q25_delays_busy[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_distance[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_hours[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_privacy[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_sensitivity[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_stressed[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_cost[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_wait[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_language[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_none[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q25_delays_other[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"


# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q25_delays_busy[is.na(df$Q25_delays_busy) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_distance[is.na(df$Q25_delays_distance) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_hours[is.na(df$Q25_delays_hours) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_privacy[is.na(df$Q25_delays_privacy) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_sensitivity[is.na(df$Q25_delays_sensitivity) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_stressed[is.na(df$Q25_delays_stressed) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_cost[is.na(df$Q25_delays_cost) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_wait[is.na(df$Q25_delays_wait) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_language[is.na(df$Q25_delays_language) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_none[is.na(df$Q25_delays_none) & df$Q25_delays_valid == "No"] <- "9999: True Missing"
df$Q25_delays_other[df$Q25_delays_none == "9999: True Missing"] <- "9999: True Missing"


# ======================================================
# Q26 STIs
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q26_STI_syphilis)
table(df$Q26_STI_chlamydia)
table(df$Q26_STI_gonorrhea)
table(df$Q26_STI_warts)
table(df$Q26_STI_herpes)
table(df$Q26_STI_urethritis)
table(df$Q26_STI_LGV_WPG_only)
table(df$Q26_STI_crabs_WPG_only)
table(df$Q26_STI_none)
table(df$Q26_STI_other_text)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q26_STI_other <- NA
df$Q26_STI_other[is.na(df$Q26_STI_other_text)] <- "No"
df$Q26_STI_other[!is.na(df$Q26_STI_other_text)] <- "Yes"
table(df$Q26_STI_other)

df$Q26_STI_other_text[df$Q26_STI_other == "No"] <- "8888: Other Not Selected"
df$Q26_STI_other_text[is.na(df$Q26_STI_other_text) & df$Q26_STI_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q26_STI_type_valid <- NA
df$Q26_STI_type_valid[df$Q26_STI_syphilis == "Syphilis" |
                        df$Q26_STI_chlamydia == "Chlamydia" |
                        df$Q26_STI_gonorrhea == "Gonorrhea" |
                        df$Q26_STI_warts == "Warts (genital or anal)" |
                        df$Q26_STI_herpes == "Herpes (genital or anal)" |
                        df$Q26_STI_urethritis == "Non-specific urethritis" |
                        df$Q26_STI_crabs_WPG_only == "Crabs" |
                        !is.na(df$Q26_STI_LGV_WPG_only) |
                        df$Q26_STI_none == "I have had no STIs in the past year" |
                        df$Q26_STI_other == "Yes"] <- "Yes"
df$Q26_STI_type_valid[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "Yes"
df$Q26_STI_type_valid[is.na(df$Q26_STI_type_valid)] <- "No"

table(df$Q26_STI_type_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q26_STI_syphilis[df$Q26_STI_syphilis == "Syphilis"] <- "Yes"
df$Q26_STI_chlamydia[df$Q26_STI_chlamydia == "Chlamydia"] <- "Yes"
df$Q26_STI_gonorrhea[df$Q26_STI_gonorrhea == "Gonorrhea"] <- "Yes"
df$Q26_STI_warts[df$Q26_STI_warts == "Warts (genital or anal)"] <- "Yes"
df$Q26_STI_herpes[df$Q26_STI_herpes == "Herpes (genital or anal)"] <- "Yes"
df$Q26_STI_urethritis[df$Q26_STI_urethritis == "Non-specific urethritis"] <- "Yes"
df$Q26_STI_crabs_WPG_only[df$Q26_STI_crabs_WPG_only == "Crabs"] <- "Yes"
df$Q26_STI_LGV_WPG_only[!is.na(df$Q26_STI_LGV_WPG_only)] <- "Yes"
df$Q26_STI_none[df$Q26_STI_none == "I have had no STIs in the past year"] <- "Yes"
df$Q26_STI_other[df$Q26_STI_other == "Yes"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q26_STI_syphilis[is.na(df$Q26_STI_syphilis) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_chlamydia[is.na(df$Q26_STI_chlamydia) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_gonorrhea[is.na(df$Q26_STI_gonorrhea) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_warts[is.na(df$Q26_STI_warts) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_herpes[is.na(df$Q26_STI_herpes) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_urethritis[is.na(df$Q26_STI_urethritis) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_LGV_WPG_only[is.na(df$Q26_STI_LGV_WPG_only) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_crabs_WPG_only[is.na(df$Q26_STI_crabs_WPG_only) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_none[is.na(df$Q26_STI_none) & df$Q26_STI_type_valid == "Yes"] <- "No"
df$Q26_STI_other[is.na(df$Q26_STI_other) & df$Q26_STI_type_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q26_STI_syphilis[df$Q26_STI_syphilis == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_syphilis == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_LGV_WPG_only[df$Q26_STI_LGV_WPG_only == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_LGV_WPG_only == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_chlamydia[df$Q26_STI_chlamydia == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_chlamydia == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_gonorrhea[df$Q26_STI_gonorrhea == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_gonorrhea == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_warts[df$Q26_STI_warts == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_warts == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_herpes[df$Q26_STI_herpes == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_herpes == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_urethritis[df$Q26_STI_urethritis == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_urethritis == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_crabs_WPG_only[df$Q26_STI_crabs_WPG_only == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_crabs_WPG_only == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

df$Q26_STI_other[df$Q26_STI_other == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"
df$Q26_STI_none[df$Q26_STI_other == "Yes" & df$Q26_STI_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q26_STI_syphilis[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_chlamydia[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_gonorrhea[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_warts[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_LGV_WPG_only[df$Q26_STI_LGV_WPG_only == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_herpes[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_urethritis[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_crabs_WPG_only[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_none[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"
df$Q26_STI_other[df$Q23_STI_test_when == "Never" | df$Q23_STI_test_when == "7777: Poor Data Quality"] <- "8888: No Last Test"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q26_STI_syphilis[is.na(df$Q26_STI_syphilis) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_chlamydia[is.na(df$Q26_STI_chlamydia) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_gonorrhea[is.na(df$Q26_STI_gonorrhea) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_warts[is.na(df$Q26_STI_warts) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_herpes[is.na(df$Q26_STI_herpes) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_LGV_WPG_only[is.na(df$Q26_STI_LGV_WPG_only) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_urethritis[is.na(df$Q26_STI_urethritis) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_crabs_WPG_only[is.na(df$Q26_STI_crabs_WPG_only) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_none[is.na(df$Q26_STI_none) & df$Q26_STI_type_valid == "No"] <- "9999: True Missing"
df$Q26_STI_other[df$Q26_STI_none == "9999: True Missing"] <- "9999: True Missing"

# ----------------------
# Winnipeg Only
# ----------------------
df$Q26_STI_crabs_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# Q27_STI_bumm
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q27_STI_bum)

# ----------------------
# Manage Data Quality and Skip Logic
# ----------------------
df$Q27_STI_bum[df$Q27_STI_bum == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q27_STI_bum[df$Q23_STI_test_when == "Never"] <- "8888: No Last Test"

# ----------------------
# Code Missing Responses
# ----------------------
df$Q27_STI_bum[is.na(df$Q27_STI_bum)] <- "9999: True Missing"

# ======================================================
# Q28_self_test_likely
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q28_self_test_likely)

# ----------------------
# Manage Data Quality Logic
# ----------------------
df$Q28_self_test_likely[df$Q28_self_test_likely == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missing Responses
# ----------------------
df$Q28_self_test_likely[is.na(df$Q28_self_test_likely)] <- "9999: True Missing"

# ======================================================
# Q29 Selt Testing Types
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q29_self_test_blood)
table(df$Q29_self_test_pee)
table(df$Q29_self_test_throat)
table(df$Q29_self_test_bum)
table(df$Q29_self_test_none_WPG_excluded)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q29_self_test_blood_valid <- NA
df$Q29_self_test_blood_valid[df$Q29_self_test_blood == "Prick your finger to provide a few drops of blood" |
                               df$Q29_self_test_pee == "Pee into a container (urine)" |
                               df$Q29_self_test_throat == "Swab your throat" |
                               df$Q29_self_test_bum == "Swab your bum (rectum)" |
                               df$Q29_self_test_none_WPG_excluded == "I could not self-collect any of the above"] <- "Yes"
df$Q29_self_test_blood_valid[is.na(df$Q29_self_test_blood_valid)] <- "No"

table(df$Q29_self_test_blood_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q29_self_test_blood[df$Q29_self_test_blood == "Prick your finger to provide a few drops of blood"] <- "Yes"
df$Q29_self_test_pee[df$Q29_self_test_pee == "Pee into a container (urine)"] <- "Yes"
df$Q29_self_test_throat[df$Q29_self_test_throat == "Swab your throat"] <- "Yes"
df$Q29_self_test_bum[df$Q29_self_test_bum == "Swab your bum (rectum)"] <- "Yes"
df$Q29_self_test_none_WPG_excluded[df$Q29_self_test_none_WPG_excluded == "I could not self-collect any of the above"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q29_self_test_blood[is.na(df$Q29_self_test_blood) & df$Q29_self_test_blood_valid == "Yes"] <- "No"
df$Q29_self_test_pee[is.na(df$Q29_self_test_pee) & df$Q29_self_test_blood_valid == "Yes"] <- "No"
df$Q29_self_test_throat[is.na(df$Q29_self_test_throat) & df$Q29_self_test_blood_valid == "Yes"] <- "No"
df$Q29_self_test_bum[is.na(df$Q29_self_test_bum) & df$Q29_self_test_blood_valid == "Yes"] <- "No"
df$Q29_self_test_none_WPG_excluded[is.na(df$Q29_self_test_none_WPG_excluded) & df$Q29_self_test_blood_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q29_self_test_pee[df$Q29_self_test_pee == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"
df$Q29_self_test_none_WPG_excluded[df$Q29_self_test_pee == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"

df$Q29_self_test_pee[df$Q29_self_test_pee == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"
df$Q29_self_test_none_WPG_excluded[df$Q29_self_test_pee == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"

df$Q29_self_test_throat[df$Q29_self_test_throat == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"
df$Q29_self_test_none_WPG_excluded[df$Q29_self_test_throat == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"

df$Q29_self_test_bum[df$Q29_self_test_bum == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"
df$Q29_self_test_none_WPG_excluded[df$Q29_self_test_bum == "Yes" & df$Q29_self_test_none_WPG_excluded == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q29_self_test_blood[is.na(df$Q29_self_test_blood) & df$Q29_self_test_blood_valid == "No"] <- "9999: True Missing"
df$Q29_self_test_pee[is.na(df$Q29_self_test_pee) & df$Q29_self_test_blood_valid == "No"] <- "9999: True Missing"
df$Q29_self_test_throat[is.na(df$Q29_self_test_throat) & df$Q29_self_test_blood_valid == "No"] <- "9999: True Missing"
df$Q29_self_test_bum[is.na(df$Q29_self_test_bum) & df$Q29_self_test_blood_valid == "No"] <- "9999: True Missing"
df$Q29_self_test_none_WPG_excluded[is.na(df$Q29_self_test_none_WPG_excluded) & df$Q29_self_test_blood_valid == "No"] <- "9999: True Missing"

# ----------------------
# Deal with skip Logic
# ----------------------
df$Q29_self_test_none_WPG_excluded[df$Q0_site == "Winnipeg"] <- "8888: Winnipeg Survey "

# ======================================================
# Q30 Blood Donation Awareness
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q30_BD_aware_HIV_tests)
table(df$Q30_BD_aware_window_period_risk)
table(df$Q30_BD_aware_MSM_risk)
table(df$Q30_BD_aware_MSM_deferral)
table(df$Q30_BD_aware_trans)
table(df$Q30_BD_aware_none)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q30_BD_aware_valid <- NA
df$Q30_BD_aware_valid[!is.na(df$Q30_BD_aware_HIV_tests) |
                        !is.na(df$Q30_BD_aware_window_period_risk) |
                        !is.na(df$Q30_BD_aware_MSM_risk) |
                        !is.na(df$Q30_BD_aware_MSM_deferral) |
                        !is.na(df$Q30_BD_aware_trans) |
                        !is.na(df$Q30_BD_aware_none) ] <- "Yes"
df$Q30_BD_aware_valid[is.na(df$Q30_BD_aware_valid)] <- "No"

table(df$Q30_BD_aware_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q30_BD_aware_HIV_tests[!is.na(df$Q30_BD_aware_HIV_tests) ] <- "Yes"
df$Q30_BD_aware_window_period_risk[!is.na(df$Q30_BD_aware_window_period_risk)] <- "Yes"
df$Q30_BD_aware_MSM_risk[!is.na(df$Q30_BD_aware_MSM_risk)] <- "Yes"
df$Q30_BD_aware_MSM_deferral[!is.na(df$Q30_BD_aware_MSM_deferral)] <- "Yes"
df$Q30_BD_aware_trans[!is.na(df$Q30_BD_aware_trans)] <- "Yes"
df$Q30_BD_aware_none[!is.na(df$Q30_BD_aware_none)] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q30_BD_aware_HIV_tests[is.na(df$Q30_BD_aware_HIV_tests) & df$Q30_BD_aware_valid == "Yes"] <- "No"
df$Q30_BD_aware_window_period_risk[is.na(df$Q30_BD_aware_window_period_risk) & df$Q30_BD_aware_valid == "Yes"] <- "No"
df$Q30_BD_aware_MSM_risk[is.na(df$Q30_BD_aware_MSM_risk) & df$Q30_BD_aware_valid == "Yes"] <- "No"
df$Q30_BD_aware_MSM_deferral[is.na(df$Q30_BD_aware_MSM_deferral) & df$Q30_BD_aware_valid == "Yes"] <- "No"
df$Q30_BD_aware_trans[is.na(df$Q30_BD_aware_trans) & df$Q30_BD_aware_valid == "Yes"] <- "No"
df$Q30_BD_aware_none[is.na(df$Q30_BD_aware_none) & df$Q30_BD_aware_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q30_BD_aware_HIV_tests[df$Q30_BD_aware_HIV_tests == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"
df$Q30_BD_aware_none[df$Q30_BD_aware_HIV_tests == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"

df$Q30_BD_aware_window_period_risk[df$Q30_BD_aware_window_period_risk == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"
df$Q30_BD_aware_none[df$Q30_BD_aware_window_period_risk == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"

df$Q30_BD_aware_MSM_risk[df$Q30_BD_aware_MSM_risk == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"
df$Q30_BD_aware_none[df$Q30_BD_aware_MSM_risk == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"

df$Q30_BD_aware_MSM_deferral[df$Q30_BD_aware_MSM_deferral == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"
df$Q30_BD_aware_none[df$Q30_BD_aware_MSM_deferral == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"

df$Q30_BD_aware_trans[df$Q30_BD_aware_trans == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"
df$Q30_BD_aware_none[df$Q30_BD_aware_trans == "Yes" & df$Q30_BD_aware_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q30_BD_aware_HIV_tests[is.na(df$Q30_BD_aware_HIV_tests) & df$Q30_BD_aware_valid == "No"] <- "9999: True Missing"
df$Q30_BD_aware_window_period_risk[is.na(df$Q30_BD_aware_window_period_risk) & df$Q30_BD_aware_valid == "No"] <- "9999: True Missing"
df$Q30_BD_aware_MSM_risk[is.na(df$Q30_BD_aware_MSM_risk) & df$Q30_BD_aware_valid == "No"] <- "9999: True Missing"
df$Q30_BD_aware_MSM_deferral[is.na(df$Q30_BD_aware_MSM_deferral) & df$Q30_BD_aware_valid == "No"] <- "9999: True Missing"
df$Q30_BD_aware_trans[is.na(df$Q30_BD_aware_trans) & df$Q30_BD_aware_valid == "No"] <- "9999: True Missing"
df$Q30_BD_aware_none[is.na(df$Q30_BD_aware_none) & df$Q30_BD_aware_valid == "No"] <- "9999: True Missing"

# ======================================================
# Q31 Blood Donation Opinions
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q31_BD_opinion_justified)
table(df$Q31_BD_opinion_discriminatory)
table(df$Q31_BD_opinion_short_deferral)
table(df$Q31_BD_opinion_num_partners)
table(df$Q31_BD_opinion_new_partners)
table(df$Q31_BD_opinion_sex_practices)
table(df$Q31_BD_opinion_would_donate)

# ----------------------
# Manage Data Quality Logic
# ----------------------
df$Q31_BD_opinion_justified[df$Q31_BD_opinion_justified == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q31_BD_opinion_discriminatory[df$Q31_BD_opinion_discriminatory == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q31_BD_opinion_short_deferral[df$Q31_BD_opinion_short_deferral == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q31_BD_opinion_num_partners[df$Q31_BD_opinion_num_partners == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q31_BD_opinion_new_partners[df$Q31_BD_opinion_new_partners == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q31_BD_opinion_would_donate[df$Q31_BD_opinion_would_donate == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missing Responses
# ----------------------
df$Q31_BD_opinion_justified[is.na(df$Q31_BD_opinion_justified)] <- "9999: True Missing"
df$Q31_BD_opinion_discriminatory[is.na(df$Q31_BD_opinion_discriminatory)] <- "9999: True Missing"
df$Q31_BD_opinion_short_deferral[is.na(df$Q31_BD_opinion_short_deferral)] <- "9999: True Missing"
df$Q31_BD_opinion_num_partners[is.na(df$Q31_BD_opinion_num_partners)] <- "9999: True Missing"
df$Q31_BD_opinion_new_partners[is.na(df$Q31_BD_opinion_new_partners)] <- "9999: True Missing"
df$Q31_BD_opinion_sex_practices[is.na(df$Q31_BD_opinion_sex_practices)] <- "9999: True Missing"
df$Q31_BD_opinion_would_donate[is.na(df$Q31_BD_opinion_would_donate)] <- "9999: True Missing"

# ======================================================
# Q32 HIV Prevention Knowledge
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q32_know_condoms)
table(df$Q32_know_PrEP)
table(df$Q32_know_PEP)
table(df$Q32_know_viral_load)
table(df$Q32_know_undetectable)
table(df$Q32_know_anal_risk_WPG_only)
table(df$Q32_know_TasP_WPG_only)
table(df$Q32_know_rapid_test_WPG_only)

# ----------------------
# Code Missing Responses
# ----------------------
df$Q32_know_condoms[is.na(df$Q32_know_condoms)] <- "9999: True Missing"
df$Q32_know_PrEP[is.na(df$Q32_know_PrEP)] <- "9999: True Missing"
df$Q32_know_PEP[is.na(df$Q32_know_PEP)] <- "9999: True Missing"
df$Q32_know_viral_load[is.na(df$Q32_know_viral_load)] <- "9999: True Missing"
df$Q32_know_undetectable[is.na(df$Q32_know_undetectable)] <- "9999: True Missing"
df$Q32_know_anal_risk_WPG_only[is.na(df$Q32_know_anal_risk_WPG_only)] <- "9999: True Missing"
df$Q32_know_TasP_WPG_only[is.na(df$Q32_know_TasP_WPG_only)] <- "9999: True Missing"
df$Q32_know_rapid_test_WPG_only[is.na(df$Q32_know_rapid_test_WPG_only)] <- "9999: True Missing"

# ----------------------
# Winnipeg Only
# ----------------------
df$Q32_know_anal_risk_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$Q32_know_TasP_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$Q32_know_rapid_test_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "


# ======================================================
# Q33_HCV_test_when
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q33_HCV_test_when)

# ----------------------
# Manage Data Quality Logic
# ----------------------
df$Q33_HCV_test_when[df$Q33_HCV_test_when == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missing Responses
# ----------------------
df$Q33_HCV_test_when[is.na(df$Q33_HCV_test_when)] <- "9999: True Missing"

# ======================================================
# Q34_HCV_test_result
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q34_HCV_test_result)

# ----------------------
# Manage Data Quality Logic
# ----------------------
df$Q34_HCV_test_result[df$Q34_HCV_test_result == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Account for skip logic
# ----------------------
df$Q34_HCV_test_result[df$Q33_HCV_test_when == "I have never tested for Hep C"] <- "8888: Never tested for HCV"

# ----------------------
# Code Missing Responses
# ----------------------
df$Q34_HCV_test_result[is.na(df$Q34_HCV_test_result)] <- "9999: True Missing"


# ======================================================
# Q35_HCV_ever_diagnosed
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q35_HCV_ever_diagnosed)


# ----------------------
# Code Missing Responses
# ----------------------
df$Q35_HCV_ever_diagnosed[is.na(df$Q35_HCV_ever_diagnosed)] <- "9999: True Missing"

# ----------------------
# Manage Data Quality 
# ----------------------
df$Q35_HCV_ever_diagnosed[df$Q35_HCV_ever_diagnosed == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q35_HCV_ever_diagnosed[df$Q35_HCV_ever_diagnosed == "No (skip to #48 - start of next section)"] <- "No"
df$Q35_HCV_ever_diagnosed[df$Q35_HCV_ever_diagnosed == "Yes (continue to next question)"] <- "Yes"
df$Q35_HCV_ever_diagnosed[df$Q0_anon_code == "C0129"] <- "Yes"
# ----------------------
# Account for skip logica
# ----------------------
df$Q35_HCV_ever_diagnosed[df$Q33_HCV_test_when == "I have never tested for Hep C"] <- "8888: Never tested for HCV"

# ======================================================
# Q36_HCV_first_diagnosed
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q36_HCV_first_diagnosed)
# ----------------------
# Manage Data Quality 
# ----------------------
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "?"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "90's"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "90's"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "idk"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "ILLEGIBLE"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "LONG TIME AGO"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "n/a"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "N/A"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "NA"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "never"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "Never"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "no"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "No"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "NO"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "Unsure"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "Unsure"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "UR"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "2048"] <- "7777: Poor Data Quality"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "3 years ago"] <- "2015"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "last year"] <- "2017"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "77"] <- "1977"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "2 weeks"] <- "2018"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "0"] <- "2018"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "2 1/2 years ago"] <- "2016"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "5 years ago (illegible)"] <- "2013"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "15  ans [15 years]"] <- "2003"
df$Q36_HCV_first_diagnosed[df$Q36_HCV_first_diagnosed == "3 ans [three years ago?]"] <- "2015"

# ----------------------
# Account for Skip Logic 
# ----------------------

df$Q36_HCV_first_diagnosed[df$Q33_HCV_test_when == "I have never tested for Hep C"] <- "8888: Never tested for HCV"
df$Q36_HCV_first_diagnosed[df$Q35_HCV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HCV"
df$Q36_HCV_first_diagnosed[is.na(df$Q35_HCV_ever_diagnosed) | df$Q35_HCV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q36_HCV_first_diagnosed[is.na(df$Q36_HCV_first_diagnosed)] <- "9999: True Missing"

# ======================================================
# Q37_HCV_last_dr_visit
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q37_HCV_last_dr_visit)
# ----------------------
# Manage Skip Logic
# ----------------------
df$Q37_HCV_last_dr_visit[df$Q33_HCV_test_when == "I have never tested for Hep C"] <- "8888: Never tested for HCV"
df$Q37_HCV_last_dr_visit[df$Q34_HCV_test_result == "I never received my result"] <- "8888: Never received HCV test result"
df$Q37_HCV_last_dr_visit[df$Q35_HCV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HCV"
df$Q37_HCV_last_dr_visit[is.na(df$Q35_HCV_ever_diagnosed) | df$Q35_HCV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q37_HCV_last_dr_visit[is.na(df$Q37_HCV_last_dr_visit)] <- "9999: True Missing"

# ======================================================
# Q38_HCV_ever_treatment
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q38_HCV_ever_treatment)

# ----------------------
# Rename Levels
# ----------------------
df$Q38_HCV_ever_treatment[df$Q38_HCV_ever_treatment == "No (skip to #48)"] <- "No"
df$Q38_HCV_ever_treatment[df$Q38_HCV_ever_treatment == "Yes (continue to next question)"] <- "Yes"
df$Q38_HCV_ever_treatment[df$Q38_HCV_ever_treatment == "No"] <- "No"

# ----------------------
# Manage Skip Logic
# ----------------------

df$Q38_HCV_ever_treatment[df$Q33_HCV_test_when == "I have never tested for Hep C"] <- "8888: Never tested for HCV"
df$Q38_HCV_ever_treatment[df$Q35_HCV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HCV"
df$Q38_HCV_ever_treatment[is.na(df$Q35_HCV_ever_diagnosed) | df$Q35_HCV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q38_HCV_ever_treatment[is.na(df$Q38_HCV_ever_treatment)] <- "9999: True Missing"


# ======================================================
# Q39_HCV_last_started_treatment
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q39_HCV_last_started_treatment)

# ----------------------
# Clean Data
# ----------------------
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "-" | 
                                    df$Q39_HCV_last_started_treatment == "/" |
                                    df$Q39_HCV_last_started_treatment == "?" |
                                    df$Q39_HCV_last_started_treatment == "n/a" |
                                    df$Q39_HCV_last_started_treatment == "19" |
                                    df$Q39_HCV_last_started_treatment == "I don't know" |
                                    df$Q39_HCV_last_started_treatment == "idk" |
                                    df$Q39_HCV_last_started_treatment == "N/A" |
                                    df$Q39_HCV_last_started_treatment == "NA" |
                                    df$Q39_HCV_last_started_treatment == "Neg" |
                                    df$Q39_HCV_last_started_treatment == "No" |
                                    df$Q39_HCV_last_started_treatment == "NO" |
                                    df$Q39_HCV_last_started_treatment == "none" |
                                    df$Q39_HCV_last_started_treatment == "unknown" |
                                    df$Q39_HCV_last_started_treatment == "X" |
                                    df$Q39_HCV_last_started_treatment == "\\"] <- "7777: Poor Data Quality"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "never" |
                                    df$Q39_HCV_last_started_treatment == "Never had HepC"] <- NA
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "Sept 2018"] <- "2018"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "0"] <- "2018"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "15 ans [15 years]"] <- "2003"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "Yes (continue to next question)"] <- "Yes"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "No"] <- "No"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "3 ans [three years ago?]"] <- "2015"
df$Q39_HCV_last_started_treatment[df$Q39_HCV_last_started_treatment == "3 ans [3 years ago?]"] <- "2015"

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q39_HCV_last_started_treatment[df$Q33_HCV_test_when == "I have never tested for Hep C"] <- "8888: Never tested for HCV"
df$Q39_HCV_last_started_treatment[df$Q35_HCV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HCV"
df$Q39_HCV_last_started_treatment[df$Q38_HCV_ever_treatment == "No"] <- "8888: Never had treatment"
df$Q39_HCV_last_started_treatment[is.na(df$Q35_HCV_ever_diagnosed) | df$Q35_HCV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q39_HCV_last_started_treatment[is.na(df$Q39_HCV_last_started_treatment)] <- "9999: True Missing"


# ======================================================
# Q40_HCV_treatment_success
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q40_HCV_treatment_success)
# ----------------------
# Manage Skip Logic
# ----------------------
df$Q40_HCV_treatment_success[df$Q38_HCV_ever_treatment == "No"] <- "8888: Never Started Treatment for HCV"
df$Q40_HCV_treatment_success[df$Q35_HCV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HCV"
df$Q40_HCV_treatment_success[df$Q35_HCV_ever_diagnosed == "I have never tested for Hep C"] <- "8888: Never tested for HCV"
df$Q40_HCV_treatment_success[df$Q40_HCV_treatment_success == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q40_HCV_treatment_success[is.na(df$Q35_HCV_ever_diagnosed) | df$Q35_HCV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"
# ----------------------
# Code Missing Responses 
# ----------------------
df$Q40_HCV_treatment_success[is.na(df$Q40_HCV_treatment_success)] <- "9999: True Missing"

# ======================================================
# Q41_HIV_test_when
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q41_HIV_test_when)

# ----------------------
# Data Quality
# ----------------------
df$Q41_HIV_test_when[df$Q41_HIV_test_when == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q41_HIV_test_when[is.na(df$Q41_HIV_test_when)] <- "9999: True Missing"

# ======================================================
# Q42_HIV_test_result
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q42_HIV_test_result)

# ----------------------
# Data Quality
# ----------------------
df$Q42_HIV_test_result[df$Q42_HIV_test_result == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic 
# ----------------------
df$Q42_HIV_test_result[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q42_HIV_test_result[is.na(df$Q42_HIV_test_result)] <- "9999: True Missing"


# ======================================================
# Q43_HIV_ever_diagnosed
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q43_HIV_ever_diagnosed)

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q43_HIV_ever_diagnosed[is.na(df$Q43_HIV_ever_diagnosed)] <- "9999: True Missing"

# ----------------------
# Rename Levles
# ----------------------
df$Q43_HIV_ever_diagnosed[df$Q43_HIV_ever_diagnosed == "No (skip to #57 - next section)"] <- "No"
df$Q43_HIV_ever_diagnosed[df$Q43_HIV_ever_diagnosed == "Yes (continue to next question)"] <- "Yes"
df$Q43_HIV_ever_diagnosed[df$Q0_anon_code == "T0138" |
                            df$Q0_anon_code == "W0267" |
                            df$Q0_anon_code == "V1245" |
                            df$Q0_anon_code == "T0605" |
                            df$Q0_anon_code == "T0104" |
                            df$Q0_anon_code == "T0704" |
                            df$Q0_anon_code == "M5027" |
                            df$Q0_anon_code == "T0913" |
                            df$Q0_anon_code == "E0247" |
                            df$Q0_anon_code == "T0755" |
                            df$Q0_anon_code == "T0138"] <- "Yes"

# ----------------------
# Account for Skip Logic 
# ----------------------
df$Q43_HIV_ever_diagnosed[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"


# ======================================================
# Q44_HIV_first_diagnosed
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q44_HIV_first_diagnosed)
# ----------------------
# Clean Responses
# ----------------------
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "-" | 
                             df$Q44_HIV_first_diagnosed == "/" | 
                             df$Q44_HIV_first_diagnosed == "?" | 
                             df$Q44_HIV_first_diagnosed == "DID NOT" | 
                             df$Q44_HIV_first_diagnosed == "JAMAIS" | 
                             df$Q44_HIV_first_diagnosed == "n/a" | 
                             df$Q44_HIV_first_diagnosed == "N/A" | 
                             df$Q44_HIV_first_diagnosed == "na" | 
                             df$Q44_HIV_first_diagnosed == "NA" | 
                             df$Q44_HIV_first_diagnosed == "Neg" | 
                             df$Q44_HIV_first_diagnosed == "Neg" | 
                             df$Q44_HIV_first_diagnosed == "never" | 
                             df$Q44_HIV_first_diagnosed == "Never" | 
                             df$Q44_HIV_first_diagnosed == "19" | 
                             df$Q44_HIV_first_diagnosed == "no" | 
                             df$Q44_HIV_first_diagnosed == "No" | 
                             df$Q44_HIV_first_diagnosed == "not sure" | 
                             df$Q44_HIV_first_diagnosed == "np" | 
                             df$Q44_HIV_first_diagnosed == "X" | 
                             df$Q44_HIV_first_diagnosed == "21 years age" | 
                             df$Q44_HIV_first_diagnosed == "Yes" | 
                             df$Q44_HIV_first_diagnosed == "YES"] <- "7777: Poor Data Quality"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "0"] <- "2018"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "03"] <- "2003"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "12 ans [12 years]"] <- "2006"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "2 years"] <- "2016"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "2011/12"] <- "2011"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "25 yrs ago"] <- "1993"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "8 yrs ago"] <- "2010"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "86"] <- "1986"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "90"] <- "1990"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "98"] <- "1998"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "2003 (?)"] <- "2003"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "1985?"] <- "1985"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "GRID 1983 1985"] <- "1983"
df$Q44_HIV_first_diagnosed[df$Q44_HIV_first_diagnosed == "last year"] <- "2017"

df$Q44_HIV_first_diagnosed[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q44_HIV_first_diagnosed[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q44_HIV_first_diagnosed[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missing Responses 
# ----------------------
df$Q44_HIV_first_diagnosed[is.na(df$Q44_HIV_first_diagnosed)] <- "9999: True Missing"

# ======================================================
# Q45_HIV_current_risk_giving
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q45_HIV_current_risk_giving)

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q45_HIV_current_risk_giving[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q45_HIV_current_risk_giving[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q45_HIV_current_risk_giving[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missings
# ----------------------
df$Q45_HIV_current_risk_giving[is.na(df$Q45_HIV_current_risk_giving)] <- "9999: True Missing"

# ======================================================
# Q46_HIV_last_dr_visit
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q46_HIV_last_dr_visit)

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q46_HIV_last_dr_visit[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q46_HIV_last_dr_visit[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q46_HIV_last_dr_visit[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missings
# ----------------------
df$Q46_HIV_last_dr_visit[is.na(df$Q46_HIV_last_dr_visit)] <- "9999: True Missing"


# ======================================================
# Q47_HIV_current_ARV
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q47_HIV_current_ARV)

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q47_HIV_current_ARV[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q47_HIV_current_ARV[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q47_HIV_current_ARV[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Data Quality
# ----------------------
df$Q47_HIV_current_ARV[df$Q47_HIV_current_ARV == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missings
# ----------------------
df$Q47_HIV_current_ARV[is.na(df$Q47_HIV_current_ARV)] <- "9999: True Missing"

# ======================================================
# Q47_HIV_ever_ARV
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q47_HIV_ever_ARV)

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q47_HIV_ever_ARV[df$Q47_HIV_current_ARV == "Yes"] <- "8888: Yes, Currently taking"
df$Q47_HIV_ever_ARV[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q47_HIV_ever_ARV[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q47_HIV_ever_ARV[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missings
# ----------------------
df$Q47_HIV_ever_ARV[is.na(df$Q47_HIV_ever_ARV)] <- "9999: True Missing"

# ======================================================
# Q47_HIV_last_missed_ARV
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q47_HIV_last_missed_ARV)

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q47_HIV_last_missed_ARV[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q47_HIV_last_missed_ARV[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q47_HIV_last_missed_ARV[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Data Quality
# ----------------------
df$Q47_HIV_last_missed_ARV[df$Q47_HIV_last_missed_ARV == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missings
# ----------------------
df$Q47_HIV_last_missed_ARV[is.na(df$Q47_HIV_last_missed_ARV)] <- "9999: True Missing"

# ======================================================
# Q48_HIV_viral_load
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q48_HIV_viral_load)

# ----------------------
# Manage Skip Logic
# ----------------------
df$Q48_HIV_viral_load[df$Q41_HIV_test_when == "I have never tested for HIV"] <- "8888: I have never tested for HIV"
df$Q48_HIV_viral_load[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$Q48_HIV_viral_load[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Data Quality
# ----------------------
df$Q48_HIV_viral_load[df$Q48_HIV_viral_load == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Code Missings
# ----------------------
df$Q48_HIV_viral_load[is.na(df$Q48_HIV_viral_load)] <- "9999: True Missing"


# ======================================================
# Q49_HIV_current_risk_getting
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q49_HIV_current_risk_getting)

# ----------------------
# Data Quality
# ----------------------
df$Q49_HIV_current_risk_getting[df$Q49_HIV_current_risk_getting == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Skip Logic
# ----------------------
df$Q49_HIV_current_risk_getting[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: Already been diagnosed with HIV"
df$Q49_HIV_current_risk_getting[is.na(df$Q43_HIV_ever_diagnosed) | df$Q43_HIV_ever_diagnosed == "9999: True Missing"] <- "8888: Missing Response for Q35_HCV_ever_diagnosed"

# ----------------------
# Code Missings
# ----------------------
df$Q49_HIV_current_risk_getting[is.na(df$Q49_HIV_current_risk_getting)] <- "9999: True Missing"

# ======================================================
# Q50_PrEP_ever_used
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q50_PrEP_ever_used)

# ----------------------
# Rename Levels
# ----------------------
df$Q50_PrEP_ever_used[df$Q50_PrEP_ever_used == "Yes, I'm taking PrEP now! (skip to #62)"] <- "Yes, Currently"
df$Q50_PrEP_ever_used[df$Q50_PrEP_ever_used == "Yes, but I stopped. Why?"] <- "Yes, Previously"

# ----------------------
# Data Quality
# ----------------------
df$Q50_PrEP_ever_used[df$Q50_PrEP_ever_used == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Skip Logic
# ----------------------
df$Q50_PrEP_ever_used[df$Q43_HIV_ever_diagnosed == "Yes" & df$Q50_PrEP_ever_used == "No"] <- "8888: HIV-Positive"

# ----------------------
# Code Missings
# ----------------------
df$Q50_PrEP_ever_used[is.na(df$Q50_PrEP_ever_used)] <- "9999: True Missing"

# ======================================================
# Q50_PrEP_ever_used_why_stopped
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q50_PrEP_ever_used_why_stopped)

# ----------------------
# Rename Levels
# ----------------------
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used == "Yes, I'm taking PrEP now! (skip to #62)"] <- "Yes, Currently"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used == "Yes, but I stopped. Why?"] <- "Yes, Previously"

# ----------------------
# Skip Logic
# ----------------------
df$Q50_PrEP_ever_used_why_stopped[df$Q43_HIV_ever_diagnosed == "Yes" & df$Q50_PrEP_ever_used == "No"] <- "8888: HIV-Positive"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used == "No"] <- "8888: Never Used PrEP"

# ----------------------
# Code Missings
# ----------------------
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used_why_stopped == "(nothing written)"] <- "9999: True Missing"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used_why_stopped == "*nothing written"] <- "9999: True Missing"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used_why_stopped == "."] <- "9999: True Missing"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used_why_stopped == "*note* left this blank"] <- "9999: True Missing"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used_why_stopped == "[BLANK]"] <- "9999: True Missing"
df$Q50_PrEP_ever_used_why_stopped[df$Q50_PrEP_ever_used_why_stopped == "___"] <- "9999: True Missing"
df$Q50_PrEP_ever_used_why_stopped[is.na(df$Q50_PrEP_ever_used_why_stopped)] <- "9999: True Missing"

# ======================================================
# Q51_PrEP_interest
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q51_PrEP_interest)

# ----------------------
# Data Quality
# ----------------------
df$Q51_PrEP_interest[df$Q51_PrEP_interest == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Skip Logic
# ----------------------
df$Q51_PrEP_interest[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"

# ----------------------
# Code Missings
# ----------------------
df$Q51_PrEP_interest[is.na(df$Q51_PrEP_interest)] <- "9999: True Missing"

# ======================================================
# Q52 Barriers to PrEP
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q52_PrEP_not_interested_risk)
table(df$Q52_PrEP_not_interested_cost)
table(df$Q52_PrEP_not_interested_prescription)
table(df$Q52_PrEP_not_interested_side_effects)
table(df$Q52_PrEP_not_interested_pills)
table(df$Q52_PrEP_not_interested_testing)
table(df$Q52_PrEP_not_interested_judgement_community)
table(df$Q52_PrEP_not_interested_judgement_healthcare)
table(df$Q52_PrEP_not_interested_HIV_only)
table(df$Q52_PrEP_not_interested_none)
table(df$Q52_PrEP_not_interested_other)
table(df$Q52_PrEP_not_interested_other_text)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q52_PrEP_not_interested_other <- NA
df$Q52_PrEP_not_interested_other[is.na(df$Q52_PrEP_not_interested_other_text)] <- "No"
df$Q52_PrEP_not_interested_other[!is.na(df$Q52_PrEP_not_interested_other_text)] <- "Yes"
df$Q52_PrEP_not_interested_other_text[df$Q52_PrEP_not_interested_other == "No"] <- "8888: Other Not Selected"
df$Q52_PrEP_not_interested_other_text[is.na(df$Q52_PrEP_not_interested_other_text) & df$Q52_PrEP_not_interested_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q52_PrEP_not_interested_valid <- NA
df$Q52_PrEP_not_interested_valid[df$Q52_PrEP_not_interested_risk == "I don't think I will get HIV" |
                                   df$Q52_PrEP_not_interested_risk == "i don't think I will get HIV" |
                                   df$Q52_PrEP_not_interested_risk == "I don't think I will get HIV" |
                                   df$Q52_PrEP_not_interested_risk == "Multiple answers selected" |
                                   df$Q52_PrEP_not_interested_cost == "Costs too much" |
                                   df$Q52_PrEP_not_interested_prescription == "Can't get a prescription" |
                                   df$Q52_PrEP_not_interested_prescription == "Can't get a prescription" |
                                   df$Q52_PrEP_not_interested_side_effects == "Side effects" |
                                   df$Q52_PrEP_not_interested_pills == "Don't like taking pills" |
                                   df$Q52_PrEP_not_interested_pills == "Don't like taking pills" |
                                   df$Q52_PrEP_not_interested_testing == "Too much routine testing and clinic visits" |
                                   !is.na(df$Q52_PrEP_not_interested_judgement_community)|
                                   !is.na(df$Q52_PrEP_not_interested_judgement_healthcare) |
                                   df$Q52_PrEP_not_interested_HIV_only == "No protection from other STIs" |
                                   df$Q52_PrEP_not_interested_none == "None of the above" |
                                   df$Q52_PrEP_not_interested_other == "Yes"] <- "Yes"
df$Q52_PrEP_not_interested_valid[is.na(df$Q52_PrEP_not_interested_valid)] <- "No"

table(df$Q52_PrEP_not_interested_valid)

# ----------------------
# Data Quality
# ----------------------
df$Q52_PrEP_not_interested_risk[df$Q52_PrEP_not_interested_risk == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q52_PrEP_not_interested_risk[!is.na(df$Q52_PrEP_not_interested_risk)] <- "Yes"
df$Q52_PrEP_not_interested_cost[!is.na(df$Q52_PrEP_not_interested_cost)] <- "Yes"
df$Q52_PrEP_not_interested_prescription[!is.na(df$Q52_PrEP_not_interested_prescription)] <- "Yes"
df$Q52_PrEP_not_interested_side_effects[!is.na(df$Q52_PrEP_not_interested_side_effects)] <- "Yes"
df$Q52_PrEP_not_interested_pills[!is.na(df$Q52_PrEP_not_interested_pills)] <- "Yes"
df$Q52_PrEP_not_interested_testing[!is.na(df$Q52_PrEP_not_interested_testing)] <- "Yes"
df$Q52_PrEP_not_interested_judgement_community[!is.na(df$Q52_PrEP_not_interested_judgement_community)] <- "Yes"
df$Q52_PrEP_not_interested_judgement_healthcare[!is.na(df$Q52_PrEP_not_interested_judgement_healthcare)] <- "Yes"
df$Q52_PrEP_not_interested_HIV_only[!is.na(df$Q52_PrEP_not_interested_HIV_only)] <- "Yes"
df$Q52_PrEP_not_interested_none[!is.na(df$Q52_PrEP_not_interested_none)] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q52_PrEP_not_interested_risk[is.na(df$Q52_PrEP_not_interested_risk) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_cost[is.na(df$Q52_PrEP_not_interested_cost) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_prescription[is.na(df$Q52_PrEP_not_interested_prescription) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_side_effects[is.na(df$Q52_PrEP_not_interested_side_effects) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_pills[is.na(df$Q52_PrEP_not_interested_pills) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_testing[is.na(df$Q52_PrEP_not_interested_testing) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_judgement_community[is.na(df$Q52_PrEP_not_interested_judgement_community) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_judgement_healthcare[is.na(df$Q52_PrEP_not_interested_judgement_healthcare) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_HIV_only[is.na(df$Q52_PrEP_not_interested_HIV_only) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_none[is.na(df$Q52_PrEP_not_interested_none) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"
df$Q52_PrEP_not_interested_other[is.na(df$Q52_PrEP_not_interested_other) & df$Q52_PrEP_not_interested_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q52_PrEP_not_interested_risk[df$Q52_PrEP_not_interested_risk == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_risk == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_cost[df$Q52_PrEP_not_interested_cost == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_cost == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_prescription[df$Q52_PrEP_not_interested_prescription == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_prescription == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_side_effects[df$Q52_PrEP_not_interested_side_effects == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_side_effects == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_pills[df$Q52_PrEP_not_interested_pills == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_pills == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_testing[df$Q52_PrEP_not_interested_testing == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_testing == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_judgement_community[df$Q52_PrEP_not_interested_judgement_community == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_judgement_community == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_judgement_healthcare[df$Q52_PrEP_not_interested_judgement_healthcare == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_judgement_healthcare == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_HIV_only[df$Q52_PrEP_not_interested_HIV_only == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_HIV_only == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

df$Q52_PrEP_not_interested_other[df$Q52_PrEP_not_interested_other == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"
df$Q52_PrEP_not_interested_none[df$Q52_PrEP_not_interested_other == "Yes" & df$Q52_PrEP_not_interested_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q52_PrEP_not_interested_risk[is.na(df$Q52_PrEP_not_interested_risk) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_cost[is.na(df$Q52_PrEP_not_interested_cost) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_prescription[is.na(df$Q52_PrEP_not_interested_prescription) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_side_effects[is.na(df$Q52_PrEP_not_interested_side_effects) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_pills[is.na(df$Q52_PrEP_not_interested_pills) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_testing[is.na(df$Q52_PrEP_not_interested_testing) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_judgement_community[is.na(df$Q52_PrEP_not_interested_judgement_community) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_judgement_healthcare[is.na(df$Q52_PrEP_not_interested_judgement_healthcare) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_HIV_only[is.na(df$Q52_PrEP_not_interested_HIV_only) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_none[is.na(df$Q52_PrEP_not_interested_none) & df$Q52_PrEP_not_interested_valid == "No"] <- "9999: True Missing"
df$Q52_PrEP_not_interested_other[df$Q52_PrEP_not_interested_none == "9999: True Missing"] <- "9999: True Missing"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q52_PrEP_not_interested_risk[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_cost[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_prescription[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_side_effects[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_pills[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_testing[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_judgement_community[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_judgement_healthcare[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_HIV_only[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_none[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
df$Q52_PrEP_not_interested_other[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

df$Q52_PrEP_not_interested_risk[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_cost[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_prescription[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_side_effects[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_pills[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_testing[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_judgement_community[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_judgement_healthcare[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_HIV_only[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_none[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"
df$Q52_PrEP_not_interested_other[df$Q50_PrEP_ever_used == "Yes, Currently"] <- "8888: Taking PrEP"

# ======================================================
# Q53_HIRI_age
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_age)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_age[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_age[is.na(df$Q53_HIRI_age)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_num_partners
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_num_partners)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_num_partners[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_num_partners[is.na(df$Q53_HIRI_num_partners)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_bottom_no_condom
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_bottom_no_condom)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_bottom_no_condom[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_bottom_no_condom[is.na(df$Q53_HIRI_bottom_no_condom)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_HIV_pos_partners
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_HIV_pos_partners)

# ----------------------
# Data Quality
# ----------------------
df$Q53_HIRI_HIV_pos_partners[df$Q53_HIRI_HIV_pos_partners == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_HIV_pos_partners[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_HIV_pos_partners[is.na(df$Q53_HIRI_HIV_pos_partners)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_top_no_condom
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_top_no_condom)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_top_no_condom[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_top_no_condom[is.na(df$Q53_HIRI_top_no_condom)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_meth
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_meth)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_meth[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_meth[is.na(df$Q53_HIRI_meth)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_poppers
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_poppers)

# ----------------------
# Data Quality
# ----------------------
df$Q53_HIRI_poppers[df$Q53_HIRI_poppers == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_poppers[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_poppers[is.na(df$Q53_HIRI_poppers)] <- "9999: True Missing"

# ======================================================
# Q53_HIRI_total
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q53_HIRI_total)
df$Q53_HIRI_total[df$Q53_HIRI_total == "0 (DON'T KNOW)"] <- "7777: Poor Data Quality"
df$Q53_HIRI_total[df$Q53_HIRI_total == "8/18"] <- "7777: Poor Data Quality"
df$Q53_HIRI_total[df$Q53_HIRI_total == "22 :O "] <- "22"


# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_total[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"

# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_total[df$Q53_HIRI_age == "7777: Poor Data Quality" | 
                    df$Q53_HIRI_num_partners == "7777: Poor Data Quality" | 
                    df$Q53_HIRI_bottom_no_condom == "7777: Poor Data Quality" |
                    df$Q53_HIRI_HIV_pos_partners == "7777: Poor Data Quality" |
                    df$Q53_HIRI_top_no_condom == "7777: Poor Data Quality" |
                    df$Q53_HIRI_meth == "7777: Poor Data Quality" |
                    df$Q53_HIRI_poppers == "7777: Poor Data Quality"] <- "9999: True Missing"
df$Q53_HIRI_total[df$Q53_HIRI_age == "9999: True Missing" | 
                    df$Q53_HIRI_num_partners == "9999: True Missing" | 
                    df$Q53_HIRI_bottom_no_condom == "9999: True Missing" |
                    df$Q53_HIRI_HIV_pos_partners == "9999: True Missing" |
                    df$Q53_HIRI_top_no_condom == "9999: True Missing" |
                    df$Q53_HIRI_meth == "9999: True Missing" |
                    df$Q53_HIRI_poppers == "9999: True Missing"] <- "9999: True Missing"
df$Q53_HIRI_total[is.na(df$Q53_HIRI_age) | 
                    is.na(df$Q53_HIRI_num_partners) | 
                    is.na(df$Q53_HIRI_bottom_no_condom) |
                    is.na(df$Q53_HIRI_HIV_pos_partners) |
                    is.na(df$Q53_HIRI_top_no_condom) |
                    is.na(df$Q53_HIRI_meth) |
                    is.na(df$Q53_HIRI_poppers)] <- "9999: True Missing"
table(df$Q53_HIRI_total)

# ----------------------
# Calculate HIRI Score
# ----------------------
table(df$Q53_HIRI_age)
df$Q53_HIRI_1 <- NA
df$Q53_HIRI_1 <- as.numeric(df$Q53_HIRI_1)
df$Q53_HIRI_1[df$Q53_HIRI_age == "<18 years (0)" | 
                df$Q53_HIRI_age == ">=49 years (0)" | 
                df$Q53_HIRI_age == "49+ years (0)"] <- 0
df$Q53_HIRI_1[df$Q53_HIRI_age == "41-48 years (2)"] <- 2
df$Q53_HIRI_1[df$Q53_HIRI_age == "29-40 years (5)"] <- 5
df$Q53_HIRI_1[df$Q53_HIRI_age == "18-28 years (8)"] <- 8
table(df$Q53_HIRI_1)

table(df$Q53_HIRI_num_partners)
df$Q53_HIRI_2 <- NA
df$Q53_HIRI_2 <- as.numeric(df$Q53_HIRI_2)
df$Q53_HIRI_2[df$Q53_HIRI_num_partners == "0-5 (0)"] <- 0
df$Q53_HIRI_2[df$Q53_HIRI_num_partners == "6-10 (4)"] <- 4
df$Q53_HIRI_2[df$Q53_HIRI_num_partners == "More than 10 (7)"] <- 7
table(df$Q53_HIRI_2)

table(df$Q53_HIRI_bottom_no_condom)
df$Q53_HIRI_3 <- NA
df$Q53_HIRI_3 <- as.numeric(df$Q53_HIRI_3)
df$Q53_HIRI_3[df$Q53_HIRI_bottom_no_condom == "No (0)"] <- 0
df$Q53_HIRI_3[df$Q53_HIRI_bottom_no_condom == "Yes (10)"] <- 10
table(df$Q53_HIRI_3)

table(df$Q53_HIRI_HIV_pos_partners)
df$Q53_HIRI_4 <- NA
df$Q53_HIRI_4 <- as.numeric(df$Q53_HIRI_4)
df$Q53_HIRI_4[df$Q53_HIRI_HIV_pos_partners == "0 (0)"] <- 0
df$Q53_HIRI_4[df$Q53_HIRI_HIV_pos_partners == "1 (4)"] <- 4
df$Q53_HIRI_4[df$Q53_HIRI_HIV_pos_partners == ">1 (8)"] <- 8
table(df$Q53_HIRI_4)

table(df$Q53_HIRI_top_no_condom)
df$Q53_HIRI_5 <- NA
df$Q53_HIRI_5 <- as.numeric(df$Q53_HIRI_5)
df$Q53_HIRI_5[df$Q53_HIRI_top_no_condom == "0-4 times (0)"] <- 0
df$Q53_HIRI_5[df$Q53_HIRI_top_no_condom == "5+ times (6)"] <- 6
table(df$Q53_HIRI_5)

table(df$Q53_HIRI_meth)
df$Q53_HIRI_6 <- NA
df$Q53_HIRI_6 <- as.numeric(df$Q53_HIRI_6)
df$Q53_HIRI_6[df$Q53_HIRI_meth == "No (0)"] <- 0
df$Q53_HIRI_6[df$Q53_HIRI_meth == "Yes (6)"] <- 6
table(df$Q53_HIRI_6)

table(df$Q53_HIRI_poppers)
df$Q53_HIRI_7 <- NA
df$Q53_HIRI_7 <- as.numeric(df$Q53_HIRI_7)
df$Q53_HIRI_7[df$Q53_HIRI_poppers == "No (0)"] <- 0
df$Q53_HIRI_7[df$Q53_HIRI_poppers == "Yes (3)"] <- 3
table(df$Q53_HIRI_7)

df$Q53_HIRI_total_calculated <- NA
df$Q53_HIRI_total_calculated <- df$Q53_HIRI_1 + df$Q53_HIRI_2 + df$Q53_HIRI_3 + df$Q53_HIRI_4 + df$Q53_HIRI_5 + df$Q53_HIRI_6 + df$Q53_HIRI_7
table(df$Q53_HIRI_total_calculated)
# ----------------------
# Account for Skip Logic
# ----------------------
df$Q53_HIRI_total_calculated[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-positive"
# ----------------------
# Account for Missing
# ----------------------
df$Q53_HIRI_total_calculated[df$Q53_HIRI_age == "7777: Poor Data Quality" | 
                               df$Q53_HIRI_num_partners == "7777: Poor Data Quality" | 
                               df$Q53_HIRI_bottom_no_condom == "7777: Poor Data Quality" |
                               df$Q53_HIRI_HIV_pos_partners == "7777: Poor Data Quality" |
                               df$Q53_HIRI_top_no_condom == "7777: Poor Data Quality" |
                               df$Q53_HIRI_meth == "7777: Poor Data Quality" |
                               df$Q53_HIRI_poppers == "7777: Poor Data Quality"] <- "9999: True Missing"
df$Q53_HIRI_total_calculated[df$Q53_HIRI_age == "9999: True Missing" | 
                               df$Q53_HIRI_num_partners == "9999: True Missing" | 
                               df$Q53_HIRI_bottom_no_condom == "9999: True Missing" |
                               df$Q53_HIRI_HIV_pos_partners == "9999: True Missing" |
                               df$Q53_HIRI_top_no_condom == "9999: True Missing" |
                               df$Q53_HIRI_meth == "9999: True Missing" |
                               df$Q53_HIRI_poppers == "9999: True Missing"] <- "9999: True Missing"
df$Q53_HIRI_total_calculated[is.na(df$Q53_HIRI_age) | 
                               is.na(df$Q53_HIRI_num_partners) | 
                               is.na(df$Q53_HIRI_bottom_no_condom) |
                               is.na(df$Q53_HIRI_HIV_pos_partners) |
                               is.na(df$Q53_HIRI_top_no_condom) |
                               is.na(df$Q53_HIRI_meth) |
                               is.na(df$Q53_HIRI_poppers)] <- "9999: True Missing"
table(df$Q53_HIRI_total_calculated)


# ======================================================
# Q54_PrEP_how_using
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q54_PrEP_how_using)
table(df$Q54_PrEP_how_using_other_text)

# ----------------------
# Account for Skip Logic
# ----------------------
table(df$Q54_PrEP_how_using_other_text)
df$Q54_PrEP_how_using[df$Q50_PrEP_ever_used == "No"] <- "8888: Never on PrEP"
df$Q54_PrEP_how_using[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"

# ----------------------
# Label Missing
# ----------------------
df$Q54_PrEP_how_using[is.na(df$Q54_PrEP_how_using)] <- "9999: True Missing"
df$Q54_PrEP_how_using_other_text[df$Q54_PrEP_how_using != "Other"] <- "8888: Other Not Selected"
df$Q54_PrEP_how_using_other_text[is.na(df$Q52_PrEP_not_interested_other_text) & df$Q54_PrEP_how_using == "Other"] <- "9999: True Missing"


# ======================================================
# Q55 PrEP Duration
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q55_PrEP_months_used)
table(df$Q55_PrEP_years_used)
# ----------------------
# Clean Data
# ----------------------
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "(checkmark)"] <- "7777: Poor Data Quality"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "(illegible)"] <- "7777: Poor Data Quality"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "???"] <- "7777: Poor Data Quality"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "N/A"] <- "7777: Poor Data Quality"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "X"] <- "7777: Poor Data Quality"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "No"] <- "7777: Poor Data Quality"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "1 (just started)"] <- "1"
df$Q55_PrEP_months_used[df$Q55_PrEP_months_used == "(3 days)"] <- "0"

df$Q55_PrEP_years_used[df$Q55_PrEP_years_used == "(checkmark)"] <- "7777: Poor Data Quality"
df$Q55_PrEP_years_used[df$Q55_PrEP_years_used == "(illegible)"] <- "7777: Poor Data Quality"
df$Q55_PrEP_years_used[df$Q55_PrEP_years_used == "???"]<- "7777: Poor Data Quality"
df$Q55_PrEP_years_used[df$Q55_PrEP_years_used == "X"]<- "7777: Poor Data Quality"
df$Q55_PrEP_years_used[df$Q55_PrEP_years_used == "N/A"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q55_PrEP_years_used[df$Q50_PrEP_ever_used == "No"] <- "8888: Never on PrEP"
df$Q55_PrEP_years_used[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$Q55_PrEP_months_used[df$Q50_PrEP_ever_used == "No"] <- "8888: Never on PrEP"
df$Q55_PrEP_months_used[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"

# ----------------------
# Label Missing
# ----------------------
df$Q55_PrEP_months_used[is.na(df$Q55_PrEP_months_used)] <- "9999: True Missing"
df$Q55_PrEP_years_used[is.na(df$Q55_PrEP_years_used)] <- "9999: True Missing"



# ======================================================
# Q56_PrEP_days_taken
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q56_PrEP_days_taken)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q56_PrEP_days_taken[df$Q50_PrEP_ever_used != "Yes, Currently"] <- "8888: Not Currently on PrEP"
df$Q56_PrEP_days_taken[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"

# ----------------------
# Data Quality
# ----------------------
df$Q56_PrEP_days_taken[df$Q56_PrEP_days_taken == "120"] <- "30"
df$Q56_PrEP_days_taken[df$Q56_PrEP_days_taken == "ALL"] <- "30"
df$Q56_PrEP_days_taken[df$Q56_PrEP_days_taken == "everyday"] <- "30"
df$Q56_PrEP_days_taken[df$Q56_PrEP_days_taken == "56"] <- "7777: Poor Data Quality"

# ----------------------
# Label Missing
# ----------------------
df$Q56_PrEP_days_taken[is.na(df$Q56_PrEP_days_taken)] <- "9999: True Missing"

# ======================================================
# Q57 Depression
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q57_depression_little_interest)
table(df$Q57_depression_feeling_down)
table(df$Q57_depression_feeling_nervous)
table(df$Q57_depression_uncontrolled_worrying)

# ----------------------
# Data Quality
# ----------------------
df$Q57_depression_little_interest[df$Q57_depression_little_interest == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q57_depression_feeling_down[df$Q57_depression_feeling_down == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q57_depression_feeling_nervous[df$Q57_depression_feeling_nervous == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q57_depression_uncontrolled_worrying[df$Q57_depression_uncontrolled_worrying == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Data Quality
# ----------------------
df$Q57_depression_little_interest[is.na(df$Q57_depression_little_interest)] <- "9999: True Missing"
df$Q57_depression_feeling_down[is.na(df$Q57_depression_feeling_down)] <- "9999: True Missing"
df$Q57_depression_feeling_nervous[is.na(df$Q57_depression_feeling_nervous)] <- "9999: True Missing"
df$Q57_depression_uncontrolled_worrying[is.na(df$Q57_depression_uncontrolled_worrying)] <- "9999: True Missing"

# ----------------------
# Calculate GAD & PHQ Scores
# ----------------------
df$Q57_depression_1 <- NA
df$Q57_depression_1[df$Q57_depression_little_interest == "Nearly every day"] <- 3
df$Q57_depression_1[df$Q57_depression_little_interest == "More than half the days"] <- 2
df$Q57_depression_1[df$Q57_depression_little_interest == "Several days"] <- 1
df$Q57_depression_1[df$Q57_depression_little_interest == "Not at all"] <- 0

df$Q57_depression_2 <- NA
df$Q57_depression_2[df$Q57_depression_feeling_down == "Nearly every day"] <- 3
df$Q57_depression_2[df$Q57_depression_feeling_down == "More than half the days"] <- 2
df$Q57_depression_2[df$Q57_depression_feeling_down == "Several days"] <- 1
df$Q57_depression_2[df$Q57_depression_feeling_down == "Not at all"] <- 0

df$Q57_anxiety_1 <- NA
df$Q57_anxiety_1[df$Q57_depression_feeling_nervous == "Nearly every day"] <- 3
df$Q57_anxiety_1[df$Q57_depression_feeling_nervous == "More than half the days"] <- 2
df$Q57_anxiety_1[df$Q57_depression_feeling_nervous == "Several days"] <- 1
df$Q57_anxiety_1[df$Q57_depression_feeling_nervous == "Not at all"] <- 0

df$Q57_anxiety_2 <- NA
df$Q57_anxiety_2[df$Q57_depression_uncontrolled_worrying == "Nearly every day"] <- 3
df$Q57_anxiety_2[df$Q57_depression_uncontrolled_worrying == "More than half the days"] <- 2
df$Q57_anxiety_2[df$Q57_depression_uncontrolled_worrying == "Several days"] <- 1
df$Q57_anxiety_2[df$Q57_depression_uncontrolled_worrying == "Not at all"] <- 0

df$Q57_depression <- NA
df$Q57_depression <- df$Q57_depression_1 + df$Q57_depression_2
df$Q57_depression[is.na(df$Q57_depression_1)] <- "9999: True Missing"
df$Q57_depression[is.na(df$Q57_depression_2)] <- "9999: True Missing"
df$Q57_depression[is.na(df$Q57_depression)] <- "9999: True Missing"

df$Q57_anxiety <- NA
df$Q57_anxiety<- df$Q57_anxiety_1 + df$Q57_anxiety_2
df$Q57_anxiety[is.na(df$Q57_anxiety_1)] <- "9999: True Missing"
df$Q57_anxiety[is.na(df$Q57_anxiety_2)] <- "9999: True Missing"
df$Q57_anxiety[is.na(df$Q57_anxiety)] <- "9999: True Missing"

# ======================================================
# Q58 Want Help
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q58_want_help_depression)
table(df$Q58_want_help_anxiety)
table(df$Q58_want_help_coming_out)
table(df$Q58_want_help_gender)
table(df$Q58_want_help_eating_disorder)
table(df$Q58_want_help_body_image)
table(df$Q58_want_help_relationship_problems)
table(df$Q58_want_help_suicidal_thoughts)
table(df$Q58_want_help_none)
table(df$Q58_want_help_other)
table(df$Q58_want_help_other_text)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q58_want_help_other <- NA
df$Q58_want_help_other[is.na(df$Q58_want_help_other_text)] <- "No"
df$Q58_want_help_other[!is.na(df$Q58_want_help_other_text)] <- "Yes"
df$Q58_want_help_other_text[df$Q58_want_help_other == "No"] <- "8888: Other Not Selected"
df$Q58_want_help_other_text[is.na(df$Q58_want_help_other_text) & df$Q58_want_help_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q58_want_help_body_image[df$Q58_want_help_body_image == "Body Image"] <- "Body image"

df$Q58_want_help_valid <- NA
df$Q58_want_help_valid[df$Q58_want_help_depression == "Depression" |
                         df$Q58_want_help_anxiety == "Anxiety" |
                         df$Q58_want_help_coming_out == "Coming out" |
                         df$Q58_want_help_gender == "Gender dysphoria and/or transition" |
                         df$Q58_want_help_eating_disorder == "Eating disorders" |
                         df$Q58_want_help_body_image == "Body image" |
                         df$Q58_want_help_relationship_problems == "Relationship problems" |
                         df$Q58_want_help_suicidal_thoughts == "Suicidal thoughts" |
                         df$Q58_want_help_none == "None of the above" |
                         df$Q58_want_help_other == "Yes"] <- "Yes"
df$Q58_want_help_valid[is.na(df$Q58_want_help_valid)] <- "No"

table(df$Q58_want_help_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q58_want_help_depression[df$Q58_want_help_depression == "Depression"] <- "Yes"
df$Q58_want_help_anxiety[df$Q58_want_help_anxiety == "Anxiety"] <- "Yes"
df$Q58_want_help_coming_out[df$Q58_want_help_coming_out == "Coming out"] <- "Yes"
df$Q58_want_help_gender[df$Q58_want_help_gender == "Gender dysphoria and/or transition"] <- "Yes"
df$Q58_want_help_eating_disorder[df$Q58_want_help_eating_disorder == "Eating disorders"] <- "Yes"
df$Q58_want_help_body_image[df$Q58_want_help_body_image == "Body image"] <- "Yes"
df$Q58_want_help_relationship_problems[df$Q58_want_help_relationship_problems == "Relationship problems"] <- "Yes"
df$Q58_want_help_suicidal_thoughts[df$Q58_want_help_suicidal_thoughts == "Suicidal thoughts"] <- "Yes"
df$Q58_want_help_none[df$Q58_want_help_none == "None of the above"] <- "Yes"
df$Q58_want_help_other[df$Q58_want_help_other == "Yes"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q58_want_help_depression[is.na(df$Q58_want_help_depression) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_anxiety[is.na(df$Q58_want_help_anxiety) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_coming_out[is.na(df$Q58_want_help_coming_out) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_gender[is.na(df$Q58_want_help_gender) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_eating_disorder[is.na(df$Q58_want_help_eating_disorder) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_body_image[is.na(df$Q58_want_help_body_image) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_relationship_problems[is.na(df$Q58_want_help_relationship_problems) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_suicidal_thoughts[is.na(df$Q58_want_help_suicidal_thoughts) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_none[is.na(df$Q58_want_help_none) & df$Q58_want_help_valid == "Yes"] <- "No"
df$Q58_want_help_other[is.na(df$Q58_want_help_other) & df$Q58_want_help_valid == "Yes"] <- "No"


# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q58_want_help_depression[df$Q58_want_help_depression == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_depression == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_anxiety[df$Q58_want_help_anxiety == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_anxiety == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_coming_out[df$Q58_want_help_coming_out == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_coming_out == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_gender[df$Q58_want_help_gender == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_gender == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_eating_disorder[df$Q58_want_help_eating_disorder == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_eating_disorder == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_body_image[df$Q58_want_help_body_image == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_body_image == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_relationship_problems[df$Q58_want_help_relationship_problems == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_relationship_problems == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_suicidal_thoughts[df$Q58_want_help_suicidal_thoughts == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_suicidal_thoughts == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

df$Q58_want_help_other[df$Q58_want_help_other == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"
df$Q58_want_help_none[df$Q58_want_help_other == "Yes" & df$Q58_want_help_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q58_want_help_depression[is.na(df$Q58_want_help_depression) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_anxiety[is.na(df$Q58_want_help_anxiety) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_coming_out[is.na(df$Q58_want_help_coming_out) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_gender[is.na(df$Q58_want_help_gender) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_eating_disorder[is.na(df$Q58_want_help_eating_disorder) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_body_image[is.na(df$Q58_want_help_body_image) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_relationship_problems[is.na(df$Q58_want_help_relationship_problems) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_suicidal_thoughts[is.na(df$Q58_want_help_suicidal_thoughts) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_none[is.na(df$Q58_want_help_none) & df$Q58_want_help_valid == "No"] <- "9999: True Missing"
df$Q58_want_help_other[df$Q58_want_help_none == "9999: True Missing"] <- "9999: True Missing"

# ======================================================
# Q59 Mental Health
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q59_mental_health_pro_elder)
table(df$Q59_mental_health_pro_psychiatrist)
table(df$Q59_mental_health_pro_counsellor)
table(df$Q59_mental_health_pro_social_worker)
table(df$Q59_mental_health_pro_knowledge_keeper)
table(df$Q59_mental_health_pro_psychologist)
table(df$Q59_mental_health_pro_peer)
table(df$Q59_mental_health_pro_sex_therapist)
table(df$Q59_mental_health_pro_none)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q59_mental_health_pro_valid <- NA
df$Q59_mental_health_pro_valid[df$Q59_mental_health_pro_elder == "Elder (Indigenous)" |
                                 df$Q59_mental_health_pro_psychiatrist == "Psychiatrist" |
                                 df$Q59_mental_health_pro_counsellor == "Registered Counsellor" |
                                 df$Q59_mental_health_pro_social_worker == "Social worker" |
                                 df$Q59_mental_health_pro_knowledge_keeper == "Knowledge Keeper (Indigenous)" |
                                 df$Q59_mental_health_pro_psychologist == "Clinical Psychologist" |
                                 df$Q59_mental_health_pro_peer == "Peer counsellor/navigator" |
                                 df$Q59_mental_health_pro_sex_therapist == "Sex therapist / sexologist" |
                                 df$Q59_mental_health_pro_none == "None of the above"] <- "Yes"
df$Q59_mental_health_pro_valid[is.na(df$Q59_mental_health_pro_valid)] <- "No"

table(df$Q59_mental_health_pro_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q59_mental_health_pro_elder[df$Q59_mental_health_pro_elder == "Elder (Indigenous)"] <- "Yes"
df$Q59_mental_health_pro_psychiatrist[df$Q59_mental_health_pro_psychiatrist == "Psychiatrist"] <- "Yes"
df$Q59_mental_health_pro_counsellor[df$Q59_mental_health_pro_counsellor == "Registered Counsellor"] <- "Yes"
df$Q59_mental_health_pro_social_worker[df$Q59_mental_health_pro_social_worker == "Social worker"] <- "Yes"
df$Q59_mental_health_pro_knowledge_keeper[df$Q59_mental_health_pro_knowledge_keeper == "Knowledge Keeper (Indigenous)"] <- "Yes"
df$Q59_mental_health_pro_psychologist[df$Q59_mental_health_pro_psychologist == "Clinical Psychologist"] <- "Yes"
df$Q59_mental_health_pro_peer[df$Q59_mental_health_pro_peer == "Peer counsellor/navigator"] <- "Yes"
df$Q59_mental_health_pro_sex_therapist[df$Q59_mental_health_pro_sex_therapist == "Sex therapist / sexologist"] <- "Yes"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_none == "None of the above"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q59_mental_health_pro_elder[is.na(df$Q59_mental_health_pro_elder) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_psychiatrist[is.na(df$Q59_mental_health_pro_psychiatrist) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_counsellor[is.na(df$Q59_mental_health_pro_counsellor) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_social_worker[is.na(df$Q59_mental_health_pro_social_worker) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_knowledge_keeper[is.na(df$Q59_mental_health_pro_knowledge_keeper) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_psychologist[is.na(df$Q59_mental_health_pro_psychologist) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_peer[is.na(df$Q59_mental_health_pro_peer) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_sex_therapist[is.na(df$Q59_mental_health_pro_sex_therapist) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"
df$Q59_mental_health_pro_none[is.na(df$Q59_mental_health_pro_none) & df$Q59_mental_health_pro_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q59_mental_health_pro_elder[df$Q59_mental_health_pro_elder == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_elder == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_psychiatrist[df$Q59_mental_health_pro_psychiatrist == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_psychiatrist == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_counsellor[df$Q59_mental_health_pro_counsellor == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_counsellor == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_social_worker[df$Q59_mental_health_pro_social_worker == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_social_worker == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_knowledge_keeper[df$Q59_mental_health_pro_knowledge_keeper == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_knowledge_keeper == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_psychologist[df$Q59_mental_health_pro_psychologist == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_psychologist == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_peer[df$Q59_mental_health_pro_peer == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_peer == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

df$Q59_mental_health_pro_sex_therapist[df$Q59_mental_health_pro_sex_therapist == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"
df$Q59_mental_health_pro_none[df$Q59_mental_health_pro_sex_therapist == "Yes" & df$Q59_mental_health_pro_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q59_mental_health_pro_elder[is.na(df$Q59_mental_health_pro_elder) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_psychiatrist[is.na(df$Q59_mental_health_pro_psychiatrist) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_counsellor[is.na(df$Q59_mental_health_pro_counsellor) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_social_worker[is.na(df$Q59_mental_health_pro_social_worker) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_knowledge_keeper[is.na(df$Q59_mental_health_pro_knowledge_keeper) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_psychologist[is.na(df$Q59_mental_health_pro_psychologist) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_peer[is.na(df$Q59_mental_health_pro_peer) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_sex_therapist[is.na(df$Q59_mental_health_pro_sex_therapist) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"
df$Q59_mental_health_pro_none[is.na(df$Q59_mental_health_pro_none) & df$Q59_mental_health_pro_valid == "No"] <- "9999: True Missing"

# ======================================================
# Q60_substances_any
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q60_substances_any)

# ----------------------
# Data Quality
# ----------------------
df$Q60_substances_any[df$Q60_substances_any == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Relabel Levels
# ----------------------
df$Q60_substances_any[df$Q60_substances_any == "No (skip to #70 - next page)"] <- "No"

# ----------------------
# Relabel missings
# ----------------------
df$Q60_substances_any[is.na(df$Q60_substances_any)] <- "9999: True Missing"

# ======================================================
# Q61 Substances Used
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q61_substances_alcohol)
table(df$Q61_substances_alcohol_sex)
table(df$Q61_substances_tobacco)
table(df$Q61_substances_tobacco_sex)
table(df$Q61_substances_marijuana)
table(df$Q61_substances_marijuana_sex)
table(df$Q61_substances_poppers)
table(df$Q61_substances_poppers_sex)
table(df$Q61_substances_ketamine)
table(df$Q61_substances_ketamine_sex)
table(df$Q61_substances_ecstasy)
table(df$Q61_substances_ecstasy_sex)
table(df$Q61_substances_crystal)
table(df$Q61_substances_crystal_sex)
table(df$Q61_substances_erection)
table(df$Q61_substances_erection_sex)
table(df$Q61_substances_crack)
table(df$Q61_substances_crack_sex)
table(df$Q61_substances_cocaine)
table(df$Q61_substances_cocaine_sex)
table(df$Q61_substances_heroin)
table(df$Q61_substances_heroin_sex)
table(df$Q61_substances_other_opioids)
table(df$Q61_substances_other_opioids_sex)
table(df$Q61_substances_fentanyl)
table(df$Q61_substances_fentanyl_sex)
table(df$Q61_substances_GHB)
table(df$Q61_substances_GHB_sex)
table(df$Q61_substances_benzos)
table(df$Q61_substances_benzos_sex)
table(df$Q61_substances_psychedelics)
table(df$Q61_substances_psychedelics_sex)
table(df$Q61_substances_steroids)
table(df$Q61_substances_steroids_sex)
table(df$Q61_substances_other)
table(df$Q61_substances_other_sex)
table(df$Q61_substances_other_text)

# ----------------------
# Backcode Sex as occuring in Main Variable
# ----------------------
df$Q61_substances_alcohol[!is.na(df$Q61_substances_alcohol_sex)] <- "Yes"
df$Q61_substances_tobacco[!is.na(df$Q61_substances_tobacco_sex)] <- "Yes"
df$Q61_substances_marijuana[!is.na(df$Q61_substances_marijuana_sex)] <- "Yes"
df$Q61_substances_poppers[!is.na(df$Q61_substances_poppers_sex)] <- "Yes"
df$Q61_substances_ketamine[!is.na(df$Q61_substances_ketamine_sex)] <- "Yes"
df$Q61_substances_ecstasy[!is.na(df$Q61_substances_ecstasy_sex)] <- "Yes"
df$Q61_substances_crystal[!is.na(df$Q61_substances_crystal_sex)] <- "Yes"
df$Q61_substances_erection[!is.na(df$Q61_substances_erection_sex)] <- "Yes"
df$Q61_substances_crack[!is.na(df$Q61_substances_crack_sex)] <- "Yes"
df$Q61_substances_cocaine[!is.na(df$Q61_substances_cocaine_sex)] <- "Yes"
df$Q61_substances_heroin[!is.na(df$Q61_substances_heroin_sex)] <- "Yes"
df$Q61_substances_other_opioids[!is.na(df$Q61_substances_other_opioids_sex)] <- "Yes"
df$Q61_substances_fentanyl[!is.na(df$Q61_substances_fentanyl_sex)] <- "Yes"
df$Q61_substances_GHB[!is.na(df$Q61_substances_GHB_sex)] <- "Yes"
df$Q61_substances_benzos[!is.na(df$Q61_substances_benzos_sex)] <- "Yes"
df$Q61_substances_psychedelics[!is.na(df$Q61_substances_psychedelics_sex)] <- "Yes"
df$Q61_substances_steroids[!is.na(df$Q61_substances_steroids_sex)] <- "Yes"
df$Q61_substances_other[!is.na(df$Q61_substances_other_sex)] <- "Yes"

# ----------------------
# Rename levels
# ----------------------
df$Q61_substances_alcohol[!is.na(df$Q61_substances_alcohol)] <- "Yes"
df$Q61_substances_tobacco[!is.na(df$Q61_substances_tobacco)] <- "Yes"
df$Q61_substances_marijuana[!is.na(df$Q61_substances_marijuana)] <- "Yes"
df$Q61_substances_poppers[!is.na(df$Q61_substances_poppers)] <- "Yes"
df$Q61_substances_ketamine[!is.na(df$Q61_substances_ketamine)] <- "Yes"
df$Q61_substances_ecstasy[!is.na(df$Q61_substances_ecstasy)] <- "Yes"
df$Q61_substances_crystal[!is.na(df$Q61_substances_crystal)] <- "Yes"
df$Q61_substances_erection[!is.na(df$Q61_substances_erection)] <- "Yes"
df$Q61_substances_crack[!is.na(df$Q61_substances_crack)] <- "Yes"
df$Q61_substances_cocaine[!is.na(df$Q61_substances_cocaine)] <- "Yes"
df$Q61_substances_heroin[!is.na(df$Q61_substances_heroin)] <- "Yes"
df$Q61_substances_other_opioids[!is.na(df$Q61_substances_other_opioids)] <- "Yes"
df$Q61_substances_fentanyl[!is.na(df$Q61_substances_fentanyl)] <- "Yes"
df$Q61_substances_GHB[!is.na(df$Q61_substances_GHB)] <- "Yes"
df$Q61_substances_benzos[!is.na(df$Q61_substances_benzos)] <- "Yes"
df$Q61_substances_psychedelics[!is.na(df$Q61_substances_psychedelics)] <- "Yes"
df$Q61_substances_steroids[!is.na(df$Q61_substances_steroids)] <- "Yes"
df$Q61_substances_other[!is.na(df$Q61_substances_other)] <- "Yes"

df$Q61_substances_alcohol_sex[!is.na(df$Q61_substances_alcohol_sex)] <- "Yes"
df$Q61_substances_tobacco_sex[!is.na(df$Q61_substances_tobacco_sex)] <- "Yes"
df$Q61_substances_marijuana_sex[!is.na(df$Q61_substances_marijuana_sex)] <- "Yes"
df$Q61_substances_poppers_sex[!is.na(df$Q61_substances_poppers_sex)] <- "Yes"
df$Q61_substances_ketamine_sex[!is.na(df$Q61_substances_ketamine_sex)] <- "Yes"
df$Q61_substances_ecstasy_sex[!is.na(df$Q61_substances_ecstasy_sex)] <- "Yes"
df$Q61_substances_crystal_sex[!is.na(df$Q61_substances_crystal_sex)] <- "Yes"
df$Q61_substances_erection_sex[!is.na(df$Q61_substances_erection_sex)] <- "Yes"
df$Q61_substances_crack_sex[!is.na(df$Q61_substances_crack_sex)] <- "Yes"
df$Q61_substances_cocaine_sex[!is.na(df$Q61_substances_cocaine_sex)] <- "Yes"
df$Q61_substances_heroin_sex[!is.na(df$Q61_substances_heroin_sex)] <- "Yes"
df$Q61_substances_other_opioids_sex[!is.na(df$Q61_substances_other_opioids_sex)] <- "Yes"
df$Q61_substances_fentanyl_sex[!is.na(df$Q61_substances_fentanyl_sex)] <- "Yes"
df$Q61_substances_GHB_sex[!is.na(df$Q61_substances_GHB_sex)] <- "Yes"
df$Q61_substances_benzos_sex[!is.na(df$Q61_substances_benzos_sex)] <- "Yes"
df$Q61_substances_psychedelics_sex[!is.na(df$Q61_substances_psychedelics_sex)] <- "Yes"
df$Q61_substances_steroids_sex[!is.na(df$Q61_substances_steroids_sex)] <- "Yes"
df$Q61_substances_other_sex[!is.na(df$Q61_substances_other_sex)] <- "Yes"

# ----------------------
# Create "None" Variable
# ----------------------
df$Q61_substances_none <- NA
df$Q61_substances_none[df$Q60_substances_any == "No"] <- "Yes"
df$Q61_substances_none[df$Q60_substances_any == "Yes"] <- "No"
table(df$Q61_substances_none)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q61_substances_valid <- NA
df$Q61_substances_valid[!is.na(df$Q61_substances_alcohol) |
                          !is.na(df$Q61_substances_tobacco) |
                          !is.na(df$Q61_substances_marijuana) |
                          !is.na(df$Q61_substances_poppers) |
                          !is.na(df$Q61_substances_ketamine) |
                          !is.na(df$Q61_substances_ecstasy) |
                          !is.na(df$Q61_substances_crystal) |
                          !is.na(df$Q61_substances_erection) |
                          !is.na(df$Q61_substances_crack) |
                          !is.na(df$Q61_substances_cocaine) |
                          !is.na(df$Q61_substances_heroin) |
                          !is.na(df$Q61_substances_other_opioids) |
                          !is.na(df$Q61_substances_fentanyl) |
                          !is.na(df$Q61_substances_GHB) |
                          !is.na(df$Q61_substances_benzos) |
                          !is.na(df$Q61_substances_psychedelics) |
                          !is.na(df$Q61_substances_steroids) |
                          !is.na(df$Q61_substances_other) |
                          !is.na(df$Q61_substances_alcohol_sex) |
                          !is.na(df$Q61_substances_tobacco_sex) |
                          !is.na(df$Q61_substances_marijuana_sex) |
                          !is.na(df$Q61_substances_poppers_sex) |
                          !is.na(df$Q61_substances_ketamine_sex) |
                          !is.na(df$Q61_substances_ecstasy_sex) |
                          !is.na(df$Q61_substances_crystal_sex) |
                          !is.na(df$Q61_substances_erection_sex) |
                          !is.na(df$Q61_substances_crack_sex) |
                          !is.na(df$Q61_substances_cocaine_sex) |
                          !is.na(df$Q61_substances_heroin_sex) |
                          !is.na(df$Q61_substances_other_opioids_sex) |
                          !is.na(df$Q61_substances_fentanyl_sex) |
                          !is.na(df$Q61_substances_GHB_sex) |
                          !is.na(df$Q61_substances_benzos_sex) |
                          !is.na(df$Q61_substances_psychedelics_sex) |
                          !is.na(df$Q61_substances_steroids_sex) |
                          !is.na(df$Q61_substances_other_sex) |
                          df$Q61_substances_none == "No" |
                          df$Q61_substances_none == "Yes"] <- "Yes"
df$Q61_substances_valid[is.na(df$Q61_substances_valid)] <- "No"

table(df$Q61_substances_valid)

# ----------------------
# Add No Variable
# ----------------------
df$Q61_substances_alcohol[is.na(df$Q61_substances_alcohol) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_tobacco[is.na(df$Q61_substances_tobacco) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_marijuana[is.na(df$Q61_substances_marijuana) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_poppers[is.na(df$Q61_substances_poppers) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_ketamine[is.na(df$Q61_substances_ketamine) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_ecstasy[is.na(df$Q61_substances_ecstasy) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_crystal[is.na(df$Q61_substances_crystal) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_erection[is.na(df$Q61_substances_erection) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_crack[is.na(df$Q61_substances_crack) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_cocaine[is.na(df$Q61_substances_cocaine) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_heroin[is.na(df$Q61_substances_heroin) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_other_opioids[is.na(df$Q61_substances_other_opioids) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_fentanyl[is.na(df$Q61_substances_fentanyl) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_GHB[is.na(df$Q61_substances_GHB) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_benzos[is.na(df$Q61_substances_benzos) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_psychedelics[is.na(df$Q61_substances_psychedelics) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_steroids[is.na(df$Q61_substances_steroids) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_other[is.na(df$Q61_substances_other) & df$Q61_substances_valid == "Yes"] <- "No"

df$Q61_substances_alcohol_sex[is.na(df$Q61_substances_alcohol_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_tobacco_sex[is.na(df$Q61_substances_tobacco_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_marijuana_sex[is.na(df$Q61_substances_marijuana_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_poppers_sex[is.na(df$Q61_substances_poppers_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_ketamine_sex[is.na(df$Q61_substances_ketamine_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_ecstasy_sex[is.na(df$Q61_substances_ecstasy_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_crystal_sex[is.na(df$Q61_substances_crystal_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_erection_sex[is.na(df$Q61_substances_erection_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_crack_sex[is.na(df$Q61_substances_crack_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_cocaine_sex[is.na(df$Q61_substances_cocaine_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_heroin_sex[is.na(df$Q61_substances_heroin_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_other_opioids_sex[is.na(df$Q61_substances_other_opioids_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_fentanyl_sex[is.na(df$Q61_substances_fentanyl_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_GHB_sex[is.na(df$Q61_substances_GHB_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_benzos_sex[is.na(df$Q61_substances_benzos_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_psychedelics_sex[is.na(df$Q61_substances_psychedelics_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_steroids_sex[is.na(df$Q61_substances_steroids_sex) & df$Q61_substances_valid == "Yes"] <- "No"
df$Q61_substances_other_sex[is.na(df$Q61_substances_other_sex) & df$Q61_substances_valid == "Yes"] <- "No"


# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q61_substances_alcohol[df$Q61_substances_alcohol == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_alcohol == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_alcohol_sex[df$Q61_substances_alcohol_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_alcohol_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_tobacco[df$Q61_substances_tobacco == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_tobacco == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_tobacco_sex[df$Q61_substances_tobacco_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_tobacco_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_marijuana[df$Q61_substances_marijuana == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_marijuana == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_marijuana_sex[df$Q61_substances_marijuana_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_marijuana_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_poppers[df$Q61_substances_poppers == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_poppers == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_poppers_sex[df$Q61_substances_poppers_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_poppers_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_ketamine[df$Q61_substances_ketamine == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_ketamine == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_ketamine_sex[df$Q61_substances_ketamine_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_ketamine_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_ecstasy[df$Q61_substances_ecstasy == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_ecstasy == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_ecstasy_sex[df$Q61_substances_ecstasy_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_ecstasy_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_crystal[df$Q61_substances_crystal == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_crystal == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_erection[df$Q61_substances_erection == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_erection == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_erection_sex[df$Q61_substances_erection_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_erection_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_crack[df$Q61_substances_crack == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_crack == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_crack_sex[df$Q61_substances_crack_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_crack_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_cocaine[df$Q61_substances_cocaine == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_cocaine == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_cocaine_sex[df$Q61_substances_cocaine_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_cocaine_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_heroin[df$Q61_substances_heroin == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_heroin == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_heroin_sex[df$Q61_substances_heroin_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_heroin_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_other_opioids[df$Q61_substances_other_opioids == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_other_opioids == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_other_opioids_sex[df$Q61_substances_other_opioids_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_other_opioids_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_fentanyl[df$Q61_substances_fentanyl == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_fentanyl == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_fentanyl_sex[df$Q61_substances_fentanyl_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_fentanyl_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_GHB[df$Q61_substances_GHB == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_GHB == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_GHB_sex[df$Q61_substances_GHB_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_GHB_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_benzos[df$Q61_substances_benzos == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_benzos == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_benzos_sex[df$Q61_substances_benzos_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_benzos_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_psychedelics[df$Q61_substances_psychedelics == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_psychedelics == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_psychedelics_sex[df$Q61_substances_psychedelics_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_psychedelics_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_steroids[df$Q61_substances_steroids == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_steroids == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_steroids_sex[df$Q61_substances_steroids_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_steroids_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_other[df$Q61_substances_other == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_other == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_other_sex[df$Q61_substances_other_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_none[df$Q61_substances_other_sex == "Yes" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$Q61_substances_other_text[df$Q61_substances_other == "7777: Poor Data Quality" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$Q61_substances_other_text[df$Q61_substances_other_sex == "7777: Poor Data Quality" & df$Q61_substances_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Label Missings
# ----------------------
df$Q61_substances_alcohol[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_tobacco[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_marijuana[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_poppers[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_ketamine[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_ecstasy[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_crystal[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_erection[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_crack[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_cocaine[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_heroin[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_other_opioids[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_fentanyl[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_GHB[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_benzos[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_psychedelics[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_steroids[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_other[df$Q61_substances_valid == "No"] <- "9999: True Missing"

df$Q61_substances_alcohol_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_tobacco_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_marijuana_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_poppers_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_ketamine_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_ecstasy_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_crystal_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_erection_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_crack_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_cocaine_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_heroin_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_other_opioids_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_fentanyl_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_GHB_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_benzos_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_psychedelics_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_steroids_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_other_sex[df$Q61_substances_valid == "No"] <- "9999: True Missing"

df$Q61_substances_none[df$Q61_substances_valid == "No"] <- "9999: True Missing"
df$Q61_substances_none[is.na(df$Q61_substances_none)] <- "9999: True Missing"

df$Q61_substances_other_text[df$Q61_substances_other_sex == "9999: True Missing"] <- "9999: True Missing"
df$Q61_substances_other_text[df$Q61_substances_other == "9999: True Missing"] <- "9999: True Missing"
df$Q61_substances_other_text[df$Q61_substances_other == "No"] <- "8888: Other Not Selected"
df$Q61_substances_other_text[is.na(df$Q61_substances_other_text) & df$Q61_substances_other == "Yes"] <- "9999: True Missing"
table(df$Q61_substances_other_text)

# ----------------------
# Backcode Missing
# ----------------------
df$Q61_substances_alcohol[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_tobacco[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_marijuana[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_poppers[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_ketamine[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_ecstasy[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_crystal[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_erection[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_crack[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_cocaine[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_heroin[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_other_opioids[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_fentanyl[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_GHB[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_benzos[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_psychedelics[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_steroids[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_other[df$Q60_substances_any == "No"] <- "8888: No Substance Use"

df$Q61_substances_alcohol_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_tobacco_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_marijuana_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_poppers_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_ketamine_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_ecstasy_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_crystal_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_erection_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_crack_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_cocaine_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_heroin_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_other_opioids_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_fentanyl_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_GHB_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_benzos_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_psychedelics_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_steroids_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"
df$Q61_substances_other_sex[df$Q60_substances_any == "No"] <- "8888: No Substance Use"

# ======================================================
# Q62_ever_injected_drugs
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q62_ever_injected_drugs)

# ----------------------
# Address Data Quality
# ----------------------
df$Q62_ever_injected_drugs[df$Q62_ever_injected_drugs == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Label Missing
# ----------------------
df$Q62_ever_injected_drugs[is.na(df$Q62_ever_injected_drugs)] <- "9999: True Missing"


# ======================================================
# Q63 Substance Use Service Utilization
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q63_substance_services_needle_exchange)
table(df$Q63_substance_services_harm_reduction)
table(df$Q63_substance_services_supervised_injection)
table(df$Q63_substance_services_Naloxone)
table(df$Q63_substance_services_Naloxone_on_me)
table(df$Q63_substance_services_Naloxone_someone_else)
table(df$Q63_substance_services_detox)
table(df$Q63_substance_services_sweat_lodge)
table(df$Q63_substance_services_NA)
table(df$Q63_substance_services_AA)
table(df$Q63_substance_services_none)
table(df$Q63_substance_services_other_text)

# ----------------------
# Create Variable to Identify "Other" Category
# ----------------------
df$Q63_substance_services_other <- NA
df$Q63_substance_services_other[is.na(df$Q63_substance_services_other_text)] <- "No"
df$Q63_substance_services_other[!is.na(df$Q63_substance_services_other_text)] <- "Yes"
table(df$Q63_substance_services_other)

df$Q63_substance_services_other_text[df$Q63_substance_services_other == "No"] <- "8888: Other Not Selected"
df$Q63_substance_services_other_text[is.na(df$Q63_substance_services_other_text) & df$Q63_substance_services_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q63_substance_services_valid <- NA
df$Q63_substance_services_valid[df$Q63_substance_services_needle_exchange == "Needle exchange" |
                                  df$Q63_substance_services_harm_reduction == "Harm reduction supplies (e.g. free pipes, straws)" |
                                  df$Q63_substance_services_supervised_injection == "Supervised injection/consumption site" |
                                  df$Q63_substance_services_Naloxone == "Naloxone/NARCAN was used [ ] on me, [ ] on someone else" |
                                  df$Q63_substance_services_Naloxone_on_me == "on me" |
                                  df$Q63_substance_services_Naloxone_someone_else == "on someone else" |
                                  df$Q63_substance_services_detox == "Detox or drug treatment facility" |
                                  df$Q63_substance_services_sweat_lodge == "Sweat Lodge or other cultural traditions" |
                                  df$Q63_substance_services_NA == "Narcotics Anonymous" |
                                  df$Q63_substance_services_AA == "Alcoholics Anonymous" |
                                  df$Q60_substances_any == "No" |
                                  df$Q63_substance_services_none == "None of the above" |
                                  df$Q63_substance_services_other == "Yes"] <- "Yes"
df$Q63_substance_services_valid[is.na(df$Q63_substance_services_valid)] <- "No"

table(df$Q63_substance_services_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q63_substance_services_needle_exchange[df$Q63_substance_services_needle_exchange == "Needle exchange"] <- "Yes"
df$Q63_substance_services_harm_reduction[df$Q63_substance_services_harm_reduction == "Harm reduction supplies (e.g. free pipes, straws)"] <- "Yes"
df$Q63_substance_services_supervised_injection[df$Q63_substance_services_supervised_injection == "Supervised injection/consumption site"] <- "Yes"
df$Q63_substance_services_Naloxone[df$Q63_substance_services_Naloxone == "Naloxone/NARCAN was used [ ] on me, [ ] on someone else"] <- "Yes"
df$Q63_substance_services_Naloxone_on_me[df$Q63_substance_services_Naloxone_on_me == "on me"] <- "Yes"
df$Q63_substance_services_Naloxone_someone_else[df$Q63_substance_services_Naloxone_someone_else == "on someone else"] <- "Yes"
df$Q63_substance_services_detox[df$Q63_substance_services_detox == "Detox or drug treatment facility"] <- "Yes"
df$Q63_substance_services_sweat_lodge[df$Q63_substance_services_sweat_lodge == "Sweat Lodge or other cultural traditions"] <- "Yes"
df$Q63_substance_services_NA[df$Q63_substance_services_NA == "Narcotics Anonymous"] <- "Yes"
df$Q63_substance_services_AA[df$Q63_substance_services_AA == "Alcoholics Anonymous"] <- "Yes"
df$Q63_substance_services_none[df$Q63_substance_services_none == "None of the above"] <- "Yes"
df$Q63_substance_services_other[df$Q63_substance_services_other == "Yes"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q63_substance_services_needle_exchange[is.na(df$Q63_substance_services_needle_exchange) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_harm_reduction[is.na(df$Q63_substance_services_harm_reduction) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_supervised_injection[is.na(df$Q63_substance_services_supervised_injection) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_Naloxone[is.na(df$Q63_substance_services_Naloxone) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_Naloxone_on_me[is.na(df$Q63_substance_services_Naloxone_on_me) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_Naloxone_someone_else[is.na(df$Q63_substance_services_Naloxone_someone_else) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_detox[is.na(df$Q63_substance_services_detox) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_sweat_lodge[is.na(df$Q63_substance_services_sweat_lodge) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_NA[is.na(df$Q63_substance_services_NA) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_AA[is.na(df$Q63_substance_services_AA) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_none[is.na(df$Q63_substance_services_none) & df$Q63_substance_services_valid == "Yes"] <- "No"
df$Q63_substance_services_other[is.na(df$Q63_substance_services_other) & df$Q63_substance_services_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q63_substance_services_needle_exchange[df$Q63_substance_services_needle_exchange == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_needle_exchange == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_harm_reduction[df$Q63_substance_services_harm_reduction == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_harm_reduction == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_supervised_injection[df$Q63_substance_services_supervised_injection == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_supervised_injection == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_Naloxone[df$Q63_substance_services_Naloxone == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_Naloxone == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_Naloxone_on_me[df$Q63_substance_services_Naloxone_on_me == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_Naloxone_on_me == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_Naloxone_someone_else[df$Q63_substance_services_Naloxone_someone_else == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_Naloxone_someone_else == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_detox[df$Q63_substance_services_detox == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_detox == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_sweat_lodge[df$Q63_substance_services_sweat_lodge == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_sweat_lodge == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_NA[df$Q63_substance_services_NA == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_NA == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_AA[df$Q63_substance_services_AA == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_AA == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

df$Q63_substance_services_other[df$Q63_substance_services_other == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"
df$Q63_substance_services_none[df$Q63_substance_services_other == "Yes" & df$Q63_substance_services_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q63_substance_services_Naloxone_someone_else[df$Q63_substance_services_Naloxone == "No"] <- "8888: No Naloxone Use"
df$Q63_substance_services_Naloxone_someone_else[df$Q63_substance_services_Naloxone == "No"] <- "8888: No Naloxone Use"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q63_substance_services_needle_exchange[is.na(df$Q63_substance_services_needle_exchange) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_harm_reduction[is.na(df$Q63_substance_services_harm_reduction) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_supervised_injection[is.na(df$Q63_substance_services_supervised_injection) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_Naloxone[is.na(df$Q63_substance_services_Naloxone) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_Naloxone_on_me[is.na(df$Q63_substance_services_Naloxone_on_me) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_Naloxone_someone_else[is.na(df$Q63_substance_services_Naloxone_someone_else) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_sweat_lodge[is.na(df$Q63_substance_services_sweat_lodge) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_detox[is.na(df$Q63_substance_services_detox) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_NA[is.na(df$Q63_substance_services_NA) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_AA[is.na(df$Q63_substance_services_AA) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_none[is.na(df$Q63_substance_services_none) & df$Q63_substance_services_valid == "No"] <- "9999: True Missing"
df$Q63_substance_services_other[df$Q63_substance_services_none == "9999: True Missing"] <- "9999: True Missing"

# ----------------------
# Accounting for Skip logic
# ----------------------
df$Q63_substance_services_needle_exchange[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_harm_reduction[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_supervised_injection[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_Naloxone[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_Naloxone_on_me[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_Naloxone_someone_else[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_sweat_lodge[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_detox[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_NA[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_AA[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_none[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"
df$Q63_substance_services_other[df$Q60_substances_any == "No"] <- "8888: No substance use in P6M"

# ======================================================
# Q64 Social involvement
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q64_social_volunteering)
table(df$Q64_social_activism)
table(df$Q64_social_sports)
table(df$Q64_social_HIV)
table(df$Q64_social_civic)
table(df$Q64_social_political)
table(df$Q64_social_pop_ups)
table(df$Q64_social_ethnoracial)
table(df$Q64_social_none)

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q64_social_valid <- NA
df$Q64_social_valid[df$Q64_social_volunteering == "Personal voluntary action, neighbourhood support, elder care" |
                      df$Q64_social_activism == "Gay activism, organization, or cultural activities" |
                      df$Q64_social_sports == "LGTBQ2S+ sport leagues or recreational activities" |
                      df$Q64_social_HIV == "HIV advocacy, AIDS service organization" |
                      df$Q64_social_civic == "Civic (non-LGBTQ2S+) activism, charity, or cultural activities" |
                      df$Q64_social_political == "Political organizing, advocacy, party membership" |
                      df$Q64_social_pop_ups == "Pop-ups (queer dance party, art show, etc.)" |
                      df$Q64_social_ethnoracial == "Ethnoracial community groups, activities" |
                      df$Q64_social_none == "I am not involved in any of the above"] <- "Yes"
df$Q64_social_valid[is.na(df$Q64_social_valid)] <- "No"

table(df$Q64_social_valid)

# ----------------------
# Renaming Positive-Response Levels to "Yes"
# ----------------------
df$Q64_social_volunteering[df$Q64_social_volunteering == "Personal voluntary action, neighbourhood support, elder care"] <- "Yes"
df$Q64_social_activism[df$Q64_social_activism == "Gay activism, organization, or cultural activities"] <- "Yes"
df$Q64_social_sports[df$Q64_social_sports == "LGTBQ2S+ sport leagues or recreational activities"] <- "Yes"
df$Q64_social_HIV[df$Q64_social_HIV == "HIV advocacy, AIDS service organization"] <- "Yes"
df$Q64_social_civic[df$Q64_social_civic == "Civic (non-LGBTQ2S+) activism, charity, or cultural activities"] <- "Yes"
df$Q64_social_political[df$Q64_social_political == "Political organizing, advocacy, party membership"] <- "Yes"
df$Q64_social_pop_ups[df$Q64_social_pop_ups == "Pop-ups (queer dance party, art show, etc.)"] <- "Yes"
df$Q64_social_ethnoracial[df$Q64_social_ethnoracial == "Ethnoracial community groups, activities"] <- "Yes"
df$Q64_social_none[df$Q64_social_none == "I am not involved in any of the above"] <- "Yes"

# ----------------------
# Creating "No" Level
# ----------------------
df$Q64_social_volunteering[is.na(df$Q64_social_volunteering) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_activism[is.na(df$Q64_social_activism) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_sports[is.na(df$Q64_social_sports) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_HIV[is.na(df$Q64_social_HIV) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_civic[is.na(df$Q64_social_civic) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_political[is.na(df$Q64_social_political) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_pop_ups[is.na(df$Q64_social_pop_ups) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_ethnoracial[is.na(df$Q64_social_ethnoracial) & df$Q64_social_valid == "Yes"] <- "No"
df$Q64_social_none[is.na(df$Q64_social_none) & df$Q64_social_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q64_social_volunteering[df$Q64_social_volunteering == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_volunteering == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_activism[df$Q64_social_activism == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_activism == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_sports[df$Q64_social_sports == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_sports == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_HIV[df$Q64_social_HIV == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_HIV == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_civic[df$Q64_social_civic == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_civic == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_political[df$Q64_social_political == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_political == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_pop_ups[df$Q64_social_pop_ups == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_pop_ups == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

df$Q64_social_ethnoracial[df$Q64_social_ethnoracial == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"
df$Q64_social_none[df$Q64_social_ethnoracial == "Yes" & df$Q64_social_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q64_social_volunteering[is.na(df$Q64_social_volunteering) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_activism[is.na(df$Q64_social_activism) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_sports[is.na(df$Q64_social_sports) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_HIV[is.na(df$Q64_social_HIV) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_civic[is.na(df$Q64_social_civic) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_political[is.na(df$Q64_social_political) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_pop_ups[is.na(df$Q64_social_pop_ups) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_ethnoracial[is.na(df$Q64_social_ethnoracial) & df$Q64_social_valid == "No"] <- "9999: True Missing"
df$Q64_social_none[is.na(df$Q64_social_none) & df$Q64_social_valid == "No"] <- "9999: True Missing"

# ======================================================
# Q65_support
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q65_support)

# ----------------------
# Data Quality
# ----------------------
df$Q65_support[df$Q65_support == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Missing Data
# ----------------------
df$Q65_support[is.na(df$Q65_support)] <- "9999: True Missing"


# ======================================================
# Q66 Satisfaction
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q66_satisfied_community)
table(df$Q66_satisfied_men)
table(df$Q66_satisfied_physical_spaces)
table(df$Q66_satisfied_online_spaces)

# ----------------------
# Manage Data Quality
# ----------------------
df$Q66_satisfied_community[df$Q66_satisfied_community == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q66_satisfied_men[df$Q66_satisfied_men == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q66_satisfied_physical_spaces[df$Q66_satisfied_physical_spaces == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q66_satisfied_online_spaces[df$Q66_satisfied_online_spaces == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Label Missing Data
# ----------------------
df$Q66_satisfied_community[is.na(df$Q66_satisfied_community)] <- "9999: True Missing"
df$Q66_satisfied_men[is.na(df$Q66_satisfied_men)] <- "9999: True Missing"
df$Q66_satisfied_physical_spaces[is.na(df$Q66_satisfied_physical_spaces)] <- "9999: True Missing"
df$Q66_satisfied_online_spaces[is.na(df$Q66_satisfied_online_spaces)] <- "9999: True Missing"

# ======================================================
# Q67_has_doctor
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q67_has_doctor)

# ----------------------
# Relabel Nos
# ----------------------
df$Q67_has_doctor[df$Q67_has_doctor == "No (skip to #87 below)"] <- "No"

# ----------------------
# Label Missing Data
# ----------------------
df$Q67_has_doctor[is.na(df$Q67_has_doctor)] <- "9999: True Missing"

# ======================================================
# Q68_doctor_knows
# ======================================================
table(df$Q68_doctor_knows)

# ----------------------
# Relabel Nos
# ----------------------
df$Q68_doctor_knows[df$Q68_doctor_knows == "No (skip to #87 below)"] <- "No"

# ----------------------
# Data Quality
# ----------------------
df$Q68_doctor_knows[df$Q68_doctor_knows == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Account for missing
# ----------------------
df$Q68_doctor_knows[df$Q67_has_doctor == "No"] <- "8888: No regular family doctor or nurse practitioner"

# ----------------------
# Label Missing Data
# ----------------------
df$Q68_doctor_knows[is.na(df$Q68_doctor_knows)] <- "9999: True Missing"

# ======================================================
# Q69 Vaccinations
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q69_vaccinated_HepB)
table(df$Q69_vaccinated_HPV)
table(df$Q69_vaccinated_HepA_WPG_only)
table(df$Q69_vaccinated_flu_WPG_only)

# ----------------------
# Data Quality
# ----------------------
df$Q69_vaccinated_HepB[df$Q69_vaccinated_HepB == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$Q69_vaccinated_HPV[df$Q69_vaccinated_HPV == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Winnipeg Only
# ----------------------
df$Q69_vaccinated_HepA_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$Q69_vaccinated_flu_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Label Missing Data
# ----------------------
df$Q69_vaccinated_HepB[is.na(df$Q69_vaccinated_HepB)] <- "9999: True Missing"
df$Q69_vaccinated_HPV[is.na(df$Q69_vaccinated_HPV)] <- "9999: True Missing"
df$Q69_vaccinated_HepA_WPG_only[is.na(df$Q69_vaccinated_HepA_WPG_only)] <- "9999: True Missing"
df$Q69_vaccinated_flu_WPG_only[is.na(df$Q69_vaccinated_flu_WPG_only)] <- "9999: True Missing"

# ======================================================
# Q70_testicular_cancer_check
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q70_testicular_cancer_check)

# ----------------------
# Data Quality
# ----------------------
df$Q70_testicular_cancer_check[df$Q70_testicular_cancer_check == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Label Missing Data
# ----------------------
df$Q70_testicular_cancer_check[is.na(df$Q70_testicular_cancer_check)] <- "9999: True Missing"

# ======================================================
# Q71 Denial of Services
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q71_denied_HIV_test)
table(df$Q71_denied_PEP)
table(df$Q71_denied_PrEP)
table(df$Q71_denied_HPV_vaccine)
table(df$Q71_denied_hormone_therapy)
table(df$Q71_denied_gender_surgery)
table(df$Q71_denied_none)

# ----------------------
# Relabel Yes
# ----------------------
df$Q71_denied_HIV_test[!is.na(df$Q71_denied_HIV_test)] <- "Yes"
df$Q71_denied_PEP[!is.na(df$Q71_denied_PEP)] <- "Yes"
df$Q71_denied_PrEP[!is.na(df$Q71_denied_PrEP)] <- "Yes"
df$Q71_denied_HPV_vaccine[!is.na(df$Q71_denied_HPV_vaccine)] <- "Yes"
df$Q71_denied_hormone_therapy[!is.na(df$Q71_denied_hormone_therapy)] <- "Yes"
df$Q71_denied_gender_surgery[!is.na(df$Q71_denied_gender_surgery)] <- "Yes"
df$Q71_denied_none[!is.na(df$Q71_denied_none)] <- "Yes"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q71_denied_valid <- NA
df$Q71_denied_valid[df$Q71_denied_HIV_test == "Yes" |
                      df$Q71_denied_PEP == "Yes" |
                      df$Q71_denied_PrEP == "Yes" |
                      df$Q71_denied_HPV_vaccine == "Yes" |
                      df$Q71_denied_hormone_therapy == "Yes" |
                      df$Q71_denied_gender_surgery == "Yes" |
                      df$Q71_denied_none == "Yes"] <- "Yes"
df$Q71_denied_valid[is.na(df$Q71_denied_valid)] <- "No"

table(df$Q71_denied_valid)

# ----------------------
# Creating "No" Level
# ----------------------
df$Q71_denied_HIV_test[is.na(df$Q71_denied_HIV_test) & df$Q71_denied_valid == "Yes"] <- "No"
df$Q71_denied_PEP[is.na(df$Q71_denied_PEP) & df$Q71_denied_valid == "Yes"] <- "No"
df$Q71_denied_PrEP[is.na(df$Q71_denied_PrEP) & df$Q71_denied_valid == "Yes"] <- "No"
df$Q71_denied_HPV_vaccine[is.na(df$Q71_denied_HPV_vaccine) & df$Q71_denied_valid == "Yes"] <- "No"
df$Q71_denied_hormone_therapy[is.na(df$Q71_denied_hormone_therapy) & df$Q71_denied_valid == "Yes"] <- "No"
df$Q71_denied_gender_surgery[is.na(df$Q71_denied_gender_surgery) & df$Q71_denied_valid == "Yes"] <- "No"
df$Q71_denied_none[is.na(df$Q71_denied_none) & df$Q71_denied_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q71_denied_HIV_test[df$Q71_denied_HIV_test == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"
df$Q71_denied_none[df$Q71_denied_HIV_test == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"

df$Q71_denied_PEP[df$Q71_denied_PEP == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"
df$Q71_denied_none[df$Q71_denied_PEP == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"

df$Q71_denied_PrEP[df$Q71_denied_PrEP == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"
df$Q71_denied_none[df$Q71_denied_PrEP == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"

df$Q71_denied_HPV_vaccine[df$Q71_denied_HPV_vaccine == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"
df$Q71_denied_none[df$Q71_denied_HPV_vaccine == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"

df$Q71_denied_hormone_therapy[df$Q71_denied_hormone_therapy == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"
df$Q71_denied_none[df$Q71_denied_hormone_therapy == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"

df$Q71_denied_gender_surgery[df$Q71_denied_gender_surgery == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"
df$Q71_denied_none[df$Q71_denied_gender_surgery == "Yes" & df$Q71_denied_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q71_denied_HIV_test[is.na(df$Q71_denied_HIV_test) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_PEP[is.na(df$Q71_denied_PEP) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_PrEP[is.na(df$Q71_denied_PrEP) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_HPV_vaccine[is.na(df$Q71_denied_HPV_vaccine) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_hormone_therapy[is.na(df$Q71_denied_hormone_therapy) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_gender_surgery[is.na(df$Q71_denied_gender_surgery) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_none[is.na(df$Q71_denied_none) & df$Q71_denied_valid == "No"] <- "9999: True Missing"
df$Q71_denied_none[df$Q71_denied_gender_surgery == "9999: True Missing"] <- "9999: True Missing"

# ======================================================
# Q72 Discrimination
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q72_discrimination_age)
table(df$Q72_discrimination_age_by_MSM)
table(df$Q72_discrimination_HIV)
table(df$Q72_discrimination_HIV_by_MSM)
table(df$Q72_discrimination_PrEP)
table(df$Q72_discrimination_PrEP_by_MSM)
table(df$Q72_discrimination_race)
table(df$Q72_discrimination_race_by_MSM)
table(df$Q72_discrimination_body)
table(df$Q72_discrimination_body_by_MSM)
table(df$Q72_discrimination_gender)
table(df$Q72_discrimination_gender_by_MSM)
table(df$Q72_discrimination_sex_orient)
table(df$Q72_discrimination_sex_orient_by_MSM)
table(df$Q72_discrimination_trans)
table(df$Q72_discrimination_trans_by_MSM)
table(df$Q72_discrimination_disability)
table(df$Q72_discrimination_disability_by_MSM)

# ----------------------
# Relabel Yes
# ----------------------
df$Q72_discrimination_age[!is.na(df$Q72_discrimination_age)] <- "Yes"
df$Q72_discrimination_age_by_MSM[!is.na(df$Q72_discrimination_age_by_MSM)] <- "Yes"
df$Q72_discrimination_HIV[!is.na(df$Q72_discrimination_HIV)] <- "Yes"
df$Q72_discrimination_HIV_by_MSM[!is.na(df$Q72_discrimination_HIV_by_MSM)] <- "Yes"
df$Q72_discrimination_PrEP[!is.na(df$Q72_discrimination_PrEP)] <- "Yes"
df$Q72_discrimination_PrEP_by_MSM[!is.na(df$Q72_discrimination_PrEP_by_MSM)] <- "Yes"
df$Q72_discrimination_race[!is.na(df$Q72_discrimination_race)] <- "Yes"
df$Q72_discrimination_race_by_MSM[!is.na(df$Q72_discrimination_race_by_MSM)] <- "Yes"
df$Q72_discrimination_body[!is.na(df$Q72_discrimination_body)] <- "Yes"
df$Q72_discrimination_body_by_MSM[!is.na(df$Q72_discrimination_body_by_MSM)] <- "Yes"
df$Q72_discrimination_gender[!is.na(df$Q72_discrimination_gender)] <- "Yes"
df$Q72_discrimination_gender_by_MSM[!is.na(df$Q72_discrimination_gender_by_MSM)] <- "Yes"
df$Q72_discrimination_sex_orient[!is.na(df$Q72_discrimination_sex_orient)] <- "Yes"
df$Q72_discrimination_sex_orient_by_MSM[!is.na(df$Q72_discrimination_sex_orient_by_MSM)] <- "Yes"
df$Q72_discrimination_trans[!is.na(df$Q72_discrimination_trans)] <- "Yes"
df$Q72_discrimination_trans_by_MSM[!is.na(df$Q72_discrimination_trans_by_MSM)] <- "Yes"
df$Q72_discrimination_disability[!is.na(df$Q72_discrimination_disability)] <- "Yes"
df$Q72_discrimination_disability_by_MSM[!is.na(df$Q72_discrimination_disability_by_MSM)] <- "Yes"

# ----------------------
# Backcode by MSM to General
# ----------------------
df$Q72_discrimination_age[!is.na(df$Q72_discrimination_age_by_MSM)] <- "Yes"
df$Q72_discrimination_HIV[!is.na(df$Q72_discrimination_HIV_by_MSM)] <- "Yes"
df$Q72_discrimination_PrEP[!is.na(df$Q72_discrimination_PrEP_by_MSM)] <- "Yes"
df$Q72_discrimination_race[!is.na(df$Q72_discrimination_race_by_MSM)] <- "Yes"
df$Q72_discrimination_body[!is.na(df$Q72_discrimination_body_by_MSM)] <- "Yes"
df$Q72_discrimination_gender[!is.na(df$Q72_discrimination_gender_by_MSM)] <- "Yes"
df$Q72_discrimination_sex_orient[!is.na(df$Q72_discrimination_sex_orient_by_MSM)] <- "Yes"
df$Q72_discrimination_trans[!is.na(df$Q72_discrimination_trans_by_MSM)] <- "Yes"
df$Q72_discrimination_disability[!is.na(df$Q72_discrimination_disability_by_MSM)] <- "Yes"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q72_discrimination_valid <- NA
df$Q72_discrimination_valid[df$Q72_discrimination_age == "Yes" |
                              df$Q72_discrimination_age_by_MSM == "Yes" |
                              df$Q72_discrimination_HIV == "Yes" |
                              df$Q72_discrimination_HIV_by_MSM == "Yes" |
                              df$Q72_discrimination_PrEP == "Yes" |
                              df$Q72_discrimination_PrEP_by_MSM == "Yes" |
                              df$Q72_discrimination_race == "Yes" |
                              df$Q72_discrimination_race_by_MSM == "Yes" |
                              df$Q72_discrimination_body == "Yes" |
                              df$Q72_discrimination_body_by_MSM == "Yes" |
                              df$Q72_discrimination_gender == "Yes" |
                              df$Q72_discrimination_gender_by_MSM == "Yes" |
                              df$Q72_discrimination_sex_orient == "Yes" |
                              df$Q72_discrimination_sex_orient_by_MSM == "Yes" |
                              df$Q72_discrimination_trans == "Yes" |
                              df$Q72_discrimination_trans_by_MSM == "Yes" |
                              df$Q72_discrimination_disability == "Yes" |
                              df$Q72_discrimination_disability_by_MSM == "Yes" ] <- "Yes"
df$Q72_discrimination_valid[is.na(df$Q72_discrimination_valid)] <- "No"

table(df$Q72_discrimination_valid)

# ----------------------
# Creating "No" Level
# ----------------------
df$Q72_discrimination_age[is.na(df$Q72_discrimination_age) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_HIV[is.na(df$Q72_discrimination_HIV) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_PrEP[is.na(df$Q72_discrimination_PrEP) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_race[is.na(df$Q72_discrimination_race) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_body[is.na(df$Q72_discrimination_body) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_gender[is.na(df$Q72_discrimination_gender) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_sex_orient[is.na(df$Q72_discrimination_sex_orient) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_trans[is.na(df$Q72_discrimination_trans) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_disability[is.na(df$Q72_discrimination_disability) & df$Q72_discrimination_valid == "Yes"] <- "No"

df$Q72_discrimination_age_by_MSM[is.na(df$Q72_discrimination_age_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_HIV_by_MSM[is.na(df$Q72_discrimination_HIV_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_PrEP_by_MSM[is.na(df$Q72_discrimination_PrEP_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_race_by_MSM[is.na(df$Q72_discrimination_race_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_body_by_MSM[is.na(df$Q72_discrimination_body_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_gender_by_MSM[is.na(df$Q72_discrimination_gender_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_sex_orient_by_MSM[is.na(df$Q72_discrimination_sex_orient_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_trans_by_MSM[is.na(df$Q72_discrimination_trans_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"
df$Q72_discrimination_disability_by_MSM[is.na(df$Q72_discrimination_disability_by_MSM) & df$Q72_discrimination_valid == "Yes"] <- "No"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q72_discrimination_age[is.na(df$Q72_discrimination_age) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_HIV[is.na(df$Q72_discrimination_HIV) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_PrEP[is.na(df$Q72_discrimination_PrEP) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_race[is.na(df$Q72_discrimination_race) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_body[is.na(df$Q72_discrimination_body) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_gender[is.na(df$Q72_discrimination_gender) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_sex_orient[is.na(df$Q72_discrimination_sex_orient) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_trans[is.na(df$Q72_discrimination_trans) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_disability[is.na(df$Q72_discrimination_disability) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"

df$Q72_discrimination_age_by_MSM[is.na(df$Q72_discrimination_age_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_HIV_by_MSM[is.na(df$Q72_discrimination_HIV_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_PrEP_by_MSM[is.na(df$Q72_discrimination_PrEP_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_race_by_MSM[is.na(df$Q72_discrimination_race_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_body_by_MSM[is.na(df$Q72_discrimination_body_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_gender_by_MSM[is.na(df$Q72_discrimination_gender_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_sex_orient_by_MSM[is.na(df$Q72_discrimination_sex_orient_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_trans_by_MSM[is.na(df$Q72_discrimination_trans_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"
df$Q72_discrimination_disability_by_MSM[is.na(df$Q72_discrimination_disability_by_MSM) & df$Q72_discrimination_valid == "No"] <- "9999: True Missing"

# ----------------------
# Correct No / Missing Level Due to no "none" option by looking to nearby answers
# ----------------------
df$Q72_discrimination_age[df$Q72_discrimination_age == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing"] <- "No"
df$Q72_discrimination_HIV[df$Q72_discrimination_HIV == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_PrEP[df$Q72_discrimination_PrEP == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_race[df$Q72_discrimination_race == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_body[df$Q72_discrimination_body == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_gender[df$Q72_discrimination_gender == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_sex_orient[df$Q72_discrimination_sex_orient == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_trans[df$Q72_discrimination_trans == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_disability[df$Q72_discrimination_disability == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_age_by_MSM[df$Q72_discrimination_age_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_HIV_by_MSM[df$Q72_discrimination_HIV_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_PrEP_by_MSM[df$Q72_discrimination_PrEP_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_race_by_MSM[df$Q72_discrimination_race_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_body_by_MSM[df$Q72_discrimination_body_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_gender_by_MSM[df$Q72_discrimination_gender_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_sex_orient_by_MSM[df$Q72_discrimination_sex_orient_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_trans_by_MSM[df$Q72_discrimination_trans_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"
df$Q72_discrimination_disability_by_MSM[df$Q72_discrimination_disability_by_MSM == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing" ] <- "No"

# ======================================================
# Q73 Abuse
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q73_abuse_verbal)
table(df$Q73_abuse_physical)
table(df$Q73_abuse_sexual)
table(df$Q73_abuse_prefer_not_to_say_WPG_only)

# ----------------------
# Recode Yes's
# ----------------------
df$Q73_abuse_verbal[!is.na(df$Q73_abuse_verbal)] <- "Yes"
df$Q73_abuse_physical[!is.na(df$Q73_abuse_physical)] <- "Yes"
df$Q73_abuse_sexual[!is.na(df$Q73_abuse_sexual)] <- "Yes"
df$Q73_abuse_prefer_not_to_say_WPG_only[!is.na(df$Q73_abuse_prefer_not_to_say_WPG_only)] <- "Yes"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q73_abuse_valid <- NA
df$Q73_abuse_valid[df$Q73_abuse_verbal == "Yes" |
                     df$Q73_abuse_physical == "Yes" |
                     df$Q73_abuse_sexual == "Yes" |
                     df$Q73_abuse_prefer_not_to_say_WPG_only == "Yes" ] <- "Yes"
df$Q73_abuse_valid[is.na(df$Q73_abuse_valid)] <- "No"

table(df$Q73_abuse_valid)

# ----------------------
# Creating "No" Level
# ----------------------
df$Q73_abuse_verbal[is.na(df$Q73_abuse_verbal) & df$Q73_abuse_valid == "Yes"] <- "No"
df$Q73_abuse_physical[is.na(df$Q73_abuse_physical) & df$Q73_abuse_valid == "Yes"] <- "No"
df$Q73_abuse_sexual[is.na(df$Q73_abuse_sexual) & df$Q73_abuse_valid == "Yes"] <- "No"
df$Q73_abuse_prefer_not_to_say_WPG_only[is.na(df$Q73_abuse_prefer_not_to_say_WPG_only) & df$Q73_abuse_valid == "Yes"] <- "No"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q73_abuse_verbal[is.na(df$Q73_abuse_verbal) & df$Q73_abuse_valid == "No"] <- "9999: True Missing"
df$Q73_abuse_physical[is.na(df$Q73_abuse_physical) & df$Q73_abuse_valid == "No"] <- "9999: True Missing"
df$Q73_abuse_sexual[is.na(df$Q73_abuse_sexual) & df$Q73_abuse_valid == "No"] <- "9999: True Missing"
df$Q73_abuse_prefer_not_to_say_WPG_only[is.na(df$Q73_abuse_prefer_not_to_say_WPG_only) & df$Q73_abuse_valid == "No"] <- "9999: True Missing"

# ----------------------
# Accounting for Skip Logic
# ----------------------
df$Q73_abuse_prefer_not_to_say_WPG_only[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Correct No / Missing Level Due to no "none" option by looking to nearby answers
# ----------------------
df$Q73_abuse_verbal[df$Q73_abuse_verbal == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing"] <- "No"
df$Q73_abuse_physical[df$Q73_abuse_physical == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing"] <- "No"
df$Q73_abuse_sexual[df$Q73_abuse_sexual == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing"] <- "No"
df$Q73_abuse_prefer_not_to_say_WPG_only[df$Q73_abuse_prefer_not_to_say_WPG_only == "9999: True Missing" & df$Q71_denied_none != "9999: True Missing"] <- "No"

# ======================================================
# Q74_forced_sex
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q74_forced_sex)

# ----------------------
# Data Quality
# ----------------------
df$Q74_forced_sex[df$Q74_forced_sex == "Prefer not to answer"] <- "7777: Poor Data Quality"

# ----------------------
# Missing Data
# ----------------------
df$Q74_forced_sex[is.na(df$Q74_forced_sex)] <- "9999: True Missing"

# ======================================================
# Q75_correctional_facility
# ======================================================
# ----------------------
# Explore Data
# ----------------------
# ----------------------
# Explore Data
# ----------------------
table(df$Q75_correctional_facility)

# ----------------------
# Data Quality
# ----------------------
df$Q75_correctional_facility[df$Q75_correctional_facility == "Prefer not to answer"] <- "7777: Poor Data Quality"

# ----------------------
# Missing Data
# ----------------------
df$Q75_correctional_facility[is.na(df$Q75_correctional_facility)] <- "9999: True Missing"

# ======================================================
# Q76_sex_w_men_ever
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q76_sex_w_men_ever)

# ======================================================
# Q76_sex_w_men_first_time_age
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q76_sex_w_men_first_time_age)

# ----------------------
# Data Quality
# ----------------------
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "14/15"] <- "14"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "15 or 16"] <- "15"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "21-25"] <- "23"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "4 non-consensually, 16 consensually"] <- "6"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "6-7"] <- "6"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "6 (Rape)"] <- "6"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "idk"] <- "7777: Poor Data Quality"
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_first_time_age == "N/A"] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q76_sex_w_men_first_time_age[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"

# ----------------------
# Label Missing
# ----------------------
df$Q76_sex_w_men_first_time_age[is.na(df$Q76_sex_w_men_first_time_age)] <- "9999: True Missing"

# ======================================================
# Q77_sex_w_men_number
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q77_sex_w_men_number)

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q77_sex_w_men_number[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"

# ----------------------
# Label Missing
# ----------------------
df$Q77_sex_w_men_number[is.na(df$Q77_sex_w_men_number)] <- "9999: True Missing"


# ======================================================
# Q77_sex_w_men_number_anal
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q77_sex_w_men_number_anal)

# ----------------------
# Data Cleaning
# ----------------------
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "<10"] <- "10"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "0.75"] <- "1"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "15+"] <- "15"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "2-3"] <- "2"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "3 or 4"] <- "3"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "30+"] <- "30"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "5-6"] <- "5"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "60 percent"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "A lot"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "all"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "ALL"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "forget"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "lots"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "N/A"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "No"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "NON"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "none"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "NONE"] <- "7777: Poor Data Quality"
df$Q77_sex_w_men_number_anal[df$Q77_sex_w_men_number_anal == "was blacked out..."] <- "7777: Poor Data Quality"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q77_sex_w_men_number_anal[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"

# ----------------------
# Label Missing
# ----------------------
df$Q77_sex_w_men_number_anal[is.na(df$Q77_sex_w_men_number_anal)] <- "9999: True Missing"

# ======================================================
# Q78_sex_w_men_anal_position
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q78_sex_w_men_anal_position)

# ----------------------
# Data Quality
# ----------------------
df$Q78_sex_w_men_anal_position[df$Q78_sex_w_men_anal_position == "Multiple answers selected"] <- "Versatile (both top and bottom)"

# ----------------------
# Account for Skip Logic
# ----------------------
df$Q78_sex_w_men_anal_position[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"

# ----------------------
# Label Missing
# ----------------------
df$Q78_sex_w_men_anal_position[is.na(df$Q78_sex_w_men_anal_position)] <- "9999: True Missing"


# ======================================================
# Q79 Kinds of Partners
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q79_sex_w_men_anal_PrEP)
table(df$Q79_sex_w_men_anal_undetectable)
table(df$Q79_sex_w_men_anal_unknown_HIV_status)
table(df$Q79_sex_w_men_anal_opposite_HIV_status)
table(df$Q79_sex_w_men_anal_age)
table(df$Q79_sex_w_men_anal_race)
table(df$Q79_sex_w_men_anal_language)
table(df$Q79_sex_w_men_anal_ONS)
table(df$Q79_sex_w_men_anal_regular)
table(df$Q79_sex_w_men_anal_none)

# ----------------------
# Recode Yes's
# ----------------------
df$Q79_sex_w_men_anal_PrEP[!is.na(df$Q79_sex_w_men_anal_PrEP)] <- "Yes"
df$Q79_sex_w_men_anal_undetectable[!is.na(df$Q79_sex_w_men_anal_undetectable)] <- "Yes"
df$Q79_sex_w_men_anal_unknown_HIV_status[!is.na(df$Q79_sex_w_men_anal_unknown_HIV_status)] <- "Yes"
df$Q79_sex_w_men_anal_opposite_HIV_status[!is.na(df$Q79_sex_w_men_anal_opposite_HIV_status)] <- "Yes"
df$Q79_sex_w_men_anal_age[!is.na(df$Q79_sex_w_men_anal_age)] <- "Yes"
df$Q79_sex_w_men_anal_race[!is.na(df$Q79_sex_w_men_anal_race)] <- "Yes"
df$Q79_sex_w_men_anal_language[!is.na(df$Q79_sex_w_men_anal_language)] <- "Yes"
df$Q79_sex_w_men_anal_ONS[!is.na(df$Q79_sex_w_men_anal_ONS)] <- "Yes"
df$Q79_sex_w_men_anal_regular[!is.na(df$Q79_sex_w_men_anal_regular)] <- "Yes"
df$Q79_sex_w_men_anal_none[!is.na(df$Q79_sex_w_men_anal_none)] <- "Yes"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q79_sex_w_men_anal_valid <- NA
df$Q79_sex_w_men_anal_valid[df$Q79_sex_w_men_anal_PrEP == "Yes" |
                              df$Q79_sex_w_men_anal_undetectable == "Yes" |
                              df$Q79_sex_w_men_anal_unknown_HIV_status == "Yes" |
                              df$Q79_sex_w_men_anal_opposite_HIV_status == "Yes" |
                              df$Q79_sex_w_men_anal_age == "Yes" |
                              df$Q79_sex_w_men_anal_race == "Yes" |
                              df$Q79_sex_w_men_anal_language == "Yes" |
                              df$Q79_sex_w_men_anal_ONS == "Yes" |
                              df$Q79_sex_w_men_anal_regular == "Yes" |
                              df$Q79_sex_w_men_anal_none == "Yes" ] <- "Yes"
df$Q79_sex_w_men_anal_valid[is.na(df$Q79_sex_w_men_anal_valid)] <- "No"

table(df$Q79_sex_w_men_anal_valid)

# ----------------------
# Creating "No" Level
# ----------------------
df$Q79_sex_w_men_anal_PrEP[is.na(df$Q79_sex_w_men_anal_PrEP) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_undetectable[is.na(df$Q79_sex_w_men_anal_undetectable) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_unknown_HIV_status[is.na(df$Q79_sex_w_men_anal_unknown_HIV_status) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_opposite_HIV_status[is.na(df$Q79_sex_w_men_anal_opposite_HIV_status) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_age[is.na(df$Q79_sex_w_men_anal_age) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_race[is.na(df$Q79_sex_w_men_anal_race) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_language[is.na(df$Q79_sex_w_men_anal_language) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_ONS[is.na(df$Q79_sex_w_men_anal_ONS) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_regular[is.na(df$Q79_sex_w_men_anal_regular) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"
df$Q79_sex_w_men_anal_none[is.na(df$Q79_sex_w_men_anal_none) & df$Q79_sex_w_men_anal_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q79_sex_w_men_anal_PrEP[df$Q79_sex_w_men_anal_PrEP == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_PrEP == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_undetectable[df$Q79_sex_w_men_anal_undetectable == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_undetectable == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_unknown_HIV_status[df$Q79_sex_w_men_anal_unknown_HIV_status == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_unknown_HIV_status == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_opposite_HIV_status[df$Q79_sex_w_men_anal_opposite_HIV_status == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_opposite_HIV_status == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_age[df$Q79_sex_w_men_anal_age == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_age == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_race[df$Q79_sex_w_men_anal_race == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_race == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_language[df$Q79_sex_w_men_anal_language == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_language == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_ONS[df$Q79_sex_w_men_anal_ONS == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_ONS == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

df$Q79_sex_w_men_anal_regular[df$Q79_sex_w_men_anal_regular == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_regular == "Yes" & df$Q79_sex_w_men_anal_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q79_sex_w_men_anal_PrEP[is.na(df$Q79_sex_w_men_anal_PrEP) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_undetectable[is.na(df$Q79_sex_w_men_anal_undetectable) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_unknown_HIV_status[is.na(df$Q79_sex_w_men_anal_unknown_HIV_status) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_opposite_HIV_status[is.na(df$Q79_sex_w_men_anal_opposite_HIV_status) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_age[is.na(df$Q79_sex_w_men_anal_age) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_race[is.na(df$Q79_sex_w_men_anal_race) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_language[is.na(df$Q79_sex_w_men_anal_language) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_ONS[is.na(df$Q79_sex_w_men_anal_ONS) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_regular[is.na(df$Q79_sex_w_men_anal_regular) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"
df$Q79_sex_w_men_anal_none[is.na(df$Q79_sex_w_men_anal_none) & df$Q79_sex_w_men_anal_valid == "No"] <- "9999: True Missing"

# ----------------------
# Correct No / Missing Level Due to no "none" option by looking to nearby answers
# ----------------------
df$Q79_sex_w_men_anal_PrEP[df$Q79_sex_w_men_anal_PrEP == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_undetectable[df$Q79_sex_w_men_anal_undetectable == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_unknown_HIV_status[df$Q79_sex_w_men_anal_unknown_HIV_status == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_opposite_HIV_status[df$Q79_sex_w_men_anal_opposite_HIV_status == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_age[df$Q79_sex_w_men_anal_age == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_race[df$Q79_sex_w_men_anal_race == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_language[df$Q79_sex_w_men_anal_language == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_ONS[df$Q79_sex_w_men_anal_ONS == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_regular[df$Q79_sex_w_men_anal_regular == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"
df$Q79_sex_w_men_anal_none[df$Q79_sex_w_men_anal_none == "9999: True Missing" & df$Q78_sex_w_men_anal_position != "9999: True Missing"] <- "No"

# ----------------------
# Accounting for Skip Logic
# ----------------------
df$Q79_sex_w_men_anal_PrEP[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_undetectable[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_unknown_HIV_status[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_opposite_HIV_status[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_age[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_race[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_language[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_ONS[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_regular[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"
df$Q79_sex_w_men_anal_none[df$Q76_sex_w_men_ever == "No"] <- "8888: Never Had Sex with a Man"

# ======================================================
# Q80 HIV Prevention Strategies 
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q80_HIV_prevention_condoms)
table(df$Q80_HIV_prevention_bottom)
table(df$Q80_HIV_prevention_top)
table(df$Q80_HIV_prevention_same_HIV_status)
table(df$Q80_HIV_prevention_partner_on_PrEP)
table(df$Q80_HIV_prevention_undetectable)
table(df$Q80_HIV_prevention_no_anal)
table(df$Q80_HIV_prevention_PEP)
table(df$Q80_HIV_prevention_PrEP)
table(df$Q80_HIV_prevention_ask_status)
table(df$Q80_HIV_prevention_monogamy)
table(df$Q80_HIV_prevention_none)

# ----------------------
# Recode Yes's
# ----------------------
df$Q80_HIV_prevention_condoms[!is.na(df$Q80_HIV_prevention_condoms)] <- "Yes"
df$Q80_HIV_prevention_bottom[!is.na(df$Q80_HIV_prevention_bottom)] <- "Yes"
df$Q80_HIV_prevention_top[!is.na(df$Q80_HIV_prevention_top)] <- "Yes"
df$Q80_HIV_prevention_same_HIV_status[!is.na(df$Q80_HIV_prevention_same_HIV_status)] <- "Yes"
df$Q80_HIV_prevention_partner_on_PrEP[!is.na(df$Q80_HIV_prevention_partner_on_PrEP)] <- "Yes"
df$Q80_HIV_prevention_undetectable[!is.na(df$Q80_HIV_prevention_undetectable)] <- "Yes"
df$Q80_HIV_prevention_no_anal[!is.na(df$Q80_HIV_prevention_no_anal)] <- "Yes"
df$Q80_HIV_prevention_PEP[!is.na(df$Q80_HIV_prevention_PEP)] <- "Yes"
df$Q80_HIV_prevention_PrEP[!is.na(df$Q80_HIV_prevention_PrEP)] <- "Yes"
df$Q80_HIV_prevention_ask_status[!is.na(df$Q80_HIV_prevention_ask_status)] <- "Yes"
df$Q80_HIV_prevention_monogamy[!is.na(df$Q80_HIV_prevention_monogamy)] <- "Yes"
df$Q80_HIV_prevention_none[!is.na(df$Q80_HIV_prevention_none)] <- "Yes"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q80_HIV_prevention_valid <- NA
df$Q80_HIV_prevention_valid[df$Q80_HIV_prevention_condoms == "Yes" |
                              df$Q80_HIV_prevention_bottom == "Yes" |
                              df$Q80_HIV_prevention_top == "Yes" |
                              df$Q80_HIV_prevention_same_HIV_status == "Yes" |
                              df$Q80_HIV_prevention_partner_on_PrEP == "Yes" |
                              df$Q80_HIV_prevention_undetectable == "Yes" |
                              df$Q80_HIV_prevention_no_anal == "Yes" |
                              df$Q80_HIV_prevention_PEP == "Yes" |
                              df$Q80_HIV_prevention_PrEP == "Yes" |
                              df$Q80_HIV_prevention_ask_status == "Yes" |
                              df$Q80_HIV_prevention_monogamy == "Yes" |
                              df$Q80_HIV_prevention_none == "Yes" ] <- "Yes"
df$Q80_HIV_prevention_valid[is.na(df$Q80_HIV_prevention_valid)] <- "No"

table(df$Q80_HIV_prevention_valid)

# ----------------------
# Creating "No" Level
# ----------------------
df$Q80_HIV_prevention_condoms[is.na(df$Q80_HIV_prevention_condoms) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_bottom[is.na(df$Q80_HIV_prevention_bottom) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_top[is.na(df$Q80_HIV_prevention_top) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_same_HIV_status[is.na(df$Q80_HIV_prevention_same_HIV_status) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_partner_on_PrEP[is.na(df$Q80_HIV_prevention_partner_on_PrEP) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_undetectable[is.na(df$Q80_HIV_prevention_undetectable) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_no_anal[is.na(df$Q80_HIV_prevention_no_anal) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_PEP[is.na(df$Q80_HIV_prevention_PEP) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_PrEP[is.na(df$Q80_HIV_prevention_PrEP) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_ask_status[is.na(df$Q80_HIV_prevention_ask_status) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_monogamy[is.na(df$Q80_HIV_prevention_monogamy) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"
df$Q80_HIV_prevention_none[is.na(df$Q80_HIV_prevention_none) & df$Q80_HIV_prevention_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q80_HIV_prevention_condoms[df$Q80_HIV_prevention_condoms == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_condoms == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_bottom[df$Q80_HIV_prevention_bottom == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_bottom == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_top[df$Q80_HIV_prevention_top == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_top == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_same_HIV_status[df$Q80_HIV_prevention_same_HIV_status == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_same_HIV_status == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_partner_on_PrEP[df$Q80_HIV_prevention_partner_on_PrEP == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_partner_on_PrEP == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_undetectable[df$Q80_HIV_prevention_undetectable == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_undetectable == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_no_anal[df$Q80_HIV_prevention_no_anal == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_no_anal == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_PEP[df$Q80_HIV_prevention_PEP == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_PEP == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_PrEP[df$Q80_HIV_prevention_PrEP == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_PrEP == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_ask_status[df$Q80_HIV_prevention_ask_status == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_ask_status == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

df$Q80_HIV_prevention_monogamy[df$Q80_HIV_prevention_monogamy == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"
df$Q80_HIV_prevention_none[df$Q80_HIV_prevention_monogamy == "Yes" & df$Q80_HIV_prevention_none == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Creating "Missing" Level
# ----------------------
df$Q80_HIV_prevention_condoms[is.na(df$Q80_HIV_prevention_condoms) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_bottom[is.na(df$Q80_HIV_prevention_bottom) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_top[is.na(df$Q80_HIV_prevention_top) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_same_HIV_status[is.na(df$Q80_HIV_prevention_same_HIV_status) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_partner_on_PrEP[is.na(df$Q80_HIV_prevention_partner_on_PrEP) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_undetectable[is.na(df$Q80_HIV_prevention_undetectable) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_no_anal[is.na(df$Q80_HIV_prevention_no_anal) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_PEP[is.na(df$Q80_HIV_prevention_PEP) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_PrEP[is.na(df$Q80_HIV_prevention_PrEP) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_ask_status[is.na(df$Q80_HIV_prevention_ask_status) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_monogamy[is.na(df$Q80_HIV_prevention_monogamy) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"
df$Q80_HIV_prevention_none[is.na(df$Q80_HIV_prevention_none) & df$Q80_HIV_prevention_valid == "No"] <- "9999: True Missing"

# ======================================================
# Q81 Enjoyable Sex Acts 
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q81_enjoy_most_bottoming)
table(df$Q81_enjoy_most_flip_fucking)
table(df$Q81_enjoy_most_topping)
table(df$Q81_enjoy_most_giving_head)
table(df$Q81_enjoy_most_getting_head)
table(df$Q81_enjoy_most_getting_rimmed)
table(df$Q81_enjoy_most_rimming)
table(df$Q81_enjoy_most_no_sex)
table(df$Q81_enjoy_most_other)
table(df$Q81_enjoy_most_other_text)

# ----------------------
# Create "Other" Variable
# ----------------------
df$Q81_enjoy_most_other <- NA
df$Q81_enjoy_most_other[!is.na(df$Q81_enjoy_most_other_text)] <- "Yes"
df$Q81_enjoy_most_other[is.na(df$Q81_enjoy_most_other_text)] <- "No"

df$Q81_enjoy_most_other_text[df$Q81_enjoy_most_other == "No"] <- "8888: Other Not Selected"
df$Q81_enjoy_most_other_text[is.na(df$Q81_enjoy_most_other_text) & df$Q81_enjoy_most_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Relabel "Yes" responses
# ----------------------
df$Q81_enjoy_most_bottoming[!is.na(df$Q81_enjoy_most_bottoming)] <- "Yes"
df$Q81_enjoy_most_topping[!is.na(df$Q81_enjoy_most_topping)] <- "Yes"
df$Q81_enjoy_most_flip_fucking[!is.na(df$Q81_enjoy_most_flip_fucking)] <- "Yes"
df$Q81_enjoy_most_giving_head[!is.na(df$Q81_enjoy_most_giving_head)] <- "Yes"
df$Q81_enjoy_most_getting_head[!is.na(df$Q81_enjoy_most_getting_head)] <- "Yes"
df$Q81_enjoy_most_getting_rimmed[!is.na(df$Q81_enjoy_most_getting_rimmed)] <- "Yes"
df$Q81_enjoy_most_rimming[!is.na(df$Q81_enjoy_most_rimming)] <- "Yes"
df$Q81_enjoy_most_no_sex[!is.na(df$Q81_enjoy_most_no_sex)] <- "Yes"

# ----------------------
# Create Variable to Identify Valid Responses to this Check-all-that apply question
# ----------------------
df$Q81_enjoy_most_valid <- NA
df$Q81_enjoy_most_valid[df$Q81_enjoy_most_bottoming == "Yes" |
                          df$Q81_enjoy_most_topping == "Yes" |
                          df$Q81_enjoy_most_flip_fucking == "Yes" |
                          df$Q81_enjoy_most_giving_head == "Yes" |
                          df$Q81_enjoy_most_getting_head == "Yes" |
                          df$Q81_enjoy_most_getting_rimmed == "Yes" |
                          df$Q81_enjoy_most_rimming == "Yes" |
                          df$Q81_enjoy_most_no_sex == "Yes"] <- "Yes"
df$Q81_enjoy_most_valid[is.na(df$Q81_enjoy_most_valid)] <- "No"

table(df$Q81_enjoy_most_valid)

# ----------------------
# Create No response
# ----------------------
df$Q81_enjoy_most_bottoming[is.na(df$Q81_enjoy_most_bottoming) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_flip_fucking[is.na(df$Q81_enjoy_most_flip_fucking) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_topping[is.na(df$Q81_enjoy_most_topping) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_giving_head[is.na(df$Q81_enjoy_most_giving_head) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_getting_head[is.na(df$Q81_enjoy_most_getting_head) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_getting_rimmed[is.na(df$Q81_enjoy_most_getting_rimmed) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_rimming[is.na(df$Q81_enjoy_most_rimming) & df$Q81_enjoy_most_valid == "Yes"] <- "No"
df$Q81_enjoy_most_no_sex[is.na(df$Q81_enjoy_most_no_sex) & df$Q81_enjoy_most_valid == "Yes"] <- "No"

# ----------------------
# Deal with internal conflicts
# ----------------------
df$Q81_enjoy_most_bottoming[df$Q81_enjoy_most_bottoming == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_bottoming == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_flip_fucking[df$Q81_enjoy_most_flip_fucking == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_flip_fucking == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_topping[df$Q81_enjoy_most_topping == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_topping == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_giving_head[df$Q81_enjoy_most_giving_head == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_giving_head == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_getting_head[df$Q81_enjoy_most_getting_head == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_getting_head == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_getting_rimmed[df$Q81_enjoy_most_getting_rimmed == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_getting_rimmed == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_rimming[df$Q81_enjoy_most_rimming == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_rimming == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

df$Q81_enjoy_most_other[df$Q81_enjoy_most_other == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_other == "Yes" & df$Q81_enjoy_most_no_sex == "Yes"] <- "7777: Poor Data Quality"

# ----------------------
# Create Missing Level
# ----------------------
df$Q81_enjoy_most_bottoming[is.na(df$Q81_enjoy_most_bottoming) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_flip_fucking[is.na(df$Q81_enjoy_most_flip_fucking) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_topping[is.na(df$Q81_enjoy_most_topping) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_giving_head[is.na(df$Q81_enjoy_most_giving_head) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_getting_head[is.na(df$Q81_enjoy_most_getting_head) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_getting_rimmed[is.na(df$Q81_enjoy_most_getting_rimmed) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_rimming[is.na(df$Q81_enjoy_most_rimming) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"
df$Q81_enjoy_most_no_sex[is.na(df$Q81_enjoy_most_no_sex) & df$Q81_enjoy_most_valid == "No"] <- "9999: True Missing"

# ----------------------
# Correct No / Missing Level Due to no "none" option by looking to nearby answers of non-check all that apply questions
# ----------------------
df$Q81_enjoy_most_bottoming[df$Q81_enjoy_most_bottoming == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_flip_fucking[df$Q81_enjoy_most_flip_fucking == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_topping[df$Q81_enjoy_most_topping == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_giving_head[df$Q81_enjoy_most_giving_head == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_getting_head[df$Q81_enjoy_most_getting_head == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_getting_rimmed[df$Q81_enjoy_most_getting_rimmed == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_rimming[df$Q81_enjoy_most_rimming == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"
df$Q81_enjoy_most_no_sex[df$Q81_enjoy_most_no_sex == "9999: True Missing" & df$Q80_HIV_prevention_none != "9999: True Missing"] <- "No"

# ======================================================
# Q82_role_model 
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$Q82_role_model)

# ----------------------
# Document Missings
# ----------------------
df$Q82_role_model[is.na(df$Q82_role_model)] <- "9999: True Missing"

# ======================================================
# ======================================================
# ======================================================
# ======================================================
# ======================================================
# ============== WINNIPEG-ONLY QUESTIONS ===============
# ======================================================
# ======================================================
# ======================================================
# ======================================================
# ======================================================

# ======================================================
# W1_environment
# ======================================================
# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W1_environment[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
# ----------------------
# Explore Data
# ----------------------
table(df$W1_environment)
# ----------------------
# Recode Missing
# ----------------------
df$W1_environment[is.na(df$W1_environment)] <- "9999: True Missing"


# ======================================================
# W2 Marriage
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W2_married_never)
table(df$W2_married_not_yet)
table(df$W2_married_yes_now)
table(df$W2_married_yes_previous)

# ----------------------
# Create "Valid" Valid Variable
# ----------------------
df$W2_married_valid <- NA
df$W2_married_valid[!is.na(df$W2_married_never) |
                      !is.na(df$W2_married_not_yet) |
                      !is.na(df$W2_married_yes_now) |
                      !is.na(df$W2_married_yes_previous)] <- "Yes"
df$W2_married_valid[is.na(df$W2_married_valid)] <- "No"
table(df$W2_married_valid)                         
# ----------------------
# Relabel Yes
# ----------------------
df$W2_married_never[df$W2_married_never == "No, and I don't ever want to"] <- "Yes"
df$W2_married_not_yet[df$W2_married_not_yet == "No, but I want to one day"] <- "Yes"
df$W2_married_yes_now[df$W2_married_yes_now == "Yes, now married"] <- "Yes"
df$W2_married_yes_previous[df$W2_married_yes_previous == "Yes, was married"] <- "Yes"

# ----------------------
# Create No Variable
# ----------------------
df$W2_married_never[is.na(df$W2_married_never)  & df$Q0_site == "Winnipeg" & df$W2_married_valid == "Yes"] <- "No"
df$W2_married_not_yet[is.na(df$W2_married_not_yet) & df$Q0_site == "Winnipeg" & df$W2_married_valid == "Yes"] <- "No"
df$W2_married_yes_now[is.na(df$W2_married_yes_now) & df$Q0_site == "Winnipeg" & df$W2_married_valid == "Yes"] <- "No"
df$W2_married_yes_previous[is.na(df$W2_married_yes_previous) & df$Q0_site == "Winnipeg" & df$W2_married_valid == "Yes"] <- "No"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W2_married_never[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W2_married_not_yet[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W2_married_yes_now[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W2_married_yes_previous[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Recode Missing
# ----------------------
df$W2_married_never[is.na(df$W2_married_never)] <- "9999: True Missing"
df$W2_married_not_yet[is.na(df$W2_married_not_yet)] <- "9999: True Missing"
df$W2_married_yes_now[is.na(df$W2_married_yes_now)] <- "9999: True Missing"
df$W2_married_yes_previous[is.na(df$W2_married_yes_previous)] <- "9999: True Missing"

# ----------------------
# Internal Consistencies
# ----------------------
df$W2_married_yes_now[df$W2_married_never == "Yes"] <- "7777: Poor Data Quality"
df$W2_married_yes_previous[df$W2_married_never == "Yes"] <- "7777: Poor Data Quality"
df$W2_married_never[df$W2_married_yes_now == "Yes"] <- "7777: Poor Data Quality"
df$W2_married_never[df$W2_married_yes_previous == "Yes"] <- "7777: Poor Data Quality"

# ======================================================
# W3_married_to
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W3_married_to)

# ----------------------
# Recode Missing
# ----------------------
df$W3_married_to[is.na(df$W3_married_to)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W3_married_to[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Account for Skip Logic
# ----------------------
df$W3_married_to[df$W2_married_yes_now == "No"] <- "8888: Not Married"


# ======================================================
# W4 Past Marriages
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W4_past_married_to_men)
table(df$W4_past_married_to_women)
table(df$W4_past_married_to_non_binary)

# ----------------------
# Create "Valid" Valid Variable
# ----------------------
df$W4_married_valid <- NA
df$W4_married_valid[!is.na(df$W4_past_married_to_men) |
                      df$W2_married_yes_previous == "Yes" |
                      !is.na(df$W4_past_married_to_women) |
                      !is.na(df$W4_past_married_to_non_binary)] <- "Yes"
df$W4_married_valid[is.na(df$W4_married_valid)] <- "No"
table(df$W4_married_valid)   

# ----------------------
# Create Yes
# ----------------------
df$W4_past_married_to_men[!is.na(df$W4_past_married_to_men)] <- "Yes" 
df$W4_past_married_to_women[!is.na(df$W4_past_married_to_women)] <- "Yes"
df$W4_past_married_to_non_binary[!is.na(df$W4_past_married_to_non_binary)] <- "Yes"

# ----------------------
# Create No
# ----------------------
df$W4_past_married_to_men[is.na(df$W4_past_married_to_men) & df$W4_married_valid == "Yes"] <- "No" 
df$W4_past_married_to_women[is.na(df$W4_past_married_to_women) & df$W4_married_valid == "Yes"] <- "No"
df$W4_past_married_to_non_binary[is.na(df$W4_past_married_to_non_binary) & df$W4_married_valid == "Yes"] <- "No"


# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W4_past_married_to_men[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W4_past_married_to_women[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W4_past_married_to_non_binary[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Account for Skip Logic
# ----------------------
df$W4_past_married_to_men[df$W2_married_yes_previous == "No"] <- "8888: Never Married"
df$W4_past_married_to_women[df$W2_married_yes_previous == "No"] <- "8888: Never Married"
df$W4_past_married_to_non_binary[df$W2_married_yes_previous == "No"] <- "8888: Never Married"

# ----------------------
# Create Missing
# ----------------------
df$W4_past_married_to_men[is.na(df$W4_past_married_to_men)] <- "9999: True Missing" 
df$W4_past_married_to_women[is.na(df$W4_past_married_to_women)] <- "9999: True Missing"
df$W4_past_married_to_non_binary[is.na(df$W4_past_married_to_non_binary)] <- "9999: True Missing"

# ======================================================
# W5 New Sex Partners
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W5_new_sex_partner_6_months)
table(df$W5_new_sex_partner_3_months)

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W5_new_sex_partner_6_months[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W5_new_sex_partner_3_months[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Check internal consistency
# ----------------------
df$W5_new_sex_partner_6_months[df$W5_new_sex_partner_3_months == "Yes"] <- "Yes"

# ----------------------
# Other Missings
# ----------------------
df$W5_new_sex_partner_6_months[is.na(df$W5_new_sex_partner_6_months)] <- "9999: True Missing"
df$W5_new_sex_partner_3_months[is.na(df$W5_new_sex_partner_3_months)] <- "9999: True Missing"

# ======================================================
# W6 STI Clinic Offerings
# ======================================================

# ----------------------
# Explore Data
# ----------------------
table(df$W6_STI_test_at_clinic)
table(df$W6_STI_test_substance_use)
table(df$W6_STI_test_offered_HIV_test)
table(df$W6_STI_test_offered_rapid_HIV_test)
table(df$W6_STI_test_none)

# ----------------------
# Create Yes Level
# ----------------------
df$W6_STI_test_at_clinic[!is.na(df$W6_STI_test_at_clinic)] <- "Yes"
df$W6_STI_test_substance_use[!is.na(df$W6_STI_test_substance_use)] <- "Yes"
df$W6_STI_test_offered_HIV_test[!is.na(df$W6_STI_test_offered_HIV_test)] <- "Yes"
df$W6_STI_test_offered_rapid_HIV_test[!is.na(df$W6_STI_test_offered_rapid_HIV_test)] <- "Yes"
df$W6_STI_test_none[!is.na(df$W6_STI_test_none)] <- "Yes"

# ----------------------
# Create Valid
# ----------------------
df$W6_STI_test_valid <- NA
df$W6_STI_test_valid[!is.na(df$W6_STI_test_at_clinic) |
                       !is.na(df$W6_STI_test_substance_use) |
                       !is.na(df$W6_STI_test_offered_HIV_test) |
                       !is.na(df$W6_STI_test_offered_rapid_HIV_test) |
                       !is.na(df$W6_STI_test_none)] <- "Yes"
df$W6_STI_test_valid[is.na(df$W6_STI_test_valid)] <- "No"
table(df$W6_STI_test_valid) 

# ----------------------
# Create No Level
# ----------------------
df$W6_STI_test_at_clinic[is.na(df$W6_STI_test_at_clinic) & df$W6_STI_test_valid == "Yes"] <- "No"
df$W6_STI_test_substance_use[is.na(df$W6_STI_test_substance_use) & df$W6_STI_test_valid == "Yes"] <- "No"
df$W6_STI_test_offered_HIV_test[is.na(df$W6_STI_test_offered_HIV_test) & df$W6_STI_test_valid == "Yes"] <- "No"
df$W6_STI_test_offered_rapid_HIV_test[is.na(df$W6_STI_test_offered_rapid_HIV_test) & df$W6_STI_test_valid == "Yes"] <- "No"
df$W6_STI_test_none[is.na(df$W6_STI_test_none) & df$W6_STI_test_valid == "Yes"] <- "No"

# ----------------------
# Create No Level
# ----------------------
df$W6_STI_test_at_clinic[is.na(df$W6_STI_test_at_clinic) & df$W6_STI_test_valid == "No"] <- "9999: True Missing"
df$W6_STI_test_substance_use[is.na(df$W6_STI_test_substance_use) & df$W6_STI_test_valid == "No"] <- "9999: True Missing"
df$W6_STI_test_offered_HIV_test[is.na(df$W6_STI_test_offered_HIV_test) & df$W6_STI_test_valid == "No"] <- "9999: True Missing"
df$W6_STI_test_offered_rapid_HIV_test[is.na(df$W6_STI_test_offered_rapid_HIV_test) & df$W6_STI_test_valid == "No"] <- "9999: True Missing"
df$W6_STI_test_none[is.na(df$W6_STI_test_none) & df$W6_STI_test_valid == "No"] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W6_STI_test_at_clinic[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W6_STI_test_substance_use[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W6_STI_test_offered_HIV_test[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W6_STI_test_offered_rapid_HIV_test[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W6_STI_test_none[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Internal Conflicts
# ----------------------
df$W6_STI_test_none[df$W6_STI_test_none == "Yes" & df$W6_STI_test_at_clinic == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_none[df$W6_STI_test_none == "Yes"  & df$W6_STI_test_substance_use == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_none[df$W6_STI_test_none == "Yes"  & df$W6_STI_test_offered_HIV_test == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_none[df$W6_STI_test_none == "Yes"  & df$W6_STI_test_offered_rapid_HIV_test == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_at_clinic[df$W6_STI_test_none == "Yes" & df$W6_STI_test_at_clinic == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_substance_use[df$W6_STI_test_none == "Yes"  & df$W6_STI_test_substance_use == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_offered_HIV_test[df$W6_STI_test_none == "Yes"  & df$W6_STI_test_offered_HIV_test == "Yes"] <- "7777: Poor Data Quality"
df$W6_STI_test_offered_rapid_HIV_test[df$W6_STI_test_none == "Yes"  & df$W6_STI_test_offered_rapid_HIV_test == "Yes"] <- "7777: Poor Data Quality"

# ======================================================
# W7_amount_willing_to_pay
# ======================================================
# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W7_amount_willing_to_pay[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Explore Data
# ----------------------
table(df$W7_amount_willing_to_pay)

# ----------------------
# Data Quality
# ----------------------
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "30-40"] <- "35"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "30-45"] <- "45"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "0-50"] <- "50"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "50-100"] <- "100"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "10-15"] <- "15"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "10-20"] <- "20"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "15 - under"] <- "15"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "25-30"] <- "30"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "Don't know"] <- "7777: Poor Data Quality"
df$W7_amount_willing_to_pay[df$W7_amount_willing_to_pay == "Any"] <- "7777: Poor Data Quality"

# ----------------------
# Missings
# ----------------------
df$W7_amount_willing_to_pay[is.na(df$W7_amount_willing_to_pay)] <- "9999: True Missing"

# ======================================================
# W8 Prevalence Guesses
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W8_guess_perc_living_w_HIV)
table(df$W8_guess_perc_using_PrEP)
table(df$W8_guess_perc_using_condoms)
table(df$W8_guess_perc_undetectable)

# ----------------------
# Data Quality
# ----------------------
df$W8_guess_perc_using_PrEP[df$W8_guess_perc_using_PrEP == "1-5"] <- "2.5"
df$W8_guess_perc_using_condoms[df$W8_guess_perc_using_condoms == "50-60"] <- "55"

# ----------------------
# Missings
# ----------------------
df$W8_guess_perc_living_w_HIV[is.na(df$W8_guess_perc_living_w_HIV)] <- "9999: True Missing"
df$W8_guess_perc_using_PrEP[is.na(df$W8_guess_perc_using_PrEP)] <- "9999: True Missing"
df$W8_guess_perc_using_condoms[is.na(df$W8_guess_perc_using_condoms)] <- "9999: True Missing"
df$W8_guess_perc_undetectable[is.na(df$W8_guess_perc_undetectable)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W8_guess_perc_living_w_HIV[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W8_guess_perc_using_PrEP[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W8_guess_perc_using_condoms[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W8_guess_perc_undetectable[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W9 Delays
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W9_delay_HIV_none)
table(df$W9_delay_HIV_busy)
table(df$W9_delay_HIV_stressed)
table(df$W9_delay_HIV_distance)
table(df$W9_delay_HIV_hours)
table(df$W9_delay_HIV_privacy)
table(df$W9_delay_HIV_wait)
table(df$W9_delay_HIV_language)
table(df$W9_delay_HIV_sensitivity)
table(df$W9_delay_HIV_cost)
table(df$W9_delay_HIV_other)
table(df$W9_delay_HIV_other_text)

df$W9_delay_HIV_other <- NA
df$W9_delay_HIV_other[is.na(df$W9_delay_HIV_other_text)] <- "No"
df$W9_delay_HIV_other[!is.na(df$W9_delay_HIV_other_text)] <- "Yes"

df$W9_delay_HIV_other_text[df$W9_delay_HIV_other == "No"] <- "8888: Other Not Selected"
df$W9_delay_HIV_other_text[is.na(df$W9_delay_HIV_other_text) & df$W9_delay_HIV_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Relabel Yes
# ----------------------
df$W9_delay_HIV_none[!is.na(df$W9_delay_HIV_none)] <- "Yes"
df$W9_delay_HIV_busy[!is.na(df$W9_delay_HIV_busy)] <- "Yes"
df$W9_delay_HIV_stressed[!is.na(df$W9_delay_HIV_stressed)] <- "Yes"
df$W9_delay_HIV_distance[!is.na(df$W9_delay_HIV_distance)] <- "Yes"
df$W9_delay_HIV_hours[!is.na(df$W9_delay_HIV_hours)] <- "Yes"
df$W9_delay_HIV_privacy[!is.na(df$W9_delay_HIV_privacy)] <- "Yes"
df$W9_delay_HIV_wait[!is.na(df$W9_delay_HIV_wait)] <- "Yes" 
df$W9_delay_HIV_language[!is.na(df$W9_delay_HIV_language)] <- "Yes"
df$W9_delay_HIV_sensitivity[!is.na(df$W9_delay_HIV_sensitivity)] <- "Yes"
df$W9_delay_HIV_cost[!is.na(df$W9_delay_HIV_cost)] <- "Yes"
df$W9_delay_HIV_other <- NA
df$W9_delay_HIV_other[!is.na(df$W9_delay_HIV_other_text)] <- "Yes"

# ----------------------
# Create a valid response variable
# ----------------------
df$W9_delay_HIV_valid <- NA
df$W9_delay_HIV_valid[!is.na(df$W9_delay_HIV_none) |
                        !is.na(df$W9_delay_HIV_busy) |
                        !is.na(df$W9_delay_HIV_stressed) |
                        !is.na(df$W9_delay_HIV_distance) |
                        !is.na(df$W9_delay_HIV_hours) |
                        !is.na(df$W9_delay_HIV_privacy) |
                        !is.na(df$W9_delay_HIV_wait) |
                        !is.na(df$W9_delay_HIV_language) |
                        !is.na(df$W9_delay_HIV_sensitivity) |
                        !is.na(df$W9_delay_HIV_cost) |
                        !is.na(df$W9_delay_HIV_other) ] <- "Yes"
df$W9_delay_HIV_valid[is.na(df$W9_delay_HIV_valid)] <- "No"
table(df$W9_delay_HIV_valid)

# ----------------------
# Relabel No
# ----------------------
df$W9_delay_HIV_none[is.na(df$W9_delay_HIV_none) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_busy[is.na(df$W9_delay_HIV_busy) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_stressed[is.na(df$W9_delay_HIV_stressed) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_distance[is.na(df$W9_delay_HIV_distance) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_hours[is.na(df$W9_delay_HIV_hours) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_privacy[is.na(df$W9_delay_HIV_privacy) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_wait[is.na(df$W9_delay_HIV_wait) & df$W9_delay_HIV_valid == "Yes"] <- "No" 
df$W9_delay_HIV_language[is.na(df$W9_delay_HIV_language) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_sensitivity[is.na(df$W9_delay_HIV_sensitivity) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_cost[is.na(df$W9_delay_HIV_cost) & df$W9_delay_HIV_valid == "Yes"] <- "No"
df$W9_delay_HIV_other[is.na(df$W9_delay_HIV_other) & df$W9_delay_HIV_valid == "Yes"] <- "No"

# ----------------------
# Missing
# ----------------------
df$W9_delay_HIV_none[is.na(df$W9_delay_HIV_none) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_busy[is.na(df$W9_delay_HIV_busy) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_stressed[is.na(df$W9_delay_HIV_stressed) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_distance[is.na(df$W9_delay_HIV_distance) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_hours[is.na(df$W9_delay_HIV_hours) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_privacy[is.na(df$W9_delay_HIV_privacy) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_wait[is.na(df$W9_delay_HIV_wait) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing" 
df$W9_delay_HIV_language[is.na(df$W9_delay_HIV_language) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_sensitivity[is.na(df$W9_delay_HIV_sensitivity) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_cost[is.na(df$W9_delay_HIV_cost) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"
df$W9_delay_HIV_other[is.na(df$W9_delay_HIV_other) & df$W9_delay_HIV_valid == "No"] <- "9999: True Missing"


# ----------------------
# Account for HIV Status
# ----------------------
df$W9_delay_HIV_none[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_busy[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_stressed[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_distance[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_hours[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_privacy[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_wait[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_language[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_sensitivity[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_cost[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"
df$W9_delay_HIV_other[df$Q43_HIV_ever_diagnosed == "No"] <- "8888: Never Diagnosed with HIV"

# ----------------------
# Non-Winnipeg
# ----------------------
df$W9_delay_HIV_none[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_busy[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_stressed[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_distance[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_hours[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_privacy[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_wait[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_language[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_sensitivity[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_cost[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W9_delay_HIV_other[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Internal Conflicts
# ----------------------
df$W9_delay_HIV_busy[df$W9_delay_HIV_busy == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_stressed[df$W9_delay_HIV_stressed == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_distance[df$W9_delay_HIV_distance == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_hours[df$W9_delay_HIV_hours == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_privacy[df$W9_delay_HIV_privacy == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_wait[df$W9_delay_HIV_wait == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality" 
df$W9_delay_HIV_language[df$W9_delay_HIV_language == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_sensitivity[df$W9_delay_HIV_sensitivity == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_cost[df$W9_delay_HIV_cost == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_other[df$W9_delay_HIV_other == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"

df$W9_delay_HIV_none[df$W9_delay_HIV_busy == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_stressed == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_distance == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_hours == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_privacy == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_wait == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality" 
df$W9_delay_HIV_none[df$W9_delay_HIV_language == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_sensitivity == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_cost == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"
df$W9_delay_HIV_none[df$W9_delay_HIV_other == "Yes" & df$W9_delay_HIV_none == "Yes"] <- "7777: Poor Data Quality"


# ======================================================
# W10 Testing Frequency
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W10_when_test_HIV_never)
table(df$W10_when_test_HIV_3_months)
table(df$W10_when_test_HIV_twice_a_year)
table(df$W10_when_test_HIV_once_a_year)
table(df$W10_when_test_HIV_few_years)
table(df$W10_when_test_HIV_before_new_partner)
table(df$W10_when_test_HIV_after_new_partner)
table(df$W10_when_test_HIV_after_risky_sex)

# ----------------------
# Relabel Yes
# ----------------------
df$W10_when_test_HIV_never[!is.na(df$W10_when_test_HIV_never)] <- "Yes"
df$W10_when_test_HIV_3_months[!is.na(df$W10_when_test_HIV_3_months)] <- "Yes"
df$W10_when_test_HIV_twice_a_year[!is.na(df$W10_when_test_HIV_twice_a_year)] <- "Yes"
df$W10_when_test_HIV_once_a_year[!is.na(df$W10_when_test_HIV_once_a_year)] <- "Yes"
df$W10_when_test_HIV_few_years[!is.na(df$W10_when_test_HIV_few_years)] <- "Yes"
df$W10_when_test_HIV_before_new_partner[!is.na(df$W10_when_test_HIV_before_new_partner)] <- "Yes"
df$W10_when_test_HIV_after_new_partner[!is.na(df$W10_when_test_HIV_after_new_partner)] <- "Yes"
df$W10_when_test_HIV_after_risky_sex[!is.na(df$W10_when_test_HIV_after_risky_sex)] <- "Yes"

# ----------------------
# Create a valid response variable
# ----------------------
df$W10_when_test_HIV_valid <- NA
df$W10_when_test_HIV_valid[!is.na(df$W10_when_test_HIV_never) |
                             !is.na(df$W10_when_test_HIV_3_months) |
                             !is.na(df$W10_when_test_HIV_twice_a_year) |
                             !is.na(df$W10_when_test_HIV_once_a_year) |
                             !is.na(df$W10_when_test_HIV_few_years) |
                             !is.na(df$W10_when_test_HIV_before_new_partner) |
                             !is.na(df$W10_when_test_HIV_after_new_partner) |
                             !is.na(df$W10_when_test_HIV_after_risky_sex)] <- "Yes"
df$W10_when_test_HIV_valid[is.na(df$W10_when_test_HIV_valid)] <- "No"
table(df$W10_when_test_HIV_valid)

# ----------------------
# Relabel No
# ----------------------
df$W10_when_test_HIV_never[is.na(df$W10_when_test_HIV_never) & df$W10_when_test_HIV_valid == "Yes"] <- "No"
df$W10_when_test_HIV_3_months[is.na(df$W10_when_test_HIV_3_months) & df$W10_when_test_HIV_valid == "Yes"] <- "No"
df$W10_when_test_HIV_twice_a_year[is.na(df$W10_when_test_HIV_twice_a_year) & df$W10_when_test_HIV_valid == "Yes"] <- "No"
df$W10_when_test_HIV_once_a_year[is.na(df$W10_when_test_HIV_once_a_year) & df$W10_when_test_HIV_valid == "Yes"] <- "No"
df$W10_when_test_HIV_few_years[is.na(df$W10_when_test_HIV_few_years) & df$W10_when_test_HIV_valid == "Yes"] <- "No"
df$W10_when_test_HIV_before_new_partner[is.na(df$W10_when_test_HIV_before_new_partner) & df$W10_when_test_HIV_valid == "Yes"] <- "No"
df$W10_when_test_HIV_after_new_partner[is.na(df$W10_when_test_HIV_after_new_partner) & df$W10_when_test_HIV_valid == "Yes"] <- "No" 
df$W10_when_test_HIV_after_risky_sex[is.na(df$W10_when_test_HIV_after_risky_sex) & df$W10_when_test_HIV_valid == "Yes"] <- "No"

# ----------------------
# Missing
# ----------------------
df$W10_when_test_HIV_never[is.na(df$W10_when_test_HIV_never) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"
df$W10_when_test_HIV_3_months[is.na(df$W10_when_test_HIV_3_months) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"
df$W10_when_test_HIV_twice_a_year[is.na(df$W10_when_test_HIV_twice_a_year) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"
df$W10_when_test_HIV_once_a_year[is.na(df$W10_when_test_HIV_once_a_year) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"
df$W10_when_test_HIV_few_years[is.na(df$W10_when_test_HIV_few_years) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"
df$W10_when_test_HIV_before_new_partner[is.na(df$W10_when_test_HIV_before_new_partner) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"
df$W10_when_test_HIV_after_new_partner[is.na(df$W10_when_test_HIV_after_new_partner) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing" 
df$W10_when_test_HIV_after_risky_sex[is.na(df$W10_when_test_HIV_after_risky_sex) & df$W10_when_test_HIV_valid == "No"] <- "9999: True Missing"


# ----------------------
# Account for HIV Status
# ----------------------
df$W10_when_test_HIV_never[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_3_months[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_twice_a_year[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_once_a_year[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_few_years[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_before_new_partner[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_after_new_partner[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"
df$W10_when_test_HIV_after_risky_sex[df$Q43_HIV_ever_diagnosed == "Yes"] <- "8888: HIV-Positive"

# ----------------------
# Non-Winnipeg
# ----------------------
df$W10_when_test_HIV_never[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_3_months[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_twice_a_year[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_once_a_year[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_few_years[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_after_new_partner[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_before_new_partner[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W10_when_test_HIV_after_risky_sex[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Internal Conflicts
# ----------------------
df$W10_when_test_HIV_3_months[df$W10_when_test_HIV_3_months == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_twice_a_year[df$W10_when_test_HIV_twice_a_year == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_once_a_year[df$W10_when_test_HIV_once_a_year == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_few_years[df$W10_when_test_HIV_few_years == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_after_new_partner[df$W10_when_test_HIV_after_new_partner == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_before_new_partner[df$W10_when_test_HIV_before_new_partner == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality" 
df$W10_when_test_HIV_after_risky_sex[df$W10_when_test_HIV_after_risky_sex == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"

df$W10_when_test_HIV_never[df$W10_when_test_HIV_3_months == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_never[df$W10_when_test_HIV_twice_a_year == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_never[df$W10_when_test_HIV_once_a_year == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_never[df$W10_when_test_HIV_few_years == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_never[df$W10_when_test_HIV_after_new_partner == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"
df$W10_when_test_HIV_never[df$W10_when_test_HIV_before_new_partner == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality" 
df$W10_when_test_HIV_never[df$W10_when_test_HIV_after_risky_sex == "Yes" & df$W10_when_test_HIV_never == "Yes"] <- "7777: Poor Data Quality"

# ======================================================
# W11 Substance Use Motives
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W11_why_substances_feel_good)
table(df$W11_why_substances_feel_better)
table(df$W11_why_substances_connect_socially)
table(df$W11_why_substances_connect_sexually)
table(df$W11_why_substances_energy)
table(df$W11_why_substances_motivation)
table(df$W11_why_substances_intense)
table(df$W11_why_substances_last_longer)
table(df$W11_why_substances_stop_worry)
table(df$W11_why_substances_stress_sexuality)
table(df$W11_why_substances_stress_gender)
table(df$W11_why_substances_others_use)
table(df$W11_why_substances_others_offer)
table(df$W11_why_substances_addicted)
table(df$W11_why_substances_none)
table(df$W11_why_substances_other_text)

# ----------------------
# Create Other Variable
# ----------------------
df$W11_why_substances_other <- NA
df$W11_why_substances_other[!is.na(df$W11_why_substances_other_text)] <- "Yes"
df$W11_why_substances_other[is.na(df$W11_why_substances_other_text)] <- "Yes"

df$W11_why_substances_other_text[df$W11_why_substances_other == "No"] <- "8888: Other Not Selected"
df$W11_why_substances_other_text[is.na(df$W11_why_substances_other_text) & df$W11_why_substances_other == "Yes"] <- "9999: True Missing"
table(df$W11_why_substances_other)

# ----------------------
# Relabel Yes
# ----------------------
df$W11_why_substances_feel_good[!is.na(df$W11_why_substances_feel_good)] <- "Yes"
df$W11_why_substances_feel_better[!is.na(df$W11_why_substances_feel_better)] <- "Yes"
df$W11_why_substances_connect_socially[!is.na(df$W11_why_substances_connect_socially)] <- "Yes"
df$W11_why_substances_connect_sexually[!is.na(df$W11_why_substances_connect_sexually)] <- "Yes"
df$W11_why_substances_energy[!is.na(df$W11_why_substances_energy)] <- "Yes"
df$W11_why_substances_motivation[!is.na(df$W11_why_substances_motivation)] <- "Yes"
df$W11_why_substances_intense[!is.na(df$W11_why_substances_intense)] <- "Yes"
df$W11_why_substances_last_longer[!is.na(df$W11_why_substances_last_longer)] <- "Yes" 
df$W11_why_substances_stop_worry[!is.na(df$W11_why_substances_stop_worry)] <- "Yes"
df$W11_why_substances_stress_sexuality[!is.na(df$W11_why_substances_stress_sexuality)] <- "Yes"
df$W11_why_substances_stress_gender[!is.na(df$W11_why_substances_stress_gender)] <- "Yes"
df$W11_why_substances_others_use[!is.na(df$W11_why_substances_others_use)] <- "Yes"
df$W11_why_substances_others_offer[!is.na(df$W11_why_substances_others_offer)] <- "Yes"
df$W11_why_substances_addicted[!is.na(df$W11_why_substances_addicted)] <- "Yes"
df$W11_why_substances_none[!is.na(df$W11_why_substances_none)] <- "Yes"


# ----------------------
# Create a valid response variable
# ----------------------
df$W11_why_substances_valid <- NA
df$W11_why_substances_valid[!is.na(df$W11_why_substances_feel_good) |
                              !is.na(df$W11_why_substances_feel_better) |
                              !is.na(df$W11_why_substances_connect_socially) |
                              !is.na(df$W11_why_substances_connect_sexually) |
                              !is.na(df$W11_why_substances_energy) |
                              !is.na(df$W11_why_substances_motivation) |
                              !is.na(df$W11_why_substances_intense) |
                              !is.na(df$W11_why_substances_last_longer) |
                              !is.na(df$W11_why_substances_stop_worry) |
                              !is.na(df$W11_why_substances_stress_sexuality) |
                              !is.na(df$W11_why_substances_stress_gender) |
                              !is.na(df$W11_why_substances_others_use) |
                              !is.na(df$W11_why_substances_others_offer) |
                              !is.na(df$W11_why_substances_addicted) |
                              !is.na(df$W11_why_substances_other) |
                              !is.na(df$W11_why_substances_none)] <- "Yes"
df$W11_why_substances_valid[is.na(df$W11_why_substances_valid)] <- "No"
table(df$W11_why_substances_valid)

# ----------------------
# Relabel No
# ----------------------
df$W11_why_substances_feel_good[is.na(df$W11_why_substances_feel_good) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_feel_better[is.na(df$W11_why_substances_feel_better) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_connect_socially[is.na(df$W11_why_substances_connect_socially) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_connect_sexually[is.na(df$W11_why_substances_connect_sexually) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_energy[is.na(df$W11_why_substances_energy) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_motivation[is.na(df$W11_why_substances_motivation) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_intense[is.na(df$W11_why_substances_intense) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_last_longer[is.na(df$W11_why_substances_last_longer) & df$W11_why_substances_valid == "Yes"] <- "No" 
df$W11_why_substances_stop_worry[is.na(df$W11_why_substances_stop_worry) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_stress_sexuality[is.na(df$W11_why_substances_stress_sexuality) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_stress_gender[is.na(df$W11_why_substances_stress_gender) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_others_use[is.na(df$W11_why_substances_others_use) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_others_offer[is.na(df$W11_why_substances_others_offer) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_addicted[is.na(df$W11_why_substances_addicted) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_other[is.na(df$W11_why_substances_other) & df$W11_why_substances_valid == "Yes"] <- "No"
df$W11_why_substances_none[is.na(df$W11_why_substances_none) & df$W11_why_substances_valid == "Yes"] <- "No"

# ----------------------
# Missing
# ----------------------
df$W11_why_substances_feel_good[is.na(df$W11_why_substances_feel_good) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_feel_better[is.na(df$W11_why_substances_feel_better) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_connect_socially[is.na(df$W11_why_substances_connect_socially) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_connect_sexually[is.na(df$W11_why_substances_connect_sexually) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_energy[is.na(df$W11_why_substances_energy) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_motivation[is.na(df$W11_why_substances_motivation) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_intense[is.na(df$W11_why_substances_intense) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_last_longer[is.na(df$W11_why_substances_last_longer) & df$W11_why_substances_valid == "No"] <- "9999: True Missing" 
df$W11_why_substances_stop_worry[is.na(df$W11_why_substances_stop_worry) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_stress_sexuality[is.na(df$W11_why_substances_stress_sexuality) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_stress_gender[is.na(df$W11_why_substances_stress_gender) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_others_use[is.na(df$W11_why_substances_others_use) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_others_offer[is.na(df$W11_why_substances_others_offer) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_addicted[is.na(df$W11_why_substances_addicted) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_other[is.na(df$W11_why_substances_other) & df$W11_why_substances_valid == "No"] <- "9999: True Missing"
df$W11_why_substances_none[is.na(df$W11_why_substances_none) & df$W11_why_substances_valid == "No"] <- "9999: True Missing" 


# ----------------------
# Non-Winnipeg
# ----------------------
df$W11_why_substances_feel_good[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_feel_better[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_connect_socially[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_connect_sexually[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_energy[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_motivation[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_intense[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_last_longer[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_stop_worry[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_stress_sexuality[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_stress_gender[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_others_use[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_others_offer[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_addicted[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_other[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W11_why_substances_none[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Internal Conflicts
# ----------------------
df$W11_why_substances_feel_good[df$W11_why_substances_feel_good == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_feel_better[df$W11_why_substances_feel_better == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_connect_socially[df$W11_why_substances_connect_socially == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_connect_sexually[df$W11_why_substances_connect_sexually == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_energy[df$W11_why_substances_energy == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_motivation[df$W11_why_substances_motivation == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_intense[df$W11_why_substances_intense == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality" 
df$W11_why_substances_last_longer[df$W11_why_substances_last_longer == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_stop_worry[df$W11_why_substances_stop_worry == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_stress_sexuality[df$W11_why_substances_stress_sexuality == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_stress_gender[df$W11_why_substances_stress_gender == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_others_use[df$W11_why_substances_others_use == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_others_offer[df$W11_why_substances_others_offer == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_addicted[df$W11_why_substances_addicted == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality" 
df$W11_why_substances_other[df$W11_why_substances_other == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"

df$W11_why_substances_none[df$W11_why_substances_feel_good == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_feel_better == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_connect_socially == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_connect_sexually == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_energy == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_motivation == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_intense == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality" 
df$W11_why_substances_none[df$W11_why_substances_last_longer == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_stop_worry == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_stress_gender == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_stress_sexuality == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_others_use == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_others_offer == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"
df$W11_why_substances_none[df$W11_why_substances_addicted == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality" 
df$W11_why_substances_none[df$W11_why_substances_other == "Yes" & df$W11_why_substances_none == "Yes"] <- "7777: Poor Data Quality"

# ======================================================
# W12 Desire to Reduce Substance Use
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W12_want_quit_reduce_no)
table(df$W12_want_reduce_yes)
table(df$W12_want_quit_yes)
table(df$W13_want_reduce_substance)
table(df$W14_want_quit_substance)

# ----------------------
# Relabel Yes
# ----------------------
df$W12_want_quit_reduce_no[!is.na(df$W12_want_quit_reduce_no)] <- "Yes"
df$W12_want_quit_yes[!is.na(df$W12_want_quit_yes)] <- "Yes"
df$W12_want_reduce_yes[!is.na(df$W12_want_reduce_yes)] <- "Yes"

# ----------------------
# Create Validity Variable
# ----------------------
df$W12_want_valid <- NA
df$W12_want_valid[!is.na(df$W12_want_quit_reduce_no) |
                    !is.na(df$W12_want_reduce_yes) |
                    !is.na(df$W13_want_reduce_substance) |
                    !is.na(df$W14_want_quit_substance) |
                    !is.na(df$W12_want_quit_yes)] <- "Yes"
df$W12_want_valid[is.na(df$W12_want_valid)] <- "No"
table(df$W12_want_valid)

# ----------------------
# Create No
# ----------------------
df$W12_want_quit_reduce_no[df$W12_want_valid == "Yes" & is.na(df$W12_want_quit_reduce_no)] <- "No"
df$W12_want_reduce_yes[df$W12_want_valid == "Yes" & is.na(df$W12_want_reduce_yes)] <- "No"
df$W12_want_quit_yes[df$W12_want_valid == "Yes" & is.na(df$W12_want_quit_yes)] <- "No"
df$W13_want_reduce_substance[df$W12_want_quit_reduce_no == "Yes"] <- "8888: Does Not Want to Reduce/Quit"
df$W14_want_quit_substance[df$W12_want_quit_reduce_no == "Yes"] <- "8888: Does Not Want to Reduce/Quit"

# ----------------------
# Missing
# ----------------------
df$W12_want_quit_reduce_no[is.na(df$W12_want_quit_reduce_no)] <- "9999: True Missing"
df$W12_want_reduce_yes[is.na(df$W12_want_reduce_yes)] <- "9999: True Missing"
df$W12_want_quit_yes[is.na(df$W12_want_quit_yes)] <- "9999: True Missing"
df$W13_want_reduce_substance[is.na(df$W13_want_reduce_substance)] <- "9999: True Missing"
df$W14_want_quit_substance[is.na(df$W14_want_quit_substance)] <- "9999: True Missing"

# ----------------------
# Internal Conflicts
# ----------------------
df$W12_want_quit_reduce_no[df$W12_want_quit_reduce_no == "Yes" & df$W12_want_reduce_yes == "Yes"] <- "7777: Poor Data Quality"
df$W12_want_quit_reduce_no[df$W12_want_quit_reduce_no == "Yes" & df$W12_want_quit_yes == "Yes"] <- "7777: Poor Data Quality"

df$W12_want_reduce_yes[df$W12_want_quit_reduce_no == "Yes" & df$W12_want_reduce_yes == "Yes"] <- "7777: Poor Data Quality"
df$W12_want_quit_yes[df$W12_want_quit_reduce_no == "Yes" & df$W12_want_quit_yes == "Yes"] <- "7777: Poor Data Quality"

df$W13_want_reduce_substance[df$W12_want_quit_reduce_no == "Yes" & !is.na(df$W13_want_reduce_substance)] <- "7777: Poor Data Quality"
df$W14_want_quit_substance[df$W12_want_quit_reduce_no == "Yes" & !is.na(df$W14_want_quit_substance)] <- "7777: Poor Data Quality"

# ----------------------
# Data Quality
# ----------------------
df$W12_want_quit_reduce_no[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W12_want_reduce_yes[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W12_want_quit_yes[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W13_want_reduce_substance[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W14_want_quit_substance[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W15_ever_injected_meth
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W15_ever_injected_meth)

# ----------------------
# Missings
# ----------------------
df$W15_ever_injected_meth[is.na(df$W15_ever_injected_meth)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W15_ever_injected_meth[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "


# ======================================================
# W16_ever_OD
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W16_ever_OD)

# ----------------------
# Missings
# ----------------------
df$W16_ever_OD[is.na(df$W16_ever_OD)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W16_ever_OD[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W17_mental_health
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W17_mental_health)

# ----------------------
# Missings
# ----------------------
df$W17_mental_health[is.na(df$W17_mental_health)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W17_mental_health[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W18 App Use
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W18_apps_squirt)
table(df$W18_apps_manhunt)
table(df$W18_apps_okcupid)
table(df$W18_apps_grindr)
table(df$W18_apps_scruff)
table(df$W18_apps_bbrt)
table(df$W18_apps_gay411)
table(df$W18_apps_growlr)
table(df$W18_apps_hornet)
table(df$W18_apps_tinder)
table(df$W18_apps_facebook)
table(df$W18_apps_snapchat)
table(df$W18_apps_instagram)
table(df$W18_apps_twitter)
table(df$W18_apps_growlr)
table(df$W18_apps_other_text)

# ----------------------
# Create "Yes" Variable
# ----------------------
df$W18_apps_squirt[!is.na(df$W18_apps_squirt)] <- "Yes"
df$W18_apps_manhunt[!is.na(df$W18_apps_manhunt)] <- "Yes"
df$W18_apps_okcupid[!is.na(df$W18_apps_okcupid)] <- "Yes"
df$W18_apps_grindr[!is.na(df$W18_apps_grindr)] <- "Yes"
df$W18_apps_scruff[!is.na(df$W18_apps_scruff)] <- "Yes"
df$W18_apps_bbrt[!is.na(df$W18_apps_bbrt)] <- "Yes"
df$W18_apps_gay411[!is.na(df$W18_apps_gay411)] <- "Yes"
df$W18_apps_growlr[!is.na(df$W18_apps_growlr)] <- "Yes"
df$W18_apps_hornet[!is.na(df$W18_apps_hornet)] <- "Yes"
df$W18_apps_tinder[!is.na(df$W18_apps_tinder)] <- "Yes"
df$W18_apps_facebook[!is.na(df$W18_apps_facebook)] <- "Yes"
df$W18_apps_snapchat[!is.na(df$W18_apps_snapchat)] <- "Yes"
df$W18_apps_instagram[!is.na(df$W18_apps_instagram)] <- "Yes"
df$W18_apps_twitter[!is.na(df$W18_apps_twitter)] <- "Yes"
df$W18_apps_growlr[!is.na(df$W18_apps_growlr)] <- "Yes"
df$W18_apps_other <- NA
df$W18_apps_other[!is.na(df$W18_apps_other_text)] <- "Yes"
df$W18_apps_other[is.na(df$W18_apps_other_text)] <- "No"

# ----------------------
# Validity
# ----------------------
df$W18_apps_valid <- NA

df$W18_apps_valid[!is.na(df$W18_apps_squirt) |
                    !is.na(df$W18_apps_manhunt) |
                    !is.na(df$W18_apps_okcupid) |
                    !is.na(df$W18_apps_grindr) |
                    !is.na(df$W18_apps_scruff) |
                    !is.na(df$W18_apps_bbrt) |
                    !is.na(df$W18_apps_gay411) |
                    !is.na(df$W18_apps_growlr) |
                    !is.na(df$W18_apps_hornet) |
                    !is.na(df$W18_apps_tinder) |
                    !is.na(df$W18_apps_facebook) |
                    !is.na(df$W18_apps_snapchat) |
                    !is.na(df$W18_apps_instagram) |
                    !is.na(df$W18_apps_twitter) |
                    !is.na(df$W18_apps_growlr) |
                    !is.na(df$W18_apps_other)] <- "Yes"
df$W18_apps_valid[is.na(df$W18_apps_valid)] <- "No"
table(df$W18_apps_valid) 

# ----------------------
# Create "No" Variable
# ----------------------
df$W18_apps_squirt[is.na(df$W18_apps_squirt) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_manhunt[is.na(df$W18_apps_manhunt) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_okcupid[is.na(df$W18_apps_okcupid) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_grindr[is.na(df$W18_apps_grindr) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_scruff[is.na(df$W18_apps_scruff) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_bbrt[is.na(df$W18_apps_bbrt) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_gay411[is.na(df$W18_apps_gay411) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_growlr[is.na(df$W18_apps_growlr) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_hornet[is.na(df$W18_apps_hornet) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_tinder[is.na(df$W18_apps_tinder) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_facebook[is.na(df$W18_apps_facebook) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_snapchat[is.na(df$W18_apps_snapchat) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_instagram[is.na(df$W18_apps_instagram) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_twitter[is.na(df$W18_apps_twitter) & df$W18_apps_valid == "Yes"] <- "No"
df$W18_apps_growlr[is.na(df$W18_apps_growlr) & df$W18_apps_valid == "Yes"] <- "No"

# ----------------------
# Create "Missing" Variable
# ----------------------
df$W18_apps_squirt[is.na(df$W18_apps_squirt) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_manhunt[is.na(df$W18_apps_manhunt) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_okcupid[is.na(df$W18_apps_okcupid) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_grindr[is.na(df$W18_apps_grindr) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_scruff[is.na(df$W18_apps_scruff) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_bbrt[is.na(df$W18_apps_bbrt) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_gay411[is.na(df$W18_apps_gay411) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_growlr[is.na(df$W18_apps_growlr) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_hornet[is.na(df$W18_apps_hornet) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_tinder[is.na(df$W18_apps_tinder) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_facebook[is.na(df$W18_apps_facebook) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_snapchat[is.na(df$W18_apps_snapchat) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_instagram[is.na(df$W18_apps_instagram) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_twitter[is.na(df$W18_apps_twitter) & df$W18_apps_valid == "No"] <- "9999: True Missing"
df$W18_apps_growlr[is.na(df$W18_apps_growlr) & df$W18_apps_valid == "No"] <- "9999: True Missing"

df$W18_apps_other_text[df$W18_apps_other == "No"] <- "8888: Other Not Selected"
df$W18_apps_other_text[is.na(df$W18_apps_other_text) & df$W18_apps_other == "Yes"] <- "9999: True Missing"

# ----------------------
# Create "Missing" Variable
# ----------------------
df$W18_apps_squirt[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_manhunt[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_okcupid[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_grindr[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_scruff[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_bbrt[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_gay411[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_growlr[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_hornet[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_tinder[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_facebook[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_snapchat[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_instagram[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_twitter[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_growlr[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W18_apps_other[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Correct for Nones
# ----------------------
df$W18_apps_squirt[df$W18_apps_squirt == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_manhunt[df$W18_apps_manhunt == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_okcupid[df$W18_apps_okcupid == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_grindr[df$W18_apps_grindr == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_scruff[df$W18_apps_scruff == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_bbrt[df$W18_apps_bbrt == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_gay411[df$W18_apps_gay411 == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_growlr[df$W18_apps_growlr == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_hornet[df$W18_apps_hornet == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_tinder[df$W18_apps_tinder == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_facebook[df$W18_apps_facebook == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_snapchat[df$W18_apps_snapchat == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_instagram[df$W18_apps_instagram == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_twitter[df$W18_apps_twitter == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_growlr[df$W18_apps_growlr == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"
df$W18_apps_other[df$W18_apps_other == "9999: True Missing" & df$W17_mental_health != "9999: True Missing"] <- "No"

# ======================================================
# W19 Connection to Community
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W19_connected_LGTBQ2Splus)
table(df$W19_connected_GBQ_men)

# ----------------------
# Missings
# ----------------------
df$W19_connected_LGTBQ2Splus[is.na(df$W19_connected_LGTBQ2Splus)] <- "9999: True Missing"
df$W19_connected_GBQ_men[is.na(df$W19_connected_GBQ_men)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W19_connected_LGTBQ2Splus[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W19_connected_GBQ_men[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "


# ======================================================
# W20 General Health
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W20_general_health)

# ----------------------
# Missings
# ----------------------
df$W20_general_health[is.na(df$W20_general_health)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W20_general_health[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W21_where_usual_care
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W21_where_usual_care)
table(df$W21_where_usual_care_other_text)

# ----------------------
# Data Quality
# ----------------------
df$W21_where_usual_care[df$W21_where_usual_care == "Multiple answers selected"] <- "7777: Poor Data Quality"
df$W21_where_usual_care[df$W21_where_usual_care == "Other, please specify"] <- "Other"

# ----------------------
# Missings
# ----------------------
df$W21_where_usual_care[is.na(df$W21_where_usual_care)] <- "9999: True Missing"
df$W21_where_usual_care_other_text[df$W21_where_usual_care != "Other"] <- "8888: Other Not Selected"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W21_where_usual_care[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W21_where_usual_care_other_text[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W22 Health Coverage
# ======================================================
table(df$W22_extended_health_medication)
table(df$W22_extended_health_counselling)
table(df$W22_extended_health_other)
table(df$W22_extended_health_none)

# ----------------------
# Relabel Yes
# ----------------------
df$W22_extended_health_medication[!is.na(df$W22_extended_health_medication)] <- "Yes"
df$W22_extended_health_counselling[!is.na(df$W22_extended_health_counselling)] <- "Yes"
df$W22_extended_health_other[!is.na(df$W22_extended_health_other)] <- "Yes"
df$W22_extended_health_none[!is.na(df$W22_extended_health_none)] <- "Yes"

# ----------------------
# Create Validity Variable
# ----------------------
df$W22_extended_health_valid <- NA
df$W22_extended_health_valid[!is.na(df$W22_extended_health_medication) |
                               !is.na(df$W22_extended_health_counselling) |
                               !is.na(df$W22_extended_health_other) |
                               !is.na(df$W22_extended_health_none)] <- "Yes"
df$W22_extended_health_valid[is.na(df$W22_extended_health_valid)] <- "No"
table(df$W22_extended_health_valid)

# ----------------------
# Create No
# ----------------------
df$W22_extended_health_medication[df$W22_extended_health_valid == "Yes" & is.na(df$W22_extended_health_medication)] <- "No"
df$W22_extended_health_counselling[df$W22_extended_health_valid == "Yes" & is.na(df$W22_extended_health_counselling)] <- "No"
df$W22_extended_health_other[df$W22_extended_health_valid == "Yes" & is.na(df$W22_extended_health_other)] <- "No"
df$W22_extended_health_none[df$W22_extended_health_valid == "Yes" & is.na(df$W22_extended_health_none)] <- "No"

# ----------------------
# Missing
# ----------------------
df$W22_extended_health_medication[is.na(df$W22_extended_health_medication)] <- "9999: True Missing"
df$W22_extended_health_counselling[is.na(df$W22_extended_health_counselling)] <- "9999: True Missing"
df$W22_extended_health_other[is.na(df$W22_extended_health_other)] <- "9999: True Missing"
df$W22_extended_health_none[is.na(df$W22_extended_health_none)] <- "9999: True Missing"

# ----------------------
# Internal Conflicts
# ----------------------
df$W22_extended_health_medication[df$W22_extended_health_none == "Yes" & df$W22_extended_health_medication == "Yes"] <- "7777: Poor Data Quality"
df$W22_extended_health_counselling[df$W22_extended_health_none == "Yes" & df$W22_extended_health_counselling == "Yes"] <- "7777: Poor Data Quality"
df$W22_extended_health_other[df$W22_extended_health_none == "Yes" & df$W22_extended_health_other == "Yes"] <- "7777: Poor Data Quality"

df$W22_extended_health_none[df$W22_extended_health_none == "Yes" & df$W22_extended_health_medication == "Yes"] <- "7777: Poor Data Quality"
df$W22_extended_health_none[df$W22_extended_health_none == "Yes" & df$W22_extended_health_counselling == "Yes"] <- "7777: Poor Data Quality"
df$W22_extended_health_none[df$W22_extended_health_none == "Yes" & df$W22_extended_health_other == "Yes"] <- "7777: Poor Data Quality"


# ----------------------
# Data Quality
# ----------------------
df$W22_extended_health_medication[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W22_extended_health_counselling[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W22_extended_health_other[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W22_extended_health_none[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W23_health_provider_negative
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W23_health_provider_negative)

# ----------------------
# Data Quality
# ----------------------
df$W23_health_provider_negative[df$W23_health_provider_negative == "Multiple answers selected"] <- "7777: Poor Data Quality"

# ----------------------
# Missings
# ----------------------
df$W23_health_provider_negative[is.na(df$W23_health_provider_negative)] <- "9999: True Missing"

# ----------------------
# Recode Missing Non-Winnipeg Results
# ----------------------
df$W23_health_provider_negative[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ======================================================
# W24 Favourite things about Pride
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$W24_fave_pride_meet_people)
table(df$W24_fave_pride_old_friends)
table(df$W24_fave_pride_community)
table(df$W24_fave_pride_support)
table(df$W24_fave_pride_political)
table(df$W24_fave_pride_let_loose)
table(df$W24_fave_pride_freaky)

# ----------------------
# Yes Variable
# ----------------------
df$W24_fave_pride_meet_people[!is.na(df$W24_fave_pride_meet_people)] <- "Yes"
df$W24_fave_pride_old_friends[!is.na(df$W24_fave_pride_old_friends)] <- "Yes"
df$W24_fave_pride_community[!is.na(df$W24_fave_pride_community)] <- "Yes"
df$W24_fave_pride_support[!is.na(df$W24_fave_pride_support)] <- "Yes"
df$W24_fave_pride_political[!is.na(df$W24_fave_pride_political)] <- "Yes"
df$W24_fave_pride_let_loose[!is.na(df$W24_fave_pride_let_loose)] <- "Yes"
df$W24_fave_pride_freaky[!is.na(df$W24_fave_pride_freaky)] <- "Yes"

# ----------------------
# Create Validity Variable
# ----------------------
df$W24_fave_pride_valid <- NA
df$W24_fave_pride_valid[!is.na(df$W24_fave_pride_meet_people) |
                          !is.na(df$W24_fave_pride_old_friends) |
                          !is.na(df$W24_fave_pride_community) |
                          !is.na(df$W24_fave_pride_support) |
                          !is.na(df$W24_fave_pride_political) |
                          !is.na(df$W24_fave_pride_let_loose) |
                          !is.na(df$W24_fave_pride_freaky)] <- "Yes"
df$W24_fave_pride_valid[is.na(df$W24_fave_pride_valid)] <- "No"
table(df$W24_fave_pride_valid)

# ----------------------
# Create No Variable
# ----------------------
df$W24_fave_pride_meet_people[is.na(df$W24_fave_pride_meet_people) & df$W24_fave_pride_valid == "Yes"] <- "No"
df$W24_fave_pride_old_friends[is.na(df$W24_fave_pride_old_friends) & df$W24_fave_pride_valid == "Yes"] <- "No"
df$W24_fave_pride_community[is.na(df$W24_fave_pride_community) & df$W24_fave_pride_valid == "Yes"] <- "No"
df$W24_fave_pride_support[is.na(df$W24_fave_pride_support) & df$W24_fave_pride_valid == "Yes"] <- "No"
df$W24_fave_pride_political[is.na(df$W24_fave_pride_political) & df$W24_fave_pride_valid == "Yes"] <- "No"
df$W24_fave_pride_let_loose[is.na(df$W24_fave_pride_let_loose) & df$W24_fave_pride_valid == "Yes"] <- "No"
df$W24_fave_pride_freaky[is.na(df$W24_fave_pride_freaky) & df$W24_fave_pride_valid == "Yes"] <- "No"

# ----------------------
# Create Missing Variable
# ----------------------
df$W24_fave_pride_meet_people[is.na(df$W24_fave_pride_meet_people) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"
df$W24_fave_pride_old_friends[is.na(df$W24_fave_pride_old_friends) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"
df$W24_fave_pride_community[is.na(df$W24_fave_pride_community) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"
df$W24_fave_pride_support[is.na(df$W24_fave_pride_support) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"
df$W24_fave_pride_political[is.na(df$W24_fave_pride_political) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"
df$W24_fave_pride_let_loose[is.na(df$W24_fave_pride_let_loose) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"
df$W24_fave_pride_freaky[is.na(df$W24_fave_pride_freaky) & df$W24_fave_pride_valid == "No"] <- "9999: True Missing"

# ----------------------
# Yes Variable
# ----------------------
df$W24_fave_pride_meet_people[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_old_friends[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_community[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_support[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_political[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_let_loose[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_freaky[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Yes Variable
# ----------------------
df$W24_fave_pride_meet_people[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_old_friends[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_community[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_support[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_political[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_let_loose[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "
df$W24_fave_pride_freaky[df$Q0_site != "Winnipeg"] <- "8888: Non-Winnipeg "

# ----------------------
# Correct No's due to exclusion of None variable
# ----------------------
df$W24_fave_pride_meet_people[df$W24_fave_pride_meet_people == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"
df$W24_fave_pride_old_friends[df$W24_fave_pride_old_friends == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"
df$W24_fave_pride_community[df$W24_fave_pride_community == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"
df$W24_fave_pride_support[df$W24_fave_pride_support == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"
df$W24_fave_pride_political[df$W24_fave_pride_political == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"
df$W24_fave_pride_let_loose[df$W24_fave_pride_let_loose == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"
df$W24_fave_pride_freaky[df$W24_fave_pride_freaky == "9999: True Missing" & df$W23_health_provider_negative != "9999: True Missing"] <- "No"

# ======================================================
# lab_HCV_final
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_HIV_final)
# ----------------------
# Missing
# ----------------------
df$lab_HCV_final[is.na(df$lab_HCV_final)] <- "8888: No DBS"

# ======================================================
# lab_HCV_quant
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_HCV_quant)

# ----------------------
# Skip Logic
# ----------------------
df$lab_HCV_quant[df$lab_HCV_quant == "Insufficient Quantity"] <- "8888: Insufficient Quantity"
df$lab_HCV_quant[df$lab_HCV_final == "Insufficient Quantity"] <- "8888: Insufficient Quantity"
df$lab_HCV_quant[df$lab_HCV_final == "HCV Ab Non-reactive"] <- "8888: HCV Ab Non-reactive"
df$lab_HCV_quant[df$lab_HCV_final == "Insufficient Quality"] <- "8888: Insufficient Quantity"

# ----------------------
# Missing
# ----------------------
df$lab_HCV_quant[is.na(df$lab_HCV_quant)] <- "8888: No DBS"

# ======================================================
# lab_HIV_combo
# ======================================================

# ----------------------
# Explore Data
# ----------------------
table(df$lab_HIV_combo)

# ----------------------
# Missing
# ----------------------
df$lab_HIV_combo[is.na(df$lab_HIV_combo)] <- "8888: No Labs"

# ======================================================
# lab_HIV_final
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_HIV_final)

# ----------------------
# Missing
# ----------------------
df$lab_HIV_final[is.na(df$lab_HIV_final)] <- "8888: No DBS"

# ======================================================
# lab_orth_HCV
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_orth_HCV)

# ----------------------
# Missing
# ----------------------
df$lab_orth_HCV[is.na(df$lab_orth_HCV)] <- "8888: No DBS"

# ======================================================
# Q0_Has_DBS_LABS
# ======================================================
df$Q0_has_dbs_labs <- NA
df$Q0_has_dbs_labs[df$lab_orth_HCV != "8888: No DBS"] <- "Yes"
df$Q0_has_dbs_labs[df$lab_orth_HCV == "8888: No DBS"] <- "No"

# ======================================================
# lab_notes
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_notes)

# ----------------------
# Missing
# ----------------------
df$lab_notes[df$lab_orth_HCV == "8888: No DBS"] <- "8888: No DBS"
df$lab_notes[is.na(df$lab_notes)] <- "No Notes"

# ======================================================
# lab_HIV_screen
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_HIV_screen)
df$lab_HIV_screen[is.na(df$lab_HIV_screen) & df$Q0_has_dbs_labs == "Yes"] <- "Non-reactive"
df$lab_HIV_screen[is.na(df$lab_HIV_screen) & df$Q0_has_dbs_labs == "No"] <- "8888: No DBS"

# ======================================================
# lab_HIV_confirmatory_test
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_HIV_confirmatory_test)
table(df$lab_HIV_screen)

df$lab_HIV_confirmatory_test[is.na(df$lab_HIV_confirmatory_test) & df$Q0_has_dbs_labs == "No"] <- "8888: No DBS"
df$lab_HIV_confirmatory_test[df$lab_HIV_screen == "Non-reactive"] <- "8888: Screen was Non-reactive"
df$lab_HIV_confirmatory_test[df$lab_HIV_confirmatory_test == "Insufficient quantity"] <- "8888: Insufficient quantity"

# ======================================================
# lab_HIV_viral_load
# ======================================================
# ----------------------
# Explore Data
# ----------------------
table(df$lab_HIV_viral_load)

df$lab_HIV_viral_load[is.na(df$lab_HIV_viral_load) & df$Q0_has_dbs_labs == "No"] <- "8888: No DBS"
df$lab_HIV_viral_load[df$lab_HIV_screen == "Non-reactive"] <- "8888: Screen was Non-reactive"
df$lab_HIV_viral_load[df$lab_HIV_confirmatory_test == "Negative"] <- "8888: Confirmatory Test was Negative"


# ======================================================
# Drop Identifiers and other excluded variables
# ======================================================
########### Remember to Drop all "Valid" indicator  variables ###########
uncoded_df <- filter(.data = uncoded_df, Q0_eligible == "Yes")

undesired <- c(
  'Q14_FSA',
  'Q73_abuse_valid',
  'Q25_delays_valid',
  'Q64_social_valid',
  'Q71_denied_valid',
  'Q24_STI_test_valid',
  'Q26_STI_type_valid',
  'Q2_ethnicity_valid',
  'Q30_BD_aware_valid',
  'Q58_want_help_valid',
  'Q21_recent_sex_valid',
  'Q61_substances_valid',
  'Q2_sex_orientation_valid',
  'Q72_discrimination_valid',
  'Q79_sex_w_men_anal_valid',
  'Q29_self_test_blood_valid',
  'Q59_mental_health_pro_valid',
  'Q21_recent_sex_partner_valid',
  'Q52_PrEP_not_interested_valid',
  'Q63_substance_services_valid',
  'Q80_HIV_prevention_valid',
  'W4_married_valid',
  'W2_married_valid',
  'Q81_enjoy_most_valid',
  'W24_fave_pride_valid',
  'W22_extended_health_valid',
  'W18_apps_valid', 
  'W12_want_valid', 
  'W11_why_substances_valid', 
  'W10_when_test_HIV_valid', 
  'W9_delay_HIV_valid', 
  'W6_STI_test_valid',
  'Q61_substances_none',
  'Q0_surveymonkey_ID', 
  'Q0_collector_ID', 
  'Q83_final_comments',
  'passport_best_time',
  'passport_email',
  'passport_sms',
  'passport_call',
  'passport_contact1',
  'passport_contact2',
  'passport_voicemail',
  'Q53_HIRI_1',
  'Q53_HIRI_2',
  'Q53_HIRI_3',
  'Q53_HIRI_4',
  'Q53_HIRI_5',
  'Q53_HIRI_6',
  'Q53_HIRI_7',
  'passport_comments',
  'passport_destroy_sample',
  'passport_password',
  'lab_notes',
  'lab_HIV_combo',
  'lab_HIV_final',
  'Q2_ethnicity_other_text',
  'Q7_sex_orientation_other_text',
  'Q9_gender_ID_other_text',
  'Q21_recent_sex_other_text',
  'Q25_delays_other_text',
  'Q26_STI_other_text',
  'Q52_PrEP_not_interested_other_text',
  'Q54_PrEP_how_using_other_text',
  'Q58_want_help_other_text',
  'Q61_substances_other_text',
  'Q63_substance_services_other_text',
  'Q81_enjoy_most_other_text',
  'W9_delay_HIV_other_text',
  'W11_why_substances_other_text',
  'W18_apps_other_text',
  'W21_where_usual_care_other_text',
  'Q57_depression_1',
  'Q57_depression_2',
  'Q57_anxiety_1',
  'Q57_anxiety_2')

df <- df %>%
  select(-one_of(undesired))

uncoded_df <- uncoded_df %>%
  select(-one_of(undesired))

# ======================================================
# Prepare Frequencies
# ======================================================
dictionary <- read.csv(file = "\\2018 Sex Now Data Cleaning\\Questions.csv")
options(max.print = 10000)
Table <- CreateTableOne(data = df)
Frequencies <- print(Table, showAllLevels = TRUE, missing = FALSE, printToggle = FALSE, cramVars = TRUE, varLabels = TRUE, explain = FALSE) %>%
  as_tibble(rownames = NA) %>%
  mutate(var = rownames(.),
         var = na_if(var, "")) %>%
  fill(var) %>%
  full_join(x = dictionary, y = ., by = c("Variables" = "var")) 

df <- as.data.frame(df)
Table <- as.data.frame(Frequencies)
colnames(Frequencies) <- c("Variable", "Question", "Notes", "Responses", "N (%)")
Frequencies <- select(Frequencies, "Variable", "Question", "Responses", "N (%)", "Notes") 

write_csv(x = Frequencies, path = "\\SexNow2018\\SN2018_Data Dictionary.csv")

# ======================================================
# Write Data File
# ======================================================
write.csv(x = df, "\\SexNow2018\\SN2018.csv")

# ======================================================
# Crosstab uncoded_df with df
# ======================================================

library(writexl)
edited_vars <- c("Q0_has_survey",
                 "Q0_has_dbs_labs",
                 "Q0_eligible",
                 "Q0_site",
                 "Q1_province",
                 "Q0_language",
                 "Q2_ethnicity_african",
                 "Q2_ethnicity_arab",
                 "Q2_ethnicity_asian",
                 "Q2_ethnicity_indigenous",
                 "Q2_ethnicity_latin",
                 "Q2_ethnicity_south_asian",
                 "Q2_ethnicity_white",
                 "Q2_ethnicity_other",
                 "Q2_ethnicity_other_text",
                 "Q3_indigenous",
                 "Q4_two_spirit",
                 "Q5_indigenous_community",
                 "Q6_indigenous_status",
                 "Q7_sex_orientation_gay",
                 "Q7_sex_orientation_asexual",
                 "Q7_sex_orientation_straight",
                 "Q7_sex_orientation_bisexual",
                 "Q7_sex_orientation_pansexual",
                 "Q7_sex_orientation_queer",
                 "Q7_sex_orientation_heteroflexible",
                 "Q7_sex_orientation_other",
                 "Q7_sex_orientation_other_text",
                 "Q8_out",
                 "Q9_gender_ID",
                 "Q9_gender_ID_other_text",
                 "Q10_trans",
                 "Q11_age",
                 "Q12_education",
                 "Q13_born_Canada",
                 "Q14_FSA",
                 "Q15_money",
                 "Q16_relationship",
                 "Q16_poly_num_men",
                 "Q16_poly_num_women",
                 "Q16_poly_num_non_bin",
                 "Q17_relationship_exclusive",
                 "Q18_sex_with_women",
                 "Q19_num_sex_partners_6_months",
                 "Q19_num_sex_partners_3_months",
                 "Q20_newest_partner_when",
                 "Q21_recent_sex_mutual_mast",
                 "Q21_recent_sex_oral",
                 "Q21_recent_sex_fingering",
                 "Q21_recent_sex_fisting",
                 "Q21_recent_sex_anal_bottom_condom",
                 "Q21_recent_sex_anal_bottom_no_condom",
                 "Q21_recent_sex_anal_top_condom",
                 "Q21_recent_sex_anal_top_no_condom",
                 "Q21_recent_sex_resp_vagina_condom",
                 "Q21_recent_sex_resp_vagina_no_condom",
                 "Q21_recent_sex_partner_vagina_condom",
                 "Q21_recent_sex_partner_vagina_no_condom",
                 "Q21_recent_sex_toys",
                 "Q21_recent_sex_online",
                 "Q21_recent_sex_threesome",
                 "Q21_recent_sex_group",
                 "Q21_recent_sex_none",
                 "Q21_recent_sex_other",
                 "Q21_recent_sex_other_text",
                 "Q22_recent_sex_partner_bought_sex",
                 "Q22_recent_sex_partner_sold_sex",
                 "Q22_recent_sex_partner_trans_man",
                 "Q22_recent_sex_partner_trans_woman",
                 "Q22_recent_sex_partner_non_binary",
                 "Q22_recent_sex_partner_none",
                 "Q23_STI_test_when",
                 "Q24_STI_test_urine",
                 "Q24_STI_test_blood",
                 "Q24_STI_test_throat",
                 "Q24_STI_test_rectal",
                 "Q24_STI_test_none",
                 "Q25_delays_busy",
                 "Q25_delays_distance",
                 "Q25_delays_hours",
                 "Q25_delays_privacy",
                 "Q25_delays_sensitivity",
                 "Q25_delays_stressed",
                 "Q25_delays_cost",
                 "Q25_delays_wait",
                 "Q25_delays_language",
                 "Q25_delays_none",
                 "Q25_delays_other",
                 "Q25_delays_other_text",
                 "Q26_STI_syphilis",
                 "Q26_STI_chlamydia",
                 "Q26_STI_gonorrhea",
                 "Q26_STI_warts",
                 "Q26_STI_herpes",
                 "Q26_STI_urethritis",
                 "Q26_STI_LGV_WPG_only",
                 "Q26_STI_crabs_WPG_only",
                 "Q26_STI_none",
                 "Q26_STI_other",
                 "Q26_STI_other_text",
                 "Q27_STI_bum",
                 "Q28_self_test_likely",
                 "Q29_self_test_blood",
                 "Q29_self_test_pee",
                 "Q29_self_test_throat",
                 "Q29_self_test_bum",
                 "Q29_self_test_none_WPG_excluded",
                 "Q30_BD_aware_HIV_tests",
                 "Q30_BD_aware_window_period_risk",
                 "Q30_BD_aware_MSM_risk",
                 "Q30_BD_aware_MSM_deferral",
                 "Q30_BD_aware_trans",
                 "Q30_BD_aware_none",
                 "Q31_BD_opinion_justified",
                 "Q31_BD_opinion_discriminatory",
                 "Q31_BD_opinion_short_deferral",
                 "Q31_BD_opinion_num_partners",
                 "Q31_BD_opinion_new_partners",
                 "Q31_BD_opinion_sex_practices",
                 "Q31_BD_opinion_would_donate",
                 "Q32_know_condoms",
                 "Q32_know_PrEP",
                 "Q32_know_PEP",
                 "Q32_know_viral_load",
                 "Q32_know_undetectable",
                 "Q32_know_anal_risk_WPG_only",
                 "Q32_know_TasP_WPG_only",
                 "Q32_know_rapid_test_WPG_only",
                 "Q33_HCV_test_when",
                 "Q34_HCV_test_result",
                 "Q35_HCV_ever_diagnosed",
                 "Q36_HCV_first_diagnosed",
                 "Q37_HCV_last_dr_visit",
                 "Q38_HCV_ever_treatment",
                 "Q39_HCV_last_started_treatment",
                 "Q40_HCV_treatment_success",
                 "Q41_HIV_test_when",
                 "Q42_HIV_test_result",
                 "Q43_HIV_ever_diagnosed",
                 "Q44_HIV_first_diagnosed",
                 "Q45_HIV_current_risk_giving",
                 "Q46_HIV_last_dr_visit",
                 "Q47_HIV_current_ARV",
                 "Q47_HIV_ever_ARV",
                 "Q47_HIV_last_missed_ARV",
                 "Q48_HIV_viral_load",
                 "Q49_HIV_current_risk_getting",
                 "Q50_PrEP_ever_used",
                 "Q50_PrEP_ever_used_why_stopped",
                 "Q51_PrEP_interest",
                 "Q52_PrEP_not_interested_risk",
                 "Q52_PrEP_not_interested_cost",
                 "Q52_PrEP_not_interested_prescription",
                 "Q52_PrEP_not_interested_side_effects",
                 "Q52_PrEP_not_interested_pills",
                 "Q52_PrEP_not_interested_testing",
                 "Q52_PrEP_not_interested_judgement_community",
                 "Q52_PrEP_not_interested_judgement_healthcare",
                 "Q52_PrEP_not_interested_HIV_only",
                 "Q52_PrEP_not_interested_none",
                 "Q52_PrEP_not_interested_other",
                 "Q52_PrEP_not_interested_other_text",
                 "Q53_HIRI_age",
                 "Q53_HIRI_num_partners",
                 "Q53_HIRI_bottom_no_condom",
                 "Q53_HIRI_HIV_pos_partners",
                 "Q53_HIRI_top_no_condom",
                 "Q53_HIRI_meth",
                 "Q53_HIRI_poppers",
                 "Q53_HIRI_total",
                 "Q53_HIRI_total_calculated",
                 "Q54_PrEP_how_using",
                 "Q54_PrEP_how_using_other_text",
                 "Q55_PrEP_months_used",
                 "Q55_PrEP_years_used",
                 "Q56_PrEP_days_taken",
                 "Q57_depression_little_interest",
                 "Q57_depression_feeling_down",
                 "Q57_depression_feeling_nervous",
                 "Q57_depression_uncontrolled_worrying",
                 "Q57_depression",
                 "Q57_anxiety",
                 "Q58_want_help_depression",
                 "Q58_want_help_anxiety",
                 "Q58_want_help_coming_out",
                 "Q58_want_help_gender",
                 "Q58_want_help_eating_disorder",
                 "Q58_want_help_body_image",
                 "Q58_want_help_relationship_problems",
                 "Q58_want_help_suicidal_thoughts",
                 "Q58_want_help_none",
                 "Q58_want_help_other",
                 "Q58_want_help_other_text",
                 "Q59_mental_health_pro_elder",
                 "Q59_mental_health_pro_psychiatrist",
                 "Q59_mental_health_pro_counsellor",
                 "Q59_mental_health_pro_social_worker",
                 "Q59_mental_health_pro_knowledge_keeper",
                 "Q59_mental_health_pro_psychologist",
                 "Q59_mental_health_pro_peer",
                 "Q59_mental_health_pro_sex_therapist",
                 "Q59_mental_health_pro_none",
                 "Q60_substances_any",
                 "Q61_substances_alcohol",
                 "Q61_substances_alcohol_sex",
                 "Q61_substances_tobacco",
                 "Q61_substances_tobacco_sex",
                 "Q61_substances_marijuana",
                 "Q61_substances_marijuana_sex",
                 "Q61_substances_poppers",
                 "Q61_substances_poppers_sex",
                 "Q61_substances_ketamine",
                 "Q61_substances_ketamine_sex",
                 "Q61_substances_ecstasy",
                 "Q61_substances_ecstasy_sex",
                 "Q61_substances_crystal",
                 "Q61_substances_crystal_sex",
                 "Q61_substances_erection",
                 "Q61_substances_erection_sex",
                 "Q61_substances_crack",
                 "Q61_substances_crack_sex",
                 "Q61_substances_cocaine",
                 "Q61_substances_cocaine_sex",
                 "Q61_substances_heroin",
                 "Q61_substances_heroin_sex",
                 "Q61_substances_other_opioids",
                 "Q61_substances_other_opioids_sex",
                 "Q61_substances_fentanyl",
                 "Q61_substances_fentanyl_sex",
                 "Q61_substances_GHB",
                 "Q61_substances_GHB_sex",
                 "Q61_substances_benzos",
                 "Q61_substances_benzos_sex",
                 "Q61_substances_psychedelics",
                 "Q61_substances_psychedelics_sex",
                 "Q61_substances_steroids",
                 "Q61_substances_steroids_sex",
                 "Q61_substances_other",
                 "Q61_substances_other_sex",
                 "Q61_substances_other_text",
                 "Q62_ever_injected_drugs",
                 "Q63_substance_services_needle_exchange",
                 "Q63_substance_services_harm_reduction",
                 "Q63_substance_services_supervised_injection",
                 "Q63_substance_services_Naloxone",
                 "Q63_substance_services_detox",
                 "Q63_substance_services_sweat_lodge",
                 "Q63_substance_services_NA",
                 "Q63_substance_services_AA",
                 "Q63_substance_services_none",
                 "Q63_substance_services_other",
                 "Q63_substance_services_other_text",
                 "Q63_substance_services_Naloxone_on_me",
                 "Q63_substance_services_Naloxone_someone_else",
                 "Q64_social_volunteering",
                 "Q64_social_activism",
                 "Q64_social_sports",
                 "Q64_social_HIV",
                 "Q64_social_civic",
                 "Q64_social_political",
                 "Q64_social_pop_ups",
                 "Q64_social_ethnoracial",
                 "Q64_social_none",
                 "Q65_support",
                 "Q66_satisfied_community",
                 "Q66_satisfied_men",
                 "Q66_satisfied_physical_spaces",
                 "Q66_satisfied_online_spaces",
                 "Q67_has_doctor",
                 "Q68_doctor_knows",
                 "Q69_vaccinated_HepB",
                 "Q69_vaccinated_HPV",
                 "Q69_vaccinated_HepA_WPG_only",
                 "Q69_vaccinated_flu_WPG_only",
                 "Q70_testicular_cancer_check",
                 "Q71_denied_HIV_test",
                 "Q71_denied_PEP",
                 "Q71_denied_PrEP",
                 "Q71_denied_HPV_vaccine",
                 "Q71_denied_hormone_therapy",
                 "Q71_denied_gender_surgery",
                 "Q71_denied_none",
                 "Q72_discrimination_age",
                 "Q72_discrimination_age_by_MSM",
                 "Q72_discrimination_HIV",
                 "Q72_discrimination_HIV_by_MSM",
                 "Q72_discrimination_PrEP",
                 "Q72_discrimination_PrEP_by_MSM",
                 "Q72_discrimination_race",
                 "Q72_discrimination_race_by_MSM",
                 "Q72_discrimination_body",
                 "Q72_discrimination_body_by_MSM",
                 "Q72_discrimination_gender",
                 "Q72_discrimination_gender_by_MSM",
                 "Q72_discrimination_sex_orient",
                 "Q72_discrimination_sex_orient_by_MSM",
                 "Q72_discrimination_trans",
                 "Q72_discrimination_trans_by_MSM",
                 "Q72_discrimination_disability",
                 "Q72_discrimination_disability_by_MSM",
                 "Q73_abuse_verbal",
                 "Q73_abuse_physical",
                 "Q73_abuse_sexual",
                 "Q73_abuse_prefer_not_to_say_WPG_only",
                 "Q74_forced_sex",
                 "Q75_correctional_facility",
                 "Q76_sex_w_men_ever",
                 "Q76_sex_w_men_first_time_age",
                 "Q77_sex_w_men_number",
                 "Q77_sex_w_men_number_anal",
                 "Q78_sex_w_men_anal_position",
                 "Q79_sex_w_men_anal_PrEP",
                 "Q79_sex_w_men_anal_undetectable",
                 "Q79_sex_w_men_anal_unknown_HIV_status",
                 "Q79_sex_w_men_anal_opposite_HIV_status",
                 "Q79_sex_w_men_anal_age",
                 "Q79_sex_w_men_anal_race",
                 "Q79_sex_w_men_anal_language",
                 "Q79_sex_w_men_anal_ONS",
                 "Q79_sex_w_men_anal_regular",
                 "Q79_sex_w_men_anal_none",
                 "Q80_HIV_prevention_condoms",
                 "Q80_HIV_prevention_bottom",
                 "Q80_HIV_prevention_top",
                 "Q80_HIV_prevention_same_HIV_status",
                 "Q80_HIV_prevention_partner_on_PrEP",
                 "Q80_HIV_prevention_undetectable",
                 "Q80_HIV_prevention_no_anal",
                 "Q80_HIV_prevention_PEP",
                 "Q80_HIV_prevention_PrEP",
                 "Q80_HIV_prevention_ask_status",
                 "Q80_HIV_prevention_monogamy",
                 "Q80_HIV_prevention_none",
                 "Q81_enjoy_most_bottoming",
                 "Q81_enjoy_most_flip_fucking",
                 "Q81_enjoy_most_topping",
                 "Q81_enjoy_most_giving_head",
                 "Q81_enjoy_most_getting_head",
                 "Q81_enjoy_most_getting_rimmed",
                 "Q81_enjoy_most_rimming",
                 "Q81_enjoy_most_no_sex",
                 "Q81_enjoy_most_other",
                 "Q81_enjoy_most_other_text",
                 "Q82_role_model",
                 "W1_environment",
                 "W2_married_never",
                 "W2_married_not_yet",
                 "W2_married_yes_now",
                 "W2_married_yes_previous",
                 "W3_married_to",
                 "W4_past_married_to_men",
                 "W4_past_married_to_women",
                 "W4_past_married_to_non_binary",
                 "W5_new_sex_partner_6_months",
                 "W5_new_sex_partner_3_months",
                 "W6_STI_test_at_clinic",
                 "W6_STI_test_substance_use",
                 "W6_STI_test_offered_HIV_test",
                 "W6_STI_test_offered_rapid_HIV_test",
                 "W6_STI_test_none",
                 "W7_amount_willing_to_pay",
                 "W8_guess_perc_living_w_HIV",
                 "W8_guess_perc_using_PrEP",
                 "W8_guess_perc_using_condoms",
                 "W8_guess_perc_undetectable",
                 "W9_delay_HIV_none",
                 "W9_delay_HIV_busy",
                 "W9_delay_HIV_stressed",
                 "W9_delay_HIV_distance",
                 "W9_delay_HIV_hours",
                 "W9_delay_HIV_privacy",
                 "W9_delay_HIV_wait",
                 "W9_delay_HIV_language",
                 "W9_delay_HIV_sensitivity",
                 "W9_delay_HIV_cost",
                 "W9_delay_HIV_other",
                 "W9_delay_HIV_other_text",
                 "W10_when_test_HIV_never",
                 "W10_when_test_HIV_3_months",
                 "W10_when_test_HIV_twice_a_year",
                 "W10_when_test_HIV_once_a_year",
                 "W10_when_test_HIV_few_years",
                 "W10_when_test_HIV_before_new_partner",
                 "W10_when_test_HIV_after_new_partner",
                 "W10_when_test_HIV_after_risky_sex",
                 "W11_why_substances_feel_good",
                 "W11_why_substances_feel_better",
                 "W11_why_substances_connect_socially",
                 "W11_why_substances_connect_sexually",
                 "W11_why_substances_energy",
                 "W11_why_substances_motivation",
                 "W11_why_substances_intense",
                 "W11_why_substances_last_longer",
                 "W11_why_substances_stop_worry",
                 "W11_why_substances_stress_sexuality",
                 "W11_why_substances_stress_gender",
                 "W11_why_substances_others_use",
                 "W11_why_substances_others_offer",
                 "W11_why_substances_addicted",
                 "W11_why_substances_none",
                 "W11_why_substances_other",
                 "W11_why_substances_other_text",
                 "W12_want_quit_reduce_no",
                 "W12_want_reduce_yes",
                 "W12_want_quit_yes",
                 "W13_want_reduce_substance",
                 "W14_want_quit_substance",
                 "W15_ever_injected_meth",
                 "W16_ever_OD",
                 "W17_mental_health",
                 "W18_apps_squirt",
                 "W18_apps_manhunt",
                 "W18_apps_okcupid",
                 "W18_apps_grindr",
                 "W18_apps_scruff",
                 "W18_apps_bbrt",
                 "W18_apps_gay411",
                 "W18_apps_growlr",
                 "W18_apps_hornet",
                 "W18_apps_tinder",
                 "W18_apps_facebook",
                 "W18_apps_snapchat",
                 "W18_apps_instagram",
                 "W18_apps_twitter",
                 "W18_apps_other",
                 "W18_apps_other_text",
                 "W19_connected_LGTBQ2Splus",
                 "W19_connected_GBQ_men",
                 "W20_general_health",
                 "W21_where_usual_care",
                 "W21_where_usual_care_other_text",
                 "W22_extended_health_medication",
                 "W22_extended_health_counselling",
                 "W22_extended_health_other",
                 "W22_extended_health_none",
                 "W23_health_provider_negative",
                 "W24_fave_pride_meet_people",
                 "W24_fave_pride_old_friends",
                 "W24_fave_pride_community",
                 "W24_fave_pride_support",
                 "W24_fave_pride_political",
                 "W24_fave_pride_let_loose",
                 "W24_fave_pride_freaky",
                 "lab_HIV_screen",
                 "lab_HIV_confirmatory_test",
                 "lab_HIV_viral_load",
                 "lab_HCV_quant",
                 "lab_orth_HCV",
                 "lab_HCV_final")

tables <- list()
table(df$lab_HIV_combo)

for(i in edited_vars){
  result <- try(table(uncoded_df[[i]], df[, i], useNA = "always", dnn = c("Original", "Edited")), silent = TRUE)
  if(class(result) == "try-error") next()
  tables[[i]] <- cbind("Original" = rownames(as.data.frame.matrix(result)), as.data.frame.matrix(result))
}

names(tables) <- str_trunc(names(tables), width = 30, side = "left", ellipsis = "")

write_xlsx(x = tables, path = "\\SexNow2018\\SN2018_Recoding Comparisons")



