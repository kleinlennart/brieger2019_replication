library(tidyverse)

# World Values Survey Wave 5 ----------------------------------------------

# https://www.worldvaluessurvey.org/WVSDocumentationWV5.jsp

raw <- readRDS(here::here("data", "WVS_Wave5", "F00007944-WV5_Data_R_v20180912.rds"))

dat <- raw %>%
  mutate(
    country_f = V2 %>% haven::as_factor(),
    country_chr = country_f %>% as.character(),

    ## Environmental Concern ########################

    # “I would give part of my income if I were certain that the money would be used to prevent environmental pollution,”
    env_income_f = V105 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")),
    # # “I would agree to an increase in taxes if the extra money were used to prevent environmental pollution.”
    env_taxes_f = V106 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")),

    # 1 for strongly disagree, 2 for disagree, 3 for rather agree, and 4 for strongly agree.
    # FIXED: "Rather agree" not a possibility here!!
    env_income_num = env_income_f %>%
      case_match(
        "Strongly disagree" ~ 1,
        "Disagree" ~ 2,
        "Agree" ~ 3,
        "Strongly agree" ~ 4
      ),
    env_taxes_num = env_taxes_f %>% case_match(
      "Strongly disagree" ~ 1,
      "Disagree" ~ 2,
      "Agree" ~ 3,
      "Strongly agree" ~ 4
    ),

    ## Social Identity ############################

    # “I see myself as a world citizen,”
    sid_world_f = V210 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")), ,
    # “I see myself as part of my local community,”
    sid_communal_f = V211 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")), ,
    # “I see myself as a part of the [respective] nation.” (Rieger)
    # FIXED: "I see myself as citizen of the [country] nation" (official)
    sid_nation_f = V212 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")), ,

    # Recoded:
    sid_world_num = sid_world_f %>% case_match(
      "Strongly disagree" ~ 1,
      "Disagree" ~ 2,
      "Agree" ~ 3,
      "Strongly agree" ~ 4
    ),
    sid_communal_num = sid_communal_f %>% case_match(
      "Strongly disagree" ~ 1,
      "Disagree" ~ 2,
      "Agree" ~ 3,
      "Strongly agree" ~ 4
    ),
    sid_nation_num = sid_nation_f %>% case_match(
      "Strongly disagree" ~ 1,
      "Disagree" ~ 2,
      "Agree" ~ 3,
      "Strongly agree" ~ 4
    ),

    ## Controls #########
    sex = V235 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Missing; Unknown", "Not asked")),
    age_f = V237 %>% haven::as_factor() %>% fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Missing; Unknown", "Not asked")),
    age_num = age_f %>% as.character() %>% as.numeric(),
    # variable consists of nine groups, ranging from 1 “no formal education” to 9 “university-level education, with degree.”
    education = V238 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Missing; Unknown", "Not asked", "Not applicable")),
    # household income on a 10-point scale ranging from 1 for “lowest decile” to 10 for “highest decile.”
    income = V253 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Missing; Unknown", "Not asked")),
    # five groups, ranging from 1 “lower class” to 5 “upper class.”
    social_class = V252 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Missing; Unknown", "Not asked")),

    # Left political ideology was measured by a 10-point scale,
    # ranging from 1 representing the most right-wing political views to 10 representing the most left- wing political views.
    pol_ideo_f = V114 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")),
    pol_ideo_num = pol_ideo_f %>%
      fct_recode(
        "1" = "Left",
        "10" = "Right",
      ) %>% as.character() %>% as.numeric(),
    left_ideology = 11 - pol_ideo_num, # the higher, the lefter
    # Inglehart’s established four-item postmaterialism index.
    postmaterialism = Y001 %>% haven::as_factor() %>%
      fct_na_level_to_value(c("No answer", "Dont know", "Missing; Not asked by the interviewer", "Not asked")) %>%
      fct_recode(
        "0" = "Materialist",
        "1" = "1",
        "2" = "2",
        "3" = "3",
        "4" = "4",
        "5" = "Post-materialist"
      ) %>% as.character() %>% as.numeric()
  ) %>%
  select(
    country_f, country_chr, env_income_f, env_taxes_f, env_income_num, env_taxes_num,
    sid_world_f, sid_communal_f, sid_nation_f, sid_world_num, sid_communal_num,
    sid_nation_num, sex, age_f, age_num, education, income, social_class,
    pol_ideo_f, pol_ideo_num, left_ideology, postmaterialism
  )


# labelled::look_for(raw, "Postmaterial")

# Item Scores -------------------------------------------------------------
dat <- dat %>%
  # select(starts_with("env_"), starts_with("sid_")) %>%
  mutate(
    # higher values -> higher concern
    env_concern_avg = rowMeans(pick(env_income_num, env_taxes_num)),
    sid_avg = rowMeans(pick(sid_world_num, sid_communal_num, sid_nation_num))
  )

# "This procedure led to a finer seven-point index, yielding fractions from 1 to 4."
# seq(1, 4, 0.5)



# Non-Response ------------------------------------------------------------

# psych::describe(dat)
# skimr::skim(dat)


saveRDS(dat, "data/export/cleaned_dat.rds")

clean <- dat %>% drop_na()
nrow(clean)


country_desc <- clean %>%
  drop_na() %>%
  count(country)
View(country_desc)


# -------------------------------------------------------------------------


# "Since both variables are highly correlated and have a Cronbach’s alpha of .79"
env_scale <- dat %>%
  select(env_income_num, env_taxes_num) %>%
  drop_na()
cor(env_scale$env_income_num, env_scale$env_taxes_num)

alpha <- psych::alpha(env_scale)
alpha[["total"]][["raw_alpha"]]



# The final sample comprises 32,777 individuals located in 38 countries
