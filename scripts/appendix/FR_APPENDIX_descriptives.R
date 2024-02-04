# GENERATES TABLE DESCRIPTIVES FOR THE APPENDIX #
library(here)
library(filesstrings)
library(glue)
library(gtsummary)
library(forcats)
source(here("scripts","data_cleaning.r"))

variable_to_model_mplus <- c(

  # IDEOLOGICAL EXTREMITY
  "spending_extr",
  "defence_extr",
  "medical_extr",
  "garant_inc_extr",
  "blacks_extr",
  "envir_extr",
  "affirm_extr",

  # AFFECTIVE POLARIZATION
  "wagner_aff",

  # POLITICAL KNOWLEDGE
  "V161513",
  "V161514",
  "V161515",
  "V161516",

  # POPULISM
  "V162259",    # Compromise in politics is selling out on one's principles [X] [Manicheism]
  "V162260",    # Most politicians do not care about the people [X] [antielite/peoplecentr]
  "V162262",    # Politicians are the main problem in the U.S.  [antielite]
  "V162264",    # People not politicians should make most important policy decisions  [peoplecentr]
  "V162265",    # Most politicians only care about interests of rich and powerful [antielite]
  "V162267",    # The will of the majority should always prevail [X]  [Manicheism]

  # CONTROLS
  # DEMO
  "V161270",    # edu
  "V161361x",   # income
  "V161267",    # age
  "V161241",    # religious importance

  # GENDER (Male)
  "FEM",        # Female

  # RACE (Ref: WHITE)
  "V161310x",
  # Primary vote
  "bernie",

  # PERCIEVED PARTY POLARIZATION
  "perc_pol",

  # L-C STRENGHT IDEOLOGICAL IDENTITY
  "id_iden7",

  # PID + STRENGHT
  "pid_3",
  "str_pid",

  # POLITICAL INTEREST
  "V162256",     #interest
  "V162257",     #follow_politics

  # EXTERNAL EFFICACY
  "V162215",     # Publ officials don’t care what people think
  "V162216",     # Have no say about what govt does

  # INTERNAL EFFICACY
  "V162217",     # Politics/govt too complicated to understand
  "V162218",     # Good understanding of political issues

  # WEIGHTS
  "V160102"

)


data_mplus_subset <- data_mplus_centred[variable_to_model_mplus]
data_mplus_subset <- data_mplus_subset  %>% filter(pid_3 %in% c("DEM", "REP")) %>% mutate_at(vars(pid_3), factor)
data_mplus_subset$pid_3 <- relevel(data_mplus_subset$pid_3, ref="DEM")

dummy <- c("pid_3",
           "str_pid",
           "bernie",
           "V161241",
           "FEM",
           "V161310x",
           "V161513",
           "V161514",
           "V161515",
           "V161516")


descriptive_selected <- data_mplus_subset %>%
  mutate_at(vars(dummy), as.factor) %>%
  mutate_at(vars(V161513,V161514,V161515,V161516),  .funs = forcats::fct_recode,
                "Incorrect" = "0",
                "Correct" = "1") %>%
  mutate_at(vars(V161241),  .funs = forcats::fct_recode,
          "Non important" = "0",
          "Important" = "1") %>%
  mutate_at(vars(bernie),  .funs = forcats::fct_recode,
            "Other candidate" = "0",
            "Sanders" = "1") %>%
  mutate_at(vars(FEM),  .funs = forcats::fct_recode,
            "Male" = "0",
            "Female" = "1") %>%
  mutate_at(vars(str_pid),  .funs = forcats::fct_recode,
          "Leaner" = "1",
          "Weak partisan" = "2",
          "Strong partisan" = "3"
          )  %>%
  mutate_at(vars(pid_3),  .funs = forcats::fct_recode,
            "Democratic Party" = "DEM",
            "Republican Party" = "REP"
  )


table_desc <-
  tbl_summary(
    descriptive_selected,
    include=(-V160102),
    type = list(variable_to_model_mplus[!variable_to_model_mplus %in% c(dummy, "V160102")] ~ 'continuous2'),
    missing = "no", # don't list missing data separately
    statistic = list(variable_to_model_mplus[!variable_to_model_mplus %in% c(dummy, "V160102")] ~ c("{mean} ({sd})","{median} ({p25}, {p75})", "{min} - {max}"),
                     dummy ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    label = c(
      spending_extr ~ "Ideological extremity (services, V161178, folded)",
      defence_extr ~ "Ideological extremity (defence, V161181, folded)",
      medical_extr ~ "Ideological extremity (medical, V161184 , folded)",
      garant_inc_extr ~ "Ideological extremity (standard living, V161189 , folded)",
      blacks_extr ~ "Ideological extremity (blacks, V161198 , folded)",
      envir_extr ~ "Ideological extremity (environment V161201 , folded)",
      affirm_extr ~ "Ideological extremity (affermative actions, V161204x, folded)",
      wagner_aff ~ "Affective Polarization (index)",
      V161513 ~ "Political knowledge (senators, V161513)",
      V161514 ~ "Political knowledge (spending, V161514)",
      V161515 ~ "Political knowledge (house, V161515)",
      V161516 ~ "Political knowledge (senate, V161516)",
      V162259 ~ "Populism (M1, V162259)",
      V162260 ~ "Populism (AE1, V162260)",
      V162262 ~ "Populism (AE2, V162262)",
      V162264 ~ "Populism (PC1, V162264)",
      V162265 ~ "Populism (AE3, V162265)",
      V162267 ~ "Populism (-, V162267)",
      V161241 ~ "Importance Religion (V161241)",
      FEM ~ "Gender (V161342)",
      V161310x ~ "Race (self-identification, V161310x)",
      perc_pol ~ "Percieved party polarization (V162260)" ,
      id_iden7 ~ "Strenght ideological identity (V162289)",
      pid_3 ~ "Party ID (V161158x)",
      str_pid ~ "Strenght Party ID (V161158x)",
      bernie ~ "Primary vote (V162260)",
      V162256 ~ "Interest in politics (V162256)",
      V162257 ~ "Follow politics in media (V162257)",
      V162215 ~ "External Efficacy (publ. officials, V162215)",
      V162216 ~ "External Efficacy (no say, V162216)",
      V162217 ~ "Internal Efficacy (too complicated, V162217)",
      V162218 ~ "Internal Efficacy (understanding, V162218)",
      V161270 ~ "Education (V161270)",
      V161361x ~ "Income (V161361x)",
      V161267 ~ "Age (V161267)"
      )) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()

saveRDS(table_desc, here("manuscript","appendix_descriptives.rds"))

