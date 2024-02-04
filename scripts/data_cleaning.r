# PACKAGE INSTALLING AND UPGRADING #
# install and load defined list of packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,repos = c(
      CRAN = 'https://cran.rstudio.com',
      CRANextra = 'https://macos.rbind.io'
    )
    )
  sapply(pkg, require, character.only = TRUE)
}

list_of_required_pkg <- c(
  "readstata13",
  'fastDummies',
  'tidyverse',
  "MplusAutomation",
  "naniar",
  "dplyr",
  "here"
)

ipak(list_of_required_pkg)

# this is necessary for mplus automation
#BiocManager::install("rhdf5")

# USER DEFINED FUNCTIONS
turn_function <- function (x) {(max(x,na.rm=T)) -x}
center_scale <- function(x) {
  x <- scale(x, scale = T, center=T)
  x <- as.vector(x)
}

# DATA IMPORTING

base <- readstata13::read.dta13(here("data", "anes_timeseries_2016_Stata13.dta"),
                                convert.factors = F,
                                missing.type = T)


### Set values >80 and <0 to NA in the following variables:
variables <- c(

  # DV: Ideological Extremity
  "V161178",  #PRE: 7pt scale spending and Services self-placement
  "V161181",  #PRE: 7pt scale defense spending self-placement
  "V161184",  #PRE: 7pt scale govt-private medical insur scale: self-placement
  "V161189",  #PRE: 7pt scale guaranteed job-income scale: self-placement
  "V161198",  #PRE: 7pt scale govt assistance to blacks scale: self-placement
  "V161201",  #PRE: 7pt scale environment-jobs tradeoff self-placement
  "V161204x", #PRE: SUMMARY - Favor or oppose affirmative action in universities

  # DV affective polarization: traits (Appendix)
  "V161116", # PRE: Affect for Democratic Pres cand: angry
  "V161118", # PRE: Affect for Democratic Pres cand: afraid
  "V161120", # PRE: Affect for Democratic Pres cand: disgusted
  "V161121", # PRE: Affect for Republican Pres cand: angry
  "V161123", # PRE: Affect for Republican Pres cand: afraid
  "V161125", # PRE: Affect for Republican Pres cand: disgusted

  ### IV: POPULISM ###
  "V162259", # Compromise in politics is selling out on one's principles
  "V162260", # Most politicians do not care about the people
  "V162262", # Politicians are the main problem in the U.S.
  "V162264", # People not politicians should make most important policy decisions
  "V162265", # Most politicians only care about interests of rich and powerful
  "V162267", # The will of the majority should always prevail

  # ANTI-GLOBALIZATION (APPENDIX)
  "V162176x", # SUMMARY- Favor/oppose free trade agreements
  "V162177",  # Should govt encourage/discourage outsourcing

  # Religiosity
  "V161241", # PRE: Is religion important part of R life

  # IV: Perceived polarization
  "V162288", # POST: CSES: 11pt scale: left-right Republican Party
  "V162287", # POST: CSES: 11pt scale: left-right Democratic Party

  # IV: Efficacy
  "V162215", # Publ officials don’t care what people think
  "V162216", # Have no say about what govt does
  "V162217", # Politics/govt too complicated to understand
  "V162218", # Good understanding of political issues

  # IV: Political Knowledge
  "V161513", # Years Senator Elected
  "V161514", # program Fed govt spends
  "V161515", # Party with Most Members In House Before Election
  "V161516", # Party with Most Members In Senate Before Election

  # IV: Other
  "V162256",  # IV: interest
  "V162257",  # IV: follow_politics
  "V162258",  # IV: understand
  "V161270",  # IV: edu
  "V161310x", # IV: race/ethnicity
  "V162289",  # IV: liberal-conservative: self placement
  "V161158x", # IV: PID 7 categories
  "V161361x", # IV: income
  "V161342"  # IV: gender [MALE, FEMALE, OTHER]
)

### Set values > 100 and <0 to NA in the following variables:
thermometers <- c(
  "V161086", # Feeling Thermometer: Democratic Presidential cand
  "V161087", # Feeling Thermometer: Republican Presidential cand
  "V161088", # Feeling Thermometer: Libertarian Presidential cand
  "V161089", # Feeling Thermometer: Green Party Presidential cand
  "V161095", # Feeling Thermometer: Democratic Party
  "V161096", # Feeling Thermometer: Republican Party
  "V162097", # Feeling Thermometer: Liberals
  "V162101"  # Feeling Thermometer: Conservatives
)

### Set values < -1 (did not vote in the primaries) to NA in the following variable:
vote_primaries <- c(
  "V161021a"
)

# set values to NA
base_cleaned <- base %>%
  replace_with_na_at(variables, ~.x >= 80) %>%
  replace_with_na_at(variables, ~.x < 0) %>%
  replace_with_na_at(thermometers, ~.x < 0) %>%
  replace_with_na_at(thermometers, ~.x > 100) %>%
  replace_with_na_at(vote_primaries, ~.x < -1)


# flipping indices that are reversed coded
vars_to_turn <- c(
  ### Populism ###
  "V162259", # Compromise in politics is selling out on one's principles
  "V162260", # Most politicians do not care about the people
  "V162262", # Politicians are the main problem in the U.S.
  "V162264", # People not politicians should make most important policy decisions
  "V162265", # Most politicians only care about interests of rich and powerful
  "V162267", # The will of the majority should always prevail

  # Political Interest
  "V162256", # Interest Politics
  "V162257", # Follow Politics
  "V162258", # Understand Politics

  # Religiosity
  "V161241", # PRE: Is religion important part of R life

  ## EFFICACY
  "V162218", # Good understanding of political issues
  "V161215" # Trust govt in Wash to do what is right #rev
)

# Need a loop because the turn function is not vectorized
for (i in vars_to_turn){
  base_cleaned[[i]] <- turn_function(base_cleaned[[i]])

}

#### RECODING AND INDICIES ####

####---DV: AFFECTIVE POLARIZATION---####
# using candidates thermometers weighted by vote share as in Wagner 2021

base_cleaned <- base_cleaned %>%
  rowwise() %>%
  mutate(sum_affective= sum(c(0.482*V161086, 0.461*V161087, 0.0327*V161088,0.0107*V161089), na.rm=T)) %>%
  mutate(sum_dem = sum(0.482*(V161086 - sum_affective)^2, na.rm=F)) %>%
  mutate(sum_rep = sum(0.461*(V161087 - sum_affective)^2, na.rm=F)) %>%
  mutate(sum_lib = sum(0.0327*(V161088 - sum_affective)^2, na.rm=F)) %>%
  mutate(sum_green = sum(0.0107*(V161089 - sum_affective)^2, na.rm=F)) %>%
  ungroup()

base_cleaned$wagner_aff <- as.double(NA)

for (i in 1:nrow(base_cleaned)){
  sum_nas <- sum(is.na(base_cleaned[i,"sum_dem"]), is.na(base_cleaned[i,"sum_rep"]),  is.na(base_cleaned[i,"sum_lib"]),  is.na(base_cleaned[i,"sum_green"]))

  if (sum_nas>2){
    base_cleaned[i,"wagner_aff"] <- NA
  }else{
    base_cleaned[i,"wagner_aff"]  <- sqrt(sum(unlist(base_cleaned[i,"sum_dem"]),unlist(base_cleaned[i,"sum_rep"]),unlist(base_cleaned[i,"sum_lib"]), unlist(base_cleaned[i,"sum_green"]),na.rm = T))
  }


}


####---DV: IDEOLOGICAL EXTREMITY---####
# respondent self-placement on issues
issues <- c(
  "V161178",    # PRE: 7pt scale spending and Services self-placement
  "V161181",    # PRE: 7pt scale defense spending self-placement
  "V161184",    # PRE: 7pt scale govt-private medical insur scale: self-plmt
  "V161189",    # PRE: 7pt scale guaranteed job-income scale: self-placement
  "V161198",    # PRE: 7pt scale govt assistance to blacks scale: self-placemt
  "V161201",    # PRE: 7pt scale environment-jobs tradeoff self-placement
  "V161204x"    # PRE: 7pt SUMMARY - Favor or oppose affirmative action in universities
)

# labels for the issues
labels <- c("spending",
            "defence",
            "medical",
            "garant_inc",
            "blacks",
            "envir",
            "affirm"
)

for (i  in seq(issues)){
  iss <- issues[i]
  label <- paste(labels[i],"extr", sep="_")
  # folding on midpoint
  out <- sqrt((base_cleaned[[iss]] - 4)^2)
  base_cleaned <- cbind(base_cleaned,out)
  names(base_cleaned)[length(names(base_cleaned))] <- label
}

#### IV: GENDER ####
base_cleaned <- base_cleaned %>% mutate_at(vars(V161342), as.factor)
base_cleaned$ged_d <- as.factor(ifelse(base_cleaned$V161342==3, NA, base_cleaned$V161342)-1)

#### IV: PID 3 categories ####
base_cleaned$pid_3 <- as.factor(ifelse(as.numeric(base_cleaned$V161158x)<4,"DEM",
                                       ifelse(as.numeric(base_cleaned$V161158x)>4, "REP", "IND")))

#### IV: STRENGHT PID ####
base_cleaned$pid_ind_to_recode <- dplyr::recode(base_cleaned$V161158x,
                                                "3"="indDem",
                                                "4"="ind",
                                                "5"="indREP",
                                                "1"="StrongDemocrat",
                                                "2"="NotStrongDemocract",
                                                "6"="NotStrongRepublican",
                                                "7"="StrongRepublican")

base_cleaned$str_pid <- dplyr::recode(base_cleaned$pid_ind_to_recode,"indDem"=1, "ind"=NA_real_, "indREP"=1, "StrongDemocrat"=3, "NotStrongDemocract"=2,"NotStrongRepublican"=2,"StrongRepublican"=3)

#### IV: POLITICAL KNOWLEDGE - CODING CORRECT ANSWERS ####
base_cleaned$V161513 <- ifelse(base_cleaned$V161513==6,1,0)
base_cleaned$V161514 <- ifelse(base_cleaned$V161514==1,1,0)
base_cleaned$V161515 <- ifelse(base_cleaned$V161515==2,1,0)
base_cleaned$V161516 <- ifelse(base_cleaned$V161516==2,1,0)

#### IV: PRIMARY VOTE - Voted Bernie ####
# recoding -1 (inapplicable) as 0. This indicates people who did not vote in the primaries at all
base_cleaned$bernie <- ifelse(base_cleaned$V161021a==2,1,0)

#### IV: PERCIEVED POLARIZATION (PARTIES) ####
base_cleaned$perc_pol <- sqrt((base_cleaned[["V162288"]] - base_cleaned[["V162287"]])^2)
base_cleaned$perc_pol7 <- sqrt((base_cleaned[["V161130"]] - base_cleaned[["V161131"]])^2)

#### IV: RACE ####
# recoding race (self-identification) with labels
# Native American coded as other since very few observations
# Reference: White
base_cleaned$V161310x <- dplyr::recode(base_cleaned$V161310x,"1"="White", "2"="African American", "3"="Asian", "4"="Native American", "5"="Hispanic","6"="Others")
base_cleaned$V161310x <- as.factor(ifelse(base_cleaned$V161310x=="Native American","Others",as.character(base_cleaned$V161310x)))
base_cleaned$V161310x <- relevel(base_cleaned$V161310x, ref="White")

#### IV: STRENGHT IDEOLOGICAL IDENTITY# ###
# using the 7pt scale liberal-conservative: self placement.
# the more extreme on the scale the stronger the ideological identity. 4 is the center
base_cleaned$id_iden7 <- sqrt((base_cleaned$V161126 - 4)^2)

#### RENAMING VARIABLES FOR MPLUS  ####

# STRENGHT PID
data_mplus <- fastDummies::dummy_cols(base_cleaned,
                                      "str_pid",
                                      remove_first_dummy = TRUE,
                                      ignore_na = TRUE)

names(data_mplus)[grep("str_pid_",names(data_mplus))] <- c("WEAK","STRONG"

)

# RACE
data_mplus <- fastDummies::dummy_cols(data_mplus,
                                      "V161310x",
                                      remove_first_dummy = TRUE,
                                      ignore_na = TRUE)

names(data_mplus)[grep("V161310x_",names(data_mplus))] <- c("BLACK","ASIAN","HISP","OTHERR")

# PID
data_mplus$pid_3 <- relevel(data_mplus$pid_3, ref="IND")

data_mplus <- fastDummies::dummy_cols(data_mplus,
                                      "pid_3",
                                      remove_first_dummy = TRUE,
                                      ignore_na = TRUE)

names(data_mplus)[grep("pid_3",names(data_mplus))][-1] <- c("DEM","REP"
)

# GENDER
data_mplus <- fastDummies::dummy_cols(data_mplus,
                                      "ged_d",
                                      remove_first_dummy = TRUE,
                                      ignore_na = TRUE)

names(data_mplus)[grep("ged_d_",names(data_mplus))] <- c("FEM")

data_mplus_centred <- data_mplus




