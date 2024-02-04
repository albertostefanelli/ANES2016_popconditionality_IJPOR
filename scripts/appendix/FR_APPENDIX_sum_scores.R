##### FIT SUM SCORE MODEL WITH OLS TO GENERATE COEFFICENT REPORTED IN TABLE 9, TABLE 10 AND 11 IN THE APPENDIX #####

library(here)
library(survey)
library(emmeans)
source(here("scripts","data_cleaning.r"))

### create sum indices and check reliability using alpha

data_mplus <- data_mplus %>%
  rowwise() %>%
  mutate(sum_pop= mean(c(V162259, V162260, V162262,V162264, V162265, V162267), na.rm=TRUE))

psych::alpha(data_mplus[,c("V162259", "V162260", "V162262","V162264", "V162265", "V162267")],check.keys=TRUE)

data_mplus <- data_mplus %>%
  rowwise() %>%
  mutate(sum_effi= mean(c(V162217, V162218), na.rm=TRUE))

psych::alpha(data_mplus[,c("V162217", "V162218")],check.keys=TRUE)

data_mplus <- data_mplus %>%
  rowwise() %>%
  mutate(sum_effe= mean(c(V162215, V162216), na.rm=TRUE))

psych::alpha(data_mplus[,c("V162215", "V162216")],check.keys=TRUE)

data_mplus <- data_mplus %>%
  rowwise() %>%
  mutate(sum_polint= mean(c(V162256, V162257), na.rm=TRUE))

psych::alpha(data_mplus[,c("V162256", "V162257")],check.keys=TRUE)

data_mplus <- data_mplus %>%
  rowwise() %>%
  mutate(sum_nk= sum(c(V161513, V161514, V161515, V161516), na.rm=TRUE))

# slope of pop across pid
# same as the estimate that we get from the package margins but with significance testing

# looping on issues to create separate polarization indexes for each issue
for (i  in seq(issues)){
  iss <- issues[i]
  label <- paste(labels[i],"spre", sep="_")
  out <- sqrt((data_mplus[[iss]] - mean(data_mplus[[iss]],na.rm=TRUE))^2)
  data_mplus <- cbind(data_mplus, out)
  names(data_mplus)[length(names(data_mplus))] <- label
}


# averaging the separate polarization indexes together
data_mplus <- data_mplus %>%
  rowwise() %>%
  mutate(spread_overall= mean(c(spending_spre, defence_spre, medical_spre, garant_inc_spre, blacks_spre, envir_spre), na.rm=TRUE))


data_mplus_centred_s <- data_mplus %>%
  ungroup() %>% # needed to disable rowwise
  mutate_at(vars(
    spread_overall,
    wagner_aff,
    id_iden7,
    perc_pol,
    sum_nk,
    sum_polint,
    sum_effe,
    sum_effi,
    sum_pop,
    V161270,
    V161361x,
    V161267), center_scale)

data_mplus_centred_f <-  data_mplus_centred_s%>%
  mutate_at(vars(pid_3), factor) %>%
  filter(pid_3 %in% c("DEM", "REP"))

### SIMPLE PAIRWISE CORRELATIONS (TABLE 11) ###

cor(data_mplus_centred_f[data_mplus_centred_f$pid_3=="REP", "sum_pop"],
data_mplus_centred_f[data_mplus_centred_f$pid_3=="REP", "spread_overall"],
use = "na.or.complete")

cor(data_mplus_centred_f[data_mplus_centred_f$pid_3=="REP", "sum_pop"],
    data_mplus_centred_f[data_mplus_centred_f$pid_3=="REP", "wagner_aff"],
    use = "na.or.complete")


cor(data_mplus_centred_f[data_mplus_centred_f$pid_3=="DEM", "sum_pop"],
    data_mplus_centred_f[data_mplus_centred_f$pid_3=="DEM", "spread_overall"],
    use = "na.or.complete")


cor(data_mplus_centred_f[data_mplus_centred_f$pid_3=="DEM", "sum_pop"],
    data_mplus_centred_f[data_mplus_centred_f$pid_3=="DEM", "wagner_aff"],
    use = "na.or.complete")

### MARGINAL EFFECTS WITH OLS ROBUST (TABLE 9 and 10) ###

# taking into account the nested structure
data_svy <- survey::svydesign(~V160001 ,
                              strata = ~V160201 ,
                              data = data_mplus_centred_f ,
                              weights = ~V160102 ,
                              nest = TRUE)


m_ext_svy <- survey::svyglm(spread_overall ~
                          sum_pop*pid_3 +
                          pid_3 +
                          sum_pop +
                          sum_effi +
                          sum_effe +
                          sum_polint +
                          sum_nk+
                          WEAK+
                          STRONG+
                          bernie+
                          V161241 +
                          perc_pol +
                          id_iden7 +
                          V161270 +
                          V161361x +
                          V161267 +
                          FEM +
                          BLACK +
                          ASIAN +
                          HISP,
                        data_svy)


ideological_trend_svy <- emtrends(m_ext_svy, pairwise ~ pid_3, var = "sum_pop", adjust = "none") %>%
  test()



m_affective_svy <- survey::svyglm(wagner_aff ~
                                    sum_pop*pid_3 +
                                    pid_3 +
                                    sum_pop +
                                    sum_effi +
                                    sum_effe +
                                    sum_polint +
                                    sum_nk+
                                    WEAK+
                                    STRONG+
                                    bernie+
                                    V161241 +
                                    perc_pol +
                                    id_iden7 +
                                    V161270 +
                                    V161361x +
                                    V161267 +
                                    FEM +
                                    BLACK +
                                    ASIAN +
                                    HISP,
                            data_svy)


affective_trend_svy <- emtrends(m_affective_svy, pairwise ~ pid_3, var = "sum_pop", adjust = "none") %>%
  test()

### survey GLM marginal effect for ideological polarization  ###

table_ideo <- ideological_trend_svy$emtrends
table_ideo <-  table_ideo %>% select(-t.ratio, -df)

names(table_ideo) <- c("PID", "Marginal effect", "Std. Error", "p-value")

table_ideo$PID <- c("Democrats", "Republicans")

table_ideo[2] <- round(table_ideo[2] ,3)
table_ideo[3] <- round(table_ideo[3] ,3)
table_ideo[4] <- round(table_ideo[4] ,4)
table_ideo[4] <- ifelse(table_ideo[4]<=0.001, "≤0.001",table_ideo[4][[1]])

### survey GLM marginal effect for affective polarization ###
table_aff <- affective_trend_svy$emtrends
table_aff <-  table_aff %>% select(-t.ratio, -df)

names(table_aff) <- c("PID", "Marginal effect", "Std. Error", "p-value")

table_aff$PID <- c("Democrats", "Republicans")

table_aff[2] <- round(table_aff[2] ,3)
table_aff[3] <- round(table_aff[3] ,3)
table_aff[4] <- round(table_aff[4] ,4)
table_aff[4] <- ifelse(table_aff[4]<=0.001, "≤0.001",table_aff[4][[1]])
