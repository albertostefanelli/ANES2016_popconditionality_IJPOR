##### READ MPLUS .OUT FILES TO GENERATE TABLE 11 IN THE APPENDIX #####

require(MplusAutomation)
require(dplyr)
require(here)
require(flextable)
library(dplyr)
library(data.table)
library(reshape2)

mod_to_read <- "F_APPENDIX_independents.out"
all_output <- readModels(here("scripts", "appendix", "mplus_out", mod_to_read), quiet = TRUE)

ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)

#### IDEOLOGICAL REGRESSION TABLE

dummy <- c("WEAK",
           "STRONG",
           "BERNIE",
           "V161241",
           "FEM",
           "BLACK",
           "ASIAN",
           "HISP",
           "OTHERR")

# DEMOCRATS

regression_ideological_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==1) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==1) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological_dem <- rbind(regression_ideological_c,regression_ideological_d)  %>% select(-std)

names(regression_ideological_dem) <- c("P","Coefficient","Std. Error","p-value")
regression_ideological_dem$`PID` <- 'Democrats'

# INDEPEDENTS

regression_ideological_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==2) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==2) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological_ind <- rbind(regression_ideological_c,regression_ideological_d)  %>% select(-std)

names(regression_ideological_ind) <- c("P","Coefficient","Std. Error","p-value")
regression_ideological_ind$`PID` <- 'Independents'

# REPUBLICANS

regression_ideological_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==3) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==3) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological_rep <- rbind(regression_ideological_c,regression_ideological_d)  %>% select(-std)

names(regression_ideological_rep) <- c("P","Coefficient","Std. Error","p-value")
regression_ideological_rep$`PID` <- 'Republicans'

merged_ideological <- rbind(regression_ideological_dem,regression_ideological_ind,regression_ideological_rep)

merged_ideological <- merged_ideological %>% mutate_at(vars("p-value"),list(~ as.numeric(.)))  %>%
  mutate_at(vars("p-value"),list(~ round(.,3)))  %>%
  mutate_at(vars("p-value"),list(~ format(.,3)))

merged_ideological <- merged_ideological %>% mutate_at(vars(Coefficient),list(~ as.numeric(.)))  %>%
  mutate_at(vars(Coefficient),list(~ round(.,3)))  %>%
  mutate_at(vars(Coefficient),list(~ format(.,3)))

merged_ideological <- merged_ideological %>% mutate_at(vars("Std. Error"),list(~ as.numeric(.)))  %>%
  mutate_at(vars("Std. Error"),list(~ round(.,3)))  %>%
  mutate_at(vars("Std. Error"),list(~ format(.,3)))

merged_ideological$"p-value" <- ifelse(merged_ideological$"p-value"<=0.001, "≤0.001",merged_ideological$"p-value")

merged_ideological$Coefficient <- ifelse(merged_ideological$Coefficient<0, merged_ideological$Coefficient, paste("", merged_ideological$Coefficient))
merged_ideological$Coefficient <- paste0(merged_ideological$Coefficient, " (", merged_ideological$"Std. Error", ")")

merged_ideological <- merged_ideological %>% select(-"Std. Error")

names(merged_ideological) <- c('p','c',"pv","dv")

merged_ideological[[1]]<- dplyr::recode(merged_ideological[[1]],
                                 `POP` = "Populist Attitudes",
                                 `EFFI` = "Internal Efficacy",
                                 `EFFE`= "External Efficacy",
                                 `POL_INT`= "Political Interest",
                                 `KN` = "Political Knowledge",
                                 `V161241` = "Importance Religion (Ref: No) ",
                                 PERC_POL = "Perceived Polarization",
                                 ID_IDEN7 = "Strength Ideological Identity",
                                 BERNIE = "Voted for Sanders (Ref: No)",
                                 V161270 = "Education",
                                 V161361X = "Income",
                                 V161267 = "Age",
                                 FEM = "Female (ref: Male)",
                                 BLACK = "African-Americans (ref: White)",
                                 ASIAN = "Asian (ref: White)",
                                 HISP = "Hispanic (ref: White)",
                                 OTHERR = "Others (ref: White)"

)

merged_ideological_d <- merged_ideological %>%
  filter(dv=="Democrats")

names(merged_ideological_d) <-  c('p','c_dem',"pv_dem","dv")
merged_ideological_d <- merged_ideological_d %>% select(-dv)

merged_ideological_i <- merged_ideological %>%
  filter(dv=="Independents")

names(merged_ideological_i) <-  c('p','c_ind',"pv_ind","dv")
merged_ideological_i <- merged_ideological_i %>% select(-dv,-p)

merged_ideological_r <- merged_ideological %>%
  filter(dv=="Republicans")

names(merged_ideological_r) <- c('p','c_rep',"pv_rep","dv")
merged_ideological_r <- merged_ideological_r %>% select(-dv, -p)

merged_df <- cbind(merged_ideological_d, merged_ideological_i, merged_ideological_r)

set_flextable_defaults(fonts_ignore = TRUE, line_spacing = 1, border.color="black")
border <- fp_border_default(width = 0.5)

table_ind_ideo <- merged_df %>%
  flextable() %>%
  add_header_row(values = c("", "Democrats", "Independents", "Republicans"),
                 colwidths = c(1, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  flextable::compose(i=2, j=c(2,4,6),
                     part = "header",
                     value = as_paragraph(
                       "Std. Coefficent (\U03B2)"
                     )
  )%>%
  flextable::compose(i=2, j=c(3,5,7),
                     part = "header",
                     value = as_paragraph(
                       "p-value"
                     )
  ) %>%
  add_footer(p = paste("N=", sample_size, ", Log-likelihood=", ll, ", AIC=", aic, ", BIC=", bic, sep="")) %>%
  merge_at(j = 1:7, part = "footer") %>%
  align(part = "footer", align = "right") %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) %>%
  hline_bottom(border = border) %>%
  hline_top(border = border) %>%
  add_footer_lines(values = "Notes: All continuous variables are standardized. Beta coefficients for Populist Attitudes are allowed to vary across partisan groups. Std. errors in pharentesis")


#### AFFECTIVE REGRESSION TABLE ####

dummy <- c("WEAK",
           "STRONG",
           "BERNIE",
           "V161241",
           "FEM",
           "BLACK",
           "ASIAN",
           "HISP",
           "OTHERR")

# DEMOCRATS

regression_affective_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==1) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="WAGNER_AFF.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==1) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="WAGNER_AFF.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective_dem <- rbind(regression_affective_c,regression_affective_d)  %>% select(-std)

names(regression_affective_dem) <- c("P","Coefficient","Std. Error","p-value")
regression_affective_dem$`PID` <- 'Democrats'

# INDEPEDENTS

regression_affective_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==2) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="WAGNER_AFF.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==2) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="WAGNER_AFF.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective_ind <- rbind(regression_affective_c,regression_affective_d)  %>% select(-std)

names(regression_affective_ind) <- c("P","Coefficient","Std. Error","p-value")
regression_affective_ind$`PID` <- 'Independents'

# REPUBLICANS

regression_affective_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==3) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="WAGNER_AFF.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==3) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="WAGNER_AFF.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective_rep <- rbind(regression_affective_c,regression_affective_d)  %>% select(-std)

names(regression_affective_rep) <- c("P","Coefficient","Std. Error","p-value")
regression_affective_rep$`PID` <- 'Republicans'

merged_affective <- rbind(regression_affective_dem,regression_affective_ind,regression_affective_rep)

merged_affective <- merged_affective %>% mutate_at(vars("p-value"),list(~ as.numeric(.)))  %>%
  mutate_at(vars("p-value"),list(~ round(.,3)))  %>%
  mutate_at(vars("p-value"),list(~ format(.,3)))

merged_affective <- merged_affective %>% mutate_at(vars(Coefficient),list(~ as.numeric(.)))  %>%
  mutate_at(vars(Coefficient),list(~ round(.,3)))  %>%
  mutate_at(vars(Coefficient),list(~ format(.,3)))

merged_affective <- merged_affective %>% mutate_at(vars("Std. Error"),list(~ as.numeric(.)))  %>%
  mutate_at(vars("Std. Error"),list(~ round(.,3)))  %>%
  mutate_at(vars("Std. Error"),list(~ format(.,3)))

merged_affective$"p-value" <- ifelse(merged_affective$"p-value"<=0.001, "≤0.001",merged_affective$"p-value")

merged_affective$Coefficient <- ifelse(merged_affective$Coefficient<0, merged_affective$Coefficient, paste("", merged_affective$Coefficient))
merged_affective$Coefficient <- paste0(merged_affective$Coefficient, " (", merged_affective$"Std. Error", ")")

merged_affective <- merged_affective %>% select(-"Std. Error")

names(merged_affective) <- c('p','c',"pv","dv")

merged_affective[[1]]<-  dplyr::recode(merged_affective[[1]],
                               `POP` = "Populist Attitudes",
                               `EFFI` = "Internal Efficacy",
                               `EFFE`= "External Efficacy",
                               `POL_INT`= "Political Interest",
                               `KN` = "Political Knowledge",
                               `V161241` = "Importance Religion (Ref: No) ",
                               PERC_POL = "Perceived Polarization",
                               ID_IDEN7 = "Strength affective Identity",
                               BERNIE = "Voted for Sanders (Ref: No)",
                               V161270 = "Education",
                               V161361X = "Income",
                               V161267 = "Age",
                               FEM = "Female (ref: Male)",
                               BLACK = "African-Americans (ref: White)",
                               ASIAN = "Asian (ref: White)",
                               HISP = "Hispanic (ref: White)",
                               OTHERR = "Others (ref: White)"

)

merged_affective_d <- merged_affective %>%
  filter(dv=="Democrats")

names(merged_affective_d) <-  c('p','c_dem',"pv_dem","dv")
merged_affective_d <- merged_affective_d %>% select(-dv)

merged_affective_i <- merged_affective %>%
  filter(dv=="Independents")

names(merged_affective_i) <-  c('p','c_ind',"pv_ind","dv")
merged_affective_i <- merged_affective_i %>% select(-dv,-p)

merged_affective_r <- merged_affective %>%
  filter(dv=="Republicans")

names(merged_affective_r) <- c('p','c_rep',"pv_rep","dv")
merged_affective_r <- merged_affective_r %>% select(-dv, -p)

merged_df <- cbind(merged_affective_d, merged_affective_i, merged_affective_r)

set_flextable_defaults(fonts_ignore = TRUE, line_spacing = 1, border.color="black")
border <- fp_border_default(width = 0.5)

table_ind_aff <- merged_df %>%
  flextable() %>%
  add_header_row(values = c("", "Democrats", "Independents", "Republicans"),
                 colwidths = c(1, 2, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  flextable::compose(i=2, j=c(2,4,6),
                     part = "header",
                     value = as_paragraph(
                       "Std. Coefficent (\U03B2)"
                     )
  )%>%
  flextable::compose(i=2, j=c(3,5,7),
                     part = "header",
                     value = as_paragraph(
                       "p-value"
                     )
  ) %>%
  add_footer(p = paste("N=", sample_size, ", Log-likelihood=", ll, ", AIC=", aic, ", BIC=", bic, sep="")) %>%
  merge_at(j = 1:7, part = "footer") %>%
  align(part = "footer", align = "right") %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) %>%
  hline_bottom(border = border) %>%
  hline_top(border = border) %>%
  add_footer_lines(values = "Notes: All continuous variables are standardized. Beta coefficients for Populist Attitudes are allowed to vary across partisan groups. Std. errors in pharentesis")



