#### READ MAIN TEXT OVERALL MODEL FROM MPLUS .OUT FILES ####
require(MplusAutomation)
require(dplyr)
require(here)
require(flextable)
library(dplyr)
library(data.table)
library(reshape2)

mod_to_read <- "F_MAIN_fixed_pop.out"

all_output <- readModels(here("scripts", "manuscript", "mplus_out", mod_to_read), quiet = TRUE)
ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)

regression_ideological_c<- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
  filter(LatentClass==1) %>%
  mutate(std="continuous") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(!param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)


# coefficents dummies do not need to be standardized.
dummy <- c("WEAK",
           "STRONG",
           "BERNIE",
           "V161241",
           "FEM",
           "BLACK",
           "ASIAN",
           "HISP",
           "OTHERR")

regression_ideological_d <- paramExtract(all_output$parameters$stdy.standardized, "regression") %>%
  filter(LatentClass==1) %>%
  mutate(std="dummy") %>%
  filter(paramHeader=="EXT.ON") %>%
  filter(param %in% dummy) %>%
  select(param, est, se, pval,std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_ideological <- rbind(regression_ideological_c,regression_ideological_d)  %>% select(-std)

names(regression_ideological) <- c("P","Coefficient","Std. Error", "p-value")
regression_ideological$`Dependent Variable` <- 'Ideological Extremity'

regression_affective_c <- paramExtract(all_output$parameters$stdyx.standardized, "regression") %>%
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
  select(param, est, se, pval, std)%>%
  as.data.frame() %>%
  mutate_at(vars(est,se,pval) , as.double)

regression_affective <- rbind(regression_affective_c,regression_affective_d)  %>% select(-std)

names(regression_affective) <- c("P","Coefficient","Std. Error",  "p-value")
regression_affective$`Dependent Variable` <- 'Affective Polarization'

merged_reg <- rbind(regression_ideological,regression_affective)

merged_reg <- merged_reg %>% mutate_at(vars("p-value"),list(~ as.numeric(.)))  %>%
  mutate_at(vars("p-value"),list(~ round(.,3)))  %>%
  mutate_at(vars("p-value"),list(~ format(.,3)))

merged_reg <- merged_reg %>% mutate_at(vars(Coefficient),list(~ as.numeric(.)))  %>%
  mutate_at(vars(Coefficient),list(~ round(.,3)))  %>%
  mutate_at(vars(Coefficient),list(~ format(.,3)))

merged_reg <- merged_reg %>% mutate_at(vars("Std. Error"),list(~ as.numeric(.)))  %>%
  mutate_at(vars("Std. Error"),list(~ round(.,3)))  %>%
  mutate_at(vars("Std. Error"),list(~ format(.,3)))

merged_reg$"p-value" <- ifelse(merged_reg$"p-value"<=0.001, "≤0.001", paste(" ",merged_reg$"p-value"))

merged_reg$Coefficient <- ifelse(merged_reg$Coefficient<0, merged_reg$Coefficient, paste("",merged_reg$Coefficient))
merged_reg$Coefficient <- paste0(merged_reg$Coefficient, " (", merged_reg$"Std. Error", ")")

merged_reg <- merged_reg %>% select(-"Std. Error")

names(merged_reg) <- c('p','c',"pv","dv")

merged_reg_ideo <- merged_reg %>%
  filter(dv=='Ideological Extremity')

names(merged_reg_ideo) <- c('p','c_ideo',"pv_ideo","dv")
merged_reg_ideo <- merged_reg_ideo %>% select(-dv)

merged_reg_aff <- merged_reg %>%
  filter(dv=="Affective Polarization")

names(merged_reg_aff) <- c('p','c_aff',"pv_aff","dv")
merged_reg_aff <- merged_reg_aff %>% select(-p,-dv)

merged_df <- cbind(merged_reg_ideo,merged_reg_aff)

merged_df[[1]]<- recode(merged_df[[1]],
                         `POP` = "Populist Attitudes",
                         `EFFI` = "Internal Efficacy",
                         `EFFE`= "External Efficacy",
                         `WEAK`= "Weak Partisan (Ref: Leaner)",
                         `STRONG`=  "Strong Partisan (Ref: Leaner)",
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

border <- fp_border_default(width = 0.5)

base_line <- merged_df %>%
  flextable() %>%
  add_header_row(values = c("", "Ideological Extremity", "Affective Polarization"),
               colwidths = c(1, 2, 2)) %>%
  align(i = 1, part = "header", align = "center") %>%
  flextable::compose(i=2, j=c(2,4),
                     part = "header",
                     value = as_paragraph(
                       "Std. Coefficent (\U03B2)"
                     )
  )%>%
flextable::compose(i=2, j=c(3,5),
                   part = "header",
                   value = as_paragraph(
                     "p-value"
                   )
) %>%
  align(part = "footer", align = "right") %>%
  flextable::compose(i=2, j=c(1),
                     part = "header",
                     value = as_paragraph(
                       ""
                     )
  ) %>%
  hline_bottom(border = border) %>%
  hline_top(border = border) %>%
  add_footer_lines(values = paste("Notes: ", "N=", sample_size, ".", " All continuous variables are standardized. Std. errors in pharentesis", sep=""))

