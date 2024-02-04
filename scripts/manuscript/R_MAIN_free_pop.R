#### READ MAIN TEXT DEM/REP MODEL FROM MPLUS .OUT FILES ####

require(MplusAutomation)
require(dplyr)
require(here)
require(flextable)
require(ggplot2)
require(ggpubr)
library(latex2exp)

#### EXTRACT MODEL RESULTS ####

mod_to_read <- "F_MAIN_free_pop.out"
all_output <- readModels(here("scripts", "manuscript", "mplus_out", mod_to_read), quiet = TRUE)
ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)

#### MAIN TEXT - PLOTS ####

###----Ideological Polarization plot-----###

ci_90_ext_dem_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_ext_dem_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_ext_dem_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_ext_dem_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_ext_dem_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_ext_dem <- data.frame(est=est_ext_dem_stdy, low_90=ci_90_ext_dem_stdy_low, up_90=ci_90_ext_dem_stdy_up,low_95=ci_95_ext_dem_stdy_low,up_95=ci_95_ext_dem_stdy_up,group="Democrats")

ci_90_ext_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_ext_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_ext_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_ext_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_ext_rep_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"EXT.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_ext_rep <- data.frame(est=est_ext_rep_stdy,low_90=ci_90_ext_rep_stdy_low,up_90=ci_90_ext_rep_stdy_up,low_95=ci_95_ext_rep_stdy_low,up_95=ci_95_ext_rep_stdy_up,group="Republicans")

plot_ideological_data <- rbind(df_ext_rep,df_ext_dem)

plot_ideological_data <- plot_ideological_data %>% mutate_at(vars(est, low_95,up_95), as.numeric) %>%
  mutate_at(vars(group), as.factor)

diff_slope <- round(plot_ideological_data[1,"est"] - plot_ideological_data[2,"est"],2)
slope_diff_aff <- paramExtract(all_output$parameters$unstandardized, "new") %>% filter(param=="EXTDEMRE") %>% select(est, se, est_se,pval)
diff_p <- ifelse(slope_diff_aff$pval<0.05, "0.05", "0.1")

ideological_comparison <- ggplot(plot_ideological_data, aes(x=group)) +
  geom_errorbar(aes(ymin=low_95, ymax=up_95, y=est),linetype=2,width=0.07,
                size=0.6,colour="grey60") +
  geom_errorbar(aes(ymin=low_90, ymax=up_90, y=est),linetype=1,width=0.07,
                size=0.6,colour="black") +
  geom_pointrange(aes(ymin=low_90, ymax=up_90, y=est),linetype=1) +
  geom_hline(yintercept = 0,linetype=2, size=0.1) +
  geom_bracket(
    xmin = "Democrats", xmax = "Republicans", y.position = 0.33,
    label = paste0("list(hat(beta)==", diff_slope, "~italic(p)<=", diff_p,")"), type = "expression",
    tip.length = c(0.01, 0.01), coord.flip=TRUE
  ) + coord_flip() +
  ylab(TeX("Standardized coefficent ($\\beta$) of populist attitudes on ideological extremity (90% and 95% CIs)")) +
  xlab("Party Identity") +
  theme_classic()

ggsave(here("figures", "M_ideological_comparison.png"),width=990*3,height=250*3, dpi=300,  units = "px")

####----Affective Polarization plot----####

ci_90_aff_dem_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_aff_dem_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_aff_dem_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_aff_dem_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_aff_dem_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_aff_dem <- data.frame(est=est_aff_dem_stdy,low_90=ci_90_aff_dem_stdy_low,up_90=ci_90_aff_dem_stdy_up,low_95=ci_95_aff_dem_stdy_low,up_95=ci_95_aff_dem_stdy_up,group="Democrats")

ci_90_aff_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_aff_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_aff_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_aff_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_aff_rep_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"WAGNER_A.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_aff_rep <- data.frame(est=est_aff_rep_stdy,low_90=ci_90_aff_rep_stdy_low,up_90=ci_90_aff_rep_stdy_up,low_95=ci_95_aff_rep_stdy_low,up_95=ci_95_aff_rep_stdy_up,group="Republicans")

plot_affective_data <- rbind(df_aff_rep,df_aff_dem)

plot_affective_data <- plot_affective_data %>% mutate_at(vars(est, low_95,up_95), as.numeric) %>%
  mutate_at(vars(group), as.factor)

diff_slope <- round(plot_affective_data[1,"est"] - plot_affective_data[2,"est"],2)
slope_diff_aff <- paramExtract(all_output$parameters$unstandardized, "new") %>% filter(param=="AFFDEMRE") %>% select(est, se, est_se,pval)
diff_p <- ifelse(slope_diff_aff$pval<0.05, "0.05", "0.1")


affective_comparison <- ggplot(plot_affective_data, aes(x=group)) +
  geom_errorbar(aes(ymin=low_95, ymax=up_95, y=est),linetype=2,width=0.07,
                size=0.6,colour="grey60") +
  geom_errorbar(aes(ymin=low_90, ymax=up_90, y=est),linetype=1,width=0.07,
                size=0.6,colour="black") +
  geom_pointrange(aes(ymin=low_90, ymax=up_90, y=est),linetype=1) +
  geom_hline(yintercept = 0,linetype=2, size=0.1) +
  geom_bracket(
    xmin = "Democrats", xmax = "Republicans", y.position = 0.33,
    label = paste0("list(hat(beta)==", diff_slope, "~italic(p)<=", diff_p,")"), type = "expression",
    tip.length = c(0.01, 0.01), coord.flip=TRUE
  ) + coord_flip() +
  ylab(TeX("Standardized coefficent ($\\beta$) of populist attitudes on affective polarizaiton (90% and 95% CIs)")) +
  xlab("Party Identity") +
  theme_classic()


ggsave(here("figures", "M_affective_comparison.png"),width=990*3,height=250*3, dpi=300,  units = "px")

##### APPENDIX - REGRESSION TABLES ####

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

regression_ideological_rep <- rbind(regression_ideological_c,regression_ideological_d)  %>% select(-std)

names(regression_ideological_rep) <- c("P","Coefficient","Std. Error","p-value")
regression_ideological_rep$`PID` <- 'Republicans'

merged_ideological <- rbind(regression_ideological_dem,regression_ideological_rep)

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

merged_ideological[[1]]<- recode(merged_ideological[[1]],
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

merged_ideological_d <- merged_ideological %>%
  filter(dv=="Democrats")

names(merged_ideological_d) <-  c('p','c_ideo',"pv_ideo","dv")
merged_ideological_d <- merged_ideological_d %>% select(-dv)

merged_ideological_r <- merged_ideological %>%
  filter(dv=="Republicans")

names(merged_ideological_r) <- c('p','c_aff',"pv_aff","dv")
merged_ideological_r <- merged_ideological_r %>% select(-dv, -p)

merged_df <- cbind(merged_ideological_d, merged_ideological_r)

set_flextable_defaults(fonts_ignore = TRUE, line_spacing = 1, border.color="black")
border <- fp_border_default(width = 0.5)


free_ideo <- merged_df %>%
  flextable() %>%
  add_header_row(values = c("", "Democrats", "Republicans"),
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
  add_footer(p = paste("N=", sample_size, ", Log-likelihood=", ll, ", AIC=", aic, ", BIC=", bic, sep="")) %>%
  merge_at(j = 1:5, part = "footer") %>%
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


#### AFFECTIVE REGRESSION TABLE


dummy <- c("WEAK",
           "STRONG",
           "BERNIE",
           "FEM",
           "BLACK",
           "ASIAN",
           "HISP",
           "OTHERR")

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

regression_affective_rep <- rbind(regression_affective_c,regression_affective_d)  %>% select(-std)

names(regression_affective_rep) <- c("P","Coefficient","Std. Error","p-value")
regression_affective_rep$`PID` <- 'Republicans'

merged_affective <- rbind(regression_affective_dem,regression_affective_rep)

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

merged_affective$Coefficient <- ifelse(merged_affective$Coefficient<0, merged_affective$Coefficient, paste("",merged_affective$Coefficient))
merged_affective$Coefficient <- paste0(merged_affective$Coefficient, " (", merged_affective$"Std. Error", ")")

merged_affective <- merged_affective %>% select(-"Std. Error")

names(merged_affective) <- c('p','c',"pv","dv")

merged_affective[[1]]<- recode(merged_affective[[1]],
                               `POP` = "Populist Attitudes",
                               `EFFI` = "Internal Efficacy",
                               `EFFE`= "External Efficacy",
                               `WEAK`= "Weak Partisan (Ref: Leaner)",
                               `STRONG`=  "Strong Partisan (Ref: Leaner)",
                               `POL_INT`= "Political Interest",
                               `KN` = "Political Knowledge",
                               `V161241` = "Religiosity",
                               PERC_POL = "Perceived Polarization",
                               ID_IDEN7 = "Strength Ideological Identity",
                               BERNIE = "Voted for Sanders",
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

names(merged_affective_d) <-  c('p','c_ideo',"pv_ideo","dv")
merged_affective_d <- merged_affective_d %>% select(-dv)

merged_affective_r <- merged_affective %>%
  filter(dv=="Republicans")

names(merged_affective_r) <- c('p','c_aff',"pv_aff","dv")
merged_affective_r <- merged_affective_r %>% select(-dv, -p)

merged_df <- cbind(merged_affective_d, merged_affective_r)

free_aff <- merged_df %>%
  flextable() %>%
  add_header_row(values = c("", "Democrats", "Republicans"),
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
  add_footer(p = paste("N=", sample_size, ", Log-likelihood=", ll, ", AIC=", aic, ", BIC=", bic, sep="")) %>%
  merge_at(j = 1:5, part = "footer") %>%
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

