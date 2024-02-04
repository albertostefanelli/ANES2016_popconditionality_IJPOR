##### READ MPLUS .OUT FILES TO GENERATE FIGURE 11 & 12 IN THE APPENDIX #####

require(MplusAutomation)
require(dplyr)
require(here)
require(flextable)
require(ggplot2)
require(ggpubr)
library(latex2exp)

#### EXTRACT MODEL RESULTS ####

mod_to_read <- "F_APPENDIX_3item.out"
all_output <- readModels(here("scripts", "appendix", "mplus_out", mod_to_read), quiet = TRUE)
ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)

#### PLOTS ####

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

df_ext_dem <- data.frame(est=est_ext_dem_stdy,low_90=ci_90_ext_dem_stdy_low,up_90=ci_90_ext_dem_stdy_up,low_95=ci_95_ext_dem_stdy_low,up_95=ci_95_ext_dem_stdy_up,group="Democrats")

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
diff_p <- ifelse(slope_diff_aff$pval<0.05, "0.05", "0.10")

ideological_comparison <- ggplot(plot_ideological_data, aes(x=group)) +
  geom_errorbar(aes(ymin=low_95, ymax=up_95, y=est),linetype=2,width=0.07,
                size=0.6,colour="grey60") +
  geom_errorbar(aes(ymin=low_90, ymax=up_90, y=est),linetype=1,width=0.07,
                size=0.6,colour="black") +
  geom_pointrange(aes(ymin=low_90, ymax=up_90, y=est),linetype=1) +
  geom_hline(yintercept = 0,linetype=2, size=0.1) +
  geom_bracket(
    xmin = "Democrats", xmax = "Republicans", y.position = 0.41,
    label = paste0("list(hat(beta)==", diff_slope, "~italic(p)>=", diff_p,")"), type = "expression",
    tip.length = c(0.01, 0.01), coord.flip=TRUE
  ) + coord_flip() +
  ylab(TeX("Standardized coefficent ($\\beta$) of populist attitudes on ideological extremity (90% and 95% CIs)")) +
  xlab("Party Identity") +
  theme_classic()

ggsave(here("figures", "APPENDIX_3item_ideological.png"),width=990*3,height=250*3, dpi=300,  units = "px")

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
diff_p <- ifelse(slope_diff_aff$pval<0.05, "0.05", "0.05")


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


ggsave(here("figures", "APPENDIX_3item_affective.png"),width=990*3,height=250*3, dpi=300,  units = "px")

