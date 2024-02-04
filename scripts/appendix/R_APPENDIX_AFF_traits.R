##### READ MPLUS .OUT FILES TO GENERATE FIGURE 8 IN THE APPENDIX #####

require(MplusAutomation)
require(dplyr)
require(here)
require(flextable)
require(ggplot2)
require(ggpubr)
library(latex2exp)
library(stringr)

#### EXTRACT MODEL RESULTS ####

mod_to_read <- "F_APPENDIX_AFF_traits.out"
all_output <- readModels(here("scripts", "appendix", "mplus_out", mod_to_read), quiet = TRUE)
ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)
label_figure <- paste("N=", sample_size,"\n", "Log-likelihood=", ll,"\n", "AIC=", aic,"\n", "BIC=", bic,"\n", sep="")

####----Affective Polarization plot----####

ci_90_aff_dem_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_aff_dem_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_aff_dem_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_aff_dem_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_aff_dem_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==1)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_aff_dem <- data.frame(est=est_aff_dem_stdy,low_90=ci_90_aff_dem_stdy_low,up_90=ci_90_aff_dem_stdy_up,low_95=ci_95_aff_dem_stdy_low,up_95=ci_95_aff_dem_stdy_up,group="Democrats")

ci_90_aff_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_aff_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_aff_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_aff_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_aff_rep_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"AFF_T.")) %>%
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
  annotate("text", x = 0.8, y = 0.2,
           size = 3,
           hjust = 0,
           label = label_figure, parse = FALSE) +
  theme_classic()

ggsave(here("figures", "APPENDIX_affective_traits.png"),width=990*3,height=250*3, dpi=300,  units = "px")

