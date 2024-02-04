##### READ MPLUS .OUT FILES TO GENERATE FIGURE 5 IN THE APPENDIX #####

require(MplusAutomation)
require(dplyr)
require(here)
require(flextable)
require(ggplot2)
require(ggpubr)
library(latex2exp)

#### EXTRACT MODEL RESULTS ####

mod_to_read <- "F_APPENDIX_ext_trade.out"
all_output <- readModels(here("scripts", "appendix", "mplus_out", mod_to_read), quiet = TRUE)
ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)

ci_90_ext_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"TRADE_EX.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_ext_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"TRADE_EX.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_ext_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"TRADE_EX.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_ext_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"TRADE_EX.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_ext_rep_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"TRADE_EX.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_ext_rep <- data.frame(est=est_ext_rep_stdy,low_90=ci_90_ext_rep_stdy_low,up_90=ci_90_ext_rep_stdy_up,low_95=ci_95_ext_rep_stdy_low,up_95=ci_95_ext_rep_stdy_up,group="Free Trade")

plot_ideological_data <- rbind(df_ext_rep)

plot_ideological_data <- plot_ideological_data %>% mutate_at(vars(est, low_95,up_95), as.numeric) %>%
  mutate_at(vars(group), as.factor)

mod_to_read <- "F_APPENDIX_ext_immigrants.out"
all_output <- readModels(here("scripts", "appendix", "mplus_out", mod_to_read), quiet = TRUE)
ll <- round(all_output$summaries$LL,2)
aic <-  round(all_output$summaries$AIC,2)
bic <-  round(all_output$summaries$BIC,2)
sample_size <- round(sum(all_output$class_counts$modelEstimated$count), 0)

ci_90_ext_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"IMM3_EXT.")) %>%
  filter(param=="POP")  %>% select(low5) %>% unlist() %>% array()

ci_90_ext_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"IMM3_EXT.")) %>%
  filter(param=="POP")  %>% select(up5) %>% unlist() %>% array()

ci_95_ext_rep_stdy_low <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"IMM3_EXT.")) %>%
  filter(param=="POP")  %>% select(low2.5) %>% unlist() %>% array()

ci_95_ext_rep_stdy_up <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"IMM3_EXT.")) %>%
  filter(param=="POP")  %>% select(up2.5) %>% unlist() %>% array()

est_ext_rep_stdy <- all_output$parameters$ci.stdyx.standardized %>% filter(LatentClass==2)  %>% filter(str_detect(paramHeader,".ON")) %>%
  filter(str_detect(paramHeader,"IMM3_EXT.")) %>%
  filter(param=="POP")  %>% select(est) %>% unlist() %>% array()

df_ext_rep_imm <- data.frame(est=est_ext_rep_stdy,low_90=ci_90_ext_rep_stdy_low,up_90=ci_90_ext_rep_stdy_up,low_95=ci_95_ext_rep_stdy_low,up_95=ci_95_ext_rep_stdy_up,group="Immigration (Cultural)")

plot_ideological_data <- rbind(df_ext_rep,df_ext_rep_imm)

plot_ideological_data <- plot_ideological_data %>% mutate_at(vars(est, low_95,up_95), as.numeric) %>%
  mutate_at(vars(group), as.factor)


ideological_comparison <- ggplot(plot_ideological_data, aes(x=group)) +
  geom_errorbar(aes(ymin=low_95, ymax=up_95, y=est),linetype=2,width=0.07,
                size=0.6,colour="grey60") +
  geom_errorbar(aes(ymin=low_90, ymax=up_90, y=est),linetype=1,width=0.07,
                size=0.6,colour="black") +
  geom_pointrange(aes(ymin=low_90, ymax=up_90, y=est),linetype=1) +
  geom_hline(yintercept = 0,linetype=2, size=0.1) +
  coord_flip() +
  ylab(TeX("Standardized coefficent ($\\beta$) of populist attitudes on ideological extremity (90% and 95% CIs)")) +
  xlab("Attitudinal Question") +
  theme_classic()

ggsave(here("figures", "APPENDIX_ext_trade_immigration.png"),width=990*3,height=250*3, dpi=300,  units = "px")
