##### FIT PARTIAL SCALAR AND SCALAR INVARIANCERE MODEL FOR IDEOLOGICAL EXTREMISIM USING MPLUS #####
# Results reported in the Appendix

library(here)
library(filesstrings)
library(glue)
source(here("scripts","data_cleaning.r"))

#### PARTIAL INVARIANCE ####
# FULL SCALAR INVARIANCE RESULTS
# CFI: 0.758
# SRMR: 0.071
# RMSEA: 0.063
# Modification Indices
#
# [ MEDICAL_ ]                      13.100     0.116      0.116        0.102
# [ GARANT_I ]                      22.207    -0.160     -0.160       -0.147
# [ ENVIR_EX ]                     193.170     0.391      0.391        0.361
# [ AFFIRM_E ]                      23.916    -0.158     -0.158       -0.122

# Intercepts set to be estimated across the two groups
# [medical_extr]
# [envir_extr]
# [affirm_extr]

variable_to_model_mplus <- c(
  "pid_3",
  "spending_extr",
  "defence_extr",
  "medical_extr",
  "garant_inc_extr",
  "blacks_extr",
  "envir_extr",
  "affirm_extr",
  "V160201",
  "V160102"
)


data_mplus_subset <- data_mplus[variable_to_model_mplus]
data_mplus_subset <- data_mplus_subset  %>% filter(pid_3 %in% c("DEM", "REP")) %>% mutate_at(vars(pid_3), factor)
data_mplus_subset$pid_3 <- relevel(data_mplus_subset$pid_3, ref="DEM")

model <- "
  MODEL:
  EXT BY spending_extr;
  EXT BY defence_extr;
  EXT BY medical_extr;
  EXT BY garant_inc_extr;
  EXT BY blacks_extr;
  EXT BY envir_extr;
  EXT BY affirm_extr;

  MODEL REP:
  [spending_extr] (i1);
  [defence_extr] (i2);
  [medical_extr] (i3rep);
  [garant_inc_extr@0] (i4);
  [blacks_extr] (i5);
  [envir_extr] (i6rep);
  [affirm_extr] (i7rep);
  [EXT](meanrep);
  EXT;

  MODEL DEM:
  [spending_extr] (i1);
  [defence_extr] (i2);
  [medical_extr] (i3dem);
  [garant_inc_extr] (i4);
  [blacks_extr] (i5);
  [envir_extr] (i6dem);
  [affirm_extr] (i7dem);
  [EXT](meandem);
  EXT;
"

model_out <- "scalar_invariance_ext.inp"


measurament <- mplusObject(
  TITLE = gsub(".inp", "",model_out),
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       ";"),30),collapse="\n"),
  "CLUSTER = V160201;",
  "GROUPING = PID_3 (1=rep 2=dem);",
  "WEIGHT=V160102;",
                   sep="\n"
  ),
  ANALYSIS = "TYPE = COMPLEX;
             ESTIMATOR IS MLR;
             PROCESSORS = 2;

       ",
  #DEFINE=def,
  MODEL = model,
  MODELCONSTRAINT= "
  NEW(meandiff);
  meandiff = meandem - meanrep;
  ",
  OUTPUT = "STANDARDIZED;FSCOEFFICIENT;",
  SAVEDATA="FILE IS scalar_invariance_ext_fs.dat;
  SAVE IS fscores;
  FORMAT IS free;",
  rdata=data_mplus_subset
)


measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)




files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "invariance", "mplus_out"),overwrite = TRUE) }


# Chi-square test is still significant (meaning we do not reach invariance)
# however, the test is very conservative
# LL Metric (more complex, all intercept free): 33534.457
# LL Partial scalar (less complex, some intercept set the same): 33540.209
(teststat <- -2 * (as.numeric(-33540.209) - as.numeric(-33534.457)))
(p.val <- pchisq(teststat, df = 3, lower.tail = FALSE))
# so we follow Cheng (2007), and we reach scalar invariance
delta_CFI <- 0.967 - 0.971
delta_RMSA <- 0.025 - 0.024
delta_SRMR <- 0.034 - 0.031

if (delta_CFI <=  -0.010 & (delta_RMSA >= 0.015 | delta_SRMR >= 0.010)) {print("Non Invariance")}else{print("Invariant")}

fs <- readModels(here("scripts", "invariance", "mplus_out","scalar_invariance_ext.out"), what="savedata")$savedata %>%
  select(EXT,PID_3) %>%
  mutate_at(vars(PID_3), factor)

mu <- plyr::ddply(fs, "PID_3", summarise, grp.mean=mean(EXT))

## APPENDIX

ggplot(fs, aes(x=EXT, color=PID_3, fill=PID_3)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.3)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=PID_3),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"),labels = c("Republican", "Democrat"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),labels = c("Republican", "Democrat"))+
  theme_classic() +
  theme(legend.position="top")+
  labs(title="", x="Ideological Extremity", y = "Density",fill="PID", color="PID") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(here("figures", "APPENDIX_scalar_invariance.png"), width=990*3,height=250*7, dpi=300,  units = "px")

