##### FIT MEASURAMENT MODEL WITH ONLY POPULISM USING MPLUS #####
# Results reported in the Appendix
source(here("scripts","data_cleaning.r"))

variable_to_model_mplus <- c(
  # POPULISM
  "V162259",
  "V162260",
  "V162262",
  "V162264",
  "V162265",
  "V162267",
  "pid_3"
)

data_mplus_subset <- data_mplus[variable_to_model_mplus]
data_mplus_subset <- data_mplus_subset  %>% filter(pid_3 %in% c("DEM", "REP")) %>% mutate_at(vars(pid_3), factor)
data_mplus_subset <- data_mplus_subset |> select(-pid_3)

# Manifest Means for Table 1 MAIN TEXT
#"V162259", # Compromise in politics is selling out on one's principles
mean(data_mplus_subset$V162259, na.rm=TRUE)

#"V162260", # Most politicians do not care about the people
mean(data_mplus_subset$V162260, na.rm=TRUE)

#"V162262", # Politicians are the main problem in the U.S.
mean(data_mplus_subset$V162262, na.rm=TRUE)

#"V162264", # People not politicians should make most important policy decisions
mean(data_mplus_subset$V162264, na.rm=TRUE)

#"V162265", # Most politicians only care about interests of rich and powerful
mean(data_mplus_subset$V162265, na.rm=TRUE)

#"V162267", # The will of the majority should always prevail
mean(data_mplus_subset$V162267, na.rm=TRUE)

model <- "
  pop BY V162259;
  pop BY V162260;
  pop BY V162262;
  pop BY V162264;
  pop BY V162265;
  pop BY V162267;

  V162259 WITH V162267;
  V162260 WITH V162264;
  V162262 WITH V162265;
"

model_out <- "cfa_measurament_populism.inp"

measurament <- mplusObject(
  TITLE = "BASE 3;",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       ";"),30),collapse="\n"),
                   "!CATEGORICAL = V161513 V161514 V161515 V161516;",
                   "!CLUSTER = V160201;",
                   "!WEIGHT=V160102;",
                   sep="\n"
  ),
  ANALYSIS = "!TYPE = COMPLEX;
             !PARAMETERIZATION=THETA;
             PROCESSORS = 2;

       ",
  #DEFINE=def,
  MODEL = model,
  OUTPUT = "STANDARDIZED; FSCOEFFICIENT; MODINDICES;",
  SAVEDATA="",
  rdata=data_mplus_subset
)

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)

params <- measurament_results$results$parameters$std.standardized
## loadings ##

loadings <- params %>%filter(str_detect(paramHeader, ".BY")) %>%
  select(est)

files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "CFA", "mplus_out"),overwrite = TRUE) }
