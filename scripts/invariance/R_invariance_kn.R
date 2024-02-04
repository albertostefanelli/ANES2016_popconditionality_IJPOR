##### FIT INVARIANCERE MODEL FOR POLITICAL KNOWLEDGE USING MPLUS #####
# Results reported in the Appendix
library(here)
library(filesstrings)
library(glue)
source(here("scripts","data_cleaning.r"))

variable_to_model_mplus <- c(
  "PID_2_DEM",
  #KNOWLEDGE
  "V161513",
  "V161514",
  "V161515",
  "V161516",
  "V160201",
  "V160102"
)


data_mplus_subset <- data_mplus_centred[data_mplus_centred$pid_3 %in% c("DEM", "REP"), variable_to_model_mplus]

model <- "
  model:
  KN BY V161513;
  KN BY V161514;
  KN BY V161515;
  KN BY V161516;

  V161513@1;
  V161514@1;
  V161515@1;
  V161516@1;

"


measurament <- mplusObject(
  TITLE = "BASE 3;",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       #"genXgen",
                                       #"skiXski",
                                       ";"),30),collapse="\n"),

                   "CATEGORICAL = V161513 V161514 V161515 V161516;,
                   CLUSTER = V160201; ",
                   "
    GROUPING = PID_2_DEM (1=rep 2=dem);
    WEIGHT=V160102;
    ",
    sep="\n"
  ),
  ANALYSIS = "TYPE = COMPLEX;
             !ESTIMATOR IS MLR;
             PARAMETERIZATION=THETA;
             !MODEL = CONFIGURAL SCALAR;
       ",
  #DEFINE=def,
  MODEL = model,
  OUTPUT = "STANDARDIZED;FSCOEFFICIENT;MODINDICES;",
  SAVEDATA="",
  rdata=data_mplus_subset
)

model_out <- "invariance_kn.inp"

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)




files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "invariance", "mplus_out"), overwrite = TRUE) }
