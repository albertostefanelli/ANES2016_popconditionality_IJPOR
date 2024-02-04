##### FIT INVARIANCE MODEL FOR IDEOLOGICAL EXTREMISIM USING MPLUS #####
# Results reported in the Appendix

library(here)
library(filesstrings)
library(glue)
source(here("scripts","data_cleaning.r"))


variable_to_model_mplus <- c(
  "PID_2_DEM",
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

data_mplus_subset <- data_mplus_centred[variable_to_model_mplus]


model <- "
  EXT BY spending_extr;
  EXT BY defence_extr;
  EXT BY medical_extr;
  EXT BY garant_inc_extr;
  EXT BY blacks_extr;
  EXT BY envir_extr;
  EXT BY affirm_extr;

"

measurament <- mplusObject(
  TITLE = "BASE 3;",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       #"genXgen",
                                       #"skiXski",
                                       ";"),30),collapse="\n"),

                   "CLUSTER = V160201; ",
                   "
    GROUPING = PID_2_DEM (1=rep 2=dem);
    WEIGHT=V160102;
    ",
    sep="\n"
  ),
  ANALYSIS = "TYPE = COMPLEX;
             !INTEGRATION=2
             ESTIMATOR IS MLR;
             MODEL = CONFIGURAL METRIC SCALAR;
             !ESTIMATOR = BAYES;
             PROCESSORS = 2;
             !BITERATIONS = 300000;
             !ALGORITHM=INTEGRATION;
             !FBITERATIONS = 300000;
             !THIN = 2;
             !ITERATIONS = (100000)
       ",
  #DEFINE=def,
  MODEL = model,
  OUTPUT = "STANDARDIZED;FSCOEFFICIENT;MODINDICES;",
  SAVEDATA="
  FILE IS measurement_fscores.dat;
  SAVE IS fscores;
  FORMAT IS free;",
  rdata=data_mplus_subset
)

model_out <- "invariance_ext.inp"

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)


files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "invariance", "mplus_out"),overwrite = TRUE) }


