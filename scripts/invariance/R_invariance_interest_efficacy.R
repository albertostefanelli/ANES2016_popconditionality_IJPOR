##### FIT INVARIANCE MODEL FOR POLITICAL INTEREST, EXTERNAL EFFICACY, AND INTERNAL EFFICACY USING MPLUS #####
# Results reported in the Appendix

library(here)
library(filesstrings)
library(glue)
source(here("scripts","data_cleaning.r"))


variable_to_model_mplus <- c(
  "PID_2_DEM",
  # Political Interest
  "V162256",  #interest
  "V162257",  #follow_politics
  # External efficacy
  "V162215", # Publ officials don’t care what people think
  "V162216", # Have no say about what govt does
  # Internal efficacy
  "V162217", # Politics/govt too complicated to understand
  "V162218", #  Good understanding of political issues
  "V160201",
  "V160102"
)


data_mplus_subset <- data_mplus_centred[variable_to_model_mplus]
summary(data_mplus_subset)

model <- "

  ! political interest
  pol_int BY V162256;
  pol_int BY V162257;

  ! external efficacy
  EFFE BY V162215;
  EFFE BY V162216;

  ! internal  efficacy

  EFFI BY V162217;
  EFFI BY V162218;
  ! allow residual covariance
  V162217  WITH V162216;
  ! same variance to avoid convergence issues
  V162215 (v1);
  V162216 (v1);

"


measurament <- mplusObject(
  TITLE = "BASE 3;",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       ";"),30),collapse="\n"),

                   "CLUSTER = V160201; ",
                   "
    GROUPING = PID_2_DEM (1=rep 2=dem);
    WEIGHT=V160102;
    ",
    sep="\n"
  ),
  ANALYSIS = "TYPE = COMPLEX;
             ESTIMATOR IS MLR;
             MODEL = CONFIGURAL METRIC SCALAR;
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

model_out <- "invariance_politics.inp"

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)


files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "invariance", "mplus_out"), overwrite = TRUE) }
