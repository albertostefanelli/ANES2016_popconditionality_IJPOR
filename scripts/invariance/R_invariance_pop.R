##### FIT INVARIANCE MODEL FOR POPULISM USING MPLUS #####
# Results reported in the Appendix
library(here)
library(filesstrings)
library(glue)
source(here("scripts","data_cleaning.r"))

variable_to_model_mplus <- c(
  "PID_2_DEM",
  # POPULISM
  "V162259", # Compromise in politics is selling out on one's principles
  "V162260", # Most politicians do not care about the people
  "V162262", # Politicians are the main problem in the U.S.
  "V162264", # People not politicians should make most important policy decisions
  "V162265", # Most politicians only care about interests of rich and powerful
  "V162267", # The will of the majority should always prevail
  "V160201", # sample clustering
  "V160102"  # sampling weights
)


data_mplus_subset <- data_mplus_centred[data_mplus_centred$pid_3 %in% c("DEM", "REP"), variable_to_model_mplus]

model <- "
  pop BY V162259*;
  pop BY V162260;
  pop BY V162262;
  pop BY V162264;
  pop BY V162265;
  pop BY V162267;

  pop@1;
  V162259 WITH V162267;
  V162260 WITH V162264;
  V162262 WITH V162265;

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
  SAVEDATA="",
  rdata=data_mplus_subset
)


model_out <- "invariance_pop.inp"


measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)


files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "invariance", "mplus_out"),overwrite = TRUE) }
