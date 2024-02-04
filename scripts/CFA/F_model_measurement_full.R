##### FIT FULL MEASURAMENT MODEL USING MPLUS #####
# Results reported in the Appendix

source(here("scripts","data_cleaning.r"))

variable_to_model_mplus <- c(
  # IDEOLOGICAL EXTREMITY
  "spending_extr",
  "defence_extr",
  "medical_extr",
  "garant_inc_extr",
  "blacks_extr",
  "envir_extr",
  "affirm_extr",

  #KNOWLEDGE
  "V161513",
  "V161514",
  "V161515",
  "V161516",

  # POPULISM
  "V162259", # Compromise in politics is selling out on one's principles [Manicheism]
  "V162260", # Most politicians do not care about the people  [antielite/peoplecentr]
  "V162262", # Politicians are the main problem in the U.S.  [antielite]
  "V162264", # People not politicians should make most important policy decisions  [peoplecentr]
  "V162265", # Most politicians only care about interests of rich and powerful [antielite]
  "V162267",  # The will of the majority should always prevail   [Manicheism]

  "V162256",  #interest
  "V162257",  #follow_politics

  "V162215", # Publ officials don’t care what people think
  "V162216", # Have no say about what govt does
  "V162217", # Politics/govt too complicated to understand
  "V162218", # Good understanding of political issues

  # WEIGHTS
  "V160102",
  # SAMPLING CLUSTER
  "V160201"

)

data_mplus_subset <- data_mplus[variable_to_model_mplus]

model <- "

  KN BY V161513;
  KN BY V161514;
  KN BY V161515;
  KN BY V161516;

  ! IRT parametrization (intercept fixed to 1)
  V161513@1;
  V161514@1;
  V161515@1;
  V161516@1;

  ! allow residuals correlation
  ! to improve measurement model
  V162259 WITH V162267;
  V162260 WITH V162264;
  V162262 WITH V162265;

  EFFE BY V162215;
  EFFE BY V162216;

  EFFI BY V162217;
  EFFI BY V162218;

  pol_int BY V162256;
  pol_int BY V162257;

  ! allow residuals correlation
  ! to improve measurement model
  V162217  WITH V162216;
  ! equal variance to avoid convergence issues
  V162215 (v1);
  V162216 (v1);

  pop BY V162259;
  pop BY V162260;
  pop BY V162262;
  pop BY V162264;
  pop BY V162265;
  pop BY V162267;

  V162259 WITH V162267;
  V162260 WITH V162264;
  V162262 WITH V162265;

  EXT BY spending_extr;
  EXT BY defence_extr;
  EXT BY medical_extr;
  EXT BY garant_inc_extr;
  EXT BY blacks_extr;
  EXT BY envir_extr;
  EXT BY affirm_extr;

  "

model_out <- "cfa_measurament_full.inp"

measurament <- mplusObject(
  TITLE = "BASE 3;",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       ";"),30),collapse="\n"),
  "CATEGORICAL = V161513 V161514 V161515 V161516;",
  "CLUSTER = V160201;",
  "WEIGHT=V160102;",
                   sep="\n"
  ),
  ANALYSIS = "TYPE = COMPLEX;
             PARAMETERIZATION=THETA;
             PROCESSORS = 2;

       ",
  #DEFINE=def,
  MODEL = model,
  OUTPUT = "STANDARDIZED; FSCOEFFICIENT; MODINDICES;",
  SAVEDATA="
  FILE IS measurement_fscores.dat;
  SAVE IS fscores;
  FORMAT IS free;",
  rdata=data_mplus_subset
)

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)


factor_scores <- measurament_results$results$savedata[, c("POP")]
factor_scores_as_df <- data.frame(factor_scores)
params <- measurament_results$results$parameters$std.standardized
## loadings ##
loadings <- params %>% filter(str_detect(paramHeader, ".BY"))

files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "CFA", "mplus_out"),overwrite = TRUE) }



