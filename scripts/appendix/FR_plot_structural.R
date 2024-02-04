##### FIT ONLY THE MEASUREMENT MODEL WITH MPLUS TO GENERATE FIGURE 1 IN THE APPENDIX (Graphical representation of the structural model) #####

library(here)
library(filesstrings)
library(glue)
library(semptools)
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

  # AFFECTIVE POLARIZATION
  "wagner_aff",

  # POLITICAL KNOWLEDGE
  "V161513",
  "V161514",
  "V161515",
  "V161516",

  # POPULISM
  "V162259",    # Compromise in politics is selling out on one's principles [X] [Manicheism]
  "V162260",    # Most politicians do not care about the people [X] [antielite/peoplecentr]
  "V162262",    # Politicians are the main problem in the U.S.  [antielite]
  "V162264",    # People not politicians should make most important policy decisions  [peoplecentr]
  "V162265",    # Most politicians only care about interests of rich and powerful [antielite]
  "V162267",    # The will of the majority should always prevail [X]  [Manicheism]

  # CONTROLS
  # DEMO
  "V161270",    # edu
  "V161361x",   # income
  "V161267",    # age
  "V161241",    # religious importance

  # GENDER (Male)
  "FEM",        # Female

  # RACE (Ref: WHITE)
  "BLACK",
  "ASIAN",
  "HISP",
  "OTHERR",
  "bernie",

  # PERCIEVED PARTY POLARIZATION
  "perc_pol",

  # L-C STRENGHT IDEOLOGICAL IDENTITY
  "id_iden7",

  # PID + STRENGHT
  #"pid_3", # excluded for plotting purposes
  "WEAK",
  "STRONG",

  # POLITICAL INTEREST
  "V162256",     #interest
  "V162257",     #follow_politics

  # EXTERNAL EFFICACY
  "V162215",     # Publ officials don’t care what people think
  "V162216",     # Have no say about what govt does

  # INTERNAL EFFICACY
  "V162217",     # Politics/govt too complicated to understand
  "V162218",     # Good understanding of political issues

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

  V161513@1;
  V161514@1;
  V161515@1;
  V161516@1;

  EFFE BY V162215;
  EFFE BY V162216;

  EFFI BY V162217;
  EFFI BY V162218;

  pol_int BY V162256;
  pol_int BY V162257;

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

  EXT ON POP
  WEAK
  STRONG
  EFFI
  EFFE
  V161241
  pol_int
  KN
  perc_pol
  id_iden7
  bernie
  V161270
  V161361x
  V161267
  FEM
  !!!! RACE !!!!
  BLACK
  ASIAN
  HISP
  OTHERR;

  wagner_aff ON POP
  WEAK
  STRONG
  EFFI
  EFFE
  V161241
  pol_int
  KN
  perc_pol
  id_iden7
  bernie
  V161270
  V161361x
  V161267
  FEM
  !!!! RACE !!!!
  BLACK
  ASIAN
  HISP
  OTHERR;

  "

measurament <- mplusObject(
  TITLE = "",
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       ";"),30),collapse="\n"),
                   "CATEGORICAL = V161513 V161514 V161515 V161516;",
                   "CLUSTER = V160201;",
                   "WEIGHT=V160102;",
                   sep="\n"
  ),
  ANALYSIS = "TYPE = COMPLEX;
             PARAMETERIZATION=THETA;
             PROCESSORS = 2;",
  MODEL = model,
  PLOT="TYPE = PLOT2;",
  rdata=data_mplus_subset
)

model_out <- "plot_structural.inp"

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)


files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "appendix", "mplus_out"),overwrite = TRUE) }

pm_no_covs <- semptools::drop_nodes(
  object = semPlotModel(here("scripts", "appendix", "mplus_out","plot_structural.out")),
  nodes = c("V161513",
            "V161514",
            "V161515",
            "V161516",

            "V162215",
            "V162216",

            "V162217",
            "V162218",

            "V162256",
            "V162257",

            "V162259",
            "V162260",
            "V162262",
            "V162264",
            "V162265",
            "V162267",

            "SPENDING",
            "DEFENCE_",
            "MEDICAL_",
            "GARANT_I",
            "BLACKS_E",
            "ENVIR_EX",
            "AFFIRM_E"
  )
)


semPlot::semPaths(pm_no_covs,
                  curvePivot = TRUE,
                  intercepts = FALSE,
                  layout = "tree2",
                  #"eq",
                  ask = FALSE,
                  thresholds=FALSE,
                  mar = c(6, 1, 6, 1),
                  label.prop = 0.5,
                  curve = 0.8,
                  edge.label.cex = 1,
                  nCharNodes=15,
                  nCharEdges=1,
                  optimizeLatRes=FALSE,
                  #centerLevels=TRUE,
                  as.expression = c("nodes", "edges"),
                  label.norm="OOOO",
                  filetype="png",
                  filename = here("figures", "plot_structural"),
                  label.font= 4,
                  nodeLabels =c(
                    "Affective\nPolarization",

                    "Weak\nPartisan\n(Ref: Leaner)",
                    "Strong\nPartisan\n(Ref: Leaner)",
                    "Importance\nReligion\n(Ref: No) ",
                    "Perceived\nPolarization",
                    "Ideological\nIdentity",
                    "Voted\nSanders\n(Ref: No)",
                    "Education",
                    "Income",
                    "Age",
                    "Female\n(ref: Male)",
                    "African-\nAmericans\n(ref: White)",
                    "Asian\n(ref: White)",
                    "Hispanic\n(ref: White)",
                    "Others\n(ref: White)",

                    "Populist\nAttitudes",
                    "Internal\nEfficacy",
                    "External\nEfficacy",
                    "Political\nInterest",
                    "Political\nKnowledge",
                    "Ideological\nExtremity"
                    ),
                  #label.cex=c(10,10),
                  label.scale=FALSE,
                  residuals=FALSE,
                  sizeMan = 6,
                  sizeLat = 6,
                  width = 25,
                  height =9
)

