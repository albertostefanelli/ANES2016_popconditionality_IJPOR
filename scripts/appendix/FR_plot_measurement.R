##### FIT ONLY THE MEASUREMENT MODEL WITH MPLUS TO GENERATE FIGURE 2 IN THE APPENDIX (Graphical representation of the measurement model) #####

library(here)
library(filesstrings)
library(glue)
library(semptools)
source(here("scripts","data_cleaning.r"))

variable_to_model_mplus <- c(
  # EXTREMITY
  "spending_extr",
  "defence_extr",
  "medical_extr",
  "garant_inc_extr",
  "blacks_extr",
  "envir_extr",
  "affirm_extr",

  # POPULISM
  "V162259",
  "V162260",
  "V162262",
  "V162264",
  "V162265",
  "V162267",

  # POLITICAL KNOWLEDGE
  "V161513",
  "V161514",
  "V161515",
  "V161516",

  # POLITICAL INTEREST
  "V162256",
  "V162257",

  # EXTERNAL EFFICACY
  "V162215",
  "V162216",

  # INTERNAL EFFICACY
  "V162217",
  "V162218",

  # WEIGHTS
  "V160102",

  # CLUSTERING
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

  V162216 WITH V162217;

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
  OUTPUT = "STANDARDIZED; FSCOEFFICIENT; MODINDICES;",
  PLOT="TYPE = PLOT2;",
  rdata=data_mplus_subset
)

model_out <- "plot_measurament.inp"

measurament_results <- mplusModeler(measurament,
                                    modelout = model_out,
                                    run=1L,
                                    Mplus_command = "/Applications/Mplus_mac/mplus",
                                    hashfilename = FALSE)


files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "appendix", "mplus_out"),overwrite = TRUE) }


semPlot::semPaths(here("scripts", "appendix", "mplus_out", "plot_measurament.out"),curvePivot = TRUE,intercepts = FALSE,
                  #layout = "tree",
                  #"eq",
                  ask = FALSE,
                  thresholds=FALSE,
                  mar = c(6, 1, 6, 1),
                  label.prop = 0.5,
                  curve = 0.8,
                  edge.label.cex = 1,
                  nCharNodes=1,
                  nCharEdges=1,
                  optimizeLatRes=FALSE,
                  #centerLevels=TRUE,
                  as.expression = c("nodes", "edges"),
                  label.norm="OOOO",
                  filetype="png",
                  filename = here("figures", "plot_measurament"),
                  nodeLabels =c(c(
                  "V161513",
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

                  "V161178\n(folded)",
                  "V161181\n(folded)",
                  "V161184\n(folded)",
                  "V161189\n(folded)",
                  "V161198\n(folded)",
                  "V161201\n(folded)",
                  "V161204x\n(folded)",

                  "Political\nKnowledge",
                  "External\nEfficacy",
                  "Internal\nEfficacy",
                  "Political\nInterest",
                  "Populist\nAttitudes",
                  "Ideological\nExtremity"

                  )

                  ),
                  label.font= 4,
                  #label.cex=c(10,10),
                  label.scale=FALSE,
                  sizeMan = 4,
                  sizeLat = 6,
                  width = 25,
                  height =9
                  )

