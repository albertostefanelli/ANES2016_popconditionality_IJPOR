##### READ MPLUS .OUT FILES TO GENERATE FIGURE 11 IN THE APPENDIX #####

library(here)
library(filesstrings)
library(glue)
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
  "V162259",    # Compromise in politics is selling out on one's principles [Manicheism]
  "V162260",    # Most politicians do not care about the people [antielite/peoplecentr]
  "V162262",    # Politicians are the main problem in the U.S.  [antielite]
  "V162264",    # People not politicians should make most important policy decisions  [peoplecentr]
  "V162265",    # Most politicians only care about interests of rich and powerful [antielite]
  "V162267",    # The will of the majority should always prevail  [Manicheism]

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
  "pid_3",
  # "WEAK",
  # "STRONG",

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

data_mplus_subset <- data_mplus_centred[variable_to_model_mplus]
data_mplus_subset$pid_3 <- relevel(data_mplus_subset$pid_3, ref="DEM")

table(data_mplus_subset$pid_3)

model <- paste(
  "
  %overall%
  KN BY V161513;
  KN BY V161514;
  KN BY V161515;
  KN BY V161516;


  pop BY V162259;
  pop BY V162260;
  pop BY V162262;
  pop BY V162264;
  pop BY V162265;
  pop BY V162267;

  V162259 WITH V162267;
  V162260 WITH V162264;
  V162262 WITH V162265;

  pol_int BY V162256;
  pol_int BY V162257;

  EFFE BY V162215;
  EFFE BY V162216;

  EFFI BY V162217;
  EFFI BY V162218;

  V162217 WITH V162216;

  EXT BY spending_extr;
  EXT BY defence_extr;
  EXT BY medical_extr;
  EXT BY garant_inc_extr;
  EXT BY blacks_extr;
  EXT BY envir_extr;
  EXT BY affirm_extr;


  wagner_aff WITH EXT;

  ",
  "EXT","ON POP
  EFFI      ! internal efficacy
  EFFE      ! external efficacy
  V161241   ! religiosity importance yes no
  pol_int   ! pol interest
  KN        ! knowledge
  perc_pol  ! percieved polarization
  id_iden7  !ideological identity strength
  bernie    ! voted for bernie
  V161270   ! education
  V161361x  ! income
  V161267   ! age
  FEM       ! gender
  !!!! RACE !!!!
  BLACK
  ASIAN
  HISP
  OTHERR;


  ",

  "wagner_aff","ON POP
  EFFI      ! internal efficacy
  EFFE      ! external efficacy
  V161241   ! religiosity importance yes no
  pol_int   ! pol interest
  KN        ! knowledge
  perc_pol  ! percieved polarization
  id_iden7  ! ideological identity strength
  bernie    ! voted for bernie
  V161270   ! education
  V161361x  ! income
  V161267   ! age
  FEM       ! gender
  !!!! RACE !!!!
  BLACK
  ASIAN
  HISP
  OTHERR;

  !!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!! DEMOCRATS !!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!

  %pid#1%

  !! EXTREMITY !!

  [EXT] (extb0g1);         ! free intercept
  EXT (vareg1);            ! free residual

  !!! freely estimated slopes !!!
  EXT ON POP (extb1g1);
  EXT ON bernie (extb2g1); !fixed

  !! AFFECTIVE !!

  [wagner_aff] (affb0g1);  ! free intercept
  wagner_aff (varag1);     ! free residual

  !!! freely estimated slopes !!!
  wagner_aff ON POP (affb1g1);
  wagner_aff ON bernie (affb2g1);

  wagner_aff WITH EXT (covg1);


  EXT ON EFFI (exts01g1) ! internal efficacy
  EFFE        (exts02g1) ! external efficacy
  V161241     (exts03g1) ! religiosity importance yes no
  pol_int     (exts04g1) ! pol interest
  KN          (exts05g1) ! knowledge
  perc_pol    (exts06g1) ! percieved polarization
  id_iden7    (exts07g1) ! ideological identity strength
  V161270     (exts11g1) ! education
  V161361x    (exts12g1) ! income
  V161267     (exts13g1) ! age
  FEM         (exts14g1) ! gender
  BLACK       (exts15g1)
  ASIAN       (exts16g1)
  HISP        (exts17g1)
  OTHERR      (exts18g1);

  wagner_aff ON EFFI (affs01g1) ! internal efficacy
  EFFE        (affs02g1)        ! external efficacy
  V161241     (affs03g1)        ! religiosity importance yes no
  pol_int     (affs04g1)        ! pol interest
  KN          (affs05g1)        ! knowledge
  perc_pol    (affs06g1)        ! percieved polarization
  id_iden7    (affs07g1)        ! ideological identity strength
  V161270     (affs11g1)        ! education
  V161361x    (affs12g1)        ! income
  V161267     (affs13g1)        ! age
  FEM         (affs14g1)        ! gender
  BLACK       (affs15g1)
  ASIAN       (affs16g1)
  HISP        (affs17g1)
  OTHERR      (affs18g1);


  !!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!! INDEPENDENTS !!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!

  %pid#2%

  !! EXTREMITY !!

  [EXT] (extb0g2);         ! free intercept
  EXT (vareg2);            ! free residual

  !!! freely estimated slopes !!!
  EXT ON POP (extb1g2);
  EXT ON bernie (extb2g1); !fixed

  !! AFFECTIVE !!

  [wagner_aff] (affb0g2);  ! free intercept
  wagner_aff (varag2);     ! free residual

  !!! freely estimated slopes !!!
  wagner_aff ON POP (affb1g2);
  wagner_aff ON bernie (affb2g2);

  wagner_aff WITH EXT (covg2);


  EXT ON EFFI (exts01g1) ! internal efficacy
  EFFE        (exts02g1) ! external efficacy
  V161241     (exts03g1) ! religiosity importance yes no
  pol_int     (exts04g1) ! pol interest
  KN          (exts05g1) ! knowledge
  perc_pol    (exts06g1) ! percieved polarization
  id_iden7    (exts07g1) ! ideological identity strength
  V161270     (exts11g1) ! education
  V161361x    (exts12g1) ! income
  V161267     (exts13g1) ! age
  FEM         (exts14g1) ! gender
  BLACK       (exts15g1)
  ASIAN       (exts16g1)
  HISP        (exts17g1)
  OTHERR      (exts18g1);

  wagner_aff ON EFFI (affs01g1) ! internal efficacy
  EFFE        (affs02g1)        ! external efficacy
  V161241     (affs03g1)        ! religiosity importance yes no
  pol_int     (affs04g1)        ! pol interest
  KN          (affs05g1)        ! knowledge
  perc_pol    (affs06g1)        ! percieved polarization
  id_iden7    (affs07g1)        ! ideological identity strength
  V161270     (affs11g1)        ! education
  V161361x    (affs12g1)        ! income
  V161267     (affs13g1)        ! age
  FEM         (affs14g1)        ! gender
  BLACK       (affs15g1)
  ASIAN       (affs16g1)
  HISP        (affs17g1)
  OTHERR      (affs18g1);


  !!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!! REPUBLICANS  !!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!

  %pid#3%

  !! EXTREMITY !!

  [EXT] (extb0g3);        ! free intercept
  [spending_extr@0];      ! need to fix observed 1 mean manifest to zero to estimte intercept
  EXT (vareg3);           ! free residual

  !!! freely estimated slopes !!!
  EXT ON POP (extb1g3);
  EXT ON bernie (extb2g1);! fixed

  !! AFFECTIVE !!

  [wagner_aff] (affb0g3); ! free intercept
  wagner_aff (varag3);    ! free residual

  !!! freely estimated slopes !!!
  wagner_aff ON POP (affb1g3);
  wagner_aff ON bernie (affb2g1);

  wagner_aff WITH EXT (covg3);

  EXT ON EFFI (exts01g1) ! internal efficacy
  EFFE        (exts02g1) ! external efficacy
  V161241     (exts03g1) ! religiosity importance yes no
  pol_int     (exts04g1) ! pol interest
  KN          (exts05g1) ! knowledge
  perc_pol    (exts06g1) ! percieved polarization
  id_iden7    (exts07g1) ! ideological identity strength
  V161270     (exts11g1) ! education
  V161361x    (exts12g1) ! income
  V161267     (exts13g1) ! age
  FEM         (exts14g1) ! gender
  BLACK       (exts15g1)
  ASIAN       (exts16g1)
  HISP        (exts17g1)
  OTHERR      (exts18g1);

  wagner_aff ON EFFI (affs01g1) ! internal efficacy
  EFFE        (affs02g1)        ! external efficacy
  V161241     (affs03g1)        ! religiosity importance yes no
  pol_int     (affs04g1)        ! pol interest
  KN          (affs05g1)        ! knowledge
  perc_pol    (affs06g1)        ! percieved polarization
  id_iden7    (affs07g1)        ! ideological identity strength
  V161270     (affs11g1)        ! education
  V161361x    (affs12g1)        ! income
  V161267     (affs13g1)        ! age
  FEM         (affs14g1)        ! gender
  BLACK       (affs15g1)
  ASIAN       (affs16g1)
  HISP        (affs17g1)
  OTHERR      (affs18g1);

"

)


model_out <- "F_APPENDIX_independents.inp"

model_syntax <- mplusObject(
  TITLE = gsub(".inp", "", model_out),
  VARIABLE = paste(paste(strwrap(paste("USEVARIABLES = ",paste(names(data_mplus_subset), collapse=" "),
                                       ";"),30),collapse="\n"),
                   "CATEGORICAL = V161513 V161514 V161515 V161516;",
                   "CLUSTER = V160201;",
                   "class = pid (3);",
                   "KNOWNCLASS = pid (pid_3=1 pid_3=2 pid_3=3);",
                   "WEIGHT=V160102;",
                   sep="\n"
  ),
  ANALYSIS = "TYPE = MIXTURE COMPLEX;
             ESTIMATOR IS MLR;
             !MODEL = CONFIGURAL METRIC SCALAR;
             ALGORITHM=INTEGRATION;
             !ITER=300;
             PROCESSORS = 2;
       ",
  MODEL = model,
  MODELCONSTRAINT= "",
  OUTPUT = "STANDARDIZED; CINTERVAL;",
  SAVEDATA="
  !COVARIANCE = cov.dat; TECH3 =tech3.dat;
  !FILE IS testid_fscores.dat;
  !SAVE IS fscores;
  !FORMAT IS free;",

  rdata=data_mplus_subset
)


pop_full_sem <- mplusModeler(model_syntax,
                             modelout = model_out,
                             run=1L,
                             Mplus_command = "/Applications/Mplus_mac/mplus",
                             hashfilename = FALSE)

files <- data.frame(names = list.files(path = here(), full.names = FALSE))
f_rad <- files %>% dplyr::filter(., grepl(gsub(".inp","",model_out), names))
for (i in 1:length(f_rad)) { file.move(glue(here("{f_rad[,i]}")), here("scripts", "appendix", "mplus_out"), overwrite = TRUE) }

