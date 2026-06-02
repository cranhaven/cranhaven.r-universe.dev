library(HealthMarkers)

# ============================================================
# HealthMarkers -- manual / interactive tests
# Run any block with Ctrl+Enter or highlight a section and run.
# Each numbered section is self-contained.
# ============================================================

# ---- 00 shared-data ----
df <- data.frame(
  G0=5.2, I0=65, G30=8.1, I30=120, G120=6.8, I120=90,
  glucose=5.2, insulin=65,
  TC=5.4, HDL_c=1.3, TG=1.6, LDL_c=3.4, ApoB=1.1, ApoA1=1.4,
  weight=82, height=178, BMI=25.9, waist=92, hip=99,
  fat_mass=22, ALM=19.5,
  WBC=6.5, neutrophils=3.8, lymphocytes=2.0, monocytes=0.5,
  eosinophils=0.2, platelets=240,
  CRP=1.8, albumin=42, ESR=12,
  creatinine=0.95, BUN=14,
  urine_albumin=18, urine_creatinine=120, eGFR=88, UACR=15,
  ALT=28, AST=22, GGT=35, bilirubin=0.9,
  sbp=122, dbp=78, bp_sys=122, bp_dia=78,
  bp_treated=FALSE, smoker=FALSE, diabetes=FALSE,
  fev1=3.4, fvc=4.3, fev1_pct=82, mmrc=1, sixmwd=420,
  age=48, sex="M", race="white", ethnicity="Caucasian",
  FFA=0.35, rate_palmitate=1.9, rate_glycerol=2.2,
  saliva_cort1=18, saliva_cort2=12, saliva_cort3=8,
  saliva_amylase=75, saliva_glucose=5.0,
  sweat_chloride=35, sweat_Na=55, sweat_K=8, sweat_lactate=12,
  weight_before=82, weight_after=81.3, duration=60, body_surface_area=1.9,
  ferritin=80, transferrin_sat=28, total_protein=72,
  EPA=2.1, DHA=4.3, Mg=0.88,
  glycated_albumin=13, uric_acid=320, phosphate=1.1,
  calcium=2.35, Na=140, K=4.1, Cl=101, HCO3=25, Tyr=65, Phe=52,
  VitD=55, VitD_ref_mean=50, VitD_ref_sd=10,
  B12=350, Folate=18,
  Retinol=0.85, Retinol_ref_mean=0.9, Retinol_ref_sd=0.2,
  Tocopherol=32, Total_lipids=3.2, PIVKA_II=4,
  VitC=65, Homocysteine=9, MMA=0.28, Zinc=14, Copper=14,
  total_testosterone=15, SHBG=38, LH=5, FSH=4,
  estradiol=110, progesterone=0.5, free_T3=4.6, free_T4=14.5,
  TSH=1.8, aldosterone=195, renin=11, GH=1.1, IGF1=175, prolactin=11,
  cortisol_0=410, cortisol_30=620,
  GSH=1200, GSSG=11,
  NfL=9.2, Kyn_nM=2700, Trp_uM=58,
  IL6=2.1, TNFa=3.4,
  MetS=0
)

cm <- list(
  G0="G0", I0="I0", G30="G30", I30="I30", G120="G120", I120="I120",
  FFA="FFA", glucose="glucose", insulin="insulin",
  TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c", ApoB="ApoB", ApoA1="ApoA1",
  weight="weight", height="height", BMI="BMI", bmi="BMI",
  waist="waist", hip="hip", fat_mass="fat_mass", ALM="ALM",
  WBC="WBC", neutrophils="neutrophils", lymphocytes="lymphocytes",
  monocytes="monocytes", eosinophils="eosinophils", platelets="platelets",
  CRP="CRP", albumin="albumin", ESR="ESR",
  creatinine="creatinine", BUN="BUN",
  urine_albumin="urine_albumin", urine_creatinine="urine_creatinine",
  eGFR="eGFR", UACR="UACR",
  ALT="ALT", AST="AST", GGT="GGT",
  sbp="sbp", dbp="dbp", bp_sys="bp_sys", bp_dia="bp_dia",
  bp_treated="bp_treated", smoker="smoker", diabetes="diabetes",
  fev1="fev1", fvc="fvc",
  age="age", sex="sex", race="race",
  rate_palmitate="rate_palmitate", rate_glycerol="rate_glycerol",
  total_testosterone="total_testosterone", SHBG="SHBG", LH="LH", FSH="FSH",
  estradiol="estradiol", progesterone="progesterone",
  free_T3="free_T3", free_T4="free_T4", TSH="TSH",
  aldosterone="aldosterone", renin="renin",
  GH="GH", IGF1="IGF1", prolactin="prolactin",
  cortisol_0="cortisol_0", cortisol_30="cortisol_30",
  GSH="GSH", GSSG="GSSG",
  NfL="NfL", Kyn_nM="Kyn_nM", Trp_uM="Trp_uM",
  IL6="IL6", TNFa="TNFa", MetS="MetS"
)

message("Shared data: ", nrow(df), " row x ", ncol(df), " cols")


# ---- 01 normalize_vec ----
x <- c(2.1, 3.5, 5.0, 1.2, 4.4)
print(normalize_vec(x, method = "z"))
print(normalize_vec(x, method = "range"))
print(normalize_vec(x, method = "robust"))


# ---- 02 calc_sds ----
df_sds <- data.frame(BMI = c(22, 28, 35))
ref_sds <- data.frame(var = "BMI", mean = 25, sd = 4)
out_sds <- calc_sds(df_sds, vars = "BMI", ref = ref_sds, verbose = FALSE)
print(out_sds)


# ---- 03 infer_cols ----
df_inf <- data.frame(pglu0 = 5.5, insu0 = 65, hedlc = 1.3, trig = 1.8)
print(infer_cols(df_inf))


# ---- 04 hm_col_report ----
rpt <- hm_col_report(df, col_map = NULL, verbose = FALSE)
message("hm_col_report: ", length(rpt), " keys mapped")


# ---- 05 fasting_is ----
out_fi <- fasting_is(df, col_map = cm, verbose = FALSE, na_action = "keep")
message("fasting_is cols: ", paste(names(out_fi), collapse = ", "))


# ---- 06 ogtt_is ----
out_oi <- ogtt_is(df, col_map = cm, verbose = FALSE, na_action = "keep")
message("ogtt_is cols: ", paste(names(out_oi), collapse = ", "))


# ---- 07 adipo_is ----
cm_adipo <- list(G0="G0", I0="I0", TG="TG", HDL_c="HDL_c", FFA="FFA",
                 waist="waist", bmi="BMI")
out_adipo <- adipo_is(df, col_map = cm_adipo, verbose = FALSE, na_action = "keep")
message("adipo_is cols: ", paste(names(out_adipo), collapse = ", "))


# ---- 08 tracer_dxa_is ----
cm_tdxa <- list(I0="I0", rate_glycerol="rate_glycerol",
                rate_palmitate="rate_palmitate", fat_mass="fat_mass",
                weight="weight", bmi="BMI", HDL_c="HDL_c")
out_tdxa <- tracer_dxa_is(df, col_map = cm_tdxa, verbose = FALSE, na_action = "keep")
message("tracer_dxa_is cols: ", paste(names(out_tdxa), collapse = ", "))


# ---- 09 all_insulin_indices ----
out_ins <- all_insulin_indices(df, col_map = cm, normalize = "none",
                                mode = "both", verbose = FALSE, na_action = "keep")
message("all_insulin_indices cols: ", ncol(out_ins))


# ---- 10 glycemic_markers ----
out_gly <- suppressWarnings(
  glycemic_markers(df, col_map = cm, normalize = "none", verbose = FALSE, na_action = "keep")
)
message("glycemic_markers cols: ", ncol(out_gly))


# ---- 11 lipid_markers ----
out_lip <- lipid_markers(df, col_map = cm, normalize = "none", verbose = FALSE, na_action = "keep")
message("lipid_markers cols: ", ncol(out_lip))


# ---- 12 atherogenic_indices ----
out_ath <- atherogenic_indices(df, col_map = cm, verbose = FALSE, na_action = "keep")
message("atherogenic_indices cols: ", paste(names(out_ath), collapse = ", "))


# ---- 13 cvd_marker_aip ----
out_aip <- cvd_marker_aip(df, col_map = cm, verbose = FALSE, na_action = "keep")
message("cvd_marker_aip cols: ", paste(names(out_aip), collapse = ", "))


# ---- 14 cvd_marker_ldl_particle_number ----
out_ldlp <- cvd_marker_ldl_particle_number(df, col_map = cm, verbose = FALSE, na_action = "keep")
message("cvd_marker_ldl_particle_number cols: ", paste(names(out_ldlp), collapse = ", "))


# ---- 15 liver_markers ----
out_liv <- liver_markers(df, col_map = cm, verbose = FALSE, na_action = "keep")
message("liver_markers cols: ", ncol(out_liv))


# ---- 16 liver_fat_markers ----
cm_lf <- list(ALT="ALT", AST="AST", BMI="BMI", sex="sex",
              diabetes="diabetes", MetS="MetS", insulin="insulin",
              waist="waist", TG="TG", HDL_c="HDL_c", glucose="glucose")
out_lf <- suppressWarnings(
  liver_fat_markers(df, col_map = cm_lf, verbose = FALSE, na_action = "keep")
)
message("liver_fat_markers cols: ", ncol(out_lf))


# ---- 17 renal_markers ----
cm_ren <- list(creatinine="creatinine", age="age", sex="sex", race="race",
               BUN="BUN", urine_albumin="urine_albumin",
               urine_creatinine="urine_creatinine")
out_ren <- renal_markers(df, col_map = cm_ren, verbose = FALSE, na_action = "keep")
message("renal_markers cols: ", ncol(out_ren))


# ---- 18 ckd_stage ----
df_ckd <- data.frame(eGFR = c(95, 55, 28), UACR = c(12, 80, 350))
out_ckd <- ckd_stage(df_ckd, col_map = list(eGFR = "eGFR", UACR = "UACR"))
print(out_ckd)


# ---- 19 kidney_failure_risk ----
df_kfre <- data.frame(age=c(65,72), sex=c(1,0), eGFR=c(45,22), UACR=c(300,1200))
out_kfre <- kidney_failure_risk(df_kfre,
  col_map = list(age="age", sex="sex", eGFR="eGFR", UACR="UACR"),
  na_action = "keep")
print(out_kfre)


# ---- 20 cvd_risk_ascvd ----
df_cvd <- data.frame(age=55, sex="M", race="white",
                     TC=5.4, HDL_c=1.3, sbp=128,
                     diabetes=0, smoker=0, bp_treated=0)
cm_cvd <- list(age="age", sex="sex", race="race",
               TC="TC", HDL_c="HDL_c", sbp="sbp",
               diabetes="diabetes", smoker="smoker", bp_treated="bp_treated")
out_ascvd <- suppressWarnings(cvd_risk_ascvd(df_cvd, col_map = cm_cvd, verbose = FALSE))
message("cvd_risk_ascvd cols: ", paste(names(out_ascvd), collapse = ", "))


# ---- 21 cvd_risk_stroke ----
out_stroke <- suppressWarnings(
  cvd_risk_stroke(df_cvd, col_map = cm_cvd, verbose = FALSE, na_action = "keep")
)
message("cvd_risk_stroke cols: ", paste(names(out_stroke), collapse = ", "))


# ---- 22 cvd_risk (wrapper) ----
out_cvdr <- suppressWarnings(
  cvd_risk(df_cvd, col_map = cm_cvd, verbose = FALSE, na_action = "keep")
)
message("cvd_risk (wrapper) cols: ", paste(names(out_cvdr), collapse = ", "))


# ---- 23 cvd_risk_qrisk3 (requires QRISK3 package) ----
if (requireNamespace("QRISK3", quietly = TRUE)) {
  out_q3 <- suppressWarnings(cvd_risk_qrisk3(df_cvd, col_map = cm_cvd, verbose = FALSE))
  message("cvd_risk_qrisk3 cols: ", paste(names(out_q3), collapse = ", "))
} else {
  message("QRISK3 not installed -- skipping cvd_risk_qrisk3")
}


# ---- 24 cvd_risk_scorescvd (requires RiskScorescvd package) ----
if (requireNamespace("RiskScorescvd", quietly = TRUE)) {
  out_sc <- suppressWarnings(cvd_risk_scorescvd(df_cvd, col_map = cm_cvd, verbose = FALSE))
  message("cvd_risk_scorescvd cols: ", paste(names(out_sc), collapse = ", "))
} else {
  message("RiskScorescvd not installed -- skipping cvd_risk_scorescvd")
}


# ---- 25 inflammatory_markers ----
cm_inf <- list(neutrophils="neutrophils", lymphocytes="lymphocytes",
               monocytes="monocytes", platelets="platelets", WBC="WBC",
               CRP="CRP", albumin="albumin", eosinophils="eosinophils", ESR="ESR")
out_inf <- inflammatory_markers(df, col_map = cm_inf, panel = "both",
                                 na_action = "keep", verbose = FALSE)
message("inflammatory_markers cols: ", ncol(out_inf))


# ---- 26 metss ----
df_mets <- data.frame(
  waist=92, bp_sys=122, bp_dia=78, glucose=5.2,
  TG=1.6, HDL_c=1.3, sex="M", race="NHW",
  bp_treated=FALSE, smoker=FALSE, diabetes=FALSE
)
out_mets <- metss(df_mets, col_map = list(), verbose = FALSE, na_action = "keep")
message("metss cols: ", paste(names(out_mets), collapse = ", "))


# ---- 27 metabolic_risk_features ----
df_mrf <- data.frame(
  chol_total=5.4, chol_ldl=3.4, chol_hdl=1.3, triglycerides=1.6,
  age_year=48, z_HOMA=0.6, glucose=5.2, HbA1c=38,
  bp_sys_z=0.3, bp_dia_z=0.1
)
out_mrf <- metabolic_risk_features(df_mrf, verbose = FALSE, na_action = "keep")
message("metabolic_risk_features new cols: ",
        paste(setdiff(names(out_mrf), names(df_mrf)), collapse = ", "))


# ---- 28 metabolic_markers ----
out_mm <- suppressWarnings(
  metabolic_markers(df, col_map = cm,
                    which = c("insulin", "lipid", "liver", "glycemic"),
                    normalize = "none", mode = "both",
                    verbose = FALSE, na_action = "keep")
)
message("metabolic_markers: ", ncol(out_mm), " total output cols")


# ---- 29 obesity_indices ----
df_ob <- data.frame(wt=82, ht=178, wc=92, hip=99, sex=0)
out_ob <- obesity_indices(df_ob,
  weight = wt, height = ht, waist = wc, hip = hip, sex = sex,
  weight_unit = "kg", height_unit = "cm",
  adjust_WHR = TRUE, include_RFM = TRUE,
  verbose = FALSE, na_action = "keep")
message("obesity_indices new cols: ",
        paste(setdiff(names(out_ob), names(df_ob)), collapse = ", "))


# ---- 30 alm_bmi_index ----
df_alm <- data.frame(alm=19.5, bmi=25.9, sex="M")
out_alm <- alm_bmi_index(df_alm,
  col_map = list(alm="alm", bmi="bmi", sex="sex"), verbose = FALSE)
message("alm_bmi_index new cols: ",
        paste(setdiff(names(out_alm), names(df_alm)), collapse = ", "))


# ---- 31 adiposity_sds ----
df_ads <- data.frame(age=48, sex="M", BMI=25.9, waist=92, weight=82, height=178)
out_ads <- suppressWarnings(
  adiposity_sds(df_ads, col_map = NULL, verbose = FALSE, na_action = "keep")
)
message("adiposity_sds new cols: ",
        paste(setdiff(names(out_ads), names(df_ads)), collapse = ", "))


# ---- 32 adiposity_sds_strat ----
out_adss <- suppressWarnings(
  adiposity_sds_strat(df_ads, col_map = NULL, verbose = FALSE, na_action = "keep")
)
message("adiposity_sds_strat new cols: ",
        paste(setdiff(names(out_adss), names(df_ads)), collapse = ", "))


# ---- 33 spirometry_markers ----
df_spiro <- data.frame(FEV1 = 3.4, FVC = 4.3)
out_spiro <- spirometry_markers(df_spiro, verbose = FALSE, na_action = "keep")
message("spirometry_markers new cols: ",
        paste(setdiff(names(out_spiro), names(df_spiro)), collapse = ", "))


# ---- 34 pulmo_markers (requires rspiro package) ----
if (requireNamespace("rspiro", quietly = TRUE)) {
  df_pul <- data.frame(age=48, sex="M", height=178,
                       ethnicity="Caucasian", fev1=3.4, fvc=4.3)
  out_pul <- suppressWarnings(
    pulmo_markers(df_pul, equation = "GLI", verbose = FALSE, na_action = "keep")
  )
  message("pulmo_markers new cols: ",
          paste(setdiff(names(out_pul), names(df_pul)), collapse = ", "))
} else {
  message("rspiro not installed -- skipping pulmo_markers")
}


# ---- 35 bode_index ----
df_bode <- data.frame(BMI=25.9, fev1_pct=82, mmrc=1, sixmwd=420)
out_bode <- bode_index(df_bode,
  col_map = list(BMI="BMI", fev1_pct="fev1_pct", mmrc="mmrc", sixmwd="sixmwd"),
  verbose = FALSE)
message("bode_index new cols: ",
        paste(setdiff(names(out_bode), names(df_bode)), collapse = ", "))


# ---- 36 bone_markers ----
df_bone <- data.frame(age=60, weight=65, height=1.65, ALM=18.2, FM=22.0,
                      BMD=0.95, BMD_ref_mean=1.00, BMD_ref_sd=0.12)
cm_bone <- list(age="age", weight="weight", height="height",
                ALM="ALM", FM="FM", BMD="BMD",
                BMD_ref_mean="BMD_ref_mean", BMD_ref_sd="BMD_ref_sd")
out_bone <- bone_markers(df_bone, col_map = cm_bone, verbose = FALSE, na_action = "keep")
message("bone_markers new cols: ",
        paste(setdiff(names(out_bone), names(df_bone)), collapse = ", "))


# ---- 37 frax_score ----
df_frax <- data.frame(age=65, sex="F", weight=58, height=162,
                      prior_fracture=0, parent_fracture=0, steroids=0,
                      rheumatoid_arthritis=0, secondary_osteoporosis=0,
                      smoker=0, alcohol=0)
out_frax <- suppressWarnings(
  frax_score(df_frax, col_map = NULL, verbose = FALSE, na_action = "keep")
)
message("frax_score new cols: ",
        paste(setdiff(names(out_frax), names(df_frax)), collapse = ", "))


# ---- 38 vitamin_markers ----
df_vit <- data.frame(
  VitD=55, VitD_ref_mean=50, VitD_ref_sd=10,
  B12=350, Folate=18, Ferritin=80, TSat=0.28,
  Cortisol=420, DHEAS=95, Testosterone=14, Estradiol=110,
  TSH=1.8, free_T4=14.5,
  Retinol=0.85, Retinol_ref_mean=0.9, Retinol_ref_sd=0.2,
  Tocopherol=32, Total_lipids=3.2, PIVKA_II=4,
  VitC=65, Homocysteine=9, MMA=0.28, Magnesium=0.88, Zinc=14, Copper=14
)
cm_vit <- as.list(names(df_vit)); names(cm_vit) <- names(df_vit)
out_vit <- vitamin_markers(df_vit, col_map = cm_vit, verbose = FALSE, na_action = "keep")
message("vitamin_markers cols: ", ncol(out_vit))


# ---- 39 vitamin_d_status ----
df_vd <- data.frame(vitamin_D = c(28, 55, 82))
out_vd <- vitamin_d_status(df_vd,
  col_map = list(vitamin_D = "vitamin_D"), verbose = FALSE)
print(out_vd)


# ---- 40 nutrient_markers ----
cm_nutr <- as.list(names(df)); names(cm_nutr) <- names(df)
out_nutr <- suppressWarnings(
  nutrient_markers(df, col_map = cm_nutr, verbose = FALSE, na_action = "keep")
)
message("nutrient_markers cols: ", ncol(out_nutr))


# ---- 41 corrected_calcium ----
df_ca <- data.frame(Ca = c(2.3, 2.5, 2.1), albumin = c(38, 42, 30))
out_ca <- corrected_calcium(df_ca,
  col_map = list(calcium="Ca", albumin="albumin"),
  units = "si", verbose = FALSE)
print(out_ca)


# ---- 42 hormone_markers ----
df_horm <- data.frame(
  TT=15, SHBG=38, LH=5, FSH=4, E2=110, Prog=0.5,
  fT3=4.6, fT4=14.5, TSH=1.8, Aldo=195, Renin=11,
  Ins=65, Gluc=8, GH=1.1, IGF1=175, Prl=11,
  Cort0=410, Cort30=620
)
cm_horm <- list(
  total_testosterone="TT", SHBG="SHBG", LH="LH", FSH="FSH",
  estradiol="E2", progesterone="Prog", free_T3="fT3",
  free_T4="fT4", TSH="TSH", aldosterone="Aldo", renin="Renin",
  insulin="Ins", glucagon="Gluc", GH="GH", IGF1="IGF1",
  prolactin="Prl", cortisol_0="Cort0", cortisol_30="Cort30"
)
out_horm <- hormone_markers(df_horm, col_map = cm_horm, verbose = FALSE, na_action = "keep")
message("hormone_markers cols: ", ncol(out_horm))


# ---- 43 allostatic_load ----
df_allo <- data.frame(SBP=c(118,142,130), DBP=c(76,92,85), CRP=c(1.2,4.8,2.1))
out_allo <- allostatic_load(df_allo,
  thresholds = list(SBP=130, DBP=85, CRP=3),
  verbose = FALSE, na_action = "keep")
print(out_allo)


# ---- 44 iAge ----
df_iage <- data.frame(CRP=1.8, IL6=2.1, TNFa=3.4)
out_iage <- iAge(df_iage,
  col_map = list(CRP="CRP", IL6="IL6", TNFa="TNFa"),
  weights = c(CRP=0.33, IL6=0.33, TNFa=0.34),
  verbose = FALSE, na_action = "keep")
print(out_iage)


# ---- 45 nfl_marker ----
df_nfl <- data.frame(NfL = c(8.5, 14.2, 22.1))
out_nfl <- nfl_marker(df_nfl,
  col_map = list(NfL="NfL"), verbose = FALSE, na_action = "keep")
print(out_nfl)


# ---- 46 kyn_trp_ratio ----
df_kyn <- data.frame(Kyn_nM = c(2500, 3100), Trp_uM = c(55, 48))
out_kyn <- kyn_trp_ratio(df_kyn,
  col_map = list(Kyn_nM="Kyn_nM", Trp_uM="Trp_uM"),
  verbose = FALSE, na_action = "keep")
print(out_kyn)


# ---- 47 oxidative_markers ----
df_ox <- data.frame(GSH = c(5.0, 3.0), GSSG = c(1.0, 0.5))
out_ox <- oxidative_markers(df_ox,
  col_map = list(GSH="GSH", GSSG="GSSG"),
  verbose = FALSE, na_action = "keep")
print(out_ox)


# ---- 48 saliva_markers ----
df_sal <- data.frame(
  saliva_cort1=18, saliva_cort2=12, saliva_cort3=8,
  saliva_amylase=75, saliva_glucose=5.0
)
out_sal <- saliva_markers(df_sal, col_map = NULL, verbose = FALSE,
                           na_action = "keep", times = c(0, 30, 60))
message("saliva_markers new cols: ",
        paste(setdiff(names(out_sal), names(df_sal)), collapse = ", "))


# ---- 49 sweat_markers ----
df_sw <- data.frame(
  sweat_chloride=35, sweat_Na=55, sweat_K=8, sweat_lactate=12,
  weight_before=82, weight_after=81.3, duration=60, body_surface_area=1.9
)
out_sw <- sweat_markers(df_sw, col_map = NULL, verbose = FALSE, na_action = "keep")
message("sweat_markers new cols: ",
        paste(setdiff(names(out_sw), names(df_sw)), collapse = ", "))


# ---- 50 urine_markers ----
df_ur <- data.frame(
  urine_albumin=18, urine_creatinine=120,
  urine_protein=25, urine_Na=80, urine_K=40
)
out_ur <- urine_markers(df_ur, verbose = FALSE, na_action = "keep")
message("urine_markers new cols: ",
        paste(setdiff(names(out_ur), names(df_ur)), collapse = ", "))


# ---- 51 sarc_f_score ----
df_sf <- data.frame(Strength=c(1,2,0), Walking=c(0,1,2),
                    Chair=c(1,1,2), Stairs=c(0,2,2), Falls=c(0,1,1))
out_sf <- sarc_f_score(df_sf, verbose = FALSE)
print(out_sf[setdiff(names(out_sf), names(df_sf))])


# ---- 52 charlson_index ----
df_cci <- data.frame(
  mi=0, chf=0, pvd=0, stroke=0, dementia=0, copd=0, rheum=0, ulcer=0,
  mild_liver=0, diabetes=1, diab_comp=0, hemiplegia=0, renal=1,
  cancer=0, leukemia=0, lymphoma=0, sev_liver=0, metastatic_cancer=0, hiv=0
)
out_cci <- charlson_index(df_cci, verbose = FALSE)
print(out_cci[setdiff(names(out_cci), names(df_cci))])


# ---- 53 frailty_index (requires 'di' package) ----
if (requireNamespace("di", quietly = TRUE)) {
  df_fi <- data.frame(age=c(70,75,80), d1=c(0,1,1), d2=c(0.2,0.8,1.0), d3=c(1L,0L,1L))
  out_fi <- frailty_index(df_fi, cols = c("d1","d2","d3"), age = "age",
                           return = "data", verbose = FALSE)
  print(out_fi[c("FI", "FI_class")])
} else {
  message("'di' package not installed -- skipping frailty_index")
}


# ---- 54 plot_frailty_age (requires 'di' package) ----
if (requireNamespace("di", quietly = TRUE)) {
  df_fi2 <- data.frame(age=c(65,70,75,80), d1=c(0,0,1,1), d2=c(0.1,0.3,0.6,0.9))
  plot_frailty_age(df_fi2, cols = c("d1","d2"), age = "age", visible = FALSE)
  message("plot_frailty_age: completed")
} else {
  message("'di' package not installed -- skipping plot_frailty_age")
}


# ---- 55 phq9_score ----
df_phq <- setNames(as.data.frame(matrix(c(0,1,2,1,0,1,2,1,0), nrow=1)),
                   sprintf("phq9_%02d", 1:9))
out_phq <- phq9_score(df_phq, verbose = FALSE, na_action = "keep")
print(out_phq[grep("^PHQ9", names(out_phq))])


# ---- 56 gad7_score ----
df_gad <- setNames(as.data.frame(matrix(c(1,2,0,1,0,2,1), nrow=1)),
                   sprintf("gad7_%02d", 1:7))
out_gad <- gad7_score(df_gad, verbose = FALSE, na_action = "keep")
print(out_gad[grep("^GAD7", names(out_gad))])


# ---- 57 k6_score ----
df_k6 <- setNames(as.data.frame(matrix(c(1,0,2,1,0,1), nrow=1)),
                  sprintf("k6_%02d", 1:6))
out_k6 <- k6_score(df_k6, verbose = FALSE, na_action = "keep")
print(out_k6[grep("^K6", names(out_k6))])


# ---- 58 k10_score ----
df_k10 <- setNames(as.data.frame(matrix(c(1,0,2,1,0,1,2,0,1,0), nrow=1)),
                   sprintf("k10_%02d", 1:10))
out_k10 <- k10_score(df_k10, verbose = FALSE, na_action = "keep")
print(out_k10[grep("^K10", names(out_k10))])


# ---- 59 ghq12_score ----
df_ghq <- setNames(as.data.frame(matrix(rep(1L, 12), nrow=1)),
                   sprintf("ghq12_%02d", 1:12))
out_ghq <- ghq12_score(df_ghq, method = "binary", verbose = FALSE, na_action = "keep")
print(out_ghq[grep("^GHQ12", names(out_ghq))])


# ---- 60 who5_score ----
df_who <- setNames(as.data.frame(matrix(c(3,4,2,3,4), nrow=1)),
                   sprintf("who5_%02d", 1:5))
out_who <- who5_score(df_who, verbose = FALSE, na_action = "keep")
print(out_who[grep("^WHO5", names(out_who))])


# ---- 61 isi_score ----
df_isi <- setNames(as.data.frame(matrix(c(1,2,1,1,0,2,1), nrow=1)),
                   sprintf("isi_%02d", 1:7))
out_isi <- isi_score(df_isi, verbose = FALSE, na_action = "keep")
print(out_isi[grep("^ISI", names(out_isi))])


# ---- 62 mdq_score ----
df_mdq <- setNames(
  as.data.frame(matrix(c(rep(1L,13), 1L, 1L, 1L), nrow=1)),
  c(sprintf("mdq_%02d", 1:13), "mdq_clustering", "mdq_impairment", "mdq_total_check")
)
out_mdq <- suppressWarnings(mdq_score(df_mdq, verbose = FALSE, na_action = "keep"))
print(out_mdq[grep("^MDQ", names(out_mdq))])


# ---- 63 asrs_score ----
df_asrs <- setNames(as.data.frame(matrix(c(3,2,3,2,1,0), nrow=1)),
                    sprintf("asrs_%02d", 1:6))
out_asrs <- asrs_score(df_asrs, verbose = FALSE, na_action = "keep")
print(out_asrs[grep("^ASRS", names(out_asrs))])


# ---- 64 bis_score ----
bis_key <- list(items = sprintf("bis_%02d", 1:5), min_val = 1, max_val = 4)
df_bis <- setNames(as.data.frame(matrix(c(2,3,1,4,2), nrow=1)),
                   sprintf("bis_%02d", 1:5))
out_bis <- bis_score(df_bis, key = bis_key, verbose = FALSE, na_action = "keep")
print(out_bis[grep("^BIS", names(out_bis))])


# ---- 65 spq_score ----
spq_key <- list(items = sprintf("spq_%02d", 1:5), min_val = 0, max_val = 1)
df_spq <- setNames(as.data.frame(matrix(c(0,1,0,1,0), nrow=1)),
                   sprintf("spq_%02d", 1:5))
out_spq <- spq_score(df_spq, key = spq_key, verbose = FALSE, na_action = "keep")
print(out_spq[grep("^SPQ", names(out_spq))])


# ---- 66 cognitive_score ----
df_cog <- data.frame(task_a=c(85,90), task_b=c(72,88), task_c=c(68,79))
cm_cog <- list(tasks = list(task_a="task_a", task_b="task_b", task_c="task_c"))
out_cog <- cognitive_score(df_cog, col_map = cm_cog, method = "z_mean", verbose = FALSE)
print(out_cog[grep("^cog", names(out_cog))])


# ---- 67 psych_dx_flags ----
df_pdx <- data.frame(has_mdd=1, has_anxiety=0, has_adhd=0,
                     has_bipolar=0, has_scz=0, has_sud=0)
cm_pdx <- list(dx = list(mdd="has_mdd", anxiety="has_anxiety", adhd="has_adhd",
                          bipolar="has_bipolar", scz="has_scz", sud="has_sud"))
out_pdx <- psych_dx_flags(df_pdx, col_map = cm_pdx)
print(out_pdx[grep("^dx_", names(out_pdx))])


# ---- 68 psych_med_flags ----
df_pmed <- data.frame(on_ssri=1, on_snri=0, on_ap=0, on_ms=0, on_anx=0)
cm_pmed <- list(med = list(ssri="on_ssri", snri="on_snri",
                            antipsychotic="on_ap",
                            mood_stabilizer="on_ms", anxiolytic="on_anx"))
out_pmed <- psych_med_flags(df_pmed, col_map = cm_pmed)
print(out_pmed[grep("^med_", names(out_pmed))])


# ---- 69 psych_markers ----
df_psy <- cbind(
  setNames(as.data.frame(matrix(c(0,1,2,1,0,1,2,1,0), nrow=1)), sprintf("phq9_%02d", 1:9)),
  setNames(as.data.frame(matrix(c(1,2,0,1,0,2,1), nrow=1)), sprintf("gad7_%02d", 1:7))
)
out_psy <- psych_markers(df_psy, which = c("phq9","gad7"),
                          verbose = FALSE, na_action = "keep")
message("psych_markers new cols: ",
        paste(setdiff(names(out_psy), names(df_psy)), collapse = ", "))


# ---- 70 impute_missing ----
df_na <- data.frame(x = c(1, NA, 3, NA), y = c(NA, 2, 3, 4))
out_imp <- impute_missing(df_na, method = "median", verbose = FALSE)
print(out_imp)


# ---- 71 impute_mice (requires mice package) ----
if (requireNamespace("mice", quietly = TRUE)) {
  out_mice <- impute_mice(df_na, m = 1, seed = 42, verbose = FALSE)
  print(out_mice)
} else {
  message("mice not installed -- skipping impute_mice")
}


# ---- 72 impute_missforest (requires missForest package) ----
if (requireNamespace("missForest", quietly = TRUE)) {
  out_mf <- suppressWarnings(impute_missforest(df_na, verbose = FALSE))
  print(out_mf)
} else {
  message("missForest not installed -- skipping impute_missforest")
}


# ---- 73 validate_inputs ----
validate_inputs(df, col_map = cm, required_keys = c("G0","I0","TC"), fn = "test")
message("validate_inputs: passed")


# ---- 74 marker_summary ----
df_sum <- data.frame(HOMA_IR=1.4, HOMA_IS=0.7, TG=1.6, HDL_c=1.3)
out_ms <- marker_summary(df_sum, verbose = FALSE)
print(out_ms)


# ---- 75 health_summary ----
df_hs <- data.frame(TC=5.4, HDL_c=1.3, TG=1.6, glucose=5.2, BMI=25.9, eGFR=88)
out_hs <- health_summary(df_hs, col_map = NULL, verbose = FALSE)
print(out_hs)


# ---- 76 all_health_markers -- fast subset (no external packages) ----
fast_groups <- c(
  "insulin_fasting", "insulin_ogtt", "insulin_adipose", "insulin_tracer_dxa",
  "lipid", "atherogenic", "cvd_aip", "cvd_ldl_particles",
  "glycemic", "liver", "liver_fat",
  "renal", "ckd_stage", "kidney_kfre", "urine",
  "inflammatory", "hormone", "nutrient", "vitamin", "vitamin_d_status",
  "calcium_corrected", "kyn_trp", "oxidative", "nfl", "iAge",
  "spirometry", "bode",
  "saliva", "sweat",
  "mets", "metabolic_risk",
  "bone", "frax", "sarc_f", "charlson",
  "allostatic_load",
  "psych",
  "adiposity_sds", "adiposity_sds_strat",
  "alm_bmi", "obesity_metrics"
)

out_fast <- suppressWarnings(
  all_health_markers(
    df, col_map = cm,
    which = fast_groups,
    include_insulin = TRUE,
    normalize = "none",
    mode = "both",
    verbose = FALSE,
    na_action = "keep"
  )
)
message("all_health_markers (fast): ", ncol(out_fast), " output cols")
print(head(setdiff(names(out_fast), names(df)), 40))


# ---- 77 all_health_markers -- which='all' (slow; uncomment to run) ----
# out_full <- suppressWarnings(
#   all_health_markers(df, col_map = cm, which = "all",
#                      normalize = "none", mode = "both",
#                      verbose = TRUE, na_action = "keep")
# )
# message("all_health_markers (all): ", ncol(out_full), " cols")


# ---- 78 all_health_markers -- unknown group error ----
tryCatch(
  all_health_markers(df, col_map = cm, which = "does_not_exist",
                     include_insulin = FALSE, verbose = FALSE),
  error = function(e) message("Expected error caught: ", conditionMessage(e))
)


message("=== All manual tests complete ===")