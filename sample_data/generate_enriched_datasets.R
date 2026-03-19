set.seed(20260319)

base_dir <- file.path("sample_data")
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

build_area_values <- function(base_value, n_areas = 4, sd_value = 6) {
  vals <- stats::rnorm(n_areas, mean = base_value, sd = sd_value)
  pmax(vals, 1)
}

line_from_values <- function(label, values, digits = 2) {
  formatted <- if (is.numeric(values)) {
    sprintf(paste0("%.", digits, "f"), values)
  } else {
    values
  }
  paste(c(label, formatted), collapse = "\t")
}

write_fluorcam_file <- function(file_path, base_fo, base_fm) {
  fo_vals <- build_area_values(base_fo, sd_value = 3)
  fm_vals <- build_area_values(base_fm, sd_value = 10)
  fv_vals <- pmax(fm_vals - fo_vals, 1)

  fm_l1_vals <- fm_vals * runif(4, 0.70, 0.76)
  fm_l2_vals <- fm_vals * runif(4, 0.54, 0.60)
  fm_l3_vals <- fm_vals * runif(4, 0.48, 0.54)
  fm_d1_vals <- fm_vals * runif(4, 0.62, 0.69)
  fm_d2_vals <- fm_vals * runif(4, 0.78, 0.84)

  ft_l1_vals <- fm_l1_vals * runif(4, 0.96, 0.995)
  ft_l2_vals <- fm_l2_vals * runif(4, 0.90, 0.96)
  ft_l3_vals <- fm_l3_vals * runif(4, 0.88, 0.94)

  fq_l1_vals <- fm_l1_vals - ft_l1_vals
  fq_l2_vals <- fm_l2_vals - ft_l2_vals
  fq_l3_vals <- fm_l3_vals - ft_l3_vals

  qy_max_vals <- pmax(0, pmin(1, fv_vals / fm_vals))

  size_vals <- as.integer(round(runif(4, min = 38000, max = 76000), 0))

  txt_lines <- c(
    "FluorCam7 Numeric Avg Export File",
    "",
    "-----------------------------------",
    "",
    "\tArea 1\tArea 2\tArea 3\tArea 4",
    "",
    line_from_values("Size [pixels]", size_vals, digits = 0),
    "",
    line_from_values("Fo", fo_vals),
    "",
    line_from_values("Fm", fm_vals),
    "",
    line_from_values("Fv", fv_vals),
    "",
    line_from_values("Fm_L1", fm_l1_vals),
    "",
    line_from_values("Fm_L2", fm_l2_vals),
    "",
    line_from_values("Fm_L3", fm_l3_vals),
    "",
    line_from_values("Fm_D1", fm_d1_vals),
    "",
    line_from_values("Fm_D2", fm_d2_vals),
    "",
    line_from_values("Ft_L1", ft_l1_vals),
    "",
    line_from_values("Ft_L2", ft_l2_vals),
    "",
    line_from_values("Ft_L3", ft_l3_vals),
    "",
    line_from_values("Fq_L1", fq_l1_vals),
    "",
    line_from_values("Fq_L2", fq_l2_vals),
    "",
    line_from_values("Fq_L3", fq_l3_vals),
    "",
    line_from_values("QY_max", qy_max_vals)
  )

  writeLines(txt_lines, file_path)
}

# -------------------------------------------------------------------------
# DATASET 1: ONE-WAY DEMO (with optional facet via Batch)
# Filename pattern: Batch_Treatment_Rep.TXT
# -------------------------------------------------------------------------
oneway_dir <- file.path(base_dir, "fluorcam_oneway_demo")
dir.create(oneway_dir, recursive = TRUE, showWarnings = FALSE)

oneway_grid <- expand.grid(
  Batch = c("B1", "B2"),
  Treatment = c("Ctrl", "TreatA", "TreatB"),
  Rep = 1:2,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(oneway_grid))) {
  row <- oneway_grid[i, ]

  batch_effect <- if (row$Batch == "B2") 6 else 0
  treatment_effect <- switch(
    row$Treatment,
    Ctrl = 0,
    TreatA = 18,
    TreatB = -12,
    0
  )

  base_fo <- 150 + batch_effect * 0.3 + treatment_effect * 0.1 + stats::rnorm(1, 0, 1.5)
  base_fm <- 680 + batch_effect + treatment_effect + stats::rnorm(1, 0, 8)

  file_name <- paste(row$Batch, row$Treatment, paste0("R", row$Rep), sep = "_")
  write_fluorcam_file(file.path(oneway_dir, paste0(file_name, ".TXT")), base_fo, base_fm)
}

# -------------------------------------------------------------------------
# DATASET 2: TWO-WAY DEMO
# Filename pattern: Genotype_Dose_Rep.TXT
# -------------------------------------------------------------------------
twoway_dir <- file.path(base_dir, "fluorcam_twoway_demo")
dir.create(twoway_dir, recursive = TRUE, showWarnings = FALSE)

twoway_grid <- expand.grid(
  Genotype = c("WT", "H1"),
  Dose = c("Low", "Mid", "High"),
  Rep = 1:3,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(twoway_grid))) {
  row <- twoway_grid[i, ]

  geno_effect <- if (row$Genotype == "H1") -10 else 0
  dose_effect <- switch(row$Dose, Low = 0, Mid = 12, High = 22, 0)
  interaction_effect <- if (row$Genotype == "H1" && row$Dose == "High") -9 else 0

  base_fo <- 152 + geno_effect * 0.2 + dose_effect * 0.1 + stats::rnorm(1, 0, 1.5)
  base_fm <- 700 + geno_effect + dose_effect + interaction_effect + stats::rnorm(1, 0, 8)

  file_name <- paste(row$Genotype, row$Dose, paste0("R", row$Rep), sep = "_")
  write_fluorcam_file(file.path(twoway_dir, paste0(file_name, ".TXT")), base_fo, base_fm)
}

# -------------------------------------------------------------------------
# DATASET 3: THREE-WAY DEMO
# Filename pattern: UV_Light_Temp_Rep.TXT
# -------------------------------------------------------------------------
threeway_dir <- file.path(base_dir, "fluorcam_threeway_demo")
dir.create(threeway_dir, recursive = TRUE, showWarnings = FALSE)

threeway_grid <- expand.grid(
  UV = c("NoUV", "UV"),
  Light = c("L200", "L400"),
  Temp = c("T20", "T30"),
  Rep = 1:2,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(threeway_grid))) {
  row <- threeway_grid[i, ]

  uv_effect <- if (row$UV == "UV") 14 else 0
  light_effect <- if (row$Light == "L400") 16 else 0
  temp_effect <- if (row$Temp == "T30") -9 else 0
  interaction_effect <- if (row$UV == "UV" && row$Light == "L400" && row$Temp == "T30") 10 else 0

  base_fo <- 148 + uv_effect * 0.2 + light_effect * 0.15 + temp_effect * 0.2 + stats::rnorm(1, 0, 2)
  base_fm <- 690 + uv_effect + light_effect + temp_effect + interaction_effect + stats::rnorm(1, 0, 10)

  file_name <- paste(row$UV, row$Light, row$Temp, paste0("R", row$Rep), sep = "_")
  write_fluorcam_file(file.path(threeway_dir, paste0(file_name, ".TXT")), base_fo, base_fm)
}

# -------------------------------------------------------------------------
# DATASET 4: CONVERT-TO-CURVE DEMO (time in filename labels)
# Filename pattern: Time_Genotype_Rep.TXT
# -------------------------------------------------------------------------
convert_dir <- file.path(base_dir, "fluorcam_convert_curve_demo")
dir.create(convert_dir, recursive = TRUE, showWarnings = FALSE)

convert_grid <- expand.grid(
  Time = c("T0", "T6h", "T24h", "T48h"),
  Genotype = c("WT", "H1"),
  Rep = 1:3,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(convert_grid))) {
  row <- convert_grid[i, ]

  time_effect <- switch(row$Time, T0 = 0, T6h = 8, T24h = 20, T48h = 13, 0)
  geno_effect <- if (row$Genotype == "H1") -7 else 0

  base_fo <- 150 + time_effect * 0.15 + geno_effect * 0.2 + stats::rnorm(1, 0, 1.8)
  base_fm <- 675 + time_effect + geno_effect + stats::rnorm(1, 0, 9)

  file_name <- paste(row$Time, row$Genotype, paste0("R", row$Rep), sep = "_")
  write_fluorcam_file(file.path(convert_dir, paste0(file_name, ".TXT")), base_fo, base_fm)
}

cat("Generated enriched sample datasets:\n")
cat("-", oneway_dir, "(", nrow(oneway_grid), "files)\n")
cat("-", twoway_dir, "(", nrow(twoway_grid), "files)\n")
cat("-", threeway_dir, "(", nrow(threeway_grid), "files)\n")
cat("-", convert_dir, "(", nrow(convert_grid), "files)\n")
