set.seed(20260316)

output_dir <- file.path("sample_data", "fluorcam_factorial")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

combinations <- expand.grid(
  UV = c("yes", "no"),
  Light = c("200", "400"),
  Temp = c("20", "30"),
  Rep = 1:3,
  stringsAsFactors = FALSE
)

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

for (i in seq_len(nrow(combinations))) {
  row <- combinations[i, ]

  uv_effect <- if (row$UV == "yes") 12 else 0
  light_effect <- if (row$Light == "400") 18 else 0
  temp_effect <- if (row$Temp == "30") -10 else 0
  interaction_effect <- if (row$UV == "yes" && row$Light == "400") 8 else 0

  base_fo <- 80 + uv_effect * 0.2 + light_effect * 0.1 + temp_effect * 0.2 + stats::rnorm(1, 0, 2)
  base_fm <- 560 + uv_effect + light_effect + temp_effect + interaction_effect + stats::rnorm(1, 0, 8)

  fo_vals <- build_area_values(base_fo, sd_value = 3)
  fm_vals <- build_area_values(base_fm, sd_value = 12)
  fv_vals <- pmax(fm_vals - fo_vals, 1)

  fm_l1_vals <- fm_vals * runif(4, 0.70, 0.76)
  fm_l2_vals <- fm_vals * runif(4, 0.52, 0.58)
  fm_l3_vals <- fm_vals * runif(4, 0.46, 0.52)

  size_vals <- as.integer(round(runif(4, min = 42000, max = 76000), 0))

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
    line_from_values("Fm_L3", fm_l3_vals)
  )

  file_name <- paste(row$UV, row$Light, row$Temp, paste0("R", row$Rep), sep = "_")
  file_path <- file.path(output_dir, paste0(file_name, ".TXT"))
  writeLines(txt_lines, file_path)
}

cat("Generated", nrow(combinations), "files in", output_dir, "\n")
