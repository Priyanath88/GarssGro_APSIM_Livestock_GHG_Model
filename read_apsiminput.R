
# ============================================================
# Title:     GGTactical simulation .txt file reader
# Author:    Priyanath Jayasinghe
# Email:     priyanath.jayasinghe@csiro.au
# Created:   2026-04-10
#
# Description:
#   Pprocesses GGTactical simulation  outputs (.txt) and formats
#   daily livestock and pasture data suitable for APSIM Next Gen simulations. This function need to be in the same location as GGTactical and automation.R file
#
# Inputs:
#   - GGTactical output text files
#
# Notes:
#   - Designed for multi-pasture systems (plant1, plant2, ...)
# ============================================================

rename_apsim_columns <- function(x) {
  rename_map <- c(          # convert original GGTactical report variables to APSIM readable parameter names
    "Date" = "date",
    "Nitrogen_excretion_area_Urine_N_kg_ha" = "urine_n",
    "Nitrogen_excretion_area_Faecal_N_kg_ha" = "faecal_n",
    "Numbers_Female_weaners" = "n_female_weaners",
    "Numbers_Female_1_2_y.o." = "n_female_1_2yo",
    "Numbers_Mature_Females" = "n_mature_females",
    "Numbers_Male_weaners" = "n_male_weaners",
    "Numbers_Male_1_2_y.o." = "n_male_1_2yo",
    "Numbers_Mature_Males" = "n_mature_males",
    "Total_pasture_intake_kg_head" = "pasture_intake_total",
    "Pasture_intake_by_class_Female_weaners_kg_head" = "pasture_intake_female_weaners",
    "Pasture_intake_by_class_Female_1_2_y.o._kg_head" = "pasture_intake_female_1_2yo",
    "Pasture_intake_by_class_Female_mature_kg_head" = "pasture_intake_female_mature",
    "Pasture_intake_by_class_Male_weaners_kg_head" = "pasture_intake_male_weaners",
    "Pasture_intake_by_class_Male_1_2_y.o._kg_head" = "pasture_intake_male_1_2yo",
    "Pasture_intake_by_class_Male_mature_kg_head" = "pasture_intake_male_mature",
    "Total_supplement_intake_kg_head" = "supplement_intake_total",
    "Supplement_intake_by_class_Female_weaners_kg_head" = "supplement_intake_female_weaners",
    "Supplement_intake_by_class_Female_1_2_y.o._kg_head" = "supplement_intake_female_1_2yo",
    "Supplement_intake_by_class_Female_mature_kg_head" = "supplement_intake_female_mature",
    "Supplement_intake_by_class_Male_weaners_kg_head" = "supplement_intake_male_weaners",
    "Supplement_intake_by_class_Male_1_2_y.o._kg_head" = "supplement_intake_male_1_2yo",
    "Supplement_intake_by_class_Male_mature_kg_head" = "supplement_intake_male_mature",
    "Potential_evapotranspiration_P1_mm" = "pet",
    "Utilization_rate_%" = "utilization_rate",
    "Numbers_Male_unweaned" = "n_male_unweaned",
    "Numbers_Male_weaners.1" = "n_male_weaners_lamb",
    "Numbers_Male_yearlings" = "n_male_yearlings",
    "Numbers_Female_unweaned" = "n_female_unweaned",
    "Numbers_Female_weaners.1" = "n_female_weaners_lamb",
    "Numbers_Female_yearlings" = "n_female_yearlings",
    "Methane_production_g_head" = "methane",
    "Pasture_intake_by_class_Unweaned_kg_head" = "pasture_intake_unweaned",
    "Pasture_intake_by_class_Male_weaners_kg_head.1" = "pasture_intake_male_weaners_lamb",
    "Pasture_intake_by_class_Male_yearlings_kg_head" = "pasture_intake_male_yearlings",
    "Pasture_intake_by_class_Female_weaners_kg_head.1" = "pasture_intake_female_weaners_lamb",
    "Pasture_intake_by_class_Female_yearlings_kg_head" = "pasture_intake_female_yearlings",
    "Supplement_intake_by_class_Unweaned_kg_head" = "supplement_intake_unweaned",
    "Supplement_intake_by_class_Male_weaners_kg_head.1" = "supplement_intake_male_weaners_lamb",
    "Supplement_intake_by_class_Male_yearlings_kg_head" = "supplement_intake_male_yearlings",
    "Supplement_intake_by_class_Female_weaners_kg_head.1" = "supplement_intake_female_weaners_lamb",
    "Supplement_intake_by_class_Female_yearlings_kg_head" = "supplement_intake_female_yearlings",
    "Methane_production_g_head.1" = "methane_young",
    "Total_pasture_intake_kg_head.1" = "pasture_intake_total_young",
    "Total_supplement_intake_kg_head.1" = "supplement_intake_total_young",
    "Digestibility_Annual_Ryegrass_Average_P1_%" = "plant1_digestibility_avg",
    "Digestibility_Annual_Ryegrass_Green_P1_%" = "plant1_digestibility_green",
    "Digestibility_Annual_Ryegrass_Dead+Litter_P1_%" = "plant1_digestibility_dead_litter",
    "Protein_content_Annual_Ryegrass_Average_P1_%" = "plant1_protein_avg",
    "Protein_content_Annual_Ryegrass_Green_P1_%" = "plant1_protein_green",
    "Protein_content_Annual_Ryegrass_Dead+Litter_P1_%" = "plant1_protein_dead_litter",
    "Total_herbage_by_classes_Annual_Ryegrass_Seedlings_P1_kg_ha" = "plant1_herbage_seedlings",
    "Total_herbage_by_classes_Annual_Ryegrass_Live_P1_kg_ha" = "plant1_herbage_live",
    "Total_herbage_by_classes_Annual_Ryegrass_Senesc._P1_kg_ha" = "plant1_herbage_senescing",
    "Total_herbage_by_classes_Annual_Ryegrass_Dead_P1_kg_ha" = "plant1_herbage_dead",
    "Total_herbage_by_classes_Annual_Ryegrass_Litter_P1_kg_ha" = "plant1_herbage_litter",
    "Rooting_depth_Annual_Ryegrass_P1_mm" = "plant1_rooting_depth",
    "Loss_rates_Annual_Ryegrass_Seedlings_P1_%_d" = "plant1_loss_seedlings",
    "Loss_rates_Annual_Ryegrass_Live_P1_%_d" = "plant1_loss_live",
    "Loss_rates_Annual_Ryegrass_Senescing_P1_%_d" = "plant1_loss_senescing",
    "Loss_rates_Annual_Ryegrass_Dead_P1_%_d" = "plant1_loss_dead",
    "Loss_rates_Annual_Ryegrass_Litter_P1_%_d" = "plant1_loss_litter",
    "Loss_rates_Annual_Ryegrass_Roots_P1_%_d" = "plant1_loss_roots",
    "Root_mass_Annual_Ryegrass_P1_kg_ha" = "plant1_root_mass",
    "Total_root_length_Annual_Ryegrass_P1_m_m2" = "plant1_root_length",
    "Sward_height_P1_mm" = "plant1_sward_height",
    "Ground_cover_P1_mÂ²_mÂ²" = "plant1_ground_cover",
    "Protein_content_Green_P1_%" = "plant1_protein_content_green",
    "Growth_rate_P1_kg_ha_d" = "plant1_growth_rate",
    "Green_area_index_P1_mÂ²_mÂ²" = "plant1_LAI"
  )

  out <- ifelse(x %in% names(rename_map), unname(rename_map[x]), x)
  make.unique(out)
}

read_grassgro_output <- function(path) {
  lines <- readLines(path, warn = FALSE)

  apsim_line <- grep("^APSIMInput\\s*$", lines)[1]
  if (is.na(apsim_line)) {
    stop("Could not find the APSIMInput section.")
  }

  group_header_line <- apsim_line + 2
  detail_header_line <- apsim_line + 3
  data_start_line <- apsim_line + 4

  split_tab <- function(x) strsplit(x, "\t", fixed = TRUE)[[1]]

  group_header <- split_tab(lines[group_header_line])
  detail_header <- split_tab(lines[detail_header_line])

  ncols <- max(length(group_header), length(detail_header))
  length(group_header) <- ncols
  length(detail_header) <- ncols
  group_header[is.na(group_header)] <- ""
  detail_header[is.na(detail_header)] <- ""

  last_group <- ""
  for (i in seq_along(group_header)) {
    if (nzchar(trimws(group_header[i]))) {
      last_group <- trimws(group_header[i])
    } else {
      group_header[i] <- last_group
    }
  }

  clean_name <- function(x) {
    x <- trimws(x)
    x <- gsub("[[:space:]/()-]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x
  }

  col_names <- character(ncols)
  for (i in seq_len(ncols)) {
    g <- trimws(group_header[i])
    d <- trimws(detail_header[i])

    if (i == 1 && g == "Date") {
      col_names[i] <- "Date"
    } else if (d == "" || d == g) {
      col_names[i] <- clean_name(g)
    } else if (g == "") {
      col_names[i] <- clean_name(d)
    } else {
      col_names[i] <- clean_name(paste(g, d, sep = "_"))
    }
  }

  col_names <- make.unique(col_names)
  col_names <- rename_apsim_columns(col_names)

  dat <- read.delim(
    path,
    sep = "\t",
    header = FALSE,
    skip = data_start_line - 1,
    col.names = col_names,
    na.strings = c("n/a", "NA", ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if ("date" %in% names(dat)) {
    dat$date <- as.Date(dat$date, format = "%d-%m-%Y")
  }

  dat
}

read_apsiminput <- function() {
  file_path <- "C:/alldata/automation_of_GG_to_Apsim/GGTactical/Windows/x86/ggoutput_Chinchilla.txt"  ## define the path for the GrassGrooutput.txt file
  gg_dat <- read_grassgro_output(file_path)

  cat("Import successful\n")
  cat("Rows:", nrow(gg_dat), "\n")
  cat("Columns:", ncol(gg_dat), "\n\n")

  cat("First 10 column names:\n")
  print(names(gg_dat)[1:min(10, ncol(gg_dat))])

  cat("\nFirst 5 rows of first 10 columns:\n")
  print(head(gg_dat[, 1:min(10, ncol(gg_dat))], 5))

  invisible(gg_dat)
}