rename_apsim_columns <- function(x) {
  rename_map <- c(
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
    "Total_root_length_Annual_Ryegrass_P1_m_m2" = "plant1_root_length"
  )

  out <- ifelse(x %in% names(rename_map), unname(rename_map[x]), x)
  make.unique(out)
}

params <- c(
  # --- General / Animal ---
  "date",
  "urine_n",
  "faecal_n",
  "n_female_weaners",
  "n_female_1_2yo",
  "n_mature_females",
  "n_male_weaners",
  "n_male_1_2yo",
  "n_mature_males",
  "pasture_intake_total",
  "pasture_intake_female_weaners",
  "pasture_intake_female_1_2yo",
  "pasture_intake_female_mature",
  "pasture_intake_male_weaners",
  "pasture_intake_male_1_2yo",
  "pasture_intake_male_mature",
  "supplement_intake_total",
  "supplement_intake_female_weaners",
  "supplement_intake_female_1_2yo",
  "supplement_intake_female_mature",
  "supplement_intake_male_weaners",
  "supplement_intake_male_1_2yo",
  "supplement_intake_male_mature",
  "pet",
  "utilization_rate",
  "n_male_unweaned",
  "n_male_weaners_lamb",
  "n_male_yearlings",
  "n_female_unweaned",
  "n_female_weaners_lamb",
  "n_female_yearlings",
  "methane",
  "pasture_intake_unweaned",
  "pasture_intake_male_weaners_lamb",
  "pasture_intake_male_yearlings",
  "pasture_intake_female_weaners_lamb",
  "pasture_intake_female_yearlings",
  "supplement_intake_unweaned",
  "supplement_intake_male_weaners_lamb",
  "supplement_intake_male_yearlings",
  "supplement_intake_female_weaners_lamb",
  "supplement_intake_female_yearlings",
  "methane_young",
  "pasture_intake_total_young",
  "supplement_intake_total_young",

  # --- Plant 1 (Pasture) ---
  "plant1_digestibility_avg",
  "plant1_digestibility_green",
  "plant1_digestibility_dead_litter",

  "plant1_protein_avg",
  "plant1_protein_green",
  "plant1_protein_dead_litter",

  "plant1_herbage_seedlings",
  "plant1_herbage_live",
  "plant1_herbage_senescing",
  "plant1_herbage_dead",
  "plant1_herbage_litter",

  "plant1_rooting_depth",

  "plant1_loss_seedlings",
  "plant1_loss_live",
  "plant1_loss_senescing",
  "plant1_loss_dead",
  "plant1_loss_litter",
  "plant1_loss_roots",

  "plant1_root_mass",
  "plant1_root_length"
)

read_apsiminput <- function(path) {
  lines <- readLines(path, warn = FALSE)

  apsim_line <- grep("^APSIMInput\\s*$", lines)[1]
  if (is.na(apsim_line)) {
    stop("Could not find the APSIMInput section.")
  }

  header_lines <- lines[(apsim_line + 1):length(lines)]
  non_empty <- which(nzchar(trimws(header_lines)))
  if (length(non_empty) < 1) {
    stop("Could not find header lines after APSIMInput.")
  }

  group_header_line <- apsim_line + non_empty[1]
  detail_header_line <- if (length(non_empty) >= 2) apsim_line + non_empty[2] else NA_integer_

  data_search_start <- if (!is.na(detail_header_line)) detail_header_line + 1 else group_header_line + 1
  data_offset <- which(nzchar(trimws(lines[data_search_start:length(lines)])))[1]
  if (is.na(data_offset)) {
    stop("Could not find data rows after headers.")
  }
  data_start_line <- data_search_start + data_offset - 1

  split_tab <- function(x) strsplit(x, "\t", fixed = TRUE)[[1]]

  clean_name <- function(x) {
    x <- trimws(x)
    x <- gsub("[[:space:]/()-]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    x
  }

  if (is.na(detail_header_line)) {
    header <- split_tab(lines[group_header_line])
    col_names <- make.unique(clean_name(header))
    if (length(col_names) >= 1 && tolower(col_names[1]) == "date")
      col_names[1] <- "Date"
  } else {
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
  }

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

test_read_apsiminput <- function() {
  file_path <- "C:/Users/JAY045/Downloads/GGTactical 1/Windows/x86/ggoutput.txt"
  gg_dat <- read_apsiminput(file_path)

  cat("Import successful\n")
  cat("Rows:", nrow(gg_dat), "\n")
  cat("Columns:", ncol(gg_dat), "\n\n")

  cat("First 10 column names:\n")
  print(names(gg_dat)[1:min(10, ncol(gg_dat))])

  cat("\nFirst 5 rows of first 10 columns:\n")
  print(head(gg_dat[, 1:min(10, ncol(gg_dat))], 5))

  invisible(gg_dat)
}