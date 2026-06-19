
# ============================================================
# Title:     GGTactical Steers simulation .txt file reader
# Author:    Priyanath Jayasinghe
# Email:     priyanath.jayasinghe@csiro.au
# Created:   2026-06-18
#
# Description:
#   Processes GGTactical Steers simulation outputs (.txt) and formats
#   daily livestock and pasture data suitable for further analysis.
#   Pasture species: Panicum coloratum - Bambatsi (P1).
#   Animal classes: Weaners / 1-2 y.o. / 2-3 y.o. / Mature.
#
# Inputs:
#   - GGTactical output text files (Steers @ Chinchilla format)
#
# Notes:
#   - The file uses latin1 encoding (² appears as Â² in raw bytes).
#   - Column renaming is done positionally because the raw header
#     contains encoding artefacts and truncated species names.
#   - This file has a fixed 47-column structure; if columns change,
#     update ggtactical_col_names() accordingly.
# ============================================================

ggtactical_col_names <- function() {
  # Returns the 47 meaningful column names in output order.
  # Column order follows the APSIMInput section of the GGTactical output.
  c(
    "date",                         #  1  Date
    "urine_n",                      #  2  Nitrogen excretion/area - Urine N (kg/ha)
    "faecal_n",                     #  3  Nitrogen excretion/area - Faecal N (kg/ha)
    "n_weaners",                    #  4  Numbers - Weaners
    "n_1_2yo",                      #  5  Numbers - 1-2 y.o.
    "n_2_3yo",                      #  6  Numbers - 2-3 y.o.
    "n_mature",                     #  7  Numbers - Mature
    "dm_intake_total",              #  8  Total DM intake (kg/head)
    "pasture_intake_weaners",       #  9  Pasture intake by class - Weaners (kg/head)
    "pasture_intake_1_2yo",         # 10  Pasture intake by class - 1-2 y.o. (kg/head)
    "pasture_intake_2_3yo",         # 11  Pasture intake by class - 2-3 y.o. (kg/head)
    "pasture_intake_mature",        # 12  Pasture intake by class - Mature (kg/head)
    "supplement_intake_weaners",    # 13  Supplement intake by class - Weaners (kg/head)
    "supplement_intake_1_2yo",      # 14  Supplement intake by class - 1-2 y.o. (kg/head)
    "supplement_intake_2_3yo",      # 15  Supplement intake by class - 2-3 y.o. (kg/head)
    "supplement_intake_mature",     # 16  Supplement intake by class - Mature (kg/head)
    "pet",                          # 17  Potential evapotranspiration - P1 (mm)
    "utilization_rate",             # 18  Utilization rate (%)
    "methane_weaners",              # 19  Methane production - Weaners (g/head)
    "methane_1_2yo",                # 20  Methane production - 1-2 y.o. (g/head)
    "methane_2_3yo",                # 21  Methane production - 2-3 y.o. (g/head)
    "methane_mature",               # 22  Methane production - Mature (g/head)
    "plant1_digestibility_avg",     # 23  Digestibility P. coloratum - Average (P1) (%)
    "plant1_digestibility_green",   # 24  Digestibility P. coloratum - Green (P1) (%)
    "plant1_digestibility_dead_litter", # 25  Digestibility P. coloratum - Dead+Litter (P1) (%)
    "plant1_protein_avg",           # 26  Protein content P. coloratum - Average (P1) (%)
    "plant1_protein_green",         # 27  Protein content P. coloratum - Green (P1) (%)
    "plant1_protein_dead_litter",   # 28  Protein content P. coloratum - Dead+Litter (P1) (%)
    "plant1_herbage_seedlings",     # 29  Total herbage - Seedlings (P1) (kg/ha)
    "plant1_herbage_live",          # 30  Total herbage - Live (P1) (kg/ha)
    "plant1_herbage_senescing",     # 31  Total herbage - Senescing (P1) (kg/ha)
    "plant1_herbage_dead",          # 32  Total herbage - Dead (P1) (kg/ha)
    "plant1_herbage_litter",        # 33  Total herbage - Litter (P1) (kg/ha)
    "plant1_rooting_depth",         # 34  Rooting depth - P1 (mm)
    "plant1_loss_seedlings",        # 35  Loss rates - Seedlings (P1) (%/d)
    "plant1_loss_live",             # 36  Loss rates - Live (P1) (%/d)
    "plant1_loss_senescing",        # 37  Loss rates - Senescing (P1) (%/d)
    "plant1_loss_dead",             # 38  Loss rates - Dead (P1) (%/d)
    "plant1_loss_litter",           # 39  Loss rates - Litter (P1) (%/d)
    "plant1_loss_roots",            # 40  Loss rates - Roots (P1) (%/d)
    "plant1_root_mass",             # 41  Root mass - P1 (kg/ha)
    "plant1_root_length",           # 42  Total root length - P1 (m/m²)
    "plant1_sward_height",          # 43  Sward height - P1 (mm)
    "plant1_ground_cover",          # 44  Ground cover - P1 (m²/m²)
    "plant1_protein_content_green",   # 45  Protein content (Average P1) (%)
    "plant1_growth_rate",           # 46  Growth rate - P1 (kg/ha/d)
    "plant1_LAI"                    # 47  Green area index - P1 (m²/m²)
  )
}


read_ggtactical_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")

  apsim_line <- grep("^APSIMInput\\s*$", lines)[1]
  if (is.na(apsim_line)) {
    stop("Could not find the APSIMInput section.")
  }

  data_start_line <- apsim_line + 4

  expected_names <- ggtactical_col_names()

  dat <- read.delim(
    path,
    sep              = "\t",
    header           = FALSE,
    skip             = data_start_line - 1,
    col.names        = expected_names,
    na.strings       = c("n/a", "NA", ""),
    check.names      = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding     = "latin1"
  )

  # Verify column count matches expectations
  if (ncol(dat) != length(expected_names)) {
    warning(
      "Expected ", length(expected_names), " columns but found ", ncol(dat),
      ". Column names may be misaligned."
    )
  }

  if ("date" %in% names(dat)) {
    dat$date <- as.Date(dat$date, format = "%d-%m-%Y")
  }

  dat
}


read_ggtactical <- function() {
  file_path <- "C:/alldata/automation_of_GG_to_Apsim/GGTactical/Windows/x86/ggoutput_Chinchilla.txt"
  gg_dat <- read_ggtactical_file(file_path)

  cat("Import successful\n")
  cat("Rows:", nrow(gg_dat), "\n")
  cat("Columns:", ncol(gg_dat), "\n\n")

  cat("First 10 column names:\n")
  print(names(gg_dat)[1:min(10, ncol(gg_dat))])

  cat("\nFirst 5 rows of first 10 columns:\n")
  print(head(gg_dat[, 1:min(10, ncol(gg_dat))], 5))

  invisible(gg_dat)
}
