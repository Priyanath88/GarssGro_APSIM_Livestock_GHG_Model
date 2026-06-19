
# ============================================================
# Title:     GGTactical to APSIM
# Author:    Priyanath Jayasinghe
# Email:     priyanath.jayasinghe@csiro.au
# Created:   2026-04-10
#
# Description:
#   This script run GGTactical and creat .txt files to be read in read_apsiminput.R function and create excel file ready to feed into APSIMX for livestock simulation 
#Inputs - 
# Notes:
#   - Designed for multi-pasture systems (plant1, plant2, ...)
#   - Support for separate weather and soil file folders with customizable output names
# ============================================================

library(openxlsx)
library(apsimx)

# ============================================================
# CONFIGURATION - Modify these variables for different scenarios
# ============================================================

# Folder paths
ggtactical_dir <- "C:/alldata/automation_of_GG_to_Apsim/GGTactical/Windows/x86"
met_files_dir <- "C:/alldata/automation_of_GG_to_Apsim/met_files"  # Folder containing weather files
soil_files_dir <- "C:/alldata/automation_of_GG_to_Apsim/soil_files"  # Folder containing soil files
output_dir <- "C:/alldata/automation_of_GG_to_Apsim/outputs"  # Folder for saving outputs

# Batch processing options
run_all_met_files <- TRUE   # Set to TRUE to run all .txt files in met_files_dir, FALSE for single file
run_all_soil_files <- TRUE # Set to TRUE to run all .xml files in soil_files_dir, FALSE for single/default soil

# File format specifications
soil_file_extension <- ".xml"  # File extension for soil files (e.g., ".xml", ".apsimx")

# Simulation parameters (used only if run_all_met_files = FALSE or run_all_soil_files = FALSE)
weather_file <- "-35.3082_149.1245.txt"  # Name of weather file in met_files_dir
soil_scenario <- "DefaultSoil"  # Soil scenario identifier (used for naming, not file-based yet)
farm_system <- "Merino @ Southern Mallee"  # GGTactical farm system name
date_start <- "1960-01-01"
date_end <- "2000-12-31"

# APSIM configuration
apsim_file_path <- "C:/alldata/SouthernMallee_LS_NextGen/SouthernMallee/SLURP_Livestock.apsimx"

# ============================================================
# Run GrassGro (GGTactical)
# ============================================================

setwd(ggtactical_dir)  # Set working directory to GGTactical files

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Source the read_apsiminput function once
source(file.path(ggtactical_dir, "read_apsiminput.R"))

# ============================================================
# Determine which files to process
# ============================================================

# Process MET files
if (run_all_met_files) {
  # Get all .txt files from met_files_dir
  met_files <- list.files(met_files_dir, pattern = "\\.txt$", full.names = FALSE)
  
  if (length(met_files) == 0) {
    stop("No .txt files found in ", met_files_dir)
  }
  
  cat("Found", length(met_files), "met files to process:\n")
  cat(paste(" -", met_files), sep = "\n")
  cat("\n")
  
} else {
  # Use single weather file specified in configuration
  met_files <- weather_file
}

# Process SOIL files
if (run_all_soil_files) {
  # Create pattern for soil files (e.g., "\\.xml$" for XML files)
  soil_pattern <- paste0(gsub("\\.", "\\\\.", soil_file_extension), "$")
  
  # Get all soil files with specified extension from soil_files_dir
  soil_files <- list.files(soil_files_dir, pattern = soil_pattern, full.names = FALSE, ignore.case = TRUE)
  
  if (length(soil_files) == 0) {
    warning("No soil files found with extension '", soil_file_extension, "' in ", soil_files_dir, " - using default soil scenario")
    soil_files <- soil_scenario
  } else {
    # Extract soil scenario names from filenames (remove the specified extension)
    soil_files <- sub(paste0(gsub("\\.", "\\\\.", soil_file_extension), "$"), "", soil_files, ignore.case = TRUE)
    cat("Found", length(soil_files), "soil files (", soil_file_extension, ") to process:\n")
    cat(paste(" -", soil_files), sep = "\n")
    cat("\n")
  }
  
} else {
  # Use single soil scenario specified in configuration
  soil_files <- soil_scenario
}

# ============================================================
# Processing loop - Run for each soil and met file combination
# ============================================================

simulation_count <- 0

for (current_soil in soil_files) {
  
  for (current_weather_file in met_files) {
    
    simulation_count <- simulation_count + 1
    
    # Extract scenario name from filename (remove .txt extension)
    met_scenario <- sub("\\.txt$", "", current_weather_file)
    soil_scenario_label <- current_soil
    
    # Create combined scenario name
    if (run_all_soil_files) {
      current_scenario_name <- paste0(met_scenario, "_", soil_scenario_label)
    } else {
      current_scenario_name <- met_scenario
    }
    
    cat("\n", strrep("=", 60), "\n")
    cat("Processing simulation", simulation_count, "\n")
    cat("Scenario:", current_scenario_name, "\n")
    if (run_all_soil_files) {
      cat("  Met file:", met_scenario, "\n")
      cat("  Soil file:", soil_scenario_label, "\n")
    }
    cat(strrep("=", 60), "\n\n")
    
    # Construct full path to weather file
    weather_file_path <- file.path(met_files_dir, current_weather_file)
    
    # Check if weather file exists
    if (!file.exists(weather_file_path)) {
      cat("ERROR: Weather file not found -", weather_file_path, "\n")
      next
    }
    
    # Define output file names
    gg_output_file <- paste0("ggoutput_", current_scenario_name, ".txt")
    excel_output_file <- paste0("AnimalInput_", current_scenario_name, ".xlsx")
    
    # Construct GGTactical command
    command_txt <- sprintf(
      'GGTactical -f "farmsystems.ggfl|%s" -w "%s" -b %s -e %s -r "custom_library.gglb|APSIMSheepInput" -o "%s"',
      farm_system, 
      weather_file_path,
      date_start,
      date_end,
      gg_output_file
    )
    
    cat("Running GGTactical command...\n")
    shell(command_txt, wait = TRUE)
    
    # ============================================================
    # Import GrassGro output
    # ============================================================
    
    cat("Importing GrassGro output...\n")
    dtout <- read_apsiminput(gg_output_file)
    
    # Save Excel file with scenario name
    excel_full_path <- file.path(output_dir, excel_output_file)
    write.xlsx(dtout, file = excel_full_path, sheetName = "AnimalInput")
    cat("Excel output saved to:", excel_full_path, "\n")
    
    # ============================================================
    # Run APSIM
    # ============================================================
    
    apsim_output_name <- paste0("SLURP_Livestock_", current_scenario_name)
    
    apsimcommand_txt <- sprintf('"%s"', apsim_file_path)
    bat_file_name <- paste0("runapsim_", current_scenario_name, ".bat")
    writeLines(apsimcommand_txt, bat_file_name)
    
    cat("Running APSIM simulation for scenario:", current_scenario_name, "\n")
    shell(cmd = bat_file_name, intern = F, wait = T, translate = T)
    
    cat("Simulation completed successfully for:", current_scenario_name, "\n")
    cat("  GGTactical output:", file.path(ggtactical_dir, gg_output_file), "\n")
    cat("  Excel output:", excel_full_path, "\n\n")
  }
}

# ============================================================
# Summary
# ============================================================

cat("\n", strrep("=", 60), "\n")
cat("ALL SIMULATIONS COMPLETED SUCCESSFULLY!\n")
cat("Total simulations completed:", simulation_count, "\n")
cat(strrep("=", 60), "\n")
cat("Output directory:", output_dir, "\n")
cat("\nProcessing Summary:\n")
cat("  Met files processed:", length(met_files), "\n")
cat("  Soil scenarios processed:", length(soil_files), "\n")
cat("  Total combinations:", length(met_files) * length(soil_files), "\n")

