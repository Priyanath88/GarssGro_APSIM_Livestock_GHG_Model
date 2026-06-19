# GHG Emissions Analysis - Crop Rotation vs Livestock
# APSIM Simulation Outputs - Southern Mallee
# Date: 2026-05-13
# ============================================================

library(tidyverse)

output_plot_dir <- "plots"
if (!dir.exists(output_plot_dir)) {
  dir.create(output_plot_dir, recursive = TRUE)
}

# ============================================================
# 1. SUMMARY STATISTICS
# ============================================================

# --- tbl_daily_CR ---
cat("=== tbl_daily_CR ===\n")
cat("Dimensions:", nrow(tbl_daily_CR), "rows x", ncol(tbl_daily_CR), "cols\n\n")

tbl_daily_CR |>
  select(where(is.numeric)) |>
  summary()

# --- tbl_annual_CR ---
cat("=== tbl_annual_CR ===\n")
cat("Dimensions:", nrow(tbl_annual_CR), "rows x", ncol(tbl_annual_CR), "cols\n\n")

tbl_annual_CR |>
  select(where(is.numeric)) |>
  summary()

# --- tbl_annual_livestock ---
cat("=== tbl_annual_livestock ===\n")
cat("Dimensions:", nrow(tbl_annual_livestock), "rows x", ncol(tbl_annual_livestock), "cols\n\n")

tbl_annual_livestock |>
  select(where(is.numeric)) |>
  summary()


# ============================================================
# 2. PLOT: AnnualChangeInSoilCarbon - Crop Rotation
# ============================================================

# Average across crop rotation simulations
avg_line <- tbl_annual_CR |>
  mutate(Date = as.integer(substr(Date, 1, 4))) |>
  group_by(Date) |>
  summarise(AnnualChangeInSoilCarbon = mean(AnnualChangeInSoilCarbon, na.rm = TRUE))

p_soil_cr <- tbl_annual_CR |>
  mutate(Date = as.integer(substr(Date, 1, 4))) |>
  ggplot(aes(x = Date, y = AnnualChangeInSoilCarbon, color = factor(SimulationID))) +
  geom_line() +
  scale_color_grey(start = 0.2, end = 0.7) +
  geom_line(data = avg_line, aes(x = Date, y = AnnualChangeInSoilCarbon),
            color = "black", linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
  labs(
    x = "Year",
    y = "Annual Change in Soil Carbon",
    color = "Simulation ID",
    caption = "Dashed black line = average across simulations"
  )

ggsave(filename = file.path(output_plot_dir, "soilcarbon_crop_rotation.png"), plot = p_soil_cr, width = 10, height = 6, dpi = 300)


# ============================================================
# 3. PLOT: AnnualChangeInSoilCarbon - Livestock
# ============================================================

# Average across livestock simulations
avg_line_ls <- tbl_annual_livestock |>
  mutate(Date = as.integer(substr(Date, 1, 4))) |>
  group_by(Date) |>
  summarise(AnnualChangeInSoilCarbon = mean(AnnualChangeInSoilCarbon, na.rm = TRUE))

p_soil_ls <- tbl_annual_livestock |>
  mutate(
    Date = as.integer(substr(Date, 1, 4)),
    SimulationID = factor(SimulationID)
  ) |>
  ggplot(aes(x = Date, y = AnnualChangeInSoilCarbon, color = SimulationID)) +
  geom_line() +
  scale_color_grey(start = 0.2, end = 0.7) +
  geom_line(data = avg_line_ls, aes(x = Date, y = AnnualChangeInSoilCarbon),
            color = "black", linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
  labs(
    x = "Year",
    y = "Annual Change in Soil Carbon",
    color = "Simulation ID",
    caption = "Dashed black line = average across simulations"
  )

ggsave(filename = file.path(output_plot_dir, "soilcarbon_livestock.png"), plot = p_soil_ls, width = 10, height = 6, dpi = 300)


# ============================================================
# 4. PLOT: AnnualChangeInSoilCarbon - Crop vs Livestock Average
# ============================================================

avg_combined <- bind_rows(
  avg_line    |> mutate(Source = "Crop Rotation"),
  avg_line_ls |> mutate(Source = "Livestock")
)

p_soil_avg <- ggplot(avg_combined, aes(x = Date, y = AnnualChangeInSoilCarbon, color = Source)) +
  geom_line() +
  scale_color_grey(start = 0.2, end = 0.7) + geom_point() +
  labs(
    x = "Year",
    y = "Annual Change in Soil Carbon (kg/ha)",
    color = "System"
  )

ggsave(filename = file.path(output_plot_dir, "soilcarbon_crop_vs_livestock_avg.png"), plot = p_soil_avg, width = 6, height = 4, dpi = 300)


# ============================================================
# 5. PLOT: AnnualTotalN2Oatm_1m - Crop vs Livestock Average
# ============================================================

avg_n2o <- bind_rows(
  tbl_annual_CR |>
    mutate(Date = as.integer(substr(Date, 1, 4))) |>
    group_by(Date) |>
    summarise(AnnualTotalN2Oatm_1m = mean(AnnualTotalN2Oatm_1m, na.rm = TRUE)) |>
    mutate(Source = "Crop Rotation"),
  tbl_annual_livestock |>
    mutate(Date = as.integer(substr(Date, 1, 4))) |>
    group_by(Date) |>
    summarise(AnnualTotalN2Oatm_1m = mean(AnnualTotalN2Oatm_1m, na.rm = TRUE)) |>
    mutate(Source = "Livestock")
)

p_n2o_avg <- ggplot(avg_n2o, aes(x = Date, y = AnnualTotalN2Oatm_1m, color = Source)) +
  geom_line() +
  scale_color_grey(start = 0.2, end = 0.7) + geom_point() +
  labs(
    x = "Year",
    y = "Annual Total N2O Atmospheric (1m) (kg/ha)",
    color = "System"
  )

ggsave(filename = file.path(output_plot_dir, "n2o_crop_vs_livestock_avg.png"), plot = p_n2o_avg, width = 6, height = 4, dpi = 300)


# ============================================================
# 6. PLOT: Average Annual CH4 Emissions - Livestock (2 simulations)
# ============================================================

ch4_avg_two_sims <- tbl_annual_livestock |>
  mutate(
    Date = as.integer(substr(Date, 1, 4)),
    Total_CH4_kg = AnnualTotalMethaneAdult + AnnualTotalMethaneYoung
  ) |>
  group_by(Date) |>
  summarise(
    Avg_Total_CH4_kg = mean(Total_CH4_kg, na.rm = TRUE),
    Simulation_Count = n_distinct(SimulationID),
    .groups = "drop"
  )

p_ch4_avg <- ch4_avg_two_sims |>
  ggplot(aes(x = Date, y = Avg_Total_CH4_kg)) +
  geom_line(color = "grey40") +
  geom_point(color = "grey40") +
  labs(
    x = "Year",
    y = "Average Annual CH4 Emissions (kg/ha/yr)"
  ) 

print(p_ch4_avg)
ggsave(filename = file.path(output_plot_dir, "ch4_avg_two_simulations.png"), plot = p_ch4_avg, width = 6, height = 4, dpi = 300)


# ============================================================
# 7. GHG EMISSIONS BUDGET (kg & tonnes CO2eq/ha/yr)
# ============================================================

# Conversion factors (IPCC AR5 GWP100):
#   Soil C  -> CO2eq: x (44/12)  [C to CO2 molecular weight ratio]
#   N2O     -> CO2eq: x 298
#   CH4     -> CO2eq: x 25

# --- Soil Carbon & N2O averages ---
ghg_summary <- bind_rows(
  tbl_annual_CR |> mutate(Source = "Crop Rotation"),
  tbl_annual_livestock |> mutate(Source = "Livestock")
) |>
  group_by(Source) |>
  summarise(
    Mean_AnnualChangeInSoilCarbon = mean(AnnualChangeInSoilCarbon, na.rm = TRUE),
    Mean_AnnualTotalN2Oatm_1m    = mean(AnnualTotalN2Oatm_1m, na.rm = TRUE)
  ) |>
  mutate(
    SoilC_CO2eq_kg = Mean_AnnualChangeInSoilCarbon * (44 / 12),
    N2O_CO2eq_kg   = Mean_AnnualTotalN2Oatm_1m * 1.57 * 298,
    SoilC_CO2eq_t  = SoilC_CO2eq_kg / 1000,
    N2O_CO2eq_t    = N2O_CO2eq_kg   / 1000
  )

# --- Methane (livestock only, values in kg/ha/yr) ---
ch4_summary <- tbl_annual_livestock |>
  summarise(
    Mean_MethaneAdult = mean(AnnualTotalMethaneAdult, na.rm = TRUE),
    Mean_MethaneYoung = mean(AnnualTotalMethaneYoung, na.rm = TRUE)
  ) |>
  mutate(
    Source        = "Livestock",
    Total_CH4_kg  = Mean_MethaneAdult + Mean_MethaneYoung,
    CH4_CO2eq_kg  = Total_CH4_kg * 25,
    CH4_CO2eq_t   = CH4_CO2eq_kg / 1000
  )

# --- Combined emissions budget (tonnes CO2eq/ha/yr) ---
ghg_budget <- ghg_summary |>
  left_join(ch4_summary |> select(Source, CH4_CO2eq_t), by = "Source") |>
  mutate(
    CH4_CO2eq_t   = replace_na(CH4_CO2eq_t, 0),
    Total_CO2eq_t = SoilC_CO2eq_t + N2O_CO2eq_t + CH4_CO2eq_t
  ) |>
  select(Source, SoilC_CO2eq_t, N2O_CO2eq_t, CH4_CO2eq_t, Total_CO2eq_t)

print(ghg_budget)
