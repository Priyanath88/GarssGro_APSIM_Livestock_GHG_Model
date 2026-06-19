# =============================================================================
# GHG Multi-Site Analysis — APSIM Outputs
# =============================================================================
# Reads all .db files in apsim_outputs/ folder, pairs crop and livestock
# databases by site, and produces:
#   1. Annual GHG panel  (SOC change, N2O, CH4) — all sites
#   2. Seasonal GHG panel (livestock vs crop)   — all sites
#   3. GHG budget table  (t CO2eq/ha/yr)
#
# FILE NAMING CONVENTION (required):
#   {SiteName}_crop.db       e.g.  Kellerberrin_crop.db
#   {SiteName}_livestock.db  e.g.  Kellerberrin_livestock.db
#   Underscores within site name are allowed: Southern_Mallee_crop.db
# =============================================================================

library(tidyverse)
library(DBI)
library(RSQLite)
library(patchwork)

# ── 1. CONFIGURATION ─────────────────────────────────────────────────────────

apsim_dir  <- "apsim_outputs"                      # folder with .db files
output_dir <- file.path(apsim_dir, "plots")        # output folder for plots

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# IPCC AR5 GWP100 conversion factors
GWP_N2O  <- 298
GWP_CH4  <- 25
C_TO_CO2 <- 44 / 12   # C -> CO2 molecular weight ratio
N2O_EF   <- 1.57      # N2O-N to N2O conversion factor

# Plot colour palettes
SYS_PAL      <- c("Crop Rotation" = "#2166ac", "Livestock" = "#d6604d")
SEASON_ORDER <- c("Summer", "Autumn", "Winter", "Spring")

# ── 2. HELPER FUNCTIONS ──────────────────────────────────────────────────────

# Assign Southern Hemisphere seasons by month number
assign_season <- function(month) {
  case_when(
    month %in% c(12,  1,  2) ~ "Summer",
    month %in% c( 3,  4,  5) ~ "Autumn",
    month %in% c( 6,  7,  8) ~ "Winter",
    month %in% c( 9, 10, 11) ~ "Spring"
  )
}

# Determine database type from table names (crop vs livestock)
detect_type <- function(db_path) {
  con  <- dbConnect(SQLite(), db_path)
  tbls <- dbListTables(con)
  dbDisconnect(con)
  if ("LivestockReport" %in% tbls) "livestock" else "crop"
}

# Extract site name from filename: {SiteName}_crop.db  ->  "Site Name"
extract_site <- function(db_path) {
  fname <- tools::file_path_sans_ext(basename(db_path))
  fname <- sub("_(crop|livestock)$", "", fname, ignore.case = TRUE)
  gsub("_", " ", fname)
}

# Detect whether a livestock DB is a beef system by checking LivestockReport
# for non-zero BeefTotalMethane. Beef annual reports often miss mature adults,
# so annual CH4 must be aggregated from the daily report instead.
is_beef_system <- function(db_path) {
  con      <- dbConnect(SQLite(), db_path)
  ls_daily <- dbReadTable(con, "LivestockReport")
  dbDisconnect(con)
  beef_col <- grep("BeefTotalMethane", names(ls_daily), value = TRUE)
  if (length(beef_col) == 0) return(FALSE)
  any(sapply(beef_col, function(col) {
    vals <- ls_daily[[col]]
    is.numeric(vals) && any(!is.na(vals) & vals != 0)
  }))
}

# Identify non-zero annual methane columns (sheep systems only)
detect_annual_methane_cols <- function(annual_ls) {
  meth_cols <- grep("Methane|methane", names(annual_ls), value = TRUE)
  # Only annual total columns (not cumulative daily or _inT tonnes duplicates)
  annual_totals <- grep("^AnnualTotal", meth_cols, value = TRUE)
  annual_totals <- annual_totals[!grepl("_inT$", annual_totals)]
  # Keep only those with non-zero values
  annual_totals[sapply(annual_totals, function(col) {
    vals <- annual_ls[[col]]
    is.numeric(vals) && any(!is.na(vals) & vals != 0)
  })]
}

# Aggregate daily BeefTotalMethane to annual totals (beef systems)
annual_ch4_from_daily <- function(db_path) {
  con      <- dbConnect(SQLite(), db_path)
  ls_daily <- dbReadTable(con, "LivestockReport")
  dbDisconnect(con)
  beef_col <- grep("BeefTotalMethane", names(ls_daily), value = TRUE)
  nonzero  <- beef_col[sapply(beef_col, function(col) {
    vals <- ls_daily[[col]]
    is.numeric(vals) && any(!is.na(vals) & vals != 0)
  })]
  ls_daily |>
    mutate(Year = as.integer(substr(Date, 1, 4)),
           CH4  = rowSums(across(all_of(nonzero)), na.rm = TRUE)) |>
    group_by(Year) |>
    summarise(Total_CH4_kg = sum(CH4, na.rm = TRUE), .groups = "drop")
}

# Identify daily methane columns in LivestockReport (sheep or beef)
detect_daily_methane_cols <- function(ls_daily) {
  # Prefer BeefTotalMethane if present and non-zero (beef cattle)
  beef_col <- grep("BeefTotalMethane", names(ls_daily), value = TRUE)
  if (length(beef_col) > 0) {
    nonzero_beef <- beef_col[sapply(beef_col, function(col) {
      vals <- ls_daily[[col]]
      is.numeric(vals) && any(!is.na(vals) & vals != 0)
    })]
    if (length(nonzero_beef) > 0) return(nonzero_beef)
  }
  # Fallback: sheep — use Methane (adult) + MethaneYoung, ignore cumulative Total* cols
  sheep_cols <- grep("^GrassGro\\.Script\\.(Methane$|MethaneYoung$)",
                     names(ls_daily), value = TRUE)
  nonzero_sheep <- sheep_cols[sapply(sheep_cols, function(col) {
    vals <- ls_daily[[col]]
    is.numeric(vals) && any(!is.na(vals) & vals != 0)
  })]
  if (length(nonzero_sheep) > 0) return(nonzero_sheep)
  
  warning("No non-zero daily methane columns found.")
  return(NULL)
}

# Safely get SimulationID column (use constant if missing)
safe_sim_id <- function(df) {
  if ("SimulationID" %in% names(df)) df$SimulationID else rep(1L, nrow(df))
}

# ── 3. LOAD ANNUAL DATA ───────────────────────────────────────────────────────

load_annual_cr <- function(db_path, site_name) {
  con <- dbConnect(SQLite(), db_path)
  df  <- dbReadTable(con, "AnnualReport")
  dbDisconnect(con)
  df |>
    mutate(Year = as.integer(substr(Date, 1, 4))) |>
    group_by(Year) |>
    summarise(
      AnnualChangeInSoilCarbon = mean(AnnualChangeInSoilCarbon, na.rm = TRUE),
      AnnualTotalN2Oatm_1m     = mean(AnnualTotalN2Oatm_1m,     na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(System = "Crop Rotation", Site = site_name)
}

load_annual_ls <- function(db_path, site_name) {
  con       <- dbConnect(SQLite(), db_path)
  annual_ls <- dbReadTable(con, "AnnualReport")
  dbDisconnect(con)

  # Beef systems: annual report often omits mature adults — aggregate from daily
  if (is_beef_system(db_path)) {
    cat("  [", site_name, "] Beef system detected: computing annual CH4 from daily report.\n")
    ch4_by_year <- annual_ch4_from_daily(db_path)
    base <- annual_ls |>
      mutate(Year = as.integer(substr(Date, 1, 4))) |>
      group_by(Year) |>
      summarise(
        AnnualChangeInSoilCarbon = mean(AnnualChangeInSoilCarbon, na.rm = TRUE),
        AnnualTotalN2Oatm_1m     = mean(AnnualTotalN2Oatm_1m,     na.rm = TRUE),
        .groups = "drop"
      )
    return(left_join(base, ch4_by_year, by = "Year") |>
             mutate(System = "Livestock", Site = site_name))
  }

  # Sheep systems: use annual total methane columns from AnnualReport
  meth_cols <- detect_annual_methane_cols(annual_ls)
  if (length(meth_cols) == 0) {
    warning(site_name, ": No non-zero annual methane columns found. CH4 set to NA.")
  }

  annual_ls |>
    mutate(
      Year         = as.integer(substr(Date, 1, 4)),
      Total_CH4_kg = if (length(meth_cols) > 0)
                       rowSums(across(all_of(meth_cols)), na.rm = TRUE)
                     else NA_real_
    ) |>
    group_by(Year) |>
    summarise(
      AnnualChangeInSoilCarbon = mean(AnnualChangeInSoilCarbon, na.rm = TRUE),
      AnnualTotalN2Oatm_1m     = mean(AnnualTotalN2Oatm_1m,     na.rm = TRUE),
      Total_CH4_kg             = mean(Total_CH4_kg,              na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(System = "Livestock", Site = site_name)
}

# ── 4. LOAD SEASONAL DATA ────────────────────────────────────────────────────

load_seasonal_cr <- function(db_path, site_name) {
  con <- dbConnect(SQLite(), db_path)
  df  <- dbReadTable(con, "Report")
  dbDisconnect(con)

  # Handle N2O column name variation across sites
  n2o_col <- grep("N2Oatm", names(df), value = TRUE, ignore.case = TRUE)[1]
  if (is.na(n2o_col)) stop(site_name, ": Cannot find N2O column in daily CR Report.")

  df |>
    mutate(SimID = safe_sim_id(pick(everything()))) |>
    arrange(SimID, Date) |>
    group_by(SimID) |>
    mutate(SOC_change = c(NA, diff(TotalC))) |>
    ungroup() |>
    mutate(
      Month  = as.integer(substr(Date, 6, 7)),
      Season = factor(assign_season(Month), levels = SEASON_ORDER)
    ) |>
    group_by(Season) |>
    summarise(
      SOC_mean = mean(SOC_change,        na.rm = TRUE),
      SOC_sd   = sd(SOC_change,          na.rm = TRUE),
      N2O_mean = mean(.data[[n2o_col]],  na.rm = TRUE),
      N2O_sd   = sd(.data[[n2o_col]],    na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(System = "Crop Rotation", Site = site_name)
}

load_seasonal_ls_soc_n2o <- function(db_path, site_name) {
  con <- dbConnect(SQLite(), db_path)
  df  <- dbReadTable(con, "PastureReport")
  dbDisconnect(con)

  df |>
    mutate(SimID = safe_sim_id(pick(everything()))) |>
    arrange(SimID, Date) |>
    group_by(SimID) |>
    mutate(SOC_daily = c(NA, diff(ChangeInSoilCarbon))) |>
    ungroup() |>
    mutate(
      Month  = as.integer(substr(Date, 6, 7)),
      Season = factor(assign_season(Month), levels = SEASON_ORDER)
    ) |>
    group_by(Season) |>
    summarise(
      SOC_mean = mean(SOC_daily,             na.rm = TRUE),
      SOC_sd   = sd(SOC_daily,               na.rm = TRUE),
      N2O_mean = mean(TotalSoilN2Oemission,  na.rm = TRUE),
      N2O_sd   = sd(TotalSoilN2Oemission,    na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(System = "Livestock", Site = site_name)
}

load_seasonal_ls_ch4 <- function(db_path, site_name) {
  con      <- dbConnect(SQLite(), db_path)
  ls_daily <- dbReadTable(con, "LivestockReport")
  dbDisconnect(con)

  ch4_cols <- detect_daily_methane_cols(ls_daily)
  if (is.null(ch4_cols)) return(NULL)

  ls_daily |>
    mutate(
      Month  = as.integer(substr(Date, 6, 7)),
      Season = factor(assign_season(Month), levels = SEASON_ORDER),
      CH4    = rowSums(across(all_of(ch4_cols)), na.rm = TRUE)
    ) |>
    group_by(Season) |>
    summarise(
      CH4_mean = mean(CH4, na.rm = TRUE),
      CH4_sd   = sd(CH4,   na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(Site = site_name, System = "Livestock")
}

# ── 5. DISCOVER & PAIR DB FILES ──────────────────────────────────────────────

db_files <- list.files(apsim_dir, pattern = "\\.db$", full.names = TRUE)
if (length(db_files) == 0) stop("No .db files found in: ", apsim_dir)

file_tbl <- tibble(path = db_files) |>
  mutate(
    type = map_chr(path, detect_type),
    site = map_chr(path, extract_site)
  )

cat("\n=== Files detected ===\n")
print(file_tbl |> select(site, type, file = basename(path)))

# Keep only sites that have both crop and livestock
paired <- file_tbl |>
  group_by(site) |>
  filter(n_distinct(type) == 2) |>
  ungroup()

unpaired <- setdiff(file_tbl$site, paired$site)
if (length(unpaired) > 0)
  warning("These sites are missing a crop or livestock file and will be skipped: ",
          paste(unpaired, collapse = ", "))

site_names <- unique(paired$site)
cat("\nSites to analyse:", paste(site_names, collapse = ", "), "\n\n")

# ── 6. MAIN LOOP — LOAD ALL DATA ─────────────────────────────────────────────

all_annual       <- vector("list", length(site_names))
all_ch4_annual   <- vector("list", length(site_names))
all_seas_soc_n2o <- vector("list", length(site_names))
all_seas_ch4     <- vector("list", length(site_names))

for (i in seq_along(site_names)) {
  site    <- site_names[i]
  cr_path <- paired |> filter(site == !!site, type == "crop")      |> pull(path)
  ls_path <- paired |> filter(site == !!site, type == "livestock")  |> pull(path)

  cat("Loading site:", site, "\n")

  # Annual
  ann_cr <- load_annual_cr(cr_path, site)
  ann_ls <- load_annual_ls(ls_path, site)
  all_annual[[i]]     <- bind_rows(ann_cr, ann_ls)
  all_ch4_annual[[i]] <- ann_ls |> select(Year, Total_CH4_kg, Site)

  # Seasonal
  all_seas_soc_n2o[[i]] <- bind_rows(
    load_seasonal_cr(cr_path, site),
    load_seasonal_ls_soc_n2o(ls_path, site)
  )
  all_seas_ch4[[i]] <- load_seasonal_ls_ch4(ls_path, site)
}

annual_all   <- bind_rows(all_annual)
ch4_annual   <- bind_rows(all_ch4_annual)
seas_soc_n2o <- bind_rows(all_seas_soc_n2o)
seas_ch4_all <- bind_rows(all_seas_ch4)

# ── 7. GHG BUDGET TABLE ──────────────────────────────────────────────────────

ghg_budget <- annual_all |>
  group_by(Site, System) |>
  summarise(
    Mean_SOC_kg  = mean(AnnualChangeInSoilCarbon, na.rm = TRUE),
    Mean_N2O_kg  = mean(AnnualTotalN2Oatm_1m,     na.rm = TRUE),
    Mean_CH4_kg  = mean(Total_CH4_kg,              na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # SOC: negated so that C loss (negative APSIM value) = positive GWP emission
    # CH4: APSIM GrassGro reports in g/ha — divide by 1000 to convert to kg first
    SOC_CO2eq_t   = -Mean_SOC_kg          * C_TO_CO2 / 1000,
    N2O_CO2eq_t   =  Mean_N2O_kg          * N2O_EF * GWP_N2O / 1000,
    CH4_CO2eq_t   =  replace_na(Mean_CH4_kg / 1000 * GWP_CH4 / 1000, 0),
    Total_CO2eq_t = SOC_CO2eq_t + N2O_CO2eq_t + CH4_CO2eq_t
  ) |>
  select(Site, System, SOC_CO2eq_t, N2O_CO2eq_t, CH4_CO2eq_t, Total_CO2eq_t)

cat("\n=== GHG Budget (t CO2eq/ha/yr) ===\n")
print(ghg_budget)
write.csv(ghg_budget, file.path(output_dir, "ghg_budget.csv"), row.names = FALSE)
cat("Budget saved to:", file.path(output_dir, "ghg_budget.csv"), "\n\n")

# ── 8. SITE COLOUR PALETTE ───────────────────────────────────────────────────

n_sites     <- length(site_names)
site_colors <- setNames(
  colorRampPalette(c("#1b7837","#762a83","#e08214","#2166ac","#d6604d",
                     "#4dac26","#8073ac","#f1a340"))(n_sites),
  site_names
)

# ── 9. PLOT: ANNUAL GHG PANEL ────────────────────────────────────────────────

plot_width <- max(10, 3.5 * n_sites)

p_soc_ann <- ggplot(annual_all,
                    aes(x = Year, y = AnnualChangeInSoilCarbon, color = System)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SYS_PAL) +
  facet_wrap(~factor(Site, levels = site_names), nrow = 1) +
  labs(x = NULL, y = "SOC Change\n(kg C/ha/yr)", color = NULL,
       subtitle = "Soil Carbon Change") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

p_n2o_ann <- ggplot(annual_all,
                    aes(x = Year, y = AnnualTotalN2Oatm_1m, color = System)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = SYS_PAL) +
  facet_wrap(~factor(Site, levels = site_names), nrow = 1) +
  labs(x = NULL, y = "Annual N2O\n(kg N2O/ha/yr)", color = NULL,
       subtitle = "N2O Emissions") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

p_ch4_ann <- ggplot(ch4_annual |> filter(!is.na(Total_CH4_kg)),
                    aes(x = Year, y = Total_CH4_kg,
                        color = factor(Site, levels = site_names))) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = site_colors, name = "Site") +
  facet_wrap(~factor(Site, levels = site_names), nrow = 1, scales = "free_y") +
  labs(x = "Year", y = "Annual CH4\n(g/ha/yr)",
       subtitle = "CH4 Emissions — Livestock only") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

p_annual <- (p_soc_ann / p_n2o_ann / p_ch4_ann) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Annual GHG Emissions — All Sites",
    subtitle = "Blue = Crop Rotation  |  Red = Livestock  |  CH4: livestock only"
  ) &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "annual_ghg_all_sites.png"),
       p_annual, width = plot_width, height = 10, dpi = 300, limitsize = FALSE)
cat("Saved: annual_ghg_all_sites.png\n")

# ── 10. PLOT: SEASONAL LIVESTOCK VS CROP PANEL ───────────────────────────────

dodge <- position_dodge(width = 0.65)

p_soc_seas <- ggplot(seas_soc_n2o, aes(x = Season, y = SOC_mean, fill = System)) +
  geom_col(position = dodge, width = 0.6) +
  geom_errorbar(aes(ymin = SOC_mean - SOC_sd, ymax = SOC_mean + SOC_sd),
                position = dodge, width = 0.25, linewidth = 0.4) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = SYS_PAL) +
  facet_wrap(~factor(Site, levels = site_names), nrow = 1) +
  labs(x = NULL, y = "Daily SOC Change\n(kg C/ha/day)", fill = NULL,
       subtitle = "Soil Carbon Change") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

p_n2o_seas <- ggplot(seas_soc_n2o, aes(x = Season, y = N2O_mean, fill = System)) +
  geom_col(position = dodge, width = 0.6) +
  geom_errorbar(aes(ymin = pmax(N2O_mean - N2O_sd, 0), ymax = N2O_mean + N2O_sd),
                position = dodge, width = 0.25, linewidth = 0.4) +
  scale_fill_manual(values = SYS_PAL) +
  facet_wrap(~factor(Site, levels = site_names), nrow = 1) +
  labs(x = NULL, y = "Daily N2O Flux\n(kg N2O/ha/day)", fill = NULL,
       subtitle = "N2O Emissions") +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

p_ch4_seas <- ggplot(seas_ch4_all,
                     aes(x = Season, y = CH4_mean,
                         fill = factor(Site, levels = site_names))) +
  geom_col(position = dodge, width = 0.6) +
  geom_errorbar(aes(ymin = pmax(CH4_mean - CH4_sd, 0), ymax = CH4_mean + CH4_sd),
                position = dodge, width = 0.25, linewidth = 0.4) +
  scale_fill_manual(values = site_colors, name = "Site") +
  labs(x = "Season", y = "Daily CH4\n(g/ha/day)", fill = "Site",
       subtitle = "CH4 Emissions — Livestock only") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

p_seasonal <- (p_soc_seas / p_n2o_seas / p_ch4_seas) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Seasonal GHG: Livestock vs Crop Rotation — All Sites",
    subtitle = "Mean ± 1 SD  |  Southern Hemisphere seasons"
  ) &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "seasonal_ghg_all_sites.png"),
       p_seasonal, width = plot_width, height = 10, dpi = 300, limitsize = FALSE)
cat("Saved: seasonal_ghg_all_sites.png\n")

# ── 11. PLOT: NET GWP STACKED BAR (kg CO2eq/ha/yr) ──────────────────────────
# GWP100 (IPCC AR5): CH4 × 25 | N2O × 298 | SOC × 44/12
# NOTE: APSIM GrassGro reports CH4 in g/ha — divide by 1000 before applying GWP

gwp_components <- annual_all |>
  group_by(Site, System) |>
  summarise(
    SOC_kgC   = mean(AnnualChangeInSoilCarbon, na.rm = TRUE),
    N2O_kgN2O = mean(AnnualTotalN2Oatm_1m,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    # Use ch4_annual which contains corrected beef totals from daily aggregation
    ch4_annual |>
      group_by(Site) |>
      summarise(CH4_g = mean(Total_CH4_kg, na.rm = TRUE), .groups = "drop") |>
      mutate(System = "Livestock"),
    by = c("Site", "System")
  ) |>
  mutate(
    # SOC: negated so that C loss (negative APSIM value) = positive GWP emission
    SOC_CO2eq = -SOC_kgC   * C_TO_CO2,
    # N2O: kg N2O/ha/yr × GWP
    N2O_CO2eq =  N2O_kgN2O * GWP_N2O,
    # CH4: APSIM GrassGro reports in g/ha — divide by 1000 before applying GWP
    CH4_CO2eq =  replace_na(CH4_g / 1000 * GWP_CH4, 0)
  )

# Net GWP per site × system (for diamond annotation)
gwp_net <- gwp_components |>
  mutate(Net = SOC_CO2eq + N2O_CO2eq + CH4_CO2eq)

# Pivot to long for stacked bars
gwp_long <- gwp_components |>
  pivot_longer(cols = c(SOC_CO2eq, N2O_CO2eq, CH4_CO2eq),
               names_to  = "Component",
               values_to = "GWP_CO2eq") |>
  mutate(
    Component = factor(Component,
                       levels = c("CH4_CO2eq", "N2O_CO2eq", "SOC_CO2eq"),
                       labels = c("CH4", "N2O", "SOC")),
    Site   = factor(Site,   levels = site_names),
    System = factor(System, levels = c("Crop Rotation", "Livestock"))
  )

gwp_net <- gwp_net |>
  mutate(Site   = factor(Site,   levels = site_names),
         System = factor(System, levels = c("Crop Rotation", "Livestock")))

# Colour scheme: SOC = green (sink/source), N2O = orange, CH4 = magenta
GWP_PAL <- c("SOC" = "#4dac26", "N2O" = "#e08214", "CH4" = "#d01c8b")

p_gwp <- ggplot(gwp_long, aes(x = System, y = GWP_CO2eq, fill = Component)) +
  geom_col(width = 0.55, position = "stack") +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "grey30") +
  # Net GWP diamond
  geom_point(data = gwp_net,
             aes(x = System, y = Net),
             inherit.aes = FALSE,
             shape = 23, size = 3.5, fill = "white", color = "black", stroke = 0.8) +
  # Net GWP label
  geom_text(data = gwp_net,
            aes(x = System, y = Net, label = round(Net, 0)),
            inherit.aes = FALSE,
            vjust = -0.9, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = GWP_PAL, name = "GHG source") +
  scale_x_discrete(labels = c("Crop\nRotation", "Livestock")) +
  scale_y_continuous(breaks = seq(-500, 4000, by = 500)) +
  facet_wrap(~Site, nrow = 1) +
  labs(
    x        = NULL,
    y        = expression("Net GWP (kg CO"[2]*"eq ha"^{-1}*" yr"^{-1}*")"),
    title    = "Net Global Warming Potential by System and Site",
    subtitle = "GWP100 (IPCC AR5): CH\u2084 \u00d7 25  |  N\u2082O \u00d7 298  |  SOC \u00d7 44/12  |  \u25c6 = net GWP"
  ) +
  theme(
    strip.text      = element_text(face = "bold"),
    legend.position = "bottom",
    plot.subtitle   = element_text(size = 9, hjust = 0.5, margin = margin(b = 6))
  )

ggsave(file.path(output_dir, "net_gwp_by_system_site.png"),
       p_gwp, width = plot_width, height = 6, dpi = 300, limitsize = FALSE)
cat("Saved: net_gwp_by_system_site.png\n")

# Print net GWP summary
cat("\n=== Net GWP Summary (kg CO2eq/ha/yr) ===\n")
gwp_net |>
  select(Site, System, SOC_CO2eq, N2O_CO2eq, CH4_CO2eq, Net) |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  print()

cat("\n✓ Analysis complete. Outputs in:", output_dir, "\n")
