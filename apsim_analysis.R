# Crop-Livestock Site Spatial Workflow in R
#
# This script reads the provided spatial layers, reprojects to a common CRS,
# extracts the most common nearby soil classes (decoded to names), links to APsoil profiles,
# and attaches agro-climatic region and state boundary information.

setwd("C:/alldata/site_analyse")
# Required packages -------------------------------------------------------
required_pkgs <- c("sf", "terra", "dplyr", "exactextractr")
missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cran.r-project.org")
}

library(sf)
library(terra)
library(dplyr)
library(exactextractr)

# File paths ---------------------------------------------------------------
site_path <- "C:/alldata/site_analyse/crop_livestock_site.csv"
agro_path <- "C:/alldata/site_analyse/Agro-climatic layer.shp"
state_path <- "C:/alldata/site_analyse/State border layer.shp"
apsoil_path <- "C:/alldata/site_analyse/APsoil_soilname_locations.csv"  # change to .shp or .kml if needed
soil_raster_path <- "C:/alldata/site_analyse/Soil classification layer.tif"
mixed_raster_path <- "C:/alldata/site_analyse/Crop livestock mixed layer.tif"
weather_path <- "C:/alldata/site_analyse/Silo weather data layer.csv"  # weather station CSV
## SA3 and land-use layers removed per user request

# Output paths -------------------------------------------------------------
output_sites_csv <- "sites_with_attributes.csv"
output_sites_shp <- "sites_with_attributes.shp"
output_mixed_cropped <- "Crop_livestock_mixed_cropped_4326.tif"
## SA3 and land-use outputs removed

# Common working CRS -------------------------------------------------------
common_crs <- "EPSG:4326"

# Helper functions ---------------------------------------------------------
read_vector_safe <- function(path, target_crs = common_crs) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path))
  }
  obj <- st_read(path, quiet = TRUE)
  st_transform(obj, target_crs)
}

read_apsoil <- function(path, target_crs = common_crs) {
  if (!file.exists(path)) {
    stop(sprintf("APsoil file not found: %s", path))
  }
  # handle CSV separately (point locations with lat/lon)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    df <- read.csv(path, stringsAsFactors = FALSE)
    lat_col <- grep("^(lat|latitude)$", names(df), ignore.case = TRUE, value = TRUE)
    lon_col <- grep("^(lon|long|longitude|lon_deg|lon_dd)$", names(df), ignore.case = TRUE, value = TRUE)
    if (length(lat_col) == 1 && length(lon_col) == 1) {
      sf_obj <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
      return(st_transform(sf_obj, target_crs))
    } else {
      # return data.frame if no lat/lon to allow caller to decide
      warning("APsoil CSV read but no lat/lon columns detected; returning data.frame.")
      return(df)
    }
  } else if (grepl("\\.(kml|kmz)$", path, ignore.case = TRUE)) {
    if (grepl("\\.kmz$", path, ignore.case = TRUE)) {
      tmpdir <- tempfile(pattern = "kmz_extract")
      dir.create(tmpdir)
      unzip(path, exdir = tmpdir)
      kml_files <- list.files(tmpdir, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)
      if (length(kml_files) == 0) {
        stop(sprintf("No KML found inside KMZ archive: %s", path))
      }
      path <- kml_files[1]
    }
    layers <- st_layers(path)$name
    if (length(layers) == 1) {
      obj <- st_read(path, layer = layers[1], quiet = TRUE)
    } else {
      obj <- st_read(path, quiet = TRUE)
    }
  } else {
    obj <- st_read(path, quiet = TRUE)
  }
  # Only transform if object is sf; otherwise return as-is
  if (inherits(obj, "sf")) {
    st_transform(obj, target_crs)
  } else {
    obj
  }
}

read_weather_csv <- function(path, target_crs = common_crs) {
  if (!file.exists(path)) {
    stop(sprintf("Weather CSV file not found: %s", path))
  }
  weather_df <- read.csv(path, stringsAsFactors = FALSE)
  lat_col <- grep("^(lat|latitude)$", names(weather_df), ignore.case = TRUE, value = TRUE)
  lon_col <- grep("^(lon|long|longitude)$", names(weather_df), ignore.case = TRUE, value = TRUE)
  if (length(lat_col) != 1 || length(lon_col) != 1) {
    stop("Weather CSV must contain exactly one latitude column and one longitude column.")
  }
  weather_sf <- st_as_sf(weather_df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  st_transform(weather_sf, target_crs)
}

# Read sites from CSV (if user provides CSV with name/lat/lon columns)
read_sites_csv <- function(path, target_crs = common_crs) {
  if (!file.exists(path)) stop(sprintf("Site CSV not found: %s", path))
  df <- read.csv(path, stringsAsFactors = FALSE)
  # find lat/lon columns
  lat_col <- grep("^(lat|latitude)$", names(df), ignore.case = TRUE, value = TRUE)
  lon_col <- grep("^(lon|long|longitude)$", names(df), ignore.case = TRUE, value = TRUE)
  if (length(lat_col) != 1 || length(lon_col) != 1) {
    stop("Site CSV must contain exactly one latitude column and one longitude column.")
  }
  # find name column (optional)
  name_col <- grep("^name$", names(df), ignore.case = TRUE, value = TRUE)
  if (length(name_col) == 0) {
    df$site_name <- as.character(seq_len(nrow(df)))
    name_col <- "site_name"
  }
  sites_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  # ensure a standard name column exists
  if (!"name" %in% names(sites_sf)) {
    sites_sf$name <- sites_sf[[name_col[1]]]
  }
  st_transform(sites_sf, target_crs)
}

top_n_values <- function(values, n = 3) {
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_character_)
  freq <- sort(table(values), decreasing = TRUE)
  names(freq)[seq_len(min(n, length(freq)))]
}

# Ensure object is an sf; convert data.frame with lat/lon if needed
ensure_sf <- function(obj, lon_candidates = c("lon","long","longitude","x"), lat_candidates = c("lat","latitude","y"), crs = 4326) {
  if (inherits(obj, "sf")) return(obj)
  if (is.data.frame(obj)) {
    names_lower <- tolower(names(obj))
    lon_idx <- which(names_lower %in% lon_candidates)
    lat_idx <- which(names_lower %in% lat_candidates)
    if (length(lon_idx) >= 1 && length(lat_idx) >= 1) {
      lon_col <- names(obj)[lon_idx[1]]
      lat_col <- names(obj)[lat_idx[1]]
      return(st_as_sf(obj, coords = c(lon_col, lat_col), crs = crs, remove = FALSE))
    }
    stop("Data.frame provided cannot be converted to sf: no lat/lon columns detected.")
  }
  stop("Object is not an sf or data.frame; cannot convert to sf.")
}

decode_soil_codes <- function(codes) {
  mapping <- c(
    "1" = "Vertosol",
    "2" = "Sodosol",
    "3" = "Dermosol",
    "4" = "Chromosol",
    "5" = "Ferrosol",
    "6" = "Kurosol",
    "7" = "Tenosol",
    "8" = "Kandosol",
    "9" = "Hydrosol",
    "10" = "Podosol",
    "11" = "Rudosol",
    "12" = "Calcarasol",
    "13" = "Organosol",
    "14" = "Anthroposol"
  )
  codes_chr <- as.character(codes)
  decoded <- mapping[codes_chr]
  decoded[is.na(decoded)] <- codes_chr[is.na(decoded)]
  decoded
}

selected_landuse_categories <- c(
  "Cereals",
  "Oil seeds",
  "Legumes",
  "Irrigated cereals",
  "Irrigated oil seeds",
  "Irrigated legumes",
  "Cotton",
  "Irrigated cotton",
  "Grazing modified pastures",
  "Hay & Silage",
  "Irrigated modified pastures",
  "Irrigated Hay & Silage"
)

get_landuse_category_names <- function(rast) {
  cats <- NULL
  if (!is.null(levels(rast))) {
    levs <- levels(rast)[[1]]
    if (is.data.frame(levs) && ncol(levs) >= 2) {
      cats <- levs
    }
  }
  if (is.null(cats)) {
    cats <- tryCatch(terra::categories(rast)[[1]], error = function(e) NULL)
  }
  if (is.null(cats) || !is.data.frame(cats)) {
    return(NULL)
  }
  cats
}

detect_sa3_id_field <- function(sf_obj) {
  candidates <- c(
    "SA3_CODE", "SA3_NAME", "SA3_NAME16", "SA3_NAME_2016",
    "sa3_code", "sa3_name", "SA3_NAME_2021", "SA3_CODE_2016",
    "SA3_MAINCODE_2016", "SA3_NAME_2011"
  )
  matches <- intersect(candidates, names(sf_obj))
  if (length(matches) > 0) return(matches[1])
  non_geom <- setdiff(names(sf_obj), attr(sf_obj, "sf_column"))
  if (length(non_geom) > 0) return(non_geom[1])
  stop("No attribute field found for SA3 ID/name.")
}

extract_top_soils <- function(df, n = 3) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(top3 = NA_character_, top3_prop = NA_character_))
  }
  if (!"coverage_fraction" %in% names(df)) {
    df$coverage_fraction <- 1
  }
  df <- df[!is.na(df$value), ]
  if (nrow(df) == 0) {
    return(list(top3 = NA_character_, top3_prop = NA_character_))
  }
  agg <- aggregate(coverage_fraction ~ value, data = df, sum)
  agg <- agg[order(-agg$coverage_fraction), ]
  total <- sum(agg$coverage_fraction)
  topn <- head(agg, n)
  top_codes <- topn$value
  top_names <- decode_soil_codes(top_codes)
  props <- topn$coverage_fraction / total
  prop_strings <- sprintf("%s: %.1f%%", top_names, props * 100)
  list(
    top3 = paste(top_names, collapse = ", "),
    top3_prop = paste(prop_strings, collapse = "; ")
  )
}

find_apsoil_field <- function(apsoil) {
  preferred <- c("Soil name", "SoilName", "SoilType", "Soil Type", "soilname", "soiltype", "Soil")
  names_lower <- tolower(names(apsoil))
  for (pref in preferred) {
    idx <- which(names_lower == tolower(pref))
    if (length(idx) > 0) {
      return(names(apsoil)[idx[1]])
    }
  }
  candidates <- grep("soil|type|name", names(apsoil), ignore.case = TRUE, value = TRUE)
  if (length(candidates) == 0) {
    return(NULL)
  }
  candidates[1]
}

extract_top_soils_from_apsoil <- function(sites, site_buffers, apsoil, field, n = 3) {
  if (is.null(field) || !field %in% names(apsoil)) {
    return(rep(list(list(top3 = NA_character_, top3_prop = NA_character_)), nrow(site_buffers)))
  }
  
  site_idx <- st_intersects(site_buffers, apsoil)
  lapply(seq_along(site_idx), function(i) {
    ids <- site_idx[[i]]
    if (length(ids) == 0) {
      return(list(top3 = NA_character_, top3_prop = NA_character_))
    }
    site_geom <- st_geometry(sites[i, ])
    dists <- as.numeric(st_distance(site_geom, st_geometry(apsoil[ids, ])))
    order_idx <- order(dists)
    ordered_names <- as.character(apsoil[[field]][ids][order_idx])
    unique_names <- unique(ordered_names)
    top_names <- head(unique_names, n)
    if (length(top_names) == 0) {
      return(list(top3 = NA_character_, top3_prop = NA_character_))
    }
    top_dists <- dists[order_idx][match(top_names, ordered_names)]
    dist_strings <- sprintf("%s (%.1f km)", top_names, top_dists / 1000)
    list(
      top3 = paste(top_names, collapse = ", "),
      top3_prop = paste(dist_strings, collapse = "; ")
    )
  })
}

# Read and reproject vector layers ----------------------------------------
cat("Reading vector layers...\n")
# allow sites to be provided as a CSV with name/lat/lon columns
if (grepl("\\.csv$", site_path, ignore.case = TRUE)) {
  cat(sprintf("Reading sites from CSV: %s\n", site_path))
  sites <- read_sites_csv(site_path)
} else {
  sites <- read_vector_safe(site_path)
}
agro <- read_vector_safe(agro_path)
state <- read_vector_safe(state_path)
apsoil <- read_apsoil(apsoil_path)

# If a CSV of APsoil locations exists, prefer it (user-provided table)
apsoil_csv_path <- "C:/alldata/site_analyse/APsoil_soilname_locations.csv"
if (file.exists(apsoil_csv_path)) {
  apsoil_csv_df <- read.csv(apsoil_csv_path, stringsAsFactors = FALSE)
  lat_col <- grep("^(lat|latitude)$", names(apsoil_csv_df), ignore.case = TRUE, value = TRUE)
  lon_col <- grep("^(lon|long|longitude)$", names(apsoil_csv_df), ignore.case = TRUE, value = TRUE)
  if (length(lat_col) == 1 && length(lon_col) == 1) {
    # detect soil name field in CSV
    soil_field_candidates <- c("Soil name","SoilName","Soil Type","SoilType","Soil","soilname","soiltype","soil")
    soil_field <- intersect(soil_field_candidates, names(apsoil_csv_df))
    if (length(soil_field) == 0) {
      soil_field <- grep("soil|type|name", names(apsoil_csv_df), ignore.case = TRUE, value = TRUE)
    }
    soil_field <- if (length(soil_field) > 0) soil_field[1] else NULL

    apsoil_csv_sf <- st_as_sf(apsoil_csv_df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
    apsoil_csv_sf <- st_transform(apsoil_csv_sf, common_crs)

    if (!is.null(soil_field)) {
      # standardise soil name field to 'SoilName' for downstream code
      names(apsoil_csv_sf)[which(names(apsoil_csv_sf) == soil_field)] <- "SoilName"
    }
    # detect APsoil identifier column (e.g. apsoil_nu, apsoil_no, apsoil_num)
    apsoil_id_candidates <- grep("apsoil|ap_soil|apsoil_no|apsoil_nu|apsoil_num", names(apsoil_csv_df), ignore.case = TRUE, value = TRUE)
    if (length(apsoil_id_candidates) > 0) {
      names(apsoil_csv_sf)[which(names(apsoil_csv_sf) == apsoil_id_candidates[1])] <- "apsoil_number"
    }

    apsoil <- apsoil_csv_sf
    cat("Using APsoil CSV:", apsoil_csv_path, "as primary APsoil layer.\n")
  } else {
    warning("APsoil CSV found but lat/lon columns not detected; ignoring CSV.")
  }
}

# Read and reproject raster layers ----------------------------------------
cat("Reading raster layers...\n")
soil_rast <- rast(soil_raster_path)
mixed_rast <- rast(mixed_raster_path)

if (crs(soil_rast) != common_crs) {
  soil_rast <- project(soil_rast, common_crs)
}
if (crs(mixed_rast) != common_crs) {
  mixed_rast <- project(mixed_rast, common_crs)
}

# Save reprojected mixed raster -------------------------------------------
cat("Writing reprojected mixed raster...\n")
writeRaster(mixed_rast, output_mixed_cropped, overwrite = TRUE)

# Extract top 3 common soil types around each site ------------------------
cat("Extracting top 3 soil types for each site with 20km buffers...\n")
if (!"site_id" %in% names(sites)) {
  sites$site_id <- seq_len(nrow(sites))
}
site_buffer_crs <- 3577  # Australia Albers, meters
sites_m <- st_transform(sites, site_buffer_crs)
site_buffers_m <- st_buffer(sites_m, dist = 20000)  # 20 km buffer in meters
site_buffers <- st_transform(site_buffers_m, common_crs)

soil_values <- exact_extract(soil_rast, site_buffers)
soil_summary <- lapply(soil_values, extract_top_soils, n = 3)

sites$top3_soil <- vapply(soil_summary, `[[`, "top3", FUN.VALUE = character(1), USE.NAMES = FALSE)
sites$top3_soil_prop <- vapply(soil_summary, `[[`, "top3_prop", FUN.VALUE = character(1), USE.NAMES = FALSE)

cat("Extracting top 3 APsoil soil names for each site with 20km buffers...\n")
# Ensure APsoil is an sf (convert if CSV/data.frame provided)
apsoil <- tryCatch(ensure_sf(apsoil, crs = common_crs), error = function(e) {
  warning("APsoil layer could not be converted to sf: ", e$message)
  return(NULL)
})

apsoil_field <- if (!is.null(apsoil)) find_apsoil_field(apsoil) else NULL
if (is.null(apsoil) || is.null(apsoil_field)) {
  warning("No APsoil attribute field matching Soil name/SoilType was found or APsoil unavailable; skipping APsoil top soil extraction.")
  sites$top3_apsoil <- NA_character_
  sites$top3_apsoil_prop <- NA_character_
  sites$top3_apsoil_numbers <- NA_character_
} else {
  cat(sprintf("Using APsoil attribute field '%s' for top soil extraction.\n", apsoil_field))
  apsoil_m <- st_transform(apsoil, site_buffer_crs)

  # helper: extract top-3 APsoil names and numbers for each site buffer
  extract_top_apsoil_info <- function(sites, site_buffers, apsoil, name_field, id_field = "apsoil_number", n = 3) {
    if (is.null(name_field) || !name_field %in% names(apsoil)) {
      return(rep(list(list(names = NA_character_, ids = NA_character_, prop = NA_character_)), nrow(site_buffers)))
    }
    # ensure id_field exists (may be NA)
    if (!id_field %in% names(apsoil)) {
      apsoil[[id_field]] <- NA_character_
    }
    site_idx <- st_intersects(site_buffers, apsoil)
    lapply(seq_along(site_idx), function(i) {
      ids <- site_idx[[i]]
      site_geom <- st_geometry(sites[i, ])
      if (length(ids) == 0) {
        # fallback: nearest features overall
        d <- as.numeric(st_distance(site_geom, st_geometry(apsoil)))
        ord <- order(d, na.last = TRUE)
        names_near <- as.character(apsoil[[name_field]][ord])
        ids_near <- as.character(apsoil[[id_field]][ord])
        unique_idx <- which(!duplicated(names_near) & !is.na(names_near))
        take <- head(unique_idx, n)
        top_names <- names_near[take]
        top_ids <- ids_near[take]
        d_top <- d[ord][take]
      } else {
        d <- as.numeric(st_distance(site_geom, st_geometry(apsoil[ids, , drop = FALSE])))
        ord_local <- order(d, na.last = TRUE)
        names_local <- as.character(apsoil[[name_field]][ids][ord_local])
        ids_local <- as.character(apsoil[[id_field]][ids][ord_local])
        unique_names <- unique(names_local[!is.na(names_local)])
        if (length(unique_names) == 0) return(list(names = NA_character_, ids = NA_character_, prop = NA_character_))
        top_names <- head(unique_names, n)
        # pick first occurrence index for each unique name to get corresponding id and distance
        first_idx <- sapply(top_names, function(tn) which(names_local == tn)[1])
        top_ids <- ids_local[first_idx]
        d_top <- d[ord_local][first_idx]
      }
      dist_strings <- sprintf("%s (%.1f km)", top_names, d_top/1000)
      list(names = paste(top_names, collapse = ", "), ids = paste(top_ids, collapse = ", "), prop = paste(dist_strings, collapse = "; "))
    })
  }

  apsoil_summary <- extract_top_apsoil_info(sites_m, site_buffers_m, apsoil_m, apsoil_field, id_field = "apsoil_number", n = 3)
  sites$top3_apsoil <- vapply(apsoil_summary, `[[`, "names", FUN.VALUE = character(1), USE.NAMES = FALSE)
  sites$top3_apsoil_numbers <- vapply(apsoil_summary, `[[`, "ids", FUN.VALUE = character(1), USE.NAMES = FALSE)
  sites$top3_apsoil_prop <- vapply(apsoil_summary, `[[`, "prop", FUN.VALUE = character(1), USE.NAMES = FALSE)
}

# Link APsoil profile data by nearest feature ------------------------------
cat("Linking APsoil profiles to each site...\n")
if (!is.null(apsoil) && inherits(apsoil, "sf") && nrow(apsoil) > 0) {
  sites <- st_join(sites, apsoil, join = st_nearest_feature, left = TRUE)
} else {
  warning("APsoil layer appears unavailable or empty; skipping APsoil join.")
}

# Attach agro-climatic region and state attributes ------------------------
cat("Attaching agro-climatic region and state attributes...\n")
# Replace 'region_name' and 'state_name' below with the actual field names
if ("region_name" %in% names(agro)) {
  agro <- agro %>% select(agro_region = region_name)
}
if ("state_name" %in% names(state)) {
  state <- state %>% select(state_name)
}

sites <- st_join(sites, agro, left = TRUE)
sites <- st_join(sites, state, left = TRUE)

# Read weather station CSV and attach nearest station info ---------------
cat("Reading weather station CSV and attaching nearest station info...\n")
weather <- read_weather_csv(weather_path)
if (nrow(weather) > 0) {
  weather <- weather %>% select(StationId, Name)
  sites <- st_join(sites, weather, join = st_nearest_feature, left = TRUE)
} else {
  warning("Weather CSV appears empty; skipping weather station join.")
}

# Save output --------------------------------------------------------------
cat("Saving output...\n")
# st_write(sites, output_sites_shp, delete_layer = TRUE, quiet = TRUE)

sites_df <- st_drop_geometry(sites)
write.csv(sites_df, output_sites_csv, row.names = FALSE)

cat("Workflow complete.\n")
cat(sprintf("Output file: %s\n", output_sites_csv))
