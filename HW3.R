# =============================================================================
# Homework 3 - Reproducing plots and exploring real spatial data
# Author: Nasser Alshammari
#
# How to run: open this folder as the RStudio working directory (or setwd() to
# it) and source the whole file. It creates:
#   data/raw/        raw downloads (never modified after download)
#   data/clean/      cleaned sites and boundaries (GeoPackage + CSV)
#   output/          all figures (.png) and tables (.csv)
#   data_sources.csv, site_match_audit.csv  (in the project folder)
# =============================================================================

# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

vehicles <- ggplot2::mpg           # Full dataset
gems <- ggplot2::diamonds          # Full dataset

dir.create("output", showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

save_plot <- function(p, file, w = 9, h = 5) {
  print(p)
  ggsave(file.path("output", file), p, width = w, height = h, dpi = 150)
}


# =============================================================================
# PROBLEM 1 - Reproduce and explain plots (25 points)
# =============================================================================

# ---- Part A (8 points) ------------------------------------------------------
# Points coloured by drv (semi-transparent), one panel per drive type,
# a straight-line (lm) fit without a confidence band in every panel.
p_A <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = drv), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  facet_wrap(~ drv) +
  labs(x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)",
       colour = "drv") +
  theme_bw()
save_plot(p_A, "P1A_displ_hwy_by_drv.png")

# Part A explanation (2 points):
# colour = drv is set only inside geom_point(), so it does NOT group the
# geom_smooth() layer. The three lines come from facet_wrap(~ drv): faceting
# splits the data into three subsets *before* any layer is computed, so
# geom_smooth() runs its own lm() separately on the 4, f and r rows in each
# panel. The lines are blue (the default) rather than coloured by drv, which
# confirms the grouping comes from the panels, not from the colour mapping.


# ---- Part B (8 points) ------------------------------------------------------
# Left: stacked counts of cars by drive type, filled by number of cylinders.
p_B_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count", fill = "factor(cyl)") +
  theme_bw()
save_plot(p_B_count, "P1B_count.png", w = 6, h = 4)

# Right: same bars, but position = "fill" rescales each bar to total 1,
# so y is the proportion of each cylinder class WITHIN that drive type.
p_B_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(x = "drv", y = "Proportion within drive type", fill = "factor(cyl)") +
  theme_bw()
save_plot(p_B_prop, "P1B_proportion.png", w = 6, h = 4)

# Sample size behind each bar (evidence for the explanation below)
drv_n <- vehicles %>% count(drv, name = "n_cars")
print(drv_n)

# Part B explanation (3 points):
# position = "fill" divides every bar by its own total, so each bar reaches
# 1.00 by construction no matter how many cars it contains. The heights show
# composition (shares of 4/5/6/8 cylinders), not size: the table above shows
# 103 four-wheel-drive, 106 front-wheel-drive but only 25 rear-wheel-drive
# cars, yet all three bars are equally tall. The r bar's proportions therefore
# rest on far fewer cars and are less stable than those of 4 and f.


# ---- Part C (9 points) ------------------------------------------------------
# "Smaller table": keep diamonds of at most 3 carats. Above 3 ct there are only
# a handful of stones spread thinly up to 5.01 ct, which would stretch the
# x-axis and hide the detail we want to compare. Both histograms use this
# SAME table.
carat_max <- 3
gems_small <- gems %>% filter(carat <= carat_max)

carat_records <- tibble(
  table    = c("original gems", "retained (carat <= 3)", "excluded (carat > 3)"),
  n_records = c(nrow(gems), nrow(gems_small), nrow(gems) - nrow(gems_small))
)
print(carat_records)
write_csv(carat_records, "output/P1C_records_retained_excluded.csv")

x_scale <- scale_x_continuous(limits = c(0, carat_max),
                              breaks = seq(0, carat_max, by = 0.5))

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  x_scale +
  labs(title = "Diamond carat - bin width 0.01",
       subtitle = paste0("Diamonds with carat <= 3 (n = ",
                         format(nrow(gems_small), big.mark = ","), ")"),
       x = "Carat", y = "Count") +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  x_scale +
  labs(title = "Diamond carat - bin width 0.5",
       subtitle = paste0("Diamonds with carat <= 3 (n = ",
                         format(nrow(gems_small), big.mark = ","), ")"),
       x = "Carat", y = "Count") +
  theme_bw()

save_plot(p_fine,   "P1C_p_fine.png")
save_plot(p_coarse, "P1C_p_coarse.png")

# Exact counts at 0.99 and 1.00 carat in the ORIGINAL gems table.
# near() avoids floating-point equality problems.
carat_099_100 <- tibble(
  carat = c(0.99, 1.00),
  n_diamonds = c(sum(near(gems$carat, 0.99)), sum(near(gems$carat, 1.00)))
)
print(carat_099_100)
write_csv(carat_099_100, "output/P1C_carat_099_vs_100.csv")

# Part C explanation (at most four sentences):
# The 0.01-wide bins reveal sharp spikes at "round" sizes such as 0.3, 0.5,
# 0.7, 1.0, 1.5 and 2.0 carats, each followed by a steady decline until the
# next round size. Just below each threshold the bins are nearly empty: only
# 23 stones are recorded at 0.99 ct versus 1,558 at exactly 1.00 ct. This
# suggests stones are cut (or weights rounded) to reach marketable milestones.
# With 0.5-wide bins these spikes are averaged away into a smooth right-skewed
# shape.


# =============================================================================
# DATA SOURCES (used by Problems 2 and 3)
# =============================================================================
# Facility type : Chicago Public Library locations
# Area units    : Chicago Community Areas (77 polygons)
# Both are published by the City of Chicago Data Portal. The script downloads
# the official exports; if the portal is unreachable it falls back to public
# GitHub copies of the same exports and records which URL was actually used.

lib_file  <- "data/raw/libraries_raw.csv"
area_file <- "data/raw/community_areas_raw.geojson"

lib_urls <- c(
  official = "https://data.cityofchicago.org/api/views/x8fc-8rcq/rows.csv?accessType=DOWNLOAD",
  mirror   = "https://raw.githubusercontent.com/JESUSC1/Chicago-Public-Libraries-Data-Analysis/main/Raw_Data/Library_Locations/Libraries_Locations_Contact_Information_and_Usual_Hours_of_Operation.csv"
)
area_urls <- c(
  official = "https://data.cityofchicago.org/api/geospatial/igwz-8jzy?method=export&format=GeoJSON",
  mirror   = "https://raw.githubusercontent.com/RandomFractals/ChicagoCrimes/master/data/chicago-community-areas.geojson"
)

# Download once; never overwrite a raw file that already exists.
download_raw <- function(urls, dest) {
  if (file.exists(dest)) return(invisible(NULL))   # keep raw data unchanged
  for (u in urls) {
    ok <- tryCatch({ download.file(u, dest, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 0) {
      writeLines(c(u, format(Sys.Date())), paste0(dest, ".source.txt"))
      return(u)
    }
  }
  stop("Could not download ", dest)
}
source_info <- function(dest, urls) {
  f <- paste0(dest, ".source.txt")
  if (file.exists(f)) { s <- readLines(f); list(url = s[1], date = s[2]) }
  else list(url = unname(urls["official"]),
            date = format(as.Date(file.info(dest)$mtime)))
}

download_raw(lib_urls, lib_file)
download_raw(area_urls, area_file)
lib_src  <- source_info(lib_file, lib_urls)
area_src <- source_info(area_file, area_urls)

data_sources <- tibble(
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  role = c("facility points (Problems 2-3)", "area boundaries (Problem 3)"),
  publisher = "City of Chicago Data Portal",
  official_page = c("https://data.cityofchicago.org/d/x8fc-8rcq",
                    "https://data.cityofchicago.org/d/igwz-8jzy"),
  url_used = c(lib_src$url, area_src$url),
  download_date = c(lib_src$date, area_src$date),
  raw_file = c(lib_file, area_file),
  format = c("CSV (LOCATION field holds latitude, longitude)", "GeoJSON polygons, EPSG:4326"),
  license = "City of Chicago Data Portal Terms of Use (public data)",
  notes = c(paste("Portal metadata: issued 2024-09-30, last modified 2024-10-04.",
                  "If url_used is a GitHub mirror, it is an earlier export of the same dataset (2023 snapshot).",
                  "Locations, not current opening status: temporary closures are not recorded."),
            paste("77 official community areas. If url_used is a GitHub mirror, it is an",
                  "earlier export of the same boundaries (former dataset cauq-8yn6)."))
)
write_csv(data_sources, "data_sources.csv")
print(data_sources %>% select(dataset, url_used, download_date))

data_caption_date <- paste0("downloaded ", lib_src$date)


# =============================================================================
# PROBLEM 2 - Where are the libraries, and how much area lies within
#             500 m / 1,000 m of at least one library? (25 points)
# =============================================================================

# ---- Data: read raw (unchanged) and clean a copy ---------------------------
lib_raw <- read_csv(lib_file, show_col_types = FALSE)
names(lib_raw) <- tolower(trimws(names(lib_raw)))   # e.g. "NAME " -> "name"
n_raw <- nrow(lib_raw)

# LOCATION looks like "(41.70283443594318, -87.61428978448026)"
loc_parts <- lib_raw$location %>%
  gsub("[()]", "", .) %>%
  strsplit(",")

lib <- lib_raw %>%
  mutate(
    name = trimws(name),
    lat = suppressWarnings(as.numeric(sapply(loc_parts, `[`, 1))),
    lon = suppressWarnings(as.numeric(sapply(loc_parts, `[`, 2)))
  )

# Coordinate checks
coord_check <- lib %>%
  mutate(problem = case_when(
    is.na(location) | location == "" ~ "missing LOCATION",
    is.na(lat) | is.na(lon)          ~ "unparseable coordinates",
    lat == 0 | lon == 0              ~ "zero coordinate",
    lat < 41.6 | lat > 42.1 |
      lon < -88.0 | lon > -87.5      ~ "outside Chicago bounding box",
    TRUE                             ~ "ok"
  ))
print(count(coord_check, problem))

# Duplicate checks.
#  * exact duplicates (every raw column identical) are true double entries;
#  * duplicate names or shared coordinates are only FLAGGED - the assignment
#    says not to remove facilities just because they share coordinates.
dup_exact  <- duplicated(lib_raw)
dup_name   <- duplicated(lib$name) | duplicated(lib$name, fromLast = TRUE)
dup_coords <- duplicated(lib[, c("lat", "lon")]) |
              duplicated(lib[, c("lat", "lon")], fromLast = TRUE)

cleaning_log <- coord_check %>%
  mutate(row_in_raw = row_number(),
         exact_duplicate = dup_exact,
         shares_name = dup_name,
         shares_coordinates = dup_coords & problem == "ok",
         removed = problem != "ok" | exact_duplicate,
         reason = case_when(problem != "ok" ~ problem,
                            exact_duplicate ~ "exact duplicate row",
                            TRUE ~ NA_character_)) %>%
  select(row_in_raw, name, lat, lon, problem, exact_duplicate,
         shares_name, shares_coordinates, removed, reason)
write_csv(cleaning_log, "output/P2_cleaning_log.csv")

sites_clean <- lib %>%
  filter(!cleaning_log$removed) %>%
  arrange(name) %>%
  mutate(site_id = sprintf("LIB%03d", row_number())) %>%
  select(site_id, name, address, zip, lat, lon)

cleaning_summary <- tibble(
  step = c("raw records", "missing/unparseable/out-of-range coordinates",
           "exact duplicate rows removed", "records sharing coordinates (kept, flagged)",
           "records sharing a name (kept, flagged)", "clean records"),
  n = c(n_raw, sum(cleaning_log$problem != "ok"), sum(dup_exact),
        sum(cleaning_log$shares_coordinates), sum(dup_name), nrow(sites_clean))
)
print(cleaning_summary)
write_csv(cleaning_summary, "output/P2_cleaning_summary.csv")
stopifnot(!anyDuplicated(sites_clean$site_id), nrow(sites_clean) >= 20)

# Projected CRS: NAD83 / UTM zone 16N (EPSG:26916). Units = metres.
# (Chicago lies in UTM zone 16; Illinois East State Plane EPSG:3435 uses feet.)
crs_m <- 26916
sites_m <- sites_clean %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_m)
cat("sites_m CRS:", st_crs(sites_m)$Name, "| units:",
    st_crs(sites_m)$units_gdal, "\n")

write_csv(st_drop_geometry(sites_m), "data/clean/sites_clean.csv")
st_write(sites_m, "data/clean/sites_m.gpkg", delete_dsn = TRUE, quiet = TRUE)

# City outline for context (the community-area boundaries, dissolved)
areas_ll <- st_read(area_file, quiet = TRUE)
city_m <- areas_ll %>% st_transform(crs_m) %>% st_make_valid() %>%
  st_union() %>% st_sf(geometry = .)

src_caption <- paste0("Source: City of Chicago Data Portal - Libraries (x8fc-8rcq) and\n",
                      "Community Areas (igwz-8jzy); ", data_caption_date,
                      ". CRS: NAD83 / UTM 16N (m).")

# ---- Maps -------------------------------------------------------------------
p_sites <- ggplot() +
  geom_sf(data = city_m, aes(fill = "City of Chicago"), colour = "grey40") +
  geom_sf(data = sites_m, aes(colour = "Public library"), size = 1.8) +
  scale_fill_manual(values = c("City of Chicago" = "grey95"), name = NULL) +
  scale_colour_manual(values = c("Public library" = "#1f4e9c"), name = NULL) +
  labs(title = paste0("Chicago Public Library locations (n = ", nrow(sites_m), ")"),
       caption = src_caption) +
  theme_bw()
save_plot(p_sites, "P2_p_sites.png", w = 8, h = 8)

# ---- Buffers: same sites, union so overlaps count once, NOT clipped --------
make_union_buffer <- function(pts, r) {
  st_buffer(pts, dist = r) %>% st_union() %>% st_sf(radius_m = r, geometry = .)
}
buffer_500  <- make_union_buffer(sites_m, 500)
buffer_1000 <- make_union_buffer(sites_m, 1000)
buffer_zones <- rbind(buffer_500, buffer_1000)
st_write(buffer_zones, "data/clean/buffer_zones.gpkg", delete_dsn = TRUE, quiet = TRUE)

buffer_summary <- buffer_zones %>%
  mutate(union_area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  as_tibble()

# Extra (for interpretation only): share of the city's land inside each zone.
city_km2 <- as.numeric(st_area(city_m)) / 1e6
buffer_summary <- buffer_summary %>%
  mutate(area_inside_city_km2 =
           sapply(list(buffer_500, buffer_1000),
                  function(b) as.numeric(st_area(st_intersection(st_geometry(b),
                                                                   st_geometry(city_m)))) / 1e6),
         pct_of_city = 100 * area_inside_city_km2 / city_km2)
print(buffer_summary)
write_csv(buffer_summary, "output/P2_buffer_summary.csv")

# Required check: the 1,000 m zone must be at least as large as the 500 m zone
stopifnot(buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1])

p_buffers <- ggplot() +
  geom_sf(data = city_m, aes(fill = "City of Chicago"), colour = "grey40") +
  geom_sf(data = buffer_500, aes(fill = "Within 500 m of a library"),
          colour = NA, alpha = 0.9) +
  geom_sf(data = sites_m, aes(colour = "Public library"), size = 0.5) +
  scale_fill_manual(values = c("City of Chicago" = "grey95",
                               "Within 500 m of a library" = "#f4a261"),
                    name = NULL) +
  scale_colour_manual(values = c("Public library" = "#1f4e9c"), name = NULL) +
  labs(title = "Area within 500 m of at least one Chicago public library",
       subtitle = sprintf("Combined (dissolved) 500 m buffer = %.1f km2 (%.1f%% of the city)",
                          buffer_summary$union_area_km2[1], buffer_summary$pct_of_city[1]),
       caption = src_caption) +
  theme_bw()
save_plot(p_buffers, "P2_p_buffers.png", w = 8, h = 8)

# Interpretation (Problem 2):
# Data: 81 raw records, 81 kept. No missing/unparseable or out-of-range
# coordinates, no exact duplicate rows, and no shared coordinates or names,
# so nothing was removed. sites_m is in NAD83 / UTM zone 16N (EPSG:26916),
# units = metres.
# Where: the 81 branches are spread across the city on a fairly regular grid,
# roughly one per neighbourhood; the visible gaps are the O'Hare airport
# appendage in the far northwest and the industrial southeast lakefront.
# How much area: the combined 500 m zone covers 63.5 km2 and the combined
# 1,000 m zone 246 km2 (check passed: 1,000 m >= 500 m). Inside the city
# that is about 11% and 40% of the land area respectively; the rest of the
# buffer area (0.3 and 8 km2) spills outside the boundary - over Lake
# Michigan and into neighbouring suburbs - and is included as required.
# 81 separate 500 m circles would total 81 * pi * 0.5^2 = 63.6 km2, almost
# exactly the union, so 500 m zones barely overlap; at 1,000 m the maximum
# would be 254 km2 versus 246 km2, so neighbouring branches begin to overlap.
# Doubling the radius roughly quadruples the covered area.


# =============================================================================
# PROBLEM 3 - Which community areas have the most libraries, and does the
#             ranking change once we account for area size? (25 points)
# =============================================================================

# ---- Boundaries: checks and cleaning ----------------------------------------
areas_raw <- st_read(area_file, quiet = TRUE)
names(areas_raw) <- tolower(names(areas_raw))

districts <- areas_raw %>%
  transmute(district_id = as.integer(area_numbe),
            district_name = tools::toTitleCase(tolower(trimws(community)))) %>%
  st_transform(crs_m)

boundary_checks <- tibble(
  check = c("polygons", "missing IDs", "duplicate IDs", "missing names",
            "empty geometries", "invalid geometries (before repair)"),
  n = c(nrow(districts), sum(is.na(districts$district_id)),
        sum(duplicated(districts$district_id)), sum(is.na(districts$district_name)),
        sum(st_is_empty(districts)), sum(!st_is_valid(districts)))
)
districts <- st_make_valid(districts)
boundary_checks <- bind_rows(boundary_checks,
  tibble(check = "invalid geometries (after st_make_valid)",
         n = sum(!st_is_valid(districts))))
print(boundary_checks)
write_csv(boundary_checks, "output/P3_boundary_checks.csv")
stopifnot(!anyDuplicated(districts$district_id), all(st_is_valid(districts)),
          st_crs(districts) == st_crs(sites_m), nrow(districts) >= 5)
st_write(districts, "data/clean/districts_m.gpkg", delete_dsn = TRUE, quiet = TRUE)

# Optional map: sites over the boundaries (all areas kept, even with no sites)
p_overlay <- ggplot() +
  geom_sf(data = districts, fill = "grey95", colour = "grey50", linewidth = 0.3) +
  geom_sf(data = sites_m, colour = "#1f4e9c", size = 1.5) +
  labs(title = "Libraries over Chicago's 77 community areas", caption = src_caption) +
  theme_bw()
save_plot(p_overlay, "P3_sites_over_boundaries.png", w = 7, h = 8)

# ---- Matching audit ----------------------------------------------------------
# st_intersects (not st_within) so a point lying exactly on a shared border
# shows up as matching 2 areas instead of silently disappearing.
match_list <- st_intersects(sites_m, districts)
site_match_audit <- tibble(
  site_id = sites_m$site_id,
  name = sites_m$name,
  n_area_matches = lengths(match_list),
  matched_district_id = sapply(match_list, function(i)
    paste(districts$district_id[i], collapse = ";"))
)
write_csv(site_match_audit, "site_match_audit.csv")

match_table <- site_match_audit %>%
  mutate(match_type = case_when(n_area_matches == 0 ~ "0 areas (unassigned)",
                                n_area_matches == 1 ~ "exactly 1 area",
                                TRUE ~ "2+ areas (unassigned)")) %>%
  count(match_type)
print(match_table)

not_exactly_one <- site_match_audit %>% filter(n_area_matches != 1)
cat("Sites without exactly one match:\n"); print(not_exactly_one)
n_unassigned <- nrow(not_exactly_one)

# ---- Summary table -------------------------------------------------------------
assigned <- site_match_audit %>%
  filter(n_area_matches == 1) %>%
  mutate(district_id = as.integer(matched_district_id))

district_counts <- districts %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  left_join(count(assigned, district_id, name = "n_sites"), by = "district_id") %>%
  mutate(n_sites = replace_na(n_sites, 0L),          # keep zero-count areas
         sites_per_km2 = n_sites / area_km2) %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2) %>%
  arrange(desc(n_sites), desc(sites_per_km2)) %>%
  as_tibble()

# Required checks
stopifnot(all(district_counts$area_km2 > 0),
          sum(district_counts$n_sites) == nrow(assigned),
          nrow(district_counts) == nrow(districts))

print(district_counts, n = Inf)
write_csv(district_counts, "output/P3_district_counts.csv")

cat("\nTop 10 by count:\n")
print(head(district_counts, 10))
cat("\nTop 10 by density:\n")
print(district_counts %>% arrange(desc(sites_per_km2)) %>% head(10))
cat("Areas with zero libraries:", sum(district_counts$n_sites == 0), "\n")

# ---- Comparison maps -----------------------------------------------------------
# Same boundaries, CRS and extent; separate legends with units.
# Zero-count areas are a real value (0), drawn white with a red outline and
# their own legend key; a missing value would be grey (na.value) - there are none.
map_data <- districts %>% left_join(district_counts %>% select(-district_name),
                                    by = "district_id")
zero_areas <- map_data %>% filter(n_sites == 0)
bb <- st_bbox(districts)
p3_caption <- paste0(src_caption, "\n", nrow(assigned), " of ", nrow(sites_m),
                     " libraries assigned; ", n_unassigned,
                     " unassigned (matched 0 or 2+ areas).\n",
                     "White with red outline = 0 libraries; grey = missing value (none present).")

base_map <- function(fill_var, fill_lab, title) {
  ggplot(map_data) +
    geom_sf(aes(fill = .data[[fill_var]]), colour = "grey40", linewidth = 0.2) +
    geom_sf(data = zero_areas, aes(colour = "0 libraries"), fill = "white",
            linewidth = 0.5) +
    scale_colour_manual(values = c("0 libraries" = "#d62828"), name = NULL) +
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
             expand = TRUE) +
    labs(title = title, fill = fill_lab, caption = p3_caption) +
    theme_bw()
}

p_count <- base_map("n_sites", "Libraries\n(count)",
                    "Number of public libraries per community area") +
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = "grey60",
                       limits = c(0, NA), breaks = 0:max(district_counts$n_sites))
p_density <- base_map("sites_per_km2", "Libraries\nper km2",
                      "Public libraries per square kilometre, by community area") +
  scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "grey60",
                       limits = c(0, NA))

save_plot(p_count,   "P3_p_count.png",   w = 7, h = 8)
save_plot(p_density, "P3_p_density.png", w = 7, h = 8)

# Interpretation (Problem 3):
# Boundaries: 77 polygons, no missing/duplicate IDs, no empty or invalid
# shapes (nothing repaired or removed). All 81 libraries matched exactly one
# community area (0 matched zero areas, 0 matched 2+), so 0 are unassigned
# and the counts total 81.
# Most libraries: five areas tie with 3 branches each - Humboldt Park,
# Portage Park, New City, Near West Side and Austin. 17 of the 77 areas,
# including O'Hare (the largest, 34.5 km2) and Hyde Park, have none.
# Accounting for size changes the ranking: Douglas (2 branches in 4.3 km2,
# 0.47 per km2) becomes the densest, followed by small areas with a single
# branch (Montclare, Armour Square, Kenwood). Austin falls from the top group
# to 0.16 per km2 because it is the second-largest area with libraries.
# Because counts are tiny (0-3), density is driven largely by polygon size,
# and residents near a border may use a library in the next area, so neither
# map alone measures access; the buffer analysis in Problem 2 is a better
# guide to coverage.

sessionInfo()

