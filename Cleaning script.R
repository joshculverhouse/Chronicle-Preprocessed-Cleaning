# ============================================================================
# Script: Cleaning Chronicle preprocessed data.R
# Purpose: Clean Chronicle-preprocessed smartphone app usage data for research.
# Steps:
#  1. Setup and dependencies
#  2. Load and combine data
#  3. Initial filtering and datetime parsing
#  4. Collapse split app sessions
#  5. Identify and remove long-running/problematic apps
#  6. Identify improbable gaps and remove surrounding days
#  7. Final data cleaning including daylight savings adjustments
# Github: https://github.com/joshculverhouse/Chronicle-Preprocessed-Cleaning
# ============================================================================

# 1. Setup and Dependencies --------------------------------------------------

# Load necessary libraries
library(pacman)
p_load(tidyverse, hms, lubridate, data.table)

# USER INPUTS: Only modify these lines ---------------------------------------
input_folder  <- "filepath to Chronicle files"
output_folder <- "filepath to outout folder for .csv"
tz            <- "America/New_York"

# 2. Load and Combine Data ---------------------------------------------------

# List all CSV files from input folder
files_list <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files into one data frame
files_df <- bind_rows(lapply(files_list, read.csv))

# 3. Initial Filtering and Datetime Parsing ----------------------------------

df <- files_df %>%
  
  arrange(participant_id, app_datetime_start) %>% 
  
  distinct() %>% 
  
  filter(app_timezone == tz) %>% # Keep only data matching user-specified timezone
  
  mutate(
    # Remove timezone offset from datetime strings
    parsed_start = ymd_hms(sub("[-+][0-9]{2}:[0-9]{2}$", "", app_datetime_start)),
    parsed_end   = ymd_hms(sub("[-+][0-9]{2}:[0-9]{2}$", "", app_datetime_end)),
    
    date = as.Date(parsed_start),  # Extract date from start datetime
    
    # Extract timestamps
    start_timestamp = as_hms(format(parsed_start, "%H:%M:%S")),
    stop_timestamp  = as_hms(format(parsed_end, "%H:%M:%S")),
    
    # Convert duration to minutes
    duration_minutes = as.numeric(app_duration_seconds) / 60,
    
    # Preserve full datetimes for gap calculations
    start_datetime = parsed_start,
    end_datetime   = parsed_end
  ) %>%
  
  # Select columns needed for further processing
  select(participant_id, app_record_type, app_title, app_full_name,
         start_datetime, end_datetime, start_timestamp, stop_timestamp,
         app_duration_seconds, date)

# 4. Collapse Split App Sessions ---------------------------------------------

# Chronicle splits apps longer than 1-hour into multiple rows.
# This merges contiguous segments back into single app sessions.

DT <- as.data.table(df)

# Ensure duration is numeric
DT[, app_duration_seconds := as.numeric(app_duration_seconds)]

# Flag the start of new sessions (end of previous != start of current)
DT[, new_session := 
     is.na(shift(end_datetime, type="lag")) | 
     (shift(end_datetime, type="lag") != start_datetime),
   by = .(participant_id, app_full_name, app_record_type)]

# Assign cumulative session IDs based on new sessions
DT[, session_id := cumsum(new_session), 
   by = .(participant_id, app_full_name, app_record_type)]

# Merge rows within the same session
df_sessions <- DT[, .(
  app_title            = first(app_title),
  start_datetime       = first(start_datetime),
  end_datetime         = last(end_datetime),
  start_timestamp      = first(start_timestamp),
  stop_timestamp       = last(stop_timestamp),
  app_duration_seconds = sum(app_duration_seconds),
  date                 = first(date),
  n_segments           = .N
), by = .(participant_id, app_record_type, app_full_name, session_id)]

# 5. Identify and Remove Long-running or Problematic Apps --------------------

# Apps known for unreliable durations
bad_apps <- c(
  "com.openlattice.chronicle",
  "com.sec.android.app.clockpackage",
  "com.google.android.deskclock",
  "com.lge.launcher3",
  "com.motorola.launcher3",
  "bitpit.launcher",
  "com.sec.android.app.launcher",
  "com.android.dreams.basic",
  "com.google.android.googlequicksearchbox",
  "com.water.reminder.tracker"
  # any others
)

# Check instances of long-running apps (6+ hours)
long_running_apps <- df_sessions %>%
  filter(app_duration_seconds > 21600, !app_full_name %in% bad_apps) %>%
  summarise(n_long_running = n())

# Count problematic ('bad') apps running >10 mins
bad_app_drops <- df_sessions %>%
  filter(app_duration_seconds > 600, app_full_name %in% bad_apps) %>%
  summarise(n_bad_apps = n())

# Filter out excessively long or problematic app sessions
df_filtered <- df_sessions %>% 
  filter(!(app_duration_seconds > 600 & app_full_name %in% bad_apps)) %>% 
  filter(app_duration_seconds < 21600)

# 6. Identify Improbable Gaps and Remove Surrounding Days --------------------

# Identify gaps >12 hours between events per participant
gap_info <- df_filtered %>% 
  group_by(participant_id) %>%
  mutate(next_event_timestamp = lead(start_datetime)) %>%
  ungroup() %>%
  mutate(gap_hours = as.numeric(difftime(next_event_timestamp, end_datetime, units = "hours"))) %>%
  filter(gap_hours > 12)

# Days adjacent to gaps are suspect and removed
gap_days_df <- gap_info %>%
  mutate(end_day = as.Date(end_datetime), next_day = as.Date(next_event_timestamp)) %>%
  select(participant_id, end_day, next_day) %>%
  pivot_longer(cols = c(end_day, next_day), names_to = "boundary", values_to = "date") %>%
  distinct()

df_gaps <- df_filtered %>% anti_join(gap_days_df, by = c("participant_id", "date"))

# 7. Final Data Cleaning Including DST Adjustments ---------------------------

df_clean <- df_gaps %>% 
  filter(!is.na(end_datetime)) %>% 
  mutate(
    # Adjust apps ending exactly at midnight
    end_datetime = if_else(
      format(end_datetime, "%H:%M:%S") == "00:00:00",
      as.POSIXct(paste0(format(end_datetime - days(1), "%Y-%m-%d"), " 23:59:59.999"), tz = "UTC"),
      end_datetime
    ),
    start_date = as.Date(start_datetime),
    end_date   = as.Date(end_datetime)
  ) %>% 
  filter(end_date == start_date) %>% 
  group_by(participant_id) %>%
  filter(start_date != min(start_date), end_date != max(end_date)) %>%
  ungroup() %>% 
  mutate(duration_secs = round(as.numeric(difftime(end_datetime, start_datetime, units = "secs")),1))

# DST adjustments: Remove DST transition days (2020–2030)
get_dst_dates <- function(year) {
  march <- ymd(paste0(year, "-03-01"))
  spring_forward <- march + days((7 - wday(march) + 1) %% 7) + weeks(1)
  november <- ymd(paste0(year, "-11-01"))
  fall_back <- november + days((7 - wday(november) + 1) %% 7)
  tibble(dst_date = c(spring_forward, fall_back))
}

dst_changes <- map_df(2020:2030, get_dst_dates) %>% pull(dst_date)

df_clean <- df_clean %>% filter(!(start_date %in% dst_changes))

# Select final columns and sort
cleaned_df <- df_clean %>% 
  select(participant_id, app_full_name, app_title, start_datetime, end_datetime,
         duration_secs, n_segments) %>% 
  arrange(participant_id, start_datetime)

# Export cleaned data
write_csv(cleaned_df, file.path(output_folder, "cleaned_data.csv"))

# ============================================================================
