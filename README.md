# Chronicle-Preprocessed-Cleaning

## Purpose:
This script cleans smartphone app usage data preprocessed by the Chronicle/Methodic. The cleaning includes removing incomplete days, adjusting for long-running or erroneous app usages, and identifying unlikely data gaps.

GetMethodic/Chronicle: https://github.com/methodic-labs/chronicle-processing

## To run
- `Cleaning script.R`
- User specifies input/output folders and timezone (tz)

## Steps for Cleaning Process:

### 1. Initial Setup

- Loads necessary libraries.
- User specifies input/output folders and timezone (tz).

### 2. Load and Combine Data

- Reads and combines multiple CSV files into a single dataset.

### 3. Filtering and Parsing Dates

- Filters data to the user-specified timezone.
- Converts datetime strings to proper datetime formats for easier processing.

### 4. Collapsing Split App Sessions

- Chronicle Preprocessing splits long-running app sessions into 1-hour segments.
- This script merges these segments back into single app sessions based on identical/contiguous start/end times.

### 5. Removing Long-running and Problematic Apps

- Apps running >6 hours were deemed unfeasible and assumed erroneous, these app usages were dropped. 
- Some apps that were deemed unlikely to be real usage if running long (e.g. clock, screensaver) are capped at 10 mins.
- 
### 6. Gap Checking and Removal

- Checks for improbable data gaps (>12 hours without any smartphone activity, which is unlikely).
- Days on which the gap starts and ends  are considered partial days and removed.

### 7. Daylight Savings Time (DST)

- Removes dates affected by DST shifts due to complexity in accurately adjusting timestamps.

### 8. Final Cleaning

- Removes the first and last day per participant (typically incomplete).
- Adjusts anomalies, especially sessions ending exactly at midnight, which cause issues for day assignment.

