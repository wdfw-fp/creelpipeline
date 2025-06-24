# workflow

# --- Load packages and functions ---
suppressPackageStartupMessages({
  library(cli)
  library(here)
  library(yaml)
  library(tidyverse)
  library(CreelEstimateR)
  library(creelutils)
  library(timeDate)
})

source(file.path("R", "download_data.R"))
source(file.path("R", "manual_data_edits.R"))
source(file.path("R", "pre_process.R"))
source(file.path("R", "run_estimates_pe.R"))
source(file.path("R", "run_estimates_bss.R"))
source(file.path("R", "post_process.R"))
source(file.path("R", "export_estimates.R"))

# --- Load config and inputs ---
config_path <- Sys.getenv("CONFIG_FILE", unset = here("config.yml"))
params <- yaml::read_yaml(config_path)$params

# --- Run pipeline steps ---

# Step 1: Download the input creel data from data.wa.gov
cli_alert_info("Step 1: Downloading input data...")

tryCatch({
  raw_data <- download_data(params)
}, error = function(e) {
  cli_alert_danger("Download failed: {e$message}")
  quit(save = "no", status = 1)
})

# Step 2: Apply manual edits (if any)
cli_alert_info("Step 2: Applying manual edits (if any)...")

tryCatch({
  validated_data <- manual_data_edits(params, raw_data)
}, error = function(e) {
 cli_alert_danger("Manual processing of data failed: {e$message}")
  quit(save = "no", status = 1)
})

# Step 3: Perform pre-processing on input data
cli_alert_info("Step 3: Pre-processing input data... ")

tryCatch({
  pre_processed_data <- pre_process(params, validated_data)
}, error = function(e) {
  cli_alert_danger("Pre-processing failed: {e$message}")
  quit(save = "no", status = 1)
})

# Step 4a: Run Point Estimate (PE) model 
if (params$model_used == "PE only" | params$model_used == "Both Models") {
  cli_alert_info("Step 4a: Running Point Estimate (PE) model...")
  
  tryCatch({
    estimates_pe <- run_estimates_pe(params, validated_data, pre_processed_data)
  }, error = function(e) {
    cli_alert_danger("Point Estimate (PE) model failed: {e$message}")
  })
}

# # Step 4b: Run Bayesian state-space model 
# if (params$model_used == "BSS only" | params$model_used == "Both Models") {
#   cli_alert_info("Step 4b: Running Bayesian state-space (BSS) model...")
#   
#   tryCatch({
#     estimates_bss <- run_estimates_bss(params, validated_data, pre_processed_data)
#   }, error = function(e) {
#     cli_alert_danger("Bayesian state-space (BSS) model failed: {e$message}")
#   })
# }
# 
# # Step 5: Post-process model estimates by transforming into a standardized format
# if (params$export == "database") {
#   cli_alert_info("Step 5: Post-processing model estimates into standardized format...")
#   
#   tryCatch({
#     creel_estimates <- post_process(params, estimates_pe, estimates_bss)
#   }, error = function(e) {
#     cli_alert_danger("Post-processing failed: {e$message}")
#   })
# }
# 
# # Step 6: Export estimates to creel database
# if (params$export == "database") {
#   cli_alert_info("Step 6: Exporting estimates to creel database...")
#   
#   tryCatch({
#     export_estimates(params, creel_estimates)
#   }, error = function(e) {
#     cli_alert_danger("Exporting estimates failed: {e$message}")
#   })
# }