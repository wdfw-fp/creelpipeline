# Step 1: Download the input creel data from data.wa.gov

# download fishery dataset using "fishery name" identifier in config params
download_data <- function(params) {
  fishery_name <- params$fishery_name
  raw_data <- creelutils::fetch_dwg(fishery_name)
  return(raw_data)
}
