# Step 4a: Run Point Estimate (PE) model 
run_estimates_pe <- function(
    params = config$params,
    raw_data = raw_data, 
    pre_processed_data = pre_processed_data
    ) {
  dwg <- pre_processed_data$raw_data
  dwg_summ <- pre_processed_data$data_summ
  # PE estimation
  
  ## Create PE input object 
  inputs_pe <- list()
  
  # Calculate total days the fishery was open per strata (strata = period, day_type, and section_num)
  (inputs_pe$days_total <- 
      CreelEstimateR::prep_inputs_pe_days_total(
        days = dwg$days
      ))
  
  # Calculate anglers per count_type object by angler_final 
  #NOTE: these estimates are used to expand index counts of objects to number of angler; e.g., trailer counts to boat anglers)
  #NOTE: these estimate are derived using interview data and calculated as a single, season-long, weighted average
  (inputs_pe$interview_ang_per_object <- 
      CreelEstimateR::prep_inputs_pe_int_ang_per_object(
        dwg_summarized = dwg_summ, 
        study_design = params$study_design
      ))
  
  # Calculate tie-in (aka census) count expansion factor by section & angler_final
  #NOTE: these estimates are used to spatially expand/bias correct angler effort count data collected during index counts
  #NOTE: these estimates are derived using paired census and index counts and calculated as a single, season-long, weighted average
  (inputs_pe$paired_census_index_counts <- 
      CreelEstimateR::prep_inputs_pe_paired_census_index_counts(
        days = dwg$days,
        dwg_summarized = dwg_summ,
        interview_ang_per_object = inputs_pe$interview_ang_per_object,
        census_expan = dwg_summ$census_expan,
        study_design = params$study_design
      ))
  
  # Calculate census-corrected angler effort (mean angler hours) by event_date, angler_type, and section_num
  (inputs_pe$ang_hrs_daily_mean <- 
      CreelEstimateR::prep_inputs_pe_ang_hrs(
        days = dwg$days, 
        dwg_summarized = dwg_summ,
        interview_ang_per_object = inputs_pe$interview_ang_per_object,
        paired_census_index_counts = inputs_pe$paired_census_index_counts,
        study_design = params$study_design
      ))
  
  # Calculate CPUE and catch_estimates by event_date, angler_type, section_num, and est_cg (aka catch group of interest)
  (inputs_pe$daily_cpue_catch_est <-
      CreelEstimateR::prep_inputs_pe_daily_cpue_catch_est(
        days = dwg$days,
        dwg_summarized = dwg_summ,
        angler_hours_daily_mean = inputs_pe$ang_hrs_daily_mean
      ))
  
  # summary - creel surveys dates by section where estimated total effort for a particular "angler_final" grouping was >0  group but no interviews were obtained
  inputs_pe$daily_cpue_catch_est |> filter(is.na(est_cg), angler_hours_daily_mean>0)
  
  # Calculate the degree of freedom used to derived estimates of uncertainty
  #NOTE: not sure these calculation are correct (e.g., WRONG/MISSING STRATA:  dplyr::group_by(section_num, angler_final))
  inputs_pe$df <- 
    CreelEstimateR::prep_inputs_pe_df(
      angler_hours_daily_mean = inputs_pe$ang_hrs_daily_mean
    )
  
  ## Generate PE 
  estimates_pe <- list() 
  
  estimates_pe$effort <- 
    CreelEstimateR::est_pe_effort(
      params = params, # new !!! CH

      days = dwg$days,
      pe_inputs_list = inputs_pe
    )
  
  estimates_pe$catch <- 
    CreelEstimateR::est_pe_catch(
      params = params, # new !!! CH
      dwg = dwg, # new !!! CH
      days = dwg$days,
      pe_inputs_list = inputs_pe
    )
  
  return(estimates_pe)
}
