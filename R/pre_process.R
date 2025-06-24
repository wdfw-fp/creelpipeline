# Step 3: Perform pre-processing on input data
pre_process <- function(
    params = NULL,
    raw_data = NULL
  ) {
  
  dwg <- raw_data
  
  # Establish analysis_id ####
  #Establishes a unique 'analysis_id' for each session and populates an 'analysis_lut'
  CreelEstimateR::generate_analysis_lut(params)
  
  # edit fn - prep_days() ####
  
  ## construct dataframe of holidays ####
  # note: alternative approach to current flatfile dependency
  
  # calculate holidays
  years <- 2015:2040
  
  # define native american heritage day as the day after thanksgiving for each year
  NativeAmericanHeritageDay <- function(year) {
    as.Date(USThanksgivingDay(year)) + lubridate::days(1)
  }
  
  # define list of holidays
  us_holiday_funs <- list(
    "New Year's Day" = USNewYearsDay,
    "Martin Luther King Jr Day" = USMLKingsBirthday,
    "President's Day" = USPresidentsDay,
    "Memorial Day" = USMemorialDay,
    "Juneteenth" = USJuneteenthNationalIndependenceDay,
    "Independence Day" = USIndependenceDay,
    "Labor Day" = USLaborDay,
    "Verteran's Day" = USVeteransDay,
    "Thanksgiving Day" = USThanksgivingDay,
    "Native American Heritage Day" = NativeAmericanHeritageDay,
    "Christmas Day" = USChristmasDay
  )
  
  # generate list of holiday dates
  holiday_df <- map_df(years, function(year) {
    map_df(names(us_holiday_funs), function(holiday_name) {
      tibble(
        year = year,
        date = as.Date(us_holiday_funs[[holiday_name]](year)),
        holiday = holiday_name
      )
    })
  })
  
  
  # Set day length times if using manual option ####
  day_length_inputs <- list()
  
  if(params$day_length_expansion == "manual"){
    # Step #1: Select general strategy for [earliest] start and [latest] end time
    day_length_inputs$start_time<-c("manual") # enter either "sunrise" or "manual" 
    day_length_inputs$end_time<-c("sunset")    # enter either "sunset"  or "manual" 
    # Step #2A: If necessary, specify an offset (in hours) to the start and end times
    # (e.g., if legal fishing occurs 1 hr. prior to sunrise & sunset, enter 1 below for both); enter 0 if no offset needed
    day_length_inputs$start_adj<-c(1) # Specify an offset for the start time (in hours);    
    day_length_inputs$end_adj<-c(1)   # Specify an offset for the end time (in hours);  
    # Step #2B: If "manual" entered for "ui_start_time" or "ui_end_time", enter the earliest start and/or latest end time for a creel survey event
    day_length_inputs$start_manual<-c("06:00:00") # Specify manual start time (format "HH:MM:SS", e.g., 6 AM = "06:00:00")
    day_length_inputs$end_manual<-  c() # Specify manual end time (format "HH:MM:SS")  
  }
  
  # function add ####
  # issue with holidays flat file being converted to date. new approach is already a date. CH
  prep_days <- function(
    date_begin, date_end,
    weekends = c("Saturday", "Sunday"),
    holidays, #date/char vector of YYYY-MM-DD dates to categorize as "weekend" strata
    lat, long,
    period_pe,
    sections, #numeric vector of all possible sections to estimate
    closures, #tibble of fishery_name, section number and date of closures
    day_length,
    day_length_inputs,
    ...){
    
    date_begin <- as.Date(date_begin, format="%Y-%m-%d")
    date_end <- as.Date(date_end, format="%Y-%m-%d")
    # holidays <- as.Date(holidays, format="%Y-%m-%d")
    
    # create tibble with dates and time period strata 
    
    days <- tibble::tibble(
      event_date = seq.Date(date_begin, date_end, by = "day"),
      day = weekdays(event_date),
      day_type = if_else(day %in% weekends | event_date %in% holidays, "weekend", "weekday"),
      day_type_num = as.integer(c("weekend" = 1, "weekday" = 0)[day_type]),  #if_else(str_detect(day_type, "end"), 1, 0),
      #Monday to Sunday weeks, see ?strptime
      week = as.numeric(format(event_date, "%W")),
      month = as.numeric(format(event_date, "%m")),
      year = as.numeric(format(event_date, "%Y")),
      period = case_when(
        period_pe == "week" ~ week,
        period_pe == "month" ~ month,
        period_pe == "duration" ~ double(1)
      ),
      day_index = as.integer(seq_along(event_date)),
      week_index = as.integer(factor(week, levels = unique(week))),
      month_index = as.integer(factor(month, levels = unique(month)))
    )
    
    # section calculating day length, using param option day_length_expansion
    # the manual options allows the user to select fixed times or sunrise / sunset with offsets for either start or end times
    # the alternative options are "night closure", "dawn/dusk", "sunrise/sunset"
    if(day_length == "manual"){
      
      # If manual start and/or end times enter, specify "ui_start_time" and "ui_end_time" in case they were left blank
      # if( day_length_inputs$start_time == "manual"){day_length_inputs$start_time<-c("sunrise")}
      # if( day_length_inputs$end_time == "manual")  {day_length_inputs$end_time<-c("sunset")} 
      
      # Day length based on sunrise/sunset 
      
      day_length_values <- 
        tibble(
          getSunlightTimes(
            keep=c("sunrise", "sunset"), 
            date=days$event_date, 
            lat = lat, 
            lon= long,
            tz = "America/Los_Angeles"
          )
        ) |> 
        select(-lat, -lon)
      
      # Start times using sunrise + offset 
      if(day_length_inputs$start_time != "manual"){
        day_length_values <- 
          day_length_values |> 
          mutate(
            start_date_time = sunrise - (60*60*day_length_inputs$start_adj)
          )
      }else{ # or manually specified time
        day_length_values <- 
          day_length_values |> 
          mutate(
            start_date_time = as.POSIXct(paste(day_length_values$date, day_length_inputs$start_manual), format = "%Y-%m-%d %H:%M:%S") 
          )
      }
      
      # End times using sunset + offset 
      if(day_length_inputs$end_time != "manual"){
        day_length_values <- 
          day_length_values |> 
          mutate(
            end_date_time = sunset + (60*60*day_length_inputs$end_adj)
          )
      }else{ # or manually specified time
        day_length_values <- 
          day_length_values |> 
          mutate(
            end_date_time = as.POSIXct(paste(day_length_values$date, day_length_inputs$end_manual), format = "%Y-%m-%d %H:%M:%S") 
          )
      }
      
      # calculate day length from start and end times from above 
      day_length_values <- day_length_values |> 
        mutate(
          day_length = as.numeric(end_date_time - start_date_time)
        ) |> 
        select(event_date = date, day_length)
      
      
      # 
    }else{
      day_length_values <- suncalc::getSunlightTimes(
        date = days$event_date,
        tz = "America/Los_Angeles",
        lat = lat, lon = long,
        keep=c("sunrise", "sunset", "dawn", "dusk")
      ) |> 
        select(event_date = date, sunrise, sunset, dawn, dusk) |> 
        mutate(
          day_length_dawn_dusk = as.numeric((dusk) - (dawn)),
          day_length_sunrise_sunset = as.numeric((sunset) - (sunrise)),
          day_length_night_closure = as.numeric((sunset + 3600) - (sunrise - 3600)),
        ) |> 
        mutate(
          day_length = case_when(
            day_length == "dawn/dusk" ~ day_length_dawn_dusk,
            day_length == "sunrise/sunset" ~ day_length_sunrise_sunset,
            day_length == "night closure" ~ day_length_night_closure
          )
        ) |> 
        select(event_date, day_length)
    }
    
    # join day_length to days tibble
    
    days <- days |> left_join(day_length_values, by = "event_date")
    
    # expanding join to incorporate closure dates 
    
    days <- left_join(
      days,
      dplyr::rows_update(
        tidyr::expand_grid(event_date = days$event_date, section_num = sections, open = TRUE)
        ,
        closures |>
          mutate(
            event_date = as.Date(event_date, format="%Y-%m-%d"),
            section_num = as.double(section_num)
          ) |> 
          dplyr::filter(dplyr::between(event_date, date_begin, date_end)) |> 
          dplyr::select(section_num, event_date) |> 
          dplyr::mutate(open = FALSE)
        ,
        by = c("section_num", "event_date")
      ) |> 
        dplyr::arrange(section_num, event_date) |> 
        dplyr::mutate(section_num = paste0("open_section_", section_num)) |> 
        tidyr::pivot_wider(names_from = section_num, values_from = open)
      ,
      by = "event_date"
    ) |> 
      mutate(
        fishery_name = params$fishery_name
      ) |> 
      relocate(fishery_name)
    
    return(days)
  }
  
  #event_date = seq.Date(as.Date(params$est_date_start), as.Date(params$est_date_end), by = "day")
  
  # prepare days dataframe ####
  dwg$days <- prep_days(
    date_begin = params$est_date_start, 
    date_end = params$est_date_end, 
    weekends = params$days_wkend,
    # holidays = read_lines(paste(here(), "input_files/dates_holidays_2015_2030.txt", sep = "/")), # issue !!!!
    holidays = holiday_df,
    lat = mean(dwg$ll$centroid_lat), #can/should consider smarter options
    long = mean(dwg$ll$centroid_lon),#can/should consider smarter options 
    period_pe = params$period_pe,
    sections = unique(dwg$effort$section_num),
    closures = dwg$closures,
    day_length = params$day_length_expansion,
    day_length_inputs = day_length_inputs
  )
  
  # Shared data aggregation ####
  # Create blank list that will be used to attach the following three datasets
  dwg_summ <- list() #intermediate objects wrangled from creel list elements
  
  # 1.) Interview data summarization
  # 1.1 - Summarize fishing time 
  interview_fishing_time <- 
    CreelEstimateR::prep_dwg_interview_fishing_time(
      dwg_interview = dwg$interview |> filter(between(event_date, as.Date(params$est_date_start), as.Date(params$est_date_end))),
      person_count_type = params$person_count_type,
      min_fishing_time = params$min_fishing_time,
      study_design = params$study_design
    )
  
  # 1.2 - Summarize angler types (angler_final based on arguments)
  interview_plus_angler_types <- 
    CreelEstimateR::prep_dwg_interview_angler_types(
      interview_fishing_time = interview_fishing_time,
      study_design = params$study_design,
      boat_type_collapse = params$boat_type_collapse,
      fish_location_determines_type = params$fish_location_determines_type,
      angler_type_kayak_pontoon = params$angler_type_kayak_pontoon
    )
  
  # 1.2.1 - Preview angler_final designations
  interview_plus_angler_types |> select(interview_id, angler_type, boat_used, boat_type, fish_from_boat, angler_final) |> group_by(boat_used, fish_from_boat, boat_type) |> count(angler_final)
  
  # creates i.) est_cg field using species, life_stage, fin_mark, and fate, ii.) summarizes the number each angler group caught, and...
  # ...iii.) creates output based on study_design and corresponding interview fields needed for angler effort calculations  
  
  # function add ###
  # this function not in test package CreelEstimateR?? CH
  
  
  prep_dwg_interview_catch <- function(
    interview_plus_angler_types, # output from preceding functions that created calculated fishing time and defined angler_final 
    dwg_catch,                   # catch data from dwg 
    study_design,                # string passed from params denoting which study design was followed during data collection
    est_catch_groups,            # data.frame passed from params of aggregated catch groups of interest to estimate
    ...){
    
    #coerce missing values to actual strings to allow params$est_catch_groups to include NAs alongside non-NA
    #to allow 'run' specification in params, add 'run' within across()
    dwg_catch_group <- 
      dwg_catch |> 
      mutate(across(c(species, life_stage, fin_mark, fate), ~replace_na(as.character(.), "NA")))
    
    catches <- map_df(
      1:nrow(est_catch_groups),
      ~dwg_catch_group |>
        filter(
          str_detect(species, est_catch_groups$species[.x]),
          str_detect(life_stage, est_catch_groups$life_stage[.x]),
          str_detect(fin_mark, est_catch_groups$fin_mark[.x]),
          str_detect(fate, est_catch_groups$fate[.x])
        ) |>
        mutate(
          est_cg = paste0(unlist(est_catch_groups[.x,]), collapse = "_")
        ) |>
        group_by(est_cg, interview_id) |>
        summarise(fish_count = sum(fish_count, na.rm = T), .groups = "drop")
    )
    
    #replicate wrangled interviews n-many of catch_groups to estimate
    int_cat <- map_df(
      1:nrow(est_catch_groups), 
      ~interview_plus_angler_types |>   
        mutate(est_cg = paste0(unlist(est_catch_groups[.x,]), collapse = "_"))
    ) |>
      left_join(catches, by = c("est_cg", "interview_id")) |>
      mutate(fish_count = replace_na(fish_count, 0)) |>
      mutate(
        fishery_name = params$fishery_name # add back fishery_name
      ) |>
      relocate(fishery_name)
    
    if(str_detect(study_design, "tandard" )){
      interview_final<-
        int_cat |> 
        dplyr::select(
          interview_id,
          section_num, event_date, angler_final, angler_final_int,
          vehicle_count, trailer_count,
          fishing_time, person_count_final, fishing_time_total,
          trip_status, previously_interviewed,
          est_cg, fish_count
        ) |>
        dplyr::arrange(section_num, event_date, angler_final) 
      
    }else if(study_design == "Drano"){
      
      interview_final<-
        int_cat |> 
        dplyr::select(
          interview_id,
          section_num, event_date, angler_final, angler_final_int,
          fishing_time, person_count_final, fishing_time_total,
          trip_status, previously_interviewed,
          est_cg, fish_count
        ) |>
        dplyr::arrange(section_num, event_date, angler_final) 
    }
    return(interview_final)
  }
  
  # 1.3 - Summarize catch (for catch groups of interest)
  interview_plus_catch <- 
    prep_dwg_interview_catch(
      interview_plus_angler_types = interview_plus_angler_types,
      study_design = params$study_design,
      dwg_catch = dwg$catch,
      est_catch_groups = params$est_catch_groups
    )
  
  # 1.4 - Attach the final formatted interview data set to "dwg_summ"
  dwg_summ$interview <-interview_plus_catch
  
  # 2.) Effort index data summarization
  # 2.1 - Aggregate index effort counts over locations within count_seq & section; count_types are converted to angler_final based on arguments
  effort_index_summ <- 
    CreelEstimateR::prep_dwg_effort_index(
      eff = dwg$effort |> filter(between(event_date, as.Date(params$est_date_start), as.Date(params$est_date_end))),
      study_design = params$study_design,
      boat_type_collapse = params$boat_type_collapse,
      fish_location_determines_type = params$fish_location_determines_type,
      angler_type_kayak_pontoon = params$angler_type_kayak_pontoon,
      params = params #new !!! CH
    )
  
  # 2.1.1 - Summary of count_types and their corresponding "angler_final" designation
  effort_index_summ$index_angler_groups |> group_by(count_type) |> distinct(angler_final) 
  
  # 2.1.2 - QAQC - are there any census effort count groups that "failed"
  # NOTE: fails get filtered out from "effort_index_summ$index_angler_final so make sure this is correct
  effort_index_summ$index_angler_groups |> distinct(angler_final) 
  
  # 2.2 - Attach the final formatted effort index data set to "dwg_summ"
  dwg_summ$effort_index <- effort_index_summ$index_angler_final     
  
  # function add ####
  # issue with how select had quotations around field names when ran from package? CH
  
  #Aggregate census (tie in) effort counts, associating to closest-in-time index count.
  
  prep_dwg_effort_census <- function(
    eff,                                # effort data from dwg filtered using start & end dates passed from params
    study_design,                       # string passed from params denoting which study design was followed during data collection
    boat_type_collapse = NA,            # string passed from params that controls whether all (potential) boat types (e.g., motor_boat, drift_boat) are collapsed (i.e., boat_type_collapse: "Yes") into a single boat type or kept separate (boat_type_collapse: "No"). 
    fish_location_determines_type = NA, # string passed from params that controls whether the observed fishing location for a given angler group during an effort count determines their angler type. 
    angler_type_kayak_pontoon = NA,     # string passed from params that controls whether a boat designated as a kayak, pontoon, or kick during an effort count or angler group interview should be designated as a boat or bank angler.
    ...){
    
    eff_cen <- dplyr::filter(eff, tie_in_indicator == 1) #Filter for effort census (aka tie-in) data
    eff_ind <- dplyr::filter(eff, tie_in_indicator == 0) #Filter for effort index data
    
    if(nrow(eff_cen) == 0){cat("ATTENTION: No effort census data collected and/or entered into the creel database \n")}
    
    if(str_detect(study_design, "tandard" )){
      #create intermediate object census_angler_groups that i.) pairs closest index and census counts and ii.) then converts count_type objects to angler_final based on study design & user defined arguments (in YAML)
      census_angler_groups<-
        dplyr::left_join(
          #census values of interest...
          dplyr::select(eff_cen, section_num, event_date, tie_in_indicator, count_type, count_quantity)
          ,
          #...get reassigned count_seq from closest index
          #this nested join is typically expanding, as multiple index counts may match each census section & date
          #then the slice_min & distinct cut back down to a single value to reassign to above
          dplyr::left_join(
            dplyr::distinct(eff_cen, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
            dplyr::distinct(eff_ind, section_num, event_date, tie_in_indicator, effort_start_time, count_sequence),
            by = c("section_num", "event_date"),
            suffix = c("_cen", "_ind")
          ) |>
            dplyr::group_by(section_num, event_date) |>
            dplyr::slice_min(abs(effort_start_time_cen - effort_start_time_ind), n = 1) |>
            dplyr::ungroup() |>
            dplyr::distinct(section_num, event_date, count_sequence = count_sequence_ind)
          ,
          by = c("section_num", "event_date")
        ) |>
        dplyr::mutate(
          angler_final = 
            dplyr::case_when(
              count_type %in% c("Boat", "Boat Anglers") ~ "boat",
              count_type %in% c("Bank", "Bank Anglers") ~ "bank",
              
              stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(count_type, "Shore") ~ "bank",
              stringr::str_detect(fish_location_determines_type, "Y") & stringr::str_detect(count_type, "Boat - D|Boat - M|Boat - P") ~ "boat",
              
              stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "Motor|Large") ~ "boat", 
              stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "Drift|Raft")  ~ "boat",
              
              stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") ~ "boat",
              stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank") ~ "bank",
              
              stringr::str_detect(fish_location_determines_type, "N") & stringr::str_detect(count_type, "No Boat") ~ "bank",
              TRUE ~ "fail"
              
            ),
          angler_final_int = as.integer(factor(angler_final))
        ) 
      
      census_angler_final<-
        census_angler_groups |>
        filter(angler_final != "fail") |> 
        dplyr::group_by(section_num, event_date, tie_in_indicator, count_sequence, angler_final, angler_final_int) |>
        dplyr::summarize(count_census = sum(count_quantity), .groups = "drop") |>
        dplyr::arrange(section_num, event_date, count_sequence) |>
        mutate(
          fishery_name = params$fishery_name # add back fishery_name
        ) |>
        relocate(fishery_name)
      
      
      
    }else if(study_design == "Drano"){
      
      cat("NOTE: Per Drano study design, daily effort counts were assumed to be census counts of banks anglers and boat vessels. 
        Effort count data were recorded and stored in the data base as index counts but in reality they are census counts with regards to their spatial coverage.
        To fit the Drano Lake creel study design within the larger creel analysis, the function 'prep_dwg_effort_index' was duplicated within 
        the 'prep_dwg_effort_census' function to create the object 'census_angler_final' where the field count_index was duplicated and named as 'count_census'.
        Overall, the effort index data set was duplicated to generate an effort census data set." )
      
      index_angler_groups<- 
        eff |> 
        dplyr::filter( 
          tie_in_indicator == 0,
          is.na(no_count_reason),
          !is.na(count_type)
        ) |>
        dplyr::mutate(
          angler_final = 
            dplyr::case_when(
              
              stringr::str_detect(count_type, "Shore") ~ "bank",
              
              stringr::str_detect(count_type, "Motor") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
              stringr::str_detect(count_type, "Motor") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_motor",
              
              stringr::str_detect(count_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
              stringr::str_detect(count_type, "Skiff|Pram") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_skiff",
              
              stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "Y") ~ "boat",
              stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "oat") & stringr::str_detect(boat_type_collapse, "N") ~ "boat_single",
              stringr::str_detect(count_type, "ontoon|ayak") & stringr::str_detect(angler_type_kayak_pontoon, "ank")  ~ "bank", 
              TRUE ~ "fail"
            )
        ) 
      
      index_angler_final<-   
        index_angler_groups |> 
        dplyr::group_by(section_num, event_date, count_sequence, angler_final) |> 
        dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |> 
        dplyr::arrange(section_num, event_date, count_sequence) |> 
        mutate(
          fishery_name = params$fishery_name # add back fishery_name
          , angler_final_int = as.integer(factor(angler_final)) 
        ) |> 
        relocate(fishery_name)
      
      census_angler_groups<-index_angler_groups
      census_angler_final<-index_angler_final |> rename(count_census = count_index) |> relocate(count_census, .after = angler_final_int)
      
    }
    
    return(list(census_angler_groups = census_angler_groups, census_angler_final = census_angler_final)) 
    
  }
  
  # 3.) Effort census data summarization
  # 3.1 - Aggregate census (tie in) effort counts, associating to closest-in-time index count
  ##NOTE: a warning message will appear indicating "...an unexpected many-to-many relationship..." if multiple effort census counts were completed in the same section and day; this message can be ignored as the data wrangling works as intended despite this warning
  effort_census_summ <- 
    prep_dwg_effort_census(
      eff = dwg$effort |> filter(between(event_date, as.Date(params$est_date_start), as.Date(params$est_date_end))),
      study_design = params$study_design,
      boat_type_collapse = params$boat_type_collapse,
      fish_location_determines_type = params$fish_location_determines_type,
      angler_type_kayak_pontoon = params$angler_type_kayak_pontoon,
      params = params # new !!! CH
    )
  
  # 3.1.1 - QAQC - are there any census effort count groups that "failed"
  # NOTE: fails get filtered out from "effort_index_summ$index_angler_final so make sure this is correct
  effort_census_summ$census_angler_groups |> filter(angler_final == "fail") 
  
  # 3.2 - Attach the final formatted effort census data set to "dwg_summ"
  dwg_summ$effort_census <- effort_census_summ$census_angler_final
  
  # 4.) Generate summary table of "p_census" 
  (dwg_summ$census_expan <- 
      CreelEstimateR::prep_dwg_census_expan(
        eff = dwg$effort
      ))  
  

  return(list(data_summ = dwg_summ, raw_data = dwg))
}

