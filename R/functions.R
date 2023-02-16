
#' creates a uniformly-spaced discrete-time grid data.table
#' @param start_times vector of date times denoting the 'start' of a time series (e.g. the start of a patient's stay in the ICU) 
#' @param end_times vector of date times denoting the 'end' of a time series  (e.g. the end of a patient's stay in the ICU) 
#' @param IDs vector of ID associated with each discrete time series. If null, each start/end time pair is given a unique ID.
#' @param sample_interval_minute: numeric scalar, the time elapsed between one grid point to another (in minutes).
#' 
#' @return long tables of variables, time points at every `sample_interval_minute` from  start_times to end_times
#' @export
GetTimeTable <- function(start_times = NULL,
                         end_times = NULL,
                         IDs = NULL, 
                         sample_interval_minute = 30) { 
  
 
  
  
  ######
  # error checking
  #######
  check_sample_int_minute(sample_interval_minute)
  check_start_end_times(start_times,end_times)
  if(is.null(IDs)){
    IDs = as.character(1:length(start_times))
  } else {
    IDs <- as.character(IDs)
    check_if_same_length(times = start_times,
                         ids = IDs,
                         val = end_times)
  }
  
  #########
  ## determine global grid times
  #########
  
  ## extract the min date 
  min_date = lubridate::as_datetime(
    # TODO: change to ceilling 
    lubridate::floor_date(min(start_times),
                          unit = paste0(sample_interval_minute," min")
    ))
  
  ## extract the max date + add one
  max_date = lubridate::as_datetime(
    lubridate::ceiling_date(max(end_times),
                            unit = paste0(sample_interval_minute," min")
    )) + lubridate::days(1)
  
  # compute date range in minutes
  date_range_minutes = difftime(max_date,
                                min_date,
                                units = "mins") |> as.numeric()
  
  # compute total number of grid points
  num_grid_points = date_range_minutes / sample_interval_minute
  
  # create a vector of all time points from min to max time range. 
  # it seems addition on datetime operates on Seconds (unit of measurement)
  grid_datetimes = min_date + lubridate::minutes(1:num_grid_points) * sample_interval_minute
  
  # now make grid times a data.table object
  grid_datetimes_dt = data.table(grid_datetimes)
  
  time_df = data.frame('IDs' = IDs,
                       'start_times' = start_times,
                       'end_times' = end_times)
  
  # make a custom grid for each encounter
  grid_datetimes_dt = grid_datetimes_dt[time_df, 
                                        .(IDs, x.grid_datetimes), 
                                        on = .(grid_datetimes >= start_times,
                                               grid_datetimes <= end_times),
                                        allow.cartesian=T]
  
  colnames(grid_datetimes_dt) = c("IDs", "grid_datetimes")
  
  return(grid_datetimes_dt)
  
}

#' get_LOCF_values; internal function
#' 
#' @param x vector of date times for 'start' of a time series (e.g. the start of a patient's stay in the ICU) 
#' @param sample_interval_minute scalar time between one time grid point and another (in minutes)
#' @param duration_hr how many hours until a variable's valuable expires
#' @return a vector where Last Observed Carry Forward
#' 
#' @noRd
get_LOCF_values <- function(x,
                            sample_interval_minute = 5,
                            duration_hr = 24) {
  

  
  # find the na indices for LOCF (Last observation carried forward)
  # and carry forward unexpired observed values
  
  # Find the index of non-NA values in the value column
  non_na_index = which(!is.na(x$value))
  
  # if all are na, do nothing
  if(!(length(x$value)==sum(is.na(x$value)))){
    
    # because were already on the grid, we know each of the indices are one 
    # sample_interval_minute apart. we can think of 'duration' in terms of
    # indices and find which indices we can carry over before duration has expired
    
    max_carry_over_indices = base::floor(as.numeric((duration_hr*60)/
                                                sample_interval_minute))
    
    # find indices eligible for carry forward (if na)
    LOCF_idx = sapply(non_na_index,
                      FUN=function(y,max_carry_over){
                        y : (y+max_carry_over)
                      },
                      max_carry_over = max_carry_over_indices) |> 
      as.numeric() |> 
      unique()
    
    n_len = length(x$value)
    
    cond = ((1:n_len) %in% LOCF_idx)
    
    x$value[cond] = data.table::nafill(x$value[cond],
                                       type='locf')
    
  } else {
    warning('one or more variable has all NA values for an ID')
  }
  
  return(x)
}


#' Put Variable on time table
#' 
#' @param values vector of values for grid
#' @param IDs vector of associated IDs for each value and datatimes
#' @param datetimes vector of date times associate with each values / IDs
#' @param var_name scalar name of variable
#' @param duration_hr how many hours a variable's valuable can be carried forward if proceeding measurements are missing.
#' @param sample_interval_minute scalar time between one time grid point and another (in minutes)
#' @param aggregration_type how to handle multiple measurements for same time point; 'latest' or 'mean'
#' @param grid grid precomputed from GetTimeTable() 
#' @param start_times optional argument used if 'grid' is NULL vector of date times denoting the 'end' of a time series.
#' @param end_times optional argument used if 'grid' is NULL, vector of date times denoting the 'start' of a time series.
#' @param IDs_times optional argument used if 'grid' is NULL, vector of IDs associated with each start and end time.
#' @return a data.table with values on a discrete time grids
#' 
#' @export
#' 
PutOnTimeTable=function(
    values,
    IDs,
    datetimes = NULL,
    var_name=NULL,
    duration_hr = NA,
    sample_interval_minute = 30,
    aggregration_type = "latest",
    grid = NULL,
    start_times = NULL,
    end_times = NULL,
    IDs_times = NULL){

  
  
  ##################
  ## error checking
  ##################
  # check if sample interval valid
  check_sample_int_min(sample_interval_minute)
  # check if all grid variables are same length
  check_if_same_length(datetimes,values,IDs)
  
  if(is.null(grid)){
    grid <- InitializeTimeTable(start_times = start_times,
                        end_times = end_times,
                        sample_interval_minute = sample_interval_minute,
                        IDs = IDs_times)
  }
  IDs <- as.character(IDs)
  
  df = data.table('var_name' = rep(var_names,
                                   length(IDs)),
                  'IDs' = IDs,
                  'value' = value,
                  'datetimes' = datetimes)
  
  df$grid_datetimes = 
    lubridate::ceiling_date(df$datetimes, units = paste0(sample_interval_minute," min"))
  
  # if there are multiple measures per grid time point, we need to consolidate
  if(aggregate_mode == 'mean'){
    df <- df |>  table.express::group_by(grid_datetimes,IDs,var_name) |>
      table.express::summarise(value = mean(value,na.rm=TRUE)) |> as.data.table()
  } else if (aggregration_type == 'latest') {
    df <- df |> table.express::filter(is.na()) |> table.express::group_by(grid_datetimes,IDs,var_name) |> 
      table.express::mutate(max_datetimes = max(datetimes,na.rm = TRUE)) 
    df <- df |>  table.express::filter(datetimes == max_datetimes) |> 
      table.express::select(var_name,IDs,value,grid_datetimes) |> as.data.table()
  }
  
  setorder(df,IDs,var_name,grid_datetimes)
  
  # split by variable name
  list_df <- df |> data.table:::split.data.table(by='var_name')
  
  # join to grid to create NAs at missing times point
  measures_on_grid = lapply(list_df,
                            function(x,grid_use){
                              table.express::right_join(x,grid_use,by=c('grid_datetimes','IDs'))},
                            grid_use=grid)
  
  # if a measurement is taken later than the start of the grid,
  # there are NAs for 'var_name', let's fix that
  meas_name = names(measures_on_grid)
  n_meas = length(meas_name)
  n_row_grid = nrow(grid)
  i = 1
  
  for(i in 1:n_meas){
    measures_on_grid[[i]]$var_name <- rep(meas_name[i],  n_row_grid)
    
    temp <- split(measures_on_grid[[i]], 
                  by='IDs')
    
    measures_on_grid[[i]] = lapply(temp,
                                   get_LOCF_values,
                                   sample_interval_minute = sample_interval_minute,
                                   duration_hr=duration[i]) |> data.table::rbindlist()
  }
  
  return(measures_on_grid |> data.table::rbindlist())
}

CbindTimeTables=function(timetable1,timetable2){
 if(!nrow(timetable1)==nrow(timetable2)){
   stop("Time tables do not have equal number of rows")
 } 
  cbind()
  
}

#' check_sample_int_min; internal function
#' 
#' @param starts vector of start times
#' @param ends vector of end times
#' 
#' @noRd
check_sample_int_min = function(x){

  if(!length(x)==1){
    stop("ERROR: no or multiple sample_interval_min passed")
    
  } else if(!is.finite(x)){
    stop("ERROR: sample_interval_min is not a finite numeric value")
    
  } else if(x<0){
    stop("ERROR: sample_interval_min is not positive")
    
  } 
  return(x)
}

#' check_start_end_times; internal function
#' 
#' @param starts vector of start times
#' @param ends vector of end times
#' 
#' @noRd
check_start_end_times = function(starts,
                                ends){

  if(!length(starts) == length(ends)){
    stop('the number of start_times does not equal number of end_times') 
  }
  if(all(starts<ends)){
    stop('end_times not later than start_times') 
  }    
  if(all(lubridate::is.POSIXct(starts)) & 
     all(lubridate::is.POSIXct(ends))){
    stop('all times must be POSIXct') 
  }
  
}

#' check if valid tuples; internal function
#' @param time vector of times
#' @param val vector of values
#' @param ids vector of ids
#' 
#' @noRd
check_if_same_length = function(times,val,ids){
  
  if(length(times)==length(val)){
    stop('the number of times does not equal number of values') 
  } else if (length(ids)==length(ends)){
    stop('the number of times does not equal number of IDs')     
  }
}

#' check if valid 'aggregration_type' argument; internal function
#' @param time vector of times
#' @param val vector of values
#' @param ids vector of ids
#' 
#' @noRd
check_aggregration_type=function(x){
  if(!length(x)==1){
    stop("please provide (only) one aggregation type")
  } 
  if(!(x %in% c("latest","mean"))){
    stop("invalid aggregation type: must be 'latest' or 'mean'")
  }
}

