.datatable.aware <- TRUE

#' creates a sequence of times uniformally-spaced for an ID
#' @param  x data.frame from created mid GetTime.TableTemplate
#' @param  si see sample_interval in `GetTime.TableTemplate`
#' @noRd
ExpandTimes=function(x,si){
  
  
  times = seq(lubridate::ceiling_date(x[,"start_times"],unit = si),
              lubridate::ceiling_date(x[,"end_times"],unit = si),,
              by = si )
  
  df = data.frame(IDs = rep(x[,"IDs"],
                            length(times)),
                  grid_times = times)
  return(df)
}

#' internal last observed carried forward, used for non-numeric data type 
#' @param x
#' @noRd
CharLOCF <- function(x) {
  y <- !is.na(x)
  c(NA, x[y])[cumsum(y)+1]
}

#' creates a uniformly-spaced discrete-time grid data.table on which all other variables are appended to.
#' @param start_times vector of date times denoting the 'start' of a time series (e.g. the start of a patient's stay in the ICU) 
#' @param end_times vector of date times denoting the 'end' of a time series  (e.g. the end of a patient's stay in the ICU) 
#' @param IDs vector of ID associated with each discrete time series. If null, each start/end time pair is given a unique ID.
#' @param sample_interval: a string that denotes the time (and the respective units) between each index in the time.table. All meaningful specifications in the English language are supported. Some example of valid inputs: '45 secs', '30 min', '22.2 mins', '2 minutes', or '3 years'.
#' @return long table of variables, time points at every `sample_interval` from  start_times to end_times
#' @export
GetTime.TableTemplate <- function(start_times = NULL,
                                  end_times = NULL,
                                  IDs = NULL, 
                                  sample_interval = "30 minutes") { 
  
  
  
  
  ######
  # error checking
  #######
  CheckSampleInt(sample_interval)
  CheckStartEndTimes(start_times,
                     end_times)
  if(is.null(IDs)){
    IDs = as.character(1:length(start_times))
  } else {
    IDs <- as.character(IDs)
    CheckIfSameLength(times = start_times,
                      ids = IDs,
                      val = end_times)
  }
  
  # construct a data.frame of ID start and end
  time_df = data.frame('IDs' = IDs,
                       'start_times' = start_times,
                       'end_times' = end_times) 
  
  grid_df =
    ExpandTimes(time_df[1,],
                si = sample_interval)
  if(length(IDs)>1){
    for(i in 2:nrow(time_df)){
      grid_df = rbind(grid_df,
                      ExpandTimes(time_df[i,],
                                  si = sample_interval))
    }
  }
  grid_dt <- grid_df |> data.table::as.data.table()
  colnames(grid_dt) = c("IDs", "grid_datetimes")
  
  return(grid_dt)
  
}

#' GetLOCFvalue; internal function
#' 
#' @param x vector of date times for 'start' of a time series (e.g. the start of a patient's stay in the ICU) 
#' @param sample_interval scalar time between one time grid point and another
#' @param duration the time in which a variable's value can be carried forward before it is considered missing data. All meaningful specifications in the English language are supported. Some example of valid inputs: '45 secs', '30 min', '22.2 mins', '2 minutes', or '3 years'.
#' @return a vector where Last Observed Carry Forward
#' 
#' @noRd
GetLOCFvalue <- function(x,
                         sample_interval = 5,
                         duration = 24) {
  
  # find the na indices for LOCF (Last observation carried forward)
  # and carry forward unexpired observed value
  
  # Find the index of non-NA value in the value column
  non_na_index = which(!is.na(x$value))
  
  # if all are na, do nothing
  if(!(length(x$value)==sum(is.na(x$value)))){
    
    # because were already on the grid, we know each of the indices are one 
    # sample_interval apart. we can think of 'duration' in terms of
    # indices and find which indices we can carry over before duration has expired
    
    if(is.finite(duration)){
      
      max_carry_over_indices = base::floor(as.numeric((lubridate::period(duration)/
                                                         lubridate::period(sample_interval))))
    } else {
      
      max_carry_over_indices = length(x$value)
    }
    
    # find indices eligible for carry forward (if na)
    LOCF_idx = sapply(non_na_index,
                      FUN=function(y,max_carry_over){
                        y : (y+max_carry_over)
                      },
                      max_carry_over = max_carry_over_indices) |> 
      as.numeric() |> 
      unique()
    
    n_len = length(x$value)
    
    # find conjunction of actual indices and eligible indices (!indices > n_len)
    cond = ((1:n_len) %in% LOCF_idx)
    
    if(is.numeric(x$value[1])){
      x$value[cond] = data.table::nafill(x$value[cond],
                                         type='locf')
    } else{
      x$value[cond] = CharLOCF(x$value[cond])
    }
  } else {
    warning('warning: a variable has only NA values for an ID')
  }
  
  return(x)
}


#' Put Variable on time.table
#' 
#' @param values  a vector of values to be added to the time table.
#' @param IDs  a vector of IDs associated with each value in value.
#' @param datetimes a vector of date-time objects associated with each value in value.
#' @param var_name a scalar string of the name of the variable to be added.
#' @param duration the time (and the respective units) for which a variable's value can be carried forward before it is considered missing data. All meaningful specifications in the English language are supported. Some example of valid inputs: '45 secs', '30 min', '22.2 mins', '2 minutes', or '3 years'.
#' @param sample_interval a string that denotes the time (and the respective units) between each index in the time.table. All meaningful specifications in the English language are supported. Some example of valid inputs: '45 secs', '30 min', '22.2 mins', '2 minutes', or '3 years'.
#' @param aggregration_type how to handle multiple measurements for same time point; 'latest' or 'mean'
#' @param Template the time grid 'Template' precomputed from GetTime.TableTemplate() 
#' @param start_times optional argument used if 'Template' is NULL vector of date times denoting the 'end' of a time series.
#' @param end_times optional argument used if 'Template' is NULL, vector of date times denoting the 'start' of a time series.
#' @param IDs_times optional argument used if 'Template' is NULL, vector of IDs associated with each start and end time.
#' @return a data.table with value on a discrete time grids
#' 
#' @export
PutOnTime.Table=function(
    values,
    IDs,
    datetimes = NULL,
    var_name = NULL,
    duration = Inf,
    sample_interval = NULL,
    aggregration_type = "latest",
    Template = NULL,
    start_times = NULL,
    end_times = NULL,
    IDs_times = NULL){
  
  # TODO: you can eliminate this line of code if tests dont fail
  # .datatable.aware = TRUE
  
  ##################
  ## error checking
  ##################
  if(!is.finite(duration)){
    duration = Inf
  } 
  
  # check if sample interval valid
  CheckSampleInt(sample_interval)
  
  # check if all grid variables are same length
  CheckIfSameLength(datetimes,values,IDs)
  
  if(is.null(Template)){
    Template <- GetTime.TableTemplate(start_times = start_times,
                                      end_times = end_times,
                                      sample_interval = sample_interval,
                                      IDs = IDs_times)
  }
  
  IDs <- as.character(IDs)
  
  df = data.table('var_name' = rep(var_name,
                                   length(IDs)),
                  'IDs' = IDs,
                  'value' = values,
                  'datetimes' = datetimes)
  
  df = df |> table.express::mutate(grid_datetimes =
                                     lubridate::ceiling_date(datetimes, unit = sample_interval)
  ) |> 
    as.data.table()
  
  # if there are multiple measures per grid time point, we need to consolidate
  if(aggregration_type == 'mean'){
    df <- df |>  table.express::group_by(grid_datetimes,IDs,var_name) |>
      table.express::summarise(value = mean(value,na.rm=TRUE)) |> as.data.table()
  } else if (aggregration_type == 'latest') {
    df <- df |> table.express::filter(!is.na(value)) |> 
      table.express::group_by(grid_datetimes,IDs,var_name) |> 
      table.express::mutate(max_datetimes = max(datetimes,na.rm = TRUE)) 
    df <- df |>  table.express::filter(datetimes == max_datetimes) |> 
      table.express::select(var_name,IDs,value,grid_datetimes) |> as.data.table()  
  }
  
  setorder(df,IDs,var_name,grid_datetimes)
  
  # split by variable name
  list_df <- df |> data.table:::split.data.table(by='var_name')
  
  # join to Template to create NAs at missing times point
  measures_on_grid = lapply(list_df,
                            function(x,grid_use){
                              table.express::right_join(x,grid_use,
                                                        by=c('grid_datetimes','IDs')) },
                            grid_use=Template)
  
  # if a measurement's associated time is later than the start of the grid
  # there are NAs for 'var_name', let's fix that
  meas_name = names(measures_on_grid)
  n_meas = length(meas_name)
  n_row_grid = nrow(Template)
  i = 1
  
  for(i in 1:n_meas){
    measures_on_grid[[i]]$var_name <- rep(meas_name[i],  n_row_grid)
    
    temp <- split(measures_on_grid[[i]],
                  by='IDs')
    
    measures_on_grid[[i]] = lapply(temp,
                                   GetLOCFvalue,
                                   sample_interval = sample_interval,
                                   duration=duration) |> data.table::rbindlist()
  }
  out =  measures_on_grid |> data.table::rbindlist()
  return(out)
}


#' Combine time.tables
#' @param time.table_names character vector containing the name of strings
#' @param join_type "right join", "cbind", "cbind_fast". "cbind_fast" is slightly more efficient but assumes everything is ordered properly, while "cbind" orders
#'  your data.table by IDs and grid_datetimes. "right_join" is the safest but least efficient and will even work with time.tables of unequal rows.
#' @export
CombineTime.Tables = function(time.table_names,
                              join_type=c("right_join")){
  
  n_tables = length(time.table_names)
  if(n_tables>1){
    out = Merge2Time.Tables(get(time.table_names[2]),get(time.table_names[1]),join_type)
    if(n_tables>2){
      for(i in 3:n_tables){
        out = Merge2Time.Tables(get(time.table_names[i]),out,join_type)
      }
    }
  } else {
    stop("number of time.tables is less than 1")
  }
  return(out)
}

#' merge time tables
#' 
#' @param giver_tt a new time.table returned from 'PutOnTime.Table'
#' @param reciever_tt a group of previously combined time tables or a new time table returned from 'PutOnTime.Table'
#' @param join_type "right join", "cbind", "cbind_fast". "cbind_fast" is slightly more efficient but assumes everything is ordered properly, while "cbind" orders
#'  your data.table by IDs and grid_datetimes. "right_join" is the safest but least efficient and will even work with Time.Tables of unequal rows.
Merge2Time.Tables=function(giver_tt,reciever_tt,join_type=c("right_join")){
  
  if(join_type=="cbind"){
    if(!nrow(giver_tt ) == nrow(reciever_tt)){
      stop("Time tables do not have equal number of rows for a 'cbind' join")
    }  
    
    data.table::setorder(giver_tt,IDs,grid_datetimes)
    data.table::setorder(reciever_tt,IDs,grid_datetimes)
    
    giver_tt  = PrepForJoin(giver_tt ,keep_template = FALSE)
    reciever_tt = PrepForJoin(reciever_tt,keep_template = TRUE)
    
    return(cbind(giver_tt ,reciever_tt))
    
  } else if(join_type=="cbind_fast"){
    
    if(!nrow(giver_tt ) == nrow(reciever_tt)){
      stop("Time tables do not have equal number of rows for a 'cbind' join")
    }  
    
    giver_tt  = PrepForJoin(giver_tt ,keep_template = FALSE)
    reciever_tt = PrepForJoin(reciever_tt,keep_template = TRUE)
    
    return(cbind(giver_tt ,reciever_tt))
    
  }  else if(join_type == 'right_join'){
    
    giver_tt  = PrepForJoin(giver_tt ,keep_template = TRUE)
    reciever_tt = PrepForJoin(reciever_tt,keep_template = TRUE)
    
    return(table.express::right_join(giver_tt ,reciever_tt,by=c("IDs","grid_datetimes")))
    
  } else {
    stop("invalid join_type")
  }
  
}

#' Prep a time.table to be joined
#' @param time.table a time.table
#' @param keep_template preserve 'IDs' and 'grid_datetimes' columns
#' @noRd
PrepForJoin=function(time.table, keep_template=TRUE){
  if(all(names(time.table) %in% c('var_name','IDs', 'value', 'grid_datetimes'))){
    
    var_name = time.table[1,'var_name'] |> as.character()
    
    if(keep_template){
      time.table=time.table[,list(value,IDs,grid_datetimes)]
    } else {
      time.table=time.table[,list(value)]
    }
    
    names(time.table)[names(time.table) == "value"] <- var_name
  } 
  
  return(time.table)
}

#' CheckSampleInt; internal function
#' 
#' @param starts vector of start times
#' @param ends vector of end times
#' 
#' @noRd
CheckSampleInt = function(x){
  
  if(!length(x)==1){
    stop("ERROR: no or multiple sample_interval_min passed")
    
  } 
  # else if(!is.finite(x)){
  #   stop("ERROR: sample_interval_min is not a finite numeric value")
  #   
  # } else if(x<0){
  #   stop("ERROR: sample_interval_min is not positive")
  #   
  # } 
  return(x)
}

#' CheckStartEndTimes; internal function
#' 
#' @param starts vector of start times
#' @param ends vector of end times
#' 
#' @noRd
CheckStartEndTimes = function(starts,
                              ends){
  
  if(!length(starts) == length(ends)){
    stop('the number of start_times does not equal number of end_times') 
  }
  if(!all(starts<ends)){
    
    stop(paste0('end_times not later than start_times',"check starts with index: ", which(!starts<ends)) 
         }    
  if(!(all(lubridate::is.POSIXct(starts)) & 
       all(lubridate::is.POSIXct(ends)))){
    stop('all times must be POSIXct') 
  }
  
  }

#' check if valid tuples; internal function
#' @param time vector of times
#' @param val vector of value
#' @param ids vector of ids
#' 
#' @noRd
CheckIfSameLength = function(times,val,ids){
  
  if(!length(times)==length(val)){
    stop('the number of times does not equal number of value') 
  } else if (!length(ids)==length(val)){
    stop('the number of times does not equal number of IDs')     
  }
  
}

#' check if valid 'aggregration_type' argument; internal function
#' @param time vector of times
#' @param val vector of value
#' @param ids vector of ids
#' 
#' @noRd
CheckAggregrationType=function(x){
  
  if(!length(x)==1){
    stop("please provide (only) one aggregation type")
  } 
  if(!(x %in% c("latest","mean"))){
    stop("invalid aggregation type: must be 'latest' or 'mean'")
  }
}

