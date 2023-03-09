
<!-- README.md is generated from README.Rmd. Please edit that file -->

# time.table

<!-- badges: start -->
<!-- badges: end -->

Often when dealing with messy, real-world time series data, we have many
variables sampled at different frequencies or sampled intermittently.
The ‘time.table’ library provides functions to easily wrangle these
different variables into a uniformly-spaced tabular format. ‘time.table’
is a library built around the popular, high-performance data
manipulation library ‘data.table’ to ensure scalability to large data
problems.

## Installation

You can install the development version of time.table from
[GitHub](https://github.com/bmgaldo/time.table) with:

``` r
# install.packages("devtools")
devtools::install_github("bmgaldo/time.table")
```

# Example data

``` r
library(time.table)
library(data.table)
library(table.express)


df_admit_discharge = data.frame(patient_id=paste0("pat_",1:2),
                                admit=c(as.POSIXct("2012-03-12 12:12:12"),
                                        as.POSIXct("2012-03-13 11:11:11")),
                                discharge=c(as.POSIXct("2012-3-20 12:12:12"),
                                            as.POSIXct("2012-03-20 10:10:10")))

head(df_admit_discharge)
#>   patient_id               admit           discharge
#> 1      pat_1 2012-03-12 12:12:12 2012-03-20 12:12:12
#> 2      pat_2 2012-03-13 11:11:11 2012-03-20 10:10:10
```

# Create a time table template

this specifics the start & end times of each IDs table. This template
will be the scaffold in which variables will be added to a timetable.

``` r

TimeTableTempl = time.table::GetTimeTableTemplate(start_times = df_admit_discharge$admit,
                                                  end_times = df_admit_discharge$discharge,
                                                  IDs = df_admit_discharge$patient_id,
                                                  sample_interval= "60 min" # grid point at every 60th minute 
)

head(TimeTableTempl)
#>      IDs      grid_datetimes
#> 1: pat_1 2012-03-12 13:00:00
#> 2: pat_1 2012-03-12 14:00:00
#> 3: pat_1 2012-03-12 15:00:00
#> 4: pat_1 2012-03-12 16:00:00
#> 5: pat_1 2012-03-12 17:00:00
#> 6: pat_1 2012-03-12 18:00:00
```

# Add a variable feature to a time table

First, we’ll generate some fake data for our two patients. In this
example, a patient’s heart rate is measured every few hours.

``` r

# patient 1
times_pat_1 = seq(as.POSIXct("2012-03-12 12:12:12"),
                  as.POSIXct("2012-03-20 20:20:20"),
                  by="5 hours") # patient 1 has their heart rate measured every 5 hours
values_pat_1 = rnorm(length(times_pat_1),80,20)
ID_pat_1 = rep("pat_1",length(times_pat_1))

# patient 2
times_pat_2 = seq(as.POSIXct("2012-03-13 11:11:11"),
                  as.POSIXct("2012-03-20 10:10:10"),
                  by="4 hours") # patient 2 has their heart rate measured every 4 hours
values_pat_2 = rnorm(length(times_pat_2),70,10)
ID_pat_2 = rep("pat_2",length(times_pat_2))


# let's store consolidate these into a data.frame
df_heartrate = data.frame(IDs = c(ID_pat_1,ID_pat_2),
                          heart_rate = c(values_pat_1,values_pat_2),
                          times = c(times_pat_1,times_pat_2))

head(df_heartrate)
#>     IDs heart_rate               times
#> 1 pat_1   73.17768 2012-03-12 12:12:12
#> 2 pat_1   87.32907 2012-03-12 17:12:12
#> 3 pat_1  100.66232 2012-03-12 22:12:12
#> 4 pat_1   93.96322 2012-03-13 03:12:12
#> 5 pat_1   75.72408 2012-03-13 08:12:12
#> 6 pat_1   51.32580 2012-03-13 13:12:12
```

``` r

heart_rate_tt = time.table::PutOnTimeTable(value = df_heartrate$heart_rate,
                                           datetimes = df_heartrate$times,
                                           IDs = df_heartrate$IDs,
                                           var_name = "heart_rate",
                                           Template = TimeTableTempl, # pass template from section one
                                           sample_interval = "60 min",
                                           aggregration_type = "mean", #how to handle multiple measurements for same time point
                                           duration  = "2 hours"# assume measures are carried forward 2 hours if no new measurement
)

head(heart_rate_tt)
#>         grid_datetimes   IDs   var_name    value
#> 1: 2012-03-12 13:00:00 pat_1 heart_rate 73.17768
#> 2: 2012-03-12 14:00:00 pat_1 heart_rate 73.17768
#> 3: 2012-03-12 15:00:00 pat_1 heart_rate 73.17768
#> 4: 2012-03-12 16:00:00 pat_1 heart_rate       NA
#> 5: 2012-03-12 17:00:00 pat_1 heart_rate       NA
#> 6: 2012-03-12 18:00:00 pat_1 heart_rate 87.32907
```

# Add a static feature to a time table

``` r

# patient 1
times_pat_1 = as.POSIXct("2012-03-12 12:12:12")
values_pat_1 = "male"
ID_pat_1 = rep("pat_1",length(values_pat_1))

# patient 2
times_pat_2 = as.POSIXct("2012-03-13 11:11:11")
values_pat_2 = "female"
ID_pat_2 = rep("pat_2",length(values_pat_2))


# let's store consolidate these into a data.frame
df_sex = data.frame(IDs=c(ID_pat_1,ID_pat_2),
                    value=c(values_pat_1,values_pat_2),
                    datetimes=c(times_pat_1,times_pat_2))

head(df_sex)
#>     IDs  value           datetimes
#> 1 pat_1   male 2012-03-12 12:12:12
#> 2 pat_2 female 2012-03-13 11:11:11

sex_tt = PutOnTimeTable(value = df_sex$value,
                        datetimes = df_sex$datetimes,
                        IDs = df_sex$IDs,
                        var_name = "sex",
                        Template = TimeTableTempl, # pass template from section one 
                        sample_interval = "60 min", 
                        aggregration_type = "latest", # how to handle multiple measurements for same time point 
                        duration = NA # a measure doesn't expire here since it's static 
                        # both 'NA' or 'Inf' would work here.
)

head(sex_tt)
#>    var_name   IDs value      grid_datetimes
#> 1:      sex pat_1  male 2012-03-12 13:00:00
#> 2:      sex pat_1  male 2012-03-12 14:00:00
#> 3:      sex pat_1  male 2012-03-12 15:00:00
#> 4:      sex pat_1  male 2012-03-12 16:00:00
#> 5:      sex pat_1  male 2012-03-12 17:00:00
#> 6:      sex pat_1  male 2012-03-12 18:00:00
```

# Combine time tables into a singular data.table

## Right join

``` r

CombineTimeTables(c("sex_tt","heart_rate_tt"),
                  join_type='right_join') 
#>      heart_rate   IDs      grid_datetimes    sex
#>   1:   73.17768 pat_1 2012-03-12 13:00:00   male
#>   2:   73.17768 pat_1 2012-03-12 14:00:00   male
#>   3:   73.17768 pat_1 2012-03-12 15:00:00   male
#>   4:         NA pat_1 2012-03-12 16:00:00   male
#>   5:         NA pat_1 2012-03-12 17:00:00   male
#>  ---                                            
#> 357:         NA pat_2 2012-03-20 07:00:00 female
#> 358:   79.66092 pat_2 2012-03-20 08:00:00 female
#> 359:   79.66092 pat_2 2012-03-20 09:00:00 female
#> 360:   79.66092 pat_2 2012-03-20 10:00:00 female
#> 361:         NA pat_2 2012-03-20 11:00:00 female
```

## Cbind

faster but less safe

``` r
# 
tt = CombineTimeTables(c("sex_tt","heart_rate_tt"),
                       join_type='cbind')

head(tt)
#>    heart_rate  sex   IDs      grid_datetimes
#> 1:   73.17768 male pat_1 2012-03-12 13:00:00
#> 2:   73.17768 male pat_1 2012-03-12 14:00:00
#> 3:   73.17768 male pat_1 2012-03-12 15:00:00
#> 4:         NA male pat_1 2012-03-12 16:00:00
#> 5:         NA male pat_1 2012-03-12 17:00:00
#> 6:   87.32907 male pat_1 2012-03-12 18:00:00
```
