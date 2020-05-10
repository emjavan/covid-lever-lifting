##############################################################
### Code for plotting on Wrangler
##############################################################
library(ggplot2)
library(gmodels) # needed for ci()
library(scales) # needed for muted() in ggplot
options(warn=-1) # suppress all the warnings from ci()

########################################################################################
########################### NEW FUNCTIONS 
########################################################################################

# function write csv for each R0>1 probability dataframe, plots heat map and US by counties
plot_prob_R0_above_1_county_map = function(df, init_num_infected){
  ################# Get all the county data ready to use #################
  prob_by_day = read.csv("prob_r0_above_below_1.csv") # prob of R0>1 given days following lever lift greater than lift day, or less
  # get day counties open and days since, currently hard coded by should move to automatic update of days and diff
  # depends on most recent NYT data as well and if that has updated
  opening_data=read.csv("raw_data/county_opening_data.csv")
  case_data = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") # open csv with NYT data for cumulative cases and deaths
  case_data$fips = as.double(case_data$fips)
  
  all_counties=usmap::us_map(regions = "counties")%>% 
    as_tibble() %>% 
    distinct(fips, abbr, county)
  all_counties$fips=as.double(all_counties$fips)
  
  map_cases=merge(all_counties, case_data, by ="fips", all.x = TRUE) # fips need to be same type so it doesn't only match strings
  map_cases$cases[is.na(map_cases$cases)]=0 # all.x is to keep all rows from all_counties data frame
  map_cases=map_cases[order(map_cases$fips, map_cases$county.x, as.Date(map_cases$date)),] # reorder to map case difference correctly
  
  # Get daily new cases through time for all counties
  FIPS=unique(map_cases$fips)
  daily_cases=c()
  for(i in 1:length(FIPS)){
    sub_df=subset(map_cases, fips==FIPS[i])
    cases=sub_df$cases
    temp_vect = rep(0, length(sub_df$fips))
    for(j in 1:length(sub_df$fips)){
      if(j==1){
        temp_vect[j]=cases[j] # minus 0 from previous days
      }else{
        temp_vect[j]=cases[j]-cases[j-1]
      } # end if
    } # end for j
    daily_cases = c(daily_cases, temp_vect)
  } # end for i
  daily_cases[daily_cases<0]=0
  map_cases = cbind(map_cases, daily_cases)
  
  # get only needed columns from df
  opening_data = subset(opening_data, select = c(fips, Reopening_date, Today_date, Days_after_lift) )
  map_opening=merge(map_cases, opening_data, all.x = TRUE)
  map_opening = map_opening %>% 
    rename(County = county.x)
  map_opening = subset(map_opening, select = -c(county.y))
  reopening_cases_df=subset(map_opening, (as.Date(map_opening$Reopening_date, "%m/%d/%y")==as.Date(map_opening$date))==TRUE) # reopening day cases matched to date
  today_cases_df=subset(map_opening, (as.Date(map_opening$Today_date, "%m/%d/%y")==as.Date(map_opening$date))==TRUE) # most recent day's cases matched to date
  reopen_today_cases_df = merge(reopening_cases_df,  today_cases_df,
                                by = c("fips", "abbr", "County", "state", "Reopening_date", "Today_date","Days_after_lift")) # combine dfs
  delta_daily_cases = (reopen_today_cases_df$daily_cases.y - reopen_today_cases_df$daily_cases.x) # current daily cases - reopening daily cases = change in detections
  reopen_today_cases_df = cbind(reopen_today_cases_df, delta_daily_cases )
  
  map_data=usmap::us_map(regions = "counties") # get polygon data for all US counties
  map_data$fips = as.numeric(map_data$fips)
  
  ################# Make plots for all data together and split by init inf #################
  inf_vect_len = length(init_num_infected)
  for(k in 1:(inf_vect_len+1)){
    if(k<=inf_vect_len){
      temp_df=subset(df, Init_Inf==init_num_infected[k])
      prob_df <- temp_df %>% 
        group_by(tl, delta) %>% 
        summarize(prob_R0_above1 = sum(R0>1)/n()) %>% 
        ungroup()
      write.csv(prob_df, paste0("wrangler_output/init_inf_", init_num_infected[k],"_prob_r0_above_1.csv")) # write output to csv
      
      #### Assign probability R0> 1 to each county that isn't NA
      num_county = length(reopen_today_cases_df$fips)
      county_prob_vect=rep(NA, num_county) 
      for(c in 1:num_county){
        # look up with subset
        temp=subset(prob_df, prob_df$tl == reopen_today_cases_df$Days_after_lift[c] & 
                      prob_df$delta == reopen_today_cases_df$delta_daily_cases[c])
        if(nrow(temp)==1){ # if value was found in table assign the exact probability
          county_prob_vect[c] = temp$prob_R0_above1
        }else{ # if the value is not found in table assign most likely probability from other values
          # subset of only the days since lever lift
          temp2 = subset(prob_df, prob_df$tl == reopen_today_cases_df$Days_after_lift[c])
          if(reopen_today_cases_df$delta_daily_cases[c] > 0){
            county_prob_vect[c] = max(temp2$prob_R0_above1) # give max prob for those days out if delta is positive
          }else{
            county_prob_vect[c] = min(temp2$prob_R0_above1) # give min prob for those days out if delta negative
          } # end if delta is positive or negative
        } # end if value not found in table
      } # end for i
      
      temp_reopen_today_cases_df = cbind(reopen_today_cases_df, county_prob_vect)
      temp_reopen_today_cases_df$fips = as.numeric(temp_reopen_today_cases_df$fips)
      new_map_data = map_data %>% 
        left_join(temp_reopen_today_cases_df, by = "fips")
      
      # Plot US Map
      png(file=paste0("wrangler_output/init_inf",init_num_infected[k], "_us_map_", temp_reopen_today_cases_df$date.y[1], ".png"), 
          width=5.25,height=3.25, units = "in", res=1200)
      plot=ggplot(new_map_data, aes(x = x, y = y)) + 
        geom_polygon(aes(group = group, fill = county_prob_vect), color = "black", size = 0.1) +
        scale_fill_gradient(low = "gainsboro", high = "dark red", name = "Probability")+
        scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
        labs(title = paste0("Probability R0>1 as of ", temp_reopen_today_cases_df$date.y[1], ", InitInf=",init_num_infected[k])) +
        theme(panel.background = element_rect(color = "white", fill = "white"), 
              legend.title=element_text(size=8), 
              legend.text=element_text(size=6),
              title =element_text(size=8))
      print(plot)
      dev.off()
      
      # Plot heat map
      heat_map=ggplot(prob_df , aes(tl,delta, fill = prob_R0_above1)) + geom_tile() +
        labs(title = paste0("Probability  R0>1 Heat Map, InitInf=",init_num_infected[k]))+
        xlab("tl, time since lever lift (days)")+
        ylab("delta, change in new detected cases")
      png(file=paste0("wrangler_output/init_inf", init_num_infected[k], "_heat_map.png"), 
          width=4.25,height=4.25, units = "in", res=1200)
      print(heat_map)
      dev.off()
    }else{
      prob_df <- df %>% 
        group_by(tl, delta) %>% 
        summarize(prob_R0_above1 = sum(R0>1)/n()) %>% 
        ungroup()
      write.csv(prob_df, "wrangler_output/all_init_inf_prob_r0_above_1.csv")
      
      #### Assign probability R0> 1 to each county that isn't NA
      num_county = length(reopen_today_cases_df$fips)
      county_prob_vect=rep(NA, num_county) 
      for(c in 1:num_county){
        # look up with subset
        temp=subset(prob_df, prob_df$tl == reopen_today_cases_df$Days_after_lift[c] & 
                      prob_df$delta == reopen_today_cases_df$delta_daily_cases[c])
        if(nrow(temp)==1){ # if value was found in table assign the exact probability
          county_prob_vect[c] = temp$prob_R0_above1
        }else{ # if the value is not found in table assign most likely probability from other values
          # subset of only the days since lever lift
          temp2 = subset(prob_df, prob_df$tl == reopen_today_cases_df$Days_after_lift[c])
          if(reopen_today_cases_df$delta_daily_cases[c] > 0){
            county_prob_vect[c] = max(temp2$prob_R0_above1) # give max prob for those days out if delta is positive
          }else{
            county_prob_vect[c] = min(temp2$prob_R0_above1) # give min prob for those days out if delta negative
          } # end if delta is positive or negative
        } # end if value not found in table
      } # end for i
      
      temp_reopen_today_cases_df = cbind(reopen_today_cases_df, county_prob_vect)
      temp_reopen_today_cases_df$fips = as.numeric(temp_reopen_today_cases_df$fips)
      new_map_data = map_data %>% 
        left_join(temp_reopen_today_cases_df, by = "fips")
      
      # Plot US Map
      png(file=paste0("wrangler_output/all_init_inf_us_map_", temp_reopen_today_cases_df$date.y[1], ".png"), 
          width=5.25,height=3.25, units = "in", res=1200)
      plot=ggplot(new_map_data, aes(x = x, y = y)) + 
        geom_polygon(aes(group = group, fill = county_prob_vect), color = "black", size = 0.1) +
        scale_fill_gradient(low = "gainsboro", high = "dark red", name = "Probability")+
        scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
        labs(title = paste0("Probability R0>1 as of ", temp_reopen_today_cases_df$date.y[1], ", InitInf=All")) +
        theme(panel.background = element_rect(color = "white", fill = "white"), 
              legend.title=element_text(size=8), 
              legend.text=element_text(size=6),
              title =element_text(size=8))
      print(plot)
      dev.off()
      
      # Plot heat map
      heat_map=ggplot(prob_df , aes(tl,delta, fill = prob_R0_above1)) + geom_tile() +
        labs(title = "Probability  R0>1 Heat Map, InitInf=All")+
        xlab("tl, time since lever lift (days)")+
        ylab("delta, change in new detected cases")
      png(file=paste0("wrangler_output/all_init_inf_heat_map.png"), 
          width=4.25,height=4.25, units = "in", res=1200)
      print(heat_map)
      dev.off()
    } # end if else to plot subset of the full data frame
  } # end for loop over all plotting options
} # end function plot_prob_R0_above_1_county_map


# function to put sims in data frame of "Init_Inf", "R0", "tl", "delta"
get_r0_tl_delta_by_pt_compare = function(data_path){
  load(data_path)
  print(data_path)
  path_split           = unlist(str_split(data_path, "_"))
  initial_R0           = as.double(path_split[3])
  init_inf             = as.double(path_split[4])
  increase_R0          = as.double(path_split[5])
  R0_final             = initial_R0+increase_R0
  days_after_lift      = 30
  total_days           = days_after_lift + 1
  days_after_lift_vect = seq(0, days_after_lift, 1)
  init_R0_vect         = rep(initial_R0,  (total_days))
  incr_R0_vect         = rep(increase_R0, (total_days))
  final_R0_vect        = rep(R0_final,    (total_days))
  init_inf_vect        = rep(init_inf,    (total_days))
  
  #sim_index <- 0 # get sim_index # leaving in to rember how to get index in sapply
  case_det_info_after_lift=sapply(sims, simplify = TRUE, function(x){
    #sim_index <<- sim_index + 1 # <<- is 'superassignment' operator
    if(increase_R0 == 0){ #R0 was not increase so have to figure out which day it would have been
      lever_lift_count = 0
      for(day_count in 1:1000){ # 100 is just a safe upper bound, but loop will break before that
        # get the max of 14 day window
        if(day_count < 14){
          max_det_in_one_day = max(x$New_Detections[1:day_count])
        }else{
          max_det_in_one_day = max(x$New_Detections[(day_count-13):day_count])
        } # end if else
        # Some simulations die out before a lever would be lifted, so action has no impact
        if(is.na(max_det_in_one_day) | day_count > length(x$New_Detections) ){
          if(day_count > length(x$New_Detections)){
            lift_day = length(x$New_Detections)
          }else{
            lift_day= day_count+1
          } # end if else
          break;
        }else{  # if max is replaced with larger non-zero number increase lever, otherwise decrease
          if(x$New_Detections[day_count] == max_det_in_one_day  & max_det_in_one_day != 0 ){
            lever_lift_count = lever_lift_count + 1
          }else{
            lever_lift_count = lever_lift_count - 1
          } # end if else if
          # when sufficiently low get day lever would have lifted and break for loop
          if(lever_lift_count <= -14){
            lift_day = day_count+1
            break;
          } # end if else
        } # end if else
      } # end for day_count 
    }else{ # for R0 increase greater than 0 the lever lift day does not need to be computed
      lift_day = which(diff(x$R0)!=0)+1       # get the day R0 increased in simulation
      if(length(lift_day)==0){ lift_day = length(x$New_Detections) } # Sim may end before R0 increased
    } # end if else
    
    det_after_lift=x$New_Detections[lift_day:(lift_day+days_after_lift)]
    det_after_lift[is.na(det_after_lift)]=0 # incase the simulation died out before 30days after lever lift day
    diff_after_lift = det_after_lift-det_after_lift[1] # subtract the lever lift day from rest to get delta
    # sim_index_vect  = rep(sim_index, (total_days)) # leaving in to rember how to get index in sapply
    
    return(diff_after_lift)
  }) # end sapply
  # R0, tl= the time since lever lifted, delta is the difference in new detected cases from lever lift day
  final_df=data.frame(init_inf=double(), R0=double(), tl=double(), delta=double()) # initialize empty final data frame
  for(i in 1:length(sims)){ # get column from matrix and put in data frame
    temp_df=data.frame(init_inf_vect, final_R0_vect, days_after_lift_vect, case_det_info_after_lift[,i])
    final_df = rbind(final_df, temp_df) # row rind for all 10,000 sims
  }
  names(final_df)=c("Init_Inf", "R0", "tl", "delta")
  return(final_df)
} # end function get_r0_tl_delta_by_pt_compare


########################################################################################
########################### OLD FUNCTIONS 
########################################################################################

get_mean_after_lift_stats_mean_compare = function(data_path){
  load(data_path)
  print(data_path)
  path_split           = unlist(str_split(data_path, "_"))
  initial_R0           = as.double(path_split[3])
  init_inf             = as.double(path_split[4])
  increase_R0          = as.double(path_split[5])
  R0_final             = initial_R0+increase_R0
  days_after_lift      = 30
  total_days           = days_after_lift + 1
  days_after_lift_vect = seq(0, days_after_lift, 1)
  init_R0_vect         = rep(initial_R0,  (total_days))
  incr_R0_vect         = rep(increase_R0, (total_days))
  final_R0_vect        = rep(R0_final,    (total_days))
  init_inf_vect        = rep(init_inf,    (total_days))
  
  case_det_info_after_lift=sapply(sims, function(x){
    if(increase_R0 == 0){ #R0 was not increase so have to figure out which day it would have been
      lever_lift_count = 0
      for(day_count in 1:1000){ # 100 is just a safe upper bound, but loop will break before that
        # get the max of 14 day window
        if(day_count < 14){
          max_det_in_one_day = max(x$New_Detections[1:day_count])
        }else{
          max_det_in_one_day = max(x$New_Detections[(day_count-13):day_count])
        } # end if else
        # Some simulations die out before a lever would be lifted, so action has no impact
        if(is.na(max_det_in_one_day) | day_count > length(x$New_Detections) ){
          if(day_count > length(x$New_Detections)){
            lift_day = length(x$New_Detections)
          }else{
            lift_day= day_count+1
          } # end if else
          break;
        }else{  # if max is replaced with larger non-zero number increase lever, otherwise decrease
          if(x$New_Detections[day_count] == max_det_in_one_day  & max_det_in_one_day != 0 ){
            lever_lift_count = lever_lift_count + 1
          }else{
            lever_lift_count = lever_lift_count - 1
          } # end if else if
          # when sufficiently low get day lever would have lifted and break for loop
          if(lever_lift_count <= -14){
            lift_day = day_count+1
            break;
          } # end if else
        } # end if else
      } # end for day_count 
    }else{ # for R0 increase greater than 0 the lever lift day does not need to be computed
      lift_day = which(diff(x$R0)!=0)+1       # get the day R0 increased in simulation
      if(length(lift_day)==0){ lift_day = length(x$New_Detections) } # Sim may end before R0 increased
    } # end if else
    
    det_after_lift=x$New_Detections[lift_day:(lift_day+days_after_lift)]
    det_after_lift[is.na(det_after_lift)]=0
    
    # nested for loops defintely not ideal, but not sure how else to deal with this
    first_less_than_mean_rest = c()
    for(increase_by in 0:50){ # get the probs for increase_by from 0:50
      # if not using difference, vector doesn't need to be length total days
      # but this matches with other fucntions currently
      tempTF1 = c(NA)
      first_less_than_mean_rest = c(first_less_than_mean_rest, tempTF1)
      # rather than point comparison lift day is compared with mean of the following days
      for(i in 2:(total_days) ){
        tempTF= as.integer(((det_after_lift[1]+increase_by) < mean(det_after_lift[2:i]) ))
        first_less_than_mean_rest = c(first_less_than_mean_rest, tempTF)
      } # end for i
    } # end for increase by
    
    # put all in 1 vector which will return as row of matrix
    all_val=c(first_less_than_mean_rest)
    return(all_val)
  }) # end sapply
  
  means=rowMeans(case_det_info_after_lift)
  df=data.frame(init_inf_vect, init_R0_vect, incr_R0_vect, final_R0_vect,
                days_after_lift_vect, means[1:(total_days)])
  names(df)=c("Init_Infected", "Init_R0", "Increase_by_R0", "R0_Final", 
              "Days_after_lift", "Det_less_than_lift")
  # append the rest of means to the data frame 
  for(i in 1:50){
    df[[paste0("Det_less_than_lift_",i)]] = means[(i*total_days+1):((i+1)*total_days)]
  }
  return(df)
} # end function get_mean_after_lift_stats_mean_compare









### Get the the mean difference in new detections after lift day and percent of days with new detections greater than lift day
# days after lift is hard coded but should maybe be an input
# Final R0>1 doesn't have error checking coded for if days after lift is beyond the last day of detections
# increase by is to determine how much greater the following days is from lift day
get_mean_after_lift_stats_pt_compare = function(data_path){
  load(data_path)
  print(data_path)
  path_split           = unlist(str_split(data_path, "_"))
  initial_R0           = as.double(path_split[3])
  init_inf             = as.double(path_split[4])
  increase_R0          = as.double(path_split[5])
  R0_final             = initial_R0+increase_R0
  days_after_lift      = 30
  total_days           = days_after_lift + 1
  days_after_lift_vect = seq(0, days_after_lift, 1)
  init_R0_vect         = rep(initial_R0,  (total_days))
  incr_R0_vect         = rep(increase_R0, (total_days))
  final_R0_vect        = rep(R0_final,    (total_days))
  init_inf_vect        = rep(init_inf,    (total_days))
  
  case_det_info_after_lift=sapply(sims, function(x){
    if(increase_R0 == 0){ #R0 was not increase so have to figure out which day it would have been
      lever_lift_count = 0
      for(day_count in 1:1000){ # 100 is just a safe upper bound, but loop will break before that
        # get the max of 14 day window
        if(day_count < 14){
          max_det_in_one_day = max(x$New_Detections[1:day_count])
        }else{
          max_det_in_one_day = max(x$New_Detections[(day_count-13):day_count])
        } # end if else
        # Some simulations die out before a lever would be lifted, so action has no impact
        if(is.na(max_det_in_one_day) | day_count > length(x$New_Detections) ){
          if(day_count > length(x$New_Detections)){
            lift_day = length(x$New_Detections)
          }else{
            lift_day= day_count+1
          } # end if else
          break;
        }else{  # if max is replaced with larger non-zero number increase lever, otherwise decrease
          if(x$New_Detections[day_count] == max_det_in_one_day  & max_det_in_one_day != 0 ){
            lever_lift_count = lever_lift_count + 1
          }else{
            lever_lift_count = lever_lift_count - 1
          } # end if else if
          # when sufficiently low get day lever would have lifted and break for loop
          if(lever_lift_count <= -14){
            lift_day = day_count+1
            break;
          } # end if else
        } # end if else
      } # end for day_count 
    }else{ # for R0 increase greater than 0 the lever lift day does not need to be computed
      lift_day = which(diff(x$R0)!=0)+1       # get the day R0 increased in simulation
      if(length(lift_day)==0){ lift_day = length(x$New_Detections) } # Sim may end before R0 increased
    } # end if else
    
    det_after_lift=x$New_Detections[lift_day:(lift_day+days_after_lift)]
    det_after_lift[is.na(det_after_lift)]=0
    # initialize with increase_by=0
    first_less_than_rest=as.integer((det_after_lift[1])<det_after_lift)
    for(increase_by in 1:50){ # get the probs for increase_by from 1:50
      temp_first=as.integer((det_after_lift[1]+increase_by)<det_after_lift)
      first_less_than_rest = c(first_less_than_rest, temp_first)
    }
    all_val=c(first_less_than_rest)
    return(all_val)
  }) # end sapply
  
  # get the mean of all rows
  means=rowMeans(case_det_info_after_lift)
  # assign first chunk of means and other helpful metrics to initialize df
  df=data.frame(init_inf_vect, init_R0_vect, incr_R0_vect, final_R0_vect,
                days_after_lift_vect, means[1:total_days])
  names(df)=c("Init_Infected", "Init_R0", "Increase_by_R0", "R0_Final", 
              "Days_after_lift", "Det_less_than_lift")
  # append the rest of means to the data frame 
  for(i in 1:50){
    df[[paste0("Det_less_than_lift_",i)]] = means[(i*total_days+1):((i+1)*total_days)]
  }
  return(df)
} # end function get_mean_after_lift_stats_pt_compare




# Plot to compare probability R0>1 with an increase by 0 to 5 upon comparison
plot_prob_R0_above_1_increase_by = function(df, input_init_inf, pt_or_mean){
  # subset by the inital number infected
  df_sub           = subset(df, Init_Infected==input_init_inf)
  prob_df          = data.frame(group=double(), day_count=double(), prob=double())
  for(i in 0:5){ # 50 is hard coded here and in get_mean_after_lift_stats_*_compare functions
    # sum all rows in dataframe subset with the same day count
    grp_sum          = rowsum(df_sub[,i+7], df_sub$Days_after_lift)
    # subset by all R0 Final > 1, inputs should have been balance about 1
    sub_greater1     = subset(df_sub, R0_Final>1)
    # sum all rows in the sub-sub-dataframe to get numerator in prob vect
    grp_sum_greater1 = rowsum(sub_greater1[,i+7], sub_greater1$Days_after_lift)
    # get proportion of runs where new det on lift day < new det on subsequent days given R0>1
    prob_greater1    = grp_sum_greater1/grp_sum
    prob_greater1    = prob_greater1[-1,] # remove the first value of lift day itself as it was 0/0=Nan
    #prob_greater1[is.na(prob_greater1)]=0.0
    increase_by_vect = rep(i, length(prob_greater1)) # initial infected as a vector
    days_after_lift  = seq(1, length(prob_greater1), 1)
    temp_df          = data.frame(days_after_lift, increase_by_vect, prob_greater1)
    prob_df          = rbind(prob_df, temp_df)
  }
  prob_df$increase_by_vect=factor(prob_df$increase_by_vect) # grouping in ggplot needs to be factor
  
  if(pt_or_mean=="pt"){
    pt_plot=ggplot(prob_df, aes(x=days_after_lift, y=prob_greater1, group=increase_by_vect, color=increase_by_vect))+
      geom_point()+
      scale_colour_grey(name="New Det Greater by")+
      expand_limits(y = 0)+
      xlab("Number of Days After Lifting Lever")+
      ylab("Prob R0>1 | New Det > Lift Day New Det")+
      labs(title=paste0("Point Comparison, Sims initialized with ", input_init_inf, " infected"))+
      scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0.0, 1.1, 0.1))+
      theme_bw(base_size = 8)
    png(file=paste0("wrangler_output/prob_R0_greater1_through_time_pt_", input_init_inf, ".png"), width=4.25,height=3.25, units = "in", res=1200)
    print(pt_plot)
    dev.off()
  }else if(pt_or_mean=="mean"){
    mean_plot=ggplot(prob_df, aes(x=days_after_lift, y=prob_greater1, group=increase_by_vect, color=increase_by_vect))+
      geom_point()+
      expand_limits(y = 0)+
      scale_colour_grey(name="New Det Greater by")+
      xlab("Number of Days After Lifting Lever")+
      ylab("Prob R0>1 | Mean(New Det) > Lift Day New Det")+
      labs(title=paste0("Mean Comparison, Sims initialized with ", input_init_inf, " infected"))+
      scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0.0, 1.1, 0.1))+
      theme_bw(base_size = 8)
    png(file=paste0("wrangler_output/prob_R0_greater1_through_time_mean_", input_init_inf, ".png"), width=4.25,height=3.25, units = "in", res=1200)
    print(mean_plot)
    dev.off()
  }else{
    print("Invalid pt_or_mean input")
  }
  return(0)
  
}




### Plot probability R0>1 through time for each init num infected scenario
plot_prob_R0_above_1 = function(df, init_num_infected, pt_or_mean){
  prob_df = data.frame(group=double(), day_count=double(), prob=double())
  for(i in 1:length(init_num_infected)){
    # subset by the inital number infected
    df_sub           = subset(df, Init_Infected==init_num_infected[i])
    # sum all rows in dataframe subset with the same day count
    grp_sum          = rowsum(df_sub$Det_less_than_lift, df_sub$Days_after_lift)
    # subset by all R0 Final > 1, inputs should have been balance about 1
    sub_greater1     = subset(df_sub, R0_Final>1)
    # sum all rows in the sub-sub-dataframe to get numerator in prob vect
    grp_sum_greater1 = rowsum(sub_greater1$Det_less_than_lift, sub_greater1$Days_after_lift)
    # get proportion of runs where new det on lift day < new det on subsequent days given R0>1
    prob_greater1    = grp_sum_greater1/grp_sum
    prob_greater1    = prob_greater1[-1,] # remove the first value of lift day itself as it was 0/0=Nan
    init_inf_vect    = rep(init_num_infected[i], length(prob_greater1)) # initial infected as a vector
    days_after_lift  = seq(1, length(prob_greater1), 1)
    temp_df          = data.frame(days_after_lift, init_inf_vect, prob_greater1)
    prob_df = rbind(prob_df, temp_df)
  }
  prob_df$init_inf_vect=factor(prob_df$init_inf_vect) # grouping in ggplot needs to be factor
  if(pt_or_mean=="pt"){
    write.csv(prob_df, "point_compare_r0_above1.csv", row.names = FALSE)
    plot=ggplot(prob_df, aes(x=days_after_lift, y=prob_greater1, group=init_inf_vect, color=init_inf_vect))+
      geom_point()+
      scale_colour_grey(name="Initial Infected")+
      expand_limits(y = 0)+
      xlab("Number of Days After Lifting Lever")+
      ylab("Prob R0>1 | New Det > Lift Day New Det")+
      scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0.0, 1.1, 0.1))+
      theme_bw(base_size = 8)
    png(file="wrangler_output/prob_R0_greater1_through_time_pt.png", width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  }else if(pt_or_mean=="mean"){
    write.csv(prob_df, "mean_compare_r0_above1.csv", row.names = FALSE)
    plot=ggplot(prob_df, aes(x=days_after_lift, y=prob_greater1, group=init_inf_vect, color=init_inf_vect))+
      geom_point()+
      expand_limits(y = 0)+
      scale_colour_grey(name="Initial Infected")+
      xlab("Number of Days After Lifting Lever")+
      ylab("Prob R0>1 | Mean(New Det) > Lift Day New Det")+
      scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0.0, 1.1, 0.1))+
      theme_bw(base_size = 8)
    png(file="wrangler_output/prob_R0_greater1_through_time_mean.png", width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  }else{
    print("Invalid pt_or_mean input")
  }# end if else
  return(0)
}
