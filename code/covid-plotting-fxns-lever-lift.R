###############################
## Code for plotting
###############################
library(ggplot2)
library(gmodels) # needed for ci()
library(scales) # needed for muted() in ggplot
options(warn=-1) # suppress all the warnings from ci()


### Get the the mean difference in new detections after lift day and percent of days with new detections greater than lift day
# days after lift is hard coded but should maybe be an input
# Final R0>1 doesn't have error checking coded for if days after lift is beyond the last day of detections
get_mean_after_lift_stats = function(data_path){
  load(data_path)
  print(data_path)
  path_split           = unlist(str_split(data_path, "_"))
  initial_R0           = as.double(path_split[3])
  init_inf             = as.double(path_split[4])
  increase_R0          = as.double(path_split[5])
  R0_final             = initial_R0+increase_R0
  days_after_lift      = 30
  days_after_lift_vect = seq(0, days_after_lift, 1)
  init_R0_vect         = rep(initial_R0,  (days_after_lift+1))
  incr_R0_vect         = rep(increase_R0, (days_after_lift+1))
  final_R0_vect        = rep(R0_final,    (days_after_lift+1))
  init_inf_vect        = rep(init_inf,    (days_after_lift+1))
  
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
    first_less_than_rest=as.integer(det_after_lift[1]<det_after_lift)
    diff=det_after_lift-det_after_lift[1]
    all_val=c(first_less_than_rest, diff)
    return(all_val)
  })
  
  means=rowMeans(case_det_info_after_lift)
  df=data.frame(init_inf_vect, init_R0_vect, incr_R0_vect, final_R0_vect,
                days_after_lift_vect, means[1:(days_after_lift+1)], means[(days_after_lift+2):((days_after_lift*2)+2)])
  names(df)=c("Init_Infected", "Init_R0", "Increase_by_R0", "R0_Final", 
              "Days_after_lift", "Det_less_than_lift", "Diff_new_det_less_than_lift")
  return(df)
} # end function get_mean_after_lift_stats


### Heat map of the percent of simulations that had new detections greater than the new detections on the lift day
heat_map_det_after_lift = function(df, init_num_infected){
  for(i in 1:length(init_num_infected)){
    df_sub=subset(df, Init_Infected==init_num_infected[i])
    plot=ggplot(df_sub, aes(x= factor(R0_Final), y=factor(Days_after_lift), fill=Det_less_than_lift))+ 
      geom_tile()+
      scale_fill_gradient(low="gray100", high="gray72")+
      geom_text(size=2, aes(label=round(Det_less_than_lift*100, digits=1) ))+
      ggtitle(paste0("Percent Sims with New Det < Lift Day New Det, Init with ", init_num_infected[i], " Infected")) +
      xlab("R0") +
      ylab("Days after lifting lever")+
      scale_x_discrete(breaks = unique(df$R0_Final))+
      scale_y_discrete(breaks = unique(df$Days_after_lift))+
      theme_bw(base_size=7)+
      theme(legend.position = "none")
    png(file=paste0("figures/heatmap_det_init_infected_",init_num_infected[i], ".png"), width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  } # end for
} # end function heat_map_det_after_lift

### Heat map of the mean difference between the new detections on lift day and the subsequent 30 days
heat_map_diff_det_after_lift = function(df, init_num_infected){
  for(i in 1:length(init_num_infected)){
    df_sub=subset(df, Init_Infected==init_num_infected[i])
    plot=ggplot(df_sub, aes(x= factor(R0_Final), y=factor(Days_after_lift), fill=Diff_new_det_less_than_lift))+ 
      geom_tile()+
      scale_fill_gradient(low="gray100", high="gray72")+
      geom_text(size=2, aes(label=round(Diff_new_det_less_than_lift, digits=0) ))+
      ggtitle(paste0("Mean Diff New Det from Lift Day to following days, Init with ", init_num_infected[i], " Infected")) +
      xlab("R0") +
      ylab("Days after lifting lever")+
      scale_x_discrete(breaks = unique(df$R0_Final))+
      scale_y_discrete(breaks = unique(df$Days_after_lift))+
      theme_bw(base_size=7)+
      theme(legend.position = "none")
    png(file=paste0("figures/heatmap_diff_det_init_infected_",init_num_infected[i], ".png"), width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  } # end for
} # end function heat_map_diff_det_after_lift


### Get mean and CI for daily infections and detections for each sim scenarios with R0 less than 1
get_total_inf_R0_less_1 = function(data_path){
  load(data_path)
  path_split    = unlist(str_split(data_path, "_"))
  initial_R0    = as.double(path_split[3])
  increase_R0   = as.double(path_split[5])
  R0_final      = initial_R0 + increase_R0
  init_inf      = as.double(path_split[4])
  # get the max number of days in a simulation
  max_num_days  = max(sapply(sims, function(x) length(x$Total_Infections) ))
  days          = seq(1, max_num_days, 1)
  init_R0_vect  = rep(initial_R0, max_num_days)
  incr_R0_vect  = rep(increase_R0, max_num_days)
  final_R0_vect = rep(R0_final, max_num_days)
  init_inf_vect = rep(init_inf, max_num_days)
  # put final R0 on end of all sims that end before the longest sim
  total_R0_mat = sapply(sims, function(x){
    vect_len=length(x$R0)
    return(c(x$R0, rep(R0_final, (max_num_days - vect_len))))
  })
  R0 = apply(total_R0_mat, 1, function(x) mean(x) )
  
  # put zeros on end of all sims that end before the longest sim
  total_inf_mat = sapply(sims, function(x){
    vect_len=length(x$Total_Infections)
    return(c(x$Total_Infections, rep(0, (max_num_days - vect_len))))
  })
  summary_stats_inf = apply(total_inf_mat, 1, function(x) ci(x) )
  total_inf_df = as.data.frame(t(summary_stats_inf))
  
  # put zeros on end of all sims that end before the longest sim
  total_det_mat = sapply(sims, function(x){
    vect_len=length(x$New_Detections)
    return(c(x$New_Detections, rep(0, (max_num_days - vect_len))))
  })
  summary_stats_det = apply(total_det_mat, 1, function(x) ci(x) )
  total_det_df = as.data.frame(t(summary_stats_det))
  
  full_df = cbind(init_inf_vect, init_R0_vect, incr_R0_vect, final_R0_vect, 
                  days, R0, total_inf_df, total_det_df)
  names(full_df) = c("Init_Infected", "Init_R0", "Increase_by_R0", "R0_Final", "Days", "R0_by_day", 
                     "Inf_Mean", "Inf_LwrCI", "Inf_UprCI", "Inf_StdErr", 
                     "Det_Mean", "Det_LwrCI", "Det_UprCI", "Det_StdErr")
  return(full_df)
} # end function get_total_inf_R0_less_1



### Plot mean infected and detected cases through time, can easily be adapted for other metrics
plot_through_time = function(df, R0, init_infected, increase){
  sub_df     = df[df$Init_R0==R0 & df$Init_Infected==init_infected,] # subset df to get correct info
  increase   = rev(increase) # reverse order of labels
  label_vect = sapply(increase, function(x) paste0(R0, "+", x) ) # make labels for legend
  
  # mean infected through time plot
  inf_plot=sub_df %>% # mutate is to change plotting order so longest vector doesn't cover shortest
    mutate(R0_Final = forcats::fct_reorder(factor(R0_Final), desc(R0_Final))) %>%
    ggplot( aes(x=Days, y=Inf_Mean)) +
    scale_colour_grey(name="R0+Increase", labels = label_vect)+
    geom_line(aes(color=R0_Final), size=1)+
    ylab("Mean Ciruclating Infected")+
    labs(title=paste0("Sims initialized with ", init_infected, " infected"))+
    theme_bw(base_size = 8)+
    theme(legend.position = c(0.8, 0.7))
  png(file=paste0("figures/",init_infected, "_circ_inf_through_time.png"),
      width=4.25,height=3.25, units = "in", res=1200)
  print(inf_plot)
  dev.off()
  
  # mean detected through time plot
  mutate_df=sub_df %>% # mutate is to change plotting order so longest vector doesn't cover shortest
    mutate(R0_Final = forcats::fct_reorder(factor(R0_Final), desc(R0_Final)))
  det_plot=ggplot(mutate_df %>% 
                    arrange(R0_Final), # need to use arrange to change point order
                  aes(x=Days, y=Det_Mean, color=R0_Final)) +
    scale_colour_grey(name="R0+Increase", labels = label_vect)+
    geom_point(size=0.5)+
    ylab("Mean Detected Daily")+
    labs(title=paste0("Sims initialized with ", init_infected, " infected"))+
    theme_bw(base_size = 8)+
    theme(legend.position = c(0.8, 0.7))
  png(file=paste0("figures/",init_infected, "_daily_det_through_time.png"),
      width=4.25,height=3.25, units = "in", res=1200)
  print(det_plot)
  dev.off()

  fin_R0_vect  = unique(sub_df$R0_Final)
  inf_end_day  = sapply(fin_R0_vect, function(x){
    sub_sub_df = sub_df[sub_df$R0_Final==x,]
    max_day    = max(sub_sub_df$Days)
    return(max_day)
  })
  return(inf_end_day)
} # end plot_through_time



### Get the cumulative infected and peak to lift metrics for heat map
get_cum_inf_lift = function(data_path){
  load(data_path)
  print( data_path )
  path_split      = unlist(str_split(data_path, "_"))
  initial_R0      = as.double(path_split[3])
  inital_infected = as.double(path_split[4])
  increase_R0     = as.double(path_split[5])
  R0_final        = initial_R0 + increase_R0
  # apply lift data functions to list of all sims
  cum_inf_lift_df = sapply(sims, function(x){ 
    # Series of if-else statements to determine the appropriate lift day
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
    
    max_inf_before_lift    = max(x$Total_Infections[1:lift_day])
    # ensure peak day returns one value and largest
    peak_day               = unlist(max(which(x$Total_Infections[1:lift_day]==max_inf_before_lift)))
    day_peak_to_lift       = lift_day - peak_day
    cum_inf_lift_day       = x$Cumulative_Infections[lift_day]
    cum_inf_7d_after_lift  = x$Cumulative_Infections[lift_day+7]
    if(is.na(cum_inf_7d_after_lift)){cum_inf_7d_after_lift = max(x$Cumulative_Infections)}
    cum_inf_14d_after_lift = x$Cumulative_Infections[lift_day+14]
    if(is.na(cum_inf_14d_after_lift)){cum_inf_14d_after_lift = max(x$Cumulative_Infections)}
    cum_inf_30d_after_lift = x$Cumulative_Infections[lift_day+30]
    if(is.na(cum_inf_30d_after_lift)){cum_inf_30d_after_lift = max(x$Cumulative_Infections)}
    return(c(day_peak_to_lift, lift_day, cum_inf_lift_day, 
             cum_inf_7d_after_lift, cum_inf_14d_after_lift, cum_inf_30d_after_lift))
  }) # end sapply
  cum_inf_lift_df=as.data.frame(t(cum_inf_lift_df))
  names(cum_inf_lift_df) = c("Days_from_peak_to_lift", "Lift_day", "Cum_inf_lift_day", 
                             "Cum_inf_7d_after_lift", "Cum_inf_14d_after_lift", "Cum_inf_30d_after_lift")
  # write output from all sims to a csv
  csv_path=str_replace(string = data_path, pattern = "rda", replacement = "csv")
  cum_inf_lift_path=str_replace(string = csv_path, pattern = "sims", replacement = "cum_inf_lift_data")
  write.csv(cum_inf_lift_df, cum_inf_lift_path, row.names = FALSE)
  #Get summary stats from all sims and return
  conf_inf=apply(cum_inf_lift_df, 2, function(x) ci(x))
  # as.vector will go through column wise and append to one vector
  full_row = c(inital_infected, initial_R0, increase_R0, R0_final, as.vector(conf_inf))
  summary_stats = t( full_row ) 
  summary_stats = as.data.frame(summary_stats)
  names(summary_stats) = 
    c("Init_Infected",                     "Init_R0",                           "Increase_by_R0",                     
      "R0_Final",
      "Mean_Days_from_peak_to_lift",       "CI_lower_Days_from_peak_to_lift",   "CI_upper_Days_from_peak_to_lift",  
      "StdError_Days_from_peak_to_lift",   "Mean_Lift_day",                     "CI_lower_Lift_day",               
      "CI_upper_Lift_day",                 "StdError_Lift_day",                 "Mean_Cum_inf_lift_day",        
      "CI_lower_Cum_inf_lift_day",         "CI_upper_Cum_inf_lift_day",         "StdError_Cum_inf_lift_day",     
      "Mean_Cum_inf_7d_after_lift",        "CI_lower_Cum_inf_7d_after_lift",    "CI_upper_Cum_inf_7d_after_lift",   
      "StdError_Cum_inf_7d_after_lift",    "Mean_Cum_inf_14d_after_lift",       "CI_lower_Cum_inf_14d_after_lift",  
      "CI_upper_Cum_inf_14d_after_lift",   "StdError_Cum_inf_14d_after_lift",   "Mean_Cum_inf_30d_after_lift",  
      "CI_lower_Cum_inf_30d_after_lift",   "CI_upper_Cum_inf_30d_after_lift",   "StdError_Cum_inf_30d_after_lift")
  
  # return data frame for heat map
  return(summary_stats)
} # end function get_cum_inf_lift 


### Plots heat map based on mean Cum_inf_30d_after_lift
### sims end from reaching 10x as many infected as start or epidemic dies out
heat_map_Cum_inf_30d_after_lift = function(df, init_num_infected){
  for(i in 1:length(init_num_infected)){
    df_sub=subset(df, Init_Infected==init_num_infected[i])
    plot=ggplot(df_sub, aes(x= factor(Init_R0), y=factor(Increase_by_R0), fill= Mean_Cum_inf_30d_after_lift))+ 
      geom_tile()+
      scale_fill_gradient(low="gray100", high="gray72")+
      geom_text(size=2, aes(label=paste0("CumInf at Lift ", round(Mean_Cum_inf_lift_day, digits=0),
                                 "\nCumInf after 30d ", round(Mean_Cum_inf_30d_after_lift, digits=0), 
                                 "\nLift after Peak ", round(Mean_Days_from_peak_to_lift, digits=0),
                                 "\nLift Day ", round(Mean_Lift_day, digits=0))))+
      ggtitle(paste0("Sim Initialized with Infected=", init_num_infected[i])) +
      xlab("Initial R0") +
      ylab("R0 Increased By")+
      scale_x_discrete(breaks = unique(df$Init_R0))+
      scale_y_discrete(breaks = unique(df$Increase_by_R0))+
      theme_bw(base_size=7)+
      theme(legend.position = "none")
    png(file=paste0("figures/heatmap_init_infected_",init_num_infected[i], ".png"), width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  } # end for
} # end function heat_map_Cum_inf_30d_after_lift

### Gets all the rows to build data frame for heat map
get_mean_sim_end = function(data_path){
  load(data_path)
  path_split      = unlist(str_split(data_path, "_"))
  initial_R0      = as.double(path_split[3])
  increase_R0     = as.double(path_split[5])
  inital_infected = as.double(path_split[4])
  mean_exit_day   = round(mean(sapply(sims, function(x) max(x$Day_Count))), digits = 0)
  mean_inf        = round(mean(sapply(sims, function(x) x$Total_Infections[max(x$Day_Count)])), digits=0)
  df              = data.frame(inital_infected, initial_R0, increase_R0, mean_exit_day, mean_inf)
  return(df)
} # end function get_mean_sim_end



### Write csv of for infected and cum_infected when sim ends or re-bounds
get_exit_day_data = function(data_path){ 
  load(data_path)
  rebound_day_inf= sapply(sims, function(x){ 
    end_day = max(x$Day_Count)
    end_day_infected = x$Total_Infections[end_day]
    if(end_day_infected == 0){
      rebound_day=NA
      eradication_day = end_day
      num_infected = end_day_infected
      cum_infected = x$Cumulative_Infections[end_day]
    }else{
      eradication_day = NA
      day_r0_increased = which(diff(x$R0)!=0)
      max_inf_before_increase = max(x$Total_Infections[1:day_r0_increased])
      rebound_day = which.max(x$Total_Infections>max_inf_before_increase)
      num_infected = x$Total_Infections[rebound_day]
      cum_infected = x$Cumulative_Infections[rebound_day]
    }
    return(c(rebound_day, eradication_day, num_infected, cum_infected)) 
  })
  rebound_day_inf = data.frame(t(rebound_day_inf))
  names(rebound_day_inf) = c("Rebound_day", "Eradication_day", "Num_Infected", "Cum_Infected")
  csv_path=str_replace(string = data_path, pattern = "rda", replacement = "csv")
  rebound_day_inf_path=str_replace(string = csv_path, pattern = "sims", replacement = "exit_day_data")
  write.csv(rebound_day_inf, rebound_day_inf_path, row.names = FALSE)
  mean_inf=round(mean(rebound_day_inf$Num_Infected), digits = 0)
  return(mean_inf)
} # end function get_exit_day_data


####### This function is only valid when all sims end on same day, otherwise df have meaningless values
# ## Write csv of infected by last day sim ends
# get_total_inf_data = function(data_path){
#   print(data_path)
#   load(data_path)
#   path_split=unlist(str_split(data_path, "_"))
#   inital_infected=strtoi(path_split[4])
# 
#   # sims are rows and days are columns
#   total_inf_mat=do.call('rbind', lapply(sims, '[[', 4)) # 4th column of dataframe is Total_Infections 
#   conf_inf=apply(total_inf_mat, 2, function(x) ci(x))
#   days = ncol(total_inf_mat)
# 
#   r0_mat=do.call('rbind', lapply(sims, '[[', 13))  #13th column is R0
#   mean_r0=apply(r0_mat, 2, function(x) mean(x))
#   day_count=seq(1, days)
#   init_inf=rep(inital_infected, days)
#   
#   total_inf_df = data.frame(day_count, init_inf, mean_r0,
#     conf_inf[1,], conf_inf[2,], conf_inf[3,], conf_inf[4,])
#   names(total_inf_df)= c("Days", "Init_Inf", "Mean_R0", "Mean_Inf", "CI_Lwr", "CI_Upr", "Std_Er")
#   csv_path=str_replace(string = data_path, pattern = "rda", replacement = "csv")
#   total_inf_path=str_replace(string = csv_path, pattern = "sims", replacement = "total_inf_data")
#   write.csv(total_inf_df, total_inf_path, row.names = FALSE)
#   return("Finished")
# }