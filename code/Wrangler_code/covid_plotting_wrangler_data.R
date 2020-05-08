##############################################################
### Code for plotting on Wrangler
##############################################################
library(ggplot2)
library(gmodels) # needed for ci()
library(scales) # needed for muted() in ggplot
options(warn=-1) # suppress all the warnings from ci()



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
    png(file=paste0("wrangler_figures/prob_R0_greater1_through_time_pt_", input_init_inf, ".png"), width=4.25,height=3.25, units = "in", res=1200)
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
    png(file=paste0("wrangler_figures/prob_R0_greater1_through_time_mean_", input_init_inf, ".png"), width=4.25,height=3.25, units = "in", res=1200)
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
    png(file="wrangler_figures/prob_R0_greater1_through_time_pt.png", width=4.25,height=3.25, units = "in", res=1200)
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
    png(file="wrangler_figures/prob_R0_greater1_through_time_mean.png", width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  }else{
    print("Invalid pt_or_mean input")
  }# end if else
  return(0)
}
