##############################################################
### Code for plotting on Wrangler
##############################################################
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
  }) # end sapply
  
  means=rowMeans(case_det_info_after_lift)
  df=data.frame(init_inf_vect, init_R0_vect, incr_R0_vect, final_R0_vect,
                days_after_lift_vect, means[1:(days_after_lift+1)], means[(days_after_lift+2):((days_after_lift*2)+2)])
  names(df)=c("Init_Infected", "Init_R0", "Increase_by_R0", "R0_Final", 
              "Days_after_lift", "Det_less_than_lift", "Diff_new_det_less_than_lift")
  return(df)
} # end function get_mean_after_lift_stats

### Plot probability R0>1 through time for each init num infected scenario
plot_prob_R0_above_1 = function(df, init_num_infected){
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
  plot=ggplot(prob_df, aes(x=days_after_lift, y=prob_greater1, group=init_inf_vect, color=init_inf_vect))+
    geom_point()+
    expand_limits(y = 0)+
    scale_colour_grey(name="Initial Infected")+
    xlab("Number of Days After Relaxing Transmission Reduction")+
    ylab("Prob New Det > Lift Day New Det | R0>1")+
    scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0.0, 1.1, 0.1))+
    theme_bw(base_size = 8)#+
    #theme(legend.position = c(0.8, 0.7))
  
  png(file="figures/prob_R0_greater1_through_time.png", width=4.25,height=3.25, units = "in", res=1200)
  print(plot)
  dev.off()
  return(0)
}
