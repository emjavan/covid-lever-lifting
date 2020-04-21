###############################
## Code for plotting
###############################
library(ggplot2)
library(gmodels) # needed for ci()
library(scales) # needed for muted() in ggplot
options(warn=-1) # suppress all the warnings from ci()

get_cum_inf_lift = function(data_path){
  load(data_path)
  print( data_path )
  path_split=unlist(str_split(data_path, "_"))
  initial_R0=as.double(path_split[3])
  inital_infected=as.double(path_split[4])
  increase_R0=as.double(path_split[5])
  # apply lift data functions to list of all sims
  cum_inf_lift_df = sapply(sims, function(x){ 
    #print("#########################################################################################")
    # Series of if-else statements to determine the appropriate lift day
    if(increase_R0 == 0){
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
            lift_day= day_count
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
            lift_day = day_count
            break;
          } # end if else
        } # end if else
      } # end for day_count 
    }else{ # for R0 increase greater than 0 the lever lift day does not need to be computed
      lift_day = which(diff(x$R0)!=0)       # get the day R0 increased in simulation
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
  full_row = c(inital_infected, initial_R0, increase_R0, as.vector(conf_inf))
  summary_stats = t( full_row ) 
  # return data frame for heat map
  return(summary_stats)
} # end function get_cum_inf_lift 


## Plots heat map based on mean Cum_inf_30d_after_lift
### sims end from reaching 10x as many infected as start or epidemic dies out
heat_map = function(df, init_num_infected){
  for(i in 1:length(init_num_infected)){
    df_sub=subset(df, Init_Infected==init_num_infected[i])
    plot=ggplot(df_sub, aes(x= factor(Init_R0), y=factor(R0_increase), fill= Mean_Cum_inf_30d_after_lift))+ 
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
      scale_y_discrete(breaks = unique(df$R0_increase))+
      theme_bw(base_size=7)+
      theme(legend.position = "none")
    png(file=paste0("figures/heatmap_init_infected_",init_num_infected[i], ".png"), width=4.25,height=3.25, units = "in", res=1200)
    print(plot)
    dev.off()
  } # end for
} # end function heat_map












### Gets all the rows to build data frame for heat map
get_mean_sim_end = function(data_path){
  load(data_path)
  path_split=unlist(str_split(data_path, "_"))
  initial_R0=as.double(path_split[3])
  increase_R0=as.double(path_split[5])
  inital_infected=as.double(path_split[4])
  mean_exit_day=round(mean(sapply(sims, function(x) max(x$Day_Count))), digits = 0)
  mean_inf=round(mean(sapply(sims, function(x) x$Total_Infections[max(x$Day_Count)])), digits=0)
  df = data.frame(inital_infected, initial_R0, increase_R0, mean_exit_day, mean_inf)
  return(df)
}




## Write csv of for infected and cum_infected when sim ends or re-bounds
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
}


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






# plot_county_summary_sensitivity <- function(df){
#   df %>% 
#     select(-frac_state_counties, - frac_state_population) %>% 
#     gather(key, value, frac_us_population, frac_us_counties) %>% 
#     mutate(key = ifelse(key == 'frac_us_population', "US Population", "US Counties")) %>% 
#     ggplot(aes(detection_probability, value, color = as.factor(r_not), group = r_not, shape=as.factor(r_not)))+
#     geom_line(size=1) + 
#     geom_point(size=2) +
#     scale_y_continuous(labels = scales::percent, limits = c(0,1))+
#     facet_wrap(~key) +
#     background_grid(major = 'xy')+
#     labs(color  = expression(R[0]), shape=expression(R[0]), linetype=expression(R[0]))+
#     xlab("Case Detection Probability")+
#     ylab("Percent")+
#     scale_color_manual(values=c("#999999", "grey39", "#000000"))+
#     theme_bw(base_size = 10)
# }
# 
# make_case_risk_plot=function(r_not_vect, det_prob){
#   ### Open files with epi_prob data for all R0 run and put in one data frame
#   full_df=data.frame(R0=double(), cases_detected=double(), epi_prob=double(), scam_epi_prob=double())
#   for(val in 1:length(r_not_vect)){
#     temp_df = read.csv( paste0("processed_data/epi_prob_data_", r_not_vect[val],"_", det_prob, "_0_1e+05.csv"), header = TRUE)
#     R0=rep(r_not_vect[val], length(temp_df$detected))
#     temp_df = cbind(R0, temp_df)
#     full_df = rbind(full_df, temp_df)
#   }
#   names(full_df) = c("R0", "cases_detected", "epi_prob", "scam_epi_prob")
#   full_df$R0 = factor(full_df$R0)
#   
#   full_df=subset(full_df, cases_detected<=50)
#   
#   ### Plot cases detected by epidemic risk
#   case_risk_plot=ggplot(full_df, aes(x=cases_detected, y=epi_prob, group=R0, color=R0, shape=R0))+ #
#   #case_risk_plot=ggplot(full_df, aes(x=cases_detected, y=scam_epi_prob, group=R0, color=R0, shape=R0))+
#     geom_line()+
#     geom_point()+
#     scale_colour_grey()+
#     expand_limits(y = 0)+
#     xlab("Cumulative Cases Reported")+
#     ylab("Epidemic Risk")+
#     labs(color="R0", shape="R0")+
#     theme_bw(base_size = 8)+
#     theme(panel.grid.minor = element_line(colour="white", size=0.1)) +
#     scale_x_continuous(minor_breaks = seq(0 , 50, 1), breaks = seq(0, 50, 5))+
#     scale_y_continuous(minor_breaks = seq(0.0 , 1.1, 0.1), breaks = seq(0, 1.1, 0.1))
# 
#   png(file="figures/case_risk_plot.png",
#       width=4.25,height=3.25, units = "in", res=1200)
#   plot(case_risk_plot)
#   dev.off()
# }







