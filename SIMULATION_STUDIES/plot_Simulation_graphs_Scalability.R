# This code creates images reported in the paper regarding Scalability results
## Extract results from the Simulation Study ----
dir_path_scenario<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_4/SCENARIOS/"
dir_path_estimate<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_4/ESTIMATE_RUNS/"
dir_path_parameters<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_4/SET_PARAMETERS/"

int.tot<-1
newlist<-NULL
n.scenari<-9
n.parameters<-8
for(int.scenario in 1:n.scenari){
  Scen<-readRDS(file = paste0(dir_path_scenario,
                              "Scenario_",
                              int.scenario,
                              ".RDS"
  ))
  for(int.parameters in 1:n.parameters){
    estimate_scenario<-readRDS(file = paste0(dir_path_estimate,
                          "Results_Scenario_",
                          int.scenario,"_",int.parameters,
                          ".RDS"
    ))
    data_parameters<-readRDS(file = paste0(dir_path_parameters,
                                           "SetParameters_",
                                           int.parameters,
                                           ".RDS"
    ))
    le<-length(estimate_scenario[[1]])
    for (i in 1:le){
      newlist[[int.tot]]<-data.frame(time=estimate_scenario[[1]][[i]]$elapsed_time,
                                     nclust_pixel=data_parameters$nclust.pt.start,
                                     nclust_prot=data_parameters$nclust.prot.start,
                                     upd_row=data_parameters$est.upd.row,
                                     upd_col=data_parameters$est.upd.col,
                                     N_pixel=Scen$n.pt,N_prot=Scen$n.prot)          
      int.tot<-int.tot+1
    }
  }
}
df_graph<-do.call(rbind,newlist)

df_graph$upd.col.row<-paste0(df_graph$upd_row,",",df_graph$upd_col)
df_graph$upd.col.row<-factor(df_graph$upd.col.row,levels = c("C,A","S,A"))

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


## Save graphs ----
df <- data_summary(df_graph, varname="time", 
                    groupnames=c("N_prot", "N_pixel", "nclust_pixel", "nclust_prot", "upd.col.row"))
df<-df[which(df$upd.col.row=="C,A"),]
# Convert dose to a factor variable
df$N_pixel<-df$N_pixel*4
df$N_prot<-df$N_prot*3
df$N_pixel=as.factor(df$N_pixel)
df$N_prot=as.factor(df$N_prot)

df<- df %>% mutate(nclust_pixel = factor(nclust_pixel),
                   nclust_prot= factor(nclust_prot))

levels(df$nclust_pixel) <- c("4" = TeX("$R = 4$"), 
                             "12" = TeX("$R = 12$"))
levels(df$nclust_prot) <- c("3" = TeX("$K = 3$"),
                                "9" = TeX("$K = 9$"))

p<- ggplot(df, aes(x=N_pixel, y=time, group=N_prot, color=N_prot)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=time-sd, ymax=time+sd), width=.2,)+
theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                   text=element_text(size=18),legend.position="bottom",
                   plot.title = element_text(hjust = 0.5))+
labs(x="Number of columns", y = "Seconds", title = "",color="Number of rows") + guides(fill=guide_legend(title="Number of proteins"))+
theme(panel.border = element_rect(color = "black", fill = NA, size = 0.3))+
  scale_color_manual(values=c('#999999','#E69F00','#333666'))+scale_y_log10()+
  facet_grid(nclust_pixel~ nclust_prot ,labeller = label_parsed) 

p
ggsave("SIMULATION_STUDIES/GRAPHS/Figure5Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure5Suppl.pdf",height = 20,width = 25,dpi = 600) 
