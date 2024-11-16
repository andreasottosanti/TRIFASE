# This code creates the images reported in the paper regarding TRIFASE clustering performance
## Extract results from the Simulation Study ----
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Simulation_Study/EXTRACT_RESULTS/"
n.scenari<-2
n.parameters<-200
int.tot<-1
newlist<-NULL
for(int.scenario in 1:n.scenari){
  for(int.parameters in 1:n.parameters){
    estimate_scenario<-readRDS(file = paste0(dir_path_extracted,
                                             "Extract_Results_Scenario_",
                                             int.scenario,"_",int.parameters,
                                             ".RDS"
    ))
    le<-length(estimate_scenario)
    for (i in 1:le){
      newlist[[int.tot]]<-data.frame(estimate_scenario[[i]][c(1:8,13:41)])
      int.tot<-int.tot+1
    }
  }
}
df_graph2<-do.call(rbind,newlist)


## Extract results from the Additional Simulation Study 1 ----
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_1/EXTRACT_RESULTS/"
n.scenari<-2
n.parameters<-100
int.tot<-1
newlist<-NULL
for(int.scenario in 1:n.scenari){
  for(int.parameters in 1:n.parameters){
    estimate_scenario<-readRDS(file = paste0(dir_path_extracted,
                                             "Extract_Results_Scenario_",
                                             int.scenario,"_",int.parameters,
                                             ".RDS"
    ))
    le<-length(estimate_scenario)
    for (i in 1:le){
      newlist[[int.tot]]<-data.frame(estimate_scenario[[i]][c(1:8,13:41)])
      int.tot<-int.tot+1
    }
  }
}
df_graph1<-do.call(rbind,newlist)


## Extract results from the Additional Simulation Study 2 ----
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_2/EXTRACT_RESULTS/"
n.scenari<-2
n.parameters<-150
int.tot<-1
newlist<-NULL
for(int.scenario in 1:n.scenari){
  for(int.parameters in 1:n.parameters){
    estimate_scenario<-readRDS(file = paste0(dir_path_extracted,
                                             "Extract_Results_Scenario_",
                                             int.scenario,"_",int.parameters,
                                             ".RDS"
    ))
    le<-length(estimate_scenario)
    for (i in 1:le){
      newlist[[int.tot]]<-data.frame(estimate_scenario[[i]][c(1:8,13:41)])
      int.tot<-int.tot+1
    }
  }
}
df_graph3<-do.call(rbind,newlist)
df_graph_tmp<-rbind(df_graph1,df_graph2)
df_graph<-rbind(df_graph_tmp,df_graph3)


## Extract results from the Additional Simulation Study 3 ----
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/EXTRACT_RESULTS/"
n.scenari<-2
n.parameters<-162
int.tot<-1
newlist<-NULL
for(int.scenario in 1:n.scenari){
  for(int.parameters in 1:n.parameters){
    estimate_scenario<-readRDS(file = paste0(dir_path_extracted,
                                             "Extract_Results_Scenario_",
                                             int.scenario,"_",int.parameters,
                                             ".RDS"
    ))
    le<-length(estimate_scenario)
    for (i in 1:le){
      newlist[[int.tot]]<-data.frame(estimate_scenario[[i]][c(1:8,13:41)])
      int.tot<-int.tot+1
    }
  }
}
df_graph_n1000<-do.call(rbind,newlist)


## Create Graphs (p=100) ----
df_graph$upd.col.row<-paste0(df_graph$est.upd.row,",",df_graph$est.upd.col)
df_graph$upd.col.row[which(df_graph$est.kernel=="km")]<-"Km"
df_graph$upd.col.row[which(df_graph$est.kernel=="none")]<-"W"
df_graph$upd.col.row<-factor(df_graph$upd.col.row,levels = c("W", "Km","C,A","C,C","C,S","S,A","S,C","S,S"))
df_graph$res.clust.row<-1-df_graph$res.clust.row
df_graph$res.clust.col<-1-df_graph$res.clust.col


### Row and Column clustering Performance ----
#### phi=10 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                                nclust.prot.start = factor(nclust.prot.start),
                      phi.start = factor(phi.start),
                      tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                      "8" = TeX("$R = 8$"), 
                                      "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                        "6" = TeX("$K = 6$"), 
                                        "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.row)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Row clustering performance (p=100)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Row_n100_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.col)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Column clustering performance (p=100)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Col_n100_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=0.1----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.row)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Row clustering performance (p=100)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Row_n100_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.col)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Column clustering performance (p=100)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Col_n100_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=20 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.row)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Row clustering performance (p=100)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Row_n100_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.col)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Column clustering performance (p=100)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Col_n100_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 


### Computational Time ----
#### phi=10 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.elapsed_time))) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + 
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "Seconds", title = "Computational time (p=100)") + guides(fill=guide_legend(title=""))+ylim(0,20)
p2_Computational_Time_n100_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed)

#### phi=0.1 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.elapsed_time))) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + 
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "Seconds", title = "Computational time (p=100)") + guides(fill=guide_legend(title=""))+ylim(0,20)
p2_Computational_Time_n100_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed)

#### phi=20 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.elapsed_time))) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + 
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "Seconds", title = "Computational time (p=100)") + guides(fill=guide_legend(title=""))+ylim(0,20)
p2_Computational_Time_n100_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed)


### Estimated non-empty Row and Column cluster ----
#### Unique Cluster Row ----
##### phi=0.1 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.row.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty row clusters (p=100)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Row_n100_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=10 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.row.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty row clusters (p=100)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Row_n100_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=20 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.row.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty row clusters (p=100)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Row_n100_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

#### Unique Cluster Col ----
##### phi=0.1 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.col.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=4.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty column clusters (p=100)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Col_n100_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=10 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.col.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=4.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty column clusters (p=100)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Col_n100_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=20 ----
df_rid<-df_graph[which((df_graph$est.kernel!="exponential" & df_graph$spat==TRUE) |
                         (df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.col.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=4.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty column clusters (p=100)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Col_n100_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))


### Estimate Tau ----
#### phi=0.1 ----
df_rid<-df_graph[which((df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.tau))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = TeX("$\\tau$"), title = TeX("Estimated $\\tau$ (p=100)")) + guides(fill=guide_legend(title=""))
p2_Estimate_Tau_n100_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=10 ----
df_rid<-df_graph[which((df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.tau))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = TeX("$\\tau$"), title = TeX("Estimated $\\tau$ (p=100)")) + guides(fill=guide_legend(title=""))
p2_Estimate_Tau_n100_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=20 ----
df_rid<-df_graph[which((df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.tau))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = TeX("$\\tau$"), title = TeX("Estimated $\\tau$ (p=100)")) + guides(fill=guide_legend(title=""))
p2_Estimate_Tau_n100_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 


## Create Graphs (p=1000) ----
df_graph_n1000$upd.col.row<-paste0(df_graph_n1000$est.upd.row,",",df_graph_n1000$est.upd.col)
df_graph_n1000$upd.col.row[which(df_graph_n1000$est.kernel=="km")]<-"Km"
df_graph_n1000$upd.col.row[which(df_graph_n1000$est.kernel=="none")]<-"W"
df_graph_n1000$upd.col.row<-factor(df_graph_n1000$upd.col.row,levels = c("W", "Km","C,A","S,A"))
df_graph_n1000$res.clust.row<-1-df_graph_n1000$res.clust.row
df_graph_n1000$res.clust.col<-1-df_graph_n1000$res.clust.col


### Row and Column clustering Performance ----
#### phi=10 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.row)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Row clustering performance (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Row_n1000_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 


p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.col)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Column clustering performance (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Col_n1000_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=0.1 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.row)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Row clustering performance (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Row_n1000_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.col)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Column clustering performance (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Col_n1000_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=20 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.row)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Row clustering performance (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Row_n1000_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=res.clust.col)) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + geom_hline(yintercept=1.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "1-CER", title = "Column clustering performance  (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0.0,1)
p2_Cluster_Col_n1000_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 


### Computational Time ----
#### phi=10 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.elapsed_time))) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + 
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "Seconds", title = "Computational time (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0,20)
p2_Computational_Time_n1000_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=0.1 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.elapsed_time))) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + 
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "Seconds", title = "Computational time (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0,20)
p2_Computational_Time_n1000_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=20 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.elapsed_time))) + geom_boxplot(alpha=.8)+
  stat_summary(geom = "point",fun = mean,position =
                 position_dodge2(width = 0.75,preserve = "single"),color="black", size=2,shape=17)
p <- p + 
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "Seconds", title = "Computational time (p=1000)") + guides(fill=guide_legend(title=""))+ylim(0,20)
p2_Computational_Time_n1000_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 


### Estimated non-empty Row and Column cluster ----
#### Unique Cluster Row ----
##### phi=0.1 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.row.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty row clusters (p=1000)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Row_n1000_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=10 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.row.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty row clusters (p=1000)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Row_n1000_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=20 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.row.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty row clusters (p=1000)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Row_n1000_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

#### Unique Cluster Col ----
##### phi=0.1 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.col.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=4.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty column clusters (p=1000)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Col_n1000_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=10 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.col.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=4.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty column clusters (p=1000)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Col_n1000_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))

##### phi=20 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel!="exponential" & df_graph_n1000$spat==TRUE) |
                         (df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.clust.col.unique))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=4.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = "# of clusters", title = "Estimated non-empty column clusters (p=1000)") + guides(fill=guide_legend(title=""))
p2_UniqueCluster_Col_n1000_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) + scale_y_continuous(breaks = ~round(unique(pretty(.))))


### Estimate Tau ----
#### phi=0.1 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==0.1)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.tau))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = TeX("$\\tau$"), title = TeX("Estimated $\\tau$ (p=1000)")) + guides(fill=guide_legend(title=""))
p2_Estimate_Tau_n1000_phi01 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=10 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==10)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.tau))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = TeX("$\\tau$"), title = TeX("Estimated $\\tau$ (p=1000)")) + guides(fill=guide_legend(title=""))
p2_Estimate_Tau_n1000_phi10 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 

#### phi=20 ----
df_rid<-df_graph_n1000[which((df_graph_n1000$est.kernel=="exponential" & df_graph_n1000$spat==TRUE & df_graph_n1000$phi.start==20)),]

df_rid<- df_rid %>% mutate(nclust.pt.start = factor(nclust.pt.start),
                           nclust.prot.start = factor(nclust.prot.start),
                           phi.start = factor(phi.start),
                           tau.start = factor(tau.start))

levels(df_rid$nclust.pt.start) <- c("4" = TeX("$R = 4$"), 
                                    "8" = TeX("$R = 8$"), 
                                    "12" = TeX("$R = 12$"))
levels(df_rid$nclust.prot.start) <- c("3" = TeX("$K = 3$"), 
                                      "6" = TeX("$K = 6$"), 
                                      "9" = TeX("$K = 9$"))

p <- ggplot(data = df_rid, aes(x=upd.col.row,y=as.numeric(res.tau))) + geom_boxplot(alpha=.8)
p <- p + geom_hline(yintercept=3.0, linetype="dashed", 
                    color = "red", size=1)+
  theme_bw() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
                     text=element_text(size=18),legend.position="none",
                     plot.title = element_text(hjust = 0.5))
p <- p + labs(x="Model", y = TeX("$\\tau$"), title = TeX("Estimated $\\tau$ (p=1000)")) + guides(fill=guide_legend(title=""))
p2_Estimate_Tau_n1000_phi20 <- p + facet_grid(nclust.pt.start ~ nclust.prot.start,labeller = label_parsed) 


## Save graphs ----
# Estimate Tau
(p2_Estimate_Tau_n100_phi01+p2_Estimate_Tau_n100_phi10+p2_Estimate_Tau_n100_phi20)/(p2_Estimate_Tau_n1000_phi01+p2_Estimate_Tau_n1000_phi10+p2_Estimate_Tau_n1000_phi20)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure9Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure9Suppl.pdf",height = 20,width = 25,dpi = 600) 

# Unique Cluster
p2_UniqueCluster_Col_n100_phi10+p2_UniqueCluster_Col_n1000_phi10
ggsave("SIMULATION_STUDIES/GRAPHS/Figure4.eps",height = 10,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure4.pdf",height = 10,width = 25,dpi = 600) 

p2_UniqueCluster_Row_n100_phi10+p2_UniqueCluster_Row_n1000_phi10
ggsave("SIMULATION_STUDIES/GRAPHS/Figure6Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure6Suppl.pdf",height = 20,width = 25,dpi = 600) 

(p2_UniqueCluster_Row_n100_phi01+p2_UniqueCluster_Row_n1000_phi01)/(p2_UniqueCluster_Col_n100_phi01+p2_UniqueCluster_Col_n1000_phi01)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure7Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure7Suppl.pdf",height = 20,width = 25,dpi = 600) 

(p2_UniqueCluster_Row_n100_phi20+p2_UniqueCluster_Row_n1000_phi20)/(p2_UniqueCluster_Col_n100_phi20+p2_UniqueCluster_Col_n1000_phi20)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure8Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure8Suppl.pdf",height = 20,width = 25,dpi = 600) 

# Computational_Time
p2_Computational_Time_n100_phi10+p2_Computational_Time_n1000_phi10
ggsave("SIMULATION_STUDIES/GRAPHS/Figure3.eps",height = 10,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure3.pdf",height = 10,width = 25,dpi = 600) 

(p2_Computational_Time_n100_phi01+p2_Computational_Time_n1000_phi01)/(p2_Computational_Time_n100_phi20+p2_Computational_Time_n1000_phi20)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure4Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure4Suppl.pdf",height = 20,width = 25,dpi = 600) 

# Cluster_Row & Col
(p2_Cluster_Row_n100_phi10+p2_Cluster_Col_n100_phi10)/(p2_Cluster_Row_n1000_phi10+p2_Cluster_Col_n1000_phi10)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure2.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure2.pdf",height = 20,width = 25,dpi = 600) 

(p2_Cluster_Row_n100_phi01+p2_Cluster_Col_n100_phi01)/(p2_Cluster_Row_n1000_phi01+p2_Cluster_Col_n1000_phi01)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure2Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure2Suppl.pdf",height = 20,width = 25,dpi = 600) 

(p2_Cluster_Row_n100_phi20+p2_Cluster_Col_n100_phi20)/(p2_Cluster_Row_n1000_phi20+p2_Cluster_Col_n1000_phi20)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure3Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure3Suppl.pdf",height = 20,width = 25,dpi = 600) 



summary(df_graph$res.mean.pt)
summary(df_graph$res.min.pt)
summary(df_graph$res.max.pt)
summary(df_graph$res.sd.pt)

summary(df_graph_n1000$res.mean.pt)
summary(df_graph_n1000$res.min.pt)
summary(df_graph_n1000$res.max.pt)
summary(df_graph_n1000$res.sd.pt)


summary(df_graph$res.mean.prot)
summary(df_graph$res.min.prot)
summary(df_graph$res.max.prot)
summary(df_graph$res.sd.prot)

summary(df_graph_n1000$res.mean.prot)
summary(df_graph_n1000$res.min.prot)
summary(df_graph_n1000$res.max.prot)
summary(df_graph_n1000$res.sd.prot)

