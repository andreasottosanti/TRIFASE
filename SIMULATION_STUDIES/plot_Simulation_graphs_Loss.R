# This code creates images reported in the paper regarding the Loss function
## Extract results from the Simulation Study (p=100) ----
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Simulation_Study/EXTRACT_RESULTS/"
dir_path_estimate<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Simulation_Study/ESTIMATE_RUNS/"

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
df_graph<-do.call(rbind,newlist)
df_rid<-df_graph[which((df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10 &
                          df_graph$tau.start==3 & df_graph$nclust.pt==4 & df_graph$nclust.prot==3 & 
                          df_graph$tau.scenario==3 & df_graph$phi.scenario==10 & 
                          df_graph$nclust.prot.start==3 & df_graph$nclust.pt.start==4)),]


int.tot<-1
newlist_runs<-NULL
newlist_runs_info<-NULL
updaterowcol<-c("C,C","C,S","C,A","S,C","S,S","S,A")
for(ind in 1:length(unique(df_rid$Number.setparameters))){
  int.scenario<-1
  int.parameters<-c(unique(df_rid$Number.setparameters))[ind]
  upd<-updaterowcol[ind]
    estimate_scenario<-readRDS(file = paste0(dir_path_estimate,
                                             "Results_Scenario_",
                                             int.scenario,"_",int.parameters,
                                             ".RDS"
    ))
    le<-length(estimate_scenario[[1]])
    for (i in 1:le){
    newlist_runs[[int.tot]]<-data.frame(loss=estimate_scenario[[1]][[i]]$loss,iter=seq(1:estimate_scenario[[1]][[i]]$niter),
                                        upd=rep(upd,length(estimate_scenario[[1]][[i]]$niter)))
    newlist_runs_info[[int.tot]]<-data.frame(lossMin=estimate_scenario[[1]][[i]]$loss[estimate_scenario[[1]][[i]]$ind_minloss-1],updateRC=upd)
    int.tot<-int.tot+1
    }
}
df_graph_rid<-do.call(rbind,newlist_runs_info)


## Create graphs (p=100)---- 
### Loss ----
p_100<-ggplot(data = df_graph_rid, aes(x=updateRC,y=log10(as.numeric(lossMin)))) + geom_boxplot()+ 
  theme_bw()+labs(x = "Model", y = expression(log[10]("loss function")),title = "Loss function values across 50 runs (p=100)")+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size=18),plot.title = element_text(size=18,hjust = 0.5),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + 
  guides(fill=guide_legend(title=""))

### CER clustering ----
# Rows
values.vec <- unlist(lapply(newlist_runs_info2, function(l) l[,1]))
values.vec <- matrix(values.vec, length(newlist_runs_info2[[1]][,1]),length(newlist_runs_info2))
reference.allocation <- tapply(df_graph_rid$lossMin, df_graph_rid$updateRC, which.min)
CER.values.rows <- names.models.rows <- c()
model.names <- names(reference.allocation)
for(j in 1:length(reference.allocation)){
  new.CER.values <- apply(values.vec[,df_graph_rid$updateRC == model.names[j]][,-reference.allocation[j]], 2, 
                          function(x) rand.index(x,values.vec[,df_graph_rid$updateRC == model.names[j]][,reference.allocation[j]]))
  CER.values.rows <- c(CER.values.rows,
                       new.CER.values
  )
  names.models.rows <- c(names.models.rows, rep(model.names[j],length(new.CER.values)))
}
reference.allocation2 <- reference.allocation + 50 * c(0:5)
p_100_rows <- ggplot(mapping = aes(x=factor(names.models.rows),
                                   y=CER.values.rows)) + geom_boxplot()+ 
  theme_bw()+labs(x = "Model", y = "1-CER",title = "Row clustering comparison over 50 runs (p=100)")+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size=18),plot.title = element_text(size=18,hjust = 0.5),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + 
  guides(fill=guide_legend(title=""))
# Cols
values.vec <- unlist(lapply(newlist_runs_info3, function(l) l[,1]))
values.vec <- matrix(values.vec, length(newlist_runs_info3[[1]][,1]),length(newlist_runs_info3))
reference.allocation <- tapply(df_graph_rid$lossMin, df_graph_rid$updateRC, which.min)
CER.values.cols <- names.models.cols <- c()
model.names <- names(reference.allocation)
for(j in 1:length(reference.allocation)){
  new.CER.values <- apply(values.vec[,df_graph_rid$updateRC == model.names[j]][,-reference.allocation[j]], 2, 
                          function(x) rand.index(x,values.vec[,df_graph_rid$updateRC == model.names[j]][,reference.allocation[j]]))
  CER.values.cols <- c(CER.values.cols,
                       new.CER.values
  )
  names.models.cols <- c(names.models.cols, rep(model.names[j],length(new.CER.values)))
}
reference.allocation2 <- reference.allocation + 50 * c(0:5)
p_100_cols <- ggplot(mapping = aes(x=factor(names.models.cols),
                                   y=CER.values.cols)) + geom_boxplot()+ 
  theme_bw()+labs(x = "Model", y = "1-CER",title = "Column clustering comparison over 50 runs (p=100)")+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size=18),plot.title = element_text(size=18,hjust = 0.5),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + 
  guides(fill=guide_legend(title=""))


## Extract results from the Simulation Study (p=1000)----
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/EXTRACT_RESULTS/"
dir_path_estimate<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/ESTIMATE_RUNS/"

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
df_graph<-do.call(rbind,newlist)
df_rid<-df_graph[which((df_graph$est.kernel=="exponential" & df_graph$spat==TRUE & df_graph$phi.start==10 &
                          df_graph$tau.start==3 & df_graph$nclust.pt==4 & df_graph$nclust.prot==3 & 
                          df_graph$tau.scenario==3 & df_graph$phi.scenario==10 & 
                          df_graph$nclust.prot.start==3 & df_graph$nclust.pt.start==4)),]


int.tot<-1
newlist_runs<-NULL
newlist_runs_info<-NULL
updaterowcol<-c("C,A","S,A")
for(ind in 1:length(unique(df_rid$Number.setparameters))){
  int.scenario<-1
  int.parameters<-c(unique(df_rid$Number.setparameters))[ind]
  upd<-updaterowcol[ind]
  estimate_scenario<-readRDS(file = paste0(dir_path_estimate,
                                           "Results_Scenario_",
                                           int.scenario,"_",int.parameters,
                                           ".RDS"
  ))
  le<-length(estimate_scenario[[1]])
  for (i in 1:le){
    newlist_runs[[int.tot]]<-data.frame(loss=estimate_scenario[[1]][[i]]$loss,iter=seq(1:estimate_scenario[[1]][[i]]$niter),
                                        upd=rep(upd,length(estimate_scenario[[1]][[i]]$niter)))
    newlist_runs_info[[int.tot]]<-data.frame(lossMin=estimate_scenario[[1]][[i]]$loss[estimate_scenario[[1]][[i]]$ind_minloss-1],updateRC=upd)
    int.tot<-int.tot+1
  }
}
df_graph_rid_n1000<-do.call(rbind,newlist_runs_info)


## Create graphs (p=1000) ----
### Loss ----
p_1000<-ggplot(data = df_graph_rid_n1000, aes(x=updateRC,y=log10(as.numeric(lossMin)))) + geom_boxplot()+ 
  theme_bw()+labs(x = "Model", y = expression(log[10]("loss function")),title = "Loss function values across 50 runs (p=1000)")+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size=18),plot.title = element_text(size=18,hjust = 0.5),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + 
  guides(fill=guide_legend(title=""))

### CER clustering ----
# Rows
values.vec <- unlist(lapply(newlist_runs_info2, function(l) l[,1]))
values.vec <- matrix(values.vec, length(newlist_runs_info2[[1]][,1]),length(newlist_runs_info2))
reference.allocation <- tapply(df_graph_rid_n1000$lossMin, df_graph_rid_n1000$updateRC, which.min)
CER.values.rows <- names.models.rows <- c()
model.names <- names(reference.allocation)
for(j in 1:length(reference.allocation)){
  new.CER.values <- apply(values.vec[,df_graph_rid_n1000$updateRC == model.names[j]][,-reference.allocation[j]], 2, 
                          function(x) rand.index(x,values.vec[,df_graph_rid_n1000$updateRC == model.names[j]][,reference.allocation[j]]))
  CER.values.rows <- c(CER.values.rows,
                       new.CER.values
  )
  names.models.rows <- c(names.models.rows, rep(model.names[j],length(new.CER.values)))
}
reference.allocation2 <- reference.allocation + 50 * c(0:1)
p_1000_rows <- ggplot(mapping = aes(x=factor(names.models.rows),
                                    y=CER.values.rows)) + geom_boxplot()+ 
  theme_bw()+labs(x = "Model", y = "1-CER",title = "Row clustering comparison over 50 runs (p=1000)")+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size=18),plot.title = element_text(size=18,hjust = 0.5),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + 
  guides(fill=guide_legend(title=""))
# Cols
values.vec <- unlist(lapply(newlist_runs_info3, function(l) l[,1]))
values.vec <- matrix(values.vec, length(newlist_runs_info3[[1]][,1]),length(newlist_runs_info3))
reference.allocation <- tapply(df_graph_rid_n1000$lossMin, df_graph_rid_n1000$updateRC, which.min)
CER.values.cols <- names.models.cols <- c()
model.names <- names(reference.allocation)
for(j in 1:length(reference.allocation)){
  new.CER.values <- apply(values.vec[,df_graph_rid_n1000$updateRC == model.names[j]][,-reference.allocation[j]], 2, 
                          function(x) rand.index(x,values.vec[,df_graph_rid_n1000$updateRC == model.names[j]][,reference.allocation[j]]))
  CER.values.cols <- c(CER.values.cols,
                       new.CER.values
  )
  names.models.cols <- c(names.models.cols, rep(model.names[j],length(new.CER.values)))
}
reference.allocation2 <- reference.allocation + 50 * c(0:1)
p_1000_cols <- ggplot(mapping = aes(x=factor(names.models.cols),
                                    y=CER.values.cols)) + geom_boxplot()+ 
  theme_bw()+labs(x = "Model", y = "1-CER",title = "Column clustering comparison over 50 runs (p=1000)")+
  theme(axis.title = element_text(size = 18),axis.text = element_text(size=18),plot.title = element_text(size=18,hjust = 0.5),panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + 
  guides(fill=guide_legend(title=""))


## Save graphs ----
(p_100+p_1000)/(p_100_rows+p_100_cols)/(p_1000_rows+p_1000_cols)
ggsave("SIMULATION_STUDIES/GRAPHS/Figure13Suppl.eps",height = 20,width = 25) 
ggsave("SIMULATION_STUDIES/GRAPHS/Figure13Suppl.pdf",height = 20,width = 25,dpi = 600) 
