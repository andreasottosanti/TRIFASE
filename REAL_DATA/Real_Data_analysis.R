# Application on real dataset ----
dir_path_runRealData<-"REAL_DATA/ESTIMATE/"

X_rid <- readRDS("REAL_DATA/DATASET/X_rid.RDS")
n <- nrow(X_rid) # n. proteins
p <- ncol(X_rid) # n. locations
S_rid<-readRDS("REAL_DATA/DATASET/S_rid.RDS")

hist(X_rid)
length(X_rid==0)


hist(scale(log10(X_rid+1)))
hist(log(X_rid**.5+1),breaks = 100,freq=F,ylim=c(0,2))

YY =  log(X_rid**.5+1)

km = kmeans(t(YY),10)

plot(S_rid,pch=21,bg=km$cluster)

## Estimate TRIFASE algorithm ----
K=10
R=5
tau.start<-3
phi.start<-c(0.5,0.7,0.9,1,3,5,10,20)
n.cores<-50
n.runs<-50
upd.row<-c("S","C")
upd.col<-c("A")

DataReal<-list()
DataReal[[1]]<-list(GenMatrix=YY, Coord=S_rid)


for(int.phi in 1:length(phi.start)){
  for(int.row in 1:length(upd.row)){
    for(int.col in 1:length(upd.col)){
      print(Sys.time())
      funEstimate(DataReal,n.cores=n.cores,n.runs=n.runs,
                  nclust.prot.start=K,
                  nclust.pt.start=R,
                  tau.start=tau.start,phi.start=phi.start[int.phi],
                  est.kernel="exponential",
                  est.upd.row=upd.row[int.row],est.upd.col=upd.col[int.col],
                  est.tau=TRUE,int.phi=int.phi,int.upd.Row=int.row,int.upd.Col=int.col,dir_path_runRealData)
      print(paste0(int.phi,"_",int.row,"_",int.col))
    }
  }
}

## Extract results ----
newlist<-NULL
res.elapsed_time<-NULL
res.iter<-NULL
res.tau<-NULL
res.minloss<-NULL
res.clust.row.unique<-NULL
res.clust.col.unique<-NULL
phi.start.set<-NULL
upd.row.set<-NULL
upd.col.set<-NULL
int3<-1
int.dataset<-1
for(int.phi in 1:length(phi.start)){
  for(int.row in 1:length(upd.row)){
    for(int.col in 1:length(upd.col)){
      data_est<-readRDS(file = paste0(dir_path_runRealData,
                                      "Results_DataReal_",
                                      int.phi,"_",int.row,"_",int.col,
                                      ".RDS"
      ))
      res = extract_best_min_loss(data_est[[int.dataset]])
      newlist[[int3]]<-data.frame(loss=res$loss,iter=seq(1:res$niter),
                                  phi=rep(phi.start[int.phi],length(res$niter)),
                                  Row=rep(upd.row[int.row],length(res$niter)),Col=rep(upd.col[int.col],length(res$niter)))
      res.elapsed_time[int3]=res$elapsed_time
      res.iter[int3]=res$niter
      res.tau[int3]=res$tau
      res.minloss[int3]=res$loss[res$ind_minloss-1]
      res.clust.row.unique[int3]=length(unique(res$f))
      res.clust.col.unique[int3]=length(unique(res$g))
      phi.start.set[int3]=phi.start[int.phi]
      upd.row.set[int3]=upd.row[int.row]
      upd.col.set[int3]=upd.col[int.col]
      int3<-int3+1
    }
  }
}
results<-data.frame(Phi=phi.start.set,UpdRow=upd.row.set,UpdCol=upd.col.set,MinLoss=res.minloss,time=res.elapsed_time,maxIter=res.iter,TauStim=res.tau,
                    NclustProt=res.clust.row.unique,NclustPixel=res.clust.col.unique)

## Save Table of Manuscript----
print(xtable(results, type = "latex"), file = "REAL_DATA/GRAPHS/Results_Grid_phi.tex")
write.csv(results,"REAL_DATA/GRAPHS/Results_Grid_phi.csv")