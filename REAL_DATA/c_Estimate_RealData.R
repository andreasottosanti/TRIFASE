#' Fit TRIFASE model to real data
#'
#' @param DataReal A list containing the values and coordinates
#' @param n.cores Number of cores for parallel computing
#' @param n.runs Number of runs of the algorithm
#' @param nclust.prot.start Set the number of clusters for features
#' @param nclust.pt.start Set the number of clusters for spot
#' @param tau.start Starting value of Tau
#' @param phi.start Starting value of Phi
#' @param est.kernel If 'none', no spatial correlation will be investigated
#' @param est.upd.row Rules for updating rows
#' @param est.upd.col Rules for updating columns
#' @param est.tau If TRUE, tau will be estimated
#' @param int.phi Index corresponding to the chosen phi parameter
#' @param int.upd.Row Index corresponding to the chosen method for updating rows
#' @param int.upd.Col Index corresponding to the chosen method for updating columns
#' @param dir_path_runRealData Set the directory where to save the RDS output file
#'
#' @return RDS files containing the fitted models (complete outputs saved in /ESTIMATE)
#' 
funEstimate<-function(DataReal,n.cores,n.runs,nclust.prot.start,nclust.pt.start,
                      tau.start,phi.start,est.kernel,est.upd.row,est.upd.col,est.tau,
                      int.phi,int.upd.Row,int.upd.Col,dir_path_runRealData){
  
  res_runs<-list()
  
  for (int in 1:length(DataReal)) {
    
    X = DataReal[[int]]$GenMatrix
    S = DataReal[[int]]$Coord
    
    if(est.kernel!="km"){
      # 50 runs in parallel
      res_runs[[int]] <- multi_runs(RUNS = n.runs, CORES = n.cores,
                                    X = X, 
                                    S = S, 
                                    K = nclust.prot.start,
                                    R = nclust.pt.start,
                                    tau = tau.start,
                                    phi = phi.start,
                                    kernel = est.kernel,
                                    n_iter = 200,
                                    seed = 213,
                                    threshold = 1e-3,
                                    estimate_tau = est.tau,
                                    upd_row = est.upd.row,
                                    upd_col = est.upd.col)
    }
    if(est.kernel=="km"){
      #K-means
      t1 <- Sys.time()
      K = nclust.prot.start
      R = nclust.pt.start
      km_rows <- kmeans(X, centers = K, nstart = 20)
      km_cols <- kmeans(t(X), centers = R, nstart = 20)
      t2 <- Sys.time()
      res_runs[[int]]<-list(elapsed_time=t2 - t1,
                            f=km_rows$cluster,
                            g=km_cols$cluster,
                            niter="NA",
                            tau="FALSE",
                            ind_minloss="NA",mu="NA",loss="NA")
    }
    
  }
  
  
  saveRDS(
    res_runs,
    file = paste0(dir_path_runRealData,
                  "Results_DataReal_",
                  int.phi,"_",int.upd.Row,"_",int.upd.Col,
                  ".RDS"
    )
  )
  
}