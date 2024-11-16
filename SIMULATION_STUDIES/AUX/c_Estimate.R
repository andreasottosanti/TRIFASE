#' Fit the models
#'
#' @param dir_path_genData Set the directory to read the RDS file containing the Dataset
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
#' @param int.scenario The index corresponding to the chosen Scenario
#' @param int.parameters The index corresponding to the chosen model parameters' combinations
#' @param dir_path_estimate Set the directory where to save the RDS output file
#'
#' @return RDS files containing the fitted models (complete outputs saved in /ESTIMATED_RUNS)
#' 
funEstimate<-function(dir_path_genData,n.cores,n.runs,nclust.prot.start,nclust.pt.start,
                      tau.start,phi.start,est.kernel,est.upd.row,est.upd.col,est.tau,
                      int.scenario,int.parameters,dir_path_estimate){
  
  data_gen<-readRDS(file = paste0(dir_path_genData,
                                  "GenData_Scenario_",
                                  int.scenario,
                                  ".RDS"
  ))
  
  res_runs<-list()
  
  for (int in 1:length(data_gen)) {
    
    X = data_gen[[int]]$GenMatrix
    S = data_gen[[int]]$Coord
    
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
    file = paste0(dir_path_estimate,
                  "Results_Scenario_",
                  int.scenario,"_",int.parameters,
                  ".RDS"
    )
  )
  
}
