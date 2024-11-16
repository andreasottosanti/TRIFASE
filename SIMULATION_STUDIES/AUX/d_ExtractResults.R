#' Extract the best runs and save the corresponding results in .RDS files
#'
#' @param dir_path_estimate Set the directory to read the RDS file containing the Fitted models
#' @param dir_path_genData Set the directory to read the RDS file containing the Generated Datasets
#' @param dir_path_parameters Set the directory to read the RDS file containing the Combination of Parameters
#' @param dir_path_scenario Set the directory to read the RDS file containing the Scenarios
#' @param int.scenario The index corresponding to the chosen Scenario
#' @param int.parameters The index corresponding to the chosen model parameters' combinations
#' @param dir_path_extracted Set the directory where to save the RDS output file
#'
#' @return RDS file containing the results corresponding to the best run
#'
funExtractResults<-function(dir_path_estimate,dir_path_genData,dir_path_parameters,dir_path_scenario,int.scenario,int.parameters,dir_path_extracted){
  
  data_est<-readRDS(file = paste0(dir_path_estimate,
                                  "Results_Scenario_",
                                  int.scenario,"_",int.parameters,
                                  ".RDS"
  ))
  
  data_gen<-readRDS(file = paste0(dir_path_genData,
                                  "GenData_Scenario_",
                                  int.scenario,
                                  ".RDS"
  ))
  
  data_parameters<-readRDS(file = paste0(dir_path_parameters,
                           "SetParameters_",
                           int.parameters,
                           ".RDS"
  ))
  
  data_scen<-readRDS(file = paste0(dir_path_scenario,
                              "Scenario_",
                              int.scenario,
                              ".RDS"
  ))
  
  extract_results<-list()
  
  for (int.dataset in 1:length(data_gen)) {
    
    if(data_parameters$est.kernel!="km"){
      # extract best
      res = extract_best_min_loss(data_est[[int.dataset]])
    }
    if(data_parameters$est.kernel=="km"){
      res = data_est[[int.dataset]]
    }
    
    W <- data_gen[[int.dataset]]$GenClustCol
    Z <- data_gen[[int.dataset]]$GenClustRow
    
    
    extract_results[[int.dataset]]<-list(res.clust.row=Spartaco_CER(res$f, Z),
                                          res.clust.col=Spartaco_CER(res$g, W),
                                          res.elapsed_time=res$elapsed_time,
                                          res.iter=res$niter,
                                          res.tau=res$tau,
                                          res.minloss=res$ind_minloss,
                                          res.clust.row.unique=length(unique(res$f)),
                                          res.clust.col.unique=length(unique(res$g)),
                                          res.mu=res$mu,res.f=res$f,res.g=res$g,res.loss=res$loss,
                                         
                                         res.mean.prot=mean(table(Z)),res.sd.prot=sd(table(Z)),
                                         res.min.prot=min(table(Z)),res.max.prot=max(table(Z)),
                                         res.mean.pt=mean(table(W)),res.sd.pt=sd(table(W)),
                                         res.min.pt=min(table(W)),res.max.pt=max(table(W)),
                                         
                                         nclust.pt.start=data_parameters$nclust.pt.start,
                                         nclust.prot.start=data_parameters$nclust.prot.start,
                                         tau.start=data_parameters$tau.start,
                                         phi.start=data_parameters$phi.start,
                                         est.kernel=data_parameters$est.kernel,
                                         est.upd.row=data_parameters$est.upd.row,
                                         est.upd.col=data_parameters$est.upd.col,
                                         est.tau=data_parameters$est.tau,
                                         
                                         nclust.pt=data_scen$nclust.pt,
                                         n.pt=data_scen$n.pt,
                                         nifix.pt=data_scen$nifix.pt,
                                         nclust.prot=data_scen$nclust.prot,
                                         n.prot=data_scen$n.prot,
                                         nifix.prot=data_scen$nifix.prot,
                                         tau.scenario=data_scen$tau.s,
                                         phi.scenario=data_scen$phi.s,
                                         vet.mean.name=data_scen$vet.mean.name,
                                         spat=data_scen$spatial,
                                         Number.scenario=data_scen$scenario,
                                         Number.setscenario=int.scenario,
                                         Number.setparameters=int.parameters
                                         )
    }

  saveRDS(
    extract_results,
    file = paste0(dir_path_extracted,
                  "Extract_Results_Scenario_",
                  int.scenario,"_",int.parameters,
                  ".RDS"
    )
  )
  
}
