#' Generate datasets
#'
#' @param dir_path_scenari Set the directory to read the RDS file containing parameters to generate the data under each scenario
#' @param int.scenario Index of Scenario to be imported
#' @param n.dataset Number of datasets to generate
#' @param n.cores Number of cores for parallel computing
#' @param dir_path_genData Set the directory where to save the RDS output file (Generated Dataset)
#'
#' @return RDS files containing the generated datasets (stored in /GENERATED_DATA)
#' 
funGenerateData<-function(dir_path_scenari,int.scenario,n.dataset,n.cores,dir_path_genData){
  
    Scen<-readRDS(file = paste0(dir_path_scenari,
                                "Scenario_",
                                int.scenario,
                                ".RDS"
    ))
    

    seed <- seq(1, n.dataset, 1)
    single_iter = function(i) {
      sim_generation_data(
        seed = seed[i],
        nclust.pt = Scen$nclust.pt,  
        n.pt = Scen$n.pt,            
        nifix.pt = Scen$nifix.pt,    
        n.prot = Scen$n.prot,        
        nclust.prot = Scen$nclust.prot,  
        nifix.prot = Scen$nifix.prot,    
        vet.mean = Scen$vet.mean,        
        tau = Scen$tau.s,            
        phi = Scen$phi.s,            
        spat=Scen$spatial            
      )
    }
    results1 = parallel::mclapply(1:n.dataset,
                                  function(i)
                                    single_iter(i),
                                  mc.cores = n.cores)
    
    saveRDS(
      results1,
      file = paste0(dir_path_genData,
                    "GenData_Scenario_",
                    int,
                    ".RDS"
      )
    )   

}
  