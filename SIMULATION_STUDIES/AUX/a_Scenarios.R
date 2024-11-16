#' Generate a list of Scenarios
#'
#' @param dir_path_scenario Set the directory where to save the RDS output file (Save the parameters used to generate the data under each scenario)
#' @param nclust.pt Number of clusters for the spots (columns) 
#' @param n.pt Number of spots (columns) 
#' @param nifix.pt Same number of spots in each cluster
#' @param nclust.prot Number of clusters for the features (rows)
#' @param n.prot Number of features (rows)
#' @param nifix.prot Same number of features in each cluster
#' @param tau.scenario Value of parameter tau to generate spatial correlation
#' @param phi.scenario Value of parameter phi to generate spatial correlation
#' @param vet.mean Vector of mean to generate data
#' @param vet.mean.name Degree of diversity among generated means
#' @param spat If TRUE spatial correlation among data will be generated
#'
#' @return Save the RDS files containing the list of parameters used to generate the data under each scenario (stored in /SCENARIOS)
#' 
funScenari<-function(dir_path_scenario,
                     nclust.pt,n.pt,nifix.pt,nclust.prot,n.prot,nifix.prot,
                     tau.scenario,phi.scenario,vet.mean,vet.mean.name,spat){
  
  int<-1
  Sys.time()
  for(int1 in 1:length(nclust.pt)){
    for(int2 in 1:length(n.pt)){
      for(int3 in 1:length(nifix.pt)){
        for(int4 in 1:length(nclust.prot)){
          for(int5 in 1:length(n.prot)){
            for(int6 in 1:length(nifix.prot)){
              for(int9 in 1:length(vet.mean.name)){
                for(int10 in 1:length(spat)){
                  if(spat[int10]==TRUE){
                    for(int7 in 1:length(tau.scenario)){
                      for(int8 in 1:length(phi.scenario)){
                        scenario<-list(nclust.pt=nclust.pt[int1],n.pt=n.pt[int2],nifix.pt=nifix.pt[int3],
                                       nclust.prot=nclust.prot[int4],n.prot=n.prot[int5],nifix.prot=nifix.prot[int6],
                                       spatial=spat[int10],
                                       tau.s=tau.scenario[int7],phi.s=phi.scenario[int8],
                                       vet.mean=vet.mean[[int9]],vet.mean.name=vet.mean.name[int9],
                                       scenario=int)
                        
                        saveRDS(
                          scenario,
                          file = paste0(dir_path_scenario,
                                        "Scenario_",
                                        int,
                                        ".RDS"
                          )
                        )
                        print(int)
                        Sys.time()
                        int<-int+1
                      }
                    }
                  }
                  if(spat[int10]==FALSE){
                    tau.scenario.tmp<-1
                      phi.scenario.tmp<-1
                        scenario<-list(nclust.pt=nclust.pt[int1],n.pt=n.pt[int2],nifix.pt=nifix.pt[int3],
                                       nclust.prot=nclust.prot[int4],n.prot=n.prot[int5],nifix.prot=nifix.prot[int6],
                                       spatial=spat[int10],
                                       tau.s=tau.scenario.tmp[int7],phi.s=phi.scenario.tmp[int8],
                                       vet.mean=vet.mean[[int9]],vet.mean.name=vet.mean.name[int9],
                                       scenario=int)
                        
                        saveRDS(
                          scenario,
                          file = paste0(dir_path_scenario,
                                        "Scenario_",
                                        int,
                                        ".RDS"
                          )
                        )
                        print(int)
                        Sys.time()
                        int<-int+1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

