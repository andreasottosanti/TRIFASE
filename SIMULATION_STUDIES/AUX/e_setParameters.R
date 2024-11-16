#' Model parameters' combinations
#'
#' @param dir_path_parameters Set the directory to save the RDS file containing the Combination of Parameters
#' @param nclust.pt.start Set the number of clusters for spots
#' @param nclust.prot.start Set the number of clusters for features
#' @param tau.start Value of parameter Tau
#' @param phi.start Value of parameter Phi
#' @param est.kernel If 'none', no spatial correlation will be investigated
#' @param est.upd.row Rules for updating rows
#' @param est.upd.col Rules for updating columns
#' @param est.tau If TRUE, tau will be estimated
#'
#' @return Save the RDS files containing the model parameters' combinations (stored in /SET_PARAMETERS)
#'
funSetParameters<-function(dir_path_parameters,
                     nclust.pt.start,nclust.prot.start,tau.start,phi.start,
                     est.kernel,est.upd.row,est.upd.col,est.tau){
  
  int<-1
  Sys.time()
      for(int1 in 1:length(nclust.pt.start)){
        for(int2 in 1:length(nclust.prot.start)){
          for(int5 in 1:length(est.kernel)){
            if(est.kernel[int5]=="exponential"){
              est.tau.tmp<-TRUE
              for(int3 in 1:length(est.upd.row)){
                for(int4 in 1:length(est.upd.col)){
                  for(int6 in 1:length(tau.start)){
                    for(int7 in 1:length(phi.start)){
                      SetParameters<-list(nclust.pt.start=nclust.pt.start[int1],
                                          nclust.prot.start=nclust.prot.start[int2],
                                          tau.start=tau.start[int6],phi.start=phi.start[int7],
                                          est.kernel=est.kernel[int5],est.upd.row=est.upd.row[int3],
                                          est.upd.col=est.upd.col[int4],est.tau=est.tau.tmp,
                                          scenarioSetParameters=int)
                      
                      saveRDS(
                        SetParameters,
                        file = paste0(dir_path_parameters,
                                      "SetParameters_",
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
            if(est.kernel[int5]=="none"){
              est.tau.tmp <- FALSE
              tau.start.tmp <- 1
              phi.start.tmp <- 1
              est.upd.row.tmp <- "C"
              est.upd.col.tmp <- "A"
              SetParameters<-list(nclust.pt.start=nclust.pt.start[int1],
                                  nclust.prot.start=nclust.prot.start[int2],
                                  tau.start=tau.start.tmp,phi.start=phi.start.tmp,
                                  est.kernel=est.kernel[int5],est.upd.row=est.upd.row.tmp,
                                  est.upd.col=est.upd.col.tmp,est.tau=est.tau.tmp,
                                  scenarioSetParameters=int)
              
              saveRDS(
                SetParameters,
                file = paste0(dir_path_parameters,
                              "SetParameters_",
                              int,
                              ".RDS"
                )
              )
              print(int)
              Sys.time()
              int<-int+1
            }
            if(est.kernel[int5]=="km"){
              est.tau.tmp <- FALSE
              tau.start.tmp <- 1
              phi.start.tmp <- 1
              est.upd.row.tmp <- "NA"
              est.upd.col.tmp <- "NA"
              SetParameters<-list(nclust.pt.start=nclust.pt.start[int1],
                                  nclust.prot.start=nclust.prot.start[int2],
                                  tau.start=tau.start.tmp,phi.start=phi.start.tmp,
                                  est.kernel=est.kernel[int5],est.upd.row=est.upd.row.tmp,
                                  est.upd.col=est.upd.col.tmp,est.tau=est.tau.tmp,
                                  scenarioSetParameters=int)
              
              saveRDS(
                SetParameters,
                file = paste0(dir_path_parameters,
                              "SetParameters_",
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
              