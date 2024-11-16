# Run additional simulation study 3 ----
# This code runs the additional case in which we increase the initial number of spots (columns).
# We set 1000 columns in the generated datasets

## Set directories to save results ----
dir_path_scenario<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/SCENARIOS/"
dir_path_genData<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/GENERATED_DATA/"
dir_path_estimate<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/ESTIMATE_RUNS/"
dir_path_extracted<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/EXTRACT_RESULTS/"
dir_path_parameters<-"SIMULATION_STUDIES/SIMULATION_RESULTS/Add_sim_study_3/SET_PARAMETERS/"

## Generate scenarios ----
nclust.pt <- c(4)
n.pt      <- c(250)
nifix.pt <- c(FALSE)
nclust.prot <- c(3)
n.prot    <- c(30)
nifix.prot <- c(FALSE)
spat<-c(TRUE,FALSE)
tau.scenario <- c(3)
phi.scenario <- c(10)
vet.mean<-list(c(1.5, .5, 0, -1.5, 0, -3, .5, 5, .5, 1.5, 0, 1))
vet.mean.name<-c("different")
funScenari(dir_path_scenario,
           nclust.pt,n.pt,nifix.pt,nclust.prot,n.prot,nifix.prot,
           tau.scenario,phi.scenario,vet.mean,vet.mean.name,spat)

## Generate data ----
# Generate the datasets (stored in /GENERATED_DATA)
n.dataset  <-30
n.scenari <- length(nclust.pt)*length(n.pt)*length(nifix.pt)*length(nclust.prot)*length(n.prot)*length(nifix.prot)*
  length(tau.scenario)*length(phi.scenario)*length(vet.mean)*length(vet.mean.name)*length(spat)
for(int in 1:n.scenari){
  funGenerateData(dir_path_scenario,int.scenario=int,n.dataset,n.cores,dir_path_genData)
}

## Generate the starting parameters ----
# Save the hyper parameters combinations (stored in /SET_PARAMETERS)
nclust.pt.start <- c(4,8,12)
nclust.prot.start <- c(3,6,9)
tau.start <- c(3,6)
phi.start <- c(1e-03,1e-01,10,20)
est.kernel<-c("km","none","exponential")
est.upd.row<-c("C","S")
est.upd.col<-c("A")
est.tau<-FALSE
funSetParameters(dir_path_parameters,
                 nclust.pt.start,nclust.prot.start,tau.start,phi.start,
                 est.kernel,est.upd.row,est.upd.col,est.tau)

## Run TRIFASE ----
# Fit the models (complete outputs saved in /ESTIMATED_RUNS)
n.runs<-5
n.parameters<-(length(nclust.pt.start)*length(nclust.prot.start)*length(tau.start)*
                 length(phi.start)*length(est.upd.row)*length(est.upd.col)*length(est.tau))+  #144 combinations for TRIFASE (est.kernel=exponential)
  (length(nclust.pt.start)*length(nclust.prot.start))+ #9 combinations for Wang (est.kernel=none)
  (length(nclust.pt.start)*length(nclust.prot.start))  #9 combinations for k-means (est.kernel=km)
for(int in 1:n.scenari){
  for(int_par in 1:n.parameters){
    data_parameters<-readRDS(file = paste0(dir_path_parameters,
                                           "SetParameters_",
                                           int_par,
                                           ".RDS"
    ))
    funEstimate(dir_path_genData,n.cores,n.runs,
                nclust.prot.start=data_parameters$nclust.prot.start,
                nclust.pt.start=data_parameters$nclust.pt.start,
                tau.start=data_parameters$tau.start,phi.start=data_parameters$phi.start,
                est.kernel=data_parameters$est.kernel,
                est.upd.row=data_parameters$est.upd.row,est.upd.col=data_parameters$est.upd.col,
                est.tau=data_parameters$est.tau,int.scenario=int,int.parameters=int_par,dir_path_estimate)
  }
}

## Extract results ----
# Extract the best runs and save the corresponding results in .RDS files (stored in /EXTRACT_RESULTS)
for(int in 1:n.scenari){
  for(int_par in 1:n.parameters){
    funExtractResults(dir_path_estimate,dir_path_genData,dir_path_parameters,dir_path_scenario,int.scenario=int,int.parameters=int_par,dir_path_extracted)
  }
}
