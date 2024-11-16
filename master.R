# Code to run the simulation study

# Section 0:
## Set the libraries ----
library(dplyr)
library(stringr)
library(ggplot2)
library(lattice)
library(Matrix)
library(TRIFASE)
library(Rfast)
library(reshape2)
library(latex2exp)
library(patchwork)
require(scales)
library(fossil)
library(xtable)
library(viridis)

## Import the functions ----
source("SIMULATION_STUDIES/AUX/sim_coordinates_matrix.R")
source("SIMULATION_STUDIES/AUX/a_Scenarios.R")
source("SIMULATION_STUDIES/AUX/b_GenerateData.R")
source("SIMULATION_STUDIES/AUX/c_Estimate.R")
source("SIMULATION_STUDIES/AUX/c_Estimate_Scalability.R")
source("SIMULATION_STUDIES/AUX/d_ExtractResults.R")
source("SIMULATION_STUDIES/AUX/e_setParameters.R")
source("SIMULATION_STUDIES/AUX/f_Performance_index.R")

# Set the number of cores to be used for parallel computing
n.cores <-60
print("END Section 0!")

# Section 1: Run the simulation study ----
# The script will 
#   - Set up the directories to save the results 
#   - Save the parameters used to generate the data under each scenario (stored in /SCENARIOS)
#   - Generate the datasets (stored in /GENERATED_DATA)
#   - Save the model parameters' combinations (stored in /SET_PARAMETERS)
#   - Fit the models (complete outputs saved in /ESTIMATED_RUNS)
#   - Extract the best runs and save the corresponding results in .RDS files (stored in /EXTRACT_RESULTS)
source("SIMULATION_STUDIES/run_functions_Simulation_study.R")
print("END Section 1!")

# Section 2: Run additional simulation study 1 ----
# This code runs the additional case in which the initial number of clusters for the spots (columns)
# is set to three times the TRUE number of column clusters used to generate the data
# R = 3 * R_iniz
source("SIMULATION_STUDIES/run_functions_Add_sim_study_1.R")
print("END Section 2!")

# Section 3: Run additional simulation study 2 ----
# This code runs the additional case in which the initial number of clusters for the features (rows)
# is set to three times the TRUE number of row cluster used to generate data
# K = 3 * K_iniz
source("SIMULATION_STUDIES/run_functions_Add_sim_study_2.R")
print("END Section 3!")

# Section 4: Run additional simulation study 3 ----
# This code runs the additional case in which we increase the initial number of spots (columns).
# We set 1000 columns in the generated datasets
source("SIMULATION_STUDIES/run_functions_Add_sim_study_3.R")
print("END Section 4!")

# Section 5: Run additional simulation study 4 ----
# This code runs the additional case in which we increase the initial number of spots (columns) 
# and features (rows) when we generated the initial datasets
source("SIMULATION_STUDIES/run_functions_Add_sim_study_4.R")
print("END Section 5!")

# Section 6: Graphs ----
## Create the images reported in the paper regarding TRIFASE clustering performance----
# This scripts will 
#   - Extract results from the different simulation studies
#   - Create graphs (p=100 & p=1000) for :
##    - Row and Column clustering performance
##    - Computational Times 
##    - Estimated non-empty row and column clusters
##    - Estimated Tau
source("SIMULATION_STUDIES/plot_Simulation_graphs_Spatial.R")

## Create the images reported in the paper regarding TRIFASE clustering performance with no spatial information----
source("SIMULATION_STUDIES/plot_Simulation_graphs_noSpatial.R")

## Create images reported in the paper regarding scalability results----
source("SIMULATION_STUDIES/plot_Simulation_graphs_Scalability.R")

## Create images reported in the paper regarding the loss function----
source("SIMULATION_STUDIES/plot_Simulation_graphs_Loss.R")
print("END Section 6!")


## DO NOT RUN ## Data are not openly available
# Section 7: Real Data analysis

## Import function ----
source("REAL_DATA/c_Estimate_RealData.R")

## Fit TRIFASE models ----
source("REAL_DATA/Real_Data_analysis.R")

## Create images reported in the paper regarding the real data example ----
source("REAL_DATA/Real_Data_graph.R")

print("END Section 7!")