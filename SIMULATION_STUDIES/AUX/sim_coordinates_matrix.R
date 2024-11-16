#' Generate data and coordinates
#'
#' @param seed Set seed
#' @param nclust.pt Number of clusters for the spots (columns) 
#' @param n.pt Number of spots (columns) 
#' @param nifix.pt Same number of spots in each cluster
#' @param nclust.prot Number of clusters for the features (rows)
#' @param n.prot Number of features (rows)
#' @param nifix.prot Same number of features in each cluster
#' @param vet.mean Vector of mean to generate data
#' @param tau Value of parameter tau to generate spatial correlation
#' @param phi Value of parameter phi to generate spatial correlation
#' @param spat If TRUE spatial correlation among data will be generated
#'
#' @return list: GenMatrix is a matrix (n.prot x n.clust.prot) x (n.pt x n.clust.pt)
#'               Coord is a matrix (n.pt x n.clust.pt) x 2
#'               GenClustCol is a vector (n.pt x n.clust.pt)
#'               GenClustRow is a vector (n.prot x n.clust.prot)
#'
sim_generation_data<-function(seed,nclust.pt,n.pt,nifix.pt,nclust.prot,n.prot,nifix.prot,vet.mean,tau,phi,spat){
  set.seed(seed)
  
  #proporzione diversa di soggetti per cluster
  #se nifix.pt==TRUE, stesso numero di campioni n.pt per cluter nclust.pt
  repeat {
    ifelse(nifix.pt==TRUE,
           trialref <- rep(1:nclust.pt, each = n.pt),
           trialref <- sort(sample(1:nclust.pt, nclust.pt * n.pt, replace = TRUE, prob = rpois(nclust.pt, 5))))
    if(length(table(trialref))==nclust.pt)
      break
  }
  
  #proporzione diversa di proteine per cluster
  repeat {
    ifelse(nifix.prot==TRUE,
           trialref.prot <- rep(1:nclust.prot, each = n.prot),
           trialref.prot <- sort(sample(1:nclust.prot, nclust.prot * n.prot, replace = TRUE, prob = rpois(nclust.prot, 5))))
    if(length(table(trialref.prot))==nclust.prot)
      break
  }
  
  #genero griglia punti per coordinate
  start_lat_seq<-seq(0,(sqrt(table(trialref))[1]),by=1)
  start_lng_seq<-seq(0,(sqrt(table(trialref))[1]),by=1)
  df = expand.grid(a = start_lat_seq, b = start_lng_seq)
  
  ##generate coordinates
  coordinates.x<-NULL
  coordinates.y<-NULL
  new_start<-0
  start_int<-0
  ind<-1
  for(i in 1:nclust.pt){
    start_lat_seq<-seq(0,(sqrt(table(trialref))[i]),by=1)
    start_lng_seq<-seq(new_start,new_start+(sqrt(table(trialref))[i]),by=1)
    start_int<-length(start_lng_seq)
    griddf <- expand.grid(latcoords = start_lat_seq, lngcoords = start_lng_seq)
    int_coord<-1
    for(int in 1:(table(trialref)[i])){
      coordinates.x[ind]<-griddf$latcoords[int_coord]
      coordinates.y[ind]<-griddf$lngcoords[int_coord]
      int_coord<-int_coord+1
      ind<-ind+1
    }
    new_start<-new_start+start_int
  }
  
  coordinates<-data.frame(coordinates.x,coordinates.y)

  #coordinates
  S <- as.matrix(coordinates)
  
  #cluster columns and rows
  W<-trialref
  Z<-trialref.prot
  
  mu.true <- matrix(vet.mean, nclust.prot, nclust.pt, byrow = T)   #matrice mu
  D <- tau*exp(-as.matrix(dist(S))/phi)                            #penalizzazione spaziale
  #voglio generare una dipendenza spaziale o no
  ifelse(spat==TRUE,
    L <- t(chol(D)),
    L <- diag(dim(D)[1]))
  X_b <- NULL
  X_tmp <- NULL
  
  for(i.cprot in 1:nclust.prot){
    U <- matrix(rnorm(sum(Z == i.cprot) * (nclust.pt*n.pt)), sum(Z == i.cprot), (nclust.pt*n.pt))
    for(j.cpt in 1:nclust.pt){
      X_tmp[[j.cpt]]<-matrix(mu.true[i.cprot,j.cpt], sum(Z == i.cprot), sum(W == j.cpt))
    }
    X_cpt = do.call(cbind, X_tmp) + U %*% t(L)
    rownames(X_cpt) <- colnames(X_cpt) <- NULL
    X_b[[i.cprot]]<-X_cpt
  }
  
  X = do.call(rbind, X_b)
  
  return(list("GenMatrix"=X,"Coord"=S,"GenClustCol"=W,"GenClustRow"=Z))
}


## list: GenMatrix is a matrix (n.prot x n.clust.prot) x (n.pt x n.clust.pt)
##       Coord is a matrix (n.pt x n.clust.pt) x 2
##       GenClustCol is a vector (n.pt x n.clust.pt)
##       GenClustRow is a vector (n.prot x n.clust.prot)