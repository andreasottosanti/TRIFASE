# Application on real dataset ----
dir_path_runRealData<-"REAL_DATA/ESTIMATE/"
dir_path_RealData   <-"REAL_DATA/DATASET/"

X_rid<-readRDS(file = paste0(dir_path_RealData,"X_rid.RDS"))
S_rid<-readRDS(file = paste0(dir_path_RealData,"S_rid.RDS"))
YY =  log(X_rid**.5+1)

km = kmeans(t(YY),10)

plot(S_rid,pch=21,bg=km$cluster)

## Create graphs ----
data_est<-readRDS(file = paste0(dir_path_runRealData,
                                "Results_DataReal_",
                                3,"_",2,"_",1,
                                ".RDS"
))

int.dataset<-1
res_scelto = extract_best_min_loss(data_est[[int.dataset]])

w_f<-apply(res_scelto$Fm,2,sum)
w_f_tot<-sum(w_f)
w_g<-apply(res_scelto$Gm,2,sum)
w_g_tot<-sum(w_g)
library(reshape2)
data1 <- melt(t(res_scelto$mu))
data1$w_Var1<-rep(NA,dim(data1)[1])
for(i in 1:length(w_g)){
  data1$w_Var1[which(data1$Var1==i)]<-w_g[i]
}
data1$w_Var2<-rep(NA,dim(data1)[1])
for(i in 1:length(w_f)){
  data1$w_Var2[which(data1$Var2==i)]<-w_f[i]
}
S1 = as_tibble(S_rid) %>% mutate(cl = res_scelto$g)
g1<-ggplot(S1)+
  geom_tile(aes(x=new_Data_matrix.coor_x,y=new_Data_matrix.coor_y,fill=factor(res_scelto$g)))+
  theme_bw()+scale_fill_viridis_d()+    #option = "H"
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position="bottom", text = element_text(size=18),
  )+
  labs(x="x coord.", y = "y coord.", title = "")+ 
  guides(fill=guide_legend(title="Column cluster"))+
  facet_grid(~"Estimated image segmentation")
g1

heatmap(res_scelto$mu)
w_f<-apply(res_scelto$Fm,2,sum)
w_f_tot<-sum(w_f)
w_g<-apply(res_scelto$Gm,2,sum)
w_g_tot<-sum(w_g)

p1<-ggplot(data1 , aes(x = factor(Var1), y = factor(Var2),fill = value)) +geom_tile(aes(fill = value, color = "white"),color = "white",size=2) +
  geom_text(aes(label=paste0("n=",w_Var2,"\n","p=",w_Var1)), fontface = "bold", col="white")+
  theme_bw() + scale_fill_viridis(discrete = FALSE,option = "H")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position="bottom", text = element_text(size=18),
        legend.key.width = unit(1,"cm"))+
  labs(x="Column cluster", y = "Row cluster", title = "", fill="Mean Value")+
  facet_grid(~"Estimated mean matrix")

g1+p1
ggsave("REAL_DATA/GRAPHS/Figure5.eps",height = 20,width = 25) 
ggsave("REAL_DATA/GRAPHS/Figure5.pdf",height = 20,width = 25,dpi = 600) 

table(res_scelto$f)
table(res_scelto$g)
apply(res_scelto$Fm,2,sum)
apply(res_scelto$Gm,2,sum)

#protein
#X_plot <- scale(as.data.frame(X_rid))
X_plot<-YY
f<-res_scelto$f
g<-res_scelto$g
X_plot$clstp<-f
X_plot$clstn<-g
X=c()
row.names(X_rid)<-NULL
colnames(X_rid)<-NULL
names(X_rid)<-NULL
coor_x<-S_rid$new_Data_matrix.coor_x
coor_y<-S_rid$new_Data_matrix.coor_y
row.names(coor_x)<-NULL
row.names(coor_y)<-NULL
vet<-as.vector(which(table(f)>0))
vet2<-as.vector(which(table(g)>0))
for(i in vet){
  for(j in vet2){
    print(i)
    if(i!=7){
      D = cbind(
        apply((YY[f==i,g==j]),2,mean,na.rm=T),
        apply((YY[f==i,g==j]),2,sd), coor_x[g==j], coor_y[g==j],rep(i,length(coor_x[g==j])),rep(j,length(coor_x[g==j])))
      row.names(D)<-NULL
      colnames(D) = c("mean","std.dev.","xc","yc","clstp","clstn")
      X=rbind(X,D)
      #X = rbind(X,reshape2::melt(D,c("xc","yc")) %>% mutate(rcl=1))
    }else{
      D = cbind( YY[f==i,g==j], rep(NA,length(coor_x[g==j])),coor_x[g==j], coor_y[g==j],rep(i,length(coor_x[g==j])),rep(j,length(coor_x[g==j])))
      row.names(D)<-NULL
      colnames(D) = c("mean","std.dev.","xc","yc","clstp","clstn")
      X=rbind(X,D)
    }
  }
}
X<-data.frame(X)
X_new = reshape2::melt(X,id.vars = c("xc","yc","clstp","clstn"))
X_new$clstn <- paste("Column cluster",X_new$clstn)
X_new$clstp <- paste("Row cluster",X_new$clstp)  
X_new$clstn<-factor(X_new$clstn,levels = paste("Column cluster",c(4,1,5,2,3)))
X_new$clstp<-factor(X_new$clstp,levels = paste("Row cluster",c(8,9,5,7,2,10,3,4,1,6)))

X_new<-X_new[which(X_new$variable=="mean"),]
p2<-ggplot(X_new %>% filter(clstp %in% paste("Row cluster",c(8,9,5,7,2))))+theme_bw()+
  geom_tile( aes(x=xc,y=yc,fill=value))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position="bottom",text = element_text(size=18),
        legend.key.width = unit(1,"cm"))+
  labs(x="x coord.", y = "y.coord", title = "", fill="Average\nabundance  ") +
  facet_grid(clstp~clstn)+
  scale_fill_viridis(option = "H",
                     limits=range(X_new$value)) 
p2
ggsave("REAL_DATA/GRAPHS/Figure14Suppl.eps",height = 20,width = 25) 
ggsave("REAL_DATA/GRAPHS/Figure14Suppl.pdf",height = 20,width = 25,dpi = 600) 

X_new<-X_new[which(X_new$variable=="mean"),]
p2<-ggplot(X_new %>% filter(!(clstp %in% paste("Row cluster",c(8,9,5,7,2)))))+theme_bw()+
  geom_tile( aes(x=xc,y=yc,fill=value))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        legend.position="bottom",text = element_text(size=18),
        legend.key.width = unit(1,"cm"))+
  labs(x="x coord.", y = "y.coord", title = "", fill="Average\nabundance  ") +
  facet_grid(clstp~clstn)+
  scale_fill_viridis(option = "H",
                     limits=range(X_new$value)) 
p2
ggsave("REAL_DATA/GRAPHS/Figure15Suppl.eps",height = 20,width = 25) 
ggsave("REAL_DATA/GRAPHS/Figure15Suppl.pdf",height = 20,width = 25,dpi = 600) 

as.character(X_new$clstp)

X_new_c7 <- X_new[ which( X_new$variable == "mean" &  (X_new$clstp == "Row cluster 7")), ]
X_new_c8 <- X_new[ which( X_new$variable == "mean" &  (X_new$clstp == "Row cluster 8")), ]
X_new_c9 <- X_new[ which( X_new$variable == "mean" &  (X_new$clstp == "Row cluster 9")), ]
X_new_c10<- X_new[ which( X_new$variable == "mean" &  (X_new$clstp == "Row cluster 10")),]

DD = rbind(
  cbind(X_new_c7,var =  "Row cluster 7"),
  cbind(X_new_c8,var =  "Row cluster 8"),
  cbind(X_new_c9,var =  "Row cluster 9"),
  cbind(X_new_c10,var = "Row cluster 10")
)
DD$var <- factor(DD$var,levels = c("Row cluster 7","Row cluster 8","Row cluster 9","Row cluster 10"))

ggplot(DD)+theme_bw()+
  geom_tile(aes(x=xc,y=yc,fill=value))+
  theme(
    text = element_text(size=18),
    legend.position = "bottom"
  )+
  labs(x="x coord.", y = "y coord.", title = "", fill="Average\nabundance  ") +
  facet_wrap(~var)+scale_fill_viridis(option = "H") 
ggsave("REAL_DATA/GRAPHS/Figure6.eps",height = 20,width = 25) 
ggsave("REAL_DATA/GRAPHS/Figure6.pdf",height = 20,width = 25,dpi = 600) 
