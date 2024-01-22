rm(list=ls())

library(TreeSim)
library(DAISIEprep)

folder<-'/Users/Luis/Desktop/RUG_IslandBiology_MScCourse/Practicals/DAISIE_Insula_beetles/0.Insula_beetles_data/Prepare_the_tree_not for students/'
island_data<-read.table(paste(folder,'Insula_beetles_distribution_data.txt',
                              sep=''),header=T)

#### Simulate tree
## Fixed seed needed to reproduce exact simulation
set.seed(266)
n<-50
lambda <- 4.0
mu <- 0.01
frac <-0.6
numbsim<-1
age<-2
tsims<-sim.bd.taxa.age(n, numbsim, lambda, mu, frac, age, mrca = FALSE)
beetle_tree<-tsims[[1]]


beetle_tree$tip.label <- c("Spec_1","Spec_2","Spec_3","Spec_4","Spec_5",
                         "Spec_6","Spec_7",
                         "Spec_8","Spec_9","Spec_10","Spec_11","Spec_12","Spec_13",
                         "Spec_14",
                         "Spec_15","Spec_16","Spec_17","Spec_18","Spec_19",
                         "Spec_20","Spec_21",
                         "Spec_22","Spec_23","Spec_24","Spec_25","Spec_26",
                         "Spec_27","Spec_28",
                         "Spec_29","Spec_30","Spec_31","Spec_32","Spec_33",
                         "Spec_34","Spec_35",
                         "Spec_36","Spec_37","Spec_38","Spec_39","Spec_40",
                         "Spec_41","Spec_42",
                         "Spec_43","Spec_44","Spec_45","Spec_46","Spec_47",
                         "Spec_48","Spec_49",
                         "Spec_50")


beetle_tree<- ladderize(beetle_tree)
plot(beetle_tree)



island_d<-as.data.frame(island_data$Island)
taxa<-as.data.frame(island_data$Tree)
islands<-as.data.frame(island_d[match(beetle_tree$tip.label,taxa[,1]),])
islands<-t(islands)
islands<-as.character(islands)
names(islands)<-beetle_tree$tip.label


#### SIMMAP 100 reconstructions
beetle_tree_simmap<-make.simmap(beetle_tree,islands,model="ER",nsim=1000)
pd<-summary(beetle_tree_simmap,plot=FALSE)

cols<-setNames(palette()[1:length(unique(islands))],sort(unique(islands)))
cols['Bahamas']<-'yellow'
cols['Hispaniola']<-'magenta'
cols['Jamaica']<-'blue'
cols['Mainland']<-'grey'
cols['LesserAntilles']<-'pink'
cols['PuertoRico']<-'forestgreen'

## Plot SIMMAP tree
par(oma=c(0,0,0,0))
plot(beetle_tree_simmap[[1]],cols,fsize=0.8)
nodelabels(pie=pd$ace,piecol=cols,cex=0.3)
tiplabels(pie=to.matrix(islands,sort(unique(islands))),piecol=cols,cex=0.25)
add.simmap.legend(colors=cols,prompt=FALSE,x=0.9*par()$usr[1],
                  y=25,fsize=0.8)


## PLOT using phyloD (DAISIEprep)
endemicity_status<-islands
endemicity_status<-as.data.frame(endemicity_status)
beetle_phylod <- phylobase::phylo4d(beetle_tree, endemicity_status)
DAISIEprep::plot_phylod(phylod = beetle_phylod)

#write.nexus(beetle_tree,file="Caribbean_beetles.tre")
