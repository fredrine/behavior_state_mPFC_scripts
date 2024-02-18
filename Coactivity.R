wdir = "/Users/fredrine/Documents/Tritask/R_scripts/" #Replace with path to repo
animal = "Diana"
#### Load functions ----
source(paste(wdir, "functions.R",sep=""))
#### Coactivity / pair care  ----
if (animal == "Diana"){DIANA = TRUE} else {DIANA = FALSE}
if (animal == "Leia"){LEIA = TRUE} else {LEIA = FALSE}
if (animal == "Rey"){REY = TRUE} else {REY = FALSE}

w = 21 # Window size for the definition of bursting
t = 5  # Threshold for number of bins in the window required to be above the 90th percentile

#### Make bursts (intervals of heightened activity) and overlaps (when pairs of cells both burst) ----
sd = 5

smoothedo1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open1_sd",sd,"bins.RDS",sep=""))
smoothedo2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open2_sd",sd,"bins.RDS",sep=""))
smoothedo3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open3_sd",sd,"bins.RDS",sep=""))
smoothedc1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing1_sd",sd,"bins.RDS",sep=""))
smoothedc2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing2_sd",sd,"bins.RDS",sep=""))
smoothedc3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing3_sd",sd,"bins.RDS",sep=""))
smootheds1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social1_sd",sd,"bins.RDS",sep=""))
smootheds2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social2_sd",sd,"bins.RDS",sep=""))
smootheds3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social3_sd",sd,"bins.RDS",sep=""))

bursts_c1 = bursting_for_all_cells(smoothedc1,window=w,threshold=t)
bursts_c2 = bursting_for_all_cells(smoothedc2,window=w,threshold=t)
bursts_c3 = bursting_for_all_cells(smoothedc3,window=w,threshold=t)
bursts_s1 = bursting_for_all_cells(smootheds1,window=w,threshold=t)
bursts_s2 = bursting_for_all_cells(smootheds2,window=w,threshold=t)
bursts_s3 = bursting_for_all_cells(smootheds3,window=w,threshold=t)
bursts_o1 = bursting_for_all_cells(smoothedo1,window=w,threshold=t)
bursts_o2 = bursting_for_all_cells(smoothedo2,window=w,threshold=t)
bursts_o3 = bursting_for_all_cells(smoothedo3,window=w,threshold=t)

saveRDS(bursts_o1, paste(wdir, "Processed data/", animal, "/bursts_Open1.RDS",sep=""))
saveRDS(bursts_o2, paste(wdir, "Processed data/", animal, "/bursts_Open2.RDS",sep=""))
saveRDS(bursts_o3, paste(wdir, "Processed data/", animal, "/bursts_Open3.RDS",sep=""))
saveRDS(bursts_c1, paste(wdir, "Processed data/", animal, "/bursts_Chasing1.RDS",sep=""))
saveRDS(bursts_c2, paste(wdir, "Processed data/", animal, "/bursts_Chasing2.RDS",sep=""))
saveRDS(bursts_c3, paste(wdir, "Processed data/", animal, "/bursts_Chasing3.RDS",sep=""))
saveRDS(bursts_s1, paste(wdir, "Processed data/", animal, "/bursts_Social1.RDS",sep=""))
saveRDS(bursts_s2, paste(wdir, "Processed data/", animal, "/bursts_Social2.RDS",sep=""))
saveRDS(bursts_s3, paste(wdir, "Processed data/", animal, "/bursts_Social3.RDS",sep=""))

overlaps_c1 = overall_coactivity_from_indices(bursts_c1,1:dim(bursts_c1)[1],FILTER=FALSE)
overlaps_c2 = overall_coactivity_from_indices(bursts_c2,1:dim(bursts_c2)[1],FILTER=FALSE)
overlaps_c3 = overall_coactivity_from_indices(bursts_c3,1:dim(bursts_c3)[1],FILTER=FALSE)
overlaps_o1 = overall_coactivity_from_indices(bursts_o1,1:dim(bursts_o1)[1],FILTER=FALSE)
overlaps_o2 = overall_coactivity_from_indices(bursts_o2,1:dim(bursts_o2)[1],FILTER=FALSE)
overlaps_o3 = overall_coactivity_from_indices(bursts_o3,1:dim(bursts_o3)[1],FILTER=FALSE)
overlaps_s1 = overall_coactivity_from_indices(bursts_s1,1:dim(bursts_s1)[1],FILTER=FALSE)
overlaps_s2 = overall_coactivity_from_indices(bursts_s2,1:dim(bursts_s2)[1],FILTER=FALSE)
overlaps_s3 = overall_coactivity_from_indices(bursts_s3,1:dim(bursts_s3)[1],FILTER=FALSE)

saveRDS(overlaps_o1, paste(wdir, "Processed data/", animal, "/overlaps_Open1.RDS",sep=""))
saveRDS(overlaps_o2, paste(wdir, "Processed data/", animal, "/overlaps_Open2.RDS",sep=""))
saveRDS(overlaps_o3, paste(wdir, "Processed data/", animal, "/overlaps_Open3.RDS",sep=""))
saveRDS(overlaps_c1, paste(wdir, "Processed data/", animal, "/overlaps_Chasing1.RDS",sep=""))
saveRDS(overlaps_c2, paste(wdir, "Processed data/", animal, "/overlaps_Chasing2.RDS",sep=""))
saveRDS(overlaps_c3, paste(wdir, "Processed data/", animal, "/overlaps_Chasing3.RDS",sep=""))
saveRDS(overlaps_s1, paste(wdir, "Processed data/", animal, "/overlaps_Social1.RDS",sep=""))
saveRDS(overlaps_s2, paste(wdir, "Processed data/", animal, "/overlaps_Social2.RDS",sep=""))
saveRDS(overlaps_s3, paste(wdir, "Processed data/", animal, "/overlaps_Social3.RDS",sep=""))



#### Randomized overlaps to get cutoff ----
ncells = dim(overlaps_o1)[1]

set.seed(1)
random_burst_c1 = bursts_c1
random_burst_c2 = bursts_c2
random_burst_c3 = bursts_c3

random_burst_o1 = bursts_o1
random_burst_o2 = bursts_o2
random_burst_o3 = bursts_o3

random_burst_s1 = bursts_s1
random_burst_s2 = bursts_s2
random_burst_s3 = bursts_s3

for (i in 1:dim(random_burst_c1)[2]){
  random_burst_c1[,i] = cyclic_shift(random_burst_c1[,i])
}
random_overlaps_c1 = overall_coactivity_from_indices(random_burst_c1,1:dim(random_burst_c1)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_c2)[2]){
  random_burst_c2[,i] = cyclic_shift(random_burst_c2[,i])
}
random_overlaps_c2 = overall_coactivity_from_indices(random_burst_c2,1:dim(random_burst_c2)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_c3)[2]){
  random_burst_c3[,i] = cyclic_shift(random_burst_c3[,i])
}
random_overlaps_c3 = overall_coactivity_from_indices(random_burst_c3,1:dim(random_burst_c3)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_s1)[2]){
  random_burst_s1[,i] = cyclic_shift(random_burst_s1[,i])
}
random_overlaps_s1 = overall_coactivity_from_indices(random_burst_s1,1:dim(random_burst_s1)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_s2)[2]){
  random_burst_s2[,i] = cyclic_shift(random_burst_s2[,i])
}
random_overlaps_s2 = overall_coactivity_from_indices(random_burst_s2,1:dim(random_burst_s2)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_s3)[2]){
  random_burst_s3[,i] = cyclic_shift(random_burst_s3[,i])
}
random_overlaps_s3 = overall_coactivity_from_indices(random_burst_s3,1:dim(random_burst_s3)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_o1)[2]){
  random_burst_o1[,i] = cyclic_shift(random_burst_o1[,i])
}
random_overlaps_o1 = overall_coactivity_from_indices(random_burst_o1,1:dim(random_burst_o1)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_o2)[2]){
  random_burst_o2[,i] = cyclic_shift(random_burst_o2[,i])
}
random_overlaps_o2 = overall_coactivity_from_indices(random_burst_o2,1:dim(random_burst_o2)[1],FILTER=FALSE)
for (i in 1:dim(random_burst_o3)[2]){
  random_burst_o3[,i] = cyclic_shift(random_burst_o3[,i])
}
random_overlaps_o3 = overall_coactivity_from_indices(random_burst_o3,1:dim(random_burst_o3)[1],FILTER=FALSE)


cutoff_c1 = quantile(random_overlaps_c1,c(0.9,0.95),na.rm=T)
cutoff_c2 = quantile(random_overlaps_c2,c(0.9,0.95),na.rm=T)
cutoff_c3 = quantile(random_overlaps_c3,c(0.9,0.95),na.rm=T)
cutoff_s1 = quantile(random_overlaps_s1,c(0.9,0.95),na.rm=T)
cutoff_s2 = quantile(random_overlaps_s2,c(0.9,0.95),na.rm=T)
cutoff_s3 = quantile(random_overlaps_s3,c(0.9,0.95),na.rm=T)
cutoff_o1 = quantile(random_overlaps_o1,c(0.9,0.95),na.rm=T)
cutoff_o2 = quantile(random_overlaps_o2,c(0.9,0.95),na.rm=T)
cutoff_o3 = quantile(random_overlaps_o3,c(0.9,0.95),na.rm=T)

cutoffs = cbind(cutoff_c1,cutoff_c2,cutoff_c3,cutoff_o1,cutoff_o2,cutoff_o3,cutoff_s1,cutoff_s2,cutoff_s3)

combined_cutoff = quantile(c(c(random_overlaps_c1,random_overlaps_c2,random_overlaps_c3,
                               random_overlaps_o1,random_overlaps_o2,random_overlaps_o3,
                               random_overlaps_s1,random_overlaps_s2,random_overlaps_s3)),c(0.9,0.95))

saveRDS(combined_cutoff,paste(wdir, "Processed data/",animal,"_cutoff_0_9_0_95.RDS",sep=""))

if (TRUE){par(mfrow=c(2,2))
  hist(apply(bursts_c1,2,mean))
  hist(apply(bursts_c2,2,mean))
  hist(apply(bursts_c3,2,mean))
  hist(apply(random_burst,2,mean))
  par(mfrow=c(2,2))
  hist(overlaps_c1,breaks=seq(-0.3,0.75,by=0.05))
  hist(overlaps_c2,breaks=seq(-0.3,0.75,by=0.05))
  hist(overlaps_c3,breaks=seq(-0.3,0.75,by=0.05))
  hist(random_overlaps_c1,breaks=seq(-0.3,0.75,by=0.05))
} #hist of overlaps


#### Use cutoff to find the overall coactivity / "pair care" level ----

combined_cutoff = readRDS(paste(wdir, "Processed data/",animal,"_cutoff_0_9_0_95.RDS",sep=""))

cutoff = combined_cutoff[2]

if (TRUE){
  pairs_c1 = rbind(which(overlaps_c1 > cutoff) %/% ncells,which(overlaps_c1 > cutoff) %% ncells)
  pairs_c1[1,which(pairs_c1[2,]!=0)] = pairs_c1[1,which(pairs_c1[2,]!=0)] + 1
  pairs_c1[2,which(pairs_c1[2,]==0)] = ncells
  pairs_c2 = rbind(which(overlaps_c2 > cutoff) %/% ncells,which(overlaps_c2 > cutoff ) %% ncells)
  pairs_c2[1,which(pairs_c2[2,]!=0)] = pairs_c2[1,which(pairs_c2[2,]!=0)] + 1
  pairs_c2[2,which(pairs_c2[2,]==0)] = ncells
  pairs_c3 = rbind(which(overlaps_c3 > cutoff) %/% ncells,which(overlaps_c3 > cutoff) %% ncells)
  pairs_c3[1,which(pairs_c3[2,]!=0)] = pairs_c3[1,which(pairs_c3[2,]!=0)] + 1
  pairs_c3[2,which(pairs_c3[2,]==0)] = ncells
  
  pairs_c1 = pairs_c1[,which(pairs_c1[1,] < pairs_c1[2,])]
  pairs_c2 = pairs_c2[,which(pairs_c2[1,] < pairs_c2[2,])]
  pairs_c3 = pairs_c3[,which(pairs_c3[1,] < pairs_c3[2,])]
  
  pairs_c1 = pairs_c1[,order(pairs_c1[1,])]
  pairs_c2 = pairs_c2[,order(pairs_c2[1,])]
  pairs_c3 = pairs_c3[,order(pairs_c3[1,])]
  
  pairs_o1 = rbind(which(overlaps_o1 > cutoff) %/% ncells,which(overlaps_o1 > cutoff) %% ncells)
  pairs_o1[1,which(pairs_o1[2,]!=0)] = pairs_o1[1,which(pairs_o1[2,]!=0)] + 1
  pairs_o1[2,which(pairs_o1[2,]==0)] = ncells
  pairs_o2 = rbind(which(overlaps_o2 > cutoff) %/% ncells,which(overlaps_o2 > cutoff ) %% ncells)
  pairs_o2[1,which(pairs_o2[2,]!=0)] = pairs_o2[1,which(pairs_o2[2,]!=0)] + 1
  pairs_o2[2,which(pairs_o2[2,]==0)] = ncells
  pairs_o3 = rbind(which(overlaps_o3 > cutoff) %/% ncells,which(overlaps_o3 > cutoff) %% ncells)
  pairs_o3[1,which(pairs_o3[2,]!=0)] = pairs_o3[1,which(pairs_o3[2,]!=0)] + 1
  pairs_o3[2,which(pairs_o3[2,]==0)] = ncells
  
  pairs_o1 = pairs_o1[,which(pairs_o1[1,] < pairs_o1[2,])]
  pairs_o2 = pairs_o2[,which(pairs_o2[1,] < pairs_o2[2,])]
  pairs_o3 = pairs_o3[,which(pairs_o3[1,] < pairs_o3[2,])]
  
  pairs_o1 = pairs_o1[,order(pairs_o1[1,])]
  pairs_o2 = pairs_o2[,order(pairs_o2[1,])]
  pairs_o3 = pairs_o3[,order(pairs_o3[1,])]
  
  pairs_s1 = rbind(which(overlaps_s1 > cutoff) %/% ncells,which(overlaps_s1 > cutoff) %% ncells)
  pairs_s1[1,which(pairs_s1[2,]!=0)] = pairs_s1[1,which(pairs_s1[2,]!=0)] + 1
  pairs_s1[2,which(pairs_s1[2,]==0)] = ncells
  pairs_s2 = rbind(which(overlaps_s2 > cutoff) %/% ncells,which(overlaps_s2 > cutoff ) %% ncells)
  pairs_s2[1,which(pairs_s2[2,]!=0)] = pairs_s2[1,which(pairs_s2[2,]!=0)] + 1
  pairs_s2[2,which(pairs_s2[2,]==0)] = ncells
  pairs_s3 = rbind(which(overlaps_s3 > cutoff) %/% ncells,which(overlaps_s3 > cutoff) %% ncells)
  pairs_s3[1,which(pairs_s3[2,]!=0)] = pairs_s3[1,which(pairs_s3[2,]!=0)] + 1
  pairs_s3[2,which(pairs_s3[2,]==0)] = ncells
  
  pairs_s1 = pairs_s1[,which(pairs_s1[1,] < pairs_s1[2,])]
  pairs_s2 = pairs_s2[,which(pairs_s2[1,] < pairs_s2[2,])]
  pairs_s3 = pairs_s3[,which(pairs_s3[1,] < pairs_s3[2,])]
  
  pairs_s1 = pairs_s1[,order(pairs_s1[1,])]
  pairs_s2 = pairs_s2[,order(pairs_s2[1,])]
  pairs_s3 = pairs_s3[,order(pairs_s3[1,])]
} # Find pairs with overlap larger than cutoff

if (TRUE){
  filter_c1 = overlaps_c1
  filter_c1[which(overlaps_c1 > cutoff)] = 1
  filter_c1[which(overlaps_c1 < cutoff)] = 0
  filter_c2 = overlaps_c2
  filter_c2[which(overlaps_c2 > cutoff)] = 1
  filter_c2[which(overlaps_c2 < cutoff)] = 0
  filter_c3 = overlaps_c3
  filter_c3[which(overlaps_c3 > cutoff)] = 1
  filter_c3[which(overlaps_c3 < cutoff)] = 0
  
  filter_s1 = overlaps_s1
  filter_s1[which(overlaps_s1 > cutoff)] = 1
  filter_s1[which(overlaps_s1 < cutoff)] = 0
  filter_s2 = overlaps_s2
  filter_s2[which(overlaps_s2 > cutoff)] = 1
  filter_s2[which(overlaps_s2 < cutoff)] = 0
  filter_s3 = overlaps_s3
  filter_s3[which(overlaps_s3 > cutoff)] = 1
  filter_s3[which(overlaps_s3 < cutoff)] = 0
  
  filter_o1 = overlaps_o1
  filter_o1[which(overlaps_o1 > cutoff)] = 1
  filter_o1[which(overlaps_o1 < cutoff)] = 0
  filter_o2 = overlaps_o2
  filter_o2[which(overlaps_o2 > cutoff)] = 1
  filter_o2[which(overlaps_o2 < cutoff)] = 0
  filter_o3 = overlaps_o3
  filter_o3[which(overlaps_o3 > cutoff)] = 1
  filter_o3[which(overlaps_o3 < cutoff)] = 0
} #Make filters of best pairs

if (TRUE){
  saveRDS(filter_o1,paste(wdir, "Processed data/",animal,"_filter_o1.RDS",sep=""))
  saveRDS(filter_o2,paste(wdir, "Processed data/",animal,"_filter_o2.RDS",sep=""))
  saveRDS(filter_o3,paste(wdir, "Processed data/",animal,"_filter_o3.RDS",sep=""))
  
  saveRDS(filter_c1,paste(wdir, "Processed data/",animal,"_filter_c1.RDS",sep=""))
  saveRDS(filter_c2,paste(wdir, "Processed data/",animal,"_filter_c2.RDS",sep=""))
  saveRDS(filter_c3,paste(wdir, "Processed data/",animal,"_filter_c3.RDS",sep=""))
  
  saveRDS(filter_s1,paste(wdir, "Processed data/",animal,"_filter_s1.RDS",sep=""))
  saveRDS(filter_s2,paste(wdir, "Processed data/",animal,"_filter_s2.RDS",sep=""))
  saveRDS(filter_s3,paste(wdir, "Processed data/",animal,"_filter_s3.RDS",sep=""))
} #Save filters

if (TRUE){
  pc_c1 = pair_care_prop(pairs_c1,bursts_c1)
  pc_c2 = pair_care_prop(pairs_c2,bursts_c2)
  pc_c3 = pair_care_prop(pairs_c3,bursts_c3)
  pc_s1 = pair_care_prop(pairs_s1,bursts_s1)
  pc_s2 = pair_care_prop(pairs_s2,bursts_s2)
  pc_s3 = pair_care_prop(pairs_s3,bursts_s3)
  pc_o1 = pair_care_prop(pairs_o1,bursts_o1)
  pc_o2 = pair_care_prop(pairs_o2,bursts_o2)
  pc_o3 = pair_care_prop(pairs_o3,bursts_o3)
  
  np$save(paste(wdir, "Processed data/",animal,"_pc_c1.npy",sep=""),pc_c1)
  np$save(paste(wdir, "Processed data/",animal,"_pc_c2.npy",sep=""),pc_c2)
  np$save(paste(wdir, "Processed data/",animal,"_pc_c3.npy",sep=""),pc_c3)
  
  np$save(paste(wdir, "Processed data/",animal,"_pc_s1.npy",sep=""),pc_s1)
  np$save(paste(wdir, "Processed data/",animal,"_pc_s2.npy",sep=""),pc_s2)
  np$save(paste(wdir, "Processed data/",animal,"_pc_s3.npy",sep=""),pc_s3)
  
  np$save(paste(wdir, "Processed data/",animal,"_pc_o1.npy",sep=""),pc_o1)
  np$save(paste(wdir, "Processed data/",animal,"_pc_o2.npy",sep=""),pc_o2)
  np$save(paste(wdir, "Processed data/",animal,"_pc_o3.npy",sep=""),pc_o3)
} #Make and save pair cares

if (TRUE){
  par(mfrow=c(3,1))
  plot(pc_c1,type="l",lwd=0.5)
  plot(pc_c2,type="l",lwd=0.5)
  plot(pc_c3,type="l",lwd=0.5)
  
  plot(pc_s1,type="l",lwd=0.5)
  plot(pc_s2,type="l",lwd=0.5)
  plot(pc_s3,type="l",lwd=0.5)
  
  plot(pc_o1,type="l",lwd=0.5)
  plot(pc_o2,type="l",lwd=0.5)
  plot(pc_o3,type="l",lwd=0.5)
} # Plot pc graphs


#####