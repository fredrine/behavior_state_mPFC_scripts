wdir = "/Users/fredrine/Documents/Tritask/R_scripts/" #Replace with path to repo
animal = "Diana"
#### Load functions ----
source(paste(wdir, "functions.R",sep=""))
#### Ensembles of cells, Diana ----
ensembles = readRDS(paste(wdir,"Processed data/",animal,"/ensembles.RDS",sep=""))
ensembles$all = 1:313
ensembles$chasinghd = sort(union(ensembles$chasing,ensembles$hd))
ensembles$rearinghd = sort(union(ensembles$rearing,ensembles$hd))
ensembles$headbobhd = sort(union(ensembles$headbob,ensembles$hd))
ensembles$investigatehd = sort(union(ensembles$investigate,ensembles$hd))
ensembles$headbobnohd = sort(setdiff(ensembles$headbob,ensembles$hd))
ensembles$investigatenohd = sort(setdiff(ensembles$investigate,ensembles$hd))
#### UMAPs ----
oldHDcolors = T # Uses another color scheme for head direction if TRUE 

sd = 5

smoothedopen1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open1_sd",sd,"bins.RDS",sep=""))
smoothedopen2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open2_sd",sd,"bins.RDS",sep=""))
smoothedopen3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open3_sd",sd,"bins.RDS",sep=""))
smoothedchase1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing1_sd",sd,"bins.RDS",sep=""))
smoothedchase2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing2_sd",sd,"bins.RDS",sep=""))
smoothedchase3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing3_sd",sd,"bins.RDS",sep=""))
smoothedsocial1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social1_sd",sd,"bins.RDS",sep=""))
smoothedsocial2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social2_sd",sd,"bins.RDS",sep=""))
smoothedsocial3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social3_sd",sd,"bins.RDS",sep=""))

if (TRUE){
  inds_for_c1 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_c1.RDS",sep=""))
  inds_for_c2 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_c2.RDS",sep=""))
  inds_for_c3 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_c3.RDS",sep=""))
  inds_for_s1 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_s1.RDS",sep=""))
  inds_for_s2 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_s2.RDS",sep=""))
  inds_for_s3 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_s3.RDS",sep=""))
  inds_for_o1 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_o1.RDS",sep=""))
  inds_for_o2 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_o2.RDS",sep=""))
  inds_for_o3 = readRDS(paste(wdir,"Processed data/", animal, "/inds_for_o3.RDS",sep=""))
  
  behaviors_c = readRDS(paste(wdir,"Processed data/behaviors_c.RDS",sep=""))
  behaviors_o = readRDS(paste(wdir,"Processed data/behaviors_o.RDS",sep=""))
  behaviors_s = readRDS(paste(wdir,"Processed data/behaviors_s.RDS",sep=""))
  behaviors_ocs = c("chasing",behaviors_s)
  
  labels_o1 = true_labels(dim(smoothedopen1)[1],behaviors_o,inds_for_o1)
  labels_o2 = true_labels(dim(smoothedopen2)[1],behaviors_o,inds_for_o2)
  labels_o3 = true_labels(dim(smoothedopen3)[1],behaviors_o,inds_for_o3)
  
  labels_s1 = true_labels(dim(smoothedsocial1)[1],behaviors_s,inds_for_s1)
  labels_s2 = true_labels(dim(smoothedsocial2)[1],behaviors_s,inds_for_s2)
  labels_s3 = true_labels(dim(smoothedsocial3)[1],behaviors_s,inds_for_s3)
  
  labels_c1 = true_labels(dim(smoothedchase1)[1],behaviors_c,inds_for_c1)
  labels_c2 = true_labels(dim(smoothedchase2)[1],behaviors_c,inds_for_c2)
  labels_c3 = true_labels(dim(smoothedchase3)[1],behaviors_c,inds_for_c3)
} # Behavior labels

if (TRUE){
  C1_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/chasing1covariates.txt",sep=""),sep=" ")[,c(1,3,16)]
  C2_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/chasing2covariates.txt",sep=""),sep=" ")[,c(1,3,16)]
  C3_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/chasing3covariates.txt",sep=""),sep=" ")[,c(1,3,16)]
  
  colnames(C1_covariates) = c("speed","HD","HDderiv")
  colnames(C2_covariates) = c("speed","HD","HDderiv")
  colnames(C3_covariates) = c("speed","HD","HDderiv")
  
  C1_covariates$speed[which(C1_covariates$speed < 0.05)] = 0.05
  C2_covariates$speed[which(C2_covariates$speed < 0.05)] = 0.05
  C3_covariates$speed[which(C3_covariates$speed < 0.05)] = 0.05
  C1_covariates$speed[which(C1_covariates$speed > 150)] = 150
  C2_covariates$speed[which(C2_covariates$speed > 150)] = 150
  C3_covariates$speed[which(C3_covariates$speed > 150)] = 150
  
  C1_covariates$HDderiv[which(C1_covariates$HDderiv > 0)] = log(1+C1_covariates$HDderiv[which(C1_covariates$HDderiv > 0)])
  C1_covariates$HDderiv[which(C1_covariates$HDderiv < 0)] = -log(1-C1_covariates$HDderiv[which(C1_covariates$HDderiv < 0)])
  C1_covariates$HDderiv[which(C1_covariates$HDderiv < -6)] = -6
  C1_covariates$HDderiv[which(C1_covariates$HDderiv > 6)] = 6
  C2_covariates$HDderiv[which(C2_covariates$HDderiv > 0)] = log(1+C2_covariates$HDderiv[which(C2_covariates$HDderiv > 0)])
  C2_covariates$HDderiv[which(C2_covariates$HDderiv < 0)] = -log(1-C2_covariates$HDderiv[which(C2_covariates$HDderiv < 0)])
  C2_covariates$HDderiv[which(C2_covariates$HDderiv < -6)] = -6
  C2_covariates$HDderiv[which(C2_covariates$HDderiv > 6)] = 6
  C3_covariates$HDderiv[which(C3_covariates$HDderiv > 0)] = log(1+C3_covariates$HDderiv[which(C3_covariates$HDderiv > 0)])
  C3_covariates$HDderiv[which(C3_covariates$HDderiv < 0)] = -log(1-C3_covariates$HDderiv[which(C3_covariates$HDderiv < 0)])
  C3_covariates$HDderiv[which(C3_covariates$HDderiv < -6)] = -6
  C3_covariates$HDderiv[which(C3_covariates$HDderiv > 6)] = 6
  
  O1_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/open1covariates.txt",sep=""),sep=" ")[,c(1,3)]
  O2_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/open2covariates.txt",sep=""),sep=" ")[,c(1,3)]
  O3_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/open3covariates.txt",sep=""),sep=" ")[,c(1,3)]
  
  colnames(O1_covariates) = c("speed","HD")
  colnames(O2_covariates) = c("speed","HD")
  colnames(O3_covariates) = c("speed","HD")
  
  O1_covariates$speed[which(O1_covariates$speed < 0.05)] = 0.05
  O2_covariates$speed[which(O2_covariates$speed < 0.05)] = 0.05
  O3_covariates$speed[which(O3_covariates$speed < 0.05)] = 0.05
  O1_covariates$speed[which(O1_covariates$speed > 150)] = 150
  O2_covariates$speed[which(O2_covariates$speed > 150)] = 150
  O3_covariates$speed[which(O3_covariates$speed > 150)] = 150
  
  S1_covariates_a = read.csv(paste(wdir, "Tracking data/", animal, "/social1acovariates.txt",sep=""),sep=" ")[,c(1,3)]
  S1_covariates_b = read.csv(paste(wdir, "Tracking data/", animal, "/social1bcovariates.txt",sep=""),sep=" ")[,c(1,3)]
  S2_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/social2covariates.txt",sep=""),sep=" ")[,c(1,3)]
  S3_covariates = read.csv(paste(wdir, "Tracking data/", animal, "/social3covariates.txt",sep=""),sep=" ")[,c(1,3)]
  
  colnames(S1_covariates_a) = c("speed","HD")
  colnames(S1_covariates_b) = c("speed","HD")
  colnames(S2_covariates) = c("speed","HD")
  colnames(S3_covariates) = c("speed","HD")
  
  S1_covariates_a$speed[which(S1_covariates_a$speed < 0.05)] = 0.05
  S1_covariates_b$speed[which(S1_covariates_b$speed < 0.05)] = 0.05
  S2_covariates$speed[which(S2_covariates$speed < 0.05)] = 0.05
  S3_covariates$speed[which(S3_covariates$speed < 0.05)] = 0.05
  S1_covariates_a$speed[which(S1_covariates_a$speed > 150)] = 150
  S1_covariates_b$speed[which(S1_covariates_b$speed > 150)] = 150
  S2_covariates$speed[which(S2_covariates$speed > 150)] = 150
  S3_covariates$speed[which(S3_covariates$speed > 150)] = 150
  
  np = import("numpy")
  c1_ego2 = np$load(paste(wdir, "Tracking data/", animal, "/c1_ego2_azimuth.npy",sep=""))
  c2_ego2 = np$load(paste(wdir, "Tracking data/", animal, "/c2_ego2_azimuth.npy",sep=""))
  c3_ego2 = np$load(paste(wdir, "Tracking data/", animal, "/c3_ego2_azimuth.npy",sep=""))
  
  o1_ego2 = np$load(paste(wdir, "Tracking data/", animal, "/o1_ego2_azimuth.npy",sep=""))
  o2_ego2 = np$load(paste(wdir, "Tracking data/", animal, "/o2_ego2_azimuth.npy",sep=""))
  o3_ego2 = np$load(paste(wdir, "Tracking data/", animal, "/o3_ego2_azimuth.npy",sep=""))
  
  C1_covariates = cbind(C1_covariates,c1_ego2)
  C2_covariates = cbind(C2_covariates,c2_ego2)
  C3_covariates = cbind(C3_covariates,c3_ego2)
  colnames(C1_covariates)[4] = "egoHD"
  colnames(C2_covariates)[4] = "egoHD"
  colnames(C3_covariates)[4] = "egoHD"
  
  O1_covariates = cbind(O1_covariates,o1_ego2)
  O2_covariates = cbind(O2_covariates,o2_ego2)
  O3_covariates = cbind(O3_covariates,o3_ego2)
  colnames(O1_covariates)[3] = "egoHD"
  colnames(O2_covariates)[3] = "egoHD"
  colnames(O3_covariates)[3] = "egoHD"
  
}#Load and adjust covariates

if (TRUE){
  
  C1_col_df = prep_colors_from_df(C1_covariates,prop=1,oldHDcolors = oldHDcolors)
  C2_col_df = prep_colors_from_df(C2_covariates,prop=1,oldHDcolors = oldHDcolors)
  C3_col_df = prep_colors_from_df(C3_covariates,prop=1,oldHDcolors = oldHDcolors)
  
  O1_col_df = prep_colors_from_df(O1_covariates,CHASE=F,prop=1,oldHDcolors = oldHDcolors)
  O2_col_df = prep_colors_from_df(O2_covariates,CHASE=F,prop=1,oldHDcolors = oldHDcolors)
  O3_col_df = prep_colors_from_df(O3_covariates,CHASE=F,prop=1,oldHDcolors = oldHDcolors)
  
  S1a_col_df = prep_colors_from_df(S1_covariates_a,CHASE=F,SOCIAL=T,prop=1,onlyspeedhd = T,oldHDcolors = oldHDcolors)
  S1b_col_df = prep_colors_from_df(S1_covariates_b,CHASE=F,SOCIAL=T,prop=1,onlyspeedhd = T,oldHDcolors = oldHDcolors)
  S1_col_df = join_list_of_arrays(S1a_col_df,S1b_col_df)
  S2_col_df = prep_colors_from_df(S2_covariates,CHASE=F,SOCIAL=T,prop=1,oldHDcolors = oldHDcolors)
  S3_col_df = prep_colors_from_df(S3_covariates,CHASE=F,SOCIAL=T,prop=1,oldHDcolors = oldHDcolors)
  
  
  if (FALSE){
    #Rearing: #9467bd
    #HB: #2ca02c
    #Inv: #d62728
    #Pursuit: #bcbd22
    #Front Paws Wall: #17becf
    #Wall Sitting: #ff7f0e
    #Wall Hanging:# #7f7f7f
    #Social Proximity: #e377c2
  }
  
  somecols_c = c("lightgray","#bcbd22","#2ca02c","#d62728","#9467bd")
  somecols_o = c("lightgray","#2ca02c","#d62728","#9467bd")
  somecols_s = c("lightgray","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e")
  somecols_ocs = c("lightgray","#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e")
  
  C1_col_df$norm = somecols_c[labels_c1+1]
  C2_col_df$norm = somecols_c[labels_c2+1]
  C3_col_df$norm = somecols_c[labels_c3+1]
  
  S1_col_df$norm = somecols_s[labels_s1+1]
  S2_col_df$norm = somecols_s[labels_s2+1]
  S3_col_df$norm = somecols_s[labels_s3+1]
  
  O1_col_df$norm = somecols_o[labels_o1+1]
  O2_col_df$norm = somecols_o[labels_o2+1]
  O3_col_df$norm = somecols_o[labels_o3+1]
  
  OCS1_col_df = list()
  OCS2_col_df = list()
  OCS3_col_df = list()
  
  for (name in c("speed","HD","norm")){
    OCS1_col_df[[name]] = c(O1_col_df[[name]],C1_col_df[[name]],S1_col_df[[name]])
    OCS2_col_df[[name]] = c(O2_col_df[[name]],C2_col_df[[name]],S2_col_df[[name]])
    OCS3_col_df[[name]] = c(O3_col_df[[name]],C3_col_df[[name]],S3_col_df[[name]])
  }
  
  OCS1_col_df$session = c(rep("#009E73",length(O1_col_df$norm)),rep("#FFD700",length(C1_col_df$norm)),rep("#CC79A7",length(S1_col_df$norm)))
  OCS2_col_df$session = c(rep("#009E73",length(O2_col_df$norm)),rep("#FFD700",length(C2_col_df$norm)),rep("#CC79A7",length(S2_col_df$norm)))
  OCS3_col_df$session = c(rep("#009E73",length(O3_col_df$norm)),rep("#FFD700",length(C3_col_df$norm)),rep("#CC79A7",length(S3_col_df$norm)))
  
  
} # full length color lists

prop = 10
sub_inds_c1 = seq(1,dim(smoothedchase1)[1],by=prop)
sub_inds_c2 = seq(1,dim(smoothedchase2)[1],by=prop)
sub_inds_c3 = seq(1,dim(smoothedchase3)[1],by=prop)

sub_inds_s1 = seq(1,dim(smoothedsocial1)[1],by=prop)
sub_inds_s2 = seq(1,dim(smoothedsocial2)[1],by=prop)
sub_inds_s3 = seq(1,dim(smoothedsocial3)[1],by=prop)

sub_inds_o1 = seq(1,dim(smoothedopen1)[1],by=prop)
sub_inds_o2 = seq(1,dim(smoothedopen2)[1],by=prop)
sub_inds_o3 = seq(1,dim(smoothedopen3)[1],by=prop)

sub_inds_ocs1 = c(sub_inds_o1,dim(smoothedopen1)[1]+sub_inds_c1,dim(smoothedopen1)[1]+dim(smoothedchase1)[1]+sub_inds_s1)
sub_inds_ocs2 = c(sub_inds_o2,dim(smoothedopen2)[1]+sub_inds_c2,dim(smoothedopen2)[1]+dim(smoothedchase2)[1]+sub_inds_s2)
sub_inds_ocs3 = c(sub_inds_o3,dim(smoothedopen3)[1]+sub_inds_c3,dim(smoothedopen3)[1]+dim(smoothedchase3)[1]+sub_inds_s3)


if (TRUE){
  
  col_dfs_C = list(col_df1=slice_list_of_arrays(C1_col_df,sub_inds_c1),col_df2=slice_list_of_arrays(C2_col_df,sub_inds_c2),col_df3=slice_list_of_arrays(C3_col_df,sub_inds_c3))
  col_dfs_O = list(col_df1=slice_list_of_arrays(O1_col_df,sub_inds_o1),col_df2=slice_list_of_arrays(O2_col_df,sub_inds_o2),col_df3=slice_list_of_arrays(O3_col_df,sub_inds_o3))
  col_dfs_S = list(col_df1=slice_list_of_arrays(S1_col_df,sub_inds_s1),col_df2=slice_list_of_arrays(S2_col_df,sub_inds_s2),col_df3=slice_list_of_arrays(S3_col_df,sub_inds_s3))
  col_dfs_OCS = list(col_df1=slice_list_of_arrays(OCS1_col_df,sub_inds_ocs1),col_df2=slice_list_of_arrays(OCS2_col_df,sub_inds_ocs2),col_df3=slice_list_of_arrays(OCS3_col_df,sub_inds_ocs3))
  
  col_df_C12 = join_list_of_arrays(col_dfs_C$col_df1,col_dfs_C$col_df2)
  col_df_C12$session = c(rep("#FFD700",length(col_dfs_C$col_df1$norm)),rep("#CC79A7",length(col_dfs_C$col_df2$norm)))
  
  col_df_O12 = join_list_of_arrays(col_dfs_O$col_df1,col_dfs_O$col_df2)
  col_df_O12$session = c(rep("#FFD700",length(col_dfs_O$col_df1$norm)),rep("#CC79A7",length(col_dfs_O$col_df2$norm)))
  
  col_df_S12 = join_list_of_arrays(col_dfs_S$col_df1,col_dfs_S$col_df2)
  col_df_S12$session = c(rep("#FFD700",length(col_dfs_S$col_df1$norm)),rep("#CC79A7",length(col_dfs_S$col_df2$norm)))
  
}
# reduced length color lists

if (TRUE){
  inputs_C = list()
  inputs_C$mat1 = as.matrix(smoothedchase1)
  inputs_C$mat2 = as.matrix(smoothedchase2)
  inputs_C$mat3 = as.matrix(smoothedchase3)
  inputs_C$sub_inds1 = sub_inds_c1
  inputs_C$sub_inds2 = sub_inds_c2
  inputs_C$sub_inds3 = sub_inds_c3
  
  inputs_S = list()
  inputs_S$mat1 = as.matrix(smoothedsocial1)
  inputs_S$mat2 = as.matrix(smoothedsocial2)
  inputs_S$mat3 = as.matrix(smoothedsocial3)
  inputs_S$sub_inds1 = sub_inds_s1
  inputs_S$sub_inds2 = sub_inds_s2
  inputs_S$sub_inds3 = sub_inds_s3
  
  inputs_O = list()
  inputs_O$mat1 = as.matrix(smoothedopen1)
  inputs_O$mat2 = as.matrix(smoothedopen2)
  inputs_O$mat3 = as.matrix(smoothedopen3)
  inputs_O$sub_inds1 = sub_inds_o1
  inputs_O$sub_inds2 = sub_inds_o2
  inputs_O$sub_inds3 = sub_inds_o3
  
  inputs_OCS = list()
  inputs_OCS$mat1 = rbind(scale(as.matrix(smoothedopen1)),scale(as.matrix(smoothedchase1)),scale(as.matrix(smoothedsocial1)))
  inputs_OCS$mat2 = rbind(scale(as.matrix(smoothedopen2)),scale(as.matrix(smoothedchase2)),scale(as.matrix(smoothedsocial2)))
  inputs_OCS$mat3 = rbind(scale(as.matrix(smoothedopen3)),scale(as.matrix(smoothedchase3)),scale(as.matrix(smoothedsocial3)))
  inputs_OCS$sub_inds1 = sub_inds_ocs1
  inputs_OCS$sub_inds2 = sub_inds_ocs2
  inputs_OCS$sub_inds3 = sub_inds_ocs3
} # session-specific inputs for UMAP

if (TRUE){
  custom.config = umap.defaults
  custom.config$metric = "cosine"#"euclidean" #"pearson"#
  custom.config$n_neighbors = 300
  custom.config$min_dist = 0.2#0.1#0.8
  custom.config$verbose = TRUE
  #custom.config$
  
  custom.config_ocs = umap.defaults
  custom.config_ocs$metric = "cosine"#"euclidean" #"pearson"#
  custom.config_ocs$n_neighbors = 100
  custom.config_ocs$min_dist = 0.1#0.1#0.8
  custom.config_ocs$verbose = TRUE
} #adjust UMAP configs

if (FALSE){
    
  #All cells
  UMAPS_from_all = make_some_umaps(inputs_C,"all",custom.config,mainmat = 2)
  UMAPS_predO_from_C = predict_UMAPs(UMAPS_from_all,inputs_O,"all")
  UMAPS_predS_from_C = predict_UMAPs(UMAPS_from_all,inputs_S,"all")
  
  #Plotting
  plot_UMAPs(UMAPS_from_all,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_predO_from_C,col_dfs_O,behaviors_o,"norm",somecols_o)
  plot_UMAPs(UMAPS_predS_from_C,col_dfs_S,behaviors_s,"norm",somecols_s)
  
  plot_UMAPs(UMAPS_from_all,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_predO_from_C,col_dfs_O,behaviors_o,"HD",somecols_o)
  plot_UMAPs(UMAPS_predS_from_C,col_dfs_S,behaviors_s,"HD",somecols_s)
  
  plot_UMAPs(UMAPS_from_all,col_dfs_C,NULL,"speed")
  plot_UMAPs(UMAPS_predO_from_C,col_dfs_O,behaviors_o,"speed",somecols_o)
  plot_UMAPs(UMAPS_predS_from_C,col_dfs_S,behaviors_s,"speed",somecols_s)
  
  #Chasing ensemble
  UMAPS_from_chasing = make_some_umaps(inputs_C,"chasing",custom.config,mainmat=2)
  UMAPS_chase_predO_from_C = predict_UMAPs(UMAPS_from_chasing,inputs_O,"chasing")
  UMAPS_chase_predS_from_C = predict_UMAPs(UMAPS_from_chasing,inputs_S,"chasing")
  
  #Plotting
  plot_UMAPs(UMAPS_from_chasing,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_chase_predO_from_C,col_dfs_O,behaviors_o,"norm",somecols_o)
  plot_UMAPs(UMAPS_chase_predS_from_C,col_dfs_S,behaviors_s,"norm",somecols_s)
  
  plot_UMAPs(UMAPS_from_chasing,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_chase_predO_from_C,col_dfs_O,behaviors_o,"HD",somecols_o)
  plot_UMAPs(UMAPS_chase_predS_from_C,col_dfs_S,behaviors_s,"HD",somecols_s)
  
  plot_UMAPs(UMAPS_from_chasing,col_dfs_C,NULL,"speed")
  plot_UMAPs(UMAPS_chase_predO_from_C,col_dfs_O,behaviors_o,"speed",somecols_o)
  plot_UMAPs(UMAPS_chase_predS_from_C,col_dfs_S,behaviors_s,"speed",somecols_s)
  
  #
  #HD ensemble
  UMAPS_from_hd = make_some_umaps(inputs_C,"hd",custom.config,mainmat=2)
  UMAPS_hd_predO_from_C = predict_UMAPs(UMAPS_from_hd,inputs_O,"hd")
  UMAPS_hd_predS_from_C = predict_UMAPs(UMAPS_from_hd,inputs_S,"hd")
  
  #Plotting
  plot_UMAPs(UMAPS_from_hd,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_hd_predO_from_C,col_dfs_O,behaviors_o,"norm",somecols_o)
  plot_UMAPs(UMAPS_hd_predS_from_C,col_dfs_S,behaviors_s,"norm",somecols_s)
  
  plot_UMAPs(UMAPS_from_hd,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_hd_predO_from_C,col_dfs_O,behaviors_o,"HD",somecols_o)
  plot_UMAPs(UMAPS_hd_predS_from_C,col_dfs_S,behaviors_s,"HD",somecols_s)
  
  #
  UMAPS_from_rearing = make_some_umaps(inputs_C,"rearing",custom.config)
  
  UMAPS_from_rearing_O = make_some_umaps(inputs_O,"rearing",custom.config)
  plot_UMAPs(UMAPS_from_rearing_O,col_dfs_O,behaviors_o,"norm",somecols_o)
  plot_UMAPs(UMAPS_from_rearing_O,col_dfs_O,NULL,"HD")
  
  UMAPS_from_headbob = make_some_umaps(inputs_C,"headbob",custom.config)
  UMAPS_from_investigate = make_some_umaps(inputs_C,"investigate",custom.config)
  UMAPS_from_hd = make_some_umaps(inputs_C,"hd",custom.config)
  
  
  
  plot_UMAPs(UMAPS_from_chasing,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_from_chasing,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_from_chasing,col_dfs_C,NULL,"speed")
  plot_UMAPs(UMAPS_from_rearing,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_from_rearing,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_from_headbob,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_from_headbob,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_from_investigate,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_from_investigate,col_dfs_C,NULL,"HD")
  plot_UMAPs(UMAPS_from_hd,col_dfs_C,behaviors_c,"norm",somecols_c)
  plot_UMAPs(UMAPS_from_hd,col_dfs_C,NULL,"HD")
  
  UMAPS_from_all_S = make_some_umaps(inputs_S,"all",custom.config)
  UMAPS_from_hd_S = make_some_umaps(inputs_S,"hd",custom.config)
  
  plot_UMAPs(UMAPS_from_all_S,col_dfs_S,behaviors_s,"norm",somecols_s)
  plot_UMAPs(UMAPS_from_all_S,col_dfs_S,NULL,"HD")
  plot_UMAPs(UMAPS_from_all_S,col_dfs_S,NULL,"speed")
  
  UMAPS_from_all_O = make_some_umaps(inputs_O,"all",custom.config)
  UMAPS_from_hd_O = make_some_umaps(inputs_O,"hd",custom.config)
  
  plot_UMAPs(UMAPS_from_all_O,col_dfs_O,behaviors_o,"norm",somecols_o)
  plot_UMAPs(UMAPS_from_all_O,col_dfs_O,NULL,"HD")
  plot_UMAPs(UMAPS_from_all_O,col_dfs_O,NULL,"speed")
  
  plot_UMAPs(UMAPS_from_hd_O,col_dfs_O,behaviors_o,"norm",somecols_o)
  plot_UMAPs(UMAPS_from_hd_O,col_dfs_O,NULL,"HD")
  
  
  UMAPS_from_all_OCS = make_some_umaps(inputs_OCS,"all",custom.config_ocs,needscaling = F,mainmat = 2)
  UMAPS_from_hd_OCS = make_some_umaps(inputs_OCS,"hd",custom.config_ocs,needscaling = F)
  plot_UMAPs(UMAPS_from_all_OCS,col_dfs_OCS,behaviors_ocs,"norm",somecols_ocs)
  plot_UMAPs(UMAPS_from_all_OCS,col_dfs_OCS,behaviors_ocs,"session",somecols_ocs)
  plot_UMAPs(UMAPS_from_all_OCS,col_dfs_OCS,behaviors_ocs,"HD",somecols_ocs)
  plot_UMAPs(UMAPS_from_all_OCS,col_dfs_OCS,behaviors_ocs,"speed",somecols_ocs)
  plot_UMAPs(UMAPS_from_hd_OCS,col_dfs_OCS,NULL,"HD")

} #Making lots of UMAPs


#### Making from a single session type, but two of that type ----

if (TRUE){
  config_c12 = umap.defaults
  config_c12$metric = "cosine"#"euclidean" #"pearson"#
  config_c12$n_neighbors = 100
  config_c12$min_dist = 0.01
  config_c12$verbose = TRUE
  #custom.config$
  
} #adjust UMAP configs

if (TRUE) {
  set.seed(1)
  UMAPS_ALL_C12 = make_one_umap(inputs_C,"all",config_c12,c(1,2))
  plot_one_UMAP(UMAPS_ALL_C12,col_df_C12,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c,dotscale=1)
  
  UMAPS_HD_C12 = make_one_umap(inputs_C,"hd",config_c12,c(1,2))
  plot_one_UMAP(UMAPS_HD_C12,col_df_C12,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  set.seed(1)
  labs_c = c(labels_c1[sub_inds_c1],labels_c2[sub_inds_c2])
  num_relevant_c = length(which(labs_c == 1))
  subsub_inds_c = sort(c(sample(which(labs_c != 1),size=num_relevant_c),which(labs_c == 1)))
  sub_col_df_C12 = slice_list_of_arrays(col_df_C12,subsub_inds_c)
  grayones = which(!(subsub_inds_c %in% which(labs_c == 1)))
  sub_col_df_C12$norm[grayones] = "lightgray"
  
  UMAPS_CHASING_C12 = make_one_umap(inputs_C,"chasing",config_c12,c(1,2),subsub_inds=subsub_inds_c)
  plot_one_UMAP(UMAPS_CHASING_C12,sub_col_df_C12,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  plot_one_UMAP(UMAPS_CHASING_C12,sub_col_df_C12,behaviors=behaviors_c,colscheme=c("norm","HD","HDderiv"),somecols=somecols_c)

  UMAPS_CHASINGHD_C12 = make_one_umap(inputs_C,"chasinghd",config_c12,c(1,2),subsub_inds=subsub_inds_c)
  plot_one_UMAP(UMAPS_CHASINGHD_C12,sub_col_df_C12,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  plot_one_UMAP(UMAPS_CHASINGHD_C12,sub_col_df_C12,behaviors=behaviors_c,colscheme=c("norm","HD","HDderiv"),somecols=somecols_c)

  set.seed(1)
  labs_c = c(labels_c1[sub_inds_c1],labels_c2[sub_inds_c2])
  num_relevant_c2 = length(which(labs_c == 2))
  subsub_inds_c2 = sort(c(sample(which(labs_c != 2),size=num_relevant_c2),which(labs_c == 2)))
  sub_col_df_C12_2 = slice_list_of_arrays(col_df_C12,subsub_inds_c2)
  grayones = which(!(subsub_inds_c2 %in% which(labs_c == 2)))
  sub_col_df_C12_2$norm[grayones] = "lightgray"
  
  UMAPS_HEADBOB_C12 = make_one_umap(inputs_C,"headbob",config_c12,c(1,2),subsub_inds=subsub_inds_c2)
  plot_one_UMAP(UMAPS_HEADBOB_C12,sub_col_df_C12_2,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  UMAPS_HEADBOBHD_C12 = make_one_umap(inputs_C,"headbobhd",config_c12,c(1,2),subsub_inds=subsub_inds_c2)
  plot_one_UMAP(UMAPS_HEADBOBHD_C12,sub_col_df_C12_2,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  
  set.seed(1)
  labs_c = c(labels_c1[sub_inds_c1],labels_c2[sub_inds_c2])
  num_relevant_c3 = length(which(labs_c == 3))
  subsub_inds_c3 = sort(c(sample(which(labs_c != 3),size=num_relevant_c3),which(labs_c == 3)))
  sub_col_df_C12_3 = slice_list_of_arrays(col_df_C12,subsub_inds_c3)
  grayones = which(!(subsub_inds_c3 %in% which(labs_c == 3)))
  sub_col_df_C12_3$norm[grayones] = "lightgray"
  
  UMAPS_INVESTIGATE_C12 = make_one_umap(inputs_C,"investigate",config_c12,c(1,2),subsub_inds=subsub_inds_c3)
  plot_one_UMAP(UMAPS_INVESTIGATE_C12,sub_col_df_C12_3,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  UMAPS_INVESTIGATEHD_C12 = make_one_umap(inputs_C,"investigatehd",config_c12,c(1,2),subsub_inds=subsub_inds_c3)
  plot_one_UMAP(UMAPS_INVESTIGATEHD_C12,sub_col_df_C12_3,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  
  set.seed(1)
  labs_c = c(labels_c1[sub_inds_c1],labels_c2[sub_inds_c2])
  num_relevant_c4 = length(which(labs_c == 4))
  subsub_inds_c4 = sort(c(sample(which(labs_c != 4),size=num_relevant_c4),which(labs_c == 4)))
  sub_col_df_C12_4 = slice_list_of_arrays(col_df_C12,subsub_inds_c4)
  grayones = which(!(subsub_inds_c4 %in% which(labs_c == 4)))
  sub_col_df_C12_4$norm[grayones] = "lightgray"
  
  UMAPS_REARING_C12 = make_one_umap(inputs_C,"rearing",config_c12,c(1,2),subsub_inds=subsub_inds_c4)
  plot_one_UMAP(UMAPS_REARING_C12,sub_col_df_C12_4,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  UMAPS_REARINGHD_C12 = make_one_umap(inputs_C,"rearinghd",config_c12,c(1,2),subsub_inds=subsub_inds_c4)
  plot_one_UMAP(UMAPS_REARINGHD_C12,sub_col_df_C12_4,behaviors=behaviors_c,colscheme=c("norm","HD","session"),somecols=somecols_c)
  
  
} #Chasing UMAPs

if (TRUE) {
  set.seed(1)
  UMAPS_ALL_O12 = make_one_umap(inputs_O,"all",config_c12,c(1,2))
  plot_one_UMAP(UMAPS_ALL_O12,col_df_O12,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  UMAPS_HD_O12 = make_one_umap(inputs_O,"hd",config_c12,c(1,2))
  plot_one_UMAP(UMAPS_HD_O12,col_df_O12,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  set.seed(1)
  labs_o = c(labels_o1[sub_inds_o1],labels_o2[sub_inds_o2])
  num_relevant_o = length(which(labs_o == 3))
  subsub_inds_o = sort(c(sample(which(labs_o != 3),size=num_relevant_o),which(labs_o == 3)))
  sub_col_df_O12 = slice_list_of_arrays(col_df_O12,subsub_inds_o)
  grayones = which(!(subsub_inds_o %in% which(labs_o == 3)))
  sub_col_df_O12$norm[grayones] = "lightgray"
  
  UMAPS_REARING_O12 = make_one_umap(inputs_O,"rearing",config_c12,c(1,2),subsub_inds=subsub_inds_o)
  plot_one_UMAP(UMAPS_REARING_O12,sub_col_df_O12,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  UMAPS_REARINGHD_O12 = make_one_umap(inputs_O,"rearinghd",config_c12,c(1,2),subsub_inds=subsub_inds_o)
  plot_one_UMAP(UMAPS_REARINGHD_O12,sub_col_df_O12,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  set.seed(1)
  num_relevant_o2 = length(which(labs_o == 2))
  subsub_inds_o2 = sort(c(sample(which(labs_o != 2),size=num_relevant_o2),which(labs_o == 2)))
  sub_col_df_O12_2 = slice_list_of_arrays(col_df_O12,subsub_inds_o2)
  grayones = which(!(subsub_inds_o2 %in% which(labs_o == 2)))
  sub_col_df_O12_2$norm[grayones] = "lightgray"
  
  UMAPS_INVESTIGATE_O12 = make_one_umap(inputs_O,"investigate",config_c12,c(1,2),subsub_inds=subsub_inds_o2)
  plot_one_UMAP(UMAPS_INVESTIGATE_O12,sub_col_df_O12_2,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  UMAPS_INVESTIGATEHD_O12 = make_one_umap(inputs_O,"investigatehd",config_c12,c(1,2),subsub_inds=subsub_inds_o2)
  plot_one_UMAP(UMAPS_INVESTIGATEHD_O12,sub_col_df_O12_2,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  set.seed(1)
  num_relevant_o3 = length(which(labs_o == 1))
  subsub_inds_o3 = sort(c(sample(which(labs_o != 1),size=num_relevant_o3),which(labs_o == 1)))
  sub_col_df_O12_3 = slice_list_of_arrays(col_df_O12,subsub_inds_o3)
  grayones = which(!(subsub_inds_o3 %in% which(labs_o == 1)))
  sub_col_df_O12_3$norm[grayones] = "lightgray"
  
  UMAPS_HEADBOB_O12 = make_one_umap(inputs_O,"headbob",config_c12,c(1,2),subsub_inds=subsub_inds_o3)
  plot_one_UMAP(UMAPS_HEADBOB_O12,sub_col_df_O12_3,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  UMAPS_HEADBOBHD_O12 = make_one_umap(inputs_O,"headbobhd",config_c12,c(1,2),subsub_inds=subsub_inds_o3)
  plot_one_UMAP(UMAPS_HEADBOBHD_O12,sub_col_df_O12_3,behaviors=behaviors_o,colscheme=c("norm","HD","session"),somecols=somecols_o)
  
  
} #Open UMAPs

if (TRUE) {
  set.seed(1)
  UMAPS_ALL_S12 = make_one_umap(inputs_S,"all",config_c12,c(1,2))
  plot_one_UMAP(UMAPS_ALL_S12,col_df_S12,behaviors=behaviors_s,colscheme=c("norm","HD","session"),somecols=somecols_s)
  
  UMAPS_HD_S12 = make_one_umap(inputs_S,"hd",config_c12,c(1,2))
  plot_one_UMAP(UMAPS_HD_S12,col_df_S12,behaviors=behaviors_s,colscheme=c("norm","HD","session"),somecols=somecols_s)
  
  set.seed(1)
  labs_s = c(labels_s1[sub_inds_s1],labels_s2[sub_inds_s2])
  num_relevant_s = length(which(labs_s == 2))
  subsub_inds_s = sort(c(sample(which(labs_s != 2),size=num_relevant_s),which(labs_s == 2)))
  sub_col_df_S12 = slice_list_of_arrays(col_df_S12,subsub_inds_s)
  grayones = which(!(subsub_inds_s %in% which(labs_s == 2)))
  sub_col_df_S12$norm[grayones] = "lightgray"
  
  UMAPS_INVESTIGATE_S12 = make_one_umap(inputs_S,"investigate",config_c12,c(1,2),subsub_inds=subsub_inds_s)
  plot_one_UMAP(UMAPS_INVESTIGATE_S12,sub_col_df_S12,behaviors=behaviors_s,colscheme=c("norm","HD","session"),somecols=somecols_s)
  
  UMAPS_INVESTIGATEHD_S12 = make_one_umap(inputs_S,"investigatehd",config_c12,c(1,2),subsub_inds=subsub_inds_s)
  plot_one_UMAP(UMAPS_INVESTIGATEHD_S12,sub_col_df_S12,behaviors=behaviors_s,colscheme=c("norm","HD","session"),somecols=somecols_s)
  
  
} #Social UMAPs

#

#####