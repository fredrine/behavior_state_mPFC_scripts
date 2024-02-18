wdir = "/Users/fredrine/Documents/Tritask/R_scripts/" #Replace with path to repo
animal = "Diana" # "Leia" # "Rey"
#### Load functions ----
source(paste(wdir, "functions.R",sep=""))
#### Load binned data ----
# Load spikes binned at 50 ms

binnedo1 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Open1.RDS",sep=""))
binnedo2 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Open2.RDS",sep=""))
binnedo3 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Open3.RDS",sep=""))
binnedc1 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Chasing1.RDS",sep=""))
binnedc2 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Chasing2.RDS",sep=""))
binnedc3 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Chasing3.RDS",sep=""))
binneds1 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Social1.RDS",sep=""))
binneds2 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Social2.RDS",sep=""))
binneds3 = readRDS(paste(wdir, "Spike data/", animal, "/Binned_spikes_50ms_Social3.RDS",sep=""))

#### Smooth spike trains and save results ----

sd = 10 # 5

smoothedo1 = smoothcols(binnedo1,sd)
smoothedo2 = smoothcols(binnedo2,sd)
smoothedo3 = smoothcols(binnedo3,sd)

smoothedc1 = smoothcols(binnedc1,sd)
smoothedc2 = smoothcols(binnedc2,sd)
smoothedc3 = smoothcols(binnedc3,sd)

smootheds1 = smoothcols(binneds1,sd)
smootheds2 = smoothcols(binneds2,sd)
smootheds3 = smoothcols(binneds3,sd)


saveRDS(smoothedo1, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open1_sd",sd,"bins.RDS",sep=""))
saveRDS(smoothedo2, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open2_sd",sd,"bins.RDS",sep=""))
saveRDS(smoothedo3, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open3_sd",sd,"bins.RDS",sep=""))
saveRDS(smoothedc1, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing1_sd",sd,"bins.RDS",sep=""))
saveRDS(smoothedc2, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing2_sd",sd,"bins.RDS",sep=""))
saveRDS(smoothedc3, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing3_sd",sd,"bins.RDS",sep=""))
saveRDS(smootheds1, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social1_sd",sd,"bins.RDS",sep=""))
saveRDS(smootheds2, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social2_sd",sd,"bins.RDS",sep=""))
saveRDS(smootheds3, paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social3_sd",sd,"bins.RDS",sep=""))

#### Make and save quantile matrices ----
# For each spike train, its smoothed values are replaced with the corresponding quantile of
# the distribution of smoothed values for that cell, resulting in a vector where all
# values between 0 and 1 are evenly represented (similar to normalizing / z-scoring)
# Used for the fingerprint similarity / population vector decoding

sd = 10

smoothedo1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open1_sd",sd,"bins.RDS",sep=""))
smoothedo2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open2_sd",sd,"bins.RDS",sep=""))
smoothedo3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Open3_sd",sd,"bins.RDS",sep=""))
smoothedc1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing1_sd",sd,"bins.RDS",sep=""))
smoothedc2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing2_sd",sd,"bins.RDS",sep=""))
smoothedc3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Chasing3_sd",sd,"bins.RDS",sep=""))
smootheds1 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social1_sd",sd,"bins.RDS",sep=""))
smootheds2 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social2_sd",sd,"bins.RDS",sep=""))
smootheds3 = readRDS(paste(wdir, "Spike data/", animal, "/Smoothed_spikes_50ms_Social3_sd",sd,"bins.RDS",sep=""))

quantile_mat_o1 = quantile_of_smoothed_spike_train(smoothedo1)
quantile_mat_o2 = quantile_of_smoothed_spike_train(smoothedo2)
quantile_mat_o3 = quantile_of_smoothed_spike_train(smoothedo3)

quantile_mat_c1_ = quantile_of_smoothed_spike_train(smoothedc1)
quantile_mat_c2 = quantile_of_smoothed_spike_train(smoothedc2)
quantile_mat_c3 = quantile_of_smoothed_spike_train(smoothedc3)

quantile_mat_s1 = quantile_of_smoothed_spike_train(smootheds1)
quantile_mat_s2 = quantile_of_smoothed_spike_train(smootheds2)
quantile_mat_s3 = quantile_of_smoothed_spike_train(smootheds3)

saveRDS(quantile_mat_o1, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Open1.RDS",sep=""))
saveRDS(quantile_mat_o2, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Open2.RDS",sep=""))
saveRDS(quantile_mat_o3, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Open3.RDS",sep=""))
saveRDS(quantile_mat_c1, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Chasing1.RDS",sep=""))
saveRDS(quantile_mat_c2, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Chasing2.RDS",sep=""))
saveRDS(quantile_mat_c3, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Chasing3.RDS",sep=""))
saveRDS(quantile_mat_s1, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Social1.RDS",sep=""))
saveRDS(quantile_mat_s2, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Social2.RDS",sep=""))
saveRDS(quantile_mat_s3, paste(wdir, "Spike data/", animal, "/Quantile_matrix_Social3.RDS",sep=""))

#####