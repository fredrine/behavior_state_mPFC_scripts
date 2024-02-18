wdir = "/Users/fredrine/Documents/Tritask/R_scripts/" #Replace with path to repo
animal = "Diana" # "Leia" # "Rey"
#### Load functions ----
source(paste(wdir, "functions.R",sep=""))

#### Average population vector (fingerprint) over behaviors ----

if (animal == "Diana"){DIANA = TRUE} else {DIANA = FALSE}
if (animal == "Leia"){LEIA = TRUE} else {LEIA = FALSE}
if (animal == "Rey"){REY = TRUE} else {REY = FALSE}

if (TRUE){
  scale = 6
  if (DIANA){
    rearing_inds_c1 = make_index_vector("chasing1_diana_rearing_merged.csv",scale)
    rearing_inds_c2 = make_index_vector("chasing2_diana_rearing_merged.csv",scale)
    rearing_inds_c3 = make_index_vector("chasing3_diana_rearing_merged.csv",scale)
    
    rearing_inds_o1 = make_index_vector("open1_diana_rearing_merged.csv",scale)
    rearing_inds_o2 = make_index_vector("open2_diana_rearing_merged.csv",scale)
    rearing_inds_o3 = make_index_vector("open3_diana_rearing_merged.csv",scale)
    
    rearing_inds_s1 = make_index_vector("social111_diana_rearing_adjusted.csv",scale)
    rearing_inds_s2 = make_index_vector("social2_diana_rearing_adjusted_pred.csv",scale)
    rearing_inds_s3 = make_index_vector("social3_diana_rearing_adjusted_pred.csv",scale)
    
    headbobb_inds_c1 = make_index_vector("chasing1_diana_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_c2 = make_index_vector("chasing2_diana_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_c3 = make_index_vector("chasing3_diana_head_bobb_adjusted_pred.csv",scale)
    
    #headbobb_inds_o1 = make_index_vector("open1_diana_head_bobb_pred_adjusted_new.csv",scale)
    headbobb_inds_o1 = make_index_vector("open1_diana_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_o2 = make_index_vector("open2_diana_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_o3 = make_index_vector("open3_diana_head_bobb_adjusted_pred.csv",scale)
    
    headbobb_inds_s1 = make_index_vector("social111_diana_head_bobbing_sitting_adjusted.csv",scale)
    headbobb_inds_s2 = make_index_vector("social2_diana_head_bobbing_sitting_adjusted_pred.csv",scale)
    headbobb_inds_s3 = make_index_vector("social3_diana_head_bobbing_sitting_adjusted_pred.csv",scale)
    
    investigate_inds_c1 = make_index_vector("chasing1_diana_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_c2 = make_index_vector("chasing2_diana_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_c3 = make_index_vector("chasing3_diana_investigation_floor_wall_adjusted_pred.csv",scale)
    
    #investigate_inds_o1 = make_index_vector("open1_diana_investigation_floor_wall_adjusted_pred_new.csv",scale)
    investigate_inds_o1 = make_index_vector("open1_diana_investigation_floor_wall_adjusted_pred.csv",scale)
    #investigate_inds_o2 = make_index_vector("open2_diana_investigation_floor_wall_adjusted_pred_new.csv",scale)
    investigate_inds_o2 = make_index_vector("open2_diana_investigation_floor_wall_adjusted_pred.csv",scale)
    #investigate_inds_o3 = make_index_vector("open3_diana_investigation_floor_wall_pred_adjust_new.csv",scale)
    investigate_inds_o3 = make_index_vector("open3_diana_investigation_floor_wall_adjusted_pred.csv",scale)
    
    investigate_inds_s1 = make_index_vector("social111_diana_floor_investigation_adjusted.csv",scale)
    investigate_inds_s2 = make_index_vector("social2_diana_floor_investigation_adjusted_pred.csv",scale)
    investigate_inds_s3 = make_index_vector("social3_diana_floor_investigation_adjusted_pred.csv",scale)
    
    front_paw_wall_inds_s1 = make_index_vector("social111_diana_front_paws_wall_adjusted.csv",scale)
    front_paw_wall_inds_s2 = make_index_vector("social2_diana_front_paws_wall_adjusted_pred.csv",scale)
    front_paw_wall_inds_s3 = make_index_vector("social3_diana_front_paws_wall_adjusted_pred.csv",scale)
    
    hang_from_wall_inds_s1 = make_index_vector("social111_diana_hanging_from_wall_org.csv",scale)
    hang_from_wall_inds_s2 = make_index_vector("social2_diana_hanging_from_wall_adjusted_pred.csv",scale)
    hang_from_wall_inds_s3 = make_index_vector("social3_diana_hanging_from_wall_adjusted_pred.csv",scale)
    
    sit_on_wall_inds_s1 = make_index_vector("social111_diana_sitting_on_wall_adjusted.csv",scale)
    sit_on_wall_inds_s2 = make_index_vector("social2_diana_sitting_on_wall_adjusted_pred.csv",scale)
    sit_on_wall_inds_s3 = make_index_vector("social3_diana_sitting_on_wall_adjusted_pred.csv",scale)
  } else if (LEIA){
    rearing_inds_c1 = make_index_vector("leia/chasing1_leia_rearing_adjusted_pred.csv",scale)
    rearing_inds_c2 = c()
    rearing_inds_c3 = make_index_vector("leia/chasing3_leia_rearing_merged.csv",scale)
    
    rearing_inds_o1 = make_index_vector("leia/open1_leia_rearing_adjusted_pred.csv",scale)
    rearing_inds_o2 = make_index_vector("leia/open2_leia_rearing_adjusted_pred.csv",scale)
    rearing_inds_o3 = make_index_vector("leia/open3_leia_rearing_merged.csv",scale)
    
    rearing_inds_s1 = make_index_vector("leia/social1_leia_rearing_adjusted_pred.csv",scale)
    rearing_inds_s2 = make_index_vector("leia/social2_leia_rearing_adjusted_pred.csv",scale)
    rearing_inds_s3 = make_index_vector("leia/social3_leia_rearing_adjusted_pred.csv",scale)
    
    headbobb_inds_c1 = make_index_vector("leia/chasing1_leia_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_c2 = make_index_vector("leia/chasing2_leia_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_c3 = make_index_vector("leia/chasing3_leia_head_bobb_adjusted_pred.csv",scale)
    
    headbobb_inds_o1 = make_index_vector("leia/open1_leia_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_o2 = make_index_vector("leia/open2_leia_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_o3 = make_index_vector("leia/open3_leia_head_bobb_adjusted_pred.csv",scale)
    
    headbobb_inds_s1 = make_index_vector("leia/social1_leia_head_bobbing_sitting_adjusted_pred.csv",scale)
    headbobb_inds_s2 = make_index_vector("leia/social2_leia_head_bobbing_sitting_adjusted_pred.csv",scale)
    headbobb_inds_s3 = make_index_vector("leia/social3_leia_head_bobbing_sitting_adjusted_pred.csv",scale)
    
    investigate_inds_c1 = make_index_vector("leia/chasing1_leia_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_c2 = make_index_vector("leia/chasing2_leia_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_c3 = make_index_vector("leia/chasing3_leia_investigation_floor_wall_adjusted_pred.csv",scale)
    
    investigate_inds_o1 = make_index_vector("leia/open1_leia_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_o2 = make_index_vector("leia/open2_leia_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_o3 = make_index_vector("leia/open3_leia_investigation_floor_wall_adjusted_pred.csv",scale)
    
    investigate_inds_s1 = make_index_vector("leia/social1_leia_floor_investigation_adjusted_pred.csv",scale)
    investigate_inds_s2 = make_index_vector("leia/social2_leia_floor_investigation_adjusted_pred.csv",scale)
    investigate_inds_s3 = make_index_vector("leia/social3_leia_floor_investigation_adjusted_pred.csv",scale)
    
    front_paw_wall_inds_s1 = make_index_vector("leia/social1_leia_front_paws_wall_adjusted_pred.csv",scale)
    front_paw_wall_inds_s2 = make_index_vector("leia/social2_leia_front_paws_wall_adjusted_pred.csv",scale)
    front_paw_wall_inds_s3 = make_index_vector("leia/social3_leia_front_paws_wall_adjusted_pred.csv",scale)
    
    hang_from_wall_inds_s1 = make_index_vector("leia/social1_leia_hanging_from_wall_org_pred.csv",scale)
    hang_from_wall_inds_s2 = make_index_vector("leia/social2_leia_hanging_from_wall_org_pred.csv",scale)
    hang_from_wall_inds_s3 = make_index_vector("leia/social3_leia_hanging_from_wall_adjusted_pred.csv",scale)
    
    sit_on_wall_inds_s1 = c()
    sit_on_wall_inds_s2 = make_index_vector("leia/social2_leia_sitting_on_wall_adjusted_pred.csv",scale)
    sit_on_wall_inds_s3 = make_index_vector("leia/social3_leia_sitting_on_wall_adjusted_pred.csv",scale)
  } else if (REY){
    rearing_inds_c1 = c()
    rearing_inds_c2 = c()
    rearing_inds_c3 = make_index_vector("rey/chasing3_rey_rearing_merged.csv",scale)
    
    rearing_inds_o1 = c()
    rearing_inds_o2 = c()
    rearing_inds_o3 = make_index_vector("rey/open3_rey_rearing_merged_pred.csv",scale)
    
    rearing_inds_s1 = make_index_vector("rey/social1_rey_rearing_adjusted_pred.csv",scale)
    rearing_inds_s2 = make_index_vector("rey/social2_rey_rearing_adjusted_pred.csv",scale)
    rearing_inds_s3 = make_index_vector("rey/social3_rey_rearing_adjusted_pred.csv",scale)
    
    headbobb_inds_c1 = make_index_vector("rey/chasing1_rey_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_c2 = make_index_vector("rey/chasing2_rey_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_c3 = make_index_vector("rey/chasing3_rey_head_bobb_adjusted_pred.csv",scale)
    
    headbobb_inds_o1 = make_index_vector("rey/open1_rey_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_o2 = make_index_vector("rey/open2_rey_head_bobb_adjusted_pred.csv",scale)
    headbobb_inds_o3 = make_index_vector("rey/open3_rey_head_bobb_adjusted_pred.csv",scale)
    
    headbobb_inds_s1 = make_index_vector("rey/social1_rey_head_bobbing_sitting_adjusted_pred.csv",scale)
    headbobb_inds_s2 = make_index_vector("rey/social2_rey_head_bobbing_sitting_adjusted_pred.csv",scale)
    headbobb_inds_s3 = make_index_vector("rey/social3_rey_head_bobbing_sitting_adjusted_pred.csv",scale)
    
    investigate_inds_c1 = make_index_vector("rey/chasing1_rey_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_c2 = make_index_vector("rey/chasing2_rey_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_c3 = make_index_vector("rey/chasing3_rey_investigation_floor_wall_adjusted_pred.csv",scale)
    
    investigate_inds_o1 = make_index_vector("rey/open1_rey_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_o2 = make_index_vector("rey/open2_rey_investigation_floor_wall_adjusted_pred.csv",scale)
    investigate_inds_o3 = make_index_vector("rey/open3_rey_investigation_floor_wall_adjusted_pred.csv",scale)
    
    investigate_inds_s1 = make_index_vector("rey/social1_rey_floor_investigation_adjusted_pred.csv",scale)
    investigate_inds_s2 = make_index_vector("rey/social2_rey_floor_investigation_adjusted_pred.csv",scale)
    investigate_inds_s3 = make_index_vector("rey/social3_rey_floor_investigation_adjusted_pred.csv",scale)
    
    front_paw_wall_inds_s1 = make_index_vector("rey/social1_rey_front_paws_wall_adjusted_pred.csv",scale)
    front_paw_wall_inds_s2 = make_index_vector("rey/social2_rey_front_paws_wall_adjusted_pred.csv",scale)
    front_paw_wall_inds_s3 = make_index_vector("rey/social3_rey_front_paws_wall_adjusted_pred.csv",scale)
    
    hang_from_wall_inds_s1 = make_index_vector("rey/social1_rey_hanging_merged.csv",scale)
    hang_from_wall_inds_s2 = make_index_vector("rey/social2_rey_hanging_merged.csv",scale)
    hang_from_wall_inds_s3 = make_index_vector("rey/social3_rey_hanging_merged.csv",scale)
    
    sit_on_wall_inds_s1 = c()
    sit_on_wall_inds_s2 = c()
    sit_on_wall_inds_s3 = c()
  }
} # Make vectors of indices for each behavior


# Chasing intervals + quantile matrices
#Diana
if (DIANA){
  interval_starts1 = ceiling(c(3101,3832,5954,12333,17734,25935,33609,39079,47425,57788,59040,65761,70904,72264)/6)
  interval_ends1 = floor(c(3300,4038,6987,12868,19745,27881,34159,40349,49173,58835,59427,67154,71843,72381)/6)
  interval_starts2 = ceiling(c(2078,6463,9611,13176,19958,26689,35943,44200,51352,57419,66133)/6)
  interval_ends2 = floor(c(2480,8844,9976,14393,21800,27260,37894,46127,53375,58907,67040)/6)
  interval_starts3 = ceiling(c(2137,8733,9230,19543,27649,33754,40991,49456,55799,62457,69612)/6)
  interval_ends3 = floor(c(3175,9047,9816,21471,28349,34985,41330,50712,56306,64004,71165)/6)
}

#Leia
if (LEIA){
  interval_starts1 = ceiling(c(1866,13355,27606,43755,55140,66527)/6)
  interval_ends1 = floor(c(3745,14665,29309,45122,56821,67830)/6)
  interval_starts2 = ceiling(c(1517,9849,11045,17398,25783,27953,39139,48554,55957,65844)/6)
  interval_ends2 = floor(c(2297,10874,11434,18384,26797,29695,39824,49481,57310,67034)/6)
  interval_starts3 = ceiling(c(1247,8094,16586,19641,23632,28367,31514,36567,40978,46298,50806,57394,60594,61130,71544)/6)
  interval_ends3 = floor(c(2094,9484,17181,20919,24454,29751,32468,38244,41576,47262,52535,58614,61072,62166,71875)/6)
}

#Rey
if (REY){
  interval_starts1 = ceiling(c(6106,10952,25277,27205,35205,49340,64395, 67135,70021,70540)/6)
  interval_ends1 = floor(c(6457,11165,26362,28329,35265,50042,65300,68046,70139,70777)/6)
  interval_starts2 = ceiling(c(4484,5149,25803,27317,40394,56308,65320,71526)/6)
  interval_ends2 = floor(c(4936,5622,26850,28072,43099,57079,65987,71712)/6)
  interval_starts3 = ceiling(c(2371,5186,48956,49927)/6)
  interval_ends3 = floor(c(4440,6145,49316,50274)/6)
}

quantile_mat_o1 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Open1.RDS",sep=""))
quantile_mat_o2 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Open2.RDS",sep=""))
quantile_mat_o3 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Open3.RDS",sep=""))
quantile_mat_c1 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Chasing1.RDS",sep=""))
quantile_mat_c2 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Chasing2.RDS",sep=""))
quantile_mat_c3 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Chasing3.RDS",sep=""))
quantile_mat_s1 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Social1.RDS",sep=""))
quantile_mat_s2 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Social2.RDS",sep=""))
quantile_mat_s3 = readRDS(paste(wdir, "Spike data/", animal, "/Quantile_matrix_Social3.RDS",sep=""))

chasing_inds_c1 = c()
for (i in 1:length(interval_starts1)){
  chasing_inds_c1 = c(chasing_inds_c1,interval_starts1[i]:interval_ends1[i])
}
chasing_inds_c2 = c()
for (i in 1:length(interval_starts2)){
  chasing_inds_c2 = c(chasing_inds_c2,interval_starts2[i]:interval_ends2[i])
}
chasing_inds_c3 = c()
for (i in 1:length(interval_starts3)){
  chasing_inds_c3 = c(chasing_inds_c3,interval_starts3[i]:interval_ends3[i])
}

inds_for_c1 = list(chasing=chasing_inds_c1,headbobb=headbobb_inds_c1,investigate=investigate_inds_c1,rearing=rearing_inds_c1)
inds_for_c2 = list(chasing=chasing_inds_c2,headbobb=headbobb_inds_c2,investigate=investigate_inds_c2,rearing=rearing_inds_c2)
inds_for_c3 = list(chasing=chasing_inds_c3,headbobb=headbobb_inds_c3,investigate=investigate_inds_c3,rearing=rearing_inds_c3)

inds_for_o1 = list(headbobb=headbobb_inds_o1,investigate=investigate_inds_o1,rearing=rearing_inds_o1)
inds_for_o2 = list(headbobb=headbobb_inds_o2,investigate=investigate_inds_o2,rearing=rearing_inds_o2)
inds_for_o3 = list(headbobb=headbobb_inds_o3,investigate=investigate_inds_o3,rearing=rearing_inds_o3)

inds_for_s1 = list(headbobb=headbobb_inds_s1,investigate=investigate_inds_s1,rearing=rearing_inds_s1,front_paw_wall=front_paw_wall_inds_s1,hang_from_wall=hang_from_wall_inds_s1,sit_on_wall=sit_on_wall_inds_s1)
inds_for_s2 = list(headbobb=headbobb_inds_s2,investigate=investigate_inds_s2,rearing=rearing_inds_s2,front_paw_wall=front_paw_wall_inds_s2,hang_from_wall=hang_from_wall_inds_s2,sit_on_wall=sit_on_wall_inds_s2)
inds_for_s3 = list(headbobb=headbobb_inds_s3,investigate=investigate_inds_s3,rearing=rearing_inds_s3,front_paw_wall=front_paw_wall_inds_s3,hang_from_wall=hang_from_wall_inds_s3,sit_on_wall=sit_on_wall_inds_s3)


behaviors_c = c("chasing","headbobb","investigate","rearing")
behaviors_o = c("headbobb","investigate","rearing")
behaviors_s = c("headbobb","investigate","rearing","front_paw_wall","hang_from_wall","sit_on_wall")


individual_binary = 0
binary = 0.5

average_vec_c1 = list()
average_vec_c2 = list()
average_vec_c3 = list()

for (b in behaviors_c){
  average_vec_c1[[b]] = average_population_vector(quantile_mat_c1,inds_for_c1[[b]])
  average_vec_c2[[b]] = average_population_vector(quantile_mat_c2,inds_for_c2[[b]])
  average_vec_c3[[b]] = average_population_vector(quantile_mat_c3,inds_for_c3[[b]])
}


average_vec_o1 = list()
average_vec_o2 = list()
average_vec_o3 = list()

for (b in behaviors_o){
  average_vec_o1[[b]] = average_population_vector(quantile_mat_o1,inds_for_o1[[b]])
  average_vec_o2[[b]] = average_population_vector(quantile_mat_o2,inds_for_o2[[b]])
  average_vec_o3[[b]] = average_population_vector(quantile_mat_o3,inds_for_o3[[b]])
}

average_vec_s1 = list()
average_vec_s2 = list()
average_vec_s3 = list()

for (b in behaviors_s){
  average_vec_s1[[b]] = average_population_vector(quantile_mat_s1,inds_for_s1[[b]])
  average_vec_s2[[b]] = average_population_vector(quantile_mat_s2,inds_for_s2[[b]])
  average_vec_s3[[b]] = average_population_vector(quantile_mat_s3,inds_for_s3[[b]])
}

#### Fingerprint similarity ----
labels_o1 = true_labels(dim(quantile_mat_o1)[1],behaviors_o,inds_for_o1)
labels_o2 = true_labels(dim(quantile_mat_o2)[1],behaviors_o,inds_for_o2)
labels_o3 = true_labels(dim(quantile_mat_o3)[1],behaviors_o,inds_for_o3)

labels_s1 = true_labels(dim(quantile_mat_s1)[1],behaviors_s,inds_for_s1)
labels_s2 = true_labels(dim(quantile_mat_s2)[1],behaviors_s,inds_for_s2)
labels_s3 = true_labels(dim(quantile_mat_s3)[1],behaviors_s,inds_for_s3)

labels_c1 = true_labels(dim(quantile_mat_c1)[1],behaviors_c,inds_for_c1)
labels_c2 = true_labels(dim(quantile_mat_c2)[1],behaviors_c,inds_for_c2)
labels_c3 = true_labels(dim(quantile_mat_c3)[1],behaviors_c,inds_for_c3)

t_c1 = table(labels_c1)
t_c2 = table(labels_c2)
t_c3 = table(labels_c3)
t_s1 = table(labels_s1)
t_s2 = table(labels_s2)
t_s3 = table(labels_s3)
t_o1 = table(labels_o1)
t_o2 = table(labels_o2)
t_o3 = table(labels_o3)

if (LEIA){
  t_c2[5] = 0
  names(t_c2)[5]="4"
  t_s1[7] = 0
  names(t_s1)[7]="6"
} else if (REY){
  t_c1[5] = 0
  names(t_c1)[5]="4"
  t_c2[5] = 0
  names(t_c2)[5]="4"
  t_o1[4] = 0
  names(t_o1)[4]="3"
  t_o2[4] = 0
  names(t_o2)[4]="3"
  t_s1[7] = 0
  names(t_s1)[7]="6"
  t_s2[7] = 0
  names(t_s2)[7]="6"
  t_s3[7] = 0
  names(t_s3)[7]="6"
}

merged_average_c23 = merge_average_vector_sets(list(average_vec_c2,average_vec_c3),rbind(t_c2,t_c3))
merged_average_c13 = merge_average_vector_sets(list(average_vec_c1,average_vec_c3),rbind(t_c1,t_c3))
merged_average_c12 = merge_average_vector_sets(list(average_vec_c1,average_vec_c2),rbind(t_c1,t_c2))

merged_average_o23 = merge_average_vector_sets(list(average_vec_o2,average_vec_o3),rbind(t_o2,t_o3))
merged_average_o13 = merge_average_vector_sets(list(average_vec_o1,average_vec_o3),rbind(t_o1,t_o3))
merged_average_o12 = merge_average_vector_sets(list(average_vec_o1,average_vec_o2),rbind(t_o1,t_o2))

merged_average_s23 = merge_average_vector_sets(list(average_vec_s2,average_vec_s3),rbind(t_s2,t_s3))
merged_average_s13 = merge_average_vector_sets(list(average_vec_s1,average_vec_s3),rbind(t_s1,t_s3))
merged_average_s12 = merge_average_vector_sets(list(average_vec_s1,average_vec_s2),rbind(t_s1,t_s2))

correlations_c1 = vector_similarity_session(quantile_mat_c1,1:dim(quantile_mat_c1)[1],behaviors_c,merged_average_c23,method="correlation")
correlations_c2 = vector_similarity_session(quantile_mat_c2,1:dim(quantile_mat_c2)[1],behaviors_c,merged_average_c13,method="correlation")
correlations_c3 = vector_similarity_session(quantile_mat_c3,1:dim(quantile_mat_c3)[1],behaviors_c,merged_average_c12,method="correlation")

correlations_s1 = vector_similarity_session(quantile_mat_s1,1:dim(quantile_mat_s1)[1],behaviors_s,merged_average_s23,method="correlation")
correlations_s2 = vector_similarity_session(quantile_mat_s2,1:dim(quantile_mat_s2)[1],behaviors_s,merged_average_s13,method="correlation")
correlations_s3 = vector_similarity_session(quantile_mat_s3,1:dim(quantile_mat_s3)[1],behaviors_s,merged_average_s12,method="correlation")

correlations_o1 = vector_similarity_session(quantile_mat_o1,1:dim(quantile_mat_o1)[1],behaviors_o,merged_average_o23,method="correlation")
correlations_o2 = vector_similarity_session(quantile_mat_o2,1:dim(quantile_mat_o2)[1],behaviors_o,merged_average_o13,method="correlation")
correlations_o3 = vector_similarity_session(quantile_mat_o3,1:dim(quantile_mat_o3)[1],behaviors_o,merged_average_o12,method="correlation")


thresholds_o1 = classification_glm_thresholds(behaviors_o,merged_average_o23,quantile_mat_o2,quantile_mat_o3,labels_o2,labels_o3)#,average_vec_o2,average_vec_o3)
thresholds_o2 = classification_glm_thresholds(behaviors_o,merged_average_o13,quantile_mat_o1,quantile_mat_o3,labels_o1,labels_o3)#,average_vec_o1,average_vec_o3)
thresholds_o3 = classification_glm_thresholds(behaviors_o,merged_average_o12,quantile_mat_o1,quantile_mat_o2,labels_o1,labels_o2)#,average_vec_o1,average_vec_o2)
thresholds_c1 = classification_glm_thresholds(behaviors_c,merged_average_c23,quantile_mat_c2,quantile_mat_c3,labels_c2,labels_c3)#,average_vec_c2,average_vec_c3)
thresholds_c2 = classification_glm_thresholds(behaviors_c,merged_average_c13,quantile_mat_c1,quantile_mat_c3,labels_c1,labels_c3)#,average_vec_c1,average_vec_c3)
thresholds_c3 = classification_glm_thresholds(behaviors_c,merged_average_c12,quantile_mat_c1,quantile_mat_c2,labels_c1,labels_c2)#,average_vec_c1,average_vec_c2)
thresholds_s1 = classification_glm_thresholds(behaviors_s,merged_average_s23,quantile_mat_s2,quantile_mat_s3,labels_s2,labels_s3)#,average_vec_s2,average_vec_s3)
thresholds_s2 = classification_glm_thresholds(behaviors_s,merged_average_s13,quantile_mat_s1,quantile_mat_s3,labels_s1,labels_s3)#,average_vec_s1,average_vec_s3)
thresholds_s3 = classification_glm_thresholds(behaviors_s,merged_average_s12,quantile_mat_s1,quantile_mat_s2,labels_s1,labels_s2)#,average_vec_s1,average_vec_s2)

predictions_o1 = predict_labels(correlations_o1,thresholds_o1)
predictions_o2 = predict_labels(correlations_o2,thresholds_o2)
predictions_o3 = predict_labels(correlations_o3,thresholds_o3)
predictions_c1 = predict_labels(correlations_c1,thresholds_c1)
predictions_c2 = predict_labels(correlations_c2,thresholds_c2)
predictions_c3 = predict_labels(correlations_c3,thresholds_c3)
predictions_s1 = predict_labels(correlations_s1,thresholds_s1)
predictions_s2 = predict_labels(correlations_s2,thresholds_s2)
predictions_s3 = predict_labels(correlations_s3,thresholds_s3)

par(mfrow=c(3,3))
dec_acc_o1 = decoder_accuracy(predictions_o1,labels_o1,behaviors_o,include_0 = T)
dec_acc_o2 = decoder_accuracy(predictions_o2,labels_o2,behaviors_o,include_0 = T)
dec_acc_o3 = decoder_accuracy(predictions_o3,labels_o3,behaviors_o,include_0 = T)
dec_acc_c1 = decoder_accuracy(predictions_c1,labels_c1,behaviors_c,include_0 = T)
dec_acc_c2 = decoder_accuracy(predictions_c2,labels_c2,behaviors_c,include_0 = T)
dec_acc_c3 = decoder_accuracy(predictions_c3,labels_c3,behaviors_c,include_0 = T)
dec_acc_s1 = decoder_accuracy(predictions_s1,labels_s1,behaviors_s,include_0 = T)
dec_acc_s2 = decoder_accuracy(predictions_s2,labels_s2,behaviors_s,include_0 = T)
dec_acc_s3 = decoder_accuracy(predictions_s3,labels_s3,behaviors_s,include_0 = T)

names_o = c("No lab","B","I","R")
names_c = c("No lab","C","B","I","R")
names_s = c("No lab","B","I","R","F","H","S")

plot_confusions(list(dec_acc_o1,dec_acc_o2,dec_acc_o3,dec_acc_c1,dec_acc_c2,dec_acc_c3,dec_acc_s1,dec_acc_s2,dec_acc_s3),names_o,names_c,names_s,bw=T)

if (TRUE){
  
  merged_average_oc = merge_average_vector_sets(list(average_vec_c1[behaviors_o],average_vec_c2[behaviors_o],average_vec_c3[behaviors_o],
                                                     average_vec_o1[behaviors_o],average_vec_o2[behaviors_o],average_vec_o3[behaviors_o]),
                                                rbind(t_c1[c(1,3:5)],t_c2[c(1,3:5)],t_c3[c(1,3:5)],
                                                      t_o1,t_o2,t_o3))
  merged_average_os = merge_average_vector_sets(list(average_vec_s1[behaviors_o],average_vec_s2[behaviors_o],average_vec_s3[behaviors_o],
                                                     average_vec_o1[behaviors_o],average_vec_o2[behaviors_o],average_vec_o3[behaviors_o]),
                                                rbind(t_s1[1:4],t_s2[1:4],t_s3[1:4],
                                                      t_o1,t_o2,t_o3))
  merged_average_cs = merge_average_vector_sets(list(average_vec_c1[behaviors_o],average_vec_c2[behaviors_o],average_vec_c3[behaviors_o],
                                                     average_vec_s1[behaviors_o],average_vec_s2[behaviors_o],average_vec_s3[behaviors_o]),
                                                rbind(t_c1[c(1,3:5)],t_c2[c(1,3:5)],t_c3[c(1,3:5)],
                                                      t_s1[1:4],t_s2[1:4],t_s3[1:4]))
  
  cross_corrs_c1 = vector_similarity_session(quantile_mat_c1,1:dim(quantile_mat_c1)[1],behaviors_o,merged_average_os,method="correlation")
  cross_corrs_c2 = vector_similarity_session(quantile_mat_c2,1:dim(quantile_mat_c2)[1],behaviors_o,merged_average_os,method="correlation")
  cross_corrs_c3 = vector_similarity_session(quantile_mat_c3,1:dim(quantile_mat_c3)[1],behaviors_o,merged_average_os,method="correlation")
  
  cross_corrs_s1 = vector_similarity_session(quantile_mat_s1,1:dim(quantile_mat_s1)[1],behaviors_o,merged_average_oc,method="correlation")
  cross_corrs_s2 = vector_similarity_session(quantile_mat_s2,1:dim(quantile_mat_s2)[1],behaviors_o,merged_average_oc,method="correlation")
  cross_corrs_s3 = vector_similarity_session(quantile_mat_s3,1:dim(quantile_mat_s3)[1],behaviors_o,merged_average_oc,method="correlation")
  
  cross_corrs_o1 = vector_similarity_session(quantile_mat_o1,1:dim(quantile_mat_o1)[1],behaviors_o,merged_average_cs,method="correlation")
  cross_corrs_o2 = vector_similarity_session(quantile_mat_o2,1:dim(quantile_mat_o2)[1],behaviors_o,merged_average_cs,method="correlation")
  cross_corrs_o3 = vector_similarity_session(quantile_mat_o3,1:dim(quantile_mat_o3)[1],behaviors_o,merged_average_cs,method="correlation")
  
  cross_labs_o1 = true_labels(dim(quantile_mat_o1)[1],behaviors_o,inds_for_o1)
  cross_labs_o2 = true_labels(dim(quantile_mat_o2)[1],behaviors_o,inds_for_o2)
  cross_labs_o3 = true_labels(dim(quantile_mat_o3)[1],behaviors_o,inds_for_o3)
  
  cross_labs_s1 = true_labels(dim(quantile_mat_s1)[1],behaviors_o,inds_for_s1)
  cross_labs_s2 = true_labels(dim(quantile_mat_s2)[1],behaviors_o,inds_for_s2)
  cross_labs_s3 = true_labels(dim(quantile_mat_s3)[1],behaviors_o,inds_for_s3)
  
  cross_labs_c1 = true_labels(dim(quantile_mat_c1)[1],behaviors_o,inds_for_c1)
  cross_labs_c2 = true_labels(dim(quantile_mat_c2)[1],behaviors_o,inds_for_c2)
  cross_labs_c3 = true_labels(dim(quantile_mat_c3)[1],behaviors_o,inds_for_c3)
  
  cross_labs_o = c(cross_labs_o1,cross_labs_o2,cross_labs_o3)
  cross_labs_c = c(cross_labs_c1,cross_labs_c2,cross_labs_c3)
  cross_labs_s = c(cross_labs_s1,cross_labs_s2,cross_labs_s3)
  quantile_o = rbind(quantile_mat_o1,quantile_mat_o2,quantile_mat_o3)
  quantile_c = rbind(quantile_mat_c1,quantile_mat_c2,quantile_mat_c3)
  quantile_s = rbind(quantile_mat_s1,quantile_mat_s2,quantile_mat_s3)
  
  cross_threshs_o = classification_glm_thresholds(behaviors_o,merged_average_cs,quantile_c,quantile_s,cross_labs_c,cross_labs_s)
  cross_threshs_c = classification_glm_thresholds(behaviors_o,merged_average_os,quantile_o,quantile_s,cross_labs_o,cross_labs_s)
  cross_threshs_s = classification_glm_thresholds(behaviors_o,merged_average_oc,quantile_o,quantile_c,cross_labs_o,cross_labs_c)
  
  cross_preds_o1 = predict_labels(cross_corrs_o1,cross_threshs_o)
  cross_preds_o2 = predict_labels(cross_corrs_o2,cross_threshs_o)
  cross_preds_o3 = predict_labels(cross_corrs_o3,cross_threshs_o)
  cross_preds_c1 = predict_labels(cross_corrs_c1,cross_threshs_c)
  cross_preds_c2 = predict_labels(cross_corrs_c2,cross_threshs_c)
  cross_preds_c3 = predict_labels(cross_corrs_c3,cross_threshs_c)
  cross_preds_s1 = predict_labels(cross_corrs_s1,cross_threshs_s)
  cross_preds_s2 = predict_labels(cross_corrs_s2,cross_threshs_s)
  cross_preds_s3 = predict_labels(cross_corrs_s3,cross_threshs_s)
  
  
  par(mfrow=c(3,3))
  cross_dec_acc_o1 = decoder_accuracy(cross_preds_o1,cross_labs_o1,behaviors_o,include_0 = T)
  cross_dec_acc_o2 = decoder_accuracy(cross_preds_o2,cross_labs_o2,behaviors_o,include_0 = T)
  cross_dec_acc_o3 = decoder_accuracy(cross_preds_o3,cross_labs_o3,behaviors_o,include_0 = T)
  cross_dec_acc_c1 = decoder_accuracy(cross_preds_c1,cross_labs_c1,behaviors_o,include_0 = T)
  cross_dec_acc_c2 = decoder_accuracy(cross_preds_c2,cross_labs_c2,behaviors_o,include_0 = T)
  cross_dec_acc_c3 = decoder_accuracy(cross_preds_c3,cross_labs_c3,behaviors_o,include_0 = T)
  cross_dec_acc_s1 = decoder_accuracy(cross_preds_s1,cross_labs_s1,behaviors_o,include_0 = T)
  cross_dec_acc_s2 = decoder_accuracy(cross_preds_s2,cross_labs_s2,behaviors_o,include_0 = T)
  cross_dec_acc_s3 = decoder_accuracy(cross_preds_s3,cross_labs_s3,behaviors_o,include_0 = T)
}# Cross task predictions

plot_confusions(list(cross_dec_acc_o1,cross_dec_acc_o2,cross_dec_acc_o3,cross_dec_acc_c1,cross_dec_acc_c2,cross_dec_acc_c3,cross_dec_acc_s1,cross_dec_acc_s2,cross_dec_acc_s3),names_o,names_o,names_o,bw=T)


if (TRUE){
  set.seed(1)
  num_shuffle = 10000
  CYCLIC = FALSE
  USE_LABEL_IN_CYCLIC = TRUE
  OTHER_SESSION_CYCLIC = TRUE
  conf_o1 = array(NA,dim=c(4,4,num_shuffle))
  conf_o2 = array(NA,dim=c(4,4,num_shuffle))
  conf_o3 = array(NA,dim=c(4,4,num_shuffle))
  conf_c1 = array(NA,dim=c(5,5,num_shuffle))
  conf_c2 = array(NA,dim=c(5,5,num_shuffle))
  conf_c3 = array(NA,dim=c(5,5,num_shuffle))
  conf_s1 = array(NA,dim=c(7,7,num_shuffle))
  conf_s2 = array(NA,dim=c(7,7,num_shuffle))
  conf_s3 = array(NA,dim=c(7,7,num_shuffle))
  confs = list(conf_o1,conf_o2,conf_o3,conf_c1,conf_c2,conf_c3,conf_s1,conf_s2,conf_s3)
  if (!CYCLIC){
    random_preds_o1 = predict_labels_random(dim(correlations_o1)[1],num_shuffle,4,labels_o2,labels_o3,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_o2 = predict_labels_random(dim(correlations_o2)[1],num_shuffle,4,labels_o1,labels_o3,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_o3 = predict_labels_random(dim(correlations_o3)[1],num_shuffle,4,labels_o1,labels_o2,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_c1 = predict_labels_random(dim(correlations_c1)[1],num_shuffle,4,labels_c2,labels_c3,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_c2 = predict_labels_random(dim(correlations_c2)[1],num_shuffle,4,labels_c1,labels_c3,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_c3 = predict_labels_random(dim(correlations_c3)[1],num_shuffle,4,labels_c1,labels_c2,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_s1 = predict_labels_random(dim(correlations_s1)[1],num_shuffle,4,labels_s2,labels_s3,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_s2 = predict_labels_random(dim(correlations_s2)[1],num_shuffle,4,labels_s1,labels_s3,CYCLIC = OTHER_SESSION_CYCLIC)
    random_preds_s3 = predict_labels_random(dim(correlations_s3)[1],num_shuffle,4,labels_s1,labels_s2,CYCLIC = OTHER_SESSION_CYCLIC)
  }
  start = proc.time()[[3]]
  for (i in 1:num_shuffle){
    if (CYCLIC){
      if (USE_LABEL_IN_CYCLIC){
        random_preds_o1 = labels_o1[cyclic_shift_ind(length(labels_o1),minlag=20)]
        random_preds_o2 = labels_o2[cyclic_shift_ind(length(labels_o2),minlag=20)]
        random_preds_o3 = labels_o3[cyclic_shift_ind(length(labels_o3),minlag=20)]
        random_preds_c1 = labels_c1[cyclic_shift_ind(length(labels_c1),minlag=20)]
        random_preds_c2 = labels_c2[cyclic_shift_ind(length(labels_c2),minlag=20)]
        random_preds_c3 = labels_c3[cyclic_shift_ind(length(labels_c3),minlag=20)]
        random_preds_s1 = labels_s1[cyclic_shift_ind(length(labels_s1),minlag=20)]
        random_preds_s2 = labels_s2[cyclic_shift_ind(length(labels_s2),minlag=20)]
        random_preds_s3 = labels_s3[cyclic_shift_ind(length(labels_s3),minlag=20)]
      } else {
        random_preds_o1 = cyclic_shift(predictions_o1)
        random_preds_o2 = cyclic_shift(predictions_o2)
        random_preds_o3 = cyclic_shift(predictions_o3)
        random_preds_c1 = cyclic_shift(predictions_c1)
        random_preds_c2 = cyclic_shift(predictions_c2)
        random_preds_c3 = cyclic_shift(predictions_c3)
        random_preds_s1 = cyclic_shift(predictions_s1)
        random_preds_s2 = cyclic_shift(predictions_s2)
        random_preds_s3 = cyclic_shift(predictions_s3)
      }
      
      
      conf_o1[,,i] = decoder_accuracy(random_preds_o1,labels_o1,behaviors_o,include_0 = T,PLOT=F)
      conf_o2[,,i] = decoder_accuracy(random_preds_o2,labels_o2,behaviors_o,include_0 = T,PLOT=F)
      conf_o3[,,i] = decoder_accuracy(random_preds_o3,labels_o3,behaviors_o,include_0 = T,PLOT=F)
      
      conf_c1[,,i] = decoder_accuracy(random_preds_c1,labels_c1,behaviors_c,include_0 = T,PLOT=F)
      conf_c2[,,i] = decoder_accuracy(random_preds_c2,labels_c2,behaviors_c,include_0 = T,PLOT=F)
      conf_c3[,,i] = decoder_accuracy(random_preds_c3,labels_c3,behaviors_c,include_0 = T,PLOT=F)
      
      conf_s1[,,i] = decoder_accuracy(random_preds_s1,labels_s1,behaviors_s,include_0 = T,PLOT=F)
      conf_s2[,,i] = decoder_accuracy(random_preds_s2,labels_s2,behaviors_s,include_0 = T,PLOT=F)
      conf_s3[,,i] = decoder_accuracy(random_preds_s3,labels_s3,behaviors_s,include_0 = T,PLOT=F)
    } else {
      conf_o1[,,i] = decoder_accuracy(random_preds_o1[,i],labels_o1,behaviors_o,include_0 = T,PLOT=F)
      conf_o2[,,i] = decoder_accuracy(random_preds_o2[,i],labels_o2,behaviors_o,include_0 = T,PLOT=F)
      conf_o3[,,i] = decoder_accuracy(random_preds_o3[,i],labels_o3,behaviors_o,include_0 = T,PLOT=F)
      
      conf_c1[,,i] = decoder_accuracy(random_preds_c1[,i],labels_c1,behaviors_c,include_0 = T,PLOT=F)
      conf_c2[,,i] = decoder_accuracy(random_preds_c2[,i],labels_c2,behaviors_c,include_0 = T,PLOT=F)
      conf_c3[,,i] = decoder_accuracy(random_preds_c3[,i],labels_c3,behaviors_c,include_0 = T,PLOT=F)
      
      conf_s1[,,i] = decoder_accuracy(random_preds_s1[,i],labels_s1,behaviors_s,include_0 = T,PLOT=F)
      conf_s2[,,i] = decoder_accuracy(random_preds_s2[,i],labels_s2,behaviors_s,include_0 = T,PLOT=F)
      conf_s3[,,i] = decoder_accuracy(random_preds_s3[,i],labels_s3,behaviors_s,include_0 = T,PLOT=F)
    }
    print_progress(i,num_shuffle,start)
  }
  
  weighted_gm = function(diagonal,weights){
    return(prod(diagonal^(weights/sum(weights))))
  }
  hist_of_z_scored = function(confs,pred_acc,labels,behaviors,PLOT=TRUE){
    xlabs = c("No label",behaviors)
    z_scored_random = apply(confs,3,diag)
    z_scored_pred = diag(pred_acc)
    
    #overall_score = weighted_gm(z_scored_pred,table(labels))
    #overall_randoms = apply(z_scored_random,2,weighted_gm,weights=table(labels))
    
    num_things = length(z_scored_pred)
    if (num_things == 4){
      par(mfrow=c(2,2))
    } else if (num_things == 5){
      par(mfrow=c(3,2))
    } else {
      par(mfrow=c(3,3))
    }
    
    #breaks = seq(-5,10,by=0.5)
    breaks = seq(0,1,by=0.05)
    for (i in 1:dim(z_scored_random)[1]){
      if (PLOT){
        zs = z_scored_random[i,]
        #zs[which(z_scored_random[i,] < -5)] = -5
        #zs[which(z_scored_random[i,] > 10)] = 10
        hist(zs,main = "Shuffled accuracy vs. decoder accuracy",xlab = xlabs[i],breaks=breaks)
        abline(v = max(c(min(c(z_scored_pred[i],10)),-5)),col="red",lty=2)
      }
      mean_ = mean(z_scored_random[i,])
      sd_ = sd(z_scored_random[i,])
      #z_scored_random[i,] = (z_scored_random[i,] - mean_)/sd_
      z_scored_pred[i] = (z_scored_pred[i]-mean_)/sd_
      
    }
    if (PLOT){
      #hist(overall_randoms,main="Weighted mean accuracy (shuffled)",xlab="",breaks=seq(0,1,by=0.05))
      #abline(v = overall_score,col="red",lty=2)
    }
    
    return(z_scored_pred)
  }
  z_scored_mat = function(confs,pred_acc){
    m = dim(pred_acc)[1]
    
    zscored_mat = matrix(NA,m,m)
    
    for (i in 1:m){
      for (j in 1:m){
        
        mean_ = mean(confs[i,j,])
        sd_ = sd(confs[i,j,])
        zscored_mat[i,j] = (pred_acc[i,j] - mean_)/sd_
      }
    }
    return(zscored_mat)
  }
  z_scored_mat_list = function(conflist,pred_acc_list){
    zscored_mats = list()
    
    for (i in 1:length(conflist)){
      zscored_mats[[i]] = z_scored_mat(conflist[[i]],pred_acc_list[[i]])
    }
    
    return(zscored_mats)
  }
  
  PLOT_HISTS = TRUE
  if (TRUE){
    z_o1 = hist_of_z_scored(conf_o1,dec_acc_o1,labels_o1,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_o2 = hist_of_z_scored(conf_o2,dec_acc_o2,labels_o2,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_o3 = hist_of_z_scored(conf_o3,dec_acc_o3,labels_o3,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    zscores_o = rbind(z_o1,z_o2,z_o3)
    
    z_c1 = hist_of_z_scored(conf_c1,dec_acc_c1,labels_c1,c("Chasing","Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_c2 = hist_of_z_scored(conf_c2,dec_acc_c2,labels_c2,c("Chasing","Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_c3 = hist_of_z_scored(conf_c3,dec_acc_c3,labels_c3,c("Chasing","Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    zscores_c = rbind(z_c1,z_c2,z_c3)
    
    z_s1 = hist_of_z_scored(conf_s1,dec_acc_s1,labels_s1,c("Head bobbing","Investigation","Rearing","Front paw wall","Hang from wall","Sit on wall"),PLOT=PLOT_HISTS)
    z_s2 = hist_of_z_scored(conf_s2,dec_acc_s2,labels_s2,c("Head bobbing","Investigation","Rearing","Front paw wall","Hang from wall","Sit on wall"),PLOT=PLOT_HISTS)
    z_s3 = hist_of_z_scored(conf_s3,dec_acc_s3,labels_s3,c("Head bobbing","Investigation","Rearing","Front paw wall","Hang from wall","Sit on wall"),PLOT=PLOT_HISTS)
    zscores_s = rbind(z_s1,z_s2,z_s3)
    
  } #Calculate zscored accuracies
  
  if (TRUE){
    saveRDS(zscores_o,paste(wdir, "Processed data/",animal,"/zscores_o.RDS",sep=""))
    saveRDS(zscores_c,paste(wdir, "Processed data/",animal,"/zscores_c.RDS",sep=""))
    saveRDS(zscores_s,paste(wdir, "Processed data/",animal,"/zscores_s.RDS",sep=""))
  } # Save zscores
  
  if (TRUE){
    if (DIANA){
      titl = "Diana"
    } else if (LEIA){
      titl = "Leia"
    } else {
      titl = "Rey"
    }
    
    zscores_s[which(zscores_s > 10)] = 10
    zscores_c[which(zscores_c > 10)] = 10
    zscores_o[which(zscores_o > 10)] = 10
    zscores_s[which(zscores_s < 0)] = 0
    zscores_c[which(zscores_c < 0)] = 0
    zscores_o[which(zscores_o < 0)] = 0
    cols_c = c("#bcbd22","#2ca02c","#d62728","#9467bd")
    cols_o = c("#2ca02c","#d62728","#9467bd")
    cols_s = c("#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e")
    par(mfrow=c(1,1))
    plot(c(0.85,9.15),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy",main=titl)
    
    points(rep(0.8,3),zscores_o[1,2:4],pch=19,col=cols_o)
    text(labels=c("B","I","R"),x=rep(1.2,3),y=zscores_o[1,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_o)
    points(rep(1.8,3),zscores_o[2,2:4],pch=19,col=cols_o)
    text(labels=c("B","I","R"),x=rep(2.2,3),y=zscores_o[2,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_o)
    points(rep(2.8,3),zscores_o[3,2:4],pch=19,col=cols_o)
    text(labels=c("B","I","R"),x=rep(3.2,3),y=zscores_o[3,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_o)
    
    points(rep(3.8,4),zscores_c[1,2:5],pch=19,col=cols_c)
    text(labels=c("C","B","I","R"),x=rep(4.2,4),y=zscores_c[1,2:5],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_c)
    points(rep(4.8,4),zscores_c[2,2:5],pch=19,col=cols_c)
    text(labels=c("C","B","I","R"),x=rep(5.2,4),y=zscores_c[2,2:5],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_c)
    points(rep(5.8,4),zscores_c[3,2:5],pch=19,col=cols_c)
    text(labels=c("C","B","I","R"),x=rep(6.2,4),y=zscores_c[3,2:5],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_c)
    
    points(rep(6.8,6),zscores_s[1,2:7],pch=19,col=cols_s)
    text(labels=c("B","I","R","F","H","S"),x=rep(7.2,6),y=zscores_s[1,2:7],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_s)
    points(rep(7.8,6),zscores_s[2,2:7],pch=19,col=cols_s)
    text(labels=c("B","I","R","F","H","S"),x=rep(8.2,6),y=zscores_s[2,2:7],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_s)
    points(rep(8.8,6),zscores_s[3,2:7],pch=19,col=cols_s)
    text(labels=c("B","I","R","F","H","S"),x=rep(9.2,6),y=zscores_s[3,2:7],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_s)
    
    abline(v = 1.5,lty=2)
    abline(v = 2.5,lty=2)
    abline(v = 3.5,lty=1)
    abline(v = 6.5,lty=1)
    abline(v = 4.5,lty=2)
    abline(v = 5.5,lty=2)
    abline(v = 7.5,lty=2)
    abline(v = 8.5,lty=2)
    abline(h = 1.96,lty=2)
    axis(side=1,at=1:9,labels=c("O1","O2","O3","C1","C2","C3","S1","S2","S3"))
    axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
  } #Plot zscored accuracies
  if (TRUE){
    zmats = z_scored_mat_list(confs,list(dec_acc_o1,dec_acc_o2,dec_acc_o3,dec_acc_c1,dec_acc_c2,dec_acc_c3,dec_acc_s1,dec_acc_s2,dec_acc_s3))
  } #Zscored version to summarize confusion matrix
  
  if (True){
    saveRDS(zmats,paste(wdir, "Processed data/",animal,"/zmats.RDS",sep=""))
  } #Save zmats
} # Shuffling to get chance level indications!

if (TRUE){
  set.seed(1)
  num_shuffle = 10000
  OTHER_SESSION_CYCLIC = TRUE
  conf_o1 = array(NA,dim=c(4,4,num_shuffle))
  conf_o2 = array(NA,dim=c(4,4,num_shuffle))
  conf_o3 = array(NA,dim=c(4,4,num_shuffle))
  conf_c1 = array(NA,dim=c(4,4,num_shuffle))
  conf_c2 = array(NA,dim=c(4,4,num_shuffle))
  conf_c3 = array(NA,dim=c(4,4,num_shuffle))
  conf_s1 = array(NA,dim=c(4,4,num_shuffle))
  conf_s2 = array(NA,dim=c(4,4,num_shuffle))
  conf_s3 = array(NA,dim=c(4,4,num_shuffle))
  confs = list(conf_o1,conf_o2,conf_o3,conf_c1,conf_c2,conf_c3,conf_s1,conf_s2,conf_s3)
  if (TRUE){
    labs_c1 = labels_c1
    labs_c1[which(labels_c1 == 1)] = 0
    labs_c1[which(labels_c1 > 1)] = labs_c1[which(labels_c1 > 1)] - 1
    labs_c2 = labels_c2
    labs_c2[which(labels_c2 == 1)] = 0
    labs_c2[which(labels_c2 > 1)] = labs_c2[which(labels_c2 > 1)] - 1
    labs_c3 = labels_c3
    labs_c3[which(labels_c3 == 1)] = 0
    labs_c3[which(labels_c3 > 1)] = labs_c3[which(labels_c3 > 1)] - 1
    labs_s1 = labels_s1
    labs_s1[which(labels_s1 > 3)] = 0
    labs_s2 = labels_s2
    labs_s2[which(labels_s2 > 3)] = 0
    labs_s3 = labels_s3
    labs_s3[which(labels_s3 > 3)] = 0
  } # Remove labels that are not head bobbing, investigation or rearing in social and chasing
  
  c_labs = c(labs_c1,labs_c2,labs_c3)
  s_labs = c(labs_s1,labs_s2,labs_s3)
  o_labs = c(labels_o1,labels_o2,labels_o3)
  
  random_preds_o1 = predict_labels_random(dim(correlations_o1)[1],num_shuffle,4,c_labs,s_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_o2 = predict_labels_random(dim(correlations_o2)[1],num_shuffle,4,c_labs,s_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_o3 = predict_labels_random(dim(correlations_o3)[1],num_shuffle,4,c_labs,s_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_c1 = predict_labels_random(dim(correlations_c1)[1],num_shuffle,4,o_labs,s_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_c2 = predict_labels_random(dim(correlations_c2)[1],num_shuffle,4,o_labs,s_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_c3 = predict_labels_random(dim(correlations_c3)[1],num_shuffle,4,o_labs,s_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_s1 = predict_labels_random(dim(correlations_s1)[1],num_shuffle,4,c_labs,o_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_s2 = predict_labels_random(dim(correlations_s2)[1],num_shuffle,4,c_labs,o_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  random_preds_s3 = predict_labels_random(dim(correlations_s3)[1],num_shuffle,4,c_labs,o_labs,CYCLIC = OTHER_SESSION_CYCLIC)
  
  start = proc.time()[[3]]
  for (i in 1:num_shuffle){
    conf_o1[,,i] = decoder_accuracy(random_preds_o1[,i],labels_o1,behaviors_o,include_0 = T,PLOT=F)
    conf_o2[,,i] = decoder_accuracy(random_preds_o2[,i],labels_o2,behaviors_o,include_0 = T,PLOT=F)
    conf_o3[,,i] = decoder_accuracy(random_preds_o3[,i],labels_o3,behaviors_o,include_0 = T,PLOT=F)
    
    conf_c1[,,i] = decoder_accuracy(random_preds_c1[,i],labs_c1,behaviors_o,include_0 = T,PLOT=F)
    conf_c2[,,i] = decoder_accuracy(random_preds_c2[,i],labs_c2,behaviors_o,include_0 = T,PLOT=F)
    conf_c3[,,i] = decoder_accuracy(random_preds_c3[,i],labs_c3,behaviors_o,include_0 = T,PLOT=F)
    
    conf_s1[,,i] = decoder_accuracy(random_preds_s1[,i],labs_s1,behaviors_o,include_0 = T,PLOT=F)
    conf_s2[,,i] = decoder_accuracy(random_preds_s2[,i],labs_s2,behaviors_o,include_0 = T,PLOT=F)
    conf_s3[,,i] = decoder_accuracy(random_preds_s3[,i],labs_s3,behaviors_o,include_0 = T,PLOT=F)
    
    print_progress(i,num_shuffle,start)
  }
  
  weighted_gm = function(diagonal,weights){
    return(prod(diagonal^(weights/sum(weights))))
  }
  hist_of_z_scored = function(confs,pred_acc,labels,behaviors,PLOT=TRUE){
    xlabs = c("No label",behaviors)
    z_scored_random = apply(confs,3,diag)
    z_scored_pred = diag(pred_acc)
    
    #overall_score = weighted_gm(z_scored_pred,table(labels))
    #overall_randoms = apply(z_scored_random,2,weighted_gm,weights=table(labels))
    
    num_things = length(z_scored_pred)
    if (num_things == 4){
      par(mfrow=c(2,2))
    } else if (num_things == 5){
      par(mfrow=c(3,2))
    } else {
      par(mfrow=c(3,3))
    }
    
    #breaks = seq(-5,10,by=0.5)
    breaks = seq(0,1,by=0.05)
    for (i in 1:dim(z_scored_random)[1]){
      if (PLOT){
        zs = z_scored_random[i,]
        #zs[which(z_scored_random[i,] < -5)] = -5
        #zs[which(z_scored_random[i,] > 10)] = 10
        hist(zs,main = "Shuffled accuracy vs. decoder accuracy",xlab = xlabs[i],breaks=breaks)
        abline(v = max(c(min(c(z_scored_pred[i],10)),-5)),col="red",lty=2)
      }
      mean_ = mean(z_scored_random[i,])
      sd_ = sd(z_scored_random[i,])
      #z_scored_random[i,] = (z_scored_random[i,] - mean_)/sd_
      z_scored_pred[i] = (z_scored_pred[i]-mean_)/sd_
      
    }
    if (PLOT){
      #hist(overall_randoms,main="Weighted mean accuracy (shuffled)",xlab="",breaks=seq(0,1,by=0.05))
      #abline(v = overall_score,col="red",lty=2)
    }
    
    return(z_scored_pred)
  }
  z_scored_mat = function(confs,pred_acc){
    m = dim(pred_acc)[1]
    
    zscored_mat = matrix(NA,m,m)
    
    for (i in 1:m){
      for (j in 1:m){
        
        mean_ = mean(confs[i,j,])
        sd_ = sd(confs[i,j,])
        zscored_mat[i,j] = (pred_acc[i,j] - mean_)/sd_
      }
    }
    return(zscored_mat)
  }
  z_scored_mat_list = function(conflist,pred_acc_list){
    zscored_mats = list()
    
    for (i in 1:length(conflist)){
      zscored_mats[[i]] = z_scored_mat(conflist[[i]],pred_acc_list[[i]])
    }
    
    return(zscored_mats)
  }
  
  PLOT_HISTS = TRUE
  if (TRUE){
    z_o1 = hist_of_z_scored(conf_o1,cross_dec_acc_o1,labels_o1,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_o2 = hist_of_z_scored(conf_o2,cross_dec_acc_o2,labels_o2,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_o3 = hist_of_z_scored(conf_o3,cross_dec_acc_o3,labels_o3,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    zscores_o = rbind(z_o1,z_o2,z_o3)
    
    z_c1 = hist_of_z_scored(conf_c1,cross_dec_acc_c1,labs_c1,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_c2 = hist_of_z_scored(conf_c2,cross_dec_acc_c2,labs_c2,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_c3 = hist_of_z_scored(conf_c3,cross_dec_acc_c3,labs_c3,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    zscores_c = rbind(z_c1,z_c2,z_c3)
    
    z_s1 = hist_of_z_scored(conf_s1,cross_dec_acc_s1,labs_s1,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_s2 = hist_of_z_scored(conf_s2,cross_dec_acc_s2,labs_s2,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    z_s3 = hist_of_z_scored(conf_s3,cross_dec_acc_s3,labs_s3,c("Head bobbing","Investigation","Rearing"),PLOT=PLOT_HISTS)
    zscores_s = rbind(z_s1,z_s2,z_s3)
    
  } #Calculate zscored accuracies
  
  if (TRUE){
    saveRDS(zscores_o,paste(wdir, "Processed data/",animal,"/zscores_o_ACROSS_TASK.RDS",sep=""))
    saveRDS(zscores_c,paste(wdir, "Processed data/",animal,"/zscores_c_ACROSS_TASK.RDS",sep=""))
    saveRDS(zscores_s,paste(wdir, "Processed data/",animal,"/zscores_s_ACROSS_TASK.RDS",sep=""))
  } # Save zscores
  
  if (TRUE){
    zmats = z_scored_mat_list(confs,list(cross_dec_acc_o1,cross_dec_acc_o2,cross_dec_acc_o3,cross_dec_acc_c1,cross_dec_acc_c2,cross_dec_acc_c3,cross_dec_acc_s1,cross_dec_acc_s2,cross_dec_acc_s3))
  } #Zscored version to summarize confusion matrix
  
  if (TRUE){
    saveRDS(zmats,paste(wdir, "Processed data/",animal,"/zmats_ACROSS_TASK.RDS",sep=""))
  } #Save zmats
  
  if (FALSE){
    if (DIANA){
      titl = "Diana"
    } else if (LEIA){
      titl = "Leia"
    } else {
      titl = "Rey"
    }
    
    zscores_s[which(zscores_s > 10)] = 10
    zscores_c[which(zscores_c > 10)] = 10
    zscores_o[which(zscores_o > 10)] = 10
    zscores_s[which(zscores_s < 0)] = 0
    zscores_c[which(zscores_c < 0)] = 0
    zscores_o[which(zscores_o < 0)] = 0
    cols_c = c("#2ca02c","#d62728","#9467bd")
    cols_o = c("#2ca02c","#d62728","#9467bd")
    cols_s = c("#2ca02c","#d62728","#9467bd")
    par(mfrow=c(1,1))
    plot(c(0.85,9.15),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy",main=titl)
    
    points(rep(0.8,3),zscores_o[1,2:4],pch=19,col=cols_o)
    text(labels=c("B","I","R"),x=rep(1.2,3),y=zscores_o[1,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_o)
    points(rep(1.8,3),zscores_o[2,2:4],pch=19,col=cols_o)
    text(labels=c("B","I","R"),x=rep(2.2,3),y=zscores_o[2,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_o)
    points(rep(2.8,3),zscores_o[3,2:4],pch=19,col=cols_o)
    text(labels=c("B","I","R"),x=rep(3.2,3),y=zscores_o[3,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_o)
    
    points(rep(3.8,3),zscores_c[1,2:4],pch=19,col=cols_c)
    text(labels=c("B","I","R"),x=rep(4.2,4),y=zscores_c[1,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_c)
    points(rep(4.8,3),zscores_c[2,2:4],pch=19,col=cols_c)
    text(labels=c("B","I","R"),x=rep(5.2,4),y=zscores_c[2,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_c)
    points(rep(5.8,3),zscores_c[3,2:4],pch=19,col=cols_c)
    text(labels=c("B","I","R"),x=rep(6.2,4),y=zscores_c[3,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_c)
    
    points(rep(6.8,3),zscores_s[1,2:4],pch=19,col=cols_s)
    text(labels=c("B","I","R"),x=rep(7.2,6),y=zscores_s[1,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_s)
    points(rep(7.8,3),zscores_s[2,2:4],pch=19,col=cols_s)
    text(labels=c("B","I","R"),x=rep(8.2,6),y=zscores_s[2,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_s)
    points(rep(8.8,3),zscores_s[3,2:4],pch=19,col=cols_s)
    text(labels=c("B","I","R"),x=rep(9.2,6),y=zscores_s[3,2:4],adj=c(0.5,0.5),cex=1.3,font=2,col=cols_s)
    
    abline(v = 1.5,lty=2)
    abline(v = 2.5,lty=2)
    abline(v = 3.5,lty=1)
    abline(v = 6.5,lty=1)
    abline(v = 4.5,lty=2)
    abline(v = 5.5,lty=2)
    abline(v = 7.5,lty=2)
    abline(v = 8.5,lty=2)
    abline(h = 1.96,lty=2)
    axis(side=1,at=1:9,labels=c("O1","O2","O3","C1","C2","C3","S1","S2","S3"))
    axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
  } #Plot zscored accuracies
} # Shuffling to get chance level indications for across task type!


if (TRUE){
  csv_c1 = make_csv_of_correlations_and_labels(correlations_c1,labels_c1,predictions_c1,cross_preds_c1,behaviors_c,names_c,names_o,paste(wdir,"Processed data/",animal,"_decoding_chasing1.csv",sep=""))
  csv_c2 = make_csv_of_correlations_and_labels(correlations_c2,labels_c2,predictions_c2,cross_preds_c2,behaviors_c,names_c,names_o,paste(wdir,"Processed data/",animal,"_decoding_chasing2.csv",sep=""))
  csv_c3 = make_csv_of_correlations_and_labels(correlations_c3,labels_c3,predictions_c3,cross_preds_c3,behaviors_c,names_c,names_o,paste(wdir,"Processed data/",animal,"_decoding_chasing3.csv",sep=""))
  
  csv_o1 = make_csv_of_correlations_and_labels(correlations_o1,labels_o1,predictions_o1,cross_preds_o1,behaviors_o,names_o,names_o,paste(wdir,"Processed data/",animal,"_decoding_open1.csv",sep=""))
  csv_o2 = make_csv_of_correlations_and_labels(correlations_o2,labels_o2,predictions_o2,cross_preds_o2,behaviors_o,names_o,names_o,paste(wdir,"Processed data/",animal,"_decoding_open2.csv",sep=""))
  csv_o3 = make_csv_of_correlations_and_labels(correlations_o3,labels_o3,predictions_o3,cross_preds_o3,behaviors_o,names_o,names_o,paste(wdir,"Processed data/",animal,"_decoding_open3.csv",sep=""))
  
  csv_s1 = make_csv_of_correlations_and_labels(correlations_s1,labels_s1,predictions_s1,cross_preds_s1,behaviors_s,names_s,names_o,paste(wdir,"Processed data/",animal,"_decoding_social1.csv",sep=""))
  csv_s2 = make_csv_of_correlations_and_labels(correlations_s2,labels_s2,predictions_s2,cross_preds_s2,behaviors_s,names_s,names_o,paste(wdir,"Processed data/",animal,"_decoding_social2.csv",sep=""))
  csv_s3 = make_csv_of_correlations_and_labels(correlations_s3,labels_s3,predictions_s3,cross_preds_s3,behaviors_s,names_s,names_o,paste(wdir,"Processed data/",animal,"_decoding_social3.csv",sep=""))
} # Save csvs

if (TRUE){
  saveRDS(correlations_c1,paste(wdir,"Processed data/", animal, "/correlations_c1.RDS",sep=""))
  saveRDS(correlations_c2,paste(wdir,"Processed data/", animal, "/correlations_c2.RDS",sep=""))
  saveRDS(correlations_c3,paste(wdir,"Processed data/", animal, "/correlations_c3.RDS",sep=""))
  saveRDS(correlations_s1,paste(wdir,"Processed data/", animal, "/correlations_s1.RDS",sep=""))
  saveRDS(correlations_s2,paste(wdir,"Processed data/", animal, "/correlations_s2.RDS",sep=""))
  saveRDS(correlations_s3,paste(wdir,"Processed data/", animal, "/correlations_s3.RDS",sep=""))
  saveRDS(correlations_o1,paste(wdir,"Processed data/", animal, "/correlations_o1.RDS",sep=""))
  saveRDS(correlations_o2,paste(wdir,"Processed data/", animal, "/correlations_o2.RDS",sep=""))
  saveRDS(correlations_o3,paste(wdir,"Processed data/", animal, "/correlations_o3.RDS",sep=""))
  
  saveRDS(inds_for_c1,paste(wdir,"Processed data/", animal, "/inds_for_c1.RDS",sep=""))
  saveRDS(inds_for_c2,paste(wdir,"Processed data/", animal, "/inds_for_c2.RDS",sep=""))
  saveRDS(inds_for_c3,paste(wdir,"Processed data/", animal, "/inds_for_c3.RDS",sep=""))
  saveRDS(inds_for_s1,paste(wdir,"Processed data/", animal, "/inds_for_s1.RDS",sep=""))
  saveRDS(inds_for_s2,paste(wdir,"Processed data/", animal, "/inds_for_s2.RDS",sep=""))
  saveRDS(inds_for_s3,paste(wdir,"Processed data/", animal, "/inds_for_s3.RDS",sep=""))
  saveRDS(inds_for_o1,paste(wdir,"Processed data/", animal, "/inds_for_o1.RDS",sep=""))
  saveRDS(inds_for_o2,paste(wdir,"Processed data/", animal, "/inds_for_o2.RDS",sep=""))
  saveRDS(inds_for_o3,paste(wdir,"Processed data/", animal, "/inds_for_o3.RDS",sep=""))
  
  saveRDS(behaviors_c,paste(wdir,"Processed data/behaviors_c.RDS",sep=""))
  saveRDS(behaviors_o,paste(wdir,"Processed data/behaviors_o.RDS",sep=""))
  saveRDS(behaviors_s,paste(wdir,"Processed data/behaviors_s.RDS",sep=""))
  
  
} #save correlations, inds and behaviors


plot_similarities(correlations_c1,correlations_c2,correlations_c3,behaviors_c,inds_for_c1,inds_for_c2,inds_for_c3,10)
plot_similarities(correlations_o1,correlations_o2,correlations_o3,behaviors_o,inds_for_o1,inds_for_o2,inds_for_o3,10)
plot_similarities(correlations_s1,correlations_s2,correlations_s3,behaviors_s,inds_for_s1,inds_for_s2,inds_for_s3,10)


#### Plot after running for all animals ----

if (TRUE){
  DIANA_zscores_o = readRDS(paste(wdir, "Processed data/Diana/zscores_o.RDS",sep=""))
  DIANA_zscores_c = readRDS(paste(wdir, "Processed data/Diana/zscores_c.RDS",sep=""))
  DIANA_zscores_s = readRDS(paste(wdir, "Processed data/Diana/zscores_s.RDS",sep=""))
  
  LEIA_zscores_o = readRDS(paste(wdir, "Processed data/Leia/zscores_o.RDS",sep=""))
  LEIA_zscores_c = readRDS(paste(wdir, "Processed data/Leia/zscores_c.RDS",sep=""))
  LEIA_zscores_s = readRDS(paste(wdir, "Processed data/Leia/zscores_s.RDS",sep=""))
  
  REY_zscores_o = readRDS(paste(wdir, "Processed data/Rey/zscores_o.RDS",sep=""))
  REY_zscores_c = readRDS(paste(wdir, "Processed data/Rey/zscores_c.RDS",sep=""))
  REY_zscores_s = readRDS(paste(wdir, "Processed data/Rey/zscores_s.RDS",sep=""))
  
  DIANA_zmats = readRDS(paste(wdir, "Processed data/Diana/zmats.RDS",sep=""))
  LEIA_zmats = readRDS(paste(wdir, "Processed data/Leia/zmats.RDS",sep=""))
  REY_zmats = readRDS(paste(wdir, "Processed data/Rey/zmats.RDS",sep=""))
  
  q = qnorm(0.95)
  SUM_MAT = matrix(NA,7,7)
  NONA_MAT = matrix(NA,7,7)
  SUMMARY_MAT = matrix(NA,7,7)
  for (i in 1:4){
    vec = c(DIANA_zmats[[4]][2,i+1],DIANA_zmats[[5]][2,i+1],DIANA_zmats[[6]][2,i+1],
            LEIA_zmats[[4]][2,i+1],LEIA_zmats[[5]][2,i+1],LEIA_zmats[[6]][2,i+1],
            REY_zmats[[4]][2,i+1],REY_zmats[[5]][2,i+1],REY_zmats[[6]][2,i+1])
    vec = (vec > q)* 1
    SUMMARY_MAT[1,i] = mean(vec,na.rm=T)
    SUM_MAT[1,i] = sum(vec,na.rm=T)
    NONA_MAT[1,i] = length(which(!is.na(vec)))
  } # Chasing row
  for (i in 2:4){
    vec = c(DIANA_zmats[[4]][i+1,2],DIANA_zmats[[5]][i+1,2],DIANA_zmats[[6]][i+1,2],
            LEIA_zmats[[4]][i+1,2],LEIA_zmats[[5]][i+1,2],LEIA_zmats[[6]][i+1,2],
            REY_zmats[[4]][i+1,2],REY_zmats[[5]][i+1,2],REY_zmats[[6]][i+1,2])
    vec = (vec > q)* 1
    SUMMARY_MAT[i,1] = mean(vec,na.rm=T)
    SUM_MAT[i,1] = sum(vec,na.rm=T)
    NONA_MAT[i,1] = length(which(!is.na(vec)))
  } # Chasing col
  for (i in 2:4){
    for (j in 2:4){
      vec = c()
      for (k in 1:3){
        vec = c(vec,DIANA_zmats[[k]][i,j],DIANA_zmats[[k+3]][i+1,j+1],DIANA_zmats[[k+6]][i,j],
                LEIA_zmats[[k]][i,j],LEIA_zmats[[k+3]][i+1,j+1],LEIA_zmats[[k+6]][i,j],
                REY_zmats[[k]][i,j],REY_zmats[[k+3]][i+1,j+1],REY_zmats[[k+6]][i,j])
      }
      vec = (vec > q)* 1
      SUMMARY_MAT[i,j] = mean(vec,na.rm=T)
      SUM_MAT[i,j] = sum(vec,na.rm=T)
      NONA_MAT[i,j] = length(which(!is.na(vec)))
    }
  } # Shared in all sessions
  for (i in 5:7){
    for (j in 2:7){
      vec = c()
      for (k in 7:9){
        vec = c(vec,DIANA_zmats[[k]][i,j],LEIA_zmats[[k]][i,j],REY_zmats[[k]][i,j])
      }
      vec = (vec > q)* 1
      SUMMARY_MAT[i,j] = mean(vec,na.rm=T)
      SUM_MAT[i,j] = sum(vec,na.rm=T)
      NONA_MAT[i,j] = length(which(!is.na(vec)))
    }
  } # Social columns
  for (i in 2:4){
    for (j in 5:7){
      vec = c()
      for (k in 7:9){
        vec = c(vec,DIANA_zmats[[k]][i,j],LEIA_zmats[[k]][i,j],REY_zmats[[k]][i,j])
      }
      vec = (vec > q)* 1
      SUMMARY_MAT[i,j] = mean(vec,na.rm=T)
      SUM_MAT[i,j] = sum(vec,na.rm=T)
      NONA_MAT[i,j] = length(which(!is.na(vec)))
    }
  } # Social rows
  
  make_axis = function(names){
    locations = seq(0,1,length.out = length(names))
    axis(side=1,at=locations,labels=names,tick=FALSE,cex.axis=1.3)
    axis(side=2,at=locations,labels=names,tick=FALSE,cex.axis=1.3)
  }
  add_numbers = function(nona,sum,offdiagwhite=FALSE){
    textcol = "black"
    m = dim(nona)[1]
    locations = seq(0,1,length.out = m)
    for (i in 1:m){
      for (j in 1:m){
        if (!is.na(nona[i,j])){
          if (i==j || !offdiagwhite){
            text(labels=paste(sum[i,j],"/",nona[i,j],sep=""),x=locations[i],y=locations[j],adj=c(0.5,0.5),col=textcol,cex=1.3,font=2)
          } else {
            text(labels=paste(sum[i,j],"/",nona[i,j],sep=""),x=locations[i],y=locations[j],adj=c(0.5,0.5),col="white",cex=1.3,font=2)
          }
        }
      }
    }
  }
  colorbar = gray.colors(64,start=0.3,end=1)
  image.plot(SUMMARY_MAT,xaxt="n",yaxt="n",xlab="Prediction",ylab="Truth",breaks=seq(0,1,length.out = 65),col=colorbar,cex.lab=1.5,cex.main=1.5)
  make_axis(c("C","B","I","R","F","H","S"))
  add_numbers(NONA_MAT,SUM_MAT,offdiagwhite = T)
  
  plot_zscores_animal = function(zscores_o,zscores_c,zscores_s,x0){
    zscores_s[which(zscores_s > 10)] = 10
    zscores_c[which(zscores_c > 10)] = 10
    zscores_o[which(zscores_o > 10)] = 10
    zscores_s[which(zscores_s < 0)] = 0
    zscores_c[which(zscores_c < 0)] = 0
    zscores_o[which(zscores_o < 0)] = 0
    cols_c = c("#bcbd22","#2ca02c","#d62728","#9467bd")
    cols_o = c("#2ca02c","#d62728","#9467bd")
    cols_s = c("#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e")
    
    points(rep(x0+0.85,3),zscores_o[1,2:4],pch=19,col=cols_o)
    points(rep(x0+1,3),zscores_o[2,2:4],pch=18,col=cols_o,cex=1.5)
    points(rep(x0+1.15,3),zscores_o[3,2:4],pch=17,col=cols_o)
    
    points(rep(x0+1.85,4),zscores_c[1,2:5],pch=19,col=cols_c)
    points(rep(x0+2,4),zscores_c[2,2:5],pch=18,col=cols_c,cex=1.5)
    points(rep(x0+2.15,4),zscores_c[3,2:5],pch=17,col=cols_c)
    
    points(rep(x0+2.85,6),zscores_s[1,2:7],pch=19,col=cols_s)
    points(rep(x0+3,6),zscores_s[2,2:7],pch=18,col=cols_s,cex=1.5)
    points(rep(x0+3.15,6),zscores_s[3,2:7],pch=17,col=cols_s)
    
  }
  par(mfrow=c(1,1))
  plot(c(0.85,9.15),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy")
  axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
  abline(h = qnorm(0.95),lty=2)
  abline(h = qnorm(0.99),lty=2)
  abline(h = qnorm(0.999),lty=2)
  plot_zscores_animal(DIANA_zscores_o,DIANA_zscores_c,DIANA_zscores_s,0)
  plot_zscores_animal(LEIA_zscores_o,LEIA_zscores_c,LEIA_zscores_s,3)
  plot_zscores_animal(REY_zscores_o,REY_zscores_c,REY_zscores_s,6)
  abline(v = 3.5,lty=1)
  abline(v = 6.5,lty=1)
  axis(side=3,at=c(2,5,8),labels=c("Diana","Leia","Rey"))
  axis(side=1,at=seq(1,9,by=1),labels=c("O","C","E","O","C","E","O","C","E"))
  par(xpd=TRUE)
  text(labels=c("*","**","***"),x=c(9.8,9.8,9.8),y=qnorm(c(0.95,0.99,0.999)),adj=c(0.5,0.7),cex=1.3,font=2)
  par(xpd=FALSE)
  
  
  vals = c()
  labs = c()
  if (TRUE){
    v = c(DIANA_zscores_c[,2],LEIA_zscores_c[,2],REY_zscores_c[,2])
    labs = c(labs,rep(1,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_o[,2],LEIA_zscores_o[,2],REY_zscores_o[,2])
    labs = c(labs,rep(2,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_c[,3],LEIA_zscores_c[,3],REY_zscores_c[,3])
    labs = c(labs,rep(3,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_s[,2],LEIA_zscores_s[,2],REY_zscores_s[,2])
    labs = c(labs,rep(4,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_o[,3],LEIA_zscores_o[,3],REY_zscores_o[,3])
    labs = c(labs,rep(5,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_c[,4],LEIA_zscores_c[,4],REY_zscores_c[,4])
    labs = c(labs,rep(6,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_s[,3],LEIA_zscores_s[,3],REY_zscores_s[,3])
    labs = c(labs,rep(7,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_o[,4],LEIA_zscores_o[,4],REY_zscores_o[,4])
    labs = c(labs,rep(8,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_c[,5],LEIA_zscores_c[,5],REY_zscores_c[,5])
    labs = c(labs,rep(9,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_s[,4],LEIA_zscores_s[,4],REY_zscores_s[,4])
    labs = c(labs,rep(10,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_s[,5],LEIA_zscores_s[,5],REY_zscores_s[,5])
    labs = c(labs,rep(11,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_s[,6],LEIA_zscores_s[,6],REY_zscores_s[,6])
    labs = c(labs,rep(12,length(v)))
    vals = c(vals,v)
    v = c(DIANA_zscores_s[,7],LEIA_zscores_s[,7],REY_zscores_s[,7])
    labs = c(labs,rep(13,length(v)))
    vals = c(vals,v)
  }
  boxdat = data.frame(cbind(vals,labs))
  
  ANIMAL = "D"
  SESSIONSYMBOL = F
  cx = 1.5 #1.5 for individual, 1 for all
  if (TRUE){
    plot_zscores_animal_2 = function(zscores_o,zscores_c,zscores_s,x0,A,cx=1,SESSIONSYMBOL=FALSE){
      zscores_s[which(zscores_s > 10)] = 10
      zscores_c[which(zscores_c > 10)] = 10
      zscores_o[which(zscores_o > 10)] = 10
      zscores_s[which(zscores_s < 0)] = 0
      zscores_c[which(zscores_c < 0)] = 0
      zscores_o[which(zscores_o < 0)] = 0
      cols_c = c("#bcbd22","#2ca02c","#d62728","#9467bd")
      cols_o = c("#2ca02c","#d62728","#9467bd")
      cols_s = c("#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e")
      
      d = 0.2
      
      if (SESSIONSYMBOL){
        d = 1
        x0 = x0 + 1
        points(x=x0-d,y=zscores_c[1,2],pch=15,col=cols_c[1],cex=cx)
        #points(x=x0,y=zscores_c[2,2],pch=22,col="black",bg=cols_c[1],cex=cx,lwd=2)
        points(x=x0,y=zscores_c[2,2],pch=15,col=cols_c[1],cex=cx)
        points(x=x0+d,y=zscores_c[3,2],pch=15,col=cols_c[1],cex=cx)
        
        points(x=x0-d+c(3,12,21),y=zscores_o[1,2:4],pch=16,col=cols_o,cex=cx)
        #points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=21,col="black",bg=cols_o,cex=cx,lwd=2)
        points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=16,col=cols_o,cex=cx)
        points(x=x0+d+c(3,12,21),y=zscores_o[3,2:4],pch=16,col=cols_o,cex=cx)
        
        points(x=x0-d+c(6,15,24),y=zscores_c[1,3:5],pch=15,col=cols_c[2:4],cex=cx)
        #points(x=x0+c(6,15,24),y=zscores_c[2,3:5],pch=22,col="black",bg=cols_c[2:4],cex=cx,lwd=2)
        points(x=x0+c(6,15,24),y=zscores_c[2,3:5],pch=15,col=cols_c[2:4],cex=cx)
        points(x=x0+d+c(6,15,24),y=zscores_c[3,3:5],pch=15,col=cols_c[2:4],cex=cx)
        
        points(x=x0-d+c(9,18,27),y=zscores_s[1,2:4],pch=17,col=cols_s[1:3],cex=cx)
        #points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=24,col="black",bg=cols_s[1:3],cex=cx,lwd=2)
        points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=17,col=cols_s[1:3],cex=cx)
        points(x=x0+d+c(9,18,27),y=zscores_s[3,2:4],pch=17,col=cols_s[1:3],cex=cx)
        
        points(x=x0-d+c(30,33,36),y=zscores_s[1,5:7],pch=17,col=cols_s[4:6],cex=cx)
        #points(x=x0+c(30,33,36),y=zscores_s[2,5:7],pch=24,col="black",bg=cols_s[4:6],cex=cx,lwd=2)
        points(x=x0+c(30,33,36),y=zscores_s[2,5:7],pch=17,col=cols_s[4:6],cex=cx)
        points(x=x0+d+c(30,33,36),y=zscores_s[3,5:7],pch=17,col=cols_s[4:6],cex=cx)
      } else {
        points(x=x0-d,y=zscores_c[1,2],pch=A,col=cols_c[1],cex=cx)
        points(x=x0,y=zscores_c[2,2],pch=A,col=cols_c[1],cex=cx)
        points(x=x0+d,y=zscores_c[3,2],pch=A,col=cols_c[1],cex=cx)
        
        points(x=x0-d+c(3,12,21),y=zscores_o[1,2:4],pch=A,col=cols_o,cex=cx)
        points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=A,col=cols_o,cex=cx)
        points(x=x0+d+c(3,12,21),y=zscores_o[3,2:4],pch=A,col=cols_o,cex=cx)
        
        points(x=x0-d+c(6,15,24),y=zscores_c[1,3:5],pch=A,col=cols_c[2:4],cex=cx)
        points(x=x0+c(6,15,24),y=zscores_c[2,3:5],pch=A,col=cols_c[2:4],cex=cx)
        points(x=x0+d+c(6,15,24),y=zscores_c[3,3:5],pch=A,col=cols_c[2:4],cex=cx)
        
        points(x=x0-d+c(9,18,27),y=zscores_s[1,2:4],pch=A,col=cols_s[1:3],cex=cx)
        points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=A,col=cols_s[1:3],cex=cx)
        points(x=x0+d+c(9,18,27),y=zscores_s[3,2:4],pch=A,col=cols_s[1:3],cex=cx)
        
        points(x=x0-d+c(30,33,36),y=zscores_s[1,5:7],pch=A,col=cols_s[4:6],cex=cx)
        points(x=x0+c(30,33,36),y=zscores_s[2,5:7],pch=A,col=cols_s[4:6],cex=cx)
        points(x=x0+d+c(30,33,36),y=zscores_s[3,5:7],pch=A,col=cols_s[4:6],cex=cx)
      }
      
    }
    par(mfrow=c(1,1),mar=c(5,5,5,10))
    if (SESSIONSYMBOL){
      plot(c(1.8,38.2),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy")
    } else {
      plot(c(1.8,38.2),c(-0.5,10),col="white",xlab="Session type, O (Open field), C (Chasing), E (Elevated track)",xaxt="n",yaxt="n",ylab="z scored accuracy")
    }
    #axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
    par(las=1)
    #axis(side=2,at=c(0,qnorm(0.95),qnorm(0.999),5,10),labels=c("< 0",expression("z"[0.05]),expression("z"[0.001]),"5","> 10"))
    #axis(side=2,at=c(0,qnorm(0.95),qnorm(0.999),5,10),labels=c("< 0",expression(paste(alpha,"=.05",sep="")),expression(paste(alpha,"=.001",sep="")),"5","> 10"))
    axis(side=2,at=c(0,5,10),labels=c("< 0","5","> 10"))
    axis(side=2,at=c(qnorm(0.95)-0.1,qnorm(0.999)-0.1),labels=c("*","***"),cex.axis=1.7,tick=F)
    axis(side=2,at=c(qnorm(0.95),qnorm(0.999)),labels=c("",""),cex.axis=1.7,tick=T)
    par(las=0)
    
    abline(h = qnorm(0.95),lty=2)
    #abline(h = qnorm(0.99),lty=2)
    abline(h = qnorm(0.999),lty=2)
    if (SESSIONSYMBOL){
      if (ANIMAL == "D"){
        plot_zscores_animal_2(DIANA_zscores_o,DIANA_zscores_c,DIANA_zscores_s,1,15,cx,SESSIONSYMBOL = SESSIONSYMBOL)
      } else if (ANIMAL == "L"){
        plot_zscores_animal_2(LEIA_zscores_o,LEIA_zscores_c,LEIA_zscores_s,1,15,cx,SESSIONSYMBOL = SESSIONSYMBOL)
      } else if (ANIMAL == "R"){
        plot_zscores_animal_2(REY_zscores_o,REY_zscores_c,REY_zscores_s,1,15,cx,SESSIONSYMBOL = SESSIONSYMBOL)
      }
    } else {
      plot_zscores_animal_2(DIANA_zscores_o,DIANA_zscores_c,DIANA_zscores_s,1,15,cx,SESSIONSYMBOL = SESSIONSYMBOL)
      plot_zscores_animal_2(LEIA_zscores_o,LEIA_zscores_c,LEIA_zscores_s,2,16,cx)
      plot_zscores_animal_2(REY_zscores_o,REY_zscores_c,REY_zscores_s,3,17,cx)
      
    }
    
    for (i in 1:12){
      if (i %in% c(1,4,7,10,11,12)){
        abline(v = 0.5+i*3,lty=1)
      } else {
        if (!SESSIONSYMBOL){
          abline(v = 0.5+i*3,lty=2)
        }
      }
    }
    if (!SESSIONSYMBOL){
      axis(side=1,at=seq(2,38,by=3),labels=c("C","O","C","E","O","C","E","O","C","E","E","E","E"))
    }
    #axis(side=3,at=c(2,8,17,26,32,35,38),labels=c("C","B","I","R","F","H","S"))
    par(xpd=TRUE)
    if (SESSIONSYMBOL){
      legend(x=40,y=10.7,legend=c("Open field","Chasing","Elevated track"),pch=c(16,15,17),bty="n",cex=1.3)
    } else {
      legend(x=40,y=10.5,legend=c("Animal 1","Animal 2","Animal 3"),pch=c(15,16,17),bty="n",cex=1.3)
    }
    legend(x=40,y=6.5,legend=c("","","","","","",""),pch=16,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    legend(x=40,y=6.5,legend=c("","","","","","",""),pch=17,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    legend(x=40,y=6.5,legend=c("Chasing","Head bobbing","Investigation","Rearing","Front paw wall","Hang from wall","Sit on wall"),pch=15,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    par(xpd=FALSE)
  } # Zscored things
  
  WITH_NUM = T
  if (TRUE){
    par(mfrow=c(1,1),mar=c(5,5,5,2))
    colorbar = gray.colors(64,start=0.05,end=1)#gray.colors(64,start=0.3,end=1)
    image.plot(SUMMARY_MAT,xaxt="n",yaxt="n",xlab="Predicted State",ylab="Behavioral State",breaks=seq(0,1,length.out = 65),col=colorbar,cex.lab=1.5,cex.main=1.5,bty="n")
    par(xpd=TRUE)
    lines(c(-1/12,-1/12),c(-1/12,7/12))
    lines(c(-1/12,7/12),c(-1/12,-1/12))
    lines(c(1/12,1/12),c(7/12,13/12))
    lines(c(-1/12,1/12),c(7/12,7/12))
    lines(c(7/12,7/12),c(-1/12,1/12))
    lines(c(7/12,13/12),c(1/12,1/12))
    lines(c(1/12,13/12),c(13/12,13/12))
    lines(c(13/12,13/12),c(1/12,13/12))
    par(xpd=FALSE)
    make_axis(c("C","B","I","R","F","H","S"))
    if (WITH_NUM){
      add_numbers(NONA_MAT,SUM_MAT,offdiagwhite = T)
    }
  } # Fraction matrix thing
  
  boxplot(vals~labs,data=boxdat,xlim=c(0.85,13.15),ylim=c(-0.5,10),xaxt="n",yaxt="n",xlab="",ylab="z scored accuracy",col=c("#bcbd22","#2ca02c","#2ca02c","#2ca02c","#d62728","#d62728","#d62728","#9467bd","#9467bd","#9467bd","#17becf","#7f7f7f","#ff7f0e"))
  abline(h = qnorm(0.95),lty=2)
  abline(h = qnorm(0.99),lty=2)
  abline(h = qnorm(0.999),lty=2)
  axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
  axis(side=1,at=1:13,labels=c("C","O","C","E","O","C","E","O","C","E","E","E","E"))
  for (i in 1:12){
    if (i %in% c(1,4,7,10,11,12)){
      abline(v = 0.5+i,lty=1)
    } else {
      abline(v = 0.5+i,lty=2)
    }
  }
  
} # Plot summary results from the chance level! (Z scores and fraction matrix)

if (TRUE){
  DIANA_zscores_o = readRDS(paste(wdir, "Processed data/Diana/zscores_o_ACROSS_TASK.RDS",sep=""))
  DIANA_zscores_c = readRDS(paste(wdir, "Processed data/Diana/zscores_c_ACROSS_TASK.RDS",sep=""))
  DIANA_zscores_s = readRDS(paste(wdir, "Processed data/Diana/zscores_s_ACROSS_TASK.RDS",sep=""))
  
  LEIA_zscores_o = readRDS(paste(wdir, "Processed data/Leia/zscores_o_ACROSS_TASK.RDS",sep=""))
  LEIA_zscores_c = readRDS(paste(wdir, "Processed data/Leia/zscores_c_ACROSS_TASK.RDS",sep=""))
  LEIA_zscores_s = readRDS(paste(wdir, "Processed data/Leia/zscores_s_ACROSS_TASK.RDS",sep=""))
  
  REY_zscores_o = readRDS(paste(wdir, "Processed data/Rey/zscores_o_ACROSS_TASK.RDS",sep=""))
  REY_zscores_c = readRDS(paste(wdir, "Processed data/Rey/zscores_c_ACROSS_TASK.RDS",sep=""))
  REY_zscores_s = readRDS(paste(wdir, "Processed data/Rey/zscores_s_ACROSS_TASK.RDS",sep=""))
  
  DIANA_zmats = readRDS(paste(wdir, "Processed data/Diana/zmats_ACROSS_TASK.RDS",sep=""))
  LEIA_zmats = readRDS(paste(wdir, "Processed data/Leia/zmats_ACROSS_TASK.RDS",sep=""))
  REY_zmats = readRDS(paste(wdir, "Processed data/Rey/zmats_ACROSS_TASK.RDS",sep=""))
  
  q = qnorm(0.95)
  SUM_MAT = matrix(NA,3,3)
  NONA_MAT = matrix(NA,3,3)
  SUMMARY_MAT = matrix(NA,3,3)
  for (i in 2:4){
    for (j in 2:4){
      vec = c()
      for (k in 1:3){
        vec = c(vec,DIANA_zmats[[k]][i,j],DIANA_zmats[[k+3]][i,j],DIANA_zmats[[k+6]][i,j],
                LEIA_zmats[[k]][i,j],LEIA_zmats[[k+3]][i,j],LEIA_zmats[[k+6]][i,j],
                REY_zmats[[k]][i,j],REY_zmats[[k+3]][i,j],REY_zmats[[k+6]][i,j])
      }
      vec = (vec > q)* 1
      SUMMARY_MAT[i-1,j-1] = mean(vec,na.rm=T)
      SUM_MAT[i-1,j-1] = sum(vec,na.rm=T)
      NONA_MAT[i-1,j-1] = length(which(!is.na(vec)))
    }
  } # Shared in all sessions
  
  make_axis = function(names){
    locations = seq(0,1,length.out = length(names))
    axis(side=1,at=locations,labels=names,tick=FALSE,cex.axis=1.3)
    axis(side=2,at=locations,labels=names,tick=FALSE,cex.axis=1.3)
  }
  add_numbers = function(nona,sum){
    textcol = "black"
    m = dim(nona)[1]
    locations = seq(0,1,length.out = m)
    for (i in 1:m){
      for (j in 1:m){
        if (!is.na(nona[i,j])){
          text(labels=paste(sum[i,j],"/",nona[i,j],sep=""),x=locations[i],y=locations[j],adj=c(0.5,0.5),col=textcol,cex=1.3,font=2)
        }
      }
    }
  }
  colorbar = gray.colors(64,start=0.05,end=1)
  image.plot(SUMMARY_MAT,xaxt="n",yaxt="n",xlab="Prediction",ylab="Truth",breaks=seq(0,1,length.out = 65),col=colorbar,cex.lab=1.5,cex.main=1.5)
  make_axis(c("B","I","R"))
  add_numbers(NONA_MAT,SUM_MAT)
  
  plot_zscores_animal = function(zscores_o,zscores_c,zscores_s,x0){
    zscores_s[which(zscores_s > 10)] = 10
    zscores_c[which(zscores_c > 10)] = 10
    zscores_o[which(zscores_o > 10)] = 10
    zscores_s[which(zscores_s < 0)] = 0
    zscores_c[which(zscores_c < 0)] = 0
    zscores_o[which(zscores_o < 0)] = 0
    cols_c = c("#2ca02c","#d62728","#9467bd")
    cols_o = c("#2ca02c","#d62728","#9467bd")
    cols_s = c("#2ca02c","#d62728","#9467bd")
    
    points(rep(x0+0.85,3),zscores_o[1,2:4],pch=19,col=cols_o)
    points(rep(x0+1,3),zscores_o[2,2:4],pch=18,col=cols_o,cex=1.5)
    points(rep(x0+1.15,3),zscores_o[3,2:4],pch=17,col=cols_o)
    
    points(rep(x0+1.85,3),zscores_c[1,2:4],pch=19,col=cols_c)
    points(rep(x0+2,3),zscores_c[2,2:4],pch=18,col=cols_c,cex=1.5)
    points(rep(x0+2.15,3),zscores_c[3,2:4],pch=17,col=cols_c)
    
    points(rep(x0+2.85,3),zscores_s[1,2:4],pch=19,col=cols_s)
    points(rep(x0+3,3),zscores_s[2,2:4],pch=18,col=cols_s,cex=1.5)
    points(rep(x0+3.15,3),zscores_s[3,2:4],pch=17,col=cols_s)
    
  }
  par(mfrow=c(1,1))
  plot(c(0.85,9.15),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy")
  axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
  abline(h = qnorm(0.95),lty=2)
  abline(h = qnorm(0.99),lty=2)
  abline(h = qnorm(0.999),lty=2)
  plot_zscores_animal(DIANA_zscores_o,DIANA_zscores_c,DIANA_zscores_s,0)
  plot_zscores_animal(LEIA_zscores_o,LEIA_zscores_c,LEIA_zscores_s,3)
  plot_zscores_animal(REY_zscores_o,REY_zscores_c,REY_zscores_s,6)
  abline(v = 3.5,lty=1)
  abline(v = 6.5,lty=1)
  axis(side=3,at=c(2,5,8),labels=c("Diana","Leia","Rey"))
  axis(side=1,at=seq(1,9,by=1),labels=c("O","C","E","O","C","E","O","C","E"))
  par(xpd=TRUE)
  text(labels=c("*","**","***"),x=c(9.8,9.8,9.8),y=qnorm(c(0.95,0.99,0.999)),adj=c(0.5,0.7),cex=1.3,font=2)
  par(xpd=FALSE)
  
  SESSIONSYMBOL = F
  cx = 1.5 #for individual, 1 for all three
  if (TRUE){
    plot_zscores_animal_2 = function(zscores_o,zscores_c,zscores_s,x0,A,cx=1,SESSIONSYMBOL=FALSE){
      zscores_s[which(zscores_s > 10)] = 10
      zscores_c[which(zscores_c > 10)] = 10
      zscores_o[which(zscores_o > 10)] = 10
      zscores_s[which(zscores_s < 0)] = 0
      zscores_c[which(zscores_c < 0)] = 0
      zscores_o[which(zscores_o < 0)] = 0
      cols_c = c("#2ca02c","#d62728","#9467bd")
      cols_o = c("#2ca02c","#d62728","#9467bd")
      cols_s = c("#2ca02c","#d62728","#9467bd")
      
      d = 0.2
      
      if (SESSIONSYMBOL){
        d = 1
        x0 = x0 + 1
        points(x=x0-d+c(3,12,21),y=zscores_o[1,2:4],pch=16,col=cols_o,cex=cx)
        points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=21,col="black",bg=cols_o,cex=cx,lwd=2)
        points(x=x0+d+c(3,12,21),y=zscores_o[3,2:4],pch=16,col=cols_o,cex=cx)
        
        points(x=x0-d+c(6,15,24),y=zscores_c[1,2:4],pch=15,col=cols_c,cex=cx)
        points(x=x0+c(6,15,24),y=zscores_c[2,2:4],pch=22,col="black",bg=cols_c,cex=cx,lwd=2)
        points(x=x0+d+c(6,15,24),y=zscores_c[3,2:4],pch=15,col=cols_c,cex=cx)
        
        points(x=x0-d+c(9,18,27),y=zscores_s[1,2:4],pch=17,col=cols_s,cex=cx)
        points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=24,col="black",bg=cols_s,cex=cx,lwd=2)
        points(x=x0+d+c(9,18,27),y=zscores_s[3,2:4],pch=17,col=cols_s,cex=cx)
      } else {
        points(x=x0-d+c(3,12,21),y=zscores_o[1,2:4],pch=A,col=cols_o,cex=cx)
        points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=A,col=cols_o,cex=cx)
        points(x=x0+d+c(3,12,21),y=zscores_o[3,2:4],pch=A,col=cols_o,cex=cx)
        
        points(x=x0-d+c(6,15,24),y=zscores_c[1,2:4],pch=A,col=cols_c,cex=cx)
        points(x=x0+c(6,15,24),y=zscores_c[2,2:4],pch=A,col=cols_c,cex=cx)
        points(x=x0+d+c(6,15,24),y=zscores_c[3,2:4],pch=A,col=cols_c,cex=cx)
        
        points(x=x0-d+c(9,18,27),y=zscores_s[1,2:4],pch=A,col=cols_s,cex=cx)
        points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=A,col=cols_s,cex=cx)
        points(x=x0+d+c(9,18,27),y=zscores_s[3,2:4],pch=A,col=cols_s,cex=cx)
      }
      
    }
    par(mfrow=c(1,1),mar=c(5,5,5,10))
    if (SESSIONSYMBOL){
      plot(c(4.5,29.5),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy")
    } else {
      plot(c(4.5,29.5),c(-0.5,10),col="white",xlab="Session type, O (Open field), C (Chasing), E (Elevated track)",xaxt="n",yaxt="n",ylab="z scored accuracy")
    }
    #axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
    par(las=1)
    #axis(side=2,at=c(0,qnorm(0.95),qnorm(0.999),5,10),labels=c("< 0",expression("z"[0.05]),expression("z"[0.001]),"5","> 10"))
    #axis(side=2,at=c(0,qnorm(0.95),qnorm(0.999),5,10),labels=c("< 0",expression(paste(alpha,"=.05",sep="")),expression(paste(alpha,"=.001",sep="")),"5","> 10"))
    axis(side=2,at=c(0,5,10),labels=c("< 0","5","> 10"))
    axis(side=2,at=c(qnorm(0.95)-0.1,qnorm(0.999)-0.1),labels=c("*","***"),cex.axis=1.7,tick=F)
    axis(side=2,at=c(qnorm(0.95),qnorm(0.999)),labels=c("",""),cex.axis=1.7,tick=T)
    par(las=0)
    
    abline(h = qnorm(0.95),lty=2)
    #abline(h = qnorm(0.99),lty=2)
    abline(h = qnorm(0.999),lty=2)
    plot_zscores_animal_2(DIANA_zscores_o,DIANA_zscores_c,DIANA_zscores_s,1,15,cx,SESSIONSYMBOL = SESSIONSYMBOL)
    if (!SESSIONSYMBOL){
      plot_zscores_animal_2(LEIA_zscores_o,LEIA_zscores_c,LEIA_zscores_s,2,16,cx)
      plot_zscores_animal_2(REY_zscores_o,REY_zscores_c,REY_zscores_s,3,17,cx)
    }
    for (i in 1:12){
      if (i %in% c(1,4,7,10,11,12)){
        abline(v = 0.5+i*3,lty=1)
      } else {
        if (!SESSIONSYMBOL){
          abline(v = 0.5+i*3,lty=2)
        }
      }
    }
    if (!SESSIONSYMBOL){
      axis(side=1,at=seq(2,38,by=3),labels=c("C","O","C","E","O","C","E","O","C","E","E","E","E"))
    }
    #axis(side=3,at=c(2,8,17,26,32,35,38),labels=c("C","B","I","R","F","H","S"))
    par(xpd=TRUE)
    if (SESSIONSYMBOL){
      legend(x=31,y=10.7,legend=c("Open field","Chasing","Elevated track"),pch=c(16,15,17),bty="n",cex=1.3)
    } else {
      legend(x=31,y=10.5,legend=c("Animal 1","Animal 2","Animal 3"),pch=c(15,16,17),bty="n",cex=1.3)
    }
    legend(x=31,y=6.5,legend=c("","","","","","",""),pch=16,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    legend(x=31,y=6.5,legend=c("","","","","","",""),pch=17,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    legend(x=31,y=6.5,legend=c("Chasing","Head bobbing","Investigation","Rearing","Front paw wall","Hang from wall","Sit on wall"),pch=15,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    par(xpd=FALSE)
  } # Zscored things
  dev.off()
  
  ANIMAL = "R"
  cx = 1.5
  if (TRUE){
    plot_zscores_animal_2 = function(zscores_o,zscores_c,zscores_s,x0,A,cx=1,SESSIONSYMBOL=FALSE){
      zscores_s[which(zscores_s > 10)] = 10
      zscores_c[which(zscores_c > 10)] = 10
      zscores_o[which(zscores_o > 10)] = 10
      zscores_s[which(zscores_s < 0)] = 0
      zscores_c[which(zscores_c < 0)] = 0
      zscores_o[which(zscores_o < 0)] = 0
      cols_c = c("#2ca02c","#d62728","#9467bd")
      cols_o = c("#2ca02c","#d62728","#9467bd")
      cols_s = c("#2ca02c","#d62728","#9467bd")
      
      d = 0.2
      
      if (SESSIONSYMBOL){
        d = 1
        x0 = x0 + 1
        points(x=x0-d+c(3,12,21),y=zscores_o[1,2:4],pch=16,col=cols_o,cex=cx)
        points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=16,col=cols_o,cex=cx)
        points(x=x0+d+c(3,12,21),y=zscores_o[3,2:4],pch=16,col=cols_o,cex=cx)
        
        points(x=x0-d+c(6,15,24),y=zscores_c[1,2:4],pch=15,col=cols_c,cex=cx)
        points(x=x0+c(6,15,24),y=zscores_c[2,2:4],pch=15,col=cols_c,cex=cx)
        points(x=x0+d+c(6,15,24),y=zscores_c[3,2:4],pch=15,col=cols_c,cex=cx)
        
        points(x=x0-d+c(9,18,27),y=zscores_s[1,2:4],pch=17,col=cols_s,cex=cx)
        points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=17,col=cols_s,cex=cx)
        points(x=x0+d+c(9,18,27),y=zscores_s[3,2:4],pch=17,col=cols_s,cex=cx)
      } else {
        points(x=x0-d+c(3,12,21),y=zscores_o[1,2:4],pch=A,col=cols_o,cex=cx)
        points(x=x0+c(3,12,21),y=zscores_o[2,2:4],pch=A,col=cols_o,cex=cx)
        points(x=x0+d+c(3,12,21),y=zscores_o[3,2:4],pch=A,col=cols_o,cex=cx)
        
        points(x=x0-d+c(6,15,24),y=zscores_c[1,2:4],pch=A,col=cols_c,cex=cx)
        points(x=x0+c(6,15,24),y=zscores_c[2,2:4],pch=A,col=cols_c,cex=cx)
        points(x=x0+d+c(6,15,24),y=zscores_c[3,2:4],pch=A,col=cols_c,cex=cx)
        
        points(x=x0-d+c(9,18,27),y=zscores_s[1,2:4],pch=A,col=cols_s,cex=cx)
        points(x=x0+c(9,18,27),y=zscores_s[2,2:4],pch=A,col=cols_s,cex=cx)
        points(x=x0+d+c(9,18,27),y=zscores_s[3,2:4],pch=A,col=cols_s,cex=cx)
      }
      
    }
    par(mfrow=c(1,1),mar=c(5,5,5,10))
    plot(c(4.5,29.5),c(-0.5,10),col="white",xlab="",xaxt="n",yaxt="n",ylab="z scored accuracy")
    
    #axis(side=2,at=seq(0,10,by=2),labels=c("< 0","2","4","6","8","> 10"))
    par(las=1)
    #axis(side=2,at=c(0,qnorm(0.95),qnorm(0.999),5,10),labels=c("< 0",expression("z"[0.05]),expression("z"[0.001]),"5","> 10"))
    #axis(side=2,at=c(0,qnorm(0.95),qnorm(0.999),5,10),labels=c("< 0",expression(paste(alpha,"=.05",sep="")),expression(paste(alpha,"=.001",sep="")),"5","> 10"))
    axis(side=2,at=c(0,5,10),labels=c("< 0","5","> 10"))
    axis(side=2,at=c(qnorm(0.95)-0.1,qnorm(0.999)-0.1),labels=c("*","***"),cex.axis=1.7,tick=F)
    axis(side=2,at=c(qnorm(0.95),qnorm(0.999)),labels=c("",""),cex.axis=1.7,tick=T)
    par(las=0)
    
    abline(h = qnorm(0.95),lty=2)
    #abline(h = qnorm(0.99),lty=2)
    abline(h = qnorm(0.999),lty=2)
    if (ANIMAL == "D"){
      plot_zscores_animal_2(DIANA_zscores_o,DIANA_zscores_c,DIANA_zscores_s,1,15,cx,SESSIONSYMBOL = T)
    } else if (ANIMAL == "L"){
      plot_zscores_animal_2(LEIA_zscores_o,LEIA_zscores_c,LEIA_zscores_s,1,15,cx,SESSIONSYMBOL = T)
    } else if (ANIMAL == "R"){
      plot_zscores_animal_2(REY_zscores_o,REY_zscores_c,REY_zscores_s,1,15,cx,SESSIONSYMBOL = T)
    }
    
    for (i in 1:12){
      if (i %in% c(1,4,7,10,11,12)){
        abline(v = 0.5+i*3,lty=1)
      }
    }
    
    #axis(side=3,at=c(2,8,17,26,32,35,38),labels=c("C","B","I","R","F","H","S"))
    par(xpd=TRUE)
    
    legend(x=31,y=10.7,legend=c("Open field","Chasing","Elevated track"),pch=c(16,15,17),bty="n",cex=1.3)
    
    legend(x=31,y=6.5,legend=c("","","","","","",""),pch=16,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    legend(x=31,y=6.5,legend=c("","","","","","",""),pch=17,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    legend(x=31,y=6.5,legend=c("Chasing","Head bobbing","Investigation","Rearing","Front paw wall","Hang from wall","Sit on wall"),pch=15,bty="n",col=c("#bcbd22","#2ca02c","#d62728","#9467bd","#17becf","#7f7f7f","#ff7f0e"),cex=1.3)
    par(xpd=FALSE)
  } # Zscored things
  
  if (TRUE){
    par(mfrow=c(1,1),mar=c(5,5,5,2))
    colorbar = gray.colors(64,start=0.05,end=1)
    #image.plot(SUMMARY_MAT,xaxt="n",yaxt="n",xlab="Predicted State",ylab="Behavioral State",breaks=seq(0,1,length.out = 65),col=colorbar,cex.lab=1.5,cex.main=1.5)#,bty="n")
    image.plot(flip(SUMMARY_MAT,c(3,1,2)),xaxt="n",yaxt="n",xlab="Predicted State",ylab="Behavioral State",breaks=seq(0,1,length.out = 65),col=colorbar,cex.lab=1.5,cex.main=1.5)#,bty="n")
    #make_axis(c("B","I","R"))
    make_axis(c("B","I","R")[c(3,1,2)])
    #add_numbers(NONA_MAT,SUM_MAT)
    add_numbers(flip(NONA_MAT,c(3,1,2)),flip(SUM_MAT,c(3,1,2)))
  } # Fraction matrix thing
  
} # Plot summary results from the chance level for across task type! (Z scores and fraction matrix)

#####