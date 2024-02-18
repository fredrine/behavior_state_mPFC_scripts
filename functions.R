#### Libraries ----
library(MGLM)
library(fields)
library(lmtest)
library(MASS)
library(mvtnorm)
library(ggbiplot)
library(fields)
library(Matrix)
library(ggplot2)
library(gridExtra)
library(sparseMVN)
library(rgl)
library(fields)
library(tidyverse)
library(RColorBrewer)
library(dendextend)
library(rmatio)
library(R.matlab)
library(umap)
library(glmnet)
library(EnvStats)
library(rhdf5)
library(vegan)
library(reticulate)
# NB NB - the list of libraries might contain some that are not used (it hasn't been cleaned up in a while)
#### Utility functions ----

gaussian_smoother1D = function(dat,sigma,window=NULL,periodic=FALSE,na.rm=F){
  if (is.null(window)){
    window = 8*sigma + 1
    mid = 4*sigma+1
  } else {
    mid = round((window-1)/2)+1
  }
  # Gaussian smoothing of a vector
  lendat = length(dat)
  kernel = array(0,dim=window)
  for (i in 1:window){
    kernel[i] = dnorm(i-mid,sd=sigma)
  }
  kernel = kernel/sum(kernel)
  newdat = array(0,lendat+2*(mid-1))
  newdat[mid:(lendat+mid-1)] = dat
  if (periodic){
    newdat[1:(mid-1)] = dat[(lendat-mid+2):lendat]
    newdat[(lendat+mid):(lendat+2*mid-2)] = dat[1:(mid-1)]
  } else {
    newdat[1:(mid-1)] = array(dat[1],mid-1)
    newdat[(lendat+mid):(lendat+2*mid-2)] = array(dat[lendat],mid-1)
  }
  result = array(0,lendat)
  for (i in 1:lendat){
    if (na.rm & is.na(newdat[i+mid-1])){
      result[i] = NA
    } else {
      result[i] = sum(kernel*newdat[i:(i+window-1)],na.rm=na.rm)
    }
  }
  return(result)
}

gaussian_smoother2D = function(dat,sigma,window=NULL,periodic=FALSE){
  dim1 = dim(dat)[1]
  dim2 = dim(dat)[2]
  
  if (is.null(window)){
    window = 8*sigma + 1
  } 
  kernel = matrix(0,nrow=window,ncol=window)
  mid = round((window-1)/2)+1
  for (i in 0:(mid-1)){
    for (j in 0:(mid-1)){
      weight = dmvnorm(c(i,j),sigma=diag(sigma,2))
      kernel[mid+i,mid+j] = weight
      kernel[mid+i,mid-j] = weight
      kernel[mid-i,mid+j] = weight
      kernel[mid-i,mid-j] = weight
    }
  }
  
  result = matrix(0,nrow=dim1,ncol=dim2)
  padded = matrix(nrow=dim1+window-1,ncol=dim2+window-1)
  padded[mid:(dim1+mid-1),mid:(dim2+mid-1)] = dat
  
  for (i in 1:dim1){
    for (j in 1:dim2){
      if (is.na(dat[i,j])){
        result[i,j] = NA
      } else {
        nonans = which(!is.na(padded[i:(i+window-1),j:(j+window-1)]))
        nonakernel = kernel/sum(kernel[nonans])
        prod = padded[i:(i+window-1),j:(j+window-1)]*nonakernel
        result[i,j] = sum(prod[nonans])
      }
    }
  }
  
  
  #result = t(apply(dat,1,gaussian_smoother1D,sigma=sigma,window=window,periodic=periodic))
  #result = apply(result,2,gaussian_smoother1D,sigma=sigma,window=window,periodic=periodic)
  return(result)
}

smoothcols = function(matrix,sd){
  for (i in 1:dim(matrix)[2]){
    matrix[,i] = gaussian_smoother1D(matrix[,i],sd)
  }
  return(matrix)
}

quantile_of_smoothed_spike_train = function(smoothed_spikes){
  # For each spike train, replace its smoothed values with the corresponding quantile of
  # the distribution of smoothed values for that cell, resulting in a vector where all
  # values between 0 and 1 are evenly represented (similar to normalizing / z-scoring)
  n = dim(smoothed_spikes)[1]
  m = dim(smoothed_spikes)[2]
  
  quantile_mat = matrix(0,nrow=n,ncol=m)
  ind = 1
  maxind = n*m
  start = proc.time()[3]
  for (i in 1:m){
    u = sort(unique(smoothed_spikes[,i]))
    
    current_sum = length(which(smoothed_spikes[,i] == u[1]))
    for (j in 2:length(u)){
      val = u[j]
      which_ones = which(smoothed_spikes[,i]==val)
      quantile_mat[which_ones,i] = current_sum/n
      current_sum = current_sum + length(which_ones)
      
      print_progress(ind,maxind,start)
      ind = ind + 1
    }
  }
  return(quantile_mat)
}

unperiodify = function(invec){
  # Makes the input vector no longer periodic in the interval -pi to pi
  vec = invec
  diffs = diff(vec)
  changes = which(abs(diffs)>pi)
  m = length(changes)
  
  prevchange = 0
  for (i in 1:m){
    if (i == m){
      end = length(vec)
    } else {
      end = changes[i+1]
    }
    change = prevchange - sign(diffs[changes[i]])
    vec[(changes[i]+1):end] = vec[(changes[i]+1):end] + 2*pi*change
    prevchange = change
  }
  return(vec)
}

periodify = function(vec){
  # Makes the vector periodic in the interval -pi to pi
  return((vec+pi)%%(2*pi)-pi)
}

fix_periodic = function(df,covname){
  m = dim(df)[1]
  wherena = which(is.na(df[[covname]]))
  wherenotna = which(!is.na(df[[covname]]))
  
  withoutna = df[[covname]][wherenotna]
  withoutnaunwrapped = unperiodify(withoutna)
  withnaunwrapped = array(NA,dim=m)
  withnaunwrapped[wherenotna] = withoutnaunwrapped
  return(periodify(approx(x=(1:m),y=withnaunwrapped,xout=(1:m),rule=2)$y))
}

make_data = function(spiketimes,new_binsize,covariates=NULL,cov_binsize=1,onlycount=FALSE,periodiccovs = c(),max_T=NULL){
  # Takes spiketimes and covariate data frame, creates dataframe with
  # counts and the desired time bin size
  # Maybe some bug with the periodify/unperiodify
  
  if (!onlycount){
    M1 = dim(covariates)[1]
    max_T = cov_binsize*M1
  } else {
    if (is.null(max_T)){
      max_T = max(spiketimes + 1)
    }
  }
  
  spiketimes = spiketimes[which(spiketimes < max_T)]
  #N = length(spiketimes)
  M2 = ceiling(max_T/new_binsize)
  
  counts = array(0,M2)
  spike_ind = ceiling(spiketimes/new_binsize)
  for (i in spike_ind){
    counts[i] = counts[i] + 1
  }
  
  if (onlycount){
    return(counts)
  }
  
  df = data.frame(counts)
  
  for (j in 1:dim(covariates)[2]){
    if (j %in% periodiccovs){
      maxa = max(covariates[,j],na.rm=T)
      if (maxa > 4){
        if (maxa > 8){
          if (maxa > 200){
            covariates[,j] = covariates[,j]*pi/180 - pi
          } else {
            covariates[,j] = covariates[,j]*pi/180
          }
        } else {
          covariates[,j] = covariates[,j] - pi
        }
      }
      covariates[,j] = fix_periodic(covariates,colnames(covariates)[j])
      nonperiodic = unperiodify(covariates[,j])
      df[[paste("X",j,sep="")]] = periodify(approx(x=(1:M1)*cov_binsize,y=nonperiodic,xout=(1:M2)*new_binsize,rule=2)$y)
    } else {
      df[[paste("X",j,sep="")]] = approx(x=(1:M1)*cov_binsize,y=covariates[,j],xout=(1:M2)*new_binsize,rule=2)$y
    }
  }
  
  colnames(df) = c("counts",colnames(covariates))
  return(df)
}

BinMean = function (vec, every, SUM = FALSE, na.rm = FALSE, MATRIX = FALSE) {
  # Combines every "every" elements of a vector (or rows of a matrix if MATRIC = TRUE)
  # by taking the mean (unless SUM = TRUE, in which case the sum is used)
  if (MATRIX){
    m = dim(vec)[2]
    firstone = BinMean(vec[,1],every=every,SUM=SUM,na.rm=na.rm)
    k = length(firstone)
    x = matrix(0,nrow=k,ncol=m)
    x[,1] = firstone
    for (i in 2:m){
      x[,i] = BinMean(vec[,i],every=every,SUM=SUM,na.rm=na.rm)
    }
  } else {
    n = length(vec)
    if (SUM){
      x = .colSums(vec, every, n %/% every, na.rm)
      r = n %% every
      if (r) x = c(x, sum(vec[(n - r + 1):n], na.rm = na.rm))
    } else {
      x = .colMeans(vec, every, n %/% every, na.rm)
      r = n %% every
      if (r) x = c(x, mean.default(vec[(n - r + 1):n], na.rm = na.rm))
    }
  }
  return(x)
}

flip = function(matrix,order){
  #Reorders a matrix' rows and columns according to the given order
  #Used for clustering
  newmat = matrix[,order]
  return(newmat[order,])
}

print_progress = function(i,n,start,precision=1){
  pad=2*ceiling(log(n)/log(10))+1
  sofar = round((proc.time()[3] - start)*10^precision)/10^precision
  remain = round((sofar/i*(n-i))*10^precision)/10^precision
  output = paste(str_pad(paste(i,"/",n,sep=""),pad,"left"),", Elapsed: ",str_pad(formatC(sofar,format = "f",digits=precision),7+precision,"left"),", remaining: ",str_pad(formatC(remain,format = "f",digits=precision),7+precision,"left"),". ",sep="")
  percent = round(i/n*100)
  halfpercent = ceiling(percent/2)
  output=paste(output,paste("Progress: |",paste(rep("|",halfpercent),collapse=""),paste(rep(" ",50-halfpercent),collapse=""),"| ",percent,"%",sep=""),sep=" ")
  if (i < n){
    message(output,"\r",appendLF=FALSE)
    flush.console()
  } else {
    print(output)
  }
}

diffrange = function(vec){
  return(diff(range(vec)))
}

#### Tuning curve functions ----

binnify = function(covariate,min,max,nbins){
  #Bin covariate
  n = length(covariate)
  covariate[which(covariate > max)] = max
  covariate[which(covariate < min)] = min
  binned = matrix(0,nrow=n,ncol=nbins)
  bsize = (max-min)/nbins
  cols = floor((covariate[1:n]-min)/bsize) + 1
  cols[which(cols==nbins+1)] = nbins
  for (i in 1:n){
    binned[i,cols[i]] = 1
  }
  binmids = seq(min+0.5*bsize,max-0.5*bsize,length.out = nbins)
  return(list("binned"=binned,"binmids"=binmids))
}

binnify2d = function(covariate1,covariate2,min1,max1,min2,max2,nbins){
  #Bin covariate
  n = length(covariate1)
  if (length(covariate2) != n){
    print("Covariates must have the same length!")
    return(NULL)
  }
  binned = matrix(0,nrow=n,ncol=nbins^2)
  bsize1 = (max1-min1)/nbins
  bsize2 = (max2-min2)/nbins
  cols = floor((covariate1[1:n]-min1)/bsize1) + 1 + nbins*floor((covariate2[1:n]-min2)/bsize2)
  cols[which(cols==nbins^2+1)] = nbins^2
  for (i in 1:n){
    binned[i,cols[i]] = 1
  }
  binmids1 = seq(min1+0.5*bsize1,max1-0.5*bsize1,length.out = nbins)
  binmids2 = seq(min2+0.5*bsize2,max2-0.5*bsize2,length.out = nbins)
  return(list("binned"=binned,"binmids1"=binmids1,"binmids2"=binmids2))
}

bin_counts = function(df,nbins,binsize){
  binned = array(0,dim=nbins)
  for (i in 1:nbins){
    val = mean(df$counts[which(df[,i+1]==1)])/binsize
    if (!is.na(val)){
      binned[i] = val
    }
  }
  return(binned)
}

bin_counts2 = function(spikes,covbins,binsize){
  nbins = dim(covbins)[2]
  binned = array(0,dim=nbins)
  for (i in 1:nbins){
    val = mean(spikes[which(covbins[,i]==1)])/binsize
    if (!is.na(val)){
      binned[i] = val
    }
  }
  return(binned)
}

binnedtuningcurve = function(cell,spike_data,binned_cov,binmids,binsize,periodic=FALSE,main=""){
  df = data.frame(cbind(spike_data[[cell]],binned_cov))
  colnames(df)[1] = "counts"
  nbins = length(binmids)
  
  N = dim(df)[1]
  half_point = floor(N/2)
  #third_point = floor(N/3)
  #twothird_point = floor(2*N/3)
  
  binned_counts1 = bin_counts(df[1:half_point,],nbins,binsize)
  #binned_counts1 = bin_counts(df[1:third_point,],nbins,binsize)
  binned_counts2 = bin_counts(df[(half_point+1):N,],nbins,binsize)
  #binned_counts2 = bin_counts(df[(third_point+1):twothird_point,],nbins,binsize)
  #binned_counts3 = bin_counts(df[(twothird_point+1):N,],nbins,binsize)
  plot(binmids,gaussian_smoother1D(binned_counts1,1,periodic=periodic),type="l",
       xlab=cell,ylab="Rate",col="blue",main=main,
       ylim=c(min(c(binned_counts1,binned_counts2)),max(c(binned_counts1,binned_counts2))))#ylim=c(min(c(binned_counts1,binned_counts2,binned_counts3)),max(c(binned_counts1,binned_counts2,binned_counts3))))
  lines(binmids,gaussian_smoother1D(binned_counts2,1,periodic=periodic),col="red")
  #lines(binmids,gaussian_smoother1D(binned_counts3,1,periodic=periodic),col="green")
}

binspikesbycov = function(spikes,cov,binsize,periodic=FALSE,mains="",xlab="x",cex=1,SHUFFLE=FALSE,num_shuffle=100,NOSPLIT=FALSE){
  if (!is.null(dim(spikes))){
    m = dim(spikes)[2]
  } else {
    m = 1
  }
  
  for (i in 1:m){
    if (m > 1){
      spks = spikes[,i]
      main = mains[i]
    } else {
      spks = spikes
      main = ""
    }
    binned = binnify(cov,min(cov),max(cov),15)
    df = data.frame(cbind(spks,binned$binned))
    binmids = binned$binmids
    colnames(df)[1] = "counts"
    nbins = length(binmids)
    
    N = dim(df)[1]
    #third_point = floor(N/3)
    #twothird_point = floor(2*N/3)
    if (NOSPLIT){
      half_point = N
    } else {
      half_point = floor(N/2)
    }
    
    #binned_counts1 = bin_counts(df[1:third_point,],nbins,binsize)
    binned_counts1 = bin_counts(df[1:half_point,],nbins,binsize)
    
    if (!NOSPLIT){
      binned_counts2 = bin_counts(df[(half_point+1):N,],nbins,binsize)
      lines(binmids,gaussian_smoother1D(binned_counts2,1,periodic=periodic),col="red",lwd=cex,cex.axis=cex,cex.lab=cex)
      ylim=c(min(c(binned_counts1,binned_counts2)),max(c(binned_counts1,binned_counts2)))
    } else {
      ylim=c(min(c(binned_counts1)),max(c(binned_counts1)))
    }
    
    #binned_counts2 = bin_counts(df[(third_point+1):twothird_point,],nbins,binsize)
    #binned_counts3 = bin_counts(df[(twothird_point+1):N,],nbins,binsize)
    plot(binmids,gaussian_smoother1D(binned_counts1,1,periodic=periodic),type="l",
         xlab=xlab,ylab="Rate",col="blue",main=main,lwd=cex,cex=cex,cex.lab=cex,cex.axis=cex,cex.main=cex,
         #ylim=c(min(c(binned_counts1,binned_counts2,binned_counts3)),max(c(binned_counts1,binned_counts2,binned_counts3))))
         ylim=ylim)
    if (!NOSPLIT){
      lines(binmids,gaussian_smoother1D(binned_counts2,1,periodic=periodic),col="red",lwd=cex,cex.axis=cex,cex.lab=cex)
    }
  
    if (SHUFFLE){
      shuffles = matrix(NA,nrow=num_shuffle,ncol=length(binned_counts1))
      
      for (i in 1:num_shuffle){
        newdf = cbind(df[cyclic_shift(1:N)[1:half_point],1],df[1:half_point,2:dim(df)[2]])
        colnames(newdf)[1] = "counts"
        shuffles[i,] = bin_counts(newdf,nbins,binsize)
      }
      
      low = apply(shuffles,2,quantile,probs=0.025)
      high = apply(shuffles,2,quantile,probs=0.975)
      
      lines(binmids,gaussian_smoother1D(low,1,periodic=periodic),col="gray",lty=2,lwd=cex,cex.axis=cex,cex.lab=cex)
      lines(binmids,gaussian_smoother1D(high,1,periodic=periodic),col="gray",lty=2,lwd=cex,cex.axis=cex,cex.lab=cex)
    }
    #lines(binmids,gaussian_smoother1D(binned_counts3,1,periodic=periodic),col="green")
  }
}

BACKUPbincurves = function(cells,spike_data,track_data,binsize,covariate,nbins,min=NULL,max=NULL,periodic=FALSE,mfr=NULL,nblocks=15,SHUFFLED=FALSE,DONTSMOOTH=FALSE){
  if (is.null(min)){min=min(track_data[[covariate]])}
  if (is.null(max)){max=max(track_data[[covariate]])}
  b = binnify(covariate=track_data[[covariate]],min=min,max=max,nbins=nbins)
  k = ceiling(sqrt(length(cells)))
  if (is.null(mfr)){
    #par(mfrow=c(k,k))
  } else if (length(mfr) == 1){
    lol = 1
  } else {
    #par(mfrow=mfr)
  }
  for (i in 1:length(cells)){
    binnedtuningcurve(cell=cells[i],spike_data=spike_data,binned_cov=b$binned,binmids=b$binmids,binsize=binsize,periodic=periodic,nblocks=nblocks,SHUFFLED=SHUFFLED,DONTSMOOTH=DONTSMOOTH) 
  }
  if (is.null(mfr)){s = k*k} else { s = mfr[1]*mfr[2]}
  if (s > length(cells)){
    hist(track_data[[covariate]],main="Histogram of occupancy",freq=FALSE,xlab="")
  }
  
  mtext(covariate, side = 3, line = -2, outer = TRUE)
}

bincurves = function(cells,spike_data,track_data,binsize,covariate,nbins,min,max,periodic=FALSE){
  b = binnify(covariate=track_data[[covariate]],min=min,max=max,nbins=nbins)
  k = ceiling(sqrt(length(cells)))
  
  par(mfrow=c(k,k))
  for (i in 1:length(cells)){
    binnedtuningcurve(cell=cells[i],spike_data=spike_data,binned_cov=b$binned,binmids=b$binmids,binsize=binsize,periodic=periodic) 
  }
  if (k*k > length(cells)){
    hist(track_data[[covariate]],main="Histogram of occupancy",freq=FALSE,xlab="")
  }
  
  mtext(covariate, side = 3, line = -2, outer = TRUE)
}

bincurvescovs = function(cell,spike_data,track_data,binsize,covariates,nbins,mins,maxs,periodics=NULL,DONTSMOOTH=FALSE,nblocks=15,SHUFFLED=FALSE,PAR=TRUE,OCC=TRUE){
  m = length(covariates)
  if (is.null(periodics)){
    periodics = rep(FALSE,m)
  }
  
  k = ceiling(sqrt(length(covariates)))
  if (PAR){
    par(mfrow=c(2,m))
  }
  for (i in 1:m){
    cov = track_data[[covariates[i]]]
    if (is.na(mins[i])){mins[i]=min(cov)}
    if (is.na(maxs[i])){maxs[i]=max(cov)}
    b = binnify(covariate=cov,min=mins[i],max=maxs[i],nbins=nbins)
    binnedtuningcurve(cell=cell,spike_data=spike_data,binned_cov=b$binned,binmids=b$binmids,binsize=binsize,periodic=periodics[i],main=covariates[i])#,DONTSMOOTH = DONTSMOOTH,nblocks=nblocks,SHUFFLED=SHUFFLED) 
  }
  if (OCC){
    for (i in 1:m){
      hist(track_data[[covariates[i]]],main="Histogram of occupancy",freq=FALSE,xlab="")
    }
  }
  if (PAR){
    mtext(paste("Cluster_",cell,sep=""), side = 3, line = -2, outer = TRUE)
  }
}

binnedtuningYX = function(Y,binned_cov,binmids,binsize,main="",LEGEND=FALSE){
  df = data.frame(cbind(Y,binned_cov))
  colnames(df)[1] = "counts"
  nbins = length(binmids)
  
  N = dim(df)[1]
  third_point = floor(N/3)
  twothird_point = floor(2*N/3)
  
  binned_counts1 = bin_counts(df[1:third_point,],nbins,binsize)
  binned_counts2 = bin_counts(df[(third_point+1):twothird_point,],nbins,binsize)
  binned_counts3 = bin_counts(df[(twothird_point+1):N,],nbins,binsize)
  plot(binmids,gaussian_smoother1D(binned_counts1,1),type="l",lwd=1.5,
       xlab="",ylab="Rate",col="royalblue1",main=main,
       ylim=c(min(c(binned_counts1,binned_counts2,binned_counts3)),max(c(binned_counts1,binned_counts2,binned_counts3))))
  lines(binmids,gaussian_smoother1D(binned_counts2,1),col="firebrick",lwd=1.5)
  lines(binmids,gaussian_smoother1D(binned_counts3,1),col="gold",lwd=1.5)
  if (LEGEND){
    legend("topleft",legend=c("First third","Second third","Last third"),col=c("royalblue1","firebrick","gold"),lwd=1.5,lty=1)
  }
}

bincurvesYX = function(Y,X,binsize=1,nbins,NEW_PLOT=TRUE,OCC=TRUE,mains=NULL){
  n = length(Y)
  k = dim(X)[2]
  if (NEW_PLOT){
    par(mfrow=c(2,k))
  }
  covs = X
  for (i in 1:k){
    mincov = sort(covs[,i])[round(0.1*n)]
    maxcov = sort(covs[,i])[round(0.9*n)]
    covs[,i][which(covs[,i] < mincov)] = mincov
    covs[,i][which(covs[,i] > maxcov)] = maxcov
    b = binnify(covariate=covs[,i],min=mincov,max=maxcov,nbins=nbins)
    if (!is.null(mains)){
      main=mains[i]
      if (i == 2){
        leg = T
      } else {
        leg = F
      }
    } else {
      main=""
      leg = F
    }
    binnedtuningYX(Y,binned_cov=b$binned,binmids=b$binmids,binsize=binsize,main=main,LEGEND=leg) 
  }
  if (OCC){
    for (i in 1:k){
      hist(covs[,i],main="Histogram of occupancy",freq=FALSE,xlab="")
    }
  }
}

ratemapXY = function(act1,act2=NULL,act3=NULL,act4=NULL,X,Y,binning=80,binsize,smoothingsd=15,minocc=0.01,xrange1=NULL,yrange1=NULL,CORNERVAL=NULL){
  if (is.null(xrange1)){
    xrange1 = range(X)
  }
  if (is.null(yrange1)){
    yrange1 = range(Y)
  }
  rm1 = matrix(NA,nrow=binning,ncol=binning)
  if (!is.null(act2)){
    rm2 = matrix(NA,nrow=binning,ncol=binning)
  }
  if (!is.null(act3)){
    rm3 = matrix(NA,nrow=binning,ncol=binning)
  }
  if (!is.null(act4)){
    rm4 = matrix(NA,nrow=binning,ncol=binning)
  }
  occupancymat = matrix(NA,nrow=binning,ncol=binning)
  Xs = floor((X-xrange1[1])/(xrange1[2]*1.001-xrange1[1])*binning)
  Ys = floor((Y-yrange1[1])/(yrange1[2]*1.001-yrange1[1])*binning)
  for (j in 0:(binning-1)){
    for (k in 0:(binning-1)){
      occupancy = intersect(which(Xs==j),which(Ys==k))
      occupancymat[j+1,k+1] = length(occupancy)*binsize
      if (length(occupancy)*binsize > minocc){
        rm1[j+1,k+1] = sum(act1[occupancy])/length(occupancy)/binsize
        if (!is.null(act2)){
          rm2[j+1,k+1] = sum(act2[occupancy])/length(occupancy)/binsize
        }
        if (!is.null(act3)){
          rm3[j+1,k+1] = sum(act3[occupancy])/length(occupancy)/binsize
        }
        if (!is.null(act4)){
          rm4[j+1,k+1] = sum(act4[occupancy])/length(occupancy)/binsize
        }
      }
    }
  }
  pded1 = matrix(NA,nrow=binning+2,ncol=binning+2)
  pded1[2:(binning+1),2:(binning+1)] = rm1
  if (!is.null(act2)){
    pded2 = matrix(NA,nrow=binning+2,ncol=binning+2)
    pded2[2:(binning+1),2:(binning+1)] = rm2
  }
  if (!is.null(act3)){
    pded3 = matrix(NA,nrow=binning+2,ncol=binning+2)
    pded3[2:(binning+1),2:(binning+1)] = rm3
  }
  if (!is.null(act4)){
    pded4 = matrix(NA,nrow=binning+2,ncol=binning+2)
    pded4[2:(binning+1),2:(binning+1)] = rm4
  }
  for (j in 1:binning){
    for (k in 1:binning){
      if (!is.na(pded1[j+1,k+1])){
        numnotna = length(which(!is.na(c(pded1[j,k+1],
                    pded1[j,k+2],pded1[j,k],pded1[j+2,k+1],
                    pded1[j+2,k+2],pded1[j+2,k],pded1[j+1,k],
                                          pded1[j+1,k+2]))))
        if (numnotna < 3){
          rm1[j,k] = NA
        }
      }
      if (!is.null(act2)){
        if (!is.na(pded2[j+1,k+1])){
          numnotna = length(which(!is.na(c(pded2[j,k+1],
                                            pded2[j,k+2],pded2[j,k],pded2[j+2,k+1],
                                            pded2[j+2,k+2],pded2[j+2,k],pded2[j+1,k],
                                            pded2[j+1,k+2]))))
          if (numnotna < 3){
            rm2[j,k] = NA
          }
        }
      }
      if (!is.null(act3)){
        if (!is.na(pded3[j+1,k+1])){
          numnotna = length(which(!is.na(c(pded3[j,k+1],
                                            pded3[j,k+2],pded3[j,k],pded3[j+2,k+1],
                                            pded3[j+2,k+2],pded3[j+2,k],pded3[j+1,k],
                                            pded3[j+1,k+2]))))
          if (numnotna < 3){
            rm3[j,k] = NA
          }
        }
      }
      if (!is.null(act4)){
        if (!is.na(pded4[j+1,k+1])){
          numnotna = length(which(!is.na(c(pded4[j,k+1],
                                            pded4[j,k+2],pded4[j,k],pded4[j+2,k+1],
                                            pded4[j+2,k+2],pded4[j+2,k],pded4[j+1,k],
                                            pded4[j+1,k+2]))))
          if (numnotna < 3){
            rm4[j,k] = NA
          }
        }
      }
    }
  }
  if (length(smoothingsd) == 1){
    smoothingsd = c(smoothingsd,0,0,0)
  }
  if (smoothingsd[1] > 0){
    rm1 = gaussian_smoother2D(rm1,smoothingsd[1])
    occupancy_final = gaussian_smoother2D(occupancymat,smoothingsd[1])
  } else {
    occupancy_final = occupancymat
  }
  if (smoothingsd[2] > 0){
    if (!is.null(act2)){
      rm2 = gaussian_smoother2D(rm2,smoothingsd[2])
    }
  }
  if (smoothingsd[3] > 0){
    if (!is.null(act3)){
      rm3 = gaussian_smoother2D(rm3,smoothingsd[3])
    }
  }
  if (smoothingsd[4] > 0){
    if (!is.null(act4)){
      rm4 = gaussian_smoother2D(rm4,smoothingsd[4])
    }
  }
  if (!is.null(CORNERVAL)){
    rm1[1,1] = CORNERVAL
  }
  results = list(rm1=rm1)
  if (!is.null(act2)){
    if (!is.null(CORNERVAL)){
      rm2[1,1] = CORNERVAL
    }
    results$rm2 = rm2
  }
  if (!is.null(act3)){
    if (!is.null(CORNERVAL)){
      rm3[1,1] = CORNERVAL
    }
    results$rm3 = rm3
  }
  if (!is.null(act4)){
    if (!is.null(CORNERVAL)){
      rm4[1,1] = CORNERVAL
    }
    results$rm4 = rm4
  }
  results$occupancy = occupancy_final
  return(results)
}

consistency_chasing_ratemaps = function(group,spikes1,spikes2,spikes3,inds1,inds2,inds3,X1,X2,X3,Y1,Y2,Y3,binning=40,binsize=0.05,smoothingsd=5,minocc=0.01,square25=FALSE,ALLINONE=FALSE,xrange=NULL,yrange=NULL,RETURNIMS=FALSE){
  if (ALLINONE){
    par(mfrow=c(1,1))
    ims = list()
    counter = 1
  } else {
    if (square25){
      par(mfrow=c(5,5),mar=c(1,2,1,2))
    } else {
      par(mfrow=c(6,3),mar=c(1,2,1,2))
      #par(mfrow=c(2,2),mar=c(2,3,2,3))
    }
  }
  
  for (i in 1:(ceiling(length(group)/3))){
    a = 3*(i-1)+1
    b = 3*(i-1)+2
    c = 3*(i-1)+3
    ya_1 = spikes1[inds1,group[a]]
    ya_2 = spikes2[inds2,group[a]]
    ya_3 = spikes3[inds3,group[a]]
    
    yb_1 = spikes1[inds1,group[b]]
    yb_2 = spikes2[inds2,group[b]]
    yb_3 = spikes3[inds3,group[b]]
    
    yc_1 = spikes1[inds1,group[c]]
    yc_2 = spikes2[inds2,group[c]]
    yc_3 = spikes3[inds3,group[c]]
    
    if (!is.na(yb_1[1])){
      if (!is.na(yc_1[1])){
        rms1 = ratemapXY(act1=ya_1,act2=yb_1,act3=yc_1,X=X1[inds1],Y=Y1[inds1],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
        rms2 = ratemapXY(act1=ya_2,act2=yb_2,act3=yc_2,X=X2[inds2],Y=Y2[inds2],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
        rms3 = ratemapXY(act1=ya_3,act2=yb_3,act3=yc_3,X=X3[inds3],Y=Y3[inds3],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
      } else {
        rms1 = ratemapXY(act1=ya_1,act2=yb_1,X=X1[inds1],Y=Y1[inds1],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
        rms2 = ratemapXY(act1=ya_2,act2=yb_2,X=X2[inds2],Y=Y2[inds2],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
        rms3 = ratemapXY(act1=ya_3,act2=yb_3,X=X3[inds3],Y=Y3[inds3],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
      }
    } else {
      rms1 = ratemapXY(act1=ya_1,X=X1[inds1],Y=Y1[inds1],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
      rms2 = ratemapXY(act1=ya_2,X=X2[inds2],Y=Y2[inds2],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
      rms3 = ratemapXY(act1=ya_3,X=X3[inds3],Y=Y3[inds3],binning=binning,binsize=binsize,smoothingsd=smoothingsd,minocc=minocc,xrange1=xrange,yrange1=yrange)
    }
    
    if (ALLINONE){
      ims[[counter]] = rbind(rms1$rm1,rep(NA,binning),rms2$rm1,rep(NA,binning),rms3$rm1,rep(NA,binning))
      counter = counter + 1
      if (!is.na(yb_1[1])){
        ims[[counter]] = rbind(rms1$rm2,rep(NA,binning),rms2$rm2,rep(NA,binning),rms3$rm2,rep(NA,binning))
        counter = counter + 1
      }
      if (!is.na(yc_1[1])){
        ims[[counter]] = rbind(rms1$rm3,rep(NA,binning),rms2$rm3,rep(NA,binning),rms3$rm3,rep(NA,binning))
        counter = counter + 1
      }
    } else {
      image.plot(rbind(rms1$rm1,rep(NA,binning),rms2$rm1,rep(NA,binning),rms3$rm1))
      if (!is.na(yb_1[1])){
        image.plot(rbind(rms1$rm2,rep(NA,binning),rms2$rm2,rep(NA,binning),rms3$rm2))
      }
      if (!is.na(yc_1[1])){
        image.plot(rbind(rms1$rm3,rep(NA,binning),rms2$rm3,rep(NA,binning),rms3$rm3))
      }
    }
  }
  if (!RETURNIMS){
    if (ALLINONE){
      if (length(ims) < 25){
        for (i in (length(ims)+1):25){
          ims[[i]] = matrix(NA,nrow=3*binning+3,ncol=binning)
          ims[[i]][1,1] = 0
        }
      } 
      row1 = rbind(ims[[1]],ims[[2]],ims[[3]],ims[[4]],ims[[5]])
      row2 = rbind(ims[[6]],ims[[7]],ims[[8]],ims[[9]],ims[[10]])
      row3 = rbind(ims[[11]],ims[[12]],ims[[13]],ims[[14]],ims[[15]])
      row4 = rbind(ims[[16]],ims[[17]],ims[[18]],ims[[19]],ims[[20]])
      row5 = rbind(ims[[21]],ims[[22]],ims[[23]],ims[[24]],ims[[25]])
      im = cbind(row5,rep(NA,15*binning+15),row4,rep(NA,15*binning+15),row3,rep(NA,15*binning+15),row2,rep(NA,15*binning+15),row1)
      image.plot(im)
      par(mfrow=c(5,1))
      image.plot(row1)
      image.plot(row2)
      image.plot(row3)
      image.plot(row4)
      image.plot(row5)
    }
  } else {
    if (ALLINONE){
      return(ims)
    } else {
      print("Need to set ALLINONE = TRUE!")
    }
  }
}

plot_some_ims = function(ims1,ims2,ALLINONE=F){
  binning = dim(ims1[[1]])[2]
  if (length(ims1) < 25){
    for (i in (length(ims1)+1):25){
      ims1[[i]] = matrix(NA,nrow=3*binning+3,ncol=binning)
      ims1[[i]][1,1] = 0
    }
  } 
  if (length(ims2) < 25){
    for (i in (length(ims2)+1):25){
      ims2[[i]] = matrix(NA,nrow=3*binning+3,ncol=binning)
      ims2[[i]][1,1] = 0
    }
  } 
  if (ALLINONE){
    row1 = rbind(ims1[[1]],ims2[[1]])
    row2 = rbind(ims1[[6]],ims2[[6]])
    row3 = rbind(ims1[[11]],ims2[[11]])
    row4 = rbind(ims1[[16]],ims2[[16]])
    row5 = rbind(ims1[[21]],ims2[[21]])
    for (j in 1:4){
      row1 = rbind(row1,ims1[[1+j]],ims2[[1+j]])
      row2 = rbind(row2,ims1[[6+j]],ims2[[6+j]])
      row3 = rbind(row3,ims1[[11+j]],ims2[[11+j]])
      row4 = rbind(row4,ims1[[16+j]],ims2[[16+j]])
      row5 = rbind(row5,ims1[[21+j]],ims2[[21+j]])
    }
    navec = rep(NA,dim(row1)[1])
    im = cbind(row5,navec,row4,navec,row3,navec,row2,navec,row1)
    image.plot(im)
  } else {
    par(mfrow=c(5,5))
    for (i in 1:25){
      image.plot(rbind(ims1[[i]],ims2[[i]]))
    }
  }
}

covariate_ratemaps = function(x_,y_,combfew,alpha=0.05,minocc=0.01,binning=40,plotpoints=NULL,PAR=TRUE){
  rms1 = ratemapXY(act1=combfew[,1],act2=combfew[,2],act3=combfew[,3],act4=combfew[,4],X=x_,Y=y_,binning=binning,smoothingsd=2,binsize=1,minocc=minocc,xrange1=quantile(x_,c(alpha/2,1-alpha/2)),yrange1=quantile(y_,c(alpha/2,1-alpha/2)))
  rms2 = ratemapXY(act1=combfew[,5],act2=combfew[,6],act3=combfew[,7],X=x_,Y=y_,binning=binning,smoothingsd=2,binsize=1,minocc=minocc,xrange1=quantile(x_,c(alpha/2,1-alpha/2)),yrange1=quantile(y_,c(alpha/2,1-alpha/2)))
  if (PAR){
    par(mfrow=c(2,4))
  }
  image.plot(rms1$rm1,main="Speed")
  if (!is.null(plotpoints)){
    x_scaled = (x_ - min(x_))/(max(x_)-min(x_))
    y_scaled = (y_ - min(y_))/(max(y_)-min(y_))
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
  image.plot(rms1$rm2,main="Neckelevation")
  if (!is.null(plotpoints)){
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
  image.plot(rms1$rm3,main="HeadRoll")
  if (!is.null(plotpoints)){
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
  image.plot(rms1$rm4,main="HeadPitch")
  if (!is.null(plotpoints)){
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
  image.plot(rms2$rm1,main="HeadAzimuth")
  if (!is.null(plotpoints)){
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
  image.plot(rms2$rm2,main="BackPitch")
  if (!is.null(plotpoints)){
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
  image.plot(rms2$rm3,main="BackAzimuth")
  if (!is.null(plotpoints)){
    points(x_scaled[which(plotpoints==1)],y_scaled[which(plotpoints==1)],pch=4,col="pink")
  }
}

plot_rms_from_list = function(X,Y,neuralmat,cell_list,smoothing_sd=15,binning=80,min_occ=0.01,num_plots=9,mpar=c(1,1),main="",SCALE=F,noaxis=F,cex=1,maxrate=NULL,RETURNPLOT=FALSE){
  if (!is.null(mpar)){
    par(mfrow=mpar)
  }
  if (length(cell_list) > num_plots){
    cells = cell_list[floor(seq(1,length(cell_list),length.out = num_plots))]
  } else if (length(cell_list) < num_plots) {
    cells = c(cell_list,rep(cell_list[length(cell_list)],num_plots-length(cell_list)))
  } else {
    cells = cell_list
  }
  for (cell in cells){
    if (!cell %in% cell_list){
      print("WHAT")
    }
  }
  im = matrix(NA,nrow=3*binning,ncol=1)
  for (i in 1:round(num_plots/3)){
    rms = ratemapXY(act1=neuralmat[,cells[3*(i-1)+1]],
                    act2=neuralmat[,cells[3*(i-1)+2]],
                    act3=neuralmat[,cells[3*(i-1)+3]],
                    X=X,Y=Y,binning=binning,
                    binsize=1/7.5,smoothingsd=smoothing_sd,minocc=min_occ)
    if (SCALE){
      im = cbind(im,rbind(rms$rm1/max(rms$rm1,na.rm=T),rms$rm2/max(rms$rm2,na.rm=T),rms$rm3/max(rms$rm3,na.rm=T)))
    } else {
      im = cbind(im,rbind(rms$rm1,rms$rm2,rms$rm3))
    }
  }
  if (!is.null(maxrate)){
    im[1,1] = maxrate
    col = tim.colors()
    col[64] = "white"
    image.plot(im,main=main,col=col,axes=!noaxis,cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex,axis.args=list(cex.axis=cex))
  } else {
    image.plot(im,main=main,axes=!noaxis,cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex,axis.args=list(cex.axis=cex))
  }
  if (RETURNPLOT){
    return(im)
  }
}

plot_rms_from_list_normalised = function(X,Y,neuralmat,cell_list,smoothing_sd=15,binning=80,min_occ=0.01,num_plots=9,mpar=c(1,1),main="",noaxis=F,cex=1,maxrate=NULL,RETURNPLOT=FALSE,cbar=NULL){
  if (!is.null(mpar)){
    par(mfrow=mpar)
  }
  if (length(cell_list) > num_plots){
    cells = cell_list[floor(seq(1,length(cell_list),length.out = num_plots))]
  } else if (length(cell_list) < num_plots) {
    cells = c(cell_list,rep(cell_list[length(cell_list)],num_plots-length(cell_list)))
  } else {
    cells = cell_list
  }
  for (cell in cells){
    if (!cell %in% cell_list){
      print("WHAT")
    }
  }
  im = matrix(NA,nrow=3*binning+2,ncol=1)
  for (i in 1:round(num_plots/3)){
    rms = ratemapXY(act1=neuralmat[,cells[3*(i-1)+1]],
                    act2=neuralmat[,cells[3*(i-1)+2]],
                    act3=neuralmat[,cells[3*(i-1)+3]],
                    X=X,Y=Y,binning=binning,
                    binsize=1/7.5,smoothingsd=smoothing_sd,minocc=min_occ)
    quantiles = c(quantile(c(rms$rm1),0.95,na.rm=T),quantile(c(rms$rm2),0.95,na.rm=T),quantile(c(rms$rm3),0.95,na.rm=T))
    rms$rm1[which(rms$rm1 > quantiles[1])] = quantiles[1]
    rms$rm2[which(rms$rm2 > quantiles[2])] = quantiles[2]
    rms$rm3[which(rms$rm3 > quantiles[3])] = quantiles[3]
    im = cbind(im,rep(NA,3*binning+2),rbind(rms$rm1/quantiles[1],rep(NA,binning),rms$rm2/quantiles[2],rep(NA,binning),rms$rm3/quantiles[3]))
    
  }
  image(im,main=main,col=cbar,axes=!noaxis,cex.lab=cex, cex.axis=cex, cex.main=cex, cex.sub=cex)#,axis.args=list(cex.axis=cex))
  
  if (RETURNPLOT){
    return(im)
  }
}

#### UMAP utilities ----
redind = function(inds,prop){
  if (prop == 1){return(inds)}
  maxind = ceiling(inds[length(inds)]/prop)
  counts = hist(inds/prop,breaks=seq(0,maxind,length.out = maxind+1),plot=FALSE)$counts
  
  #return(inds[which(inds%%prop == 0)]/prop)
  return(which(counts > prop/2))
}

prep_colors_from_df = function(df,CHASE=TRUE,SOCIAL=FALSE,prop=6,onlyspeedhd=FALSE,oldHDcolors=FALSE){
  df$HD = df$HD/180*pi
  if (!SOCIAL){
    df$egoHD = df$egoHD/180*pi
  }
  
  Main_bs = 1/120
  new_df = make_data(c(),6*prop*Main_bs,covariates=df,cov_binsize=Main_bs,periodiccovs = c(2))
  
  if (oldHDcolors){
    colfunc1 <- colorRampPalette(c('firebrick','royalblue1',"darkcyan","gold",'firebrick'))
    mycolors1 = colfunc1(20)
  } else {
    mycolors1 = viridis(20)
  }
  colfunc2 <- colorRampPalette(c('royalblue1','firebrick','gold'))
  mycolors2 = colfunc2(20)
  
  colfunc3 = colorRampPalette(c('royalblue1','firebrick'))
  mycolors3 = colfunc3(20)
  
  speed = log(new_df$speed)
  speed = speed - log(0.05)#min(speed)
  speed = floor(speed/(log(150)-log(0.05))*19)+1#max(speed)*19)+1
  speedcolor = mycolors2[speed]
  
  HD = new_df$HD
  HD = HD+pi
  HD = floor(HD/(2*pi)*19)+1
  HDcolor = mycolors1[HD]
  
  if (!is.null(new_df$HDderiv)){
    dHD = new_df$HDderiv
    dHD = dHD + 6
    dHD = floor(dHD/(12)*19)+1
    dHDcolor = mycolors3[dHD]
    coldf = list(speed=speedcolor,HD=HDcolor,HDderiv=dHDcolor)
  } else {
    coldf = list(speed=speedcolor,HD=HDcolor)
  }
  
  return(coldf)
}

slice_list_of_arrays = function(a_list,subinds){
  a_names = names(a_list)
  b_list = list()
  for (i in 1:length(a_list)){
    b_list[[a_names[i]]] = a_list[[a_names[i]]][subinds]
  }
  return(b_list)
}

join_list_of_arrays = function(a_list,b_list){
  a_names = names(a_list)
  c_list = list()
  for (i in 1:length(a_list)){
    c_list[[a_names[i]]] = c(a_list[[a_names[i]]],b_list[[a_names[i]]])
  }
  return(c_list)
}

colorscale = function(x,col1,col2,PERIODIC=FALSE,numcol=20){
  integers = as.integer(cut(x,quantile(x,seq(0,1,length.out = numcol+1))))
  integers[which(is.na(integers))] = 1
  if (PERIODIC){
    colfunc = colorRampPalette(c(col1,col2,col1))(numcol)
  } else {
    colfunc = colorRampPalette(c(col1,col2))(numcol)
  }
  colors = colfunc[integers]
  return(colors)
}

make_some_umaps = function(inputs,ensemble,custom.config,mainmat=1,needscaling=T){
  if (mainmat == 1){
    mat1 = inputs$mat1
    mat2 = inputs$mat2
    mat3 = inputs$mat3
    sub_inds1 = inputs$sub_inds1
    sub_inds2 = inputs$sub_inds2
    sub_inds3 = inputs$sub_inds3
  } else if (mainmat == 2){
    mat1 = inputs$mat2
    mat2 = inputs$mat1
    mat3 = inputs$mat3
    sub_inds1 = inputs$sub_inds2
    sub_inds2 = inputs$sub_inds1
    sub_inds3 = inputs$sub_inds3
  } else if (mainmat == 3){
    mat1 = inputs$mat3
    mat2 = inputs$mat2
    mat3 = inputs$mat1
    sub_inds1 = inputs$sub_inds3
    sub_inds2 = inputs$sub_inds2
    sub_inds3 = inputs$sub_inds1
  }
  
  X_1 = data.frame(mat1[sub_inds1,ensembles[[ensemble]]])
  X_2 = data.frame(mat2[sub_inds2,ensembles[[ensemble]]])
  X_3 = data.frame(mat3[sub_inds3,ensembles[[ensemble]]])
  
  UMAP1 = umap(X_1,config = custom.config)
  UMAP2 = predict(UMAP1,X_2)
  UMAP3 = predict(UMAP1,X_3)
  if (mainmat == 1){
    return(list(UMAP1=UMAP1$layout,UMAP2=UMAP2,UMAP3=UMAP3,umap=UMAP1))
  } else if (mainmat == 2){
    return(list(UMAP1=UMAP2,UMAP2=UMAP1$layout,UMAP3=UMAP3,umap=UMAP1))
  } else if (mainmat == 3){
    return(list(UMAP1=UMAP3,UMAP2=UMAP2,UMAP3=UMAP1$layout,umap=UMAP1))
  }
}

make_one_umap = function(inputs,ensemble,custom.config,matstouse=c(1,2),subsub_inds=NULL){
  if (1 %in% matstouse){
    mat = scale(inputs$mat1)
    sub_inds = inputs$sub_inds1
    
    if (2 %in% matstouse){
      sub_inds = c(sub_inds,dim(mat)[1]+inputs$sub_inds2)
      mat = rbind(mat,scale(inputs$mat2))
    } else {
      sub_inds = c(sub_inds,dim(mat)[1]+inputs$sub_inds3)
      mat = rbind(mat,inputs$mat3)
    }
  } else {
    sub_inds = c(inputs$sub_inds2,dim(inputs$mat2)[1]+inputs$sub_inds3)
    mat = rbind(inputs$mat2,inputs$mat3)
  }
  
  if (!is.null(subsub_inds)){
    sub_inds = sub_inds[subsub_inds]
  }
  
  X = data.frame(mat[sub_inds,ensembles[[ensemble]]])
  
  UMAP = umap(X,config = custom.config)
  return(list(UMAP=UMAP$layout,umap=UMAP))
}

predict_UMAPs = function(UMAPS,inputs,ensemble){
  mat1 = inputs$mat1
  mat2 = inputs$mat2
  mat3 = inputs$mat3
  sub_inds1 = inputs$sub_inds1
  sub_inds2 = inputs$sub_inds2
  sub_inds3 = inputs$sub_inds3
  
  umap = UMAPS$umap
  
  X_1 = data.frame(mat1[sub_inds1,ensembles[[ensemble]]])
  X_2 = data.frame(mat2[sub_inds2,ensembles[[ensemble]]])
  X_3 = data.frame(mat3[sub_inds3,ensembles[[ensemble]]])
  
  UMAP1 = predict(umap,X_1)
  UMAP2 = predict(umap,X_2)
  UMAP3 = predict(umap,X_3)
  
  return(list(UMAP1=UMAP1,UMAP2=UMAP2,UMAP3=UMAP3))
}

plot_UMAPs = function(UMAPS,col_dfs,behaviors=NULL,colscheme="norm",somecols=NULL){
  col_df1 = col_dfs$col_df1
  col_df2 = col_dfs$col_df2
  col_df3 = col_dfs$col_df3
  xl = c(min(c(min(UMAPS$UMAP1[,1]),min(UMAPS$UMAP2[,1]),min(UMAPS$UMAP3[,1]))),max(c(max(UMAPS$UMAP1[,1]),max(UMAPS$UMAP2[,1]),max(UMAPS$UMAP3[,1]))))
  yl = c(min(c(min(UMAPS$UMAP1[,2]),min(UMAPS$UMAP2[,2]),min(UMAPS$UMAP3[,2]))),max(c(max(UMAPS$UMAP1[,2]),max(UMAPS$UMAP2[,2]),max(UMAPS$UMAP3[,2]))))
  par(mfrow=c(2,2))
  plot(UMAPS$UMAP1[,1],UMAPS$UMAP1[,2],pch=19,cex=0.8,xlim=xl,ylim=yl,col=col_df1[[colscheme]],xlab="Dimension 1",ylab="Dimension 2")
  plot(UMAPS$UMAP2[,1],UMAPS$UMAP2[,2],pch=19,cex=0.8,xlim=xl,ylim=yl,col=col_df2[[colscheme]],xlab="Dimension 1",ylab="Dimension 2")
  plot(UMAPS$UMAP3[,1],UMAPS$UMAP3[,2],pch=19,cex=0.8,xlim=xl,ylim=yl,col=col_df3[[colscheme]],xlab="Dimension 1",ylab="Dimension 2")
  plot.new()
  if (colscheme == "norm"){
    legend("topright",legend=c("no label",behaviors),col=somecols,lty=1,cex=2.5,lwd=3)
  } else if (colscheme == "session"){
    legend("topright",legend=c("Open","Chasing","Social"),col=c("#009E73","#FFD700","#CC79A7"),lty=1,cex=2.5,lwd=3)
  }
}

plot_one_UMAP = function(UMAP,col_df,behaviors=NULL,colscheme=c("norm"),somecols=NULL,dotscale=1,othernames=NULL){
  par(mar=c(4,4,3,3))
  if (length(colscheme) == 1){
    par(mfrow=c(1,2))
    plot(UMAP$UMAP[,1],UMAP$UMAP[,2],pch=19,cex=0.8*dotscale,col=col_df[[colscheme[1]]],xlab="Dimension 1",ylab="Dimension 2")
  } else if (length(colscheme) %in% c(2,3)){
    par(mfrow=c(2,2))
    for (i in 1:length(colscheme)){
      plot(UMAP$UMAP[,1],UMAP$UMAP[,2],pch=19,cex=0.8*dotscale,col=col_df[[colscheme[i]]],xlab="Dimension 1",ylab="Dimension 2")
    }
  }
  plot.new()
  if (is.null(othernames)){
    othernames = c("no label",behaviors)
  }
  if (length(othernames) > 5){
    cx = 2.3
  } else {
    cx = 2.5
  }
  legend("topright",legend=othernames,col=somecols,cex=cx,pch=19,bty="n")
  
}



#### Population vector decoding ----
make_index_vector = function(filename,scale=6){
  timestamps = round(read.csv(paste(wdir, "Tracking data/behaviours_and_units/",filename,sep=""),header=F)/scale)
  inds = c()
  for (i in 1:dim(timestamps)[1]){
    inds = c(inds,max(c(1,timestamps[i,1])):timestamps[i,2])
  }
  #lengths = timestamps[,2]-timestamps[,1]
  #hist(lengths/20,xlab="Seconds",main=filename)
  return(inds)
}

average_population_vector = function(quantile_mat,indices){
  m = dim(quantile_mat)[2]
  
  num_inds = length(indices)
  
  average_vec = array(0,dim=m)
  
  if (num_inds > 0){
    for (index in indices){
      vec = quantile_mat[index,]
      average_vec = average_vec + vec
    }
    
    average_vec = average_vec / num_inds
  }
  return(average_vec)
}

true_labels = function(n,behaviors,inds){
  m = length(behaviors)
  labels = rep(0,n)
  
  for (i in 1:n){
    for (b in 1:m){
      if (i %in% inds[[behaviors[b]]]){
        labels[i] = b
      }
    }
  }
  return(labels)
}

merge_average_vector_sets = function(average_vector_set_list,weights){
  weightsum = apply(weights,2,sum)
  for (i in 1:dim(weights)[1]){
    weights[i,] = weights[i,]/weightsum
  }
  
  merged_vector_set = list()
  behaviors = names(average_vector_set_list[[1]])
  
  for (j in 1:(dim(weights)[2]-1)){
    b = behaviors[j]
    avg = average_vector_set_list[[1]][[b]]*weights[1,j+1]
    for (i in 2:dim(weights)[1]){
      avg = avg + average_vector_set_list[[i]][[b]]*weights[i,j+1]
    }
    merged_vector_set[[b]] = avg
    if (is.na(avg[1])){
      merged_vector_set[[b]] = rep(0,length(avg))
    }
  }
  
  return(merged_vector_set)
}

vector_similarity = function(vec,behaviors,average_vectors,method="correlation"){
  similarity_scores = array(NA,dim=length(behaviors))
  
  for (i_b in 1:length(behaviors)){
    if (method == "correlation"){
      if (sum(average_vectors[[behaviors[i_b]]]^2)==0){
        similarity_scores[i_b] = 0
      } else {
        similarity_scores[i_b] = cor(vec,average_vectors[[behaviors[i_b]]],use="pairwise.complete.obs")
      }
    } else {
      similarity_scores[i_b] = mean((vec-average_vectors[[behaviors[i_b]]])^2,na.rm=T)
    }
  }
  return(similarity_scores)
}

vector_similarity_session = function(quantile_mat,indices,behaviors,average_vectors,method="correlation"){
  n = length(indices)
  l = length(behaviors)
  
  similarity_measures = matrix(NA,nrow=n,ncol=l)
  
  start = proc.time()[3]
  for (i in 1:n){
    vec = quantile_mat[indices[i],]
    similarity_measures[i,] = vector_similarity(vec,behaviors,average_vectors,method)
    
    print_progress(i,n,start)
  }
  return(similarity_measures)
}

classification_glm_thresholds = function(behaviors,merged_avg,quantile_mat_x,quantile_mat_y,labels_x,labels_y,avg_x=NULL,avg_y=NULL){
  if (is.null(avg_x)){
    insample_corrs_x = vector_similarity_session(quantile_mat_x,1:dim(quantile_mat_x)[1],behaviors,merged_avg,method="correlation")
    insample_corrs_y = vector_similarity_session(quantile_mat_y,1:dim(quantile_mat_y)[1],behaviors,merged_avg,method="correlation")
    
    combined_corrs = rbind(insample_corrs_x,insample_corrs_y)
  } else {
    insample_corrs_x = vector_similarity_session(quantile_mat_x,1:dim(quantile_mat_x)[1],behaviors,avg_y,method="correlation")
    insample_corrs_y = vector_similarity_session(quantile_mat_y,1:dim(quantile_mat_y)[1],behaviors,avg_x,method="correlation")
  }
  thresholds = array(0,length(behaviors))
  
  for (i in 1:length(behaviors)){
    if (is.null(avg_x)){
      y = array(0,length(labels_x)+length(labels_y))
      y[which(labels_x == i)] = 1
      y[length(labels_x) + which(labels_y == i)] = 1
      
      if (sum(y) > 0){
        fit = glm(y~combined_corrs[,i],family=binomial)
        ROCit_obj <- rocit(score=fit$fitted.values,class=y)
        p = plot(ROCit_obj)
        
        k = p$`optimal Youden Index point`[4]
        b0 = fit$coefficients[1]
        b1 = fit$coefficients[2]
        thresholds[i] = (-log(1/k - 1)-b0)/b1
      } else {
        thresholds[i] = 1
      }
      #-fit$coefficients[1]/fit$coefficients[2]
    } else {
      y1 = array(0,length(labels_x))
      y2 = array(0,length(labels_y))
      y1[which(labels_x == i)] = 1
      y2[which(labels_y == i)] = 1
      
      fit1 = glm(y1~insample_corrs_x[,i],family=binomial)
      fit2 = glm(y2~insample_corrs_y[,i],family=binomial)
      
      opposite_fitted_1 = 1/(1+exp(-(fit2$coefficients[1]+fit2$coefficients[2]*insample_corrs_x[,i])))
      opposite_fitted_2 = 1/(1+exp(-(fit1$coefficients[1]+fit1$coefficients[2]*insample_corrs_y[,i])))
      
      ROCit_obj1 <- rocit(score=opposite_fitted_1,class=y1)
      ROCit_obj2 <- rocit(score=opposite_fitted_2,class=y2)
      p1 = plot(ROCit_obj1)
      p2 = plot(ROCit_obj2)
      
      k = p1$`optimal Youden Index point`[4]
      b0 = fit1$coefficients[1]
      b1 = fit1$coefficients[2]
      t1 = (-log(1/k - 1)-b0)/b1
      
      k = p2$`optimal Youden Index point`[4]
      b0 = fit1$coefficients[1]
      b1 = fit1$coefficients[2]
      t2 = (-log(1/k - 1)-b0)/b1
      
      thresholds[i] = (t1+t2)/2
      
      #((-fit1$coefficients[1]/fit1$coefficients[2])+(-fit2$coefficients[1]/fit2$coefficients[2]))/2
    }
  }
  
  return(thresholds)
}

predict_labels = function(corrs,thresholds){
  preds = array(0,dim(corrs)[1])
  
  for (i in 1:length(preds)){
    passing = corrs[i,] > thresholds
    num_passing = sum(passing)
    if (num_passing == 1){
      preds[i] = which(passing)
    } else if (num_passing > 1){
      preds[i] = which.max(corrs[i,])
    }
  }
  return(preds)
}

decoder_accuracy = function(predictions,labels,behaviors,include_0=TRUE,PLOT=TRUE){
  if (include_0){
    m = length(behaviors)+1
    classes = 0:(m-1)
    behaviors = c("no label",behaviors)
  } else {
    m = length(behaviors)
    classes = 1:m
  }
  conf_mat = matrix(NA,nrow=m,ncol=m)
  
  for (i in 1:m){
    for (j in 1:m){
      conf_mat[i,j] = length(which(predictions == classes[i] & labels == classes[j]))/length(which(labels==classes[j]))
    }
  }
  if (PLOT){
    colorbar = hcl.colors(64,palette="SunsetDark")
    image.plot(conf_mat,breaks=seq(0,1,length.out = 65),col=colorbar)
  }
  
  rownames(conf_mat) = paste("Pred:",behaviors)
  colnames(conf_mat) = paste("Truth:",behaviors)
  return(conf_mat)
}

plot_confusions = function(conf_mat_list,names_o,names_c,names_s,bw=FALSE,nolab=TRUE,withtext=TRUE,reordering=FALSE){
  make_axis = function(names){
    locations = seq(0,1,length.out = length(names))
    axis(side=1,at=locations,labels=names,tick=FALSE,cex.axis=1.7)
    axis(side=2,at=locations,labels=names,tick=FALSE,cex.axis=1.7)
  }
  add_numbers = function(conf_mat,bw=FALSE){
    if (bw){
      textcol = "black"
    } else {
      textcol = "black"
    }
    num_things = dim(conf_mat)[1]
    if (num_things %in% c(3,4)){
      cex = 2
    } else if (num_things == 5){
      cex = 1.7
    } else if (num_things == 6){
      cex = 1.5
    } else if (num_things == 7){
      cex = 1.4
    } else {
      cex = 10
    }
    
    conf_mat = round(conf_mat*100)/100
    locations = seq(0,1,length.out = dim(conf_mat)[1])
    for (i in 1:dim(conf_mat)[1]){
      for (j in 1:dim(conf_mat)[2]){
        if (is.na(conf_mat[i,j])){
          textcol = "white"
        } else if (conf_mat[i,j] <= 0.1){
          textcol = "white"
        } else {
          textcol = "black"
        }
        text(labels=conf_mat[i,j],x=locations[i],y=locations[j],adj=c(0.5,0.5),col=textcol,cex=cex,font=2)
      }
    }
  }
  
  namelist = list(names_o,names_o,names_o,names_c,names_c,names_c,names_s,names_s,names_s)
  titles = c("Open Field 1","Open Field 2", "Open Field 3","Chasing Task 1", "Chasing Task 2","Chasing Task 3","Elevated Track 1", "Elevated Track 2", "Elevated Track 3")
  
  par(mfrow=c(3,3))
  for (i in 1:length(conf_mat_list)){
    if (bw){
      colorbar = gray.colors(64,start=0.05,end=1)
    } else {
      colorbar = hcl.colors(64,palette="SunsetDark")
    }
    numlabs = length(namelist[[i]])
    if (nolab){
      inds = 1:numlabs
    } else {
      inds = 2:numlabs
    }
    cm = conf_mat_list[[i]][inds,inds]
    cm[which(is.na(cm))] = 0
    
    names = namelist[[i]][inds]
    
    if (reordering){
      oldorder = 1:length(names)
      neworder = 1:length(names)
      neworder[which(names == "B")] = oldorder[which(names == "R")]
      neworder[which(names == "I")] = oldorder[which(names == "B")]
      neworder[which(names == "R")] = oldorder[which(names == "I")]
      cm = flip(cm,neworder)
      names = names[neworder]
      
    }
    
    image.plot(cm,xaxt="n",yaxt="n",xlab="Predicted State",ylab="Behavioral State",breaks=seq(0,1,length.out = 65),col=colorbar,main=titles[i],cex.lab=1.7,cex.main=1.7,axis.args=list(cex.axis=1.5))
    make_axis(names)
    if (withtext){
      add_numbers(cm,bw=bw)
    }
  }
}

predict_labels_random = function(n,num_random,num_class,labels_x,labels_y,CYCLIC=FALSE){
  
  preds = matrix(0,nrow=n,ncol=num_random)
  labs = c(labels_x,labels_y)
  
  start = proc.time()[[3]]
  for (i in 1:num_random){
    if (CYCLIC){
      preds[,i] = cyclic_shift(labs)[1:n]
    } else {
      preds[,i] = sample(labs,n,T)
    }
    print_progress(i,num_random,start)
  }
  return(preds)
}

cyclic_shift = function(vec,silent=TRUE){
  temp = FALSE
  if (is.null((dim(vec)))){
    n = length(vec)
    new_vec = array(0,dim=n)
  } else if (length(dim(vec))==2){
    n = dim(vec)[1]
    ncols = dim(vec)[2]
    new_vec = matrix(0,nrow=n,ncol=ncols)
  } else if (is.null((dim(dim(vec))))){
    n = length(vec)
    new_vec = array(0,dim=n)
    temp = TRUE
  } else {
    n = dim(vec)[1]
    ncols = dim(vec)[2]
    new_vec = matrix(0,nrow=n,ncol=ncols)
    if (length(dim(vec))==1){
      #print("hmm")
      temp = TRUE
    }
  }
  lag = floor(runif(1,min=0,max=n))
  if (is.null((dim(vec))) || temp){
    new_vec[(lag+1):n] = vec[1:(n-lag)]
    if (lag > 0){
      new_vec[1:lag] = vec[(n-lag+1):n]
    }
  } else {
    for (i in 1:(n-lag)){
      for (j in 1:ncols){
        new_vec[lag+i,j] = vec[i,j]
      }
    }
    #new_vec[(lag+1):n,1:ncols] = vec[1:(n-lag),1:ncols]
    if (lag > 0){
      for (i in 1:lag){
        for (j in 1:ncols){
          new_vec[i,j] = vec[n-lag+i,j]
        }
      }
      #new_vec[1:lag,1:ncols] = vec[(n-lag+1):n,1:ncols]
    }
  }
  if (!silent){
    print(lag)
  }
  return(new_vec)
}

cyclic_shift_ind = function(n,minlag=0,RETURNLAG=FALSE){
  lag = floor(runif(1,min=minlag,max=n-minlag))
  if (lag == 0){
    ind = 1:n
  } else {
    ind = c((lag+1):n,1:lag)
  }
  if (RETURNLAG){
    return(list(lag=lag,ind=ind))
  } else {
    return(ind)
  }
}

make_csv_of_correlations_and_labels = function(correlations,labels,predictions_within,predictions_across,behaviors,names_in,names_across,filename){
  bigthing = cbind(correlations,labels,predictions_within,predictions_across)
  colnames(bigthing) = c(behaviors,"label","within_pred","across_pred")
  
  df = data.frame(bigthing)
  df$label = names_in[df$label+1]
  df$within_pred = names_in[df$within_pred+1]
  df$across_pred = names_across[df$across_pred+1]
  write.csv(df,filename,row.names=F,dec=".",sep=",")
  
  return(df)
}

plot_similarities = function(correlations_1,correlations_2,correlations_3,behaviors,inds_1,inds_2,inds_3,sd=0,sessiontype="Chasing"){
  if (length(behaviors) == 4){
    colors = c("#FFD700","#009E73","#D55E00","#0072B2")#c("#F0E442","#009E73","#D55E00","#0072B2")
  } else if (length(behaviors) == 3){
    colors = c("purple","#D55E00","#0072B2")
  } else {
    colors = c("purple","#D55E00","#0072B2","#009E73","#CC79A7","brown")
  }
  if (sd > 0){
    for (i in 1:length(behaviors)){
      correlations_1[,i] = gaussian_smoother1D(correlations_1[,i],sd)
      correlations_2[,i] = gaussian_smoother1D(correlations_2[,i],sd)
      correlations_3[,i] = gaussian_smoother1D(correlations_3[,i],sd)
    }
  }
  
  pch = 19
  cex = 0.5
  maxest = max(c(max(correlations_1),max(correlations_2),max(correlations_3)))
  minest = min(c(min(correlations_1),min(correlations_2),min(correlations_3)))
  if (minest > 0){
    mark = -0.05
  } else {
    mark = 1.1*minest
  }
  yl = c(mark,maxest)
  
  par(mfrow=c(4,1),mar=c(4,4,4,4))
  plot(1,1,xaxt="n",yaxt="n",xlab="",ylab="",col="white")
  legend("topleft",legend=behaviors,col=colors,lty=1,bg="white",lwd=2,cex=2)
  plot(correlations_1[,1],type="l",col=colors[1],ylim=yl,xlim=c(0,length(correlations_1[,1])*1),ylab="Correlation with fingerprint",xlab="Time bin",main=paste(sessiontype,"1"))
  #legend("topright",legend=behaviors,col=colors,lty=1,bg="white")
  points(inds_1[[behaviors[1]]],rep(mark,length(inds_1[[behaviors[1]]])),pch=pch,cex=cex,col=colors[1])
  for (i in 2:length(behaviors)){
    lines(correlations_1[,i],col=colors[i])
    points(inds_1[[behaviors[i]]],rep(mark,length(inds_1[[behaviors[i]]])),pch=pch,cex=cex,col=colors[i])
  }
  plot(correlations_2[,1],type="l",col=colors[1],ylim=yl,xlim=c(0,length(correlations_2[,1])*1),ylab="Correlation with fingerprint",xlab="Time bin",main=paste(sessiontype,"2"))
  points(inds_2[[behaviors[1]]],rep(mark,length(inds_2[[behaviors[1]]])),pch=pch,cex=cex,col=colors[1])
  for (i in 2:length(behaviors)){
    lines(correlations_2[,i],col=colors[i])
    points(inds_2[[behaviors[i]]],rep(mark,length(inds_2[[behaviors[i]]])),pch=pch,cex=cex,col=colors[i])
  }
  plot(correlations_3[,1],type="l",col=colors[1],ylim=yl,xlim=c(0,length(correlations_3[,1])*1),ylab="Correlation with fingerprint",xlab="Time bin",main=paste(sessiontype,"3"))
  points(inds_3[[behaviors[1]]],rep(mark,length(inds_3[[behaviors[1]]])),pch=pch,cex=cex,col=colors[1])
  for (i in 2:length(behaviors)){
    lines(correlations_3[,i],col=colors[i])
    points(inds_3[[behaviors[i]]],rep(mark,length(inds_3[[behaviors[i]]])),pch=pch,cex=cex,col=colors[i])
  }
  
}

#### Coactivity functions ----
find_bursts = function(spike_train,window=5,threshold=3,NOSMOOTH=FALSE){
  n = length(spike_train)
  
  windowmid = (window+1)/2
  bursting = array(0,n)
  if (NOSMOOTH){
    binnedspiketrain = BinMean(spike_train,every=window,SUM=TRUE)
    quantile90 = quantile(binnedspiketrain,0.9)
    
    if (quantile90 >= threshold){
      for (i in windowmid:(n-windowmid+1)){
        if (sum(spike_train[(i-windowmid+1):(i+windowmid-1)]) >= quantile90){
          bursting[i] = 1
        }
      }
    }
  } else {
    quantile90 = quantile(spike_train,0.9)
    above_90 = (spike_train >= quantile90)
    
    for (i in windowmid:(n-windowmid+1)){
      if (sum(above_90[(i-windowmid+1):(i+windowmid-1)]) >= threshold){
        bursting[i] = 1
      }
    }
  }
  
  return(bursting)
}

bursting_for_all_cells = function(spikemat,window=5,threshold=3,NOSMOOTH=FALSE){
  burstmat = matrix(0,nrow=dim(spikemat)[1],ncol=dim(spikemat)[2])
  for (j in 1:dim(spikemat)[2]){
    burstmat[,j] = find_bursts(spikemat[,j],window=window,threshold=threshold,NOSMOOTH=NOSMOOTH)
  }
  return(burstmat)
}

overall_coactivity_from_indices = function(bigbinarybin,indices,filterquantile=0.8,FILTER=TRUE){
  n = length(indices)
  m = dim(bigbinarybin)[2]
  
  if (FILTER){
    filter = filter_from_top10_sessionwide(bigbinarybin,filterquantile=filterquantile)$filter
  }
  
  overall_coactivity = matrix(0,nrow=m,ncol=m)
  ind = 1
  nmax = (m-1)*m/2
  start = proc.time()[[3]]
  for (i in 1:(m-1)){
    for (j in (i+1):m){
      gm = sqrt((sum(bigbinarybin[indices,i])*sum(bigbinarybin[indices,j])))
      val = sum(bigbinarybin[indices,i]*bigbinarybin[indices,j])/gm - gm/n # scaled Geometric mean
      if (!is.na(val)){
        overall_coactivity[i,j] = val
        overall_coactivity[j,i] = val
      }
      print_progress(ind,nmax,start)
      ind = ind + 1
    }
  }
  if (FILTER){
    overall_coactivity = overall_coactivity*filter
  }
  return(overall_coactivity)
}

filter_from_top10_sessionwide = function(bigbinarybin,filterquantile=0.8){
  n = dim(bigbinarybin)[1]
  m = dim(bigbinarybin)[2]
  
  overall_product = matrix(NA,nrow=m,ncol=m)
  for (i in 1:(m-1)){
    for (j in (i+1):m){
      gm = sqrt((sum(bigbinarybin[,i])*sum(bigbinarybin[,j])))
      val = sum(bigbinarybin[,i]*bigbinarybin[,j])/gm - gm/n # scaledGeometric mean
      overall_product[i,j] = val
      overall_product[j,i] = val
    }
  }
  quantile90 = quantile(overall_product,probs=filterquantile,na.rm=T)
  filter = matrix(0,nrow=m,ncol=m)
  filter[which(overall_product > quantile90)] = 1
  
  return(list(filter=filter,sessionwide_coactivity=overall_product))
}

pair_care_prop = function(pairs,burstmat){
  n = dim(burstmat)[1]
  npair = dim(pairs)[2]
  summed_care = array(0,n)
  
  start = proc.time()[[3]]
  for (i in 1:npair){
    pair_overlap = pair_care(pairs[,i],burstmat)
    summed_care[pair_overlap] = summed_care[pair_overlap] + 1
    
    print_progress(i,npair,start)
  }
  return(summed_care/npair)
}

pair_care = function(pair,burstmat){
  return(burst_overlap(which(burstmat[,pair[1]]>0),which(burstmat[,pair[2]]>0)))
}

burst_overlap = function(bursts1,bursts2){
  return(intersect(bursts1,bursts2))
}

#####