rm(list=ls()); options(warn = -1)
setwd("/home/antonio.calcagni/storage/mef_continued")
sts = read.table(file = "settings.dat",header = FALSE,sep = ",",as.is = TRUE)
source("utils/utils.R"); source("utils/FLSA.R"); source("utils/cd-score.R"); source("utils/cv_functions.R")
library(doParallel); library(stm)
options(warn = -1)
set.seed(sts[1,2]);

ncores=sts[3,2]
cl = parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl,cores=ncores)
combAbind = abind::abind


cat("\n Running..")

#dataId = sts[6,2]
dataId = 3
load(paste0("datain/casestudy_data_",dataId,".RData"))
dtm_filtered_X = as.matrix(dtm_filtered)
export_data = c("dtm_filtered_X","dtm_matrix_filtered","words"); export_pkgs = c("stm")

ntopics = 2:(sts[3,2]+1)
B = length(ntopics)
chunk.size = floor(B/ncores)
n_resid = rep(0,ncores)
if((ncores*chunk.size)<B){nresid = B - ncores*chunk.size;
n_resid = rmultinom(1, size = nresid, prob = rep(1/ncores,ncores))}

KK=sts[4,2] #maximum steps of the outer loop (repetitions)

for(kk in 1:KK){ #repeated-Kfold-CV
  cat("\n |------- KK = ",kk," -------|")
  
  K=sts[5,2]; cvs = cvTools::cvFolds(NROW(dtm_matrix_filtered),K,type="random")[c(4,5)]
  
  cat("\n FLSA-svd..")
  out <- foreach(h=1:ncores, .combine = "combAbind",.export = export_data,.packages = export_pkgs) %dopar% {
    res = array(NA,dim=c(6,K,chunk.size+n_resid[h]))
    iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
    for(u in iid_core){
      xout = tryCatch({cv_evalTopics_flsa(K,cvs,dtm_filtered_X,dtm_matrix_filtered,words,ntopics[u],type="svd")},error=function(e){return(matrix(NA,nrow=6,ncol=K))})
      res[,,u-(h-1)*chunk.size] = xout
    }
    res
  }
  save(out,file = paste0("dataout/casestudy_",dataId,"_cv_results_flsa_",kk,".RData"))
  rm(out)
  
  cat("\n LDA..")
  out <- foreach(h=1:ncores, .combine = "combAbind",.export = export_data,.packages = export_pkgs) %dopar% {
    res = array(NA,dim=c(6,K,chunk.size+n_resid[h]))
    iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
    for(u in iid_core){
      xout = tryCatch({cv_evalTopics_lda(K,cvs,NULL,dtm_matrix_filtered,words,ntopics[u])},error=function(e){return(matrix(NA,nrow=6,ncol=K))})
      res[,,u-(h-1)*chunk.size] = xout
    }
    res
  }
  save(out,file = paste0("dataout/casestudy_",dataId,"_cv_results_lda_",kk,".RData"))
  rm(out)
  
  cat("\n STM..")
  out <- foreach(h=1:ncores, .combine = "combAbind",.export = export_data,.packages = export_pkgs) %dopar% {
    res = array(NA,dim=c(6,K,chunk.size+n_resid[h]))
    iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
    for(u in iid_core){
      xout = tryCatch({cv_evalTopics_stm(K,cvs,NULL,dtm_matrix_filtered,words,ntopics[u])},error=function(e){return(matrix(NA,nrow=6,ncol=K))})
      res[,,u-(h-1)*chunk.size] = xout
    }
    res
  }
  save(out,file = paste0("dataout/casestudy_",dataId,"_cv_results_stm_",kk,".RData"))
  rm(out)
  
  cat("\n LSA..")
  out <- foreach(h=1:ncores, .combine = "combAbind",.export = export_data,.packages = export_pkgs) %dopar% {
    res = array(NA,dim=c(6,K,chunk.size+n_resid[h]))
    iid_core = ((h-1)*chunk.size+1):((h*chunk.size)+n_resid[h])
    for(u in iid_core){
      xout = tryCatch({cv_evalTopics_lda(K,cvs,dtm_filtered_X,dtm_matrix_filtered,words,ntopics[u])},error=function(e){return(matrix(NA,nrow=6,ncol=K))})
      res[,,u-(h-1)*chunk.size] = xout
    }
    res
  }
  save(out,file = paste0("dataout/casestudy_",dataId,"_cv_results_lsa_",kk,".RData"))
  rm(out)
  
  cat("\n |------- -- - -- -------| \n")
}

doParallel::stopImplicitCluster(); parallel::stopCluster(cl)
cat("\n Done")

