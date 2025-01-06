#rm(list=ls()); graphics.off(); options(warn = -1)

dataId=1
KK=25
out_kk = array(dim = c(6,10,30,KK)) #metrics x Kfold x ntopics x KK 
mtds = c("flsa","lda","stm","lsa")

for(i in 1:length(mtds)){
  for(kk in 1:KK){
    load(paste0("/home/antonio.calcagni/storage/mef_continued/dataout/casestudy_",dataId,"_cv_results_",mtds[i],"_",kk,".RData"))
    out_kk[,,,kk] = out
  }
  #out=apply(out_kk,c(1,2,3),mean)
  save(out_kk,file = paste0("/home/antonio.calcagni/storage/mef_continued/results/casestudy_",dataId,"_cv_results_",mtds[i],".RData"))
}
