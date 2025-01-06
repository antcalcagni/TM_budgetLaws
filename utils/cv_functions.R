
compute_measures = function(PW_T,pdT,dtm_matrix,fcm_matrix,topTerms,doclengths){
  noc = NCOL(pdT)
  cohs = tryCatch({coherence(dtm_data=dtm_matrix,fcm_data=fcm_matrix,topic_words_dist=t(PW_T),top_terms_topics=topTerms,average=TRUE,smth=1,probcoh = FALSE,metrics_text2vec=c("mean_logratio","mean_pmi","mean_difference"))[1:3]},
                   error=function(e){return(c(NA,NA,NA))})
  hlld = tryCatch({mean_inf.rm(mapply(function(j)dist_from_corpus(PW_T=PW_T[,j],dtm_data=dtm_matrix),1:noc))},error=function(e){return(NA)})
  m_arun10 = tryCatch({metric_arun2010(t(PW_T),pdT,doclengths)},error=function(e){return(NA)})
  cds = tryCatch(mean(cd_score(logbeta = safelog(t(PW_T)),dtm_matrix = dtm_matrix,w=0.7)),error=function(e){})
  
  return(c(cohs,hlld,m_arun10,cds))
  # mean_logratio (UMASS-like), mean_pmi (UCI-like), mean_difference, Hellinger dist, Arun_2010, avg CD-score
}



## FLSA
cv_evalTopics_flsa = function(K=5,cvs=NULL,Xw=NULL,dtm_data=NULL,words_dic=NULL,noc=NULL,...){
  Cvout = matrix(nrow=6,ncol=K); rownames(Cvout) = c("mean_logratio","mean_pmi","mean_diff","Helling_dist","Arun_2010","CD-score")
  for(k in 1:K){
    cat("\n CV..",k)
    train_iid = cvs$subsets[cvs$which==k]; test_iid = cvs$subsets[cvs$which!=k]
    train_model = FLSA(X=Xw[train_iid,],dtm=dtm_data[train_iid,],words=words_dic[train_iid],noc=noc,compute_stats=FALSE,...)
    fcm_test = Matrix::crossprod(Matrix::Matrix(dtm_data[test_iid,]),Matrix::Matrix(dtm_data[test_iid,]))
    pdT_test = FLSA(X=Xw[test_iid,],dtm=dtm_data[test_iid,],words=words_dic[test_iid],noc=noc,fc_validity=FALSE,compute_stats=FALSE,...)[[1]]$PD_T
    
    PW_T = train_model[[1]]$PW_T #nwords x ntopics
    Cvout[,k] = compute_measures(PW_T = PW_T,pdT = pdT_test,
                                 dtm_matrix = dtm_data[test_iid,],
                                 fcm_matrix = fcm_test,
                                 topTerms = train_model[[1]]$PW_T_sort,
                                 doclengths = apply(dtm_data[test_iid,],1,sum))
  }
  return(Cvout)
}


## LDA
cv_evalTopics_lda = function(K=5,cvs=NULL,Xw=NULL,dtm_data=NULL,words_dic=NULL,noc=NULL){
  Cvout = matrix(nrow=6,ncol=K); rownames(Cvout) = c("mean_logratio","mean_pmi","mean_diff","Helling_dist","Arun_2010","CD-score")
  for(k in 1:K){
    cat("\n CV..",k)
    train_iid = cvs$subsets[cvs$which==k]; test_iid = cvs$subsets[cvs$which!=k]
    fcm_test = Matrix::crossprod(Matrix::Matrix(dtm_data[test_iid,]),Matrix::Matrix(dtm_data[test_iid,]))
    train_model = topicmodels::LDA(slam::as.simple_triplet_matrix(dtm_data[train_iid,]),noc,method="Gibbs",control=list(burnin=500,iter=2500))
    pdT_test = topicmodels::LDA(slam::as.simple_triplet_matrix(dtm_data[test_iid,]),noc,method="Gibbs",control=list(burnin=500,iter=2500))@gamma
    
    PW_T = t(exp(train_model@beta))
    Cvout[,k] = compute_measures(PW_T = PW_T,pdT = pdT_test,
                                 dtm_matrix = dtm_data[test_iid,],
                                 fcm_matrix = fcm_test,
                                 topTerms = topicmodels::terms(train_model,30),
                                 doclengths = apply(dtm_data[test_iid,],1,sum))
  }
  return(Cvout)
}


## STM
# Note: this method does not allows for having empty docs! So, the number of words at each k can change.
cv_evalTopics_stm = function(K=5,cvs=NULL,Xw=NULL,dtm_data=NULL,words_dic=NULL,noc=NULL){
  Cvout = matrix(nrow=6,ncol=K); rownames(Cvout) = c("mean_logratio","mean_pmi","mean_diff","Helling_dist","Arun_2010","CD-score")
  for(k in 1:K){
    cat("\n CV..",k)
    train_iid = cvs$subsets[cvs$which==k]; test_iid = cvs$subsets[cvs$which!=k]
    train_model = stm::stm(quanteda::as.dfm(dtm_data[train_iid,]),K = noc,verbose = FALSE, init.type="Spectral",emtol = 1e-3,max.em.its = 25)
    words_currentlyUsed = train_model$vocab
    PW_T = t(exp(train_model$beta$logbeta[[1]]))
    Iid = apply(PW_T,2,order); topTerms = sapply(1:noc,function(j)words_currentlyUsed[Iid[,j]])[1:30,]
    fcm_test = Matrix::crossprod(Matrix::Matrix(dtm_data[test_iid,words_currentlyUsed]),Matrix::Matrix(dtm_data[test_iid,words_currentlyUsed]))
    pdT_test = stm::stm(quanteda::as.dfm(dtm_data[test_iid,words_currentlyUsed]),K = noc,verbose = FALSE, init.type="Spectral",emtol = 1e-3,max.em.its = 25)$theta
    
    Cvout[,k] = compute_measures(PW_T = PW_T,pdT = pdT_test,
                                 dtm_matrix = dtm_data[test_iid,words_currentlyUsed],
                                 fcm_matrix = fcm_test,
                                 topTerms = topTerms,
                                 doclengths = apply(dtm_data[test_iid,words_currentlyUsed],1,sum))
  }
  return(Cvout)
}


## LSA
cv_evalTopics_lsa = function(K=5,cvs=NULL,Xw=NULL,dtm_data=NULL,words_dic=NULL,noc=NULL){
  Cvout = matrix(nrow=6,ncol=K); rownames(Cvout) = c("mean_logratio","mean_pmi","mean_diff","Helling_dist","Arun_2010","CD-score")
  for(k in 1:K){
    cat("\n CV..",k)
    train_iid = cvs$subsets[cvs$which==k]; test_iid = cvs$subsets[cvs$which!=k]
    fcm_test = Matrix::crossprod(Matrix::Matrix(dtm_data[test_iid,]),Matrix::Matrix(dtm_data[test_iid,]))
    
    train_model = lsa::lsa(Xw[train_iid,],noc)
    train_model$PW_T = train_model$dk%*%solve(diag(apply(train_model$dk,2,sum))); train_model$PW_T[train_model$PW_T<=0]=1e-6
    train_model$PD_T = train_model$tk%*%solve(diag(apply(train_model$tk,2,sum))); train_model$PD_T[train_model$PD_T<=0]=1e-6
    train_model$PW_T_sort = mapply(function(j)names(train_model$PW_T[order(train_model$PW_T[,j],decreasing = TRUE),j][1:30]),1:noc) #most frequent terms for each topic
    pdT_test = lsa::lsa(Xw[test_iid,],noc)$tk; pdT_test=pdT_test%*%solve(diag(apply(pdT_test,2,sum))); pdT_test[pdT_test<=0]=1e-6
    
    
    PW_T = train_model$PW_T #nwords x ntopics
    Cvout[,k] = compute_measures(PW_T = PW_T,pdT = pdT_test,
                                 dtm_matrix = dtm_data[test_iid,],
                                 fcm_matrix = fcm_test,
                                 topTerms = train_model$PW_T_sort,
                                 doclengths = apply(dtm_data[test_iid,],1,sum))
  }
  return(Cvout)
}
