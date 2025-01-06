
# Initial settings --------------------------------------------------------
rm(list=ls()); graphics.off(); options(warn = -1)
setwd("/home/antonio/MEGA/Lavoro_sync/My papers/Submitted/Disentangling Industrial Priorities in Budget Legislation_A Comparative Topic Modeling Analysis of Italian Policies/Rcode")
source("utils/utils.R"); source("utils/FLSA.R")
library(tm); library(quanteda); library(stringr); library(xtable)


# Raw data (statistics) ---------------------------------------------------
txt = list()
txt[[1]] <- xlsx::read.xlsx(file = "raw_data/2020_LEGGE 30 dicembre 2020 n178.xlsx",header = FALSE,encoding = "UTF-8",sheetIndex = 1)
txt[[2]] <- xlsx::read.xlsx(file = "raw_data/2021_LEGGE 30 dicembre 2021 n234.xlsx",header = FALSE,encoding = "UTF-8",sheetIndex = 1)
txt[[3]] <- xlsx::read.xlsx(file = "raw_data/2022_LEGGE 29 dicembre 2022 n197.xlsx",header = FALSE,encoding = "UTF-8",sheetIndex = 1)
txt[[4]] <- xlsx::read.xlsx(file = "raw_data/2023_LEGGE 30 dicembre 2023 n213.xlsx",header = FALSE,encoding = "UTF-8",sheetIndex = 1)

txt[[1]] <- txt[[1]]$X2
txt[[2]] <- txt[[2]]$X2
txt[[3]] <- txt[[3]]$X2
txt[[4]] <- txt[[4]]$X2

txt_all <- unlist(txt)
txt_all <- stringr::str_to_lower(string = txt_all)
txt_all_1 <- gsub("[[:punct:]'`\"@]", "", txt_all)

length(txt_all) #number of documents
words = unlist(str_split(txt, "\\W+")) #words
word_tokens = length(words)
word_types = length(unique(words)) #number of word-types
ttr = word_types / word_tokens*100 #TTr
hapaxes = sum(table(words) == 1)/word_types*100 #hapaxes


# Preprocessed data (statistics) ------------------------------------------
dataId = 3 #Boolean transformation
load(paste0("results/casestudy_data_",dataId,".RData"))

# Repeated 10-fold CV -----------------------------------------------------
# The procedure has been run on a HPC remote system (see: 'casestudy_parallel_cv_all.R' and 'casestudy_parallel_cv_all_bin.R').
# The results are: 
#     - results/casestudy_cv_results_[...].RData
# The results have been arranged into arrays of 6 (measures) x 10 (folds) x 30 (topics) x 25 (repetitions)

dataId = 3

OUT = list()
load(paste0("results/casestudy_",dataId,"_cv_results_flsa.RData"))
ntopics = dim(out_kk)[3]+1
nocs = 2:ntopics
K=dim(out_kk)[2]
cvres_flsa = out_kk; OUT[[1]] = out_kk

load(paste0("results/casestudy_",dataId,"_cv_results_lda.RData"))
cvres_lda = out_kk; OUT[[2]] = out_kk

load(paste0("results/casestudy_",dataId,"_cv_results_stm.RData"))
cvres_stm = out_kk; OUT[[3]] = out_kk

load(paste0("results/casestudy_",dataId,"_cv_results_lsa.RData"))
cvres_lsa = out_kk; OUT[[4]] = out_kk

#load(paste0("data/casestudy_data_",dataId,".RData"))


# Results -----------------------------------------------------------------
source("utils/kneePoint.R")

## Figure 1 ##

cls = c("#36648B", #steelblue4 
        "#CD4F39", #tomato3 (red-like)
        "#68228B", #darkorchid4
        "#8B4726", #sienna4
        "#8B8B00") #yellow4

ms = c("UMass-like","UCI-like","MeanDiff","HellingerDist","","CD-score")
algs = c("FLSA","LDA","CTM","LSA")
sg = c(-1,1,1,1,-1,-1)
lwdx=2

## Avg Metrics
Ntopics = matrix(nrow=4,ncol=6); colnames(Ntopics)=ms; rownames(Ntopics)=algs
Zbds = mapply(function(g)unlist(mapply(function(m)summary(OUT[[g]][m,,,])[c("Min.","Max.")],1:6)),1:4,SIMPLIFY = FALSE) #bounds for the six metrics 

#x11();
tikzDevice::tikz(file='../paper/figures/fig1.tex',width=8.5,height=6.5)
par(mfcol=c(4,5),mar=c(5,6,4,2))
par(mfcol=c(4,4),mar=c(5,6,4,2))
k=0; j=0; ks=seq(1,24,by=6)
#for(m in c(1:4,6)){ #loop over metrics
for(m in c(1:4)){ #loop over metrics
  k=k+1
  for(g in c(4,1,2,3)){ #loop over methods
    Z1 = apply(OUT[[g]][m,,,],c(2),function(x)c(quantile(x,c(0.25,0.75),na.rm=TRUE),mean(x,na.rm=TRUE)))
    
    plot(nocs,Z1[3,],bty="n",type="l",lwd=lwdx,ylim = c(Zbds[[g]][1,m],Zbds[[g]][2,m]),main="",xlab="",ylab="",col=cls[g]);
    j=j+1;if(j%in%seq(1,20,by=4)){title(ms[m])}
    polygon(c(nocs, rev(nocs)), c(Z1[2,], rev(Z1[1,])),col = "#EEE9E9",border = FALSE); lines(nocs,Z1[3,],lwd=lwdx,col=cls[g])
    px1=kneePoint(x = nocs,y = Z1[3,],plot = FALSE,df = 1,bty="n",sign = sg[m],xQuery = nocs)
    abline(v = c(px1),col=cls[g],lty=2,lwd=1.25); text(c(px1)+2,c(Z1[3,1]),labels = c(px1),cex = 1.25,col=cls[g])
    if(sum(k==ks)){mtext(algs[g], side = 2, line = 3, las = 0)}
    Ntopics[g,m] = px1
  }
}
dev.off()


## Table 1 ##
Ntopics = Ntopics[,-5]; 
Ntopics = cbind(Ntopics,apply(Ntopics[,1:5],1,max)); colnames(Ntopics)[6]="max"
Xtab = Ntopics[c(4,1,2,3),]
Xtab_tex = xtable::xtable(Xtab)
attributes(Xtab_tex)$caption = ""
attributes(Xtab_tex)$label = "tab1"
attributes(Xtab_tex)$align = rep("c",length(attributes(Xtab_tex)$align))
print.xtable(Xtab_tex,table.placement = "h!",sanitize.text.function = function(x){x})


## Run the methods again on the selected number of topics ##
OUT=vector("list",4)

# FLSA
out_flsa = FLSA(X=as.matrix(dtm_filtered),fcm = fcm_matrix_filtered,dtm=dtm_matrix_filtered,words=words,noc=Ntopics[1,6],compute_stats=TRUE,type="svd")
OUT[[1]]$PW_T = out_flsa$FLSA$PW_T
OUT[[1]]$pt =   out_flsa$FLSA$PT
OUT[[1]]$exec = round(exclusive_term_ratio(out_flsa)$normalized,2)
OUT[[1]]$topic_coherence = out_flsa$stats$topic_coherence

write.csv(out_flsa$FLSA$Frex_W_T_sort,file = paste0("results/Frexs_",dataId,"_fLSA_K",Ntopics[1,6],".csv"))


# LDA
out_lda = topicmodels::LDA(slam::as.simple_triplet_matrix(dtm_matrix_filtered),Ntopics[2,6],method="Gibbs",control=list(burnin=500,iter=2500))
PW_T = t(exp(out_lda@beta)); PD_T = out_lda@gamma; rownames(PW_T) = words
topic_coherence = coherence(dtm = dtm_matrix_filtered,fcm = fcm_matrix_filtered,topic_words_dist = t(PW_T),top_terms_topics = topicmodels::terms(out_lda,30))
doc_lengths = apply(dtm_matrix_filtered,1,sum); 
PT_unno = apply(PD_T*doc_lengths%*%matrix(1,1,Ntopics[2,6]),2,sum)
OUT[[2]]$PW_T = PW_T
OUT[[2]]$pt =   PT_unno/sum(PT_unno)
OUT[[2]]$exec = apply(topic_coherence,2,mean)
OUT[[2]]$topic_coherence = topic_coherence

PW_T_frex = stm::calcfrex(logbeta = log(t(PW_T)),wordcounts = doc_lengths); rownames(PW_T_frex) = rownames(PW_T)
Frex_sorted = mapply(function(j)names(PW_T_frex[order(PW_T_frex[,j],decreasing = TRUE),j][1:30]),1:Ntopics[2,6]) #most frequent terms for each topic
colnames(Frex_sorted)=paste0("topic",1:Ntopics[2,6]); Frex_W_T_sort = Frex_sorted
write.csv(Frex_W_T_sort,file = paste0("results/Frexs_",dataId,"_LDA_K",Ntopics[2,6],".csv"))

rm(PW_T,PD_T,topic_coherence,doc_lengths,PT_unno)

# CTM
out_stm = stm::stm(quanteda::as.dfm(dtm_matrix_filtered),K = Ntopics[3,6],verbose = FALSE, init.type="Spectral",emtol = 1e-3,max.em.its = 100)
PW_T = t(exp(out_stm$beta$logbeta[[1]])); PD_T = out_stm$theta; rownames(PW_T) = words
words_currentlyUsed = out_stm$vocab; Iid = apply(PW_T,2,order); topTerms = sapply(1:Ntopics[3,6],function(j)words_currentlyUsed[Iid[,j]])[1:30,]
topic_coherence = coherence(dtm = dtm_matrix_filtered,fcm = fcm_matrix_filtered,topic_words_dist = t(PW_T),top_terms_topics = topTerms)
doc_lengths = apply(dtm_matrix_filtered,1,sum); 
PT_unno = apply(PD_T*doc_lengths%*%matrix(1,1,Ntopics[3,6]),2,sum)
OUT[[3]]$PW_T = PW_T
OUT[[3]]$pt =   PT_unno/sum(PT_unno)
OUT[[3]]$exec = apply(topic_coherence,2,mean)
OUT[[3]]$topic_coherence = topic_coherence

PW_T_frex = stm::calcfrex(logbeta = log(t(PW_T)),wordcounts = doc_lengths); rownames(PW_T_frex) = rownames(PW_T)
Frex_sorted = mapply(function(j)names(PW_T_frex[order(PW_T_frex[,j],decreasing = TRUE),j][1:30]),1:Ntopics[3,6]) #most frequent terms for each topic
colnames(Frex_sorted)=paste0("topic",1:Ntopics[3,6]); Frex_W_T_sort = Frex_sorted
write.csv(Frex_W_T_sort,file = paste0("results/Frexs_",dataId,"_STM_K",Ntopics[3,6],".csv"))
out_stm$Frex_sorted = Frex_sorted

rm(PW_T,PD_T,topic_coherence,doc_lengths,PT_unno)


# LSA
out_lsa = lsa::lsa(as.matrix(dtm_filtered),Ntopics[4,6])
PW_T = out_lsa$dk%*%solve(diag(apply(out_lsa$dk,2,sum))); PW_T[PW_T<=0]=1e-6
PD_T = out_lsa$tk%*%solve(diag(apply(out_lsa$tk,2,sum))); PD_T[PD_T<=0]=1e-6
PW_T_sort = mapply(function(j)names(PW_T[order(PW_T[,j],decreasing = TRUE),j][1:30]),1:Ntopics[4,6]) #most frequent terms for each topic
topic_coherence = coherence(dtm = dtm_matrix_filtered,fcm = fcm_matrix_filtered,topic_words_dist = t(PW_T),top_terms_topics = PW_T_sort)
doc_lengths = apply(dtm_matrix_filtered,1,sum); 
PT_unno = apply(PD_T*doc_lengths%*%matrix(1,1,Ntopics[4,6]),2,sum)
OUT[[4]]$PW_T = PW_T
OUT[[4]]$pt =   PT_unno/sum(PT_unno)
OUT[[4]]$exec = apply(topic_coherence,2,mean)
OUT[[4]]$topic_coherence = topic_coherence

PW_T_frex = stm::calcfrex(logbeta = log(t(PW_T)),wordcounts = doc_lengths); rownames(PW_T_frex) = rownames(PW_T)
Frex_sorted = mapply(function(j)names(PW_T_frex[order(PW_T_frex[,j],decreasing = TRUE),j][1:30]),1:Ntopics[4,6]) #most frequent terms for each topic
colnames(Frex_sorted)=paste0("topic",1:Ntopics[4,6]); Frex_W_T_sort = Frex_sorted
write.csv(Frex_W_T_sort,file = paste0("results/Frexs_",dataId,"_LSA_K",Ntopics[4,6],".csv"))

rm(PW_T,PD_T,topic_coherence,doc_lengths,PT_unno)


## PCP on the estimated P_DT ##

rpca_out = rpca::rpca(M = out_flsa$FLSA$PT_D,term.delta = 1e-05) #fLSA
i1a = norm(rpca_out$S,type="F")/norm(rpca_out$L+rpca_out$S,type="F"); print(i1a)
i1b =c(length(rpca_out$L.svd$d),NCOL(rpca_out$L)); print(i1b)
rpca_flsa = rpca_out

rpca_out = rpca::rpca(M = out_lda@gamma[,],term.delta = 1e-05) #LDA
i2a = norm(rpca_out$S,type="F")/norm(rpca_out$L+rpca_out$S,type="F"); print(i2a)
i2b =c(length(rpca_out$L.svd$d),NCOL(rpca_out$L)); print(i2b)
rpca_lda = rpca_out

rpca_out = rpca::rpca(M = out_stm$theta[,],term.delta = 1e-05) #CTM
i3a = norm(rpca_out$S,type="F")/norm(rpca_out$L+rpca_out$S,type="F"); print(i3a)
i3b =c(length(rpca_out$L.svd$d),NCOL(rpca_out$L)); print(i3b)
rpca_ctm = rpca_out

rpca_out = rpca::rpca(M = out_lsa$tk%*%solve(diag(apply(out_lsa$tk,2,sum))),term.delta = 1e-05) #LSA
i4a = norm(rpca_out$S,type="F")/norm(rpca_out$L+rpca_out$S,type="F"); print(i4a)
i4b =c(length(rpca_out$L.svd$d),NCOL(rpca_out$L)); print(i4b)
rpca_lsa = rpca_out

## Table 2 ##
CL = round(1-c(i4a,i1a,i2a,i3a),3)
RL = round(1-c(i4b[1]/i4b[2],i1b[1]/i1b[2],i2b[1]/i2b[2],i3b[1]/i3b[2]),3)
rr = rbind(i4b,i1b,i2b,i3b)
CLtab = cbind(CL,rr,RL); rownames(CLtab) = c("LSA","fLSA","LDA","CTM")
colnames(CLtab) = c("CL","r(PDT)","r(X)","RL")
Xtab_tex = xtable::xtable(CLtab)
attributes(Xtab_tex)$caption = ""
attributes(Xtab_tex)$label = "tab2"
attributes(Xtab_tex)$align = rep("c",length(attributes(Xtab_tex)$align))
print.xtable(Xtab_tex,table.placement = "h!",sanitize.text.function = function(x){x})

## Figure 2 ##
tikzDevice::tikz(file='../paper/figures/fig2.tex',width=6.5,height=6.5)
par(mfrow=c(2,2),mar=c(5,6,4,2))

x=apply(rpca_lsa$L,1,sum)/apply(rpca_lsa$L+rpca_lsa$S,1,sum)*100
out=smooth.spline(x,df = median(x)); x=out$y
plot(x,1:length(x),type="l",xlim=c(0,100),bty="n",axes = FALSE,xlab = "",ylab = "")
polygon(c(x,rep(0,length(x))),c(1:length(x), rev(1:length(x))), col = cls[4],border = FALSE);
polygon(c(x,rev(100-x+x)),c(1:length(x), rev(1:length(x))),col = "#B0C4DE",border = FALSE);
abline(v = 50,lty=2,col="#6E7B8B"); axis(side = 1,at = c(0,50,100)); title("(A) LSA",adj=0)

x=apply(rpca_flsa$L,1,sum)/apply(rpca_flsa$L+rpca_flsa$S,1,sum)*100
out=smooth.spline(x,df = median(x)); x=out$y
plot(x,1:length(x),type="l",bty="n",xlim = c(0,100),axes = FALSE,xlab = "",ylab = "")
polygon(c(x,rep(0,length(x))),c(1:length(x), rev(1:length(x))), col = cls[1],border = FALSE);
polygon(c(x,rev(100-x+x)),c(1:length(x), rev(1:length(x))),col = "#B0C4DE",border = FALSE);
abline(v = 50,lty=2,col="#6E7B8B"); axis(side = 1,at = c(0,50,100)); title("(B) fLSA",adj=0)

x=apply(rpca_lda$L,1,sum)/apply(rpca_lda$L+rpca_lda$S,1,sum)*100
out=smooth.spline(x,df = median(x)); x=out$y
plot(x,1:length(x),type="l",xlim=c(0,100),bty="n",axes = FALSE,xlab = "",ylab = "")
polygon(c(x,rep(0,length(x))),c(1:length(x), rev(1:length(x))), col = cls[2],border = FALSE);
polygon(c(x,rev(100-x+x)),c(1:length(x), rev(1:length(x))),col = "#B0C4DE",border = FALSE);
abline(v = 50,lty=2,col="#6E7B8B"); axis(side = 1,at = c(0,50,100)); title("(C) LDA",adj=0)

x=apply(rpca_ctm$L,1,sum)/apply(rpca_ctm$L+rpca_ctm$S,1,sum)*100
out=smooth.spline(x,df = median(x)); x=out$y
plot(x,1:length(x),type="l",xlim=c(0,100),bty="n",axes = FALSE,xlab = "",ylab = "")
polygon(c(x,rep(0,length(x))),c(1:length(x), rev(1:length(x))), col = cls[3],border = FALSE);
polygon(c(x,rev(100-x+x)),c(1:length(x), rev(1:length(x))),col = "#B0C4DE",border = FALSE);
abline(v = 50,lty=2,col="#6E7B8B"); axis(side = 1,at = c(0,50,100)); title("(D) CTM",adj=0)
dev.off()

## Evaluating the CTM and fLSA-based topic solutions ##

FQ = matrix(0,21,15); fq = matrix(0,21,1)
for(j in 1:21){
  y=out_stm$Frex_sorted[,j]
  A=t(mapply(function(i)apply(out_flsa$FLSA$Frex_W_T_sort,2,function(x)sum(y[i]==x)),1:30))
  FQ[j,] = apply(A,2,sum)
  fq[j] = sum(apply(A,1,sum))/30
}
rownames(FQ)=as.character(1:21); colnames(FQ)=as.character(1:15)

100*sum(apply((FQ),1,sum))/(30*21) #about 11% of FREXs provided by CTM are allocated into fLSA topic solutions 
x=apply(1*(FQ>0),1,sum)
x[sort(x,index.return=TRUE,decreasing = TRUE)$ix] #most allocated CTM-based topics (ie, 6,1,16,20..)
A = FQ[c(2,11,15),]; A=A[,as.numeric(which(apply(A>0,2,sum)==TRUE))] #allocations of CTM-based 'Industrial topics'

x=apply(1*(FQ>0),2,sum)
x[sort(x,index.return=TRUE,decreasing = TRUE)$ix] #fLSA-based topics which receive from the CTM-based solution (eg, topic 9 Industrial receives many entries)
A = FQ[c(9,11),]; A=A[,as.numeric(which(apply(A>0,2,sum)==TRUE))] #entries of CTM-based topics into fLSA-based Industrial topics

px=apply(FQ,1,sum)/sum(FQ) #distribution of the CTM-based topic allocations
x11(); plot(px,type="h")
sum(-px*log(px))/(log(21))

px=apply(FQ,2,sum)/sum(FQ) #distribution of the fLSA-based entries
x11(); plot(px,type="h")
sum(-px*log(px))/(log(15))

mean(apply(FQ,1,max)/apply(FQ,1,sum)) #purity index for CTM wrt fLSA topic solutions
#High Purity Index (close to 1): This indicates that each topic in Technique 1 predominantly aligns with a single topic in Technique 2. In other words, each topic in Technique 1 has a clear counterpart in Technique 2, suggesting that both techniques have identified similar thematic structures.
#Low Purity Index: This suggests that each topic in Technique 1 is distributed across multiple topics in Technique 2. It implies that Technique 1’s topics do not have a clear one-to-one mapping with Technique 2’s topics, indicating a difference in how the techniques interpret the thematic structure of the corpus.

mean(apply(1*(FQ>0),1,sum)/15) #Entanglement index
#A high Topic Entanglement Index (close to 1) indicates that topics from Technique 1 are heavily interconnected with multiple topics from Technique 2. 
# This suggests thematic overlap and complexity, meaning that the topics identified are not well-defined and likely represent broader themes that span multiple categories in the other technique.
#A low TEI indicates that topics are more distinct and have fewer connections to topics in the other technique. 
# This suggests that the topic modeling techniques have effectively captured distinct themes without much overlap, leading to clearer thematic delineation.

mean(apply(FQ,1,function(x)-sum(x/15*log(x/15+1e-9))/log(15))) #average normalized-entropy wrt CTM departures to fLSA topics
#A lower value (close to zero) indicates that most of the departures go into a single fLSA-based topic
#A higher value (close to one) indicates that most of the departures go into all the fLSA-based topics


## Figure 3 ##

datax = as.data.frame(as.table(FQ))
colnames(datax) = c("CTM", "fLSA", "fq")
datax$fqnorm = datax$fq / max(datax$fq)

library(viridis)

png(file='../paper/figures/fig3.png',units = "cm",width=19.5,height=11.5,res=1000)
g1 = ggplot(datax, aes(axis1 = CTM, axis2 = fLSA, y = fqnorm)) +
  geom_alluvium(aes(fill = CTM,width=fqnorm*0.01)) +
  geom_stratum(color = NA,size=10,alpha=1e-4) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum),size = 4,colour = label_color)) +
  scale_x_discrete(limits = c("CTM", "fLSA"), expand = c(0.05, 0.05)) +
  labs(title = "",x = "",y = "") +
  scale_fill_viridis_d(option = "turbo",alpha = 0.5)+
  theme_bw() + ggtitle("(A) ") +
  theme(legend.position = "none",axis.title.y = element_text(size=0),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=0),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(size=0))


cls = rep("NA",21); cls[c(2,11,15)] = "lightgray"
datax$label_color = ifelse(datax$fLSA%in%c(9,11) ,"red",NA)

g2 = ggplot(datax, aes(axis1 = CTM, axis2 = fLSA, y = fqnorm)) +
  geom_alluvium(aes(fill = CTM,width=fqnorm*0.01)) +
  geom_stratum(color = NA,size=10,alpha=1e-4) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum),size = 4,colour = label_color)) +
  scale_x_discrete(limits = c("CTM", "fLSA"), expand = c(0.05, 0.05)) +
  labs(title = "",x = "",y = "") +
  scale_fill_manual(values = cls) +
  theme_bw() + ggtitle("(B)") +
  theme(legend.position = "none",axis.title.y = element_text(size=0),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=0),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(size=0))

gridExtra::grid.arrange(g1,g2,ncol=2)
dev.off()


##
year = unlist(lapply(strsplit(x = rownames(dtm_matrix_filtered),split = "_"),function(x)x[[1]]))

P_DT1 = out_flsa$FLSA$PT_D[,c(9,11,4,5,7)]; #P_DT1 = P_DT1/apply(P_DT1,1,sum)
A = rbind(table(apply(P_DT1,1,which.max)[year=="2020"]),
          table(apply(P_DT1,1,which.max)[year=="2021"]),
          table(apply(P_DT1,1,which.max)[year=="2022"]),
          table(apply(P_DT1,1,which.max)[year=="2023"]))
A = A/sum(apply(A,1,sum))

P_DT1 = out_stm$theta[,c(2,11,15,3)]; #P_DT1 = P_DT1/apply(P_DT1,1,sum)
B = rbind(table(apply(P_DT1,1,which.max)[year=="2020"]),
          table(apply(P_DT1,1,which.max)[year=="2021"]),
          table(apply(P_DT1,1,which.max)[year=="2022"]),
          table(apply(P_DT1,1,which.max)[year=="2023"]))
B = B/sum(apply(B,1,sum))
B = round(cbind(A,B),3)
rownames(B) = c("2020","2021","2022","2023")

Xtab_tex = xtable::xtable(B)
attributes(Xtab_tex)$caption = ""
attributes(Xtab_tex)$label = "tab5"
attributes(Xtab_tex)$align = rep("c",length(attributes(Xtab_tex)$align))
print.xtable(Xtab_tex,table.placement = "h!",sanitize.text.function = function(x){x})


## Figure 4 ##

A = rbind(table(apply(out_flsa$FLSA$PT_D[year=="2020",],1,which.max)),
          table(apply(out_flsa$FLSA$PT_D[year=="2021",],1,which.max)),
          table(apply(out_flsa$FLSA$PT_D[year=="2022",],1,which.max)),
          table(apply(out_flsa$FLSA$PT_D[year=="2023",],1,which.max)))
A = A[,c(1,2,12,9,11,4,5,7,6,10,13,14,3,8,15)]

B = rbind(table(apply(out_stm$theta[year=="2020",],1,which.max)),
          table(apply(out_stm$theta[year=="2021",],1,which.max)),
          table(apply(out_stm$theta[year=="2022",],1,which.max)),
          table(apply(out_stm$theta[year=="2023",],1,which.max)))
B = B[,c(8,9,10,21,2,11,15,3,4,6,12,13,17,18,1,5,7,14,16,19,20)]

cls = c("#8FBC8F","#CDBE70","#A4D3EE","#CDB5CD")

tikzDevice::tikz(file='../paper/figures/fig4.tex',width=25,height=8.5)
par(mfrow=c(1,2),mar=c(2,2,4,2))
barplot(A,col = cls,border = "white",ylim = c(0,550),cex.names = 2,cex.axis = 2); title("(A) fLSA",adj=0,cex.main=3)
barplot(B,col = cls,border = "white",ylim = c(0,550),cex.names = 2,cex.axis=1.5); title("(B) CTM",adj=0,cex.main=3)
add_legend("topright",fill = cls,legend = c("2020","2021","2022","2023"),border = FALSE,bty = "n",ncol = 1,cex=3)
dev.off()
