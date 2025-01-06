# Initial settings --------------------------------------------------------
rm(list=ls()); graphics.off(); options(warn = -1)
setwd("/home/antonio/MEGA/Lavoro_sync/My papers/Submitted/Disentangling Industrial Priorities in Budget Legislation_A Comparative Topic Modeling Analysis of Italian Policies/Rcode")
source("utils/utils.R"); source("utils/FLSA.R")
library(tm); library(quanteda); library(stringr)


# Data --------------------------------------------------------------------
corpus = list()
for(i in 1:4){
  txt_paths = tm::DirSource(directory = paste0("data/202",i-1,"/out_corpora/"),mode = "text")
  temp_corpus = tm::VCorpus(txt_paths); names(temp_corpus)=paste0("202",i-1,"_",names(temp_corpus))
  corpus[[i]]=temp_corpus
}
corpus = c(corpus[[1]],corpus[[2]],corpus[[3]],corpus[[4]])
summary(corpus)
N=length(corpus)

# Dtm and Vocab with raw data
dtm = tm::DocumentTermMatrix(corpus); dtm_matrix = as.matrix(dtm)
out = apply(dtm_matrix,2,sum); 
Vocab = data.frame(term=names(out),freq=as.numeric(out),cums=cumsum(as.numeric(out))); Vocab = Vocab[order(Vocab$freq,decreasing = FALSE),]; head(Vocab)
Vocab$freq_rel = round(Vocab$freq/sum(Vocab$freq),4); Vocab$cums_rel = round(Vocab$cums/sum(Vocab$freq),4)
Vocab = Vocab[order(Vocab$freq,decreasing = TRUE),]; head(Vocab)
M_raw = NROW(Vocab)

# Save and manually mark words that need to be removed 
#Vocab$keep = 1
#write.csv(x = Vocab,file = "data/dictionary.csv",dec = ".")
X=read.csv(file = "data/dictionary.csv")[c(1,7)]

#x11(); plot(1:M,Vocab$freq,type="h",bty="n")
summary(Vocab$freq)
hpx = sum(Vocab$freq==1)/M_raw*100

# Removing words appearing rarely
length(Vocab$term[Vocab$freq<9])/M_raw
1-length(Vocab$term[Vocab$freq<9])/M_raw

# Filter-out the dtm matrix
jjd = c(X[!X[,2],1], #words marked manually
        as.numeric(which(apply(dtm_matrix,2,sum)<8)), #remove rare words
        grep(x = colnames(dtm_matrix),pattern = "per_cento"),
        grep(x = colnames(dtm_matrix),pattern = "anno_"),
        grep(x = colnames(dtm_matrix),pattern = "unità"),
        grep(x = colnames(dtm_matrix),pattern = "euro"),
        grep(x = colnames(dtm_matrix),pattern = "comma|commi|lett"),
        grep(x = colnames(dtm_matrix),pattern = "[0-9]{1,3}[a-z]{1}"), #posizioni economiche
        grep(x = colnames(dtm_matrix),pattern = "\\b[0-9]{4}-[0-9]{4}\\b") #years
)
length(jjd)/M_raw

dtm_matrix_filtered = dtm_matrix[,setdiff(1:M_raw,jjd)] #direction: keep
dtm_matrix_filtered = dtm_matrix_filtered[which(apply(dtm_matrix_filtered,1,sum)>0),] #keep documents with at least 1 word!
out = apply(dtm_matrix_filtered,2,sum); 
Vocab_filtered = data.frame(term=names(out),freq=as.numeric(out)); Vocab_filtered = Vocab_filtered[order(Vocab_filtered$freq,decreasing = TRUE),]; head(Vocab_filtered)
M = NROW(Vocab_filtered); 
summary(Vocab_filtered$freq)

# Note:
##_1: Tf
##_2: TfIdf
##_3: Binary
## dtm_matrix_filtered: unweighted dtm represented as matrix
## dtm_filtered: weighted dtm represented as 'tm' matrix

dtm_filtered = as.DocumentTermMatrix(dtm_matrix_filtered,weighting = tm::weightTf) 
fcm_matrix_filtered = t(dtm_matrix_filtered)%*%dtm_matrix_filtered 
words = colnames(dtm_matrix_filtered) 
save(dtm_matrix_filtered,dtm_filtered,fcm_matrix_filtered,words,file = "data/casestudy_data_1.RData") #Tf

dtm_filtered = as.DocumentTermMatrix(dtm_matrix_filtered,weighting = tm::weightTfIdf)
save(dtm_matrix_filtered,dtm_filtered,fcm_matrix_filtered,words,file = "data/casestudy_data_2.RData") #TfIdf

dtm_filtered = as.DocumentTermMatrix(dtm_matrix_filtered,weighting = tm::weightBin)
save(dtm_matrix_filtered,dtm_filtered,fcm_matrix_filtered,words,file = "data/casestudy_data_3.RData") #Binary
