
cd_score = function(logbeta=NULL,dtm_matrix=NULL,M=10,w=0.5){
  
  noc = NROW(logbeta) #number of topics
  X = matrix(nrow = noc,ncol = 2)
  
  # computing exclusivity using the 'stm' approach
  wordcounts = as.matrix(apply(dtm_matrix,2,sum))
  X[,1] = calc_frex(logbeta,w,wordcounts,M) #exclusivity
  
  # computing semantic coherence using the 'stm' approach
  documents = quanteda::convert(quanteda::as.dfm(dtm_matrix),to="stm")[[1]]
  X[,2] = semantic_coherence(logbeta,documents,M) #semantic coherence
  
  # computing the CD-score as row-wise Eucl distance of the (normalized) matrix X
  X_normalized = apply(X,2,function(x)(x-min(x))/(max(x)-min(x)))
  cdz = apply(X_normalized,1,function(x)norm(x,type="2"))
  
  return(cdz)
}



semantic_coherence = function(logbeta=NULL, documents, M=10){ #adapted from the 'stm' library
  args <- asSTMCorpus(documents)
  documents <- args$documents
  
  { 
    beta <- logbeta
    #Get the Top N Words
    top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
    triplet <- doc.to.ijv(documents)
    mat <- slam::simple_triplet_matrix(triplet$i, triplet$j,triplet$v, ncol=NCOL(beta))
    result = semCoh1beta(mat, M, beta=beta)
    return(result)
  }
}

semCoh1beta <- function(mat, M, beta){
  #Get the Top N Words
  top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
  wordlist <- unique(as.vector(top.words))
  mat <- mat[,wordlist]
  mat$v <- ifelse(mat$v>1, 1,mat$v) #binarize
  
  #do the cross product to get co-occurrences
  cross <- slam::tcrossprod_simple_triplet_matrix(t(mat))
  
  #create a list object with the renumbered words (so now it corresponds to the rows in the table)
  temp <- match(as.vector(top.words),wordlist)
  labels <- split(temp, rep(1:nrow(beta), each=M))
  
  #Note this could be done with recursion in an elegant way, but let's just be simpler about it.
  sem <- function(ml,cross) {
    m <- ml[1]; l <- ml[2]
    log(.01 + cross[m,l]) - log(cross[l,l] + .01)
  }
  result <- vector(length=nrow(beta))
  for(k in 1:nrow(beta)) {
    grid <- expand.grid(labels[[k]],labels[[k]])
    colnames(grid) <- c("m", "l") #corresponds to original paper
    grid <- grid[grid$m > grid$l,]
    calc <- apply(grid,1,sem,cross)
    result[k] <- sum(calc)
  }
  return(result)
}

doc.to.ijv <- function(documents, fixzeroindex=TRUE) {
  #Turns our format into triplet format (can be zero indexed)
  indices <- unlist(lapply(documents, '[',1,)) #grab the first row
  if((0 %in% indices) & fixzeroindex) indices <- indices + 1 #if zero-indexed, fix it.
  counts <- lapply(documents, '[',2,)  #grab the second row but preserve the list structure for a moment
  VsubD <- unlist(lapply(counts,length)) #grab the number of unique words per document
  rowsums <- unlist(lapply(counts,sum)) #grab the number of tokens per documents
  docids <- rep(1:length(documents), times=VsubD) #add row numbers
  counts <- unlist(counts) #unlist the count structure
  #now we return using the convention for the simple triplet matrix,
  #plus the row sums which we use in DMR.
  return(list(i=as.integer(docids), j=as.integer(indices), v=as.integer(counts), rowsums=as.integer(rowsums)))
}


safelog <- function(x, min=-1000) { #reproduced from the 'stm' library
  out <- log(x)
  out[which(out< min)] <- min
  out
}

col.lse <- function(mat) { #reproduced from the 'stm' library
  matrixStats::colLogSumExps(mat)
}

calc_frex = function(logbeta, w=.5, wordcounts=NULL, M=10){ #adapted from the 'stm' library
  excl <- t(t(logbeta) - col.lse(logbeta))
  if(!is.null(wordcounts)) {
    #if word counts provided calculate the shrinkage estimator
    excl <- safelog(sapply(1:ncol(excl), function(j)stm::js.estimate(exp(excl[,j]), wordcounts[j])))
  } 
  freqscore <- apply(logbeta,1,rank)/ncol(logbeta)
  exclscore <- apply(excl,1,rank)/ncol(logbeta)
  frex <- 1/(w/freqscore + (1-w)/exclscore)
  iid = apply(frex,2,order,decreasing=TRUE)
  out = apply(sapply(1:NCOL(frex),function(j)frex[iid[,j],j][1:M]),2,sum)
  return(out)
}


