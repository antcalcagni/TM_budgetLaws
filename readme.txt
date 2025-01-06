
== Notes for the reader ================
This folder already contains preprocessed data that are ready to be analysed as shown in the paper. 
Therefore, to reproduce the (final) results described in the paper it is enough to run the script paper.R.
Instead, if one needs to run all the steps, including pre-processing and cross-validation for the case study, the following steps need to be executed:
    1. preprocessing.R
    2. dtm_generation.R
    3. paper.R
========================================


== Folder 'Rcode' ======================
It contains three main R files: 

+ preprocessing.R
    It takes as input raw files stored in 'raw_data' and save pre-processed corpora into 'data'.

+ dtm_generation.R
    It takes as input pre-processed corpora stored into 'data' and produces document-term matrices (TF: case 1; TF-IDF: case 2; Boolean: case 3) used for the cross-validation analysis 
    (case study).
+ main.R
    After selecting the type of data input (case 1, case 2,case 3), it produces the results of the descriptive, graphical, and model-based analyses described 
    in the paper.

Notes:
    The files abbreviazioni_giuridiche.csv and stopwords-it.txt are used internally by the routine preprocessing.R.
    The subfolder 'utilities' contains scripts (ie, utilities) used internally by the main procedures.
    The subfolder 'results' contains the results of the cross-validation analysis (aggregated results). Basically, it mostly contains the file also contained into 
    casestudy_crossValidation/results.
========================================


== Subfolder 'Rcode/casestudy_crossValidation' ==
datain:     folder containing the document-term matrix of the pre-processed corpus

dataout:    folder containing the results of the following routines:
                + casestudy_parallel_cv_all.R       (for document-term matrices weigthed using TF --case 1-- and TF-IDF -- case 2)
                + casestudy_parallel_cv_all_binary.R   (for document-term matrices weighted using Boolean approach --case 3)

results:    folder containing the results of the following routine:
                + casestudy_parallel_cv_aggregate_repeatedCV.R    
                
run.sh             bash file to run casestudy_parallel_cv_all.R on a SLURM-based HPC
run_binary.sh      bash file to run casestudy_parallel_cv_all_binary.R on a SLURM-based HPC
=================================================



