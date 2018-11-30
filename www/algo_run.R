algo_run <- function(name_disease_1,
                     name_disease_2,
                     name_disease_3,
                     name_disease_4,
                     name_disease_5,
                     
                     prevalence_dis_1,
                     prevalence_dis_2,
                     prevalence_dis_3,
                     prevalence_dis_4,
                     prevalence_dis_5,
                     
                     sensitivity_dis_1,
                     sensitivity_dis_2,
                     sensitivity_dis_3,
                     sensitivity_dis_4,
                     sensitivity_dis_5,
                     
                     specificity_dis_1,
                     specificity_dis_2,
                     specificity_dis_3,
                     specificity_dis_4,
                     specificity_dis_5,
                     prevalence_dis_other){
  
  
  
  # for testing
  # name_disease_1 = "Malaria"
  # name_disease_2 = "Dengue"
  # name_disease_3 = "Scrub"
  # name_disease_4 = "Typhoid"
  # name_disease_5 = "Leptospira"
  # prevalence_dis_1 = 3
  # prevalence_dis_2 = 7
  # prevalence_dis_3 = 4
  # prevalence_dis_4 = 1
  # prevalence_dis_5 = 4
  # sensitivity_dis_1 = 95
  # sensitivity_dis_2 = 84
  # sensitivity_dis_3 = 73
  # sensitivity_dis_4 = 69
  # sensitivity_dis_5 = 71
  # specificity_dis_1 = 95
  # specificity_dis_2 = 94
  # specificity_dis_3 = 97
  # specificity_dis_4 = 90
  # specificity_dis_5 = 65
  # prevalence_dis_other = 0
  
  
  
  
  x <- 10000  # number of simulations
  
  r <- sum(c(name_disease_1, name_disease_2, name_disease_3, name_disease_4, name_disease_5) != "") # number of diseases
  n <- r # number of tests which is the same as the number of diseases
  name_diseases <- c(name_disease_1, name_disease_2, name_disease_3, name_disease_4, name_disease_5)[1:r]
  
  
  prev <- c(prevalence_dis_1, prevalence_dis_2, prevalence_dis_3, prevalence_dis_4, prevalence_dis_5) # prev of 5 conditions under consideration
  sn <- c(sensitivity_dis_1, sensitivity_dis_2, sensitivity_dis_3, sensitivity_dis_4, sensitivity_dis_5)/100 # corresponding sensitivity
  sp <- c(specificity_dis_1, specificity_dis_2, specificity_dis_3, specificity_dis_4, specificity_dis_5)/100 # corresponding specificity
  
  prevx <- prevalence_dis_other  # prev of other illnesses where tests are done in reality
  
  perm <- permutations(n, r, v = 1:n) # possible combinations
  
  #data frame of required data
  perm_name <- perm
  perm_name[perm == 1] <- name_diseases[1]
  perm_name[perm == 2] <- name_diseases[2]
  perm_name[perm == 3] <- name_diseases[3]
  perm_name[perm == 4] <- name_diseases[4]
  perm_name[perm == 5] <- name_diseases[5]
  
  lp <- nrow(perm)  #length of permutation vector
  
  
  OUTpropCD <- matrix(NA, lp, 5) # proportion of correct diagnosis for each diagnosis
  OUTsumCD <- c()                # total correct diagnosis
  OUTPPV <- matrix(NA, lp, 5)    # positive predictive value for each disease diagnosis
  OUTNPV <- matrix(NA, lp, 5)    # negative predictive value for each disease diagnosis
  OUTCCD <- matrix(NA, lp, 5)    # cumulative correct diagnosis after every test for each algorithm 
  OUTCD <- matrix(NA, lp, 5)     # correct diagnosis for each test
  
  # -------------------------------------------------------------------------------------
  
  time_start <- Sys.time()
  
  for (j in 1:lp)   #repeats the run for each of 120 possible algorithm
  {
    print(paste0("Permutation ", j))
    
    ds <- rep(0, x)
    TP1 <- rep(0, x)
    FP1 <- rep(0, x)
    TP2 <- rep(0, x)
    FP2 <- rep(0, x)
    TP3 <- rep(0, x)
    FP3 <- rep(0, x)
    TP4 <- rep(0, x)
    FP4 <- rep(0, x)
    TP5 <- rep(0, x)
    FP5 <- rep(0, x)
    TN1 <- rep(0, x)
    FN1 <- rep(0, x)
    TN2 <- rep(0, x)
    FN2 <- rep(0, x)
    TN3 <- rep(0, x)
    FN3 <- rep(0, x)
    TN4 <- rep(0, x)
    FN4 <- rep(0, x)
    TN5 <- rep(0, x)
    FN5 <- rep(0, x)
    CD1<- rep(0, x)
    CD2<- rep(0, x)
    CD3<- rep(0, x)
    CD4<- rep(0, x)
    CD5<- rep(0, x)
    
    prevj <- prev[perm[j, ]] 
    snj <- sn[perm[j, ]]
    spj <- sp[perm[j, ]]
    
    ri <- prevj/(sum(prevj)+prevx)    #relative prevalence 
    cri <- cumsum(ri) #cumulative relative prevalence 
    
    #set.seed(1)
    rand<-runif(x, 0, 1)  #x random numbers between 0 and 1; each random generates a patient
    
    rand_unif <- matrix(runif(10*x, 0, 1), x, 10)
    
    for(i in 1:x)
    {
      #Assign diagnosis based on pretest probability 
      v <- which(rand[i] <= cri)
      if(any(v)) ds[i] <- min(v)
    }
    
    
    TP1[ds == 1 & rand_unif[, 1] <= snj[1]] <- 1
    FP1[ds != 1 & rand_unif[, 2] <= (1 - spj[1])] <- 1
    TP2[ds == 2 & TP1 == 0 & FP1 == 0 & rand_unif[, 3]<=snj[2]] <- 1
    FP2[ds != 2 & TP1 == 0 & FP1 == 0 & rand_unif[, 4]<=(1-spj[2])] <- 1
    TP3[ds == 3 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & rand_unif[, 5]<=snj[3]] <- 1
    FP3[ds != 3 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & rand_unif[, 6]<=(1-spj[3])] <- 1
    TP4[ds == 4 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & rand_unif[, 7]<=snj[4]] <- 1
    FP4[ds != 4 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & rand_unif[, 8]<=(1-spj[4])] <- 1
    TP5[ds == 5 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & TP4 == 0 & FP4 == 0 & rand_unif[, 9]<=snj[5]] <- 1
    FP5[ds != 5 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & TP4 == 0 & FP4 == 0 & rand_unif[, 10]<=(1-spj[5])] <- 1
    
    TN1[ds != 1 & FP1 == 0] <- 1
    FN1[ds == 1 & TP1 == 0] <- 1
    TN2[ds != 2 & TP1 == 0 & FP1 == 0 & FP2 == 0] <- 1
    FN2[ds == 2 & TP1 == 0 & FP1 == 0 & TP2 == 0] <- 1
    TN3[ds != 3 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & FP3 == 0] <- 1
    FN3[ds == 3 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0] <- 1
    TN4[ds != 4 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & FP4 == 0] <- 1
    FN4[ds == 4 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & TP4 == 0] <- 1
    TN5[ds != 5 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & TP4 == 0 & FP4 == 0 & FP5 == 0] <- 1
    FN5[ds == 5 & TP1 == 0 & FP1 == 0 & TP2 == 0 & FP2 == 0 & TP3 == 0 & FP3 == 0 & TP4 == 0 & FP4 == 0 & TP5 == 0] <- 1
    
    CD1[TP1==1]<-1
    CD2[TP2==1]<-1
    CD3[TP3==1]<-1
    CD4[TP4==1]<-1
    CD5[TP5==1]<-1
    
    dis<- c(sum(ds==1),sum(ds==2), sum(ds==3), sum(ds==4), sum(ds==5) )  # QUESTION: Should you not record the case ds == 0? (Ans: not required at this time)
    
    #Total 
    #True positives
    sumTP<- c(sum(TP1), sum(TP2), sum(TP3), sum(TP4), sum(TP5))
    
    #False positives
    sumFP<- c(sum(FP1), sum(FP2), sum(FP3), sum(FP4), sum(FP5))
    
    #Total positives
    sumToP<- sumTP+sumFP
    
    #True negatives 
    sumTN<- c(sum(TN1), sum(TN2), sum(TN3), sum(TN4), sum(TN5))
    
    #False negatives
    sumFN<- c(sum(FN1), sum(FN2), sum(FN3), sum(FN4), sum(FN5))
    
    #Correct diagnosis 
    sumCD<-c(sum(CD1), sum(CD2), sum(CD3), sum(CD4), sum(CD5))
    
    #Cumulative correct diagnosis 
    CCD<- c(sumCD[1], sumCD[1]+sumCD[2], sumCD[1]+sumCD[2]+sumCD[3], sumCD[1]+sumCD[2]+sumCD[3]+sumCD[4], sumCD[1]+sumCD[2]+sumCD[3]+sumCD[4]+sumCD[5])
    
    #predictive values for every test in each algorithm 
    PPV<- sumTP/sumToP
    NPV<- sumTN/(sumTN+sumFN)
    
    #Outputs
    OUTpropCD[j,]<-sumCD/dis
    OUTsumCD[j]<-sum(sumCD)/x
    OUTPPV[j,]<-PPV
    OUTNPV[j,]<-NPV
    OUTCCD[j,]<-CCD/x
    OUTCD[j,]<-sumCD/x
  }
  
  time_end <- Sys.time()
  print(paste0("Execution in ", round(time_end - time_start, 4)))
  print(paste0("Execution in ", round(time_end - time_start, 4)))
  
  # -------------------------------------------------------------------------------------
  
  # Text output
  OUTCD <- OUTCD[, !(apply(OUTCD, 2, sum) == 0)]
  
  # print(OUTCD)
  # print(OUTsumCD)
  
  a1 <- which(OUTsumCD == sort(OUTsumCD, decreasing = TRUE)[1])[1]
  a2 <- which(OUTsumCD == sort(OUTsumCD, decreasing = TRUE)[2])[1]
  a3 <- which(OUTsumCD == sort(OUTsumCD, decreasing = TRUE)[3])[1]
  an <- which(OUTsumCD == sort(OUTsumCD, decreasing = FALSE)[1])[1]
  
  # Plot 1
  d1 <- data_frame(names = perm_name[a1, ], diagnosis = OUTpropCD[a1, 1:r])
  print(d1)
  # d1$names <- factor(d1$names, levels = unique(d1$names)[order(d1$diagnosis, decreasing = TRUE)]) # order by magnitude
  d1$names <- factor(d1$names, levels = d1$names) # order by the algo
  
  # Plot 2
  d2 <- data_frame(names = perm_name[a1, ], predictive = OUTPPV[a1, 1:r])
  print(d2)
  # d2$names <- factor(d2$names, levels = unique(d2$names)[order(d2$predictive, decreasing = TRUE)]) # order by magnitude
  d2$names <- factor(d2$names, levels = d2$names) # order by the algo
  
  
  # Plot #3
  d3 <- data_frame(`Test Name` = as.vector(perm_name), 
                    `Correctly Diagnosed Test` = as.vector(OUTCD),
                    Algorithm = rep(apply(perm_name, 1, paste, collapse = ", "), r), 
                    `Correctly Diagnosed` = rep(OUTsumCD, times = r),
                    Position = rep(1:r, each = lp)) %>%
    arrange(desc(`Correctly Diagnosed`))
  
  print(d3)

  return(list(algo_1 = paste0("Best Algorithm ", "(correct diagnosis score = ", round(sort(OUTsumCD, decreasing = TRUE)[1], 3), "): ", paste(perm_name[a1, ], collapse = " => ")),
              algo_2 = paste0("2nd Best Algorithm ", "(correct diagnosis score = ", round(sort(OUTsumCD, decreasing = TRUE)[2], 3), "): ", paste(perm_name[a2, ], collapse = " => ")),
              algo_3 = paste0("3rd Best Algorithm ", "(correct diagnosis score = ", round(sort(OUTsumCD, decreasing = TRUE)[3], 3), "): ", paste(perm_name[a3, ], collapse = " => ")),
              algo_n = paste0("Worst Algorithm ", "(correct diagnosis score = ", round(sort(OUTsumCD, decreasing = FALSE)[1], 3), "): ", paste(perm_name[an, ], collapse = " => ")),
              d1 = d1,
              d2 = d2,
              d3 = d3
              ))
}