####################################################################################################################################
##### Author: Anna Schritz                                                                                                  ########
##### Creation Date: 14/05/2018                                                                                             ########
##### Last updated: 13.09.2023                                                                                              ########
##### File Name: FunctionDescriptiveStatisticsTable.R                                                                       ########
##### Path: K:/CCMS Public/R_SAS good practices/R/01 - January 2020/FunctionDescriptiveStatisticsTable.R                                                                                             ########
##### Purpose: Function that creates common table for continuous and categorised/binary variables for total population and  ########
#####          stratified by a grouping variable with only 2 categories (so far)                                                                            ########
##### Reason for update:            include IQR;
#####                               level names of categorised variables as variable names in descrBy2;
#####                   14/01/2020  statistical test to compare between two groups in descrBy2()
#####                               choose between parametric test (t-test) and non-parametric test (wilcoxon)
#####                   22/01/2020  new function descrTab() where the for-loop for all variables is already included
#####                               so a vector of variables can be input and a table will be returned
####################################################################################################################################
Sys.setenv(LANG = "en")

library(psych)


####################################################################################################################################
#### function to get summary descriptive statistics for continuous and categorical variables ####
#### for a whole list of variables returning a table ready for publication

descrTabPub <- function(varVec, nameVec=varVec, database, print = TRUE, addN = FALSE){
  # var (character vector): vector of names of variables to perform descriptive statistics on
  # name (character vector): to rename of variables -> vector of new names for variables to appear in table
  # database: name of dataset
  # print (logical): print variable name per iteration
  # addN (logical): should the N (number of available data) be added for every continuous variable and adding
  #               NA category?

  database = as.data.frame(database)
  # for some reason the function stopped working with an error "object 'tot' not found"
  # therefore, an additional line below has been added, and the function started working
  tot<-c()

  totTab <- c()

  for(i in 1:length(varVec)){
    # for continuous variables
    if(is.numeric(database[,varVec[i]]) & all(is.na(database[,varVec[i]])) == FALSE){
      if(print == TRUE){
        print(varVec[i])
      }
    
      if(addN == TRUE){
        Ncont <- table(is.na(database[,varVec[i]]))
      }
      




      mat <- data.frame(psych::describe(database[,varVec[i]]))[,c(3,4)]
      if(addN == TRUE){
        mat$mean <- paste0(round(mat$mean,2), " (", round(mat$sd,2), "); N=", Ncont["FALSE"])
      }else{
        mat$mean <- paste0(round(mat$mean,2), " (", round(mat$sd,2), ")")
      }
      mat <- data.frame(mat[,-c(2)])
      
      row <- c()

      row.names(mat) = nameVec[i]
      colnames(mat) = c("Mean (SD)/ N (%)")
      tot <- mat

    }

    ##################################################################################################################################
    #### for categorical or binary variables ####
    if((is.factor(database[,varVec[i]]) | is.character(database[,varVec[i]])) & all(is.na(database[,varVec[i]])) == FALSE){
      if(print == TRUE){
        print(varVec[i])
      }
      
      
      if(addN == TRUE & is.factor(database[,varVec[i]])){
        database[,varVec[i]] <- addNA(database[,varVec[i]])
      }else if(addN == TRUE & is.character(database[,varVec[i]])){
        database[,varVec[i]] <- ifelse(is.na(database[,varVec[i]]), "NA", database[,varVec[i]])
      }
      

      nb <- table(database[,varVec[i]])

      perc <- prop.table(table(database[,varVec[i]]))


      nb3 <- matrix(nb, ncol = 1)
      perc3 <- matrix(perc, ncol = 1)

      tot <- cbind("Mean (SD)/ N (%)" = paste0(nb3[,1], " (", round(perc3[,1]*100,2), "%)"))
      row.names(tot) <- levels(database[,varVec[i]])

      head <- matrix(data = NA, nrow = 1, ncol = 1)
      row.names(head) <- nameVec[i]

      tot <- rbind(head, tot)
    } else if(all(is.na(database[,varVec[i]])) == TRUE){
      tot <- c()
    }

    totTab <- rbind(totTab, tot)

  }

  return(totTab)
}



#### function to get summary descriptive statistics for continuous and categorical variables ####
#### stratified by grouping variable with more than 2 categories; output is table for publication
descrByCatTabPub <- function(varVec, nameVec=varVec, catVar, database, contTest = "kruskal", print = TRUE,
                             addN = FALSE){
  # varVec (character Vector): names of variable
  # nameVec (character vector): english names of variable
  # catVar (character): categorical variable to define subgroups
  # database: name of dataset
  # contTest (character vector): "kruskal" if Kruskal-Wallis Rank Sum Test(non-parametric) should be used or
  #                       "anova" if Anova should be used (parametric)
  #                        "none" if no test should be applied
  # print (logical): print variable name per iteration
  # addN (logical): should the N (number of available data) be added for every continuous variable and adding
    #               NA category?

  database = as.data.frame(database)

  ################################
  groups <- length(levels(database[,catVar]))
  gnames <- levels(database[,catVar])
  #################################
  totTab <- c()

  for(i in 1:length(varVec)){
    ##################################################################################################################################
    #### for continuous variables #####
    if(is.numeric(database[,varVec[i]])){
      if(print == TRUE){
        print(varVec[i])
      }


      mat <- psych::describeBy(database[,varVec[i]], group = database[,catVar], digits= 2, mat = TRUE)[c(5,6)]


      if(contTest[i] == "kruskal"){
        # wilcoxon rank sum test to compare between two groups
        pval_cont <- round(kruskal.test(database[,varVec[i]] ~ database[,catVar])$p.value, 3)
      }else if(contTest[i] == "anova"){
        pval_cont <- round(anova(lm(database[,varVec[i]] ~ database[,catVar]))[1,5], 3)
      }else if(contTest[i] == "none"){
        pval_cont <- NA
      }

      if(addN == TRUE){
         Ncont <- table(is.na(database[,varVec[i]]), database[,catVar])
      }



      row <- c()

      for(j in 1:groups){
          if(addN == TRUE){
              row <- cbind(row, paste0(round(mat[j,1],2), " (", round(mat[j,2],2), ")", "; N=", Ncont["FALSE", j]))
          }else{
              row <- cbind(row, paste0(round(mat[j,1],2), " (", round(mat[j,2],2), ")"))
          }
      }

      row <- cbind(row, pval_cont)

      colnames(row) <- c(gnames, "p-value")
      row.names(row) <- c(nameVec[i])
      tot <- row

    }

    ##################################################################################################################################
    #### for categorical or binary variables ####
    if((is.factor(database[,varVec[i]]) | is.character(database[,varVec[i]])) & all(is.na(database[,varVec[i]]))==FALSE){
      if(print == TRUE){
        print(varVec[i])
      }

        if(addN == TRUE & is.factor(database[,varVec[i]])){
            database[,varVec[i]] <- addNA(database[,varVec[i]])
        }else if(addN == TRUE & is.character(database[,varVec[i]])){
            database[,varVec[i]] <- ifelse(is.na(database[,varVec[i]]), "NA", database[,varVec[i]])
        }

      nb <- table(database[,varVec[i]], database[,catVar])

      perc <- prop.table(table(database[,varVec[i]], database[,catVar]),2)


      if(length(levels(database[,varVec[i]])) > 1 & (contTest[i] == "anova" | contTest[i] == "kruskal")){
        chi <- round(chisq.test(database[,varVec[i]], database[,catVar])$p.value, 3)
      }else{
        chi <- NA
      }


      nb3 <- matrix(nb, ncol = groups)
      perc3 <- matrix(perc, ncol = groups)

      tot <- c()

      for(j in 1:groups){
        tot <- cbind(tot, paste0(nb3[,j], " (",round(perc3[,j]*100,2), "%)"))
      }

      tot <- cbind(tot, NA)

      colnames(tot) <- c(gnames, "p-value")



      row.names(tot) <- levels(database[,varVec[i]])

      head <- matrix(data = NA, nrow = 1, ncol = groups+1)
      row.names(head) <- nameVec[i]
      head[1,groups+1] <- chi



      catBy_summary <- rbind(head, tot)


      tot <- catBy_summary
    }else if(all(is.na(database[,varVec[i]])) == TRUE){
      tot <- c()
    }


    totTab <- rbind(totTab, tot)
  }
  return(data.frame(totTab))
}


####################################################################################################################################
