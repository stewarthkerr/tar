#Function for ordering the records
eventindex <- function(ren){
  return(as.numeric(ifelse(grepl('pre',ren),1,ifelse(
    grepl('3_month',ren),2,ifelse(
      grepl('6_month',ren),3,ifelse(
        grepl('1_year',ren),4,ifelse(
          grepl('2_year',ren),5,ifelse(
            grepl('3_year',ren),6,ifelse(
              grepl('4_year',ren),7,ifelse(
                grepl('5_year',ren),8,ifelse(
                  grepl('6_year',ren),9,ifelse(
                    grepl('7_year',ren),10,ifelse(
                      grepl('8',ren),11,ifelse(
                        grepl('9',ren),12,ifelse(
                          grepl('10',ren),13,'')))))))))))))))}

#Function to perform all of the scoring
#Dependencies: dplyr package
#Required input: TAR dataframe
fn_tar_scoring <- function(df)
  {
    y <- transmute(df,studyid=studyid,redcap_event_name=redcap_event_name,
                fadi_count = fn_fadi_count(df),fadi_mean = fn_fadi_mean(df),fadi_total = fn_fadi_total(df),fadi_index=fn_fadi_index(df),
                smfa_fun_count = fn_smfa_function_count(df), smfa_fun_avg = fn_smfa_function_mean(df), smfa_fun_atotal = fn_smfa_function_total(df), smfa_fun_index = fn_smfa_function_index(df),
                smfa_bother_count = fn_smfa_bother_count(df), smfa_bother_avg = fn_smfa_bother_mean(df), smfa_bother_atotal = fn_smfa_bother_total(df), smfa_bother_index = fn_smfa_bother_index(df),
                aofas_hfoot = fn_aofas_hfoot(df), aofas_hfoot_function = fn_aofas_hfoot_function(df), aofas_hfoot_pain = aofas_1, aofas_hfoot_alignment =  aofas_9,
                faos_pain = fn_faos_pain(df), faos_symptoms = fn_faos_symptoms(df), faos_adl = fn_faos_adl(df), faos_spandrecpain = fn_faos_spandrecpain(df), faos_qol = fn_faos_qol(df),
                sf36_body_pain = fn_sf36_body_pain(df), sf36_general_health = fn_sf36_general_health(df), sf36_mental_health = fn_sf36_mental_health(df),sf36_physical_function = fn_sf36_physical_function(df),sf36_role_emotional = fn_sf36_role_emotional(df),sf36_role_physical = fn_sf36_role_physical(df), sf36_social_fun = fn_sf36_social_function(df), sf36_vitality = fn_sf36_vitality(df), sf36_rep_health = fn_sf36_reported_health(df)
                )
    
    y <- mutate(y,sf36_total = fn_sf36_total(y),
                sf36_phy_hea_dim = fn_sf36_physical_health_dimension(y),
                sf36_men_hea_dim = fn_sf36_mental_health_dimension(y),
                sf36_mcs = fn_sf36_mcs(y),
                sf36_pcs = fn_sf36_pcs(y))
  
    return(y) 
  }

#FADI Scores
  fn_fadi_count <- function(df)
    {
      df <- select(df,fadi_1:fadi_26)
      return(26-apply(is.na(df),1,sum))
       }
  
  fn_fadi_mean <- function(df)
    {
      df <- select(df,fadi_1:fadi_26)
      return(ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))))
    }
  
  fn_fadi_total <- function(df)
    {
      df <- select(df,fadi_1:fadi_26)
      return(ifelse(apply(df,1,sum,na.rm=TRUE)==0,NA,(apply(df,1,sum,na.rm=TRUE))))
    }
    
  fn_fadi_index <- function(df)
    {
      df <- select(df,fadi_1:fadi_26)
      x <- ifelse(apply(df,1,sum,na.rm=TRUE)==0,NA,(apply(df,1,sum,na.rm=TRUE))) #fadi_total
      y <- apply(is.na(df),1,sum) #fadi_count
      
        return(x/(4*26-y))
    }

#SMFA Scores
  fn_smfa_function_count <- function(df)
    {
      df <- select(df,smfa_1:smfa_34)
      return(34-apply(is.na(df),1,sum))
    }
  
  fn_smfa_function_mean <- function(df)
    {
      df <- select(df,smfa_1:smfa_34)
      return(ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))))
    }
  
  fn_smfa_function_total <- function(df)
    {
      df <- select(df,smfa_1:smfa_34)
      x <- 34-apply(is.na(df),1,sum) #smfa_function_count
      y <- ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))) #smfa_function_mean
      
      #If we have smfa_num >= 17, then we report score using smfa_mean as the score for each question
      return(ifelse(x>=17,34*y,NA))
    }
  
  fn_smfa_function_index <- function(df)
    {
      df <- select(df,smfa_1:smfa_34)
      x <- 34-apply(is.na(df),1,sum) #smfa_function_count
      y <- ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))) #smfa_function_mean
      z <- 34*y #smfa_function_total
      
      #Only return an index if count is 17 or more (greater than 50% response)
      return(ifelse(x>=17,(100*(z-34)/136),NA))
    }

#SMFA Bother calculations
  fn_smfa_bother_count <- function(df)
  {
    df <- select(df,smfa_35:smfa_46)
    return(12-apply(is.na(df),1,sum))
  }
  
  fn_smfa_bother_mean <- function(df)
  {
    df <- select(df,smfa_35:smfa_46)
    return(ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))))
  }
  
  fn_smfa_bother_total <- function(df)
  {
    df <- select(df,smfa_35:smfa_46)
    return(ifelse(apply(df,1,sum,na.rm=TRUE)==0,NA,(apply(df,1,sum,na.rm=TRUE))))
  }
  
  fn_smfa_bother_index <- function(df)
  {
    df <- select(df,smfa_35:smfa_46)
    x <- 12-apply(is.na(df),1,sum) #smfa_bother_count
    y <- ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))) #smfa_bother_mean
    z <- 12*y #smfa_bother_total
    
    return(ifelse(x>=8.999,100*(z-12)/48,NA))
  }

#SF36 - http://www.alswh.org.au/images/content/pdf/InfoData/Data_Dictionary_Supplement/DDSSection2SF36.pdf
    fn_sf36_body_pain <- function(df)
      {
        return(
          ifelse(df$sf36_7=='6' & df$sf36_8 == '0',100*(df$sf36_7+4)/10,
          ifelse(df$sf36_7 !='6' & df$sf36_8 == '0',100*(df$sf36_7+3)/10,
                 100*(df$sf36_7+df$sf36_8-2)/10)))
      }
    
    fn_sf36_general_health <- function(df)
      {
          df <- select(df,sf36_11a,sf36_11b,sf36_11c,sf36_11d,sf36_1)
          miss <- apply(is.na(df),1,sum) #missing
          y <- apply(!is.na(df),1,sum) #count
          subsum <- (apply(df,1,sum,na.rm=TRUE))

          return(ifelse(df$sf36_1=='1',(100*(5+subsum-y+1)/(4*(y+1))),
          ifelse(df$sf36_1=='2',(100*(4.4+subsum-y+1)/(4*(y+1))),
          ifelse(df$sf36_1=='3',(100*(3.4+subsum-y+1)/(4*(y+1))),
          ifelse(df$sf36_1=='4',(100*(2+subsum-y+1)/(4*(y+1))),
          ifelse(df$sf36_1=='5',(100*(1+subsum-y+1)/(4*(y+1))),NA))))))
      }
    
    fn_sf36_mental_health <- function(df)
      {
        df <- select(df,sf36_9b,sf36_9c,sf36_9d,sf36_9f,sf36_9h)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        subsum <- (apply(df,1,sum,na.rm=TRUE))
        return(ifelse(miss>2,NA,100*(subsum-y)/(y*5)))
      }
    
    fn_sf36_physical_function <- function(df)
      {
        df <- select(df,sf36_3a,sf36_3b,sf36_3c,sf36_3d,sf36_3e,sf36_3f,sf36_3g,sf36_3h,sf36_3i,sf36_3j)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        subsum <- (apply(df,1,sum,na.rm=TRUE))
        return(ifelse(miss>5,NA,100*(subsum-y)/(y*2)))
      }      
    
    fn_sf36_role_emotional <- function(df)
      {
      df <- select(df,sf36_5a,sf36_5b,sf36_5c)
      miss <- apply(is.na(df),1,sum) #missing
      y <- apply(!is.na(df),1,sum) #count
      subsum <- (apply(df,1,sum,na.rm=TRUE))
      return(ifelse(miss>1,NA,100*(subsum-y)/y))
      }        
    
    fn_sf36_role_physical <- function(df)
      {
        df <- select(df,sf36_4a,sf36_4b,sf36_4c,sf36_4d)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        subsum <- (apply(df,1,sum,na.rm=TRUE)) 
        return(ifelse(miss>2,NA,100*(subsum-y)/y))
      }
    
    fn_sf36_social_function <- function(df)
      {
        df <- select(df,sf36_6,sf36_10)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        subsum <- (apply(df,1,sum,na.rm=TRUE))
        return(ifelse(miss>1,NA,100*(subsum-y)/(y*4)))
      }        
    
    fn_sf36_vitality <- function(df)
      {
        df <- select(df,sf36_9a,sf36_9e,sf36_9g,sf36_9i)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        subsum <- (apply(df,1,sum,na.rm=TRUE))
        return(ifelse(miss>2,NA,100*(subsum-y)/(y*5)))
      }
    
    fn_sf36_reported_health <- function(df)
      {
        return(ifelse(df$sf36_2==1,2,
        ifelse(df$sf36_2==2,1,
        ifelse(df$sf36_2==3,0,
        ifelse(df$sf36_2==4,-1,
        ifelse(df$sf36_2==5,-2,NA))))))
      }
    
    fn_sf36_total <- function(df)
      {
        df <- select(df,sf36_body_pain,sf36_general_health,sf36_mental_health,sf36_physical_function,sf36_role_emotional,sf36_role_physical,sf36_social_fun,sf36_vitality)
        return(ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))))
      }

    fn_sf36_physical_health_dimension <- function(df)
      {
        df <- select(df,sf36_body_pain,sf36_general_health,sf36_physical_function,sf36_role_physical,sf36_vitality)
        return(ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))))
      }
      
    fn_sf36_mental_health_dimension <- function(df)
      {
        df <- select(df,sf36_general_health,sf36_mental_health,sf36_role_emotional,sf36_social_fun,sf36_vitality)
        return(ifelse(is.nan(apply(df,1,mean,na.rm=TRUE)),NA,(apply(df,1,mean,na.rm=TRUE))))
      }
    
    fn_sf36_mcs <- function(df)
      {
        df <- select(df,sf36_general_health,sf36_mental_health,sf36_role_emotional,sf36_social_fun,sf36_vitality,sf36_body_pain,sf36_physical_function,sf36_role_physical)
        pf <- ((df$sf36_physical_function - 84.52404) / 22.89490) * -0.22999
        rp <- ((df$sf36_role_physical - 81.19907) / 33.79729) * -0.12329
        bp <- ((df$sf36_body_pain - 75.49196) / 23.55879) * -0.09731
        gh <- ((df$sf36_general_health - 72.21316) / 20.16964) * -0.01571
        vt <- ((df$sf36_vitality - 61.05453) / 20.86942) * 0.23534
        sf <- ((df$sf36_social_fun - 83.59753) / 22.37642) * 0.26876
        re <- ((df$sf36_role_emotional - 81.29467) / 33.02717) * 0.43407
        mh <- ((df$sf36_mental_health - 74.84212) / 18.01189) * 0.48581
        sf36_mcs <- ((pf+rp+bp+gh+vt+sf+re+mh)*10)+50
        return(sf36_mcs)
      }
    
    fn_sf36_pcs <- function(df)
      {
        df <- select(df,sf36_general_health,sf36_mental_health,sf36_role_emotional,sf36_social_fun,sf36_vitality,sf36_body_pain,sf36_physical_function,sf36_role_physical)
        pf <- ((df$sf36_physical_function - 84.52404) / 22.89490) * 0.42402
        rp <- ((df$sf36_role_physical - 81.19907) / 33.79729) * 0.35119
        bp <- ((df$sf36_body_pain - 75.49196) / 23.55879) * 0.31754
        gh <- ((df$sf36_general_health - 72.21316) / 20.16964) * 0.24954
        vt <- ((df$sf36_vitality - 61.05453) / 20.86942) * 0.02877
        sf <- ((df$sf36_social_fun - 83.59753) / 22.37642) * -0.00753
        re <- ((df$sf36_role_emotional - 81.29467) / 33.02717) * -0.19206
        mh <- ((df$sf36_mental_health - 74.84212) / 18.01189) * -0.22069
        sf36_pcs <- ((pf+rp+bp+gh+vt+sf+re+mh)*10)+50
        return(sf36_pcs)
      }
    

#AOFAS
    fn_aofas_hfoot <- function(df)
      {
        return(df$aofas_1 + df$aofas_2 + df$aofas_3 + df$aofas_4 + df$aofas_5 + df$aofas_6 + df$aofas_7 + df$aofas_8 + df$aofas_9)
      }
    
    fn_aofas_hfoot_function <- function(df) 
      {      
          return(df$aofas_2 + df$aofas_3 + df$aofas_4 + df$aofas_5 + df$aofas_6 + df$aofas_7 + df$aofas_8)
      }
    
#FAOS
    fn_faos_pain <- function(df)
      {
        df <- select(df,contains('faos_p'))
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        return(ifelse(miss>2,NA,100 - (apply(df,1,sum,na.rm=TRUE)*100/(y*4))))
    }    
    
    fn_faos_symptoms <- function(df)
      {
        df <- select(df,faos_s1,faos_s2,faos_s3,faos_s4,faos_s5,faos_s6,faos_s7)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        return(ifelse(miss>2,NA,100 - (apply(df,1,sum,na.rm=TRUE)*100/(y*4))))
    }

    fn_faos_adl <- function(df)
      {
        df <- select(df,contains('faos_a'))
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        return(ifelse(miss>2,NA,100 - (apply(df,1,sum,na.rm=TRUE)*100/(y*4))))
    }    

    fn_faos_spandrecpain <- function(df)
      {
        df <- select(df,faos_sp1,faos_sp2,faos_sp3,faos_sp4,faos_sp5)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        return(ifelse(miss>2,NA,100 - (apply(df,1,sum,na.rm=TRUE)*100/(y*4))))
    }

    fn_faos_qol <-function(df)
      {
        df <- select(df,faos_q1,faos_q2,faos_q3,faos_q4)
        miss <- apply(is.na(df),1,sum) #missing
        y <- apply(!is.na(df),1,sum) #count
        return(ifelse(miss>2,NA,100 - (apply(df,1,sum,na.rm=TRUE)*100/(y*4))))
      }