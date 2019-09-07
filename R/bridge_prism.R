#' @title Run ACCEPT Model
#' @description This function calls the predictACCEPT model. 
#' If no model_input is specified, it will use the defaults
#' If one of the columns in model_input is missing, it will replace with the default column
#' It will check all the columns of the patient_data object; if one is missing, it will
#' replace with the default, and will remove any extra columns
#' @param model_input A list/json object with "patient_data", "random_sampling_N", "random_distribution_iteration",
#' and "calculate_CIs" as columns
#' @return Returns a list of results
model_run<-function(model_input=NULL)
{

  
    results <- predictCFMortality(model_input)
    
    return(as.list(results))
}


get_default_input <- function() {
  model_input <- list(age = 16, male = 0, fvc = 66.7, fev1 = 47.4, fev1LastYear = 80.5, bcepacia = 0, underweight = 0, nHosp = 0, pancreaicInsufficient = 1, CFRelatedDiabetes = 0, ageAtDiagnosis = 0.9)
  return(flatten_list(model_input))
}



#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }
  
  for(i in 1:length(lst))
  {
    nm<-names(lst[i])
    
    message(nm)
    
    if(prefix!="")  nm<-paste(prefix,nm,sep=".")
    
    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}
