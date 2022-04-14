# 1. Author details
# Authors: Ashish Shrestha*, Margaret Garcia
# *Contact email: ashish.shrs@asu.edu

# 2. File details
# Last updated: 02/04/2022
# Objective: The purpose of this script is to fill missing attributes-values for conduits and junctions of SWMM model for the approach used in the publication below.
# Publication: Shrestha, A., Mascaro, G., & Garcia, M. (2022). Effects of stormwater infrastructure data completeness and model resolution on urban flood modeling. 
# Journal of Hydrology, 607(July 2021), 127498. https://doi.org/10.1016/j.jhydrol.2022.127498

# 3. Script:
# Notes: Conduit.csv is the conduit file of your SWMM 5 model. It is created by converting Conduit.shp file to *.shp file.
# Example below is for ROUGHNESS, this can be repeated for any attributes.

conduit_file = read.csv("Conduit.csv", header=TRUE)
conduit_file$upstream_ID <- "a"  #adding a column for upstream IDs
conduit_file$downstream_ID <- "b" #adding a column for downstream IDs
conduit_file$upstream_attribute_val <- 0 #adding a colum for upstream attribute-value
conduit_file$downstream_attribute_val <- 0 #adding a colum for downstream attribute-value

ai <- 1
while (ai<length(conduit_file[,1])) {
  testUP <- conduit_file[ai,"INLETNODE"]
  testDOWN <- conduit_file[ai,"OUTLETNODE"]
  if(is_empty(conduit_file[(conduit_file[,"OUTLETNODE"] == testUP),"NAME"])) {
    conduit_file[ai,"upstream_ID"]<- NA #if no upstream conduit is identified 
    conduit_file[ai,"upstream_attribute_val"] <- NA 
  } else {
    array_cond <- conduit_file[(conduit_file[,"OUTLETNODE"] == testUP),"NAME"] #this array extract names of all upstream conduits
    array_loc <- c() #this array extract location (or row number) of all upstream conduits
    array_your_attribute <- c() #this array extract attribute-value of all corresponding conduits
    for (j in 1:length(array_cond)){
      array_loc[j] <- which(conduit_file$NAME == array_cond[j])
      array_your_attribute[j] <- conduit_file$ROUGHNESS[array_loc[j]]
    }
    conduit_file[ai,"upstream_ID"] <- array_cond[which(array_your_attribute==min(array_your_attribute))[1]] #is conduit ID, takes one of the minimum
    conduit_file[ai,"upstream_attribute_val"] <- min(array_your_attribute) # is Attri_val
  }
  if(is_empty(conduit_file[(conduit_file[,"INLETNODE"] == testDOWN),"NAME"])) {
    conduit_file[ai,"downstream_ID"] <- NA #if no downstream conduit is identified 
    conduit_file[ai,"downstream_attribute_val"] <- NA #min Attri_val is assigned 1 if downstream conduit is not found
  } else {
    array_cond2 <- conduit_file[(conduit_file[,"INLETNODE"] == testDOWN),"NAME"]
    array_loc2 <- c()
    array_your_attribute2 <- c()
    for (k in 1:length(array_cond2)){
      array_loc2[k] <- which(conduit_file$NAME == array_cond2[k])
      array_your_attribute2[k] <- conduit_file$ROUGHNESS[array_loc2[k]]
    }
    conduit_file[ai,"downstream_ID"] <- array_cond2[which(array_roughness2==min(array_roughness2))[1]] 
    conduit_file[ai,"downstream_attribute_val"] <- min(array_roughness2)
  }
  ai <- ai + 1
}
conduit_attribute_info <-  as.data.frame(cbind(conduit_file$NAME, conduit_file$upstream_ID, conduit_file$downstream_ID, conduit_file$upstream_attribute_val,conduit_file$downstream_attribute_val))
colnames(conduit_attribute_info) = c("Name", "upstream_ID","downstream_ID","upstream_attribute_val","downstream_attribute_val")



# Identifying upstream and downstream Junctions and Conduits
conduit_file = read.csv("Conduit.csv", header=TRUE)
junction_file = read.csv("Junction.csv", header = TRUE)
junction_file$upstream_ID <- "a"
junction_file$downstream_ID <- "b"
junction_file$upstream_attribute_val <- 0
junction_file$downstream_attribute_val <- 0
junction_file$Attri_val <- 0
outfall_list <- c("O1", "O2") # complete list of outfall
for (i in 1:nrow(junction_file)){
  Position_of_upstream_ID <- which(conduit_file$OUTLETNODE == as.character(junction_file$NAME[i]))
  Vector_of_upstream_ID <- conduit_file[Position_of_upstream_ID,3]
  Vector_of_upstream_ID <- Vector_of_upstream_ID[!(Vector_of_upstream_ID %in% outfall_list)] #It removes outfalls from vector, if outfall do not have required attributes
  Attri_val_of_upstream_ID <- c()# create vector for all upstream nodes' depth
  Attri_val <- c()
  ##
  Position_of_downstream_ID <- which(conduit_file$INLETNODE == as.character(junction_file$NAME[i]))
  Vector_of_downstream_ID <- conduit_file[Position_of_downstream_ID,4] #correction from:  conduit_file[Position_of_upstream_ID,4]
  Vector_of_downstream_ID <- Vector_of_downstream_ID[!(Vector_of_downstream_ID %in% outfall_list)] #It removes outfalls from vector, if outfall do not have required attributes
  Attri_val__of_downstream_ID <- c()# create vector for all upstream nodes' depth
  ##
  for (j in 1:length(Vector_of_upstream_ID)){
    Attri_val_of_upstream_ID[j] <- junction_file[which(junction_file$NAME == Vector_of_upstream_ID[j]),11]
    Attri_val[j] <- conduit_file[Position_of_upstream_ID[j], "GEOM1"]
  }
  ##
  for (k in 1:length(Vector_of_downstream_ID)){
    Attri_val__of_downstream_ID[k] <- junction_file[which(junction_file$NAME == Vector_of_downstream_ID[k]),11]
  }
  ##
  junction_file$upstream_ID[i] <- ifelse(length(Vector_of_upstream_ID)== 0, NA, as.character(Vector_of_upstream_ID[which(Attri_val_of_upstream_ID == min(Attri_val_of_upstream_ID))[1]]))
  junction_file$upstream_attribute_val[i] <- ifelse(length(Vector_of_upstream_ID)== 0, NA, as.numeric(min(Attri_val_of_upstream_ID))[1])
  junction_file$Attri_val[i] <- ifelse(length(Vector_of_upstream_ID)==0, 1.5, as.numeric(max(Attri_val)))
  ##
  junction_file$downstream_ID[i] <- ifelse(length(Vector_of_downstream_ID)== 0, NA, as.character(Vector_of_downstream_ID[which(Attri_val__of_downstream_ID == min(Attri_val__of_downstream_ID))[1]]))
  junction_file$downstream_attribute_val[i] <- ifelse(length(Vector_of_downstream_ID)== 0, NA, as.numeric(min(Attri_val__of_downstream_ID))[1])
}
conduit_junction_attribute_info = as.data.frame(cbind(junction_file$NAME,junction_file$upstream_ID, junction_file$downstream_ID, junction_file$upstream_attribute_val, junction_file$downstream_attribute_val, junction_file$Attri_val))
colnames(conduit_junction_attribute_info) = c("Name", "Distancefromoutlet","upstream_ID","downstream_ID","upstream_attribute_val","downstream_attribute_val","Attri_val")




# Sampling atttribute-values from selected features # Example is for roughness
#Calling required libraries
library(swmmr)
library(EnvStats)
library(Rcpp)
library(tibble)
library(utils)
library(xts)
library(zoo)
# Feel free to change this to your path of choice.
inp_file <- "/.../SWMM_model_name.inp" # model file directory
tmp_rpt_file <- "/.../SWMM_model_name.rpt"
tmp_out_file <- "/.../SWMM_model_name.out"
swmm_files <- run_swmm(
  inp = inp_file,
  rpt = tmp_rpt_file,
  out = tmp_out_file
)

mat_out_fld = as.data.frame(matrix(0 ,nrow = 194, ncol = 1)) #creating matrix for sampled value
mat_rpt1 = as.data.frame(matrix(0 ,nrow = 850, ncol = 1)) #creating matrix for sampled value
mat_rpt2 = as.data.frame(matrix(0 ,nrow = 6, ncol = 1)) #creating matrix for sampled value
for (i in 1:100){ # main loop for sampling
  rand1 = 55  #Total = 1091, 5%=55, 25%=272, 50%=545, 75%=818
  input1 = read_inp(inp_file) # reading input file
  df_rou_0 <- as.data.frame(input1$conduits) #extract
  df_rou <- merge(x= df_rou_0, y = conduit_attribute_info, by="Name")
  sample_rows <- sample.rows(df_rou, rand1)
  known_mat <- anti_join(df_rou, sample_rows)
  loc = as.data.frame(matrix(0, nrow = length(sample_rows$Name), ncol = 1)) # matrix for location of sampled subset
  for (ii in 1:nrow(sample_rows)){ #loop for location of sampled subset
    loc[ii,1] <- which(input1$conduits$Name == as.character(sample_rows[ii,1]))
  }
  
  for (iii in 1:nrow(loc)){ # loop for replacing roughness in the input file
    if (is.na(sample_rows$upID[iii])=="FALSE"){
      #Second Check
      if (any(sample_rows$Name==as.character(sample_rows$upID[iii]))=="FALSE"){
        input1$conduits$Roughness[loc[iii,1]] = as.numeric(sample_rows$upRoughness[iii])
      }else{
        input1$conduits$Roughness[loc[iii,1]] = ifelse(is.na(sample_rows$downID[iii])=="TRUE", remp(1, known_mat$Roughness),
                                                       ifelse(any(sample_rows$Name==as.character(sample_rows$downID[iii]))=="FALSE", as.numeric(sample_rows$downRoughness[iii]), remp(1, known_mat$Roughness)))
      }
      #Second Check
    }else{
      #Second Check
      if (is.na(sample_rows$downID[iii])=="FALSE"){
        input1$conduits$Roughness[loc[iii,1]] = ifelse(any(sample_rows$Name==as.character(sample_rows$downID[iii]))=="FALSE", as.numeric(sample_rows$downRoughness[iii]), remp(1, known_mat$Roughness))
      }else{
        input1$conduits$Roughness[loc[iii,1]] = remp(1, known_mat$Roughness)
      }
      #Second Check
    }
  }
  
  input_fil <-tempfile() #directory
  out_fil <- tempfile() #directory
  rpt_fil <- tempfile() #directory
  write_inp(input1, input_fil) #saving input file in temporary location
  swmm_files <- run_swmm( #executing run. this command calls swmm.exe 
    inp = input_fil,
    rpt = rpt_fil,
    out = out_fil
  )
  swmmoutt <- read_out(out_fil, iType = 3, vIndex = 10)
  swmmoutt_xts <- as.xts(swmmoutt$system_variable$total_external_flooding) # this is extracted time series
  swmmoutt_ts <- fortify.zoo(swmmoutt_xts)  # and TS is converted to data base
  mat_out_fld <- cbind(mat_out_fld, swmmoutt_ts$swmmoutt_xts)
  names(mat_out_fld)[i+1]<- paste0("S_FLD", i)
  cat("Completed iteration:", i, "\n")
  swmmrptt <-read_rpt(rpt_fil)
  mat_rpt1 <- qpcR:::cbind.na(mat_rpt1, swmmrptt$node_flooding_summary$Hours_Flooded)
  names(mat_rpt1)[i+1]<- paste0("S_FLD", i)
  mat_rpt2 <- cbind(mat_rpt2, swmmrptt$runoff_quantity_continuity$Volume)
  names(mat_rpt2)[i+1]<- paste0("S_FLD", i)
}
#lapply(mat_out_fld, sum)
write.csv(mat_out_fld, "system_flooding.csv")
write.csv(mat_rpt1, "duration_of_flooding.csv")




