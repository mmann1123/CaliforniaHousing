rm(list = ls())
library(plm)
library(rgdal)
library(splm)
library(spdep)
library(sp)
library(snowfall)
library(rgdal)
library(bootstrap)
library(RANN)
 
beginCluster(type="SOCK")

##########################################################################################
VERSION = "6.5TUrbNS+"
DIRECTORY = paste("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep="") 

##########################################################################################
#  find specification by ecoregion 
CntyJep = read.csv(file=paste(DIRECTORY,"\\CntyJepAREACity",VERSION,".csv",sep=""))
# note: FIDS matches the county codes 
jepFIDS = by(CntyJep[,"FIPS"],CntyJep[,"jepson_Unq_ID_edit"],unique)
spec_holder = read.table(file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,".csv",sep=""),header=T, sep=",", stringsAsFactors=T)

##########################################################################################
#load("C:/Users/mmann1123/Desktop/Share/Final_Variables_2/Out-of-sample5-FUNCTIONS_workspace.RData")
### IMPORT POLYGONS AND ITS DBF FILE
Block.39.ex = readOGR("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion5")
Block.dbf = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion5.dbf")   #read dbf file for edit

### remove rows with more than 2 (expected) NAs
Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]



##########################################################################################
# EVALUATE OUT OF SAMPLE PERFORMANCE
# View groups 
mapgroups()         # this function adds jepson group ids to the county boundary map




# Calculate Mean Square Prediction Error using K-fold 
kfold_validation(group_num=4)
out1= data.frame(ids= as.character(jepFIDS[1:(length(jepFIDS)-0)  ]), Validation_city= unlist(Validation_city[2:length(Validation_city)]), Validation_rural=unlist(Validation_rural[2:length(Validation_rural)]) )
out1
write.csv(out1, file=paste(DIRECTORY,"\\",VERSION,"_mnsq_err.csv",sep=""))







##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
kfold_validation <- function(group_num){
  Validation_city = list(NA )
  Validation_rural = list(NA )
  CROSS_VAL <- function(counties){
      for (urban1 in 1:2){       
          print(counties)
          if(class(counties)=="list"){  counties = unlist(counties)  }
          
          spec_fn = spec_holder[spec_holder$county==paste(counties, collapse=" ") &  spec_holder$urban==urban1,"formulas"] 
          print(spec_fn)
          
          get_data2(urban = urban1, counties=counties,urban_rural_threshold=1)     # 0 for all, 1 for urban, 2 for rural
          fm3 = as.formula(paste("HuDac4 ~ ",spec_fn,"+ as.factor(time)",sep="") )
          
          outer = mycrossval(data_in=panel.data2, ngroup=group_num)   # some  groupings will just be singular....  figure out how to get around this. To see run this multiple times for a regression that comes back with an error.
          print(paste("mean sqr error: ", outer$mn_sqr_err))
          
          if(urban1==1){Validation_city  <<-c(Validation_city,outer$mn_sqr_err) }
          if(urban1==2){Validation_rural <<-c(Validation_rural,outer$mn_sqr_err)}
      }
  }
  system.time(lapply( jepFIDS[1:(length(jepFIDS)-0)  ], function(x) CROSS_VAL(unlist(x))))
}

mycrossval <- function (data_in=panel.data2,  ngroup = n) 
{
  print("data_in must be panel.data2, non panel format")
  call <- match.call()
  #x <- as.matrix(x)
  data_in$my_index = 1:length(data_in[,1])
  n <- length(unique(data_in$id) )
  ngroup <- trunc(ngroup)
  if (ngroup < 2) {
    stop("ngroup should be greater than or equal to 2")
  }
  if (ngroup > n) {
    stop("ngroup should be less than or equal to the number of observations")
  }
  if (ngroup == n) {
    groups <- 1:n
    leave.out <- 1
  }
  if (ngroup < n) {
    leave.out <- trunc(n/ngroup)          # individuals per group
    o = sample(unique(data_in$id))           # all individuals in random order
    groups <- vector("list", ngroup)      # list of groups
    
    for (j in 1:(ngroup - 1)) {
      # place individuals into n-1 groups 
      jj <- (1 + (j - 1) * leave.out)    
      groups[[j]] <- (o[jj:(jj + leave.out - 1)])
    }
    groups[[ngroup]] <- o[(1 + (ngroup - 1) * leave.out):n]  # nth group 
  }
  
  for (j in 1:(ngroup)) {
    # get row numbers for each id 
    groups[[j]]  =   data_in[ unfactor(data_in$id)%in%  unfactor(groups[[j]]),"my_index"]
  }
  
  out <- vector("list", ngroup)
  
  for (j in 1:ngroup) {
    #limit to ids in subsample build weight matrix and run spml
    Block.39_spC2 = Block.39_spC[as.character(Block.39_spC$SUM090) %in% as.character( data_in[-groups[[j]],"SUM090" ]) ,]    # limit polygons to current estimation county and crossval subsample
 #   KNN4<-knn2nb(knearneigh(coordinates(Block.39_spC2),k=3))   #12
  #  WKNN4 <- nb2listw(KNN4, style="W")
    KNN1=knn2nb(knearneigh(coordinates(Block.39_spC2),k=1))    # use to replace i with no neighbors
    DNN3=dnearneigh(coordinates(Block.39_spC2),0,50000,longlat=F)   # in meters for this projection
    PNN3=poly2nb(Block.39_spC2, snap=c(500),row.names=as.character(1:length(Block.39_spC2)) )     # change row.names to match polygon.id from attributes(DNN3)
     
    for(x in 1:length(DNN3)){     # replace empty with k near neigh
        if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
    }
    DNN3 = union.nb(DNN3,PNN3)
    
    WKNN4 = nb2listw(DNN3, style="W")
    
    # try to avoid computationally signular 
    u = tryCatch( {   spml(formula= fm3, data=data_in[-groups[[j]],],listw=WKNN4,index=c("id","time"), model="pooling", lag=T, spatial.error="none")}, 
                  error = function(err) {print("Avoiding Error: Return NA b/c that sample was computationally singular  ")
                                         return(NA)  })
    if( !is.na(u)  ){  # if no error
          # calculate out-of-sample
          # isolate out of sample for year 1999
          out_sample = data_in[groups[[j]],]
          out_sample = out_sample[out_sample$time ==99, ]
          Xs  = model.matrix.lm(fm3,out_sample, na.action=NULL)
          
          N =  length(unique(row.names(Xs)))
          I = diag(N)
          rho = u$arcoef
          
          Block.39_spC2 = Block.39_spC[as.character(Block.39_spC$SUM090) %in% as.character( data_in[groups[[j]],"SUM090" ]) ,]    # limit polygons to current estimation county and crossval subsample
    #       KNN<-knn2nb(knearneigh(coordinates(Block.39_spC2),k=3))   #12
    #       WKNN <- nb2listw(KNN, style="W")
          KNN1=knn2nb(knearneigh(coordinates(Block.39_spC2),k=1))    # use to replace i with no neighbors
          DNN3=dnearneigh(coordinates(Block.39_spC2),0,50000,longlat=F)   # in meters for this projection
          PNN3=poly2nb(Block.39_spC2, snap=c(500),row.names=as.character(1:length(Block.39_spC2)) )     # change row.names to match polygon.id from attributes(DNN3)
           for(x in 1:length(DNN3)){     # replace empty with k near neigh
              if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
          }
          DNN3 = union.nb(DNN3,PNN3)
          
          WKNN4 =nb2listw(DNN3, style="W")
          
          
          A <- solve(I - rho*listw2mat(WKNN4))
          Yfore = data.frame(Yfore = A %*%  Xs[,colnames(Xs) %in%  names(u$coefficients)]  %*% u$coefficients )
          errors_sq = (out_sample[ ,"HuDac4"] - Yfore)^2
          out[[j]] = errors_sq
    }                                                                                                                               
          else{ out[[j]] = NA
    }
  }
  
  return(list(sqr_err = out, mn_sqr_err=mean(unlist(out)),  ngroup = ngroup, leave.out = leave.out, 
              groups = groups, call = call))
}


"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y



get_data2 <- function(urban,counties,urban_rural_threshold){
    #Load relevant data
    urban_rural_threshold <<- urban_rural_threshold  # make a global variable so can be used elsewhere
    print("un# load when ready, must change back")
    #load("C:/Users/mmann1123/Desktop/Share/Final_Variables_2/Out-of-sample5-FUNCTIONS_workspace.RData")
    Block.39.ex<<-Block.39.ex
    
    ###  LIMIT TO COUNTIES 
    counties <<-  counties 
    print(paste("Urban option set to",urban,sep=" "))
    print(paste("Processing County # ",counties,sep=""))
    Block.39_spC = Block.39.ex[unfactor(Block.39.ex$COUNTY) %in% counties,]
    Block.39.dbf_spC = Block.dbf[unfactor(Block.dbf$COUNTY) %in% counties,]
    Block.39.dbf_spC$ordersp = 1:length(Block.39.dbf_spC$SUM090)   #used to reorder data when applying W in matrix math
    
    ### LIMIT TO URBAN OR RURAL
    Block.39_spC$URBAN = 0
    Block.39_spC$URBAN[Block.39_spC$HuDac4_99 >= urban_rural_threshold]=1    # same for polygons
    Block.39_spC$RURAL = 0
    Block.39_spC$RURAL[Block.39_spC$HuDac4_99 < urban_rural_threshold]=1    # same for polygons
    Block.39.dbf_spC$URBAN = 0
    Block.39.dbf_spC[Block.39.dbf_spC$HuDac4_99 >= urban_rural_threshold,"URBAN"]=1
    Block.39.dbf_spC$RURAL = 0
    Block.39.dbf_spC[Block.39.dbf_spC$HuDac4_99 < urban_rural_threshold,"RURAL"]=1
    
    
    print(paste("# Rural blocks = ",sum(Block.39.dbf_spC$RURAL),"   # Urban blocks =",sum(Block.39.dbf_spC$URBAN), sep=" "))
    
    if(sum(Block.39.dbf_spC$URBAN)<20 | sum(Block.39.dbf_spC$RURAL)<20){print("Not enough observations to split urban rural... combining")
                                                                        urban_seperate <<- F } # do this before split or else creates error
    
    if(sum(Block.39.dbf_spC$URBAN)>=20 & sum(Block.39.dbf_spC$RURAL)>=20){    # must have a minimum number of observations
        # if at least 30 observation of urban and rural each, split 
        if(urban ==1){Block.39.dbf_spC= Block.39.dbf_spC[Block.39.dbf_spC$URBAN %in% c(1),]
                      Block.39_spC= Block.39_spC[Block.39_spC$URBAN %in% c(1),]
                      print("Limiting to all urban areas")      
        }
        
        if(urban ==2){Block.39.dbf_spC = Block.39.dbf_spC[Block.39.dbf_spC$URBAN %in% c(0),]
                      Block.39_spC = Block.39_spC[Block.39_spC$URBAN %in% c(0),]
                      print("Limiting to all rural areas")
        }
        urban_seperate <<- T
    }
    
    
    Block.39.dbf_spC <<- Block.39.dbf_spC       # make available outside of function
    Block.39_spC     <<- Block.39_spC
    print(paste("# block groups =",dim(Block.39.dbf_spC)[1],sep=" ")  )
    
    
    size = length(unique(Block.39_spC$SUM090))
    if (size > 1500){
        ### create smaller sample   
        NSAMPLES = 1500   
        # set.seed(123)
        set.seed(123)
        Block.39_sp = Block.39_spC[Block.39_spC$SUM090 %in% sample(Block.39_spC$SUM090, NSAMPLES , prob=(Block.39_spC$Est_Acr/sum(Block.39_spC$Est_Acr))   ),]
        # set.seed(123)
        set.seed(123)
        Block.39.dbf_sp = Block.39.dbf_spC[Block.39.dbf_spC$SUM090 %in% sample(Block.39.dbf_spC$SUM090, NSAMPLES, prob=(Block.39.dbf_spC$Est_Acr/sum(Block.39.dbf_spC$Est_Acr))),]
        print("Sample size limited to 1500 for regression")
    }
    
    if (size <= 1500){
        ### keep all observations
        Block.39_sp = Block.39_spC 
        Block.39.dbf_sp = Block.39.dbf_spC 
    }
    # set to maintain sort order for future export
    Block.39.dbf_sp$ordersp = 1:length(Block.39.dbf_sp$SUM090) 
    
    ### neighbors 
    #KNN4<<-knn2nb(knearneigh(coordinates(Block.39_sp),k=3))   #12     replace with distance based 
    #WKNN4 <<- nb2listw(KNN4, style="W")
    print("starting neighborhood")
    KNN1<<-knn2nb(knearneigh(coordinates(Block.39_sp),k=1))    # use to replace i with no neighbors
    DNN3<<-dnearneigh(coordinates(Block.39_sp),0,50000,longlat=F)   # in meters for this projection
    PNN3=poly2nb(Block.39_sp, snap=c(500),row.names=as.character(1:length(Block.39_sp)) )     # change row.names to match polygon.id from attributes(DNN3)
    print(DNN3)
    
    for(x in 1:length(DNN3)){     # replace empty with k near neigh
        if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
    }
    DNN3 = union.nb(DNN3,PNN3)
    
    WKNN4 <<- nb2listw(DNN3, style="W")
    print(WKNN4)
    print("ending neighborhood")
    
    ################################################
    ######   PUT DATA INTO PANEL LONG        ######
    panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c(grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp))  ) , sep = "_")
    #panel.dataXX= reshape(Block.39.dbf_spC, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_spC)),grep("Cum",names(Block.39.dbf_spC)),grep("DD100_",names(Block.39.dbf_spC)), grep("DD6585_",names(Block.39.dbf_spC)), grep("DD95_",names(Block.39.dbf_spC))  ) , sep = "_")
    
    panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
   # panel.dataXX <-  pdata.frame(panel.dataXX, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
    
    panel.data = add_panel_variables(panel.data)
    panel.specify <<-panel.data
    #panel.dataXX = add_panel_variables(panel.dataXX)
    
    panel.data2 = data.frame(subset(panel.data, select=c(id,time)), subset(panel.data, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
    panel.data2 <<- panel.data2[panel.data2$time %in% 49:99, ]                    # limit to years of interest
    #panel.data2XX = data.frame(subset(panel.dataXX, select=c(id,time)), subset(panel.dataXX, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
    #panel.data2XX <<- panel.data2XX[panel.data2XX$time %in% 49:99, ]                    # limit to years of interest
}   # end get_data 


mapgroups <- function(){
  # this function adds jepson group ids to the county boundary map
  jepper = unique(CntyJep[with(CntyJep, order(jepson_Unq_ID_edit)),"jepson_Unq_ID_edit"]) #unique jep ids in order
  county_map = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Boundary_Files\\geodatabase\\County.dbf")
  county_map$Group_Map = NA
  county_map$FIPS_Num = unfactor(county_map$FIPS)
  
  for (i in jepper){   # for each Jepson 
    # print(paste("Jepson Zone", i, jepFIDS[[as.character(i)]]))
    
    for(j in jepFIDS[[as.character(i)]] ){   # for each county in a jepson group
      county_map[county_map$FIPS_Num == j,"Group_Map"] = i     # set    
    }
  }
  
  write.dbf(county_map,"C:\\Users\\mmann1123\\Desktop\\Share\\Boundary_Files\\geodatabase\\County.dbf")
}



unfactor <- function(factors){return(as.numeric(as.character(factors)))}



### CREATE NEW VARIABLES
add_panel_variables  = function(data){
  data$HuDaclag    = lag(data$HuDac4, 1) 
  data$LnHuDaclag  = log(data$HuDaclag+1) 
  data$HuDaclagSqr = data$HuDaclag^2 
  data$HuDaclagQb  = data$HuDaclag^3 
  data$HuDaclagFr  = data$HuDaclag^4 
  data$HuDaclagFv  = data$HuDaclag^5
  #data$HuDaclag2   = lag(data$HuDac4, 2) 
  #data$LnRoads     = log(data$AllRoads+1) 
  #data$WaterTT2 = data$WaterTT^2 
  #data$ExcludeTT2 = data$ExcludeTT^2 
  #data$ExcludeTT3 = data$ExcludeTT^3 
  
  #data$AllRoadsSqr = data$AllRoads^2 
  #data$AllRoadsQb  = data$AllRoads^3
  #data$Rough_mnSqr = data$Rough_mn^2 
  #data$DD65852   = data$DD6585^2 
  #data$Slope_mnSqr = data$Slope_mn^2 
  #data$WaterTTSqr  = data$WaterTT^2 
  #data$DD1002      = data$DD100^2
  #data$DD1003      = data$DD100^3
  #data$NParkTT2    = data$NParkTT^2
  #data$PP20kTT2    = data$PP20kTT^2
  #data$PP30kTT2    = data$PP30kTT^2
  #data$PPAllTT2    = data$PPAllTT^2
  #data$IndianTT2   = data$IndianTT^2
  
  if(length(counties)==1){data$COUNTY=counties}                            # if only one county COUNTy would be removed and this interferes later
  return(data)
}



