


# this version uses theobalds cutoff rural = all areas less than urban_rural_threshold units / acre which is "rural and exurban" classes
# from dick cameron's email 
# also solve the probabily of random sampling

rm(list =  ls() %w/o% "Block.39.ex"   )
library(plm)
library(rgdal)
library(splm)
library(spdep)
library(sp)
#library(snowfall)
library(raster)
library(snow)
library(plyr)
#beginCluster(type="SOCK")



# save.image(paste(DIRECTORY,'workspace.RData',sep='\\'))
load(paste('G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.7TUrbNS+D20km','workspace.RData',sep='\\'))


##########################################################################################
VERSION = "6.9TUrbNS+D20kmturnoffpop"
DIRECTORY = paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep="") 



##########################################################################################
#load("C:/Users/mmann1123/Desktop/Share/Final_Variables_2/Out-of-sample5-FUNCTIONS_workspace.RData")
### IMPORT POLYGONS AND ITS DBF FILE
# Block.39.ex = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion6")
# Block.dbf = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")   #read dbf file for edit
# 
# ### remove rows with more than 2 (expected) NAs
# Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
# Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]





##########################################################################################
#  find specification by ecoregion 
CntyJep = read.csv(file=paste(DIRECTORY,"\\CntyJepAREACity",VERSION,".csv",sep=""))
# note: FIDS matches the county codes 
jepFIDS = by(CntyJep[,"FIPS"],CntyJep[,"jepson_Unq_ID_edit"],unique)

mapgroups()         # this function adds jepson group ids to the county boundary map 



##########################################################################################
#  NEW SPECIFIER 
# finds specification that minimizes AIC writes output to "specificationsAIC_jepAREAcity",VERSION,".csv"
# spec_holder = data.frame(county="jo", urban=c(as.numeric(NA)),lamda=c(as.numeric(NA)),lamdaP=c(as.numeric(NA)),aic=c(as.numeric(NA)),sqrerr=c(as.numeric(NA)), formulas="HI",stringsAsFactors=F)
# rows = 0
# specifyit <- function(counties ){
#     # specifyit tests quadratic  ^3 vs ^2 vs ^1 vs Ln
#     if(class(counties)=="list"){  counties = unlist(counties)  }
#     
#     for (urban1 in 1:2){
#         rows <<- rows+1   # keep <<- 
#         get_data(urban1,counties,urban_rural_threshold=1)     # 0 for all, 1 for urban, 2 for rural
#         
#         resultlist <<- specifierAIC4(county=counties,urban = urban1)
#         print(resultlist)
#         spec_holder[rows,"county"]   =  paste( counties, collapse=" ") 
#         spec_holder[rows,"urban"]    =    urban1
#         spec_holder[rows,"formulas"] =  resultlist[[2]]["eq_vars"] #  resultlist[[1]] is choice by sqrError  resultlist[[2]] is by AIC
#         spec_holder[rows,"lamda"]    =    resultlist[[2]]["rho"] 
#         spec_holder[rows,"lamdaP"]   =   resultlist[[2]]["rhoP"]
#         spec_holder[rows,"aic"]   =   resultlist[[2]]["aic"]
#         spec_holder[rows,"sqrerr"]   =   resultlist[[2]]["sqrerr"]
#         print(paste(spec_holder,sep = ""))
#     }
#     spec_holder <<- spec_holder  # keep <<- 
#     print(spec_holder)
#     print("#################  ONE COUNTY COMPLETE ###############")  
#     print(Sys.time())
# }
# 
# system.time(lapply(jepFIDS[1:length(jepFIDS)], function(x) specifyit(unlist(x))))
# setwd( paste(DIRECTORY))
# write.csv( spec_holder, file = paste("specificationsAIC_jepAREAcity",VERSION,".csv", sep="") )
# 
#  





 
########################################################################################
# estimate and export
# using specifications set above, estimate regression coefficients, store them and 
spec_holder = read.table(file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,".csv",sep=""),header=T, sep=",", stringsAsFactors=T)

sink(paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,"_sink.txt",sep=""),append=T,split=T )
 
spec_holder

plm_out12=list()
n= -2  #start at -2
runit <- function(counties){
      n<<-n+2
      for (urban1 in  1:2){       
        spec_fn = spec_holder[spec_holder$county==paste(counties, collapse=" ") &  spec_holder$urban==urban1,"formulas"] 
        
        get_data(urban = urban1, counties ,urban_rural_threshold=1 )     # urban = 0 for all, 1 for urban, 2 for rural  # defaults:urban = urban1, counties ,urban_rural_threshold=1
                
        # should loop through counties applying coeffecients individually
        export_out = estimate_export_region(urban=urban1,counties=counties,spec_fn=spec_fn)  
        plm_out = export_out[[1]]
        time_coef = export_out[[2]]   
        time_coef_out = do.call(rbind,time_coef$time.coef)
        row.names(time_coef_out) = unlist(time_coef$county)
        colnames(time_coef_out) = c( colnames(time_coef_out)[-c( (dim(time_coef_out)[2]-4):(dim(time_coef_out)[2])   )],'109','119','129','139','149')
        write.table(time_coef_out, file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,"time_coef.csv",sep=""), sep=',',append=T)
        
        print("#################  ONE COUNTY COMPLETE ###############")  
        print(Sys.time())   

        plm_out12[[n+urban1]] = plm_out
        }
      plm_out12<<-plm_out12
      return(plm_out12 )
}

system.time( lapply( jepFIDS[1:(length(jepFIDS)-0)  ], function(x) runit(unlist(x))) )

#  save(plm_out12,  file = paste(DIRECTORY,"regression_out.RData",sep="\\") ) 
   



###################################################
#  get regression summaries 
# mean median results 
load( file = paste(DIRECTORY,"regression_out.RData",sep="\\") )

    regressionlist = plm_out12  # get all regresions
    urban_regressions = lapply( seq(1,length(regressionlist),by=2), function(x) regressionlist[[x]])
    rural_regressions = lapply( seq(2,length(regressionlist),by=2), function(x) regressionlist[[x]])
    
    urban_county_coefficients= unlist(lapply(1:length(urban_regressions), function(x) urban_regressions[[x]]$coefficients[grep('COUNTY',names(urban_regressions[[x]]$coefficients))]))
    median(urban_county_coefficients)
    urban_other_coefficients= lapply(1:length(urban_regressions), function(x) urban_regressions[[x]]$coefficients[-c(grep('COUNTY',names(urban_regressions[[x]]$coefficients)))])


    urban_other_coefficients_table =  as.data.frame( t(urban_other_coefficients[[1]]),)
    for(i in 2:length(urban_other_coefficients)) {urban_other_coefficients_table= rbind.fill(urban_other_coefficients_table, as.data.frame( t(urban_other_coefficients[[i]]),)  ) } # merge by column name
    apply(urban_other_coefficients_table,2,median,na.rm=T)
    urban_county_P=  unlist(lapply(1:length(urban_regressions), function(x) summary(urban_regressions[[x]])$CoefTable[grep('COUNTY',row.names(summary(urban_regressions[[x]])$CoefTable) ),'Pr(>|t|)']))
    urban_other_P=  lapply(1:length(urban_regressions), function(x) summary(urban_regressions[[x]])$CoefTable[-c(grep('COUNTY',row.names(summary(urban_regressions[[x]])$CoefTable) )),'Pr(>|t|)'])
    urban_other_P_table = as.data.frame( t(urban_other_P[[1]]),)
    for(i in 2:length(urban_other_P)) {urban_other_P_table= rbind.fill(urban_other_P_table, as.data.frame( t(urban_other_P[[i]]),)  ) } # merge by column name
    apply(urban_other_P_table,2,median,na.rm=T)
    apply(urban_other_P_table<=0.1,2,sum,na.rm=T)/apply(!is.na(urban_other_P_table),2,sum,na.rm=T) # percentage significant
       

    rural_county_coefficients= unlist(lapply(1:length(rural_regressions), function(x) rural_regressions[[x]]$coefficients[grep('COUNTY',names(rural_regressions[[x]]$coefficients))]))
    rural_other_coefficients= lapply(1:length(rural_regressions), function(x)  rural_regressions[[x]]$coefficients[-c(grep('COUNTY',names(rural_regressions[[x]]$coefficients)))])
    rural_other_coefficients_table =  as.data.frame( t(rural_other_coefficients[[1]]),)
    for(i in 2:length(rural_other_coefficients)) {rural_other_coefficients_table= rbind.fill(rural_other_coefficients_table, as.data.frame( t(rural_other_coefficients[[i]]),)  ) } # merge by column name
    
     apply(rural_other_coefficients_table,2,median,na.rm=T)
     rural_county_P=  unlist(lapply(1:length(rural_regressions), function(x) summary(rural_regressions[[x]])$CoefTable[grep('COUNTY',row.names(summary(rural_regressions[[x]])$CoefTable) ),'Pr(>|t|)']))
     rural_other_P=  lapply(1:length(rural_regressions), function(x) summary(rural_regressions[[x]])$CoefTable[-c(grep('COUNTY',row.names(summary(rural_regressions[[x]])$CoefTable) )),'Pr(>|t|)'])
     rural_other_P_table = as.data.frame( t(rural_other_P[[1]]),)
     for(i in 2:length(rural_other_P)) {rural_other_P_table= rbind.fill(rural_other_P_table, as.data.frame( t(rural_other_P[[i]]),)  ) } # merge by column name
     apply(rural_other_P_table,2,median,na.rm=T)
     apply(rural_other_P_table<=0.1,2,sum,na.rm=T)/apply(!is.na(rural_other_P_table),2,sum,na.rm=T) # percentage significant
                                     
                                     
                                     
                                     

# urban_other_table2 = lapply(1:length(urban_regressions), function(x) summary(urban_regressions[[x]])$CoefTable)
# urban_other_table2 = lapply(1:length(urban_other_table2), function(x) colnames(urban_other_table2[[x]])= paste(colnames(urban_other_table2[[x]]),x,sep="_") )
library(plyr)
do.call(rbind.fill, list(urban_other_table2[[1]],urban_other_table2[[2]]))

    rho_urban_regressions = unlist(lapply( 1:length(urban_regressions), function(x) summary(urban_regressions[[x]])$arcoef))
    rhop_urban_regressions = unlist(lapply( 1:length(urban_regressions), function(x) summary(urban_regressions[[x]])$ARCoefTable[,'Pr(>|t|)']))
    percent_sig_rhop_urban_regressions=sum( rhop_urban_regressions <=0.1)/length(rhop_urban_regressions)
    median(rho_urban_regressions)
    
range(rho_urban_regressions)
rho_rural_regressions = unlist(lapply( 1:length(rural_regressions), function(x) summary(rural_regressions[[x]])$arcoef))
    rhop_rural_regressions = unlist(lapply( 1:length(rural_regressions), function(x) summary(rural_regressions[[x]])$ARCoefTable[,'Pr(>|t|)']))
    percent_sig_rhop_rural_regressions=sum( rhop_rural_regressions <=0.1,na.rm=T)/length(na.omit(rhop_rural_regressions))
    median(rho_rural_regressions)
range(rho_rural_regressions)

    rsqr_urban_regressions = unlist(lapply( 1:length(urban_regressions), function(x) summary(urban_regressions[[x]])$rsqr))    
    rsqr_rural_regressions = unlist(lapply( 1:length(rural_regressions), function(x) summary(rural_regressions[[x]])$rsqr))    
    median(c(rsqr_urban_regressions,rsqr_rural_regressions))


###################################################
#  get regression summaries 
#  predict out of sample just using housing density coef
load( file = paste(DIRECTORY,"regression_out.RData",sep="\\") )

regressionlist = plm_out12  # get all regresions

for(i in 1:22){
  model.data = regressionlist[i][[1]]$model
  minum = min(model.data$HuDaclag)
  maxum = max(model.data$HuDaclag)
  new.den.range = seq(minum, maxum*3,length.out=length(model.data$HuDaclag))
  model.coef =regressionlist[i][[1]]$coefficients 
  
  prediction = new.den.range*model.coef['HuDaclag'] + new.den.range^2*model.coef['HuDaclagSqr'] +new.den.range^3*model.coef['HuDaclagQb']  
  
  if(i==1){plot(new.den.range, prediction,type="l",xlim=c(0,5),ylim=c(0,10))}
  if(i!=1){lines(new.den.range, prediction,type="l",col=rainbow(22)[i])}
 }



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################




specifierAIC4 <- function(county,urban){
    # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
    n=dim(panel.specify)[1]
    lags = c('HuDaclag+HuDaclagSqr+HuDaclagQb','HuDaclag + HuDaclagSqr','LnHuDaclag')
    variables = c( 'dist_ppall'  )
    variables2 = c('tminave+I(tminave^2)+tmaxave+I(tmaxave^2)+pptave')
    
    
    eq_vars  =  do.call(paste, c(expand.grid(variables2,variables,lags),sep="+"))    
    rho_hold = data.frame(rho=rep(as.numeric(NA),length(eq_vars)), rhoP=rep(as.numeric(NA),length(eq_vars)),aic=rep(as.numeric(NA),length(eq_vars)),sqrerr=rep(as.numeric(NA),length(eq_vars)) )
    row.names(rho_hold) = paste("F", paste( rep(length(lags):1,each=length(variables)*length(variables2)),1:length(eq_vars),sep="") ,sep="")
    rho_hold$eq_vars = eq_vars
    
    if(length(county) >=1 ) {
        rho_hold$eq_vars = paste("COUNTY+",rho_hold$eq_vars,sep='')
    }
    rho_hold$eq_vars = paste("HuDac4 ~",rho_hold$eq_vars,sep='')
    
    
    for(fi in 1:length(rho_hold$eq_vars))  {
        specify = tryCatch( {spml(formula=as.formula(rho_hold$eq_vars[fi]),data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
                                                                                                                                                                  return("errors") })    
        print(summary(specify))
        if(specify=="errors"){
            next
        } else{
            k=(length(specify$coefficients)-1)
            AIC3a = 2*k - 2*specify$logLik
            AICc3a = AIC3a + (2*k*(k+1))/(n-k-1)
            rho_hold$rho[fi]  = summary(specify)$ErrCompTable[1]
            rho_hold$rhoP[fi] = summary(specify)$ErrCompTable[4]    
            rho_hold$aic[fi]  = AICc3a
            rho_hold$sqrerr[fi]  = sum(specify$residuals^2,na.rm=T)
            remove(specify)
        }     
    }     
    rho_hold[is.na(rho_hold$rho),] = 1000000   # avoid choosing NAs as minimum value   
    write.csv(rho_hold, paste(DIRECTORY,"\\specificationsAIC_",urban,"_",county[1],"_",VERSION,".csv",sep="") )
    
    choice_sqrerr = rho_hold[rho_hold$sqrerr==min(rho_hold$sqrerr,na.rm=T),]
    choice_AIC = rho_hold[rho_hold$aic==min(rho_hold$aic,na.rm=T),]    
    return(list(choice_sqrerr,choice_AIC,rho_hold))
}



estimate_export_region <- function(urban,counties,spec_fn){   
  #  use only for jepson secifications
  text_formula= paste(spec_fn)
  if(length(grep("HuDac4",text_formula)) !=0){fm3 = as.formula(paste( spec_fn,"+time",sep="") )}
  if(length(grep("HuDac4",text_formula)) ==0){fm3 = as.formula(paste("HuDac4 ~ ",spec_fn,"+time",sep="") )}
  print(fm3)
  pool = spmlXX(fm3, data= panel.data2, dataXX=panel.data2XX, listw=WKNN4, index=c("id","time"),  model="pooling", lag=T, spatial.error = "none") #spatial.error="b", lag=FALSE)
  print(summary(pool))
  print(summary(pool)$rsqr)
  time_coef_holder= list()
  
  for(i in 1:length(counties)){
    if( dim(panel.data2XX[unfactor(panel.data2XX$COUNTY) %in% counties[i],])[1]>0 ){   #avoid empty counties (ie no rural or urban)
      forecast_out = forecastTDPOP_County3(data=panel.data2XX[unfactor(panel.data2XX$COUNTY) %in% counties[i],], results=pool,urban=urban,Rural_threshold=urban_rural_threshold, Rural_Amp_factor=1,depreciation=1, county_scale=F, polys=Block.39_spC[unfactor(Block.39_spC$COUNTY) %in% counties[i],],formula=fm3, constant=T,timedummy=T, years=5, calcA=T)
      panel.data4XX = forecast_out[[1]] 
      all.coefficients = forecast_out[[2]]
      panel.data4XX2 = data.frame(panel.data4XX)
      
      time_coef_holder$county[i] = list(as.numeric(counties[i]))
      time_coef_holder$time.coef[i] = list(all.coefficients)
      
      # Export forcast
      Block.39_spC2 = Block.39_spC[unfactor(Block.39_spC$COUNTY) %in% counties[i],]    # limit polygons to current estimation county
      Block.39_spC2$sort = 1:length(Block.39_spC2$TotPxl)
      data.wide4XX = reshape(panel.data4XX2[,c("id","time","SUM090","HuDac4")], idvar="id", timevar="time", direction="wide",sep="_")
      data.wide4XX = subset(data.wide4XX, select=-c(SUM090_59,SUM090_69,SUM090_79,SUM090_89,SUM090_99,SUM090_109,SUM090_119,SUM090_129,SUM090_139,SUM090_149))
      merger = merge(slot(Block.39_spC2,"data"), data.wide4XX, by.x="SUM090", by.y="SUM090_49",all.x=T)
      merger  = merger[with(merger, order(sort) ), ]                  # resort to polygon order
      merger$id  = factor(merger$id)
      slot(Block.39_spC2,"data") = data.frame(merger)                 # replace poly data
      
      setwd(paste(DIRECTORY))
      writeOGR(Block.39_spC2, getwd(), paste("County_ECOcity", VERSION,counties[i],"_",urban, sep=""), driver="ESRI Shapefile")
      write.dbf(slot(Block.39_spC2,"data"),file= paste("County_ECOcity", VERSION,counties[i],"_",urban,".dbf", sep=""),factor2char=T)   
        }
    }
  return(list(pool,time_coef_holder))
}




#######################################################
addpredyear2 <- function(addyears,data){
  print("adding prediction year")
  # NOTE: DATA EXPANDED TIME VARIENT AND TIME INVARIENT KEPT AT PREVIOUS VALUE
  
  if (addyears==0){return(NULL)} 
  # time invariant variables stored in "tvarient" above 
  # add the additional observations 'addyears' then merge orginal data
  maxyear = max(as.numeric(as.character(data$time)))                         # save for later
  
  for (i in 1:addyears){   
    time = rep(c(unique(as.numeric(as.character(data$time))),max(as.numeric(as.character(data$time)))+10), length(unique(data$id)))
    id   = rep(unique(data$id), each=max(length(unique(time))))
    data2 = data.frame(time, id)
    data = merge(data2, data, all.x=T)
    data = data[with(data, order(id,time) ), ]
    row.names(data) = paste(data$id,data$time,sep=".")
    remove(data2)
  }
  
  # convert data from long to wide in order to replace missing values from time invariant variables
  set.seed(10)
  print(paste("Sample size:", min(length(data$id),200) ) )
  samples  = sample(data$id, min(length(data$id),200) , replace=T  )          # SAMPLE B/C PVAR TAKE A LONG TIME limit to 200
  tvarient = pvar(na.omit(data[data$id %in% samples,]))$id.variation          # get time variant again
  data = pdata.frame(data, c("id","time"), drop.index=FALSE)                  # declare panel data individ and time
  
  # convert to wide
  data5 = reshape(data, dir = "wide", timevar="time", idvar="id")             # convert to wide to make replacing NAs easier
  
  for (j in seq(maxyear,(maxyear+addyears*10),by=10)  ){
    # FILL YEARS WITH NO VALUES WITH T-1 
    data5[,grep(j+10,names(data5))] = data5[,grep(j,names(data5))]             # replace 99+ Nas with previous year
  }
  
  data5 = reshape(data5, dir = "long", sep=".")                                    # convert back to long
  data5 = pdata.frame(as.data.frame(data5), c("id","time"), drop.index=FALSE)      # declare panel data DOESN'T WORK WITHOUT
  
  h =  strsplit(names(data5), split="\\.")
  for(g in 1:length(h)){
    if (h[[g]][1] == "Row") {h[[g]] = c("Row.names", "39")  }                  # avoid problem with string split of Row.names on "."                                       # replace b/c Row.names is split to "Row" "names"  
  }
  
  names(data5) = apply(as.data.frame(h)[1,], 2, function(x) as.character(x))   # remove .39 from names
  data5 = data5[with(data5, order(id,time) ), ]                                # resort
  
  # recalculate lags and squares 
  if(is.null(data5$HuDac4)){ data5$HuDac4=0 }
  data5 = add_panel_variables(data5)                                           # replace lags etc
  
  # panel.data5$Row.names = paste(panel.data5$id, panel.data5$time, sep="-")
  data5$HuDac4[data5$time == max(unfactor(data5$time))]  = NA                  # make sure that this is NA b/c prediction function looks for missing values to fill 
  data5$EVHudac4[data5$time == max(unfactor(data5$time))]  = NA                  # make sure that this is NA b/c prediction function looks for missing values to fill 
  
  return( data5 )    # assign to global env. 
}




forecastTDPOP_County3 <- function(data,polys,urban,county_scale,Rural_threshold,Rural_Amp_factor,formula,results,depreciation,constant, years, timedummy,calcA){ 
  ############# begin routine
  EVHudac4 = data.frame(EVHudac4 = results$fitted.valuesXX)
  holdthecounty = unique(data$COUNTY)
  data = merge(data, EVHudac4, by="row.names")
  data = data[with(data, order(id,time) ), ]
  data =  addpredyear2(1, data)              # add one year to original data
  data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights
  if(is.null(data$COUNTY) ){data$COUNTY=holdthecounty
                            print(paste("County:",holdthecounty,sep=" "))}
  
  fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
  Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
  
  N = length(unique(row.names(Xs)))
  I = diag(N)
  rho = results$arcoef
  
  ## Prepare data
  ## Distance weights - limit to areas included in regression using ider.SUM090
  data.wide = reshape(data[,c("id","time","SUM090")], idvar="id", timevar="time", direction="wide")
  ider = strsplit(row.names(Xs), split="-")
  ider = unique(as.vector(unlist(lapply(1:length(ider), function (x) ider[[x]][1]))))   # unique ids
  ider.SUM090 = data.wide$SUM090.49[data.wide$id %in% ider]
  data_ider = polys[polys$SUM090 %in% ider.SUM090,]    
  W = listw2mat(theneigh(data_ider))
  
  if (results$call$model == "pooling"){   # subtracting off mean error
    ## Mu (using mean resid as FE for now)
    ider2 = strsplit(row.names(data.frame(Resd=results$residualsXX)), split="-")
    ids2  = as.vector(as.numeric(unlist(lapply(1:length(ider2), function (x) ider2[[x]][1]))))   # unique ids
    names(ids2) = row.names(data.frame(Resd=results$residualsXX))  
    XsR   = merge(data.frame(ids2=ids2), data.frame(Resd=results$residualsXX), by="row.names")
    
    # get unique id from panel.data2XX that match SUM090 from fore.data thereby limiting to needed sample, put in same order as SUM090 in foredata
    get = panel.data2XX[as.character(panel.data2XX$SUM090) %in% as.character(fore.data$SUM090) & panel.data2XX$time == 69, c("id","SUM090")]
    XsR = XsR[XsR$ids2 %in% as.numeric(get$id),]                  # limit to correct ids    
    Prdmn = aggregate(Resd ~ ids2, data = XsR, FUN="mean", na.rm=TRUE )   # GET MEAN ERROR (av can be used in prediction Baltagi '10')
    Prdmn = Prdmn[with(Prdmn, order(ids2) ), 2]    # order and drop id
  }
  
  
  if(calcA==T) { print("Calculating A")
                 A <<- solve(I - rho*W)}
  
  ### READ IN HOUSING COUNT DATA AND OPTIMIZE TIME DUMMY TO ESTIMATE HOUSING DENSITy
  # add coefficient for forecast time dummies
  gethouseproj <- function(urban_in){
    house = read.csv("G:\\Faculty\\Mann\\Share\\Population_Forecast\\HCum_Forecast.csv")
    house = house[unfactor(house$FIPS.x) %in% counties,]
    fore.house = house[house$time %in% c(max(unfactor(data$time))+1900),]      # isolate prediction year   +1900 to convert time stamp 
    fore.house$FIPS.x=as.numeric(fore.house$FIPS.x)
    county_area=aggregate(Est_Acr~COUNTY, data=data[data$time==49,], sum)  # get total inhabitable acres of each block group  '49 because multiple years in data
    county_area$COUNTY=as.numeric(as.character(county_area$COUNTY))
    county_house = merge(fore.house, county_area, by.x="FIPS.x", by.y="COUNTY")  
    # limit estimated houses according 0 all, 1 urban, 2 rural sampling
    if(urban_seperate ==T){
      if(urban_in==1){  print("RUNNING URBAN CORRECTION")
                        percent_urban = county_house$Cum_99U2cut.588/(county_house$Cum_99U2cut.588+county_house$Cum_99R2cut.588)
                        print(paste("Percent urban is",percent_urban,sep=" "))
                        county_house$CumEst_2000 = county_house$CumEst_2000*percent_urban  }
      if(urban_in==2){ print("RUNNING RURAL CORRECTION") 
                       percent_rural = county_house$Cum_99R2cut.588/(county_house$Cum_99U2cut.588+county_house$Cum_99R2cut.588)
                       print(paste("Percent rural is",percent_rural,sep=" "))
                       county_house$CumEst_2000 = county_house$CumEst_2000*percent_rural }
    }
    if(urban_seperate ==F){ print("TOO FEW BLOCK GROUPS, not seperating urban & rural")}
    
    print(paste("Target # of houses is",round(county_house$CumEst_2000), sep=" "))
    county_house <<- county_house
  }
  #gethouseproj(urban)   # get population and housing forecasts 
  
  # initial value for optimization
  start = results$coefficients[length(results$coefficients)]
  results$coefficients=  c(results$coefficients,start) # add initial iterate later
  
  rural_amp <- function(threshold,Amp_factor,Yfore){
    # can be used to take previous values of low density housing and increase them
    lagyear = max(unfactor(data$time))-10
    lagthreshold = (data$HuDac4[data$time == 99] < threshold)          # find   where prediction is lower than threshold in 99
    if(sum(lagthreshold)==0){return(Yfore)
                             print("No rural areas to increase")    }
    previous_den = data$HuDac4[data$time == lagyear] 
    Yfore[lagthreshold,] = previous_den[lagthreshold] *  Amp_factor    THIS IS USUALY ON FOR REAL ESTIMATION
    return(Yfore)
  }
  
  ### Function to choose optimal time dummy 
  print("Start Optimization aggregate house count sum of squares")
  OPTIM = function(dum.i){  
    results$coefficients[length(results$coefficients)] = dum.i              # replace with optimal dum.i
    
    Yfore = data.frame(Yfore = A %*% Xs %*% results$coefficients + A %*% Prdmn)  
    row.names(Yfore) = row.names(Xs)
    
    # rural amplification -  increase rate of growth
    if(Rural_Amp_factor!=1){Yfore = rural_amp( Rural_threshold, Rural_Amp_factor,Yfore)} 
    
    # get estimated # houses by county
    merger = merge(fore.data, Yfore, by="row.names"  )             # merge density estimates with data
    row.names(merger) = merger$Row.names 
    merger$est_house_fore =   merger$Yfore * merger$Est_Acr        # estimate # houses in block group
    est_house_fore_county = aggregate(est_house_fore~COUNTY, data=merger, sum)      
    est_house_fore_county$COUNTY = as.numeric(as.character(est_house_fore_county$COUNTY))
    est_house_fore_county = merge(county_house, est_house_fore_county, by.x="FIPS.x", by.y="COUNTY")
    est_house_count <<- est_house_fore_county
    #print( paste(est_house_fore_county$est_house_fore/ est_house_fore_county$CumEst_2000, "%", sep=" " )  )           
    out =   sum(abs(est_house_fore_county$CumEst_2000 - est_house_fore_county$est_house_fore)) 
    #print(paste("off by =",out,sep=" "))
    return(out)
  }
  # run optimization of time dummy variable
  dum=nlminb( c(results$coefficients[length(results$coefficients)]),OPTIM, lower=-Inf, upper=Inf)
  OPTIM(dum$par)
  results$coefficients[length(results$coefficients)] = dum$par                # replace coeff
  print("Results from time dummy optimization NLM:")
  print(paste("Dummy Estimated as", round(dum$par,3), sep=" "))
  print(paste(  round(est_house_count$est_house_fore),"houses simulated of",round(est_house_count$CumEst_2000), "estimate","=",est_house_count$est_house_fore/est_house_count$CumEst_2000,sep=" "))
  if(est_house_count$est_house_fore/est_house_count$CumEst_2000 >2 | est_house_count$est_house_fore/est_house_count$CumEst_2000 < -2){ print("ERROR: ESTIMATE OUT OF BOUNDS STOPING ESTIMATION")
                                                                                                                                         print("##################################")}
  
  # REESTIMATE DENSITIES
  Yfore = data.frame(Yfore = A %*% Xs %*% results$coefficients + A %*% Prdmn) 
  Yfore[Yfore<0] = 0  
  # THIS SHOULD BE LIMITED SO DENSITIES DON'T DECLINE
  row.names(Yfore) = row.names(Xs)
  
  # rural amp controled for in optimization 
  if(Rural_Amp_factor!=1){
    print(paste("Rural Amplification Implemented for all block groups with density <",Rural_threshold,"houses/ha",sep=" "))
    print(paste("They will grow at a rate of",(Rural_Amp_factor-1),"for all prediction years",sep=" "))
    Yfore = rural_amp( Rural_threshold, Rural_Amp_factor,Yfore)
  } 
  
  # INSERT ESTIMATES INTO ORIGINAL DATA
  data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA     # NA out forecast data
  data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= Yfore[na.omit(match(row.names(data),row.names(Yfore))),"Yfore"]   # replace with forecast
  
  # RESTRICT SO NO DECLINING DENSITIES
  RESTICT <- function(data){
    maxyear = max(unfactor(data$time))
    tmaxyear=((data$HuDac4[data$time == maxyear] < data$HuDac4[data$time == (maxyear-10)])) # find dates where prediction is lower than previous value
    tmaxyear= tmaxyear[tmaxyear==T]            # get only true values 
    if(sum(tmaxyear)!=0){                      # only restrict for samples that actually have some declining values
      prev_v_EVHudac4 = lag(data[,c("EVHudac4")])          # isolate previous value for replacement
      prev_v_HuDac4 = lag(data[,c("HuDac4")])*depreciation   # *0.98 assumes 2% housing loss in stagnant areas
      #########################################
      data[row.names(data) %in% names(tmaxyear),c("EVHudac4", "HuDac4")] = cbind(prev_v_EVHudac4[names(prev_v_EVHudac4) %in% names(tmaxyear)], prev_v_HuDac4[names(prev_v_HuDac4) %in% names(tmaxyear)])  #replace declining value with previous
    }
    return(data)  
  }
  #data = RESTICT(data)
   
  
  ##############################
  ### add more prediction years
  for (j in 1:(years-1)){    
        # trouble again with "row.name" maybe... 
        data =  addpredyear2(1, data)              # add one year to original data
        if(is.null(data$COUNTY) ){data$COUNTY=holdthecounty}   # add in the county again
        data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights
        
        fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
        print( c("year",max(unfactor(data$time))) )
        Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
        gethouseproj(urban)   # get population and housing forecasts  urban or rural
        
        # add coefficient for forecast time dummies
        results$coefficients=  c(results$coefficients,0) # add initial iterate later THIS IS PLACE HOLDER
        
        # run optimization of time dummy variable
        dum=nlminb( c(results$coefficients[length(results$coefficients)]),OPTIM, lower=-Inf, upper=Inf)
        OPTIM(dum$par)
        results$coefficients[length(results$coefficients)] = dum$par                # replace coeff
        print(paste("Results from time dummy optimization NLM in",max(unfactor(data$time)),sep=" "))
        print(paste("Dummy Estimated as", round(dum$par,3), sep=" "))
        print(paste(  round(est_house_count$est_house_fore),"houses simulated of",round(est_house_count$CumEst_2000), "estimate","=",est_house_count$est_house_fore/est_house_count$CumEst_2000,sep=" "))
        if(est_house_count$est_house_fore/est_house_count$CumEst_2000 >10 | est_house_count$est_house_fore/est_house_count$CumEst_2000 < -10){ print("ERROR: ESTIMATE OUT OF BOUNDS STOPING ESTIMATION")
                                                                                                                                               print("##################################")}
        
        
        # reestimate values with new coefficient
        YforeN = data.frame(Yfore= A %*% Xs %*% results$coefficients  + A %*% Prdmn)
        YforeN[YforeN<0] = 0
        row.names(YforeN) = row.names(Xs)
        
        # rural amp controled for in optimization 
        if(Rural_Amp_factor!=1){YforeN = rural_amp( Rural_threshold, Rural_Amp_factor,YforeN)}
        
        data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA
        data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= YforeN[na.omit(match(row.names(data),row.names(YforeN))),"Yfore"]
        
        # RESTRICT SO NO DECLINING DENSITIES
        data = RESTICT(data)
        
        ### Adjust to match expected county housing counts
        if (county_scale==T){  county_scaler() } 
  }  
  
  print("All Coefficients are as follows:")
  print(results$coefficients)
  return(list(data,results$coefficients))
}



mapgroups <- function(){
  # this function adds jepson group ids to the county boundary map
  jepper = unique(CntyJep[with(CntyJep, order(jepson_Unq_ID_edit)),"jepson_Unq_ID_edit"]) #unique jep ids in order
  county_map = read.dbf("G:\\Faculty\\Mann\\Share\\Boundary_Files\\geodatabase\\County.dbf")
  county_map$Group_Map = NA
  county_map$FIPS_Num = unfactor(county_map$FIPS)
  
  for (i in jepper){   # for each Jepson 
    # print(paste("Jepson Zone", i, jepFIDS[[as.character(i)]]))
    
    for(j in jepFIDS[[as.character(i)]] ){   # for each county in a jepson group
      county_map[county_map$FIPS_Num == j,"Group_Map"] = i     # set    
    }
  }
  
  write.dbf(county_map,"G:\\Faculty\\Mann\\Share\\Boundary_Files\\geodatabase\\County.dbf")
}



get_data <- function(urban,counties,urban_rural_threshold){
  #Load relevant data
  urban_rural_threshold <<- urban_rural_threshold  # make a global variable so can be used elsewhere
  
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

  print("starting neighborhood")
  KNN1<<-knn2nb(knearneigh(coordinates(Block.39_sp),k=1))    # use to replace i with no neighbors
  DNN3<<-dnearneigh(coordinates(Block.39_sp),0,20000,longlat=F)   # in meters for this projection
  #PNN3=poly2nb(Block.39_sp, snap=c(500),row.names=as.character(1:length(Block.39_sp)) )     # change row.names to match polygon.id from attributes(DNN3)
  #print(PNN3)
  
#   for(x in 1:length(PNN3)){     # replace empty with k near neigh
#     if( PNN3[[x]][1] == 0 ){PNN3[[x]] = KNN1[[x]]}
#   }
#   DNN3 = union.nb(DNN3,PNN3)
for(x in 1:length(DNN3)){     # replace empty with k near neigh
      if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
  }
  #PNN3 = nblag(PNN3, 5)
  #PNN3 = nblag_cumul(PNN3)
  
  WKNN4 <<- nb2listw(DNN3, style="W")
  print(WKNN4)
  print("ending neighborhood")
  
  ################################################
  ######   PUT DATA INTO PANEL LONG        ######
  panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c(grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp))       ) , sep = "_")
  panel.dataXX= reshape(Block.39.dbf_spC, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_spC)),grep("Cum",names(Block.39.dbf_spC)),grep("DD100_",names(Block.39.dbf_spC)), grep("DD6585_",names(Block.39.dbf_spC)), grep("DD95_",names(Block.39.dbf_spC))      ) , sep = "_")
  
  panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  panel.dataXX <-  pdata.frame(panel.dataXX, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  
  #pdim(panel.data)
  #pdim(panel.dataXX)
  
  panel.data = add_panel_variables(panel.data)
  panel.specify <<-panel.data
  panel.dataXX = add_panel_variables(panel.dataXX)
  
  panel.data2 = data.frame(subset(panel.data, select=c(id,time)), subset(panel.data, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  panel.data2 <<- panel.data2[panel.data2$time %in% 49:99, ]                    # limit to years of interest
  panel.data2XX = data.frame(subset(panel.dataXX, select=c(id,time)), subset(panel.dataXX, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  panel.data2XX <<- panel.data2XX[panel.data2XX$time %in% 49:99, ]                    # limit to years of interest
}   # end get_data 


get_data2 <- function( counties ){
    Block.39.ex<<-Block.39.ex
    
    ###  LIMIT TO COUNTIES 
    counties <<-  counties 
    print(paste("Urban option set to",urban,sep=" "))
    print(paste("Processing County # ",counties,sep=""))
    Block.39_spC = Block.39.ex[unfactor(Block.39.ex$COUNTY) %in% counties,]
    Block.39.dbf_spC = Block.dbf[unfactor(Block.dbf$COUNTY) %in% counties,]
    Block.39.dbf_spC$ordersp = 1:length(Block.39.dbf_spC$SUM090)   #used to reorder data when applying W in matrix math
   
    
    
    Block.39.dbf_spC <<- Block.39.dbf_spC       # make available outside of function
    Block.39_spC     <<- Block.39_spC
    print(paste("# block groups =",dim(Block.39.dbf_spC)[1],sep=" ")  )
    
    
    size = length(unique(Block.39_spC$SUM090))
    if (size > 3500){
        ### create smaller sample   
        NSAMPLES = 3500   
        # set.seed(123)
        set.seed(123)
        Block.39_sp = Block.39_spC[Block.39_spC$SUM090 %in% sample(Block.39_spC$SUM090, NSAMPLES , prob=(Block.39_spC$Est_Acr/sum(Block.39_spC$Est_Acr))   ),]
        # set.seed(123)
        set.seed(123)
        Block.39.dbf_sp = Block.39.dbf_spC[Block.39.dbf_spC$SUM090 %in% sample(Block.39.dbf_spC$SUM090, NSAMPLES, prob=(Block.39.dbf_spC$Est_Acr/sum(Block.39.dbf_spC$Est_Acr))),]
        print("Sample size limited to 1500 for regression")
    }
    
    if (size <= 3500){
        ### keep all observations
        Block.39_sp = Block.39_spC 
        Block.39.dbf_sp = Block.39.dbf_spC 
    }
    # set to maintain sort order for future export
    Block.39.dbf_sp$ordersp = 1:length(Block.39.dbf_sp$SUM090) 
    
    ### neighbors 
    
    print("starting neighborhood")
    KNN1<<-knn2nb(knearneigh(coordinates(Block.39_sp),k=1))    # use to replace i with no neighbors
    DNN3<<-dnearneigh(coordinates(Block.39_sp),0,20000,longlat=F)   # in meters for this projection
#     PNN3=poly2nb(Block.39_sp, snap=c(500),row.names=as.character(1:length(Block.39_sp)) )     # change row.names to match polygon.id from attributes(DNN3)
#     print(PNN3)
#     
#     for(x in 1:length(PNN3)){     # replace empty with k near neigh
#         if( PNN3[[x]][1] == 0 ){PNN3[[x]] = KNN1[[x]]}
#     }
#      DNN3 = union.nb(DNN3,PNN3)
for(x in 1:length(DNN3)){     # replace empty with k near neigh
        if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
    }
    #PNN3 = nblag(PNN3, 5)
    #PNN3 = nblag_cumul(PNN3)
    
    WKNN4 <<- nb2listw(DNN3, style="W")
    print(WKNN4)
    print("ending neighborhood")
    
    ################################################
    ######   PUT DATA INTO PANEL LONG        ######
    panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c(grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp))       ) , sep = "_")
    panel.dataXX= reshape(Block.39.dbf_spC, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_spC)),grep("Cum",names(Block.39.dbf_spC)),grep("DD100_",names(Block.39.dbf_spC)), grep("DD6585_",names(Block.39.dbf_spC)), grep("DD95_",names(Block.39.dbf_spC))      ) , sep = "_")
    
    panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
    panel.dataXX <-  pdata.frame(panel.dataXX, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
    
    #pdim(panel.data)
    #pdim(panel.dataXX)
    
    panel.data = add_panel_variables(panel.data)
    panel.specify <<-panel.data
    panel.dataXX = add_panel_variables(panel.dataXX)
    
    panel.data2 = data.frame(subset(panel.data, select=c(id,time)), subset(panel.data, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
    panel.data2 <<- panel.data2[panel.data2$time %in% 49:99, ]                    # limit to years of interest
    panel.data2XX = data.frame(subset(panel.dataXX, select=c(id,time)), subset(panel.dataXX, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
    panel.data2XX <<- panel.data2XX[panel.data2XX$time %in% 49:99, ]                    # limit to years of interest
}   # end get_data 


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



theneigh <- function(polygons){
  #KNN4XX=knn2nb(knearneigh(coordinates(polygons),k=3))   #12  replace with distance
  #WKNN4XX <<- nb2listw(KNN4XX, style="W")  
  print("begin XX neigh")
  KNN1<<-knn2nb(knearneigh(coordinates(polygons),k=1))    # use to replace i with no neighbors
  DNN3<<-dnearneigh(coordinates(polygons),0,20000,longlat=F)
  #PNN3=poly2nb(polygons, snap=c(500),row.names=as.character(1:length(polygons)) )     # change row.names to match polygon.id from attributes(DNN3)
  for(x in 1:length(DNN3)){     # replace empty with k near neigh
    if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
  }
  #DNN3 = union.nb(DNN3,PNN3)
  #PNN3 = nblag(PNN3, 5)
  #PNN3 = nblag_cumul(PNN3)
  
  WKNN4XX <<- nb2listw(DNN3, style="W")
  print("end XX neigh")
  return(WKNN4XX)
}

 

# 
# specifierAIC3 <- function(){
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   n=dim(panel.specify)[1]
#   
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=c(9,9,9), rhoP=c(9,9,9))
#   row.names(rho_hold) = c("f3","f2","f0")
#   
#   f3 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb
#   f2 = HuDac4 ~ HuDaclag+HuDaclagSqr
#   f0 = HuDac4 ~ LnHuDaclag
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   
#   ################
#   specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify3=="errors"){
#     AIC3= 100000
#     AICc3 = 100000
#   } else{
#     k=(length(specify3$coefficients)-1)
#     AIC3 = 2*k - 2*specify3$logLik
#     AICc3 = AIC3 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify3)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify3)$ErrCompTable[4]      
#     remove(specify3)
#   }
#   
#   ################
#   specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify2=="errors"){
#     AIC2=100000
#     AICc2 = 100000
#   } else{
#     k=(length(specify2$coefficients)-1)
#     AIC2 = 2*k - 2*specify2$logLik
#     AICc2 = AIC2 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[2] = summary(specify2)$ErrCompTable[1]
#     rho_hold$rhoP[2] = summary(specify2)$ErrCompTable[4]
#     remove(specify2)
#   }
#   
#   ################          
#   specify0 = tryCatch( {spml(formula=f0,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify0=="errors"){
#     AIC0=100000
#     AICc0 = 100000
#   } else{
#     k=(length(specify0$coefficients)-1)
#     AIC0 = 2*k - 2*specify0$logLik
#     AICc0 = AIC0 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[3] = summary(specify0)$ErrCompTable[1]
#     rho_hold$rhoP[3] = summary(specify0)$ErrCompTable[4]
#     remove(specify0)
#   }
#   
#   choice = data.frame(f3=AICc3,f2=AICc2,f0=AICc0)
#   if(AIC3== 100000 & AIC2== 100000 & AIC0== 100000){stop("ERROR: No equations working")}
#   usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#   fm3 = get(usef[length(usef)])
#   rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#   rho_hold <<- rho_hold_fm
#   return(fm3)
# }


#specifierAIC3C <- function(){
#  
#  # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#  n=dim(panel.specify)[1]
#  
#  fm3 = 1   # to be replaced
#  rho_hold = data.frame(rho=c(9,9,9), rhoP=c(9,9,9))
#  row.names(rho_hold) = c("f3","f2","f0")
#  
#  f3 = HuDac4 ~ COUNTY+HuDaclag+HuDaclagSqr+HuDaclagQb
#  f2 = HuDac4 ~ COUNTY+HuDaclag+HuDaclagSqr
#  f0 = HuDac4 ~ COUNTY+LnHuDaclag
#  # switch between specification options   if not polynomial use logistic
#  # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#  
#  ################
#  specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                               return("errors") })
#  if(specify3=="errors"){
#    AIC3= 100000
#    AICc3 = 100000
#  } else{
#    k=(length(specify3$coefficients)-1)
#    AIC3 = 2*k - 2*specify3$logLik
#    AICc3 = AIC3 + (2*k*(k+1))/(n-k-1)
#    rho_hold$rho[1] = summary(specify3)$ErrCompTable[1]
#    rho_hold$rhoP[1] = summary(specify3)$ErrCompTable[4]      
#    remove(specify3)
#  }
#  
#  ################
#  specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                               return("errors") })
#  if(specify2=="errors"){
#    AIC2=100000
#    AICc2 = 100000
#  } else{
#    k=(length(specify2$coefficients)-1)
#    AIC2 = 2*k - 2*specify2$logLik
#    AICc2 = AIC2 + (2*k*(k+1))/(n-k-1)
#    rho_hold$rho[2] = summary(specify2)$ErrCompTable[1]
#    rho_hold$rhoP[2] = summary(specify2)$ErrCompTable[4]
#    remove(specify2)
#  }
##  
#  ################          
#  specify0 = tryCatch( {spml(formula=f0,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                               return("errors") })
#  if(specify0=="errors"){
#    AIC0=100000
#    AICc0 = 100000
#  } else{
#    k=(length(specify0$coefficients)-1)
#    AIC0 = 2*k - 2*specify0$logLik
#    AICc0 = AIC0 + (2*k*(k+1))/(n-k-1)
#    rho_hold$rho[3] = summary(specify0)$ErrCompTable[1]
#    rho_hold$rhoP[3] = summary(specify0)$ErrCompTable[4]
#    remove(specify0)
#  }
#  
#  choice = data.frame(f3=AICc3,f2=AICc2,f0=AICc0)
#  if(AIC3== 100000 & AIC2== 100000 & AIC0== 100000){stop("ERROR: No equations working")}
#  usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#  fm3 = get(usef[length(usef)])
#  rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#  rho_hold <<- rho_hold_fm
#  return(fm3)
#}






# 
# specifierAIC5C <- function(){
#   print("############################# NOT WORKING #########################")
#   
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=c(9,9,9,9,9), rhoP=c(9,9,9,9,9))
#   row.names(rho_hold) = c("f5","f4","f3","f2","f0")
#   f5 = HuDac4 ~ COUNTY+HuDaclag+HuDaclagSqr+HuDaclagQb+HuDaclagFr+HuDaclagFv
#   f4 = HuDac4 ~ COUNTY+HuDaclag+HuDaclagSqr+HuDaclagQb+HuDaclagFr
#   f3 = HuDac4 ~ COUNTY+HuDaclag+HuDaclagSqr+HuDaclagQb
#   f2 = HuDac4 ~ COUNTY+HuDaclag+HuDaclagSqr
#   f0 = HuDac4 ~ COUNTY+LnHuDaclag
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   specify5 = tryCatch( {spml(formula=f5,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   n=dim(panel.specify)[1]
#   if(specify5=="errors"){
#     AIC5= 100000
#     AICc5 = 100000
#   } else{
#     k=(length(specify5$coefficients)-1)
#     AIC5 = 2*k - 2*specify5$logLik
#     AICc5 = AIC5 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify5)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify5)$ErrCompTable[4]
#     remove(specify5)
#   }
#   
#   specify4 = tryCatch( {spml(formula=f4,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   n=dim(panel.specify)[1]
#   if(specify4=="errors"){
#     AIC4= 100000
#     AICc4 = 100000
#   } else{
#     k=(length(specify4$coefficients)-1)
#     AIC4 = 2*k - 2*specify4$logLik
#     AICc4 = AIC4 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify4)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify4)$ErrCompTable[4]
#     remove(specify4)
#   }  
#   
#   ################
#   specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify3=="errors"){
#     AIC3= 100000
#     AICc3 = 100000
#   } else{
#     k=(length(specify3$coefficients)-1)
#     AIC3 = 2*k - 2*specify3$logLik
#     AICc3 = AIC3 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[2] = summary(specify3)$ErrCompTable[1]
#     rho_hold$rhoP[2] = summary(specify3)$ErrCompTable[4]      
#     remove(specify3)
#   }
#   
#   ################
#   specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify2=="errors"){
#     AIC2=100000
#     AICc2 = 100000
#   } else{
#     k=(length(specify2$coefficients)-1)
#     AIC2 = 2*k - 2*specify2$logLik
#     AICc2 = AIC2 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[3] = summary(specify2)$ErrCompTable[1]
#     rho_hold$rhoP[3] = summary(specify2)$ErrCompTable[4]
#     remove(specify2)
#   }
#   
#   ################          
#   specify0 = tryCatch( {spml(formula=f0,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify0=="errors"){
#     AIC0=100000
#     AICc0 = 100000
#   } else{
#     k=(length(specify0$coefficients)-1)
#     AIC0 = 2*k - 2*specify0$logLik
#     AICc0 = AIC0 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[4] = summary(specify0)$ErrCompTable[1]
#     rho_hold$rhoP[4] = summary(specify0)$ErrCompTable[4]
#     remove(specify0)
#   }
#   
#   choice = data.frame(f5=AICc5,f4=AICc4,f3=AICc3,f2=AICc2,f0=AICc0)
#   if(AIC5== 100000 & AIC4== 100000 & AIC3== 100000 & AIC2== 100000 & AIC0== 100000){stop("ERROR: No equations working")}
#   usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#   fm3 = get(usef[length(usef)])
#   rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#   rho_hold <<- rho_hold_fm
#   return(fm3)
# }
# 
# specifierAIC5 <- function(){
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=c(9,9,9,9,9), rhoP=c(9,9,9,9,9))
#   row.names(rho_hold) = c("f5","f4","f3","f2","f0")
#   f5 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb+HuDaclagFr+HuDaclagFv
#   f4 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb+HuDaclagFr
#   f3 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb
#   f2 = HuDac4 ~ HuDaclag+HuDaclagSqr
#   f0 = HuDac4 ~ LnHuDaclag
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   specify5 = tryCatch( {spml(formula=f5,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   n=dim(panel.specify)[1]
#   if(specify5=="errors"){
#     AIC5= 100000
#     AICc5 = 100000
#   } else{
#     k=(length(specify5$coefficients)-1)
#     AIC5 = 2*k - 2*specify5$logLik
#     AICc5 = AIC5 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify5)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify5)$ErrCompTable[4]
#     remove(specify5)
#   }
#   
#   specify4 = tryCatch( {spml(formula=f4,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   n=dim(panel.specify)[1]
#   if(specify4=="errors"){
#     AIC4= 100000
#     AICc4 = 100000
#   } else{
#     k=(length(specify4$coefficients)-1)
#     AIC4 = 2*k - 2*specify4$logLik
#     AICc4 = AIC4 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify4)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify4)$ErrCompTable[4]
#     remove(specify4)
#   }  
#   
#   ################
#   specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify3=="errors"){
#     AIC3= 100000
#     AICc3 = 100000
#   } else{
#     k=(length(specify3$coefficients)-1)
#     AIC3 = 2*k - 2*specify3$logLik
#     AICc3 = AIC3 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[2] = summary(specify3)$ErrCompTable[1]
#     rho_hold$rhoP[2] = summary(specify3)$ErrCompTable[4]      
#     remove(specify3)
#   }
#   
#   ################
#   specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify2=="errors"){
#     AIC2=100000
#     AICc2 = 100000
#   } else{
#     k=(length(specify2$coefficients)-1)
#     AIC2 = 2*k - 2*specify2$logLik
#     AICc2 = AIC2 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[3] = summary(specify2)$ErrCompTable[1]
#     rho_hold$rhoP[3] = summary(specify2)$ErrCompTable[4]
#     remove(specify2)
#   }
#   
#   ################          
#   specify0 = tryCatch( {spml(formula=f0,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify0=="errors"){
#     AIC0=100000
#     AICc0 = 100000
#   } else{
#     k=(length(specify0$coefficients)-1)
#     AIC0 = 2*k - 2*specify0$logLik
#     AICc0 = AIC0 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[4] = summary(specify0)$ErrCompTable[1]
#     rho_hold$rhoP[4] = summary(specify0)$ErrCompTable[4]
#     remove(specify0)
#   }
#   
#   choice = data.frame(f5=AICc5,f4=AICc4,f3=AICc3,f2=AICc2,f0=AICc0)
#   if(AIC5== 100000 & AIC4== 100000 & AIC3== 100000 & AIC2== 100000 & AIC0== 100000){stop("ERROR: No equations working")}
#   usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#   fm3 = get(usef[length(usef)])
#   rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#   rho_hold <<- rho_hold_fm
#   return(fm3)
# }
# 
# 
# 
# specifierAIC4 <- function(){
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=c(9,9,9,9), rhoP=c(9,9,9,9))
#   row.names(rho_hold) = c("f4","f3","f2","f0")
#   f4 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb+HuDaclagFr
#   f3 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb
#   f2 = HuDac4 ~ HuDaclag+HuDaclagSqr
#   f0 = HuDac4 ~ LnHuDaclag
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   specify4 = tryCatch( {spml(formula=f4,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   n=dim(panel.specify)[1]
#   if(specify4=="errors"){
#     AIC4= 100000
#     AICc4 = 100000
#   } else{
#     k=(length(specify4$coefficients)-1)
#     AIC4 = 2*k - 2*specify4$logLik
#     AICc4 = AIC4 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify4)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify4)$ErrCompTable[4]
#     remove(specify4)
#   }  
#   
#   ################
#   specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify3=="errors"){
#     AIC3= 100000
#     AICc3 = 100000
#   } else{
#     k=(length(specify3$coefficients)-1)
#     AIC3 = 2*k - 2*specify3$logLik
#     AICc3 = AIC3 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[2] = summary(specify3)$ErrCompTable[1]
#     rho_hold$rhoP[2] = summary(specify3)$ErrCompTable[4]      
#     remove(specify3)
#   }
#   
#   ################
#   specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify2=="errors"){
#     AIC2=100000
#     AICc2 = 100000
#   } else{
#     k=(length(specify2$coefficients)-1)
#     AIC2 = 2*k - 2*specify2$logLik
#     AICc2 = AIC2 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[3] = summary(specify2)$ErrCompTable[1]
#     rho_hold$rhoP[3] = summary(specify2)$ErrCompTable[4]
#     remove(specify2)
#   }
#   
#   ################          
#   specify0 = tryCatch( {spml(formula=f0,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   if(specify0=="errors"){
#     AIC0=100000
#     AICc0 = 100000
#   } else{
#     k=(length(specify0$coefficients)-1)
#     AIC0 = 2*k - 2*specify0$logLik
#     AICc0 = AIC0 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[4] = summary(specify0)$ErrCompTable[1]
#     rho_hold$rhoP[4] = summary(specify0)$ErrCompTable[4]
#     remove(specify0)
#   }
#   
#   choice = data.frame(f4=AICc4,f3=AICc3,f2=AICc2,f0=AICc0)
#   if(AIC4== 100000 & AIC3== 100000 & AIC2== 100000 & AIC0== 100000){stop("ERROR: No equations working")}
#   usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#   fm3 = get(usef[length(usef)])
#   rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#   rho_hold <<- rho_hold_fm
#   return(fm3)
# }


estimate_export <- function(urban,counties,spec_fn){   
  # NOTE NEED TO REPLACE TIME WITH DUMMYS FOR EACH YEAR... SEEMS TO BE USING 109 ?? MAYBE
  fm3 = as.formula(paste("HuDac4~",spec_fn,"+time",sep="") )
  print(fm3)
  pool = spmlXX(fm3, data= panel.data2, dataXX=panel.data2XX, listw=WKNN4, index=c("id","time"),  model="pooling", lag=T, spatial.error = "none") #spatial.error="b", lag=FALSE)
  print(summary(pool))
  print(summary(pool)$rsqr)
  
  panel.data4XX = forecastTDPOP_County2(data=panel.data2XX, results=pool,urban=urban,Rural_threshold=1, Rural_Amp_factor=1,depreciation=0.99, county_scale=F, polys=Block.39_spC,formula=fm3, constant=T,timedummy=T, years=5, calcA=T)
  panel.data4XX2 = data.frame(panel.data4XX)
  
  # Export forcast
  Block.39_spC2 = Block.39_spC
  Block.39_spC2$sort = 1:length(Block.39_spC2$TotPxl)
  data.wide4XX = reshape(panel.data4XX2[,c("id","time","SUM090","HuDac4")], idvar="id", timevar="time", direction="wide",sep="_")
  data.wide4XX = subset(data.wide4XX, select=-c(SUM090_59,SUM090_69,SUM090_79,SUM090_89,SUM090_99,SUM090_109,SUM090_119,SUM090_129,SUM090_139,SUM090_149))
  merger = merge(slot(Block.39_spC2,"data"), data.wide4XX, by.x="SUM090", by.y="SUM090_49",all.x=T)
  merger  = merger[with(merger, order(sort) ), ]                  # resort to polygon order
  merger$id  = factor(merger$id)
  slot(Block.39_spC2,"data") = data.frame(merger)                 # replace poly data
  
  setwd("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\Experiment")
  writeOGR(Block.39_spC2, getwd(), paste("County_specAIC",counties,"_",urban, sep=""), driver="ESRI Shapefile")
  write.dbf(slot(Block.39_spC2,"data"),file=paste("County_specAIC",counties,"_",urban,".dbf", sep=""),factor2char=T)  
}







# 
# specifier <- function(){
#   # DONT USE>>>>  use aic specifier instead
#   print("DONT USE THIS")
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=9, rhoT=9)
#   f4 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb+HuDaclagFr
#   f3 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb
#   f2 = HuDac4 ~ HuDaclag+HuDaclagSqr
#   f1 = HuDac4 ~ HuDaclag
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   specify4 = tryCatch( {spml(formula=f4,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   hole = specify4
#   if(specify4=="errors"){specify4= matrix(1,5,4) } else specify4=as.data.frame((summary(specify4)$CoefTable))
#   if(specify4[dim(specify4)[1],dim(specify4)[2]] <= 0.1 & specify4[dim(specify4)[1]-1,dim(specify4)[2]] <= 0.1 & specify4[dim(specify4)[1]-2,dim(specify4)[2]] <= 0.1 & specify4[dim(specify4)[1]-3,dim(specify4)[2]] <= 0.1 & specify4[dim(specify4)[1],1 ] < 0 ){ fm3 =f4}  
#   if(fm3==f4){  rho_hold[1,1]= summary(hole)$ErrCompTable[,"Estimate"]
#                 rho_hold[1,2]= summary(hole)$ErrCompTable[,"Pr(>|t|)"]
#                 rho_hold <<- rho_hold
#                 return(fm3)} # break if true
#   ################
#   specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   hole = specify3
#   if(specify3=="errors"){specify3= matrix(1,4,4) } else specify3=as.data.frame((summary(specify3)$CoefTable))
#   if(specify3[dim(specify3)[1],dim(specify3)[2]] <= 0.1 & specify3[dim(specify3)[1]-1,dim(specify3)[2]] <= 0.1 & specify3[dim(specify3)[1]-2,dim(specify3)[2]] <= 0.1 & specify3[dim(specify3)[1],1 ] < 0 ){ fm3 =f3} 
#   if(fm3==f3){  rho_hold[1,1]= summary(hole)$ErrCompTable[,"Estimate"]
#                 rho_hold[1,2]= summary(hole)$ErrCompTable[,"Pr(>|t|)"]
#                 rho_hold <<- rho_hold
#                 return(fm3)} # break if true
#   ################
#   specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   hole = specify2
#   if(specify2=="errors"){specify2= matrix(1,3,4) } else specify2=as.data.frame((summary(specify2)$CoefTable))
#   if(specify2[dim(specify2)[1],dim(specify2)[2]] <= 0.1 & specify2[dim(specify2)[1]-1,dim(specify2)[2]] <= 0.1 & specify2[dim(specify2)[1],1 ] < 0 ){ fm3 =f2} 
#   if(fm3==f2){  rho_hold[1,1]= summary(hole)$ErrCompTable[,"Estimate"]
#                 rho_hold[1,2]= summary(hole)$ErrCompTable[,"Pr(>|t|)"]
#                 rho_hold <<- rho_hold
#                 return(fm3)} # break if true        
#   ################        
#   specify1 = tryCatch( {spml(formula=f1,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   hole = specify1
#   if(specify1=="errors"){specify1= matrix(1,2,4) } else specify1=as.data.frame((summary(specify1)$CoefTable))
#   if(specify1[dim(specify1)[1],dim(specify1)[2]] <= 0.1 & specify1[dim(specify1)[1],1 ] < 0 ){ fm3 = f1} else fm3 = HuDac4 ~ LnHuDaclag  
#   if(fm3==f1){  rho_hold[1,1]= summary(hole)$ErrCompTable[,"Estimate"]
#                 rho_hold[1,2]= summary(hole)$ErrCompTable[,"Pr(>|t|)"]
#   }
#   rho_hold <<- rho_hold
#   return(fm3)
# }


#sfInit(parallel=TRUE, cpus=4, type="SOCK")
#stopifnot( sfCpus() == 2 )
#stopifnot( sfParallel() == TRUE )

#system.time(sfExportAll( ) )   # used for large datasets, can use sfExport for individual data exports to slaves
#sfLibrary( splm)
#sfLibrary( spdep)
#sfLibrary( rgdal)

#sfClusterSetupRNG( )
#system.time( sfClusterApplyLB( cnty, function(x) runit(x)))
#sfStop( )
#sfRemoveAll()



#sink()
#unlink("C:/Users/mmann1123/Desktop/Share/Final_Variables_2/regres_Out8.txt")


#############################
#### forecast 
 

 


###########################################





###########################################
forecastTDPOP_County2 <- function(data,polys,urban,county_scale,Rural_threshold,Rural_Amp_factor,formula,results,depreciation,constant, years, timedummy,calcA){ 
  
  ############# begin routine
  EVHudac4 = data.frame(EVHudac4 = results$fitted.valuesXX)
  data = merge(data, EVHudac4, by="row.names")
  data = data[with(data, order(id,time) ), ]
  data =  addpredyear2(1, data)              # add one year to original data
  data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights
  
  fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
  Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
  
  N = length(unique(row.names(Xs)))
  I = diag(N)
  rho = results$arcoef
  
  ## Prepare data
  ## Distance weights - limit to areas included in regression using ider.SUM090
  data.wide = reshape(data[,c("id","time","SUM090")], idvar="id", timevar="time", direction="wide")
  ider = strsplit(row.names(Xs), split="-")
  ider = unique(as.vector(unlist(lapply(1:length(ider), function (x) ider[[x]][1]))))   # unique ids
  ider.SUM090 = data.wide$SUM090.49[data.wide$id %in% ider]
  data_ider = polys[polys$SUM090 %in% ider.SUM090,]    
  W = listw2mat(theneigh(data_ider))
  
  if (results$call$model == "pooling"){
    ## Mu (using mean resid as FE for now)
    ider2 = strsplit(row.names(data.frame(Resd=results$residualsXX)), split="-")
    ids2  = as.vector(as.numeric(unlist(lapply(1:length(ider2), function (x) ider2[[x]][1]))))   # unique ids
    names(ids2) = row.names(data.frame(Resd=results$residualsXX))
    XsR   = merge(data.frame(ids2=ids2), data.frame(Resd=results$residualsXX), by="row.names")
    Prdmn = aggregate( Resd ~ ids2, data = XsR, FUN="mean", na.rm=TRUE )   # GET MEAN ERROR (av can be used in prediction Baltagi '10')
    Prdmn = Prdmn[with(Prdmn, order(ids2) ), 2]
  }
  
  
  if(calcA==T) { print("Calculating A")
                 A <<- solve(I - rho*W)}
  
  ### READ IN HOUSING COUNT DATA AND OPTIMIZE TIME DUMMY TO ESTIMATE HOUSING DENSITy
  # add coefficient for forecast time dummies
  gethouseproj <- function(urban_in){
    house = read.csv("G:\\Faculty\\Mann\\Share\\Population_Forecast\\HCum_Forecast.csv")
    house = house[unfactor(house$FIPS.x) %in% counties,]
    fore.house = house[house$time %in% c(max(unfactor(data$time))+1900),]      # isolate prediction year   +1900 to convert time stamp 
    fore.house$FIPS.x=as.numeric(fore.house$FIPS.x)
    county_area=aggregate(Est_Acr~COUNTY, data=data[data$time==49,], sum)  # get total inhabitable acres of each block group  '49 because multiple years in data
    county_area$COUNTY=as.numeric(as.character(county_area$COUNTY))
    county_house = merge(fore.house, county_area, by.x="FIPS.x", by.y="COUNTY")  
    # limit estimated houses according 0 all, 1 urban, 2 rural sampling
    if(urban_seperate ==T){
      if(urban_in==1){  print("RUNNING URBAN CORRECTION")
                        percent_urban = county_house$Cum_99U2cut.588/(county_house$Cum_99U2cut.588+county_house$Cum_99R2cut.588)
                        print(paste("Percent urban is",percent_urban,sep=" "))
                        county_house$CumEst_2000 = county_house$CumEst_2000*percent_urban  }
      if(urban_in==2){ print("RUNNING RURAL CORRECTION") 
                       percent_rural = county_house$Cum_99R2cut.588/(county_house$Cum_99U2cut.588+county_house$Cum_99R2cut.588)
                       print(paste("Percent rural is",percent_rural,sep=" "))
                       county_house$CumEst_2000 = county_house$CumEst_2000*percent_rural }
    }
    if(urban_seperate ==F){ print("TOO FEW BLOCK GROUPS, not seperating urban & rural")}
    
    print(paste("Target # of houses is",round(county_house$CumEst_2000), sep=" "))
    county_house <<- county_house
  }
  gethouseproj(urban)   # get population and housing forecasts 
  
  # initial value for optimization
  start = results$coefficients[length(results$coefficients)]
  results$coefficients=  c(results$coefficients,start) # add initial iterate later
  
  rural_amp <- function(threshold,Amp_factor,Yfore){
    # can be used to take previous values of low density housing and increase them
    lagyear = max(unfactor(data$time))-10
    lagthreshold = (data$HuDac4[data$time == 99] < threshold)          # find   where prediction is lower than threshold in 99
    if(sum(lagthreshold)==0){return(Yfore)
                             print("No rural areas to increase")    }
    previous_den = data$HuDac4[data$time == lagyear] 
    Yfore[lagthreshold,] = previous_den[lagthreshold] *  Amp_factor
    return(Yfore)
  }
  
  ### Function to choose optimal time dummy 
  print("Start Optimization aggregate house count sum of squares")
  OPTIM = function(dum.i){  
    results$coefficients[length(results$coefficients)] = dum.i              # replace with optimal dum.i
    #print(paste(dum.i, "dummy value",sep=" "))
    Yfore = data.frame(Yfore = A %*% Xs %*% results$coefficients + A %*% Prdmn) 
    #Yfore[Yfore<0] = 0    CAN'T DO THIS MESSES WITH OPTIMIZATION
    row.names(Yfore) = row.names(Xs)
    
    # rural amplification -  increase rate of growth
    if(Rural_Amp_factor!=1){Yfore = rural_amp( Rural_threshold, Rural_Amp_factor,Yfore)} 
    
    # get estimated # houses by county
    merger = merge(fore.data, Yfore, by="row.names"  )             # merge density estimates with data
    row.names(merger) = merger$Row.names 
    merger$est_house_fore =   merger$Yfore * merger$Est_Acr        # estimate # houses in block group
    est_house_fore_county = aggregate(est_house_fore~COUNTY, data=merger, sum)      
    est_house_fore_county$COUNTY = as.numeric(as.character(est_house_fore_county$COUNTY))
    est_house_fore_county = merge(county_house, est_house_fore_county, by.x="FIPS.x", by.y="COUNTY")
    est_house_count <<- est_house_fore_county
    #print( paste(est_house_fore_county$est_house_fore/ est_house_fore_county$CumEst_2000, "%", sep=" " )  )           
    out =   sum(abs(est_house_fore_county$CumEst_2000 - est_house_fore_county$est_house_fore)) 
    #print(paste("off by =",out,sep=" "))
    return(out)
  }
  # run optimization of time dummy variable
  dum=nlminb( c(results$coefficients[length(results$coefficients)]),OPTIM, lower=-Inf, upper=Inf)
  OPTIM(dum$par)
  results$coefficients[length(results$coefficients)] = dum$par                # replace coeff
  print("Results from time dummy optimization NLM:")
  print(paste("Dummy Estimated as", round(dum$par,3), sep=" "))
  print(paste(  round(est_house_count$est_house_fore),"houses simulated of",round(est_house_count$CumEst_2000), "estimate","=",est_house_count$est_house_fore/est_house_count$CumEst_2000,sep=" "))
  if(est_house_count$est_house_fore/est_house_count$CumEst_2000 >10 | est_house_count$est_house_fore/est_house_count$CumEst_2000 < -10){ print("ERROR: ESTIMATE OUT OF BOUNDS STOPING ESTIMATION")
                                                                                                                                         print("##################################")}
  
  # REESTIMATE DENSITIES
  Yfore = data.frame(Yfore = A %*% Xs %*% results$coefficients + A %*% Prdmn) 
  Yfore[Yfore<0] = 0  
  # THIS SHOULD BE LIMITED SO DENSITIES DON'T DECLINE
  row.names(Yfore) = row.names(Xs)
  
  # rural amp controled for in optimization 
  if(Rural_Amp_factor!=1){
    print(paste("Rural Amplification Implemented for all block groups with density <",Rural_threshold,"houses/ha",sep=" "))
    print(paste("They will grow at a rate of",(Rural_Amp_factor-1),"for all prediction years",sep=" "))
    Yfore = rural_amp( Rural_threshold, Rural_Amp_factor,Yfore)} 
  
  # INSERT ESTIMATES INTO ORIGINAL DATA
  data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA     # NA out forecast data
  data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= Yfore[na.omit(match(row.names(data),row.names(Yfore))),"Yfore"]   # replace with forecast
  
  # RESTRICT SO NO DECLINING DENSITIES
  RESTICT <- function(data){
    maxyear = max(unfactor(data$time))
    tmaxyear=((data$HuDac4[data$time == maxyear] < data$HuDac4[data$time == (maxyear-10)])) # find dates where prediction is lower than previous value
    tmaxyear= tmaxyear[tmaxyear==T]            # get only true values 
    if(sum(tmaxyear)!=0){                      # only restrict for samples that actually have some declining values
      prev_v_EVHudac4 = lag(data[,c("EVHudac4")])          # isolate previous value for replacement
      prev_v_HuDac4 = lag(data[,c("HuDac4")])*depreciation   # *0.98 assumes 2% housing loss in stagnant areas
      #########################################
      data[row.names(data) %in% names(tmaxyear),c("EVHudac4", "HuDac4")] = cbind(prev_v_EVHudac4[names(prev_v_EVHudac4) %in% names(tmaxyear)], prev_v_HuDac4[names(prev_v_HuDac4) %in% names(tmaxyear)])  #replace declining value with previous
    }
    return(data)  
  }
  data = RESTICT(data)
  
  county_scaler <- function(){
    # county scale uses "pecentage" to scale each county up or down. Where the objective is 
    # to minimize the abs difference between the expected housing count "CumEst_2000" and predicted "house_count"
    print("Optimizing county level estimates by * by % scaler to better match CumEst_2000 estimates")
    # initialize percentage @ 100%
    percent = data.frame( COUNTY = unique(data$COUNTY) ,  estper = rep(1, length(unique(data$COUNTY))) )
    
    county_scaler <- function(percent_county){  # objective function       
      county_Preds = data[data$COUNTY %in% county  & data$time %in% max(unfactor(data$time)),c("COUNTY","HuDac4", "Est_Acr")]   # use HuDac4 because it is better estimate                                    
      county_Preds$house_count = county_Preds$HuDac4 * county_Preds$Est_Acr * percent_county  # estimate # of predicted houses by block SCALED  by estper          
      county_Preds$COUNTY =  unfactor(county_Preds$COUNTY)                      # remove factor
      county_Preds = aggregate( house_count ~ COUNTY, data=county_Preds, sum)   # estimate # of predicted houses by County
      #print(county_Preds)
      county_Preds = merge(county_Preds, county_house, by.x="COUNTY", by.y="FIPS.x") 
      optim_target <<- abs(county_Preds$house_count - county_Preds$CumEst_2000)
      # print(paste("For county",county,"Percentage",percent_county,"is off by",optim_target,sep=" "))
    }
    
    # using optimize function b/c it is quicker and more accurate than optim routines
    for (i in 1:length(percent$COUNTY)){
      county = percent$COUNTY[i]                                                 # iterate through counties 
      optmized_percent = optimize(county_scaler,  c(0,5),tol=0.001)
      percent[percent$COUNTY %in% county,"estper"] = optmized_percent$minimum    # replace estimated %scaler with optimal 
      if( optmized_percent$minimum>4.5){print("**WARNING: SCALING FACTORS ARE NEARLY OUT OF RANGE**")}
    }
    print(paste("County",unfactor(county),"estimates is scaled by", round(optmized_percent$minimum,4)," and off by:",round(optim_target),"houses", sep=" "))
    
    # get adjusted estimates  
    data = merge(data, percent, by="COUNTY")                                # link scaler estimates
    data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] * data$estper     
    data = subset(data,select=-c(estper))
    data <<- pdata.frame(data,c("id","time"), drop.index=FALSE)   # somehow is getting converted to data.frame not pdataframe
  }  
  
  ### Adjust to match expected county housing counts
  if (county_scale==T){  county_scaler() }
  
  ##############################
  ### add more prediction years
  for (j in 1:(years-1)){    
    # trouble again with "row.name" maybe... 
    data =  addpredyear2(1, data)              # add one year to original data
    data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights
    
    fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
    print( c("year",max(unfactor(data$time))) )
    Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
    gethouseproj(urban)   # get population and housing forecasts  urban or rural
    
    # add coefficient for forecast time dummies
    results$coefficients=  c(results$coefficients,0) # add initial iterate later THIS IS PLACE HOLDER
    
    # run optimization of time dummy variable
    dum=nlminb( c(results$coefficients[length(results$coefficients)]),OPTIM, lower=-Inf, upper=Inf)
    OPTIM(dum$par)
    results$coefficients[length(results$coefficients)] = dum$par                # replace coeff
    print("Results from time dummy optimization NLM:")
    print(paste("Dummy Estimated as", round(dum$par,3), sep=" "))
    print(paste(  round(est_house_count$est_house_fore),"houses simulated of",round(est_house_count$CumEst_2000), "estimate","=",est_house_count$est_house_fore/est_house_count$CumEst_2000,sep=" "))
    if(est_house_count$est_house_fore/est_house_count$CumEst_2000 >10 | est_house_count$est_house_fore/est_house_count$CumEst_2000 < -10){ print("ERROR: ESTIMATE OUT OF BOUNDS STOPING ESTIMATION")
                                                                                                                                           print("##################################")}
    
    
    # reestimate values with new coefficient
    YforeN = data.frame(Yfore= A %*% Xs %*% results$coefficients  + A %*% Prdmn)
    YforeN[YforeN<0] = 0
    row.names(YforeN) = row.names(Xs)
    
    # rural amp controled for in optimization 
    if(Rural_Amp_factor!=1){YforeN = rural_amp( Rural_threshold, Rural_Amp_factor,YforeN)}
    
    data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA
    data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= YforeN[na.omit(match(row.names(data),row.names(YforeN))),"Yfore"]
    
    # RESTRICT SO NO DECLINING DENSITIES
    data = RESTICT(data)
    
    ### Adjust to match expected county housing counts
    if (county_scale==T){  county_scaler() } 
  }  
  
  print("All Coefficients are as follows:")
  print(results$coefficients)
  return(data)
}




effects.splmXX <- function (object, ...){
    x <- object
    if (class(x) != "splm") 
        stop(paste("methos not implemented for objects of class", 
            class(x)))
    if (class(x) != "splm" && (x$type != "fixed effects lag" || 
        x$type != "fixed effects error")) 
        stop(paste("methos not implemented for objects of type", 
            x$type))
    all.FE <- x$res.eff[[1]]
    all.FEXX <- x$res.eff[[1]]
    effects <- x$effects
    if (effects == "pooled") 
        stop("No fixed effects available if effects == pooled")
    if (effects == "spfe") {
        INT <- all.FE$intercept
        INTXX <- all.FE$interceptXX
        se.INT <- all.FE$res.se.con
        z <- INT/se.INT
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        INTTable <- cbind(INT, se.INT, z, p)
        INTTableXX <- INTXX
        colnames(INTTable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        rownames(INTTable) <- "(Intercept)"
        SP.EFF   <- all.FE$res.sfe
        SP.EFFXX <- all.FE$res.sfeXX
        
        se.SP.EFF <- all.FE$res.se.sfe
        z <- SP.EFF/se.SP.EFF
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        SETable <- cbind(SP.EFF, se.SP.EFF, z, p)
        SETableXX <- cbind(SP.EFFXX)
        
        colnames(SETable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        res <- list(INTTable = INTTable, INTTableXX=INTTableXX, SETable = SETable, 
                    SETableXX = SETableXX, effects = effects)
    }
    if (effects == "tpfe") {
        INT <- all.FE$intercept
        se.INT <- all.FE$res.se.con
        z <- INT/se.INT
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        INTTable <- cbind(INT, se.INT, z, p)
        colnames(INTTable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        rownames(INTTable) <- "(Intercept)"
        TP.EFF <- all.FE$res.tfe
        se.TP.EFF <- all.FE$res.se.tfe
        z <- TP.EFF/se.TP.EFF
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        TETable <- cbind(TP.EFF, se.TP.EFF, z, p)
        colnames(TETable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        res <- list(INTTable = INTTable, TETable = TETable, effects = effects)
    }
    if (effects == "sptpfe") {
        INT <- all.FE$intercept
        se.INT <- all.FE$res.se.con
        z <- INT/se.INT
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        INTTable <- cbind(INT, se.INT, z, p)
        colnames(INTTable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        rownames(INTTable) <- "(Intercept)"
        SP.EFF <- all.FE$res.sfe
        se.SP.EFF <- all.FE$res.se.sfe
        z <- SP.EFF/se.SP.EFF
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        SETable <- cbind(SP.EFF, se.SP.EFF, z, p)
        colnames(SETable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        TP.EFF <- all.FE$res.tfe
        se.TP.EFF <- all.FE$res.se.tfe
        z <- TP.EFF/se.TP.EFF
        p <- 2 * pnorm(abs(z), lower.tail = FALSE)
        TETable <- cbind(TP.EFF, se.TP.EFF, z, p)
        colnames(TETable) <- c("Estimate", "Std. Error", "t-value", 
            "Pr(>|t|)")
        res <- list(INTTable = INTTable, SETable = SETable, TETable = TETable, 
            effects = effects)
    }
    res
    class(res) <- "effects.splm"
    return(res)
}



###########################################
forecastTD <- function(data,polys,formula,results,constant, years, timedummy,calcA){ 
  EVHudac4 = data.frame(EVHudac4 = results$fitted.valuesXX)
  data = merge(data, EVHudac4, by="row.names")
  data = data[with(data, order(id,time) ), ]
  data =  addpredyear2(1, data)              # add one year to original data
  data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights

  fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
  Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
 
  N = length(unique(row.names(Xs)))
  I = diag(N)
  rho = results$arcoef
    ## Prepare data
    ## Distance weights - limit to areas included in regression using ider.SUM090
    data.wide = reshape(data[,c("id","time","SUM090")], idvar="id", timevar="time", direction="wide")
    ider = strsplit(row.names(Xs), split="-")
    ider = unique(as.vector(unlist(lapply(1:length(ider), function (x) ider[[x]][1]))))   # unique ids
    ider.SUM090 = data.wide$SUM090.49[data.wide$id %in% ider]
    data_ider = polys[polys$SUM090 %in% ider.SUM090,]    
    W = listw2mat(theneigh(data_ider))
  
    if (results$call$model == "pooling"){
      ## Mu (using mean resid as FE for now)
      ider2 = strsplit(row.names(data.frame(Resd=results$residualsXX)), split="-")
      ids2  = as.vector(as.numeric(unlist(lapply(1:length(ider2), function (x) ider2[[x]][1]))))   # unique ids
      names(ids2) = row.names(data.frame(Resd=results$residualsXX))
      XsR   = merge(data.frame(ids2=ids2), data.frame(Resd=results$residualsXX), by="row.names")
      Prdmn = aggregate( Resd ~ ids2, data = XsR, FUN="mean", na.rm=TRUE )   # GET MEAN ERROR (av can be used in prediction Baltagi '10')
      Prdmn = Prdmn[with(Prdmn, order(ids2) ), 2]
    }
  
 #   else{ if (results$call$effect == "individual"){
      ## MU (for SPFE)
 #     ider2 = strsplit(row.names(data.frame(Resd=SPFE$residualsXX)), split="-")
 #     ids2  = unique(as.vector(as.numeric(unlist(lapply(1:length(ider2), function (x) ider2[[x]][1])))))   # unique ids
 
 #     SPFEeffects = effects.splmXX(results)
 #     SPFEeffects$SETableXX
 #   }}
  

  
  if(calcA==T) { A <<- solve(I - rho*W)}
  
  # add coefficient for forecast time dummies
  results$coefficients= c(results$coefficients,0.0)

  #results$coefficients= c(results$coefficients,results$coefficients["time99"])   # fix time intercept
 # time109 = 0.0
 # names(time109) = "time109"
   
  # coefficients = data.frame(t(data.frame(c(results$coefficients,time109) )))
  # row.names(coefficients) = NULL
  # coefficients = subset(coefficients,select=c(sort(names(coefficients)))) 
  # coefficients[,sort(names(coefficients))]                          #resort to Xs order

  
  
  Yfore = data.frame(Yfore = A %*% Xs %*% results$coefficients + A %*% Prdmn)
  Yfore[Yfore<0] = 0
  row.names(Yfore) = row.names(Xs)

  data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA     # NA out forecast data
  data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= Yfore[na.omit(match(row.names(data),row.names(Yfore))),"Yfore"]   # replace with forecast
 
  
  ### add more prediction years
  for (j in 1:(years-1)){
    data =  addpredyear2(1, data)              # add one year to original data
    data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights
  
    fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
    c("year",max(unfactor(data$time)))
    Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
    
    # add coefficient for forecast time dummies
    #results$coefficients= c(results$coefficients,results$coefficients["time99"])
    results$coefficients= c(results$coefficients,0.0)
  
    YforeN = data.frame(Yfore= A %*% Xs %*% results$coefficients  + A %*% Prdmn)
    YforeN[YforeN<0] = 0
    row.names(YforeN) = row.names(Xs)
    data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA
    data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= YforeN[na.omit(match(row.names(data),row.names(YforeN))),"Yfore"]
  }  
  
return(data)
}


###########################################
forecastFE <- function(data,polys,formula,results,constant, years, timedummy){ 
  EVHudac4 = data.frame(EVHudac4 = results$fitted.valuesXX)
  data = merge(data, EVHudac4, by="row.names")
  data = data[with(data, order(id,time) ), ]
  data =  addpredyear2(1, data)              # add one year to original data
  data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights

  fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
  Xs  = model.matrix.lm(formula,fore.data, na.action=NULL)
 
  N = length(unique(row.names(Xs)))
  I = diag(N)
  rho = results$arcoef
    ## Prepare data
    ## Distance weights - limit to areas included in regression using ider.SUM090
    data.wide = reshape(data[,c("id","time","SUM090")], idvar="id", timevar="time", direction="wide")
    ider = strsplit(row.names(Xs), split="-")
    ider = unique(as.vector(unlist(lapply(1:length(ider), function (x) ider[[x]][1]))))   # unique ids
    ider.SUM090 = data.wide$SUM090.49[data.wide$id %in% ider]
    data_ider = polys[polys$SUM090 %in% ider.SUM090,]    
    W = listw2mat(theneigh(data_ider))
  
    if (results$call$model == "pooling"){
      ## Mu (using mean resid as FE for now)
      ider2 = strsplit(row.names(data.frame(Resd=results$residualsXX)), split="-")
      ids2  = as.vector(as.numeric(unlist(lapply(1:length(ider2), function (x) ider2[[x]][1]))))   # unique ids
      names(ids2) = row.names(data.frame(Resd=results$residualsXX))
      XsR   = merge(data.frame(ids2=ids2), data.frame(Resd=results$residualsXX), by="row.names")
      Prdmn = aggregate( Resd ~ ids2, data = XsR, FUN="mean", na.rm=TRUE )   # GET MEAN ERROR (av can be used in prediction Baltagi '10')
      Prdmn = Prdmn[with(Prdmn, order(ids2) ), 2]
    }
  
    else{ if (results$call$effect == "individual"){
      ## MU (for SPFE)
      ider2 = strsplit(row.names(data.frame(Resd=SPFE$residualsXX)), split="-")
      ids2  = unique(as.vector(as.numeric(unlist(lapply(1:length(ider2), function (x) ider2[[x]][1])))))   # unique ids
 
      SPFEeffects = effects.splmXX(results)
      SPFEeffects$SETableXX
    }}
  
  A = solve(I - rho*W)
  Yfore = data.frame(Yfore = A %*% Xs %*% results$coefficients + A %*% Prdmn)
  row.names(Yfore) = row.names(Xs)

  data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA
  data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= Yfore[na.omit(match(row.names(data),row.names(Yfore))),"Yfore"]
 
  ### add more years
  for (j in 1:(years-1)){
    data =  addpredyear2(1, data)              # add one year to original data
    data = data[with(data, order(ordersp)), ]  # put in same order as spatial weights
  
    fore.data = data[data$time %in% max(unfactor(data$time))]      # isolate prediction year
    Xs  = model.frame(formula,fore.data, na.action=NULL)
    Xs  = Xs[,-1]                             # get rid of Y
    if (constant == T) {Xs= data.frame(Intercept=1, Xs)}
    Xs = as.matrix(Xs)  
    YforeN = data.frame(Yfore= A %*% Xs %*% results$coefficients + A %*% Prdmn)
    row.names(YforeN) = row.names(Xs)
    data[data$time %in% max(unfactor(data$time)),c("EVHudac4", "HuDac4")] = NA
    data[is.na(data$HuDac4),c("EVHudac4","HuDac4") ]= YforeN[na.omit(match(row.names(data),row.names(YforeN))),"Yfore"]
  }  
  
return(data)
}



#######################################################
unfactor <- function(factors){return(as.numeric(as.character(factors)))}




#######################################################
addpredyear3 = function(addyears,data){
  print("adding prediction year")
  # NOTE: DATA EXPANDED TIME VARIENT AND TIME INVARIENT KEPT AT PREVIOUS VALUE
  
  if (addyears==0){return(NULL)} 
  # time invariant variables stored in "tvarient" above 
  # add the additional observations 'addyears' then merge orginal data
  maxyear = max(as.numeric(as.character(data$time)))                         # save for later
  
  for (i in 1:addyears){   
    time = rep(c(unique(as.numeric(as.character(data$time))),max(as.numeric(as.character(data$time)))+10), length(unique(data$id)))
    id   = rep(unique(data$id), each=max(length(unique(time))))
    data2 = data.frame(time, id)
    data = merge(data2, data, all.x=T)
    data = data[with(data, order(id,time) ), ]
    row.names(data) = paste(data$id,data$time,sep=".")
    remove(data2)
  }
  
  # convert data from long to wide in order to replace missing values from time invariant variables
  set.seed(10)
  samples  = sample(data$id,min(200,length(data$id)), replace=T)                                               # SAMPLE B/C PVAR TAKE A LONG TIME
  tvarient = pvar(na.omit(data[data$id %in% samples,]))$id.variation    # get time variant again
  data = pdata.frame(data, c("id","time"), drop.index=FALSE)            # declare panel data individ and time
  
  # convert to wide
  data5 = reshape(data, dir = "wide", timevar="time", idvar="id")                                    # convert to wide to make replacing NAs easier
  
  for (j in seq(maxyear,(maxyear+addyears*10),by=10)  ){
    # FILL YEARS WITH NO VALUES WITH T-1 
    data5[,grep(j+10,names(data5))] = data5[,grep(j,names(data5))]    # replace 99+ Nas with previous year
  }
  
  data5 = reshape(data5, dir = "long", sep=".")                                    # convert back to long
  data5 = pdata.frame(as.data.frame(data5), c("id","time"), drop.index=FALSE)      # declare panel data DOESN'T WORK WITHOUT
  
  h =  strsplit(names(data5), split="\\.")
  for(g in 1:length(h)){
    if (h[[g]][1] == "Row") {h[[g]] = c("Row.names", "39")  }                  # avoid problem with string split of Row.names on "."                                       # replace b/c Row.names is split to "Row" "names"  
  }
  
  names(data5) = apply(as.data.frame(h)[1,], 2, function(x) as.character(x))   # remove .39 from names
  data5 = data5[with(data5, order(id,time) ), ]                                # resort
  
  # recalculate lags and squares 
  data5 = add_panel_variables(data5)                                           # replace lags etc
  
  # panel.data5$Row.names = paste(panel.data5$id, panel.data5$time, sep="-")
  data5$HuDac4[data5$time == max(unfactor(data5$time))]  = NA                  # make sure that this is NA b/c prediction function looks for missing values to fill 
  data5$EVHudac4[data5$time == max(unfactor(data5$time))]  = NA                  # make sure that this is NA b/c prediction function looks for missing values to fill 
  
  return( data5 )    # assign to global env. 
}



#######################################################
map.it <- function(rresults){
  merger= merge(data.frame(E=rresults$residualsXX), data.frame(FV=rresults$fitted.valuesXX), by="row.names") 
  row.names(merger) = merger$Row.names
  panel.data2XXpl = merge(panel.data2XX, merger, by="row.names",  incomparables=NA)
  Emn = aggregate( E ~ SUM090, data = panel.data2XXpl, FUN="mean", na.rm=TRUE )                       # OR GET AVERAGE (av can be used in prediction Baltagi '10')
  Block.39.dbf_spCE = merge(Block.39.dbf_spC, Emn, by="SUM090", all.x=T,  incomparables=NA)   #BRING PREDICTIONS TO POLYGON DBF
  Block.39.dbf_spCE$INTPTLAT = as.numeric(as.character(Block.39.dbf_spCE$INTPTLAT))                #CONVERT FACTOR->NUMERIC
  Block.39.dbf_spCE$INTPTLON = as.numeric(as.character(Block.39.dbf_spCE$INTPTLON))
  acord = coordinates(Block.39_spC)
  points= SpatialPointsDataFrame(coords=acord, data=as.data.frame(Block.39.dbf_spCE))
  class <- classIntervals(points$E, n=8, style = "pretty") #normally sd
  plot.new()                             #make plots
  trellis.par.set(sp.theme())
  p1 = spplot(points,"E", cuts=class$brks ,scales=list(draw=T))
  print(p1, position =c(0,.2,1,1),more=T)
  par(new=T)
  par(plt = c(0.1,1,0.1,0.45))
  hist(points$E, breaks=160, main=NULL, xlab="", ylab="")
  remove(merger, panel.data2XXpl, Emn,Block.39.dbf_spCE,acord)
}

#######################################################
#######################################################


spmlXX <- function (formula, data, dataXX, index = NULL, listw,  listw2 = listw, 
    model = c("within", "random", "pooling"), effect = c("individual", 
        "time", "twoways"), lag = FALSE, spatial.error = c("b", 
        "kkp", "none"),    ...) #listwXX,
{
  
    if (!lag) stop(" spmlxx is only used for lagged Y at the moment sorry")
    if (spatial.error!="none" | is.null(spatial.error) ) stop(" spmlxx can't be used for lagged error at the moment sorry")

    cl <- match.call()
    checklw <- function(x) {
        if (!("listw" %in% class(x))) {
            if ("matrix" %in% class(x)) {
                require(spdep)
                x <- listw2mat(x)
            }
            else {
                stop("'listw' has to be either a 'listw' or a 'matrix' object")
            }
        }
        return(x)
    }
    checklw(listw)
    checklw(listw2)
    #checklw(listwXX)   created later
    switch(match.arg(model), within = {
        if (lag) {
            model <- switch(match.arg(spatial.error), b = "sarar", 
                kkp = "sarar", none = "lag")
        } else {
            model <- "error"
        }
        effects <- switch(match.arg(effect), individual = "spfe", 
            time = "tpfe", twoways = "sptpfe")
        res <- spfemlXX(formula = formula, data = data, dataXX = dataXX, index = index, 
            listw = listw, listw2 = listw2,  model = model, effects = effects, # listwXX = listwXX,
            cl = cl, ...)
    }, random = {
        switch(match.arg(effect), time = {
            stop("time random effects not implemented")
        }, twoways = {
            stop("twoway random effects not implemented")
        }, individual = {
            errors <- switch(match.arg(spatial.error), b = "semre", 
                kkp = "sem2re", none = "re")
        })
        res <- spreml(formula = formula, data = data, index = index, 
            w = listw2mat(listw), w2 = listw2mat(listw2), lag = lag, 
            errors = errors, cl = cl, ...)
    }, pooling = {
        errors <- switch(match.arg(spatial.error), b = "sem", 
            kkp = "sem", none = "ols")
        res <- spremlXX(formula = formula, data = data, dataXX = dataXX, index = index, 
            w = listw2mat(listw), w2 = listw2mat(listw2), lag = lag, 
            errors = errors, cl = cl, effects="pooling", ...)
    })
    return(res)
}

################################################################
### USED FOR POOLED LAG=T effect
spremlXX <- function (formula, data,dataXX, index = NULL, w, w2 = w, lag = FALSE, 
    errors = c("semsrre", "semsr", "srre", "semre", "re", "sr", 
        "sem", "ols", "sem2re"), pvar = FALSE, hess = FALSE, 
    quiet = TRUE, initval = c("zeros", "estimate"), x.tol = 1.5e-18, 
    rel.tol = 1e-15, cl = NULL, ...) 
{    
    trace <- as.numeric(!quiet)
    if (pvar) 
        print("<implement pvar>")
    if (!is.null(index)) {
        require(plm)
        data <- plm.data(data, index)
        dataXX <- plm.data(dataXX, index)  
    }
    index <- data[, 1]
    tindex <- data[, 2]
    indexXX <- dataXX[, 1]
    tindexXX <- dataXX[, 2]
    if (is.null(cl)) 
        cl <- match.call()
    require(nlme)
    if (!is.matrix(w)) {
        if ("listw" %in% class(w)) {
            require(spdep)
            w <- listw2mat(w)
        }
        else {
            stop("w has to be either a 'matrix' or a 'listw' object")
        }
    }
    if (dim(data)[[1]] != length(index)) 
        stop("Non conformable arguments")
    if (dim(dataXX)[[1]] != length(indexXX)) 
        stop("Non conformable arguments")
    head(model.frame(formula,data)  )
    
    X <- model.matrix(formula, data =  data )
    y <- model.response(model.frame(formula, data = data))
    xXX <- model.matrix(formula, data = dataXX)
    yXX <- model.response(model.frame(formula, data = dataXX))
    names(index) <- row.names(data)
    names(indexXX) <- row.names(dataXX)

    

    #### create listwXX for block data excluding na's
    datawideXX = reshape(dataXX, idvar="id", timevar="time", direction="wide")
    ider = strsplit(row.names(xXX), split="-")
    ider = unique(as.vector(unlist(lapply(1:length(ider), function (x) ider[[x]][1]))))   # unique ids
    ider.SUM090 = datawideXX$SUM090.49[datawideXX$id %in% ider]

    Block.39_sp_ider = Block.39.ex[Block.39.ex$SUM090 %in% ider.SUM090,]    
    listwXX = theneigh(Block.39_sp_ider)
    wXX <- listw2mat(listwXX )

    ind <- index[which(names(index) %in% row.names(X))]
    indXX <- indexXX[which(names(indexXX) %in% row.names(xXX))]    #helps remove NAs
    tind <- tindex[which(names(index) %in% row.names(X))]
    tindXX <- tindexXX[which(names(indexXX) %in% row.names(xXX))]
    oo <- order(tind, ind)
    ooXX <- order(tindXX, indXX)
    X <- X[oo, ]
    xXX <- xXX[ooXX, ]
    y <- y[oo]
    yXX <- yXX[ooXX]
    
    ind <- ind[oo]
    indXX <- indXX[ooXX]
    tind <- tind[oo]
    tindXX <- tindXX[ooXX]
    n <- length(unique(ind))
    nXX <- length(unique(indXX))
    k <- dim(X)[[2]]
    kXX <- dim(xXX)[[2]]
    t <-   max(tapply(X[, 1], ind, length))
    tXX <- max(tapply(xXX[, 1], indXX, length), na.rm=T)      #, na.rm=Tassumes its balanced should be fine   
    nT <- length(ind)
    nTXX <- length(indXX)
    if (dim(w)[[1]] != n) 
        stop("Non conformable spatial weights")
    if (dim(wXX)[[1]] != nXX) 
        stop("Non conformable spatial weights XX")

    balanced <- n * t == nT
    if (!balanced) 
        stop("Estimation method unavailable for unbalanced panels")
    balanced <- nXX * tXX == nTXX
    if (!balanced) 
        stop("Estimation method unavailable for unbalanced panels XX")

    sv.length <- switch(match.arg(errors), semsrre = 3, semsr = 2, 
        srre = 2, semre = 2, re = 1, sr = 1, sem = 1, ols = 0, 
        sem2re = 2)
    errors. <- match.arg(errors)
    if (is.numeric(initval)) {
        if (length(initval) != sv.length) {
            stop("Incorrect number of initial values supplied for error vcov parms")
        }
        coef0 <- initval
    }
    else {
        switch(match.arg(initval), zeros = {
            coef0 <- rep(0, sv.length)
        }, estimate = {
            if (nchar(errors.) < 4) {
                stop("Pre-estimation of unique vcov parm is meaningless: \n please select (default) option 'zeros' or supply a scalar")
            }
            coef0 <- NULL
            if (grepl("re", errors.)) {
                REmodel <- REmod(X, y, ind, tind, n, k, t, nT, 
                  w, coef0 = 0, hess = FALSE, trace = trace, 
                  x.tol = 1.5e-18, rel.tol = 1e-15, ...)
                coef0 <- c(coef0, REmodel$errcomp)
            }
            if (grepl("sr", errors.)) {
                ARmodel <- ssrmod(X, y, ind, tind, n, k, t, nT, 
                  w, coef0 = 0, hess = FALSE, trace = trace, 
                  x.tol = 1.5e-18, rel.tol = 1e-15, ...)
                coef0 <- c(coef0, ARmodel$errcomp)
            }
            if (grepl("sem", errors.)) {
                SEMmodel <- semmod(X, y, ind, tind, n, k, t, 
                  nT, w, coef0 = 0, hess = FALSE, trace = trace, 
                  x.tol = 1.5e-18, rel.tol = 1e-15, ...)
                coef0 <- c(coef0, SEMmodel$errcomp)
            }
        })
    }
    if (lag) {
  # i think this switchs  RES<-est.fun to RES<-sarmod etc
        est.fun <- switch(match.arg(errors), semsrre = {
            saremsrREmod
        }, semsr = {
            saremsrmod
        }, srre = {
            sarsrREmod
        }, semre = {
            saremREmod
        }, re = {
            sarREmod
        }, sr = {
            sarsrmod
        }, sem = {
            saremmod
	    # errors = "none" switched to ols
        }, ols = {
            sarmodXX
        }, sem2re = {
            sarem2REmod
        })
        coef0 <- c(coef0, 0)
    }
    else {
        est.fun <- switch(match.arg(errors), semsrre = {
            semsrREmod
        }, semsr = {
            semsrmod
        }, srre = {
            ssrREmod
        }, semre = {
            semREmod
        }, re = {
            REmod
        }, sr = {
            ssrmod
        }, sem = {
            semmod
        }, ols = {
            olsmod
        }, sem2re = {
            sem2REmod
        })
        arcoef <- NULL
    }
   
    RES <- est.fun(X, y, ind, tind, n, k, t, nT, w = w, w2 = w2, 
        coef0 = coef0, hess = hess, trace = trace, x.tol = x.tol, 
        rel.tol = rel.tol)
     
    y.hat <- as.vector(X %*% RES$betas)
    y.hatXX <- as.vector(xXX %*% RES$betas)
    res <- y - y.hat
    resXX <- yXX - y.hatXX
    nam.rows <- dimnames(X)[[1]]
    names(y.hat) <- nam.rows
    names(res) <- nam.rows
    nam.rowsXX <- dimnames(xXX)[[1]]
    names(y.hatXX) <- nam.rowsXX
    names(resXX) <- nam.rowsXX

    model.data <- data.frame(cbind(y, X[, -1]))
    model.dataXX <- data.frame(cbind(yXX, xXX[, -1]))
    dimnames(model.data)[[1]] <- nam.rows
    dimnames(model.dataXX)[[1]] <- nam.rowsXX
    type <- "random effects ML"
    sigma2 <- list(one = 3, idios = 2, id = 1)
    spmod <- list(coefficients = RES$betas, arcoef = RES$arcoef, 
        errcomp = RES$errcomp, vcov = RES$covB, vcov.arcoef = RES$covAR, 
        vcov.errcomp = RES$covPRL, residuals = res,  residualsXX = resXX,
	      fitted.values = y.hat, fitted.valuesXX = y.hatXX, 
        sigma2 = sigma2, model = model.data, type = type, call = cl, 
        errors = errors, logLik = RES$ll)
    class(spmod) <- "splm"
    return(spmod)
}






sarmodXX <- function (X, y, ind, tind, n, k, t, nT, w, w2, coef0 = 0, hess = FALSE, 
    trace = trace, x.tol = 1.5e-18, rel.tol = 1e-15, ...) 
{
    nam.beta <- dimnames(X)[[2]]
    nam.errcomp <- c("lambda")
    myparms0 <- coef0
    lower.bounds <- c(-0.999)
    upper.bounds <- c(0.999)
    B <- function(lambda, w) diag(1, ncol(w)) - lambda * w
    detB <- function(lambda, w) det(B(lambda, w))
    ll.c <- function(phipsi, y, X, n, t, w, w2, wy) {
        psi <- phipsi
        Ay <- y - psi * wy
        glsres <- OLSstep(X, Ay)
        e <- glsres[["ehat"]]
        s2e <- glsres[["sigma2"]]
        zero <- t * log(detB(psi, w2))
        due <- 0
        tre <- -n * t/2 * log(s2e)
        quattro <- -1/(2 * s2e) * crossprod(e)
        const <- -(n * t)/2 * log(2 * pi)
        ll.c <- const + zero + due + tre + quattro
        llc <- -ll.c
    }
    Wy <- function(y, w, tind) {
        wy <- list()
        for (j in 1:length(unique(tind))) {
            yT <- y[tind == unique(tind)[j]]
            wy[[j]] <- w %*% yT
        }
        return(unlist(wy))
    }
    OLSstep <- function(X, y) {
        b.hat <- solve(crossprod(X), crossprod(X, y))
        ehat <- y - X %*% b.hat
        sigma2ehat <- crossprod(ehat)/(n * t)
        return(list(betahat = b.hat, ehat = ehat, sigma2 = sigma2ehat))
    }
    wy <- Wy(y, w2, tind)
    optimum <- nlminb(start = myparms0, objective = ll.c, gradient = NULL, 
        hessian = NULL, y = y, X = X, n = n, t = t, w = w, w2 = w2, 
        wy = wy, scale = 1, control = list(x.tol = x.tol, rel.tol = rel.tol, 
            trace = trace), lower = lower.bounds, upper = upper.bounds)
    myll <- -optimum$objective
    myparms <- optimum$par
    Ay <- y - myparms[length(myparms)] * wy
    beta <- OLSstep(X, Ay)
    covB <- as.numeric(beta[[3]]) * solve(crossprod(X))
    covTheta <- solve(-fdHess(myparms, function(x) -ll.c(x, y, 
        X, n, t, w, w2, wy))$Hessian)
    nvcovpms <- length(nam.errcomp) - 1
    covAR <- covTheta[nvcovpms + 1, nvcovpms + 1, drop = FALSE]
    covPRL <- NULL
    betas <- as.vector(beta[[1]])
    arcoef <- myparms[which(nam.errcomp == "lambda")]
    errcomp <- NULL
    names(betas) <- nam.beta
    names(arcoef) <- "lambda"
    dimnames(covB) <- list(nam.beta, nam.beta)
    dimnames(covAR) <- list(names(arcoef), names(arcoef))
    RES <- list(betas = betas, arcoef = arcoef, errcomp = errcomp, 
        covB = covB, covAR = covAR, covPRL = covPRL, ll = myll)
    return(RES)
}
 




## sent lag=T pooled 
sarmodXX_ORGINAL <- function (X,xXX, y,yXX, ind, tind, tindXX, n, k, t, nT, w, w2, coef0 = 0, hess = FALSE, 
    trace = trace, x.tol = 1.5e-18, rel.tol = 1e-15, ...) 
{
    nam.beta <- dimnames(X)[[2]]
    nam.betaXX <- dimnames(xXX)[[2]]
    nam.errcomp <- c("lambda")
    myparms0 <- coef0
    lower.bounds <- c(-0.999)
    upper.bounds <- c(0.999)
    B <- function(lambda, w) diag(1, ncol(w)) - lambda * w
    
    detB <- function(lambda, w) det(B(lambda, w))
    ll.c <- function(phipsi, y, X, n, t, w, w2, wy) {
        psi <- phipsi
        Ay <- y - psi * wy
        glsres <- OLSstep(X, Ay)
        e <- glsres[["ehat"]]
        s2e <- glsres[["sigma2"]]
        zero <- t * log(detB(psi, w2))
        due <- 0
        tre <- -n * t/2 * log(s2e)
        quattro <- -1/(2 * s2e) * crossprod(e)
        const <- -(n * t)/2 * log(2 * pi)
        ll.c <- const + zero + due + tre + quattro
        llc <- -ll.c
    }
    Wy <- function(y, w, tind) {
        wy <- list()
        for (j in 1:length(unique(tind))) {
            yT <- y[tind == unique(tind)[j]]
            wy[[j]] <- w %*% yT       #get mean neighbor?
        }
        return(unlist(wy))
    }
   # WyXX <- function(yXX, wXX, tindXX) {
   #     wyXX <- list()
   #     for (j in 1:length(unique(tindXX))) {
   #         yTXX <- yXX[tindXX == unique(tindXX)[j]]
   #         wyXX[[j]] <- wXX %*% yTXX
   #     }
   #     return(unlist(wyXX))
   # }
    
    OLSstep <- function(X, y) {
        b.hat <- solve(crossprod(X), crossprod(X, y))
        ehat <- y - X %*% b.hat
        sigma2ehat <- crossprod(ehat)/(n * t)
        return(list(betahat = b.hat, ehat = ehat, sigma2 = sigma2ehat))
    }
    wy <- Wy(y, w2, tind)
    #wyXX <- WyXX(yXX, w2XX, tindXX)
    optimum <- nlminb(start = myparms0, objective = ll.c, gradient = NULL, 
        hessian = NULL, y = y, X = X, n = n, t = t, w = w, w2 = w2, 
        wy = wy, scale = 1, control = list(x.tol = x.tol, rel.tol = rel.tol, 
            trace = trace), lower = lower.bounds, upper = upper.bounds)
    myll <- -optimum$objective        
     
    myparms <<- optimum$par                    # optimized rho
    Ay <<- y - myparms[length(myparms)] * wy   # Y - (rho*Wy)  remove rho effect
    beta <- OLSstep(X, Ay)                     # Y - (rho*Wy) = Bx  non-spatial effect
    covB <- as.numeric(beta[[3]]) * solve(crossprod(X))
    covTheta <- solve(-fdHess(myparms, function(x) -ll.c(x, y, 
        X, n, t, w, w2, wy))$Hessian)
    nvcovpms <- length(nam.errcomp) - 1
    covAR <- covTheta[nvcovpms + 1, nvcovpms + 1, drop = FALSE]
    covPRL <- NULL
    betas <- as.vector(beta[[1]])
    arcoef <- myparms[which(nam.errcomp == "lambda")]
    errcomp <- NULL
    names(betas) <- nam.beta
    names(arcoef) <- "lambda"
    #export some
    betas <<- betas
    arcoef <<- arcoef
    y <<- y 
    wy <<- wy 
    Wy <<- Wy 
    #wyXX <<- wyXX
    
    dimnames(covB) <- list(nam.beta, nam.beta)
    dimnames(covAR) <- list(names(arcoef), names(arcoef))	  
    RES <- list(betas = betas, arcoef = arcoef, errcomp = errcomp, 
        covB = covB, covAR = covAR, covPRL = covPRL, ll = myll)
    return(RES) # returns to spreml
}









###############################################################
## call for TPFE and SPFE lag=T
spfemlXX <- function (formula, data = list(), index = NULL, listw, listw2 = NULL, 
    model = c("lag", "error", "sarar"), effects = c("pooled", 
        "spfe", "tpfe", "sptpfe"), method = "eigen", na.action = na.fail, 
    quiet = TRUE, zero.policy = NULL, interval = NULL, tol.solve = 1e-10, 
    control = list(), legacy = FALSE, llprof = NULL, cl = NULL, dataXX=list(), #listwXX,
    ...) 
{
    timings <- list()
    .ptime_start <- proc.time()
    model <- match.arg(model)
    if (model == "sarar") 
        con <- list(tol.opt = .Machine$double.eps^0.5, fdHess = NULL, 
            optimHess = FALSE, LAPACK = FALSE, compiled_sse = FALSE, 
            Imult = 2, cheb_q = 5, MC_p = 16, MC_m = 30, super = FALSE, 
            npars = 4L, lower = c(-1, -1) + .Machine$double.eps^0.5, 
            upper = c(1, 1) - .Machine$double.eps^0.5)
    else con <- list(tol.opt = .Machine$double.eps^0.5, fdHess = NULL, 
        optimHess = FALSE, compiled_sse = FALSE, Imult = 2, cheb_q = 5, 
        MC_p = 16, MC_m = 30, super = FALSE)
    nmsC <- names(con)
    con[(namc <- names(control))] <- control
    if (length(noNms <- namc[!namc %in% nmsC])) 
        warning("unknown names in control: ", paste(noNms, collapse = ", "))
    if (is.null(quiet)) 
        quiet <- !get("verbose", env = spdep:::.spdepOptions)
    stopifnot(is.logical(quiet))
    if (is.null(zero.policy)) 
        zero.policy <- get.ZeroPolicyOption()
    stopifnot(is.logical(zero.policy))
    if (!is.null(index)) {
        require(plm)
        data <- plm.data(data, index)
        dataXX <- plm.data(dataXX, index)  
    }
    
    index <- data[, 1]
    tindex <- data[, 2]
    indexXX <- dataXX[, 1]
    tindexXX <- dataXX[, 2]
    if (is.null(cl)) 
        cl <- match.call()
    effects <- match.arg(effects)
    if (dim(data)[[1]] != length(index)) 
        stop("Non conformable arguments")
    if (dim(dataXX)[[1]] != length(indexXX)) 
        stop("Non conformable arguments")
    x <- model.matrix(formula, data = data)
    y <- model.response(model.frame(formula, data = data))
    xXX <- model.matrix(formula, data = dataXX)
    yXX <- model.response(model.frame(formula, data = dataXX))
    names(index) <- row.names(data)
    names(indexXX) <- row.names(dataXX)
    
    #### create listwXX for block data excluding na's
    datawideXX = reshape(dataXX, idvar="id", timevar="time", direction="wide")
    ider = strsplit(row.names(xXX), split="-")
    ider = unique(as.vector(unlist(lapply(1:length(ider), function (x) ider[[x]][1]))))   # unique ids
    ider.SUM090 = datawideXX$SUM090.49[datawideXX$id %in% ider]

    Block.39_sp_ider = Block.39.ex[Block.39.ex$SUM090 %in% ider.SUM090,]    
    listwXX = theneigh(Block.39_sp_ider)
    
    ind   <- index[which(names(index) %in% row.names(x))]
    indXX <- indexXX[which(names(indexXX) %in% row.names(xXX))]    #helps remove NAs
    tind  <- tindex[which(names(index) %in% row.names(x))]
    tindXX <- tindexXX[which(names(indexXX) %in% row.names(xXX))]
    oo <- order(tind, ind)
    ooXX <- order(tindXX, indXX)
    x <- x[oo, ]
    xXX <- xXX[ooXX, ]
    y <- y[oo]
    yXX <- yXX[ooXX]
    ind <- ind[oo]
    indXX <- indXX[ooXX]
    tind <- tind[oo]
    tindXX <- tindXX[ooXX]
    clnames <- colnames(x)
    rwnames <- rownames(x)
    clnamesXX <- colnames(xXX)
    rwnamesXX <- rownames(xXX)
    if (effects != "pooled" && colnames(x)[1] == "(Intercept)") {
        x <- x[, -1]
    }
    if (effects != "pooled" && colnames(xXX)[1] == "(Intercept)") {
        xXX <- xXX[, -1]
    }
    if (is.vector(x)) {
        k <- 1
        x <- matrix(x)
        colnames(x) <- clnames[-1]
        dimnames(x)[[1]] <- rwnames
        dimnames(x)[[2]] <- clnames[-1]
    }
    else k <- dim(x)[[2]]
    
    if (is.vector(xXX)) {
        kXX <- 1
        xXX <- matrix(xXX)
        colnames(xXX) <- clnamesXX[-1]
        dimnames(xXX)[[1]] <- rwnamesXX
        dimnames(xXX)[[2]] <- clnamesXX[-1]
    }
    else kXX <- dim(xXX)[[2]]


    N <- length(unique(ind))
    NXX <- length(unique(indXX))
    n <- N
    nXX <- NXX	
    T <- max(tapply(x[, 1], ind, length))
    TXX <- max(tapply(xXX[, 1], indXX, length), na.rm=T)      #, na.rm=Tassumes its balanced should be fine   
 
    NT <- length(ind)
    NTXX <- length(indXX)
    if (is.matrix(listw)) {
        if (dim(listw)[[1]] != N) 
            stop("Non conformable spatial weights")
        require(spdep)
        listw <- mat2listw(listw)
        if (dim(listwXX)[[1]] != NXX) 
            stop("Non conformable spatial weights in listwXX")
        listwXX <- mat2listw(listwXX)
    }
    if (!inherits(listw, "listw")) 
        stop("No neighbourhood list")
    if (model == "sarar") {
        if (is.null(listw2)) 
            stop("No sarar sorry")
        else if (!inherits(listw2, "listw")) 
            stop("No 2nd neighbourhood list")
    }
    if (is.null(con$fdHess)) 
        con$fdHess <- method != "eigen"
    stopifnot(is.logical(con$fdHess))
    can.sim <- FALSE
    if (listw$style %in% c("W", "S")) 
        can.sim <- spdep:::can.be.simmed(listw)
    if (listwXX$style %in% c("W", "S")) 
        can.simXX <- spdep:::can.be.simmed(listwXX)
    if (model == "sarar") {
        if (is.null(listw2)) 
            listw2 <- listw
        else if (!inherits(listw2, "listw")) 
            stop("No 2nd neighbourhood list")
        if (listw2$style %in% c("W", "S")) 
            can.sim2 <- spdep:::can.be.simmed(listw2)
    }
    if (model == "sarar") {
        stopifnot(is.numeric(con$lower))
        stopifnot(length(con$lower) == 2)
        if (!is.null(con$pars)) {
            stopifnot(is.numeric(con$pars))
            stopifnot(length(con$pars) == length(con$lower))
        }
        stopifnot(is.integer(con$npars))
        stopifnot(is.numeric(con$upper))
        stopifnot(length(con$upper) == length(con$lower))
        stopifnot(is.logical(con$fdHess))
        stopifnot(is.logical(con$optimHess))
        stopifnot(is.logical(con$LAPACK))
        stopifnot(is.logical(con$super))
    }
    switch(model, lag = if (!quiet) cat("\n Spatial Lag Fixed Effects Model \n"), 
        error = if (!quiet) cat("\n Spatial Error Fixed Effects Model\n"), 
        sarar = if (!quiet) cat("\n Spatial SARAR Fixed Effects Model\n"), 
        stop("\nUnknown model type\n"))
    balanced <- N * T == NT	 
    if (!balanced) 
        stop("Estimation method unavailable for unbalanced panels")
    balancedXX <- NXX * TXX == NTXX
    if (!balancedXX) 
        stop("Estimation method unavailable for unbalanced panels XX" )  #hope shouldn't matter

######### not XXed
    mt <- terms(formula, data = data)
    mf <- lm(formula, data, method = "model.frame")
    na.act <- attr(mf, "na.action")

########
    indic <- seq(1, T)
    indicXX <- seq(1, TXX)
    inde <- as.numeric(rep(indic, each = N))
    indeXX <- as.numeric(rep(indicXX, each = NXX))
 
    indic1 <- seq(1, N)
    inde1 <- rep(indic1, T)
    indic1XX <- seq(1, NXX)
    inde1XX <- rep(indic1XX, T)
    
    env <- new.env(parent = globalenv())
    assign("y", y, envir = env)
    assign("yXX", yXX, envir = env)
    assign("x", x, envir = env)
    assign("xXX", xXX, envir = env)
    assign("listw", listw, envir = env)
    assign("listwXX", listwXX, envir = env)
    assign("NT", NT, envir = env)
    assign("NTXX", NTXX, envir = env)
    assign("T", T, envir = env)
    assign("TXX", TXX, envir = env)
    assign("k", k, envir = env)
    assign("kXX", kXX, envir = env)
    assign("n", n, envir = env)
    assign("nXX", nXX, envir = env)

    wy <- unlist(tapply(y, inde, function(u) lag.listw(listw, 
        u), simplify = TRUE))
    wyXX <- unlist(tapply(yXX, indeXX, function(u) lag.listw(listwXX,   
        u), simplify = TRUE))   
    if (effects == "tpfe" | effects == "sptpfe") {
        ytms <- tapply(y, inde, mean)
        ytmsXX <- tapply(yXX, indeXX, mean)
        tpms <- function(q) tapply(q, inde, mean)
        tpmsXX <- function(qXX) tapply(qXX, indeXX, mean)
        xtms <- apply(x, 2, tpms)
        xtmsXX <- apply(xXX, 2, tpmsXX)
        ytm <- rep(ytms, each = N)
        xtm <- matrix(, NT, k)
        ytmXX <- rep(ytmsXX, each = NXX)
        xtmXX <- matrix(, NTXX, kXX)

        for (i in 1:k) xtm[, i] <- rep(xtms[, i], each = N)
        for (i in 1:k) xtmXX[, i] <- rep(xtmsXX[, i], each = NXX)
        if (model %in% c("lag", "sarar")) {
            wytms <- tapply(wy, inde, mean)
            wytmsXX <- tapply(wyXX, indeXX, mean)
            wytm <- rep(wytms, each = N)
            assign("wytms", wytms, envir = env)
            assign("wytmsXX", wytmsXX, envir = env)
        }
        assign("ytms", ytms, envir = env)
        assign("xtms", xtms, envir = env)
        assign("ytmsXX", ytmsXX, envir = env)
        assign("xtmsXX", xtmsXX, envir = env)
    }
    ###  SPATIAL FE   #########WORKING ON THIS
    if (effects == "spfe" | effects == "sptpfe") {
        ysms <- tapply(y, inde1, mean)                   # demean variables
        ysmsXX <- tapply(yXX, inde1XX, mean)              # demean variables

        spms <- function(q) tapply(q, inde1, mean)
        spmsXX <- function(q) tapply(q, inde1XX, mean)
        xsms <- apply(x, 2, spms)
        xsmsXX <- apply(xXX, 2, spmsXX)

        ysm <- rep(ysms, T)
        xsm <- matrix(, NT, k)
        ysmXX <- rep(ysmsXX, T)
        xsmXX <- matrix(, NTXX, k)
        
        for (i in 1:k) xsm[, i] <- rep(xsms[, i], T)
        for (i in 1:k) xsmXX[, i] <- rep(xsmsXX[, i], T)

        if (model %in% c("lag", "sarar")) {
            wysms   <- tapply(wy, inde1, mean)
            wysmsXX <- tapply(wyXX, inde1XX, mean)
            wysm    <- rep(wysms, T)
            wysmXX  <- rep(wysmsXX, T)
            assign("wysms", wysms, envir = env)
            assign("wysmsXX", wysmsXX, envir = env)
        }
        assign("ysms", ysms, envir = env)
        assign("ysmsXX", ysmsXX, envir = env)
        assign("xsms", xsms, envir = env)
        assign("xsmsXX", xsmsXX, envir = env)
    }
    if (effects == "pooled") {
        yt <- y
        xt <- x
        ytXX <- yXX
        xtXX <- xXX

    }
    if (effects == "tpfe") {
        yt <- y - ytm
        xt <- x - xtm    
        ytXX <- yXX - ytmXX
        xtXX <- xXX - xtmXX
    }
    if (effects == "spfe") {   
        yt <- y - ysm           ## seems to work up to this point
        xt <- x - xsm
        ytXX <- yXX - ysmXX
        xtXX <- xXX - xsmXX      
    }
    if (effects == "sptpfe") {
        yt <- y - ysm - ytm + rep(mean(y), NT)
        xmm <- matrix(, NT, (k))
        for (i in 1:(k)) xmm[, i] <- rep(mean(x[, i]), NT)
        xt <- x - xsm - xtm + xmm
    }
    wyt <- unlist(tapply(yt, inde, function(u) lag.listw(listw, 
        u), simplify = TRUE))
    if (model == "sarar") {
        w2yt <- unlist(tapply(yt, inde, function(u) lag.listw(listw2, 
            u), simplify = TRUE))
        w2wyt <- unlist(tapply(wyt, inde, function(u) lag.listw(listw2, 
            u), simplify = TRUE))
    }
    if (model == "error") {
        dm <- function(A) trash <- unlist(tapply(A, inde, function(TT) lag.listw(listw, 
            TT), simplify = TRUE))
        wxt <- apply(xt, 2, dm)
        colnames(wxt) <- paste("Lag.", colnames(x), sep = "")
        wx <- apply(x, 2, dm)
        colnames(wx) <- paste("lag.", colnames(x), sep = "")
    }
    if (model == "sarar") {
        dm <- function(A) trash <- unlist(tapply(A, inde, function(TT) lag.listw(listw2, 
            TT), simplify = TRUE))
        wxt <- apply(xt, 2, dm)
        colnames(wxt) <- paste("Lag.", colnames(x), sep = "")
        wx <- apply(x, 2, dm)
        colnames(wx) <- paste("lag.", colnames(x), sep = "")
    }
    colnames(xt) <- dimnames(x)[[2]]
    assign("yt", yt, envir = env)
    assign("ytXX", ytXX, envir = env)
    assign("xt", xt, envir = env)
    assign("xtXX", xtXX, envir = env)
    assign("wyt", wyt, envir = env)
    assign("wy", wy, envir = env)
    assign("wyXX", wyXX, envir = env)

    if (model %in% c("error", "sarar")) {
        assign("wx", wx, envir = env)
        assign("wxt", wxt, envir = env)
        if (model == "sarar") {
            assign("w2yt", w2yt, envir = env)
            assign("w2wyt", w2wyt, envir = env)
            assign("listw2", listw2, envir = env)
        }
    }
    assign("inde", inde, envir = env)
    assign("indeXX", indeXX, envir = env)
    assign("con", con, envir = env)
    assign("verbose", !quiet, envir = env)
    assign("can.sim", can.sim, envir = env)
    assign("compiled_sse", con$compiled_sse, envir = env)
    assign("similar", FALSE, envir = env)
    assign("similar2", FALSE, envir = env)
    assign("family", "SAR", envir = env)
    assign("LAPACK", con$LAPACK, envir = env)
    timings[["set_up"]] <- proc.time() - .ptime_start
    .ptime_start <- proc.time()
    if (!quiet) 
        cat("Jacobian calculated using ")
    switch(method, eigen = {
        if (!quiet) cat("neighbourhood matrix eigenvalues\n")
        if (model != "sarar") {
            eigen_setup(env)
            er <- get("eig.range", envir = env)
            if (is.null(interval)) interval <- c(er[1] + .Machine$double.eps, 
                er[2] - .Machine$double.eps)
        } else {
            eigen_setup(env, which = 1)
            eigen_setup(env, which = 2)
        }
    }, Matrix = {
        if (listw$style %in% c("W", "S") && !can.sim) stop("Matrix method requires symmetric weights")
        if (listw$style %in% c("B", "C", "U") && !(is.symmetric.glist(listw$neighbours, 
            listw$weights))) stop("Matrix method requires symmetric weights")
        if (!quiet) cat("sparse matrix Cholesky decomposition\n")
        if (model == "sarar") {
            Imult <- con$Imult
            if (listw$style == "B") {
                Imult <- ceiling((2/3) * max(sapply(listw$weights, 
                  sum)))
            }
            Matrix_setup(env, Imult, con$super, which = 1)
            W <- as(as_dgRMatrix_listw(listw), "CsparseMatrix")
            I <- as_dsCMatrix_I(n)
            if (listw2$style %in% c("W", "S") && !can.sim2) stop("Matrix method requires symmetric weights")
            if (listw2$style %in% c("B", "C", "U") && !(is.symmetric.glist(listw2$neighbours, 
                listw2$weights))) stop("Matrix method requires symmetric weights")
            Imult <- con$Imult
            if (listw2$style == "B") {
                Imult <- ceiling((2/3) * max(sapply(listw2$weights, 
                  sum)))
            }
            Matrix_setup(env, Imult, con$super, which = 2)
            W2 <- as(as_dgRMatrix_listw(listw2), "CsparseMatrix")
        } else {
            Imult <- con$Imult
            if (is.null(interval)) {
                if (listw$style == "B") {
                  Imult <- ceiling((2/3) * max(sapply(listw$weights, 
                    sum)))
                  interval <- c(-0.5, +0.25)
                } else interval <- c(-1, 0.999)
            }
            if (is.null(con$super)) con$super <- as.logical(NA)
            Matrix_setup(env, Imult, con$super)
            W <- as(as_dgRMatrix_listw(listw), "CsparseMatrix")
            I <- as_dsCMatrix_I(n)
        }
    }, spam = {
        if (!require(spam)) stop("spam not available")
        if (listw$style %in% c("W", "S") && !can.sim) stop("spam method requires symmetric weights")
        if (listw$style %in% c("B", "C", "U") && !(is.symmetric.glist(listw$neighbours, 
            listw$weights))) stop("spam method requires symmetric weights")
        if (!quiet) cat("sparse matrix Cholesky decomposition\n")
        if (model == "sarar") {
            spam_setup(env, which = 1)
            W <- as.spam.listw(get("listw", envir = env))
            if (listw2$style %in% c("W", "S") && !can.sim2) stop("spam method requires symmetric weights")
            if (listw2$style %in% c("B", "C", "U") && !(is.symmetric.glist(listw2$neighbours, 
                listw2$weights))) stop("spam method requires symmetric weights")
            spam_setup(env, which = 2)
            W2 <- as.spam.listw(get("listw2", envir = env))
        } else {
            spam_setup(env)
            W <- as.spam.listw(get("listw", envir = env))
            if (is.null(interval)) interval <- c(-1, 0.999)
        }
    }, Chebyshev = {
        if (listw$style %in% c("W", "S") && !can.sim) stop("Chebyshev method requires symmetric weights")
        if (listw$style %in% c("B", "C", "U") && !(is.symmetric.glist(listw$neighbours, 
            listw$weights))) stop("Chebyshev method requires symmetric weights")
        if (!quiet) cat("sparse matrix Chebyshev approximation\n")
        if (model == "sarar") {
            cheb_setup(env, q = con$cheb_q, which = 1)
            W <- get("W", envir = env)
            I <- as_dsCMatrix_I(n)
            if (listw2$style %in% c("W", "S") && !can.sim2) stop("Chebyshev method requires symmetric weights")
            if (listw2$style %in% c("B", "C", "U") && !(is.symmetric.glist(listw2$neighbours, 
                listw2$weights))) stop("Chebyshev method requires symmetric weights")
            cheb_setup(env, q = con$cheb_q, which = 2)
            W2 <- get("W2", envir = env)
        } else {
            cheb_setup(env, q = con$cheb_q)
            W <- get("W", envir = env)
            I <- as_dsCMatrix_I(n)
            if (is.null(interval)) interval <- c(-1, 0.999)
        }
    }, MC = {
        if (!listw$style %in% c("W")) stop("MC method requires row-standardised weights")
        if (!quiet) cat("sparse matrix Monte Carlo approximation\n")
        if (model == "sarar") {
            mcdet_setup(env, p = con$MC_p, m = con$MC_m, which = 1)
            W <- get("W", envir = env)
            I <- as_dsCMatrix_I(n)
            if (!listw2$style %in% c("W")) stop("MC method requires row-standardised weights")
            mcdet_setup(env, p = con$MC_p, m = con$MC_m, which = 2)
            W2 <- get("W2", envir = env)
        } else {
            mcdet_setup(env, p = con$MC_p, m = con$MC_m)
            W <- get("W", envir = env)
            I <- as_dsCMatrix_I(n)
            if (is.null(interval)) interval <- c(-1, 0.999)
        }
    }, LU = {
        if (!quiet) cat("sparse matrix LU decomposition\n")
        if (model == "sarar") {
            LU_setup(env, which = 1)
            W <- get("W", envir = env)
            I <- get("I", envir = env)
            LU_setup(env, which = 2)
            W2 <- get("W2", envir = env)
        } else {
            LU_setup(env)
            W <- get("W", envir = env)
            I <- get("I", envir = env)
            if (is.null(interval)) interval <- c(-1, 0.999)
        }
    }, stop("...\nUnknown method\n"))
    nm <- paste(method, "set_up", sep = "_")
    timings[[nm]] <- proc.time() - .ptime_start
    .ptime_start <- proc.time()



#######################
    if (model == "lag") {
        RES <- splaglm(env = env, zero.policy = zero.policy, 
            interval = interval)
        res.eff <- felagXX(env = env, beta = RES$coeff, sige = RES$s2, 
            effects = effects, method = method, rho = RES$rho, 
            legacy = legacy, zero.policy = zero.policy)
    }
    if (model == "error") {
        RES <- sperrorlm(env = env, zero.policy = zero.policy, 
            interval = interval)
        res.eff <- feerror(env = env, beta = RES$coeff, sige = RES$s2, 
            effects = effects, method = method, lambda = RES$lambda, 
            legacy = legacy)
    }
    if (model == "sarar") {
        RES <- spsararlm(env = env, zero.policy = zero.policy, 
            con = con, llprof = llprof, tol.solve = tol.solve)
        res.eff <- felag(env = env, beta = RES$coeff, sige = RES$s2, 
            effects = effects, method = method, rho = RES$lambda, 
            legacy = legacy, zero.policy = zero.policy)
    }


    yme <- y - mean(y)
    rsqr2 <- crossprod(yme)
    rsqr1 <- crossprod(res.eff[[1]]$res.e)
    res.R2 <- 1 - rsqr1/rsqr2
    y.hat <- res.eff[[1]]$xhat
    y.hatXX <- res.eff[[1]]$xhatXX

    res <- as.numeric(res.eff[[1]]$res.e)
    resXX <- as.numeric(res.eff[[1]]$res.eXX)

    N.vars <- res.eff$N.vars
    nam.rows <- dimnames(x)[[1]]
    names(y.hat) <- nam.rows
    nam.rowsXX <- dimnames(xXX)[[1]]
    names(y.hatXX) <- nam.rowsXX                      # y.hatXX is null???
    names(res) <- nam.rows
    names(resXX) <- nam.rowsXX

    model.data <- data.frame(cbind(y, x))
    dimnames(model.data)[[1]] <- nam.rows
    if (model == "lag") 
        spat.coef <- RES$rho
    if (model == "error") 
        spat.coef <- RES$lambda
    if (model == "sarar") 
        spat.coef <- c(RES$rho, RES$lambda)
    if (is.null(RES$lambda.se) && model == "error") 
        Coeff <- RES$coeff
    else Coeff <- c(spat.coef, RES$coeff)
    type <- paste("fixed effects", model)
    var <- RES$asyvar1
    if (model == "sarar") {
        var <- matrix(0, (ncol(RES$asyvar1) + 2), (ncol(RES$asyvar1) + 
            2))
        var[1, 1] <- RES$lambda.se
        var[2, 2] <- RES$rho.se
        var[((2 + 1):ncol(var)), ((2 + 1):ncol(var))] <- RES$asyvar1
    }
    spmod <- list(coefficients = Coeff, errcomp = NULL, vcov = var, 
        spat.coef = spat.coef, vcov.errcomp = NULL, residuals = res, residualsXX = resXX,
        fitted.values = y.hat, fitted.valuesXX = y.hatXX, sigma2 = RES$s2, type = type, 
        model = model.data, call = cl, logLik = RES$ll, method = method, 
        effects = effects, res.eff = res.eff)
    class(spmod) <- "splm"
    return(spmod)
}





felagXX <- function (env, beta, sige, effects, method, rho, legacy, zero.policy) 
{
    y <- get("y", envir = env)
    yXX <- get("yXX", envir = env)
    x <- get("x", envir = env)
    xXX <- get("xXX", envir = env)
    wy <- get("wy", envir = env)
    wyXX <- get("wyXX", envir = env)
    yt <- get("yt", envir = env)
    ytXX <- get("ytXX", envir = env)
    xt <- get("xt", envir = env)
    xtXX <- get("xtXX", envir = env)
    N <- get("n", envir = env)
    NXX <- get("nXX", envir = env)
    T <- get("T", envir = env)
    TXX <- get("TXX", envir = env)
    NT <- get("NT", envir = env)
    NTXX <- get("NTXX", envir = env)
    k <- get("k", envir = env)
    kXX <- get("kXX", envir = env)
    listw <- get("listw", envir = env)
    listwXX <- get("listwXX", envir = env)
    inde <- get("inde", envir = env)
    indeXX <- get("indeXX", envir = env)

    mx <- apply(x, 2, mean)
    mxXX <- apply(xXX, 2, mean)
    intercept <- mean(y) - mean(wy) * rho - mx %*% beta
    interceptXX <- mean(yXX) - mean(wyXX) * rho - mxXX %*% beta

    if (effects == "spfe") {
        ysms <- get("ysms", envir = env)
        xsms <- get("xsms", envir = env)
        ysmsXX <- get("ysmsXX", envir = env)
        xsmsXX <- get("xsmsXX", envir = env)
        
        wysms <- get("wysms", envir = env)
        wysmsXX <- get("wysmsXX", envir = env)

        res.sfe <- as.matrix(ysms) - as.matrix(wysms) * rho - 
            xsms %*% as.matrix(beta) - as.numeric(intercept)
        res.sfeXX <- as.matrix(ysmsXX) - as.matrix(wysmsXX) * rho - 
            xsmsXX %*% as.matrix(beta) - as.numeric(interceptXX)
        xhat <- x %*% as.matrix(beta) + rep(res.sfe, T) + as.numeric(intercept)
        xhatXX <- xXX %*% as.matrix(beta) + rep(res.sfeXX, T) + as.numeric(interceptXX)
        
        res.var.sfe <- (sige/T) + diag((as.numeric(sige) * (xsms %*% 
            solve(crossprod(xt)) %*% t(xsms))))
        res.se.sfe <- sqrt(res.var.sfe)
        res.t.sfe <- res.sfe/res.se.sfe
        res.se.con <- sqrt(as.numeric(sige)/NT + as.numeric(sige) * 
            t(as.matrix(mx)) %*% solve(crossprod(xt)) %*% as.matrix(mx))
        res.t.con <- as.numeric(intercept)/res.se.con
        N.vars <- k + N
        res.e <- y - xhat - rho * wy
        res.eXX <- yXX - xhatXX - rho * wyXX
        FE.out <- list(res.sfe = res.sfe, res.sfeXX=res.sfeXX, 
            res.se.sfe = res.se.sfe, intercept = intercept, interceptXX = interceptXX,
            res.se.con = res.se.con, xhat = xhat, xhatXX = xhatXX,
            N.vars = N.vars, res.e = res.e, res.eXX=res.eXX)
    }
    if (effects == "tpfe") {
        ytms <- get("ytms", envir = env)
        ytmsXX <- get("ytmsXX", envir = env)
        xtms <- get("xtms", envir = env)
        xtmsXX <- get("xtmsXX", envir = env)
        wytms <- get("wytms", envir = env)
        wytmsXX <- get("wytmsXX", envir = env)

        res.tfe <- as.matrix(ytms) - as.matrix(wytms) * rho - 
            xtms %*% as.matrix(beta) - as.numeric(intercept)
        res.tfeXX <- as.matrix(ytmsXX) - as.matrix(wytmsXX) * rho - 
            xtmsXX %*% as.matrix(beta) - as.numeric(interceptXX)
        xhat <- x %*% as.matrix(beta) + rep(res.tfe, each = N) + 
            as.numeric(intercept)
        xhatXX <- xXX %*% as.matrix(beta) + rep(res.tfeXX, each = NXX) + 
            as.numeric(interceptXX)

        res.var.tfe <- (sige/N) + (as.numeric(sige) * (xtms %*% 
            solve(crossprod(xt)) %*% t(xtms)))
        res.se.tfe <- sqrt(diag(res.var.tfe))
        res.t.tfe <- res.tfe/res.se.tfe
        res.se.con <- sqrt(as.numeric(sige)/NT + as.numeric(sige) * 
            t(as.matrix(mx)) %*% solve(crossprod(xt)) %*% as.matrix(mx))
        res.t.con <- as.numeric(intercept)/res.se.con
        N.vars <- k + T
        res.e <- y - xhat - rho * wy
        res.eXX <- yXX - xhatXX - rho * wyXX
        FE.out <- list(res.tfe = res.tfe, res.se.tfe = res.se.tfe, 
            intercept = intercept, res.se.con = res.se.con, xhat = xhat, xhatXX = xhatXX,
            N.vars = N.vars, res.e = res.e, res.eXX= res.eXX)
    }
    if (effects == "sptpfe") {
        ysms <- get("ysms", envir = env)
        xsms <- get("xsms", envir = env)
        wysms <- get("wysms", envir = env)
        ytms <- get("ytms", envir = env)
        xtms <- get("xtms", envir = env)
        wytms <- get("wytms", envir = env)
        res.sfe <- as.matrix(ysms) - as.matrix(wysms) * rho - 
            xsms %*% as.matrix(beta) - as.numeric(intercept)
        res.tfe <- as.matrix(ytms) - as.matrix(wytms) * rho - 
            xtms %*% as.matrix(beta) - as.numeric(intercept)
        res.var.sfe <- (sige/T) + (as.numeric(sige) * (xsms %*% 
            solve(crossprod(xt)) %*% t(xsms)))
        res.se.sfe <- sqrt(diag(res.var.sfe))
        res.var.tfe <- (sige/N) + (as.numeric(sige) * (xtms %*% 
            solve(crossprod(xt)) %*% t(xtms)))
        res.se.tfe <- sqrt(diag(res.var.tfe))
        res.t.sfe <- res.sfe/res.se.sfe
        res.t.tfe <- res.tfe/res.se.tfe
        res.se.con <- sqrt(as.numeric(sige)/NT + as.numeric(sige) * 
            t(as.matrix(mx)) %*% solve(crossprod(xt)) %*% as.matrix(mx))
        res.t.con <- as.numeric(intercept)/res.se.con
        xhat <- x %*% as.matrix(beta) + rep(res.sfe, T) + rep(res.tfe, 
            each = N) + as.numeric(intercept)
        N.vars <- k + N + T - 1
        res.e <- y - xhat - rho * wy
        FE.out <- list(res.tfe = res.tfe, res.se.tfe = res.se.tfe, 
            res.sfe = res.sfe, res.se.sfe = res.se.sfe, intercept = intercept, 
            res.se.con = res.se.con, xhat = xhat, N.vars = N.vars, 
            res.e = res.e)
    }
    if (effects == "pooled") {
        xhat <- x %*% as.matrix(beta)
        xhatXX <- xXX %*% as.matrix(beta)
        res.e <- y - xhat - rho * wy
        res.eXX <- yXX - xhatXX - rho * wyXX
        FE.out <- list(xhat = xhat,xhatXX = xhatXX , N.vars = k, res.e = res.e, res.eXX = res.eXX)
    }
    if (legacy) {
        W <- listw2dgCMatrix(listw, zero.policy = zero.policy)
        IrW <- Diagonal(N) - rho * W
        IrWi <- solve(IrW)
        xtb <- xt %*% beta
        yhat <- unlist(tapply(xhat, inde, function(u) IrWi %*% 
            u))
        ywhat <- unlist(tapply(xtb, inde, function(u) IrWi %*% 
            u))
        r1 <- as.matrix(yt - mean(yt))
        r2 <- as.matrix(ywhat - mean(ywhat))
        r1r2 <- crossprod(r1, r2)
        r1r1 <- crossprod(r1)
        r2r2 <- crossprod(r2)
        res.corr <- as.numeric(r1r2^2)/(as.numeric(r1r1) * as.numeric(r2r2))
    }
    else res.corr <- NULL
    FE.out <- list(FE.out, res.corr = res.corr)
    FE.out
}


####### dont touch this should be find used to estimate RHO in sample
splaglm <-function (env, zero.policy = zero.policy, interval = interval) 
{
    xt <- get("xt", envir = env)
    yt <- get("yt", envir = env)
    wyt <- get("wyt", envir = env)
    con <- get("con", envir = env)
    NT <- get("NT", envir = env)
    T <- get("T", envir = env)
    listw <- get("listw", envir = env)
    inde <- get("inde", envir = env)
    XpX <- crossprod(xt)
    b0 <- solve(XpX, crossprod(xt, yt))
    b1 <- solve(XpX, crossprod(xt, wyt))
    e0 <- yt - xt %*% b0
    e1 <- wyt - xt %*% b1
    e0e0 <- crossprod(e0)
    e1e1 <- crossprod(e1)
    e0e1 <- t(e1) %*% e0
    assign("e0e0", e0e0, envir = env)
    assign("e1e1", e1e1, envir = env)
    assign("e0e1", e0e1, envir = env)
    opt <- optimize(conclikpan, interval = interval, maximum = TRUE, 
        env = env, tol = con$tol.opt)
    rho <- opt$maximum
    names(rho) <- "lambda"
    LL <- opt$objective
    optres <- opt
    lm.lag <- lm((yt - rho * wyt) ~ xt - 1)
    p <- lm.lag$rank
    r <- residuals(lm.lag)
    fit <- yt - r
    names(r) <- names(fit)
    betas <- coefficients(lm.lag)
    names(betas) <- colnames(xt)
    SSE <- deviance(lm.lag)
    s2 <- SSE/NT
    tr <- function(A) sum(diag(A))
    W <- listw2dgCMatrix(listw, zero.policy = zero.policy)
    A <- solve(diag(NT/T) - rho * W)
    WA <- W %*% A
    one <- T * (tr(WA %*% WA) + tr(t(WA) %*% WA))
    lag <- function(q) trash <- unlist(tapply(q, inde, function(TT) as.matrix(WA %*% 
        TT), simplify = TRUE))
    lag2 <- function(q) trash <- unlist(tapply(q, inde, function(TT) as.matrix(t(WA) %*% 
        TT), simplify = TRUE))
    WAxt <- apply(as.matrix(xt), 2, lag)
    WAWAxt <- apply(WAxt, 2, lag2)
    xtWAWAxt <- crossprod(xt, WAWAxt)
    xtWAxt <- crossprod(xt, WAxt)
    xtxt <- crossprod(xt)
    two <- 1/as.numeric(s2) * t(betas) %*% xtWAWAxt %*% betas
    V <- one + two
    zero <- rbind(rep(0, length(betas)))
    col1 <- rbind(NT/(2 * (s2^2)), T * tr(WA)/s2, t(zero))
    three <- (1/as.numeric(s2)) * xtWAxt %*% betas
    col2 <- rbind(T * tr(WA)/s2, V, three)
    col3 <- rbind(zero, t(three), 1/as.numeric(s2) * xtxt)
    asyvar <- cbind(col1, col2, col3)
    asyv <- solve(asyvar, tol = con$tol.solve)
    rownames(asyv) <- colnames(asyv) <- c("sigma", "lambda", 
        colnames(xt))
    rho.se <- sqrt(asyv[2, 2])
    rest.se <- sqrt(diag(asyv))[-c(1:2)]
    asyvar1 <- asyv[-1, -1]
    rownames(asyvar1) <- colnames(asyvar1) <- c("lambda", colnames(xt))
    return <- list(coeff = betas, rho = rho, s2 = s2, rest.se = rest.se, 
        rho.se = rho.se, asyvar1 = asyvar1)
}

### also dont mess with, ML function
conclikpan <- function (lambda, env) 
{
    e0e0 <- get("e0e0", envir = env)
    e1e1 <- get("e1e1", envir = env)
    e0e1 <- get("e0e1", envir = env)
    NT <- get("NT", envir = env)
    T <- get("T", envir = env)
    Nsig <- e0e0 - 2 * lambda * e0e1 + lambda * lambda * e1e1
    sigma2 <- Nsig/NT
    ldet <- do_ldet(lambda, env)
    ret <- -(NT/2) * log(Nsig) + T * ldet
    if (get("verbose", envir = env)) 
        cat("rho:\t", lambda, "\tfunction value:\t", ret, "\n")
    ret
}



"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y





# ##########################################################################################
# #load("C:/Users/mmann1123/Desktop/Share/Final_Variables_2/Out-of-sample5-FUNCTIONS_workspace.RData")
# ### IMPORT POLYGONS AND ITS DBF FILE
# Block.39.ex = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion6")
# 
# tmax1971_2000 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\tmax1971_2000ave_HST.asc")
# tmax1941_1970 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\tmax1941_1970ave_HST.asc")
# tmax1911_1940 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\tmax1911_1940ave_HST.asc")
# 
# tmin1971_2000 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\tmin1971_2000ave_HST.asc")
# tmin1941_1970 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\tmin1941_1970ave_HST.asc")
# tmin1911_1940 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\tmin1911_1940ave_HST.asc")
# 
# 
# ppt1941_1970 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\ppt1941_1970ave_HST.asc")
# ppt1911_1940 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\ppt1911_1940ave_HST.asc")
# ppt1971_2000 = raster("P:\\Meg's computer (Scheherazade)\\meg\\cec_vulnerability & adaptation\\data analysis\\digested data\\ppt1971_2000ave_HST.asc")
# 
# ppt_ave = mean(ppt1941_1970,ppt1911_1940,ppt1971_2000)
# tmax_ave = mean(tmax1971_2000,tmax1941_1970,tmax1911_1940)
# tmin_ave = mean(tmin1971_2000,tmin1941_1970,tmin1911_1940)
# 
# projection(ppt_ave)='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
# projection(tmax_ave)='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
# projection(tmin_ave)='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
# 
# writeRaster(ppt_ave, "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Climate Normals Meg\\ppt_ave19112000.tif", format="GTiff", overwrite=TRUE)
# writeRaster(tmax_ave, "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Climate Normals Meg\\tmax_ave19112000.tif", format="GTiff", overwrite=TRUE)
# writeRaster(tmin_ave, "G:\\Faculty\\Mann\\Share\\Environemental Factors\\Climate Normals Meg\\tmin_ave19112000.tif", format="GTiff", overwrite=TRUE)
# 
# ppt_ave2 = projectRaster(ppt_ave, crs= (proj4string(Block.39.ex)))
# tmax_ave2 = projectRaster(tmax_ave, crs= (proj4string(Block.39.ex)))
#  
# 
# Block.dbf = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")   #read dbf file for edit
# pptave = read.dbf("G:\\Faculty\\Mann\\Share\\Environemental Factors\\Climate Normals Meg\\pptave1100.dbf")
# tmax = read.dbf("G:\\Faculty\\Mann\\Share\\Environemental Factors\\Climate Normals Meg\\tmax1100.dbf")
# tmin = read.dbf("G:\\Faculty\\Mann\\Share\\Environemental Factors\\Climate Normals Meg\\tmin1100.dbf")
# Block.dbf = merge(Block.dbf, pptave, by="SUM090", incomparables=NA,all=T)
# Block.dbf = subset(Block.dbf, select=-c(OBJECTID.y))
# Block.dbf = merge(Block.dbf, tmax, by="SUM090", incomparables=NA,all=T)
# Block.dbf = subset(Block.dbf, select=-c(OBJECTID))
# Block.dbf = merge(Block.dbf, tmin, by="SUM090", incomparables=NA,all=T)
# Block.dbf = subset(Block.dbf, select=-c(OBJECTID))
# names(Block.dbf)[3]="OBJECTID"
# Block.dbf = Block.dbf[with(Block.dbf, order(sort) ), ]                  # resort to polygon order
# write.dbf(Block.dbf,file=paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf"))


rbind.files <- function(file.paths,delimitor,file_names){
    imp <- vector("list", length(file.paths)) 
    for (i in 1:length(file.paths)) { 
        a<- read.delim(file.paths[i], sep=delimitor,  header = TRUE) 
        if(!is.null(file_names) ) {a$file.name = file_names[i]} 
        imp[[i]] = a
    } 
    combined <- do.call("rbind", imp)
    return(combined)}


# 
# 
# specifierAIC3dist <- function(county){
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   n=dim(panel.specify)[1]
#   
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=c(9,9,9,9,9,9,9,9,9), rhoP=c(9,9,9,9,9,9,9,9,9))
#   row.names(rho_hold) = c("f3a","f3b","f3c","f2a","f2b","f2c","f0a","f0b","f0c")
#   
#   f3a = HuDac4 ~ COUNTY +  dist_ppall + HuDaclag+HuDaclagSqr+HuDaclagQb
#   f3b = HuDac4 ~ COUNTY +  dist_pp30k + HuDaclag+HuDaclagSqr+HuDaclagQb
#   f3c = HuDac4 ~ COUNTY +  dist_pp20k + HuDaclag+HuDaclagSqr+HuDaclagQb
#   
#   f2a = HuDac4 ~ COUNTY +  dist_ppall + HuDaclag + HuDaclagSqr
#   f2b = HuDac4 ~ COUNTY +  dist_pp30k + HuDaclag+HuDaclagSqr
#   f2c = HuDac4 ~ COUNTY +  dist_pp20k + HuDaclag+HuDaclagSqr
#   
#   f0a = HuDac4 ~ COUNTY +  dist_ppall + LnHuDaclag
#   f0b = HuDac4 ~ COUNTY +  dist_pp30k + LnHuDaclag
#   f0c = HuDac4 ~ COUNTY +  dist_pp20k + LnHuDaclag
#   
#   
#   if(length(county) ==1 ) {     # avoid signular for COUNTY if only one county present
#     f3a = HuDac4 ~  dist_ppall + HuDaclag+HuDaclagSqr+HuDaclagQb
#     f3b = HuDac4 ~  dist_pp30k + HuDaclag+HuDaclagSqr+HuDaclagQb
#     f3c = HuDac4 ~  dist_pp20k + HuDaclag+HuDaclagSqr+HuDaclagQb
#     
#     f2a = HuDac4 ~  dist_ppall + HuDaclag + HuDaclagSqr
#     f2b = HuDac4 ~  dist_pp30k + HuDaclag+HuDaclagSqr
#     f2c = HuDac4 ~  dist_pp20k + HuDaclag+HuDaclagSqr
#     
#     f0a = HuDac4 ~  dist_ppall + LnHuDaclag
#     f0b = HuDac4 ~  dist_pp30k + LnHuDaclag
#     f0c = HuDac4 ~  dist_pp20k + LnHuDaclag
# 
#     print("Only one county")
#   }
#   
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   
#   ################
#   specify3a = tryCatch( {spml(formula=f3a,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   print(summary(specify3a))
#   if(specify3a=="errors"){
#     AIC3a = 100000
#     AICc3a = 100000
#   } else{
#     k=(length(specify3a$coefficients)-1)
#     AIC3a = 2*k - 2*specify3a$logLik
#     AICc3a = AIC3a + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify3a)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify3a)$ErrCompTable[4]      
#     remove(specify3a)
#   }
#  
#   ################
#   specify3b = tryCatch( {spml(formula=f3b,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                  return("errors") })
#   print(summary(specify3b))
#   if(specify3b=="errors"){
#     AIC3b = 100000
#     AICc3b = 100000
#   } else{
#     k=(length(specify3b$coefficients)-1)
#     AIC3b = 2*k - 2*specify3b$logLik
#     AICc3b = AIC3b + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[2] = summary(specify3b)$ErrCompTable[1]
#     rho_hold$rhoP[2] = summary(specify3b)$ErrCompTable[4]      
#     remove(specify3b)
#   }
#   
#   ################
#   specify3c = tryCatch( {spml(formula=f3c,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                  return("errors") })
#   print(summary(specify3c))
#   if(specify3c=="errors"){
#     AIC3c = 100000
#     AICc3c = 100000
#   } else{
#     k=(length(specify3c$coefficients)-1)
#     AIC3c = 2*k - 2*specify3c$logLik
#     AICc3c = AIC3c + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[3] = summary(specify3c)$ErrCompTable[1]
#     rho_hold$rhoP[3] = summary(specify3c)$ErrCompTable[4]      
#     remove(specify3c)
#   }
#   
#   ################
#   specify2a = tryCatch( {spml(formula=f2a,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   print(summary(specify2a))
#   if(specify2a=="errors"){
#     AIC2a=100000
#     AICc2a = 100000
#   } else{
#     k=(length(specify2a$coefficients)-1)
#     AIC2a = 2*k - 2*specify2a$logLik
#     AICc2a = AIC2a + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[4] = summary(specify2a)$ErrCompTable[1]
#     rho_hold$rhoP[4] = summary(specify2a)$ErrCompTable[4]
#     remove(specify2a)
#   }
#   
#   ################
#   specify2b = tryCatch( {spml(formula=f2b,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                  return("errors") })
#   print(summary(specify2b))
#   if(specify2b=="errors"){
#     AIC2b =100000
#     AICc2b = 100000
#   } else{
#     k=(length(specify2b$coefficients)-1)
#     AIC2b = 2*k - 2*specify2b$logLik
#     AICc2b = AIC2b + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[5] = summary(specify2b)$ErrCompTable[1]
#     rho_hold$rhoP[5] = summary(specify2b)$ErrCompTable[4]
#     remove(specify2b)
#   }
#   
#   ################
#   specify2c = tryCatch( {spml(formula=f2c,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                  return("errors") })
#   print(summary(specify2c))
#   if(specify2c=="errors"){
#     AIC2c =100000
#     AICc2c = 100000
#   } else{
#     k=(length(specify2c$coefficients)-1)
#     AIC2c = 2*k - 2*specify2c$logLik
#     AICc2c = AIC2c + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[6] = summary(specify2c)$ErrCompTable[1]
#     rho_hold$rhoP[6] = summary(specify2c)$ErrCompTable[4]
#     remove(specify2c)
#   }
#   
#   ################          
#   specify0a = tryCatch( {spml(formula=f0a,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   print(summary(specify0a))
#   if(specify0a=="errors"){
#     AIC0a =100000
#     AICc0a = 100000
#   } else{
#     k=(length(specify0a$coefficients)-1)
#     AIC0a = 2*k - 2*specify0a$logLik
#     AICc0a = AIC0a + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[7] = summary(specify0a)$ErrCompTable[1]
#     rho_hold$rhoP[7] = summary(specify0a)$ErrCompTable[4]
#     remove(specify0a)
#   }
#   
#   ################          
#   specify0b = tryCatch( {spml(formula=f0b,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                  return("errors") })
#   print(summary(specify0b))
#   if(specify0b=="errors"){
#     AIC0b =100000
#     AICc0b = 100000
#   } else{
#     k=(length(specify0b$coefficients)-1)
#     AIC0b = 2*k - 2*specify0b$logLik
#     AICc0b = AIC0b + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[8] = summary(specify0b)$ErrCompTable[1]
#     rho_hold$rhoP[8] = summary(specify0b)$ErrCompTable[4]
#     remove(specify0b)
#   }
#   
#   ################          
#   specify0c = tryCatch( {spml(formula=f0c,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                  return("errors") })
#   print(summary(specify0c))
#   if(specify0c=="errors"){
#     AIC0c =100000
#     AICc0c = 100000
#   } else{
#     k=(length(specify0c$coefficients)-1)
#     AIC0c = 2*k - 2*specify0c$logLik
#     AICc0c = AIC0c + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[9] = summary(specify0c)$ErrCompTable[1]
#     rho_hold$rhoP[9] = summary(specify0c)$ErrCompTable[4]
#     remove(specify0c)
#   }
#   
#   choice = data.frame(f3a=AICc3a,f3b=AICc3b,f3c=AICc3c,f2a=AICc2a,f2b=AICc2b,f2c=AICc2c,f0a=AICc0a,f0b=AICc0b,f0c=AICc0c)
#   if(AIC3a== 100000 & AIC3b== 100000 & AIC3c== 100000 & AIC2a== 100000 & AIC2b== 100000 & AIC2c== 100000 & AIC0a== 100000 & AIC0b== 100000 & AIC0c== 100000){stop("ERROR: No equations working")}
#   usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#   fm3 = get(usef[length(usef)])
#   rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#   rho_hold <<- rho_hold_fm
#   return(fm3)
# }
# 
# 
# specifierAIC3C <- function(county){
#   # FIND SPECIFICATION THAT ENDS WITH NEGATIVE AND SIGNIFICANT 
#   n=dim(panel.specify)[1]
#   
#   fm3 = 1   # to be replaced
#   rho_hold = data.frame(rho=c(9,9,9), rhoP=c(9,9,9))
#   row.names(rho_hold) = c("f3","f2","f0")
#    
#   f3 = HuDac4 ~ COUNTY + dist_india + dist_npark + dist_pp20k +dist_pp30k + dist_ppall + HuDaclag+HuDaclagSqr+HuDaclagQb
#   f2 = HuDac4 ~ COUNTY + dist_india + dist_npark + dist_pp20k +dist_pp30k + dist_ppall+ HuDaclag+HuDaclagSqr
#   f0 = HuDac4 ~ COUNTY + dist_india + dist_npark + dist_pp20k +dist_pp30k + dist_ppall+ LnHuDaclag
#   
#   if(length(county) ==1 ) {     # avoid signular for COUNTY if only one county present
#     f3 = HuDac4 ~  HuDaclag+HuDaclagSqr+HuDaclagQb+ dist_india + dist_npark + dist_pp20k +dist_pp30k + dist_ppall
#     f2 = HuDac4 ~  HuDaclag+HuDaclagSqr+ dist_india + dist_npark + dist_pp20k +dist_pp30k + dist_ppall
#     f0 = HuDac4 ~  LnHuDaclag+ dist_india + dist_npark + dist_pp20k +dist_pp30k + dist_ppall
#     print("Only one county")
#   }
#    
#   # switch between specification options   if not polynomial use logistic
#   # use trycatch to avoid singular matrix error, but obective to get ceofficient table from spml
#   
#   ################
#   specify3 = tryCatch( {spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   print(summary(specify3))
#   if(specify3=="errors"){
#     AIC3= 100000
#     AICc3 = 100000
#   } else{
#     k=(length(specify3$coefficients)-1)
#     AIC3 = 2*k - 2*specify3$logLik
#     AICc3 = AIC3 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[1] = summary(specify3)$ErrCompTable[1]
#     rho_hold$rhoP[1] = summary(specify3)$ErrCompTable[4]      
#     remove(specify3)
#   }
#   
#   ################
#   specify2 = tryCatch( {spml(formula=f2,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   print(summary(specify2))
#   if(specify2=="errors"){
#     AIC2=100000
#     AICc2 = 100000
#   } else{
#     k=(length(specify2$coefficients)-1)
#     AIC2 = 2*k - 2*specify2$logLik
#     AICc2 = AIC2 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[2] = summary(specify2)$ErrCompTable[1]
#     rho_hold$rhoP[2] = summary(specify2)$ErrCompTable[4]
#     remove(specify2)
#   }
#   
#   ################          
#   specify0 = tryCatch( {spml(formula=f0,data=panel.specify,listw=WKNN4,model="pooling",effect="time")}, error = function(err) {print(paste("Avoid_ERROR:  ",err))
#                                                                                                                                return("errors") })
#   print(summary(specify0))
#   if(specify0=="errors"){
#     AIC0=100000
#     AICc0 = 100000
#   } else{
#     k=(length(specify0$coefficients)-1)
#     AIC0 = 2*k - 2*specify0$logLik
#     AICc0 = AIC0 + (2*k*(k+1))/(n-k-1)
#     rho_hold$rho[3] = summary(specify0)$ErrCompTable[1]
#     rho_hold$rhoP[3] = summary(specify0)$ErrCompTable[4]
#     remove(specify0)
#   }
#   
#   choice = data.frame(f3=AICc3,f2=AICc2,f0=AICc0)
#   if(AIC3== 100000 & AIC2== 100000 & AIC0== 100000){stop("ERROR: No equations working")}
#   usef = names(choice[grepl(min(choice),choice)])    # choose lowest - possible to get more than one   
#   fm3 = get(usef[length(usef)])
#   rho_hold_fm = rho_hold[grep(usef,row.names(rho_hold)),]
#   rho_hold <<- rho_hold_fm
#   return(fm3)
# }
#