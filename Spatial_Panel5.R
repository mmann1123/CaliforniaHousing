library(mlogit)
library(plm)
library(rgdal)
library(splm)
library(spdep)
library(maptools)
library(PBSmapping)
library(RANN)
library(car)  #linear hypothesis tests
library(classInt)   #plotting classes

rm(list = ls())

  ### IMPORT POLYGONS AND ITS DBF FILE
  Block.39.ex = readOGR("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion5")
  Block.dbf = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion5_Original.dbf")   #read dbf file for edit

  ###  LIMIT TO COUNTIES 
  counties = c(37,33,30,13)
  Block.39_sp = Block.39.ex[Block.39.ex$County %in% counties,]
  Block.39.dbf_sp = Block.dbf[Block.dbf$County %in% counties,]
  
  ### create smaller sample   
  NSAMPLES = 2000   
  set.seed(123)
  Block.39_sp = Block.39_sp[Block.39_sp$SUM090 %in% sample(Block.39_sp$SUM090, NSAMPLES),]
  set.seed(123)
  Block.39.dbf_sp = Block.39.dbf_sp[Block.39.dbf_sp$SUM090 %in% sample(Block.39.dbf_sp$SUM090, NSAMPLES),]
  Block.39.dbf_sp$ordersp = 1:length(Block.39.dbf_sp$SUM090)

  ### neighbors 
  distanceband = 1000
  block.nb4=dnearneigh(coordinates(Block.39_sp), 0, distanceband, longlat = FALSE, row.names=Block.39_sp$SUM090) #3000
  W4 = nb2listw(block.nb4,style="W", zero.policy=TRUE)

  KNN4=knn2nb(knearneigh(coordinates(Block.39_sp),k=12))   #6
  WKNN4 = nb2listw(KNN4, style="W")
 
  ### GET NEIGHBORS HOUSING DENSITY FOR REGRESSION
  panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp)), grep("nbHuD4_",names(Block.39.dbf_sp))  ) , sep = "_")
  panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
 
           #getAvDen_START = function(polys, distanceband, data1, startyear,endyear){  # returns nbHuDac4_all
  getAvDen_START(Block.39_sp, distanceband, panel.data, 39,109 )
  Block.39.dbf_sp = merge(Block.39.dbf_sp,nbHuDac4_all, by="SUM090" )         # merge in nb housing density
  Block.39.dbf_sp = Block.39.dbf_sp[with(Block.39.dbf_sp, order(ordersp) ), ]

  ################################################
  ######   PUT DATA INTO PANEL LONG        ######
  panel.data = reshape(Block.39.dbf_sp, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp)) , grep("nbHuD4_",names(Block.39.dbf_sp)) ) , sep = "_")
  panel.data = pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  summary(panel.data)
  #by(panel.data$HuDac4, panel.data$time, mean )
  pdim(panel.data)
  
  ### CREATE NEW VARIABLES
  panel.data$HuDaclag = lag(panel.data$HuDac4, 1) 
  panel.data$HuDaclagSqr = panel.data$HuDaclag^2 
  panel.data$HuDaclagQb = panel.data$HuDaclag^3 
  panel.data$HuDaclag2 = lag(panel.data$HuDac4, 2) 
  panel.data$AllRoadsSqr = panel.data$AllRoads^2 
  panel.data$Rough_mnSqr = panel.data$Rough_mn^2 
  panel.data$DD6585Sqr = panel.data$DD6585^2 
  panel.data$Slope_mnSqr = panel.data$Slope_mn^2 
  panel.data$WaterTTSqr = panel.data$WaterTT^2 
  panel.data$nbHuD4lag  = lag(panel.data$nbHuD4,1)

 
  panel.data2 = data.frame(subset(panel.data, select=c(id,time)), subset(panel.data, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  panel.data2 = panel.data2[panel.data2$time %in% 49:99, ]                    # limit to years of interest
  #panel.data2 = na.omit(panel.data2)                                         # remove nas

  ################################################
  ######          RUN REGRESSIONS           ######    
   # call splm:::spreml  sub functions 
   #splm:::olsmod
   #splm:::spreml(fm2, data = panel.data2, index=c("id","time"),   w = W4,  lag = T)

  ### TEST FOR SPATIAL AUTOCORRELATION
  bsktest(fm2,data=panel.data2, listw = WKNN4, test="CLMlambda", standardize=TRUE)
  # spatial autocorrelation
  bsktest(fm2,data=panel.data2, listw = WKNN4, test="LM1", standardize=TRUE)
  # no random effects
  bsktest(fm2,data=panel.data2, listw = WKNN4, test="LM2", standardize=TRUE)
  # no spatial autocorrel?? Baltagi, B.H., Song, S.H. and Koh, W. (2003) Testing panel data regression models with spatial error correlation. Journal of Econometrics, 117, 123-150.


  mod1<- spgm(fm2, data=panel.data2, listw = WKNN4, model = "random", spatial.error = TRUE)
  mod2<- spgm(fm2, data=panel.data2, listw = WKNN4, model = "within", spatial.error = TRUE)
  test2<-sphtest(mod1, mod2)  # use fixed effects
  test2




  #fm = HuDac4 ~  HuDaclag +HuDaclagSqr+SlopePr15 +Rough_mn+ Incorp + factor(time)  
  #  SlopePr15 + Rough_mn +Rough_mnSqr + DD6585 + DD100 +
  #  HuDaclagQb PPAllTT

  #b=spml(fm, data= panel.data2, listw=W4,index=c("id","time"),  model="pooling", spatial.error="b", lag=FALSE)
  #summary(b)

  fm2 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb +Slope_mn+WaterTT # + as.numeric(time)  #factor(time) +DD6585
  b=spml(fm2, data= panel.data2, listw=WKNN4,index=c("id","time"), effect="time", lag=T, spatial.error = "none") #spatial.error="b", lag=FALSE)
  summary(b)
  results = summary(b)                                          # use to extract t-values, p-values, R2 or other stats  

  


  fm2 = HuDac4 ~ HuDaclag +HuDaclagSqr+HuDaclagQb+Slope_mn +WaterTT +DD6585 
  c=spml(fm2, data= panel.data2, listw=WKNN4, index=c("id","time"),  effect="time", lag=T, spatial.error = "none") # can only do FE if nblist has no zero links
  summary(c)
  effects.splm(c)
  hist(c$residuals[c$residuals<10],breaks=50)

  fm2 = HuDac4 ~ HuDaclag +HuDaclagSqr+HuDaclagQb+Slope_mn +WaterTT #+DD6585 
  d=spml(fm2, data= panel.data2, listw=WKNN4, index=c("id","time"),  effect="time", lag=F, spatial.error = "kkp") # can only do FE if nblist has no zero links
  summary(d)

  f = HuDac4 ~ HuDaclag +HuDaclagSqr + DD6585
  e=spml(f, data= panel.data2, listw=WKNN4, index=c("id","time"),model="within",  effect="individual", lag=F, spatial.error = "none") # can only do FE if nblist has no zero links
  g=summary(e)

  ### Hypothesis testing 
  linearHypothesis(c,"HuDaclag + 2*HuDaclagSqr + 3*HuDaclagQb = 0") # tests Beta1 = Beta2
  coef = c$coefficients
  covv = c$vcov
  deltaMethod(coef,"HuDaclag+HuDaclagSqr+HuDaclagQb", vcov = covv)

  ### Hausmann test 
  mod1 =spml(f, data= panel.data2, listw=WKNN4, index=c("id","time"),model="within",  effect="time", lag=T, spatial.error = "none") # can only do FE if nblist has no zero links
  mod2 =spml(f, data= panel.data2, listw=WKNN4, index=c("id","time"),model="random", effect="time",lag=T, spatial.error = "none") # can only do FE if nblist has no zero links
  test2<-sphtest(mod1, mod2)
  summary(test2)

  #fm2 = HuDac4 ~ 0+HuDaclag+HuDaclagSqr+HuDaclagQb  +Slope_mn+WaterTT  + as.numeric(time) 
  #fm2 = HuDac4 ~ HuDaclag +HuDaclagSqr+HuDaclagQb+Slope_mn +WaterTT +DD6585+factor(time)  
  # maybe better   fm2 = HuDac4 ~ HuDaclag +HuDaclagSqr+HuDaclagQb+Slope_mn +Slope_mnSqr+WaterTT+WaterTTSqr +DD6585+factor(time)  
  # very good   fm2 = HuDac4 ~  HuDaclag +HuDaclagSqr+HuDaclagQb+ Slope_mn +WaterTT +DD6585+factor(time)  
  # good  fm2 = HuDac4 ~  HuDaclag +HuDaclagSqr+HuDaclagQb+ Slope_mn +IrrgTT+PP30kTT+ PrimSecRd+factor(time)  
  # fm2 = HuDac4 ~  HuDaclag +HuDaclagSqr+HuDaclagQb+ Slope_mn +IrrgTT+ OceanTT+WaterTT +factor(time)  
  # fm2 = HuDac4 ~  HuDaclag +HuDaclagSqr+HuDaclagQb+ Slope_mn +IrrgTT+ OceanTT+WaterTT+AllRoads +factor(time)  
  #not as.integer(POP100)  Rough_mn factor(COUNTY)
  # fm2 = HuDac4 ~ log(HuDaclag+1) +Slope_mn +Slope_mnSqr+WaterTT+WaterTTSqr +DD6585+factor(time)  




  ############  PLOT RESULTS #############
  trellis.par.set(sp.theme())
  resd = data.frame(resd =b$residual[names(b$residual ) %in% paste(1:NSAMPLES,"-99", sep="")] )
  points= SpatialPointsDataFrame(coords=coordinates(Block.39_sp),data=resd)
  class <- classIntervals(points$resd, n=7, style = "quantile")
  spplot(points,"resd", cuts=class$brks )


  resd = data.frame(resd =c$residual[names(c$residual ) %in% paste(1:NSAMPLES,"-99", sep="")] )
  points= SpatialPointsDataFrame(coords=coordinates(Block.39_sp),data=resd)
  class <- classIntervals(points$resd, n=7, style = "quantile")
  spplot(points,"resd", cuts=class$brks )

  resd = data.frame(resd =d$residual[names(d$residual ) %in% paste(1:NSAMPLES,"-99", sep="")] )
  points= SpatialPointsDataFrame(coords=coordinates(Block.39_sp),data=resd)
  class <- classIntervals(points$resd, n=7, style = "quantile")
  spplot(points,"resd", cuts=class$brks )










##########################################################################################################
################## JACKNIFE CROSS SECTION OUT OF SAMPLE  ########################################################################
##########################################################################################################

  ### IMPORT POLYGONS AND ITS DBF FILE
  Block.39.ex = readOGR("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090","BG_splt_090_Exclude")
  Block.dbf = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_splt_090_Exclude.dbf")   #read dbf file for edit

  ###  LIMIT TO COUNTIES 
  counties = c(37,33,30,13)
  Block.39_sp = Block.39.ex[Block.39.ex$County %in% counties,]
  Block.39.dbf_sp = Block.dbf[Block.dbf$County %in% counties,]

 ### create smaller sample   
  NSAMPLES = 1000   
  set.seed(123)
  Block.39_jk = Block.39_sp[Block.39_sp$SUM090 %in% sample(Block.39_sp$SUM090, NSAMPLES),]
  set.seed(123)
  Block.39.dbf_jk = Block.39.dbf_sp[Block.39.dbf_sp$SUM090 %in% sample(Block.39.dbf_sp$SUM090, NSAMPLES),]
  Block.39.dbf_jk$ordersp = 1:length(Block.39.dbf_jk$SUM090)


  ### GET NEIGHBORS HOUSING DENSITY FOR REGRESSION
  getAvDen_39_109(Block.39_jk,39,109)         # should be (39,109) pass function(Block.39_sp4,startyear,endyear) get nbHuDac4_all(dataframe of neigh av density) and dbn(nb list, with distance band)
  nbHuDac4_all[is.na(nbHuDac4_all)]=0        # remove NAs
  Block.39.dbf_jk = merge(Block.39.dbf_jk,nbHuDac4_all, by="SUM090" )         # merge in nb housing density
  Block.39.dbf_jk = Block.39.dbf_jk[with(Block.39.dbf_jk, order(ordersp) ), ]

  ################################################
  ######   PUT DATA INTO PANEL LONG        ######
  panel.data= reshape(Block.39.dbf_jk, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_jk)),grep("Cum",names(Block.39.dbf_jk)),grep("DD100_",names(Block.39.dbf_jk)), grep("DD6585_",names(Block.39.dbf_jk)), grep("DD95_",names(Block.39.dbf_jk)) , grep("nbHuD4_",names(Block.39.dbf_jk)) ) , sep = "_")
  panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  #summary(panel.data)
  #by(panel.data$HuDac4, panel.data$time, mean )
  pdim(panel.data)
  
  ### CREATE NEW VARIABLES
  panel.data$HuDaclag = lag(panel.data$HuDac4, 1) 
  panel.data$HuDaclagSqr = panel.data$HuDaclag^2 
  panel.data$HuDaclagQb = panel.data$HuDaclag^3 
  panel.data$HuDaclag2 = lag(panel.data$HuDac4, 2) 
  panel.data$AllRoadsSqr = panel.data$AllRoads^2 
  panel.data$Rough_mnSqr = panel.data$Rough_mn^2 
  panel.data$DD6585Sqr = panel.data$DD6585^2 
  panel.data$Slope_mnSqr = panel.data$Slope_mn^2 
  panel.data$WaterTTSqr = panel.data$WaterTT^2 
  panel.data$nbHuD4lag = lag(panel.data$nbHuD4, 1)
 
  panel.data2 = data.frame(subset(panel.data, select=c(id,time)), subset(panel.data, select=-c(id,time)))   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  panel.data2 = panel.data2[panel.data2$time %in% 49:99, ]

#########################################################
  uniq = as.numeric(as.character(unique(panel.data2$id)))
for (i in 1:length(unique(panel.data2$id))){
  sampleit = uniq[uniq != i]
  SmpPdIn  = panel.data2[panel.data2$id %in% sampleit,]
  SmpPdInPoly = Block.39_sp[Block.39_sp$SUM090 %in% unique(SmpPdIn$SUM090),]
  SmpPdOut = panel.data2[panel.data2$id %in% i,]
  
  ### neighbors 
  distanceband = 1000
  block.nb4=dnearneigh(coordinates(SmpPdInPoly), 0, distanceband, longlat = FALSE, row.names=SmpPdInPoly$SUM090) #3000
  W4 = nb2listw(block.nb4,style="W", zero.policy=TRUE)
  
  ### regressions
  fm2 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb +Slope_mn+WaterTT # + as.numeric(time)  #factor(time) +DD6585
  b=spml(fm2, data= SmpPdIn, listw=W4,index=c("id","time"), model="pooling", lag=T, spatial.error = "none") #spatial.error="b", lag=FALSE)
 
  }

forecast.jk.out = function(dataoutsmp,formula,results, constant){
  form1 = as.character(formula)                                        #extract formula used
  form = paste(form1[2],form1[1],form1[3], " + nbHuD4",sep=" ")      #append nb density 
  Xs  = model.frame(as.formula(form),dataoutsmp, na.action=NULL)
  Xs  = subset(Xs, select=-c(HuDac4))                             # remove Y
  if (constant == T) {Xs= data.frame(Intercept=1, Xs)}
  b   = results
  Bs  = as.vector(c(b$coefficients, b$arcoef))                    # get coefficients + spatial coef
  XBs = sweep(Xs,MARGIN=2,Bs,`*`)                                 # apply coefficients to Xs
  P1  = data.frame(P1=rowSums(XBs))                               # non-spatial yhat
  den     = subset(dataoutsmp, select=c(SUM090,time,id,HuDac4))
  Preds   = merge(P1, den, by="row.names", incomparables=NA)
  Preds$E1 = Preds$HuDac4-Preds$P1         
}






##########################################################################################################
################## CROSS SECTION OUT OF SAMPLE  ########################################################################
##########################################################################################################

# read http://ftp.iza.org/dp4242.pdf
  ###  LIMIT TO COUNTIES 
  counties = c(37,33,30,13)
  Block.39_sp4 = Block.39.ex[Block.39.ex$County %in% counties,]
  Block.39.dbf_sp4 = Block.dbf[Block.dbf$County %in% counties,]

  ### get neigh av density levels 
  getAvDen_39_109(Block.39_sp4,39,109)         # should be (39,109) pass function(Block.39_sp4,startyear,endyear) get nbHuDac4_all(dataframe of neigh av density) and dbn(nb list, with distance band)
  
  #getAvDen_START = function(polys, distanceband, data1){
  #getAvDen_START(Block.39_sp4, 1000, Block.39.dbf_sp4)

  nbHuDac4_all[is.na(nbHuDac4_all)] = 0        # replace NA from no nb with 0
  Block.39.dbf_sp4 = merge(Block.39.dbf_sp4 , nbHuDac4_all, by="SUM090", all.x=T, all.y=F, incomparables=NA)

  ################################################
  ######   PUT DATA INTO PANEL LONG        ######

  panel.data4 = reshape(Block.39.dbf_sp4, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_sp4)),grep("Cum",names(Block.39.dbf_sp4)),grep("DD100_",names(Block.39.dbf_sp4)), grep("DD6585_",names(Block.39.dbf_sp4)), grep("DD95_",names(Block.39.dbf_sp4))  , grep("nbHuD4",names(Block.39.dbf_sp4))  ) , sep = "_")
  panel.data4 = pdata.frame(panel.data4, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  #summary(panel.data4)
  #by(panel.data4$HuDac4, panel.data4$time, mean )
  #pdim(panel.data4)
  
  ### CREATE NEW VARIABLES
  panel.data4$HuDaclag = lag(panel.data4$HuDac4, 1) 
  panel.data4$HuDaclagSqr = panel.data4$HuDaclag^2 
  panel.data4$HuDaclagQb = panel.data4$HuDaclag^3 
  panel.data4$AllRoadsSqr = panel.data4$AllRoads^2 
  panel.data4$Rough_mnSqr = panel.data4$Rough_mn^2 
  panel.data4$DD6585Sqr = panel.data4$DD6585^2 
  panel.data4$Slope_mnSqr = panel.data4$Slope_mn^2 
  panel.data4$WaterTTSqr = panel.data4$WaterTT^2
  

  ### CONVERT TO DATA FRAME (NEEDED FOR SPATIAL PANEL)
  panel.data4 = data.frame(subset(panel.data4, select=c(id,time)), subset(panel.data4, select=-c(id,time))   )   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  panel.data4 = panel.data4[panel.data4$time %in% 49:109, ]
     
  ### ADD ROWS FOR FORECAST 
  #addyears = 1
  #addmoreyears(addyears)                                          # function adds additional years to panel data


  ### EXTRACT RELEVANT DATA AND APPLY COEFFICIENTS
  form = as.character(fm2)                                        #extract formula used
  form = paste(form[2],form[1],form[3], " + nbHuD4",sep=" ")      #append nb density 
  Xs  = model.matrix( as.formula(form) , panel.data4)             # get all relevant data
  Bs  = as.vector(c(b$coefficients, b$arcoef))                    # get coefficients + spatial coef
  XBs = sweep(Xs,MARGIN=2,Bs,`*`)                                 # apply coefficients to Xs
  P1  = rowSums(XBs)                                              # non-spatial yhat
  # results = summary(b)                                          # use to extract t-values, p-values, R2 or other stats  

  ###  COMPARE PREDS 
  den = subset(panel.data4, select=c(SUM090,time,id,HuDac4))
  Preds = merge(P1, den, by="row.names")
  Preds$E1 = Preds$HuDac4-Preds$x          

  ### DISPLAY RESIDS 
  hist(Preds$E1[Preds$E1<4 & Preds$E1> -4 ], breaks=30)
  hist(Preds$HuDac4[Preds$HuDac4<20], breaks=30)

  ### PLOT RESIDUALS OF OUT OF SAMPLE  
    ### RESET LIMIT TO COUNTIES  
    Block.39_sp4 = Block.39.ex[Block.39.ex$County %in% counties,]
    Block.39.dbf_sp4 = Block.dbf[Block.dbf$County %in% counties,]

  #E1yr = Preds[Preds$time %in% 49,]                                                          #LIMIT TO RELEVANT YEAR
  E1mn = aggregate( E1 ~ SUM090, data = Preds, FUN="mean", na.rm=TRUE )                       # OR GET AVERAGE (av can be used in prediction Baltagi '10')
  Block.39.dbf_sp5 = merge(Block.39.dbf_sp4, E1mn, by="SUM090", all.x=T,  incomparables=NA)   #BRING PREDICTIONS TO POLYGON DBF
  Block.39.dbf_sp5$INTPTLAT = as.numeric(as.character(Block.39.dbf_sp5$INTPTLAT))                #CONVERT FACTOR->NUMERIC
  Block.39.dbf_sp5$INTPTLON = as.numeric(as.character(Block.39.dbf_sp5$INTPTLON))
  acord = coordinates(Block.39_sp4)

  points= SpatialPointsDataFrame(coords=acord, data=as.data.frame(Block.39.dbf_sp5))
  class <- classIntervals(points$E1, n=8, style = "sd")

  plot.new()
  trellis.par.set(sp.theme())
  p1 = spplot(points,"E1", cuts=class$brks ,scales=list(draw=T))
  print(p1, position =c(0,.2,1,1),more=T)
  par(new=T)
  par(plt = c(0.1,1,0.1,0.45))
  hist(points$E1, breaks=160, main=NULL, xlab="", ylab="")







########################################################################################
##########################  TIME OUT OF SAMPLE  ########################################
########################################################################################
# read http://ftp.iza.org/dp4242.pdf
 
 ###  LIMIT TO COUNTIES 
  counties = c(37,33,30,13)
  Block.39_sp = Block.39.ex[Block.39.ex$County %in% counties,]
  Block.39.dbf_sp = Block.dbf[Block.dbf$County %in% counties,]
  Block.39.dbf_sp$ordersp = 1:length(Block.39.dbf_sp$SUM090)

  ### neighbors 
  distanceband = 1000
  block.nb4=dnearneigh(coordinates(Block.39_sp), 0, distanceband, longlat = FALSE, row.names=Block.39_sp$SUM090) #3000
  W4 = nb2listw(block.nb4,style="W", zero.policy=TRUE)

  KNN4=knn2nb(knearneigh(coordinates(Block.39_sp),k=12))   #6
  WKNN4 = nb2listw(KNN4, style="W")
 
  ### GET NEIGHBORS HOUSING DENSITY FOR REGRESSION
  panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp))  ) , sep = "_")
  panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
 
           #getAvDen_START = function(polys, distanceband, data1, startyear,endyear){  # returns nbHuDac4_all
  getAvDen_START(Block.39_sp, distanceband, panel.data, 39,109 )
  Block.39.dbf_sp = merge(Block.39.dbf_sp,nbHuDac4_all, by="SUM090" )         # merge in nb housing density
  Block.39.dbf_sp = Block.39.dbf_sp[with(Block.39.dbf_sp, order(ordersp) ), ]

  ################################################
  ######   PUT DATA INTO PANEL LONG        ######
  panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp)) , grep("nbHuD4_",names(Block.39.dbf_sp)) ) , sep = "_")
  panel.data <-  pdata.frame(panel.data, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  #summary(panel.data)
  #by(panel.data$HuDac4, panel.data$time, mean )
  pdim(panel.data)
  
  ### CREATE NEW VARIABLES

add_panel_variables  = function(data){
    data$HuDaclag    = lag(data$HuDac4, 1) 
    data$HuDaclagSqr = data$HuDaclag^2 
    data$HuDaclagQb  = data$HuDaclag^3 
    data$HuDaclag2   = lag(data$HuDac4, 2) 
    data$AllRoadsSqr = data$AllRoads^2 
    data$Rough_mnSqr = data$Rough_mn^2 
    data$DD6585Sqr   = data$DD6585^2 
    data$Slope_mnSqr = data$Slope_mn^2 
    data$WaterTTSqr  = data$WaterTT^2 
    data$nbHuD4lag   = lag(data$nbHuD4,1)
    return(data)
}

  panel.data = add_panel_variables(panel.data)
  ### CONVERT TO DATA FRAME (NEEDED FOR SPATIAL PANEL)
  panel.data4 = data.frame(subset(panel.data, select=c(id,time,SUM090)), subset(panel.data, select=-c(id,time,SUM090))   )   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  forecastit(panel.data4,fm2,b, constant=T )                               # predict for 109
  getAvNbDen_PRED(Block.39_sp, distanceband=1000, panel.data4) 

  addpredyear(1) #only do 1 time  accidently removes SUM090
  forecastit(panel.data4,fm2,b, constant=T )
  getAvNbDen_PRED(Block.39_sp, distanceband=1000, panel.data4 )
  ###  order should be rep(forcastit, recalc av density, addpredyear)


#
  



##############################################################################################################
###############  Functions  ##################################################################################
##############################################################################################################

##  WORKING DON'T MESS
forecastit = function(data,formula,results, constant){
  ### MAKE OUT OF SAMPLE PRED BASED ON MISSING VALUES IN HuDac4
  ### NOTE:   NEIGHBORHOOD EFFECT is ARCOEF*nbHuD4lag (LAG!!!)
  ### EXTRACT RELEVANT DATA AND APPLY COEFFICIENTS
  form1 = as.character(formula)                                        #extract formula used
  form = paste(form1[2],form1[1],form1[3], " + nbHuD4lag",sep=" ")      #append nb density 
  Xs  = model.frame(as.formula(form),data, na.action=NULL)
  Xs  = subset(Xs, select=-c(HuDac4))                             # remove Y
  if (constant == T) {Xs= data.frame(Intercept=1, Xs)}
  b   = results
  Bs  = as.vector(c(b$coefficients, b$arcoef))                    # get coefficients + spatial coef
  XBs = sweep(Xs,MARGIN=2,Bs,`*`)                                 # apply coefficients to Xs
  P1  = data.frame(P1=rowSums(XBs))                               # non-spatial yhat

  ###  COMPARE PREDS 
  den     = subset(data, select=c(SUM090,time,id,HuDac4))
  Preds   = merge(P1, den, by="row.names", incomparables=NA)
  Preds$E1 = Preds$HuDac4-Preds$P1         
  E1mn = aggregate( E1 ~ SUM090, data = Preds, FUN="mean", na.rm=TRUE )   # GET MEAN ERROR (av can be used in prediction Baltagi '10')
  names(E1mn) = c("SUM090","E1mn")
  Preds = merge(Preds,E1mn,by="SUM090", all.x=T, incomparables=NA)
  
  ###  MAKE OUT OF SAMPLE PRED BASED ON MISSING VALUES IN HuDac4
  Preds$HuDac4[is.na(Preds$HuDac4)] = Preds$P1[is.na(Preds$HuDac4)] + Preds$E1mn[is.na(Preds$HuDac4)]
  Preds = Preds[with(Preds, order(id,time) ), ]
  row.names(Preds) = Preds$Row.names

  ### MERGE PREDICTED HUDAC4 WITH ORIGINAL DATA
  data = merge(subset(data,select=-c(HuDac4)), subset(Preds, select=c(HuDac4)), by="row.names", incomparables=NA   )
  data = data[with(data, order(id,time) ), ]
  data$HuDac4[data$HuDac4<0] = 0                               # don't allow negative preds
  row.names(data) = paste(data$id, data$time, sep="-")
  data = subset(data, select=-c(Row.names))
  panel.data4 <<- data
}


#######################################################
# pass polygons start year and end year, get neighbor densities 
getAvDen_39_109 = function(Block.39_sp4,startyear,endyear){
    dbn <<- dnearneigh(coordinates(Block.39_sp4), 0, distanceband, longlat = FALSE, row.names=Block.39_sp4$SUM090)  
    a = dbn
    b = Block.39_sp4@data
    out = data.frame(SUM090=b[["SUM090"]])
    j=2
    for (i in seq(startyear,endyear,by=10)){
      # works but don't use - assign( paste("nbHuD4_",i,sep=""), unlist(lapply(a, function(x) mean( b[[paste("HuDac4_",i,sep="")]][x], na.omit=TRUE))) ) # assign average neigh density for yr      
      out[,j]= unlist(lapply(a, function(x) mean( b[[paste("HuDac4_",i,sep="")]][x], na.omit=TRUE)))
      j = j+1
      }
    names(out) = c("SUM090", paste("nbHuD4_",seq(startyear,endyear,by=10),sep=""))
    nbHuDac4_all <<- out
  }


#######################################################
# pass polygons start year and end year, get neighbor densities 
# add results to data1
# WORKING DON'T MESS!
 
getAvNbDen_PRED = function(polys, distanceband, data1){
    startyear = min(data1$time,na.rm=T)
    endyear = max(data1$time,na.rm=T)
    a = dnearneigh(coordinates(polys), 0, distanceband, longlat = FALSE, row.names=polys$SUM090)  
    pb = polys@data                                                #get poly data so that row#s match new panel.data
    pb = data.frame(polyorder = 1:length(pb$SUM090), subset(pb, select=c(SUM090)) )     #get SUM090                            # just keep unique id for merge
    d = data1                                                          # get panel data
    if(class(d)[1]=="data.frame") {  d = pdata.frame(d, c("id","time"), drop.index=FALSE)  }  # declare panel data individ and time      
    d = as.data.frame( reshape(d, dir = "wide") )
    d = merge(pb, d, by.x="SUM090", by.y="SUM090.39",  incomparables=NA)
    d = d[with(d, order(polyorder) ), ]         # IMPORTANT!               # resort to be in same order as polygons and neigh 
    row.names(d) = d$polyorder                                             # just in case
    names(d)[1] = "SUM090.39"   
    out = data.frame(SUM090=d[["SUM090.39"]])                          #PLACE HOLDER
    j=2                                                                #avoid overwriting SUM090
    for (i in seq(startyear,endyear,by=10)){
      paste("now working on year",i,sep=" ")                           # create table of neighboring densities
      out[,j]= unlist(lapply(a, function(x) mean( d[[paste("HuDac4.",i,sep="")]][x], na.omit=TRUE)))    #get average density of neighbors (doesn't work for out of sample)
      j = j+1
      }
    names(out) = c("SUM090", paste("nbHuD4.",seq(startyear,endyear,by=10),sep=""))
    # replace missing data with NA and zero neighbor with 0 nbdensity 
    out[is.na(out)] = 0                                               # remove inf, if no neighbors are present
 
    dp = reshape(d, dir = "long", grep("\\." ,names(d))  )            # reshape both to make merge easier
    dp = subset(dp, select=-c(nbHuD4,SUM090))
    row.names(dp) = paste(dp$id,dp$time,sep=".")
    dp = dp[with(dp, order(id,time) ), ]
    outp = reshape(out, dir = "long", grep("\\." ,names(out))  )
    outp = outp[with(outp, order(id,time) ), ]
    row.names(outp) = paste(outp$id,outp$time,sep=".")
    
    
    d = merge(dp, subset(outp, select=-c(id,time)), by="row.names")   # now repalcing nb density with new 
    d = d[with(d, order(id,time) ), ]          
    row.names(d) = paste(d$id,d$time,sep=".")
    d = cbind( subset(d, select=c(id,time,SUM090,polyorder)),subset(d,select=-c(Row.names, SUM090,id,time,polyorder)))
    
    nbHuDac4_all <<- out 
    panel.data4  <<- d 
  }



#######################################################
# pass polygons start year and end year, get neighbor densities 

# WORKING DON'T MESS!
getAvDen_START = function(polys, distanceband, data1){
    startyear = min(data1$time,na.rm=T)
    endyear = max(data1$time,na.rm=T)
    a = dnearneigh(coordinates(polys), 0, distanceband, longlat = FALSE, row.names=polys$SUM090)  
    b = polys@data                                                #get poly data so that row#s match new panel.data
    b = data.frame(order = 1:length(b$SUM090), subset(b, select=c(SUM090)) )                                   # just keep unique id for merge
    d = data1
    if(class(d)[1]=="data.frame") {  d = pdata.frame(d, c("id","time"), drop.index=FALSE)  }  # declare panel data individ and time      
    d = as.data.frame( reshape(d, dir = "wide") )
    d = merge(b, d, by.x="SUM090", by.y="SUM090.39",  incomparables=NA)
    d = d[with(d, order(order) ), ]                                    # resort to be in same order as polygons and neigh 
    row.names(d) = d$order                                             # just in case
    names(d)[1] = "SUM090.39"   
    out = data.frame(SUM090=d[["SUM090.39"]])                          #PLACE HOLDER
    j=2                                                                #avoid overwriting SUM090
    for (i in seq(startyear,endyear,by=10)){
      paste("now working on year",i,sep=" ")
      
            # should be HuDac4_LAGGED for pred 
      out[,j]= unlist(lapply(a, function(x) mean( d[[paste("HuDac4.",i,sep="")]][x], na.omit=TRUE)))    #get average density of neighbors (doesn't work for out of sample)
      j = j+1
      }
    names(out) = c("SUM090", paste("nbHuD4_",seq(startyear,endyear,by=10),sep=""))
    # replace missing data with NA and zero neighbor with 0 nbdensity 
    out[is.na(out)] = 0 
    out[,dim(out)[2]] = NA
    nbHuDac4_all <<- out
  }



#######################################################
addmoreyears = function(addyears){
  if (addyears==0){return(NULL)} 
  # time invariant variables stored in "tvarient" above 
  # add the additional observations 'addyears' then merge orginal data
  for (i in 1:addyears){   
    time = rep(c(unique(as.numeric(as.character(panel.data4$time))),max(as.numeric(as.character(panel.data4$time)))+10), length(unique(panel.data4$id)))
    id   = rep( unique(panel.data4$id), each=max(length(unique(time))))
    data2 = data.frame(time, id)
    panel.data4 = merge(data2, panel.data4, all.x=T)
    panel.data4 = panel.data4[with(panel.data4, order(id,time) ), ]
  }
  # convert data from long to wide in order to replace missing values from time invariant variables
  set.seed(10)
  samples  = sample(panel.data4$id,200)                                               # SAMPLE B/C PVAR TAKE A LONG TIME
  tvarient = pvar(na.omit(panel.data4[panel.data4$id %in% samples,]))$id.variation   # get time variant again
  panel.data4 = pdata.frame(panel.data4, c("id","time"), drop.index=FALSE)            # declare panel data individ and time
  panel.data4 = reshape(panel.data4, dir = "wide")                                    # convert to wide to make replacing NAs easier

  for (j in seq(109,  (109+addyears*10),by=10)  ){
      panel.data4[,grep(j+10,names(panel.data4))] = panel.data4[,grep(j,names(panel.data4))]  # replace 99+ Nas with previous year
    }

  panel.data4 = reshape(panel.data4, dir = "long")                            # convert back to long
  names(panel.data4) = apply(data.frame(strsplit(names(panel.data4), split="\\."))[1,], 2, function(x) as.character(x))   # remove .39 from names
  panel.data4 = panel.data4[with(panel.data4, order(id,time) ), ]              # resort
  panel.data4[as.numeric(as.character(panel.data4$time))>=109,tvarient & names(panel.data4)!="id"]  = NA # put back NAs in time varying 

  panel.data4 <<- panel.data4    # assign to global env. 
}
  
 

#######################################################
# WORKING DON'T MESS 
addpredyear = function(addyears){
  # NOTE: DATA EXPANDED TIME VARIENT AND TIME INVARIENT KEPT AT PREVIOUS VALUE
    
  if (addyears==0){return(NULL)} 
  # time invariant variables stored in "tvarient" above 
  # add the additional observations 'addyears' then merge orginal data
    maxyear = max(as.numeric(as.character(panel.data4$time)))                         # save for later

  for (i in 1:addyears){   
    time = rep(c(unique(as.numeric(as.character(panel.data4$time))),max(as.numeric(as.character(panel.data4$time)))+10), length(unique(panel.data4$id)))
    id   = rep(unique(panel.data4$id), each=max(length(unique(time))))
    data2 = data.frame(time, id)
    panel.data4 = merge(data2, panel.data4, all.x=T)
    panel.data4 = panel.data4[with(panel.data4, order(id,time) ), ]
    row.names(panel.data4) = paste(panel.data4$id,panel.data4$time,sep=".")
  }

  # convert data from long to wide in order to replace missing values from time invariant variables
  set.seed(10)
  samples  = sample(panel.data4$id,200)                                               # SAMPLE B/C PVAR TAKE A LONG TIME
  tvarient = pvar(na.omit(panel.data4[panel.data4$id %in% samples,]))$id.variation    # get time variant again
  panel.data4 = pdata.frame(panel.data4, c("id","time"), drop.index=FALSE)            # declare panel data individ and time
  
  # convert to wide
  panel.data5 = reshape(panel.data4, dir = "wide", timevar="time", idvar="id")                                    # convert to wide to make replacing NAs easier
  
  for (j in seq(maxyear,(maxyear+addyears*10),by=10)  ){
      # FILL YEARS WITH NO VALUES WITH T-1 
      panel.data5[,grep(j+10,names(panel.data5))] = panel.data5[,grep(j,names(panel.data5))]  # replace 99+ Nas with previous year
    }

  panel.data5 = reshape(panel.data5, dir = "long", sep=".")                                    # convert back to long
  panel.data5 = pdata.frame(as.data.frame(panel.data5), c("id","time"), drop.index=FALSE)      # declare panel data DOESN'T WORK WITHOUT

  h =  strsplit(names(panel.data5), split="\\.")
  if (h[[3]][1] == "Row") {h[[3]] = c("Row.names", "39")  }             # avoid problem with string split of Row.names on "."                                       # replace b/c Row.names is split to "Row" "names"
  names(panel.data5) = apply(as.data.frame(h)[1,], 2, function(x) as.character(x))   # remove .39 from names
  panel.data5 = panel.data5[with(panel.data5, order(id,time) ), ]             # resort

  # recalculate lags and squares 
  panel.data5 = add_panel_variables(panel.data5)                              # replace lags etc

 # panel.data5$Row.names = paste(panel.data5$id, panel.data5$time, sep="-")
  panel.data5$HuDac4[panel.data5$time == (maxyear+10)]  = NA                  # make sure that this is NA b/c prediction function looks for missing values to fill 
  panel.data4 <<- as.data.frame( panel.data5 )    # assign to global env. 
}
  


##############################################################################################################
###############  End Functions  ##################################################################################
##############################################################################################################












  








########################################################################################
##########################  TIME OUT OF SAMPLE  SAVING ########################################
########################################################################################
# read http://ftp.iza.org/dp4242.pdf
  ###  LIMIT TO COUNTIES 
  counties = c(37,33,30,13)
  Block.39_sp4 = Block.39.ex[Block.39.ex$County %in% counties,]
  Block.39.dbf_sp4 = Block.dbf[Block.dbf$County %in% counties,]

  ### get neigh av density levels 
  getAvDen_39_109(Block.39_sp4,39,109)         # should be (39,109) pass function(Block.39_sp4,startyear,endyear) get nbHuDac4_all(dataframe of neigh av density) and dbn(nb list, with distance band)
  nbHuDac4_all[is.na(nbHuDac4_all)] = 0        # replace NA from no nb with 0
  Block.39.dbf_sp4 = merge(Block.39.dbf_sp4 , nbHuDac4_all, by="SUM090", all.x=T, all.y=F, incomparables=NA)

  ################################################
  ######   PUT DATA INTO PANEL LONG        ######

  panel.data4 = reshape(Block.39.dbf_sp4, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_sp4)),grep("Cum",names(Block.39.dbf_sp4)),grep("DD100_",names(Block.39.dbf_sp4)), grep("DD6585_",names(Block.39.dbf_sp4)), grep("DD95_",names(Block.39.dbf_sp4))  , grep("nbHuD4",names(Block.39.dbf_sp4))  ) , sep = "_")
  panel.data4 = pdata.frame(panel.data4, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
  #summary(panel.data4)
  #by(panel.data4$HuDac4, panel.data4$time, mean )
  #pdim(panel.data4)
  
  ### CREATE NEW VARIABLES
  panel.data4$HuDaclag = lag(panel.data4$HuDac4, 1) 
  panel.data4$HuDaclagSqr = panel.data4$HuDaclag^2 
  panel.data4$HuDaclagQb = panel.data4$HuDaclag^3 
  panel.data4$AllRoadsSqr = panel.data4$AllRoads^2 
  panel.data4$Rough_mnSqr = panel.data4$Rough_mn^2 
  panel.data4$DD6585Sqr = panel.data4$DD6585^2 
  panel.data4$Slope_mnSqr = panel.data4$Slope_mn^2 
  panel.data4$WaterTTSqr = panel.data4$WaterTT^2
  
  ### ADD ROWS FOR FORECAST 
  addyears = 1
  addmoreyears(addyears)                                          # function adds additional years to panel data

  ### CONVERT TO DATA FRAME (NEEDED FOR SPATIAL PANEL)
  panel.data4 = data.frame(subset(panel.data4, select=c(id,time)), subset(panel.data4, select=-c(id,time))   )   # CANT be in panel data dataframe                                     # convert back to data.frame as per example
  panel.data4 = panel.data4[panel.data4$time %in% 49:119, ]
 

  ### EXTRACT RELEVANT DATA AND APPLY COEFFICIENTS
  form1 = as.character(fm2)                                        #extract formula used
  form = paste(form1[2],form1[1],form1[3], " + nbHuD4",sep=" ")      #append nb density 
  Xs  = model.frame(as.formula(form),panel.data4, na.action=NULL)
  Xs  = subset(Xs, select=-c(HuDac4))                             # remove Y
  Bs  = as.vector(c(b$coefficients, b$arcoef))                    # get coefficients + spatial coef
  XBs = sweep(Xs,MARGIN=2,Bs,`*`)                                 # apply coefficients to Xs
  P1  = data.frame(P1=rowSums(XBs))                               # non-spatial yhat

  ###  COMPARE PREDS 
  den     = subset(panel.data4, select=c(SUM090,time,id,HuDac4))
  Preds   = merge(P1, den, by="row.names")
  Preds$E1 = Preds$HuDac4-Preds$P1         
  E1mn = aggregate( E1 ~ SUM090, data = Preds, FUN="mean", na.rm=TRUE )                       # OR GET AVERAGE (av can be used in prediction Baltagi '10')
  names(E1mn) = c("SUM090","E1mn")
  Preds = merge(Preds, E1mn,by="SUM090")
  
  ###  MAKE OUT OF SAMPLE PRED 
  Preds$HuDac4[is.na(Preds$HuDac4)] = Preds$P1[is.na(Preds$HuDac4)] + Preds$E1mn[is.na(Preds$HuDac4)]
  Preds = Preds[with(Preds, order(id,time) ), ]






  ### DISPLAY RESIDS 
  #hist(Preds$E1[Preds$E1<4 & Preds$E1> -4 ], breaks=30)
  #hist(Preds$HuDac4[Preds$HuDac4<20], breaks=30)

  ### PLOT RESIDUALS OF OUT OF SAMPLE  
    ### RESET LIMIT TO COUNTIES  
  #  Block.39_sp4 = Block.39.ex[Block.39.ex$County %in% counties,]
  #  Block.39.dbf_sp4 = Block.dbf[Block.dbf$County %in% counties,]

  #E1yr = Preds[Preds$time %in% 49,]                                                          #LIMIT TO RELEVANT YEAR
  #E1mn = aggregate( E1 ~ SUM090, data = Preds, FUN="mean", na.rm=TRUE )                       # OR GET AVERAGE (av can be used in prediction Baltagi '10')
  #Block.39.dbf_sp5 = merge(Block.39.dbf_sp4, E1mn, by="SUM090", all.x=T,  incomparables=NA)   #BRING PREDICTIONS TO POLYGON DBF
 


























test = panel.data4[as.numeric(as.character(panel.data4$id)) %in% c(1,2),]
  set.seed(10)
  samples  = sample(test$id,2)                           # SAMPLE B/C PVAR TAKE A LONG TIME
  tvarient = pvar(na.omit(test[test$id %in% samples,]))$time.variation
 
test = pdata.frame(test, c("id","time"), drop.index=FALSE)    # declare panel data individ and time
test4 = reshape(test, dir = "wide")

for (j in seq(99, (max(as.numeric(as.character(test$time)))-10),by=10)  ){
    test4[,grep(j+10,names(test4))] = test4[,grep(j,names(test4))]
  }

test4 = reshape(test4, dir = "long")
names(test4) = apply(data.frame(strsplit(names(test4), split="\\."))[1,], 2, function(x) as.character(x))
 
test4 = test4[with(test4, order(id,time) ), ]    
test4[as.numeric(as.character(test4$time))>=109,!tvarient]  = NA



head(test4)


test4 = test4[by(test4, 2,order(names(test4)))]
panel.data4 = pdata.frame(panel.data4, c("id","time"), drop.index=FALSE)    # declare panel data individ and time







test$missing = is.na(test)[,"SUM090"] | test$time == 109

a= by(test, as.numeric(as.character(test$id)),mean, na.rm=T)
hold =  test[test$missing,]


fillout = function(test){
  add = -1*addyears
  if(is.na(test$SUM090)){
      for (i in seq(-1,add)){
        
   
        }       
  }
}




table(panel.data4$id)
test[grepl(NA,test$SUM090 ),]
#













 

















#########################  OLD  ###########################################
###########################################################################
###########################################################################
###########################################################################
#########################   CREATE SPATIAL WEIGHTS ###


 Block.39 = readOGR("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\Build Date","HCounts_39")
 names(Block.39)
 counties = c(37,33,30,13)
 Block.39_sp = Block.39[Block.39$County %in% counties,]

plot(Block.39_sp) 
wide= read.table(file="County")
 

 log_smpl = c("0083156","0084165","0081688")
 wide_sp2 = subset(wide, LOGRECNO2 %in% log_smpl)
 head(wide_sp2)
 Block.39_sp = Block.39[Block.39$LOGRECNO %in% log_smpl,]
 plot(Block.39_sp)

 ### select counties of interest
 counties = c(37,33,30,13)
 wide_sp = subset(wide, County_mean %in% counties)
 wide_logrecno = wide_sp$LOGRECNO2
 #Block.dbf = subset(Block.dbf, LOGRECNO %in% wide_logrecno)
 Block.39_sp = Block.39[Block.39$LOGRECNO %in% wide_logrecno,]
 plot(Block.39_sp)
 plot(Block.39, add=TRUE)
 str(Block.39)
 head(Block.39_sp$data)

Block.39 = 

nc <- read.shape(system.file("shapes/sids.shp", package = "maptools")[1])


head(wide_sp)



####################################################################################################
####################################################################################################
####################################################################################################
#########################   CREATE SPATIAL WEIGHTS ###   OLD VERSION 
 
 Block.39 = readOGR("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\Build Date","HCounts_39")
 Block.dbf = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\Build Date\\HCounts_39.dbf")   #read dbf file for edit
 Block.dbf = subset(Block.dbf, select=c(LOGRECNO, zoneFID ))
 names(Block.dbf)
 head(Block.dbf)

 zoneFID = Block.dbf$zoneFID 
 block.nb=poly2nb(Block.39, snap=500, row.names=zoneFID)   #ID is now zonesFID
 W = nb2listw(block.nb,style="W", zero.policy=TRUE)
 

### zoneFID from W matches 
 setwd("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables")
 wide= read.table(file="Final_Variables_FINAL.csv", sep=",", header=TRUE)
  
 wide = subset(wide, select=-c(Num100DD1939))
 panel.data = reshape(wide, idvar="Value", dir = "long", varying = c(  grep("CUM_",names(wide))[1:8], 
	grep("NbHu_",names(wide))[1:8], grep("DN.NbHu_",names(wide)),  
	grep("Num95",names(wide)), grep("Num65",names(wide)), grep("Num100",names(wide)), 
	grep("DN.CUM_",names(wide))  ) , sep = "_")

 ### CREATE NEW VARIABLES
 panel.data$DN.CUMlag = lag(panel.data$DN.CUM, 1) 
 panel.data$roads_meansqr = panel.data$roads_mean^2 

 ### select counties of interest
 counties = c(37,33,30,13)
 panel_sp = subset(panel.data, County_mean %in% counties)
 panel_sp = pdata.frame(panel_sp, index = c("Value","time") )

 ### create smaller sample 
 NSAMPLES = 200    
 panel_sp=panel_sp[panel_sp$Value %in% sample(panel_sp$Value, NSAMPLES),]
 

remove("Block.39","Block.dbf","block.nb","counties","logrecno","Produc","usaww","wide","zoneFID")

################################################
######          RUN REGRESSIONS           ######    
fm = DN.CUM ~ DN.CUMlag  


spml(fm, data= panel_sp, listw=W, model=c("pooling"),
	effect=c("time"), lag=TRUE)


 

data(Produc, package = "Ecdat") 
data(usaww, package = "splm")    # matrix of neighbors 
# id's match Produc state names

Produc <- Produc[Produc$year<1975, ] 
head(Produc) 
usaww[1:10,1:10]
fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unempr
summary(  spfeml(fm, data = Produc, listw = mat2listw(usaww), effects = "spfe", method = "eigen") )










# for (i in panel.data4$id){
  #  holdinv = subset(panel.data4[panel.data4$id==i,tvarient==FALSE] , select=-c(id,time, nbHuD4))               # GET ONE INDIV AN INVARIENT
  #  holdreplace = holdinv[3,]
    #  select data that need to be replaced, are time invarient, have id == i, and are not time or id
  #  panel.data4[panel.data4$id==i,tvarient==F & names(panel.data4) %in% names(holdinv) & !(names(panel.data4) %in% c("id","time"))  ]  =  holdreplace
  #  }



#####################################################################
#####################################################################
#####################################################################




spml2 <- function (formula, data, index = NULL, listw, listw2 = listw, 
    model = c("within", "random", "pooling"), effect = c("individual", 
        "time", "twoways"), lag = FALSE, spatial.error = c("b", 
        "kkp", "none"), ...) 
{
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
    switch(match.arg(model), within = {
        if (lag) {
            model <- switch(match.arg(spatial.error), b = "sarar", 
                kkp = "sarar", none = "lag")
        } else {
            model <- "error"
        }
        effects <- switch(match.arg(effect), individual = "spfe", 
            time = "tpfe", twoways = "sptpfe")
        res <- spfeml(formula = formula, data = data, index = index, 
            listw = listw, listw2 = listw2, model = model, effects = effects, 
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
        res <- spreml2(formula = formula, data = data, index = index, 
            w = listw2mat(listw), w2 = listw2mat(listw2), lag = lag, 
            errors = errors, cl = cl, ...)
    })
    return(res)
    errors <<- errors
}









spreml2 <- function (formula, data, index = NULL, w, w2 = w, lag = FALSE, 
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
    }
    index <- data[, 1]
    tindex <- data[, 2]
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
    X <- model.matrix(formula, data = data)
    y <- model.response(model.frame(formula, data = data))
    names(index) <- row.names(data)
    ind <- index[which(names(index) %in% row.names(X))]
    tind <- tindex[which(names(index) %in% row.names(X))]
    oo <- order(tind, ind)
    X <- X[oo, ]
    y <- y[oo]
    ind <- ind[oo]
    tind <- tind[oo]
    n <- length(unique(ind))
    k <- dim(X)[[2]]
    t <- max(tapply(X[, 1], ind, length))
    nT <- length(ind)
    if (dim(w)[[1]] != n) 
        stop("Non conformable spatial weights")
    balanced <- n * t == nT
    if (!balanced) 
        stop("Estimation method unavailable for unbalanced panels")
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
        }, ols = {
            sarmod2
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
    res <- y - y.hat
    nam.rows <- dimnames(X)[[1]]
    names(y.hat) <- nam.rows
    names(res) <- nam.rows
    model.data <- data.frame(cbind(y, X[, -1]))
    dimnames(model.data)[[1]] <- nam.rows
    type <- "random effects ML"
    sigma2 <- list(one = 3, idios = 2, id = 1)
    spmod <- list(coefficients = RES$betas, arcoef = RES$arcoef, 
        errcomp = RES$errcomp, vcov = RES$covB, vcov.arcoef = RES$covAR, 
        vcov.errcomp = RES$covPRL, residuals = res, fitted.values = y.hat, 
        sigma2 = sigma2, model = model.data, type = type, call = cl, 
        errors = errors, logLik = RES$ll)
    class(spmod) <- "splm"
    return(spmod)
}


sarmod2 <- function (X, y, ind, tind, n, k, t, nT, w, w2, coef0 = 0, hess = FALSE, 
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
     arcoefsarmod <<- arcoef
    betassarmod <<- betas
    assign("arcoefsarmod", arcoef, envir = .GlobalEnv) 
    assign("betassarmod", betas, envir = .GlobalEnv)
    
    return(RES)


}


















