
########################################
########################################
########## PAPER VISUALIZATIONS
rm(list = ls())
install.packages("ggplot2")
library(ggplot2)
install.packages("rgdal")
library(rgdal)
install.packages("foreign")
library(foreign)
install.packages("sp")
library(sp)
install.packages("raster")
library(raster)
install.packages("maptools")
library(maptools)
install.packages("car")
library(car)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("fields")
library(fields)
install.packages("splm")
library(splm)
install.packages("spdep")
library(spdep)
install.packages("maptools")
library(maptools)
install.packages("gpclib")
library(gpclib)
install.packages("gridExtra")
library(gridExtra)
install.packages("snow")
library(snow)

source("G:\\Faculty\\Mann\\Share\\Scripts\\py.Union_analysis.R")

library(ggplot2)
library(rgdal)
library(foreign)
library(sp)
library(raster)
library(maptools)
library(car)
library(RColorBrewer)
library(fields)
library(splm)
library(spdep)
library(gpclib)
library(gridExtra)
library(snow)

options(warn=1)  # print warnings as they occur
#library(snow)
#beginCluster(type="SOCK")
#options(warn=2)  # treat warnings as errors
gpclibPermit() # required for fortify method
unfactor <- function(factors){return(as.numeric(as.character(factors)))}




#############################################################################
# describe effects of housing on agriculture and grazing 
hden_p = readOGR("G://Faculty//Mann//Share//Final_Variables_2//CountyRun//JepsonGroups6//JepsonAreaSpecCity6.9ppnoneTUrbNS+D20km","SpecifyEcoCity6.9ppnoneTUrbNS+D20km")
landcover = raster('G:\\Faculty\\Mann\\Share\\NLCD LandCover/nlcd2006_landcover_CA_se5.tif')
example = raster('G:\\Faculty\\Mann\\Other\\Fire\\Allroads.tif')
#81  Pasture/Hay - areas of grasses, legumes, or grass-legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle. Pasture/hay vegetation accounts for greater than 20% of total vegetation.
#82  Cultivated Crops - areas used for the production of annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and also perennial woody crops such as orchards and vineyards. Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled.


beginCluster(type='SOCK',4)
landcover2 = resample(landcover,example,'ngb' )
endCluster()
agri = landcover2
agri[agri!=82]=NA
agri[agri==82]=1
pasture = landcover2
pasture[pasture!=81]=NA
pasture[pasture==81]=1

windows()  
plot(agri)
plot(hden_p,add=T)
plot(pasture)

beginCluster(type='SOCK',4)
ext = raster::extract(agri,hden_p, fun=sum, na.rm=T )   # sums the number 1080m cells 
extlist = list(ext)
save(extlist,file='G:\\Faculty\\Mann\\Share\\Fire\\agri_v6.RData') #was agri2
endCluster()

# didn't work same as agri?
beginCluster(type='SOCK',4)
remove(ext, extlist)
ext = extract(pasture,hden_p , fun=sum, na.rm=T)   # sums the number 1080m cells 
extlist = list(ext)
save(extlist,file='G:\\Faculty\\Mann\\Share\\Fire\\pasture_v6.RData')
endCluster()
 
load('G:\\Faculty\\Mann\\Share\\Fire\\agri_v6.RData')
hden_p$agri_sum = extlist[[1]]
remove(extlist)
load('G:\\Faculty\\Mann\\Share\\Fire\\pasture_v6.RData')
hden_p$pasture_sum = extlist[[1]]

windows()
hden_p$agri_sum[is.na(hden_p$agri_sum)]=0
hden_p$pasture_sum[is.na(hden_p$pasture_sum)]=0
all_agri = hden_p[hden_p$agri_sum>0 | hden_p$pasture_sum>0 | !is.na(hden_p$agri_sum )| !is.na(hden_p$pasture_sum ) & (hden_p$HuDac4_149-hden_p$HuDac4_99)>0,]
plot(all_agri$agri_sum,(all_agri$HuDac4_149/all_agri$HuDac4_99)) 
(sum(all_agri$agri_sum,na.rm=T)*1080*1080)/(1000*1000) # Num km2 agri experiencing change 
(sum(all_agri$pasture_sum,na.rm=T)*1080*1080)/(1000*1000) # Num km2 pasture experiencing change 
sum(getValues(pasture),na.rm=T) # total actual pasture and ag lands
sum(getValues(agri),na.rm=T)
#percent of land effected (by split block group)
((sum(all_agri$pasture_sum,na.rm=T)*1080*1080)/(1000*1000))/sum(getValues(pasture),na.rm=T)
((sum(all_agri$agri_sum,na.rm=T)*1080*1080)/(1000*1000))/(sum(getValues(agri),na.rm=T))
# % of land cover (row crop ag vs. natural/pastureland) for all areas with growth
all_agri$land_status = NA
all_agri$land_status[all_agri$agri_sum >0] =  'agriculture'
all_agri$land_status[all_agri$pasture_sum >0] =  'pasture'
all_agri$land_status[is.na(all_agri$land_status) ] = 'none'
all_agri$land_status[is.na(all_agri$land_status) ] = 'none'

all_agri$Est_Km2 =all_agri$Est_Acr  / 247.10
all_agri2 = aggregate(Est_Km2~ land_status ,all_agri,sum)
windows()
ggplot(all_agri2, aes(x = factor(land_status), y = Est_Km2)) + geom_bar(stat = "identity")


data_agri = hden_p@data
data_agri$change_den = hden_p$HuDac4_149-hden_p$HuDac4_99
data_agri$agri_D =  data_agri$agri_sum>0
windows()
ggplot(data_agri,aes(factor(agri_D),Acres ))+ geom_boxplot()
+ylim(c(0,100))
 

#############################################################################
# plot person / house ratio historical
 
# SEE PopulationDensityStudy6.R in G://Faculty/Mann/Share/PopulationForecast

############################################################################
## plot histogram of split block group arcres

historical = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5/BG_splt_090_Exclusion6.dbf")


#DOESN"T WORK  create more specific measures of accuracy for 39-99
historical = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5/BG_splt_090_Exclusion6.dbf")
# historical_L = historical[,c(2,75:81) ]
# historical_L = reshape(historical_L, dir = "long" , varying = grep("HuD",names(historical_L)) , sep = "_")
# 
# future     = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.9ppnoneTUrbNS+D20km\\SpecifyEcoCity6.9ppnoneTUrbNS+D20km.dbf") #JepsonAreaSpecCity6.9TUrbNS+D20km\\SpecifyEcoCity6.9TUrbNS+D20km_SM.dbf")
# future_L = future[,c(1,83:89) ]
# future_L = reshape(future_L, dir = "long" , varying = grep("HuD",names(future_L)) , sep = "_")
# 
# hist_fut = merge(historical_L, future_L, by=c("SUM090","time"), all=F)
# 
# windows()
# ggplot( hist_fut )+geom_point(aes(x=HuDac4.x,y=HuDac4.x))+facet_wrap(~time)


############################################################################
#  plot panel of densities and tiled density

#historical = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")
#BAU = cbind(historical$COUNTY,historical$CountyName, historical[,grepl("HuDac4_", names(historical))] )
#BAU = subset(BAU, select=-c(HuDac4_109))


future     = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.9ppnoneTUrbNS+D20km\\SpecifyEcoCity6.9ppnoneTUrbNS+D20km.dbf") #JepsonAreaSpecCity6.9TUrbNS+D20km\\SpecifyEcoCity6.9TUrbNS+D20km_SM.dbf")
RuralLabel = "Rd25"
UrbanLanel = "Ud25"

names(future)[grep(RuralLabel,names(future))] = paste(RuralLabel,"_",c(109,119,129,139,149),sep="")
names(future)[grep(UrbanLanel,names(future))] = paste(UrbanLanel,"_",c(109,119,129,139,149),sep="")
names(future)

# setup data 
    # get business as usual and aggregate
    BAU2 = cbind(future$COUNTY,future$CountyName,future$Acres, future[,grepl("HuDac4_", names(future))] )
    names(BAU2) = c("COUNTY","NAME",'Acres',paste("HuD_",seq(39,149, by =10), sep=""))
    A_BAU = aggregate(. ~   NAME,  BAU2, median)
    P_BAU= reshape(A_BAU, dir = "long", varying = grep("HuD",names(A_BAU)) , sep = "_")
    P_BAU$Time = P_BAU$time +1901    
    
    # get urban data and aggregate
    U2 = cbind(future$COUNTY,future$CountyName,future$Acres, future[,grepl(paste(UrbanLanel,"_",sep=""), names(future))] )
    if(length(grep('\\.', names(U2))) != 0){
        U2 = subset(U2, select=-c( grep('\\.', names(U2))  )) # remove extras with name .
    }
    names(U2) = c("COUNTY","NAME",'Acres',paste("HuD_",seq(109,149, by =10), sep=""))
    A_U = aggregate(. ~   NAME,  U2, median)
    P_U= reshape(A_U, dir = "long", varying = grep("HuD",names(A_U)) , sep = "_")
    P_U$Time = P_U$time +1901
    
    # get rural data and aggregate
    R2 = cbind(future$COUNTY,future$CountyName,future$Acres, future[,grepl(paste(RuralLabel,"_",sep=""), names(future))] )
    if(length(grep('\\.', names(R2))) != 0){
        R2 = subset(R2, select=-c(grep('\\.', names(R2))  )) # remove extras 
    }
    names(R2) = c("COUNTY","NAME",'Acres',paste("HuD_",seq(109,149, by =10), sep=""))
    A_R = aggregate(. ~   NAME,  R2, median)
    P_R= reshape(A_R, dir = "long", varying = grep("HuD",names(A_R)) , sep = "_")
    P_R$Time = P_R$time +1901

    # remove "county" from county name
    A_BAU$NAME = sapply(1:length(A_BAU$NAME), function(x) substr(as.character(A_BAU[x,"NAME"]),1,nchar(as.character(A_BAU[x,"NAME"]))-7  )  ) 
    A_U$NAME = sapply(1:length(A_U$NAME), function(x) substr(as.character(A_U[x,"NAME"]),1,nchar(as.character(A_U[x,"NAME"]))-7  )  ) 
    A_R$NAME = sapply(1:length(A_R$NAME), function(x) substr(as.character(A_R[x,"NAME"]),1,nchar(as.character(A_R[x,"NAME"]))-7  )  ) 

    # create panel of all data 
    P_BAU$Scenario = 'BAU Growth'
    P_R$Scenario = 'Rural Growth'
    P_U$Scenario = 'Urban Growth'
    P_U$Scenario = factor(P_U$Scenario, levels=levels(factor(P_U$Scenario)), ordered=T)
    P_All = rbind(P_BAU,P_R,P_U )
    P_All$TimeScenario = paste(P_All$Time, P_All$Scenario, sep='')  # create group by year and scenario for boxplot 

# line plot historical densities 
    # Heat map & histograms
    windows()
    ggplot(data=P_BAU , aes(x=Time, y=HuD , group=COUNTY,color=COUNTY)) + geom_line(size=1,linetype=c("solid"))  
    ggplot(data=P_R, aes(x=Time, y=HuD , group=COUNTY,color=COUNTY)) + geom_line(size=1,linetype=c("solid"))  
    ggplot(data=P_U, aes(x=Time, y=HuD , group=COUNTY,color=COUNTY)) + geom_line(size=1,linetype=c("solid"))  

    ggplot(data= P_BAU[P_BAU$Time>=1979,] , aes(x=Time, y=HuD , group=COUNTY,colour=Scenario)) + geom_line( )  +
          geom_line(data=P_R ) + geom_line(data=P_U )

    library(scales)    # log scaled 
    ggplot(data=P_BAU , aes(x=Time, y=HuD , group=COUNTY,color=COUNTY)) + geom_line(size=1,linetype=c("solid")) + scale_y_continuous(trans=log2_trans())

# box plot 
    windows()
    ggplot(P_All[P_All$Time>=1979 ,], aes(x=Time, y=HuD, group=TimeScenario, fill=Scenario)) + geom_boxplot()   + coord_cartesian(ylim = c(0, 10) )+stat_summary(fun.y=mean, geom="point", shape=5, size=4)
# Violin plot
    ggplot(P_All[P_All$Time>=2009 ,], aes(x=Time, y=HuD, group=TimeScenario, fill=Scenario)) + geom_violin()  + coord_cartesian(ylim = c(0, 10) )  

 

# Density plot historical densities 
    #data
    for (i in seq(39,149,by=10)){   
      year = grep(as.character(i),names(A_BAU) )[1]     # find column # for current year, if two options 39 vs 139 choose first
      assign(paste("HDB",i,sep="_"), cbind(as.character(A_BAU[,"NAME"]),unfactor(A_BAU[,year]) ,paste("Y_",i,sep=""))  )
      # this assign is used to hepl with ggplot histograms
    }
    all_HD_yr = as.data.frame( rbind( HDB_59,HDB_69,HDB_79,HDB_89,HDB_99,HDB_109,HDB_119,HDB_129,HDB_139,HDB_149) ,stringsAsFactors=F )
    names(all_HD_yr) = c("Name","Density","Year")
    all_HD_yr$Density = as.numeric(all_HD_yr$Density)
    all_HD_yr$Year =  factor(all_HD_yr$Year,levels=c( "Y_59","Y_69","Y_79","Y_89","Y_99","Y_109","Y_119","Y_129","Y_139","Y_149"),c("1960","1970","1980","1990","2000","2010","2020","2030","2040","2050"))
    all_HD_yr$Name = factor(all_HD_yr$Name,levels=rev(levels(factor(all_HD_yr$Name))),ordered=T )
    # plot 
    ggplot(all_HD_yr, aes(Density, fill = Year)) + geom_density(alpha=.4, colour ="grey33") +scale_fill_grey(start=0,end=1 )+ ylab("Density") + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, 1.5), xlim=c(0,5)) 
    ggplot(all_HD_yr, aes(Density, fill = Year)) + geom_density(alpha=.25, colour ="grey33") +scale_fill_grey(start=0,end=1 )+ ylab("Density") + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, 1.5), xlim=c(0,5)) 
    windows()
    ggplot(all_HD_yr, aes(Density, fill = Year)) + geom_density(alpha=.25 )  + xlab("Median County Housing Density by Year")+ ylab("Density") + coord_cartesian(ylim = c(0, 1.25), xlim=c(0,5)) 

 
## tile densities   
    g = ggplot(all_HD_yr, aes(Year, Name)) +geom_tile(aes(fill = Density),colour = "white")  
    g + theme_grey(base_size = 10) +  scale_fill_gradientn(colours = c("gray98","gray87","gray77","gray67","gray57","gray47","gray37","gray27" ) ,breaks=c(.25,.5,1,3,10,15))
     
    g = ggplot(all_HD_yr, aes(Year, Name)) +geom_tile(aes(fill = Density),colour = "white")  
    g + theme_grey(base_size = 10) +  scale_fill_gradientn(colours = c( "gray98","gray87","gray77","gray67","gray57","gray47","gray37","gray27","gray17","gray7" ) ,breaks=c(.25,.5,1,2,3,5,7.5,10,15))
    
    windows()# used in paper other option
    g = ggplot(all_HD_yr, aes(Year, Name)) +geom_tile(aes(fill = Density),colour = "white")  
    g + theme_grey(base_size = 10)  + scale_fill_gradient(low="gray98", high="black")

    
    g = ggplot(all_HD_yr, aes(Year, Name)) +geom_tile(aes(fill = Density),colour = "white")  
    g + theme_grey(base_size = 10) +  scale_fill_gradientn(colours = c( "gray99","gray87","gray77","gray67","gray57","gray47","gray37","gray27","gray17","gray7" ) ,breaks=c(.25,.5,1,1.5,2,3,4,5,10,15))
    
    # without sanfran * used in paper *
    all_HD_yr2 = all_HD_yr[-c(grep('SAN FRANCISCO',all_HD_yr[,1])), ]
    g = ggplot(all_HD_yr2, aes(Year, Name)) +geom_tile(aes(fill = Density),colour = "white")  
    g + theme_grey(base_size = 10)+xlab('Decade')+ylab('County Name')  + scale_fill_gradient(low="gray98", high="black", name = "Density h/ac")


    write.csv(all_HD_yr, 'G://Faculty//Mann//Share//density_v6.csv')
    library(reshape2)

    
    head(all_HD_yr)
    data.wide <-dcast(all_HD_yr, Name ~ Year, value.var="Density")
    write.csv(data.wide, 'G://Faculty//Mann//Share//density_v6.csv')
    
    library(xtable)
    tablex = xtable(data.wide)      
    print.xtable(tablex, type="html", file="county_density.html")

## Density change 
    p_all_HD_yr =  pdata.frame(all_HD_yr, c("Name","Year"), drop.index=FALSE)
    p_all_HD_yr$DensityCHG = diff(x=p_all_HD_yr$Density,1)
    p_all_HD_yr$DensityLAG = lag(x=p_all_HD_yr$Density,1)
    p_all_HD_yr$PCHNGDensity =  p_all_HD_yr$DensityCHG/p_all_HD_yr$DensityLAG

    g = ggplot(p_all_HD_yr[unfactor(p_all_HD_yr$Year)>1980,], aes(Year, Name)) +geom_tile(aes(fill = PCHNGDensity),colour = "white")  
    g + theme_grey(base_size = 10) +  scale_fill_gradientn(colours = c( "gray99","gray87","gray77","gray67","gray57","gray47","gray37","gray27"  ) ,breaks=c(0,.1,.25,.5,.75,1,1.25,1.5,2))

## show % change 1999 to 2049 sorted    * USED IN PAPER * 
    p_all_HD_yr2049  =  p_all_HD_yr[unfactor(p_all_HD_yr$Year)==2049,]
    p_all_HD_yr1999  =  p_all_HD_yr[unfactor(p_all_HD_yr$Year)==1999,]
    DenChange2049199=  data.frame(densitychange = (p_all_HD_yr2049$Density - p_all_HD_yr1999$Density))
    row.names(DenChange2049199) = row.names(p_all_HD_yr2049)
    cbind(row.names(DenChange2049199)[  order(DenChange2049199$densitychange)   ],    DenChange2049199[  order(DenChange2049199$densitychange)   ,])
    PDenChange2049199=  data.frame(Pdensitychange = ((p_all_HD_yr2049$Density - p_all_HD_yr1999$Density)/p_all_HD_yr1999$Density))
    row.names(PDenChange2049199) = row.names(p_all_HD_yr2049)
    cbind(row.names(PDenChange2049199)[  order(PDenChange2049199$Pdensitychange)   ],    PDenChange2049199[  order(PDenChange2049199$Pdensitychange)   ,])



############################################################################
#  Density plot compare urban rural scenarios 
#  Aggregated at the county level 
# get data
    for (i in seq(109,149,by=10)){
        year = grep(as.character(i),names(A_U) )[1]
        assign(paste("HDU",i,sep="_"), cbind(as.character(A_U[,"NAME"]),unfactor(A_U[,year]) ,paste("Y_",i,sep=""))  )
            # this assign is used to hepl with ggplot histograms
    }
    for (i in seq(109,149,by=10)){
        year = grep(as.character(i),names(A_R) )[1]
        assign(paste("HDR",i,sep="_"), cbind(as.character(A_R[,"NAME"]),unfactor(A_R[,year]) ,paste("Y_",i,sep=""))  )
        # this assign is used to hepl with ggplot histograms
    }
    for (i in seq(109,149,by=10)){
        year = grep(as.character(i),names(A_BAU) )[1]
        assign(paste("HDB",i,sep="_"), cbind(as.character(A_BAU[,"NAME"]),unfactor(A_BAU[,year]) ,paste("Y_",i,sep=""))  )
        # this assign is used to hepl with ggplot histograms
    }
   
    all_HDB_yr = as.data.frame( rbind( HDB_109,HDB_119,HDB_129,HDB_139,HDB_149) ,stringsAsFactors=F )
    names(all_HDB_yr) = c("Name","Density","Year")
    all_HDB_yr$Density = as.numeric(all_HDB_yr$Density)
    all_HDB_yr$Year =  factor(all_HDB_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c( "2009","2019","2029","2039","2049"))
    
    all_HDU_yr = as.data.frame( rbind( HDU_109,HDU_119,HDU_129,HDU_139,HDU_149) ,stringsAsFactors=F )
    names(all_HDU_yr) = c("Name","Density","Year")
    all_HDU_yr$Density = as.numeric(all_HDU_yr$Density)
    all_HDU_yr$Year =  factor(all_HDU_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c( "2009","2019","2029","2039","2049"))
    
    all_HDR_yr = as.data.frame( rbind( HDR_109,HDR_119,HDR_129,HDR_139,HDR_149) ,stringsAsFactors=F )
    names(all_HDR_yr) = c("Name","Density","Year")
    all_HDR_yr$Density = as.numeric(all_HDR_yr$Density)
    all_HDR_yr$Year =  factor(all_HDR_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c( "2009","2019","2029","2039","2049"))

# Plots 
 bau=   ggplot(all_HDB_yr, aes(Density, fill = Year)) + geom_density(alpha=.25 )  + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, .4), xlim=c(0,7))+ labs(title="BAU") 
 urb=    ggplot(all_HDU_yr, aes(Density, fill = Year)) + geom_density(alpha=.25 )  + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, .4), xlim=c(0,7)) + labs(title="Urban Growth") 
 rur=    ggplot(all_HDR_yr, aes(Density, fill = Year)) + geom_density(alpha=.25 )  + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, .4), xlim=c(0,7)) + labs(title="Rural Growth") 

windows() 
grid.arrange(bau,urb,rur,ncol=3)  # note: corner missing because of zero density areas 



###  PLOTTING DENSITIES WITHOUT AGGREGATING JUST DO SBG LEVEL  

    for (i in seq(109,149,by=10)){
        year = grep(as.character(i),names(U2) )[1]
        assign(paste("SBGHDU2",i,sep="_"), cbind(as.character(U2[,"NAME"]),unfactor(U2[,year]) ,paste("Y_",i,sep=""))  )
        # this assign is used to hepl with ggplot histograms
    }
    for (i in seq(109,149,by=10)){
        year = grep(as.character(i),names(BAU2) )[1]
        assign(paste("SBGHDBAU2",i,sep="_"), cbind(as.character(BAU2[,"NAME"]),unfactor(BAU2[,year]) ,paste("Y_",i,sep=""))  )
        # this assign is used to hepl with ggplot histograms
    }
    
    for (i in seq(109,149,by=10)){
        year = grep(as.character(i),names(R2) )[1]
        assign(paste("SBGHDR2",i,sep="_"), cbind(as.character(R2[,"NAME"]),unfactor(R2[,year]) ,paste("Y_",i,sep=""))  )
        # this assign is used to hepl with ggplot histograms
    }
    
    all_SBGHDBAU2_yr = as.data.frame( rbind( SBGHDBAU2_109,SBGHDBAU2_119,SBGHDBAU2_129,SBGHDBAU2_139,SBGHDBAU2_149) ,stringsAsFactors=F )
    names(all_SBGHDBAU2_yr) = c("Name","Density","Year")
    all_SBGHDBAU2_yr$Density = as.numeric(all_SBGHDBAU2_yr$Density)
    all_SBGHDBAU2_yr$Year =  factor(all_SBGHDBAU2_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c( "2010","2020","2030","2040","2050"))
    all_SBGHDBAU2_yr$Scenario = "aBAU Growth"
    
    all_SBGHDU2_yr = as.data.frame( rbind( SBGHDU2_109,SBGHDU2_119,SBGHDU2_129,SBGHDU2_139,SBGHDU2_149) ,stringsAsFactors=F )
    names(all_SBGHDU2_yr) = c("Name","Density","Year")
    all_SBGHDU2_yr$Density = as.numeric(all_SBGHDU2_yr$Density)
    all_SBGHDU2_yr$Year =  factor(all_SBGHDU2_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c( "2010","2020","2030","2040","2050"))
    all_SBGHDU2_yr$Scenario = "bUrban Growth"
    
    all_SBGHDR2_yr = as.data.frame( rbind( SBGHDR2_109,SBGHDR2_119,SBGHDR2_129,SBGHDR2_139,SBGHDR2_149) ,stringsAsFactors=F )
    names(all_SBGHDR2_yr) = c("Name","Density","Year")
    all_SBGHDR2_yr$Density = as.numeric(all_SBGHDR2_yr$Density)
    all_SBGHDR2_yr$Year =  factor(all_SBGHDR2_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c("2010","2020","2030","2040","2050"))
    all_SBGHDR2_yr$Scenario = "cRural Growth"
    
    all_SBG = rbind(all_SBGHDBAU2_yr,all_SBGHDU2_yr,all_SBGHDR2_yr)
    all_SBG$TimeScenario = paste(all_SBG$Year, all_SBG$Scenario, sep='')  # create group by year and scenario for boxplot 
    all_SBG$Scenario = factor(all_SBG$Scenario, levels=c('aBAU Growth', 'bUrban Growth', 'cRural Growth'),labels=c('BAU Growth', 'Urban Growth', 'Rural Growth'), ordered=T)

# Plots at SBG level 
    a=ggplot(all_SBGHDU2_yr, aes(Density, fill = Year)) + geom_density(alpha=.5 ) + ylab("Density") + xlab(" ") + coord_cartesian(ylim = c(0, .2), xlim=c(0,10)) +opts(title='Urban Growth',legend.position="none")
    b=ggplot(all_SBGHDBAU2_yr, aes(Density, fill = Year)) + geom_density(alpha=.5 )+ ylab(" ")  + xlab("Median SBG Housing Density by Year") + coord_cartesian(ylim = c(0, .2), xlim=c(0,10)) +opts(title='BAU Growth',legend.position="none")
    c=ggplot(all_SBGHDR2_yr, aes(Density, fill = Year)) + geom_density(alpha=.5 ) + ylab(" ") + xlab(" ") + coord_cartesian(ylim = c(0, .2), xlim=c(0,10)) +opts(title='Rural Growth',legend.position="none")
    
    windows()
    grid.arrange(a,b,c,ncol=3)

# box plot  SBG
    ggplot(all_SBG[all_SBG$Time>=1979 ,], aes(x=Year, y=Density, group=TimeScenario, fill=Scenario)) + geom_boxplot()    + coord_cartesian(ylim = c(0, 10) )
# Violin plot  SBG
    ggplot(all_SBG[all_SBG$Time>=2009 ,], aes(x=Year, y=Density, group=TimeScenario, fill=Scenario)) + geom_violin()  + coord_cartesian(ylim = c(0, 10) )  
 # both   SBG    
    pd = position_dodge(0.9)
    ggplot(all_SBG[all_SBG$Time>=1979 ,], aes(x=Year, y=Density, group=TimeScenario, fill=Scenario)) +geom_violin(position=pd)    +
        geom_boxplot(position=pd,width=0.1,outlier.size=0,fill='grey')  + coord_cartesian(ylim = c(0, 10) )+
    stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=4, position=pd)

    #* USED IN PAPER * violin plot
    ggplot(all_SBG[all_SBG$Time>=1979 ,], aes(x=Year, y=Density, group=TimeScenario, fill=Scenario)) +  geom_boxplot(position=pd,outlier.size=0 )   +
        geom_violin(position=pd,width=1,outlier.size=0,fill='grey',alpha=.5)  + coord_cartesian(ylim = c(0, 10) )+
        stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=4, position=pd)+xlab('Decade')+ylab('Housing Density')
#       ymin, lower whisker, lower hinge - 1.5 * IQR
#       lower, lower hinge, 25% quantile
#       middle, median, 50% quantile
#       upper, upper hinge, 75% quantile
#       ymax, upper whisker, upper hinge + 1.5 * IQR
 
################### 
# plot WUI density classes 

# prepare data    
# aggregate at time and wui class for SBGS
    P_BAU_SBG= reshape(BAU2, dir = "long", varying = grep("HuD",names(BAU2)) , sep = "_")
    P_BAU_SBG$Density = P_BAU_SBG$HuD
    P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 4] = 'Very-High'
    P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 1 & P_BAU_SBG$Density < 4] = 'High'
    P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 0.41 & P_BAU_SBG$Density < 1] = 'Medium'
    P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 0.025 & P_BAU_SBG$Density < 0.41] = 'Low'
    P_BAU_SBG$WUClass[P_BAU_SBG$Density < 0.025 ]       = 'Sparse'
    P_BAU_SBG$WUClass= factor(P_BAU_SBG$WUClass,levels=c('Very-High','High','Medium','Low','Sparse'),ordered=T)
    P_BAU_SBG$Houses = P_BAU_SBG$Density*P_BAU_SBG$Acres
    P_BAU_SBG=pdata.frame(P_BAU_SBG, c("id","time"), drop.index=FALSE)
    P_BAU_SBG$LagDen = lag(P_BAU_SBG$Density,1)
    A_BAU_SBG = aggregate(cbind(Acres,Houses) ~time+WUClass , data = P_BAU_SBG,sum)
    A_BAU_SBG$Time = unfactor(A_BAU_SBG$time) +1900

    
    P_U_SBG= reshape(U2, dir = "long", varying = grep("HuD",names(U2)) , sep = "_")
    P_U_SBG$Density = P_U_SBG$HuD
    P_U_SBG$WUClass[P_U_SBG$Density >= 4] = 'Very-High'
    P_U_SBG$WUClass[P_U_SBG$Density >= 1 & P_U_SBG$Density < 4] = 'High'
    P_U_SBG$WUClass[P_U_SBG$Density >= 0.41 & P_U_SBG$Density < 1] = 'Medium'
    P_U_SBG$WUClass[P_U_SBG$Density >= 0.025 & P_U_SBG$Density < 0.41] = 'Low'
    P_U_SBG$WUClass[P_U_SBG$Density < 0.025 ]       = 'Sparse'
    P_U_SBG$WUClass= factor(P_U_SBG$WUClass,levels=c('Very-High','High','Medium','Low','Sparse'),ordered=T)
    P_U_SBG$Houses = P_U_SBG$Density*P_U_SBG$Acres
    A_U_SBG = aggregate(cbind(Acres,Houses) ~time+WUClass , data = P_U_SBG,sum)
    A_U_SBG$Time = unfactor(A_U_SBG$time)+1900
    A_U_SBG = rbind(A_BAU_SBG[A_BAU_SBG$Time<2009,],A_U_SBG)

    P_R_SBG= reshape(R2, dir = "long", varying = grep("HuD",names(R2)) , sep = "_")
    P_R_SBG$Density = P_R_SBG$HuD
    P_R_SBG$WUClass[P_R_SBG$Density >= 4] = 'Very-High'
    P_R_SBG$WUClass[P_R_SBG$Density >= 1 & P_R_SBG$Density < 4] = 'High'
    P_R_SBG$WUClass[P_R_SBG$Density >= 0.41 & P_R_SBG$Density < 1] = 'Medium'
    P_R_SBG$WUClass[P_R_SBG$Density >= 0.025 & P_R_SBG$Density < 0.41] = 'Low'
    P_R_SBG$WUClass[P_R_SBG$Density < 0.025 ]       = 'Sparse'
    P_R_SBG$WUClass= factor(P_R_SBG$WUClass,levels=c('Very-High','High','Medium','Low','Sparse'),ordered=T)
    P_R_SBG$Houses = P_R_SBG$Density*P_R_SBG$Acres
    A_R_SBG = aggregate(cbind(Acres,Houses) ~time+WUClass , data = P_R_SBG,sum)
    A_R_SBG$Time = unfactor(A_R_SBG$time)+1900
    A_R_SBG = rbind(A_BAU_SBG[A_BAU_SBG$Time<2009,],A_R_SBG)



#     # aggregate at time and housing class for SBGS
#     P_BAU_SBG= reshape(BAU2, dir = "long", varying = grep("HuD",names(BAU2)) , sep = "_")
#     P_BAU_SBG$Density = P_BAU_SBG$HuD
#     P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 6] = 'Very-High'
#     P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 3 & P_BAU_SBG$Density < 6] = 'High'
#     P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 1 & P_BAU_SBG$Density < 3] = 'Medium'
#     P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 0.41 & P_BAU_SBG$Density < 1] = 'Low'
#     P_BAU_SBG$WUClass[P_BAU_SBG$Density >= 0.025 & P_BAU_SBG$Density < 0.41] = 'Very-Low'
#     P_BAU_SBG$WUClass[P_BAU_SBG$Density < 0.025 ]       = 'Sparse'
#     P_BAU_SBG$WUClass= factor(P_BAU_SBG$WUClass,levels=c('Very-High','High','Medium','Low','Very-Low','Sparse'),ordered=T)
#     P_BAU_SBG$Houses = P_BAU_SBG$Density*P_BAU_SBG$Acres
#     P_BAU_SBG=pdata.frame(P_BAU_SBG, c("id","time"), drop.index=FALSE)
#     P_BAU_SBG$LagDen = lag(P_BAU_SBG$Density,1)
#     A_BAU_SBG = aggregate(cbind(Acres,Houses) ~time+WUClass , data = P_BAU_SBG,sum)
#     A_BAU_SBG$Time = unfactor(A_BAU_SBG$time) +1900
#     
#     
#     P_U_SBG= reshape(U2, dir = "long", varying = grep("HuD",names(U2)) , sep = "_")
#     P_U_SBG$Density = P_U_SBG$HuD
#     P_U_SBG$WUClass[P_U_SBG$Density >= 6] = 'Very-High'
#     P_U_SBG$WUClass[P_U_SBG$Density >= 3 & P_U_SBG$Density < 6] = 'High'
#     P_U_SBG$WUClass[P_U_SBG$Density >= 1 & P_U_SBG$Density < 3] = 'Medium'
#     P_U_SBG$WUClass[P_U_SBG$Density >= 0.41 & P_U_SBG$Density < 1] = 'Low'
#     P_U_SBG$WUClass[P_U_SBG$Density >= 0.025 & P_U_SBG$Density < 0.41] = 'Very-Low'
#     P_U_SBG$WUClass[P_U_SBG$Density < 0.025 ]       = 'Sparse'
#     P_U_SBG$WUClass= factor(P_U_SBG$WUClass,levels=c('Very-High','High','Medium','Low','Very-Low','Sparse'),ordered=T)
#     P_U_SBG$Houses = P_U_SBG$Density*P_U_SBG$Acres
#     A_U_SBG = aggregate(cbind(Acres,Houses) ~time+WUClass , data = P_U_SBG,sum)
#     A_U_SBG$Time = unfactor(A_U_SBG$time)+1900
#     A_U_SBG = rbind(A_BAU_SBG[A_BAU_SBG$Time<2009,],A_U_SBG)
# 
# 
#     P_R_SBG= reshape(R2, dir = "long", varying = grep("HuD",names(R2)) , sep = "_")
#     P_R_SBG$Density = P_R_SBG$HuD
#     P_R_SBG$WUClass[P_R_SBG$Density >= 6] = 'Very-High'
#     P_R_SBG$WUClass[P_R_SBG$Density >= 3 & P_R_SBG$Density < 6] = 'High'
#     P_R_SBG$WUClass[P_R_SBG$Density >= 1 & P_R_SBG$Density < 3] = 'Medium'
#     P_R_SBG$WUClass[P_R_SBG$Density >= 0.41 & P_R_SBG$Density < 1] = 'Low'
#     P_R_SBG$WUClass[P_R_SBG$Density >= 0.025 & P_R_SBG$Density < 0.41] = 'Very-Low'
#     P_R_SBG$WUClass[P_R_SBG$Density < 0.025 ]       = 'Sparse'
#     P_R_SBG$WUClass= factor(P_R_SBG$WUClass,levels=c('Very-High','High','Medium','Low','Very-Low','Sparse'),ordered=T)
#     P_R_SBG$Houses = P_R_SBG$Density*P_R_SBG$Acres
#     A_R_SBG = aggregate(cbind(Acres,Houses) ~time+WUClass , data = P_R_SBG,sum)
#     A_R_SBG$Time = unfactor(A_R_SBG$time)+1900
#     A_R_SBG = rbind(A_BAU_SBG[A_BAU_SBG$Time<2009,],A_R_SBG)

 

# Table  *used in table 3 Acreage  by density classification in 2050 *
    A_BAU_SBG[with(A_BAU_SBG, order(WUClass,Time) ), ]
    A_R_SBG[with(A_R_SBG, order(WUClass,Time) ), ]
    A_U_SBG[with(A_U_SBG, order(WUClass,Time) ), ]

    # used in paper 
    tabl3 = data.frame(DensityClass=c('Very-High','High','Medium','Low','Sparse'),
                       BussinessAsUsual=round(c(A_BAU_SBG[A_BAU_SBG$time==149 & A_BAU_SBG$WUClass=='Very-High','Acres'],
                                          A_BAU_SBG[A_BAU_SBG$time==149 & A_BAU_SBG$WUClass=='High','Acres'],
                                          A_BAU_SBG[A_BAU_SBG$time==149 & A_BAU_SBG$WUClass=='Medium','Acres'],
                                          A_BAU_SBG[A_BAU_SBG$time==149 & A_BAU_SBG$WUClass=='Low','Acres'],
                                          A_BAU_SBG[A_BAU_SBG$time==149 & A_BAU_SBG$WUClass=='Sparse','Acres'])),
                       RuralScenario=round(c(A_R_SBG[A_R_SBG$time==149 & A_R_SBG$WUClass=='Very-High','Acres'],
                                       A_R_SBG[A_R_SBG$time==149 & A_R_SBG$WUClass=='High','Acres'],
                                       A_R_SBG[A_R_SBG$time==149 & A_R_SBG$WUClass=='Medium','Acres'],
                                       A_R_SBG[A_R_SBG$time==149 & A_R_SBG$WUClass=='Low','Acres'],
                                       A_R_SBG[A_R_SBG$time==149 & A_R_SBG$WUClass=='Sparse','Acres'])),
                       UrbanScenario=round(c(A_U_SBG[A_U_SBG$time==149 & A_U_SBG$WUClass=='Very-High','Acres'],
                                       A_U_SBG[A_U_SBG$time==149 & A_U_SBG$WUClass=='High','Acres'],
                                       A_U_SBG[A_U_SBG$time==149 & A_U_SBG$WUClass=='Medium','Acres'],
                                       A_U_SBG[A_U_SBG$time==149 & A_U_SBG$WUClass=='Low','Acres'],
                                       A_U_SBG[A_U_SBG$time==149 & A_U_SBG$WUClass=='Sparse','Acres'])),                      
                       TwoThousandLevel=round(c(A_U_SBG[A_U_SBG$time==99 & A_U_SBG$WUClass=='Very-High','Acres'],
                                       A_U_SBG[A_U_SBG$time==99 & A_U_SBG$WUClass=='High','Acres'],
                                       A_U_SBG[A_U_SBG$time==99 & A_U_SBG$WUClass=='Medium','Acres'],
                                       A_U_SBG[A_U_SBG$time==99 & A_U_SBG$WUClass=='Low','Acres'],
                                       A_U_SBG[A_U_SBG$time==99 & A_U_SBG$WUClass=='Sparse','Acres']))
                       )
    
    library(xtable)
    tablex3 = xtable(tabl3)      
    print.xtable(tablex3, floating=F,type="html", file="table3_Acreagebydensityclassificationin2050.html")


# * used in paper * I think 
# Table of times lag den = 0 and den >0
    # newly developed pre 1999
    sum(P_BAU_SBG$LagDen==0 & P_BAU_SBG$Density>0 & unfactor(P_BAU_SBG$time)<=99,na.rm=T)#/length(unique(P_BAU_SBG$time[unfactor(P_BAU_SBG$time)<=99]  ))
    sum(P_BAU_SBG$LagDen==0 & P_BAU_SBG$Density>0 & unfactor(P_BAU_SBG$time)<=99,na.rm=T) /sum( P_BAU_SBG$Density==0 & unfactor(P_BAU_SBG$time)<=99,na.rm=T)  # as % of total number of empty polygons
# newly developed post 1999
    sum(P_BAU_SBG$LagDen==0 & P_BAU_SBG$Density>0& unfactor(P_BAU_SBG$time)>99,na.rm=T)#/length(unique(P_BAU_SBG$time[unfactor(P_BAU_SBG$time)>99]  ))
    sum(P_BAU_SBG$LagDen==0 & P_BAU_SBG$Density>0& unfactor(P_BAU_SBG$time)>99,na.rm=T) /sum( P_BAU_SBG$Density==0 & unfactor(P_BAU_SBG$time)>99,na.rm=T)# as % of total number of empty polygons

    P_BAU_SBG$empty = P_BAU_SBG$Density==0
    empty_SBGS = aggregate(empty~time, P_BAU_SBG, sum)  
    empty_SBGS$Time = unfactor(empty_SBGS$time)+1900
    # number of empty SBG   USED IN PAPER
    windows()
    ggplot(empty_SBGS,aes(x = Time, y = empty,group=1))+ geom_line( size=2,colour="lightblue")+opts(title=' ', legend.position="none")+xlab('')+ylab('Number of uninhabited SBGs')
# average acerage of 'rural' SBGs as apposed to urban
    rural = BAU2[BAU2$HuD_99<1,]
    median(rural$Acres)
    urban = BAU2[BAU2$HuD_99>=1,]
    median(urban$Acres)
    rural$Type = "Rural"
    urban$Type = "Urban"
    urban_rural=rbind(rural,urban)
 


a= ggplot(urban_rural, aes(Acres ,group=Type, fill=Type)) + geom_histogram(alpha=.5 , position="dodge",aes(y=..density..),binwidth=10000 ,)  + ylab("Density") + xlab("Acres")   +opts(title=' ') +coord_cartesian(  xlim=c(0,2.e+5))
brewerplot <- function (palette) { # add color scheme
    a + scale_fill_brewer(palette = palette) 
}
plot(a)
brewerplot ("Dark2") 
  

  # Acrage stacked area plots     USED IN PAPER
  colours <- c("#FFFFCC", '#C7E9B4','#7FCDBB','#41B6C4','#2C7FB8','#253494')  #  colours <- c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494") 
  ba=ggplot(A_BAU_SBG,aes(x = Time, y = Acres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
    geom_area(alpha=.75, position = 'stack') + geom_line( aes(ymax=Acres), position="stack", colour='darkgrey')+opts(title='BAU Growth', legend.position="none")+xlab('')
  ua=ggplot(A_U_SBG,aes(x = Time, y = Acres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
    geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')   +opts(title='Urban Growth', legend.position="none")+xlab('')  +ylab('')
  ra=ggplot(A_R_SBG,aes(x = Time, y = Acres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
    geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')  +opts(title='Rural Growth', legend.position="none")+xlab('')+ylab('')
  
  # house stacked area counts
  bb=ggplot(A_BAU_SBG,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
    geom_area(alpha=.75) + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')+opts( legend.position="none")+xlab('')
  ub=ggplot(A_U_SBG,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
    geom_area(alpha=.75) + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')   +opts( legend.position="none")+ylab('')+xlab('')+
    opts(legend.direction = "horizontal", legend.position = c(-1.1, 1.095), legend.background = theme_rect(colour = 'grey90', fill = 'grey90', size = .5, linetype='solid'))
  rb=ggplot(A_R_SBG,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
    geom_area(alpha=.75) + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')   +opts( legend.position="none")+xlab('Decade')+ylab('') 
  
  # All combined 
  windows()   # * used in paper * #
   grid.arrange(ba,ra,ua,bb,rb,ub,ncol=3)  # note: corner missing because of zero density areas 

  
  # seperately  
  A_BAU_SBG$Scenario = "BAU Growth"
  A_U_SBG$Scenario = "Urban Growth"
  A_R_SBG$Scenario = "Rural Growth"
  A_ALL_SBG=rbind(A_BAU_SBG,A_U_SBG,A_R_SBG)
  windows()
      Agrid=ggplot(A_ALL_SBG,aes(x = Time, y = Acres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
          geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')+opts(  legend.position="none")+xlab('')
      Agrid+ facet_grid(.~Scenario)
  windows()
      Hgrid=ggplot(A_ALL_SBG,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
          geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')+opts(  legend.position="none")+xlab('')
      Hgrid+ facet_grid(.~Scenario)
       

#################################### NUMERICS  ###############################
# get total area change in acres and house counts for each scenario 
# change from 1999 to 2049
    find_acres <- function(time, class, scenario){
      all_data <<- rbind(A_BAU_SBG,A_U_SBG,A_R_SBG)
      all_data$Acres[all_data$Time==time & all_data$WUClass==class & all_data$Scenario==scenario] 
    }
    find_houses <- function(time, class, scenario){
      all_data <<- rbind(A_BAU_SBG,A_U_SBG,A_R_SBG)
      all_data$Houses[all_data$Time==time & all_data$WUClass==class & all_data$Scenario==scenario] 
    }
 
    # change in acres 'BAU Growth'
    find_acres(2049,'Sparse','BAU Growth')/find_acres(1999,'Sparse','BAU Growth') #0.8362701
    find_acres(2049,'Low','BAU Growth')/find_acres(1999,'Low','BAU Growth') #1.589186
    find_acres(2049,'Medium','BAU Growth')/find_acres(1999,'Medium','BAU Growth') # 1.843023
    find_acres(2049,'High','BAU Growth')/find_acres(1999,'High','BAU Growth') #0.5404677
    find_acres(2049,'Very-High','BAU Growth')/find_acres(1999,'Very-High','BAU Growth') #2.267543
    
    # change in houses 'BAU Growth'
    find_houses(2049,'Sparse','BAU Growth')/find_acres(1999,'Sparse','BAU Growth') #0.005659861
    find_houses(2049,'Low','BAU Growth')/find_acres(1999,'Low','BAU Growth') #0.1452837
    find_houses(2049,'Medium','BAU Growth')/find_acres(1999,'Medium','BAU Growth') # 1.217033
    find_houses(2049,'High','BAU Growth')/find_acres(1999,'High','BAU Growth') #1.260497
    find_houses(2049,'Very-High','BAU Growth')/find_acres(1999,'Very-High','BAU Growth') #19.52944
    
    # change in acres 'Urban Growth'
    find_acres(2049,'Sparse','Urban Growth')/find_acres(1999,'Sparse','Urban Growth') #0.949584
    find_acres(2049,'Low','Urban Growth')/find_acres(1999,'Low','Urban Growth') #1.203339
    find_acres(2049,'Medium','Urban Growth')/find_acres(1999,'Medium','Urban Growth') # 1.129212
    find_acres(2049,'High','Urban Growth')/find_acres(1999,'High','Urban Growth') #0.4073834
    find_acres(2049,'Very-High','Urban Growth')/find_acres(1999,'Very-High','Urban Growth') #2.337194
    
    # change in houses 'Urban Growth'
    find_houses(2049,'Sparse','Urban Growth')/find_acres(1999,'Sparse','Urban Growth') #0.005880858
    find_houses(2049,'Low','Urban Growth')/find_acres(1999,'Low','Urban Growth') #0.1104848
    find_houses(2049,'Medium','Urban Growth')/find_acres(1999,'Medium','Urban Growth') # 0.6586518
    find_houses(2049,'High','Urban Growth')/find_acres(1999,'High','Urban Growth') #1.0576
    find_houses(2049,'Very-High','Urban Growth')/find_acres(1999,'Very-High','Urban Growth') #20.86237
      
    # change in acres 'Rural Growth'
    find_acres(2049,'Sparse','Rural Growth')/find_acres(1999,'Sparse','Rural Growth') #0.7351495
    find_acres(2049,'Low','Rural Growth')/find_acres(1999,'Low','Rural Growth') #1.906725
    find_acres(2049,'Medium','Rural Growth')/find_acres(1999,'Medium','Rural Growth') # 1.708309
    find_acres(2049,'High','Rural Growth')/find_acres(1999,'High','Rural Growth') #1.160147
    find_acres(2049,'Very-High','Rural Growth')/find_acres(1999,'Very-High','Rural Growth') #2.115984
    
    # change in houses 'Rural Growth'
    find_houses(2049,'Sparse','Rural Growth')/find_acres(1999,'Sparse','Rural Growth') #0.005819574
    find_houses(2049,'Low','Rural Growth')/find_acres(1999,'Low','Rural Growth') #0.1977647
    find_houses(2049,'Medium','Rural Growth')/find_acres(1999,'Medium','Rural Growth') # 1.081757
    find_houses(2049,'High','Rural Growth')/find_acres(1999,'High','Rural Growth') #2.457639
    find_houses(2049,'Very-High','Rural Growth')/find_acres(1999,'Very-High','Rural Growth') #16.48978
    
    # can be used in paper but already done above
    #TABLE 3: HOUSING COUNTS BY DENSITY CLASSIFICATION IN 2049 from above
    aggregate(Houses ~   Scenario+Time+WUClass,  all_data, sum)
    
    #TABLE 3: ACREAGE BY DENSITY CLASSIFICATION IN 2049 from above
    aggregate( Acres ~   Scenario+Time+WUClass,  all_data, sum)

    # Recession accounted for? NO... 
    recession_houses = aggregate(Houses ~   Scenario+Time,  all_data, sum)
    recession_houses[recession_houses$Scenario == 'BAU Growth',]
    windows()
    ggplot(data=recession_houses , aes(x=Time, y=Houses  )) + geom_line(size=1,linetype=c("solid"))  


##################################################
    # Fire 
 
  # create estimates of fire risk for new housing. 
  # create polygon union of Proposed fire hazard classes and housing. 
  #   FHSZ_path = 'G:/Faculty/Mann/Share/Fire/FRAP_FHSZ/ProposedFullState/PFHSZ.shp'
  #   House_path ="G:/Faculty/Mann/Share/Final_Variables_2/CountyRun/HousingFinal/HousingFinal.shp"
  #   in_features = c(FHSZ_path,House_path)
  #   out_feature_class = 'G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FHSZ\\ProposedFullState\\FHSZ_HuDac_union2'
  #   join_attributes = 'ONLY_FID'
  #   py.Union_analysis(in_features,out_feature_class,join_attributes,cluster_tolerance="", gaps="")
  #   # calculate polygon areas   in meters^2
  #   py.CalculateAreas_stats(input='G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FHSZ\\ProposedFullState\\FHSZ_HuDac_union2.shp', calculate_output='G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FHSZ\\ProposedFullState\\FHSZ_HuDac_union3.shp')
  #   
  RuralLabel = "Rd25a"
  UrbanLanel = "Ud25a"

  housing = read.dbf('G://Faculty//Mann//Share//Final_Variables_2//CountyRun//JepsonGroups6//JepsonAreaSpecCity6.9ppnoneTUrbNS+D20km//SpecifyEcoCity6.9ppnoneTUrbNS+D20km.dbf')
  summary(housing$Acres)

  # merge data based on fids
  FHSZ= read.dbf('G:/Faculty/Mann/Share/Fire/FRAP_FHSZ/ProposedFullState/PFHSZ.dbf')
  names(FHSZ)
  summary(FHSZ$acres)
  summary(FHSZ$HAZ_CLASS)
  FHSZ$fid = 0:(length(FHSZ[,1])-1)
  FHSZ =  subset(FHSZ,select=c('fid','HAZ_CODE'))
  HUDAC =read.dbf("G:/Faculty/Mann/Share/Final_Variables_2/CountyRun/HousingFinal/HousingFinal.dbf")
  HUDAC$fid = 0:(length(HUDAC[,1])-1)
  # used the union of housing and FHSZ boundaries
  FHSZ_HUDAC= read.dbf('G:\\Faculty\\Mann\\Share\\Fire\\FRAP_FHSZ\\ProposedFullState\\FHSZ_HuDac_union3.dbf')  # fid = -1 is non-overlapping areas
  FHSZ_HUDAC$sort = 1:length(FHSZ_HUDAC[,1])
  FHSZ_HUDAC = merge.data.frame(FHSZ_HUDAC,FHSZ,by.x='FID_PFHSZ',by.y='fid',all.x=T,all.y=F)
  FHSZ_HUDAC = merge.data.frame(FHSZ_HUDAC,HUDAC,by.x='FID_Housin',by.y='fid',all.x=T,all.y=F)
  FHSZ_HUDAC =FHSZ_HUDAC[order(FHSZ_HUDAC$sort.x),]


  # Visualize data interactions
  # stacked area based on number of houses. 
  
  # setup data 
  #  get business as usual and aggregate
  FIRE_BAU2 = cbind(FHSZ_HUDAC$COUNTY,FHSZ_HUDAC$CountyName,FHSZ_HUDAC$HAZ_CODE,FHSZ_HUDAC$Acres,FHSZ_HUDAC$F_AREA, FHSZ_HUDAC[,grepl("HuDac4_", names(FHSZ_HUDAC))] )
  names(FIRE_BAU2) = c("COUNTY","NAME",'Haz_Code','Acres','F_area',paste("HuD_",seq(39,149, by =10), sep=""))
  PFIRE_BAU= reshape(FIRE_BAU2, dir = "long", varying = grep("HuD",names(FIRE_BAU2)) , sep = "_")
  PFIRE_BAU$F_acres = PFIRE_BAU$F_area/4046.85642        # convert to acres and
  PFIRE_BAU$Houses = PFIRE_BAU$HuD * PFIRE_BAU$F_acres   # esimates number of houses
  PFIRE_BAU$Time = PFIRE_BAU$time +1900    
  PFIRE_BAU = aggregate(cbind(F_acres,Houses) ~   Haz_Code+Time,  PFIRE_BAU, sum)
  #  get urban data and aggregate
  FIRE_U2 = cbind(FHSZ_HUDAC$COUNTY,FHSZ_HUDAC$CountyName,FHSZ_HUDAC$HAZ_CODE,FHSZ_HUDAC$Acres,FHSZ_HUDAC$F_AREA, FHSZ_HUDAC[,grepl(paste(UrbanLanel,"_",sep=""), names(FHSZ_HUDAC))] )
  names(FIRE_U2) = c("COUNTY","NAME",'Haz_Code','Acres','F_area',paste("HuD_",seq(109,149, by =10), sep=""))
  FIRE_U2= reshape(FIRE_U2, dir = "long", varying = grep("HuD",names(FIRE_U2)) , sep = "_")
  FIRE_U2$F_acres = FIRE_U2$F_area/4046.85642        # convert to acres and
  FIRE_U2$Houses = FIRE_U2$HuD * FIRE_U2$F_acres   # esimates number of houses
  FIRE_U2$Time = FIRE_U2$time +1900
  FIRE_U2 = aggregate(cbind(F_acres,Houses) ~   Haz_Code+Time,  FIRE_U2, sum)
# get rural data and aggregate
  FIRE_R2 = cbind(FHSZ_HUDAC$COUNTY,FHSZ_HUDAC$CountyName,FHSZ_HUDAC$HAZ_CODE,FHSZ_HUDAC$Acres,FHSZ_HUDAC$F_AREA, FHSZ_HUDAC[,grepl(paste(RuralLabel,"_",sep=""), names(FHSZ_HUDAC))] )
  names(FIRE_R2) = c("COUNTY","NAME",'Haz_Code','Acres','F_area',paste("HuD_",seq(109,149, by =10), sep=""))
  FIRE_R2= reshape(FIRE_R2, dir = "long", varying = grep("HuD",names(FIRE_R2)) , sep = "_")
  FIRE_R2$F_acres = FIRE_R2$F_area/4046.85642        # convert to acres and
  FIRE_R2$Houses = FIRE_R2$HuD * FIRE_R2$F_acres   # esimates number of houses
  FIRE_R2$Time = FIRE_R2$time +1900
  FIRE_R2 = aggregate(cbind(F_acres,Houses) ~   Haz_Code+Time,  FIRE_R2, sum)
   
  # create panel of all data 
  PFIRE_BAU$Scenario = 'BAU Growth'
  FIRE_R2$Scenario = 'Rural Growth'
  FIRE_U2$Scenario = 'Urban Growth'
  FIREP_All = rbind(PFIRE_BAU,FIRE_R2,FIRE_U2 )
  FIREP_All$Scenario = factor(FIREP_All$Scenario, levels=levels(factor(FIREP_All$Scenario)), ordered=T)
  FIREP_All$TimeScenario = paste(FIREP_All$Time, FIREP_All$Scenario, sep='')  # create group by year and scenario for boxplot 
  
  
  # reclassify
  FIREP_All$Risk_Class = 'a'
  FIREP_All$Risk_Class[FIREP_All$Haz_Code==-2] = 'Urban'
  FIREP_All$Risk_Class[FIREP_All$Haz_Code==-1] = 'Non-Wildland'
  FIREP_All$Risk_Class[FIREP_All$Haz_Code==1] = 'Moderate'
  FIREP_All$Risk_Class[FIREP_All$Haz_Code==2] = 'High'
  FIREP_All$Risk_Class[FIREP_All$Haz_Code==3] = 'Very High'
  FIREP_All$Risk_Class = factor(FIREP_All$Risk_Class,levels=c('Urban','Non-Wildland','Moderate','High','Very High'),ordered=T)
  


  # # house stacked area counts
  #   
  # windows()
  # ggplot(FIREP_All[FIREP_All$Time>2000,],aes(x = Time, y = Houses, fill=Risk_Class, group=  Risk_Class))+ geom_line(aes(ymax=Houses), position="stack", colour='darkgrey')+ geom_area(alpha=.75) +
  #   scale_fill_brewer(type="seq", palette=18,name = "Fire Risk Class")+facet_grid(~Scenario)  + coord_cartesian(ylim=c(1e7,2.25e7) )+xlab('Decade')
  # 
  # windows()
  # ggplot(FIREP_All[FIREP_All$Time>2000,], aes(x=Time, y=Houses, fill=Risk_Class)) + geom_bar(stat="identity", position=position_dodge(), colour="black")+facet_grid(~Scenario)
  # 
  # windows()
  # ggplot(FIREP_All[FIREP_All$Time>2000,], aes(x=Time, y=Houses, fill=Risk_Class)) + geom_bar(stat="identity", position=position_dodge(), colour="black")+facet_grid(~Scenario)
  # 
  # ggplot(FIREP_All[FIREP_All$Time>2000 & FIREP_All$Haz_Code!=-2,], aes(Risk_Class,y=Houses,fill=Scenario)) + geom_bar(stat="identity", position=position_dodge())+facet_grid(~Time)+ scale_fill_grey(start=0,end=.7)+xlab("Wildfire Hazard Severity Zone")+ theme( axis.text.x  = element_text(angle=90,vjust=0.3))
  
  # use one of these two
  windows()
  ggplot(FIREP_All[FIREP_All$Time>2000 & FIREP_All$Haz_Code!=-2,], aes(x=Time,y=Houses,fill=Scenario,width=3)) + 
        geom_bar(stat="identity",width=10, position=position_dodge(width = 12))+geom_line(aes(x=Time,y=Houses,fill=Scenario ,linetype= Scenario),size = .7)+facet_grid(~Risk_Class)+ scale_fill_grey(start=0,end=.7)+xlab("Wildfire Hazard Severity Zone")+ theme( axis.text.x  = element_text(angle=90,vjust=0.3))
  
  # * used in paper * 
  windows()
  ggplot(FIREP_All[FIREP_All$Time>2000 & FIREP_All$Haz_Code!=-2,], aes(x=Time,y=Houses,fill=Scenario,width=3)) + 
    geom_bar(stat="identity",width=10, position=position_dodge(width = 12))+facet_grid(~Risk_Class)+ scale_fill_grey(start=0,end=.7)+xlab("Wildfire Hazard Severity Zone")+ theme( axis.text.x  = element_text(angle=90,vjust=0.3))
  
  windows()
  ggplot(FIREP_All[FIREP_All$Time>2000 & FIREP_All$Haz_Code!=-3,], aes(x=Time,y=Houses,fill=Scenario,width=3)) + 
    geom_bar(stat="identity",width=10, position=position_dodge(width = 12))+facet_grid(~Risk_Class)+ scale_fill_grey(start=0,end=.7)+xlab("Wildfire Hazard Severity Zone")+ theme( axis.text.x  = element_text(angle=90,vjust=0.3))+ coord_cartesian(ylim=c(0,2500000) )
    
  # * used in paper * 
  #TABLE ?: HOUSING COUNTS BY DENSITY CLASSIFICATION IN 2049 from above
  #aggregate(Houses ~   Scenario+Time+Risk_Class,  FIREP_All, sum)

  find_houses <- function(time, class, scenario){
    all_data <<- FIREP_All
    all_data$Houses[all_data$Time==time & all_data$Risk_Class==class & all_data$Scenario==scenario] 
  }
  
  find_houses(2049,'Very High','BAU Growth')  #1708204
  find_houses(2049,'Very High','Rural Growth') #2151088
  find_houses(2049,'Very High','Urban Growth') #1576485

  find_houses(2049,'Very High','Urban Growth')-find_houses(1999,'Very High','BAU Growth')  #644858.6
  find_houses(2049,'Very High','Rural Growth')-find_houses(1999,'Very High','BAU Growth')  #1219461




  ### new plot 
  # fire density housing density

  fire7100 = raster('G:\\Faculty\\Mann\\Share\\Fire\\RastersInput\\fire_7100.tif')
  fire7100 = as.numeric(fire7100[1:dim(fire7100)[1],1:dim(fire7100)[2]])
  den =   raster('G:\\Faculty\\Mann\\Share\\Fire\\RastersInput\\den_99_1km.tif')
  den=as.numeric(den[1:dim(den)[1],1:dim(den)[2]])
  denfire = data.frame(den = den, fire = fire7100)
  windows()
  
  plot(y=den,x=fire7100, xlab='housing density', ylab='fire count', xlim=c(0,20))
  abline(lm())  


  # best so far but doesn't deflate for acres
  qplot(den, data=denfire, weight=fire7100, geom="histogram",binwidth =.25)+ coord_cartesian(xlim=c(0,10),ylim=c(0,100) ) +ylab("Fire Pixel Count")+xlab('Housing Density / ac')
  
  
  #########
  # deflate fire counts by acres  SBG level
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")
  fire7100 = raster('G:\\Faculty\\Mann\\Share\\Fire\\RastersInput\\fire_7100.tif')
  windows()  
  plot(fire7100)
  plot(hden_p)
    
  library(snow)
  beginCluster(type='SOCK')
  ext = extract(fire7100,hden_p, small=T, fun=sum, na.rm=T)
  extlist = list(ext)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\Fire\\ext.RData')
  endCluster()

  hden_p$fire_sum = ext    
  hden_p$fire_sum_ac =  hden_p$fire_sum / hden_p$Acres  
       
  windows()
  qplot((HuDac4_79+HuDac4_89+HuDac4_99)/3, data=as.data.frame(hden_p), weight=fire_sum_ac, geom="histogram",binwidth =.25)+ylab("Fire Pixels / ac")+xlab('Housing Density / ac')+ coord_cartesian(xlim=c(0,15),ylim=c(0,12) )
  qplot((HuDac4_79+HuDac4_89+HuDac4_99)/3, data=as.data.frame(hden_p), weight=fire_sum_ac , geom="histogram",binwidth =.15)+ylab("Fire Pixels / ac")+xlab('Housing Density / ac')+ coord_cartesian(xlim=c(0,15),ylim=c(0,1) )+ geom_density(adjust=1/5)
  #fire7100 = data.frame(fire=as.numeric(fire7100[1:dim(fire7100)[1],1:dim(fire7100)[2]]))
  #qplot(fire, data=fire7100 , geom="histogram",binwidth =.15 )
  windows()  
  ggplot(as.data.frame(hden_p)[hden_p$fire_sum_ac!=0,], aes((HuDac4_79+HuDac4_89+HuDac4_99)/3, fire_sum_ac)) + geom_point(alpha = 1/5)
  ggplot(as.data.frame(hden_p)[hden_p$fire_sum_ac!=0,], aes((HuDac4_79+HuDac4_89+HuDac4_99)/3, fire_sum_ac)) + geom_point(alpha = 1/5)
       
  #########
  # county level 
  county = readOGR("G:\\Faculty\\Mann\\Share\\Boundary_Files\\geodatabase","County_dis")
  beginCluster(type='SOCK')
  fire7100_2 = raster('G:\\Faculty\\Mann\\Share\\Fire\\RastersInput\\fire_7100.tif')
  ext_c = extract(fire7100_2,county, small=T, fun=sum, na.rm=T)     
  extlist_c = list(ext_c)
  save(extlist_c,file='G:\\Faculty\\Mann\\Share\\Fire\\ext_c.RData')
  county$fire_sum = ext_c
  county$fire_sum_km = county$fire_sum / county$SUM_km2

  ggplot(as.data.frame(county)[county$fire_sum_km!=0,], aes(SUM_km2 , fire_sum_km)) + geom_point(alpha = 1/5,)+ stat_smooth() + coord_cartesian(xlim=c(0,50000),ylim=c(0,.4) )

  # bring in population
  pop= read.csv("G:\\Faculty\\Mann\\Share\\Population_Forecast\\POPHIST0.csv")
  county_pop = merge(county, pop, by.x='NAME_UCASE', by.y='County', all.x=T)
  county_pop$pop_km = county_pop$POP_2000 /county_pop$SUM_km2

  # place where there are fires
  windows()
  ggplot(as.data.frame(county_pop)[county$fire_sum_km!=0,], aes(pop_km , fire_sum_km, label=NAME_UCASE)) + geom_point(alpha = 1/5,)+ stat_smooth()+ geom_text(hjust=0, vjust=0, size =2 )  
  # all counties with or without fires 
  windows()
  ggplot(as.data.frame(county_pop) , aes(pop_km , fire_sum_km, label=NAME_UCASE)) + geom_point(alpha = 1/5,)+ stat_smooth()  + coord_cartesian(xlim=c(0,1000)  )+ geom_text(hjust=0, vjust=0, size =2 )


############################################################################
############################################################################
  # urban mean distance from wildlands 

  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")
  disttowildland = raster('G:\\Faculty\\Mann\\Share\\Wild_Dist_to_Urban/EucDistWildLand_m.tif')  
  library(snow)

#   for( j in seq(59,149, by=10)){
#     hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")
#     disttowildland = raster('G:\\Faculty\\Mann\\Share\\Wild_Dist_to_Urban/EucDistWildLand_m.tif')  
#     
#     # 1960
#     assign('density',as.numeric(hden_p[,paste('HuDac4_',j,sep='')]))
#     assign(paste('hden_p',j,sep=''),value=hden_p[  density >=1,])
#     hden_p59 = hden_p[hden_p$HuDac4_59>=1,]
#     beginCluster(4, type='SOCK')
#     ext = extract(disttowildland, hden_p59,small=T, fun=mean, na.rm=T,sp=T)
#     extlist = list(ext,hden_p59)
#     save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p59_mndist.RData')
#     endCluster()
#     remove(ext,extlist)
#     
#   }


  # 1960
  hden_p59 = hden_p[hden_p$HuDac4_59>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p59,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p59)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p59_mindist.RData')
  remove(hden_p59,ext,extlist)
  endCluster()
  remove(ext,extlist)

  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 1970
  hden_p69 = hden_p[hden_p$HuDac4_69>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p69,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p69)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p69_mindist.RData')
  remove(hden_p69,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 1980
  hden_p79 = hden_p[hden_p$HuDac4_79>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p79,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p79)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p79_mindist.RData')
  remove(hden_p79,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 1990
  hden_p89 = hden_p[hden_p$HuDac4_89>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p89,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p89)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p89_mindist.RData')
  remove(hden_p89,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 2000
  hden_p99 = hden_p[hden_p$HuDac4_99>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p99,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p99)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p99_mindist.RData')
  remove(hden_p99,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 2010
  hden_p109b = hden_p[hden_p$HuDac4_109>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p109b,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p109b)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p109_mindist.RData')
  remove(hden_p109b,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 2020
  hden_p119b = hden_p[hden_p$HuDac4_119>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p119b,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p119b)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p119_mindist.RData')
  remove(hden_p119b,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 2030
  hden_p129b = hden_p[hden_p$HuDac4_129>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p129b,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p129b)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p129_mindist.RData')
  remove(hden_p129b,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 2040
  hden_p139b = hden_p[hden_p$HuDac4_139>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p139b,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p139b)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p139_mindist.RData')
  remove(hden_p139b,ext,extlist)
  endCluster()
  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")

  # 2050
  hden_p149b = hden_p[hden_p$HuDac4_149>=1,]
  beginCluster(4, type='SOCK')
  ext = extract(disttowildland, hden_p149b,small=T, fun=min, na.rm=T,sp=T)
  extlist = list(ext,hden_p149b)
  save(extlist,file='G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\hden_p149_mindist.RData')
  remove(hden_p149b,ext,extlist)
  endCluster()
 

  # extract data from polygons
  setwd('G:\\Faculty\\Mann\\Share\\MeanDistToWildLands\\')
  directory = sort(dir(),decreasing=T)
  baseyear = 59
  for(i in 1:length(directory)){    # might have messed up "hden_p129_mndist.RData" 
    load( directory[i] )
    assign( paste('WDist',baseyear,sep="_"), as.data.frame(extlist[[1]]$EucDistWildLand_m ))
    temp = get(paste('WDist',baseyear,sep="_"))
    temp$Year = baseyear+1900
    assign( paste('WDist',baseyear,sep="_"),temp)
    baseyear = baseyear+10
    remove(extlist,i)
  }
  


  ############################################################################
  ############################################################################
  # water recharge


  hden_p = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\HousingFinal","HousingFinal")
  water0010 = raster('G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\rch2000_2009_PrcR_A2.tif')
  water1020 = raster('G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\rch2000_2009_PrcR_A2.tif')
  water2030 = raster('G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\rch2000_2009_PrcR_A2.tif')
  water3040 = raster('G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\rch2000_2009_PrcR_A2.tif')
  water4050 = raster('G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\rch2000_2009_PrcR_A2.tif')
 
  #sum water % account for in each block group 
  library(snow)
  beginCluster(4,type='SOCK')
  ext0010 = extract(water0010,hden_p, small=T, fun=sum, na.rm=T)
  ext0010 = list(ext0010)
  save(ext0010,file='G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\waterext0010.RData')
  endCluster()
  remove(ext0010)
  beginCluster(4,type='SOCK')
  ext1020 = extract(water1020,hden_p, small=T, fun=sum, na.rm=T)
  ext1020 = list(ext1020)
  save(ext1020,file='G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\waterext1020.RData')
  endCluster()
  remove(ext1020 )  
  beginCluster(4,type='SOCK')
  ext2030 = extract(water2030,hden_p, small=T, fun=sum, na.rm=T)
  ext2030 = list(ext2030)
  save(ext2030,file='G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\waterext2030.RData')
  endCluster()
  remove(ext2030 )   
  beginCluster(4,type='SOCK')
  ext3040 = extract(water3040,hden_p, small=T, fun=sum, na.rm=T)
  ext3040 = list(ext3040)
  save(ext3040,file='G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\waterext3040.RData')
  endCluster()
  remove(ext3040 )   
  beginCluster(4,type='SOCK')
  ext4050 = extract(water4050,hden_p, small=T, fun=sum, na.rm=T)
  ext4050 = list(ext4050)
  save(ext4050,file='G:\\Faculty\\Mann\\GFDLA2_BCM\\WaterAggregated1080\\Summary\\waterext4050.RData')
  endCluster()
  remove(ext4050 )   














############################################################################
# Polygon plots  # not use in paper
load(paste('G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.7TUrbNS+D20km','workspace.RData',sep='\\'))
#https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
poly = Block.39.ex
poly@data$id = rownames(poly@data)
poly.points = fortify(poly, region="id")
poly.df = join(poly.points, poly@data, by="id")

save.image( poly.points,'G:\\Faculty\\Mann\\Share\\Plots\\polypoints.RData' )

ggplot(poly.df) + 
  aes(long,lat,group=group,fill=LEVEL3_NAM) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("Utah Ecoregion")





# not used
# prepare data  aggregated as the median county level      
    P_BAU$Density = P_BAU$HuD
    P_BAU$WUClass[P_BAU$Density >= 4] = 'DenseUrban'
    P_BAU$WUClass[P_BAU$Density >= 1 & P_BAU$Density < 4] = 'Urban'
    P_BAU$WUClass[P_BAU$Density >= 0.41 & P_BAU$Density < 1] = 'Interface'
    P_BAU$WUClass[P_BAU$Density >= 0.025 & P_BAU$Density < 0.41] = 'Intermix'
    P_BAU$WUClass[P_BAU$Density < 0.025 ]       = 'Sparse'
    P_BAU$WUClass= factor(P_BAU$WUClass,levels=c('DenseUrban','Urban','Interface','Intermix','Sparse'),ordered=T)
    P_BAU$Houses = P_BAU$Density*P_BAU$Acres

    P_R$Density = P_R$HuD
    P_R$WUClass[P_R$Density >= 4] = 'DenseUrban'
    P_R$WUClass[P_R$Density >= 1 & P_R$Density < 4] = 'Urban'
    P_R$WUClass[P_R$Density >= 0.41 & P_R$Density < 1] = 'Interface'
    P_R$WUClass[P_R$Density >= 0.025 & P_R$Density < 0.41] = 'Intermix'
    P_R$WUClass[P_R$Density < 0.025  ]     = 'Sparse'
    P_R$WUClass= factor(P_R$WUClass,levels=c('DenseUrban','Urban','Interface','Intermix','Sparse'),ordered=T)
    P_R$Houses = P_R$Density*P_R$Acres

    P_U$Density = P_U$HuD
    P_U$WUClass[P_U$Density >= 4] = 'DenseUrban'
    P_U$WUClass[P_U$Density >= 1 & P_U$Density < 4] = 'Urban'
    P_U$WUClass[P_U$Density >= 0.41 & P_U$Density < 1] = 'Interface'
    P_U$WUClass[P_U$Density >= 0.025 & P_U$Density < 0.41] = 'Intermix'
    P_U$WUClass[P_U$Density < 0.025  ]     = 'Sparse'
    P_U$WUClass= factor(P_U$WUClass,levels=c('DenseUrban','Urban','Interface','Intermix','Sparse'),ordered=T)
    P_U$Houses = P_U$Density*P_U$Acres
    Tacres = sum(P_U$Acres) 

    # aggregate by time and wui     
    A_BAU = aggregate(cbind(Acres,Houses) ~Time+WUClass , data = P_BAU,sum)
    A_R = aggregate(cbind(Acres,Houses)~Time+WUClass , data = P_R,sum)
    A_R = rbind(A_BAU[A_BAU$Time<2009,],A_R)
    A_U = aggregate(cbind(Acres,Houses)~Time+WUClass , data = P_U,sum)
    A_U = rbind(A_BAU[A_BAU$Time<2009,],A_U)
    A_BAU$PerAcres = A_BAU$Acres /sum(A_BAU$Acres)*100
    A_R$PerAcres = A_R$Acres /sum(A_R$Acres)*100
    A_U$PerAcres = A_U$Acres /sum(A_U$Acres)*100


# Acrage stacked area plots 
    colours <- c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494") 
    ba=ggplot(A_BAU,aes(x = Time, y = PerAcres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
        geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')+ labs(title="BAU Growth" )+ theme(legend.position = "none") +xlab('')
    ra=ggplot(A_R,aes(x = Time, y = PerAcres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
        geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey') + labs(title="Rural Growth" )+ theme(legend.position = "none")    +ylab('')
    ua=ggplot(A_U,aes(x = Time, y = PerAcres, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
        geom_area(alpha=.75, position = 'stack') + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')  labs(title="Urban Growth" )+ theme(legend.position = "none")+xlab('')+ylab('')

# house stacked area counts
    bb=ggplot(A_BAU,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
        geom_area(alpha=.75) + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')+opts( legend.position="none")+xlab('')
    rb=ggplot(A_R,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
        geom_area(alpha=.75) + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')   +opts( legend.position="none")+ylab('')
    ub=ggplot(A_U,aes(x = Time, y = Houses, fill=WUClass, group=WUClass))+ scale_fill_manual (values=colours)+
        geom_area(alpha=.75) + geom_line(aes(ymax=Acres), position="stack", colour='darkgrey')   +opts( legend.position="none")+xlab('')+ylab('') + opts(legend.direction = "horizontal", legend.position = c(-1.1, 1.1), legend.background = theme_rect(colour = 'black', fill = 'grey90', size = .5, linetype='solid'))
                                                                                                                                           
                                                                                                                                          
# All combined 
windows() 
grid.arrange(ba,ra,ua,bb,rb,ub,ncol=3)  # note: corner missing because of zero density areas 

 


#












# 
# all_SBGHDBAU2_yr$Scenario = "BAU"
# all_HDU2_yr$Scenario = "Urb"
# all_HDR2_yr$Scenario = "Rur"
# 
# all_HDBUR_yr = rbind(all_HDR2_yr,all_SBGHDBAU2_yr,all_HDU2_yr)
# all_HDR2_yr$Scenario = factor(all_HDR2_yr$Scenario, levels=c('BAU','Rur','Urb'),ordered=T)
# ggplot(all_HDBUR_yr, aes(Density, fill = Scenario)) + geom_density(alpha=.5  )  + xlab("Median SBG Housing Density by Scenario") + coord_cartesian(ylim = c(0, .25), xlim=c(0,12)) 
# 
# all_HDBUR_yr = rbind(all_HDR2_yr,all_SBGHDBAU2_yr,all_HDU2_yr)
# all_HDBUR_yr = all_HDBUR_yr[all_HDBUR_yr$Scenario=="Rur",]
# a =ggplot(all_HDBUR_yr, aes(Density, fill = Year)) + geom_density(alpha=.5  )    + coord_cartesian(ylim = c(0, .2), xlim=c(0,12)) +opts(title='Rural Growth',legend.position="none")
# 
# all_HDBUR_yr = rbind(all_HDR2_yr,all_SBGHDBAU2_yr,all_HDU2_yr)
# all_HDBUR_yr = all_HDBUR_yr[all_HDBUR_yr$Scenario=="BAU",]
# b=ggplot(all_HDBUR_yr, aes(Density, fill = Year)) + geom_density(alpha=.5  )  + xlab("Median SBG Housing Density by Scenario") + coord_cartesian(ylim = c(0, .2), xlim=c(0,12)) +opts(title='BAU',legend.position="none")
# 
# all_HDBUR_yr = rbind(all_HDR2_yr,all_SBGHDBAU2_yr,all_HDU2_yr)
# all_HDBUR_yr = all_HDBUR_yr[all_HDBUR_yr$Scenario=="Urb",]
# c=ggplot(all_HDBUR_yr, aes(Density, fill = Year)) + geom_density(alpha=.5  )    + coord_cartesian(ylim = c(0, .2), xlim=c(0,12)) +opts(title='Urban Growth',legend.position="none")
# 
# library(gridExtra)
# windows()
# grid.arrange(a,b,c,ncol=3)

# # plot difference between BAU and rural urban 
# diff_UrbBAU = all_HDU2_yr
# diff_UrbBAU$Density = diff_UrbBAU$Density - all_HDBAU2_yr$Density
# diff_RurBAU = all_HDR2_yr
# diff_RurBAU$Density = diff_RurBAU$Density - all_HDBAU2_yr$Density
# 
# ggplot(diff_UrbBAU, aes(Density, fill = Year)) + geom_density(alpha=.5 )  + xlab("Median County Housing Density by Year") + coord_cartesian( xlim=c(-3,3)) 
# ggplot(diff_RurBAU, aes(Density, fill = Year)) + geom_density(alpha=.5 )  + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, 1), xlim=c(-3,3)) 


# 
# # get difference between BAU and R and U and aggregate by county 
# BAU3 = BAU2[,c("COUNTY",'NAME','HuDB_109','HuDB_119','HuDB_129','HuDB_139','HuDB_149')]
# Dif_R2_BAU2 =cbind(BAU3[,c('COUNTY','NAME')], R2[,3:7] - BAU3[,3:7] )
# Dif_U2_BAU2 =cbind(BAU3[,c('COUNTY','NAME')], U2[,3:7] - BAU3[,3:7] )
# 
# Dif_R2_BAU2_R = aggregate(. ~   NAME,  Dif_R2_BAU2, median)
# ## tile densities 
# for (i in seq(109,149,by=10)){
#     year = grep(as.character(i),names(R2) )[1]
#     assign(paste("DifR2",i,sep="_"), cbind(as.character(Dif_R2_BAU2_R[,"NAME"]),unfactor(Dif_R2_BAU2_R[,year]) ,paste("Y_",i,sep=""))  )
#     # this assign is used to hepl with ggplot histograms
# }
# 
# DIfR2_yr = as.data.frame( rbind(DifR2_109,DifR2_119,DifR2_129,DifR2_139,DifR2_149) ,stringsAsFactors=F )
# names(DIfR2_yr) = c("Name","Density","Year")
# DIfR2_yr$Density = as.numeric(DIfR2_yr$Density)
# DIfR2_yr$Year =  factor(DIfR2_yr$Year,levels=c( "Y_109","Y_119","Y_129","Y_139","Y_149"),c( "2009","2019","2029","2039","2049"))
# 
# g = ggplot(DIfR2_yr, aes(Year, Name)) +geom_tile(aes(fill = Density),colour = "white")  
# g + theme_grey(base_size = 10) +  scale_fill_gradientn(colours = c("gray98","gray87","gray77","gray67","gray57","gray47","gray37","gray27" ) ,breaks=c(.25,.5,1,1.5,3))
# 
# 
# ##########



for (i in seq(39,149,by=10)){
    year = grep(as.character(i),names(A_BAU) )[1]     # find column # for current year, if two options 39 vs 139 choose first
    assign(paste("HD",i,sep="_"), cbind(as.character(A_BAU[,"NAME"]),unfactor(A_BAU[,year]) ,paste("Y_",i,sep=""))  )
    # this assign is used to hepl with ggplot histograms
}

all_HD_yr = as.data.frame( rbind( HD_59,HD_69,HD_79,HD_89,HD_99,HD_109,HD_119,HD_129,HD_139,HD_149) ,stringsAsFactors=F )
names(all_HD_yr) = c("Name","Density","Year")
all_HD_yr$Density = as.numeric(all_HD_yr$Density)
all_HD_yr$Year =  factor(all_HD_yr$Year,levels=c( "Y_59","Y_69","Y_79","Y_89","Y_99","Y_109","Y_119","Y_129","Y_139","Y_149"),c("1959","1969","1979","1989","1999","2009","2019","2029","2039","2049"))
ggplot(all_HD_yr, aes(Density, fill = Year)) + geom_density(alpha=.4, colour ="grey33") +scale_fill_grey(start=0,end=1 ) + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, 2.5), xlim=c(0,5)) 
ggplot(all_HD_yr, aes(Density, fill = Year)) + geom_density(alpha=.25 )  + xlab("Median County Housing Density by Year") + coord_cartesian(ylim = c(0, 1.25), xlim=c(0,5)) 




############################################################################
### raster time plots

###      NOTE: THIS MIGHT BE A GOOD WAY TO PRESENT THE FORECAST DATA!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  library(rasterVis)
  library(zoo)

  
  npark = raster("G:\\Faculty\\Mann\\Share\\Roads\\Dist_Path\\dist_nparks1.tif")
  D1939 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1939R2.tif")
  D1939[D1939<0] = 0  # corret resampling error
  D1949 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1949R2.tif")
  D1949[D1949<0] = 0 
  D1959 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1959R2.tif")
  D1959[D1959<0] = 0  
  D1969 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1969R2.tif")
  D1969[D1969<0] = 0  
  D1979 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1979R2.tif")
  D1979[D1979<0] = 0  
  D1989 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1989R2.tif")
  D1989[D1989<0] = 0  
  D1999 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D1999R2.tif")
  D1999[D1999<0] = 0  
  D2009 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D2009R2.tif")  # used rasterize in arc to build these
  D2009[D2009<0] = 0
  D2009 =  crop(D2009, extent(D1999))
  D2019 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D2019R2.tif")
  D2019 =  crop(D2019, extent(D1999))
  D2019[D2019<0] = 0
  D2029 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D2029R2.tif")
  D2029 =  crop(D2029, extent(D1999))
  D2029[D2029<0] = 0
  D2039 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D2039R2.tif")
  D2039 =  crop(D2039, extent(D1999))
  D2039[D2039<0] = 0
  D2049 = raster("G:\\Faculty\\Mann\\Share\\Plots\\DensityRaters\\D2049R2.tif")
  D2049 =  crop(D2049, extent(D1999))
  D2049[D2049<0] = 0

  # density = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion6")
  # D1939 = rasterize(density,npark, "HuDac4_39")
  # writeRaster(D1939,"G:\\Faculty\\Mann\\Share\\Plots\\D1939.tif",format="GTiff", overwrite=T)
  # D1949 = rasterize(density,npark, "HuDac4_49")
  # writeRaster(D1949,"G:\\Faculty\\Mann\\Share\\Plots\\D1949.tif",format="GTiff", overwrite=T)
  # D1959 = rasterize(density,npark, "HuDac4_59")
  # writeRaster(D1959,"G:\\Faculty\\Mann\\Share\\Plots\\D1959.tif",format="GTiff", overwrite=T)
  # D1969 = rasterize(density,npark, "HuDac4_69")
  # writeRaster(D1969,"G:\\Faculty\\Mann\\Share\\Plots\\D1969.tif",format="GTiff", overwrite=T)
  # D1979 = rasterize(density,npark, "HuDac4_79")
  # writeRaster(D1979,"G:\\Faculty\\Mann\\Share\\Plots\\D1979.tif",format="GTiff", overwrite=T)
  # D1989 = rasterize(density,npark, "HuDac4_89")
  # writeRaster(D1989,"G:\\Faculty\\Mann\\Share\\Plots\\D1989.tif",format="GTiff", overwrite=T)
  # D1999 = rasterize(density,npark, "HuDac4_99")
  # writeRaster(D1999,"G:\\Faculty\\Mann\\Share\\Plots\\D1999.tif",format="GTiff", overwrite=T)
  #IMPORTANT: ran resample.py to resample data to 250m rather than 100m (taking too long for plotting)
  #density = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity6.4TUrbNS+","SpecifyEcoCity6.4TUrbNS+")
 

  density_stack = stack(D1939,D1949,D1959,D1969,D1979,D1989,D1999,D2009,D2019,D2029,D2039,D2049)
  idx = seq(as.Date("1939-01-01"),as.Date("2049-01-01") ,'year' )
  idx = idx[c(1,11,21,31,41,51,61,71,81,91,101,111)]   # keep only valid decades
  
  density_stack <- setZ(density_stack, idx)
  layerNames(density_stack) <- as.character(c(D1939,D1949,D1959,D1969,D1979,D1989,D1999,D2009,D2019,D2029,D2039,D2049))

#pdf("G:\\Faculty\\Mann\\Share\\Plots\\plot.pdf", height=7, width=7)
library(rasterVis)
  hovmoller(density_stack, dirXY=y,contour=F, panel=panel.levelplot.raster,
          interpolate=F, par.settings=RdBuTheme,   na.rm=T)
#dev.off()
 
#pdf("G:\\Faculty\\Mann\\Share\\Plots\\plot.pdf", height=7, width=7)
hovmoller(density_stack, dirXY=y,contour=F, panel=panel.levelplot.raster,
          interpolate=F, par.settings=GrTheme,   na.rm=T)
#dev.off()

############################################################################
## PLOT RESIDUAL VARIOGRAM

### IMPORT POLYGONS AND ITS DBF FILE
Block.39.ex = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion6")
Block.dbf = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")   #read dbf file for edit
  
### remove rows with more than 2 (expected) NAs
Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]

get_data(urban=1,counties=unique(Block.dbf$COUNTY),urban_rural_threshold=1)
  

library(PBSmapping)  # get real centroids in lat lon WGS 1984
sample_poly = Block.39.ex[Block.39.ex$SUM090 %in% panel.specify$SUM090, ]
sample_poly = spTransform(sample_poly, CRS("+proj=longlat +datum=WGS84"))
Block.39.exPS = SpatialPolygons2PolySet(sample_poly)
centroids = calcCentroid(Block.39.exPS,rollup=1)
panel.specify$id = unfactor(panel.specify$id)
panel.specify = pdata.frame(merge(panel.specify,centroids, by.x="id", by.y="PID"), index=c("id","time"))

panel.specify$INTPTLAT = panel.specify$Y   # replace with real lat lon
panel.specify$INTPTLON = panel.specify$X

KNN4<<-knn2nb(knearneigh(coordinates(sample_poly),k=4))   #12
WKNN4 <<- nb2listw(KNN4, style="W")

KNN1<<-knn2nb(knearneigh(coordinates(sample_poly),k=1))    # distance
DNN3<<-dnearneigh(coordinates(sample_poly),0,70,longlat=T)    
for(x in 1:length(DNN3)){     # replace empty with k near neigh
  if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
}
WDNN3 <<- nb2listw(DNN3, style="W")

f3 = HuDac4 ~ HuDaclag+HuDaclagSqr+HuDaclagQb
out = spml(formula=f3,data=panel.specify,listw=WKNN4,model="pooling",effect="time")
out2 = plm(formula=f3,data=panel.specify,model="pooling",effect="time")
summary(out)
summary(out2)
names(out)

samples_error = out$residuals[ grep("99", names(out$residuals))]
samples_error2 = out2$residuals[ grep("99", names(out$residuals))]   # doesnt have time in name, use previous rows
samples =  as.data.frame(panel.specify[ row.names(panel.specify) %in% names(samples_error),] )  # get year 99 data
samples2 =  as.data.frame(panel.specify[ row.names(panel.specify) %in% names(samples_error),] )  # get year 99 data

samples = data.frame(samples,error = samples_error)  # add in error  already checked that id and time matched
samples2 = data.frame(samples2,error = samples_error2)  # add in error  already checked that id and time matched
coordinates(samples) = ~ INTPTLAT+INTPTLON
coordinates(samples2) = ~ INTPTLAT+INTPTLON

spplot(samples, "error") 
spplot(samples2, "error") 
library(gstat)

space = variogram(error~INTPTLAT+INTPTLON,  data=samples)
not_space = variogram(error~INTPTLAT+INTPTLON,   data=samples2)
plot(space$dist,space$gamma)
points(not_space$dist, not_space$gamma,col="red")

############################################################################
####  moran's I plot 
indata = as.data.frame(panel.specify[grep("99",row.names(panel.specify))])
indata$INTPTLAT = unfactor(indata$INTPTLAT)
indata$INTPTLON = unfactor(indata$INTPTLON)
coordinates(indata) = ~ INTPTLON+INTPTLAT
indata = indata[-c(468,42),]
KNN3<<-knn2nb(knearneigh(coordinates(indata),k=4))   
WKNN3 <<- nb2listw(KNN3, style="W")
moran.plot( as.vector(indata$HuDac4),WKNN3, main="k=4")

KNN1<<-knn2nb(knearneigh(coordinates(indata),k=1))    
DNN3<<-dnearneigh(coordinates(indata),0,7,longlat=T)    
for(x in 1:length(DNN3)){     # replace empty with k near neigh
  if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
}
WDNN3 <<- nb2listw(DNN3, style="W")
moran.plot( as.vector(indata$HuDac4),WDNN3 )

#library(GeoXp)
#indata = as.data.frame(panel.specify[grep("99",row.names(panel.specify))])
#coordinates(indata) = ~ INTPTLON+INTPTLAT
#moranplotmap(indata,"HuDac4",WDNN3)



############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################



############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################
############################################################################




## plot rho and siginificance for rural and urban at different distances and K neigh

### IMPORT POLYGONS AND ITS DBF FILE
Block.39.ex = readOGR("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion6")
Block.dbf = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")   #read dbf file for edit

### remove rows with more than 2 (expected) NAs
Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]

#get_data(urban=1,counties=c(37,59,65,71,73,111),urban_rural_threshold=1)

#sample_poly = Block.39.ex[Block.39.ex$SUM090 %in% panel.specify$SUM090, ]
 

#############################
#   KNEIGH

VERSION = "6.7TUrbNS+PD25kmb"
DIRECTORY = paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep="") 

CntyJep = read.csv(file=paste(DIRECTORY,"\\CntyJepAREACity",VERSION,".csv",sep=""))
jepFIDS = by(CntyJep[,"FIPS"],CntyJep[,"jepson_Unq_ID_edit"],unique)

runit <- function(counties){
    leading_county_num = counties[1]
    spec_holder = read.table(file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,".csv",sep=""),header=T, sep=",", stringsAsFactors=T)
    
    for (urban1 in  1:2){  
        print(paste(urban1, leading_county_num))
        dist_out = data.frame(order=c(rep(is.numeric(NA),15)), rho=c(rep(is.numeric(NA),15)), rho_p=c(rep(is.numeric(NA),15)), sqrerr=c(rep(is.numeric(NA),15)), aic=c(rep(is.numeric(NA),15))  )
        ### remove rows with more than 2 (expected) NAs
        Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
        Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]
        
        get_data(urban=urban1,counties=counties,urban_rural_threshold=1)
        sample_poly = Block.39.ex[Block.39.ex$SUM090 %in% panel.specify$SUM090, ]
        
        spec_fn = as.formula( paste(spec_holder[spec_holder$county==paste(counties, collapse=" ") &  spec_holder$urban==urban1,"formulas"],sep="")   )
        j=1
        for(i in c(1,2,3,4,5,10,15,25,50)){
            KNN1<-knn2nb(knearneigh(coordinates(sample_poly),k=i))    # distance
            WDNN3 <- nb2listw(KNN1, style="W")
            
            out = tryCatch( {spml(formula=spec_fn,data=panel.specify,listw=WDNN3,model="pooling",effect="time") }, error = function(err) {print(paste("Avoid_ERROR:  ",err))
                                                                                                                                          return("errors") })            
            summary(out)
            dist_out$order[j] = as.numeric(i)
            if(out!="errors"){   # only if out didn't produce error
                
                k=(length(out$coefficients)-1)
                n=dim(panel.specify)[1]                
                AIC3a = 2*k - 2*out$logLik                
                AICc3a = AIC3a + (2*k*(k+1))/(n-k-1)
                
                dist_out$rho[j]   = summary(out)$ErrCompTable[1]
                dist_out$rho_p[j] = summary(out)$ErrCompTable[4]  
                dist_out$sqrerr[j] = sum(out$residuals^2,na.rm=T)
                dist_out$aic[j] = AICc3a
            }
            print(dist_out)
            j=j+1
        }
        
        write.csv(dist_out, paste("G:\\Faculty\\Mann\\Share\\Plots\\kneigh_out_",urban1,"_",leading_county_num,".csv",sep=""))        
    }
}

lapply( jepFIDS[1:length(jepFIDS) ], function(x) runit(unlist(x)))


#######################################
# distance 

VERSION = "6.7TUrbNS+PD25kmb"
DIRECTORY = paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep="") 

CntyJep = read.csv(file=paste(DIRECTORY,"\\CntyJepAREACity",VERSION,".csv",sep=""))
jepFIDS = by(CntyJep[,"FIPS"],CntyJep[,"jepson_Unq_ID_edit"],unique)

runit <- function(counties){
    leading_county_num = counties[1]
    spec_holder = read.table(file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,".csv",sep=""),header=T, sep=",", stringsAsFactors=T)
    
    for (urban1 in  1:2){  
        print(paste(urban1, leading_county_num))
        dist_out = data.frame(order=c(rep(is.numeric(NA),15)), rho=c(rep(is.numeric(NA),15)), rho_p=c(rep(is.numeric(NA),15)), sqrerr=c(rep(is.numeric(NA),15)), aic=c(rep(is.numeric(NA),15))  )
        ### remove rows with more than 2 (expected) NAs
        Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
        Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]
        
        get_data(urban=urban1,counties=counties,urban_rural_threshold=1)
        sample_poly = Block.39.ex[Block.39.ex$SUM090 %in% panel.specify$SUM090, ]
        
        spec_fn = as.formula( paste(spec_holder[spec_holder$county==paste(counties, collapse=" ") &  spec_holder$urban==urban1,"formulas"],sep="")   )
        KNN1<-knn2nb(knearneigh(coordinates(sample_poly),k=1))    
        j=1
         for(i in c(1000,5000,10000,15000,20000,25000,50000,100000)){
           
            DNN3<-dnearneigh(coordinates(sample_poly),0,i,longlat=F)    
            for(x in 1:length(DNN3)){     # replace empty with k near neigh
                if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
            }
            WDNN3 <- nb2listw(DNN3, style="W")            
            out = tryCatch( {spml(formula=spec_fn,data=panel.specify,listw=WDNN3,model="pooling",effect="time") }, error = function(err) {print(paste("Avoid_ERROR:  ",err))
                                                                                                                                          return("errors") })            
            summary(out)
            dist_out$order[j] = as.numeric(i)
            if(out!="errors"){   # only if out didn't produce error
                dist_out$rho[j]   = summary(out)$ErrCompTable[1]
                dist_out$rho_p[j] = summary(out)$ErrCompTable[4]                                 
                k=(length(out$coefficients)-1)
                n=dim(panel.specify)[1]                
                AIC3a = 2*k - 2*out$logLik                
                AICc3a = AIC3a + (2*k*(k+1))/(n-k-1)                
                dist_out$sqrerr[j] = sum(out$residuals^2,na.rm=T)
                dist_out$aic[j] = AICc3a
                               
            }
            print(dist_out)
            j=j+1
        }        
        write.csv(dist_out, paste("G:\\Faculty\\Mann\\Share\\Plots\\dist_out_",urban1,"_",leading_county_num,".csv",sep=""))        
    }
}
lapply( jepFIDS[1:length(jepFIDS) ], function(x) runit(unlist(x)))

 

################################
#   polygon adjacency and higher order

VERSION = "6.7TUrbNS+PD25kmb"
DIRECTORY = paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep="") 

CntyJep = read.csv(file=paste(DIRECTORY,"\\CntyJepAREACity",VERSION,".csv",sep=""))
jepFIDS = by(CntyJep[,"FIPS"],CntyJep[,"jepson_Unq_ID_edit"],unique)

runit <- function(counties){
    leading_county_num = counties[1]
    spec_holder = read.table(file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,".csv",sep=""),header=T, sep=",", stringsAsFactors=T)
    
    for (urban1 in  1:2){  
        print(paste(urban1, leading_county_num))
        dist_out = data.frame(order=c(rep(is.numeric(NA),15)), rho=c(rep(is.numeric(NA),15)), rho_p=c(rep(is.numeric(NA),15)), sqrerr=c(rep(is.numeric(NA),15)), aic=c(rep(is.numeric(NA),15))  )
        ### remove rows with more than 2 (expected) NAs
        Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
        Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]
        
        get_data(urban=urban1,counties=counties,urban_rural_threshold=1)
        sample_poly = Block.39.ex[Block.39.ex$SUM090 %in% panel.specify$SUM090, ]
        
        PNN3=poly2nb(sample_poly, snap=c(500),row.names=as.character(1:length(sample_poly)))    
        KNN1<-knn2nb(knearneigh(coordinates(sample_poly),k=1))    # distance
        spec_fn = as.formula( paste(spec_holder[spec_holder$county==paste(counties, collapse=" ") &  spec_holder$urban==urban1,"formulas"],sep="")   )
        j=1
        for(i in c(1,2,3,4,5,10,15)){
            PNN4 = PNN3
            if(i>1){DNNHO <- nblag(PNN4, maxlag=i)    # if k is greater than 1 get higher order neighbors 
                    PNN4 <- nblag_cumul(DNNHO)}
            for(x in 1:length(PNN4)){     # replace empty with k near neigh
                if( PNN4[[x]][1] == 0 ){PNN4[[x]] = KNN1[[x]]}
            }
            WDNN3 <- nb2listw(PNN4, style="W")
            
            out = tryCatch( {spml(formula=spec_fn,data=panel.specify,listw=WDNN3,model="pooling",effect="time") }, error = function(err) {print(paste("Avoid_ERROR:  ",err))
                                                                                                                                          return("errors") })            
            summary(out)
            dist_out$order[j] = as.numeric(i)
            if(out!="errors"){   # only if out didn't produce error
                dist_out$rho[j]   = summary(out)$ErrCompTable[1]
                dist_out$rho_p[j] = summary(out)$ErrCompTable[4]  
                           
                k=(length(out$coefficients)-1)
                n=dim(panel.specify)[1]                
                AIC3a = 2*k - 2*out$logLik                
                AICc3a = AIC3a + (2*k*(k+1))/(n-k-1)
                
                dist_out$sqrerr[j] = sum(out$residuals^2,na.rm=T)
                dist_out$aic[j] = AICc3a                
            }
            print(dist_out)
            j=j+1
        }
        
        write.csv(dist_out, paste("G:\\Faculty\\Mann\\Share\\Plots\\poly_out_",urban1,"_",leading_county_num,".csv",sep=""))        
    }
}

lapply( jepFIDS[1:length(jepFIDS) ], function(x) runit(unlist(x)))


 
        
################################################
#  union of polygon adjacency and and distance 
 
VERSION = "6.7TUrbNS+PD25kmb"
DIRECTORY = paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep="") 

CntyJep = read.csv(file=paste(DIRECTORY,"\\CntyJepAREACity",VERSION,".csv",sep=""))
jepFIDS = by(CntyJep[,"FIPS"],CntyJep[,"jepson_Unq_ID_edit"],unique)

runit <- function(counties){
    leading_county_num = counties[1]
    
    spec_holder = read.table(file=paste(DIRECTORY,"\\specificationsAIC_jepAREAcity",VERSION,".csv",sep=""),header=T, sep=",", stringsAsFactors=T)
    
    for (urban1 in  1:2){  
        dist_out = data.frame(order=c(rep(is.numeric(NA),15)), rho=c(rep(is.numeric(NA),15)), rho_p=c(rep(is.numeric(NA),15)), sqrerr=c(rep(is.numeric(NA),15)), aic=c(rep(is.numeric(NA),15))  )
        ### remove rows with more than 2 (expected) NAs
        Block.dbf = Block.dbf[apply(Block.dbf, 1, function(x) sum(is.na(x)))<=2,]
        Block.39.ex = Block.39.ex[Block.39.ex$SUM090 %in% unique(Block.dbf$SUM090),]
        
        get_data(urban=urban1,counties=counties,urban_rural_threshold=1)
        
        sample_poly = Block.39.ex[Block.39.ex$SUM090 %in% panel.specify$SUM090, ]
        PNN3=poly2nb(sample_poly, snap=c(500),row.names=as.character(1:length(sample_poly)) )     # change row.names to match polygon.id from attributes(DNN3)
        KNN1= knn2nb(knearneigh(coordinates(sample_poly),k=1))    # distance
        spec_fn = as.formula( paste(spec_holder[spec_holder$county==paste(counties, collapse=" ") &  spec_holder$urban==urban1,"formulas"],sep="")   )
        j=1
        #c(1000,10000,25000,50000,75000,80000,90000,100000,125000,150000)
        for(i in c(1000,5000,10000,15000,20000,25000,50000,100000)){
            remove(out)   # remove in case out = error 
            DNN3 = dnearneigh(coordinates(sample_poly),0,i,longlat=F)       
            for(x in 1:length(DNN3)){     # replace empty poly neigh with k near neigh
                if( DNN3[[x]][1] == 0 ){DNN3[[x]] = KNN1[[x]]}
            }
            DNN3 = union.nb(DNN3,PNN3)
            WDNN3 = nb2listw(DNN3, style="W")
            out = tryCatch( {spml(formula=spec_fn,data=panel.specify,listw=WDNN3,model="pooling",effect="time") }, error = function(err) {print(paste("Avoid_ERROR:  ",err))
                                                                                                                               return("errors") })
            dist_out$order[j] = as.numeric(i)
            if(out!="errors"){   # only if out didn't produce error
                dist_out$rho[j]   = summary(out)$ErrCompTable[1]
                dist_out$rho_p[j] = summary(out)$ErrCompTable[4]  
                
                k=(length(out$coefficients)-1)
                n=dim(panel.specify)[1]                
                AIC3a = 2*k - 2*out$logLik                
                AICc3a = AIC3a + (2*k*(k+1))/(n-k-1)
                
                dist_out$sqrerr[j] = sum(out$residuals^2,na.rm=T)
                dist_out$aic[j] = AICc3a
            }
            print(dist_out)
            j=j+1
            }
          print(paste(urban1, leading_county_num))
          write.csv(dist_out, paste("G:\\Faculty\\Mann\\Share\\Plots\\poly_dist_out_",urban1,"_",leading_county_num,".csv",sep=""))        
        }

}

lapply( jepFIDS[1:length(jepFIDS) ], function(x) runit(unlist(x)))
 

##################################################
### COMBINE DATA
# dist = list.files('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Dist\\','.csv')
# data_out =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Dist\\',dist[1],sep=''))             
# data_out = data.frame(id =rep(1,dim(data_out)[1]),data_out)
# for(i in 2:length(dist)){
#     data_in =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Dist\\',dist[i],sep=''))
#     data_in = data.frame(id =rep(i,dim(data_in)[1]),data_in)
#     data_out = rbind(data_out,data_in)
# }
# data_out$Type = 'Dist'
# data_out = na.omit(data_out[!data_out[,4:6]==0,])
# write.table(data_out, 'G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Dist\\all_dist.csv',sep=",") 
all_dist = read.csv('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Dist\\all_dist.csv',header=T)
head(all_dist)
###################
# kneigh = list.files('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\kneigh\\','.csv')
# data_out =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\kneigh\\',kneigh[1],sep=''))             
# data_out = data.frame(id =rep(1,dim(data_out)[1]),data_out)
# for(i in 2:length(dist)){
#     data_in =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\kneigh\\',kneigh[i],sep=''))
#     data_in = data.frame(id =rep(i,dim(data_in)[1]),data_in)
#     data_out = rbind(data_out,data_in)
# }
# data_out$Type = 'KNeigh'
# data_out = na.omit(data_out[!data_out[,4:6]==0,])
# write.table(data_out, 'G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\kneigh\\all_kneigh.csv',sep=",") 
all_kneigh = read.csv('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\kneigh\\all_kneigh.csv')
head(all_kneigh)

###################
# Poly = list.files('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Poly\\','.csv')
# data_out =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Poly\\',Poly[1],sep=''))             
# data_out = data.frame(id =rep(1,dim(data_out)[1]),data_out)
# for(i in 2:length(dist)){
#     data_in =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Poly\\',Poly[i],sep=''))
#     data_in = data.frame(id =rep(i,dim(data_in)[1]),data_in)
#     data_out = rbind(data_out,data_in)
# }
# data_out$Type = 'Poly'
# data_out = na.omit(data_out[!data_out[,4:6]==0,])
# write.table(data_out, 'G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Poly\\all_Poly.csv',sep=',') 
all_Poly = read.csv('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\Poly\\all_Poly.csv')
head(all_Poly)

###################
# PolyDist = list.files('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\PolyDist\\','.csv')
# data_out =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\PolyDist\\',PolyDist[1],sep=''))             
# data_out = data.frame(id =rep(1,dim(data_out)[1]),data_out)
# for(i in 2:length(dist)){
#     data_in =  read.csv(paste('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\PolyDist\\',PolyDist[i],sep=''))
#     data_in = data.frame(id =rep(i,dim(data_in)[1]),data_in)
#     data_out = rbind(data_out,data_in)
# }
# data_out$Type = 'PolyDist'
# data_out = na.omit(data_out[!data_out[,4:6]==0,])
# write.table(data_out, 'G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\PolyDist\\all_PolyDist.csv',sep=',') 

all_PolyDist = read.csv('G:\\Faculty\\Mann\\Share\\Plots\\DistanceBandTable2\\PolyDist\\all_PolyDist.csv')
head(all_PolyDist)

P_PolyDist = pdata.frame(all_PolyDist,index=c('id','X'))
P_Dist = pdata.frame(all_dist,index=c('id','X'))

P_Dist$Type = "Dist"
P_PolyDist$Type = "PolyDist"
P_Dist$SqrErr = Within(P_Dist$sqrerr)
P_PolyDist$SqrErr = Within(P_PolyDist$sqrerr)
P_PolyDist$AIC = Within(P_PolyDist$aic)
P_Dist$AIC = Within(P_Dist$aic)

Distances = rbind(P_PolyDist,P_Dist )
Distances$Distance = Distances$order
DistancesA = aggregate(. ~Distance+Type,data=Distances, function(x) median(x,na.rm=T))
a = ggplot(data=DistancesA, aes(x=Distance, y=SqrErr , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)
plot(a)
c=ggplot(data=DistancesA, aes(x=Distance, y=sqrerr , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5) + coord_cartesian(ylim = c(100, 300)) 
e = ggplot(data=DistancesA, aes(x=Distance, y=AIC , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)
g = ggplot(data=DistancesA, aes(x=Distance, y=aic , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)+ coord_cartesian(ylim = c(250, 750))

P_kneigh = pdata.frame(all_kneigh,index=c('id','X'))
P_Poly = pdata.frame(all_Poly,index=c('id','X'))

P_Poly$Type = "Poly"
P_kneigh$Type = "Kneigh"
P_Poly$SqrErr = Within(P_Poly$sqrerr)
P_kneigh$SqrErr = Within(P_kneigh$sqrerr)
P_kneigh$AIC = Within(P_kneigh$aic)
P_Poly$AIC = Within(P_Poly$aic)

Neigh = rbind(P_kneigh,P_Poly)
Neigh$Neighbor = Neigh$order
Neigh$Neighbor = factor(Neigh$order)
NeighA = aggregate(. ~Neighbor+Type,data=Neigh, function(x) median(x,na.rm=T))
ggplot(data=NeighA, aes(x=Neighbor, y=AIC , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)
b = ggplot(data=NeighA, aes(x=Neighbor, y=SqrErr , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)
ggplot(data=NeighA, aes(x=Neighbor, y=rho_p , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)
d=ggplot(data=NeighA, aes(x=Neighbor, y=sqrerr , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5) + coord_cartesian(ylim = c(100, 300)) 
f=ggplot(data=NeighA, aes(x=Neighbor, y=AIC , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)
h=ggplot(data=NeighA, aes(x=Neighbor, y=aic , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5)+ coord_cartesian(ylim = c(250, 750))

library(gridExtra)
windows()
grid.arrange(a, b, ncol=2) #demeaned sqrerror
windows()
grid.arrange(c, d, ncol=2) #absolute sqrerror    USED IN PAPER
windows()
grid.arrange(e, f, ncol=2) # demeaned AIC
windows()
grid.arrange(g, h, ncol=2) # absolute AIC


#look at signficance
i=ggplot(data=DistancesA, aes(x=Distance, y=rho_p , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5) 
j=ggplot(data=NeighA, aes(x=Neighbor, y=rho_p , group=Type,color=Type, shape=Type)) + geom_line(size=1,linetype=c("dashed")) + geom_point(size=3.5) 
grid.arrange(i, j, ncol=2) # median rho P

# FOR RHO TABLE 2 
library(plyr)
dtN <- data.frame(Neigh)
ddply(dtN[dtN$rho!=-0.999,],~order+Type,summarise,rangeL=range(rho)[1],median=median(rho),rangeH=range(rho)[2],rangePL=range(rho_p)[1],rangePH=range(rho_p)[2],medianP=median(rho_p))
dtD<- data.frame(Distances)
ddply(dtD[dtD$rho!=-0.999,],~order+Type,summarise,rangeL=range(rho)[1],median=median(rho),rangeH=range(rho)[2],rangePL=range(rho_p)[1],rangePH=range(rho_p)[2],medianP=median(rho_p))
 

##########################################################################################
##########################################################################################
##########################################################################################





##########################################################################################
library(lattice)
xyplot(HuDB ~ time | COUNTY, data=P_BAU)

coplot(HuDB ~ time|COUNTY, type="l", data=P_BAU, ylim=c(0,30), col="blue" , )

windows()
scatterplot(HuDB ~ time | COUNTY, data=P_BAU, reg.line=F,legend.plot=F, ylim=c(0,8),pch=NA, lwd=1.5, cex=.5)
points(HuDU[time>99] ~ time[time>99], data=P_Urban, pch=0, add=T, cex=.5  )
points(HuDR[time>99] ~ time[time>99], data=P_Rural, pch=1, add=T, cex=.5)

scatterplot(HuDB ~ time | COUNTY, data=P_BAU, reg.line=F,legend.plot=F, ylim=c(0,30),pch=NA, lwd=1.5, cex=.5)
for (group in P_Urban$id){
    lines(P_Urban$HuDU[P_Urban$time>99 & group==P_Urban$id] ~ P_Urban$time[P_Urban$time>99 & group==P_Urban$id], data=P_Urban, pch=0,  cex=.5, col="grey")  
}

rep(1:10,each=3)

library(ggplot2)
qplot(time,HuDU,facets= . ~ COUNTY,  geom = "line",data= P_Urban, ylim=c(0,30))


##########################################################
#  congestion

# rasterize traffic 
example = raster("G:/Faculty/Mann/Other/Fire/AllRoads.tif" )
proj = proj4string(raster("G:/Faculty/Mann/Other/Fire/AllRoads.tif" ))

traffic = readOGR("G:\\Faculty\\Mann\\Share\\CA Traffic","AADT_1m_proj",disambiguateFIDs=T,verbose=T,dropNULLGeometries=T)

VehMiles = rasterize(traffic,example,field='VehicMiles', fun=sum)
writeRaster(VehMiles,"G:/Faculty/Mann/Other/Fire/VehicMiles.tif")
AADT = rasterize(traffic,example,field='AADT', fun=sum)
writeRaster(AADT,"G:/Faculty/Mann/Other/Fire/AADT.tif")
total_miles = rasterize(traffic,example,field='Miles', fun=sum)
writeRaster(total_miles,"G:/Faculty/Mann/Other/Fire/total_miles.tif")
congestion = VehMiles/total_miles
writeRaster(congestion,"G:/Faculty/Mann/Other/Fire/congestionAADTperMile.tif")

congestion = raster("G:/Faculty/Mann/Other/Fire/congestionAADTperMile.tif")
proj4string(congestion)='+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0' 
VehMiles = raster("G:/Faculty/Mann/Other/Fire/VehicMiles.tif")
proj4string(VehMiles)='+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0' 
den_7600= raster(  "G:\\Faculty\\Mann\\Other\\Fire\\den_7600_proj.tif" )
den_0125 = raster(  "G:\\Faculty\\Mann\\Other\\Fire\\den_0125_proj.tif"  )
den_2650 = raster( "G:\\Faculty\\Mann\\Other\\Fire\\den_2650_proj.tif" )
denmax50x50_7600 = raster("G:\\Faculty\\Mann\\Other\\Fire\\denmax50x50_7600_proj.tif")
denmax50x50_2650 = raster("G:\\Faculty\\Mann\\Other\\Fire\\denmax50x50_2650_proj.tif")
 
library(rasterVis)
cong_stack = stack(VehMiles,congestion,den_7600,den_0125,den_2650,denmax50x50_7600,denmax50x50_2650)
 
qplot( (getValues(den_2650-den_7600)/getValues( den_7600)), getValues(VehMiles),alpha=.6 )
 
windows()
qplot(getValues(den_7600), getValues(VehMiles),alpha=.9  )
windows()
qplot(getValues(den_2650-den_7600), getValues(VehMiles),alpha=.9 )
windows()
prc_chng = (den_2650-den_7600) /den_7600
prc_chng[prc_chng>500]=500
library(colorspace)
levelplot(prc_chng*VehMiles ,pretty=T  ,  par.settings= BTCTheme())
windows()
qplot(getValues(prc_chng), getValues(VehMiles),alpha=.9 )

prc_chng2 = (denmax50x50_2650-denmax50x50_7600) /denmax50x50_7600
prc_chng2[prc_chng2>500]=500
levelplot(prc_chng2*VehMiles ,pretty=T  ,  par.settings= BTCTheme())
qplot(getValues(prc_chng2), getValues(VehMiles),alpha=.9 )


##########################################################################################
##########################################################################################
###################   Housing starts ####################################
library(zoo)
starts = read.csv('G:\\Faculty\\Mann\\Share\\Census Data\\Housing Starts\\Census_housing_starts.csv')
starts$Date =    strptime(as.character(starts$Month), "%m/%d/%Y")
starts$Year = starts$Date$year
starts$Housing.Starts = starts$WestHousingStarts*1000 # adjust for fact in thousands 
starts$Accumulated.Housing.Starts = cumsum(starts$Housing.Starts)

ggplot(data=starts, aes(x=Date, y=Housing.Starts,group=1  )) + 
    geom_line(  linetype="solid", size=1) +  ylab("Monthly Housing Starts Western States")

ggplot(data=starts, aes(x=Date, y=Accumulated.Housing.Starts,group=1  )) + 
    geom_line(  linetype="solid", size=1)+  ylab("Accumulated Monthly Housing Starts Western States")


starts$CensusDecade =  cut(as.numeric(starts$Year),
                        breaks=c(59,69,79,89,99,109,Inf),
                        labels=c('60-69','70-79','80-89','90-99','00-09','10-19') )

starts_A_decade = aggregate(Housing.Starts~ CensusDecade,starts,sum)
starts_A_decade[ (dim(starts_A_decade)[1]),'Housing.Starts']=NA  # remove forecast not appropriate 
starts_A_decade$Class = 'Observed'
ggplot(data=starts_A_decade, aes(x=CensusDecade, y=Housing.Starts,group=1 )) + 
    geom_line(colour="red", linetype="dotted", size=1.5)  


############################################################################
# assume housing starts remain constant at final month of data and aggregate
# days till next census /30  for months
(strptime( '1/1/2019', "%m/%d/%Y")-strptime('7/1/2012', "%m/%d/%Y"))  # 79 months
startsC = starts
startsC[(dim(startsC)[1]+1):(dim(startsC)[1]+79),]  =  startsC[dim(startsC)[1],] 
startsC$CensusDecade =  cut(as.numeric(startsC$Year),
                           breaks=c(59,69,79,89,99,109,Inf),
                           labels=c('60-69','70-79','80-89','90-99','00-09','10-19') )

startsC_A_decade = aggregate(Housing.Starts~ CensusDecade,startsC,sum)
startsC_A_decade$Class= 'Constant'
ggplot(data=startsC_A_decade, aes(x=CensusDecade, y=Housing.Starts,group=1 )) + 
    geom_line(colour="black", linetype="solid", size=1.25)  


############################################################################
# assume housing starts increase at current annual rate  
# days till next census /30  for months
startsL = starts
sampledata = startsL[startsL$Date>= strptime('7/1/2011', "%m/%d/%Y"),]
sampledata$period = 1:dim(sampledata)[1]
reg = lm(Housing.Starts~period,data=sampledata )
newdata_in=data.frame(period = dim(sampledata)[1]:(dim(sampledata)[1]+78)) # 79 more periods until census
pred= predict.lm(reg,newdata=newdata_in)
startsL[(dim(startsL)[1]+1):(dim(startsL)[1]+79),]  =  startsL[dim(startsL)[1],] # place hold
startsL[644:dim(startsL)[1],'Housing.Starts'] = pred                             # replace with preidiction

startsL$CensusDecade =  cut(as.numeric(startsL$Year),
                            breaks=c(59,69,79,89,99,109,Inf),
                            labels=c('60-69','70-79','80-89','90-99','00-09','10-19') )

startsL_A_decade = aggregate(Housing.Starts~ CensusDecade,startsL,sum)
startsL_A_decade$Class= '1Y Linear'

ggplot(data=startsL_A_decade, aes(x=CensusDecade, y=Housing.Starts,group=1 )) + 
    geom_line(colour="black", linetype="solid", size=1.25)  

############################################################################
# assume pre 2000 quantile levels of starts 
# days till next census /30  for months
startsQ = starts
startsQ0 = starts
startsQ25 = starts
startsQ50 = starts
startsQ75 = starts
startsQ100 = starts

sampledata = startsQ[startsQ$Date<= strptime('1/1/2000', "%m/%d/%Y"),]
quan= quantile(sampledata$Housing.Starts)

startsQ0[(dim(startsQ0)[1]+1):(dim(startsQ0)[1]+79),]  =  startsQ0[dim(startsQ0)[1],] # place hold
startsQ0[644:dim(startsQ0)[1],'Housing.Starts'] = as.numeric(quan[1])                            # replace with preidiction
startsQ25[(dim(startsQ25)[1]+1):(dim(startsQ25)[1]+79),]  =  startsQ25[dim(startsQ25)[1],] # place hold
startsQ25[644:dim(startsQ25)[1],'Housing.Starts'] = as.numeric( quan[2])                            # replace with preidiction
startsQ50[(dim(startsQ50)[1]+1):(dim(startsQ50)[1]+79),]  =  startsQ50[dim(startsQ50)[1],] # place hold
startsQ50[644:dim(startsQ50)[1],'Housing.Starts'] = as.numeric( quan[3])                            # replace with preidiction
startsQ75[(dim(startsQ75)[1]+1):(dim(startsQ75)[1]+79),]  =  startsQ75[dim(startsQ75)[1],] # place hold
startsQ75[644:dim(startsQ75)[1],'Housing.Starts'] = as.numeric( quan[4])                            # replace with preidiction
startsQ100[(dim(startsQ100)[1]+1):(dim(startsQ100)[1]+79),]  =  startsQ100[dim(startsQ100)[1],] # place hold
startsQ100[644:dim(startsQ100)[1],'Housing.Starts'] = as.numeric( quan[5])                            # replace with preidiction

startsQ0$CensusDecade =  cut(as.numeric(startsQ0$Year),
                            breaks=c(59,69,79,89,99,109,Inf),
                            labels=c('60-69','70-79','80-89','90-99','00-09','10-19') )   
startsQ25$CensusDecade= startsQ0$CensusDecade   # add census decade breaks 
startsQ50$CensusDecade= startsQ0$CensusDecade
startsQ75$CensusDecade= startsQ0$CensusDecade
startsQ100$CensusDecade= startsQ0$CensusDecade


startsQ0_A_decade = aggregate(Housing.Starts~ CensusDecade,startsQ0,sum)
startsQ0_A_decade$Class= '0% Quantile'

startsQ25_A_decade = aggregate(Housing.Starts~ CensusDecade,startsQ25,sum)
startsQ25_A_decade$Class= '25% Quantile'

startsQ50_A_decade = aggregate(Housing.Starts~ CensusDecade,startsQ50,sum)
startsQ50_A_decade$Class= '50% Quantile'

startsQ75_A_decade = aggregate(Housing.Starts~ CensusDecade,startsQ75,sum)
startsQ75_A_decade$Class= '75% Quantile'

startsQ100_A_decade = aggregate(Housing.Starts~ CensusDecade,startsQ100,sum)
startsQ100_A_decade$Class= '100% Quantile'

all_quantile = rbind(startsQ0_A_decade,startsQ25_A_decade,startsQ50_A_decade,startsQ75_A_decade,startsQ100_A_decade)

all_quantile$Class = factor(all_quantile$Class, levels=rev(c( "0% Quantile","25% Quantile","50% Quantile","75% Quantile","100% Quantile")))
ggplot(data=all_quantile, aes(x=CensusDecade, y=Housing.Starts,group=Class,colour=Class )) + 
    geom_line(  linetype="dotted", size=1.5)  + scale_colour_grey(start = .2, end = .7)+ geom_line(data = starts_A_decade,linetype="solid", size=1.5,colour="Black" )


#############   join all data 
all_data = rbind(starts_A_decade,startsC_A_decade,startsL_A_decade)
ggplot(data=all_data, aes(x=CensusDecade, y=Housing.Starts,group=Class,colour=Class )) + 
    geom_line(  linetype="dotted", size=1.5)  + scale_colour_grey(start = 0, end = 1)+ geom_line(data = starts_A_decade,linetype="solid", size=1.5,colour="Grey" )


#############   join only linear  data 
all_data = rbind(starts_A_decade, startsL_A_decade)
ggplot(data=all_data, aes(x=CensusDecade, y=Housing.Starts,group=Class,colour=Class )) + 
    geom_line(  linetype="dotted", size=1.5)  + scale_colour_grey(start = 0, end = 1)+ geom_line(data = starts_A_decade,linetype="solid", size=1.5,colour="Grey" )








############################################################################
############################################################################
############################################################################
############################################################################
############################################################################

get_data <- function(urban,counties,urban_rural_threshold){
  #Load relevant data
  
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
  KNN4<<-knn2nb(knearneigh(coordinates(Block.39_sp),k=3))   #12
  WKNN4 <<- nb2listw(KNN4, style="W")
  
  
  ################################################
  ######   PUT DATA INTO PANEL LONG        ######
  panel.data= reshape(Block.39.dbf_sp, dir = "long", varying = c(grep("HuDac",names(Block.39.dbf_sp)),grep("Cum",names(Block.39.dbf_sp)),grep("DD100_",names(Block.39.dbf_sp)), grep("DD6585_",names(Block.39.dbf_sp)), grep("DD95_",names(Block.39.dbf_sp))  ) , sep = "_")
  panel.dataXX= reshape(Block.39.dbf_spC, dir = "long", varying = c( grep("HuDac",names(Block.39.dbf_spC)),grep("Cum",names(Block.39.dbf_spC)),grep("DD100_",names(Block.39.dbf_spC)), grep("DD6585_",names(Block.39.dbf_spC)), grep("DD95_",names(Block.39.dbf_spC))  ) , sep = "_")
  
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
}





### CREATE NEW VARIABLES
add_panel_variables  = function(data){
  data$HuDaclag    = lag(data$HuDac4, 1) 
  data$LnHuDaclag  = log(data$HuDaclag+1) 
  data$HuDaclagSqr = data$HuDaclag^2 
  data$HuDaclagQb  = data$HuDaclag^3 
  data$HuDaclagFr  = data$HuDaclag^4 
  data$HuDaclagFv  = data$HuDaclag^5
  
  if(length(counties)==1){data$COUNTY=counties}                            # if only one county COUNTy would be removed and this interferes later
  return(data)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, by.row=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
