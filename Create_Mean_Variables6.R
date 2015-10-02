library(rgdal)
library(sp)
library(raster)
library(maptools)

options(warn=1)  # print warnings as they occur
#options(warn=2)  # treat warnings as errors
        
rm(list = ls())

#########################  Process simple data  ##########################################
## get data of interest, calculate mean / sum / etc for relevant variables
## mask all unihabitable areas summarize by SUM090 from HCounts_39 polygon 

########################################################################
############   MEDIAN REPORTED VARIABLES   ###################

IN = c("C:\\Users\\mmann1123\\Desktop\\Share\\Boundary_Files\\CA_County_Rstr.tif",
  "C:\\Users\\mmann1123\\Desktop\\Share\\Environemental Factors\\Jepson_raster.tif")
  ## shouldn't need this... only block numbs not split "C:\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\Tiger_Files_00_ESRI\\Census_raw_STFID.tif")
NAMES = c("County","Jepson")

for (i in 1:length(IN)){
  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  data = getValues(raster(IN[i]))
  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  nas2 = is.na(CBG) | is.na(data) | mask==1  

  data[nas2] = NA
  CBG[nas2]  = NA
  data       = na.omit(data)
  CBG        = na.omit(CBG)
    if( dim(matrix(data))[1] != dim(matrix(CBG))[1] ){
      print(paste("dimensions not same for raster",NAMES[i],sep=" "))
      break  
    }
  #### data #####
  data1= as.data.frame(cbind(CBG, data))
  mn1=aggregate(data ~ CBG, data = data1, FUN="median", na.rm=TRUE )
  out = data.frame(mn1)
  names(out) = c("SUM090", NAMES[i] )
  #out[,2] = round(out[,2])
  write.table(out, file = paste("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\",NAMES[i],"_CBG5.csv",sep=""), sep=",", row.names=FALSE)
  remove("data1", "out", "mn1","CBG","nas2")
}


########################################################################
#######################   MEAN  REPORTED VARIABLES   ###################  
rm(list = ls())

IN = c("G:\\Faculty\\Mann\\Share\\PopulatedPlaces_BTS\\1950 Census Populated Places\\Path_Dist5_PPAll_1940.tif")
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_Incorp_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_AllRoadsNHPNv6_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_Prim_Sec_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_OceanBay_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_PP30k_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_PP20k_10_3.tif", 
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_PPAll_10_3.tif", 
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_Water_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_NPark_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_Irrigation_10_3.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\elevation srtm\\STRMGeoTiff\\Slope_srtm_All_100m_clip.tif",  
       #"C:\\Users\\mmann1123\\Desktop\\Share\\elevation srtm\\STRMGeoTiff\\Roughness_srtm_All_100m_clip.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_ExludeTT.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Path_Dist6_Indian_10_3.tif",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Dist_Path\\dist_incorp1.tif",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Dist_Path\\dist_indian1.tif",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Dist_Path\\dist_nparks1.tif",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Dist_Path\\dist_pp20k1.tif",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Dist_Path\\dist_pp30k1.tif",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Dist_Path\\dist_ppall1.tif"
        # Dont use these, use decedal temp data instead "C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Av.Num65_85DD_extnd_100m.tif",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Av.Num95DD_extnd_100m.tif"
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Roads\\Cost_Path_v6\\Av.Num100DD_extnd_100m.tif"   )

NAMES = c(#"Incorp","AllRoads","PrimSecRd","OceanTT","PP30kTT","PP20kTT","PPAllTT",
          #"WaterTT","NParkTT","IrrgTT","Slope_mn","Rough_mn", "ExcludeTT", "IndianTT",
         # "dist_incorp","dist_indian","dist_nparks","dist_pp20k","dist_pp30k","dist_ppall"
          "ppall_1940")

for (i in 1:length(IN)){
  CBG = getValues(raster("G:\\Faculty\\Mann\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  data = getValues(raster(IN[i]))
  mask = getValues(raster("G:\\Faculty\\Mann\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  if(length(CBG)!= length(data)  ){break}
  nas2 = is.na(CBG) | is.na(data ) | mask==1            #create uniform mask

  data[nas2] = NA
  CBG[nas2] = NA
  data= na.omit(data)                                   #reduce file size
  CBG= na.omit(CBG)
    if( dim(matrix(data))[1] != dim(matrix(CBG))[1] ){
      print(paste("dimensions not same for raster",NAMES[i],sep=" "))
      break  
    }
  #### data #####
  data1= as.data.frame(cbind(CBG, data))
  mn1=aggregate( data ~ CBG, data = data1, FUN="mean", na.rm=TRUE )   #aggregate data
  out = data.frame(mn1)
  names(out) = c("SUM090", NAMES[i] )
  write.table(out, file = paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\",NAMES[i],"_CBG5.csv",sep=""), sep=",", row.names=FALSE)
  remove("data1", "out", "mn1","CBG","nas2")
}



##############################################################################################
#######################   MEAN  REPORTED VARIABLES  SPECIAL SLOPE LOGICALS ###################
############   % of total area not above slope grade of interest  ############################
rm(list = ls())

IN = c("C:\\Users\\mmann1123\\Desktop\\Share\\elevation srtm\\STRMGeoTiff\\Slope_srtm_All_100m_clip.tif" )
NAMES = c("Slope_")

for (i in c(10,15)){
  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  data = getValues(raster(IN[1]))
  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  nas2 = is.na(CBG) | is.na(data ) | mask==1            #create uniform mask

  data[nas2] = NA
  CBG[nas2]  = NA
  data= na.omit(data)                                   #reduce file size
  CBG = na.omit(CBG)
    if( dim(matrix(data))[1] != dim(matrix(CBG))[1] ){
      print(paste("dimensions not same for raster slope",i,sep=" "))
      break  
    }
  #### data #####
  data1= as.data.frame(cbind( CBG, data>=i, rep(1,length(data)) ))
  names(data1) = c("CBG", "slope", "pixel")
  remove("CBG", "data") 
  mn  = aggregate( slope ~ CBG, data = data1, FUN="sum", na.rm=TRUE )   # #pixels over % grade
  mn.p= aggregate(pixel ~ CBG, data = data1,  FUN="sum", na.rm=TRUE)    # total #pixels  
  pr_slope = 1-(mn$slope / mn.p$pixel)                                  # % not greater than grade
  out = data.frame(cbind( mn$CBG, pr_slope ))
  names(out) = c("SUM090", paste("SlopePr",i,sep="") )
  write.table(out, file = paste("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\SlopePR_",i,"_CBG5.csv",sep=""), sep=",", row.names=FALSE)
  remove("data1", "out", "mn","mn.p","nas2")
}


######  calc the percentage area of a block covered by indian lands

IN = c("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\PublicLand_FRAP\\Indian Lands\\Reclass_IndianRast.tif" )
NAMES = c("IndPrc")

  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  data = getValues(raster(IN[1]))
  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  nas2 = is.na(CBG) | is.na(data ) | mask==1            #create uniform mask

  data[nas2] = NA
  CBG[nas2]  = NA
  data= na.omit(data)                                   #reduce file size
  CBG = na.omit(CBG)
    if( dim(matrix(data))[1] != dim(matrix(CBG))[1] ){
      print(paste("dimensions not same for raster slope",i,sep=" "))
      break  
    }
  #### data #####
  data1= as.data.frame(cbind( CBG, data, rep(1,length(data)) ))
  names(data1) = c("CBG", "IndPrc", "pixel")
  data1$IndPrc[data1$IndPrc==1]=0
  data1$IndPrc[data1$IndPrc==2]=1
  remove("CBG", "data") 
  mn  = aggregate( IndPrc ~ CBG, data = data1, FUN="sum", na.rm=TRUE )   # #pixels over % grade
  mn.p= aggregate(pixel ~ CBG, data = data1,  FUN="sum", na.rm=TRUE)    # total #pixels  
  IndPrc = (mn$IndPrc / mn.p$pixel)                                  # % not greater than grade
  out = data.frame(cbind( mn$CBG, IndPrc ))
  names(out) = c("SUM090", "IndPrc" )
  write.table(out, file = "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\IndPrc_CBG5.csv", sep=",", row.names=FALSE)
  remove("data1", "out", "mn","mn.p","nas2")




########################################################################
####################   IMPORT ALL DECADAL TEMP DATA   ################## 

  setwd("C:\\Users\\mmann1123\\Desktop\\Share\\Environemental Factors\\Met Data\\NLDAS_Met\\decades\\")
  dr = dir()
  dr=dr[grepl("extnd_smp.tif", dir()) & !grepl(".ovr", dir()) & !grepl(".xml", dir())  ]  #get only relevant tiffs

  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  rasters = getValues(raster(paste(dr[1])))                   #prime the pumpS!

  nas = is.na(CBG) | mask==1 | is.na(rasters)                 #creat uniform mask
  CBG[nas] = NA
  CBG = na.omit(CBG)
  remove("mask", "rasters")

  for (i in 1:length(dr)){
      setwd("C:\\Users\\mmann1123\\Desktop\\Share\\Environemental Factors\\Met Data\\NLDAS_Met\\decades\\")
      rasters = getValues(raster(paste(dr[i])))
      rasters[nas] = NA
  	  rasters = na.omit(rasters)
		
      if( dim(matrix(rasters))[1] != dim(matrix(CBG))[1] ){
        print(paste("dimensions not same for raster",strsplit(dr[i], split="\\.")[[1]][2],sep=" "))
        break  
        }   
    
  	  #### all temp scenarios #####
  		data1= as.data.frame(cbind(CBG, rasters))
  		remove("rasters")
  		mn7=aggregate(rasters~ CBG, data = data1, FUN="mean", na.rm=TRUE)
  		out = data.frame(mn7)
  		names(out) = c("SUM090", strsplit(dr[i], split="\\.")[[1]][2] )

  		write.table(out, file = paste("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\",strsplit(dr[i], split="\\.")[[1]][2],"_CBG5.csv", sep=""), sep=",", row.names=FALSE)
  		remove( "out", "mn7", "data1")	
	 }


########################################################################
#########################  Calculate TOTAL & Inhabitable Area ##########
rm(list = ls())

  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  nas2 = is.na(CBG) | mask==1                          #create uniform mask

  CBG_ID = na.omit(CBG)                                 # use to aggregate total area
  CBG[nas2] = NA                                        # use to aggregate as exluded data
  CBG= na.omit(CBG)                                     #reduce file size    

  #### Inhabitable Area #####                           #uses mask to exclude
  data1= as.data.frame(cbind(CBG, as.integer(!is.na(CBG)) ))
  names(data1) = c("CBG","Pixel")
  mn1=aggregate( Pixel ~ CBG, data = data1, FUN="sum", na.rm=TRUE )   #aggregate data
  out1 = data.frame(mn1)
  names(out1) = c("SUM090", "InhbPxl" )
  write.table(out1, file = "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\InhbPxl_CBG5.csv", sep=",", row.names=FALSE)
  remove("data1", "out1", "mn1","CBG","nas2")
  
  #### Total Area #####
  data1= as.data.frame(cbind(CBG_ID, as.integer(!is.na(CBG_ID)) ))
  names(data1) = c("CBG_ID","Pixel")
  remove("CBG_ID")
  out=data.frame(aggregate( Pixel ~ CBG_ID, data = data1, FUN="sum", na.rm=TRUE ) )  #aggregate data
  names(out) = c("SUM090", "TotPxl" )
  write.table(out, file = "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\TotPxl_CBG5.csv", sep=",", row.names=FALSE)
 
########################################################################
#########################  Calculate Contain Indian lands ##########
rm(list = ls())

  indian = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\PublicLand_FRAP\\Indian Lands\\Reclass_IndianRast.tif"))  #real unique id still need to use lookup table
  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090.tif"))  #real unique id still need to use lookup table
  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  nas2 = is.na(CBG) | mask==1 | is.na(indian)                           #create uniform mask

   CBG[nas2] = NA                                          
  indian[nas2] = NA                                          

  CBG = na.omit(CBG)                                     #reduce file size    
  indian = na.omit(indian)                               #reduce file size    
  data1= as.data.frame(cbind( CBG, indian ))
  data1$indian[data1$indian==1] = 0 
  data1$indian[data1$indian==2] = 1 
  remove(CBG, mask,nas2)  

  ind=aggregate(indian ~ CBG, data = data1, FUN="max", na.rm=TRUE)
  names(ind) = c("SUM090", "IndianDm" )
  write.table(ind, file = "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\IndianDm_CBG5.csv", sep=",", row.names=FALSE)




####################################################################################
################## LINK ALL DATA     to   Block.39      using SUM090       ##################
rm(list = ls())

Block.39 =  read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_splt_090_clipCopy_ORIGINAL.dbf")  #duplicate of original 
Block.39$Sorter = 1:dim(Block.39)[1]
length(Block.39$SUM090) ==  length(unique(Block.39$SUM090))        #check that all ids are unique
 
link.table = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090_LinkTable.dbf")       # get SUM090 link table
link.table = subset(link.table, select=c(Value, SUM090))
names(link.table) = c("SUM090_link","SUM090")
Block.39   = merge(Block.39, link.table, by="SUM090", all.x=TRUE, incomparables=NA)

setwd("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5")
  #get all file names for means
	a=dir()
	b=a[grep("_CBG5.csv", dir())]

for (i in 1:length(b)) {
  readmn = read.table( b[i], header=TRUE, sep=",")
	Block.39 = merge(Block.39, readmn ,  by.x ="SUM090_link", by.y="SUM090", all.x=TRUE, incomparables=NA)
	}
 
  #a= Block.39[, !(names(Block.39) %in% c("Sorter","InhbPxl","SUM090","LOGRECNO.y","STATE_y","PrcArea","TotPxl") ) ]  
  #a[a$InhbPxl.y == 0 | is.na(a$InhbPxl.y),] = NA         # replace all 0 size and NA with 
  #Block.39[, !(names(Block.39) %in% c("Sorter","InhbPxl.x","SUM090","LOGRECNO","PrcArea","TotPxl") ) ] = a
  #Block.39$CUMLAW_109 = NA                               # remove any zeroes from missing data
  #Block.39$HuDac_109 = NA

 # Scale County level housing counts to match census county counts
 # get actual housing counts by county 
  county_count = read.csv("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\Historical County Counts\\HistoricalHouseCountByCounty.csv", header=T)
  HU_C = data.frame(Block.39[ ,grep("Cum", names(Block.39))],COUNTY_y=Block.39[,c("COUNTY_y")])               # get all hu counts
  county_lookup = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Boundary_Files\\geodatabase\\County.dbf")  
  county_lookup = unique(subset(county_lookup, select=c("FMNAME_PC", "FIPS")))
  county_lookup$FMNAME_PC = toupper(county_lookup$FMNAME_PC)  
  county_count$CountyName = toupper(county_count$CountyName)  

  county_count = merge(county_count,county_lookup, by.x="CountyName", by.y="FMNAME_PC")  # gets fips code for county_count
  block_count  = aggregate(. ~ COUNTY_y, data=HU_C, FUN="sum")  # aggregate block data
  block_county = merge(county_count, block_count, by.x="FIPS", by.y="COUNTY_y")
 # compare estimated counts to actual
  for (i in seq(39,99,by=10)){
    year = grep(as.character(i),names(block_county) )
    #block_county[,paste("diff",i,sep="_")] = block_county[,year[2]]/block_county[,year[1]]
    assign(paste("diff",i,sep="_"), cbind(as.numeric(block_county[,year[2]]/block_county[,year[1]]),paste("Y_",i,sep="")) )
    # this assign is used to hepl with ggplot histograms
    }
  library(ggplot2)
  all_diff = as.data.frame( rbind(diff_39,diff_49,diff_59,diff_69,diff_79,diff_89,diff_99) ,stringsAsFactors=F )
  names(all_diff) = c("data","Year")
  all_diff$data = as.numeric(all_diff$data)
  all_diff$Year = as.factor(all_diff$Year)
  ggplot(all_diff, aes(data, fill = Year)) + geom_density(alpha=.5) +scale_fill_grey(start=0,end=1) + xlab("Percentage of Actual by County") + coord_cartesian(ylim = c(0, 17)) 
  by(all_diff$data,INDICES= all_diff$Year,FUN=summary )

 ### Correct for count bias
    # divide block counts by total estimated counts for each county
    names(block_count) = c("COUNTY_y","Cum39_Cy","Cum49_Cy","Cum59_Cy","Cum69_Cy","Cum79_Cy","Cum89_Cy","Cum99_Cy")
  Block.39 =  merge(Block.39,block_count,by="COUNTY_y")
  for (i in seq(39,99,by=10)){
    # calc block as percentage of block total
    year = grep(paste("Cum",i,sep=""),names(Block.39) )
    Block.39[,paste("CumPer",i,sep="")] = Block.39[,year[1]]/Block.39[,year[2]]  # percentage of total represented by each block
  }
  Block.39 = subset(Block.39, select=-c(Cum39_Cy,Cum49_Cy,Cum59_Cy,Cum69_Cy,Cum79_Cy,Cum89_Cy,Cum99_Cy)) # get rid of county estimated counts
  
 # multiply block-level percentage by actual county housing couts
  # CumPer = percentage of estimated housing count (estimates distribution spatially)
  # County_Count = Acuatly county level totals
  # CumAdj= New count estimates calibrated to match county level counts
  Block.39 = merge( Block.39,county_count, by.x = "COUNTY_y", by.y = "FIPS", incomparables=NA)
  
  for (i in seq(39,99,by=10)){  # multiply real housing county by percentage share
    year = c(grep(paste("CumPer",i,sep=""),names(Block.39) ),grep(paste("HCC",i,sep="_"),names(Block.39) ))
    Block.39[,paste("CumAdj",i,sep="_")] = Block.39[,year[1]]*Block.39[,year[2]]  # percentage of total represented by each block
  }
  Block.39 = subset(Block.39, select=-c(CumPer39,CumPer49,CumPer59,CumPer69,CumPer79,CumPer89,CumPer99)) # get rid of county estimated counts

 
  # Create density values   based on inhabitable area     CONVERT TO hu/acre    
  # 100m*100m=10,000m^2 = 2.4710538 acres
  HU = Block.39[ ,grep("CumAdj", names(Block.39))]               # get all hu counts
  HU_Den = data.frame(HU / (Block.39$InhbPxl*2.4710538))      # convert to acres then get density
  names(HU_Den) = c("HuDac4_39", "HuDac4_49", "HuDac4_59", "HuDac4_69", "HuDac4_79", "HuDac4_89", "HuDac4_99")
  HU_Den[is.na(HU_Den)] = 0                                   # replace missing values with 0
  Est_Acr = Block.39$InhbPxl*2.4710538                        # estimate pixel based area (ac) to compare with polygon area
  Block.39 = cbind(Block.39, Est_Acr, HU_Den)

  Block.39 = Block.39[with(Block.39, order(Sorter) ), ]     # must sort back into original order!!!!!!  
  write.dbf(Block.39, "C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_splt_090_clipCopy6.dbf")
  

####################################################################################
############    merge all data from final block data to excluded polygons ###########
  Block.39.excld = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion5.dbf") #was BG_splt_090_Exclusion5_ORIGINAL.dbf
  Block.39.excld = subset(Block.39.excld,select=c(SUM090))
  Block.39.copy  = read.dbf("C:\\Users\\mmann1123\\Desktop\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_splt_090_clipCopy6.dbf")  #duplicate of original hcount_39
  Block.39.excld$sort = 1:length(Block.39.excld$SUM090) ##   make sure to resort Block.39.excld by ID before export!!!!
  length(Block.39.excld$SUM090) == length(unique(Block.39.excld$SUM090)) 

  Block.39.copy$CumAdj_109 = NA                    # add to keep number time 39-109 constant between variables
  Block.39.copy$HuDac4_109 = NA
  Block.39.excld = merge(Block.39.excld,Block.39.copy,all.x=T, all.y=F, by="SUM090")          # merge only relevant ids


  names(Block.39.excld)
    # NOTE MUST KEEP OBJECTID_1 FOR EXPORT 
  Block.39.excld = subset(Block.39.excld, select=c(SUM090,COUNTY_y,OBJECTID,sort,STATE_y,County,CountyName,TRACT,BLKGRP,POP100,Tot00,
                                                   INTPTLAT, INTPTLON,Est_Acr,TotPxl,InhbPxl,Jepson,
                                                   dist_incor,dist_pp30k,dist_ppall,dist_india,dist_npark,dist_pp20k,
                                                   ExcludeTT,Incorp,IndianTT,IndPrc,IndianDm,OceanTT,WaterTT,IrrgTT,NParkTT,
                                                   PP20kTT,PP30kTT,PPAllTT,PrimSecRd,AllRoads,
                                                   DD100_39,DD100_49,DD100_59,DD100_69,DD100_79,DD100_89,DD100_99,DD100_109,
                                                   DD6585_39,DD6585_49,DD6585_59,DD6585_69,DD6585_79,DD6585_89,DD6585_99,DD6585_109,
                                                   DD95_39,DD95_49,DD95_59,DD95_69,DD95_79,DD95_89,DD95_99,DD95_109,
                                                   Rough_mn,Slope_mn,SlopePr10,SlopePr15,
                                                   CumAdj_39,CumAdj_49,CumAdj_59,CumAdj_69,CumAdj_79,CumAdj_89,CumAdj_99,CumAdj_109,
                                                   HuDac4_39,HuDac4_49,HuDac4_59,HuDac4_69,HuDac4_79,HuDac4_89,HuDac4_99,HuDac4_109))   # get rid of duplicates


  names(Block.39.excld) = c("SUM090","COUNTY","OBJECTID","sort","STATE","County","CountyName","TRACT","BLKGRP","POP00","Tot00",
                            "INTPTLAT", "INTPTLON","Est_Acr","TotPxl","InhbPxl","Jepson",
                            "dist_incor","dist_pp30k","dist_ppall","dist_india","dist_npark","dist_pp20k",
                            "ExcludeTT","Incorp","IndianTT","IndPrc","IndianDm","OceanTT","WaterTT","IrrgTT","NParkTT",
                            "PP20kTT","PP30kTT","PPAllTT","PrimSecRd","AllRoads",
                            "DD100_39","DD100_49","DD100_59","DD100_69","DD100_79","DD100_89","DD100_99","DD100_109",
                            "DD6585_39","DD6585_49","DD6585_59","DD6585_69","DD6585_79","DD6585_89","DD6585_99","DD6585_109",
                            "DD95_39","DD95_49","DD95_59","DD95_69","DD95_79","DD95_89","DD95_99","DD95_109",
                            "Rough_mn","Slope_mn","SlopePr10","SlopePr15",
                            "CumAdj_39","CumAdj_49","CumAdj_59","CumAdj_69","CumAdj_79","CumAdj_89","CumAdj_99","CumAdj_109",
                            "HuDac4_39","HuDac4_49","HuDac4_59","HuDac4_69","HuDac4_79","HuDac4_89","HuDac4_99","HuDac4_109")   # get rid of duplicates
 
  Block.39.excld = Block.39.excld[with(Block.39.excld,order(sort)),]         # sort by sort
  write.dbf(Block.39.excld, "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")


####################################################################################
#### add new variables indianTT and excludeTT, indian %, distance based measures 
rm(list = ls())

  Block.39.excld = read.dbf("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6.dbf")
  Block.39.excld$sort = 1:length(Block.39.excld$SUM090)
  b = c("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\ppall_1940_CBG5.csv" )
    #"C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\ExcludeTT_CBG5.csv",
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\IndianTT_CBG5.csv"
       # "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\IndianDm_CBG5.csv"
       #"C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\IndPrc_CBG5.csv"
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\dist_incorp_CBG5.csv",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\dist_indian_CBG5.csv",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\dist_nparks_CBG5.csv",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\dist_pp20k_CBG5.csv",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\dist_pp30k_CBG5.csv",
#        "C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\dist_ppall_CBG5.csv"  )

  link.table = read.dbf("G:\\Faculty\\Mann\\Share\\Census Data\\BuildDate_Raw\\BlogGrp_split_U_R_090\\BG_SUM090_LinkTable.dbf")       # get SUM090 link table
  link.table = subset(link.table, select=c(Value, SUM090))
  names(link.table) = c("SUM090_link","SUM090")
  Block.39.excld   = merge(Block.39.excld, link.table, by="SUM090", all.x=TRUE, incomparables=NA)

for (i in 1:length(b)) {
  readmn = read.table( b[i], header=TRUE, sep=",")
  Block.39.excld = merge(Block.39.excld, readmn ,  by.x ="SUM090_link", by.y="SUM090", all.x=TRUE, incomparables=NA)
	}
  
  Block.39.excld= subset(Block.39.excld, select=-c(SUM090_lin,SUM090_link ))

  Block.39.excld = Block.39.excld[with(Block.39.excld,order(sort)),]         # sort by sort
  write.dbf(Block.39.excld, "G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6_new1282014.dbf")
  #working version G:\\Faculty\\Mann\\Share\\Final_Variables_2\\New_CBGNum_outputs5\\BG_splt_090_Exclusion6_Original.dbf
          
 

####################################################################################
####################################################################################
#################  OTHER OPERATIONS ################################################
####################    CREATE SUM090 MAP THAT EXCLUDEDS EXCLUDED AREAS  #######
## after exporting I build an attribute table and converted to polygons in arc, => .tif"

  mask = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Exclusion Layer\\All_Mrg_PubWatEasMil4v2_rst.tif" ))
  CBG = getValues(raster("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables\\.tif"))  #real unique id   
  CBG_rst = raster("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables\\.tif")   # get raster  
  
  CBG[mask==1]  = -9999                        # remove exclusion areas
  CBG[is.na(CBG)]  = -9999                        # remove exclusion areas

  CBG_rst[] = CBG                           # replace values
  writeRaster(CBG_rst, filename="C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables\\.tif", format="GTiff",datatype='INT2U', overwrite=TRUE)


  plot(CBG_rst)
  
################################################################################
#################### VISUALIZATIONS  ########################################









