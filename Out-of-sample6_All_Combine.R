
####  takes all county level rural and urban projections and combines them into one shapefile


library(rgdal)
library(splm)
library(maptools)
library(sp)

# combine all county shapefiles into one 

VERSION = "6.9ppnoneTUrbNS+D20km"

#setwd(paste("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\CountyRun\\Jepson_individual\\",VERSION,sep=""))
setwd(paste("G:\\Faculty\\Mann\\Share\\Final_Variables_2\\CountyRun\\JepsonGroups6\\JepsonAreaSpecCity",VERSION,sep=""))
contents =  dir()[grep(".shp",dir())]

x = readShapePoly(contents[1])                                  # preload
x@data = subset(x@data,select=-c(HuDac4_49_,HuDac4_59_,HuDac4_69_,HuDac4_79_,HuDac4_89_,HuDac4_99_,HuDac4_109 ))   # remove problem names
x1 = subset(x@data, select=c(names(x@data[,grep("HuDac4_",names(x@data))])))       # get real data
names(x1) = c("HuDac4_39", "HuDac4_49", "HuDac4_59", "HuDac4_69", "HuDac4_79", "HuDac4_89", "HuDac4_99", "HuDac4_109", "HuDac4_119","HuDac4_129","HuDac4_139", "HuDac4_149")
x@data = subset(x@data, select=-c(HuDac4_39,HuDac4_49_.1,HuDac4_59_.1,HuDac4_69_.1,HuDac4_79_.1,HuDac4_89_.1,HuDac4_99_.1,HuDac4_109.1, HuDac4_119, HuDac4_129 ,HuDac4_139, HuDac4_149)   )  # remove wrong name
x@data = cbind(x@data, x1)

x = spChFIDs(x,as.character(x$SUM090))                          # set spatial id (must be unique)
  for(i in 2:length(contents)){
    print(paste("# i =",i,contents[i],sep=" "))
    y = readShapePoly(contents[i])
    
    y@data = subset(y@data,select=-c(HuDac4_49_,HuDac4_59_,HuDac4_69_,HuDac4_79_,HuDac4_89_,HuDac4_99_,HuDac4_109 ))   # remove problem names
    x1 = subset(y@data, select=c(names(y@data[,grep("HuDac4_",names(y@data))])))       # get real data
    names(x1) = c("HuDac4_39", "HuDac4_49", "HuDac4_59", "HuDac4_69", "HuDac4_79", "HuDac4_89", "HuDac4_99", "HuDac4_109", "HuDac4_119","HuDac4_129","HuDac4_139", "HuDac4_149")
    y@data = subset(y@data, select=-c(HuDac4_39,HuDac4_49_.1,HuDac4_59_.1,HuDac4_69_.1,HuDac4_79_.1,HuDac4_89_.1,HuDac4_99_.1,HuDac4_109.1, HuDac4_119, HuDac4_129 ,HuDac4_139, HuDac4_149)   )  # remove wrong name
    y@data = cbind(y@data, x1)
    
    y = spChFIDs(y,as.character(y$SUM090))
    if( sum(unique(as.character(x$SUM090)) %in%  unique(as.character(y$SUM090))) >0 ){next}    # avoid duplicate counties (where urban and rural couldn't be seperated)
                # %in% finds matching ids and sum counts number of non-unique values
    x = spRbind(x,y)    
  }
 proj4string(x)= proj4string=CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")

 
writeOGR(x, getwd(), paste("SpecifyEcoCity",VERSION,sep=""), driver="ESRI Shapefile")
write.dbf(slot(x,"data"),file=paste("SpecifyEcoCity",VERSION,sep=""),factor2char=T)  



#