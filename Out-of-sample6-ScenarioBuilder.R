
rm(list =  ls() )
unfactor <- function(factors){return(as.numeric(as.character(factors)))}


##########################################################################
#### rural scenario builder ####

# add_rural_growth_rate is a multiplier put .9 for -10% or 1.1 for +10%
growth_scenariosA(scenario_name= "Ra25",add_rural_growth_rate = 1.25, Rural_threshold=urban_rural_threshold)
# Scenario A: scale up or down rural growth by uniform rate, adjust urban house count to  compensate
growth_scenariosA <- function(scenario_name, add_rural_growth_rate, Rural_threshold) {
    print("Choose the .dbf file for the polygons of interest")
    pather = file.choose(new = TRUE) 
    
    Density.dbf = read.dbf(paste(pather),as.is=T)   #read dbf file for edit
    Density.dbf = subset(Density.dbf, select=c(SUM090,COUNTY,Est_Acr,HuDac4_99,HuDac4_109,HuDac4_119,HuDac4_129,HuDac4_139,HuDac4_149))
    Density.dbf = reshape(Density.dbf, dir = "long", sep="_", varying= c( grep("HuDac4",names(Density.dbf)) ))
    
    
    Density.dbf$Cum   = Density.dbf$Est_Acr * Density.dbf$HuDac4    # get house counts 
    ruralin99 = as.data.frame( Density.dbf$HuDac4[Density.dbf$time==99] <= Rural_threshold  )   # bring rural threshold in 
    names(ruralin99) = "ruralin99"
    ruralin99$id =  lapply(strsplit( rownames(ruralin99),"-"), function(x) x[1])
    Density.dbf = merge(Density.dbf, ruralin99, by="id")
    Density.dbf = pdata.frame(Density.dbf, index=c("id","time"), drop.index = F, row.names = T)
    
    # Calculate rural density at additional rate
    Density.dbf$HuDac4_lag = lag(Density.dbf$HuDac4,1)
    Density.dbf$rate =  Density.dbf$HuDac4 / Density.dbf$HuDac4_lag
    Density.dbf$rate_plus = Density.dbf$rate 
    Density.dbf$rate_plus[Density.dbf$ruralin99] = Density.dbf$rate[Density.dbf$ruralin99] + (add_rural_growth_rate-1)      # new growth rate for areas rural in '99
    Density.dbf$HuDac4_scenario  = Density.dbf$HuDac4_lag * Density.dbf$rate_plus             # concurrent (not lagged) density based on new growth rate
    
    # adjust urban counts for rural growth 
    for( dater in  c(109,119,129,139,149) ){
        # change number of houses in scenario vs. bau 
        additional_rural = sum(  (Density.dbf$HuDac4_scenario[unfactor(Density.dbf$time)==dater]-Density.dbf$HuDac4[unfactor(Density.dbf$time)==dater]) * Density.dbf$Est_Acr[unfactor(Density.dbf$time)==dater], na.rm=T ) 
        
        Density.dbf$urban_select = Density.dbf$time == dater & Density.dbf$ruralin99 == F                       # areas with year dater and rural in 99
        urban_houses = sum(Density.dbf$HuDac4[Density.dbf$urban_select ] * Density.dbf$Est_Acr[Density.dbf$urban_select])   # count of urban houses
        urban_less_rural = urban_houses - additional_rural                                          # urban house target moving houses from(to) urban to(from)rural
        
        urban_scaler <- function(percent_scaler){
            # use optimize to reduce urban by a % until it removes houses = count or additional rural houses
            urban_houses_scaler = sum(Density.dbf$HuDac4[Density.dbf$urban_select] * Density.dbf$Est_Acr[Density.dbf$urban_select] * percent_scaler) 
            print(paste("Diff in housing = ",abs(urban_houses_scaler-urban_less_rural)))
            return( abs(urban_houses_scaler-urban_less_rural) )
        }
        
        # use optimize to increase rural by a % until it addes houses = count or additional rural houses
        percent_scaler = nlminb(  start=0, objective=urban_scaler, lower=0, upper=20)
        # replace new density for rural growth scenario
        change_urban = (urban_houses * percent_scaler$par) - urban_houses
        print(paste("In year ",dater,"Additional Rural Count = ",additional_rural,"Change in Urban Count = ",change_urban,"percent = ",(percent_scaler$par)))
        
        # probably dont need to multiply and devide by * Density.dbf$Est_Acr[urban_select]
        Density.dbf$HuDac4_scenario[Density.dbf$urban_select] = (Density.dbf$HuDac4_scenario[Density.dbf$urban_select]  * (percent_scaler$par))  # Calc new urban housing density (rural already done)
        # check that sum of houses from scenario matches non-scenario
    }
    # Change scenario name
    namer= names(Density.dbf)
    namer[namer=="HuDac4_scenario"] =  paste("HuD",scenario_name,sep="_")
    print(paste("The attribute for this scenario is HuD",scenario_name,sep="_") )
    names(Density.dbf) = namer
    Density.dbf <<- subset(Density.dbf, select=-c(HuDac4_lag,rate, rate_plus,ruralin99 ))
    head(Density.dbf)
    
    #print("Export back to polygons? 1 = y, 0 = n: IF YOU WANT TO DO ANOTHER PRESS 0")
    #y = scan(nlines=1)
    #if(y==1){ 
    
    Density.dbf2 = read.dbf(paste(pather),as.is=T)   #read dbf file for edit
    Density.dbf2$sort = 1:dim(Density.dbf2)[1]
    Density.dbf  = subset(Density.dbf, select=-c(COUNTY,Est_Acr, HuDac4, Cum, urban_select))
    Density.dbf  = Density.dbf[Density.dbf$time != 99,]  # no estimate for 99 
    
    #print('hope this works... double check that ')
    #Density.dbf[is.na(Density.dbf$)] = 0 
    
    Density.dbf3 = reshape(Density.dbf,idvar="id", timevar="time", direction="wide" )
    Density.dbf3 = Density.dbf3[ ,c(2,grep("HuD_", names(Density.dbf3)))]
    Density.dbf2 = merge(Density.dbf2,Density.dbf3, by.x="SUM090" , by.y="SUM090.109")
    Density.dbf2 = Density.dbf2[with(Density.dbf2, order(sort) ), ]                  # resort to polygon order
    
    write.dbf(Density.dbf2,file=paste(pather))
    # }
    remove(y)
}  


##########################################################################
##########################################################################

growth_scenariosB(scenario_name= "Ub205",fraction_new_to_rural_in = .05, Rural_threshold=1)
# Scenario B: For all new houses partition them between urban and rural with
# fraction_new_to_rural_in % going to rural areas all at the county level 
growth_scenariosB <- function(scenario_name, counties_in ,fraction_new_to_rural_in, Rural_threshold) {
    print("Choose the .dbf file for the polygons of interest")
    pather = file.choose(new = TRUE) 
    
    Density.dbf = read.dbf(paste(pather),as.is=T )   #read dbf file for edit
    Density.dbf = subset(Density.dbf, select=c(SUM090,COUNTY,Acres,HuDac4_99,HuDac4_109,HuDac4_119,HuDac4_129,HuDac4_139,HuDac4_149))
    Density.dbf = reshape(Density.dbf, dir = "long", sep="_", varying= c( grep("HuDac4",names(Density.dbf)) ) )
    Density.dbf$Cum   = Density.dbf$Acres * Density.dbf$HuDac4    # get house counts 
    Density.dbf$ruralin99 = Density.dbf$HuDac4[Density.dbf$time==99]<= Rural_threshold
    Density.dbf <- pdata.frame(Density.dbf, index=c("id","time"), drop.index = F, row.names = T)
    Density.dbf$COUNTY = unfactor(  Density.dbf$COUNTY)
    Density.dbf$time   = unfactor(Density.dbf$time)
    
    
    #     head(Density.dbf[Density.dbf$ruralin99==T & Density.dbf$Cum!=0 & Density.dbf$id==969,])
    #     head(Density.dbf[Density.dbf$ruralin99==T & Density.dbf$Cum!=0& Density.dbf$id==998,])
    #     head(Density.dbf[Density.dbf$ruralin99==F & Density.dbf$Cum!=0& Density.dbf$id==1,])
    #     head(Density.dbf[Density.dbf$ruralin99==T,],200)
    #     head(Density.dbf[Density.dbf$ruralin99==T,"Scenario"]-Density.dbf[Density.dbf$ruralin99==T,"HuDac4"],200)
    #     Density.dbf$HuDac4
    #     
    # get house counts for each county and time period 
    county_time_cum = aggregate( Cum ~  COUNTY+ time , data=Density.dbf , sum)
    county_time_cum = county_time_cum[order(county_time_cum$COUNTY ),]
    county_time_cum = as.data.frame(pdata.frame(county_time_cum, index=c("COUNTY","time"), drop.index = F, row.names = T),stringsAsFactors=F)
    county_time_cum$new_homes = diff(county_time_cum$Cum,1)
    county_time_cum$COUNTY = unfactor(county_time_cum$COUNTY)
    county_time_cum$time = unfactor(county_time_cum$time)
    
    # get rural urban counts 
    rural_county_time_cum = aggregate( Cum ~  COUNTY+ time , data=Density.dbf[Density.dbf$ruralin99==T,] , sum) 
    rural_county_time_cum = as.data.frame(pdata.frame(rural_county_time_cum, index=c("COUNTY","time"), drop.index = F, row.names = T),stringsAsFactors=F)
    rural_county_time_cum$COUNTY = unfactor(rural_county_time_cum$COUNTY)
    rural_county_time_cum$time = unfactor(rural_county_time_cum$time)
    
    urban_county_time_cum = aggregate( Cum ~  COUNTY+ time , data=Density.dbf[Density.dbf$ruralin99==F,] , sum) 
    urban_county_time_cum = as.data.frame(pdata.frame(urban_county_time_cum, index=c("COUNTY","time"), drop.index = F, row.names = T),stringsAsFactors=F)
    urban_county_time_cum$COUNTY = unfactor(urban_county_time_cum$COUNTY)
    urban_county_time_cum$time = unfactor(urban_county_time_cum$time)
    
    Density.dbf$Scenario = Density.dbf$HuDac4
    
    for( county_j in 1:length(unique(county_time_cum$COUNTY))){
        #  PROBLEM WITH COUNTY 3  J=2
        # PROBLEM QUICKLY JUMPS UP DENSITY FOR RURAL AREAS IN FIRST PERIOD    
        for(time_i in 1:length(unique(county_time_cum$time) )){
            if(time_i == 1 ){next}  # avoid 1999 
            print(paste("County:",unique(county_time_cum$COUNTY)[county_j]))
            print(paste("Time:",unique(county_time_cum$time)[time_i]))
            
            # GRADUALLY MOVE TO FULL FRACTION TO AVOID JUMP?
            if(time_i == 2 ){fraction_new_to_rural = fraction_new_to_rural_in *.25}
            if(time_i == 3 ){fraction_new_to_rural = fraction_new_to_rural_in *.50}
            if(time_i == 4 ){fraction_new_to_rural = fraction_new_to_rural_in *.75}
            print("remove this later")
            if(time_i != 3 & time_i != 2 & time_i != 4){fraction_new_to_rural = fraction_new_to_rural_in}
            
            houses_to_redistribute = county_time_cum$new_homes[county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  county_time_cum$time ==unique(county_time_cum$time)[time_i]]
            new_urban_house_to_redist = houses_to_redistribute  * (1-fraction_new_to_rural)
            new_rural_house_to_redist = houses_to_redistribute  * (fraction_new_to_rural)
            
            urban_previous_house_cum = urban_county_time_cum$Cum[urban_county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  urban_county_time_cum$time ==unique(county_time_cum$time)[time_i-1]]
            rural_previous_house_cum = rural_county_time_cum$Cum[rural_county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  rural_county_time_cum$time ==unique(county_time_cum$time)[time_i-1]]
            
            if(length(urban_previous_house_cum) == 0){urban_previous_house_cum=0}  # avoid empty vector
            if(length(rural_previous_house_cum) == 0){rural_previous_house_cum=0} 
            if(length(rural_previous_house_cum) == 0 & length(urban_previous_house_cum) == 0 ) {next} # avoid if no houses anywhere 
            if(urban_previous_house_cum < new_urban_house_to_redist     ){
                # PROBLEM SHOULD FIGURE OUT WHETHER MOVING HOUSES TO OR AWAY FROM URBAN
                new_rural_house_to_redist =  new_rural_house_to_redist - (urban_previous_house_cum - new_urban_house_to_redist)# add unavailable houses to rural redist
                new_urban_house_to_redist = houses_to_redistribute - new_rural_house_to_redist
            }
            if(rural_previous_house_cum < new_rural_house_to_redist    ){
                # PROBLEM SHOULD FIGURE OUT WHETHER MOVING HOUSES TO OR AWAY FROM rural IN LOGIC ABOVE
                new_urban_house_to_redist =  new_urban_house_to_redist - (rural_previous_house_cum - new_rural_house_to_redist) # add unavailable houses to urban redist
                new_rural_house_to_redist =  houses_to_redistribute - new_urban_house_to_redist  # set rural target to distribute remaining and available 
            }
            
            urban_target_cum =  new_urban_house_to_redist +  urban_previous_house_cum
            rural_target_cum =  new_rural_house_to_redist +  rural_previous_house_cum
            
            urban_selector = Density.dbf$ruralin99==F & Density.dbf$COUNTY == unique(county_time_cum$COUNTY)[county_j] & Density.dbf$time == unique(county_time_cum$time)[time_i]
            rural_selector = Density.dbf$ruralin99==T & Density.dbf$COUNTY == unique(county_time_cum$COUNTY)[county_j] & Density.dbf$time == unique(county_time_cum$time)[time_i]
            
            urban_count_scaler <- function(percent_scaler_u){                   
                # total number of houses scaled by function 
                #urban_houses_scaler = sum( Density.dbf$Scenario[urban_selector] * Density.dbf$Acres[urban_selector] * percent_scaler) 
                urban_houses_scaler = sum( Density.dbf$Cum[urban_selector]   * percent_scaler_u) 
                print(paste("Diff in housing = ",urban_target_cum ,   urban_houses_scaler))
                return(  (    urban_target_cum -   urban_houses_scaler      )^2 ) 
            }
            
            # use optimize to increase rural by a % until it addes houses = count or additional rural houses
            urban_percent_scaler = nlminb(  start=0.5, objective=urban_count_scaler)
            urban_percent_scaler$par
            sum( Density.dbf$Cum[urban_selector]   * urban_percent_scaler$par)
            
            rural_count_scaler <- function(percent_scaler_r){                   
                # total number of houses scaled by function 
                rural_houses_scaler = sum( Density.dbf$Cum[rural_selector]   * percent_scaler_r) 
                print(paste("Diff in housing = ", rural_target_cum ,  rural_houses_scaler))
                return(  (    rural_target_cum -   rural_houses_scaler      )^2) 
            }
            
            # use optimize to increase rural by a % until it addes houses = count or additional rural houses
            rural_percent_scaler = nlminb( start=0.5, objective=rural_count_scaler)
            rural_percent_scaler$par
            sum( Density.dbf$Cum[rural_selector]   * rural_percent_scaler$par)
            
            
            print( paste("Total rural house count:",sum( Density.dbf$Cum[rural_selector]   * rural_percent_scaler$par)    ))
            print( paste("With a target of:",rural_target_cum))
            
            print( paste("New urban house count:",sum(Density.dbf$Cum[urban_selector]  * urban_percent_scaler$par)  ))
            print( paste("With a target of:",urban_target_cum))
            print( rural_percent_scaler$message )
            print( urban_percent_scaler$message )
            
            Density.dbf$Scenario[rural_selector] = (Density.dbf$Cum[rural_selector] * rural_percent_scaler$par) /Density.dbf$Acres[rural_selector]
            Density.dbf$Scenario[urban_selector] = (Density.dbf$Cum[urban_selector] * urban_percent_scaler$par) / Density.dbf$Acres[urban_selector]              
        }             
    } 
    
    namer= names(Density.dbf)
    namer[namer=="Scenario"] =  paste("HuD",scenario_name,sep="_")
    print(paste("The attribute for this scenario is HuD",scenario_name,sep="_") )
    names(Density.dbf) = namer
    Density.dbf = subset(Density.dbf, select=-c(Acres, Cum,ruralin99, COUNTY, HuDac4 ))
    head(Density.dbf)
    
    Density.dbf2 = read.dbf(paste(pather),as.is=T)   #read dbf file for edit
    Density.dbf2$sort = 1:dim(Density.dbf2)[1]
    Density.dbf  = Density.dbf[Density.dbf$time != 99,]  # no estimate for 99 
    
    #print('hope this works... double check that ')
    Density.dbf[is.na(Density.dbf[,paste("HuD",scenario_name,sep="_")])] = 0 
    
    Density.dbf3 = reshape(Density.dbf,idvar="id", timevar="time", direction="wide" )
    Density.dbf3 = Density.dbf3[ ,c(2,grep("HuD_", names(Density.dbf3)))]
    Density.dbf2 = merge(Density.dbf2,Density.dbf3, by.x="SUM090" , by.y="SUM090.109")
    Density.dbf2 = Density.dbf2[with(Density.dbf2, order(sort) ), ]                  # resort to polygon order
    
    write.dbf(Density.dbf2,file=paste(pather))    
}


##########################################################################
##########################################################################

growth_scenariosC(Rural_threshold=1,scenario_name= "u3",counties_list_in=jepFIDS,fraction_new_to_rural_in = 0.1)
# Scenario C: For all new houses partition them between urban and rural with
# fraction_new_to_rural_in % going to rural areas all at the county level 
# where urban is defined as urban and urban neighbors. 
growth_scenariosC <- function(scenario_name, counties_list_in ,fraction_new_to_rural_in, Rural_threshold) {
    print("Choose the .dbf file for the polygons of interest")
    pather = file.choose(new = TRUE) 
    
    if(exists("Block.39.ex" )==F){ print("Reading in Shapefile... will take a while")
                                   Block.39.ex = readOGR("C:\\Users\\mmann1123\\Desktop\\Share\\Final_Variables_2\\New_CBGNum_outputs5","BG_splt_090_Exclusion6")}
    unfactor <- function(factors){return(as.numeric(as.character(factors)))}
    "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
    Density.dbf = read.dbf(paste(pather),as.is=T )   #read dbf file for edit
    Density.dbf = subset(Density.dbf, select=c(SUM090,COUNTY,Acres,HuDac4_99,HuDac4_109,HuDac4_119,HuDac4_129,HuDac4_139,HuDac4_149))
    Density.dbf = reshape(Density.dbf, dir = "long", sep="_", varying= c( grep("HuDac4",names(Density.dbf)) ) )
    Density.dbf$Cum   = Density.dbf$Acres * Density.dbf$HuDac4    # get house counts 
    Density.dbf$ruralin99 = Density.dbf$HuDac4[Density.dbf$time==99]<= Rural_threshold
    Density.dbf <- pdata.frame(Density.dbf, index=c("id","time"), drop.index = F, row.names = T)
    Density.dbf$COUNTY = unfactor(  Density.dbf$COUNTY)
    Density.dbf$time   = unfactor(Density.dbf$time)
    
    # hold space for scenario 
    Density.dbf$Scenario = Density.dbf$HuDac4
    Density.dbf$Cum_Scenario = Density.dbf$Cum
    
    for(county_j in 1:length(counties_list_in )){
        COUNTIES = counties_list_in[[county_j]]
        
        # get house counts for each county group and time period 
        county_time_cum = aggregate( Cum ~  time , data=Density.dbf[Density.dbf$COUNTY %in% COUNTIES] , sum)
        county_time_cum$new_homes = c(NA, diff(county_time_cum$Cum,1))
        
        # define areas acceptable for urban expansion 
        Block.County = Block.39.ex[ unfactor(Block.39.ex$COUNTY) %in% COUNTIES ,]
        print("starting neighborhood")
        PNN3=poly2nb(Block.County, snap=c(500),row.names=as.character(1:length(Block.County)) )     # change row.names to match polygon.id from attributes(DNN3)
        Block.County$ruralin99 = Block.County$HuDac4_99<= Rural_threshold
        
        # remove all neighbors for nonurban 
        for(x in 1:length(PNN3)){     
            if( Block.County$ruralin99[x] == T ){ PNN3[[x]]  = as.integer(0)}
        }
        Block.County = subset(Block.County, select=-c(ruralin99))
        row_of_urban_and_neigh = unique(unlist(PNN3))[order(unique(unlist(PNN3)))] # row number of all urban and polyneigh
        Block.County$urbandneigh = F                             # placeholder & reset
        Block.County$urbandneigh[row_of_urban_and_neigh] =  T
        Block.County$urbandneigh[1:length(Block.County$urbandneigh) %w/o% row_of_urban_and_neigh] = F
        
        
        time_periods = unique(county_time_cum$time)
        Density.dbf$urbandneigh = F # create space
        for(time in time_periods){
            # put urbanneigh into density.dbf without using merge!  for each time period since match only returns first instance 
            row_in_Density.dbf = match(Block.County$SUM090 ,Density.dbf$SUM090[Density.dbf$time == time]) 
            Density.dbf$urbandneigh[Density.dbf$time == time][row_in_Density.dbf] = Block.County$urbandneigh
        }
        
        #head(Density.dbf[Density.dbf$COUNTY==1,],300)
        #head(Density.dbf[Density.dbf$COUNTY==1 & Density.dbf$urbandneigh==F,c("HuDac4","Scenario")],300)
        
        for(time_i in 1:length(unique(county_time_cum$time) )){
            if(time_i == 1 ){next}  # avoid 1999 
            print(paste("County:", COUNTIES))
            print(paste("Time:",unique(county_time_cum$time)[time_i]))   
            
            # get rural urban counts 
            rural_county_time_cum = aggregate( Cum_Scenario ~ time , data=Density.dbf[Density.dbf$urbandneigh==F & Density.dbf$COUNTY %in% COUNTIES,] , sum) 
            rural_county_time_cum$time = unfactor(rural_county_time_cum$time)
            
            urban_neigh_county_time_cum = aggregate( Cum_Scenario ~    time , data=Density.dbf[Density.dbf$urbandneigh==T & Density.dbf$COUNTY %in% COUNTIES,] , sum) 
            urban_neigh_county_time_cum$time = unfactor(urban_neigh_county_time_cum$time)
            
            # GRADUALLY MOVE TO FULL FRACTION TO AVOID JUMP?
            if(time_i == 2 ){ print('reducing rural fraction to 25% of X, easing into it... ')
                              fraction_new_to_rural = fraction_new_to_rural_in *.25}
            if(time_i == 3 ){print('reducing rural fraction to 50% of X, easing into it... ')
                             fraction_new_to_rural = fraction_new_to_rural_in *.50}
            if(time_i == 4 ){print('reducing rural fraction to 75% of X, easing into it... ')
                             fraction_new_to_rural = fraction_new_to_rural_in *.75}
            print("remove this later")
            if(time_i != 3 & time_i != 2 & time_i != 4){fraction_new_to_rural = fraction_new_to_rural_in}
            
            houses_to_redistribute = county_time_cum$new_homes[ county_time_cum$time ==unique(county_time_cum$time)[time_i]]
            new_urban_house_to_redist = houses_to_redistribute  * (1-fraction_new_to_rural)
            new_rural_house_to_redist = houses_to_redistribute  * (fraction_new_to_rural)
            
            urban_previous_house_cum = urban_neigh_county_time_cum$Cum_Scenario[  urban_neigh_county_time_cum$time ==unique(county_time_cum$time)[time_i-1]]
            rural_previous_house_cum = rural_county_time_cum$Cum_Scenario[ rural_county_time_cum$time ==unique(county_time_cum$time)[time_i-1]]
            
            if(length(urban_previous_house_cum) == 0){urban_previous_house_cum=0}  # avoid empty vector
            if(length(rural_previous_house_cum) == 0){rural_previous_house_cum=0} 
            if(length(rural_previous_house_cum) == 0 & length(urban_previous_house_cum) == 0 ) {next} # avoid if no houses anywhere 
            if(urban_previous_house_cum < new_urban_house_to_redist     ){
                # PROBLEM SHOULD FIGURE OUT WHETHER MOVING HOUSES TO OR AWAY FROM URBAN
                new_rural_house_to_redist =  new_rural_house_to_redist - (urban_previous_house_cum - new_urban_house_to_redist)# add unavailable houses to rural redist
                new_urban_house_to_redist = houses_to_redistribute - new_rural_house_to_redist
            }
            if(rural_previous_house_cum < new_rural_house_to_redist    ){
                # PROBLEM SHOULD FIGURE OUT WHETHER MOVING HOUSES TO OR AWAY FROM rural IN LOGIC ABOVE
                new_urban_house_to_redist =  new_urban_house_to_redist - (rural_previous_house_cum - new_rural_house_to_redist) # add unavailable houses to urban redist
                new_rural_house_to_redist =  houses_to_redistribute - new_urban_house_to_redist  # set rural target to distribute remaining and available 
            }
            
            urban_target_cum =  new_urban_house_to_redist +  urban_previous_house_cum
            rural_target_cum =  new_rural_house_to_redist +  rural_previous_house_cum
            
            urban_selector = Density.dbf$urbandneigh==T & Density.dbf$COUNTY %in% COUNTIES  & Density.dbf$time == unique(county_time_cum$time)[time_i]
            rural_selector = Density.dbf$urbandneigh==F & Density.dbf$COUNTY %in% COUNTIES  & Density.dbf$time == unique(county_time_cum$time)[time_i]
            
            urban_count_scaler <- function(percent_scaler_u){                   
                # total number of houses scaled by function 
                #urban_houses_scaler = sum( Density.dbf$Scenario[urban_selector] * Density.dbf$Acres[urban_selector] * percent_scaler) 
                urban_houses_scaler = sum( Density.dbf$Cum_Scenario[urban_selector]   * percent_scaler_u) 
                print(paste("Diff in housing = ",urban_target_cum ,   urban_houses_scaler))
                return(  (    urban_target_cum -   urban_houses_scaler      )^2 ) 
            }
            
            # use optimize to increase rural by a % until it addes houses = count or additional rural houses
            urban_percent_scaler = nlminb(  start=0.5, objective=urban_count_scaler)
            urban_percent_scaler$par
            
            rural_count_scaler <- function(percent_scaler_r){                   
                # total number of houses scaled by function 
                rural_houses_scaler = sum( Density.dbf$Cum_Scenario[rural_selector]   * percent_scaler_r) 
                print(paste("Diff in housing = ", rural_target_cum ,  rural_houses_scaler))
                return(  (    rural_target_cum -   rural_houses_scaler      )^2) 
            }
            
            # use optimize to increase rural by a % until it addes houses = count or additional rural houses
            rural_percent_scaler = nlminb( start=0.5, objective=rural_count_scaler)
            rural_percent_scaler$par            
            
            print( paste("Total rural house count:",sum( Density.dbf$Cum_Scenario[rural_selector]   * rural_percent_scaler$par)    ))
            print( paste("With a target of:",rural_target_cum))
            
            print( paste("New urban house count:",sum(Density.dbf$Cum_Scenario[urban_selector]  * urban_percent_scaler$par)  ))
            print( paste("With a target of:",urban_target_cum))
            print( rural_percent_scaler$message )
            print( urban_percent_scaler$message )
            
            Density.dbf$Scenario[rural_selector] = (Density.dbf$Cum_Scenario[rural_selector] * rural_percent_scaler$par) /Density.dbf$Acres[rural_selector]
            Density.dbf$Scenario[urban_selector] = (Density.dbf$Cum_Scenario[urban_selector] * urban_percent_scaler$par) / Density.dbf$Acres[urban_selector]              
            
            Density.dbf$Cum_Scenario[rural_selector] = (Density.dbf$Cum_Scenario[rural_selector] * rural_percent_scaler$par)  
            Density.dbf$Cum_Scenario[urban_selector] = (Density.dbf$Cum_Scenario[urban_selector] * urban_percent_scaler$par)                          
        }             
    } 
    
    namer= names(Density.dbf)
    namer[namer=="Scenario"] =  paste("HuD",scenario_name,sep="_")
    print(paste("The attribute for this scenario is HuD",scenario_name,sep="_") )
    names(Density.dbf) = namer
    Density.dbf = subset(Density.dbf, select=-c(Acres, Cum,ruralin99, COUNTY, HuDac4 ))
    head(Density.dbf)
    
    Density.dbf2 = read.dbf(paste(pather),as.is=T)   #read dbf file for edit
    Density.dbf2$sort = 1:dim(Density.dbf2)[1]
    Density.dbf  = Density.dbf[Density.dbf$time != 99,]  # no estimate for 99 
    
    #print('hope this works... double check that ')
    Density.dbf[is.na(Density.dbf[,paste("HuD",scenario_name,sep="_")])] = 0 
    
    Density.dbf3 = reshape(Density.dbf,idvar="id", timevar="time", direction="wide" )
    Density.dbf3 = Density.dbf3[ ,c(2,grep("HuD_", names(Density.dbf3)))]
    Density.dbf2 = merge(Density.dbf2,Density.dbf3, by.x="SUM090" , by.y="SUM090.109")
    Density.dbf2 = Density.dbf2[with(Density.dbf2, order(sort) ), ]                  # resort to polygon order
    
    write.dbf(Density.dbf2,file=paste(pather))    
}


##########################################################################
##########################################################################


############ used in paper scenario analysis... 

growth_scenariosD(scenario_name= "Ud25",add_fraction_new_to_rural = -.25, Rural_threshold=1)
# Scenario D: For all new houses partition them between urban and rural with
# fraction_new_to_rural_in orginal split plus or minus some % going to rural areas all at the county level 
# add_fraction_new_to_rural +0.25 for 25% more rural, -0.25 for 25% less rural
growth_scenariosD <- function(scenario_name, counties_in ,add_fraction_new_to_rural, Rural_threshold) {
    print("Choose the .dbf file for the polygons of interest")
    pather = file.choose(new = TRUE) 
    
    Density.dbf = read.dbf(paste(pather),as.is=T )   #read dbf file for edit
    Density.dbf = subset(Density.dbf, select=c(SUM090,COUNTY,Acres,HuDac4_99,HuDac4_109,HuDac4_119,HuDac4_129,HuDac4_139,HuDac4_149))
    Density.dbf = reshape(Density.dbf, dir = "long", sep="_", varying= c( grep("HuDac4",names(Density.dbf)) ) )
    Density.dbf$Cum   = Density.dbf$Acres * Density.dbf$HuDac4    # get house counts 
    Density.dbf$ruralin99 = Density.dbf$HuDac4[Density.dbf$time==99]<= Rural_threshold
    Density.dbf <- pdata.frame(Density.dbf, index=c("id","time"), drop.index = F, row.names = T)
    Density.dbf$COUNTY = unfactor(  Density.dbf$COUNTY)
    Density.dbf$time   = unfactor(Density.dbf$time)
    
    
    #     head(Density.dbf[Density.dbf$ruralin99==T & Density.dbf$Cum!=0 & Density.dbf$id==969,])
    #     head(Density.dbf[Density.dbf$ruralin99==T & Density.dbf$Cum!=0& Density.dbf$id==998,])
    #     head(Density.dbf[Density.dbf$ruralin99==F & Density.dbf$Cum!=0& Density.dbf$id==1,])
    #     head(Density.dbf[Density.dbf$ruralin99==T,],200)
    #     head(Density.dbf[Density.dbf$ruralin99==T,"Scenario"]-Density.dbf[Density.dbf$ruralin99==T,"HuDac4"],200)
    #     Density.dbf$HuDac4
    #     
    # get house counts for each county and time period 
    county_time_cum = aggregate( Cum ~  COUNTY+ time , data=Density.dbf , sum)
    county_time_cum = county_time_cum[order(county_time_cum$COUNTY ),]
    county_time_cum = as.data.frame(pdata.frame(county_time_cum, index=c("COUNTY","time"), drop.index = F, row.names = T),stringsAsFactors=F)
    county_time_cum$new_homes = diff(county_time_cum$Cum,1)    # possible issue... total new homes and new homes same
    county_time_cum$COUNTY = unfactor(county_time_cum$COUNTY)
    county_time_cum$time = unfactor(county_time_cum$time)
    county_time_cum$total_newhomes =  diff(county_time_cum$Cum,1)
    
    # get rural urban counts 
    rural_county_time_cum = aggregate( Cum ~  COUNTY+ time , data=Density.dbf[Density.dbf$ruralin99==T,] , sum) 
    rural_county_time_cum = as.data.frame(pdata.frame(rural_county_time_cum, index=c("COUNTY","time"), drop.index = F, row.names = T),stringsAsFactors=F)
    rural_county_time_cum$COUNTY = unfactor(rural_county_time_cum$COUNTY)
    rural_county_time_cum$time = unfactor(rural_county_time_cum$time)
    rural_county_time_cum$rural_newhomes =  diff(rural_county_time_cum$Cum,1)
    
    urban_county_time_cum = aggregate( Cum ~  COUNTY+ time , data=Density.dbf[Density.dbf$ruralin99==F,] , sum) 
    urban_county_time_cum = as.data.frame(pdata.frame(urban_county_time_cum, index=c("COUNTY","time"), drop.index = F, row.names = T),stringsAsFactors=F)
    urban_county_time_cum$COUNTY = unfactor(urban_county_time_cum$COUNTY)
    urban_county_time_cum$time = unfactor(urban_county_time_cum$time)
    urban_county_time_cum$urban_newhomes =  diff(urban_county_time_cum$Cum,1)
    
    head(rural_county_time_cum)
    head(urban_county_time_cum)
    Density.dbf$Scenario = Density.dbf$HuDac4    # start with default densities 
    
    county_time_cum_p = merge(county_time_cum,rural_county_time_cum,by=c('COUNTY','time'),all=T )
    county_time_cum_p = merge(county_time_cum_p,urban_county_time_cum,by=c('COUNTY','time'),all=T )
    county_time_cum_p$rural_newhomes[is.na(county_time_cum_p$rural_newhomes)]=0
    county_time_cum_p$urban_newhomes[is.na(county_time_cum_p$urban_newhomes)]=0
    
    # percent of growth urban or rural
    county_time_cum_p$per_rural_new = county_time_cum_p$rural_newhomes/county_time_cum_p$total_newhomes
    county_time_cum_p$per_urban_new = county_time_cum_p$urban_newhomes/county_time_cum_p$total_newhomes
    county_time_cum_p = county_time_cum_p[,c('time','COUNTY','total_newhomes','rural_newhomes','urban_newhomes','per_rural_new','per_urban_new')] 
    
    for( county_j in 1:length(unique(county_time_cum$COUNTY))){
        #  PROBLEM WITH COUNTY 3  J=2
        # PROBLEM QUICKLY JUMPS UP DENSITY FOR RURAL AREAS IN FIRST PERIOD    
        for(time_i in 1:length(unique(county_time_cum$time) )){
            if(time_i == 1 ){next}  # avoid 1999 
            print(paste("County:",unique(county_time_cum$COUNTY)[county_j]))
            print(paste("Time:",unique(county_time_cum$time)[time_i]))
            
            houses_to_redistribute = county_time_cum$new_homes[county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  county_time_cum$time ==unique(county_time_cum$time)[time_i]]
            urban_per_to_redistribute = county_time_cum_p$per_urban_new[county_time_cum_p$COUNTY == unique(county_time_cum_p$COUNTY)[county_j] &  county_time_cum_p$time ==unique(county_time_cum_p$time)[time_i]]  # lookup percentage for county and time
            rural_per_to_redistribute = county_time_cum_p$per_rural_new[county_time_cum_p$COUNTY == unique(county_time_cum_p$COUNTY)[county_j] &  county_time_cum_p$time ==unique(county_time_cum_p$time)[time_i]]
            
            bound01 <-function(x){max(min(x,1),0)} # bound calculation between zero and one 
            
            urban_new_per = bound01(urban_per_to_redistribute-add_fraction_new_to_rural) # redistribute: increase or decrease % by add_fraction_new_to_rural
            rural_ner_per = bound01(rural_per_to_redistribute+add_fraction_new_to_rural)
            
            if(round((urban_new_per+rural_ner_per),digits=2)!=1){stop("Urban and Rual Percentages don't add to 1")}
            
            new_urban_house_to_redist = houses_to_redistribute * urban_new_per  # get count of new rural and urban houses
            new_rural_house_to_redist = houses_to_redistribute * rural_ner_per
            
            # get previous housing counts for county and time t-1 to add houses too 
            urban_previous_house_cum = urban_county_time_cum$Cum[urban_county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  urban_county_time_cum$time ==unique(county_time_cum$time)[time_i-1]]
            rural_previous_house_cum = rural_county_time_cum$Cum[rural_county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  rural_county_time_cum$time ==unique(county_time_cum$time)[time_i-1]]
            
            if(length(urban_previous_house_cum) == 0){urban_previous_house_cum=0}  # avoid empty vector
            if(length(rural_previous_house_cum) == 0){rural_previous_house_cum=0} 
            if(length(rural_previous_house_cum) == 0 & length(urban_previous_house_cum) == 0 ) {next} # avoid if no houses anywhere 
            
            # estimate target housing counts for urban and rural 
            urban_target_cum =  new_urban_house_to_redist +  urban_previous_house_cum
            rural_target_cum =  new_rural_house_to_redist +  rural_previous_house_cum
            
            urban_selector = Density.dbf$ruralin99==F & Density.dbf$COUNTY == unique(county_time_cum$COUNTY)[county_j] & Density.dbf$time == unique(county_time_cum$time)[time_i]
            rural_selector = Density.dbf$ruralin99==T & Density.dbf$COUNTY == unique(county_time_cum$COUNTY)[county_j] & Density.dbf$time == unique(county_time_cum$time)[time_i]
            
            urban_count_scaler <- function(percent_scaler_u){                   
                # total number of houses scaled by function 
                urban_houses_scaler = sum( Density.dbf$Cum[urban_selector]   * percent_scaler_u) 
                print(paste("Diff in housing = ",urban_target_cum /   urban_houses_scaler))
                return(  (    urban_target_cum -   urban_houses_scaler      )^2 ) 
            }
            
            # use optimize to increase rural by a % until it addes houses = count or additional rural houses
            urban_percent_scaler = nlminb(  start=0.5, objective=urban_count_scaler)
            urban_percent_scaler$par
            sum( Density.dbf$Cum[urban_selector] * urban_percent_scaler$par)    # estimted housing count urban scenario
            
            rural_count_scaler <- function(percent_scaler_r){                   
                # total number of houses scaled by function 
                rural_houses_scaler = sum( Density.dbf$Cum[rural_selector]   * percent_scaler_r) 
                print(paste("Diff in housing = ", rural_target_cum /  rural_houses_scaler))
                return(  (    rural_target_cum -   rural_houses_scaler      )^2) 
            }
            
            # use optimize to increase rural by a % until it addes houses = count or additional rural houses
            rural_percent_scaler = nlminb( start=0.5, objective=rural_count_scaler)
            rural_percent_scaler$par
            sum( Density.dbf$Cum[rural_selector]   * rural_percent_scaler$par)   # estimted housing count rural scenario 
            
            print( paste("Total rural house count:",sum( Density.dbf$Cum[rural_selector]   * rural_percent_scaler$par)    ))
            print( paste("With a target of:",rural_target_cum))
            
            print( paste("New urban house count:",sum(Density.dbf$Cum[urban_selector]  * urban_percent_scaler$par)  ))
            print( paste("With a target of:",urban_target_cum))
            print( rural_percent_scaler$message )
            print( urban_percent_scaler$message )
            
            Density.dbf$Scenario[rural_selector] = (Density.dbf$Cum[rural_selector] * rural_percent_scaler$par) /Density.dbf$Acres[rural_selector]
            Density.dbf$Scenario[urban_selector] = (Density.dbf$Cum[urban_selector] * urban_percent_scaler$par) / Density.dbf$Acres[urban_selector]              

            # now that housing counts have changed, update all 'Cum' stats for use in next period 
            urban_county_time_selector = urban_county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  urban_county_time_cum$time ==unique(county_time_cum$time)[time_i]
            urban_county_time_cum$Cum[urban_county_time_selector] = (sum(Density.dbf$Cum[urban_selector]) * urban_percent_scaler$par)  
            rural_county_time_selector = rural_county_time_cum$COUNTY == unique(county_time_cum$COUNTY)[county_j] &  rural_county_time_cum$time ==unique(county_time_cum$time)[time_i]
            rural_county_time_cum$Cum[rural_county_time_selector] = (sum(Density.dbf$Cum[rural_selector]) * rural_percent_scaler$par)            
            Density.dbf$Cum[rural_selector] = (Density.dbf$Cum[rural_selector] * rural_percent_scaler$par)
            Density.dbf$Cum[urban_selector] = (Density.dbf$Cum[urban_selector] * urban_percent_scaler$par)
            
            sum(Density.dbf$Cum[rural_selector])+ sum(Density.dbf$Cum[urban_selector]) # total number of houses in scenario for county j time t
            
        }             
    } 
    
    #saver = Density.dbf
    
    namer= names(Density.dbf)
    namer[namer=="Scenario"] =      scenario_name 
    print(paste("The attribute for this scenario is " ,scenario_name,sep="") )
    names(Density.dbf) = namer
    head(Density.dbf)
    Density.dbf = subset(Density.dbf, select=-c(Acres, Cum,ruralin99, COUNTY, HuDac4 ))
    
    Density.dbf2 = read.dbf(paste(pather),as.is=T)   #read dbf file for edit
    Density.dbf2$sort = 1:dim(Density.dbf2)[1]
    Density.dbf  = Density.dbf[Density.dbf$time != 99,]  # no estimate for 99 
    
    Density.dbf[is.na(Density.dbf[, scenario_name ])] = 0 
    
    Density.dbf3 = reshape(Density.dbf,idvar="id", timevar="time", direction="wide" )
    Density.dbf3 = Density.dbf3[ ,c(2,grep(scenario_name , names(Density.dbf3)))]
    Density.dbf2 = merge(Density.dbf2,Density.dbf3, by.x="SUM090" , by.y="SUM090.109")
    Density.dbf2 = Density.dbf2[with(Density.dbf2, order(sort) ), ]                  # resort to polygon order
    
    write.dbf(Density.dbf2,file=paste(pather))    
}



#







