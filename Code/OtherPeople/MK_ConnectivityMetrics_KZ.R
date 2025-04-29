#This script is for plotting some of the basic network connectivity metrics. 
#this is for kz
library(sp)
library(sf)
library(ggmap)
library('raster')
library('rgdal')
library('stats')
library(readr)
library(ggplot2)
library(reshape2)
library(devtools)
library(streamDAG)
library(data.table)
require(scales)
library(zoo)
library(plotly)

#import the data and look at it#####
kz_lengths<- read.csv("H:/Documents/AIMS/data/otherRegions/konza/konza_lengths_all.csv")
kz_coords<- read.csv("H:/Documents/AIMS/data/otherRegions/konza/konza_locations.csv")
kz_nodedata<- read.csv("H:/Documents/AIMS/data/otherRegions/konza/kz_nodedata.csv")
#kz_arc_pres_abs<- read_csv("H:/Documents/AIMS/R/STIC_tables/kz_arc_pres_abs_filled.csv", 
#                          col_types = cols(...1 = col_skip()))

kz_arc_pres_abs<- read_csv("H:/Documents/AIMS/data/otherRegions/konza/kz_arc_presence_abscence_version1_20240220.csv", 
                           col_types = cols(...1 = col_skip()))
kz_arc_pres_abs$datetime<- as.POSIXct(kz_arc_pres_abs$datetime, format="%m/%d/%Y %H:%M", tz="UTC")

#kz_node_pres_abs<- read_csv("H:/Documents/AIMS/R/STIC_tables/kzpresence_abscence_allyrs_filled.csv",  col_types = cols(...1 = col_skip()))
kz_node_pres_abs<- read.csv("H:/Documents/AIMS/data/otherRegions/konza/wetness_table_combined_withconfluences2022.csv")
head(kz_node_pres_abs)

#streamnet<- read_sf("H:/Documents/AIMS/GISdata/AIMs_Mountain_West_Dry_Creek_Site_CON1W/CON1W_Stream_Polyline.shp")

kz_node_pres_abs$datetime<- as.POSIXct(kz_node_pres_abs$datetime, format="%m/%d/%Y %H:%M", tz="UTC")

#limit the time period
#dry down:
#kz_node_pres_abs<- kz_node_pres_abs[kz_node_pres_abs$datetime>"2022-06-11" & kz_node_pres_abs$datetime< "2022-10-01",]
#kz_arc_pres_abs<- kz_arc_pres_abs[kz_arc_pres_abs$datetime>"2022-06-11" & kz_arc_pres_abs$datetime< "2022-10-01",]
#wet up
#kz_node_pres_abs<- kz_node_pres_abs[kz_node_pres_abs$datetime<"2022-06-12" & kz_node_pres_abs$datetime> "2022-02-01",]
#kz_arc_pres_abs<- kz_arc_pres_abs[kz_arc_pres_abs$datetime<"2022-06-12" & kz_arc_pres_abs$datetime> "2022-02-01",]


#****************************#
####Instaneous Flowing Network Extent####
#% of sensors flowing. Network scale through time. 
nodpa<- kz_node_pres_abs
nodpa$inst_flow_net<- (rowSums(nodpa[2:ncol(nodpa)], na.rm = T)/ (rowSums(!is.na(nodpa[,2:ncol(nodpa)])))) * 100 #only includes stics without NA
head(nodpa)
nodpa$datetime<- as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M")

#plot
ggplot(data=nodpa)+
  geom_point(aes(x=datetime, y=inst_flow_net))+
  xlab('Date')+
  ylab('Instantaneous Flowing Network Extent (%)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/instantaneousflowingnetwork_kz.png")

write.csv(nodpa, "H:/Documents/AIMS/R/excel/kz_sticmetrics/inst_flow_net_nodes.csv")

#**********************************************************************#
####Instaneous Flowing Network Extent for N4D####
#% of sensors flowing. Network scale through time. 
nodpa<- kz_node_pres_abs

N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

nodpa<-nodpa[,grep(paste(N4D_stics,collapse="|"), names(nodpa))] #subst just those stics

nodpa$inst_flow_net<- (rowSums(nodpa[2:ncol(nodpa)], na.rm = T)/ (rowSums(!is.na(nodpa[,2:ncol(nodpa)])))) * 100 #only includes stics without NA
head(nodpa)
nodpa$datetime<- as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M")

#plot
ggplot(data=nodpa)+
  geom_point(aes(x=datetime, y=inst_flow_net))+
  geom_line(aes(x=datetime, y=inst_flow_net))+
  xlab('Date')+
  ylab('Instantaneous Flowing Network Extent (%)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/instantaneousflowingnetwork_kz_N4D.png")

write.csv(nodpa, "H:/Documents/AIMS/R/excel/kz_sticmetrics/inst_flow_net_nodes_N4D.csv")


#**********************************************************************#
#*fraction of time a node is wet broken up by year and season and month####
nodpa<- kz_node_pres_abs
colnames(nodpa)[1]<-"datetime"
nodpa$datetime<- as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M", tz='MST')
nodpa$wy<- ifelse(month(nodpa$datetime)>9, year(nodpa$datetime)+1, year(nodpa$datetime))
nodpa$mnth<- month(nodpa$datetime)

#seasonal values
nodpa$season<- ifelse(nodpa$mnth %in% c(12,1,2), "winter",
                      ifelse(nodpa$mnth %in% c(3,4,5), 'spring',
                             ifelse(nodpa$mnth %in% c(6,7,8), 'summer',
                                    ifelse(nodpa$mnth %in% c(9,10,11), 'fall', NA))))

#aggregate
annual_agg<- aggregate(nodpa[,c(3:ncol(kz_node_pres_abs))],by=list(nodpa$wy), sum, na.rm=T) #annual
annual_tot<- aggregate(nodpa[,c(3:ncol(kz_node_pres_abs))],by=list(nodpa$wy), function(x) sum(!is.na(x))) #annual potential
mnthly_agg<- aggregate(nodpa[,c(3:ncol(kz_node_pres_abs))], by=list(nodpa$wy, nodpa$mnth), sum, na.rm=T) #monthly potential
mnthly_tot<- aggregate(nodpa[,c(3:ncol(kz_node_pres_abs))],by=list(nodpa$wy, nodpa$mnth), function(x) sum(!is.na(x))) #monthly potential
seasonal_agg<- aggregate(nodpa[,c(3:ncol(kz_node_pres_abs))], by=list(nodpa$wy, nodpa$season), sum, na.rm=T) #monthly potential
seasonal_tot<- aggregate(nodpa[,c(3:ncol(kz_node_pres_abs))],by=list(nodpa$wy, nodpa$season), function(x) sum(!is.na(x))) #monthly potential

#annual node permanence####
prob_node_annual<- annual_agg[,(2:ncol(annual_agg))]/annual_tot[,c(2:ncol(annual_tot))]
prob_node_annual$wy<- annual_agg$Group.1
prob_node_annual_m<- melt(prob_node_annual, id.vars="wy")
prob_node_annual2<- dcast(variable~wy,data=prob_node_annual_m, value.var="value")
prob_node_annual2$diff22_23<- prob_node_annual2$`2022` - prob_node_annual2$`2023`

ggplot(data=prob_node_annual_m)+
  geom_bar(aes(x=variable, y=value, fill=as.character(wy)),stat='identity', position = 'dodge')

ggplot(data=prob_node_annual2)+
  geom_bar(aes(x=variable, y=diff22_23),stat='identity', position = 'dodge')

write.csv(prob_node_annual_m, "H:/Documents/AIMS/R/excel/kz_sticmetrics/percenttimenodeflows_annualwy_wetup2022.csv")

#season node permanence####
prob_node_season<- seasonal_agg[,(3:ncol(seasonal_agg))]/seasonal_tot[,c(3:ncol(seasonal_tot))]
head(prob_node_season)
prob_node_season$wy<- seasonal_agg$Group.1
prob_node_season$season<- seasonal_agg$Group.2
prob_node_season_m<- melt(prob_node_season, id.vars=c("wy","season"))

ggplot(data=prob_node_season_m)+
  geom_bar(aes(x=variable, y=value, fill=as.character(wy)),stat='identity', position = 'dodge')+
  facet_grid(season~.)

write.csv(prob_node_season_m, "H:/Documents/AIMS/R/excel/kz_sticmetrics/percenttimenodeflows_wetupwy.csv")

#mnthly node permanence####
prob_node_mnthly<- mnthly_agg[,(3:ncol(mnthly_agg))]/mnthly_tot[,c(3:ncol(mnthly_tot))]
head(prob_node_mnthly)
prob_node_mnthly$wy<- mnthly_agg$Group.1
prob_node_mnthly$mnthly<- mnthly_agg$Group.2
prob_node_mnthly_m<- melt(prob_node_mnthly, id.vars=c("wy","mnthly"))

ggplot(data=prob_node_mnthly_m)+
  geom_bar(aes(x=variable, y=value, fill=as.character(wy)),stat='identity', position = 'dodge')+
  facet_grid(mnthly~.)

write.csv(prob_node_mnthly_m, "H:/Documents/AIMS/R/excel/kz_sticmetrics/percenttimenodeflows_mnthlywywetup.csv")

#****************************#
#First day of climate year no flow####
nodpa<- kz_node_pres_abs
nodpa$datetime<-as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M", tz="MST")
#limit by year
nodpa<- nodpa[nodpa$datetime>as.POSIXct("2/1/2022", format="%m/%d/%Y") & nodpa$datetime< as.POSIXct("10/1/2022", format="%m/%d/%Y"),] #only works for 2022...
kz_nod_m<- melt(nodpa, id.vars=c('datetime'))
#kz_nod_m<- melt(kz_node_pres_abs, id.vars=c('datetime'))

kz_nod_m_noflow<- kz_nod_m[kz_nod_m$value == 0,]
kz_nod_m_noflow$datetime<- as.POSIXct(kz_nod_m_noflow$datetime, format="%m/%d/%Y %H:%M")
firstdaynoflow<- aggregate(kz_nod_m_noflow$datetime, by=list(kz_nod_m_noflow$variable), min)
firstdaynoflow

write.csv(firstdaynoflow, "H:/Documents/AIMS/R/excel/kz_sticmetrics/firstdaynoflow2022.csv")

#plot
coords_noflow<- merge(kz_coords, firstdaynoflow, by.x='SiteID', by.y='Group.1')
coords_noflow$kzate<- as.numeric(format(as.Date(coords_noflow$x), "%j"))
noflow_points<- st_as_sf(coords_noflow ,coords=c("E", "N"), crs= crs(streamnet))

ggplot()+
  geom_sf(data= streamnet, aes(geometry=geometry), color = 'black')+
  geom_sf(data= noflow_points, aes(geometry=geometry, col=kzate), cex=5)+
  scale_color_gradient(low="yellow", high="blue",  name = "First day Dry (Julian Day)")+
  ggtitle('First Day No Flow')+
  theme_classic()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/FirstDayNoFlow.png")

#rolling mean to determine the first period of no flow
kz_node_pres_abs$datetime<- as.POSIXct(kz_node_pres_abs$datetime, format="%m/%d/%Y %H:%M")
kz_node_pres_abs_2022<- kz_node_pres_abs[kz_node_pres_abs$datetime< "2022-06-11",]
kz_node_pres_abs_2022$dt<- as.Date(kz_node_pres_abs_2022$datetime)
kz_node_pres_abs_2022_day<- aggregate(kz_node_pres_abs_2022, by=list(kz_node_pres_abs_2022$dt), max)
kz_node_pres_abs_roll<- rollapply(kz_node_pres_abs_2022_day[,3:(ncol(kz_node_pres_abs_2022_day)-1)], 1, mean, by.column=TRUE, na.action=na.omit, align="left")

kz_node_pres_abs_roll<- cbind(kz_node_pres_abs_roll, kz_node_pres_abs_2022_day[1:131,] ) #just change to match...

kz_nod_m<- melt(kz_node_pres_abs_2022_day[,3:ncol(kz_node_pres_abs_2022_day)], id.vars=c('dt'))

kz_nod_m_noflow<- kz_nod_m[kz_nod_m$value == 0,] #first day where it is 0/dry
kz_nod_m_noflow<- kz_nod_m[kz_nod_m$value == 1,] #first day where it is 1/wet
#kz_nod_m_noflow<- kz_nod_m_noflow[kz_nod_m_noflow$dt>as.Date("2022-05-15"),] #we want the dry down not the winter dry period date
firstdaynoflow<- aggregate(kz_nod_m_noflow$dt, by=list(kz_nod_m_noflow$variable), min)
firstdaynoflow


write.csv(firstdaynoflow, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kzfirtsdayrewet_for1FULLdays_2022.csv")




#****************************#
#Active Stream network density ####
#stream reach length divided by drainage area
#kz drainage area: 17 km2

#kz_arc_pres_abs<- cbind(kz_node_pres_abs$datetime, kz_arc_pres_abs) #add the date columb to the arc dataframe
#kz_arc_pres_abs_df<-cbind(index_val=rownames(as.data.frame(kz_arc_pres_abs)), as.data.frame(kz_arc_pres_abs))
#colnames(kz_arc_pres_abs_df)[2]<-"datetime"
kz_arc_pres_abs_df<- kz_arc_pres_abs
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format="%m/%d/%Y %H:%M")
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kzarcs_lengths<- merge(kz_arc_pres_abs_df, kz_lengths[,1:2], by.x="variable", by.y="Arcs")
head(kzarcs_lengths)

kzarcs_lengths$value<-as.numeric(kzarcs_lengths$value)
kzarcs_lengths$Lengths<-as.numeric(kzarcs_lengths$Lengths)
kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step
kz_net_length_time$Group.1<- as.POSIXct(kz_net_length_time$Group.1, format= "%m/%d/%Y %H:%M")

kz_net_length_time$asdn<- (kz_net_length_time$x) / 5.4 #active stream density in km/km2

ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=asdn))+
  xlab('Date')+
  ylab('Active Stream Network Density (km/km2)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/activestreamnetworkdensity.png")

write.csv(kz_net_length_time, "H:/Documents/AIMS/R/excel/kz_sticmetrics/activestreamnetworkdensity2022.csv")

#the proportion of the total network length
networklength<- sum(unique(kzarcs_lengths$Lengths))
#create a column for the total potential length in a timestep
kzarcs_lengths$ones<- ifelse(is.na(kzarcs_lengths$value),NA,1)
kzarcs_lengths$length_time<- kzarcs_lengths$ones * kzarcs_lengths$Lengths
totlength_time<- aggregate(kzarcs_lengths[,7], by=list(kzarcs_lengths$datetime), sum, na.rm=T) #create a total length frame
colnames(totlength_time)<-c("dt", 'totlength_km'); totlength_time$dt<-as.POSIXct(totlength_time$dt, format="%m/%d/%Y %H:%M")
kz_net_length_time<- merge(kz_net_length_time, totlength_time, by.x='Group.1', by.y="dt") #merge to add that column

kz_net_length_time$network_flowing<- (kz_net_length_time$x) / kz_net_length_time$totlength_km

p<- ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=network_flowing))+
  xlab('Date')+
  ylab('Stream permanence')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/proportion_network_flowing.png")
write.csv(kz_net_length_time, "H:/Documents/AIMS/R/excel/kz_sticmetrics/activestreamnetworkprop.csv")

ggplotly(p)


ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=network_flowing))+
  geom_line(aes(x=Group.1, y=asdn))+
  xlab('Date')+
  ylab('Stream permanence')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#asdn and proportion total length with only N4D####
kz_arc_pres_abs_df<- kz_arc_pres_abs
N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N4D_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)

kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format="%m/%d/%Y %H:%M")
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kzarcs_lengths<- merge(kz_arc_pres_abs_df, kz_lengths[,1:2], by.x="variable", by.y="Arcs")
head(kzarcs_lengths)

kzarcs_lengths$value<-as.numeric(kzarcs_lengths$value)
kzarcs_lengths$Lengths<-as.numeric(kzarcs_lengths$Lengths)
kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step
kz_net_length_time$Group.1<- as.POSIXct(kz_net_length_time$Group.1, format= "%m/%d/%Y %H:%M")

kz_net_length_time$asdn<- (kz_net_length_time$x) / 1.17 #active stream density in km/km2

ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=asdn))+
  xlab('Date')+
  ylab('Active Stream Network Density (km/km2)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/activestreamnetworkdensityN4D.png")

write.csv(kz_net_length_time, "H:/Documents/AIMS/R/excel/kz_sticmetrics/activestreamnetworkdensityN4D.csv")

#the proportion of the total network length
#networklength<- sum(kz_lengths$Lengths)
networklength<- sum(unique(kzarcs_lengths[,c(1,4)]$Lengths))
networklength
kz_net_length_time$network_flowing<- (kz_net_length_time$x) / networklength

p<- ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=network_flowing))+
  xlab('Date')+
  ylab('Stream permanence')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/proportion_network_flowingN4D.png")
write.csv(kz_net_length_time, "H:/Documents/AIMS/R/excel/kz_sticmetrics/activestreamnetworkpropN4D.csv")

ggplotly(p)


ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=network_flowing))+
  geom_line(aes(x=Group.1, y=asdn))+
  xlab('Date')+
  ylab('Stream permanence')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#*****************************#

#kz duration curve######
cdf_kzarcs<- ecdf(kz_sticnet$x)
kz_sticnet$ecdf_prob_network<- cdf_kzarcs(kz_sticnet$x)


#the cdf
ggplot(data= kz_sticnet)+
  geom_line(aes(y=x, x=ecdf_prob_network))+
  geom_point(aes(y=x, x=ecdf_prob_network))+
  ylab("Length (km)")+
  xlab("CDF")+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/metrics/kz_activelength_cdf2.png")

#the ecdf
ggplot(data= kz_sticnet)+
  geom_line(aes(x=x, y=1-ecdf_prob_network))+
  geom_point(aes(x=x, y=1-ecdf_prob_network))+
  xlab("Length (km)")+
  ylab("Exceedence probability of active stream length")+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/metrics/kz_activelength_exceedence2.png")


#****************************#
#*loop- active stream network density####
#****************************#
#*
kz_arc_pres_abs_df<- kz_arc_pres_abs
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge with the lengths
kzarcs_lengths<- merge(kz_arc_pres_abs_df_daily, kz_lengths, by.x="variable", by.y="Arcs")
unique(kzarcs_lengths$variable)

kzarcs_lengths$value<-as.numeric(kzarcs_lengths$value)
kzarcs_lengths$Lengths<-as.numeric(kzarcs_lengths$Lengths)
kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum, na.rm=T) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step
kz_net_length_time$Group.1<- as.POSIXct(kz_net_length_time$Group.1, format= "%m/%d/%Y %H:%M")

kz_net_length_time$asdn<- (kz_net_length_time$x) / 5.4 #active stream density in km/km2
head(kz_net_length_time)


#rest for below
kz_arc_pres_abs_df<- kz_arc_pres_abs
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge the longer data with the other stuff and create the appropriate headers
kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,14,18,17)]
colnames(kzarcs_lengths_2)[6] <- "wetdry_bin"
head(kzarcs_lengths_2)
unique(kzarcs_lengths_2$fromto)
unique(kz_lengths$Arcs)

#kzarcs_lengths_2$datetime<- as.POSIXct(kzarcs_lengths_2$datetime, format= "%m/%d/%Y %H:%M")
#kzarcs_lengths_2[duplicated(kzarcs_lengths_2),]

#*******************************************#
####loop- Active Stream network density###
gnd<- kzarcs_lengths_2
n<- unique(gnd$from) #all except  the sink. can't remove
n
active_lengthdf<- NULL

for (i in 1:length(n)) {
  node_rem<- n[i]
  gnd<- kzarcs_lengths_2
  newtonode <- gnd$to[gnd$from == node_rem] #identify the new node
  additionalreach <- gnd$reach_length[gnd$from == node_rem] #identify the new reach length
  from_wetdry_l<- gnd$reach_length[gnd$from == node_rem] #identify wet/dry
  from_wetdry_l
  to_wetdry_l<- gnd$reach_length[gnd$to == node_rem]
  to_wetdry_l
  #If there are two reaches going to the node (confluence) then don't 
  #change the reaches wet dry status. If it is only one reach, replace the wet dry with the longest reach 
  
  # if (length(to_wetdry_l)==1) {
  #   if (from_wetdry_l > to_wetdry_l) {
  #     gnd$wetdry_bin[gnd$to == node_rem] = gnd$wetdry_bin[gnd$from == node_rem]
  #   }
  #   else{
  #     gnd$wetdry_bin[gnd$to == node_rem] = gnd$wetdry_bin[gnd$to == node_rem]
  #   }
  # }
  
  #determine the wet dry status of the new arc if it is at the end of the reach. Take a value of 0.5 if one is 0 and the other is 1
  #keep the reach, but we change the reach value to match the to node
  if (length(unique(to_wetdry_l))==0) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      gnd_sub$wetdry_bin[gnd_sub$from == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == unique(newtonode)]
    }
    #remerge into the other file
    gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    
    new_gnd<-gnd
  }
  
  #determine the wet dry status of the new arc. Take a value of 0.5 if one is 0 and the other is 1
  if (length(unique(to_wetdry_l))==1) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem]) & is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = NA
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem] )) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$to == node_rem]
      } else if (gnd_sub$wetdry_bin[gnd_sub$to == node_rem] == gnd_sub$wetdry_bin[gnd_sub$from == node_rem]) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else{
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = 0.5
      }
      #remerge into the other file
      gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    }
    #change the network only for the reaches in the middle not the top. We want to preserve the max network length
    gnd$wetdry_bin[gnd$to == node_rem]
    gnd$wetdry_bin[gnd$from == node_rem]
    
    gnd$wetdry_bin[gnd$to == node_rem]
    
    new_gnd<- gnd[gnd$from != node_rem,]  #drop the unnecessary node/reach 
    #add the additional reach length to the wet one, or the first. 
    toindex<- new_gnd$wetdry_bin[new_gnd$to == node_rem]
    fromindex<- new_gnd$wetdry_bin[new_gnd$from == node_rem]
    newlength <- unique(from_wetdry_l) + unique(to_wetdry_l)
    
    new_gnd$reach_length[new_gnd$to == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$to == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$to == node_rem])
    new_gnd$reach_length[new_gnd$to == node_rem]
    new_gnd$reach_length[new_gnd$from == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$from == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$from == node_rem])
    new_gnd$reach_length[new_gnd$from == node_rem]
    new_gnd[new_gnd$to == node_rem,]
    
    new_gnd$to[new_gnd$to == node_rem] = newtonode #change to the new node
    new_gnd[new_gnd$to == newtonode,]
    
  }
  
  
  #calculate active stream network length
  new_gnd$length_value<- new_gnd$wetdry_bin * new_gnd$reach_length
  new_activelength<- aggregate(new_gnd$length_value, by=list(new_gnd$datetime), sum, na.rm =T)
  
  new_activelength$asdn<- (new_activelength$x) / 5.4 #active stream density in km/km2
  new_data<- cbind(node_rem, newtonode, new_activelength)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}

active_lengthdf<- as.data.frame(active_lengthdf)
colnames(active_lengthdf)<- c('node_rem', 'newtonode', 'Group.1', 'newactive_length', 'new_asdn')
all_active_lengthdf<- merge(active_lengthdf, kz_net_length_time, by='Group.1')
all_active_lengthdf$diff_from_real<- round(as.numeric(all_active_lengthdf$new_asdn), 7) - round(all_active_lengthdf$asdn,7) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_streamlength<- round(as.numeric(all_active_lengthdf$newactive_length), 7) - round(all_active_lengthdf$x,7) #round or else they both are off at the 18th decimal
head(all_active_lengthdf)

#calcualte the slope of the line between the two 
#all the data
summary(lm(new_asdn~asdn, data=all_active_lengthdf)) #slope of0.99. The y value changes 0.99 when the x changes by 1 unit

slope_newold_asdn<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_asdn') ,
                   list(slope = ~lm(new_asdn ~ asdn)$coef[2])))

summary(lm(newactive_length~x, data=all_active_lengthdf)) #slope of 1.1. The y value changes 1.1 when the x changes by 1 unit

#same slopes as asdn since the asdn is essentially just a scaled version
slope_newold_activelength<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('newactive_length') ,
                   list(slope = ~lm(newactive_length~x)$coef[2])))

all_active_lengthdf$percent_diff<- (all_active_lengthdf$new_asdn / all_active_lengthdf$asdn)* 100
med_diff<-  aggregate(all_active_lengthdf$percent_diff, by=list(all_active_lengthdf$Group.1), median, na.rm=T)

#write.csv(active_lengthdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_asdn_remove1_allyrs.csv")
#write.csv(slope_newold_asdn, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_asdn_remove1_slopes_allyrs.csv")


ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=diff_from_real, color=node_rem))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_diff_fromactual_drydown2022.png")

ggplot()+
  geom_line(data=all_active_lengthdf, aes(x=Group.1, y=percent_diff-100, color=node_rem))+
  geom_point(data=all_active_lengthdf, aes(x=Group.1, y=percent_diff-100, color=node_rem))+
  geom_line(data=med_diff, aes(x=Group.1, y=x-100), color ='black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_diff_fromactual_percent_drydown2022.png")



ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=new_asdn, color=node_rem))+
  geom_line(aes(x=Group.1, y=asdn), color = 'black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_timeseries_drydown2022.png")

ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=newactive_length, color=node_rem))+
  geom_line(aes(x=Group.1, y=x), color = 'black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_timeseries_activelength_drydown2022.png")

ggplot(data= all_active_lengthdf)+
  geom_boxplot(aes(x= node_rem, y= percent_diff))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_boxplot_percentdiff_drydown2022.png")


ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=asdn, y=new_asdn, color=node_rem))+
  geom_smooth(method ='lm', aes(x=asdn, y=new_asdn), color = 'black')+
  geom_abline(slope=1, intercept=0, color = 'black', linetype='dashed', lwd=1)+
  theme_classic()
  

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_vs_new.png")

ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=asdn, y=new_asdn, color=node_rem))+
  geom_smooth(method ='lm', aes(x=asdn, y=new_asdn, color = node_rem), se = F)+
  geom_abline(slope=1, intercept=0, color = 'black', linetype='dashed', lwd=1)+
  theme_classic()

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_vs_new_trendlines.png")

#asdn vs percent difference
ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=asdn, y=percent_diff-100, color=node_rem))+
  geom_smooth(method ='lm', aes(x=asdn, y=percent_diff-100, color = node_rem), se = F)+
  geom_hline(yintercept=0, color = 'black', linetype='dashed', lwd=1)+
  theme_classic()

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_percendiff_asdn.png")

#the above into facetgrid...
ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=asdn, y=percent_diff-100))+
  geom_smooth(method ='lm', aes(x=asdn, y=percent_diff-100), se = F)+
  geom_hline(yintercept=0, color = 'black', linetype='dashed', lwd=1)+
  facet_wrap(node_rem~., scales = 'free')+
  theme_bw()

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_diff_fromactual_percent.png")

#facet grid with the lengths
ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=x, y=percent_diff-100))+
  geom_smooth(method ='lm', aes(x=x, y=percent_diff-100), se = F)+
  geom_hline(yintercept=0, color = 'black', linetype='dashed', lwd=1)+
  facet_wrap(node_rem~., scales = 'free')+
  theme_bw()

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_activelength_diff_fromactual_percent.png")

ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=x, y=newactive_length, color=node_rem))+
  geom_smooth(method ='lm', aes(x=x, y=newactive_length, color = node_rem), se = F)+
  geom_abline(slope=1, intercept=0, color = 'black', linetype='dashed', lwd=1)+
  theme_classic()


ggplot()+
  geom_col(data=slope_newold_asdn, aes(x=node_rem, y= new_asdn_slope-1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_slopes_minus1_drydown2022.png")


#*****************************************#
#*Loop through the Stream Length Duration Curve and remove one####
#
kz_arc_pres_abs_df<- kz_arc_pres_abs
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#calculate the data to compare the leave one out stuff to
kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths #this will change the length value to 0 if dry but return the length of the reach if wet
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum, na.rm=T) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step

kz_net_length_time$yr<- year(kz_net_length_time$Group.1)
kz_net_length_time$mnth<- month(kz_net_length_time$Group.1)
kz_net_length_time$wy<- ifelse(kz_net_length_time$mnth>9, kz_net_length_time$yr + 1, kz_net_length_time$yr)

cdf_kzarcs<- ecdf(kz_net_length_time$x) # the data to compare to 
kz_net_length_time$ecdf_prob<- cdf_kzarcs(kz_net_length_time$x)

#rest for below
kz_arc_pres_abs_df<- kz_arc_pres_abs
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge the longer data with the other stuff and create the appropriate headers
kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,14,18,17)]
colnames(kzarcs_lengths_2)[6] <- "wetdry_bin"

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(gnd$from) #all except  the sink. can't remove
n
active_lengthdf<- NULL

for (i in 1:length(n)) {
  node_rem<- n[i]
  gnd<- kzarcs_lengths_2
  newtonode <- gnd$to[gnd$from == node_rem] #identify the new node
  additionalreach <- gnd$reach_length[gnd$from == node_rem] #identify the new reach length
  from_wetdry_l<- gnd$reach_length[gnd$from == node_rem] #identify wet/dry
  from_wetdry_l
  to_wetdry_l<- gnd$reach_length[gnd$to == node_rem]
  to_wetdry_l
  
  #determine the wet dry status of the new arc if it is at the end of the reach. Take a value of 0.5 if one is 0 and the other is 1
  #keep the reach, but we change the reach value to match the to node
  if (length(unique(to_wetdry_l))==0) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      gnd_sub$wetdry_bin[gnd_sub$from == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == unique(newtonode)]
    }
    #remerge into the other file
    gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    
    new_gnd<-gnd
  }
  
  #determine the wet dry status of the new arc. Take a value of 0.5 if one is 0 and the other is 1
  if (length(unique(to_wetdry_l))==1) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem]) & is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = NA
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem] )) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$to == node_rem]
      } else if (gnd_sub$wetdry_bin[gnd_sub$to == node_rem] == gnd_sub$wetdry_bin[gnd_sub$from == node_rem]) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else{
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = 0.5
      }
      #remerge into the other file
      gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    }
    gnd$wetdry_bin[gnd$to == node_rem]
    gnd$wetdry_bin[gnd$from == node_rem]
    
    gnd$wetdry_bin[gnd$to == node_rem]
    
    new_gnd<- gnd[gnd$from != node_rem,]  #drop the unnecessary node/reach 
    
    #add the additional reach length to the wet one, or the first. 
    toindex<- new_gnd$wetdry_bin[new_gnd$to == node_rem]
    fromindex<- new_gnd$wetdry_bin[new_gnd$from == node_rem]
    
    newlength <- unique(from_wetdry_l) + unique(to_wetdry_l)
    
    new_gnd$reach_length[new_gnd$to == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$to == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$to == node_rem])
    new_gnd$reach_length[new_gnd$to == node_rem]
    new_gnd$reach_length[new_gnd$from == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$from == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$from == node_rem])
    new_gnd$reach_length[new_gnd$from == node_rem]
    new_gnd[new_gnd$to == node_rem,]
    
    new_gnd$to[new_gnd$to == node_rem] = newtonode #change to the new node
    new_gnd[new_gnd$to == newtonode,]
  }
  
  #calculate stream length duration curve
  new_gnd$length_value<- new_gnd$wetdry_bin * new_gnd$reach_length
  new_activelength<- aggregate(new_gnd$length_value, by=list(new_gnd$datetime), sum, na.rm =T)
  
  new_activelength$yr<- year(new_activelength$Group.1)
  new_activelength$mnth<- month(new_activelength$Group.1)
  new_activelength$wy<- ifelse(new_activelength$mnth>9, new_activelength$yr + 1, new_activelength$yr)
  cdf_gjarcs<- ecdf(new_activelength$x)
  new_activelength$ecdf_prob<- cdf_gjarcs(new_activelength$x)
  
  new_data<- cbind(node_rem, newtonode, new_activelength)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}


sum(unique(kzarcs_lengths$Lengths))
sum(unique(new_gnd$reach_length)) #why is this greater...? shouldn't be. 
unique(new_gnd$reach_length)
unique(kzarcs_lengths$Lengths)
unique(kzarcs_lengths$variable)
unique(new_gnd$fromto)

active_lengthdf<- as.data.frame(active_lengthdf)
colnames(active_lengthdf)[8]<- c('new_ecdf')
all_active_lengthdf<- merge(active_lengthdf, kz_net_length_time, by='Group.1')
all_active_lengthdf$diff_from_real<- round(as.numeric(all_active_lengthdf$new_ecdf), 7) - round(all_active_lengthdf$ecdf_prob,7) #round or else they both are off at the 18th decimal
head(all_active_lengthdf)

#calculate the percent change
all_active_lengthdf$percent_diff<- (all_active_lengthdf$new_ecdf/ all_active_lengthdf$ecdf_prob)*100
# #merge by active length instead
# all_active_lengthdf2<- merge(all_active_lengthdf, kz_net_length_time[,c(2,6)], by.x= 'x.x', by.y = 'x', all=T)
# all_active_lengthdf2$diff2 <- (all_active_lengthdf2$new_ecdf / all_active_lengthdf2$ecdf_prob.y)*100
# all_active_lengthdf2$diff_slkz <- ((1-all_active_lengthdf2$new_ecdf) / (1-all_active_lengthdf2$ecdf_prob.y))*100
# t<- all_active_lengthdf2[all_active_lengthdf2$node_rem %in% c('kz17'),]

#calculate the median value of the percent change
med_diff<-  aggregate(all_active_lengthdf$percent_diff, by=list(all_active_lengthdf$Group.1), median, na.rm=T)

#calcualte the slope of the line between the two 

slope_newold_ecdf<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_ecdf') ,
                   list(slope = ~lm(new_ecdf ~ ecdf_prob)$coef[2])))


#calculate the slope of the sldc curves
slope_sldc_new<- all_active_lengthdf[!is.na(all_active_lengthdf$new_ecdf),] %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_ecdf') ,
                   list(slope = ~lm(new_ecdf ~ x.x)$coef[2])))

slope_sldc_new$slope_orig <- summary(lm(ecdf_prob~x.y, all_active_lengthdf))$coef[2]
slope_sldc_new$slope_change<-slope_sldc_new$slope_orig - slope_sldc_new$new_ecdf_slope

##plot of sldc changes####
#plot cdf##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= ecdf_prob))+
  xlab('Active Stream Length (km)')+
  ylab('CDF')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_ecdf_allsite_drydown2022.png")

#plot sldc##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_point(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= 1-ecdf_prob))+
  geom_point(aes(x=x.y, y= 1-ecdf_prob))+
  xlab('Active Stream Length (m)')+
  ylab('duration')+
  theme_bw()+
  facet_wrap(node_rem~.)

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_sldc_allsite_facet_drydown2022.png")

#plot sldc##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_point(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= 1-ecdf_prob))+
  geom_point(aes(x=x.y, y= 1-ecdf_prob))+
  xlab('Active Stream Length (km)')+
  ylab('duration')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_slkz_allsite_drydown2022.png")

#write.csv(active_lengthdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_ecdf_prob_remove1_allyrs.csv")
#write.csv(slope_newold_ecdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_ecdf_remove1_slopes_allyrs.csv")

ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=diff_from_real, color=node_rem))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_ecdf_prob_diff_fromactual.png")


#activelength percent diff facet plot difference
ggplot()+
  geom_point(data=all_active_lengthdf, aes(x=x.x, y=percent_diff-100, color=node_rem))+
  facet_wrap(node_rem~., scales = 'free')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_sldc_prob_diff_fromactual_percent_time.png", height=15,width=15)

#new and old through time 
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=new_ecdf, color=node_rem))+
  geom_line(aes(x=Group.1, y=ecdf_prob), color = 'black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_ecdf_prob_timeseries_drydown2022.png")


#node removed with the differnce matched to the stream length for the SLDC
ggplot(data= all_active_lengthdf)+
  geom_boxplot(aes(x= node_rem, y= percent_diff-100))+
  ylab('SLDC difference')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ylim(-25, 150)

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_slkz_prob_boxplot_percentdiff_drydown2022.png")


#boxplots of the slkz values (durations)
ggplot(data= all_active_lengthdf)+
  geom_boxplot(aes(x= node_rem, y= 1-new_ecdf))+
  ylab('SLDC')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#active length through time
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=x.x, color=node_rem))+
  theme_classic()+
  ylab("length (km)")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_activelength_time_drydown2022.png")

ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=ecdf_prob, y=percent_diff-100, color=node_rem))+
  geom_smooth(method ='lm', aes(x=ecdf_prob, y=percent_diff-100, color = node_rem), se = F)+
  geom_hline(yintercept=0, color = 'black', linetype='dashed', lwd=1)+
  theme_classic()

#ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_ecdf_prob_percendiff_ecdf_prob.png")

#the above into facetgrid... the cdf probability vs percent difference
ggplot(data=all_active_lengthdf)+
  geom_point(aes(x=1-ecdf_prob, y=percent_diff-100))+
  geom_smooth(method ='lm', aes(x=ecdf_prob, y=percent_diff-100), se = F)+
  geom_hline(yintercept=0, color = 'black', linetype='dashed', lwd=1)+
  facet_wrap(node_rem~., scales = 'free')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_slkz_prob_diff_fromactual_percent.png")

#probabilities of each other
ggplot()+
  geom_col(data=slope_newold_ecdf, aes(x=node_rem, y= new_ecdf_slope-1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_ecdf_changeinslope_drydown2022.png")

ggplot()+
  geom_col(data=slope_sldc_new, aes(x=node_rem, y= slope_change))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_ecdf_changeinslopeofsldc_drydown2022.png")

#*********************************************#
#**********************************************#
#*loop- active stream network density with N4D####
#****************************#
#*
kz_arc_pres_abs_df<- kz_arc_pres_abs
N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N4D_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge with the lengths
kzarcs_lengths<- merge(kz_arc_pres_abs_df_daily, kz_lengths, by.x="variable", by.y="Arcs")
unique(kzarcs_lengths$variable)

kzarcs_lengths$value<-as.numeric(kzarcs_lengths$value)
kzarcs_lengths$Lengths<-as.numeric(kzarcs_lengths$Lengths)
kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum, na.rm=T) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step
kz_net_length_time$Group.1<- as.POSIXct(kz_net_length_time$Group.1, format= "%m/%d/%Y %H:%M")

kz_net_length_time$asdn<- (kz_net_length_time$x) / 1.17 #active stream density in km/km2
head(kz_net_length_time)
#plot(x=kz_net_length_time$Group.1, y=kz_net_length_time$asdn, type="line")
#rest for below
kz_arc_pres_abs_df<- kz_arc_pres_abs
kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N4D_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge the longer data with the other stuff and create the appropriate headers
kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,14,18,17)]
colnames(kzarcs_lengths_2)[6] <- "wetdry_bin"
head(kzarcs_lengths_2)
unique(kzarcs_lengths_2$fromto)
#unique(kz_lengths$Arcs)

#kzarcs_lengths_2$datetime<- as.POSIXct(kzarcs_lengths_2$datetime, format= "%m/%d/%Y %H:%M")
#kzarcs_lengths_2[duplicated(kzarcs_lengths_2),]

#*******************************************#
###loop- Active Stream network density##
gnd<- kzarcs_lengths_2
n<- unique(gnd$from) #all except  the sink. can't remove
n
active_lengthdf<- NULL

for (i in 1:length(n)) {
  node_rem<- n[i]
  gnd<- kzarcs_lengths_2
  newtonode <- gnd$to[gnd$from == node_rem] #identify the new node
  additionalreach <- gnd$reach_length[gnd$from == node_rem] #identify the new reach length
  from_wetdry_l<- gnd$reach_length[gnd$from == node_rem] #identify wet/dry
  from_wetdry_l
  to_wetdry_l<- gnd$reach_length[gnd$to == node_rem]
  to_wetdry_l
  #If there are two reaches going to the node (confluence) then don't 
  #change the reaches wet dry status. If it is only one reach, replace the wet dry with the longest reach 
  
  # if (length(to_wetdry_l)==1) {
  #   if (from_wetdry_l > to_wetdry_l) {
  #     gnd$wetdry_bin[gnd$to == node_rem] = gnd$wetdry_bin[gnd$from == node_rem]
  #   }
  #   else{
  #     gnd$wetdry_bin[gnd$to == node_rem] = gnd$wetdry_bin[gnd$to == node_rem]
  #   }
  # }
  
  #determine the wet dry status of the new arc if it is at the end of the reach. Take a value of 0.5 if one is 0 and the other is 1
  #keep the reach, but we change the reach value to match the to node
  if (length(unique(to_wetdry_l))==0) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      gnd_sub$wetdry_bin[gnd_sub$from == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == unique(newtonode)]
    }
    #remerge into the other file
    gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    
    new_gnd<-gnd
  }
  
  #determine the wet dry status of the new arc. Take a value of 0.5 if one is 0 and the other is 1
  if (length(unique(to_wetdry_l))==1) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem]) & is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = NA
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem] )) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$to == node_rem]
      } else if (gnd_sub$wetdry_bin[gnd_sub$to == node_rem] == gnd_sub$wetdry_bin[gnd_sub$from == node_rem]) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else{
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = 0.5
      }
      #remerge into the other file
      gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    }
    #change the network only for the reaches in the middle not the top. We want to preserve the max network length
    gnd$wetdry_bin[gnd$to == node_rem]
    gnd$wetdry_bin[gnd$from == node_rem]
    
    gnd$wetdry_bin[gnd$to == node_rem]
    
    new_gnd<- gnd[gnd$from != node_rem,]  #drop the unnecessary node/reach 
    #add the additional reach length to the wet one, or the first. 
    toindex<- new_gnd$wetdry_bin[new_gnd$to == node_rem]
    fromindex<- new_gnd$wetdry_bin[new_gnd$from == node_rem]
    newlength <- unique(from_wetdry_l) + unique(to_wetdry_l)
    
    new_gnd$reach_length[new_gnd$to == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$to == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$to == node_rem])
    new_gnd$reach_length[new_gnd$to == node_rem]
    new_gnd$reach_length[new_gnd$from == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$from == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$from == node_rem])
    new_gnd$reach_length[new_gnd$from == node_rem]
    new_gnd[new_gnd$to == node_rem,]
    
    new_gnd$to[new_gnd$to == node_rem] = newtonode #change to the new node
    new_gnd[new_gnd$to == newtonode,]
    
  }
  
  
  #calculate active stream network length
  new_gnd$length_value<- new_gnd$wetdry_bin * new_gnd$reach_length
  new_activelength<- aggregate(new_gnd$length_value, by=list(new_gnd$datetime), sum, na.rm =T)
  
  new_activelength$asdn<- (new_activelength$x) / 1.17 #active stream density in km/km2
  new_data<- cbind(node_rem, newtonode, new_activelength)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}

active_lengthdf<- as.data.frame(active_lengthdf)
colnames(active_lengthdf)<- c('node_rem', 'newtonode', 'Group.1', 'newactive_length', 'new_asdn')
all_active_lengthdf<- merge(active_lengthdf, kz_net_length_time, by='Group.1')
all_active_lengthdf$diff_from_real<- round(as.numeric(all_active_lengthdf$new_asdn), 7) - round(all_active_lengthdf$asdn,7) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_streamlength<- round(as.numeric(all_active_lengthdf$newactive_length), 7) - round(all_active_lengthdf$x,7) #round or else they both are off at the 18th decimal
head(all_active_lengthdf)

#calcualte the slope of the line between the two 
#all the data
summary(lm(new_asdn~asdn, data=all_active_lengthdf)) #slope of0.99. The y value changes 0.99 when the x changes by 1 unit

slope_newold_asdn<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_asdn') ,
                   list(slope = ~lm(new_asdn ~ asdn)$coef[2])))

summary(lm(newactive_length~x, data=all_active_lengthdf)) #slope of 1.1. The y value changes 1.1 when the x changes by 1 unit

#same slopes as asdn since the asdn is essentially just a scaled version
slope_newold_activelength<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('newactive_length') ,
                   list(slope = ~lm(newactive_length~x)$coef[2])))

all_active_lengthdf$percent_diff<- (all_active_lengthdf$new_asdn / all_active_lengthdf$asdn)* 100
med_diff<-  aggregate(all_active_lengthdf$percent_diff, by=list(all_active_lengthdf$Group.1), median, na.rm=T)

#write.csv(active_lengthdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_asdn_remove1_allyrs.csv")
#write.csv(slope_newold_asdn, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_asdn_remove1_slopes_allyrs.csv")


ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=diff_from_real, color=node_rem))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_diff_fromactual.png")

ggplot()+
  geom_line(data=all_active_lengthdf, aes(x=Group.1, y=percent_diff-100, color=node_rem))+
  geom_point(data=all_active_lengthdf, aes(x=Group.1, y=percent_diff-100, color=node_rem))+
  geom_line(data=med_diff, aes(x=Group.1, y=x-100), color ='black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_diff_fromactual_percent.png")



ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=new_asdn, color=node_rem))+
  geom_line(aes(x=Group.1, y=asdn), color = 'black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_timeseries_drydown2022.png")

ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=Group.1, y=newactive_length, color=node_rem))+
  geom_line(aes(x=Group.1, y=x), color = 'black')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(date_breaks = '1 month')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_timeseries_activelength_drydown2022.png")

ggplot(data= all_active_lengthdf)+
  geom_boxplot(aes(x= node_rem, y= percent_diff))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_boxplot_percentdiffN4D_drydown2022.png")



ggplot()+
  geom_col(data=slope_newold_asdn, aes(x=node_rem, y= new_asdn_slope-1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/remove_one_asdn_slopes_minus1N4D_drydown2022.png")


#*********************************************#
#*calculate connectivity for the individual Konza tribs####
#*
#N02B
N2B_stics<- kz_stics[ ,c('datetime', 'kz02M04', 'kz02M05', 'kz02M06', 'kz02M07', 'kz02M08', 'kz02M09', 'kz02M10', 'kz02M11')]
N2B_stics$inst_flow_net<- (rowSums(N2B_stics[2:ncol(N2B_stics)], na.rm = T)/ (rowSums(!is.na(N2B_stics[,2:ncol(N2B_stics)])))) * 100 #only includes stics without NA
#change to daily
N2B_stics$datetime<- as.POSIXct(N2B_stics$datetime, format= "%m/%d/%Y %H:%M")
N2B_stics$dt<- format(N2B_stics$datetime, "%Y-%m-%d")
N2Bstics_day<- aggregate(N2B_stics$inst_flow_net, by=list(N2B_stics$dt), mean, na.rm=T)


#N20B
N20B_stics<- kz_stics[ ,c('datetime', 'kz20M02', 'kz20M03', 'kz20M04', 'kz20M05')]
N20B_stics$inst_flow_net<- (rowSums(N20B_stics[2:ncol(N20B_stics)], na.rm = T)/ (rowSums(!is.na(N20B_stics[,2:ncol(N20B_stics)])))) * 100 #only includes stics without NA
#change to daily
N20B_stics$datetime<- as.POSIXct(N20B_stics$datetime, format= "%m/%d/%Y %H:%M")
N20B_stics$dt<- format(N20B_stics$datetime, "%Y-%m-%d")
N20Bstics_day<- aggregate(N20B_stics$inst_flow_net, by=list(N20B_stics$dt), mean, na.rm=T)


#N01B
N01B_stics<- kz_stics[ ,c('datetime', 'kz01M01', 'kz01M02', 'kz01M03', 'kz01M04')]
N01B_stics$inst_flow_net<- (rowSums(N01B_stics[2:ncol(N01B_stics)], na.rm = T)/ (rowSums(!is.na(N01B_stics[,2:ncol(N01B_stics)])))) * 100 #only includes stics without NA
#change to daily
N01B_stics$datetime<- as.POSIXct(N01B_stics$datetime, format= "%m/%d/%Y %H:%M")
N01B_stics$dt<- format(N01B_stics$datetime, "%Y-%m-%d")
N01Bstics_day<- aggregate(N01B_stics$inst_flow_net, by=list(N01B_stics$dt), mean, na.rm=T)

#N04D
N04D_stics<- kz_stics[ ,c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
                          'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')]
N04D_stics$inst_flow_net<- (rowSums(N04D_stics[2:ncol(N04D_stics)], na.rm = T)/ (rowSums(!is.na(N04D_stics[,2:ncol(N04D_stics)])))) * 100 #only includes stics without NA
#change to daily
N04D_stics$datetime<- as.POSIXct(N04D_stics$datetime, format= "%m/%d/%Y %H:%M")
N04D_stics$dt<- format(N04D_stics$datetime, "%Y-%m-%d")
N04Dstics_day<- aggregate(N04D_stics$inst_flow_net, by=list(N04D_stics$dt), mean, na.rm=T)

#*****************************#
#*run the leave one out for the sldc by trib: N2B####
N2B_stics<-c('datetime', 'kz02M04', 'kz02M05', 'kz02M06', 'kz02M07', 'kz02M08', 'kz02M09', 'kz02M10', 'kz02M11')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N2B_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#calculate the data to compare the leave one out stuff to
#merge with the lengths
kzarcs_lengths<- merge(kz_arc_pres_abs_df_daily, kz_lengths, by.x="variable", by.y="Arcs")
unique(kzarcs_lengths$variable)

kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths #this will change the length value to 0 if dry but return the length of the reach if wet
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum, na.rm=T) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step

kz_net_length_time$yr<- year(kz_net_length_time$Group.1)
kz_net_length_time$mnth<- month(kz_net_length_time$Group.1)
kz_net_length_time$wy<- ifelse(kz_net_length_time$mnth>9, kz_net_length_time$yr + 1, kz_net_length_time$yr)

cdf_kzarcs<- ecdf(kz_net_length_time$x) # the data to compare to 
kz_net_length_time$ecdf_prob<- cdf_kzarcs(kz_net_length_time$x)

ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=x))

#rest for below
N2B_stics<-c('datetime', 'kz02M04', 'kz02M05', 'kz02M06', 'kz02M07', 'kz02M08', 'kz02M09', 'kz02M10', 'kz02M11')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N2B_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge the longer data with the other stuff and create the appropriate headers
kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,14,18,17)]
colnames(kzarcs_lengths_2)[6] <- "wetdry_bin"

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(gnd$from) #all except  the sink. can't remove
n
active_lengthdf<- NULL

for (i in 1:length(n)) {
  node_rem<- n[i]
  gnd<- kzarcs_lengths_2
  newtonode <- gnd$to[gnd$from == node_rem] #identify the new node
  additionalreach <- gnd$reach_length[gnd$from == node_rem] #identify the new reach length
  from_wetdry_l<- gnd$reach_length[gnd$from == node_rem] #identify wet/dry
  from_wetdry_l
  to_wetdry_l<- gnd$reach_length[gnd$to == node_rem]
  to_wetdry_l
  
  #determine the wet dry status of the new arc if it is at the end of the reach. Take a value of 0.5 if one is 0 and the other is 1
  #keep the reach, but we change the reach value to match the to node
  if (length(unique(to_wetdry_l))==0) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      gnd_sub$wetdry_bin[gnd_sub$from == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == unique(newtonode)]
    }
    #remerge into the other file
    gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    
    new_gnd<-gnd
  }
  
  #determine the wet dry status of the new arc. Take a value of 0.5 if one is 0 and the other is 1
  if (length(unique(to_wetdry_l))==1) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem]) & is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = NA
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem] )) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$to == node_rem]
      } else if (gnd_sub$wetdry_bin[gnd_sub$to == node_rem] == gnd_sub$wetdry_bin[gnd_sub$from == node_rem]) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else{
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = 0.5
      }
      #remerge into the other file
      gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    }
    gnd$wetdry_bin[gnd$to == node_rem]
    gnd$wetdry_bin[gnd$from == node_rem]
    
    gnd$wetdry_bin[gnd$to == node_rem]
    
    new_gnd<- gnd[gnd$from != node_rem,]  #drop the unnecessary node/reach 
    
    #add the additional reach length to the wet one, or the first. 
    toindex<- new_gnd$wetdry_bin[new_gnd$to == node_rem]
    fromindex<- new_gnd$wetdry_bin[new_gnd$from == node_rem]
    
    newlength <- unique(from_wetdry_l) + unique(to_wetdry_l)
    
    new_gnd$reach_length[new_gnd$to == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$to == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$to == node_rem])
    new_gnd$reach_length[new_gnd$to == node_rem]
    new_gnd$reach_length[new_gnd$from == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$from == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$from == node_rem])
    new_gnd$reach_length[new_gnd$from == node_rem]
    new_gnd[new_gnd$to == node_rem,]
    
    new_gnd$to[new_gnd$to == node_rem] = newtonode #change to the new node
    new_gnd[new_gnd$to == newtonode,]
  }
  
  #calculate stream length duration curve
  new_gnd$length_value<- new_gnd$wetdry_bin * new_gnd$reach_length
  new_activelength<- aggregate(new_gnd$length_value, by=list(new_gnd$datetime), sum, na.rm =T)
  
  new_activelength$yr<- year(new_activelength$Group.1)
  new_activelength$mnth<- month(new_activelength$Group.1)
  new_activelength$wy<- ifelse(new_activelength$mnth>9, new_activelength$yr + 1, new_activelength$yr)
  cdf_gjarcs<- ecdf(new_activelength$x)
  new_activelength$ecdf_prob<- cdf_gjarcs(new_activelength$x)
  
  new_data<- cbind(node_rem, newtonode, new_activelength)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}


sum(unique(kzarcs_lengths$Lengths))
sum(unique(new_gnd$reach_length)) #why is this greater...? shouldn't be. 
unique(new_gnd$reach_length)
unique(kzarcs_lengths$Lengths)
unique(kzarcs_lengths$variable)
unique(new_gnd$fromto)

active_lengthdf<- as.data.frame(active_lengthdf)
colnames(active_lengthdf)[8]<- c('new_ecdf')
all_active_lengthdf<- merge(active_lengthdf, kz_net_length_time, by='Group.1')
all_active_lengthdf$diff_from_real<- round(as.numeric(all_active_lengthdf$new_ecdf), 7) - round(all_active_lengthdf$ecdf_prob,7) #round or else they both are off at the 18th decimal
head(all_active_lengthdf)

#calculate the percent change
all_active_lengthdf$percent_diff<- (all_active_lengthdf$new_ecdf/ all_active_lengthdf$ecdf_prob)*100
# #merge by active length instead
# all_active_lengthdf2<- merge(all_active_lengthdf, kz_net_length_time[,c(2,6)], by.x= 'x.x', by.y = 'x', all=T)
# all_active_lengthdf2$diff2 <- (all_active_lengthdf2$new_ecdf / all_active_lengthdf2$ecdf_prob.y)*100
# all_active_lengthdf2$diff_slkz <- ((1-all_active_lengthdf2$new_ecdf) / (1-all_active_lengthdf2$ecdf_prob.y))*100
# t<- all_active_lengthdf2[all_active_lengthdf2$node_rem %in% c('kz17'),]

#calculate the median value of the percent change
med_diff<-  aggregate(all_active_lengthdf$percent_diff, by=list(all_active_lengthdf$Group.1), median, na.rm=T)

#calcualte the slope of the line between the two 

slope_newold_ecdf<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_ecdf') ,
                   list(slope = ~lm(new_ecdf ~ ecdf_prob)$coef[2])))


#calculate the slope of the sldc curves
slope_sldc_new<- all_active_lengthdf[!is.na(all_active_lengthdf$new_ecdf),] %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_ecdf') ,
                   list(slope = ~lm(new_ecdf ~ x.x)$coef[2])))

slope_sldc_new$slope_orig <- summary(lm(ecdf_prob~x.y, all_active_lengthdf))$coef[2]
slope_sldc_new$slope_change<-slope_sldc_new$slope_orig - slope_sldc_new$new_ecdf_slope

##plot of sldc changes: N2B####
#plot cdf##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= ecdf_prob))+
  xlab('Active Stream Length (km)')+
  ylab('CDF')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/remove_one_ecdf_allsite.png")

#plot sldc##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_point(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= 1-ecdf_prob))+
  geom_point(aes(x=x.y, y= 1-ecdf_prob))+
  xlab('Active Stream Length (m)')+
  ylab('duration')+
  theme_bw()+
  facet_wrap(node_rem~.)

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/remove_one_sldc_allsite_facet.png")

#plot sldc##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_point(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= 1-ecdf_prob))+
  geom_point(aes(x=x.y, y= 1-ecdf_prob))+
  xlab('Active Stream Length (km)')+
  ylab('duration')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/remove_one_slkz_allsite.png")

write.csv(active_lengthdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_ecdf_prob_remove1_allyrs_tribN2B.csv")
write.csv(slope_newold_ecdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_ecdf_remove1_slopes_allyrs_tribN2B.csv")

#probabilities of each other
ggplot()+
  geom_col(data=slope_newold_ecdf, aes(x=node_rem, y= new_ecdf_slope-1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/remove_one_ecdf_changeinslope.png")

ggplot()+
  geom_col(data=slope_sldc_new, aes(x=node_rem, y= slope_change))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/remove_one_ecdf_changeinslopeofsldc.png")

#*********************************************#
#*#*****************************#
#*run the leave one out for the sldc by trib: N4D####
N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N4D_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#calculate the data to compare the leave one out stuff to
#merge with the lengths
kzarcs_lengths<- merge(kz_arc_pres_abs_df_daily, kz_lengths, by.x="variable", by.y="Arcs")
unique(kzarcs_lengths$variable)

kzarcs_lengths$length_value<- kzarcs_lengths$value * kzarcs_lengths$Lengths #this will change the length value to 0 if dry but return the length of the reach if wet
kz_net_length_time<- aggregate(kzarcs_lengths$length_value, by=list(kzarcs_lengths$datetime), sum, na.rm=T) #at each date/time sum the total network length
head(kz_net_length_time) #this is the total active network during a time step

kz_net_length_time$yr<- year(kz_net_length_time$Group.1)
kz_net_length_time$mnth<- month(kz_net_length_time$Group.1)
kz_net_length_time$wy<- ifelse(kz_net_length_time$mnth>9, kz_net_length_time$yr + 1, kz_net_length_time$yr)

cdf_kzarcs<- ecdf(kz_net_length_time$x) # the data to compare to 
kz_net_length_time$ecdf_prob<- cdf_kzarcs(kz_net_length_time$x)

ggplot(data=kz_net_length_time)+
  geom_line(aes(x=Group.1, y=x))

#rest for below
N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N4D_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")
#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')

#merge the longer data with the other stuff and create the appropriate headers
kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,14,18,17)]
colnames(kzarcs_lengths_2)[6] <- "wetdry_bin"

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(gnd$from) #all except  the sink. can't remove
n<-n[3:13] #can't remove confluences?
n
active_lengthdf<- NULL
#rm(i)
#i=2

for (i in 1:length(n)) {
  node_rem<- n[i]
  gnd<- kzarcs_lengths_2
  newtonode <- gnd$to[gnd$from == node_rem] #identify the new node
  additionalreach <- gnd$reach_length[gnd$from == node_rem] #identify the new reach length
  from_wetdry_l<- gnd$reach_length[gnd$from == node_rem] #identify wet/dry
  from_wetdry_l
  to_wetdry_l<- gnd$reach_length[gnd$to == node_rem]
  to_wetdry_l
  
  #determine the wet dry status of the new arc if it is at the end of the reach. Take a value of 0.5 if one is 0 and the other is 1
  #keep the reach, but we change the reach value to match the to node
  if (length(unique(to_wetdry_l))==0) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      gnd_sub$wetdry_bin[gnd_sub$from == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == unique(newtonode)]
    }
    #remerge into the other file
    gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    
    new_gnd<-gnd
  }
  
  #determine the wet dry status of the new arc. Take a value of 0.5 if one is 0 and the other is 1
  if (length(unique(to_wetdry_l))==1) {
    #need to add something for this being in long format? if they match dates?
    for (k in 1:length(unique(gnd$datetime))) {
      gnd_sub<- gnd[gnd$datetime == gnd$datetime[k],]
      print(paste0(gnd$datetime[k], k))
      if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem]) & is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = NA
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$to == node_rem] )) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else if (is.na(gnd_sub$wetdry_bin[gnd_sub$from == node_rem])) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$to == node_rem]
      } else if (gnd_sub$wetdry_bin[gnd_sub$to == node_rem] == gnd_sub$wetdry_bin[gnd_sub$from == node_rem]) {
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = gnd_sub$wetdry_bin[gnd_sub$from == node_rem]
      } else{
        gnd_sub$wetdry_bin[gnd_sub$to == node_rem] = 0.5
      }
      #remerge into the other file
      gnd[gnd$datetime == gnd$datetime[k],]<- gnd_sub[gnd_sub$datetime == gnd_sub$datetime,]
    }
    gnd$wetdry_bin[gnd$to == node_rem]
    gnd$wetdry_bin[gnd$from == node_rem]
    
    gnd$wetdry_bin[gnd$to == node_rem]
    
    new_gnd<- gnd[gnd$from != node_rem,]  #drop the unnecessary node/reach 
    
    #add the additional reach length to the wet one, or the first. 
    toindex<- new_gnd$wetdry_bin[new_gnd$to == node_rem]
    fromindex<- new_gnd$wetdry_bin[new_gnd$from == node_rem]
    
    newlength <- unique(from_wetdry_l) + unique(to_wetdry_l)
    
    new_gnd$reach_length[new_gnd$to == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$to == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$to == node_rem])
    new_gnd$reach_length[new_gnd$to == node_rem]
    new_gnd$reach_length[new_gnd$from == node_rem]<- ifelse(new_gnd$reach_length[new_gnd$from == node_rem] + additionalreach == newlength, newlength, new_gnd$reach_length[new_gnd$from == node_rem])
    new_gnd$reach_length[new_gnd$from == node_rem]
    new_gnd[new_gnd$to == node_rem,]
    
    new_gnd$to[new_gnd$to == node_rem] = newtonode #change to the new node
    new_gnd[new_gnd$to == newtonode,]
  }
  
  #calculate stream length duration curve
  new_gnd$length_value<- new_gnd$wetdry_bin * new_gnd$reach_length
  new_activelength<- aggregate(new_gnd$length_value, by=list(new_gnd$datetime), sum, na.rm =T)
  
  new_activelength$yr<- year(new_activelength$Group.1)
  new_activelength$mnth<- month(new_activelength$Group.1)
  new_activelength$wy<- ifelse(new_activelength$mnth>9, new_activelength$yr + 1, new_activelength$yr)
  cdf_gjarcs<- ecdf(new_activelength$x)
  new_activelength$ecdf_prob<- cdf_gjarcs(new_activelength$x)
  
  new_data<- cbind(node_rem, newtonode, new_activelength)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}


sum(unique(kzarcs_lengths$Lengths))
sum(unique(new_gnd$reach_length)) #why is this greater...? shouldn't be. 
unique(new_gnd$reach_length)
unique(kzarcs_lengths$Lengths)
unique(kzarcs_lengths$variable)
unique(new_gnd$fromto)

active_lengthdf<- as.data.frame(active_lengthdf)
colnames(active_lengthdf)[8]<- c('new_ecdf')
all_active_lengthdf<- merge(active_lengthdf, kz_net_length_time, by='Group.1')
all_active_lengthdf$diff_from_real<- round(as.numeric(all_active_lengthdf$new_ecdf), 7) - round(all_active_lengthdf$ecdf_prob,7) #round or else they both are off at the 18th decimal
head(all_active_lengthdf)

#calculate the percent change
all_active_lengthdf$percent_diff<- (all_active_lengthdf$new_ecdf/ all_active_lengthdf$ecdf_prob)*100
# #merge by active length instead
# all_active_lengthdf2<- merge(all_active_lengthdf, kz_net_length_time[,c(2,6)], by.x= 'x.x', by.y = 'x', all=T)
# all_active_lengthdf2$diff2 <- (all_active_lengthdf2$new_ecdf / all_active_lengthdf2$ecdf_prob.y)*100
# all_active_lengthdf2$diff_slkz <- ((1-all_active_lengthdf2$new_ecdf) / (1-all_active_lengthdf2$ecdf_prob.y))*100
# t<- all_active_lengthdf2[all_active_lengthdf2$node_rem %in% c('kz17'),]

#calculate the median value of the percent change
med_diff<-  aggregate(all_active_lengthdf$percent_diff, by=list(all_active_lengthdf$Group.1), median, na.rm=T)

#calcualte the slope of the line between the two 

slope_newold_ecdf<- all_active_lengthdf %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_ecdf') ,
                   list(slope = ~lm(new_ecdf ~ ecdf_prob)$coef[2])))


#calculate the slope of the sldc curves
slope_sldc_new<- all_active_lengthdf[!is.na(all_active_lengthdf$new_ecdf),] %>% 
  group_by(node_rem) %>%
  summarise(across(starts_with('new_ecdf') ,
                   list(slope = ~lm(new_ecdf ~ x.x)$coef[2])))

slope_sldc_new$slope_orig <- summary(lm(ecdf_prob~x.y, all_active_lengthdf))$coef[2]
slope_sldc_new$slope_change<-slope_sldc_new$slope_orig - slope_sldc_new$new_ecdf_slope

##plot of sldc changes: N4D####
#plot cdf##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= ecdf_prob))+
  xlab('Active Stream Length (km)')+
  ylab('CDF')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/remove_one_ecdf_allsite_drydown2022.png")

#plot sldc##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_point(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= 1-ecdf_prob))+
  geom_point(aes(x=x.y, y= 1-ecdf_prob))+
  xlab('Active Stream Length (m)')+
  ylab('duration')+
  theme_bw()+
  facet_wrap(node_rem~.)

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/remove_one_sldc_allsite_facet_drydown2022.png")

#plot sldc##
ggplot(data=all_active_lengthdf)+
  geom_line(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_point(aes(x=x.x, y=1-new_ecdf, color = node_rem))+
  geom_line(aes(x=x.y, y= 1-ecdf_prob))+
  geom_point(aes(x=x.y, y= 1-ecdf_prob))+
  xlab('Active Stream Length (km)')+
  ylab('duration')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/remove_one_slkz_allsite_drydown2022.png")

write.csv(active_lengthdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_ecdf_prob_remove1_allyrs_tribN4D.csv")
write.csv(slope_newold_ecdf, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kz_ecdf_remove1_slopes_allyrs_tribN4D.csv")

#probabilities of each other
ggplot()+
  geom_col(data=slope_newold_ecdf, aes(x=node_rem, y= new_ecdf_slope-1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/remove_one_ecdf_changeinslope_drydown2022.png")

ggplot()+
  geom_col(data=slope_sldc_new, aes(x=node_rem, y= slope_change))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Slope of original versus node removed asdn")+
  xlab("Node Removed")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/remove_one_ecdf_changeinslopeofsldc_drydown2022.png")



#********************************************************#
#*centrality metric by trib: N2B####
#using streamDAG. 
#N2Bnetwork<- (kz02M11_1 --+ kz02M10_1 --+ kz02M09_1 --+ kz02M08_1 --+ kz02M07_1 --+ kz02M06_1 --+ kz02M05_1 --+ kz02M04_1)
#N4Dnetwork<- (kz04M13_1 --+ kz04M12_1 --+ kz04C05 --+ kz04M11_1 --+ kz04C04 --+ kz04M10_1 --+ kz04M09_1 --+ kz04C03,
 #             kz04T02_1 --+ kz04C03 --+ kz04M08_1 --+ kz04M07_1 --+ kz04C02,
  #            kz04T01_1 --+ kz04C02 --+ kz04M06_1 --+ kz04M05_1 --+ kz04M04_1 --+ kz04M03_1)

kz_net<- graph_from_literal(kz02M11 --+ kz02M10 --+ kz02M09 --+ kz02M08 --+ kz02M07 --+ kz02M06 --+ kz02M05 --+ kz02M04)

x<- kz_coords[,3] ; y <- kz_coords[,2]
names=kz_coords[,1]
spatial.plot(kz_net, x,y,names, cex.text = 1)

local<- local.summary(kz_net)
round(local,2)

cc<-complete.cases(local)
local.cc<- local[cc,]
scaled.local<- scale(t(local.cc))
local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
rownames(local_met)<- 1:nrow(local_met)
local_met<-as.data.frame(local_met)
local_met_orig<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
local_met_m<- melt(local_met_orig, id.vars=c('nodes'))
local_met_m$value<-as.numeric(local_met_m$value)

ggplot(data=local_met_m)+
  geom_col(aes(x=nodes, y=value)) +
  facet_grid(variable~., scales='free')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/centrality_metrics_node.png", width=15)

#*******************************************#
#leave one out with the centrality index: trib N2B####

graph_from_string <- function(x) {
  e <- str2expression(strsplit(x, ",")[[1]])
  do.call(igraph:::graph_from_literal_i, list(e))
}

#go into the string and remove that node, recalculate the metric, save results

orig_string<- ("kz02M11 -- kz02M10 -- kz02M09 -- kz02M08 -- kz02M07 -- kz02M06 -- kz02M05 -- kz02M04")

#
N2B_stics<-c('datetime', 'kz02M04', 'kz02M05', 'kz02M06', 'kz02M07', 'kz02M08', 'kz02M09', 'kz02M10', 'kz02M11')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N2B_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)

kz_arc_pres_abs_df <- kz_arc_pres_abs_df[,colSums(is.na(kz_arc_pres_abs_df))<nrow(kz_arc_pres_abs_df)]

kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")

#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')
unique(kz_arc_pres_abs_df_daily$variable)
#merge the longer data with the other stuff and create the appropriate headers

kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
kzarcs_lengths_2<- merge(kzarcs_lengths_2, kz_lengths, by.y= 'Arcs', by.x='fromto') #use the other lenght arcs to avoid issues of rounding
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,19,18,17)]
colnames(kzarcs_lengths_2)[6] <- c( "wetdry_bin")
unique(kzarcs_lengths_2$fromto)


#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(c(gnd$from, gnd$to)) #all except  the sink. can't remove
#n<- n[9:35] #don't do the confluences
n
active_lengthdf<- NULL
#i=8
for (i in 1:length(n)) {
  node_rem<- n[i]
  node_rem
  #newstring <- gsub(node_rem, "", orig_string)
  #newstring<- gsub("--+ ,", ",", newstring)
  newstring<- gsub(paste0("-- ", node_rem),"", orig_string)
  newstring<- gsub(paste0(node_rem, " --"),"", newstring)
  newstring<- gsub("--","--+", newstring)
  orig_string
  newstring
  newgraph<- eval(str2lang(sprintf("graph_from_literal(%s)", newstring)))
  newgraph
  spatial.plot(newgraph, x,y,names, cex.text = 1)
  #calculate the centrality index's
  local<- local.summary(newgraph)
  cc<-complete.cases(local)
  local.cc<- local[cc,]
  scaled.local<- scale(t(local.cc))
  local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
  rownames(local_met)<- 1:nrow(local_met)
  local_met<-as.data.frame(local_met)
  #local_met<- local_met[!local_met$nodes %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),] #don't look at the confluences
  local_met<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
  #local_met_m<- melt(local_met, id.vars=c('nodes'))
  #local_met_m$value<-as.numeric(local_met_m$value)
  
  new_data<- cbind(node_rem, local_met)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}

all_active_lengthdf<- merge(active_lengthdf, local_met_orig, by=c('nodes'), all=T)
all_active_lengthdf$diff_from_real_betweenCentr<-as.numeric(all_active_lengthdf$betweenness.cent.x) - as.numeric(all_active_lengthdf$betweenness.cent.y) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_impcloseness<-as.numeric(all_active_lengthdf$imp.closeness.cent.x) - as.numeric(all_active_lengthdf$imp.closeness.cent.y) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_alphacent<-as.numeric(all_active_lengthdf$alpha.cent.x) - as.numeric(all_active_lengthdf$alpha.cent.y) #round or else they both are off at the 18th decimal

head(all_active_lengthdf)
#calculate the percent change
all_active_lengthdf$alpha_cent_percent_diff<- ((as.numeric(all_active_lengthdf$alpha.cent.x)/ as.numeric(all_active_lengthdf$alpha.cent.y))*100)-100
all_active_lengthdf$impclose_percent_diff<- ((as.numeric(all_active_lengthdf$imp.closeness.cent.x)/ as.numeric(all_active_lengthdf$imp.closeness.cent.y))*100)-100
all_active_lengthdf$between_percent_diff<- ((as.numeric(all_active_lengthdf$betweenness.cent.x)/ as.numeric(all_active_lengthdf$betweenness.cent.y))*100)-100

#average the change value
#average change for node removed- how much of an impact does that node have
cent_stats<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), mean, na.rm=T)
cent_statsmin<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), min, na.rm=T)
cent_statsmax<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), max, na.rm=T)
cent_stats$stat<- 'mean';cent_statsmin$stat<-'min';cent_statsmax$stat<- 'max'
cent_stats_a <- rbind(cent_stats, cent_statsmax, cent_statsmin)

#average change when other nodes are removed- how sensitive is that node
cent_stats2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), mean, na.rm=T)
cent_statsmin2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), min, na.rm=T)
cent_statsmax2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), max, na.rm=T)
cent_stats2$stat<- 'mean';cent_statsmin2$stat<-'min';cent_statsmax2$stat<- 'max'
cent_stats_b <- rbind(cent_stats2, cent_statsmax2, cent_statsmin2)

#plot centrality differences:trib N2B####
ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=impclose_percent_diff))+
  facet_grid(node_rem~., scales='free')+
  ylab('% change in closeness- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/centralitycloseness_change.png", width=12, height = 15)


ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=between_percent_diff))+
  facet_grid(node_rem~.)+
  ylab('% change in betweenness- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/centralitybetweenness_change.png", width=12, height = 15)


ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=alpha_cent_percent_diff))+
  facet_grid(node_rem~., scales='free')+
  ylab('% change in alpha- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/centralityalpha_change.png", width=12, height = 15)

#senstivity of the node removed
ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=node_rem, y=diff_from_real_betweenCentr))

ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=node_rem, y=diff_from_real_impcloseness))

#how the node changes when others are removed - sensitivity of that node
ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=nodes, y=diff_from_real_betweenCentr))

ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=nodes, y=diff_from_real_impcloseness))

#change when that node is removed 
ggplot(data=cent_stats_a)+
  geom_point(aes(x=Group.1, y=x, color=stat))+
  geom_line(aes(x=Group.1, y=x, group = Group.1))+
  ggtitle("betweenness change when the node is removed")+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/centralitybetweenness_change_value.png", width = 12)  

#how that node changes when other nodes are removed
ggplot(data=cent_stats_b)+
  geom_point(aes(x=Group.1, y=x, color=stat))+
  geom_line(aes(x=Group.1, y=x, group = Group.1))+
  ggtitle("betweenness change when other nodes are removed")+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/centralitybetweenness_change_value_vs2.png", width=12)  

#*******************************************#
#global centrality changes: N2B####
spatial.plot(kz_net, x,y,names, cex.text = 1)

####global metrics##
g <- global.summary(kz_net, sink = "P1")
g

g3 <-cbind(nodes=rownames(t(g)), t(g))
rownames(g3)<- 1:nrow(g3)
g3<-as.data.frame(g3)

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(c(gnd$from, gnd$to)) #all except  the sink. can't remove
#n<- n[9:35] #don't do the confluences
n
globalmetricsdf<- NULL
#i=8
for (i in 1:length(n)) {
  node_rem<- n[i]
  node_rem
  #newstring <- gsub(node_rem, "", orig_string)
  #newstring<- gsub("--+ ,", ",", newstring)
  newstring<- gsub(paste0("-- ", node_rem),"", orig_string)
  newstring<- gsub(paste0(node_rem, " --"),"", newstring)
  newstring<- gsub("--","--+", newstring)
  orig_string
  newstring
  newgraph<- eval(str2lang(sprintf("graph_from_literal(%s)", newstring)))
  newgraph
  spatial.plot(newgraph, x,y,names, cex.text = 1)
  #calculate the centrality index's
  g2<- global.summary(newgraph, sink='P1')
  cc<-complete.cases(g2)
  g2.cc<- g2[cc,]
  scaled.g2<- scale(t(g2.cc))
  g2_met <-cbind(nodes=rownames(t(g2.cc)), t(g2.cc))
  rownames(g2_met)<- 1:nrow(g2_met)
  g2_met<-as.data.frame(g2_met)
  #g2_met<- g2_met[!g2_met$nodes %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),] #don't look at the confluences
  g2_met<- g2_met[,c(5)] #only look at the centrality metrics 
  #g2_met_m<- melt(g2_met, id.vars=c('nodes'))
  #g2_met_m$value<-as.numeric(g2_met_m$value)
  
  new_data<- cbind(node_rem, g2_met)
  globalmetricsdf<- rbind(globalmetricsdf, new_data)
}

all_globalmetricsdf<- as.data.frame(globalmetricsdf)
all_globalmetricsdf$orig_centr<- g3$Mean.a.centrality
all_globalmetricsdf$diff_from_real_betweenCentr<-as.numeric(all_globalmetricsdf$g2_met) - as.numeric(all_globalmetricsdf$orig_centr) #round or else they both are off at the 18th decimal

head(all_globalmetricsdf)

#calculate the percent change
all_globalmetricsdf$percent_diff<- ((as.numeric(all_globalmetricsdf$g2_met)/ as.numeric(all_globalmetricsdf$orig_centr))*100)-100

ggplot(data=all_globalmetricsdf)+
  geom_col(aes(x= node_rem, y=diff_from_real_betweenCentr))+
  theme_bw()+
  ggtitle("global change in centrality")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/global_change_centrality.png", width=12)  


ggplot(data=all_globalmetricsdf)+
  geom_col(aes(x= node_rem, y=percent_diff))


#********************************************************#
#*centrality metric by trib: N4D####
#using streamDAG. 
#N4Dnetwork<- (kz02M11_1 --+ kz02M10_1 --+ kz02M09_1 --+ kz02M08_1 --+ kz02M07_1 --+ kz02M06_1 --+ kz02M05_1 --+ kz02M04_1)
#N4Dnetwork<- (kz04M13_1 --+ kz04M12_1 --+ kz04C05 --+ kz04M11_1 --+ kz04C04 --+ kz04M10_1 --+ kz04M09_1 --+ kz04C03,
#             kz04T02_1 --+ kz04C03 --+ kz04M08_1 --+ kz04M07_1 --+ kz04C02,
#            kz04T01_1 --+ kz04C02 --+ kz04M06_1 --+ kz04M05_1 --+ kz04M04_1 --+ kz04M03_1)

kz_net<- graph_from_literal(kz04M13 --+ kz04M12 --+ kz04C05 --+ kz04M11 --+ kz04C04 --+ kz04M10 --+ kz04M09 --+ kz04C03,
                                         kz04T02 --+ kz04C03 --+ kz04M08 --+ kz04M07 --+ kz04C02,
                                        kz04T01 --+ kz04C02 --+ kz04M06 --+ kz04M05 --+ kz04M04 --+ kz04M03)

x<- kz_coords[,3] ; y <- kz_coords[,2]
names=kz_coords[,1]
spatial.plot(kz_net, x,y,names, cex.text = 1)

local<- local.summary(kz_net)
round(local,2)

cc<-complete.cases(local)
local.cc<- local[cc,]
scaled.local<- scale(t(local.cc))
local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
rownames(local_met)<- 1:nrow(local_met)
local_met<-as.data.frame(local_met)
local_met_orig<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
local_met_m<- melt(local_met_orig, id.vars=c('nodes'))
local_met_m$value<-as.numeric(local_met_m$value)

ggplot(data=local_met_m)+
  geom_col(aes(x=nodes, y=value)) +
  facet_grid(variable~., scales='free')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centrality_metrics_node.png", width=15)

#*******************************************#
#leave one out with the centrality index: trib N4D####

graph_from_string <- function(x) {
  e <- str2expression(strsplit(x, ",")[[1]])
  do.call(igraph:::graph_from_literal_i, list(e))
}

#go into the string and remove that node, recalculate the metric, save results

orig_string<- ("kz04M13 --+ kz04M12 --+ kz04C05 --+ kz04M11 --+ kz04C04 --+ kz04M10 --+ kz04M09 --+ kz04C03,
                                         kz04T02 --+ kz04C03 --+ kz04M08 --+ kz04M07 --+ kz04C02,
                                        kz04T01 --+ kz04C02 --+ kz04M06 --+ kz04M05 --+ kz04M04 --+ kz04M03")

#
N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

kz_arc_pres_abs_df<-kz_arc_pres_abs[,grep(paste(N4D_stics,collapse="|"), names(kz_arc_pres_abs))] #subst just those stics
head(kz_arc_pres_abs_df)

kz_arc_pres_abs_df <- kz_arc_pres_abs_df[,colSums(is.na(kz_arc_pres_abs_df))<nrow(kz_arc_pres_abs_df)]

kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")

#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')
unique(kz_arc_pres_abs_df_daily$variable)
#merge the longer data with the other stuff and create the appropriate headers

kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
kzarcs_lengths_2<- merge(kzarcs_lengths_2, kz_lengths, by.y= 'Arcs', by.x='fromto') #use the other lenght arcs to avoid issues of rounding
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,19,18,17)]
colnames(kzarcs_lengths_2)[6] <- c( "wetdry_bin")
unique(kzarcs_lengths_2$fromto)


#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(c(gnd$from, gnd$to)) #all except  the sink. can't remove
#n<- n[9:35] #don't do the confluences
n
active_lengthdf<- NULL
#i=8
for (i in 1:length(n)) {
  node_rem<- n[i]
  node_rem
  #newstring <- gsub(node_rem, "", orig_string)
  #newstring<- gsub("--+ ,", ",", newstring)
  newstring<- gsub(paste0("-- ", node_rem),"", orig_string)
  newstring<- gsub(paste0(node_rem, " --"),"", newstring)
  newstring<- gsub("--","--+", newstring)
  orig_string
  newstring
  newgraph<- eval(str2lang(sprintf("graph_from_literal(%s)", newstring)))
  newgraph
  spatial.plot(newgraph, x,y,names, cex.text = 1)
  #calculate the centrality index's
  local<- local.summary(newgraph)
  cc<-complete.cases(local)
  local.cc<- local[cc,]
  scaled.local<- scale(t(local.cc))
  local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
  rownames(local_met)<- 1:nrow(local_met)
  local_met<-as.data.frame(local_met)
  #local_met<- local_met[!local_met$nodes %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),] #don't look at the confluences
  local_met<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
  #local_met_m<- melt(local_met, id.vars=c('nodes'))
  #local_met_m$value<-as.numeric(local_met_m$value)
  
  new_data<- cbind(node_rem, local_met)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}

all_active_lengthdf<- merge(active_lengthdf, local_met_orig, by=c('nodes'), all=T)
all_active_lengthdf$diff_from_real_betweenCentr<-as.numeric(all_active_lengthdf$betweenness.cent.x) - as.numeric(all_active_lengthdf$betweenness.cent.y) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_impcloseness<-as.numeric(all_active_lengthdf$imp.closeness.cent.x) - as.numeric(all_active_lengthdf$imp.closeness.cent.y) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_alphacent<-as.numeric(all_active_lengthdf$alpha.cent.x) - as.numeric(all_active_lengthdf$alpha.cent.y) #round or else they both are off at the 18th decimal

head(all_active_lengthdf)
#calculate the percent change
all_active_lengthdf$alpha_cent_percent_diff<- ((as.numeric(all_active_lengthdf$alpha.cent.x)/ as.numeric(all_active_lengthdf$alpha.cent.y))*100)-100
all_active_lengthdf$impclose_percent_diff<- ((as.numeric(all_active_lengthdf$imp.closeness.cent.x)/ as.numeric(all_active_lengthdf$imp.closeness.cent.y))*100)-100
all_active_lengthdf$between_percent_diff<- ((as.numeric(all_active_lengthdf$betweenness.cent.x)/ as.numeric(all_active_lengthdf$betweenness.cent.y))*100)-100

#average the change value
#average change for node removed- how much of an impact does that node have
cent_stats<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), mean, na.rm=T)
cent_statsmin<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), min, na.rm=T)
cent_statsmax<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), max, na.rm=T)
cent_stats$stat<- 'mean';cent_statsmin$stat<-'min';cent_statsmax$stat<- 'max'
cent_stats_a <- rbind(cent_stats, cent_statsmax, cent_statsmin)

#average change when other nodes are removed- how sensitive is that node
cent_stats2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), mean, na.rm=T)
cent_statsmin2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), min, na.rm=T)
cent_statsmax2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), max, na.rm=T)
cent_stats2$stat<- 'mean';cent_statsmin2$stat<-'min';cent_statsmax2$stat<- 'max'
cent_stats_b <- rbind(cent_stats2, cent_statsmax2, cent_statsmin2)

#plot centrality differences:trib N4D####
ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=impclose_percent_diff))+
  facet_grid(node_rem~., scales='free')+
  ylab('% change in closeness- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centralitycloseness_change.png", width=12, height = 15)


ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=between_percent_diff))+
  facet_grid(node_rem~.)+
  ylab('% change in betweenness- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centralitybetweenness_change.png", width=12, height = 15)


ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=alpha_cent_percent_diff))+
  facet_grid(node_rem~., scales='free')+
  ylab('% change in alpha- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centralityalpha_change.png", width=12, height = 15)

#senstivity of the node removed
ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=node_rem, y=diff_from_real_betweenCentr))

ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=node_rem, y=diff_from_real_impcloseness))

#how the node changes when others are removed - sensitivity of that node
ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=nodes, y=diff_from_real_betweenCentr))

ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=nodes, y=diff_from_real_impcloseness))

#change when that node is removed 
ggplot(data=cent_stats_a)+
  geom_point(aes(x=Group.1, y=x, color=stat))+
  geom_line(aes(x=Group.1, y=x, group = Group.1))+
  ggtitle("betweenness change when the node is removed")+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centralitybetweenness_change_value.png", width = 12)  

#how that node changes when other nodes are removed
ggplot(data=cent_stats_b)+
  geom_point(aes(x=Group.1, y=x, color=stat))+
  geom_line(aes(x=Group.1, y=x, group = Group.1))+
  ggtitle("betweenness change when other nodes are removed")+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centralitybetweenness_change_value_vs2.png", width=12)  

#*******************************************#
#global centrality changes N4D####
spatial.plot(kz_net, x,y,names, cex.text = 1)

####global metrics##
g <- global.summary(kz_net, sink = "P1")
g

g3 <-cbind(nodes=rownames(t(g)), t(g))
rownames(g3)<- 1:nrow(g3)
g3<-as.data.frame(g3)

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(c(gnd$from, gnd$to)) #all except  the sink. can't remove
#n<- n[9:35] #don't do the confluences
n
globalmetricsdf<- NULL
#i=8
for (i in 1:length(n)) {
  node_rem<- n[i]
  node_rem
  #newstring <- gsub(node_rem, "", orig_string)
  #newstring<- gsub("--+ ,", ",", newstring)
  newstring<- gsub(paste0("-- ", node_rem),"", orig_string)
  newstring<- gsub(paste0(node_rem, " --"),"", newstring)
  newstring<- gsub("--","--+", newstring)
  orig_string
  newstring
  newgraph<- eval(str2lang(sprintf("graph_from_literal(%s)", newstring)))
  newgraph
  spatial.plot(newgraph, x,y,names, cex.text = 1)
  #calculate the centrality index's
  g2<- global.summary(newgraph, sink='P1')
  cc<-complete.cases(g2)
  g2.cc<- g2[cc,]
  scaled.g2<- scale(t(g2.cc))
  g2_met <-cbind(nodes=rownames(t(g2.cc)), t(g2.cc))
  rownames(g2_met)<- 1:nrow(g2_met)
  g2_met<-as.data.frame(g2_met)
  #g2_met<- g2_met[!g2_met$nodes %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),] #don't look at the confluences
  g2_met<- g2_met[,c(5)] #only look at the centrality metrics 
  #g2_met_m<- melt(g2_met, id.vars=c('nodes'))
  #g2_met_m$value<-as.numeric(g2_met_m$value)
  
  new_data<- cbind(node_rem, g2_met)
  globalmetricsdf<- rbind(globalmetricsdf, new_data)
}

all_globalmetricsdf<- as.data.frame(globalmetricsdf)
all_globalmetricsdf$orig_centr<- g3$Mean.a.centrality
all_globalmetricsdf$diff_from_real_betweenCentr<-as.numeric(all_globalmetricsdf$g2_met) - as.numeric(all_globalmetricsdf$orig_centr) #round or else they both are off at the 18th decimal

head(all_globalmetricsdf)

#calculate the percent change
all_globalmetricsdf$percent_diff<- ((as.numeric(all_globalmetricsdf$g2_met)/ as.numeric(all_globalmetricsdf$orig_centr))*100)-100

ggplot(data=all_globalmetricsdf)+
  geom_col(aes(x= node_rem, y=diff_from_real_betweenCentr))+
  theme_bw()+
  ggtitle("global change in centrality")

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/global_change_centrality.png", width=12)  


ggplot(data=all_globalmetricsdf)+
  geom_col(aes(x= node_rem, y=percent_diff))



#********************************************************#
#*centrality metric by trib: N4D####
#using streamDAG. 
#N4Dnetwork<- (kz02M11_1 --+ kz02M10_1 --+ kz02M09_1 --+ kz02M08_1 --+ kz02M07_1 --+ kz02M06_1 --+ kz02M05_1 --+ kz02M04_1)
#N4Dnetwork<- (kz04M13_1 --+ kz04M12_1 --+ kz04C05 --+ kz04M11_1 --+ kz04C04 --+ kz04M10_1 --+ kz04M09_1 --+ kz04C03,
#             kz04T02_1 --+ kz04C03 --+ kz04M08_1 --+ kz04M07_1 --+ kz04C02,
#            kz04T01_1 --+ kz04C02 --+ kz04M06_1 --+ kz04M05_1 --+ kz04M04_1 --+ kz04M03_1)

kz_net<- graph_from_literal(kz04M13 --+ kz04M12 --+ kz04C05 --+ kz04M11 --+ kz04C04 --+ kz04M10 --+ kz04M09 --+ kz04C03,
                            kz04T02 --+ kz04C03 --+ kz04M08 --+ kz04M07 --+ kz04C02,
                            kz04T01 --+ kz04C02 --+ kz04M06 --+ kz04M05 --+ kz04M04 --+ kz04M03)

x<- kz_coords[,3] ; y <- kz_coords[,2]
names=kz_coords[,1]
spatial.plot(kz_net, x,y,names, cex.text = 1)

local<- local.summary(kz_net)
round(local,2)

cc<-complete.cases(local)
local.cc<- local[cc,]
scaled.local<- scale(t(local.cc))
local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
rownames(local_met)<- 1:nrow(local_met)
local_met<-as.data.frame(local_met)
local_met_orig<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
local_met_m<- melt(local_met_orig, id.vars=c('nodes'))
local_met_m$value<-as.numeric(local_met_m$value)

ggplot(data=local_met_m)+
  geom_col(aes(x=nodes, y=value)) +
  facet_grid(variable~., scales='free')

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centrality_metrics_node.png", width=15)

#*******************************************#
#leave one out with the centrality index entire network####
kz_net<- graph_from_literal(kz02M11 --+ kz02M10 --+ kz02M09 --+ kz02M08 --+ kz02M07 --+ kz02M06 --+ kz02M05 --+ kz02M04)


graph_from_string <- function(x) {
  e <- str2expression(strsplit(x, ",")[[1]])
  do.call(igraph:::graph_from_literal_i, list(e))
}

#go into the string and remove that node, recalculate the metric, save results
origgraph<-graph_from_literal(kz02M11 --+ kz02M10 --+ kz02M09 --+ kz02M08 --+ kz02M07 --+ kz02M06 --+ kz02M05 --+ 
                                kz02M04 --+ kz02M03 --+ kz02M02 --+ kz02M01 --+ kzSFC01,
               kz04M13 --+ kz04M12 --+ kz04C05 --+ kz04M11 --+ kz04C04 --+ kz04M10 --+ kz04M09 --+ kz04C03,
               kz04T02 --+ kz04C03 --+ kz04M08 --+ kz04M07 --+ kz04C02,
               kz04T01 --+ kz04C02 --+ kz04M06 --+ kz04M05 --+ kz04M04 --+ kz04M03 --+ kz04M02 --+ kz04C01,
               kz04W04 --+ kz04W03 --+ kz04W02 --+ kz04W01 --+ kz04C01 --+ kz04M01 --+ kzSFC02,
               kz01M06 --+ kz01M05 --+ kz01M04 --+ kz01M03 --+ kz01M02 --+ kz01M01 --+ kzSFC05,
               kz20M05 --+ kz20M04 --+ kz20M03 --+ kz20M02 --+ kz20M01 --+ kzSFC05  --+ kzSFM07 --+ kzSFM06 --+ kzSFC04,
               kzSFT02 --+ kzSFC04 --+ kzSFM05 --+ kzSFM04 --+ kzSFC03,
               kzSFT01 --+ kzSFC03 --+ kzSFM03 --+ kzSFC02 --+ kzSFM02 --+ kzSFC01 --+ kzSFM01)

x<- kz_coords[,3] ; y <- kz_coords[,2]
names=kz_coords[,1]
names
spatial.plot(origgraph, x,y,names, cex.text = 1)

local<- local.summary(origgraph)
round(local,2)

cc<-complete.cases(local)
local.cc<- local[cc,]
scaled.local<- scale(t(local.cc))
local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
rownames(local_met)<- 1:nrow(local_met)
local_met<-as.data.frame(local_met)
local_met_orig<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
local_met_m<- melt(local_met_orig, id.vars=c('nodes'))
local_met_m$value<-as.numeric(local_met_m$value)

ggplot(data=local_met_m)+
  geom_col(aes(x=nodes, y=value)) +
  facet_grid(variable~., scales='free')


orig_string<- ("kz02M11 -- kz02M10 -- kz02M09 -- kz02M08 -- kz02M07 -- kz02M06 -- kz02M05 -- 
                                kz02M04 -- kz02M03 -- kz02M02 -- kz02M01 -- kzSFC01,
               kz04M13 -- kz04M12 -- kz04C05 -- kz04M11 -- kz04C04 -- kz04M10 -- kz04M09 -- kz04C03,
               kz04T02 -- kz04C03 -- kz04M08 -- kz04M07 -- kz04C02,
               kz04T01 -- kz04C02 -- kz04M06 -- kz04M05 -- kz04M04 -- kz04M03 -- kz04M02 -- kz04C01,
               kz04W04 -- kz04W03 -- kz04W02 -- kz04W01 -- kz04C01 -- kz04M01 -- kzSFC02,
               kz01M06 -- kz01M05 -- kz01M04 -- kz01M03 -- kz01M02 -- kz01M01 -- kzSFC05,
               kz20M05 -- kz20M04 -- kz20M03 -- kz20M02 -- kz20M01 -- kzSFC05  -- kzSFM07 -- kzSFM06 -- kzSFC04,
               kzSFT02 -- kzSFC04 -- kzSFM05 -- kzSFM04 -- kzSFC03,
               kzSFT01 -- kzSFC03 -- kzSFM03 -- kzSFC02 -- kzSFM02 -- kzSFC01 -- kzSFM01")


kz_arc_pres_abs_df<- kz_arc_pres_abs
head(kz_arc_pres_abs_df)
kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")

head(kz_arc_pres_abs_df)

# kz_arc_pres_abs_df <- kz_arc_pres_abs_df[,colSums(is.na(kz_arc_pres_abs_df))<nrow(kz_arc_pres_abs_df)]
# 
# kz_arc_pres_abs_df<- melt(kz_arc_pres_abs_df[,c(1:ncol(kz_arc_pres_abs_df))], id.vars ="datetime")
# kz_arc_pres_abs_df$datetime<- as.POSIXct(kz_arc_pres_abs_df$datetime, format= "%m/%d/%Y %H:%M")

#change to daily
kz_arc_pres_abs_df$dt<- as.Date(kz_arc_pres_abs_df$datetime, format="%Y-%m-%d")
kz_arc_pres_abs_df_daily<- aggregate(kz_arc_pres_abs_df$value, by=list(kz_arc_pres_abs_df$dt, kz_arc_pres_abs_df$variable), 
                                     FUN= median, na.action=na.omit)
colnames(kz_arc_pres_abs_df_daily)<- c('datetime', 'variable', 'value')
unique(kz_arc_pres_abs_df_daily$variable)
#merge the longer data with the other stuff and create the appropriate headers

kzarcs_lengths_2<- merge(kz_nodedata, kz_arc_pres_abs_df_daily, by.y= 'variable', by.x='fromto')
kzarcs_lengths_2<- merge(kzarcs_lengths_2, kz_lengths, by.y= 'Arcs', by.x='fromto') #use the other lenght arcs to avoid issues of rounding
#kzarcs_lengths_2<- kzarcs_lengths_2[,c(1,12,13,19,18,17)]
colnames(kzarcs_lengths_2)[6] <- c( "wetdry_bin")
unique(kzarcs_lengths_2$fromto)

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(c(gnd$from, gnd$to)) #all except  the sink. can't remove
#n<- n[9:35] #don't do the confluences
n
active_lengthdf<- NULL
#i=8
for (i in 1:length(n)) {
  node_rem<- n[i]
  node_rem
  #newstring <- gsub(node_rem, "", orig_string)
  #newstring<- gsub("--+ ,", ",", newstring)
  newstring<- gsub(paste0("-- ", node_rem),"", orig_string)
  newstring<- gsub(paste0(node_rem, " --"),"", newstring)
  newstring<- gsub("--","--+", newstring)
  orig_string
  newstring
  newgraph<- eval(str2lang(sprintf("graph_from_literal(%s)", newstring)))
  newgraph
  spatial.plot(newgraph, x,y,names, cex.text = 1)
  #calculate the centrality index's
  local<- local.summary(newgraph)
  cc<-complete.cases(local)
  local.cc<- local[cc,]
  scaled.local<- scale(t(local.cc))
  local_met <-cbind(nodes=rownames(t(local.cc)), t(local.cc))
  rownames(local_met)<- 1:nrow(local_met)
  local_met<-as.data.frame(local_met)
  #local_met<- local_met[!local_met$nodes %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),] #don't look at the confluences
  local_met<- local_met[,c(1,2,4,5)] #only look at the centrality metrics 
  #local_met_m<- melt(local_met, id.vars=c('nodes'))
  #local_met_m$value<-as.numeric(local_met_m$value)
  
  new_data<- cbind(node_rem, local_met)
  active_lengthdf<- rbind(active_lengthdf, new_data)
}


all_active_lengthdf<- merge(active_lengthdf, local_met_orig, by=c('nodes'), all=T)
all_active_lengthdf$diff_from_real_betweenCentr<-as.numeric(all_active_lengthdf$betweenness.cent.x) - as.numeric(all_active_lengthdf$betweenness.cent.y) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_impcloseness<-as.numeric(all_active_lengthdf$imp.closeness.cent.x) - as.numeric(all_active_lengthdf$imp.closeness.cent.y) #round or else they both are off at the 18th decimal
all_active_lengthdf$diff_from_real_alphacent<-as.numeric(all_active_lengthdf$alpha.cent.x) - as.numeric(all_active_lengthdf$alpha.cent.y) #round or else they both are off at the 18th decimal

head(all_active_lengthdf)
#calculate the percent change
all_active_lengthdf$alpha_cent_percent_diff<- ((as.numeric(all_active_lengthdf$alpha.cent.x)/ as.numeric(all_active_lengthdf$alpha.cent.y))*100)-100
all_active_lengthdf$impclose_percent_diff<- ((as.numeric(all_active_lengthdf$imp.closeness.cent.x)/ as.numeric(all_active_lengthdf$imp.closeness.cent.y))*100)-100
all_active_lengthdf$between_percent_diff<- ((as.numeric(all_active_lengthdf$betweenness.cent.x)/ as.numeric(all_active_lengthdf$betweenness.cent.y))*100)-100

#average the change value
#average change for node removed- how much of an impact does that node have
cent_stats<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), mean, na.rm=T)
cent_statsmin<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), min, na.rm=T)
cent_statsmax<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$node_rem), max, na.rm=T)
cent_stats$stat<- 'mean';cent_statsmin$stat<-'min';cent_statsmax$stat<- 'max'
cent_stats_a <- rbind(cent_stats, cent_statsmax, cent_statsmin)

#average change when other nodes are removed- how sensitive is that node
cent_stats2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), mean, na.rm=T)
cent_statsmin2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), min, na.rm=T)
cent_statsmax2<- aggregate(all_active_lengthdf$diff_from_real_betweenCentr, by=list(all_active_lengthdf$nodes), max, na.rm=T)
cent_stats2$stat<- 'mean';cent_statsmin2$stat<-'min';cent_statsmax2$stat<- 'max'
cent_stats_b <- rbind(cent_stats2, cent_statsmax2, cent_statsmin2)

#plot centrality differences:trib entire network####
ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=impclose_percent_diff))+
  facet_grid(node_rem~., scales='free')+
  ylab('% change in closeness- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/centralitycloseness_change.png", width=12, height = 15)


ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=between_percent_diff))+
  facet_grid(node_rem~.)+
  ylab('% change in betweenness- node removed from network')+
  theme_bw()

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/centralitybetweenness_change.png", width=12, height = 15)


ggplot(data=all_active_lengthdf)+
  geom_col(aes(x=nodes, y=alpha_cent_percent_diff))+
  facet_grid(node_rem~., scales='free')+
  ylab('% change in alpha- node removed from network')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/centralityalpha_change.png", width=12, height = 15)

#senstivity of the node removed
ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=node_rem, y=diff_from_real_betweenCentr))

ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=node_rem, y=diff_from_real_impcloseness))

#how the node changes when others are removed - sensitivity of that node
ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=nodes, y=diff_from_real_betweenCentr))

ggplot(data=all_active_lengthdf)+
  geom_boxplot(aes(x=nodes, y=diff_from_real_impcloseness))

#change when that node is removed 
ggplot(data=cent_stats_a)+
  geom_point(aes(x=Group.1, y=x, color=stat))+
  geom_line(aes(x=Group.1, y=x, group = Group.1))+
  ggtitle("betweenness change when the node is removed")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/centralitybetweenness_change_value.png", width = 12)  

#how that node changes when other nodes are removed
ggplot(data=cent_stats_b)+
  geom_point(aes(x=Group.1, y=x, color=stat))+
  geom_line(aes(x=Group.1, y=x, group = Group.1))+
  ggtitle("betweenness change when other nodes are removed")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/centralitybetweenness_change_value_vs2.png", width=12)  

#*******************************************#
#global centrality changes entire network####
spatial.plot(kz_net, x,y,names, cex.text = 1)

####global metrics##
g <- global.summary(origgraph, sink = "kzSFM01")
g

g3 <-cbind(nodes=rownames(t(g)), t(g))
rownames(g3)<- 1:nrow(g3)
g3<-as.data.frame(g3)

#define stuff before loop
gnd<- kzarcs_lengths_2
n<- unique(c(gnd$from, gnd$to)) #all except  the sink. can't remove
#n<- n[9:35] #don't do the confluences
n
globalmetricsdf<- NULL
#i=8
for (i in 1:length(n)) {
  node_rem<- n[i]
  node_rem
  #newstring <- gsub(node_rem, "", orig_string)
  #newstring<- gsub("--+ ,", ",", newstring)
  newstring<- gsub(paste0("-- ", node_rem),"", orig_string)
  newstring<- gsub(paste0(node_rem, " --"),"", newstring)
  newstring<- gsub("--","--+", newstring)
  orig_string
  newstring
  newgraph<- eval(str2lang(sprintf("graph_from_literal(%s)", newstring)))
  newgraph
  spatial.plot(newgraph, x,y,names, cex.text = 1)
  #calculate the centrality index's
  g2<- global.summary(newgraph, sink='P1')
  cc<-complete.cases(g2)
  g2.cc<- g2[cc,]
  scaled.g2<- scale(t(g2.cc))
  g2_met <-cbind(nodes=rownames(t(g2.cc)), t(g2.cc))
  rownames(g2_met)<- 1:nrow(g2_met)
  g2_met<-as.data.frame(g2_met)
  #g2_met<- g2_met[!g2_met$nodes %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"),] #don't look at the confluences
  g2_met<- g2_met[,c(5)] #only look at the centrality metrics 
  #g2_met_m<- melt(g2_met, id.vars=c('nodes'))
  #g2_met_m$value<-as.numeric(g2_met_m$value)
  
  new_data<- cbind(node_rem, g2_met)
  globalmetricsdf<- rbind(globalmetricsdf, new_data)
}

all_globalmetricsdf<- as.data.frame(globalmetricsdf)
all_globalmetricsdf$orig_centr<- g3$Mean.a.centrality
all_globalmetricsdf$diff_from_real_betweenCentr<-as.numeric(all_globalmetricsdf$g2_met) - as.numeric(all_globalmetricsdf$orig_centr) #round or else they both are off at the 18th decimal

head(all_globalmetricsdf)

#calculate the percent change
all_globalmetricsdf$percent_diff<- ((as.numeric(all_globalmetricsdf$g2_met)/ as.numeric(all_globalmetricsdf$orig_centr))*100)-100

ggplot(data=all_globalmetricsdf)+
  geom_col(aes(x= node_rem, y=diff_from_real_betweenCentr))+
  theme_bw()+
  ggtitle("global change in centrality")

ggplot(data=all_globalmetricsdf)+
  geom_col(aes(x= node_rem, y=percent_diff))

#*********************************************#
#*********************************************#
#*track metrics through time####
# walk global.summary through node presence / absence data
kz_net<-graph_from_literal(kz02M11 --+ kz02M10 --+ kz02M09 --+ kz02M08 --+ kz02M07 --+ kz02M06 --+ kz02M05 --+ 
                                kz02M04 --+ kz02M03 --+ kz02M02 --+ kz02M01 --+ kzSFC01,
                              kz04M13 --+ kz04M12 --+ kz04C05 --+ kz04M11 --+ kz04C04 --+ kz04M10 --+ kz04M09 --+ kz04C03,
                              kz04T02 --+ kz04C03 --+ kz04M08 --+ kz04M07 --+ kz04C02,
                              kz04T01 --+ kz04C02 --+ kz04M06 --+ kz04M05 --+ kz04M04 --+ kz04M03 --+ kz04M02 --+ kz04C01,
                              kz04W04 --+ kz04W03 --+ kz04W02 --+ kz04W01 --+ kz04C01 --+ kz04M01 --+ kzSFC02,
                              kz01M06 --+ kz01M05 --+ kz01M04 --+ kz01M03 --+ kz01M02 --+ kz01M01 --+ kzSFC05,
                              kz20M05 --+ kz20M04 --+ kz20M03 --+ kz20M02 --+ kz20M01 --+ kzSFC05  --+ kzSFM07 --+ kzSFM06 --+ kzSFC04,
                              kzSFT02 --+ kzSFC04 --+ kzSFM05 --+ kzSFM04 --+ kzSFC03,
                              kzSFT01 --+ kzSFC03 --+ kzSFM03 --+ kzSFC02 --+ kzSFM02 --+ kzSFC01 --+ kzSFM01)

x<- kz_coords[,3] ; y <- kz_coords[,2]
names=kz_coords[,1]
names
spatial.plot(kz_net, x,y,names, cex.text = 1)

kz_node_pres_abs.nodate<-kz_node_pres_abs[,c(2:ncol(kz_node_pres_abs))]
#kz_node_pres_abs<-kz_node_pres_abs[,c(2:ncol(kz_node_pres_abs))]

icsl <- 1:nrow(kz_node_pres_abs) -> intact.to.sink -> a.cent -> harary -> global.eff
#rm(i)
for(i in 1:nrow(kz_node_pres_abs)){
  temp.graph <- delete.nodes.pa(kz_net, kz_node_pres_abs.nodate[i,])
  # replace direction symbol for igraph comparability
  namelv <- gsub("->", "|", kz_lengths[,1])
  a <- attributes(E(temp.graph))$vname
  a
  namelv
  w <- which(namelv %in% a)
  #length.sub<- kz_lengths$Lengths[!gsub("->","|", kz_lengths$Arcs) %in% a,]
  #length.sub<- kz_lengths$Lengths[!gsub("->","|", kz_lengths$Arcs) %in% a]
  length.sub <- kz_lengths[,2][w]
  E(temp.graph)$weights <- length.sub
  icsl[i] <- ICSL(temp.graph)
  global.eff <- global.efficiency(temp.graph)
  intact.to.sink[i] <- size.intact.to.sink(temp.graph, "kzSFM01")
  a.cent[i] <- mean(alpha_centrality(temp.graph), na.rm = T)
  harary[i] <- harary(temp.graph)
}
global <- cbind(icsl, global.eff, intact.to.sink, a.cent, harary)
global
# standardize measures
scaled.global <- scale(global)
par(mar = c(7,4.2,1.5,2))
# plot
matplot(scaled.global, xaxt = "n", type = "l", col = hcl.colors(5, palette = "spectral"),
        ylab = "Standardized global measures", lty = 1:2)
legend("topright", lty = 1:2, col = hcl.colors(5, palette = "spectral"),
       legend = c("ICSL", "global efficiency", "intact stream length to sink", "alpha-centrality", "Harary"), cex = .8)
axis(side = 1, at = c(1,21,41,61,81,100), labels = subset[,1][c(1,21,41,61,81,100)],
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 6)

#add info and save
colnames(scaled.global)<- c( "ICSL", "globalefficiency", "intactstreamlengthtosink", "alphacentrality", "Harary")
colnames(global)<- c( "ICSL", "globalefficiency", "intactstreamlengthtosink", "alphacentrality", "Harary")
#scaled.global<-scaled.global[,c(2:6)]
global<- cbind(kz_node_pres_abs[,1] ,as.data.frame(global))
scaled.global<- cbind(kz_node_pres_abs[,1] , as.data.frame(scaled.global))
colnames(global)[1]<-'datetime'
colnames(scaled.global)[1]<-'datetime'
global$datetime<- strftime(global$datetime)
scaled.global$datetime<-strftime(scaled.global$datetime)
write.csv(global, "H:/Documents/AIMS/R/excel/kz_sticmetrics/streamdag_out/kz_global_weighted.csv")
write.csv(scaled.global, "H:/Documents/AIMS/R/excel/kz_sticmetrics/streamdag_out/kz_globalscaled_weighted.csv")

ggplot(data=global)+
  geom_line(aes(x=datetime, y=alphacentrality))


####
#unweighted metrics
# walk global.summary through node presence / absence data
global <- matrix(ncol = 23, nrow = nrow(kz_node_pres_abs))
g<- rownames(global.summary(kz_net, sink = "kzSFM01"))
g
for(i in 1:nrow(kz_node_pres_abs)){
  global[i,] <- global.summary(delete.nodes.pa(kz_net, kz_node_pres_abs.nodate[i,]), sink = "kzSFM01")
}

# standardize measures
scaled.global <- scale(global)
par(mar = c(7,4.2,1.5,2))
scaled.global <- scale(global)

# plot
matplot(scaled.global, xaxt = "n", type = "l", lty = 1:5, col = hcl.colors(18, palette = "spectral"),
        ylab = "Standardized global measures")
legend("bottomright", lty = 1:5, col = hcl.colors(18, palette = "spectral"),
       legend = row.names(g), cex = .55)
axis(side = 1, at = c(1), labels = kz_node_pres_abs[,1][c(1)],
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 6)

#add info and save
colnames(scaled.global)<- g
colnames(global)<- g
global<- cbind(kz_node_pres_abs[,1] ,as.data.frame(global))
scaled.global<- cbind(kz_node_pres_abs[,1] , as.data.frame(scaled.global))
colnames(global)[1]<-'datetime'
colnames(scaled.global)[1]<-'datetime'
global$datetime<- strftime(global$datetime)
scaled.global$datetime<-strftime(scaled.global$datetime)
write.csv(global, "H:/Documents/AIMS/R/excel/kz_sticmetrics/streamdag_out/kz_global_unweight.csv")
write.csv(scaled.global, "H:/Documents/AIMS/R/excel/kz_sticmetrics/streamdag_out/kz_globalscaled_unweight.csv")

ggplot(data=global)+
  geom_line(aes(x=datetime, y=Mean.a.centrality))

# walk local.summary through node presence / absence data
local <- matrix(ncol = 17, nrow = nrow(kz_node_pres_abs))
local.summary(kz_net)
g<- rownames(local.summary(kz_net))
g
#rm(l)
#local characteristics through time but only save the outlet
for(i in 1:nrow(kz_node_pres_abs)){
  l <- local.summary(delete.nodes.pa(kz_net, kz_node_pres_abs.nodate[i,]))
  local[i,]<-l[,'kzSFM01']
}


# standardize measures
scaled.local <- scale(local)
par(mar = c(7,4.2,1.5,2))
scaled.local <- scale(local)

# plot
matplot(scaled.local, xaxt = "n", type = "l", lty = 1:5, col = hcl.colors(18, palette = "spectral"),
        ylab = "Standardized local measures")
legend("bottomright", lty = 1:5, col = hcl.colors(18, palette = "spectral"),
       legend = row.names(g), cex = .55)
axis(side = 1, at = c(1), labels = kz_node_pres_abs[,1][c(1)],
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 6)

#add info and save
colnames(scaled.local)<- g
colnames(local)<- g
local<- cbind(kz_node_pres_abs[,1] ,as.data.frame(local))
scaled.local<- cbind(kz_node_pres_abs[,1] , as.data.frame(scaled.local))
colnames(local)[1]<-'datetime'
colnames(scaled.local)[1]<-'datetime'
local$datetime<- strftime(local$datetime)
scaled.local$datetime<-strftime(scaled.local$datetime)
write.csv(local, "H:/Documents/AIMS/R/excel/kz_sticmetrics/streamdag_out/kz_local_unweight.csv")
write.csv(scaled.local, "H:/Documents/AIMS/R/excel/kz_sticmetrics/streamdag_out/kz_localscaled_unweight.csv")

ggplot(data=local)+
  geom_line(aes(x=datetime, y=local$alpha.cent))


#***********************************************#
#*figure out break points and what sites are at break points: N2B####
library(Rbeast)

N2B_stics<-c('datetime', 'kz02M04', 'kz02M05', 'kz02M06', 'kz02M07', 'kz02M08', 'kz02M09', 'kz02M10', 'kz02M11')

nodpa<- kz_node_pres_abs
nodpa<-nodpa[,grep(paste(N2B_stics,collapse="|"), names(nodpa))] #subst just those stics
str(nodpa)
nodpa$inst_flow_net<- (rowSums(nodpa[2:ncol(nodpa)], na.rm = T)/ (rowSums(!is.na(nodpa[,3:ncol(nodpa)])))) * 100 #only includes stics without NA. Also, make sure number columns match nodpa df
head(nodpa)
nodpa$datetime<- as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M") #will error if I used date_round vs datetime and vice versa
nodpa<- nodpa[order(nodpa$datetime),]

#plot
ggplot(data=nodpa)+
  geom_point(aes(x=datetime, y=inst_flow_net))+
  geom_line(aes(x=datetime, y=inst_flow_net))+
  xlab('Date')+
  ylab('Instantaneous Flowing Network Extent (%)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N2B/instnetworkN2B.png")

fit<- beast(nodpa$inst_flow_net, scp.minmax=c(0,50), tcp.minmax = c(0,50), method='bayes')
#fit2<- beast.irreg(nodpa$inst_flow_net,time=nodpa$date_round, season='dummy')
fit
print(fit)
#plot(fit,interactive=TRUE)
plot(fit)
fit$trend$cp #the location of changepoints from most possible to least
fit$trend$cpPr #the probabilities of the changepoints

chngepoints<- cbind(fit$trend$cp, fit$trend$cpPr)
chngepoints

changedates<- c(nodpa[chngepoints[1,1],1],
                nodpa[chngepoints[2,1],1],nodpa[chngepoints[3,1],1],nodpa[chngepoints[4,1],1], nodpa[chngepoints[5,1],1],
                nodpa[chngepoints[6,1],1])

changedates<-  as.data.frame(changedates)
changedates

#write.csv(changedates, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kzbreakpointsN2B.csv")

#pick out the dates that I want to focus on
#early (indicator of drydown, mid, late (dry down after the initial drydown))
brkpts<- changedates[c(2),] #
brkpts


#***********************************************#
#*figure out break points and what sites are at break points: N4D####
library(Rbeast)

N4D_stics<-c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
             'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13')

nodpa<- kz_node_pres_abs
nodpa<-nodpa[,grep(paste(N4D_stics,collapse="|"), names(nodpa))] #subst just those stics
str(nodpa)
nodpa$inst_flow_net<- (rowSums(nodpa[2:ncol(nodpa)], na.rm = T)/ (rowSums(!is.na(nodpa[,3:ncol(nodpa)])))) * 100 #only includes stics without NA. Also, make sure number columns match nodpa df
head(nodpa)
nodpa$datetime<- as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M") #will error if I used date_round vs datetime and vice versa
nodpa<- nodpa[order(nodpa$datetime),]

#plot
ggplot(data=nodpa)+
  geom_point(aes(x=datetime, y=inst_flow_net))+
  geom_line(aes(x=datetime, y=inst_flow_net))+
  xlab('Date')+
  ylab('Instantaneous Flowing Network Extent (%)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("H:/Documents/AIMS/R/figures/kz/stics/metrics/N4D/instnetworkN4D.png")

fit<- beast(nodpa$inst_flow_net, scp.minmax=c(0,10), tcp.minmax = c(0,10), method='bayes')
#fit2<- beast.irreg(nodpa$inst_flow_net,time=nodpa$date_round, season='dummy')
fit
print(fit)
#plot(fit,interactive=TRUE)
plot(fit)
fit$trend$cp #the location of changepoints from most possible to least
fit$trend$cpPr #the probabilities of the changepoints

chngepoints<- cbind(fit$trend$cp, fit$trend$cpPr)
chngepoints

changedates<- c(nodpa[chngepoints[1,1],1],
                nodpa[chngepoints[2,1],1],nodpa[chngepoints[3,1],1],nodpa[chngepoints[4,1],1], nodpa[chngepoints[5,1],1],
                nodpa[chngepoints[6,1],1],nodpa[chngepoints[7,1],1],nodpa[chngepoints[8,1],1],nodpa[chngepoints[9,1],1],
                nodpa[chngepoints[10,1],1])

changedates<-  as.data.frame(changedates)
changedates
#write.csv(changedates, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kzbreakpointsN4D.csv")

#pick out the dates that I want to focus on
#early (indicator of drydown, mid, late (dry down after the initial drydown))
brkpts<- changedates[c(5,2),] #N4D
brkpts



#***********************************************#
#*figure out break points and what sites are at break points: full watershed####
library(Rbeast)

nodpa<- kz_node_pres_abs
str(nodpa)
nodpa$inst_flow_net<- (rowSums(nodpa[2:ncol(nodpa)], na.rm = T)/ (rowSums(!is.na(nodpa[,3:ncol(nodpa)])))) * 100 #only includes stics without NA. Also, make sure number columns match nodpa df
head(nodpa)
nodpa$datetime<- as.POSIXct(nodpa$datetime, format="%m/%d/%Y %H:%M") #will error if I used date_round vs datetime and vice versa
nodpa<- nodpa[order(nodpa$datetime),]

#plot
ggplot(data=nodpa)+
  geom_point(aes(x=datetime, y=inst_flow_net))+
  geom_line(aes(x=datetime, y=inst_flow_net))+
  xlab('Date')+
  ylab('Instantaneous Flowing Network Extent (%)')+
  scale_x_datetime(date_breaks = '2 weeks')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

fit<- beast(nodpa$inst_flow_net, scp.minmax=c(0,10), tcp.minmax = c(0,10), method='bayes')
#fit2<- beast.irreg(nodpa$inst_flow_net,time=nodpa$date_round, season='dummy')
fit
print(fit)
#plot(fit,interactive=TRUE)
plot(fit)
fit$trend$cp #the location of changepoints from most possible to least
fit$trend$cpPr #the probabilities of the changepoints

chngepoints<- cbind(fit$trend$cp, fit$trend$cpPr)
chngepoints

changedates<- c(nodpa[chngepoints[1,1],1],
                nodpa[chngepoints[2,1],1],nodpa[chngepoints[3,1],1],nodpa[chngepoints[4,1],1], nodpa[chngepoints[5,1],1],
                nodpa[chngepoints[6,1],1],nodpa[chngepoints[7,1],1],nodpa[chngepoints[8,1],1],nodpa[chngepoints[9,1],1],
                nodpa[chngepoints[10,1],1])

changedates<-  as.data.frame(changedates)
changedates
write.csv(changedates, "H:/Documents/AIMS/R/excel/kz_sticmetrics/kzbreakpointsfull_wetup.csv")

#pick out the dates that I want to focus on
#early (indicator of drydown, mid, late (dry down after the initial drydown))
brkpts<- changedates[c(4),] 
brkpts

#**************************************************#
#which nodes are changing then####
firstdaynoflow<- read_csv("H:/Documents/AIMS/R/excel/kz_sticmetrics/kzfirtsdayrewet_for1FULLdays_2022.csv")

#N2B
firstdaynoflow<- firstdaynoflow[firstdaynoflow$Group.1 %in% c('datetime', 'kz02M04', 'kz02M05', 'kz02M06', 'kz02M07', 'kz02M08', 'kz02M09', 'kz02M10', 'kz02M11'),]

#N4D
firstdaynoflow<- firstdaynoflow[firstdaynoflow$Group.1 %in% c('datetime', 'kz04M03', 'kz04M04', 'kz04M05', 'kz04M06', 'kz04C02', 'kz04T01', 'kz04M07', 'kz04M08', 'kz04C03',
                                                              'kz04M09', 'kz04M10', 'kz04T02',  'kz04M11',  'kz04M12', 'kz04M13'),]

firstdaynoflow$timing<-  ifelse(firstdaynoflow$x< as.Date(brkpts[1]), "early", 'late') #need a greater than since the early spring is dry
head(firstdaynoflow)

#write.csv(firstdaynoflow, "H:/Documents/AIMS/R/excel/kz_sticmetrics/firstfulldaynoflow_withtimingN4D.csv")
#write.csv(firstdaynoflow, "H:/Documents/AIMS/R/excel/kz_sticmetrics/firstfulldaynoflow_withtimingN2B.csv")
#write.csv(firstdaynoflow, "H:/Documents/AIMS/R/excel/kz_sticmetrics/firstfulldaynoflow_withtiming.csv")
#write.csv(firstdaynoflow, "H:/Documents/AIMS/R/excel/kz_sticmetrics/firstfulldayrewet_withtiming.csv")

