library(NPSForVeg)
library(dplyr)

#IMPORT DATA
NCRN<-importNCRN("T:/I&M/MONITORING/FOREST_VEGETATION/08_DATA/2019/Exports/20190926")
ANTI<-NCRN[[1]]
CATO<-NCRN[[2]]
CHOH<-NCRN[[3]]
GWMP<-NCRN[[4]]
HAFE<-NCRN[[5]]
MANA<-NCRN[[6]]
MONO<-NCRN[[7]]
NACE<-NCRN[[8]]
PRWI<-NCRN[[9]]
ROCR<-NCRN[[10]]
WOTR<-NCRN[[11]]
Tags<-read.csv("T:/I&M/MONITORING/FOREST_VEGETATION/08_DATA/2019/Exports/20190926/Tag_History.csv", header=T, as.is=T)

#UNTIL THE FULL CYCLE HAS BEEN COMPLETED, SOME ANALYSES SHOULD BE...
#limited to plots that were sampled in 2015 AND have been re-sampled in 2019
Plots2015<-getPlots(NCRN, years=c(2015))
Plots2015<-as.vector(Plots2015$Plot_Name)
Plots2019<-getPlots(NCRN, years=c(2019))
Plots2019<-as.vector(Plots2019$Plot_Name)
Plots<-intersect(Plots2015,Plots2019)


#IDENTIFY TREES WITH EQUIV DBH<10
trees2019<-getPlants(NCRN, "trees", years=c(2019))	
	trees2019<-dplyr::rename(trees2019, Year2=Sample_Year, Date2=Date, Stems2=Stems, DBH2=Equiv_Live_DBH_cm, BA2=SumLiveBasalArea_cm2,Status2=Status) 
trees2019toosmall<-dplyr::filter(trees2019, DBH2<10)
write.csv(trees2019toosmall, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019toosmall.csv")

#####IDENTIFY SUSPICIOUS TREE DBH CHANGES
trees2015<-getPlants(NCRN, "trees", years=c(2015))
	trees2015<-dplyr::rename(trees2015, Year1=Sample_Year, Date1=Date, Stems1=Stems, DBH1=Equiv_Live_DBH_cm, BA1=SumLiveBasalArea_cm2,Status1=Status) 
change<-dplyr::full_join(trees2015,trees2019, by=c("Unit_Code","Plot_Name", "Tag","Latin_Name"))  %>%
  mutate(DBHch=(DBH2-DBH1)/4, DBHchange=DBH2-DBH1, BAChange=(BA2-BA1)/4) 
BigChange<-dplyr::filter(change, DBHchange > 3 | DBHchange < (-3))
BigChange<-dplyr::select(BigChange, Plot_Name, Tag, Year1, DBH1, Status1, Year2, Date2, DBH2, Status2, Crown_Description.y, DBHchange)
write.csv(BigChange, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019DBHTreechanges.csv")

#####SUSPICIOUS SAPLING DBH CHANGES
saps2015<-getPlants(NCRN, "saplings", "all", years=c(2015))
	saps2015<-dplyr::rename(saps2015, Year1=Sample_Year, Date1=Date, DBH1=Equiv_Live_DBH_cm, BA1=SumLiveBasalArea_cm2,Status1=Status) 
saps2019<-getPlants(NCRN, "saplings", "all", years=c(2019))	
	saps2019<-dplyr::rename(saps2019, Year2=Sample_Year, Date2=Date,DBH2=Equiv_Live_DBH_cm, BA2=SumLiveBasalArea_cm2,Status2=Status) 
Sapchange<-dplyr::full_join(saps2015,saps2019, by=c("Unit_Code","Plot_Name", "Tag","Latin_Name"))  %>%
  mutate(DBHch=(DBH2-DBH1)/4, DBHchange=DBH2-DBH1, BAChange=(BA2-BA1)/4) 
SapBigChange<-dplyr::filter(Sapchange, DBHchange > 3 | DBHchange < (-2))
SapBigChange<-dplyr::select(SapBigChange, Plot_Name, Tag, Latin_Name, Year1, DBH1, Status1, Year2, DBH2, Status2, DBHchange)
write.csv(SapBigChange, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019DBHSapchanges.csv")


#####IDENTIFY ERRONIOUS OR UNLIKELY CHANGES IN STATUS
trees2015<-getPlants(NCRN, "trees", "all", years=c(2015))
  trees2015<-dplyr::rename(trees2015, Year1=Sample_Year, Date1=Date, Stems1=Stems, DBH1=Equiv_Live_DBH_cm, BA1=SumLiveBasalArea_cm2,Status1=Status) 
trees2019<-getPlants(NCRN, "trees", "all", years=c(2019))	
trees2019<-dplyr::rename(trees2019, Year2=Sample_Year, Date2=Date, Stems2=Stems, DBH2=Equiv_Live_DBH_cm, BA2=SumLiveBasalArea_cm2,Status2=Status) 
change<-dplyr::full_join(trees2015, trees2019, by=c("Unit_Code","Plot_Name", "Tag","Latin_Name")) 

OddStatusChanges<-dplyr::filter(change, Status1=="Dead Standing" & Status2=="Alive Standing"|
					Status1=="Dead Standing" & Status2=="Missing - Presumed Dead"|
					Status1=="Dead Standing" & Status2=="Alive Leaning"|
					Status1=="Dead Standing" & Status2=="Alive Broken"|
					Status1=="Dead Fallen" & Status2=="Alive Standing"|
					Status1=="Dead Fallen" & Status2=="Missing - Presumed Dead"|
					Status1=="Dead Fallen" & Status2=="Alive Leaning"|
					Status1=="Dead Fallen" & Status2=="Alive Broken"|
					Status1=="Dead Fallen" & Status2=="Dead Standing"|
					Status1=="Dead Fallen" & Status2=="Dead Fallen"|
					Status1=="Downgraded to Non-Sampled" & Status2=="Dead Standing"|
					Status1=="Missing - Presumed Dead" & Status2=="Alive Standing"|
					Status1=="Missing - Presumed Dead" & Status2=="Alive Fallen"|
					Status1=="Missing - Presumed Dead" & Status2=="Alive Leaning"|
					Status1=="Missing - Presumed Dead" & Status2=="Alive Broken")
write.csv(OddStatusChanges, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019OddStatusChanges_Trees.csv")


####WRITE THIS CODE FOR SAPLINGS -- EXPORT TABLES ARE CURRENTLY MISSING MANY SAPLINGS (THOSE WITHOUT HABIT FIELD FILLED IN)
OddSapStatusChanges<-dplyr::filter(Sapchange, Status1=="Dead Standing" & Status2=="Alive Standing"|
                                  Status1=="Dead Standing" & Status2=="Missing - Presumed Dead"|
                                  Status1=="Dead Standing" & Status2=="Alive Leaning"|
                                  Status1=="Dead Standing" & Status2=="Alive Broken"|
                                  Status1=="Dead Fallen" & Status2=="Alive Standing"|
                                  Status1=="Dead Fallen" & Status2=="Missing - Presumed Dead"|
                                  Status1=="Dead Fallen" & Status2=="Alive Leaning"|
                                  Status1=="Dead Fallen" & Status2=="Alive Broken"|
                                  Status1=="Dead Fallen" & Status2=="Dead Standing"|
                                  Status1=="Dead Fallen" & Status2=="Dead Fallen"|
                                  Status1=="Downgraded to Non-Sampled" & Status2=="Dead Standing"|
                                  Status1=="Missing - Presumed Dead" & Status2=="Alive Standing"|
                                  Status1=="Missing - Presumed Dead" & Status2=="Alive Fallen"|
                                  Status1=="Missing - Presumed Dead" & Status2=="Alive Leaning"|
                                  Status1=="Missing - Presumed Dead" & Status2=="Alive Broken")
write.csv(OddSapStatusChanges, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019OddStatusChanges_Saplings.csv")


#####IDENTIFY PLOTS WHERE FOREST PESTS OR PATHOGENS HAVE BEEN DETECTED?


#####IDENTIFY SUSPICIOUS VINE CHANGES, BY TREE
vines2015<-getPlants(NCRN, "vines", years=c(2015))
vines2015<-dplyr::filter(vines2015, Tag_Status=="Tree")
	vinesbytree2015<-aggregate(Latin_Name~Host_Tag, data=vines2015, length)
	vinesbytree2015<-dplyr::rename(vinesbytree2015, VineCt2015=Latin_Name)
vines2019<-getPlants(NCRN, "vines", years=c(2019))
vines2019<-dplyr::filter(vines2019, Tag_Status=="Tree")
	vinesbytree2019<-aggregate(Latin_Name~Host_Tag, data=vines2019, length)
	vinesbytree2019<-dplyr::rename(vinesbytree2019, VineCt2019=Latin_Name)
QCVinesByTree<-merge(vinesbytree2015, vinesbytree2019, all=T)
	TagInfo<-dplyr::select(Tags, Host_Tag=Tag, Plot_Name=Plot_Name, Year=Latest_Event)
	QCVinesByTree<-merge(QCVinesByTree, TagInfo, all.x=T)
	QCVinesByTree[is.na(QCVinesByTree)]<-0
#limit to Plots that have sampled in 2015 AND 2019
	QCVinesByTree<-filter(QCVinesByTree, Plot_Name %in% Plots)
	QCVinesByTree$Change<-QCVinesByTree$VineCt2019-QCVinesByTree$VineCt2015
	QCVinesByTree<-filter(QCVinesByTree, Change !=0)
#only include trees in this list that were alive in 2019 (dead trees won't have any vine record)
	alivetrees<-getPlants(NCRN, "trees", status="alive", years=c(2019))
	QCVinesByTree<-filter(QCVinesByTree, Host_Tag %in% alivetrees$Tag)
write.csv(QCVinesByTree, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019QCVinesByTree.csv")


#IDENTIFY SUSPICIOUS VINE CHANGES, BY PLOT
vinesbyplot2015<-aggregate(Latin_Name~Plot_Name, data=vines2015, length)
	vinesbyplot2015<-dplyr::rename(vinesbyplot2015, PlotVineCt2015=Latin_Name)
vinesbyplot2019<-aggregate(Latin_Name~Plot_Name, data=vines2019, length)
	vinesbyplot2019<-dplyr::rename(vinesbyplot2019, PlotVineCt2019=Latin_Name)
QCVinesByPlot<-merge(vinesbyplot2015, vinesbyplot2019, all=T)
#limit to Plots that have sampled in 2015 AND 2019
  QCVinesByPlot<-filter(QCVinesByPlot, Plot_Name %in% Plots)
#replace NAs with 0
  QCVinesByPlot[is.na(QCVinesByPlot)]<-0
  QCVinesByPlot$Change<-QCVinesByPlot$PlotVineCt2019-QCVinesByPlot$PlotVineCt2015
#add count of living trees in 2015 and 2019
  alive2019<-aggregate(Tag~Plot_Name, alivetrees, length)
  alive2019<-rename(alive2019, LivingTrees2019=Tag)
  alivetrees2015<-getPlants(NCRN, "trees", status="alive", years=c(2015))
  alive2015<-aggregate(Tag~Plot_Name, alivetrees2015, length)
  alive2015<-rename(alive2015, LivingTrees2015=Tag)
  QCVinesByPlot<-merge(QCVinesByPlot, alive2015)
  QCVinesByPlot<-merge(QCVinesByPlot, alive2019)
write.csv(QCVinesByPlot, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019QCVinesByPlot.csv")


#####CREATE "NEW DETECTION" LIST -- PLOTS WHERE TARGETED SPECIES WERE NEWLY DETECTED



#####LARGE CHANGES IN PLOT-LEVEL COVER?
herbs2015<-getPlants(NCRN, "herbs", years=c(2015))
  herbs2015<-aggregate(Percent_Cover~ Latin_Name + Plot_Name, data=herbs2015, sum)
  herbs2015<-dplyr::rename(herbs2015, TotalPlotCover2015=Percent_Cover)
herbs2019<-getPlants(NCRN, "herbs", years=c(2019))
  herbs2019<-aggregate(Percent_Cover~ Latin_Name + Plot_Name, data=herbs2019, sum)
  herbs2019<-dplyr::rename(herbs2019, TotalPlotCover2019=Percent_Cover)
HerbChanges<-merge(herbs2015, herbs2019, all=T)
#limit to Plots that have sampled in 2015 AND 2019
  HerbChanges<-filter(HerbChanges, Plot_Name %in% Plots)
#replace NAs with 0
  HerbChanges[is.na(HerbChanges)]<-0
#percent change
  HerbChanges$PercChange<-(HerbChanges$TotalPlotCover2019-HerbChanges$TotalPlotCover2015)/HerbChanges$TotalPlotCover2015
#identify plots where there was a large change in total cover for targeted herbs
  BigHerbChanges<-filter(HerbChanges, PercChange>.7 | PercChange<(.7*-1))    #NOT SURE HOW USEFUL THIS REALLY IS
#write csv for this table...
  #write.csv(BigHerbChanges, "T:/I&M/MONITORING/Forest_Vegetation/QAP/QAQC tables/2019/2019LargeChangeInTotalCover.csv") 
#identify plots where new targetted herbs were detected
  NewDetection<-filter(HerbChanges, PercChange==Inf)
  write.csv(NewDetection, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019NewHerbDetections.csv")
#identify plots where targetted herbs no longer detected
  NoLongerPresent<-filter(HerbChanges, PercChange==-1)
  write.csv(NoLongerPresent, "T:/I&M/MONITORING/FOREST_VEGETATION/10_QA/DATA_REVIEW/2019/2019HerbsNoLongerPresent.csv")
  
  