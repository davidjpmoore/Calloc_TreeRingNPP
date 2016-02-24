# Query PEcAn database to generate aggregated allometrics

# This needs to be a local directory which houses the Pecan scripts to be used. 
setwd("pecan.allometry") 

# The Designated directory where the resulting allometries will be stored
outdir <- "../allometries"

#File Path of allometry database
parm.path <- "data/Table3_GTR-NE-319.v2_RossAdendum4.csv"
#parm.path <- "data/Table3_GTR-NE-319.v2_RossAdendum4.csv"
#allometry file needs to be replaced with Ross's custom file 
#should the main PECAN allometry file be updated with Ross's additions?

# Files listed as 'Chojnacky' in the comments have been manually added by Ross Alexander with permission by Dr. Chojnacky.  These equations do not appear in the standard 'Table3_GTR-NE-319.v2.csv' file.  Dr. Chojnacky asked that these equations not be made public at this time.  

#Pecan Scripts to be used
source("R/AllomAve.R")
source("R/query.allom.data.R")
source("R/allom.BayesFit.R")
source("R/read.allom.data.R")

# ----------------------------------
# Generating Allometries
# ----------------------------------

# pfts           = local object which contains a list of species to be used.  Use USFS acronyms and species codes
# AllomAve       = Function to calc. new allometries
#                  Needs:
#                  pfts   = designated list of species to query
#                  2      = component (whole aboveground biomass) from Jenkins Table 5
#                  outdir = directory to house results
#                  parm   = database file
#                  ngibbs = number of iterations
#                  dmin   = minimum diameter
#                  dmax   = maximum diameter

pfts = list(east.hard = data.frame(spcd=1000,acronym="EHW"),
			FAGR = data.frame(spcd=531,acronym="FAGR"), 
            LITU = data.frame(spcd=621,acronym="LITU"),
            QUAL = data.frame(spcd=802,acronym="QUAL"),
            QURU= data.frame(spcd=833,acronym="QURU"),
            POGR = data.frame(spcd=743,acronym="POGR"),
            ACSA = data.frame(spcd=318,acronym="ACSA"),
            ACRU = data.frame(spcd=316,acronym="ACRU"),
            PIST = data.frame(spcd=129,acronym="PIST"),
            BEPA = data.frame(spcd=375,acronym="BEPA"),
            FRAM = data.frame(spcd=541,acronym="FRAM"),
            NYSY = data.frame(spcd=693,acronym="NYSY"),
            ASTR = data.frame(spcd=367,acronym="ASTR"))
            
AllomAve(pfts,2,outdir= file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)


#TIAM Runs
pfts = list(TIAM = data.frame(spcd=951,acronym="TIAM"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=1, dmax=500)

#SAAL Runs
pfts = list(SAAL = data.frame(spcd=951,acronym="SAAL"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=1, dmax=500)

#ULRU Runs
pfts = list(ULRU = data.frame(spcd=951,acronym="ULRU"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=1, dmax=500)


#PIPO runs with Tyson and Chojnacky equations manually added by Ross
pfts = list(PIPO = data.frame(spcd=122,acronym="PIPO"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# General Fir from Chojnacky
pfts = list(ABIES = data.frame(spcd=10,acronym="ABIES"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# General pine from Chojnacky
pfts = list(PINUS = data.frame(spcd=100,acronym="PINUS"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# General spruce from Chojnacky
pfts = list(PICEA = data.frame(spcd=90,acronym="PICEA"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# General doug fir from Chojnacky
pfts = list(PSME = data.frame(spcd=202,acronym="PSME"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

#ABCO runs
pfts = list(ABCO = data.frame(spcd=15,acronym="ABCO"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

#POTR
pfts = list(POTR = data.frame(spcd=746,acronym="POTR"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

#QUVE
pfts = list(QUVE = data.frame(spcd=837,acronym="QUVE"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

#Silver Maple
pfts = list(ACSA2 = data.frame(spcd=318,acronym="ACSA2"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

#Carya Sp.
pfts = list(CARYA = data.frame(spcd=400,acronym="CARYA"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# # Paw Paw (ASTR)--NO EQUATION
# pfts = list(ASTR = data.frame(spcd=367,acronym="ASTR"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10, dmin=6, dmax=500)

# Ulmus sp.--NOEQUATION
# pfts = list(ULMUS = data.frame(spcd=970,acronym="ULMUS"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10, dmin=6, dmax=500)

# Frax sp.--NO EQUATION
# pfts = list(FRAXI = data.frame(spcd=540,acronym="FRAXI"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10, dmin=6, dmax=500)

# # JUVI--NO Equation; use THUJA OCCIDENTALIS
pfts = list(JUVI = data.frame(spcd=241,acronym="JUVI"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# LIST
pfts = list(LIST2 = data.frame(spcd=611,acronym="LIST"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# Oak sp.
pfts = list(QUERC = data.frame(spcd=800,acronym="QUERC"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# PITA
pfts = list(PITA = data.frame(spcd=131,acronym="PITA"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# PRSE
pfts = list(PRSE = data.frame(spcd=762,acronym="PRSE"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000 dmin=6, dmax=500)

# # QUBI-- NO EQUATION
# pfts = list(QUBI = data.frame(spcd=804,acronym="QUBI"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10, dmin=6, dmax=500)

# QUMU--NO EQUATION
pfts = list(QUMU = data.frame(spcd=826,acronym="QUMU"))
AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# QUPH--NO EQUATION
# pfts = list(QUPH = data.frame(spcd=831,acronym="QUPH"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10, dmin=6, dmax=500)

# ULAL--NO EQUATION
# pfts = list(ULAL = data.frame(spcd=971,acronym="ULAL"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10, dmin=6, dmax=500)


# ULRU--NO EQUATION
# pfts = list(ULRU = data.frame(spcd=975,acronym="ULRU"))
# AllomAve(pfts,2,outdir=file.path(outdir),parm=parm.path,ngibbs=10000, dmin=6, dmax=500)



##########################################################
#making up our own PFT's, just need a species
#recieved file from 
##########################################################
# This is a table with Species & PFT assignments from Simon
pft.db <- read.csv("../raw_input_files/FIA_conversion_v0.2.csv", header=T)


##########################################################
#Early hardwood
early.hardwood<- pft.db[pft.db$pft=="EH", c("acronym", "spcd")]

pfts = list(early.hardwood = data.frame(spcd=early.hardwood$spcd,acronym=early.hardwood$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

##########################################################
#Southern Mid hardwood
s.mid.hardwood<- pft.db[pft.db$pft=="SMH", c("acronym", "spcd")]

pfts = list(s.mid.hardwood = data.frame(spcd=s.mid.hardwood$spcd,acronym=s.mid.hardwood$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=0.1, dmax=500)

##########################################################
#Northern Mid hardwood
n.mid.hardwood<- pft.db[pft.db$pft=="NMH", c("acronym", "spcd")]

pfts = list(n.mid.hardwood = data.frame(spcd=n.mid.hardwood$spcd,acronym=n.mid.hardwood$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=0.1, dmax=500)

##########################################################
#Late hardwood
late.hardwood<- pft.db[pft.db$pft=="LH", c("acronym", "spcd")]

pfts = list(late.hardwood = data.frame(spcd=late.hardwood$spcd,acronym=late.hardwood$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=0.1, dmax=500)

##########################################################
#Mid Conifer
mid.con<- pft.db[pft.db$pft=="MC", c("acronym", "spcd")]

pfts = list(mid.con = data.frame(spcd=mid.con$spcd,acronym=mid.con$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

##########################################################
#Late Conifer
late.con<- pft.db[pft.db$pft=="LC", c("acronym", "spcd")]

pfts = list(late.con = data.frame(spcd=late.con$spcd,acronym=late.con$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

##########################################################
#Northern Pine
n.pine<- pft.db[pft.db$pft=="NP", c("acronym", "spcd")]

pfts = list(n.pine = data.frame(spcd=n.pine$spcd,acronym=n.pine$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

##########################################################
#Southern Pine
s.pine<- pft.db[pft.db$pft=="NP", c("acronym", "spcd")]

pfts = list(s.pine = data.frame(spcd=s.pine$spcd,acronym=s.pine$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)


#################################################################
# Making CLM PFT's from the cross-walk
#--------------------------------------

# Broadleaf deciduous
broad.decid <- pft.db[pft.db$CLM=="BLD", c("acronym", "spcd")]

pfts = list(broad.decid = data.frame(spcd=broad.decid$spcd, acronym = broad.decid$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)

# Coniferous Evergreen
con.eg <- pft.db[pft.db$CLM=="CEG", c("acronym", "spcd")]

pfts = list(con.eg = data.frame(spcd = con.eg$spcd, acronym = con.eg$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)


# Broadleaf Evergreen
broad.eg <- pft.db[pft.db$CLM=="BLE", c("acronym", "spcd")]

pfts = list(broad.eg = data.frame(spcd = broad.eg$spcd, acronym = broad.eg$acronym))
AllomAve(pfts,2,outdir=outdir,parm=parm.path,ngibbs=10000, dmin=6, dmax=500)



