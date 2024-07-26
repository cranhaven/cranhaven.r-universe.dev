# changeRangeR: An R package for reproducible biodiversity change metrics
# from species distribution estimates.
#
# PD_from_rasters.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the changeRangeR R package.
#
# changeRangeR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# changeRangeR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with changeRangeR. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
# ######Phylogenetic diversity from distribution models in tif format and phylogenies in nexus format######
# ###This script applies to any distribution map in raster format changing only the extension type in line 52###
# #############Last updated January 2014 by Andrea Paz#################
# #############Last updated October 2023 by BAJ#################
#
#
# #Clean workspace
# rm(list=ls())
#
# #Load required packages
# library(raster)
# library(sf)
# library(picante)
# #####Function to load and crop rasters######
#
# funcion1=function (x, mask=selected_mask,factor=resolution) {
#   capa=raster(x)
#   capa1=aggregate(capa,factor,fun=max)
#   capa2=crop(capa1,extent(mask))
#   return(capa2)
# }
#
# ########################################USER DEFINED VARIABLES#########################################
# ######Prompt user to determine location of files and selected geographic areas, resolution etc.########
# #######################################################################################################
#
#
#   {print("Remember phylogeny file must be stored in working directory and named phylogeny.nex")
#
#   # set working directory
#   print("Choose working directory for storage:")
#   working_directory<-choose.dir()
#   setwd(working_directory)
#
#   #Select folder containing distribution maps
#   print("Select folder containing distribution maps")
#   maps_folder<-choose.dir()
#
#   #File containing shape of the area to be analyzed (can be a country, province, ecosystem etc.). It will be treated as a mask
#    print("Select working mask (i.e Colombia)")
#    working_mask<-file.choose()
#
#   #Determine resolution as aggregation factor
#   resolution1<-readline("Aggregation factor for model (Integer): ")
#    resolution<-as.numeric(resolution1)
#   #Determine names for outputs
#   PD_map<-readline("Name of phylogenetic diversity raster: Do not use spaces and must include .asc extention (Ex: PD_country.asc): ")
#   species_richness<-readline("Name of the species richness map: Do not use spaces and must include .asc extention  (Ex: Species_richness_country.asc): ")
#
#   #Read distribution files and obtain species names for phylogeny
#
#   distribution_files<-list.files(path=maps_folder, pattern= "*.tif$")
#   species_names<-sub("_10p_cut.tif","",distribution_files)
#   table1<-as.data.frame(species_names)
#   colnames(table1)<-"Grid"
#   write.table(table1,"species_list.txt",quote=F,row.names=F) #seria bueno que agregara el nombre de la mascara para saber si es colombia o paramo
#
#   #Read Shape of the geographic Mask
#   selected_mask<-readShapePoly(working_mask)
#
# #########################
# ####START ANALYSES#######
# #########################
#    setwd(maps_folder)
# #Create empty raster of the desired area and resolution to assign pixel numbers
# r<-raster(distribution_files[1])
# r=aggregate(r,resolution,fun=max)
# r<-crop(r,extent(selected_mask))
# grid=r
# names(grid)="grid"
# grid[1:ncell(grid)]<-1:ncell(grid)
# grid_list<-list(grid)
# names(grid_list)<-"Grid"
# #Load distribution files, aggregate and crop to Colombia extent
#
#    layers<-list()
#    for (i in 1:length(distribution_files))
#    {
#     layers[i]<-funcion1(distribution_files[i])
#     print(i)
#    }
# #layers<-lapply(distribution_files,funcion1)
#
#
# #assign species names to layers with species distributions
# names(layers)<-as.vector(table1$Grid)
# layers[sapply(layers,is.null)]<-NULL
#
# #Combine distributions with pixel number raster
# complete_list<-c(grid_list,layers)
#
# #Stack all distribution files and pixel numbers for PD computation
# Stack<-stack(complete_list)
#
# #Convert to data frame for storage and posterior analysis
#
# community_data<-as.data.frame(Stack)
#
# #Store
#    setwd(working_directory)
# write.table(community_data, file = "communities.txt", append = FALSE,row.names=F,quote=F,sep="\t") #tratar de agregar nombre de mascara
#
#
# #Generate a raster of taxonomic diversity (TD) for comparison
# all_species<-stack(layers)
# TD<-calc(all_species,sum)
# writeRaster(TD,species_richness)
# }
#
# #####This part when the phylogeny is ready#####
#
# #Phylogenetic diversity computation
#
# #Read phylogeny file in nexus format
#   ##To read a phylogeny in newick format use read.tree instead of read.nexus
#       #In working directory always a file with the same name: phylogeny.nex
#
#   user_phylogeny=read.nexus("phylogeny.nex")
#
#
# #In the community data frame NA must be eliminated
# community_data=na.omit(community_data)
# head(community_data)
#
# #III-Phylogenetic diversity computation
#   #computes only Faith�s PD others may be added
#
# pd.result <-pd(community_data[,2:ncol(community_data)],user_phylogeny,include.root = TRUE)
#
#
# #Add the pixel PD value to data frame
# community_data$pd<-pd.result[[1]]
#
# #Write the new matrix to a file to avoid rerunning the script for potential further analyses
# write.table(community_data, file = "communities_and_pd.txt", append = FALSE,row.names=F,quote=F,sep="\t")
#
# #IV-Generate a raster containing PD information per pixel
#
# #1-First generate an empty raster using a base model for resolution and area
#
# r<-raster(ncol=1462,nrow=624)
# res(r)<-resolution
# r<-crop(r,extent(selected_mask))
# values(r)<-0
# map<-readOGR(dsn=maps_folder,layer=species_names[[1]]) #this will need to be replaced with st_read() BAJ
# r<-rasterize(map,r,map$PRESENCE,update=T)
# r<-mask(r,selected_mask)
# pd_ras<-r
# values(pd_ras)<-NA #Eliminate every value they will be replaced by PD values further down
#
#
# #2- Assign PD values to raster
# pd_ras[community_data$Grid]<-community_data$pd
#
# #3- Save raster to file
#
# writeRaster(pd_ras,PD_map)
#
# #4-Optional plotting map in R (write to pdf?)
# plot(pd_ras)
