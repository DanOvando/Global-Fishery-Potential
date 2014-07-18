# ######################################
# #Maid Service--------------------------------------------------
# # This code performs a number of cleaning and processing steps on the database 
# ######################################
# 
# MaidService<- function(Data,TrimCatch,InterpolateCatch,RemoveStocks)
# {
#   Data<- RAM
#   
#   TrimCatch<- 1
#   
#   InterpolateCatch<- 0
#   
#   RemoveStocks<- FALSE
#     
# Fisheries<- unique(Data[,IdVar])
# 
# if (TrimCatch==T)
# {
#   for (f in 1:length(Fisheries))
#   {
#     Where<- Data[,IdVar]==Fisheries[f]
#     
#     FirstCatch<- which( Where==T & is.na(Data$Catch)==F)
#   }
#   
# }
#   
#   
#   
# }