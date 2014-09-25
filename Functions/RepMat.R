RepMat<- function(Data,Reps,Direction)
{
  # RepMat: A function to create repeated entries of a dataframe or matrix ---------------
  # Data is the row(s) or column(s) that you want to replice. 
  # Reps is the number of times you want it replicated
  # Direction indicates whether you want to rep rows or columns
  
  DataToRep<- Data
  
#   Reps<- Reps-
  if (Reps>1)
  {
    Reps<- Reps-1
  if (Direction=='Rows')
  {
    
    for (r in 1:Reps)
    {
      Data<- rbind(Data,DataToRep)
      
    }
    
  }
  if (Direction=='Columns')
  {
    
    for (r in 1:Reps)
    {
      Data<- cbind(Data,DataToRep)
      
    }    
  }
  }
  return(Data)
}


