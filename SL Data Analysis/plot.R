#
# Function to plot data.
#


PlotShoppingListData <- function(TransformedDb, x_axis, GroupBy, PlotType) {
        

  if(PlotType == "a") {    
            
    ggplot(data = TransformedDb) + 
        geom_bar(mapping = aes(x = x_axis))
            
    } else if (PlotType == "b") { 
      
        ggplot(data = TransformedDb) + 
            geom_bar(mapping = aes(x = x_axis, fill = GroupBy), 
                     position = "dodge")
                    
              } else { 
                       ggplot(data = TransformedDb) + 
                          geom_bar(mapping = aes(x = x_axis, fill = GroupBy), 
                                   position = "fill")
                     }

}

