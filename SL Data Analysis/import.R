#
# Logic to import and connect data tables.
#

# Import Csv Files, Parse and Save Rds Files-----------------------------

  Get_Consumers_Table <- function() {
    
    Consumers_Table <- 
    read_csv("data/Consumers.csv", 
             col_names = c("UserID", "InstallDate"),
             col_types = cols(
               UserID = col_character(),
               InstallDate = col_datetime()
             )
    )
    # Remove the extra characters from UserID: ObjectId(57fa2639e4b0d16eaea8ee92)
    Consumers_Table$UserID <-  str_sub(Consumers_Table$UserID, 10, -2) 
    
    as_tibble(Consumers_Table)
  }
  
  Get_ShoppingList_Table <- function() {
    
    ShoppingList_Table <- 
    read_csv("data/shoppingList.csv", 
             col_names = c("ListID", "UserID", "CreationDate", "EditDate", "OrderStatus"),
             col_types = cols(
               ListID = col_character(),
               UserID = col_character(),
               CreationDate = col_datetime(),
               EditDate = col_datetime(),
               OrderStatus = col_factor(levels = NULL)
             )
    )
  
    # Remove the extra characters from ListID: ObjectId(57fa2639e4b0d16eaea8ee92)
    # Here we do not require str_sum() for UserId because it is in proper format.
    ShoppingList_Table$ListID <-  str_sub(ShoppingList_Table$ListID, 10, -2) 
    
    as_tibble(ShoppingList_Table)
  }
  
  Get_listUserMapping_Table <- function() {
    
    listUserMapping_Table <- 
    read_csv("data/listUserMapping.csv",
             na = "null",
             col_names = c("UserID", "ListID", "Owner"),
             col_types = cols(
               UserID = col_character(),
               ListID = col_character(),
               Owner = col_logical()
             )
    )
    
    as_tibble(listUserMapping_Table)
  }
  
  Get_listItems_Table <- function() {
    
    listItems_Table <- 
    read_csv("data/listItems.csv",
             col_names = c("ItemID", "ListID", "Quantity", "UnitMeasure"),
             col_types = cols(
               ItemID = col_character(),
               ListID = col_character(),
               Quantity = col_double(),
               UnitMeasure = col_character()
             )
    )
  
    # Remove the extra characters from ItemID: ObjectId(57fa2639ddwa8ee92)
    listItems_Table$ItemID <- str_sub(listItems_Table$ItemID, 10, -2)
    
    as_tibble(listItems_Table)
  }

# Join tables to create rawDb--------------------------------------------
  
JoinTables_ToCreate_UserCountComparison_RawDb <- function() {
 
  # Separating with and without UserID data 
  
  ShoppingList_Without_UserID_Table <- 
    
    Get_ShoppingList_Table() %>%
    
    filter(is.na(UserID)) %>%

    select(-(UserID))
  
  ShoppingList_With_UserID_Table <-
    
    filter(Get_ShoppingList_Table(), !is.na(UserID))
  
  # Adding UserID to data without UserID and combining with data already having UserID. 
  
  ShoppingList_Corrected_Table <- 
    
    filter(Get_listUserMapping_Table(), Owner == TRUE) %>%
  
    inner_join(ShoppingList_Without_UserID_Table, by = "ListID") %>%
    
    select(-(Owner)) %>%
  
    union(ShoppingList_With_UserID_Table) 
  
  # Connect to Consumer_Table to append install date.
    
  UserCountComparison_RawDb <-   
    
    left_join(ShoppingList_Corrected_Table, Get_Consumers_Table(), by = "UserID") %>%
  
    filter(!is.na(InstallDate)) %>%
    
    replace_na(NumberOfItems = 0)
  
}

JoinTables_ToCreate_ListCountComparison_RawDb <- function() {
  
  ListCountComparison_RawDb <- 
    
    Get_listItems_Table() %>% 
    
    select(ItemID, ListID) %>%
    
    group_by(ListID) %>%
    
    summarise(NumberOfItems = n()) %>%
    
    right_join(JoinTables_ToCreate_UserCountComparison_RawDb(), by = "ListID") %>%
    
    replace_na(list(NumberOfItems = 0))
  
}


