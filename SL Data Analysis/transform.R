#
# Logic to transform data as per the analysis requirement. 
#

Transform_UserCountComparisonRawDb_ByUserType <- 
  function(RawDb, Duration_1) {
  
  # RawDb1 <- UserCountComparison_RawDb
  
  # Duration_1 <- parse_date(c("2019-01-01", "2019-02-20"))

  UserType <- c("OldUser", "NewUser")
  
  OwnershipType <- c("Few", "Some", "Many")
  
  UserCountComparison_ByUserType_TransformedDb <- 
    
    filter(RawDb, CreationDate > Duration_1[1] & CreationDate <= Duration_1[2]) %>%
    
    group_by(UserID, InstallDate) %>%
    
    summarise(NumberOfLists = n()) %>%  
    
    mutate(
      
           U1vsU2 = if_else(InstallDate < Duration_1[1], UserType[1], UserType[2]),
    
           Ownership_Count_Type = 
             
             if_else(NumberOfLists < 5, 
                     OwnershipType[1], if_else(NumberOfLists >= 5 & NumberOfLists < 10, 
                                               OwnershipType[2], OwnershipType[3])))
  
  # print(UserCountComparison_ByUserType_TransformedDb)
  
  # ggplot(data = UserCountComparison_ByUserType_TransformedDb) + 
  #   geom_bar(mapping = aes(x = U1vsU2))
  #     geom_bar(mapping = aes(x = U1vsU2, fill = Ownership_Count_Type), position = "dodge")

}

Transform_UserCountComparisonRawDb_ByBuiltType <- 
  function(RawDb, Duration_1, Duration_2, UserSelected) {
  
  # RawDb <- UserCountComparison_RawDb
    
  # Duration_1 <- parse_date(c("2019-01-01", "2019-01-30"))
    
  # Duration_2 <- parse_date(c("2019-02-01", "2019-02-28"))
  
  # UserSelected <- "e"
    
  UserType <- c("OldUser", "NewUser", "LaterUser")
    
  OwnershipType <- c("Few", "Some", "Many")
  
  CreationPeriod <- c("Period_1", "Period_2", "Period_NA")
    
  UserCountComparison_ByBuiltType_TransformedDb <-
    
    mutate(RawDb, 
           
           D1vsD2 = 
             
             if_else(CreationDate >= Duration_1[1] & CreationDate <= Duration_1[2],
                     CreationPeriod[1], 
                     if_else(CreationDate >= Duration_2[1] & CreationDate <= Duration_2[2],
                             CreationPeriod[2], CreationPeriod[3])) 
           
           ) %>% 
    
    filter(D1vsD2 != CreationPeriod[3]) %>% 
    
    group_by(D1vsD2, UserID, InstallDate) %>%
    
    summarise(NumberOfLists = n()) %>% 
    
    mutate( 
           
          Ownership_Count_Type = 
            
             if_else(NumberOfLists < 5, 
                OwnershipType[1], if_else(NumberOfLists >= 5 & NumberOfLists < 10, 
                                          OwnershipType[2], OwnershipType[3])), 
      
          InstallDate_Vs_Duration_1 = 
             
             if_else(InstallDate < Duration_1[1], 
                     UserType[1], 
                     if_else(InstallDate >= Duration_1[1] & InstallDate < Duration_1[2], 
                             UserType[2], UserType[3])),
           
           
          InstallDate_Vs_Duration_2 =
             
             if_else(InstallDate < Duration_2[1], 
                     UserType[1], 
                     if_else(InstallDate >= Duration_2[1] & InstallDate < Duration_2[2], 
                             UserType[2], UserType[3]))
    )
  
  if(UserSelected == "d") {
          
          UserCountComparison_ByBuiltType_TransformedDb <- 
            
            UserCountComparison_ByBuiltType_TransformedDb 
          
  } else if (UserSelected == "e") {
          
          UserCountComparison_ByBuiltType_TransformedDb <- 
            
            filter(UserCountComparison_ByBuiltType_TransformedDb, 
                   InstallDate_Vs_Duration_1 == UserType[2] 
                   | InstallDate_Vs_Duration_2 == UserType[2])
  } else {
          
          UserCountComparison_ByBuiltType_TransformedDb <- 
            
            filter(UserCountComparison_ByBuiltType_TransformedDb, 
                   InstallDate_Vs_Duration_1 == UserType[1] 
                   & InstallDate_Vs_Duration_2 == UserType[1])
  }
}

Transform_ListCountComparisonRawDb_ByUserType <- 
  function(RawDb, Duration_1) {
  
    # RawDb3 <- ListCountComparison_RawDb
    
    # Duration_1 <- parse_date(c("2019-01-01", "2019-02-20"))
    
    UserType <- c("OldUser", "NewUser")
    
    SizeType <- c("Small", "Medium", "Large")
    
    ListCountComparison_ByUserType_TransformedDb <- 
      
      filter(RawDb, CreationDate > Duration_1[1] & CreationDate <= Duration_1[2]) %>%
      
      mutate(
        
             U1vsU2 = if_else(InstallDate < Duration_1[1], UserType[1], UserType[2]),
      
             Size_Count_Type = 
               
               if_else(NumberOfItems < 6, 
                       SizeType[1], if_else(NumberOfItems >= 6 & NumberOfItems < 15, 
                                            SizeType[2], SizeType[3])))
  
}

Transform_ListCountComparisonRawDb_ByBuiltType <- 
  function(RawDb, Duration_1, Duration_2, UserSelected) {
  
    # RawDb4 <- ListCountComparison_RawDb
    
    # Duration_1 <- parse_date(c("2019-01-01", "2019-01-30"))
    
    # Duration_2 <- parse_date(c("2019-02-01", "2019-02-28"))
    
    # UserSelected <- 3
    
    UserType <- c("OldUser", "NewUser", "LaterUser")
    
    SizeType <- c("Small", "Medium", "Large")
    
    CreationPeriod <- c("Period_1", "Period_2", "Period_NA")
    
    ListCountComparison_ByBuiltType_TransformedDb <-
      
      mutate(RawDb, 
             
             D1vsD2 = 
               
               if_else(CreationDate >= Duration_1[1] & CreationDate <= Duration_1[2],
                       CreationPeriod[1], 
                       if_else(CreationDate >= Duration_2[1] & CreationDate <= Duration_2[2],
                               CreationPeriod[2], CreationPeriod[3])) 
             
      ) %>% 
      
      filter(D1vsD2 != CreationPeriod[3]) %>% 
      
      mutate( 
        
        Size_Count_Type = 
          
          if_else(NumberOfItems < 6, 
                  SizeType[1], if_else(NumberOfItems >= 6 & NumberOfItems < 15, 
                                       SizeType[2], SizeType[3])),
    
        InstallDate_Vs_Duration_1 = 
          
          if_else(InstallDate < Duration_1[1], 
                  UserType[1], 
                  if_else(InstallDate >= Duration_1[1] & InstallDate < Duration_1[2], 
                          UserType[2], UserType[3])),
        
        
        InstallDate_Vs_Duration_2 =
          
          if_else(InstallDate < Duration_2[1], 
                  UserType[1], 
                  if_else(InstallDate >= Duration_2[1] & InstallDate < Duration_2[2], 
                          UserType[2], UserType[3]))
      )
    
    if(UserSelected == "d") {
            
            ListCountComparison_ByBuiltType_TransformedDb <- 
              
              ListCountComparison_ByBuiltType_TransformedDb
            
    } else if (UserSelected == "e") {
            
            ListCountComparison_ByBuiltType_TransformedDb <- 
              
              filter(ListCountComparison_ByBuiltType_TransformedDb, 
                     InstallDate_Vs_Duration_1 == UserType[2] 
                     | InstallDate_Vs_Duration_2 == UserType[2])
    } else {
            
            ListrCountComparison_ByBuiltType_TransformedDb <- 
              
              filter(ListCountComparison_ByBuiltType_TransformedDb, 
                     InstallDate_Vs_Duration_1 == UserType[1] 
                     & InstallDate_Vs_Duration_2 == UserType[1])
    }
  
}
