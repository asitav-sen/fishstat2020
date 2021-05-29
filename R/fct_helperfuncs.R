#' helperfuncs 
#'
#' @description Function to aggregate production by state
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename arrange
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
fish.prod.by.state<-function(df){

  df<-
    df %>% 
    group_by(State) %>% 
    summarize(production=sum(Prod_in_lac_tonne, na.rm = T)) %>% 
    rename(name=State, value=production)
  dt <-
    df %>% 
    group_by(name) %>% 
    summarize(value=sum(value)) %>% 
    arrange(desc(value))
  dt$name<-factor(dt$name, levels = dt$name)
  
  df %>% 
    mutate(name=factor(name, levels = levels(dt$name))) 
  
  
  
}

#' @description Function to aggregate production India
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
fish.prod.byyear<-function(df){

  df %>% 
    group_by(Year) %>% 
    summarize(production=sum(Prod_in_lac_tonne, na.rm = T)) %>% 
    rename(name=Year, value=production) %>% 
    mutate(name=as.factor(name)) 
}

#' @description Function to aggregate production India by type
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
fish.prod.yeartype<-function(df){

  df %>% 
    group_by(Year, Type) %>% 
    summarize(production=sum(Prod_in_lac_tonne, na.rm = T)) %>% 
    rename(name=Year, value=production) %>% 
    mutate(name=as.factor(name), Type=as.factor(Type))
}

#' @description Function to aggregate inland resources India
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
india.inland.resources<-function(df){

df %>% 
    group_by(Type) %>% 
    summarize(Number=sum(Number,na.rm = T), Area=sum(Area_Ha,na.rm = T), Length=sum(Length_km,na.rm = T))
}

