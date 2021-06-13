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


#' @description Function to aggregate prod by species
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
spe.prod<-function(df){
  
  df<-
  df %>% 
    group_by(Species) %>% 
    summarize(production=sum(Prod_lac_tonne)) %>% 
    rename(name=Species, value=production) %>% 
    arrange(value) %>% 
    mutate(name=factor(name, levels = name))
  
  
    
}

#' @description Function to aggregate fishermen engagement
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
fish.eng.o<-function(df){
  
  df<-
    df %>% 
    group_by(Type, Gender) %>% 
    summarize(Number=sum(Number, na.rm = T)) %>% 
    rename(name=Type, Type=Gender, value=Number)
  dt<-
    df %>% 
    group_by(name) %>% 
    summarize(value=sum(value)) %>% 
    arrange(value)
  dt$name<-factor(dt$name, levels = dt$name)
  df %>% 
    mutate(name=factor(name, levels = levels(dt$name))) 
  
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom magrittr %>%
#' @param df,df2 Input data.
#' @return aggregated dataframe
#'
#' @noRd
op.p.fish<-function(df, df2){
  k<-
    df2 %>% 
    group_by(State, Type) %>% 
    summarize(fishin_pop=sum(fishing_pop, na.rm = T)) %>% 
    ungroup()
  l<-df %>% 
    inner_join(k, by=c("State"="State", "Type"="Type")) %>% 
    mutate(output_per_fish=Prod_in_lac_tonne*100000/fishin_pop) %>% 
    mutate(output_per_fish=ifelse(is.nan(output_per_fish) | is.infinite(output_per_fish), 0, output_per_fish))
  
  m<-
    l %>% 
    select(-Prod_in_lac_tonne) %>% 
    group_by(State) %>% 
    summarize(output_per_fish=mean(output_per_fish)) %>% 
    arrange(output_per_fish) %>% 
    filter(output_per_fish>0.05)
  m$State<-factor(m$State, levels = m$State)
  
  l %>% 
    mutate(State=factor(State, levels = levels(m$State))) %>% 
    #select(State, Type, output_per_fish) %>% 
    rename(name=State, value=output_per_fish, value2=Prod_in_lac_tonne) %>% 
    filter(value>0.05)
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
cons.cal<-function(df){

  a<-df %>% 
    group_by(State) %>% 
    summarize(ppc_pa=mean(pcc_kg_pa, na.rm = T)) %>% 
    arrange(ppc_pa)
  a$State<-factor(a$State, levels = a$State)
  
  df %>% 
    mutate(State=factor(State, levels = levels(a$State), ordered = T)) %>% 
    rename(name=State, value=pcc_kg_pa, type=Area)
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
veg.cal<-function(df){
  
  a<-df %>% 
    select(c(1,4,5)) %>% 
    pivot_longer(cols=c("veg","non_veg"), names_to = "veg", values_to="pop") %>% 
    group_by(State) %>% 
    summarize(pop=sum(pop, na.rm = T)) %>% 
    arrange(pop)
  a$State<-factor(a$State, levels = a$State)
  
  df %>% 
    mutate(State=factor(State, levels = levels(a$State), ordered = T)) %>% 
    pivot_longer(cols=c("veg","non_veg"), names_to = "veg", values_to="pop") %>% 
    group_by(State, veg) %>% 
    summarize(pop=sum(pop, na.rm = T)) %>% 
    rename(name=State, value=pop, Type=veg)
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
train.cal<-function(df){
  
  a<-df %>% 
    group_by(State) %>% 
    summarize(trainings=sum(fishermen_trained, na.rm = T)) %>% 
    arrange(trainings)
  a$State<-factor(a$State, levels = a$State)

  a %>% 
    rename(name=State, value=trainings)
  
}


#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
fishing.pop.cal<-function(df){
  
  a<-df %>% 
    arrange(fishermen)
  a$State<-factor(a$State, levels = a$State)
  
  a %>% 
    rename(name=State, value=fishermen) %>% 
    select(name, value)
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
fishing.pop.per.cal<-function(df){
  
  a<-df %>% 
    arrange(fish_per_1000)
  a$State<-factor(a$State, levels = a$State)
  
  a %>% 
    rename(name=State, value=fish_per_1000) %>% 
    select(name, value)
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
infra<-function(df){
  
  a<-df %>% 
    filter(type!="Sanctioned") %>% 
    mutate(type=ifelse(type=="installation","new", type)) %>% 
    group_by(State, resource, type) %>% 
    summarize(Area=sum(Area_ha, na.rm = T), Units=sum(Number, na.rm=T))
  b<- a %>% 
    group_by(State) %>% 
    summarize(Area=sum(Area, na.rm = T))
  b$State<-factor(b$State, levels = b$State)
  
  a %>% 
    mutate(State=factor(State, levels = b$State))
  
}

#' @description Function to calculate output per fisherman
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr group_by summarize rename mutate select inner_join ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @param df Input data.
#' @return aggregated dataframe
#'
#' @noRd
ind.fish.seed<-function(df){
  
 df %>% 
    group_by(Year) %>% 
    summarize(Prod_lac_fry=sum(Prod_lac_fry)) %>% 
    rename(name=Year, value=Prod_lac_fry)
  
}

