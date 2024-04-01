library(ggplot2)
library(dplyr)
library(sna)
library(knitr)
library(reshape2)
library(igraph)
library(GGally)
library(rlist)


STATES <- 8

STATENAMES <-  c("Unexposed",
                 "Asymptomatic & contagious",
                 "Symptomatic and contagious",
                 "Symptomatic and not contagious",
                 "Post-COVID immune",
                 "Naturally immune",
                 "Death",
                 "Left")


STATELABELS <<-  c("Unexposed","Asymptomatic\n & contagious",
                   "Symptomatic \n& contagious",
                   "Symptomatic \n& not contagious",
                   "Post-COVID immune",
                   "Naturally immune",
                   "Death",
                   "Left network")

########################################################################
#
## create an agent
#
########################################################################
makeAgent <- function(psychstate,biostate,age=30,traveler=0,is_infected = 0)
{
  
  return (list(psychstate=psychstate,
               biostate=biostate,
               age=age,
               nextbiostate=NA,
               biostatecountdown=NA,traveler=traveler,is_infected = is_infected))
}

# * 1. Unexposed
# * 2. Asymptomatic but infected/contagious
# * 3. Symptomatic and contagious
# * 4. Symptomatic and not contagious
# * 5. Post-COVID Immune
# * 6. Naturally immune (will not contract)
# * 7. Death
# * 8. Left network


bioTransition <- matrix(0,STATES,STATES)
bioMin <- matrix(1,STATES)      #state time minimum
bioMax <- matrix(1,STATES)      #state time maximum

bioMin[2] <- 3            #infected but asymptomatic for 3 to 10 days
bioMax[2] <- 10         
bioTransition[2,3] <- .1  #transition to infected with symptoms
bioTransition[2,5] <- .90  #transition to no longer contagious/cured

bioMin[3] <-    3             #symptoms + contagion
bioMax[3] <- 8                #symptoms + contagion max
bioTransition[3,4] <-  .95    #transitioon to no longer contagious
bioTransition[3,7] <-  .05    #transitioon to death state 

bioMin[4] <- 1          #symptoms bot no longer contagiious
bioMax[4] <- 7
bioTransition[4,5] <- 1  #Transition to 'immune' cured state.


setAgentState<- function(agent, biostate)
{
  agent$biostate <- biostate
  if(sum(bioTransition[biostate,])>0) # this state transitions to something else.
  {
    ##which state do we go to?
    agent$biostatecountdown <- sample(x=seq(bioMin[biostate],bioMax[biostate]),1) #how long will we state in this state?
    agent$nextbiostate <- sample(1:STATES, prob=bioTransition[agent$biostate,],size=1)
    
  } else{
    agent$biostatecountdown <- NA
    agent$nextbiostate <- NA   ##just so we can tell if the agent is finished.
  }
  return(agent) 
}

transitionAgent<- function(agent)
{
  return(setAgentState(agent,agent$nextbiostate))
}

updateAgent<- function(agent)
{
  if(!is.na(agent$biostatecountdown))
  {
    agent$biostatecountdown <- agent$biostatecountdown -1
    if(agent$biostatecountdown <=0)  ##new state
    {
      agent <- transitionAgent(agent)
      
    }
  }
  return(agent)
}

setAgentStatusInfected<- function(agent,status=1)
{
  agent$is_infected <- status
  return(agent)
}

#####################################################################
#
## Make a Network
#
#####################################################################
makeNetwork<- function(numAgents=5,numsets=3,steps=1,power=1)
{
  ord <- sample(numAgents)
  
  #print("____________________ord")
  #print(ord)
  
  
  #old_agent_order <<- ord 
  
  tmp<-as_adjacency_matrix(sample_pa(numAgents,power=power),sparse=F)
  
  tmp <- tmp + t(tmp)
  
  tmp <- tmp[ord,ord]
  
  if(numsets>1)
  {
    for(i in 2:numsets)
    {
      ord <- sample(numAgents)
      
      sn2 <-as_adjacency_matrix(sample_pa(numAgents,power=power),sparse=F)[ord,ord]
      tmp <- tmp + sn2 + t(sn2)
      
    }
  }
  if(steps>1)
  {
    for(i in 2:steps)
    {
      tmp <- tmp + tmp %*% tmp  ## iterate to neighbors
    }
  }
  (tmp>0)+0
  
}

######################################################################
#
## Add agent to the network
#
######################################################################

addAgentToNetwork<- function(numAgents=5,prev_network = socialnetwork,connection_number=1,self_connected=1,pool)
{
  
  if(connection_number < 1){
    
    connection_number <- 1
  }
  
  number_of_rows <- nrow(prev_network)
  
  starting_point <- number_of_rows+1
  end_point <- number_of_rows + numAgents
  
  
  for(i in starting_point:end_point) {
    
    
    prev_network <- rbind(prev_network, 0)
    prev_network <- cbind(prev_network, 0)
    
    
    
    
  }  
  
  
  if(number_of_rows>= connection_number ){
    
    #x <- 1:number_of_rows
    
    u <- 1:number_of_rows
    
    for (val in u) {
      
      if((pool[[val]]$biostate != 8) ){
        
        if((pool[[val]]$biostate != 7)){
          x<-u
        }
      }
      
    }    
    
    
    a <- sample(x, connection_number)
    # print("SSSSSSSSSSSSSSSSSSSS")
    # print(a)
    # 
    
    
    # repeat{
    #   
    #   if((pool[[a]]$biostate != 8) ){
    #     
    #     if((pool[[a]]$biostate != 7)){
    #       break
    #     }
    #     
    #   }
    # }
    # print(a)
    
    
    for(i in starting_point:end_point) {
      if(self_connected==1){
        
        prev_network[i,i] <- 1
      }
      
      current_new_agent <-i
      for (i in seq_along(a)) {
        
        #print(a[i])
        prev_network[a[i],current_new_agent] <- 1
        prev_network[current_new_agent,a[i]] <- 1
      }
      
      
      
    }     
    
    
    
  }
  else{
    
    #x <- 1:number_of_rows
    # print("SSSSSSSSSggfgfgSSSSSSSSSSS")
    # print(a)
    #     
    u <- 1:number_of_rows
    
    for (val in u) {
      
      if((pool[[val]]$biostate != 8) ){
        
        if((pool[[val]]$biostate != 7)){
          x<-u
        }
      }
      
    }      
    
    a <- sample(x, 1)
    
    
    
    
    # print(a)
    
    
    for(i in starting_point:end_point) {
      if(self_connected==1){
        
        prev_network[i,i] <- 1
      }
      
      current_new_agent <-i
      for (i in seq_along(a)) {
        
        #print(a[i])
        prev_network[a[i],current_new_agent] <- 1
        prev_network[current_new_agent,a[i]] <- 1
      }
      
      
      
    }    
    
    
    
  }
  
  
  
  
  return(prev_network)
  
  
}

######################################################################
#
## social connection of the travelers
#
######################################################################

mygplot_updated <- function(coord, network,states,main="")
{
  if(is.null(coord))
  {
    coord  <- gplot.layout.fruchtermanreingold(network,layout.par=list(niter=500))
    #gplot.layout.fruchtermanreingold(network,layout.par=list(niter=500))
    #ggnet2(network, mode = "fruchtermanreingold", layout.par=list(niter=500))
    #gplot.layout.fruchtermanreingold(network,layout.par=list(niter=500))
  }
  
  newmin <- mean(coord[,2]) - (-min(coord[,2]) + mean(coord[,2])) * 1.4
  palette=c("white","yellow","red","green","darkgreen","blue","black","grey")
  plot(coord,col="black",bty="n",pch=16,cex=2.7,xaxt="n",yaxt="n",main=main,xlab="",ylab="",axes=F,
       ylim=c(newmin,max(coord[,2])),type="n")
  
  
  travelers <- sapply(pool,function(x){x$traveler})
  
  for(i in 1:nrow(network))
  {
    if(sum(network[i,])>0)
    {
      segments(coord[i,1],
               coord[i,2],
               coord[network[i,]==1,1],
               coord[network[i,]==1,2],col="grey40")
    } 
  }
  points(coord,pch=c(16,15)[travelers+1],cex=2.3,col= palette[states])
  points(coord,pch=c(1,0)[travelers+1],cex=2.3,col="black")
  
  legend(mean(coord[,1]),min(coord[,2]),bty='n',y.intersp=.7,cex=.8,
         STATENAMES, pch=16,col=palette)
  
  if(sum(travelers)>0)
  {
    #print("plotting rectangle")
    rect(min(coord[travelers,1]),
         min(coord[travelers,2]),
         max(coord[travelers,1]),
         max(coord[travelers,2]))
    
  }
  
  
  return (coord)
}

#####################################################################
#
## 
#
#####################################################################

travel_ban_lift <- function( enable_outside_travel_restriction_day,second_wave_day,
                             connection_number = 1, will_affect_traveler = 0, numDays = 50,
                             people_leave_status = 1,
                             number_of_people_leave = 4,
                             naturalImmunity = .01,numInteractions_value = 14,
                             contagionProb = .05,####5% chance
                             sampleFromNetwork = .98,traveler_percentage_mean = 1,
                             traveler_percentage_ceiling = 2, traveler_increase = 0,
                             boundary_of_traveler_change = 2, traveler_interval_day = 5, 
                             enable_second_wave = 1,
                             enable_outside_travel_restriction =0,need_to_update_graph_cord = 0 ){ 
  
  
  
  
  socialnetwork <- original_network_of_agents
  numAgents <- numAgents_initial
  people_left <-0
  
  
  
  plotNetwork <-T
  
  # If i create the social network here, works ok
  #socialnetwork <-makeNetwork(numAgents,numsets=1,power=.5,steps=2)
  #print(socialnetwork)
  
  #pool <<-pool_initial
  
  .GlobalEnv$pool <<- pool_initial
  
  numInteractions <-  rep(numInteractions_value,numDays) ##interaction during no travel bans
  numInteractions[enable_outside_travel_restriction_day:second_wave_day] <- as.integer(numInteractions_value*(1/3))  ##interaction during travel restriction
  
  
  local_population_infection_number_local_variable <<- local_population_infection_number
  traveler_infection_number_local_variable <- 0
  
  if(plotNetwork)
  {
    cc <-mygplot_updated(coord=NULL,socialnetwork,rep(1,nrow(socialnetwork)),main="Initial state")
    
    xrange <-   range(cc[,1])
    xval <- xrange[2] + (xrange[2]-xrange[1])/3
    yval <- mean(cc[,2])
    sd <- (xrange[2]-xrange[1]) /10
  }  
  
  
  
  
  disthistory <- matrix(NA,ncol=STATES,nrow=numDays)
  
  # pool <<-pool_initial
  
  
  for(day in 1:numDays)
  {
    
    
    ###################agent leaving Start
    
    #innetwork <- sapply(.GlobalEnv$pool,function(y){!(y$biostate==7||y$biostate==8)})
    if(!enable_outside_travel_restriction){
      if(people_leave_status){
        ####Hard coding day for people leave
        #.GlobalEnv$pool <- list.remove(.GlobalEnv$pool,  c(1))
        
        if(day == (second_wave_day+7)){
          
          number_of_people_leave <- as.integer((1/100)*numAgents) ##1% will leave the system
          
          
          u <- 1:numAgents
          
          for (val in u) {
            
            if((.GlobalEnv$pool[[val]]$biostate != 8) ){
              
              if((.GlobalEnv$pool[[val]]$biostate != 7)){
                x<-u
                
              }
            }
            
          }    
          
          a <- sample(x, number_of_people_leave)
          
          
          for (i in seq_along(a)) {
            
            #print(a[i])
            x <- a[i]
            .GlobalEnv$pool[[x]] <- setAgentState( .GlobalEnv$pool[[x]],8)
            
            socialnetwork[x,] <- 0
            socialnetwork[,x] <-0
          }                
          
          
          
          
        }          
        if(day == (second_wave_day+14)){
          
          number_of_people_leave <- as.integer((1/100)*numAgents) ##1% will leave the system
          
          
          u <- 1:numAgents
          
          for (val in u) {
            
            if((.GlobalEnv$pool[[val]]$biostate != 8) ){
              
              if((.GlobalEnv$pool[[val]]$biostate != 7)){
                x<-u
                
              }
            }
            
          }    
          
          a <- sample(x, number_of_people_leave)
          
          
          for (i in seq_along(a)) {
            
            #print(a[i])
            x <- a[i]
            .GlobalEnv$pool[[x]] <- setAgentState( .GlobalEnv$pool[[x]],8)
            
            socialnetwork[x,] <- 0
            socialnetwork[,x] <-0
          }                
          
          
          
          
        }              
        
        # if(day == (second_wave_day+5)){
        #   
        #   d <- 1:numAgents
        #   number_traveler_found
        #   repeat{
        #     statements...
        #     if(condition){
        #       break
        #     }
        #   }            
        #   
        #   
        #   
        #       x <- 18
        #       .GlobalEnv$pool[[x]] <- setAgentState( .GlobalEnv$pool[[x]],8)
        # 
        #       socialnetwork[x,] <- 0
        #       socialnetwork[,x]<-0
        # }
        
        
        
      }
      
      
    }
    
    ###################agent leaving ENd
    
    
    
    innetwork <- sapply(.GlobalEnv$pool,function(y){!(y$biostate==7||y$biostate==8)})
    
    ##who are you going to talk to today.
    sneezers <- rep((1:numAgents)[innetwork],each=numInteractions[day])
    sneezedons <- rep(NA,length(sneezers))
    
    for(i in 1:length(sneezers))
    {
      if(runif(1)<(1-sampleFromNetwork))
      {
        
        sneezedons[i] <- sample((1:numAgents)[innetwork],1)
      }else{
        
        probs <- socialnetwork[sneezers[i],]* innetwork
        sneezedons[i] <- sample(1:numAgents,prob=probs,1)
        
      }
    }
    
    
    for(i in 1:length(sneezers))
    {
      
      agent1 <- .GlobalEnv$pool[[ sneezers[i] ]]
      agent2 <- .GlobalEnv$pool[[ sneezedons[i] ]]
      
      
      ##this constitutes the rules of infection.
      if((agent1$biostate==2 || agent1$biostate==3 ) & agent2$biostate==1 & runif(1)<contagionProb)
      {
        .GlobalEnv$pool[[ sneezedons[i] ]] <- setAgentState(agent2,2)##infect!
      }
      
    }
    ##increment each agent 1-day.
    for(i in 1:numAgents)
    {
      .GlobalEnv$pool[[i]] <- updateAgent(.GlobalEnv$pool[[i]])
      
      ###keep track number of local population infection
      
      
      if(.GlobalEnv$pool[[i]]["traveler"] == 0 ){
        if(.GlobalEnv$pool[[i]]["biostate"] == 2){
          if(.GlobalEnv$pool[[i]]["is_infected"] == 0){         
            local_population_infection_number_local_variable <-local_population_infection_number_local_variable+1
            .GlobalEnv$pool[[i]] <- setAgentStatusInfected( .GlobalEnv$pool[[i]],1)
          }
          
        }
        
        
      }         
      if(.GlobalEnv$pool[[i]]["traveler"] == 1 ){
        if(.GlobalEnv$pool[[i]]["biostate"] == 2){
          if(.GlobalEnv$pool[[i]]["is_infected"] == 0){         
            traveler_infection_number_local_variable <-traveler_infection_number_local_variable+1
            .GlobalEnv$pool[[i]] <- setAgentStatusInfected( .GlobalEnv$pool[[i]],1)
          }
          
        }
        
        
      }            
      
      
    }
    
    states <- sapply(.GlobalEnv$pool,FUN=function(x){x$biostate})
    distrib <- table(factor(states,levels=1:STATES))
    disthistory[day,] <- distrib
    # print(disthistory[day,])
    
    
    
    
    
    #####################################
    ############################
    ##################################
    ################
    ################################   
    ####################Adding New agents as travelers in the network
    
    #traveler_interval_day <-sample(c(3,2,5),1)
    
    # traveler_increase <- sample(c(0,1),1)
    
    
    
    if(traveler_increase == 0){
      if(day == second_wave_day){
        
        if(enable_second_wave == 1){
          traveler_percentage_mean <- as.integer(traveler_percentage_ceiling/1.5)
          
          
        }
        
        
      }
    }
    ##travel restriction on
    
    
    
    ###if second wave already happned due to premature removal of travel ban. traveler will come in future days
    if((enable_second_wave == 1) && (day == second_wave_day)){
      
      enable_outside_travel_restriction <-0
      
    }
    else if((enable_second_wave == 1) && (day > second_wave_day)){
      
      enable_outside_travel_restriction <-0 ###if we do not want to close down the travel after second wave
    }
    else if(enable_outside_travel_restriction_day == 0){
      
      enable_outside_travel_restriction <-0
      
    }else if(enable_outside_travel_restriction_day == day){
      enable_outside_travel_restriction <-1
      
    }else if(enable_outside_travel_restriction == 1){
      
      enable_outside_travel_restriction <-1
      
    }
    else{
      
      enable_outside_travel_restriction <-0
      
    }
    
    
    
    
    
    ####
    if(((!(day %% traveler_interval_day)) ||
        (day==second_wave_day)) && 
       (enable_outside_travel_restriction==0)){
      
      if(traveler_percentage_mean < 1){
        
        traveler_percentage_mean =  as.integer(traveler_percentage_ceiling/2)
      }
      else if(traveler_percentage_mean>traveler_percentage_ceiling){
        
        traveler_percentage_mean = traveler_percentage_ceiling
      }
      
      
      if(traveler_percentage_mean > 0){
        number_of_traveler <-as.integer((traveler_percentage_mean/100) * numAgents_initial)
        current_number_traveler_remaining <- number_of_traveler
        if(traveler_increase < 1){
          
          traveler_percentage_mean = traveler_percentage_mean - boundary_of_traveler_change
          
          
        }else{
          
          traveler_percentage_mean = traveler_percentage_mean + boundary_of_traveler_change
          
          
          
        }
        ###enabling travel restruition after second wave
        if(enable_second_wave == 1){
          if((day==second_wave_day) && (enable_outside_travel_restriction_day != 0)){
            
            enable_outside_travel_restriction<-1
          }
        }
        
        ###for evenly distributing travel to network I run a do while loop
        #print("number_of_traveler")
        #print(number_of_traveler)
        #print("-----------------------start-------------------")
        repeat{
          current_number_traveler <- sample(c(1,2,3,4,5),1)
          
          #
          
          
          if(current_number_traveler>current_number_traveler_remaining){
            
            current_number_traveler <- current_number_traveler_remaining
          }
          
          # print(current_number_traveler)
          
          
          if(current_number_traveler > 0)
          {
            starting_numagents <- numAgents + 1
            ending_numagent <- numAgents+current_number_traveler
            
            ####connection_number indicates new agent connected how many number of previous agents, currently it is a number between 1-3
            
            socialnetwork <- addAgentToNetwork(numAgents = current_number_traveler,
                                               prev_network = socialnetwork,
                                               connection_number = connection_number,
                                               self_connected=1,.GlobalEnv$pool)
            # print(socialnetwork)
            
            newxy <- cbind(rnorm(current_number_traveler,mean=xval,sd=sd),
                           rnorm(current_number_traveler, mean=yval,sd=sd))
            
            cc <- rbind(cc,newxy)
            for(i in starting_numagents:ending_numagent)
            {
              ###printing to see how are travelers
              # print("adding")
              .GlobalEnv$pool[[i]] <<- makeAgent(psychstate=1,
                                                 biostate=sample(c(1,6),
                                                                 p=c(1-naturalImmunity, naturalImmunity),1),
                                                 traveler = 1)
              # print("added")
              ################to make randomize infect -> traveler
              
              will_be_infected <- sample(c(0,1), 1)
              
              
              
              # print(paste("infecting",i))
              #####Tell shane about if setAgentState is 2, then number of local remains ok, but if state is 1 then it changes
              if(will_affect_traveler == 1){
                #print("poit a")
                tmp <-  setAgentState(.GlobalEnv$pool[[i]],2) ##infect this person
                #print("point b")
                .GlobalEnv$pool[[i]] <<-tmp
                
              }else{
                
                # print("point b1")
                # tmp <- setAgentState(.GlobalEnv$pool[[i]],1) ##do not infect this person
                # print("point b2")
                # .GlobalEnv$pool[[i]] <<- tmp
                # print("point b3")
              }
              # print("infected")
              
              # 
              
              
              
              
              
            }
            numAgents <- ending_numagent
            need_to_update_graph_cord <- 1
          }
          current_number_traveler_remaining <- current_number_traveler_remaining-current_number_traveler
          if(current_number_traveler_remaining < 1){
            #print("-----------------------end-------------------")
            break
          }
          
        }
      }
    }
    
    if(plotNetwork)
    {
      ## print(1)
      
      ##show this to SHane
      # cc <-mygplot(coord=NULL,socialnetwork,rep(1,nrow(socialnetwork)),main="Initial state")
      # mygplot(coord=NULL,socialnetwork,states,main=paste("Day",day))
      
      if(need_to_update_graph_cord){
        # print("UPDATE NEEDED")
        
        mygplot_updated(cc,socialnetwork,states,main=paste("Day",day, " ", "Traveler Added", number_of_traveler))
        
        
      }else{
        
        mygplot_updated(cc,socialnetwork,states,main=paste("Day",day))
      }
      need_to_update_graph_cord <- 0
      
      
      
      
      
      
    }    
    
    
    ###vary week for travel restriction
    
    
    
    
  }
  
  total_local <-0
  affected_locals <- 0
  affected_travelers <- 0
  local_death <- 0
  total_traveler <-0
  traveler_death <- 0
  ok_travler <- 0
  ok_local<- 0
  #print(disthistory)
  
  for(i in 1:numAgents)
  {
    
    if(pool[[i]]["traveler"] == 0){
      
      #print(i)
      
      total_local <-total_local+1
      if(pool[[i]]["biostate"] == 2 || pool[[i]]["biostate"] == 3 || pool[[i]]["biostate"] == 4)
      {
        affected_locals <-affected_locals + 1
      }
      else if(pool[[i]]["biostate"] == 7){
        
        local_death <-local_death+1
      }
      else{
        
        ok_local <- ok_local+1
      }
      
    }
    else{
      total_traveler <- total_traveler+1
      if(pool[[i]]["biostate"] == 2 || pool[[i]]["biostate"] == 3 || pool[[i]]["biostate"] == 4)
      {
        affected_travelers <-affected_travelers + 1
      }
      else if(pool[[i]]["biostate"] == 7){
        
        traveler_death <-traveler_death+1
      }  
      else{
        
        ok_travler <- ok_travler+1
      }       
      
      
    }
  }
  
  #.GlobalEnv$pool <- list.remove(.GlobalEnv$pool, 1)
  #print(.GlobalEnv$pool)
  print("TOTAL")
  print(numAgents)
  print("Total Local")
  print(total_local)
  # print("Local Recovered/Not affected")
  # print(ok_local)
  # print("Local Affected")
  # print(affected_locals)
  print("Local Death")
  print(local_death)
  print("-----")
  print("Total Traveler")
  print(total_traveler)  
  
  # print("Traveler Recovered/Not affected")
  # print(ok_travler)
  # print("Traveler Affected")
  # print(affected_travelers)
  print("Traveler Death")
  print(traveler_death)
  
  
  print("Local infected at somepoint")
  print(local_population_infection_number_local_variable)
  print("Traveler infected at somepoint")
  print(traveler_infection_number_local_variable) 
  print("Total infected at somepoint")
  print(traveler_infection_number_local_variable + local_population_infection_number_local_variable) 
  
  # print(traveler_percentage_mean)
  
  
  
  
  
  
  return(disthistory)
  
  
}

#####################################################################
#
## initial simulations
#
#####################################################################

numAgents_initial <<- 700
naturalImmunity <- .01
socialnetwork <-makeNetwork(numAgents_initial,numsets=1,power=.5,steps=2)
local_population_infection_number <-0

infect_local <- 0

original_network_of_agents <<- socialnetwork

.GlobalEnv$pool <- list()
for(i in 1:numAgents_initial)
{
  .GlobalEnv$pool[[i]] <- makeAgent(psychstate=1,
                                    biostate=sample(c(1,6),
                                                    p=c(1-naturalImmunity, naturalImmunity),1))
}

##infect patient 0
numInfected <- 3
for(i in sample(numAgents_initial,numInfected))
{
  if(infect_local){
    
    .GlobalEnv$pool[[i]] <- setAgentState(pool[[i]],2) ##infect this person
    
    
    if(.GlobalEnv$pool[[i]]["biostate"] == 2){
      if(.GlobalEnv$pool[[i]]["is_infected"] == 0){         
        local_population_infection_number <-local_population_infection_number+1
        .GlobalEnv$pool[[i]] <- setAgentStatusInfected( .GlobalEnv$pool[[i]],1)
      }
      
    }
  }
  
} 

pool_initial <<-.GlobalEnv$pool
local_population_infection_number <<- local_population_infection_number

##################################################################
#
## travel ban lifted on day 49 with 3 contacts per traveller
#
##################################################################



####################################################################
#
## the ui and server 
#
####################################################################

library(shiny)

####################################################################
#
## the ui 
#
####################################################################


ui <- fluidPage(
  titlePanel("COVID-19 Network Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("numDays", "Number of Days:", min = 1, max = 100, value = 50),
      sliderInput("numAgents", "Number of Agents:", min = 100, max = 1000, value = 700),
      # Add other parameters as necessary...
      actionButton("runSimulation", "Run Simulation")
    ),
    
    mainPanel(
      plotOutput("networkPlot")
      # textOutput("statistics")
      # Other output elements...
    )
  )
)


####################################################################
#
## the server 
#
####################################################################

server <- function(input, output, session) {
  observeEvent(input$runSimulation, {
    numDays <- input$numDays
    numAgents <- input$numAgents
    
    socialnetwork <-makeNetwork(numAgents,numsets=1,power=.5,steps=2)
    naturalImmunity <- .01
    
    # initial sims
    enable_outside_travel_restriction_day <- 7
    second_wave_day <- as.integer((49/100)*numDays)
    
    # Output the final network plot
    output$networkPlot <- renderPlot({
      results = travel_ban_lift(enable_outside_travel_restriction_day,second_wave_day,
                                connection_number = 3,will_affect_traveler = 1,numDays = numDays)
    })
    # output$statistics <- renderText({
    #   paste("Total Agents: ", numAgents, ...)
    # })
  })
  
}
#####################################################################
#
## run the app
#
#####################################################################
shinyApp(ui = ui, server = server)