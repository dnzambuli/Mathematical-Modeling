#############################################################################
#
# Necessary Libraries and Defining states
#
############################################################################

library(ggplot2)
library(dplyr)
library(sna)
library(knitr)
library(reshape2)
library(igraph)
#library(animation)

STATES <- 7

STATENAMES <-  c("Unexposed",
                 "Asymptomatic & contagious",
                 "Symptomatic and contagious",
                 "Symptomatic and not contagious",
                 "Post-COVID immune",
                 "Naturally immune",
                 "Death")


STATELABELS <-  c("Unexposed","Asymptomatic\n & contagious",
                  "Symptomatic \n& contagious",
                  "Symptomatic \n& not contagious",
                  "Post-COVID immune",
                  "Naturally immune",
                  "Death")


#############################################################################
#
# Define the agent, and transition matrix
#
############################################################################

makeAgent <- function(psychstate,biostate,age=30)
{
  
  return (list(psychstate=psychstate,
               biostate=biostate,
               age=age,
               nextbiostate=NA,
               biostatecountdown=NA))
}


# * 1. Unexposed
# * 2. Asymptomatic but infected/contagious
# * 3. Symptomatic and contagious
# * 4. Symptomatic and not contagious
# * 5. Post-COVID Immune
# * 6. Naturally immune (will not contract)
# * 7. Death


bioTransition <- matrix(0,STATES,STATES)
bioMin <- matrix(1,STATES)      #state time minimum
bioMax <- matrix(1,STATES)      #state time maximum



bioMin[2] <- 3            #infected but asymptomatic for 3 to 15 days
bioMax[2] <- 15          
bioTransition[2,3] <- .15  #transition to infected with symptoms
bioTransition[2,5] <- .85  #transition to no longer contagious/cured


bioMin[3] <-    3             #symptoms + contagion
bioMax[3] <- 8                #symptoms + contagion max
bioTransition[3,4] <-  .95    #transition to no longer contagious
bioTransition[3,7] <-  .05    #transition to death state 


bioMin[4] <- 1          #symptoms but no longer contagious
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

#############################################################################
#
# Rudimentary biotransition graph
#
############################################################################

# Convert the bioTransition matrix to a graph object
bioTransitionGraph <- function() {
  g <- graph_from_adjacency_matrix(bioTransition, mode = "directed", weighted = TRUE)
  E(g)$label <- E(g)$weight
  V(g)$label <- STATENAMES
  return(g)
}



#############################################################################
#
# Defining networks
#
############################################################################

makeNetwork<- function(numAgents,numsets=3,steps=1,power=1)
{
  ord <- sample(numAgents)
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

## This tries to make a network with families, schools, workplaces..
## 
makeDemographicNetwork <- function(numAgents=1000, 
                                   householdsize=2.5,
                                   numSchools=4,
                                   numWorkplaces=25,
                                   agePyramid = c(20,8,10,12,14,13,12,11))
  
{
  
  numChildren <- floor(sum(agePyramid[1:2])/sum(agePyramid) * numAgents)
  numAdults <- numAgents-numChildren
  
  ##Adult ages age:
  
  adultAge<- sample(c(3:8)*10-5,size=numAdults,replace=T,prob=agePyramid[-(1:2)])
  childrenAge <- sample(c(5:15),size=numChildren,replace=T,prob=c(1:11))#agePyramid[1:2])
  ages <- c(adultAge,childrenAge)
  ##Each household
  numHouseholds <-floor( numAgents/householdsize)
  householdXY <- matrix(runif(numHouseholds*2)*10,ncol=2)
  
  #Now, let's assign people to households.  Adults can be assigned to anywhere;
  ##children need to be assigned to a household that already exists.
  
  ##assign 'head-of-household' ##first the adults, then the children
  myHousehold <- rep(NA,numAgents)
  myHousehold[1:numHouseholds] <- 1:numHouseholds
  myHousehold[(numHouseholds+1):numAgents] <-  sample(numHouseholds,replace=T,size=numAgents-numHouseholds)
  
  
  ##now, all households are assigned.  Let's assign workplaces and schools.  
  ##for simplicity, every adult has a workplace, and every child has a school, even though children
  ## or adults may work from home/too young for school, or be retired,
  ##wokplaces 1 is hosptial
  ## workplace 1+1;numschools are considered schools
  
  ##Workplace size is roughly zipfs-law
  workPlaces <-sample(1:numWorkplaces,size=numAdults,prob=1/(1+1:numWorkplaces),replace=T)
  schoolPlaces <- sample(1+(1:numSchools),size=numChildren,replace=T)
  work <- c(workPlaces,rep(0,numChildren))
  school <- c(rep(0,numAdults),schoolPlaces)
  pool <- list()
  for(i in 1:numAgents)
  {
    tmpAgent <- makeAgent(psychstate=1,
                          biostate=1,
                          age=ages[i])
    
    tmpAgent$household = myHousehold[i]
    tmpAgent$work = work[i]  ##work or school
    tmpAgent$school=school[i]
    tmpAgent$head <- (i<numHouseholds)
    tmpAgent$xy <- householdXY[myHousehold[i] ]
    pool[[i]] <-tmpAgent
  }
  
  
  ##now, let's lay out the social network
  schoolmat <- (outer(school,school,"==") * outer(school,school,"*"))>0
  workmat <- (outer(work,work,"==") * outer(work,work,"*"))>0
  housemat <- ( outer(myHousehold,myHousehold,"=="))>0
  agentXY <- householdXY[myHousehold,] +  matrix(.15* rnorm(length(myHousehold)*2),ncol=2)
  neighborhood <- as.matrix(dist(agentXY)) < runif(nrow(agentXY)^2)*2
  #  neighborhood <- outer(agentXY,agentXY, function(a,b){(a-b)^2 < runif(1)*.5})
  network <- schoolmat+ workmat + housemat + neighborhood
  return (list(xy=agentXY, pool=pool,network=network,
               neighborhood=neighborhood,
               worknet=workmat,
               schoolnet=schoolmat,
               housenet=housemat))
}

#############################################################################
#
# Plotting the networks
#
############################################################################
mygplot1 <- function(coord, network,states,main="",edgecol="grey40",add=F)
{
  if(is.null(coord))
  {
    coord  <- gplot.layout.fruchtermanreingold(network,layout.par=list(niter=500))
  }
  
  newmin <- mean(coord[,2]) - (-min(coord[,2]) + mean(coord[,2])) * 1.4
  palette=c("white","yellow","red","green","darkgreen","blue","black")
  if(add==F)
    plot(coord,col="black",bty="n",pch=16,cex=2.7,xaxt="n",yaxt="n",main=main,
         xlab="",ylab="",axes=F,
         ylim=c(newmin,max(coord[,2])),type="n")
  
  for(i in 1:nrow(network))
  {
    if(sum(network[i,])>0)
    {
      segments(coord[i,1],
               coord[i,2],
               coord[network[i,]>0,1,drop=F],
               coord[network[i,]>0,2,drop=F],col=edgecol)
    }
  }
  points(coord,pch=16,cex=2.3,col= palette[states])
  
  points(coord,pch=1,cex=2.3,col="black")
  legend(mean(coord[,1]),min(coord[,2]),bty='n',y.intersp=.7,cex=.8,
         STATENAMES, pch=16,col=palette)
  
  return (coord)
}

net <- makeDemographicNetwork(500, numSchools=4,
                              numWorkplaces=50)

#############################################################################
#
# DAY BY DAY SIMULATION
#
############################################################################
numAgents <- 1000
numDays <- 100
naturalImmunity <- 0
net <- makeDemographicNetwork(numAgents)

pool <- net$pool
cc <- net$xy
socialnetwork <- net$network


numInteractions <-  rep(8,numDays)  ##how many interactions per day per agent on average?
contagionProb <- rep(.03,numDays)    ##normal contagioun probability after concat
sampleFromNetwork <- rep(1.0,numDays)  ##how likely you are to stick with 'your' network

#numInteractions[10:numDays] <- 3     ##quarantiine goes into effect day 3.
#numInteractions[25:numDays] <- 15  ##re-open 

plotNetwork <- TRUE
#re-use previous network
#socialnetwork <-makeNetwork(numAgents,numsets=1,power=.5)

disthistory <- matrix(NA,ncol=7,nrow=numDays)

pool <- list()
for(i in 1:numAgents)
{
  pool[[i]] <- makeAgent(psychstate=1,
                         biostate=sample(c(1,6),
                                         p=c(1-naturalImmunity, naturalImmunity),1))
}

##infect patient 0
numInfected <- 5

# Other libraries and your custom functions (`makeAgent`, `setAgentState`, `updateAgent`, etc.)

# Define UI
ui <- fluidPage(
  titlePanel("Day by Day Spread of Infection Simulation"),
  sidebarLayout(
    sidebarPanel(
      #should be in a reactive element
      sliderInput("numDaysSlider", "Select Number of Days to Simulate:", 
                  min = 1, max = 100, value = 50),
      actionButton("submit", "Run Simulation")
    ),
    mainPanel(
      plotOutput("dailyPlot")
    )
  )
)

 
# Define server logic
server <- function(input, output) {
  # Reactive values to store the simulation state
  simulationResults <- reactiveValues()
  
  observeEvent(input$submit, {
    numDays <- input$numDaysSlider  # Get the number of days from the slider input
    numAgents <- 1000
    naturalImmunity <- 0
    net <- makeDemographicNetwork(numAgents)
    pool <- net$pool
    cc <- net$xy
    socialnetwork <- net$network
    numInteractions <- rep(8, numDays)
    contagionProb <- rep(.03, numDays)
    sampleFromNetwork <- rep(1.0, numDays)
    numInfected <- 5  # Number of initially infected agents
    
    # Infect a few agents
    for(i in sample(numAgents, numInfected)) {
      pool[[i]] <- setAgentState(pool[[i]], 2)
    }
    
    # Initialize state distribution history
    disthistory <- matrix(NA, ncol = 7, nrow = numDays)
    
    # Day-by-day simulation
    for(day in 1:numDays) {
      # Interactions of the day
      for(i in 1:numAgents * numInteractions[day]) {
        sneezer <- sample(numAgents, 1)
        sneezedon <- if(runif(1) < sampleFromNetwork[day]) {
          sample(numAgents, 1)
        } else {
          sample(which(socialnetwork[sneezer, ] > 0), 1)
        }
        
        # Infection logic
        if(pool[[sneezer]]$biostate == 2 && pool[[sneezedon]]$biostate == 1 && runif(1) < contagionProb[day]) {
          pool[[sneezedon]] <- setAgentState(pool[[sneezedon]], 2)
        }
      }
      
      # Update states
      for(i in 1:numAgents) {
        pool[[i]] <- updateAgent(pool[[i]])
      }
      
      # Record state distribution
      states <- sapply(pool, function(x) x$biostate)
      disthistory[day, ] <- table(factor(states, levels = 1:7))
      
      # Update reactive values for plotting
      simulationResults$disthistory <- disthistory
      if(plotNetwork) {
        simulationResults$plot <- mygplot(cc, socialnetwork, states, main = paste("Day", day))
      }
    }
  })
  
  output$dailyPlot <- renderPlot({
    # Plot the final state of the simulation
    if (!is.null(simulationResults$plot)) {
      print(simulationResults$plot)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

