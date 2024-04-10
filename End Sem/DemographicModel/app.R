#
# This is The End Trimester Exam for Nzambuli Daniel
# The goal is to create a shiny app that allows an interactive view of the work by
# Lamia Alam & Shane Mueller 
#
# it can be found here:
#
# https://stmueller.github.io/epidemic-agents/web/epidemic-demographics.html
#

library(shiny)
library(shinydashboard)
# library(plotly)
library(igraph)

library(ggplot2)
library(dplyr)
library(sna)
library(knitr)
library(reshape2)
library(igraph)

##############################################################
#
## The biological state has a few specific cases
#
##############################################################

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

###############################################################
#
## Timecourse and Biological model
# 1. Will model timecourse on a timecourse of 1-day units.
# 2. ONLY the natural progression of the disease
# 3. keep track of the next transition point for any state
#
###############################################################

##############
#
## make an agent
#
##############

makeAgent = function(psychstate,biostate,age=30)
{
  
  return (list(psychstate=psychstate,
               biostate=biostate,
               age=age,
               nextbiostate=NA,
               biostatecountdown=NA))
}

#############
#
## transition matrix
# 1. simplify the logic above
# 2. But we need more like a task network because the time taken to transition matters
# 3. All timing distributions are uniform with a min and max parameter for time in each state.
# 
### Remember 
#
## * 1. Unexposed
## * 2. Asymptomatic but infected/contagious
## * 3. Symptomatic and contagious
## * 4. Symptomatic and not contagious
## * 5. Post-COVID Immune
## * 6. Naturally immune (will not contract)
## * 7. Death
#
#############
bioTransition = matrix(0,STATES,STATES)
bioMin = matrix(1,STATES)      #state time minimum
bioMax = matrix(1,STATES)      #state time maximum

##################################################################
#
## Set the agent states 
#
##################################################################

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

#################################################################
#
## transition Agent
#
#################################################################

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

##############
#
## make network
#
##############

makeDemographicNetwork <- function(numAgents=1000, 
                                   householdsize,
                                   numSchools=4,  # Number of each type of school
                                   numWorkplaces=25,
                                   agePyramid = c(20,8,10,12,14,13,12,11),
                                   preSchRate,
                                   primSchRate,
                                   secSchRate,
                                   uniRate) {
  
  # Define age groups based on school types
  numPreSchool <- round(agePyramid[1] / sum(agePyramid) * numAgents * preSchRate)  # Pre-school children with graduation probability
  numPrimarySchool <- round((agePyramid[2] + agePyramid[3]) / sum(agePyramid) * numAgents * primSchRate) # Primary school children
  numSecondarySchool <- round(agePyramid[4] / sum(agePyramid) * numAgents *  secSchRate) # Secondary school children
  numUniversity <- round(agePyramid[5] / sum(agePyramid) * numAgents *  uniRate) # University students
  
  # Determine age for each group
  ages <- c(sample(3:5, numPreSchool, replace = T),  # Assuming average pre-school age
            sample(5:15, numPrimarySchool, replace = T),  # Average primary school age
            sample(15:19, numSecondarySchool, replace = T),  # Average secondary school age
            sample(18:24, numUniversity, replace = T),  # Average university age
            sample(25:75, numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity, replace=TRUE))  # Adults
  
  # Determine age for each group and assign education level
  age_levels <- c(rep("PreSchool", numPreSchool),  
                  rep("Primary", numPrimarySchool),
                  rep("Secondary", numSecondarySchool),
                  rep("University", numUniversity),
                  rep("Working", numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity))
  
  # data for the ages 
  ageData = data.frame(Age = ages, Level = as.factor(age_levels))
  
  numHouseholds <- floor(numAgents / householdsize)
  householdXY <- matrix(runif(numHouseholds * 2) * 10, ncol=2)
  
  myHousehold <- sample(1:numHouseholds, size=numAgents, replace=T)
  
  # Assign individuals to schools and workplaces
  schoolTypes <- c(rep("Pre-school", numPreSchool),
                   rep("Primary", numPrimarySchool),
                   rep("Secondary", numSecondarySchool),
                   rep("University", numUniversity),
                   rep("None", numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity))
  
  employed <- sample(1:numAgents, round(0.75 * (numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity)), replace=FALSE)
  employmentStatus <- rep(FALSE, numAgents)
  employmentStatus[employed] <- TRUE
  
  work <- ifelse(employmentStatus, sample(1:numWorkplaces, length(employed), replace=TRUE), 0)
  school <- match(schoolTypes, c("Pre-school", "Primary", "Secondary", "University", "None"))  # Simple mapping for illustration
  
  pool <- list()
  for(i in 1:numAgents) {
    tmpAgent <- makeAgent(psychstate=1, biostate=1, age=ages[i])
    tmpAgent$household <- myHousehold[i]
    tmpAgent$work <- work[i]
    tmpAgent$school <- school[i]
    tmpAgent$head <- (i <= numHouseholds)
    tmpAgent$xy <- householdXY[myHousehold[i]]
    pool[[i]] <- tmpAgent
  }
  
  # Create matrices for connections within schools, workplaces, and households
  schoolmat <- (outer(school, school, "==") * outer(school, school, "*")) > 0
  workmat <- (outer(work, work, "==") * outer(work, work, "*")) > 0
  housemat <- (outer(myHousehold, myHousehold, "==")) > 0
  agentXY <- householdXY[myHousehold, ] + matrix(.15 * rnorm(length(myHousehold) * 2), ncol=2)
  neighborhood <- as.matrix(dist(agentXY)) < runif(nrow(agentXY)^2) * 2
  
  network <- schoolmat + workmat + housemat + neighborhood
  
  return(list(xy=agentXY, pool=pool, network=network,
              neighborhood=neighborhood,
              worknet=workmat,
              schoolnet=schoolmat,
              housenet=housemat,
              ages = ageData))
}


#################################################################
#
## The network with homesteads
#
#################################################################

makeTransNetwork <- function(numAgents=1000, 
                                   householdsize=2.5,
                                   numPreSchools,  # Number of pre-schools
                                   numPriSchools,  # Number of primary schools
                                   numSecSchools,  # Number of secondary schools
                                   numUniversities,  # Number of universities
                                   numWorkplaces=25,
                                   agePyramid = c(20,8,10,12,14,13,12,11)) {
  
  # Calculate the number of individuals in each education level based on agePyramid and other parameters
  numPreSchool <- round(agePyramid[1] / sum(agePyramid) * numAgents)  # Pre-school children
  numPrimarySchool <- round((agePyramid[2] + agePyramid[3]) / sum(agePyramid) * numAgents)  # Primary school children
  numSecondarySchool <- round(agePyramid[4] / sum(agePyramid) * numAgents)  # Secondary school children
  numUniversity <- round(agePyramid[5] / sum(agePyramid) * numAgents)  # University students
  
  
  # Determine age for each group
  ages <- c(sample(3:5, numPreSchool, replace = TRUE),  # Pre-school age
            sample(6:14, numPrimarySchool, replace = TRUE),  # Primary school age
            sample(15:19, numSecondarySchool, replace = TRUE),  # Secondary school age
            sample(20:24, numUniversity, replace = TRUE),  # University age
            sample(25:75, numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity, replace=TRUE))  # Adults
  
  numHouseholds <- floor(numAgents / householdsize)
  householdXY <- matrix(runif(numHouseholds * 2) * 10, ncol=2)
  
  myHousehold <- sample(1:numHouseholds, size=numAgents, replace=T)
  
  # Assign individuals to schools and workplaces
  school <- c(sample(1:numPreSchools, numPreSchool, replace=TRUE),
              sample(numPreSchools+1:numPreSchools+numPriSchools, numPrimarySchool, replace=TRUE),
              sample(numPreSchools+numPriSchools+1:numPreSchools+numPriSchools+numSecSchools, numSecondarySchool, replace=TRUE),
              sample(numPreSchools+numPriSchools+numSecSchools+1:numPreSchools+numPriSchools+numSecSchools+numUniversities, numUniversity, replace=TRUE),
              rep(0, numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity))  # Non-students have no school
  
  employed <- sample(1:numAgents, round(0.75 * (numAgents - numPreSchool - numPrimarySchool - numSecondarySchool - numUniversity)), replace=FALSE)
  employmentStatus <- rep(FALSE, numAgents)
  employmentStatus[employed] <- TRUE
  
  work <- ifelse(employmentStatus, sample(1:numWorkplaces, length(employed), replace=TRUE), 0)
  
  pool <- list()
  for(i in 1:numAgents) {
    tmpAgent <- makeAgent(psychstate=1, biostate=1, age=ages[i])
    tmpAgent$household <- myHousehold[i]
    tmpAgent$work <- work[i]
    tmpAgent$school <- school[i]
    tmpAgent$head <- (i <= numHouseholds)
    tmpAgent$xy <- householdXY[myHousehold[i]]
    pool[[i]] <- tmpAgent
  }
  
  # Creating separate matrices for each type of school
  preSchoolmat <- outer(school, school, "==") * (school <= numPreSchools & school > 0)
  priSchoolmat <- outer(school, school, "==") * (school > numPreSchools & school <= numPreSchools + numPriSchools)
  secSchoolmat <- outer(school, school, "==") * (school > numPreSchools + numPriSchools & school <= numPreSchools + numPriSchools + numSecSchools)
  uniSchoolmat <- outer(school, school, "==") * (school > numPreSchools + numPriSchools + numSecSchools)
  
  # Combined school network
  combinedSchoolmat <- preSchoolmat | priSchoolmat | secSchoolmat | uniSchoolmat
  
  
  # schoolmat <- (outer(school, school, "==") * outer(school, school, "*")) > 0
  workmat <- (outer(work, work, "==") * outer(work, work, "*")) > 0
  housemat <- (outer(myHousehold, myHousehold, "==")) > 0
  agentXY <- householdXY[myHousehold, ] + matrix(.15 * rnorm(length(myHousehold) * 2), ncol=2)
  neighborhood <- as.matrix(dist(agentXY)) < runif(nrow(agentXY)^2) * 2
  
  network <- schoolmat + workmat + housemat + neighborhood
  
  return(list(xy=agentXY, pool=pool, network=network,
              neighborhood=neighborhood,
              worknet=workmat,
              schoolnet=combinedSchoolmat,
              preSchoolnet=preSchoolmat, 
              priSchoolnet=priSchoolmat, 
              secSchoolnet=secSchoolmat, 
              uniSchoolnet=uniSchoolmat,
              housenet = housemat))
}

#################################################################
#
## plot the network
#
#################################################################

mygplot <- function(coord, network,states,main="",edgecol="grey40",add=F)
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

#################################################################
#
## The shinny app
#
#################################################################

# Define UI for application that draws a histogram
ui = dashboardPage(
  dashboardHeader(title = "BioTransition Model"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      tabPanel( "transition Probabilities",
        h3("This tab illustrates the state transition probabilities."),
        h1(" "),
        sliderInput("2_time", "Time in Asymptomatic Stage", 1, 25, c(3, 15)),
        sliderInput("3_time", "Time in Asymptomatic Stage", 1, 25, c(3, 8)),
        sliderInput("4_time", "Time in Asymptomatic Stage", 1, 25, c(1, 7)),
        sliderInput("Ass_2_sym", "Transition Probability to infected with symptoms", 0, 1, 0.15),
        sliderInput("Sym_2_Death", "Transition to Death", 0, 1, 0.05),
        actionButton("updatePlot", "Render Plot"),
        plotOutput("plot1")  # first tab state transition
      ),
      tabPanel( "Age Distribution",
        h3("The age distribution across the population"),
        h1(" "),
        sliderInput("pre_sch", "Graduation rate from Pre-School", 0, 1, 0.053),
        sliderInput("prim_sch", "Graduation rate from Primary School", 0, 1, 0.498),
        sliderInput("sec_sch", "Graduation rate from Secondary School", 0, 1, 0.245),
        sliderInput("uni", "Graduation rate from University", 0, 1, 0.071),
        actionButton("pyramidPlot", "Render AgePyramid"),
        plotOutput("agePyramidPlot") # second plot of the age pyramid
      ),
      tabPanel("Households",
               h3("The distribution of households in the environment"),
               sliderInput("num_Agents", "The number of Agents", 100, 3000, 1000),
               sliderInput("num_PreSch", "The number of Pre-Schools", 1, 40, 4),
               sliderInput("num_Pri", "The number of Primary Schools", 1, 20, 10),
               sliderInput("num_Sec", "The number of Secondary Schools", 1, 15, 5),
               sliderInput("num_Uni", "The number of Universities", 1, 7, 5),
               sliderInput("num_Wrk", "The number of Work Places", 10, 50, 25),
               actionButton("HousePlot", "Render Households"),
               plotOutput("householdPlot")
      ),
      tabPanel("School Network",
               h3("The distribution of households in the environment"),
               sliderInput("num_S_Agents", "The number of Agents", 100, 3000, 1000),
               sliderInput("num_S_PreSch", "The number of Pre-Schools", 1, 40, 4),
               sliderInput("num_S_Pri", "The number of Primary Schools", 1, 20, 10),
               sliderInput("num_S_Sec", "The number of Secondary Schools", 1, 15, 5),
               sliderInput("num_S_Uni", "The number of Universities", 1, 7, 5),
               sliderInput("num_S_Wrk", "The number of Work Places", 10, 50, 25),
               actionButton("School_Net", "Render School Net"),
               plotOutput("SchoolNet")
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  # Example data setup
  STATES = 7
  
  plotData <- eventReactive(input$updatePlot, {
    mtrans <- matrix(0, STATES, STATES)
    mtrans[2, 3] <- input$Ass_2_sym
    mtrans[2, 5] <- (1- input$Ass_2_sym)
    mtrans[3,4] <-  (1 - input$Sym_2_Death)
    mtrans[3, 7] <- input$Sym_2_Death
    mtrans[4,5] <- 1
    graph <- graph_from_adjacency_matrix(mtrans, mode = "directed", weighted = TRUE)
    V(graph)$name <- STATELABELS
    graph
  })
  
  # Define plots
  
  ###############################################################
  #
  ## Transition States
  #
  ###############################################################
  output$plot1 = renderPlot({
    # create the plot for the first transition, weight based on bioTransition values
    g <- plotData()
    if (!is.null(g)) {
      plot.igraph(g, edge.arrow.size=.5, edge.label = E(g)$weight,
                  vertex.shape="square", vertex.label=V(g)$name,
                  vertex.size=35, vertex.label.cex=1.2, vertex.label.color="black",
                  vertex.label.family="sans", edge.label.cex=1.0, edge.label.color="red")
    }
  }, width = 500, height = 500)
  
  
  ###############################################################
  #
  ## age pyramid
  #
  ###############################################################
  
  demographicNetwork <- eventReactive(input$pyramidPlot,{
    makeDemographicNetwork(householdsize = 2.5,
                           preSchRate = input$pre_sch, 
                           primSchRate = input$prim_sch, 
                           secSchRate = input$sec_sch, 
                           uniRate = input$uni)
  })
  
  ################
  #
  ## plot the age pyramid
  #
  ################
  
  output$agePyramidPlot <- renderPlot({
    data <- demographicNetwork()
    ages <- data$ages  # Extract ages from your data structure
    
    # Create the histogram of ages
    # Check if 'ages' is directly accessible and is the correct data we want to plot
    if (!is.null(ages)) {
      # Create the histogram of ages directly from the vector
      ggplot(data = ages, aes(x=Age, fill = Level)) +
        geom_histogram(binwidth=1, color='black') +
        labs(title="Age Pyramid by Education Level", x="Age", y="Frequency") +
        scale_fill_brewer(palette="Set1") +
        theme_minimal()
    }
  })
  
  ################
  #
  ## plot the household network
  #
  ################

  houseHolds <- eventReactive(input$HousePlot, {
    makeTransNetwork(numAgents = input$num_Agents, householdsize = 2.5, numPreSchools = input$num_PreSch, numPriSchools = input$num_Pri, numSecSchools = input$num_Sec, numUniversities = input$num_Uni, numWorkplaces = input$num_Wrk)
  })
  
  output$householdPlot <- renderPlot({
    net <- houseHolds()
    if (!is.null(net) && !is.null(net$xy) && !is.null(net$housenet)) {
      mygplot(coord=net$xy, network=net$housenet, states=rep(1, nrow(net$housenet)), main="Households", edgecol="blue")
    }
  }, height = 900, width = 900)
  
  ################
  #
  ## plot the School network
  #
  ################
  
  schoolNetwork <- eventReactive(input$School_Net, {
    makeTransNetwork(numAgents = input$num_Agents, householdsize = 2.5, numPreSchools = input$num_PreSch, numPriSchools = input$num_Pri, numSecSchools = input$num_Sec, numUniversities = input$num_Uni, numWorkplaces = input$num_Wrk)
  })
  
  output$SchoolNet <- renderPlot({
    net <- schoolNetwork()
    if (!is.null(net) && !is.null(net$schoolnet)) {
      mygplot(coord = net$xy, network = net$schoolnet, states = rep(1, nrow(net$schoolnet)), main = "School Network", edgecol = "orange")
    }
  }, height = 900, width = 900)
    
  
  
}

# Run the application 
shinyApp(ui, server)
