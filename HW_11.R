#install all important packages #

library(arules)

library(shiny)

library(rmarkdown)

library(corrplot)

library(RColorBrewer)

library(tidyverse)

library(arulesViz)
#set direcory#
getwd()
setwd('/Users/harshita/Downloads')

#read data#
data <- read_csv('employee_attrition(1).csv') 
head(data)

#get type of data in each column#
typeof(sapply(data,typeof))

#get number of unique values in each row#
lengths(lapply(data, unique))

#check number of Na values in each column and remove rows with na values #
for (col in colnames(data)){
  print(col)
  print(which(is.na(data[col])))
}

data<- na.omit(data)
 
#columns  EmployeeCount, Over18 , StandardHours just have single value in the column and thus, woldun't contibute in any analysis#
irr_col=c('EmployeeCount','Over18','StandardHours')

hist_fun <-function(column){
  x=sapply(data[column],as.numeric)
  return(hist(x, main= column, xlab=column))
}

num_col = c('Age','DailyRate','DistanceFromHome','HourlyRate','MonthlyRate','MonthlyIncome','MonthlyRate','TotalWorkingYears','YearsAtCompany','YearsInCurrentRole','YearsSinceLastPromotion','YearsWithCurrManager','NumCompaniesWorked','YearsWithCurrManager','PercentSalaryHike','PerformanceRating','RelationshipSatisfaction','StandardHours')

for (col in num_col){
  hist_fun(col)
}


M <-round(cor(data.frame('Age'=data['Age'],'DailyRate'=data['DailyRate'],'DistanceFromHome'=data['DistanceFromHome'],'HourlyRate'=data['HourlyRate'],'MonthlyRate'=data['MonthlyRate'],'MonthlyIncome'=data['MonthlyIncome'],'TotalWorkingYears'=data['TotalWorkingYears'],'YearsAtCompany'=data['YearsAtCompany'],'YearsInCurrentRole'=data['YearsInCurrentRole'],'YearsSinceLastPromotion'=data['YearsSinceLastPromotion'],'YearsWithCurrManager'=data['YearsWithCurrManager','NumCompaniesWorked'=data['NumCompaniesWorked'],'YearsWithCurrManager'=data['YearsWithCurrManager']]),use = "complete.obs"),2)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
M

# Age and TotalWorkingYears are positively correlated 0.65#
# MonthlyIncome and TotalWorkingYears 0.73#
# YearsInCurrentRole and YearsAtCompany 0.61#
# YearsSinceLastPromotion and YearsAtCompany 0.54#

boxplot(data$DistanceFromHome)
which(data$DistanceFromHome > 200)
data <- data[-which(data$DistanceFromHome > 200),]


boxplot(data$MonthlyIncome)
which(data$MonthlyIncome > 17000)
x <- c(which(data$MonthlyIncome > 17000))
barplot(table(data[x,5]))

boxplot(data$NumCompaniesWorked)
which(data$NumCompaniesWorked ==9)

boxplot(data$TotalWorkingYears)
which(data$TotalWorkingYears>100)
data<- data[-which(data$TotalWorkingYears>100),]
data<- data[-which(data$YearsWithCurrManager>100),]

x<-which(data$TotalWorkingYears>30)
barplot(table(data[x,]$JobLevel))

barplot(table(data$Attrition))
barplot(table(data$BusinessTravel))
barplot(table(data$Department))
barplot(table(data$Education))
barplot(table(data$EducationField))
barplot(table(data$EnvironmentSatisfaction))
barplot(table(data$Gender))
barplot(table(data$JobInvolvement))
barplot(table(data$JobLevel))
barplot(table(data$JobRole))
barplot(table(data$JobSatisfaction))
barplot(table(data$MaritalStatus))
barplot((table(data$OverTime)))
barplot(table(data$PerformanceRating))
barplot(table(data$RelationshipSatisfaction))
barplot(table(data$StockOptionLevel))
barplot(table(data$WorkLifeBalance))


#Discretization of continus variable#
continus_var=c('Age','DailyRate','DistanceFromHome','HourlyRate','MonthlyIncome','MonthlyRate','NumCompaniesWorked','PercentSalaryHike','TotalWorkingYears','YearsAtCompany','YearsInCurrentRole','YearsSinceLastPromotion','YearsWithCurrManager')
data['Age_dis']=data['Age']>=median(data$Age)
data['DailyRate_dis']=data['DailyRate']>=median(data$DailyRate)
data['DistanceFromHome_dis'] = data['DistanceFromHome']>=median(data$DistanceFromHome)
data['HourlyRate_dis'] = data['HourlyRate']>=median(data$HourlyRate)
data['MonthlyIncome_dis'] = data['MonthlyIncome']>=median(data$MonthlyIncome)
data['MonthlyRate_dis'] = data['MonthlyRate']>=median(data$MonthlyRate)
data['NumCompaniesWorked_dis'] = data['NumCompaniesWorked']>=2
data['PercentSalaryHike_dis'] = data['PercentSalaryHike']>=median(data$PercentSalaryHike)
data['TotalWorkingYears_dis'] = data['TotalWorkingYears']>=10
data['YearsAtCompany_dis'] = data['TotalWorkingYears']>=10
data['YearsInCurrentRole_dis'] = data['YearsInCurrentRole']>=5
data['YearsSinceLastPromotion_dis']=data['YearsSinceLastPromotion']>=3
data['YearsWithCurrManager_dis'] = data['YearsWithCurrManager']>=5
d_col = c('Age_dis','DailyRate_dis','DistanceFromHome_dis','HourlyRate_dis','MonthlyIncome_dis','NumCompaniesWorked_dis','PercentSalaryHike_dis','TotalWorkingYears_dis','YearsAtCompany_dis','YearsInCurrentRole_dis','YearsSinceLastPromotion_dis','YearsWithCurrManager_dis')

#factorize#
fac_col=c('Education','EnvironmentSatisfaction','JobInvolvement','JobLevel','JobSatisfaction','PerformanceRating','RelationshipSatisfaction','StockOptionLevel','WorkLifeBalance')
data$Education<-factor(data$Education)
data$EnvironmentSatisfaction<-factor(data$EnvironmentSatisfaction)
data$JobInvolvement<-factor(data$JobInvolvement)
data$JobLevel<-factor(data$JobLevel)
data$JobSatisfaction<-factor(data$JobSatisfaction)
data$PerformanceRating<- factor(data$PerformanceRating)
data$RelationshipSatisfaction<-factor(data$RelationshipSatisfaction)
data$StockOptionLevel <- factor(data$StockOptionLevel)
data$WorkLifeBalance<-factor(data$WorkLifeBalance)
data$Attrition<- factor(data$Attrition)
data$BusinessTravel<-factor(data$BusinessTravel)
data$Department<-factor(data$Department)
data$EducationField<-factor(data$EducationField)
data$Gender<-factor(data$Gender)
data$JobRole<-factor(data$JobRole)
data$MaritalStatus<-factor(data$MaritalStatus)
data$OverTime<-factor(data$OverTime)



#arules#
o_col=c('Attrition','BusinessTravel','Department','EducationField','Gender','JobRole','MaritalStatus','OverTime')
a_col=c(fac_col,d_col,o_col)
data <- data[,c(fac_col,d_col,o_col)] 
colnames(data)

new_data<-data[,-c(1,2,4,5,7,8,17,19,20,27)]
#rules_record <- apriori(data,appearance = list(default = "lhs", rhs = c("Attrition=No", "Attrition=Yes")),#
                        #control = list(verbose = F))#
rules_record <- apriori(new_data,appearance = list(default = "lhs", rhs = c("Attrition=No", "Attrition=Yes")),
                        control = list(verbose = F))

plot(rules_record, measure = c("support", "lift"), shading = "confidence")


#some logical tests#
which(data$YearsInCurrentRole>data$TotalWorkingYears)
data<-data[-which(data$YearsWithCurrManager>data$YearsAtCompany),]
which(data$YearsInCurrentRole>data$YearsAtCompany)
which(data$YearsSinceLastPromotion>data$YearsAtCompany)
which(data$JobSatisfaction==data$EnvironmentSatisfaction)




















#Shiny App#

ui <- fluidPage(
  
  # App title ----
  titlePanel("Top 5 Association Rules for Attrition"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "confidence",
                  label = "Confidence:",
                  min = 0, max = 1, step = 0.05,value = 0.05),
      sliderInput(inputId = "support",
                  label = "Support:",
                  min = 0, max = 0.5, step = 0.05, value = 0.05),
      
      selectInput(inputId = "att",
                  label="Att",
                  choices=c("No","Yes")),
      
      radioButtons(inputId = "sort", 
                   label = "sort_by", 
                   choices = c("support", "confidence", "lift"))
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      verbatimTextOutput(outputId = "ARules"),
      plotOutput(outputId="plot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$ARules <- renderPrint({
    
    s<-as.numeric(input$support)
    c<-as.numeric(input$confidence)
    sb<-as.character(input$sort)
    attrition <- as.character(input$att)
    attrition<-paste("Attrition=",as.character(attrition),'sep'= '')
    rules_record <- apriori(new_data,parameter = list(support = s, confidence = c),appearance = list(default = "lhs", rhs = attrition),
                            control = list(verbose = F))
    inspect(head(sort(rules_record, by = as.character(sb), descreasing = T), 5))
    
  }) 
  output$plot<-renderPlot({
    s<-as.numeric(input$support)
    c<-as.numeric(input$confidence)
    sb<-as.character(input$sort)
    attrition <- as.character(input$att)
    attrition<-paste("Attrition=",as.character(attrition),'sep'= '')
    rules_record <- apriori(new_data,parameter = list(support = s, confidence = c),appearance = list(default = "lhs", rhs = attrition),
                            control = list(verbose = F))
    plot(rules_record, measure = c("support", "lift"), shading = "confidence")
  })
}
library(shiny)
shinyApp(ui,server)



rsconnect::setAccountInfo(name='harshitaasnani',
                          token='BCD85288DD3FEF82790BC09E87774B0B',
                          secret='lom1XCJB+YnnHy/jPa6oG5bzazniJoe1NoVWu86y')


library(rsconnect)
rsconnect::deployApp('Users/harshita/Downloads/HW_11.R')

