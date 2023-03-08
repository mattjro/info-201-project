

library(shiny)
library(dplyr)
library(ggplot2)


data.exam <- read.csv(file.path("data.final", "exams (1).csv") )


ui <- fluidPage(
  navbarPage("Info201 Final Program: Explore Exam Dataset",
             tabsetPanel(
               tabPanel("Introduction", 
                        img(alt = "Image of exam", 
                            src = "https://i0.wp.com/academiamag.com/wp-content/uploads/2022/05/shutterstock_1664708983.jpg?w=860&ssl=1" ),
                        h1("Project purpose"),
                        p("The report provides a broad summary of how different factors (gender, race, parental level of education ,
                          lunch, and test preparation) affect students' scores in an exam (math, reading, 
                          and writing. With the results, we hope to infer how these different factors affect students' 
                          performance in exams in general (not only in the dataset exam), in order to help 
                          teachers improve their teaching strategies, 
                          students improve their learning strategies, and guide school administration in their policies. 
                          Also, in our website, we will focus especially on how gender is correlated with students' 
                          Academic Performance ( and test the common belief that people who identify as male are good at stem subjects while
                          people who identify as female are good at liberal art subjects) "),
                        h2("Data Set"),
                        a( href = "https://www.kaggle.com/datasets/whenamancodes/students-performance-in-exams", "click here to 
                           check these students' performance in the exams dataset"),
                        p("In this website we will explore this dataset 
                          about the performance of students in three separate exams: a math exam, a reading exam, 
                          and a writing exam. We found this dataset on kaggle.com after searching for a dataset with both numerical 
                          and categorical data involved. Aman Chauhan who is labeled as an Expert under 
                          Kaggle’s progression system collected this dataset. He is ranked 67 out of 72,128 competitors. "),
                        p("52% of the students are male and 48% are female. There are many different ethnic groups that are labeled A-E for either 
                        privacy concerns or perhaps context is not needed. Group C takes up 32% of the sample while Group D takes up about 26% of the sample. 
                        Also, one inference we can make based on the data is that these students might not all be in a similar geographical location because 
                        most districts will have either all standard lunches or all free/reduced lunches. This makes sense since some students had a test preparation 
                        course while some did not. The lunch variable is categorical, with observations being binary (two options). These can be either “standard” or “free/reduced”. 
                        The test preparation course variable is similar, being categorical and binary with the two options being “completed” or “none”. The math score, reading score, 
                        and writing score variables are all coded as numerical values. They are all within reasonable ranges which will be located below. The gender of the students are 
                        categorical being either male or female."),
                        h3("Here is a small (random) sample of data:"),
                        fluidRow(
                          column(width = 3,
                                 tableOutput("sample"))
                        )
                        
                        
                        ),
               tabPanel("The relationship between gender and subjects (plot)", 
                        h1("Score distribution"),
                        p("In this tab you can choose different sujects, and find
                          find the relation between gender (female, male) and 
                          sujects (math, reading, writing)"),
                        sidebarLayout(
                          sidebarPanel(
                        fluidRow(
                          column(width = 10, 
                                 radioButtons("subjects", "Choose the subject you 
                                              want to explore: ", choices = c("math", "reading"
                                              , "writing")
                                              )
                        ))),
                        mainPanel(
                          plotOutput("plot"),
                          textOutput("text"),
                          plotOutput("plot2")
                        ))
                        
               ),
               tabPanel("The relationship between gender and subjects (table)",
                        h1("Score table ( group by gender )"),
                        p("this tab allows users choose different exam and gender combinations. This table is arranged from the highest total scores to 
                          lowest one"),
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(width = 10,
                                     selectInput("subject2", "Choose the subject combo you
                                                 would like to explore:", choices = c("math&reading", "math&writing", 
                                                 "reading&writing", "all"))
                              )
                            ),
                            column(width = 10, 
                                   checkboxGroupInput("gender", "Choose gender:", 
                                                      choices = unique(data.exam$gender)))
                          ), 
                          mainPanel(
                            textOutput("text2"),
                            tableOutput("table")
                          )
                        )
                 
               ),
               tabPanel("the relationship between different factors and exams",
                        h1(" Average score "),
                        p("In this panel, users can explore the relationships between"),
                        p("different factors (race/ethnicity, parental level of education, lunch, test preparation course"),
                        p("and different exams (math, reading, writing)"),
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(width = 10,
                                     radioButtons("subject3", "Choose the exam (exams) you would 
                                                  like to explore", choices = c("math", "reading", "writing", "all")))),
                            column(width = 10,
                                   radioButtons("factors", "Choose the exam factor you would
                                                like to explore", choices = c("gender","race.ethnicity",
                                                                              "parental.level.of.education","lunch",
                                                                              "test.preparation.course")))
                          ),
                          mainPanel(
                            plotOutput("plot4")
                          )
                        )
                        ),
               tabPanel("Conclusion",
                        img("EXAM",
                            src = "https://img.freepik.com/free-photo/final-exam-results-test-reading-books-words-concept_53876-123721.jpg"),
                        h1("Conclusion"),
                        p("From our data, we can find (summary by factors):"),
                        tags$li("Gender: Males do better in math exam than females. However, females have higher average scores in 
                                writing and reading than males have"),
                        p(" "),
                        tags$li("Race: Group E has the highest average score in math exam, while group c has
                                the lowest score in math exam. Group E has the highest average score in reading while 
                                group C has the lowest score. Group D has the highest average score in s
                                writing exam while group C has the lowest score. Overall, Group E did 
                                best in these exams while group C did worst in these exams"),
                        p(" "),
                        tags$li("Parental level of education: students that are children of parents with a master's degree have the highest 
                               score in math, writing and reading exams while students that are children of parents with high school degree
                                have the lowest score in math, writing and reading exams"),
                        p(" "),
                        tags$li("Lunch: students with standard lunch have higher score than students with free or 
                               standard lunch. Also, we found that the students' lunch status affects math scores the most"),
                        p(" "),
                        tags$li("Test preparation course: students who completed the test 
                               preparation course have higher scores than students who 
                               did not finish the course. Also, test preparation scores 
                               affect reading exam the most."),
                        h2("Quality of Data"),
                        p("While we found our data reasonable and clear, the data leaves out some  
                          important factors such as gender, race and test preparation which are
                          the most common factors when we evaluate students' grades."),
                        p("However, it still gives some biased results. For example, for race / ethnicity, 
                          35% student from group C, 26% students from group D in this dataset. Because 
                          of the different amount of students from different race groups, when we
                          analyse the dataset, we would get the biased result ( different races affect 
                          student performance ). Also, because of the biased result, if some teachers check 
                          this result, they may pay more attention on students who come from race group E, and 
                          ignore students who come from race group C (becasue students from group E got the highest 
                          score in these exams while students from group C got the lowest score )
                          "),
                        h3("Future Idea"),
                        p("For future research, we would like to include datasets that 
                        contain more different factors and exams. For example, we can add more 
                          exams like Chinese, chemistry ... Also, to get unbiased results, 
                          we will try our best to find datasets that contain the same or similar amount of
                          samples in each factor")
                        
                        
                        )
  )             )
)


server <- function(input, output) {
  
  output$sample <- renderTable({
    data.exam %>% 
      sample_n(6)
  })
  
  output$plot <- renderPlot({
    if (input$subjects == "math") {
      ggplot(data.exam,aes(x = math.score, fill = gender)) +
      geom_histogram(binwidth = 5,position = "dodge") +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "math socre", y = "number of students")
    } else if (input$subjects == "reading") {
      ggplot(data.exam,aes(x = reading.score, fill = gender)) +
        geom_histogram(binwidth = 5,position = "dodge") +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "reading socre", y = "number of students")
    } else {
      ggplot(data.exam,aes(x = writing.score, fill = gender)) +
        geom_histogram(binwidth = 5,position = "dodge") +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
        labs(x = "writing socre", y = "number of students")
    }
      
  })
  
  output$plot2 <- renderPlot({
    data3 <- data.exam %>% 
      group_by(gender) %>% 
      summarise(average = mean(math.score))
    if (input$subjects == "math") {
      ggplot(data3, aes(gender, average)) +
        geom_col(fill = c("chartreuse", "blue4"))
    } else if (input$subjects == "reading") {
      data3 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(reading.score))
      ggplot(data3, aes(gender, average)) +
        geom_col(fill = c("aquamarine", "blue3"))
    } else {
      data3 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(writing.score))
      ggplot(data3, aes(gender, average)) +
        geom_col(fill = c("chartreuse3", "blue2"))
    } 
  })
  
  
  output$text <- renderText({
    if (input$subjects == "math") {
      data1 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(math.score, na.rm = TRUE))
      if (data1$average[data1$gender == "female"] > data1$average[data1$gender == "male"]) {
        paste("the avergae math score for female is higher than male") 
      } else {
        paste("the avergae math score for male ",
              data1$average[data1$gender == "male"],"is higher than female ",
              data1$average[data1$gender == "female"])
      }
    } else if (input$subjects == "reading") {
      data1 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(reading.score, na.rm = TRUE))
      if (data1$average[data1$gender == "female"] > data1$average[data1$gender == "male"]) {
        paste("the avergae reading score for female", data1$average[data1$gender == "female"],
              "is higher than male", data1$average[data1$gender == "male"]) 
      } else {
        paste("the avergae reading score for male ",
              data1$average[data1$gender == "male"],"is higher than female ",
              data1$average[data1$gender == "female"])
      }
    } else {
      data1 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(writing.score))
      if (data1$average[data1$gender == "female"] > data1$average[data1$gender == "male"]) {
        paste("the avergae writing score for female", data1$average[data1$gender == "female"],
              "is higher than male", data1$average[data1$gender == "male"]) 
      } else {
        paste("the avergae writing score for male ",
              data1$average[data1$gender == "male"],"is higher than female ",
              data1$average[data1$gender == "female"])
      }
    }
  })
  
  
  output$table <- renderTable({
    if (input$subject2 == "math&reading") {
      data.exam %>% 
      mutate(total.score = math.score + reading.score) %>% 
      filter(gender %in% input$gender) %>% 
      select(gender, math.score, reading.score, total.score) %>% 
      arrange(-total.score)
    } else if (input$subject2 == "math&writing") {
      data.exam %>% 
        mutate(total.score = math.score + writing.score) %>% 
        filter(gender %in% input$gender) %>% 
        select(gender, math.score, writing.score, total.score) %>% 
        arrange(-total.score)
    } else if (input$subject2 == "reading&writing") {
      data.exam %>% 
        mutate(total.score = reading.score + writing.score) %>% 
        filter(gender %in% input$gender) %>% 
        select(gender, reading.score, writing.score, total.score) %>% 
        arrange(-total.score)
    } else {
      data.exam %>% 
        mutate(total.score = reading.score + writing.score + math.score) %>% 
        filter(gender %in% input$gender) %>% 
        select(gender, reading.score, writing.score, math.score, total.score) %>% 
        arrange(-total.score)
    }
    
  })
  
  output$text2 <- renderText({
    data3 <- data.exam %>% 
      mutate(total.score = reading.score + writing.score + math.score) %>% 
      group_by(gender) %>% 
      summarise(average = mean(total.score))
    paste("the averge socre in this exam ^^ Female:", data3$average[data3$gender == "female"],
          "and", "Male:", data3$average[data3$gender == "male"])
  })
  
  
  
  output$plot4 <- renderPlot({
    if (input$subject3 == "math" & input$factors == "gender") {
   data5 <- data.exam %>% 
     group_by(gender) %>% 
     summarise(average = mean(math.score))
      ggplot(data5,aes(gender, average))+
      geom_col(fill = c("blue3","red3"))
    } else if (input$subject3 == "math" & input$factors == "race.ethnicity") {
      data5 <- data.exam %>% 
        group_by(race.ethnicity) %>% 
        summarise(average = mean(math.score))
      ggplot(data5,aes(race.ethnicity, average, fill = race.ethnicity))+
        geom_col( )
    } else if (input$subject3 == "math" & input$factors == "parental.level.of.education") {
      data5 <- data.exam %>% 
        group_by(parental.level.of.education) %>% 
        summarise(average = mean(math.score))
      ggplot(data5,aes(parental.level.of.education, average, fill = parental.level.of.education))+
        geom_col()
    }  else if (input$subject3 == "math" & input$factors == "lunch") {
      data5 <- data.exam %>% 
        group_by(lunch) %>% 
        summarise(average = mean(math.score))
      ggplot(data5,aes(lunch, average, fill = lunch))+
        geom_col()
    } else if (input$subject3 == "math" & input$factors == "test.preparation.course") {
      data5 <- data.exam %>% 
        group_by(test.preparation.course) %>% 
        summarise(average = mean(math.score))
      ggplot(data5,aes(test.preparation.course, average, fill = test.preparation.course))+
        geom_col()
    } else if (input$subject3 == "reading" & input$factors == "gender") {
      data5 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(reading.score))
      ggplot(data5,aes(gender, average))+
        geom_col(fill = c("blue3","red3"))
    } else if (input$subject3 == "reading" & input$factors == "race.ethnicity") {
      data5 <- data.exam %>% 
        group_by(race.ethnicity) %>% 
        summarise(average = mean(reading.score))
      ggplot(data5,aes(race.ethnicity, average, fill = race.ethnicity))+
        geom_col( )
    } else if (input$subject3 == "reading" & input$factors == "parental.level.of.education") {
      data5 <- data.exam %>% 
        group_by(parental.level.of.education) %>% 
        summarise(average = mean(reading.score))
      ggplot(data5,aes(parental.level.of.education, average, fill = parental.level.of.education))+
        geom_col()
    }  else if (input$subject3 == "reading" & input$factors == "lunch") {
      data5 <- data.exam %>% 
        group_by(lunch) %>% 
        summarise(average = mean(reading.score))
      ggplot(data5,aes(lunch, average, fill = lunch))+
        geom_col()
    } else if (input$subject3 == "reading" & input$factors == "test.preparation.course") {
      data5 <- data.exam %>% 
        group_by(test.preparation.course) %>% 
        summarise(average = mean(reading.score))
      ggplot(data5,aes(test.preparation.course, average, fill = test.preparation.course))+
        geom_col()
    }  else if (input$subject3 == "writing" & input$factors == "gender") {
      data5 <- data.exam %>% 
        group_by(gender) %>% 
        summarise(average = mean(writing.score))
      ggplot(data5,aes(gender, average))+
        geom_col(fill = c("blue3","red3"))
    } else if (input$subject3 == "writing" & input$factors == "race.ethnicity") {
      data5 <- data.exam %>% 
        group_by(race.ethnicity) %>% 
        summarise(average = mean(writing.score))
      ggplot(data5,aes(race.ethnicity, average, fill = race.ethnicity))+
        geom_col( )
    } else if (input$subject3 == "writing" & input$factors == "parental.level.of.education") {
      data5 <- data.exam %>% 
        group_by(parental.level.of.education) %>% 
        summarise(average = mean(writing.score))
      ggplot(data5,aes(parental.level.of.education, average, fill = parental.level.of.education))+
        geom_col()
    }  else if (input$subject3 == "writing" & input$factors == "lunch") {
      data5 <- data.exam %>% 
        group_by(lunch) %>% 
        summarise(average = mean(writing.score))
      ggplot(data5,aes(lunch, average, fill = lunch))+
        geom_col()
    } else if (input$subject3 == "writing" & input$factors == "test.preparation.course") {
      data5 <- data.exam %>% 
        group_by(test.preparation.course) %>% 
        summarise(average = mean(writing.score))
      ggplot(data5,aes(test.preparation.course, average, fill = test.preparation.course))+
        geom_col()
    } else if (input$subject3 == "all" & input$factors == "gender") {
      data5 <- data.exam %>%
        mutate(all = math.score + reading.score + writing.score) %>% 
        group_by(gender) %>% 
        summarise(average = mean(all))
      ggplot(data5,aes(gender, average))+
        geom_col(fill = c("blue3","red3"))
    } else if (input$subject3 == "all" & input$factors == "race.ethnicity") {
      data5 <- data.exam %>% 
        mutate(all = math.score + reading.score + writing.score) %>% 
        group_by(race.ethnicity) %>% 
        summarise(average = mean(all))
      ggplot(data5,aes(race.ethnicity, average, fill = race.ethnicity))+
        geom_col( )
    } else if (input$subject3 == "all" & input$factors == "parental.level.of.education") {
      data5 <- data.exam %>% 
        mutate(all = math.score + reading.score + writing.score) %>% 
        group_by(parental.level.of.education) %>% 
        summarise(average = mean(all))
      ggplot(data5,aes(parental.level.of.education, average, fill = parental.level.of.education))+
        geom_col()
    }  else if (input$subject3 == "all" & input$factors == "lunch") {
      data5 <- data.exam %>%
        mutate(all = math.score + reading.score + writing.score) %>% 
        group_by(lunch) %>% 
        summarise(average = mean(all))
      ggplot(data5,aes(lunch, average, fill = lunch))+
        geom_col()
    } else if (input$subject3 == "all" & input$factors == "test.preparation.course") {
      data5 <- data.exam %>%
        mutate(all = math.score + reading.score + writing.score) %>% 
        group_by(test.preparation.course) %>% 
        summarise(average = mean(all))
      ggplot(data5,aes(test.preparation.course, average, fill = test.preparation.course))+
        geom_col()
    }   
  })
    

}


shinyApp(ui = ui, server = server)
