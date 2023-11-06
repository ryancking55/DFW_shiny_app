# Fall 2019 - Fall 2022 DFW Report

# load packages:
library(shiny)
library(treemap)
library(dplyr)
library(data.table)




testPal = c("#ffffff", "#ffffff","#ffffff","#ffffff","#ffffff",
            "#ffffff","#f6bab8","#e17576","#c41e3aff","#9d182e")

testPal2 = c("#ffffff", "#ffffff","#ffffff","#ffffff","#ffffff",
             "#c1d1d1", "#a2bbbb","#84a5a5","#668f90","#487a7b")



# read files:
DFW_by_college_app = read.csv("DFW_by_college.csv", header = T)
DFW_by_department_app = read.csv("DFW_by_department.csv", header = T)
DFW_by_course_app = read.csv("DFW_by_course.csv", header = T)
DFW_by_instructor_app = read.csv("DFW_by_instructor.csv", header = T)


# helper function:
getLevels = function(df, subsection, grp, subgroup){
  return(
    levels(
      as.factor(
        df[[subsection]][df[[grp]] == subgroup]
        )
      )
    )
}


# app:
ui <- fluidPage(
  titlePanel("Fall 2019 - Fall 2022 DFW Report"),
  sidebarLayout(
    sidebarPanel(
      
      #img(src = "Full Logo Horizontalv2.png", align = "center"),
      img(src = "DSIR Report Logov3.jpg", align = "left"),
      
      selectInput(
        "select", "Department", choices = list(
        "All" = c("All"),
        "COSTEM" = c(getLevels(DFW_by_course_app, "DEPT", "College", "COSTEM")),
        "COAL" = c(getLevels(DFW_by_course_app, "DEPT", "College", "COAL")),
        "COBH" = c(getLevels(DFW_by_course_app, "DEPT", "College", "COBH")),
        "COB" = c(getLevels(DFW_by_course_app, "DEPT", "College", "COB")),
        "COE" = c(getLevels(DFW_by_course_app, "DEPT", "College", "COE")),
        "APSU" = c(getLevels(DFW_by_course_app, "DEPT", "College", "1"))),
        selected = "All"
        ),

      uiOutput("conditional_courses")
    ),
    
    
    mainPanel(
      #img(src = "DSIR Report Logov2.jpg", align = "left"),
      tags$style(
                 type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
                 ),
      shinycssloaders::withSpinner(
        plotOutput(
          "data", height = "800px", width = "1000px"
          ), 
        type = 1, color = "blue", size = 2
        )
    )
  )
)


server <- function(input, output, session) {
  
  output$conditional_courses = renderUI({
    req(input$select != "All")
    selectInput(
          "courses", "Courses", choices = c(
          "All", 
          getLevels(DFW_by_course_app, "TITLE", "DEPT", input$select)),
          selected = "All"
          )
  })

  
  output$data = renderPlot(
    {
      if (input$select %in% c("All")) {
        DFW_by_department_app %>% filter(College != "1") %>%
        treemap( 
                index = c("College", "deptProp"), 
                vSize = "propDFW", 
                vColor = "propDFW", 
                type = "value", 
                #fun.aggregate = "mean",
                reverse.legend = TRUE,
                title = input$select,
                title.legend = "Proportion DFWs",
                palette = "RdBu",
                #palette = testPal2,
                fontsize.labels = c(35, 15),
                fontface.labels = "bold",
                fontsize.legend = 20,
                fontsize.title = 30,
                bg.labels = c("#ffffff"),
                align.labels = c("center", "center"),
                ymod.labels = c(0,0,0),
                xmod.labels = c(.1,0,0),
                border.lwds = c(7,2,1),
                border.col = c("black", "black", "black"),
                lowerbound.cex.labels = 0.5)
      }
      
      else if (input$courses %in% c("All")) {
        Sys.sleep(0.1)
        DFW_by_course_app %>% filter(DEPT %in% input$select) %>%
          treemap( 
            index = "courseProp", 
            vSize = "propDFW", 
            vColor = "propDFW", 
            type = "value", 
            #palette = testPal2,
            palette = "RdBu",
            reverse.legend = TRUE,
            title.legend = "Proportion DFWs",
            fontsize.legend = 20,
            title = input$select,
            fontsize.title = 30)
      }
      
      else {
        Sys.sleep(0.3)
        DFW_by_instructor_app %>% filter(TITLE %in% input$courses) %>%
          treemap( 
            index = "instructProp", 
            vSize = "propDFW", 
            vColor = "propDFW", 
            type = "value", 
            #palette = testPal2,
            palette = "RdBu",
            reverse.legend = TRUE,
            title.legend = "Proportion DFWs",
            fontsize.legend = 20,
            title = input$courses,
            fontsize.title = 30,
            fontsize.labels = ifelse(input$select %in% c("APSU"), 10, 15))
      }
      
    } 
    
  )}
shinyApp(ui, server)
