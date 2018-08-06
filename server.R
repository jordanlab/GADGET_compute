library(shinydashboard, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(leaflet, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(stargazer, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(jsonlite, lib.loc = "/projects/grs-data/R/x86_64-redhat-linux-gnu-library/3.4/")
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(stringr)
library(DT)
library(gridExtra)
library(ggvis)


createLink <- function(val) {
  sprintf('<a href="https://www.ebi.ac.uk/gwas/search?query=%s" target="_blank">%s</a>',val,val)
}
options(shiny.reactlog = FALSE)
source("summ.R")
source("create_forked_task.R")


readGrs = function(fileName, sampleInfo) {
  grs = my.read.table(fileName, comment.char = "##")
  colnames(grs) = c("IND", "GRS")
  grs = merge(grs, sampleInfo, by = "IND")
}

my.read.table = function(fileName, comment.char) {
  clean.lines = sub(paste0(comment.char, ".*"), "", readLines(fileName))
  clean.lines = sub("SAMPLE.*", "", clean.lines)
  read.table(text = paste(clean.lines, collapse = "\n"))
}
# metadata files
sampleInfo = read.csv("./data/20130606_sample_info.csv", fileEncoding = "latin1")
sampleInfo = sampleInfo[, c(1, 3, 5)]
colnames(sampleInfo) = c("IND", "POP", "GEN")
supInfo = read.csv("./data/super_population_info.csv", fileEncoding = "latin1")
supInfo = supInfo[, c(1, 3, 7)]
colnames(supInfo) = c("POP", "SUP", "FULL")
sampleInfo = merge(sampleInfo, supInfo, by = "POP")
supNames = c("AFR", "EUR", "AMR", "SAS", "EAS")
c5 = c("#484496", "#F4A500", "#328A4F", "#944116", "#D92414")
names(c5) = supNames


plotGRS <- function(outdir) {
  filelist = dir(paste(outdir, "/", sep = ""), pattern = "*.txt")
  for (i in 1:length(filelist)) {
    pdfDir = paste("mkdir -p ", outdir, "/pdf", sep = "")
    system(pdfDir)
    pdfOut = paste0(outdir, "/pdf/", gsub("txt", "pdf", filelist[i]))
    file = paste(outdir, "/", filelist[i], sep = "")
    grs = readGrs(file, sampleInfo)
    
    # ANOVA
    grsMod = lm(GRS ~ SUP, data = grs)
    grsAno = anova(grsMod)
    fstat = grsAno$`F value`[1]
    pval = grsAno$`Pr(>F)`[1]
    
    # Medians
    grsdt = as.data.table(grs)
    grsdt$GRS = as.numeric(grsdt$GRS)
    grsdt[, SUPMED := median(GRS), by = SUP]
    medians = as.data.frame(grsdt[which(!duplicated(grsdt$SUP)), 5:7, with = F])
    orderedSup = medians$SUP[order(medians$SUPMED)]
    grs$SUP = ordered(grs$SUP, levels = orderedSup)
    
    # Boxplot
    g <- ggplot(grs, aes(x = SUP, y = GRS)) +
      geom_point(position = position_jitter(width = 0.6, height = 0), aes(col = SUP)) +
      scale_color_manual(values = c5, guide = F) +
      geom_boxplot(outlier.shape = NA, alpha = 0.3) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(
        title = gsub(".txt", "", filelist[i]),
        x = paste(
          "F value=",
          format(fstat, digit = 5),
          " P-value =",
          format(pval, digit = 3)
        )
      )
    pdf(pdfOut)
    plot(g)
    dev.off()
  }
  tarFiles = paste0("zip -j ", outdir, "/plots.zip ", outdir, "/pdf/*.pdf")
  system(tarFiles)
}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}


shinyServer(function(input, output, session) {


  makeReactiveBinding("task")
  task <- NULL
  newwd = paste(tempdir(), format(Sys.time(), "%Y%m%d%H%M%S"), sep = "/")
  dir.create(newwd)
  newwd
  observe({
    toggle(condition = input$submit != 0L, selector = "#GRS.nav.nav-tabs.shiny-tab-input.shiny-bound-input li a[data-value=plots]")
  })
  hide(selector = "#navbar li a[data-value=mytab2]")
  observe({
    toggleState("batchSubmit", isValidEmail(input$email) && !is.null(input$batchFile))
  })
  observe({
    toggle(condition = isValidEmail(input$email) && !is.null(input$batchFile), selector = "#GRS.nav.nav-tabs.shiny-tab-input.shiny-bound-input li a[data-value=Batch]")
  })
  observeEvent(input$batchSubmit,{
    updateTabsetPanel(session, "GRS", selected = "Batch")
    shinyjs::disable("batchSubmit")
  
    if (isValidEmail(input$email)){
      if (is.null(input$batchFile))
        return()
      baseName = tempfile(tmpdir='')
      batchPath = paste0('/projects/grs-data/batchSubmission/',baseName,".list")
      emailPath = paste0(batchPath,".email")
      file.copy(input$batchFile$datapath, batchPath)
      queueSize <- length(list.files('/projects/grs-data/batchSubmission/', "*.list"))
      fileConn <- file(emailPath)
      writeLines(c(input$email),fileConn)
      close(fileConn)
      if (queueSize-1 == 1){
        outText <- paste0("There is <b>1</b> submission ahead of you")
      }
      else{
        outText <- paste0("There are <b>", queueSize-1, "</b> submissions ahead of you")
      } 
      

      output$submission <- renderUI({ 
        HTML(paste("","","<h3>Your submission has been added to the queue!</h3>",
          "<h4>You'll be emailed an HTML report of your results when the analysis is completed",outText,
          "</h4>","<pre>Please add gadget@biosci.gatech.edu to your contacts to ensure delivery</pre>", sep = '<br/>'))
      })
      Sys.sleep(.5)
      shinyjs::enable("batchSubmit")
    }
    else{
      output$submission <- renderText({ 
      "Your email could not be validated, please try again"
      })
    }
  }
  )
  
  observeEvent(input$submit, {
    # switch tab
    updateTabsetPanel(session, "GRS", selected = "plots")
    outdir = paste0(newwd, "/")
    if (input$entryMethod == "paste") {
      df <- data.frame(t(matrix(
        unlist(input$hot$params$data), nrow = 4
      )))
      colnames(df) = c("", "", "", "")
      
    }
    if (input$entryMethod == "upload") {
      df = read.table(input$snpfile$datapath, sep = "\t")
      colnames(df) = c("", "", "", "")
    }
    
    isolate(write.table(
      df,
      file = paste0(newwd, "/SNPs.tsv"),
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    ))
    
  })
  cache_tbl = NULL
  observeEvent(input$clearTable, {
    output$hot = renderRHandsontable({
      DF = data.frame(
        SNP = c("", "", "", "", ""),
        EffectAllele = c("", "", "", "", ""),
        Phenotype = c("", "", "", "", ""),
        EffectSize = c("", "", "", "", ""),
        stringsAsFactors = FALSE
      )
      rhandsontable(DF) %>%
        hot_context_menu(customOpts = list(csv = list(
          name = "Download to CSV",
          callback = htmlwidgets::JS(
            "function (key, options) {
            var csv = csvString(this);
            
            var link = document.createElement('a');
            link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
            encodeURIComponent(csv));
            link.setAttribute('download', 'data.csv');
            
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
    }"
          )
        ))) %>%
        hot_table(
          highlightCol = TRUE,
          highlightRow = TRUE,
          strict = FALSE,
          allowInvalid = TRUE
        )
    })
    reset(input$clearTable)
  })
  
  observe({
    # Observer for rendering rhandsontable
    if (input$loadTable == 0L) {
      output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
          DF = hot_to_r(input$hot)
        }  else if (!is.null(cache_tbl)) {
          DF = hot_to_r(cache_tbl)
          cache_tbl <<- NULL
        }
        else {
          DF = data.frame(
            SNP = c("", "", "", "", ""),
            EffectAllele = c("", "", "", "", ""),
            Phenotype = c("", "", "", "", ""),
            EffectSize = c("", "", "", "", ""),
            stringsAsFactors = FALSE
          )
        }
        
        rhandsontable(DF) %>%
          hot_context_menu(customOpts = list(
            csv = list(
              name = "Download to CSV",
              callback = htmlwidgets::JS(
                "function (key, options) {
                var csv = csvString(this);
                
                var link = document.createElement('a');
                link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                encodeURIComponent(csv));
                link.setAttribute('download', 'data.csv');
                
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
      }"
              )
            )
          )) %>%
          hot_table(
            highlightCol = TRUE,
            highlightRow = TRUE,
            strict = FALSE,
            allowInvalid = TRUE
          )
      })
    }
    else{
      # If no user input, and button to load example table is pressed read the kidney disease example set and render to table
      output$hot = renderRHandsontable({
        DF = read.table(
          "/projects/grs-data/compute-app/www/kidney_disease_snps.tsv",
          sep = "\t",
          header = T,
          stringsAsFactors = FALSE
        )
        rhandsontable(DF) %>%
          hot_context_menu(customOpts = list(
            csv = list(
              name = "Download to CSV",
              callback = htmlwidgets::JS(
                "function (key, options) {
                var csv = csvString(this);
                
                var link = document.createElement('a');
                link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                encodeURIComponent(csv));
                link.setAttribute('download', 'data.csv');
                
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
      }"
              )
            )
          )) %>%
          hot_table(
            highlightCol = TRUE,
            highlightRow = TRUE,
            strict = FALSE,
            allowInvalid = TRUE
          )
      })
      reset(input$loadTable)
    }
  })
  
  grsPlot = reactive({
    outdir = paste0(newwd, "/")
    if (input$entryMethod == "paste" && input$subType == "interactive") {
      df <- data.frame(t(matrix(
        unlist(input$hot$params$data), nrow = 4
      )), stringsAsFactors = FALSE)
      colnames(df) = c("", "", "", "")
      
    }
    if (input$entryMethod == "upload" && input$subType == "interactive") {
      df = read.table(input$snpfile$datapath, sep = "\t", stringsAsFactors = FALSE)
      colnames(df) = c("", "", "", "")
    }
    
    isolate(write.table(
      df,
      file = paste0(newwd, "/SNPs.tsv"),
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    ))
    getGRS = paste0("/projects/grs-data/scripts/get_grs.pl -s ",
                    outdir,
                    "/SNPs.tsv -o ",
                    outdir, input$weight)
    getGRS
    system(getGRS)
    plotGRS(outdir)
  })

  observeEvent(input$submit, { 

    task <<- create_forked_task({
      isolate(grsPlot())
    })

    prog <- Progress$new(session)
    prog$set(message = "Computing Polygenic Trait Scores", value = 1)
    o <- observe({
      req(task$completed())
      o$destroy()
      prog$close()
      })
    shinyjs::toggle(id="mapbox")
    shinyjs::toggle(id="stripbox")
    shinyjs::toggle(id="plotbox")
    })

  output$choose_phenotype <- renderUI({
    req(task)$result()
    shinyjs::toggle(id="mapbox")
    shinyjs::toggle(id="stripbox")
    shinyjs::toggle(id="plotbox")
    input$submit
    outdir = paste0(newwd, "/")
    filelist = dir(outdir, pattern = "*.txt")
    filelist <- gsub(".txt", "", filelist)
    selectInput("phenotype",
                "",
                as.list(filelist))
  })

  output$mytable = DT::renderDataTable({
        summData = grs()
        summData = summarySE2(summData, measurevar="GRS", groupvars=c("FULL"))
      },
      options = list(paging = FALSE, 
        # autoWidth = TRUE,
        searching = FALSE
        ),
      rownames= FALSE,
      # binfo = 0
      )
  output$SNPtable = DT::renderDataTable(
    # req(task)$result()
      DT::datatable({
        counts = read.table(paste(newwd, "/", input$phenotype, ".snpcount", sep = ""), sep="\t", header=T, row.names=NULL)
      counts$Calls = counts$Calls/2504
      counts$SNP <- createLink(counts$SNP)
      counts
      },
      escape = FALSE, selection="single", rownames=F, options = list(lengthMenu = c(5,30,50,100), pageLength = 5)) %>%
            formatStyle(columns = c("SNP"), 
                        color = "black") %>%
            formatPercentage(columns=c('Calls'), 2) %>%
            formatStyle(
              'Calls',
              background = styleColorBar(counts$Calls*0.5, 'steelblue'),
              backgroundSize = '100% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
            )
        )
  output$aovSummary = reactivePrint(function() {
    if (is.null(input$phenotype))
        return()
      stargazer(grsAno,
        grsMod,
        type = "html",
        title=c("Summary of ANOVA","Summar of Linear Regression"))
    })
  output$plot <- renderPlot({
    req(task)$result()
    if (is.null(input$phenotype))
      return()
    outdir = paste0(newwd, "/")
    file = paste(outdir, "/", input$phenotype, ".txt", sep = "")
    grs = readGrs(file, sampleInfo)
    
    # ANOVA
    grsMod <<- lm(GRS ~ SUP, data = grs)
    grsAno <<- anova(grsMod)
    fstat = grsAno$`F value`[1]
    pval = grsAno$`Pr(>F)`[1]
    
    # Medians
    grsdt = as.data.table(grs)
    grsdt$GRS = as.numeric(grsdt$GRS)
    grsdt[, SUPMED := median(GRS), by = SUP]
    medians = as.data.frame(grsdt[which(!duplicated(grsdt$SUP)), 5:7, with = F])
    orderedSup = medians$FULL[order(medians$SUPMED)]
    grs$FULL = ordered(grs$FULL, levels = orderedSup)
    # Boxplot
    #title = ifelse(is.null(title), "", title)
    ggplot(grs, aes(x = FULL, y = GRS)) +
      geom_point(
        position = position_jitter(width = 0.6, height = 0.05),
        aes(
          col = FULL,
          size = 0.001,
          fill = SUP
        ),
        colour = "black",
        pch = 21,
        show.legend = F
      ) +
      scale_fill_manual(values = c5, guide = F) +
      geom_boxplot(outlier.shape = NA, alpha = 0.3) +
      theme_bw() +
      theme(panel.grid = element_blank()) + theme(text = element_text(size=15)) +
      labs(
        title = gsub(".txt", "", input$phenotype),
        x = paste(
          "F value=",
          format(fstat, digit = 5),
          ", P-value =",
          format(pval, digit = 3)
        )
      )
  })
  output$dlPlots <- renderUI({
    downloadButton('dlPlotstar', 'Download GADGET plots',
                   class = 'btn btn-large')
    
  })
  output$dlReport <- renderUI({
    downloadButton('report',
                   paste('Download report for', input$phenotype),
                   class = 'btn btn-large')
    
  })
  
  
  output$dlPlotstar = downloadHandler(
    filename = function() {
      paste(
        "GADGETplots-",
        input$popSelect,
        paste(collapse = '-'),
        '-',
        gsub(' ', '-', gsub(':', '-', Sys.time())),
        '.zip',
        sep = ''
      )
    },
    content = function(file) {
      outdir = paste0(newwd, "/")
      copyFile = paste(outdir, "plots.zip", sep = "")
      file.copy(copyFile, file)
    }
  )
  

  grs <- reactive({
    outdir = paste0(newwd, "/")
    grs = readGrs(paste(outdir, "/", input$phenotype, ".txt", sep = ""),
                  sampleInfo)
  })
  medians <- reactive({
    grs = grs()
    # Medians
    grsdt = as.data.table(grs)
    grsdt$GRS = as.numeric(grsdt$GRS)
    grsdt[, SUPMED := median(GRS), by = SUP]
    grsdt[, POPMED := median(GRS), by = POP]
    mediansSUP = as.data.frame(grsdt[which(!duplicated(grsdt$SUP)), 5:7, with = F])
    mediansPOP = as.data.frame(grsdt[which(!duplicated(grsdt$POP)), c(3,5,8), with =
                                       F])
    list(mediansSUP = mediansSUP, mediansPOP = mediansPOP)
  })
  output$mymap <- renderLeaflet({
    if (is.null(input$phenotype))
      return()
    points = read.csv("./data/geo.csv")
    mediansPOP = medians()[[2]]
    points = merge(points, mediansPOP, by = "POP")
    points$POPMEDScaled = points$POPMED / mean(points$POPMED)
    points$description = paste0(
      "<p style=\"color:black\"><b>Sample information: </b>",
      points$description,
      " (",
      points$POP,
      ")",
      ". <b>Population size:</b> ",
      points$nSample,
      ". <b>Median PTS for this population:</b> ",
      round(points$POPMED,4),
      "</p>"
    )
    # Colors
    supNames = c("AFR", "EUR", "AMR", "SAS", "EAS")
    c5 = c("#484496", "#F4A500", "#328A4F", "#944116", "#D92414")
    names(c5) = supNames
    fullNames = c("African", "European", "Admixed American", "Southeast Asian", "East Asian")
    cF = c("#484496", "#F4A500", "#328A4F", "#944116", "#D92414")
    names(cF) = fullNames
    pal <- colorFactor(c5, levels = names(c5))
    palF <- colorFactor(cF, levels = names(cF))
    if (input$legend) {
        leaflet() %>%
        addProviderTiles("CartoDB", options = providerTileOptions(noWrap = TRUE)) %>%
        fitBounds(
          lng1 = max(points$lng),
          lat1 = 50, 
          lng2 = min(points$lng),
          lat2 = min(points$lat)
        ) %>%
        addCircleMarkers(
          lng = ~ lng,
          lat = ~ lat,
          weight = 1,
          radius = ~ (20*2^POPMEDScaled/max(POPMEDScaled)),
          popup = ~ description,
          color = ~ pal(SUP.x),
          stroke = F,
          fillOpacity = 0.5,
          data = points
        ) %>%
        addLegend(
          "bottomright",
          pal = palF,
          values = fullNames,
          title = "Populations"
        )
    }
    else{
      leaflet() %>%
        addProviderTiles("CartoDB", options = providerTileOptions(noWrap = TRUE)) %>%
        fitBounds(
          lng1 = max(points$lng),
          lat1 = 50,
          lng2 = min(points$lng),
          lat2 = min(points$lat)
        ) %>%
        addCircleMarkers(
          lng = ~ lng,
          lat = ~ lat,
          weight = 1,
          radius = ~ (20*2^POPMEDScaled/max(POPMEDScaled)),
          popup = ~ description,
          color = ~ pal(SUP.x),
          stroke = F,
          fillOpacity = 0.5,
          data = points
        )
    }
  })
  
  observeEvent(input$legend, {
    isolate({
      proxy <- leafletProxy("mymap")
      proxy %>% clearControls()
      if (input$legend) {
        points = read.csv("./data/geo.csv")
        fullNames = c("African",
                      "European",
                      "Admixed American",
                      "Southeast Asian",
                      "East Asian")
        cF = c("#484496", "#F4A500", "#328A4F", "#944116", "#D92414")
        names(cF) = fullNames
        palF <- colorFactor(cF, levels = names(cF))
        proxy %>% addLegend(
          "bottomright",
          pal = palF,
          values = points$FULL,
          title = "Populations"
        )
      }
    })
  })
  output$stripplot <- renderPlot({
    req(task)$result()
    grsdf = grs()
    title = input$phenotype
    medians = medians()[[2]]
    orderedPop = medians$POP[order(medians$POPMED)]
    medians = medians[order(medians$POPMED),]
    
    grsdf$POP = ordered(grsdf$POP, levels = orderedPop)
    grsdf = grsdf[order(grsdf$POP, grsdf$GRS),]
    grslist = split(grsdf, f = grsdf$POP)
    names(grslist) = orderedPop
    
    grslist = lapply(grslist, function(x) {
      x$x = rep(1, nrow(x))
      x$x = 100 * x$x / sum(x$x)
      x
    })
    
    
    grsdf = do.call(rbind, grslist)
    spacer_size = 10
    grsdf$x = cumsum(grsdf$x)
    ymax = max(grsdf$GRS) * 1.2
    ymin = min(grsdf$GRS) * 0.8
    medians$x = seq(1, nrow(medians) * 100, by = 100)
    medians$xend = seq(100, nrow(medians) * 100, by = 100)
    medians$midx = medians$x + (medians$xend - medians$x + 1) / 2
    if (nrow(medians) %% 2 == 0) {
      medians$col = factor(rep(1:2, nrow(medians) / 2))
    } else{
      medians$col = factor(c(rep(1:2, (
        nrow(medians) - 1
      ) / 2), 1))
    }
    medians$midy = (ymax - ymin) / 2 + ymin
    medians$h = ymax - ymin
    medians$ymin = ymin
    
    ggplot() +
      geom_tile(data = medians, aes(
        x = midx,
        y = midy,
        fill = col,
        height = h
      )) +
      scale_fill_manual(values = c("lightgrey", "white"),
                        guide = F) +
      geom_point(
        data = grsdf,
        aes(x = x, y = GRS, col = SUP),
        alpha = 0.5,
        size = 1.5
      ) +
      scale_color_manual(values = c5, guide = F) +
      geom_segment(
        data = medians,
        aes(
          x = x,
          y = POPMED,
          xend = xend,
          yend = POPMED
        ),
        size = 1,
        col = "red"
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
      ) + theme(text = element_text(size=15)) +
      labs(y = "Polygenic Trait Score", x = "Population", title = title) +
      geom_text(
        data = medians,
        aes(x = midx, y = ymin, label = POP, col=SUP),
        fontface="bold",
        angle = -45,
        vjust = 1,
        hjust = -0.2,
        size = 4
      ) +
      coord_cartesian(ylim = c(ymin - (ymax - ymin) * 0.1, ymax))
    
  })
  output$report <- downloadHandler(
    # Download handler for report HTML File
    filename = function() {
      paste0(input$phenotype, "-report.html")
    },
    # File name for report is Phenotype-report.html
    content = function(file) {
      # Copy the report.Rmd template
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Pass list of params to Rscript for filling stuff
      params <-
        list(pheno = input$phenotype, dir = paste0(newwd, "/"))
      
      # Render document to html
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  state <- reactiveValues()
  onBookmark(function(state) {
  })
  onRestore(function(state) {
    # Read session data for input and restore table values
    tmp = state$input$hot
    tmp$data = JSonlite::fromJSON(jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
})
