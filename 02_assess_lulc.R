## assess mapbiomas collection 6.0 data
## any issue/bug please write to dhemerson.costa@ipam.org.br

## import packages
library (ggplot2)
library (ggrepel)
library (raster)

## avoid scientific notation
options(scipen=999)

###### function definition section - please dont change below this line

## function to read each raster in the folder './raster'/ as a unique file and stack them as a band 
read_raster <- function(file_path) {
  ## list files in the path
  files <- list.files(file_path, full.names=TRUE)
  ## for each file
  for (i in 1:length(files)) {
    ## if length equal to 1, create object
    if (i == 1) {
      obj <- raster(files[i])
    }
    ## else, stack object
    else {
      obj2 <- raster(files[i])
      obj <- stack(obj, obj2)
    }
  }
  
  return (obj)
}

## function to import statistics table
read_table <- function(file_path) {
  ## list files
  files <- list.files(file_path, full.names=TRUE)
  ## import table
  table <- read.csv(files[1], dec='.', sep=',')[-1][-5]
  ## parse year variable from bandname
  table$year <- sapply(strsplit(table$band, split='_', fixed=TRUE), function(x) (x[2]))
  ## rectfy area to hectares
  table$area <- table$area * 100
  
  return(table)
}

## function to reclassify names of classes
reclass_names <- function(level, input) {
  ## reclassify to level 0
  if (level == 0) {
    ## susbet only natural
    natural <- subset(input, class == '1' | class == '3' | class == '4' | class == '5' |
                        class == '6' | class == '49' | class == '10' | class == '11' |
                        class == '12' | class == '32' | class == '29' | class == '50' |
                        class == '13' | class == '23'  | class == '33') 
    
    ## subset only antrhopic
    anthropic <- subset(input, class != '1' & class != '3' & class != '4' & class != '5' &
                          class != '6' & class != '49' & class != '10' & class != '11' &
                          class != '12' & class != '32' & class != '29' & class != '50' &
                          class != '13' & class != '23'  & class != '33') 
    
    ## pass metadata
    natural$class_rec <- 'Natural'
    anthropic$class_rec <- 'Anthropic'
    data <- rbind (natural, anthropic)
    
    ## aggregate
    data <- aggregate(x=list(area=data$area), by=list(year=data$year, class= data$class_rec), FUN='sum')
    
    return (data)
  }
  ## reclassify to level 2
  if (level == 1) {
    ## subset native
    forest <- subset(input, class == '3' | class == '5' | class == '6')
    savanna <- subset(input, class == '4' | class == '49')
    grassland <- subset(input, class == '10' | class == '11' | class == '12' | class == '32' |
                               class == '50' | class == '13')
    ## farming
    agriculture <- subset (input, class == '14' | class == '18' | class == '19' | class== '39' |
                                  class == '20' | class == '40' | class == '41' | class== '36' |
                                  class == '46' | class == '47' | class == '48')
    ## pasture
    pasture <- subset (input, class == '15')
    
    ## forestry 
    forestry <- subset(input, class == '9')
    
    ## mosaic of
    mosaic <- subset (input, class == '21')
    
    ## others
    others <- subset (input, class == '29' | class == '22' | class == '23' | class == '24' | 
                             class == '51' | class == '52' | class == '53' | class == '30' |
                             class == '25' | class == '26' | class == '33' | class == '31' |
                             class == '54' | class == '55' | class == '56' | class == '27')
    
    ## add labels
    forest$class_rec <- 'Forest Formation'
    savanna$class_rec <- 'Savanna Formation'
    grassland$class_rec <- 'Grassland'
    agriculture$class_rec <- 'Agriculture'
    pasture$class_rec <- 'Pasture'
    forestry$class_rec <- 'Forestry'
    mosaic$class_rec <- 'Mosaic of Agr. and Pasture'
    others$class_rec <- 'Other'
    data <- rbind (forest, savanna, grassland, agriculture, pasture, forestry, mosaic, others)
    
    ## aggregate
    data <- aggregate(x=list(area=data$area), by=list(year=data$year, class= data$class_rec), FUN='sum')
    
    ## reorder
    data$class <- factor(data$class, levels = c("Forest Formation", "Savanna Formation", "Grassland",
                                                "Forestry", "Pasture", "Agriculture", "Mosaic of Agr. and Pasture",
                                                "Other"))
    
    return (data)
  }

  if (level == 4) {
    data <- input
    data$class <- data$class_name
    return (data)
  }
}

## plot specific year
plot_year <- function (data, lvl, yr) {
  ## subset
  table2 <- reclass_names(input= data, level = lvl)
  ## compute position and labels
  df <- subset(table2, year == yr)
  df$relac <- (df$area / sum(df$area) * 100)
  df$fraction = df$relac / sum(df$relac)
  df$ymax = cumsum(df$fraction)
  df$ymin = c(0, head(df$ymax, n=-1))
  df$labelPosition <- (df$ymax + df$ymin) / 2
  df$label <- paste0(round(df$relac, digits=1), "%")
  
  ## define pallete
  if (lvl == 0) {
    pal <- c('orange', 'forestgreen')
  }
  if (lvl == 1) {
    pal <- c('#006400', '#00ff00', '#b8af4f', '#ad4413', '#ffd966', '#e974ed', '#fff3bf', '#D5D5E5')
  }
  
  # Make the plot
  if (lvl == 0 | lvl == 1) {
  p <- ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=class)) +
    geom_rect(alpha=0.9) +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
    theme_void() +
    scale_fill_manual(values=pal) +
    geom_label_repel(x=3.5, aes(y=labelPosition, label=label), label.size=NA, size=7, color='black', fill=NA)
  } else {
    p <- ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=class)) +
      geom_rect(alpha=0.9) +
      coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
      xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
      theme_void() +
      geom_label_repel(x=3.5, aes(y=labelPosition, label=label), label.size=NA, size=7, color='black', fill=NA)
  }
  
  return (p)
}

## plot historic of land cover
plot_historic <- function (data, lvl) {
  ## process table
  table2 <- reclass_names(input= data, level = lvl)
  
  ## define pallete
  if (lvl == 0) {
    pal <- c('orange', 'forestgreen')
  }
  if (lvl == 1) {
    pal <- c('#006400', '#00ff00', '#b8af4f', '#ad4413', '#ffd966', '#e974ed', '#fff3bf', '#D5D5E5')
  }

  
  ## plot the historic of LULC
  if (lvl == 0 | lvl == 1) {
    p <- ggplot (data= table2, aes(x=as.numeric(year), y= as.numeric(area))) +
      geom_area(aes(group=class, fill=class), alpha=0.9) +
      scale_fill_manual('Class', values=pal) +
      theme_minimal() +
      xlab ('Ano') + ylab('Área (ha)')
  } else {
    p <- ggplot (data= table2, aes(x=as.numeric(year), y= as.numeric(area))) +
      geom_area(aes(group=class, fill=class), alpha=0.9) +
      theme_minimal() +
      xlab ('Ano') + ylab('Área (ha)')
  }

  
  return (p)
  
}


############# end of function definition section - user can edit below this line 

## read raster files as stack image (one year per band)
#image <- read_raster(file_path = './raster')

## read statistics 
table <- read_table(file_path = './table')

## plot LULC for a specific year
plot_year (data= table, lvl= 0, yr = 2020)

## plot historic of LULC (avaliable lvl = 0, 1 and 4)
plot_historic(data= table, lvl= 0)

## create auxiliary table
table_aux <- reclass_names(input= table, level = 0)


