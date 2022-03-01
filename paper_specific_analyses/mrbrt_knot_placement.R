
# MR-BRT Ensemble Knot Placement Tables



library(ggpubr)
library(grid)
library(gridExtra)
library(gtable)
library(data.table)


boldsplit <- function( string , split ){
  require( stringr )
  blurb <- paste( string )
  blurbs <- strsplit( blurb , paste(split) )
  annot <- bquote( paste( bold( .( blurbs[[1]][1] ) ) , .(split) , .(blurbs[[1]][2]) , sep = "" ) )
  return( annot )
}





me_names <- c("stunting_overall", "stunting_severe", "stunting_extreme",
              "wasting_overall", "wasting_severe", "wasting_extreme",
              "underweight_overall", "underweight_severe", "underweight_extreme")

me_titles <- c("Overall Stunting", "Severe Stunting", "Extreme Stunting", 
               "Overall Wasting", "Severe Wasting", "Extreme Wasting", 
               "Overall Underweight", "Severe Underweight", "Extreme Underweight")



knot.files <- c("knot_placement_1_2.csv", "knot_placement_2_2.csv",
                "knot_placement_1_3.csv", "knot_placement_2_3.csv",
                "knot_placement_1_388.csv", "knot_placement_2_388.csv",
                "knot_placement_1_389.csv", "knot_placement_2_389.csv",
                "knot_placement_1_238.csv", "knot_placement_2_238.csv",
                "knot_placement_1_34.csv", "knot_placement_2_34.csv")




param <- data.table(me_names = rep(me_names, each = 12),
                    me_titles = rep(me_titles, each = 12), 
                    knot.files = rep(knot.files, times = 9))

param[, index:= 1:.N]
param[, sex := rep(c("Male", "Female"),times = 54)]
param[, page := paste0(me_titles, " in ", sex, "s")]
param[, agegroup := rep(c(2, 2, 3, 3, 388, 388, 389, 389, 238, 238, 34, 34), times = 9)]


param[agegroup == 2, age_group_name := "Early Neonatal"]
param[agegroup == 3, age_group_name := "Late Neonatal"]
param[agegroup == 388, age_group_name := "1-5 Months"]
param[agegroup == 389, age_group_name := "6-11 Months"]
param[agegroup == 238, age_group_name := "12-23 Months"]
param[agegroup == 34, age_group_name := "2-4 Years"]

y <- rep(c(1:2),times = 6)
y.final <- c(y, y+2, y+4, y+6, y+8, y + 10, y + 12, y + 14, y +16)

param[, pdf.label := y.final]



for (p in unique(param$page)) {
  
  
  
  all.files.for.page <- param[page == p]$knot.files
  
  current.me_name <- unique(param[page == p]$me_names)
  
  current.sex <- unique(param[page == p]$sex)
  
  current.pdf.label <- unique(param[page == p]$pdf.label)
  
  
  # read in all the files in each folder for one page, make 6 tables and combine them
  
  
  
  
  t1 <- fread(paste0("filepath", current.me_name, "/", all.files.for.page[1]))
  
  t1a <- data.table(format(round(t1[, 1:6], digits = 3), nsmall = 3))
  weight.column <- paste0(format(round(t1$weights * 100, digits = 2), nsmall = 2), "%")
  t1a$weights <- weight.column
  
  #this actually creates the table
  t1a.table <- tableGrob(t1a,cols = c("Knot 1", "Knot 2","Knot 3", "Knot 4", "Knot 5", "Knot 6", "Weight"))
  
  t1a.table <- gtable_add_grob(t1a.table,
                               grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                               t = 2, b = 21, l = 8)
  
  
  table.title <- textGrob("Early Neonatal",gp=gpar(fontsize=16))
  padding <- unit(5,"mm")
  
  t1a.table <- gtable_add_rows(
    t1a.table, 
    heights = grobHeight(table.title) + padding,
    pos = 0)
  t1a.table <- gtable_add_grob(
    t1a.table, 
    table.title, 
    1, 1, 1, ncol(t1a.table))
  
  
  
  t2 <- fread(paste0("filepath", current.me_name, "/", all.files.for.page[2]))
  
  t2a <- data.table(format(round(t2[, 1:6], digits = 3), nsmall = 3))
  weight.column <- paste0(format(round(t2$weights * 100, digits = 2), nsmall = 2), "%")
  t2a$weights <- weight.column
  
  #this actually creates the table
  t2a.table <- tableGrob(t2a,cols = c("Knot 1", "Knot 2","Knot 3", "Knot 4", "Knot 5", "Knot 6", "Weight"))
  
  t2a.table <- gtable_add_grob(t2a.table,
                               grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                               t = 2, b = 21, l = 8)
  
  
  table.title <- textGrob("Late Neonatal",gp=gpar(fontsize=16))
  padding <- unit(5,"mm")
  
  t2a.table <- gtable_add_rows(
    t2a.table, 
    heights = grobHeight(table.title) + padding,
    pos = 0)
  t2a.table <- gtable_add_grob(
    t2a.table, 
    table.title, 
    1, 1, 1, ncol(t2a.table))
  
  
  
  t3 <- fread(paste0("filepath", current.me_name, "/", all.files.for.page[3]))
  
  t3a <- data.table(format(round(t3[, 1:6], digits = 3), nsmall = 3))
  weight.column <- paste0(format(round(t3$weights * 100, digits = 2), nsmall = 2), "%")
  t3a$weights <- weight.column
  
  #this actually creates the table
  t3a.table <- tableGrob(t3a,cols = c("Knot 1", "Knot 2","Knot 3", "Knot 4", "Knot 5", "Knot 6", "Weight"))
  
  t3a.table <- gtable_add_grob(t3a.table,
                               grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                               t = 2, b = 21, l = 8)
  
  
  table.title <- textGrob("1-5 Months",gp=gpar(fontsize=16))
  padding <- unit(5,"mm")
  
  t3a.table <- gtable_add_rows(
    t3a.table, 
    heights = grobHeight(table.title) + padding,
    pos = 0)
  t3a.table <- gtable_add_grob(
    t3a.table, 
    table.title, 
    1, 1, 1, ncol(t3a.table))
  
  
  
  
  t4 <- fread(paste0("filepath", current.me_name, "/", all.files.for.page[4]))
  
  t4a <- data.table(format(round(t4[, 1:6], digits = 3), nsmall = 3))
  weight.column <- paste0(format(round(t4$weights * 100, digits = 2), nsmall = 2), "%")
  t4a$weights <- weight.column
  
  #this actually creates the table
  t4a.table <- tableGrob(t4a,cols = c("Knot 1", "Knot 2","Knot 3", "Knot 4", "Knot 5", "Knot 6", "Weight"))
  
  t4a.table <- gtable_add_grob(t4a.table,
                               grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                               t = 2, b = 21, l = 8)
  
  
  table.title <- textGrob("6-11 Months",gp=gpar(fontsize=16))
  padding <- unit(5,"mm")
  
  t4a.table <- gtable_add_rows(
    t4a.table, 
    heights = grobHeight(table.title) + padding,
    pos = 0)
  t4a.table <- gtable_add_grob(
    t4a.table, 
    table.title, 
    1, 1, 1, ncol(t4a.table))
  
  
  
  
  
  t5 <- fread(paste0("filepath", current.me_name, "/", all.files.for.page[5]))
  
  t5a <- data.table(format(round(t5[, 1:6], digits = 3), nsmall = 3))
  weight.column <- paste0(format(round(t5$weights * 100, digits = 2), nsmall = 2), "%")
  t5a$weights <- weight.column
  
  #this actually creates the table
  t5a.table <- tableGrob(t5a,cols = c("Knot 1", "Knot 2","Knot 3", "Knot 4", "Knot 5", "Knot 6", "Weight"))
  
  t5a.table <- gtable_add_grob(t5a.table,
                               grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                               t = 2, b = 21, l = 8)
  
  
  table.title <- textGrob("12-23 Months",gp=gpar(fontsize=16))
  padding <- unit(5,"mm")
  
  t5a.table <- gtable_add_rows(
    t5a.table, 
    heights = grobHeight(table.title) + padding,
    pos = 0)
  t5a.table <- gtable_add_grob(
    t5a.table, 
    table.title, 
    1, 1, 1, ncol(t5a.table))
  
  
  
  
  
  t6 <- fread(paste0("filepath", current.me_name, "/", all.files.for.page[6]))
  
  t6a <- data.table(format(round(t6[, 1:6], digits = 3), nsmall = 3))
  weight.column <- paste0(format(round(t6$weights * 100, digits = 2), nsmall = 2), "%")
  t6a$weights <- weight.column
  
  #this actually creates the table
  t6a.table <- tableGrob(t6a,cols = c("Knot 1", "Knot 2","Knot 3", "Knot 4", "Knot 5", "Knot 6", "Weight"))
  
  t6a.table <- gtable_add_grob(t6a.table,
                               grobs = segmentsGrob(x0 = unit(0,"npc"),y0 = unit(0,"npc"),x1 = unit(0,"npc"),y1 = unit(1,"npc"),gp = gpar(lwd = 1)),
                               t = 2, b = 21, l = 8)
  
  
  table.title <- textGrob("2-4 Years",gp=gpar(fontsize=16))
  padding <- unit(5,"mm")
  
  t6a.table <- gtable_add_rows(
    t6a.table, 
    heights = grobHeight(table.title) + padding,
    pos = 0)
  t6a.table <- gtable_add_grob(
    t6a.table, 
    table.title, 
    1, 1, 1, ncol(t6a.table))
  
  
  empty <- ggplot() + theme_void()
  
  caption.plot <- ggplot() + theme_void() +
    labs(caption = expression(paste(bold("       Appendix F: \n"), "Twenty MR-BRT models with different knot placements were fit to each age-sex, weighted based by their predictive performance.\nKnots 1 and 6 were always respectively placed at the lowest and highest estimated UHC Index values input to the model."))) +
    theme(plot.caption = element_text(size = 16, hjust = 0))
  
  
  
  title.caption <- boldsplit(string = paste0(p, ": ", "Ensemble MR-BRT Knot Placement"), split = ":")
  title.plot <- ggplot() + theme_void() +
    labs(caption = title.caption) +
    theme(plot.caption = element_text(size = 20, hjust = .5))
  
  
  
  final.arranged <- ggarrange(empty, empty, empty,
                              empty, title.plot, empty,
                              empty, empty, empty,
                              t1a.table, t2a.table, t3a.table,
                              empty, empty, empty,
                              t4a.table, t5a.table, t6a.table,
                              caption.plot, empty, empty,
                              nrow=7, ncol = 3, heights = c(.06,.01, .06, 1,.0001, 1, .08))
  
  pdf(paste0("filepath", current.pdf.label ,".pdf"), height = 14.5, width = 18)
  print(final.arranged)
  dev.off()
  
  
  
}











