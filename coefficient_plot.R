# coefficient plots
library(grid)

######## Commodities #############
vp <- viewport(x=0.5,y=0.5,width=0.9, height=0.8)
pushViewport(vp)

# grid xaxis bottom
grid.xaxis(at=c(0.2,c(0.333,0.466,0.599,0.732,0.865,1)), label=seq(-1.50,1.50,0.5), gp = gpar(fontsize = 10), main = TRUE)

# grid xaxis top
grid.xaxis(at=c(0.2,c(0.333,0.466,0.599,0.732,0.865,1)), label=c(0.22,0.37,0.61,1.00,1.65,2.72,4.48), gp = gpar(fontsize = 10), main = FALSE)

# grid lines 
lines <- c(0.1667,0.333,0.5,0.6667,0.8333) # 1/6

for (i in 1:5) {
  grid.lines(x = unit(c(0.2,1),"npc"),
             y = unit(c(lines[i],lines[i]),"npc"),
             default.units = "npc",
             arrow = NULL, name = NULL,
             gp=gpar(), draw = TRUE, vp = NULL)
}

# grid text labels left side
labels <- c("base and ferroalloy metals", "biological resources","nonferrous metals","energy sources", "nonmetallic minerals","precious metals")

labelpositions <- c(0.92,0.75,0.58,0.42,0.25,0.1)

for (i in 1:6){
  grid.text(labels[i],
            x = unit(0,"npc"),
            y = unit(labelpositions[i],"npc"),
            just = "left", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}

# values for escalation stages 6,7,8 inside vp

coef_comm <- summary(multi_z2)$coefficients[1:3,12:17] # coefficients of commodities
exp_comm <- exp(summary(multi_z2)$coefficients[1:3,12:17]) # exp(cofficients of commodities)
 

for (i in 1:18) {
  coef_comm_grid <- ((coef_comm[i]+1.5)/3*0.8+0.2)
  
}
for (i in 1:18) {
  coef_comm_grid[i] <- ((coef_comm[i]+1.5)/3*0.8+0.2)
  
}

print(coef_comm_grid)

stages <- c("6","7","8","6","7","8","6","7","8","6","7","8","6","7","8","6","7","8")
ypositions <- c("0.92","0.92","0.92","0.75","0.75","0.75","0.58", "0.58","0.58","0.42","0.42", "0.42","0.25", "0.25","0.25","0.1","0.1","0.1")

for (i in 1:18) {
  grid.text(stages[i],
            x = unit(coef_comm_grid[i],"npc"),
            y = unit(ypositions[i],"npc"),
            just = "centre", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}

# position of escalation stage 5 (baseline)
for(i in 1:6) {
  grid.text("5",
            x = unit(0.6, "npc"),
            y = unit(labelpositions[i], "npc"),
            just = "centre", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}

# scale labels
# grid.text("Logit Coefficient Scale",
#           x = unit(0.2, "npc"),
#           y = unit(0, "npc"),
#           just = "right", hjust = NULL, vjust = NULL, rot = 0,
#           check.overlap = FALSE, default.units = "npc",
#           name = NULL, gp = gpar(fontsize = 10), draw = TRUE, vp = NULL)
# 
# grid.text("Factor Change Scale",
#           x = unit(0.18, "npc"),
#           y = unit(1, "npc"),
#           just = "right", hjust = NULL, vjust = NULL, rot = 0,
#           check.overlap = FALSE, default.units = "npc",
#           name = NULL, gp = gpar(fontsize = 10), draw = TRUE, vp = NULL)

##### mobilizing groups ####
vp <- viewport(x=0.5,y=0.5,width=0.9, height=0.8)
pushViewport(vp)

# grid xaxis bottom
grid.xaxis(at=c(0.2,c(0.333,0.466,0.599,0.732,0.865,1)), label=c(-2.50,-1.667,-0.833,0,0.833,1.667,2.50), gp = gpar(fontsize = 10), main = TRUE)

# grid xaxis top
grid.xaxis(at=c(0.2,c(0.333,0.466,0.599,0.732,0.865,1)), label=c(round(exp(-2.50), digits= 2),round(exp(-1.667), digits = 2),round(exp(-0.833), digits = 2),round(exp(0), digits = 2),round(exp(0.833), digits = 2),round(exp(1.667), digits = 2),round(exp(2.50), digits = 2)), gp = gpar(fontsize = 10), main = FALSE) 

# grid lines 
lines_groups <- c(0.25,0.5,0.75) # 1/4

for (i in 1:3) {
  grid.lines(x = unit(c(0.2,1),"npc"),
             y = unit(c(lines_groups[i],lines_groups[i]),"npc"),
             default.units = "npc",
             arrow = NULL, name = NULL,
             gp=gpar(), draw = TRUE, vp = NULL)
}

# grid text labels left side
labels_groups <- c("economic actors", "excluded/marginalized", "local people", "organization")

labelpositions_groups <- c((0.75+(0.25/2)),(0.5+(0.25/2)),(0.25+(0.25/2)),(0+(0.25/2)))

for (i in 1:4){
  grid.text(labels_groups[i],
            x = unit(0,"npc"),
            y = unit(labelpositions_groups[i],"npc"),
            just = "left", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}


# values for escalation stages 6,7,8 inside vp
coef_groups <- summary(multi_z2)$coefficients[1:3,21:24]
exp_groups <- exp(summary(multi_z2)$coefficients[1:3, 21:24])

for (i in 1:12) {
  coef_groups_grid <- (coef_groups[i]+2.5)/5*0.8+0.2 
}
for (i in 1:12) {
  coef_groups_grid[i] <- (coef_groups[i]+2.5)/5*0.8+0.2 
}

print(coef_groups_grid)

stages_groups <- c("6","7","8","6","7","8","6","7","8","6","7","8")
ypositions_groups <- c((0.75+(0.25/2)),(0.75+(0.25/2)),(0.75+(0.25/2)),(0.5+(0.25/2)),(0.5+(0.25/2)),(0.5+(0.25/2)),(0.25+(0.25/2)),(0.25+(0.25/2)),(0.25+(0.25/2)),(0+(0.25/2)),(0+(0.25/2)),(0+(0.25/2)))

for (i in 1:12) {
  if (i != 4 & i != 7 & i != 9 & i != 11){
  grid.text(stages_groups[i],
            x = unit(coef_groups_grid[i],"npc"),
            y = unit(ypositions_groups[i],"npc"),
            just = "centre", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
  }
    else {grid.text(stages_groups[i],
                    x = unit(coef_groups_grid[i],"npc"),
                    y = unit(ypositions_groups[i],"npc"),
                    just = "centre", hjust = NULL, vjust = NULL, rot = 0,
                    check.overlap = FALSE, default.units = "npc",
                    name = NULL, gp = gpar(fontsize = 15), draw = FALSE, vp = NULL)
      
    }

}

# draw missing values above others
for (i in 1:12) {
  if (i %in% c(4,7,9,11)){
    grid.text(stages_groups[i],
              x = unit(coef_groups_grid[i],"npc"),
              y = unit((ypositions_groups[i]+0.06),"npc"),
              just = "centre", hjust = NULL, vjust = NULL, rot = 0,
              check.overlap = FALSE, default.units = "npc",
              name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
  }
  else {grid.text(stages_groups[i],
                  x = unit(coef_groups_grid[i],"npc"),
                  y = unit(ypositions_groups[i],"npc"),
                  just = "centre", hjust = NULL, vjust = NULL, rot = 0,
                  check.overlap = FALSE, default.units = "npc",
                  name = NULL, gp = gpar(fontsize = 15), draw = FALSE, vp = NULL)
    
  }
  
}

# position of escalation stage 5 (baseline)
for(i in 1:6) {
  grid.text("5",
            x = unit(0.6, "npc"),
            y = unit(labelpositions_groups[i], "npc"),
            just = "centre", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}

# draw significance

# significance line for excluded/marginalized 5->7 5->8
grid.lines(x=unit(c(0.6,coef_groups_grid[5]),"npc"),
           y = unit(c(labelpositions_groups[2],labelpositions_groups[2]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

grid.lines(x=unit(c(0.6,coef_groups_grid[6]),"npc"),
           y = unit(c(labelpositions_groups[2],labelpositions_groups[2]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

# grid.lines(x = unit(c(0.6,0.6),"npc"),
#            y = unit(c((labelpositions_groups[2]-0.06),labelpositions_groups[2]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)
# 
# grid.lines(x = unit(c(coef_groups_grid[6],coef_groups_grid[6]),"npc"),
#            y = unit(c((labelpositions_groups[2]-0.06),labelpositions_groups[2]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)
# 
# grid.lines(x = unit(c(coef_groups_grid[5],coef_groups_grid[5]),"npc"),
#            y = unit(c((labelpositions_groups[2]-0.06),labelpositions_groups[2]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)


# significance line for local people 5->7

grid.lines(x=unit(c(0.6,coef_groups_grid[8]),"npc"),
           y = unit(c(labelpositions_groups[3],labelpositions_groups[3]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

# grid.lines(x = unit(c(0.6,0.6),"npc"),
#            y = unit(c((labelpositions_groups[3]-0.06),labelpositions_groups[3]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)
# 
# grid.lines(x = unit(c(coef_groups_grid[8],coef_groups_grid[8]),"npc"),
#            y = unit(c((labelpositions_groups[3]-0.06),labelpositions_groups[3]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)

# significance line for organzation 5->8

grid.lines(x=unit(c(0.6,coef_groups_grid[12]),"npc"),
           y = unit(c(labelpositions_groups[4],labelpositions_groups[4]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

# grid.lines(x = unit(c(0.6,0.6),"npc"),
#            y = unit(c((labelpositions_groups[4]-0.06),labelpositions_groups[4]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)
# 
# grid.lines(x = unit(c(coef_groups_grid[12],coef_groups_grid[12]),"npc"),
#            y = unit(c((labelpositions_groups[4]-0.06),labelpositions_groups[4]),"npc"),
#            default.units="npc",
#            arrow = NULL, name = NULL, 
#            gp = gpar(lty="dashed"),draw = TRUE, vp = NULL)


#### operator & reaction stage ####
vp <- viewport(x=0.5,y=0.5,width=0.9, height=0.8)
pushViewport(vp)

# grid xaxis bottom
grid.xaxis(at=c(0.2,c(0.333,0.466,0.599,0.732,0.865,1)), label=c(-2.50,-1.667,-0.833,0,0.833,1.667,2.50), gp = gpar(fontsize = 10), main = TRUE)

# grid xaxis top
grid.xaxis(at=c(0.2,c(0.333,0.466,0.599,0.732,0.865,1)), label=c(round(exp(-2.50), digits= 2),round(exp(-1.667), digits = 2),round(exp(-0.833), digits = 2),round(exp(0), digits = 2),round(exp(0.833), digits = 2),round(exp(1.667), digits = 2),round(exp(2.50), digits = 2)), gp = gpar(fontsize = 10), main = FALSE) 

# grid lines 
lines_operreact <- c(0.2,0.4,0.6,0.8) # 1/5

for (i in 1:4) {
  grid.lines(x = unit(c(0.2,1),"npc"),
             y = unit(c(lines_operreact[i],lines_operreact[i]),"npc"),
             default.units = "npc",
             arrow = NULL, name = NULL,
             gp=gpar(), draw = TRUE, vp = NULL)
}

# grid text labels left side
labels_operreact <- c("foreign company", "illegal mining", "mobilization:latent", "mobilization:reaction", "mobilization:reparations")

labelpositions_operreact <- c((0.8+(0.2/2)),(0.6+(0.2/2)),(0.4+(0.2/2)),(0.2+(0.2/2)), (0+(0.2/2)))

for (i in 1:5){
  grid.text(labels_operreact[i],
            x = unit(0,"npc"),
            y = unit(labelpositions_operreact[i],"npc"),
            just = "left", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}


# values for escalation stages 6,7,8 inside vp

# operator 25:26
coef_oper <- summary(multi_z2)$coefficients[1:3,25:26]
exp_oper <- exp(summary(multi_z2)$coefficients[1:3,25:26])

# reaction stage 18:20
coef_react <- summary(multi_z2)$coefficients[1:3,18:20]
exp_react <- exp(summary(multi_z2)$coefficients[1:3,18:20])

coef_operreact <- summary(multi_z2)$coefficients[1:3,c(25,26,18,19,20)]
exp_operreact <- exp(summary(multi_z2)$coefficients[1:3,c(25,26,18,19,20)])

for (i in 1:15) {
  coef_operreact_grid <- (coef_operreact[i]+2.5)/5*0.8+0.2 
}
for (i in 1:15) {
  coef_operreact_grid[i] <- (coef_operreact[i]+2.5)/5*0.8+0.2 
}

print(coef_operreact_grid)

stages_operreact <- c("6","7","8","6","7","8","6","7","8","6","7","8","6","7","8")
ypositions_operreact <- c((0.8+(0.2/2)),(0.8+(0.2/2)),(0.8+(0.2/2)),(0.6+(0.2/2)),(0.6+(0.2/2)),(0.6+(0.2/2)),(0.4+(0.2/2)),(0.4+(0.2/2)),(0.4+(0.2/2)),(0.2+(0.2/2)),(0.2+(0.2/2)),(0.2+(0.2/2)), (0+(0.2/2)),(0+(0.2/2)),(0+(0.2/2)))

for (i in 1:15) {
  if (i != 4 & i != 6 & i != 7 & i != 9 & i != 10 & i != 13 & i != 15){
    grid.text(stages_operreact[i],
              x = unit(coef_operreact_grid[i],"npc"),
              y = unit(ypositions_operreact[i],"npc"),
              just = "centre", hjust = NULL, vjust = NULL, rot = 0,
              check.overlap = FALSE, default.units = "npc",
              name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
  } else {
    grid.text(stages_operreact[i],
              x = unit(coef_operreact_grid[i],"npc"),
              y = unit(ypositions_operreact[i],"npc"),
              just = "centre", hjust = NULL, vjust = NULL, rot = 0,
              check.overlap = FALSE, default.units = "npc",
              name = NULL, gp = gpar(fontsize = 15), draw = FALSE, vp = NULL)
  }
}

# draw missing values above others
for (i in 1:15) {
  if (i %in% c(6,9,10,13,15)){
    grid.text(stages_operreact[i],
              x = unit(coef_operreact_grid[i],"npc"),
              y = unit((ypositions_operreact[i]+0.06),"npc"),
              just = "centre", hjust = NULL, vjust = NULL, rot = 0,
              check.overlap = FALSE, default.units = "npc",
              name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
  }
  else {grid.text(stages_operreact[i],
                  x = unit(coef_operreact_grid[i],"npc"),
                  y = unit(ypositions_operreact[i],"npc"),
                  just = "centre", hjust = NULL, vjust = NULL, rot = 0,
                  check.overlap = FALSE, default.units = "npc",
                  name = NULL, gp = gpar(fontsize = 15), draw = FALSE, vp = NULL)
    
  }
  
}

# manually draw 4 & 7
grid.text("6",
          x = unit(0.19,"npc"),
          y = unit(ypositions_operreact[4],"npc"),
          just = "centre", hjust = NULL, vjust = NULL, rot = 0,
          check.overlap = FALSE, default.units = "npc",
          name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)

grid.text("6",
          x = unit(0.19,"npc"),
          y = unit(ypositions_operreact[7],"npc"),
          just = "centre", hjust = NULL, vjust = NULL, rot = 0,
          check.overlap = FALSE, default.units = "npc",
          name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)



# position of escalation stage 5 (baseline)
for(i in 1:5) {
  grid.text("5",
            x = unit(0.6, "npc"),
            y = unit(labelpositions_operreact[i], "npc"),
            just = "centre", hjust = NULL, vjust = NULL, rot = 0,
            check.overlap = FALSE, default.units = "npc",
            name = NULL, gp = gpar(fontsize = 15), draw = TRUE, vp = NULL)
}

# draw significance

# significance line for reaction 5->7 
grid.lines(x=unit(c(0.6,coef_operreact_grid[11]),"npc"),
           y = unit(c(labelpositions_operreact[4],labelpositions_operreact[4]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

# significance line for reparations 5->7 
grid.lines(x=unit(c(0.6,coef_operreact_grid[14]),"npc"),
           y = unit(c(labelpositions_operreact[5],labelpositions_operreact[5]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

# significance line for illegal mining 5->6
grid.lines(x=unit(c(0.6,0.19),"npc"),
           y = unit(c(labelpositions_operreact[2],labelpositions_operreact[2]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

# significance line for latent mob 5->6
grid.lines(x=unit(c(0.6,0.19),"npc"),
           y = unit(c(labelpositions_operreact[3],labelpositions_operreact[3]),"npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(lty="dashed"), draw = TRUE, vp = NULL) 

