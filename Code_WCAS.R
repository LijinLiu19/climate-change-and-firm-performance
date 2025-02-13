#1.Baseline
library(readxl)
data<- read_excel("C:/Users/29381/Desktop/firm/wenjianjia/data.xlsx")
dat<-data
X<-dat[,8:21]
W<-dat$shock1
Y<-dat$sales/10000
summary(dat)
library(grf)
f1 <- causal_forest(X, Y, W, num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
 honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05)
average_treatment_effect(f1)
test_calibration(f1)
best_linear_projection(f1)
f2 <- causal_forest(X, Y, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$industry)
average_treatment_effect(f2)
test_calibration(f2)
best_linear_projection(f2)
f3 <- causal_forest(X, Y, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$cid)
average_treatment_effect(f3)
test_calibration(f3)
best_linear_projection(f3)
f4 <- causal_forest(X, Y, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$hhid)
average_treatment_effect(f4,"all")
test_calibration(f4)
best_linear_projection(f4)
Y1<-dat$employment
f5 <- causal_forest(X, Y1, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05)
average_treatment_effect(f5)
test_calibration(f5)
best_linear_projection(f5)
f6 <- causal_forest(X, Y1, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$industry)
average_treatment_effect(f6,"treat")
test_calibration(f6)
best_linear_projection(f6)
f7 <- causal_forest(X, Y1, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$cid)
average_treatment_effect(f7,"treat")
test_calibration(f7)
best_linear_projection(f7)
f8 <- causal_forest(X, Y1, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$hhid)
average_treatment_effect(f8,"overlap")
test_calibration(f8)
best_linear_projection(f8)
Y2<-dat$profit/10000
f9 <- causal_forest(X, Y2, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$cid)
average_treatment_effect(f9)
test_calibration(f9)
best_linear_projection(f9)
f10<-causal_forest(X, Y2, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                   honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05)
average_treatment_effect(f10)
test_calibration(f10)
best_linear_projection(f11)
f11<- causal_forest(X, Y2, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$industry)
average_treatment_effect(f11)
test_calibration(f11)
average_treatment_effect(f11,"treat")
test_calibration(f11)
f12<- causal_forest(X, Y2, W,num.trees = 2000, sample.fraction = 0.5, min.node.size = 5,
                    honesty = TRUE, honesty.fraction = 0.5,alpha = 0.05,clusters = X$hhid)
average_treatment_effect(f12,"all")
test_calibration(f12)
d1<-average_treatment_effect(f3)
#Calculate 95% confidence intervals
higer_ci1=d1["estimate"] + 1.96 * d1["std.err"]
higer_ci1
lower_ci1=d1["estimate"] - 1.96 * d1["std.err"]
lower_ci1
d2<-average_treatment_effect(f7)
higer_ci2=d2["estimate"] + 1.96 * d2["std.err"]
d3<-average_treatment_effect(f9)
higer_ci3=d2["estimate"] + 1.96 * d2["std.err"]
da1<-f3$predictions
fil_data1 <- da1[da1 <=higer_ci1]
da2<-f7$predictions
library(ggplot2)
fil_data2 <- da2[da2 <=higer_ci2]
da3<-f9$predictions
fil_data3 <- da3[da3 <=higer_ci3]
#Plot the distribution of treatment effects
install.packages("cowplot")
library(cowplot)
p1<-ggplot(mapping=aes(f3$predictions )) +
  geom_density(fill = "skyblue") +
  xlab("Treatment Effects") + ylab("Density")+
  geom_vline(xintercept =0, color = "red", linetype = "dashed")+ggtitle("Sales")
p2<-ggplot(mapping=aes(x =f7$predictions )) +
  geom_density(fill = "blue") +
  xlab("Treatment Effects") + ylab("Density")+
  geom_vline(xintercept =0, color = "red", linetype = "dashed") +ggtitle("Employment")
p3<-ggplot(mapping=aes(x =f9$predictions )) +
  geom_density(fill = "green") +
  xlab("Treatment Effects") + ylab("Density")+
  geom_vline(xintercept =0, color = "red", linetype = "dashed")+ggtitle("Profit") 
plot_grid(p1, p2,p3, nrow = 1)
#Calculate the importance of variables and rank them in order of importance
importance_df <- data.frame(
  Variable = paste0("X", 1:14),
  Importance = variable_importance(f3)
)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

p4<-ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  ggtitle("Sales") +
  theme_minimal()

importance_df1 <- data.frame(
  Variable = paste0("X", 1:14),
  Importance = variable_importance(f7)
)
importance_df1 <- importance_df1[order(importance_df1$Importance, decreasing = TRUE), ]

p5<-ggplot(importance_df1, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  ggtitle("Employment") +
  theme_minimal()

importance_df2 <- data.frame(
  Variable = paste0("X", 1:14),
  Importance = variable_importance(f9)
)
importance_df2 <- importance_df2[order(importance_df2$Importance, decreasing = TRUE), ]

p6<-ggplot(importance_df2, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Variables") +
  ylab("Importance") +
  ggtitle("Profit") +
  theme_minimal()
plot_grid(p4, p5,p6, nrow = 3)

#2.Robustness checks,We follow the settings in the baseline model.
#2.2Recalculating W
W1<-dat$shock2
f1_2_1 <- causal_forest(X, Y, W1,clusters = X$cid)
f2_2_1 <- causal_forest(X, Y1, W1,clusters = X$cid)
f3_2_1 <- causal_forest(X, Y2, W1,clusters = X$cid)
verage_treatment_effect(f1_2_1)
verage_treatment_effect(f2_2_1)
verage_treatment_effect(f3_2_1)
W2<-dat$shock3
f1_2_2 <- causal_forest(X, Y, W2,clusters = X$cid)
f2_2_2 <- causal_forest(X, Y1, W2,clusters = X$cid)
f3_2_2 <- causal_forest(X, Y2, W2,clusters = X$cid)
average_treatment_effect(f1_2_2)
average_treatment_effect(f2_2_2)
average_treatment_effect(f3_2_2)
#2.4Recalculation of W after taking into account regional differences
W2<-dat$shock4
a1<-causal_forest(X, Y, W4,clusters = X$cid)%>%average_treatment_effect()
a2 <- causal_forest(X, Y2, W4,clusters = X$cid)%>%average_treatment_effect()
a3 <- causal_forest(X, Y3, W4,clusters = X$cid)%>%average_treatment_effect()

#2.1Confounding factor test,Drawing the mountains of treatment effects.
X1<-dat[,8:22]
f11 <- causal_forest(X1, Y, W,clusters = X$cid)
f21 <- causal_forest(X1, Y2, W,clusters = X$cid)
f31 <- causal_forest(X1, Y3, W,clusters = X$cid)
X2<-dat[,8:23]
f12 <- causal_forest(X2, Y, W,clusters = X$cid)
f22 <- causal_forest(X2, Y2, W,clusters = X$cid)
f32 <- causal_forest(X2, Y3, W,clusters = X$cid)
X3<-dat[,c(8,9,10,11,12,13,15,16,17,18,19,20,21)]
f13 <- causal_forest(X3, Y, W,clusters = X$cid)
f23 <- causal_forest(X3, Y2, W,clusters = X$cid)
f33 <- causal_forest(X3, Y3, W,clusters = X$cid)
X4<-dat[,c(8,9,10,11,12,13,15,16,18,20,21)]
X5<-dat[,c(9,10,11,12,13,15,16,18,19,20,21)]
f14 <- causal_forest(X4, Y, W,clusters = X$cid)
f24 <- causal_forest(X5, Y2, W,clusters = X$cid)
f34 <- causal_forest(X5, Y3, W,clusters = X$cid)
chars <- c("Base", "Test1", "Test2","Test3","Test4")  
repeated_chars_strings <- rep(chars, each=11590) 

data4<- data.frame(
  group =repeated_chars_strings,
  value = c(f1$predictions, f11$predictions,f12$predictions,f13$predictions,f14$predictions)
)

library(ggridges)
library(ggplot2)
p4<-ggplot(data4, aes(x = value, y = group, fill = group)) +
  geom_density_ridges(alpha = 0.7) +
  xlab("Treatment Effects") + ylab("Density") +
  ggtitle("Sales")

data5<- data.frame(
  group =repeated_chars_strings,
  value = c(f2$predictions, f21$predictions,f22$predictions,f23$predictions,f24$predictions)
)
p5<-ggplot(data5, aes(x = value, y = group, fill = group)) +
  geom_density_ridges(alpha = 0.7) +
  xlab("Treatment Effects") + ylab("Density") +
  ggtitle("Employment")

data6<- data.frame(
  group =repeated_chars_strings,
  value = c(f3$predictions, f31$predictions,f32$predictions,f33$predictions,f34$predictions)
)
p6<-ggplot(data6, aes(x = value, y = group, fill = group)) +
  geom_density_ridges(alpha = 0.7) +
  xlab("Treatment Effects") + ylab("Density") +
  ggtitle("Profit")
library(cowplot)
plot_grid(p4, p5,p6,nrow =1)

#New data1
library(readxl)
data1<- read_excel("C:/Users/29381/Desktop/data1.xlsx")
dat<-data1
X<-dat[,8:21]
W<-dat$shock1
Y1<-dat$Sales/10000
Y2<-dat$Employment
Y3<-dat$Profit/10000
a<-causal_forest(X, Y, W,clusters = X$cid)%>%average_treatment_effect()
a2 <- causal_forest(X, Y2, W,clusters = X$cid)%>%average_treatment_effect()
a3 <- causal_forest(X, Y3, W,clusters = X$cid)%>%average_treatment_effect()

#New data2
library(readxl)
data2<- read_excel("C:/Users/29381/Desktop/data2.xlsx")
dat22<-data2
X<-dat[,8:21]
W<-dat$shock1
Y1<-dat$Sales/10000
Y2<-dat$Employment
Y3<-dat$Prifit/10000
a<-causal_forest(X, Y, W,clusters = X$cid)%>%average_treatment_effect()
a2 <- causal_forest(X, Y2, W,clusters = X$cid)%>%average_treatment_effect()
a3 <- causal_forest(X, Y3, W,clusters = X$cid)%>%average_treatment_effect()

#New data3
library(readxl)
data3<- read_excel("C:/Users/29381/Desktop/data3.xlsx")
dat33<-data3
X<-dat[,8:21]
W<-dat$shock1
Y1<-dat$Sales/10000
Y2<-dat$Employment
Y3<-dat$Profit/10000
b<-causal_forest(X, Y, W,clusters = X$cid)%>%average_treatment_effect()
b2 <- causal_forest(X, Y2, W,clusters = X$cid)%>%average_treatment_effect()
b3 <- causal_forest(X, Y3, W,clusters = X$cid)%>%average_treatment_effect()

#New data4
library(readxl)
data4<- read_excel("C:/Users/29381/Desktop/data4.xlsx")
dat33<-data4
X<-dat[,8:21]
W<-dat$shock1
Y1<-dat$Sales/10000
Y2<-dat$Employment
Y3<-dat$Profit/10000
d1<-causal_forest(X, Y, W,clusters = X$cid)%>%average_treatment_effect()
d2 <- causal_forest(X, Y2, W,clusters = X$cid)%>%average_treatment_effect()
d3 <- causal_forest(X, Y3, W,clusters = X$cid)%>%average_treatment_effect()

library(readxl)
data5<- read_excel("C:/Users/29381/Desktop/data5.xlsx")
dat33<-data5
X<-dat[,8:21]
W<-dat$shock1
Y1<-dat$Sales/10000
Y2<-dat$Employment
Y3<-dat$Profit/10000
r1<-causal_forest(X, Y, W,clusters = X$cid)%>%average_treatment_effect()
r2 <- causal_forest(X, Y2, W,clusters = X$cid)%>%average_treatment_effect()
r3 <- causal_forest(X, Y3, W,clusters = X$cid)%>%average_treatment_effect()

#3.Mechanism analysis
library(readxl)
data6<- read_excel("C:/Users/29381/Desktop/data6.xlsx")
dat2<-data6
X2<-dat2[,15:32]
W2<-dat2$shock1
Y2<-dat$income/10000
Y2_1<-dat2$consumption/10000
Y2_2<-dat2$consumption1/1000
Y2_3<-dat2$consumption2/1000
Y2_4<-dat2$consumption3/1000
a<-causal_forest(X2, Y2_1, W2,clusters = X2$city_lab)%>%average_treatment_effect()
a1<- causal_forest(X2, Y2_1, W2,clusters = X2$city_lab)%>%average_treatment_effect()
a2<-causal_forest(X2, Y2_2, W2,clusters = X2$city_lab)%>%average_treatment_effect()
a3<-causal_forest(X2, Y2_3, W2,clusters = X2$city_lab)%>%average_treatment_effect()
a4<-causal_forest(X2, Y2_4, W2,clusters = X2$city_lab)%>%average_treatment_effect()
higer_ci1=a1["estimate"] + 1.96 * a1["std.err"]
lower_ci1= a1["estimate"] - 1.96 * a1["std.err"]
higer_ci2=a2["estimate"] + 1.96 * a2["std.err"]
lower_ci2=a2["estimate"] - 1.96 * a2["std.err"]
higer_ci3=a3["estimate"] + 1.96 * a3["std.err"]
lower_ci3=a3["estimate"] - 1.96 * a3["std.err"]
higer_ci4=a4["estimate"] + 1.96 * a4["std.err"]
lower_ci4=a4["estimate"] - 1.96 * a4["std.err"]

studies <- c("Total Consumption", "Subsistence Consumption", "Developmental Consumption", "Entertainment consumption")
effect_size <- c(a1, a2, a3,a4 )
ci_low <- c(lower_ci1, lower_ci2,lower_ci3,lower_ci4 )
ci_high <- c(higer_ci1, higer_ci2, higer_ci3, -higer_ci4)
library(forestplot)
forestplot(labeltext = studies,   
           mean = effect_size,   
           lower =ci_low ,   
           upper = ci_high,  
           xlab = "Treatment Effects",  
           boxsize = 0.07,
           col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"))

library(readxl)
data<- read_excel("C:/Users/29381/Desktop/data.xlsx")
dat<-data
X<-dat[,8:21]
W<-dat$shock1
Y<-dat$profitability
Y1<-dat$coest
j1<-causal_forest(X, Y, W,clusters = X2$cid)%>%average_treatment_effect()
j2<-causal_forest(X, Y1, W,clusters = X2$cid)%>%average_treatment_effect()
j3<-survival_forest(X,Y1,W, num.trees = 1000, sample.fraction = 0.5,
                    mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                    honesty = TRUE,
                    honesty.fraction = 0.5,
                    honesty.prune.leaves = TRUE,
                    alpha = 0.05,
                   )%>%average_treatment_effect()

#4Heterogeneity analysis
library(readxl)
data<- read_excel("C:/Users/29381/Desktop/data.xlsx")
dat<-data
X<-dat[,8:21]
W<-dat$shock1
Y<-dat$Sales/10000
library(grf)
f1 <- causal_forest(X, Y, W,clusters = X$cid)
plot(tree <- get_tree(f1,1))
Y1<-dat$Employment
f2 <- causal_forest(X, Y1, W,clusters = X$cid)
Y2<-dat$Profit/10000
f3 <- causal_forest(X, Y2, W,clusters = X$cid)
library(ggplot2)

p1<-ggplot(mapping=aes(x=X$asset,y=f1$predictions))
     +geom_smooth()
        +xlab('Asset')
            +ylab('Treatment Effects')+ggtitle("Sales")
p2<-ggplot(mapping=aes(x=X$age,y=f1$predictions))
      +geom_smooth()
          +xlab('age')
                +ylab('Treatment Effects')+ggtitle("Sales")
p3<-ggplot(mapping=aes(x=X$asset,y=f2$predictions))
      +geom_smooth()+xlab('Asset')+
             ylab('Treatment Effects')+ggtitle("Employment")
p4<-ggplot(mapping=aes(x=X$age,y=f2$predictions))
      +geom_smooth()+xlab('Age')+
            ylab('Treatment Effects')+ggtitle("Employment")
p5<-ggplot(mapping=aes(x=X$asset,y=f3$predictions))
       +geom_smooth()+xlab('Asset')+
            ylab('Treatment Effects')+ggtitle("Profit")
p6<-ggplot(mapping=aes(x=X$age,y=f3$predictions))
       +geom_smooth()+xlab('Age')+
             ylab('Treatment Effects')+ggtitle("Profit")
library(cowplot)
plot_grid(p1,p2,p3,p4,p5,p6,nrow = 2)
write.csv(f1$predictions, file = "output1.csv", row.names = TRUE)
write.csv(f2$predictions, file = "output2.csv", row.names = TRUE)
write.csv(f3$predictions, file = "output3.csv", row.names = TRUE)

#Adaptive strategies
library(readxl)
Adaptive<- read_excel("C:/Users/29381/Desktop/Adaptive.xlsx")
dat<-Adaptive
XY<-daty[,8:22]
WY<-daty$shock1
YY<-daty$Sales/10000
YY1<-daty$Employment
YY2<-daty$Profit/10000
library(grf)
f1 <- causal_forest(XY, YY, WY,clusters = XY$cid)
f2<-causal_forest(XY, YY1, WY,clusters = XY$cid)
f3<-causal_forest(XY, YY2, WY,clusters = XY$cid)
XY1<-daty[,c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,23)]
f4 <- causal_forest(XY1, YY, WY,clusters = XY1$cid)
f5<-causal_forest(XY1, YY1, WY,clusters = XY1$cid)
f6<-causal_forest(XY1, YY2, WY,clusters = XY1$cid)
XY2<-daty[,c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,24)]
f7 <- causal_forest(XY2, YY, WY,clusters = XY2$cid)
f8<-causal_forest(XY2, YY1, WY,clusters = XY2$cid)
f9<-causal_forest(XY2, YY2, WY,clusters = XY2$cid)

p9<-ggplot(mapping=aes(x=XY2$digitalfinance,y=f9$predictions))
       +geom_smooth()+xlab('digital finance')+
           ylab('Treatment Effects')+ggtitle("Profit")
p8<-ggplot(mapping=aes(x=XY2$digitalfinance,y=f8$predictions))
          +geom_smooth()+xlab('digital finance')+
               ylab('Treatment Effects')+ggtitle("Employment")
p7<-ggplot(mapping=aes(x=XY2$digitalfinance,y=f7$predictions))
           +geom_smooth()+xlab('digital finance')+
               ylab('Treatment Effects')+ggtitle("Sales")
p6<-ggplot(mapping=aes(x=XY1$mechanization,y=f6$predictions))
           +geom_smooth()+xlab('mechanization')+
                 ylab('Treatment Effects')+ggtitle("Profit")
p5<-ggplot(mapping=aes(x=XY1$mechanization,y=f5$predictions))
         +geom_smooth()+xlab('mechanization')+
             ylab('Treatment Effects')+ggtitle("Employment")
p4<-ggplot(mapping=aes(x=XY1$mechanization,y=f4$predictions))
           +geom_smooth()+xlab('mechanization')+
                  ylab('Treatment Effects')+ggtitle("Sales")
p1<-ggplot(mapping=aes(x=XY$effectivearea,y=f1$predictions))
            +geom_smooth()+xlab('effective area')+
                     ylab('Treatment Effects')+ggtitle("Sales")
p3<-ggplot(mapping=aes(x=XY$effectivearea,y=f3$predictions))
         +geom_smooth()+xlab('effective area')+
                ylab('Treatment Effects')+ggtitle("Profit")
p2<-ggplot(mapping=aes(x=XY$effectivearea,y=f2$predictions))
       +geom_smooth()+xlab('effective area')+
               ylab('Treatment Effects')+ggtitle("Employment")

library(cowplot)
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow = 3)