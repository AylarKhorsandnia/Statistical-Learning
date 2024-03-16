library(data.table)
library(ggplot2)
Sys.setlocale(locale = 'persian')

d = fread('C:/Users/aylar/Downloads/Uni Courses/1401-2 Regression - Mirsadeghi/Aylar/CHW1/iranprovs_mortality_monthly.csv', encoding = 'UTF-8')

d$ym_num = d$y + d$m / 13

ds = d[, .(n = sum(n)), .(y, m, ym_num, prov)]

month_str = c("1398/10", "98/11", "98/12", "99/1", "99/2", 
              "99/3", "99/4", "99/5", "99/6", "99/7", 
              "99/8", "99/9", "99/10", "99/11", "99/12",
              "400/1", "400/2", "400/3", "400/4", "400/5", "400/6", "400/7", 
              "400/8", "400/9", "400/10", "400/11", "400/12",
              "401/1", "401/2", "401/3", "401/4", "401/5", "401/6", "401/7")
#############

M = 5
PROV = unique(ds$prov)[29]
dsm = ds[prov == PROV & m == M,]
ggplot(dsm, aes(ym_num, n))+
#  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1389:1401)+
  geom_vline(xintercept = 1398 + 10/13, linetype = 'dashed')+
  ggtitle(label = PROV, subtitle = paste('month: ', M))

################

final_table <- as.data.frame(matrix(0, nrow = 31, ncol = 34))
final_table_normalized <- as.data.frame(matrix(0, nrow = 31, ncol = 34))

rownames(final_table) = unique(ds$prov)
rownames(final_table_normalized) = unique(ds$prov)
colnames(final_table) = month_str
colnames(final_table_normalized) = month_str

for (i in c(1:31)){
  for (M in c(1:12)){
    PROV = unique(ds$prov)[i]
    dsm = ds[prov == PROV & m == M,]
    ym_num_covid = 1398 + 10/12 - 1/24
    
    dsm2fit = dsm[ym_num > 1398 + 10/12 - 1/24 - 6 & 
                    ym_num < ym_num_covid]
    
    fit = lm(n ~ ym_num, dsm2fit)
    f <- summary(fit)$fstatistic
    p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    if (p_value > 0.05){
      dsm$predict = mean(dsm2fit$n)
      sigma = sd(dsm2fit$n)
    } else{
      sigma <- summary(fit)$sigma
      dsm$predicte = predict(fit, dsm)
    }
    dsm$difference = dsm$n - dsm$predict
    dsm$difference[dsm$difference < 2*sigma] = 0
    dsm$difference_normalized = 100*dsm$difference/dsm$n
    dsm_after_covid = dsm[ym_num >= ym_num_covid]
    dsm_after_covid$col_number = 12*(dsm_after_covid$y - 1398) + dsm_after_covid$m - 9
    dsm_after_covid = dsm_after_covid[, .(col_number, difference, difference_normalized, n)]

    for(j in c(1: nrow(dsm_after_covid))){
    final_table[i, dsm_after_covid$col_number[j]] = as.integer(dsm_after_covid$difference[j])
    final_table_normalized[i, dsm_after_covid$col_number[j]] = as.double(dsm_after_covid$difference_normalized[j])
    }
  }
}

library(RColorBrewer)  
heatmap(as.matrix(final_table), col=brewer.pal(9, "Blues"), Colv = NA, Rowv = NA)
heatmap(as.matrix(final_table_normalized), col=brewer.pal(9, "Blues"), Colv = NA, Rowv = NA)


#Question 2
Total_death = sum(final_table)

#Question 3
total_death_prov = as.matrix(rowSums(as.matrix(final_table)))

         
#Question 4
final_table_4 <- as.data.frame(matrix(0, nrow = 31, ncol = 34))
final_table_normalized_4 <- as.data.frame(matrix(0, nrow = 31, ncol = 34))

rownames(final_table_4) = unique(ds$prov)
rownames(final_table_normalized_4) = unique(ds$prov)
colnames(final_table_4) = month_str
colnames(final_table_normalized_4) = month_str

ds = d[, .(n=sum(n)), .(y, m, ym_num, prov, age_group)]
ds = ds[ds$age_group == "25-29" | ds$age_group == "30-34" | ds$age_group == "35-39" |
          ds$age_group == "40-44" | ds$age_group == "45-49" | ds$age_group == "50-54" |
          ds$age_group == "55-59" | ds$age_group == "60-64" |ds$age_group == "65-69" |
          ds$age_group == "70-75"]
ds = d[, .(n=sum(n)), .(y, m, ym_num, prov)]
for (i in c(1:31)){
  for (M in c(1:12)){
    PROV = unique(ds$prov)[i]
    dsm = ds[prov == PROV & m == M,]
    ym_num_covid = 1398 + 10/12 - 1/24
    
    dsm2fit = dsm[ym_num > 1398 + 10/12 - 1/24 - 6 & 
                    ym_num < ym_num_covid]
    
    fit = lm(n ~ ym_num, dsm2fit)
    f <- summary(fit)$fstatistic
    p_value <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    if (p_value > 0.05){
      dsm$predict = mean(dsm2fit$n)
      sigma = sd(dsm2fit$n)
    } else{
      sigma <- summary(fit)$sigma
      dsm$predicte = predict(fit, dsm)
    }
    dsm$difference = dsm$n - dsm$predict
    dsm$difference[dsm$difference < 2*sigma] = 0
    dsm$difference_normalized = 100*dsm$difference/dsm$n
    dsm_after_covid = dsm[ym_num >= ym_num_covid]
    dsm_after_covid$col_number = 12*(dsm_after_covid$y - 1398) + dsm_after_covid$m - 9
    dsm_after_covid = dsm_after_covid[, .(col_number, difference, difference_normalized, n)]
    
    for(j in c(1: nrow(dsm_after_covid))){
      final_table_4[i, dsm_after_covid$col_number[j]] = as.integer(dsm_after_covid$difference[j])
      final_table_normalized_4[i, dsm_after_covid$col_number[j]] = as.double(dsm_after_covid$difference_normalized[j])
    }
  }
}

heatmap(as.matrix(final_table_4), col=brewer.pal(9, "Reds"), Colv = NA, Rowv = NA)
heatmap(as.matrix(final_table_normalized_4), col=brewer.pal(9, "Reds"), Colv = NA, Rowv = NA)

total_death_prov_4 = as.matrix(rowSums(as.matrix(final_table_normalized_4)))
