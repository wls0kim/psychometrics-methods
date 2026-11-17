
- 이직의도 수준이 다른 3개 잠재계층과 계층별 응답확률을 가정해 모의자료
  생성

- LCA로 추정한 계층의 실제 계층 복원 정도 확인

- 필요한 패키지: `slca` (LCA 추정), `dplyr`

- 만드는 파일: `data.csv`(모의자료), `result.csv`(사후확률과 추정 계층)

- 순서: 모수 설정 → 계층별 응답확률 설정 → 자료 생성 → LCA 추정 → 실제
  계층과 비교

## 1. 준비

``` r
library(dplyr)  # 변수 변환
```

``` r
library(slca)   # 잠재계층분석 추정
```

## 2. 모수 설정

``` r
set.seed(123)

n_samples <- 1000 # 응답자 1,000명
n_classes <- 3  # 잠재계층 3개 (이직의도 낮음, 중간, 높음)
class_probs <- c(0.25, 0.35, 0.40)  # 계층 비율
n_items <- 9  # 문항 9개 (차원별 3개)

# 문항별 응답 범주
n_categories <- c(3, 4, 3,  # 조직 차원 (경력개발, 월급여, 비전)
                 3, 3, 3,   # 직무 차원 (업무자율성, 업무적합성, 직장내멘토)
                 4, 4, 3)   # 개인 차원 (근무시간, 재직기간, 직업정체성)
```

## 3. 계층별 응답확률

- 문항별, 계층별 응답 범주 선택 확률
- 행: 계층(1 = 이직의도 낮음, 2 = 중간, 3 = 높음), 열: 응답 범주, 각
  행의 합은 1

### 3.1. 조직 차원

``` r
response_probs <- list()

# 1. 경력 개발 지원 (1 = 관심 없음, 2 = 실질적 지원 부족, 3 = 적극 지원)
response_probs[[1]] <- matrix(c(
  0.05, 0.3, 0.65,  # 계층 1 (이직의도 낮음)
  0.2, 0.6, 0.2,  # 계층 2 (이직의도 중간)
  0.6, 0.3, 0.1   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)

# 2. 월 평균 급여 (1 = 300만원 미만, 2 = 300~600만원, 3 = 600~900만원, 4 = 900만원 이상)
response_probs[[2]] <- matrix(c(
  0.1, 0.1, 0.3, 0.5,  # 계층 1 (이직의도 낮음)
  0.3, 0.4, 0.2, 0.1,  # 계층 2 (이직의도 중간)
  0.5, 0.3, 0.15, 0.05   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)

# 3. 회사의 미래 비전 (1 = 불명확, 2 = 명확하지만 공감 어려움, 3 = 명확하고 개인 성장과 일치)
response_probs[[3]] <- matrix(c(
  0.05, 0.25, 0.7,  # 계층 1 (이직의도 낮음)
  0.3, 0.5, 0.2,  # 계층 2 (이직의도 중간)
  0.7, 0.2, 0.1   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)
```

### 3.2. 직무 차원

``` r
# 4. 업무 자율성 (1 = 자율성 낮음, 2 = 부분적 자율성, 3 = 자율성 높음)
response_probs[[4]] <- matrix(c(
  0.05, 0.15, 0.8,  # 계층 1 (이직의도 낮음)
  0.15, 0.7, 0.15,  # 계층 2 (이직의도 중간)
  0.6, 0.3, 0.1   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)

# 5. 업무 적합성 (1 = 잘 맞지 않음, 2 = 보통, 3 = 매우 잘 맞음)
response_probs[[5]] <- matrix(c(
  0.1, 0.2, 0.7,  # 계층 1 (이직의도 낮음)
  0.15, 0.7, 0.15,  # 계층 2 (이직의도 중간)
  0.75, 0.15, 0.1   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)

# 6. 직장 내 멘토 (1 = 없음, 2 = 비공식적 멘토, 3 = 공식적 멘토)
response_probs[[6]] <- matrix(c(
  0.1, 0.25, 0.65,  # 계층 1 (이직의도 낮음)
  0.2, 0.6, 0.2,  # 계층 2 (이직의도 중간)
  0.7, 0.2, 0.1   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)
```

### 3.3. 개인 차원

``` r
# 7. 근무시간 (1 = 40시간 미만, 2 = 40~45시간, 3 = 45~50시간, 4 = 50시간 이상)
response_probs[[7]] <- matrix(c(
  0.5, 0.3, 0.1, 0.1,  # 계층 1 (이직의도 낮음)
  0.1, 0.4, 0.3, 0.2,  # 계층 2 (이직의도 중간)
  0.1, 0.1, 0.3, 0.5   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)

# 8. 재직기간 (1 = 3년 미만, 2 = 3~5년, 3 = 6~10년, 4 = 11년 이상)
response_probs[[8]] <- matrix(c(
  0.1, 0.1, 0.2, 0.5,  # 계층 1 (이직의도 낮음)
  0.2, 0.3, 0.4, 0.1,  # 계층 2 (이직의도 중간)
  0.7, 0.15, 0.1, 0.05   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)

# 9. 직업 정체성 (1 = 약함, 2 = 중간, 3 = 강함)
response_probs[[9]] <- matrix(c(
  0.1, 0.25, 0.65,  # 계층 1 (이직의도 낮음)
  0.1, 0.6, 0.3,  # 계층 2 (이직의도 중간)
  0.75, 0.15, 0.1   # 계층 3 (이직의도 높음)
), nrow = n_classes, byrow = TRUE)
```

## 4. 자료 생성

- 응답자별로 계층을 먼저 추출한 뒤, 그 계층의 응답확률로 9개 문항 응답
  추출

``` r
data <- data.frame(matrix(NA, nrow = n_samples, ncol = n_items + 1))
colnames(data) <- c("turnover_class", paste0("item", 1:n_items))
# 실제 계층 (추정 결과와 비교하기 위해 보관)
data$turnover_class <- sample(1:n_classes, size = n_samples, replace = TRUE, prob = class_probs)

# 문항 응답 생성
for (i in 1:n_samples) {
  class_id <- data$turnover_class[i]
  
  for (j in 1:n_items) {
    data[i, j+1] <- sample(1:n_categories[j], size = 1, prob = response_probs[[j]][class_id, ])
  }
}
```

## 5. 변수 이름과 범주 이름 붙이기

``` r
# 변수 이름
colnames(data) <- c("turnover_class", 
                    "career", "salary", "vision",  # 조직 차원
                    "autonomy", "fit", "mentor",  # 직무 차원
                    "work_hour", "tenure", "identity")  # 개인 차원

data <- data %>%
  mutate(
    turnover_class = factor(turnover_class, levels = 1:3, labels = c("낮음", "중간", "높음")),
    career = factor(career, levels = 1:3, labels = c("관심 없음", "실질적 지원 부족", "적극 지원")),
    salary = factor(salary, levels = 1:4, labels = c("300만원 미만", "300~600만원", "600~900만원", "900만원 이상")),
    vision = factor(vision, levels = 1:3, labels = c("불명확", "명확하지만 공감 어려움", "명확 및 개인 성장과 일치")),
    autonomy = factor(autonomy, levels = 1:3, labels = c("자율성 낮음", "부분적 자율성", "자율성 높음")),
    fit = factor(fit, levels = 1:3, labels = c("잘 맞지 않음", "보통", "매우 잘 맞음")),
    mentor = factor(mentor, levels = 1:3, labels = c("없음", "비공식적 멘토", "공식적 멘토")),
    work_hour = factor(work_hour, levels = 1:4, labels = c("40시간 미만", "40~45시간", "45~50시간", "50시간 이상")),
    tenure = factor(tenure, levels = 1:4, labels = c("3년 미만", "3~5년", "6~10년", "11년 이상")),
    identity = factor(identity, levels = 1:3, labels = c("약함", "중간", "강함"))
  )

table(data$turnover_class) / n_samples  # 생성된 계층 비율 (설정값 0.25, 0.35, 0.40과 비교)
```

    ## 
    ##  낮음  중간  높음 
    ## 0.248 0.348 0.404

``` r
head(data)
```

<div class="kable-table">

| turnover_class | career | salary | vision | autonomy | fit | mentor | work_hour | tenure | identity |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| 높음 | 관심 없음 | 300~600만원 | 불명확 | 부분적 자율성 | 보통 | 없음 | 45~50시간 | 3년 미만 | 약함 |
| 낮음 | 적극 지원 | 900만원 이상 | 명확 및 개인 성장과 일치 | 자율성 높음 | 매우 잘 맞음 | 공식적 멘토 | 40~45시간 | 6~10년 | 강함 |
| 중간 | 실질적 지원 부족 | 300~600만원 | 명확하지만 공감 어려움 | 자율성 높음 | 보통 | 비공식적 멘토 | 50시간 이상 | 6~10년 | 중간 |
| 낮음 | 적극 지원 | 900만원 이상 | 명확 및 개인 성장과 일치 | 자율성 높음 | 매우 잘 맞음 | 공식적 멘토 | 40시간 미만 | 6~10년 | 강함 |
| 낮음 | 적극 지원 | 600~900만원 | 명확 및 개인 성장과 일치 | 자율성 높음 | 보통 | 비공식적 멘토 | 40시간 미만 | 11년 이상 | 강함 |
| 높음 | 관심 없음 | 300만원 미만 | 불명확 | 부분적 자율성 | 매우 잘 맞음 | 비공식적 멘토 | 40~45시간 | 6~10년 | 약함 |

</div>

``` r
write.csv(data, "data.csv", row.names = FALSE)
```

## 6. LCA 추정과 실제 계층 비교

### 6.1. 3계층 모형 추정

``` r
set.seed(123)

# 잠재변수 turnover(3개 계층)를 9개 문항으로 측정하는 모형
lca_model <- slca(turnover(3) ~ career + salary + vision + autonomy + fit + mentor + work_hour + tenure + identity)
lca_fit <- estimate(lca_model, data)

param(lca_fit)  # 추정된 계층 비율과 문항 응답확률
```

    ## PI :
    ## (turnover)
    ##   class
    ##          1       2       3
    ##     0.4001  0.3569  0.2430
    ## 
    ## RHO :
    ## (a)
    ##         class
    ## response       1       2       3
    ##    1(V1)  0.5751  0.1785  0.0418
    ##    2      0.3483  0.6445  0.3155
    ##    3      0.0766  0.1770  0.6428
    ##    1(V2)  0.4581  0.3345  0.0796
    ##    2      0.3379  0.3810  0.1105
    ##    3      0.1443  0.1924  0.2494
    ##    4      0.0597  0.0922  0.5605
    ##    1(V3)  0.6827  0.3141  0.0319
    ##    2      0.2158  0.4827  0.2073
    ##    3      0.1015  0.2032  0.7608
    ##    1(V4)  0.5956  0.1378  0.0515
    ##    2      0.2895  0.6787  0.1192
    ##    3      0.1149  0.1835  0.8293
    ##    1(V5)  0.7571  0.1757  0.0797
    ##    2      0.1649  0.6437  0.2153
    ##    3      0.0781  0.1806  0.7050
    ##    1(V6)  0.7090  0.1994  0.1322
    ##    2      0.1969  0.5989  0.2572
    ##    3      0.0941  0.2017  0.6105
    ##    1(V7)  0.1119  0.0944  0.5208
    ##    2      0.0602  0.4353  0.3274
    ##    3      0.3253  0.2468  0.0772
    ##    4      0.5026  0.2236  0.0746
    ##    1(V8)  0.7299  0.1966  0.0773
    ##    2      0.1191  0.3236  0.1352
    ##    3      0.1141  0.3602  0.1968
    ##    4      0.0370  0.1196  0.5906
    ##    1(V9)  0.7337  0.0912  0.1105
    ##    2      0.1685  0.5784  0.2024
    ##    3      0.0978  0.3304  0.6871
    ## 
    ##          V1     V2     V3     V4       V5  V6     V7        V8     V9      
    ## turnover career salary vision autonomy fit mentor work_hour tenure identity

``` r
post_probs <- round(lca_fit$posterior$marginal$turnover, 3)  # 응답자별 계층 사후확률
pred_cls   <- predict(lca_fit, data, type = "class")         # 사후확률이 가장 큰 계층
```

### 6.2. 혼동행렬과 분류 지표

- LCA는 계층 번호를 임의로 붙이므로(label switching) 추정된 응답확률을
  보고 계층 1 = 높음, 2 = 중간, 3 = 낮음으로 대응
- 시드나 패키지 버전이 바뀌면 대응 재확인 필요

``` r
conf_matrix <- table(True_Class = data$turnover_class, Predicted_Class = pred_cls$turnover)
conf_matrix
```

    ##           Predicted_Class
    ## True_Class   1   2   3
    ##       낮음   1   9 238
    ##       중간  17 324   7
    ##       높음 381  23   0

``` r
colnames(conf_matrix) <- c("높음", "중간", "낮음")  # 추정 계층 1, 2, 3에 이름 붙이기
conf_matrix <- conf_matrix[c("높음", "중간", "낮음"), ]  # 실제 계층 순서를 열 순서와 맞추기
conf_matrix
```

    ##           Predicted_Class
    ## True_Class 높음 중간 낮음
    ##       높음  381   23    0
    ##       중간   17  324    7
    ##       낮음    1    9  238

``` r
recall_per_class <- diag(conf_matrix) / rowSums(conf_matrix)
mean(recall_per_class)  # Balanced Accuracy
```

    ## [1] 0.9445937

``` r
precision_per_class <- diag(conf_matrix) / colSums(conf_matrix)
f1_per_class <- 2 * (precision_per_class * recall_per_class) / (precision_per_class + recall_per_class)
mean(f1_per_class, na.rm = TRUE)  # Macro F1 Score
```

    ## [1] 0.9449711

### 6.3. 응답자별 결과표

``` r
pred_cls_named <- factor(pred_cls$turnover, 
                         levels = c(1, 2, 3), 
                         labels = c("높음", "중간", "낮음")) 

result_table <- cbind(post_probs, 
                      turnover_pred = as.character(pred_cls_named),
                      turnover_real = as.character(data$turnover_class)) # 사후확률, 추정 계층, 실제 계층

head(result_table)
```

    ##   1       2       3       turnover_pred turnover_real
    ## 1 "0.991" "0.009" "0"     "높음"        "높음"       
    ## 2 "0"     "0"     "1"     "낮음"        "낮음"       
    ## 3 "0.003" "0.996" "0.002" "중간"        "중간"       
    ## 4 "0"     "0"     "1"     "낮음"        "낮음"       
    ## 5 "0"     "0.002" "0.998" "낮음"        "낮음"       
    ## 6 "0.187" "0.813" "0"     "중간"        "높음"

``` r
write.csv(result_table, "result.csv", row.names = FALSE)
```
