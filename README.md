# 측정과 잠재변수 모형 실습

척도 타당화, 구조방정식, 잠재계층분석을 다룬 대학원 수업 프로젝트 코드

- 대학원 수업의 기말 프로젝트와 세미나 실습에서 직접 작성한 코드
- 원자료, 결과 파일, 보고서 원문은 비공개

| 폴더 | 수업 | 내용 | 방법 | 도구 |
|---|---|---|---|---|
| [2024-1_burnout-scale-cfa](2024-1_burnout-scale-cfa) | 다변량심리데이터분석 (고려대) | 한국판 직무소진 척도(K-BAT) 문항 분석 | 확인적 요인분석, 고전검사이론 문항 분석 | R |
| [2024-1_mindreading-esem-replication](2024-1_mindreading-esem-replication) | 연구실 분석 실습 | 공개 논문의 분석 재현 | 문항 묶음(parcel) CFA, ESEM (Mplus 연동) | R, Mplus |
| [2025-1_lca-from-scratch](2025-1_lca-from-scratch) | 잠재변수모형 (고려대) | 잠재계층분석(LCA) 추정 알고리즘 구현 | 모의자료 생성, EM, Newton-Raphson | R |

## 2024-1 다변량심리데이터분석: 직무소진 척도 문항 분석

`kbat_cfa_item_analysis.Rmd`

- 한국판 직무소진평가척도(K-BAT) 22문항 응답 자료로 척도의 구조와 문항 양호도를 점검
- KMO, Bartlett 구형성 검정, 다변량 정규성 확인 후 1요인 모형과 4요인 모형(소진, 심리적 거리두기, 정서 손상, 인지 손상)을 CFA로 비교
- 고전검사이론 기준의 문항 난이도와 상하위 27% 변별도를 직접 함수로 계산

## 2024-1 연구실 실습: 마음읽기 측정 연구 재현

`mindreading_replication.Rmd`

- 연구실 분석 실습으로, 마음읽기(mindreading) 측정의 타당도를 다룬 공개 논문(*Mindreading Measures Misread? A Multimethod Investigation into the Validity of Self-Report and Task-Based Approaches*)의 OSF 공개 자료와 코드를 따라가며 분석을 재현
- 자기보고식과 과제 기반 마음읽기, 일반 인지기능, 심리사회적 기능 척도를 문항 묶음(parcel) 기반 CFA로 측정하고, `MplusAutomation`으로 ESEM 모형을 실행
- 원 논문 코드의 흐름을 단계별로 나누고 한국어 주석을 달아 재정리 (분석 설계는 원저자)

## 2025-1 잠재변수모형: LCA 추정 알고리즘 직접 구현

| 파일 | 내용 |
|---|---|
| `01_generate_data.Rmd` | 이직의도 수준이 다른 3개 잠재계층을 가정하고, 계층별 응답 확률을 정해 다범주 문항 9개의 모의자료 생성 |
| `02_em_newton_raphson.Rmd` | LCA 모수 추정을 EM, Newton-Raphson, 두 방법을 합친 하이브리드로 직접 구현하고, 초기값(참값, 무작위)에 따른 수렴과 추정 정확도를 비교 |
| `03_lca_latent_group.Rmd` | 잠재 집단 변수가 있는 다수준 형태의 LCA를 EM으로 구현해 GSS 자료에 적용 |
