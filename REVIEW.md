# Critical Review: PPMI Thesis (2017)

Retrospective assessment of *"Personalised Map Matched Imputation: imputing missing data from smartphone location logs"* by Boaz Sobrado, Utrecht University.

---

## What Works Well

The core insight — that human mobility is highly regular and that a personalised spatial map can exploit this regularity for imputation — is genuinely clever and ahead of its time. The three-pronged evaluation design (map quality, cross-validation, external validation with transit cards) shows good experimental instincts. The spatial downsampling argument (averaging hundreds of observations per bin vs. a few per time window) is compelling and well-articulated. The codebase is reasonably organised for a 2017 thesis.

---

## Statistical & Methodological Gaps

### 1. Single-Subject Evaluation

The manuscript states the method was evaluated on data from "a single individual." While data from 3 participants (Boaz, Sipke, Peter) was collected and used for descriptive statistics (`scripts/auxDataDetails.R`), only one participant's data was used for PPMI evaluation. This is the most critical limitation — no claims about method performance can generalise beyond one person's mobility pattern. A commuter, delivery driver, and retiree would produce very different results.

**What should have been done:** Evaluate PPMI on all 3 available participants and report per-subject and aggregate results. Even N=3 with consistent improvements would be far more convincing than N=1.

### 2. No Formal Statistical Testing

There are zero p-values, confidence intervals, or significance tests in the entire thesis. All method comparisons are purely descriptive (raw means and medians in Table 1). Claims like "performs substantially better" have no statistical backing.

**What should have been done:** At minimum, paired Wilcoxon signed-rank tests on per-period distance errors between methods, with effect sizes. Bootstrap confidence intervals on the mean/median distance metrics.

### 3. No Proper Cross-Validation

The evaluation uses a single train/test split (75/25) with a fixed random seed (`set.seed(1234)` in `getRemoveIndexFunc.R`). The neural network uses Keras' internal `validation_split` parameter, which is not equivalent to proper held-out evaluation. There are no repeated trials and no variance estimates.

**What should have been done:** K-fold temporal cross-validation (k=10) across multiple months, reporting mean ± standard deviation. Multiple random seeds to assess stability.

### 4. Inadequate Sample Size at Day Level

Removing 25% of days from a single month yields approximately 7-8 test cases for the 1-day evaluation. This is far too few to draw reliable conclusions, yet the day-level result is presented as a headline finding.

**What should have been done:** Use multiple months (the full 6-month Netherlands period) and temporal CV to increase the effective sample size. Report per-fold results.

### 5. Missing Data Mechanism Not Analysed

The thesis never examines whether GPS missingness is MCAR, MAR, or MNAR. If the phone systematically loses signal underground, indoors, or during travel, then the missing data is structurally different from observed data. Simulating missingness by randomly removing observed periods does not capture this reality.

Evidence from the code: accuracy filtering (`accuracyLim = 250m`) removes 9% of measurements before evaluation. The Palmius implementation further filters by speed (`maxSpeed = 27.7 m/s`). These preprocessing steps may systematically exclude the hardest-to-impute cases.

**What should have been done:** Characterise the missingness mechanism. Compare the distribution of accuracy, time-of-day, and location for observed vs. missing periods. Simulate missingness that matches the observed pattern (e.g., longer gaps at night, signal loss in buildings).

### 6. Parameter Selection by Visual Inspection

Nearly every parameter is hardcoded or chosen by visual inspection:

| Parameter | Value | Justification in Thesis |
|-----------|-------|------------------------|
| Pause time threshold (`timeLim`) | 300s | None |
| Pause distance threshold (`distLim`) | 50m | None |
| Minimum pause duration (`minPause`) | 100s | None |
| Clustering height (`d`) | 150–450m | Visual inspection |
| Accuracy filter (`accuracyLim`) | 250m | None |
| Path clustering distance (`dPathLim`) | 300m | None |
| DTW clustering height (`h`) | 0.2 | None |
| Grid cell size (`cs`) | 0.5° (~55km) | None |
| NN architecture (250→120) | Arbitrary | None |
| NN epochs (10) | Arbitrary | None |
| NN dropout (0.4, 0.3) | Arbitrary | None |
| NN batch size (200) | Arbitrary | None |

The manuscript acknowledges the bias-precision tradeoff (§ Results, Map Building) but doesn't quantify it.

**What should have been done:** Sensitivity analysis showing how results change across a grid of parameter values. Data-driven parameter selection (e.g., silhouette scores for clustering distance, early stopping for NN epochs).

### 7. Neural Network Issues

- The softmax outputs are severely overconfident (median confidence = 1.0), making the uncertainty estimates useless.
- The architecture is a vanilla feedforward network with no sequence modeling — it only sees the previous and next bin, missing longer temporal patterns.
- An LSTM was attempted (`PPMI/scripts/nnLSTM.R`) but abandoned with the comment "doesn't work!"
- No hyperparameter tuning, no ablation study, no learning curves reported.
- 1222 output classes (cluster bins) with many rare classes — a known difficulty for classification that is never addressed.

**What should have been done:** Temperature scaling or Platt scaling for calibrated probabilities. Hyperparameter search (even random search). Ablation study showing the contribution of each input feature. Class imbalance handling (weighted loss, focal loss, or merging rare bins).

### 8. Comparison Fairness

The thesis acknowledges the comparison is "somewhat unfair" since Palmius and Barnett & Onnella were designed for custom (high-frequency) logs rather than secondary (low-frequency) logs. Yet conclusions about PPMI's superiority are still drawn from these comparisons.

Additional fairness issues:
- **Different information access:** PPMI uses the full feature set with temporal features; Palmius uses rule-based distance thresholds; Barnett & Onnella uses spatial simulation.
- **Different preprocessing:** Each method applies different filtering before evaluation.
- **Home coordinate hardcoding:** Palmius has hardcoded home coordinates (`home <- c(5.113919, 52.10421)`); PPMI infers home from clustering.
- **Coverage asymmetry:** Palmius achieves 0% coverage for 1-day intervals. Comparing mean distance when one method refuses to impute hard cases is misleading — it inflates PPMI's errors relative to methods that only impute easy cases.

**What should have been done:** Compare on the intersection of cases all methods impute (matched comparison). Report coverage-adjusted metrics. Use a time-of-day baseline (predict where the person was at this time on previous days) as a stronger baseline.

### 9. External Validation Weakness

The public transport validation uses only 97 events from a single month. The results are mixed: PPMI achieves higher accuracy (24% vs. 16%) but worse median distance (1037m vs. 555m) compared to the naive baseline. This is too few observations and too ambiguous a result to support strong claims.

**What should have been done:** Collect more ground truth data (professional GPS device worn in parallel, or longer transit card records). Report confidence intervals on the 97-event comparison.

### 10. No Multiple Imputation

The thesis generates single point imputations. Multiple imputation — generating several plausible trajectories and pooling results — is the standard approach in missing data research (Rubin, 1987). The thesis discusses using prediction probabilities for uncertainty but never implements it.

The manuscript actually hints at this capability: "with PPMI it is possible to model uncertainty using the predicted probabilities of each estimate" — but this is never operationalised.

**What should have been done:** Draw multiple samples from the softmax distribution to generate multiple plausible trajectories. Pool downstream analyses (e.g., time at home) using Rubin's rules.

---

## Code Quality Issues

### Critical
- **Hardcoded absolute paths** (`/Users/boazsobrado/Desktop/...`) in `mapBuilding.R`, `Clusters.R`, etc. — unusable by anyone else.
- **Scope bugs:** `expDist.R` references undefined variable `t`; `resultsBarnett&Onnella.R` line 119 references non-existent `mobmat2`.
- **Missing function definitions:** `getTestTrain()` and `getModelInput()` are called but never defined in the repository.

### Moderate
- **Hardcoded geography:** UTM zone (EPSG:32631) and home coordinates are Netherlands-specific. Comment in `latlon2UTMFunc.R`: "MISSING: read into what UTM zone to use."
- **Duplicate code:** `gridFunc` defined identically in `gridify.R` and `rastering.R`. Neural network model definition repeated 3 times (5-min, 1-hr, 1-day).
- **No unit tests.** `testBinning.R` and `testIanEuclidianDist.R` are exploratory scripts, not tests.
- **No error handling:** Only 2 guard clauses in the entire codebase. Empty clusters, NA values, and insufficient data for DTW are not handled.
- **Global environment pollution:** All functions loaded via `sapply(file.sources, source, .GlobalEnv)` with no namespace management.

---

## Methods That Were Available But Not Used

1. **Hidden Markov Models** — a natural fit for state-based mobility (home/work/transit) with probabilistic transitions. Well-established by 2017.
2. **Kalman filtering** — actually prototyped in `thesisProposalPlot.R` with a working implementation, but abandoned. Would have provided principled uncertainty quantification.
3. **Gaussian processes / kriging** — standard spatial interpolation methods that provide posterior distributions, not point estimates.
4. **Recurrent neural networks** — attempted (LSTM) but abandoned. Would capture longer temporal dependencies than the feedforward network.
5. **Spatial autocorrelation analysis** — Moran's I, variograms — standard in geostatistics, not used.

---

## Summary Assessment

| Aspect | Rating | Notes |
|--------|--------|-------|
| Core idea | Strong | Personalised spatial maps are a genuine contribution |
| Literature review | Adequate | 58 references; misses Rubin, general missing data theory |
| Statistical rigour | Weak | No formal tests, no CIs, no power analysis |
| Evaluation design | Moderate | Three approaches, each with significant limitations |
| Sample size | Critical | N=1 for evaluation; data from 3 participants underutilised |
| Parameter justification | Weak | Visual inspection; no sensitivity analysis |
| ML methodology | Weak | Arbitrary architecture, no tuning, overconfident |
| Comparison fairness | Weak | Methods designed for different data types |
| Code quality | Below average | Hardcoded paths, scope bugs, no tests |
| Reproducibility | Low | Data unavailable (justified); code has missing functions and bugs |

The thesis presents an interesting methodological idea with engineering that is adequate for a masters thesis, but the statistical evaluation is too thin to support the claims made. The N=1 evaluation, absence of formal tests, single train/test split, and unjustified parameters are the main gaps. The data from all 3 participants should have been used. Several promising approaches (Kalman filter, LSTM, HMMs) were explored in scripts but abandoned without being integrated.
