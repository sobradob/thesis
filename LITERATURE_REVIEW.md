# Literature Review: Post-2017 Advances in GPS Trajectory Imputation

Survey of publications from 2017–2025 relevant to the PPMI thesis on handling missing smartphone GPS data.

---

## 1. Trajectory Imputation Methods

The field has moved from simple interpolation to sophisticated deep learning approaches.

**Key papers:**

- **Barnett & Onnela (2020)** — "Inferring Mobility Measures from GPS Traces with Missing Data," *Biostatistics* 21(2). The formal publication of the comparison method used in the PPMI thesis. Introduced weighted resampling of observed GPS data, achieving 10x error reduction over linear interpolation on GeoLife. [Link](https://academic.oup.com/biostatistics/article-abstract/21/2/e98/5145908)

- **Liu & Onnela (2021)** — "Bidirectional Imputation of Spatial GPS Trajectories with Missingness Using Sparse Online Gaussian Process," *JAMIA*. Directly extends Barnett & Onnela with GP-based uncertainty quantification and linear computational complexity. [Link](https://pmc.ncbi.nlm.nih.gov/articles/PMC8324229/)

- **Hwang, Webber-Ritchey & Moxley (2022)** — "Comparison of GPS Imputation Methods in Environmental Health Research," *Geospatial Health* 17(2). Proposed conditional path interpolation (CPI), imputing locations as a function of movement state (stop/move). [Link](https://www.geospatialhealth.net/index.php/gh/article/view/1081)

- **TERI (Chen et al., 2024)** — "An Effective Framework for Trajectory Recovery," *VLDB* 17. Does not require oracle recovery positions, using learnable Fourier features for time/distance encoding with contrastive learning. [Link](https://www.vldb.org/pvldb/vol17/p414-chen.pdf)

- **GTI (2023)** — "A Scalable Graph-based Trajectory Imputation," ACM SIGSPATIAL. Exploits cross-trajectory mutual information from aggregated knowledge of all input trajectories — addresses map-free scenarios similar to PPMI. [Link](https://dl.acm.org/doi/10.1145/3589132.3625620)

- **Xu et al. (2023)** — "Uncovering the Missing Pattern: Unified Framework Towards Trajectory Imputation and Prediction," CVPR 2023. Multi-Space Graph Neural Network with Temporal Decay for simultaneous imputation and prediction. [Link](https://openaccess.thecvf.com/content/CVPR2023/papers/Xu_Uncovering_the_Missing_Pattern_Unified_Framework_Towards_Trajectory_Imputation_and_CVPR_2023_paper.pdf)

- **TrajImpute (2024)** — First dedicated benchmark dataset for evaluating imputation-aware trajectory prediction. [Link](https://arxiv.org/html/2411.00174)

**Trend:** RNN/encoder-decoder (2020) → GAN-based (2023) → graph neural networks (2023–2024) → diffusion models with uncertainty quantification (2025).

---

## 2. Deep Learning for Trajectory Prediction

Transformers and foundation models have reshaped mobility prediction.

**Key papers:**

- **DeepMove (Feng et al., 2018)** — Multi-modal embedding with recurrent layers and historical attention for capturing regular mobility patterns. Early influential work.

- **GeoFormer (2023)** — Decoder-only transformer (GPT-style) for human mobility forecasting, top-3 at HuMob Challenge 2023. [Link](https://dl.acm.org/doi/10.1145/3615894.3628499)

- **UniMob (2024)** — "A Universal Model for Human Mobility Prediction." Multi-view mobility tokenizer with diffusion transformer, 14–25% improvement in noisy/scarce data scenarios. [Link](https://arxiv.org/html/2412.15294v1)

- **State Space Models + Transformers (2024)** — Mamba state space model encoder with transformer decoder for multi-city prediction at HuMob Challenge 2024. [Link](https://dl.acm.org/doi/10.1145/3681771.3699912)

- **Pre-trained Transformer for Mobility (2024)** — Pre-trained on country-scale unlabeled mobility data, up to 38% boost on downstream tasks. [Link](https://arxiv.org/html/2406.04029v2)

- **LLM-Mob (2024)** — Large Language Models for mobility prediction via context-inclusive prompts. [Link](https://arxiv.org/html/2404.16921v1)

**Important finding:** HuMob Challenge 2024 showed transformers are not universally superior — simple pattern-based approaches (same time on previous days/weeks) can be competitive due to strong temporal autocorrelation in mobility. [Link](https://dl.acm.org/doi/10.1145/3681771.3700130)

**Benchmark:** The **HuMob Challenge** (2023, 2024) using **YJMob100K** (100K individuals, 75–90 days, 500m grid, Yahoo Japan) is now the standard benchmark. [Link](https://www.nature.com/articles/s41597-024-03237-9)

---

## 3. Map Matching Advances

- **Transformer-based Map Matching (2024)** — Encoder-decoder transformer as surrogate for rule-based map matching, 75% accuracy on Manhattan GPS traces. [Link](https://arxiv.org/html/2404.12460v1)

- **Progressive Chunked Transformer (2025)** — Trajectory reconstruction from sparse/noisy GPS data, addressing autoregressive decoder inefficiency for long trajectories. [Link](https://www.sciencedirect.com/science/article/pii/S277242472500040X)

- **Enhanced IVMM (2025)** — Interactive Voting-Based Map Matching incorporating OpenStreetMap for missing road network data. [Link](https://arxiv.org/html/2508.11235)

- **TRMMA (2025)** — 75.9x faster than RNTrajRec for 1000-trajectory recovery. [Link](https://arxiv.org/abs/2508.10460)

**Trend:** Classical HMM/Kalman filter → deep learning approaches (transformers, Seq2Seq) that learn noise patterns directly from data.

---

## 4. Human Mobility Modeling

- **Predictability at Scale (Nature Scientific Reports, 2021)** — Spatio-temporal resolution and processing methods significantly affect entropy and predictability. Theoretical maximum predictability: 88–93%. [Link](https://www.nature.com/articles/s41598-021-94102-x)

- **Pattern Matching for Prediction Accuracy (EPJ Data Science, 2022)** — Five pattern-matching measures explain over 88% of variability in prediction accuracy. [Link](https://epjdatascience.springeropen.com/articles/10.1140/epjds/s13688-022-00356-4)

- **CANOE (2025)** — "Beyond Regularity: Modeling Chaotic Mobility Patterns." Addresses dynamic imbalance between periodic and chaotic patterns, 3–13% improvement over SOTA. [Link](https://arxiv.org/abs/2509.11713)

- **Federated Learning for Privacy-Aware Mobility (Frontiers in AI, 2022)** — Mobility modeling without centralizing sensitive location data. [Link](https://www.frontiersin.org/journals/artificial-intelligence/articles/10.3389/frai.2022.867046/full)

- **Comprehensive Review (J. Computational Social Science, 2025)** — "A Review of Human Mobility: Linking Data, Models, and Real-World Applications." [Link](https://link.springer.com/article/10.1007/s42001-025-00414-7)

**Relevance to PPMI:** The thesis's core insight — that human mobility is highly regular and personalised — has been strongly validated (88–93% theoretical predictability). However, newer work shows regularity-focused models alone are insufficient for chaotic/non-routine patterns.

---

## 5. Missing Data in Mobile Sensing

- **Kiang et al. (2021)** — "Sociodemographic Characteristics of Missing Data in Digital Phenotyping." 211 participants, 29,500 person-days. GPS non-collection did not differ by demographics, but increased 0.5–0.9% per week. [Link](https://pmc.ncbi.nlm.nih.gov/articles/PMC8322366/)

- **Android vs. iOS Data Quality (2024)** — GPS had the highest missing data ratio, especially iOS (Median MDR = 0.99 vs. much lower on Android). Platform differences create massive data quality variation. [Link](https://pmc.ncbi.nlm.nih.gov/articles/PMC11478693/)

- **Muller et al. (2022)** — "Analyzing GPS Data for Psychological Research: A Tutorial," *AMPPS*. Practical R-based guide for cleaning GPS data, computing mobility features, and handling missing data. [Link](https://journals.sagepub.com/doi/10.1177/25152459221082680)

- **Systematic Review of GPS Best Practices (2024)** — 157 manuscripts: only 12.1% reported GPS data lost to signal loss, only 15.7% reported noise levels. Massive inconsistency in missing data handling. [Link](https://pmc.ncbi.nlm.nih.gov/articles/PMC10836389/)

- **Digital Phenotyping for Mental Health (2023)** — Systematic review of 29 studies. GPS was the most frequently used sensor (91/112 studies). Depression correlates: regularity of movement (r = −0.63), location variance (r = −0.58), time at home (r = 0.49). [Link](https://www.jmir.org/2023/1/e46778/)

**Relevance to PPMI:** The thesis identified missing GPS data as a fundamental challenge. The 2024 systematic review confirmed most studies still fail to adequately handle it, and iOS data quality (MDR ~0.99) makes imputation methods even more critical.

---

## 6. Spatial Imputation (Kriging, Gaussian Processes)

- **Multi-Task GP for Mobile Data (2024)** — Generalizable multi-task GP framework imputing latitude and longitude simultaneously with uncertainty quantification. *Transportation Research Part C*. [Link](https://www.sciencedirect.com/science/article/abs/pii/S0968090X24000445)

- **Spatio-Temporal DeepKriging (2023)** — DNN-based model combining spatial basis function embeddings with LSTM/ConvLSTM, addressing non-Gaussianity limitations of traditional kriging. [Link](https://www.sciencedirect.com/science/article/abs/pii/S2211675323000489)

- **KITS: Inductive Spatio-Temporal Kriging (2024)** — Enables inductive kriging without re-training for new locations. [Link](https://arxiv.org/html/2311.02565v2)

- **Bayesian Kernelized Matrix Factorization (IEEE, 2022)** — GP priors for latent factors enabling kriging at unseen locations. [Link](https://ieeexplore.ieee.org/abstract/document/9745749)

**Trend:** Classical GP/kriging hybridised with deep learning (DNNs, LSTMs, GNNs). Uncertainty quantification preserved while overcoming scalability limitations.

---

## 7. Multiple Imputation for Spatial/Trajectory Data

This remains the **least developed** area — an open research problem.

- **DTW-Based Multiple Imputation** — Reduces trajectories to time series and selects imputation candidates based on DTW distance. Works for longer gaps.

- **CNN-LSTM for Long-term Gaps (2022)** — Multi-scale residual CNN-stacked LSTM for missing time-activity data, 84% accuracy on 180 individuals' mobile phone data. [Link](https://www.sciencedirect.com/science/article/abs/pii/S0198971522000679)

- **Longitudinal Imputation Comparison (BMC, 2023)** — Compared 27 missing data approaches including MI methods. [Link](https://link.springer.com/article/10.1186/s12874-023-01968-8)

**Gap:** Formally extending Rubin's multiple imputation framework to trajectory data — with proper variance estimation across imputed datasets — remains unsolved. Most methods use bespoke approaches rather than formal MI. The thesis's identification of this limitation was prescient.

---

## 8. Trajectory Foundation Models (2023–2025)

A major new development not anticipated in the 2017 thesis:

- **TrajBERT (2023)** — BERT-based trajectory recovery with spatial-temporal refinement, *IEEE Transactions on Mobile Computing*.

- **UniTraj (2024)** — Universal trajectory foundation model with worldwide dataset, pre-trained via masked trajectory modeling. [Link](https://arxiv.org/html/2411.03859v2)

- **PTR (2024)** — Pre-trained Language Model for Trajectory Recovery, adapting LLMs for free-space and map-matched recovery. [Link](https://arxiv.org/html/2410.14281v1)

- **Building a Foundation Model for Trajectory from Scratch (2025)** — Pedagogical bridge between general-purpose LLMs and domain-specific trajectory models. [Link](https://arxiv.org/html/2511.20610v1)

---

## 9. Synthetic Trajectory Generation

- **SynMob (NeurIPS 2023)** — Synthetic GPS trajectories via diffusion models. [Link](https://openreview.net/forum?id=oz4AGs0phP)
- **ControlTraj (KDD 2024)** — Controllable generation with topology-constrained diffusion. [Link](https://arxiv.org/html/2404.15380v1)
- **GeoLife+ (2024)** — Large-scale simulated trajectories calibrated to GeoLife, enabling benchmarking without privacy constraints. [Link](https://arxiv.org/html/2410.11853v1)

---

## 10. Key Survey Papers

- **Luca et al. (2022)** — "A Survey on Deep Learning for Human Mobility," *ACM Computing Surveys*. Most comprehensive survey, classifying predictive and generative tasks. [Link](https://dl.acm.org/doi/abs/10.1145/3485125)
- **MobilityDL (GeoInformatica, 2024)** — Deep learning from trajectory data by use case, architecture, and data granularity. [Link](https://link.springer.com/article/10.1007/s10707-024-00518-8)
- **Trajectory Generative Models Survey (GeoInformatica, 2025)** — VAEs, GANs, and diffusion models for trajectory generation. [Link](https://link.springer.com/article/10.1007/s10707-025-00558-8)

---

## How the Thesis's Limitations Have Been Addressed

| Thesis Limitation | Status (2025) |
|---|---|
| Single-subject evaluation | Foundation models (UniTraj, TrajBERT) enable cross-user and cross-region transfer learning |
| No uncertainty quantification | GP-based methods (Liu & Onnela 2021) and diffusion models provide calibrated uncertainty |
| Small-scale evaluation | HuMob Challenge provides standardised 100K-user benchmarks; GeoLife+ offers scalable synthetic data |
| Simple feedforward neural network | Transformers, GNNs, state-space models, and LLM-based approaches now standard |
| Map-agnostic but limited | GTI (2023) validates map-free imputation; transformer matching learns from data |
| No formal multiple imputation | **Still an open problem** — most methods use bespoke approaches rather than Rubin's MI |
| Platform-specific data quality | Well-documented (iOS MDR ~0.99) but not solved |
| Regularity assumption only | CANOE (2025) explicitly models chaotic/non-routine mobility patterns |
| No cross-trajectory learning | Graph-based methods (GTI, IGNNK) exploit cross-trajectory mutual information |

---

## Implications for Updating PPMI

If revisiting the PPMI approach today, the most impactful improvements would be:

1. **Replace the feedforward NN with a transformer or state-space model** — captures longer temporal dependencies and benefits from pre-training on large mobility corpora.
2. **Add uncertainty quantification** — either via GP-based imputation (Liu & Onnela 2021) or by generating multiple trajectory samples from a diffusion model.
3. **Use the HuMob Challenge / YJMob100K** as a standardised benchmark for multi-user evaluation.
4. **Leverage cross-trajectory information** — the personalised map idea could be enhanced with graph-based methods that share information across users with overlapping routes.
5. **Address the iOS data quality problem** — iOS MDR of ~0.99 makes the thesis's problem statement even more relevant, but requires methods robust to extreme sparsity.
