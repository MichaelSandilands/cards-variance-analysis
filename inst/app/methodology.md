## Purpose

This app reproduces the pre-submission checks applied to card payments
statistics before they are filed with the regulator. It answers two questions
for every reporting period:

1. **Is each movement explainable?** (variance analysis)
2. **Is the return internally and externally consistent?** (validation rules)

Anything that fails either test lands in the **exceptions log**, which is
exported with a blank commentary column for the reporting team to complete
before sign-off.

## Data

Central Bank of Ireland card statistics, October 2022 to August 2026:

| Return | Content | Series |
|---|---|---|
| MPCI | Cards in issue: debit and credit | 2 |
| MPCP | Card payments and ATM withdrawals by Irish-issued cards, by terminal location (domestic / abroad) and channel (remote / non-remote) | 24 |
| MPCY | Card payments, contactless, NFC and ATM withdrawals by county | 216 |

Each return arrives in a slightly different layout (column names, date
formats). All three are harmonised into one tidy schema so a single engine
runs across them. Suppressed cells (confidentiality flag `C`) are kept as
`NA` and reported as *Not assessed*, never silently dropped.

## Variance tests

Every series is padded to a complete monthly grid, then:

| Test | How | Why |
|---|---|---|
| Raw MoM | `timetk::tk_augment_lags()` | Context only. December always jumps, so this never escalates on its own. |
| YoY growth shift | YoY growth less its trailing 3-month average | Steady structural growth (mobile wallets +30% a year) is not an error. A sudden change in that growth is. This also catches base effects a year after a break. |
| Seasonally adjusted MoM | STL decomposition (log scale, i.e. multiplicative) | Strips out the seasonal pattern so the remaining move is the one that needs explaining. The December peak grows with spending, so the decomposition is multiplicative. |
| Rolling z-score | MoM change standardised against the previous 12 months (`timetk::slidify_vec()`), excluding the current month | Measures how unusual this move is for this series. |
| STL anomaly | `timetk::anomalize()`: IQR on the STL remainder | An independent, model-based outlier flag with an expected range. |

A **materiality floor** (absolute value per measure) stops tiny series from
flagging on large percentages. A minimum move stops very smooth series from
flagging on statistically "unusual" but trivial changes.

**Status**: *Breach* = two or more independent tests fail; *Review* = one
fails; *Pass* = none fail. A raw MoM move with every other test passing is
labelled *Seasonal*.

## Value bridge

A value movement splits exactly into a volume effect and an average-ticket
effect:

ΔValue = (N₁ − N₀) × ATV₀ + (ATV₁ − ATV₀) × N₁

This tells you whether a spike came from more transactions or from bigger
ones, which is usually the first question in a variance query.

## Validation rules

| ID | Rule |
|---|---|
| VR01 | MPCP: all locations = domestic + abroad |
| VR02 | MPCP: all channels = remote + non-remote |
| VR03 | MPCY: counties sum to no more than the national total; a gap with no suppressed county is an unallocated residual |
| VR04 | MPCY: NFC ≤ contactless ≤ all card payments, per county |
| VR05 | Cross-return: MPCY national total = MPCP domestic |
| VR06 | Average transaction value within a plausible band and stable year on year |
| VR07 | Completeness: every series every period; blanks only where flagged confidential; no duplicates |
| VR08 | No negative values |
| VR09 | Card payments per card in issue (MPCP ÷ MPCI) within band and stable year on year |

## Findings in the bundled data

* **March 2023 break**: debit cards in issue rose 28% and card payment
  volumes 37% in the same month. Payments per card rose only 12%, close to
  the usual February-to-March seasonal rise (+7% in 2024). Most of the jump
  is therefore explained by a larger card base. That points to a change in
  the reporting population rather than a reporting error.
* **Base effect, March 2024**: one year on, YoY growth in MPCP volumes drops
  from a ~40% trend to ~12%. The YoY-shift test flags it, and the commentary
  is "base effect of the March 2023 population change".
* **Average ticket, late 2023**: county ATVs run 15–25% below the prior year
  (VR06). This is a mix effect consistent with the new reporters' lower-value
  transactions, and it drops out once the comparison base includes them.
* **August 2026, Laois**: card payments, contactless and NFC all jump 23–56%
  month on month with no matching move nationally. This is the kind of
  movement to query with the reporting agent (e.g. a merchant
  re-classification or a terminal relocated across a county boundary).
* **VR05**: MPCY and MPCP domestic totals differ by a few thousandths of a
  percent in most months, which looks like revision timing between the
  returns. October 2022 differs by about 0.09% and fails the default
  tolerance.
