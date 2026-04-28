# OPTIVAL: Model-Based Optimal Test Validity Analysis

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)]()
<!-- badges: end -->

## Overview

**OPTIVAL** is an R package that implements a model-based approach for determining optimal item subsets that maximize the predictive validity of test scores with respect to external variables. Unlike empirical approaches that may capitalize on chance, OPTIVAL uses a principled methodology based on Item Response Theory (IRT) and linear factor analysis.

The package is particularly useful for:
- **Scale development and refinement**: Identifying the most informative items for predicting external criteria
- **Test shortening**: Reducing test length while maintaining or maximizing predictive validity
- **Validity optimization**: Systematically improving the external validity of non-cognitive measures

## Key Features

- **Model-based item selection**: Uses Confirmatory Factor Analysis (CFA) calibration rather than purely empirical correlations
- **Bootstrap validation**: Includes bootstrap procedures to avoid capitalization on chance
- **Incremental validity analysis**: Determines the contribution of each item to overall test validity
- **Interactive Shiny application**: User-friendly web interface for non-R users
- **Comprehensive outputs**: Provides detailed results including validity curves, optimal subsets, and model fit indices

## Methodology

### Theoretical Foundation

The approach is based on an extended congeneric measurement model that includes both test items and an external criterion variable. The key assumptions are:

1. **Model-based calibration**: Items are first calibrated using a unidimensional linear factor-analytic model treated as a linear IRT model for continuous responses
2. **Local independence**: Item specificities and random errors are uncorrelated
3. **Structural validity**: The external variable relates to items through the common latent construct

From this framework, OPTIVAL derives:
- Model-implied item-criterion correlations
- Expected validity as a function of test length
- Conditions for incremental validity
- Optimal item ordering and subset selection

### Key Advantages

- **Generalizability**: Results are expected to generalize better to new samples and external variables
- **Stability**: Minimizes capitalization on chance through model-based predictions
- **Interpretability**: Provides clear theoretical rationale for item selection decisions
- **Validation**: Includes bootstrap procedures to empirically verify model predictions

## Installation

You can install OPTIVAL directly from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install OPTIVAL
devtools::install_github("dnavarrogonzalez/OPTIVAL")
```

## Quick Start

### Using the Shiny Application (Recommended for most users)

The easiest way to use OPTIVAL is through the interactive Shiny application:

```r
library(OPTIVAL)
run_optival()
```

This will launch a web browser with a user-friendly interface where you can:
1. Upload your data file (CSV, TXT, or other delimited formats)
2. Optionally provide pre-calibrated factor loadings
3. Configure bootstrap parameters
4. View comprehensive results including plots and tables

### Using the R Function Directly

For programmatic use or integration into analysis pipelines:

```r
library(OPTIVAL)

# Load your data
# Columns 1 to n-1: test items
# Column n: external criterion variable
data <- read.csv("your_data.csv")

# Run OPTIVAL analysis
results <- OPTIVAL(
  data_matrix = as.matrix(data),
  precalibrated_loadings = NULL,  # NULL = estimate from data
  n_bootstrap = 2000,             # Number of bootstrap replications
  verbose = TRUE                  # Print progress messages
)

# View results
print(results$optimal_n_items)
print(results$optimal_items)
print(results$max_observed_validity)

# Access detailed output
str(results)
```

### Example with Simulated Data

```r
# Simulate data: 20 items + 1 criterion variable, 500 subjects
set.seed(123)
n_subjects <- 500
n_items <- 20

# Generate latent construct
theta <- rnorm(n_subjects)

# Generate items with varying loadings
loadings <- seq(0.5, 0.9, length.out = n_items)
items <- sapply(loadings, function(lambda) {
  lambda * theta + sqrt(1 - lambda^2) * rnorm(n_subjects)
})

# Generate criterion variable
criterion_loading <- 0.85
criterion <- criterion_loading * theta + sqrt(1 - criterion_loading^2) * rnorm(n_subjects)

# Combine data
data_matrix <- cbind(items, criterion)

# Run OPTIVAL
results <- OPTIVAL(
  data_matrix = data_matrix,
  n_bootstrap = 2000
)

# View optimal subset
cat("Optimal number of items:", results$optimal_n_items, "\n")
cat("Selected items:", paste(results$optimal_items, collapse = ", "), "\n")
cat("Observed validity:", round(results$max_observed_validity, 4), "\n")
cat("Theoretical ceiling:", round(results$criterion_loading, 4), "\n")
```

## Understanding the Output

The `OPTIVAL()` function returns a list with the following components:

### Essential Results

- `optimal_n_items`: Suggested optimal number of items
- `optimal_items`: Vector of selected item indices
- `item_order`: Recommended ordering of items by incremental validity
- `max_observed_validity`: Maximum observed validity coefficient (bootstrap-based)

### Validity Indices

- `item_loadings`: Factor loadings for each item
- `criterion_loading`: Factor loading for the external criterion (theoretical validity ceiling)
- `validity_indices`: Item-criterion correlations (bootstrap-based)
- `incremental_validity`: Incremental contribution of each item to test validity
- `expected_validity`: Model-predicted validity for each test length
- `observed_validity`: Bootstrap-estimated validity for each test length

### Model Fit (when CFA is used)

- `model_fit`: Named vector with fit indices (χ², CFI, TLI, RMSEA, SRMR)

### Additional Information

- `n_items`: Number of items analyzed
- `n_subjects`: Sample size
- `n_bootstrap`: Number of bootstrap replications
- `validity_correlation`: Correlation between item loadings and item validities

## Use Cases

### 1. Scale Development

When developing a new scale, OPTIVAL helps identify which items contribute most to external validity:

```r
# Full item pool with criterion
results <- OPTIVAL(full_item_pool)

# Select optimal subset for final scale
final_items <- results$optimal_items
```

### 2. Test Shortening

When you need to create a shorter version of an existing test:

```r
# Analyze full test
results <- OPTIVAL(long_test_data)

# Determine how many items can be removed
cat("Can reduce from", results$n_items, "to", results$optimal_n_items, "items\n")
cat("Maintains", round(100 * results$observed_validity[results$optimal_n_items] / 
                      results$observed_validity[results$n_items], 1), 
    "% of full test validity\n")
```

### 3. Validity Optimization

Compare different item sets or orderings:

```r
# Try different configurations
result1 <- OPTIVAL(data_matrix)
result2 <- OPTIVAL(data_matrix, precalibrated_loadings = custom_loadings)

# Compare effectiveness
comparison <- data.frame(
  Configuration = c("Default", "Custom"),
  Optimal_Items = c(result1$optimal_n_items, result2$optimal_n_items),
  Max_Validity = c(result1$max_observed_validity, result2$max_observed_validity)
)
print(comparison)
```

## Web Application Access

In addition to the R package, OPTIVAL is also available as a web application hosted at:

**http://psicor.fcep.urv.cat/miapp/OPTIVAL/**

This allows use without installing R or any packages. Simply upload your data file through the web interface.

## Data Format

### Input Data Requirements

1. **File format**: CSV, TXT, or other delimited text files
2. **Structure**: 
   - Each row represents one subject/respondent
   - Columns 1 to (n-1): Test item scores
   - Column n: External criterion variable
3. **Data type**: Continuous or reasonably continuous responses
4. **Missing data**: Should be handled before analysis
5. **Scaling**: Can be on any scale (data is standardized internally)

### Example Data Format

```
Item1,Item2,Item3,Item4,Item5,Criterion
4.2,3.8,4.5,3.9,4.1,75.3
3.5,3.2,3.7,3.4,3.6,68.9
4.8,4.6,4.9,4.7,4.5,82.1
...
```

### Pre-calibrated Loadings (Optional)

If you have pre-calibrated factor loadings from a previous analysis, you can provide them as:

1. A separate file with one loading per line
2. Must include ALL loadings: items + criterion (last value)
3. Loadings should be between 0 and 1

Example loadings file:
```
0.75
0.68
0.82
0.71
0.79
0.85
```

## Frequently Asked Questions

**Q: How many items do I need for OPTIVAL to work?**
A: Minimum 3-4 items, but results are more reliable with 5+ items. The method works best with 10-30 items.

**Q: Can I use ordinal/Likert-type items?**
A: Yes. While the method assumes continuous responses, it works reasonably well with Likert scales (5+ categories).

**Q: What if my data doesn't fit the model well?**
A: Check the model fit indices in the output. Poor fit suggests the unidimensional assumption may not hold. Consider using a more appropriate measurement model or reassessing your item pool.

**Q: How do I choose the number of bootstrap replications?**
A: Default is 2000, which provides stable estimates. Use 1000 for faster (but less precise) results, or 5000+ for publication-quality precision.

**Q: Can I use multiple external criteria?**
A: Currently, OPTIVAL handles one external criterion at a time. For multiple criteria, run separate analyses or consider multivariate extensions (future work).

**Q: What if items have negative loadings?**
A: Reverse-score items before analysis. All loadings should be positive for the unidimensional model.

## Citation

Pending.

## Contributing

Contributions are welcome! Please feel free to:

- Report bugs or issues
- Suggest enhancements
- Submit pull requests

For major changes, please open an issue first to discuss what you would like to change.

## License

This package is licensed under the GNU General Public License v3.0 (GPL-3). See [LICENSE](LICENSE) file for details.

## Contact

- **David Navarro-González**: david.navarro@urv.cat
- **GitHub Issues**: [https://github.com/dnavarrogonzalez/OPTIVAL/issues](https://github.com/dnavarrogonzalez/OPTIVAL/issues)
