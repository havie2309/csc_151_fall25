# Bitcoin Trends During the Pandemic

Authors: A’aishah, Hoang, Duc, Vy  
Course: CSC-151-04  
Date: November 26, 2025 

## Project overview

This project analyzes Bitcoin (BTC) price behavior during the COVID-19 pandemic. We define the pandemic analysis window as February 1, 2020 through August 31, 2021. Using the "Bitcoin Historical Data" dataset (Kaggle), we aggregate the raw second-level price data into daily records and answer three main questions:

- When was the largest two-week (14-day) change in BTC price?
- What are the three lowest and three highest opening prices (by day) during the pandemic window?
- What was the overall rate of change of BTC value across the pandemic timeframe?

## Dataset

Source: Zielak, "Bitcoin Historical Data" (Kaggle)  
Kaggle page: https://www.kaggle.com/datasets/mczielinski/bitcoin-historical-data

Notes:
- The raw dataset contains second-level entries (open, high, low, close) for many years. We limit analysis to the specified pandemic window to keep processing tractable.
- We aggregate second-level records into daily entries and define the "open" for a day as the first price of that calendar day and "close" as the last price of that day.

## Methods

1. Data preparation
   - Filter dataset to the date range 2020-02-01 through 2021-08-31.
   - Remove corrupted or incomplete rows.
   - Aggregate second-level entries into one record per day (date, open, close, high, low, volume).
   - Format date strings as `MM-DD-YYYY`.

2. Finding extremes (3 highest / 3 lowest opens)
   - Sort daily entries by `open` value using a comparator that treats the entry as a financial record.
   - Output the top 3 and bottom 3 entries.

3. Two-week maximum change
   - Implement a sliding-window / recursive procedure across the daily list to find the maximum difference between a day and the day exactly 14 days later (index offset = 13).
   - The algorithm generalizes to other window sizes.

4. Overall rate of change
   - Compute rate-of-change across the pandemic window by comparing the open at the start and the close at the end of the window, and optionally by accumulating daily percent changes to estimate cumulative movement.

## Dependencies & setup

Recommended Python environment:
- Python 3.8+
- Create and activate a virtual environment:
  - python -m venv venv
  - source venv/bin/activate  (macOS / Linux)
  - venv\Scripts\activate     (Windows)
- Install required packages:
  - pip install -r requirements.txt
  - Typical packages: pandas, numpy, matplotlib, seaborn, jupyter

If you don't have a requirements.txt yet, a minimal set:
- pandas
- numpy
- matplotlib
- seaborn
- jupyter

## How to run

Example CLI usage (adjust paths and script names to match repo code):

1. Clean & aggregate raw data
   - python src/clean_and_aggregate.py --input data/bitcoin_raw.csv --output data/bitcoin_daily.csv --start 2020-02-01 --end 2021-08-31

2. Compute top/bottom opens
   - python src/find_extremes.py --input data/bitcoin_daily.csv --top_out outputs/top3_highest_open.csv --bottom_out outputs/top3_lowest_open.csv

3. Compute two-week maximum change and overall rate
   - python src/rate_of_change.py --input data/bitcoin_daily.csv --window 14 --report_out outputs/two_week_max_change.csv --overall_out outputs/overall_rate.csv

4. Generate plots (example)
   - python src/plot_results.py --input data/bitcoin_daily.csv --out_dir outputs/figures

(If you use notebooks, open `notebooks/analysis.ipynb` and run the cells after installing dependencies.)

## Expected outputs

- CSVs:
  - outputs/top3_highest_open.csv
  - outputs/top3_lowest_open.csv
  - outputs/two_week_max_change.csv
  - outputs/overall_rate.csv

- Figures:
  - outputs/figures/price_over_time.png
  - outputs/figures/two_week_change_histogram.png


## License & contact

- License: Add your preferred license (e.g., MIT) or state course-specific licensing.
- Contact: For questions about the analysis, contact the project authors (A’aishah, Hoang, Duc, Vy).