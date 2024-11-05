# ANALYSIS OF FINANCIAL PERFORMANCE OF FINANCIAL SECTOR COMPANIES FOR OPTIMAL PORTFOLIO FORMATION USING FUZZY C-MEANS, FUZZY GUSTAFSON KESSEL AND MARKOWITZ METHODS

## Project Objective
According to data from the Indonesia Central Securities Depository (KSEI), the number of investors in the capital market throughout 2022 reached 10.31 million people, marking a 37.68% increase compared to the 2021 figure of 7.49 million and a staggering 536.42% increase in the last five years since 2018. Out of the 10.31 million, 4.43 million were investors in the stock market, indicating a high interest in stocks. Stocks are closely related to the principle of high risk, high return. Investors in this instrument need appropriate strategies and steps to maximize profits. One strategy to reduce investment risk is portfolio formation. In this study, companies were grouped based on their financial ratios using the Fuzzy C-Means and Fuzzy Gustafson Kessel methods. Subsequently, portfolio formation was carried out for companies in the financial sector.

## Dataset and Src
- <a href="https://github.com/Nathasyarnt/Financial-Analysis-for-Optimal-Portfolio-Formation-using-Fuzzy-and-Markowitz/tree/main/Data">Dataset</a>
- <a href="https://github.com/Nathasyarnt/Financial-Analysis-for-Optimal-Portfolio-Formation-using-Fuzzy-and-Markowitz/tree/main/Src">Src</a>

## Process
- Calculate the financial ratios of financial companies based on their respective sub-sectors.
- Identify the characteristics of the financial ratio data of the financial sector companies.
- Eliminate outlier data and perform data standardization.
- Group selected companies based on financial ratios obtained from the 2022 financial statements using the Fuzzy C-Means and Fuzzy Gustafson-Kessel methods.
- Calculate the return for each company in each sub-sector.
- Eliminate companies with returns lower than the risk-free rate.
- Analyze using the Markowitz method.
- Evaluate the performance of the portfolio that has been formed using the Markowitz method.
- Interpret the results.
- Draw conclusions and provide recommendations.

## Project Result
- Each sub-sector has different outlier data and variable units, in the banking sub-sector outlier data was found in the companies AMAR, BACA, BANK, BBKP, BBNI, BBSI, BBYB, BEKS, BGTG, BMRI, BRIS, BSWD, BTPN, BTPS, DNAR and SMRA, in the insurance sub-sector outlier data was found in the companies BHAT, JMAS, MTWI, PNIN and PNLF, in the leasing sub-sector outlier data was found in the companies DEFI and MFIN, in the investment service sub-sector outlier data was found in the companies ANOR, APIC, BPII, CASA, DNET, LPPS, NICK, OCAP, PANS, SMMA, SRTG, STAR, VICO and YULE so it is necessary to remove outliers and standardize data.
- The selection of the best cluster method resulted in the Fuzzy C-Means method as the best method with the optimum number of clusters for each company, namely 2 clusters.
- The formation of an optimal portfolio in finance obtained an optimal return of 15.00% with a risk level of 18.67%, with a distribution of funds of 7.41% from BBCA company, 28.78% from BBMD company, 6.80% from BBRI company, 1.30% from BDMN company, 7.23% from BNGA company, 12.94 from NISP company, 1.34% from PNBN company, 2.25% from ABDA company, 7.24% from AMAG company, 1.03% from LPGI company, 19.23% from ADMF company, 3.33% from BBLD company, 0.22% from WOMF company and 0.89% from RELI company. The portfolio of financial sector companies has good performance with a sharpe ratio value of 58.29%.
