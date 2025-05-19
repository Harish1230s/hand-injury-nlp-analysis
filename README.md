# Innovate Y – Hand Injury Risk Analysis in Manufacturing

This project was developed as part of the SEG track in DataWhiz 2025. It aims to identify root causes and trends in hand-related injuries in manufacturing using structured OSHA data and advanced NLP techniques on incident narratives.

## 🚀 Objective

Analyze OSHA's Severe Injury Report dataset to:
- Detect patterns in hand injuries using structured and unstructured fields
- Apply NLP (Topic Modeling with LDA) to classify incident narratives
- Categorize injury severity and source mechanisms
- Build a logistic regression model to predict hospitalization risk
- Present insights through a Power BI dashboard

---

## 🧪 Techniques Used

- **Data Cleaning**: UTF-8 encoding, duplicate filtering, standardization
- **Reverse Geocoding**: Auto-filled missing city values using coordinates
- **Categorical Engineering**:
  - `injurycategory` (e.g., Amputation, Fracture, Laceration)
  - `severitylevel` (High, Medium, Low)
  - `sourcecategory` (e.g., Hand Tools, Machinery, Chemicals)
- **Topic Modeling**: LDA on Final Narratives → 5 Dominant Themes:
  - Caught in Press
  - Lathe Entanglement
  - Chemical Exposure
  - Tool Handling
  - Blade/Sharp Object Incidents
- **Predictive Modeling**: Logistic regression to predict `hospitalized`

---

## 📊 Key Insights

- 58.6% of hand injuries were amputations.
- Press brakes, band saws, and conveyors were the most common sources.
- Injuries peaked mid-week (Tuesdays and Wednesdays).
- States with high temporary workforce (like TX) had higher injury counts.
- NLP helped identify hidden risks not captured by structured fields.

---

## 📈 Visualization

Power BI dashboard includes:
- Geo-map of injury density by city
- Ribbon chart of injury types by state
- Trendline of injuries by year
- Bar chart of top employers with frequent incidents

---

## 👥 Team

- Harish Kumar Sarathi  
- Debasmita Ray  
- Soundariyan Venkatachelam  
- Avantika Patrina Ananth

---

## 📌 Tools Used

- R, data.table, tidygeocoder, topicmodels, tm
- Power BI
- OSHA Severe Injury Dataset
