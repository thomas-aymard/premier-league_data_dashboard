# ⚽ Dashboard Analytique : Statistiques & ACP de la Premier League (2006-2018)

Application web interactive développée en **R (Shiny / Bslib)** dans le cadre du BUT Science des Données (IUT Paris Rives de Seine). Ce projet analyse en profondeur 12 saisons de Premier League à travers 4 560 matchs historiques.

🔗 **Accès direct au Live Demo :** [Tester l'application en ligne](https://reportingr.shinyapps.io/reporting/)

---

### 📊 Fonctionnalités du Dashboard
* **Présentation & Contexte :** Introduction détaillée de la base de données, des variables et de la structure de l'application.
* **Statistiques & Analyses univariées / bivariées (`Plotly`) :** Exploration dynamique des performances des clubs (buts, points, victoires, défaites) avec classements, boxplots de régularité et nuages de points corrélés.
* **Évolution Temporelle :** Suivi comparatif des trajectoires des clubs phares au fil des saisons.
* **Avantage du Terrain (Dom / Ext) :** Analyse comparative pour mesurer l'impact réel du douzième homme à domicile par rapport aux performances à l'extérieur.
* **Analyse en Composantes Principales (ACP - `FactoMineR`) :** Réalisation d'une ACP multivariée pour cartographier les profils des équipes, les corrélations entre indicateurs et visualiser la variance expliquée.
* **Exploration & Export :** Tableau de données complet et agrégé par saison avec options de téléchargement direct (`CSV`, `Excel`, `Copier`).

---

### 🛠️ Technologies utilisées
* **Langage :** R
* **Framework Web :** Shiny, Bslib (Thème Bootstrap 5 / Flatly - Design personnalisé Premier League)
* **Manipulation & Agrégation :** Tidyverse (`dplyr`, `tidyr`)
* **Statistiques & ACP :** FactoMineR
* **Visualisation interactive :** Plotly, DT (DataTables), ShinyWidgets
