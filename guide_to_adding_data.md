# 🌿 Adding Data Using GitHub Branches

This guide explains how to safely **add new data** to the MFHC dashboard using GitHub branches.  
Following this process ensures the `main` branch and live dashboard remain **stable** until your changes are reviewed and approved.

---

## 🧭 Overview

When you create a new **branch**, you are working in a **private copy** of the project.  
You can add new data, update scripts, and test your code without affecting the `main` branch.

Your changes will only go live after you create a **Pull Request (PR)** and it is **approved and merged**.

---

## 🧰 Step-by-Step Workflow

### 1. Clone the Repository
If you haven’t already, clone the MFHC repository:

```bash
git clone https://github.com/NSF-ALL-SPICE-Alliance/MFHC.git
cd MFHC
```

If you don’t have collaborator access, fork the repository first on GitHub and then clone **your fork**.

---

### 2. Create a New Branch
Before adding new data, create your own branch.  
Each branch should describe the change you’re making:

```bash
git checkout main
git pull origin main
git checkout -b feature-add-waterquality
```

💡 Example branch names:
- `maya-add-tempdata`
- `johnny-update-forloop`
- `feature-new-sensor-data`

You’re now working in your **own sandbox** — safe to make edits!

---

### 3. Download New Data 📥
Download the latest dataset(s) and drag them into the **`raw_data/`** folder of the project.

Example:
```
MFHC/
 ├─ raw_data/
 │   ├─ new_sensor_2025.csv
 │   └─ temp_readings_jan2025.csv
```

---

### 4. Update the For Loop 🔄
Open the file:
```
scripts/for_loop_temperature.Rmd
```

Add your new dataset to:
- The **trimming section**, where data is cleaned and standardized
- The **join data section**, where individual datasets are combined

Follow the same format used for the existing data code chunks.

---

### 5. Run the Code 🚀
Run all the code in `for_loop_temperature.Rmd`.

This will create a new file:
```
cleaned_data/temp_joined.csv
```

✅ This file combines all temperature data, including the new one you added.

---

### 6. Commit & Push 💾➡️🌐
Save and commit your changes:

```bash
git add raw_data/ cleaned_data/temp_joined.csv scripts/for_loop_temperature.Rmd
git commit -m "Added new temperature dataset and updated for loop"
git push origin feature-add-waterquality
```

Your branch and all its files are now on GitHub — but **still separate** from the `main` branch.

---

### 7. Generate the Master Data 📊
Run the R Markdown file:
```
scripts/joining_data.Rmd
```

This will generate an updated master file:
```
cleaned_data/master_datapivot.csv
```

Commit and push it:
```bash
git add cleaned_data/master_datapivot.csv
git commit -m "Generated new master_datapivot.csv after adding new data"
git push origin feature-add-waterquality
```

---

### 8. Create a Pull Request 🪄
Now that your branch has the new data and scripts:
1. Go to the [MFHC GitHub Repo](https://github.com/NSF-ALL-SPICE-Alliance/MFHC)
2. Click **Compare & pull request**
3. Add a short description of what you changed (e.g., “Added new temperature sensor data for Jan 2025”)
4. Assign **@connorflynn** as a reviewer
5. Click **Create pull request**

Your edits will be reviewed and merged once approved.

---

## 🔒 Safety Reminder

| Action | Affects `main` branch? | Description |
|--------|------------------------|--------------|
| Edit files on your feature branch | ❌ No | Safe sandbox |
| Commit and push to your branch | ❌ No | Changes stored under your branch |
| Open Pull Request | ⚠️ No (proposal only) | Waiting for review |
| Merge Pull Request | ✅ Yes | Approved changes become part of `main` |

Until the Pull Request is merged, **the live app remains untouched**.

---

## 🧹 Clean Up
After your pull request is merged, you can safely delete your local branch:

```bash
git branch -d feature-add-waterquality
git checkout main
git pull origin main
```

---

### 🎉 Summary

✅ Work safely in your own branch  
✅ Add and process data using R Markdown scripts  
✅ Commit and push your work  
✅ Open a Pull Request for review  
✅ Merge only after approval  

You’re contributing like a pro — mahalo for helping improve the MFHC dashboard! 🌊
