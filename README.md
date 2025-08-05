# Welcome to the Maunalua Analytics for Stewardship and Tracking (MAST) Repository!

![MAST](https://github.com/user-attachments/assets/118080b7-4e40-4109-858a-43452b18286e)


<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/79951e4f-d012-47aa-b917-e1aea2179957" alt="SpiceLogo1" width="200" height="75">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/103060ad-2896-45b4-bf2d-06bec3d8119f" alt="mfhc" width="150" height="100">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/a2fc9fd6-ea5a-4e7d-ad61-52f3d0951dd6" alt="cuh_logo" width="125" height="100">





https://github.com/user-attachments/assets/ae21a8a5-0164-4902-b299-3c96ec0170e6





## 🌺 Project Overview

This project is a collaboration between **Chris Cramer**, founder and Executive Director of the [Maunalua Fishpond Heritage Center](https://maunaluafishpond.org/), and **Dr. Lupita Ruiz Jones**, [Assistant Professor of Environmental Science at Chaminade University](https://lupita-ruiz-jones.squarespace.com/).

### 👩🏽‍🔬 Student Researchers

Under the mentorship of Dr. Lupita Ruiz Jones and supported by the [Louis Stokes Alliances for Minority Participation (LSAMP)](https://www.nsf.gov/funding/opportunities/lsamp-louis-stokes-alliances-minority-participation):

- Hina Ioane  
- Kate Dugger

Under the mentorship of [Connor Flynn](https://connorflynn.github.io/) and supported by the [NSF INCLUDES Alliance Supporting Pacific Impact through Computational Excellence (All-SPICE)](https://www.nsfspicealliance.org/):

- Anson Ekau  
- Brandon Koskie

All data and code is publicly available via the project’s [GitHub repository](https://github.com/NSF-ALL-SPICE-Alliance/MFHC).

---




![anson_gif](https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/ec7c1917-672c-4071-b319-eeb4f2ffade8)



<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/1a3799c5-3501-4702-bf05-ef731d2ad1f7" alt="team_bridge" width="300" height="250">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/c4a99b0e-744f-41fd-b0be-c818e528cf23" alt="chris_teaching" width="300" height="250">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/86409f9e-81f9-4188-b52d-4d6e2e0557b7" alt="anson_pres_photo" width="300" height="250">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/e220ca48-b1dd-4448-b363-7d4944c5166c" alt="anson_aag_globe" width="300" height="250">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/ab8a56ff-6241-40f9-b8c3-2b3486398d29" alt="brandon_pres_photo" width="300" height="250">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/b37be1fc-44e8-4887-8c9e-4a60d80adb68" alt="spring" width="300" height="250">



### 🌊 Adding New Data Update

Follow these steps to upload new data to the dashboard and keep everything flowing smoothly. 🐟💧

1. Download New Data 📥

Download the latest data files and drag them into the raw_data folder.

2. Update the For Loop 🔄

Add the code to the [trimming section of the for_loop_temperature.Rmd](https://github.com/NSF-ALL-SPICE-Alliance/MFHC/blob/7a64ecf53d1c734bceba0186795d9604cfadaf45/for_loop_cleaning_scripts/for_loop_temperature.Rmd#L124C2-L124C10) file.

Next, add the code to the [join data section of the for_loop_temperature.Rmd](https://github.com/NSF-ALL-SPICE-Alliance/MFHC/blob/7a64ecf53d1c734bceba0186795d9604cfadaf45/for_loop_cleaning_scripts/for_loop_temperature.Rmd#L301) file.

3. Run the Code 🚀

Run all the code in for_loop_temperature.Rmd.

This will generate a new CSV called temp_joined in the cleaned_data folder.

4. Commit & Push 💾➡️🌐

Commit the new temp_joined file and the updated raw_data to GitHub.

5. Generate the Master Data 📊

Run the joining_data.Rmd to generate master_datapivot.csv in the cleaned_data folder.

Push the new master_datapivot.csv to GitHub.

6. Update the Dashboard 📈✨

Now, users can pull the latest changes and run shiny_script.R to see the updated data on the dashboard! 🎉
