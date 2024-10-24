# Welcome to our Maunalua Fishpond Heritage Center Dashboard and Analysis Repository!

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/79951e4f-d012-47aa-b917-e1aea2179957" alt="SpiceLogo1" width="200" height="75">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/103060ad-2896-45b4-bf2d-06bec3d8119f" alt="mfhc" width="150" height="100">

<img src="https://github.com/NSF-ALL-SPICE-Alliance/MFHC/assets/76076246/a2fc9fd6-ea5a-4e7d-ad61-52f3d0951dd6" alt="cuh_logo" width="125" height="100">


### About

From the [Maunalua Fishpond Heritage Center Website](https://maunaluafishpond.org/): 

"Nearly all of Hawai‘i’s freshwater springs have disappeared – many filled in for development, ending the fresh and salt water connection that is critical to the life cycle of many native species. In Kuli‘ou‘ou, we had a rare opportunity to permanently protect a fertile estuary that is home to rare freshwater limpets pipiwai and hapawai that cling to the Hawaiian drystack wall lining the spring, ‘ama‘ama (mullet) and āholehole (young Hawaiian flagtail) that feed on the prized green limu ‘ele‘ele that blankets the spring floor, native shrimp ‘opae ‘oeha‘a and ‘opae huna, and endangered ‘ae‘o (Hawaiian stilts) that feed in Kānewai Fishpond. This effort also protects the many cultural sites surrounding the spring including the mākāhā (fishpond sluice gate), and kū‘ula (fishing stone shrine) with an upright Kū stone balanced by a low Hina stone where the fishermen of old would have given offerings asking for a plentiful catch."

### Goal of this Work

Clean, Analyze, and share data from Kalauhaʻihaʻi and Kānewai with the MFHC Team and surrounding community

The primary product is a Shiny Dashboard that displays pH, oxygen, conductivity, and temperature data collected by [Dr. Lupita](https://lupita-ruiz-jones.squarespace.com/) and students in the Chaminade University of Honolulu Environmental Science Dept from Kalauhaʻihaʻi and Kānewai


[Beta Dashboard](https://connorflynn.shinyapps.io/maunalua-fishpond-heritage-center/?_ga=2.191367721.2028241035.1678747718-1779377801.1678413849#section-kalauhaihai)

**Students**

[Anson Ekau](https://github.com/aekau21)

[Brandon Kioske](https://github.com/BrandonKoskie)

[Hina Ioane](https://github.com/Hinaioane)

**Mentor**

[Connor Flynn](https://github.com/ConnorFlynn)


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

Add the code to the trimming section of the for_loop_temperature.Rmd file.

Next, add the code to the join data section of the for_loop_temperature.Rmd file.

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
