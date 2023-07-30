#Akanksha Khamkar
#Part B:
# Locating the cleaned and joined data in my current directory is called final_data1_cleaned.csv
ls final_cleaneddataset.csv

#Counting the number of unique locations
awk -F',' '{print $1}' final_cleaneddataset.csv | sort | uniq -c

#sorting the countries in the descending order 
awk -F',' '{print $1}' final_cleaneddataset.csv | sort -nr


#displaying the unique countries in the required order.
awk -F',' '{print $1}' final_cleaneddataset.csv | sort -k2 -f country_desc_order.txt - | sort -nr


##Task 2:

# Read the CSV file and extract the first column which is location using awk
awk -F',' 'NR>1 {print $1}' final_cleaneddataset.csv | sort | uniq > country_desc_order.txt

# Loop through the unique countries in the file
while read -r country; do
    # Count the total number of cases for each country using awk
    total_cases=$(awk -F',' -v country="$country" '$1 == country {sum += $5} END {print sum}' final_cleaneddataset.csv)
    
    # Print the country and its total number of cases
    echo "$country: $total_cases"
done < country_desc_order.txt

# Remove the temporary file
rm country_desc_order.txt

#These numbers represent the total count of new COVID-19 cases reported for each country in the dataset. 
#It indicates the scale of the pandemic's impact in each country. 
#Countries like France, Italy, and Spain have reported a significantly higher number of cases compared to Australia and Iran.
#This information can be valuable for understanding the burden of COVID-19 in different regions and assessing the effectiveness of containment measures implemented by each country.



