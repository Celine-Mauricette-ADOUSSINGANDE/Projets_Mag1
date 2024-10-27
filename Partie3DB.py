
from pymongo import MongoClient

# Connecting to the MongoDB database
client = MongoClient('localhost', 27017)
db = client.hospital_cicb
patients_collection = db.patients


# Query 1: Display female patients with medical records
patients_F = patients_collection.find({"Sex": "F", "Medical_History": "Yes"})
print("Female patients with medical records are:")
for patient in patients_F:
    print(patient)


# Query 2: View Patient Diagnosed with Diabetes
patients_diabetes = patients_collection.find({"Diagnosis": "Diabetes"})
print("\nPatients with a Diabetes Diagnosis:")
for patient in patients_diabetes:
    print(patient)


# Query 3: View patients over 60 years of age
patients_age = patients_collection.find({"Age": {"$gt": "60"}})
print("\nPatients over 60 years of age:")
for patient in patients_age:
    print(patient)


# Query 4: View Male patients under 70 years old with an open medical record
patients_M = patients_collection.find({"Sex": "M",
                                       "Age": {"$lt": "70"},
                                       "Medical_History": "Yes",
                                       "File_Status": "Open"})
print("Male patient under the age of 70 with an open medical record is:")
for patient in patients_M:
    print("Patient Name:", patient["Name"])
    print("Patient Gender:", patient["Sex"])
    print("The age of the patient:", patient["Age"])
    print("Patient Medical_History:", patient["Medical_History"])
    print("Patient File_Status:", patient["File_Status"])


# Query 5:Display the name, gender and address of the patient
# who is in Emergency 103 in room 33
patient = patients_collection.find_one({"ID_EMERGENCY": "103",
                                        "ID_ROOMS": "33"})
if patient:
    print("Patient Name:", patient["Name"])
    print("Patient Gender:", patient["Sex"])
    print("Patient addresst:", patient["Address"])
else:
    print("No patients were found with these identifiers.")


# Closing the database connection
client.close()
