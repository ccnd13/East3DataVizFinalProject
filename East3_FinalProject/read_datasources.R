library(tidyverse)

#CSV Files
phone_log<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/311_Phone_Call_Log_Mod.csv'
df_311_phone_log<-read.csv(phone_log)

contact_mgmt<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/311_Contact_Management_Cases.csv'
df_311_contact_mgmt<-read.csv(contact_mgmt)

bus_license<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Business_Licenses_geocoded.csv'
df_bus_license<-read.csv(bus_license)

park_locations<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Parks_Locations_and_Features.csv'
df_park_locations<-read.csv(park_locations)

public_facilities<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Public_Facilities.csv'
df_public_facilities<-read.csv(public_facilities)

street_lights<-'https://raw.githubusercontent.com/ccnd13/East3DataVizFinalProject/main/FinalProject/Street_Lights.csv'
df_street_lights<-read.csv(street_lights)

