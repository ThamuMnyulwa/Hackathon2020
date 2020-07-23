from geopy.geocoders import Nominatim
import requests 

address = "120 Grassington Road, Nottingham"
geolocator =Nominatim(user_agent="my-application")
location = geolocator.geocode(address, timeout=10, exactly_one=False)
print(location)