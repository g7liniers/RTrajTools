d.harvesing = function(z1,z2) {
  # Harvesing distance between two points in the sphere.
  # z1 = (lat,lon); z2 = (lat,lon)
  # r = 6356.78  # Radio de la tierra en los polos
  r = 6371
  pi = 3.14159265358979323846
  p =  pi/180
  difLat = (z1[1] - z2[1])*p
  difLong = (z1[2] - z2[2])*p
  h = (1 - cos(difLat))/2 + cos(z1[1]*p)*cos(z2[1]*p)*(1 - cos(difLong))/2
  d = 2*r*asin(sqrt(h))
  return(d)
}