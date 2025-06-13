from measure_dim import ComputerVision
import os

cv = ComputerVision()

cwd = os.getcwd()
files = os.listdir(cwd)

# images = []
# meta = []
print("################################## Start Script ##################################")
for f in files:
    if f.lower().endswith('tif'):
        image_name = os.path.splitext(f)[0]
        print("##################################################################################")
        print("## Starting measures for %s" % image_name)
        xml_file = image_name + ".tif_meta.xml"
        if xml_file in files:
            pixels = cv.utils.get_pixels(xml_file)
            print("Get pixels: %s" % pixels)
            data = cv.measure_object_dimension(f, xml_file=xml_file, unit = 'um')
            print(data)

    # 	images.append(f)
    # 	for i in images:
    # 	  image = i
    # 	  cv.measure_object_dimension(image, coin_diameter = 100, unit = 'um')
    #
    # if f.lower().endswith('xml'):
    # 	meta.append(f)
    # 	for m in meta:
    # 		file = m
    # 		cv.utils.get_pixels(file)






#192.97
#88.55
#13158.83
#465
