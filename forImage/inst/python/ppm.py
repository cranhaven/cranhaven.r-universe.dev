import os
import numpy as np
import imutils
import xml.etree.ElementTree as ET

class Pixels:
    def __init__(self):
        pass

    def get_pixels_axio(self, xml_file):
        
        tree = ET.parse(xml_file)
        root = tree.getroot()
        for child in root.findall('.//Scaling'):
            x_scale = float((child[3].text).replace(',', '.'))
            y_scale = float((child[8].text).replace(',', '.'))
            if x_scale != y_scale:
                print("X and Y coordinates are not equal. Check xml file")
                break
            else:
                name = os.path.splitext(xml_file)[0]
                name = os.path.splitext(name)[0]
                return x_scale

    #def get_pixels_zen(self, image_file):
    #def get_pixels_leica(self, image_file):
    #def get_pixels_olympus(self, image_file):
