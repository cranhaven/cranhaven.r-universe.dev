import os
import numpy as np
import cv2
import imutils
import pandas as pd

class Protoplasm:
    def __init__(self):
        pass

    def measure(self, file, scale, rotate_angle = 0):
        image = cv2.imread(file)
        #scale_percent = 40  # percent of original size
        #width = int(image.shape[1] * scale_percent / 100)
        #height = int(image.shape[0] * scale_percent / 100)
        #dim = (width, height)
        # resize image
        #image = cv2.resize(image, dim, interpolation=cv2.INTER_AREA)
        image = imutils.rotate(image, rotate_angle)
        original = image.copy()
        image = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)

        #see count colours option
        
        #lower = np.array([150, 130, 0], dtype="uint8")
        #upper = np.array([170, 255, 255], dtype="uint8")
        #lower = np.array([128, 0, 0], dtSype="uint8")
        #upper = np.array([179, 255, 255], dtype="uint8")
        #lower = np.array([155, 25, 0], dtype="uint8")
        #upper = np.array([179, 255, 255], dtype="uint8")
        lower = np.array([150, 110, 0], dtype="uint8")
        upper = np.array([180, 255, 255], dtype="uint8")
        mask = cv2.inRange(image, lower, upper)

        kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3,3))
        opening = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel, iterations=1)
        cnts = cv2.findContours(opening, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        cnts = cnts[0] if len(cnts) == 2 else cnts[1]

        area = 0
        for c in cnts:
            area += (cv2.contourArea(c) * (scale ** 2))
            #area += (cv2.contourArea(c) * 0.1511111111111111)
            cv2.drawContours(original,[c], 0, (0,0,255), 2)

        name = os.path.splitext(file)[0] + "_prot.png"
        cv2.imwrite(name, original)
                
        df = pd.DataFrame({'area_p': [area], 'filename': [file]})
        return df

        




        
