from cv_utilities import Utilities as utils
import cv2
import os
from PIL import Image
import pandas as pd


class ComputerVision:
    def __init__(self):
        self.utils = utils()

    def measure_object_dimension(self, image, scale = None, reference_scale = None, unit = 'um', save = False, path = None,  resize_width=0, rotate_angle=0, blur=(1, 9), cannyMin=50, cannyMax=0, edge_iterations=1):
        firstImage = 0
        utils = self.utils


        #load the image, convert it to grayscale, and blur it slightly - review
        resized, blurred, area, filename, contours = utils.optimize_image(image, resize_width, rotate_angle, blur)

        # step I.2: perform edge detection, then perform a dilation + erotion to close gaps in between object edges
        edge = utils.detect_edge(blurred, cannyMin, cannyMax)

        # step I.3: find and sort objects (sort from left-to-right)
        # objs = utils.detect_and_sort_objects(edge)
        objs = contours

        # II. LOOP OVER THE OBJECTS IDENTIFIED
        data = []

        for idx, obj in enumerate(objs):
            # step II.1: compute the bounding box of the object and draw the box (rectangle)
            box, resized = utils.create_bounding_box(resized, obj)

            # step II.2: mark the corners of the box
            utils.mark_corners(box, resized)

            # step II.3: compute the midpoints and mark them
            tltrX, tltrY, blbrX, blbrY, tlblX, tlblY, trbrX, trbrY = utils.get_midpoints(box, resized)

            # step II.4: compute the Euclidean distance between the midpoints
            dA, dB = utils.get_distances(
                tltrX, tltrY, blbrX, blbrY, tlblX, tlblY, trbrX, trbrY)

            # step II.5: perform the calibration pixel to millimeters if the pixels per metric has not been initialized
            if scale is None: scale = reference_scale / dB # metric / pixel
            
            if dA * scale > 50 and dB * scale > 50:
                diamA, diamB = utils.get_dimensions(dA, dB, scale, resized, unit, tltrX, tltrY, trbrX, trbrY)
                
                # Get the filename only from the initial file path.
                file = os.path.basename(filename)
                # split text to get filename and ext separated
                (file, ext) = os.path.splitext(file) 
                
                if save:
                    name = file + ".png"
                    if path is None: 
                        cv2.imwrite(name, resized)
                    else:
                        n = path + name
                        cv2.imwrite(n, resized)
                        
                
                
                #name = file + ".png"
                #cv2.imwrite(name, resized)

                data.append({'filename': file, 'diamA': diamA, 'diamB': diamB, 'area': area[idx]})


        df = pd.DataFrame(data)
        df.area = df.area * (scale ** 2)
        return df
        
