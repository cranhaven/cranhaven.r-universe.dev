from __future__ import print_function, division
from builtins import input
import numpy as np
import cv2
import imutils


class Utilities:
    def __init__(self):
        pass

    def optimize_image(self, filename, resize_width, rotate_angle, blur):
        image = cv2.imread(filename)
        #scale_percent = 40  # percent of original size
        #width = int(image.shape[1] * scale_percent / 100)
        #height = int(image.shape[0] * scale_percent / 100)
        #dim = (width, height)
        # resize image
        #image = cv2.resize(image, dim, interpolation=cv2.INTER_AREA)
        image = imutils.rotate(image, angle=rotate_angle)
        shifted = cv2.pyrMeanShiftFiltering(image, 31, 41)
        gray = cv2.cvtColor(shifted, cv2.COLOR_BGR2GRAY)
        gray = cv2.GaussianBlur(gray, (5,5), 0)
        ret, thresh = cv2.threshold(gray, 45, 245, cv2.THRESH_BINARY)
        kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (5,5))
        morphed = cv2.morphologyEx(thresh, cv2.MORPH_OPEN, kernel)
        #thresh = cv2.erode(morphed, None, iterations=2)
        thresh = cv2.dilate(morphed, None, iterations=2)
        contours, hierarchy = cv2.findContours(thresh, cv2.RETR_TREE, cv2.CHAIN_APPROX_NONE)

        contour_list = []
        area = []
        #perimeter = []
        for contour in contours:
            #p = (cv2.arcLength(contour, True) / 2.5)
            a = (cv2.contourArea(contour))
            if a > 5000:
                contour_list.append(contour)
                area.append(a)
                #perimeter.append(p)


        cv2.drawContours(image, contour_list, -1, (0, 255, 0), 3)
        return image, gray, area, filename, contour_list

    def detect_edge(self, image, cannyMin, cannyMax):
        edged = cv2.Canny(image, cannyMin, cannyMax)
        edged = cv2.dilate(edged, None, iterations=1)
        edged = cv2.erode(edged, None, iterations=1)
        return edged

    def detect_and_sort_objects(self, image):
        from imutils import contours

        cnts = cv2.findContours(image.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        cnts = cnts[0] if imutils.is_cv2() else cnts[1]
        (cnts, _) = contours.sort_contours(cnts)
        return cnts

    def create_bounding_box(self, image, target_object, draw=True):
        from imutils import perspective

        orig = image.copy()
        box = cv2.minAreaRect(target_object)
        box = cv2.cv.BoxPoints(box) if imutils.is_cv2() else cv2.boxPoints(box)
        box = np.array(box, dtype='int')

        '''
    order the points in the object such that they appear in top-left, top-right,
    bottom-right, and bottom-left order, then draw the outline of the rotated
    bounding box
    '''
        box = perspective.order_points(box)
        if draw == True:
            cv2.drawContours(orig, [box.astype('int')], -1, (0, 255, 0), 1)
        return box, orig

    def mark_corners(self, box, image):
        for (x, y) in box:
            cv2.circle(image, (int(x), int(y)), 3, (0, 0, 255), -1)

    def get_midpoints(self, box, image, draw=True):
        def midpoint(ptA, ptB):
            return ((ptA[0] + ptB[0]) * 0.5, (ptA[1] + ptB[1]) * 0.5)

        # unpack the ordered bounding box
        (tl, tr, br, bl) = box

        # compute the midpoint between the top-left and top-right, followed by the midpoint between bottom-left and bottom-right
        (tltrX, tltrY) = midpoint(tl, tr)
        (blbrX, blbrY) = midpoint(bl, br)

        # compute the midpoint between the top-left and bottom-left points, followed by the midpoint between the top-right and bottom-right
        (tlblX, tlblY) = midpoint(tl, bl)
        (trbrX, trbrY) = midpoint(tr, br)

        if draw:
            # draw the midpoints on the image
            cv2.circle(image, (int(tltrX), int(tltrY)), 3, (255, 0, 0), -1)
            cv2.circle(image, (int(blbrX), int(blbrY)), 3, (255, 0, 0), -1)
            cv2.circle(image, (int(tlblX), int(tlblY)), 3, (255, 0, 0), -1)
            cv2.circle(image, (int(trbrX), int(trbrY)), 3, (255, 0, 0), -1)

            # draw lines between the midpoints
            cv2.line(image, (int(tltrX), int(tltrY)),
                     (int(blbrX), int(blbrY)), (255, 0, 255), 1)
            cv2.line(image, (int(tlblX), int(tlblY)),
                     (int(trbrX), int(trbrY)), (255, 0, 255), 1)

        return tltrX, tltrY, blbrX, blbrY, tlblX, tlblY, trbrX, trbrY

    def get_distances(self, tltrX, tltrY, blbrX, blbrY, tlblX, tlblY, trbrX, trbrY):
        from scipy.spatial import distance as dist
        dA = dist.euclidean((tltrX, tltrY), (blbrX, blbrY))
        dB = dist.euclidean((tlblX, tlblY), (trbrX, trbrY))
        return dA, dB

    def get_dimensions(self, dA, dB, scale, image, unit, tltrX, tltrY, trbrX, trbrY):
        dimA = dA * scale
        dimB = dB * scale
        # draw the dimensions on the image
        cv2.putText(image, "{:.1f}{}".format(dimA, unit), (int(tltrX - 15), int(tltrY - 10)),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 1)
        cv2.putText(image, "{:.1f}{}".format(dimB, unit), (int(trbrX + 10), int(trbrY)),
                    cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 1)
        return dimA, dimB

