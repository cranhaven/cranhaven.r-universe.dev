#ifndef SRC_POINTCLOUD_LIDRPOINTCLOUD_H_
#define SRC_POINTCLOUD_LIDRPOINTCLOUD_H_

#include <RcppArmadillo.h>
#include "AbstractPointCloud.h"
#include "../Vec3d.h"

class LidrPointCloud : public AbstractPointCloud {
private:
    Rcpp::NumericMatrix coords;
    Rcpp::NumericMatrix normals;
    size_t currentIndex;
    Vec3d minBounds, maxBounds;
    unsigned int nPoints;

public:
    LidrPointCloud(Rcpp::NumericMatrix coords_, Rcpp::NumericMatrix normals_) 
        : coords(coords_), normals(normals_), currentIndex(0), nPoints(coords_.nrow()) {
        
        // Calculate bounding box
        std::vector<double> min_vec(3, std::numeric_limits<double>::max());
        std::vector<double> max_vec(3, std::numeric_limits<double>::lowest());
        
        for (unsigned int i = 0; i < nPoints; i++) {
            for (int j = 0; j < 3; j++) {
                min_vec[j] = std::min(min_vec[j], coords(i,j));
                max_vec[j] = std::max(max_vec[j], coords(i,j));
            }
        }
        
        mMin = min_vec;
        mMax = max_vec;
        minBounds = Vec3d(min_vec[0], min_vec[1], min_vec[2]);
        maxBounds = Vec3d(max_vec[0], max_vec[1], max_vec[2]);
        mNumPoints = nPoints;
    }

    virtual ~LidrPointCloud() {}

    virtual void resetCursor() override {
        currentIndex = 0;
    }

    virtual bool endOfCloud() override {
        return currentIndex >= nPoints;
    }

    virtual std::vector<double> getNextPoint() override {
        std::vector<double> point(6);
        // XYZ coordinates
        for (int i = 0; i < 3; i++) {
            point[i] = coords(currentIndex, i);
        }
        // Normal vectors
        for (int i = 0; i < 3; i++) {
            point[i + 3] = normals(currentIndex, i);
        }
        currentIndex++;
        return point;
    }

    virtual unsigned int getNumPoints() override {
        return nPoints;
    }
};

#endif /* SRC_POINTCLOUD_LIDRPOINTCLOUD_H_ */
