#ifndef LINCONGAUSSR_ACTIVE_INTERSECTION_H
#define LINCONGAUSSR_ACTIVE_INTERSECTION_H

#include "ellipse.h"
#include "linear_constraints.h"

namespace linconGaussR{
class rotationAngle{
    public:
        double rotation_angle;
        arma::vec shifted_angle;
        rotationAngle() = default;
        rotationAngle(double rot, arma::vec shifted){
            rotation_angle = rot;
            shifted_angle = shifted;
        }
};


class ActiveIntersections{
    public:
        Ellipse ellipse;
        LinearConstraints lincon;
        int N_constraints;
        bool ellipse_in_domain;
        ActiveIntersections() = default;
        ActiveIntersections(Ellipse elpse, LinearConstraints lcon){
            ellipse = elpse;
            lincon = lcon;
            N_constraints = lincon.b.n_rows;
            ellipse_in_domain = true;
        }
        inline arma::Col<int> _index_active(const arma::vec &t, double dt);
        inline arma::vec intersection_angles();
        inline arma::vec find_active_intersections();
        inline rotationAngle rotated_intersections();
};

inline arma::Col<int> ActiveIntersections::_index_active(const arma::vec &t, double dt){
    arma::Col<int> idx(t.n_elem);
    arma::Mat<int> temp;
    for(int i = 0; i < t.n_elem; i++){
        temp = lincon.integration_domain(ellipse.x(t(i)+dt));
        temp -= lincon.integration_domain(ellipse.x(t(i)-dt));
        idx(i) = temp(0);
    }
    return(idx);
}

inline arma::vec ActiveIntersections::intersection_angles(){
    arma::vec g1 = lincon.A * ellipse.a1;
    
    arma::vec g2 = lincon.A * ellipse.a2;
    
    arma::vec r = arma::sqrt(g1 % g1 + g2 % g2);
    arma::vec phi = 2 * arma::atan( g2 / (r + g1)); 
    arma::vec arg = - (lincon.b / r);
    
    arma::mat theta(N_constraints,2,fill::zeros);
    arma::uvec nointer = arma::find(abs(arg)>1);
    arg(nointer) += datum::nan;// write NaNs if there is no intersection
    
    theta.col(0) = arma::acos(arg) + phi;
    theta.col(1) = -arma::acos(arg) + phi;
    
    arma::vec res = vectorise(theta);
    res = res(find_finite(res));
    res(arma::find(res<0.)) += 2. * datum::pi;
    
    return(sort(res));
}


inline arma::vec ActiveIntersections::find_active_intersections(){
    double delta_theta = 1.e-10 * 2.* datum::pi;
    arma::vec theta = this->intersection_angles();
    arma::Col<int> active_directions = this->_index_active(theta, delta_theta);
    arma::vec theta_active = theta(find(active_directions!=0));
    
    
    while(theta_active.n_elem % 2 == 1){
        delta_theta *= 1.0e-1;
        active_directions = this->_index_active(theta, delta_theta);
        theta_active = theta(find(active_directions!=0));
    }
    if(theta_active.n_elem==0){
        theta_active = {{0},{2*datum::pi}};
        
        if(as_scalar(lincon.integration_domain(ellipse.x(2 * datum::pi * arma::randu<double>())))==0){
            ellipse_in_domain = false;
        }
    }
    else{
        arma::Col<int> temp = active_directions(arma::find(active_directions!=0));
        if( temp(0) == -1){
            arma::vec temp2(1);
            temp2(0) = theta_active(0);
            
            theta_active = arma::join_cols(theta_active,temp2); 
            theta_active.shed_row(0);

        }
    }
    
    return(theta_active);

}

inline rotationAngle ActiveIntersections::rotated_intersections(){
    
    arma::vec slices = this->find_active_intersections();
    double rotation_angle = slices(0);
    slices -= rotation_angle;
    slices(find(slices<0.)) += 2. * datum::pi;
    rotationAngle res(rotation_angle, slices);
    return(res);
}

}

#endif
