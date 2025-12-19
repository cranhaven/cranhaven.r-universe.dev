#include "adssub.h"
#include "Zlibs.h"
#include <math.h>
#include <R.h>

/************************************************************************/
/*Fonctions de calcul geometriques pour la correction des effets de bord*/
/************************************************************************/

/* a exterieur ; b et c interieur*/
double un_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
    {
    double alpha, beta, gamma, delta, ttt, ang;
    double ex, ey, fx, fy;

    /*premier point d'intersection*/
    alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
    beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
    gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    if (delta<=0)
        Rprintf("erreur1\n");
    ttt=(-beta-sqrt(delta))/(2*alpha);
    if ((ttt<=0)||(ttt>=1))
        Rprintf("erreur2\n");
    ex=ax+ttt*(bx-ax);
    ey=ay+ttt*(by-ay);

    /* deuxieme point d'intersection*/
    alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
    beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
    delta=beta*beta-4*alpha*gamma;
    if (delta<=0)
        Rprintf("erreur3\n");
    ttt=(-beta-sqrt(delta))/(2*alpha);
    if ((ttt<=0)||(ttt>=1))
        Rprintf("erreur4\n");
    fx=ax+ttt*(cx-ax);
    fy=ay+ttt*(cy-ay);

    /*calcul de l'angle*/
    ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
    return ang;
    }

/* a interieur , b et c exterieur*/
double deux_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
    {
    double alpha, beta, gamma, delta, ttt, ang;
    double ex, ey, fx, fy, gx, gy, hx, hy;
    int cas;

    /* premier point d'intersection*/
    alpha=((bx-ax)*(bx-ax)+(by-ay)*(by-ay));
    beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
    gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    if (delta<=0)
        Rprintf("erreur6\n");
    ttt=(-beta+sqrt(delta))/(2*alpha);
    if ((ttt<=0)||(ttt>=1))
        Rprintf("erreur7\n");
    ex=ax+ttt*(bx-ax);
    ey=ay+ttt*(by-ay);

    /* deuxieme point d'intersection*/
    alpha=((cx-ax)*(cx-ax)+(cy-ay)*(cy-ay));
    beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
    delta=beta*beta-4*alpha*gamma;
    if (delta<=0)
        Rprintf("erreur8\n");
    ttt=(-beta+sqrt(delta))/(2*alpha);
    if ((ttt<=0)||(ttt>=1))
        Rprintf("erreur9\n");
    fx=ax+ttt*(cx-ax);
    fy=ay+ttt*(cy-ay);

    /* y a t il deux autres intersections?*/
    cas=0;
    alpha=((cx-bx)*(cx-bx)+(cy-by)*(cy-by));
    beta=(2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by));
    gamma=((bx-x)*(bx-x)+(by-y)*(by-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    if (delta>0)
        {
        ttt=(-beta-sqrt(delta))/(2*alpha);
        if ((ttt>=0)&&(ttt<=1))
            {
            gx=bx+ttt*(cx-bx);
            gy=by+ttt*(cy-by);
            ttt=(-beta+sqrt(delta))/(2*alpha);
            if ((ttt>=0)&&(ttt<=1))
                {
                cas=1;
                hx=bx+ttt*(cx-bx);
                hy=by+ttt*(cy-by);
                }
            else
                Rprintf("erreur9bis\n");
            }
        }

    /* calcul de l'angle*/
    if (cas==0)
        ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
    else
        {
        ang=bacos(((ex-x)*(gx-x)+(ey-y)*(gy-y))/(d*d));
        ang+=bacos(((fx-x)*(hx-x)+(fy-y)*(hy-y))/(d*d));
        }

    return ang;
    }

/* a exterieur, b interieur, c sur le bord*/
double ununun_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
    {
    double alpha, beta, gamma, delta, ttt, ang;
    double ex, ey, fx, fy;

    /* premier point d'intersection sur ab*/
    alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
    beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
    gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    if (delta<=0)
        Rprintf("erreur1b\n");
    ttt=(-beta-sqrt(delta))/(2*alpha);
    if ((ttt<=0)||(ttt>=1))
        Rprintf("erreur2b\n");
    ex=ax+ttt*(bx-ax);
    ey=ay+ttt*(by-ay);

    /* deuxieme point d'intersection ac*/
    alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
    beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
    delta=beta*beta-4*alpha*gamma;
    ttt=1;
    if (delta>0)
        {
        ttt=(-beta-sqrt(delta))/(2*alpha);
        if ((ttt<=0)||(ttt>1))
            ttt=1;
        if (ttt<=0)
            Rprintf("e3b\n");
        }
    fx=ax+ttt*(cx-ax);
    fy=ay+ttt*(cy-ay);

    /* calcul de l'angle*/
    ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
    return ang;
    }

/* a,b et c exterieurs*/
double trois_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
    {
    double alpha, beta, gamma, delta, te, tf, tg, th, ti, tj, ang;
    double ex=0, ey=0, fx=0, fy=0, gx=0, gy=0, hx=0, hy=0, ix=0, iy=0, jx=0, jy=0;

    /* premier segment ab*/
    alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
    beta=2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay);
    gamma=(ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d;
    delta=beta*beta-4*alpha*gamma;
    if (delta<0)
        {
        te=-1;
        tf=-1;
        }
    else
        {
        te=(-beta-sqrt(delta))/(2*alpha);
        tf=(-beta+sqrt(delta))/(2*alpha);
        if ((te<0)||(te>=1)||(tf==0))
            {
            te=-1;
            tf=-1;
            }
        else
            {
            ex=ax+te*(bx-ax);
            ey=ay+te*(by-ay);
            fx=ax+tf*(bx-ax);
            fy=ay+tf*(by-ay);
            if ((tf<=0)||(tf>1))
                Rprintf("pb te %f tf %f\n", te, tf);
            }
        }

    /* deuxieme segment bc*/
    alpha=(cx-bx)*(cx-bx)+(cy-by)*(cy-by);
    beta=2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by);
    gamma=(bx-x)*(bx-x)+(by-y)*(by-y)-d*d;
    delta=beta*beta-4*alpha*gamma;
    if (delta<0)
        {
        tg=-1;
        th=-1;
        }
    else
        {
        tg=(-beta-sqrt(delta))/(2*alpha);
        th=(-beta+sqrt(delta))/(2*alpha);
        if ((tg<0)||(tg>=1)||(th==0))
            {
            tg=-1;
            th=-1;
            }
        else
            {
            gx=bx+tg*(cx-bx);
            gy=by+tg*(cy-by);
            hx=bx+th*(cx-bx);
            hy=by+th*(cy-by);
            if ((th<=0)||(th>1))
                Rprintf("pb tg %f th %f\n", tg, th);
            }
        }

    /* troisieme segment ca*/
    alpha=(ax-cx)*(ax-cx)+(ay-cy)*(ay-cy);
    beta=2*(cx-x)*(ax-cx)+2*(cy-y)*(ay-cy);
    gamma=(cx-x)*(cx-x)+(cy-y)*(cy-y)-d*d;
    delta=beta*beta-4*alpha*gamma;
    if (delta<0)
        {
        ti=-1;
        tj=-1;
        }
    else
        {
        ti=(-beta-sqrt(delta))/(2*alpha);
        tj=(-beta+sqrt(delta))/(2*alpha);
        if ((ti<0)||(ti>=1)||(tj==0))
            {
            ti=-1;
            tj=-1;
            }
        else
            {
            ix=cx+ti*(ax-cx);
            iy=cy+ti*(ay-cy);
            jx=cx+tj*(ax-cx);
            jy=cy+tj*(ay-cy);
            if ((tj<=0)||(tj>1))
                Rprintf("pb ti %f tj %f\n", ti, tj);
            }
        }

    /* quelle configuration ?*/
    if (te<0)
        {
        if (tg<0)
            {
            if (ti<0)
                /* pas d'intersection... ouf!*/
                ang=0;
            else
                /* un seul cote (ca) coupe le cercle en i,j*/
                ang=bacos(((ix-x)*(jx-x)+(iy-y)*(jy-y))/(d*d));
            }
        else
            {
            if (ti<0)
                /* un seul cote (bc) coupe le cercle en g,h*/
                ang=bacos(((gx-x)*(hx-x)+(gy-y)*(hy-y))/(d*d));
            else
                { /* deux cotes (bc et ca) coupent le cercle en g,h,i,j*/
                ang=bacos(((gx-x)*(jx-x)+(gy-y)*(jy-y))/(d*d));
                ang+=bacos(((hx-x)*(ix-x)+(hy-y)*(iy-y))/(d*d));
                }
            }
        }
    else
        {
        if (tg<0)
            {
            if (ti<0)
                /* un seul cote (ab) coupe le cercle en e,f*/
                ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
            else
                { /* deux cotes (ab et ca) coupent le cercle en e,f,i,j*/
                ang=bacos(((ex-x)*(jx-x)+(ey-y)*(jy-y))/(d*d));
                ang+=bacos(((fx-x)*(ix-x)+(fy-y)*(iy-y))/(d*d));
                }
            }
        else
            {
            if (ti<0)
                { /* deux cotes (ab et bc) coupent le cercle en e,f,g,h*/
                ang=bacos(((ex-x)*(hx-x)+(ey-y)*(hy-y))/(d*d));
                ang+=bacos(((fx-x)*(gx-x)+(fy-y)*(gy-y))/(d*d));
                }
            else
                { /* les trois cotes coupent le cercle*/
                ang=bacos(((ex-x)*(jx-x)+(ey-y)*(jy-y))/(d*d));
                ang+=bacos(((hx-x)*(ix-x)+(hy-y)*(iy-y))/(d*d));
                ang+=bacos(((fx-x)*(gx-x)+(fy-y)*(gy-y))/(d*d));
                }
            }
        }

    /*if ((ang<0)||(ang>Pi()))*/
    if ((ang<0)||(ang>3.141593))
        Rprintf("erreur12 : ang=%11.10f, %f %f %f %f %f %f\n", ang, te, tf, tg, th, ti, tj);

    return ang;
    }

/* a est le point sur le bord , b et c exterieur*/
double deuxun_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
    {
    double alpha, beta, gamma, delta, te, tf, tg, th, ang;
    double ex, ey, fx, fy, gx, gy, hx, hy;
    int cas;

    /* premier point d'intersection*/
    alpha=((bx-ax)*(bx-ax)+(by-ay)*(by-ay));
    beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
    gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    te=0;
    if (delta>0)
        {
        te=(-beta+sqrt(delta))/(2*alpha);
        if ((te<0)||(te>=1))
            te=0;
        if (te>=1)
            Rprintf("e15\n");
        }
    ex=ax+te*(bx-ax);
    ey=ay+te*(by-ay);

    /* deuxieme point d'intersection*/
    alpha=((cx-ax)*(cx-ax)+(cy-ay)*(cy-ay));
    beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
    delta=beta*beta-4*alpha*gamma;
    tf=0;
    if (delta>0)
        {
        tf=(-beta+sqrt(delta))/(2*alpha);
        if ((tf<0)||(tf>=1))
            tf=0;
        if (tf>=1)
            Rprintf("e15\n");
        }
    fx=ax+tf*(cx-ax);
    fy=ay+tf*(cy-ay);

    /* y a t il deux autres intersections?*/
    cas=0;
    alpha=((cx-bx)*(cx-bx)+(cy-by)*(cy-by));
    beta=(2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by));
    gamma=((bx-x)*(bx-x)+(by-y)*(by-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    if (delta>0)
        {
        tg=(-beta-sqrt(delta))/(2*alpha);
        if ((tg>=0)&&(tg<=1))
            {
            gx=bx+tg*(cx-bx);
            gy=by+tg*(cy-by);
            th=(-beta+sqrt(delta))/(2*alpha);
            if ((th>=0)&&(th<=1))
                {
                cas=1;
                hx=bx+th*(cx-bx);
                hy=by+th*(cy-by);
                }
            else
                Rprintf("erreur9ter\n");
            }
        }

    /* calcul de l'angle*/
    if (cas==0)
        {
        if ((te==0)&&(tf==0))
            ang=0;
        else
            ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
        }
    else
        {
        ang=bacos(((ex-x)*(gx-x)+(ey-y)*(gy-y))/(d*d));
        ang+=bacos(((fx-x)*(hx-x)+(fy-y)*(hy-y))/(d*d));
        }
    return ang;
    }

/* a exterieur, b et c sur le bord*/
double deuxbord_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
    {
    double alpha, beta, gamma, delta, te, tf, ang;
    double ex, ey, fx, fy;

    /* premier point d'intersection sur ab*/
    alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
    beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
    gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
    delta=beta*beta-4*alpha*gamma;
    te=1;
    if (delta>0)
        {
        te=(-beta-sqrt(delta))/(2*alpha);
        if ((te<=0)||(te>=1))
            te=1;
        if (te<=0)
            Rprintf("e1t\n");
        }
    ex=ax+te*(bx-ax);
    ey=ay+te*(by-ay);

    /* deuxieme point d'intersection ac*/
    alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
    beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
    delta=beta*beta-4*alpha*gamma;
    tf=1;
    if (delta>0)
        {
        tf=(-beta-sqrt(delta))/(2*alpha);
        if ((tf<=0)||(tf>=1))
            tf=1;
        if (tf<=0)
            Rprintf("e4t\n");
        }
    fx=ax+tf*(cx-ax);
    fy=ay+tf*(cy-ay);

    /* calcul de l'angle*/
    ang=bacos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
    return ang;
    }

/*retourne 1 si le point x,y est du meme cote de la droite (ab) que c (seg=0) ou sur la droite (seg=1)*/
int in_droite(double x, double y, double ax, double ay, double bx, double by, double cx, double cy, int seg)
    {
    double vabx, vaby, vacx, vacy, vamx, vamy, pv1, pv2;

    vabx=bx-ax;
    vaby=by-ay;
    vacx=cx-ax;
    vacy=cy-ay;
    vamx=x-ax;
    vamy=y-ay;
    pv1=vabx*vacy-vaby*vacx;
    pv2=vabx*vamy-vaby*vamx;

    if (seg==0)
        {
        if (((pv1>0)&&(pv2>0))||((pv1<0)&&(pv2<0))) /*pour overlap*/
            return 1;
        else
            return 0;
        }
    if (seg==1)
        {
        if (((pv1>0)&&(pv2>=0))||((pv1<0)&&(pv2<=0))) /*pour points*/
            return 1;
        else
            return 0;
        }
    return -1;
    }

/*retourne 1 si (x,y) est dans le triangle abc (seg=0) ou sur ses bords (seg=1)*/
int in_triangle(double x, double y, double ax, double ay, double bx, double by, double cx, double cy, int seg)
    {
    int res;

    res=0;
    if (in_droite(x, y, ax, ay, bx, by, cx, cy, seg)==1)
        if (in_droite(x, y, bx, by, cx, cy, ax, ay, seg)==1)
            if (in_droite(x, y, cx, cy, ax, ay, bx, by, seg)==1)
                res=1;
    return res;
    }

/*Range les resultats pour l'ic*/
void ic(int i, int i0, double **gic, double **kic, double *gic1, double *kic1, int nbInt)
    {
    int j, cro;
    double mer;

    /*On stocke les 2i0+1 premieres valeurs en les triant au fur et a mesure*/
    if (i<=2*i0+1)
        {

        for (j=1; j<=nbInt; j++)
            {
            gic[j][i]=gic1[j-1];
            kic[j][i]=kic1[j-1];
            }

        /*De la deuxieme a la 2i0+1 eme valeur : on trie la nouvelle valeur en direct*/
        if (i>1)
            {

            /*Tri bulle de g vers le bas*/
            for (j=1; j<=nbInt; j++)
                {
                if (gic[j][i-1]>gic[j][i])
                    {
                    mer=gic[j][i];
                    cro=i-1;
                    while ((cro>0)&&(gic[j][cro]>mer))
                        {
                        gic[j][cro+1]=gic[j][cro];
                        cro=cro-1;
                        }
                    gic[j][cro+1]=mer;
                    }
                }

            /*Tri bulle de k vers le bas*/
            for (j=1; j<=nbInt; j++)
                {
                if (kic[j][i-1]>kic[j][i])
                    {
                    mer=kic[j][i];
                    cro=i-1;
                    while ((cro>0)&&(kic[j][cro]>mer))
                        {
                        kic[j][cro+1]=kic[j][cro];
                        cro=cro-1;
                        }
                    kic[j][cro+1]=mer;
                    }
                }
            }
        }
    else
        {
        /*On a deja rempli et trie le tableau des 2i0+1 valeurs, on met la nouvelle valeur en i0*/
        for (j=1; j<=nbInt; j++)
            {
            gic[j][i0+1]=gic1[j-1];
            kic[j][i0+1]=kic1[j-1];
            }

        /*On trie les nouvelles valeurs de k et g*/
        for (j=1; j<=nbInt; j++)
            {

            /* si g doit descendre*/
            if (gic[j][i0+1]<gic[j][i0])
                {
                mer=gic[j][i0+1];
                cro=i0;
                while ((cro>0)&&(gic[j][cro]>mer))
                    {
                    gic[j][cro+1]=gic[j][cro];
                    cro=cro-1;
                    }
                gic[j][cro+1]=mer;
                }
                /* si g doit monter*/
            else
                {
                if (gic[j][i0+1]>gic[j][i0+2])
                    {
                    mer=gic[j][i0+1];
                    cro=i0+2;
                    while ((cro<2*i0+2)&&(gic[j][cro]<mer))
                        {
                        gic[j][cro-1]=gic[j][cro];
                        cro=cro+1;
                        }
                    gic[j][cro-1]=mer;
                    }
                }

            /* si k doit descendre*/
            if (kic[j][i0+1]<kic[j][i0])
                {
                mer=kic[j][i0+1];
                cro=i0;
                while ((cro>0)&&(kic[j][cro]>mer))
                    {
                    kic[j][cro+1]=kic[j][cro];
                    cro=cro-1;
                    }
                kic[j][cro+1]=mer;
                }
                /* si k doit monter*/
            else
                {
                if (kic[j][i0+1]>kic[j][i0+2])
                    {
                    mer=kic[j][i0+1];
                    cro=i0+2;
                    while ((cro<2*i0+2)&&(kic[j][cro]<mer))
                        {
                        kic[j][cro-1]=kic[j][cro];
                        cro=cro+1;
                        }
                    kic[j][cro-1]=mer;
                    }
                }
            }
        }
    }

/******************************************************************************/
/* Cette routine donne le perimetre/ddd du cercle centre en (xxx,yyy) et      */
/* de rayon ddd, qui est a l'interieur de la zone rectangulaire xmi xma ymiyma*/
/* Elle traite les cas 1 bord, 2 bords d'angle (2), 2 bords opposes, 3 bords  */
/* Ce resultat correspond a la correction des effets de bord pour Ripley      */

/******************************************************************************/
double perim_in_rect(double xxx, double yyy, double ddd, double xmi, double xma, double ymi, double yma)
    {
    double d1, d2, d3, d4;

    if ((xxx>=xmi+ddd)&&(yyy>=ymi+ddd)&&(xxx<=xma-ddd)&&(yyy<=yma-ddd))
        { /*Rprintf("*");*/
        return 2*Pi();
        }
    else
        {
        d1=(xxx-xmi)/ddd;
        d2=(yyy-ymi)/ddd;
        d3=(xma-xxx)/ddd;
        d4=(yma-yyy)/ddd;
        if (d1>=1)
            {
            if (d2>=1)
                {
                if (d3>=1)
                    {
                    if (d4>=1) /* cercle dans le rectangle */
                        {
                        return 2*Pi();
                        }
                    else /* bord seul en d4 */
                        {
                        return (2*(Pi()-acos(d4)));
                        }
                    }
                else
                    {
                    if (d4>=1) /* bord seul en d3 */
                        {
                        return (2*(Pi()-acos(d3)));
                        }
                    else /* 2 bords d3 et d4 */
                        {
                        if (d3*d3+d4*d4<1)
                            {
                            return (1.5*Pi()-acos(d3)-acos(d4));
                            }
                        else
                            {
                            return (2*(Pi()-acos(d3)-acos(d4)));
                            }
                        }
                    }
                }
            else
                {
                if (d3>=1)
                    {
                    if (d4>=1) /* bord seul en d2 */
                        {
                        return (2*(Pi()-acos(d2)));
                        }
                    else /* 2 bords d2 et d4 */
                        {
                        return (2*(Pi()-acos(d2)-acos(d4)));
                        }
                    }
                else
                    {
                    if (d4>=1) /* 2 bords d2 et d3 */
                        {
                        if (d2*d2+d3*d3<1)
                            {
                            return ((1.5*Pi()-acos(d2)-acos(d3)));
                            }
                        else
                            {
                            return (2*(Pi()-acos(d2)-acos(d3)));
                            }
                        }
                    else /* 3 bords d2,d3,d4 */
                        {
                        if (d2*d2+d3*d3<1)
                            {
                            if (d3*d3+d4*d4<1)
                                {
                                return ((Pi()-acos(d2)-acos(d4)));
                                }
                            else
                                {
                                return ((1.5*Pi()-acos(d2)-acos(d3)-2*acos(d4)));
                                }
                            }
                        else
                            {
                            if (d3*d3+d4*d4<1)
                                {
                                return ((1.5*Pi()-2*acos(d2)-acos(d3)-acos(d4)));
                                }
                            else
                                {
                                return (2*(Pi()-acos(d2)-acos(d3)-acos(d4)));
                                }
                            }
                        }
                    }
                }
            }
        else
            {
            if (d2>=1)
                {
                if (d3>=1)
                    {
                    if (d4>=1) /* bord seul en d1 */
                        {
                        return (2*(Pi()-acos(d1)));
                        }
                    else /* 2 bords d1 et d4 */
                        {
                        if (d1*d1+d4*d4<1)
                            {
                            return ((1.5*Pi()-acos(d1)-acos(d4)));
                            }
                        else
                            {
                            return (2*(Pi()-acos(d1)-acos(d4)));
                            }
                        }
                    }
                else
                    {
                    if (d4>=1) /* 2 bords d1 et d3 */
                        {
                        return (2*(Pi()-acos(d1)-acos(d3)));
                        }
                    else /* 3 bords d1,d3,d4 */
                        {
                        if (d3*d3+d4*d4<1)
                            {
                            if (d4*d4+d1*d1<1)
                                {
                                return ((Pi()-acos(d3)-acos(d1)));
                                }
                            else
                                {
                                return ((1.5*Pi()-acos(d3)-acos(d4)-2*acos(d1)));
                                }
                            }
                        else
                            {
                            if (d4*d4+d1*d1<1)
                                {
                                return ((1.5*Pi()-2*acos(d3)-acos(d4)-acos(d1)));
                                }
                            else
                                {
                                return (2*(Pi()-acos(d3)-acos(d4)-acos(d1)));
                                }
                            }
                        }
                    }
                }
            else
                {
                if (d3>=1)
                    {
                    if (d4>=1) /* 2 bords d1 et d2 */
                        {
                        if (d1*d1+d2*d2<1)
                            {
                            return ((1.5*Pi()-acos(d1)-acos(d2)));
                            }
                        else
                            {
                            return (2*(Pi()-acos(d1)-acos(d2)));
                            }
                        }
                    else /* 3 bords d1,d2,d4 */
                        {
                        if (d4*d4+d1*d1<1)
                            {
                            if (d1*d1+d2*d2<1)
                                {
                                return ((Pi()-acos(d4)-acos(d2)));
                                }
                            else
                                {
                                return ((1.5*Pi()-acos(d4)-acos(d1)-2*acos(d2)));
                                }
                            }
                        else
                            {
                            if (d1*d1+d2*d2<1)
                                {
                                return ((1.5*Pi()-2*acos(d4)-acos(d1)-acos(d2)));
                                }
                            else
                                {
                                return (2*(Pi()-acos(d4)-acos(d1)-acos(d2)));
                                }
                            }
                        }
                    }
                else
                    {
                    if (d4>=1) /* 3 bords d1,d2,d3 */
                        {
                        if (d1*d1+d2*d2<1)
                            {
                            if (d2*d2+d3*d3<1)
                                {
                                return ((Pi()-acos(d1)-acos(d3)));
                                }
                            else
                                {
                                return ((1.5*Pi()-acos(d1)-acos(d2)-2*acos(d3)));
                                }
                            }
                        else
                            {
                            if (d2*d2+d3*d3<1)
                                {
                                return ((1.5*Pi()-2*acos(d1)-acos(d2)-acos(d3)));
                                }
                            else
                                {
                                return (2*(Pi()-acos(d1)-acos(d2)-acos(d3)));
                                }
                            }
                        }
                    else /* 4 bords : je ne peux pas faire */
                        {
                        Rprintf("erreur : le nombre d'intervalles est trop grand\n");
                        return -1;
                        }
                    }
                }
            }
        }
    }

/*pour une zone circulaire definie par x0, y0, r0*/
double perim_in_disq(double xxx, double yyy, double ddd,
                     double x0, double y0, double r0)
    {
    double d1;

    d1=sqrt((xxx-x0)*(xxx-x0)+(yyy-y0)*(yyy-y0));
    if (d1+ddd<=r0)
        return 2*Pi();
    else
        return 2*(Pi()-acos((r0*r0-d1*d1-ddd*ddd)/(2*d1*ddd)));
    }

/* renvoie la somme des angles du perim a l'interieur des triangles*/
double perim_triangle(double x, double y, double d, int triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy)
    {
    double angle, epsilon;
    double doa, dob, doc;
    int h;
    //int i;

    epsilon=0.0001;
    angle=0;


    for (h=0; h<triangle_nb; h++)
        {
        doa=sqrt((x-ax[h])*(x-ax[h])+(y-ay[h])*(y-ay[h]));
        dob=sqrt((x-bx[h])*(x-bx[h])+(y-by[h])*(y-by[h]));
        doc=sqrt((x-cx[h])*(x-cx[h])+(y-cy[h])*(y-cy[h]));

        if (doa-d<-epsilon)
            {
            if (dob-d<-epsilon)
                {
                if (doc-d<-epsilon)
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                else if (doc-d>epsilon)
                    angle+=un_point(cx[h], cy[h], ax[h], ay[h], bx[h], by[h], x, y, d);
                else
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                }
            else if (dob-d>epsilon)
                {
                if (doc-d<-epsilon)
                    angle+=un_point(bx[h], by[h], ax[h], ay[h], cx[h], cy[h], x, y, d);
                else if (doc-d>epsilon)
                    angle+=deux_point(ax[h], ay[h], bx[h], by[h], cx[h], cy[h], x, y, d);
                else
                    angle+=ununun_point(bx[h], by[h], ax[h], ay[h], cx[h], cy[h], x, y, d);
                }
            else /* b sur le bord*/
                {
                if (doc-d<-epsilon)
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                else if (doc-d>epsilon)
                    angle+=ununun_point(cx[h], cy[h], ax[h], ay[h], bx[h], by[h], x, y, d);
                else
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                }
            }
        else if (doa-d>epsilon)
            {
            if (dob-d<-epsilon)
                {
                if (doc-d<-epsilon)
                    angle+=un_point(ax[h], ay[h], bx[h], by[h], cx[h], cy[h], x, y, d);
                else if (doc-d>epsilon)
                    angle+=deux_point(bx[h], by[h], ax[h], ay[h], cx[h], cy[h], x, y, d);
                else
                    angle+=ununun_point(ax[h], ay[h], bx[h], by[h], cx[h], cy[h], x, y, d);
                }
            else if (dob-d>epsilon)
                {
                if (doc-d<-epsilon)
                    angle+=deux_point(cx[h], cy[h], ax[h], ay[h], bx[h], by[h], x, y, d);
                else if (doc-d>epsilon)
                    angle+=trois_point(ax[h], ay[h], bx[h], by[h], cx[h], cy[h], x, y, d);
                else
                    angle+=deuxun_point(cx[h], cy[h], ax[h], ay[h], bx[h], by[h], x, y, d);
                }
            else /* b sur le bord*/
                {
                if (doc-d<-epsilon)
                    angle+=ununun_point(ax[h], ay[h], cx[h], cy[h], bx[h], by[h], x, y, d);
                else if (doc-d>epsilon)
                    angle+=deuxun_point(bx[h], by[h], ax[h], ay[h], cx[h], cy[h], x, y, d);
                else
                    angle+=deuxbord_point(ax[h], ay[h], bx[h], by[h], cx[h], cy[h], x, y, d);
                }
            }
        else /* a sur le bord*/
            {
            if (dob-d<-epsilon)
                {
                if (doc-d<-epsilon)
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                else if (doc-d>epsilon)
                    angle+=ununun_point(cx[h], cy[h], bx[h], by[h], ax[h], ay[h], x, y, d);
                else
                    //i=1
                    ; /* le triangle est	dans le cercle, TVB*/
                }
            else if (dob-d>epsilon)
                {
                if (doc-d<-epsilon)
                    angle+=ununun_point(bx[h], by[h], cx[h], cy[h], ax[h], ay[h], x, y, d);
                else if (doc-d>epsilon)
                    angle+=deuxun_point(ax[h], ay[h], bx[h], by[h], cx[h], cy[h], x, y, d);
                else
                    angle+=deuxbord_point(bx[h], by[h], ax[h], ay[h], cx[h], cy[h], x, y, d);
                }
            else /* b sur le bord*/
                {
                if (doc-d<-epsilon)
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                else if (doc-d>epsilon)
                    angle+=deuxbord_point(cx[h], cy[h], ax[h], ay[h], bx[h], by[h], x, y, d);
                else
                    //i=1
                    ; /* le triangle est dans le cercle, TVB*/
                }
            }
        }

    return angle;
    }






/******************************************************************************/
/* Calcule la fonction de Ripley K(r) pour un semis (x,y) en parametres       */
/* dans une zone de forme rectangulaire de bornes xmi xma ymi yma             */
/* Les corrections des effets de bords sont fait par la methode de Ripley,    */
/* i.e. l'inverse de la proportion d'arc de cercle inclu dans la fenetre.     */
/* Les calculs sont faits pour les t2 premiers intervalles de largeur dt.     */
/* La routine calcule g, densite des couples de points;  et la fonction K     */
/* Les resultats sont stockes dans des tableaux g et k donnes en parametres   */

/******************************************************************************/

int ripley_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRect(*point_nb, x, y, xmi, xma, ymi, yma);

    /* On rangera dans g le nombre de couples de points par distance tt*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }

    /*On regarde les couples (i,j) et (j,i) : donc pour i>j seulement*/
    for (i=1; i<*point_nb; i++)
        {
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                /* dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_rect(x[i], y[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;

                /* pour [j,i] : correction des effets de bord*/
                cin=perim_in_rect(x[j], y[j], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }
        }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*fonction de Ripley pour une zone circulaire*/
int ripley_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                int *t2, double *dt, double *g, double *k)
    {
    int tt, i, j;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCirc(*point_nb, x, y, x0, y0, *r0);

    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }
    for (i=1; i<*point_nb; i++)
        { /*On calcule le nombre de couples de points par distance g*/
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_disq(x[i], y[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_disq(x[j], y[j], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }
        }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*Ripley triangles dans rectangle*/
int ripley_tr_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                   int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                   int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRectTri(*point_nb, x, y, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);

    /* On calcule le nombre de couples de points par distance g*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }
    for (i=1; i<*point_nb; i++)
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_rect(x[i], y[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[i], y[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_rect(x[j], y[j], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[j], y[j], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*Ripley triangle dans disque*/
int ripley_tr_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0, int *triangle_nb,
                   double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                   int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCircTri(*point_nb, x, y, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);

    /* On calcule le nombre de couples de points par distance g*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }
    for (i=1; i<*point_nb; i++)
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* pour [i,j] : correction des effets de bord*/
                cin=perim_in_disq(x[i], y[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[i], y[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;

                /* pour [j,i] : correction des effets de bord*/
                cin=perim_in_disq(x[j], y[j], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[j], y[j], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*fonction de Ripley avec intervalle de confiance pour une zone rectangulaire*/
int ripley_rect_ic(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma, double *densite,
                   int *t2, double *dt, int *nbSimu, double *prec, double *lev, double *g, double *k,
                   double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    int erreur=0;

    erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0)
        {
        return -1;
        }

    /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);


    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/(*densite);
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    int lp=0;

    /*boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        s_alea_rect(*point_nb, x, y, *xmi, *xma, *ymi, *yma, *prec);
        erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, gic1, kic1);
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Ripley\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/(*densite);
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(nn[j]-*densite)<=(float) fabs(nictmp-*densite))
                    {
                    nval[j]+=1;
                    }
                if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                    {
                    kval[j]+=1;
                    }
                if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                    {
                    lval[j]+=1;
                    }
                }

            /*Traitement des resultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux resultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }

    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);

    return 0;
    }

/*fonction de Ripley avec intervalle de confiance pour une zone circulaire*/
int ripley_disq_ic(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0, double *densite,
                   int *t2, double *dt, int *nbSimu, double *prec, double *lev, double *g, double *k,
                   double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    int erreur=0;

    erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0)
        {
        return -1;
        }

    /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/(*densite);
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    int lp=0;

    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        s_alea_disq(*point_nb, x, y, *x0, *y0, *r0, *prec);
        erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, gic1, kic1);
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i--;
            Rprintf("ERREUR Ripley\n");
            }
        else
            { /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/(*densite);
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(nn[j]-*densite)<=(float) fabs(nictmp-*densite))
                    {
                    nval[j]+=1;
                    }
                if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                    {
                    kval[j]+=1;
                    }
                if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                    {
                    lval[j]+=1;
                    }
                }

            /*Traitement des resultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux resultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }

    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);

    return 0;
    }

/*fonction de Ripley avec intervalle de confiance pour une zone rectangulaire + triangles*/
int ripley_tr_rect_ic(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma, double *densite,
                      int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                      int *t2, double *dt, int *nbSimu, double *prec, double *lev, double *g, double *k,
                      double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    int erreur=0;

    erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        {
        return -1;
        }

    /* definition de i0 : indice ou sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/(*densite);
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    int lp=0;

    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        s_alea_tr_rect(*point_nb, x, y, *xmi, *xma, *ymi, *yma, *triangle_nb, ax, ay, bx, by, cx, cy, *prec);
        erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);

        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Ripley\n");
            }
        else
            { /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/(*densite);
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(nn[j]-*densite)<=(float) fabs(nictmp-*densite))
                    {
                    nval[j]+=1;
                    }
                if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                    {
                    kval[j]+=1;
                    }
                if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                    {
                    lval[j]+=1;
                    }
                }

            /*Traitement des resultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux resultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }

    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);

    return 0;
    }

/*fonction de Ripley avec intervalle de confiance pour une zone circulaire + triangles*/
int ripley_tr_disq_ic(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0, double *densite,
                      int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                      int *t2, double *dt, int *nbSimu, double *prec, double *lev, double *g, double *k,
                      double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    int erreur=0;

    erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        {
        return -1;
        }

    /* definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);


    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(*densite*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/(*densite);
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    int lp=0;

    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {

        s_alea_tr_disq(*point_nb, x, y, *x0, *y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy, *prec);
        erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);

        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Ripley\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(*densite*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/(*densite);
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(nn[j]-*densite)<=(float) fabs(nictmp-*densite))
                    {
                    nval[j]+=1;
                    }
                if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) fabs(kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                    {
                    kval[j]+=1;
                    }
                if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                    {
                    lval[j]+=1;
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }

    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);

    return 0;
    }

/*fonction de Ripley locale pour une zone rectangulaire*/
int ripleylocal_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                     int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRect(*point_nb, x, y, xmi, xma, ymi, yma);


    taballoc(&g, *point_nb, *t2);
    taballoc(&k, *point_nb, *t2);

    for (i=0; i<*point_nb; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;

    for (i=1; i<*point_nb; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* pour [i,j] : correction des effets de bord*/
                cin=perim_in_rect(x[i], y[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_rect(x[j], y[j], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                g[j][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*fonction de Ripley locale pour une zone circulaire*/
int ripleylocal_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                     int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCirc(*point_nb, x, y, x0, y0, *r0);

    taballoc(&g, *point_nb, *t2);
    taballoc(&k, *point_nb, *t2);

    for (i=0; i<*point_nb; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;
    for (i=1; i<*point_nb; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_disq(x[i], y[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_disq(x[j], y[j], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                g[j][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]+=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*fonction de Ripley locale triangles dans rectangle*/
int ripleylocal_tr_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                        int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                        int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRectTri(*point_nb, x, y, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);

    taballoc(&g, *point_nb, *t2);
    taballoc(&k, *point_nb, *t2);

    for (i=0; i<*point_nb; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;

    for (i=1; i<*point_nb; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_rect(x[i], y[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[i], y[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_rect(x[j], y[j], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[j], y[j], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[j][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*fonction de Ripley locale triangles dans cercle*/
int ripleylocal_tr_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                        int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                        int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCircTri(*point_nb, x, y, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);
    taballoc(&g, *point_nb, *t2);
    taballoc(&k, *point_nb, *t2);

    for (i=0; i<*point_nb; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;
    for (i=1; i<*point_nb; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* pour [i,j] : correction des effets de bord*/
                cin=perim_in_disq(x[i], y[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[i], y[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_disq(x[j], y[j], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[j], y[j], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[j][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]+=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*Densite locale pour une zone rectangulaire*/
int density_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi,
                 double *yma, int *t2, double *dt, double *xx, double *yy, int *sample_nb, double *count)
    {
    int tt, i, j;
    double ddd, cin;
    double **s;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalSample(*sample_nb, xx, yy, *xmi, *ymi);
    decalRect(*point_nb, x, y, xmi, xma, ymi, yma);
    taballoc(&s, *sample_nb, *t2);

    for (j=0; j<*sample_nb; j++)
        {
        for (tt=0; tt<*t2; tt++)
            s[j][tt]=0;
        for (i=0; i<*point_nb; i++) /* On calcule le nombre de voisins dans chaque disque de rayon r*/
            {
            ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
            if (ddd<*t2*(*dt))
                {
                tt=ddd/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_rect(xx[j], yy[j], ddd, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                s[j][tt]+=2*Pi()/cin;
                }
            }
        }
    for (i=0; i<*sample_nb; i++)
        for (tt=1; tt<*t2; tt++)
            s[i][tt]+=s[i][tt-1]; /* on integre*/

    /*Copies des valeurs dans le tableau resultat*/
    for (i=0; i<*sample_nb; i++)
        for (tt=0; tt<*t2; tt++)
            count[i*(*t2)+tt]=s[i][tt];

    freetab(s);

    return 0;
    }

/*Densite locale pour une zone circulaire*/
int density_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                 int *t2, double *dt, double *xx, double *yy, int *sample_nb, double *count)
    {
    int tt, i, j;
    double ddd, cin;
    double **s;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalSample(*sample_nb, xx, yy, *x0-*r0, *y0-*r0);
    decalCirc(*point_nb, x, y, x0, y0, *r0);


    taballoc(&s, *sample_nb, *t2);

    for (j=0; j<*sample_nb; j++)
        {
        for (tt=0; tt<*t2; tt++)
            s[j][tt]=0;
        for (i=0; i<*point_nb; i++) /* On calcule le nombre de voisins dans chaque disque de rayon r*/
            {
            ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
            if (ddd<*t2*(*dt))
                {
                tt=ddd/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_disq(xx[j], yy[j], ddd, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                s[j][tt]+=2*Pi()/cin;
                }
            }
        }
    for (i=0; i<*sample_nb; i++)
        for (tt=1; tt<*t2; tt++)
            s[i][tt]+=s[i][tt-1]; /* on integre*/

    /*Copies des valeurs dans le tableau resultat*/
    for (i=0; i<*sample_nb; i++)
        for (tt=0; tt<*t2; tt++)
            count[i*(*t2)+tt]=s[i][tt];


    freetab(s);

    return 0;
    }

/*Densite locale pour triangles dans rectangle*/
int density_tr_rect(int *point_nb, double *x, double *y, double *xmi, double *xma,
                    double *ymi, double *yma, int *triangle_nb, double *ax, double *ay, double *bx,
                    double *by, double *cx, double *cy, int *t2, double *dt, double *xx,
                    double *yy, int *sample_nb, double *count)
    {
    int tt, i, j;
    double ddd, cin;
    double **s;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalSample(*sample_nb, xx, yy, *xmi, *ymi);
    decalRectTri(*point_nb, x, y, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);
    taballoc(&s, *sample_nb, *t2);

    for (j=0; j<*sample_nb; j++)
        {
        for (tt=0; tt<*t2; tt++)
            s[j][tt]=0;
        for (i=0; i<*point_nb; i++) /* On calcule le nombre de voisins dans chaque disque de rayon r*/
            {
            ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
            if (ddd<*t2*(*dt))
                {
                tt=ddd/(*dt);

                /*correction des effets de bord*/
                cin=perim_in_rect(xx[j], yy[j], ddd, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(xx[j], yy[j], ddd, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                s[j][tt]+=2*Pi()/cin;
                }
            }
        }
    for (i=0; i<*sample_nb; i++)
        for (tt=1; tt<*t2; tt++)
            s[i][tt]+=s[i][tt-1]; /* on integre */

    /* Copies des valeurs dans le tableau resultat*/
    for (i=0; i<*sample_nb; i++)
        for (tt=0; tt<*t2; tt++)
            count[i*(*t2)+tt]=s[i][tt];

    freetab(s);

    return 0;
    }

/*Densite locale pour triangles dans cercle*/
int density_tr_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                    int *triangle_nb, double *ax, double *ay, double *bx, double *by,
                    double *cx, double *cy, int *t2, double *dt, double *xx, double *yy,
                    int *sample_nb, double *count)
    {
    int tt, i, j;
    double ddd, cin;
    double **s;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalSample(*sample_nb, xx, yy, *x0-*r0, *y0-*r0);
    decalCircTri(*point_nb, x, y, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);
    taballoc(&s, *sample_nb, *t2);

    for (j=0; j<*sample_nb; j++)
        {
        for (tt=0; tt<*t2; tt++)
            s[j][tt]=0;
        for (i=0; i<*point_nb; i++) /* On calcule le nombre de voisins dans chaque disque de rayon r*/
            {
            ddd=sqrt((xx[j]-x[i])*(xx[j]-x[i])+(yy[j]-y[i])*(yy[j]-y[i]));
            if (ddd<*t2*(*dt))
                {
                tt=ddd/(*dt);

                /*correction des effets de bord*/
                cin=perim_in_disq(xx[j], yy[j], ddd, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(xx[j], yy[j], ddd, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                s[j][tt]+=2*Pi()/cin;
                }
            }
        }
    for (i=0; i<*sample_nb; i++)
        for (tt=1; tt<*t2; tt++)
            s[i][tt]+=s[i][tt-1]; /* on integre*/

    /*Copies des valeurs dans le tableau resultat*/
    for (i=0; i<*sample_nb; i++)
        for (tt=0; tt<*t2; tt++)
            count[i*(*t2)+tt]=s[i][tt];

    freetab(s);

    return 0;
    }

/******************************************************************************/
/* Calcule la fonction intertype pour les semis (x,y) et (x2,y2) en parametres*/
/* dans une zone de forme rectangulaire de bornes xmi xma ymi yma             */
/* Les corrections des effets de bords sont fait par la methode de Ripley,    */
/* i.e. l'inverse de la proportion d'arc de cercle inclu dans la fenetre.     */
/* Les calculs sont faits pour les t2 premiers intervalles de largeur dt.     */
/* La routine calcule g12, densite des couples de points;  et la fonction K12 */
/* Les resultats sont stockes dans des tableaux g et k donnes en parametres   */

/******************************************************************************/

int intertype_rect(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                   double *xmi, double *xma, double *ymi, double *yma, int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRect2(*point_nb1, x1, y1, *point_nb2, x2, y2, xmi, xma, ymi, yma);

    /*On rangera dans g le nombre de couples de points par distance tt*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }

    /* On regarde tous les couples (i,j)*/
    for (i=0; i<*point_nb1; i++)
        {
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                { /* dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);
                /* correction des effets de bord*/
                cin=perim_in_rect(x1[i], y1[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("\ncin<0 sur i AVANT");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }
        }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb1);
        }

    /*on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*fonction intertype pour une zone circulaire*/
int intertype_disq(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2,
                   double *y2, double *x0, double *y0, double *r0, int *t2, double *dt, double *g, double *k)
    {
    int tt, i, j;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCirc2(*point_nb1, x1, y1, *point_nb2, x2, y2, x0, y0, *r0);

    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }
    for (i=0; i<*point_nb1; i++) /* On calcule le nombre de couples de points par distance g*/
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_disq(x1[i], y1[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("\ncin<0 sur i AVANT");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb1);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*Intertype triangles dans rectangle*/
int intertype_tr_rect(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                      double *xmi, double *xma, double *ymi, double *yma, int *triangle_nb, double *ax, double *ay, double *bx, double *by,
                      double *cx, double *cy, int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRectTri2(*point_nb1, x1, y1, *point_nb2, x2, y2, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);

    /* On calcule le nombre de couples de points par distance g*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }
    for (i=0; i<*point_nb1; i++)
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);
                cin=perim_in_rect(x1[i], y1[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("\ncin<0 sur i AVANT");
                    return -1;
                    }
                cin=cin-perim_triangle(x1[i], y1[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb1);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*Intertype triangles dans cercle*/
int intertype_tr_disq(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                      double *x0, double *y0, double *r0, int *triangle_nb, double *ax, double *ay, double *bx, double *by,
                      double *cx, double *cy, int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d, cin;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCircTri2(*point_nb1, x1, y1, *point_nb2, x2, y2, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);

    /* On calcule le nombre de couples de points par distance g*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        }
    for (i=0; i<*point_nb1; i++)
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);
                cin=perim_in_disq(x1[i], y1[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("\ncin<0 sur i AVANT");
                    return -1;
                    }
                cin=cin-perim_triangle(x1[i], y1[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                }
            }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=g[tt]/(*point_nb1);
        }

    /* on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        }

    return 0;
    }

/*fonction intertype avec intervalle de confiance pour une zone rectangulaire*/
int intertype_rect_ic(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                      double *xmi, double *xma, double *ymi, double *yma, double *surface,
                      int *t2, double *dt, int *nbSimu, int *h0, double *prec, int *nsimax, int *conv, int *rep, double *lev, double *g, double *k,
                      double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2, r;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    double *gt, *kt, *lt, *nt;
    int erreur=0;
    int *type;
    //double *x,*y,*cost,surface,*xx,*yy;
    double *x, *y, *cost, densite_1, densite_2, densite_tot;
    int point_nb=0, point_nbtot;

    densite_1=(*point_nb1)/(*surface);
    densite_2=(*point_nb2)/(*surface);
    point_nbtot=(*point_nb1)+(*point_nb2);
    densite_tot=point_nbtot/(*surface);

    erreur=intertype_rect(point_nb1, x1, y1, point_nb2, x2, y2, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0) return -1;

    //Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    //Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);

    //Normalisation de g et k et calcul de l et n pour le calcul des p-values
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(densite_2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/densite_2;
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    //Initialisations avant la boucle principale
    if (*h0==1)
        { //Option 1 : substitutions : on stocke tous les points
        vecalloc(&x, point_nbtot);
        vecalloc(&y, point_nbtot);
        vecintalloc(&type, point_nbtot);
        for (i=0; i<*point_nb1; i++)
            {
            x[i]=x1[i];
            y[i]=y1[i];
            }
        for (i=0; i<*point_nb2; i++)
            {
            x[*point_nb1+i]=x2[i];
            y[*point_nb1+i]=y2[i];
            }
        //on lance Ripley sur tous les points + normalization pour le calcul des p-values
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&nt, *t2);
        erreur=ripley_rect(&point_nbtot, x, y, xmi, xma, ymi, yma, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            {
            gt[j]=gt[j]/(densite_tot*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
            nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
            kt[j]=kt[j]/(densite_tot);
            lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
            }
        }
    if (*h0==2)
        { //Option 2 : initialisation coordonn�es points d�cal�s
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        }

    if (*h0==3)
        { //Option 3 : on lance Ripley sur les points de type 1
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&cost, *nsimax);
        /*densite1=(*densite-*densite2);
        surface=(*point_nb1)/densite1;*/
        erreur=ripley_rect(point_nb1, x1, y1, xmi, xma, ymi, yma, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            lt[j]=sqrt(kt[j]/(densite_1*Pi()))-(j+1)*(*dt);
        }
    int lp=0;

    //boucle principale de MC
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {

        //On simule les hypotheses nulles
        if (*h0==1)
            erreur=randlabelling(x, y, *point_nb1, x1, y1, *point_nb2, x2, y2, type);
        if (*h0==2)
            erreur=randshifting_rect(&point_nb, x, y, *point_nb1, x1, y1, *xmi, *xma, *ymi, *yma, *prec);
        if (*h0==3)
            {
            erreur=1;
            r=0;
            while (erreur!=0)
                {
                erreur=mimetic_rect(point_nb1, x1, y1, surface, xmi, xma, ymi, yma, prec, t2, dt, lt, nsimax, conv, cost, gt, kt, x, y, 0);
                r=r+erreur;
                if (r==*rep)
                    {
                    Rprintf("\nStop: mimetic_rect failed to converge more than %d times\n", r);
                    Rprintf("Adjust arguments nsimax and/or conv\n");
                    return -1;
                    }
                }
            }

        if (erreur==0)
            {
            if (*h0==1)//etiquetage aleatoire
                erreur=intertype_rect(point_nb1, x1, y1, point_nb2, x2, y2, xmi, xma, ymi, yma, t2, dt, gic1, kic1);
            if (*h0==2) // d�callage avec rectangle
                erreur=intertype_rect(&point_nb, x, y, point_nb2, x2, y2, xmi, xma, ymi, yma, t2, dt, gic1, kic1);
            if (*h0==3) // mim�tique
                erreur=intertype_rect(point_nb1, x, y, point_nb2, x2, y2, xmi, xma, ymi, yma, t2, dt, gic1, kic1);
            }
        // si il y a une erreur on recommence une simulation
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Intertype\n");
            }
        else
            {
            //comptage du nombre de |�obs|<=|�simu| pour test local
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(densite_2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/densite_2;
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if (*h0==1)
                    {
                    if ((float) fabs(gg[j]-gt[j])<=(float) fabs(gictmp-gt[j]))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float) fabs(nictmp-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt)))))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-kt[j])<=(float) (kictmp-kt[j]))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j]-lt[j])<=(float) fabs(lictmp-lt[j]))
                        {
                        lval[j]+=1;
                        }
                    }
                else
                    { //h0=2 ou 3
                    if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-densite_2)<=(float) fabs(nictmp-densite_2))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) (kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                        {
                        lval[j]+=1;
                        }
                    }
                }

            //Traitement des r�sultats
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    //Copies des valeurs dans les tableaux r�sultats
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }


    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);
    if (*h0==1)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(nt);
        freeintvec(type);
        }
    if (*h0==3)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(cost);
        }
    freevec(x);
    freevec(y);
    return 0;
    }

/*fonction intertype avec intervalle de confiance pour une zone circulaire*/
int intertype_disq_ic(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                      double *x0, double *y0, double *r0, double *surface, int *t2, double *dt, int *nbSimu, int *h0, double *prec,
                      int *nsimax, int *conv, int *rep, double *lev, double *g, double *k, double *gic1, double *gic2,
                      double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2, r;
    double **gic, **kic;
    double *gt, *kt, *lt, *nt;
    double *gg, *kk, *ll, *nn;
    int erreur=0;
    int *type;
    double *x, *y, *cost, densite_1, densite_2, densite_tot;
    int point_nb=0, point_nbtot;

    densite_1=(*point_nb1)/(*surface);
    densite_2=(*point_nb2)/(*surface);
    point_nbtot=(*point_nb1)+(*point_nb2);
    densite_tot=point_nbtot/(*surface);

    erreur=intertype_disq(point_nb1, x1, y1, point_nb2, x2, y2, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0) return -1;

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(densite_2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/(densite_2);
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    /*Initialisations avant la boucle principale*/

    if (*h0==1)
        { /*Option 1 : substitutions : on stocke tous les points*/
        vecalloc(&x, point_nbtot);
        vecalloc(&y, point_nbtot);
        vecintalloc(&type, point_nbtot);
        for (i=0; i<*point_nb1; i++)
            {
            x[i]=x1[i];
            y[i]=y1[i];
            }
        for (i=0; i<*point_nb2; i++)
            {
            x[*point_nb1+i]=x2[i];
            y[*point_nb1+i]=y2[i];
            }
        /*on lance Ripley sur tous les points + normalization pour le calcul des p-values*/
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&nt, *t2);
        erreur=ripley_disq(&point_nbtot, x, y, x0, y0, r0, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            {
            gt[j]=gt[j]/(densite_tot*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
            nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
            kt[j]=kt[j]/(densite_tot);
            lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
            }
        }
    //Sinon option 2 : rien a initialiser*
    if (*h0==2)
        {
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        }
    if (*h0==3)
        { //Option 3 : on lance Ripley sur les points de type 1
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&cost, *nsimax);
        erreur=ripley_disq(point_nb1, x1, y1, x0, y0, r0, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            lt[j]=sqrt(kt[j]/(densite_1*Pi()))-(j+1)*(*dt);
        }
    int lp=0;

    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {

        /*On simule les hypoth�ses nulles*/
        if (*h0==1)
            erreur=randlabelling(x, y, *point_nb1, x1, y1, *point_nb2, x2, y2, type);
        if (*h0==2)
            erreur=randshifting_disq(&point_nb, x, y, *point_nb1, x1, y1, *x0, *y0, *r0, *prec);
        if (*h0==3)
            {
            erreur=1;
            r=0;
            while (erreur!=0)
                {
                erreur=mimetic_disq(point_nb1, x1, y1, surface, x0, y0, r0, prec, t2, dt, lt, nsimax, conv, cost, gt, kt, x, y, 0);
                r=r+erreur;
                if (r==*rep)
                    {
                    Rprintf("\nStop: mimetic_disq failed to converge more than %d times\n", r);
                    Rprintf("Adjust arguments nsimax and/or conv\n");
                    return -1;
                    }
                }
            }
        if (erreur==0)
            {
            if (*h0==1)
                {
                erreur=intertype_disq(point_nb1, x1, y1, point_nb2, x2, y2, x0, y0, r0, t2, dt, gic1, kic1);
                }
            if (*h0==2)
                {
                erreur=intertype_disq(&point_nb, x, y, point_nb2, x2, y2, x0, y0, r0, t2, dt, gic1, kic1);
                }
            if (*h0==3)
                {
                // mim�tique
                erreur=intertype_disq(point_nb1, x, y, point_nb2, x2, y2, x0, y0, r0, t2, dt, gic1, kic1);
                }
            }
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Intertype\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(densite_2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/densite_2;
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if (*h0==1)
                    {
                    if ((float) fabs(gg[j]-gt[j])<=(float) fabs(gictmp-gt[j]))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float) fabs(nictmp-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt)))))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-kt[j])<=(float) (kictmp-kt[j]))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j]-lt[j])<=(float) fabs(lictmp-lt[j]))
                        {
                        lval[j]+=1;
                        }
                    }
                else
                    {//h0=2 ou 3
                    if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-densite_2)<=(float) fabs(nictmp-densite_2))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) (kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                        {
                        lval[j]+=1;
                        }
                    }
                }
            /*Traitement des r�sultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }
    i1=i0+2;
    i2=i0;
    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }
    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);
    if (*h0==1)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(nt);
        freeintvec(type);
        }
    if (*h0==3)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(cost);
        }
    freevec(x);
    freevec(y);
    return 0;
    }

/*fonction intertype avec intervalle de confiance pour une zone rectangulaire + triangles*/
int intertype_tr_rect_ic(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                         double *xmi, double *xma, double *ymi, double *yma, double *surface,
                         int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                         int *t2, double *dt, int *nbSimu, int *h0, double *prec, int *nsimax, int *conv, int *rep, double *lev, double *g, double *k,
                         double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2, r;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    double *gt, *kt, *lt, *nt;
    int erreur=0;
    int *type;
    double *x, *y, *cost, densite_1, densite_2, densite_tot;
    int point_nb=0, point_nbtot;
    double **tab;

    densite_1=(*point_nb1)/(*surface);
    densite_2=(*point_nb2)/(*surface);
    point_nbtot=(*point_nb1)+(*point_nb2);
    densite_tot=point_nbtot/(*surface);

    erreur=intertype_tr_rect(point_nb1, x1, y1, point_nb2, x2, y2, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0) return -1;

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);
    taballoc(&tab, 2, point_nbtot);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(densite_2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/densite_2;
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    /*Initialisations avant la boucle principale*/

    if (*h0==1)
        { /*Option 1 : substitutions : on stocke tous les points*/
        vecalloc(&x, point_nbtot);
        vecalloc(&y, point_nbtot);
        vecintalloc(&type, point_nbtot);
        for (i=0; i<*point_nb1; i++)
            {
            x[i]=x1[i];
            y[i]=y1[i];
            }
        for (i=0; i<*point_nb2; i++)
            {
            x[*point_nb1+i]=x2[i];
            y[*point_nb1+i]=y2[i];
            }
        /*on lance Ripley sur tous les points + normalization pour le calcul des p-values*/
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&nt, *t2);
        erreur=ripley_tr_rect(&point_nbtot, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            {
            gt[j]=gt[j]/(densite_tot*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
            nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
            kt[j]=kt[j]/densite_tot;
            lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
            }
        }
    /*Sinon option 2 : rien a initialiser*/
    if (*h0==2)
        {
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        }

    if (*h0==3)
        { //Option 3 : on lance Ripley sur les points de type 1
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&cost, *nsimax);
        erreur=ripley_tr_rect(point_nb1, x1, y1, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++) /*normalisation pour mimetic*/
            lt[j]=sqrt(kt[j]/(densite_1*Pi()));
        }

    int lp=0;

    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {

        /*On simule les hypoth�ses nulles*/
        if (*h0==1)
            erreur=randlabelling(x, y, *point_nb1, x1, y1, *point_nb2, x2, y2, type);
        if (*h0==2)
            erreur=randshifting_tr_rect(&point_nb, x, y, *point_nb1, x1, y1, *xmi, *xma, *ymi, *yma, *triangle_nb, ax, ay, bx, by, cx, cy, *prec);
        if (*h0==3)
            {
            erreur=1;
            r=0;
            while (erreur!=0)
                {
                erreur=mimetic_tr_rect(point_nb1, x1, y1, surface, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, prec, t2, dt, lt, nsimax, conv, cost, gt, kt, x, y, 0);
                r=r+erreur;
                if (r==*rep)
                    {
                    Rprintf("\nStop: mimetic_tr_rect failed to converge more than %d times\n", r);
                    Rprintf("Adjust arguments nsimax and/or conv\n");
                    return -1;
                    }
                }
            }

        if (erreur==0)
            {
            if (*h0==1) //�tiquetage al�atoire
                erreur=intertype_tr_rect(point_nb1, x1, y1, point_nb2, x2, y2, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
            if (*h0==2) //d�callage avec rectangle
                erreur=intertype_tr_rect(&point_nb, x, y, point_nb2, x2, y2, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
            if (*h0==3) // mim�tique
                erreur=intertype_tr_rect(point_nb1, x, y, point_nb2, x2, y2, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
            }
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Intertype\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(densite_2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/(densite_2);
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if (*h0==1)
                    {
                    if ((float) fabs(gg[j]-gt[j])<=(float) fabs(gictmp-gt[j]))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float) fabs(nictmp-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt)))))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-kt[j])<=(float) (kictmp-kt[j]))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j]-lt[j])<=(float) fabs(lictmp-lt[j]))
                        {
                        lval[j]+=1;
                        }
                    }
                else
                    { //h0=2 ou 3
                    if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-densite_2)<=(float) fabs(nictmp-densite_2))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) (kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                        {
                        lval[j]+=1;
                        }
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }
    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);
    if (*h0==1)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(nt);
        freeintvec(type);
        }
    if (*h0==3)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(cost);
        }
    freevec(x);
    freevec(y);
    return 0;
    }

/*fonction intertype avec intervalle de confiance pour une zone circulaire + triangles*/
int intertype_tr_disq_ic(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2,
                         double *x0, double *y0, double *r0, double *surface,
                         int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                         int *t2, double *dt, int *nbSimu, int *h0, double *prec, int *nsimax, int *conv, int *rep, double *lev, double *g, double *k,
                         double *gic1, double *gic2, double *kic1, double *kic2, double *gval, double *kval, double *lval, double *nval)
    {
    int i, j, i0, i1, i2, r;
    double **gic, **kic;
    double *gg, *kk, *ll, *nn;
    double *gt, *kt, *lt, *nt;
    int erreur=0;
    int *type;
    double *x, *y, *cost, densite_1, densite_2, densite_tot;
    int point_nb=0, point_nbtot;

    densite_1=(*point_nb1)/(*surface);
    densite_2=(*point_nb2)/(*surface);
    point_nbtot=(*point_nb1)+(*point_nb2);
    densite_tot=point_nbtot/(*surface);

    erreur=intertype_tr_disq(point_nb1, x1, y1, point_nb2, x2, y2, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0) return -1;

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&gg, *t2);
    vecalloc(&kk, *t2);
    vecalloc(&ll, *t2);
    vecalloc(&nn, *t2);
    for (i=0; i<*t2; i++)
        {
        gg[i]=g[i]/(densite_2*(Pi()*(i+1)*(i+1)*(*dt)*(*dt)-Pi()*i*i*(*dt)*(*dt)));
        nn[i]=k[i]/(Pi()*(i+1)*(i+1)*(*dt)*(*dt));
        kk[i]=k[i]/densite_2;
        ll[i]=sqrt(kk[i]/Pi())-(i+1)*(*dt);
        gval[i]=1;
        kval[i]=1;
        nval[i]=1;
        lval[i]=1;
        }

    /*Initialisations avant la boucle principale*/

    if (*h0==1)
        { /*Option 1 : substitutions : on stocke tous les points*/
        vecalloc(&x, point_nbtot);
        vecalloc(&y, point_nbtot);
        vecintalloc(&type, point_nbtot);
        for (i=0; i<*point_nb1; i++)
            {
            x[i]=x1[i];
            y[i]=y1[i];
            }
        for (i=0; i<*point_nb2; i++)
            {
            x[*point_nb1+i]=x2[i];
            y[*point_nb1+i]=y2[i];
            }
        /*on lance Ripley sur tous les points + normalization pour le calcul des p-values*/
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&nt, *t2);
        erreur=ripley_tr_disq(&point_nbtot, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            {
            gt[j]=gt[j]/(densite_tot*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
            nt[j]=kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
            kt[j]=kt[j]/densite_tot;
            lt[j]=sqrt(kt[j]/Pi())-(j+1)*(*dt);
            }
        }
    /*Sinon option 2 : rien a initialiser*/
    if (*h0==2)
        {
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        }
    if (*h0==3)
        { //Option 3 : on lance Ripley sur les points de type 1
        vecalloc(&x, *point_nb1);
        vecalloc(&y, *point_nb1);
        vecalloc(&gt, *t2);
        vecalloc(&kt, *t2);
        vecalloc(&lt, *t2);
        vecalloc(&cost, *nsimax);
        erreur=ripley_tr_disq(point_nb1, x1, y1, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gt, kt);
        if (erreur!=0) return -1;
        for (j=0; j<*t2; j++)
            lt[j]=sqrt(kt[j]/(densite_1*Pi()))-(j+1)*(*dt);
        }
    int lp=0;

    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {

        /*On simule les hypoth�ses nulles*/
        if (*h0==1)
            erreur=randlabelling(x, y, *point_nb1, x1, y1, *point_nb2, x2, y2, type);
        if (*h0==2)
            erreur=randshifting_tr_disq(&point_nb, x, y, *point_nb1, x1, y1, *x0, *y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy, *prec);
        if (*h0==3)
            {
            erreur=1;
            r=0;
            while (erreur!=0)
                {
                erreur=mimetic_tr_disq(point_nb1, x1, y1, surface, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, prec, t2, dt, lt, nsimax, conv, cost, gt, kt, x, y, 0);
                r=r+erreur;
                if (r==*rep)
                    {
                    Rprintf("\nStop: mimetic_tr_disq failed to converge more than %d times\n", r);
                    Rprintf("Adjust arguments nsimax and/or conv\n");
                    return -1;
                    }
                }
            }

        if (erreur==0)
            {
            if (*h0==1) //�tiquetage al�atoire
                erreur=intertype_tr_disq(point_nb1, x1, y1, point_nb2, x2, y2, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
            if (*h0==2) //d�callage avec rectangle
                erreur=intertype_tr_disq(&point_nb, x, y, point_nb2, x2, y2, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
            if (*h0==3) // mim�tique
                erreur=intertype_tr_disq(point_nb1, x, y, point_nb2, x2, y2, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
            }
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Intertype\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gictmp, kictmp, lictmp, nictmp;
            for (j=0; j<*t2; j++)
                {
                gictmp=gic1[j]/(densite_2*(Pi()*(j+1)*(j+1)*(*dt)*(*dt)-Pi()*j*j*(*dt)*(*dt)));
                nictmp=kic1[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt));
                kictmp=kic1[j]/densite_2;
                lictmp=sqrt(kictmp/Pi())-(j+1)*(*dt);
                if (*h0==1)
                    {
                    if ((float) fabs(gg[j]-gt[j])<=(float) fabs(gictmp-gt[j]))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt))))<=(float) fabs(nictmp-(densite_2*kt[j]/(Pi()*(j+1)*(j+1)*(*dt)*(*dt)))))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-kt[j])<=(float) (kictmp-kt[j]))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j]-lt[j])<=(float) fabs(lictmp-lt[j]))
                        {
                        lval[j]+=1;
                        }
                    }
                else
                    {//h0=2 ou 3
                    if ((float) fabs(gg[j]-1)<=(float) fabs(gictmp-1))
                        {
                        gval[j]+=1;
                        }
                    if ((float) fabs(nn[j]-densite_2)<=(float) fabs(nictmp-densite_2))
                        {
                        nval[j]+=1;
                        }
                    if ((float) fabs(kk[j]-Pi()*(j+1)*(j+1)*(*dt)*(*dt))<=(float) (kictmp-Pi()*(j+1)*(j+1)*(*dt)*(*dt)))
                        {
                        kval[j]+=1;
                        }
                    if ((float) fabs(ll[j])<=(float) fabs(lictmp))
                        {
                        lval[j]+=1;
                        }
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gic, kic, gic1, kic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }
    freetab(gic);
    freetab(kic);
    freevec(gg);
    freevec(kk);
    freevec(ll);
    freevec(nn);
    if (*h0==1)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(nt);
        freeintvec(type);
        }
    if (*h0==3)
        {
        freevec(gt);
        freevec(kt);
        freevec(lt);
        freevec(cost);
        }
    freevec(x);
    freevec(y);
    return 0;
    }

/*fonction intertype locale pour une zone rectangulaire*/
int intertypelocal_rect(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2, double *xmi, double *xma,
                        double *ymi, double *yma, int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRect2(*point_nb1, x1, y1, *point_nb2, x2, y2, xmi, xma, ymi, yma);
    taballoc(&g, *point_nb1, *t2);
    taballoc(&k, *point_nb1, *t2);

    for (i=0; i<*point_nb1; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;

    for (i=0; i<*point_nb1; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_rect(x1[i], y1[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb1; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb1; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*fonction intertype locale pour une zone circulaire*/
int intertypelocal_disq(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2, double *x0, double *y0,
                        double *r0, int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCirc2(*point_nb1, x1, y1, *point_nb2, x2, y2, x0, y0, *r0);


    taballoc(&g, *point_nb1, *t2);
    taballoc(&k, *point_nb1, *t2);

    for (i=0; i<*point_nb1; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;

    for (i=0; i<*point_nb1; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_disq(x1[i], y1[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb1; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb1; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*fonction intertype locale pour une zone rectangulaire + triangles*/
int intertypelocal_tr_rect(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2, double *xmi, double *xma,
                           double *ymi, double *yma, int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                           int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRectTri2(*point_nb1, x1, y1, *point_nb2, x2, y2, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);
    taballoc(&g, *point_nb1, *t2);
    taballoc(&k, *point_nb1, *t2);

    for (i=0; i<*point_nb1; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;

    for (i=0; i<*point_nb1; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_rect(x1[i], y1[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x1[i], y1[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb1; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb1; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/*fonction intertype locale pour une zone circulaire + triangles*/
int intertypelocal_tr_disq(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2, double *x0, double *y0,
                           double *r0, int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                           int *t2, double *dt, double *gi, double *ki)
    {
    int tt, i, j;
    double d, cin;
    double **g, **k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCircTri2(*point_nb1, x1, y1, *point_nb2, x2, y2, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);
    taballoc(&g, *point_nb1, *t2);
    taballoc(&k, *point_nb1, *t2);

    for (i=0; i<*point_nb1; i++)
        for (tt=0; tt<*t2; tt++)
            g[i][tt]=0;

    for (i=0; i<*point_nb1; i++) /* On calcule le nombre de couples de points par distance g */
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                {
                tt=d/(*dt);

                /* correction des effets de bord*/
                cin=perim_in_disq(x1[i], y1[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x1[i], y1[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[i][tt]+=2*Pi()/cin;
                }
            }

    for (i=0; i<*point_nb1; i++)
        {
        k[i][0]=g[i][0];
        for (tt=1; tt<*t2; tt++)
            k[i][tt]=k[i][tt-1]+g[i][tt]; /* on integre */
        }

    /*Copies des valeurs dans les tableaux resultat*/
    for (i=0; i<*point_nb1; i++)
        {
        for (tt=0; tt<*t2; tt++)
            {
            gi[i*(*t2)+tt]=g[i][tt];
            ki[i*(*t2)+tt]=k[i][tt];
            }
        }

    freetab(g);
    freetab(k);

    return 0;
    }

/******************************************************************************/
/* Cette routine cree un semis poissonnien a la precision p pour x et y,      */
/* dans une forme rectangulaire xmi xma ymi yma, a la precision p,            */
/* et le range dans les tableaux dont les pointeurs sont fournis en parametre */

/******************************************************************************/
void s_alea_rect(int point_nb, double x[], double y[],
                 double xmi, double xma, double ymi, double yma, double p)
    {
    int i;
    double xr, yr;
    xr=xma-xmi;
    yr=yma-ymi;
    GetRNGstate();
    for (i=0; i<point_nb; i++)
        {
        x[i]=xmi+(unif_rand()*(xr/p))*p;
        y[i]=ymi+(unif_rand()*(yr/p))*p;
        }
    PutRNGstate();
    }

/*pour un zone circulaire*/
void s_alea_disq(int point_nb, double *x, double *y, double x0, double y0, double r0, double p)
    {
    int i;
    double xx, yy, rr;
    rr=2*r0;
    GetRNGstate();
    i=0;
    while (i<point_nb)
        {
        xx=x0-r0+(unif_rand()*(rr/p))*p;
        yy=y0-r0+(unif_rand()*(rr/p))*p;
        if ((xx-x0)*(xx-x0)+(yy-y0)*(yy-y0)<r0*r0)
            {
            x[i]=xx;
            y[i]=yy;
            i++;
            }
        }
    PutRNGstate();
    }

/*pour une zone rectangulaire avec exclusion de triangles*/
void s_alea_tr_rect(int point_nb, double *x, double *y, double xmi, double xma, double ymi, double yma, int triangle_nb,
                    double *ax, double *ay, double *bx, double *by, double *cx, double *cy, double p)
    {
    int i, j, erreur;
    double xr, yr;
    xr=xma-xmi;
    yr=yma-ymi;
    GetRNGstate();

    i=0;
    while (i<point_nb)
        { /* on simule le ieme point dans le rectangle*/
        x[i]=xmi+(unif_rand()*(xr/p))*p;
        y[i]=ymi+(unif_rand()*(yr/p))*p;

        /* si il n'est dans aucun triangle, on passe au suivant, sinon on recommence*/
        erreur=0;
        j=0;
        while ((j<triangle_nb)&&(erreur==0))
            {
            if (in_triangle(x[i], y[i], ax[j], ay[j], bx[j], by[j], cx[j], cy[j], 1))
                {
                erreur=1;
                }
            j++;
            }
        if (erreur==0)
            {
            i++;
            }
        }
    PutRNGstate();
    }

/*pour une zone circulaire avec exclusion de triangles*/
void s_alea_tr_disq(int point_nb, double *x, double *y, double x0, double y0, double r0, int triangle_nb,
                    double *ax, double *ay, double *bx, double *by, double *cx, double *cy, double p)
    {
    int i, j, erreur;
    double rr;
    rr=2*r0;
    GetRNGstate();

    i=0;
    while (i<point_nb)
        {
        erreur=0;
        /* on simule le ieme point dans le cercle*/
        x[i]=x0-r0+(unif_rand()*(rr/p))*p;
        y[i]=y0-r0+(unif_rand()*(rr/p))*p;
        if ((x[i]-x0)*(x[i]-x0)+(y[i]-y0)*(y[i]-y0)>r0*r0) erreur=1;

        /* si il n'est dans aucun triangle, on passe au suivant, sinon on recommence*/
        j=0;
        while ((j<triangle_nb)&&(erreur==0))
            {
            if (in_triangle(x[i], y[i], ax[j], ay[j], bx[j], by[j], cx[j], cy[j], 1))
                {
                erreur=1;
                }
            j++;
            }
        if (erreur==0)
            {
            i++;
            }
        }
    PutRNGstate();
    }

/************************************/
/*hypotheses nulles pour intertype :*/
/************************************/

/*1 : etiquetage aleatoire*/
int randlabelling(double *x, double *y, int point_nb1, double *x1, double *y1, int point_nb2, double *x2, double *y2, int *type)
    {
    int j, jj;
    int erreur=0;

    GetRNGstate();

    for (j=0; j<point_nb1+point_nb2; j++)
        {
        type[j]=2;
        }
    /* on tire point_nb type 1*/
    j=0;
    while (j<point_nb1)
        {
        jj=unif_rand()*(point_nb1+point_nb2);
        while (type[jj]!=2)
            {
            jj=unif_rand()*(point_nb1+point_nb2);
            }
        type[jj]=1;
        x1[j]=x[jj];
        y1[j]=y[jj];
        j++;
        }
    PutRNGstate();
    /*Il reste point_nb2 type 2*/
    jj=0;
    for (j=0; j<point_nb1+point_nb2; j++)
        {
        if (type[j]==2)
            {
            x2[jj]=x[j];
            y2[jj]=y[j];
            jj=jj+1;
            }
        }
    if (jj!=point_nb2)
        {
        Rprintf("erreur substitution\n");
        erreur=1;
        }
    else
        {
        erreur=0;
        }

    return erreur;
    }

/*2 : decallage*/
int randshifting_rect(int *point_nb, double *x, double *y, int point_nb1, double *x1, double *y1,
                      double xmi, double xma, double ymi, double yma, double prec)
    {
    int j;
    int dx, dy;

    GetRNGstate();

    /*On decalle type 1*/
    *point_nb=point_nb1;

    /*en x d'abord*/
    dx=unif_rand()*((xma-xmi)/prec)*prec;
    for (j=0; j<*point_nb; j++)
        {
        x[j]=x1[j]+dx;
        if (x[j]>xma)
            {
            x[j]=x[j]-(xma-xmi);
            }
        }
    /*en y ensuite*/
    dy=unif_rand()*((yma-ymi)/prec)*prec;
    for (j=0; j<*point_nb; j++)
        {
        y[j]=y1[j]+dy;
        if (y[j]>yma)
            {
            y[j]=y[j]-(yma-ymi);
            }
        }

    PutRNGstate();

    return 0;
    }

int randshifting_disq(int *point_nb, double *x, double *y, int point_nb1, double *x1, double *y1,
                      double x0, double y0, double r0, double prec)
    {
    int i;

    randshifting_rect(point_nb, x, y, point_nb1, x1, y1, x0-r0, x0+r0, y0-r0, y0+r0, prec);

    /*suppression des points hors cercle*/
    i=0;
    while (i<*point_nb)
        {
        if ((x[i]-x0)*(x[i]-x0)+(y[i]-y0)*(y[i]-y0)>r0*r0)
            {
            x[i]=x[*point_nb];
            y[i]=y[*point_nb];
            i--;
            *point_nb=*point_nb-1;
            }
        i++;
        }

    return 0;
    }

int randshifting_tr_rect(int *point_nb, double *x, double *y, int point_nb1, double *x1, double *y1,
                         double xmi, double xma, double ymi, double yma, int triangle_nb, double *ax, double *ay, double *bx, double *by,
                         double *cx, double *cy, double prec)
    {
    int i, j;
    int erreur;

    randshifting_rect(point_nb, x, y, point_nb1, x1, y1, xmi, xma, ymi, yma, prec);

    /*suppression des points dans triangles*/
    i=0;
    erreur=0;
    while (i<*point_nb)
        {
        j=0;
        while ((j<triangle_nb)&&(erreur==0))
            {
            if (in_triangle(x[i], y[i], ax[j], ay[j], bx[j], by[j], cx[j], cy[j], 1)) erreur=1;
            j++;
            }
        if (erreur==1)
            {
            x[i]=x[*point_nb];
            y[i]=y[*point_nb];
            i--;
            *point_nb=*point_nb-1;
            }
        i++;
        erreur=0;
        }

    return 0;
    }

int randshifting_tr_disq(int *point_nb, double *x, double *y, int point_nb1, double *x1, double *y1,
                         double x0, double y0, double r0, int triangle_nb, double *ax, double *ay, double *bx, double *by,
                         double *cx, double *cy, double prec)
    {
    int i, j;
    int erreur;

    randshifting_disq(point_nb, x, y, point_nb1, x1, y1, x0, y0, r0, prec);

    /*suppression des points dans triangles*/
    i=0;
    erreur=0;
    while (i<*point_nb)
        {
        j=0;
        while ((j<triangle_nb)&&(erreur==0))
            {
            if (in_triangle(x[i], y[i], ax[j], ay[j], bx[j], by[j], cx[j], cy[j], 1)) erreur=1;
            j++;
            }
        if (erreur==1)
            {
            x[i]=x[*point_nb];
            y[i]=y[*point_nb];
            i--;
            *point_nb=*point_nb-1;
            }
        i++;
        erreur=0;
        }

    return 0;
    }

/******************************************************************************/
/* Calcule la fonction de corr�lation des marques Km(r) pour un semis (x,y)   */
/* affect� d'une marque quantitative c(x,y) en parametres					  */
/* dans une zone de forme rectangulaire de bornes xmi xma ymi yma             */
/* Les corrections des effets de bords sont fait par la methode de Ripley,    */
/* i.e. l'inverse de la proportion d'arc de cercle inclu dans la fenetre.     */
/* Les calculs sont faits pour les t2 premiers intervalles de largeur dt.     */
/* La routine calcule g, densite des couples de points;  et la fonction K     */
/* Les resultats sont stockes dans des tableaux g et k donnes en parametres   */

/******************************************************************************/
int corr_rect(int *point_nb, double *x, double *y, double *c, double *xmi, double *xma, double *ymi, double *yma,
              int *t2, double *dt, double *gm, double *km)
    {
    int i, j, tt;
    double d, cin, cmoy, cvar;
    double *g, *k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRect(*point_nb, x, y, xmi, xma, ymi, yma);

    /*On calcule la moyenne des marques*/
    cmoy=0;
    for (i=0; i<*point_nb; i++)
        cmoy+=c[i];
    cmoy=cmoy/(*point_nb);

    /*On calcule la variance des marques*/
    cvar=0;
    for (i=0; i<*point_nb; i++)
        cvar+=(c[i]-cmoy)*(c[i]-cmoy);
    cvar=cvar/(*point_nb);

    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    /* On rangera dans g le nombre de couples de points par distance tt
     et dans gm la somme des covariances des marques */
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        gm[tt]=0;
        }

    /*On regarde les couples (i,j) et (j,i) : donc pour i>j seulement*/
    for (i=1; i<*point_nb; i++)
        {
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                /* dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_rect(x[i], y[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_rect(x[j], y[j], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
                }
            }
        }

    /* on integre*/
    k[0]=g[0];
    km[0]=gm[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        km[tt]=km[tt-1]+gm[tt];
        }

    /* on normalise*/
    for (tt=0; tt<*t2; tt++)
        {
        gm[tt]=gm[tt]/(g[tt]*cvar);
        km[tt]=km[tt]/(k[tt]*cvar);
        }

    freevec(g);
    freevec(k);
    return 0;
    }

/*function de corr�lation dans forme circulaire*/
int corr_disq(int *point_nb, double *x, double *y, double *c, double *x0, double *y0, double *r0,
              int *t2, double *dt, double *gm, double *km)
    {
    int i, j, tt;
    double d, cin, cmoy, cvar;
    double *g, *k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCirc(*point_nb, x, y, x0, y0, *r0);

    /*On calcule la moyenne des marques*/
    cmoy=0;
    for (i=0; i<*point_nb; i++)
        cmoy+=c[i];
    cmoy=cmoy/(*point_nb);

    /*On calcule la variance des marques*/
    cvar=0;
    for (i=0; i<*point_nb; i++)
        cvar+=(c[i]-cmoy)*(c[i]-cmoy);
    cvar=cvar/(*point_nb);

    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    /*On rangera dans g le nombre de couples de points par distance tt
     et dans gm la somme des covariances des marques */
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        gm[tt]=0;
        }

    /*On regarde les couples (i,j) et (j,i) : donc pour i>j seulement*/
    for (i=1; i<*point_nb; i++)
        {
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                /* dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);

                /* pour [i,j] : correction des effets de bord*/
                cin=perim_in_disq(x[i], y[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_disq(x[j], y[j], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
                }
            }
        }

    /* on integre*/
    k[0]=g[0];
    km[0]=gm[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        km[tt]=km[tt-1]+gm[tt];
        }

    /* on normalise*/
    for (tt=0; tt<*t2; tt++)
        {
        gm[tt]=gm[tt]/(g[tt]*cvar);
        km[tt]=km[tt]/(k[tt]*cvar);
        }

    freevec(g);
    freevec(k);
    return 0;
    }

/*Kcor triangles dans rectangle*/
int corr_tr_rect(int *point_nb, double *x, double *y, double *c, double *xmi, double *xma, double *ymi, double *yma,
                 int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                 int *t2, double *dt, double *gm, double *km)
    {
    int i, j, tt;
    double d, cin, cmoy, cvar;
    double *g, *k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalRectTri(*point_nb, x, y, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);

    /*On calcule la moyenne des marques*/
    cmoy=0;
    for (i=0; i<*point_nb; i++)
        cmoy+=c[i];
    cmoy=cmoy/(*point_nb);

    /*On calcule la variance des marques*/
    cvar=0;
    for (i=0; i<*point_nb; i++)
        cvar+=(c[i]-cmoy)*(c[i]-cmoy);
    cvar=cvar/(*point_nb);

    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    /*On rangera dans g le nombre de couples de points par distance tt
     et dans gm la somme des covariances des marques */
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        gm[tt]=0;
        }

    /*On regarde les couples (i,j) et (j,i) : donc pour i>j seulement*/
    for (i=1; i<*point_nb; i++)
        {
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                /*dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);

                /*pour [i,j] : correction des effets de bord*/
                cin=perim_in_rect(x[i], y[i], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[i], y[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_rect(x[j], y[j], d, *xmi, *xma, *ymi, *yma);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[j], y[j], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
                }
            }
        }


    /*on integre*/
    k[0]=g[0];
    km[0]=gm[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        km[tt]=km[tt-1]+gm[tt];
        }

    /* on normalise*/
    for (tt=0; tt<*t2; tt++)
        {
        gm[tt]=gm[tt]/(g[tt]*cvar);
        km[tt]=km[tt]/(k[tt]*cvar);
        }

    freevec(g);
    freevec(k);
    return 0;
    }

/*kcor triangles dans disque*/
int corr_tr_disq(int *point_nb, double *x, double *y, double *c, double *x0, double *y0, double *r0,
                 int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                 int *t2, double *dt, double *gm, double *km)
    {
    int i, j, tt;
    double d, cin, cmoy, cvar;
    double *g, *k;

    /*Decalage pour n'avoir que des valeurs positives*/
    decalCircTri(*point_nb, x, y, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);

    /*On calcule la moyenne des marques*/
    cmoy=0;
    for (i=0; i<*point_nb; i++)
        cmoy+=c[i];
    cmoy=cmoy/(*point_nb);

    /*On calcule la variance des marques*/
    cvar=0;
    for (i=0; i<*point_nb; i++)
        cvar+=(c[i]-cmoy)*(c[i]-cmoy);
    cvar=cvar/(*point_nb);

    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    /* On rangera dans g le nombre de couples de points par distance tt
     et dans gm la somme des covariances des marques */
    for (tt=0; tt<*t2; tt++)
        {
        g[tt]=0;
        gm[tt]=0;
        }

    /*On regarde les couples (i,j) et (j,i) : donc pour i>j seulement*/
    for (i=1; i<*point_nb; i++)
        {
        for (j=0; j<i; j++)
            {
            d=sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
            if (d<*t2*(*dt))
                {
                /* dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);

                /* pour [i,j] : correction des effets de bord*/
                cin=perim_in_disq(x[i], y[i], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur i AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[i], y[i], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;

                /*pour [j,i] : correction des effets de bord*/
                cin=perim_in_disq(x[j], y[j], d, *x0, *y0, *r0);
                if (cin<0)
                    {
                    Rprintf("cin<0 sur j AVANT\n");
                    return -1;
                    }
                cin=cin-perim_triangle(x[j], y[j], d, *triangle_nb, ax, ay, bx, by, cx, cy);
                if (cin<0)
                    {
                    Rprintf("Overlapping triangles\n");
                    return -1;
                    }
                g[tt]+=2*Pi()/cin;
                gm[tt]+=(c[i]-cmoy)*(c[j]-cmoy)*2*Pi()/cin;
                }
            }
        }

    /* on integre*/
    k[0]=g[0];
    km[0]=gm[0];
    for (tt=1; tt<*t2; tt++)
        {
        k[tt]=k[tt-1]+g[tt];
        km[tt]=km[tt-1]+gm[tt];
        }

    /* on normalise*/
    for (tt=0; tt<*t2; tt++)
        {
        gm[tt]=gm[tt]/(g[tt]*cvar);
        km[tt]=km[tt]/(k[tt]*cvar);
        }

    freevec(g);
    freevec(k);
    return 0;
    }

/*Kcor dans rectangle + ic*/
int corr_rect_ic(int *point_nb, double *x, double *y, double *c, double *xmi, double *xma, double *ymi, double *yma,
                 int *t2, double *dt, int *nbSimu, double *lev, double *gm, double *km,
                 double *gmic1, double *gmic2, double *kmic1, double *kmic2, double *gmval, double *kmval)
    {
    int i, j, i0, i1, i2;
    double *c2;
    double **gmic, **kmic;
    double *ggm, *kkm;

    int erreur=0;

    erreur=corr_rect(point_nb, x, y, c, xmi, xma, ymi, yma, t2, dt, gm, km);
    if (erreur!=0)
        {
        return -1;
        }

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gmic, *t2+1, 2*i0+10+1);
    taballoc(&kmic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&ggm, *t2);
    vecalloc(&kkm, *t2);
    for (i=0; i<*t2; i++)
        {
        ggm[i]=gm[i];
        kkm[i]=km[i];

        gmval[i]=1;
        kmval[i]=1;
        }

    int lp=0;
    vecalloc(&c2, *point_nb);
    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        randmark(*point_nb, c, c2);
        erreur=corr_rect(point_nb, x, y, c2, xmi, xma, ymi, yma, t2, dt, gmic1, kmic1);
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR mark correlation\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gmictmp, kmictmp;
            for (j=0; j<*t2; j++)
                {
                gmictmp=gmic1[j];
                kmictmp=kmic1[j];
                if ((float) fabs(ggm[j]-1)<=(float) fabs(gmictmp-1))
                    {
                    gmval[j]+=1;
                    }
                if ((float) fabs(kkm[j])<=(float) fabs(kmictmp))
                    {
                    kmval[j]+=1;
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gmic, kmic, gmic1, kmic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gmic1[i]=gmic[i+1][i1];
        gmic2[i]=gmic[i+1][i2];
        kmic1[i]=kmic[i+1][i1];
        kmic2[i]=kmic[i+1][i2];
        }


    freetab(gmic);
    freetab(kmic);
    freevec(ggm);
    freevec(kkm);
    freevec(c2);
    return 0;
    }

/*Kcor dans disque + ic*/
int corr_disq_ic(int *point_nb, double *x, double *y, double *c, double *x0, double *y0, double *r0,
                 int *t2, double *dt, int *nbSimu, double *lev, double *gm, double *km,
                 double *gmic1, double *gmic2, double *kmic1, double *kmic2, double *gmval, double *kmval)
    {
    int i, j, i0, i1, i2;
    double *c2;
    double **gmic, **kmic;
    double *ggm, *kkm;

    int erreur=0;

    erreur=corr_disq(point_nb, x, y, c, x0, y0, r0, t2, dt, gm, km);
    if (erreur!=0)
        {
        return -1;
        }

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gmic, *t2+1, 2*i0+10+1);
    taballoc(&kmic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&ggm, *t2);
    vecalloc(&kkm, *t2);
    for (i=0; i<*t2; i++)
        {
        ggm[i]=gm[i];
        kkm[i]=km[i];

        gmval[i]=1;
        kmval[i]=1;
        }

    int lp=0;
    vecalloc(&c2, *point_nb);
    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        randmark(*point_nb, c, c2);
        erreur=corr_disq(point_nb, x, y, c2, x0, y0, r0, t2, dt, gmic1, kmic1);
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR mark correlation\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gmictmp, kmictmp;
            for (j=0; j<*t2; j++)
                {
                gmictmp=gmic1[j];
                kmictmp=kmic1[j];
                if ((float) fabs(ggm[j]-1)<=(float) fabs(gmictmp-1))
                    {
                    gmval[j]+=1;
                    }
                if ((float) fabs(kkm[j])<=(float) fabs(kmictmp))
                    {
                    kmval[j]+=1;
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gmic, kmic, gmic1, kmic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gmic1[i]=gmic[i+1][i1];
        gmic2[i]=gmic[i+1][i2];
        kmic1[i]=kmic[i+1][i1];
        kmic2[i]=kmic[i+1][i2];
        }


    freetab(gmic);
    freetab(kmic);
    freevec(ggm);
    freevec(kkm);
    freevec(c2);
    return 0;
    }

/*Kcor triangles dans rectangle + ic*/
int corr_tr_rect_ic(int *point_nb, double *x, double *y, double *c, double *xmi, double *xma, double *ymi, double *yma,
                    int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                    int *t2, double *dt, int *nbSimu, double *lev, double *gm, double *km,
                    double *gmic1, double *gmic2, double *kmic1, double *kmic2, double *gmval, double *kmval)
    {
    int i, j, i0, i1, i2;
    double *c2;
    double **gmic, **kmic;
    double *ggm, *kkm;

    int erreur=0;

    erreur=corr_tr_rect(point_nb, x, y, c, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gm, km);
    if (erreur!=0)
        {
        return -1;
        }

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gmic, *t2+1, 2*i0+10+1);
    taballoc(&kmic, *t2+1, 2*i0+10+1);

    /*Normalisation de g et k et calcul de l et n pour le calcul des p-values*/
    vecalloc(&ggm, *t2);
    vecalloc(&kkm, *t2);
    for (i=0; i<*t2; i++)
        {
        ggm[i]=gm[i];
        kkm[i]=km[i];

        gmval[i]=1;
        kmval[i]=1;
        }

    int lp=0;
    vecalloc(&c2, *point_nb);
    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        randmark(*point_nb, c, c2);
        erreur=corr_tr_rect(point_nb, x, y, c2, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gmic1, kmic1);
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Intertype\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gmictmp, kmictmp;
            for (j=0; j<*t2; j++)
                {
                gmictmp=gmic1[j];
                kmictmp=kmic1[j];
                if ((float) fabs(ggm[j]-1)<=(float) fabs(gmictmp-1))
                    {
                    gmval[j]+=1;
                    }
                if ((float) fabs(kkm[j])<=(float) fabs(kmictmp))
                    {
                    kmval[j]+=1;
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gmic, kmic, gmic1, kmic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gmic1[i]=gmic[i+1][i1];
        gmic2[i]=gmic[i+1][i2];
        kmic1[i]=kmic[i+1][i1];
        kmic2[i]=kmic[i+1][i2];
        }


    freetab(gmic);
    freetab(kmic);
    freevec(ggm);
    freevec(kkm);
    freevec(c2);
    return 0;
    }

/*Kcor triangles dans disque + ic*/
int corr_tr_disq_ic(int *point_nb, double *x, double *y, double *c, double *x0, double *y0, double *r0,
                    int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                    int *t2, double *dt, int *nbSimu, double *lev, double *gm, double *km,
                    double *gmic1, double *gmic2, double *kmic1, double *kmic2, double *gmval, double *kmval)
    {
    int i, j, i0, i1, i2;
    double *c2;
    double **gmic, **kmic;
    double *ggm, *kkm;

    int erreur=0;

    erreur=corr_tr_disq(point_nb, x, y, c, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gm, km);
    if (erreur!=0)
        {
        return -1;
        }

    /*D�finition de i0 : indice o� sera stock� l'estimation des bornes de l'IC*/
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
    taballoc(&gmic, *t2+1, 2*i0+10+1);
    taballoc(&kmic, *t2+1, 2*i0+10+1);

    /*Calcul de gm et km pour le calcul des p-values*/
    vecalloc(&ggm, *t2);
    vecalloc(&kkm, *t2);
    for (i=0; i<*t2; i++)
        {
        ggm[i]=gm[i];
        kkm[i]=km[i];

        gmval[i]=1;
        kmval[i]=1;
        }

    int lp=0;
    vecalloc(&c2, *point_nb);
    /* boucle principale de MC*/
    Rprintf("Monte Carlo simulation\n");
    for (i=1; i<=*nbSimu; i++)
        {
        randmark(*point_nb, c, c2);
        erreur=corr_tr_disq(point_nb, x, y, c2, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gmic1, kmic1);
        /* si il y a une erreur on recommence une simulation*/
        if (erreur!=0)
            {
            i=i-1;
            Rprintf("ERREUR Intertype\n");
            }
        else
            {
            /*comptage du nombre de |�obs|<=|�simu| pour test local*/
            double gmictmp, kmictmp;
            for (j=0; j<*t2; j++)
                {
                gmictmp=gmic1[j];
                kmictmp=kmic1[j];
                if ((float) fabs(ggm[j]-1)<=(float) fabs(gmictmp-1))
                    {
                    gmval[j]+=1;
                    }
                if ((float) fabs(kkm[j])<=(float) fabs(kmictmp))
                    {
                    kmval[j]+=1;
                    }
                }

            /*Traitement des r�sultats*/
            ic(i, i0, gmic, kmic, gmic1, kmic1, *t2);
            }
        R_FlushConsole();
        progress(i, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    /*Copies des valeurs dans les tableaux r�sultats*/
    for (i=0; i<*t2; i++)
        {
        gmic1[i]=gmic[i+1][i1];
        gmic2[i]=gmic[i+1][i2];
        kmic1[i]=kmic[i+1][i1];
        kmic2[i]=kmic[i+1][i2];
        }


    freetab(gmic);
    freetab(kmic);
    freevec(ggm);
    freevec(kkm);
    freevec(c2);
    return 0;
    }

/*mark permutations*/
//old version
/*void randmark2(int point_nb,double *c,double *c2)
{   int j,jj;

        for(j=0;j<point_nb;j++)
                c2[j]=-1;
        j=0;
        GetRNGstate();
        while (j<point_nb) {
                jj=unif_rand()*(point_nb);
                while (c2[jj]>-1) {
                        jj=unif_rand()*(point_nb);
                }
                c2[jj]=c[j];
                j++;
        }
        PutRNGstate();
}*/

//new version D. Redondo 2013

void randmark(int point_nb, double *c, double *c2)
    {
    int j, jj, i;
    double temp;
    for (i=0; i<point_nb; i++)
        {
        c2[i]=c[i];
        }
    GetRNGstate();

    for (j=0; j<point_nb; j++)
        {
        jj=unif_rand()*point_nb;
        temp=c2[j];
        c2[j]=c2[jj];
        c2[jj]=temp;
        }
    PutRNGstate();
    }


/*********************************************************************************************/
/* Fonctions de Shimatani pour les semis de points multivari�s + fonctions utilitaires      */
/********************************************************************************************/
/// fonction de shimatani pour une zone rectangulaire

int shimatani_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi,
                   double *yma, int *t2, double *dt, int *nbtype, int *type, double *surface, double *gs, double *ks, int *error)
    {
    int i, j, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;

    double *g;
    double *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                {
                l[i]++;
                }
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii;
    double *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg;
    double *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0)
        {
        Rprintf("ERREUR 0 Ripley\n");
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=((intensity*intensity)*(g[j])/intensity*ds[j]);
        //	g[j]=intensity*g[j]/ds[j];
        k[j]=(intensity*(k[j]));
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        {
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_rect(&l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, t2, dt, g, k);
            if (erreur!=0)
                {
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+((intensity1*intensity1)*(g[j])/intensity1*ds[j]);
                //	gii[j]+=intensity1*g[j]/ds[j];

                kii[j]=kii[j]+(intensity1*(k[j]));
                }

            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }
        }

    for (i=0; i < *nbtype; i++)
        free(xx[i]);
    free(xx);
    for (i=0; i < *nbtype; i++)
        free(yy[i]);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int shimatani_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                   int *t2, double *dt, int *nbtype, int *type, double *surface, double *gs, double *ks, int *error)
    {
    int i, j, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*intensity*g[j]/intensity*ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        {
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_disq(&l[i+1], xx[i], yy[i], x0, y0, r0, t2, dt, g, k);
            if (erreur!=0)
                Rprintf("ERREUR 1 Ripley\n");
            intensity1=l[i+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+intensity1*intensity1*g[j]/intensity1*ds[j];
                kii[j]=kii[j]+intensity1*k[j];
                }
            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }
        }

    for (i=0; i < *nbtype; i++)
        free(xx[i]);
    free(xx);
    for (i=0; i < *nbtype; i++)
        free(yy[i]);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int shimatani_tr_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi,
                      double *yma, int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                      int *t2, double *dt, int *nbtype, int *type, double *surface, double *gs, double *ks, int *error)
    {
    int i, j, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;

    double *g;
    double *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                {
                l[i]++;
                }
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii;
    double *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg;
    double *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        {
        Rprintf("ERREUR 0 Ripley\n");
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*intensity*g[j]/intensity*ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        {
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_tr_rect(&l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
            if (erreur!=0)
                {
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+intensity1*intensity1*g[j]/intensity1*ds[j];
                kii[j]=kii[j]+intensity1*k[j];
                }
            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }
        }

    for (i=0; i < *nbtype; i++)
        free(xx[i]);
    free(xx);
    for (i=0; i < *nbtype; i++)
        free(yy[i]);
    free(yy);
    free(g);
    free(k);
    free(l);
    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int shimatani_tr_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                      int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                      int *t2, double *dt, int *nbtype, int *type, double *surface, double *gs, double *ks, int *error)
    {
    int i, j, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;

    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*intensity*g[j]/intensity*ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        {
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_tr_disq(&l[i+1], xx[i], yy[i], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
            if (erreur!=0)
                {
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+intensity1*intensity1*g[j]/intensity1*ds[j];
                kii[j]=kii[j]+intensity1*k[j];
                }
            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }
        }

    for (i=0; i < *nbtype; i++)
        free(xx[i]);
    free(xx);
    for (i=0; i < *nbtype; i++)
        free(yy[i]);
    free(yy);
    free(g);
    free(k);
    free(l);
    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int shimatani_rect_ic(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                      int *t2, double *dt, int *nbSimu, double *lev,
                      int *nbtype, int *type, double *surface, double *D,
                      double *gs, double *ks, double *gic1, double *gic2, double *kic1, double *kic2,
                      double *gval, double *kval, int *error)
    {
    int i, j, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    //pour tous les points
    erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0)
        {
        Rprintf("ERREUR 0 Ripley\n");
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*intensity*g[j]/intensity*ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        { //boucle initiale
        //pour chaque esp�ce
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_rect(&l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, t2, dt, g, k);
            if (erreur!=0)
                {
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/ *(surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+((intensity1*intensity1)*(g[j])/intensity1*ds[j]);
                kii[j]=kii[j]+(intensity1*(k[j]));
                }
            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }

        //simulations sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        //Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        //Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);
        double *gsic, *ksic;
        vecalloc(&gsic, *t2);
        vecalloc(&ksic, *t2);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;

        //boucle principale de MC
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=0; i<*nbtype; i++)
                {
                erreur=ripley_rect(&l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, t2, dt, gic1, kic1);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Ripley\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+((intensity1*intensity1)*(gic1[j])/intensity1*ds[j]);
                    kii[j]=kii[j]+(intensity1*(kic1[j]));
                    }

                }

            for (j=0; j<*t2; j++)
                {
                gii[j]=1-gii[j]/tabg[j];
                kii[j]=1-kii[j]/tabk[j];
                if ((float) fabs(gs[j]-*D)<=(float) fabs(gii[j]-*D))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(ks[j]-*D)<=(float) fabs(kii[j]-*D))
                    {
                    kval[j]+=1;
                    }
                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }
        free(gsic);
        free(ksic);
        free(gic);
        free(kic);
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    free(gii);
    free(kii);
    free(ds);

    return 0;
    }

int shimatani_disq_ic(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                      int *t2, double *dt, int *nbSimu, double *lev,
                      int *nbtype, int *type, double *surface, double *D,
                      double *gs, double *ks,
                      double *gic1, double *gic2, double *kic1, double *kic2,
                      double *gval, double *kval, int *error)
    {
    int i, j, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    //pour tous les points
    erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*intensity*g[j]/intensity*ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }

    if (z==0)
        { //boucle initiale
        //pour chaque esp�ce
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_disq(&l[i+1], xx[i], yy[i], x0, y0, r0, t2, dt, g, k);
            if (erreur!=0)
                {
                i=i-1;
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/ *(surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+intensity1*intensity1*g[j]/intensity1*ds[j];
                kii[j]=kii[j]+intensity1*k[j];
                }

            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }

        //simulaitons sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;

        /*boucle principale de MC*/
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=0; i<*nbtype; i++)
                {
                erreur=ripley_disq(&l[i+1], xx[i], yy[i], x0, y0, r0, t2, dt, gic1, kic1);
                if (erreur!=0)
                    {
                    Rprintf("ERREUR 2 Ripley\n");
                    }
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+((intensity1*intensity1)*(gic1[j])/intensity1*ds[j]);
                    kii[j]=kii[j]+(intensity1*(kic1[j]));
                    }
                }

            for (j=0; j<*t2; j++)
                {
                gii[j]=1-gii[j]/tabg[j];
                kii[j]=1-kii[j]/tabk[j];
                if ((float) fabs(gs[j]-*D)<=(float) fabs(gii[j]-*D)) gval[j]+=1;
                if ((float) fabs(ks[j]-*D)<=(float) fabs(kii[j]-*D)) kval[j]+=1;
                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }
        free(gic);
        free(kic);
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);
    free(tabg);
    free(tabk);
    free(gii);
    free(kii);
    free(ds);

    return 0;
    }

int shimatani_tr_rect_ic(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi,
                         double *yma, int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                         int *t2, double *dt, int *nbSimu, double *lev,
                         int *nbtype, int *type, double *surface, double *D,
                         double *gs, double *ks, double *gic1, double *gic2, double *kic1, double *kic2,
                         double *gval, double *kval, int *error)
    {
    int i, j, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;

    double *g;
    double *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                {
                l[i]++;
                }
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii;
    double *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg;
    double *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    //pour tous les points
    erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        {
        Rprintf("ERREUR 0 Ripley\n");
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=((intensity*intensity)*(g[j])/intensity*ds[j]);
        k[j]=(intensity*(k[j]));
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        { //boucle initiale
        //pour chaque esp�ce
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_tr_rect(&l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
            if (erreur!=0)
                {
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/ *(surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+((intensity1*intensity1)*(g[j])/intensity1*ds[j]);
                kii[j]=kii[j]+(intensity1*(k[j]));
                }
            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }

        //simulaitons sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);
        double *gsic, *ksic;
        vecalloc(&gsic, *t2);
        vecalloc(&ksic, *t2);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;

        /*boucle principale de MC*/
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=0; i<*nbtype; i++)
                {
                erreur=ripley_tr_rect(&l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
                if (erreur!=0)
                    {
                    Rprintf("ERREUR 2 Ripley\n");
                    }
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+((intensity1*intensity1)*(gic1[j])/intensity1*ds[j]);
                    kii[j]=kii[j]+(intensity1*(kic1[j]));
                    }

                }

            for (j=0; j<*t2; j++)
                {
                gii[j]=1-gii[j]/tabg[j];
                kii[j]=1-kii[j]/tabk[j];
                if ((float) fabs(gs[j]-*D)<=(float) fabs(gii[j]-*D))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(ks[j]-*D)<=(float) fabs(kii[j]-*D))
                    {
                    kval[j]+=1;
                    }
                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }

        free(gsic);
        free(ksic);
        free(gic);
        free(kic);
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);
    free(tabg);
    free(tabk);
    free(gii);
    free(kii);
    free(ds);

    return 0;
    }

int shimatani_tr_disq_ic(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                         int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                         int *t2, double *dt, int *nbSimu, double *lev,
                         int *nbtype, int *type, double *surface, double *D,
                         double *gs, double *ks,
                         double *gic1, double *gic2, double *kic1, double *kic2,
                         double *gval, double *kval, int *error)
    {
    int i, j, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds;

    double *g;
    double *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                {
                l[i]++;
                }
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1;
    double intensity=*point_nb/(*surface);
    double *gii;
    double *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg;
    double *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    //pour tous les points
    erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        {
        Rprintf("ERREUR 0 Ripley\n");
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=((intensity*intensity)*(g[j])/intensity*ds[j]);
        k[j]=(intensity*(k[j]));
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        }
    if (z==0)
        { //boucle initiale
        //pour chaque esp�ce
        for (i=0; i<*nbtype; i++)
            {
            erreur=ripley_tr_disq(&l[i+1], xx[i], yy[i], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
            if (erreur!=0)
                {
                Rprintf("ERREUR 1 Ripley\n");
                }
            intensity1=l[i+1]/ *(surface);
            for (j=0; j<*t2; j++)
                {
                gii[j]=gii[j]+((intensity1*intensity1)*(g[j])/intensity1*ds[j]);
                kii[j]=kii[j]+(intensity1*(k[j]));
                }
            }

        for (j=0; j<*t2; j++)
            {
            gs[j]=1-gii[j]/tabg[j];
            ks[j]=1-kii[j]/tabk[j];
            }

        //simulaitons sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);
        double *gsic, *ksic;
        vecalloc(&gsic, *t2);
        vecalloc(&ksic, *t2);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;

        /*boucle principale de MC*/
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=0; i<*nbtype; i++)
                {
                erreur=ripley_tr_disq(&l[i+1], xx[i], yy[i], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
                if (erreur!=0)
                    {
                    Rprintf("ERREUR 2 Ripley\n");
                    }
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+((intensity1*intensity1)*(gic1[j])/intensity1*ds[j]);
                    kii[j]=kii[j]+(intensity1*(kic1[j]));
                    }

                }

            for (j=0; j<*t2; j++)
                {
                gii[j]=1-gii[j]/tabg[j];
                kii[j]=1-kii[j]/tabk[j];
                if ((float) fabs(gs[j]-*D)<=(float) fabs(gii[j]-*D))
                    {
                    gval[j]+=1;
                    }
                if ((float) fabs(ks[j]-*D)<=(float) fabs(kii[j]-*D))
                    {
                    kval[j]+=1;
                    }
                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }

        free(gsic);
        free(ksic);
        free(gic);
        free(kic);
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);
    free(tabg);
    free(tabk);
    free(gii);
    free(kii);
    free(ds);

    return 0;
    }

/*permutation of multivariate marks*/
int randomlab(double *x, double *y, int total_nb, int *type, int nb_type, double **xx, int *taille_xy, double **yy)
    {
    int j, jj;
    int erreur=0;

    GetRNGstate();

    for (j=0; j<total_nb; j++)
        {
        type[j]=nb_type;
        }
    j=0;
    int i=0;

    while (i<nb_type-1)
        {
        //Rprintf("taille :%d\n",taille_xy[i+1]);
        while (j<taille_xy[i+1])
            {
            jj=unif_rand()*(total_nb);
            while (type[jj]!=nb_type)
                {
                jj=unif_rand()*(total_nb);
                }
            type[jj]=i+1;
            xx[i][j]=x[jj];
            yy[i][j]=y[jj];
            j++;
            }
        j=0;
        i++;
        }

    PutRNGstate();

    jj=0;
    for (j=0; j<total_nb; j++)
        {
        if (type[j]==nb_type)
            {
            xx[nb_type-1][jj]=x[j];
            yy[nb_type-1][jj]=y[j];
            jj=jj+1;
            }
        }
    if (jj!=taille_xy[nb_type])
        {
        //Rprintf("jj : %d ::: taille : %d\n",jj,taille_xy[nb_type]);
        erreur=1;
        }
    else
        {
        erreur=0;
        }
    return erreur;
    }

/**********************************************************/
/* Fonctions de Rao pour les semis de points multivari�s  */
/**********************************************************/
//V2 as wrapper of K12fun - 08/2013

int rao_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
             int *t2, double *dt, int *h0, int *nbtype, int *type, double *mat, double *surface, double *HD, double *gr, double *kr, double *gs, double *ks, int *error)
    {
    int i, j, p, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;

    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }
    //Ripley all points	
    erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pair of types
    if (z==0)
        {
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_rect(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], xmi, xma, ymi, yma, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_rect(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }

        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }
        }

    for (i=0; i<*nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int rao_rect_ic(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                int *t2, double *dt, int *nbSimu, int *h0, double *lev,
                int *nbtype, int *type, double *mat, double *surface, double *HD,
                double *gr, double *kr, double *gs, double *ks, double *gic1, double *gic2, double *kic1, double *kic2,
                double *gval, double *kval, int *error)
    {
    int i, j, p, b, z=0;
    int *l;
    int compt[*nbtype+1];
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    // l contient le nombre d'arbres par esp�ce
    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    // cr�ation des tableaux xx et yy par esp�ce
    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    //Ripley for all points	
    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }
    //standardization de Ripley (et Shimatani si H0=2)
    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }

    //Intertype for each pairs of types
    if (z==0)
        {
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_rect(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], xmi, xma, ymi, yma, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_rect(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }

        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }

        //simulations sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        //Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        //Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }
        //pr�paration des tableaux pour randomisation de dis (H0=2)
        int lp=0;
        double HDsim;
        int *vec, mat_size;
        vecintalloc(&vec, *nbtype);
        double *matp;
        mat_size=*nbtype*(*nbtype-1)/2;
        vecalloc(&matp, mat_size);
        if (*h0==2)
            {
            for (i=0; i<*nbtype; i++)
                vec[i]=i;
            for (i=0; i<mat_size; i++)
                matp[i]=0;
            }

        //boucle principale de MC
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            if (*h0==1) //random labelling
                randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            if (*h0==2) //distance randomisation
                randomdist(vec, *nbtype, mat, matp);
            HDsim=0;
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=1; i<*nbtype; i++)
                {
                for (p=0; p<i; p++)
                    {
                    if (*h0==1)
                        dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                    else // (*h0==2)
                        {
                        dis=matp[p*(*nbtype-2)-(p-1)*p/2+i-1];
                        HDsim+=(float) l[i+1]/(float) (*point_nb)*(float) l[p+1]/(float) (*point_nb)*dis;
                        }
                    erreur=intertype_rect(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], xmi, xma, ymi, yma, t2, dt, gic1, kic1);
                    if (erreur!=0)
                        Rprintf("ERREUR 1 Intertype\n");
                    intensity1=l[i+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity1*gic1[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity1*kic1[j];
                        }
                    erreur=intertype_rect(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, t2, dt, gic1, kic1);
                    if (erreur!=0)
                        Rprintf("ERREUR 2 Intertype\n");
                    intensity2=l[p+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity2*gic1[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity2*kic1[j];
                        }
                    }
                }

            for (j=0; j<*t2; j++)
                {
                if (*h0==1)
                    {// standardisation by HD when species labels are randomized
                    gii[j]=gii[j]/(tabg[j]*(*HD));
                    kii[j]=kii[j]/(tabk[j]*(*HD));
                    //deviation from theo=1
                    if ((float) fabs(gr[j]-1)<=(float) fabs(gii[j]-1)) gval[j]+=1;
                    if ((float) fabs(kr[j]-1)<=(float) fabs(kii[j]-1)) kval[j]+=1;
                    }
                if (*h0==2)
                    {// standardisation by Hsim when distance matrix is randomized
                    gii[j]=gii[j]/(tabg[j]*2*HDsim);
                    kii[j]=kii[j]/(tabk[j]*2*HDsim);
                    //deviation from theo=shimatani
                    if ((float) fabs(gr[j]-gs[j])<=(float) fabs(gii[j]-gs[j])) gval[j]+=1;
                    if ((float) fabs(kr[j]-ks[j])<=(float) fabs(kii[j]-ks[j])) kval[j]+=1;
                    }
                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }
        free(gic);
        free(kic);
        freeintvec(vec);
        freevec(matp);
        }


    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);


    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);
    return 0;
    }

int rao_disq(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
             int *t2, double *dt, int *h0, int *nbtype, int *type, double *mat, double *surface, double *HD, double *gr, double *kr, double *gs, double *ks, int *error)
    {
    int i, j, p, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;

    double *g;
    double *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);

    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }
    //Ripley all points	
    erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_disq(point_nb, x, y, x0, y0, r0, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pair of types
    if (z==0)
        {
        for (i=0; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_disq(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], x0, y0, r0, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_disq(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], x0, y0, r0, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }
        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }
        }

    for (i=0; i<*nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int rao_disq_ic(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                int *t2, double *dt, int *nbSimu, int *h0, double *lev,
                int *nbtype, int *type, double *mat, double *surface, double *HD,
                double *gr, double *kr, double *gs, double *ks, double *gic1, double *gic2, double *kic1, double *kic2,
                double *gval, double *kval, int *error)
    {
    int i, j, p, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx;
    double **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    //Ripley for all points	
    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_disq(point_nb, x, y, x0, y0, r0, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }
    //standardization de Ripley (et Shimatani si H0=2)
    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pair of types
    if (z==0)
        {
        for (i=0; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_disq(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], x0, y0, r0, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_disq(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], x0, y0, r0, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }
        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }

        //simulaitons sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;
        double HDsim;
        int *vec, mat_size;
        vecintalloc(&vec, *nbtype);
        double *matp;
        mat_size=*nbtype*(*nbtype-1)/2;
        vecalloc(&matp, mat_size);
        if (*h0==2)
            {
            for (i=0; i<*nbtype; i++)
                vec[i]=i;
            for (i=0; i<mat_size; i++)
                matp[i]=0;
            }

        /*boucle principale de MC*/
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            if (*h0==1) //random labelling
                randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            if (*h0==2) //distance randomisation
                randomdist(vec, *nbtype, mat, matp);
            HDsim=0;
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=0; i<*nbtype; i++)
                {
                for (p=0; p<i; p++)
                    {
                    if (*h0==1)
                        dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                    else // (*h0==2)
                        {
                        dis=matp[p*(*nbtype-2)-(p-1)*p/2+i-1];
                        HDsim+=(float) l[i+1]/(float) (*point_nb)*(float) l[p+1]/(float) (*point_nb)*dis;
                        }
                    erreur=intertype_disq(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], x0, y0, r0, t2, dt, g, k);
                    if (erreur!=0)
                        Rprintf("ERREUR 1 Intertype\n");
                    intensity1=l[i+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity1*k[j];
                        }
                    erreur=intertype_disq(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], x0, y0, r0, t2, dt, g, k);
                    if (erreur!=0)
                        Rprintf("ERREUR 2 Intertype\n");
                    intensity2=l[p+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity2*k[j];
                        }
                    }
                }
            for (j=0; j<*t2; j++)
                {
                if (*h0==1)
                    {// standardisation by HD when species labels are randomized
                    gii[j]=gii[j]/(tabg[j]*(*HD));
                    kii[j]=kii[j]/(tabk[j]*(*HD));
                    //deviation from theo=1
                    if ((float) fabs(gr[j]-1)<=(float) fabs(gii[j]-1)) gval[j]+=1;
                    if ((float) fabs(kr[j]-1)<=(float) fabs(kii[j]-1)) kval[j]+=1;
                    }
                if (*h0==2)
                    {// standardisation by Hsim when distance matrix is randomized
                    gii[j]=gii[j]/(tabg[j]*2*HDsim);
                    kii[j]=kii[j]/(tabk[j]*2*HDsim);
                    //deviation from theo=shimatani
                    if ((float) fabs(gr[j]-gs[j])<=(float) fabs(gii[j]-gs[j])) gval[j]+=1;
                    if ((float) fabs(kr[j]-ks[j])<=(float) fabs(kii[j]-ks[j])) kval[j]+=1;
                    }

                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }
        free(gic);
        free(kic);
        freeintvec(vec);
        freevec(matp);
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int rao_tr_rect(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi,
                double *yma, int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                int *t2, double *dt, int *h0, int *nbtype, int *type, double *mat, double *surface, double *HD, double *gr, double *kr, double *gs, double *ks, int *error)
    {
    int i, j, p, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;

    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }
    //Ripley all points	
    erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pair of types
    if (z==0)
        {
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_tr_rect(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_tr_rect(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }

        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

/***************/
int rao_tr_disq(int *point_nb, double *x, double *y, double *x0, double *y0,
                double *r0, int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                int *t2, double *dt, int *h0, int *nbtype, int *type, double *mat, double *surface, double *HD, double *gr, double *kr, double *gs, double *ks, int *error)
    {
    int i, j, p, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }
    //Ripley all points	
    erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pair of types
    if (z==0)
        {
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_tr_disq(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_tr_disq(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }
        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }
        }

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int rao_tr_rect_ic(int *point_nb, double *x, double *y, double *xmi, double *xma, double *ymi, double *yma,
                   int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                   int *t2, double *dt, int *nbSimu, int *h0, double *lev, int *nbtype, int *type, double *mat, double *surface, double *HD,
                   double *gr, double *kr, double *gs, double *ks, double *gic1, double *gic2, double *kic1, double *kic2,
                   double *gval, double *kval, int *error)
    {
    int i, j, p, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    //Ripley for all points	
    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }
    //Ripley all points	
    erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pairs of types
    if (z==0)
        {
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_tr_rect(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_tr_rect(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }

        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }

        //simulaitons sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;
        double HDsim;
        int *vec, mat_size;
        vecintalloc(&vec, *nbtype);
        double *matp;
        mat_size=*nbtype*(*nbtype-1)/2;
        vecalloc(&matp, mat_size);
        if (*h0==2)
            {
            for (i=0; i<*nbtype; i++)
                vec[i]=i;
            for (i=0; i<mat_size; i++)
                matp[i]=0;
            }

        /*boucle principale de MC*/
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            if (*h0==1) //random labelling
                randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            if (*h0==2) //distance randomisation
                randomdist(vec, *nbtype, mat, matp);
            HDsim=0;

            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=1; i<*nbtype; i++)
                {
                for (p=0; p<i; p++)
                    {
                    if (*h0==1)
                        dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                    else // (*h0==2)
                        {
                        dis=matp[p*(*nbtype-2)-(p-1)*p/2+i-1];
                        HDsim+=(float) l[i+1]/(float) (*point_nb)*(float) l[p+1]/(float) (*point_nb)*dis;
                        }
                    erreur=intertype_tr_rect(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
                    if (erreur!=0)
                        Rprintf("ERREUR 1 Intertype\n");
                    intensity1=l[i+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity1*gic1[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity1*kic1[j];
                        }
                    erreur=intertype_tr_rect(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
                    if (erreur!=0)
                        Rprintf("ERREUR 2 Intertype\n");
                    intensity2=l[p+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity2*gic1[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity2*kic1[j];
                        }
                    }
                }

            for (j=0; j<*t2; j++)
                {
                if (*h0==1)
                    {// standardisation by HD when species labels are randomized
                    gii[j]=gii[j]/(tabg[j]*(*HD));
                    kii[j]=kii[j]/(tabk[j]*(*HD));
                    //deviation from theo=1
                    if ((float) fabs(gr[j]-1)<=(float) fabs(gii[j]-1)) gval[j]+=1;
                    if ((float) fabs(kr[j]-1)<=(float) fabs(kii[j]-1)) kval[j]+=1;
                    }
                if (*h0==2)
                    {// standardisation by Hsim when distance matrix is randomized
                    gii[j]=gii[j]/(tabg[j]*2*HDsim);
                    kii[j]=kii[j]/(tabk[j]*2*HDsim);
                    //deviation from theo=shimatani
                    if ((float) fabs(gr[j]-gs[j])<=(float) fabs(gii[j]-gs[j])) gval[j]+=1;
                    if ((float) fabs(kr[j]-ks[j])<=(float) fabs(kii[j]-ks[j])) kval[j]+=1;
                    }
                }

            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }
        free(gic);
        free(kic);
        freeintvec(vec);
        freevec(matp);
        }


    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

int rao_tr_disq_ic(int *point_nb, double *x, double *y, double *x0, double *y0, double *r0,
                   int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                   int *t2, double *dt, int *nbSimu, int *h0, double *lev, int *nbtype, int *type, double *mat, double *surface, double *HD,
                   double *gr, double *kr, double *gs, double *ks, double *gic1, double *gic2, double *kic1, double *kic2,
                   double *gval, double *kval, int *error)
    {
    int i, j, p, b, z=0;
    int *l; // contient le nombre d'arbres par esp�ce
    int compt[*nbtype+1]; // tableau de compteurs pour xx et yy
    vecintalloc(&l, *nbtype+1);
    double *ds, dis;
    double *g, *k;
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    //Ripley for all points	
    int erreur;
    double intensity1, intensity2;
    double intensity=*point_nb/(*surface);
    double *gii, *kii;
    vecalloc(&gii, *t2);
    vecalloc(&kii, *t2);
    double *tabg, *tabk;
    vecalloc(&tabg, *t2);
    vecalloc(&tabk, *t2);

    for (j=0; j<*t2; j++)
        {
        gii[j]=0;
        kii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }
    //Ripley all points	
    erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        Rprintf("ERREUR 0 Ripley\n");

    // Shimatani function for normalisation under H0=2
    double D=0;
    if (*h0==2)
        {
        erreur=shimatani_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, nbtype, type, surface, gs, ks, error);
        for (i=1; i<(*nbtype+1); i++)
            D+=(float) l[i]*((float) l[i]-1);
        D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));
        }

    for (j=0; j<*t2; j++)
        {
        g[j]=intensity*g[j]/ds[j];
        k[j]=intensity*k[j];
        tabg[j]=g[j];
        tabk[j]=k[j];
        if (g[j]==0)
            {
            error[j]=j;
            z+=1;
            }
        if (*h0==2)
            {
            gs[j]=gs[j]/D;
            ks[j]=ks[j]/D;
            }
        }
    //Intertype for each pairs of types
    if (z==0)
        {
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                erreur=intertype_tr_disq(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity1*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity1*k[j];
                    }
                erreur=intertype_tr_disq(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    gii[j]=gii[j]+dis*intensity2*g[j]/ds[j];
                    kii[j]=kii[j]+dis*intensity2*k[j];
                    }
                }
            }

        for (j=0; j<*t2; j++)
            {
            gr[j]=gii[j]/(tabg[j]*(*HD));
            kr[j]=kii[j]/(tabk[j]*(*HD));
            }

        //simulaitons sur gii, kii
        double **gic, **kic;
        int i0, i1, i2;

        /*Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC*/
        i0=*lev/2*(*nbSimu+1);
        if (i0<1) i0=1;

        /*Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC*/
        taballoc(&gic, *t2+1, 2*i0+10+1);
        taballoc(&kic, *t2+1, 2*i0+10+1);

        for (i=0; i<*t2; i++)
            {
            gval[i]=1;
            kval[i]=1;
            }

        int lp=0;
        double HDsim;
        int *vec, mat_size;
        vecintalloc(&vec, *nbtype);
        double *matp;
        mat_size=*nbtype*(*nbtype-1)/2;
        vecalloc(&matp, mat_size);
        if (*h0==2)
            {
            for (i=0; i<*nbtype; i++)
                vec[i]=i;
            for (i=0; i<mat_size; i++)
                matp[i]=0;
            }

        /*boucle principale de MC*/
        Rprintf("Monte Carlo simulation\n");
        for (b=1; b<=*nbSimu; b++)
            {
            if (*h0==1) //random labelling
                randomlab(x, y, *point_nb, type, *nbtype, xx, l, yy);
            if (*h0==2) //distance randomisation
                randomdist(vec, *nbtype, mat, matp);
            HDsim=0;
            for (i=0; i<*t2; i++)
                {
                gii[i]=0;
                kii[i]=0;
                }
            for (i=1; i<*nbtype; i++)
                {
                for (p=0; p<i; p++)
                    {
                    if (*h0==1)
                        dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
                    else // (*h0==2)
                        {
                        dis=matp[p*(*nbtype-2)-(p-1)*p/2+i-1];
                        HDsim+=(float) l[i+1]/(float) (*point_nb)*(float) l[p+1]/(float) (*point_nb)*dis;
                        }
                    erreur=intertype_tr_disq(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
                    if (erreur!=0)
                        Rprintf("ERREUR 1 Intertype\n");
                    intensity1=l[i+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity1*gic1[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity1*kic1[j];
                        }
                    erreur=intertype_tr_disq(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, gic1, kic1);
                    if (erreur!=0)
                        Rprintf("ERREUR 2 Intertype\n");
                    intensity2=l[p+1]/(*surface);
                    for (j=0; j<*t2; j++)
                        {
                        gii[j]=gii[j]+dis*intensity2*gic1[j]/ds[j];
                        kii[j]=kii[j]+dis*intensity2*kic1[j];
                        }
                    }
                }
            for (j=0; j<*t2; j++)
                {
                if (*h0==1)
                    {// standardisation by HD when species labels are randomized
                    gii[j]=gii[j]/(tabg[j]*(*HD));
                    kii[j]=kii[j]/(tabk[j]*(*HD));
                    //deviation from theo=1
                    if ((float) fabs(gr[j]-1)<=(float) fabs(gii[j]-1)) gval[j]+=1;
                    if ((float) fabs(kr[j]-1)<=(float) fabs(kii[j]-1)) kval[j]+=1;
                    }
                if (*h0==2)
                    {// standardisation by Hsim when distance matrix is randomized
                    gii[j]=gii[j]/(tabg[j]*2*HDsim);
                    kii[j]=kii[j]/(tabk[j]*2*HDsim);
                    //deviation from theo=shimatani
                    if ((float) fabs(gr[j]-gs[j])<=(float) fabs(gii[j]-gs[j])) gval[j]+=1;
                    if ((float) fabs(kr[j]-ks[j])<=(float) fabs(kii[j]-ks[j])) kval[j]+=1;
                    }
                }


            //Traitement des resultats
            ic(b, i0, gic, kic, gii, kii, *t2);
            R_FlushConsole();
            progress(b, &lp, *nbSimu);
            }

        i1=i0+2;
        i2=i0;

        //Copies des valeurs dans les tableaux resultats
        for (i=0; i<*t2; i++)
            {
            gic1[i]=gic[i+1][i1];
            gic2[i]=gic[i+1][i2];
            kic1[i]=kic[i+1][i1];
            kic2[i]=kic[i+1][i2];
            }
        free(gic);
        free(kic);
        freeintvec(vec);
        freevec(matp);
        }


    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);

    free(tabg);
    free(tabk);

    freevec(gii);
    freevec(kii);
    freevec(ds);

    return 0;
    }

//permutation of species distance matrix

int randomdist(int *vec, int nb_type, double *mat, double *matp)
    {
    int i, j, a, jj;
    int rowvec, colvec, ind;
    int erreur=0;

    GetRNGstate();
    i=nb_type-1;
    while (i>0)
        {
        jj=unif_rand()*(i);
        a=vec[i];
        vec[i]=vec[jj];
        vec[jj]=a;
        i=i-1;
        }
    PutRNGstate();
    a=0;
    for (i=1; i<nb_type; i++)
        for (j=0; j<(nb_type-i); j++)
            {
            rowvec=i+j;
            colvec=i-1;
            if (vec[rowvec]>vec[colvec])
                ind=vec[colvec]*(nb_type-2)-(vec[colvec]-1)*vec[colvec]/2+vec[rowvec]-1;
            else
                {
                ind=vec[rowvec]*(nb_type-2)-(vec[rowvec]-1)*vec[rowvec]/2+vec[colvec]-1;
                }
            matp[a]=mat[ind];
            a++;
            }
    return erreur;
    }

/******************************************************/
/*Mimetic point process as in Goreaud et al. 2004     */

/******************************************************/
int mimetic_rect(int *point_nb, double *x, double *y, double *surface, double *xmi, double *xma, double *ymi, double *yma,
                 double *prec, int *t2, double *dt, double *lobs, int *nsimax, int *conv, double *cost,
                 double *g, double *k, double *xx, double *yy, int *mess)
    {
    int i, compteur_c=0, r=0, erreur=0;
    int compteur=0;
    double *l;
    double cout, cout_c;
    double intensity=(*point_nb)/(*surface);
    vecalloc(&l, *t2);

    // Shift windows to ensure positive coordinates
    double offsetx=((*xmi)<0)?(*xmi):0.;
    double offsety=((*ymi)<0)?(*ymi):0.;
    decalRect(*point_nb, x, y, xmi, xma, ymi, yma);

    //creation of a initial point pattern and cost
    s_alea_rect(*point_nb, x, y, *xmi, *xma, *ymi, *yma, *prec);
    erreur=ripley_rect(point_nb, x, y, xmi, xma, ymi, yma, t2, dt, g, k);
    if (erreur!=0) return -1;
    cout=0;
    for (i=0; i<*t2; i++)
        { //l[i]=sqrt(k[i]/(intensity*Pi()));
        l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
        cout+=(lobs[i]-l[i])*(lobs[i]-l[i]);
        }
    cost[0]=cout;
    int lp=0;
    if (mess!=0)
        Rprintf("Simulated annealing\n");
    cout_c=0;
    while (compteur<*nsimax)
        {
        cout_c=echange_point_rect(*point_nb, x, y, *xmi, *xma, *ymi, *yma, intensity, *prec, cout, lobs, t2, dt, g, k);
        if (cout==cout_c)
            compteur_c++;
        else
            compteur_c=0;
        cout=cout_c;
        //Rprintf(" co�t calcul� : %f\n", cout);
        compteur++;
        cost[compteur]=cout;
        if (compteur_c==*conv)
            break;
        if (mess!=0)
            {
            R_FlushConsole();
            progress(compteur, &lp, *nsimax);
            }
        }
    if (compteur==*nsimax)
        {
        if (mess!=0)
            Rprintf("Warning: failed to converge after nsimax=%d simulations", *nsimax);
        r=1;
        }
    for (i=0; i<(*point_nb); i++)
        {
        // Shift x and y back into original windows
        x[i]+=offsetx;
        y[i]+=offsety;
        xx[i]=x[i];
        yy[i]=y[i];
        }
    // Shift windows back into original location
    *xmi+=offsetx;
    *xma+=offsetx;
    *ymi+=offsety;
    *yma+=offsety;
    free(l);
    return r;
    }

double echange_point_rect(int point_nb, double *x, double *y, double xmi, double xma, double ymi, double yma, double intensity, double p, double cout, double * lobs, int *t2, double *dt, double *g, double *k)
    {
    double xr, yr, xcent[4], ycent[4], n_cout[4], *l, xprec, yprec;
    int erreur, max, i, j, num;
    vecalloc(&l, *t2);
    GetRNGstate();
    num=unif_rand()* (point_nb); // numero de l'arbre que l'on retire
    xprec=x[num];
    yprec=y[num];
    xr=xma-xmi;
    yr=yma-ymi;

    for (i=0; i<*t2; i++)
        {
        g[i]=0;
        k[i]=0;
        }


    for (j=0; j<4; j++)
        {
        xcent[j]=xmi+(unif_rand()*(xr/p))*p; // coordonn�es (x,y) du point tir�
        ycent[j]=ymi+(unif_rand()*(yr/p))*p;
        x[num]=xcent[j];
        y[num]=ycent[j];
        erreur=ripley_rect(&point_nb, x, y, &xmi, &xma, &ymi, &yma, t2, dt, g, k);
        if (erreur!=0) return -1;
        for (i=0; i<*t2; i++)
            {
            l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
            //l[i]=sqrt(k[i]/(intensity*Pi()));
            }
        n_cout[j]=0;
        for (i=0; i<*t2; i++)
            {
            n_cout[j]+=(lobs[i]-l[i])*(lobs[i]-l[i]);
            //n_cout[j]+=(lobs[i]-l[i])*(lobs[i]-l[i])/(lobs[i]*lobs[i]);
            }
        }
    PutRNGstate();
    max=0;
    for (i=1; i<4; i++)
        {
        if (n_cout[i]<n_cout[max])
            max=i;
        }
    if (n_cout[max]<cout) // on prend le nouveau point qui minimise le co�t
        {
        x[num]=xcent[max];
        y[num]=ycent[max];
        cout=n_cout[max];
        }
    else // on reprend l'ancien point
        {
        x[num]=xprec;
        y[num]=yprec;
        }

    free(l);

    return cout;


    }

int mimetic_disq(int *point_nb, double *x, double *y, double *surface, double *x0, double *y0, double *r0,
                 double *prec, int *t2, double *dt, double *lobs, int *nsimax, int *conv, double *cost,
                 double *g, double *k, double *xx, double *yy, int *mess)
    {
    int i, compteur_c=0, r=0, erreur=0;
    int compteur=0;
    double *l;
    double cout, cout_c;
    double intensity=(*point_nb)/(*surface);
    vecalloc(&l, *t2);

    // Shift windows to ensure positive coordinates
    double offsetx=((*x0-*r0)<0)?(*x0-*r0):0.;
    double offsety=((*y0-*r0)<0)?(*y0-*r0):0.;
    decalCirc(*point_nb, x, y, x0, y0, *r0);

    //creation of a initial point pattern and cost
    //r=s_RegularProcess_disq(*point_nb,3,x,y,*x0,*y0,*r0,*prec);
    //r=s_NeymanScott_disq(5,*point_nb,3,x,y,*x0,*y0,*r0,*prec);
    s_alea_disq(*point_nb, x, y, *x0, *y0, *r0, *prec);
    erreur=ripley_disq(point_nb, x, y, x0, y0, r0, t2, dt, g, k);
    if (erreur!=0)
        return -1;
    cout=0;
    for (i=0; i<*t2; i++)
        {
        l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
        cout+=(lobs[i]-l[i])*(lobs[i]-l[i]);
        }
    cost[0]=cout;

    int lp=0;
    if (mess!=0)
        Rprintf("Simulated annealing\n");
    while (compteur<*nsimax)
        {
        cout_c=echange_point_disq(*point_nb, x, y, *x0, *y0, *r0, intensity, *prec, cout, lobs, t2, dt, g, k);
        if (cout==cout_c)
            {
            compteur_c++;
            }
        else compteur_c=0;
        cout=cout_c;
        //Rprintf(" co�t calcul� : %f\n", cout);
        compteur++;
        cost[compteur]=cout;
        if (compteur_c==*conv)
            break;
        if (mess!=0)
            {
            R_FlushConsole();
            progress(compteur, &lp, *nsimax);
            }
        }
    if (compteur==(*nsimax))
        {
        if (mess!=0)
            Rprintf("Warning: failed to converge after nsimax=%d simulations", *nsimax);
        r=1;
        }
    for (i=0; i<(*point_nb); i++)
        {
        // Shift x and y back into original windows
        x[i]+=offsetx;
        y[i]+=offsety;
        xx[i]=x[i];
        yy[i]=y[i];
        }
    // Shift windows back to original location
    *x0+=offsetx;
    *y0+=offsety;
    free(l);
    return r;
    }

double echange_point_disq(int point_nb, double *x, double *y, double x0, double y0, double r0, double intensity, double p, double cout, double * lobs, int *t2, double *dt, double *g, double *k)
    {
    double rr, xcent[4], ycent[4], n_cout[4], *l, xprec, yprec;
    int erreur, max, i, j, num;
    vecalloc(&l, *t2);
    GetRNGstate();
    num=unif_rand()* (point_nb); // numero de l'arbre que l'on retire
    xprec=x[num];
    yprec=y[num];
    rr=2*r0;
    for (j=0; j<4; j++)
        {
        xcent[j]=x0-r0+(unif_rand()*(rr/p))*p; // coordonn�es (x,y) du point tir�
        ycent[j]=y0-r0+(unif_rand()*(rr/p))*p;
        while ((xcent[j]-x0)*(xcent[j]-x0)+(ycent[j]-y0)*(ycent[j]-y0)>=r0*r0)
            {
            xcent[j]=x0-r0+(unif_rand()*(rr/p))*p; // coordonn�es (x,y) du point tir�
            ycent[j]=y0-r0+(unif_rand()*(rr/p))*p;
            }
        x[num]=xcent[j];
        y[num]=ycent[j];
        erreur=ripley_disq(&point_nb, x, y, &x0, &y0, &r0, t2, dt, g, k);
        if (erreur!=0)
            {
            return -1;
            }
        for (i=0; i<*t2; i++)
            {
            l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
            }

        n_cout[j]=0;
        for (i=0; i<*t2; i++)
            {
            n_cout[j]+=(lobs[i]-l[i])*(lobs[i]-l[i]);
            }


        }
    PutRNGstate();
    max=0;
    for (i=1; i<4; i++)
        {
        if (n_cout[i]<n_cout[max])
            max=i;
        }
    if (n_cout[max]<cout) // on prend le nouveau point qui minimise le co�t
        {
        x[num]=xcent[max];
        y[num]=ycent[max];
        cout=n_cout[max];
        }
    else // on reprend l'ancien point
        {
        x[num]=xprec;
        y[num]=yprec;
        }

    free(l);

    return cout;


    }

int mimetic_tr_rect(int *point_nb, double *x, double *y, double *surface, double *xmi, double *xma, double *ymi, double *yma,
                    int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                    double *prec, int *t2, double *dt, double *lobs, int *nsimax, int *conv, double *cost,
                    double *g, double *k, double *xx, double *yy, int *mess)
    {
    int i, compteur_c=0, r=0, erreur=0;
    int compteur=0;
    double *l;
    double cout, cout_c;
    double intensity=(*point_nb)/(*surface);
    vecalloc(&l, *t2);

    // Shift windows to ensure positive coordinates
    double offsetx=((*xmi)<0)?(*xmi):0.;
    double offsety=((*ymi)<0)?(*ymi):0.;
    decalRectTri(*point_nb, x, y, xmi, xma, ymi, yma, *triangle_nb, ax, ay, bx, by, cx, cy);

    //creation of a initial point pattern and cost
    s_alea_tr_rect(*point_nb, x, y, *xmi, *xma, *ymi, *yma, *triangle_nb, ax, ay, bx, by, cx, cy, *prec);
    erreur=ripley_tr_rect(point_nb, x, y, xmi, xma, ymi, yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        return -1;
    cout=0;
    for (i=0; i<*t2; i++)
        {
        l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
        cout+=(lobs[i]-l[i])*(lobs[i]-l[i]);
        }
    cost[0]=cout;

    int lp=0;
    if (mess!=0)
        Rprintf("Simulated annealing\n");
    while (compteur<*nsimax)
        {
        cout_c=echange_point_tr_rect(*point_nb, x, y, *xmi, *xma, *ymi, *yma, triangle_nb, ax, ay, bx, by, cx, cy, intensity, *prec, cout, lobs, t2, dt, g, k);
        if (cout==cout_c)
            compteur_c++;
        else compteur_c=0;
        cout=cout_c;
        //Rprintf(" co�t calcul� : %f\n", cout);
        compteur++;
        cost[compteur]=cout;
        if (compteur_c==*conv)
            break;
        if (mess!=0)
            {
            R_FlushConsole();
            progress(compteur, &lp, *nsimax);
            }
        }
    if (compteur==(*nsimax))
        {
        if (mess!=0)
            Rprintf("Warning: failed to converge after nsimax=%d simulations", *nsimax);
        r=1;
        }
    for (i=0; i<(*point_nb); i++)
        {
        // Shift coordinates back into original windows
        x[i]+=offsetx;
        y[i]+=offsety;
        xx[i]=x[i];
        yy[i]=y[i];
        }
    // Shift windows back to original position
    *xmi+=offsetx;
    *xma+=offsetx;
    *ymi+=offsety;
    *yma+=offsety;
    // Shift triangles back to original position
    for (i=0; i<(*triangle_nb); i++)
        {
        ax[i]+=offsetx;
        bx[i]+=offsetx;
        cx[i]+=offsetx;
        ay[i]+=offsety;
        by[i]+=offsety;
        cy[i]+=offsety;
        }
    free(l);
    return r;
    }

double echange_point_tr_rect(int point_nb, double *x, double *y, double xmi, double xma, double ymi, double yma,
                             int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                             double intensity, double p, double cout, double * lobs, int *t2, double *dt, double *g, double *k)
    {
    double xr, yr, xcent[4], ycent[4], n_cout[4], *l, xprec, yprec;
    int erreur, max, i, j, num, erreur_tr, s;
    vecalloc(&l, *t2);
    GetRNGstate();
    num=unif_rand()* (point_nb); // numero de l'arbre que l'on retire
    xprec=x[num];
    yprec=y[num];
    xr=xma-xmi;
    yr=yma-ymi;
    for (j=0; j<4; j++)
        {
        do
            {
            erreur_tr=0;
            xcent[j]=xmi+(unif_rand()*(xr/p))*p; // coordonn�es (x,y) du point tir�
            ycent[j]=ymi+(unif_rand()*(yr/p))*p;
            x[num]=xcent[j];
            y[num]=ycent[j];
            for (s=0; s<*triangle_nb; s++)
                if (in_triangle(x[num], y[num], ax[s], ay[s], bx[s], by[s], cx[s], cy[s], 1))
                    erreur_tr=1;
            }
        while (erreur_tr==1);
        //Rprintf("point pris\n");

        erreur=ripley_tr_rect(&point_nb, x, y, &xmi, &xma, &ymi, &yma, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
        if (erreur!=0) return -1;
        for (i=0; i<*t2; i++)
            l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
        n_cout[j]=0;
        for (i=0; i<*t2; i++)
            n_cout[j]+=(lobs[i]-l[i])*(lobs[i]-l[i]);
        }
    PutRNGstate();
    max=0;
    for (i=1; i<4; i++)
        {
        if (n_cout[i]<n_cout[max])
            max=i;
        }
    if (n_cout[max]<cout) // on prend le nouveau point qui minimise le co�t
        {
        x[num]=xcent[max];
        y[num]=ycent[max];
        cout=n_cout[max];
        }
    else // on reprend l'ancien point
        {
        x[num]=xprec;
        y[num]=yprec;
        }

    free(l);

    return cout;


    }

int mimetic_tr_disq(int *point_nb, double *x, double *y, double *surface, double *x0, double *y0, double *r0,
                    int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                    double *prec, int *t2, double *dt, double *lobs, int *nsimax, int *conv, double *cost,
                    double *g, double *k, double *xx, double *yy, int *mess)
    {
    int i, compteur_c=0, r=0, erreur=0;
    int compteur=0;
    double *l;
    double cout, cout_c;
    double intensity=(*point_nb)/(*surface);
    vecalloc(&l, *t2);

    // Shift windows to ensure positive coordinates
    double offsetx=((*x0-*r0)<0)?(*x0-*r0):0.;
    double offsety=((*y0-*r0)<0)?(*y0-*r0):0.;
    decalCircTri(*point_nb, x, y, x0, y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy);

    //creation of a initial point pattern and cost
    //r=s_RegularProcess_tr_disq(*point_nb,3,x,y,*x0,*y0,*r0,triangle_nb,ax,ay,bx,by,cx,cy,*prec);
    //r=s_NeymanScott_tr_disq(4,*point_nb,2,x,y,*x0,*y0,*r0,triangle_nb,ax,ay,bx,by,cx,cy,*prec);
    s_alea_tr_disq(*point_nb, x, y, *x0, *y0, *r0, *triangle_nb, ax, ay, bx, by, cx, cy, *prec);
    erreur=ripley_tr_disq(point_nb, x, y, x0, y0, r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
    if (erreur!=0)
        return -1;
    cout=0;
    for (i=0; i<*t2; i++)
        {
        l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
        cout+=(lobs[i]-l[i])*(lobs[i]-l[i]);
        }
    cost[0]=cout;

    int lp=0;
    if (mess!=0)
        Rprintf("Simulated annealing\n");
    while (compteur<*nsimax)
        {
        cout_c=echange_point_tr_disq(*point_nb, x, y, *x0, *y0, *r0, triangle_nb, ax, ay, bx, by, cx, cy, intensity, *prec, cout, lobs, t2, dt, g, k);
        if (cout==cout_c)
            compteur_c++;
        else
            compteur_c=0;
        cout=cout_c;
        //Rprintf(" co�t calcul� : %f\n", cout);
        compteur++;
        cost[compteur]=cout;
        if (compteur_c==*conv)
            break;
        if (mess!=0)
            {
            R_FlushConsole();
            progress(compteur, &lp, *nsimax);
            }
        }
    if (compteur==(*nsimax))
        {
        if (mess!=0)
            Rprintf("Warning: failed to converge after nsimax=%d simulations", *nsimax);
        r=1;
        }
    for (i=0; i<(*point_nb); i++)
        {
        // Shift x and y back into original windows
        x[i]+=offsetx;
        y[i]+=offsety;
        xx[i]=x[i];
        yy[i]=y[i];
        }
    // Shift windows back to original location
    *x0+=offsetx;
    *y0+=offsety;
    // Shift triangles back to original position
    for (i=0; i<(*triangle_nb); i++)
        {
        ax[i]+=offsetx;
        bx[i]+=offsetx;
        cx[i]+=offsetx;
        ay[i]+=offsety;
        by[i]+=offsety;
        cy[i]+=offsety;
        }
    free(l);
    return r;
    }

double echange_point_tr_disq(int point_nb, double *x, double *y, double x0, double y0, double r0,
                             int *triangle_nb, double *ax, double *ay, double *bx, double *by, double *cx, double *cy,
                             double intensity, double p, double cout, double * lobs, int *t2, double *dt, double *g, double *k)
    {
    double rr, xcent[4], ycent[4], n_cout[4], *l, xprec, yprec;
    int erreur, max, i, j, num, erreur_tr, s;
    vecalloc(&l, *t2);
    GetRNGstate();
    num=unif_rand()* (point_nb); // numero de l'arbre que l'on retire
    xprec=x[num];
    yprec=y[num];
    rr=2*r0;
    for (j=0; j<4; j++)
        {
        do
            {
            erreur_tr=0;
            xcent[j]=x0-r0+(unif_rand()*(rr/p))*p; // coordonn�es (x,y) du point tir�
            ycent[j]=y0-r0+(unif_rand()*(rr/p))*p;
            while ((xcent[j]-x0)*(xcent[j]-x0)+(ycent[j]-y0)*(ycent[j]-y0)>=r0*r0)
                {
                xcent[j]=x0-r0+(unif_rand()*(rr/p))*p; // coordonn�es (x,y) du point tir�
                ycent[j]=y0-r0+(unif_rand()*(rr/p))*p;
                }
            x[num]=xcent[j];
            y[num]=ycent[j];
            for (s=0; s<*triangle_nb; s++)
                {
                if (in_triangle(x[num], y[num], ax[s], ay[s], bx[s], by[s], cx[s], cy[s], 1))
                    {
                    erreur_tr=1;
                    //Rprintf("erreur_tr\n");
                    }
                }

            }
        while (erreur_tr==1);
        //Rprintf("point pris\n");

        erreur=ripley_tr_disq(&point_nb, x, y, &x0, &y0, &r0, triangle_nb, ax, ay, bx, by, cx, cy, t2, dt, g, k);
        if (erreur!=0)
            {
            return -1;
            }
        for (i=0; i<*t2; i++)
            {
            l[i]=sqrt(k[i]/(intensity*Pi()))-(i+1)*(*dt);
            }

        n_cout[j]=0;
        for (i=0; i<*t2; i++)
            {
            n_cout[j]+=(lobs[i]-l[i])*(lobs[i]-l[i]);
            }


        }
    PutRNGstate();
    max=0;
    for (i=1; i<4; i++)
        {
        if (n_cout[i]<n_cout[max])
            max=i;
        }
    if (n_cout[max]<cout) // on prend le nouveau point qui minimise le co�t
        {
        x[num]=xcent[max];
        y[num]=ycent[max];
        cout=n_cout[max];
        }
    else // on reprend l'ancien point
        {
        x[num]=xprec;
        y[num]=yprec;
        }

    free(l);

    return cout;

    }

int shen(int *point_nb, double *x, double *y,
         int *t2, double *dt,
         int *nbtype, int *type, double *mat, double *surface, double *HD,
         double *gd, double *kd, int *error)
    {
    int i, j, p;
    int *l;
    int compt[*nbtype+1];
    double *g, *k;
    double *ds, dis;

    vecintalloc(&l, *nbtype+1);
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    // l contient le nombre d'arbres par esp�ce
    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    // cr�ation des tableaux xx et yy par esp�ce
    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1, intensity2;
    //double intensity=*point_nb/(*surface);
    double *gsii, *ksii, *grii, *krii;
    vecalloc(&gsii, *t2);
    vecalloc(&ksii, *t2);
    vecalloc(&grii, *t2);
    vecalloc(&krii, *t2);

    for (j=0; j<*t2; j++)
        {
        gsii[j]=0;
        ksii[j]=0;
        grii[j]=0;
        krii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    double D=(float) l[1]*((float) l[1]-1);

    //Intertype for each pairs of types
    for (i=1; i<*nbtype; i++)
        {
        D+=(float) l[i+1]*((float) l[i+1]-1);
        for (p=0; p<i; p++)
            {
            dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
            //D+=(float)l[i+1]*((float)l[p+1]);
            //H+=dis*(float)l[i]*((float)l[p]);
            erreur=intertype(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], t2, dt, g, k);
            if (erreur!=0)
                Rprintf("ERREUR 1 Intertype\n");
            intensity1=l[i+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gsii[j]+=intensity1*g[j]/ds[j];
                ksii[j]+=intensity1*k[j];
                grii[j]+=dis*intensity1*g[j]/ds[j];
                krii[j]+=dis*intensity1*k[j];
                }
            erreur=intertype(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], t2, dt, g, k);
            if (erreur!=0)
                Rprintf("ERREUR 2 Intertype\n");
            intensity2=l[p+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gsii[j]+=intensity2*g[j]/ds[j];
                ksii[j]+=intensity2*k[j];
                grii[j]+=dis*intensity2*g[j]/ds[j];
                krii[j]+=dis*intensity2*k[j];
                }
            }
        }

    D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));

    for (j=0; j<*t2; j++)
        {
        gd[j]=(D*grii[j])/((*HD)*gsii[j]);
        kd[j]=(D*krii[j])/((*HD)*ksii[j]);
        }
    /*D=D/((float)(*point_nb)*((float)(*point_nb)-1));
    H=H/((float)(*point_nb)*((float)(*point_nb)-1));
	
    for(j=0;j<*t2;j++)
    {	gd[j]=(grii[j]/H)/(gsii[j]/D);
            kd[j]=(krii[j]/H)/(ksii[j]/D);
    }*/

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);
    freevec(gsii);
    freevec(ksii);
    freevec(grii);
    freevec(krii);
    freevec(ds);
    return 0;
    }

int shen_ic(int *point_nb, double *x, double *y,
            int *t2, double *dt, int *nbSimu, double *lev,
            int *nbtype, int *type, double *mat, double *surface, double *HD,
            double *gd, double *kd, double *gic1, double *gic2, double *kic1, double *kic2,
            double *gval, double *kval, int *error)
    {
    int i, j, p;
    int *l;
    int compt[*nbtype+1];
    double *g, *k;
    double *ds, dis;

    vecintalloc(&l, *nbtype+1);
    vecalloc(&g, *t2);
    vecalloc(&k, *t2);

    // l contient le nombre d'arbres par esp�ce
    for (i=1; i<*nbtype+1; i++)
        {
        l[i]=0;
        compt[i]=0;
        for (j=0; j<*point_nb; j++)
            {
            if (type[j]==i)
                l[i]++;
            }
        }

    // cr�ation des tableaux xx et yy par esp�ce
    double **xx, **yy;
    xx=taballoca(*nbtype, l);
    yy=taballoca(*nbtype, l);
    vecalloc(&ds, *t2);
    complete_tab(*point_nb, xx, yy, type, compt, l, x, y);

    int erreur;
    double intensity1, intensity2;
    //double intensity=*point_nb/(*surface);
    double *gsii, *ksii, *grii, *krii;
    vecalloc(&gsii, *t2);
    vecalloc(&ksii, *t2);
    vecalloc(&grii, *t2);
    vecalloc(&krii, *t2);

    for (j=0; j<*t2; j++)
        {
        gsii[j]=0;
        ksii[j]=0;
        grii[j]=0;
        krii[j]=0;
        ds[j]=(Pi()*(j+1)*(*dt)*(j+1)*(*dt))-(Pi()*j*j*(*dt)*(*dt));
        }

    double D=(float) l[1]*((float) l[1]-1);

    //Intertype for each pairs of types
    for (i=1; i<*nbtype; i++)
        {
        D+=(float) l[i+1]*((float) l[i+1]-1);
        for (p=0; p<i; p++)
            {
            dis=mat[p*(*nbtype-2)-(p-1)*p/2+i-1];
            //D+=(float)l[i+1]*((float)l[p+1]);
            //H+=dis*(float)l[i]*((float)l[p]);
            erreur=intertype(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], t2, dt, g, k);
            if (erreur!=0)
                Rprintf("ERREUR 1 Intertype\n");
            intensity1=l[i+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gsii[j]+=intensity1*g[j]/ds[j];
                ksii[j]+=intensity1*k[j];
                grii[j]+=dis*intensity1*g[j]/ds[j];
                krii[j]+=dis*intensity1*k[j];
                }
            erreur=intertype(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], t2, dt, g, k);
            if (erreur!=0)
                Rprintf("ERREUR 2 Intertype\n");
            intensity2=l[p+1]/(*surface);
            for (j=0; j<*t2; j++)
                {
                gsii[j]+=intensity2*g[j]/ds[j];
                ksii[j]+=intensity2*k[j];
                grii[j]+=dis*intensity2*g[j]/ds[j];
                krii[j]+=dis*intensity2*k[j];
                }
            }
        }

    D=1-D/((float) (*point_nb)*((float) (*point_nb)-1));

    for (j=0; j<*t2; j++)
        {
        gd[j]=(D*grii[j])/((*HD)*gsii[j]);
        kd[j]=(D*krii[j])/((*HD)*ksii[j]);
        }

    //////////////
    //simulations sur gii, kii
    double **gic, **kic;
    int i0, i1, i2;

    //Definition de i0 : indice ou sera stocke l'estimation des bornes de l'IC
    i0=*lev/2*(*nbSimu+1);
    if (i0<1) i0=1;

    //Initialisation des tableaux dans lesquels on va stocker les valeurs extremes lors de MC
    taballoc(&gic, *t2+1, 2*i0+10+1);
    taballoc(&kic, *t2+1, 2*i0+10+1);

    for (i=0; i<*t2; i++)
        {
        gval[i]=1;
        kval[i]=1;
        }
    //pr�paration des tableaux pour randomisation de dis (H0=2)
    int b, lp=0;
    double HDsim;
    int *vec, mat_size;
    vecintalloc(&vec, *nbtype);
    double *matp;
    mat_size=*nbtype*(*nbtype-1)/2;
    vecalloc(&matp, mat_size);
    for (i=0; i<*nbtype; i++)
        vec[i]=i;
    for (i=0; i<mat_size; i++)
        matp[i]=0;

    //boucle principale de MC
    Rprintf("Monte Carlo simulation\n");
    for (b=1; b<=*nbSimu; b++)
        {
        randomdist(vec, *nbtype, mat, matp);
        HDsim=0;
        for (i=0; i<*t2; i++)
            {
            grii[i]=0;
            krii[i]=0;
            }
        for (i=1; i<*nbtype; i++)
            {
            for (p=0; p<i; p++)
                {
                dis=matp[p*(*nbtype-2)-(p-1)*p/2+i-1];
                HDsim+=(float) l[i+1]/(float) (*point_nb)*(float) l[p+1]/(float) (*point_nb)*dis;
                erreur=intertype(&l[i+1], xx[i], yy[i], &l[p+1], xx[p], yy[p], t2, dt, gic1, kic1);
                if (erreur!=0)
                    Rprintf("ERREUR 1 Intertype\n");
                intensity1=l[i+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    grii[j]+=dis*intensity1*gic1[j]/ds[j];
                    krii[j]+=dis*intensity1*kic1[j];
                    }
                erreur=intertype(&l[p+1], xx[p], yy[p], &l[i+1], xx[i], yy[i], t2, dt, gic1, kic1);
                if (erreur!=0)
                    Rprintf("ERREUR 2 Intertype\n");
                intensity2=l[p+1]/(*surface);
                for (j=0; j<*t2; j++)
                    {
                    grii[j]+=dis*intensity2*gic1[j]/ds[j];
                    krii[j]+=dis*intensity2*kic1[j];
                    }
                }
            }

        for (j=0; j<*t2; j++)
            {
            grii[j]=(D*grii[j])/((2*HDsim)*gsii[j]);
            krii[j]=(D*krii[j])/((2*HDsim)*ksii[j]);
            //deviation from theo=1
            if ((float) fabs(gd[j]-1)<=(float) fabs(grii[j]-1)) gval[j]+=1;
            if ((float) fabs(kd[j]-1)<=(float) fabs(krii[j]-1)) kval[j]+=1;
            }

        //Traitement des resultats
        ic(b, i0, gic, kic, grii, krii, *t2);
        R_FlushConsole();
        progress(b, &lp, *nbSimu);
        }

    i1=i0+2;
    i2=i0;

    //Copies des valeurs dans les tableaux resultats
    for (i=0; i<*t2; i++)
        {
        gic1[i]=gic[i+1][i1];
        gic2[i]=gic[i+1][i2];
        kic1[i]=kic[i+1][i1];
        kic2[i]=kic[i+1][i2];
        }
    free(gic);
    free(kic);
    freeintvec(vec);
    freevec(matp);

    for (i=0; i < *nbtype; i++)
        {
        free(xx[i]);
        free(yy[i]);
        }
    free(xx);
    free(yy);
    free(g);
    free(k);
    free(l);
    freevec(gsii);
    freevec(ksii);
    freevec(grii);
    freevec(krii);
    freevec(ds);
    return 0;
    }

int intertype(int *point_nb1, double *x1, double *y1, int *point_nb2, double *x2, double *y2, int *t2, double *dt, double *g, double *k)
    {
    int i, j, tt;
    double d;

    /*On rangera dans g le nombre de couples de points par distance tt*/
    for (tt=0; tt<*t2; tt++)
        g[tt]=0;

    /* On regarde tous les couples (i,j)*/
    for (i=0; i<*point_nb1; i++)
        {
        for (j=0; j<*point_nb2; j++)
            {
            d=sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
            if (d<*t2*(*dt))
                { /* dans quelle classe de distance est ce couple ?*/
                tt=d/(*dt);
                /* pas de correction des effets de bord*/
                g[tt]+=1;
                }
            }
        }

    /* on moyenne -> densite*/
    for (tt=0; tt<*t2; tt++)
        g[tt]=g[tt]/(*point_nb1);

    /*on integre*/
    k[0]=g[0];
    for (tt=1; tt<*t2; tt++)
        k[tt]=k[tt-1]+g[tt];

    return 0;
    }
