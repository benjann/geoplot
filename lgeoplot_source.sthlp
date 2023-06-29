*! Source of lgeoplot.mlib
*! {smcl}
*!     {helpb lgeoplot_source##geo_area:geo_area()}
*!     {helpb lgeoplot_source##geo_centroid:geo_centroid()}
*!     {helpb lgeoplot_source##geo_circle_tangents:geo_circle_tangents()}
*!     {helpb lgeoplot_source##geo_pid:geo_pid()}
*!     {helpb lgeoplot_source##geo_plevel:geo_plevel()}
*!     {helpb lgeoplot_source##geo_pointinpolygon:geo_pointinpolygon()}
*!     {helpb lgeoplot_source##geo_rotate:geo_rotate()}
*!     {helpb lgeoplot_source##geo_spjoin:geo_spjoin()}
*!     {helpb lgeoplot_source##geo_symbol:geo_symbol()}
*!     {helpb lgeoplot_source##geo_welzl:geo_welzl()}
*! {asis}

version 17

*! {smcl}
*! {marker geo_area}{bf:geo_area()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Computes area of each unit using the Shoelace formula
*! See https://en.wikipedia.org/wiki/Shoelace_formula
*! assuming (1) that the polygons belonging to a unit are separated by a row
*! or missing coordinates, (2) that the coordinates of each polygon are in
*! order (clockwise or counter-clockwise), (3) that the polygons wrap around
*! (first coordinate = last coordinate), (4) that the polygons are grouped by
*! unit ID, (5) that nested polygons within a unit switch orientation (so that
*! "holes" will be deducted from the total area of the unit)
*!
*! Syntax:
*!
*!      result = geo_area(rtype, ID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a real colvector of length
*!          n containing repeated area values, else the result is a r x 2 matrix
*!          with ID in 1st row and area in 2nd row, where r is the number of units
*!  ID      real colvector of unit IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!

mata:
mata set matastrict on

real matrix geo_area(real scalar rtype, real colvector ID, real matrix XY)
{
    real scalar    n, i, ai, bi
    real colvector a, b, A
    
    n = rows(XY)
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    if (rtype) {
        A = J(n,1,.)
        for (;i;i--) {
            ai = a[i]; bi = b[i]
            A[|ai \ bi|] = J(bi-ai+1, 1, _geo_area(XY[|ai,1 \ bi,.|]))
        }
        return(A)
    }
    A = J(i,1,.)
    for (;i;i--) A[i] = _geo_area(XY[|a[i],1 \ b[i],.|])
    return((ID[a],A))
}

real scalar _geo_area(real matrix XY)
{
    real scalar n, d
    
    n = rows(XY)
    if (n==0) return(J(0,1,.))
    if (n==1) return(0)
    d = sum(XY[,1] :* (XY[|2,2\.,2|]\.) - (XY[|2,1\.,1|]\.) :* XY[,2])
    return(abs(d)/2)
}

end

*! {smcl}
*! {marker geo_centroid}{bf:geo_centroid()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Computes centroid of each unit; see https://en.wikipedia.org/wiki/Centroid
*! assuming (1) that the polygons belonging to a unit are separated by a row
*! or missing coordinates, (2) that the coordinates of each polygon are in
*! order (clockwise or counter-clockwise), (3) that the polygons wrap around
*! (first coordinate = last coordinate), (4) that the polygons are grouped by
*! unit ID, (5) that nested polygons within a unit switch orientation (so that
*! "holes" will be deducted from the total area of the unit)
*!
*! Syntax:
*!
*!      result = geo_centroid(rtype, ID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a n x 2 matrix containing
*!          repeated centroids, else the result is a r x 3 matrix with ID in
*!          1st row, X of centroid in 2nd row, and Y of centroid in 3rd row,
*!          where r is the number of units
*!  ID      real colvector of unit IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!

mata:
mata set matastrict on

real matrix geo_centroid(real scalar rtype, real colvector ID, real matrix XY)
{
    real scalar    n, i, ai, bi
    real colvector a, b
    real matrix    C
    
    n = rows(XY)
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = n
    else      b = a[|2 \. |] :- 1 \ n
    if (rtype) {
        C = J(n,2,.)
        for (;i;i--) {
            ai = a[i]; bi = b[i]
            C[|ai,1 \ bi,2|] = J(bi-ai+1, 1, _geo_centroid(XY[|ai,1 \ bi,.|]))
        }
        return(C)
    }
    C = J(i,2,.)
    for (;i;i--) C[i,] = _geo_centroid(XY[|a[i],1 \ b[i],.|])
    return((ID[a],C))
}

real rowvector _geo_centroid(real matrix XY)
{
    real scalar    n, x, y
    real colvector d
    
    n = rows(XY)
    if (n==0) return(J(0,2,.))
    if (n==1) return(XY[1,])
    d = XY[,1] :* (XY[|2,2\.,2|]\.) - (XY[|2,1\.,1|]\.) :* XY[,2]
    x = sum((XY[,1] + (XY[|2,1\.,1|]\.)) :* d)
    y = sum((XY[,2] + (XY[|2,2\.,2|]\.)) :* d)
    return((x, y) :/ (3 * sum(d)))
}

end

*! {smcl}
*! {marker geo_circle_tangents}{bf:geo_circle_tangents()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Returns the points of the external tangents connecting two circles, based on
*! https://en.wikipedia.org/wiki/Tangent_lines_to_circles
*!
*! Syntax:
*!
*!      points = geo_circle_tangents(circle1, circle2)
*!
*!  points   4 x 2 real matrix of the points; X in col 1, Y in col 2
*!           J(0,0,.) will be returned if the tangents do not exist
*!  circle1  real rowvector containing mid and radius (X,Y,R) of circle 1
*!  circle2  real rowvector containing mid and radius (X,Y,R) of circle 2
*!

local RS      real scalar
local Circle  real rowvector  // (X,Y,R)
local Points  real matrix     // (X,Y) \ (X,Y) \ ...

mata:
mata set matastrict on

`Points' geo_circle_tangents(`Circle' c1, `Circle' c2)
{
    `RS'     d, alpha1, alpha2, beta, gamma
    `Points' P
    
    d = sqrt((c2[1] - c1[1])^2 + (c2[2] - c1[2])^2)
    if (c1[3]>=(d+c2[3])) return(J(0,0,.)) // c1 covers c2
    if (c2[3]>=(d+c1[3])) return(J(0,0,.)) // c2 covers c1
    if (c1[1]<c2[1])  beta  =  asin((c2[3] - c1[3]) / d)
    else              beta  = -asin((c2[3] - c1[3]) / d)
    if (c1[1]==c2[1]) gamma =   sign(c2[2] - c1[2]) * pi()/2
    else              gamma = -atan((c2[2] - c1[2]) / (c2[1] - c1[1]))
    alpha1 = gamma - beta
    alpha2 = gamma + beta
    P      = (c1[1] + c1[3] * sin(alpha1), c1[2] + c1[3] * cos(alpha1)) \
             (c2[1] + c2[3] * sin(alpha1), c2[2] + c2[3] * cos(alpha1)) \
             (c1[1] - c1[3] * sin(alpha2), c1[2] - c1[3] * cos(alpha2)) \
             (c2[1] - c2[3] * sin(alpha2), c2[2] - c2[3] * cos(alpha2))
    return(P)
}

end

*! {smcl}
*! {marker geo_pid}{bf:geo_pid()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Generate polygon ID within unit ID
*!
*! Syntax:
*!
*!      PID = geo_pid(ID, XY)
*!
*!  PID     real colvector of within-unit polygon IDs
*!  ID      real colvector of unit IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!          each polygon is assumed to start (or end) with a row of missings
*!          polygons are assumed to be grouped by unit ID
*!

mata:
mata set matastrict on

real colvector geo_pid(real colvector ID, real matrix XY)
{
    real scalar    i
    real colvector a, b, p
    
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = rows(XY)
    else      b = a[|2 \. |] :- 1 \ rows(XY)
    p = J(rows(XY),1,.)
    for (;i;i--) p[|a[i] \ b[i]|] = _geo_pid(XY[|a[i],1 \ b[i],.|])
    return(p)
}

real colvector _geo_pid(real matrix XY)
{
    real scalar    n
    real colvector p
    
    n = rows(XY)
    if (n==0) return(J(0,1,.))
    if (n==1) return(1)
    p = rowmissing(XY):!=0             // missing starts new polygon
    if (!p[1]) p = p[n] \ p[|1 \ n-1|] // assume polygons end with missing
    return(runningsum(p))
}

end

*! {smcl}
*! {marker geo_plevel}{bf:geo_plevel()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Determines plot levels of polygons: 0 = neither enclave nor exclave, 1 =
*! enclave, 2 = exclave, 3 = enclave within exclave, 4 = exclave within
*! enclave, etc.
*!
*! Syntax:
*!
*!      result = geo_plevel(rtype, ID, PID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a real colvector of length
*!          n containing repeated plot level values, else the result is a r x 2
*!          matrix with ID in 1st row, PID in 2nd row, and plot level in 3rd
*!          row, where r is the total number of polygons
*!  ID      real colvector of unit IDs
*!  PID     real colvector of within-unit polygon IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!

local Bool      real scalar
local Int       real scalar
local IntC      real colvector
local RS        real scalar
local RC        real colvector
local RM        real matrix
local POLYGON   _geo_POLYGON
local Polygon   struct `POLYGON' scalar
local pPolygon  pointer (`Polygon') scalar
local pPolygons pointer (`Polygon') vector
local UNIT      _geo_UNIT
local Unit      struct `UNIT' scalar
local pUnits    pointer (`Unit') vector

mata:
mata set matastrict on

struct `POLYGON' {
    `Int'  id       // within-unit polygon id
    `RM'   XY       // coordinates
    `RS'   xmin, xmax, ymin, ymax
    `Int'  a, b     // data range of polygon
    `Int'  l        // plot level
    `IntC' d        // list of descendants
}

struct `UNIT' {
    `Int'       id   // unit id
    `Int'       n    // number of polygons within unit
    `pPolygons' p    // polygons
}

`RM' geo_plevel(`Int' rtype, `RC' ID, `RC' PID, `RM' XY)
{
    `Int'    i, j, a, b
    `RM'     P
    `pUnits' u
    
    // collect units and polygons
    u = _geo_collect_units(ID, PID, XY)
    // determine within unit plot levels
    for (i=length(u);i;i--) _geo_plevel_within(*u[i])
    // update plot levels based on between unit comparisons
    for (i=length(u);i;i--) {
        for (j=i-1; j; j--) _geo_plevel_between(*u[i], *u[j])
    }
    // fill in result
    if (rtype) {
        P = J(rows(ID), 1, .)
        for (i=length(u);i;i--) {
            for (j=u[i]->n;j;j--) {
                a = u[i]->p[j]->a
                b = u[i]->p[j]->b
                P[|a \ b|] = J(b-a+1, 1, u[i]->p[j]->l)
            }
        }
        return(P)
    }
    a = 0
    for (i=length(u);i;i--) a = a + u[i]->n
    P = J(a,3,.)
    for (i=length(u);i;i--) {
        for (j=u[i]->n;j;j--) {
            P[a,] = (u[i]->id, u[i]->p[j]->id, u[i]->p[j]->l)
            a--
        }
    }
    return(P)
}

void _geo_plevel_within(`Unit' u)
{
    `Int'      i, j
    `pPolygon' pi, pj

    for (i=u.n;i;i--) {
        pi = u.p[i]
        for (j=i-1; j; j--) {
            pj = u.p[j]
            if (pi->ymax < pj->ymin) continue
            if (pi->xmax < pj->xmin) continue
            if (pj->ymax < pi->ymin) continue
            if (pj->xmax < pi->xmin) continue
            if (!_geo_plevel_within_d(u, i, j)) {
                if (_geo_plevel_notinside(pi->XY, pj->XY)) {
                    if (!_geo_plevel_within_d(u, j, i)) {
                        if (_geo_plevel_notinside(pj->XY, pi->XY)) continue
                    }
                    // pj is inside pi
                    pj->l = pj->l + 1
                    pi->d = pi->d \ j
                    continue
                }
            }
            // pi is inside pj
            pi->l = pi->l + 1
            pj->d = pj->d \ i
        }
    }
}

`Bool' _geo_plevel_within_d(`Unit' u, `Int' i, `Int' j)
{   // checks whether polygon i is among descendants of polygon j
    `Int'  ji
    `IntC' d
    
    d = u.p[j]->d
    for (ji=rows(d);ji;ji--) {
        if (i==d[ji]) return(1)
        if (_geo_plevel_within_d(u, i, d[ji])) return(1)
    }
    return(0)
}

void _geo_plevel_between(`Unit' ui, `Unit' uj)
{
    `Int'      i, j, l
    `pPolygon' pi, pj
    
    // update plot level
    for (i=ui.n;i;i--) {
        pi = ui.p[i]
        l  = pi->l
        for (j=uj.n;j;j--) {
            pj = uj.p[j]
            if (mod(l,2)!=mod(pj->l, 2)) continue
            if (pi->ymax < pj->ymin) continue
            if (pi->xmax < pj->xmin) continue
            if (pj->ymax < pi->ymin) continue
            if (pj->xmax < pi->xmin) continue
            if (_geo_plevel_notinside(pi->XY, pj->XY)) {
                if (_geo_plevel_notinside(pj->XY, pi->XY)) continue
                // pj is in pi; increase plot level by 2
                pj->l = pj->l + 2
                continue
            }
            // pi is inside pj; increase plot level by 2
            pi->l = pi->l + 2
        }
    }
}

`Bool' _geo_plevel_notinside(`RM' xy, `RM' XY)
{   // check if at least one point of xy is outside XY
    `Int' i
    `RS'  px, py

    for (i=rows(xy);i;i--) {
        px = xy[i,1]; py = xy[i,2]
        if (!geo_pointinpolygon(px, py, XY)) return(1)
    }
    return(0)
}

`pUnits' _geo_collect_units(`RC' ID, `RC' PID, `RM' XY)
{
    `Int'    i
    `IntC'   a, b
    `pUnits' u
    
    a = selectindex(_mm_unique_tag(ID))
    i = rows(a)
    if (i<=1) b = rows(ID)
    else      b = a[|2 \. |] :- 1 \ rows(ID)
    u = J(i,1,NULL)
    for (;i;i--) u[i] = &_geo_collect_unit(a[i], b[i], ID, PID, XY)
    return(u)
}

`Unit' _geo_collect_unit(`Int' a, `Int' b, `RC' ID, `RC' PID, `RM' XY)
{
    `Int'  i
    `IntC' i1, i2
    `Unit' u
    
    u.id = ID[a]
    i1   = (a - 1) :+ selectindex(_mm_unique_tag(PID[|a \ b|]))
    u.n  = i = rows(i1)
    if (i<=1) i2 = b
    else      i2  = i1[|2 \. |] :- 1 \ b
    u.p = J(u.n, 1, NULL)
    for (;i;i--) {
        u.p[i] = &_geo_collect_polygon(i1[i], i2[i], PID, XY)
    }
    return(u)
}

`Polygon' _geo_collect_polygon(`Int' a, `Int' b, `RC' PID, `RM' XY)
{
    `RM'      minmax
    `Polygon' p
    
    p.id = PID[a]
    p.a  = a
    p.b  = b
    p.l  = 0
    p.XY = XY[|a,1 \ b,2|]
    p.XY = select(p.XY, !rowmissing(p.XY)) // exclude missing points
    minmax = colminmax(p.XY)
    p.xmin = minmax[1,1]
    p.xmax = minmax[2,1]
    p.ymin = minmax[1,2]
    p.ymax = minmax[2,2]
    return(p)
}

end

*! {smcl}
*! {marker geo_pointinpolygon}{bf:geo_pointinpolygon()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Determines whether a point is inside or outside of a given polygon; the
*! implementation is based on the code examples at
*! https://rosettacode.org/wiki/Ray-casting_algorithm
*!
*! Syntax:
*!
*!      inside = geo_pointinpolygon(x, y, XY)
*!
*!  inside  real scalar equal to 1 if point is inside, 0 else
*!  x       real scalar containing X coordinate of point
*!  y       real scalar containing Y coordinate of point
*!  YX      n x 2 real matrix containing (X,Y) coordinates of polygon
*!

mata:
mata set matastrict on

real scalar geo_pointinpolygon(real scalar x, real scalar y, real matrix XY)
{
    real scalar  i, c
    real scalar  py, ax, ay, bx, by
    
    c = 0
    i = rows(XY)
    ax = XY[i,1]; ay = XY[i,2]
    i--
    for (;i;i--) {
        bx = ax; by = ay
        ax = XY[i,1]; ay = XY[i,2]
        py = y // work on copy
        if (ay>by) {
            if (_geo_rayintersect(x, py, bx, by, ax, ay)) c++
        }
        else {
            if (_geo_rayintersect(x, py, ax, ay, bx, by)) c++
        }
    }
    return(mod(c,2))
}

real scalar _geo_rayintersect(real scalar px, real scalar py,
    real scalar ax, real scalar ay, real scalar bx, real scalar by)
{
    real scalar eps
    real scalar m_red, m_blue
    
    eps = 0.00001
    if (py==ay | py==by)    py = py + eps
    if (py>by | py<ay)      return(0)
    if (px > max((ax, bx))) return(0)
    if (px < min((ax, bx))) return(1)
    if (ax!=bx) m_red  = (by - ay) / (bx - ax)
    if (ax!=px) m_blue = (py - ay) / (px - ax)
    return(m_blue >= m_red)
}

end

*! {smcl}
*! {marker geo_rotate}{bf:geo_rotate()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Rotates coordinates around (0,0) by angle degrees (counterclockwise)
*!
*! Syntax:
*!
*!      result = geo_rotate(YX, angle)
*!
*!  result  n x 2 real matrix of rotated (X,Y) coordinates
*!  YX      n x 2 real matrix of (X,Y) coordinates
*!  angle   real scalar specifying angle
*!
*! Rotate in place:
*!
*!      _geo_rotate(YX, angle)
*!

mata:
mata set matastrict on

real matrix geo_rotate(real matrix XY, real scalar angle)
{
    real scalar r
    
    r  = angle * pi() / 180
    return((XY[,1] * cos(r) - XY[,2] * sin(r),
            XY[,1] * sin(r) + XY[,2] * cos(r)))
}

void _geo_rotate(real matrix XY, real scalar angle)
{
    if (angle==0) return
    XY = geo_rotate(XY, angle)
}

end

*! {smcl}
*! {marker geo_spjoin}{bf:geo_spjoin()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Spatially joins the points in xy to the polygons in XY
*!
*! Syntax:
*!
*!      result = geo_spjoin(xy, ID, PID, XY [, PL])
*!
*!  result  r x 1 colvector containing matched IDs
*!  xy      r x 2 matrix containing points to be matched
*!  ID      n x 1 real colvector of unit IDs
*!  PID     n x 1 real colvector of within-unit polygon IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*?  PL      optional n x 1 real colvector containing plot level indicator 
*!

local Int       real scalar
local IntC      real colvector
local RS        real scalar
local RC        real colvector
local RM        real matrix
local POLYGON   _geo_POLYGON
local Polygon   struct `POLYGON' scalar
local pPolygon  pointer (`Polygon') scalar
local UNIT      _geo_UNIT
local Unit      struct `UNIT' scalar
local pUnits    pointer (`Unit') vector

mata:
mata set matastrict on

`RC' geo_spjoin(`RM' xy, `RC' ID, `RC' PID, `RM' XY, | `RC' PL)
{
    `Int'    i
    `IntC'   p, P
    `RC'     id, L
    pointer  pl
    
    if (!rows(PL)) return(_geo_spjoin(xy, ID, PID, XY))
    if (hasmissing(PL)) pl = &editmissing(PL, 0) // treat missing as 0
    else                pl = &PL
    L  = mm_unique(select(*pl, _mm_uniqrows_tag((ID, PID))))
    id = J(rows(xy), 1, .)
    p  = . // use all points in first round
    for (i=length(L);i;i--) {
        if (mod(L[i],2)) continue // skip enclaves
        P     = selectindex(*pl:==L[i])
        id[p] = _geo_spjoin(xy[p,], ID[P], PID[P], XY[P,])
        if (i>1) p = selectindex(id:>=.) // remaining unmatched points
    }
    return(id)
}

`RC' _geo_spjoin(`RM' xy, `RC' ID, `RC' PID, `RM' XY)
{
    `Int'    i
    `RC'     id
    `pUnits' u
    
    u = _geo_collect_units(ID, PID, XY)
    i = rows(xy)
    id = J(i,1,.)
    for (;i;i--) id[i] = __geo_spjoin(xy[i,1], xy[i,2], u)
    return(id)
}

`RS' __geo_spjoin(`RS' px, `RS' py, `pUnits' u)
{
    `Int'      i, j
    `pPolygon' p
    
    for (i=length(u);i;i--) {
        for (j=u[i]->n;j;j--) {
            p = u[i]->p[j]
            if (px < p->xmin) continue
            if (px > p->xmax) continue
            if (py < p->ymin) continue
            if (py > p->ymax) continue
            if (geo_pointinpolygon(px, py, p->XY)) return(u[i]->id)
        }
    }
    return(.)
}

end

*! {smcl}
*! {marker geo_symbol}{bf:symbol()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Returns the coordinates of a selected symbol
*!
*! Syntax:
*!
*!      result = geo_symbol("symbol" [, n, "arg"])
*!
*!  result  r x 2 real matrix of the (X,Y) coordinates or r x 3 real matrix
*!          of the (X,Y) coordinates and a plot level indicator
*!  symbol  name of symbol
*!  n       real scalar specifying number of points on circle; use symbol
*!          "circle" with low n to create a regular polygons, e.g.
*!          geo_symbol("circle", 6) for a hexagon
*!  arg     string scalar containing symbol-specific arguments
*!

mata:
mata set matastrict on

real matrix geo_symbol(string scalar sym, | real scalar n, string scalar arg)
{
    pointer(real matrix function) scalar f
    
    if (sym=="")                f = &_geo_symbol_circle()
    else if (sym=="circle")     f = &_geo_symbol_circle()
    else if (sym=="slice")      f = &_geo_symbol_slice()
    else if (sym=="arc")        f = &_geo_symbol_arc()
    else if (sym=="pentagram")  f = &_geo_symbol_pentagram()
    else if (sym=="hexagram")   f = &_geo_symbol_hexagram()
    else if (sym=="pin")        f = &_geo_symbol_pin()
    else if (sym=="pin2")       f = &_geo_symbol_pin2()
    else {
        f = findexternal("_geo_symbol_" + sym + "()")
        if (f==NULL) {
            errprintf("geo_symbol(): symbol '%s' not found\n", sym)
            exit(111)
        }
    }
    return((*f)(n, arg))
}

real matrix _geo_symbol_circle(| real scalar n0, string scalar arg)
{
    real colvector r, n
    pragma unused arg
    
    if (n0>=.) n = 100
    else       n = n0
    r = .25 - .5/n // set angle such that shape has horizontal edge at bottom
    r = rangen(-r, 1-r, n+1) * (2 * pi())
    return((cos(r), sin(r)))
}

real matrix _geo_symbol_slice(| real scalar n, string scalar arg)
{
    return((0,0) \ _geo_symbol_arc(n, arg) \ (0,0))
}

real matrix _geo_symbol_arc(| real scalar n0, string scalar arg)
{
    real scalar    n, angle
    real colvector r
    
    if (arg=="") angle = 180 // default
    else {
        angle = strtoreal(arg)
        if (angle<-360 | angle>360) {
            errprintf("symbol arc: {it:angle} must be in [-360,360]\n")
            exit(125)
        }
    }
    if (n0>=.) n = max((2, ceil(abs(angle)/360 * 100)))
    else       n = n0
    r = rangen(0, angle/180, n) * pi()
    return((cos(r), sin(r)))
}

real matrix _geo_symbol_pentagram(| real scalar n, string scalar arg)
{
    real matrix XY, xy
    pragma unused n
    pragma unused arg
    
    // outer pentagon
    XY = _geo_symbol_circle(5)
    // inner pentagon
    xy = _geo_symbol_circle(5) / (.5 * (1 + sqrt(5)))^2
    _geo_rotate(xy, 180)
    // merge
    XY = colshape((XY[1::5,], (xy[4::5,] \ xy[1::3,])), 2)
    XY = XY \ XY[1,] // last point = first point
    return(XY)
}

real matrix _geo_symbol_hexagram(| real scalar n, string scalar arg)
{
    real matrix XY, xy
    pragma unused n
    pragma unused arg
    
    // outer hexagon
    XY = _geo_symbol_circle(6)
    _geo_rotate(XY, 90)
    // inner hexagon
    xy = _geo_symbol_circle(6) * sqrt(1/3)
    // merge
    XY = colshape(((XY[5::6,] \ XY[1::4,]), xy[1::6,]), 2)
    XY = XY \ XY[1,] // last point = first point
    return(XY)
}

real matrix _geo_symbol_pin(| real scalar n0, string scalar arg)
{
    real colvector n, r, h
    real matrix    xy
    
    if (arg=="") h = 1/3 // default
    else {
        h = strtoreal(arg)
        if (h<0 | h>1) {
            errprintf("symbol pin: {it:headsize} must be in [0,1]\n")
            exit(125)
        }
    }
    if (n0>=.) n = max((4, ceil(h * 100)))
    else       n = n0
    r = rangen(-.25, .75, n+1) * (2 * pi())
    xy = h * (cos(r), sin(r))
    xy[,2] = xy[,2] :+ (2-h)
    xy = xy \ (0,0) \ (0,2-2*h)
    return(xy)
}

real matrix _geo_symbol_pin2(| real scalar n0, string scalar arg)
{
    real colvector n, r, a, c
    real matrix    xy0, xy1
    
    if (arg=="") a = .6 // default
    else {
        a = strtoreal(arg)
        if (a<0 | a>1) {
            errprintf("symbol pin2: {it:headsize} must be in [0,1]\n")
            exit(125)
        }
    }
    if (n0>=.) n = max((4, ceil(a * 100)))
    else       n = n0
    // outer polygon
    c = 2 - a
    r = rangen(-asin(a/c), pi()+asin(a/c), n+1)
    xy0 = a * (cos(r),sin(r))
    xy0[,2] = xy0[,2] :+ c
    xy0 = xy0 \ (0,0) \ xy0[1,] 
    xy0 = xy0, J(rows(xy0),1,0) // plevel=0
    // inner polygon
    r = rangen(0, 1, n+1) * (2 * pi())
    xy1 = (0.5*a) * (cos(r), sin(r))
    xy1[,2] = xy1[,2] :+ c
    xy1 = (.,.) \ xy1
    xy1 = xy1, J(rows(xy1),1,1) // plevel=1
    return(xy0 \ xy1)
}

end

*! {smcl}
*! {marker geo_welzl}{bf:geo_welzl()}{asis}
*! version 1.0.0  27jun2023  Ben Jann
*!
*! Find minimum enclosing circle using Welzl's algorithm; based on
*! https://www.geeksforgeeks.org/minimum-enclosing-circle-using-welzls-algorithm/
*! and
*! https://stackoverflow.com/questions/50925307/iterative-version-of-welzls-algorithm
*!
*!
*! Syntax:
*!
*!      circle = geo_welzl(points [, recursive])
*!
*!  circle     real rowvector containing mid and radius (X,Y,R)
*!  points     n x 2 real matrix containing points; X in col 1, Y in col 2;
*!             should not contain missing values
*!  recursive  recursive!=0 requests the recursive algorithm
*!

local RS      real scalar
local Bool    real scalar
local Int     real scalar
local Point   real rowvector  // (X,Y)
local Points  real matrix     // (X,Y) \ (X,Y) \ ...
local Circle  real rowvector  // (X,Y,R)
local FRAME   _geo_welzl_frame
local Frame   struct `FRAME' scalar
local PFrame  pointer (`Frame') scalar
local PFrames pointer (`Frame') rowvector
local FRAMES  _geo_welzl_frames
local Frames  struct `FRAMES' scalar

mata:
mata set matastrict on

// main function

`Circle' geo_welzl(`Points' P0, | `Bool' recursive)
{
    `Points' P
    
    if (args()<2) recursive = 0
    if (isfleeting(P0)) {
        P0 = jumble(select(P0, mm_uniqrows_tag(P0)))
        if (recursive) return(_geo_welzl_recursive(P0, J(0,2,.), rows(P0)))
        return(_geo_welzl_iterative(P0))
    }
    P = jumble(select(P0, mm_uniqrows_tag(P0)))
    if (recursive) return(_geo_welzl_recursive(P, J(0,2,.), rows(P)))
    return(_geo_welzl_iterative(P))
}

// iterative variant of Welzl algorithm

struct `FRAME' {
    `Int'    s  // stage
    `Int'    n  // current n
    `Point'  p  // selected point
    `Points' r  // up to three selected points
}

`Frame' _geo_welzl_frame_copy(`Frame' f0)
{
    `Frame' f
    
    f = f0
    return(f)
}

struct `FRAMES' {
    `Int'     n  // current length
    `PFrames' F  // stack of frames
}

`PFrames' _geo_welzl_frames_pop(`Frames' frames)
{
    `PFrame' f
    
    f = frames.F[frames.n]
    frames.n = frames.n - 1
    return(f)
}

void _geo_welzl_frames_append(`Frames' frames, `PFrame' f)
{
    frames.n = frames.n + 1
    frames.F[frames.n] = f
}

`Circle' _geo_welzl_iterative(`Points' P)
{
    `Circle'  c
    `PFrame'  frame
    `Frames'  frames
    
    // initialize frames stacks
    frames.n       = 1
    frames.F       = J(1, rows(P)+1, NULL)
    frames.F[1]    = &(`FRAME'())
    frames.F[1]->s = 1
    frames.F[1]->n = rows(P)
    frames.F[1]->r = J(0,2,.)
    // iterate
    while (frames.n) {
        frame = _geo_welzl_frames_pop(frames)
        if (frame->n==0 | rows(frame->r)==3) {
            c = _geo_welzl_min_circle_trivial(frame->r)
        }
        else if (frame->s==1) {
            frame->p     = P[1,] // pick 1st (input has been jumbled)
            P[1,]        = P[frame->n,]
            P[frame->n,] = frame->p
            frame->s     = 2
            _geo_welzl_frames_append(frames, frame)
            frame        = &_geo_welzl_frame_copy(*frame)
            frame->s     = 1
            frame->n     = frame->n - 1
            _geo_welzl_frames_append(frames, frame)
        }
        else if (frame->s==2) {
            if (!_geo_welzl_is_inside(c, frame->p)) {
                frame->s = 1
                frame->n = frame->n - 1
                frame->r = frame->r \ frame->p
                _geo_welzl_frames_append(frames, frame)
            }
        }
    }
    return(c)
}

// recursive Welzl algorithm

`Circle' _geo_welzl_recursive(`Points' P, `Points' R0, `Int' n)
{
    `Point'  p
    `Circle' d
    `Points' R
    
    R = R0 // work on copy!
    if (n == 0 | rows(R) == 3) return(_geo_welzl_min_circle_trivial(R))
    p     = P[1,] // pick 1st (input has been jumbled)
    P[1,] = P[n,]
    P[n,] = p
    d     = _geo_welzl_recursive(P, R, n - 1)
    if (_geo_welzl_is_inside(d, p)) return(d)
    R     = R \ p
    return(_geo_welzl_recursive(P, R, n - 1))
}

// helper functions

// - get minimum enclosing circle given 3 or less points
`Circle' _geo_welzl_min_circle_trivial(`Points' P)
{
    `Int' n
    
    n = rows(P)
    assert(n <= 3)
    if (n == 0) return(J(1,3,0))
    if (n == 1) return((P,0))
    if (n == 2) return(_geo_welzl_circle_from2(P[1,], P[2,]))
    return(_geo_welzl_circle_from3(P[1,], P[2,], P[3,]))
}

// - get unique circle that intersects three points
`Circle' _geo_welzl_circle_from3(`Point' A, `Point' B, `Point' C)
{
    `RS'     bx, by, cx, cy, b, c, d
    `Circle' D
    
    bx = B[1] - A[1]
    by = B[2] - A[2]
    cx = C[1] - A[1]
    cy = C[2] - A[2]
    b  = bx^2 + by^2
    c  = cx^2 + cy^2
    d  = 2 * (bx * cy - by * cx)
    D  = A + ((cy * b - by * c) / d, (bx * c - cx * b) / d)
    D  = D, _geo_welzl_dist(D, A)
    return(D)
}

// - get smallest circle that intersects 2 points
`Circle' _geo_welzl_circle_from2(`Point' A, `Point' B)
{
    return(((A[1] + B[1]) / 2, (A[2] + B[2]) / 2, _geo_welzl_dist(A, B) / 2))
}

// - check whether a point lies inside or on the boundaries of the circle
`Bool' _geo_welzl_is_inside(`Circle' c, `Point' p)
{
    return(_geo_welzl_dist(c[(1,2)], p) <= c[3])
}

// - euclidean distance between two points
`RS' _geo_welzl_dist(`Point' a, `Point' b)
{
    return(sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2))
}

end
