*! Source of lgeoplot.mlib
*! {smcl}
*!     {helpb lgeoplot_source##geo_area:geo_area()}
*!     {helpb lgeoplot_source##geo_centroid:geo_centroid()}
*!     {helpb lgeoplot_source##geo_circle_tangents:geo_circle_tangents()}
*!     {helpb lgeoplot_source##geo_clip:geo_clip()}
*!     {helpb lgeoplot_source##geo_hull:geo_hull()}
*!     {helpb lgeoplot_source##geo_orientation:geo_orientation()}
*!     {helpb lgeoplot_source##geo_pid:geo_pid()}
*!     {helpb lgeoplot_source##geo_plevel:geo_plevel()}
*!     {helpb lgeoplot_source##geo_pointinpolygon:geo_pointinpolygon()}
*!     {helpb lgeoplot_source##geo_rotate:geo_rotate()}
*!     {helpb lgeoplot_source##geo_spjoin:geo_spjoin()}
*!     {helpb lgeoplot_source##geo_symbol:geo_symbol()}
*!     {helpb lgeoplot_source##geo_welzl:geo_welzl()}
*! {asis}

version 16.1

*! {smcl}
*! {marker geo_area}{bf:geo_area()}{asis}
*! version 1.0.1  30jun2023  Ben Jann
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
*! Syntax 1:
*!
*!      result = geo_area(rtype, ID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a real colvector of length
*!          n containing repeated area values, else the result is a r x 2 matrix
*!          with ID in 1st row and area in 2nd row, where r is the number of units
*!  ID      real colvector of unit IDs; can also specify ID as real scalar if
*!          there is only a single unit
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!
*! Syntax 2:
*!
*!      result = _geo_area(XY)
*!
*!  result  real scalar containing area
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!

mata:
mata set matastrict on

real matrix geo_area(real scalar rtype, real colvector id, real matrix XY)
{
    real scalar    n, i, ai, bi
    real colvector a, b, A
    pointer scalar ID
    
    n = rows(XY)
    if (length(id)==1) ID = &J(n,1,id)
    else               ID = &id
    a = selectindex(_mm_unique_tag(*ID))
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
    return(((*ID)[a],A))
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
*! {marker geo_bbox}{bf:geo_bbox()}{asis}
*! version 1.0.0  30jun2023  Ben Jann
*!
*! Determine (minimum) bounding box around cloud of points
*! minimum bounding box algorithm loosely based on
*! https://en.wikipedia.org/wiki/Minimum_bounding_box_algorithms
*!
*! Syntax:
*!
*!      result = geo_bbox(XY [, type])
*!
*!  result  real matrix containing (X,Y) of bounding box (counterclockwise)
*!  XY      n x 2 real matrix containing points; X in col 1, Y in col 2;
*!             should not contain missing values
*!  type    real scalar selecting type of box and algorithm
*!           0 = regular (unrotated) bounding box (the default)
*!           1 = minimum-area bounding box using rotating calipers algorithm
*!           2 = minimum-perimeter bounding box using rotating calipers alg.
*!          -1 = minimum-area bounding box using tilting algorithm
*!          -2 = minimum-perimeter bounding box using tilting algorithm
*!

mata
mata set matastrict on

real matrix geo_bbox(real matrix XY, | real scalar type)
{
    if (type==1)  return(_geo_bbox_rc(XY))
    if (type==2)  return(_geo_bbox_rc(XY, 1))
    if (type==-1) return(_geo_bbox_tilt(XY))
    if (type==-2) return(_geo_bbox_tilt(XY,1))
    return(_geo_bbox(XY))
}

real matrix _geo_bbox(real matrix XY)
{
    real matrix xy
    
    xy = colminmax(XY)
    return((xy[2,1],xy[1,2]) \ // xmax,ymin
           (xy[2,1],xy[2,2]) \ // xmax,ymax
           (xy[1,1],xy[2,2]) \ // xmin,ymax
           (xy[1,1],xy[1,2]) \ // xmin,ymin
           (xy[2,1],xy[1,2]))  // xmax,ymin
}

real matrix _geo_bbox_rc(real matrix XY, | real scalar perim)
{
    real scalar    b, r, t, l, a, n, xmin, xmax, ymax
    real matrix    xy, xy1
    real scalar    size, Size
    real matrix    box, Box
    
    if (args()<2) perim = 0
    // get convex hull
    xy = geo_hull(XY)
    n = rows(xy)
    if (n==0) return(J(5,2,.))
    if (n==1) return(J(5,1,xy[1,]))
    n = n - 1 // omit last point (which is equal to first point)
    // determine minimal box using rotating calipers
    Size = .
    b = r = t = l = 1
    for (; b<=n; b++) {
        // find/update calipers, one on each side (bottom, right, top, left)
        a = _geo_bbox_rc_angle(xy,n, b)
        r = _geo_bbox_rc_update(xy,n, r, a, 1)
        t = _geo_bbox_rc_update(xy,n, t, a, 2)
        l = _geo_bbox_rc_update(xy,n, l, a, 3)
        // compute size of box
        xy1  = xy[mod(b-1,n)+1,] // reference point
        box  = _geo_bbox_rc_rotate(xy[mod((r,t,l):-1,n):+1,]:-xy1, -a)
        xmin = box[3,1]; xmax = box[1,1]; ymax = box[2,2] // ymin is 0
        if (perim) size = (xmax-xmin) + (ymax)
        else       size = (xmax-xmin) * (ymax)
        // store box if smaller than pervious smallest box
        if (size < Size) {
            Size = size
            box = (xmax,0) \ (xmax,ymax) \ (xmin,ymax) \ (xmin,0)
            Box = _geo_bbox_rc_rotate(box, a) :+ xy1
        }
    }
    // rearrange so that start is at bottom right
    _geo_bbox_rearrange(Box)
    return(Box \ Box[1,])
}

real scalar _geo_bbox_rc_angle(real matrix XY, real scalar n, real scalar i0)
{   // returns counterclockwise angle of edge
    real scalar    i, j, a
    real rowvector xy
    
    i = mod(i0-1,n) + 1
    j = mod(i0, n) + 1
    xy = XY[j,] - XY[i,]
    a = acos(xy[1] / sqrt(sum(xy:^2)))
    if (xy[2]<0) a = 2*pi() - a
    return(a + floor((i0-1)/n)*2*pi())
}

real scalar _geo_bbox_rc_update(real matrix XY, real scalar n,
    real scalar i, real scalar r, real scalar side)
{
    real scalar j
    
    j = i
    while (_geo_bbox_rc_angle(XY,n, j) < (side/2*pi() + r)) j++
    return(j)
}

real matrix _geo_bbox_rc_rotate(real matrix XY, real scalar r)
{
    real scalar s, c
    
    s = sin(r); c = cos(r)
    return((XY[,1] * c - XY[,2] * s, XY[,1] * s + XY[,2] * c))
}

real matrix _geo_bbox_tilt(real matrix XY, | real scalar perim)
{
    real scalar i, n, a, s, S
    real matrix xy, xy0, b, B
    
    if (args()<2) perim = 0
    xy = geo_hull(XY)
    n = rows(xy)
    if (n==0) return(J(5,2,.))
    if (n==1) return(J(5,1,xy[1,]))
    S = .
    for (i=2;i<=n;i++) {
        xy0 = xy :- xy[i-1,]
        a   = _geo_bbox_tilt_angle(xy0[i,])
        b   = colminmax(geo_rotate(xy0, -a))
        if (perim) s = (b[2,1]-b[1,1]) + (b[2,2]-b[1,2])
        else       s = (b[2,1]-b[1,1]) * (b[2,2]-b[1,2])
        if (s<S) {
            S = s
            b = (b[2,1],b[1,2]) \ (b[2,1],b[2,2]) \
                (b[1,1],b[2,2]) \ (b[1,1],b[1,2])
            B = geo_rotate(b, a) :+ xy[i-1,]
        }
    }
    _geo_bbox_rearrange(B)
    return(B \ B[1,])
}

real scalar _geo_bbox_tilt_angle(real rowvector xy)
{
    real scalar r
    
    r = sqrt(sum(xy:^2))
    return(sign(xy[2]) * acos(xy[1]/r) / pi() * 180)
}

void _geo_bbox_rearrange(real matrix XY)
{
    real colvector i, j
    real matrix    w
    
    i = j = w = .
    minindex(XY[,2], 1, i, w)
    if (length(i)>1) {
        maxindex(XY[i,1], 1, j, w)
        i = i[j[1]]
    }
    if (i==1) return
    if (i==rows(XY)) XY = XY[i,] \ XY[|1,1 \ i-1,.|]
    else             XY = XY[|i,1 \ .,.|] \ XY[|1,1 \ i-1,.|]
}

end

*! {smcl}
*! {marker geo_centroid}{bf:geo_centroid()}{asis}
*! version 1.0.1  30jun2023  Ben Jann
*!
*! Computes centroid of each unit; see https://en.wikipedia.org/wiki/Centroid
*! assuming (1) that the polygons belonging to a unit are separated by a row
*! or missing coordinates, (2) that the coordinates of each polygon are in
*! order (clockwise or counter-clockwise), (3) that the polygons wrap around
*! (first coordinate = last coordinate), (4) that the polygons are grouped by
*! unit ID, (5) that nested polygons within a unit switch orientation (so that
*! "holes" will be deducted from the total area of the unit)
*!
*! Syntax 1:
*!
*!      result = geo_centroid(rtype, ID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a n x 2 matrix containing
*!          repeated centroids, else the result is a r x 3 matrix with ID in
*!          1st row, X of centroid in 2nd row, and Y of centroid in 3rd row,
*!          where r is the number of units
*!  ID      real colvector of unit IDs; can also specify ID as real scalar if
*!          there is only a single unit
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!
*! Syntax 2:
*!
*!      result = _geo_centroid(XY)
*!
*!  result  1 x 2 real vector containing centroid
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!

mata:
mata set matastrict on

real matrix geo_centroid(real scalar rtype, real colvector id, real matrix XY)
{
    real scalar    n, i, ai, bi
    real colvector a, b
    real matrix    C
    pointer scalar ID
    
    n = rows(XY)
    if (length(id)==1) ID = &J(n,1,id)
    else               ID = &id
    a = selectindex(_mm_unique_tag(*ID))
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
    return(((*ID)[a],C))
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
*! {marker geo_clip}{bf:geo_clip()}{asis}
*! version 1.0.2  10oct2023  Ben Jann
*!
*! Applies convex polygon or polyline clipping using a modified
*! Sutherland–Hodgman algorithm (divided polygons will be returned 
*! if appropriate)
*! (https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm)
*!
*! Syntax:
*!
*!      result = geo_clip(XY, mask [, line])
*!
*!  result   coordinates of clipped polygons (first and/or last row equal to
*!           missing if input started and/or ended with missing); separate shapes
*!           divided by missing; J(0,2,.) is returned if no points are inside
*!           clipping mask 
*!  XY       n x 2 real matrix containing the (X,Y) coordinates of the polygons
*!           or polylines to be clipped; separate shapes divided by missing;
*!           a shape is interpreted as a polygon if its first point is equal
*!           to its last point, else the shape is interpreted as polyline; point
*!           data is assumed if XY does not contain missing
*!  mask     n x 2 real matrix containing the (X,Y) coordinates of the convex
*!           clipping mask; invalid results will be returned if the clipping
*!           mask is not convex
*!  line     line!=0 enforces polyline clipping for polygons (i.e. do not
*!           close the clipped polygons)
*!
*! For rectangular clipping, you may also use the following function:
*!
*!      result = geo_rclip(XY, limits [, line])
*!
*!  limits   vector specifying the clipping limits: (Xmin, Xmax, Ymin, Ymax)
*!           (missing will be interpreted as +/- infinity)
*!
*!  other arguments as for geo_clip()
*!

local Bool      real scalar
local BoolC     real colvector
local Int       real scalar
local IntC      real colvector
local RS        real scalar
local RC        real colvector
local RR        real rowvector
local RM        real matrix
local SS        string scalar
local PS        pointer scalar
local PC        pointer colvector

mata:
mata set matastrict on

`RM' geo_clip(`RM' XY, `RM' mask, | `Bool' line)
{
    if (args()<3) line = 0
    return(_geo_clip(XY, _geo_clip_mask(mask), line))
}

`RM' _geo_clip_mask(`RM' mask0, | `SS' nm)
{
    `RM' mask
    
    if (args()<2) nm = "mask"
    if (cols(mask0)!=2) {
        errprintf("{it:%s} must have two columns\n", nm)
        exit(3200)
    }
    mask = select(mask0, !rowmissing(mask0))
    if (rows(mask)) {
        mask = mask \ mask[1,]    // wrap around
        mask = _mm_uniqrows(mask) // remove repetitions
    }
    if (rows(mask)<3) {
        errprintf("{it:%s} must contain at least two unique points\n", nm)
        exit(3200)
    }
    if (_geo_orientation(mask)==-1) {
        mask = mask[1::rows(mask),] // make counterclockwise
    }
    return(mask)
}

`RM' geo_rclip(`RM' XY, `RM' limits, | `Bool' line)
{
    if (args()<3) line = 0
    return(_geo_clip(XY, _geo_rclip_limits(limits), line))
}

`RC' _geo_rclip_limits(`RM' limits)
{
    `RC' mask
 
    mask = vec(limits)
    if (length(mask)<4) mask = mask \ J(4-length(mask),1,.)
    return(mask)
}

`RM' _geo_clip(`RM' XY, `RM' mask, `Bool' line)
{
    `Bool'  mfirst, mlast, hasmis
    `Int'   a, b
    `RM'    xy
    
    // defaults and checks
    if (args()<3) line = 0
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    // handle missings
    a = 1; b = rows(XY)
    if (b==0) return(J(1,2,.))  // empty input
    mfirst = hasmissing(XY[a,]) // has leading missings
    mlast  = hasmissing(XY[b,]) // has trailing missings
    if (mfirst) {
        for (++a;a<=b;a++) {
            if (!hasmissing(XY[a,])) break
        }
        if (a>b) return(J(1,2,.)) // input all missing
    }
    if (mlast) {
        for (--b;b;b--) {
            if (!hasmissing(XY[b,])) break
        }
    }
    hasmis = hasmissing(XY[|a,1 \ b,2|])
    // apply clipping
    if (hasmis) { // multiple shape items
        xy = _geo_clip_apply(&__geo_clip(), XY, mask, line, a, b)
    }
    else { // single shape item
        if (!mfirst & !mlast) return(_geo_clip_point(XY, mask)) // point data
        xy = __geo_clip(J(1,2,.) \ XY[|a,1 \ b,2|], mask, line)
    }
    // return
    if (rows(xy)) {
        if (!mfirst) xy = xy[|2,1\.,.|]
        if (mlast)   xy = xy \ J(1,2,.)
    }
    return(xy)
}

`RM' _geo_clip_apply(`PS' f, `RM' XY, `RM' mask, `Bool' line, `Int' a, `Int' b)
{
    `BoolC' mis
    `Int'   i, j, J
    `IntC'  A, B
    `RM'    xy
    `PC'    I
    
    // collect indices of shape items
    mis = selectindex(rowmissing(XY[|a,1 \ b,2|])) :+ (a - 1)
    i = rows(mis) + 1
    A = B = J(i, 1, .)
    A[1] = a; B[i] = b
    for (--i;i;i--) {
        A[i+1] = mis[i] + 1
        B[i]   = mis[i] - 1
    }
    // process each item 
    i = rows(A)
    I = J(i, 1, NULL)
    J = 0
    for (;i;i--) {
        I[i] = &(*f)(J(1,2,.) \ XY[|A[i],1 \ B[i],2|], mask, line)
        J = J + rows(*I[i])
    }
    // collect results
    xy = J(J, 2, .)
    for (i=length(I);i;i--) {
        j = rows(*I[i])
        if (!j) continue // empty shape item
        xy[|J-j+1,. \ J,.|] = *I[i]
        J = J - j
    }
    return(xy)
}

`RM' _geo_clip_point(`RM' XY, `RM' mask)
{
    if (cols(mask)==1) return(select(XY, __geo_rclip_point(XY, mask)))
    return(select(XY, __geo_clip_point(XY, mask)))
}

`BoolC' __geo_rclip_point(`RM' XY, `RC' mask)
{
    `RC' in

    in = J(rows(XY),1,1)
    if (mask[1]<.) in = in :& (_geo_clip_outside(XY, mask[1]):<=0)
    if (mask[3]<.) in = in :& (_geo_clip_outside(XY[,(2,1)], mask[3]):<=0)
    if (mask[2]<.) in = in :& (-_geo_clip_outside(XY, mask[2]):<=0)
    if (mask[4]<.) in = in :& (-_geo_clip_outside(XY[,(2,1)], mask[4]):<=0)
    return(in)
}

`BoolC' __geo_clip_point(`RM' XY, `RM' mask)
{
    `Int' i, n
    `RC'  in

    in = J(rows(XY),1,1)
    n = rows(mask)
    for (i=1;i<n;i++) {
        in = in :& (_geo_clip_outside(XY, mask[|i,1 \ i+1,2|]):<=0)
    }
    return(in)
}

`RM' __geo_clip(`RM' XY, `RM' mask, `Bool' line)
{
    `Bool' polygon, flip
    `Int'  i, n
    `PS'   f

    // line vs polygon
    if (rows(XY)==2) polygon = 0 // single point input
    else             polygon = (XY[2,]==XY[rows(XY),]) // closed path
    // line clipping
    if (line | !polygon) {
        if (polygon) {
            // rearrange points so that polygon does not start inside
            if (_geo_clip_larrange(XY, mask)) return(XY) // all points inside
        }
        f = &_geo_clip_line()
        flip = 0
    }
    else {
        f = &_geo_clip_area()
        flip = _geo_orientation(XY[|2,1 \ .,.|])==1 // is counterclockwise
        if (flip) XY = J(1,2,.) \ XY[rows(XY)::2,.] // flip orientation
    }
    if (cols(mask)==1) { // rclip
        if (mask[1]<.) { // xmin
            XY = (*f)(XY, mask[1])
        }
        if (mask[3]<.) { // ymin (-90° rotation)
            XY = (*f)((XY[,2], -XY[,1]), mask[3])
            XY = (-XY[,2], XY[,1])
        }
        if (mask[2]<.) { // xmax (-180° rotation)
            XY = -(*f)(-XY, -mask[2])
        }
        if (mask[4]<.) { // ymax (-270° rotation)
            XY = (*f)((-XY[,2], XY[,1]), -mask[4])
            XY = (XY[,2], -XY[,1])
        }
    }
    else {
        n = rows(mask)
        for (i=1;i<n;i++) XY = (*f)(XY, mask[|i,1 \ i+1,2|])
    }
    // returns
    if (flip) {
        if (rows(XY)) {
            XY = J(1,2,.) \ XY[rows(XY)::2,.] // restore orientation
        }
    }
    return(XY)
}

`Bool' _geo_clip_larrange(`RM' XY, `RM' mask)
{
    `Int'  i, n
    
    if (cols(mask)==1) { // rclip
        if (_geo_rclip_larrange(XY, mask[1], 1, 0)) return(0)
        if (_geo_rclip_larrange(XY, mask[3], 2, 0)) return(0)
        if (_geo_rclip_larrange(XY, mask[2], 1, 1)) return(0)
        if (_geo_rclip_larrange(XY, mask[4], 2, 1)) return(0)
    }
    else {
        n = rows(mask)
        for (i=1;i<n;i++) {
            if (__geo_clip_larrange(XY,
                _geo_clip_outside(XY, mask[|i,1 \ i+1,2|]))) return(0)
        }
    }
    return(1) // all points are inside
}

`Bool' _geo_rclip_larrange(`RM' XY, `RS' c, `Int' j, `Bool' neg)
{
    `IntC' out
    
    if (c>=.) return(0)
    if (neg) out = XY[,j] :> c
    else     out = XY[,j] :< c
    return(__geo_clip_larrange(XY, out))
}

`Bool' __geo_clip_larrange(`RM' XY, `IntC' out)
{
    `Int'  i, n

    n = rows(out)
    for (i=2;i<=n;i++) {
        if (out[i]==1) {
            if (i>2) XY = XY[1,] \ XY[|i,1 \ .,.|] \ XY[|3,1 \ i,.|]
            return(1)
        }
    }
    return(0)
}

`RM' _geo_clip_line(`RM' XY, `RM' c)
{   // first point assumed missing
    `Int'  i, j
    `IntC' out
    `RM'   xy
    
    // checks
    i = rows(XY)
    if (i<2) return(J(0,2,.)) // empty input
    out = _geo_clip_outside(XY, c)
    if (!anyof(out,1)) return(XY) // all points inside
    if (!any(out:<=0)) return(J(0,2,.)) // all points outside
    // apply clipping
    j = 2 * i
    xy = J(j++, 2, .)
    for (;i;i--) {
        if (out[i]>0) { // point is outside or missing
            if (out[i]>=.) continue // skip point if missing
            for (--i;i;i--) { // find first inside point or end of path
                if (out[i]<=0) { // inside point found
                    if (out[i]) // add point on edge
                        xy[--j,] = _geo_clip_intersect(c, XY[i+1,], XY[i,])
                    i++ // move back one point
                    break
                }
                if (out[i]>=.) break // end of path
            }
        }
        else { // point is inside
            xy[--j,] = XY[i,] // add inside point
            for (--i;i;i--) { // find first outside point or end of path
                if (out[i]<=0) { // point is inside
                    xy[--j,] = XY[i,] // add inside point
                    continue
                }
                // point is outside or missing (end of path)
                if (out[i]==1) { // point is outside
                    i++ // move back one point
                    if (out[i]) // add point on edge
                        xy[--j,] = _geo_clip_intersect(c, XY[i,], XY[i-1,])
                }
                break
            }
            j-- // add missing (terminate path)
        }
    }
    // return
    return(xy[|j,1 \ .,.|])
}

`RM' _geo_clip_area(`RM' XY, `RM' c)
{   // first point assumed missing
    `Int'   i, j
    `Bool'  hasentr
    `BoolC' isentr
    `IntC'  out
    `RM'    xy
    
    // checks
    i = rows(XY)
    if (i<2) return(J(0,2,.)) // empty input
    out = _geo_clip_outside(XY, c)
    if (!anyof(out,1)) return(XY) // all points inside
    if (!any(out:<=0)) return(J(0,2,.)) // all points outside
    _geo_clip_area_arrange(XY, out)
    // apply clipping
    j = 2 * i
    isentr = J(j, 1, 0)
    hasentr = 0
    xy = J(j++, 2, .)
    for (;i;i--) {
        if (out[i]>0) { // point is outside or missing
            if (out[i]>=.) continue // skip point if missing
            for (--i;i;i--) { // find first inside point or end of path
                if (out[i]<=0) { // inside point found
                    if (out[i]) { // add point on edge; this is the entry point
                        xy[--j,] = _geo_clip_intersect(c, XY[i+1,], XY[i,])
                        isentr[j] = 1
                    }
                    else isentr[j-1] = 1 // next point is the entry point
                    i++ // move back one point
                    hasentr = 1
                    break
                }
                if (out[i]>=.) break // end of path
            }
        }
        else { // point is inside
            xy[--j,]  = XY[i,] // add inside point
            for (--i;i;i--) { // find first outside point or end of path
                if (out[i]<=0) { // point is inside
                    xy[--j,]  = XY[i,] // add inside point
                    continue
                }
                // point is outside or missing (end of path)
                if (out[i]==1) { // point is outside
                    i++ // move back one point
                    if (out[i]) { // add point on edge
                        xy[--j,]  = _geo_clip_intersect(c, XY[i,], XY[i-1,])
                    }
                }
                break
            }
            isentr[--j] = . // add missing (terminate path)
        }
    }
    // return
    xy = xy[|j,1 \ .,.|]
    if (hasentr) xy = _geo_clip_area_join(xy, c, isentr[|j \. |])
    return(xy)
}

void _geo_clip_area_arrange(`RM' XY, `IntC' out)
{   // rearrange points so that each path starts with an outside point
    // (unless the path is fully inside)
    `Int' i, j, i1
    
    i = rows(out)
    for (;i;i--) {
        i1 = i
        for (;i;i--) {
            if (out[i]>=.) break // end of path
            if (out[i]==1) {
                j = i
                for (--i;i;i--) {
                    if (out[i]>=.) break // end of path
                }
                if (j<i1 & j>(i+1)) {
                    out[|i\i1|] = out[i] \ out[|j\i1|] \ out[|i+2\j|]
                    XY[|i,1\i1,.|] = XY[i,] \ XY[|j,1\i1,.|] \ XY[|i+2,1\j,.|]
                }
                break
            }
        }
    }
}

`RM' _geo_clip_area_join(`RM' XY, `RM' c, `BoolC' isentr)
{   // join pieces of paths crossing the boundary and close the polygons
    `Int' j, j1, k, K, i
    `RC'  yin, yout
    `RM'  ab
    `Int' p
    
    K = sum(isentr:==1)
    ab = J(K,4,.)
    j = rows(XY)
    p = J(j + K, 1, .)
    k = 0
    i = 1
    for (;j;j--) {
        j1 = j--
        while (isentr[j]<.) j--
        if (isentr[j1]==0) { // path is completely inside
            p[|i \ i + j1 - j|] = j::j1
            i = i + j1 - j + 1
            continue
        }
        ab[++k,] = 
            j, j1, _geo_clip_clpos(c, XY[j+1,]), _geo_clip_clpos(c, XY[j1,])
    }
    _sort(ab, -3)
    while (1) {
        p[|i \ i + ab[1,2] - ab[1,1]|] = ab[1,1]::ab[1,2]
        i = i + ab[1,2] - ab[1,1] + 1
        yin  = ab[1,3]
        yout = ab[1,4]
        while (1) {
            K = rows(ab)
            for (k=K;k>1;k--) {
                if (ab[k,3]>=ab[k,4]) continue // wrong orientation
                if (ab[k,3]<yout | ab[k,3]>yin) continue // above or below 
                p[|i \ i + ab[k,2] - ab[k,1] - 1|] = (ab[k,1]+1)::ab[k,2]
                i = i + ab[k,2] - ab[k,1] // (missing is skipped)
                yout = ab[k,4]
                ab = select(ab, (1::K):!=k)
                break
            }
            if (k<2) break
        }
        p[i++] = ab[1,1] + 1 // close polygon
        if (K==1) break
        ab = ab[|2,1 \ .,.|]
    }
    return(XY[p[|1 \ i-1|],])
}

`IntC' _geo_clip_outside(`RM' XY, `RM' c)
{   // result: 1 outside, -1 inside, 0 on edge, . missing
    if (length(c)==1) {
        // c is the lower limit of X; point is outside if x-value < c
        // (additionally make sure that missing if y-value is missing)
        return(sign(c :- XY[,1]) :* XY[,2]:^0)
    }
    // c is and edge: c = p1 \ p2 with p1 = (x1,y1) and p2 = (x2,x2)
    // check whether path p1 -> p2 -> XY[i,] turns left (inside)
    // or right (outside)
    return(sign((XY[,1] :- c[1,1]) * (c[2,2] - c[1,2])
              - (XY[,2] :- c[1,2]) * (c[2,1] - c[1,1])))
}

`RR' _geo_clip_intersect(`RM' c, `RR' p1, `RR' p2)
{   // obtain intersection between two lines
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    if (length(c)==1) {
        // c is the lower limit of X; compute y at which p1->p2 crosses c
        return((c, p1[2] + (p2[2]-p1[2]) * (c-p1[1]) / (p2[1]-p1[1])))
    }
    return(((c[1,1] * c[2,2] - c[1,2] * c[2,1]) :* (p1    - p2)
          - (p1[1]  * p2[2]  - p1[2]  * p2[1])  :* (c[1,] - c[2,]))
        / ((c[1,1]-c[2,1])*(p1[2]-p2[2]) - (c[1,2]-c[2,2])*(p1[1]-p2[1])))
}

`RS' _geo_clip_clpos(`RM' c, `RR' p)
{   // obtain position of point along clipping line
    real scalar r
    
    if (length(c)==1) return(p[2]) // c is the lower limit of X
    // rotate system
    r = atan2(c[2,2]-c[1,2], c[2,1]-c[1,1]) - pi()
    return(p[1]*sin(r) + p[2]*cos(r))
}

end

*! {smcl}
*! {marker geo_hull}{bf:geo_hull()}{asis}
*! version 1.0.0  30jun2023  Ben Jann
*!
*! Determine convex hull around cloud of points using Graham scan; based on
*! https://en.wikipedia.org/wiki/Graham_scan and
*! https://www.geeksforgeeks.org/convex-hull-using-graham-scan/
*!
*! Syntax:
*!
*!      result = geo_hull(XY)
*!
*!  result  real matrix containing (X,Y) of convex hull (counterclockwise)
*!  XY      n x 2 real matrix containing points; X in col 1, Y in col 2;
*!             should not contain missing values
*!

mata
mata set matastrict on

real matrix geo_hull(real matrix XY)
{
    real scalar    i0, i, j, n
    real colvector p, s
    
    n  = rows(XY)
    if (n==0) return(J(0,2,.))     // no data
    if (n==1) return(XY[,(1,2)])   // only one point
    // get reference point
    i0 = _geo_hull_refpoint(XY)
    // obtain counterclockwise order of remaining points along half-circle
    p  = _geo_hull_order(XY, i0)
    // obtain convex hull
    n  = length(p)
    if (n<3) return(XY[(i0\p\i0),(1,2)]) // 3 points or less
    s = J(n+1,1,.) // initialize stack
    s[(1,2,3)] = i0 \ p[1] \p[2]; j = 3 // first three points
    for (i=3;i<=n;i++) {
        while (1) {
            if (j<2) break
            if (_geo_hull_leftturn(XY[s[j-1],], XY[s[j],], XY[p[i],])) break
            j--
        }
        j++
        s[j] = p[i]
    }
    p = s[|1\j|] \ i0
    return(XY[p,(1,2)])
}

real scalar _geo_hull_refpoint(real matrix XY)
{   // get index of point with lowest X (and lowest Y within ties)
    real colvector i, j
    real matrix    w
    
    i = j = w = .
    minindex(XY[,2], 1, i, w)
    if (length(i)==1) return(i)
    minindex(XY[i,1], 1, j, w)
    return(i[j[1]])
}

real colvector _geo_hull_order(real matrix XY, real scalar i0)
{
    real colvector p, dx, dy, q, a, d
    
    p = 1::rows(XY) // initialize permutation vector
    dx = XY[,1] :- XY[i0,1] // X distance to point i0
    dy = XY[,2] :- XY[i0,2] // Y distance to point i0
    p = select(p, !(dx:==0 :& dy:==0)) // remove point i0 and its duplicates
    if (length(p)==0) return(J(0,1,.)) // no remaining points
    q = sign(dx) // 1 = 1st quadrant, -1 = 2nd quadrant, 0 = on vertical edge
    a = dy :/ dx // order of "angle"; positive in 1st, negative in 2nd quadrant
    _geo_hull_editmin(a, q) // replace missing in 2nd quadrant by mindouble()
    d = dx:^2 + dy:^2 // (squared) distance from point i0
    // sort points by angle and distance with respect to point i0
    _collate(p, order((q,a,d)[p,], (-1,2,3)))
    // ties in angle: keep point with largest distance to point i0
    return(p[selectindex(_mm_uniqrows_tag((q,a)[p,], 1))])
}

void _geo_hull_editmin(real colvector a, real colvector q)
{
    real colvector p
    
    p = selectindex(a:>=. :& q:<0)
    if (length(p)) a[p] = J(length(p), 1, mindouble())
}

real scalar _geo_hull_leftturn(real rowvector p1, real rowvector p2,
    real rowvector p3)
{
    real scalar d
    
    d = (p2[2] - p1[2]) * (p3[1] - p2[1]) - (p2[1] - p1[1]) * (p3[2] - p2[2])
    return(d<0)
}

end

*! {smcl}
*! {marker geo_orientation}{bf:geo_orientation()}{asis}
*! version 1.0.1  10oct2023  Ben Jann
*!
*! Determine orientation of polygon
*! see https://en.wikipedia.org/wiki/Curve_orientation
*!
*! Syntax:
*!
*!      result = geo_orientation(XY)
*!
*!  result  1 if orientation is positive (counterclockwise), -1 if negative
*!          (clockwise), 0 if undefined
*!  XY      n x 2 real matrix containing points of polygon; rows containing
*!          missing will be ignored
*!

mata
mata set matastrict on

real scalar geo_orientation(real matrix XY)
{   // orientation of polygon
    if (cols(XY)!=2) {
        errprintf("input must have two columns\n")
        exit(3200)
    }
    if (!rows(XY)) return(0) // no data; orientation undefined
    if (hasmissing(XY)) return(_geo_orientation(select(XY, !rowmissing(XY))))
    return(_geo_orientation(XY))
}

real scalar _geo_orientation(real matrix XY)
{   // result: -1 = clockwise, 1 = counterclockwise, 0 = undefined
    real scalar    n, i, a
    real colvector A, B, C
  
    n = rows(XY)
    if (!n) return(0) // no data; orientation undefined
    a = _geo_hull_refpoint(XY)
    A = XY[a,]
    for (i=1;i<n;i++) {
        B = XY[mod(a + i - 1, n) + 1,]
        if (B!=A) break
    }
    if (i==n) return(0) // second point not found; orientation undefined
    for (i=1;i<n;i++) {
        C = XY[mod(a - i - 1, n) + 1,]
        if (C!=A) {
            if (C!=B) break
        }
    }
    if (i==n) return(0) // third point not found; orientation undefined
    return(sign((B[1]-A[1]) * (C[2]-A[2]) - (C[1]-A[1]) * (B[2]-A[2])))
}

end

*! {smcl}
*! {marker geo_pid}{bf:geo_pid()}{asis}
*! version 1.0.1  05oct2023  Ben Jann
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
*!          if neither the first nor the last coordinate within an ID is
*!          missing, point data is assumed (i.e. each row within the ID is
*!          counted as a separate single-point polygon)
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
    if (!p[1]) p = J(n,1,1)            // assume point data
    return(runningsum(p))
}

end

*! {smcl}
*! {marker geo_plevel}{bf:geo_plevel()}{asis}
*! version 1.0.1  02oct2023  Ben Jann
*!
*! Determines plot levels of polygons: 0 = neither enclave nor exclave, 1 =
*! enclave, 2 = exclave, 3 = enclave within exclave, 4 = exclave within
*! enclave, etc.
*!
*! Syntax:
*!
*!      result = geo_plevel(rtype, ID, PID, XY [, nodots])
*!
*!  rtype   results type: if rtype!=0 the result is a real colvector of length
*!          n containing repeated plot level values, else the result is a r x 2
*!          matrix with ID in 1st row, PID in 2nd row, and plot level in 3rd
*!          row, where r is the total number of polygons
*!  ID      real colvector of unit IDs
*!  PID     real colvector of within-unit polygon IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!  nodots  do not display progress dots if nodots!=0
*!

local Bool      real scalar
local Int       real scalar
local IntC      real colvector
local RS        real scalar
local RC        real colvector
local RM        real matrix
local SS        string scalar
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

`RM' geo_plevel(`Int' rtype, `RC' ID, `RC' PID, `RM' XY, | `Bool' nodots)
{
    `Int'    i, j, a, b, i0, n
    `RM'     P
    `pUnits' u
    
    if (args()<5) nodots = 0
    // collect units and polygons
    u = _geo_collect_units(ID, PID, XY)
    n = length(u)
    // determine within unit plot levels
   if (!nodots) i0 = _geo_progress_init("pass 1/2: ")
    for (i=n;i;i--) {
        _geo_plevel_within(*u[i])
        if (!nodots) _geo_progressdots(1-(i-1)/n, i0)
    }
    // update plot levels based on between unit comparisons
    if (!nodots) i0 = _geo_progress_init("pass 2/2: ")
    for (i=n;i;i--) {
        for (j=i-1; j; j--) _geo_plevel_between(*u[i], *u[j])
        if (!nodots) _geo_progressdots(1-(i-1)/n, i0)
    }
    // fill in result
    if (rtype) {
        P = J(rows(ID), 1, .)
        for (i=n;i;i--) {
            for (j=u[i]->n;j;j--) {
                a = u[i]->p[j]->a
                b = u[i]->p[j]->b
                P[|a \ b|] = J(b-a+1, 1, u[i]->p[j]->l)
            }
        }
        return(P)
    }
    a = 0
    for (i=n;i;i--) a = a + u[i]->n
    P = J(a,3,.)
    for (i=n;i;i--) {
        for (j=u[i]->n;j;j--) {
            P[a,] = (u[i]->id, u[i]->p[j]->id, u[i]->p[j]->l)
            a--
        }
    }
    return(P)
}

`Int' _geo_progress_init(`SS' msg)
{
    displayas("txt")
    printf("(%s0%%", msg)
    displayflush()
    return(0)
}

void _geo_progressdots(`Int' p, `Int' j)
{
    while (1) {
        if (p < (j+1)/40) break
        j++
        if (mod(j,4)) {
            printf(".")
            displayflush()
        }
        else {
            printf("%g%%", j/4*10)
            if (j==40) printf(")\n")
            displayflush()
        }
    }
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
*!      result = geo_rotate(XY, angle)
*!
*!  result  n x 2 real matrix of rotated (X,Y) coordinates
*!  XY      n x 2 real matrix of (X,Y) coordinates
*!  angle   real scalar specifying angle
*!
*! Rotate in place:
*!
*!      _geo_rotate(XY, angle)
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
*! version 1.0.1  02oct2023  Ben Jann
*!
*! Spatially joins the points in xy to the polygons in XY
*!
*! Syntax:
*!
*!      result = geo_spjoin(xy, ID, PID, XY [, PL, nodots])
*!
*!  result  r x 1 colvector containing matched IDs
*!  xy      r x 2 matrix containing points to be matched
*!  ID      n x 1 real colvector of unit IDs
*!  PID     n x 1 real colvector of within-unit polygon IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons
*!  PL      optional n x 1 real colvector containing plot level indicator 
*!  nodots  do not display progress dots if nodots!=0
*!

local Bool      real scalar
local Int       real scalar
local IntC      real colvector
local RS        real scalar
local RC        real colvector
local RM        real matrix
local SS        string scalar
local POLYGON   _geo_POLYGON
local Polygon   struct `POLYGON' scalar
local pPolygon  pointer (`Polygon') scalar
local UNIT      _geo_UNIT
local Unit      struct `UNIT' scalar
local pUnits    pointer (`Unit') vector

mata:
mata set matastrict on

`RC' geo_spjoin(`RM' xy, `RC' ID, `RC' PID, `RM' XY, | `RC' PL, `Bool' nodots)
{
    `Int'    i, n
    `IntC'   p, P
    `RC'     id, L
    pointer  pl
    
    if (args()<6) nodots = 0
    if (!rows(PL)) return(_geo_spjoin(xy, ID, PID, XY, nodots, "pass 1/1: "))
    if (hasmissing(PL)) pl = &editmissing(PL, 0) // treat missing as 0
    else                pl = &PL
    L  = mm_unique(select(*pl, _mm_uniqrows_tag((ID, PID))))
    if (length(L)) L = select(L, !mod(L,2)) // skip enclaves
    id = J(rows(xy), 1, .)
    p  = . // use all points in first round
    n = length(L)
    for (i=n;i;i--) {
        P     = selectindex(*pl:==L[i])
        id[p] = _geo_spjoin(xy[p,], ID[P], PID[P], XY[P,], nodots,
            sprintf("pass %g/%g: ", n-i+1, n))
        if (i>1) p = selectindex(id:>=.) // remaining unmatched points
    }
    return(id)
}

`RC' _geo_spjoin(`RM' xy, `RC' ID, `RC' PID, `RM' XY, `Bool' nodots, `SS' msg)
{
    `Int'    i, i0, n
    `RC'     id
    `pUnits' u
    
    if (!nodots) i0 = _geo_progress_init(msg)
    u = _geo_collect_units(ID, PID, XY)
    n = rows(xy)
    id = J(n,1,.)
    for (i=n;i;i--) {
        id[i] = __geo_spjoin(xy[i,1], xy[i,2], u)
        if (!nodots) _geo_progressdots(1-(i-1)/n, i0)
    }
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
*! version 1.0.2  06jul2023  Ben Jann
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
    real matrix    xy
    pragma unused arg
    
    if (n0>=.) n = 100
    else       n = n0
    r = .25 - .5/n // set angle such that shape has horizontal edge at bottom
    r = rangen(-r, 1-r, n+1) * (2 * pi())
    xy = (cos(r), sin(r))
    xy[n+1,] = xy[1,] // for sake of precision
    return(xy)
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

real matrix _geo_symbol_star(| real scalar n, string scalar arg)
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

real matrix _geo_symbol_star6(| real scalar n, string scalar arg)
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

real matrix _geo_symbol_hexagram(| real scalar n, string scalar arg)
{
    real matrix XY
    pragma unused n
    pragma unused arg
    
    XY = _geo_symbol_star6()
    return(XY[(1,5,9,1),] \ (.,.) \ XY[(7,11,3,7),])
}

real matrix _geo_symbol_pentagram(| real scalar n, string scalar arg)
{
    pragma unused n
    pragma unused arg
    
    return(_geo_symbol_star()[(1,5,9,3,7,1),])
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
    // outer polygon (counterclockwise)
    c = 2 - a
    r = rangen(-asin(a/c), pi()+asin(a/c), n+1)
    xy0 = a * (cos(r),sin(r))
    xy0[,2] = xy0[,2] :+ c
    xy0 = xy0 \ (0,0) \ xy0[1,] 
    xy0 = xy0, J(rows(xy0),1,0) // plevel=0
    // inner polygon (clockwise)
    r = rangen(1, 0, n+1) * (2 * pi())
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

