*! Source of lgeoplot.mlib
*! {smcl}
*!     {helpb lgeoplot_source##geo_area:geo_area()}
*!     {helpb lgeoplot_source##geo_bbox:geo_bbox()}
*!     {helpb lgeoplot_source##geo_bshare:geo_bshare()}
*!     {helpb lgeoplot_source##geo_centroid:geo_centroid()}
*!     {helpb lgeoplot_source##geo_circle_tangents:geo_circle_tangents()}
*!     {helpb lgeoplot_source##geo_clip:geo_clip()}
*!     {helpb lgeoplot_source##geo_gtype:geo_gtype()}
*!     {helpb lgeoplot_source##geo_hull:geo_hull()}
*!     {helpb lgeoplot_source##geo_json:geo_json_import() and geo_json2xy()}
*!     {helpb lgeoplot_source##geo_orientation:geo_orientation()}
*!     {helpb lgeoplot_source##geo_pid:geo_pid()}
*!     {helpb lgeoplot_source##geo_plevel:geo_plevel()}
*!     {helpb lgeoplot_source##geo_pointinpolygon:geo_pointinpolygon()}
*!     {helpb lgeoplot_source##geo_project:geo_project()}
*!     {helpb lgeoplot_source##geo_refine:geo_refine()}
*!     {helpb lgeoplot_source##geo_rotate:geo_rotate()}
*!     {helpb lgeoplot_source##geo_simplify:geo_simplify()}
*!     {helpb lgeoplot_source##geo_spjoin:geo_spjoin()}
*!     {helpb lgeoplot_source##geo_symbol:geo_symbol()}
*!     {helpb lgeoplot_source##geo_tissot:geo_tissot()}
*!     {helpb lgeoplot_source##geo_welzl:geo_welzl()}
*!     {helpb lgeoplot_source##geo_wkt2xy:geo_wkt2xy()}
*!     {helpb lgeoplot_source##_geo_progress:_geo_progress()}
*!     {help  lgeoplot_source##varia:Further functions}
*! {asis}

version 16.1

*! {smcl}
*! {marker geo_area}{bf:geo_area()}{asis}
*! version 1.0.2  29dec2023  Ben Jann
*!
*! Computes area of each unit using the Shoelace formula; see
*! https://en.wikipedia.org/wiki/Shoelace_formula. Only polygons are considered.
*! Nested polygons within a unit are assumed to switch orientation (so that
*! "holes" will be deducted from the total area of the unit). Zero will be
*! returned for units that do not contain polygons.
*!
*! geo_gtype() is used to classify the geometry items within a unit; polygons
*! are assumed to start with missing.
*!
*!      result = geo_area(rtype, ID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a n x 1 vector containing
*!          repeated area values, else the result is a r x 1 matrix with ID in
*!          1st column and area in 2nd column, where r is the number of units
*!  ID      n x 1 real colvector of unit IDs (or, possibly, scalar if single
*!          unit); rows are assumed to be grouped by unit ID
*!  XY      n x 2 or n x 4 real matrix of (X,Y) coordinates of the geometry
*!          items; if XY is n x 4 (paired coordinates), area will be zero for
*!          all units
*!

mata:
mata set matastrict on

real matrix geo_area(real scalar rtype, real colvector ID, real matrix XY)
{
    real scalar    i, n, a, b
    real colvector p
    real matrix    A
    
    if (!anyof((2,4), cols(XY))) {
        errprintf("{it:XY} must have two or four columns\n")
        exit(3200)
    }
    n = rows(XY)
    i = rows(ID)
    if (i!=1) {
        if (i!=n) exit(error(3200))
    }
    if (!n) return(J(0, 1 + !rtype, .)) // no data
    if (n==1) { // single obs
        if (rtype) return(0)
        else       return(ID, 0)
    }
    if (rows(ID)==1) { // single unit
        p = 1
        A = _geo_area(XY)
    }
    else {
        p = selectindex(_mm_unique_tag(ID))
        i = rows(p)
        if (i==n) { // one obs per unit
            if (rtype) return(J(i,1,0))
            else       return(ID, J(i,1,0))
        }
        A = J(i,1,.)
        a = n + 1
        for (;i;i--) {
            b = a - 1; a = p[i]
            A[i] = _geo_area(XY[|a,1 \ b,.|])
        }
    }
    if (rtype) {
        i = rows(p)
        a = n + 1
        A = A \ J(n-i,1,.)
        for (;i;i--) {
            b = a - 1; a = p[i]
            A[|a \ b|] = J(b-a+1, 1, A[i,])
        }
        return(A)
    }
    else return(ID[p], A)
}

real rowvector _geo_area(real matrix XY)
{
    real colvector g, cnt
    
    if (cols(XY)>2) return(0) // pc
    g = geo_gtype(3, ., geo_pid(., XY), XY, cnt=.)
    if (cnt[4]) { // area of polygons
        if (sum(cnt)==cnt[4]) return(__geo_area(XY))
        return(__geo_area(select(XY, g:==3)))
    }
    return(0)
}

real rowvector __geo_area(real matrix XY)
{
    real scalar A
    
    if (rows(XY)<2) return(0)
    A = sum(rowsum((1,-1) :* XY :* (XY[|2,1\.,.|] \ (.,.))[,(2,1)]))
    return(abs(editmissing(A,0))/2)
}

end

*! {smcl}
*! {marker geo_bbox}{bf:geo_bbox()}{asis}
*! version 1.0.1  01jun2024  Ben Jann
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
*!  XY      n x 2 real matrix containing the points; rows containing missing
*!          will be ignored
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
    if (args()<2) type = 0
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (!hasmissing(XY)) return(_geo_bbox(XY, type))
    return(_geo_bbox(select(XY, !rowmissing(XY)), type))
}

real matrix _geo_bbox(real matrix XY, real scalar type)
{
    if (type==1)  return(_geo_bbox_rc(XY))
    if (type==2)  return(_geo_bbox_rc(XY, 1))
    if (type==-1) return(_geo_bbox_tilt(XY))
    if (type==-2) return(_geo_bbox_tilt(XY,1))
    return(_geo_bbox_regular(XY))
}

real matrix _geo_bbox_regular(real matrix XY)
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
    xy = _geo_hull(XY)
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
    xy = _geo_hull(XY)
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
*! {marker geo_bshare}{bf:geo_bshare()}{asis}
*! version 1.2.1  05jul2024  Ben Jann
*!
*! Algorithm to extract/identify shared borders between shape items; a shared
*! border is defined as an identical sequence of vertices (apart from
*! orientation) that exists in two shape items.
*!
*! Syntax:
*!
*!      result = geo_bshare(ID, PID, XY [, rtype, nodots ])
*!
*!  result  result depending on rtype (see below)
*!  ID      real colvector of unit IDs
*!  PID     real colvector of within-unit shape item IDs
*!  XY      n x 2 real matrix containing the (X,Y) coordinates of the shape
*!          items
*!  rtype   real scalar setting the type of output
*!          0: result will be a r x 5 matrix containing shape items representing
*!             shared borders; each shared border is only included once; columns
*!             are (id, id1, id2, x, y), where id contains a unique id for each
*!             item, id1 and id2 contain the ids of the units that share the
*!             border, x and y are the coordinates; r is the total length across
*!             all items; id1 and id2 will be the same for shared borders
*!             between items within a single item
*!          1: result will be a 2*r x 3 matrix containing shape items
*!             representing the shared borders of each unit (each shared border
*!             will be included twice); columns are (id, x, y) where id is the
*!             unit id and x and y are the coordinates
*!          2: result will be a r x 3 matrix containing for each unit a set of
*!             shape items representing the segments that are not shared
*!             borders; columns are (id, x, y) where id is the unit id and x and
*!             y are the coordinates; q is the total length across all included
*!             items
*!          3: result will be a n x 1 vector that tags all points in XY equal to
*!             the start or the end of a shared border
*!          4: result will be a n x 1 vector that tags all points in XY that are
*!             part of a shared border
*!          5: result will be a n x 1 vector that tags all points in XY equal to
*!             the start of a path with the length of the path
*!          default is 0; values other than the ones above are treated as 0
*!  nodots  nodots!=0 suppresses progress dots
*!
*! The algorithm assumed that the shape items have no repeated points, apart
*! from the first and last point in a polygon.
*!

local Bool    real scalar
local BoolC   real colvector
local Int     real scalar
local IntC    real colvector
local IntM    real matrix
local RS      real scalar
local RC      real colvector
local RR      real rowvector
local RM      real matrix
local ITEM    _geo_bshare_ITEM
local Item    struct `ITEM' scalar
local pItems  pointer (`Item') colvector
local BRDR    _geo_bshare_BRDR
local Brdr    struct `BRDR' scalar
local pBrdrs  pointer (`Brdr') colvector
local BRDRS   _geo_bshare_BRDRS
local Brdrs   struct `BRDRS' scalar
local NBRDRS  _geo_bshare_NBRDRS
local Nbrdrs  struct `NBRDRS' scalar
local pNbrdrs pointer (`RM') colvector

mata:

struct `ITEM' {    // shape item
    `Int'  id, pid // unit id and within-unit item id
    `Int'  a, b    // range indices of item in original data
    `Int'  gt      // 1 = point, 2 = line, 3 = polygon, 0 = empty
    `Bool' mis     // has leading row
    `RM'   XY      // coordinates of item (without leading row)
    `RS'   xmin, xmax, ymin, ymax // limits of data range 
}

struct `BRDR' {        // shared-border info
    `Int'    id1, id2  // internal ids of involved items
    `Int'    a1, a2    // within-item start indices
    `Int'    l         // length of shared border
}

struct `BRDRS' { // collection of shared border infos
    `Int'    n   // current length
    `pBrdrs' B   // stack of borders
}

struct `NBRDRS' { // collection of non-shared border infos
    `Int'     n   // current length
    `pNbrdrs' B   // stack of borders
}

`RM' geo_bshare(`RC' ID, `RC' PID, `RM' XY, | `Int' rtype, `Bool' nodots)
{
    `Int'    i, j, d0, dn, d
    `pItems' P
    `Brdrs'  B
    
    if (args()<4) rtype = 0
    if (args()<5) nodots = 0
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    // collect polygons
    P = _geo_bshare_items(ID, PID, XY)
    // identify common borders
    i = length(P)
    if (!nodots) {
        displayas("txt")
        printf("(search for shared borders among %g shape items)\n", i)
        d0 = _geo_progress_init("(")
        dn = comb(i, 2); d = 0
    }
    B.n = 0 // initialize stack
    for (;i;i--) {
        for (j=i-1;j;j--) {
            _geo_bshare(B, *P[i], *P[j], i, j)
            if (!nodots) _geo_progress(d0, ++d/dn)
        }
    }
    if (!nodots) _geo_progress_end(d0, ")")
    // return
    if (rtype==1) return(_geo_bshare_r1(B, P))
    if (rtype==2) return(_geo_bshare_r2(B, P, rows(XY)))
    if (rtype==3) return(_geo_bshare_tag(3, B, P, rows(XY)))
    if (rtype==4) return(_geo_bshare_tag(4, B, P, rows(XY)))
    if (rtype==5) return(_geo_bshare_tag(5, B, P, rows(XY)))
                  return(_geo_bshare_r0(B, P))
}

`pItems' _geo_bshare_items(`RC' ID, `RC' PID, `RM' XY)
{   // return pointer vector of all shape items
    `Int'    i, a, b
    `IntC'   p
    `pItems' P
    
    p = selectindex(_mm_uniqrows_tag((ID,PID)))
    i = rows(p)
    P = J(i, 1, NULL)
    a = rows(XY) + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        P[i] = &_geo_bshare_item(ID, PID, XY, a, b)
    }
    return(P)
}

`Item' _geo_bshare_item(`RC' ID, `RC' PID, `RM' XY, `Int' a, `Int' b)
{   // collect shape item
    `RM'   minmax
    `Item' p
    
    p.id   = ID[a]
    p.pid  = PID[a]
    p.gt   = _geo_gtype(XY, a, b)
    p.a    = a
    p.b    = b
    p.mis  = b>a // ignore first row if more than one row
    if (!p.gt) return(p) // empty item
    p.XY   = XY[|a+p.mis,1 \ b,2|]
    minmax = colminmax(p.XY)
    p.xmin = minmax[1,1]
    p.xmax = minmax[2,1]
    p.ymin = minmax[1,2]
    p.ymax = minmax[2,2]
    return(p)
}

void _geo_bshare_add_brdr(`Brdrs' B, `Int' id1, `Int' id2, `Int' a1, `Int' a2,
    `Int' l)
{   // add info on shared border to collection
    `Brdr' b
    
    b.id1 = id1
    b.id2 = id2
    b.a1  = a1
    b.a2  = a2
    b.l   = l
    B.n = B.n + 1
    if (B.n>length(B.B)) B.B = B.B \ J(100,1,NULL)
    B.B[B.n] = &b
}

void _geo_bshare_add_nbrdr(`Nbrdrs' N, `RM' idxy)
{   // add data of non-shared border to collection
    N.n = N.n + 1
    if (N.n>length(N.B)) N.B = N.B \ J(100,1,NULL)
    N.B[N.n] = &idxy[.,.]
}

`RM' _geo_bshare_r0(`Brdrs' A, `pItems' P)
{   // return shared borders (unique)
    `Int'  i, id, n, a, b
    `Item' p
    `Brdr' B
    `RM'   R
    
    n = 0
    for (i=A.n;i;i--) n = n + A.B[i]->l + 1
    R = J(n,5,.) // id, id1, id2, x, y
    id = b = 0
    for (i=A.n;i;i--) {
        id++
        B = *A.B[i]
        p = *P[B.id1]
        a = b + 1; b = a + B.l
        R[|a,1 \ b,3|] = J(b-a+1, 1, (id, p.id, P[B.id2]->id))
        a++
        R[|a,4 \ b,5|] = __geo_bshare_r0(p.XY, B.a1, B.l)
    }
    return(R)
}

`RM' __geo_bshare_r0(`RM' XY, `Int' a, `Int' l)
{   // extract path from XY
    `Int' b, n
    
    n = rows(XY)
    b = a + l - 1
    if (b<=n) return(XY[|a,1 \ b,2|])
    return(XY[|a,1 \ n,2|] \ XY[|2,1 \ b-n+1,2|])
}

`RM' _geo_bshare_r1(`Brdrs' A, `pItems' P)
{   // return shared borders (with duplicates)
    `Int'   i, n, a, b
    `Item'  p
    `Brdr'  B
    `RM'    R
    
    n = 0
    for (i=A.n;i;i--) n = n + A.B[i]->l + 1
    R = J(2*n,3,.) // id, x, y
    b = 0
    for (i=A.n;i;i--) {
        B = *A.B[i]
        // Item 1
        p = *P[B.id1]
        a = b + 1; b = a + B.l
        R[|a,1 \ b,1|] = J(b-a+1, 1, p.id)
        a++
        R[|a,2 \ b,3|] = __geo_bshare_r0(p.XY, B.a1, B.l)
        // Item 2
        p = *P[B.id2]
        a = b + 1; b = a + B.l
        R[|a,1 \ b,1|] = J(b-a+1, 1, p.id)
        a++
        R[|a,2 \ b,3|] = __geo_bshare_r0(p.XY, B.a2, B.l)
    }
    return(mm_sort(R,1,1))
}

`RM' _geo_bshare_r2(`Brdrs' B, `pItems' P, `Int' n)
{   // return non-shared borders
    `Int'    r, i, a, b
    `IntC'   R, S
    `Nbrdrs' N
    `RM'     idxy
    
    // tag shared borders
    R = _geo_bshare_tag(3, B, P, n) + _geo_bshare_tag(4, B, P, n)
    S = _geo_bshare_tag(5, B, P, n)
    // extract non-shared borders
    N.n = r = 0 // initialize stack
    for (i=rows(P);i;i--) __geo_bshare_r2(N, r, R, S, *P[i])
    // collect results
    idxy = J(r, 3, .) // id, x, y
    b = 0
    for (i=N.n;i;i--) {
        a = b + 1
        b = b + rows(*N.B[i])
        idxy[|a,1 \ b,3|] = *N.B[i]
    }
    return(mm_sort(idxy,1,1))
}

void __geo_bshare_r2(`Nbrdrs' N, `Int' r, `IntC' R, `IntC' S, `Item' p)
{   // extract segments of shape item that are not shared boders
    `Int'  a, a0, b, n, l
    `IntC' T, U
    
    T = R[|p.a+p.mis \ p.b|]
    U = S[|p.a+p.mis \ p.b|]
    n = rows(T)
    if (n==1) { // point item
        if (!T) _geo_bshare_add_nbrdr(N, (J(2,1,p.id), ((.,.) \ p.XY)))
        return
    }
    b = .
    a0 = 0
    for (a=n-1; a>a0; a--) { // look for sequences of type 2[0...]2
        if (T[a]==1) continue
        if (T[a]==0) continue
        // reached only if T[a]==2
        if (U[a]>1) { // current point is start of a path of length>1
            b = a
            continue
        }
        if (b>=.) {
            if (p.gt==3) { // wrap around
                for (; a0<a;a0++) {
                    if (T[a0+1]==0) continue
                    break // reached only if T[a0+1]==2
                }
                if (a0) { // moved at least one position
                    l = 2 + n - a + a0
                    r = r + l
                    _geo_bshare_add_nbrdr(N, (J(l,1,p.id),
                        ((.,.) \ p.XY[|a,1 \ n,2|] \ p.XY[|2,1 \ a0+1,2|])))
                    continue
                }
            }
            b = n
        }
        l = b - a + 2
        r = r + l
        _geo_bshare_add_nbrdr(N, (J(l,1,p.id), ((.,.) \ p.XY[|a,1 \ b,2|])))
    }
    if (!a) { // collect first segment if T starts with 0
        if (T[1]) return
        if (b>=.) { // item has no shared borders
            r = r + n + 1
            _geo_bshare_add_nbrdr(N, (J(n+1,1,p.id), ((.,.) \ p.XY)))
            return
        }
        l = b + 1
        r = r + l
        _geo_bshare_add_nbrdr(N, (J(l,1,p.id), ((.,.) \ p.XY[|1,1 \ b,2|])))
    }
}

`BoolC' _geo_bshare_tag(`Int' rtype, `Brdrs' A, `pItems' P, `Int' n)
{   // tag points on shared border
    `Int'   i
    `Item'  p
    `Brdr'  B
    `BoolC' R
    pointer (function) scalar f
    
    if      (rtype==4) f = &_geo_bshare_r4() // tag all points on shared border
    else if (rtype==5) f = &_geo_bshare_r5() // store length in first point
    else               f = &_geo_bshare_r3() // tag start and end
    R = J(n,1,0)
    for (i=A.n;i;i--) {
        B = *A.B[i]
        // handle item 1
        p = *P[B.id1]
        (*f)(R, p.a+p.mis, p.b, B.a1, B.l, p.gt!=3)
        // handle item 2
        p = *P[B.id2]
        (*f)(R, p.a+p.mis, p.b, B.a2, B.l, p.gt!=3)
    }
    return(R)
}

void _geo_bshare_r3(`BoolC' R, `Int' a, `Int' b, `Int' i, `Int' l,
    `Bool' nowrap)
{   // tag start and end points of shared borders
    `Int' j

    R[a + i - 1] = 1
    j = a + i + l - 2
    if (nowrap)   R[j] = 1
    else if (j<b) R[j] = 1
    else if (j>b) R[a + j-b] = 1
    else          R[a] = R[b] = 1 
}

void _geo_bshare_r4(`BoolC' R, `Int' a, `Int' b, `Int' i, `Int' l,
    `Bool' nowrap)
{   // tag all points that are on a shared border
    `Int' j0, j1

    j0 =  a + i - 1
    j1 = j0 + l - 1
    if (nowrap)    R[|j0\j1|] = J(l,1,1)
    else if (j1<b) R[|j0\j1|] = J(l,1,1)
    else {
        R[|a\a+j1-b|] = J(j1-b+1,1,1)
        R[|j0\b|]     = J(b-j0+1,1,1)
    }
}

void _geo_bshare_r5(`BoolC' R, `Int' a, `Int' b, `Int' i, `Int' l,
    `Bool' nowrap)
{   // store length of shared border in first point of shared border (use
    // the length of the longest path if the point is the start of multiple
    // shared borders)
    `Int' j
    
    j = a + i - 1
    R[j] = max((l,R[j]))
    if (nowrap) return
    if      (j==a) R[b] = max((l,R[b]))
    else if (j==b) R[a] = max((l,R[a]))
}

void _geo_bshare(`Brdrs' B, `Item' pi, `Item' pj, `Int' id1, `Int' id2)
{   // find shared borders among shape items
    `Int'   i0, i, ni, j, nj, l
    `BoolC' tj
    `IntC'  J
    `RR'    xy
    `RM'    iXY, jXY
    
    if (!(pi.gt))          return // pi is empty
    if (!(pj.gt))          return // pj is empty
    if (pi.xmax < pj.xmin) return // pi left of pj
    if (pi.xmin > pj.xmax) return // pi right of pj
    if (pi.ymax < pj.ymin) return // pi below pj
    if (pi.ymin > pj.ymax) return // pi above pj
    if (pi.gt==3) iXY = pi.XY[|2,1\.,.|]
    else          iXY = pi.XY
    if (pj.gt==3) jXY = pj.XY[|2,1\.,.|]
    else          jXY = pj.XY
    ni = rows(iXY); nj = rows(jXY)
    J = 1::nj; tj = J(nj,1,1) // initialize pj search index
    i0 = 0
    for (i=ni;i>i0;i--) {
        xy = iXY[i,]
        if (xy[1] < pj.xmin) continue // xy left of pj
        if (xy[1] > pj.xmax) continue // xy right of pj
        if (xy[2] < pj.ymin) continue // xy below pj
        if (xy[2] > pj.ymax) continue // xy above pj
        j = _geo_bshare_match(xy, jXY, J) // find match in pj
        if (!j) continue // no match found in pj
        tj[j] = 0 // mark point in pj as used
        l = _geo_bshare_path(i0, i, ni, pi.gt, iXY, j, nj, pj.gt, jXY, tj)
        _geo_bshare_add_brdr(B, id1, id2, i+(pi.gt==3), j+(pj.gt==3), l)
        J = selectindex(tj) // update pj search index
    }
}

`Int' _geo_bshare_match(`RR' xy, `RM' XY, `BoolC' J)
{   // find match of point in XY; use the last match in case of ties
    `Int' j
    
    if (!anyof(XY[J,1], xy[1])) return(0) // quick check in one dimension
    for (j=rows(J);j;j--) {
        if (xy==XY[J[j],]) return(J[j])
    }
    return(0)
}

`Int' _geo_bshare_path(`Int' i00, `Int' i, `Int' ni, `Int' gti, `RM' iXY,
    `Int' j, `Int' nj, `Int' gtj, `RM' jXY, `BoolC' tj)
{   // follow common path in item pi and item pj, starting from an initial match
    `Int' l, ii, i0, jj, j0, d

    // setup
    l = 1 // length is at least 1
    d = 0 // relative direction (-1 same, 0 not yet set, 1 opposite)
    // follow path downwards in item 2
    ii = i; jj = j
    __geo_bshare_path(l, min((i-1,j-1)), 0,ii,iXY, 0,jj,jXY, tj)
    j0 = jj // save start index of path in item 2
    if (jj==1 & gtj==3) { // wrap around if item 2 is polygon
        jj = nj + 1
        __geo_bshare_path(l, min((ii-1,nj+1-j)), 0,ii,iXY, 0,jj,jXY, tj)
        if (jj<=nj) j0 = jj // save start index of path in item 2
    }
    if (l>1) d = -1
    // follow path upwards in item 2 if downward search was not successful
    if (!d) {
        ii = i; jj = j
        __geo_bshare_path(l, min((i-1,nj-j)), 0,ii,iXY, 1,jj,jXY, tj)
        if (jj==nj & gtj==3) { // wrap around if item 2 is polygon
            jj = 0
            __geo_bshare_path(l, min((ii-1,j)), 0,ii,iXY, 1,jj,jXY, tj)
        }
        if (l>1) d = 1
    }
    if (i<ni | gti!=3) {; i = ii; j = j0; return(l); }
    // follow path upwards in item 1 (i.e. wrap around) if item 1 is polygon
    // and matching point is at end of item 1
    i0 = ii // save start index of path in item 1
    if (d<=0) { // follow path upwards in item 2
        ii = 0; jj = j
        __geo_bshare_path(l, min((i0,nj-j,nj+1-l)), 1,ii,iXY, 1,jj,jXY, tj)
        if (jj==nj & gtj==3) { // wrap around if item 2 is polygon
            jj = 0
            __geo_bshare_path(l, min((i0-ii,nj+1-l)), 1,ii,iXY, 1,jj,jXY, tj)
        }
        if (l>1) d = -1 // preserve direction if not yet set
    }
    if (d>=0) { // follow path downwards in item 2
        ii = 0; jj = j
        __geo_bshare_path(l, min((i0,j-1,nj+1-l)), 1,ii,iXY, 0,jj,jXY, tj)
        j0 = jj // save start index of path in item 2
        if (jj==1 & gtj==3) { // wrap around if item 2 is polygon
            jj = nj + 1
            __geo_bshare_path(l, min((i0-ii,nj+1-l)), 1,ii,iXY, 0,jj,jXY, tj)
            if (jj<=nj) j0 = jj // save start index of path in item 2
        }
    }
    if (ii>0) i00 = ii // update item 1 search range
    i = i0; j = j0     // return start indices
    return(l)          // return length of path (including first point)
}

void __geo_bshare_path(`Int' l, `Int' k, `Bool' iup, `Int' i, `RM' iXY,
    `Bool' jup, `Int' j, `RM' jXY, `BoolC' tj)
{   // follow path up or down
    // k = max possible remaining length of path in the specified direction
    if (iup) {
        if (jup) {
            for (;k;k--) {
                if (iXY[++i,]!=jXY[++j,]) {; i--; j--; break; }
                l++
                tj[j] = 0 // mark point in pj as used
            }
            return
        }
        for (;k;k--) {
            if (iXY[++i,]!=jXY[--j,]) {; i--; j++; break; }
            l++
            tj[j] = 0 // mark point in pj as used
        }
        return
    }
    if (jup) {
        for (;k;k--) {
            if (iXY[--i,]!=jXY[++j,]) {; i++; j--; break; }
            l++
            tj[j] = 0 // mark point in pj as used
        }
        return
    }
    for (;k;k--) {
        if (iXY[--i,]!=jXY[--j,]) {; i++; j++; break; }
        l++
        tj[j] = 0 // mark point in pj as used
    }
}

end

*! {smcl}
*! {marker geo_centroid}{bf:geo_centroid()}{asis}
*! version 1.0.2  29dec2023  Ben Jann
*!
*! Computes centroid of each unit. If a unit contains at least one polygon,
*! the formula for polygons is used (https://en.wikipedia.org/wiki/Centroid) and
*! lines and points in the unit will be ignored. Nested polygons within a unit
*! are assumed to switch orientation (so that "holes" will be deducted from the
*! total area of the unit). If a unit contains at least one line (but no 
*! polygons), the formula for lines will be used (weighted mean of midpoints of
*! line segments) and points in the unit will be ignored. If a unit only
*! contains points, the formula for points will be used (mean).
*!
*! geo_gtype() is used to classify the geometry items within a unit; polygons
*! and lines are assumed to start with missing.
*!
*!      result = geo_centroid(rtype, ID, XY)
*!
*!  rtype   results type: if rtype!=0 the result is a n x 2 matrix containing
*!          repeated centroids, else the result is a r x 3 matrix with ID in
*!          1st column and centroid coordinates in columns 2 and 3, where r is
*!          the number of units
*!  ID      n x 1 real colvector of unit IDs (or, possibly, scalar if single
*!          unit); rows are assumed to be grouped by unit ID
*!  XY      n x 2 or n x 4 real matrix of (X,Y) coordinates of the geometry
*!          items; if XY is n x 4 (paired coordinates), centroids are computed
*!          from the first coordinate
*!

mata:
mata set matastrict on

real matrix geo_centroid(real scalar rtype, real colvector ID, real matrix XY)
{
    real scalar    i, n, a, b
    real colvector p
    real matrix    C
    
    if (!anyof((2,4), cols(XY))) {
        errprintf("{it:XY} must have two or four columns\n")
        exit(3200)
    }
    n = rows(XY)
    i = rows(ID)
    if (i!=1) {
        if (i!=n) exit(error(3200))
    }
    if (!n) return(J(0, 2 + !rtype, .)) // no data
    if (n==1) { // single obs
        C = editmissing(XY[,(1,2)], .)
        if (rtype) return(C)
        else       return(ID, C)
    }
    if (rows(ID)==1) { // single unit
        p = 1
        C = _geo_centroid(XY)
    }
    else {
        p = selectindex(_mm_unique_tag(ID))
        i = rows(p)
        C = J(i,2,.)
        if (i==n) {
            for (;i;i--) C[i,] = editmissing(XY[i,(1,2)], .)
            if (rtype) return(C)
            else       return(ID, C)
        }
        a = n + 1
        for (;i;i--) {
            b = a - 1; a = p[i]
            C[i,] = _geo_centroid(XY[|a,1 \ b,.|])
        }
    }
    if (rtype) {
        i = rows(p)
        a = n + 1
        C = C \ J(n-i,2,.)
        for (;i;i--) {
            b = a - 1; a = p[i]
            C[|a,1 \ b,2|] = J(b-a+1, 1, C[i,])
        }
        return(C)
    }
    else return(ID[p], C)
}

real rowvector _geo_centroid(real matrix XY)
{
    real colvector g, cnt
    
    if (cols(XY)>2) return(mean(XY[,(1,2)])) // pc
    g = geo_gtype(3, ., geo_pid(., XY), XY, cnt=.)
    if (cnt[4]) { // centroid of polygons
        if (sum(cnt)==cnt[4]) return(__geo_centroid(1, XY))
        return(__geo_centroid(1, select(XY, g:==3)))
    }
    if (cnt[3]) { // centroid of lines
        if (sum(cnt)==cnt[3]) return(__geo_centroid(0, XY))
        return(__geo_centroid(0, select(XY, g:==2)))
    }
    if (cnt[2]) { // centroid of points
        if (sum(cnt)==cnt[2]) return(mean(XY))
        return(mean(select(XY, g:==1)))
    }
    return(J(1,2,.)) // all empty
}

real rowvector __geo_centroid(real scalar poly, real matrix XY)
{
    real scalar    A
    real colvector a
    real matrix    XY2
    
    if (rows(XY)<2) return(editmissing(XY, .))
    XY2 = XY[|2,1\.,.|] \ (.,.)
    // polygon
    if (poly) {
        a = rowsum((1,-1) :* XY :* XY2[,(2,1)])
        A = sum(a)
        if (A & A<.) return(colsum(a :* (XY + XY2)) / (3 * A))
    }
    // line
    a = sqrt(rowsum((XY2 - XY):^2))
    A = sum(a)
    if (A & A<.) return(colsum(a :* (XY + XY2)) / (2 * A))
    // point
    return(mean(XY))
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
*! version 1.0.3  19jun2024  Ben Jann
*!
*! Clips or selects shape items by a convex mask; a modified Sutherland–Hodgman
*! algorithm is used for clipping (returning divided polygons if appropriate);
*! see https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm
*!
*! geo_pid() is used to identify shape items; polygons and lines are assumed
*! to start with missing
*!
*! Syntax:
*!
*!      result = geo_clip(XY, mask [, method])
*!
*!  result   coordinates of clipped/selected shapes
*!  XY       n x 2 real matrix containing the (X,Y) coordinates of the shape
*!           to be clipped or selected; can also be a n x 4 matrix with each
*!           row containing a separate paired-coordinates item (X1, Y1, X2, Y2)
*!  mask     n x 2 real matrix containing the (X,Y) coordinates of the convex
*!           clipping mask; invalid results will be returned if the clipping
*!           mask is not convex
*!  method   real scalar setting the method:
*!           0 = regular clipping
*!           1 = enforce line clipping (i.e., do not close the clipped polygons)
*!           2 = apply selection rather than clipping; shape items with at least
*!               one point inside the mask will be retained, all other items
*!               will be removed
*!           3 = like 2, but an item will only be retained if all points of the
*!               item are inside the mask
*!           4 = like 2, but all items will be retained if at least one item has
*!               a point inside the mask; else all items will be removed
*!           5 = like 3, but all items will be removed if at least one item has
*!               a point outside the mask, else all items will be retained
*!           default is 0; values other than the ones above are treated as 0
*!
*! In case of a rectangular mask, you can also use the following function:
*!
*!      result = geo_rclip(XY, limits [, method])
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
local IntM      real matrix
local RS        real scalar
local RC        real colvector
local RR        real rowvector
local RM        real matrix
local SS        string scalar
local PS        pointer scalar
local PC        pointer colvector

mata:
mata set matastrict on

`RM' geo_clip(`RM' XY, `RM' mask, | `Int' method)
{
    return(_geo_clip_or_select(XY, _geo_clip_mask(mask), method))
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
    if (geo_orientation(mask)==-1) {
        mask = mask[1::rows(mask),] // make counterclockwise
    }
    return(mask)
}

`RM' geo_rclip(`RM' XY, `RM' limits, | `Int' method)
{
    return(_geo_clip_or_select(XY, _geo_rclip_limits(limits), method))
}

`RC' _geo_rclip_limits(`RM' limits)
{
    `RC' mask
 
    mask = vec(limits)
    if (length(mask)<4) mask = mask \ J(4-length(mask),1,.)
    return(mask)
}

`RM' _geo_clip_or_select(`RM' XY, `RM' mask, `Int' method, | `Bool' map)
{   // map will be replaced by an r x 4 matrix of input and output indices; each
    // row contains (a0,b0,a1,b2) where (a0,b0) are the start and end indices of
    // the original item in XY and (a1,b1) are the start and end indices of the
    // transformed item (or set of items) in the output; a1>b1 for dropped items
    if (args()<4) map = 0
    if (!(anyof((2,4), cols(XY)))) {
        errprintf("{it:XY} must have two or four columns\n")
        exit(3200)
    }
    if (anyof((2,3,4,5), method)) return(_geo_select(XY, mask, method, map))
    return(_geo_clip(XY, mask, method==1, map))
}

`RM' _geo_select(`RM' XY, `RM' mask, `Int' method, `Bool' map)
{
    `Int'   i, n, a, b
    `IntC'  p
    `BoolC' I
    
    // identify shape items
    n = rows(XY)
    p = selectindex(_mm_unique_tag(geo_pid(., XY)))
    i = rows(p)
    // handle all items together if point data
    if (i==n) {
        if (cols(mask)==1) I = __geo_rclip_point(XY, mask)
        else               I =  __geo_clip_point(XY, mask)
    }
    // else loop over items
    else {
        I = J(n, 1, .)
        a = n + 1
        for (;i;i--) {
            b = a - 1; a = p[i]
            I[|a\b|] = __geo_select(XY, a, b, mask, method)
        }
    }
    // select items
    if (method==4 | method==5) {
        if (method==4 ? any(I) : all(I)) { // at least one inside / all inside
            if (map) map = J(1,2,(p, (p:-1\n)[|2\.|]))
            return(XY)
        }
        else { // else drop all
            if (map) map = p, (p:-1\n)[|2\.|], J(rows(p),2,.)
            return(J(0, cols(XY), .))
        }
    }
    if (map) map = _geo_select_map(p, I)
    return(select(XY, I))
}

`IntM' _geo_select_map(`IntC' p, `BoolC' I)
{
    `Int'  i, a, b, r
    `IntM' map
    
    i = rows(p)
    map = J(i, 4, .)
    a = rows(I) + 1
    r = 0
    for (;i;i--) {
        map[i,2] = b = a - 1
        map[i,1] = a = p[i]
        map[i,4] = r + 1
        r = r + sum(I[|a\b|])
        map[i,3] = r
    }
    map[,(3,4)] = (r+1) :- map[,(3,4)]
    return(map)
}

`BoolC' __geo_select(`RM' XY, `Int' a, `Int' b, `RM' mask, `Int' method)
{   // first row in XY assumed missing
    `BoolC' I

    if (cols(mask)==1) I = __geo_rclip_point(XY[|a+1,1\b,2|], mask)
    else               I =  __geo_clip_point(XY[|a+1,1\b,2|], mask)
    if (method==2) {
        if (any(I)) return(J(b-a+1,1,1)) // at least one point inside
        else        return(J(b-a+1,1,0)) // else drop all
    }
    if (method==3) {
        if (all(I)) return(J(b-a+1,1,1)) // all points inside
        else        return(J(b-a+1,1,0)) // else drop all
    }
    if (any(I)) return(1 \ I)
    else        return(0 \ I)
}

`RM' _geo_clip(`RM' XY, `RM' mask, `Bool' line, `Bool' map)
{
    `Int'   i, a, b, r
    `IntC'  p
    `IntM'  M
    `RM'    xy
    `PC'    I
    
    // identify shape items
    p = selectindex(_mm_unique_tag(geo_pid(., XY)))
    i = rows(p)
    // handle all items together if point data
    if (i==rows(XY)) return(_geo_clip_point(XY, mask, map))
    // else loop over items ...
    r = 0
    I = J(i, 1, NULL)
    M = J(i, 4, .)
    a = rows(XY) + 1
    for (;i;i--) {
        M[i,2] = b = a - 1; M[i,1] = a = p[i]
        I[i] = &__geo_clip(XY, a, b, mask, line)
        M[i,4] = r + 1
        r = r + rows(*I[i])
        M[i,3] = r
    }
    M[,(3,4)] = (r+1) :- M[,(3,4)]
    // ... and collect results
    xy = J(r, cols(XY), .)
    for (i=length(I);i;i--) {
        a = M[i,3]; b = M[i,4]
        if (a>b) continue // empty item
        xy[|a,1 \ b,.|] = *I[i]
    }
    if (map) map = M
    return(xy)
}

`RM' __geo_clip(`RM' XY, `Int' a, `Int' b, `RM' mask, `Bool' line)
{   // first row in XY assumed missing; XY assumed to have two columns
    `Bool' flip
    `Int'  r, i, n
    `RM'   xy
    `PS'   f
    
    r = b - a + 1
    // point item
    if (r<=2) {
        xy = _geo_clip_point(XY[|a+1,1 \ b,2|], mask, 0)
        if (rows(xy)) return(XY[a,] \ xy)
        return(J(0,2,.))
    }
    // polygon item
    xy = XY[|a,1 \ b,2|]
    if (xy[2,]==xy[r,]) {
        if (line) {
            // rearrange points so that polygon does not start inside
            if (_geo_clip_larrange(xy, mask)) return(xy) // all points inside
            // enforce line clipping
            f = &_geo_clip_line()
            flip = 0
        }
        else {
            f = &_geo_clip_area()
            flip = geo_orientation(xy)==1        // is counterclockwise
            if (flip) xy = J(1,2,.) \ xy[r::2,.] // flip orientation
        }
    }
    // line item
    else { 
        f = &_geo_clip_line()
        flip = 0
    }
    // rectangular clipping
    if (cols(mask)==1) {
        if (mask[1]<.) { // xmin
            xy = (*f)(xy, mask[1])
        }
        if (mask[3]<.) { // ymin (-90° rotation)
            xy = (*f)((xy[,2], -xy[,1]), mask[3])
            xy = (-xy[,2], xy[,1])
        }
        if (mask[2]<.) { // xmax (-180° rotation)
            xy = -(*f)(-xy, -mask[2])
        }
        if (mask[4]<.) { // ymax (-270° rotation)
            xy = (*f)((-xy[,2], xy[,1]), -mask[4])
            xy = (xy[,2], -xy[,1])
        }
    }
    // convex clipping
    else {
        n = rows(mask)
        for (i=1;i<n;i++) xy = (*f)(xy, mask[|i,1 \ i+1,2|])
    }
    // restore orientation
    if (flip) {
        r = rows(xy)
        if (r) xy = J(1,2,.) \ xy[r::2,.] 
    }
    // return
    return(xy)
}

`RM' _geo_clip_point(`RM' XY, `RM' mask, `Bool' map)
{
    `Int'   n
    `BoolC' I
    `IntC'  b1
    
    if (cols(mask)==1) I = __geo_rclip_point(XY, mask)
    else               I =  __geo_clip_point(XY, mask)
    if (map) {
        n = rows(I)
        b1 = runningsum(I)
        map = J(1,2,1::n), (1 \ b1:+1)[|1\n|], b1
    }
    return(select(XY, I))
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
*! {marker geo_gtype}{bf:geo_gtype()}{asis}
*! version 1.0.0  28dec2023  Ben Jann
*!
*! Determine geometry types
*!
*! Syntax:
*!
*!      result = geo_gtype(rtype, ID, PID, XY [, count])
*!
*!  rtype   results type: if rtype==0 the result is a r x 3 matrix with ID in
*!          1st column, PID in 2nd column, and geometry type in 3rd column
*!          (0 = empty, 1 = Point, 2 = LineString, 3 = Polygon), where r is the
*!          total number of polygons; if rtype==1 the result is an n x 1 
*!          colvector containing repeated geometry type values; if rtype==2 the
*!          result is a 4 x 1 colvector of geometry type counts (number of
*!          empty items, points, lines, polygons); if rtype==3, result is the
*!          same as in rtype==1, but optional argument count will be filled in
*!          with type counts
*!  ID      n x 1 real colvector of unit IDs (or, possibly, scalar if single
*!          unit); rows are assumed to be grouped by unit ID
*!  PID     n x 1 real colvector of within-unit geometry-item IDs
*!  XY      n x 2 or n x 4 real matrix of (X,Y) coordinates of the geometry
*!          items; if XY is n x 4 (paired coordinates), only the first two
*!          columns will be considered

mata:
mata set matastrict on

real matrix geo_gtype(real scalar rtype, real colvector ID,
    real colvector PID, real matrix XY, | transmorphic cnt)
{
    real scalar    i, n, a, b, r
    real colvector p, GT
    
    // check conformability
    if (!(anyof((2,4), cols(XY)))) {
        errprintf("{it:XY} must have two or four columns\n")
        exit(3200)
    }
    n = rows(XY)
    i = rows(ID)
    if (n!=i & i!=1)  exit(error(3200))
    if (n!=rows(PID)) exit(error(3200))
    if (n==0) { // no data
        if (rtype==0) return(J(0,3,.))
        if (rtype==1) return(J(0,1,.))
        if (rtype==2) return(J(4,1,0))
        if (rtype==3) {; cnt = J(4,1,0); return(J(0,1,.)); }
        _geo_rtype_err(0..3)
    }
    // determine geometry types
    if (i==1) p = selectindex(_mm_unique_tag(PID))
    else      p = selectindex(_mm_uniqrows_tag((ID,PID)))
    i = rows(p)
    a = n + 1
    if (rtype==0) {
        GT = J(i,1,.)
        for (;i;i--) {
            b = a - 1; a = p[i]
            GT[i] = _geo_gtype(XY, a, b)
        }
        if (rows(ID)==1) return(J(rows(GT),1,ID), PID[p], GT)
        return(ID[p], PID[p], GT)
    }
    if (rtype==1) {
        GT = J(n,1,.)
        for (;i;i--) {
            b = a - 1; a = p[i]
            GT[|a \ b|] = J(b - a + 1, 1, _geo_gtype(XY, a, b))
        }
        return(GT)
    }
    if (rtype==2) {
        GT = J(4,1,0)
        for (;i;i--) {
            b = a - 1; a = p[i]
            r = _geo_gtype(XY, a, b) + 1
            GT[r] = GT[r] + 1
        }
        return(GT)
    }
    if (rtype==3) {
        cnt = J(4,1,0)
        GT = J(n,1,.)
        for (;i;i--) {
            b = a - 1; a = p[i]
            r = _geo_gtype(XY, a, b) + 1
            cnt[r] = cnt[r] + 1
            GT[|a \ b|] = J(b - a + 1, 1, r-1)
        }
        return(GT)
    }
    _geo_rtype_err(0..3)
}

real scalar _geo_gtype(real matrix XY, real scalar a, real scalar b)
{
    real scalar r
    
    r = b - a + 1
    if (r==1) {
        if (missing(XY[a,(1,2)])==2) return(0)      // empty
        return(1)                                   // point
    }
    if (r==2) return(1)                             // point
    if (r>=5) {
        if (XY[a+1,(1,2)]==XY[b,(1,2)]) return(3)   // polygon
        return(2)                                   // line
    }
    return(2)                                       // line
}

void _geo_rtype_err(real rowvector v)
{
    errprintf("{it:rtype} must be in {%s}\n", invtokens(strofreal(v),","))
    exit(3300)
}

end

*! {smcl}
*! {marker geo_hull}{bf:geo_hull()}{asis}
*! version 1.0.2  01jun2024  Ben Jann
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
*!  XY      n x 2 real matrix containing the points; rows containing missing
*!          will be ignored
*!

mata
mata set matastrict on

real matrix geo_hull(real matrix XY)
{
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (!hasmissing(XY)) return(_geo_hull(XY))
    return(_geo_hull(select(XY, !rowmissing(XY))))
}

real matrix _geo_hull(real matrix XY)
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
{   // get index of point with lowest Y (and lowest X within ties)
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
*! {marker geo_json}{bf:geo_json_import() and geo_json2xy()}{asis}
*! version 1.0.1  19dec2023  Ben Jann
*!
*! geo_json_import() imports a GeoJSON FeatureCollection from a source file
*! (see https://en.wikipedia.org/wiki/GeoJSON and https://geojson.org/). The
*! collection will be returned as a string matrix with properties (variables)
*! in columns and features (units) in rows.
*!
*!      S = geo_json_import(filename [, rc])
*!
*!  S         string matrix containing the parsed GeoJSON FeatureCollection;
*!            for each feature (unit), column one will contain the GeoJSON
*!            geometry specification; the remaining columns will contain the
*!            values of the properties; the first row in S will contain the
*!            property names; the second row indicates the data types of the 
*!            properties ("string" or "numeric"); data type is "string" if the
*!            property contains a string value for at least one features, else
*!            the data type is "numeric"; the data type of the first column 
*!            (geometry) is always "string"
*!  filename  string scalar providing the (path and) name of the source file
*!  rc        real scalar that will be set to 1 if an issue occurred (invalid
*!            or incomplete GeoJSON specification); else 0
*!
*! geo_json2xy() extracts the coordinates form a GeoJSON geometry object (see
*! https://en.wikipedia.org/wiki/GeoJSON). Supported geometry types are
*! Point, MultiPoint, LineString, MultiLineString, Polygon, MultiPolygon, and
*! GeometryCollection (case sensitive). In the returned matrix of coordinates,
*! each extracted line or polygon and, in case of a GeometryCollection, each
*! point will start with a row of missing values. Missing rows will be omitted
*! by default when extracting coordinates from a Point or MultiPoint object.
*!
*!      XY = geo_json2xy(s [, mis, gtype, rc])
*!
*!  XY      n x 2 real matrix containing the extracted coordinates; J(1,2,.) is
*!          returned if no coordinates are found
*!  s       string scalar containing the GeoJSON specification
*!  mis     mis!=0 enforces separating points by missing in any case
*!  gtype   string scalar that will be set to the geometry type of s
*!  rc      real scalar that will be set to 1 if an issue occurred (invalid
*!          or incomplete GeoJSON specification); else 0
*!

local Bool  real scalar
local BoolC real colvector
local Int   real scalar
local SS    string scalar
local SC    string colvector
local SM    string matrix
local RR    real rowvector
local RM    real matrix
local PC    pointer colvector
local T     transmorphic

mata:
mata set matastrict on

`SM' geo_json_import(`SS' fn, | `Bool' rc)
{
    `SM' SUB
    `T'  t, s
    
    // parser definition
    rc = 0
    t = _geo_json_parser()
    SUB = (`"\""',"<!DQ!>",`"""')', ("\\","<!BS!>","\")'
    // import file
    s = _geo_json_fget(fn, SUB)
    // parse FeatureCollection
    tokenset(t, s)
    s = _geo_json_unbind(tokenget(t), "{}", rc)
    tokenset(t, "")
    s = _geo_json_object(t, s, rc)
    if (asarray(s, "type")!=`""FeatureCollection""') {
        errprintf("file does not seem to contain a GeoJSON FeatureCollection\n")
        exit(499)
    }
    // collect features and return
    s = _geo_json_unbind(asarray(s, "features"), "[]", rc)
    return(_geo_json_features(t, s, SUB, rc))
}

`SS' _geo_json_fget(`SS' fn, `SM' SUB)
{
    `Int' j, fh
    `SS'  s

    fh = fopen(fn, "r")
    s = fread(fh, 1073741824) // max length = 1 GB
    if (fread(fh,1)!=J(0, 0, "")) {
        fclose(fh)
        errprintf("file too large; maximum size is 1 GB\n")
        exit(498)
    }
    fclose(fh)
    for (j=cols(SUB);j;j--) s = subinstr(s, SUB[1,j], SUB[2,j])
    for (j=9;j<=13;j++)     s = subinstr(s, char(j), " ") // white space
    return(s)
}

`SM' _geo_json_features(`T' t0, `SS' s, `SM' SUB, `Bool' rc)
{
    `Int'   i, n, J
    `SM'    S
    `T'     t, V, F
    
    // collect features and store in matrix S
    t = _geo_json_array_set(t0, s)
    n = _geo_json_array_n(t, rc)
    if (!n) return(J(0,0,"")) // empty array
    V = asarray_create() // for positions of properties in S
    S = J(n,50,"")
    J = 1 /*2*/
    for (i=1;i<=n;i++) {
        // parse feature object
        F = _geo_json_object(t0,
            _geo_json_unbind(_geo_json_array_get(t), "{}", rc), rc)
        if (asarray(F, "type")!=`""Feature""') {
            rc = 1
            continue
        }
        // collect id and geometry
        S[i,1] = asarray(F, "geometry")
        /*S[i,2] = asarray(F, "id")*/
        // collect properties
        _geo_json_features_properties(t0, i, F, V, S, J, rc)
    }
    S = S[,1..J]
    // add names and storage types and cleanup strings
    S = _geo_json_features_S(S, V)
    // drop id column if empty
    /*if (allof(S[|3,2 \ .,2|],"")) S = (J<=2 ? S[,1] : (S[,1], S[,3..J]))*/
    // revert substitutions
    for (i=cols(SUB);i;i--) S = subinstr(S, SUB[2,i], SUB[3,i])
    // return
    return(S)
}

void _geo_json_features_properties(`T' t0, `Int' i, `T' F, `T' V, `SM' S,
    `Int' J, `Bool' rc)
{
    `Int'   k, K, j
    `SS'    key
    `SC'    keys
    `T'     A
    
    A = asarray(F, "properties")
    if (A=="") return // "properties": null
    A = _geo_json_object(t0, _geo_json_unbind(A, "{}", rc), rc)
    keys = asarray_keys(A)
    K = rows(keys)
    for (k=1;k<=K;k++) {
        key = keys[k]
        j = asarray(V, key)
        if (!length(j)) {
            J++
            if (J>cols(S)) S = S, J(rows(S),50,"")
            asarray(V, key, J)
            j = J
        }
        S[i,j] = asarray(A, key)
    }
}

`SM' _geo_json_features_S(`SM' S, `t' V)
{
    `Int'   j
    `BoolC' q
    `SS'    key
    `SC'    keys, Sj
    `SM'    L
    
    L = J(2,cols(S),"")
    L[1,1] = "_GEOMETRY"
    L[2,1] = "string"
    L[1,2] = "id"
    keys = asarray_keys(V)
    for (j=rows(keys);j;j--) {
        key = keys[j]
        L[1,asarray(V, key)] = key
    }
    for (j=cols(S);j>1;j--) {
        Sj = S[,j]
        q = (substr(Sj,1,1)+substr(Sj,-1.1)):==`""""'
        if (any(q)) {
            L[2,j] = "string"
            S[,j]  = substr(Sj, 2, strlen(Sj)-(2:*q))
        }
        else L[2,j] = "numeric"
    }
    return(L \ S)
}

`RM' geo_json2xy(`SS' S, | `Bool' mis, `SS' kw, `Bool' rc)
{
    `RM' XY
    `T'  t0, s
    
    if (args()<2) mis = 0
    rc = 0
    t0 = _geo_json_parser()
    s = _geo_json_unbind(strtrim(S), "{}", rc)
    s = _geo_json_object(t0, s, rc)
    kw = _geo_json_unbind(asarray(s, "type"), `""""')
    if (kw=="GeometryCollection") {
        s = _geo_json_unbind(asarray(s, "geometries"), "[]", rc)
        XY = _geo_json2xy_C(t0, rc, s)
    }
    else {
        s = _geo_json_unbind(asarray(s, "coordinates"), "[]", rc)
        if      (kw=="Point")           XY = _geo_json2xy_p(rc, s, mis!=0)
        else if (kw=="MultiPoint")      XY = _geo_json2xy_P(t0, rc, s, mis!=0)
        else if (kw=="LineString")      XY = _geo_json2xy_l(t0, rc, s)
        else if (kw=="MultiLineString") XY = _geo_json2xy_L(t0, rc, s)
        else if (kw=="Polygon")         XY = _geo_json2xy_s(t0, rc, s)
        else if (kw=="MultiPolygon")    XY = _geo_json2xy_S(t0, rc, s)
        else {
            rc = 1
            kw = ""
            XY = J(0,2,.)
        }
    }
    if (!rows(XY)) XY = (.,.)
    return(XY)
}

// parser for GeometryCollection
`RM' _geo_json2xy_C(`T' t0, `Bool' rc, `SS' S)
{
    `Int' i, n, N, a, b
    `SS'  kw
    `RM'  XY
    `PC'  P
    `T'   t, s
    
    t = _geo_json_array_set(t0, S)
    n = _geo_json_array_n(t, rc)
    if (!n) return(J(0,2,.)) // array is empty
    P = J(n,1,NULL)
    N = 0
    for (i=1;i<=n;i++) {
        s  = _geo_json_unbind(_geo_json_array_get(t), "{}", rc)
        s  = _geo_json_object(t0, s, rc)
        kw = _geo_json_unbind(asarray(s, "type"), `""""')
        s  = _geo_json_unbind(asarray(s, "coordinates"), "[]", rc)
        if      (kw=="Point")           P[i] = &_geo_json2xy_p(rc, s, 1)
        else if (kw=="MultiPoint")      P[i] = &_geo_json2xy_P(t0, rc, s, 1)
        else if (kw=="LineString")      P[i] = &_geo_json2xy_l(t0, rc, s)
        else if (kw=="MultiLineString") P[i] = &_geo_json2xy_L(t0, rc, s)
        else if (kw=="Polygon")         P[i] = &_geo_json2xy_s(t0, rc, s)
        else if (kw=="MultiPolygon")    P[i] = &_geo_json2xy_S(t0, rc, s)
        else {
            rc = 1
            P[i] = &(J(0,2,.))
        }
        N = N + rows(*P[i])
    }
    XY = J(N,2,.)
    b = 0
    for (i=1;i<=n;i++) {
        a = b + 1
        b = b + rows(*P[i])
        if (b<a) continue // empty element
        XY[|a,1 \ b,2|] = *P[i]
    }
    return(XY)
}

// parser for point
`RM' _geo_json2xy_p(`Bool' rc, `SS' s, `Bool' mis)
{
    return(J(mis,2,.) \ __geo_json2xy_item(rc, s))
}

// parser for points
`RM' _geo_json2xy_P(`T' t, `Bool' rc, `SS' s, `Bool' mis)
{
    return(_geo_json2xy_array(t, rc, s, mis ? 2 : 1))
}

// parser for line
`RM' _geo_json2xy_l(`T' t, `Bool' rc, `SS' s)
{
    return(_geo_json2xy_array(t, rc, s, 0))
}

// parser for lines
`RM' _geo_json2xy_L(`T' t, `Bool' rc, `SS' s)
{
    return(_geo_json2xy_arrays(t, rc, s, 0))
}

// parser for polygon
`RM' _geo_json2xy_s(`T' t, `Bool' rc, `SS' s)
{
    return(_geo_json2xy_arrays(t, rc, s, 0))
}

// parser for polygons
`RM' _geo_json2xy_S(`T' t, `Bool' rc, `SS' s)
{
    return(_geo_json2xy_arrays(t, rc, s, 1))
}

// collect coordinates from (nested) arrays
`RM' _geo_json2xy_arrays(`T' t0, `Bool' rc, `SS' S, `Int' levl)
{
    `Int' i, n, N, a, b
    `SS'  s
    `RM'  XY
    `PC'  P
    `T'   t
    
    t = _geo_json_array_set(t0, S)
    n = _geo_json_array_n(t, rc)
    if (!n) return(J(0,2,.)) // array is empty
    P = J(n,1,NULL)
    N = 0
    for (i=1;i<=n;i++) {
        s = _geo_json_unbind(_geo_json_array_get(t), "[]", rc)
        if (levl) P[i] = &_geo_json2xy_arrays(t0, rc, s, levl-1)
        else      P[i] = &_geo_json2xy_array(t0, rc, s, 0)
        N = N + rows(*P[i])
    }
    XY = J(N,2,.)
    b = 0
    for (i=1;i<=n;i++) {
        a = b + 1
        b = b + rows(*P[i])
        if (b<a) continue // empty element
        XY[|a,1 \ b,2|] = *P[i]
    }
    return(XY)
}

// collect coordinates from single array
// pt: 0 line/polygon; 1 points w/o missings, 2 points w missings
`RM' _geo_json2xy_array(`T' t0, `Bool' rc, `SS' s, `Int' pt)
{
    `Int' i, n
    `RM'  XY
    `T'   t
    
    t = _geo_json_array_set(t0, s)
    n = _geo_json_array_n(t, rc)
    if (!n) return(J(0,2,.)) // array is empty
    XY = J(n, 2, .)
    for (i=1;i<=n;i++) {
        XY[i,] = __geo_json2xy_item(rc,
            _geo_json_unbind(_geo_json_array_get(t), "[]", rc))
    }
    if      (!pt)   XY = (.,.) \ XY // add leading missing
    else if (pt==2) XY = _geo_json2xy_array_addmis(XY)
    return(XY)
}

`RM' _geo_json2xy_array_addmis(`RM' xy)
{
    `Int' n
    `RM'  XY
    
    n = rows(xy)
    XY = J(2*n,cols(xy),.)
    XY[(1::n)*2,] = xy
    return(XY)
}

// collect coordinates from single item
`RR' __geo_json2xy_item(`Bool' rc, `SS' s)
{
    `Int' j
    `RR'  xy
    
    xy = strtoreal(tokens(subinstr(s,","," ")))
    j = length(xy)
    if (j!=2) {
        rc = 1
        if (j>2)       xy = xy[(1,2)] // too many elements
        else if (j==1) xy = (xy,.)    // too few element
        else           xy = (.,.)     // too few element
    }
    return(xy)
}

// JSON parser definition
`T' _geo_json_parser()
{
    return(tokeninit(" ", (",", ":"), (`""""', "{}", "[]")))
}

// return JSON object as associative array; assumes that {} binding has already
// been removed
`T' _geo_json_object(`T' t0, `SS' S, | `Bool' rc)
{
    `SS' key, s
    `T'  t, A

    // setup associative array
    A = asarray_create()
    asarray_notfound(A, "")
    // collect members: "key" : value [, "key" : value ...]
    t = t0
    tokenset(t, S)
    s = tokenget(t) // get first key
    if (s=="") return(A) // object is empty
    while (1) {
        if (s==",") { // element is empty; skip comma and continue
            rc = 1
            s = tokenget(t)
            if (s=="") break // reached end
            continue
        }
        if (s==":") { // key is missing; skip next token and comma and continue
            rc = 1
            s = tokenget(t)
            if (s!=",") s = tokenget(t)
            s = tokenget(t)
            if (s=="") break // reached end
            continue
        }
        key = _geo_json_unbind(s, `""""', rc)
        s = tokenget(t) // get colon
        if (s==":") s = tokenget(t) // get value
        else        rc = 1 // colon is missing
        if (s==",") { // value is missing; skip comma and continue
            rc = 1
            s = tokenget(t)
            if (s=="") break // reached end
            continue
        }
        if (s=="") { // value is missing and reached end
            rc = 1
            break
        }
        if      (s=="true")  s = "1"
        else if (s=="false") s = "0"
        else if (s=="null")  s = ""
        asarray(A, key, s) // post key and value
        s = tokenget(t) // get comma
        if (s=="") break // reached end
        if (s!=",") { // comma is missing; treat as next key
            rc = 1
            continue
        }
        s = tokenget(t) // get next key
        if (s=="") { // reached end; last element is empty
            rc = 1
            break
        }
    }
    return(A)
}

// parse JSON array; assumes that [] binding has already been removed
// - setup parser
`T' _geo_json_array_set(`T' t0, `SS' s)
{
    `T' t
    
    t = t0
    tokenset(t, s)
    return(t)
}
// - count number of (remaining) elements
`Int' _geo_json_array_n(`T' t, | `Bool' rc)
{
    `SS'  s
    `Int' n, p
    
    p = tokenoffset(t)
    s = tokenget(t)
    if (s=="") return(0) // array is empty
    n = 0
    while (1) {
        n++
        if (s==",") { // element is empty
            rc = 1
            s = tokenget(t)
            continue
        }
        s = tokenget(t) // skip comma
        if (s=="") break // reached end
        if (s!=",") { // comma is missing
            rc = 1
            continue
        }
        s = tokenget(t)
        if (s=="") { // last element empty
            rc = 1
            n++
            break
        }
    }
    tokenoffset(t, p) // move back to start
    return(n)
}
// - get next element
`SS' _geo_json_array_get(`T' t)
{
    `SS'  s
    
    
    s = tokenget(t)
    if (s==",") return("") // element is empty
    if (s=="")  return("") // element is empty
    if (tokenpeek(t)==",") (void) tokenget(t) // skip comma after element
    return(s)
}

// remove binding characters; type
//    s = _geo_json_unbind(s, `""""')   for quotes
//    s = _geo_json_unbind(s, "{}")     for braces
//    s = _geo_json_unbind(s, "[]")     for brackets
`SS' _geo_json_unbind(`SS' s, `SS' lr, | `Bool' rc)
{
    if ((substr(s,1,1)+substr(s,-1,1))!=lr) {
        rc = 1
        return(s)
    }
    return(substr(s,2,strlen(s)-2))
}

end

*! {smcl}
*! {marker geo_orientation}{bf:geo_orientation()}{asis}
*! version 1.0.2  01jun2024  Ben Jann
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
{
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (!hasmissing(XY)) return(_geo_orientation(XY))
    return(_geo_orientation(select(XY, !rowmissing(XY))))
}

real scalar _geo_orientation(real matrix XY)
{
    real scalar    n, i, a
    real rowvector c
    real colvector A, B, C
    
    n = rows(XY)
    if (n==0) return(0) // no data; orientation undefined
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
*! version 1.0.2  25dec2023  Ben Jann
*!
*! Generate geometry-item ID (polygon ID) within unit ID
*!
*! Syntax:
*!
*!      PID = geo_pid(ID, XY)
*!
*!  PID     n x 1 real colvector of within-unit geometry-item IDs
*!  ID      n x 1 real colvector of unit IDs (or, possibly, scalar if single
*!          unit); rows are assumed to be grouped by unit ID
*!  XY      n x 2 or n x 4 real matrix of (X,Y) coordinates of the geometry
*!          items; if XY is n x 4 (paired coordinates), each row is considered 
*!          a separate item; else, if the first row of a unit is not missing,
*!          each row within the unit is considered a separate item); else each
*!          missing row within the unit starts a new item
*!

mata:
mata set matastrict on

real colvector geo_pid(real colvector ID, real matrix XY)
{
    real scalar    c, i, n, a, b
    real colvector p, id
    
    // check conformability
    c = cols(XY)
    if (c!=2 & c!=4) {
        errprintf("{it:XY} must have two or four columns\n")
        exit(3200)
    }
    n = rows(XY)
    i = rows(ID)
    if (n!=i & i!=1) exit(error(3200))
    // generate item id
    if (n==0) return(J(0,1,.))      // no data
    if (c==4) return(1::n)          // paired-coordinate data
    if (i==1) return(_geo_pid(XY))  // single unit
    p = selectindex(_mm_unique_tag(ID))
    i = rows(p)
    if (i==1) return(_geo_pid(XY))  // single unit
    a = n + 1
    id = J(n,1,.)
    for (;i;i--) {
        b = a - 1; a = p[i]
        id[|a \ b|] = _geo_pid(XY, a, b)
    }
    return(id)
}

real colvector _geo_pid(real matrix XY, | real scalar a, real scalar b)
{
    if (args()==1) {
        if (missing(XY[1,])!=2) return(1::rows(XY))
        return(runningsum(rowmissing(XY):==2))
    }
    if (missing(XY[a,])!=2) return(1::(b-a+1))
    return(runningsum(rowmissing(XY[|a,1 \ b,2|]):==2))
}

end

*! {smcl}
*! {marker geo_plevel}{bf:geo_plevel()}{asis}
*! version 1.0.2  01jun2024  Ben Jann
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
*!          n containing repeated plot level values, else the result is a r x 3
*!          matrix with ID in 1st column, PID in 2nd column, and plot level in
*!          3rd column, where r is the total number of polygons
*!  ID      real colvector of unit IDs
*!  PID     real colvector of within-unit polygon IDs
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons; missing
*!          coordinates will be ignored
*!  nodots  nodots!=0 suppresses progress dots 
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

`RM' geo_plevel(`Int' rtype, `RC' ID, `RC' PID, `RM' XY, | `Bool' nodots)
{
    `Int'    i, j, a, b, i0, n
    `RM'     P
    `pUnits' u
    
    if (args()<5) nodots = 0
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    // collect units and polygons
    u = _geo_collect_units(ID, PID, XY)
    n = length(u)
    // determine within unit plot levels
    if (!nodots) i0 = _geo_progress_init("(pass 1/2: ")
    for (i=n;i;i--) {
        _geo_plevel_within(*u[i])
        if (!nodots) _geo_progress(i0, 1-(i-1)/n)
    }
    if (!nodots) _geo_progress_end(i0, ")")
    // update plot levels based on between unit comparisons
    if (!nodots) i0 = _geo_progress_init("(pass 2/2: ")
    for (i=n;i;i--) {
        for (j=i-1; j; j--) _geo_plevel_between(*u[i], *u[j])
        if (!nodots) _geo_progress(i0, 1-(i-1)/n)
    }
    if (!nodots) _geo_progress_end(i0, ")")
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
{   // treat as not inside as soon as an outside point is found
    // treat as inside as soon as an inside point is found
    // (assuming that there are no crossings)
    // treat as not inside if all points are on border
    `Int' i, r

    for (i=rows(xy);i;i--) {
        r = geo_pointinpolygon(xy[i,1], xy[i,2], XY)
        if (r==0) return(1) // point is outside
        if (r==1) return(0) // point is inside
    }
    return(0) // all points on border
}

`pUnits' _geo_collect_units(`RC' ID, `RC' PID, `RM' XY)
{
    `Int'    i, a, b
    `IntC'   p
    `pUnits' u
    
    p = selectindex(_mm_unique_tag(ID))
    i = rows(p)
    u = J(i,1,NULL)
    a = rows(ID) + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        u[i] = &_geo_collect_unit(a, b, ID, PID, XY)
    }
    return(u)
}

`Unit' _geo_collect_unit(`Int' a, `Int' b, `RC' ID, `RC' PID, `RM' XY)
{
    `Int'  i, i0, i1
    `IntC' p
    `Unit' u
    
    u.id = ID[a]
    p    = (a - 1) :+ selectindex(_mm_unique_tag(PID[|a \ b|]))
    u.n  = i = rows(p)
    u.p  = J(u.n, 1, NULL)
    i0   = b + 1
    for (;i;i--) {
        i1 = i0 - 1; i0 = p[i]
        u.p[i] = &_geo_collect_polygon(i0, i1, PID, XY)
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
*! version 1.1.0  16may2024  Ben Jann
*!
*! Determines whether a point is inside or outside of a given polygon; the
*! implementation is loosely based on the examples at
*! https://rosettacode.org/wiki/Ray-casting_algorithm
*!
*! Syntax:
*!
*!      inside = geo_pointinpolygon(x, y, XY)
*!
*!  inside  real scalar equal to
*!              0 if point is outside
*!              1 if point is inside
*!              2 if point lies on edge
*!              3 is point lies on vertex
*!  x       real scalar containing X coordinate of point
*!  y       real scalar containing Y coordinate of point
*!  XY      n x 2 real matrix containing (X,Y) coordinates of polygon
*!          all arguments assumed non-missing
*!

mata:
mata set matastrict on

real scalar geo_pointinpolygon(real scalar x, real scalar y, real matrix XY)
{
    real scalar  i, c, C, poly
    real scalar  ax, ay, bx, by
    
    /*
    if (cols(XY)!=2) {
         errprintf("{it:XY} must have two columns\n")
         exit(3200)
    }
    */
    // XY empty
    if (!(i = rows(XY))) return(0)
    // XY is point
    if (i==1) {
        if ((x,y)==XY) return(3)
        return(0)
    }
    // XY is polygon or line
    poly = (XY[1,]==XY[i,])
    C = 0
    ax = XY[i,1]; ay = XY[i,2]
    i--
    for (;i;i--) {
        bx = ax; by = ay
        ax = XY[i,1]; ay = XY[i,2]
        if (ay>by) c = _geo_rayintersect(x, y, bx, by, ax, ay)
        else       c = _geo_rayintersect(x, y, ax, ay, bx, by)
        if (c) {
            if (c<2) C++
            else     return(c)  // point lies on vertex or on edge
        }
    }
    if (poly) return(mod(C,2))
    return(0) // XY is line; position inside not possible
}

real scalar _geo_rayintersect(real scalar px, real scalar py,
    real scalar ax, real scalar ay, real scalar bx, real scalar by)
{
    real scalar a_b, a_p
    
    // A: outside of bounding box of segment
    if (py>by) return(0)          // above
    if (py<ay) return(0)          // below
    if (ax>bx) {
        if (px > ax) return(0)    // right
        if (px < bx) {            // left
            if (ay==by) return(0) // do not count if segment is flat
            if (ay==py) return(0) // do not count if at bottom
            return(1)
        }
    }
    else {
        if (px > bx) return(0)    // right
        if (px < ax) {            // left
            if (ay==by) return(0) // do not count if segment is flat
            if (ay==py) return(0) // do not count if at bottom
            return(1)
        }
    }
    // B: on edge of bounding box
    if (py==ay) {
        if (px==ax) return(3)     // p equal to a
        if (ay==by) {             // segment is flat
            if (px==bx) return(3) // p equal to b
            return(2)             // p lies on segment
        }
        return(0)                 // at bottom; do not count
    }
    if (py==by) {
        if (px==bx) return(3)     // p equal to b
        return(bx>ax)             // at top; count if b is right of a
    }
    if (px==ax) {
        if (ax==bx) return(2)     // segment is upright; p lies on segment
        return(bx>ax)             // count if b is right of a
    }
    if (px==bx) {
        return(bx<ax)             // count if b is left of a
    }
    // C: inside bounding box
    a_b = (by - ay) / (bx - ax)   // slope of a->b
    a_p = (py - ay) / (px - ax)   // slope of a->p
    if (a_p==a_b) return(2)       // p lies on segment
    return(a_p>a_b)               // count if slope of a->p is larger
}

end

*! {smcl}
*! {marker geo_project}{bf:geo_project()}{asis}
*! version 1.0.0  03jun2024  Ben Jann
*!
*! Apply a map projection to given (unprojected) coordinates.
*!
*! Syntax:
*!
*!      result = geo_project(XY, [ pname, rad, opts ])
*!
*!  result  n x 2 real matrix of projected (X,Y) coordinates
*!  XY      n x 2 real matrix of original (X,Y) coordinates
*!  pname   string scalar selecting the projection; abbreviation and uppercase
*!          spelling allowed for built-in projections; default is "webmercator"
*!  rad     rad!=0 specifies that the original coordinates are in radians, not
*!          in degrees; this also affects the interpretation of optional
*!          arguments (if relevant)
*!  opts    real vector containing optional arguments
*!

mata:
mata set matastrict on

real matrix geo_project(real matrix XY, | string scalar s, real scalar rad,
    real vector opts)
{
    pointer (function) scalar f
    
    if (args()<3) rad = 0
    if (cols(XY)!=2) {
         errprintf("{it:XY} must have two columns\n")
         exit(3200)
    }
    (void) _geo_project_find(s, f=NULL)
    return((*f)(XY, rad, opts))
}

string scalar _geo_project_find(string scalar s0, | pointer (function) scalar f)
{
    string scalar    s
    string rowvector list
    
    list = "webmercator", "mercator", "emercator", "miller", "gallstereo",
           "lambert", "behrmann", "hobodyer", "gallpeters", "peters",
           "equirectangular",
           "robinson", "equalearth", "naturalearth",
           "winkeltripel", "hammer",
           "conic", "albers", "lambertconic",
           "orthographic"
    (void) _mm_strexpand(s=s0, strlower(strtrim(s0)), list, "webmercator")
    f = findexternal("geo_project_"+s+"()")
    if (f==NULL) {
        errprintf("projection {bf:%s} not found\n", s0)
        exit(3499)
    }
    return(s) // return expanded name
}

real scalar _geo_project_opt(real vector opts, real scalar j, real scalar def)
{   
    return(length(opts)>=j ? opts[j] : def)
}

real matrix _geo_project_rad(real scalar rad, real matrix X)
{
    if (rad) return(X)
    return(X * (pi()/180))
}

real colvector _geo_project_clip(real colvector x, string scalar lbl,
    real scalar limit, | real scalar offset)
{
    real scalar    min, max, clipped
    real rowvector minmax
    real colvector X, p
    
    // setup
    if (args()<4) offset = 0
    if (limit>=.)  return(x) // nothing to do
    if (offset>=.) return(x) // nothing to do
    max = offset + abs(limit) 
    min = offset - abs(limit)
    // clip data
    minmax = minmax(x)
    clipped = 0
    if (minmax[1]<min) {
        p = selectindex(x:<min)
        X = x
        X[p] = J(length(p), 1, min)
        clipped = 1
    }
    if (minmax[2]>max & minmax[2]<.) {
        p = selectindex(x:>max :& x:<.)
        if (clipped) X[p] = J(length(p), 1, max)
        else {
            X = x
            X[p] = J(length(p), 1, max)
            clipped = 1
        }
    }
    if (clipped) {
        printf("{txt}(using clipped values for %s coordinates" +/*
            */ " outside [%g,%g])\n", lbl, min, max)
        return(X)
    }
    return(x)
}

real matrix geo_project_webmercator(real matrix XY, real scalar rad,
     | real vector opts)
{   // Web Mercator projection based on
    // https://en.wikipedia.org/wiki/Web_Mercator_projection
    // Y clipped at +/- 85.051129
    real scalar    r, x0, yc
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 360/pi() * atan(exp(pi())) - 90) // Y limit
    r  = 256 * 2^_geo_project_opt(opts, 1, 0) / (2*pi()) // zoom
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    return(r * (
        x :+ pi(),
        ln(tan(pi()/4 :+ y/2)) :- pi()
        ))
}

real matrix geo_project_mercator(real matrix XY, real scalar rad,
     | real vector opts)
{   // Mercator projection (spherical earth model) based on
    // https://en.wikipedia.org/wiki/Mercator_projection
    real scalar    r, x0, yc
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    return(r * ( 
        x,
        sign(y) :* ln(tan(pi()/4 :+ abs(y)/2))
        )) // using abs(y) and multiply by sign to prevent missing at -90
}

real matrix geo_project_emercator(real matrix XY, real scalar rad,
     | real vector opts)
{   // Mercator projection (ellipsoid earth model) based on
    // https://en.wikipedia.org/wiki/Mercator_projection
    // inverse flattening parameter according to WGS 84 from
    // https://en.wikipedia.org/wiki/World_Geodetic_System
    // note:
    // - the squared eccentricity is defined as e2 = 1 - (b/a)^2, where a is
    //   the semi-major axis (a = 6378137) and b is the semi-minor axis of the
    //   ellipsoid
    // - the flattening (ellipticity) is defined as f = (a - b) / a, the
    //   inverse flattening as m = 1/f
    // - this implies: e2 = 2*f - f^2 = 2/m - (1/m)^2
    //                 b = a - a*f = a - a/m
    real scalar    r, x0, yc, m, e
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    m  = _geo_project_opt(opts, 3, 298.257223563) // inverse flattening
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    e  = sqrt(2/m - (1/m)^2)
    return(r * ( 
        x,
        sign(y) :* ln(tan(pi()/4 :+ abs(y)/2) :* ((1 :- e * sin(abs(y)))
            :/  (1 :+ e * sin(abs(y)))):^(e/2))
        )) // using abs(y) and multiply by sign to prevent missing at -90
}

real matrix geo_project_miller(real matrix XY, real scalar rad,
     | real vector opts)
{   // Miller cylindrical projection using the formula given at
    // https://en.wikipedia.org/wiki/Miller_cylindrical_projection
    real scalar    r, x0, yc
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    return(r * (
        x,
        (5/4) * ln(tan(pi()/4 :+ (2/5)*y))
        ))
}

real matrix geo_project_gallstereo(real matrix XY, real scalar rad,
     | real vector opts)
{   // Gall stereographic projection using the formula given at
    // https://en.wikipedia.org/wiki/Gall_stereographic_projection
    real scalar    r, x0, yc
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    return(r * (
        x / sqrt(2),
        (1 + sqrt(2)/2) * tan(y/2)
        ))
}

real matrix geo_project_lambert(real matrix XY, real scalar rad,
     | real vector opts)
{   // Lambert cylindrical equal-area projection using the formula given at
    // https://en.wikipedia.org/wiki/Cylindrical_equal-area_projection
    real scalar r, x0, s
    
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    s  = _geo_project_opt(opts, 3, 1) // stretch factor
    return(_geo_project_lambert(XY, rad, r, x0, s))
}

real matrix _geo_project_lambert(real matrix XY, real scalar rad,
     real scalar r, real scalar x0, real scalar s)
{
    real scalar    yc
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90) // Y limit
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    return(r * (sqrt(s) * x, sin(y) / sqrt(s)))
}

real matrix geo_project_behrmann(real matrix XY, real scalar rad,
     | real vector opts)
{   // Behrmann projection using the formula given at
    // https://en.wikipedia.org/wiki/Cylindrical_equal-area_projection
    real scalar r, x0
    
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    return(_geo_project_lambert(XY, rad, r, x0, .75))
}

real matrix geo_project_hobodyer(real matrix XY, real scalar rad,
     | real vector opts)
{   // Hobo–Dyer projection using the formula given at
    // https://en.wikipedia.org/wiki/Cylindrical_equal-area_projection
    real scalar r, x0
    
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    return(_geo_project_lambert(XY, rad, r, x0, cos(37.5 * pi() / 180)^2))
}

real matrix geo_project_gallpeters(real matrix XY, real scalar rad,
     | real vector opts)
{   // Gall–Peters projection using the formula given at
    // https://en.wikipedia.org/wiki/Cylindrical_equal-area_projection
    real scalar r, x0
    
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    return(_geo_project_lambert(XY, rad, r, x0, .5))
}

real matrix geo_project_peters(real matrix XY, real scalar rad,
     | real vector opts)
{
    return(geo_project_gallpeters(XY, rad, opts))
}

real matrix geo_project_equirectangular(real matrix XY, real scalar rad,
     | real vector opts)
{   // Equirectangular projection based on
    // https://en.wikipedia.org/wiki/Equirectangular_projection
    real scalar    y1, r, x0, y0
    real colvector x, y
    
    y1 = _geo_project_rad(rad, _geo_project_opt(opts, 1, 0)) // std parallel
    r  = _geo_project_opt(opts, 2, 1) // radius of earth
    x0 = _geo_project_opt(opts, 3, 0) // central meridian
    y0 = _geo_project_opt(opts, 4, 0) // central parallel
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, XY[,2] :- y0)
    return(r * (cos(y1) * x, y))
}
real matrix geo_project_robinson(real matrix XY, real scalar rad,
     | real vector opts)
{   // robinson projection using linear interpolation; the length of the central
    // meridian will be 1.3523/.8487/pi() = .5071880041 times the equator length;
    // values taken from https://en.wikipedia.org/wiki/Robinson_projection; also
    // see: Ipbuker, C. (2005). A Computational Approach to the Robinson 
    // Projection. Survey Review 38(297): 204–217. DOI:10.1179/sre.2005.38.297.204
    real scalar    r, x0, yc, i, j
    real colvector X, Y, x, y, d
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    X  = r *
         (1,.9986,.9954,.99,.9822,.973,.96,.9427,.9216,.8962,.8679,
          .835,.7986,.7597,.7186,.6732,.6213,.5722,.5322,.5322)'
    Y  = r * 1.3523/.8487 *
         (0,.062,.124,.186,.248,.31,.372,.434,.4958,.5571,.6176,
          .6769,.7346,.7903,.8435,.8936,.9394,.9761,1,1)'
    d  = abs(y) * (180/(5*pi())) // 5 degree steps
    i  = trunc(d)
    d  = d - i
    i  = editmissing(i :+ 1, 1) // (set to arbitrary index if y is missing)
    j  = i :+ 1
    return(((X[i] + d :* (X[j]-X[i])) :* x,
            (Y[i] + d :* (Y[j]-Y[i])) :* sign(y)))
}

real matrix geo_project_equalearth(real matrix XY, real scalar rad,
    | real vector opts)
{   // equal earth projection using the formula given at
    // https://en.wikipedia.org/wiki/Equal_Earth_projection
    real scalar    r, x0, yc, A1, A2, A3, A4
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x = _geo_project_rad(rad, XY[,1] :- x0)
    y = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    A1 = 1.340264; A2 = -0.081106; A3 = 0.000893; A4 = 0.003796
    y = asin(sqrt(3)/2 * sin(y))
    return(r * (
        2/3*sqrt(3)*(x :* cos(y) :/ (9*A4*y:^8 + 7*A3*y:^6 + 3*A2*y:^2 :+ A1)),
        A4*y:^9 +   A3*y:^7 +   A2*y:^3  + A1*y
        ))
}

real matrix geo_project_naturalearth(real matrix XY, real scalar rad,
    | real vector opts)
{   // natural earth projection using the formula in source code at
    // http://www.shadedrelief.com/NE_proj/natural_earth_proj.zip
    // (formula at https://en.wikipedia.org/wiki/Natural_Earth_projection
    // 27may2024, appears wrong)
    real scalar    r, x0, yc, A0, A1, A2, A3, A4, B0, B1, B2, B3, B4
    real colvector x, y, p2, p4
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    A0 =  .8707;   A1 = -.131979; A2 = -.013791; A3 = .003971; A4 = -.001529
    B0 = 1.007226; B1 =  .015085; B2 = -.044475; B3 = .028874; B4 = -.005916
    p2 = y:^2; p4 = p2:^2
    return(r * (
        x :* (A0 :+ p2 :* (A1 :+ p2 :* (A2 :+ p4 :* p2 :* (A3 :+ p2 :* A4)))),
        y :* (B0 :+ p2 :* (B1 :+ p4 :* (B2 :+ B3 :* p2 :+ B4 :* p4)))
        ))
}

real matrix geo_project_winkeltripel(real matrix XY, real scalar rad,
    | real vector opts)
{   // Winkel tripel projection using the formula given at
    // https://en.wikipedia.org/wiki/Winkel_tripel_projection
    real scalar    y1, r, x0, yc
    real colvector x, y, a
    
    yc = _geo_project_rad(!rad, 90)     // Y limit
    y1 = _geo_project_rad(!rad, 180/pi()) * acos(2/pi()) // default for y1
    y1 = _geo_project_opt(opts, 1, y1)  // std parallel
    r  = _geo_project_opt(opts, 2, 1)   // radius of earth
    x0 = _geo_project_opt(opts, 3, 0)   // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    y1 = _geo_project_rad(rad, y1)
    a  = acos(cos(y) :* cos(x/2)) // alpha
    a  = editmissing(sin(a) :/ a, 1) // sinc(alpha)
    return((r/2) * (
        x * cos(y1) + 2 * (cos(y) :* sin(x/2) :/ a),
        y + sin(y) :/ a
        ))
}

real matrix geo_project_hammer(real matrix XY, real scalar rad,
    | real vector opts)
{   // Winkel tripel projection using the formula given at
    // https://en.wikipedia.org/wiki/Winkel_tripel_projection
    real scalar    r, x0, yc
    real colvector x, y, d
    
    yc = _geo_project_rad(!rad, 90)   // Y limit
    r  = _geo_project_opt(opts, 1, 1) // radius of earth
    x0 = _geo_project_opt(opts, 2, 0) // central meridian
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    d  = sqrt(1 :+ cos(y) :* cos(x/2))
    return(r * (
        (2 * sqrt(2)) * (cos(y) :* sin(x/2) :/ d),
        sqrt(2) * (sin(y) :/ d)
        ))
}

real matrix geo_project_conic(real matrix XY, real scalar rad,
     | real vector opts)
{   // Equidistant conic projection using the formula given at
    // https://en.wikipedia.org/wiki/Equidistant_conic_projection
    real scalar    y1, y2, r, x0, y0, yc, n, G, p
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)     // Y limit
    y2 = _geo_project_rad(!rad, 60)     // default for y2
    y1 = _geo_project_opt(opts, 1,  0)  // 1st standard parallel
    y2 = _geo_project_opt(opts, 2, y2)  // 2nd standard parallel
    r  = _geo_project_opt(opts, 3,  1)  // radius of earth
    x0 = _geo_project_opt(opts, 4,  0)  // central meridian
    y0 = _geo_project_opt(opts, 5,  0)  // central parallel
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    y1 = _geo_project_rad(rad, y1)
    y2 = _geo_project_rad(rad, y2)
    y0 = _geo_project_rad(rad, y0)
    n  = y1==y2 ? sin(y1) : (cos(y1) - cos(y2)) / (y2 - y1)
    G  = cos(y1)/n + y1
    p  = G - y0 // rho0
    y  = G :- y // rho
    return(r * (y :* sin(n*x), p :- y :* cos(n*x)))
}

real matrix geo_project_albers(real matrix XY, real scalar rad,
     | real vector opts)
{   // Albers equal-area conic projection using the formula given at
    // https://en.wikipedia.org/wiki/Albers_projection
    real scalar    y1, y2, r, x0, y0, yc, n, C, p
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)     // Y limit
    y2 = _geo_project_rad(!rad, 60)     // default for y2
    y1 = _geo_project_opt(opts, 1,  0)  // 1st standard parallel
    y2 = _geo_project_opt(opts, 2, y2)  // 2nd standard parallel
    r  = _geo_project_opt(opts, 3,  1)  // radius of earth
    x0 = _geo_project_opt(opts, 4,  0)  // central meridian
    y0 = _geo_project_opt(opts, 5,  0)  // central parallel
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    y1 = _geo_project_rad(rad, y1)
    y2 = _geo_project_rad(rad, y2)
    y0 = _geo_project_rad(rad, y0)
    n  = (sin(y1) + sin(y2)) / 2
    C  = cos(y1)^2 + 2*n*sin(y1)
    p  = r/n * sqrt(C - 2*n*sin(y0)) // rho0
    y  = r/n * sqrt(C :- 2*n*sin(y)) // rho
    return((y :* sin(n*x), p :- y :* cos(n*x)))
}

real matrix geo_project_lambertconic(real matrix XY, real scalar rad,
    | real vector opts)
{   // Lambert conformal conic projection using the formula given at
    // https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection
    real scalar    y1, y2, r, x0, y0, yc, n, F, p
    real colvector x, y
    
    yc = _geo_project_rad(!rad, 90)     // Y limit
    y2 = _geo_project_rad(!rad, 60)     // default for y2
    y1 = _geo_project_opt(opts, 1,  0)  // 1st standard parallel
    y2 = _geo_project_opt(opts, 2, y2)  // 2nd standard parallel
    r  = _geo_project_opt(opts, 3,  1)  // radius of earth
    x0 = _geo_project_opt(opts, 4,  0)  // central meridian
    y0 = _geo_project_opt(opts, 5,  0)  // central parallel
    x  = _geo_project_rad(rad, XY[,1] :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc))
    y1 = _geo_project_rad(rad, y1)
    y2 = _geo_project_rad(rad, y2)
    y0 = _geo_project_rad(rad, y0)
    n  = y1==y2 ? sin(y1) :
         ln(cos(y1)/cos(y2)) / ln(tan(pi()/4 + y2/2) / tan(pi()/4 + y1/2))
    F  = cos(y1) * tan(pi()/4 + y1/2)^n / n
    p  = r*F * (1 / tan(pi()/4 + y0/2)):^n  // rho0
    y  = r*F * (1 :/ tan(pi()/4 :+ y/2)):^n // rho
    return((y :* sin(n*x), p :- y :* cos(n*x)))
}

real matrix geo_project_orthographic(real matrix XY, real scalar rad,
    | real vector opts)
{   // Orthographic projection using the formula given at
    // https://en.wikipedia.org/wiki/Orthographic_map_projection
    real scalar    r, x0, y0, xc, yc
    real colvector x, y
    
    xc = _geo_project_rad(!rad, 90)     // X limit
    yc = _geo_project_rad(!rad, 90)     // Y limit
    r  = _geo_project_opt(opts, 1, 1)   // radius of earth
    x0 = _geo_project_opt(opts, 2, 0)   // central meridian
    y0 = _geo_project_opt(opts, 3, 0)   // central parallel
    x  = _geo_project_rad(rad, _geo_project_clip(XY[,1], "X", xc, x0) :- x0)
    y  = _geo_project_rad(rad, _geo_project_clip(XY[,2], "Y", yc, y0))
    y0 = _geo_project_rad(rad, y0)
    return(r * (
        cos(y) :* sin(x),
        cos(y0) :* sin(y) - sin(y0)*cos(y):*cos(x)
        ))
}

end

*! {smcl}
*! {marker geo_refine}{bf:geo_refine()}{asis}
*! version 1.0.2  02jun2024  Ben Jann
*!
*! Refines polygons and lines by adding extra points such that maximum distance
*! between neighboring points is smaller than or equal to delta.
*!
*! geo_pid() is used to identify shape items; polygons and lines are assumed
*! to start with missing
*!
*! Syntax:
*!
*!      result = geo_refine(XY, delta)
*!
*!  result  n x 2 real matrix containing refined shapes
*!  XY      n x 2 real matrix containing the (X,Y) coordinates of the shape
*!          items to be refined
*!  delta   real scalar specifying threshold for adding points (minimum
*!          distance)
*!

local Int real scalar
local RS  real scalar
local RR  real rowvector
local RM  real matrix
local PC  pointer colvector

mata:
mata set matastrict on

`RM' geo_refine(`RM' XY, `RS' delta)
{
    `Int' i, n, r, k, a, b
    `RR'  xy0, xy1
    `RM'  xy, res
    `PC'  I
    
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (delta<=0) return(XY) // do not refine if delta=0
    if (delta>=.) return(XY) // do not refine if delta>=.
    if (all(_mm_unique_tag(geo_pid(., XY)))) return(XY) // point data
    // loop over coordinates ...
    n = rows(XY)
    r = 0
    I = J(n, 1, NULL)
    xy0 = (.,.)
    for (i=1;i<=n;i++) {
        xy1 = XY[i,]
        if      (hasmissing(xy1)) xy = xy1
        else if (hasmissing(xy0)) xy = xy1
        else {
            k = ceil(sqrt((xy1[1]-xy0[1])^2 + (xy1[2]-xy0[2])^2) / delta)
            if (k<2) xy = xy1 // distance not smaller than delta
            else xy = xy0 :+ (1::k-1)/k * (xy1[1]-xy0[1], xy1[2]-xy0[2]) \ xy1
        }
        r = r + rows(xy)
        I[i] = &xy[.,.]
        xy0 = xy1
    }
    // ... and collect results
    res = J(r, 2, .)
    b = 0
    for (i=1;i<=n;i++) {
        xy = *I[i]
        a = b + 1
        b = b + rows(xy)
        res[|a,1 \ b,.|] = xy
    }
    return(res)
}

end

*! {smcl}
*! {marker geo_rotate}{bf:geo_rotate()}{asis}
*! version 1.0.1  01jun2024  Ben Jann
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
    
    if (cols(XY)!=2) {
         errprintf("{it:XY} must have two columns\n")
         exit(3200)
    }
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
*! {marker geo_simplify}{bf:geo_simplify()}{asis}
*! version 1.0.1  06jun2024  Ben Jann
*!
*! Simplify polygons and lines using Visvalingam–Whyatt algorithm
*! see: https://en.wikipedia.org/wiki/Visvalingam%E2%80%93Whyatt_algorithm
*!
*! geo_pid() is used to identify shape items; polygons and lines are assumed
*! to start with missing
*!
*! Syntax:
*!
*!      result = geo_simplify(XY, delta [, B ])
*!
*!  result  n x 1 colvector tagging points to be kept (0 drop, 1 keep)
*!  XY      n x 2 real matrix containing the (X,Y) coordinates of the shape
*!          items to be simplified
*!  delta   real scalar specifying threshold for dropping points (minimum
*!          triangle area)
*!  B       n x 1 real colvector tagging start and end points of shared
*!          borders (0 regular point, !0 start or end point of shared border);
*!          simplification will then operate by segments formed by these points,
*!          ensuring consistent simplification of shared borders across shape
*!          items
*!
*! Note that all point items will be dropped (unless delta==0). Line items that
*! are simplified to two points will be dropped unless their length is at least
*! 2*sqrt(2*delta). Polygon items that are simplified to two points will be
*! dropped.
*!

local Bool  real scalar
local BoolC real colvector
local Int   real scalar
local IntC  real colvector
local RS    real scalar
local RC    real colvector
local RR    real rowvector
local RM    real matrix
local PC    pointer colvector

mata:

`BoolC' geo_simplify(`RM' XY, `RS' delta0, | `BoolC' B)
{
    `Int'   i, n, a, b
    `RS'    delta
    `IntC'  p
    `BoolC' I
    
    // checks
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    n = rows(XY)
    delta = 2 * delta0 // _geo_vwhyatt() operates on 2*(triangle area)
    if (delta<=0) return(J(n,1,1)) // keep all
    // loop over shape items
    p = selectindex(_mm_unique_tag(geo_pid(., XY)))
    i = rows(p)
    if (i==n) return(J(n,1,0)) // point data; drop all
    I = J(n,1,1)
    a = n + 1
    for (;i;i--) {
        b = a - 1; a = p[i]
        if ((b-a)<2) { // item has 2 or less rows; must be point => drop
            I[|a\b|] = J(b-a+1,1,0)
            continue
        }
        _geo_simplify(I, XY, B, a, b, delta)
    }
    return(I)
}

void _geo_simplify(`BoolC' I, `RM' XY, `BoolC' B, `Int' a, `Int' b, `RS' delta)
{
    `Int' r
    
    r = b - a // note: first row in XY is missing
    if (r>2 & rows(B) ? (any(B[|a+1\b|]) ? 1 : 0) : 0) {
        // process item piece by piece if there are shared borders (and the
        // item has at least 3 points)
        I[|a+1\b|] = _geo_simplify_B(XY[|a+1,1\b,2|], B[|a+1\b|], delta, r)
    }
    else {
        // else process entire item in one go
        I[|a+1\b|] = _geo_vwhyatt(XY[|a+1,1\b,2|], delta, r)
        if (r<3) {
            if (_geo_simplify_dist(XY[a+1,], XY[b,])<2*sqrt(delta)) {
                I[a+1] = 0; I[b] = 0; r = 0
            }
        }
    }
    if (!r) I[a] = 0 // untag first row if entire item dropped
}

`BoolC' _geo_simplify_B(`RM' XY, `BoolC' B, `RS' delta, `Int' n)
{   // simplify piece by piece; B assumed fleeting; n will be updated
    `Int'   a, b, i, r, c
    `IntC'  p
    `BoolC' I
    
    I = J(n, 1, 1)
    c = 1
    // item is polygon and first row untagged: last segment wraps around
    if (XY[1,]==XY[n,] & !B[1]) {
        // get start points of segments
        p = selectindex(B)
        i = rows(p)
        // process last segment
        a = p[i]
        b = p[1]
        r = n + b - a // length of segment
        I[a::n \ 2::b] = _geo_vwhyatt(XY[a::n \ 2::b,], delta, r)
        I[1] = I[n] // update first row
        if (r>2) c = 0 // (more than 2 remaining points)
        n = r
        i--
    }
    // not a polygon or first row tagged; no wrapping needed
    else {
        B[1] = 1; B[n] = 1 // tag first and last row in any case
        p = selectindex(B) // get start points of segments
        i = rows(p) - 1    // last element of p is end point, so ignore
        a = n
        n = 0
    }
    // process (remaining) segments
    for (;i;i--) {
        b = a; a = p[i]
        r = b - a + 1
        I[|a\b|] = _geo_vwhyatt(XY[|a,1 \ b,2|], delta, r)
        if (r>2) c = 0 // (more than 2 remaining points)
        n = n + r
    }
    // if no piece has more than 2 points: check whether further simplification
    // removes the item
    if (c) {
        if (_geo_simplify_empty(selectindex(I), XY, delta)) {
            n = 0
            I = J(rows(I), 1, 0)
        }
    }
    return(I)
}

`Bool' _geo_simplify_empty(`IntC' p, `RM' XY, `RS' delta)
{
    `Int' r
    
    r = rows(p)
    (void) _geo_vwhyatt(XY[p,], delta, r)
    if (r<3) {
        if (_geo_simplify_dist(XY[p[1],], XY[p[rows(p)],])<2*sqrt(delta)) {
            return(1)
        }
    }
    return(0)
}

`RC' _geo_simplify_dist(`RM' A, `RM' B) // distance between A and B
{
    return(sqrt((A[1]-B[1])^2 + (A[2]-B[2])^2))
}

`BoolC' _geo_vwhyatt(`RM' XY, `RS' delta, `Int' r) // r will be updated
{   // tags points to be kept; delta is assumed to be equal to twice the minimum
    // triangle area
    `Int'  i, i0, i1, n
    `IntC' I
    `RC'   A
    
    n = r
    if (n<3) return(J(n,1,1)) // only two points; nothing to drop
    if (delta>=.) {
        // delta is infinity; drop all points except first and last
        r = 2
        return(1 \ J(n-2,1,0) \ 1) 
    }
    A = . \ _geo_vwhyatt_A(XY) \ .
    I = J(n, 1, 1)
    i = _geo_vwhyatt_imin(A)
    while (A[i]<delta) {
        // remove point
        A[i] = .
        I[i] = 0
        r--
        // update area of point above and below
        i1 = _geo_vwhyatt_i1(I, i)
        i0 = _geo_vwhyatt_i0(I, i)
        if (i1<n) {
            i = i1
            i1 = _geo_vwhyatt_i1(I, i)
            A[i] = _geo_vwhyatt_a(XY[i,], XY[i0,], XY[i1,])
            if (i0>1) {
                i1 = i
                i = i0
                i0 = _geo_vwhyatt_i0(I, i)
                A[i] = _geo_vwhyatt_a(XY[i,], XY[i0,], XY[i1,])
            }
        }
        else if (i0>1) {
            i = i0
            i0 = _geo_vwhyatt_i0(I, i)
            A[i] = _geo_vwhyatt_a(XY[i,], XY[i0,], XY[i1,])
        }
        else break // only 2 points left
        // get index of new minimum
        i = _geo_vwhyatt_imin(A)
    }
    return(I)
}

`Int' _geo_vwhyatt_imin(A)
{
    transmorphic i, w
    pragma unset i
    pragma unset w
    
    minindex(A, 1, i, w)
    return(i[1]) // will return error if A is all missing
}

`Int' _geo_vwhyatt_i1(`IntC' I, `Int' i)
{
    `Int' i1
    
    i1 = i + 1
    while (!I[i1]) i1++
    return(i1)
}

`Int' _geo_vwhyatt_i0(`IntC' I, `Int' i)
{
    `Int' i0
    
    i0 = i - 1
    while (!I[i0]) i0--
    return(i0)
}

`RC' _geo_vwhyatt_A(`RM' XY)
{
    `RM' xy, lo, up
    
    xy = lo = up = (2,1 \ rows(XY)-1,2)
    lo[,1] = lo[,1] :- 1
    up[,1] = up[,1] :+ 1
    return(_geo_vwhyatt_a(XY[|xy|], XY[|lo|], XY[|up|]))
}

`RC' _geo_vwhyatt_a(`RM' xy, `RM' lo, `RM' up) // returns double triangle area
{
    return(abs((up[,1]-xy[,1]) :* (lo[,2]-xy[,2])
             - (lo[,1]-xy[,1]) :* (up[,2]-xy[,2])))
}

end

*! {smcl}
*! {marker geo_spjoin}{bf:geo_spjoin()}{asis}
*! version 1.0.2  23oct2023  Ben Jann
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
*!  XY      n x 2 real matrix of (X,Y) coordinates of the polygons; missing
*!          coordinates will be ignored
*!  PL      optional n x 1 real colvector containing plot level indicator 
*!  nodots  nodots!=0 suppresses progress dots
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
    `RC'     L
    `RM'     id
    pointer  pl
    
    if (args()<6) nodots = 0
    if (cols(xy)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (!rows(PL)) return(_geo_spjoin(xy, ID, PID, XY, 0, nodots, "(")[,1])
    if (hasmissing(PL)) pl = &editmissing(PL, 0) // treat missing as 0
    else                pl = &PL
    L  = mm_unique(select(*pl, _mm_uniqrows_tag((ID, PID))))
    id = J(rows(xy), 2, .) // second column will be used to tag matched points
    p  = . // use all points in first round
    n = length(L)
    for (i=n;i;i--) {
        P      = selectindex(*pl:==L[i])
        id[p,] = _geo_spjoin(xy[p,], ID[P], PID[P], XY[P,], mod(L[i],2), nodots,
            sprintf("(pass %g/%g: ", n-i+1, n))
        if (i>1) p = selectindex(!id[,2]) // remaining unmatched points
    }
    return(id[,1])
}

`RM' _geo_spjoin(`RM' xy, `RC' ID, `RC' PID, `RM' XY, `Bool' enclave,
    `Bool' nodots, `SS' msg)
{
    `Int'    i, i0, n, ii
    `IntC'   p
    `RM'     id
    `pUnits' u
    
    if (!nodots) i0 = _geo_progress_init(msg)
    u = _geo_collect_units(ID, PID, XY)
    n = rows(xy)
    id = J(n, 1, (.,0)) // second column: set 1 if matched
    p = unorder(n) // (evaluate points in random order for even progress)
    for (i=n;i;i--) {
        ii = p[i]
        id[ii,] = __geo_spjoin(xy[ii,1], xy[ii,2], enclave, u)
        if (!nodots) _geo_progress(i0, 1-(i-1)/n)
    }
    if (!nodots) _geo_progress_end(i0, ")")
    return(id)
}

`RM' __geo_spjoin(`RS' px, `RS' py, `Bool' enclave, `pUnits' u)
{
    `Int'      i, j, m
    `pPolygon' p
    
    for (i=length(u);i;i--) {
        for (j=u[i]->n;j;j--) {
            p = u[i]->p[j]
            if (px < p->xmin) continue
            if (px > p->xmax) continue
            if (py < p->ymin) continue
            if (py > p->ymax) continue
            if (m=geo_pointinpolygon(px, py, p->XY)) {
                // if not enclave: return id of matching unit
                if (!enclave) return((u[i]->id,1))
                // if enclave and inside: tag as matched, but do not return ID
                if (m==1)     return((.,1))
                // if enclave and on edge: do not tag, but exit search
                              return((.,0))
            }
        }
    }
    return((.,0))
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
*! {marker geo_tissot}{bf:geo_tissot()}{asis}
*! version 1.0.0  26may2024  Ben Jann
*!
*! Compute (unprojected) Tissot's indicatrix at a given point and radius
*! (see https://en.wikipedia.org/wiki/Tissot%27s_indicatrix); based on code
*! from -geo2xy- (from SSC) by Robert Picard (program -geo2xy_tissot- in
*! geo2xy.ado)
*!
*! Syntax:
*!
*!      XY = geo_tissot(x, y, r, n)
*!
*!  XY         (n+1) x 2 real matrix containing coordinates of Tissot polygon
*!  x          real scalar specifying X coordinate of point (in degrees)
*!  y          real scalar specifying Y coordinate of point (in degrees)
*!  r          real scalar specifying radius (in degrees);
*!             y +/- r must within +/- 90 for valid results
*!  n          real scalar setting number of edges of polygon
*!
*!  Function _geo_tissot() is like geo_tissot(), but with input and output in
*!  radians rather than degrees.

mata:
mata set matastrict on

real matrix geo_tissot(real scalar x, real scalar y, real scalar r,
    real scalar n)
{
    return(_geo_tissot(x*pi()/180, y*pi()/180, r*pi()/180, n)*(180/pi()))
}

real matrix _geo_tissot(real scalar x, real scalar y, real scalar r,
    real scalar n)
{
    real colvector b, Y
    
    b = rangen(0, 2*pi(), n+1)
    b[n+1] = 0 // for sake of precision (last point = first point)
    Y = asin(sin(y) * cos(r) :+ cos(y) * sin(r) :* cos(b))
    return((
        x :+ atan2(cos(r) :- sin(y) * sin(Y), sin(b) :* sin(r) * cos(y)),
        Y))
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
*!      circle = geo_welzl(XY [, recursive])
*!
*!  circle     real rowvector containing mid and radius (X,Y,R)
*!  XY         n x 2 real matrix containing points; rows containing missing
*!             will be ignored
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

`Circle' geo_welzl(`Points' XY, | `Bool' recursive)
{
    pointer (`Points') scalar xy
    
    if (args()<2) recursive = 0
    if (cols(XY)!=2) {
        errprintf("{it:XY} must have two columns\n")
        exit(3200)
    }
    if (isfleeting(XY)) xy = &XY
    else                xy = &J(1,1,XY)
    if (hasmissing(*xy)) *xy = select(*xy, !rowmissing(*xy))
    *xy = jumble(select(*xy, mm_uniqrows_tag(*xy)))
    if (recursive) return(_geo_welzl_recursive(*xy, J(0,2,.), rows(*xy)))
    return(_geo_welzl_iterative(*xy))
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

*! {smcl}
*! {marker geo_wkt2xy}{bf:geo_wkt2xy()}{asis}
*! version 1.0.0  02dec2023  Ben Jann
*!
*! Extracts the coordinates form a WKT geometry specification ("Well-known text
*! representation of geometry geometry; see
*! https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry).
*! Supported geometry types are Point, MultiPoint, LineString, MultiLineString,
*! Polygon, MultiPolygon, and GeometryCollection (case insensitive).
*! In the returned matrix of coordinates, each extracted line or polygon and,
*! in case of a GeometryCollection, each point will start with a row of missing
*! values. Missing rows will be omitted by default when extracting coordinates
*! from a Point or MultiPoint specification.
*!
*!      XY = geo_wkt2xy(s [, mis, gtype, rc])
*!
*!  XY      n x 2 real matrix containing the extracted coordinates; J(1,2,.) is
*!          returned if no coordinates are found
*!  s       string scalar containing the WKT specification
*!  mis     mis!=0 enforces separating points by missing in any case
*!  gtype   string scalar that will be set to the geometry type of s
*!  rc      real scalar that will be set to 1 if an issue occurred (invalid
*!          or incomplete WKT specification); else 0
*!

local Bool real scalar
local Int  real scalar
local RR   real rowvector
local RM   real matrix
local SS   string scalar
local SR   string rowvector
local T    transmorphic

mata:
mata set matastrict on

// main function: determine geometry type and launch relevant parser
`RM' geo_wkt2xy(`SS' S, | `Bool' mis, `SS' kw, `Bool' rc)
{
    `SS' s
    `RM' XY
    `T'  t
    
    if (args()<2) mis = 0
    rc = 0
    t = tokeninit("", ",", "()")
    tokenset(t, S)
    kw = strtrim(strlower(tokenget(t)))
    s  = tokenget(t)
    _geo_wkt2xy_pstrip(rc, t, s)
    if (tokenrest(t)!="") rc = 1
    XY = J(0,2,.)
    if (kw=="point") {
        kw = "Point"
        _geo_wkt2xy_p(rc, XY, s, mis!=0)
    }
    else if (kw=="multipoint") {
        kw = "MultiPoint"
        _geo_wkt2xy_P(t, rc, XY, s, mis!=0)
    }
    else if (kw=="linestring") {
        kw = "LineString"
        _geo_wkt2xy_l(rc, XY, s)
    }
    else if (kw=="multilinestring") {
        kw = "MultiLineString"
        _geo_wkt2xy_L(t, rc,  XY, s)
    }
    else if (kw=="polygon") {
        kw = "Polygon"
        _geo_wkt2xy_s(t, rc, XY, s)
    }
    else if (kw=="multipolygon") {
        kw = "MultiPolygon"
        _geo_wkt2xy_S(t, rc, XY, s)
    }
    else if (kw=="geometrycollection") {
        kw = "GeometryCollection"
        _geo_wkt2xy_C(t, rc, XY, s)
    }
    else {
        rc = 1
        kw = ""
    }
    if (!rows(XY)) XY = (.,.)
    return(XY)
}

// parser for geometrycollection
void _geo_wkt2xy_C(`T' t0, `Bool' rc, `RM' XY, `SS' S)
{
    `SS' kw, s
    `T'  t
    
    t = t0
    tokenset(t, S)
    while ((kw = tokenget(t))!="") {
        if (kw==",") continue
        kw = strtrim(strlower(kw))
        s  = tokenget(t)
        _geo_wkt2xy_pstrip(rc, t, s)
        if      (kw=="point")           _geo_wkt2xy_p(rc, XY, s, 1)
        else if (kw=="multipoint")      _geo_wkt2xy_P(t0, rc, XY, s, 1)
        else if (kw=="linestring")      _geo_wkt2xy_l(rc, XY, s)
        else if (kw=="multilinestring") _geo_wkt2xy_L(t0, rc,  XY, s)
        else if (kw=="polygon")         _geo_wkt2xy_s(t0, rc, XY, s)
        else if (kw=="multipolygon")    _geo_wkt2xy_S(t0, rc, XY, s)
        else rc = 1
    }
}

// parser for point
void _geo_wkt2xy_p(`Bool' rc, `RM' XY, `SS' S, `Bool' mis)
{
    XY = XY \ J(mis, 2, .) \ __geo_wkt2xy_xy(rc, S)
}

// parser for points
void _geo_wkt2xy_P(`T' t0, `Bool' rc, `RM' XY, `SS' S, `Bool' mis)
{
    `SS' s
    `T'  t
    
    t = t0
    tokenset(t, S)
    while ((s = tokenget(t))!="") {
        if (s==",") continue
        if (strtrim(s)=="") continue
        // note; parentheses are optional; cannot use _geo_wkt2xy_pstrip()
        if (substr(s,1,1)=="(") {
            s = strltrim(substr(s,2,.))
            if (substr(s,-1,1)==")") s = strrtrim(substr(s,1,strlen(s)-1))
            else rc = 1 // opening paren only
        }
        else if (substr(s,-1,1)==")") {
            s = strrtrim(substr(s,1,strlen(s)-1))
            rc = 1 // closing paren only
        }
        _geo_wkt2xy_p(rc, XY, s, mis)
    }
}

// parser for line
void _geo_wkt2xy_l(`Bool' rc, `RM' XY, `SS' S)
{
    XY = XY \ _geo_wkt2xy_xy(rc, S)
}

// parser for lines
void _geo_wkt2xy_L(`T' t0, `Bool' rc, `RM' XY, `SS' S)
{
    `SS' s
    `T'  t
    
    t = t0
    tokenset(t, S)
    while ((s = tokenget(t))!="") {
        if (s==",") continue
        if (strtrim(s)=="") continue
        _geo_wkt2xy_pstrip(rc, t, s)
        _geo_wkt2xy_l(rc, XY, s)
    }
}

// parser for polygon
void _geo_wkt2xy_s(`T' t0, `Bool' rc, `RM' XY, `SS' S)
{
    `SS' s
    `T'  t
    
    t = t0
    tokenset(t, S)
    while ((s = tokenget(t))!="") {
        if (s==",") continue
        if (strtrim(s)=="") continue
        _geo_wkt2xy_pstrip(rc, t, s)
        XY = XY \ _geo_wkt2xy_xy(rc, s)
    }
}

// parser for multipolygon
void _geo_wkt2xy_S(`T' t0, `Bool' rc, `RM' XY, `SS' S)
{
    `SS' s
    `T'  t
    
    t = t0
    tokenset(t, S)
    while ((s = tokenget(t))!="") {
        if (s==",") continue
        if (strtrim(s)=="") continue
        _geo_wkt2xy_pstrip(rc, t, s)
        _geo_wkt2xy_s(t0, rc, XY, s)
    }
}

// collect coordinates of a single item
`RM' _geo_wkt2xy_xy(`Bool' rc, `SS' s)
{
    `Int' i, n
    `SR'  S
    `RM'  XY
    
    S = tokens(s, ",")
    S = select(S, S:!=",")
    n = length(S)
    if (!n) { // item is empty
        rc = 1
        return(J(0,2,.))
    }
    XY = J(n, 2, .)
    for (i=1;i<=n;i++) XY[i,] = __geo_wkt2xy_xy(rc, S[i])
    return((.,.) \ XY) // add leading row containing missing
}

`RR' __geo_wkt2xy_xy(`Bool' rc, `SS' s)
{
    `Int' j
    `RR'  xy
    
    xy = strtoreal(tokens(s))
    j = length(xy)
    if (j!=2) {
        rc = 1
        if (j>2)       xy = xy[(1,2)] // too many elements
        else if (j==1) xy = (xy,.)    // too few element
        else           xy = (.,.)     // too few element
    }
    return(xy)
}

// remove outer parentheses (sets rc = 1 if no outer parentheses)
void _geo_wkt2xy_pstrip(`Bool' rc, `T' t, `SS' s)
{
    if (substr(s,1,1)=="(") {
        s = strltrim(substr(s,2,.))
        if (substr(s,-1,1)==")") s = strrtrim(substr(s,1,strlen(s)-1))
        else {
            rc = 1
            s = s + tokenrest(t) // add rest if closing paren is missing
            tokenset(t, "")
        }
    }
    else {
        if (substr(s,-1,1)==")") s = strrtrim(substr(s,1,strlen(s)-1))
        rc = 1
    }
}

end

*! {smcl}
*! {marker _geo_progress}{bf:_geo_progress()}{asis}
*! version 1.0.1  23jun2024  Ben Jann
*!
*! Helper function to display progress dots.
*!
*! Syntax:
*!
*!      j = _geo_progress_init(prefix)
*!      ...
*!      _geo_progress(j, p)
*!      ...
*!      _geo_progress_end(j, suffix)
*!
*!  j       real scalar containing progress counter
*!  p       real scalar specifying proportion of progress
*!  prefix  string scalar specifying prefix to be printed
*!  suffix  string scalar specifying suffix to be printed
*!

local Int       real scalar
local SS        string scalar

mata:
mata set matastrict on

`Int' _geo_progress_init(`SS' msg)
{
    displayas("txt")
    printf("%s0%%", msg)
    displayflush()
    return(0)
}

void _geo_progress(`Int' j, `Int' p)
{
    while (1) {
        if (j>=40) break
        if (p < (j+1)/40) break
        j++
        if (mod(j,4)) {
            printf(".")
            displayflush()
        }
        else {
            printf("%g%%", j/4*10)
            displayflush()
        }
    }
}

void _geo_progress_end(`Int' j, `SS' msg)
{
    _geo_progress(j, 1) // complete to 100% if necessary
    printf("%s\n", msg)
    displayflush()
}

end

*! {smcl}
*! {marker varia}Further functions{asis}
*! version 1.0.0  30jun2024  Ben Jann
*!

mata:
mata set matastrict on

void _geo_parse_marginexp(string scalar nm, string scalar s,
    | real scalar lb, real scalar def)
{
    real scalar    i, n
    real rowvector R
    string scalar  tok
    transmorphic   t
    
    if (args()<4) def = 0
    R = strtoreal(tokens(s))
    n = length(R)
    if (!n) { // empty
        st_local(nm, invtokens(strofreal(J(1,4,def))))
        return
    }
    if (!hasmissing(R)) { // all numeric
        if (n<4) {
            R = R, J(1,4-n,.)
            for (i=4;i>=n;i--) R[i] = R[mod(i-1,n)+1] // recycle
        }
        else if (n>4) R = R[|1\4|]
    }
    else {
        R = J(1,4,def)
        t = tokeninit(" ", "=")
        tokenset(t, s)
        while ((tok = tokenget(t))!="") {
            i = 0
            if      (tok=="l") i = 1
            else if (tok=="r") i = 2
            else if (tok=="b") i = 3
            else if (tok=="t") i = 4
            if (i) {
                if (tokenpeek(t)=="=") (void) tokenget(t) // remove "="
                R[i] = strtoreal(tokenget(t))
                continue
            }
            errprintf("invalid {bf:%s()}\n", nm)
            exit(198)
        }
    }
    if (hasmissing(R)) {
        errprintf("invalid {bf:%s()}; missing not allowed\n", nm)
        exit(198)
    }
    if (lb<.) {
        if (any(R:<lb)) {
            errprintf("invalid {bf:%s()}; has values out of range\n", nm)
            exit(198)
        }
    }
    st_local(nm, invtokens(strofreal(R)))
}

end
