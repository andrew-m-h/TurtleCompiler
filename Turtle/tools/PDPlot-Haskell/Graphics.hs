-------------------------------------------------------------------------
-- Graphics module for Haskell.

-- Clem Baker-Finch.
-- Based on a script written by Tim Lambert of UNSW.
-- Thanks Tim.

-- Co-ordinates are Float values.
-- Angles are presently Int degrees, but I might change that.
-- Gray scale is Int percentage.
-------------------------------------------------------------------------

module Graphics (
  Picture, Object(Line, Polygon, Text, Colour), Path, Point,
  Colour (Red,Yellow,Green,Cyan,Blue,Magenta,Grey), Percent,
  shift, rotate, scale, enlarge, shrink, draw, drawTo) where

-- A picture is a list of objects.

type Picture = [Object]

-- Objects are the basic elements that make up a picture.

data Object = Line Path | Polygon Path
            | Text String Point | Colour Colour Picture

data Colour = Red | Yellow | Green | Cyan
            | Blue | Magenta | Grey Percent

-- Black is Grey 0, white is Grey 100.

type Percent = Int

-- Points are ordered pairs of Floats.  Each pixel is one unit.

type Point = (Float,Float)

-- A Path is specified as a sequence of points.

type Path = [Point]


-------------------------------------------------------------------------

-- Shift a picture. e.g shift (1,2) moves the whole picture 1 unit right
-- and up 2.  We do this by shifting every point in the picture.

shift :: Point -> Picture -> Picture
shift t = mapPic (shiftPoint t)

-------------------------------------------------------------------------

-- Rotate a picture. For example, rotate 10 rotates a picture by 10
-- degrees about the origin (0,0) which is the bottome left-hand corner
-- of the page.  Again we do this by rotating every point in the picture.

rotate :: Int -> Picture -> Picture 
rotate t = mapPic (rotatePoint t)

-------------------------------------------------------------------------

-- Scale a picture.  For example, scale (2,2) makes a picture twice as
-- big and scale (0.5,0.5) makes a picture half as big.  Again this is
-- achieved by scaling the co-ordinates of each point in the picture.

scale :: Point -> Picture -> Picture
scale t = mapPic (scalePoint t)

-- For convenience, enlarge and shrink are also provided (and defined
-- in terms of scale).

-- For example, enlarge (2,2) makes the whole picture twice as big.

enlarge :: Point -> Picture -> Picture
enlarge t = scale t

-- For example, shrink (2,2) makes the whole picture half as big.

shrink :: Point -> Picture -> Picture
shrink (sx,sy) = scale (1/sx,1/sy)

-------------------------------------------------------------------------

-- The next three functions shift, scale and rotate points. 

shiftPoint :: Point -> Point -> Point
shiftPoint (tx,ty) (x,y) = (tx+x,ty+y)

scalePoint :: Point -> Point -> Point
scalePoint (sx,sy) (x,y) = (sx*x,sy*y)
 
rotatePoint :: Int -> Point -> Point 
rotatePoint angle (x,y) = 
    (x * cos theta - y * sin theta ,
     x * sin theta + y * cos theta ) 
    where theta = (fromIntegral angle * pi) / 180.0

-------------------------------------------------------------------------

-- Takes a function of type Point -> Point and applies it to every point
-- in a picture. It is similar to map.

mapPic :: (Point -> Point) -> Picture -> Picture
mapPic f pic 
 = map fpic pic
   where fpic (Line xs) = Line (map f xs)
         fpic (Polygon xs) = Polygon (map f xs)
         fpic (Text s x) = Text s (f x)
         fpic (Colour c pic) = Colour c (mapPic f pic)

-------------------------------------------------------------------------

-- draw and drawTo produce postscript files representing the pictures.
-- They can be displayed with Ghostview or similar viewers.

draw:: Picture -> IO ()
draw p = writeFile psfilename (prologue++concat (map psObj p))

psfilename = "pic.ps"

-- Alternatively, you can specify your own target postscript file.

drawTo :: FilePath -> Picture -> IO ()
drawTo f p = writeFile f (prologue++concat (map psObj p))

-------------------------------------------------------------------------
-- The following functions handle the details of the translation to 
-- Postscript.
-------------------------------------------------------------------------

-- prologue contains PostScript definitions needed to set things up.

prologue = 
    "%!PS\n" ++
    "/Courier findfont " ++ show charHeight ++ " scalefont setfont\n" ++
    "/l {lineto} def /m {newpath moveto} def /s {stroke} def\n" ++
    "/f {fill} def\n"

-- listPath prints the coordinates of all the points in the path.

listPath:: Path -> [Char]
listPath = concat . map listPoint

-- listPoint prints the coordinates of a single point.

listPoint:: Point -> [Char]
listPoint (x,y) = show x ++ " " ++ show y ++ "\n"

-- Height of one character.

charHeight = 10 :: Float

-- Width of one character.

charWidth = 6 :: Float

-- Convert a single object to postscript.

psObj :: Object -> [Char]
psObj (Line [])      = ""
psObj (Line ps)      = psLinePath ps ++ "s\n"
psObj (Polygon [])   = ""
psObj (Polygon ps)   = psLinePath ps ++ "f\n"
psObj (Text s p)     =
    listPoint p ++ " moveto (" ++ escape s ++ ") show"++"\n"
psObj (Colour c pic) = 
    "save\n"++(psCol c)++(concat(map psObj pic))++ "restore\n"

-- Convert Colours to postscript.

psCol :: Colour -> String
psCol (Grey p) = show p ++ " 100 div setgray\n"
psCol Red      = "1 0 0 setrgbcolor\n"
psCol Green    = "0 1 0 setrgbcolor\n"
psCol Blue     = "0 0 1 setrgbcolor\n"
psCol Cyan     = "0 1 1 setrgbcolor\n"
psCol Magenta  = "1 0 1 setrgbcolor\n"
psCol Yellow   = "1 1 0 setrgbcolor\n"

-- psLinePath makes a lineto for each point in a path.

psLinePath :: Path -> String
psLinePath []         = []
psLinePath ((x,y):ps) =
    show x ++ " " ++ show y ++ " m\n"++
    concat [show x ++ " " ++ show y ++ " l\n"|(x,y)<-ps]

-- escape parentheses and newlines in postscript strings.

escape :: String -> String
escape []       = []
escape ('(':s)  = '\\':'(':escape s
escape (')':s)  = '\\':')':escape s
escape ('\n':s) = '\\':'n':escape s
escape ('\\':s) = '\\':'\\':escape s
escape (c:s)    = c:escape s

-------------------------------------------------------------------------

