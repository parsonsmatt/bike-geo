-- | Calculator and logic for figuring out bike geometry.
module Bike.Geo where

import Control.Monad

-- | A bicycle stem connects the handlebars and the steerer tube. Stems are
-- marketed with a length (usually in millimeters) and an angle of
-- rise/drop.
data Stem = Stem
    { stemLength :: Double
    -- ^ The length of the stem corresponds to the hypotenuse of a right
    -- triangle.
    , stemAngle :: Double
    -- ^ Stems are usually marketed with some angle. An angle of 0 means that it
    -- sticks straight out. An angle of +6 means that the stem is angled at
    -- 6 degrees from the steerer tube.
    }
    deriving stock Show

-- | An 'Offset' describes the difference in rise and the difference in
-- reach of a given component set.
data Offset = Offset
    { offsetRise :: Double
    , offsetReach :: Double
    }
    deriving stock Show

diffOffset :: Offset -> Offset -> Offset
diffOffset o0 o1 =
    Offset
        { offsetRise =
            offsetRise o0 - offsetRise o1
        , offsetReach =
            offsetReach o0 - offsetReach o1
        }

radians :: Double -> Double
radians degrees = degrees * pi / 180

stemOffset :: Stem -> Offset
stemOffset Stem {..} =
    Offset
        { offsetRise =
            -- sin t = o / h
            -- h * sin t = o
            stemLength * sin (radians stemAngle)
        , offsetReach =
            -- cos t = a / h
            -- h * cos t = a
            stemLength * cos (radians stemAngle)
        }

-- | A bicycle 'Frameset' usually means the frame and the steerer tube,
-- plus the headset (the bearings and other things that allow the steerer
-- tube to spin with little resistance).
data Frameset = Frameset
    { framesetHeadTubeAngle :: Double
    -- ^ The head tube angle is located at the bottom of the fork, and on
    -- the side towards the bicycle frame. The head tube angle affects the
    -- actual angle that the stem points out from, and determines the final
    -- rise and reach from the stem.
    , framesetStack :: Double
    -- ^ The stack of the frame is the measurement of the vertical height
    -- separating the center of the bottom bracket to the center of the
    -- steerer tube exit hole in the head tube, in millimeters.
    , framesetReach :: Double
    -- ^ The reach of the frame  is the measurement of the horizontal
    -- distance between the center of the bottom bracket to the center of
    -- the steerer tube exit hole in the head tube, in millimeters.
    , framesetSeatTubeAngle :: Double
    -- ^ The seat tube angle determines how far back the saddle goes when it is
    -- raised, or how far forward the saddle goes when it is lowered.
    }

-- | While you can calculate an 'Offset' from just a 'Stem', you need to
-- know the head tube angle of the bike in question to determine the actual
-- offset of the stem.
stemOnFrameOffset :: Stem -> Frameset -> Offset
stemOnFrameOffset Stem {..} Frameset {..} =
    Offset
        { offsetRise =
            stemLength * sin (radians (stemAngle + (90 - framesetHeadTubeAngle)))
        , offsetReach =
            stemLength * cos (radians (stemAngle + (90 - framesetHeadTubeAngle)))
        }

woodsmokeSmall :: Frameset
woodsmokeSmall =
    Frameset
        { framesetHeadTubeAngle = 67.8
        , framesetStack = 606
        , framesetReach = 409
        , framesetSeatTubeAngle = 73.3
        }

stems :: [Stem]
stems = do
    stemLength <- [ 30, 40 .. 100 ]
    stemAngle <- [0, 6, 9, 12, 15, 20, 25, 30, 35]
    pure Stem {..}

-- λ> stemOnFrameOffset (Stem 40 6) woodsmokeSmall
-- Offset {offsetRise = 18.90203059476216, offsetReach = 35.252138082599686}
currentSituation :: Offset
currentSituation = stemOnFrameOffset (Stem 40 6) woodsmokeSmall

options = do
    stem <- stems
    let
        o@Offset {..} =
            stemOnFrameOffset stem woodsmokeSmall
    guard $ offsetReach <= 40
    guard $ offsetRise >= 20
    pure (stem, diffOffset o currentSituation)

-- | A 'BikeBuild' is a collection of parts. Taken together, they can
data BikeBuild = BikeBuild
    { bikeBuildFrameset :: Frameset
    , bikeBuildStem :: Stem
    , bikeBuildCrankset :: Crankset
    }

-- | A crankset refers to the pair of crank arms that spin around the bottom
-- bracket. The length of the crank arm influences how high the seatpost sits,
-- which affects the overall reach of the bicycle build.
data Crankset = Crankset
    { cranksetLength :: Double
    -- ^ The length of a crank arm, from the center of the bottom bracket to the
    -- center of the pedal spindle hole.
    --
    -- If you get shorter cranks, like 165mm instead of 175mm, then your foot
    -- will be 10mm higher at the bottom of the pedal stroke. This allows for
    -- you to raise the seatpost. The amount of rise depends on the seat tube
    -- angle.
    }

{-

$crankseat

OK, so let's reason about how cranksets and seatposts affect saddle height. I'm
not going to try and calculate ideal saddle height - I am going to *assume* it.
End user records a height they like, and we calculate verything else from it.

Basically we have these points:

      A
       \
        \
         \
          B
          |
          |
          |
          C

* A is the saddle rail
* B is the bottom bracket center
* C is the pedal spindle

So segment BC is the crank arm length, segment A-B is the length of seat tube
and seat post. If we alter BC, how do we want to alter AB?

With a bit more points labeled:

      A   E
       \
        \
         \
      F   B
          |
          |
          |
      D   C

* D is the point below the saddle rails at the height of the crank pedal
  spindle.
* E is the point directly above the bottom bracket at the height of the saddle
  rails.
* F is the point below the saddle rails and behind the bottom bracket.

We can extend A-B to keep either CA or CE constant. How does this differ?

We have two right triangles: one described by FBA and another by FBC. The angle
F->BC is *also* the seat tuble angle. Nice.

But, if we want to know A-C segment, then we need to consider the triangle given
by ABC. We can create a point on the midpoint between A-C, call it G, and
subdivide the triangle into two right triangles.

   A
   \ \
    \   \
     \     \
      \     B
       G    |
        \   |
         \  |
          \ |
           C

The angle between G-AB is 90 degrees and B-AG is half of the seat tube *crank*
angle (or, 90 + seat tube angle / 2). This triangle is symmetrical with GBC, so
we can determine the G-C segment length and double it for the A-C segment.

So, rewriting,

    B
    |  \
    |     \
    |        \
    G---------C

B-C is crank length.
Angle B-GC is known.

I want to determine the length of the opposite side, and I know the hypotenuse,
adjacent, and angle.

cos t = a / h
cos ((90 + STA) / 2) = a / crank-length
crank-length * cos t = G-C

We get 2 (G-C) for A-C.

Finally, we have *another* triangle, and this one gives us the offset.

    A---E
     \  |
      \ |
       \|
        C

We know A-C length, and we have a right triangle. We need to determine the angle
C-AE (or A-EC, equivalent). A triangle's angles sum to 180, so we need to split
90 degrees between them.

Let's look at BGC triangle again. The angle at B-AC is (90 + seat tube angle).
Then we bisected that triangle, creating two right triangles. So that angle was
(90 + seat tube angle) / 2. Then, we know Hypotenuse, we know the angle at C-AE,
so we can figure out the adjacent and opposite angles.

sin t = o/h
h * sin t = o

cos t = a/h
h * cost t = a

-}

-- | The angle of the seat tube to the crank when the crank is pointing down.
seatTubeCrankAngle :: Frameset -> Double
seatTubeCrankAngle Frameset {..} =
    framesetSeatTubeAngle + 90

-- | Calculate the setback and total height of the distance between the saddle
-- rails and the crankset spindle.
--
-- The 'Offset' returned corresponds to segments CD and CE.
pedalToSaddleOffset
    :: Double
    -- ^ Measurement of A-B: the distance from BB to saddle rails
    -> Frameset
    -> Crankset
    -> Offset
pedalToSaddleOffset seatpostLength fs Crankset {..} =
    Offset
        { offsetRise =
            ac * sin cToAEAngle
        , offsetReach =
            -- we negate the computation, since it is a setback, not reach
            negate $
                ac * cos cToAEAngle
        }
  where
    cToAEAngle =
        radians (180 - 90 - (seatTubeAngleToCranks / 2))
    ac =
        2 * gc
    gc =
        cranksetLength * cos (radians (seatTubeAngleToCranks / 2))
    seatTubeAngleToCranks =
        seatTubeCrankAngle fs
