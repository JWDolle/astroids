-- Enemy specific code goes here

module Enemy where
import Graphics.Gloss
import Entity

data EnemyType = SmallAsteroid  {
				location :: Point
				speed :: Double
				direction :: Vector
				health :: Int
				collider :: BoundingBox
                sprite :: Picture
                } | BigAsteroid {
				location :: Point
				speed :: Double
				direction :: Vector
				health :: Int
                collider :: BoundingBox
                sprite :: Picture
				asteroids :: [EnemyType]
                } | UFO         {
				Location :: Point
				speed :: Double
				direction :: Vector
				health :: Int
                collider :: BoundingBox
                sprite :: Picture
				bullets :: [Bullet]
                } deriving (Renderable)

instance Renderable EnemyType where
    render (SmallAsteroid _ _ _ _ _ sprite) = sprite
    render (BigAsteroid _ _ _ _ _ sprite _) = sprite
    render (UFO _ _ _ _ _ sprite _)         = sprite