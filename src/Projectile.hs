-- Projectile code goes here
module Projectile where
import Graphics.Gloss
data Projectile = Bullet {
    bShape :: Picture
    bBB :: BoundingBox
} | Laser {
    bShape :: Picture
    lBB :: BoundingBox
}
