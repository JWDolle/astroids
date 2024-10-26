module Pipeline where
import Model
import Collision

import Updates
 
-- this is for a pipelin looking like this {GameState i e p c Playing}
pipeline1:: Float -> GameState -> GameState
pipeline1 secs gstate@(GameState i e p c u s l b r Playing ) =
    let 
        timeUpdate = gstate{elapsedTime = e + secs }
        playerUpdate = updatePlayer timeUpdate
        enemiesUpdated = updateEnemies playerUpdate
        bulletUpdated= updateBullets (elapsedTime enemiesUpdated) enemiesUpdated
        
        collisionHandle = handleCollision bulletUpdated
       
        
        updatedState = collisionHandle
    in updatedState
