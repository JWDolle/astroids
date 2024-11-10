module Pipeline where
import Model
import Collision
import Enemy
import Updates
 
-- The update pipeline for the game
pipeline1:: Float -> GameState -> GameState
pipeline1 secs gstate@(GameState i e p c u s l b r sc Playing ) =
    let 
        timeUpdate = gstate{elapsedTime = e + secs }
        playerUpdate = updatePlayer timeUpdate
        enemiesUpdated = updateEnemies playerUpdate
        --intermediate steps
        laserSpawned = spawnLasers enemiesUpdated
        resetUFOtimers = map filterUfo (ufos laserSpawned)
        --end intermediate steps
        reseted = laserSpawned{ufos = resetUFOtimers}
        
        bulletUpdated= updateBullets (elapsedTime reseted) reseted
        laserUpdated = updateLaser (elapsedTime bulletUpdated ) bulletUpdated
        collisionHandle = handleCollision laserUpdated
       
        
        updatedState = collisionHandle
    in updatedState
